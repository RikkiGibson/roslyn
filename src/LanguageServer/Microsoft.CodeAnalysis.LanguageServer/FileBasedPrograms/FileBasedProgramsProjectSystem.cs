// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.ErrorReporting;
using Microsoft.CodeAnalysis.Features.Workspaces;
using Microsoft.CodeAnalysis.LanguageServer.Handler;
using Microsoft.CodeAnalysis.LanguageServer.HostWorkspace;
using Microsoft.CodeAnalysis.LanguageServer.HostWorkspace.ProjectTelemetry;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.ProjectSystem;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Shared.TestHooks;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.Workspaces.ProjectSystem;
using Microsoft.CommonLanguageServerProtocol.Framework;
using Microsoft.Extensions.Logging;
using Roslyn.LanguageServer.Protocol;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.LanguageServer.FileBasedPrograms;

/// <summary>Handles loading both miscellaneous files and file-based program projects.</summary>
internal sealed class FileBasedProgramsProjectSystem : LanguageServerProjectLoader, ILspMiscellaneousFilesWorkspaceProvider, IDisposable, IOnInitialized
{
    private readonly ILspServices _lspServices;
    private readonly ILogger<FileBasedProgramsProjectSystem> _logger;
    private readonly VirtualProjectXmlProvider _projectXmlProvider;
    private readonly CanonicalMiscFilesProjectLoader _canonicalMiscFilesLoader;

    public ImmutableArray<string> WorkspaceFoldersOpt { get; private set; }

    public FileBasedProgramsProjectSystem(
        ILspServices lspServices,
        VirtualProjectXmlProvider projectXmlProvider,
        LanguageServerWorkspaceFactory workspaceFactory,
        IFileChangeWatcher fileChangeWatcher,
        IGlobalOptionService globalOptionService,
        ILoggerFactory loggerFactory,
        IAsynchronousOperationListenerProvider listenerProvider,
        ProjectLoadTelemetryReporter projectLoadTelemetry,
        ServerConfigurationFactory serverConfigurationFactory,
        IBinLogPathProvider binLogPathProvider,
        DotnetCliHelper dotnetCliHelper)
            : base(
                workspaceFactory,
                fileChangeWatcher,
                globalOptionService,
                loggerFactory,
                listenerProvider,
                projectLoadTelemetry,
                serverConfigurationFactory,
                binLogPathProvider,
                dotnetCliHelper)
    {
        _lspServices = lspServices;
        _logger = loggerFactory.CreateLogger<FileBasedProgramsProjectSystem>();
        _projectXmlProvider = projectXmlProvider;
        _canonicalMiscFilesLoader = new CanonicalMiscFilesProjectLoader(
                workspaceFactory,
                fileChangeWatcher,
                globalOptionService,
                loggerFactory,
                listenerProvider,
                projectLoadTelemetry,
                serverConfigurationFactory,
                binLogPathProvider,
                dotnetCliHelper);

        globalOptionService.AddOptionChangedHandler(this, OnGlobalOptionChanged);
    }

    public void Dispose()
    {
        _canonicalMiscFilesLoader.Dispose();
        GlobalOptionService.RemoveOptionChangedHandler(this, OnGlobalOptionChanged);
    }

    private void OnGlobalOptionChanged(object sender, object target, OptionChangedEventArgs args)
    {
        foreach (var (key, value) in args.ChangedOptions)
        {
            if (key.Option.Equals(LanguageServerProjectSystemOptionsStorage.EnableFileBasedPrograms))
            {
                // This event handler can't be async, so we ignore the resulting task here,
                // and take care that the ignored call doesn't throw an exception
                _ = HandleEnableFileBasedProgramsChangedAsync((bool)value!);
                break;
            }
        }

        async Task HandleEnableFileBasedProgramsChangedAsync(bool value)
        {
            using var token = Listener.BeginAsyncOperation(nameof(HandleEnableFileBasedProgramsChangedAsync));
            try
            {
                // Note: Changing the 'enableFileBasedPrograms' setting causes many subtle differences in how loose files are handled.
                // For example, loose files which don't look like file-based programs, are put in projects forked from the canonical project loader, only when the setting is enabled, etc.
                // We anticipate that changing this setting will be infrequent, and, the cost of needing to reload will be acceptable given that.
                _logger.LogInformation($"Detected enableFileBasedPrograms changed to '{value}'. Unloading loose file projects.");
                await UnloadAllProjectsAsync();
                await _canonicalMiscFilesLoader.UnloadAllProjectsAsync();
            }
            catch (Exception ex) when (FatalError.ReportAndCatch(ex, ErrorSeverity.General))
            {
                throw ExceptionUtilities.Unreachable();
            }
        }
    }

    private static string GetDocumentFilePath(DocumentUri uri) => uri.ParsedUri is { } parsedUri ? ProtocolConversions.GetDocumentFilePathFromUri(parsedUri) : uri.UriString;

    public enum LooseDocumentKind
    {
        ProjectBasedApp,
        MiscFile,
        MiscFileWithSemanticErrors,
        FileBasedApp,
    }

    private async ValueTask<LooseDocumentKind> ClassifyDocumentAsync(DocumentUri documentUri, ImmutableDictionary<DocumentUri, TrackedDocumentInfo> trackedDocuments, CancellationToken cancellationToken)
    {
        // roslyn/docs/features/file-based-programs-vscode.md

        // 1. Is the file in a currently loaded project?
        // - Yes → Classify as Project-Based App
        // - No → Continue to next check
        var hostWorkspace = _workspaceFactory.HostWorkspace;
        var hostDocuments = await hostWorkspace.CurrentSolution.GetTextDocumentsAsync(documentUri, cancellationToken);

        // Determine whether an entity separate from FileBasedProgramsProjectSystem, such as CPS or LanguageServerProjectSystem,
        // has loaded a project containing this document into the host workspace.
        var filePath = GetDocumentFilePath(documentUri);
        if (hostDocuments.Any(static (doc, filePath) => doc.Project.FilePath != filePath, filePath))
        {
            return LooseDocumentKind.ProjectBasedApp;
        }

        // TODO2: should probably handle this check in here.
        // 1.1. Is this a script file or Razor file? If so, classify as 'plain misc file'.

        // 2. Is `enableFileBasedPrograms` enabled?
        //    - No → Classify as Misc File
        //    - Yes → Continue to next check
        var enableFileBasedPrograms = GlobalOptionService.GetOption(LanguageServerProjectSystemOptionsStorage.EnableFileBasedPrograms);
        if (!enableFileBasedPrograms)
        {
            return LooseDocumentKind.MiscFile;
        }

        // 3. Does the file have an absolute path? (i.e. it represents a file on disk, and it is not a "virtual document" created for a new, not-yet-saved file, or similar.)
        // - Yes → Go to (4)
        // - No → Go to (5)

        // 4. Does the file have `#:` or `#!` directives?
        // - Yes → Classify as File-Based App. Restore if needed and show semantic errors.
        // - No → Continue to next check
        if (filePath is { }
            && PathUtilities.IsAbsolute(filePath)
            && VirtualProjectXmlProvider.HasFileBasedAppDirectives(trackedDocuments[documentUri].SourceText))
        {
            return LooseDocumentKind.FileBasedApp;
        }

        // 5. Is `enableFileBasedProgramsWhenAmbiguous` enabled? (default: `false` in release, `true` in prerelease)
        // - No → Classify as Misc File
        // - Yes → Continue to heuristic detection
        if (!GlobalOptionService.GetOption(LanguageServerProjectSystemOptionsStorage.EnableFileBasedProgramsWhenAmbiguous))
        {
            return LooseDocumentKind.MiscFile;
        }

        // Heuristic Detection:

        // 6. Are top-level statements present?
        // - No → Classify as Misc File
        // - Yes → Continue to next check

        // Use an existing syntax tree from misc files workspace, if present.
        // Otherwise we will have to do a parse (unfortunately).
        var existingDoc = _workspaceFactory.MiscellaneousFilesWorkspace.CurrentSolution.GetTextDocuments(documentUri).OfType<Document>().FirstOrDefault();
        var syntaxTree = existingDoc is { } ? await existingDoc.GetSyntaxTreeAsync(cancellationToken) : null;
        syntaxTree ??= CSharpSyntaxTree.ParseText(trackedDocuments[documentUri].SourceText, cancellationToken: cancellationToken);

        var containsTopLevelStatements = syntaxTree.GetRoot(cancellationToken) is CompilationUnitSyntax compilationUnit
            && compilationUnit.Members.Any(SyntaxKind.GlobalStatement);
        if (!containsTopLevelStatements)
        {
            return LooseDocumentKind.MiscFile;
        }

        // 7. Is the file included in a `.csproj` cone?
        //    - "Cone" means that a containing directory, at some level of nesting, has a `.csproj` file in it.
        //    - Note that this specific check is only performed at the time the file is opened. We think that the typical case is that the user will load a new project they are creating. Loading the project will cause the file to start being treated as project-based app per (1). If the user does not load the new project, then stale diagnostics may remain present until the file is closed and re-opened.
        //    - Yes → Classify as Misc File (wait for project to load)
        //    - No → Classify as Misc File w/ Semantic Errors

        // TODO2: the result of this check should be cached, watched and invalidated appropriately, by a self-contained component
        if (filePath is { } && CheckIsContainedInCsprojCone(filePath))
        {
            return LooseDocumentKind.MiscFile;
        }

        return LooseDocumentKind.MiscFileWithSemanticErrors;
    }

    private async ValueTask<TextDocument?> GetOrLoadDocumentCoreAsync(TextDocumentIdentifier textDocumentIdentifier, LooseDocumentKind documentKind, ImmutableDictionary<DocumentUri, TrackedDocumentInfo> trackedDocuments, CancellationToken cancellationToken)
    {
        var documentUri = textDocumentIdentifier.DocumentUri;
        if (documentKind is LooseDocumentKind.ProjectBasedApp)
        {
            var documents = await _workspaceFactory.HostWorkspace.CurrentSolution.GetTextDocumentsAsync(documentUri, cancellationToken).ConfigureAwait(false);
            if (documents is [])
            {
                _logger.LogWarning("Classified document '{documentUri}' as project-based, then didn't find a document for it in the host workspace.", documentUri);
                return null;
            }

            return documents.FindDocumentInProjectContext(textDocumentIdentifier, (sln, id) => sln.GetRequiredTextDocument(id));
        }
        // TODO2: below cases should log when adding a new document as in the original LspWorkspaceManager code
        else if (documentKind is LooseDocumentKind.FileBasedApp)
        {
            return await GetOrLoadFileBasedAppAsync();
        }
        else if (documentKind is LooseDocumentKind.MiscFile or LooseDocumentKind.MiscFileWithSemanticErrors)
        {
            return await GetOrLoadMiscFileAsync();
        }
        else
        {
            throw ExceptionUtilities.UnexpectedValue(documentKind);
        }

        async ValueTask<TextDocument> GetOrLoadFileBasedAppAsync()
        {
            var documents = await _workspaceFactory.HostWorkspace.CurrentSolution.GetTextDocumentsAsync(documentUri, cancellationToken).ConfigureAwait(false);
            // TODO2: We need to test a file based app which sets `#:property TargetFrameworks=...`
            // which could violate this SingleOrDefault assumption (and require us to pass a full TextDocumentIdentifier to select the right project).
            var fileBasedDoc = documents.SingleOrDefault(doc => doc.Project.FilePath == GetDocumentFilePath(documentUri));
            if (fileBasedDoc is { })
                return fileBasedDoc;

            var documentInfo = trackedDocuments[documentUri];
            var languageInfoProvider = _lspServices.GetRequiredService<ILanguageInfoProvider>();
            if (!languageInfoProvider.TryGetLanguageInformation(documentUri, documentInfo.LanguageId, out var languageInformation))
            {
                Contract.Fail($"Could not find language information for '{documentUri}'");
            }

            // Note: for simplicity, the file-based app projects are always put in the host workspace, even when in the primordial state.
            var primordialDoc = AddPrimordialDocument(_workspaceFactory.HostProjectFactory, GetDocumentFilePath(documentUri), documentInfo.SourceText, languageInformation);
            Contract.ThrowIfNull(primordialDoc.FilePath);
            await BeginLoadingProjectWithPrimordialAsync(primordialDoc.FilePath, _workspaceFactory.HostProjectFactory, primordialProjectId: primordialDoc.Project.Id, doDesignTimeBuild: true);
            return primordialDoc;
        }

        async ValueTask<TextDocument> GetOrLoadMiscFileAsync()
        {
            var documents = await _workspaceFactory.MiscellaneousFilesWorkspace.CurrentSolution.GetTextDocumentsAsync(documentUri, cancellationToken).ConfigureAwait(false);
            var miscDoc = documents.SingleOrDefault();
            if (miscDoc is { })
                return miscDoc;

            var documentInfo = trackedDocuments[documentUri];
            var languageInfoProvider = _lspServices.GetRequiredService<ILanguageInfoProvider>();
            if (!languageInfoProvider.TryGetLanguageInformation(documentUri, documentInfo.LanguageId, out var languageInformation))
            {
                Contract.Fail($"Could not find language information for '{documentUri}'");
            }

            // TODO2: Do not use canonical loader when enableFileBasedPrograms: false.
            // Perhaps finer-grained classification like 'RichMisc' vs 'Misc' vs 'MiscWithSemanticErrors' is needed
            return await _canonicalMiscFilesLoader.AddMiscellaneousDocumentAsync(GetDocumentFilePath(documentUri), documentInfo.SourceText, cancellationToken);
        }
    }

    private async ValueTask UpdateWorkspaceStateAsync(DocumentUri documentUri, LooseDocumentKind documentKind)
    {
        var filePath = GetDocumentFilePath(documentUri);
        if (documentKind is LooseDocumentKind.ProjectBasedApp)
        {
            // Unload any file-based app projects and misc files projects we had for it.
            if (filePath is { })
            {
                await TryUnloadProjectAsync(filePath);
                await _canonicalMiscFilesLoader.TryUnloadProjectAsync(filePath);
            }
        }
        else if (documentKind is LooseDocumentKind.FileBasedApp)
        {
            // Unload any misc files project we had for it.
            if (filePath is { })
            {
                await _canonicalMiscFilesLoader.TryUnloadProjectAsync(filePath);
            }
        }
        else if (documentKind is LooseDocumentKind.MiscFile)
        {
            // Ensure HasAllInformation is disabled.
            var miscDocument = _workspaceFactory.MiscellaneousFilesWorkspace.CurrentSolution.GetTextDocuments(documentUri).SingleOrDefault();
            if (miscDocument is { Project: { State.HasAllInformation: true, Id: var projectId } })
            {
                _workspaceFactory.MiscellaneousFilesWorkspaceProjectFactory.ApplyChangeToWorkspace(
                    workspace => workspace.OnHasAllInformationChanged(projectId, hasAllInformation: false));
            }
        }
        else if (documentKind is LooseDocumentKind.MiscFileWithSemanticErrors)
        {
            // Ensure HasAllInformation is enabled
            var miscDocument = _workspaceFactory.MiscellaneousFilesWorkspace.CurrentSolution.GetTextDocuments(documentUri).SingleOrDefault();
            if (miscDocument is { Project: { State.HasAllInformation: false, Id: var projectId } })
            {
                _workspaceFactory.MiscellaneousFilesWorkspaceProjectFactory.ApplyChangeToWorkspace(
                    workspace => workspace.OnHasAllInformationChanged(projectId, hasAllInformation: true));
            }
        }
        else
        {
            throw ExceptionUtilities.UnexpectedValue(documentKind);
        }
    }

    public bool ManagesWorkspace(Workspace workspace)
    {
        return workspace == _workspaceFactory.HostWorkspace || workspace == _workspaceFactory.MiscellaneousFilesWorkspace;
    }

    public async ValueTask<TextDocument?> GetOrLoadDocumentAsync(TextDocumentIdentifier textDocumentIdentifier, ImmutableDictionary<DocumentUri, TrackedDocumentInfo> trackedDocuments, CancellationToken cancellationToken)
    {
        var documentUri = textDocumentIdentifier.DocumentUri;
        var documentKind = await ClassifyDocumentAsync(documentUri, trackedDocuments, cancellationToken);
        _logger.LogDebug("Classified '{documentUri}' as '{documentKind}'", documentUri, documentKind);
        await UpdateWorkspaceStateAsync(documentUri, documentKind);
        return await GetOrLoadDocumentCoreAsync(textDocumentIdentifier, documentKind, trackedDocuments, cancellationToken);
    }

    private bool CheckIsContainedInCsprojCone(string csFilePath)
    {
        // We only do csproj-in-cone checks if the file is contained in a currently opened workspace folder
        if (WorkspaceFoldersOpt.IsDefaultOrEmpty)
            return false;

        // When the path is not absolute (for virtual documents, etc), we can't perform this search.
        // Optimistically assume there is no csproj in cone.
        if (!PathUtilities.IsAbsolute(csFilePath))
            return false;

        // Precondition: opened workspace folder paths, have already been deduplicated to remove folders in the same hierarchy.
        // e.g. 'workspaceFolderPaths' will not contain both `C:\src\roslyn`, and `C:\src\roslyn\docs`.
        var containingWorkspacePath = WorkspaceFoldersOpt.FirstOrDefault(
            (workspacePath, csFilePath) => PathUtilities.IsSameDirectoryOrChildOf(child: csFilePath, parent: workspacePath), arg: csFilePath);
        if (containingWorkspacePath is null)
            return false;

        var directoryName = PathUtilities.GetDirectoryName(csFilePath);
        while (PathUtilities.IsSameDirectoryOrChildOf(child: directoryName, parent: containingWorkspacePath))
        {
            var containsCsproj = Directory.EnumerateFiles(directoryName, "*.csproj").Any();
            if (containsCsproj)
                return true;

            directoryName = PathUtilities.GetDirectoryName(directoryName);
        }

        return false;
    }

    public async ValueTask<TextDocument?> AddMiscellaneousDocumentAsync(DocumentUri uri, SourceText documentText, string languageId, ILspLogger logger)
    {
        var documentFilePath = GetDocumentFilePath(uri);
        var languageInfoProvider = _lspServices.GetRequiredService<ILanguageInfoProvider>();
        if (!languageInfoProvider.TryGetLanguageInformation(uri, languageId, out var languageInformation))
        {
            Contract.Fail($"Could not find language information for {uri} with absolute path {documentFilePath}");
        }

        var supportsDesignTimeBuild = languageInformation.LanguageName == LanguageNames.CSharp
            && (languageInformation.ScriptExtension is null || languageInformation.ScriptExtension != PathUtilities.GetExtension(documentFilePath))
            && GlobalOptionService.GetOption(LanguageServerProjectSystemOptionsStorage.EnableFileBasedPrograms);

        // Check if this is a C# file that should use the canonical misc files loader
        if (supportsDesignTimeBuild)
        {
            // For virtual (non-file) URIs or non-file-based programs, use the canonical loader
            if (uri.ParsedUri is null || !uri.ParsedUri.IsFile || !VirtualProjectXmlProvider.HasFileBasedAppDirectives(documentText))
            {
                return await _canonicalMiscFilesLoader.AddMiscellaneousDocumentAsync(documentFilePath, documentText, CancellationToken.None);
            }
        }

        // Use the original file-based programs logic
        var primordialDoc = AddPrimordialDocument(_workspaceFactory.MiscellaneousFilesWorkspaceProjectFactory, documentFilePath, documentText, languageInformation);
        Contract.ThrowIfNull(primordialDoc.FilePath);

        var doDesignTimeBuild = uri.ParsedUri?.IsFile is true && supportsDesignTimeBuild;
        await BeginLoadingProjectWithPrimordialAsync(primordialDoc.FilePath, _workspaceFactory.MiscellaneousFilesWorkspaceProjectFactory, primordialProjectId: primordialDoc.Project.Id, doDesignTimeBuild);

        return primordialDoc;
    }

    private TextDocument AddPrimordialDocument(ProjectSystemProjectFactory projectFactory, string documentFilePath, SourceText documentText, LanguageInformation languageInformation)
    {
        var workspace = projectFactory.Workspace;
        var sourceTextLoader = new SourceTextLoader(documentText, documentFilePath);
        var enableFileBasedPrograms = GlobalOptionService.GetOption(LanguageServerProjectSystemOptionsStorage.EnableFileBasedPrograms);
        var projectInfo = MiscellaneousFileUtilities.CreateMiscellaneousProjectInfoForDocument(
            workspace, documentFilePath, sourceTextLoader, languageInformation, documentText.ChecksumAlgorithm, workspace.Services.SolutionServices, [], enableFileBasedPrograms);

        projectFactory.ApplyChangeToWorkspace(workspace => workspace.OnProjectAdded(projectInfo));

        // https://github.com/dotnet/roslyn/pull/78267
        // Work around an issue where opening a Razor file in the misc workspace causes a crash.
        if (languageInformation.LanguageName == LanguageInfoProvider.RazorLanguageName)
        {
            var docId = projectInfo.AdditionalDocuments.Single().Id;
            return workspace.CurrentSolution.GetRequiredAdditionalDocument(docId);
        }

        var id = projectInfo.Documents.Single().Id;
        return workspace.CurrentSolution.GetRequiredDocument(id);
    }

    public async ValueTask<bool> TryRemoveMiscellaneousDocumentAsync(DocumentUri uri)
    {
        var documentPath = GetDocumentFilePath(uri);
        // First try to remove from the canonical misc files loader if it was created
        var removedFromCanonical = await _canonicalMiscFilesLoader.TryUnloadProjectAsync(documentPath);
        if (removedFromCanonical)
            return true;

        // Fall back to the file-based programs logic
        return await TryUnloadProjectAsync(documentPath);
    }

    protected override async Task<RemoteProjectLoadResult?> TryLoadProjectInMSBuildHostAsync(
        BuildHostProcessManager buildHostProcessManager, string documentPath, CancellationToken cancellationToken)
    {
        var content = await _projectXmlProvider.GetVirtualProjectContentAsync(documentPath, _logger, cancellationToken);
        if (content is not var (virtualProjectContent, diagnostics))
        {
            // https://github.com/dotnet/roslyn/issues/78618: falling back to this until dotnet run-api is more widely available
            _logger.LogInformation($"Failed to obtain virtual project for '{documentPath}' using dotnet run-api. Falling back to directly creating the virtual project.");
            virtualProjectContent = VirtualProjectXmlProvider.MakeVirtualProjectContent_DirectFallback(documentPath);
            diagnostics = [];
        }

        foreach (var diagnostic in diagnostics)
        {
            _logger.LogError($"{diagnostic.Location.Path}{diagnostic.Location.Span.Start}: {diagnostic.Message}");
        }

        // When loading a virtual project, the path to the on-disk source file is not used. Instead the path is adjusted to end with .csproj.
        // This is necessary in order to get msbuild to apply the standard c# props/targets to the project.
        var virtualProjectPath = VirtualProjectXmlProvider.GetVirtualProjectPath(documentPath);
        const BuildHostProcessKind buildHostKind = BuildHostProcessKind.NetCore;
        var buildHost = await buildHostProcessManager.GetBuildHostAsync(buildHostKind, virtualProjectPath, dotnetPath: null, cancellationToken);
        var loadedFile = await buildHost.LoadProjectAsync(virtualProjectPath, virtualProjectContent, languageName: LanguageNames.CSharp, cancellationToken);

        return new RemoteProjectLoadResult
        {
            ProjectFile = loadedFile,
            // If we have made it this far, we must have determined that the document is a file-based program.
            // TODO: we should assert this somehow. However, we cannot use the on-disk state of the file to do so, because the decision to load this as a file-based program was based on in-editor content.
            ProjectFactory = _workspaceFactory.HostProjectFactory,
            IsFileBasedProgram = true,
            IsMiscellaneousFile = false,
            PreferredBuildHostKind = buildHostKind,
            ActualBuildHostKind = buildHostKind,
        };
    }

    protected override async ValueTask TransitionPrimordialProjectToLoaded_NoLockAsync(
        Dictionary<string, ProjectLoadState> loadedProjects,
        string projectPath,
        ProjectLoadState.Primordial projectState,
        CancellationToken cancellationToken)
    {
        await projectState.PrimordialProjectFactory.ApplyChangeToWorkspaceAsync(
            workspace => workspace.OnProjectRemoved(projectState.PrimordialProjectId),
            cancellationToken);
    }

    public Task OnInitializedAsync(ClientCapabilities clientCapabilities, RequestContext context, CancellationToken cancellationToken)
    {
        var initializeManager = context.GetRequiredService<IInitializeManager>();
        if (initializeManager.TryGetInitializeParams() is { WorkspaceFolders: [_, ..] nonEmptyWorkspaceFolders })
        {
            var nonOverlappingWorkspacePaths = getNonOverlappingFolderPaths(nonEmptyWorkspaceFolders);
            this.WorkspaceFoldersOpt = nonOverlappingWorkspacePaths;
        }

        return Task.CompletedTask;

        ImmutableArray<string> getNonOverlappingFolderPaths(WorkspaceFolder[] workspaceFolders)
        {
            var builder = ArrayBuilder<string>.GetInstance(workspaceFolders.Length);
            foreach (var workspaceFolder in workspaceFolders)
            {
                // Only care about real, on-disk folders
                if (workspaceFolder.DocumentUri.ParsedUri is not { } parsedUri)
                    continue;

                var currentPath = ProtocolConversions.GetDocumentFilePathFromUri(parsedUri);
                // When multiple folders are in the same hierarchy, take the higher one and drop the lower one.
                var existingIndex = builder.FindIndex((oldPath, currentPath) => PathUtilities.IsSameDirectoryOrChildOf(child: oldPath, parent: currentPath), currentPath);
                if (existingIndex != -1)
                    builder[existingIndex] = currentPath;
                else
                    builder.Add(currentPath);
            }

            return builder.ToImmutableAndFree();
        }
    }
}
