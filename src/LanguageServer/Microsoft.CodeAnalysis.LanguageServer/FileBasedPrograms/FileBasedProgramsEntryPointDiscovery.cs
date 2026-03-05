// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#define COUNTFILES

using System.Collections.Immutable;
using System.Composition;
using System.Diagnostics;
using System.IO.Enumeration;
using Microsoft.CodeAnalysis.Collections;
using Microsoft.CodeAnalysis.EditorConfig;
using Microsoft.CodeAnalysis.ErrorReporting;
using Microsoft.CodeAnalysis.Features.Workspaces;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.LanguageServer.Handler;
using Microsoft.CodeAnalysis.LanguageServer.HostWorkspace;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Text;
using Microsoft.Extensions.Logging;
using Roslyn.LanguageServer.Protocol;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.LanguageServer.FileBasedPrograms;

[Shared]
[ExportLspServiceFactory(typeof(FileBasedProgramsEntryPointDiscovery), ProtocolConstants.RoslynLspLanguagesContract)]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class FileBasedProgramsEntryPointDiscoveryFactory(LanguageServerWorkspaceFactory workspaceFactory, ILoggerFactory loggerFactory) : ILspServiceFactory
{
    public ILspService CreateILspService(LspServices lspServices, WellKnownLspServerKinds serverKind)
    {

        return new FileBasedProgramsEntryPointDiscovery(workspaceFactory, loggerFactory, lspServices);
    }
}

internal sealed class FileBasedProgramsEntryPointDiscovery(LanguageServerWorkspaceFactory workspaceFactory, ILoggerFactory loggerFactory, LspServices lspServices) : ILspService, IOnInitialized
{
    private readonly ILogger _logger = loggerFactory.CreateLogger<FileBasedProgramsEntryPointDiscovery>();
    private ImmutableArray<string> _workspaceFolders;

    public Task OnInitializedAsync(ClientCapabilities clientCapabilities, RequestContext context, CancellationToken cancellationToken)
    {
        var initializeManager = context.GetRequiredService<IInitializeManager>();
        if (initializeManager.TryGetInitializeParams() is { WorkspaceFolders: [_, ..] nonEmptyWorkspaceFolders })
        {
            _workspaceFolders = GetFolderPaths(nonEmptyWorkspaceFolders);

            // TODO2: ensure EnableFileBasedPrograms is set
            _ = Task.Run(async () =>
            {
                try
                {
                    await FindAndLoadEntryPointsAsync();
                }
                catch (Exception ex) when (FatalError.ReportAndCatch(ex))
                {
                    throw ExceptionUtilities.Unreachable();
                }
            }, cancellationToken);
        }

        return Task.CompletedTask;

        static ImmutableArray<string> GetFolderPaths(WorkspaceFolder[] workspaceFolders)
        {
            var builder = ArrayBuilder<string>.GetInstance(workspaceFolders.Length);
            foreach (var workspaceFolder in workspaceFolders)
            {
                if (workspaceFolder.DocumentUri.ParsedUri is not { } parsedUri)
                    continue;

                var workspaceFolderPath = ProtocolConversions.GetDocumentFilePathFromUri(parsedUri);
                builder.Add(workspaceFolderPath);
            }

            return builder.ToImmutableAndFree();
        }
    }

    private async Task FindAndLoadEntryPointsAsync()
    {
        if (_workspaceFolders.IsDefaultOrEmpty)
        {
            _logger.LogTrace("No workspace folders to search.");
            return;
        }

        if (lspServices.GetService<ILspMiscellaneousFilesWorkspaceProvider>()
            is not FileBasedProgramsProjectSystem fileBasedProgramsProjectSystem)
        {
            _logger.LogWarning("Did not find FileBasedProgramsProjectSystem. Not discovering entry points.");
            return;
        }

        var stopwatch = Stopwatch.StartNew();
        var discoveredCount = 0;
#if COUNTFILES
        var csFileCount = 0;
#endif
        var languageInfo = new LanguageInformation(LanguageNames.CSharp, scriptExtension: ".csx");
        var solutionServices = workspaceFactory.HostWorkspace.Services.SolutionServices;
        // using var searchedCandidatesFile = new StreamWriter(File.Create("/Users/rikkigibson/Desktop/SearchedCandidates.txt"));
        foreach (var folder in _workspaceFolders)
        {
            // Top-level bailout
            if (Directory.EnumerateFiles(folder, "*.csproj").Any())
            {
                _logger.LogDebug("'{folder}' contains a csproj file, so not searching it for entry points.", folder);
                continue;
            }

            _logger.LogDebug("Starting file-based app entry point discovery in '{folder}'", folder);
            // Note: the common case by far is that there is a single workspace folder.
            foreach (var csFilePath in EnumeratePossibleEntryPoints(folder))
            {
                using var fileStream = File.OpenRead(csFilePath);
                // TODO2: would using a workspace text loader help here at all?
                if (!VirtualProjectXmlProvider.HasFileBasedAppDirectives(SourceText.From(fileStream)))
                    continue;

                _logger.LogInformation("Discovered file-based app entry point: {csFilePath}", csFilePath);
                Interlocked.Increment(ref discoveredCount);
                var textLoader = new WorkspaceFileTextLoader(solutionServices, csFilePath, defaultEncoding: null);

                // TODO2: Address threading issues.
                // Maybe this would be made simpler by eliminating Primordial state.
                _ = await fileBasedProgramsProjectSystem.BeginLoadingFileBasedAppAsync(csFilePath, textLoader, languageInfo);
            }
        }
        stopwatch.Stop();
        _logger.LogInformation("MAGIC Discovered {discoveredCount} file-based app entry points in {stopwatch.ElapsedMilliseconds} milliseconds.", discoveredCount, stopwatch.ElapsedMilliseconds);
#if COUNTFILES
        _logger.LogDebug("Checked {checkedCount} C# files during discovery.", csFileCount);
#endif
        FileSystemEnumerable<string> EnumeratePossibleEntryPoints(string directory)
        {
            return new FileSystemEnumerable<string>(
                directory, transform: (ref entry) => entry.ToFullPath(), options: new() { RecurseSubdirectories = true })
            {
                ShouldIncludePredicate = ShouldInclude,
                ShouldRecursePredicate = ShouldRecurse
            };

            bool ShouldInclude(ref FileSystemEntry entry)
            {
                // Only very cheap checks of the 'entry' itself are performed here.
                // Actually cracking the file is in parallel
                if (entry.IsDirectory || !Path.GetExtension(entry.FileName).Equals(".cs", StringComparison.Ordinal))
                    return false;

                Interlocked.Increment(ref csFileCount);
                // searchedCandidatesFile.WriteLine(entry.ToFullPath());
                return true;
            }

            bool ShouldRecurse(ref FileSystemEntry entry)
            {
                var directoryInfo = (DirectoryInfo)entry.ToFileSystemInfo();
                return !directoryInfo.EnumerateFiles("*.csproj").Any();
            }
        }
    }
}