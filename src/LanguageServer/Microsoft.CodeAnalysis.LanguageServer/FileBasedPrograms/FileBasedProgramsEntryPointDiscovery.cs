// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#define COUNTFILES

using System.Collections.Immutable;
using System.Composition;
using System.Diagnostics;
using System.IO.Enumeration;
using System.Text.Json;
using System.Text.Json.Serialization;
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

internal sealed partial class FileBasedProgramsEntryPointDiscovery(LanguageServerWorkspaceFactory workspaceFactory, ILoggerFactory loggerFactory, LspServices lspServices) : ILspService, IOnInitialized
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

        // Invariant: If a directory is contained in 'checkedDirectories', its parent is also contained in 'checkedDirectories'.
        var checkedDirectories = PooledDictionary<string, Node>.GetInstance();
        var fileBasedAppNodes = ArrayBuilder<Node>.GetInstance();
        foreach (var folder in _workspaceFolders)
        {
            checkedDirectories.Clear();
            // TODO2: deserialize from temp if available.
            // TODO2: Need to deal with files being deleted or kind changed from the previous run.
            var rootNode = new Node(folder, isDirectory: true, parent: null);

            // Top-level bailout
            if (Directory.EnumerateFiles(folder, "*.csproj").Any())
            {
                _logger.LogDebug("'{folder}' contains a csproj file, so not searching it for entry points.", folder);
                continue;
            }

            var stopwatch1 = Stopwatch.StartNew();
            _logger.LogDebug("Starting file-based app entry point discovery in '{folder}'", folder);
            // Note: the common case by far is that there is a single workspace folder.
            foreach (var csFilePath in EnumeratePossibleEntryPoints(folder, rootNode))
            {
                _logger.LogInformation("Discovered file-based app entry point: {csFilePath}", csFilePath);
                Interlocked.Increment(ref discoveredCount);
                var textLoader = new WorkspaceFileTextLoader(solutionServices, csFilePath, defaultEncoding: null);

                // TODO2: Address threading issues.
                // Maybe this would be made simpler by eliminating Primordial state.
                _ = await fileBasedProgramsProjectSystem.BeginLoadingFileBasedAppAsync(csFilePath, textLoader, languageInfo);
            }
            stopwatch1.Stop();
            _logger.LogInformation("WS {folder} discovery count {discoveredCount} in {stopwatch.ElapsedMilliseconds} milliseconds.", folder, discoveredCount, stopwatch1.ElapsedMilliseconds);

            // Clear the ContainsFileBasedApp flag on all checked directories
            foreach (var node in checkedDirectories.Values)
                node.IsOrContainsFileBasedApp = false;

            // Set the flag again on the appropriate directories by walking up the file-based app nodes.
            // Note: If the directory was reported to contained file-based apps, we should have checked it, even if it was up to date.
            foreach (var node in fileBasedAppNodes)
            {
                Contract.ThrowIfFalse(!node.IsDirectory && node.IsOrContainsFileBasedApp);
                for (var currentNode = node; currentNode.Parent?.IsOrContainsFileBasedApp == false; currentNode = currentNode.Parent)
                    currentNode.IsOrContainsFileBasedApp = true;
            }

            // Note: we want to keep info about even the child directories which didn't contain file-based apps.
            // This lets us run minimal searches when a file is added to a subdirectory next time.

            // TODO2: stop and inspect.
            int x = 42;
            var content = JsonSerializer.Serialize(rootNode, NodeSerializerContext.Default.Node);

            checkedDirectories.Clear();

            stopwatch1.Start();
            // Note: the common case by far is that there is a single workspace folder.
            foreach (var csFilePath in EnumeratePossibleEntryPoints(folder, rootNode))
            {
                _logger.LogInformation("Discovered file-based app entry point again : {csFilePath}", csFilePath);
            }

            _logger.LogInformation("WS {folder} incremental rediscovery count {discoveredCount} in {stopwatch.ElapsedMilliseconds} milliseconds.", folder, discoveredCount, stopwatch.ElapsedMilliseconds);
            stopwatch1.Stop();

            // Update directory contains flags.. how?
            // var stack = ArrayBuilder<Node>.GetInstance();
            // stack.Push(rootNode);
        }

        checkedDirectories.Free();
        stopwatch.Stop();
        _logger.LogInformation("MAGIC Discovered {discoveredCount} file-based app entry points in {stopwatch.ElapsedMilliseconds} milliseconds.", discoveredCount, stopwatch.ElapsedMilliseconds);
#if COUNTFILES
        _logger.LogDebug("Checked {checkedCount} C# files during discovery.", csFileCount);
#endif

        FileSystemEnumerable<string> EnumeratePossibleEntryPoints(string directory, Node rootNode)
        {
            Contract.ThrowIfFalse(checkedDirectories.Count == 0);
            Contract.ThrowIfFalse(fileBasedAppNodes.IsEmpty);
            checkedDirectories.Add(rootNode.FullPath, rootNode);

            return new FileSystemEnumerable<string>(
                directory, transform: (ref entry) => entry.ToFullPath(), options: new() { RecurseSubdirectories = true })
            {
                ShouldIncludePredicate = ShouldInclude,
                ShouldRecursePredicate = ShouldRecurse
            };

            bool ShouldInclude(ref FileSystemEntry entry)
            {
                if (entry.IsDirectory || !Path.GetExtension(entry.FileName).Equals(".cs", StringComparison.Ordinal))
                    return false;

                var currentDirectoryNode = checkedDirectories.GetAlternateLookup<ReadOnlySpan<char>>()[entry.Directory];
                var fullPath = entry.ToFullPath();
                if (currentDirectoryNode.Children.TryGetValue(fullPath, out var childNode))
                {
                    if (childNode.LastWriteTimeUtc == entry.LastWriteTimeUtc)
                        return childNode.IsOrContainsFileBasedApp;
                }
                else
                {
                    childNode = currentDirectoryNode.AddChild(fullPath, isDirectory: false);
                }

                childNode.LastWriteTimeUtc = entry.LastWriteTimeUtc;
                using var fileStream = ((FileInfo)entry.ToFileSystemInfo()).OpenRead();
                childNode.IsOrContainsFileBasedApp = VirtualProjectXmlProvider.HasFileBasedAppDirectives(SourceText.From(fileStream)); // TODO2: read only partial content
                if (childNode.IsOrContainsFileBasedApp)
                    fileBasedAppNodes.Add(childNode);

                Interlocked.Increment(ref csFileCount);
                // searchedCandidatesFile.WriteLine(entry.ToFullPath());
                return childNode.IsOrContainsFileBasedApp;
            }

            bool ShouldRecurse(ref FileSystemEntry entry)
            {
                var currentDirectoryNode = checkedDirectories.GetAlternateLookup<ReadOnlySpan<char>>()[entry.Directory];
                var fullPath = entry.ToFullPath();
                if (currentDirectoryNode.Children.TryGetValue(fullPath, out var childNode))
                {
                    if (childNode.LastWriteTimeUtc == entry.LastWriteTimeUtc)
                        return childNode.IsOrContainsFileBasedApp;
                }
                else
                {
                    childNode = currentDirectoryNode.AddChild(fullPath, isDirectory: true);
                }

                childNode.LastWriteTimeUtc = entry.LastWriteTimeUtc;
                var containsCsproj = Directory.EnumerateFiles(fullPath, "*.csproj").Any();
                if (containsCsproj)
                {
                    childNode.IsOrContainsFileBasedApp = false;
                    childNode.Children.Clear();
                    return false;
                }

                checkedDirectories.Add(childNode.FullPath, childNode);
                return true;
            }
        }
    }

    /// <summary>
    /// Cache of directories and files where we found file-based apps in a workspace folder.
    /// Isomorphic to workspace directory structure.
    /// Used on subsequent startups to reduce file system activity.
    /// </summary>
    internal class Node(string fullPath, bool isDirectory, Node? parent)
    {
        public string FullPath { get; } = fullPath;
        public bool IsDirectory { get; } = isDirectory;

        [JsonIgnore] // TODO2: how to wire this up when deserializing?
        public Node? Parent { get; } = parent;

        public DateTimeOffset LastWriteTimeUtc { get; set; }
        public bool IsOrContainsFileBasedApp { get; set; }

        public Dictionary<string, Node> Children { get; } = [];

        public Node AddChild(string fullPath, bool isDirectory)
        {
            Contract.ThrowIfFalse(IsDirectory);
            var child = new Node(fullPath, isDirectory, this);
            Children.Add(fullPath, child);
            return child;
        }
    }

    [JsonSerializable(typeof(Node))]
    internal partial class NodeSerializerContext : JsonSerializerContext;
}
