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
using Microsoft.CodeAnalysis.ErrorReporting;
using Microsoft.CodeAnalysis.Features.Workspaces;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.LanguageServer.Handler;
using Microsoft.CodeAnalysis.LanguageServer.HostWorkspace;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Utilities;
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
    private static readonly StringComparer s_pathComparison = StringComparer.OrdinalIgnoreCase;
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
            _logger.LogDebug("No workspace folders to search for file-based apps.");
            return;
        }

        if (lspServices.GetService<ILspMiscellaneousFilesWorkspaceProvider>()
            is not FileBasedProgramsProjectSystem fileBasedProgramsProjectSystem)
        {
            _logger.LogWarning("Did not find FileBasedProgramsProjectSystem. Not discovering entry points.");
            return;
        }

        var languageInfo = new LanguageInformation(LanguageNames.CSharp, scriptExtension: ".csx");
        var solutionServices = workspaceFactory.HostWorkspace.Services.SolutionServices;

        // Note: the overwhelmingly common case is when there is just one workspace folder.
        // For simplicity we orient our search around one workspace folder at a time.
        foreach (var workspaceFolder in _workspaceFolders)
        {
            await FindAndLoadEntryPointsAsync(workspaceFolder);
        }

        async Task FindAndLoadEntryPointsAsync(string workspaceFolder)
        {
            var stopwatch = Stopwatch.StartNew();
            var cachePath = VirtualProjectXmlProvider.GetDiscoveryCachePath(workspaceFolder);
            Cache? cache = null;
            try
            {
                using var cacheFile = File.OpenRead(cachePath);
                cache = JsonSerializer.Deserialize(cacheFile, CacheSerializerContext.Default.Cache);
            }
            catch (Exception ex)
            {
                _logger.LogDebug("Could not read cache file: {ex.Message}", ex.Message);
            }

            cache ??= new Cache(workspaceFolder, DateTimeOffset.MinValue, FileBasedAppFullPaths: [], DirectoriesContainingCsproj: []);

            // Record a timestamp before checking/traversing workspace contents,
            // so that changes that happen concurrently with this walk, are caught by the next walk
            var walkStartTimeUtc = DateTimeOffset.UtcNow;

            // Load known file-based apps from cache
            var newFileBasedAppsBuilder = ArrayBuilder<string>.GetInstance(cache.FileBasedAppFullPaths.Length);
            foreach (var fileBasedAppPath in cache.FileBasedAppFullPaths)
            {
                var fileInfo = new FileInfo(fileBasedAppPath);
                if (!fileInfo.Exists)
                {
                    // Deleted since our last walk.
                    continue;
                }

                if (fileInfo.LastWriteTimeUtc > cache.LastWalkTimeUtc && !IsFileBasedApp(fileInfo.FullName))
                {
                    // Changed to stop being a file-based app since our last walk.
                    continue;
                }

                newFileBasedAppsBuilder.Add(fileBasedAppPath);
                _logger.LogInformation("MAGIC Discovered file-based app (cache hit): {fileBasedAppPath}", fileBasedAppPath);
                await BeginLoadingAsync(fileBasedAppPath);
            }

            // Search for changes since our last walk.
            var directoriesContainingCsprojBuilder = ArrayBuilder<string>.GetInstance(cache.FileBasedAppFullPaths.Length);
            var enumerator = new IncrementalEntryPointEnumerator(cache, directoriesContainingCsprojBuilder);
            while (enumerator.MoveNext())
            {
                var fileBasedAppPath = enumerator.Current;
                newFileBasedAppsBuilder.Add(fileBasedAppPath);
                _logger.LogInformation("MAGIC Discovered file-based app (cache miss): {csFilePath}", fileBasedAppPath);
                await BeginLoadingAsync(fileBasedAppPath);
            }

            stopwatch.Stop();
            _logger.LogInformation("MAGIC Finished discovery in {workspaceFolder} in {stopwatch.ElapsedMilliseconds} milliseconds", workspaceFolder, stopwatch.ElapsedMilliseconds);
            newFileBasedAppsBuilder.Sort(s_pathComparison);
            directoriesContainingCsprojBuilder.Sort(s_pathComparison);
            var newCache = new Cache(workspaceFolder, walkStartTimeUtc, newFileBasedAppsBuilder.ToImmutableAndFree(), directoriesContainingCsprojBuilder.ToImmutableAndFree());

            IOUtilities.PerformIO(() =>
            {
                Directory.CreateDirectory(Path.GetDirectoryName(cachePath)!);
                using var file = File.Create(cachePath);
                JsonSerializer.Serialize(file, newCache, CacheSerializerContext.Default.Cache);
            });
        }

        async ValueTask BeginLoadingAsync(string csFilePath)
        {
            var textLoader = new WorkspaceFileTextLoader(solutionServices, csFilePath, defaultEncoding: null);
            _ = await fileBasedProgramsProjectSystem.BeginLoadingFileBasedAppAsync(csFilePath, textLoader, languageInfo);
        }
    }

    /// <summary>Check if discovery should consider this a file-based app.</summary>
    private static bool IsFileBasedApp(string fullPath)
    {
        using var fileStream = File.OpenRead(fullPath);
        var isFileBasedApp = VirtualProjectXmlProvider.HasFileBasedAppDirectives(SourceText.From(fileStream));
        return isFileBasedApp;
    }

    private class IncrementalEntryPointEnumerator(Cache cache, ArrayBuilder<string> directoriesContainingCsprojBuilder)
        : FileSystemEnumerator<string>(cache.WorkspacePath, options: new EnumerationOptions { RecurseSubdirectories = true })
    {
        protected override string TransformEntry(ref FileSystemEntry entry)
            => entry.ToFullPath();

        protected override bool ShouldIncludeEntry(ref FileSystemEntry entry)
        {
            if (entry.IsDirectory || !Path.GetExtension(entry.FileName).Equals(".cs", StringComparison.OrdinalIgnoreCase))
            {
                // Cheap check indicates this is not a file-based app.
                return false;
            }

            if (entry.LastWriteTimeUtc <= cache.LastWalkTimeUtc)
            {
                // Already up to date. If it is an FBA, it was visited by the initial cache loop.
                return false;
            }

            var fullPath = entry.ToFullPath();
            if (cache.FileBasedAppFullPaths.BinarySearch(fullPath, s_pathComparison) >= 0)
            {
                // Already handled by the initial cache loop.
                return false;
            }

            return IsFileBasedApp(fullPath);
        }

        protected override bool ShouldRecurseIntoEntry(ref FileSystemEntry entry)
        {
            var fullPath = entry.ToFullPath();
            if (entry.LastWriteTimeUtc <= cache.LastWalkTimeUtc)
            {
                // Directory timestamps update when the directory contents are changed (i.e. directly contained files are added/deleted/renamed).
                // If our last walk time is newer than the directory last write time, then we know our cached result of whether the directory contains a csproj is still applicable.
                if (cache.DirectoriesContainingCsproj.BinarySearch(fullPath, s_pathComparison) >= 0)
                {
                    // Still contains a csproj. Do not descend.
                    directoriesContainingCsprojBuilder.Add(fullPath);
                    return false;
                }

                return true;
            }

            // Directory contents changed since last walk, see if it contains a csproj file.
            var containsCsproj = Directory.EnumerateFiles(fullPath, "*.csproj").Any();
            if (containsCsproj)
            {
                directoriesContainingCsprojBuilder.Add(fullPath);
                return false;
            }

            return true;
        }
    }

    internal record Cache(string WorkspacePath, DateTimeOffset LastWalkTimeUtc, ImmutableArray<string> FileBasedAppFullPaths, ImmutableArray<string> DirectoriesContainingCsproj)
    {
        public ImmutableArray<string> FileBasedAppFullPaths { get; init; } = FileBasedAppFullPaths.NullToEmpty();
        public ImmutableArray<string> DirectoriesContainingCsproj { get; init; } = DirectoriesContainingCsproj.NullToEmpty();
    }

    [JsonSerializable(typeof(Cache))]
    internal partial class CacheSerializerContext : JsonSerializerContext;
}
