// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

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
using Microsoft.CodeAnalysis.Options;
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
internal sealed class FileBasedProgramsEntryPointDiscoveryFactory(LanguageServerWorkspaceFactory workspaceFactory, IGlobalOptionService globalOptionService, ILoggerFactory loggerFactory) : ILspServiceFactory
{
    public ILspService CreateILspService(LspServices lspServices, WellKnownLspServerKinds serverKind)
    {
        return new FileBasedProgramsEntryPointDiscovery(workspaceFactory, globalOptionService, loggerFactory, lspServices);
    }
}

internal sealed partial class FileBasedProgramsEntryPointDiscovery(
    LanguageServerWorkspaceFactory workspaceFactory, IGlobalOptionService globalOptionService, ILoggerFactory loggerFactory, LspServices lspServices) : ILspService, IOnInitialized
{
    private static readonly StringComparer s_pathComparer = StringComparer.OrdinalIgnoreCase;

    /// <summary>Directories which are ignored per convention.</summary>
    /// <remarks>Some conventional directories like '.git' and '.vs' are expected to be marked hidden and will be automatically ignored by discovery.</remarks>
    private static readonly ImmutableArray<string> s_ignoredDirectories = [
        "artifacts",
        "bin",
        "obj",
        "node_modules"
    ];

    private readonly ILogger _logger = loggerFactory.CreateLogger<FileBasedProgramsEntryPointDiscovery>();
    private ImmutableArray<string> _workspaceFolders;

    public Task OnInitializedAsync(ClientCapabilities clientCapabilities, RequestContext context, CancellationToken cancellationToken)
    {
        var initializeManager = context.GetRequiredService<IInitializeManager>();
        if (initializeManager.TryGetInitializeParams() is { WorkspaceFolders: [_, ..] nonEmptyWorkspaceFolders })
        {
            _workspaceFolders = GetFolderPaths(nonEmptyWorkspaceFolders);

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

    internal async Task FindAndLoadEntryPointsAsync()
    {
        if (!globalOptionService.GetOption(LanguageServerProjectSystemOptionsStorage.EnableFileBasedPrograms))
            return;

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
            foreach (var fileBasedAppPath in FindEntryPoints(workspaceFolder))
            {
                await BeginLoadingAsync(fileBasedAppPath);
            }
        }

        async ValueTask BeginLoadingAsync(string csFilePath)
        {
            var textLoader = new WorkspaceFileTextLoader(solutionServices, csFilePath, defaultEncoding: null);
            _ = await fileBasedProgramsProjectSystem.AddDocumentAsync(csFilePath, textLoader, languageInfo);
        }
    }

    internal IEnumerable<string> FindEntryPoints(string workspaceFolder)
    {
        var stopwatch = Stopwatch.StartNew();
        var cachePath = Path.Join(VirtualProjectXmlProvider.GetDiscoveryCacheDirectory(workspaceFolder), "cache.json");
        Cache? cache = null;
        try
        {
            using var cacheFile = File.OpenRead(cachePath);
            cache = JsonSerializer.Deserialize(cacheFile, CacheSerializerContext.Default.Cache);

            // Drop malformed caches
            if (cache?.WorkspacePath.Equals(workspaceFolder, StringComparison.OrdinalIgnoreCase) == false
                || cache is { FileBasedAppFullPaths.IsDefault: true } or { DirectoriesContainingCsproj.IsDefault: true })
            {
                cache = null;
            }
        }
        catch (Exception ex)
        {
            _logger.LogDebug("Could not read cache file: {ex.Message}", ex.Message);
        }

        cache ??= new Cache(workspaceFolder, DateTimeOffset.MinValue, FileBasedAppFullPaths: [], DirectoriesContainingCsproj: []);

        // Record a timestamp before checking/traversing workspace contents,
        // so that changes that happen concurrently with this walk, are caught by the next walk
        var walkStartTimeUtc = DateTimeOffset.UtcNow;

        // Initial cache loop: load known file-based apps
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
            yield return fileBasedAppPath;
        }

        // Search for changes since our last walk.
        var directoriesContainingCsprojBuilder = ArrayBuilder<string>.GetInstance(cache.DirectoriesContainingCsproj.Length);
        var enumerator = new IncrementalEntryPointEnumerator(cache, directoriesContainingCsprojBuilder);
        while (enumerator.MoveNext())
        {
            var fileBasedAppPath = enumerator.Current;
            newFileBasedAppsBuilder.Add(fileBasedAppPath);
            _logger.LogInformation("MAGIC Discovered file-based app (cache miss): {csFilePath}", fileBasedAppPath);
            yield return fileBasedAppPath;
        }

        stopwatch.Stop();
        _logger.LogInformation("MAGIC Finished discovery in {workspaceFolder} in {stopwatch.ElapsedMilliseconds} milliseconds", workspaceFolder, stopwatch.ElapsedMilliseconds);
        newFileBasedAppsBuilder.Sort(s_pathComparer);
        directoriesContainingCsprojBuilder.Sort(s_pathComparer);
        var newCache = new Cache(workspaceFolder, walkStartTimeUtc, newFileBasedAppsBuilder.ToImmutableAndFree(), directoriesContainingCsprojBuilder.ToImmutableAndFree());

        IOUtilities.PerformIO(() =>
        {
            Directory.CreateDirectory(Path.GetDirectoryName(cachePath)!);
            using var file = File.Create(cachePath);
            JsonSerializer.Serialize(file, newCache, CacheSerializerContext.Default.Cache);
        });
    }

    /// <summary>Check if discovery should consider this a file-based app.</summary>
    private static bool IsFileBasedApp(string fullPath)
    {
        using var fileStream = File.OpenRead(fullPath);
        var isFileBasedApp = VirtualProjectXmlProvider.HasFileBasedAppDirectives(SourceText.From(fileStream));
        return isFileBasedApp;
    }

    private class IncrementalEntryPointEnumerator : FileSystemEnumerator<string>
    {
        private readonly Cache _cache;
        private readonly ArrayBuilder<string> _directoriesContainingCsprojBuilder;

        /// <summary>
        /// Directories under the workspace folder which have a newer create/modify timestamp than the last walk time, and their subdirectories.
        /// In this case, items may have been moved into the directory since the last walk.
        /// </summary>
        private readonly HashSet<string> _newerDirectories = new HashSet<string>(s_pathComparer);

        public IncrementalEntryPointEnumerator(Cache cache, ArrayBuilder<string> directoriesContainingCsprojBuilder)
            : base(cache.WorkspacePath, options: new EnumerationOptions { RecurseSubdirectories = true })
        {
            _cache = cache;
            _directoriesContainingCsprojBuilder = directoriesContainingCsprojBuilder;

            // Note: a creation time can be newer than the last write time when a file is copied or moved.
            var workspaceDirectoryInfo = new DirectoryInfo(_cache.WorkspacePath);
            if (workspaceDirectoryInfo.CreationTimeUtc > cache.LastWalkTimeUtc
                || workspaceDirectoryInfo.LastWriteTimeUtc > cache.LastWalkTimeUtc)
            {
                _newerDirectories.Add(workspaceDirectoryInfo.FullName);
            }
        }

        protected override string TransformEntry(ref FileSystemEntry entry)
            => entry.ToFullPath();

        private bool IsCacheUpToDate(ref FileSystemEntry entry)
        {
            // Note: the create timestamp can be newer than the modify timestamp when a file is copied or moved.
            return !_newerDirectories.GetAlternateLookup<ReadOnlySpan<char>>().Contains(entry.Directory)
                && entry.CreationTimeUtc <= _cache.LastWalkTimeUtc
                && entry.LastWriteTimeUtc <= _cache.LastWalkTimeUtc;
        }

        protected override bool ShouldIncludeEntry(ref FileSystemEntry entry)
        {
            if (entry.IsDirectory || !Path.GetExtension(entry.FileName).Equals(".cs", StringComparison.OrdinalIgnoreCase))
            {
                // Cheap check indicates this is not a file-based app.
                return false;
            }

            if (IsCacheUpToDate(ref entry))
            {
                // Already up to date. If it is an FBA, it was visited by the initial cache loop.
                return false;
            }

            var fullPath = entry.ToFullPath();
            if (_cache.FileBasedAppFullPaths.BinarySearch(fullPath, s_pathComparer) >= 0)
            {
                // File has changed since our last walk, but it's under a cached file-based app path.
                // The initial cache loop already handled it.
                return false;
            }

            return IsFileBasedApp(fullPath);
        }

        protected override bool ShouldRecurseIntoEntry(ref FileSystemEntry entry)
        {
            foreach (var ignored in s_ignoredDirectories)
            {
                if (entry.FileName.Equals(ignored, StringComparison.OrdinalIgnoreCase))
                    return false;
            }

            var fullPath = entry.ToFullPath();
            if (IsCacheUpToDate(ref entry))
            {
                if (_cache.DirectoriesContainingCsproj.BinarySearch(fullPath, s_pathComparer) >= 0)
                {
                    // Still contains a csproj. Do not recurse.
                    _directoriesContainingCsprojBuilder.Add(fullPath);
                    return false;
                }

                return true;
            }

            // Directory contents changed since last walk.
            // Check again if it contains a csproj file.
            var containsCsproj = Directory.EnumerateFiles(fullPath, "*.csproj").Any();
            if (containsCsproj)
            {
                _directoriesContainingCsprojBuilder.Add(fullPath);
                return false;
            }

            // Changed since last walk, and doesn't contain a csproj file.
            // User may have moved new folders or files into this directory since last walk.
            _newerDirectories.Add(fullPath);
            return true;
        }
    }

    internal record Cache(string WorkspacePath, DateTimeOffset LastWalkTimeUtc, ImmutableArray<string> FileBasedAppFullPaths, ImmutableArray<string> DirectoriesContainingCsproj)
    {
        public ImmutableArray<string> FileBasedAppFullPaths { get; init; } = FileBasedAppFullPaths;
        public ImmutableArray<string> DirectoriesContainingCsproj { get; init; } = DirectoriesContainingCsproj;
    }

    [JsonSerializable(typeof(Cache))]
    internal partial class CacheSerializerContext : JsonSerializerContext;
}
