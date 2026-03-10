// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Buffers;
using System.Collections.Concurrent;
using System.Diagnostics;
using System.IO;
using System.IO.Enumeration;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using BenchmarkDotNet.Attributes;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Text;

namespace FileBasedProgramsEntryPointDiscoveryBenchmarks;

[MemoryDiagnoser]
public class EntryPointDiscoveryBenchmark
{
    private const string FileBasedProgramFeatureName = "FileBasedProgram";
    private const int EntireFileSentinel = -1;

    private static readonly CSharpParseOptions s_parseOptions =
        CSharpParseOptions.Default.WithFeatures([new(FileBasedProgramFeatureName, "true")]);

    [Params("~/src/roslyn/")]
    public string Folder { get; set; } = string.Empty;

    [Params(1, 4, 16)]
    public int DegreeOfParallelism { get; set; }

    [Params(1024, 4096, EntireFileSentinel)]
    public int LeadingContentBytes { get; set; }

    private string _folderPath = string.Empty;

    [GlobalSetup]
    public void GlobalSetup()
    {
        _folderPath = ExpandPath(Folder);
        if (!Directory.Exists(_folderPath))
            throw new DirectoryNotFoundException($"Benchmark folder does not exist: '{_folderPath}'");
    }

    [Benchmark]
    public async Task<DiscoverySummary> FindAndLogEntryPointsAsync()
    {
        var stopwatch = Stopwatch.StartNew();
        var discoveredCount = 0;
        var checkedCsFileCount = 0;
        var logCount = 0;

        if (Directory.EnumerateFiles(_folderPath, "*.csproj").Any())
        {
            stopwatch.Stop();
            return new DiscoverySummary(discoveredCount, checkedCsFileCount, logCount, stopwatch.ElapsedMilliseconds);
        }

        await Parallel.ForEachAsync(
            EnumeratePossibleEntryPoints(_folderPath),
            new ParallelOptions { MaxDegreeOfParallelism = DegreeOfParallelism },
            async (csFilePath, cancellationToken) =>
            {
                if (!await HasFileBasedAppDirectivesAsync(csFilePath, LeadingContentBytes, cancellationToken).ConfigureAwait(false))
                    return;

                Trace.WriteLine($"Discovered file-based app entry point: {csFilePath}");
                Interlocked.Increment(ref logCount);
                Interlocked.Increment(ref discoveredCount);
            }).ConfigureAwait(false);

        stopwatch.Stop();
        return new DiscoverySummary(discoveredCount, checkedCsFileCount, logCount, stopwatch.ElapsedMilliseconds);

        FileSystemEnumerable<string> EnumeratePossibleEntryPoints(string directory)
        {
            return new FileSystemEnumerable<string>(
                directory,
                transform: (ref FileSystemEntry entry) => entry.ToFullPath(),
                options: new EnumerationOptions { RecurseSubdirectories = true })
            {
                ShouldIncludePredicate = ShouldInclude,
                ShouldRecursePredicate = ShouldRecurse,
            };

            bool ShouldInclude(ref FileSystemEntry entry)
            {
                if (entry.IsDirectory || !Path.GetExtension(entry.FileName).Equals(".cs", StringComparison.Ordinal))
                    return false;

                Interlocked.Increment(ref checkedCsFileCount);
                return true;
            }

            static bool ShouldRecurse(ref FileSystemEntry entry)
            {
                var directoryInfo = (DirectoryInfo)entry.ToFileSystemInfo();
                return !directoryInfo.EnumerateFiles("*.csproj").Any();
            }
        }
    }

    public async Task<DiscoverySummary> FindAndLogEntryPointsWithCandidateSizeLoggingAsync()
    {
        var stopwatch = Stopwatch.StartNew();
        var discoveredCount = 0;
        var checkedCsFileCount = 0;
        var logCount = 0;
        var candidateFiles = new ConcurrentBag<CandidateFile>();

        if (Directory.EnumerateFiles(_folderPath, "*.csproj").Any())
        {
            stopwatch.Stop();
            return new DiscoverySummary(discoveredCount, checkedCsFileCount, logCount, stopwatch.ElapsedMilliseconds);
        }

        await Parallel.ForEachAsync(
            EnumeratePossibleEntryPoints(_folderPath),
            new ParallelOptions { MaxDegreeOfParallelism = DegreeOfParallelism },
            async (csFilePath, cancellationToken) =>
            {
                if (!await HasFileBasedAppDirectivesAsync(csFilePath, LeadingContentBytes, cancellationToken).ConfigureAwait(false))
                    return;

                Trace.WriteLine($"Discovered file-based app entry point: {csFilePath}");
                Interlocked.Increment(ref logCount);
                Interlocked.Increment(ref discoveredCount);
            }).ConfigureAwait(false);

        foreach (var candidateFile in candidateFiles.OrderByDescending(static c => c.SizeBytes).ThenBy(static c => c.Path, StringComparer.Ordinal))
        {
            Console.WriteLine($"Checked candidate file: {candidateFile.Path} ({candidateFile.SizeBytes} bytes)");
        }

        stopwatch.Stop();
        return new DiscoverySummary(discoveredCount, checkedCsFileCount, logCount, stopwatch.ElapsedMilliseconds);

        FileSystemEnumerable<string> EnumeratePossibleEntryPoints(string directory)
        {
            return new FileSystemEnumerable<string>(
                directory,
                transform: (ref FileSystemEntry entry) => entry.ToFullPath(),
                options: new EnumerationOptions { RecurseSubdirectories = true })
            {
                ShouldIncludePredicate = ShouldInclude,
                ShouldRecursePredicate = ShouldRecurse,
            };

            bool ShouldInclude(ref FileSystemEntry entry)
            {
                if (entry.IsDirectory || !Path.GetExtension(entry.FileName).Equals(".cs", StringComparison.Ordinal))
                    return false;

                var candidatePath = entry.ToFullPath();
                var candidateSizeBytes = entry.Length;
                candidateFiles.Add(new CandidateFile(candidatePath, candidateSizeBytes));
                Interlocked.Increment(ref checkedCsFileCount);
                return true;
            }

            static bool ShouldRecurse(ref FileSystemEntry entry)
            {
                var directoryInfo = (DirectoryInfo)entry.ToFileSystemInfo();
                return !directoryInfo.EnumerateFiles("*.csproj").Any();
            }
        }
    }

    private static async ValueTask<bool> HasFileBasedAppDirectivesAsync(string csFilePath, int leadingContentBytes, CancellationToken cancellationToken)
    {
        using var fileStream = File.OpenRead(csFilePath);

        if (leadingContentBytes == EntireFileSentinel)
        {
            return HasFileBasedAppDirectives(SourceText.From(fileStream));
        }

        var bytesToRead = (int)Math.Min(fileStream.Length, leadingContentBytes);
        if (bytesToRead <= 0)
            return false;

        var bytes = ArrayPool<byte>.Shared.Rent(bytesToRead);
        await fileStream.ReadExactlyAsync(bytes.AsMemory(0, bytesToRead), cancellationToken).ConfigureAwait(false);
        var result = HasFileBasedAppDirectives(SourceText.From(bytes, bytesToRead));
        ArrayPool<byte>.Shared.Return(bytes);
        return result;
    }

    private static bool HasFileBasedAppDirectives(SourceText text)
    {
        var tokenizer = SyntaxFactory.CreateTokenParser(text, s_parseOptions);
        var result = tokenizer.ParseLeadingTrivia();
        foreach (var trivia in result.Token.LeadingTrivia)
        {
            if (trivia.Kind() is SyntaxKind.ShebangDirectiveTrivia or SyntaxKind.IgnoredDirectiveTrivia)
                return true;
        }

        return false;
    }

    private static string ExpandPath(string path)
    {
        if (path.StartsWith("~/", StringComparison.Ordinal))
        {
            var home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
            return Path.Combine(home, path[2..]);
        }

        return path;
    }

    public readonly record struct DiscoverySummary(int DiscoveredCount, int CheckedCsFileCount, int LogCount, long ElapsedMilliseconds);

    private readonly record struct CandidateFile(string Path, long SizeBytes);
}
