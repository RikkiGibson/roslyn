// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Linq;
using System.Threading.Tasks;
using BenchmarkDotNet.Running;

namespace FileBasedProgramsEntryPointDiscoveryBenchmarks;

internal static class Program
{
    private static void Main(string[] args)
    {
        if (args.Contains("--run-find-once", StringComparer.Ordinal))
        {
            RunFindOnceAsync().GetAwaiter().GetResult();
            return;
        }

        if (args.Contains("--run-candidate-size-log", StringComparer.Ordinal))
        {
            RunCandidateSizeLoggingOnceAsync().GetAwaiter().GetResult();
            return;
        }

        BenchmarkRunner.Run<EntryPointDiscoveryBenchmark>();
    }

    private static async Task RunCandidateSizeLoggingOnceAsync()
    {
        var benchmark = new EntryPointDiscoveryBenchmark
        {
            Folder = "~/src/roslyn/",
            DegreeOfParallelism = 4,
            LeadingContentBytes = 4096,
        };

        benchmark.GlobalSetup();
        var summary = await benchmark.FindAndLogEntryPointsWithCandidateSizeLoggingAsync();
        Console.WriteLine($"Summary: discovered={summary.DiscoveredCount}, checked={summary.CheckedCsFileCount}, logged={summary.LogCount}, elapsedMs={summary.ElapsedMilliseconds}");
    }

    private static async Task RunFindOnceAsync()
    {
        var benchmark = new EntryPointDiscoveryBenchmark
        {
            Folder = "~/src/roslyn/",
            DegreeOfParallelism = 4,
            LeadingContentBytes = 4096,
        };

        benchmark.GlobalSetup();
        var summary = await benchmark.FindAndLogEntryPointsAsync();
        Console.WriteLine($"Summary: discovered={summary.DiscoveredCount}, checked={summary.CheckedCsFileCount}, logged={summary.LogCount}, elapsedMs={summary.ElapsedMilliseconds}");
    }
}
