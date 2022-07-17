using System;
using BenchmarkDotNet.Running;

var summary = BenchmarkRunner.Run<SyntaxBenchmarks>();
