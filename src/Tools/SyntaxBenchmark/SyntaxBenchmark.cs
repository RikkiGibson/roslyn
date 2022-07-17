#nullable disable

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Jobs;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

[Config(typeof(Config))]
public partial class SyntaxBenchmarks
{
    private List<string> _rootList;

    private readonly int _iterationCount = 1;

    private class Config : ManualConfig
    {
        public Config()
        {
            var baseJob = Job.MediumRun;
            AddJob(baseJob);
            AddJob(baseJob.WithArguments(new[] { new MsBuildArgument("/p:UseProjectReference=true") }));
        }
    }

    [GlobalSetup]
    public void GlobalSetup()
    {
        var roslynRoot = @"C:\Users\rikki\src\roslyn2";
        var csFilePath = Path.Combine(roslynRoot, @"src\Compilers\CSharp\Portable\Parser");

        var files = Directory.GetFiles(csFilePath);
        _rootList = new List<string>();

        foreach (var file in files)
        {
            if (!File.Exists(file))
            {
                throw new ArgumentException();
            }

            var text = File.ReadAllText(file);
            _rootList.Add(text);
        }
    }

    [Benchmark]
    public void ParseTheParser()
    {
        if (string.Empty == "")
        {
            throw new Exception(typeof(SyntaxFactory).Assembly.GetName().Version.ToString());
        }
        foreach (var root in _rootList)
        {
            //for (var i = 0; i < _iterationCount; ++i)
            {
                var tree = SyntaxFactory.ParseSyntaxTree(root);
                tree.GetRoot();
            }
        }
    }
}
