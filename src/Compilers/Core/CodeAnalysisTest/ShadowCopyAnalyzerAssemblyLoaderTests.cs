﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System.Collections.Immutable;
using System.IO;
using System.Reflection;
using System.Text;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Test.Utilities;
using Roslyn.Test.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.UnitTests
{
    public sealed class ShadowCopyAnalyzerAssemblyLoaderTests : TestBase
    {
        private static readonly CSharpCompilationOptions s_dllWithMaxWarningLevel = new(OutputKind.DynamicallyLinkedLibrary, warningLevel: CodeAnalysis.Diagnostic.MaxWarningLevel);

        [Fact, WorkItem(32226, "https://github.com/dotnet/roslyn/issues/32226")]
        public void LoadWithDependency()
        {
            var directory = Temp.CreateDirectory();
            var immutable = directory.CopyFile(typeof(ImmutableArray).Assembly.Location);
            var microsoftCodeAnalysis = directory.CopyFile(typeof(DiagnosticAnalyzer).Assembly.Location);

            var analyzerDependencyFile = CreateAnalyzerDependency();
            var analyzerMainFile = CreateMainAnalyzerWithDependency(analyzerDependencyFile);
            var loader = new ShadowCopyAnalyzerAssemblyLoader(Path.Combine(directory.Path, "AnalyzerAssemblyLoader"));

            var analyzerMainReference = new AnalyzerFileReference(analyzerMainFile.Path, loader);
            analyzerMainReference.AnalyzerLoadFailed += (_, e) => AssertEx.Fail(e.Exception.Message);
            var analyzerDependencyReference = new AnalyzerFileReference(analyzerDependencyFile.Path, loader);
            analyzerDependencyReference.AnalyzerLoadFailed += (_, e) => AssertEx.Fail(e.Exception.Message);

            var analyzers = analyzerMainReference.GetAnalyzersForAllLanguages();
            Assert.Equal(1, analyzers.Length);
            Assert.Equal("TestAnalyzer", analyzers[0].ToString());

            Assert.Equal(0, analyzerDependencyReference.GetAnalyzersForAllLanguages().Length);

            Assert.NotNull(analyzerDependencyReference.GetAssembly());

            TempFile CreateAnalyzerDependency()
            {
                var analyzerDependencySource = @"
using System;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;

public abstract class AbstractTestAnalyzer : DiagnosticAnalyzer
{
    protected static string SomeString = nameof(SomeString);
    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { throw new NotImplementedException(); } }
    public override void Initialize(AnalysisContext context) { throw new NotImplementedException(); }
}";

                var analyzerDependencyCompilation = CSharpCompilation.Create(
                   "AnalyzerDependency",
                   new SyntaxTree[] { SyntaxFactory.ParseSyntaxTree(analyzerDependencySource) },
                   new MetadataReference[]
                   {
                    TestMetadata.NetStandard20.mscorlib,
                    TestMetadata.NetStandard20.netstandard,
                    TestMetadata.NetStandard20.SystemRuntime,
                    MetadataReference.CreateFromFile(immutable.Path),
                    MetadataReference.CreateFromFile(microsoftCodeAnalysis.Path)
                   },
                   s_dllWithMaxWarningLevel);

                return directory.CreateDirectory("AnalyzerDependency").CreateFile("AnalyzerDependency.dll").WriteAllBytes(analyzerDependencyCompilation.EmitToArray());
            }

            TempFile CreateMainAnalyzerWithDependency(TempFile analyzerDependency)
            {
                var analyzerMainSource = @"
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public sealed class TestAnalyzer : AbstractTestAnalyzer
{
    private static string SomeString2 = AbstractTestAnalyzer.SomeString;
}";
                var analyzerMainCompilation = CSharpCompilation.Create(
                   "AnalyzerMain",
                   new SyntaxTree[] { SyntaxFactory.ParseSyntaxTree(analyzerMainSource) },
                   new MetadataReference[]
                   {
                        TestMetadata.NetStandard20.mscorlib,
                        TestMetadata.NetStandard20.netstandard,
                        TestMetadata.NetStandard20.SystemRuntime,
                        MetadataReference.CreateFromFile(immutable.Path),
                        MetadataReference.CreateFromFile(microsoftCodeAnalysis.Path),
                        MetadataReference.CreateFromFile(analyzerDependency.Path)
                   },
                   s_dllWithMaxWarningLevel);

                return directory.CreateDirectory("AnalyzerMain").CreateFile("AnalyzerMain.dll").WriteAllBytes(analyzerMainCompilation.EmitToArray());
            }
        }

        [Fact]
        public void AssemblyLoading_MultipleVersions()
        {
            StringBuilder sb = new StringBuilder();

            var path1 = Temp.CreateDirectory();
            var gammaDll = path1.CreateFile("Gamma.dll").WriteAllBytes(TestResources.AssemblyLoadTests.Gamma);
            var delta1Dll = path1.CreateFile("Delta.dll").WriteAllBytes(TestResources.AssemblyLoadTests.Delta1);

            var path2 = Temp.CreateDirectory();
            var epsilonDll = path2.CreateFile("Epsilon.dll").WriteAllBytes(TestResources.AssemblyLoadTests.Epsilon);
            var delta2Dll = path2.CreateFile("Delta.dll").WriteAllBytes(TestResources.AssemblyLoadTests.Delta2);

            var loader = new DefaultAnalyzerAssemblyLoader();
            loader.AddDependencyLocation(gammaDll.Path);
            loader.AddDependencyLocation(delta1Dll.Path);
            loader.AddDependencyLocation(epsilonDll.Path);
            loader.AddDependencyLocation(delta2Dll.Path);

            Assembly gamma = loader.LoadFromPath(gammaDll.Path);
            var g = gamma.CreateInstance("Gamma.G");
            g.GetType().GetMethod("Write").Invoke(g, new object[] { sb, "Test G" });

            Assembly epsilon = loader.LoadFromPath(epsilonDll.Path);
            var e = epsilon.CreateInstance("Epsilon.E");
            e.GetType().GetMethod("Write").Invoke(e, new object[] { sb, "Test E" });

            var actual = sb.ToString();
            if (ExecutionConditionUtil.IsCoreClr)
            {
                Assert.Equal(
@"Delta: Gamma: Test G
Delta.2: Epsilon: Test E
",
                    actual);
            }
            else
            {
                Assert.Equal(
@"Delta: Gamma: Test G
Delta: Epsilon: Test E
",
                    actual);
            }
        }
    }
}
