// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Test.Utilities;
using Roslyn.Test.Utilities;
using Roslyn.Utilities;
using Xunit;
using Xunit.Abstractions;
using Xunit.Sdk;
using Microsoft.CodeAnalysis.VisualBasic;
#if NET
using Roslyn.Test.Utilities.CoreClr;
using System.Runtime.Loader;
#else
using Roslyn.Test.Utilities.Desktop;
#endif

namespace Microsoft.CodeAnalysis.UnitTests
{

#if NET

    public sealed class InvokeUtil
    {
        internal void Exec(
            ITestOutputHelper testOutputHelper,
            ImmutableArray<IAnalyzerPathResolver> pathResolvers,
            ImmutableArray<IAnalyzerAssemblyResolver> assemblyResolvers,
            AssemblyLoadTestFixture fixture,
            AnalyzerTestKind kind,
            string typeName,
            string methodName,
            object? state = null)
        {
            using var tempRoot = new TempRoot();
            switch (kind)
            {
                case AnalyzerTestKind.LoadDirect:
                    assemblyResolvers = [.. assemblyResolvers, AnalyzerAssemblyLoader.DiskAnalyzerAssemblyResolver];
                    break;
                case AnalyzerTestKind.LoadStream:
                    assemblyResolvers = [.. assemblyResolvers, AnalyzerAssemblyLoader.StreamAnalyzerAssemblyResolver];
                    break;
                case AnalyzerTestKind.ShadowLoad:
                    pathResolvers = [.. pathResolvers, new ShadowCopyAnalyzerPathResolver(tempRoot.CreateDirectory().Path)];
                    assemblyResolvers = [.. assemblyResolvers, AnalyzerAssemblyLoader.DiskAnalyzerAssemblyResolver];
                    break;
                default:
                    throw ExceptionUtilities.Unreachable();
            }

            var loader = new AnalyzerAssemblyLoader(pathResolvers, assemblyResolvers, compilerLoadContext: null);

            var compilerContextAssemblies = loader.CompilerLoadContext.Assemblies.Select(a => a.FullName).ToHashSet();
            try
            {
                Exec(testOutputHelper, fixture, loader, typeName, methodName, state);
            }
            finally
            {
                // Verify the test did not leak any test fixture assemblies into the compiler
                // load context. We check only for fixture assemblies rather than requiring the
                // full set to be unchanged because any work occurring ambiently
                // in the process can cause more assemblies to get loaded in this context.
                var leakedAssemblies = loader.CompilerLoadContext.Assemblies
                    .Where(a => !compilerContextAssemblies.Contains(a.FullName) &&
                                a.Location.StartsWith(fixture.TempDirectory, StringComparison.OrdinalIgnoreCase))
                    .Select(a => a.FullName)
                    .ToArray();
                Assert.Empty(leakedAssemblies);
            }
        }

        // TODO2: remove link before merging. Helix log showing the flaky failure this change addresses:
        // https://helix.dot.net/api/2019-06-17/jobs/acddef87-dfd0-4007-b05a-89dffbfdd6e9/workitems/workitem_4/console
        internal void Exec(
            ITestOutputHelper testOutputHelper,
            AssemblyLoadTestFixture fixture,
            AnalyzerAssemblyLoader loader,
            string typeName,
            string methodName,
            object? state = null)
        {
            var defaultContextAssemblies = AssemblyLoadContext.Default.Assemblies.Select(a => a.FullName).ToHashSet();
            using var tempRoot = new TempRoot();

            try
            {
                AnalyzerAssemblyLoaderTests.InvokeTestCode(loader, fixture, typeName, methodName, state);
            }
            finally
            {
                testOutputHelper.WriteLine($"Test fixture root: {fixture.TempDirectory}");

                foreach (var context in loader.GetDirectoryLoadContextsSnapshot())
                {
                    testOutputHelper.WriteLine($"Directory context: {context.Directory}");
                    foreach (var assembly in context.Assemblies)
                    {
                        testOutputHelper.WriteLine($"\t{assembly.FullName}");
                    }
                }

                if (loader.AnalyzerPathResolvers.OfType<ShadowCopyAnalyzerPathResolver>().FirstOrDefault() is { } shadowResolver)
                {
                    testOutputHelper.WriteLine($"{nameof(ShadowCopyAnalyzerPathResolver)}: {shadowResolver.BaseDirectory}");
                }

                testOutputHelper.WriteLine($"Loader path maps");
                foreach (var pair in loader.GetPathMapSnapshot())
                {
                    testOutputHelper.WriteLine($"\t{pair.OriginalAssemblyPath} -> {pair.ResolvedAssemblyPath}");
                }

                // Verify the test did not leak any test fixture assemblies into the
                // default load context. We check only for fixture assemblies rather than
                // requiring the full set to be unchanged because other test collections run
                // in parallel and can trigger ambient assembly loads at any time.
                var leakedAssemblies = AssemblyLoadContext.Default.Assemblies
                    .Where(a => !defaultContextAssemblies.Contains(a.FullName) &&
                                a.Location.StartsWith(fixture.TempDirectory, StringComparison.OrdinalIgnoreCase))
                    .Select(a => a.FullName)
                    .ToArray();
                Assert.Empty(leakedAssemblies);
            }
        }
    }

#else

    public sealed class InvokeUtil : MarshalByRefObject
    {
        internal void Exec(
            ITestOutputHelper testOutputHelper,
            AssemblyLoadTestFixture fixture,
            AnalyzerTestKind kind,
            string typeName,
            string methodName,
            IAnalyzerPathResolver[] pathResolvers,
            object? state)
        {
            using var tempRoot = new TempRoot();
            pathResolvers = kind switch
            {
                AnalyzerTestKind.LoadDirect => pathResolvers,
                AnalyzerTestKind.ShadowLoad => [.. pathResolvers, new ShadowCopyAnalyzerPathResolver(tempRoot.CreateDirectory().Path)],
                _ => throw ExceptionUtilities.Unreachable(),
            };

            var loader = new AnalyzerAssemblyLoader(pathResolvers.ToImmutableArray());

            try
            {
                AnalyzerAssemblyLoaderTests.InvokeTestCode(loader, fixture, typeName, methodName, state);
            }
            catch (TargetInvocationException ex) when (ex.InnerException is XunitException)
            {
                var inner = ex.InnerException;
                throw new Exception(inner.Message + inner.StackTrace);
            }
            finally
            {
                testOutputHelper.WriteLine($"Test fixture root: {fixture.TempDirectory}");

                testOutputHelper.WriteLine($"Loaded Assemblies");
                foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies().OrderByDescending(x => x.FullName))
                {
                    testOutputHelper.WriteLine($"\t{assembly.FullName} -> {assembly.Location}");
                }

                if (loader.AnalyzerPathResolvers.OfType<ShadowCopyAnalyzerPathResolver>().FirstOrDefault() is { } shadowResolver)
                {
                    testOutputHelper.WriteLine($"{nameof(ShadowCopyAnalyzerPathResolver)}: {shadowResolver.BaseDirectory}");
                }

                testOutputHelper.WriteLine($"Loader path maps");
                foreach (var pair in loader.GetPathMapSnapshot())
                {
                    testOutputHelper.WriteLine($"\t{pair.OriginalAssemblyPath} -> {pair.ResolvedAssemblyPath}");
                }
            }
        }
    }

#endif

}
