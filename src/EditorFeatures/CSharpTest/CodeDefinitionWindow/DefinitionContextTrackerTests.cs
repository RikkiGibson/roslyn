// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeDefinitionWindow;
using Microsoft.CodeAnalysis.Editor.UnitTests;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.Test.Utilities;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Test.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.Editor.CSharp.UnitTests.CodeDefinitionWindow;

[UseExportProvider]
public sealed class DefinitionContextTrackerTests
{
    [WpfFact]
    public async Task FileBasedProgramProjectDirective()
    {
        using var workspace = EditorTestWorkspace.Create(
            """
            <Workspace>
                <Project Language="C#" CommonReferences="true" Features="FileBasedProgram=true">
                    <Document FilePath="App.cs">
                        #:project $$Target.csproj
                        System.Console.WriteLine();
                    </Document>
                    <ProjectReference>Target</ProjectReference>
                </Project>
                <Project FilePath="Target.csproj" AssemblyName="Target" Language="C#" CommonReferences="true" />
            </Workspace>
            """,
            composition: EditorTestCompositions.EditorFeatures.AddParts(typeof(TestCodeDefinitionWindowService)));
        var hostDocument = workspace.Documents.Single();
        var document = workspace.CurrentSolution.GetDocument(hostDocument.Id)!;
        var tracker = workspace.ExportProvider.GetExportedValue<DefinitionContextTracker>();

        var locations = await tracker.GetContextFromPointAsync(document, hostDocument.CursorPosition!.Value, CancellationToken.None);

        var location = Assert.Single(locations);
        Assert.Equal("Target.csproj", location.DisplayName);
        Assert.Equal(Path.Combine(TestWorkspace.RootDirectory, "Target.csproj"), location.FilePath);
        Assert.Equal(default(LinePosition), location.Position);
    }

    [Export(typeof(ICodeDefinitionWindowService)), PartNotDiscoverable]
    private sealed class TestCodeDefinitionWindowService : ICodeDefinitionWindowService
    {
        [ImportingConstructor]
        [Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
        public TestCodeDefinitionWindowService()
        {
        }

        public Task<bool> IsWindowOpenAsync(CancellationToken cancellationToken)
            => throw new NotSupportedException();

        public Task SetContextAsync(ImmutableArray<CodeDefinitionWindowLocation> locations, CancellationToken cancellationToken)
            => throw new NotSupportedException();
    }
}
