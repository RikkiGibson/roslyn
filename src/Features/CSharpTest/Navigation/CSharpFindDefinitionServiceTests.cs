// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Navigation;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Test.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests.Navigation;

[UseExportProvider]
public sealed class CSharpFindDefinitionServiceTests
{
    [Theory]
    [InlineData("include", "Included.cs", "C:\\src\\Included.cs")]
    [InlineData("ref", "..\\lib\\Referenced.cs", "C:\\lib\\Referenced.cs")]
    [InlineData("project", "..\\lib\\Library.csproj", null)]
    public async Task TestFileBasedProgramDirective(string directiveName, string directiveValue, string? expectedFilePath)
    {
        var workspaceXml = $$"""
            <Workspace>
                <Project Language="C#" CommonReferences="true" Features="FileBasedProgram=true" FilePath="C:\src\App.csproj" AssemblyName="App">
                    <ProjectReference>Library</ProjectReference>
                    <Document FilePath="C:\src\Program.cs">#:{{directiveName}} {{directiveValue}}$$</Document>
                    <Document FilePath="C:\src\Included.cs">class Included { }</Document>
                </Project>
                <Project Language="C#" CommonReferences="true" FilePath="C:\lib\Library.csproj" AssemblyName="Library">
                    <Document FilePath="C:\lib\Referenced.cs">class Referenced { }</Document>
                </Project>
            </Workspace>
            """;
        using var workspace = TestWorkspace.Create(workspaceXml);
        var testDocument = workspace.Documents.Single(static document => document.CursorPosition.HasValue);
        var document = workspace.CurrentSolution.GetRequiredDocument(testDocument.Id);
        var service = document.GetRequiredLanguageService<INavigableItemsService>();

        var items = await service.GetNavigableItemsAsync(document, testDocument.CursorPosition!.Value, CancellationToken.None);

        if (expectedFilePath is null)
        {
            Assert.Empty(items);
        }
        else
        {
            Assert.Equal(expectedFilePath, Assert.Single(items).Document.FilePath);
        }
    }
}
