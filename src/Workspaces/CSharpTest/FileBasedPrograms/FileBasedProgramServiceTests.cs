// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.IO;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.FileBasedPrograms;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests.FileBasedPrograms;

public sealed class FileBasedProgramServiceTests
{
    [Fact]
    public async Task GetDirectiveInfoAsync()
    {
        using var workspace = new AdhocWorkspace();
        var project = workspace.AddProject("Test", LanguageNames.CSharp)
            .WithParseOptions(CSharpParseOptions.Default.WithFeatures([new("FileBasedProgram", "true")]));
        Assert.True(workspace.TryApplyChanges(project.Solution));
        var source = "#:include   Library.cs  \nSystem.Console.WriteLine();";
        var document = workspace.AddDocument(project.Id, "Program.cs", SourceText.From(source)).WithFilePath("/src/Program.cs");
        var service = workspace.Services.GetRequiredService<IFileBasedProgramService>();

        var info = await service.GetDirectiveInfoAsync(document, source.IndexOf("Library", System.StringComparison.Ordinal), CancellationToken.None);

        Assert.NotNull(info);
        Assert.Equal(FileBasedProgramDirectiveKind.Include, info.Value.Kind);
        Assert.Equal("Library.cs", info.Value.Value);
        Assert.Equal("Library.cs", source[info.Value.ValueSpan.Start..info.Value.ValueSpan.End]);

        Assert.Null(await service.GetDirectiveInfoAsync(document, source.IndexOf("include", System.StringComparison.Ordinal), CancellationToken.None));
    }

    [Theory]
    [InlineData("include")]
    [InlineData("ref")]
    public async Task GetNavigationInfoAsync_UsesSolutionModel(string directiveName)
    {
        using var workspace = new AdhocWorkspace();
        var rootDirectory = Path.Combine(Path.GetPathRoot(System.Environment.CurrentDirectory)!, "fba-navigation-test");
        var sourceDirectory = Path.Combine(rootDirectory, "src");
        var libraryDirectory = Path.Combine(rootDirectory, "lib");
        var includedFilePath = Path.Combine(sourceDirectory, "Included.cs");
        var projectFilePath = Path.Combine(libraryDirectory, "Library.csproj");
        var referencedFilePath = Path.Combine(libraryDirectory, "Referenced.cs");
        var appProject = workspace.AddProject("App", LanguageNames.CSharp)
            .WithParseOptions(CSharpParseOptions.Default.WithFeatures([new("FileBasedProgram", "true")]));
        var referencedProjectId = ProjectId.CreateNewId();
        var solution = appProject.Solution.AddProject(
            ProjectInfo.Create(referencedProjectId, VersionStamp.Default, "Library", "Library", LanguageNames.CSharp, filePath: projectFilePath));
        var referencedProject = solution.GetRequiredProject(referencedProjectId);
        var referencedDocument = referencedProject.AddDocument("Referenced.cs", SourceText.From(""), filePath: referencedFilePath);
        appProject = referencedDocument.Project.Solution.GetRequiredProject(appProject.Id)
            .AddProjectReference(new ProjectReference(referencedProject.Id));

        var includedDocument = appProject.AddDocument("Included.cs", SourceText.From(""), filePath: includedFilePath);
        appProject = includedDocument.Project;

        var directiveValue = directiveName switch
        {
            "include" => "Included.cs",
            "ref" => "../lib/Referenced.cs",
            _ => throw ExceptionUtilities.UnexpectedValue(directiveName),
        };
        var expectedPath = directiveName switch
        {
            "include" => includedFilePath,
            "ref" => referencedFilePath,
            _ => throw ExceptionUtilities.UnexpectedValue(directiveName),
        };
        var source = $"#:{directiveName} {directiveValue}";
        var document = appProject.AddDocument("Program.cs", SourceText.From(source), filePath: Path.Combine(sourceDirectory, "Program.cs"));
        Assert.True(workspace.TryApplyChanges(document.Project.Solution));
        document = workspace.CurrentSolution.GetRequiredDocument(document.Id);
        var service = workspace.Services.GetRequiredService<IFileBasedProgramService>();

        Assert.NotNull(await service.GetDirectiveInfoAsync(document, source.Length, CancellationToken.None));
        var info = await service.GetNavigationInfoAsync(document, source.Length, CancellationToken.None);

        Assert.NotNull(info);
        Assert.Equal(expectedPath, Assert.Single(info.Value.TargetFilePaths));
    }

    [Fact]
    public async Task GetNavigationInfoAsync_ProjectReturnsNull()
    {
        using var workspace = new AdhocWorkspace();
        var project = workspace.AddProject("App", LanguageNames.CSharp)
            .WithParseOptions(CSharpParseOptions.Default.WithFeatures([new("FileBasedProgram", "true")]));
        const string source = "#:project ../lib/Library.csproj";
        var document = project.AddDocument("Program.cs", SourceText.From(source), filePath: "/src/Program.cs");
        Assert.True(workspace.TryApplyChanges(document.Project.Solution));
        document = workspace.CurrentSolution.GetRequiredDocument(document.Id);
        var service = workspace.Services.GetRequiredService<IFileBasedProgramService>();

        var info = await service.GetNavigationInfoAsync(document, source.Length, CancellationToken.None);

        Assert.Null(info);
    }

    [Fact]
    public async Task GetNavigationInfoAsync_IncludeGlobWithoutMatcherReturnsNull()
    {
        using var workspace = new AdhocWorkspace();
        var rootDirectory = Path.Combine(Path.GetPathRoot(System.Environment.CurrentDirectory)!, "fba-navigation-test");
        var sourceDirectory = Path.Combine(rootDirectory, "src");
        var project = workspace.AddProject("App", LanguageNames.CSharp)
            .WithParseOptions(CSharpParseOptions.Default.WithFeatures([new("FileBasedProgram", "true")]));
        const string source = "#:include **/*.cs";
        var document = project.AddDocument("Program.cs", SourceText.From(source), filePath: Path.Combine(sourceDirectory, "Program.cs"));
        Assert.True(workspace.TryApplyChanges(document.Project.Solution));
        document = workspace.CurrentSolution.GetRequiredDocument(document.Id);
        var service = workspace.Services.GetRequiredService<IFileBasedProgramService>();

        var info = await service.GetNavigationInfoAsync(document, source.Length, CancellationToken.None);

        Assert.Null(info);
    }
}
