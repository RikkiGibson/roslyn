// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Microsoft.CodeAnalysis.FileBasedPrograms;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.LanguageServer.HostWorkspace;
using Microsoft.CodeAnalysis.LanguageServer.Services;
using Xunit.Abstractions;

namespace Microsoft.CodeAnalysis.LanguageServer.UnitTests.HostWorkspace;

public sealed class LanguageServerFilePathMatcherServiceTests(ITestOutputHelper testOutputHelper)
    : AbstractLanguageServerHostTests(testOutputHelper)
{
    [Fact]
    public async Task ExportedInLanguageServerHostWorkspace()
    {
        await using var testLspServer = await CreateLanguageServerAsync(serverConfiguration: ServerConfigurationWithoutDevKit);
        var workspace = testLspServer.GetRequiredLspService<IHostWorkspaceProvider>().Workspace;

        Assert.IsType<LanguageServerFilePathMatcherService>(workspace.Services.GetRequiredService<IFilePathMatcherService>());
    }

    [Theory]
    [InlineData("*.cs", "First.cs", true)]
    [InlineData("*.cs", "nested/Second.cs", false)]
    [InlineData("**/*.cs", "First.cs", true)]
    [InlineData("**/*.cs", "nested/Second.cs", true)]
    [InlineData("**/*.cs", "nested/Second.vb", false)]
    [InlineData("File?.cs", "File1.cs", false)]
    [InlineData("File?.cs", "File12.cs", false)]
    public void Matches(string pattern, string relativeCandidatePath, bool expected)
    {
        var baseDirectory = Path.Combine(Path.GetTempPath(), "file-path-matcher-tests");
        var candidateFilePath = Path.Combine(baseDirectory, relativeCandidatePath.Replace('/', Path.DirectorySeparatorChar));

        Assert.Equal(
            expected,
            LanguageServerFilePathMatcherService.MatchesPath(baseDirectory, pattern, candidateFilePath));
    }
}
