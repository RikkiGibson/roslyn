// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis.LanguageServer.FileBasedPrograms;
using Microsoft.CodeAnalysis.LanguageServer.HostWorkspace;
using Microsoft.CodeAnalysis.LanguageServer.UnitTests.Miscellaneous;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Shared.TestHooks;
using Microsoft.CodeAnalysis.Test.Utilities;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.UnitTests;
using Microsoft.CodeAnalysis.Workspaces.ProjectSystem;
using Microsoft.CommonLanguageServerProtocol.Framework;
using Microsoft.Extensions.Logging;
using Microsoft.VisualStudio.Composition;
using Roslyn.LanguageServer.Protocol;
using Roslyn.Test.Utilities;
using Roslyn.Utilities;
using StreamJsonRpc;
using Xunit.Abstractions;

namespace Microsoft.CodeAnalysis.LanguageServer.UnitTests;

public sealed class FileBasedProgramsEntryPointDiscoveryTests : AbstractLanguageServerProtocolTests, IDisposable
{
    private readonly ILoggerFactory _loggerFactory;
    private readonly TestOutputLoggerProvider _loggerProvider;
    private readonly TempRoot _tempRoot;
    private readonly TempDirectory _mefCacheDirectory;

    public FileBasedProgramsEntryPointDiscoveryTests(ITestOutputHelper testOutputHelper) : base(testOutputHelper)
    {
        _loggerProvider = new TestOutputLoggerProvider(testOutputHelper);
        _loggerFactory = new LoggerFactory([_loggerProvider]);
        _tempRoot = new();
        _mefCacheDirectory = _tempRoot.CreateDirectory();
    }

    protected override async ValueTask<ExportProvider> CreateExportProviderAsync()
    {
        AsynchronousOperationListenerProvider.Enable(enable: true);

        var (exportProvider, _) = await LanguageServerTestComposition.CreateExportProviderAsync(
            _loggerFactory,
            includeDevKitComponents: false,
            cacheDirectory: _mefCacheDirectory.Path,
            extensionPaths: []);

        return exportProvider;
    }

    public void Dispose()
    {
        _tempRoot.Dispose();
        _loggerProvider.Dispose();
    }

    [Fact]
    public async Task TestDiscovery_01()
    {
        // Simple case
        // tempDir/
        //   App.cs
        //   Ordinary.cs

        var tempDir = _tempRoot.CreateDirectory();

        // Delete artifacts from possible previous runs of this test
        var cacheDirectory = VirtualProjectXmlProvider.GetDiscoveryCacheDirectory(tempDir.Path);
        if (Directory.Exists(cacheDirectory))
            Directory.Delete(cacheDirectory, recursive: true);

        var appText = """
            #:sdk Microsoft.Net.SDK
            Console.WriteLine("Hello World");
            """;
        var appFile = tempDir.CreateFile("App.cs").WriteAllText(appText);
        var ordinaryText = """
            public class Ordinary { }
            """;
        var ordinaryFile = tempDir.CreateFile("Ordinary.cs").WriteAllText(ordinaryText);

        await using var testLspServer = await CreateTestLspServerAsync(string.Empty, mutatingLspWorkspace: false, new InitializationOptions
        {
            ServerKind = WellKnownLspServerKinds.CSharpVisualBasicLspServer,
            WorkspaceFolders =
            [
                new() { DocumentUri = CreateAbsoluteDocumentUri(tempDir.Path), Name = "workspace1" }
            ]
        });

        var discovery = testLspServer.GetRequiredLspService<FileBasedProgramsEntryPointDiscovery>();
        AssertEx.SequenceEqual([appFile.Path], discovery.FindEntryPoints(tempDir.Path));

        // Verify stability
        AssertEx.SequenceEqual([appFile.Path], discovery.FindEntryPoints(tempDir.Path));

        // Changed but still has '#:'
        appFile.WriteAllText(appText + """

            Console.WriteLine("Additional content");
            """);
        AssertEx.SequenceEqual([appFile.Path], discovery.FindEntryPoints(tempDir.Path));

        // Deleted from disk
        File.Delete(appFile.Path);
        AssertEx.Empty(discovery.FindEntryPoints(tempDir.Path));

        // Put back on disk
        appFile.WriteAllText(appText);
        AssertEx.SequenceEqual([appFile.Path], discovery.FindEntryPoints(tempDir.Path));

        // Changed and no longer has '#:'
        appFile.WriteAllText("""
            Console.WriteLine("No more #: directives!");
            """);
        AssertEx.Empty(discovery.FindEntryPoints(tempDir.Path));

        // Changed and again has '#:'
        appFile.WriteAllText(appText);
        AssertEx.SequenceEqual([appFile.Path], discovery.FindEntryPoints(tempDir.Path));
    }

    [Fact]
    public async Task TestDiscovery_02()
    {
        // Demonstrate ignored folders behavior
        // tempDir/
        //   artifacts/App1.cs
        //   App2.cs

        var tempDir = _tempRoot.CreateDirectory();

        // Delete artifacts from possible previous runs of this test
        var cacheDirectory = VirtualProjectXmlProvider.GetDiscoveryCacheDirectory(tempDir.Path);
        if (Directory.Exists(cacheDirectory))
            Directory.Delete(cacheDirectory, recursive: true);

        var artifactsDir = tempDir.CreateDirectory("artifacts");
        var app1Text = """
            #:sdk Microsoft.Net.SDK
            Console.WriteLine("Hello World");
            """;
        var app1File = artifactsDir.CreateFile("App1.cs").WriteAllText(app1Text);

        var app2Text = app1Text;
        var app2File = tempDir.CreateFile("App2.cs").WriteAllText(app2Text);

        await using var testLspServer = await CreateTestLspServerAsync(string.Empty, mutatingLspWorkspace: false, new InitializationOptions
        {
            ServerKind = WellKnownLspServerKinds.CSharpVisualBasicLspServer,
            WorkspaceFolders =
            [
                new() { DocumentUri = CreateAbsoluteDocumentUri(tempDir.Path), Name = "workspace1" }
            ]
        });

        var discovery = testLspServer.GetRequiredLspService<FileBasedProgramsEntryPointDiscovery>();
        AssertEx.SequenceEqual([app2File.Path], discovery.FindEntryPoints(tempDir.Path));

        // Verify stability
        AssertEx.SequenceEqual([app2File.Path], discovery.FindEntryPoints(tempDir.Path));
    }

    [Fact]
    public async Task TestDiscovery_03()
    {
        // Demonstrate csproj-in-cone behavior
        // tempDir/
        //   Project/
        //     Project.csproj
        //     Program.cs
        //   App.cs

        var tempDir = _tempRoot.CreateDirectory();

        // Delete artifacts from possible previous runs of this test
        var cacheDirectory = VirtualProjectXmlProvider.GetDiscoveryCacheDirectory(tempDir.Path);
        if (Directory.Exists(cacheDirectory))
            Directory.Delete(cacheDirectory, recursive: true);

        var projectDir = tempDir.CreateDirectory("Project");
        var csprojFile = projectDir.CreateFile("Project.csproj");

        var appText = """
            #:sdk Microsoft.Net.SDK
            Console.WriteLine("Hello World");
            """;
        var programFile = projectDir.CreateFile("Program.cs").WriteAllText(appText);
        var appFile = tempDir.CreateFile("App1.cs").WriteAllText(appText);

        await using var testLspServer = await CreateTestLspServerAsync(string.Empty, mutatingLspWorkspace: false, new InitializationOptions
        {
            ServerKind = WellKnownLspServerKinds.CSharpVisualBasicLspServer,
            WorkspaceFolders =
            [
                new() { DocumentUri = CreateAbsoluteDocumentUri(tempDir.Path), Name = "workspace1" }
            ]
        });

        var discovery = testLspServer.GetRequiredLspService<FileBasedProgramsEntryPointDiscovery>();
        AssertEx.SequenceEqual([appFile.Path], discovery.FindEntryPoints(tempDir.Path));

        // Verify stability
        AssertEx.SequenceEqual([appFile.Path], discovery.FindEntryPoints(tempDir.Path));

        // Delete the csproj file
        File.Delete(csprojFile.Path);
        AssertEx.SequenceEqual([appFile.Path, programFile.Path], discovery.FindEntryPoints(tempDir.Path).ToArray());

        // Verify stability
        AssertEx.SequenceEqual([appFile.Path, programFile.Path], discovery.FindEntryPoints(tempDir.Path));
    }

    [Fact]
    public async Task TestDiscovery_04()
    {
        // Ensure discovery occurs when relevant options are enabled
        // Note: the option is checked in the higher level API, so we need to verify the effects in project system.
        var tempDir = _tempRoot.CreateDirectory();

        // Delete artifacts from possible previous runs of this test
        var cacheDirectory = VirtualProjectXmlProvider.GetDiscoveryCacheDirectory(tempDir.Path);
        if (Directory.Exists(cacheDirectory))
            Directory.Delete(cacheDirectory, recursive: true);

        var appText = """
            #:sdk Microsoft.Net.SDK
            Console.WriteLine("Hello World");
            """;
        var appFile = tempDir.CreateFile("App1.cs").WriteAllText(appText);

        await using var testLspServer = await CreateTestLspServerAsync(string.Empty, mutatingLspWorkspace: false, new InitializationOptions
        {
            ServerKind = WellKnownLspServerKinds.CSharpVisualBasicLspServer,
            OptionUpdater = options => options.SetGlobalOption(LanguageServerProjectSystemOptionsStorage.EnableFileBasedPrograms, true),
            WorkspaceFolders =
            [
                new() { DocumentUri = CreateAbsoluteDocumentUri(tempDir.Path), Name = "workspace1" }
            ]
        });

        var discovery = testLspServer.GetRequiredLspService<FileBasedProgramsEntryPointDiscovery>();
        await discovery.FindAndLoadEntryPointsAsync();
        await testLspServer.TestWorkspace.GetService<AsynchronousOperationListenerProvider>().GetWaiter(FeatureAttribute.Workspace).ExpeditedWaitAsync();
        var (workspace, document) = await GetRequiredLspWorkspaceAndDocumentAsync(CreateAbsoluteDocumentUri(appFile.Path), testLspServer);
        Assert.Equal(WorkspaceKind.Host, workspace.Kind);
        Assert.NotNull(document);
    }

    [Fact]
    public async Task TestDiscovery_05()
    {
        // Ensure discovery doesn't occur when relevant options are disabled
        // Note: the option is checked in the higher level API, so we need to verify the effects in project system.
        var tempDir = _tempRoot.CreateDirectory();

        // Delete artifacts from possible previous runs of this test
        var cacheDirectory = VirtualProjectXmlProvider.GetDiscoveryCacheDirectory(tempDir.Path);
        if (Directory.Exists(cacheDirectory))
            Directory.Delete(cacheDirectory, recursive: true);

        var appText = """
            #:sdk Microsoft.Net.SDK
            Console.WriteLine("Hello World");
            """;
        var appFile = tempDir.CreateFile("App1.cs").WriteAllText(appText);

        await using var testLspServer = await CreateTestLspServerAsync(string.Empty, mutatingLspWorkspace: false, new InitializationOptions
        {
            ServerKind = WellKnownLspServerKinds.CSharpVisualBasicLspServer,
            OptionUpdater = options => options.SetGlobalOption(LanguageServerProjectSystemOptionsStorage.EnableFileBasedPrograms, false),
            WorkspaceFolders =
            [
                new() { DocumentUri = CreateAbsoluteDocumentUri(tempDir.Path), Name = "workspace1" }
            ]
        });

        var discovery = testLspServer.GetRequiredLspService<FileBasedProgramsEntryPointDiscovery>();
        await discovery.FindAndLoadEntryPointsAsync();
        await testLspServer.TestWorkspace.GetService<AsynchronousOperationListenerProvider>().GetWaiter(FeatureAttribute.Workspace).ExpeditedWaitAsync();
        var (workspace, document) = await GetLspWorkspaceAndDocumentAsync(CreateAbsoluteDocumentUri(appFile.Path), testLspServer);
        Assert.Null(workspace);
        Assert.Null(document);
    }

    private static async Task<(Workspace? workspace, Document? document)> GetLspWorkspaceAndDocumentAsync(DocumentUri uri, TestLspServer testLspServer)
    {
        var (workspace, _, document) = await testLspServer.GetManager().GetLspDocumentInfoAsync(CreateTextDocumentIdentifier(uri), CancellationToken.None).ConfigureAwait(false);
        return (workspace, document as Document);
    }

    private static async Task<(Workspace workspace, Document document)> GetRequiredLspWorkspaceAndDocumentAsync(DocumentUri uri, TestLspServer testLspServer)
    {
        var (workspace, document) = await GetLspWorkspaceAndDocumentAsync(uri, testLspServer);
        Assert.NotNull(workspace);
        Assert.NotNull(document);
        return (workspace, document);
    }

    // TODO2: test discovery->moving a file->rediscovery
}
