// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.DotNet.FileBasedPrograms;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.FileBasedPrograms;

[ExportWorkspaceService(typeof(IFileBasedProgramService)), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class FileBasedProgramService() : IFileBasedProgramService
{
    public async ValueTask<FileBasedProgramNavigationInfo?> GetNavigationInfoAsync(
        Document document, int position, CancellationToken cancellationToken)
    {
        if (await GetDirectiveInfoAsync(document, position, cancellationToken).ConfigureAwait(false) is not { } directive)
            return null;

        if (directive.Kind == FileBasedProgramDirectiveKind.Project)
            return null;

        if (directive.Kind == FileBasedProgramDirectiveKind.Include && directive.Value.IndexOfAny(['*', '?']) >= 0)
        {
            var sourceDirectory = Path.GetDirectoryName(document.FilePath!);
            var pathMatcher = document.Project.Solution.Services.GetService<IFilePathMatcherService>();
            if (sourceDirectory is null || pathMatcher is null)
                return null;

            var matchingPaths = document.Project.Documents
                .WhereAsArray(static (candidate, arg) =>
                    candidate.FilePath is { } filePath && arg.pathMatcher.Matches(arg.sourceDirectory, arg.pattern, filePath),
                    (pathMatcher, sourceDirectory, pattern: directive.Value))
                .SelectAsArray(static candidate => candidate.FilePath!);

            return matchingPaths.IsEmpty ? null : new(directive.ValueSpan, matchingPaths);
        }

        if (TryResolvePath(document.FilePath!, directive.Value) is not { } resolvedPath)
            return null;

        var targetPaths = directive.Kind switch
        {
            FileBasedProgramDirectiveKind.Include => document.Project.Documents
                .WhereAsArray(static (candidate, path) => candidate.FilePath is { } filePath && PathUtilities.PathsEqual(filePath, path), resolvedPath)
                .SelectAsArray(static candidate => candidate.FilePath!),

            FileBasedProgramDirectiveKind.Ref => document.Project.ProjectReferences
                .Select(reference => document.Project.Solution.GetRequiredProject(reference.ProjectId))
                .SelectMany(static project => project.Documents)
                .WhereAsArray(static (candidate, path) => candidate.FilePath is { } filePath && PathUtilities.PathsEqual(filePath, path), resolvedPath)
                .SelectAsArray(static candidate => candidate.FilePath!),

            _ => throw ExceptionUtilities.UnexpectedValue(directive.Kind),
        };

        return targetPaths.IsEmpty ? null : new(directive.ValueSpan, targetPaths);

        static string? TryResolvePath(string sourceFilePath, string directivePath)
        {
            var sourceDirectory = Path.GetDirectoryName(sourceFilePath);
            if (sourceDirectory is null)
                return null;

            try
            {
                var combinedPath = Path.Combine(sourceDirectory, directivePath.Replace('\\', '/'));
                return PathUtilities.IsAbsolute(combinedPath) ? Path.GetFullPath(combinedPath) : null;
            }
            catch (Exception exception) when (exception is ArgumentException or IOException or NotSupportedException)
            {
                return null;
            }
        }
    }

    public async ValueTask<FileBasedProgramDirectiveInfo?> GetDirectiveInfoAsync(
        Document document, int position, CancellationToken cancellationToken)
    {
        if (document.Project.Language != LanguageNames.CSharp ||
            document.FilePath is null ||
            document.Project.ParseOptions?.Features.ContainsKey("FileBasedProgram") != true)
        {
            return null;
        }

        var text = await document.GetTextAsync(cancellationToken).ConfigureAwait(false);
        var directives = FileLevelDirectiveHelpers.FindDirectives(
            new SourceFile(document.FilePath, text),
            reportAllErrors: false,
            static (_, _, _, _, _) => { },
            checkDuplicates: false);

        foreach (var directive in directives)
        {
            var kind = directive switch
            {
                CSharpDirective.IncludeOrExclude { Kind: CSharpDirective.IncludeOrExcludeKind.Include } => FileBasedProgramDirectiveKind.Include,
                CSharpDirective.Project => FileBasedProgramDirectiveKind.Project,
                CSharpDirective.Ref => FileBasedProgramDirectiveKind.Ref,
                _ => (FileBasedProgramDirectiveKind?)null,
            };

            if (kind is not null &&
                position >= directive.Info.ValueSpan.Start &&
                position <= directive.Info.ValueSpan.End)
            {
                return new(kind.Value, ((CSharpDirective.Named)directive).Name, directive.Info.ValueSpan);
            }
        }

        return null;
    }

    public string GetArtifactsPath(string entryPointFileFullPath, string? dotNetSubdirectory = null)
        => VirtualProjectBuilder.GetArtifactsPath(entryPointFileFullPath, dotNetSubdirectory);

    public string GetTempSubdirectory(string? dotNetSubdirectory = null)
        => VirtualProjectBuilder.GetTempSubdirectory(dotNetSubdirectory);

    public IDictionary<string, string> GetGlobalBuildProperties()
    {
        var result = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        foreach (var kvp in VirtualProjectBuilder.GetGlobalBuildProperties())
        {
            result.Add(kvp.Key, kvp.Value);
        }
        return result;
    }

    public bool IsValidEntryPointPath(string entryPointFilePath)
        => VirtualProjectBuilder.IsValidEntryPointPath(entryPointFilePath);

    public async ValueTask<IProjectRootElement> LoadFileBasedAppProjectAsync(
        IBuildService buildService,
        IProjectCollection projectCollection,
        string entryPointFilePath,
        Action<string> reportError)
    {
        var entryPointFileFullPath = Path.GetFullPath(entryPointFilePath);
        var virtualProjectBuilder = new VirtualProjectBuilder(buildService, entryPointFileFullPath, targetFramework: null);
        var result = await virtualProjectBuilder.CreateProjectInstanceAsync(
            projectCollection,
            (text, path, textSpan, message, innerException) => reportError($"{new SourceFile(path, text).GetLocationString(textSpan)}: {message}"))
            .ConfigureAwait(false);
        return result.ProjectRootElement;
    }
}
