// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Text;
using Microsoft.DotNet.FileBasedPrograms;

namespace Microsoft.CodeAnalysis.FileBasedPrograms;

internal interface IFileBasedProgramService : IWorkspaceService
{
    ValueTask<FileBasedProgramDirectiveInfo?> GetDirectiveInfoAsync(
        Document document, int position, CancellationToken cancellationToken);

    ValueTask<FileBasedProgramNavigationInfo?> GetNavigationInfoAsync(
        Document document, int position, CancellationToken cancellationToken);

    string GetArtifactsPath(string entryPointFileFullPath, string? dotNetSubdirectory = null);

    string GetTempSubdirectory(string? dotNetSubdirectory = null);
    IDictionary<string, string> GetGlobalBuildProperties();

    bool IsValidEntryPointPath(string entryPointFilePath);

    ValueTask<IProjectRootElement> LoadFileBasedAppProjectAsync(
        IBuildService buildService,
        IProjectCollection projectCollection,
        string entryPointFilePath,
        Action<string> reportError);
}

internal enum FileBasedProgramDirectiveKind
{
    Include,
    Project,
    Ref,
}

internal readonly record struct FileBasedProgramDirectiveInfo(
    FileBasedProgramDirectiveKind Kind, string Value, TextSpan ValueSpan);

internal readonly record struct FileBasedProgramNavigationInfo(
    TextSpan ValueSpan, ImmutableArray<string> TargetFilePaths);
