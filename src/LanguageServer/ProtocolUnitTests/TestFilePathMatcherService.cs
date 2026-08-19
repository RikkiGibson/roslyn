// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Composition;
using System.IO;
using Microsoft.CodeAnalysis.FileBasedPrograms;
using Microsoft.CodeAnalysis.Host.Mef;

namespace Microsoft.CodeAnalysis.LanguageServer.UnitTests;

[ExportWorkspaceService(typeof(IFilePathMatcherService))]
[Shared]
[PartNotDiscoverable]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class TestFilePathMatcherService() : IFilePathMatcherService
{
    public bool Matches(string baseDirectory, string pattern, string candidateFilePath)
        => pattern == "included/**/*.cs" &&
           candidateFilePath.StartsWith(Path.Combine(baseDirectory, "included"), StringComparison.OrdinalIgnoreCase) &&
           candidateFilePath.EndsWith(".cs", StringComparison.OrdinalIgnoreCase);
}
