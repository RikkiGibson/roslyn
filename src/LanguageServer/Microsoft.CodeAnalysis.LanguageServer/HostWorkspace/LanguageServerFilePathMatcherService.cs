// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Composition;
using Microsoft.CodeAnalysis.FileBasedPrograms;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.Extensions.FileSystemGlobbing;

namespace Microsoft.CodeAnalysis.LanguageServer.HostWorkspace;

[ExportWorkspaceService(typeof(IFilePathMatcherService), ServiceLayer.Host), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class LanguageServerFilePathMatcherService() : IFilePathMatcherService
{
    public bool Matches(string baseDirectory, string pattern, string candidateFilePath)
        => MatchesPath(baseDirectory, pattern, candidateFilePath);

    internal static bool MatchesPath(string baseDirectory, string pattern, string candidateFilePath)
    {
        var matcher = new Matcher();
        matcher.AddInclude(pattern);
        return matcher.Match(baseDirectory, candidateFilePath).HasMatches;
    }
}
