// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

namespace Microsoft.CodeAnalysis.LanguageServer.FileBasedPrograms;

public enum LooseDocumentKind
{
    ProjectBasedApp,

    /// <summary>Bare miscellaneous file with no references.</summary>
    BareMiscFile,

    /// <summary>Miscellaneous file with references to standard library.</summary>
    RichMiscFile,
    RichMiscFileWithSemanticErrors,

    FileBasedApp,
}