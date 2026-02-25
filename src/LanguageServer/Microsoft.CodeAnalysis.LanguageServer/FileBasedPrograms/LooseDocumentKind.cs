// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

namespace Microsoft.CodeAnalysis.LanguageServer.FileBasedPrograms;

public enum LooseDocumentKind
{
    /// <summary>Part of an ordinary project.</summary>
    ProjectBasedApp,

    /// <summary>Bare miscellaneous file with no references.</summary>
    BareMiscFile,

    /// <summary>Miscellaneous file with references to standard library, but does not display semantic errors. Likely not a file-based app.</summary>
    RichMiscFile,

    /// <summary>Miscellaneous file with references to standard library and displays semantic errors. May be a file-based app.</summary>
    RichMiscFileWithSemanticErrors,

    /// <summary>Definitely a file-based app. Gets its own design-time build, restore etc.</summary>
    FileBasedApp,
}