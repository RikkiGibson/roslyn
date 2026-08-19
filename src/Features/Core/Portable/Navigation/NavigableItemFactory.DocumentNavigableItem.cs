// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.Navigation;

internal static partial class NavigableItemFactory
{
    private sealed class DocumentNavigableItem(Document document, TextSpan sourceSpan) : INavigableItem
    {
        public Glyph Glyph => document.Project.Language == LanguageNames.CSharp ? Glyph.CSharpFile : Glyph.BasicFile;
        public ImmutableArray<TaggedText> DisplayTaggedParts => [];
        public bool DisplayFileLocation => true;
        public bool IsImplicitlyDeclared => false;
        public INavigableItem.NavigableDocument Document { get; } = INavigableItem.NavigableDocument.FromDocument(document);
        public TextSpan SourceSpan { get; } = sourceSpan;
        public bool IsStale => false;
        public ImmutableArray<INavigableItem> ChildItems => [];
    }
}
