// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Navigation;

internal interface INavigableItem
{
    Glyph Glyph { get; }

    /// <summary>
    /// The tagged parts to display for this item. If default, the line of text from <see
    /// cref="Document"/> is used.
    /// </summary>
    ImmutableArray<TaggedText> DisplayTaggedParts { get; }

    /// <summary>
    /// Return true to display the file path of <see cref="Document"/> and the span of <see
    /// cref="SourceSpan"/> when displaying this item.
    /// </summary>
    bool DisplayFileLocation { get; }

    /// <summary>
    /// his is intended for symbols that are ordinary symbols in the language sense, and may be
    /// used by code, but that are simply declared implicitly rather than with explicit language
    /// syntax.  For example, a default synthesized constructor in C# when the class contains no
    /// explicit constructors.
    /// </summary>
    bool IsImplicitlyDeclared { get; }

    NavigableDocument Document { get; }
    TextSpan SourceSpan { get; }

    /// <summary>
    /// True if this search result represents an item that existed in the past, but which may
    /// not exist currently, or which may have moved to a different location.  Consumers should
    /// be resilient to that being the case and not being able to necessarily navigate to the
    /// <see cref="SourceSpan"/> provided.
    /// </summary>
    bool IsStale { get; }

    ImmutableArray<INavigableItem> ChildItems { get; }

    /// <summary>
    /// A document-backed target, or a file-only target with a null <see cref="Id"/> and a
    /// <see cref="FileLocation"/> that can be used without loading document text.
    /// </summary>
    sealed record NavigableDocument(NavigableProject Project, string Name, string? FilePath, IReadOnlyList<string> Folders, DocumentId? Id, SourceGeneratedDocumentIdentity? SourceGeneratedDocumentIdentity, Workspace? Workspace, FileLinePositionSpan? FileLocation = null)
    {
        public static NavigableDocument FromDocument(Document document)
            => new(
                NavigableProject.FromProject(document.Project),
                document.Name,
                document.FilePath,
                document.Folders,
                document.Id,
                SourceGeneratedDocumentIdentity: (document as SourceGeneratedDocument)?.Identity,
                document.Project.Solution.TryGetWorkspace());

        public static NavigableDocument FromFileLocation(Project project, FileLinePositionSpan fileLocation)
        {
            Contract.ThrowIfFalse(fileLocation.IsValid && PathUtilities.IsAbsolute(fileLocation.Path));
            return new(
                NavigableProject.FromProject(project),
                Path.GetFileName(fileLocation.Path),
                fileLocation.Path,
                Folders: [],
                Id: null,
                SourceGeneratedDocumentIdentity: null,
                project.Solution.TryGetWorkspace(),
                fileLocation);
        }

        /// <summary>
        /// Get the <see cref="CodeAnalysis.Document"/> within <paramref name="solution"/> which is referenced by
        /// this navigable item. The document is required to exist within the solution, e.g. a case where the
        /// navigable item was constructed during a Find Symbols operation on the same solution instance.
        /// File-only targets must use <see cref="FileLocation"/> instead.
        /// </summary>
        internal ValueTask<Document> GetRequiredDocumentAsync(Solution solution, CancellationToken cancellationToken)
        {
            Contract.ThrowIfNull(Id, "File-only navigation targets do not have a document.");
            return solution.GetRequiredDocumentAsync(Id, includeSourceGenerated: SourceGeneratedDocumentIdentity is not null, cancellationToken);
        }

        internal async ValueTask<FileLinePositionSpan?> GetFileLocationAsync(Solution solution, TextSpan sourceSpan, CancellationToken cancellationToken)
        {
            if (Id is null)
            {
                Contract.ThrowIfNull(FileLocation, "File-only navigation targets must have a file location.");
                return FileLocation;
            }

            if (FilePath is null)
                return null;

            var text = await GetTextAsync(solution, cancellationToken).ConfigureAwait(false);
            return new FileLinePositionSpan(FilePath, text.Lines.GetLinePositionSpan(sourceSpan));
        }

        /// <summary>
        /// Get the <see cref="SourceText"/> of the <see cref="CodeAnalysis.Document"/> within
        /// <paramref name="solution"/> which is referenced by this navigable item. The document is required to
        /// exist within the solution, e.g. a case where the navigable item was constructed during a Find Symbols
        /// operation on the same solution instance.
        /// </summary>
        internal async ValueTask<SourceText> GetTextAsync(Solution solution, CancellationToken cancellationToken)
        {
            var document = await GetRequiredDocumentAsync(solution, cancellationToken).ConfigureAwait(false);
            return await document.GetValueTextAsync(cancellationToken).ConfigureAwait(false);
        }

        internal SourceText? TryGetTextSynchronously(Solution solution, CancellationToken cancellationToken)
        {
            var document = Id is null ? null : solution.GetDocument(Id);
            return document?.GetTextSynchronously(cancellationToken);
        }
    }

    record struct NavigableProject(string Name, ProjectId Id)
    {
        public static NavigableProject FromProject(Project project)
            => new(project.Name, project.Id);
    }
}
