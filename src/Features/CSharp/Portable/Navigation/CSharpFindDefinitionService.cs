// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Composition;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.FileBasedPrograms;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.Navigation;
using Microsoft.CodeAnalysis.Shared.Extensions;

namespace Microsoft.CodeAnalysis.CSharp.Navigation;

[ExportLanguageService(typeof(INavigableItemsService), LanguageNames.CSharp), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class CSharpFindDefinitionService() : AbstractNavigableItemsService
{
    protected override async ValueTask<ImmutableArray<INavigableItem>> GetAdditionalNavigableItemsAsync(
        Document document, int position, CancellationToken cancellationToken)
    {
        var fileBasedProgramService = document.Project.Solution.Services.GetService<IFileBasedProgramService>();
        if (fileBasedProgramService is null ||
            await fileBasedProgramService.GetNavigationInfoAsync(document, position, cancellationToken).ConfigureAwait(false) is not { TargetFilePaths: var targetFilePaths })
        {
            return [];
        }

        var builder = ImmutableArray.CreateBuilder<INavigableItem>(targetFilePaths.Length);
        foreach (var targetFilePath in targetFilePaths)
        {
            foreach (var documentId in document.Project.Solution.GetDocumentIdsWithFilePath(targetFilePath))
            {
                builder.Add(NavigableItemFactory.GetItemFromDocument(
                    document.Project.Solution.GetRequiredDocument(documentId), sourceSpan: default));
            }
        }

        return builder.MoveToImmutable();
    }
}
