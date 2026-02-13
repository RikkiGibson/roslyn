// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Composition;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.LanguageServer.Handler;
using Microsoft.CommonLanguageServerProtocol.Framework;
using Roslyn.LanguageServer.Protocol;

namespace Microsoft.CodeAnalysis.LanguageServer.FileBasedPrograms;

[ExportCSharpVisualBasicStatelessLspService(typeof(DidChangeWorkspaceFoldersHandler)), Shared]
[Method(Methods.WorkspaceDidChangeWorkspaceFoldersName)]
internal sealed class DidChangeWorkspaceFoldersHandler : ILspServiceNotificationHandler<DidChangeWorkspaceFoldersParams>
{
    [ImportingConstructor]
    [Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
    public DidChangeWorkspaceFoldersHandler()
    {
    }

    public bool MutatesSolutionState => false;
    public bool RequiresLSPSolution => false;

    async Task INotificationHandler<DidChangeWorkspaceFoldersParams, RequestContext>.HandleNotificationAsync(DidChangeWorkspaceFoldersParams request, RequestContext requestContext, CancellationToken cancellationToken)
    {
        // Note: The notification we received, contains only 'Added' and 'Removed' items.
        // For simplicity, we ask the client for the latest set instead of computing it from the most recent version.
        var clientManager = requestContext.GetRequiredService<IClientLanguageServerManager>();
        var workspaceFolders = await clientManager.SendRequestAsync<object?, WorkspaceFolder[]?>(
            Methods.WorkspaceFoldersName,
            null,
            CancellationToken.None);

        WorkspaceFoldersChanged?.Invoke(this, workspaceFolders);
    }

    public event EventHandler<WorkspaceFolder[]?>? WorkspaceFoldersChanged;
}
