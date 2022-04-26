// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.UseThrowIfNull
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = PredefinedCodeFixProviderNames.UseThrowIfNull), Shared]
    internal sealed class CSharpUseThrowIfNullCodeFixProvider : SyntaxEditorBasedCodeFixProvider
    {
        [ImportingConstructor]
        [Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
        public CSharpUseThrowIfNullCodeFixProvider()
        {
        }

        public override ImmutableArray<string> FixableDiagnosticIds
            => ImmutableArray.Create(IDEDiagnosticIds.UseThrowIfNullId);

        public override Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            RegisterCodeFix(context, CSharpAnalyzersResources.Use_parameter_null_checking, nameof(CSharpAnalyzersResources.Use_parameter_null_checking));
            return Task.CompletedTask;
        }

        protected override Task FixAllAsync(
            Document document, ImmutableArray<Diagnostic> diagnostics,
            SyntaxEditor editor, CodeActionOptionsProvider options, CancellationToken cancellationToken)
        {
            // before: if (param is null) throw...
            // after:: ArgumentNullException.ThrowIfNull(param);
            foreach (var diagnostic in diagnostics)
            {
                var node = diagnostic.Location.FindNode(getInnermostNodeForTie: true, cancellationToken: cancellationToken);
                var paramName = diagnostic.Properties[CSharpUseThrowIfNullDiagnosticAnalyzer.ParameterName];

                var throwIfNullStatement = SyntaxFactory.ParseStatement($"{nameof(System)}.{nameof(ArgumentNullException)}.ThrowIfNull({paramName})");
                switch (node)
                {
                    case IfStatementSyntax { Else.Statement: BlockSyntax { Statements: var statementsWithinElse } } ifStatementWithElseBlock:
                        var parent = (BlockSyntax)ifStatementWithElseBlock.GetRequiredParent();
                        var newStatements = parent.Statements.ReplaceRange(ifStatementWithElseBlock, statementsWithinElse.Select(s => s.WithPrependedLeadingTrivia(SyntaxFactory.ElasticMarker)));
                        editor.InsertBefore(parent, throwIfNullStatement);
                        editor.ReplaceNode(parent, parent.WithStatements(newStatements));
                        break;
                    case IfStatementSyntax { Else.Statement: StatementSyntax statementWithinElse }:
                        editor.InsertBefore(node, throwIfNullStatement);
                        editor.ReplaceNode(node, statementWithinElse.WithPrependedLeadingTrivia(SyntaxFactory.ElasticMarker));
                        break;
                    case IfStatementSyntax:
                        editor.ReplaceNode(node, throwIfNullStatement);
                        break;
                    default:
                        throw ExceptionUtilities.UnexpectedValue(node);
                }
            }

            return Task.CompletedTask;
        }
    }
}
