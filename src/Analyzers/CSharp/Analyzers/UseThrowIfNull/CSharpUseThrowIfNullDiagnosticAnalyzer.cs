// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.CSharp.CodeStyle;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Shared.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.UseThrowIfNull
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    internal sealed class CSharpUseThrowIfNullDiagnosticAnalyzer
        : AbstractBuiltInCodeStyleDiagnosticAnalyzer
    {
        public const string ParameterName = nameof(ParameterName);

        private const string ArgumentNullExceptionName = $"{nameof(System)}.{nameof(ArgumentNullException)}";
        private static readonly LocalizableResourceString s_resourceTitle = new(nameof(AnalyzersResources.Null_check_can_be_simplified), AnalyzersResources.ResourceManager, typeof(AnalyzersResources));

        public CSharpUseThrowIfNullDiagnosticAnalyzer()
            : base(IDEDiagnosticIds.UseThrowIfNullId,
                   EnforceOnBuildValues.UseThrowIfNull,
                   CSharpCodeStyleOptions.PreferThrowIfNull,
                   LanguageNames.CSharp,
                   s_resourceTitle,
                   s_resourceTitle)
        {
        }

        public override DiagnosticAnalyzerCategory GetAnalyzerCategory()
            => DiagnosticAnalyzerCategory.SemanticSpanAnalysis;

        protected override void InitializeWorker(AnalysisContext context)
            => context.RegisterCompilationStartAction(context =>
            {
                var compilation = (CSharpCompilation)context.Compilation;
                if (compilation.LanguageVersion < LanguageVersionExtensions.CSharpNext)
                {
                    return;
                }

                var argumentNullException = compilation.GetBestTypeByMetadataName(ArgumentNullExceptionName);
                if (argumentNullException is null)
                {
                    return;
                }

                IMethodSymbol? argumentNullExceptionConstructor = null;
                IMethodSymbol? argumentNullExceptionStringConstructor = null;
                foreach (var constructor in argumentNullException.InstanceConstructors)
                {
                    if (argumentNullExceptionConstructor is not null && argumentNullExceptionStringConstructor is not null)
                    {
                        break;
                    }

                    switch (constructor)
                    {
                        case { DeclaredAccessibility: Accessibility.Public, Parameters.Length: 0 }:
                            argumentNullExceptionConstructor = constructor;
                            break;
                        case { DeclaredAccessibility: Accessibility.Public, Parameters.Length: 1 }
                            when constructor.Parameters[0].Type.SpecialType == SpecialType.System_String:

                            argumentNullExceptionStringConstructor = constructor;
                            break;
                    }
                }

                // Can only offer the fix if the method we want to use actually exists.
                IMethodSymbol? throwIfNullMethod = null;
                foreach (var symbol in argumentNullException.GetMembers("ThrowIfNull"))
                {
                    if (symbol is IMethodSymbol { DeclaredAccessibility: Accessibility.Public, IsStatic: true, Parameters: [{ Type.SpecialType: SpecialType.System_Object }, { Type.SpecialType: SpecialType.System_String }] } method)
                    {
                        throwIfNullMethod = method;
                        break;
                    }
                }

                if (argumentNullExceptionConstructor is null || argumentNullExceptionStringConstructor is null || throwIfNullMethod is null)
                {
                    return;
                }

                var objectType = compilation.GetSpecialType(SpecialType.System_Object);
                var referenceEqualsMethod = (IMethodSymbol?)objectType
                    .GetMembers(nameof(ReferenceEquals))
                    .FirstOrDefault(m => m is IMethodSymbol { DeclaredAccessibility: Accessibility.Public, Parameters.Length: 2 });

                // We are potentially interested in any declaration that has parameters.
                // However, we avoid indexers specifically because of the complexity of locating and deleting equivalent null checks across multiple accessors.
                context.RegisterSyntaxNodeAction(
                    context => AnalyzeSyntax(context, argumentNullExceptionConstructor, argumentNullExceptionStringConstructor, referenceEqualsMethod),
                    SyntaxKind.ConstructorDeclaration,
                    SyntaxKind.MethodDeclaration,
                    SyntaxKind.LocalFunctionStatement,
                    SyntaxKind.SimpleLambdaExpression,
                    SyntaxKind.ParenthesizedLambdaExpression,
                    SyntaxKind.AnonymousMethodExpression,
                    SyntaxKind.OperatorDeclaration,
                    SyntaxKind.ConversionOperatorDeclaration);
            });

        private void AnalyzeSyntax(
            SyntaxNodeAnalysisContext context,
            IMethodSymbol argumentNullExceptionConstructor,
            IMethodSymbol argumentNullExceptionStringConstructor,
            IMethodSymbol? referenceEqualsMethod)
        {
            var cancellationToken = context.CancellationToken;

            var semanticModel = context.SemanticModel;
            var syntaxTree = semanticModel.SyntaxTree;

            var option = context.Options.GetOption(CSharpCodeStyleOptions.PreferThrowIfNull, syntaxTree, cancellationToken);
            if (!option.Value)
            {
                return;
            }

            var node = context.Node;
            var block = node switch
            {
                MethodDeclarationSyntax methodDecl => methodDecl.Body,
                ConstructorDeclarationSyntax constructorDecl => constructorDecl.Body,
                LocalFunctionStatementSyntax localFunctionStatement => localFunctionStatement.Body,
                AnonymousFunctionExpressionSyntax anonymousFunction => anonymousFunction.Block,
                OperatorDeclarationSyntax operatorDecl => operatorDecl.Body,
                ConversionOperatorDeclarationSyntax conversionDecl => conversionDecl.Body,
                _ => throw ExceptionUtilities.UnexpectedValue(node)
            };

            // More scenarios should be supported eventually: https://github.com/dotnet/roslyn/issues/58699
            if (block is null)
            {
                return;
            }

            var methodSymbol = node is AnonymousFunctionExpressionSyntax
                ? (IMethodSymbol?)semanticModel.GetSymbolInfo(node, cancellationToken).Symbol
                : (IMethodSymbol?)semanticModel.GetDeclaredSymbol(node, cancellationToken);
            if (methodSymbol is null || methodSymbol.Parameters.IsEmpty)
            {
                return;
            }

            foreach (var statement in block.Statements)
            {
                if (TryGetParameterNullCheckedByStatement(statement) is var (parameter, diagnosticLocation)
                    && ParameterCanUseNullChecking(parameter))
                {
                    context.ReportDiagnostic(DiagnosticHelper.Create(
                        Descriptor,
                        diagnosticLocation,
                        option.Notification.Severity,
                        additionalLocations: null,
                        properties: ImmutableDictionary<string, string?>.Empty.Add(ParameterName, parameter.Name)));
                }
            }

            return;

            bool ParameterCanUseNullChecking([NotNullWhen(true)] IParameterSymbol? parameter)
            {
                if (parameter is null)
                    return false;

                if (parameter.RefKind == RefKind.Out)
                    return false;

                // TODO: we can also support on pointer types if the appropriate ThrowIfNull overload is present.
                // however, we shouldn't offer the fix on nullable value types, since we would have to box.
                return parameter.Type.IsReferenceType;
            }

            (IParameterSymbol parameter, Location diagnosticLocation)? TryGetParameterNullCheckedByStatement(StatementSyntax statement)
            {
                // if (param == null) { throw new ArgumentNullException(nameof(param)); }
                // if (param is null) { throw new ArgumentNullException(nameof(param)); }
                // if (object.ReferenceEquals(param, null)) { throw new ArgumentNullException(nameof(param)); }
                if (statement is IfStatementSyntax ifStatement)
                {
                    ExpressionSyntax left, right;
                    switch (ifStatement)
                    {
                        case { Condition: BinaryExpressionSyntax(SyntaxKind.EqualsExpression) binary }:
                            left = binary.Left;
                            right = binary.Right;
                            break;
                        case { Condition: IsPatternExpressionSyntax { Expression: var patternInput, Pattern: ConstantPatternSyntax { Expression: var patternExpression } } }:
                            left = patternInput;
                            right = patternExpression;
                            break;
                        case { Condition: InvocationExpressionSyntax { Expression: var receiver, ArgumentList.Arguments: { Count: 2 } arguments } }
                            when referenceEqualsMethod != null && referenceEqualsMethod.Equals(semanticModel.GetSymbolInfo(receiver, cancellationToken).Symbol):

                            left = arguments[0].Expression;
                            right = arguments[1].Expression;
                            break;

                        default:
                            return null;
                    }

                    var parameterInBinary = left.IsKind(SyntaxKind.NullLiteralExpression) ? TryGetParameter(right)
                        : right.IsKind(SyntaxKind.NullLiteralExpression) ? TryGetParameter(left)
                        : null;
                    if (parameterInBinary is null)
                    {
                        return null;
                    }

                    var throwStatement = ifStatement.Statement switch
                    {
                        ThrowStatementSyntax @throw => @throw,
                        BlockSyntax { Statements: { Count: 1 } statements } => statements[0] as ThrowStatementSyntax,
                        _ => null
                    };

                    if (throwStatement?.Expression is not ObjectCreationExpressionSyntax thrownInIf
                        || !IsConstructorApplicable(thrownInIf, parameterInBinary))
                    {
                        return null;
                    }

                    // The if statement could be associated with an arbitrarily complex else clause. We only want to highlight the "if" part which is removed by the fix.
                    var location = Location.Create(ifStatement.SyntaxTree, Text.TextSpan.FromBounds(ifStatement.SpanStart, ifStatement.Statement.Span.End));
                    return (parameterInBinary, location);
                }

                return null;
            }

            IParameterSymbol? TryGetParameter(ExpressionSyntax maybeParameter)
            {
                // `(object)x == null` is often used to ensure reference equality is used.
                // therefore, we specially unwrap casts when the cast is to `object`.
                if (maybeParameter is CastExpressionSyntax { Type: var type, Expression: var operand })
                {
                    if (semanticModel.GetTypeInfo(type).Type?.SpecialType != SpecialType.System_Object)
                    {
                        return null;
                    }

                    maybeParameter = operand;
                }

                if (semanticModel.GetSymbolInfo(maybeParameter).Symbol is not IParameterSymbol { ContainingSymbol: { } containingSymbol } parameterSymbol || !containingSymbol.Equals(methodSymbol))
                {
                    return null;
                }

                return parameterSymbol;
            }

            bool IsConstructorApplicable(ObjectCreationExpressionSyntax exceptionCreation, IParameterSymbol parameterSymbol)
            {
                if (exceptionCreation.ArgumentList?.Arguments is not { } arguments)
                {
                    return false;
                }

                // 'new ArgumentNullException()'
                if (argumentNullExceptionConstructor.Equals(semanticModel.GetSymbolInfo(exceptionCreation, cancellationToken).Symbol))
                {
                    return arguments.Count == 0;
                }

                // 'new ArgumentNullException(nameof(param))' (or equivalent)
                if (!argumentNullExceptionStringConstructor.Equals(semanticModel.GetSymbolInfo(exceptionCreation, cancellationToken).Symbol))
                {
                    return false;
                }

                if (arguments.Count != 1)
                {
                    return false;
                }

                var constantValue = semanticModel.GetConstantValue(arguments[0].Expression, cancellationToken);
                if (constantValue.Value is not string constantString || !string.Equals(constantString, parameterSymbol.Name, StringComparison.Ordinal))
                {
                    return false;
                }

                return true;
            }
        }
    }
}
