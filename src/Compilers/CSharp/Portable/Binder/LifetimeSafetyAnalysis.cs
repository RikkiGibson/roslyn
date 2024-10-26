// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal struct ExpressionWithLifetimes
    {
        internal required BoundExpression Expression { get; init; }

        /// <summary>The lifetime of a 'ref' to this expression.</summary>
        internal required Lifetime RefLifetime { get; init; }

        /// <summary>The lifetime of 'ref's within the value of this expression.</summary>
        internal required Lifetime ValueLifetime { get; init; }
    }

    internal struct TargetLifetimes
    {
        /// <summary>Returns 'true' if the target is a ref, e.g. a by-ref return or parameter.</summary>
        internal required bool IsByRef { get; init; }

        /// <summary>Is a readonly ref being taken to the source expression. Implies 'IsByRef'.</summary>
        internal required bool IsRefReadonly { get; init; }

        /// <summary>The ref lifetime of the target, if applicable.</summary>
        internal required Lifetime RefLifetime { get; init; }

        /// <summary>The value lifetime of the target.</summary>
        internal required Lifetime ValueLifetime { get; init; }
    }

    internal sealed class LifetimeSafetyAnalysis : BoundTreeWalkerWithStackGuardWithoutRecursionOnTheLeftOfBinaryOperator
    {
        private readonly CSharpCompilation _compilation;
        private readonly MethodSymbol _symbol;
        private readonly BoundNode _body;
        private readonly bool _inUnsafeRegion;
        private readonly bool _useUpdatedEscapeRules;
        private readonly BindingDiagnosticBag _diagnostics;

        private Lifetime _localLifetime;
        private ExpressionWithLifetimes _visitResult;

        private LifetimeSafetyAnalysis(
            CSharpCompilation compilation,
            MethodSymbol symbol,
            BoundNode body,
            bool inUnsafeRegion,
            bool useUpdatedEscapeRules,
            BindingDiagnosticBag diagnostics)
        {
            _compilation = compilation;
            _symbol = symbol;
            _body = body;
            _inUnsafeRegion = inUnsafeRegion;
            _useUpdatedEscapeRules = useUpdatedEscapeRules;
            _diagnostics = diagnostics;

            _localLifetime = Lifetime.CurrentMethod;
        }

        internal static void Analyze(CSharpCompilation compilation, MethodSymbol symbol, BoundNode body, BindingDiagnosticBag diagnostics)
        {
            var visitor = new LifetimeSafetyAnalysis(
                compilation,
                symbol,
                body,
                inUnsafeRegion: InUnsafeMethod(symbol),
                useUpdatedEscapeRules: symbol.ContainingModule.UseUpdatedEscapeRules,
                diagnostics);
            try
            {
                visitor.Visit(body);
            }
            catch (CancelledByStackGuardException e)
            {
                e.AddAnError(diagnostics);
            }
        }

        private static bool InUnsafeMethod(Symbol symbol)
        {
            if (symbol is SourceMemberMethodSymbol { IsUnsafe: true })
            {
                return true;
            }

            var type = symbol.ContainingType;
            while (type is { })
            {
                var def = type.OriginalDefinition;
                if (def is SourceMemberContainerTypeSymbol { IsUnsafe: true })
                {
                    return true;
                }
                type = def.ContainingType;
            }

            return false;
        }

        /// <summary>
        /// Returns true if the conversion is valid.
        /// </summary>
        internal static bool CheckConversion(ExpressionWithLifetimes source, TargetLifetimes target, DiagnosticBag diagnostics)
        {
            // TODO2: how should specific diagnostics work?
            // Does the caller need to issue the appropriate diagnostic?
            // How to do this with minimal repetition of logic.
            // Perhaps errors for bad conversions would be a decent starting point here

            if (!target.IsByRef)
            {
                if (!source.ValueLifetime.IsConvertibleTo(target.ValueLifetime))
                {
                    diagnostics.Add(ErrorCode.ERR_ModuleEmitFailure, source.Expression.Syntax.Location);
                    return false;
                }

                return true;
            }

            if (!source.RefLifetime.IsConvertibleTo(target.RefLifetime))
            {
                diagnostics.Add(ErrorCode.ERR_ModuleEmitFailure, source.Expression.Syntax.Location);
                return false;
            }

            if (target.IsRefReadonly)
            {
                if (!source.ValueLifetime.IsConvertibleTo(target.ValueLifetime))
                {
                    diagnostics.Add(ErrorCode.ERR_ModuleEmitFailure, source.Expression.Syntax.Location);
                    return false;
                }

                return true;
            }

            if (!source.ValueLifetime.Equals(target.ValueLifetime))
            {
                diagnostics.Add(ErrorCode.ERR_ModuleEmitFailure, source.Expression.Syntax.Location);
                return false;
            }

            return true;
        }
    }
}
