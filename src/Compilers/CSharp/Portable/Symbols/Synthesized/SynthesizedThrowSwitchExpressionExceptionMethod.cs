// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CodeGen;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    /// <summary>
    /// Passes an unmatched value to <see cref="System.Runtime.CompilerServices.SwitchExpressionException(object)"/> and throws it.
    /// </summary>
    internal sealed class SynthesizedThrowSwitchExpressionExceptionWithValueMethod : SynthesizedGlobalMethodSymbol
    {
        internal SynthesizedThrowSwitchExpressionExceptionWithValueMethod(SourceModuleSymbol containingModule, PrivateImplementationDetails privateImplType, TypeSymbol returnType, TypeSymbol argumentParamType, TypeSymbol unmatchedValueParamType)
            : base(containingModule, privateImplType, returnType, PrivateImplementationDetails.SynthesizedThrowSwitchExpressionExceptionFunctionName)
        {
            this.SetParameters(ImmutableArray.Create<ParameterSymbol>(
                SynthesizedParameterSymbol.Create(this, TypeWithAnnotations.Create(argumentParamType), ordinal: 0, RefKind.None, "unmatchedValue")));
        }

        internal override void GenerateMethodBody(TypeCompilationState compilationState, BindingDiagnosticBag diagnostics)
        {
            SyntheticBoundNodeFactory F = new SyntheticBoundNodeFactory(this, this.GetNonNullSyntaxNode(), compilationState, diagnostics);
            F.CurrentFunction = this;

            try
            {
                ParameterSymbol unmatchedValue = this.Parameters[0];

                //throw new SwitchExpressionException(unmatchedValue);

                var body = F.Block(
                        ImmutableArray<LocalSymbol>.Empty,
                        F.Throw(F.New(F.WellKnownMethod(WellKnownMember.System_Runtime_CompilerServices_SwitchExpressionException__ctorObject), ImmutableArray.Create<BoundExpression>(F.Parameter(unmatchedValue)))));

                // NOTE: we created this block in its most-lowered form, so analysis is unnecessary
                F.CloseMethod(body);
            }
            catch (SyntheticBoundNodeFactory.MissingPredefinedMember ex)
            {
                diagnostics.Add(ex.Diagnostic);
                F.CloseMethod(F.ThrowNull());
            }
        }
    }
}
