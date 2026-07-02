// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using System;
using Roslyn.Utilities;
using System.Linq;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal abstract partial class BoundTreeVisitor<A, R>
    {
        protected BoundTreeVisitor()
        {
        }

        public virtual R Visit(BoundNode? node, A arg)
        {
            if (node == null)
            {
                return default(R)!;
            }

            // this switch contains fewer than 50 of the most common node kinds
            switch (node.Kind)
            {
                case BoundKind.TypeExpression:
                    return VisitTypeExpression((BoundTypeExpression)node, arg);
                case BoundKind.NamespaceExpression:
                    return VisitNamespaceExpression((BoundNamespaceExpression)node, arg);
                case BoundKind.UnaryOperator:
                    return VisitUnaryOperator((BoundUnaryOperator)node, arg);
                case BoundKind.IncrementOperator:
                    return VisitIncrementOperator((BoundIncrementOperator)node, arg);
                case BoundKind.BinaryOperator:
                    return VisitBinaryOperator((BoundBinaryOperator)node, arg);
                case BoundKind.CompoundAssignmentOperator:
                    return VisitCompoundAssignmentOperator((BoundCompoundAssignmentOperator)node, arg);
                case BoundKind.AssignmentOperator:
                    return VisitAssignmentOperator((BoundAssignmentOperator)node, arg);
                case BoundKind.NullCoalescingOperator:
                    return VisitNullCoalescingOperator((BoundNullCoalescingOperator)node, arg);
                case BoundKind.ConditionalOperator:
                    return VisitConditionalOperator((BoundConditionalOperator)node, arg);
                case BoundKind.ArrayAccess:
                    return VisitArrayAccess((BoundArrayAccess)node, arg);
                case BoundKind.TypeOfOperator:
                    return VisitTypeOfOperator((BoundTypeOfOperator)node, arg);
                case BoundKind.DefaultLiteral:
                    return VisitDefaultLiteral((BoundDefaultLiteral)node, arg);
                case BoundKind.DefaultExpression:
                    return VisitDefaultExpression((BoundDefaultExpression)node, arg);
                case BoundKind.IsOperator:
                    return VisitIsOperator((BoundIsOperator)node, arg);
                case BoundKind.AsOperator:
                    return VisitAsOperator((BoundAsOperator)node, arg);
                case BoundKind.Conversion:
                    return VisitConversion((BoundConversion)node, arg);
                case BoundKind.SequencePointExpression:
                    return VisitSequencePointExpression((BoundSequencePointExpression)node, arg);
                case BoundKind.SequencePoint:
                    return VisitSequencePoint((BoundSequencePoint)node, arg);
                case BoundKind.SequencePointWithSpan:
                    return VisitSequencePointWithSpan((BoundSequencePointWithSpan)node, arg);
                case BoundKind.Block:
                    return VisitBlock((BoundBlock)node, arg);
                case BoundKind.LocalDeclaration:
                    return VisitLocalDeclaration((BoundLocalDeclaration)node, arg);
                case BoundKind.MultipleLocalDeclarations:
                    return VisitMultipleLocalDeclarations((BoundMultipleLocalDeclarations)node, arg);
                case BoundKind.Sequence:
                    return VisitSequence((BoundSequence)node, arg);
                case BoundKind.NoOpStatement:
                    return VisitNoOpStatement((BoundNoOpStatement)node, arg);
                case BoundKind.ReturnStatement:
                    return VisitReturnStatement((BoundReturnStatement)node, arg);
                case BoundKind.ThrowStatement:
                    return VisitThrowStatement((BoundThrowStatement)node, arg);
                case BoundKind.ExpressionStatement:
                    return VisitExpressionStatement((BoundExpressionStatement)node, arg);
                case BoundKind.BreakStatement:
                    return VisitBreakStatement((BoundBreakStatement)node, arg);
                case BoundKind.ContinueStatement:
                    return VisitContinueStatement((BoundContinueStatement)node, arg);
                case BoundKind.IfStatement:
                    return VisitIfStatement((BoundIfStatement)node, arg);
                case BoundKind.ForEachStatement:
                    return VisitForEachStatement((BoundForEachStatement)node, arg);
                case BoundKind.TryStatement:
                    return VisitTryStatement((BoundTryStatement)node, arg);
                case BoundKind.Literal:
                    return VisitLiteral((BoundLiteral)node, arg);
                case BoundKind.ThisReference:
                    return VisitThisReference((BoundThisReference)node, arg);
                case BoundKind.Local:
                    return VisitLocal((BoundLocal)node, arg);
                case BoundKind.Parameter:
                    return VisitParameter((BoundParameter)node, arg);
                case BoundKind.LabelStatement:
                    return VisitLabelStatement((BoundLabelStatement)node, arg);
                case BoundKind.GotoStatement:
                    return VisitGotoStatement((BoundGotoStatement)node, arg);
                case BoundKind.LabeledStatement:
                    return VisitLabeledStatement((BoundLabeledStatement)node, arg);
                case BoundKind.StatementList:
                    return VisitStatementList((BoundStatementList)node, arg);
                case BoundKind.ConditionalGoto:
                    return VisitConditionalGoto((BoundConditionalGoto)node, arg);
                case BoundKind.Call:
                    return VisitCall((BoundCall)node, arg);
                case BoundKind.ObjectCreationExpression:
                    return VisitObjectCreationExpression((BoundObjectCreationExpression)node, arg);
                case BoundKind.DelegateCreationExpression:
                    return VisitDelegateCreationExpression((BoundDelegateCreationExpression)node, arg);
                case BoundKind.FieldAccess:
                    return VisitFieldAccess((BoundFieldAccess)node, arg);
                case BoundKind.PropertyAccess:
                    return VisitPropertyAccess((BoundPropertyAccess)node, arg);
                case BoundKind.Lambda:
                    return VisitLambda((BoundLambda)node, arg);
                case BoundKind.NameOfOperator:
                    return VisitNameOfOperator((BoundNameOfOperator)node, arg);
            }

            return VisitInternal(node, arg);
        }

        public virtual R DefaultVisit(BoundNode node, A arg)
        {
            return default(R)!;
        }
    }

    internal abstract partial class BoundTreeVisitor
    {
        protected BoundTreeVisitor()
        {
        }

        [DebuggerHidden]
        [return: NotNullIfNotNull(nameof(node))]
        public virtual BoundNode? Visit(BoundNode? node)
        {
            if (node != null)
            {
                // Rewriters preserve non-nullness (see NotNullIfNotNull); walkers return null but discard the result.
                return node.Accept(this)!;
            }

            return null;
        }

        [DebuggerHidden]
        public virtual BoundNode? DefaultVisit(BoundNode node)
        {
            return null;
        }

        public class CancelledByStackGuardException : Exception
        {
            public readonly BoundNode Node;

            public CancelledByStackGuardException(Exception inner, BoundNode node)
                : base(inner.Message, inner)
            {
                Node = node;
            }

            public void AddAnError(DiagnosticBag diagnostics)
            {
                diagnostics.Add(ErrorCode.ERR_InsufficientStack, GetTooLongOrComplexExpressionErrorLocation(Node));
            }

            public void AddAnError(BindingDiagnosticBag diagnostics)
            {
                diagnostics.Add(ErrorCode.ERR_InsufficientStack, GetTooLongOrComplexExpressionErrorLocation(Node));
            }

            public static Location GetTooLongOrComplexExpressionErrorLocation(BoundNode node)
            {
                SyntaxNode syntax = node.Syntax;

                if (syntax is not (ExpressionSyntax or PatternSyntax))
                {
                    syntax = syntax.DescendantNodes(n => n is not (ExpressionSyntax or PatternSyntax)).FirstOrDefault(n => n is ExpressionSyntax or PatternSyntax) ?? syntax;
                }

                return syntax.GetFirstToken().GetLocation();
            }
        }

        /// <summary>
        /// Consumers must provide implementation for <see cref="VisitExpressionOrPatternWithoutStackGuard"/>.
        /// </summary>
        [DebuggerStepThrough]
        protected BoundNode VisitExpressionOrPatternWithStackGuard(ref int recursionDepth, BoundNode node)
        {
            Debug.Assert(node is BoundExpression or BoundPattern);
            BoundNode? result;
            recursionDepth++;
#if DEBUG
            int saveRecursionDepth = recursionDepth;
#endif

            if (recursionDepth > 1 || !ConvertInsufficientExecutionStackExceptionToCancelledByStackGuardException())
            {
                EnsureSufficientExecutionStack(recursionDepth);

                result = VisitExpressionOrPatternWithoutStackGuard(node);
            }
            else
            {
                result = VisitExpressionOrPatternWithStackGuard(node);
            }

#if DEBUG
            Debug.Assert(saveRecursionDepth == recursionDepth);
#endif
            recursionDepth--;
            return result!;
        }

        [DebuggerStepThrough]
        protected virtual void EnsureSufficientExecutionStack(int recursionDepth)
        {
            StackGuard.EnsureSufficientExecutionStack(recursionDepth);
        }

        protected virtual bool ConvertInsufficientExecutionStackExceptionToCancelledByStackGuardException()
        {
            return true;
        }

        [DebuggerStepThrough]
        private BoundNode? VisitExpressionOrPatternWithStackGuard(BoundNode node)
        {
            try
            {
                return VisitExpressionOrPatternWithoutStackGuard(node);
            }
            catch (InsufficientExecutionStackException ex)
            {
                throw new CancelledByStackGuardException(ex, node);
            }
        }

        /// <summary>
        /// We should be intentional about behavior of derived classes regarding guarding against stack overflow.
        /// </summary>
        protected abstract BoundNode? VisitExpressionOrPatternWithoutStackGuard(BoundNode node);
    }
}
