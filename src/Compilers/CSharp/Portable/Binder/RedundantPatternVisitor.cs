// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;

namespace Microsoft.CodeAnalysis.CSharp;

internal sealed class RedundantPatternVisitor
{
    private readonly BindingDiagnosticBag _diagnostics;
    private readonly PatternInfo _originalPatternInfo;

    // The possible values for a given variable being checked by the pattern. i.e. 'PatternInfo.Slot'.
    private readonly List<IValueSet> _possibleValuesBySlot = [];
    private int _nextSlot = 0;
    private PatternSequenceKind _enclosingPatternSequenceKind;

    public RedundantPatternVisitor(BindingDiagnosticBag diagnostics, BoundExpression expression, BoundPattern pattern)
    {
        _diagnostics = diagnostics;
        _originalPatternInfo = new PatternInfo(parent: null, slot: _nextSlot++, pattern, expression.Type);
    }

    public void Analyze()
    {
        Visit(_originalPatternInfo);
    }

    // Postcondition: '_possibleValuesForVariable' is updated, to reflect what happens when the pattern succeeds
    private void Visit(PatternInfo patternInfo)
    {
        switch (patternInfo)
        {
            case { Pattern: BoundConstantPattern constantPattern }: VisitConstantPattern(patternInfo, constantPattern); break;
            case { Pattern: BoundBinaryPattern binaryPattern }: VisitBinaryPattern(patternInfo, binaryPattern); break;
            default: throw new InvalidOperationException();
        }
    }

    private IValueSet? GetValues(int slot)
    {
        return slot < _possibleValuesBySlot.Count
            ? _possibleValuesBySlot[slot]
            : null;
    }

    private void SetValues(int slot, IValueSet valueSet)
    {
        Debug.Assert(slot <= _possibleValuesBySlot.Count);
        if (slot < _possibleValuesBySlot.Count)
            _possibleValuesBySlot[slot] = valueSet;
        else
            _possibleValuesBySlot.Add(valueSet);
    }

    private void VisitConstantPattern(PatternInfo patternInfo, BoundConstantPattern constantPattern)
    {
        IValueSet? existingValues = GetValues(patternInfo.Slot);
        IValueSet? @newValue = ValueSetFactory.ForType(patternInfo.Pattern.InputType)?.Related(BinaryOperatorKind.Equal, constantPattern.ConstantValue);

        if (newValue is null)
            return;

        if (existingValues is null)
        {
            SetValues(patternInfo.Slot, newValue);
            return;
        }

        IValueSet result = _enclosingPatternSequenceKind == PatternSequenceKind.Or ? existingValues.Union(newValue) : existingValues.Intersect(newValue);
        if (result.Equals(existingValues))
        {
            // e.g. 'A op B' could have been simplified to just 'A'
            _diagnostics.Add(ErrorCode.WRN_RedundantPattern, patternInfo.Pattern.Syntax.Location);
        }
        else if (result.Equals(newValue))
        {
            // 'A op B' could have been simplified to just 'B'.
        }

        SetValues(patternInfo.Slot, result);
    }

    private void VisitBinaryPattern(PatternInfo patternInfo, BoundBinaryPattern binaryPattern)
    {
        var isOrPattern = binaryPattern.Disjunction;
        var savedPatternSequenceKind = _enclosingPatternSequenceKind;
        Visit(new PatternInfo(patternInfo.Parent, patternInfo.Slot, binaryPattern.Left, binaryPattern.Left.InputType));

        _enclosingPatternSequenceKind = isOrPattern ? PatternSequenceKind.Or : PatternSequenceKind.And;
        Visit(new PatternInfo(patternInfo.Parent, patternInfo.Slot, binaryPattern.Right, binaryPattern.Right.InputType));

        _enclosingPatternSequenceKind = savedPatternSequenceKind;
    }

    private enum PatternSequenceKind { Or, And };

    private class PatternInfo(PatternInfo? parent, int slot, BoundPattern pattern, TypeSymbol? inputType)
    {
        public PatternInfo? Parent { get; } = parent;
        public int Slot { get; } = slot;
        public BoundPattern Pattern { get; } = pattern;
        public TypeSymbol? InputType { get; } = inputType;
    }
}