// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

// Throwaway diagnostic tool used to gauge progress of the nullable migration.
// Reports boundary crossings between nullable-enabled code and oblivious-nullability
// members (i.e. members whose type comes from a `#nullable disable` context),
// restricted to members declared in an assembly whose name contains "Microsoft.CodeAnalysis".
//
// NOTE: this intentionally does NOT use OperationKind.Conversion. An oblivious type and
// its annotated counterpart (e.g. oblivious `Symbol` vs. non-null `Symbol`) are the SAME
// underlying type with only the annotation differing, so Roslyn's IOperation tree does not
// synthesize a conversion node for it at all - nullable flow analysis tracks that distinction
// separately. So instead we look directly at member references/arguments/returns and compare
// declared annotations against the position's nullable context.
//
// OBL0001 "ConvertingFromOblivious": code in a nullable-enabled position reads an
//   oblivious-nullability member (e.g. calling an oblivious method from enabled code).
// OBL0002 "ConvertingToOblivious": a value with known (non-oblivious) annotation flows into
//   an oblivious-nullability destination (argument, return, or assignment target).

using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;

namespace ObliviousNullabilityAnalyzer;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public sealed class ObliviousNullabilityAnalyzer : DiagnosticAnalyzer
{
    public const string ConvertingFromObliviousId = "OBL0001";
    public const string ConvertingToObliviousId = "OBL0002";

    private static readonly DiagnosticDescriptor s_convertingFromOblivious = new(
        ConvertingFromObliviousId,
        title: "Reading an oblivious-nullability value from nullable-enabled code",
        messageFormat: "Nullable-enabled code reads oblivious-nullability member '{0}'",
        category: "Nullability",
        DiagnosticSeverity.Warning,
        isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor s_convertingToOblivious = new(
        ConvertingToObliviousId,
        title: "Feeding a nullable-enabled value into an oblivious-nullability destination",
        messageFormat: "Nullable-enabled value flows into oblivious-nullability member '{0}'",
        category: "Nullability",
        DiagnosticSeverity.Warning,
        isEnabledByDefault: true);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } =
        ImmutableArray.Create(s_convertingFromOblivious, s_convertingToOblivious);

    // Fully-qualified signature format that also includes parameter names (not just types), so a
    // reported parameter can be located within its declaring method's signature at a glance. Built
    // explicitly (not derived from a built-in format) since e.g. FullyQualifiedFormat omits
    // IncludeContainingType, which drops the containing type/method name entirely for constructors.
    private static readonly SymbolDisplayFormat s_methodSignatureFormat = new SymbolDisplayFormat(
        globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Omitted,
        typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
        genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters,
        memberOptions: SymbolDisplayMemberOptions.IncludeParameters | SymbolDisplayMemberOptions.IncludeContainingType,
        parameterOptions: SymbolDisplayParameterOptions.IncludeType | SymbolDisplayParameterOptions.IncludeName,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.UseSpecialTypes | SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers);

    public override void Initialize(AnalysisContext context)
    {
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
        context.EnableConcurrentExecution();
        context.RegisterOperationAction(AnalyzeMemberRead, OperationKind.Invocation, OperationKind.PropertyReference, OperationKind.FieldReference);
        context.RegisterOperationAction(AnalyzeArgument, OperationKind.Argument);
        context.RegisterOperationAction(AnalyzeReturn, OperationKind.Return);
        context.RegisterOperationAction(AnalyzeAssignment, OperationKind.SimpleAssignment, OperationKind.CoalesceAssignment);
    }

    // OBL0001: enabled code reading a member whose declared type is oblivious.
    private static void AnalyzeMemberRead(OperationAnalysisContext context)
    {
        var operation = context.Operation;

        (ISymbol member, ITypeSymbol type)? candidate = operation switch
        {
            IInvocationOperation invocation => (invocation.TargetMethod, invocation.TargetMethod.ReturnType),
            IPropertyReferenceOperation propertyRef => (propertyRef.Property, propertyRef.Property.Type),
            IFieldReferenceOperation fieldRef => (fieldRef.Field, fieldRef.Field.Type),
            _ => null,
        };

        if (candidate is not { } value)
        {
            return;
        }

        var (member, type) = value;

        if (!IsRelevantType(type) || type.NullableAnnotation != NullableAnnotation.None)
        {
            return;
        }

        if (!IsOfInterest(member))
        {
            return;
        }

        if (!IsEnabledContext(operation))
        {
            return;
        }

        context.ReportDiagnostic(Diagnostic.Create(s_convertingFromOblivious, operation.Syntax.GetLocation(), member.ToDisplayString()));
    }

    // OBL0002: an argument whose parameter is oblivious, fed a value with real annotation info.
    //
    // Registering on OperationKind.Argument (rather than separately on Invocation/ObjectCreation/
    // indexer PropertyReference/etc.) conveniently covers every member kind that takes arguments
    // in one place. But a member's parameters are usually uniformly oblivious or uniformly
    // enabled - the whole declaration sits on one side of a #nullable boundary - so a single call
    // passing several oblivious arguments is really just ONE finding ("this member is being used
    // from enabled code"), not one finding per parameter. Reporting per-parameter would bloat the
    // list and inflate counts proportionally to a member's parameter count. So: only the first
    // qualifying argument of a given call reports, and it reports the member being called, not
    // the specific parameter.
    private static void AnalyzeArgument(OperationAnalysisContext context)
    {
        var argument = (IArgumentOperation)context.Operation;
        var parameter = argument.Parameter;

        if (parameter is null || !IsRelevantType(parameter.Type) || parameter.Type.NullableAnnotation != NullableAnnotation.None)
        {
            return;
        }

        if (!IsOfInterest(parameter))
        {
            return;
        }

        if (!HasKnownAnnotation(argument.Value))
        {
            return;
        }

        if (!IsFirstQualifyingArgument(argument))
        {
            return;
        }

        var location = argument.Parent?.Syntax.GetLocation() ?? argument.Syntax.GetLocation();
        context.ReportDiagnostic(Diagnostic.Create(s_convertingToOblivious, location, parameter.ContainingSymbol.ToDisplayString(s_methodSignatureFormat)));
    }

    // True if no earlier argument in the same call already qualifies as an oblivious-destination
    // finding (so that call reports exactly once, via its first qualifying argument).
    private static bool IsFirstQualifyingArgument(IArgumentOperation argument)
    {
        var siblings = argument.Parent switch
        {
            IInvocationOperation invocation => invocation.Arguments,
            IObjectCreationOperation objectCreation => objectCreation.Arguments,
            IPropertyReferenceOperation propertyRef => propertyRef.Arguments,
            _ => default,
        };

        if (siblings.IsDefault)
        {
            return true;
        }

        foreach (var sibling in siblings)
        {
            if (sibling == argument)
            {
                return true;
            }

            var siblingParameter = sibling.Parameter;
            if (siblingParameter is not null
                && IsRelevantType(siblingParameter.Type)
                && siblingParameter.Type.NullableAnnotation == NullableAnnotation.None
                && IsOfInterest(siblingParameter)
                && HasKnownAnnotation(sibling.Value))
            {
                return false;
            }
        }

        return true;
    }

    // OBL0002: returning a value with real annotation info from a method whose return type is oblivious.
    private static void AnalyzeReturn(OperationAnalysisContext context)
    {
        var returnOp = (IReturnOperation)context.Operation;

        if (context.ContainingSymbol is not IMethodSymbol method
            || !IsRelevantType(method.ReturnType)
            || method.ReturnType.NullableAnnotation != NullableAnnotation.None)
        {
            return;
        }

        if (!IsOfInterest(method))
        {
            return;
        }

        if (returnOp.ReturnedValue is not { } returnedValue || !HasKnownAnnotation(returnedValue))
        {
            return;
        }

        context.ReportDiagnostic(Diagnostic.Create(s_convertingToOblivious, returnOp.Syntax.GetLocation(), method.ToDisplayString(s_methodSignatureFormat)));
    }

    // Does this expression's own type carry a real (non-oblivious) annotation, so that feeding
    // it into an oblivious destination genuinely discards known information?
    private static bool HasKnownAnnotation(IOperation value)
        => value.Type is { } type && IsRelevantType(type) && type.NullableAnnotation != NullableAnnotation.None;

    // OBL0002: assigning a value with real annotation info into an oblivious field/property.
    private static void AnalyzeAssignment(OperationAnalysisContext context)
    {
        var assignment = (IAssignmentOperation)context.Operation;

        (ISymbol member, ITypeSymbol type)? candidate = assignment.Target switch
        {
            IPropertyReferenceOperation propertyRef => (propertyRef.Property, propertyRef.Property.Type),
            IFieldReferenceOperation fieldRef => (fieldRef.Field, fieldRef.Field.Type),
            _ => null,
        };

        if (candidate is not { } value)
        {
            return;
        }

        var (member, type) = value;

        if (!IsRelevantType(type) || type.NullableAnnotation != NullableAnnotation.None)
        {
            return;
        }

        if (!IsOfInterest(member))
        {
            return;
        }

        if (!HasKnownAnnotation(assignment.Value))
        {
            return;
        }

        context.ReportDiagnostic(Diagnostic.Create(s_convertingToOblivious, assignment.Syntax.GetLocation(), member.ToDisplayString()));
    }

    private static bool IsEnabledContext(IOperation operation)
    {
        var model = operation.SemanticModel;
        if (model is null)
        {
            return false;
        }

        var nullableContext = model.GetNullableContext(operation.Syntax.SpanStart);
        return (nullableContext & NullableContext.AnnotationsEnabled) == NullableContext.AnnotationsEnabled;
    }

    private static bool IsRelevantType(ITypeSymbol type)
        => type.IsReferenceType || type.TypeKind == TypeKind.TypeParameter;

    private static bool IsOfInterest(ISymbol symbol)
    {
        var assemblyName = symbol.ContainingAssembly?.Name;
        return assemblyName is not null && assemblyName.Contains("Microsoft.CodeAnalysis");
    }
}

