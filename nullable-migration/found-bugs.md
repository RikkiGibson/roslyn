# Nullability bugs found during the migration

Real nullability defects (a genuinely missing null check, a field that can be null where callers
assume otherwise, etc.) discovered while nullable-enabling files. These are **not** fixed as part of
the migration commit — the migration must stay behavior-preserving. Each entry is fixed separately so
the semantic change gets its own review.

## How to use this log
When you find a real bug while enabling a file:
1. Add an entry below.
2. Keep the migration commit behavior-preserving (annotate to the *true* nullability and either defer
   the file, or land it with a clearly-commented, behavior-preserving guard referencing the entry via
   a `TODO2`). Do **not** insert or remove a runtime null check as part of the migration.
3. Fix the bug in a separate change with its own scrutiny; then check the entry off.

## Entry template
```
### <short title>
- File: <path>#L<line>
- Status: open | fix-in-progress | fixed (<commit/PR>)
- Migration handling: deferred | landed-with-guard (<where>)
- What we know: <why this is a real bug — the code path where null can flow and is dereferenced/cast>
- Proposed fix: <the semantic change under consideration>
```

## Entries

### TypeWithAnnotations.GetPublicSymbol returns null but is typed non-nullable
- File: src/Compilers/CSharp/Portable/Symbols/NullableAnnotationExtensions.cs (GetPublicSymbol(this TypeWithAnnotations))
- Status: open
- Migration handling: file nullable-enabled, this single method kept in a `#nullable disable` island (order 16 landed partially)
- What we know: The body is `return type.Type?.GetITypeSymbol(type.ToPublicAnnotation());`. When `type.Type` is null the `?.` short-circuits and the method returns null, yet the declared return type is non-nullable `ITypeSymbol`. Every observed caller feeds the result into a non-null public `Type` property (`IFieldSymbol.Type`, `IPropertySymbol.Type`, `IArrayTypeSymbol.ElementType`, etc.), so the contract appears to be non-null in practice and `type.Type` is presumably non-null at those sites. But the `Type?.` is likely intentional, not accidental — we need the caller to tell us whether it expected the `TypeWithAnnotations` to have a type.
- Proposed fix: Investigate whether `type.Type` can actually be null here (e.g. change `?.` to `.` and see which tests fail). Resolving it cleanly probably means splitting the API so callers state their expectation — a `GetPublicSymbol` (nullable) vs `GetRequiredPublicSymbol` (non-null) pair, as the Roslyn IDE layer does. That changes API surface, so it is out of scope for the behavior-preserving migration and is deferred.
