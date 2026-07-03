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

### Cci.IMethodDefinition.PlatformInvokeData interface not annotated nullable
- File: src/Compilers/CSharp/Portable/Emitter/Model/MethodSymbolAdapter.cs (Cci.IMethodDefinition.PlatformInvokeData)
- Status: open
- Migration handling: file nullable-enabled, this single property kept in a `#nullable disable` island (order 77)
- What we know: The Cci interface member `Cci.IMethodDefinition.PlatformInvokeData` (src/Compilers/Core/Portable/PEWriter/Members.cs) is typed non-nullable `IPlatformInvokeInformation` with a documented precondition `// ^ requires this.IsPlatformInvoke`. Implementations legitimately return null when the member is not a P/Invoke: this adapter returns `IsExtension ? null : GetDllImportData()`, `MethodDefinitionBase.PlatformInvokeData => null`, and `VtblGap` also islands the same member with `#nullable disable`. An explicit interface implementation cannot annotate its return nullable without matching a nullable interface member, so the honest annotation requires changing the shared Cci interface.
- Proposed fix: Annotate `Cci.IMethodDefinition.PlatformInvokeData` (and the base/other implementations) as nullable `IPlatformInvokeInformation?` in the Core Cci layer, then remove the islands here and in `VtblGap`. This is a shared-interface change touching Core (and consumed by the VB compiler), so it is out of scope for the behavior-preserving per-file migration.

### Cci.IModuleReference.GetContainingAssembly interface not annotated nullable
- File: src/Compilers/Core/Portable/Emit/CommonPEModuleBuilder.cs (Cci.IModuleReference.GetContainingAssembly)
- Status: open
- Migration handling: file nullable-enabled, this single method kept in a `#nullable disable` island (order 95)
- What we know: The Cci interface member `Cci.IModuleReference.GetContainingAssembly` (src/Compilers/Core/Portable/PEWriter/References.cs) is typed non-nullable `IAssemblyReference`. This base implementation legitimately returns null for netmodules: `return OutputKind == OutputKind.NetModule ? null : (Cci.IAssemblyReference)this;`. Callers null-check the result, so the effective contract is nullable. An explicit interface implementation cannot annotate its return nullable without matching a nullable interface member, so the honest annotation requires changing the shared Cci interface.
- Proposed fix: Annotate `Cci.IModuleReference.GetContainingAssembly` as returning nullable `IAssemblyReference?` in the Core Cci layer, then remove this island. This is a shared-interface change touching Core (and consumed by the VB compiler), so it is out of scope for the behavior-preserving per-file migration.

### TypeWithAnnotations.GetUnificationUseSiteDiagnosticRecursive ref params kept oblivious
- File: src/Compilers/CSharp/Portable/Symbols/TypeWithAnnotations.cs (GetUnificationUseSiteDiagnosticRecursive)
- Status: open
- Migration handling: file nullable-enabled, this single method kept in a `#nullable disable` island (order 107)
- What we know: The method has `ref DiagnosticInfo result` / `ref HashSet<TypeSymbol> checkedTypes` params. The honest annotation is `ref DiagnosticInfo?` / `ref HashSet<TypeSymbol>?` (result/checkedTypes start null and are lazily populated). But this method delegates to the abstract `TypeSymbol.GetUnificationUseSiteDiagnosticRecursive` (declared in the still-`#nullable disable` TypeSymbol.cs) and the static overloads in Symbol.cs. Annotating the ref params forced the derived overrides in ArrayTypeSymbol/PointerTypeSymbol to also annotate, churning derived types before their base type is enabled (violates base-before-derived). Kept oblivious to avoid that ripple.
- Proposed fix: When Symbol.cs/TypeSymbol.cs are nullable-enabled, annotate the abstract/virtual/static `GetUnificationUseSiteDiagnosticRecursive` ref params nullable there first, then remove this island and annotate the derived overrides in the same pass.

### Symbol.cs high-usage genuinely-nullable members deferred to dedicated stages
- File: src/Compilers/CSharp/Portable/Symbols/Symbol.cs (multiple members)
- Status: open
- Migration handling: file nullable-enabled (Stage A); the following high-usage genuinely-nullable members are kept in `#nullable disable` islands (oblivious signatures) so enabling them does not churn the ~100 already-migrated derived symbol types that override them. Each is to be enabled in its own dedicated commit (Stage B+) so its bug-catching ripple can be assessed in isolation.
- What we know: These members are legitimately nullable, but they are dereferenced at a large number of consumer/override sites that currently assume non-null. Deferring them keeps the base-type migration loop-friendly:
  - `ContainingSymbol` (abstract) and the `Containing*` family (`ContainingType`, `ContainingNamespace`, `ContainingAssembly`) plus `DeclaringCompilation` — return null at the top of the symbol hierarchy; ~50 derived overrides already declare `?` and ~20+ consumer deref sites assume non-null. Attempted (Stage B) and REVERTED: annotating `ContainingSymbol` as `Symbol?` produced 67 unique consumer errors across ~30 files, resolved almost entirely by `ContainingSymbol!` at the deref sites (the container is genuinely non-null for a concrete symbol kind in each context). Sprinkling `!` at every use site is not the desired end state — this cluster really wants a `RequiredContainingSymbol` (assert-non-null) accessor paired with the nullable `ContainingSymbol`, mirroring the IDE layer's `GetRequired*` pattern. Introducing that accessor reshapes the API at every call site, which is out of scope for the behavior-preserving annotation pass, so this stays DEFERRED (island retained).
  - `ContainingModule` — same shape as the `Containing*` family.
  - virtual `Equals(Symbol other, TypeCompareKind compareKind)` — honest param is `Symbol?`; annotating churns every derived `Equals` override.
  - `AddSynthesizedAttributes(PEModuleBuilder, ref ArrayBuilder<CSharpAttributeData> attributes)` and static `AddSynthesizedAttribute(ref ArrayBuilder<CSharpAttributeData> attributes, CSharpAttributeData attribute)` — the `ref ArrayBuilder<>` is genuinely nullable but `ref` invariance forces every override/caller to match in lockstep.
  - `MergeUseSiteDiagnostics(ref DiagnosticInfo result, DiagnosticInfo info)` — honest signature is `ref DiagnosticInfo?`; `ref` invariance was feared to cascade into use-site-diagnostic callers. RESOLVED (Stage B): enabled as `[NotNullIfNotNull(nameof(info))] ref DiagnosticInfo? result, DiagnosticInfo? info`; the `NotNullIfNotNull` postcondition let all callers keep their flow state, zero ripple.
  - `PrimaryDependency` — returns null for the core library; annotating cascades into several PE*/Retargeting* consumer sites that assign it to a non-nullable local and pass it to `ToUseSiteInfo(AssemblySymbol)`.
- Proposed fix: In dedicated follow-up commits, enable each cluster, annotating the base member and fixing the resulting consumer/override sites in the same pass; remove the island once each is clean. Exception: the `ContainingSymbol`/`Containing*`/`DeclaringCompilation` cluster should NOT be forced through with use-site `!`; it warrants a separate `RequiredContainingSymbol`-style accessor design (API reshaping, out of scope here) and remains deferred.

### Pre-existing EE consumer nullable errors surfaced by CompilerConsumers.slnf (not from Symbol.cs)
- File: src/ExpressionEvaluator/CSharp/Source/ExpressionCompiler/CompilationContext.cs#L1073, #L1237; src/ExpressionEvaluator/CSharp/Source/ExpressionCompiler/EETypeNameDecoder.cs#L67
- Status: open
- Migration handling: pre-existing (reproduce with Symbol.cs stashed); not introduced by any Stage A change.
- What we know: Building `CompilerConsumers.slnf` with `-p:WarningsAsErrors=nullable` reports 3 errors in the EE ExpressionCompiler consumer that already exist on the branch independent of Symbol.cs: two `CS8604` passing `binder.ContainingMemberOrLambda` (nullable) to `AliasSymbol.CreateCustomDebugInfoAlias(... Symbol containingSymbol ...)`, and one `CS8602` dereferencing `Module.GetReferencedAssemblySymbol(index)` (nullable). The EE project is not part of the primary per-file feedback build, so these leaked from an earlier migration commit.
- Proposed fix: Address as part of nullable-enabling the EE ExpressionCompiler files (or a dedicated cleanup), not as part of Symbol.cs.
