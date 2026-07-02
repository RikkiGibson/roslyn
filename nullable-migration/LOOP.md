# Nullable-enable loop — per-iteration instructions

You are processing ONE file per iteration from `nullable-migration/worklist.json`.
Do exactly one file, commit, then stop. The loop driver will re-invoke you for the next file.

## Goal
`src/Compilers` projects already default to `<Nullable>enable</Nullable>`. The files in the
worklist opt out with a top-of-file `#nullable disable` and sprinkle `#nullable enable`/`disable`
toggles throughout. "Nullable-enabling" a file means removing those directives so the whole file
inherits the project default, then resolving the resulting warnings.

## Hard constraints
- **No runtime semantic changes.** Do not add/remove null checks, throws, or branches that change
  behavior at runtime.
- **First ask: is a null check already happening in a way the compiler can't see?** Most warnings in
  these files come from an invariant that IS enforced at runtime but is not visible to flow analysis
  (a guard property/method, a preceding `Debug.Assert`, an initialization order, a `TryGet` pattern).
  The correct fix is to *teach the compiler about that existing check* with an attribute — not to
  silence it. Preferred attributes:
  `[MemberNotNull]` / `[MemberNotNullWhen]` (a property/method guarantees a field is non-null),
  `[NotNullWhen]` / `[MaybeNullWhen]` (a bool return implies (non-)nullness of an out/param),
  `[NotNullIfNotNull]`, `[DisallowNull]`, `[AllowNull]`, `[return: NotNull]`.
  Example from this migration: a `bool HasUniqueSymbol` getter that checks `_field != null` should be
  annotated `[MemberNotNullWhen(true, nameof(_field))]`, so the guarded cast needs no `!`.
- Use the null-forgiving operator `!` only as a **last resort**, when an invariant is genuinely known
  but cannot be expressed with an attribute or a local check. Prefer pairing it with a
  `Debug.Assert(...)` (assert is release-time no-op, so no runtime change). Do not sprinkle `!` to make
  warnings disappear — that hides real information and is not the kind of fix we want.
- **Do NOT change the nullability of public APIs.** This loop is out of scope for public surface
  (e.g. `ISymbol` and anything tracked in `PublicAPI.Shipped.txt` / `PublicAPI.Unshipped.txt`). Adding
  or removing `?` on a public member's parameter/return/property is an API change requiring separate
  review and API-tracking updates. If a warning can only be resolved by changing a public API's
  nullability, **DEFER the file** (note: "requires public API nullability change"). Do not annotate the
  public signature to make the warning go away. (Safety net: accidental public-API nullability changes
  generally break CI — the public API analyzer / a build leg will flag them — so mistakes here are
  usually caught, but don't rely on that; defer deliberately.)
- Keep edits minimal and local. Cross-file annotations are allowed but should be small. If enabling
  this file forces broad ripples across other files' public/internal surface, that is a signal to DEFER.
- Follow repo conventions: `_camelCase` private fields, `Contract.ThrowIfNull` only where a null check
  already exists, blank lines must contain no whitespace, no trailing whitespace, no `TODO` (use `TODO2`).

## Feedback loop: use the language server, not full builds
Live diagnostics from the Roslyn language server (`get_errors`) are near-instant and are the primary
feedback mechanism. **Critical:** nullable (CS8xxx) warnings only appear when the file's *active
project context* is a .NET Core TFM (e.g. `net10.0`), NOT `netstandard2.0` (which `NoWarn`s Nullable).
Before relying on diagnostics for a file, set the context once by running the VS Code command
`csharp.changeProjectContext` and selecting the `net10.0` context for the project. Verify by confirming
a known nullable issue surfaces. A full `dotnet build <csproj> -f net10.0` is only needed as an
optional final/CI-level check.

## Procedure

1. **Pick the file.** Run `python3 nullable-migration/pick-next.py`. If it prints nothing, the loop is
   done — stop. Otherwise take the printed `<order>` and `<path>`. Mark it in-progress:
   `python3 nullable-migration/mark-status.py --order <order> --status in-progress`.

2. **Set the active project context to `net10.0`** (once per project) via the `csharp.changeProjectContext`
   command, so the language server surfaces nullable warnings. Project mapping (for the optional build):
   - `src/Compilers/CSharp/Portable/...` → `src/Compilers/CSharp/Portable/Microsoft.CodeAnalysis.CSharp.csproj`
   - `src/Compilers/Core/Portable/...` → `src/Compilers/Core/Portable/Microsoft.CodeAnalysis.csproj`
   - `src/Compilers/VisualBasic/Portable/...` → `src/Compilers/VisualBasic/Portable/Microsoft.CodeAnalysis.VisualBasic.vbproj`

3. **Baseline check.** Confirm the file currently reports no diagnostics via `get_errors`.

4. **Remove the directives.** In the target file, delete the leading `#nullable disable` and every
   interior `#nullable enable` / `#nullable disable` / `#nullable restore` line. Leave the license
   header intact. Do NOT remove qualified directives such as `#nullable disable warnings` or
   `#nullable enable annotations` without understanding them — if present, inspect and treat with care.

5. **Read warnings from the LSP.** Call `get_errors` on the file. The reported CS8### items are your
   work items. (If nothing surfaces after removing a `#nullable disable`, re-check the active project
   context is `net10.0`, not `netstandard2.0`.)

6. **Fix warnings** per the constraints above. In priority order:
   - **Express an existing, untracked invariant with an attribute** (`[MemberNotNullWhen]`,
     `[NotNullWhen]`, etc.) — this is the preferred fix and covers the majority of cases.
   - Add `?` to parameters/fields/locals/returns that can legitimately be null.
   - Add a local null check or `Debug.Assert(x is not null)` where that reflects the real invariant.
   - Only as a last resort, `!` (ideally with a `Debug.Assert`).
   Re-run `get_errors` until there are zero CS8 warnings and zero errors for the file.

   **If a warning reveals a REAL bug** (a genuinely missing null check — null can actually flow to a
   dereference/cast at runtime), do NOT fix the bug here. Semantic changes need higher scrutiny than a
   mechanical migration. Instead:
   - Record it in `nullable-migration/found-bugs.md` (what we know, where null flows, proposed fix).
   - Keep this commit **behavior-preserving**: annotate to the *true* nullability and either **defer**
     the file (preferred if the bug is central), or land it with a clearly-commented, behavior-preserving
     guard (`Debug.Assert`/`!`) carrying a `TODO2` that references the bug entry. Never add or remove a
     runtime null check as part of the migration.
   - The bug fix happens as a separate change with its own review.

7. **Decision:**
   - **Enable (default):** the file builds clean with reasonable, local changes → keep changes.
   - **Defer:** clean enablement requires an API redesign, a **public API nullability change**, a large
     cross-file ripple, **or** a real bug is best fixed first. Then: `git checkout -- <path>` (and any
     other files you touched), and mark deferred with a reason:
     `python3 nullable-migration/mark-status.py --order <order> --status deferred --note "<why; link found-bugs.md entry if applicable>"`.
     Commit nothing for a deferral except the worklist update (step 9). Stop.

8. **Final verification.** `get_errors` on the file must be clean. Optionally run
   `dotnet build <csproj> -f net10.0 -p:RunAnalyzersDuringBuild=false -tl:off` as a heavier confirmation
   (e.g. before pushing). A clean LSP diagnostics result is the required bar per iteration.

9. **Record + commit** (one file per commit):
   - `python3 nullable-migration/mark-status.py --order <order> --status done`
   - `git add <path> nullable-migration/worklist.json` (plus any other files you had to annotate)
   - `git commit -m "Nullable-enable <relative path>"`

10. **Stop.** Exactly one file per iteration.

## Notes
- If the baseline build (step 3) is already broken, do not proceed; mark the item `blocked` with the
  error and stop.
- If you touched other files to add annotations, include them in the same commit and mention them in
  the commit body.
