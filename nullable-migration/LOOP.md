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

## Feedback loop: build the project (LSP context is unreliable)
The ideal fast path is the Roslyn language server (`get_errors`), but nullable (CS8xxx) warnings only
appear when the file's *active project context* is a .NET Core TFM (e.g. `net10.0`). In practice **that
context does NOT stick** — it keeps reverting to `netstandard2.0`, where Nullable is `NoWarn`'d, so
`get_errors` reports a misleading "clean" result (zero warnings even for a file that still has disabled
regions). Only the user can reset the context, and it reverts again quickly.

**Therefore: use a real build as the feedback mechanism.** Build the containing project for `net10.0`
with analyzers disabled (fast, no analyzer cost) **and nullable warnings elevated to errors** so the
compiler short-circuits the emit/lowering phase as soon as it finds a nullable issue:
```
dotnet build <csproj> -f net10.0 -p:RunAnalyzersDuringBuild=false -p:WarningsAsErrors=nullable -p:GenerateFullPaths=true -tl:off
```
Filter output with `grep -E "error CS|Warning\(s\)|Error\(s\)"`. The `error CS8###` lines are your work
items; iterate until `0 Warning(s)  0 Error(s)`. Because the whole project compiles, this **also
surfaces cross-file ripple** (e.g. a caller in another file that now passes a possibly-null argument) in
the same build — no separate ripple check needed for same-project consumers.

If you *do* try `get_errors` and it shows no CS8xxx after removing a `#nullable disable`, DO NOT trust
it — verify with a throwaway `string x = null;` (must warn CS8600); if it doesn't warn, the context is
wrong and you must build instead.

## Procedure

1. **Pick the file.** Run `python3 nullable-migration/loop.py next`. If it prints nothing, the loop is
   done — stop. It prints the `order`, `path`, `project`, containing `csproj`, and the exact `build`
   command to use. Mark it in-progress:
   `python3 nullable-migration/loop.py status --order <order> --status in-progress`.

2. **Identify the containing project** (needed for the build feedback loop):
   - `src/Compilers/CSharp/Portable/...` → `src/Compilers/CSharp/Portable/Microsoft.CodeAnalysis.CSharp.csproj`
   - `src/Compilers/Core/Portable/...` → `src/Compilers/Core/Portable/Microsoft.CodeAnalysis.csproj`
   - `src/Compilers/VisualBasic/Portable/...` → `src/Compilers/VisualBasic/Portable/Microsoft.CodeAnalysis.VisualBasic.vbproj`

3. **Baseline build.** Optionally build the project once (command below) to confirm it is warning-clean
   before you start, so any new warnings are attributable to your change.

4. **Remove the directives.** In the target file, delete the leading `#nullable disable` and every
   interior `#nullable enable` / `#nullable disable` / `#nullable restore` line. Leave the license
   header intact. Do NOT remove qualified directives such as `#nullable disable warnings` or
   `#nullable enable annotations` without understanding them — if present, inspect and treat with care.

5. **Build to read warnings.** Build the containing project for `net10.0` with nullable-as-errors:
   `dotnet build <csproj> -f net10.0 -p:RunAnalyzersDuringBuild=false -p:WarningsAsErrors=nullable -p:GenerateFullPaths=true -tl:off`
   The reported `error CS8###` items are your work items (in the target file **and any same-project
   callers** that now see possibly-null arguments).

6. **Fix warnings** per the constraints above. In priority order:
   - **Express an existing, untracked invariant with an attribute** (`[MemberNotNullWhen]`,
     `[NotNullWhen]`, etc.) — this is the preferred fix and covers the majority of cases.
   - Add `?` to parameters/fields/locals/returns that can legitimately be null.
   - Add a local null check or `Debug.Assert(x is not null)` where that reflects the real invariant.
   - Only as a last resort, `!` (ideally with a `Debug.Assert`).
   Re-build the project until there are zero CS8 warnings and zero errors for the file.

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
   - **Island (preferred over deferring the whole file):** if only ONE method/region can't be enabled
     cleanly (needs an API redesign, or a real-bug investigation), enable the REST of the file and wrap
     JUST that method/region back in `#nullable disable` / `#nullable enable` with a short `TODO2:`
     comment explaining why (do NOT invent a GitHub issue number). Log it in `found-bugs.md`. Prefer
     this over deferring the whole file, and over papering the method with `!`.
   - **Defer (whole file):** clean enablement requires an API redesign, a **public API nullability
     change**, or a large cross-file ripple that can't be islanded. Then: `git checkout -- <path>` (and
     any other files you touched), and mark deferred with a reason:
     `python3 nullable-migration/loop.py status --order <order> --status deferred --note "<why; link found-bugs.md entry if applicable>"`.
     Commit nothing for a deferral except the worklist update (step 9). Stop.

8. **Final verification.** The containing project must build clean for `net10.0`:
   `dotnet build <csproj> -f net10.0 -p:RunAnalyzersDuringBuild=false -p:WarningsAsErrors=nullable -p:GenerateFullPaths=true -tl:off`
   → require `0 Warning(s)  0 Error(s)`. This already covers same-project cross-file ripple. If your
   change touched an **interface/base member signature** used by *other* projects (incl. the VB
   compiler) or *other* TFMs, also verify with the whole compilers filter:
   `dotnet build -p:RunAnalyzersDuringBuild=false -p:GenerateFullPaths=true -tl:off Compilers.slnf`
   (~3 min; note the filter build does not elevate nullable to errors, so grep `warning CS`).

9. **Record + commit** (one file per commit):
   - `python3 nullable-migration/loop.py status --order <order> --status done --note "<short note>"`
   - `git add <path> nullable-migration/worklist.json` (plus any other files you had to annotate)
   - `git commit -m "Nullable-enable <relative path>"`
   - Orchestrator can then cheaply confirm with `python3 nullable-migration/loop.py verify --order <order>`
     (checks working tree clean + terminal status + shows HEAD; no rebuild).

10. **Stop.** Exactly one file per iteration.

## Notes
- If the baseline build (step 3) is already broken, do not proceed; mark the item `blocked` with the
  error and stop.
- If you touched other files to add annotations, include them in the same commit and mention them in
  the commit body.
