NOTE: this whole file is outdated. Refer to `LOOP.md` instead.

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
- **Legacy `(object)x != null` idiom** (a reference-equality null check that bypasses user-defined
  `==`/`!=`): when it trips CS8600, change the cast to nullable — `(object?)x != null` /
  `(object?)x == null`. Do **not** rewrite it to `x is not null` / `x is null`; the `(object?)` form is
  the least-invasive edit and preserves the operator-bypass intent.
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

## Feedback loop: the LSP diagnostics daemon (fast path; no build needed)
Do **not** use the CLI's built-in `get_errors`/`lsp` tool for this. It has no reliable way to pin a
multi-targeted project's "active" TFM context, and in practice it silently reverts to `netstandard2.0`
(where Nullable is `NoWarn`'d), reporting a misleading "clean" result (zero warnings even for a file
that still has disabled regions).

**Use `nullable-migration/lsp/lsp_diagnostics.py` instead.** It talks to a real `roslyn-language-server`
directly over LSP and explicitly pins every diagnostics request to the `net10.0` project context (via
the `_vs_projectContext` VS LSP extension), bypassing the "active context" heuristic entirely. It keeps
the server loaded as a background daemon scoped to `CompilerConsumers.slnf` (not the full `Roslyn.slnx`),
so it is much cheaper to start than a build, and near-instant on repeat calls against already-open files:
```
python3 nullable-migration/lsp/lsp_diagnostics.py <file1.cs> [<file2.cs> ...]
```
- **Starts the daemon automatically on first use** — you do not need to start it yourself. It stays
  running in the background across files/iterations; leave it running (do not stop it after each file).
  If you suspect it's stuck or a project needs a full reload (e.g. files added/removed), run
  `python3 nullable-migration/lsp/lsp_diagnostics.py --restart <file.cs>`.
- Pass **every file whose diagnostics you care about in one call** (the target file plus any same-project
  callers you suspect may ripple) — this surfaces cross-file impact just like a build would, without a
  build.
- Output is one line per diagnostic: `path(line,col): severity CODE: message`. Filter for `CS8` codes;
  ignore IDE0xxx/CAxxxx analyzer hints (this tool doesn't disable analyzers, unlike the build command).
- If you inject a throwaway `string x = null;` and it does NOT report CS8600 for that file, something is
  wrong (e.g. daemon didn't reload after an edit, or file isn't part of `CompilerConsumers.slnf`) — fall
  back to `--restart`, and if that doesn't help, fall back to the build command below.
- A full `dotnet build` is only needed once per file, as the **final verification** in step 8 (and
  optionally as a **baseline** in step 3) — the LSP daemon is for the fast, iterative edit/check cycle
  in between.

## Procedure

1. **Pick the file.** Run `python3 nullable-migration/loop.py next`. If it prints nothing, the loop is
   done — stop. It prints the `order`, `path`, `project`, containing `csproj`, and the exact `build`
   command to use. Mark it in-progress:
   `python3 nullable-migration/loop.py status --order <order> --status in-progress`.
   - **Base before derived.** Items are normally easiest-first, but `next` enforces a
     base-before-derived constraint: if the lowest-order pending file declares a type that derives
     from (or implements) a type declared in a *different* still-pending file, `next` skips it and
     hands you the base file first (it prints a `base-first: skipping ...` note on stderr explaining
     which base it is waiting on). Annotating a base member's nullability ripples into every override,
     so doing the base first avoids re-touching (churning) the derived files. Just take whatever `next`
     gives you — the ordering is handled for you.

2. **Identify the containing project** (needed for the build feedback loop):
   - `src/Compilers/CSharp/Portable/...` → `src/Compilers/CSharp/Portable/Microsoft.CodeAnalysis.CSharp.csproj`
   - `src/Compilers/Core/Portable/...` → `src/Compilers/Core/Portable/Microsoft.CodeAnalysis.csproj`
   - `src/Compilers/VisualBasic/Portable/...` → `src/Compilers/VisualBasic/Portable/Microsoft.CodeAnalysis.VisualBasic.vbproj`

3. **Baseline check.** Optionally run the LSP daemon against the file once (command below) to confirm it
   is warning-clean before you start, so any new warnings are attributable to your change. A full build
   baseline is not necessary unless the LSP result looks suspicious.

4. **Remove the directives.** In the target file, delete the leading `#nullable disable` and every
   interior `#nullable enable` / `#nullable disable` / `#nullable restore` line. Leave the license
   header intact. Do NOT remove qualified directives such as `#nullable disable warnings` or
   `#nullable enable annotations` without understanding them — if present, inspect and treat with care.

5. **Check diagnostics.** Run the LSP daemon against the target file (it auto-starts if not already
   running) — include any same-project files you suspect may ripple:
   `python3 nullable-migration/lsp/lsp_diagnostics.py <path> [<other-suspect-paths>...]`
   The reported `CS8###` items are your work items (in the target file **and any same-project
   callers** that now see possibly-null arguments).

6. **Fix warnings** per the constraints above. In priority order:
   - **Express an existing, untracked invariant with an attribute** (`[MemberNotNullWhen]`,
     `[NotNullWhen]`, etc.) — this is the preferred fix and covers the majority of cases.
   - Add `?` to parameters/fields/locals/returns that can legitimately be null.
   - Add a local null check or `Debug.Assert(x is not null)` where that reflects the real invariant.
   - Only as a last resort, `!` (ideally with a `Debug.Assert`).
   Re-run `lsp_diagnostics.py` on the file (and any rippled callers) until there are zero CS8 warnings.

   **Reassigning an `is`-pattern variable to a possibly-null value:** don't introduce a fresh local
   just to work around `CS8600` on `if (expr is T x) { ... x = x.SomeNullableMember; ... }` — a plain
   type pattern `is T x` always narrows `x`'s *declared* type to non-null `T`, which is why reassigning
   a nullable value into it warns. Instead combine it with a `var` pattern: `if (expr is T and var x)`.
   The `var` pattern designates `x` with the (nullable-preserving) type of `expr` itself narrowed by the
   preceding `T` check, i.e. effectively `T?` when `expr`'s type permits null — so `x` can be freely
   reassigned to a nullable value with no warning, and flow analysis still treats `x` as non-null
   immediately after the check. This does **not** apply when the value being reassigned is a plain
   method/local-function **parameter** rather than an `is`-pattern variable (parameters can't be
   redeclared) — in that case a fresh nullable local for the loop/walk is the correct fix.

   **Before patching a member's ripple, count its call sites — don't just silence them one by one.**
   After annotating a member with `?`, first gather *all* the `CS8###` sites that member alone causes
   (grep the diagnostics output by member name) — don't fix them yet. If clearing them would require
   `!`/`Debug.Assert` at **several distinct, unrelated call sites** (different files/methods, not one
   localized spot), STOP: that repetition is the signal that the member's declared nullability doesn't
   match how callers actually use it, and patching every site with `!` only hides the mismatch instead
   of fixing it. This is an **Island** decision (step 7), made from the diagnostic *list* alone, before
   you've written any fix — do not iterate call-site-by-call-site hoping it converges.

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
   - **Island (preferred over deferring the whole file):** if only ONE method/region/member can't be
     enabled cleanly, keep the REST of the file enabled and put back **only that member's signature**
     (property/method declaration, or explicit interface impl) in a `#nullable disable` / `#nullable
     enable` pair, so the member's declared type reverts to oblivious. Reasons to island a member:
       - It needs an API redesign to express its real nullability cleanly (the case this guidance is
         about — see the call-site-counting rule in step 6).
       - It needs a real-bug investigation before its nullability can be decided.
     **What islanding means, concretely:**
       - Wrap **only the member's own declaration line(s)** in `#nullable disable` / `#nullable enable`
         — do NOT add `?` to it, and do NOT touch any call sites at all (no `!`, no `Debug.Assert`, no
         casts added anywhere else). If you already added `!`s at call sites while investigating, revert
         them (`git diff`/`git checkout -p`) once you decide to island — the whole point is that call
         sites stay exactly as they were before your change.
       - Add a short comment directly above the island explaining why, with a `TODO2:` reference (do NOT
         invent a GitHub issue number), and log a matching entry in `found-bugs.md` describing the
         desired end-state API shape.
     Prefer this over deferring the whole file, and **prefer it over papering every call site with `!`**
     — if you find yourself adding more than one or two `!`s for the same member, that's a sign you
     should have islanded instead; undo them and island.
   - **Defer (whole file):** clean enablement requires an API redesign, a **public API nullability
     change**, or a large cross-file ripple that can't be islanded. Then: `git checkout -- <path>` (and
     any other files you touched), and mark deferred with a reason:
     `python3 nullable-migration/loop.py status --order <order> --status deferred --note "<why; link found-bugs.md entry if applicable>"`.
     Commit nothing for a deferral except the worklist update (step 9). Stop.

8. **Final verification.** The containing project must build clean for `net10.0`:
   `dotnet build <csproj> -f net10.0 -p:RunAnalyzersDuringBuild=false -p:WarningsAsErrors=nullable -p:GenerateFullPaths=true -tl:off`
   → require `0 Warning(s)  0 Error(s)`. This already covers same-project cross-file ripple. If your
   change touched an **interface/base member signature** used by *other* projects (incl. the VB
   compiler) — e.g. a change under `src/Compilers/Core/Portable/...` that alters an `abstract` /
   `virtual` / interface member's nullability — also verify with the ripple filter
   `CompilerConsumers.slnf` (the non-test set of projects with `InternalsVisibleTo` access to
   `Microsoft.CodeAnalysis` / `Microsoft.CodeAnalysis.CSharp`). Do **not** use `Compilers.slnf` (it
   pulls in test projects and is slower); use:
   `dotnet build CompilerConsumers.slnf -p:RunAnalyzersDuringBuild=false -p:WarningsAsErrors=nullable -p:GenerateFullPaths=true -tl:off`
   Because `-p:WarningsAsErrors=nullable` propagates to every project built, this elevates nullable to
   errors across all the consumers in one build; grep `-E "error CS|Warning\(s\)|Error\(s\)"` and require
   `0 Error(s)`. (VB projects never emit CS8xxx, so they can't break from nullable annotation changes.)

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
