# Nullable migration for `src/Compilers`

A work queue + agent loop ("ralph wiggum loop") to remove the sprinkled `#nullable disable`/`enable`
toggles from compiler source files and let them inherit the project default `<Nullable>enable</Nullable>`.

## Why these files
The compiler projects already set `<Nullable>enable</Nullable>` (see `eng/targets/Settings.props`).
146 non-test files under `src/Compilers` still opt out with a top `#nullable disable` and re-enable
regions piecemeal. This effort finishes that job, one file at a time.

## Files here
- `worklist.json` — source of truth. One entry per file with `order`, difficulty metrics
  (`disabledLines`, `toggles`, `totalLines`), a `status`, and a free-text `note`.
- `worklist.md` — human-readable rendering of the queue (regenerate if you want; JSON is authoritative).
- `LOOP.md` — the exact per-iteration instructions the agent follows.
- `loop.py` — the single entry point for routine loop actions. Subcommands:
  - `loop.py next` — print the next `pending` file (order, path, project, csproj, build command).
    Normally easiest-first, but enforces a **base-before-derived** constraint (see Ordering below).
  - `loop.py status --order <n> --status <s> [--note "..."]` — update an item's `status`/`note`.
  - `loop.py verify --order <n>` — cheap post-subagent check (working tree clean + terminal status +
    show HEAD; no rebuild).
  Using one command prefix (`python3 nullable-migration/loop.py`) means a single terminal auto-approve
  rule covers the whole loop.
- `pick-next.py` / `mark-status.py` — deprecated thin shims that delegate to `loop.py`.
- `found-bugs.md` — log of real nullability bugs uncovered during the migration (fixed separately).

Statuses: `pending` → `in-progress` → `done` | `deferred` | `blocked`.

## Strategy: one file at a time (not all at once)
We deliberately enable **one file per commit** rather than flipping everything and grinding warnings
globally. Rationale:
- Bounded, reviewable diffs; each commit is independently revertable.
- Warnings are isolated to the file just enabled (other files stay `#nullable disable`, so their
  warnings don't appear), which makes cause/effect obvious.
- Easy to `defer` an awkward file without blocking the rest.
- A global flip produces thousands of simultaneous, interdependent warnings that are hard to attribute.

Ordering is **easiest-first** (fewest currently-disabled lines) to build momentum, with one override:
a **base-before-derived** constraint. `next` will not hand you a file whose declared type derives from
(or implements) a type declared in another *still-pending* file — it gives you the base file first and
prints a `base-first: skipping ...` note explaining what it is waiting on. Annotating a base member's
nullability ripples into every override, so doing the base first avoids re-touching (churning) the
derived files when the base is later annotated. The check is a lightweight lexical scan over pending
files and falls back to plain easiest-first if it detects a cycle, so it can never deadlock the loop.
If you additionally want to minimize churn, note that the hardest low-level types
(`Symbol`, `TypeSymbol`, `NamedTypeSymbol`, `MethodSymbol`, ...) are the roots of most hierarchies, so
expect the constraint to pull them earlier than their raw line-count would suggest.

## Resolutions
Each file ends in one of:
- **done** — directives removed, warnings resolved with local, semantics-preserving changes. May keep a
  single `#nullable disable` **island** around one method that needs an API redesign or a real-bug fix
  (with a `TODO2:` comment + a `found-bugs.md` entry) — preferred over deferring the whole file.
- **deferred** — clean enablement needs an API redesign or a large cross-file ripple that can't be
  islanded. Revert the file and record why in `note`. Revisit later.
- **blocked** — couldn't even baseline-build; record the error.

No runtime semantic changes are permitted. When a warning stems from a null check that already exists
but the compiler can't see (a guard property/method, a preceding assert, a `TryGet` pattern), **teach
the compiler about it with an attribute** rather than silencing it: `[MemberNotNull]` /
`[MemberNotNullWhen]`, `[NotNullWhen]` / `[MaybeNullWhen]`, `[NotNullIfNotNull]`, `[DisallowNull]`,
`[AllowNull]`. The null-forgiving operator `!` is a last resort, not the default tool.

The migration will sometimes uncover a **real** nullability bug (a genuinely missing null check). Do
not fix it inline — record it in `found-bugs.md`, keep the migration commit behavior-preserving
(defer the file or land a documented, behavior-preserving guard), and fix the bug in a separate change
so the semantic change gets its own review.

**Public APIs are out of scope.** Do not change the nullability of public surface (e.g. `ISymbol`, or
anything tracked in `PublicAPI.Shipped.txt` / `PublicAPI.Unshipped.txt`). If a warning can only be
resolved by re-annotating a public member, defer the file (note: "requires public API nullability
change"). Accidental public-API changes generally break CI, but defer deliberately rather than relying
on that.

## Running the loop
Drive it however you invoke your agent. The minimal shell driver:

```bash
while path=$(python3 nullable-migration/loop.py next --path); do
  # invoke your coding agent with nullable-migration/LOOP.md as the prompt.
  # the agent processes exactly one file ($path), commits, and updates worklist.json.
  <your-agent-cli> --prompt-file nullable-migration/LOOP.md || break
done
```

Check progress any time:
```bash
python3 - <<'PY'
import json,collections
d=json.load(open("nullable-migration/worklist.json"))
c=collections.Counter(i["status"] for i in d["items"])
print(c, "of", d["count"])
PY
```

## Feedback: language server, not builds
Use live Roslyn language-server diagnostics (`get_errors`) for near-instant feedback instead of full
builds. The catch: nullable (CS8xxx) warnings only surface when the file's **active project context**
is a .NET Core TFM (`net10.0`), not `netstandard2.0` (which `NoWarn`s Nullable via `DisableNullableWarnings`).
Set the context once per project with the VS Code command `csharp.changeProjectContext` → pick the
`net10.0` context. A full `dotnet build <csproj> -f net10.0 -p:RunAnalyzersDuringBuild=false -tl:off`
is only needed as an optional heavier confirmation.
