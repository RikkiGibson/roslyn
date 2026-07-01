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
- `pick-next.py` — prints the next `pending` file (lowest `order`).
- `mark-status.py` — safely updates an item's `status`/`note`.

Statuses: `pending` → `in-progress` → `done` | `deferred` | `blocked`.

## Strategy: one file at a time (not all at once)
We deliberately enable **one file per commit** rather than flipping everything and grinding warnings
globally. Rationale:
- Bounded, reviewable diffs; each commit is independently revertable.
- Warnings are isolated to the file just enabled (other files stay `#nullable disable`, so their
  warnings don't appear), which makes cause/effect obvious.
- Easy to `defer` an awkward file without blocking the rest.
- A global flip produces thousands of simultaneous, interdependent warnings that are hard to attribute.

Ordering is **easiest-first** (fewest currently-disabled lines) to build momentum. If you prefer to
minimize cross-file annotation churn instead, process low-level types first
(`Symbol`, `TypeSymbol`, `NamedTypeSymbol`, `MethodSymbol`, ...) so higher-level files see correct
annotations — but those are the hardest, so expect slower early progress.

## Resolutions
Each file ends in one of:
- **done** — directives removed, warnings resolved with local, semantics-preserving changes.
- **deferred** — clean enablement needs an API redesign or a large cross-file ripple. Revert the file
  and record why in `note`. Revisit later.
- **blocked** — couldn't even baseline-build; record the error.

No runtime semantic changes are permitted. Nullable **attributes** (`[NotNullWhen]`, `[MaybeNullWhen]`,
`[MemberNotNull]`, `[NotNullIfNotNull]`, `[DisallowNull]`, `[AllowNull]`, ...) are the preferred tool
for preserving behavior while satisfying the analyzer.

## Running the loop
Drive it however you invoke your agent. The minimal shell driver:

```bash
while path=$(python3 nullable-migration/pick-next.py --path); do
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

## Build note
Nullable (CS8xxx) warnings only surface on the .NET Core TFM (`net10.0`); the `netstandard2.0` leg
suppresses them via `DisableNullableWarnings`. Always verify with:
```
dotnet build <project.csproj> -f net10.0 -p:RunAnalyzersDuringBuild=false -tl:off
```
