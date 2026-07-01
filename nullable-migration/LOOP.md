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
- Nullable **attributes are allowed and preferred** to express intent without runtime changes:
  `[NotNullWhen]`, `[MaybeNullWhen]`, `[NotNullIfNotNull]`, `[MemberNotNull]`, `[MemberNotNullWhen]`,
  `[DisallowNull]`, `[AllowNull]`, `[return: NotNull]`.
- Use the null-forgiving operator `!` only where an invariant is genuinely known but not expressible.
  Prefer pairing it with a `Debug.Assert(...)` (assert is release-time no-op, so no runtime change).
- Keep edits minimal and local. Cross-file annotations are allowed but should be small. If enabling
  this file forces broad ripples across other files' public/internal surface, that is a signal to DEFER.
- Follow repo conventions: `_camelCase` private fields, `Contract.ThrowIfNull` only where a null check
  already exists, blank lines must contain no whitespace, no trailing whitespace, no `TODO` (use `TODO2`).

## Procedure

1. **Pick the file.** Run `python3 nullable-migration/pick-next.py`. If it prints nothing, the loop is
   done — stop. Otherwise take the printed `<order>` and `<path>`. Mark it in-progress:
   `python3 nullable-migration/mark-status.py --order <order> --status in-progress`.

2. **Identify the project** (for building):
   - `src/Compilers/CSharp/Portable/...` → `src/Compilers/CSharp/Portable/Microsoft.CodeAnalysis.CSharp.csproj`
   - `src/Compilers/Core/Portable/...` → `src/Compilers/Core/Portable/Microsoft.CodeAnalysis.csproj`
   - `src/Compilers/VisualBasic/Portable/...` → `src/Compilers/VisualBasic/Portable/Microsoft.CodeAnalysis.VisualBasic.vbproj`

3. **Baseline build** (must be clean before you start):
   ```
   dotnet build <csproj> -f net10.0 -p:RunAnalyzersDuringBuild=false -tl:off
   ```
   Only the `net10.0` TFM surfaces nullable (CS8xxx) warnings; the `netstandard2.0` leg suppresses them.

4. **Remove the directives.** In the target file, delete the leading `#nullable disable` and every
   interior `#nullable enable` / `#nullable disable` / `#nullable restore` line. Leave the license
   header intact. Do NOT remove qualified directives such as `#nullable disable warnings` or
   `#nullable enable annotations` without understanding them — if present, inspect and treat with care.

5. **Build and read warnings.** Re-run the build from step 3. Collect the `warning CS8###` lines that
   point at the target file. These are your work items.

6. **Fix warnings** per the constraints above. Typical fixes:
   - Add `?` to parameters/fields/locals/returns that can legitimately be null.
   - Annotate methods with the attributes above to preserve existing null-tolerance of callers.
   - Add `Debug.Assert(x is not null)` + `!` where an invariant holds.
   Re-build until there are zero CS8 warnings for the file and zero errors for the project.

7. **Decision:**
   - **Enable (default):** the file builds clean with reasonable, local changes → keep changes.
   - **Defer:** clean enablement requires an API redesign or large cross-file ripple. Then:
     `git checkout -- <path>` (and any other files you touched), and mark deferred with a reason:
     `python3 nullable-migration/mark-status.py --order <order> --status deferred --note "<why>"`.
     Commit nothing for a deferral except the worklist update (step 9). Stop.

8. **Final verification.** `dotnet build <csproj> -f net10.0 -p:RunAnalyzersDuringBuild=false -tl:off`
   must succeed with no new warnings. If the file has meaningful unit-test coverage and time permits,
   the driver may run targeted tests, but a clean build is the required bar.

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
