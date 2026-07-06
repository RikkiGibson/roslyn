# Nullable-enable loop — per-iteration instructions

You will process one item from `nullable-migration/worklist.json`, then stop. You will be re-invoked for the next file.

## Goal

- Nullable enable the file you found in the worklist by deleting the `#nullable` directives in it, and addressing the warnings.
- Semantic changes are not allowed. Changes should almost always be limited to adding `?`, nullability attributes such as `[NotNullWhen(true)]`, `Debug.Assert(obj != null)`, or `!` as a last resort.
- No changes are to be made to test code, or to nullable annotations of public APIs.
- `src/Compilers` projects default to `<Nullable>enable</Nullable>`.
- Individual files in the worklist contain `#nullable disable`/`enable` one or more times, to disable certain parts of the file.

## Use LSP diagnostics daemon to get live diagnostics

Do **not** use the CLI's built-in `get_errors`/`lsp` tool for this. We need special features from the LSP daemon created specifically for this workflow.

**Use `nullable-migration/lsp/lsp_diagnostics.py` instead.**
Pass **every file whose diagnostics you care about in one call**.
Leave the daemon running; do not stop it. It is reused between loop iterations.

You can sanity check that the daemon is working by inserting `string x = null;` into a method body and expecting it to report a warning.

If you suspect the daemon is stuck or a project needs a full reload (e.g. files added/removed), run
  `python3 nullable-migration/lsp/lsp_diagnostics.py --restart <file.cs>`.

Output is one line per diagnostic: `path(line,col): severity CODE: message`. We are mainly interested in `CS8` codes. Analyzer warnings/hints may also be present. Avoid introducing new analyzer diagnostics, but don't do work to resolve pre-existing analyzer diagnostics.

- `CompilerConsumers.slnf` contains the full set of projects we care about with respect to this migration.

## Warning resolution tips

- Resolve warnings on `(object)obj != null` by changing to `(object?)obj != null`.
- Resolve warnings on reassigning pattern variables `if (M() is string value) { value = null; }` by changing to `if (M() is string and var value) ...`, to permit assigning null.
- Look for whether a null check is occurring indirectly. For example, `if (x.HasValue) x.Value.M()`. We can annotate a property with `[MemberNotNullWhen(nameof(Value), true)]` to indicate another property is non-null. It's desirable to resolve nullable warnings using attribuets, allowing more precise nullability facts to flow through the program without semantic changes.
- Mostly prefer `Debug.Assert()` over `!`, except, perhaps in very obvious cases where we would throw right afterwards if the value were null.
- Take note when usage of a specific API is associated with many new nullable warnings. If more than a very small number of API usages require `Assert()`/`!` to fix warnings, then, it is likely preferable to "island" the member. This means putting `#nullable disable/enable` back around that member's signature only. This reflects the fact that we likely want to make an API shape change in the future to ease nullable analysis of the usages of the API.
   - Example of an "islanded" member: `Symbol.ContainingSymbol`. While `Symbol?` is a good type for that member, it results in huge numbers of nullable warnings that we don't want to suppress as part of this work. We prefer to leave the API nullable-disabled and return later when we are ready to make potentially bigger API shape changes to make correct usage possible without `!`.
- If a nullable warning reflects a real bug (something genuinely might be null, where non-null is needed), do not try to fix the bug. Instead record what you know about the problem in found-bugs.md and resolve the warning without semantic changes.
- **`BoundTreeRewriter`/`BoundTreeVisitor`'s `Visit`/`VisitXxx` methods are uniformly typed `BoundNode?`, but this conflates two different contracts**: (1) required (non-optional) children — e.g. `BoundBinaryOperator.Left` — where an override must never actually return null even though the signature allows it, held together only by convention (and the `!` in `Visit`'s `[return: NotNullIfNotNull]` implementation), not by the type system; and (2) genuinely optional children (fields already declared nullable in `BoundNodes.xml`, e.g. `BoundGotoStatement.LabelExpressionOpt`) or list elements (filtered out by `DoVisitList`), where returning null is a legitimate, meaningful deletion. **Do not attempt to narrow `VisitXxx`'s return type to non-nullable `BoundNode`** to resolve warnings here — it breaks the legitimate deletion pattern used by real overrides (e.g. `VisitLabel`, `VisitCatchBlock`). If a file's migration surfaces a call site where this required-vs-optional ambiguity can't be resolved without a real API redesign (e.g. distinguishing "visit a required child" from "visit an optional child" at the type level), treat it like other API-shape problems in the islanding rule below: prefer islanding the specific member/call site, or leaving it nullable-disabled, over forcing `!`/asserts that paper over an ambiguity we can't yet express precisely.
- `!` is mostly justified when it should be obvious to the code reader but not the compiler that the item is non-null. For example, `bool isNotNull = x != null; if (isNotNull) x.M();`.
- Prefer `Debug.Assert(x is not null)` over `!` when the variable is dereferenced at multiple points afterward: the assert narrows the compiler's flow state persistently from that point forward, while `!` only suppresses the warning at that one expression. Don't introduce a fresh `is not null`/`!= null` re-check as a substitute for an existing "obvious to the reader" bool/assert that already established non-nullness (e.g. don't replace `if (hasThing)` with `if (thing is not null)` just to satisfy the compiler) — add a single `Debug.Assert` instead and keep using the plain variable name afterward.

## Workflow

1. **Pick the file.** Run `python3 nullable-migration/loop.py next`. If it prints nothing, the loop is done — stop. It prints the `order`, `path`, `project`, containing `csproj`, and the exact `build` command to use. Mark it in-progress: `python3 nullable-migration/loop.py status --order <order> --status in-progress`.

2. **Identify the containing project** (needed for the build feedback loop):
   - `src/Compilers/CSharp/Portable/...` → `src/Compilers/CSharp/Portable/Microsoft.CodeAnalysis.CSharp.csproj`
   - `src/Compilers/Core/Portable/...` → `src/Compilers/Core/Portable/Microsoft.CodeAnalysis.csproj`
   - `src/Compilers/VisualBasic/Portable/...` → `src/Compilers/VisualBasic/Portable/Microsoft.CodeAnalysis.VisualBasic.vbproj`

3. **Baseline check.** Optionally run the LSP daemon against the file once (command below) to confirm it
   is warning-clean before you start, so any new warnings are attributable to your change. A full build
   baseline is not necessary unless the LSP result looks suspicious.

4. **Remove the directives.** In the target file, delete the leading `#nullable disable` and every
   interior `#nullable enable` / `#nullable disable` / `#nullable restore` line. Delete the newline also to avoid double-blank-line warnings. Leave the license
   header intact. Do NOT remove qualified directives such as `#nullable disable warnings` or
   `#nullable enable annotations` without understanding them — if present, inspect and treat with care.

5. **Check diagnostics.** Run the LSP daemon against the target file (it auto-starts if not already
   running) — include any same-project files you suspect may ripple:
   `python3 nullable-migration/lsp/lsp_diagnostics.py <path> [<other-suspect-paths>...]`
   The reported `CS8###` items are your work items (in the target file **and any same-project
   callers** that now see possibly-null arguments).

6. **Fix warnings** per "Warning resolution tips" above. As you make changes, re-run `lsp_diagnostics.py` on the file (and any rippled callers), until there are zero CS8 warnings.

7. **Decision:**
   - **Enable (default):** the file builds clean with reasonable, local changes → keep changes.
   - **Island (preferred over deferring the whole file):** if only ONE method/region/member can't be
     enabled cleanly, keep the REST of the file enabled and put back **only that member's signature**
     (property/method declaration, or explicit interface impl) in a `#nullable disable` / `#nullable
     enable` pair, so the member's declared type reverts to oblivious. Reasons to island a member:
       - It needs an API redesign to express its real nullability cleanly (the case this guidance is
         about — see the call-site-counting rule in step 6).
       - It needs a real-bug investigation before its nullability can be decided.
       - Prefer this over deferring the whole file, and **prefer it over papering every call site with `!`**

8. **Final verification**. Build `dotnet build CompilerConsumers.slnf -p:RunAnalyzersDuringBuild=false -p:WarningsAsErrors=nullable -p:GenerateFullPaths=true -tl:off`. `CompilerConsumers.slnf` reflects the whole set of projects we care about with respect to this migration. Return to step (6) if needed.

9. **Record + commit** (one file per commit):
   - `python3 nullable-migration/loop.py status --order <order> --status done --note "<short note>"`
   - `git add <path> nullable-migration/worklist.json` (plus any other files you had to annotate)
   - `git commit -m "Nullable-enable <relative path>"`
   - Orchestrator can then cheaply confirm with `python3 nullable-migration/loop.py verify --order <order>`
     (checks working tree clean + terminal status + shows HEAD; no rebuild).

10. **Stop.** Exactly one file per iteration.
