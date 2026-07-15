# nullable migration process

Note: during this process we temporarily pare-down the projects listed in `CompilerConsumers.slnf`, to only have net10.0 or similar TFM. The human will restore this to the original configuration afterwards.

## 1: Replace directives

Replace all `#nullable disable`/`enable` directives under the indicated folder with:  
`#pragma warning disable CS8600 // Converting null literal or possible null value to non-nullable type.`

Do not perform this step in files containing public APIs; such files can be ignored.

If the pragma is redundant, then, you can just delete the directive and not replace it with anything.

Note: the user may have already done this step when you are invoked.

## 2: Build

Remember this command to build: `dotnet build CompilerConsumers.slnf`.  
You may want to redirect output to file for ease of reference.

Avoid using `dotnet build` except as a final backstop after this point.

Instead, *before and after* you change a member, do the following:
- use `vscode_listCodeUsages` tool to find all usages of the member.
- for each file containing a usage, use `get_errors` tool to get errors in the file.

This makes it easy and fast to gauge what effect your change actually had on the diagnostics.

## 3: Inspect members associated with warnings

When a nullable warning is reported on usage of a member, it's a sign that something involved may need to be nullable-annotated.

Go through the warnings from the build 1-by-1. We start making successive passes, starting with the easiest-to-resolve warnings, and ending with the hardest-to-resolve. In the first pass almost any complication observed from adjusting the annotation can be handled by skipping the warning.

No special note needs to be taken when skipping a warning, you can just skip it.

After a whole pass is done we will `dotnet build` again, and the warnings we skipped last time will show up again for us to revisit in next pass.

Use the tools mentioned in (2) to do the following:
- Look for other existing warnings related to usage of the member.
- Look for evidence of whether the original code authors intended to permit null values or not.
- When you make a change, look for ripple effects.

Desirable changes include:
- Adding `?`.
- Adding `[NotNullWhen(true)]`, `[MaybeNullWhen(false)]`, `[NotNullIfNotNull]`, etc. attributes, to let compiler get the right answer without need for suppressions.

Less desirable, but acceptable, changes include:
- Adding `Debug.Assert()`, when original code clearly expects non-null in a certain path but compiler can't track it.
- Adding `!`. This is mainly acceptable in untracked-yet-clear cases like `bool isNotNull = x != null; if (isNotNull) x.M();`. It's OK to use `x!.M()` there.

Undesirable changes include:
- Style changes. Avoid changing `(object)x != null` to `x is not null` or any other forms of modernization, despite semantic equivalence.

Once a pass over the build warnings is finished, we can do a new build to get a full set of warnings, and make a new pass looking for the next easiest items.
