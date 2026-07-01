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
_None yet._
