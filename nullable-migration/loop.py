#!/usr/bin/env python3
"""Single entry point for the nullable-enable migration loop.

Having ONE stable command prefix makes it trivial to write a terminal
auto-approve rule: allow `python3 nullable-migration/loop.py` and every
routine loop action (pick next, mark status, verify a commit) is covered,
with no ad-hoc `python3 -c "..."` invocations to reason about.

Subcommands:
  next                    Print the next pending item (order, path, project, csproj, build cmd).
  next --path             Print only the next pending path.
  list                    List all pending items in base-before-derived processing order,
                          showing declared types and which base file each waits on.
  status --order N --status done [--note "..."]
  status --path P --status deferred [--note "..."]
                          Update a worklist item's status/note.
  verify --order N        Cheap post-subagent check: working tree clean +
                          HEAD commit present + item status is done/deferred.
                          Exits 0 (PASS) or 1 (FAIL). Does NOT build.

Ordering: items are normally taken easiest-first (lowest "order"), BUT a
base-before-derived constraint takes precedence. If a pending file declares a
type that derives from (or implements) a type declared in a DIFFERENT pending
file, the base file is annotated first. Annotating a base member's nullability
ripples into its overrides, so doing the base first avoids re-touching (churning)
the derived files. `next` skips a lower-order derived file until its pending base
files are done, and prints a note on stderr when it does so. Cycles fall back to
plain easiest-first so the loop can't deadlock.

Valid statuses: pending, in-progress, done, deferred, blocked
"""
import argparse
import json
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
WORKLIST = os.path.join(HERE, "worklist.json")
VALID = {"pending", "in-progress", "done", "deferred", "blocked"}

# Short project name (from worklist "project") -> containing csproj/vbproj (repo-relative).
PROJECT_FILE = {
    "Core": "src/Compilers/Core/Portable/Microsoft.CodeAnalysis.csproj",
    "CSharp": "src/Compilers/CSharp/Portable/Microsoft.CodeAnalysis.CSharp.csproj",
    "VisualBasic": "src/Compilers/VisualBasic/Portable/Microsoft.CodeAnalysis.VisualBasic.vbproj",
}


def _load():
    with open(WORKLIST, encoding="utf-8") as f:
        return json.load(f)


def _save(data):
    with open(WORKLIST, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2)
        f.write("\n")


def _find(data, path=None, order=None):
    for it in data["items"]:
        if (path and it["path"] == path) or (order is not None and it["order"] == order):
            return it
    return None


def _csproj(item):
    return PROJECT_FILE.get(item.get("project", ""), "")


# --- base-before-derived ordering -------------------------------------------
# Heuristic type-graph over PENDING worklist files so we annotate a base type
# before any derived type. This is intentionally a lightweight lexical scan, not
# a full parse: a false positive only makes us do a base file slightly earlier
# (which is the goal anyway), and a false negative just falls back to easiest-
# first. It never blocks the loop (cycles fall back to plain order).

_CS_TYPE_DECL = re.compile(
    r"\b(?:class|struct|interface|record(?:\s+struct|\s+class)?)\s+"
    r"([A-Za-z_]\w*)"                       # 1: type name
    r"(?:\s*<[^>{}]*>)?"                    # optional generic params
    r"(?:\s*:\s*(?P<bases>[^{;\n]*))?",     # optional base list up to { ; or EOL
    re.MULTILINE,
)
_VB_TYPE_DECL = re.compile(r"\b(?:Class|Structure|Interface|Module)\s+([A-Za-z_]\w*)", re.IGNORECASE)
_VB_INHERITS = re.compile(r"\bInherits\s+([A-Za-z_][\w.]*)", re.IGNORECASE)


def _simple_name(name):
    """Strip generic args, namespace qualifiers, and trailing '?'."""
    name = name.split("<", 1)[0].strip().rstrip("?").strip()
    if "." in name:
        name = name.rsplit(".", 1)[-1]
    return name


def _read_source(path):
    try:
        with open(os.path.join(REPO, path), encoding="utf-8") as f:
            return f.read()
    except OSError:
        return ""


def _declared_and_bases(path):
    """Return (types declared in this file, base/interface simple-names it uses)."""
    text = _read_source(path)
    declared, bases = set(), set()
    if path.endswith(".vb"):
        for m in _VB_TYPE_DECL.finditer(text):
            declared.add(m.group(1))
        for m in _VB_INHERITS.finditer(text):
            bases.add(_simple_name(m.group(1)))
    else:
        for m in _CS_TYPE_DECL.finditer(text):
            declared.add(m.group(1))
            base_list = m.group("bases")
            if base_list:
                for part in base_list.split(","):
                    if part.strip().startswith("where"):
                        break
                    s = _simple_name(part)
                    if s:
                        bases.add(s)
    bases -= declared  # a base declared in the same file creates no cross-file order
    return declared, bases


def _pending_ready(items):
    """Split pending items into (ready, blocked).

    A pending item is BLOCKED if one of the types it declares derives from a type
    declared in a *different* pending file; that base file must go first.
    Returns (pending, ready, blocked_by) where blocked_by maps path -> set(paths).
    """
    pending = [it for it in items if it["status"] == "pending"]
    info = {it["path"]: _declared_and_bases(it["path"]) for it in pending}
    decl_map = {}  # type name -> set of pending paths declaring it
    for path, (declared, _bases) in info.items():
        for t in declared:
            decl_map.setdefault(t, set()).add(path)
    ready, blocked_by = [], {}
    for it in pending:
        _declared, bases = info[it["path"]]
        blockers = set()
        for b in bases:
            for p in decl_map.get(b, ()):
                if p != it["path"]:
                    blockers.add(p)
        if blockers:
            blocked_by[it["path"]] = blockers
        else:
            ready.append(it)
    return pending, ready, blocked_by


def _git(*args):
    return subprocess.run(
        ["git", *args], cwd=REPO, capture_output=True, text=True
    )


def cmd_next(args) -> int:
    data = _load()
    pending, ready, blocked_by = _pending_ready(data["items"])
    if not pending:
        print("no pending items", file=sys.stderr)
        return 1
    # Prefer base-before-derived: pick the easiest READY item. If nothing is
    # ready (a dependency cycle), fall back to plain easiest-first so we never
    # deadlock.
    pool = ready if ready else pending
    nxt = min(pool, key=lambda it: it["order"])
    strict = min(pending, key=lambda it: it["order"])
    if nxt is not strict:
        blockers = sorted(blocked_by.get(strict["path"], ()))
        print(
            "base-first: skipping order "
            + f"{strict['order']} ({strict['path']}) until its pending base file(s) are done: "
            + ", ".join(blockers),
            file=sys.stderr,
        )
    if args.path:
        print(nxt["path"])
        return 0
    csproj = _csproj(nxt)
    print(f"order:   {nxt['order']}")
    print(f"path:    {nxt['path']}")
    print(f"project: {nxt.get('project', '')}")
    print(f"csproj:  {csproj}")
    print(
        "build:   dotnet build "
        + csproj
        + " -f net10.0 -p:RunAnalyzersDuringBuild=false -p:WarningsAsErrors=nullable"
        + ' -p:GenerateFullPaths=true -tl:off 2>&1 | grep -E "error CS|Warning\\(s\\)|Error\\(s\\)"'
    )
    return 0


def _processing_order(items):
    """Simulate the base-before-derived pick order over all PENDING items.

    Repeatedly takes the lowest-order READY item (no pending base file still
    ahead of it), mirroring what `next` does turn by turn. Returns a list of
    (item, blockers_at_pick_time) in the order the loop would process them.
    Cycles fall back to plain easiest-first so this always terminates.
    """
    remaining = [it for it in items if it["status"] == "pending"]
    info = {it["path"]: _declared_and_bases(it["path"]) for it in remaining}
    ordered = []
    done_paths = set()
    while remaining:
        decl_map = {}
        for it in remaining:
            for t in info[it["path"]][0]:
                decl_map.setdefault(t, set()).add(it["path"])
        ready, blocked_by = [], {}
        for it in remaining:
            _declared, bases = info[it["path"]]
            blockers = set()
            for b in bases:
                for p in decl_map.get(b, ()):
                    if p != it["path"]:
                        blockers.add(p)
            if blockers:
                blocked_by[it["path"]] = blockers
            else:
                ready.append(it)
        pool = ready if ready else remaining
        nxt = min(pool, key=lambda it: it["order"])
        ordered.append((nxt, sorted(blocked_by.get(nxt["path"], ()))))
        remaining.remove(nxt)
        done_paths.add(nxt["path"])
    return ordered


def cmd_list(args) -> int:
    data = _load()
    counts = {}
    for it in data["items"]:
        counts[it["status"]] = counts.get(it["status"], 0) + 1
    summary = ", ".join(f"{k}={counts[k]}" for k in sorted(counts))
    print(f"# {data.get('count', len(data['items']))} items: {summary}")
    print("# pending items in base-before-derived processing order:")
    ordered = _processing_order(data["items"])
    for pos, (it, blockers) in enumerate(ordered, start=1):
        declared, bases = _declared_and_bases(it["path"])
        types = ",".join(sorted(declared)) if declared else "-"
        line = f"{pos:>3}. order={it['order']:<4} {it['path']}"
        print(line)
        extra = f"       declares: {types}"
        if blockers:
            extra += f"  | base-first after: {', '.join(os.path.basename(b) for b in blockers)}"
        print(extra)
    return 0


def cmd_status(args) -> int:
    if not args.path and args.order is None:
        print("error: provide --path or --order", file=sys.stderr)
        return 2
    data = _load()
    item = _find(data, path=args.path, order=args.order)
    if item is None:
        print("error: no matching item", file=sys.stderr)
        return 1
    item["status"] = args.status
    if args.note is not None:
        item["note"] = args.note
    _save(data)
    print(f"{item['order']}\t{item['path']}\t{item['status']}")
    return 0


def cmd_verify(args) -> int:
    """Cheap post-subagent verification. No build."""
    data = _load()
    item = _find(data, order=args.order)
    if item is None:
        print(f"FAIL: no worklist item with order {args.order}", file=sys.stderr)
        return 1

    ok = True

    # 1. Working tree must be clean (subagent must have committed everything).
    porcelain = _git("status", "--porcelain").stdout.strip()
    if porcelain:
        ok = False
        print("FAIL: working tree is dirty (subagent didn't finish cleanly):")
        print(porcelain)
    else:
        print("PASS: working tree clean")

    # 2. Worklist status must be a terminal state.
    if item["status"] in ("done", "deferred"):
        print(f"PASS: worklist status = {item['status']}")
    else:
        ok = False
        print(f"FAIL: worklist status = {item['status']} (expected done/deferred)")

    # 3. Show HEAD so the caller can eyeball the commit.
    head = _git("log", "-1", "--format=%h %s").stdout.strip()
    print(f"HEAD:  {head}")
    print(f"note:  {item.get('note', '')}")

    return 0 if ok else 1


def cmd_resort(args) -> int:
    """Persist base-before-derived ordering into worklist.json's "order" field.

    `next`/`list` already compute a base-before-derived pick order on the fly
    (see `_processing_order`), but that's a runtime overlay: the "order" field
    on disk still reflects plain easiest-first, so anyone skimming
    worklist.json/worklist.md sees a misleading sequence, and any other tooling
    that just sorts by "order" would miss the constraint.

    This reassigns "order" for all PENDING items to match that computed
    sequence, one-to-one, reusing the exact same set of order numbers already
    held by pending items (just permuted among them) -- so done/deferred/
    blocked items keep their original order values untouched and no numbers
    collide. This is a heuristic lexical pass (see `_declared_and_bases`), not
    a perfect dependency sort -- it's meant to reduce re-touching derived files
    after a later base-type edit, not guarantee zero churn.
    """
    data = _load()
    pending = [it for it in data["items"] if it["status"] == "pending"]
    if not pending:
        print("no pending items; nothing to resort", file=sys.stderr)
        return 0

    ordered = _processing_order(data["items"])  # [(item, blockers), ...] in pick order
    available_orders = sorted(it["order"] for it in pending)

    changes = []
    for new_order, (item, blockers) in zip(available_orders, ordered):
        if item["order"] != new_order:
            changes.append((item["path"], item["order"], new_order, blockers))
        item["order"] = new_order

    if args.dry_run:
        print(f"[dry run] {len(changes)} of {len(pending)} pending item(s) would move:")
    else:
        _save(data)
        print(f"{len(changes)} of {len(pending)} pending item(s) moved:")

    for path, old_order, new_order, blockers in changes:
        arrow = f"{old_order:>4} -> {new_order:<4}"
        extra = f"  (base-first after: {', '.join(os.path.basename(b) for b in blockers)})" if blockers else ""
        print(f"  {arrow} {path}{extra}")

    return 0


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)

    p_next = sub.add_parser("next", help="print the next pending item")
    p_next.add_argument("--path", action="store_true", help="print only the path")
    p_next.set_defaults(func=cmd_next)

    p_list = sub.add_parser("list", help="list pending items in base-before-derived processing order")
    p_list.set_defaults(func=cmd_list)

    p_status = sub.add_parser("status", help="update a worklist item's status/note")
    p_status.add_argument("--path")
    p_status.add_argument("--order", type=int)
    p_status.add_argument("--status", required=True, choices=sorted(VALID))
    p_status.add_argument("--note", default=None)
    p_status.set_defaults(func=cmd_status)

    p_verify = sub.add_parser("verify", help="cheap post-subagent completion check (no build)")
    p_verify.add_argument("--order", type=int, required=True)
    p_verify.set_defaults(func=cmd_verify)

    p_resort = sub.add_parser(
        "resort",
        help="persist base-before-derived ordering into worklist.json's order field for pending items",
    )
    p_resort.add_argument("--dry-run", action="store_true", help="show what would change without saving")
    p_resort.set_defaults(func=cmd_resort)

    args = ap.parse_args()
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
