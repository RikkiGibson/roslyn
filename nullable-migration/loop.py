#!/usr/bin/env python3
"""Single entry point for the nullable-enable migration loop.

Having ONE stable command prefix makes it trivial to write a terminal
auto-approve rule: allow `python3 nullable-migration/loop.py` and every
routine loop action (pick next, mark status, verify a commit) is covered,
with no ad-hoc `python3 -c "..."` invocations to reason about.

Subcommands:
  next                    Print the next pending item (order, path, project, csproj, build cmd).
  next --path             Print only the next pending path.
  status --order N --status done [--note "..."]
  status --path P --status deferred [--note "..."]
                          Update a worklist item's status/note.
  verify --order N        Cheap post-subagent check: working tree clean +
                          HEAD commit present + item status is done/deferred.
                          Exits 0 (PASS) or 1 (FAIL). Does NOT build.

Valid statuses: pending, in-progress, done, deferred, blocked
"""
import argparse
import json
import os
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


def _git(*args):
    return subprocess.run(
        ["git", *args], cwd=REPO, capture_output=True, text=True
    )


def cmd_next(args) -> int:
    data = _load()
    pending = [it for it in data["items"] if it["status"] == "pending"]
    if not pending:
        print("no pending items", file=sys.stderr)
        return 1
    nxt = min(pending, key=lambda it: it["order"])
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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)

    p_next = sub.add_parser("next", help="print the next pending item")
    p_next.add_argument("--path", action="store_true", help="print only the path")
    p_next.set_defaults(func=cmd_next)

    p_status = sub.add_parser("status", help="update a worklist item's status/note")
    p_status.add_argument("--path")
    p_status.add_argument("--order", type=int)
    p_status.add_argument("--status", required=True, choices=sorted(VALID))
    p_status.add_argument("--note", default=None)
    p_status.set_defaults(func=cmd_status)

    p_verify = sub.add_parser("verify", help="cheap post-subagent completion check (no build)")
    p_verify.add_argument("--order", type=int, required=True)
    p_verify.set_defaults(func=cmd_verify)

    args = ap.parse_args()
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
