#!/usr/bin/env python3
"""Update the status/note of a worklist item, keyed by path or order.

Usage:
  mark-status.py --path src/Compilers/... --status done
  mark-status.py --order 5 --status deferred --note "needs API redesign of X"

Valid statuses: pending, in-progress, done, deferred, blocked
"""
import argparse
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
WORKLIST = os.path.join(HERE, "worklist.json")
VALID = {"pending", "in-progress", "done", "deferred", "blocked"}


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--path")
    ap.add_argument("--order", type=int)
    ap.add_argument("--status", required=True, choices=sorted(VALID))
    ap.add_argument("--note", default=None)
    args = ap.parse_args()
    if not args.path and args.order is None:
        print("error: provide --path or --order", file=sys.stderr)
        return 2

    with open(WORKLIST, encoding="utf-8") as f:
        data = json.load(f)

    match = None
    for it in data["items"]:
        if (args.path and it["path"] == args.path) or (args.order is not None and it["order"] == args.order):
            match = it
            break
    if match is None:
        print("error: no matching item", file=sys.stderr)
        return 1

    match["status"] = args.status
    if args.note is not None:
        match["note"] = args.note

    with open(WORKLIST, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2)
        f.write("\n")

    print(f"{match['order']}\t{match['path']}\t{match['status']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
