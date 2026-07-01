#!/usr/bin/env python3
"""Print the next pending item from worklist.json (lowest 'order').

Usage:
  pick-next.py           # prints "<order>\t<path>" or nothing if none pending
  pick-next.py --path    # prints only the path
"""
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
WORKLIST = os.path.join(HERE, "worklist.json")


def main() -> int:
    with open(WORKLIST, encoding="utf-8") as f:
        data = json.load(f)
    pending = [it for it in data["items"] if it["status"] == "pending"]
    if not pending:
        return 1
    nxt = min(pending, key=lambda it: it["order"])
    if "--path" in sys.argv:
        print(nxt["path"])
    else:
        print(f"{nxt['order']}\t{nxt['path']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
