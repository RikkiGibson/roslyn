#!/usr/bin/env python3
"""Deprecated shim. Prefer `python3 nullable-migration/loop.py status ...`.

Kept so older references keep working; delegates to loop.py so there is a
single source of truth and a single command prefix to auto-approve.
"""
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import loop  # noqa: E402

if __name__ == "__main__":
    sys.argv = ["loop.py", "status", *sys.argv[1:]]
    sys.exit(loop.main())
