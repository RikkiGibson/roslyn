#!/usr/bin/env python3
"""
Throwaway tool: builds CompilerConsumers.slnx with the ObliviousNullabilityAnalyzer enabled
(see ObliviousNullabilityAnalyzer.cs), extracts its OBL0001/OBL0002 diagnostics, and writes
three reports to artifacts/log/oblivious-nullability/:

  obl0001_by_symbol.txt  - oblivious members READ from nullable-enabled code, most-used first.
  obl0002_by_symbol.txt  - oblivious members/parameters FED a nullable-enabled value, most-used first.
  priority_by_file.txt   - usage counts aggregated by the file where the oblivious member is
                           declared, cross-referenced with git history so genuinely-active files
                           with many blocked usages outrank stale/rarely-touched ones with a
                           similar raw count. score = count / (1 + days_since_last_commit / 365).

Usage:
  python3 generate-report.py                  # rebuild CompilerConsumers.slnx and regenerate all reports
  python3 generate-report.py --log build.log  # reuse an existing build log instead of rebuilding
"""
from __future__ import annotations

import argparse
import re
import subprocess
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_OUT_DIR = REPO_ROOT / "artifacts" / "log" / "oblivious-nullability"

# Matches e.g.:
#   warning OBL0002: Nullable-enabled value flows into oblivious-nullability member 'Foo.Bar(int x)' [declared in src/Foo.cs]
MEMBER_RE = re.compile(r"warning (OBL000[12]):.*? member '([^']+)'(?: \[declared in ([^\]]+)\])?")

STALE_DAYS_THRESHOLD = 365


def run_build(solution: str, log_path: Path) -> None:
    print(f"Building {solution} with analyzers enabled (this can take a minute or two)...")
    with open(log_path, "w") as log_file:
        subprocess.run(
            ["dotnet", "build", solution, "-t:Rebuild", "-p:RunAnalyzersDuringBuild=true", "-tl:off"],
            cwd=REPO_ROOT,
            stdout=log_file,
            stderr=subprocess.STDOUT,
            check=False,  # analyzer warnings never fail the build; inspect the log either way
        )


def dedupe_log_lines(log_path: Path) -> list[str]:
    """MSBuild's console logger streams each diagnostic inline AND reprints the full list again
    after the final 'Build succeeded.'/'Build FAILED.' line - keep only what comes after that
    marker so counts aren't doubled."""
    lines = log_path.read_text(errors="replace").splitlines()
    for i, line in enumerate(lines):
        if line.strip() in ("Build succeeded.", "Build FAILED."):
            return lines[i + 1:]
    return lines


def is_real_source_file(file: str | None) -> bool:
    if not file or file == "?" or not file.startswith("src/"):
        return False
    # Exclude generated/obj output that happens to resolve under a path containing "/src/"
    # earlier in the string (e.g. resource-generated files under artifacts/obj/...).
    return "/obj/" not in file and "/artifacts/" not in file


def extract(lines: list[str]) -> tuple[Counter, Counter, Counter]:
    obl0001: Counter = Counter()
    obl0002: Counter = Counter()
    file_counts: Counter = Counter()
    for line in lines:
        match = MEMBER_RE.search(line)
        if not match:
            continue
        code, member, file = match.group(1), match.group(2), match.group(3)
        (obl0001 if code == "OBL0001" else obl0002)[member] += 1
        if is_real_source_file(file):
            file_counts[file] += 1
    return obl0001, obl0002, file_counts


def write_by_symbol(counter: Counter, path: Path) -> None:
    with open(path, "w") as f:
        for member, count in counter.most_common():
            f.write(f"{count:6d} {member}\n")


def last_commit_info(file: str) -> tuple[str | None, int | None]:
    try:
        out = subprocess.run(
            ["git", "log", "-1", "--format=%cI", "--", file],
            cwd=REPO_ROOT, capture_output=True, text=True, check=True,
        ).stdout.strip()
    except subprocess.CalledProcessError:
        return None, None
    if not out:
        return None, None
    last_commit = datetime.fromisoformat(out)
    days_ago = (datetime.now(timezone.utc) - last_commit).days
    return last_commit.date().isoformat(), days_ago


def write_priority_by_file(file_counts: Counter, path: Path) -> None:
    rows = []
    for file, count in file_counts.items():
        last_commit, days_ago = last_commit_info(file)
        score = count / (1 + days_ago / 365.0) if days_ago is not None else 0.0
        rows.append((score, count, last_commit, days_ago, file))
    rows.sort(key=lambda r: -r[0])

    with open(path, "w") as f:
        f.write("# Prioritized oblivious-usage-count-by-file report.\n")
        f.write("# score = usage_count / (1 + days_since_last_commit/365) -- recently-active files\n")
        f.write("# with many blocked usages rank highest; old/untouched files are discounted even\n")
        f.write(f"# if their raw usage count is high. 'stale' marks files not touched in >{STALE_DAYS_THRESHOLD} days.\n")
        f.write(f"{'score':>8}  {'count':>6}  {'last_commit':>11}  {'days_ago':>8}  {'stale':>5}  file\n")
        for score, count, last_commit, days_ago, file in rows:
            lc = last_commit or "N/A"
            da = str(days_ago) if days_ago is not None else "N/A"
            stale = "yes" if (days_ago is not None and days_ago > STALE_DAYS_THRESHOLD) else ""
            f.write(f"{score:8.1f}  {count:6d}  {lc:>11}  {da:>8}  {stale:>5}  {file}\n")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--solution", default="CompilerConsumers.slnx", help="Solution/slnx to rebuild.")
    parser.add_argument("--out-dir", default=str(DEFAULT_OUT_DIR), help="Directory to write reports to.")
    parser.add_argument("--log", default=None, help="Reuse an existing build log instead of rebuilding.")
    args = parser.parse_args()

    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    log_path = Path(args.log) if args.log else out_dir / "build.log"
    if not args.log:
        run_build(args.solution, log_path)

    lines = dedupe_log_lines(log_path)
    obl0001, obl0002, file_counts = extract(lines)

    write_by_symbol(obl0001, out_dir / "obl0001_by_symbol.txt")
    write_by_symbol(obl0002, out_dir / "obl0002_by_symbol.txt")
    write_priority_by_file(file_counts, out_dir / "priority_by_file.txt")

    print(f"OBL0001: {sum(obl0001.values())} usages, {len(obl0001)} distinct members")
    print(f"OBL0002: {sum(obl0002.values())} usages, {len(obl0002)} distinct members")
    print(f"Wrote reports to {out_dir}")


if __name__ == "__main__":
    main()
