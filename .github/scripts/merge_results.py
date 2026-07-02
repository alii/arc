#!/usr/bin/env python3
"""Merge per-shard test262 results.json files into a single results.json.

Usage: merge_results.py OUT SHARD.json [SHARD.json ...]

Each input is the RESULTS_FILE written by the test runner for one shard
({pass, fail, skip, total, tested, percent}). Counts are summed and the
aggregate recomputed with the runner's own definitions (test262_exec.gleam):
tested = pass + fail, total = tested + skip, and percent is pass/tested
TRUNCATED to two decimals (format_percent), so the merged file is
byte-identical to what one unsharded run would have written.
"""

import json
import sys


def format_percent(passed: int, tested: int) -> str:
    """Mirror format_percent in test/test262_exec.gleam: truncate, 2 decimals."""
    if tested <= 0:
        return "0.00"
    pct_x100 = (passed * 10_000) // tested
    return f"{pct_x100 // 100}.{pct_x100 % 100:02d}"


def main() -> int:
    if len(sys.argv) < 3:
        print("usage: merge_results.py OUT SHARD.json [SHARD.json ...]", file=sys.stderr)
        return 2
    out, shard_files = sys.argv[1], sys.argv[2:]
    totals = {"pass": 0, "fail": 0, "skip": 0}
    for path in shard_files:
        with open(path) as f:
            result = json.load(f)
        for key in totals:
            totals[key] += result[key]
    tested = totals["pass"] + totals["fail"]
    total = tested + totals["skip"]
    percent = format_percent(totals["pass"], tested)
    # Same key order and number formatting as the runner's RESULTS_FILE writer.
    with open(out, "w") as f:
        f.write(
            '{"pass":%d,"fail":%d,"skip":%d,"total":%d,"tested":%d,"percent":%s}'
            % (totals["pass"], totals["fail"], totals["skip"], total, tested, percent)
        )
    print(f"merged {len(shard_files)} shard(s): {totals['pass']}/{tested} = {percent}%")
    return 0


if __name__ == "__main__":
    sys.exit(main())
