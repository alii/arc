#!/bin/bash
# Time one iteration of each *_run.js on qjs and bun-llint (µs).
# Uses hyperfine for stable numbers; falls back to a wrapper if not present.
set -euo pipefail
cd "$(dirname "$0")"

wrap() {
  # Wrap the run script so it prints elapsed µs (perf.now — Date.now for qjs).
  local base=$1
  cat > /tmp/_tw_$base.cjs <<EOF
var __t0 = (typeof performance!=='undefined'?performance.now():Date.now());
$(cat ${base}_run.cjs)
var __t1 = (typeof performance!=='undefined'?performance.now():Date.now());
console.log("US " + Math.round((__t1-__t0)*1000));
EOF
}

for b in richards deltablue crypto raytrace; do
  wrap $b
  # warm ×1, measure ×5, report min
  qmin=999999999; bmin=999999999
  for i in 1 2 3 4 5 6; do
    q=$(qjs /tmp/_tw_$b.cjs 2>/dev/null | grep '^US' | awk '{print $2}')
    l=$(BUN_JSC_useJIT=0 bun /tmp/_tw_$b.cjs 2>/dev/null | grep '^US' | awk '{print $2}')
    [ $i -eq 1 ] && continue
    [ "$q" -lt "$qmin" ] && qmin=$q
    [ "$l" -lt "$bmin" ] && bmin=$l
  done
  printf "EXT %-10s qjs=%s bun-llint=%s\n" $b $qmin $bmin
done
