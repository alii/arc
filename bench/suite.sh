#!/bin/sh
# usage: bench/suite.sh [ebin_root] > out.txt ; columns: name reds ms high peak
# reds and ms from measure.escript, high and peak from memory.escript
root=${1:-build/dev/erlang}
runs=${RUNS:-3}
for f in bench/v8-v7/richards_run.cjs bench/v8-v7/deltablue_run.cjs \
         bench/v8-v7/crypto_run.cjs bench/v8-v7/raytrace_run.cjs bench/micro/*.js; do
  n=$(basename $f .js); n=$(basename $n _run.cjs)
  m=$(escript bench/measure.escript $f $runs $root 2>/dev/null | grep REDS)
  g=$(escript bench/memory.escript $f $root 2>/dev/null | grep ALLOC)
  reds=$(echo $m | awk '{print $2}'); ms=$(echo $m | awk '{print $6}')
  high=$(echo $g | awk '{print $6}'); peak=$(echo $g | awk '{print $4}'); alloc=$(echo $g | awk '{print $2}')
  printf "%-18s %12s %8s %10s %10s %12s\n" $n $reds $ms $high $peak $alloc
done
