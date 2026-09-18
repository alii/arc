#!/bin/sh
# usage: bench/compare.sh base.txt new.txt ; ratios new/base per column
sort $1 > $1.s; sort $2 > $2.s; join $1.s $2.s | awk 'BEGIN{printf "%-18s %8s %8s %8s %8s %8s\n","name","reds","ms","high","peak","alloc"} {printf "%-18s %8.3f %8.3f %8.3f %8.3f %8.3f\n",$1,$7/$2,$8/$3,$9/$4,$10/$5,$11/$6}'
