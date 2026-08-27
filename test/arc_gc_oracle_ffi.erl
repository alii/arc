-module(arc_gc_oracle_ffi).
-export([check/1, fire/1, reset/0, stats/0, self_mem/0, now_us/0, keep/1]).

gv(K, D) -> case get(K) of undefined -> D; V -> V end.

check(Grown) ->
    put(arc_gc_checks, gv(arc_gc_checks, 0) + 1),
    put(arc_gc_max_grown, max(gv(arc_gc_max_grown, 0), Grown)),
    nil.

fire(_Grown) ->
    put(arc_gc_fires, gv(arc_gc_fires, 0) + 1),
    nil.

reset() ->
    erase(arc_gc_checks),
    erase(arc_gc_fires),
    erase(arc_gc_max_grown),
    nil.

stats() ->
    {gv(arc_gc_checks, 0), gv(arc_gc_fires, 0), gv(arc_gc_max_grown, 0)}.

self_mem() ->
    erlang:garbage_collect(),
    {memory, M} = process_info(self(), memory),
    M.

now_us() -> erlang:monotonic_time(microsecond).

%% defeats last-use liveness so self_mem counts it
keep(X) -> X.
