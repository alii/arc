-module(arc_gc_oracle_ffi).
-export([check/1, fire/1, reset/0, stats/0, self_mem/0, now_us/0, keep/1]).

%% Process-dict counters read by test/arc_gc_oracle_probe.gleam.
%% `check` is called on every entry to maybe_collect_at_toplevel;
%% `fire` on the True arm only.

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

%% Force full GC on self, then report bytes. Live locals in the caller
%% (e.g. the returned Engine) survive this GC and are counted.
self_mem() ->
    erlang:garbage_collect(),
    {memory, M} = process_info(self(), memory),
    M.

now_us() -> erlang:monotonic_time(microsecond).

%% Identity — call after self_mem to defeat the compiler's last-use
%% liveness so the engine is definitely counted in self_mem's number.
keep(X) -> X.
