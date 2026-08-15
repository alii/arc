%%% arc_rt_gc_ffi — the deep BEAM-term walk for `rt_gc`
%%% (SPEC §7.M2; M2.md:177-196).
%%%
%%% Hand-written Erlang, so it carries the `arc_rt_` namespace prefix
%%% (overview §5) and can NEVER collide with an OTP module — exactly like
%%% `arc_rt_store_ffi`. Pure term walk: no NIF, no process state,
%%% cannot crash the node.
%%%
%%% Why a shim: `refs_in_term/2` recurses into a fun's captured environment
%%% via `erlang:fun_info(F, env)` — the load-bearing case (M2-I8) that keeps
%%% a JS closure's captured Handle bindings alive across GC. Gleam has no
%%% way to name a fun's env, so this is the ONE piece inexpressible there.
-module(arc_rt_gc_ffi).
-export([refs_in_term/2]).

%% refs_in_term(Term, Acc) -> [Int | Acc]
%% Deep walk: push every `{js_cell, N}` id reachable inside Term onto Acc.
%% Recurses into tuples/lists/maps AND a fun's captured env, so a JS closure
%% stored in a cell keeps its captured handles alive. Total: any leaf term
%% (atom | number | binary | ref | pid | port | []) contributes nothing.
refs_in_term({js_cell, N}, Acc) when is_integer(N) -> [N | Acc];
refs_in_term(F, Acc) when is_function(F) ->
    {env, Env} = erlang:fun_info(F, env),
    lists:foldl(fun refs_in_term/2, Acc, Env);
refs_in_term(T, Acc) when is_tuple(T) ->
    lists:foldl(fun refs_in_term/2, Acc, tuple_to_list(T));
refs_in_term([H | T], Acc) -> refs_in_term(T, refs_in_term(H, Acc));
refs_in_term(M, Acc) when is_map(M) ->
    maps:fold(fun(K, V, A) -> refs_in_term(V, refs_in_term(K, A)) end, Acc, M);
refs_in_term(_, Acc) -> Acc.
