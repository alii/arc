-module(arc_rt_gc_ffi).
-export([refs_in_term/2, refs_in_props/2, refs_in_symbol_props/2]).

-include("arc_rt_layout.hrl").

%% walks fun env too so closures keep captured handles alive
refs_in_term({js_cell, N}, Acc) when is_integer(N) -> [N | Acc];
refs_in_term(F, Acc) when is_function(F) ->
    {env, Env} = erlang:fun_info(F, env),
    lists:foldl(fun refs_in_term/2, Acc, Env);
refs_in_term(T, Acc) when is_tuple(T) -> refs_in_tuple(T, tuple_size(T), Acc);
refs_in_term([H | T], Acc) -> refs_in_term(T, refs_in_term(H, Acc));
refs_in_term(M, Acc) when is_map(M) ->
    maps:fold(fun(K, V, A) -> refs_in_term(V, refs_in_term(K, A)) end, Acc, M);
refs_in_term(_, Acc) -> Acc.

refs_in_tuple(_, 0, Acc) -> Acc;
refs_in_tuple(T, I, Acc) -> refs_in_tuple(T, I - 1, refs_in_term(element(I, T), Acc)).

refs_in_props(Props, Acc) -> refs_in_prop_list(maps:values(Props), Acc).

refs_in_symbol_props([{_, P} | T], Acc) -> refs_in_symbol_props(T, refs_in_prop(P, Acc));
refs_in_symbol_props([], Acc) -> Acc.

refs_in_prop_list([P | T], Acc) -> refs_in_prop_list(T, refs_in_prop(P, Acc));
refs_in_prop_list([], Acc) -> Acc.

refs_in_prop({?DATAPROP_TAG, V, _, _, _, _}, Acc) -> refs_in_term(V, Acc);
refs_in_prop({?ACCESSORPROP_TAG, G, S, _, _, _}, Acc) -> refs_in_term(G, refs_in_term(S, Acc));
refs_in_prop(P, Acc) -> refs_in_term(P, Acc).
