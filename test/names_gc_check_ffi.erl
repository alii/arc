-module(names_gc_check_ffi).
-export([dyn_keys/2]).

-include("arc/rt/arc_rt_layout.hrl").
-include("arc/rt/arc_rt_names.hrl").

%% every dynamic key reachable in a term, layout aware, independent of the gc's root list
dyn_keys(Term, Acc) -> walk(Term, Acc).

add(K, Acc) when is_integer(K), K >= 0,
        (K band 3 =:= ?KEY_KIND_PRIVATE orelse K >= ?NAME_KEY(?N_FIXED_COUNT)) ->
    Acc#{K => nil};
add(_, Acc) -> Acc.

keyed(M, Acc) when is_map(M) -> maps:fold(fun(K, V, A) -> walk(V, add(K, A)) end, Acc, M);
keyed(_, Acc) -> Acc.

walk({?PRIVATE_TAG, K}, Acc) when is_integer(K) -> add(K, Acc);
walk({?OKEY_STRING, K}, Acc) when is_integer(K) -> add(K, Acc);
walk(T, Acc) when tuple_size(T) =:= ?FT_ARITY, element(1, T) =:= ?FT_TAG ->
    Keys = tuple_to_list(element(?FT_KEYS, T)),
    tuple_rest(T, lists:foldl(fun add/2, Acc, Keys));
walk(T, Acc) when tuple_size(T) =:= ?SOBJECT_ARITY, element(1, T) =:= ?SOBJECT_TAG ->
    tuple_rest(T, keyed(element(?SOBJECT_PROPS, T), Acc));
walk(T, Acc) when tuple_size(T) =:= ?SSHAPED_ARITY, element(1, T) =:= ?SSHAPED_TAG ->
    tuple_rest(T, keyed(element(?SSHAPED_OFFSETS, T), Acc));
walk(T, Acc) when tuple_size(T) =:= ?SHAPE_ARITY, element(1, T) =:= ?SHAPE_TAG ->
    keyed(element(?SHAPE_TRANSITIONS, T), keyed(element(?SHAPE_OFFSETS, T), Acc));
%% the name table itself is not a holder
walk(T, Acc) when tuple_size(T) =:= ?NAMES_ARITY, element(1, T) =:= ?NAMES_TAG -> Acc;
walk(T, Acc) when is_tuple(T) -> tuple_rest(T, Acc);
walk([H | T], Acc) -> walk(T, walk(H, Acc));
walk(M, Acc) when is_map(M) ->
    maps:fold(fun(K, V, A) -> walk(V, walk(K, A)) end, Acc, M);
walk(F, Acc) when is_function(F) ->
    {env, Env} = erlang:fun_info(F, env),
    lists:foldl(fun walk/2, Acc, Env);
walk(_, Acc) -> Acc.

tuple_rest(T, Acc) -> lists:foldl(fun walk/2, Acc, tuple_to_list(T)).
