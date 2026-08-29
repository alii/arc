-module(arc_rt_gc_ffi).
-export([refs_in_term/2, refs_in_props/2, refs_in_symbol_props/2]).
-export([keys_in_term/2, keys_in_slot/2, keys_in_keyed/2]).

-include("arc_rt_layout.hrl").
-include("arc_rt_names.hrl").

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

%% fixed names and index keys are never swept
-define(DYN_KEY(K), (K >= 0 andalso
    (K band 3 =:= ?KEY_KIND_PRIVATE orelse K >= ?NAME_KEY(?N_FIXED_COUNT)))).

%% keys a template, private name or object key names
keys_in_term({?PRIVATE_TAG, K}, Acc) when is_integer(K) -> Acc#{K => nil};
keys_in_term({?OKEY_STRING, K}, Acc) when is_integer(K) ->
    case ?DYN_KEY(K) of true -> Acc#{K => nil}; false -> Acc end;
keys_in_term({?HANDLE_TAG, _}, Acc) -> Acc;
keys_in_term(T, Acc) when tuple_size(T) =:= ?FT_ARITY, element(1, T) =:= ?FT_TAG ->
    Keys = element(?FT_KEYS, T),
    keys_in_term(element(?FT_FUNCTIONS, T), key_slots(Keys, tuple_size(Keys), Acc));
keys_in_term(T, Acc) when is_tuple(T) -> keys_in_tuple(T, tuple_size(T), Acc);
keys_in_term(F, Acc) when is_function(F) ->
    {env, Env} = erlang:fun_info(F, env),
    lists:foldl(fun keys_in_term/2, Acc, Env);
keys_in_term([H | T], Acc) -> keys_in_term(T, keys_in_term(H, Acc));
keys_in_term(M, Acc) when is_map(M) ->
    maps:fold(fun(K, V, A) -> keys_in_term(V, keys_in_term(K, A)) end, Acc, M);
keys_in_term(_, Acc) -> Acc.

keys_in_tuple(_, 0, Acc) -> Acc;
keys_in_tuple(T, I, Acc) -> keys_in_tuple(T, I - 1, keys_in_term(element(I, T), Acc)).

key_slots(_, 0, Acc) -> Acc;
key_slots(T, I, Acc) ->
    K = element(I, T),
    case ?DYN_KEY(K) of
        true -> key_slots(T, I - 1, Acc#{K => nil});
        false -> key_slots(T, I - 1, Acc)
    end.

%% prop values and elements are js values and name no keys
keys_in_slot(S, Acc) when element(1, S) =:= ?SOBJECT_TAG ->
    keys_in_term(element(?SOBJECT_KIND, S), keys_in_keyed(element(?SOBJECT_PROPS, S), Acc));
keys_in_slot(S, Acc) when element(1, S) =:= ?SSHAPED_TAG ->
    keys_in_keyed(element(?SSHAPED_OFFSETS, S), Acc);
keys_in_slot(S, Acc) -> keys_in_term(S, Acc).

keys_in_keyed(M, Acc) ->
    maps:fold(fun(K, _, A) ->
        case ?DYN_KEY(K) of true -> A#{K => nil}; false -> A end
    end, Acc, M).
