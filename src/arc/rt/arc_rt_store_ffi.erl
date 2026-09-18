-module(arc_rt_store_ffi).
-export([t_throw/2, is_handle/1, as_object_key/1,
         t_cell_get/2, t_box_get/2]).

-include("arc_rt_layout.hrl").

t_cell_get(St, {?HANDLE_TAG, Id}) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
        ?STORE_FREE_CELL -> dangling(<<"t_cell_get">>);
        Cell -> Cell
    end.

t_box_get(St, {?HANDLE_TAG, Id}) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
        ?STORE_FREE_CELL -> dangling(<<"t_box_get">>);
        {?SBOX_TAG, V} -> V
    end.

dangling(Who) ->
    erlang:error(#{gleam_error => panic, message =>
        <<Who/binary, ": dangling Handle (use-after-free)">>}).

t_throw(St, V) -> erlang:error(?JS_THROW(St, V)).

is_handle({?HANDLE_TAG, N}) when is_integer(N) -> true;
is_handle(_) -> false.

as_object_key({?OKEY_STRING, _} = K) -> K;
as_object_key({?OKEY_SYMBOL, _} = K) -> K;
as_object_key(K) -> {?OKEY_STRING, K}.
