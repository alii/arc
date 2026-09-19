-module(arc_rt_store_ffi).
-export([throw/2, is_handle/1, as_object_key/1,
         cell_get/2, box_get/2]).

-include("arc_rt_layout.hrl").

cell_get(St, {?HANDLE_TAG, Id}) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_CELLS, Store)) of
        ?STORE_FREE_CELL -> dangling(<<"cell_get">>);
        Cell -> Cell
    end.

box_get(St, {?HANDLE_TAG, Id}) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_CELLS, Store)) of
        ?STORE_FREE_CELL -> dangling(<<"box_get">>);
        {?SBOX_TAG, V} -> V
    end.

dangling(Who) ->
    erlang:error(#{gleam_error => panic, message =>
        <<Who/binary, ": dangling Handle (use-after-free)">>}).

throw(St, V) -> erlang:error(?JS_THROW(St, V)).

is_handle({?HANDLE_TAG, N}) when is_integer(N) -> true;
is_handle(_) -> false.

as_object_key({?STRINGKEY_TAG, _} = K) -> K;
as_object_key({?SYMBOLKEY_TAG, _} = K) -> K;
as_object_key(K) -> {?STRINGKEY_TAG, K}.
