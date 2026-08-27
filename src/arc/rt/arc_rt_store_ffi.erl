-module(arc_rt_store_ffi).
-export([t_throw/2, is_handle/1, identity/1, as_object_key/1,
         t_cell_get/2, t_var_get/2]).

-include("arc_rt_layout.hrl").

t_cell_get(St, {?HANDLE_TAG, Id}) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
        ?STORE_FREE_SLOT -> erlang:error(#{gleam_error => panic, message =>
            <<"t_cell_get: dangling Handle (use-after-free)"/utf8>>});
        Slot -> Slot
    end.

t_var_get(St, {?HANDLE_TAG, Id}) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
        ?STORE_FREE_SLOT -> erlang:error(#{gleam_error => panic, message =>
            <<"t_var_get: dangling Handle (use-after-free)"/utf8>>});
        {s_box, V} -> V
    end.

t_throw(St, V) -> erlang:error({wasm_exn, 0, [St, V]}).

is_handle({js_cell, N}) when is_integer(N) -> true;
is_handle(_) -> false.

identity(X) -> X.

as_object_key({string_key, _} = K) -> K;
as_object_key({symbol_key, _} = K) -> K;
as_object_key(K) -> {string_key, K}.
