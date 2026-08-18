%%% arc_rt_store_ffi — the threaded-throw + Handle-probe shim for
%%% `rt_store` (M1b, SPEC §7; R2).
%%%
%%% Hand-written Erlang, so it carries the `arc_rt_` namespace prefix
%%% (overview §5) and can NEVER collide with an OTP module — exactly like
%%% `arc_rt_call_ffi`. Pure term construction /
%%% pattern matching + native raise: no NIF, no process state, cannot crash the
%%% node.
%%%
%%% Why a shim: (1) `t_throw` must raise the SAME `{wasm_exn, TagId, Payload}`
%%% term that the 2core `rt_exn` `throw_exn/2` produces, so the emitted
%%% per-clause `try…catch` and top-level run-ABI catch match it identically; the
%%% payload carries the THREADED Agent `St` alongside the thrown JsVal
%%% `V` (payload order `[St, V]` — R2) so the catch site recovers the mutated
%%% state. (2) `is_handle`/`handle_id` are total pattern-match probes on the
%%% opaque `JsVal` wire form for a Handle (`{js_cell, N}`, SPEC §2.3) — trivial
%%% and zero-copy in Erlang; awkward via `dynamic` in Gleam.
-module(arc_rt_store_ffi).
-export([t_throw/2, is_handle/1, handle_id/1, identity/1, as_object_key/1,
         t_cell_get/2, t_var_get/2, data_new/0]).

-include("arc_rt_layout.hrl").

%% data_new() -> array()
%% The empty cell arena: unset ids answer ?STORE_FREE_SLOT.
data_new() -> array:new({default, ?STORE_FREE_SLOT}).

%% t_cell_get(St, {js_cell, Id}) -> JsSlot
%% Hot-path cell read — inlines require_js + the array read so emitted code
%% and internal callers pay one lookup, not two cross-module calls.
t_cell_get(St, {?HANDLE_TAG, Id}) ->
    Store = element(?AGENT_STORE, St),
    case array:get(Id, element(?STORE_DATA, Store)) of
        ?STORE_FREE_SLOT -> erlang:error(#{gleam_error => panic, message =>
            <<"t_cell_get: dangling Handle (use-after-free)"/utf8>>});
        Slot -> Slot
    end.

%% t_var_get(St, {js_cell, Id}) -> JsVal
%% Read a compiled-code variable box: the cell holds `{s_box, V}` (rt_types
%% `SBox`, the same slot the interpreter uses for captured bindings), so the
%% GC's `refs_in_cell` can trace it. Same dangling-handle posture as
%% `t_cell_get`; a non-`SBox` slot here is an emitter bug and crashes.
t_var_get(St, {?HANDLE_TAG, Id}) ->
    Store = element(?AGENT_STORE, St),
    case array:get(Id, element(?STORE_DATA, Store)) of
        ?STORE_FREE_SLOT -> erlang:error(#{gleam_error => panic, message =>
            <<"t_var_get: dangling Handle (use-after-free)"/utf8>>});
        {s_box, V} -> V
    end.

%% t_throw(St, V) -> no_return()
%% Raise a WASM exception at ERROR class (same channel as
%% the 2core `rt_exn` `throw_exn/2` and `{wasm_trap,_}`). TagId is fixed at 0
%% (the JS exception tag); Payload is `[St, V]` — state FIRST, thrown value
%% SECOND (R2) — so the catch dispatches on the term shape and unpacks both.
t_throw(St, V) -> erlang:error({wasm_exn, 0, [St, V]}).

%% is_handle(V) -> boolean()
%% True iff `V` is the Handle wire form `{js_cell, N}` with an integer id
%% (SPEC §2.3). Total: any other JsVal wire term (undefined/null/true/false/
%% number/binary/{js_bigint,_}/{js_sym,_}/js_tdz/…) yields false.
is_handle({js_cell, N}) when is_integer(N) -> true;
is_handle(_) -> false.

%% handle_id({js_cell, N}) -> N
%% Extract the integer cell id from a Handle wire term. Partial by design —
%% callers gate on `is_handle/1` (or a `KHandle` classify) first; a non-Handle
%% argument function_clause-crashes rather than fabricating an id.
handle_id({js_cell, N}) -> N.

%% identity(X) -> X — Gleam-opaque unsafe_coerce for wire-level term reuse
%% (SPEC§8 adapters). Total.
identity(X) -> X.

%% as_object_key(K) -> ObjectKey
%% Normalise arc's SPEC§8 wire key (either a bare PropertyKey `{named,_}` /
%% `{index,_}` / `{private,_}` from `anf.object_key_lit`, or an already-
%% wrapped `{string_key,_}` / `{symbol_key,_}` from `t_to_property_key`) to
%% the ObjectKey the M4 primitives take. Total.
as_object_key({string_key, _} = K) -> K;
as_object_key({symbol_key, _} = K) -> K;
as_object_key(K) -> {string_key, K}.
