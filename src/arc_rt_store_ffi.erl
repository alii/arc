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
         t_cell_get/2]).

-include("arc_rt_layout.hrl").

%% t_cell_get(St, {js_cell, Id}) -> JsSlot
%% Hot-path cell read — inlines require_js + dict:get so emitted code and
%% internal callers pay one map lookup, not two cross-module calls. The
%% returned Slot has any `t_set_prop_own_data` fast-path writes overlaid
%% (see arc_rt_obj_ffi header) so the general path never observes a
%% stale prop value. perf8 U2: array-overlay probe gates on the constant-
%% atom ?TC_ARR_IDS tracker (obj_ffi.erl:86) instead of `get({tc_arr,Id})`
%% — deltablue's hot path (nothing overlaid) pays zero tuple alloc. The
%% tracker is a superset (jsv_evict leaves stale entries) so a false hit
%% falls through jsv_overlay_slot's own undefined arm — correct, just slow.
t_cell_get(St, {?HANDLE_TAG, Id}) ->
    Store = element(?AGENT_STORE, St),
    case element(?STORE_DATA, Store) of
        #{Id := Slot} ->
            case get(Id) of
                undefined ->
                    case get(arc_rt_tc_arr_ids) of
                        #{Id := _} ->
                            arc_rt_obj_ffi:jsv_overlay_slot(
                              Id, Slot);
                        _ -> Slot
                    end;
                _ -> arc_rt_obj_ffi:jsv_overlay_slot(Id, Slot)
            end;
        _ -> erlang:error(#{gleam_error => panic, message =>
            <<"t_cell_get: dangling Handle (use-after-free)"/utf8>>})
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
