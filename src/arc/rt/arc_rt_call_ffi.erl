%%% arc_rt_call_ffi — the CompiledFn apply/catch + Frame shim for `rt_call`
%%% (M-CALL, M8; SPEC §7). Pure term construction / pattern matching + apply +
%%% native try/catch: no NIF, no process state, cannot crash the node.
%%%
%%% Why a shim: (1) `t_call_protected` must catch the SAME
%%% `{wasm_exn, 0, [St, V]}` term that `arc_rt_store_ffi:t_throw/2`
%%% raises (R2 payload order `[St, V]`) and turn it into a Gleam
%%% `Completion` — Gleam has no `try…catch` over an opaque `CompiledFn`
%%% apply. (2) `mk_frame` builds the D5 PLAIN 4-tuple Frame wire (NOT a
%%% Gleam-tagged record) that emitted code indexes via `element/2` at the
%%% R7 0-based logical positions this=0/active_func=1/home_object=2/
%%% new_target=3. (3) `t_kfn_code` is the one-read CallClosure probe the
%%% interpreter and emitted code share. The emitted code's call-site fast
%%% paths (inline caches, `new` fast path) live in arc_rt_call_fast_ffi.
-module(arc_rt_call_ffi).
-export([t_call_protected/4, t_apply_protected/2, t_native_protected/4,
         mk_frame/4, t_kfn_code/3, birth_props/2]).

-include("arc_rt_layout.hrl").

%% t_kfn_code(St, Callee, This) -> {Code, ResolvedThis, Simple} | undefined
%% CallClosure fast-path probe (JRead). One heap read, no cross-module calls.
%% Record indices come from arc_rt_layout.hrl (asserted by
%% arc_rt_layout_test). The KCompiled match is positional:
%% {?KFN_TAG, Code, Home, Flags, FieldsInit, Simple, Name, Length, Birth}.
%% Simple is the raw Option term: `none` | `{some,{CodeS,Arity}}`.
t_kfn_code(St, {js_cell, Id}, This) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                {?KFN_TAG, Code, none, Flags, _, Simple, _, _, _}
                  when element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false,
                       element(?FNFLAGS_IS_GEN, Flags) =:= false,
                       element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                    %% §10.2.1.2 OrdinaryCallBindThis inlined: arrow keeps
                    %% caller `this`; strict passes it through; sloppy
                    %% undefined/null → globalThis, object → itself. A
                    %% sloppy primitive `this` must box (allocates), so it
                    %% misses to the full path.
                    case element(?FNFLAGS_IS_ARROW, Flags)
                         orelse element(?FNFLAGS_IS_STRICT, Flags) of
                        true -> {Code, This, Simple};
                        false when This =:= undefined; This =:= null ->
                            {Code,
                             element(?REALM_GLOBAL,
                                     element(?AGENT_REALM, St)),
                             Simple};
                        false when element(1, This) =:= ?HANDLE_TAG ->
                            {Code, This, Simple};
                        false -> undefined
                    end;
                _ -> undefined
            end;
        _ -> undefined
    end;
t_kfn_code(_, _, _) -> undefined.

%% t_call_protected(St, Code, Frame, Args) -> {Completion, St'}
%% Apply the opaque `CompiledFn` (`fun(St, Frame, Args) -> {V, St'}`, D4) and
%% wrap the outcome as a Gleam `Completion` wire term. A `t_throw`-raised
%% `{wasm_exn, 0, [St2, E]}` (R2: state FIRST, thrown value SECOND) becomes
%% `ThrowCompletion(E)` with the mutated `St2` recovered; a trap or any other
%% error class/shape is NOT caught here — it propagates to the run-ABI.
t_call_protected(St, Code, Frame, Args) ->
    try Code(St, Frame, Args) of
        {V, St2} -> {{?COMPLETION_NORMAL, V}, St2}
    catch
        error:{wasm_exn, 0, [St2, E]} -> {{?COMPLETION_THROW, E}, St2}
    end.

%% t_native_protected(St, Tag, This, Args) -> {Completion, St'}
%% `t_apply_protected` around `arc_rt_builtins_ffi:dispatch_native/4` with
%% no thunk built for it.
t_native_protected(St, Tag, This, Args) ->
    try arc_rt_builtins_ffi:dispatch_native(St, Tag, This, Args) of
        {V, St2} -> {{?COMPLETION_NORMAL, V}, St2}
    catch
        error:{wasm_exn, 0, [St2, E]} -> {{?COMPLETION_THROW, E}, St2}
    end.

%% t_apply_protected(St, Body) -> {Completion, St'}
%% Same catch as `t_call_protected` around a 1-arg Gleam thunk
%% `fun(St) -> {V, St'}` — for the non-`CompiledFn` `t_call` dispatch arms
%% (native / bound / proxy / not-a-function TypeError) whose bodies may
%% `t_throw` mid-evaluation and must surface as `ThrowCompletion` too.
t_apply_protected(St, Body) ->
    try Body(St) of
        {V, St2} -> {{?COMPLETION_NORMAL, V}, St2}
    catch
        error:{wasm_exn, 0, [St2, E]} -> {{?COMPLETION_THROW, E}, St2}
    end.

%% mk_frame(This, ActiveFunc, HomeObj, NewTarget) -> {This, ActiveFunc, HomeObj, NewTarget}
%% D5: the Frame passed to a `CompiledFn` is a PLAIN 4-tuple (no tag atom).
%% Emitted code reads it via `element(N+1, Frame)` for R7 0-based index N.
mk_frame(This, ActiveFunc, HomeObj, NewTarget) ->
    {This, ActiveFunc, HomeObj, NewTarget}.

%% birth_props(LengthV, Name) -> Dict(PropertyKey, Property)
%% The own props a function object is born with (rt/call birth_props):
%% `length` {W:F, E:F, C:T} at seq 0, then `name` likewise at seq 1. One
%% literal map build on the closure-creation path.
birth_props(LengthV, Name) ->
    #{{?KEY_NAMED, <<"length">>} =>
          {?DATAPROP_TAG, LengthV, false, false, true, 0},
      {?KEY_NAMED, <<"name">>} =>
          {?DATAPROP_TAG, Name, false, false, true, 1}}.
