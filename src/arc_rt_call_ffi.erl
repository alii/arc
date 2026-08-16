%%% arc_rt_call_ffi — the CompiledFn apply/catch + Frame/Step wire
%%% shim for `rt_call` / `rt_async` (M-CALL, M8; SPEC §7).
%%%
%%% Hand-written Erlang, so it carries the `arc_rt_` namespace prefix
%%% (overview §5) and can NEVER collide with an OTP module — exactly like
%%% `arc_rt_store_ffi`. Pure term construction /
%%% pattern matching + apply + native try/catch: no NIF, no process state,
%%% cannot crash the node.
%%%
%%% Why a shim: (1) `t_call_protected` must catch the SAME
%%% `{wasm_exn, 0, [St, V]}` term that `arc_rt_store_ffi:t_throw/2`
%%% raises (R2 payload order `[St, V]`) and turn it into a Gleam
%%% `Completion` — Gleam has no `try…catch` over an opaque `CompiledFn`
%%% apply. (2) `mk_frame` builds the D5 PLAIN 4-tuple Frame wire (NOT a
%%% Gleam-tagged record) that emitted code indexes via `element/2` at the
%%% R7 0-based logical positions this=0/active_func=1/home_object=2/
%%% new_target=3. (3) `apply_sm` / `step_classify` bridge the M18
%%% state-machine closure ABI `fun(St,Rs,Sent,Loc) -> {Step, St'}` and its
%%% raw step tags to the Gleam `Step` sum.
-module(arc_rt_call_ffi).
-export([t_call_protected/4, t_apply_protected/2, mk_frame/4, apply_sm/5,
         step_classify/1, t_kfn_code/3, t_new_simple/3,
         t_call_method_mono/4]).

-include("arc_rt_layout.hrl").

%% t_kfn_code(St, Callee, This) -> {Code, ResolvedThis, Simple} | undefined
%% CallClosure fast-path probe (JRead). One heap read, no cross-module calls.
%% Record indices come from arc_rt_layout.hrl (asserted by
%% arc_rt_layout_test). The KCompiled match is positional:
%% {?KFN_TAG, Code, Home, Flags, FieldsInit, Captures, Simple}.
%% Simple is the raw Option term: `none` | `{some,{CodeS,Arity}}`.
t_kfn_code(St, {js_cell, Id}, This) ->
    Store = element(?AGENT_STORE, St),
    case array:get(Id, element(?STORE_DATA, Store)) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                {?KFN_TAG, Code, none, Flags, _, Simple}
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

%% Proto-walk depth cap for t_call_method_mono. deltablue.js `inheritsFrom`
%% chains reach 3 hops (StayConstraint→UnaryConstraint→Constraint); richards
%% is flat 1-hop. 4 covers both with headroom; deeper → miss to full path.
-define(MONO_PROTO_MAX, 4).

%% t_call_method_mono(St, Recv, KeyBin, Args) -> {V, St'} | {miss, St}
%% JMut fast-path probe for `o.m(args)`. Folds the get_prop_any proto walk +
%% t_kfn_code + CallClosure apply into ONE FFI call: own-then-proto data-prop
%% lookup (up to ?MONO_PROTO_MAX hops) → gate on ordinary user KCompiled or
%% KNative → apply with `this=Recv`. Any shape miss → `{miss, St}` (St
%% UNCHANGED — no side-effect precedes the apply) and the emitter falls back
%% to the full path. NOTE the emitter guard is `V =:= miss`, NOT `IsAtom(V)`
%% — a method may return undefined/null/bool. §9.1.8.1 own-before-proto: a
%% shaped own slot or an own data prop shadows the proto method; an own
%% accessor shadows too and misses.
t_call_method_mono(St, Recv = {?HANDLE_TAG, RId}, KeyBin, Args) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case array:get(RId, Data) of
        RSlot when is_tuple(RSlot) ->
            Own = case element(1, RSlot) of
                ?SOBJECT_TAG -> mono_own_value(RSlot, KeyBin);
                ?SSHAPED_TAG -> mono_shaped_own(Store, RSlot, KeyBin);
                _ -> miss
            end,
            case Own of
                absent ->
                    %% proto is element 3 for BOTH s_object and s_shaped_object.
                    mono_proto(St, Data, element(?SOBJECT_PROTO, RSlot),
                               KeyBin, Recv, Args);
                miss -> {miss, St};
                V -> mono_apply(St, Data, V, Recv, Args)
            end;
        _ -> {miss, St}
    end;
t_call_method_mono(St, _, _, _) -> {miss, St}.

mono_proto(St, Data, {?SOME, {?HANDLE_TAG, PId}}, KeyBin, Recv, Args) ->
    mono_proto_walk(St, Data, PId, KeyBin, Recv, Args, ?MONO_PROTO_MAX);
mono_proto(St, _, _, _, _, _) -> {miss, St}.

%% Bounded walk. Accessor or non-cell hit at any hop shadows → miss.
mono_proto_walk(St, _, _, _, _, _, 0) -> {miss, St};
mono_proto_walk(St, Data, Id, KeyBin, Recv, Args, Fuel) ->
    case array:get(Id, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case mono_own_value(Slot, KeyBin) of
                absent ->
                    case element(?SOBJECT_PROTO, Slot) of
                        {?SOME, {?HANDLE_TAG, NId}} ->
                            mono_proto_walk(St, Data, NId, KeyBin, Recv,
                                            Args, Fuel - 1);
                        _ -> {miss, St}
                    end;
                V -> mono_apply(St, Data, V, Recv, Args)
            end;
        _ -> {miss, St}
    end.

%% Own named data prop of an SObject. `absent` = key not present → caller
%% falls through to proto. An accessor SHADOWS proto, so return a non-cell
%% (`miss`) rather than `absent` — mono_apply then misses.
mono_own_value(Slot, KeyBin) ->
    case element(?SOBJECT_PROPS, Slot) of
        #{{?KEY_NAMED, KeyBin} := Prop}
          when element(1, Prop) =:= ?DATAPROP_TAG ->
            element(?DATAPROP_VALUE, Prop);
        #{{?KEY_NAMED, KeyBin} := _} -> miss;
        _ -> absent
    end.

%% §9.1.8.1 own-slot probe for an SShapedObject via JsStore.shapes.
mono_shaped_own(Store, RSlot, KeyBin) ->
    Sid = element(?SSHAPED_SID, RSlot),
    case element(?STORE_SHAPES, Store) of
        #{Sid := Desc} ->
            case element(?SHAPE_OFFSETS, Desc) of
                #{KeyBin := Off} ->
                    element(Off + 1, element(?SSHAPED_SLOTS, RSlot));
                _ -> absent
            end;
        _ -> absent
    end.

%% Gate + apply. Same KCompiled gate as t_kfn_code (home_object=:=none so
%% super.x methods miss to the full MOR). KNative → dispatch_native (M6 seam)
%% so `Array.prototype.push` etc. hit here too. `this` is Recv — always a
%% cell, so no OrdinaryCallBindThis substitution. A this-ABI simple variant
%% (KCompiled.simple with needs_this=true) of matching arity is applied as
%% CodeT(St, Recv, P0..Pn-1) with no Frame tuple; otherwise Frame per D5
%% mk_frame.
mono_apply(St, Data, Fn = {?HANDLE_TAG, FnId}, Recv, Args) ->
    case array:get(FnId, Data) of
        FSlot when element(1, FSlot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, FSlot) of
                {?KFN_TAG, Code, ?NONE, Flags, _, Simple}
                  when element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false,
                       element(?FNFLAGS_IS_GEN, Flags) =:= false,
                       element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                    case Simple of
                        {?SOME, {CodeT, Arity, true}}
                          when length(Args) =:= Arity ->
                            apply_this(CodeT, St, Recv, Args);
                        _ -> Code(St, {Recv, Fn, undefined, undefined}, Args)
                    end;
                {?KNATIVE_TAG, Tag, _, _, _} ->
                    arc@rt@builtins:dispatch_native(
                        St, Tag, Recv, Args);
                _ -> {miss, St}
            end;
        _ -> {miss, St}
    end;
mono_apply(St, _, _, _, _) -> {miss, St}.

apply_this(CodeT, St, Recv, []) -> CodeT(St, Recv);
apply_this(CodeT, St, Recv, [A]) -> CodeT(St, Recv, A);
apply_this(CodeT, St, Recv, [A, B]) -> CodeT(St, Recv, A, B);
apply_this(CodeT, St, Recv, [A, B, C]) -> CodeT(St, Recv, A, B, C);
apply_this(CodeT, St, Recv, Args) -> erlang:apply(CodeT, [St, Recv | Args]).

%% t_new_simple(St, Ctor, Args) -> {Handle, St'} | {miss, St}
%% JMut fast-path probe for `new F(args)` on a plain-function ctor
%% (§10.2.2 base case). Gate: F is a KCompiled with is_constructor,
%% NOT class/derived/gen/async, home_object=fields_init=none, and its own
%% "prototype" is a data-property Handle → inline OrdinaryCreateFromConstructor
%% + `t_cell_new` + apply body + §10.2.2 step 13 base return-override
%% (object result overrides `this`; else new `this`). Any shape miss →
%% `{miss, St}` and the emitter's IsAtom guard falls back to `t_construct`.
%% The new object is born as an SShapedObject at the empty root shape, so
%% the ctor body's `this.k = v` writes extend it along shape transitions
%% (rt_obj set_own_shaped) and later `.k` reads/writes hit the shaped-slot
%% fast paths. Record indices via arc_rt_layout.hrl.
t_new_simple(St, Ctor = {?HANDLE_TAG, CId}, Args) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case array:get(CId, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                {?KFN_TAG, Code, ?NONE, Flags, ?NONE, _}
                  when element(?FNFLAGS_IS_CTOR, Flags) =:= true,
                       element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false,
                       element(?FNFLAGS_IS_DERIVED, Flags) =:= false,
                       element(?FNFLAGS_IS_GEN, Flags) =:= false,
                       element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_NAMED, <<"prototype">>} := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG ->
                            case element(?DATAPROP_VALUE, Prop) of
                                Proto = {?HANDLE_TAG, _} ->
                                    new_simple_apply(St, Store, Data, Ctor,
                                                     Code, Proto, Args);
                                _ -> {miss, St}
                            end;
                        _ -> {miss, St}
                    end;
                _ -> {miss, St}
            end;
        _ -> {miss, St}
    end;
t_new_simple(St, _, _) -> {miss, St}.

%% Inline `t_cell_new` (rt_store.gleam) + apply + return-override.
new_simple_apply(St, Store, Data, Ctor, Code, Proto, Args) ->
    NewSlot = {?SSHAPED_TAG, 0, {?SOME, Proto}, {}},
    {NewId, Free, Next} = case element(?STORE_FREE, Store) of
        [Id | Rest] -> {Id, Rest, element(?STORE_NEXT, Store)};
        [] -> N = element(?STORE_NEXT, Store), {N, [], N + 1}
    end,
    Store2 = setelement(?STORE_DATA, Store, array:set(NewId, NewSlot, Data)),
    Store3 = setelement(?STORE_FREE, Store2, Free),
    Store4 = setelement(?STORE_NEXT, Store3, Next),
    Store5 = setelement(?STORE_ALLOC, Store4, element(?STORE_ALLOC, Store) + 1),
    St2 = setelement(?AGENT_STORE, St, Store5),
    NewThis = {?HANDLE_TAG, NewId},
    Frame = {NewThis, Ctor, undefined, Ctor},
    {V, St3} = Code(St2, Frame, Args),
    case V of
        {?HANDLE_TAG, _} -> {V, St3};
        _ -> {NewThis, St3}
    end.

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

%% apply_sm(St, Code, Rs, Sent, Loc) -> {RawStep, St'}
%% Invoke a M18 state-machine `CompiledFn` (`fun(St,Rs,Sent,Loc) -> {Step,St'}`).
%% Returns the closure's result verbatim; the caller runs `step_classify/1` on
%% the raw step term.
apply_sm(St, Code, Rs, Sent, Loc) -> Code(St, Rs, Sent, Loc).

%% step_classify(RawStep) -> Step
%% Decode the M18 emitted-code step tags into the Gleam `Step` wire encoding
%% (`rt_async.Step`): return/throw carry a value; yield/await carry the
%% yielded/awaited value, the next resume-state Int, and the saved locals Loc.
step_classify({return, V})      -> {step_return, V};
step_classify({throw, V})       -> {step_throw, V};
step_classify({yield, V, N, L}) -> {step_yield, V, N, L};
step_classify({await, V, N, L}) -> {step_await, V, N, L}.
