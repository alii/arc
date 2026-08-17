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
         t_call_fast/4, t_call_fast0/3, t_call_fast1/4, t_call_fast2/5,
         t_call_fast3/6,
         t_call_method_mono/4, t_call_method_ic/6, t_call_method_ic0/5,
         t_call_method_ic1/6, t_call_method_ic2/7, t_call_method_ic3/8]).

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

%% Ordinary user function: not a class ctor, generator or async.
-define(KFN_PLAIN(Flags),
        (element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false andalso
         element(?FNFLAGS_IS_GEN, Flags) =:= false andalso
         element(?FNFLAGS_IS_ASYNC, Flags) =:= false)).

%% t_call_fast(St, F, This, Args) -> {V, St'}
%% The generic `f(args)` site as ONE host op: the t_kfn_code gate (matched
%% in place, no triple built), then the simple-ABI (arity match) or Frame
%% apply the emitter used to inline at every site, else
%% arc@rt@call:t_call_checked. Same gate, same Frame, same fallback.
t_call_fast(St, F, This, Args) ->
    call_fast(St, F, This, Args, undefined, undefined, undefined).

%% t_call_fastN(St, F, This, A1..AN) — the same with 0..3 positional args, so
%% a simple-ABI hit applies the variant with no args list and no apply hop.
t_call_fast0(St, F, This) ->
    call_fast(St, F, This, 0, undefined, undefined, undefined).
t_call_fast1(St, F, This, A) ->
    call_fast(St, F, This, 1, A, undefined, undefined).
t_call_fast2(St, F, This, A, B) ->
    call_fast(St, F, This, 2, A, B, undefined).
t_call_fast3(St, F, This, A, B, C) ->
    call_fast(St, F, This, 3, A, B, C).

%% N is the args list itself, or 0..3 with the args in A, B, C.
call_fast(St, F = {?HANDLE_TAG, Id}, This, N, A, B, C) ->
    case array:get(Id, element(?STORE_DATA, element(?AGENT_STORE, St))) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                {?KFN_TAG, Code, ?NONE, Flags, _, Simple}
                  when ?KFN_PLAIN(Flags) ->
                    %% §10.2.1.2 bind-this as in t_kfn_code.
                    case element(?FNFLAGS_IS_ARROW, Flags)
                         orelse element(?FNFLAGS_IS_STRICT, Flags) of
                        true ->
                            apply_fast(St, F, Code, Simple, This, N, A, B, C);
                        false when This =:= undefined; This =:= null ->
                            G = element(?REALM_GLOBAL, element(?AGENT_REALM, St)),
                            apply_fast(St, F, Code, Simple, G, N, A, B, C);
                        false when element(1, This) =:= ?HANDLE_TAG ->
                            apply_fast(St, F, Code, Simple, This, N, A, B, C);
                        false -> call_slow(St, F, This, N, A, B, C)
                    end;
                _ -> call_slow(St, F, This, N, A, B, C)
            end;
        _ -> call_slow(St, F, This, N, A, B, C)
    end;
call_fast(St, F, This, N, A, B, C) -> call_slow(St, F, This, N, A, B, C).

call_slow(St, F, This, N, A, B, C) ->
    arc@rt@call:t_call_checked(St, F, This, args(N, A, B, C)).

apply_fast(St, _, _, {?SOME, {CodeS, Arity, NeedsThis}}, ThisR, Args, _, _, _)
  when is_list(Args), length(Args) =:= Arity ->
    case NeedsThis of
        true -> apply_this(CodeS, St, ThisR, Args);
        false -> erlang:apply(CodeS, [St | Args])
    end;
apply_fast(St, _, _, {?SOME, {CodeS, N, true}}, ThisR, N, A, B, C) ->
    case N of
        0 -> CodeS(St, ThisR);
        1 -> CodeS(St, ThisR, A);
        2 -> CodeS(St, ThisR, A, B);
        3 -> CodeS(St, ThisR, A, B, C)
    end;
apply_fast(St, _, _, {?SOME, {CodeS, N, false}}, _, N, A, B, C) ->
    case N of
        0 -> CodeS(St);
        1 -> CodeS(St, A);
        2 -> CodeS(St, A, B);
        3 -> CodeS(St, A, B, C)
    end;
apply_fast(St, F, Code, _, ThisR, N, A, B, C) ->
    Code(St, {ThisR, F, undefined, undefined}, args(N, A, B, C)).

%% IcEntry (rt_types.gleam): {ic_call, KeyBin, Entries}, each entry
%% {ic_call_way, Sid, PId, Chain, Fn} with Chain = [{ProtoId, ProtoSlot}]
%% from the receiver's proto down to the holder. Up to ?IC_CALL_WAYS entries
%% per site (raytrace's shape.intersect and Class.create's shared
%% `this.initialize.apply(this, arguments)` are polymorphic).
-define(IC_CALL, ic_call).
-define(IC_CALL_WAY, ic_call_way).
-define(IC_CALL_WAYS, 4).

%% t_call_method_ic(St, Recv, KeyBin, Args, Site, RSite) -> {V, St'}
%% JMut. The whole compiled `o.key(args)` site as ONE host op: the IC probe
%% below, and on its miss the same read + call the emitter used to inline at
%% every site — `t_get_prop_site` at the read site `RSite`, then `call_fast`
%% with `this = Recv`. St is unchanged on a probe miss (no side effect
%% precedes the apply), so the read observes exactly the state it did inline.
%%
%% The probe: `t_call_method_mono` with a per-site inline cache (JsStore.ics).
%% Hit: receiver is a shaped object of an entry's shape (so no own `key`),
%% its proto is the entry's first cell and every cell on the chain still
%% holds the very slot the key was resolved through (an equal slot has the
%% same props and the same proto link; any write replaces it), then apply
%% the entry's data value with the mono gate. Otherwise the mono body runs
%% and, when a shaped receiver resolves the key on its proto chain, records
%% the way: replacing a stale entry (same shape and proto, a chain cell was
%% written) or adding one while the site has room.
t_call_method_ic(St, Recv, KeyBin, Args, Site, RSite) ->
    method(St, Recv, KeyBin, Site, RSite, Args, undefined, undefined,
           undefined).

%% t_call_method_icN(St, Recv, KeyBin, Site, RSite, A1..AN) — the same with
%% 0..3 positional args, so a hit applies a matching simple variant with no
%% args list, no length/1 and no apply hop.
t_call_method_ic0(St, Recv, KeyBin, Site, RSite) ->
    method(St, Recv, KeyBin, Site, RSite, 0, undefined, undefined, undefined).
t_call_method_ic1(St, Recv, KeyBin, Site, RSite, A) ->
    method(St, Recv, KeyBin, Site, RSite, 1, A, undefined, undefined).
t_call_method_ic2(St, Recv, KeyBin, Site, RSite, A, B) ->
    method(St, Recv, KeyBin, Site, RSite, 2, A, B, undefined).
t_call_method_ic3(St, Recv, KeyBin, Site, RSite, A, B, C) ->
    method(St, Recv, KeyBin, Site, RSite, 3, A, B, C).

%% N is the args list itself, or 0..3 with the args in A, B, C.
method(St, Recv, KeyBin, Site, RSite, N, A, B, C) ->
    case ic(St, Recv, KeyBin, Site, N, A, B, C) of
        {miss, St1} ->
            {F, St2} = arc_rt_obj_ffi:t_get_prop_site(St1, Recv, KeyBin,
                                                      RSite),
            call_fast(St2, F, Recv, N, A, B, C);
        Hit -> Hit
    end.

ic(St, Recv = {?HANDLE_TAG, RId}, KeyBin, Site, N, A, B, C) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    RSlot = array:get(RId, Data),
    case element(?STORE_ICS, Store) of
        #{Site := {?IC_CALL, KeyBin, Entries}} ->
            case RSlot of
                {?SSHAPED_TAG, Sid, Proto, _} ->
                    case ic_probe(Data, Sid, Proto, Entries) of
                        Fn = {?HANDLE_TAG, _} ->
                            apply_pos(St, Data, Fn, Recv, N, A, B, C);
                        stale ->
                            mono(St, Recv, RSlot, KeyBin, args(N, A, B, C),
                                 Site);
                        miss when length(Entries) < ?IC_CALL_WAYS ->
                            mono(St, Recv, RSlot, KeyBin, args(N, A, B, C),
                                 Site);
                        miss ->
                            mono(St, Recv, RSlot, KeyBin, args(N, A, B, C),
                                 none)
                    end;
                _ -> mono(St, Recv, RSlot, KeyBin, args(N, A, B, C), none)
            end;
        #{Site := _} ->
            mono(St, Recv, RSlot, KeyBin, args(N, A, B, C), none);
        _ -> mono(St, Recv, RSlot, KeyBin, args(N, A, B, C), Site)
    end;
ic(St, _, _, _, _, _, _, _) -> {miss, St}.

args(L, _, _, _) when is_list(L) -> L;
args(0, _, _, _) -> [];
args(1, A, _, _) -> [A];
args(2, A, B, _) -> [A, B];
args(3, A, B, C) -> [A, B, C].

%% mono_apply with positional args: a simple variant of arity N is applied
%% directly (with `this` only when it reads it); the Frame and native paths
%% cons the list.
apply_pos(St, Data, Fn, Recv, Args, _, _, _) when is_list(Args) ->
    mono_apply(St, Data, Fn, Recv, Args);
apply_pos(St, Data, Fn = {?HANDLE_TAG, FnId}, Recv, N, A, B, C) ->
    case array:get(FnId, Data) of
        FSlot when element(1, FSlot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, FSlot) of
                {?KFN_TAG, Code, ?NONE, Flags, _, Simple}
                  when ?KFN_PLAIN(Flags) ->
                    case Simple of
                        {?SOME, {CodeT, N, true}} ->
                            case N of
                                0 -> CodeT(St, Recv);
                                1 -> CodeT(St, Recv, A);
                                2 -> CodeT(St, Recv, A, B);
                                3 -> CodeT(St, Recv, A, B, C)
                            end;
                        {?SOME, {CodeT, N, false}} ->
                            case N of
                                0 -> CodeT(St);
                                1 -> CodeT(St, A);
                                2 -> CodeT(St, A, B);
                                3 -> CodeT(St, A, B, C)
                            end;
                        _ ->
                            Code(St, {Recv, Fn, undefined, undefined},
                                 args(N, A, B, C))
                    end;
                {?KNATIVE_TAG, Tag, _, _, _} ->
                    arc@rt@builtins:dispatch_native(
                        St, Tag, Recv, args(N, A, B, C));
                _ -> {miss, St}
            end;
        _ -> {miss, St}
    end.

%% Fn (hit) | stale (a way for this shape+proto failed its chain) | miss.
ic_probe(Data, Sid, Proto = {?SOME, {?HANDLE_TAG, PId}},
         [{?IC_CALL_WAY, Sid, PId, Chain, Fn} | _]) ->
    case ic_chain_ok(Data, Proto, Chain) of
        true -> Fn;
        false -> stale
    end;
ic_probe(Data, Sid, Proto, [_ | Rest]) -> ic_probe(Data, Sid, Proto, Rest);
ic_probe(_, _, _, []) -> miss.

ic_chain_ok(_, _, []) -> true;
ic_chain_ok(Data, {?SOME, {?HANDLE_TAG, PId}}, [{PId, PSlot} | Rest]) ->
    case array:get(PId, Data) of
        PSlot -> ic_chain_ok(Data, element(?SOBJECT_PROTO, PSlot), Rest);
        _ -> false
    end;
ic_chain_ok(_, _, _) -> false.

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
    Data = element(?STORE_DATA, element(?AGENT_STORE, St)),
    mono(St, Recv, array:get(RId, Data), KeyBin, Args, none);
t_call_method_mono(St, _, _, _) -> {miss, St}.

%% RSlot is the receiver's slot; Site is `none` (no cache) or the site id
%% to record the resolved way on.
mono(St, Recv, RSlot, KeyBin, Args, Site) when is_tuple(RSlot) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    {Own, Ic} = case element(1, RSlot) of
        ?SOBJECT_TAG -> {mono_own_value(RSlot, KeyBin), none};
        ?SSHAPED_TAG when Site =:= none ->
            {mono_shaped_own(Store, RSlot, KeyBin), none};
        ?SSHAPED_TAG ->
            {mono_shaped_own(Store, RSlot, KeyBin),
             {Site, element(?SSHAPED_SID, RSlot), []}};
        _ -> {miss, none}
    end,
    case Own of
        absent ->
            %% proto is element 3 for BOTH s_object and s_shaped_object.
            mono_proto(St, Data, element(?SOBJECT_PROTO, RSlot), KeyBin,
                       Recv, Args, Ic);
        miss -> {miss, St};
        V -> mono_apply(St, Data, V, Recv, Args)
    end;
mono(St, _, _, _, _, _) -> {miss, St}.

mono_proto(St, Data, {?SOME, {?HANDLE_TAG, PId}}, KeyBin, Recv, Args, Ic) ->
    mono_proto_walk(St, Data, PId, KeyBin, Recv, Args, ?MONO_PROTO_MAX, Ic);
mono_proto(St, _, _, _, _, _, _) -> {miss, St}.

%% Bounded walk. Accessor or non-cell hit at any hop shadows → miss. Ic is
%% `none` or `{Site, Sid, Chain}` accumulating the hops walked (reversed).
mono_proto_walk(St, _, _, _, _, _, 0, _) -> {miss, St};
mono_proto_walk(St, Data, Id, KeyBin, Recv, Args, Fuel, Ic) ->
    case array:get(Id, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            mono_hop(St, Data, Id, Slot, mono_own_value(Slot, KeyBin),
                     KeyBin, Recv, Args, Fuel, Ic);
        Slot when element(1, Slot) =:= ?SSHAPED_TAG ->
            Own = mono_shaped_own(element(?AGENT_STORE, St), Slot, KeyBin),
            mono_hop(St, Data, Id, Slot, Own, KeyBin, Recv, Args, Fuel, Ic);
        _ -> {miss, St}
    end.

%% proto is element 3 for BOTH s_object and s_shaped_object.
mono_hop(St, Data, Id, Slot, absent, KeyBin, Recv, Args, Fuel, Ic) ->
    case element(?SOBJECT_PROTO, Slot) of
        {?SOME, {?HANDLE_TAG, NId}} ->
            mono_proto_walk(St, Data, NId, KeyBin, Recv, Args, Fuel - 1,
                            ic_hop(Ic, Id, Slot));
        _ -> {miss, St}
    end;
mono_hop(St, Data, Id, Slot, Fn = {?HANDLE_TAG, _}, KeyBin, Recv, Args, _, Ic)
  when Ic =/= none ->
    case mono_apply(St, Data, Fn, Recv, Args) of
        {miss, _} = Miss -> Miss;
        {V, St2} -> {V, ic_fill(St2, ic_hop(Ic, Id, Slot), Fn, KeyBin)}
    end;
mono_hop(St, Data, _, _, V, _, Recv, Args, _, _) ->
    mono_apply(St, Data, V, Recv, Args).

ic_hop(none, _, _) -> none;
ic_hop({Site, Sid, Chain}, Id, Slot) -> {Site, Sid, [{Id, Slot} | Chain]}.

%% Record the resolved way after a successful apply: drop the entry for the
%% same shape and proto (it went stale), then add while there is room.
ic_fill(St, {Site, Sid, RevChain}, Fn, KeyBin) ->
    Store = element(?AGENT_STORE, St),
    Ics = element(?STORE_ICS, Store),
    Chain = [{PId, _} | _] = lists:reverse(RevChain),
    Kept = case Ics of
        #{Site := {?IC_CALL, KeyBin, Es}} ->
            [E || E = {?IC_CALL_WAY, S, P, _, _} <- Es,
                  S =/= Sid orelse P =/= PId];
        _ -> []
    end,
    case length(Kept) < ?IC_CALL_WAYS of
        true ->
            New = {?IC_CALL_WAY, Sid, PId, Chain, Fn},
            IcE = {?IC_CALL, KeyBin, [New | Kept]},
            setelement(?AGENT_STORE, St,
                       setelement(?STORE_ICS, Store, Ics#{Site => IcE}));
        false -> St
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
                  when ?KFN_PLAIN(Flags) ->
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
