%%% arc_rt_call_fast_ffi — the call-site fast paths AOT-emitted code calls
%%% directly (`aot/src/arc_aot/host_ops.gleam`): `t_call_fast*` for a plain
%%% closure call, `t_call_method_ic*` / `t_call_method_mono` for a method call
%%% through the per-site inline cache, `t_new_simple` for `new C(...)` on an
%%% ordinary constructor. Every entry answers `{miss, St}` on any shape it
%%% does not handle and the emitter's guard falls back to the full
%%% `arc@rt@call` path. Record indices come from arc_rt_layout.hrl (asserted
%%% by arc_rt_layout_test). Nothing here is bound from Gleam except by tests.
-module(arc_rt_call_fast_ffi).
-export([t_call_fast/4, t_call_fast0/3, t_call_fast1/4, t_call_fast2/5,
         t_call_fast3/6,
         t_call_method_mono/4, t_call_method_ic/6, t_call_method_ic0/5,
         t_call_method_ic1/6, t_call_method_ic2/7, t_call_method_ic3/8,
         t_new_simple/3]).

-include("arc_rt_layout.hrl").

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
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, St))) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                {?KFN_TAG, Code, Home, Flags, _, Simple, _, _, _}
                  when ?KFN_PLAIN(Flags) ->
                    %% §10.2.1.2 bind-this as in t_kfn_code.
                    case element(?FNFLAGS_IS_ARROW, Flags)
                         orelse element(?FNFLAGS_IS_STRICT, Flags) of
                        true ->
                            apply_fast(St, F, Code, Home, Simple, This, N, A,
                                       B, C);
                        false when This =:= undefined; This =:= null ->
                            G = element(?REALM_GLOBAL, element(?AGENT_REALM, St)),
                            apply_fast(St, F, Code, Home, Simple, G, N, A, B,
                                       C);
                        false when element(1, This) =:= ?HANDLE_TAG ->
                            apply_fast(St, F, Code, Home, Simple, This, N, A,
                                       B, C);
                        false -> call_slow(St, F, This, N, A, B, C)
                    end;
                _ -> call_slow(St, F, This, N, A, B, C)
            end;
        _ -> call_slow(St, F, This, N, A, B, C)
    end;
call_fast(St, F, This, N, A, B, C) -> call_slow(St, F, This, N, A, B, C).

call_slow(St, F, This, N, A, B, C) ->
    arc@rt@call:t_call_checked(St, F, This, args(N, A, B, C)).

apply_fast(St, _, _, _, {?SOME, {CodeS, Arity, NeedsThis}}, ThisR, Args, _, _,
           _)
  when is_list(Args), length(Args) =:= Arity ->
    case NeedsThis of
        true -> apply_this(CodeS, St, ThisR, Args);
        false -> erlang:apply(CodeS, [St | Args])
    end;
apply_fast(St, _, _, _, {?SOME, {CodeS, N, true}}, ThisR, N, A, B, C) ->
    case N of
        0 -> CodeS(St, ThisR);
        1 -> CodeS(St, ThisR, A);
        2 -> CodeS(St, ThisR, A, B);
        3 -> CodeS(St, ThisR, A, B, C)
    end;
apply_fast(St, _, _, _, {?SOME, {CodeS, N, false}}, _, N, A, B, C) ->
    case N of
        0 -> CodeS(St);
        1 -> CodeS(St, A);
        2 -> CodeS(St, A, B);
        3 -> CodeS(St, A, B, C)
    end;
apply_fast(St, F, Code, Home, _, ThisR, N, A, B, C) ->
    Code(St, {ThisR, F, home(Home), undefined}, args(N, A, B, C)).

%% Frame slot 3: the callee's [[HomeObject]] cell, or `undefined`.
home({?SOME, H}) -> H;
home(?NONE) -> undefined.

%% IcEntry (rt_types.gleam): {ic_call, KeyBin, Ways}, Ways a map from a
%% way's Match to {Chain, Fn, Kind} (rt_types.IcCallWay): Match says which
%% receivers the way answers for —
%%   {ic_shaped, Sid, PId}  a shaped object of shape Sid (so no own `key`)
%%                          whose proto is PId;
%%   {ic_plain, PId}        an SObject whose named `key` would be a plain
%%                          props entry (arc_rt_obj_ffi:named_plain) but is
%%                          absent, and whose proto is PId;
%%   {ic_own, RId}          the very cell RId while it still holds the slot
%%                          in Chain (= [{RId, Slot}]), `key` being its own
%%                          data property;
%%   {ic_prim, W, PId}      a string (W = ?REALM_STRING, `key` not its own
%%                          "length") or number (?REALM_NUMBER) primitive
%%                          while the realm's wrapper prototype is PId, the
%%                          callee a native or strict function (`this` stays
%%                          the primitive, §10.2.1.2) —
%% Chain = [{ProtoId, ProtoSlot}] from the receiver's proto down to the holder
%% and Kind the callee cell's ObjKind when the way was filled. Up to
%% ?IC_CALL_WAYS ways per site (raytrace's shape.intersect and Class.create's
%% shared `this.initialize.apply(this, arguments)` are polymorphic).
-define(IC_CALL, ic_call).
-define(IC_CALL_WAYS, 16).

%% t_call_method_ic(St, Recv, KeyBin, Args, Site, RSite) -> {V, St'}
%% JMut. The whole compiled `o.key(args)` site as ONE host op: the IC probe
%% below, and on its miss the same read + call the emitter used to inline at
%% every site — `t_get_prop_site` at the read site `RSite`, then `call_fast`
%% with `this = Recv`. St is unchanged on a probe miss (no side effect
%% precedes the apply), so the read observes exactly the state it did inline.
%%
%% The probe: `t_call_method_mono` with a per-site inline cache (JsStore.ics).
%% Hit: a way's Match holds for the receiver and every cell on its chain
%% still holds the very slot the key was resolved through (an equal slot has
%% the same props and the same proto link; any write replaces it), then
%% apply the way's recorded callee kind with the mono gate, without reading
%% the callee cell (rt_types.IcCallWay says why). Otherwise the mono body
%% runs and, when it resolves the key to a callee that passes the gate,
%% records the way before applying it: replacing a stale way (same Match, a
%% chain cell was written) or adding one while the site has room. An `own`
%% way whose cell changed is never refilled, so a receiver whose own slots
%% keep changing costs one probe, not a store write per call.
t_call_method_ic(St, Recv, KeyBin, Args, Site, RSite) ->
    ic(St, Recv, KeyBin, Site, RSite, Args, undefined, undefined, undefined).

%% t_call_method_icN(St, Recv, KeyBin, Site, RSite, A1..AN) — the same with
%% 0..3 positional args, so a hit applies a matching simple variant with no
%% args list, no length/1 and no apply hop.
t_call_method_ic0(St, Recv, KeyBin, Site, RSite) ->
    ic(St, Recv, KeyBin, Site, RSite, 0, undefined, undefined, undefined).
t_call_method_ic1(St, Recv, KeyBin, Site, RSite, A) ->
    ic(St, Recv, KeyBin, Site, RSite, 1, A, undefined, undefined).
t_call_method_ic2(St, Recv, KeyBin, Site, RSite, A, B) ->
    ic(St, Recv, KeyBin, Site, RSite, 2, A, B, undefined).
t_call_method_ic3(St, Recv, KeyBin, Site, RSite, A, B, C) ->
    ic(St, Recv, KeyBin, Site, RSite, 3, A, B, C).

%% N is the args list itself, or 0..3 with the args in A, B, C. A hit applies
%% the callee as a tail call; every other way runs the mono body under
%% `slow`, which takes the emitter's read + call on its miss. Fill is `none`
%% (record nothing) or the site to record the resolved way on.
ic(St, Recv = {?HANDLE_TAG, RId}, KeyBin, Site, RSite, N, A, B, C) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    RSlot = arc_rt_arena_ffi:get(RId, Data),
    Fill = case element(?STORE_ICS, Store) of
        #{Site := {?IC_CALL, KeyBin, Ways}} ->
            case ic_probe(Data, RId, RSlot, KeyBin, Ways) of
                {hit, _, _} = Hit -> Hit;
                stale -> Site;
                spent -> none;
                miss when map_size(Ways) < ?IC_CALL_WAYS -> Site;
                miss -> none
            end;
        #{Site := _} -> none;
        _ -> Site
    end,
    case Fill of
        {hit, Fn1, Kind1} -> apply_kind(St, Kind1, Fn1, Recv, N, A, B, C);
        _ ->
            slow(mono(St, Recv, RSlot, KeyBin, args(N, A, B, C), Fill), Recv,
                 KeyBin, RSite, N, A, B, C)
    end;
ic(St, Recv, KeyBin, Site, RSite, N, A, B, C) ->
    case prim_wrapper(Recv, KeyBin) of
        none -> slow({miss, St}, Recv, KeyBin, RSite, N, A, B, C);
        W -> prim(St, Recv, W, KeyBin, Site, RSite, N, A, B, C)
    end.

%% The Realm field of the wrapper prototype a primitive receiver's method
%% read starts from (arc_rt_obj_ffi:read_prim), or `none`.
prim_wrapper(Recv, KeyBin) when is_binary(Recv), KeyBin =/= <<"length">> ->
    ?REALM_STRING;
prim_wrapper(Recv, _) when is_number(Recv) -> ?REALM_NUMBER;
prim_wrapper(_, _) -> none.

%% The method call on a string / number primitive: probe the site's
%% `ic_prim` way for this wrapper, else walk from the wrapper prototype as
%% the mono body does, recording the way.
prim(St, Recv, W, KeyBin, Site, RSite, N, A, B, C) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    Proto = {?SOME, {?HANDLE_TAG, PId}} =
        {?SOME, element(?PAIR_PROTO, element(W, element(?AGENT_REALM, St)))},
    Fill = case element(?STORE_ICS, Store) of
        #{Site := {?IC_CALL, KeyBin, Ways}} ->
            case Ways of
                #{{ic_prim, W, PId} := {Chain, Fn0, Kind0}} ->
                    case ic_chain_ok(Data, Proto, Chain) of
                        true -> {hit, Fn0, Kind0};
                        false -> Site
                    end;
                _ when map_size(Ways) < ?IC_CALL_WAYS -> Site;
                _ -> none
            end;
        #{Site := _} -> none;
        _ -> Site
    end,
    case Fill of
        {hit, Fn, Kind} -> apply_kind(St, Kind, Fn, Recv, N, A, B, C);
        none -> slow({miss, St}, Recv, KeyBin, RSite, N, A, B, C);
        _ ->
            slow(mono_proto_walk(St, Data, PId, KeyBin, Recv,
                                 args(N, A, B, C), ?MONO_PROTO_MAX,
                                 {Fill, {ic_prim, W}, []}),
                 Recv, KeyBin, RSite, N, A, B, C)
    end.

slow({miss, St}, Recv, KeyBin, RSite, N, A, B, C) ->
    {F, St1} = arc_rt_obj_ffi:t_get_prop_site(St, Recv, KeyBin, RSite),
    call_fast(St1, F, Recv, N, A, B, C);
slow(Hit, _, _, _, _, _, _, _) -> Hit.

args(L, _, _, _) when is_list(L) -> L;
args(0, _, _, _) -> [];
args(1, A, _, _) -> [A];
args(2, A, B, _) -> [A, B];
args(3, A, B, C) -> [A, B, C].

%% Apply a way's recorded callee Kind (already past the mono gate when it
%% was filled) with positional args: a simple variant of arity N is applied
%% directly (with `this` only when it reads it); the Frame and native paths
%% cons the list.
apply_kind(St, Kind, Fn, Recv, Args, _, _, _) when is_list(Args) ->
    kind_apply(St, Kind, Fn, Recv, Args);
apply_kind(St, {?KFN_TAG, Code, Home, _, _, Simple, _, _, _}, Fn, Recv, N, A,
           B, C) ->
    case Simple of
        {?SOME, {CodeT, N, true}} ->
            case N of
                0 -> CodeT(St, Recv);
                1 -> CodeT(St, Recv, A);
                2 -> CodeT(St, Recv, A, B);
                3 -> CodeT(St, Recv, A, B, C)
            end;
        {?SOME, {CodeS, N, false}} ->
            case N of
                0 -> CodeS(St);
                1 -> CodeS(St, A);
                2 -> CodeS(St, A, B);
                3 -> CodeS(St, A, B, C)
            end;
        _ -> Code(St, {Recv, Fn, home(Home), undefined}, args(N, A, B, C))
    end;
apply_kind(St, {?KNATIVE_TAG, Tag, _, _, _}, _, Recv, N, A, B, C) ->
    arc@rt@builtins:dispatch_native(St, Tag, Recv, args(N, A, B, C)).

%% {hit, Fn, Kind} | stale (a shaped/plain way for this receiver failed its
%% chain: refill) | spent (an own way for this cell no longer holds: do not
%% refill) | miss (no way for this receiver).
ic_probe(Data, _, {?SSHAPED_TAG, Sid, {?SOME, {?HANDLE_TAG, PId}} = Proto, _},
         _, Ways) ->
    case Ways of
        #{{ic_shaped, Sid, PId} := {Chain, Fn, Kind}} ->
            case ic_chain_ok(Data, Proto, Chain) of
                true -> {hit, Fn, Kind};
                false -> stale
            end;
        _ -> miss
    end;
ic_probe(Data, RId, RSlot, KeyBin, Ways) when element(1, RSlot) =:= ?SOBJECT_TAG ->
    case Ways of
        #{{ic_own, RId} := {[{_, Slot}], Fn, Kind}} ->
            case Slot =:= RSlot of
                true -> {hit, Fn, Kind};
                false -> spent
            end;
        _ ->
            case element(?SOBJECT_PROTO, RSlot) of
                {?SOME, {?HANDLE_TAG, PId}} = Proto ->
                    case Ways of
                        #{{ic_plain, PId} := {Chain, Fn, Kind}} ->
                            Own = is_map_key({?KEY_NAMED, KeyBin},
                                             element(?SOBJECT_PROPS, RSlot))
                                orelse not arc_rt_obj_ffi:named_plain(
                                             element(?SOBJECT_KIND, RSlot),
                                             KeyBin),
                            case Own of
                                true -> miss;
                                false ->
                                    case ic_chain_ok(Data, Proto, Chain) of
                                        true -> {hit, Fn, Kind};
                                        false -> stale
                                    end
                            end;
                        _ -> miss
                    end;
                _ -> miss
            end
    end;
ic_probe(_, _, _, _, _) -> miss.

ic_chain_ok(_, _, []) -> true;
ic_chain_ok(Data, {?SOME, {?HANDLE_TAG, PId}}, [{PId, PSlot} | Rest]) ->
    case arc_rt_arena_ffi:get(PId, Data) of
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
    mono(St, Recv, arc_rt_arena_ffi:get(RId, Data), KeyBin, Args, none);
t_call_method_mono(St, _, _, _) -> {miss, St}.

%% RSlot is the receiver's slot; Site is `none` (no cache) or the site id
%% to record the resolved way on. Ic is `none` or `{Site, Match0, Chain}`:
%% the way being built, Match0 lacking the proto id the first hop supplies.
mono(St, Recv = {?HANDLE_TAG, RId}, RSlot, KeyBin, Args, Site)
  when is_tuple(RSlot) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    {Own, Ic} = case element(1, RSlot) of
        ?SOBJECT_TAG when Site =:= none ->
            {mono_own_value(RSlot, KeyBin), none};
        ?SOBJECT_TAG ->
            case arc_rt_obj_ffi:named_plain(element(?SOBJECT_KIND, RSlot),
                                            KeyBin) of
                true -> {mono_own_value(RSlot, KeyBin), {Site, ic_plain, []}};
                false -> {mono_own_value(RSlot, KeyBin), none}
            end;
        ?SSHAPED_TAG when Site =:= none ->
            {mono_shaped_own(Store, RSlot, KeyBin), none};
        ?SSHAPED_TAG ->
            {mono_shaped_own(Store, RSlot, KeyBin),
             {Site, {ic_shaped, element(?SSHAPED_SID, RSlot)}, []}};
        _ -> {miss, none}
    end,
    case Own of
        absent ->
            %% proto is element 3 for BOTH s_object and s_shaped_object.
            mono_proto(St, Data, element(?SOBJECT_PROTO, RSlot), KeyBin,
                       Recv, Args, Ic);
        miss -> {miss, St};
        V when Ic =/= none, element(1, RSlot) =:= ?SOBJECT_TAG ->
            mono_found(St, Data, V, KeyBin, Recv, Args,
                       {Site, {ic_own, RId, RSlot}, []});
        V -> mono_apply(St, Data, V, Recv, Args)
    end;
mono(St, _, _, _, _, _) -> {miss, St}.

mono_proto(St, Data, {?SOME, {?HANDLE_TAG, PId}}, KeyBin, Recv, Args, Ic) ->
    mono_proto_walk(St, Data, PId, KeyBin, Recv, Args, ?MONO_PROTO_MAX, Ic);
mono_proto(St, _, _, _, _, _, _) -> {miss, St}.

%% Bounded walk. Accessor or non-cell hit at any hop shadows → miss. Ic
%% accumulates the hops walked (reversed).
mono_proto_walk(St, _, _, _, _, _, 0, _) -> {miss, St};
mono_proto_walk(St, Data, Id, KeyBin, Recv, Args, Fuel, Ic) ->
    case arc_rt_arena_ffi:get(Id, Data) of
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
mono_hop(St, Data, Id, Slot, V, KeyBin, Recv, Args, _, Ic) when Ic =/= none ->
    mono_found(St, Data, V, KeyBin, Recv, Args, ic_hop(Ic, Id, Slot));
mono_hop(St, Data, _, _, V, _, Recv, Args, _, _) ->
    mono_apply(St, Data, V, Recv, Args).

%% The key resolved to V with a way to record: gate, fill, then apply. A
%% primitive receiver is passed as `this` unconverted, so only a native or a
%% strict function takes it (§10.2.1.2 OrdinaryCallBindThis).
mono_found(St, Data, Fn = {?HANDLE_TAG, _}, KeyBin, Recv, Args, Ic) ->
    case mono_kind(Data, Fn) of
        miss -> {miss, St};
        {?KFN_TAG, _, _, Flags, _, _, _, _, _}
          when not is_tuple(Recv),
               element(?FNFLAGS_IS_STRICT, Flags) =/= true ->
            {miss, St};
        Kind ->
            kind_apply(ic_fill(St, Ic, Fn, Kind, KeyBin), Kind, Fn, Recv, Args)
    end;
mono_found(St, _, _, _, _, _, _) -> {miss, St}.

ic_hop(none, _, _) -> none;
ic_hop({Site, Match, Chain}, Id, Slot) -> {Site, Match, [{Id, Slot} | Chain]}.

%% Record the resolved way under its Match (replacing a stale one) while the
%% site has room. An `own` way's chain is the receiver cell itself.
ic_fill(St, {Site, Match0, RevChain}, Fn, Kind, KeyBin) ->
    Store = element(?AGENT_STORE, St),
    Ics = element(?STORE_ICS, Store),
    Chain = lists:reverse(RevChain),
    {Match, Way} = case {Match0, Chain} of
        {{ic_shaped, Sid}, [{PId, _} | _]} ->
            {{ic_shaped, Sid, PId}, {Chain, Fn, Kind}};
        {ic_plain, [{PId, _} | _]} -> {{ic_plain, PId}, {Chain, Fn, Kind}};
        {{ic_own, RId, RSlot}, []} ->
            {{ic_own, RId}, {[{RId, RSlot}], Fn, Kind}};
        {{ic_prim, W}, [{PId, _} | _]} -> {{ic_prim, W, PId}, {Chain, Fn, Kind}}
    end,
    Ways = case Ics of
        #{Site := {?IC_CALL, KeyBin, Ways0}} -> Ways0;
        _ -> #{}
    end,
    case is_map_key(Match, Ways) orelse map_size(Ways) < ?IC_CALL_WAYS of
        true ->
            IcE = {?IC_CALL, KeyBin, Ways#{Match => Way}},
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

%% Gate + apply. Same KCompiled gate as call_fast; a method's [[HomeObject]]
%% rides in the Frame (a simple variant never reads it). KNative →
%% dispatch_native (M6 seam) so `Array.prototype.push` etc. hit here too.
%% `this` is Recv — always a cell, so no OrdinaryCallBindThis substitution. A
%% simple variant (KCompiled.simple) of matching arity is applied as
%% CodeT(St, Recv, P0..Pn-1) / CodeS(St, P0..Pn-1) with no Frame tuple;
%% otherwise Frame per D5 mk_frame.
mono_apply(St, Data, Fn = {?HANDLE_TAG, _}, Recv, Args) ->
    case mono_kind(Data, Fn) of
        miss -> {miss, St};
        Kind -> kind_apply(St, Kind, Fn, Recv, Args)
    end;
mono_apply(St, _, _, _, _) -> {miss, St}.

%% The callee's ObjKind when it passes the gate, else `miss`.
mono_kind(Data, {?HANDLE_TAG, FnId}) ->
    case arc_rt_arena_ffi:get(FnId, Data) of
        FSlot when element(1, FSlot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, FSlot) of
                Kind = {?KFN_TAG, _, _, Flags, _, _, _, _, _}
                  when ?KFN_PLAIN(Flags) ->
                    Kind;
                Kind when element(1, Kind) =:= ?KNATIVE_TAG -> Kind;
                _ -> miss
            end;
        _ -> miss
    end.

kind_apply(St, {?KFN_TAG, Code, Home, _, _, Simple, _, _, _}, Fn, Recv, Args) ->
    case Simple of
        {?SOME, {CodeT, Arity, true}} when length(Args) =:= Arity ->
            apply_this(CodeT, St, Recv, Args);
        {?SOME, {CodeS, Arity, false}} when length(Args) =:= Arity ->
            erlang:apply(CodeS, [St | Args]);
        _ -> Code(St, {Recv, Fn, home(Home), undefined}, Args)
    end;
kind_apply(St, {?KNATIVE_TAG, Tag, _, _, _}, _, Recv, Args) ->
    arc@rt@builtins:dispatch_native(St, Tag, Recv, Args).

apply_this(CodeT, St, Recv, []) -> CodeT(St, Recv);
apply_this(CodeT, St, Recv, [A]) -> CodeT(St, Recv, A);
apply_this(CodeT, St, Recv, [A, B]) -> CodeT(St, Recv, A, B);
apply_this(CodeT, St, Recv, [A, B, C]) -> CodeT(St, Recv, A, B, C);
apply_this(CodeT, St, Recv, Args) -> erlang:apply(CodeT, [St, Recv | Args]).

%% t_new_simple(St, Ctor, Args) -> {Handle, St'} | {miss, St}
%% JMut fast-path probe for `new F(args)` on a base constructor (§10.2.2
%% kind base): F is a KCompiled with is_constructor, a plain function or a
%% base class constructor, NOT derived/gen/async, no fields_init, and its own
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
    case arc_rt_arena_ffi:get(CId, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                Kind = {?KFN_TAG, _, _, Flags, ?NONE, _, _, _, _}
                  when element(?FNFLAGS_IS_CTOR, Flags) =:= true,
                       element(?FNFLAGS_IS_DERIVED, Flags) =:= false,
                       element(?FNFLAGS_IS_GEN, Flags) =:= false,
                       element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_NAMED, <<"prototype">>} := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG ->
                            case element(?DATAPROP_VALUE, Prop) of
                                Proto = {?HANDLE_TAG, _} ->
                                    new_simple_apply(St, Store, Data, Ctor,
                                                     Kind, Proto, Args);
                                _ -> {miss, St}
                            end;
                        _ -> {miss, St}
                    end;
                _ -> {miss, St}
            end;
        _ -> {miss, St}
    end;
t_new_simple(St, _, _) -> {miss, St}.

%% Inline `t_cell_new` (rt_store.gleam) + apply + return-override. The
%% arity guard lets the compiler fold the store update into one tuple build.
%% A simple variant of matching arity takes `this` positionally (it never
%% reads new.target); otherwise the Frame carries NewTarget = Ctor.
new_simple_apply(St, Store, Data, Ctor, {_, Code, Home, _, _, Simple, _, _, _},
                 Proto, Args)
  when tuple_size(Store) =:= ?STORE_ARITY ->
    NewSlot = {?SSHAPED_TAG, 0, {?SOME, Proto}, {}},
    NewId = element(?STORE_NEXT, Store),
    Store2 = setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(NewId, NewSlot, Data)),
    Store3 = setelement(?STORE_NEXT, Store2, NewId + 1),
    Store4 = setelement(?STORE_ALLOC, Store3, element(?STORE_ALLOC, Store) + 1),
    St2 = setelement(?AGENT_STORE, St, Store4),
    NewThis = {?HANDLE_TAG, NewId},
    {V, St3} = case Simple of
        {?SOME, {CodeT, Arity, true}} when length(Args) =:= Arity ->
            apply_this(CodeT, St2, NewThis, Args);
        {?SOME, {CodeS, Arity, false}} when length(Args) =:= Arity ->
            erlang:apply(CodeS, [St2 | Args]);
        _ -> Code(St2, {NewThis, Ctor, home(Home), Ctor}, Args)
    end,
    case V of
        {?HANDLE_TAG, _} -> {V, St3};
        _ -> {NewThis, St3}
    end.
