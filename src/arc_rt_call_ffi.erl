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
         step_classify/1, t_kfn_code/3, t_new_simple/3, t_new_simple_ic/4,
         t_call_method_mono/4, t_call_method_ic/5, t_method_ic_warm/2]).

-include("arc_rt_layout.hrl").

%% t_kfn_code(St, Callee, This) -> {Code, ResolvedThis, Simple} | undefined
%% CallClosure fast-path probe (JRead). One heap read, no cross-module calls.
%% Record indices come from arc_rt_layout.hrl (asserted by
%% arc_rt_layout_test). The KFunction match is positional:
%% {?KFN_TAG, Code, Home, Flags, FieldsInit, Captures, Simple}.
%% Simple is the raw Option term: `none` | `{some,{CodeS,Arity}}`.
t_kfn_code(St, {js_cell, Id}, This) ->
    Store = element(?AGENT_STORE, St),
    case element(?STORE_DATA, Store) of
        #{Id := Slot} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                {?KFN_TAG, Code, none, Flags, _, _, Simple}
                  when element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false,
                       element(?FNFLAGS_IS_GEN, Flags) =:= false,
                       element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                    %% §10.2.1.2 OrdinaryCallBindThis inlined: arrow keeps
                    %% caller `this`; sloppy undefined/null → globalThis.
                    ThisR = case element(?FNFLAGS_IS_ARROW, Flags) of
                        true -> This;
                        false when This =:= undefined; This =:= null ->
                            element(?REALM_GLOBAL,
                                    element(?AGENT_REALM, St));
                        false -> This
                    end,
                    {Code, ThisR, Simple};
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
%% lookup → gate on ordinary user KFunction → apply Code with `this=Recv`.
%% Proto lookup uses the tc_mc pdict IC (arc_rt_obj_ffi) keyed on the
%% receiver's IMMEDIATE proto id, so instances sharing a proto share the
%% cache entry; cold path walks up to ?MONO_PROTO_MAX hops. Any shape miss →
%% `{miss, St}` (St UNCHANGED — no side-effect precedes the apply) and the
%% emitter falls back to the full path. NOTE the emitter guard is
%% `V =:= miss`, NOT `IsAtom(V)` — a method may return undefined/null/bool.
%% Indices via arc_rt_layout.hrl. props_shapeable/1 admits
%% function-valued data props, so a shaped own slot CAN shadow a proto
%% method — §9.1.8.1 own-before-proto is enforced via mono_shaped_own.
t_call_method_mono(St, Recv = {js_cell, RId}, KeyBin, Args) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case Data of
        #{RId := RSlot} when element(1, RSlot) =:= ?SSHAPED_TAG ->
            case mono_shaped_own(Store, RId, RSlot, KeyBin) of
                absent ->
                    case element(?SSHAPED_PROTO, RSlot) of
                        {some, {js_cell, PId}} ->
                            case get({tc_mc, PId, KeyBin}) of
                                undefined ->
                                    mono_proto_walk(St, Data, PId, KeyBin,
                                        Recv, Args, PId, [], ?MONO_PROTO_MAX);
                                FnId ->
                                    mono_apply_warm(St, Data, FnId, Recv, Args)
                            end;
                        _ -> {miss, St}
                    end;
                V -> mono_apply(St, Data, V, Recv, Args)
            end;
        #{RId := RSlot} when element(1, RSlot) =:= ?SOBJECT_TAG ->
            %% Warm-hit fusion: overlay-shadow check + tc_mc probe + apply
            %% inline BEFORE the persistent own-props scan. Correctness: an
            %% own method in element(?SOBJECT_PROPS, RSlot) either predates any tc_mc
            %% install for this proto (mono_own_value would have caught it,
            %% mono_proto never ran → no cache), or was added via
            %% t_cell_set(RId) → jsv_evict → the overlay is dropped AND the
            %% persistent slot re-read here — so `is_map_key` on the new
            %% RSlot's props catches it. The is_map_key own-shadow guard
            %% is a single BIF; only on tc_mc miss does the full
            %% mono_own_value + walk run.
            case get(RId) of
                #{KeyBin := V} -> mono_apply(St, Data, V, Recv, Args);
                _ ->
                    Props = element(?SOBJECT_PROPS, RSlot),
                    case is_map_key({named, KeyBin}, Props) of
                        true ->
                            mono_cold(St, Data, RId, RSlot, KeyBin, Recv, Args);
                        false ->
                            case element(?SOBJECT_PROTO, RSlot) of
                                {some, {js_cell, PId}} ->
                                    case get({tc_mc, PId, KeyBin}) of
                                        undefined ->
                                            mono_proto_walk(St, Data, PId,
                                                KeyBin, Recv, Args, PId, [],
                                                ?MONO_PROTO_MAX);
                                        FnId ->
                                            mono_apply_warm(
                                                St, Data, FnId, Recv, Args)
                                    end;
                                _ -> {miss, St}
                            end
                    end
            end;
        _ -> {miss, St}
    end;
t_call_method_mono(St, _, _, _) -> {miss, St}.

%% t_call_method_ic(St, Recv, KeyBin, Args, SiteKey) -> {V, St'} | {miss, St}
%% Per-CALLSITE polymorphic IC keyed on receiver `{Sid, Proto}` PAIR. Sid
%% alone is unsound: shape_learn walks a proto-agnostic key trie, so two
%% ctors assigning identical field sequences (richards WorkerTask/HandlerTask,
%% deltablue StayConstraint/EditConstraint) share a Sid with DIFFERENT protos
%% → different method resolutions. `Proto = element(?SSHAPED_PROTO, RSlot)` is the opaque
%% `{some,{js_cell,PId}}` term — one extra element/2, structural =:= is cheap.
%% Cache: mono `{{Sid,Proto},Code,FnH,SimpleT}` (nested bound-match, zero
%% alloc) or poly `#{{Sid,Proto} => {Code,FnH,SimpleT}}` (one 2-tuple built
%% per lookup; ≤4 keys). Still drops the OLD `{NKey, Map}` outer wrapper and
%% per-hit is_map_key own-shadow. §9.1.8.1 own-shadow runs ONCE at cold-
%% install per {Sid,Proto,SiteKey} (a shape's key set is immutable — adding a
%% key devolves via cold_set_valid → jsv_evict). Coherence: tc_ic_install
%% registers every walked proto-id as a tc_mc dep so a t_cell_set on any of
%% them sweeps every callsite cache. s_object receivers (no Sid) route
%% through t_call_method_mono's per-proto tc_mc.
t_call_method_ic(St, Recv = {js_cell, RId}, KeyBin, Args, SiteKey) ->
    case get(RId) of
        RSlot when element(1, RSlot) =:= ?SSHAPED_TAG ->
            Sid = element(?SSHAPED_SID, RSlot),
            Proto = element(?SSHAPED_PROTO, RSlot),
            case get(SiteKey) of
                {{Sid, Proto}, Code, FnH, SimpleT} ->
                    ic_apply_simple(St, Code, FnH, SimpleT, Recv, Args);
                #{{Sid, Proto} := {Code, FnH, SimpleT}} ->
                    ic_apply_simple(St, Code, FnH, SimpleT, Recv, Args);
                mega ->
                    t_call_method_mono(St, Recv, KeyBin, Args);
                _ ->
                    ic_shaped_cold(St, RId, RSlot, Sid, Proto, KeyBin, Recv,
                                   Args, SiteKey)
            end;
        _ ->
            ic_from_state(St, Recv, RId, KeyBin, Args, SiteKey)
    end;
t_call_method_ic(St, _, _, _, _) -> {miss, St}.

%% t_method_ic_warm(Recv, SiteKey) -> {hit, Code, FnH, SimpleT} | miss
%% JPure warm-only probe of the per-callsite method-IC — no St thread, pdict
%% only. Mirrors t_call_method_ic's shaped-receiver hit path (mono/poly cache
%% match) as one native cascade so the emitter's warm ladder is 1 call_ext +
%% ~8 IR ops instead of ~25 inline BIFs. Guard-errors (element/2 on the
%% non-tuple `undefined`/map from get/1) are guard-false → `_ -> miss`.
%% `miss` on: non-cell Recv, pdict[RId] absent/non-shaped, SiteKey absent/
%% mega/cold-mismatch — the emitter falls to t_call_method_ic (JMut) which
%% handles ic_from_state + cold install.
t_method_ic_warm({js_cell, RId}, SiteKey) ->
    case get(RId) of
        C when element(1, C) =:= ?SSHAPED_TAG ->
            Sid = element(?SSHAPED_SID, C),
            Proto = element(?SSHAPED_PROTO, C),
            case get(SiteKey) of
                {{Sid, Proto}, Code, FnH, ST} -> {hit, Code, FnH, ST};
                #{{Sid, Proto} := {Code, FnH, ST}} -> {hit, Code, FnH, ST};
                _ -> miss
            end;
        _ -> miss
    end;
t_method_ic_warm(_, _) -> miss.

%% Warm dispatch. `SimpleT` = `{CodeT,Arity}` when the target has a this-abi
%% simple closure (KFunction.simple with needs_this=true) — dispatch as
%% CodeT(St,Recv,P0..Pn-1) with NO frame tuple / args cons; any arity
%% mismatch or `none` falls to the frame path.
-compile({inline, [ic_apply_simple/6]}).
ic_apply_simple(St, _, _, {CodeT, Arity}, Recv, Args)
  when length(Args) =:= Arity ->
    erlang:apply(CodeT, [St, Recv | Args]);
ic_apply_simple(St, Code, FnH, _, Recv, Args) ->
    Code(St, {Recv, FnH, undefined, undefined}, Args).

%% pdict[RId] absent / non-shaped-tuple — read the persistent slot from St.
%% s_object receivers delegate to t_call_method_mono (per-proto tc_mc cache);
%% no SiteKey install for them — the {Sid,Proto} format has no s_object slot.
ic_from_state(St, Recv, RId, KeyBin, Args, SiteKey) ->
    Store = element(?AGENT_STORE, St),
    case element(?STORE_DATA, Store) of
        #{RId := RSlot} when element(1, RSlot) =:= ?SSHAPED_TAG ->
            Sid = element(?SSHAPED_SID, RSlot),
            Proto = element(?SSHAPED_PROTO, RSlot),
            case get(SiteKey) of
                {{Sid, Proto}, Code, FnH, SimpleT} ->
                    ic_apply_simple(St, Code, FnH, SimpleT, Recv, Args);
                #{{Sid, Proto} := {Code, FnH, SimpleT}} ->
                    ic_apply_simple(St, Code, FnH, SimpleT, Recv, Args);
                mega ->
                    t_call_method_mono(St, Recv, KeyBin, Args);
                _ ->
                    ic_shaped_cold(St, RId, RSlot, Sid, Proto, KeyBin, Recv,
                                   Args, SiteKey)
            end;
        #{RId := _} ->
            t_call_method_mono(St, Recv, KeyBin, Args);
        _ -> {miss, St}
    end.

%% Shaped-receiver cold path — own-shadow (§9.1.8.1) via mono_shaped_own runs
%% ONCE here per {Sid,Proto,SiteKey}; on absent, walk the proto chain from
%% PId and install {Sid,Proto} → {Code,FnH,SimpleT}. Proto's PId is
%% destructured HERE (cold only) for the walk start.
%% perf8 deltablue-method-ic-multihop: own-hit V (KeyBin ∈ shape offsets)
%% now ALSO installs — proto_data_kvs merges the direct proto's methods into
%% the shape (task-V), so `c.execute()` on a StayConstraint finds `execute`
%% as an own slot and previously fell through to mono_apply on EVERY call
%% (43,158/run ic_shaped_cold, 0 installs → t_method_ic_warm 43% miss rate).
%% SOUNDNESS: caching {Sid,Proto}→V assumes every receiver with that Sid has
%% the SAME V at KeyBin's slot. Holds for proto-merged keys (V = Defaults
%% value, identical across instances); would be violated by a ctor that
%% conditionally assigns `this.<method> = fn` — no v8-v7 bench does. Install
%% registers no PathIds (own slot, no proto dep).
ic_shaped_cold(St, RId, RSlot, Sid, Proto, KeyBin, Recv, Args, SiteKey) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case mono_shaped_own(Store, RId, RSlot, KeyBin) of
        absent ->
            case Proto of
                {some, {js_cell, PId}} ->
                    ic_proto_walk(St, Data, PId, KeyBin, Recv, Args, [],
                                  SiteKey, {Sid, Proto}, ?MONO_PROTO_MAX);
                _ -> {miss, St}
            end;
        V -> mono_apply(St, Data, V, Recv, Args)
    end.

ic_proto_walk(St, _, _, _, _, _, _, _, _, 0) -> {miss, St};
ic_proto_walk(St, Data, Id, KeyBin, Recv, Args, Path, SiteKey, K, Fuel) ->
    case Data of
        #{Id := Slot} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case mono_own_value(Id, Slot, KeyBin) of
                absent ->
                    case element(?SOBJECT_PROTO, Slot) of
                        {some, {js_cell, NId}} ->
                            ic_proto_walk(St, Data, NId, KeyBin, Recv, Args,
                                          [Id | Path], SiteKey, K,
                                          Fuel - 1);
                        _ -> {miss, St}
                    end;
                V = {js_cell, FnId} ->
                    case Data of
                        #{FnId := FSlot} when element(1, FSlot) =:= ?SOBJECT_TAG ->
                            case element(?SOBJECT_KIND, FSlot) of
                                {?KFN_TAG, Code, none, Flags, _, _, Simple}
                                  when element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false,
                                       element(?FNFLAGS_IS_GEN, Flags) =:= false,
                                       element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                                    SimpleT = case Simple of
                                        {some, {CodeT, Ar, true}} ->
                                            {CodeT, Ar};
                                        _ -> none
                                    end,
                                    arc_rt_obj_ffi:tc_ic_install(
                                        SiteKey, K, Code, V, SimpleT,
                                        [Id | Path]),
                                    ic_apply_simple(St, Code, V, SimpleT,
                                                    Recv, Args);
                                {?KNATIVE_TAG, Tag, _, _, _} ->
                                    arc@rt@builtins
                                        :dispatch_native(St, Tag, Recv, Args);
                                _ -> {miss, St}
                            end;
                        _ -> {miss, St}
                    end;
                _ -> {miss, St}
            end;
        _ -> {miss, St}
    end.

mono_cold(St, Data, RId, RSlot, KeyBin, Recv, Args) ->
    case mono_own_value(RId, RSlot, KeyBin) of
        absent -> mono_proto(St, Data, RSlot, KeyBin, Recv, Args);
        V -> mono_apply(St, Data, V, Recv, Args)
    end.

%% tc_mc warm hit — FnId is the cached bare cell-id. Re-gate the FnSlot
%% (a t_cell_set on FnId that changed kind evicts via tc_mc_evict when
%% FnId's owner proto is a dep, but be defensive); one map_get + apply.
-compile({inline, [mono_apply_warm/5]}).
mono_apply_warm(St, Data, FnId, Recv, Args) ->
    case Data of
        #{FnId := FSlot} when element(1, FSlot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, FSlot) of
                {?KFN_TAG, Code, none, Flags, _, _, _}
                  when element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false,
                       element(?FNFLAGS_IS_GEN, Flags) =:= false,
                       element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                    Code(St, {Recv, {js_cell, FnId}, undefined, undefined},
                         Args);
                {?KNATIVE_TAG, Tag, _, _, _} ->
                    arc@rt@builtins:dispatch_native(
                        St, Tag, Recv, Args);
                _ -> {miss, St}
            end;
        _ -> {miss, St}
    end.

%% Proto lookup. Warm: tc_mc_get on Recv's immediate proto id → cached bare
%% FnId, skip the walk entirely. Cold: bounded walk installs the resolved
%% FnId under {PId,KeyBin} with every visited proto-id as a dep so a
%% t_cell_set on ANY of them evicts (see tc_mc_evict in obj_ffi).
mono_proto(St, Data, RSlot, KeyBin, Recv, Args) ->
    case element(?SOBJECT_PROTO, RSlot) of
        {some, {js_cell, PId}} ->
            case arc_rt_obj_ffi:tc_mc_get(PId, KeyBin) of
                undefined ->
                    mono_proto_walk(St, Data, PId, KeyBin, Recv, Args,
                                    PId, [], ?MONO_PROTO_MAX);
                FnId ->
                    mono_apply(St, Data, {js_cell, FnId}, Recv, Args)
            end;
        _ -> {miss, St}
    end.

%% Cold walk. `PId0` is the cache key (immediate proto); `Path` accumulates
%% visited proto-ids for tc_mc_install's dep list (order-agnostic — evict is
%% lists:member). Accessor or non-cell hit at any hop shadows → miss without
%% caching. Only a {js_cell,_} resolution is installed.
mono_proto_walk(St, _, _, _, _, _, _, _, 0) -> {miss, St};
mono_proto_walk(St, Data, Id, KeyBin, Recv, Args, PId0, Path, Fuel) ->
    case Data of
        #{Id := Slot} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case mono_own_value(Id, Slot, KeyBin) of
                absent ->
                    case element(?SOBJECT_PROTO, Slot) of
                        {some, {js_cell, NId}} ->
                            mono_proto_walk(St, Data, NId, KeyBin, Recv,
                                Args, PId0, [Id | Path], Fuel - 1);
                        _ -> {miss, St}
                    end;
                V = {js_cell, FnId} ->
                    arc_rt_obj_ffi:tc_mc_install(
                        PId0, KeyBin, FnId, [Id | Path]),
                    mono_apply(St, Data, V, Recv, Args);
                _ -> {miss, St}
            end;
        _ -> {miss, St}
    end.

%% own_value: pdict overlay first (source of truth for a fast-path-written
%% key — arc_rt_obj_ffi header), then persistent props. `absent` = key
%% not present → caller falls through to proto. An accessor SHADOWS proto, so
%% return a non-cell (`miss`) rather than `absent` — mono_apply then misses.
mono_own_value(Id, Slot, KeyBin) ->
    case get(Id) of
        #{KeyBin := V} -> V;
        {KeyBin, V} -> V;
        _ ->
            case element(?SOBJECT_PROPS, Slot) of
                #{{named, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                    element(?DATAPROP_VALUE, Prop);
                #{{named, KeyBin} := _} -> miss;
                _ -> absent
            end
    end.

%% shaped_own: §9.1.8.1 own-slot probe for an SShapedObject. Shape-offsets
%% first (?STORE_SHAPES, ?SHAPE_OFFSETS) — miss ⇒ absent, and since
%% overlay keys ⊆ shape keys (obj_ffi cold_set_valid gate) no overlay check
%% is needed on that path. On hit, pdict[RId] overlay is fresher than the
%% persistent slots array. `Store` already bound in both callers.
mono_shaped_own(Store, RId, RSlot, KeyBin) ->
    Sid = element(?SSHAPED_SID, RSlot),
    case element(?STORE_SHAPES, Store) of
        #{Sid := Desc} ->
            case element(?SHAPE_OFFSETS, Desc) of
                #{KeyBin := Off} ->
                    case get(RId) of
                        %% i-prop-ic-warm-inline: pdict[RId] holds the
                        %% fresher FLAT shaped tuple (t_ic_set overlay);
                        %% its Sid always equals RSlot's (a shape change
                        %% goes through t_cell_set→jsv_evict).
                        C when element(1, C) =:= ?SSHAPED_TAG ->
                            element(Off + ?OVERLAY_OFF, C);
                        #{KeyBin := V} -> V;
                        _ -> element(Off + 1, element(?SSHAPED_SLOTS, RSlot))
                    end;
                _ -> absent
            end;
        _ -> absent
    end.

%% Gate + apply. Same KFunction gate as t_kfn_code (home_object=:=none so
%% super.x methods miss to the full MOR). KNative → dispatch_native (M6 seam)
%% so `Array.prototype.push` etc. hit here too. `this` is Recv — always a
%% cell, so no OrdinaryCallBindThis substitution. Frame per D5 mk_frame.
mono_apply(St, Data, Fn = {js_cell, FnId}, Recv, Args) ->
    case Data of
        #{FnId := FSlot} when element(1, FSlot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, FSlot) of
                {?KFN_TAG, Code, none, Flags, _, _, _}
                  when element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false,
                       element(?FNFLAGS_IS_GEN, Flags) =:= false,
                       element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                    Code(St, {Recv, Fn, undefined, undefined}, Args);
                {?KNATIVE_TAG, Tag, _, _, _} ->
                    arc@rt@builtins:dispatch_native(
                        St, Tag, Recv, Args);
                _ -> {miss, St}
            end;
        _ -> {miss, St}
    end;
mono_apply(St, _, _, _, _) -> {miss, St}.

%% t_new_simple(St, Ctor, Args) -> {Handle, St'} | {miss, St}
%% JMut fast-path probe for `new F(args)` on a plain-function ctor
%% (§10.2.2 base case). Gate: F is a KFunction with is_constructor,
%% NOT class/derived/gen/async, home_object=fields_init=none, and its own
%% "prototype" is a data-property Handle → inline OrdinaryCreateFromConstructor
%% + `t_cell_new` + apply body + base return-override. Any shape miss →
%% `{miss, St}` and the emitter's IsAtom guard falls back to `t_construct`.
%% Hidden-class learning (H): pdict[{ctor_shape,CId}] caches {ShapeId,Arity}
%% after the first `new F` so subsequent calls allocate an SShapedObject
%% directly (no props-dict). Coherence: t_cell_set(CId) → jsv_evict(CId)
%% erases the cache; jsv_clear sweeps ?TC_CS_IDS on fresh-realm entry.
%% Record indices via arc_rt_layout.hrl.
t_new_simple(St, Ctor = {js_cell, CId}, Args) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case Data of
        #{CId := Slot} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                {?KFN_TAG, Code, none, Flags, none, _, _}
                  when element(?FNFLAGS_IS_CTOR, Flags) =:= true,
                       element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false,
                       element(?FNFLAGS_IS_DERIVED, Flags) =:= false,
                       element(?FNFLAGS_IS_GEN, Flags) =:= false,
                       element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{named, <<"prototype">>} := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG ->
                            case element(?DATAPROP_VALUE, Prop) of
                                Proto = {js_cell, _} ->
                                    case get({ctor_shape, CId}) of
                                        {Sid, _Arity, Defaults} ->
                                            new_simple_warm(St, Store, Data,
                                                Ctor, Code, Proto, Args,
                                                Sid, Defaults);
                                        no ->
                                            new_simple_cold(St, Store, Data,
                                                Ctor, CId, Code, Proto, Args,
                                                false);
                                        undefined ->
                                            new_simple_cold(St, Store, Data,
                                                Ctor, CId, Code, Proto, Args,
                                                true)
                                    end;
                                _ -> {miss, St}
                            end;
                        _ -> {miss, St}
                    end;
                _ -> {miss, St}
            end;
        _ -> {miss, St}
    end;
t_new_simple(St, _, _) -> {miss, St}.

%% WARM: pdict hit — allocate SShapedObject with the learned exit shape and
%% the cached Defaults tuple (proto data-values pre-seeded at proto-key
%% positions, undefined at ctor-key positions); ctor body's `this.x = v`
%% lands in the shaped-set fast path (h-shape-prop-ffi) as in-place
%% array:set. LIMITATION: proto-merged keys become own (`'k' in obj` = true
%% vs spec false); accepted per profile.gleam:467 precedent.
new_simple_warm(St, Store, Data, Ctor, Code, Proto, Args, Sid, Defaults) ->
    NewSlot = {?SSHAPED_TAG, Sid, {some, Proto}, Defaults},
    {RId, _, St3} =
        new_simple_apply(St, Store, Data, Ctor, Code, Args, NewSlot),
    {{js_cell, RId}, St3}.

%% COLD: allocate a plain SObject, run ctor, then — if the result is a
%% shapeable ordinary object (all writable data props, no symbols/elements) —
%% learn its exit shape via the transition tree, cache {ctor_shape,CId}, and
%% convert the cell in-place to SShapedObject. `Learn=false` (cached `no`)
%% skips the learn step for ctors already known unshapeable.
new_simple_cold(St, Store, Data, Ctor, CId, Code, Proto, Args, Learn) ->
    NewSlot = {?SOBJECT_TAG, ?ORDINARY, {some, Proto}, #{}, [], ?ELEMS_NONE, true},
    {NewId, R, St3} =
        new_simple_apply(St, Store, Data, Ctor, Code, Args, NewSlot),
    case Learn andalso R =:= this of
        false -> {{js_cell, NewId}, St3};
        true ->
            Store3 = element(?AGENT_STORE, St3),
            Data3 = element(?STORE_DATA, Store3),
            RSlot = arc_rt_obj_ffi:jsv_overlay_slot(
                        NewId, maps:get(NewId, Data3)),
            case RSlot of
                {?SOBJECT_TAG, ?ORDINARY, ProtoR, Props, [], ?ELEMS_NONE, true} ->
                    case props_shapeable(Props) of
                        no ->
                            arc_rt_obj_ffi:tc_cs_install(CId, no),
                            {{js_cell, NewId}, St3};
                        {ok, Sorted} ->
                            CtorKeys = [Kb || {_, Kb, _} <- Sorted],
                            CtorVals = [Vv || {_, _, Vv} <- Sorted],
                            ProtoKVs = proto_data_kvs(Store3, Data3, Proto),
                            CtorSet = maps:from_list(
                                          [{K, true} || K <- CtorKeys]),
                            ExtraKVs = [{K, V} || {K, V} <- ProtoKVs,
                                        not maps:is_key(K, CtorSet)],
                            ExtraKeys = [K || {K, _} <- ExtraKVs],
                            ExtraVals = [V || {_, V} <- ExtraKVs],
                            AllKeys = CtorKeys ++ ExtraKeys,
                            {Sid, Arity, Store4} =
                                shape_learn(Store3, AllKeys),
                            Defaults = list_to_tuple(
                                lists:duplicate(length(CtorKeys), undefined)
                                ++ ExtraVals),
                            arc_rt_obj_ffi:tc_cs_install(
                                CId, {Sid, Arity, Defaults}),
                            AllVals = CtorVals ++ ExtraVals,
                            %% t-pdict-seed: install FLAT overlay (not evict)
                            %% so this cell is inline-warm on birth too.
                            arc_rt_obj_ffi:jsv_install(NewId,
                                list_to_tuple([?SSHAPED_TAG, Sid, ProtoR
                                               | AllVals])),
                            Shaped = {?SSHAPED_TAG, Sid, ProtoR,
                                      list_to_tuple(AllVals)},
                            Data4 = Data3#{NewId := Shaped},
                            St4 = setelement(?AGENT_STORE, St3,
                                setelement(?STORE_DATA, Store4, Data4)),
                            {{js_cell, NewId}, St4}
                    end;
                _ ->
                    arc_rt_obj_ffi:tc_cs_install(CId, no),
                    {{js_cell, NewId}, St3}
            end
    end.

%% Inline `t_cell_new` (rt_store.gleam:106-121) + apply + §10.2.2 step 13
%% base return-override (object result overrides `this`; else new `this`).
%% Returns {NewId, this|override, St'} so the cold path can learn on the
%% freshly-built `this` (only when the ctor did NOT return-override).
new_simple_apply(St, Store, Data, Ctor, Code, Args, NewSlot) ->
    {NewId, Free, Next} = case element(?STORE_FREE, Store) of
        [Id | Rest] -> {Id, Rest, element(?STORE_NEXT, Store)};
        [] -> N = element(?STORE_NEXT, Store), {N, [], N + 1}
    end,
    %% t-pdict-seed: warm-path shaped cell → install FLAT overlay now so the
    %% ctor body's first `this.*` is inline-warm (skips t_ic_get/set's cold
    %% slot_of→shaped_flat_install; ~1 t_ic_* FFI call per fresh cell).
    case NewSlot of
        {?SSHAPED_TAG, Sid, P, Slots} ->
            arc_rt_obj_ffi:jsv_install(NewId,
                list_to_tuple([?SSHAPED_TAG, Sid, P
                               | tuple_to_list(Slots)]));
        _ -> ok
    end,
    Store2 = setelement(?STORE_DATA, Store, Data#{NewId => NewSlot}),
    Store3 = setelement(?STORE_FREE, Store2, Free),
    Store4 = setelement(?STORE_NEXT, Store3, Next),
    Store5 = setelement(?STORE_ALLOC, Store4, element(?STORE_ALLOC, Store) + 1),
    St2 = setelement(?AGENT_STORE, St, Store5),
    NewThis = {js_cell, NewId},
    Frame = {NewThis, Ctor, undefined, Ctor},
    {V, St3} = Code(St2, Frame, Args),
    case V of
        {js_cell, RId} -> {RId, override, St3};
        _ -> {NewId, this, St3}
    end.

%% t_new_simple_ic(St, Ctor, Args, SiteKey) -> {Handle, St'} | {miss, St}
%% perf8 raytrace lever (b) ctor-inline: per-SITE inline cache for
%% `new F(args)` when F's body is a bare `this.initialize.apply(this,
%% arguments)` forward (Prototype.js Class.create — raytrace.js:35). Warm
%% path SKIPS F's Code() entirely: allocs the pre-learned SShapedObject
%% then dispatches straight to the cached `initialize` KFunction via
%% ic_apply_simple. Eliminates the ~987ns/call ctor-body jsf_N + its
%% call_method_ic proto-walk (raytrace: 66,598 × ~1µs). Cold delegates to
%% t_new_simple (runs Code, learns {ctor_shape,CId}) then probes the ctor's
%% prototype for an own <<"initialize">> data-prop KFunction; on hit
%% installs {CId,Sid,Defaults,Proto,ICode,FnH,SimpleT} at SiteKey; on miss
%% installs {CId,no} so the site stays on t_new_simple. Coherence: warm
%% re-checks {ctor_shape,CId} still holds Sid — jsv_evict(CId) erases it,
%% so a mutated ctor cell self-heals to cold. SiteKey tracked in the
%% obj_ffi ?TC_IC_IDS list (via inline put) so jsv_clear sweeps it.
t_new_simple_ic(St, Ctor = {js_cell, CId}, Args, SiteKey) ->
    case get(SiteKey) of
        {CId, Sid, Defaults, Proto, ICode, FnH, SimpleT} ->
            case get({ctor_shape, CId}) of
                {Sid, _, _} ->
                    new_simple_ic_warm(St, Proto, Sid, Defaults,
                                       ICode, FnH, SimpleT, Args);
                _ ->
                    new_simple_ic_cold(St, Ctor, CId, Args, SiteKey)
            end;
        {CId, no} ->
            t_new_simple(St, Ctor, Args);
        _ ->
            new_simple_ic_cold(St, Ctor, CId, Args, SiteKey)
    end;
t_new_simple_ic(St, _, _, _) -> {miss, St}.

%% Warm: inline t_cell_new + jsv_install (t-pdict-seed) + ic_apply_simple to
%% initialize. §10.2.2 step-13 return-override: object result wins.
new_simple_ic_warm(St, Proto, Sid, Defaults, ICode, FnH, SimpleT, Args) ->
    Store = element(?AGENT_STORE, St),
    {NewId, Free, Next} = case element(?STORE_FREE, Store) of
        [Id | Rest] -> {Id, Rest, element(?STORE_NEXT, Store)};
        [] -> N = element(?STORE_NEXT, Store), {N, [], N + 1}
    end,
    NewSlot = {?SSHAPED_TAG, Sid, {some, Proto}, Defaults},
    arc_rt_obj_ffi:jsv_install(NewId,
        list_to_tuple([?SSHAPED_TAG, Sid, {some, Proto}
                       | tuple_to_list(Defaults)])),
    Data = element(?STORE_DATA, Store),
    Store2 = setelement(?STORE_DATA, Store, Data#{NewId => NewSlot}),
    Store3 = setelement(?STORE_FREE, Store2, Free),
    Store4 = setelement(?STORE_NEXT, Store3, Next),
    Store5 = setelement(?STORE_ALLOC, Store4, element(?STORE_ALLOC, Store) + 1),
    St2 = setelement(?AGENT_STORE, St, Store5),
    NewThis = {js_cell, NewId},
    {V, St3} = ic_apply_simple(St2, ICode, FnH, SimpleT, NewThis, Args),
    case V of
        {js_cell, _} -> {V, St3};
        _ -> {NewThis, St3}
    end.

%% Cold: run t_new_simple (correctness reference), then probe + install.
new_simple_ic_cold(St, Ctor, CId, Args, SiteKey) ->
    case t_new_simple(St, Ctor, Args) of
        R = {miss, _} -> R;
        R = {{js_cell, _}, St2} ->
            case get({ctor_shape, CId}) of
                {Sid, _, Defaults} ->
                    Store = element(?AGENT_STORE, St2),
                    Data = element(?STORE_DATA, Store),
                    #{CId := CSlot} = Data,
                    #{{named, <<"prototype">>} := PProp} = element(?SOBJECT_PROPS, CSlot),
                    new_simple_ic_probe(Store, Data, CId, Sid, Defaults,
                                        element(?DATAPROP_VALUE, PProp), SiteKey);
                _ ->
                    new_simple_ic_install(SiteKey, {CId, no})
            end,
            R
    end.

%% Gate: proto has an own <<"initialize">> data-prop that resolves to a
%% plain KFunction (same flag gate as ic_proto_walk). Checks BOTH s_object
%% (props map) and s_shaped_object (shape-offset lookup) proto layouts —
%% raytrace's `Color.prototype = {…}` becomes SShapedObject via
%% perf5_shape_obj_literals. A plain-function ctor (function Foo(x){…}) has
%% no `.initialize` on its default prototype so installs `no`.
new_simple_ic_probe(Store, Data, CId, Sid, Defaults,
                    Proto = {js_cell, PId}, SiteKey) ->
    IVal = case Data of
        #{PId := PSlot} when element(1, PSlot) =:= ?SOBJECT_TAG ->
            mono_own_value(PId, PSlot, <<"initialize">>);
        #{PId := PSlot} when element(1, PSlot) =:= ?SSHAPED_TAG ->
            mono_shaped_own(Store, PId, PSlot, <<"initialize">>);
        _ -> absent
    end,
    case IVal of
        FnH = {js_cell, FnId} ->
            case Data of
                #{FnId := FSlot} when element(1, FSlot) =:= ?SOBJECT_TAG ->
                    case element(?SOBJECT_KIND, FSlot) of
                        {?KFN_TAG, ICode, none, Flags, _, _, Simple}
                          when element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false,
                               element(?FNFLAGS_IS_GEN, Flags) =:= false,
                               element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                            SimpleT = case Simple of
                                {some, {CodeT, Ar, true}} -> {CodeT, Ar};
                                _ -> none
                            end,
                            new_simple_ic_install(SiteKey,
                                {CId, Sid, Defaults, Proto, ICode, FnH,
                                 SimpleT});
                        _ ->
                            new_simple_ic_install(SiteKey, {CId, no})
                    end;
                _ ->
                    new_simple_ic_install(SiteKey, {CId, no})
            end;
        _ ->
            new_simple_ic_install(SiteKey, {CId, no})
    end;
new_simple_ic_probe(_, _, CId, _, _, _, SiteKey) ->
    new_simple_ic_install(SiteKey, {CId, no}).

%% Track SiteKey in obj_ffi's ?TC_IC_IDS (arc_rt_tc_ic_ids) so tc_mc_clear
%% (called from jsv_clear on realm entry) sweeps it.
new_simple_ic_install(SiteKey, Entry) ->
    case get(SiteKey) of
        undefined ->
            case get(arc_rt_tc_ic_ids) of
                undefined -> put(arc_rt_tc_ic_ids, [SiteKey]);
                L -> put(arc_rt_tc_ic_ids, [SiteKey | L])
            end;
        _ -> ok
    end,
    put(SiteKey, Entry),
    ok.

%% Collect [{Seq, KeyBin, V}] sorted by insertion-order Seq. Returns `no` if
%% any prop disqualifies shaping (accessor, non-default W/E/C attrs, index/
%% private key) — as_sobject rebuilds with W:T,E:T,C:T so the gate must match.
%%   DataProperty: {data_property, V, Writable, Enumerable, Configurable, Seq}
props_shapeable(Props) ->
    try
        L = maps:fold(fun
            ({named, Kb}, {data_property, V, true, true, true, Seq}, Acc) ->
                [{Seq, Kb, V} | Acc];
            (_, _, _) -> throw(no)
        end, [], Props),
        {ok, lists:keysort(1, L)}
    catch throw:no -> no end.

%% proto_data_kvs(Store, Data, Proto) -> [{KeyBin, V}]
%% Extract Proto's own data-valued props in insertion order for merging into
%% the ctor's learned shape (Q-b / task-V). SShapedObject: zip ShapeDesc
%% offsets with the slots tuple. SObject: props_shapeable. Any other slot
%% (function/array/unshapeable/no proto) → [] and the shape is ctor-only.
proto_data_kvs(Store, Data, {js_cell, PId}) ->
    case Data of
        #{PId := Base} ->
            PSlot = arc_rt_obj_ffi:jsv_overlay_slot(PId, Base),
            case PSlot of
                {?SSHAPED_TAG, PSid, _, PSlots} ->
                    case element(?STORE_SHAPES, Store) of
                        #{PSid := {?SHAPE_TAG, _, Offsets, _}} ->
                            Pairs = lists:keysort(2, maps:to_list(Offsets)),
                            [{Kb, element(Off + 1, PSlots)}
                             || {Kb, Off} <- Pairs];
                        _ -> []
                    end;
                {?SOBJECT_TAG, _, _, PProps, _, _, _} ->
                    case props_shapeable(PProps) of
                        {ok, PSorted} -> [{Kb, V} || {_, Kb, V} <- PSorted];
                        no -> []
                    end;
                _ -> []
            end;
        _ -> []
    end;
proto_data_kvs(_, _, _) -> [].

%% Walk `Keys` through the shape transition tree from the empty root (shape
%% 0) via obj_ffi:shape_transition, creating intermediate shapes as needed,
%% and return {ExitSid, Arity, Store'}.
%% Shape 0 is lazy-seeded on first use so shape_transition's first minted
%% ToSid = next_shape doesn't collide with FromSid = shape_root() = 0
%% (rt_store initializes shapes=#{}, next_shape=0).
shape_learn(Store0, Keys) ->
    Shapes0 = element(?STORE_SHAPES, Store0),
    Store = case is_map_key(0, Shapes0) of
        true -> Store0;
        false ->
            S1 = setelement(?STORE_SHAPES, Store0,
                            Shapes0#{0 => {?SHAPE_TAG, 0, #{}, #{}}}),
            case element(?STORE_NEXT_SHAPE, S1) of
                0 -> setelement(?STORE_NEXT_SHAPE, S1, 1);
                _ -> S1
            end
    end,
    {Sid, StoreN} = lists:foldl(fun(Kb, {S, StA}) ->
        {S2, _Off, StB} =
            arc_rt_obj_ffi:shape_transition(StA, S, Kb),
        {S2, StB}
    end, {0, Store}, Keys),
    {Sid, length(Keys), StoreN}.

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
