%%% arc_rt_obj_ffi — own-data-property fast-path probes for
%%% `rt_obj` (SPEC §7.M4; hot-path fixes #1/#2).
%%%
%%% Hand-written Erlang, so it carries the `arc_rt_` namespace prefix
%%% (overview §5) and can NEVER collide with an OTP module — exactly like
%%% `arc_rt_call_ffi`.
%%%
%%% Why a shim: the emitted `.x` / `.x = v` fast path wants a SINGLE probe
%%% for the common case (own writable DataProperty on an ordinary SObject)
%%% with NO cross-module `classify`/`as_object_key`/`t_get_prop` proto-walk
%%% chain. On any shape miss the atom `miss` is returned and the emitter's
%%% guard falls back to the full `t_get_prop_any` / `t_set_prop_any` path.
%%%
%%% Mutation model (obj≤20000µs target, bench-verify): a hit writes to a
%%% per-cell PROCESS-DICTIONARY overlay and returns `St` UNCHANGED (the
%%% persistent Agent rebuild floors `.x = v` at ~90ns; ≤20ns/iter
%%% is unreachable that way). The overlay is a POLYMORPHIC small map
%%% `pdict[Id] → #{KeyBin => V}` — every validated own-data key on the
%%% object is cached, so multi-field workloads (richards TaskControlBlock
%%% reads 6+ fields/iter) stay warm on every field, not just the first.
%%% BEAM small-maps (≤32 keys) are a flat key/value array, so `map_get` on
%%% the overlay is a linear scan of a handful of binaries — cheaper than
%%% the slot_of chain the old mono `{_,_}→miss` arm fell through to. The
%%% pdict key is the BARE integer cell-id (integer hash is ~free), so the
%%% emitted warm hit is inline `maps:get(Kb, erlang:get(element(2,H)), D)`
%%% via `ir.MapOp(MapGet)`; this module supplies the cold-path
%%% validate/install and the coherence hooks. TRANSITIONAL: every function
%%% below also accepts the legacy `{KeyBin, V}` tuple shape (still written
%%% by the pre-poly emitter's speculative `put` in `set_prop_fast`) and
%%% normalizes it to a map, so this file lands independently of the
%%% expr.gleam inline rewrite.
%%% `pdict[arc_rt_jsv_ids]` tracks every installed Id so `jsv_clear`/
%%% `jsv_flush` can enumerate them without a full pdict scan and without
%%% sweeping unrelated integer keys. Coherence with the general path is
%%% kept by the 3 choke points that see every non-fast-path slot access:
%%%   * `t_cell_get`  — checks `pdict[Id]` and calls `jsv_overlay_slot` so
%%%     the general path sees fast-path writes (arc_rt_store_ffi).
%%%   * `t_cell_set`  — calls `jsv_evict(Id)` before its persistent write
%%%     (rt_store.gleam), so a shape change re-forces validation.
%%%   * `t_collect`   — calls `jsv_flush/1` first so mark sees overlay
%%%     values as roots (rt_gc.gleam).
%%% `apply_js_main` clears the overlay on entry and flushes on exit so a
%%% re-applied seed observes an identical fresh realm.
-module(arc_rt_obj_ffi).
-export([t_get_prop_own_data/3, t_set_prop_own_data/4,
         t_set_prop_own_cold/4, t_instanceof_fast/3,
         t_get_elem_fast/3, t_get_elem_fast_p/3, t_set_elem_fast/4,
         t_set_elem_fast_p/4, t_arr_c_load/1, t_get_elem_fast_c/4,
         t_global_get_fast/2, t_global_handle/1,
         t_ic_get/4, t_ic_proto_get/4, t_ic_set/5, t_ic_warm_get/2,
         t_ic_warm_set/3,
         t_shaped_get/2, t_shaped_set/4,
         t_new_object_shaped/4,
         jsv_install/2, jsv_evict/1, jsv_clear/0, jsv_flush/1,
         jsv_overlay_slot/2,
         tc_mc_get/2, tc_mc_install/3, tc_mc_install/4,
         tc_ic_install/5, tc_ic_install/6,
         tc_cs_install/2,
         shape_offset/3, shape_transition/3,
         shape_desc/2, shape_root/0,
         shape_slots_get/2, shape_slots_fold/3]).

%% Record indices come from arc_rt_layout.hrl (asserted by
%% arc_rt_layout_test). Plain tuples indexed here:
%%   SShapedObject slots (plain tuple, arity = ShapeDesc.arity;
%%     element(Off+1, Slots))
%%   FLAT pdict overlay (perf5): {s_shaped_object,Sid,P,X0,…,Xn-1} — slots
%%     inlined from position ?OVERLAY_OFF; SiteKey caches OffF=Off+?OVERLAY_OFF.
%%   PropertyKey Named: {named, BinString}
%% Overlay pdict key: the bare integer cell-id `Id` — shared verbatim with
%% the emitted code's inline `erlang:get`/`put`. Installed Ids are tracked
%% in `pdict[arc_rt_jsv_ids]`.

-include("arc_rt_layout.hrl").

-define(IDS, arc_rt_jsv_ids).
-define(GID, arc_rt_jsv_gid).
-define(TC_MC_IDS, arc_rt_tc_mc_ids).
-define(TC_MC_DEPS, arc_rt_tc_mc_deps).
-define(TC_IC_IDS, arc_rt_tc_ic_ids).
-define(TC_CS_IDS, arc_rt_tc_cs_ids).
-define(TC_ARR_IDS, arc_rt_tc_arr_ids).
-define(TC_ARR_MIN_LEN, 8).
-define(SHAPE_OFF_IDS, arc_rt_shape_off_ids).

%% t_get_prop_own_data(St, {js_cell,Id}, KeyBin) -> V | miss
%% JRead cold-path probe. The emitted code's inline `maps:get(Kb, get(Id),
%% miss)` covers the warm hit; this is called on a warm miss (no cache /
%% key not yet in overlay / non-map pdict entry). Validates own
%% DataProperty on an Ordinary SObject (kind=:=ordinary avoids ArrayObj's
%% virtual "length") and EXTENDS the overlay map so the next read of this
%% key is warm. Any shape miss → `miss`. Legacy `{Kb,V}` tuple entries
%% (pre-poly emitter's speculative put) are normalized to a map in place.
t_get_prop_own_data(St, {js_cell, Id}, KeyBin) ->
    case get(Id) of
        C when element(1, C) =:= ?SSHAPED_TAG ->
            case shape_offset_cached(St, element(?SSHAPED_SID, C), KeyBin) of
                miss -> miss;
                Off -> element(Off + ?OVERLAY_OFF, C)
            end;
        M when is_map(M) ->
            case M of
                #{KeyBin := V} -> V;
                _ -> cold_get(St, Id, KeyBin, M)
            end;
        undefined -> cold_get(St, Id, KeyBin, #{});
        {KeyBin, V} -> V;
        {Kb, Ov} -> cold_get(St, Id, KeyBin, #{Kb => Ov})
    end;
t_get_prop_own_data(_, _, _) -> miss.

%% t_global_get_fast(St, KeyBin) -> V | miss
%% JRead global-var read fast path (richards: 41k/run t_global_get for
%% ID_*, STATE_*, KIND_* consts + ctor bindings — all own writable data
%% props on the Ordinary global object). Piggybacks on the poly overlay:
%% pdict[GId] caches every read key so hot loops warm-hit; coherence via
%% t_global_set → t_set_prop → t_cell_set(GId) → jsv_evict(GId). GId
%% itself is cached in pdict[?GID] (realm.global_object is fixed for a
%% run; jsv_clear on apply_js_main entry drops it) so the warm hit is a
%% single get(?GID) + get(GId) + map match.
t_global_get_fast(St, KeyBin) ->
    GId = case get(?GID) of
              undefined ->
                  Realm = element(?AGENT_REALM, St),
                  {js_cell, G} = element(?REALM_GLOBAL, Realm),
                  put(?GID, G), G;
              G -> G
          end,
    case get(GId) of
        #{KeyBin := V} -> V;
        M when is_map(M) -> cold_get(St, GId, KeyBin, M);
        undefined -> cold_get(St, GId, KeyBin, #{});
        {KeyBin, V} -> V;
        {Kb, Ov} -> cold_get(St, GId, KeyBin, #{Kb => Ov})
    end.

%% t_global_handle(St) -> {js_cell, GId}
%% JRead — the realm's global-object handle. Kept for a possible future
%% inlined-warm-hit emit; not on the hot path today.
t_global_handle(St) ->
    Realm = element(?AGENT_REALM, St),
    element(?REALM_GLOBAL, Realm).

%% cold_get — validate + install into the overlay for `Id`. `M0` is the
%% prior poly-map overlay (possibly `#{}`). Empty `M0` installs the MONO
%% `{KeyBin, V}` tuple (perf2 obj_prop contract: 1 get + 1 put/iter,
%% ~11ns hand-BEAM); a non-empty `M0` extends the poly map so a second key
%% EXTENDS rather than evicts and no un-flushed write is lost. The
%% mono→poly transition happens at the `{Kb, Ov}` normalise-to-map arm in
%% every caller, which passes `#{Kb => Ov}` (map_size 1) here.
cold_get(St, Id, KeyBin, M0) ->
    case peek_get(St, Id, KeyBin) of
        miss -> miss;
        V ->
            case map_size(M0) of
                0 -> jsv_install(Id, {KeyBin, V});
                _ -> jsv_install(Id, M0#{KeyBin => V})
            end,
            V
    end.

%% peek_get — own-data props-map lookup with NO pdict side-effect. Shared
%% by cold_get (which then installs) and the `{_,_}` second-key arm (which
%% must not, to preserve the other key's cached write).
peek_get(St, Id, KeyBin) ->
    case slot_of(St, Id) of
        {?SSHAPED_TAG, Sid, _, Slots} ->
            case shape_offset_cached(St, Sid, KeyBin) of
                miss -> miss;
                Off -> element(Off + 1, Slots)
            end;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG,
                  element(?SOBJECT_KIND, Slot) =:= ?ORDINARY ->
            case element(?SOBJECT_PROPS, Slot) of
                #{{named, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                    element(?DATAPROP_VALUE, Prop);
                _ -> miss
            end;
        _ -> miss
    end.

%% t_set_prop_own_data(St, {js_cell,Id}, KeyBin, V) -> ok | miss
%% JRead fast-path probe (St is READ for cold-path validation only; never
%% rebound). Warm hit — `KeyBin` already in the overlay map — is a
%% `get`+`maps:put`+`put` (3 BIFs; the poly emitter may inline this). A
%% key NOT yet in the map validates §10.1.9 OrdinarySet step 2.a (existing
%% own writable DataProperty) and EXTENDS the map on pass; the map is left
%% untouched on fail so no other key's un-flushed write is lost. Legacy
%% tuple entries are normalized to a map first.
t_set_prop_own_data(St, {js_cell, Id}, KeyBin, V) ->
    case get(Id) of
        C when element(1, C) =:= ?SSHAPED_TAG ->
            case shape_offset_cached(St, element(?SSHAPED_SID, C), KeyBin) of
                miss -> miss;
                Off ->
                    tc_mc_evict(Id),
                    put(Id, setelement(Off + ?OVERLAY_OFF, C, V)),
                    ok
            end;
        M when is_map(M) -> set_own_map(St, Id, KeyBin, V, M);
        undefined -> set_own_map(St, Id, KeyBin, V, #{});
        {Kb, Ov} -> set_own_map(St, Id, KeyBin, V, #{Kb => Ov})
    end;
t_set_prop_own_data(_, _, _, _) -> miss.

set_own_map(St, Id, KeyBin, V, M0) ->
    case is_map_key(KeyBin, M0) of
        true -> tc_mc_evict(Id), put(Id, M0#{KeyBin := V}), ok;
        false ->
            case cold_set_valid(St, Id, KeyBin) of
                true ->
                    tc_mc_evict(Id),
                    %% Mirror cold_get: fresh install → MONO `{KeyBin,V}`
                    %% tuple; extending an existing overlay stays poly-map.
                    case map_size(M0) of
                        0 -> jsv_install(Id, {KeyBin, V});
                        _ -> jsv_install(Id, M0#{KeyBin => V})
                    end,
                    ok;
                false -> miss
            end
    end.

%% t_set_prop_own_cold(St, H, KeyBin, Old) -> {ok, St'}
%% JMut cold-path companion for the (pre-poly) emitter's INLINED
%% speculative `erlang:put(Id, {KeyBin, V})`. `Old` is that put's return —
%% the pre-write overlay entry — and `V` is recovered from the speculative
%% tuple now at `pdict[Id]`. Normalize `Old` to a map, then: key already
%% present → warm, install merged map; key absent → validate + extend on
%% pass, else RESTORE `Old` (so no cached write is lost) and run the full
%% `t_set_prop_any` HERE so the emitter has no separate slow-path arm. The
%% poly emitter drops the speculative-tuple inline entirely and calls
%% `t_set_prop_own_data` instead, at which point this function is dead.
t_set_prop_own_cold(St, H = {js_cell, Id}, KeyBin, Old) ->
    {_, V} = get(Id),
    M0 = case Old of
             M when is_map(M) -> M;
             {Kb, Ov} -> #{Kb => Ov};
             _ -> #{}
         end,
    case is_map_key(KeyBin, M0) of
        true -> tc_mc_evict(Id), put(Id, M0#{KeyBin := V}), {ok, St};
        false ->
            case cold_set_valid(St, Id, KeyBin) of
                true ->
                    tc_mc_evict(Id), jsv_install(Id, M0#{KeyBin => V}),
                    {ok, St};
                false ->
                    case Old of
                        undefined -> erase(Id);
                        _ -> put(Id, Old)
                    end,
                    slow_set(St, H, KeyBin, V)
            end
    end;
%% Non-handle receiver: the emitter's `is_tuple ∧ element(1) =:= js_cell`
%% guard routes bigint/symbol/primitive receivers to slow `set_prop` BEFORE
%% the speculative put, so this arm is unreachable from the inline path.
%% `{miss, St}` shape for JMut consistency.
t_set_prop_own_cold(St, _, _, _) -> {miss, St}.

slow_set(St, H, KeyBin, V) ->
    'arc@rt@obj':t_set_prop_any(
        St, H, {string_key, {named, KeyBin}}, V).

%% Validate-only variant of the cold set — no pdict side-effect; caller
%% decides keep-or-erase.
cold_set_valid(St, Id, KeyBin) ->
    case slot_of(St, Id) of
        {?SSHAPED_TAG, Sid, _, _} ->
            %% Shaped slots are all writable-data by construction; key ∈
            %% shape → overlay-set ok. Key ∉ shape → miss so slow-path
            %% devolves (h-shape-slowpath-compat) rather than transitioning
            %% here — keeps this JRead.
            shape_offset_cached(St, Sid, KeyBin) =/= miss;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG,
                  element(?SOBJECT_KIND, Slot) =:= ?ORDINARY ->
            case element(?SOBJECT_PROPS, Slot) of
                #{{named, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG,
                       element(?DATAPROP_WRITABLE, Prop) =:= true -> true;
                _ -> false
            end;
        _ -> false
    end.

%% t_instanceof_fast(St, V, Ctor) -> 0 | 1 | miss
%% NOT on the v8-v7 hot path — richards/deltablue/base contain zero
%% `instanceof` (grep -c = 0); kept as a general JRead probe only, NOT a
%% richards-goal contributor. JRead fast-path for §13.10.2
%% InstanceofOperator → §7.3.22 OrdinaryHasInstance. Gate: `Ctor` is an
%% s_object with `k_function` kind
%% (NOT k_bound / proxy) and empty own `symbol_props` (element 5) — so no
%% own @@hasInstance override; the inherited Function.prototype
%% [@@hasInstance] IS OrdinaryHasInstance, which this inlines — holding an
%% own "prototype" DataProperty whose value is a cell `{js_cell, PId}`.
%% Then walk `V`'s proto chain (element 3) comparing cell-ids to `PId`,
%% depth-capped at 64 hops → miss so a proxy-cycle falls to the full path's
%% RangeError. Non-cell `V` → 0 (§7.3.22 step 3). Any other shape → `miss`
%% and the emitter falls back to `t_instance_of`. No overlay concern: kind
%% / symbol_props / proto are never overlaid, and the own-data overlay only
%% installs on `ordinary`-kind slots — never on a KFunction ctor.
t_instanceof_fast(St, V, {js_cell, CId}) ->
    case slot_of(St, CId) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG,
                  element(?SOBJECT_SYMBOL_PROPS, Slot) =:= [] ->
            case element(?SOBJECT_KIND, Slot) of
                Kind when element(1, Kind) =:= ?KFN_TAG ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{named, <<"prototype">>} := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG ->
                            case element(?DATAPROP_VALUE, Prop) of
                                {js_cell, PId} -> proto_has(St, V, PId, 64);
                                _ -> miss
                            end;
                        _ -> miss
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
t_instanceof_fast(_, _, _) -> miss.

%% §7.3.22 step 7 chain walk — reads proto (element 3) only, so no overlay
%% read needed. Fuel exhaustion on a `{js_cell,_}` V → miss (clause 2);
%% non-cell V (bigint / symbol / primitive) → 0 (clause 3, step 3).
proto_has(St, {js_cell, VId}, PId, Fuel) when Fuel > 0 ->
    case slot_of(St, VId) of
        %% proto is element 3 for BOTH s_object and s_shaped_object.
        Slot when element(1, Slot) =:= ?SOBJECT_TAG;
                  element(1, Slot) =:= ?SSHAPED_TAG ->
            case element(?SOBJECT_PROTO, Slot) of
                none -> 0;
                {some, {js_cell, PId}} -> 1;
                {some, {js_cell, Next}} ->
                    proto_has(St, {js_cell, Next}, PId, Fuel - 1);
                _ -> miss
            end;
        _ -> miss
    end;
proto_has(_, {js_cell, _}, _, _) -> miss;
proto_has(_, _, _, _) -> 0.

%% ──────────────────── indexed-element fast path ────────────────────
%% SPEC array-index-fast-path — deltablue OrderedCollection/Plan.execute()
%% inner loops read `this.elms[i]` on every iteration; the general path is
%% `to_property_key` (JMut, canonicalizes to {index,N}) → `t_get_prop_any`
%% (proto walk + kind dispatch). This inlines the ArrayObj Dense/Sparse
%% element read/write with a shape guard, `miss` on anything exotic.
%%   ObjKind ArrayObj: {array_obj, Length}.
%%   JsElements: no_elements | {dense, array:array()} | {sparse, #{Int=>V}}.
%% The named-prop pdict overlay is disjoint from indexed elements, so no
%% overlay read/evict is needed on either path.

%% t_get_elem_fast(St, Recv, Idx) -> V | miss
%% JRead. Gate: Recv={js_cell,Id}, Idx a bare non-negative BEAM integer (the
%% JsVal wire form for a JS integer number — a float / string / bigint index
%% falls to `to_property_key`), slot is ArrayObj with Idx < Length and no
%% {index,Idx} props override. Holes (dense default / sparse-absent) miss so
%% the full path handles the proto walk. `IsAtom` on the emitter side treats
%% any atom-valued V (undefined/true/…) as a miss too — a perf loss only.
%% slot_of + elem_read are inlined into a single case cascade — profiling
%% attributed 227µs+224µs (of 734µs total) to the two function-call hops
%% on richards' hot indexed-read path (SPEC k-elem-fast-flatten).
t_get_elem_fast(St, {js_cell, Id}, Idx)
  when is_integer(Idx), Idx >= 0 ->
    Store = element(?AGENT_STORE, St),
    case element(?STORE_DATA, Store) of
        #{Id := Slot} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                {?ARRAYOBJ_TAG, Length} when Idx < Length ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{index, Idx} := _} -> miss;
                        _ ->
                            case element(?SOBJECT_ELEMENTS, Slot) of
                                {?ELEMS_DENSE, A} ->
                                    case Idx < array:size(A) of
                                        true ->
                                            V = array:get(Idx, A),
                                            case V =:= array:default(A) of
                                                true -> miss;
                                                false -> V
                                            end;
                                        false -> miss
                                    end;
                                {?ELEMS_SPARSE, M} ->
                                    case M of
                                        #{Idx := V} -> V;
                                        _ -> miss
                                    end;
                                _ -> miss
                            end
                    end;
                _ -> miss
            end;
        %% s_shaped_object (named-field only) and any other slot
        %% shape fall through — miss defers to the general path.
        _ -> miss
    end;
t_get_elem_fast(_, _, _) -> miss.

%% t_get_elem_fast_p(St, Recv, Idx) -> V | miss
%% JRead (perf7_arr_pdict). Warm path reads `{tc_arr,Id}` overlay (1 pdict
%% get + inlined elem read); cold `undefined` is a direct tail-call to
%% t_get_elem_fast — zero duplicated Store cascade (SPEC arr-pdict-db-cost
%% (a)): deltablue's short-array reads pay pdict-get + t_get_elem_fast's
%% inlined cascade, no install-gate probe, no elem_read hop. Install is a
%% side-effect via tc_arr_cold_install (Length>=?TC_ARR_MIN_LEN gate) so
%% crypto's read-before-write arrays still go warm; t_set_elem_fast_p's cold
%% arm covers write-first arrays. Overlay is `{Length, {tuple_dense, T}}`
%% (perf8) — warm hit is ONE `element(Idx+1, T)`; a hole reads back
%% `undefined` which the emitter's IsAtom miss-guard already routes to slow
%% (get_elem_fast/expr.gleam:1925 — atom-valued hits conflate with `miss`).
t_get_elem_fast_p(St, {js_cell, Id} = Recv, Idx)
  when is_integer(Idx), Idx >= 0 ->
    case get({tc_arr, Id}) of
        undefined ->
            tc_arr_cold_install(St, Id),
            t_get_elem_fast(St, Recv, Idx);
        {Length, {tuple_dense, T}} when Idx < Length ->
            element(Idx + 1, T);
        {Length, {?ELEMS_DENSE, A}} when Idx < Length ->
            case Idx < array:size(A) of
                true ->
                    V = array:get(Idx, A),
                    case V =:= array:default(A) of
                        true -> miss;
                        false -> V
                    end;
                false -> miss
            end;
        {Length, {?ELEMS_SPARSE, M}} when Idx < Length ->
            case M of
                #{Idx := V} -> V;
                _ -> miss
            end;
        {_, _} -> miss
    end;
t_get_elem_fast_p(_, _, _) -> miss.

%% Read-side cold install probe (SPEC arr-pdict-db-cost (a)). One Store peek;
%% short-circuits on Length < ?TC_ARR_MIN_LEN so deltablue's 1-3 elem
%% OrderedCollection arrays skip the map_size/extensible checks. Install only
%% when no props at all AND extensible (§10.4.2.1: t_set_elem_fast_p's
%% Idx==Length append can't see element(7), so gate here). crypto's
%% BigInteger digit arrays are ~19-37 (CRT primes p,q ≈19; modulus n ≈37 at
%% dbits=28) so ?TC_ARR_MIN_LEN=8 keeps them warm.
tc_arr_cold_install(St, Id) ->
    Store = element(?AGENT_STORE, St),
    case element(?STORE_DATA, Store) of
        #{Id := Slot}
          when element(1, Slot) =:= ?SOBJECT_TAG,
               element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
            case element(?SOBJECT_KIND, Slot) of
                {?ARRAYOBJ_TAG, Length}
                  when Length >= ?TC_ARR_MIN_LEN ->
                    case map_size(element(?SOBJECT_PROPS, Slot)) of
                        0 -> tc_arr_install(
                               Id, Length, element(?SOBJECT_ELEMENTS, Slot));
                        _ -> ok
                    end;
                _ -> ok
            end;
        _ -> ok
    end.

%% t_set_elem_fast(St, Recv, Idx, V) -> St' | miss
%% JMutMiss. Gate: Recv={js_cell,Id} ArrayObj, Idx bare non-negative integer
%% in [0, Length) (in-bounds — no length update), no {index,Idx} props
%% override, extensible=true (covers hole-fill; overwrite-on-frozen is rare
%% and misses harmlessly). Dense set additionally requires Idx < array:size
%% to avoid an unbounded auto-extend. Writes elements storage in place;
%% named-prop overlay untouched. Returns the rebuilt St' (a tuple) on hit /
%% bare `miss` atom otherwise — the emitter's `is_atom` guard distinguishes
%% them without a {V,St'} 2-tuple alloc per hit.
t_set_elem_fast(St, {js_cell, Id}, Idx, V)
  when is_integer(Idx), Idx >= 0 ->
    %% perf7_arr_pdict coherence: this JMut path writes Store as truth AND
    %% write-throughs the overlay iff present (tc_arr_sync) — no erase, so
    %% crypto's read-write loop stays warm and ?TC_ARR_IDS never re-appends.
    %% Miss → slow path → t_cell_set → jsv_evict handles the erase.
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case Data of
        #{Id := Slot}
          when element(1, Slot) =:= ?SOBJECT_TAG,
               element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
            case element(?SOBJECT_KIND, Slot) of
                {?ARRAYOBJ_TAG, Length} when Idx < Length ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{index, Idx} := _} -> miss;
                        _ ->
                            case elem_write(element(?SOBJECT_ELEMENTS, Slot), Idx, V) of
                                miss -> miss;
                                NewE ->
                                    tc_arr_sync(Id, {Length, NewE}),
                                    NewSlot = setelement(?SOBJECT_ELEMENTS, Slot, NewE),
                                    setelement(?AGENT_STORE, St,
                                        setelement(?STORE_DATA, Store,
                                            Data#{Id := NewSlot}))
                            end
                    end;
                {?ARRAYOBJ_TAG, Length} when Idx =:= Length ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{index, Idx} := _} -> miss;
                        _ ->
                            case elem_write_grow(element(?SOBJECT_ELEMENTS, Slot), Idx, V) of
                                miss -> miss;
                                NewE ->
                                    tc_arr_sync(Id, {Length + 1, NewE}),
                                    NewSlot = setelement(?SOBJECT_ELEMENTS,
                                        setelement(?SOBJECT_KIND, Slot,
                                            {?ARRAYOBJ_TAG, Length + 1}),
                                        NewE),
                                    setelement(?AGENT_STORE, St,
                                        setelement(?STORE_DATA, Store,
                                            Data#{Id := NewSlot}))
                            end
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
t_set_elem_fast(_, _, _, _) -> miss.

elem_write({?ELEMS_DENSE, A}, Idx, V) ->
    case Idx < array:size(A) of
        true -> {?ELEMS_DENSE, array:set(Idx, V, A)};
        false -> miss
    end;
elem_write({?ELEMS_SPARSE, M}, Idx, V) ->
    {?ELEMS_SPARSE, M#{Idx => V}};
elem_write(_, _, _) -> miss.

%% Append at Idx==Length: dense array:set/3 auto-extends past size(A), so no
%% bounds gate; sparse is just a map put. Any other elements-shape misses.
elem_write_grow({?ELEMS_DENSE, A}, Idx, V) ->
    {?ELEMS_DENSE, array:set(Idx, V, A)};
elem_write_grow({?ELEMS_SPARSE, M}, Idx, V) ->
    {?ELEMS_SPARSE, M#{Idx => V}};
elem_write_grow(_, _, _) -> miss.

%% t_set_elem_fast_p(St, Recv, Idx, V) -> 0 | miss
%% JRead (perf7_arr_pdict). Warm pdict-overlay write — no Store rebuild, no
%% {V,St'} tuple. Cold arm (overlay undefined) reads St to install: crypto's
%% bnpCopyTo/bnpDLShiftTo write `r_array[i]=v` into fresh arrays with no
%% prior element read, so read-side install never fires (~76-86k misses/run
%% pre-fix). Same ?TC_ARR_MIN_LEN gate as t_get_elem_fast_p keeps deltablue's
%% 1-3 elem OrderedCollection arrays out. Idx > Length still misses to the
%% `set_prop` slow path (t_cell_set→jsv_evict erases; next access reinstalls).
t_set_elem_fast_p(St, {js_cell, Id}, Idx, V)
  when is_integer(Idx), Idx >= 0 ->
    case get({tc_arr, Id}) of
        {Length, {tuple_dense, T}} when Idx < Length ->
            put({tc_arr, Id},
                {Length, {tuple_dense, setelement(Idx + 1, T, V)}}),
            0;
        {Length, {tuple_dense, T}} when Idx =:= Length ->
            put({tc_arr, Id},
                {Length + 1, {tuple_dense, erlang:append_element(T, V)}}),
            0;
        {Length, Elems} when Idx < Length ->
            case elem_write(Elems, Idx, V) of
                miss -> miss;
                NewE -> put({tc_arr, Id}, {Length, NewE}), 0
            end;
        {Length, Elems} when Idx =:= Length ->
            case elem_write_grow(Elems, Idx, V) of
                miss -> miss;
                NewE -> put({tc_arr, Id}, {Length + 1, NewE}), 0
            end;
        {_, _} -> miss;
        undefined ->
            Store = element(?AGENT_STORE, St),
            case element(?STORE_DATA, Store) of
                #{Id := Slot}
                  when element(1, Slot) =:= ?SOBJECT_TAG,
                       element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
                    case element(?SOBJECT_KIND, Slot) of
                        {?ARRAYOBJ_TAG, Length}
                          when Idx =< Length,
                               Length >= ?TC_ARR_MIN_LEN ->
                            case map_size(element(?SOBJECT_PROPS, Slot)) of
                                0 ->
                                    tc_arr_install_write(
                                      Id, Length,
                                      element(?SOBJECT_ELEMENTS, Slot), Idx, V);
                                _ -> miss
                            end;
                        _ -> miss
                    end;
                _ -> miss
            end
    end;
t_set_elem_fast_p(_, _, _, _) -> miss.

%% t_arr_c_load(Recv) -> {Length,Elems} | undefined
%% JPure (perf8_arr_c_hoist). Pre-loop hoist of `get({tc_arr,Id})` for a
%% loop-invariant read-only bracket base — the emitter binds this once and
%% passes it as ArrC to t_get_elem_fast_c per read (0 pdict-get). Non-cell /
%% not-yet-installed → undefined; the `_c` warm arm rejects and tail-calls
%% `_p` so cold install still fires once.
t_arr_c_load({js_cell, Id}) -> get({tc_arr, Id});
t_arr_c_load(_) -> undefined.

%% t_get_elem_fast_c(St, ArrC, Recv, Idx) -> V | miss
%% JRead (perf8_arr_c_hoist). Warm hit reads via the pre-fetched ArrC overlay
%% — tuple_dense is ONE `element(Idx+1, T)` (crypto am3 this_array[i] ≈2.4M/
%% run). Any ArrC miss (undefined / OOB / non-tuple_dense) tail-calls
%% t_get_elem_fast_p so cold install + hole/proto fallback are byte-identical
%% to the un-hoisted path.
t_get_elem_fast_c(_, {Length, {tuple_dense, T}}, _, Idx)
  when is_integer(Idx), Idx >= 0, Idx < Length ->
    element(Idx + 1, T);
t_get_elem_fast_c(St, _, Recv, Idx) ->
    t_get_elem_fast_p(St, Recv, Idx).

%% Cold-install helper for t_set_elem_fast_p: apply the write to Store's
%% Elems, then install the post-write overlay in one put. Length is Store's
%% pre-write length; grows by one iff Idx =:= Length.
tc_arr_install_write(Id, Length, Elems, Idx, V) when Idx < Length ->
    case elem_write(Elems, Idx, V) of
        miss -> miss;
        NewE -> tc_arr_install(Id, Length, NewE), 0
    end;
tc_arr_install_write(Id, Length, Elems, Idx, V) ->
    case elem_write_grow(Elems, Idx, V) of
        miss -> miss;
        NewE -> tc_arr_install(Id, Length + 1, NewE), 0
    end.

%% Install the array overlay for `Id` and track it so jsv_clear/jsv_flush
%% can enumerate without a pdict scan. Map-set tracker (idempotent — no
%% unbounded growth on evict→reinstall churn). perf8: dense Store elements
%% are flattened to `{tuple_dense, T}` (invariant: tuple_size(T) =:= Length)
%% so t_get_elem_fast_p's warm hit is a bare `element(Idx+1, T)` — crypto
%% am3's 2.4M reads/run were paying array:size + array:get + array:default.
tc_arr_install(Id, Length, Elems) ->
    put({tc_arr, Id}, {Length, tc_arr_densify(Length, Elems)}),
    D0 = case get(?TC_ARR_IDS) of undefined -> #{}; D -> D end,
    put(?TC_ARR_IDS, D0#{Id => true}),
    ok.

%% Store's `{dense, A}` → overlay `{tuple_dense, T}` with tuple_size == Length
%% (array:resize pads/truncates with A's default — the atom `undefined`, which
%% the emitter's IsAtom guard treats as miss). Sparse and NoElements pass
%% through unchanged; jsv_overlay_slot inverts.
tc_arr_densify(Length, {?ELEMS_DENSE, A}) ->
    {tuple_dense, list_to_tuple(array:to_list(array:resize(Length, A)))};
tc_arr_densify(_, Elems) -> Elems.

%% Write-through the overlay iff already installed (t_set_elem_fast hit arm).
%% Never tracks — install owns tracking; this only refreshes a live entry.
tc_arr_sync(Id, {Length, Elems}) ->
    case get({tc_arr, Id}) of
        undefined -> ok;
        _ -> put({tc_arr, Id}, {Length, tc_arr_densify(Length, Elems)})
    end.

%% ──────────────────── proto-method IC (tc_mc) ────────────────────
%% Candidate D — pdict `{tc_mc, PId, KeyBin}` → FnId cache for
%% `t_call_method_mono` (arc_rt_call_ffi). deltablue.js has 3-level
%% proto chains via `inheritsFrom` (StayConstraint→UnaryConstraint→
%% Constraint, ScaleConstraint→BinaryConstraint→Constraint) so a 1-hop
%% mono probe misses inherited `Constraint.prototype` methods; richards.js
%% is flat 1-hop. `tc_mc_get` is the warm probe (called before the proto
%% walk); `tc_mc_install` is the cold-hit hook (after the walk resolves
%% FnId). Installed `{PId, KeyBin}` pairs are tracked in `?TC_MC_IDS` so
%% `jsv_clear` can sweep without a pdict prefix-scan. `?TC_MC_DEPS` is a
%% map-set `#{ProtoId => true}` of every proto-id any entry's resolution
%% WALKED THROUGH (immediate → owner, inclusive), so `tc_mc_evict(Id)` on
%% a `t_cell_set` OR overlay write to ANY proto in a cached chain drops
%% the whole cache — a mid-chain shadow or owner-proto rewrite is then
%% re-resolved cold. Instance ids are never in `?TC_MC_DEPS`, so the
%% per-field overlay-write evict-probe is an O(1) `is_map_key` miss.
%% FnId is a bare cell-id — GC coherence is via
%% `jsv_flush`, which calls `tc_mc_clear` unconditionally before mark; no
%% separate root registration.

tc_mc_get(PId, KeyBin) ->
    get({tc_mc, PId, KeyBin}).

tc_mc_install(PId, KeyBin, FnId) ->
    tc_mc_install(PId, KeyBin, FnId, [PId]).

%% PathIds = every proto-id the cold walk visited (PId first, owner last).
%% call_ffi passes the full path; the /3 shim above defaults to [PId] for
%% the 1-hop case.
tc_mc_install(PId, KeyBin, FnId, PathIds) ->
    put({tc_mc, PId, KeyBin}, FnId),
    case get(?TC_MC_IDS) of
        undefined -> put(?TC_MC_IDS, [{PId, KeyBin}]);
        L -> put(?TC_MC_IDS, [{PId, KeyBin} | L])
    end,
    D0 = case get(?TC_MC_DEPS) of undefined -> #{}; D -> D end,
    put(?TC_MC_DEPS, lists:foldl(fun(I, A) -> A#{I => true} end, D0, PathIds)),
    ok.

%% Coherence: `Id` is a proto any cached resolution depends on → drop the
%% whole cache (proto mutation is rare; correctness > warm preservation).
%% `Id` not a dep (the common case: an instance's overlay write /
%% t_cell_set) → O(1) map-miss no-op.
tc_mc_evict(Id) ->
    case get(?TC_MC_DEPS) of
        undefined -> ok;
        Deps ->
            case is_map_key(Id, Deps) of
                true -> tc_mc_clear();
                false -> ok
            end
    end.

tc_mc_clear() ->
    erase(?TC_MC_DEPS),
    case erase(?TC_MC_IDS) of
        undefined -> ok;
        L -> _ = [erase({tc_mc, P, K}) || {P, K} <- L], ok
    end,
    case erase(?TC_IC_IDS) of
        undefined -> ok;
        Ls -> _ = [erase(S) || S <- Ls], ok
    end.

%% Per-CALLSITE poly IC install for t_call_method_ic (call_ffi). Cache is
%% keyed on `K = {Sid, Proto}` — Sid alone is UNSOUND: shape_learn walks a
%% proto-agnostic key trie, so two ctors with identical field sequences share
%% a Sid with different protos (richards WorkerTask/HandlerTask; deltablue
%% StayConstraint/EditConstraint) → different method resolutions. Format:
%% mono `{K, Code, FnH, SimpleT}` (nested bound-match at warm hit, zero
%% alloc) or poly `#{K => {Code, FnH, SimpleT}}` (≤4). ≥5th distinct K marks
%% the site `mega` (routes through t_call_method_mono's per-proto tc_mc).
%% Registers PathIds as tc_mc deps so a proto mutation sweeps every callsite
%% cache via tc_mc_evict → tc_mc_clear. `SimpleT` is `{CodeT,Arity}` (this-
%% abi closure — see rt_types KFunction.simple) or `none`; carried so the
%% inline warm-hit can dispatch CodeT([Recv|Pos]) with zero frame/args-cons.
tc_ic_install(SiteKey, K, Code, FnH, PathIds) ->
    tc_ic_install(SiteKey, K, Code, FnH, none, PathIds).

tc_ic_install(SiteKey, K, Code, FnH, SimpleT, PathIds) ->
    D0 = case get(?TC_MC_DEPS) of undefined -> #{}; D -> D end,
    put(?TC_MC_DEPS, lists:foldl(fun(I, A) -> A#{I => true} end, D0, PathIds)),
    case get(SiteKey) of
        undefined ->
            case get(?TC_IC_IDS) of
                undefined -> put(?TC_IC_IDS, [SiteKey]);
                L -> put(?TC_IC_IDS, [SiteKey | L])
            end,
            put(SiteKey, {K, Code, FnH, SimpleT});
        mega -> ok;
        {K, _, _, _} -> put(SiteKey, {K, Code, FnH, SimpleT});
        {K0, C0, F0, S0} ->
            put(SiteKey, #{K0 => {C0, F0, S0}, K => {Code, FnH, SimpleT}});
        M when is_map(M) ->
            case map_size(M) >= 4 andalso not is_map_key(K, M) of
                true -> put(SiteKey, mega);
                false -> put(SiteKey, M#{K => {Code, FnH, SimpleT}})
            end
    end,
    ok.

%% ──────────────────── per-site prop IC (i-prop-ic) ────────────────────
%% SPEC i-prop-ic — per-CALLSITE inline cache for static `.x` reads/writes
%% on SShapedObject cells. Warm hit = one pdict get + shape-id compare +
%% array:get. `SiteKey` is a ConstBinary (`<<"@pg", N:32>>`) minted once by
%% the emitter; pdict[SiteKey] = `{ShapeId, Off}`. Tracked in ?TC_IC_IDS so
%% tc_mc_clear (called by jsv_clear/jsv_flush/proto-mutation evict) sweeps
%% every site.
%%   SShapedObject (JsSlot): {s_shaped_object, ShapeId, Proto, Slots}
%%   ShapeDesc: {shape_desc, Arity, Offsets, Transitions}.

%% t_ic_get(St, {js_cell,Id}, KeyBin, SiteKey) -> V | miss
%% JRead. Warm hit is TWO pdict gets + ONE shape-id compare + ONE array:get
%% — no `St` touch (SPEC i-prop-ic "ONE compare + ONE array indexed read";
%% 134ns→~10ns). pdict[Id] holds the ENTIRE `{s_shaped_object,Sid,P,Slots}`
%% tuple (installed by ic_load below; fresher than St after any ic_set),
%% pdict[SiteKey] holds `{Sid,Off}`. Coherence rides the existing overlay
%% hooks: t_cell_get→jsv_overlay_slot returns the pdict tuple verbatim;
%% t_cell_set→jsv_evict erases it; jsv_flush writes it back to Data. Cold /
%% shape mismatch → re-resolve via shape_offset, install, read. Non-shaped
%% receivers (SObject map overlay / cold-undefined) tail into the SAME
%% own-data path as t_get_prop_own_data (map warm-hit else cold_get) so the
%% emitter's cold tier is ONE call_ext: `miss` here means the full
%% t_get_prop_any proto-walk is needed.
t_ic_get(St, {js_cell, Id}, KeyBin, SiteKey) ->
    case get(Id) of
        C when element(1, C) =:= ?SSHAPED_TAG ->
            Sid = element(?SSHAPED_SID, C),
            case get(SiteKey) of
                {Sid, OffF} -> element(OffF, C);
                _ -> ic_cold_get(St, Sid, C, KeyBin, SiteKey)
            end;
        M when is_map(M) ->
            case M of
                #{KeyBin := V} -> V;
                _ -> cold_get(St, Id, KeyBin, M)
            end;
        undefined ->
            %% pdict[Id] cold — pull the slot from St (nested JsSlot). Shaped
            %% → install the FLAT tuple overlay + SiteKey; non-shaped →
            %% cold_get installs the SObject map overlay.
            case slot_of(St, Id) of
                {?SSHAPED_TAG, Sid, P, Slots} ->
                    C = shaped_flat_install(Id, Sid, P, Slots),
                    case get(SiteKey) of
                        {Sid, OffF} -> element(OffF, C);
                        _ -> ic_cold_get(St, Sid, C, KeyBin, SiteKey)
                    end;
                _ -> cold_get(St, Id, KeyBin, #{})
            end;
        {KeyBin, V} -> V;
        {Kb, Ov} -> cold_get(St, Id, KeyBin, #{Kb => Ov})
    end;
t_ic_get(_, _, _, _) -> miss.

%% t_ic_proto_get(St, {js_cell,Id}, KeyBin, SiteKey) -> V | miss
%% JRead own+proto data-get (perf8 raytrace own_property_of lever). Emitter
%% calls this AFTER t_ic_get miss — which for non-`ordinary` kinds (KFunction,
%% ErrorObj, …) means "peek_get gate rejected", NOT "own-absent". So: (1) own
%% probe on the RECEIVER's Store props first (any kind; overlay-aware) —
%% catches raytrace `.prototype` on KFunction, `e.message` on ErrorObj. (2) On
%% own-absent, warm-hit pdict[SiteKey]={PId,V} keyed on receiver's IMMEDIATE
%% proto-id, else walk proto chain via Store.data up to ?IC_PG_MAX hops.
%% Install {PId,V} at SiteKey + PathIds in ?TC_MC_DEPS so a proto write's
%% tc_mc_evict sweeps it. Targets rt_obj:get_from 416k×254ns +
%% own_property_of 462k×201ns ≈ 199ms (raytrace Q). Mono cache — poly-proto
%% sites thrash (correct: mismatch → cold walk). `miss` on: accessor / virtual
%% own key / not found / no proto → emitter falls to t_get_prop_any. Kinds
%% with virtual named/index own props (ArrayObj/StringObj/ArgumentsObj
%% "length") bail — the props-dict probe would report false absent.
-define(IC_PG_MAX, 4).
t_ic_proto_get(St, {js_cell, Id}, KeyBin, SiteKey) ->
    case slot_of(St, Id) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case ic_pg_kind_ok(element(?SOBJECT_KIND, Slot)) of
                false -> miss;
                true ->
                    case ic_pg_own(Id, Slot, KeyBin) of
                        {hit, V} -> V;
                        accessor -> miss;
                        absent ->
                            ic_pg_proto(St, element(?SOBJECT_PROTO, Slot), KeyBin, SiteKey)
                    end
            end;
        {?SSHAPED_TAG, _, P, _} ->
            %% Shaped own already covered by t_ic_get's ic_cold_get; miss
            %% here IS own-absent → proto tier directly.
            ic_pg_proto(St, P, KeyBin, SiteKey);
        _ -> miss
    end;
t_ic_proto_get(_, _, _, _) -> miss.

%% Kinds whose named-key [[GetOwnProperty]] is exactly `dict.get(props,K)` —
%% i.e. NO virtual own named/index props that a props-dict probe would miss.
%% ArrayObj/StringObj/ArgumentsObj (virtual "length"/index), ModuleNamespace,
%% ProxyObj, TypedArrayObj → false: bail to t_get_prop_any. `ordinary` is a
%% bare atom (arity-0 constructor); every other kind is a tagged tuple.
ic_pg_kind_ok(?ORDINARY) -> true;
ic_pg_kind_ok(Kind) when is_tuple(Kind) ->
    case element(1, Kind) of
        ?ARRAYOBJ_TAG -> false;
        string_obj -> false;
        arguments_obj -> false;
        module_namespace -> false;
        proxy_obj -> false;
        typed_array_obj -> false;
        _ -> true
    end;
ic_pg_kind_ok(_) -> false.

ic_pg_proto(St, Proto, KeyBin, SiteKey) ->
    case Proto of
        {some, {js_cell, PId}} ->
            case get(SiteKey) of
                {PId, V} -> V;
                _ ->
                    Store = element(?AGENT_STORE, St),
                    ic_pg_walk(element(?STORE_DATA, Store), PId, KeyBin, SiteKey,
                               PId, [], ?IC_PG_MAX)
            end;
        _ -> miss
    end.

ic_pg_walk(_, _, _, _, _, _, 0) -> miss;
ic_pg_walk(Data, Id, KeyBin, SiteKey, RootPId, Path, Fuel) ->
    case Data of
        #{Id := Slot} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case ic_pg_own(Id, Slot, KeyBin) of
                {hit, V} ->
                    ic_track(SiteKey),
                    put(SiteKey, {RootPId, V}),
                    D0 = case get(?TC_MC_DEPS) of
                             undefined -> #{}; D -> D end,
                    put(?TC_MC_DEPS, lists:foldl(
                        fun(I, A) -> A#{I => true} end, D0, [Id | Path])),
                    V;
                accessor -> miss;
                absent ->
                    case element(?SOBJECT_PROTO, Slot) of
                        {some, {js_cell, NId}} ->
                            ic_pg_walk(Data, NId, KeyBin, SiteKey,
                                       RootPId, [Id | Path], Fuel - 1);
                        _ -> miss
                    end
            end;
        _ -> miss
    end.

%% Overlay-aware own-data probe (mono_own_value pattern, call_ffi.erl:367).
%% pdict map/mono-tuple carries the freshest write; Store props otherwise.
ic_pg_own(Id, Slot, KeyBin) ->
    case get(Id) of
        #{KeyBin := V} -> {hit, V};
        {KeyBin, V} -> {hit, V};
        _ ->
            case element(?SOBJECT_PROPS, Slot) of
                #{{named, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                    {hit, element(?DATAPROP_VALUE, Prop)};
                #{{named, KeyBin} := _} -> accessor;
                _ -> absent
            end
    end.

%% t_ic_warm_get(Obj, SiteKey) -> V | miss
%% JPure warm-only probe — NO St, NO cold install. The emitter calls this
%% first (single call_ext, ~12ns); on `miss` it falls to t_ic_get (JRead)
%% which does the cold install. Keeps the emitted IR small (one bind_if
%% instead of five nested) so BEAM's JIT sees a straight-line body.
%% t_shaped_get(C, SiteKey) -> V | miss
%% JPure. `C` is the ALREADY-FETCHED pdict[Id] overlay (the emitter's
%% threaded `_this_c`) — no `get(Id)` here. Warm-hit shaped read as one
%% call_ext: the emitter's inlined 4-bind_if ladder costs ~10 BIFs + 1
%% l_join `apply` (~34ns/read); this is ~20ns. Used ONLY by the
%% simple_this arm (richards' `this.x` reads, ~150k/run) — non-this reads
%% keep the inline mono/map ladder so obj_prop's mono-hit stays 3 BIFs.
t_shaped_get(C, SiteKey)
  when is_tuple(C), is_atom(element(1, C)) ->
    case get(SiteKey) of
        {Sid, Off} when Sid =:= element(?SSHAPED_SID, C) -> element(Off, C);
        _ -> miss
    end;
t_shaped_get(_, _) -> miss.

%% t_shaped_set(C, SiteKey, Id, V) -> NC | miss
%% JPure. Warm-hit shaped write as one call_ext; on hit does the
%% setelement + put(Id) and RETURNS the new overlay tuple so the emitter
%% rebinds `_this_c` (slot -1) to it — same threading contract as the
%% inlined path. `Id` is `_this_id`; passed so the write stays JPure
%% (no St).
t_shaped_set(C, SiteKey, Id, V)
  when is_tuple(C), is_atom(element(1, C)) ->
    case get(SiteKey) of
        {Sid, Off} when Sid =:= element(?SSHAPED_SID, C) ->
            NC = setelement(Off, C, V),
            put(Id, NC),
            NC;
        _ -> miss
    end;
t_shaped_set(_, _, _, _) -> miss.

t_ic_warm_get({js_cell, Id}, SiteKey) ->
    case get(Id) of
        C when element(1, C) =:= ?SSHAPED_TAG ->
            Sid = element(?SSHAPED_SID, C),
            case get(SiteKey) of
                {Sid, OffF} -> element(OffF, C);
                _ -> miss
            end;
        _ -> miss
    end;
t_ic_warm_get(_, _) -> miss.

%% t_ic_warm_set(Obj, SiteKey, V) -> 0 | miss
%% JPure warm-only write — pdict overlay update, no St. `miss` on any cold
%% state; the emitter falls to t_ic_set (JRead) which installs.
t_ic_warm_set({js_cell, Id}, SiteKey, V) ->
    case get(Id) of
        C when element(1, C) =:= ?SSHAPED_TAG ->
            Sid = element(?SSHAPED_SID, C),
            case get(SiteKey) of
                {Sid, OffF} ->
                    put(Id, setelement(OffF, C, V)),
                    0;
                _ -> miss
            end;
        _ -> miss
    end;
t_ic_warm_set(_, _, _) -> miss.

%% SiteKey caches `{Sid, OffF}` where OffF = shape offset + ?OVERLAY_OFF, 1-based into
%% the FLAT pdict tuple `{tag,Sid,P,X0,…}` so the emitter's inlined warm hit
%% is a single `element(OffF, C)` / `setelement(OffF, C, V)` on the outer
%% tuple (no nested rebuild). `C` is the flat pdict tuple.
ic_cold_get(St, Sid, C, KeyBin, SiteKey) ->
    case shape_offset(St, Sid, KeyBin) of
        miss -> miss;
        Off ->
            ic_track(SiteKey),
            put(SiteKey, {Sid, Off + ?OVERLAY_OFF}),
            element(Off + ?OVERLAY_OFF, C)
    end.

%% t_ic_set(St, {js_cell,Id}, KeyBin, V, SiteKey) -> 0 | ok | miss
%% JRead — the write lands in the pdict overlay (same coherence contract as
%% t_set_prop_own_data), so St is READ-ONLY and never rebuilt. Warm hit is
%% TWO pdict gets + ONE shape-id compare + ONE setelement + ONE pdict put.
%% Non-shaped receivers (SObject map overlay / cold-undefined) tail into
%% set_own_map — the SAME own-data write path as t_set_prop_own_data — so
%% the emitter's cold tier is ONE call_ext; `miss` here means the full
%% t_set_prop_any is needed.
t_ic_set(St, {js_cell, Id}, KeyBin, V, SiteKey) ->
    case get(Id) of
        C when element(1, C) =:= ?SSHAPED_TAG ->
            Sid = element(?SSHAPED_SID, C),
            case get(SiteKey) of
                {Sid, OffF} ->
                    put(Id, setelement(OffF, C, V)),
                    0;
                _ ->
                    ic_cold_set(St, Id, Sid, C, KeyBin, V, SiteKey)
            end;
        M when is_map(M) -> set_own_map(St, Id, KeyBin, V, M);
        undefined ->
            case slot_of(St, Id) of
                {?SSHAPED_TAG, Sid, P, Slots} ->
                    C = shaped_flat_install(Id, Sid, P, Slots),
                    case get(SiteKey) of
                        {Sid, OffF} ->
                            put(Id, setelement(OffF, C, V)),
                            0;
                        _ ->
                            ic_cold_set(St, Id, Sid, C, KeyBin, V, SiteKey)
                    end;
                _ -> set_own_map(St, Id, KeyBin, V, #{})
            end;
        %% Mono same-key: STAY mono (perf2 obj_prop hot path — the inline
        %% emitter handles the loop-warm case; this arm catches the FFI-cold
        %% first write after cold_get installed `{KeyBin,_}`).
        {KeyBin, _} -> tc_mc_evict(Id), put(Id, {KeyBin, V}), ok;
        {Kb, Ov} -> set_own_map(St, Id, KeyBin, V, #{Kb => Ov})
    end;
t_ic_set(_, _, _, _, _) -> miss.

ic_cold_set(St, Id, Sid, C, KeyBin, V, SiteKey) ->
    case shape_offset(St, Sid, KeyBin) of
        miss -> miss;
        Off ->
            ic_track(SiteKey),
            put(SiteKey, {Sid, Off + ?OVERLAY_OFF}),
            put(Id, setelement(Off + ?OVERLAY_OFF, C, V)),
            0
    end.

%% ── FLAT pdict overlay ⇄ nested JsSlot (perf5 flat-pdict) ──
%% pdict[Id] holds `{s_shaped_object,Sid,P,X0,…,Xn-1}` (slots inlined at
%% positions ?OVERLAY_OFF..N) so a warm write is ONE `setelement(OffF,C,V)` — no nested
%% Slots-tuple rebuild + 4-tuple wrapper alloc. JsStore keeps the nested
%% `{s_shaped_object,Sid,P,{X0,…}}` (Gleam record). Conversion is COLD-only.
shaped_flat_install(Id, Sid, P, Slots) ->
    C = list_to_tuple([?SSHAPED_TAG, Sid, P | tuple_to_list(Slots)]),
    jsv_install(Id, C),
    C.

%% perf7 z3-jsv-flush-cost: strip the {tag,Sid,P} header via 3×delete_element
%% BIF (C-side tuple copy) instead of `[element(I,C) || I <- lists:seq(4,N)]`
%% — the LC was raytrace's #2 sink (931k iter/run ≈ 111ms traced, avg 14
%% slots/obj). No interpreted lc$^0 fn, no lists:seq cons churn.
shaped_unflat(C) ->
    {?SSHAPED_TAG, element(?SSHAPED_SID, C), element(?SSHAPED_PROTO, C),
     erlang:delete_element(1,
       erlang:delete_element(1,
         erlang:delete_element(1, C)))}.

%% ── ShapeSlots FFI (rt_types.gleam) — plain-tuple slot storage. ──
%% shape_slots_get(Slots, Off) -> JsVal — 0-based offset. Gleam-side slow
%% paths (as_sobject, GC refs_in_cell) go through this.
shape_slots_get(Slots, Off) -> element(Off + 1, Slots).

%% shape_slots_fold(Slots, Acc, F) -> Acc' — fold F(Off, V, A) over every
%% slot. Mirrors the tree_array.sparse_fold contract used by rt_gc.
shape_slots_fold(Slots, Acc, F) ->
    shape_slots_fold_1(Slots, Acc, F, 1, tuple_size(Slots)).
shape_slots_fold_1(_, Acc, _, I, N) when I > N -> Acc;
shape_slots_fold_1(Slots, Acc, F, I, N) ->
    shape_slots_fold_1(Slots, F(I - 1, element(I, Slots), Acc), F, I + 1, N).

%% Register SiteKey in ?TC_IC_IDS on first install so the sweep in
%% tc_mc_clear erases it. Idempotent via the pdict[SiteKey] presence check.
ic_track(SiteKey) ->
    case get(SiteKey) of
        undefined ->
            case get(?TC_IC_IDS) of
                undefined -> put(?TC_IC_IDS, [SiteKey]);
                L -> put(?TC_IC_IDS, [SiteKey | L])
            end;
        _ -> ok
    end.

%% ─────────────────── hidden-class shape table (h-shape) ───────────────────
%% Shapes are structural descriptors of a fixed keyset→offset mapping,
%% stored in JsStore.shapes :: #{ShapeId => ShapeDesc} (?STORE_SHAPES,
%% appended by h-shape-types). A shape is IMMUTABLE once created — the
%% only mutation is `transitions` gaining an edge. next_shape (?STORE_NEXT_SHAPE)
%% is the monotone id counter. Shape 0 is the empty root.
%%   ShapeDesc = {shape_desc, Arity, #{KeyBin=>Off}, #{KeyBin=>ToSid}}.

%% shape_offset_cached — pdict `{shape_off,Sid,Kb}→Off` memoized wrapper.
%% jsv_overlay_slot (which has no St) reads it to flush overlay writes
%% into shaped slots. Tracked in ?SHAPE_OFF_IDS and swept by jsv_clear —
%% shape ids are per-JsStore, pdict is process-global, so a re-applied
%% seed / second realm in the same BEAM process reuses ids for different
%% ShapeDescs and a stale entry would return the wrong offset.
shape_offset_cached(St, Sid, KeyBin) ->
    K = {shape_off, Sid, KeyBin},
    case get(K) of
        undefined ->
            case shape_offset(St, Sid, KeyBin) of
                miss -> miss;
                Off ->
                    put(K, Off),
                    case get(?SHAPE_OFF_IDS) of
                        undefined -> put(?SHAPE_OFF_IDS, [K]);
                        L -> put(?SHAPE_OFF_IDS, [K | L])
                    end,
                    Off
            end;
        Off -> Off
    end.

%% shape_offset(St, ShapeId, KeyBin) -> Off | miss
%% Raw ShapeDesc.offsets lookup. Hot callers (t_ic_get/set) memoize in
%% pdict[SiteKey]; cold callers go through shape_offset_cached.
shape_offset(St, Sid, KeyBin) ->
    Store = element(?AGENT_STORE, St),
    case element(?STORE_SHAPES, Store) of
        #{Sid := Desc} ->
            case element(?SHAPE_OFFSETS, Desc) of
                #{KeyBin := Off} -> Off;
                _ -> miss
            end;
        _ -> miss
    end.

%% t_new_object_shaped(St, SiteKey, Keys, Vals) -> {JsVal, St'}
%% JMut — object-literal fast path (shape-object-literals). The emitter
%% routes `{a:v, b:w, ..}` here when EVERY property is a plain named-key
%% InitProperty (no computed/index/__proto__/method/accessor/spread and no
%% duplicate keys); Keys is the compile-time [<<"a">>, <<"b">>, ..] and
%% Vals the L-to-R evaluated values in the SAME order. Allocates an
%% SShapedObject directly with proto = %Object.prototype% so subsequent
%% `.a`/`.a=` on the literal hit the shaped t_ic_get/set warm path — the
%% obj_prop microbench regressed 11.8k→21.7k when perf4's IC probe preceded
%% own_data on plain-SObject literals. Per-SITE Sid cache in pdict[SiteKey]
%% (tracked in ?TC_IC_IDS, swept by jsv_clear); cold path walks Keys through
%% the shape transition tree. Like new_simple_warm the fresh slot is NOT
%% pre-installed in the pdict overlay — first `.x` does that lazily via
%% t_ic_get's undefined→slot_of→jsv_install.
t_new_object_shaped(St, SiteKey, Keys, Vals) ->
    Store0 = element(?AGENT_STORE, St),
    Realm = element(?AGENT_REALM, St),
    Proto = element(?PAIR_PROTO, element(?REALM_OBJECT, Realm)),
    {Sid, Store1} = case get(SiteKey) of
        S when is_integer(S) -> {S, Store0};
        _ ->
            {S, StoreN} = shape_learn_keys(Store0, Keys),
            ic_track(SiteKey),
            put(SiteKey, S),
            {S, StoreN}
    end,
    Slot = {?SSHAPED_TAG, Sid, {some, Proto}, list_to_tuple(Vals)},
    Data = element(?STORE_DATA, Store1),
    {NewId, Free, Next} = case element(?STORE_FREE, Store1) of
        [Id | Rest] -> {Id, Rest, element(?STORE_NEXT, Store1)};
        [] -> N = element(?STORE_NEXT, Store1), {N, [], N + 1}
    end,
    Store2 = setelement(?STORE_DATA, Store1, Data#{NewId => Slot}),
    Store3 = setelement(?STORE_FREE, Store2, Free),
    Store4 = setelement(?STORE_NEXT, Store3, Next),
    Store5 = setelement(?STORE_ALLOC, Store4, element(?STORE_ALLOC, Store1) + 1),
    {{js_cell, NewId}, setelement(?AGENT_STORE, St, Store5)}.

%% Walk `Keys` through the transition tree from root=0 (lazy-seeded); local
%% twin of call_ffi's shape_learn (not exported there). {Sid, Store'}.
shape_learn_keys(Store0, Keys) ->
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
    lists:foldl(fun(Kb, {S, StA}) ->
        {S2, _Off, StB} = shape_transition(StA, S, Kb),
        {S2, StB}
    end, {0, Store}, Keys).

%% shape_root() -> 0
%% The empty-shape id. h-shape-types seeds `#{0 => {shape_desc,0,#{},#{}}}`
%% into JsStore.shapes at store construction; shape_transition tolerates a
%% missing root by treating it as empty.
shape_root() -> 0.

%% shape_desc(Store, Sid) -> {shape_desc,Arity,Offsets,Transitions} | miss
%% For h-shape-slowpath-compat's `as_sobject` (rebuild props Dict from
%% offsets) and h-shape-new-learn's cold-path convert.
shape_desc(Store, Sid) ->
    case element(?STORE_SHAPES, Store) of
        #{Sid := Desc} -> Desc;
        _ -> miss
    end.

%% shape_transition(Store, FromSid, KeyBin) -> {ToSid, Off, Store'}
%% Find-or-create the successor shape reached by adding `KeyBin` to
%% `FromSid`. Returns the target shape id, the offset `KeyBin` occupies in
%% it (= FromSid's arity), and Store' with any newly-minted shape and the
%% back-edge on FromSid installed. Idempotent on an existing edge (Store
%% returned unchanged). Used by h-shape-new-learn's cold path to walk a
%% freshly-constructed SObject's key list from shape_root to its exit
%% shape.
shape_transition(Store, FromSid, KeyBin) ->
    Shapes = element(?STORE_SHAPES, Store),
    From = case Shapes of
               #{FromSid := D} -> D;
               _ -> {?SHAPE_TAG, 0, #{}, #{}}
           end,
    {?SHAPE_TAG, Arity, Offsets, Trans} = From,
    case Trans of
        #{KeyBin := ToSid} ->
            {ToSid, Arity, Store};
        _ ->
            case Offsets of
                #{KeyBin := Off} ->
                    %% Key already in this shape (repeat add) — no-op edge.
                    {FromSid, Off, Store};
                _ ->
                    ToSid = element(?STORE_NEXT_SHAPE, Store),
                    ToDesc = {?SHAPE_TAG, Arity + 1,
                              Offsets#{KeyBin => Arity}, #{}},
                    From1 = {?SHAPE_TAG, Arity, Offsets,
                             Trans#{KeyBin => ToSid}},
                    Shapes1 = Shapes#{FromSid => From1, ToSid => ToDesc},
                    Store1 = setelement(?STORE_NEXT_SHAPE,
                                 setelement(?STORE_SHAPES, Store, Shapes1), ToSid + 1),
                    {ToSid, Arity, Store1}
            end
    end.

%% ───────────────────────── overlay bookkeeping ─────────────────────────

%% Read the slot for `Id` from `St.store.data`. `miss` if absent (a
%% dangling handle). NOT the overlay — cold-path
%% validation reads the persistent shape.
slot_of(St, Id) ->
    case element(?STORE_DATA, element(?AGENT_STORE, St)) of
        #{Id := Slot} -> Slot;
        _ -> miss
    end.

%% Install/replace `Id`'s overlay map and track `Id` so `jsv_clear`/
%% `jsv_flush` can enumerate installed cells without a full pdict scan.
%% `track_id` is idempotent-by-duplication (harmless — flush/clear are).
jsv_install(Id, Map) ->
    put(Id, Map),
    track_id(Id).

track_id(Id) ->
    case get(?IDS) of
        undefined -> put(?IDS, [Id]);
        Ids -> put(?IDS, [Id | Ids])
    end.

%% jsv_evict(Id) — drop the cached value for cell `Id`. Called by
%% `t_cell_set` before its persistent write so a descriptor / shape change
%% forces the fast path back to cold validation. The caller reads via
%% `t_cell_get` (which overlays) BEFORE calling this, so dropping here
%% never loses a write. `Id` is left on the tracking list (harmless — an
%% erased key is a no-op at flush/clear).
jsv_evict(Id) ->
    erase(Id),
    erase({ctor_shape, Id}),
    erase({tc_arr, Id}),
    tc_mc_evict(Id),
    nil.

%% jsv_clear() — drop the whole overlay. Called by `apply_js_main` on
%% entry so re-applying a shared seed observes an identical fresh realm.
jsv_clear() ->
    tc_mc_clear(),
    tc_cs_clear(),
    case erase(?TC_ARR_IDS) of
        undefined -> ok;
        AIds -> _ = [erase({tc_arr, AId}) || AId <- maps:keys(AIds)], ok
    end,
    case erase(?SHAPE_OFF_IDS) of
        undefined -> ok;
        Ks -> _ = [erase(K) || K <- Ks], ok
    end,
    erase(?GID),
    case erase(?IDS) of
        undefined -> nil;
        Ids -> _ = [erase(Id) || Id <- Ids], nil
    end.

%% tc_cs_install(CId, {ShapeId,Arity} | no) — cache the learned exit shape
%% for a ctor cell (t_new_simple cold path). `no` marks a ctor whose result
%% is unshapeable so subsequent calls skip the learn step. Tracked in
%% ?TC_CS_IDS for jsv_clear sweep; per-CId eviction is via jsv_evict(CId).
tc_cs_install(CId, Entry) ->
    put({ctor_shape, CId}, Entry),
    case get(?TC_CS_IDS) of
        undefined -> put(?TC_CS_IDS, [CId]);
        L -> put(?TC_CS_IDS, [CId | L])
    end,
    ok.

tc_cs_clear() ->
    case erase(?TC_CS_IDS) of
        undefined -> ok;
        L -> _ = [erase({ctor_shape, C}) || C <- L], ok
    end.

%% jsv_overlay_slot(Id, Slot) -> Slot' — patch EVERY overlaid prop into
%% `Slot`. Cheap when nothing is cached (one pdict miss). Used by
%% `t_cell_get` so the general path sees fast-path writes, and by
%% `jsv_flush`. Accepts the poly map and (transitionally) a legacy tuple.
jsv_overlay_slot(Id, Slot0) ->
    %% perf7_arr_pdict: fold the {tc_arr,Id} overlay's {Length,Elems} into
    %% the ArrayObj slot first so t_cell_get / jsv_flush see fast-path
    %% element writes. `{tuple_dense, T}` is overlay-only — invert to Store's
    %% `{dense, array}` here (default `undefined` matches every Dense
    %% construction site, rt_obj.gleam:256/1849). Disjoint from the
    %% named-prop overlay below.
    Slot = case get({tc_arr, Id}) of
        {Length, {tuple_dense, T}}
          when element(1, Slot0) =:= ?SOBJECT_TAG,
               element(1, element(?SOBJECT_KIND, Slot0)) =:= ?ARRAYOBJ_TAG ->
            A = array:from_list(tuple_to_list(T), undefined),
            setelement(?SOBJECT_ELEMENTS,
                       setelement(?SOBJECT_KIND, Slot0, {?ARRAYOBJ_TAG, Length}),
                       {?ELEMS_DENSE, A});
        {Length, Elems}
          when element(1, Slot0) =:= ?SOBJECT_TAG,
               element(1, element(?SOBJECT_KIND, Slot0)) =:= ?ARRAYOBJ_TAG ->
            setelement(?SOBJECT_ELEMENTS,
                       setelement(?SOBJECT_KIND, Slot0, {?ARRAYOBJ_TAG, Length}),
                       Elems);
        _ -> Slot0
    end,
    case get(Id) of
        %% i-prop-ic-warm-inline: pdict[Id] is the FLAT shaped tuple
        %% (t_ic_get/set install + write here); strictly fresher than `Slot`
        %% from St. Unflatten to the nested JsSlot for the store.
        C when element(1, C) =:= ?SSHAPED_TAG -> shaped_unflat(C);
        M when is_map(M), element(1, Slot) =:= ?SSHAPED_TAG ->
            %% Flush overlay writes into the slots array by shape offset.
            %% Every overlay key was installed via peek_get/cold_set_valid
            %% → shape_offset_cached, so pdict[{shape_off,Sid,Kb}] is
            %% populated. A miss (key ∉ shape) is dropped — unreachable per
            %% the cold_set_valid gate.
            {?SSHAPED_TAG, Sid, _, Slots} = Slot,
            setelement(?SSHAPED_SLOTS, Slot, maps:fold(
                fun(Kb, V, S) ->
                    case get({shape_off, Sid, Kb}) of
                        undefined -> S;
                        Off -> setelement(Off + 1, S, V)
                    end
                end, Slots, M));
        M when is_map(M), element(1, Slot) =:= ?SOBJECT_TAG ->
            setelement(?SOBJECT_PROPS, Slot, maps:fold(fun overlay_prop/3,
                                          element(?SOBJECT_PROPS, Slot), M));
        {Kb, V} when element(1, Slot) =:= ?SOBJECT_TAG ->
            setelement(?SOBJECT_PROPS, Slot,
                       overlay_prop(Kb, V, element(?SOBJECT_PROPS, Slot)));
        _ -> Slot
    end.


%% Fold body — writes `V` into the existing DataProperty descriptor for
%% `Kb` (preserving writable/enumerable/configurable). A key that no
%% longer maps to a data_property is skipped: it can only have changed
%% shape via `t_cell_set`, which evicts first, so this arm is unreachable
%% for a coherent overlay.
overlay_prop(Kb, V, Props) ->
    case Props of
        #{{named, Kb} := Prop} when element(1, Prop) =:= ?DATAPROP_TAG ->
            Props#{{named, Kb} := setelement(?DATAPROP_VALUE, Prop, V)};
        _ -> Props
    end.

%% jsv_flush(St) -> St' — merge every cached value into `St.store.data`
%% and clear the overlay. Called before GC (so mark sees overlay handles as
%% roots) and at `apply_js_main` exit (so the returned St' is self-
%% contained). O(cached cells); typically tiny.
jsv_flush(St) ->
    tc_mc_clear(),
    tc_cs_clear(),
    Ids0 = case get(?IDS) of undefined -> []; L -> L end,
    Ids = case get(?TC_ARR_IDS) of
        undefined -> Ids0;
        A -> maps:keys(A) ++ Ids0
    end,
    case Ids of
        [] -> St;
        _ ->
            Store = element(?AGENT_STORE, St),
            Data = element(?STORE_DATA, Store),
            NewData = flush_ids(Data, Ids),
            jsv_clear(),
            setelement(?AGENT_STORE, St, setelement(?STORE_DATA, Store, NewData))
    end.

flush_ids(Data, []) -> Data;
flush_ids(Data, [Id | Ids]) ->
    case Data of
        #{Id := Slot} ->
            flush_ids(Data#{Id := jsv_overlay_slot(Id, Slot)}, Ids);
        _ -> flush_ids(Data, Ids)
    end.
