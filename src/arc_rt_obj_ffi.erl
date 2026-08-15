%%% arc_rt_obj_ffi — own-property fast-path probes for `rt_obj`
%%% (SPEC §7.M4).
%%%
%%% Hand-written Erlang, so it carries the `arc_rt_` namespace prefix
%%% (overview §5) and can NEVER collide with an OTP module — exactly like
%%% `arc_rt_call_ffi`. Pure term construction / pattern matching over the
%%% threaded `St`: no NIF, no process state, cannot crash the node.
%%%
%%% Why a shim: the emitted `.x` / `.x = v` / `a[i]` fast paths want a
%%% SINGLE probe for the common case (own writable DataProperty on an
%%% ordinary SObject, own slot on an SShapedObject, in-bounds ArrayObj
%%% element) with NO cross-module `classify`/`as_object_key`/`t_get_prop`
%%% proto-walk chain. On any shape miss the atom `miss` is returned and the
%%% emitter's guard falls back to the full `t_get_prop_any` /
%%% `t_set_prop_any` path. Reads return the bare value; writes return the
%%% rebuilt `St'`.
-module(arc_rt_obj_ffi).
-export([t_get_prop_own_data/3, t_set_prop_own_data/4,
         t_instanceof_fast/3,
         t_get_elem_fast/3, t_set_elem_fast/4,
         t_global_get_fast/2,
         shape_slots_get/2, shape_slots_set/3, shape_slots_append/2,
         shape_slots_fold/3]).

%% Record indices come from arc_rt_layout.hrl (asserted by
%% arc_rt_layout_test). Plain tuples indexed here:
%%   SShapedObject slots (plain tuple, arity = ShapeDesc.arity;
%%     element(Off+1, Slots))
%%   PropertyKey Named: {named, BinString}

-include("arc_rt_layout.hrl").

-compile({inline, [peek_get/3, slot_of/2, shape_offset/3]}).

%% t_get_prop_own_data(St, {js_cell,Id}, KeyBin) -> V | miss
%% JRead. Own DataProperty on an Ordinary SObject (kind=:=ordinary avoids
%% ArrayObj's virtual "length") or own slot on an SShapedObject. Accessors,
%% exotic kinds and absent keys → `miss` (the full path does the proto walk).
t_get_prop_own_data(St, {?HANDLE_TAG, Id}, KeyBin) ->
    peek_get(St, Id, KeyBin);
t_get_prop_own_data(_, _, _) -> miss.

%% t_global_get_fast(St, KeyBin) -> V | miss
%% JRead global-var read: own data prop on the realm's global object.
t_global_get_fast(St, KeyBin) ->
    Realm = element(?AGENT_REALM, St),
    {?HANDLE_TAG, GId} = element(?REALM_GLOBAL, Realm),
    peek_get(St, GId, KeyBin).

peek_get(St, Id, KeyBin) ->
    case slot_of(St, Id) of
        {?SSHAPED_TAG, Sid, _, Slots} ->
            case shape_offset(St, Sid, KeyBin) of
                miss -> miss;
                Off -> element(Off + 1, Slots)
            end;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG,
                  element(?SOBJECT_KIND, Slot) =:= ?ORDINARY ->
            case element(?SOBJECT_PROPS, Slot) of
                #{{?KEY_NAMED, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                    element(?DATAPROP_VALUE, Prop);
                _ -> miss
            end;
        _ -> miss
    end.

%% t_set_prop_own_data(St, {js_cell,Id}, KeyBin, V) -> St' | miss
%% JMutMiss. §10.1.9.2 OrdinarySetWithOwnDescriptor step 2 for an EXISTING
%% own writable DataProperty on an Ordinary SObject (value replaced in the
%% descriptor, attributes kept) or an own slot on an SShapedObject (all
%% shaped slots are writable data by construction). Anything else → `miss`
%% so the full `t_set_prop_any` runs. Returns the rebuilt St' (a tuple) on
%% hit; the emitter's `is_atom` guard distinguishes it from `miss`.
t_set_prop_own_data(St, {?HANDLE_TAG, Id}, KeyBin, V) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case Data of
        #{Id := Slot} ->
            case Slot of
                {?SSHAPED_TAG, Sid, P, Slots} ->
                    case shape_offset(St, Sid, KeyBin) of
                        miss -> miss;
                        Off ->
                            NewSlot = {?SSHAPED_TAG, Sid, P,
                                       setelement(Off + 1, Slots, V)},
                            setelement(?AGENT_STORE, St,
                                setelement(?STORE_DATA, Store,
                                           Data#{Id := NewSlot}))
                    end;
                _ when element(1, Slot) =:= ?SOBJECT_TAG,
                       element(?SOBJECT_KIND, Slot) =:= ?ORDINARY ->
                    Props = element(?SOBJECT_PROPS, Slot),
                    K = {?KEY_NAMED, KeyBin},
                    case Props of
                        #{K := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG,
                               element(?DATAPROP_WRITABLE, Prop) =:= true ->
                            NewProps =
                                Props#{K := setelement(?DATAPROP_VALUE, Prop, V)},
                            NewSlot = setelement(?SOBJECT_PROPS, Slot, NewProps),
                            setelement(?AGENT_STORE, St,
                                setelement(?STORE_DATA, Store,
                                           Data#{Id := NewSlot}));
                        _ -> miss
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
t_set_prop_own_data(_, _, _, _) -> miss.

%% t_instanceof_fast(St, V, Ctor) -> 0 | 1 | miss
%% JRead fast-path for §13.10.2 InstanceofOperator → §7.3.22
%% OrdinaryHasInstance. Gate: `Ctor` is an s_object with `k_function` kind
%% (NOT k_bound / proxy) and empty own `symbol_props` — so no own
%% @@hasInstance override; the inherited Function.prototype[@@hasInstance]
%% IS OrdinaryHasInstance, which this inlines — holding an own "prototype"
%% DataProperty whose value is a cell `{js_cell, PId}`. Then walk `V`'s
%% proto chain comparing cell-ids to `PId`, depth-capped at 64 hops → miss
%% so a proxy-cycle falls to the full path's RangeError. Non-cell `V` → 0
%% (§7.3.22 step 3). Any other shape → `miss` and the emitter falls back
%% to `t_instance_of`.
t_instanceof_fast(St, V, {?HANDLE_TAG, CId}) ->
    case slot_of(St, CId) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG,
                  element(?SOBJECT_SYMBOL_PROPS, Slot) =:= [] ->
            case element(?SOBJECT_KIND, Slot) of
                Kind when element(1, Kind) =:= ?KFN_TAG ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_NAMED, <<"prototype">>} := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG ->
                            case element(?DATAPROP_VALUE, Prop) of
                                {?HANDLE_TAG, PId} -> proto_has(St, V, PId, 64);
                                _ -> miss
                            end;
                        _ -> miss
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
t_instanceof_fast(_, _, _) -> miss.

%% §7.3.22 step 7 chain walk. Fuel exhaustion on a `{js_cell,_}` V → miss
%% (clause 2); non-cell V (bigint / symbol / primitive) → 0 (clause 3).
proto_has(St, {?HANDLE_TAG, VId}, PId, Fuel) when Fuel > 0 ->
    case slot_of(St, VId) of
        %% proto is element 3 for BOTH s_object and s_shaped_object.
        Slot when element(1, Slot) =:= ?SOBJECT_TAG;
                  element(1, Slot) =:= ?SSHAPED_TAG ->
            case element(?SOBJECT_PROTO, Slot) of
                ?NONE -> 0;
                {?SOME, {?HANDLE_TAG, PId}} -> 1;
                {?SOME, {?HANDLE_TAG, Next}} ->
                    proto_has(St, {?HANDLE_TAG, Next}, PId, Fuel - 1);
                _ -> miss
            end;
        _ -> miss
    end;
proto_has(_, {?HANDLE_TAG, _}, _, _) -> miss;
proto_has(_, _, _, _) -> 0.

%% ──────────────────── indexed-element fast path ────────────────────
%% deltablue OrderedCollection/Plan.execute() inner loops read
%% `this.elms[i]` on every iteration; the general path is
%% `to_property_key` (JMut, canonicalizes to {index,N}) → `t_get_prop_any`
%% (proto walk + kind dispatch). This inlines the ArrayObj Dense/Sparse
%% element read/write with a shape guard, `miss` on anything exotic.
%%   ObjKind ArrayObj: {array_obj, Length}.
%%   JsElements: no_elements | {dense, array:array()} | {sparse, #{Int=>V}}.

%% t_get_elem_fast(St, Recv, Idx) -> V | miss
%% JRead. Gate: Recv={js_cell,Id}, Idx a bare non-negative BEAM integer (the
%% JsVal wire form for a JS integer number — a float / string / bigint index
%% falls to `to_property_key`), slot is ArrayObj with Idx < Length and no
%% {index,Idx} props override. Holes (dense default / sparse-absent) miss so
%% the full path handles the proto walk. `IsAtom` on the emitter side treats
%% any atom-valued V (undefined/true/…) as a miss too — a perf loss only.
t_get_elem_fast(St, {?HANDLE_TAG, Id}, Idx)
  when is_integer(Idx), Idx >= 0 ->
    Store = element(?AGENT_STORE, St),
    case element(?STORE_DATA, Store) of
        #{Id := Slot} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                {?ARRAYOBJ_TAG, Length} when Idx < Length ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_INDEX, Idx} := _} -> miss;
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
        _ -> miss
    end;
t_get_elem_fast(_, _, _) -> miss.

%% t_set_elem_fast(St, Recv, Idx, V) -> St' | miss
%% JMutMiss. Gate: Recv={js_cell,Id} ArrayObj, Idx bare non-negative integer
%% in [0, Length] (Idx =:= Length appends and bumps Length), no {index,Idx}
%% props override, extensible=true (covers hole-fill and append). Dense
%% overwrite additionally requires Idx < array:size to avoid an unbounded
%% auto-extend. Returns the rebuilt St' (a tuple) on hit / bare `miss` atom
%% otherwise — the emitter's `is_atom` guard distinguishes them without a
%% {V,St'} 2-tuple alloc per hit.
t_set_elem_fast(St, {?HANDLE_TAG, Id}, Idx, V)
  when is_integer(Idx), Idx >= 0 ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case Data of
        #{Id := Slot}
          when element(1, Slot) =:= ?SOBJECT_TAG,
               element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
            case element(?SOBJECT_KIND, Slot) of
                {?ARRAYOBJ_TAG, Length} when Idx < Length ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_INDEX, Idx} := _} -> miss;
                        _ ->
                            case elem_write(element(?SOBJECT_ELEMENTS, Slot), Idx, V) of
                                miss -> miss;
                                NewE ->
                                    NewSlot = setelement(?SOBJECT_ELEMENTS, Slot, NewE),
                                    setelement(?AGENT_STORE, St,
                                        setelement(?STORE_DATA, Store,
                                            Data#{Id := NewSlot}))
                            end
                    end;
                {?ARRAYOBJ_TAG, Length} when Idx =:= Length ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_INDEX, Idx} := _} -> miss;
                        _ ->
                            case elem_write_grow(element(?SOBJECT_ELEMENTS, Slot), Idx, V) of
                                miss -> miss;
                                NewE ->
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

%% ── ShapeSlots FFI (rt_types.gleam) — plain-tuple slot storage. ──
%% shape_slots_get(Slots, Off) -> JsVal — 0-based offset.
shape_slots_get(Slots, Off) -> element(Off + 1, Slots).

%% shape_slots_set(Slots, Off, V) -> Slots' — overwrite the 0-based slot.
shape_slots_set(Slots, Off, V) -> setelement(Off + 1, Slots, V).

%% shape_slots_append(Slots, V) -> Slots' — the slot a shape transition adds.
shape_slots_append(Slots, V) -> erlang:append_element(Slots, V).

%% shape_slots_fold(Slots, Acc, F) -> Acc' — fold F(Off, V, A) over every
%% slot. Mirrors the tree_array.sparse_fold contract used by rt_gc.
shape_slots_fold(Slots, Acc, F) ->
    shape_slots_fold_1(Slots, Acc, F, 1, tuple_size(Slots)).
shape_slots_fold_1(_, Acc, _, I, N) when I > N -> Acc;
shape_slots_fold_1(Slots, Acc, F, I, N) ->
    shape_slots_fold_1(Slots, F(I - 1, element(I, Slots), Acc), F, I + 1, N).

%% shape_offset(St, ShapeId, KeyBin) -> Off | miss
%% ShapeDesc.offsets lookup in JsStore.shapes.
%%   ShapeDesc = {shape_desc, Arity, #{KeyBin=>Off}, #{KeyBin=>ToSid}}.
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

%% Read the slot for `Id` from `St.store.data`. `miss` if absent (a
%% dangling handle).
slot_of(St, Id) ->
    case element(?STORE_DATA, element(?AGENT_STORE, St)) of
        #{Id := Slot} -> Slot;
        _ -> miss
    end.
