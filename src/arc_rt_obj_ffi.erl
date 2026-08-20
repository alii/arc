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
%%% rebuilt `St'`. The interpreter's kernels (arc_interp_ffi) share the
%%% proto-chain predicate `named_free` and `store_put_seq`.
-module(arc_rt_obj_ffi).
-export([t_get_prop_own_data/3, t_set_prop_own_data/4, t_set_prop_named/5,
         t_create_data_prop/4, store_put_seq/3,
         t_get_prop_ic/4, t_get_prop_ic_miss/4, t_get_prop_slow/4,
         t_get_prop_site/4,
         t_instanceof_fast/3,
         t_get_elem_fast/3, t_set_elem_fast/4,
         t_global_get_fast/2, t_global_get/2,
         named_free/5,
         shape_slots_get/2, shape_slots_set/3, shape_slots_append/2,
         shape_slots_fold/3]).

%% Record indices come from arc_rt_layout.hrl (asserted by
%% arc_rt_layout_test). Plain tuples indexed here:
%%   SShapedObject slots (plain tuple, arity = ShapeDesc.arity;
%%     element(Off+1, Slots))
%%   PropertyKey Named: {named, BinString}

-include("arc_rt_layout.hrl").

-compile({inline, [peek_get/3, slot_of/2, shape_offset/3, get_any/3,
                   named_plain/2, store_put_seq/3, index_read/2, peek_slot/3,
                   index_write/4, elem_write/3]}).

%% IcEntry (rt_types.gleam): {ic_read, Sid, Off, KeyBin}.
-define(IC_READ, ic_read).

%% t_get_prop_own_data(St, {js_cell,Id}, KeyBin) -> V | miss
%% JRead. Own DataProperty on an Ordinary SObject (kind=:=ordinary avoids
%% ArrayObj's virtual "length") or own slot on an SShapedObject. Accessors,
%% exotic kinds and absent keys → `miss` (the full path does the proto walk).
t_get_prop_own_data(St, {?HANDLE_TAG, Id}, KeyBin) ->
    peek_get(St, Id, KeyBin);
t_get_prop_own_data(_, _, _) -> miss.

%% t_get_prop_ic(St, {js_cell,Id}, KeyBin, Site) -> V | miss
%% JRead. Warm inline-cache hit for a compiled `.key` read site: the entry
%% installed at `Site` names the shape it was seen on and the slot offset
%% the key has in that shape (`arc/rt/types.IcEntry`). Anything else → `miss`
%% and the emitter runs `t_get_prop_ic_miss`.
t_get_prop_ic(St, {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    case element(?STORE_ICS, Store) of
        #{Site := {?IC_READ, Sid, Off, KeyBin}} ->
            case array:get(Id, element(?STORE_DATA, Store)) of
                {?SSHAPED_TAG, Sid, _, Slots} -> element(Off + 1, Slots);
                _ -> miss
            end;
        _ -> miss
    end;
t_get_prop_ic(_, _, _, _) -> miss.

%% t_get_prop_ic_miss(St, Recv, KeyBin, Site) -> {V | miss, St'}
%% JMut. The own-data probe on the receiver, filling an empty `Site` from a
%% shaped own hit. Kept for the host-op table; the emitted read sites use
%% `t_get_prop_site` / `t_get_prop_slow`, which also walk the proto chain.
t_get_prop_ic_miss(St, {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    case array:get(Id, element(?STORE_DATA, Store)) of
        {?SSHAPED_TAG, Sid, _, Slots} ->
            case shape_offset(St, Sid, KeyBin) of
                miss -> {miss, St};
                Off ->
                    {element(Off + 1, Slots),
                     ic_fill(St, Store, Site, Sid, Off, KeyBin)}
            end;
        Slot -> {peek_slot(St, Slot, KeyBin), St}
    end;
t_get_prop_ic_miss(St, _, _, _) -> {miss, St}.

%% t_get_prop_slow(St, Recv, KeyBin, Site) -> {V, St'}
%% JMut. Everything past the IC hit for a compiled `.key` read, on ONE read
%% of the receiver cell: the own slot / own DataProperty (a shaped own hit
%% fills `Site` when it is still empty), then the §10.1.8.1 proto walk while
%% every hop is a plain data lookup (`proto_read`), and only an accessor, an
%% exotic hop or a primitive receiver's wrapper miss reaches the Gleam
%% [[Get]]. The emitter runs `t_get_prop_ic` (JRead, no St alloc on a hit)
%% and calls this on `miss`.
t_get_prop_slow(St, Recv = {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    read_named(St, Store, Data, Recv, array:get(Id, Data), KeyBin, Site);
t_get_prop_slow(St, Recv, KeyBin, _) -> read_prim(St, Recv, KeyBin).

%% t_get_prop_site(St, Recv, KeyBin, Site) -> {V, St'}
%% JMut. The whole compiled `.key` read at one site: IC hit on the slot read
%% here, else `read_named` on that same slot.
t_get_prop_site(St, Recv = {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    Slot = array:get(Id, Data),
    case element(?STORE_ICS, Store) of
        #{Site := {?IC_READ, Sid, Off, KeyBin}}
          when element(1, Slot) =:= ?SSHAPED_TAG,
               element(?SSHAPED_SID, Slot) =:= Sid ->
            {element(Off + 1, element(?SSHAPED_SLOTS, Slot)), St};
        _ -> read_named(St, Store, Data, Recv, Slot, KeyBin, Site)
    end;
t_get_prop_site(St, Recv, KeyBin, _) -> read_prim(St, Recv, KeyBin).

%% The read past the IC probe, on the receiver's already-read Slot.
read_named(St, Store, Data, Recv, {?SSHAPED_TAG, Sid, Proto, Slots}, KeyBin,
           Site) ->
    Shapes = element(?STORE_SHAPES, Store),
    case Shapes of
        #{Sid := Desc} ->
            case element(?SHAPE_OFFSETS, Desc) of
                #{KeyBin := Off} ->
                    {element(Off + 1, Slots),
                     ic_fill(St, Store, Site, Sid, Off, KeyBin)};
                _ -> read_proto(St, Data, Shapes, Proto, Recv, KeyBin)
            end;
        _ -> get_any(St, Recv, KeyBin)
    end;
read_named(St, Store, Data, Recv, Slot, KeyBin, _)
  when element(1, Slot) =:= ?SOBJECT_TAG ->
    Kind = element(?SOBJECT_KIND, Slot),
    case named_plain(Kind, KeyBin) of
        true ->
            case element(?SOBJECT_PROPS, Slot) of
                #{{?KEY_NAMED, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                    {element(?DATAPROP_VALUE, Prop), St};
                #{{?KEY_NAMED, KeyBin} := _} -> get_any(St, Recv, KeyBin);
                _ ->
                    read_proto(St, Data, element(?STORE_SHAPES, Store),
                               element(?SOBJECT_PROTO, Slot), Recv, KeyBin)
            end;
        %% §10.4.2 Array "length" always tracks ArrayObj.length (a props
        %% entry only overrides its attributes), an integer JsVal as is.
        false when element(1, Kind) =:= ?ARRAYOBJ_TAG ->
            {element(?ARRAYOBJ_LENGTH, Kind), St};
        false -> get_any(St, Recv, KeyBin)
    end;
read_named(St, _, _, Recv, _, KeyBin, _) -> get_any(St, Recv, KeyBin).

read_proto(St, Data, Shapes, Proto, Recv, KeyBin) ->
    case proto_read(Data, Shapes, Proto, KeyBin, 64) of
        miss -> get_any(St, Recv, KeyBin);
        V -> {V, St}
    end.

%% A string / number primitive has no own named props besides String
%% "length" (§10.4.3.5), so a data read walks the realm's wrapper prototype;
%% a getter there misses so the full path passes the primitive as `this`.
read_prim(St, Bin, <<"length">>) when is_binary(Bin) ->
    {arc_string_ffi:string_codepoint_length(Bin), St};
read_prim(St, Bin, KeyBin) when is_binary(Bin) ->
    read_wrapper(St, ?REALM_STRING, Bin, KeyBin);
read_prim(St, N, KeyBin) when is_number(N) ->
    read_wrapper(St, ?REALM_NUMBER, N, KeyBin);
read_prim(St, Recv, KeyBin) -> get_any(St, Recv, KeyBin).

read_wrapper(St, Which, Recv, KeyBin) ->
    Pair = element(Which, element(?AGENT_REALM, St)),
    Store = element(?AGENT_STORE, St),
    read_proto(St, element(?STORE_DATA, Store), element(?STORE_SHAPES, Store),
               {?SOME, element(?PAIR_PROTO, Pair)}, Recv, KeyBin).

get_any(St, Recv, KeyBin) ->
    'arc@rt@obj':t_get_prop_any(St, Recv, {?OKEY_STRING, {?KEY_NAMED, KeyBin}}).

%% proto_read(Data, Shapes, Proto, KeyBin, Fuel) -> V | undefined | miss
%% §10.1.8.1 OrdinaryGet steps 1-3 from the prototype link `Proto` down,
%% while every hop is a plain lookup: an own slot on an SShapedObject, or an
%% own DataProperty in an SObject's props map for a kind whose named keys
%% are not virtual (`named_plain`). An accessor, an exotic hop, a dangling
%% handle or more than Fuel hops miss; absent on the whole chain is
%% `undefined`, exactly as OrdinaryGet answers.
proto_read(_, _, ?NONE, _, _) -> undefined;
proto_read(_, _, _, _, 0) -> miss;
proto_read(Data, Shapes, {?SOME, {?HANDLE_TAG, Id}}, KeyBin, Fuel) ->
    case array:get(Id, Data) of
        {?SSHAPED_TAG, Sid, Proto, Slots} ->
            case Shapes of
                #{Sid := Desc} ->
                    case element(?SHAPE_OFFSETS, Desc) of
                        #{KeyBin := Off} -> element(Off + 1, Slots);
                        _ -> proto_read(Data, Shapes, Proto, KeyBin, Fuel - 1)
                    end;
                _ -> miss
            end;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Slot), KeyBin) of
                false -> miss;
                true ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_NAMED, KeyBin} := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG ->
                            element(?DATAPROP_VALUE, Prop);
                        #{{?KEY_NAMED, KeyBin} := _} -> miss;
                        _ ->
                            proto_read(Data, Shapes,
                                       element(?SOBJECT_PROTO, Slot), KeyBin,
                                       Fuel - 1)
                    end
            end;
        _ -> miss
    end;
proto_read(_, _, _, _, _) -> miss.

%% Whether a Named key on this ObjKind is a plain props-map entry for
%% [[Get]], [[Set]] and property creation (rt/obj own_property_of, get_from,
%% set_from): Proxy, module namespace and TypedArray cells are exotic for
%% string keys, and Array / String objects synthesize "length".
named_plain(?ORDINARY, _) -> true;
named_plain(Kind, _) when is_atom(Kind) -> true;
named_plain(Kind, KeyBin) ->
    case element(1, Kind) of
        ?PROXYOBJ_TAG -> false;
        module_namespace -> false;
        typed_array_obj -> false;
        ?ARRAYOBJ_TAG -> KeyBin =/= <<"length">>;
        string_obj -> KeyBin =/= <<"length">>;
        _ -> true
    end.

%% Install the read entry at an empty Site. A site keeps its first entry
%% (a mismatch is a plain probe, never a re-install), so a polymorphic site
%% never churns the store. `none`: no site to fill.
ic_fill(St, _, none, _, _, _) -> St;
ic_fill(St, Store, Site, Sid, Off, KeyBin) ->
    Ics = element(?STORE_ICS, Store),
    case is_map_key(Site, Ics) of
        true -> St;
        false ->
            setelement(?AGENT_STORE, St,
                       setelement(?STORE_ICS, Store,
                                  Ics#{Site => {?IC_READ, Sid, Off, KeyBin}}))
    end.

%% t_global_get_fast(St, KeyBin) -> V | miss
%% JRead global-var read: own data prop on the realm's global object.
t_global_get_fast(St, KeyBin) ->
    {?HANDLE_TAG, GId} = element(?REALM_GLOBAL, element(?AGENT_REALM, St)),
    Store = element(?AGENT_STORE, St),
    peek_slot(St, array:get(GId, element(?STORE_DATA, Store)), KeyBin).

%% t_global_get(St, KeyBin) -> {V, St'}
%% JMut kernel behind the emitted `global_get` host op: the own-data probe
%% above, then the full spec read (proto walk / accessors / ReferenceError)
%% on a miss. One call per site instead of probe + is_atom + branch.
t_global_get(St, KeyBin) ->
    case t_global_get_fast(St, KeyBin) of
        miss -> arc@rt@obj:t_global_get(St, KeyBin);
        V -> {V, St}
    end.

peek_get(St, Id, KeyBin) ->
    peek_slot(St, slot_of(St, Id), KeyBin).

%% Own data value of an already-read slot: a shaped slot, or a DataProperty
%% in the props map of a kind whose named keys are plain. `miss` otherwise.
peek_slot(St, {?SSHAPED_TAG, Sid, _, Slots}, KeyBin) ->
    case shape_offset(St, Sid, KeyBin) of
        miss -> miss;
        Off -> element(Off + 1, Slots)
    end;
peek_slot(_, Slot, KeyBin) when element(1, Slot) =:= ?SOBJECT_TAG ->
    case named_plain(element(?SOBJECT_KIND, Slot), KeyBin) of
        true ->
            case element(?SOBJECT_PROPS, Slot) of
                #{{?KEY_NAMED, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                    element(?DATAPROP_VALUE, Prop);
                _ -> miss
            end;
        false -> miss
    end;
peek_slot(_, _, _) -> miss.

%% t_set_prop_own_data(St, {js_cell,Id}, KeyBin, V) -> St' | miss
%% JMutMiss. §10.1.9.1 OrdinarySet for a Named key when it lands on the
%% receiver as plain data, on ONE read of the receiver cell:
%%  * SShapedObject: overwrite an own slot, or ADD a field along the shape's
%%    existing transition edge for KeyBin when the proto chain holds nothing
%%    but writable data at KeyBin (`named_free`, steps 1-2) — the move
%%    `set_own_shaped` makes; the first-ever transition still misses so shape
%%    minting stays in Gleam.
%%  * SObject of a kind whose named keys are plain (`named_plain`): replace
%%    the value of an EXISTING own writable DataProperty (attributes and seq
%%    kept, §10.1.9.2 step 2.c-d), or CREATE {V, W, E, C} on an extensible
%%    receiver when the key is absent and the proto chain is `named_free`
%%    (step 2.e, CreateDataProperty), stamped with the store's prop_seq.
%% A non-writable property, an own accessor, a setter / read-only property up
%% the chain, a non-extensible receiver for a new key and every exotic
%% receiver → `miss` so the full `t_set_prop_any` runs. Returns the rebuilt
%% St' (a tuple) on hit; the emitter's `is_atom` guard distinguishes it.
t_set_prop_own_data(St, {?HANDLE_TAG, Id}, KeyBin, V) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case array:get(Id, Data) of
        {?SSHAPED_TAG, Sid, P, Slots} ->
            Shapes = element(?STORE_SHAPES, Store),
            case shaped_write(Data, Shapes, Sid, P, Slots, KeyBin, V) of
                miss -> miss;
                NewSlot ->
                    setelement(?AGENT_STORE, St,
                        setelement(?STORE_DATA, Store,
                                   array:set(Id, NewSlot, Data)))
            end;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Slot), KeyBin) of
                false -> miss;
                true ->
                    Props = element(?SOBJECT_PROPS, Slot),
                    K = {?KEY_NAMED, KeyBin},
                    case Props of
                        #{K := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG,
                               element(?DATAPROP_WRITABLE, Prop) =:= true ->
                            NewProp = setelement(?DATAPROP_VALUE, Prop, V),
                            NewSlot = setelement(?SOBJECT_PROPS, Slot,
                                                 Props#{K := NewProp}),
                            setelement(?AGENT_STORE, St,
                                setelement(?STORE_DATA, Store,
                                           array:set(Id, NewSlot, Data)));
                        #{K := _} -> miss;
                        _ when element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
                            case named_free(Data, element(?STORE_SHAPES, Store),
                                            element(?SOBJECT_PROTO, Slot),
                                            K, 64) of
                                false -> miss;
                                true ->
                                    Seq = element(?STORE_PROP_SEQ, Store),
                                    Prop = {?DATAPROP_TAG, V, true, true, true,
                                            Seq},
                                    NewSlot = setelement(?SOBJECT_PROPS, Slot,
                                                         Props#{K => Prop}),
                                    setelement(?AGENT_STORE, St,
                                        store_put_seq(Store,
                                            array:set(Id, NewSlot, Data),
                                            Seq + 1))
                            end;
                        _ -> miss
                    end
            end;
        _ -> miss
    end;
t_set_prop_own_data(_, _, _, _) -> miss.

%% store_put_seq(Store, Data, Seq) -> Store2
%% Store with new data and prop_seq; the arity guard lets the compiler build
%% the updated store as one tuple.
store_put_seq(Store, Data, Seq) when tuple_size(Store) =:= ?STORE_ARITY ->
    setelement(?STORE_PROP_SEQ, setelement(?STORE_DATA, Store, Data), Seq).

%% t_set_prop_named(St, Obj, KeyBin, V, Strict) -> St'
%% JMutUnit. The whole `.key = v` write in one call: the own-data probe above,
%% then the full `t_set_prop_any` / `t_set_prop_strict` on miss. Yields only
%% the rebound St'; the emitter already holds V.
t_set_prop_named(St, Obj, KeyBin, V, Strict) ->
    case t_set_prop_own_data(St, Obj, KeyBin, V) of
        miss ->
            Key = {?KEY_NAMED, KeyBin},
            {_, St1} = case Strict of
                true -> 'arc@rt@obj':t_set_prop_strict(St, Obj, Key, V);
                false -> 'arc@rt@obj':t_set_prop_any(St, Obj, Key, V)
            end,
            St1;
        St1 -> St1
    end.

%% t_create_data_prop(St, Recv, Key, V) -> {true, St'}
%% JMut. §7.3.5 CreateDataProperty behind the `define_prop` host op (object
%% literal members, class fields): a NEW plain key — Named on a kind whose
%% named keys are plain, or Index on a kind whose indices are plain props —
%% on an extensible SObject is one props insert stamped with prop_seq; a
%% shaped receiver overwrites its slot or moves along an existing transition
%% edge ([[DefineOwnProperty]] never consults the proto chain). Key is the
%% SPEC§8 wire key (bare PropertyKey or {string_key, PropertyKey}). An
%% existing key, a first-ever transition, a symbol and every exotic receiver
%% take `arc@rt@obj:t_create_data_prop_slow`.
t_create_data_prop(St, Recv = {?HANDLE_TAG, Id}, Key, V) ->
    PK = case Key of
        {?OKEY_STRING, K} -> K;
        K -> K
    end,
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    Slot = array:get(Id, Data),
    R = case PK of
        {?KEY_NAMED, KeyBin} when element(1, Slot) =:= ?SSHAPED_TAG ->
            shaped_define(element(?STORE_SHAPES, Store), Slot, KeyBin, V);
        {?KEY_NAMED, KeyBin} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Slot), KeyBin) of
                true -> plain_define(Slot, PK, V, Store);
                false -> miss
            end;
        {?KEY_INDEX, _} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case index_in_props(element(?SOBJECT_KIND, Slot)) of
                true -> plain_define(Slot, PK, V, Store);
                false -> miss
            end;
        _ -> miss
    end,
    case R of
        miss -> 'arc@rt@obj':t_create_data_prop_slow(St, Recv, Key, V);
        {seq, NewSlot, Seq} ->
            {true, setelement(?AGENT_STORE, St,
                              store_put_seq(Store, array:set(Id, NewSlot, Data),
                                            Seq))};
        NewSlot ->
            {true, setelement(?AGENT_STORE, St,
                              setelement(?STORE_DATA, Store,
                                         array:set(Id, NewSlot, Data)))}
    end;
t_create_data_prop(St, Recv, Key, V) ->
    'arc@rt@obj':t_create_data_prop_slow(St, Recv, Key, V).

%% New {V, W, E, C} entry under an absent key on an extensible SObject.
plain_define(Slot, PK, V, Store) ->
    Seq = element(?STORE_PROP_SEQ, Store),
    Props = element(?SOBJECT_PROPS, Slot),
    case is_map_key(PK, Props) orelse
         element(?SOBJECT_EXTENSIBLE, Slot) =/= true of
        true -> miss;
        false ->
            {seq, setelement(?SOBJECT_PROPS, Slot,
                             Props#{PK => {?DATAPROP_TAG, V, true, true, true,
                                           Seq}}),
             Seq + 1}
    end.

%% Overwrite the slot, or append along the shape's existing transition edge.
shaped_define(Shapes, {?SSHAPED_TAG, Sid, P, Slots}, KeyBin, V) ->
    case Shapes of
        #{Sid := Desc} ->
            case element(?SHAPE_OFFSETS, Desc) of
                #{KeyBin := Off} ->
                    {?SSHAPED_TAG, Sid, P, setelement(Off + 1, Slots, V)};
                _ ->
                    case element(?SHAPE_TRANSITIONS, Desc) of
                        #{KeyBin := To} ->
                            {?SSHAPED_TAG, To, P,
                             erlang:append_element(Slots, V)};
                        _ -> miss
                    end
            end;
        _ -> miss
    end.

%% t_instanceof_fast(St, V, Ctor) -> 0 | 1 | miss
%% JRead fast-path for §13.10.2 InstanceofOperator → §7.3.22
%% OrdinaryHasInstance. Gate: `Ctor` is an s_object with `k_compiled` kind
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
%% (clause 2); non-cell V (bigint / symbol / primitive) → 0 (clause 3). A
%% Proxy anywhere on the chain → miss: its [[GetPrototypeOf]] is a trap
%% (§10.5.1), never the stored proto field.
proto_has(St, {?HANDLE_TAG, VId}, PId, Fuel) when Fuel > 0 ->
    case slot_of(St, VId) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG,
                  element(1, element(?SOBJECT_KIND, Slot)) =:= ?PROXYOBJ_TAG ->
            miss;
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
%% element read/write with a shape guard, the plain-object `{index,N}` props
%% entry, and a string key as the named read/write; `miss` on anything
%% exotic.
%%   ObjKind ArrayObj: {array_obj, Length}.
%%   JsElements: no_elements | {dense, array:array()} | {sparse, #{Int=>V}}.

%% The largest array index, 2^32-2 (rt_types.max_array_index): a larger
%% integer key is the Named string key of its decimal digits, never {index,N}.
-define(MAX_ARRAY_INDEX, 4294967294).

%% t_get_elem_fast(St, Recv, Key) -> V | miss
%% JRead. Recv={js_cell,Id}. Key a bare BEAM integer array index (the JsVal
%% wire form for a JS integer number, or an integral float): an ArrayObj
%% element with Idx < Length and no {index,Idx} props override, or the own
%% `{index,Idx}` DataProperty of an object whose indices are plain props.
%% Key a string: canonicalized, then the named data read (own, then the
%% proto walk) or the index read. Holes and absent keys miss so the full
%% path does the proto walk; a fractional / negative / symbol / object key
%% misses to `to_property_key`. `IsAtom` on the emitter side treats any
%% atom-valued V (undefined/true/…) as a miss too — a perf loss only.
t_get_elem_fast(St, {?HANDLE_TAG, Id}, Idx)
  when is_integer(Idx), Idx >= 0, Idx =< ?MAX_ARRAY_INDEX ->
    Store = element(?AGENT_STORE, St),
    index_read(array:get(Id, element(?STORE_DATA, Store)), Idx);
%% Integral float index (8/2, Math.floor(x)): same element, canonicalized
%% like CanonicalNumericIndexString (-0.0 → 0; a huge one fails Idx < Length).
t_get_elem_fast(St, Recv, Idx)
  when is_float(Idx), Idx >= 0.0, Idx == trunc(Idx) ->
    t_get_elem_fast(St, Recv, trunc(Idx));
t_get_elem_fast(St, {?HANDLE_TAG, Id}, Key) when is_binary(Key) ->
    case arc_rt_val_ffi:t_to_property_key_fast(Key) of
        {?OKEY_STRING, {?KEY_NAMED, KeyBin}} ->
            Store = element(?AGENT_STORE, St),
            Data = element(?STORE_DATA, Store),
            case named_read(Data, element(?STORE_SHAPES, Store),
                            array:get(Id, Data), KeyBin) of
                undefined -> miss;
                V -> V
            end;
        {?OKEY_STRING, {?KEY_INDEX, Idx}} ->
            Store = element(?AGENT_STORE, St),
            index_read(array:get(Id, element(?STORE_DATA, Store)), Idx);
        _ -> miss
    end;
t_get_elem_fast(_, _, _) -> miss.

%% Own element / index property of an already-read slot; `miss` when absent
%% or not plain data.
index_read(Slot, Idx) when element(1, Slot) =:= ?SOBJECT_TAG ->
    case element(?SOBJECT_KIND, Slot) of
        {?ARRAYOBJ_TAG, Length} when Idx < Length ->
            case element(?SOBJECT_PROPS, Slot) of
                #{{?KEY_INDEX, Idx} := _} -> miss;
                _ ->
                    case element(?SOBJECT_ELEMENTS, Slot) of
                        {?ELEMS_DENSE, A} ->
                            case array:get(Idx, A) of
                                ?ELEMS_HOLE -> miss;
                                V -> V
                            end;
                        {?ELEMS_SPARSE, #{Idx := V}} -> V;
                        _ -> miss
                    end
            end;
        Kind ->
            case index_in_props(Kind) of
                true ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_INDEX, Idx} := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG ->
                            element(?DATAPROP_VALUE, Prop);
                        _ -> miss
                    end;
                false -> miss
            end
    end;
index_read(_, _) -> miss.

%% The value `read_named` would produce without St: own data, then the
%% plain proto walk. `miss` for anything that needs the full [[Get]].
named_read(Data, Shapes, {?SSHAPED_TAG, Sid, Proto, Slots}, KeyBin) ->
    case Shapes of
        #{Sid := Desc} ->
            case element(?SHAPE_OFFSETS, Desc) of
                #{KeyBin := Off} -> element(Off + 1, Slots);
                _ -> proto_read(Data, Shapes, Proto, KeyBin, 64)
            end;
        _ -> miss
    end;
named_read(Data, Shapes, Slot, KeyBin) when element(1, Slot) =:= ?SOBJECT_TAG ->
    Kind = element(?SOBJECT_KIND, Slot),
    case named_plain(Kind, KeyBin) of
        true ->
            case element(?SOBJECT_PROPS, Slot) of
                #{{?KEY_NAMED, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                    element(?DATAPROP_VALUE, Prop);
                #{{?KEY_NAMED, KeyBin} := _} -> miss;
                _ ->
                    proto_read(Data, Shapes, element(?SOBJECT_PROTO, Slot),
                               KeyBin, 64)
            end;
        false when element(1, Kind) =:= ?ARRAYOBJ_TAG ->
            element(?ARRAYOBJ_LENGTH, Kind);
        false -> miss
    end;
named_read(_, _, _, _) -> miss.

%% Whether an integer-index key on this ObjKind is a plain props-map entry
%% (rt/obj own_property_of, set_own_string): Array / Arguments keep indices
%% in `elements` (Arguments may map them), String indices are virtual,
%% TypedArray / Proxy / module namespace are exotic.
index_in_props(Kind) when is_atom(Kind) -> true;
index_in_props(Kind) ->
    case element(1, Kind) of
        ?ARRAYOBJ_TAG -> false;
        ?ARGUMENTSOBJ_TAG -> false;
        string_obj -> false;
        typed_array_obj -> false;
        ?PROXYOBJ_TAG -> false;
        module_namespace -> false;
        _ -> true
    end.

%% t_set_elem_fast(St, Recv, Key, V) -> St' | miss
%% JMutMiss. Recv={js_cell,Id}. Key a bare integer array index: on an
%% extensible ArrayObj, Idx in [0, Length] (Idx =:= Length appends and bumps
%% Length), no {index,Idx} props override (covers hole-fill and append; a
%% dense overwrite additionally requires Idx < array:size to avoid an
%% unbounded auto-extend); on an object whose indices are plain props, the
%% `{index,Idx}` overwrite / create of `t_set_prop_own_data`. Key a string:
%% canonicalized, then the named or the index write. Returns the rebuilt St'
%% (a tuple) on hit / bare `miss` atom otherwise — the emitter's `is_atom`
%% guard distinguishes them without a {V,St'} 2-tuple alloc per hit.
t_set_elem_fast(St, {?HANDLE_TAG, Id}, Idx, V)
  when is_integer(Idx), Idx >= 0, Idx =< ?MAX_ARRAY_INDEX ->
    index_write(St, Id, Idx, V);
t_set_elem_fast(St, Recv, Idx, V)
  when is_float(Idx), Idx >= 0.0, Idx == trunc(Idx) ->
    t_set_elem_fast(St, Recv, trunc(Idx), V);
t_set_elem_fast(St, Recv = {?HANDLE_TAG, Id}, Key, V) when is_binary(Key) ->
    case arc_rt_val_ffi:t_to_property_key_fast(Key) of
        {?OKEY_STRING, {?KEY_NAMED, KeyBin}} ->
            t_set_prop_own_data(St, Recv, KeyBin, V);
        {?OKEY_STRING, {?KEY_INDEX, Idx}} -> index_write(St, Id, Idx, V);
        _ -> miss
    end;
t_set_elem_fast(_, _, _, _) -> miss.

index_write(St, Id, Idx, V) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case array:get(Id, Data) of
        Slot when tuple_size(Slot) =:= ?SOBJECT_ARITY,
                  element(1, Slot) =:= ?SOBJECT_TAG,
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
                                            array:set(Id, NewSlot, Data)))
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
                                            array:set(Id, NewSlot, Data)))
                            end
                    end;
                Kind ->
                    case index_in_props(Kind) of
                        true ->
                            index_prop_write(St, Store, Data, Id, Slot, Idx, V);
                        false -> miss
                    end
            end;
        %% Non-extensible: only an existing plain index prop is written.
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case index_in_props(element(?SOBJECT_KIND, Slot)) of
                true ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_INDEX, Idx} := _} ->
                            index_prop_write(St, Store, Data, Id, Slot, Idx, V);
                        _ -> miss
                    end;
                false -> miss
            end;
        _ -> miss
    end.

%% `obj[Idx] = v` on an object whose indices are plain props: the
%% `t_set_prop_own_data` overwrite / create arms with an `{index,Idx}` key.
index_prop_write(St, Store, Data, Id, Slot, Idx, V) ->
    Props = element(?SOBJECT_PROPS, Slot),
    K = {?KEY_INDEX, Idx},
    case Props of
        #{K := Prop}
          when element(1, Prop) =:= ?DATAPROP_TAG,
               element(?DATAPROP_WRITABLE, Prop) =:= true ->
            NewProp = setelement(?DATAPROP_VALUE, Prop, V),
            NewSlot = setelement(?SOBJECT_PROPS, Slot, Props#{K := NewProp}),
            setelement(?AGENT_STORE, St,
                       setelement(?STORE_DATA, Store,
                                  array:set(Id, NewSlot, Data)));
        #{K := _} -> miss;
        _ when element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
            case index_free(Data, element(?SOBJECT_PROTO, Slot), Idx, 64) of
                false -> miss;
                true ->
                    Seq = element(?STORE_PROP_SEQ, Store),
                    Prop = {?DATAPROP_TAG, V, true, true, true, Seq},
                    NewSlot =
                        setelement(?SOBJECT_PROPS, Slot, Props#{K => Prop}),
                    setelement(?AGENT_STORE, St,
                               store_put_seq(Store,
                                             array:set(Id, NewSlot, Data),
                                             Seq + 1))
            end;
        _ -> miss
    end.

%% index_free(Data, Proto, Idx, Fuel) -> boolean()
%% `named_free` for an integer index: every hop either lacks an own property
%% at Idx or holds writable data there. A shaped hop never has index keys;
%% an Array / unmapped Arguments hop answers from its props override (a
%% present element is writable data, so the walk just goes on); String /
%% TypedArray / Proxy / namespace / mapped Arguments hops, an accessor or
%% read-only index, or more than Fuel hops → false.
index_free(_, ?NONE, _, _) -> true;
index_free(_, _, _, 0) -> false;
index_free(Data, {?SOME, {?HANDLE_TAG, PId}}, Idx, Fuel) ->
    case array:get(PId, Data) of
        {?SSHAPED_TAG, _, P2, _} -> index_free(Data, P2, Idx, Fuel - 1);
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            Kind = element(?SOBJECT_KIND, Slot),
            Walk = index_in_props(Kind) orelse element(1, Kind) =:= ?ARRAYOBJ_TAG
                orelse (element(1, Kind) =:= ?ARGUMENTSOBJ_TAG
                        andalso element(?ARGUMENTSOBJ_MAPPED, Kind) =:= ?NONE),
            case Walk of
                false -> false;
                true ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_INDEX, Idx} := Prop} ->
                            element(1, Prop) =:= ?DATAPROP_TAG andalso
                                element(?DATAPROP_WRITABLE, Prop) =:= true;
                        _ ->
                            index_free(Data, element(?SOBJECT_PROTO, Slot),
                                       Idx, Fuel - 1)
                    end
            end;
        _ -> false
    end;
index_free(_, _, _, _) -> false.

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

%% shaped_write(Data, Shapes, Sid, Proto, Slots, KeyBin, V) -> Slot' | miss
%% Overwrite an existing slot in place, or append along the cached
%% transition edge when the proto chain cannot intercept the write.
shaped_write(Data, Shapes, Sid, P, Slots, KeyBin, V) ->
    case Shapes of
        #{Sid := Desc} ->
            case element(?SHAPE_OFFSETS, Desc) of
                #{KeyBin := Off} ->
                    {?SSHAPED_TAG, Sid, P, setelement(Off + 1, Slots, V)};
                _ ->
                    case element(?SHAPE_TRANSITIONS, Desc) of
                        #{KeyBin := To} ->
                            case named_free(Data, Shapes, P,
                                            {?KEY_NAMED, KeyBin}, 64) of
                                true ->
                                    {?SSHAPED_TAG, To, P,
                                     erlang:append_element(Slots, V)};
                                false -> miss
                            end;
                        _ -> miss
                    end
            end;
        _ -> miss
    end.

%% named_free(Data, Shapes, Proto, K, Fuel) -> boolean()
%% True when every object on the proto chain from `Proto` down either lacks
%% an own property at the Named key K or holds a writable data property
%% there (a shaped slot always is), along hops whose named lookup is a pure
%% slots/props probe, so §10.1.9.2 lands on the receiver. An accessor, a
%% read-only property, an exotic hop (Proxy, TypedArray, Array "length", …),
%% a dangling handle or a chain deeper than Fuel → false. K is the props-map
%% key `{named, KeyBin}`, built once by the caller rather than per hop.
named_free(_, _, ?NONE, _, _) -> true;
named_free(_, _, _, _, 0) -> false;
named_free(Data, Shapes, {?SOME, {?HANDLE_TAG, PId}}, K, Fuel) ->
    case array:get(PId, Data) of
        {?SSHAPED_TAG, Sid, P2, _} ->
            case Shapes of
                #{Sid := Desc} ->
                    case element(?SHAPE_OFFSETS, Desc) of
                        #{element(2, K) := _} -> true;
                        _ -> named_free(Data, Shapes, P2, K, Fuel - 1)
                    end;
                _ -> false
            end;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Slot), element(2, K)) of
                true ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{K := Prop} ->
                            element(1, Prop) =:= ?DATAPROP_TAG andalso
                                element(?DATAPROP_WRITABLE, Prop) =:= true;
                        _ ->
                            named_free(Data, Shapes,
                                       element(?SOBJECT_PROTO, Slot),
                                       K, Fuel - 1)
                    end;
                false -> false
            end;
        _ -> false
    end;
named_free(_, _, _, _, _) -> false.

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
    case array:get(Id, element(?STORE_DATA, element(?AGENT_STORE, St))) of
        ?STORE_FREE_SLOT -> miss;
        Slot -> Slot
    end.
