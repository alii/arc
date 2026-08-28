%% fast paths return miss and the caller takes the full path
-module(arc_rt_obj_ffi).
-export([t_get_prop_own_data/3, t_set_prop_own_data/4, t_set_prop_named/5,
         t_create_data_prop/4, store_put_seq/3,
         t_get_prop_ic/4, t_get_prop_ic_miss/4, t_get_prop_slow/4,
         t_get_prop_site/4,
         t_instanceof_fast/3,
         t_get_elem_fast/3, t_set_elem_fast/4,
         t_global_get_fast/2, t_global_get/2,
         named_free/5, free_chain/4, named_plain/2,
         shape_slots_new/0, shape_slots_get/2, shape_slots_set/3,
         shape_slots_append/2,
         shape_slots_fold/3]).

-include("arc_rt_layout.hrl").

-compile({inline, [peek_get/3, slot_of/2, get_any/3,
                   named_plain/2, birth_plain/2, store_put_seq/3, index_read/2,
                   peek_slot/3, index_write/4, elem_write/3,
                   named_free_next/5, set_prop_new/7, free_chain/4,
                   in_store/2, in_agent/2, in_props/2, in_value/2, touched/2]}).

-define(IC_READ, ic_read).
-define(PROTO_KEY, {?KEY_NAMED, <<"__proto__">>}).
-define(IC_READ_WAYS, 8).

t_get_prop_own_data(St, {?HANDLE_TAG, Id}, KeyBin) ->
    peek_get(St, Id, KeyBin);
t_get_prop_own_data(_, _, _) -> miss.

t_get_prop_ic(St, {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    case element(?STORE_ICS, Store) of
        #{Site := {?IC_READ, KeyBin, Offs}} ->
            case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
                {?SSHAPED_TAG, Sid, _, Slots, _} ->
                    case Offs of
                        #{Sid := Off} -> element(Off + 1, Slots);
                        _ -> miss
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
t_get_prop_ic(_, _, _, _) -> miss.

t_get_prop_ic_miss(St, {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
        {?SSHAPED_TAG, Sid, _, Slots, Offs} ->
            case Offs of
                #{KeyBin := Off} ->
                    {element(Off + 1, Slots),
                     ic_fill(St, Store, Site, Sid, Off, KeyBin)};
                _ -> {miss, St}
            end;
        Slot -> {peek_slot(St, Slot, KeyBin), St}
    end;
t_get_prop_ic_miss(St, _, _, _) -> {miss, St}.

t_get_prop_slow(St, Recv = {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    read_named(St, Store, Data, Recv, arc_rt_arena_ffi:get(Id, Data), KeyBin, Site);
t_get_prop_slow(St, Recv, KeyBin, _) -> read_prim(St, Recv, KeyBin).

t_get_prop_site(St, Recv = {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    Slot = arc_rt_arena_ffi:get(Id, Data),
    case Slot of
        {?SSHAPED_TAG, Sid, _, Slots, _} ->
            case element(?STORE_ICS, Store) of
                #{Site := {?IC_READ, KeyBin, #{Sid := Off}}} ->
                    {element(Off + 1, Slots), St};
                _ -> read_named(St, Store, Data, Recv, Slot, KeyBin, Site)
            end;
        {?SOBJECT_TAG, ?ORDINARY, _, #{{?KEY_NAMED, KeyBin} := Prop}, _, _, _}
          when element(1, Prop) =:= ?DATAPROP_TAG ->
            {element(?DATAPROP_VALUE, Prop), St};
        _ -> read_named(St, Store, Data, Recv, Slot, KeyBin, Site)
    end;
t_get_prop_site(St, Recv, KeyBin, _) -> read_prim(St, Recv, KeyBin).

read_named(St, Store, Data, Recv, {?SSHAPED_TAG, Sid, Proto, Slots, Offs},
           KeyBin, Site) ->
    case Offs of
        #{KeyBin := Off} ->
            {element(Off + 1, Slots), ic_fill(St, Store, Site, Sid, Off, KeyBin)};
        _ ->
            read_proto(St, Data, element(?STORE_SHAPES, Store), Proto, Recv,
                       KeyBin)
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

%% §10.1.8.1 ordinary get while every hop is plain data
proto_read(_, _, ?NONE, _, _) -> undefined;
proto_read(_, _, _, _, 0) -> miss;
proto_read(Data, Shapes, {?SOME, {?HANDLE_TAG, Id}}, KeyBin, Fuel) ->
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, _, Proto, Slots, Offs} ->
            case Offs of
                #{KeyBin := Off} -> element(Off + 1, Slots);
                _ -> proto_read(Data, Shapes, Proto, KeyBin, Fuel - 1)
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

named_plain(?ORDINARY, _) -> true;
named_plain(Kind, _) when is_atom(Kind) -> true;
named_plain(Kind, KeyBin) ->
    case element(1, Kind) of
        ?PROXYOBJ_TAG -> false;
        module_namespace -> false;
        typed_array_obj -> false;
        ?ARRAYOBJ_TAG -> KeyBin =/= <<"length">>;
        string_obj -> KeyBin =/= <<"length">>;
        ?KBYTECODE_TAG -> birth_plain(element(?KBYTECODE_BIRTH, Kind), KeyBin);
        ?KFN_TAG -> birth_plain(element(?KFN_BIRTH, Kind), KeyBin);
        _ -> true
    end.

birth_plain(?BIRTH_SETTLED, _) -> true;
birth_plain(_, <<"length">>) -> false;
birth_plain(_, <<"name">>) -> false;
birth_plain(Birth, <<"prototype">>) ->
    element(?BIRTH_PROTOTYPE_PARENT, Birth) =:= ?NONE;
birth_plain(_, _) -> true.

ic_fill(St, _, none, _, _, _) -> St;
ic_fill(St, Store, Site, Sid, Off, KeyBin) ->
    Ics = element(?STORE_ICS, Store),
    case Ics of
        #{Site := {?IC_READ, KeyBin, Offs}}
          when map_size(Offs) < ?IC_READ_WAYS ->
            in_agent(St,
                       setelement(?STORE_ICS, Store,
                                  Ics#{Site := {?IC_READ, KeyBin,
                                                Offs#{Sid => Off}}}));
        #{Site := _} -> St;
        _ ->
            in_agent(St,
                       setelement(?STORE_ICS, Store,
                                  Ics#{Site => {?IC_READ, KeyBin,
                                                #{Sid => Off}}}))
    end.

t_global_get_fast(St, KeyBin) ->
    {?HANDLE_TAG, GId} = element(?REALM_GLOBAL, element(?AGENT_REALM, St)),
    Store = element(?AGENT_STORE, St),
    peek_slot(St, arc_rt_arena_ffi:get(GId, element(?STORE_DATA, Store)), KeyBin).

t_global_get(St, KeyBin) ->
    case t_global_get_fast(St, KeyBin) of
        miss -> arc@rt@obj:t_global_get(St, KeyBin);
        V -> {V, St}
    end.

peek_get(St, Id, KeyBin) ->
    peek_slot(St, slot_of(St, Id), KeyBin).

peek_slot(_, {?SSHAPED_TAG, _, _, Slots, Offs}, KeyBin) ->
    case Offs of
        #{KeyBin := Off} -> element(Off + 1, Slots);
        _ -> miss
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

%% §10.1.9.1 ordinary set when it lands as plain data
t_set_prop_own_data(St, {?HANDLE_TAG, Id}, KeyBin, V) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, _, _, _, _} = Shaped ->
            Shapes = element(?STORE_SHAPES, Store),
            case shaped_write(Data, Shapes, Shaped, KeyBin, V) of
                miss -> miss;
                NewSlot ->
                    in_agent(St,
                        in_store(Store,
                                   arc_rt_arena_ffi:set(Id, NewSlot, Data)))
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
                            NewProp = in_value(Prop, V),
                            NewSlot = in_props(Slot,
                                                 Props#{K := NewProp}),
                            in_agent(St,
                                touched(in_store(Store,
                                           arc_rt_arena_ffi:set(Id, NewSlot, Data)), Slot));
                        #{K := _} -> miss;
                        _ when element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
                            case free_chain(Store, Data,
                                            element(?SOBJECT_PROTO, Slot), K) of
                                false -> miss;
                                true -> set_prop_new(St, Store, Data, Id, Slot, K, V);
                                {true, Store1} ->
                                    set_prop_new(St, Store1, Data, Id, Slot, K, V)
                            end;
                        _ -> miss
                    end
            end;
        _ -> miss
    end;
t_set_prop_own_data(_, _, _, _) -> miss.

set_prop_new(St, Store, Data, Id, Slot, K, V)
  when tuple_size(Store) =:= ?STORE_ARITY ->
    Seq = element(?STORE_PROP_SEQ, Store),
    Props = element(?SOBJECT_PROPS, Slot),
    NewSlot = in_props(Slot,
                         Props#{K => {?DATAPROP_TAG, V, true, true, true, Seq}}),
    in_agent(St,
             touched(store_put_seq(Store, arc_rt_arena_ffi:set(Id, NewSlot, Data),
                                   Seq + 1), Slot)).

%% proto chain takes a plain named write at k, filling the store memo
free_chain(_, _, ?NONE, _) -> true;
free_chain(Store, Data, {?SOME, {?HANDLE_TAG, PId}} = Proto,
           {?KEY_NAMED, KB} = K)
  when byte_size(KB) =/= 9; KB =/= <<"__proto__">> ->
    case is_map_key(PId, element(?STORE_FREE_PROTOS, Store)) of
        true -> true;
        false -> named_free_cached(Store, Data, Proto, K)
    end;
free_chain(Store, Data, Proto, K) ->
    named_free(Data, element(?STORE_SHAPES, Store), Proto, K, 64).

store_put_seq(Store, Data, Seq) when tuple_size(Store) =:= ?STORE_ARITY ->
    setelement(?STORE_PROP_SEQ, setelement(?STORE_DATA, Store, Data), Seq).

%% arity guards make these record updates instead of bif calls
in_store(Store, Data) when tuple_size(Store) =:= ?STORE_ARITY ->
    setelement(?STORE_DATA, Store, Data).

%% global object readers watch the epoch
touched(Store, Slot) when tuple_size(Store) =:= ?STORE_ARITY,
                          element(?SOBJECT_KIND, Slot) =:= ?GLOBALOBJ ->
    setelement(?STORE_GLOBAL_EPOCH, Store, element(?STORE_GLOBAL_EPOCH, Store) + 1);
touched(Store, _) -> Store.

in_agent(St, Store) when tuple_size(St) =:= ?AGENT_ARITY ->
    setelement(?AGENT_STORE, St, Store).

in_props(Slot, Props) when tuple_size(Slot) =:= ?SOBJECT_ARITY ->
    setelement(?SOBJECT_PROPS, Slot, Props).

in_value(Prop, V) when tuple_size(Prop) =:= ?DATAPROP_ARITY ->
    setelement(?DATAPROP_VALUE, Prop, V).

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

%% §7.3.5 create data property, new plain key only
t_create_data_prop(St, Recv = {?HANDLE_TAG, Id}, Key, V) ->
    PK = case Key of
        {?OKEY_STRING, K} -> K;
        K -> K
    end,
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    Slot = arc_rt_arena_ffi:get(Id, Data),
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
            Store1 = store_put_seq(Store, arc_rt_arena_ffi:set(Id, NewSlot, Data), Seq),
            {true, in_agent(St, touched(Store1, NewSlot))};
        NewSlot ->
            Store1 = in_store(Store, arc_rt_arena_ffi:set(Id, NewSlot, Data)),
            {true, in_agent(St, touched(Store1, NewSlot))}
    end;
t_create_data_prop(St, Recv, Key, V) ->
    'arc@rt@obj':t_create_data_prop_slow(St, Recv, Key, V).

plain_define(Slot, PK, V, Store) ->
    Seq = element(?STORE_PROP_SEQ, Store),
    Props = element(?SOBJECT_PROPS, Slot),
    case is_map_key(PK, Props) orelse
         element(?SOBJECT_EXTENSIBLE, Slot) =/= true of
        true -> miss;
        false ->
            {seq, in_props(Slot,
                             Props#{PK => {?DATAPROP_TAG, V, true, true, true,
                                           Seq}}),
             Seq + 1}
    end.

shaped_define(Shapes, {?SSHAPED_TAG, Sid, P, Slots, Offs} = Shaped, KeyBin, V) ->
    case Offs of
        #{KeyBin := Off} ->
            setelement(?SSHAPED_SLOTS, Shaped, setelement(Off + 1, Slots, V));
        _ ->
            case shaped_next(Shapes, Sid, KeyBin) of
                miss -> miss;
                {To, ToOffs} ->
                    {?SSHAPED_TAG, To, P, erlang:append_element(Slots, V), ToOffs}
            end
    end.

%% known successor shape for adding keybin
shaped_next(Shapes, Sid, KeyBin) ->
    case Shapes of
        #{Sid := Desc} ->
            case element(?SHAPE_TRANSITIONS, Desc) of
                #{KeyBin := To} ->
                    #{To := ToDesc} = Shapes,
                    {To, element(?SHAPE_OFFSETS, ToDesc)};
                _ -> miss
            end;
        _ -> miss
    end.

%% §7.3.22 ordinary has instance, depth capped at 64
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

proto_has(St, {?HANDLE_TAG, VId}, PId, Fuel) when Fuel > 0 ->
    case slot_of(St, VId) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG,
                  element(1, element(?SOBJECT_KIND, Slot)) =:= ?PROXYOBJ_TAG ->
            miss;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG;
                  element(1, Slot) =:= ?SSHAPED_TAG ->
            %% proto is element 3 in both slot kinds
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

%% 2^32-2, rt_types.max_array_index
-define(MAX_ARRAY_INDEX, 4294967294).

t_get_elem_fast(St, {?HANDLE_TAG, Id}, Idx)
  when is_integer(Idx), Idx >= 0, Idx =< ?MAX_ARRAY_INDEX ->
    Store = element(?AGENT_STORE, St),
    index_read(arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)), Idx);
t_get_elem_fast(St, Recv, Idx)
  when is_float(Idx), Idx >= 0.0, Idx == trunc(Idx) ->
    t_get_elem_fast(St, Recv, trunc(Idx));
t_get_elem_fast(St, {?HANDLE_TAG, Id}, Key) when is_binary(Key) ->
    case arc_rt_val_ffi:t_to_property_key_fast(Key) of
        {?OKEY_STRING, {?KEY_NAMED, KeyBin}} ->
            Store = element(?AGENT_STORE, St),
            Data = element(?STORE_DATA, Store),
            case named_read(Data, element(?STORE_SHAPES, Store),
                            arc_rt_arena_ffi:get(Id, Data), KeyBin) of
                undefined -> miss;
                V -> V
            end;
        {?OKEY_STRING, {?KEY_INDEX, Idx}} ->
            Store = element(?AGENT_STORE, St),
            index_read(arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)), Idx);
        _ -> miss
    end;
t_get_elem_fast(_, _, _) -> miss.

index_read(Slot, Idx) when element(1, Slot) =:= ?SOBJECT_TAG ->
    case element(?SOBJECT_KIND, Slot) of
        {?ARRAYOBJ_TAG, Length} when Idx < Length ->
            case element(?SOBJECT_PROPS, Slot) of
                #{{?KEY_INDEX, Idx} := _} -> miss;
                _ ->
                    case element(?SOBJECT_ELEMENTS, Slot) of
                        {?ELEMS_DENSE, A} ->
                            case arc_tree_array_ffi:get(Idx, A) of
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

named_read(Data, Shapes, {?SSHAPED_TAG, _, Proto, Slots, Offs}, KeyBin) ->
    case Offs of
        #{KeyBin := Off} -> element(Off + 1, Slots);
        _ -> proto_read(Data, Shapes, Proto, KeyBin, 64)
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
    case arc_rt_arena_ffi:get(Id, Data) of
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
                                    in_agent(St,
                                        in_store(Store,
                                            arc_rt_arena_ffi:set(Id, NewSlot, Data)))
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
                                    in_agent(St,
                                        in_store(Store,
                                            arc_rt_arena_ffi:set(Id, NewSlot, Data)))
                            end
                    end;
                Kind ->
                    case index_in_props(Kind) of
                        true ->
                            index_prop_write(St, Store, Data, Id, Slot, Idx, V);
                        false -> miss
                    end
            end;
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

index_prop_write(St, Store, Data, Id, Slot, Idx, V) ->
    Props = element(?SOBJECT_PROPS, Slot),
    K = {?KEY_INDEX, Idx},
    case Props of
        #{K := Prop}
          when element(1, Prop) =:= ?DATAPROP_TAG,
               element(?DATAPROP_WRITABLE, Prop) =:= true ->
            NewProp = in_value(Prop, V),
            NewSlot = in_props(Slot, Props#{K := NewProp}),
            in_agent(St,
                     touched(in_store(Store, arc_rt_arena_ffi:set(Id, NewSlot, Data)),
                             Slot));
        #{K := _} -> miss;
        _ when element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
            case index_free(Data, element(?SOBJECT_PROTO, Slot), Idx, 64) of
                false -> miss;
                true ->
                    Seq = element(?STORE_PROP_SEQ, Store),
                    Prop = {?DATAPROP_TAG, V, true, true, true, Seq},
                    NewSlot =
                        in_props(Slot, Props#{K => Prop}),
                    in_agent(St,
                             touched(store_put_seq(Store,
                                                   arc_rt_arena_ffi:set(Id, NewSlot, Data),
                                                   Seq + 1), Slot))
            end;
        _ -> miss
    end.

index_free(_, ?NONE, _, _) -> true;
index_free(_, _, _, 0) -> false;
index_free(Data, {?SOME, {?HANDLE_TAG, PId}}, Idx, Fuel) ->
    case arc_rt_arena_ffi:get(PId, Data) of
        {?SSHAPED_TAG, _, P2, _, _} -> index_free(Data, P2, Idx, Fuel - 1);
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
    case Idx < arc_tree_array_ffi:size(A) of
        true -> {?ELEMS_DENSE, arc_tree_array_ffi:set(Idx, V, A)};
        false -> miss
    end;
elem_write({?ELEMS_SPARSE, M}, Idx, V) ->
    {?ELEMS_SPARSE, M#{Idx => V}};
elem_write(_, _, _) -> miss.

elem_write_grow({?ELEMS_DENSE, A}, Idx, V) ->
    {?ELEMS_DENSE, arc_tree_array_ffi:set(Idx, V, A)};
elem_write_grow({?ELEMS_SPARSE, M}, Idx, V) ->
    {?ELEMS_SPARSE, M#{Idx => V}};
elem_write_grow(_, _, _) -> miss.

shape_slots_get(Slots, Off) -> element(Off + 1, Slots).

shape_slots_new() -> {}.

shape_slots_set(Slots, Off, V) -> setelement(Off + 1, Slots, V).

shape_slots_append(Slots, V) -> erlang:append_element(Slots, V).

shape_slots_fold(Slots, Acc, F) ->
    shape_slots_fold_1(Slots, Acc, F, 1, tuple_size(Slots)).
shape_slots_fold_1(_, Acc, _, I, N) when I > N -> Acc;
shape_slots_fold_1(Slots, Acc, F, I, N) ->
    shape_slots_fold_1(Slots, F(I - 1, element(I, Slots), Acc), F, I + 1, N).

shaped_write(Data, Shapes, {?SSHAPED_TAG, Sid, P, Slots, Offs} = Shaped, KeyBin,
             V) ->
    case Offs of
        #{KeyBin := Off} ->
            setelement(?SSHAPED_SLOTS, Shaped, setelement(Off + 1, Slots, V));
        _ ->
            case shaped_next(Shapes, Sid, KeyBin) of
                miss -> miss;
                {To, ToOffs} ->
                    case named_free(Data, Shapes, P, {?KEY_NAMED, KeyBin}, 64) of
                        true ->
                            {?SSHAPED_TAG, To, P, erlang:append_element(Slots, V),
                             ToOffs};
                        false -> miss
                    end
            end
    end.

%% true | false | {true, Store1} once a clean chain is remembered
named_free_cached(Store, Data, Proto, K) ->
    Shapes = element(?STORE_SHAPES, Store),
    case clean_chain(Data, Shapes, Proto, 64, []) of
        Ids when is_list(Ids) ->
            Free = lists:foldl(fun(Id, M) -> M#{Id => nil} end,
                               element(?STORE_FREE_PROTOS, Store), Ids),
            {true, setelement(?STORE_FREE_PROTOS, Store, Free)};
        false -> named_free(Data, Shapes, Proto, K, 64)
    end.

%% every hop ordinary or shaped with only writable named data, bar __proto__
clean_chain(_, _, ?NONE, _, Ids) -> Ids;
clean_chain(Data, Shapes, {?SOME, {?HANDLE_TAG, PId}}, Fuel, Ids) when Fuel > 0 ->
    case arc_rt_arena_ffi:get(PId, Data) of
        {?SSHAPED_TAG, _, P2, _, _} ->
            clean_chain(Data, Shapes, P2, Fuel - 1, [PId | Ids]);
        {?SOBJECT_TAG, ?ORDINARY, P2, Props, _, _, _} ->
            clean_props(maps:next(maps:iterator(Props)))
                andalso clean_chain(Data, Shapes, P2, Fuel - 1, [PId | Ids]);
        _ -> false
    end;
clean_chain(_, _, _, _, _) -> false.

clean_props(none) -> true;
clean_props({{?KEY_NAMED, _} = K, Prop, I}) ->
    case Prop of
        {?DATAPROP_TAG, _, true, _, _, _} -> clean_props(maps:next(I));
        _ when K =:= ?PROTO_KEY -> clean_props(maps:next(I));
        _ -> false
    end;
clean_props({_, _, I}) -> clean_props(maps:next(I)).

%% true when the proto chain cannot intercept a write at k
named_free(_, _, ?NONE, _, _) -> true;
named_free(_, _, _, _, 0) -> false;
named_free(Data, Shapes, {?SOME, {?HANDLE_TAG, PId}}, K, Fuel) ->
    case arc_rt_arena_ffi:get(PId, Data) of
        {?SSHAPED_TAG, _, P2, _, Offs} ->
            case Offs of
                #{element(2, K) := _} -> true;
                _ -> named_free_next(Data, Shapes, P2, K, Fuel)
            end;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Slot), element(2, K)) of
                true ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{K := Prop} ->
                            element(1, Prop) =:= ?DATAPROP_TAG andalso
                                element(?DATAPROP_WRITABLE, Prop) =:= true;
                        _ ->
                            named_free_next(Data, Shapes,
                                            element(?SOBJECT_PROTO, Slot), K, Fuel)
                    end;
                false -> false
            end;
        _ -> false
    end;
named_free(_, _, _, _, _) -> false.

named_free_next(_, _, ?NONE, _, _) -> true;
named_free_next(Data, Shapes, P, K, Fuel) ->
    named_free(Data, Shapes, P, K, Fuel - 1).

slot_of(St, Id) ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, St))) of
        ?STORE_FREE_SLOT -> miss;
        Slot -> Slot
    end.
