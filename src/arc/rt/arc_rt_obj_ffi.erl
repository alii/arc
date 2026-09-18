%% object model kernels; exports may answer miss for the general path
-module(arc_rt_obj_ffi).
-export([t_get_prop_own_data/3, t_set_prop_own_data/4, t_set_named/5,
         plain_copy_data_props/3, plain_for_in_keys/2, plain_own_enum_pairs/2,
         t_create_data_prop/4,
         t_get_named_ic_fill/4, t_get_named/4,
         t_get_named_site/4,
         t_instanceof_i32/3, t_instanceof_i32_general/3,
         t_get_elem/3, t_set_elem/4, t_array_lit/2,
         t_array_lit_packed/2,
         t_global_peek/2, t_global_get/2,
         named_write_walk/5, chain_takes_named_write/4, named_plain/2,
         shape_slots_new/0, shape_slots_get/2, shape_slots_set/3,
         shape_slots_append/2, get_symbol_data/3, native_token/1, elem_at/2,
         elem_write_grow/3, shaped_next/3]).

-include("arc_rt_layout.hrl").

-compile({inline, [peek_named_at/3, peek_named/3, live_cell/2, general_get/3,
                   named_plain/2, birth_plain/2, store_put_seq/3, index_read/2,
                   index_write/4, elem_write/3, named_write_walk_next/5,
                   set_prop_new/7, chain_takes_named_write/4, with_store/2,
                   with_data/2, with_props/2, with_value/2,
                   bump_epoch_if_global/2]}).

-define(PROTO_KEY, {?KEY_NAMED, <<"__proto__">>}).
-define(IC_READ_WAYS, 8).

t_get_prop_own_data(St, {?HANDLE_TAG, Id}, KeyBin) ->
    peek_named_at(St, Id, KeyBin);
t_get_prop_own_data(_, _, _) -> miss.

%% nested constant array literals arrive as {js_alit, Elems}
t_array_lit(St, Elems) ->
    {Vs, St1} = array_lit_elems(Elems, St, []),
    'arc@rt@obj':t_new_array(St1, Vs).

array_lit_elems([{js_alit, Sub} | T], St, Acc) ->
    {V, St1} = t_array_lit(St, Sub),
    array_lit_elems(T, St1, [V | Acc]);
array_lit_elems([V | T], St, Acc) -> array_lit_elems(T, St, [V | Acc]);
array_lit_elems([], St, Acc) -> {lists:reverse(Acc), St}.

t_array_lit_packed(St, Bin) -> t_array_lit(St, binary_to_term(Bin)).


t_get_named_ic_fill(St, {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
        {?SSHAPED_TAG, Sid, _, Slots, Offs} ->
            case Offs of
                #{KeyBin := Off} ->
                    {?SLOT_AT(Slots, Off),
                     ic_fill(St, Store, Site, Sid, Off, KeyBin)};
                _ -> {miss, St}
            end;
        Cell -> {peek_named(St, Cell, KeyBin), St}
    end;
t_get_named_ic_fill(St, _, _, _) -> {miss, St}.

t_get_named(St, Recv = {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    read_named(St, Store, Data, Recv, arc_rt_arena_ffi:get(Id, Data), KeyBin, Site);
t_get_named(St, Recv, KeyBin, _) -> read_prim(St, Recv, KeyBin).

t_get_named_site(St, Recv = {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    Cell = arc_rt_arena_ffi:get(Id, Data),
    case Cell of
        {?SSHAPED_TAG, Sid, _, Slots, _} ->
            case element(?STORE_ICS, Store) of
                #{Site := {?IC_READ, KeyBin, #{Sid := Off}}} ->
                    {?SLOT_AT(Slots, Off), St};
                _ -> read_named(St, Store, Data, Recv, Cell, KeyBin, Site)
            end;
        {?SOBJECT_TAG, ?ORDINARY, _, #{{?KEY_NAMED, KeyBin} := Prop}, _, _, _}
          when element(1, Prop) =:= ?DATAPROP_TAG ->
            {element(?DATAPROP_VALUE, Prop), St};
        _ -> read_named(St, Store, Data, Recv, Cell, KeyBin, Site)
    end;
t_get_named_site(St, Recv, KeyBin, _) -> read_prim(St, Recv, KeyBin).

read_named(St, Store, Data, Recv, {?SSHAPED_TAG, Sid, Proto, Slots, Offs},
           KeyBin, Site) ->
    case Offs of
        #{KeyBin := Off} ->
            {?SLOT_AT(Slots, Off), ic_fill(St, Store, Site, Sid, Off, KeyBin)};
        _ ->
            read_proto(St, Data, element(?STORE_SHAPES, Store), Proto, Recv,
                       KeyBin)
    end;
read_named(St, Store, Data, Recv, Cell, KeyBin, _)
  when element(1, Cell) =:= ?SOBJECT_TAG ->
    Kind = element(?SOBJECT_KIND, Cell),
    case named_plain(Kind, KeyBin) of
        true ->
            case element(?SOBJECT_PROPS, Cell) of
                #{{?KEY_NAMED, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                    {element(?DATAPROP_VALUE, Prop), St};
                #{{?KEY_NAMED, KeyBin} := _} -> general_get(St, Recv, KeyBin);
                _ ->
                    read_proto(St, Data, element(?STORE_SHAPES, Store),
                               element(?SOBJECT_PROTO, Cell), Recv, KeyBin)
            end;
        false when element(1, Kind) =:= ?ARRAYOBJ_TAG ->
            {element(?ARRAYOBJ_LENGTH, Kind), St};
        false -> general_get(St, Recv, KeyBin)
    end;
read_named(St, _, _, Recv, _, KeyBin, _) -> general_get(St, Recv, KeyBin).

read_proto(St, Data, Shapes, Proto, Recv, KeyBin) ->
    case proto_read(Data, Shapes, Proto, KeyBin, ?MAX_PROTO_HOPS) of
        miss -> general_get(St, Recv, KeyBin);
        V -> {V, St}
    end.

read_prim(St, S, <<"length">>) when ?IS_STR(S) ->
    {arc_rt_js_string_ffi:len(S), St};
read_prim(St, S, KeyBin) when ?IS_STR(S) ->
    read_wrapper(St, ?REALM_STRING, S, KeyBin);
read_prim(St, N, KeyBin) when is_number(N) ->
    read_wrapper(St, ?REALM_NUMBER, N, KeyBin);
read_prim(St, Recv, KeyBin) -> general_get(St, Recv, KeyBin).

read_wrapper(St, Which, Recv, KeyBin) ->
    Pair = element(Which, element(?AGENT_REALM, St)),
    Store = element(?AGENT_STORE, St),
    read_proto(St, element(?STORE_DATA, Store), element(?STORE_SHAPES, Store),
               {?SOME, element(?PAIR_PROTO, Pair)}, Recv, KeyBin).

general_get(St, Recv, KeyBin) ->
    'arc@rt@obj':t_get_prop_untyped_key(St, Recv, {?OKEY_STRING, {?KEY_NAMED, KeyBin}}).

%% §10.1.8.1 ordinary get while every hop is plain data
proto_read(_, _, ?NONE, _, _) -> undefined;
proto_read(_, _, _, _, 0) -> miss;
proto_read(Data, Shapes, {?SOME, {?HANDLE_TAG, Id}}, KeyBin, Fuel) ->
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, _, Proto, Slots, Offs} ->
            case Offs of
                #{KeyBin := Off} -> ?SLOT_AT(Slots, Off);
                _ -> proto_read(Data, Shapes, Proto, KeyBin, Fuel - 1)
            end;
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Cell), KeyBin) of
                false -> miss;
                true ->
                    case element(?SOBJECT_PROPS, Cell) of
                        #{{?KEY_NAMED, KeyBin} := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG ->
                            element(?DATAPROP_VALUE, Prop);
                        #{{?KEY_NAMED, KeyBin} := _} -> miss;
                        _ ->
                            proto_read(Data, Shapes,
                                       element(?SOBJECT_PROTO, Cell), KeyBin,
                                       Fuel - 1)
                    end
            end;
        _ -> miss
    end;
proto_read(_, _, _, _, _) -> miss.

named_plain(Kind, KeyBin) -> ?NAMED_KEY_IS_PLAIN(Kind, KeyBin, <<"length">>).

birth_plain(Birth, KeyBin) ->
    ?LAZY_KEY_IS_PLAIN(Birth, KeyBin, <<"length">>, <<"name">>, <<"prototype">>).

ic_fill(St, _, none, _, _, _) -> St;
ic_fill(St, Store, Site, Sid, Off, KeyBin) ->
    Ics = element(?STORE_ICS, Store),
    case Ics of
        #{Site := {?IC_READ, KeyBin, Offs}}
          when map_size(Offs) < ?IC_READ_WAYS ->
            with_store(St,
                       setelement(?STORE_ICS, Store,
                                  Ics#{Site := {?IC_READ, KeyBin,
                                                Offs#{Sid => Off}}}));
        #{Site := _} -> St;
        _ ->
            with_store(St,
                       setelement(?STORE_ICS, Store,
                                  Ics#{Site => {?IC_READ, KeyBin,
                                                #{Sid => Off}}}))
    end.

t_global_peek(St, KeyBin) ->
    {?HANDLE_TAG, GId} = element(?REALM_GLOBAL, element(?AGENT_REALM, St)),
    Store = element(?AGENT_STORE, St),
    peek_named(St, arc_rt_arena_ffi:get(GId, element(?STORE_DATA, Store)), KeyBin).

t_global_get(St, KeyBin) ->
    case t_global_peek(St, KeyBin) of
        miss -> 'arc@rt@obj':t_global_get(St, KeyBin);
        V -> {V, St}
    end.

peek_named_at(St, Id, KeyBin) ->
    peek_named(St, live_cell(St, Id), KeyBin).

peek_named(_, {?SSHAPED_TAG, _, _, Slots, Offs}, KeyBin) ->
    case Offs of
        #{KeyBin := Off} -> ?SLOT_AT(Slots, Off);
        _ -> miss
    end;
peek_named(_, Cell, KeyBin) when element(1, Cell) =:= ?SOBJECT_TAG ->
    case named_plain(element(?SOBJECT_KIND, Cell), KeyBin) of
        true ->
            case element(?SOBJECT_PROPS, Cell) of
                #{{?KEY_NAMED, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                    element(?DATAPROP_VALUE, Prop);
                _ -> miss
            end;
        false -> miss
    end;
peek_named(_, _, _) -> miss.

%% §10.1.9.1 ordinary set when it lands as plain data
t_set_prop_own_data(St, {?HANDLE_TAG, Id}, KeyBin, V) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, _, _, _, _} = Shaped ->
            Shapes = element(?STORE_SHAPES, Store),
            case shaped_write(Data, Shapes, Shaped, KeyBin, V) of
                miss -> miss;
                NewCell ->
                    with_store(St,
                        with_data(Store,
                                  arc_rt_arena_ffi:set(Id, NewCell, Data)))
            end;
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Cell), KeyBin) of
                false -> miss;
                true ->
                    Props = element(?SOBJECT_PROPS, Cell),
                    K = {?KEY_NAMED, KeyBin},
                    case Props of
                        #{K := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG,
                               element(?DATAPROP_WRITABLE, Prop) =:= true ->
                            NewProp = with_value(Prop, V),
                            NewCell = with_props(Cell, Props#{K := NewProp}),
                            Store1 = with_data(Store,
                                               arc_rt_arena_ffi:set(Id, NewCell, Data)),
                            with_store(St, bump_epoch_if_global(Store1, Cell));
                        #{K := _} -> miss;
                        _ when element(?SOBJECT_EXTENSIBLE, Cell) =:= true ->
                            case chain_takes_named_write(
                                   Store, Data, element(?SOBJECT_PROTO, Cell), K) of
                                false -> miss;
                                true -> set_prop_new(St, Store, Data, Id, Cell, K, V);
                                {true, Store1} ->
                                    set_prop_new(St, Store1, Data, Id, Cell, K, V)
                            end;
                        _ -> miss
                    end
            end;
        _ -> miss
    end;
t_set_prop_own_data(_, _, _, _) -> miss.

set_prop_new(St, Store, Data, Id, Cell, K, V)
  when tuple_size(Store) =:= ?STORE_SIZE ->
    Seq = element(?STORE_PROP_SEQ, Store),
    Props = element(?SOBJECT_PROPS, Cell),
    NewCell = with_props(Cell, Props#{K => ?PLAIN_PROPERTY(V, Seq)}),
    Store1 = store_put_seq(Store, arc_rt_arena_ffi:set(Id, NewCell, Data), Seq + 1),
    with_store(St, bump_epoch_if_global(Store1, Cell)).

%% true when no proto can intercept a plain write at k, memo in free_protos
chain_takes_named_write(_, _, ?NONE, _) -> true;
chain_takes_named_write(Store, Data, {?SOME, {?HANDLE_TAG, PId}} = Proto,
                        {?KEY_NAMED, KB} = K)
  when byte_size(KB) =/= 9; KB =/= <<"__proto__">> ->
    case is_map_key(PId, element(?STORE_FREE_PROTOS, Store)) of
        true -> true;
        false -> remember_plain_chain(Store, Data, Proto, K)
    end;
chain_takes_named_write(Store, Data, Proto, K) ->
    named_write_walk(Data, element(?STORE_SHAPES, Store), Proto, K, ?MAX_PROTO_HOPS).

store_put_seq(Store, Data, Seq) when tuple_size(Store) =:= ?STORE_SIZE ->
    setelement(?STORE_PROP_SEQ, setelement(?STORE_DATA, Store, Data), Seq).

%% size guards make these record updates instead of bif calls
with_data(Store, Data) when tuple_size(Store) =:= ?STORE_SIZE ->
    setelement(?STORE_DATA, Store, Data).

%% global object readers watch the epoch
bump_epoch_if_global(Store, Cell) when tuple_size(Store) =:= ?STORE_SIZE,
                                       element(?SOBJECT_KIND, Cell) =:= ?GLOBALOBJ ->
    setelement(?STORE_GLOBAL_EPOCH, Store, element(?STORE_GLOBAL_EPOCH, Store) + 1);
bump_epoch_if_global(Store, _) -> Store.

with_store(St, Store) when tuple_size(St) =:= ?AGENT_SIZE ->
    setelement(?AGENT_STORE, St, Store).

with_props(Cell, Props) when tuple_size(Cell) =:= ?SOBJECT_SIZE ->
    setelement(?SOBJECT_PROPS, Cell, Props).

with_value(Prop, V) when tuple_size(Prop) =:= ?DATAPROP_SIZE ->
    setelement(?DATAPROP_VALUE, Prop, V).

t_set_named(St, Obj, KeyBin, V, Strict) ->
    case t_set_prop_own_data(St, Obj, KeyBin, V) of
        miss ->
            Key = {?KEY_NAMED, KeyBin},
            {_, St1} = case Strict of
                true -> 'arc@rt@obj':t_set_prop_strict(St, Obj, Key, V);
                false -> 'arc@rt@obj':t_set_prop_untyped_key(St, Obj, Key, V)
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
    Cell = arc_rt_arena_ffi:get(Id, Data),
    R = case PK of
        {?KEY_NAMED, KeyBin} when element(1, Cell) =:= ?SSHAPED_TAG ->
            shaped_define(element(?STORE_SHAPES, Store), Cell, KeyBin, V);
        {?KEY_NAMED, KeyBin} when element(1, Cell) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Cell), KeyBin) of
                true -> plain_define(Cell, PK, V, Store);
                false -> miss
            end;
        {?KEY_INDEX, _} when element(1, Cell) =:= ?SOBJECT_TAG ->
            case index_keys_in_props(element(?SOBJECT_KIND, Cell)) of
                true -> plain_define(Cell, PK, V, Store);
                false -> miss
            end;
        _ -> miss
    end,
    case R of
        miss -> 'arc@rt@obj':t_create_data_prop_general(St, Recv, Key, V);
        {seq, NewCell, Seq} ->
            Store1 = store_put_seq(Store, arc_rt_arena_ffi:set(Id, NewCell, Data), Seq),
            {true, with_store(St, bump_epoch_if_global(Store1, NewCell))};
        NewCell ->
            Store1 = with_data(Store, arc_rt_arena_ffi:set(Id, NewCell, Data)),
            {true, with_store(St, bump_epoch_if_global(Store1, NewCell))}
    end;
t_create_data_prop(St, Recv, Key, V) ->
    'arc@rt@obj':t_create_data_prop_general(St, Recv, Key, V).

plain_define(Cell, PK, V, Store) ->
    Seq = element(?STORE_PROP_SEQ, Store),
    Props = element(?SOBJECT_PROPS, Cell),
    case is_map_key(PK, Props) orelse
         element(?SOBJECT_EXTENSIBLE, Cell) =/= true of
        true -> miss;
        false ->
            {seq, with_props(Cell,
                             Props#{PK => ?PLAIN_PROPERTY(V, Seq)}),
             Seq + 1}
    end.

shaped_define(Shapes, {?SSHAPED_TAG, Sid, P, Slots, Offs} = Shaped, KeyBin, V) ->
    case Offs of
        #{KeyBin := Off} ->
            setelement(?SSHAPED_SLOTS, Shaped, ?SLOT_SET(Slots, Off, V));
        _ ->
            case shaped_next(Shapes, Sid, KeyBin) of
                miss -> miss;
                {To, ToOffs} ->
                    {?SSHAPED_TAG, To, P, erlang:append_element(Slots, V), ToOffs}
            end
    end.

shaped_next(Shapes, Sid, KeyBin) -> ?SHAPED_NEXT(Shapes, Sid, KeyBin).

%% §7.3.22 ordinary has instance
t_instanceof_i32(St, V, {?HANDLE_TAG, CId}) ->
    case live_cell(St, CId) of
        Cell when element(1, Cell) =:= ?SOBJECT_TAG,
                  element(?SOBJECT_SYMBOL_PROPS, Cell) =:= [] ->
            case element(?SOBJECT_KIND, Cell) of
                Kind when element(1, Kind) =:= ?COMPILEDFN_TAG ->
                    case element(?SOBJECT_PROPS, Cell) of
                        #{{?KEY_NAMED, <<"prototype">>} := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG ->
                            case element(?DATAPROP_VALUE, Prop) of
                                {?HANDLE_TAG, PId} -> proto_has(St, V, PId, ?MAX_PROTO_HOPS);
                                _ -> miss
                            end;
                        _ -> miss
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
t_instanceof_i32(_, _, _) -> miss.

t_instanceof_i32_general(St, V, Ctor) ->
    case 'arc@rt@ops':t_instance_of(St, V, Ctor) of
        {true, St1} -> {1, St1};
        {false, St1} -> {0, St1}
    end.

%% 1 | 0 | miss
proto_has(St, {?HANDLE_TAG, VId}, PId, Fuel) when Fuel > 0 ->
    case live_cell(St, VId) of
        Cell when element(1, Cell) =:= ?SOBJECT_TAG,
                  element(1, element(?SOBJECT_KIND, Cell)) =:= ?PROXYOBJ_TAG ->
            miss;
        Cell when element(1, Cell) =:= ?SOBJECT_TAG;
                  element(1, Cell) =:= ?SSHAPED_TAG ->
            case element(?CELL_PROTO, Cell) of
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

t_get_elem(St, {?HANDLE_TAG, Id}, Idx)
  when is_integer(Idx), Idx >= 0, Idx =< ?MAX_ARRAY_INDEX ->
    Store = element(?AGENT_STORE, St),
    index_read(arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)), Idx);
t_get_elem(St, Recv, Idx)
  when is_float(Idx), Idx >= 0.0, Idx == trunc(Idx) ->
    t_get_elem(St, Recv, trunc(Idx));
t_get_elem(St, {?HANDLE_TAG, Id}, Key) when ?IS_STR(Key) ->
    case arc_rt_val_ffi:property_key_of(Key) of
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
t_get_elem(_, _, _) -> miss.

index_read(Cell, Idx) when element(1, Cell) =:= ?SOBJECT_TAG ->
    case element(?SOBJECT_KIND, Cell) of
        {?ARRAYOBJ_TAG, Length} when Idx < Length ->
            case element(?SOBJECT_PROPS, Cell) of
                #{{?KEY_INDEX, Idx} := _} -> miss;
                _ ->
                    case element(?SOBJECT_ELEMENTS, Cell) of
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
            case index_keys_in_props(Kind) of
                true ->
                    case element(?SOBJECT_PROPS, Cell) of
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
        #{KeyBin := Off} -> ?SLOT_AT(Slots, Off);
        _ -> proto_read(Data, Shapes, Proto, KeyBin, ?MAX_PROTO_HOPS)
    end;
named_read(Data, Shapes, Cell, KeyBin) when element(1, Cell) =:= ?SOBJECT_TAG ->
    Kind = element(?SOBJECT_KIND, Cell),
    case named_plain(Kind, KeyBin) of
        true ->
            case element(?SOBJECT_PROPS, Cell) of
                #{{?KEY_NAMED, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                    element(?DATAPROP_VALUE, Prop);
                #{{?KEY_NAMED, KeyBin} := _} -> miss;
                _ ->
                    proto_read(Data, Shapes, element(?SOBJECT_PROTO, Cell),
                               KeyBin, ?MAX_PROTO_HOPS)
            end;
        false when element(1, Kind) =:= ?ARRAYOBJ_TAG ->
            element(?ARRAYOBJ_LENGTH, Kind);
        false -> miss
    end;
named_read(_, _, _, _) -> miss.

index_keys_in_props(Kind) when is_atom(Kind) -> true;
index_keys_in_props(Kind) ->
    case element(1, Kind) of
        ?ARRAYOBJ_TAG -> false;
        ?ARGUMENTSOBJ_TAG -> false;
        ?STRINGOBJ_TAG -> false;
        ?TYPEDARRAYOBJ_TAG -> false;
        ?PROXYOBJ_TAG -> false;
        ?MODULENS_TAG -> false;
        _ -> true
    end.

t_set_elem(St, {?HANDLE_TAG, Id}, Idx, V)
  when is_integer(Idx), Idx >= 0, Idx =< ?MAX_ARRAY_INDEX ->
    index_write(St, Id, Idx, V);
t_set_elem(St, Recv, Idx, V)
  when is_float(Idx), Idx >= 0.0, Idx == trunc(Idx) ->
    t_set_elem(St, Recv, trunc(Idx), V);
t_set_elem(St, Recv = {?HANDLE_TAG, Id}, Key, V) when ?IS_STR(Key) ->
    case arc_rt_val_ffi:property_key_of(Key) of
        {?OKEY_STRING, {?KEY_NAMED, KeyBin}} ->
            t_set_prop_own_data(St, Recv, KeyBin, V);
        {?OKEY_STRING, {?KEY_INDEX, Idx}} -> index_write(St, Id, Idx, V);
        _ -> miss
    end;
t_set_elem(_, _, _, _) -> miss.

index_write(St, Id, Idx, V) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        Cell when tuple_size(Cell) =:= ?SOBJECT_SIZE,
                  element(1, Cell) =:= ?SOBJECT_TAG,
                  element(?SOBJECT_EXTENSIBLE, Cell) =:= true ->
            case element(?SOBJECT_KIND, Cell) of
                {?ARRAYOBJ_TAG, Length} when Idx < Length ->
                    case element(?SOBJECT_PROPS, Cell) of
                        #{{?KEY_INDEX, Idx} := _} -> miss;
                        _ ->
                            case elem_write(element(?SOBJECT_ELEMENTS, Cell), Idx, V) of
                                miss -> miss;
                                NewE ->
                                    NewCell = setelement(?SOBJECT_ELEMENTS, Cell, NewE),
                                    with_store(St,
                                        with_data(Store,
                                            arc_rt_arena_ffi:set(Id, NewCell, Data)))
                            end
                    end;
                {?ARRAYOBJ_TAG, Length} when Idx =:= Length ->
                    case element(?SOBJECT_PROPS, Cell) of
                        #{{?KEY_INDEX, Idx} := _} -> miss;
                        _ ->
                            case elem_append(element(?SOBJECT_ELEMENTS, Cell), Idx, V) of
                                miss -> miss;
                                NewE ->
                                    NewCell = setelement(?SOBJECT_ELEMENTS,
                                        setelement(?SOBJECT_KIND, Cell,
                                            {?ARRAYOBJ_TAG, Length + 1}),
                                        NewE),
                                    with_store(St,
                                        with_data(Store,
                                            arc_rt_arena_ffi:set(Id, NewCell, Data)))
                            end
                    end;
                Kind ->
                    case index_keys_in_props(Kind) of
                        true ->
                            index_prop_write(St, Store, Data, Id, Cell, Idx, V);
                        false -> miss
                    end
            end;
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case index_keys_in_props(element(?SOBJECT_KIND, Cell)) of
                true ->
                    case element(?SOBJECT_PROPS, Cell) of
                        #{{?KEY_INDEX, Idx} := _} ->
                            index_prop_write(St, Store, Data, Id, Cell, Idx, V);
                        _ -> miss
                    end;
                false -> miss
            end;
        _ -> miss
    end.

index_prop_write(St, Store, Data, Id, Cell, Idx, V) ->
    Props = element(?SOBJECT_PROPS, Cell),
    K = {?KEY_INDEX, Idx},
    case Props of
        #{K := Prop}
          when element(1, Prop) =:= ?DATAPROP_TAG,
               element(?DATAPROP_WRITABLE, Prop) =:= true ->
            NewProp = with_value(Prop, V),
            NewCell = with_props(Cell, Props#{K := NewProp}),
            Store1 = with_data(Store, arc_rt_arena_ffi:set(Id, NewCell, Data)),
            with_store(St, bump_epoch_if_global(Store1, Cell));
        #{K := _} -> miss;
        _ when element(?SOBJECT_EXTENSIBLE, Cell) =:= true ->
            case chain_takes_index_write(Data, element(?SOBJECT_PROTO, Cell), Idx,
                                         ?MAX_PROTO_HOPS) of
                false -> miss;
                true ->
                    Seq = element(?STORE_PROP_SEQ, Store),
                    Prop = ?PLAIN_PROPERTY(V, Seq),
                    NewCell = with_props(Cell, Props#{K => Prop}),
                    Store1 = store_put_seq(Store,
                                           arc_rt_arena_ffi:set(Id, NewCell, Data),
                                           Seq + 1),
                    with_store(St, bump_epoch_if_global(Store1, Cell))
            end;
        _ -> miss
    end.

chain_takes_index_write(_, ?NONE, _, _) -> true;
chain_takes_index_write(_, _, _, 0) -> false;
chain_takes_index_write(Data, {?SOME, {?HANDLE_TAG, PId}}, Idx, Fuel) ->
    case arc_rt_arena_ffi:get(PId, Data) of
        {?SSHAPED_TAG, _, P2, _, _} ->
            chain_takes_index_write(Data, P2, Idx, Fuel - 1);
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            Kind = element(?SOBJECT_KIND, Cell),
            Walk = index_keys_in_props(Kind) orelse element(1, Kind) =:= ?ARRAYOBJ_TAG
                orelse (element(1, Kind) =:= ?ARGUMENTSOBJ_TAG
                        andalso element(?ARGUMENTSOBJ_MAPPED, Kind) =:= ?NONE),
            case Walk of
                false -> false;
                true ->
                    case element(?SOBJECT_PROPS, Cell) of
                        #{{?KEY_INDEX, Idx} := Prop} ->
                            element(1, Prop) =:= ?DATAPROP_TAG andalso
                                element(?DATAPROP_WRITABLE, Prop) =:= true;
                        _ ->
                            chain_takes_index_write(Data,
                                                    element(?SOBJECT_PROTO, Cell),
                                                    Idx, Fuel - 1)
                    end
            end;
        _ -> false
    end;
chain_takes_index_write(_, _, _, _) -> false.

elem_write({?ELEMS_DENSE, A}, Idx, V) ->
    case Idx < arc_tree_array_ffi:size(A) of
        true -> {?ELEMS_DENSE, arc_tree_array_ffi:set(Idx, V, A)};
        false -> miss
    end;
elem_write({?ELEMS_SPARSE, M}, Idx, V) ->
    {?ELEMS_SPARSE, M#{Idx => V}};
elem_write(_, _, _) -> miss.

elem_append({?ELEMS_DENSE, A}, Idx, V) ->
    {?ELEMS_DENSE, arc_tree_array_ffi:set(Idx, V, A)};
elem_append({?ELEMS_SPARSE, M}, Idx, V) ->
    {?ELEMS_SPARSE, M#{Idx => V}};
elem_append(_, _, _) -> miss.

elem_write_grow(Els, Idx, V) -> ?ELEM_WRITE_GROW(Els, Idx, V).

native_token(Cell) -> ?NATIVE_TOKEN(Cell).

elem_at(Els, Idx) -> ?ELEM_AT(Els, Idx).

shape_slots_get(Slots, Off) -> ?SLOT_AT(Slots, Off).

shape_slots_new() -> {}.

shape_slots_set(Slots, Off, V) -> ?SLOT_SET(Slots, Off, V).

shape_slots_append(Slots, V) -> erlang:append_element(Slots, V).

shaped_write(Data, Shapes, {?SSHAPED_TAG, Sid, P, Slots, Offs} = Shaped, KeyBin,
             V) ->
    case Offs of
        #{KeyBin := Off} ->
            setelement(?SSHAPED_SLOTS, Shaped, ?SLOT_SET(Slots, Off, V));
        _ ->
            case shaped_next(Shapes, Sid, KeyBin) of
                miss -> miss;
                {To, ToOffs} ->
                    case named_write_walk(Data, Shapes, P, {?KEY_NAMED, KeyBin},
                                          ?MAX_PROTO_HOPS) of
                        true ->
                            {?SSHAPED_TAG, To, P, erlang:append_element(Slots, V),
                             ToOffs};
                        false -> miss
                    end
            end
    end.

%% true | false | {true, Store1} once a plain chain is remembered
remember_plain_chain(Store, Data, Proto, K) ->
    Shapes = element(?STORE_SHAPES, Store),
    case plain_chain_ids(Data, Shapes, Proto, ?MAX_PROTO_HOPS, []) of
        Ids when is_list(Ids) ->
            Free = lists:foldl(fun(Id, M) -> M#{Id => nil} end,
                               element(?STORE_FREE_PROTOS, Store), Ids),
            {true, setelement(?STORE_FREE_PROTOS, Store, Free)};
        false -> named_write_walk(Data, Shapes, Proto, K, ?MAX_PROTO_HOPS)
    end.

%% every hop ordinary or shaped with only writable named data, bar __proto__
plain_chain_ids(_, _, ?NONE, _, Ids) -> Ids;
plain_chain_ids(Data, Shapes, {?SOME, {?HANDLE_TAG, PId}}, Fuel, Ids) when Fuel > 0 ->
    case arc_rt_arena_ffi:get(PId, Data) of
        {?SSHAPED_TAG, _, P2, _, _} ->
            plain_chain_ids(Data, Shapes, P2, Fuel - 1, [PId | Ids]);
        {?SOBJECT_TAG, ?ORDINARY, P2, Props, _, _, _} ->
            props_all_plain_writable(maps:next(maps:iterator(Props)))
                andalso plain_chain_ids(Data, Shapes, P2, Fuel - 1, [PId | Ids]);
        _ -> false
    end;
plain_chain_ids(_, _, _, _, _) -> false.

props_all_plain_writable(none) -> true;
props_all_plain_writable({{?KEY_NAMED, _} = K, Prop, I}) ->
    case Prop of
        {?DATAPROP_TAG, _, true, _, _, _} -> props_all_plain_writable(maps:next(I));
        _ when K =:= ?PROTO_KEY -> props_all_plain_writable(maps:next(I));
        _ -> false
    end;
props_all_plain_writable({_, _, I}) -> props_all_plain_writable(maps:next(I)).

%% uncached walk behind chain_takes_named_write
named_write_walk(_, _, ?NONE, _, _) -> true;
named_write_walk(_, _, _, _, 0) -> false;
named_write_walk(Data, Shapes, {?SOME, {?HANDLE_TAG, PId}}, K, Fuel) ->
    case arc_rt_arena_ffi:get(PId, Data) of
        {?SSHAPED_TAG, _, P2, _, Offs} ->
            case Offs of
                #{element(2, K) := _} -> true;
                _ -> named_write_walk_next(Data, Shapes, P2, K, Fuel)
            end;
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Cell), element(2, K)) of
                true ->
                    case element(?SOBJECT_PROPS, Cell) of
                        #{K := Prop} ->
                            element(1, Prop) =:= ?DATAPROP_TAG andalso
                                element(?DATAPROP_WRITABLE, Prop) =:= true;
                        _ ->
                            named_write_walk_next(Data, Shapes,
                                                  element(?SOBJECT_PROTO, Cell), K,
                                                  Fuel)
                    end;
                false -> false
            end;
        _ -> false
    end;
named_write_walk(_, _, _, _, _) -> false.

named_write_walk_next(_, _, ?NONE, _, _) -> true;
named_write_walk_next(Data, Shapes, P, K, Fuel) ->
    named_write_walk(Data, Shapes, P, K, Fuel - 1).

live_cell(St, Id) ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, St))) of
        ?STORE_FREE_CELL -> miss;
        Cell -> Cell
    end.

%% object spread onto a fresh literal when the source holds only plain data
plain_copy_data_props(St, {?HANDLE_TAG, TId}, {?HANDLE_TAG, SId})
  when tuple_size(St) =:= ?AGENT_SIZE ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(TId, Data) of
        {?SOBJECT_TAG, ?ORDINARY, _, TProps, _, _, true} = TCell ->
            case source_pairs(arc_rt_arena_ffi:get(SId, Data)) of
                miss -> copy_miss;
                Pairs ->
                    case merge_pairs(Pairs, TProps, element(?STORE_PROP_SEQ, Store)) of
                        miss -> copy_miss;
                        {TProps2, Seq} ->
                            NewCell = setelement(?SOBJECT_PROPS, TCell, TProps2),
                            {copied, with_store(St, store_put_seq(Store,
                                arc_rt_arena_ffi:set(TId, NewCell, Data), Seq))}
                    end
            end;
        _ -> copy_miss
    end;
plain_copy_data_props(_, _, _) -> copy_miss.

source_pairs({?SOBJECT_TAG, ?ORDINARY, _, Props, [], ?ELEMS_NONE, _}) ->
    L = maps:to_list(Props),
    case lists:all(fun plain_named/1, L) of
        false -> miss;
        true ->
            Sorted = lists:sort(fun({_, A}, {_, B}) ->
                element(?DATAPROP_SEQ, A) =< element(?DATAPROP_SEQ, B) end, L),
            [{K, element(?DATAPROP_VALUE, P)}
             || {K, P} <- Sorted, element(?DATAPROP_ENUMERABLE, P) =:= true]
    end;
source_pairs({?SSHAPED_TAG, _, _, Slots, Offs}) ->
    [{{?KEY_NAMED, KB}, ?SLOT_AT(Slots, Off)}
     || {KB, Off} <- lists:keysort(2, maps:to_list(Offs))];
source_pairs(_) -> miss.

plain_named({{?KEY_NAMED, _}, P}) -> element(1, P) =:= ?DATAPROP_TAG;
plain_named(_) -> false.

merge_pairs([], Props, Seq) -> {Props, Seq};
merge_pairs([{K, V} | Rest], Props, Seq) ->
    case Props of
        #{K := Old} when element(1, Old) =:= ?DATAPROP_TAG ->
            merge_pairs(Rest,
                        Props#{K := ?PLAIN_PROPERTY(V, element(?DATAPROP_SEQ, Old))},
                        Seq);
        #{K := _} -> miss;
        _ ->
            merge_pairs(Rest, Props#{K => ?PLAIN_PROPERTY(V, Seq)}, Seq + 1)
    end.

%% §14.7.5.9 key list when the whole chain is plain named data
plain_for_in_keys(St, {?HANDLE_TAG, Id}) when tuple_size(St) =:= ?AGENT_SIZE ->
    Data = element(?STORE_DATA, element(?AGENT_STORE, St)),
    for_in_chain(Data, arc_rt_arena_ffi:get(Id, Data), #{}, [], ?MAX_PROTO_HOPS);
plain_for_in_keys(_, _) -> miss.

for_in_chain(_, _, _, _, 0) -> miss;
for_in_chain(Data, {?SSHAPED_TAG, _, Proto, _, Offs}, Seen, Acc, Fuel) ->
    Keys = [KB || {KB, _} <- lists:keysort(2, maps:to_list(Offs))],
    for_in_add(Data, Proto, Keys, [], Seen, Acc, Fuel);
for_in_chain(Data, {?SOBJECT_TAG, Kind, Proto, Props, _, ?ELEMS_NONE, _},
             Seen, Acc, Fuel) when Kind =:= ?ORDINARY; Kind =:= ?GLOBALOBJ ->
    case for_in_named(maps:to_list(Props), [], []) of
        miss -> miss;
        {Enum, Hidden} -> for_in_add(Data, Proto, Enum, Hidden, Seen, Acc, Fuel)
    end;
for_in_chain(_, _, _, _, _) -> miss.

for_in_add(Data, Proto, Enum, Hidden, Seen, Acc, Fuel) ->
    Acc1 = lists:foldl(fun(K, A) ->
               case is_map_key(K, Seen) of true -> A; false -> [K | A] end
           end, Acc, Enum),
    Seen1 = lists:foldl(fun(K, S) -> S#{K => []} end, Seen, Enum ++ Hidden),
    case Proto of
        ?NONE -> {plain_keys, [arc_rt_js_string_ffi:mk(K) || K <- lists:reverse(Acc1)]};
        {?SOME, {?HANDLE_TAG, P}} ->
            for_in_chain(Data, arc_rt_arena_ffi:get(P, Data), Seen1, Acc1, Fuel - 1)
    end.

%% enumerable named keys by seq plus the hidden ones; both property records
%% keep enumerable and seq at the same positions
for_in_named([], Enum, Hidden) ->
    Sorted = lists:sort(fun({A, _}, {B, _}) -> A =< B end, Enum),
    {[K || {_, K} <- Sorted], Hidden};
for_in_named([{{?KEY_NAMED, K}, P} | Rest], Enum, Hidden) ->
    case element(?DATAPROP_ENUMERABLE, P) of
        true ->
            for_in_named(Rest, [{element(?DATAPROP_SEQ, P), K} | Enum], Hidden);
        false -> for_in_named(Rest, Enum, [K | Hidden])
    end;
for_in_named(_, _, _) -> miss.

%% §7.3.24 own enumerable named data pairs in order
plain_own_enum_pairs(St, {?HANDLE_TAG, Id}) when tuple_size(St) =:= ?AGENT_SIZE ->
    Data = element(?STORE_DATA, element(?AGENT_STORE, St)),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, _, _, Slots, Offs} ->
            {plain_pairs, [{KB, ?SLOT_AT(Slots, Off)}
                           || {KB, Off} <- lists:keysort(2, maps:to_list(Offs))]};
        {?SOBJECT_TAG, ?ORDINARY, _, Props, _, ?ELEMS_NONE, _} ->
            case lists:all(fun plain_named/1, maps:to_list(Props)) of
                false -> miss;
                true ->
                    L = lists:sort(fun({_, A}, {_, B}) ->
                            element(?DATAPROP_SEQ, A) =< element(?DATAPROP_SEQ, B)
                        end, maps:to_list(Props)),
                    {plain_pairs, [{KB, element(?DATAPROP_VALUE, P)}
                                  || {{?KEY_NAMED, KB}, P} <- L,
                                     element(?DATAPROP_ENUMERABLE, P) =:= true]}
            end;
        _ -> miss
    end;
plain_own_enum_pairs(_, _) -> miss.

%% §10.1.8.1 over data props; a getter that returns this is the receiver
get_symbol_data(St, {?HANDLE_TAG, Id} = Recv, Sym) ->
    Data = element(?STORE_DATA, element(?AGENT_STORE, St)),
    symbol_walk(Data, Id, Sym, Recv, ?MAX_PROTO_HOPS);
get_symbol_data(_, _, _) -> miss.

symbol_walk(_, _, _, _, 0) -> miss;
symbol_walk(Data, Id, Sym, Recv, Fuel) ->
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, _, Proto, _} -> symbol_next(Data, Proto, Sym, Recv, Fuel);
        {?SOBJECT_TAG, Kind, Proto, _, SymProps, _, _} ->
            case is_tuple(Kind) andalso element(1, Kind) =:= ?PROXYOBJ_TAG of
                true -> miss;
                false ->
                    case lists:keyfind(Sym, 1, SymProps) of
                        {_, Prop} when element(1, Prop) =:= ?DATAPROP_TAG ->
                            element(?DATAPROP_VALUE, Prop);
                        {_, {?ACCESSORPROP_TAG, {?SOME, Getter}, _, _, _, _}} ->
                            getter_returns_this(Data, Getter, Recv);
                        {_, _} -> miss;
                        false -> symbol_next(Data, Proto, Sym, Recv, Fuel)
                    end
            end;
        _ -> miss
    end.

symbol_next(_, ?NONE, _, _, _) -> undefined;
symbol_next(Data, {?SOME, {?HANDLE_TAG, Id}}, Sym, Recv, Fuel) ->
    symbol_walk(Data, Id, Sym, Recv, Fuel - 1).

getter_returns_this(Data, {?HANDLE_TAG, G}, Recv) ->
    case arc_rt_arena_ffi:get(G, Data) of
        {?SOBJECT_TAG, {?NATIVEFN_TAG, return_this, _, _, _}, _, _, _, _, _} -> Recv;
        _ -> miss
    end;
getter_returns_this(_, _, _) -> miss.
