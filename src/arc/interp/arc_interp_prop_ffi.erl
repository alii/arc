%% property kernels for the interpreter; exports may answer miss, never raise
-module(arc_interp_prop_ffi).
-export([get_field/3, find_accessor/3, own_data/2, get_elem/3, get_elem2/3, put_field/5, put_elem/4,
         define_field/4, new_object/5, new_receiver/2, get_global/3,
         put_global/6]).

-include("../rt/arc_rt_layout.hrl").

%% §10.1.8.1 ordinary get, miss when anything observable
get_field(Agent, {?HANDLE_TAG, Id}, K) ->
    cell_field(element(?AGENT_STORE, Agent), Id, K, undefined);
get_field(_, Bin, ?LENGTH_KEY) when is_binary(Bin) ->
    byte_size(Bin);
get_field(_, {?STR_TAG, _, Len, _}, ?LENGTH_KEY) ->
    Len;
get_field(Agent, S, K) when ?IS_STR(S) ->
    proto_field(Agent, ?REALM_STRING, K);
get_field(Agent, N, K) when is_number(N) ->
    proto_field(Agent, ?REALM_NUMBER, K);
get_field(_, _, _) -> miss.

own_data(Props, K) ->
    case Props of
        #{K := Prop} when element(1, Prop) =:= ?DATAPROP_TAG ->
            element(?DATAPROP_VALUE, Prop);
        _ -> miss
    end.

%% the accessor K resolves to along a plain chain, else no_accessor
find_accessor(Agent, {?HANDLE_TAG, Id}, K) ->
    Data = element(?STORE_DATA, element(?AGENT_STORE, Agent)),
    accessor_walk(Data, arc_rt_arena_ffi:get(Id, Data), K, ?MAX_PROTO_HOPS);
find_accessor(_, _, _) -> no_accessor.

accessor_walk(_, _, _, 0) -> no_accessor;
accessor_walk(Data, {?SSHAPED_TAG, _, Proto, _, Offs}, K, Fuel) ->
    case is_map_key(element(2, K), Offs) of
        true -> no_accessor;
        false -> accessor_next(Data, Proto, K, Fuel)
    end;
accessor_walk(Data, Cell, K, Fuel) when element(1, Cell) =:= ?SOBJECT_TAG ->
    case named_plain(element(?SOBJECT_KIND, Cell), K) of
        false -> no_accessor;
        true ->
            case element(?SOBJECT_PROPS, Cell) of
                #{K := Prop} when element(1, Prop) =:= ?ACCESSORPROP_TAG ->
                    {accessor, element(?ACCESSORPROP_GET, Prop),
                     element(?ACCESSORPROP_SET, Prop)};
                #{K := _} -> no_accessor;
                _ -> accessor_next(Data, element(?SOBJECT_PROTO, Cell), K, Fuel)
            end
    end;
accessor_walk(_, _, _, _) -> no_accessor.

accessor_next(Data, {?SOME, {?HANDLE_TAG, P}}, K, Fuel) ->
    accessor_walk(Data, arc_rt_arena_ffi:get(P, Data), K, Fuel - 1);
accessor_next(_, _, _, _) -> no_accessor.

%% §9.1.1.4.6 global getbindingvalue, plain case
get_global(Agent, Lex, Name) ->
    case Lex of
        #{Name := Binding} ->
            case element(?LEXICAL_GLOBAL_VALUE, Binding) of
                js_tdz -> miss;
                V -> V
            end;
        _ ->
            {?HANDLE_TAG, G} = element(?REALM_GLOBAL, element(?AGENT_REALM, Agent)),
            cell_field(element(?AGENT_STORE, Agent), G, {?KEY_NAMED, Name}, miss)
    end.

%% §9.1.1.4.5 setmutablebinding on the global object
put_global(Store, Lex, Global, Name, V, Strict) ->
    case is_map_key(Name, Lex) of
        true -> miss;
        false -> put_field(Store, Global, {?KEY_NAMED, Name}, V, not Strict)
    end.

%% getters miss so the general path passes the primitive as this
proto_field(Agent, Which, K) ->
    Pair = element(Which, element(?AGENT_REALM, Agent)),
    {?HANDLE_TAG, Id} = element(?PAIR_PROTO, Pair),
    cell_field(element(?AGENT_STORE, Agent), Id, K, undefined).

cell_field(Store, Id, K, Absent) ->
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, _, Proto, Slots, Offs} ->
            KeyBin = element(2, K),
            case Offs of
                #{KeyBin := Off} -> ?SLOT_AT(Slots, Off);
                _ -> field_next(Data, Proto, K, ?MAX_PROTO_HOPS, Absent)
            end;
        {?SOBJECT_TAG, ?ORDINARY, Proto, Props, _, _, _} ->
            case Props of
                #{K := Prop} when element(1, Prop) =:= ?DATAPROP_TAG ->
                    element(?DATAPROP_VALUE, Prop);
                #{K := _} -> miss;
                _ -> field_next(Data, Proto, K, ?MAX_PROTO_HOPS, Absent)
            end;
        Cell -> hop(Data, Cell, K, ?MAX_PROTO_HOPS, Absent)
    end.

hop(Data, Cell, K, Fuel, Absent) ->
    case Cell of
        {?SSHAPED_TAG, _, Proto, Slots, Offs} ->
            KeyBin = element(2, K),
            case Offs of
                #{KeyBin := Off} -> ?SLOT_AT(Slots, Off);
                _ -> field_next(Data, Proto, K, Fuel, Absent)
            end;
        _ when element(1, Cell) =:= ?SOBJECT_TAG ->
            Kind = element(?SOBJECT_KIND, Cell),
            case named_plain(Kind, K) of
                false -> named_virtual(Kind, K);
                true ->
                    case element(?SOBJECT_PROPS, Cell) of
                        #{K := Prop} ->
                            case element(1, Prop) of
                                ?DATAPROP_TAG -> element(?DATAPROP_VALUE, Prop);
                                _ -> miss
                            end;
                        _ ->
                            field_next(Data, element(?SOBJECT_PROTO, Cell), K,
                                       Fuel, Absent)
                    end
            end;
        _ -> miss
    end.

field_next(_, ?NONE, _, _, Absent) -> Absent;
field_next(Data, {?SOME, {?HANDLE_TAG, P}}, K, Fuel, Absent) when Fuel > 1 ->
    hop(Data, arc_rt_arena_ffi:get(P, Data), K, Fuel - 1, Absent);
field_next(_, _, _, _, _) -> miss.

named_virtual({?ARRAYOBJ_TAG, Length}, ?LENGTH_KEY) -> Length;
named_virtual(_, _) -> miss.

-compile({inline, [named_plain/2, named_virtual/2, birth_plain/2, cell_field/4,
                   hop/5, proto_field/3, put_prop/7, put_new/6, set_plain/5,
                   shaped_grow/7, shaped_next/3, chain_takes_write/4,
                   literal_props/3]}).
named_plain(Kind, K) -> ?NAMED_KEY_IS_PLAIN(Kind, K, ?LENGTH_KEY).

birth_plain(Birth, K) ->
    ?LAZY_KEY_IS_PLAIN(Birth, K, ?LENGTH_KEY, {?KEY_NAMED, <<"name">>},
                       {?KEY_NAMED, <<"prototype">>}).

%% holes miss so the full path walks the proto chain
get_elem(Store, {?HANDLE_TAG, Id}, Idx) when is_integer(Idx), Idx >= 0 ->
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length}, _, Props, _, Elems, _} ->
            if
                Idx >= Length -> miss;
                Props =:= #{} -> elem_read(Elems, Idx);
                is_map_key({?KEY_INDEX, Idx}, Props) -> miss;
                true -> elem_read(Elems, Idx)
            end;
        {?SOBJECT_TAG, {?ARGUMENTSOBJ_TAG, _, _}, _, Props, _, Elems, _} ->
            case is_map_key({?KEY_INDEX, Idx}, Props) of
                true -> miss;
                false -> elem_read(Elems, Idx)
            end;
        {?SOBJECT_TAG, Kind, Proto, Props, _, _, _} when Idx =< ?MAX_ARRAY_INDEX ->
            case index_kind_is_plain(Kind) of
                false -> miss;
                true ->
                    case Props of
                        #{{?KEY_INDEX, Idx} := Prop} ->
                            case element(1, Prop) of
                                ?DATAPROP_TAG -> element(?DATAPROP_VALUE, Prop);
                                _ -> miss
                            end;
                        _ ->
                            case chain_lacks_index(Data, Proto, Idx,
                                                   ?MAX_PROTO_HOPS) of
                                true -> undefined;
                                false -> miss
                            end
                    end
            end;
        _ -> miss
    end;
get_elem(_, S, Idx) when is_integer(Idx), ?IS_STR(S) ->
    case arc_rt_js_string_ffi:char_at_val(S, Idx) of
        {some, Ch} -> Ch;
        none -> miss
    end;
get_elem(Store, {?HANDLE_TAG, _} = Obj, Key) when ?IS_STR(Key) ->
    case arc_rt_val_ffi:property_key_of(Key) of
        {?OKEY_STRING, {?KEY_NAMED, _} = K} ->
            cell_field(Store, element(?HANDLE_ID, Obj), K, undefined);
        {?OKEY_STRING, {?KEY_INDEX, Idx}} -> get_elem(Store, Obj, Idx);
        _ -> miss
    end;
get_elem(_, _, _) -> miss.

%% only an integer key is its own canonical key
get_elem2(Store, Obj, Idx) when is_integer(Idx) -> get_elem(Store, Obj, Idx);
get_elem2(_, _, _) -> miss.

-compile({inline, [elem_read/2, elem_overwrite/3]}).
elem_read({?ELEMS_DENSE, {?VEC_TAG, _, _, _, _, _} = A}, Idx) ->
    case arc_tree_array_ffi:get(Idx, A) of
        ?ELEMS_HOLE -> miss;
        V -> V
    end;
elem_read({?ELEMS_DENSE, T}, Idx) when Idx < tuple_size(T) ->
    case element(Idx + 1, T) of
        ?ELEMS_HOLE -> miss;
        V -> V
    end;
elem_read({?ELEMS_SPARSE, M}, Idx) ->
    case M of
        #{Idx := V} -> V;
        _ -> miss
    end;
elem_read(_, _) -> miss.

%% §10.1.9.2 ordinary set, plain writable data only
put_field(Store, {?HANDLE_TAG, Id}, K, V, Create)
  when tuple_size(Store) =:= ?STORE_SIZE ->
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, Sid, P, Slots, Offs} = Cell ->
            KeyBin = element(2, K),
            case Offs of
                #{KeyBin := Off} ->
                    NewCell = setelement(?SSHAPED_SLOTS, Cell,
                                         ?SLOT_SET(Slots, Off, V)),
                    setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, NewCell, Data));
                _ when Create ->
                    case shaped_next(Store, Sid, KeyBin) of
                        miss -> miss;
                        Next ->
                            case chain_takes_write(Store, Data, P, K) of
                                false -> miss;
                                true ->
                                    shaped_grow(Store, Data, Id, Next, P, Slots, V);
                                {true, Store1} ->
                                    shaped_grow(Store1, Data, Id, Next, P, Slots, V)
                            end
                    end;
                _ -> miss
            end;
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Cell), K) of
                false -> miss;
                true -> put_prop(Store, Data, Id, Cell, K, V, Create)
            end;
        _ -> miss
    end;
put_field(_, _, _, _, _) -> miss.

put_prop(Store, Data, Id, Cell, K, V, Create) ->
    {_, Kind, Proto, Props, Sym, Elems, Ext} = Cell,
    case Props of
        #{K := {?DATAPROP_TAG, _, true, E, C, Sq}} ->
            NewCell = {?SOBJECT_TAG, Kind, Proto,
                       Props#{K := {?DATAPROP_TAG, V, true, E, C, Sq}},
                       Sym, Elems, Ext},
            set_plain(Store, Id, NewCell, Data, Kind);
        #{K := _} -> miss;
        _ when Create, Ext =:= true ->
            case chain_takes_write(Store, Data, Proto, K) of
                false -> miss;
                true -> put_new(Store, Data, Id, Cell, K, V);
                {true, Store1} -> put_new(Store1, Data, Id, Cell, K, V)
            end;
        _ -> miss
    end.

shaped_next(Store, Sid, KeyBin) ->
    ?SHAPED_NEXT(element(?STORE_SHAPES, Store), Sid, KeyBin).

shaped_grow(Store, Data, Id, {To, ToOffs}, P, Slots, V)
  when tuple_size(Store) =:= ?STORE_SIZE ->
    NewCell = {?SSHAPED_TAG, To, P, erlang:append_element(Slots, V), ToOffs},
    setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, NewCell, Data)).

put_new(Store, Data, Id, Cell, K, V) when tuple_size(Store) =:= ?STORE_SIZE ->
    {_, Kind, Proto, Props, Sym, Elems, Ext} = Cell,
    Seq = element(?STORE_PROP_SEQ, Store),
    NewCell = {?SOBJECT_TAG, Kind, Proto,
               Props#{K => ?PLAIN_PROPERTY(V, Seq)},
               Sym, Elems, Ext},
    setelement(?STORE_PROP_SEQ, set_plain(Store, Id, NewCell, Data, Kind), Seq + 1).

%% global object readers watch the epoch
set_plain(Store, Id, Cell, Data, ?GLOBALOBJ) when tuple_size(Store) =:= ?STORE_SIZE ->
    setelement(?STORE_GLOBAL_EPOCH,
               setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, Cell, Data)),
               element(?STORE_GLOBAL_EPOCH, Store) + 1);
set_plain(Store, Id, Cell, Data, _) when tuple_size(Store) =:= ?STORE_SIZE ->
    setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, Cell, Data)).

%% §7.3.5 createdataproperty on ordinary extensible object
define_field(Store, {?HANDLE_TAG, Id}, K, V)
  when tuple_size(Store) =:= ?STORE_SIZE ->
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, Sid, P, Slots, Offs} = Cell
          when element(1, K) =:= ?KEY_NAMED ->
            KeyBin = element(2, K),
            case Offs of
                #{KeyBin := Off} ->
                    NewCell = setelement(?SSHAPED_SLOTS, Cell,
                                         ?SLOT_SET(Slots, Off, V)),
                    setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, NewCell, Data));
                _ ->
                    case shaped_next(Store, Sid, KeyBin) of
                        miss -> miss;
                        Next -> shaped_grow(Store, Data, Id, Next, P, Slots, V)
                    end
            end;
        {?SOBJECT_TAG, ?ORDINARY, Proto, Props, Sym, Elems, true} ->
            case Props of
                #{K := {?DATAPROP_TAG, _, _, _, true, Sq}} ->
                    NewCell = {?SOBJECT_TAG, ?ORDINARY, Proto,
                               Props#{K := {?DATAPROP_TAG, V, true, true, true, Sq}},
                               Sym, Elems, true},
                    setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, NewCell, Data));
                #{K := _} -> miss;
                _ ->
                    Seq = element(?STORE_PROP_SEQ, Store),
                    NewCell = {?SOBJECT_TAG, ?ORDINARY, Proto,
                               Props#{K => ?PLAIN_PROPERTY(V, Seq)},
                               Sym, Elems, true},
                    setelement(?STORE_PROP_SEQ,
                               setelement(?STORE_DATA, Store,
                                          arc_rt_arena_ffi:set(Id, NewCell, Data)),
                               Seq + 1)
            end;
        _ -> miss
    end;
define_field(_, _, _, _) -> miss.

%% keys given last first, values on top of stack
new_object(Store, Proto, Keys, N, Stack) when tuple_size(Store) =:= ?STORE_SIZE ->
    Seq = element(?STORE_PROP_SEQ, Store),
    {Props, Stack2} = literal_props(Keys, Stack, Seq),
    Cell = {?SOBJECT_TAG, ?ORDINARY, {?SOME, Proto}, Props, [], ?ELEMS_NONE, true},
    Id = element(?STORE_NEXT, Store),
    Store2 = setelement(?STORE_DATA, Store,
                        arc_rt_arena_ffi:set(Id, Cell, element(?STORE_DATA, Store))),
    Store3 = setelement(?STORE_NEXT, Store2, Id + 1),
    Store4 = setelement(?STORE_ALLOC_SINCE_GC, Store3,
                        element(?STORE_ALLOC_SINCE_GC, Store) + 1),
    {{?HANDLE_TAG, Id}, Stack2, setelement(?STORE_PROP_SEQ, Store4, Seq + N)}.

%% §10.1.13 once prototype has been read
new_receiver(Agent, {?HANDLE_TAG, _} = Proto)
  when tuple_size(Agent) =:= ?AGENT_SIZE ->
    case element(?AGENT_STORE, Agent) of
        Store when tuple_size(Store) =:= ?STORE_SIZE ->
            Cell = {?SSHAPED_TAG, 0, {?SOME, Proto}, {}, #{}},
            Id = element(?STORE_NEXT, Store),
            Store2 = setelement(?STORE_DATA, Store,
                                arc_rt_arena_ffi:set(Id, Cell, element(?STORE_DATA, Store))),
            Store3 = setelement(?STORE_NEXT, Store2, Id + 1),
            Store4 = setelement(?STORE_ALLOC_SINCE_GC, Store3,
                                element(?STORE_ALLOC_SINCE_GC, Store) + 1),
            {{?HANDLE_TAG, Id}, setelement(?AGENT_STORE, Agent, Store4)};
        _ -> miss
    end;
new_receiver(_, _) -> miss.

literal_props([], Stack, _) -> {#{}, Stack};
literal_props([K1], [V1 | Stack], Seq) ->
    {#{K1 => ?PLAIN_PROPERTY(V1, Seq)}, Stack};
literal_props([K2, K1], [V2, V1 | Stack], Seq) ->
    {#{K1 => ?PLAIN_PROPERTY(V1, Seq), K2 => ?PLAIN_PROPERTY(V2, Seq + 1)}, Stack};
literal_props(Keys, Stack, Seq) ->
    {Pairs, Stack2} = literal_pairs(Keys, Stack, Seq + length(Keys) - 1, []),
    {maps:from_list(Pairs), Stack2}.

literal_pairs([K | Keys], [V | Stack], Seq, Acc) ->
    literal_pairs(Keys, Stack, Seq - 1, [{K, ?PLAIN_PROPERTY(V, Seq)} | Acc]);
literal_pairs([], Stack, _, Acc) -> {Acc, Stack}.

chain_takes_write(Store, _, {?SOME, {?HANDLE_TAG, PId}}, {?KEY_NAMED, KB})
  when is_map_key(PId, element(?STORE_FREE_PROTOS, Store)),
       byte_size(KB) =/= 9 orelse KB =/= <<"__proto__">> ->
    true;
chain_takes_write(Store, Data, Proto, {?KEY_NAMED, _} = K) ->
    arc_rt_obj_ffi:chain_takes_named_write(Store, Data, Proto, K);
chain_takes_write(_, Data, Proto, {?KEY_INDEX, Idx}) ->
    chain_lacks_index(Data, Proto, Idx, ?MAX_PROTO_HOPS).

%% creating an element needs free proto chain, writable length
put_elem(Store, {?HANDLE_TAG, Id}, Idx, V)
  when is_integer(Idx), Idx >= 0, tuple_size(Store) =:= ?STORE_SIZE ->
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length} = Kind, Proto, Props, Sym, Elems, true}
          when Props =:= #{}; not is_map_key({?KEY_INDEX, Idx}, Props) ->
            if
                Idx < Length ->
                    NewE = case elem_overwrite(Elems, Idx, V) of
                        hole ->
                            case chain_lacks_index(Data, Proto, Idx,
                                                   ?MAX_PROTO_HOPS) of
                                true -> elem_write_grow(Elems, Idx, V);
                                false -> miss
                            end;
                        E -> E
                    end,
                    case NewE of
                        miss -> miss;
                        _ ->
                            NewCell = {?SOBJECT_TAG, Kind, Proto, Props, Sym, NewE, true},
                            setelement(?STORE_DATA, Store,
                                       arc_rt_arena_ffi:set(Id, NewCell, Data))
                    end;
                Idx =:= Length, Idx =< ?MAX_ARRAY_INDEX ->
                    case length_writable(Props)
                         andalso chain_lacks_index(Data, Proto, Idx,
                                                   ?MAX_PROTO_HOPS) of
                        false -> miss;
                        true ->
                            case elem_write_grow(Elems, Idx, V) of
                                miss -> miss;
                                NewE ->
                                    NewCell = {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Idx + 1},
                                               Proto, Props, Sym, NewE, true},
                                    setelement(?STORE_DATA, Store,
                                               arc_rt_arena_ffi:set(Id, NewCell, Data))
                            end
                    end;
                true -> miss
            end;
        {?SOBJECT_TAG, Kind, _, _, _, _, true} = Cell
          when is_atom(Kind), Idx =< ?MAX_ARRAY_INDEX ->
            put_prop(Store, Data, Id, Cell, {?KEY_INDEX, Idx}, V, true);
        _ -> miss
    end;
put_elem(Store, {?HANDLE_TAG, _} = Obj, Key, V) when ?IS_STR(Key) ->
    case arc_rt_val_ffi:property_key_of(Key) of
        {?OKEY_STRING, {?KEY_NAMED, _} = K} -> put_field(Store, Obj, K, V, true);
        {?OKEY_STRING, {?KEY_INDEX, Idx}} -> put_elem(Store, Obj, Idx, V);
        _ -> miss
    end;
put_elem(_, _, _, _) -> miss.

length_writable(#{?LENGTH_KEY := Prop})
  when element(1, Prop) =:= ?DATAPROP_TAG ->
    element(?DATAPROP_WRITABLE, Prop) =:= true;
length_writable(_) -> true.

chain_lacks_index(_, ?NONE, _, _) -> true;
chain_lacks_index(_, _, _, 0) -> false;
chain_lacks_index(Data, {?SOME, {?HANDLE_TAG, P}}, Idx, Fuel) ->
    case arc_rt_arena_ffi:get(P, Data) of
        {?SSHAPED_TAG, _, Proto, _, Offs} ->
            (not is_map_key(integer_to_binary(Idx), Offs))
                andalso chain_lacks_index(Data, Proto, Idx, Fuel - 1);
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length}, Proto, _, _, _, _}
          when Idx >= Length ->
            chain_lacks_index(Data, Proto, Idx, Fuel - 1);
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            index_kind_is_plain(element(?SOBJECT_KIND, Cell))
                andalso (not is_map_key({?KEY_INDEX, Idx},
                                        element(?SOBJECT_PROPS, Cell)))
                andalso (not elem_has(element(?SOBJECT_ELEMENTS, Cell), Idx))
                andalso chain_lacks_index(Data, element(?SOBJECT_PROTO, Cell),
                                          Idx, Fuel - 1);
        _ -> false
    end;
chain_lacks_index(_, _, _, _) -> false.

index_kind_is_plain(Kind) when is_atom(Kind) -> true;
index_kind_is_plain(Kind) ->
    case element(1, Kind) of
        ?PROXYOBJ_TAG -> false;
        ?MODULENS_TAG -> false;
        ?TYPEDARRAYOBJ_TAG -> false;
        ?STRINGOBJ_TAG -> false;
        _ -> true
    end.

elem_has({?ELEMS_DENSE, A}, Idx) -> arc_tree_array_ffi:get(Idx, A) =/= ?ELEMS_HOLE;
elem_has({?ELEMS_SPARSE, M}, Idx) -> is_map_key(Idx, M);
elem_has(_, _) -> false.

elem_overwrite({?ELEMS_DENSE, {?VEC_TAG, _, _, _, _, _} = A}, Idx, V) ->
    case arc_tree_array_ffi:get(Idx, A) of
        ?ELEMS_HOLE -> hole;
        _ -> {?ELEMS_DENSE, arc_tree_array_ffi:set(Idx, V, A)}
    end;
elem_overwrite({?ELEMS_DENSE, T}, Idx, V) when Idx < tuple_size(T) ->
    case element(Idx + 1, T) of
        ?ELEMS_HOLE -> hole;
        _ -> {?ELEMS_DENSE, setelement(Idx + 1, T, V)}
    end;
elem_overwrite({?ELEMS_SPARSE, M}, Idx, V) ->
    case M of
        #{Idx := _} -> {?ELEMS_SPARSE, M#{Idx := V}};
        _ -> hole
    end;
elem_overwrite(_, _, _) -> hole.

elem_write_grow(Els, Idx, V) -> ?ELEM_WRITE_GROW(Els, Idx, V).
