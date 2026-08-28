%% property fast paths: answer or miss, never raise
-module(arc_interp_prop_ffi).
-export([get_field/3, own_data/2, get_elem/3, get_elem2/3, put_field/5, put_elem/4,
         define_field/4, new_object/5, new_receiver/2, get_global/3,
         put_global/6]).

-include("../rt/arc_rt_layout.hrl").

-define(LENGTH_KEY, {?KEY_NAMED, <<"length">>}).

%% §10.1.8.1 ordinary get, miss when anything observable
get_field(Agent, {?HANDLE_TAG, Id}, K) ->
    cell_field(element(?AGENT_STORE, Agent), Id, K, undefined);
get_field(_, Bin, ?LENGTH_KEY) when is_binary(Bin) ->
    arc_string_ffi:string_codepoint_length(Bin);
get_field(Agent, Bin, K) when is_binary(Bin) ->
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

%% §9.1.1.4.6 global getbindingvalue, plain case
get_global(Agent, Lex, Name) ->
    case Lex of
        #{Name := Binding} ->
            case element(2, Binding) of
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

%% getters miss so slow path passes primitive as this
proto_field(Agent, Which, K) ->
    Pair = element(Which, element(?AGENT_REALM, Agent)),
    {?HANDLE_TAG, Id} = element(?PAIR_PROTO, Pair),
    cell_field(element(?AGENT_STORE, Agent), Id, K, undefined).

%% 64 is max proto hops before miss
cell_field(Store, Id, K, Absent) ->
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, _, Proto, Slots, Offs} ->
            KeyBin = element(2, K),
            case Offs of
                #{KeyBin := Off} -> element(Off + 1, Slots);
                _ -> field_next(Data, Proto, K, 64, Absent)
            end;
        {?SOBJECT_TAG, ?ORDINARY, Proto, Props, _, _, _} ->
            case Props of
                #{K := Prop} when element(1, Prop) =:= ?DATAPROP_TAG ->
                    element(?DATAPROP_VALUE, Prop);
                #{K := _} -> miss;
                _ -> field_next(Data, Proto, K, 64, Absent)
            end;
        Slot -> hop(Data, Slot, K, 64, Absent)
    end.

hop(Data, Slot, K, Fuel, Absent) ->
    case Slot of
        {?SSHAPED_TAG, _, Proto, Slots, Offs} ->
            KeyBin = element(2, K),
            case Offs of
                #{KeyBin := Off} -> element(Off + 1, Slots);
                _ -> field_next(Data, Proto, K, Fuel, Absent)
            end;
        _ when element(1, Slot) =:= ?SOBJECT_TAG ->
            Kind = element(?SOBJECT_KIND, Slot),
            case named_plain(Kind, K) of
                false -> named_virtual(Kind, K);
                true ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{K := Prop} ->
                            case element(1, Prop) of
                                ?DATAPROP_TAG -> element(?DATAPROP_VALUE, Prop);
                                _ -> miss
                            end;
                        _ ->
                            field_next(Data, element(?SOBJECT_PROTO, Slot), K,
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

%% false when named keys on this kind are exotic or virtual
-compile({inline, [named_plain/2, named_virtual/2, birth_plain/2, cell_field/4,
                   hop/5, proto_field/3, put_prop/7, put_new/6, set_plain/5, shaped_grow/7, shaped_next/3,
                   chain_free/4,
                   literal_props/3]}).
named_plain(?ORDINARY, _) -> true;
named_plain(Kind, _) when is_atom(Kind) -> true;
named_plain(Kind, K) ->
    case element(1, Kind) of
        ?PROXYOBJ_TAG -> false;
        module_namespace -> false;
        typed_array_obj -> false;
        ?ARRAYOBJ_TAG -> K =/= ?LENGTH_KEY;
        string_obj -> K =/= ?LENGTH_KEY;
        ?KBYTECODE_TAG -> birth_plain(element(?KBYTECODE_BIRTH, Kind), K);
        ?KFN_TAG -> birth_plain(element(?KFN_BIRTH, Kind), K);
        _ -> true
    end.

%% length/name/prototype not in props until birth settled
birth_plain(?BIRTH_SETTLED, _) -> true;
birth_plain(_, ?LENGTH_KEY) -> false;
birth_plain(_, {?KEY_NAMED, <<"name">>}) -> false;
birth_plain(Birth, {?KEY_NAMED, <<"prototype">>}) ->
    element(?BIRTH_PROTOTYPE_PARENT, Birth) =:= ?NONE;
birth_plain(_, _) -> true.

%% holes miss so the full path walks the proto chain
-define(MAX_ARRAY_INDEX, 4294967294).
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
            case index_is_plain(Kind) of
                false -> miss;
                true ->
                    case Props of
                        #{{?KEY_INDEX, Idx} := Prop} ->
                            case element(1, Prop) of
                                ?DATAPROP_TAG -> element(?DATAPROP_VALUE, Prop);
                                _ -> miss
                            end;
                        _ ->
                            case index_free(Data, Proto, Idx, 64) of
                                true -> undefined;
                                false -> miss
                            end
                    end
            end;
        _ -> miss
    end;
get_elem(Store, {?HANDLE_TAG, _} = Obj, Key) when is_binary(Key) ->
    case arc_rt_val_ffi:t_to_property_key_fast(Key) of
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
  when tuple_size(Store) =:= ?STORE_ARITY ->
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, Sid, P, Slots, Offs} = Slot ->
            KeyBin = element(2, K),
            case Offs of
                #{KeyBin := Off} ->
                    NewSlot = setelement(?SSHAPED_SLOTS, Slot,
                                         setelement(Off + 1, Slots, V)),
                    setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, NewSlot, Data));
                _ when Create ->
                    case shaped_next(Store, Sid, KeyBin) of
                        miss -> miss;
                        Next ->
                            case chain_free(Store, Data, P, K) of
                                false -> miss;
                                true ->
                                    shaped_grow(Store, Data, Id, Next, P, Slots, V);
                                {true, Store1} ->
                                    shaped_grow(Store1, Data, Id, Next, P, Slots, V)
                            end
                    end;
                _ -> miss
            end;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Slot), K) of
                false -> miss;
                true -> put_prop(Store, Data, Id, Slot, K, V, Create)
            end;
        _ -> miss
    end;
put_field(_, _, _, _, _) -> miss.

put_prop(Store, Data, Id, Slot, K, V, Create) ->
    {_, Kind, Proto, Props, Sym, Elems, Ext} = Slot,
    case Props of
        #{K := {?DATAPROP_TAG, _, true, E, C, Sq}} ->
            NewSlot = {?SOBJECT_TAG, Kind, Proto,
                       Props#{K := {?DATAPROP_TAG, V, true, E, C, Sq}},
                       Sym, Elems, Ext},
            set_plain(Store, Id, NewSlot, Data, Kind);
        #{K := _} -> miss;
        _ when Create, Ext =:= true ->
            case chain_free(Store, Data, Proto, K) of
                false -> miss;
                true -> put_new(Store, Data, Id, Slot, K, V);
                {true, Store1} -> put_new(Store1, Data, Id, Slot, K, V)
            end;
        _ -> miss
    end.

%% known successor shape for adding keybin, as {To, ToOffsets}
shaped_next(Store, Sid, KeyBin) ->
    Shapes = element(?STORE_SHAPES, Store),
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

shaped_grow(Store, Data, Id, {To, ToOffs}, P, Slots, V)
  when tuple_size(Store) =:= ?STORE_ARITY ->
    NewSlot = {?SSHAPED_TAG, To, P, erlang:append_element(Slots, V), ToOffs},
    setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, NewSlot, Data)).

put_new(Store, Data, Id, Slot, K, V) when tuple_size(Store) =:= ?STORE_ARITY ->
    {_, Kind, Proto, Props, Sym, Elems, Ext} = Slot,
    Seq = element(?STORE_PROP_SEQ, Store),
    NewSlot = {?SOBJECT_TAG, Kind, Proto,
               Props#{K => {?DATAPROP_TAG, V, true, true, true, Seq}},
               Sym, Elems, Ext},
    setelement(?STORE_PROP_SEQ, set_plain(Store, Id, NewSlot, Data, Kind), Seq + 1).

%% global object readers watch the epoch
set_plain(Store, Id, Slot, Data, ?GLOBALOBJ) when tuple_size(Store) =:= ?STORE_ARITY ->
    setelement(?STORE_GLOBAL_EPOCH,
               setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, Slot, Data)),
               element(?STORE_GLOBAL_EPOCH, Store) + 1);
set_plain(Store, Id, Slot, Data, _) when tuple_size(Store) =:= ?STORE_ARITY ->
    setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, Slot, Data)).

%% §7.3.5 createdataproperty on ordinary extensible object
define_field(Store, {?HANDLE_TAG, Id}, K, V)
  when tuple_size(Store) =:= ?STORE_ARITY ->
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, Sid, P, Slots, Offs} = Slot
          when element(1, K) =:= ?KEY_NAMED ->
            KeyBin = element(2, K),
            case Offs of
                #{KeyBin := Off} ->
                    NewSlot = setelement(?SSHAPED_SLOTS, Slot,
                                         setelement(Off + 1, Slots, V)),
                    setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, NewSlot, Data));
                _ ->
                    case shaped_next(Store, Sid, KeyBin) of
                        miss -> miss;
                        Next -> shaped_grow(Store, Data, Id, Next, P, Slots, V)
                    end
            end;
        {?SOBJECT_TAG, ?ORDINARY, Proto, Props, Sym, Elems, true} ->
            case Props of
                #{K := {?DATAPROP_TAG, _, _, _, true, Sq}} ->
                    NewSlot = {?SOBJECT_TAG, ?ORDINARY, Proto,
                               Props#{K := {?DATAPROP_TAG, V, true, true, true, Sq}},
                               Sym, Elems, true},
                    setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, NewSlot, Data));
                #{K := _} -> miss;
                _ ->
                    Seq = element(?STORE_PROP_SEQ, Store),
                    NewSlot = {?SOBJECT_TAG, ?ORDINARY, Proto,
                               Props#{K => {?DATAPROP_TAG, V, true, true, true, Seq}},
                               Sym, Elems, true},
                    setelement(?STORE_PROP_SEQ,
                               setelement(?STORE_DATA, Store,
                                          arc_rt_arena_ffi:set(Id, NewSlot, Data)),
                               Seq + 1)
            end;
        _ -> miss
    end;
define_field(_, _, _, _) -> miss.

%% keys given last first, values on top of stack
new_object(Store, Proto, Keys, N, Stack) when tuple_size(Store) =:= ?STORE_ARITY ->
    Seq = element(?STORE_PROP_SEQ, Store),
    {Props, Stack2} = literal_props(Keys, Stack, Seq),
    Slot = {?SOBJECT_TAG, ?ORDINARY, {?SOME, Proto}, Props, [], ?ELEMS_NONE, true},
    Id = element(?STORE_NEXT, Store),
    Store2 = setelement(?STORE_DATA, Store,
                        arc_rt_arena_ffi:set(Id, Slot, element(?STORE_DATA, Store))),
    Store3 = setelement(?STORE_NEXT, Store2, Id + 1),
    Store4 = setelement(?STORE_ALLOC, Store3, element(?STORE_ALLOC, Store) + 1),
    {{?HANDLE_TAG, Id}, Stack2, setelement(?STORE_PROP_SEQ, Store4, Seq + N)}.

%% §10.1.13 once prototype has been read
new_receiver(Agent, {?HANDLE_TAG, _} = Proto)
  when tuple_size(Agent) =:= ?AGENT_ARITY ->
    case element(?AGENT_STORE, Agent) of
        Store when tuple_size(Store) =:= ?STORE_ARITY ->
            Slot = {?SSHAPED_TAG, 0, {?SOME, Proto}, {}, #{}},
            Id = element(?STORE_NEXT, Store),
            Store2 = setelement(?STORE_DATA, Store,
                                arc_rt_arena_ffi:set(Id, Slot, element(?STORE_DATA, Store))),
            Store3 = setelement(?STORE_NEXT, Store2, Id + 1),
            Store4 = setelement(?STORE_ALLOC, Store3,
                                element(?STORE_ALLOC, Store) + 1),
            {{?HANDLE_TAG, Id}, setelement(?AGENT_STORE, Agent, Store4)};
        _ -> miss
    end;
new_receiver(_, _) -> miss.

-define(WEC(V, Seq), {?DATAPROP_TAG, V, true, true, true, Seq}).
literal_props([], Stack, _) -> {#{}, Stack};
literal_props([K1], [V1 | Stack], Seq) ->
    {#{K1 => ?WEC(V1, Seq)}, Stack};
literal_props([K2, K1], [V2, V1 | Stack], Seq) ->
    {#{K1 => ?WEC(V1, Seq), K2 => ?WEC(V2, Seq + 1)}, Stack};
literal_props(Keys, Stack, Seq) ->
    {Pairs, Stack2} = literal_pairs(Keys, Stack, Seq + length(Keys) - 1, []),
    {maps:from_list(Pairs), Stack2}.

literal_pairs([K | Keys], [V | Stack], Seq, Acc) ->
    literal_pairs(Keys, Stack, Seq - 1, [{K, ?WEC(V, Seq)} | Acc]);
literal_pairs([], Stack, _, Acc) -> {Acc, Stack}.

chain_free(Store, _, {?SOME, {?HANDLE_TAG, PId}}, {?KEY_NAMED, KB})
  when is_map_key(PId, element(?STORE_FREE_PROTOS, Store)),
       byte_size(KB) =/= 9 orelse KB =/= <<"__proto__">> ->
    true;
chain_free(Store, Data, Proto, {?KEY_NAMED, _} = K) ->
    arc_rt_obj_ffi:free_chain(Store, Data, Proto, K);
chain_free(_, Data, Proto, {?KEY_INDEX, Idx}) ->
    index_free(Data, Proto, Idx, 64).

%% creating an element needs free proto chain, writable length
put_elem(Store, {?HANDLE_TAG, Id}, Idx, V)
  when is_integer(Idx), Idx >= 0, tuple_size(Store) =:= ?STORE_ARITY ->
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length} = Kind, Proto, Props, Sym, Elems, true}
          when Props =:= #{}; not is_map_key({?KEY_INDEX, Idx}, Props) ->
            if
                Idx < Length ->
                    NewE = case elem_overwrite(Elems, Idx, V) of
                        hole ->
                            case index_free(Data, Proto, Idx, 64) of
                                true -> elem_write_grow(Elems, Idx, V);
                                false -> miss
                            end;
                        E -> E
                    end,
                    case NewE of
                        miss -> miss;
                        _ ->
                            NewSlot = {?SOBJECT_TAG, Kind, Proto, Props, Sym, NewE, true},
                            setelement(?STORE_DATA, Store,
                                       arc_rt_arena_ffi:set(Id, NewSlot, Data))
                    end;
                Idx =:= Length, Idx =< ?MAX_ARRAY_INDEX ->
                    case length_writable(Props)
                         andalso index_free(Data, Proto, Idx, 64) of
                        false -> miss;
                        true ->
                            case elem_write_grow(Elems, Idx, V) of
                                miss -> miss;
                                NewE ->
                                    NewSlot = {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Idx + 1},
                                               Proto, Props, Sym, NewE, true},
                                    setelement(?STORE_DATA, Store,
                                               arc_rt_arena_ffi:set(Id, NewSlot, Data))
                            end
                    end;
                true -> miss
            end;
        {?SOBJECT_TAG, Kind, _, _, _, _, true} = Slot
          when is_atom(Kind), Idx =< ?MAX_ARRAY_INDEX ->
            put_prop(Store, Data, Id, Slot, {?KEY_INDEX, Idx}, V, true);
        _ -> miss
    end;
put_elem(Store, {?HANDLE_TAG, _} = Obj, Key, V) when is_binary(Key) ->
    case arc_rt_val_ffi:t_to_property_key_fast(Key) of
        {?OKEY_STRING, {?KEY_NAMED, _} = K} -> put_field(Store, Obj, K, V, true);
        {?OKEY_STRING, {?KEY_INDEX, Idx}} -> put_elem(Store, Obj, Idx, V);
        _ -> miss
    end;
put_elem(_, _, _, _) -> miss.

length_writable(#{?LENGTH_KEY := Prop})
  when element(1, Prop) =:= ?DATAPROP_TAG ->
    element(?DATAPROP_WRITABLE, Prop) =:= true;
length_writable(_) -> true.

index_free(_, ?NONE, _, _) -> true;
index_free(_, _, _, 0) -> false;
index_free(Data, {?SOME, {?HANDLE_TAG, P}}, Idx, Fuel) ->
    case arc_rt_arena_ffi:get(P, Data) of
        {?SSHAPED_TAG, _, Proto, _, Offs} ->
            (not is_map_key(integer_to_binary(Idx), Offs))
                andalso index_free(Data, Proto, Idx, Fuel - 1);
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length}, Proto, _, _, _, _}
          when Idx >= Length ->
            index_free(Data, Proto, Idx, Fuel - 1);
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            index_is_plain(element(?SOBJECT_KIND, Slot))
                andalso (not is_map_key({?KEY_INDEX, Idx},
                                        element(?SOBJECT_PROPS, Slot)))
                andalso (not elem_has(element(?SOBJECT_ELEMENTS, Slot), Idx))
                andalso index_free(Data, element(?SOBJECT_PROTO, Slot), Idx,
                                   Fuel - 1);
        _ -> false
    end;
index_free(_, _, _, _) -> false.

index_is_plain(Kind) when is_atom(Kind) -> true;
index_is_plain(Kind) ->
    case element(1, Kind) of
        ?PROXYOBJ_TAG -> false;
        module_namespace -> false;
        typed_array_obj -> false;
        string_obj -> false;
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

%% bounds mirror rt/elements dense promotion policy
-define(MAX_GAP, 1024).
-define(MAX_DENSE_INDEX, 10000000).
elem_write_grow({?ELEMS_DENSE, A}, Idx, V) ->
    case Idx - arc_tree_array_ffi:size(A) =< ?MAX_GAP andalso Idx < ?MAX_DENSE_INDEX of
        true -> {?ELEMS_DENSE, arc_tree_array_ffi:set(Idx, V, A)};
        false -> miss
    end;
elem_write_grow({?ELEMS_SPARSE, M}, Idx, V) -> {?ELEMS_SPARSE, M#{Idx => V}};
elem_write_grow(?ELEMS_NONE, Idx, V) when Idx =< ?MAX_GAP ->
    {?ELEMS_DENSE, arc_tree_array_ffi:set(Idx, V, {})};
elem_write_grow(_, _, _) -> miss.
