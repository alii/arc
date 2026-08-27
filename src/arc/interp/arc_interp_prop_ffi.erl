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
        {?SOBJECT_TAG, ?ORDINARY, Proto, Props, _, _, _} ->
            case Props of
                #{K := Prop} when element(1, Prop) =:= ?DATAPROP_TAG ->
                    element(?DATAPROP_VALUE, Prop);
                #{K := _} -> miss;
                _ ->
                    field_next(Data, element(?STORE_SHAPES, Store), Proto, K,
                               64, Absent)
            end;
        Slot ->
            hop(Data, element(?STORE_SHAPES, Store), Slot, K, 64, Absent)
    end.

hop(Data, Shapes, Slot, K, Fuel, Absent) ->
    case Slot of
        {?SSHAPED_TAG, Sid, Proto, Slots} ->
            case Shapes of
                #{Sid := Desc} ->
                    KeyBin = element(2, K),
                    case element(?SHAPE_OFFSETS, Desc) of
                        #{KeyBin := Off} -> element(Off + 1, Slots);
                        _ -> field_next(Data, Shapes, Proto, K, Fuel, Absent)
                    end;
                _ -> miss
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
                            field_next(Data, Shapes,
                                       element(?SOBJECT_PROTO, Slot),
                                       K, Fuel, Absent)
                    end
            end;
        _ -> miss
    end.

field_next(_, _, ?NONE, _, _, Absent) -> Absent;
field_next(Data, Shapes, {?SOME, {?HANDLE_TAG, P}}, K, Fuel, Absent)
  when Fuel > 1 ->
    hop(Data, Shapes, arc_rt_arena_ffi:get(P, Data), K, Fuel - 1, Absent);
field_next(_, _, _, _, _, _) -> miss.

named_virtual({?ARRAYOBJ_TAG, Length}, ?LENGTH_KEY) -> Length;
named_virtual(_, _) -> miss.

%% false when named keys on this kind are exotic or virtual
-compile({inline, [named_plain/2, named_virtual/2, birth_plain/2, cell_field/4,
                   hop/6, proto_field/3, put_prop/7, chain_free/4,
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
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            Props = element(?SOBJECT_PROPS, Slot),
            case element(?SOBJECT_KIND, Slot) of
                {?ARRAYOBJ_TAG, Length} when Idx < Length ->
                    case index_overridden(Props, Idx) of
                        true -> miss;
                        false -> elem_read(element(?SOBJECT_ELEMENTS, Slot), Idx)
                    end;
                {?ARRAYOBJ_TAG, _} -> miss;
                {?ARGUMENTSOBJ_TAG, _, _} ->
                    case index_overridden(Props, Idx) of
                        true -> miss;
                        false -> elem_read(element(?SOBJECT_ELEMENTS, Slot), Idx)
                    end;
                Kind when Idx =< ?MAX_ARRAY_INDEX ->
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
                                    case index_free(Data, element(?STORE_SHAPES, Store),
                                                    element(?SOBJECT_PROTO, Slot),
                                                    Idx, 64) of
                                        true -> undefined;
                                        false -> miss
                                    end
                            end
                    end;
                _ -> miss
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

-compile({inline, [index_overridden/2]}).
index_overridden(Props, Idx) ->
    map_size(Props) =/= 0 andalso is_map_key({?KEY_INDEX, Idx}, Props).

elem_read({?ELEMS_DENSE, A}, Idx) ->
    case array:get(Idx, A) of
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
        {?SSHAPED_TAG, Sid, P, Slots} ->
            case element(?STORE_SHAPES, Store) of
                #{Sid := Desc} ->
                    KeyBin = element(2, K),
                    case element(?SHAPE_OFFSETS, Desc) of
                        #{KeyBin := Off} ->
                            NewSlot = {?SSHAPED_TAG, Sid, P,
                                       setelement(Off + 1, Slots, V)},
                            setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, NewSlot, Data));
                        _ -> miss
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
            setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, NewSlot, Data));
        #{K := _} -> miss;
        _ when Create, Ext =:= true ->
            case chain_free(Data, element(?STORE_SHAPES, Store), Proto, K) of
                false -> miss;
                true ->
                    Seq = element(?STORE_PROP_SEQ, Store),
                    NewSlot = {?SOBJECT_TAG, Kind, Proto,
                               Props#{K => {?DATAPROP_TAG, V, true, true, true, Seq}},
                               Sym, Elems, Ext},
                    setelement(?STORE_PROP_SEQ,
                               setelement(?STORE_DATA, Store,
                                          arc_rt_arena_ffi:set(Id, NewSlot, Data)),
                               Seq + 1)
            end;
        _ -> miss
    end.

%% §7.3.5 createdataproperty on ordinary extensible object
define_field(Store, {?HANDLE_TAG, Id}, K, V)
  when tuple_size(Store) =:= ?STORE_ARITY ->
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
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
            Slot = {?SOBJECT_TAG, ?ORDINARY, {?SOME, Proto}, #{}, [], ?ELEMS_NONE,
                    true},
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

chain_free(Data, Shapes, Proto, {?KEY_NAMED, _} = K) ->
    arc_rt_obj_ffi:named_free(Data, Shapes, Proto, K, 64);
chain_free(Data, Shapes, Proto, {?KEY_INDEX, Idx}) ->
    index_free(Data, Shapes, Proto, Idx, 64).

%% creating an element needs free proto chain, writable length
put_elem(Store, {?HANDLE_TAG, Id}, Idx, V)
  when is_integer(Idx), Idx >= 0, Idx =< ?MAX_ARRAY_INDEX,
       tuple_size(Store) =:= ?STORE_ARITY ->
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        Slot when tuple_size(Slot) =:= ?SOBJECT_ARITY,
                  element(1, Slot) =:= ?SOBJECT_TAG,
                  element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
            Props = element(?SOBJECT_PROPS, Slot),
            case element(?SOBJECT_KIND, Slot) of
                Kind when is_atom(Kind) ->
                    put_prop(Store, Data, Id, Slot, {?KEY_INDEX, Idx}, V, true);
                _ when map_size(Props) =/= 0
                       andalso is_map_key({?KEY_INDEX, Idx}, Props) -> miss;
                {?ARRAYOBJ_TAG, Length} when Idx < Length ->
                    Elems = element(?SOBJECT_ELEMENTS, Slot),
                    NewE = case elem_overwrite(Elems, Idx, V) of
                        hole ->
                            case index_free(Data, element(?STORE_SHAPES, Store),
                                            element(?SOBJECT_PROTO, Slot), Idx, 64) of
                                true -> elem_write(Elems, Idx, V);
                                false -> miss
                            end;
                        E -> E
                    end,
                    case NewE of
                        miss -> miss;
                        _ ->
                            NewSlot = setelement(?SOBJECT_ELEMENTS, Slot, NewE),
                            setelement(?STORE_DATA, Store,
                                       arc_rt_arena_ffi:set(Id, NewSlot, Data))
                    end;
                {?ARRAYOBJ_TAG, Idx} ->
                    case length_writable(Props)
                         andalso index_free(Data, element(?STORE_SHAPES, Store),
                                            element(?SOBJECT_PROTO, Slot), Idx, 64) of
                        false -> miss;
                        true ->
                            case elem_write_grow(element(?SOBJECT_ELEMENTS, Slot), Idx, V) of
                                miss -> miss;
                                NewE ->
                                    NewSlot = setelement(?SOBJECT_ELEMENTS,
                                        setelement(?SOBJECT_KIND, Slot,
                                                   {?ARRAYOBJ_TAG, Idx + 1}),
                                        NewE),
                                    setelement(?STORE_DATA, Store,
                                               arc_rt_arena_ffi:set(Id, NewSlot, Data))
                            end
                    end;
                _ -> miss
            end;
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

index_free(_, _, ?NONE, _, _) -> true;
index_free(_, _, _, _, 0) -> false;
index_free(Data, Shapes, {?SOME, {?HANDLE_TAG, P}}, Idx, Fuel) ->
    case arc_rt_arena_ffi:get(P, Data) of
        {?SSHAPED_TAG, Sid, Proto, _Slots} ->
            case Shapes of
                #{Sid := Desc} ->
                    (not is_map_key(integer_to_binary(Idx),
                                    element(?SHAPE_OFFSETS, Desc)))
                        andalso index_free(Data, Shapes, Proto, Idx, Fuel - 1);
                _ -> false
            end;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            index_is_plain(element(?SOBJECT_KIND, Slot))
                andalso (not is_map_key({?KEY_INDEX, Idx},
                                        element(?SOBJECT_PROPS, Slot)))
                andalso (not elem_has(element(?SOBJECT_ELEMENTS, Slot), Idx))
                andalso index_free(Data, Shapes, element(?SOBJECT_PROTO, Slot),
                                   Idx, Fuel - 1);
        _ -> false
    end;
index_free(_, _, _, _, _) -> false.

index_is_plain(Kind) when is_atom(Kind) -> true;
index_is_plain(Kind) ->
    case element(1, Kind) of
        ?PROXYOBJ_TAG -> false;
        module_namespace -> false;
        typed_array_obj -> false;
        string_obj -> false;
        _ -> true
    end.

elem_has({?ELEMS_DENSE, A}, Idx) -> array:get(Idx, A) =/= ?ELEMS_HOLE;
elem_has({?ELEMS_SPARSE, M}, Idx) -> is_map_key(Idx, M);
elem_has(_, _) -> false.

elem_overwrite({?ELEMS_DENSE, A}, Idx, V) ->
    case array:get(Idx, A) of
        ?ELEMS_HOLE -> hole;
        _ -> {?ELEMS_DENSE, array:set(Idx, V, A)}
    end;
elem_overwrite({?ELEMS_SPARSE, M}, Idx, V) ->
    case M of
        #{Idx := _} -> {?ELEMS_SPARSE, M#{Idx := V}};
        _ -> hole
    end;
elem_overwrite(_, _, _) -> hole.

elem_write({?ELEMS_DENSE, A}, Idx, V) ->
    case Idx < array:size(A) of
        true -> {?ELEMS_DENSE, array:set(Idx, V, A)};
        false -> miss
    end;
elem_write({?ELEMS_SPARSE, M}, Idx, V) -> {?ELEMS_SPARSE, M#{Idx => V}};
elem_write(_, _, _) -> miss.

%% bounds mirror rt/elements dense promotion policy
-define(MAX_GAP, 1024).
-define(MAX_DENSE_INDEX, 10000000).
elem_write_grow({?ELEMS_DENSE, A}, Idx, V) ->
    case Idx - array:size(A) =< ?MAX_GAP andalso Idx < ?MAX_DENSE_INDEX of
        true -> {?ELEMS_DENSE, array:set(Idx, V, A)};
        false -> miss
    end;
elem_write_grow({?ELEMS_SPARSE, M}, Idx, V) -> {?ELEMS_SPARSE, M#{Idx => V}};
elem_write_grow(?ELEMS_NONE, Idx, V) when Idx =< ?MAX_GAP ->
    {?ELEMS_DENSE, array:set(Idx, V, array:new({default, ?ELEMS_HOLE}))};
elem_write_grow(_, _, _) -> miss.
