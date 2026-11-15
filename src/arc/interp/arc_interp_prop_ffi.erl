%% property kernels for the interpreter; exports may answer miss, never raise
-module(arc_interp_prop_ffi).
-export([has_property/3, has_own/3, private_get/3, private_put/4, private_define/4]).
-export([get_field/3, find_accessor/3, get_elem/3, put_field/5, put_elem/4, define_field/4,
         new_object/4, get_global/3, put_global/6]).

-include("../rt/arc_rt_layout.hrl").
-include("../rt/arc_rt_arena.hrl").

%% §10.1.8.1 ordinary get, miss when anything observable
get_field(St, {?HANDLE_TAG, Id}, K) ->
    object_get(St, Id, K, undefined);
get_field(_, Bin, ?LENGTH_KEY_BY_ID) when is_binary(Bin) ->
    byte_size(Bin);
get_field(_, {?STR_TAG, _, Len, _}, ?LENGTH_KEY_BY_ID) ->
    Len;
get_field(St, S, K) when ?IS_STR(S) ->
    proto_field(St, ?REALM_STRING, K);
get_field(St, N, K) when is_number(N) ->
    proto_field(St, ?REALM_NUMBER, K);
get_field(_, _, _) -> miss.

own_data(Props, K) ->
    case Props of
        #{?KEY_ID(K) := {T, Prop}}
          when ?IS_TEXT_OF(T, K), element(1, Prop) =:= ?DATAPROPERTY_TAG ->
            element(?DATAPROPERTY_VALUE, Prop);
        _ -> miss
    end.

%% §13.10.1 `in` over a plain chain, miss when anything exotic is on it
has_property(St, Key, {?HANDLE_TAG, Id}) ->
    Cells = element(?AGENT_CELLS, St),
    case arc_rt_val_ffi:property_key_of(Key) of
        {?STRINGKEY_TAG, {?KEY_NAMED, _, _} = K} ->
            has_walk(Cells, arc_rt_arena_ffi:get(Id, Cells), K, ?MAX_PROTO_HOPS);
        {?STRINGKEY_TAG, {?KEY_INDEX, Idx}} ->
            case arc_rt_arena_ffi:get(Id, Cells) of
                {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length}, _, _, _, Elems, _}
                  when Idx < Length ->
                    case elem_read(Elems, Idx) of
                        miss -> miss;
                        _ -> true
                    end;
                _ -> miss
            end;
        {?SYMBOLKEY_TAG, Sym} ->
            has_symbol(Cells, arc_rt_arena_ffi:get(Id, Cells), Sym, ?MAX_PROTO_HOPS);
        _ -> miss
    end;
has_property(_, _, _) -> miss.

has_symbol(_, _, _, 0) -> miss;
has_symbol(Cells, {?SSHAPEDOBJECT_TAG, _, Proto, _, _}, Sym, Fuel) ->
    has_symbol_next(Cells, Proto, Sym, Fuel);
has_symbol(_, {?SOBJECT_TAG, {?PROXYOBJ_TAG, _, _, _}, _, _, _, _, _}, _, _) -> miss;
has_symbol(Cells, {?SOBJECT_TAG, _, Proto, _, SymProps, _, _}, Sym, Fuel) ->
    lists:keymember(Sym, 1, SymProps) orelse has_symbol_next(Cells, Proto, Sym, Fuel);
has_symbol(_, _, _, _) -> miss.

has_symbol_next(_, ?NONE, _, _) -> false;
has_symbol_next(Cells, {?SOME, {?HANDLE_TAG, P}}, Sym, Fuel) ->
    has_symbol(Cells, arc_rt_arena_ffi:get(P, Cells), Sym, Fuel - 1).

%% §7.3.12 hasownproperty of a name on an object that keeps it plainly
has_own(St, {?HANDLE_TAG, Id}, Key) when ?IS_STR(Key) ->
    case arc_rt_val_ffi:property_key_of(Key) of
        {?STRINGKEY_TAG, {?KEY_NAMED, _, _} = K} ->
            case arc_rt_arena_ffi:get(Id, element(?AGENT_CELLS, St)) of
                {?SSHAPEDOBJECT_TAG, _, _, _, Offs} ->
                    case Offs of
                        #{?KEY_ID(K) := {SK, _}} when ?SAME_TEXT(SK, K) -> true;
                        #{?KEY_ID(K) := _} -> miss;
                        _ -> false
                    end;
                {?SOBJECT_TAG, Kind, _, Props, _, _, _} ->
                    case named_plain(Kind, K) andalso Props of
                        false -> miss;
                        #{?KEY_ID(K) := {T, _}} when ?IS_TEXT_OF(T, K) -> true;
                        #{?KEY_ID(K) := _} -> miss;
                        _ -> false
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
has_own(_, _, _) -> miss.

has_walk(_, _, _, 0) -> miss;
has_walk(Cells, {?SSHAPEDOBJECT_TAG, _, Proto, _, Offs}, K, Fuel) ->
    case Offs of
        #{?KEY_ID(K) := {SK, _}} when ?SAME_TEXT(SK, K) -> true;
        _ -> has_next(Cells, Proto, K, Fuel)
    end;
has_walk(Cells, Cell, K, Fuel) when element(1, Cell) =:= ?SOBJECT_TAG ->
    case named_plain(element(?SOBJECT_KIND, Cell), K) of
        false -> miss;
        true ->
            case element(?SOBJECT_PROPS, Cell) of
                #{?KEY_ID(K) := {T, _}} when ?IS_TEXT_OF(T, K) -> true;
                #{?KEY_ID(K) := _} -> miss;
                _ -> has_next(Cells, element(?SOBJECT_PROTO, Cell), K, Fuel)
            end
    end;
has_walk(_, _, _, _) -> miss.

has_next(_, ?NONE, _, _) -> false;
has_next(Cells, {?SOME, {?HANDLE_TAG, P}}, K, Fuel) ->
    has_walk(Cells, arc_rt_arena_ffi:get(P, Cells), K, Fuel - 1).

%% §7.3.30 privateget of a field or method; accessors and absence miss
private_get(St, {?HANDLE_TAG, Id}, Name) ->
    case arc_rt_arena_ffi:get(Id, element(?AGENT_CELLS, St)) of
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case private_key(Name) of
                miss -> miss;
                K -> own_data(element(?SOBJECT_PROPS, Cell), K)
            end;
        _ -> miss
    end;
private_get(_, _, _) -> miss.

%% §7.3.31 privateset of a field; methods, accessors and absence miss
private_put(St, {?HANDLE_TAG, Id}, Name, V) ->
    Cells = element(?AGENT_CELLS, St),
    case arc_rt_arena_ffi:get(Id, Cells) of
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case private_key(Name) of
                miss -> miss;
                K ->
                    put_prop(St, Cells, Id, Cell, K, ?KEY_ID(K), ?KEY_TEXT(K), V, false)
            end;
        _ -> miss
    end;
private_put(_, _, _, _) -> miss.

%% §7.3.28 privatefieldadd to an extensible object that lacks the name
private_define(St, {?HANDLE_TAG, Id}, Name, V) when tuple_size(St) =:= ?AGENT_SIZE ->
    Cells = element(?AGENT_CELLS, St),
    case private_key(Name) of
        miss -> miss;
        K ->
            Seq = element(?AGENT_PROP_SEQ, St),
            Prop = {?DATAPROPERTY_TAG, V, true, false, true, Seq},
            case arc_rt_arena_ffi:get(Id, Cells) of
                {?SOBJECT_TAG, Kind, Proto, Props, Sym, Elems, true}
                  when not is_map_key(?KEY_ID(K), Props) ->
                    Cell = {?SOBJECT_TAG, Kind, Proto,
                            Props#{?KEY_ID(K) => ?NAMED_ENTRY(K, Prop)}, Sym, Elems, true},
                    setelement(?AGENT_PROP_SEQ, set_plain(St, Id, Cell, Cells, Kind),
                               Seq + 1);
                {?SSHAPEDOBJECT_TAG, _, Proto, {}, _} ->
                    Cell = {?SOBJECT_TAG, ?ORDINARY, Proto,
                            #{?KEY_ID(K) => ?NAMED_ENTRY(K, Prop)}, [], ?ELEMS_NONE, true},
                    setelement(?AGENT_PROP_SEQ,
                               setelement(?AGENT_CELLS, St, arc_rt_arena_ffi:set(Id, Cell, Cells)),
                               Seq + 1);
                _ -> miss
            end
    end;
private_define(_, _, _, _) -> miss.

private_key({?KEY_PRIVATE, _, _} = K) -> K;
private_key(_) -> miss.

%% accessor a plain chain lookup of K finds, else no_accessor
find_accessor(St, {?HANDLE_TAG, Id}, K) ->
    Cells = element(?AGENT_CELLS, St),
    accessor_walk(Cells, arc_rt_arena_ffi:get(Id, Cells), K, ?MAX_PROTO_HOPS);
find_accessor(_, _, _) -> no_accessor.

accessor_walk(_, _, _, 0) -> no_accessor;
accessor_walk(Cells, {?SSHAPEDOBJECT_TAG, _, Proto, _, Offs}, K, Fuel) ->
    case Offs of
        #{?KEY_ID(K) := {SK, _}} when ?SAME_TEXT(SK, K) -> no_accessor;
        _ -> accessor_next(Cells, Proto, K, Fuel)
    end;
accessor_walk(Cells, Cell, K, Fuel) when element(1, Cell) =:= ?SOBJECT_TAG ->
    case named_plain(element(?SOBJECT_KIND, Cell), K) of
        false -> no_accessor;
        true ->
            case element(?SOBJECT_PROPS, Cell) of
                #{?KEY_ID(K) := {T, Prop}}
                  when ?IS_TEXT_OF(T, K), element(1, Prop) =:= ?ACCESSORPROPERTY_TAG ->
                    {accessor, element(?ACCESSORPROPERTY_GET, Prop),
                     element(?ACCESSORPROPERTY_SET, Prop)};
                #{?KEY_ID(K) := _} -> no_accessor;
                _ -> accessor_next(Cells, element(?SOBJECT_PROTO, Cell), K, Fuel)
            end
    end;
accessor_walk(_, _, _, _) -> no_accessor.

accessor_next(Cells, {?SOME, {?HANDLE_TAG, P}}, K, Fuel) ->
    accessor_walk(Cells, arc_rt_arena_ffi:get(P, Cells), K, Fuel - 1);
accessor_next(_, _, _, _) -> no_accessor.

%% §9.1.1.4.6 global getbindingvalue, plain case
get_global(St, Lex, K) ->
    case Lex of
        #{?KEY_ID(K) := {T, Binding}} when ?IS_TEXT_OF(T, K) ->
            case element(?LEXICAL_GLOBAL_VALUE, Binding) of
                js_tdz -> miss;
                V -> V
            end;
        #{?KEY_ID(K) := _} -> miss;
        _ ->
            {?HANDLE_TAG, G} = element(?REALM_GLOBAL_OBJECT, element(?AGENT_REALM, St)),
            object_get(St, G, K, miss)
    end.

%% §9.1.1.4.5 setmutablebinding on the global object
put_global(St, Lex, Global, K, V, Strict) ->
    case is_map_key(?KEY_ID(K), Lex) of
        true -> miss;
        false -> put_field(St, Global, K, V, not Strict)
    end.

%% getters miss so the general path passes the primitive as this
proto_field(St, Which, K) ->
    Pair = element(Which, element(?AGENT_REALM, St)),
    {?HANDLE_TAG, Id} = element(?BUILTINPAIR_PROTOTYPE, Pair),
    object_get(St, Id, K, undefined).

object_get(St, Id, K, Absent) ->
    Cells = element(?AGENT_CELLS, St),
    case arena_get(Id, Cells) of
        {?SSHAPEDOBJECT_TAG, _, Proto, Slots, Offs} ->
            case Offs of
                #{?KEY_ID(K) := {SK, Off}} when ?SAME_TEXT(SK, K) -> ?SLOT_AT(Slots, Off);
                _ -> proto_get(Cells, Proto, K, ?MAX_PROTO_HOPS, Absent)
            end;
        {?SOBJECT_TAG, ?ORDINARY, Proto, Props, _, _, _} ->
            case Props of
                #{?KEY_ID(K) := {T, Prop}}
                  when ?IS_TEXT_OF(T, K), element(1, Prop) =:= ?DATAPROPERTY_TAG ->
                    element(?DATAPROPERTY_VALUE, Prop);
                #{?KEY_ID(K) := _} -> miss;
                _ -> proto_get(Cells, Proto, K, ?MAX_PROTO_HOPS, Absent)
            end;
        Cell -> chain_get(Cells, Cell, K, ?MAX_PROTO_HOPS, Absent)
    end.

chain_get(Cells, Cell, K, Fuel, Absent) ->
    case Cell of
        {?SSHAPEDOBJECT_TAG, _, Proto, Slots, Offs} ->
            case Offs of
                #{?KEY_ID(K) := {SK, Off}} when ?SAME_TEXT(SK, K) -> ?SLOT_AT(Slots, Off);
                _ -> proto_get(Cells, Proto, K, Fuel, Absent)
            end;
        _ when element(1, Cell) =:= ?SOBJECT_TAG ->
            Kind = element(?SOBJECT_KIND, Cell),
            case named_plain(Kind, K) of
                false -> named_virtual(Kind, K);
                true ->
                    case element(?SOBJECT_PROPS, Cell) of
                        #{?KEY_ID(K) := {T, Prop}}
                          when ?IS_TEXT_OF(T, K),
                               element(1, Prop) =:= ?DATAPROPERTY_TAG ->
                            element(?DATAPROPERTY_VALUE, Prop);
                        #{?KEY_ID(K) := _} -> miss;
                        _ ->
                            proto_get(Cells, element(?SOBJECT_PROTO, Cell), K,
                                       Fuel, Absent)
                    end
            end;
        _ -> miss
    end.

proto_get(_, ?NONE, _, _, Absent) -> Absent;
proto_get(Cells, {?SOME, {?HANDLE_TAG, P}}, K, Fuel, Absent) when Fuel > 1 ->
    chain_get(Cells, arena_get(P, Cells), K, Fuel - 1, Absent);
proto_get(_, _, _, _, _) -> miss.

named_virtual({?ARRAYOBJ_TAG, Length}, ?LENGTH_KEY_BY_ID) -> Length;
named_virtual(_, _) -> miss.

-compile({inline, [named_plain/2, named_virtual/2, birth_plain/2, object_get/4,
                   chain_get/5, proto_field/3, put_prop/9, put_new/7, set_plain/5,
                   shaped_grow/7, shaped_next/3, chain_takes_write/4,
                   literal_props/3]}).
named_plain(Kind, K) -> ?NAMED_KEY_IS_PLAIN(Kind, K).

birth_plain(Birth, K) ->
    ?LAZY_KEY_IS_PLAIN(Birth, K).

%% holes miss so the full path walks the proto chain
get_elem(St, {?HANDLE_TAG, Id}, Idx) when is_integer(Idx), Idx >= 0 ->
    Cells = element(?AGENT_CELLS, St),
    case arena_get(Id, Cells) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length}, _, Props, _, Elems, _} ->
            if
                Idx >= Length -> miss;
                Props =:= #{} -> elem_read(Elems, Idx);
                is_map_key(?INDEX_SLOT(Idx), Props) -> miss;
                true -> elem_read(Elems, Idx)
            end;
        {?SOBJECT_TAG, {?ARGUMENTSOBJ_TAG, _, _}, _, Props, _, Elems, _} ->
            case is_map_key(?INDEX_SLOT(Idx), Props) of
                true -> miss;
                false -> elem_read(Elems, Idx)
            end;
        %% §10.4.5.4 an index out of the view reads undefined, no proto walk
        {?SOBJECT_TAG, {?TYPEDARRAYOBJ_TAG, {?HANDLE_TAG, Buf}, ElemKind, Offset, Len},
         _, _, _, _, _} when Idx =< ?MAX_ARRAY_INDEX ->
            case arena_get(Buf, Cells) of
                {?SOBJECT_TAG, {?ARRAYBUFFEROBJ_TAG, Storage}, _, _, _, _, _} ->
                    case 'arc@rt@buffer':element_of_storage(Storage, ElemKind, Offset,
                                                            Len, Idx) of
                        {?SOME, V} -> V;
                        ?NONE -> undefined
                    end;
                _ -> miss
            end;
        {?SOBJECT_TAG, Kind, Proto, Props, _, _, _} when Idx =< ?MAX_ARRAY_INDEX ->
            case index_kind_is_plain(Kind) of
                false -> miss;
                true ->
                    case Props of
                        #{?INDEX_SLOT(Idx) := {_, Prop}} ->
                            case element(1, Prop) of
                                ?DATAPROPERTY_TAG -> element(?DATAPROPERTY_VALUE, Prop);
                                _ -> miss
                            end;
                        _ ->
                            case chain_lacks_index(Cells, Proto, Idx,
                                                   ?MAX_PROTO_HOPS) of
                                true -> undefined;
                                false -> miss
                            end
                    end
            end;
        _ -> miss
    end;
get_elem(_, S, Idx) when is_integer(Idx), ?IS_STR(S) ->
    case arc_rt_js_string_ffi:char_at(S, Idx) of
        {?SOME, Ch} -> Ch;
        ?NONE -> miss
    end;
get_elem(St, {?HANDLE_TAG, _} = Obj, Key) when ?IS_STR(Key) ->
    case arc_rt_val_ffi:property_key_of(Key) of
        {?STRINGKEY_TAG, {?KEY_NAMED, _, _} = K} ->
            object_get(St, element(?HANDLE_ID, Obj), K, undefined);
        {?STRINGKEY_TAG, {?KEY_INDEX, Idx}} -> get_elem(St, Obj, Idx);
        _ -> miss
    end;
get_elem(St, {?HANDLE_TAG, Id}, {js_sym, Sym}) ->
    Cells = element(?AGENT_CELLS, St),
    symbol_get(Cells, arc_rt_arena_ffi:get(Id, Cells), Sym, ?MAX_PROTO_HOPS);
get_elem(_, _, _) -> miss.

%% only a proxy treats a symbol key specially; accessors miss
symbol_get(_, _, _, 0) -> miss;
symbol_get(Cells, {?SSHAPEDOBJECT_TAG, _, Proto, _, _}, Sym, Fuel) ->
    symbol_next(Cells, Proto, Sym, Fuel);
symbol_get(_, {?SOBJECT_TAG, {?PROXYOBJ_TAG, _, _, _}, _, _, _, _, _}, _, _) -> miss;
symbol_get(Cells, {?SOBJECT_TAG, _, Proto, _, SymProps, _, _}, Sym, Fuel) ->
    case lists:keyfind(Sym, 1, SymProps) of
        false -> symbol_next(Cells, Proto, Sym, Fuel);
        {_, Prop} when element(1, Prop) =:= ?DATAPROPERTY_TAG ->
            element(?DATAPROPERTY_VALUE, Prop);
        _ -> miss
    end;
symbol_get(_, _, _, _) -> miss.

symbol_next(_, ?NONE, _, _) -> undefined;
symbol_next(Cells, {?SOME, {?HANDLE_TAG, P}}, Sym, Fuel) ->
    symbol_get(Cells, arc_rt_arena_ffi:get(P, Cells), Sym, Fuel - 1).

-compile({inline, [elem_read/2, elem_overwrite/3]}).
elem_read({?ELEMS_DENSE, {?VEC_TAG, _, _, _, _, _} = A}, Idx) ->
    case arc_tree_array_ffi:get_or_hole(Idx, A) of
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
put_field(St, {?HANDLE_TAG, Id}, K, V, Create)
  when tuple_size(St) =:= ?AGENT_SIZE ->
    Cells = element(?AGENT_CELLS, St),
    case arena_get(Id, Cells) of
        {?SSHAPEDOBJECT_TAG, Sid, P, Slots, Offs} = Cell ->
            case Offs of
                #{?KEY_ID(K) := {SK, Off}} when ?SAME_TEXT(SK, K) ->
                    NewCell = setelement(?SSHAPEDOBJECT_SLOTS, Cell,
                                         ?SLOT_SET(Slots, Off, V)),
                    setelement(?AGENT_CELLS, St, arc_rt_arena_ffi:set(Id, NewCell, Cells));
                _ when Create ->
                    case shaped_next(St, Sid, K) of
                        miss -> miss;
                        Next ->
                            case chain_takes_write(St, Cells, P, K) of
                                false -> miss;
                                true ->
                                    shaped_grow(St, Cells, Id, Next, P, Slots, V);
                                {true, St1} ->
                                    shaped_grow(St1, Cells, Id, Next, P, Slots, V)
                            end
                    end;
                _ -> miss
            end;
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Cell), K) of
                false -> miss;
                true ->
                    put_prop(St, Cells, Id, Cell, K, ?KEY_ID(K), ?KEY_TEXT(K), V, Create)
            end;
        _ -> miss
    end;
put_field(_, _, _, _, _) -> miss.

%% Slot and Text come from K, passed apart because an index key holds neither
put_prop(St, Cells, Id, Cell, K, Slot, Text, V, Create) ->
    {_, Kind, Proto, Props, Sym, Elems, Ext} = Cell,
    case Props of
        #{Slot := {T, {?DATAPROPERTY_TAG, _, true, E, C, Sq}}}
          when ?ID_IS_EXACT(Slot) orelse T =:= Text ->
            NewCell = {?SOBJECT_TAG, Kind, Proto,
                       Props#{Slot := {Text, {?DATAPROPERTY_TAG, V, true, E, C, Sq}}},
                       Sym, Elems, Ext},
            set_plain(St, Id, NewCell, Cells, Kind);
        #{Slot := _} -> miss;
        _ when Create, Ext =:= true ->
            case chain_takes_write(St, Cells, Proto, K) of
                false -> miss;
                true -> put_new(St, Cells, Id, Cell, Slot, Text, V);
                {true, St1} -> put_new(St1, Cells, Id, Cell, Slot, Text, V)
            end;
        _ -> miss
    end.

shaped_next(St, Sid, K) ->
    Step = ?SHAPE_STEP(K),
    ?SHAPED_NEXT(?STORED(?STORE_SHAPES, St), Sid, Step).

shaped_grow(St, Cells, Id, {To, ToOffs}, P, Slots, V)
  when tuple_size(St) =:= ?AGENT_SIZE ->
    NewCell = {?SSHAPEDOBJECT_TAG, To, P, erlang:append_element(Slots, V), ToOffs},
    setelement(?AGENT_CELLS, St, arc_rt_arena_ffi:set(Id, NewCell, Cells)).

put_new(St, Cells, Id, Cell, Slot, Text, V) when tuple_size(St) =:= ?AGENT_SIZE ->
    {_, Kind, Proto, Props, Sym, Elems, Ext} = Cell,
    Seq = element(?AGENT_PROP_SEQ, St),
    NewCell = {?SOBJECT_TAG, Kind, Proto,
               Props#{Slot => {Text, ?PLAIN_PROPERTY(V, Seq)}},
               Sym, Elems, Ext},
    setelement(?AGENT_PROP_SEQ, set_plain(St, Id, NewCell, Cells, Kind), Seq + 1).

%% global object readers watch the epoch
set_plain(St, Id, Cell, Cells, ?GLOBALOBJ) when tuple_size(St) =:= ?AGENT_SIZE ->
    ?GLOBAL_WRITTEN(setelement(?AGENT_CELLS, St, arc_rt_arena_ffi:set(Id, Cell, Cells)));
set_plain(St, Id, Cell, Cells, _) when tuple_size(St) =:= ?AGENT_SIZE ->
    setelement(?AGENT_CELLS, St, arc_rt_arena_ffi:set(Id, Cell, Cells)).

%% §7.3.5 createdataproperty on ordinary extensible object
define_field(St, {?HANDLE_TAG, Id}, K, V)
  when tuple_size(St) =:= ?AGENT_SIZE ->
    Cells = element(?AGENT_CELLS, St),
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SSHAPEDOBJECT_TAG, Sid, P, Slots, Offs} = Cell
          when element(1, K) =:= ?KEY_NAMED ->
            case Offs of
                #{?KEY_ID(K) := {SK, Off}} when ?SAME_TEXT(SK, K) ->
                    NewCell = setelement(?SSHAPEDOBJECT_SLOTS, Cell,
                                         ?SLOT_SET(Slots, Off, V)),
                    setelement(?AGENT_CELLS, St, arc_rt_arena_ffi:set(Id, NewCell, Cells));
                _ ->
                    case shaped_next(St, Sid, K) of
                        miss -> miss;
                        Next -> shaped_grow(St, Cells, Id, Next, P, Slots, V)
                    end
            end;
        {?SOBJECT_TAG, ?ORDINARY, Proto, Props, Sym, Elems, true}
          when element(1, K) =:= ?KEY_NAMED ->
            case Props of
                #{?KEY_ID(K) := {T, {?DATAPROPERTY_TAG, _, _, _, true, Sq}}}
                  when ?IS_TEXT_OF(T, K) ->
                    NewCell = {?SOBJECT_TAG, ?ORDINARY, Proto,
                               Props#{?KEY_ID(K) :=
                                          {T, {?DATAPROPERTY_TAG, V, true, true, true, Sq}}},
                               Sym, Elems, true},
                    setelement(?AGENT_CELLS, St, arc_rt_arena_ffi:set(Id, NewCell, Cells));
                #{?KEY_ID(K) := _} -> miss;
                _ ->
                    Seq = element(?AGENT_PROP_SEQ, St),
                    NewCell = {?SOBJECT_TAG, ?ORDINARY, Proto,
                               Props#{?KEY_ID(K) =>
                                          ?NAMED_ENTRY(K, ?PLAIN_PROPERTY(V, Seq))},
                               Sym, Elems, true},
                    setelement(?AGENT_PROP_SEQ,
                               setelement(?AGENT_CELLS, St, arc_rt_arena_ffi:set(Id, NewCell, Cells)),
                               Seq + 1)
            end;
        _ -> miss
    end;
define_field(_, _, _, _) -> miss.

%% keys given last first, values on top of stack
new_object(St, Keys, N, Stack) when tuple_size(St) =:= ?AGENT_SIZE ->
    Seq = element(?AGENT_PROP_SEQ, St),
    {Props, Stack2} = literal_props(Keys, Stack, Seq),
    Proto = ?REALM_SHARES(?REALMSHARED_OBJECT_PROTO, element(?AGENT_REALM, St)),
    Cell = {?SOBJECT_TAG, ?ORDINARY, Proto, Props, [], ?ELEMS_NONE, true},
    Id = element(?AGENT_NEXT_ID, St),
    St2 = ?ALLOC_CELL(St, element(?AGENT_CELLS, St), Id, Cell),
    {{?HANDLE_TAG, Id}, Stack2, setelement(?AGENT_PROP_SEQ, St2, Seq + N)}.

%% the compiler only groups names whose ids differ
literal_props([K1], [V1 | Stack], Seq) ->
    {#{?KEY_ID(K1) => ?NAMED_ENTRY(K1, ?PLAIN_PROPERTY(V1, Seq))}, Stack};
literal_props([K2, K1], [V2, V1 | Stack], Seq) ->
    {#{?KEY_ID(K1) => ?NAMED_ENTRY(K1, ?PLAIN_PROPERTY(V1, Seq)),
       ?KEY_ID(K2) => ?NAMED_ENTRY(K2, ?PLAIN_PROPERTY(V2, Seq + 1))}, Stack};
literal_props(Keys, Stack, Seq) ->
    {Pairs, Stack2} = literal_pairs(Keys, Stack, Seq + length(Keys) - 1, []),
    {maps:from_list(Pairs), Stack2}.

literal_pairs([K | Keys], [V | Stack], Seq, Acc) ->
    literal_pairs(Keys, Stack, Seq - 1,
                  [{?KEY_ID(K), ?NAMED_ENTRY(K, ?PLAIN_PROPERTY(V, Seq))} | Acc]);
literal_pairs([], Stack, _, Acc) -> {Acc, Stack}.

chain_takes_write(St, Cells, Proto, {?KEY_NAMED, _, _} = K) ->
    arc_rt_obj_ffi:chain_takes_named_write(St, Cells, Proto, K);
chain_takes_write(St, Cells, Proto, {?KEY_INDEX, Idx}) ->
    arc_rt_obj_ffi:chain_takes_index_write(St, Cells, Proto, Idx).

%% creating an element needs a free proto chain and a writable length
put_elem(St, {?HANDLE_TAG, Id}, Idx, V)
  when is_integer(Idx), Idx >= 0, tuple_size(St) =:= ?AGENT_SIZE ->
    Cells = element(?AGENT_CELLS, St),
    case arena_get(Id, Cells) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length} = Kind, Proto, Props, Sym, Elems, true} = Cell
          when Props =:= #{}; not is_map_key(?INDEX_SLOT(Idx), Props) ->
            if
                Idx < Length ->
                    case elem_overwrite(Elems, Idx, V) of
                        hole ->
                            fill_hole(St, Cells, Id, Cell, Proto, Elems, Idx, V);
                        NewE ->
                            NewCell = {?SOBJECT_TAG, Kind, Proto, Props, Sym, NewE, true},
                            setelement(?AGENT_CELLS, St, arc_rt_arena_ffi:set(Id, NewCell, Cells))
                    end;
                Idx =:= Length, Idx =< ?MAX_ARRAY_INDEX ->
                    case length_writable(Props)
                         andalso arc_rt_obj_ffi:chain_takes_index_write(
                                   St, Cells, Proto, Idx) of
                        false -> miss;
                        Free ->
                            case elem_write_grow(Elems, Idx, V) of
                                miss -> miss;
                                NewE ->
                                    NewCell = {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Idx + 1},
                                               Proto, Props, Sym, NewE, true},
                                    St1 = setelement(?AGENT_CELLS,
                                                     arc_rt_obj_ffi:remembering(Free, St),
                                                     arc_rt_arena_ffi:set(Id, NewCell, Cells)),
                                    ?GROWN(St1, Idx, Idx + 1)
                            end
                    end;
                true -> miss
            end;
        {?SOBJECT_TAG, Kind, _, _, _, _, true} = Cell
          when is_atom(Kind), Idx =< ?MAX_ARRAY_INDEX ->
            put_prop(St, Cells, Id, Cell, {?KEY_INDEX, Idx}, ?INDEX_SLOT(Idx), <<>>, V,
                     true);
        _ -> miss
    end;
put_elem(St, {?HANDLE_TAG, Id} = Obj, Key, V) when ?IS_STR(Key) ->
    case arc_rt_val_ffi:property_key_of(Key) of
        {?STRINGKEY_TAG, {?KEY_NAMED, _, _} = K} ->
            case put_field(St, Obj, K, V, true) of
                miss -> put_first_key(St, Id, K, V);
                St1 -> St1
            end;
        {?STRINGKEY_TAG, {?KEY_INDEX, Idx}} -> put_elem(St, Obj, Idx, V);
        _ -> miss
    end;
put_elem(_, _, _, _) -> miss.

%% an empty `{}` given its first name through a computed key is a dictionary
put_first_key(St, Id, K, V) when tuple_size(St) =:= ?AGENT_SIZE ->
    Cells = element(?AGENT_CELLS, St),
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SSHAPEDOBJECT_TAG, ?PLAIN_EMPTY_SHAPE, Proto, {}, _} ->
            case chain_takes_write(St, Cells, Proto, K) of
                false -> miss;
                true -> put_first_prop(St, Cells, Id, Proto, K, V);
                {true, St1} -> put_first_prop(St1, Cells, Id, Proto, K, V)
            end;
        _ -> miss
    end.

put_first_prop(St, Cells, Id, Proto, K, V) when tuple_size(St) =:= ?AGENT_SIZE ->
    Seq = element(?AGENT_PROP_SEQ, St),
    Cell = {?SOBJECT_TAG, ?ORDINARY, Proto,
            #{?KEY_ID(K) => ?NAMED_ENTRY(K, ?PLAIN_PROPERTY(V, Seq))}, [], ?ELEMS_NONE, true},
    setelement(?AGENT_PROP_SEQ,
               setelement(?AGENT_CELLS, St, arc_rt_arena_ffi:set(Id, Cell, Cells)),
               Seq + 1).

%% a hole is a missing property, so the protos get a say
fill_hole(St, Cells, Id, Cell, Proto, Elems, Idx, V) ->
    case arc_rt_obj_ffi:chain_takes_index_write(St, Cells, Proto, Idx) of
        false -> miss;
        Free ->
            case elem_write_grow(Elems, Idx, V) of
                miss -> miss;
                NewE ->
                    setelement(?AGENT_CELLS, arc_rt_obj_ffi:remembering(Free, St),
                               arc_rt_arena_ffi:set(
                                 Id, setelement(?SOBJECT_ELEMENTS, Cell, NewE), Cells))
            end
    end.

length_writable(#{?KEY_ID(?LENGTH_KEY) := {_, Prop}})
  when element(1, Prop) =:= ?DATAPROPERTY_TAG ->
    element(?DATAPROPERTY_WRITABLE, Prop) =:= true;
%% some other name sits where length would be, so let the general path look
length_writable(#{?KEY_ID(?LENGTH_KEY) := _}) -> false;
length_writable(_) -> true.

chain_lacks_index(_, ?NONE, _, _) -> true;
chain_lacks_index(_, _, _, 0) -> false;
chain_lacks_index(Cells, {?SOME, {?HANDLE_TAG, P}}, Idx, Fuel) ->
    case arc_rt_arena_ffi:get(P, Cells) of
        %% a shape holds names only, and no name spells an index
        {?SSHAPEDOBJECT_TAG, _, Proto, _, _} ->
            chain_lacks_index(Cells, Proto, Idx, Fuel - 1);
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length}, Proto, _, _, _, _}
          when Idx >= Length ->
            chain_lacks_index(Cells, Proto, Idx, Fuel - 1);
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            index_kind_is_plain(element(?SOBJECT_KIND, Cell))
                andalso (not is_map_key(?INDEX_SLOT(Idx),
                                        element(?SOBJECT_PROPS, Cell)))
                andalso (not elem_has(element(?SOBJECT_ELEMENTS, Cell), Idx))
                andalso chain_lacks_index(Cells, element(?SOBJECT_PROTO, Cell),
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

elem_has({?ELEMS_DENSE, A}, Idx) -> arc_tree_array_ffi:get_or_hole(Idx, A) =/= ?ELEMS_HOLE;
elem_has({?ELEMS_SPARSE, M}, Idx) -> is_map_key(Idx, M);
elem_has(_, _) -> false.

elem_overwrite({?ELEMS_DENSE, A}, Idx, V) ->
    case arc_tree_array_ffi:overwrite(Idx, V, A) of
        hole -> hole;
        A1 -> {?ELEMS_DENSE, A1}
    end;
elem_overwrite({?ELEMS_SPARSE, M}, Idx, V) ->
    case M of
        #{Idx := _} -> {?ELEMS_SPARSE, M#{Idx := V}};
        _ -> hole
    end;
elem_overwrite(_, _, _) -> hole.

elem_write_grow(Els, Idx, V) -> ?ELEM_WRITE_GROW(Els, Idx, V).
