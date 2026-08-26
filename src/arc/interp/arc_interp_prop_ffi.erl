%%% arc_interp_prop_ffi — the interpreter's fused property, element and
%%% global access kernels (`.x`, `.x = v`, `a[i]`, `a[i] = v`, field define,
%%% global read/write). Same contract as arc_interp_ffi: match the raw JsVal
%%% wire term and the store records directly, answer the hit value (reads) or
%%% the rebuilt agent (writes), or the atom `miss` when the operands need
%%% anything observable. TOTAL: no clause raises for any wire term. Shares
%%% the proto-chain predicate `named_free` and `store_put_seq` with the AOT
%%% fast paths in arc_rt_obj_ffi.
-module(arc_interp_prop_ffi).
-export([get_field/3, get_elem/3, get_elem2/3, put_field/4, put_elem/4,
         define_field/4, get_global/3, put_global/6]).

-include("../rt/arc_rt_layout.hrl").

%% The Named "length" PropertyKey term.
-define(LENGTH_KEY, {?KEY_NAMED, <<"length">>}).

%% get_field(Agent, V, K) -> JsVal | miss
%% §10.1.8.1 OrdinaryGet for a Named string key on an object cell, walking
%% the prototype chain while every hop is an ordinary read: an own slot on
%% an SShapedObject, or an own DataProperty in an SObject's props map for a
%% kind whose named keys are not virtual. A string or number primitive
%% starts the walk at its realm wrapper prototype (String "length" is
%% answered directly, as is an Array cell's). Accessors, Proxy / module
%% namespace / TypedArray cells, a String object's "length", a dangling
%% handle, any other primitive, or more than 64 hops all miss. Absent on
%% the whole chain is `undefined`, exactly as OrdinaryGet answers.
%% K is the canonical `{named, KeyBin}` PropertyKey term the opcode carries
%% (the compiler emits Index keys for array-index strings), used as the
%% props-map key as is so no hop rebuilds it.
get_field(Agent, {?HANDLE_TAG, Id}, K) ->
    cell_field(element(?AGENT_STORE, Agent), Id, K);
get_field(_, Bin, ?LENGTH_KEY) when is_binary(Bin) ->
    arc_string_ffi:string_codepoint_length(Bin);
get_field(Agent, Bin, K) when is_binary(Bin) ->
    proto_field(Agent, ?REALM_STRING, K);
get_field(Agent, N, K) when is_number(N) ->
    proto_field(Agent, ?REALM_NUMBER, K);
get_field(_, _, _) -> miss.

%% get_global(Agent, Lex, NameBin) -> JsVal | miss
%% §9.1.1.4.6 GetBindingValue on the global Environment Record for the
%% plain case: an initialised lexical (let/const/class) binding from Lex
%% (the realm's `lexical_globals` map of {let|const, V}), else an own or
%% inherited plain data property of the global object, walked as get_field
%% walks. A binding in its TDZ, an accessor, an exotic hop, or a name absent
%% everywhere (ReferenceError, or "undefined" for typeof) miss.
get_global(Agent, Lex, Name) ->
    case Lex of
        #{Name := Binding} ->
            case element(2, Binding) of
                js_tdz -> miss;
                V -> V
            end;
        _ ->
            {?HANDLE_TAG, G} = element(?REALM_GLOBAL, element(?AGENT_REALM, Agent)),
            Store = element(?AGENT_STORE, Agent),
            field_walk(element(?STORE_DATA, Store), element(?STORE_SHAPES, Store),
                       G, {?KEY_NAMED, Name}, 64, miss)
    end.

%% put_global(Store, Lex, Global, NameBin, V, Strict) -> Store2 | miss
%% §9.1.1.4.5 SetMutableBinding, object-record half, as put_field on the
%% global object: an existing own writable data property is replaced; a
%% sloppy frame may also create it (strict must see ReferenceError). A
%% lexical binding of the name, or anything put_field misses on, miss.
put_global(Store, Lex, Global, Name, V, Strict) ->
    case is_map_key(Name, Lex) of
        true -> miss;
        false -> put_field(Store, Global, {?KEY_NAMED, Name}, V, not Strict)
    end.

%% A string / number primitive has no own named props besides String
%% "length", so a read walks the realm's wrapper prototype. Only a data
%% property answers here; a getter misses so the slow path can pass the
%% primitive as `this`.
proto_field(Agent, Which, K) ->
    Pair = element(Which, element(?AGENT_REALM, Agent)),
    {?HANDLE_TAG, Id} = element(?PAIR_PROTO, Pair),
    cell_field(element(?AGENT_STORE, Agent), Id, K).

cell_field(Store, Id, K) ->
    field_walk(element(?STORE_DATA, Store), element(?STORE_SHAPES, Store),
               Id, K, 64, undefined).

%% Absent is the answer when the whole chain lacks the key: `undefined` for
%% OrdinaryGet, `miss` for a global binding lookup.
field_walk(_, _, _, _, 0, _) -> miss;
field_walk(Data, Shapes, Id, K, Fuel, Absent) ->
    case array:get(Id, Data) of
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
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
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
field_next(Data, Shapes, {?SOME, {?HANDLE_TAG, P}}, K, Fuel, Absent) ->
    field_walk(Data, Shapes, P, K, Fuel - 1, Absent);
field_next(_, _, _, _, _, _) -> miss.

%% The one virtual named data property a read kernel synthesizes: an Array
%% cell's "length" IS its kind payload (§10.4.2, always an own data
%% property, so no chain walk). Every other non-plain named read misses.
named_virtual({?ARRAYOBJ_TAG, Length}, ?LENGTH_KEY) -> Length;
named_virtual(_, _) -> miss.

%% Whether a Named key on this ObjKind is a plain props-map entry for both
%% [[Get]] and [[Set]] (rt/obj own_property_of, get_from, set arms): Proxy,
%% module namespace and TypedArray cells are exotic for string keys, and
%% Array / String objects synthesize "length".
-compile({inline, [named_plain/2, named_virtual/2]}).
named_plain(?ORDINARY, _) -> true;
named_plain(Kind, _) when is_atom(Kind) -> true;
named_plain(Kind, K) ->
    case element(1, Kind) of
        ?PROXYOBJ_TAG -> false;
        module_namespace -> false;
        typed_array_obj -> false;
        ?ARRAYOBJ_TAG -> K =/= ?LENGTH_KEY;
        string_obj -> K =/= ?LENGTH_KEY;
        _ -> true
    end.

%% get_elem(Store, V, Key) -> JsVal | miss
%% `V[Key]` for the shapes a loop body produces: a non-negative integer
%% index into an Array or Arguments cell (own element present, no
%% {index,_} props override; holes miss so the full path does the proto
%% walk), an array index on an ordinary props-only cell (an own data
%% property, or `undefined` when the whole plain chain lacks it), or a
%% string key, which canonicalizes and reads as get_field / an index.
%% Anything else (float or negative index, symbol, object key, an exotic
%% cell) misses.
-define(MAX_ARRAY_INDEX, 4294967294).
get_elem(Store, {?HANDLE_TAG, Id}, Idx) when is_integer(Idx), Idx >= 0 ->
    Data = element(?STORE_DATA, Store),
    case array:get(Id, Data) of
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
            cell_field(Store, element(?HANDLE_ID, Obj), K);
        {?OKEY_STRING, {?KEY_INDEX, Idx}} -> get_elem(Store, Obj, Idx);
        _ -> miss
    end;
get_elem(_, _, _) -> miss.

%% get_elem2(Store, V, Key) -> JsVal | miss
%% get_elem for GetElem2, which also re-pushes the canonical key: only an
%% integer key is its own canonical value, so any other key misses.
get_elem2(Store, Obj, Idx) when is_integer(Idx) -> get_elem(Store, Obj, Idx);
get_elem2(_, _, _) -> miss.

%% An {index,Idx} props entry shadowing the elements store (a defineProperty'd
%% element). An array's props map is nearly always empty, which map_size
%% sees without building the key.
-compile({inline, [index_overridden/2]}).
index_overridden(Props, Idx) ->
    map_size(Props) =/= 0 andalso is_map_key({?KEY_INDEX, Idx}, Props).

elem_read({?ELEMS_DENSE, A}, Idx) ->
    case Idx < array:size(A) of
        true ->
            case array:get(Idx, A) of
                ?ELEMS_HOLE -> miss;
                V -> V
            end;
        false -> miss
    end;
elem_read({?ELEMS_SPARSE, M}, Idx) ->
    case M of
        #{Idx := V} -> V;
        _ -> miss
    end;
elem_read(_, _) -> miss.

%% put_field(Store, V, K, Val) -> Store2 | miss
%% §10.1.9.2 OrdinarySetWithOwnDescriptor for a kind whose named keys are
%% ordinary. Step 2, an EXISTING own writable data property: overwrite the
%% SShapedObject slot, or replace the value inside the DataProperty
%% (attributes and creation seq kept, §10.1.11). Step 1 → 2.c-h, CREATION
%% on an extensible SObject: only when the prototype chain holds nothing
%% at the key but plain writable data (chain_free), so a setter or a
%% read-only property up the chain still takes the slow path; the new
%% {W,E,C} property is stamped with the store's prop_seq (t_next_prop_seq).
%% Non-writable, accessors, non-extensible / shaped receivers for a new key
%% and exotic receivers miss. Returns the rebuilt store.
put_field(Store, Obj, K, V) -> put_field(Store, Obj, K, V, true).

%% Create: whether an absent key may be created (false: replace only).
put_field(Store, {?HANDLE_TAG, Id}, K, V, Create) ->
    Data = element(?STORE_DATA, Store),
    case array:get(Id, Data) of
        {?SSHAPED_TAG, Sid, P, Slots} ->
            case element(?STORE_SHAPES, Store) of
                #{Sid := Desc} ->
                    KeyBin = element(2, K),
                    case element(?SHAPE_OFFSETS, Desc) of
                        #{KeyBin := Off} ->
                            NewSlot = {?SSHAPED_TAG, Sid, P,
                                       setelement(Off + 1, Slots, V)},
                            setelement(?STORE_DATA, Store, array:set(Id, NewSlot, Data));
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

%% put_prop(Store, Data, Id, Slot, K, V, Create) -> Store2 | miss
%% The props-map half of put_field / put_elem for an SObject whose lookup
%% of K ({named,Bin} or {index,Idx}) is a plain props probe: replace the
%% value of an existing own writable data property, or (Create) add a
%% {W,E,C} one stamped with the store's prop_seq when the receiver is
%% extensible and the chain above holds nothing but writable data at K.
put_prop(Store, Data, Id, Slot, K, V, Create) ->
    Props = element(?SOBJECT_PROPS, Slot),
    case Props of
        #{K := Prop}
          when element(1, Prop) =:= ?DATAPROP_TAG,
               element(?DATAPROP_WRITABLE, Prop) =:= true ->
            NewProps = Props#{K := setelement(?DATAPROP_VALUE, Prop, V)},
            NewSlot = setelement(?SOBJECT_PROPS, Slot, NewProps),
            setelement(?STORE_DATA, Store, array:set(Id, NewSlot, Data));
        #{K := _} -> miss;
        _ when Create, element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
            case chain_free(Data, element(?STORE_SHAPES, Store),
                            element(?SOBJECT_PROTO, Slot), K) of
                false -> miss;
                true ->
                    Seq = element(?STORE_PROP_SEQ, Store),
                    Prop = {?DATAPROP_TAG, V, true, true, true, Seq},
                    NewSlot = setelement(?SOBJECT_PROPS, Slot, Props#{K => Prop}),
                    arc_rt_obj_ffi:store_put_seq(Store, array:set(Id, NewSlot, Data),
                                                 Seq + 1)
            end;
        _ -> miss
    end.

%% define_field(Store, V, K, Val) -> Store2 | miss
%% §7.3.5 CreateDataProperty of a Named key on an ordinary, extensible
%% SObject (the `{key: v}` literal field): a fresh {W,E,C} data property
%% stamped with the store's prop_seq, or an in-place replacement of a
%% configurable data property (creation order kept, §10.1.11). A
%% non-configurable or accessor current property, and any other receiver,
%% miss to the full [[DefineOwnProperty]].
define_field(Store, {?HANDLE_TAG, Id}, K, V) ->
    Data = element(?STORE_DATA, Store),
    case array:get(Id, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG,
                  element(?SOBJECT_KIND, Slot) =:= ?ORDINARY,
                  element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
            Props = element(?SOBJECT_PROPS, Slot),
            case Props of
                #{K := Old} when element(1, Old) =:= ?DATAPROP_TAG,
                                 element(?DATAPROP_CONFIGURABLE, Old) =:= true ->
                    Prop = {?DATAPROP_TAG, V, true, true, true,
                            element(?DATAPROP_SEQ, Old)},
                    NewSlot = setelement(?SOBJECT_PROPS, Slot, Props#{K := Prop}),
                    setelement(?STORE_DATA, Store, array:set(Id, NewSlot, Data));
                #{K := _} -> miss;
                _ ->
                    Seq = element(?STORE_PROP_SEQ, Store),
                    Prop = {?DATAPROP_TAG, V, true, true, true, Seq},
                    NewSlot = setelement(?SOBJECT_PROPS, Slot, Props#{K => Prop}),
                    arc_rt_obj_ffi:store_put_seq(Store, array:set(Id, NewSlot, Data),
                                                 Seq + 1)
            end;
        _ -> miss
    end;
define_field(_, _, _, _) -> miss.

chain_free(Data, Shapes, Proto, {?KEY_NAMED, _} = K) ->
    arc_rt_obj_ffi:named_free(Data, Shapes, Proto, K, 64);
chain_free(Data, Shapes, Proto, {?KEY_INDEX, Idx}) ->
    index_free(Data, Shapes, Proto, Idx, 64).

%% put_elem(Store, V, Idx, Val) -> Store2 | miss
%% `V[Idx] = Val` on an extensible Array cell for an array index Idx
%% (0 =< Idx =< 2^32-2, rt_types.max_array_index) in [0, Length].
%% Overwriting a present element is a write to an own writable data
%% property. Filling a hole or appending at Idx == Length creates a
%% property, so it first needs the prototype chain to hold nothing at Idx
%% (a setter or read-only index up the chain takes the store, §10.1.9.2
%% step 2) and, for the append, a writable "length" (§10.4.2.1 step 2.h).
%% An ordinary props-only receiver takes the put_field write under the
%% {index,Idx} key; a string key canonicalizes to one of the two. An
%% {index,Idx} props override on an array, a non-extensible or exotic
%% receiver, a key past the array-index range (2^32-1 is a Named key and
%% never moves "length"), or a dense fill past the allocated size misses.
put_elem(Store, {?HANDLE_TAG, Id}, Idx, V)
  when is_integer(Idx), Idx >= 0, Idx =< ?MAX_ARRAY_INDEX ->
    Data = element(?STORE_DATA, Store),
    case array:get(Id, Data) of
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
                    case elem_has(Elems, Idx)
                         orelse index_free(Data, element(?STORE_SHAPES, Store),
                                           element(?SOBJECT_PROTO, Slot), Idx, 64) of
                        false -> miss;
                        true ->
                            case elem_write(Elems, Idx, V) of
                                miss -> miss;
                                NewE ->
                                    NewSlot = setelement(?SOBJECT_ELEMENTS, Slot, NewE),
                                    setelement(?STORE_DATA, Store,
                                               array:set(Id, NewSlot, Data))
                            end
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
                                               array:set(Id, NewSlot, Data))
                            end
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
put_elem(Store, {?HANDLE_TAG, _} = Obj, Key, V) when is_binary(Key) ->
    case arc_rt_val_ffi:t_to_property_key_fast(Key) of
        {?OKEY_STRING, {?KEY_NAMED, _} = K} -> put_field(Store, Obj, K, V);
        {?OKEY_STRING, {?KEY_INDEX, Idx}} -> put_elem(Store, Obj, Idx, V);
        _ -> miss
    end;
put_elem(_, _, _, _) -> miss.

%% The Array "length" attribute override, when defineProperty made one;
%% absent means the default writable length.
length_writable(#{?LENGTH_KEY := Prop})
  when element(1, Prop) =:= ?DATAPROP_TAG ->
    element(?DATAPROP_WRITABLE, Prop) =:= true;
length_writable(_) -> true.

%% index_free(Data, Shapes, Proto, Idx, Fuel) -> boolean()
%% No object on the prototype chain starting at Proto has an own property
%% at Idx, along hops whose index lookup is a pure props/elements probe.
%% A Proxy, String, TypedArray or namespace hop, a dangling handle, or more
%% than Fuel hops answer false.
index_free(_, _, ?NONE, _, _) -> true;
index_free(_, _, _, _, 0) -> false;
index_free(Data, Shapes, {?SOME, {?HANDLE_TAG, P}}, Idx, Fuel) ->
    case array:get(P, Data) of
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

%% Whether an Index key on this ObjKind is answered by the props map plus
%% the elements store alone (rt/obj own_property_of): Proxy and namespace
%% cells trap, String objects expose their code units, TypedArrays their
%% buffer.
index_is_plain(Kind) when is_atom(Kind) -> true;
index_is_plain(Kind) ->
    case element(1, Kind) of
        ?PROXYOBJ_TAG -> false;
        module_namespace -> false;
        typed_array_obj -> false;
        string_obj -> false;
        _ -> true
    end.

%% A present (non-hole) element at Idx.
elem_has({?ELEMS_DENSE, A}, Idx) ->
    Idx < array:size(A) andalso array:get(Idx, A) =/= ?ELEMS_HOLE;
elem_has({?ELEMS_SPARSE, M}, Idx) -> is_map_key(Idx, M);
elem_has(_, _) -> false.

elem_write({?ELEMS_DENSE, A}, Idx, V) ->
    case Idx < array:size(A) of
        true -> {?ELEMS_DENSE, array:set(Idx, V, A)};
        false -> miss
    end;
elem_write({?ELEMS_SPARSE, M}, Idx, V) -> {?ELEMS_SPARSE, M#{Idx => V}};
elem_write(_, _, _) -> miss.

%% Append at Idx == Length. A dense array:set/3 extends past size(A) itself;
%% the gap and size bounds are rt/elements' dense-promotion policy, past
%% which the write belongs to the sparse representation (miss). An empty
%% store starts dense the way rt/elements `set` does.
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
