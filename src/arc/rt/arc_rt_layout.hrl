%% gleam record layouts and shared constants, checked by arc_rt_layout_test
%% <CTOR>_TAG is the record atom, <CTOR>_<FIELD> a 1-based position,
%% <CTOR>_SIZE the tuple size including the tag

-ifndef(ARC_RT_LAYOUT_HRL).
-define(ARC_RT_LAYOUT_HRL, true).

%% gleam option
-define(SOME, some).
-define(NONE, none).

%% Agent
-define(AGENT_TAG, agent).
-define(AGENT_STORE, 2).
-define(AGENT_REALM, 3).
-define(AGENT_HOST_FNS, 7).
-define(AGENT_REALMS, 8).
-define(AGENT_SIZE, 12).

%% Store
-define(STORE_TAG, store).
-define(STORE_CELLS, 2).
-define(STORE_NEXT_ID, 3).
-define(STORE_ALLOC_SINCE_GC, 4).
-define(STORE_PROP_SEQ, 6).
-define(STORE_SHAPES, 7).
-define(STORE_NEXT_SHAPE, 8).
-define(STORE_ICS, 9).
-define(STORE_PLAIN_WRITE_PROTOS, 10).
-define(STORE_GLOBAL_EPOCH, 11).
-define(STORE_PINNED_ROOTS, 14).
-define(STORE_SIZE, 15).
%% arena marker for a freed cell id
-define(STORE_FREE_CELL, js_free).

%% Realm and BuiltinPair
-define(REALM_TAG, realm).
-define(REALM_OBJECT, 2).
-define(REALM_FUNCTION, 3).
-define(REALM_ARRAY, 4).
-define(REALM_STRING, 5).
-define(REALM_NUMBER, 6).
-define(REALM_MAP, 18).
-define(REALM_SET, 19).
-define(REALM_ARRAY_ITER_PROTO, 37).
-define(REALM_STRING_ITER_PROTO, 38).
-define(REALM_MAP_ITER_PROTO, 39).
-define(REALM_SET_ITER_PROTO, 40).
-define(REALM_GLOBAL_OBJECT, 51).
-define(REALM_ID, 53).
-define(REALM_SIZE, 55).
-define(BUILTINPAIR_TAG, builtin_pair).
-define(BUILTINPAIR_PROTOTYPE, 2).
-define(BUILTINPAIR_CONSTRUCTOR, 3).

%% LexicalGlobal, Let and Const alike
-define(LEXICAL_GLOBAL_VALUE, 2).

%% JsVal
-define(HANDLE_TAG, handle).
-define(HANDLE_ID, 2).
-define(STR_TAG, js_str).
-define(IS_STR(V), (is_binary(V) orelse
                    (is_tuple(V) andalso tuple_size(V) =:= 4
                     andalso element(1, V) =:= ?STR_TAG))).
-define(IS_INF(V), (V =:= js_inf orelse V =:= js_neg_inf)).
-define(IS_NULLISH(V), (V =:= undefined orelse V =:= null)).
-define(IS_JS_NUMBER(V),
        (is_number(V) orelse V =:= js_nan orelse ?IS_INF(V))).

%% cells
-define(SBOX_TAG, s_box).
-define(SBOX_VALUE, 2).

-define(SOBJECT_TAG, s_object).
-define(SOBJECT_KIND, 2).
-define(SOBJECT_PROTO, 3).
-define(SOBJECT_PROPS, 4).
-define(SOBJECT_SYMBOL_PROPS, 5).
-define(SOBJECT_ELEMENTS, 6).
-define(SOBJECT_EXTENSIBLE, 7).
-define(SOBJECT_SIZE, 7).

-define(SSHAPEDOBJECT_TAG, s_shaped_object).
-define(SSHAPEDOBJECT_SHAPE_ID, 2).
-define(SSHAPEDOBJECT_PROTO, 3).
-define(SSHAPEDOBJECT_SLOTS, 4).
-define(SSHAPEDOBJECT_OFFSETS, 5).
-define(SSHAPEDOBJECT_SIZE, 5).

%% proto sits at the same position in both object cells
-define(CELL_PROTO, ?SOBJECT_PROTO).

%% ShapeDesc and the shape slots tuple
-define(SHAPE_TAG, shape_desc).
-define(SHAPE_SLOT_COUNT, 2).
-define(SHAPE_OFFSETS, 3).
-define(SHAPE_TRANSITIONS, 4).
-define(SHAPE_SIZE, 4).
-define(SLOT_AT(Slots, Off), element((Off) + 1, Slots)).
-define(SLOT_SET(Slots, Off, V), setelement((Off) + 1, Slots, V)).

%% ObjKind
-define(ORDINARY, ordinary).
-define(GLOBALOBJ, global_obj).

-define(COMPILEDFN_TAG, compiled_fn).
-define(COMPILEDFN_CODE, 2).
-define(COMPILEDFN_HOME_OBJECT, 3).
-define(COMPILEDFN_FLAGS, 4).
-define(COMPILEDFN_FIELDS_INIT, 5).
-define(COMPILEDFN_DIRECT_ENTRY, 6).
-define(COMPILEDFN_NAME, 7).
-define(COMPILEDFN_LENGTH, 8).
-define(COMPILEDFN_BIRTH, 9).
-define(COMPILEDFN_SIZE, 9).
%% pattern over the fields the call paths read
-define(COMPILEDFN(Code, Home, Flags, FieldsInit, DirectEntry),
        {?COMPILEDFN_TAG, Code, Home, Flags, FieldsInit, DirectEntry, _, _, _}).
%% the direct_entry field holds some ?DIRECT_ENTRY or none
-define(DIRECT_ENTRY_TAG, direct_entry).
-define(DIRECT_ENTRY(Code, Arity, TakesThis),
        {?DIRECT_ENTRY_TAG, Code, Arity, TakesThis}).
-define(DIRECT_ENTRY_SIZE, 4).

-define(BYTECODEFN_TAG, bytecode_fn).
-define(BYTECODEFN_BIRTH, 9).
-define(BYTECODEFN_SIZE, 9).

-define(BIRTH_SETTLED, birth_settled).
-define(BIRTHPENDING_TAG, birth_pending).
-define(BIRTHPENDING_PROTOTYPE_PARENT, 2).

-define(NATIVEFN_TAG, native_fn).
-define(NATIVEFN_TOKEN, 2).
-define(NATIVEFN_NAME, 3).
-define(NATIVEFN_LENGTH, 4).
-define(NATIVEFN_CONSTRUCTIBLE, 5).
-define(NATIVEFN_SIZE, 5).

-define(BOUNDFN_TAG, bound_fn).

-define(ARRAYOBJ_TAG, array_obj).
-define(ARRAYOBJ_LENGTH, 2).
-define(ARRAYOBJ_SIZE, 2).

-define(ARGUMENTSOBJ_TAG, arguments_obj).
-define(ARGUMENTSOBJ_MAPPED, 3).
-define(ARGUMENTSOBJ_SIZE, 3).

-define(ARRAYITERATOR_TAG, array_iterator).
-define(ARRAYITERATOR_TARGET, 2).
-define(ARRAYITERATOR_INDEX, 3).
-define(ARRAYITERATOR_KIND, 4).
-define(ARRAYITERATOR_SIZE, 4).
-define(ARRAYITER_VALUES, array_iter_values).

-define(GENERATOROBJ_TAG, generator_obj).
-define(GENERATOROBJ_DATA, 2).
-define(GENERATOROBJ_SIZE, 2).

-define(MAPOBJ_TAG, map_obj).
-define(SETOBJ_TAG, set_obj).
-define(PROXYOBJ_TAG, proxy_obj).
-define(STRINGOBJ_TAG, string_obj).
-define(STRINGOBJ_VALUE, 2).
-define(TYPEDARRAYOBJ_TAG, typed_array_obj).
-define(MODULENS_TAG, module_namespace).

-define(FNFLAGS_TAG, fn_flags).
-define(FNFLAGS_IS_CONSTRUCTOR, 2).
-define(FNFLAGS_IS_CLASS_CONSTRUCTOR, 3).
-define(FNFLAGS_IS_DERIVED_CONSTRUCTOR, 4).
-define(FNFLAGS_IS_ARROW, 5).
-define(FNFLAGS_IS_GENERATOR, 6).
-define(FNFLAGS_IS_ASYNC, 7).
-define(FNFLAGS_IS_STRICT, 8).
-define(FNFLAGS_SIZE, 8).
%% neither class constructor nor generator nor async
-define(IS_PLAIN_FN(Flags),
        (element(?FNFLAGS_IS_CLASS_CONSTRUCTOR, Flags) =:= false andalso
         element(?FNFLAGS_IS_GENERATOR, Flags) =:= false andalso
         element(?FNFLAGS_IS_ASYNC, Flags) =:= false)).

%% NativeToken values the kernels recognise
-define(ITERATORN_TAG, iterator_n).
-define(TOKEN_RETURN_THIS, return_this).
-define(TOKEN_ARRAY_ITER_NEXT, {?ITERATORN_TAG, array_iterator_next}).
-define(TOKEN_GENERATOR_NEXT, {generator_n, generator_next}).
-define(TOKEN_ARRAY_VALUES, {array_n, array_prototype_values}).
-define(TOKEN_STRING_ITER, {string_n, string_prototype_symbol_iterator}).
-define(TOKEN_STRING_ITER_NEXT, {?ITERATORN_TAG, string_iterator_next}).
-define(TOKEN_MAP_ENTRIES, {map_n, map_entries}).
-define(TOKEN_MAP_ITER_NEXT, {?ITERATORN_TAG, map_iterator_next}).
-define(TOKEN_SET_VALUES, {set_n, set_values}).
-define(TOKEN_SET_ITER_NEXT, {?ITERATORN_TAG, set_iterator_next}).

%% Property
-define(DATAPROPERTY_TAG, data_property).
-define(DATAPROPERTY_VALUE, 2).
-define(DATAPROPERTY_WRITABLE, 3).
-define(DATAPROPERTY_ENUMERABLE, 4).
-define(DATAPROPERTY_CONFIGURABLE, 5).
-define(DATAPROPERTY_SEQ, 6).
-define(DATAPROPERTY_SIZE, 6).
-define(ACCESSORPROPERTY_TAG, accessor_property).
-define(ACCESSORPROPERTY_GET, 2).
-define(ACCESSORPROPERTY_SET, 3).
-define(ACCESSORPROPERTY_SIZE, 6).
%% writable, enumerable, configurable data property
-define(PLAIN_PROPERTY(V, Seq), {?DATAPROPERTY_TAG, V, true, true, true, Seq}).

%% PropertyKey and ObjectKey
-define(KEY_NAMED, named).
-define(KEY_INDEX, index).
-define(KEY_PRIVATE, private).
-define(STRINGKEY_TAG, string_key).
-define(SYMBOLKEY_TAG, symbol_key).
-define(LENGTH_KEY, {?KEY_NAMED, <<"length">>}).
%% SymbolId for Symbol.iterator
-define(SYMBOL_ITERATOR, {well_known_symbol, sym_iterator}).

%% Elements
-define(ELEMS_NONE, no_elements).
-define(ELEMS_DENSE, dense).
-define(ELEMS_SPARSE, sparse).
-define(ELEMS_HOLE, js_hole).
-define(VEC_TAG, js_vec).

%% Completion, coroutine steps, iterator records
-define(COMPLETION_NORMAL, normal_completion).
-define(COMPLETION_THROW, throw_completion).
-define(STEP_RETURN, step_return).
-define(STEP_THROW, step_throw).
-define(STEP_YIELD, step_yield).
-define(STEP_AWAIT, step_await).
-define(RESUMECOMPILED_TAG, resume_compiled).
-define(RESUMEFRAME_TAG, resume_frame).
-define(ITERATORRECORD_TAG, iterator_record).
-define(ARC_ITER, arc_iter).

%% shapes shared with emitted code
%% the js exception as raised and caught, fixed by carder rt_exn
-define(JS_THROW(St, E), {wasm_exn, 0, [St, E]}).
%% call frame handed to compiled code
-define(FRAME(This, Fn, Home, NewTarget), {This, Fn, Home, NewTarget}).

%% constants
%% bytecode key.max_array_index, 2^32 - 2
-define(MAX_ARRAY_INDEX, 4294967294).
%% limits.max_safe_integer, 2^53 - 1
-define(MAX_SAFE_INT, 9007199254740991).
%% proto chain hops a kernel walks before it answers miss
-define(MAX_PROTO_HOPS, 64).
%% dense promotion policy, mirrors rt/elements
-define(MAX_GAP, 1024).
-define(MAX_DENSE_INDEX, 10000000).

%% shared kernels; each module instantiates them as one-line locals

%% store with Cell written at Id, next_id and alloc_since_gc bumped
-define(ALLOC_CELL(Store, Cells, Id, Cell),
        setelement(?STORE_ALLOC_SINCE_GC,
                   setelement(?STORE_NEXT_ID,
                              setelement(?STORE_CELLS, Store,
                                         arc_rt_arena_ffi:set(Id, Cell, Cells)),
                              Id + 1),
                   element(?STORE_ALLOC_SINCE_GC, Store) + 1)).

%% which builtin a native fn cell dispatches to, else none
-define(NATIVE_TOKEN(Cell),
        (case Cell of
             _ when element(1, Cell) =:= ?SOBJECT_TAG,
                    element(1, element(?SOBJECT_KIND, Cell)) =:= ?NATIVEFN_TAG ->
                 element(?NATIVEFN_TOKEN, element(?SOBJECT_KIND, Cell));
             _ -> none
         end)).

%% element at idx or the hole
-define(ELEM_AT(Els, Idx),
        (case Els of
             {?ELEMS_DENSE, ElemAtVec} -> arc_tree_array_ffi:get_or_hole(Idx, ElemAtVec);
             {?ELEMS_SPARSE, ElemAtMap} ->
                 case ElemAtMap of
                     #{Idx := ElemAtV} -> ElemAtV;
                     _ -> ?ELEMS_HOLE
                 end;
             _ -> ?ELEMS_HOLE
         end)).

%% append at idx while the dense gap policy allows it, else miss
-define(ELEM_WRITE_GROW(Els, Idx, V),
        (case Els of
             {?ELEMS_DENSE, GrowVec} ->
                 case Idx - arc_tree_array_ffi:size(GrowVec) =< ?MAX_GAP
                      andalso Idx < ?MAX_DENSE_INDEX of
                     true -> {?ELEMS_DENSE, arc_tree_array_ffi:set(Idx, V, GrowVec)};
                     false -> miss
                 end;
             {?ELEMS_SPARSE, GrowMap} -> {?ELEMS_SPARSE, GrowMap#{Idx => V}};
             ?ELEMS_NONE when Idx =< ?MAX_GAP ->
                 {?ELEMS_DENSE, arc_tree_array_ffi:set(Idx, V, {})};
             _ -> miss
         end)).

%% known successor shape for adding keybin, as {To, ToOffsets}, else miss
-define(SHAPED_NEXT(Shapes, Sid, KeyBin),
        (case Shapes of
             #{Sid := NextFromDesc} ->
                 case element(?SHAPE_TRANSITIONS, NextFromDesc) of
                     #{KeyBin := NextTo} ->
                         {NextTo, element(?SHAPE_OFFSETS, map_get(NextTo, Shapes))};
                     _ -> miss
                 end;
             _ -> miss
         end)).

%% false for exotic or virtual named keys; needs a local birth_plain/2
-define(NAMED_KEY_IS_PLAIN(Kind, K, LengthK),
        (case Kind of
             ?ORDINARY -> true;
             _ when is_atom(Kind) -> true;
             _ ->
                 case element(1, Kind) of
                     ?PROXYOBJ_TAG -> false;
                     ?MODULENS_TAG -> false;
                     ?TYPEDARRAYOBJ_TAG -> false;
                     ?ARRAYOBJ_TAG -> K =/= LengthK;
                     ?STRINGOBJ_TAG -> K =/= LengthK;
                     ?BYTECODEFN_TAG ->
                         birth_plain(element(?BYTECODEFN_BIRTH, Kind), K);
                     ?COMPILEDFN_TAG ->
                         birth_plain(element(?COMPILEDFN_BIRTH, Kind), K);
                     _ -> true
                 end
         end)).

%% length, name, prototype are not in props until birth settles
-define(LAZY_KEY_IS_PLAIN(Birth, K, LengthK, NameK, PrototypeK),
        (case Birth of
             ?BIRTH_SETTLED -> true;
             _ when K =:= LengthK -> false;
             _ when K =:= NameK -> false;
             _ when K =:= PrototypeK ->
                 element(?BIRTHPENDING_PROTOTYPE_PARENT, Birth) =:= ?NONE;
             _ -> true
         end)).

%% inline cache entry tags in store ics
-define(IC_READ, ic_read).
-define(IC_CALL, ic_call).
-define(IC_INIT, ic_init).
-define(IC_GLOBAL, ic_global).
-define(IC_OFF, ic_off).
%% IcCallMatch tags
-define(ICPLAIN_TAG, ic_plain).
-define(ICOWN_TAG, ic_own).
-define(ICPRIM_TAG, ic_prim).

-endif.
