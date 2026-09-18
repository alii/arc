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

%% JsStore
-define(STORE_TAG, js_store).
-define(STORE_DATA, 2).
-define(STORE_NEXT, 3).
-define(STORE_ALLOC_SINCE_GC, 4).
-define(STORE_PROP_SEQ, 6).
-define(STORE_SHAPES, 7).
-define(STORE_NEXT_SHAPE, 8).
-define(STORE_ICS, 9).
-define(STORE_FREE_PROTOS, 10).
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
-define(REALM_GLOBAL, 51).
-define(REALM_ID, 53).
-define(REALM_SIZE, 55).
-define(PAIR_TAG, builtin_pair).
-define(PAIR_PROTO, 2).
-define(PAIR_CTOR, 3).

%% LexicalGlobal, Let and Const alike
-define(LEXICAL_GLOBAL_VALUE, 2).

%% JsVal
-define(HANDLE_TAG, js_cell).
-define(HANDLE_ID, 2).
-define(STR_TAG, js_str).
-define(IS_STR(V), (is_binary(V) orelse
                    (is_tuple(V) andalso tuple_size(V) =:= 4
                     andalso element(1, V) =:= ?STR_TAG))).
-define(IS_INF(V), (V =:= js_inf orelse V =:= js_neg_inf)).
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

-define(SSHAPED_TAG, s_shaped_object).
-define(SSHAPED_SID, 2).
-define(SSHAPED_PROTO, 3).
-define(SSHAPED_SLOTS, 4).
-define(SSHAPED_OFFSETS, 5).
-define(SSHAPED_SIZE, 5).

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

-define(KFN_TAG, k_compiled).
-define(KFN_CODE, 2).
-define(KFN_HOME, 3).
-define(KFN_FLAGS, 4).
-define(KFN_FIELDS_INIT, 5).
-define(KFN_SIMPLE, 6).
-define(KFN_NAME, 7).
-define(KFN_LENGTH, 8).
-define(KFN_BIRTH, 9).
-define(KFN_SIZE, 9).
%% pattern over the fields the call paths read
-define(KFN(Code, Home, Flags, FieldsInit, Simple),
        {?KFN_TAG, Code, Home, Flags, FieldsInit, Simple, _, _, _}).
%% the simple field holds some ?DIRECT_ENTRY or none
-define(DIRECT_ENTRY(Code, Arity, TakesThis), {Code, Arity, TakesThis}).

-define(KBYTECODE_TAG, k_bytecode).
-define(KBYTECODE_BIRTH, 9).
-define(KBYTECODE_SIZE, 9).

-define(BIRTH_SETTLED, birth_settled).
-define(BIRTH_PENDING_TAG, birth_pending).
-define(BIRTH_PROTOTYPE_PARENT, 2).

-define(KNATIVE_TAG, k_native).
-define(KNATIVE_TOKEN, 2).
-define(KNATIVE_NAME, 3).
-define(KNATIVE_LENGTH, 4).
-define(KNATIVE_CONSTRUCTIBLE, 5).
-define(KNATIVE_SIZE, 5).

-define(BOUNDFN_TAG, k_bound).

-define(ARRAYOBJ_TAG, array_obj).
-define(ARRAYOBJ_LENGTH, 2).
-define(ARRAYOBJ_SIZE, 2).

-define(ARGUMENTSOBJ_TAG, arguments_obj).
-define(ARGUMENTSOBJ_MAPPED, 3).
-define(ARGUMENTSOBJ_SIZE, 3).

-define(ARRAYITER_TAG, array_iterator).
-define(ARRAYITER_TARGET, 2).
-define(ARRAYITER_INDEX, 3).
-define(ARRAYITER_KIND, 4).
-define(ARRAYITER_SIZE, 4).
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
-define(FNFLAGS_IS_CTOR, 2).
-define(FNFLAGS_IS_CLASS_CTOR, 3).
-define(FNFLAGS_IS_DERIVED, 4).
-define(FNFLAGS_IS_ARROW, 5).
-define(FNFLAGS_IS_METHOD, 6).
-define(FNFLAGS_IS_GEN, 7).
-define(FNFLAGS_IS_ASYNC, 8).
-define(FNFLAGS_IS_STRICT, 9).
-define(FNFLAGS_SIZE, 9).
%% neither class constructor nor generator nor async
-define(IS_PLAIN_FN(Flags),
        (element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false andalso
         element(?FNFLAGS_IS_GEN, Flags) =:= false andalso
         element(?FNFLAGS_IS_ASYNC, Flags) =:= false)).

%% NativeToken values the kernels recognise
-define(TOKEN_ARRAY_ITER_NEXT, {iterator_n, array_iterator_next}).
-define(TOKEN_GENERATOR_NEXT, {generator_n, generator_next}).
-define(TOKEN_ARRAY_VALUES, {array_n, array_prototype_values}).
-define(TOKEN_STRING_ITER, {string_n, string_prototype_symbol_iterator}).
-define(TOKEN_STRING_ITER_NEXT, {iterator_n, string_iterator_next}).
-define(TOKEN_MAP_ENTRIES, {map_n, map_entries}).
-define(TOKEN_MAP_ITER_NEXT, {iterator_n, map_iterator_next}).
-define(TOKEN_SET_VALUES, {set_n, set_values}).
-define(TOKEN_SET_ITER_NEXT, {iterator_n, set_iterator_next}).

%% Property
-define(DATAPROP_TAG, data_property).
-define(DATAPROP_VALUE, 2).
-define(DATAPROP_WRITABLE, 3).
-define(DATAPROP_ENUMERABLE, 4).
-define(DATAPROP_CONFIGURABLE, 5).
-define(DATAPROP_SEQ, 6).
-define(DATAPROP_SIZE, 6).
-define(ACCESSORPROP_TAG, accessor_property).
-define(ACCESSORPROP_GET, 2).
-define(ACCESSORPROP_SET, 3).
-define(ACCESSORPROP_SIZE, 6).
%% writable, enumerable, configurable data property
-define(PLAIN_PROPERTY(V, Seq), {?DATAPROP_TAG, V, true, true, true, Seq}).

%% PropertyKey and ObjectKey
-define(KEY_NAMED, named).
-define(KEY_INDEX, index).
-define(KEY_PRIVATE, private).
-define(OKEY_STRING, string_key).
-define(OKEY_SYMBOL, symbol_key).
-define(LENGTH_KEY, {?KEY_NAMED, <<"length">>}).

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
-define(RESUME_COMPILED_TAG, resume_compiled).
-define(RESUME_FRAME_TAG, resume_frame).
-define(ITERATOR_RECORD_TAG, iterator_record).
-define(ARC_ITER, arc_iter).

%% shapes shared with emitted code
%% the js exception as raised and caught, fixed by carder rt_exn
-define(JS_THROW(St, E), {wasm_exn, 0, [St, E]}).
%% call frame handed to compiled code
-define(FRAME(This, Fn, Home, NewTarget), {This, Fn, Home, NewTarget}).

%% constants
%% rt_types.max_array_index, 2^32 - 2
-define(MAX_ARRAY_INDEX, 4294967294).
%% limits.max_safe_integer, 2^53 - 1
-define(MAX_SAFE_INT, 9007199254740991).
%% proto chain hops a kernel walks before it answers miss
-define(MAX_PROTO_HOPS, 64).
%% dense promotion policy, mirrors rt/elements
-define(MAX_GAP, 1024).
-define(MAX_DENSE_INDEX, 10000000).

%% inline cache entry tags in store ics
-define(IC_READ, ic_read).
-define(IC_CALL, ic_call).
-define(IC_INIT, ic_init).
-define(IC_GLOBAL, ic_global).
-define(IC_OFF, ic_off).

-endif.
