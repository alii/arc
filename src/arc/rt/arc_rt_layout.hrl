%% gleam record tuple positions, checked by arc_rt_layout_test

-ifndef(ARC_RT_LAYOUT_HRL).
-define(ARC_RT_LAYOUT_HRL, true).

-define(AGENT_TAG, agent).
-define(AGENT_STORE, 2).
-define(AGENT_REALM, 3).
-define(AGENT_HOST_FNS, 7).
-define(AGENT_REALMS, 8).
-define(AGENT_ARITY, 11).
-define(SOME, some).
-define(NONE, none).

-define(STEP_RETURN, step_return).
-define(STEP_THROW, step_throw).
-define(STEP_YIELD, step_yield).
-define(STEP_AWAIT, step_await).
-define(RESUME_COMPILED_TAG, resume_compiled).
-define(RESUME_FRAME_TAG, resume_frame).

-define(STORE_TAG, js_store).
-define(STORE_DATA, 2).
-define(STORE_NEXT, 3).
-define(STORE_PINNED_ROOTS, 4).
-define(STORE_ALLOC, 5).
-define(STORE_SHAPES, 14).
-define(STORE_NEXT_SHAPE, 15).
-define(STORE_ARITY, 20).
-define(STORE_ICS, 17).
-define(STORE_FREE_PROTOS, 18).
-define(STORE_GLOBAL_EPOCH, 19).
-define(STORE_NAMES, 20).

-define(NAMES_TAG, name_table).
-define(NAMES_NUMBERS, 2).
-define(NAMES_TEXTS, 3).
-define(NAMES_NEXT, 4).
-define(NAMES_PINNED, 5).
-define(NAMES_ARITY, 6).

-define(REALM_TAG, realm).
-define(REALM_OBJECT, 2).
-define(REALM_FUNCTION, 3).
-define(REALM_ARRAY, 4).
-define(REALM_GLOBAL, 51).
-define(REALM_ID, 53).
-define(REALM_ARITY, 55).
-define(PAIR_TAG, builtin_pair).
-define(PAIR_PROTO, 2).
-define(PAIR_CTOR, 3).

-define(HANDLE_TAG, js_cell).
-define(HANDLE_ID, 2).

-define(PRIVATE_TAG, js_private).

-define(FT_TAG, func_template).
-define(FT_CONSTANTS, 7).
-define(FT_KEYS, 8).
-define(FT_FUNCTIONS, 10).
-define(FT_ARITY, 22).

-define(SBOX_TAG, s_box).
-define(SBOX_VALUE, 2).

-define(SOBJECT_TAG, s_object).
-define(SOBJECT_KIND, 2).
-define(SOBJECT_PROTO, 3).
-define(SOBJECT_PROPS, 4).
-define(SOBJECT_SYMBOL_PROPS, 5).
-define(SOBJECT_ELEMENTS, 6).
-define(SOBJECT_EXTENSIBLE, 7).
-define(SOBJECT_ARITY, 7).

-define(SSHAPED_TAG, s_shaped_object).
-define(SSHAPED_SID, 2).
-define(SSHAPED_PROTO, 3).
-define(SSHAPED_SLOTS, 4).
-define(SSHAPED_OFFSETS, 5).
-define(SSHAPED_ARITY, 5).

-define(SHAPE_TAG, shape_desc).
-define(SHAPE_ARITY_F, 2).
-define(SHAPE_OFFSETS, 3).
-define(SHAPE_TRANSITIONS, 4).
-define(SHAPE_ARITY, 4).

-define(KFN_TAG, k_compiled).
-define(KFN_CODE, 2).
-define(KFN_HOME, 3).
-define(KFN_FLAGS, 4).
-define(KFN_FIELDS_INIT, 5).
-define(KFN_SIMPLE, 6).
-define(KFN_NAME, 7).
-define(KFN_LENGTH, 8).
-define(KFN_BIRTH, 9).
-define(KFN_ARITY, 9).

-define(KBYTECODE_TAG, k_bytecode).
-define(KBYTECODE_BIRTH, 9).
-define(KBYTECODE_ARITY, 9).

-define(BIRTH_SETTLED, birth_settled).
-define(BIRTH_PENDING_TAG, birth_pending).
-define(BIRTH_PROTOTYPE_PARENT, 2).

-define(KNATIVE_TAG, k_native).
-define(KNATIVE_TOKEN, 2).
-define(KNATIVE_NAME, 3).
-define(KNATIVE_LENGTH, 4).
-define(KNATIVE_CONSTRUCTIBLE, 5).
-define(KNATIVE_ARITY, 5).
-define(ARRAYOBJ_TAG, array_obj).
-define(ARRAYOBJ_LENGTH, 2).
-define(ARRAYOBJ_ARITY, 2).
-define(ORDINARY, ordinary).
-define(GLOBALOBJ, global_obj).
-define(ARGUMENTSOBJ_TAG, arguments_obj).
-define(ARGUMENTSOBJ_MAPPED, 3).
-define(ARGUMENTSOBJ_ARITY, 3).
-define(ARRAYITER_TAG, array_iterator).
-define(ARRAYITER_TARGET, 2).
-define(ARRAYITER_INDEX, 3).
-define(ARRAYITER_KIND, 4).
-define(ARRAYITER_ARITY, 4).
-define(ARRAYITER_VALUES, array_iter_values).
-define(GENERATOROBJ_TAG, generator_obj).
-define(GENERATOROBJ_DATA, 2).
-define(GENERATOROBJ_ARITY, 2).
-define(TOKEN_ARRAY_ITER_NEXT, {iterator_n, array_iterator_next}).
-define(TOKEN_GENERATOR_NEXT, {generator_n, generator_next}).
-define(PROXYOBJ_TAG, proxy_obj).
-define(STRINGOBJ_TAG, string_obj).
-define(STRINGOBJ_VALUE, 2).

-define(FNFLAGS_TAG, fn_flags).
-define(FNFLAGS_IS_CTOR, 2).
-define(FNFLAGS_IS_CLASS_CTOR, 3).
-define(FNFLAGS_IS_DERIVED, 4).
-define(FNFLAGS_IS_ARROW, 5).
-define(FNFLAGS_IS_METHOD, 6).
-define(FNFLAGS_IS_GEN, 7).
-define(FNFLAGS_IS_ASYNC, 8).
-define(FNFLAGS_IS_STRICT, 9).
-define(FNFLAGS_ARITY, 9).

-define(DATAPROP_TAG, data_property).
-define(DATAPROP_VALUE, 2).
-define(DATAPROP_WRITABLE, 3).
-define(DATAPROP_ENUMERABLE, 4).
-define(DATAPROP_CONFIGURABLE, 5).
-define(DATAPROP_SEQ, 6).
-define(DATAPROP_ARITY, 6).
-define(ACCESSORPROP_TAG, accessor_property).
-define(ACCESSORPROP_GET, 2).
-define(ACCESSORPROP_SET, 3).
-define(ACCESSORPROP_ARITY, 6).

-define(OKEY_STRING, string_key).
-define(OKEY_SYMBOL, symbol_key).

-define(ELEMS_NONE, no_elements).
-define(ELEMS_DENSE, dense).
-define(ELEMS_SPARSE, sparse).
-define(ELEMS_HOLE, js_hole).
-define(VEC_TAG, js_vec).

-define(COMPLETION_NORMAL, normal_completion).
-define(COMPLETION_THROW, throw_completion).

-define(REALM_STRING, 5).
-define(REALM_NUMBER, 6).

-define(STORE_PROP_SEQ, 8).

-define(STORE_FREE_SLOT, js_free).

-endif.
