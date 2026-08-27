%% Tuple positions (1-based, element/2) and tag atoms of the Gleam runtime
%% records that hand-written Erlang indexes directly. One definition site;
%% asserted against the real Gleam constructors by arc_rt_layout_test.

-ifndef(ARC_RT_LAYOUT_HRL).
-define(ARC_RT_LAYOUT_HRL, true).

%% arc/rt/types.Agent:
%%   {agent, Store, Realm, TemplateObjects, Frames, Hooks, HostFns, Realms,
%%    ImportHook, Waiters, CallDepth}.
%% The indexed fields are bare (no Option wrapper).
-define(AGENT_TAG, agent).
-define(AGENT_STORE, 2).
-define(AGENT_REALM, 3).
-define(AGENT_HOST_FNS, 7).
-define(AGENT_REALMS, 8).
-define(AGENT_ARITY, 11).
-define(SOME, some).
-define(NONE, none).

%% Plain (non-record) tuples the Erlang also indexes; documented, not named:
%%   Frame  {This, ActiveFunc, HomeObject, NewTarget}      (call_ffi mk_frame)
%%   KFN_SIMPLE payload {some, {Code, Arity, NeedsFrame}}   (call_ffi, call_fast_ffi)
%%   ShapeSlots: bare tuple, 0-based Off reads element(Off + 1, Slots)

%% arc/rt/types.Step / Resume (built by async_ffi from the sm wire step)
-define(STEP_RETURN, step_return).
-define(STEP_THROW, step_throw).
-define(STEP_YIELD, step_yield).
-define(STEP_AWAIT, step_await).
-define(RESUME_COMPILED_TAG, resume_compiled).
-define(RESUME_FRAME_TAG, resume_frame).

%% arc/rt/types.JsStore
-define(STORE_TAG, js_store).
-define(STORE_DATA, 2).
-define(STORE_NEXT, 3).
-define(STORE_PINNED_ROOTS, 4).
-define(STORE_ALLOC, 5).
-define(STORE_SHAPES, 14).
-define(STORE_NEXT_SHAPE, 15).
-define(STORE_ARITY, 17).
-define(STORE_ICS, 17).

%% arc/rt/types.Realm / BuiltinPair
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

%% arc/rt/types.Handle, also the JsVal object wire row
-define(HANDLE_TAG, js_cell).
-define(HANDLE_ID, 2).

%% arc/rt/types.JsSlot: SBox(value), a captured binding's cell
-define(SBOX_TAG, s_box).
-define(SBOX_VALUE, 2).

%% arc/rt/types.JsSlot: SObject
-define(SOBJECT_TAG, s_object).
-define(SOBJECT_KIND, 2).
-define(SOBJECT_PROTO, 3).
-define(SOBJECT_PROPS, 4).
-define(SOBJECT_SYMBOL_PROPS, 5).
-define(SOBJECT_ELEMENTS, 6).
-define(SOBJECT_EXTENSIBLE, 7).
-define(SOBJECT_ARITY, 7).

%% arc/rt/types.JsSlot: SShapedObject. Slots is a plain tuple, 0-based Off
%% reads element(Off + 1, Slots).
-define(SSHAPED_TAG, s_shaped_object).
-define(SSHAPED_SID, 2).
-define(SSHAPED_PROTO, 3).
-define(SSHAPED_SLOTS, 4).
-define(SSHAPED_ARITY, 4).

%% arc/rt/types.ShapeDesc
-define(SHAPE_TAG, shape_desc).
-define(SHAPE_ARITY_F, 2).
-define(SHAPE_OFFSETS, 3).
-define(SHAPE_TRANSITIONS, 4).
-define(SHAPE_ARITY, 4).

%% arc/rt/types.ObjKind: KCompiled
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

%% arc/rt/types.ObjKind: KBytecode (an interpreted function never takes a
%% compiled-code fast path; its [[Call]] goes through JsOps)
-define(KBYTECODE_TAG, k_bytecode).
-define(KBYTECODE_BIRTH, 9).
-define(KBYTECODE_ARITY, 9).

%% arc/rt/types.FnBirth
-define(BIRTH_SETTLED, birth_settled).
-define(BIRTH_PENDING_TAG, birth_pending).
-define(BIRTH_PROTOTYPE_PARENT, 2).

%% arc/rt/types.ObjKind: KNative, ArrayObj, Ordinary
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
%% arc/rt/types.ObjKind: ArgumentsObj(length, mapped)
-define(ARGUMENTSOBJ_TAG, arguments_obj).
-define(ARGUMENTSOBJ_MAPPED, 3).
-define(ARGUMENTSOBJ_ARITY, 3).
%% arc/rt/types.ObjKind: ArrayIterator(target, index, kind), GeneratorObj(data)
-define(ARRAYITER_TAG, array_iterator).
-define(ARRAYITER_TARGET, 2).
-define(ARRAYITER_INDEX, 3).
-define(ARRAYITER_KIND, 4).
-define(ARRAYITER_ARITY, 4).
-define(ARRAYITER_VALUES, array_iter_values).
-define(GENERATOROBJ_TAG, generator_obj).
-define(GENERATOROBJ_DATA, 2).
-define(GENERATOROBJ_ARITY, 2).
%% arc/rt/types.NativeToken: IteratorN(ArrayIteratorNext), GeneratorN(GeneratorNext)
-define(TOKEN_ARRAY_ITER_NEXT, {iterator_n, array_iterator_next}).
-define(TOKEN_GENERATOR_NEXT, {generator_n, generator_next}).
%% arc/rt/types.ObjKind: ProxyObj (tag only: fast paths must never read
%% through a proxy's stored proto/props — its internal methods are traps)
-define(PROXYOBJ_TAG, proxy_obj).
%% arc/rt/types.ObjKind: StringObj(value)
-define(STRINGOBJ_TAG, string_obj).
-define(STRINGOBJ_VALUE, 2).

%% arc/rt/types.FnFlags
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

%% arc/rt/types.Property
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

%% arc/rt/types.PropertyKey / ObjectKey (props map keys)
-define(KEY_NAMED, named).
-define(KEY_INDEX, index).
-define(KEY_PRIVATE, private).
-define(OKEY_STRING, string_key).
-define(OKEY_SYMBOL, symbol_key).

%% arc/rt/types.JsElements
-define(ELEMS_NONE, no_elements).
-define(ELEMS_DENSE, dense).
-define(ELEMS_SPARSE, sparse).
%% The dense store's `array` default (arc_rt_val_ffi:mk_hole/0): an absent
%% index, never a JsVal. Reads past the array's size answer it too.
-define(ELEMS_HOLE, js_hole).

%% arc/rt/call.Completion (built by the call ffi, matched by Gleam)
-define(COMPLETION_NORMAL, normal_completion).
-define(COMPLETION_THROW, throw_completion).

%% arc/rt/types.Realm: the primitive wrapper prototypes the fused field
%% read walks from for a string / number receiver.
-define(REALM_STRING, 5).
-define(REALM_NUMBER, 6).

%% arc/rt/types.JsStore: the property creation-order stamp the fused
%% field write bumps when it creates an own property.
-define(STORE_PROP_SEQ, 8).

%% arc/rt/types.JsStore.data is the cell arena (arc_rt_arena_ffi): a
%% freed or never-minted id reads back as this sentinel.
-define(STORE_FREE_SLOT, js_free).

-endif.
