%% Tuple positions (1-based, element/2) and tag atoms of the Gleam runtime
%% records that hand-written Erlang indexes directly. One definition site;
%% asserted against the real Gleam constructors by arc_rt_layout_test.

-ifndef(ARC_RT_LAYOUT_HRL).
-define(ARC_RT_LAYOUT_HRL, true).

%% Outer threaded state. Today this is twocore rt_state.InstanceState
%% (js_store=9, js_realm=10); becomes arc's Agent when the runtime moves.
%% Both fields are Option-wrapped on the wire: {some, X} | none.
-define(AGENT_STORE, 9).
-define(AGENT_REALM, 10).
-define(SOME, some).
-define(NONE, none).

%% rt_js_types.JsStore
-define(STORE_TAG, js_store).
-define(STORE_DATA, 2).
-define(STORE_FREE, 3).
-define(STORE_NEXT, 4).
-define(STORE_PINNED_ROOTS, 5).
-define(STORE_ALLOC, 6).
-define(STORE_SHAPES, 17).
-define(STORE_NEXT_SHAPE, 18).
-define(STORE_ARITY, 18).

%% rt_js_types.Realm / BuiltinPair
-define(REALM_TAG, realm).
-define(REALM_OBJECT, 2).
-define(REALM_FUNCTION, 3).
-define(REALM_ARRAY, 4).
-define(REALM_GLOBAL, 49).
-define(REALM_ARITY, 49).
-define(PAIR_TAG, builtin_pair).
-define(PAIR_PROTO, 2).
-define(PAIR_CTOR, 3).

%% rt_js_types.Handle, also the JsVal object wire row
-define(HANDLE_TAG, js_cell).
-define(HANDLE_ID, 2).

%% rt_js_types.JsSlot: SObject
-define(SOBJECT_TAG, s_object).
-define(SOBJECT_KIND, 2).
-define(SOBJECT_PROTO, 3).
-define(SOBJECT_PROPS, 4).
-define(SOBJECT_SYMBOL_PROPS, 5).
-define(SOBJECT_ELEMENTS, 6).
-define(SOBJECT_EXTENSIBLE, 7).
-define(SOBJECT_ARITY, 7).

%% rt_js_types.JsSlot: SShapedObject. Slots is a plain tuple, 0-based Off
%% reads element(Off + 1, Slots).
-define(SSHAPED_TAG, s_shaped_object).
-define(SSHAPED_SID, 2).
-define(SSHAPED_PROTO, 3).
-define(SSHAPED_SLOTS, 4).
-define(SSHAPED_ARITY, 4).

%% Flat pdict overlay {s_shaped_object, Sid, P, X0, .., Xn-1}: the shaped
%% record with Slots spliced in place, so slot Off lives at Off + OVERLAY_OFF
%% and per-site ICs cache OffF = Off + OVERLAY_OFF.
-define(OVERLAY_OFF, ?SSHAPED_SLOTS).

%% rt_js_types.ShapeDesc
-define(SHAPE_TAG, shape_desc).
-define(SHAPE_ARITY_F, 2).
-define(SHAPE_OFFSETS, 3).
-define(SHAPE_TRANSITIONS, 4).
-define(SHAPE_ARITY, 4).

%% rt_js_types.ObjKind: KFunction
-define(KFN_TAG, k_function).
-define(KFN_CODE, 2).
-define(KFN_HOME, 3).
-define(KFN_FLAGS, 4).
-define(KFN_FIELDS_INIT, 5).
-define(KFN_CAPTURES, 6).
-define(KFN_SIMPLE, 7).
-define(KFN_ARITY, 7).

%% rt_js_types.ObjKind: KNative, ArrayObj, Ordinary
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

%% rt_js_types.FnFlags
-define(FNFLAGS_TAG, fn_flags).
-define(FNFLAGS_IS_CTOR, 2).
-define(FNFLAGS_IS_CLASS_CTOR, 3).
-define(FNFLAGS_IS_DERIVED, 4).
-define(FNFLAGS_IS_ARROW, 5).
-define(FNFLAGS_IS_METHOD, 6).
-define(FNFLAGS_IS_GEN, 7).
-define(FNFLAGS_IS_ASYNC, 8).
-define(FNFLAGS_ARITY, 8).

%% rt_js_types.Property
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

%% rt_js_types.PropertyKey / ObjectKey (props map keys)
-define(KEY_NAMED, named).
-define(KEY_INDEX, index).
-define(KEY_PRIVATE, private).
-define(OKEY_STRING, string_key).
-define(OKEY_SYMBOL, symbol_key).

%% rt_js_types.JsElements
-define(ELEMS_NONE, no_elements).
-define(ELEMS_DENSE, dense).
-define(ELEMS_SPARSE, sparse).

%% rt_js_call.Completion (built by the call ffi, matched by Gleam)
-define(COMPLETION_NORMAL, normal_completion).
-define(COMPLETION_THROW, throw_completion).

-endif.
