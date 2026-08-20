-module(arc_rt_layout_root_ffi).
-export([idx/1, tag/1, element/2, tuple_size/1, dyn/1, slots/1]).

-include("arc_rt_layout.hrl").

%% Macro table keyed by name so the Gleam test asserts the header, not a
%% second copy of the numbers.
idx(<<"AGENT_ARITY">>) -> ?AGENT_ARITY;
idx(<<"AGENT_STORE">>) -> ?AGENT_STORE;
idx(<<"AGENT_REALM">>) -> ?AGENT_REALM;
idx(<<"AGENT_HOST_FNS">>) -> ?AGENT_HOST_FNS;
idx(<<"AGENT_REALMS">>) -> ?AGENT_REALMS;
idx(<<"STORE_DATA">>) -> ?STORE_DATA;
idx(<<"STORE_NEXT">>) -> ?STORE_NEXT;
idx(<<"STORE_PINNED_ROOTS">>) -> ?STORE_PINNED_ROOTS;
idx(<<"STORE_ALLOC">>) -> ?STORE_ALLOC;
idx(<<"STORE_PROP_SEQ">>) -> ?STORE_PROP_SEQ;
idx(<<"STORE_SHAPES">>) -> ?STORE_SHAPES;
idx(<<"STORE_NEXT_SHAPE">>) -> ?STORE_NEXT_SHAPE;
idx(<<"STORE_ARITY">>) -> ?STORE_ARITY;
idx(<<"STORE_ICS">>) -> ?STORE_ICS;
idx(<<"REALM_OBJECT">>) -> ?REALM_OBJECT;
idx(<<"REALM_FUNCTION">>) -> ?REALM_FUNCTION;
idx(<<"REALM_ARRAY">>) -> ?REALM_ARRAY;
idx(<<"REALM_STRING">>) -> ?REALM_STRING;
idx(<<"REALM_NUMBER">>) -> ?REALM_NUMBER;
idx(<<"REALM_GLOBAL">>) -> ?REALM_GLOBAL;
idx(<<"REALM_ID">>) -> ?REALM_ID;
idx(<<"REALM_ARITY">>) -> ?REALM_ARITY;
idx(<<"PAIR_PROTO">>) -> ?PAIR_PROTO;
idx(<<"PAIR_CTOR">>) -> ?PAIR_CTOR;
idx(<<"HANDLE_ID">>) -> ?HANDLE_ID;
idx(<<"SOBJECT_KIND">>) -> ?SOBJECT_KIND;
idx(<<"SOBJECT_PROTO">>) -> ?SOBJECT_PROTO;
idx(<<"SOBJECT_PROPS">>) -> ?SOBJECT_PROPS;
idx(<<"SOBJECT_SYMBOL_PROPS">>) -> ?SOBJECT_SYMBOL_PROPS;
idx(<<"SOBJECT_ELEMENTS">>) -> ?SOBJECT_ELEMENTS;
idx(<<"SOBJECT_EXTENSIBLE">>) -> ?SOBJECT_EXTENSIBLE;
idx(<<"SOBJECT_ARITY">>) -> ?SOBJECT_ARITY;
idx(<<"SSHAPED_SID">>) -> ?SSHAPED_SID;
idx(<<"SSHAPED_PROTO">>) -> ?SSHAPED_PROTO;
idx(<<"SSHAPED_SLOTS">>) -> ?SSHAPED_SLOTS;
idx(<<"SSHAPED_ARITY">>) -> ?SSHAPED_ARITY;
idx(<<"SHAPE_ARITY_F">>) -> ?SHAPE_ARITY_F;
idx(<<"SHAPE_OFFSETS">>) -> ?SHAPE_OFFSETS;
idx(<<"SHAPE_TRANSITIONS">>) -> ?SHAPE_TRANSITIONS;
idx(<<"SHAPE_ARITY">>) -> ?SHAPE_ARITY;
idx(<<"KFN_CODE">>) -> ?KFN_CODE;
idx(<<"KFN_HOME">>) -> ?KFN_HOME;
idx(<<"KFN_FLAGS">>) -> ?KFN_FLAGS;
idx(<<"KFN_FIELDS_INIT">>) -> ?KFN_FIELDS_INIT;
idx(<<"KFN_SIMPLE">>) -> ?KFN_SIMPLE;
idx(<<"KFN_ARITY">>) -> ?KFN_ARITY;
idx(<<"KNATIVE_TOKEN">>) -> ?KNATIVE_TOKEN;
idx(<<"KNATIVE_NAME">>) -> ?KNATIVE_NAME;
idx(<<"KNATIVE_LENGTH">>) -> ?KNATIVE_LENGTH;
idx(<<"KNATIVE_CONSTRUCTIBLE">>) -> ?KNATIVE_CONSTRUCTIBLE;
idx(<<"KNATIVE_ARITY">>) -> ?KNATIVE_ARITY;
idx(<<"ARRAYOBJ_LENGTH">>) -> ?ARRAYOBJ_LENGTH;
idx(<<"ARRAYOBJ_ARITY">>) -> ?ARRAYOBJ_ARITY;
idx(<<"ARGUMENTSOBJ_MAPPED">>) -> ?ARGUMENTSOBJ_MAPPED;
idx(<<"ARGUMENTSOBJ_ARITY">>) -> ?ARGUMENTSOBJ_ARITY;
idx(<<"SBOX_VALUE">>) -> ?SBOX_VALUE;
idx(<<"FNFLAGS_IS_CTOR">>) -> ?FNFLAGS_IS_CTOR;
idx(<<"FNFLAGS_IS_CLASS_CTOR">>) -> ?FNFLAGS_IS_CLASS_CTOR;
idx(<<"FNFLAGS_IS_DERIVED">>) -> ?FNFLAGS_IS_DERIVED;
idx(<<"FNFLAGS_IS_ARROW">>) -> ?FNFLAGS_IS_ARROW;
idx(<<"FNFLAGS_IS_METHOD">>) -> ?FNFLAGS_IS_METHOD;
idx(<<"FNFLAGS_IS_GEN">>) -> ?FNFLAGS_IS_GEN;
idx(<<"FNFLAGS_IS_ASYNC">>) -> ?FNFLAGS_IS_ASYNC;
idx(<<"FNFLAGS_IS_STRICT">>) -> ?FNFLAGS_IS_STRICT;
idx(<<"FNFLAGS_ARITY">>) -> ?FNFLAGS_ARITY;
idx(<<"DATAPROP_VALUE">>) -> ?DATAPROP_VALUE;
idx(<<"DATAPROP_WRITABLE">>) -> ?DATAPROP_WRITABLE;
idx(<<"DATAPROP_ENUMERABLE">>) -> ?DATAPROP_ENUMERABLE;
idx(<<"DATAPROP_CONFIGURABLE">>) -> ?DATAPROP_CONFIGURABLE;
idx(<<"DATAPROP_SEQ">>) -> ?DATAPROP_SEQ;
idx(<<"DATAPROP_ARITY">>) -> ?DATAPROP_ARITY;
idx(<<"ACCESSORPROP_GET">>) -> ?ACCESSORPROP_GET;
idx(<<"ACCESSORPROP_SET">>) -> ?ACCESSORPROP_SET;
idx(<<"ACCESSORPROP_ARITY">>) -> ?ACCESSORPROP_ARITY.

tag(<<"AGENT_TAG">>) -> ?AGENT_TAG;
tag(<<"SOME">>) -> ?SOME;
tag(<<"NONE">>) -> ?NONE;
tag(<<"STORE_TAG">>) -> ?STORE_TAG;
tag(<<"REALM_TAG">>) -> ?REALM_TAG;
tag(<<"PAIR_TAG">>) -> ?PAIR_TAG;
tag(<<"HANDLE_TAG">>) -> ?HANDLE_TAG;
tag(<<"SBOX_TAG">>) -> ?SBOX_TAG;
tag(<<"SOBJECT_TAG">>) -> ?SOBJECT_TAG;
tag(<<"SSHAPED_TAG">>) -> ?SSHAPED_TAG;
tag(<<"SHAPE_TAG">>) -> ?SHAPE_TAG;
tag(<<"STEP_RETURN">>) -> ?STEP_RETURN;
tag(<<"STEP_THROW">>) -> ?STEP_THROW;
tag(<<"STEP_YIELD">>) -> ?STEP_YIELD;
tag(<<"STEP_AWAIT">>) -> ?STEP_AWAIT;
tag(<<"RESUME_COMPILED_TAG">>) -> ?RESUME_COMPILED_TAG;
tag(<<"RESUME_FRAME_TAG">>) -> ?RESUME_FRAME_TAG;
tag(<<"KFN_TAG">>) -> ?KFN_TAG;
tag(<<"KBYTECODE_TAG">>) -> ?KBYTECODE_TAG;
tag(<<"KNATIVE_TAG">>) -> ?KNATIVE_TAG;
tag(<<"ARRAYOBJ_TAG">>) -> ?ARRAYOBJ_TAG;
tag(<<"ARGUMENTSOBJ_TAG">>) -> ?ARGUMENTSOBJ_TAG;
tag(<<"ORDINARY">>) -> ?ORDINARY;
tag(<<"PROXYOBJ_TAG">>) -> ?PROXYOBJ_TAG;
tag(<<"FNFLAGS_TAG">>) -> ?FNFLAGS_TAG;
tag(<<"DATAPROP_TAG">>) -> ?DATAPROP_TAG;
tag(<<"ACCESSORPROP_TAG">>) -> ?ACCESSORPROP_TAG;
tag(<<"KEY_NAMED">>) -> ?KEY_NAMED;
tag(<<"KEY_INDEX">>) -> ?KEY_INDEX;
tag(<<"KEY_PRIVATE">>) -> ?KEY_PRIVATE;
tag(<<"OKEY_STRING">>) -> ?OKEY_STRING;
tag(<<"OKEY_SYMBOL">>) -> ?OKEY_SYMBOL;
tag(<<"ELEMS_NONE">>) -> ?ELEMS_NONE;
tag(<<"ELEMS_DENSE">>) -> ?ELEMS_DENSE;
tag(<<"ELEMS_SPARSE">>) -> ?ELEMS_SPARSE;
tag(<<"ELEMS_HOLE">>) -> ?ELEMS_HOLE;
tag(<<"COMPLETION_NORMAL">>) -> ?COMPLETION_NORMAL;
tag(<<"COMPLETION_THROW">>) -> ?COMPLETION_THROW.

element(N, T) -> erlang:element(N, T).

tuple_size(T) when is_tuple(T) -> erlang:tuple_size(T);
tuple_size(_) -> 0.

dyn(X) -> X.

%% ShapeSlots wire: a plain tuple.
slots(L) -> list_to_tuple(L).

