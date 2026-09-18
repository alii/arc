-module(arc_rt_layout_root_ffi).
-export([idx/1, tag/1, element_of/2, size_of/1, dyn/1, slots/1,
         compiled_fn_parts/1, direct_entry/3, is_plain_fn/1, plain_property/2,
         slot_at/2, slot_set/3, frame/4, is_js_number/1, is_inf/1, is_str/1,
         is_nullish/1, elem_at/2, elem_write_grow/3, native_token/1,
         named_plain/2, birth_plain/2, shaped_next/3]).

-compile({inline, [birth_plain/2]}).

-include("arc/rt/arc_rt_layout.hrl").

idx(<<"AGENT_SIZE">>) -> ?AGENT_SIZE;
idx(<<"AGENT_STORE">>) -> ?AGENT_STORE;
idx(<<"AGENT_REALM">>) -> ?AGENT_REALM;
idx(<<"AGENT_HOST_FNS">>) -> ?AGENT_HOST_FNS;
idx(<<"AGENT_REALMS">>) -> ?AGENT_REALMS;
idx(<<"STORE_CELLS">>) -> ?STORE_CELLS;
idx(<<"STORE_NEXT_ID">>) -> ?STORE_NEXT_ID;
idx(<<"STORE_PINNED_ROOTS">>) -> ?STORE_PINNED_ROOTS;
idx(<<"STORE_ALLOC_SINCE_GC">>) -> ?STORE_ALLOC_SINCE_GC;
idx(<<"STORE_PROP_SEQ">>) -> ?STORE_PROP_SEQ;
idx(<<"STORE_SHAPES">>) -> ?STORE_SHAPES;
idx(<<"STORE_NEXT_SHAPE">>) -> ?STORE_NEXT_SHAPE;
idx(<<"STORE_SIZE">>) -> ?STORE_SIZE;
idx(<<"STORE_ICS">>) -> ?STORE_ICS;
idx(<<"STORE_PLAIN_WRITE_PROTOS">>) -> ?STORE_PLAIN_WRITE_PROTOS;
idx(<<"STORE_GLOBAL_EPOCH">>) -> ?STORE_GLOBAL_EPOCH;
idx(<<"REALM_OBJECT">>) -> ?REALM_OBJECT;
idx(<<"REALM_FUNCTION">>) -> ?REALM_FUNCTION;
idx(<<"REALM_ARRAY">>) -> ?REALM_ARRAY;
idx(<<"REALM_STRING">>) -> ?REALM_STRING;
idx(<<"REALM_NUMBER">>) -> ?REALM_NUMBER;
idx(<<"REALM_ARRAY_ITER_PROTO">>) -> ?REALM_ARRAY_ITER_PROTO;
idx(<<"REALM_STRING_ITER_PROTO">>) -> ?REALM_STRING_ITER_PROTO;
idx(<<"REALM_MAP">>) -> ?REALM_MAP;
idx(<<"REALM_SET">>) -> ?REALM_SET;
idx(<<"REALM_MAP_ITER_PROTO">>) -> ?REALM_MAP_ITER_PROTO;
idx(<<"REALM_SET_ITER_PROTO">>) -> ?REALM_SET_ITER_PROTO;
idx(<<"REALM_GLOBAL">>) -> ?REALM_GLOBAL;
idx(<<"REALM_ID">>) -> ?REALM_ID;
idx(<<"REALM_SIZE">>) -> ?REALM_SIZE;
idx(<<"BUILTINPAIR_PROTO">>) -> ?BUILTINPAIR_PROTO;
idx(<<"BUILTINPAIR_CTOR">>) -> ?BUILTINPAIR_CTOR;
idx(<<"HANDLE_ID">>) -> ?HANDLE_ID;
idx(<<"SOBJECT_KIND">>) -> ?SOBJECT_KIND;
idx(<<"SOBJECT_PROTO">>) -> ?SOBJECT_PROTO;
idx(<<"SOBJECT_PROPS">>) -> ?SOBJECT_PROPS;
idx(<<"SOBJECT_SYMBOL_PROPS">>) -> ?SOBJECT_SYMBOL_PROPS;
idx(<<"SOBJECT_ELEMENTS">>) -> ?SOBJECT_ELEMENTS;
idx(<<"SOBJECT_EXTENSIBLE">>) -> ?SOBJECT_EXTENSIBLE;
idx(<<"SOBJECT_SIZE">>) -> ?SOBJECT_SIZE;
idx(<<"SSHAPEDOBJECT_SID">>) -> ?SSHAPEDOBJECT_SID;
idx(<<"SSHAPEDOBJECT_PROTO">>) -> ?SSHAPEDOBJECT_PROTO;
idx(<<"SSHAPEDOBJECT_SLOTS">>) -> ?SSHAPEDOBJECT_SLOTS;
idx(<<"SSHAPEDOBJECT_OFFSETS">>) -> ?SSHAPEDOBJECT_OFFSETS;
idx(<<"SSHAPEDOBJECT_SIZE">>) -> ?SSHAPEDOBJECT_SIZE;
idx(<<"SHAPE_SLOT_COUNT">>) -> ?SHAPE_SLOT_COUNT;
idx(<<"SHAPE_OFFSETS">>) -> ?SHAPE_OFFSETS;
idx(<<"SHAPE_TRANSITIONS">>) -> ?SHAPE_TRANSITIONS;
idx(<<"SHAPE_SIZE">>) -> ?SHAPE_SIZE;
idx(<<"COMPILEDFN_CODE">>) -> ?COMPILEDFN_CODE;
idx(<<"COMPILEDFN_HOME">>) -> ?COMPILEDFN_HOME;
idx(<<"COMPILEDFN_FLAGS">>) -> ?COMPILEDFN_FLAGS;
idx(<<"COMPILEDFN_FIELDS_INIT">>) -> ?COMPILEDFN_FIELDS_INIT;
idx(<<"COMPILEDFN_DIRECT_ENTRY">>) -> ?COMPILEDFN_DIRECT_ENTRY;
idx(<<"DIRECT_ENTRY_SIZE">>) -> ?DIRECT_ENTRY_SIZE;
idx(<<"COMPILEDFN_NAME">>) -> ?COMPILEDFN_NAME;
idx(<<"COMPILEDFN_LENGTH">>) -> ?COMPILEDFN_LENGTH;
idx(<<"COMPILEDFN_BIRTH">>) -> ?COMPILEDFN_BIRTH;
idx(<<"COMPILEDFN_SIZE">>) -> ?COMPILEDFN_SIZE;
idx(<<"BYTECODEFN_BIRTH">>) -> ?BYTECODEFN_BIRTH;
idx(<<"BIRTHPENDING_PROTOTYPE_PARENT">>) -> ?BIRTHPENDING_PROTOTYPE_PARENT;
idx(<<"BYTECODEFN_SIZE">>) -> ?BYTECODEFN_SIZE;
idx(<<"NATIVEFN_TOKEN">>) -> ?NATIVEFN_TOKEN;
idx(<<"NATIVEFN_NAME">>) -> ?NATIVEFN_NAME;
idx(<<"NATIVEFN_LENGTH">>) -> ?NATIVEFN_LENGTH;
idx(<<"NATIVEFN_CONSTRUCTIBLE">>) -> ?NATIVEFN_CONSTRUCTIBLE;
idx(<<"NATIVEFN_SIZE">>) -> ?NATIVEFN_SIZE;
idx(<<"ARRAYOBJ_LENGTH">>) -> ?ARRAYOBJ_LENGTH;
idx(<<"ARRAYOBJ_SIZE">>) -> ?ARRAYOBJ_SIZE;
idx(<<"ARGUMENTSOBJ_MAPPED">>) -> ?ARGUMENTSOBJ_MAPPED;
idx(<<"ARGUMENTSOBJ_SIZE">>) -> ?ARGUMENTSOBJ_SIZE;
idx(<<"STRINGOBJ_VALUE">>) -> ?STRINGOBJ_VALUE;
idx(<<"ARRAYITERATOR_TARGET">>) -> ?ARRAYITERATOR_TARGET;
idx(<<"ARRAYITERATOR_INDEX">>) -> ?ARRAYITERATOR_INDEX;
idx(<<"ARRAYITERATOR_KIND">>) -> ?ARRAYITERATOR_KIND;
idx(<<"ARRAYITERATOR_SIZE">>) -> ?ARRAYITERATOR_SIZE;
idx(<<"GENERATOROBJ_DATA">>) -> ?GENERATOROBJ_DATA;
idx(<<"GENERATOROBJ_SIZE">>) -> ?GENERATOROBJ_SIZE;
idx(<<"SBOX_VALUE">>) -> ?SBOX_VALUE;
idx(<<"FNFLAGS_IS_CTOR">>) -> ?FNFLAGS_IS_CTOR;
idx(<<"FNFLAGS_IS_CLASS_CTOR">>) -> ?FNFLAGS_IS_CLASS_CTOR;
idx(<<"FNFLAGS_IS_DERIVED">>) -> ?FNFLAGS_IS_DERIVED;
idx(<<"FNFLAGS_IS_ARROW">>) -> ?FNFLAGS_IS_ARROW;
idx(<<"FNFLAGS_IS_METHOD">>) -> ?FNFLAGS_IS_METHOD;
idx(<<"FNFLAGS_IS_GEN">>) -> ?FNFLAGS_IS_GEN;
idx(<<"FNFLAGS_IS_ASYNC">>) -> ?FNFLAGS_IS_ASYNC;
idx(<<"FNFLAGS_IS_STRICT">>) -> ?FNFLAGS_IS_STRICT;
idx(<<"FNFLAGS_SIZE">>) -> ?FNFLAGS_SIZE;
idx(<<"DATAPROPERTY_VALUE">>) -> ?DATAPROPERTY_VALUE;
idx(<<"DATAPROPERTY_WRITABLE">>) -> ?DATAPROPERTY_WRITABLE;
idx(<<"DATAPROPERTY_ENUMERABLE">>) -> ?DATAPROPERTY_ENUMERABLE;
idx(<<"DATAPROPERTY_CONFIGURABLE">>) -> ?DATAPROPERTY_CONFIGURABLE;
idx(<<"DATAPROPERTY_SEQ">>) -> ?DATAPROPERTY_SEQ;
idx(<<"DATAPROPERTY_SIZE">>) -> ?DATAPROPERTY_SIZE;
idx(<<"ACCESSORPROPERTY_GET">>) -> ?ACCESSORPROPERTY_GET;
idx(<<"ACCESSORPROPERTY_SET">>) -> ?ACCESSORPROPERTY_SET;
idx(<<"ACCESSORPROPERTY_SIZE">>) -> ?ACCESSORPROPERTY_SIZE;
idx(<<"LEXICAL_GLOBAL_VALUE">>) -> ?LEXICAL_GLOBAL_VALUE;
idx(<<"CELL_PROTO">>) -> ?CELL_PROTO;
idx(<<"MAX_ARRAY_INDEX">>) -> ?MAX_ARRAY_INDEX;
idx(<<"MAX_SAFE_INT">>) -> ?MAX_SAFE_INT;
idx(<<"MAX_DENSE_INDEX">>) -> ?MAX_DENSE_INDEX;
idx(<<"MAX_GAP">>) -> ?MAX_GAP.

tag(<<"AGENT_TAG">>) -> ?AGENT_TAG;
tag(<<"SOME">>) -> ?SOME;
tag(<<"NONE">>) -> ?NONE;
tag(<<"STORE_TAG">>) -> ?STORE_TAG;
tag(<<"REALM_TAG">>) -> ?REALM_TAG;
tag(<<"BUILTINPAIR_TAG">>) -> ?BUILTINPAIR_TAG;
tag(<<"HANDLE_TAG">>) -> ?HANDLE_TAG;
tag(<<"STR_TAG">>) -> ?STR_TAG;
tag(<<"SBOX_TAG">>) -> ?SBOX_TAG;
tag(<<"SOBJECT_TAG">>) -> ?SOBJECT_TAG;
tag(<<"SSHAPEDOBJECT_TAG">>) -> ?SSHAPEDOBJECT_TAG;
tag(<<"SHAPE_TAG">>) -> ?SHAPE_TAG;
tag(<<"STEP_RETURN">>) -> ?STEP_RETURN;
tag(<<"STEP_THROW">>) -> ?STEP_THROW;
tag(<<"STEP_YIELD">>) -> ?STEP_YIELD;
tag(<<"STEP_AWAIT">>) -> ?STEP_AWAIT;
tag(<<"RESUMECOMPILED_TAG">>) -> ?RESUMECOMPILED_TAG;
tag(<<"RESUMEFRAME_TAG">>) -> ?RESUMEFRAME_TAG;
tag(<<"COMPILEDFN_TAG">>) -> ?COMPILEDFN_TAG;
tag(<<"DIRECT_ENTRY_TAG">>) -> ?DIRECT_ENTRY_TAG;
tag(<<"BIRTH_SETTLED">>) -> ?BIRTH_SETTLED;
tag(<<"BIRTHPENDING_TAG">>) -> ?BIRTHPENDING_TAG;
tag(<<"BYTECODEFN_TAG">>) -> ?BYTECODEFN_TAG;
tag(<<"NATIVEFN_TAG">>) -> ?NATIVEFN_TAG;
tag(<<"ARRAYOBJ_TAG">>) -> ?ARRAYOBJ_TAG;
tag(<<"ARGUMENTSOBJ_TAG">>) -> ?ARGUMENTSOBJ_TAG;
tag(<<"ARRAYITERATOR_TAG">>) -> ?ARRAYITERATOR_TAG;
tag(<<"ARRAYITER_VALUES">>) -> ?ARRAYITER_VALUES;
tag(<<"GENERATOROBJ_TAG">>) -> ?GENERATOROBJ_TAG;
tag(<<"TOKEN_ARRAY_ITER_NEXT">>) -> ?TOKEN_ARRAY_ITER_NEXT;
tag(<<"TOKEN_GENERATOR_NEXT">>) -> ?TOKEN_GENERATOR_NEXT;
tag(<<"TOKEN_ARRAY_VALUES">>) -> ?TOKEN_ARRAY_VALUES;
tag(<<"TOKEN_STRING_ITER">>) -> ?TOKEN_STRING_ITER;
tag(<<"TOKEN_STRING_ITER_NEXT">>) -> ?TOKEN_STRING_ITER_NEXT;
tag(<<"TOKEN_MAP_ENTRIES">>) -> ?TOKEN_MAP_ENTRIES;
tag(<<"TOKEN_MAP_ITER_NEXT">>) -> ?TOKEN_MAP_ITER_NEXT;
tag(<<"TOKEN_SET_VALUES">>) -> ?TOKEN_SET_VALUES;
tag(<<"TOKEN_SET_ITER_NEXT">>) -> ?TOKEN_SET_ITER_NEXT;
tag(<<"TOKEN_RETURN_THIS">>) -> ?TOKEN_RETURN_THIS;
tag(<<"ITERATORN_TAG">>) -> ?ITERATORN_TAG;
tag(<<"VEC_TAG">>) -> ?VEC_TAG;
tag(<<"ORDINARY">>) -> ?ORDINARY;
tag(<<"PROXYOBJ_TAG">>) -> ?PROXYOBJ_TAG;
tag(<<"STRINGOBJ_TAG">>) -> ?STRINGOBJ_TAG;
tag(<<"FNFLAGS_TAG">>) -> ?FNFLAGS_TAG;
tag(<<"DATAPROPERTY_TAG">>) -> ?DATAPROPERTY_TAG;
tag(<<"ACCESSORPROPERTY_TAG">>) -> ?ACCESSORPROPERTY_TAG;
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
tag(<<"COMPLETION_THROW">>) -> ?COMPLETION_THROW;
tag(<<"STORE_FREE_CELL">>) -> ?STORE_FREE_CELL;
tag(<<"BOUNDFN_TAG">>) -> ?BOUNDFN_TAG;
tag(<<"TYPEDARRAYOBJ_TAG">>) -> ?TYPEDARRAYOBJ_TAG;
tag(<<"MODULENS_TAG">>) -> ?MODULENS_TAG;
tag(<<"MAPOBJ_TAG">>) -> ?MAPOBJ_TAG;
tag(<<"SETOBJ_TAG">>) -> ?SETOBJ_TAG;
tag(<<"GLOBALOBJ">>) -> ?GLOBALOBJ;
tag(<<"ITERATORRECORD_TAG">>) -> ?ITERATORRECORD_TAG;
tag(<<"LENGTH_KEY">>) -> ?LENGTH_KEY;
tag(<<"SYMBOL_ITERATOR">>) -> ?SYMBOL_ITERATOR;
tag(<<"IC_READ">>) -> ?IC_READ;
tag(<<"IC_CALL">>) -> ?IC_CALL;
tag(<<"IC_INIT">>) -> ?IC_INIT;
tag(<<"IC_GLOBAL">>) -> ?IC_GLOBAL;
tag(<<"IC_OFF">>) -> ?IC_OFF;
tag(<<"ICPLAIN_TAG">>) -> ?ICPLAIN_TAG;
tag(<<"ICOWN_TAG">>) -> ?ICOWN_TAG;
tag(<<"ICPRIM_TAG">>) -> ?ICPRIM_TAG.

element_of(N, T) -> element(N, T).

size_of(T) when is_tuple(T) -> tuple_size(T);
size_of(_) -> 0.

dyn(X) -> X.

slots(L) -> list_to_tuple(L).

compiled_fn_parts(?COMPILEDFN(Code, Home, Flags, FieldsInit, DirectEntry)) ->
    {Code, Home, Flags, FieldsInit, DirectEntry}.

direct_entry(Code, Arity, TakesThis) -> ?DIRECT_ENTRY(Code, Arity, TakesThis).

is_plain_fn(Flags) -> ?IS_PLAIN_FN(Flags).

plain_property(V, Seq) -> ?PLAIN_PROPERTY(V, Seq).

slot_at(Slots, Off) -> ?SLOT_AT(Slots, Off).

slot_set(Slots, Off, V) -> ?SLOT_SET(Slots, Off, V).

frame(This, Fn, Home, NewTarget) -> ?FRAME(This, Fn, Home, NewTarget).

is_js_number(V) -> ?IS_JS_NUMBER(V).

is_inf(V) -> ?IS_INF(V).

is_str(V) -> ?IS_STR(V).

is_nullish(V) -> ?IS_NULLISH(V).

elem_at(Els, Idx) -> ?ELEM_AT(Els, Idx).

elem_write_grow(Els, Idx, V) -> ?ELEM_WRITE_GROW(Els, Idx, V).

native_token(Cell) -> ?NATIVE_TOKEN(Cell).

named_plain(Kind, KeyBin) -> ?NAMED_KEY_IS_PLAIN(Kind, KeyBin, <<"length">>).

shaped_next(Shapes, Sid, KeyBin) -> ?SHAPED_NEXT(Shapes, Sid, KeyBin).

birth_plain(Birth, KeyBin) ->
    ?LAZY_KEY_IS_PLAIN(Birth, KeyBin, <<"length">>, <<"name">>, <<"prototype">>).
