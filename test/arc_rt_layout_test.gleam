// arc_rt_layout.hrl indices must match the gleam records

import arc/bytecode/binop
import arc/bytecode/key.{Index, Named, Private, max_array_index}
import arc/internal/ordered_entries
import arc/internal/tree_array
import arc/interp/kernel
import arc/rt/arena
import arc/rt/bytecode.{type EnvTuple, type FuncTemplate}
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/lang as rt_lang
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type CompiledCode, type FnFlags, type JsVal, type ShapeSlots,
  AccessorProperty, ArgumentsObj, ArrayObj, BirthPending, BirthSettled, BoundFn,
  BytecodeFn, CompiledFn, DataProperty, Dense, DirectEntry, FnFlags, GlobalObj,
  Handle, IteratorRecord, KHandle, MapObj, ModuleNamespace, NativeFn, NoElements,
  Ordinary, ProxyObj, ResumeCompiled, ResumeFrame, ReturnThis, SBox, SObject,
  SShapedObject, SetObj, ShapeDesc, Sparse, StepAwait, StepReturn, StepThrow,
  StepYield, Store, StringKey, StringObj, SymbolKey, TypedArrayObj, mk_int,
  plain_object,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import rt_helpers

@external(erlang, "arc_rt_layout_root_ffi", "idx")
fn idx(name: String) -> Int

@external(erlang, "arc_rt_layout_root_ffi", "tag")
fn tag(name: String) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "element_of")
fn element(n: Int, of: Dynamic) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "size_of")
fn tuple_size(of: Dynamic) -> Int

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn dyn(x: a) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn dummy_code(label: String) -> CompiledCode

@external(erlang, "arc_rt_layout_root_ffi", "slots")
fn slots(vals: List(JsVal)) -> ShapeSlots

@external(erlang, "arc_rt_layout_root_ffi", "compiled_fn_parts")
fn compiled_fn_parts(kind: types.ObjKind) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "direct_entry")
fn direct_entry(
  code: CompiledCode,
  arity: Int,
  takes_this takes_this: Bool,
) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "is_plain_fn")
fn is_plain_fn(flags: FnFlags) -> Bool

@external(erlang, "arc_rt_layout_root_ffi", "plain_property")
fn plain_property(v: JsVal, seq: Int) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "slot_at")
fn slot_at(slots: ShapeSlots, off: Int) -> JsVal

@external(erlang, "arc_rt_layout_root_ffi", "slot_set")
fn slot_set(slots: ShapeSlots, off: Int, v: JsVal) -> ShapeSlots

@external(erlang, "arc_rt_layout_root_ffi", "frame")
fn frame_macro(this: JsVal, f: JsVal, home: JsVal, nt: JsVal) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "is_js_number")
fn is_js_number(v: JsVal) -> Bool

@external(erlang, "arc_rt_layout_root_ffi", "is_inf")
fn is_inf(v: JsVal) -> Bool

@external(erlang, "arc_rt_layout_root_ffi", "is_str")
fn is_str(v: JsVal) -> Bool

@external(erlang, "arc_rt_layout_root_ffi", "is_nullish")
fn is_nullish(v: JsVal) -> Bool

@external(erlang, "arc_rt_layout_root_ffi", "elem_at")
fn elem_at(els: types.JsElements, idx: Int) -> JsVal

@external(erlang, "arc_rt_layout_root_ffi", "elem_write_grow")
fn elem_write_grow(els: types.JsElements, idx: Int, v: JsVal) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "native_token")
fn native_token(cell: types.Cell) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "named_plain")
fn named_plain(kind: types.ObjKind, key: BitArray) -> Bool

@external(erlang, "arc_rt_layout_root_ffi", "birth_plain")
fn birth_plain(birth: types.FnBirth, key: BitArray) -> Bool

@external(erlang, "arc_rt_layout_root_ffi", "shaped_next")
fn shaped_next(
  shapes: dict.Dict(Int, types.ShapeDesc),
  sid: Int,
  key: BitArray,
) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn template(label: String) -> FuncTemplate

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn env(vals: List(JsVal)) -> EnvTuple

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn sm_fn(label: String) -> types.SmFn

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn loc(label: String) -> types.Loc

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn frame(label: String) -> bytecode.SuspendedFrame

fn at(record: a, name: String) -> Dynamic {
  element(idx(name), dyn(record))
}

fn tag_of(record: a) -> Dynamic {
  element(1, dyn(record))
}

fn size_of(record: a) -> Int {
  tuple_size(dyn(record))
}

fn seeded() -> Agent {
  rt_helpers.agent()
}

fn no_flags() -> FnFlags {
  FnFlags(
    is_constructor: False,
    is_class_constructor: False,
    is_derived_constructor: False,
    is_arrow: False,
    is_method: False,
    is_generator: False,
    is_async: False,
    is_strict: False,
  )
}

pub fn agent_test() {
  let st = seeded()
  assert tag_of(st) == tag("AGENT_TAG")
  assert size_of(st) == idx("AGENT_SIZE")
  assert at(st, "AGENT_STORE") == dyn(st.store)
  assert at(st, "AGENT_REALM") == dyn(st.realm)
  assert at(st, "AGENT_HOST_FNS") == dyn(st.host_fns)
  assert at(st, "AGENT_REALMS") == dyn(st.realms)
  assert tag_of(at(st, "AGENT_STORE")) == tag("STORE_TAG")
  assert tag_of(at(st, "AGENT_REALM")) == tag("REALM_TAG")
  assert dict.get(st.realms, st.realm.id) == Ok(st.realm)
}

pub fn store_test() {
  let base = rt_store.new()
  let desc =
    ShapeDesc(
      slot_count: 1,
      offsets: dict.from_list([#(<<"k":utf8>>, 0)]),
      transitions: dict.new(),
    )
  let store =
    Store(
      ..base,
      cells: arena.set(3, SBox(types.mk_string("d")), base.cells),
      next_id: 13,
      pinned_roots: set.from_list([3]),
      alloc_since_gc: 14,
      prop_seq: 16,
      shapes: dict.from_list([#(7, desc)]),
      next_shape: 15,
      ics: dict.from_list([
        #(1, types.IcRead(<<"k":utf8>>, dict.from_list([#(7, 0)]))),
      ]),
    )
  assert tag_of(store) == tag("STORE_TAG")
  assert size_of(store) == idx("STORE_SIZE")
  assert at(store, "STORE_CELLS") == dyn(store.cells)
  assert at(store, "STORE_NEXT_ID") == dyn(13)
  assert at(store, "STORE_PINNED_ROOTS") == dyn(store.pinned_roots)
  assert at(store, "STORE_ALLOC_SINCE_GC") == dyn(14)
  assert at(store, "STORE_PROP_SEQ") == dyn(16)
  assert at(store, "STORE_SHAPES") == dyn(store.shapes)
  assert at(store, "STORE_NEXT_SHAPE") == dyn(15)
  assert at(store, "STORE_ICS") == dyn(store.ics)
  assert at(store, "STORE_PLAIN_WRITE_PROTOS") == dyn(store.plain_write_protos)
  assert at(store, "STORE_GLOBAL_EPOCH") == dyn(store.global_epoch)
  assert dyn(arena.get(3, arena.free(3, store.cells))) == tag("STORE_FREE_CELL")
}

pub fn realm_test() {
  let realm = seeded().realm
  assert tag_of(realm) == tag("REALM_TAG")
  assert size_of(realm) == idx("REALM_SIZE")
  assert at(realm, "REALM_OBJECT") == dyn(realm.object)
  assert at(realm, "REALM_FUNCTION") == dyn(realm.function)
  assert at(realm, "REALM_ARRAY") == dyn(realm.array)
  assert at(realm, "REALM_STRING") == dyn(realm.string)
  assert at(realm, "REALM_NUMBER") == dyn(realm.number)
  assert at(realm, "REALM_ARRAY_ITER_PROTO") == dyn(realm.array_iter_proto)
  assert at(realm, "REALM_STRING_ITER_PROTO") == dyn(realm.string_iter_proto)
  assert at(realm, "REALM_MAP") == dyn(realm.map)
  assert at(realm, "REALM_SET") == dyn(realm.set)
  assert at(realm, "REALM_MAP_ITER_PROTO") == dyn(realm.map_iter_proto)
  assert at(realm, "REALM_SET_ITER_PROTO") == dyn(realm.set_iter_proto)
  assert at(realm, "REALM_GLOBAL") == dyn(realm.global_object)
  assert at(realm, "REALM_ID") == dyn(realm.id)
  assert realm.id == 0
  assert realm.object != realm.function
  assert realm.function != realm.array
  let pair = realm.object
  assert tag_of(pair) == tag("BUILTINPAIR_TAG")
  assert size_of(pair) == 3
  assert at(pair, "BUILTINPAIR_PROTO") == dyn(pair.prototype)
  assert at(pair, "BUILTINPAIR_CTOR") == dyn(pair.constructor)
  assert pair.prototype != pair.constructor
  let v = types.mk_string("g")
  assert at(types.Let(v), "LEXICAL_GLOBAL_VALUE") == dyn(v)
  assert at(types.Const(v), "LEXICAL_GLOBAL_VALUE") == dyn(v)
}

pub fn jsval_predicates_test() {
  let str = types.mk_string("s")
  let wide = types.mk_string("h\u{e9}")
  assert tag_of(wide) == tag("STR_TAG")
  assert is_str(str)
  assert is_str(wide)
  assert !is_str(types.mk_int(1))
  assert !is_str(types.mk_undefined())
  let nums = [
    types.mk_int(1),
    types.mk_number(types.JFloat(1.5)),
    types.mk_number(types.JNan),
    types.mk_number(types.JPosInf),
    types.mk_number(types.JNegInf),
  ]
  assert list.all(nums, is_js_number)
  assert is_inf(types.mk_number(types.JPosInf))
  assert is_inf(types.mk_number(types.JNegInf))
  assert !is_inf(types.mk_number(types.JNan))
  assert !is_inf(types.mk_int(1))
  assert !is_js_number(str)
  assert !is_js_number(types.mk_bigint(1))
  assert !is_js_number(types.mk_undefined())
  assert is_nullish(types.mk_undefined())
  assert is_nullish(types.mk_null())
  assert !is_nullish(types.mk_int(0))
  assert !is_nullish(str)
}

pub fn constants_test() {
  assert idx("MAX_ARRAY_INDEX") == max_array_index
  assert idx("MAX_SAFE_INT") == limits.max_safe_integer
  assert idx("MAX_DENSE_INDEX") == limits.max_dense_index
}

pub fn handle_test() {
  let h = Handle(4242)
  assert tag_of(h) == tag("HANDLE_TAG")
  assert size_of(h) == 2
  assert at(h, "HANDLE_ID") == dyn(4242)
  assert dyn(types.mk_object(h)) == dyn(h)
  let assert KHandle(back) = types.classify(types.mk_object(h))
  assert back == h
  assert dyn(Some(h)) == dyn(#(tag("SOME"), h))
  assert dyn(None) == tag("NONE")
}

pub fn sobject_test() {
  let proto = Handle(1)
  let vx = types.mk_string("vx")
  let props =
    dict.from_list([
      #(
        Named("x"),
        DataProperty(
          value: vx,
          writable: True,
          enumerable: True,
          configurable: True,
          seq: 0,
        ),
      ),
    ])
  let symbol_props = [
    #(
      types.symbol_iterator,
      DataProperty(
        value: vx,
        writable: False,
        enumerable: False,
        configurable: False,
        seq: 1,
      ),
    ),
  ]
  let elements = Sparse(dict.from_list([#(0, types.mk_string("e0"))]))
  let obj =
    SObject(
      kind: ArrayObj(9),
      proto: Some(proto),
      props:,
      symbol_props:,
      elements:,
      extensible: False,
    )
  assert tag_of(obj) == tag("SOBJECT_TAG")
  assert size_of(obj) == idx("SOBJECT_SIZE")
  assert at(obj, "SOBJECT_KIND") == dyn(ArrayObj(9))
  assert at(obj, "SOBJECT_PROTO") == dyn(Some(proto))
  assert element(2, at(obj, "SOBJECT_PROTO")) == dyn(proto)
  assert at(obj, "SOBJECT_PROPS") == dyn(props)
  assert at(obj, "SOBJECT_SYMBOL_PROPS") == dyn(symbol_props)
  assert at(obj, "SOBJECT_ELEMENTS") == dyn(elements)
  assert at(obj, "SOBJECT_EXTENSIBLE") == dyn(False)
  assert dyn(Ordinary) == tag("ORDINARY")
  let kind = ArrayObj(9)
  assert tag_of(kind) == tag("ARRAYOBJ_TAG")
  assert size_of(kind) == idx("ARRAYOBJ_SIZE")
  assert at(kind, "ARRAYOBJ_LENGTH") == dyn(9)
  let args = ArgumentsObj(length: 2, mapped: None)
  assert tag_of(args) == tag("ARGUMENTSOBJ_TAG")
  assert size_of(args) == idx("ARGUMENTSOBJ_SIZE")
  assert at(args, "ARGUMENTSOBJ_MAPPED") == dyn(None)
  assert tag_of(ProxyObj(target: proto, handler: proto, revoked: False))
    == tag("PROXYOBJ_TAG")
  let wrapper = StringObj("s")
  assert tag_of(wrapper) == tag("STRINGOBJ_TAG")
  assert at(wrapper, "STRINGOBJ_VALUE") == dyn("s")
  assert dyn(GlobalObj) == tag("GLOBALOBJ")
  assert tag_of(BoundFn(target: proto, bound_this: vx, bound_args: []))
    == tag("BOUNDFN_TAG")
  let view =
    TypedArrayObj(
      buffer: proto,
      elem_kind: types.NumKind(types.Uint8Kind),
      byte_offset: 0,
      length: None,
    )
  assert tag_of(view) == tag("TYPEDARRAYOBJ_TAG")
  assert tag_of(ModuleNamespace(dict.new())) == tag("MODULENS_TAG")
  assert tag_of(MapObj(ordered_entries.new())) == tag("MAPOBJ_TAG")
  assert tag_of(SetObj(ordered_entries.new())) == tag("SETOBJ_TAG")
  let box = SBox(types.mk_string("b"))
  assert tag_of(box) == tag("SBOX_TAG")
  assert at(box, "SBOX_VALUE") == dyn(types.mk_string("b"))
}

pub fn keys_and_elements_test() {
  assert tag_of(Named("x")) == tag("KEY_NAMED")
  assert dyn(Named("length")) == tag("LENGTH_KEY")
  assert element(2, dyn(Named("x"))) == dyn("x")
  assert tag_of(Index(5)) == tag("KEY_INDEX")
  assert element(2, dyn(Index(5))) == dyn(5)
  assert tag_of(Private("#p")) == tag("KEY_PRIVATE")
  assert tag_of(StringKey(Named("x"))) == tag("OKEY_STRING")
  assert element(2, dyn(StringKey(Named("x")))) == dyn(Named("x"))
  assert tag_of(SymbolKey(types.symbol_iterator)) == tag("OKEY_SYMBOL")
  assert dyn(types.symbol_iterator) == tag("SYMBOL_ITERATOR")
  assert dyn(NoElements) == tag("ELEMS_NONE")
  let arr = tree_array.from_list([types.mk_string("a")])
  assert tag_of(Dense(arr)) == tag("ELEMS_DENSE")
  let deep = tree_array.from_list(list.repeat(types.mk_int(0), 65))
  assert tag_of(deep) == tag("VEC_TAG")
  assert element(2, dyn(Dense(arr))) == dyn(arr)
  let sparse = dict.from_list([#(0, types.mk_string("s"))])
  assert tag_of(Sparse(sparse)) == tag("ELEMS_SPARSE")
  assert element(2, dyn(Sparse(sparse))) == dyn(sparse)
  assert dyn(types.mk_hole()) == tag("ELEMS_HOLE")
}

pub fn kernel_macros_test() {
  let a = types.mk_string("a")
  let hole = types.mk_hole()
  let dense = Dense(tree_array.from_list([a]))
  let sparse = Sparse(dict.from_list([#(3, a)]))
  assert elem_at(dense, 0) == a
  assert elem_at(dense, 1) == hole
  assert elem_at(sparse, 3) == a
  assert elem_at(sparse, 0) == hole
  assert elem_at(NoElements, 0) == hole
  assert elem_write_grow(dense, 1, a)
    == dyn(Dense(tree_array.from_list([a, a])))
  assert elem_write_grow(dense, idx("MAX_GAP") + 2, a) == dyn(Miss)
  assert elem_write_grow(NoElements, 0, a)
    == dyn(Dense(tree_array.from_list([a])))
  assert elem_write_grow(sparse, 9, a)
    == dyn(Sparse(dict.from_list([#(3, a), #(9, a)])))
  let native =
    SObject(
      kind: NativeFn(
        token: ReturnThis,
        name: "n",
        length: 0,
        constructible: False,
      ),
      proto: None,
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    )
  assert native_token(native) == dyn(ReturnThis)
  assert native_token(SObject(..native, kind: Ordinary)) == tag("NONE")
  assert named_plain(Ordinary, <<"length">>)
  assert !named_plain(ArrayObj(0), <<"length">>)
  assert named_plain(ArrayObj(0), <<"x">>)
  assert birth_plain(BirthSettled, <<"name">>)
  assert !birth_plain(BirthPending(None), <<"name">>)
  assert !birth_plain(BirthPending(None), <<"length">>)
  assert birth_plain(BirthPending(None), <<"prototype">>)
  assert !birth_plain(BirthPending(Some(Handle(1))), <<"prototype">>)
  assert birth_plain(BirthPending(Some(Handle(1))), <<"x">>)
  let k = <<"k">>
  let to =
    ShapeDesc(
      slot_count: 1,
      offsets: dict.from_list([#(k, 0)]),
      transitions: dict.new(),
    )
  let from =
    ShapeDesc(
      slot_count: 0,
      offsets: dict.new(),
      transitions: dict.from_list([#(k, 9)]),
    )
  let shapes = dict.from_list([#(7, from), #(9, to)])
  assert shaped_next(shapes, 7, k) == dyn(#(9, to.offsets))
  assert shaped_next(shapes, 7, <<"z">>) == dyn(Miss)
  assert shaped_next(shapes, 3, k) == dyn(Miss)
}

pub fn sshaped_object_test() {
  let s0 = types.mk_string("s0")
  let s1 = types.mk_string("s1")
  let sl = slots([s0, s1, types.mk_string("s2")])
  let offs = dict.from_list([#(<<"k":utf8>>, 0)])
  let obj =
    SShapedObject(
      shape_id: 21,
      proto: Some(Handle(2)),
      slots: sl,
      offsets: offs,
    )
  assert tag_of(obj) == tag("SSHAPEDOBJECT_TAG")
  assert size_of(obj) == idx("SSHAPEDOBJECT_SIZE")
  assert at(obj, "SSHAPEDOBJECT_SID") == dyn(21)
  assert at(obj, "SSHAPEDOBJECT_PROTO") == dyn(Some(Handle(2)))
  assert at(obj, "SSHAPEDOBJECT_SLOTS") == dyn(sl)
  assert at(obj, "SSHAPEDOBJECT_OFFSETS") == dyn(offs)
  assert idx("CELL_PROTO") == idx("SSHAPEDOBJECT_PROTO")
  assert idx("CELL_PROTO") == idx("SOBJECT_PROTO")
  assert tuple_size(dyn(sl)) == 3
  assert element(1, dyn(sl)) == dyn(s0)
  assert element(2, dyn(sl)) == dyn(s1)
  assert rt_obj.shape_slots_get(sl, 1) == s1
  assert slot_at(sl, 1) == s1
  assert slot_set(sl, 0, s1) == rt_obj.shape_slots_set(sl, 0, s1)
}

pub fn shape_desc_test() {
  let desc =
    ShapeDesc(
      slot_count: 2,
      offsets: dict.from_list([#(<<"a":utf8>>, 0), #(<<"b":utf8>>, 1)]),
      transitions: dict.from_list([#(<<"c":utf8>>, 9)]),
    )
  assert tag_of(desc) == tag("SHAPE_TAG")
  assert size_of(desc) == idx("SHAPE_SIZE")
  assert at(desc, "SHAPE_SLOT_COUNT") == dyn(2)
  assert at(desc, "SHAPE_OFFSETS") == dyn(desc.offsets)
  assert at(desc, "SHAPE_TRANSITIONS") == dyn(desc.transitions)
}

pub fn fn_flags_test() {
  let base = no_flags()
  let names = [
    "FNFLAGS_IS_CTOR", "FNFLAGS_IS_CLASS_CTOR", "FNFLAGS_IS_DERIVED",
    "FNFLAGS_IS_ARROW", "FNFLAGS_IS_METHOD", "FNFLAGS_IS_GEN",
    "FNFLAGS_IS_ASYNC", "FNFLAGS_IS_STRICT",
  ]
  let one_hot = [
    #("FNFLAGS_IS_CTOR", FnFlags(..base, is_constructor: True)),
    #("FNFLAGS_IS_CLASS_CTOR", FnFlags(..base, is_class_constructor: True)),
    #("FNFLAGS_IS_DERIVED", FnFlags(..base, is_derived_constructor: True)),
    #("FNFLAGS_IS_ARROW", FnFlags(..base, is_arrow: True)),
    #("FNFLAGS_IS_METHOD", FnFlags(..base, is_method: True)),
    #("FNFLAGS_IS_GEN", FnFlags(..base, is_generator: True)),
    #("FNFLAGS_IS_ASYNC", FnFlags(..base, is_async: True)),
    #("FNFLAGS_IS_STRICT", FnFlags(..base, is_strict: True)),
  ]
  assert list.length(one_hot) == list.length(names)
  use #(set_name, flags) <- list.each(one_hot)
  assert tag_of(flags) == tag("FNFLAGS_TAG")
  assert size_of(flags) == idx("FNFLAGS_SIZE")
  use name <- list.each(names)
  assert at(flags, name) == dyn(name == set_name)
}

pub fn is_plain_fn_test() {
  let base = no_flags()
  assert is_plain_fn(base)
  assert is_plain_fn(FnFlags(..base, is_constructor: True, is_strict: True))
  assert is_plain_fn(FnFlags(..base, is_arrow: True, is_method: True))
  assert !is_plain_fn(FnFlags(..base, is_class_constructor: True))
  assert !is_plain_fn(FnFlags(..base, is_generator: True))
  assert !is_plain_fn(FnFlags(..base, is_async: True))
}

pub fn compiled_fn_test() {
  let code = dummy_code("code")
  let code_s = dummy_code("code_s")
  let flags = FnFlags(..no_flags(), is_arrow: True)
  let compiled =
    CompiledFn(
      code:,
      home_object: Some(Handle(30)),
      flags:,
      fields_init: Some(Handle(31)),
      direct_entry: Some(DirectEntry(code_s, 2, True)),
      name: "nm",
      length: 2,
      birth: BirthPending(Some(Handle(32))),
    )
  assert tag_of(compiled) == tag("COMPILEDFN_TAG")
  assert size_of(compiled) == idx("COMPILEDFN_SIZE")
  assert at(compiled, "COMPILEDFN_CODE") == dyn(code)
  assert at(compiled, "COMPILEDFN_HOME") == dyn(Some(Handle(30)))
  assert at(compiled, "COMPILEDFN_FLAGS") == dyn(flags)
  assert at(compiled, "COMPILEDFN_FIELDS_INIT") == dyn(Some(Handle(31)))
  assert at(compiled, "COMPILEDFN_NAME") == dyn("nm")
  assert at(compiled, "COMPILEDFN_LENGTH") == dyn(2)
  let birth = at(compiled, "COMPILEDFN_BIRTH")
  assert birth == dyn(BirthPending(Some(Handle(32))))
  assert tag_of(birth) == tag("BIRTHPENDING_TAG")
  assert at(birth, "BIRTHPENDING_PROTOTYPE_PARENT") == dyn(Some(Handle(32)))
  let entry = at(compiled, "COMPILEDFN_DIRECT_ENTRY")
  assert tag_of(entry) == tag("SOME")
  let inner = element(2, entry)
  assert tag_of(inner) == tag("DIRECT_ENTRY_TAG")
  assert tuple_size(inner) == idx("DIRECT_ENTRY_SIZE")
  assert element(2, inner) == dyn(code_s)
  assert element(3, inner) == dyn(2)
  assert element(4, inner) == dyn(True)
  assert inner == direct_entry(code_s, 2, takes_this: True)
  assert inner == dyn(DirectEntry(code_s, 2, True))
  assert compiled_fn_parts(compiled)
    == dyn(#(
      code,
      Some(Handle(30)),
      flags,
      Some(Handle(31)),
      Some(DirectEntry(code_s, 2, True)),
    ))
  let bare =
    CompiledFn(
      code:,
      home_object: None,
      flags:,
      fields_init: None,
      direct_entry: None,
      name: "",
      length: 0,
      birth: BirthSettled,
    )
  assert at(bare, "COMPILEDFN_HOME") == tag("NONE")
  assert at(bare, "COMPILEDFN_FIELDS_INIT") == tag("NONE")
  assert at(bare, "COMPILEDFN_DIRECT_ENTRY") == tag("NONE")
  assert at(bare, "COMPILEDFN_BIRTH") == tag("BIRTH_SETTLED")
}

pub fn native_fn_test() {
  let kn =
    NativeFn(token: ReturnThis, name: "nm", length: 3, constructible: True)
  assert tag_of(kn) == tag("NATIVEFN_TAG")
  assert size_of(kn) == idx("NATIVEFN_SIZE")
  assert at(kn, "NATIVEFN_TOKEN") == dyn(ReturnThis)
  assert at(kn, "NATIVEFN_NAME") == dyn("nm")
  assert at(kn, "NATIVEFN_LENGTH") == dyn(3)
  assert at(kn, "NATIVEFN_CONSTRUCTIBLE") == dyn(True)
}

pub fn data_property_test() {
  let v = types.mk_string("v")
  let names = [
    "DATAPROPERTY_WRITABLE",
    "DATAPROPERTY_ENUMERABLE",
    "DATAPROPERTY_CONFIGURABLE",
  ]
  let one_hot = [
    #(
      "DATAPROPERTY_WRITABLE",
      DataProperty(
        value: v,
        writable: True,
        enumerable: False,
        configurable: False,
        seq: 77,
      ),
    ),
    #(
      "DATAPROPERTY_ENUMERABLE",
      DataProperty(
        value: v,
        writable: False,
        enumerable: True,
        configurable: False,
        seq: 77,
      ),
    ),
    #(
      "DATAPROPERTY_CONFIGURABLE",
      DataProperty(
        value: v,
        writable: False,
        enumerable: False,
        configurable: True,
        seq: 77,
      ),
    ),
  ]
  list.each(one_hot, fn(entry) {
    let #(set_name, prop) = entry
    assert tag_of(prop) == tag("DATAPROPERTY_TAG")
    assert size_of(prop) == idx("DATAPROPERTY_SIZE")
    assert at(prop, "DATAPROPERTY_VALUE") == dyn(v)
    assert at(prop, "DATAPROPERTY_SEQ") == dyn(77)
    use name <- list.each(names)
    assert at(prop, name) == dyn(name == set_name)
  })
  assert plain_property(v, 9)
    == dyn(DataProperty(
      value: v,
      writable: True,
      enumerable: True,
      configurable: True,
      seq: 9,
    ))
  let g = types.mk_string("g")
  let s = types.mk_string("s")
  let acc =
    AccessorProperty(
      get: Some(g),
      set: Some(s),
      enumerable: True,
      configurable: False,
      seq: 78,
    )
  assert tag_of(acc) == tag("ACCESSORPROPERTY_TAG")
  assert size_of(acc) == idx("ACCESSORPROPERTY_SIZE")
  assert at(acc, "ACCESSORPROPERTY_GET") == dyn(Some(g))
  assert at(acc, "ACCESSORPROPERTY_SET") == dyn(Some(s))
}

pub fn step_and_resume_test() {
  let v = types.mk_string("v")
  let compiled = ResumeCompiled(sm: sm_fn("sm"), rs: 3, loc: loc("L"))
  assert tag_of(compiled) == tag("RESUMECOMPILED_TAG")
  assert size_of(compiled) == 4
  assert element(2, dyn(compiled)) == dyn("sm")
  assert element(3, dyn(compiled)) == dyn(3)
  assert element(4, dyn(compiled)) == dyn("L")
  let parked = ResumeFrame(frame: frame("F"))
  assert tag_of(parked) == tag("RESUMEFRAME_TAG")
  assert element(2, dyn(parked)) == dyn("F")
  assert tag_of(StepReturn(v)) == tag("STEP_RETURN")
  assert element(2, dyn(StepReturn(v))) == dyn(v)
  assert tag_of(StepThrow(v)) == tag("STEP_THROW")
  assert tag_of(StepYield(v, compiled)) == tag("STEP_YIELD")
  assert element(2, dyn(StepYield(v, compiled))) == dyn(v)
  assert element(3, dyn(StepYield(v, compiled))) == dyn(compiled)
  assert tag_of(StepAwait(v, parked)) == tag("STEP_AWAIT")
  assert element(3, dyn(StepAwait(v, parked))) == dyn(parked)
}

pub fn completion_test() {
  let v = types.mk_string("c")
  assert tag_of(NormalCompletion(v)) == tag("COMPLETION_NORMAL")
  assert element(2, dyn(NormalCompletion(v))) == dyn(v)
  assert tag_of(ThrowCompletion(v)) == tag("COMPLETION_THROW")
  assert element(2, dyn(ThrowCompletion(v))) == dyn(v)
}

pub fn frame_test() {
  let this = types.mk_string("this")
  let f = types.mk_string("fn")
  let home = types.mk_string("home")
  let nt = types.mk_undefined()
  let frame = rt_call.mk_frame(this, f, home, nt)
  assert frame_macro(this, f, home, nt) == dyn(frame)
  assert dyn(frame) == dyn(#(this, f, home, nt))
  assert rt_call.frame_active_func(frame) == f
}

@external(erlang, "arc_rt_obj_ffi", "get_elem")
fn get_elem(st: Agent, recv: JsVal, idx: Int) -> Dynamic

@external(erlang, "arc_rt_obj_ffi", "set_elem")
fn set_elem(st: Agent, recv: JsVal, idx: Int, v: JsVal) -> Dynamic

@external(erlang, "arc_rt_obj_ffi", "get_prop_own_data")
fn get_prop_own_data(st: Agent, recv: JsVal, key: BitArray) -> Dynamic

@external(erlang, "arc_rt_obj_ffi", "set_prop_own_data")
fn set_prop_own_data(st: Agent, recv: JsVal, key: BitArray, v: JsVal) -> Dynamic

@external(erlang, "arc_rt_obj_ffi", "instanceof_i32")
fn instanceof_i32(st: Agent, v: JsVal, ctor: JsVal) -> Dynamic

@external(erlang, "arc_rt_call_ic_ffi", "call_method_mono")
fn call_method_mono(
  st: Agent,
  recv: JsVal,
  key: BitArray,
  args: List(JsVal),
) -> #(Dynamic, Agent)

@external(erlang, "arc_rt_call_ic_ffi", "new_direct")
fn new_direct(st: Agent, ctor: JsVal, args: List(JsVal)) -> #(Dynamic, Agent)

type Probe {
  Miss
}

pub fn typed_array_fast_paths_miss_test() {
  let st = seeded()
  let #(ctor, st) = rt_lang.global_get(st, <<"Uint8Array">>)
  let n = types.mk_int(4)
  let #(h, st) = rt_call.construct(st, ctor, [n], ctor)
  let ta = types.mk_object(h)
  let #(_, st) =
    rt_obj.set_prop(st, ta, StringKey(Named("extra")), types.mk_string("x"))
  assert get_elem(st, ta, 0) == dyn(Miss)
  assert set_elem(st, ta, 0, n) == dyn(Miss)
  assert set_elem(st, ta, 4, n) == dyn(Miss)
  assert get_prop_own_data(st, ta, <<"length">>) == dyn(Miss)
  assert get_prop_own_data(st, ta, <<"extra">>) == dyn(Miss)
  assert set_prop_own_data(st, ta, <<"extra">>, n) == dyn(Miss)
}

pub fn proxy_fast_paths_miss_test() {
  let st = seeded()
  let n = types.mk_int(4)
  let #(arr, st) = rt_obj.new_array(st, [n, n])
  let #(handler, st) = rt_obj.new_object_literal(st)
  let #(proxy_ctor, st) = rt_lang.global_get(st, <<"Proxy">>)
  let #(ph, st) = rt_call.construct(st, proxy_ctor, [arr, handler], proxy_ctor)
  let p = types.mk_object(ph)
  assert get_elem(st, p, 0) == dyn(Miss)
  assert set_elem(st, p, 0, n) == dyn(Miss)
  assert get_prop_own_data(st, p, <<"length">>) == dyn(Miss)
  assert set_prop_own_data(st, p, <<"length">>, n) == dyn(Miss)
  assert call_method_mono(st, p, <<"push">>, [n]).0 == dyn(Miss)
  // instanceof over a proxy must reach the getprototypeof trap
  let ctor_flags = FnFlags(..no_flags(), is_constructor: True)
  let #(f, st) =
    rt_call.new_function(st, dummy_code("F"), ctor_flags, "F", 0, None)
  let #(_, st) = rt_obj.get_prop(st, f, StringKey(Named("prototype")))
  let #(plain, st) = rt_obj.new_object_literal(st)
  assert instanceof_i32(st, plain, f) == dyn(0)
  assert instanceof_i32(st, p, f) == dyn(Miss)
  let #(child, st) = rt_obj.new_object(st, Some(ph))
  assert instanceof_i32(st, types.mk_object(child), f) == dyn(Miss)
}

pub fn string_object_fast_paths_miss_test() {
  let st = seeded()
  let n = types.mk_int(1)
  let #(string_ctor, st) = rt_lang.global_get(st, <<"String">>)
  let #(sh, st) =
    rt_call.construct(st, string_ctor, [types.mk_string("abc")], string_ctor)
  let s = types.mk_object(sh)
  let #(_, st) =
    rt_obj.set_prop(st, s, StringKey(Named("extra")), types.mk_string("x"))
  assert get_elem(st, s, 0) == dyn(Miss)
  assert set_elem(st, s, 0, n) == dyn(Miss)
  assert set_elem(st, s, 3, n) == dyn(Miss)
  assert get_prop_own_data(st, s, <<"length">>) == dyn(Miss)
  assert set_prop_own_data(st, s, <<"length">>, n) == dyn(Miss)
  assert get_prop_own_data(st, s, <<"extra">>) == dyn(types.mk_string("x"))
  assert set_prop_own_data(st, s, <<"extra">>, n) != dyn(Miss)
}

pub fn bytecode_function_fast_paths_miss_test() {
  let st = seeded()
  let flags = FnFlags(..no_flags(), is_constructor: True, is_strict: True)
  let kind =
    BytecodeFn(
      template: template("tpl"),
      env: env([]),
      home_object: None,
      flags:,
      fields_init: None,
      realm: 0,
      unit_id: 0,
      birth: BirthSettled,
    )
  assert tag_of(kind) == tag("BYTECODEFN_TAG")
  assert size_of(kind) == idx("BYTECODEFN_SIZE")
  assert at(kind, "BYTECODEFN_BIRTH") == tag("BIRTH_SETTLED")
  let #(fh, st) =
    rt_store.cell_new(
      st,
      plain_object(kind, Some(st.realm.function.prototype), dict.new()),
    )
  let f = types.mk_object(fh)
  assert rt_val.is_callable(st, f)
  assert rt_call.is_constructor(st, f)
  let undef = types.mk_undefined()
  assert rt_call.direct_callee(st, f, undef) == dyn(Miss)
  let #(o, st) = rt_obj.new_object_literal(st)
  let #(_, st) = rt_obj.set_prop(st, o, StringKey(Named("m")), f)
  assert call_method_mono(st, o, <<"m">>, []).0 == dyn(Miss)
  assert new_direct(st, f, []).0 == dyn(Miss)
  assert instanceof_i32(st, o, f) == dyn(Miss)
}

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn compiled_code(
  code: fn(Agent, Dynamic, List(JsVal)) -> #(JsVal, Agent),
) -> CompiledCode

pub fn compiled_function_fast_paths_hit_test() {
  let st = seeded()
  let undef = types.mk_undefined()
  let code = compiled_code(fn(st, _frame, _args) { #(undef, st) })
  let flags = FnFlags(..no_flags(), is_constructor: True, is_strict: True)
  let #(f, st) = rt_call.new_function(st, code, flags, "F", 0, None)
  assert rt_call.direct_callee(st, f, undef) != dyn(Miss)
  assert new_direct(st, f, []).0 == dyn(Miss)
  let #(proto, st) = rt_obj.get_prop(st, f, StringKey(Named("prototype")))
  let #(this, _) = new_direct(st, f, [])
  assert this != dyn(Miss)
  assert this != dyn(proto)
}

pub fn binop_kind_terms_test() {
  let six = mk_int(6)
  let three = mk_int(3)
  let answers = [
    #(binop.AddOp, dyn(mk_int(9))),
    #(binop.PureOp(binop.Arith(binop.Sub)), dyn(mk_int(3))),
    #(binop.PureOp(binop.Arith(binop.Mul)), dyn(mk_int(18))),
    #(binop.PureOp(binop.Arith(binop.Div)), dyn(mk_int(2))),
    #(binop.PureOp(binop.Arith(binop.Mod)), dyn(mk_int(0))),
    #(binop.PureOp(binop.Bitwise(binop.BitAnd)), dyn(mk_int(2))),
    #(binop.PureOp(binop.Bitwise(binop.BitOr)), dyn(mk_int(7))),
    #(binop.PureOp(binop.Bitwise(binop.BitXor)), dyn(mk_int(5))),
    #(binop.PureOp(binop.Bitwise(binop.ShiftLeft)), dyn(mk_int(48))),
    #(binop.PureOp(binop.Bitwise(binop.ShiftRight)), dyn(mk_int(0))),
    #(binop.PureOp(binop.Bitwise(binop.ShiftRightUnsigned)), dyn(mk_int(0))),
    #(binop.PureOp(binop.Equality(binop.LooseEq)), dyn(False)),
    #(binop.PureOp(binop.Equality(binop.LooseNotEq)), dyn(True)),
    #(binop.PureOp(binop.Equality(binop.StrictEq)), dyn(False)),
    #(binop.PureOp(binop.Equality(binop.StrictNotEq)), dyn(True)),
    #(binop.PureOp(binop.Compare(binop.Less)), dyn(False)),
    #(binop.PureOp(binop.Compare(binop.LessEq)), dyn(False)),
    #(binop.PureOp(binop.Compare(binop.Greater)), dyn(True)),
    #(binop.PureOp(binop.Compare(binop.GreaterEq)), dyn(True)),
    #(binop.PureOp(binop.Arith(binop.Exp)), dyn(kernel.Miss)),
    #(binop.InOp, dyn(kernel.Miss)),
    #(binop.InstanceOfOp, dyn(kernel.Miss)),
  ]
  list.each(answers, fn(row) {
    let #(kind, expected) = row
    assert dyn(kernel.classified_binop(kind, six, three)) == expected
  })
}

pub fn iterator_kinds_test() {
  let h = Handle(9)
  let it = types.ArrayIterator(target: h, index: 4, kind: types.ArrayIterValues)
  assert tag_of(it) == tag("ARRAYITERATOR_TAG")
  assert size_of(it) == idx("ARRAYITERATOR_SIZE")
  assert at(it, "ARRAYITERATOR_TARGET") == dyn(h)
  assert at(it, "ARRAYITERATOR_INDEX") == dyn(4)
  assert at(it, "ARRAYITERATOR_KIND") == tag("ARRAYITER_VALUES")
  let g = types.GeneratorObj(data: h)
  assert tag_of(g) == tag("GENERATOROBJ_TAG")
  assert size_of(g) == idx("GENERATOROBJ_SIZE")
  assert at(g, "GENERATOROBJ_DATA") == dyn(h)
  assert dyn(types.IteratorN(types.ArrayIteratorNext))
    == tag("TOKEN_ARRAY_ITER_NEXT")
  assert dyn(types.GeneratorN(types.GeneratorNext))
    == tag("TOKEN_GENERATOR_NEXT")
  assert tag_of(types.IteratorN(types.ArrayIteratorNext))
    == tag("ITERATORN_TAG")
  assert dyn(types.ArrayN(types.ArrayPrototypeValues))
    == tag("TOKEN_ARRAY_VALUES")
  assert dyn(types.StringN(types.StringPrototypeSymbolIterator))
    == tag("TOKEN_STRING_ITER")
  assert dyn(types.IteratorN(types.StringIteratorNext))
    == tag("TOKEN_STRING_ITER_NEXT")
  assert dyn(types.MapN(types.MapEntries)) == tag("TOKEN_MAP_ENTRIES")
  assert dyn(types.IteratorN(types.MapIteratorNext))
    == tag("TOKEN_MAP_ITER_NEXT")
  assert dyn(types.SetN(types.SetValues)) == tag("TOKEN_SET_VALUES")
  assert dyn(types.IteratorN(types.SetIteratorNext))
    == tag("TOKEN_SET_ITER_NEXT")
  assert dyn(types.ReturnThis) == tag("TOKEN_RETURN_THIS")
  let v = types.mk_object(h)
  assert tag_of(IteratorRecord(iterator: v, next_method: v))
    == tag("ITERATORRECORD_TAG")
}

pub fn ic_entry_tags_test() {
  let key = <<"k":utf8>>
  assert tag_of(types.IcRead(key, dict.new())) == tag("IC_READ")
  assert tag_of(types.IcCall(key, dict.new(), dict.new())) == tag("IC_CALL")
  let blank = SBox(types.mk_undefined())
  assert tag_of(types.IcInit(0, 1, blank, [])) == tag("IC_INIT")
  assert tag_of(types.IcGlobal(key, 0, types.mk_undefined(), 0))
    == tag("IC_GLOBAL")
  assert dyn(types.IcOff) == tag("IC_OFF")
  assert tag_of(types.IcPlain(1)) == tag("ICPLAIN_TAG")
  assert tag_of(types.IcOwn(1)) == tag("ICOWN_TAG")
  assert tag_of(types.IcPrim(1, 2)) == tag("ICPRIM_TAG")
}
