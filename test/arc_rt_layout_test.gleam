//// Asserts that every tuple index and tag atom in arc_rt_layout.hrl matches
//// the Gleam runtime records the hand-written Erlang fast paths index with
//// element/2. A field reorder or insert in those records fails here.

import emit_2core_harness as harness
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import twocore/runtime/rt_js_builtins
import twocore/runtime/rt_js_call.{NormalCompletion, ThrowCompletion}
import twocore/runtime/rt_js_store
import twocore/runtime/rt_js_tree_array as tree_array
import twocore/runtime/rt_js_types.{
  type CompiledFn, type FnFlags, type JsVal, type ShapeSlots, AccessorProperty,
  ArrayObj, DataProperty, Dense, FnFlags, Index, JsCell, JsStore, KFunction,
  KHandle, KNative, Named, NativeUnseeded, NoElements, Ordinary, Private, SBox,
  SObject, SShapedObject, ShapeDesc, Sparse, StringKey, SymbolKey,
}
import twocore/runtime/rt_state.{type InstanceState}

@external(erlang, "arc_rt_layout_test_ffi", "idx")
fn idx(name: String) -> Int

@external(erlang, "arc_rt_layout_test_ffi", "tag")
fn tag(name: String) -> Dynamic

@external(erlang, "arc_rt_layout_test_ffi", "element")
fn element(n: Int, of: Dynamic) -> Dynamic

@external(erlang, "arc_rt_layout_test_ffi", "tuple_size")
fn tuple_size(of: Dynamic) -> Int

@external(erlang, "arc_rt_layout_test_ffi", "dyn")
fn dyn(x: a) -> Dynamic

@external(erlang, "arc_rt_layout_test_ffi", "dyn")
fn compiled_fn(label: String) -> CompiledFn

@external(erlang, "arc_rt_layout_test_ffi", "slots")
fn slots(vals: List(JsVal)) -> ShapeSlots

@external(erlang, "arc_rt_layout_test_ffi", "pdict_get")
fn pdict_get(key: Dynamic) -> Dynamic

@external(erlang, "twocore_rt_js_obj_ffi", "t_new_object_shaped")
fn t_new_object_shaped(
  st: InstanceState,
  site: BitArray,
  keys: List(BitArray),
  vals: List(JsVal),
) -> #(JsVal, InstanceState)

@external(erlang, "twocore_rt_js_obj_ffi", "t_ic_get")
fn t_ic_get(
  st: InstanceState,
  obj: JsVal,
  key: BitArray,
  site: BitArray,
) -> Dynamic

@external(erlang, "twocore_rt_js_obj_ffi", "jsv_clear")
fn jsv_clear() -> Nil

fn at(record: a, name: String) -> Dynamic {
  element(idx(name), dyn(record))
}

fn tag_of(record: a) -> Dynamic {
  element(1, dyn(record))
}

fn arity(record: a) -> Int {
  tuple_size(dyn(record))
}

fn seeded() -> InstanceState {
  let st =
    rt_state.fresh_full(
      rt_state.FullDecl(mems: [], globals: [], tables: [], ref_globals: []),
    )
  let st =
    rt_state.t_with_js_store(
      st,
      rt_js_store.t_store_new(harness.twocore_test_hooks()),
    )
  let #(_realm, st) = rt_js_builtins.init_realm(st)
  st
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
  )
}

pub fn instance_state_test() {
  let bare =
    rt_state.fresh_full(
      rt_state.FullDecl(mems: [], globals: [], tables: [], ref_globals: []),
    )
  assert at(bare, "AGENT_STORE") == tag("NONE")
  assert at(bare, "AGENT_REALM") == tag("NONE")
  let st = seeded()
  let assert Some(store) = rt_state.t_js_store(st)
  let realm = rt_state.t_realm(st)
  assert at(st, "AGENT_STORE") == dyn(Some(store))
  assert at(st, "AGENT_REALM") == dyn(Some(realm))
  let wrapped = at(st, "AGENT_STORE")
  assert tag_of(wrapped) == tag("SOME")
  assert arity(wrapped) == 2
  assert element(2, wrapped) == dyn(store)
}

pub fn js_store_test() {
  let base = rt_js_store.t_store_new(harness.twocore_test_hooks())
  let desc =
    ShapeDesc(
      arity: 1,
      offsets: dict.from_list([#(<<"k":utf8>>, 0)]),
      transitions: dict.new(),
    )
  let store =
    JsStore(
      ..base,
      data: dict.from_list([#(3, SBox(rt_js_types.mk_string("d")))]),
      free: [11, 12],
      next: 13,
      pinned_roots: set.from_list([3]),
      alloc_since_gc: 14,
      shapes: dict.from_list([#(7, desc)]),
      next_shape: 15,
    )
  assert tag_of(store) == tag("STORE_TAG")
  assert arity(store) == idx("STORE_ARITY")
  assert at(store, "STORE_DATA") == dyn(store.data)
  assert at(store, "STORE_FREE") == dyn([11, 12])
  assert at(store, "STORE_NEXT") == dyn(13)
  assert at(store, "STORE_PINNED_ROOTS") == dyn(store.pinned_roots)
  assert at(store, "STORE_ALLOC") == dyn(14)
  assert at(store, "STORE_SHAPES") == dyn(store.shapes)
  assert at(store, "STORE_NEXT_SHAPE") == dyn(15)
}

pub fn realm_test() {
  let realm = rt_state.t_realm(seeded())
  assert tag_of(realm) == tag("REALM_TAG")
  assert arity(realm) == idx("REALM_ARITY")
  assert at(realm, "REALM_OBJECT") == dyn(realm.object)
  assert at(realm, "REALM_FUNCTION") == dyn(realm.function)
  assert at(realm, "REALM_ARRAY") == dyn(realm.array)
  assert at(realm, "REALM_GLOBAL") == dyn(realm.global_object)
  assert realm.object != realm.function
  assert realm.function != realm.array
  let pair = realm.object
  assert tag_of(pair) == tag("PAIR_TAG")
  assert arity(pair) == 3
  assert at(pair, "PAIR_PROTO") == dyn(pair.prototype)
  assert at(pair, "PAIR_CTOR") == dyn(pair.constructor)
  assert pair.prototype != pair.constructor
}

pub fn handle_test() {
  let h = JsCell(4242)
  assert tag_of(h) == tag("HANDLE_TAG")
  assert arity(h) == 2
  assert at(h, "HANDLE_ID") == dyn(4242)
  assert dyn(rt_js_types.mk_object(h)) == dyn(h)
  let assert KHandle(back) = rt_js_types.classify(rt_js_types.mk_object(h))
  assert back == h
  assert dyn(Some(h)) == dyn(#(tag("SOME"), h))
  assert dyn(None) == tag("NONE")
}

pub fn sobject_test() {
  let proto = JsCell(1)
  let vx = rt_js_types.mk_string("vx")
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
      rt_js_types.symbol_iterator,
      DataProperty(
        value: vx,
        writable: False,
        enumerable: False,
        configurable: False,
        seq: 1,
      ),
    ),
  ]
  let elements = Sparse(dict.from_list([#(0, rt_js_types.mk_string("e0"))]))
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
  assert arity(obj) == idx("SOBJECT_ARITY")
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
  assert arity(kind) == idx("ARRAYOBJ_ARITY")
  assert at(kind, "ARRAYOBJ_LENGTH") == dyn(9)
}

pub fn keys_and_elements_test() {
  assert tag_of(Named("x")) == tag("KEY_NAMED")
  assert element(2, dyn(Named("x"))) == dyn("x")
  assert tag_of(Index(5)) == tag("KEY_INDEX")
  assert element(2, dyn(Index(5))) == dyn(5)
  assert tag_of(Private(<<"#p":utf8>>)) == tag("KEY_PRIVATE")
  assert tag_of(StringKey(Named("x"))) == tag("OKEY_STRING")
  assert element(2, dyn(StringKey(Named("x")))) == dyn(Named("x"))
  assert tag_of(SymbolKey(rt_js_types.symbol_iterator)) == tag("OKEY_SYMBOL")
  assert dyn(NoElements) == tag("ELEMS_NONE")
  let arr =
    tree_array.from_list(
      [rt_js_types.mk_string("a")],
      rt_js_types.mk_undefined(),
    )
  assert tag_of(Dense(arr)) == tag("ELEMS_DENSE")
  assert element(2, dyn(Dense(arr))) == dyn(arr)
  let sparse = dict.from_list([#(0, rt_js_types.mk_string("s"))])
  assert tag_of(Sparse(sparse)) == tag("ELEMS_SPARSE")
  assert element(2, dyn(Sparse(sparse))) == dyn(sparse)
}

pub fn sshaped_object_test() {
  let s0 = rt_js_types.mk_string("s0")
  let s1 = rt_js_types.mk_string("s1")
  let sl = slots([s0, s1, rt_js_types.mk_string("s2")])
  let obj = SShapedObject(shape_id: 21, proto: Some(JsCell(2)), slots: sl)
  assert tag_of(obj) == tag("SSHAPED_TAG")
  assert arity(obj) == idx("SSHAPED_ARITY")
  assert at(obj, "SSHAPED_SID") == dyn(21)
  assert at(obj, "SSHAPED_PROTO") == dyn(Some(JsCell(2)))
  assert at(obj, "SSHAPED_SLOTS") == dyn(sl)
  assert tuple_size(dyn(sl)) == 3
  assert element(1, dyn(sl)) == dyn(s0)
  assert element(2, dyn(sl)) == dyn(s1)
  assert rt_js_types.shape_slots_get(sl, 1) == s1
}

pub fn shape_desc_test() {
  let desc =
    ShapeDesc(
      arity: 2,
      offsets: dict.from_list([#(<<"a":utf8>>, 0), #(<<"b":utf8>>, 1)]),
      transitions: dict.from_list([#(<<"c":utf8>>, 9)]),
    )
  assert tag_of(desc) == tag("SHAPE_TAG")
  assert arity(desc) == idx("SHAPE_ARITY")
  assert at(desc, "SHAPE_ARITY_F") == dyn(2)
  assert at(desc, "SHAPE_OFFSETS") == dyn(desc.offsets)
  assert at(desc, "SHAPE_TRANSITIONS") == dyn(desc.transitions)
}

pub fn fn_flags_test() {
  let base = no_flags()
  let names = [
    "FNFLAGS_IS_CTOR", "FNFLAGS_IS_CLASS_CTOR", "FNFLAGS_IS_DERIVED",
    "FNFLAGS_IS_ARROW", "FNFLAGS_IS_METHOD", "FNFLAGS_IS_GEN",
    "FNFLAGS_IS_ASYNC",
  ]
  let one_hot = [
    #("FNFLAGS_IS_CTOR", FnFlags(..base, is_constructor: True)),
    #("FNFLAGS_IS_CLASS_CTOR", FnFlags(..base, is_class_constructor: True)),
    #("FNFLAGS_IS_DERIVED", FnFlags(..base, is_derived_constructor: True)),
    #("FNFLAGS_IS_ARROW", FnFlags(..base, is_arrow: True)),
    #("FNFLAGS_IS_METHOD", FnFlags(..base, is_method: True)),
    #("FNFLAGS_IS_GEN", FnFlags(..base, is_generator: True)),
    #("FNFLAGS_IS_ASYNC", FnFlags(..base, is_async: True)),
  ]
  assert list.length(one_hot) == list.length(names)
  use #(set_name, flags) <- list.each(one_hot)
  assert tag_of(flags) == tag("FNFLAGS_TAG")
  assert arity(flags) == idx("FNFLAGS_ARITY")
  use name <- list.each(names)
  assert at(flags, name) == dyn(name == set_name)
}

pub fn kfunction_test() {
  let code = compiled_fn("code")
  let code_s = compiled_fn("code_s")
  let flags = FnFlags(..no_flags(), is_arrow: True)
  let kfn =
    KFunction(
      code:,
      home_object: Some(JsCell(30)),
      flags:,
      fields_init: Some(JsCell(31)),
      captures: [JsCell(32)],
      simple: Some(#(code_s, 2, True)),
    )
  assert tag_of(kfn) == tag("KFN_TAG")
  assert arity(kfn) == idx("KFN_ARITY")
  assert at(kfn, "KFN_CODE") == dyn(code)
  assert at(kfn, "KFN_HOME") == dyn(Some(JsCell(30)))
  assert at(kfn, "KFN_FLAGS") == dyn(flags)
  assert at(kfn, "KFN_FIELDS_INIT") == dyn(Some(JsCell(31)))
  assert at(kfn, "KFN_CAPTURES") == dyn([JsCell(32)])
  let simple = at(kfn, "KFN_SIMPLE")
  assert tag_of(simple) == tag("SOME")
  let inner = element(2, simple)
  assert tuple_size(inner) == 3
  assert element(1, inner) == dyn(code_s)
  assert element(2, inner) == dyn(2)
  assert element(3, inner) == dyn(True)
  let bare =
    KFunction(
      code:,
      home_object: None,
      flags:,
      fields_init: None,
      captures: [],
      simple: None,
    )
  assert at(bare, "KFN_HOME") == tag("NONE")
  assert at(bare, "KFN_FIELDS_INIT") == tag("NONE")
  assert at(bare, "KFN_SIMPLE") == tag("NONE")
}

pub fn knative_test() {
  let kn =
    KNative(tag: NativeUnseeded, name: "nm", length: 3, constructible: True)
  assert tag_of(kn) == tag("KNATIVE_TAG")
  assert arity(kn) == idx("KNATIVE_ARITY")
  assert at(kn, "KNATIVE_TOKEN") == dyn(NativeUnseeded)
  assert at(kn, "KNATIVE_NAME") == dyn("nm")
  assert at(kn, "KNATIVE_LENGTH") == dyn(3)
  assert at(kn, "KNATIVE_CONSTRUCTIBLE") == dyn(True)
}

pub fn data_property_test() {
  let v = rt_js_types.mk_string("v")
  let names = [
    "DATAPROP_WRITABLE",
    "DATAPROP_ENUMERABLE",
    "DATAPROP_CONFIGURABLE",
  ]
  let one_hot = [
    #(
      "DATAPROP_WRITABLE",
      DataProperty(
        value: v,
        writable: True,
        enumerable: False,
        configurable: False,
        seq: 77,
      ),
    ),
    #(
      "DATAPROP_ENUMERABLE",
      DataProperty(
        value: v,
        writable: False,
        enumerable: True,
        configurable: False,
        seq: 77,
      ),
    ),
    #(
      "DATAPROP_CONFIGURABLE",
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
    assert tag_of(prop) == tag("DATAPROP_TAG")
    assert arity(prop) == idx("DATAPROP_ARITY")
    assert at(prop, "DATAPROP_VALUE") == dyn(v)
    assert at(prop, "DATAPROP_SEQ") == dyn(77)
    use name <- list.each(names)
    assert at(prop, name) == dyn(name == set_name)
  })
  let g = rt_js_types.mk_string("g")
  let s = rt_js_types.mk_string("s")
  let acc =
    AccessorProperty(
      get: Some(g),
      set: Some(s),
      enumerable: True,
      configurable: False,
      seq: 78,
    )
  assert tag_of(acc) == tag("ACCESSORPROP_TAG")
  assert arity(acc) == idx("ACCESSORPROP_ARITY")
  assert at(acc, "ACCESSORPROP_GET") == dyn(Some(g))
  assert at(acc, "ACCESSORPROP_SET") == dyn(Some(s))
}

pub fn completion_test() {
  let v = rt_js_types.mk_string("c")
  assert tag_of(NormalCompletion(v)) == tag("COMPLETION_NORMAL")
  assert element(2, dyn(NormalCompletion(v))) == dyn(v)
  assert tag_of(ThrowCompletion(v)) == tag("COMPLETION_THROW")
  assert element(2, dyn(ThrowCompletion(v))) == dyn(v)
}

pub fn overlay_test() {
  jsv_clear()
  let st = seeded()
  let va = rt_js_types.mk_string("va")
  let vb = rt_js_types.mk_string("vb")
  let vc = rt_js_types.mk_string("vc")
  let #(obj, st) =
    t_new_object_shaped(
      st,
      <<"@layout-new":utf8>>,
      [<<"a":utf8>>, <<"b":utf8>>, <<"c":utf8>>],
      [va, vb, vc],
    )
  let assert KHandle(JsCell(id)) = rt_js_types.classify(obj)
  let assert SShapedObject(shape_id: sid, proto:, slots: sl) =
    rt_js_store.t_cell_get(st, JsCell(id))
  assert rt_js_types.shape_slots_get(sl, 1) == vb
  let site = <<"@layout-get":utf8>>
  assert t_ic_get(st, obj, <<"b":utf8>>, site) == dyn(vb)
  let off = idx("OVERLAY_OFF")
  let flat = pdict_get(dyn(id))
  assert tag_of(flat) == tag("SSHAPED_TAG")
  assert element(idx("SSHAPED_SID"), flat) == dyn(sid)
  assert element(idx("SSHAPED_PROTO"), flat) == dyn(proto)
  assert tuple_size(flat) == off - 1 + 3
  assert element(0 + off, flat) == dyn(va)
  assert element(1 + off, flat) == dyn(vb)
  assert element(2 + off, flat) == dyn(vc)
  assert pdict_get(dyn(site)) == dyn(#(sid, 1 + off))
  jsv_clear()
}
