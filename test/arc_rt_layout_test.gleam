//// Asserts that every tuple index and tag atom in arc_rt_layout.hrl matches
//// the Gleam runtime records the hand-written Erlang fast paths index with
//// element/2. A field reorder or insert in those records fails here.

import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type CompiledFn, type FnFlags, type HostHooks, type JsVal,
  type ShapeSlots, AccessorProperty, ArrayObj, DataProperty, Dense, FnFlags,
  HostHooks, Index, JsCell, JsStore, KFunction, KHandle, KNative, Named,
  NativeUnseeded, NoElements, Ordinary, Private, ProxyObj, SBox, SObject,
  SShapedObject, ShapeDesc, Sparse, StringKey, SymbolKey,
} as rt_types
import arc/vm/internal/tree_array
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{None, Some}
import gleam/set

@external(erlang, "arc_rt_layout_root_ffi", "idx")
fn idx(name: String) -> Int

@external(erlang, "arc_rt_layout_root_ffi", "tag")
fn tag(name: String) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "element")
fn element(n: Int, of: Dynamic) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "tuple_size")
fn tuple_size(of: Dynamic) -> Int

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn dyn(x: a) -> Dynamic

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn compiled_fn(label: String) -> CompiledFn

@external(erlang, "arc_rt_layout_root_ffi", "slots")
fn slots(vals: List(JsVal)) -> ShapeSlots

fn at(record: a, name: String) -> Dynamic {
  element(idx(name), dyn(record))
}

fn tag_of(record: a) -> Dynamic {
  element(1, dyn(record))
}

fn arity(record: a) -> Int {
  tuple_size(dyn(record))
}

fn hooks() -> HostHooks {
  HostHooks(
    monotonic_now: fn() { 0 },
    random: fn() { 0.5 },
    sleep_ms: fn(_) { Nil },
    print: fn(_) { Nil },
  )
}

fn seeded() -> Agent {
  rt_builtins.new_agent(hooks())
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

pub fn agent_test() {
  let st = seeded()
  assert tag_of(st) == tag("AGENT_TAG")
  assert arity(st) == idx("AGENT_ARITY")
  assert at(st, "AGENT_STORE") == dyn(st.store)
  assert at(st, "AGENT_REALM") == dyn(st.realm)
  assert tag_of(at(st, "AGENT_STORE")) == tag("STORE_TAG")
  assert tag_of(at(st, "AGENT_REALM")) == tag("REALM_TAG")
}

pub fn js_store_test() {
  let base = rt_store.t_store_new(hooks())
  let desc =
    ShapeDesc(
      arity: 1,
      offsets: dict.from_list([#(<<"k":utf8>>, 0)]),
      transitions: dict.new(),
    )
  let store =
    JsStore(
      ..base,
      data: dict.from_list([#(3, SBox(rt_types.mk_string("d")))]),
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
  let realm = seeded().realm
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
  assert dyn(rt_types.mk_object(h)) == dyn(h)
  let assert KHandle(back) = rt_types.classify(rt_types.mk_object(h))
  assert back == h
  assert dyn(Some(h)) == dyn(#(tag("SOME"), h))
  assert dyn(None) == tag("NONE")
}

pub fn sobject_test() {
  let proto = JsCell(1)
  let vx = rt_types.mk_string("vx")
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
      rt_types.symbol_iterator,
      DataProperty(
        value: vx,
        writable: False,
        enumerable: False,
        configurable: False,
        seq: 1,
      ),
    ),
  ]
  let elements = Sparse(dict.from_list([#(0, rt_types.mk_string("e0"))]))
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
  assert tag_of(ProxyObj(target: proto, handler: proto, revoked: False))
    == tag("PROXYOBJ_TAG")
}

pub fn keys_and_elements_test() {
  assert tag_of(Named("x")) == tag("KEY_NAMED")
  assert element(2, dyn(Named("x"))) == dyn("x")
  assert tag_of(Index(5)) == tag("KEY_INDEX")
  assert element(2, dyn(Index(5))) == dyn(5)
  assert tag_of(Private(<<"#p":utf8>>)) == tag("KEY_PRIVATE")
  assert tag_of(StringKey(Named("x"))) == tag("OKEY_STRING")
  assert element(2, dyn(StringKey(Named("x")))) == dyn(Named("x"))
  assert tag_of(SymbolKey(rt_types.symbol_iterator)) == tag("OKEY_SYMBOL")
  assert dyn(NoElements) == tag("ELEMS_NONE")
  let arr =
    tree_array.from_list([rt_types.mk_string("a")], rt_types.mk_undefined())
  assert tag_of(Dense(arr)) == tag("ELEMS_DENSE")
  assert element(2, dyn(Dense(arr))) == dyn(arr)
  let sparse = dict.from_list([#(0, rt_types.mk_string("s"))])
  assert tag_of(Sparse(sparse)) == tag("ELEMS_SPARSE")
  assert element(2, dyn(Sparse(sparse))) == dyn(sparse)
}

pub fn sshaped_object_test() {
  let s0 = rt_types.mk_string("s0")
  let s1 = rt_types.mk_string("s1")
  let sl = slots([s0, s1, rt_types.mk_string("s2")])
  let obj = SShapedObject(shape_id: 21, proto: Some(JsCell(2)), slots: sl)
  assert tag_of(obj) == tag("SSHAPED_TAG")
  assert arity(obj) == idx("SSHAPED_ARITY")
  assert at(obj, "SSHAPED_SID") == dyn(21)
  assert at(obj, "SSHAPED_PROTO") == dyn(Some(JsCell(2)))
  assert at(obj, "SSHAPED_SLOTS") == dyn(sl)
  assert tuple_size(dyn(sl)) == 3
  assert element(1, dyn(sl)) == dyn(s0)
  assert element(2, dyn(sl)) == dyn(s1)
  assert rt_types.shape_slots_get(sl, 1) == s1
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
  let v = rt_types.mk_string("v")
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
  let g = rt_types.mk_string("g")
  let s = rt_types.mk_string("s")
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
  let v = rt_types.mk_string("c")
  assert tag_of(NormalCompletion(v)) == tag("COMPLETION_NORMAL")
  assert element(2, dyn(NormalCompletion(v))) == dyn(v)
  assert tag_of(ThrowCompletion(v)) == tag("COMPLETION_THROW")
  assert element(2, dyn(ThrowCompletion(v))) == dyn(v)
}

@external(erlang, "arc_rt_obj_ffi", "t_get_elem_fast")
fn get_elem_fast(st: Agent, recv: JsVal, idx: Int) -> Dynamic

@external(erlang, "arc_rt_obj_ffi", "t_set_elem_fast")
fn set_elem_fast(st: Agent, recv: JsVal, idx: Int, v: JsVal) -> Dynamic

@external(erlang, "arc_rt_obj_ffi", "t_get_prop_own_data")
fn get_prop_own_data(st: Agent, recv: JsVal, key: BitArray) -> Dynamic

@external(erlang, "arc_rt_obj_ffi", "t_set_prop_own_data")
fn set_prop_own_data(st: Agent, recv: JsVal, key: BitArray, v: JsVal) -> Dynamic

@external(erlang, "arc_rt_obj_ffi", "t_instanceof_fast")
fn instanceof_fast(st: Agent, v: JsVal, ctor: JsVal) -> Dynamic

@external(erlang, "arc_rt_call_ffi", "t_call_method_mono")
fn call_method_mono(
  st: Agent,
  recv: JsVal,
  key: BitArray,
  args: List(JsVal),
) -> #(Dynamic, Agent)

type Probe {
  Miss
}

/// Integer-indexed exotics never take the Erlang own-data / element fast
/// paths: every probe on a TypedArray cell misses, so the exotic MOP arms in
/// arc/rt/obj run.
pub fn typed_array_fast_paths_miss_test() {
  let st = seeded()
  let #(ctor, st) = rt_obj.t_global_get(st, <<"Uint8Array">>)
  let n = rt_types.mk_number(rt_types.JInt(4))
  let #(h, st) = rt_call.t_construct(st, ctor, [n], ctor)
  let ta = rt_types.mk_object(h)
  let #(_, st) =
    rt_obj.t_set_prop(
      st,
      ta,
      StringKey(Named("extra")),
      rt_types.mk_string("x"),
    )
  assert get_elem_fast(st, ta, 0) == dyn(Miss)
  assert set_elem_fast(st, ta, 0, n) == dyn(Miss)
  assert set_elem_fast(st, ta, 4, n) == dyn(Miss)
  assert get_prop_own_data(st, ta, <<"length">>) == dyn(Miss)
  assert get_prop_own_data(st, ta, <<"extra">>) == dyn(Miss)
  assert set_prop_own_data(st, ta, <<"extra">>, n) == dyn(Miss)
}

/// Proxy exotics never take an Erlang fast path: own-data / element probes,
/// the monomorphic method call and the instanceof chain walk all miss on a
/// Proxy cell (its internal methods are traps, never its stored fields), so
/// the §10.5 arms in arc/rt/obj run.
pub fn proxy_fast_paths_miss_test() {
  let st = seeded()
  let n = rt_types.mk_number(rt_types.JInt(4))
  let #(arr, st) = rt_obj.t_new_array(st, [n, n])
  let #(handler, st) = rt_obj.t_new_object_literal(st)
  let #(proxy_ctor, st) = rt_obj.t_global_get(st, <<"Proxy">>)
  let #(ph, st) =
    rt_call.t_construct(st, proxy_ctor, [arr, handler], proxy_ctor)
  let p = rt_types.mk_object(ph)
  assert get_elem_fast(st, p, 0) == dyn(Miss)
  assert set_elem_fast(st, p, 0, n) == dyn(Miss)
  assert get_prop_own_data(st, p, <<"length">>) == dyn(Miss)
  assert set_prop_own_data(st, p, <<"length">>, n) == dyn(Miss)
  assert call_method_mono(st, p, <<"push">>, [n]).0 == dyn(Miss)
  // `p instanceof F` must reach the getPrototypeOf trap: the probe takes the
  // fast path for a plain-function ctor over an ordinary V, but never over a
  // proxy V or an ordinary V whose prototype chain crosses a proxy.
  let #(fh, st) =
    rt_call.t_fn_new(st, compiled_fn("F"), [], no_flags(), "F", 0, None, None)
  let #(f, st) = rt_call.t_make_constructor(st, rt_types.mk_object(fh))
  let #(plain, st) = rt_obj.t_new_object_literal(st)
  assert instanceof_fast(st, plain, f) == dyn(0)
  assert instanceof_fast(st, p, f) == dyn(Miss)
  let #(child, st) = rt_obj.t_new_object(st, Some(ph))
  assert instanceof_fast(st, rt_types.mk_object(child), f) == dyn(Miss)
}

/// String exotics never take the Erlang own-data / element fast paths: the
/// synthesized index and "length" properties live in no props dict.
pub fn string_object_fast_paths_miss_test() {
  let st = seeded()
  let n = rt_types.mk_number(rt_types.JInt(1))
  let #(string_ctor, st) = rt_obj.t_global_get(st, <<"String">>)
  let #(sh, st) =
    rt_call.t_construct(
      st,
      string_ctor,
      [rt_types.mk_string("abc")],
      string_ctor,
    )
  let s = rt_types.mk_object(sh)
  let #(_, st) =
    rt_obj.t_set_prop(st, s, StringKey(Named("extra")), rt_types.mk_string("x"))
  assert get_elem_fast(st, s, 0) == dyn(Miss)
  assert set_elem_fast(st, s, 0, n) == dyn(Miss)
  assert set_elem_fast(st, s, 3, n) == dyn(Miss)
  assert get_prop_own_data(st, s, <<"length">>) == dyn(Miss)
  assert get_prop_own_data(st, s, <<"extra">>) == dyn(Miss)
  assert set_prop_own_data(st, s, <<"extra">>, n) == dyn(Miss)
}
