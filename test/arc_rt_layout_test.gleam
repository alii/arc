//// Asserts that every tuple index and tag atom in arc_rt_layout.hrl matches
//// the Gleam runtime records the hand-written Erlang fast paths index with
//// element/2. A field reorder or insert in those records fails here.

import arc/bytecode/opcode
import arc/internal/tree_array
import arc/interp/ffi
import arc/rt/arena
import arc/rt/bytecode.{type EnvTuple, type FuncTemplate}
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type CompiledFn, type FnFlags, type JsVal, type ShapeSlots,
  AccessorProperty, ArgumentsObj, ArrayObj, BirthPending, BirthSettled,
  DataProperty, Dense, FnFlags, Index, JsCell, JsStore, KBytecode, KCompiled,
  KHandle, KNative, Named, NoElements, Ordinary, Private, ProxyObj,
  ResumeCompiled, ResumeFrame, ReturnThis, SBox, SObject, SShapedObject,
  ShapeDesc, Sparse, StepAwait, StepReturn, StepThrow, StepYield, StringKey,
  StringObj, SymbolKey,
} as rt_types
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

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn template(label: String) -> FuncTemplate

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn env(vals: List(JsVal)) -> EnvTuple

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn sm_fn(label: String) -> rt_types.SmFn

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn loc(label: String) -> rt_types.Loc

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn frame(label: String) -> bytecode.SuspendedFrame

fn at(record: a, name: String) -> Dynamic {
  element(idx(name), dyn(record))
}

fn tag_of(record: a) -> Dynamic {
  element(1, dyn(record))
}

fn arity(record: a) -> Int {
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
  assert arity(st) == idx("AGENT_ARITY")
  assert at(st, "AGENT_STORE") == dyn(st.store)
  assert at(st, "AGENT_REALM") == dyn(st.realm)
  assert at(st, "AGENT_HOST_FNS") == dyn(st.host_fns)
  assert at(st, "AGENT_REALMS") == dyn(st.realms)
  assert tag_of(at(st, "AGENT_STORE")) == tag("STORE_TAG")
  assert tag_of(at(st, "AGENT_REALM")) == tag("REALM_TAG")
  assert dict.get(st.realms, st.realm.id) == Ok(st.realm)
}

pub fn js_store_test() {
  let base = rt_store.t_store_new()
  let desc =
    ShapeDesc(
      arity: 1,
      offsets: dict.from_list([#(<<"k":utf8>>, 0)]),
      transitions: dict.new(),
    )
  let store =
    JsStore(
      ..base,
      data: arena.set(3, SBox(rt_types.mk_string("d")), base.data),
      next: 13,
      pinned_roots: set.from_list([3]),
      alloc_since_gc: 14,
      prop_seq: 16,
      shapes: dict.from_list([#(7, desc)]),
      next_shape: 15,
      ics: dict.from_list([
        #(1, rt_types.IcRead(<<"k":utf8>>, dict.from_list([#(7, 0)]))),
      ]),
    )
  assert tag_of(store) == tag("STORE_TAG")
  assert arity(store) == idx("STORE_ARITY")
  assert at(store, "STORE_DATA") == dyn(store.data)
  assert at(store, "STORE_NEXT") == dyn(13)
  assert at(store, "STORE_PINNED_ROOTS") == dyn(store.pinned_roots)
  assert at(store, "STORE_ALLOC") == dyn(14)
  assert at(store, "STORE_PROP_SEQ") == dyn(16)
  assert at(store, "STORE_SHAPES") == dyn(store.shapes)
  assert at(store, "STORE_NEXT_SHAPE") == dyn(15)
  assert at(store, "STORE_ICS") == dyn(store.ics)
}

pub fn realm_test() {
  let realm = seeded().realm
  assert tag_of(realm) == tag("REALM_TAG")
  assert arity(realm) == idx("REALM_ARITY")
  assert at(realm, "REALM_OBJECT") == dyn(realm.object)
  assert at(realm, "REALM_FUNCTION") == dyn(realm.function)
  assert at(realm, "REALM_ARRAY") == dyn(realm.array)
  assert at(realm, "REALM_STRING") == dyn(realm.string)
  assert at(realm, "REALM_NUMBER") == dyn(realm.number)
  assert at(realm, "REALM_GLOBAL") == dyn(realm.global_object)
  assert at(realm, "REALM_ID") == dyn(realm.id)
  assert realm.id == 0
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
  let args = ArgumentsObj(length: 2, mapped: None)
  assert tag_of(args) == tag("ARGUMENTSOBJ_TAG")
  assert arity(args) == idx("ARGUMENTSOBJ_ARITY")
  assert at(args, "ARGUMENTSOBJ_MAPPED") == dyn(None)
  assert tag_of(ProxyObj(target: proto, handler: proto, revoked: False))
    == tag("PROXYOBJ_TAG")
  let wrapper = StringObj("s")
  assert tag_of(wrapper) == tag("STRINGOBJ_TAG")
  assert at(wrapper, "STRINGOBJ_VALUE") == dyn("s")
  let box = SBox(rt_types.mk_string("b"))
  assert tag_of(box) == tag("SBOX_TAG")
  assert at(box, "SBOX_VALUE") == dyn(rt_types.mk_string("b"))
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
  assert dyn(rt_types.mk_hole()) == tag("ELEMS_HOLE")
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
  assert arity(flags) == idx("FNFLAGS_ARITY")
  use name <- list.each(names)
  assert at(flags, name) == dyn(name == set_name)
}

pub fn kcompiled_test() {
  let code = compiled_fn("code")
  let code_s = compiled_fn("code_s")
  let flags = FnFlags(..no_flags(), is_arrow: True)
  let kfn =
    KCompiled(
      code:,
      home_object: Some(JsCell(30)),
      flags:,
      fields_init: Some(JsCell(31)),
      simple: Some(#(code_s, 2, True)),
      name: "nm",
      length: 2,
      birth: BirthPending(Some(JsCell(32))),
    )
  assert tag_of(kfn) == tag("KFN_TAG")
  assert arity(kfn) == idx("KFN_ARITY")
  assert at(kfn, "KFN_CODE") == dyn(code)
  assert at(kfn, "KFN_HOME") == dyn(Some(JsCell(30)))
  assert at(kfn, "KFN_FLAGS") == dyn(flags)
  assert at(kfn, "KFN_FIELDS_INIT") == dyn(Some(JsCell(31)))
  assert at(kfn, "KFN_NAME") == dyn("nm")
  assert at(kfn, "KFN_LENGTH") == dyn(2)
  let birth = at(kfn, "KFN_BIRTH")
  assert birth == dyn(BirthPending(Some(JsCell(32))))
  assert tag_of(birth) == tag("BIRTH_PENDING_TAG")
  assert at(birth, "BIRTH_PROTOTYPE_PARENT") == dyn(Some(JsCell(32)))
  let simple = at(kfn, "KFN_SIMPLE")
  assert tag_of(simple) == tag("SOME")
  let inner = element(2, simple)
  assert tuple_size(inner) == 3
  assert element(1, inner) == dyn(code_s)
  assert element(2, inner) == dyn(2)
  assert element(3, inner) == dyn(True)
  let bare =
    KCompiled(
      code:,
      home_object: None,
      flags:,
      fields_init: None,
      simple: None,
      name: "",
      length: 0,
      birth: BirthSettled,
    )
  assert at(bare, "KFN_HOME") == tag("NONE")
  assert at(bare, "KFN_FIELDS_INIT") == tag("NONE")
  assert at(bare, "KFN_SIMPLE") == tag("NONE")
  assert at(bare, "KFN_BIRTH") == tag("BIRTH_SETTLED")
}

pub fn knative_test() {
  let kn = KNative(tag: ReturnThis, name: "nm", length: 3, constructible: True)
  assert tag_of(kn) == tag("KNATIVE_TAG")
  assert arity(kn) == idx("KNATIVE_ARITY")
  assert at(kn, "KNATIVE_TOKEN") == dyn(ReturnThis)
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

pub fn step_and_resume_test() {
  let v = rt_types.mk_string("v")
  let compiled = ResumeCompiled(sm: sm_fn("sm"), rs: 3, loc: loc("L"))
  assert tag_of(compiled) == tag("RESUME_COMPILED_TAG")
  assert arity(compiled) == 4
  assert element(2, dyn(compiled)) == dyn("sm")
  assert element(3, dyn(compiled)) == dyn(3)
  assert element(4, dyn(compiled)) == dyn("L")
  let parked = ResumeFrame(frame: frame("F"))
  assert tag_of(parked) == tag("RESUME_FRAME_TAG")
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

@external(erlang, "arc_rt_call_fast_ffi", "t_call_method_mono")
fn call_method_mono(
  st: Agent,
  recv: JsVal,
  key: BitArray,
  args: List(JsVal),
) -> #(Dynamic, Agent)

@external(erlang, "arc_rt_call_fast_ffi", "t_new_simple")
fn new_simple(st: Agent, ctor: JsVal, args: List(JsVal)) -> #(Dynamic, Agent)

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
  let ctor_flags = FnFlags(..no_flags(), is_constructor: True)
  let #(f, st) =
    rt_call.t_new_function(st, compiled_fn("F"), ctor_flags, "F", 0, None)
  let #(_, st) = rt_obj.t_get_prop(st, f, StringKey(Named("prototype")))
  let #(plain, st) = rt_obj.t_new_object_literal(st)
  assert instanceof_fast(st, plain, f) == dyn(0)
  assert instanceof_fast(st, p, f) == dyn(Miss)
  let #(child, st) = rt_obj.t_new_object(st, Some(ph))
  assert instanceof_fast(st, rt_types.mk_object(child), f) == dyn(Miss)
}

/// A String exotic's synthesized index and "length" properties live in no
/// props dict, so they never take the Erlang own-data / element fast paths;
/// its plain named properties do.
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
  assert set_prop_own_data(st, s, <<"length">>, n) == dyn(Miss)
  assert get_prop_own_data(st, s, <<"extra">>) == dyn(rt_types.mk_string("x"))
  assert set_prop_own_data(st, s, <<"extra">>, n) != dyn(Miss)
}

/// Interpreted functions never take a compiled-code fast path: the closure
/// probe, the monomorphic method call, `new` and `instanceof` all miss on a
/// KBytecode cell, so its [[Call]]/[[Construct]] reach `JsOps`.
pub fn bytecode_function_fast_paths_miss_test() {
  let st = seeded()
  let flags = FnFlags(..no_flags(), is_constructor: True, is_strict: True)
  let kind =
    KBytecode(
      template: template("tpl"),
      env: env([]),
      home_object: None,
      flags:,
      fields_init: None,
      realm: 0,
      unit: 0,
      birth: BirthSettled,
    )
  assert tag_of(kind) == tag("KBYTECODE_TAG")
  assert arity(kind) == idx("KBYTECODE_ARITY")
  assert at(kind, "KBYTECODE_BIRTH") == tag("BIRTH_SETTLED")
  let #(fh, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind:,
        proto: Some(st.realm.function.prototype),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let f = rt_types.mk_object(fh)
  assert rt_call.is_callable(st, f)
  assert rt_call.is_constructor(st, f)
  let undef = rt_types.mk_undefined()
  assert dyn(rt_call.t_kfn_code(st, f, undef)) == dyn(undef)
  let #(o, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) = rt_obj.t_set_prop(st, o, StringKey(Named("m")), f)
  assert call_method_mono(st, o, <<"m">>, []).0 == dyn(Miss)
  assert new_simple(st, f, []).0 == dyn(Miss)
  assert instanceof_fast(st, o, f) == dyn(Miss)
}

@external(erlang, "arc_rt_layout_root_ffi", "dyn")
fn compiled_code(
  code: fn(Agent, Dynamic, List(JsVal)) -> #(JsVal, Agent),
) -> CompiledFn

/// The positional `KCompiled` matches in the call fast paths keep up with the
/// record: a plain compiled function takes the closure probe, and `new` on it
/// takes the fast path once its `prototype` is settled.
pub fn compiled_function_fast_paths_hit_test() {
  let st = seeded()
  let undef = rt_types.mk_undefined()
  let code = compiled_code(fn(st, _frame, _args) { #(undef, st) })
  let flags = FnFlags(..no_flags(), is_constructor: True, is_strict: True)
  let #(f, st) = rt_call.t_new_function(st, code, flags, "F", 0, None)
  assert dyn(rt_call.t_kfn_code(st, f, undef)) != dyn(undef)
  assert new_simple(st, f, []).0 == dyn(Miss)
  let #(proto, st) = rt_obj.t_get_prop(st, f, StringKey(Named("prototype")))
  let #(this, _) = new_simple(st, f, [])
  assert this != dyn(Miss)
  assert this != dyn(proto)
}

@external(erlang, "arc_rt_ops_ffi", "binop")
fn k_binop(kind: opcode.Classified, a: JsVal, b: JsVal) -> Dynamic

/// arc_rt_ops_ffi:binop/3 matches the `opcode.Classified` term the resolver
/// stores in BinOp: every operator it can run answers a value for two small
/// integers, and the heap-reading ones answer `miss`.
fn num(n: Int) -> JsVal {
  rt_types.mk_number(rt_types.JInt(n))
}

pub fn binop_kind_terms_test() {
  let six = num(6)
  let three = num(3)
  let answers = [
    #(opcode.Add, dyn(num(9))),
    #(opcode.Sub, dyn(num(3))),
    #(opcode.Mul, dyn(num(18))),
    #(opcode.Div, dyn(num(2))),
    #(opcode.Mod, dyn(num(0))),
    #(opcode.BitAnd, dyn(num(2))),
    #(opcode.BitOr, dyn(num(7))),
    #(opcode.BitXor, dyn(num(5))),
    #(opcode.ShiftLeft, dyn(num(48))),
    #(opcode.ShiftRight, dyn(num(0))),
    #(opcode.UShiftRight, dyn(num(0))),
    #(opcode.Eq, dyn(False)),
    #(opcode.NotEq, dyn(True)),
    #(opcode.StrictEq, dyn(False)),
    #(opcode.StrictNotEq, dyn(True)),
    #(opcode.Lt, dyn(False)),
    #(opcode.LtEq, dyn(False)),
    #(opcode.Gt, dyn(True)),
    #(opcode.GtEq, dyn(True)),
    #(opcode.Exp, dyn(ffi.Miss)),
    #(opcode.In, dyn(ffi.Miss)),
    #(opcode.InstanceOf, dyn(ffi.Miss)),
  ]
  list.each(answers, fn(row) {
    let #(kind, expected) = row
    assert k_binop(opcode.classify(kind), six, three) == expected
  })
}

pub fn iterator_kinds_test() {
  let h = JsCell(9)
  let it =
    rt_types.ArrayIterator(target: h, index: 4, kind: rt_types.ArrayIterValues)
  assert tag_of(it) == tag("ARRAYITER_TAG")
  assert arity(it) == idx("ARRAYITER_ARITY")
  assert at(it, "ARRAYITER_TARGET") == dyn(h)
  assert at(it, "ARRAYITER_INDEX") == dyn(4)
  assert at(it, "ARRAYITER_KIND") == tag("ARRAYITER_VALUES")
  let g = rt_types.GeneratorObj(data: h)
  assert tag_of(g) == tag("GENERATOROBJ_TAG")
  assert arity(g) == idx("GENERATOROBJ_ARITY")
  assert at(g, "GENERATOROBJ_DATA") == dyn(h)
  assert dyn(rt_types.IteratorN(rt_types.ArrayIteratorNext))
    == tag("TOKEN_ARRAY_ITER_NEXT")
  assert dyn(rt_types.GeneratorN(rt_types.GeneratorNext))
    == tag("TOKEN_GENERATOR_NEXT")
}
