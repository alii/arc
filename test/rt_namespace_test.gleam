import arc/bytecode/key.{Named}
import arc/rt/builtins as rt_builtins
import arc/rt/call.{ThrowCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/lang as rt_lang
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type ParsedDesc, DataProperty, JInt, KNum,
  KStr, KUndef, ParsedDesc, SBox, StringKey, SymbolKey, classify, mk_int,
  mk_object, mk_string, mk_tdz, mk_undefined,
}
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import rt_helpers

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks())
}

fn key(name: String) {
  StringKey(key.canonical(name))
}

fn throws(st: Agent, body: fn(Agent) -> #(a, Agent)) -> String {
  let #(c, st) =
    rt_call.try_run(st, fn(st) {
      let #(_, st) = body(st)
      #(mk_undefined(), st)
    })
  let assert ThrowCompletion(err) = c
  let #(ctor, st) = rt_obj.get_prop(st, err, key("constructor"))
  let #(name, _) = rt_obj.get_prop(st, ctor, key("name"))
  let assert KStr(name) = classify(name)
  name
}

fn fixture() -> #(Handle, JsVal, Handle, Handle, Agent) {
  let st = agent()
  let #(box_a, st) = rt_store.cell_new(st, SBox(mk_int(1)))
  let #(box_b, st) = rt_store.cell_new(st, SBox(mk_tdz()))
  let exports = dict.from_list([#("b", box_b), #("a", box_a)])
  let #(ns_h, st) =
    rt_store.cell_new(st, rt_obj.module_namespace_cell(exports, "Module"))
  #(ns_h, mk_object(ns_h), box_a, box_b, st)
}

pub fn get_reads_the_live_binding_test() {
  let #(_, ns, box_a, _, st) = fixture()
  let #(v, st) = rt_obj.get_prop(st, ns, key("a"))
  assert classify(v) == KNum(JInt(1))
  let st = rt_store.cell_set(st, box_a, SBox(mk_int(2)))
  let #(v, st) = rt_obj.get_prop(st, ns, key("a"))
  assert classify(v) == KNum(JInt(2))
  let #(v, st) = rt_obj.get_prop(st, ns, key("toString"))
  assert classify(v) == KUndef
  let #(v, _) = rt_obj.get_prop(st, ns, key("nope"))
  assert classify(v) == KUndef
}

pub fn tdz_binding_is_a_reference_error_test() {
  let #(ns_h, ns, _, box_b, st) = fixture()
  assert throws(st, rt_obj.get_prop(_, ns, key("b"))) == "ReferenceError"
  assert throws(st, rt_obj.get_own_property(_, ns_h, key("b")))
    == "ReferenceError"
  assert throws(st, rt_obj.for_in_keys(_, ns)) == "ReferenceError"
  let #(object, st) = rt_lang.global_get(st, <<"Object">>)
  assert throws(st, rt_call.call_method(_, object, key("keys"), [ns]))
    == "ReferenceError"
  let #(has, st) = rt_obj.has_prop(st, ns, key("b"))
  assert has
  let #(keys, st) = rt_obj.own_keys(st, ns_h)
  assert list.length(keys) == 3
  let st = rt_store.cell_set(st, box_b, SBox(mk_int(3)))
  let #(v, _) = rt_obj.get_prop(st, ns, key("b"))
  assert classify(v) == KNum(JInt(3))
}

pub fn own_keys_are_sorted_exports_then_to_string_tag_test() {
  let #(ns_h, ns, _, box_b, st) = fixture()
  let #(keys, st) = rt_obj.own_keys(st, ns_h)
  assert keys
    == [
      StringKey(Named("a")),
      StringKey(Named("b")),
      SymbolKey(types.symbol_to_string_tag),
    ]
  let st = rt_store.cell_set(st, box_b, SBox(mk_int(3)))
  let #(names, st) = rt_obj.for_in_keys(st, ns)
  assert list.map(names, classify) == [KStr("a"), KStr("b")]
  let #(object, st) = rt_lang.global_get(st, <<"Object">>)
  let #(object_proto, st) = rt_obj.get_prop(st, object, key("prototype"))
  let #(to_string, st) = rt_obj.get_prop(st, object_proto, key("toString"))
  let #(tag, _) = rt_call.call(st, to_string, ns, [])
  assert classify(tag) == KStr("[object Module]")
}

pub fn descriptor_shape_test() {
  let #(ns_h, _, _, _, st) = fixture()
  let #(d, st) = rt_obj.get_own_property(st, ns_h, key("a"))
  let assert Some(DataProperty(
    value:,
    writable: True,
    enumerable: True,
    configurable: False,
    ..,
  )) = d
  assert classify(value) == KNum(JInt(1))
  let #(d, st) = rt_obj.get_own_property(st, ns_h, key("nope"))
  assert d == None
  let #(d, _) =
    rt_obj.get_own_property(st, ns_h, SymbolKey(types.symbol_to_string_tag))
  let assert Some(DataProperty(
    value:,
    writable: False,
    enumerable: False,
    configurable: False,
    ..,
  )) = d
  assert classify(value) == KStr("Module")
}

pub fn writes_and_deletes_fail_test() {
  let #(ns_h, ns, box_a, _, st) = fixture()
  let #(ok, st) = rt_obj.set_prop(st, ns, key("a"), mk_int(9))
  assert !ok
  let #(ok, st) = rt_obj.set_prop(st, ns, key("fresh"), mk_int(9))
  assert !ok
  let assert SBox(value:) = rt_store.cell_get(st, box_a)
  assert classify(value) == KNum(JInt(1))
  let #(other, st) = rt_obj.new_object_literal(st)
  let assert types.KHandle(other_h) = classify(other)
  assert throws(st, rt_obj.set_prop_with_receiver(
      _,
      other_h,
      key("b"),
      mk_int(1),
      ns,
    ))
    == "ReferenceError"
  let #(ok, st) =
    rt_obj.set_prop_with_receiver(st, other_h, key("a"), mk_int(1), ns)
  assert !ok
  let #(ok, st) = rt_obj.delete_prop(st, ns_h, key("a"))
  assert !ok
  let #(ok, st) = rt_obj.delete_prop(st, ns_h, key("nope"))
  assert ok
  let #(ok, _) =
    rt_obj.delete_prop(st, ns_h, SymbolKey(types.symbol_to_string_tag))
  assert !ok
}

pub fn prototype_and_extensibility_test() {
  let #(ns_h, _, _, _, st) = fixture()
  let #(proto, st) = rt_obj.get_prototype_of(st, ns_h)
  assert proto == None
  let #(ext, st) = rt_obj.is_extensible(st, ns_h)
  assert !ext
  let #(ok, st) = rt_obj.prevent_extensions(st, ns_h)
  assert ok
  let #(res, st) = rt_obj.set_prototype_of(st, ns_h, None)
  assert res == Ok(Nil)
  let #(res, _) =
    rt_obj.set_prototype_of(st, ns_h, Some(st.realm.object.prototype))
  assert res != Ok(Nil)
}

fn value_desc(v: JsVal) -> ParsedDesc {
  ParsedDesc(
    value: Some(v),
    get: None,
    set: None,
    writable: None,
    enumerable: None,
    configurable: None,
  )
}

pub fn define_own_property_only_accepts_no_ops_test() {
  let #(ns_h, _, _, _, st) = fixture()
  let none = ParsedDesc(..value_desc(mk_int(0)), value: None)
  let #(ok, st) =
    rt_obj.define_own_prop(st, ns_h, key("a"), value_desc(mk_int(1)))
  assert ok
  let #(ok, st) =
    rt_obj.define_own_prop(st, ns_h, key("a"), value_desc(mk_int(9)))
  assert !ok
  let #(ok, st) = rt_obj.define_own_prop(st, ns_h, key("a"), none)
  assert ok
  let #(ok, st) =
    rt_obj.define_own_prop(
      st,
      ns_h,
      key("a"),
      ParsedDesc(..none, configurable: Some(True)),
    )
  assert !ok
  let #(ok, st) =
    rt_obj.define_own_prop(
      st,
      ns_h,
      key("a"),
      ParsedDesc(..none, enumerable: Some(False)),
    )
  assert !ok
  let #(ok, st) =
    rt_obj.define_own_prop(
      st,
      ns_h,
      key("a"),
      ParsedDesc(..none, writable: Some(False)),
    )
  assert !ok
  let #(ok, st) =
    rt_obj.define_own_prop(
      st,
      ns_h,
      key("a"),
      ParsedDesc(..none, get: Some(mk_undefined())),
    )
  assert !ok
  let #(ok, st) =
    rt_obj.define_own_prop(st, ns_h, key("nope"), value_desc(mk_int(1)))
  assert !ok
  assert throws(st, rt_obj.define_own_prop(
      _,
      ns_h,
      key("b"),
      value_desc(mk_int(1)),
    ))
    == "ReferenceError"
  let #(object, st) = rt_lang.global_get(st, <<"Object">>)
  let #(desc, st) = rt_obj.new_object_literal(st)
  let #(_, st) = rt_obj.set_prop(st, desc, key("value"), mk_int(9))
  assert throws(
      st,
      rt_call.call_method(_, object, key("defineProperty"), [
        mk_object(ns_h),
        mk_string("a"),
        desc,
      ]),
    )
    == "TypeError"
}

pub fn binding_boxes_survive_collection_test() {
  let #(ns_h, ns, box_a, box_b, st) = fixture()
  let st = rt_lang.global_set(st, <<"ns">>, ns)
  let st = rt_gc.collect(st, [])
  assert rt_gc.is_live(st, ns_h)
  assert rt_gc.is_live(st, box_a)
  assert rt_gc.is_live(st, box_b)
  let #(v, _) = rt_obj.get_prop(st, ns, key("a"))
  assert classify(v) == KNum(JInt(1))
}
