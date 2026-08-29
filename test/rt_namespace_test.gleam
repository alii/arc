import arc/rt/builtins as rt_builtins
import arc/rt/call.{ThrowCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/name_keys as nk
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type ParsedDesc, DataProperty, JInt, KNum,
  KStr, KUndef, ParsedDesc, SBox, StringKey, SymbolKey, classify, mk_number,
  mk_object, mk_string, mk_tdz, mk_undefined,
}
import gleam/list
import gleam/option.{None, Some}
import rt_helpers

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn t_apply_protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(rt_call.Completion, Agent)

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks())
}

fn int(i: Int) -> JsVal {
  mk_number(JInt(i))
}

fn throws(st: Agent, body: fn(Agent) -> #(a, Agent)) -> String {
  let #(c, st) =
    t_apply_protected(st, fn(st) {
      let #(_, st) = body(st)
      #(mk_undefined(), st)
    })
  let assert ThrowCompletion(err) = c
  let #(k_, st) = rt_helpers.key(st, "constructor")
  let #(ctor, st) = rt_obj.t_get_prop(st, err, k_)
  let #(k_, st) = rt_helpers.key(st, "name")
  let #(name, _) = rt_obj.t_get_prop(st, ctor, k_)
  let assert KStr(name) = classify(name)
  name
}

fn fixture() -> #(Agent, Handle, JsVal, Handle, Handle) {
  let st = agent()
  let #(cell_a, st) = rt_store.t_cell_new(st, SBox(int(1)))
  let #(cell_b, st) = rt_store.t_cell_new(st, SBox(mk_tdz()))
  let #(ns_h, st) =
    rt_obj.t_new_module_namespace(st, [#("b", cell_b), #("a", cell_a)])
  #(st, ns_h, mk_object(ns_h), cell_a, cell_b)
}

pub fn get_reads_the_live_binding_test() {
  let #(st, _, ns, cell_a, _) = fixture()
  let #(k_, st) = rt_helpers.key(st, "a")
  let #(v, st) = rt_obj.t_get_prop(st, ns, k_)
  assert classify(v) == KNum(JInt(1))
  let st = rt_store.t_cell_set(st, cell_a, SBox(int(2)))
  let #(k_, st) = rt_helpers.key(st, "a")
  let #(v, st) = rt_obj.t_get_prop(st, ns, k_)
  assert classify(v) == KNum(JInt(2))
  let #(k_, st) = rt_helpers.key(st, "toString")
  let #(v, st) = rt_obj.t_get_prop(st, ns, k_)
  assert classify(v) == KUndef
  let #(k_, st) = rt_helpers.key(st, "nope")
  let #(v, _) = rt_obj.t_get_prop(st, ns, k_)
  assert classify(v) == KUndef
}

pub fn tdz_binding_is_a_reference_error_test() {
  let #(st, ns_h, ns, _, cell_b) = fixture()
  let #(k_, st) = rt_helpers.key(st, "b")
  assert throws(st, rt_obj.t_get_prop(_, ns, k_)) == "ReferenceError"
  let #(k_, st) = rt_helpers.key(st, "b")
  assert throws(st, rt_obj.t_get_own_property(_, ns_h, k_)) == "ReferenceError"
  assert throws(st, rt_obj.t_for_in_keys(_, ns)) == "ReferenceError"
  let #(object, st) = rt_obj.t_global_get(st, <<"Object">>)
  let #(k_, st) = rt_helpers.key(st, "keys")
  assert throws(st, rt_call.t_call_method(_, object, k_, [ns]))
    == "ReferenceError"
  let #(k_, st) = rt_helpers.key(st, "b")
  let #(has, st) = rt_obj.t_has_prop(st, ns, k_)
  assert has
  let #(keys, st) = rt_obj.t_own_keys(st, ns_h)
  assert list.length(keys) == 3
  let st = rt_store.t_cell_set(st, cell_b, SBox(int(3)))
  let #(k_, st) = rt_helpers.key(st, "b")
  let #(v, _) = rt_obj.t_get_prop(st, ns, k_)
  assert classify(v) == KNum(JInt(3))
}

pub fn own_keys_are_sorted_exports_then_to_string_tag_test() {
  let #(st, ns_h, ns, _, cell_b) = fixture()
  let #(keys, st) = rt_obj.t_own_keys(st, ns_h)
  assert keys
    == [
      rt_helpers.key(st, "a").0,
      rt_helpers.key(st, "b").0,
      SymbolKey(types.symbol_to_string_tag),
    ]
  let st = rt_store.t_cell_set(st, cell_b, SBox(int(3)))
  let #(names, st) = rt_obj.t_for_in_keys(st, ns)
  assert list.map(names, classify) == [KStr("a"), KStr("b")]
  let #(object, st) = rt_obj.t_global_get(st, <<"Object">>)
  let #(k_, st) = rt_helpers.key(st, "prototype")
  let #(object_proto, st) = rt_obj.t_get_prop(st, object, k_)
  let #(k_, st) = rt_helpers.key(st, "toString")
  let #(to_string, st) = rt_obj.t_get_prop(st, object_proto, k_)
  let #(tag, _) = rt_call.t_call_checked(st, to_string, ns, [])
  assert classify(tag) == KStr("[object Module]")
}

pub fn descriptor_shape_test() {
  let #(st, ns_h, _, _, _) = fixture()
  let #(k_, st) = rt_helpers.key(st, "a")
  let #(d, st) = rt_obj.t_get_own_property(st, ns_h, k_)
  let assert Some(DataProperty(
    value:,
    writable: True,
    enumerable: True,
    configurable: False,
    ..,
  )) = d
  assert classify(value) == KNum(JInt(1))
  let #(k_, st) = rt_helpers.key(st, "nope")
  let #(d, st) = rt_obj.t_get_own_property(st, ns_h, k_)
  assert d == None
  let #(d, _) =
    rt_obj.t_get_own_property(st, ns_h, SymbolKey(types.symbol_to_string_tag))
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
  let #(st, ns_h, ns, cell_a, _) = fixture()
  let #(k_, st) = rt_helpers.key(st, "a")
  let #(ok, st) = rt_obj.t_set_prop(st, ns, k_, int(9))
  assert !ok
  let #(k_, st) = rt_helpers.key(st, "fresh")
  let #(ok, st) = rt_obj.t_set_prop(st, ns, k_, int(9))
  assert !ok
  let assert SBox(value:) = rt_store.t_cell_get(st, cell_a)
  assert classify(value) == KNum(JInt(1))
  let #(other, st) = rt_obj.t_new_object_literal(st)
  let assert types.KHandle(other_h) = classify(other)
  let #(k_b, st) = rt_helpers.key(st, "b")
  assert throws(st, rt_obj.t_set_prop_with_receiver(_, other_h, k_b, int(1), ns))
    == "ReferenceError"
  let #(k_, st) = rt_helpers.key(st, "a")
  let #(ok, st) = rt_obj.t_set_prop_with_receiver(st, other_h, k_, int(1), ns)
  assert !ok
  let #(k_, st) = rt_helpers.key(st, "a")
  let #(ok, st) = rt_obj.t_delete_prop(st, ns_h, k_)
  assert !ok
  let #(k_, st) = rt_helpers.key(st, "nope")
  let #(ok, st) = rt_obj.t_delete_prop(st, ns_h, k_)
  assert ok
  let #(ok, _) =
    rt_obj.t_delete_prop(st, ns_h, SymbolKey(types.symbol_to_string_tag))
  assert !ok
}

pub fn prototype_and_extensibility_test() {
  let #(st, ns_h, _, _, _) = fixture()
  let #(proto, st) = rt_obj.t_get_prototype_of(st, ns_h)
  assert proto == None
  let #(ext, st) = rt_obj.t_is_extensible(st, ns_h)
  assert !ext
  let #(ok, st) = rt_obj.t_prevent_extensions(st, ns_h)
  assert ok
  let #(ok, st) = rt_obj.t_set_prototype(st, ns_h, None)
  assert ok
  let #(ok, _) =
    rt_obj.t_set_prototype(st, ns_h, Some(st.realm.object.prototype))
  assert !ok
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
  let #(st, ns_h, _, _, _) = fixture()
  let none = ParsedDesc(..value_desc(int(0)), value: None)
  let #(k_, st) = rt_helpers.key(st, "a")
  let #(ok, st) = rt_obj.t_define_own_prop(st, ns_h, k_, value_desc(int(1)))
  assert ok
  let #(k_, st) = rt_helpers.key(st, "a")
  let #(ok, st) = rt_obj.t_define_own_prop(st, ns_h, k_, value_desc(int(9)))
  assert !ok
  let #(k_, st) = rt_helpers.key(st, "a")
  let #(ok, st) = rt_obj.t_define_own_prop(st, ns_h, k_, none)
  assert ok
  let #(k_a, st) = rt_helpers.key(st, "a")
  let #(ok, st) =
    rt_obj.t_define_own_prop(
      st,
      ns_h,
      k_a,
      ParsedDesc(..none, configurable: Some(True)),
    )
  assert !ok
  let #(k_a, st) = rt_helpers.key(st, "a")
  let #(ok, st) =
    rt_obj.t_define_own_prop(
      st,
      ns_h,
      k_a,
      ParsedDesc(..none, enumerable: Some(False)),
    )
  assert !ok
  let #(k_a, st) = rt_helpers.key(st, "a")
  let #(ok, st) =
    rt_obj.t_define_own_prop(
      st,
      ns_h,
      k_a,
      ParsedDesc(..none, writable: Some(False)),
    )
  assert !ok
  let #(k_a, st) = rt_helpers.key(st, "a")
  let #(ok, st) =
    rt_obj.t_define_own_prop(
      st,
      ns_h,
      k_a,
      ParsedDesc(..none, get: Some(mk_undefined())),
    )
  assert !ok
  let #(k_, st) = rt_helpers.key(st, "nope")
  let #(ok, st) = rt_obj.t_define_own_prop(st, ns_h, k_, value_desc(int(1)))
  assert !ok
  let #(k_b, st) = rt_helpers.key(st, "b")
  assert throws(st, rt_obj.t_define_own_prop(_, ns_h, k_b, value_desc(int(1))))
    == "ReferenceError"
  let #(object, st) = rt_obj.t_global_get(st, <<"Object">>)
  let #(desc, st) = rt_obj.t_new_object_literal(st)
  let #(k_, st) = rt_helpers.key(st, "value")
  let #(_, st) = rt_obj.t_set_prop(st, desc, k_, int(9))
  assert throws(
      st,
      rt_call.t_call_method(_, object, StringKey(nk.define_property), [
        mk_object(ns_h),
        mk_string("a"),
        desc,
      ]),
    )
    == "TypeError"
}

pub fn binding_cells_survive_collection_test() {
  let #(st, ns_h, ns, cell_a, cell_b) = fixture()
  let st = rt_obj.t_global_set(st, <<"ns">>, ns)
  let st = rt_gc.t_collect(st, [])
  assert rt_gc.t_is_live(st, ns_h)
  assert rt_gc.t_is_live(st, cell_a)
  assert rt_gc.t_is_live(st, cell_b)
  let #(k_, st) = rt_helpers.key(st, "a")
  let #(v, _) = rt_obj.t_get_prop(st, ns, k_)
  assert classify(v) == KNum(JInt(1))
}
