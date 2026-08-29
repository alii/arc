import arc/bytecode/key.{Index, Named}
import arc/interp/ffi
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type JsVal, JFloat, JInt, KHandle, KNum, KStr, KUndef, StringKey, classify,
  mk_number, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/option.{None, Some}
import rt_helpers

fn mk_int(i: Int) -> JsVal {
  mk_number(JInt(i))
}

pub fn add_numbers_and_strings_test() {
  assert classify(ffi.add(mk_int(1), mk_int(2))) == KNum(JInt(3))
  assert classify(ffi.add(mk_int(1), mk_number(JFloat(0.5))))
    == KNum(JFloat(1.5))
  assert classify(ffi.add(mk_string("a"), mk_int(1))) == KStr("a1")
  assert !ffi.is_miss(ffi.add(mk_int(1), mk_int(2)))
}

pub fn add_object_misses_test() {
  let st = rt_helpers.agent()
  let #(obj, _st) = rt_obj.t_new_object_literal(st)
  assert ffi.is_miss(ffi.add(obj, mk_int(1)))
  assert ffi.is_miss(ffi.sub(mk_string("3"), mk_int(1)))
}

pub fn compare_and_equality_test() {
  assert ffi.truthy(ffi.lt(mk_int(1), mk_number(JFloat(1.5))))
  assert !ffi.is_miss(ffi.lt(mk_int(1), mk_int(2)))
  assert ffi.is_miss(ffi.lt(mk_string("1"), mk_int(2)))
  assert ffi.truthy(ffi.strict_eq(mk_int(1), mk_number(JFloat(1.0))))
  assert !ffi.truthy(ffi.strict_eq(mk_int(1), mk_string("1")))
  assert ffi.truthy(ffi.strict_neq(mk_int(1), mk_string("1")))
  assert ffi.is_miss(ffi.neq(mk_int(1), mk_string("1")))
  assert !ffi.truthy(ffi.neq(mk_int(1), mk_number(JFloat(1.0))))
  assert classify(ffi.step(mk_int(1), 1)) == KNum(JInt(2))
  assert classify(ffi.step(mk_number(JFloat(1.5)), -1)) == KNum(JFloat(0.5))
  assert ffi.is_miss(ffi.step(mk_string("1"), 1))
  assert ffi.is_miss(ffi.eq(mk_int(1), mk_string("1")))
  assert ffi.truthy(mk_string("x"))
  assert !ffi.truthy(mk_int(0))
  assert ffi.nullish(mk_undefined())
  assert ffi.type_of(mk_int(0)) == "number"
}

pub fn get_and_put_field_test() {
  let st = rt_helpers.agent()
  let #(obj, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) = rt_obj.t_set_prop(st, obj, StringKey(Named("x")), mk_int(42))
  assert classify(ffi.get_field(st, obj, key.Named("x"))) == KNum(JInt(42))
  assert classify(ffi.get_field(st, obj, key.Named("missing"))) == KUndef
  let assert KHandle(_) =
    classify(ffi.get_field(st, obj, key.Named("constructor")))
  assert ffi.is_miss(ffi.get_field(st, obj, key.Named("__proto__")))
  assert classify(ffi.get_field(st, mk_string("héllo"), key.Named("length")))
    == KNum(JInt(5))
  let assert KHandle(_) =
    classify(ffi.get_field(st, mk_string("s"), key.Named("slice")))
  assert classify(ffi.get_field(st, mk_string("s"), key.Named("nope")))
    == KUndef
  let assert KHandle(_) =
    classify(ffi.get_field(st, mk_int(1), key.Named("toFixed")))
  assert ffi.is_miss(ffi.get_field(st, mk_string("s"), key.Named("__proto__")))
  assert ffi.is_miss(ffi.get_field(st, mk_undefined(), key.Named("x")))
  assert ffi.type_of_in(st.store, obj) == "object"
  let store = ffi.put_field(st.store, obj, key.Named("x"), mk_int(43), True)
  assert !ffi.is_miss(store)
  let st = types.Agent(..st, store:)
  let #(v, st) = rt_obj.t_get_prop(st, obj, StringKey(Named("x")))
  assert classify(v) == KNum(JInt(43))
  let store = ffi.put_field(st.store, obj, key.Named("y"), mk_int(1), True)
  assert !ffi.is_miss(store)
  let st = types.Agent(..st, store:)
  let #(keys, st) = rt_obj.t_own_keys(st, handle_of(obj))
  assert keys == [StringKey(Named("x")), StringKey(Named("y"))]
  let #(desc, st) =
    rt_obj.t_get_own_property(st, handle_of(obj), StringKey(Named("y")))
  let assert Some(types.DataProperty(
    writable: True,
    enumerable: True,
    configurable: True,
    ..,
  )) = desc
  assert ffi.is_miss(ffi.put_field(
    st.store,
    obj,
    key.Named("__proto__"),
    mk_int(1),
    True,
  ))
  let #(_, st) = rt_obj.t_prevent_extensions(st, handle_of(obj))
  assert ffi.is_miss(ffi.put_field(
    st.store,
    obj,
    key.Named("z"),
    mk_int(1),
    True,
  ))
}

pub fn get_and_put_elem_test() {
  let st = rt_helpers.agent()
  let #(arr, st) = rt_obj.t_new_array(st, [mk_int(10), mk_int(20)])
  assert classify(ffi.get_elem(st.store, arr, mk_int(1))) == KNum(JInt(20))
  assert ffi.is_miss(ffi.get_elem(st.store, arr, mk_int(2)))
  assert classify(ffi.get_elem(st.store, arr, mk_string("0"))) == KNum(JInt(10))
  let store = ffi.put_elem(st.store, arr, mk_int(2), mk_int(30))
  assert !ffi.is_miss(store)
  let st = types.Agent(..st, store:)
  let #(len, st) = rt_obj.t_get_prop(st, arr, StringKey(Named("length")))
  assert classify(len) == KNum(JInt(3))
  assert ffi.is_miss(ffi.put_elem(st.store, arr, mk_int(5), mk_int(1)))
}

fn handle_of(v: JsVal) -> types.Handle {
  let assert KHandle(h) = classify(v)
  h
}

fn array_prototype(
  st: types.Agent,
  arr: JsVal,
) -> #(types.Handle, types.Agent) {
  let #(proto, st) = rt_obj.t_get_prototype_of(st, handle_of(arr))
  let assert Some(p) = proto
  #(p, st)
}

pub fn put_elem_inherited_setter_on_append_misses_test() {
  let st = rt_helpers.agent()
  let #(arr, st) = rt_obj.t_new_array(st, [mk_int(10), mk_int(20)])
  let #(proto, st) = array_prototype(st, arr)
  let #(setter, st) =
    rt_helpers.func(st, fn(st, _args) { #(mk_undefined(), st) })
  let #(_, st) =
    rt_obj.t_define_own_accessor(
      st,
      proto,
      StringKey(Index(2)),
      None,
      Some(setter),
      True,
      True,
    )
  assert ffi.is_miss(ffi.put_elem(st.store, arr, mk_int(2), mk_int(30)))
  assert !ffi.is_miss(ffi.put_elem(st.store, arr, mk_int(1), mk_int(21)))
}

pub fn put_elem_inherited_readonly_on_hole_fill_misses_test() {
  let st = rt_helpers.agent()
  let #(arr, st) = rt_obj.t_new_array(st, [mk_int(0), mk_int(1), mk_int(2)])
  let #(_, st) = rt_obj.t_delete_prop(st, handle_of(arr), StringKey(Index(1)))
  assert !ffi.is_miss(ffi.put_elem(st.store, arr, mk_int(1), mk_int(9)))
  let #(proto, st) = array_prototype(st, arr)
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      proto,
      StringKey(Index(1)),
      mk_string("proto"),
      False,
      True,
      True,
    )
  assert ffi.is_miss(ffi.put_elem(st.store, arr, mk_int(1), mk_int(9)))
}

pub fn put_elem_frozen_length_on_append_misses_test() {
  let st = rt_helpers.agent()
  let #(arr, st) = rt_obj.t_new_array(st, [mk_int(1), mk_int(2)])
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      handle_of(arr),
      StringKey(Named("length")),
      mk_int(2),
      False,
      False,
      False,
    )
  assert ffi.is_miss(ffi.put_elem(st.store, arr, mk_int(2), mk_int(3)))
  assert !ffi.is_miss(ffi.put_elem(st.store, arr, mk_int(0), mk_int(7)))
}

pub fn put_elem_sparse_hole_walks_chain_test() {
  let st = rt_helpers.agent()
  let #(arr, st) = rt_obj.t_new_array(st, [])
  let #(_, st) =
    rt_obj.t_set_prop(st, arr, StringKey(Index(100_000)), mk_int(1))
  assert !ffi.is_miss(ffi.put_elem(st.store, arr, mk_int(5), mk_int(7)))
  let #(object_ctor, st) = rt_helpers.global(st, "Object")
  let #(object_proto, st) = rt_helpers.get(st, object_ctor, "prototype")
  let #(setter, st) =
    rt_helpers.func(st, fn(st, _args) { #(mk_undefined(), st) })
  let #(_, st) =
    rt_obj.t_define_own_accessor(
      st,
      handle_of(object_proto),
      StringKey(Index(5)),
      None,
      Some(setter),
      True,
      True,
    )
  assert ffi.is_miss(ffi.put_elem(st.store, arr, mk_int(5), mk_int(7)))
}

pub fn put_elem_past_index_range_misses_test() {
  let st = rt_helpers.agent()
  let #(arr, st) = rt_obj.t_new_array(st, [])
  let #(_, st) =
    rt_obj.t_set_prop(st, arr, StringKey(Index(100_000)), mk_int(1))
  let #(_, st) =
    rt_obj.t_set_prop(
      st,
      arr,
      StringKey(Named("length")),
      mk_int(4_294_967_295),
    )
  assert ffi.is_miss(ffi.put_elem(
    st.store,
    arr,
    mk_int(4_294_967_295),
    mk_int(1),
  ))
  assert !ffi.is_miss(ffi.put_elem(
    st.store,
    arr,
    mk_int(4_294_967_294),
    mk_int(1),
  ))
}

pub fn guard_catches_js_throw_test() {
  let st = rt_helpers.agent()
  let #(obj, st) = rt_obj.t_new_object_literal(st)
  let assert ffi.Ok(value:, agent: _) =
    ffi.guard3(rt_obj.t_get_prop, st, obj, StringKey(Named("nope")))
  assert classify(value) == KUndef
  let assert ffi.Threw(agent:, thrown:) =
    ffi.guard3(rt_obj.t_get_prop, st, mk_undefined(), StringKey(Named("x")))
  let #(msg, _) = rt_val.t_to_string(agent, thrown)
  assert msg == "TypeError: Cannot read properties of undefined (reading 'x')"
}
