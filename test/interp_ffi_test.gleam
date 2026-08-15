import arc/interp/ffi
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type JsVal, JFloat, JInt, KHandle, KNum, KStr, KUndef, Named, StringKey,
  classify, mk_number, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
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
  assert ffi.lt(mk_int(1), mk_number(JFloat(1.5)))
  assert !ffi.is_miss(ffi.lt(mk_int(1), mk_int(2)))
  assert ffi.is_miss(ffi.lt(mk_string("1"), mk_int(2)))
  assert ffi.strict_eq(mk_int(1), mk_number(JFloat(1.0)))
  assert !ffi.strict_eq(mk_int(1), mk_string("1"))
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
  assert classify(ffi.get_field(st.store, obj, "x")) == KNum(JInt(42))
  assert classify(ffi.get_field(st.store, obj, "missing")) == KUndef
  // Inherited data property walks the chain; an accessor misses.
  let assert KHandle(_) = classify(ffi.get_field(st.store, obj, "constructor"))
  assert ffi.is_miss(ffi.get_field(st.store, obj, "__proto__"))
  assert ffi.type_of_in(st.store, obj) == "object"
  // Overwrite through the kernel, read back through the runtime.
  let store = ffi.put_field(st.store, obj, "x", mk_int(43))
  assert !ffi.is_miss(store)
  let st = types.Agent(..st, store:)
  let #(v, st) = rt_obj.t_get_prop(st, obj, StringKey(Named("x")))
  assert classify(v) == KNum(JInt(43))
  // Creating a property is not the kernel's job.
  assert ffi.is_miss(ffi.put_field(st.store, obj, "y", mk_int(1)))
}

pub fn get_and_put_elem_test() {
  let st = rt_helpers.agent()
  let #(arr, st) = rt_obj.t_new_array(st, [mk_int(10), mk_int(20)])
  assert classify(ffi.get_elem(st.store, arr, mk_int(1))) == KNum(JInt(20))
  assert ffi.is_miss(ffi.get_elem(st.store, arr, mk_int(2)))
  assert classify(ffi.get_elem(st.store, arr, mk_string("0"))) == KNum(JInt(10))
  // Append at length bumps the array length.
  let store = ffi.put_elem(st.store, arr, mk_int(2), mk_int(30))
  assert !ffi.is_miss(store)
  let st = types.Agent(..st, store:)
  let #(len, st) = rt_obj.t_get_prop(st, arr, StringKey(Named("length")))
  assert classify(len) == KNum(JInt(3))
  assert ffi.is_miss(ffi.put_elem(st.store, arr, mk_int(5), mk_int(1)))
}

pub fn guard_catches_js_throw_test() {
  let st = rt_helpers.agent()
  let #(obj, st) = rt_obj.t_new_object_literal(st)
  let assert ffi.Ok(value:, agent: _) =
    ffi.guard3(rt_obj.t_get_prop, st, obj, StringKey(Named("nope")))
  assert classify(value) == KUndef
  // Reading a property of undefined throws TypeError inside the runtime.
  let assert ffi.Threw(agent:, thrown:) =
    ffi.guard3(rt_obj.t_get_prop, st, mk_undefined(), StringKey(Named("x")))
  let #(msg, _) = rt_val.t_to_string(agent, thrown)
  assert msg == "TypeError: Cannot read properties of undefined (reading 'x')"
}
