import arc/interp/ffi
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type JsVal, Index, JFloat, JInt, KHandle, KNum, KStr, KUndef, Named, StringKey,
  classify, mk_number, mk_string, mk_undefined,
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
  assert classify(ffi.get_field(st, obj, "x")) == KNum(JInt(42))
  assert classify(ffi.get_field(st, obj, "missing")) == KUndef
  // Inherited data property walks the chain; an accessor misses.
  let assert KHandle(_) = classify(ffi.get_field(st, obj, "constructor"))
  assert ffi.is_miss(ffi.get_field(st, obj, "__proto__"))
  // Primitives: String "length" is virtual, anything else reads the realm
  // wrapper prototype; a getter there misses.
  assert classify(ffi.get_field(st, mk_string("héllo"), "length"))
    == KNum(JInt(5))
  let assert KHandle(_) = classify(ffi.get_field(st, mk_string("s"), "slice"))
  assert classify(ffi.get_field(st, mk_string("s"), "nope")) == KUndef
  let assert KHandle(_) = classify(ffi.get_field(st, mk_int(1), "toFixed"))
  assert ffi.is_miss(ffi.get_field(st, mk_string("s"), "__proto__"))
  assert ffi.is_miss(ffi.get_field(st, mk_undefined(), "x"))
  assert ffi.type_of_in(st.store, obj) == "object"
  // Overwrite through the kernel, read back through the runtime.
  let store = ffi.put_field(st.store, obj, "x", mk_int(43))
  assert !ffi.is_miss(store)
  let st = types.Agent(..st, store:)
  let #(v, st) = rt_obj.t_get_prop(st, obj, StringKey(Named("x")))
  assert classify(v) == KNum(JInt(43))
  // Creation on an extensible receiver whose chain holds nothing at the
  // key: a fresh {W,E,C} property stamped after the existing ones.
  let store = ffi.put_field(st.store, obj, "y", mk_int(1))
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
  // An accessor up the chain and a non-extensible receiver miss.
  assert ffi.is_miss(ffi.put_field(st.store, obj, "__proto__", mk_int(1)))
  let #(_, st) = rt_obj.t_prevent_extensions(st, handle_of(obj))
  assert ffi.is_miss(ffi.put_field(st.store, obj, "z", mk_int(1)))
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

/// §10.1.9.2 step 2: appending under an inherited index setter must run the
/// setter, so the kernel leaves it to the full [[Set]].
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
  // Overwriting a present own element never consults the chain.
  assert !ffi.is_miss(ffi.put_elem(st.store, arr, mk_int(1), mk_int(21)))
}

/// An inherited non-writable data property at the index rejects the write,
/// so filling a hole under one misses.
pub fn put_elem_inherited_readonly_on_hole_fill_misses_test() {
  let st = rt_helpers.agent()
  let #(arr, st) = rt_obj.t_new_array(st, [mk_int(0), mk_int(1), mk_int(2)])
  let #(_, st) = rt_obj.t_delete_prop(st, handle_of(arr), StringKey(Index(1)))
  // A hole over a clean chain fills in place.
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

/// §10.4.2.1 step 2.h: no append past a non-writable "length".
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

/// A sparse array's absent index is a hole too: an inherited setter on the
/// chain takes the store.
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

/// 2^32-1 is not an array index: writing it never grows "length".
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
  // The last real index below that length is a plain sparse hole fill.
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
  // Reading a property of undefined throws TypeError inside the runtime.
  let assert ffi.Threw(agent:, thrown:) =
    ffi.guard3(rt_obj.t_get_prop, st, mk_undefined(), StringKey(Named("x")))
  let #(msg, _) = rt_val.t_to_string(agent, thrown)
  assert msg == "TypeError: Cannot read properties of undefined (reading 'x')"
}
