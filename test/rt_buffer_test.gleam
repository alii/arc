//// ArrayBuffer / SharedArrayBuffer / TypedArray / DataView / Atomics on the
//// arc/rt runtime, driven through the runtime's own construct/call/property
//// primitives.

import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type JsVal, JFloat, JInt, KBig, KBool, KHandle, KNum, KStr, KUndef,
  StringKey, canonical_key, classify, mk_bigint, mk_bool, mk_number, mk_object,
  mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{Some}
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

fn ints(xs: List(Int)) -> List(JsVal) {
  list.map(xs, int)
}

fn global(st: Agent, name: String) -> JsVal {
  let #(v, _) = rt_obj.t_global_get(st, <<name:utf8>>)
  v
}

/// `new Name(...args)`.
fn construct(st: Agent, name: String, args: List(JsVal)) -> #(JsVal, Agent) {
  let ctor = global(st, name)
  let #(h, st) = rt_call.t_construct(st, ctor, args, ctor)
  #(mk_object(h), st)
}

fn get(st: Agent, obj: JsVal, key: String) -> #(JsVal, Agent) {
  rt_obj.t_get_prop(st, obj, StringKey(canonical_key(key)))
}

fn get_(st: Agent, obj: JsVal, key: String) -> JsVal {
  get(st, obj, key).0
}

fn set(st: Agent, obj: JsVal, key: String, v: JsVal) -> Agent {
  let #(_, st) = rt_obj.t_set_prop(st, obj, StringKey(canonical_key(key)), v)
  st
}

/// `obj.name(...args)` as a completion.
fn attempt(
  st: Agent,
  obj: JsVal,
  name: String,
  args: List(JsVal),
) -> #(rt_call.Completion, Agent) {
  let #(f, st) = get(st, obj, name)
  rt_call.t_call(st, f, obj, args)
}

/// `obj.name(...args)`, asserting a normal completion.
fn invoke(
  st: Agent,
  obj: JsVal,
  name: String,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(c, st) = attempt(st, obj, name, args)
  let assert NormalCompletion(v) = c
  #(v, st)
}

/// `obj.name(...args)`, asserting it throws; the error-constructor name.
fn throws(st: Agent, obj: JsVal, name: String, args: List(JsVal)) -> String {
  let #(c, st) = attempt(st, obj, name, args)
  let assert ThrowCompletion(err) = c
  error_name(st, err)
}

/// `Ns.name(...args)` for a namespace / static function.
fn static(
  st: Agent,
  ns: String,
  name: String,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  invoke(st, global(st, ns), name, args)
}

fn array(st: Agent, xs: List(JsVal)) -> #(JsVal, Agent) {
  rt_obj.t_new_array(st, xs)
}

/// The error-constructor name of a thrown value.
fn error_name(st: Agent, err: JsVal) -> String {
  let ctor = get_(st, err, "constructor")
  let assert KStr(name) = classify(get_(st, ctor, "name"))
  name
}

fn joined(st: Agent, ta: JsVal) -> String {
  let #(s, _) = invoke(st, ta, "join", [])
  let assert KStr(s) = classify(s)
  s
}

fn options(st: Agent, key: String, v: JsVal) -> #(JsVal, Agent) {
  let #(h, st) = rt_obj.t_new_object(st, Some(st.realm.object.prototype))
  let o = mk_object(h)
  #(o, set(st, o, key, v))
}

pub fn uint8_from_array_index_get_set_length_test() {
  let st = agent()
  let #(src, st) = array(st, ints([1, 2, 300]))
  let #(ta, st) = construct(st, "Uint8Array", [src])
  let st = set(st, ta, "1", int(7))
  assert classify(get_(st, ta, "0")) == KNum(JInt(1))
  assert classify(get_(st, ta, "1")) == KNum(JInt(7))
  // 300 wraps mod 2^8.
  assert classify(get_(st, ta, "2")) == KNum(JInt(44))
  assert classify(get_(st, ta, "length")) == KNum(JInt(3))
  assert classify(get_(st, ta, "byteLength")) == KNum(JInt(3))
}

pub fn float64_over_buffer_with_byte_offset_test() {
  let st = agent()
  let #(buf, st) = construct(st, "ArrayBuffer", [int(24)])
  let #(f, st) = construct(st, "Float64Array", [buf, int(8)])
  assert classify(get_(st, f, "length")) == KNum(JInt(2))
  assert classify(get_(st, f, "byteOffset")) == KNum(JInt(8))
  let st = set(st, f, "0", mk_number(JFloat(1.5)))
  assert classify(get_(st, f, "0")) == KNum(JFloat(1.5))
  // 1.5 as a little-endian double: the last byte of element 0 is 0x3F at
  // buffer offset 15.
  let #(bytes, st) = construct(st, "Uint8Array", [buf])
  assert classify(get_(st, bytes, "15")) == KNum(JInt(63))
  assert get_(st, f, "buffer") == buf
  let _ = st
}

pub fn out_of_bounds_read_undefined_write_ignored_test() {
  let st = agent()
  let #(ta, st) = construct(st, "Int16Array", [int(2)])
  assert classify(get_(st, ta, "5")) == KUndef
  assert classify(get_(st, ta, "-1")) == KUndef
  let st = set(st, ta, "5", int(9))
  let st = set(st, ta, "1.5", int(9))
  assert classify(get_(st, ta, "5")) == KUndef
  assert classify(get_(st, ta, "length")) == KNum(JInt(2))
  // A canonical numeric string never becomes an ordinary own property.
  let assert KHandle(h) = classify(ta)
  let #(keys, _) = rt_obj.t_own_keys(st, h)
  assert keys == [StringKey(canonical_key("0")), StringKey(canonical_key("1"))]
}

pub fn resizable_buffer_length_tracking_view_test() {
  let st = agent()
  let #(opts, st) = options(st, "maxByteLength", int(16))
  let #(buf, st) = construct(st, "ArrayBuffer", [int(4), opts])
  assert classify(get_(st, buf, "resizable")) == KBool(True)
  let #(view, st) = construct(st, "Uint8Array", [buf])
  assert classify(get_(st, view, "length")) == KNum(JInt(4))
  let #(_, st) = invoke(st, buf, "resize", [int(12)])
  assert classify(get_(st, view, "length")) == KNum(JInt(12))
  let st = set(st, view, "11", int(5))
  assert classify(get_(st, view, "11")) == KNum(JInt(5))
  let #(_, st) = invoke(st, buf, "resize", [int(2)])
  assert classify(get_(st, view, "length")) == KNum(JInt(2))
  assert classify(get_(st, view, "11")) == KUndef
  // A fixed view that no longer fits is wholly out of bounds.
  let #(_, st) = invoke(st, buf, "resize", [int(8)])
  let #(fixed, st) = construct(st, "Uint8Array", [buf, int(4), int(4)])
  let #(_, st) = invoke(st, buf, "resize", [int(6)])
  assert classify(get_(st, fixed, "length")) == KNum(JInt(0))
  assert classify(get_(st, fixed, "0")) == KUndef
}

pub fn transfer_detaches_source_test() {
  let st = agent()
  let #(buf, st) = construct(st, "ArrayBuffer", [int(8)])
  let #(view, st) = construct(st, "Uint8Array", [buf])
  let st = set(st, view, "0", int(3))
  let #(moved, st) = invoke(st, buf, "transfer", [])
  assert classify(get_(st, buf, "detached")) == KBool(True)
  assert classify(get_(st, buf, "byteLength")) == KNum(JInt(0))
  assert classify(get_(st, moved, "byteLength")) == KNum(JInt(8))
  assert classify(get_(st, view, "length")) == KNum(JInt(0))
  assert classify(get_(st, view, "0")) == KUndef
  let #(moved_view, st) = construct(st, "Uint8Array", [moved])
  assert classify(get_(st, moved_view, "0")) == KNum(JInt(3))
  // ValidateTypedArray on a detached view is a TypeError.
  assert throws(st, view, "fill", [int(1)]) == "TypeError"
}

pub fn data_view_int16_endianness_and_float16_test() {
  let st = agent()
  let #(buf, st) = construct(st, "ArrayBuffer", [int(8)])
  let #(dv, st) = construct(st, "DataView", [buf])
  let #(_, st) = invoke(st, dv, "setInt16", [int(0), int(-2), mk_bool(True)])
  let #(le, st) = invoke(st, dv, "getInt16", [int(0), mk_bool(True)])
  let #(be, st) = invoke(st, dv, "getInt16", [int(0), mk_bool(False)])
  assert classify(le) == KNum(JInt(-2))
  // Bytes FE FF read big-endian: 0xFEFF as signed = -257.
  assert classify(be) == KNum(JInt(-257))
  let #(_, st) = invoke(st, dv, "setFloat16", [int(2), mk_number(JFloat(1.5))])
  let #(h, st) = invoke(st, dv, "getFloat16", [int(2)])
  assert classify(h) == KNum(JFloat(1.5))
  // 65520 rounds up past the largest finite half → +Infinity.
  let #(_, st) = invoke(st, dv, "setFloat16", [int(4), int(65_520)])
  let #(inf, st) = invoke(st, dv, "getFloat16", [int(4)])
  assert classify(inf) == KNum(types.JPosInf)
  assert throws(st, dv, "getFloat64", [int(4)]) == "RangeError"
}

pub fn bigint64_array_test() {
  let st = agent()
  let #(ta, st) = construct(st, "BigInt64Array", [int(2)])
  let st = set(st, ta, "0", mk_bigint(-5))
  // 2^63 wraps to the most negative int64.
  let st = set(st, ta, "1", mk_bigint(9_223_372_036_854_775_808))
  assert classify(get_(st, ta, "0")) == KBig(-5)
  assert classify(get_(st, ta, "1")) == KBig(-9_223_372_036_854_775_808)
  // A Number into a BigInt view is a TypeError from ToBigInt.
  let #(c, st) =
    t_apply_protected(st, fn(st) { #(mk_undefined(), set(st, ta, "0", int(1))) })
  let assert ThrowCompletion(err) = c
  assert error_name(st, err) == "TypeError"
}

pub fn keys_in_and_descriptor_test() {
  let st = agent()
  let #(src, st) = array(st, ints([3, 1, 2]))
  let #(ta, st) = construct(st, "Int8Array", [src])
  let #(keys, st) = static(st, "Object", "keys", [ta])
  assert joined(st, keys) == "0,1,2"
  let #(has1, st) = rt_obj.t_has_prop(st, ta, StringKey(canonical_key("1")))
  let #(has3, st) = rt_obj.t_has_prop(st, ta, StringKey(canonical_key("3")))
  assert has1
  assert !has3
  let #(desc, st) =
    static(st, "Object", "getOwnPropertyDescriptor", [ta, mk_string("0")])
  assert classify(get_(st, desc, "value")) == KNum(JInt(3))
  assert classify(get_(st, desc, "writable")) == KBool(True)
  assert classify(get_(st, desc, "enumerable")) == KBool(True)
  assert classify(get_(st, desc, "configurable")) == KBool(True)
  let #(none, st) =
    static(st, "Object", "getOwnPropertyDescriptor", [ta, mk_string("7")])
  assert classify(none) == KUndef
  // Live elements cannot be deleted; absent ones "delete" vacuously.
  let assert KHandle(h) = classify(ta)
  let #(d0, st) = rt_obj.t_delete_prop(st, h, StringKey(canonical_key("0")))
  let #(d9, _) = rt_obj.t_delete_prop(st, h, StringKey(canonical_key("9")))
  assert !d0
  assert d9
}

pub fn atomics_on_shared_int32_test() {
  let st = agent()
  let #(sab, st) = construct(st, "SharedArrayBuffer", [int(8)])
  let #(ta, st) = construct(st, "Int32Array", [sab])
  let #(old, st) = static(st, "Atomics", "add", [ta, int(0), int(5)])
  assert classify(old) == KNum(JInt(0))
  let #(v, st) = static(st, "Atomics", "load", [ta, int(0)])
  assert classify(v) == KNum(JInt(5))
  let #(w, st) =
    static(st, "Atomics", "compareExchange", [ta, int(0), int(5), int(9)])
  assert classify(w) == KNum(JInt(5))
  let #(miss, st) =
    static(st, "Atomics", "compareExchange", [ta, int(0), int(5), int(1)])
  assert classify(miss) == KNum(JInt(9))
  assert classify(get_(st, ta, "0")) == KNum(JInt(9))
  let #(woken, st) = static(st, "Atomics", "notify", [ta, int(0), int(1)])
  assert classify(woken) == KNum(JInt(0))
  // This agent cannot block: wait validates, then throws TypeError.
  let atomics = global(st, "Atomics")
  assert throws(st, atomics, "wait", [ta, int(0), int(0), int(0)])
    == "TypeError"
  // An out-of-range index is a RangeError.
  assert throws(st, atomics, "load", [ta, int(2)]) == "RangeError"
}

pub fn set_subarray_slice_sort_at_test() {
  let st = agent()
  let #(src, st) = array(st, ints([5, 1, 4]))
  let #(x, st) = construct(st, "Float32Array", [src])
  let #(nine, st) = array(st, ints([9]))
  let #(_, st) = invoke(st, x, "set", [nine, int(2)])
  assert joined(st, x) == "5,1,9"
  let #(sub, st) = invoke(st, x, "subarray", [int(1)])
  assert joined(st, sub) == "1,9"
  // subarray shares the buffer.
  assert get_(st, sub, "buffer") == get_(st, x, "buffer")
  let #(sl, st) = invoke(st, x, "slice", [int(0), int(2)])
  assert joined(st, sl) == "5,1"
  assert get_(st, sl, "buffer") != get_(st, x, "buffer")
  let #(sorted, st) = invoke(st, x, "toSorted", [])
  assert joined(st, sorted) == "1,5,9"
  assert joined(st, x) == "5,1,9"
  let #(same, st) = invoke(st, x, "sort", [])
  assert same == x
  assert joined(st, x) == "1,5,9"
  let #(last, st) = invoke(st, x, "at", [int(-1)])
  assert classify(last) == KNum(JFloat(9.0))
  let #(none, _) = invoke(st, x, "at", [int(3)])
  assert classify(none) == KUndef
}

pub fn wide_integer_float_store_rounds_like_arithmetic_test() {
  let st = agent()
  let #(f, st) = construct(st, "Float64Array", [int(2)])
  // 2^64 + 2049 has no double; the store must round it the way the rest of
  // the runtime rounds a wide integer to a Number.
  let wide = 18_446_744_073_709_553_665
  let st = set(st, f, "0", int(wide))
  assert classify(get_(st, f, "0")) == KNum(rt_val.num_from_int(wide))
  let st = set(st, f, "1", int(9_007_199_254_740_993))
  assert classify(get_(st, f, "1")) == KNum(JFloat(9_007_199_254_740_992.0))
}

/// §23.2.5.1: a primitive first argument is `? ToIndex`ed BEFORE
/// AllocateTypedArray reads `newTarget.prototype`; an Object first argument
/// allocates (and so reads the prototype) first.
pub fn constructor_reads_new_target_prototype_in_spec_order_test() {
  let st = agent()
  let #(nt_h, st) = rt_obj.t_new_object(st, Some(st.realm.object.prototype))
  let #(_, st) =
    rt_obj.t_define_own_accessor(
      st,
      nt_h,
      StringKey(canonical_key("prototype")),
      Some(mk_object(st.realm.throw_type_error)),
      option.None,
      False,
      True,
    )
  let nt = mk_object(nt_h)
  let ctor = global(st, "Int8Array")
  let #(c, st) =
    t_apply_protected(st, fn(st) {
      let #(h, st) = rt_call.t_construct(st, ctor, [int(-1)], nt)
      #(mk_object(h), st)
    })
  let assert ThrowCompletion(err) = c
  assert error_name(st, err) == "RangeError"
  let #(src, st) = array(st, ints([1]))
  let #(c, st) =
    t_apply_protected(st, fn(st) {
      let #(h, st) = rt_call.t_construct(st, ctor, [src], nt)
      #(mk_object(h), st)
    })
  let assert ThrowCompletion(err) = c
  assert error_name(st, err) == "TypeError"
}
