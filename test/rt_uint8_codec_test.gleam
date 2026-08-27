import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type JsVal, JInt, KNum, KStr, StringKey, canonical_key, classify,
  mk_bool, mk_number, mk_object, mk_string,
}
import gleam/list
import gleam/option.{Some}
import rt_helpers

fn int(i: Int) -> JsVal {
  mk_number(JInt(i))
}

fn ints(xs: List(Int)) -> List(JsVal) {
  list.map(xs, int)
}

fn global(st: Agent, name: String) -> JsVal {
  let #(v, _) = rt_helpers.global(st, name)
  v
}

fn u8(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let ctor = global(st, "Uint8Array")
  let #(h, st) = rt_call.t_construct(st, ctor, args, ctor)
  #(mk_object(h), st)
}

fn u8_of(st: Agent, bytes: List(Int)) -> #(JsVal, Agent) {
  let #(src, st) = rt_obj.t_new_array(st, ints(bytes))
  u8(st, [src])
}

fn get_(st: Agent, obj: JsVal, key: String) -> JsVal {
  let #(v, _) = rt_obj.t_get_prop(st, obj, StringKey(canonical_key(key)))
  v
}

fn set(st: Agent, obj: JsVal, key: String, v: JsVal) -> Agent {
  let #(_, st) = rt_obj.t_set_prop(st, obj, StringKey(canonical_key(key)), v)
  st
}

fn options(st: Agent, kvs: List(#(String, JsVal))) -> #(JsVal, Agent) {
  let #(h, st) = rt_obj.t_new_object(st, Some(st.realm.object.prototype))
  let o = mk_object(h)
  let st = list.fold(kvs, st, fn(st, kv) { set(st, o, kv.0, kv.1) })
  #(o, st)
}

fn attempt(
  st: Agent,
  obj: JsVal,
  name: String,
  args: List(JsVal),
) -> #(rt_call.Completion, Agent) {
  let #(f, st) = rt_helpers.get(st, obj, name)
  rt_call.t_call(st, f, obj, args)
}

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

fn throws(st: Agent, obj: JsVal, name: String, args: List(JsVal)) -> String {
  let #(c, st) = attempt(st, obj, name, args)
  let assert ThrowCompletion(err) = c
  error_name(st, err)
}

fn error_name(st: Agent, err: JsVal) -> String {
  let ctor = get_(st, err, "constructor")
  let assert KStr(n) = classify(get_(st, ctor, "name"))
  n
}

fn str(st: Agent, obj: JsVal, name: String, args: List(JsVal)) -> String {
  let #(v, _) = invoke(st, obj, name, args)
  let assert KStr(s) = classify(v)
  s
}

fn joined(st: Agent, ta: JsVal) -> String {
  str(st, ta, "join", [])
}

pub fn to_base64_default_and_url_omit_padding_test() {
  let st = rt_helpers.agent()
  let #(ta, st) = u8_of(st, [251, 255, 191, 1, 2])
  assert str(st, ta, "toBase64", []) == "+/+/AQI="
  let #(opts, st) =
    options(st, [
      #("alphabet", mk_string("base64url")),
      #("omitPadding", mk_bool(True)),
    ])
  assert str(st, ta, "toBase64", [opts]) == "-_-_AQI"
}

pub fn to_hex_test() {
  let st = rt_helpers.agent()
  let #(ta, st) = u8_of(st, [0, 15, 16, 171, 255])
  assert str(st, ta, "toHex", []) == "000f10abff"
  let #(empty, st) = u8(st, [int(0)])
  assert str(st, empty, "toHex", []) == ""
}

pub fn from_hex_round_trip_and_errors_test() {
  let st = rt_helpers.agent()
  let ctor = global(st, "Uint8Array")
  let #(ta, st) = invoke(st, ctor, "fromHex", [mk_string("DEADbeef")])
  assert joined(st, ta) == "222,173,190,239"
  assert str(st, ta, "toHex", []) == "deadbeef"
  assert classify(get_(st, ta, "length")) == KNum(JInt(4))
  assert throws(st, ctor, "fromHex", [mk_string("abc")]) == "SyntaxError"
  assert throws(st, ctor, "fromHex", [mk_string("zz")]) == "SyntaxError"
  assert throws(st, ctor, "fromHex", [int(1)]) == "TypeError"
}

pub fn from_base64_whitespace_and_strict_padding_test() {
  let st = rt_helpers.agent()
  let ctor = global(st, "Uint8Array")
  let #(ta, st) = invoke(st, ctor, "fromBase64", [mk_string(" +/8\n= \t")])
  assert joined(st, ta) == "251,255"
  let #(loose, st) = invoke(st, ctor, "fromBase64", [mk_string("QQ")])
  assert joined(st, loose) == "65"
  let #(strict, st) = options(st, [#("lastChunkHandling", mk_string("strict"))])
  assert throws(st, ctor, "fromBase64", [mk_string("QQ"), strict])
    == "SyntaxError"
  assert throws(st, ctor, "fromBase64", [mk_string("QR=="), strict])
    == "SyntaxError"
  let #(ok, st) = invoke(st, ctor, "fromBase64", [mk_string("QQ=="), strict])
  assert joined(st, ok) == "65"
  let #(url, st) = options(st, [#("alphabet", mk_string("base64url"))])
  let #(u, st) = invoke(st, ctor, "fromBase64", [mk_string("-_8"), url])
  assert joined(st, u) == "251,255"
  assert throws(st, ctor, "fromBase64", [mk_string("+/8"), url])
    == "SyntaxError"
}

pub fn set_from_base64_stop_before_partial_short_target_test() {
  let st = rt_helpers.agent()
  let #(target, st) = u8(st, [int(4)])
  let #(opts, st) =
    options(st, [#("lastChunkHandling", mk_string("stop-before-partial"))])
  let #(res, st) =
    invoke(st, target, "setFromBase64", [mk_string("AQID BAU="), opts])
  assert classify(get_(st, res, "read")) == KNum(JInt(4))
  assert classify(get_(st, res, "written")) == KNum(JInt(3))
  assert joined(st, target) == "1,2,3,0"
  let #(t2, st) = u8(st, [int(8)])
  let #(res2, st) = invoke(st, t2, "setFromBase64", [mk_string("AQIDB"), opts])
  assert classify(get_(st, res2, "read")) == KNum(JInt(4))
  assert classify(get_(st, res2, "written")) == KNum(JInt(3))
  let #(t3, st) = u8(st, [int(8)])
  let #(c, st) = attempt(st, t3, "setFromBase64", [mk_string("AQID$$$$")])
  let assert ThrowCompletion(err) = c
  assert error_name(st, err) == "SyntaxError"
  assert joined(st, t3) == "1,2,3,0,0,0,0,0"
}

pub fn set_from_hex_written_count_test() {
  let st = rt_helpers.agent()
  let #(target, st) = u8(st, [int(3)])
  let #(res, st) = invoke(st, target, "setFromHex", [mk_string("a0b1c2d3")])
  assert classify(get_(st, res, "read")) == KNum(JInt(6))
  assert classify(get_(st, res, "written")) == KNum(JInt(3))
  assert joined(st, target) == "160,177,194"
  let #(src, st) = rt_obj.t_new_array(st, ints([1]))
  let i8_ctor = global(st, "Int8Array")
  let #(i8_h, st) = rt_call.t_construct(st, i8_ctor, [src], i8_ctor)
  let set_from_hex = get_(st, target, "setFromHex")
  let #(c, _) =
    rt_call.t_call(st, set_from_hex, mk_object(i8_h), [mk_string("00")])
  let assert ThrowCompletion(_) = c
}

pub fn invalid_option_value_type_error_test() {
  let st = rt_helpers.agent()
  let #(ta, st) = u8_of(st, [1, 2, 3])
  let #(bad_alphabet, st) = options(st, [#("alphabet", mk_string("nope"))])
  assert throws(st, ta, "toBase64", [bad_alphabet]) == "TypeError"
  let #(num_alphabet, st) = options(st, [#("alphabet", int(64))])
  assert throws(st, ta, "toBase64", [num_alphabet]) == "TypeError"
  let #(bad_handling, st) =
    options(st, [#("lastChunkHandling", mk_string("sloppy"))])
  assert throws(st, ta, "setFromBase64", [mk_string("QQ=="), bad_handling])
    == "TypeError"
  assert throws(st, ta, "toBase64", [int(1)]) == "TypeError"
}
