import arc/engine.{Returned}
import arc/rt/inspect as rt_inspect
import arc/rt/js_string
import arc/rt/types.{type JsVal, JInt, KNum, KStr, classify}
import arc/rt/utf8
import rt_helpers

fn eval(eng: engine.Engine(host), source: String) -> JsVal {
  let assert Ok(#(Returned(value:), _)) = engine.eval(eng, source)
  value
}

fn int_of(eng: engine.Engine(host), source: String, expected: Int) -> Nil {
  assert classify(eval(eng, source)) == KNum(JInt(expected))
}

fn str_of(eng: engine.Engine(host), source: String, expected: String) -> Nil {
  assert classify(eval(eng, source)) == KStr(expected)
}

fn lone() -> JsVal {
  js_string.from_units([0xD800])
}

pub fn host_safe_replaces_lone_surrogate_test() {
  assert utf8.host_safe(js_string.text(lone())) == "\u{FFFD}"
}

pub fn escape_inspect_escapes_lone_surrogate_test() {
  assert rt_inspect.describe(rt_helpers.agent(), lone()) == "'\\ud800'"
}

pub fn format_error_is_host_safe_test() {
  assert rt_inspect.format_error(rt_helpers.agent(), lone()) == "\u{FFFD}"
}

pub fn from_char_code_units_test() {
  let eng = engine.new()
  int_of(eng, "String.fromCharCode(0xD800).length", 1)
  int_of(eng, "String.fromCharCode(0xD800).charCodeAt(0)", 0xD800)
}

pub fn astral_code_unit_indexing_test() {
  let eng = engine.new()
  int_of(eng, "'\\u{10000}'.length", 2)
  int_of(eng, "'\\u{10000}'.charCodeAt(0)", 0xD800)
  int_of(eng, "'\\u{10000}'.charCodeAt(1)", 0xDC00)
  int_of(eng, "'\\u{10000}'.charAt(0).charCodeAt(0)", 0xD800)
  int_of(eng, "'a\\u{10000}b'.slice(1, 2).charCodeAt(0)", 0xD800)
  int_of(eng, "'a\\u{10000}b'.indexOf(String.fromCharCode(0xDC00))", 2)
}

pub fn normalize_keeps_lone_surrogate_test() {
  let eng = engine.new()
  int_of(
    eng,
    "String.fromCharCode(0xD800).normalize('NFC').charCodeAt(0)",
    0xD800,
  )
  int_of(eng, "'a\\u0301'.normalize('NFC').length", 1)
}

pub fn encode_uri_rejects_lone_surrogate_test() {
  let eng = engine.new()
  str_of(
    eng,
    "try { encodeURI(String.fromCharCode(0xD800)); 'no' } catch (e) { e.name }",
    "URIError",
  )
}

pub fn escape_roundtrip_lone_surrogate_test() {
  let eng = engine.new()
  str_of(eng, "escape(String.fromCharCode(0xD800))", "%uD800")
  int_of(eng, "unescape('%uD800').charCodeAt(0)", 0xD800)
}

pub fn to_well_formed_test() {
  let eng = engine.new()
  int_of(
    eng,
    "String.fromCharCode(0xD800).toWellFormed().charCodeAt(0)",
    0xFFFD,
  )
}

pub fn segmenter_lone_surrogate_test() {
  let eng = engine.new()
  int_of(
    eng,
    "new Intl.Segmenter().segment(String.fromCharCode(0xD800)).containing(0).segment.charCodeAt(0)",
    0xD800,
  )
}

pub fn property_key_lone_surrogate_test() {
  let eng = engine.new()
  int_of(
    eng,
    "var o = {}; o[String.fromCharCode(0xD800)] = 1; o[String.fromCharCode(0xD800)]",
    1,
  )
}
