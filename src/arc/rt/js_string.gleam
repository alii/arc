// indexes by UTF-16 code unit; astral chars occupy two units

import arc/rt/types.{type JsVal}
import gleam/option.{type Option}
import gleam/order.{type Order}

// these take the js value so tagged strings keep their index

@external(erlang, "arc_rt_js_string_ffi", "concat")
pub fn concat(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_js_string_ffi", "length")
pub fn length(s: JsVal) -> Int

@external(erlang, "arc_rt_js_string_ffi", "text")
pub fn text(s: JsVal) -> String

@external(erlang, "arc_rt_js_string_ffi", "is_str")
pub fn is_str(v: JsVal) -> Bool

@external(erlang, "arc_rt_js_string_ffi", "codepoint_at")
pub fn codepoint_at(s: JsVal, idx: Int) -> Option(Int)

@external(erlang, "arc_rt_js_string_ffi", "code_unit_at")
pub fn code_unit_at(s: JsVal, idx: Int) -> Option(Int)

@external(erlang, "arc_rt_js_string_ffi", "char_at")
pub fn char_at(s: JsVal, idx: Int) -> Option(JsVal)

@external(erlang, "arc_rt_js_string_ffi", "substring")
pub fn substring(s: JsVal, start: Int, len: Int) -> JsVal

@external(erlang, "arc_rt_js_string_ffi", "index_of")
pub fn index_of(hay: JsVal, needle: JsVal, from: Int) -> Option(Int)

@external(erlang, "arc_rt_js_string_ffi", "compare")
pub fn compare(a: JsVal, b: JsVal) -> Order

@external(erlang, "arc_rt_js_string_ffi", "raw_byte_offset")
pub fn byte_offset(s: String, unit: Int) -> Int

@external(erlang, "arc_rt_js_string_ffi", "raw_unit_index")
pub fn unit_index(s: String, byte: Int) -> Int

@external(erlang, "arc_rt_js_string_ffi", "raw_is_well_formed")
pub fn is_well_formed(s: String) -> Bool

@external(erlang, "arc_rt_js_string_ffi", "raw_to_well_formed")
pub fn to_well_formed(s: String) -> String

@external(erlang, "arc_rt_js_string_ffi", "from_units")
pub fn from_units(units: List(Int)) -> JsVal

@external(erlang, "arc_rt_js_string_ffi", "from_texts")
pub fn from_texts(parts: List(String)) -> List(JsVal)
