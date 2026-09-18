// TODO(Deviation): indexes by codepoint, js wants utf-16 code units

import arc/rt/types.{type JsVal}
import gleam/option.{type Option}

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

@external(erlang, "arc_rt_js_string_ffi", "char_at")
pub fn char_at(s: JsVal, idx: Int) -> Option(JsVal)

@external(erlang, "arc_rt_js_string_ffi", "substring")
pub fn substring(s: JsVal, start: Int, len: Int) -> JsVal

@external(erlang, "arc_rt_js_string_ffi", "index_of")
pub fn index_of(hay: JsVal, needle: JsVal, from: Int) -> Option(Int)

@external(erlang, "arc_rt_js_string_ffi", "from_texts")
pub fn from_texts(parts: List(String)) -> List(JsVal)
