// codepoint ops on gleam text, never gleam/string graphemes

// TODO(Deviation): indexes by codepoint, js wants utf-16 code units

import gleam/option.{type Option}

@external(erlang, "arc_rt_utf8_ffi", "char_at")
pub fn char_at(s: String, idx: Int) -> Option(String)

@external(erlang, "arc_rt_utf8_ffi", "length")
pub fn length(s: String) -> Int

@external(erlang, "arc_rt_utf8_ffi", "slice")
pub fn slice(s: String, start: Int, len: Int) -> String

@external(erlang, "arc_rt_utf8_ffi", "drop_start")
pub fn drop_start(s: String, n: Int) -> String

@external(erlang, "arc_rt_utf8_ffi", "explode")
pub fn explode(s: String) -> List(String)

@external(erlang, "arc_rt_utf8_ffi", "split")
pub fn split(s: String, sep: String, limit: Int) -> List(String)

@external(erlang, "arc_rt_utf8_ffi", "replace_literal")
pub fn replace_literal(
  s: String,
  search: String,
  replacement: String,
  all all: Bool,
) -> String

@external(erlang, "arc_rt_utf8_ffi", "repeat")
pub fn repeat(s: String, n: Int) -> String

@external(erlang, "arc_rt_utf8_ffi", "char_at_offset")
pub fn char_at_offset(s: String, off: Int) -> Option(#(String, Int))

@external(erlang, "arc_rt_utf8_ffi", "index_of")
pub fn index_of(haystack: String, needle: String, from: Int) -> Option(Int)

@external(erlang, "arc_rt_utf8_ffi", "has_byte")
pub fn has_byte(s: String, byte: Int) -> Bool

@external(erlang, "arc_rt_utf8_ffi", "contains")
pub fn contains(haystack: String, needle: String) -> Bool

// from the very end, no position clamp needed
@external(erlang, "arc_rt_utf8_ffi", "last_index_of_all")
pub fn last_index_of_all(haystack: String, needle: String) -> Option(Int)

@external(erlang, "arc_rt_utf8_ffi", "last_index_of")
pub fn last_index_of(haystack: String, needle: String, from: Int) -> Option(Int)

@external(erlang, "arc_rt_utf8_ffi", "replacement_codepoint")
pub fn replacement_codepoint() -> UtfCodepoint

@external(erlang, "arc_rt_utf8_ffi", "ascii_upper")
pub fn ascii_upper(s: String) -> Option(String)

@external(erlang, "arc_rt_utf8_ffi", "ascii_lower")
pub fn ascii_lower(s: String) -> Option(String)

// js whitespace and line terminators, not just ascii
@external(erlang, "arc_rt_utf8_ffi", "trim_js_ws")
pub fn trim_js_ws(s: String) -> String

@external(erlang, "arc_rt_utf8_ffi", "trim_leading_js_ws")
pub fn trim_leading_js_ws(s: String) -> String

@external(erlang, "arc_rt_utf8_ffi", "trim_trailing_js_ws")
pub fn trim_trailing_js_ws(s: String) -> String
