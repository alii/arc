// use these, never gleam/string graphemes, on js strings
// TODO(Deviation): indexes by codepoint, js wants utf-16 code units

import gleam/option.{type Option}

@external(erlang, "arc_string_ffi", "string_char_at")
pub fn char_at(s: String, idx: Int) -> Option(String)

@external(erlang, "arc_string_ffi", "string_codepoint_length")
pub fn length(s: String) -> Int

@external(erlang, "arc_string_ffi", "string_cp_slice")
pub fn slice(s: String, start: Int, len: Int) -> String

@external(erlang, "arc_string_ffi", "string_cp_drop")
pub fn drop_start(s: String, n: Int) -> String

@external(erlang, "arc_string_ffi", "string_cp_explode")
pub fn explode(s: String) -> List(String)

@external(erlang, "arc_string_ffi", "string_split")
pub fn split(s: String, sep: String, limit: Int) -> List(String)

@external(erlang, "arc_string_ffi", "string_repeat")
pub fn repeat(s: String, n: Int) -> String

@external(erlang, "arc_string_ffi", "string_codepoint_at")
pub fn codepoint_at(s: String, pos: Int) -> Option(Int)

@external(erlang, "arc_string_ffi", "string_char_at_offset")
pub fn char_at_offset(s: String, off: Int) -> Option(#(String, Int))

@external(erlang, "arc_string_ffi", "string_index_of")
pub fn index_of(haystack: String, needle: String, from: Int) -> Option(Int)

@external(erlang, "arc_string_ffi", "string_last_index_of")
pub fn last_index_of(haystack: String, needle: String, from: Int) -> Option(Int)

@external(erlang, "arc_string_ffi", "replacement_codepoint")
pub fn replacement_codepoint() -> UtfCodepoint

@external(erlang, "arc_string_ffi", "string_ascii_upper")
pub fn ascii_upper(s: String) -> Option(String)

@external(erlang, "arc_string_ffi", "string_ascii_lower")
pub fn ascii_lower(s: String) -> Option(String)
