// use these, never gleam/string graphemes, on js strings
// TODO(Deviation): indexes by codepoint, js wants utf-16 code units

import gleam/option.{type Option}

@external(erlang, "arc_string_ffi", "string_char_at")
pub fn char_at(s: String, idx: Int) -> Option(String)

@external(erlang, "arc_string_ffi", "string_codepoint_length")
pub fn length(s: String) -> Int

@external(erlang, "arc_string_ffi", "string_cp_slice")
pub fn slice(s: String, start: Int, len: Int) -> String

// cp_len must be length(s), lets ascii strings skip the rescan
@external(erlang, "arc_string_ffi", "slice_known")
pub fn slice_known(s: String, cp_len: Int, start: Int, len: Int) -> String

@external(erlang, "arc_string_ffi", "drop_known")
pub fn drop_known(s: String, cp_len: Int, n: Int) -> String

@external(erlang, "arc_string_ffi", "index_of_known")
pub fn index_of_known(
  haystack: String,
  cp_len: Int,
  needle: String,
  from: Int,
) -> Option(Int)

@external(erlang, "arc_string_ffi", "string_cp_drop")
pub fn drop_start(s: String, n: Int) -> String

@external(erlang, "arc_string_ffi", "string_cp_explode")
pub fn explode(s: String) -> List(String)

@external(erlang, "arc_string_ffi", "string_split")
pub fn split(s: String, sep: String, limit: Int) -> List(String)

@external(erlang, "arc_string_ffi", "string_replace_literal")
pub fn replace_literal(
  s: String,
  search: String,
  replacement: String,
  all: Bool,
) -> String

@external(erlang, "arc_string_ffi", "string_repeat")
pub fn repeat(s: String, n: Int) -> String

@external(erlang, "arc_string_ffi", "string_codepoint_at")
pub fn codepoint_at(s: String, pos: Int) -> Option(Int)

@external(erlang, "arc_string_ffi", "string_char_at_offset")
pub fn char_at_offset(s: String, off: Int) -> Option(#(String, Int))

@external(erlang, "arc_string_ffi", "string_index_of")
pub fn index_of(haystack: String, needle: String, from: Int) -> Option(Int)

@external(erlang, "arc_string_ffi", "has_byte")
pub fn has_byte(s: String, byte: Int) -> Bool

@external(erlang, "arc_string_ffi", "string_contains")
pub fn contains(haystack: String, needle: String) -> Bool

// from the very end, no position clamp needed
@external(erlang, "arc_string_ffi", "string_last_index_of_all")
pub fn last_index_of_all(haystack: String, needle: String) -> Option(Int)

@external(erlang, "arc_string_ffi", "string_last_index_of")
pub fn last_index_of(haystack: String, needle: String, from: Int) -> Option(Int)

@external(erlang, "arc_string_ffi", "replacement_codepoint")
pub fn replacement_codepoint() -> UtfCodepoint

@external(erlang, "arc_string_ffi", "string_ascii_upper")
pub fn ascii_upper(s: String) -> Option(String)

@external(erlang, "arc_string_ffi", "string_ascii_lower")
pub fn ascii_lower(s: String) -> Option(String)
