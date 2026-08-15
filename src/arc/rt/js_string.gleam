//// JS-string primitives — the counterparts of `gleam/string` that measure a
//// string in JS string units rather than grapheme clusters.
////
//// A grapheme cluster is never a JS string unit, so `gleam/string.length`,
//// `.slice`, `.drop_start` and `.to_graphemes` must not appear on any path
//// that measures or indexes a JS string value; use these instead. All five
//// walk UTF-8 codepoints in FFI (~20x faster than the grapheme-clustering
//// `gleam/string` equivalents, which go through `unicode_util:gc`).
////
//// TODO(Deviation): JS indexes by UTF-16 code unit, so astral-plane chars
//// should count as 2 indices. Codepoint indexing matches code-unit indexing
//// for all BMP chars, so this is strictly more correct than grapheme
//// indexing was. Full fix needs UTF-16 string storage.

import gleam/option.{type Option}

/// Get the character at codepoint index `idx`, or None if out of bounds.
///
/// Implements **StringGetOwnProperty** §10.4.3.5 steps 8-10.
@external(erlang, "arc_string_ffi", "string_char_at")
pub fn char_at(s: String, idx: Int) -> Option(String)

/// Codepoint count — the counterpart of `gleam/string.length`.
@external(erlang, "arc_string_ffi", "string_codepoint_length")
pub fn length(s: String) -> Int

/// Codepoint-based substring: `len` codepoints starting at codepoint `start`.
///
/// The counterpart of `gleam/string.slice`. Plain UTF-8 byte walk returning a
/// sub-binary — no per-character allocation.
@external(erlang, "arc_string_ffi", "string_cp_slice")
pub fn slice(s: String, start: Int, len: Int) -> String

/// Drop the first `n` codepoints (clamps; n <= 0 returns `s` unchanged).
/// The counterpart of `gleam/string.drop_start` — sub-binary result, alloc-free
/// walk, and it never merges a combining mark into the character before it.
@external(erlang, "arc_string_ffi", "string_cp_drop")
pub fn drop_start(s: String, n: Int) -> String

/// Split into single-codepoint strings, one per index of the JS string.
///
/// The counterpart of `gleam/string.to_graphemes`, and the only correct one:
/// `to_graphemes` folds a combining mark into the character before it, so it
/// yields FEWER entries than `length` reports and disagrees with `char_at` on
/// every index after the mark. This walks the same codepoints `length` counts,
/// so `list.length(explode(s)) == length(s)` always holds.
@external(erlang, "arc_string_ffi", "string_cp_explode")
pub fn explode(s: String) -> List(String)

/// Integer codepoint at codepoint index `pos`, or None when out of bounds.
/// Shares the cursor cache with `char_at`, so sequential scans resume from
/// the previous position instead of re-walking from byte 0.
@external(erlang, "arc_string_ffi", "string_codepoint_at")
pub fn codepoint_at(s: String, pos: Int) -> Option(Int)

/// StringIndexOf (§7.1.18): the codepoint index of the first occurrence of
/// `needle` at or after `from`, or None when there is none. Total — an empty
/// `needle` matches at `from` (clamped into `[0, length(haystack)]`), per the
/// operation's step 2.
///
/// The JS boundary (`String.prototype.indexOf` and friends) is where None
/// becomes -1; nothing else may treat -1 as an index.
@external(erlang, "arc_string_ffi", "string_index_of")
pub fn index_of(haystack: String, needle: String, from: Int) -> Option(Int)

/// The reverse of `index_of`: the codepoint index of the last occurrence of
/// `needle` starting at or before `from`, or None. Same byte-level search as
/// `index_of`, so the two always agree on which occurrences exist.
@external(erlang, "arc_string_ffi", "string_last_index_of")
pub fn last_index_of(haystack: String, needle: String, from: Int) -> Option(Int)

/// U+FFFD REPLACEMENT CHARACTER. FFI because UtfCodepoint has no public
/// constructor — on Erlang it's just an Int, so this is a constant-pool load
/// instead of a `string.utf_codepoint` call + Result unwrap + assert.
@external(erlang, "arc_string_ffi", "replacement_codepoint")
pub fn replacement_codepoint() -> UtfCodepoint
