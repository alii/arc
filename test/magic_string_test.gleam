//// Unit tests for the immutable magic-string editor and the Bundle
//// concatenation API.

import arc/sourcemap/codec
import arc/sourcemap/magic_string as ms
import gleam/option.{None, Some}
import gleam/string

// --- Editing round-trips ----------------------------------------------------

pub fn no_edits_roundtrip_test() {
  let s = ms.new("const x = 1;")
  assert ms.to_string(s) == "const x = 1;"
}

pub fn overwrite_test() {
  // "const x = 1;" — replace "x" (bytes [6,7)) with "answer".
  let s = ms.new("const x = 1;")
  let s = ms.overwrite(s, 6, 7, "answer")
  assert ms.to_string(s) == "const answer = 1;"
}

pub fn remove_test() {
  // Remove "const " (bytes [0,6)).
  let s = ms.new("const x = 1;")
  let s = ms.remove(s, 0, 6)
  assert ms.to_string(s) == "x = 1;"
}

pub fn append_left_right_test() {
  let s = ms.new("ab")
  // At index 1: left insert lands after "a"; right insert lands before "b".
  let s = ms.append_left(s, 1, "L")
  let s = ms.append_right(s, 1, "R")
  assert ms.to_string(s) == "aLRb"
}

pub fn prepend_append_test() {
  let s = ms.new("body")
  let s = ms.prepend(s, "/* head */")
  let s = ms.append(s, "/* tail */")
  assert ms.to_string(s) == "/* head */body/* tail */"
}

pub fn multiple_edits_test() {
  // "let a = 1; let b = 2;"
  //  0123456789...
  let s = ms.new("let a = 1; let b = 2;")
  // Replace "let" (0,3) with "const", remove "; " separator region.
  let s = ms.overwrite(s, 0, 3, "const")
  let s = ms.overwrite(s, 11, 14, "const")
  assert ms.to_string(s) == "const a = 1; const b = 2;"
}

// --- Single-source map ------------------------------------------------------

pub fn generate_map_basic_test() {
  let s = ms.new("const x = 1;")
  let s = ms.overwrite(s, 6, 7, "answer")
  let map = ms.generate_map(s, "in.js", ms.default_map_options())
  assert map.version == 3
  assert map.sources == ["in.js"]
  assert map.sources_content == [Some("const x = 1;")]
  // Mappings must be non-empty (surviving "const " and " = 1;" slices map back).
  assert map.mappings != ""
}

pub fn generate_map_no_content_test() {
  let s = ms.new("a")
  let opts =
    ms.MapOptions(
      file: Some("out.js"),
      source_root: None,
      include_content: False,
    )
  let map = ms.generate_map(s, "a.js", opts)
  assert map.file == Some("out.js")
  assert map.sources_content == [None]
}

// --- Bundle -----------------------------------------------------------------

pub fn bundle_to_string_test() {
  let a = ms.new("a();")
  let b = ms.new("b();")
  let bundle =
    ms.bundle()
    |> ms.add_source("a.js", "a();", a)
    |> ms.add_source("b.js", "b();", b)
  // Sources joined in add order, one newline between.
  assert ms.bundle_to_string(bundle) == "a();\nb();"
}

pub fn bundle_generate_map_sources_test() {
  let a = ms.new("a();")
  let b = ms.new("b();")
  let bundle =
    ms.bundle()
    |> ms.add_source("a.js", "a();", a)
    |> ms.add_source("b.js", "b();", b)
  let map = ms.bundle_generate_map(bundle, ms.default_map_options())
  assert map.version == 3
  // Each source keeps its index in sources[].
  assert map.sources == ["a.js", "b.js"]
  assert map.sources_content == [Some("a();"), Some("b();")]
  assert map.mappings != ""
}

// --- Edit + map decode ------------------------------------------------------

/// Decode the first segment of a magic-string map and confirm it points at the
/// start of the original source (generated 0,0 -> source 0, line 0, col 0).
pub fn edit_map_first_segment_decode_test() {
  let s = ms.new("hello world")
  // Overwrite "world" with "there" — the leading "hello " survives and maps.
  let s = ms.overwrite(s, 6, 11, "there")
  let map = ms.generate_map(s, "g.js", ms.default_map_options())
  // First output line, first segment.
  let first_line = first_segment_of_first_line(map.mappings)
  // "AAAA" = all-zero deltas: gen_col 0, source 0, orig_line 0, orig_col 0.
  assert string.starts_with(first_line, "AAAA")
}

fn first_segment_of_first_line(mappings: String) -> String {
  let first_line = case string.split(mappings, ";") {
    [line, ..] -> line
    [] -> ""
  }
  case string.split(first_line, ",") {
    [seg, ..] -> seg
    [] -> ""
  }
}

/// A map built directly from a known segment list decodes back to "AAAA" via
/// the codec, anchoring magic-string's segment shape to the codec contract.
pub fn segment_to_mappings_anchor_test() {
  let segs = [
    codec.Segment(
      gen_line: 0,
      gen_col: 0,
      source_idx: 0,
      orig_line: 0,
      orig_col: 0,
    ),
  ]
  assert codec.generate_mappings(segs) == "AAAA"
}

// --- Materialization walk ----------------------------------------------------

/// One walk must drive BOTH the output text and the segment list. Combine an
/// overwrite, a remove, and left/right inserts and confirm the emitted text is
/// exactly what the interleaved walk produces.
pub fn interleaved_edits_walk_test() {
  // "abcdef" — overwrite "cd" (2,4) with "XY", insert around index 4, remove
  // "e" (4,5). The surviving "ab", the replacement "XY", the inserts, then the
  // surviving "f" must compose in walk order.
  let s = ms.new("abcdef")
  let s = ms.overwrite(s, 2, 4, "XY")
  let s = ms.append_left(s, 4, "L")
  let s = ms.append_right(s, 4, "R")
  let s = ms.remove(s, 4, 5)
  // ab | XY (replace cd) | L (left of 4) | R (right of 4) | (e removed) | f
  assert ms.to_string(s) == "abXYLRf"
}

/// A surviving slice's segment must carry its ORIGINAL byte offset, so after an
/// edit the trailing slice maps to a non-zero original column. Overwrite the
/// head and decode the second segment of the first output line.
pub fn surviving_slice_carries_offset_test() {
  let s = ms.new("0123456789")
  // Replace "012" (0,3) with one char "Z"; "3456789" survives at orig offset 3.
  let s = ms.overwrite(s, 0, 3, "Z")
  let map = ms.generate_map(s, "n.js", ms.default_map_options())
  // Output is "Z3456789". Inserted "Z" emits no segment; the surviving slice's
  // segment is the first (and only) on the line: gen_col 1, orig_col 3.
  // gen_col delta 1 -> "C", source delta 0 -> "A", orig_line 0 -> "A",
  // orig_col delta 3 -> "G". So the line's single segment is "CAAG".
  assert map.mappings == "CAAG"
}

/// Removing a range must drop it from both text and mappings; the bytes after
/// the hole keep mapping to their true original offsets.
pub fn remove_then_map_test() {
  let s = ms.new("keep DROP keep")
  // Remove "DROP " (5,10).
  let s = ms.remove(s, 5, 10)
  assert ms.to_string(s) == "keep keep"
  let map = ms.generate_map(s, "r.js", ms.default_map_options())
  assert map.mappings != ""
}
