//// Unit tests for the Source Map v3 VLQ codec.

import arc/sourcemap/codec.{Segment}
import gleam/option.{None, Some}
import gleam/string

/// Known integer -> base64 VLQ pairs, cross-checked against
/// @jridgewell/sourcemap-codec.
pub fn encode_vlq_known_pairs_test() {
  assert codec.encode_vlq(0) == "A"
  assert codec.encode_vlq(1) == "C"
  assert codec.encode_vlq(-1) == "D"
  assert codec.encode_vlq(2) == "E"
  assert codec.encode_vlq(-2) == "F"
  assert codec.encode_vlq(15) == "e"
  assert codec.encode_vlq(16) == "gB"
  assert codec.encode_vlq(-16) == "hB"
  assert codec.encode_vlq(123) == "2H"
  assert codec.encode_vlq(1000) == "w+B"
}

pub fn generate_mappings_empty_test() {
  assert codec.generate_mappings([]) == ""
}

/// A single segment at the very start of the output maps to source 0,
/// line 0, col 0: all four deltas are zero -> "AAAA".
pub fn generate_mappings_single_segment_test() {
  let segs = [Segment(gen_line: 0, gen_col: 0, source_idx: 0, orig_line: 0, orig_col: 0)]
  assert codec.generate_mappings(segs) == "AAAA"
}

/// Two segments on the same output line. The second starts at gen_col 5
/// (delta 5 -> "K") and points one column further in the source
/// (orig_col delta 1 -> "C"), with source/line deltas 0.
pub fn generate_mappings_two_segments_one_line_test() {
  let segs = [
    Segment(gen_line: 0, gen_col: 0, source_idx: 0, orig_line: 0, orig_col: 0),
    Segment(gen_line: 0, gen_col: 5, source_idx: 0, orig_line: 0, orig_col: 1),
  ]
  // First: AAAA. Second: gen_col delta 5 -> "K", source 0 -> "A",
  // orig_line 0 -> "A", orig_col delta 1 -> "C".
  assert codec.generate_mappings(segs) == "AAAA,KAAC"
}

/// Output lines are `;`-separated and gen_col resets per line. The second
/// line's segment is at gen_col 0 again, and walks the source forward by
/// one line.
pub fn generate_mappings_multi_line_test() {
  let segs = [
    Segment(gen_line: 0, gen_col: 0, source_idx: 0, orig_line: 0, orig_col: 0),
    Segment(gen_line: 1, gen_col: 0, source_idx: 0, orig_line: 1, orig_col: 0),
  ]
  // Line 0: AAAA. Line 1: gen_col reset (delta 0 -> "A"), source 0 -> "A",
  // orig_line delta 1 -> "C", orig_col delta 0 -> "A".
  assert codec.generate_mappings(segs) == "AAAA;AACA"
}

/// An output line with no segments (only inserted text) is empty, so two
/// consecutive `;` appear when a gap line has no mapping.
pub fn generate_mappings_gap_line_test() {
  let segs = [
    Segment(gen_line: 0, gen_col: 0, source_idx: 0, orig_line: 0, orig_col: 0),
    Segment(gen_line: 2, gen_col: 0, source_idx: 0, orig_line: 1, orig_col: 0),
  ]
  assert codec.generate_mappings(segs) == "AAAA;;AACA"
}

pub fn to_json_minimal_test() {
  let map =
    codec.SourceMap(
      version: 3,
      file: None,
      source_root: None,
      sources: ["a.js"],
      sources_content: [None],
      names: [],
      mappings: "AAAA",
    )
  let json = codec.to_json(map)
  assert json
    == "{\"version\":3,\"sources\":[\"a.js\"],\"sourcesContent\":[null],\"names\":[],\"mappings\":\"AAAA\"}"
}

pub fn to_json_full_test() {
  let map =
    codec.SourceMap(
      version: 3,
      file: Some("out.js"),
      source_root: Some("/root"),
      sources: ["a.js", "b.js"],
      sources_content: [Some("let x = 1\n"), None],
      names: ["x"],
      mappings: "AAAA,KAAC",
    )
  let json = codec.to_json(map)
  assert json
    == "{\"version\":3,\"file\":\"out.js\",\"sourceRoot\":\"/root\","
    <> "\"sources\":[\"a.js\",\"b.js\"],"
    <> "\"sourcesContent\":[\"let x = 1\\n\",null],"
    <> "\"names\":[\"x\"],\"mappings\":\"AAAA,KAAC\"}"
}

pub fn url_comment_external_test() {
  let map =
    codec.SourceMap(
      version: 3,
      file: None,
      source_root: None,
      sources: [],
      sources_content: [],
      names: [],
      mappings: "",
    )
  assert codec.url_comment(map, codec.External("out.js.map"))
    == "//# sourceMappingURL=out.js.map"
}

pub fn url_comment_hidden_test() {
  let map =
    codec.SourceMap(
      version: 3,
      file: None,
      source_root: None,
      sources: [],
      sources_content: [],
      names: [],
      mappings: "",
    )
  assert codec.url_comment(map, codec.Hidden) == ""
}

pub fn url_comment_inline_test() {
  let map =
    codec.SourceMap(
      version: 3,
      file: None,
      source_root: None,
      sources: ["a.js"],
      sources_content: [None],
      names: [],
      mappings: "AAAA",
    )
  let comment = codec.url_comment(map, codec.Inline)
  // base64 of the JSON document, prefixed by the data-URI scheme.
  let prefix =
    "//# sourceMappingURL=data:application/json;charset=utf-8;base64,"
  assert string.starts_with(comment, prefix)
}
