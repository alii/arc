//// Source Map v3 model + base64 VLQ codec.
////
//// This is the byproduct layer of the magic-string edit log: a `Segment`
//// list (generated position -> original position, with inserted text
//// producing no segment) is turned into the compact `mappings` string of a
//// Source Map v3 document, which `to_json` serializes and `url_comment`
//// references.
////
//// References consulted before writing this: the Source Map v3 spec
//// (mappings grammar, UTF-16 column rule, sourcesContent), magic-string's
//// `generateMap`, and @jridgewell/sourcemap-codec for the VLQ encoding.

import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string

/// A Source Map v3 document.
///
/// `sources_content` is positional with `sources`: each entry is the full
/// original text of the source at the same index, or `None` when the content
/// is not embedded (serialized as JSON `null`).
pub type SourceMap {
  SourceMap(
    version: Int,
    file: Option(String),
    source_root: Option(String),
    sources: List(String),
    sources_content: List(Option(String)),
    names: List(String),
    mappings: String,
  )
}

/// How a generated file should reference its source map.
pub type MapMode {
  /// Emit a `sourceMappingURL` comment pointing at a separate `.map` file.
  External(url: String)
  /// Emit a `sourceMappingURL` comment with the whole map inlined as a
  /// base64 `data:` URI.
  Inline
  /// Emit no comment (the map exists but is not referenced from the output).
  Hidden
}

/// One mapping from a position in the generated output to a position in an
/// original source. Half-open columns are UTF-16 code units; lines are
/// zero-based. Inserted (synthesized) output produces no `Segment`.
pub type Segment {
  Segment(
    gen_line: Int,
    gen_col: Int,
    source_idx: Int,
    orig_line: Int,
    orig_col: Int,
  )
}

/// Base64 VLQ alphabet (Source Map v3): A-Z, a-z, 0-9, '+', '/'.
const base64_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

/// Encode a single signed integer as a base64 VLQ string.
///
/// Sign is folded into the low bit (`value << 1`, or `(-value << 1) | 1` for
/// negatives), then the magnitude is split into 5-bit groups emitted
/// low-group-first. Every group except the last carries the continuation bit
/// (32); each resulting 6-bit value indexes into `base64_alphabet`.
pub fn encode_vlq(value: Int) -> String {
  let vlq = case value < 0 {
    True -> int.bitwise_or(int.bitwise_shift_left(-value, 1), 1)
    False -> int.bitwise_shift_left(value, 1)
  }
  encode_vlq_loop(vlq, "")
}

fn encode_vlq_loop(vlq: Int, acc: String) -> String {
  let digit = int.bitwise_and(vlq, 31)
  let rest = int.bitwise_shift_right(vlq, 5)
  case rest > 0 {
    // More groups follow: set the continuation bit on this one.
    True -> encode_vlq_loop(rest, acc <> base64_char(int.bitwise_or(digit, 32)))
    False -> acc <> base64_char(digit)
  }
}

fn base64_char(n: Int) -> String {
  string.slice(base64_alphabet, n, 1)
}

/// Running delta state carried across the whole map for the source-pointing
/// fields. `gen_col` is intentionally NOT here — it resets at every output
/// line, so it is threaded separately within a line.
type DeltaState {
  DeltaState(source_idx: Int, orig_line: Int, orig_col: Int)
}

/// Build the Source Map v3 `mappings` string from a flat segment list.
///
/// Output lines are `;`-separated and segments within a line are
/// `,`-separated. Each segment is the VLQ of four deltas:
/// `[gen_col, source_idx, orig_line, orig_col]`. `gen_col` is delta'd within
/// the line (reset to 0 per line); the other three are delta'd across the
/// whole map. Inserted text contributes no segment (so a line with only
/// inserted output is empty). `hires` is false: one segment per chunk.
pub fn generate_mappings(segments: List(Segment)) -> String {
  case segments {
    [] -> ""
    _ -> {
      let sorted = list.sort(segments, by: compare_segment)
      let max_line =
        list.fold(sorted, 0, fn(acc, s) {
          case s.gen_line > acc {
            True -> s.gen_line
            False -> acc
          }
        })
      let by_line = list.group(sorted, by: fn(s) { s.gen_line })
      // int.range's `to` is exclusive, so go to max_line + 1 to cover the
      // last output line.
      let #(lines_rev, _final) =
        int.range(0, max_line + 1, #([], DeltaState(0, 0, 0)), fn(acc, line) {
          let #(lines, delta) = acc
          // `list.group` does not preserve order within a group, so re-sort
          // this line's segments by generated column before encoding.
          let segs =
            dict.get(by_line, line)
            |> result.unwrap([])
            |> list.sort(by: fn(a, b) { int.compare(a.gen_col, b.gen_col) })
          let #(line_str, next_delta) = emit_line(segs, delta)
          #([line_str, ..lines], next_delta)
        })
      lines_rev |> list.reverse |> string.join(";")
    }
  }
}

/// Encode one output line's segments, threading the cross-map delta state in
/// and out. `gen_col` resets to 0 at the start of every line.
fn emit_line(segs: List(Segment), delta: DeltaState) -> #(String, DeltaState) {
  let #(parts_rev, _gen_col, next_delta) =
    list.fold(segs, #([], 0, delta), fn(acc, seg) {
      let #(parts, prev_gen_col, d) = acc
      let field =
        string.concat([
          encode_vlq(seg.gen_col - prev_gen_col),
          encode_vlq(seg.source_idx - d.source_idx),
          encode_vlq(seg.orig_line - d.orig_line),
          encode_vlq(seg.orig_col - d.orig_col),
        ])
      let next = DeltaState(seg.source_idx, seg.orig_line, seg.orig_col)
      #([field, ..parts], seg.gen_col, next)
    })
  #(parts_rev |> list.reverse |> string.join(","), next_delta)
}

fn compare_segment(a: Segment, b: Segment) -> order.Order {
  case int.compare(a.gen_line, b.gen_line) {
    order.Eq -> int.compare(a.gen_col, b.gen_col)
    other -> other
  }
}

/// Serialize a `SourceMap` to a JSON string. `file` and `sourceRoot` are
/// omitted when `None`; `sourcesContent` entries are `null` when `None`.
pub fn to_json(map: SourceMap) -> String {
  let fields =
    [
      Some("\"version\":" <> int.to_string(map.version)),
      option.map(map.file, fn(f) { "\"file\":" <> json_string(f) }),
      option.map(map.source_root, fn(r) {
        "\"sourceRoot\":" <> json_string(r)
      }),
      Some("\"sources\":" <> json_array(list.map(map.sources, json_string))),
      Some(
        "\"sourcesContent\":"
        <> json_array(list.map(map.sources_content, json_nullable)),
      ),
      Some("\"names\":" <> json_array(list.map(map.names, json_string))),
      Some("\"mappings\":" <> json_string(map.mappings)),
    ]
    |> option.values
  "{" <> string.join(fields, ",") <> "}"
}

fn json_array(items: List(String)) -> String {
  "[" <> string.join(items, ",") <> "]"
}

fn json_nullable(value: Option(String)) -> String {
  case value {
    Some(s) -> json_string(s)
    None -> "null"
  }
}

fn json_string(value: String) -> String {
  let escaped =
    value
    |> string.replace("\\", "\\\\")
    |> string.replace("\"", "\\\"")
    |> string.replace("\n", "\\n")
    |> string.replace("\r", "\\r")
    |> string.replace("\t", "\\t")
  "\"" <> escaped <> "\""
}

/// Produce the `sourceMappingURL` comment for the given map and mode.
///
/// `External` points at a sibling `.map` URL, `Inline` embeds the whole map
/// as a base64 `data:` URI, and `Hidden` emits nothing.
pub fn url_comment(map: SourceMap, mode: MapMode) -> String {
  case mode {
    External(url:) -> "//# sourceMappingURL=" <> url
    Hidden -> ""
    Inline -> {
      let b64 =
        bit_array.base64_encode(bit_array.from_string(to_json(map)), True)
      "//# sourceMappingURL=data:application/json;charset=utf-8;base64," <> b64
    }
  }
}
