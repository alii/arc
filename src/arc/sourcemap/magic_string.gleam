//// Immutable magic-string: original source text plus an offset-addressed edit
//// log, materialized once at the end into both output text and a source-map
//// segment list.
////
//// This mirrors Rich Harris's `magic-string` (and Rollup's bundling model): a
//// real bundler keeps the ORIGINAL source and makes surgical edits
//// (remove/overwrite/insert) addressed by byte offset. The source map is a
//// byproduct of replaying that edit log — surviving original slices carry their
//// original byte offset (and so produce a mapping segment), while inserted or
//// replaced text produces no mapping.
////
//// Offsets are UTF-8 BYTE offsets into the original source, matching the byte
//// spans the parser attaches to AST nodes. UTF-16 column conversion (the
//// Source Map v3 column unit) happens later, in `position_index`, at map-emit
//// time.
////
//// References consulted before writing this: magic-string's `MagicString`,
//// `Bundle`, and `generateMap`; @jridgewell/sourcemap-codec; and the Source
//// Map v3 spec.

import arc/sourcemap/codec.{type Segment, type SourceMap}
import arc/sourcemap/position_index
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// One overwrite/remove over the half-open byte range `[start, end)`. `content`
/// is the replacement text (empty string for a removal). The replacement is
/// emitted verbatim and produces NO mapping segment (it is synthesized text).
type Edit {
  Edit(start: Int, end: Int, content: String)
}

/// Immutable string editor. Keeps the original `source` plus a sorted edit log
/// and is materialized lazily by `to_string` / `generate_map`.
///
/// - `intro` / `outro` accumulate `prepend` / `append` content wrapping the
///   whole string.
/// - `edits` maps an edit's start offset to the overwrite/remove at that point.
/// - `left_inserts` / `right_inserts` map a byte offset to text inserted just
///   left of (after the preceding content) or right of (before the following
///   content) that offset, matching magic-string's `appendLeft` / `appendRight`.
pub opaque type MagicString {
  MagicString(
    source: String,
    intro: String,
    outro: String,
    edits: Dict(Int, Edit),
    left_inserts: Dict(Int, String),
    right_inserts: Dict(Int, String),
  )
}

/// A materialized output fragment. `orig` is `Some(byte_offset)` for a surviving
/// slice of the original source (it produces a mapping) and `None` for inserted
/// or replacement text (no mapping).
type Piece {
  Piece(text: String, orig: Option(Int))
}

/// Create an editor over `source` with no edits.
pub fn new(source: String) -> MagicString {
  MagicString(
    source: source,
    intro: "",
    outro: "",
    edits: dict.new(),
    left_inserts: dict.new(),
    right_inserts: dict.new(),
  )
}

/// Replace the original byte range `[start, end)` with `with`. The replacement
/// is inserted text and produces no mapping. Overlapping overwrite/remove ranges
/// are rejected: materialization panics if two ranges overlap (matching
/// magic-string, which throws). Only the last-write-wins case of two edits at the
/// SAME start offset is tolerated (a later `overwrite` at an offset replaces the
/// earlier one in the edit map), as are zero-width inserts.
pub fn overwrite(
  ms: MagicString,
  start: Int,
  end: Int,
  with content: String,
) -> MagicString {
  MagicString(
    ..ms,
    edits: dict.insert(ms.edits, start, Edit(start:, end:, content:)),
  )
}

/// Delete the original byte range `[start, end)`.
pub fn remove(ms: MagicString, start: Int, end: Int) -> MagicString {
  overwrite(ms, start, end, "")
}

/// Insert `content` just LEFT of `index` — i.e. after the content that ends at
/// `index`, before anything inserted to the right of `index`. Repeated calls at
/// the same index append in call order.
pub fn append_left(
  ms: MagicString,
  index: Int,
  content: String,
) -> MagicString {
  let existing = dict.get(ms.left_inserts, index) |> result.unwrap("")
  MagicString(
    ..ms,
    left_inserts: dict.insert(ms.left_inserts, index, existing <> content),
  )
}

/// Insert `content` just RIGHT of `index` — i.e. before the content that starts
/// at `index`, after anything inserted to the left of `index`. Repeated calls at
/// the same index append in call order.
pub fn append_right(
  ms: MagicString,
  index: Int,
  content: String,
) -> MagicString {
  let existing = dict.get(ms.right_inserts, index) |> result.unwrap("")
  MagicString(
    ..ms,
    right_inserts: dict.insert(ms.right_inserts, index, existing <> content),
  )
}

/// Prepend `content` to the very start of the output. Repeated calls stack so
/// the most recent prepend ends up first.
pub fn prepend(ms: MagicString, content: String) -> MagicString {
  MagicString(..ms, intro: content <> ms.intro)
}

/// Append `content` to the very end of the output.
pub fn append(ms: MagicString, content: String) -> MagicString {
  MagicString(..ms, outro: ms.outro <> content)
}

/// Materialize the edited output as a string.
pub fn to_string(ms: MagicString) -> String {
  materialize(ms)
  |> list.fold("", fn(acc, piece) { acc <> piece.text })
}

// --- Materialization --------------------------------------------------------

/// Replay the edit log into an ordered list of output pieces.
///
/// Walks the original source from offset 0: at each position it emits any
/// left-then-right inserts bound to that offset, then either the replacement of
/// an edit beginning there (advancing past the edit's end) or the surviving
/// original slice up to the next boundary (carrying its original offset). `intro`
/// and `outro` wrap the result.
fn materialize(ms: MagicString) -> List(Piece) {
  let bytes = bit_array.from_string(ms.source)
  let byte_len = bit_array.byte_size(bytes)
  validate_no_overlap(ms)
  validate_inserts_outside_edits(ms)
  let boundaries = compute_boundaries(ms, byte_len)
  let body = walk(ms, bytes, byte_len, boundaries, 0, [])
  let intro_pieces = case ms.intro {
    "" -> []
    text -> [Piece(text, None)]
  }
  let outro_pieces = case ms.outro {
    "" -> []
    text -> [Piece(text, None)]
  }
  list.flatten([intro_pieces, list.reverse(body), outro_pieces])
}

/// Panic if any two overwrite/remove ranges overlap. magic-string rejects
/// overlapping edits rather than silently resolving them, and so do we: an
/// overlap is a caller bug (two edits claiming the same original bytes), not a
/// recoverable condition. Edits are sorted by start, then each range's end is
/// checked against the next range's start. Zero-width edits at the same offset
/// (pure insertion points) do not overlap and are allowed.
fn validate_no_overlap(ms: MagicString) -> Nil {
  ms.edits
  |> dict.values
  |> list.sort(fn(a, b) { int.compare(a.start, b.start) })
  |> check_overlap
}

fn check_overlap(edits: List(Edit)) -> Nil {
  case edits {
    [] | [_] -> Nil
    [a, b, ..rest] ->
      case b.start < a.end {
        True ->
          panic as {
            "magic_string: overlapping edits ["
            <> int.to_string(a.start)
            <> ", "
            <> int.to_string(a.end)
            <> ") and ["
            <> int.to_string(b.start)
            <> ", "
            <> int.to_string(b.end)
            <> ")"
          }
        False -> check_overlap([b, ..rest])
      }
  }
}

/// Panic if any append_left/append_right index falls STRICTLY inside an
/// overwrite/remove range. The walk visits an edit's start, emits the inserts
/// bound there, then jumps straight to the edit's end — so an insert at an
/// interior offset (`start < index < end`) would never be visited and would be
/// silently dropped. That is a caller bug (attaching text inside replaced
/// original bytes), so we surface it rather than drop it. Inserts exactly at an
/// edit's `start` or `end` are boundary offsets the walk does visit, so they are
/// allowed.
fn validate_inserts_outside_edits(ms: MagicString) -> Nil {
  let edits =
    ms.edits
    |> dict.values
    |> list.filter(fn(e) { e.end > e.start })
  let insert_indices =
    list.append(dict.keys(ms.left_inserts), dict.keys(ms.right_inserts))
    |> list.unique
  list.each(insert_indices, fn(index) {
    case list.find(edits, fn(e) { index > e.start && index < e.end }) {
      Ok(e) ->
        panic as {
          "magic_string: insert at offset "
          <> int.to_string(index)
          <> " falls inside overwrite/remove range ["
          <> int.to_string(e.start)
          <> ", "
          <> int.to_string(e.end)
          <> ")"
        }
      Error(Nil) -> Nil
    }
  })
}

/// Sorted, de-duplicated set of offsets at which a surviving slice must stop:
/// every edit start and every insert index, plus the end of the source.
fn compute_boundaries(ms: MagicString, byte_len: Int) -> List(Int) {
  list.flatten([
    dict.keys(ms.edits),
    dict.keys(ms.left_inserts),
    dict.keys(ms.right_inserts),
    [byte_len],
  ])
  |> list.unique
  |> list.sort(int.compare)
}

/// Tail-recursive walk building the body piece list in reverse.
fn walk(
  ms: MagicString,
  bytes: BitArray,
  byte_len: Int,
  boundaries: List(Int),
  pos: Int,
  acc: List(Piece),
) -> List(Piece) {
  let acc = push_insert(acc, ms.left_inserts, pos)
  let acc = push_insert(acc, ms.right_inserts, pos)
  use <- bool_at_end(pos >= byte_len, acc)
  case dict.get(ms.edits, pos) {
    Ok(edit) ->
      case edit.end > pos {
        True ->
          walk(ms, bytes, byte_len, boundaries, edit.end, [
            Piece(edit.content, None),
            ..acc
          ])
        // Zero-width or already-consumed edit at this offset: treat as a plain
        // boundary and emit the surviving slice.
        False -> emit_slice(ms, bytes, byte_len, boundaries, pos, acc)
      }
    Error(Nil) -> emit_slice(ms, bytes, byte_len, boundaries, pos, acc)
  }
}

/// Emit the surviving original slice from `pos` to the next boundary, carrying
/// `pos` as the mapping offset, then continue the walk.
fn emit_slice(
  ms: MagicString,
  bytes: BitArray,
  byte_len: Int,
  boundaries: List(Int),
  pos: Int,
  acc: List(Piece),
) -> List(Piece) {
  let stop = next_boundary(boundaries, pos, byte_len)
  let text = slice_bytes(bytes, pos, stop - pos)
  walk(ms, bytes, byte_len, boundaries, stop, [Piece(text, Some(pos)), ..acc])
}

/// Helper so the end-of-source check reads as an early return without nesting.
fn bool_at_end(
  at_end: Bool,
  acc: List(Piece),
  cont: fn() -> List(Piece),
) -> List(Piece) {
  case at_end {
    True -> acc
    False -> cont()
  }
}

fn push_insert(
  acc: List(Piece),
  inserts: Dict(Int, String),
  pos: Int,
) -> List(Piece) {
  case dict.get(inserts, pos) {
    Ok("") -> acc
    Ok(text) -> [Piece(text, None), ..acc]
    Error(Nil) -> acc
  }
}

fn next_boundary(boundaries: List(Int), pos: Int, byte_len: Int) -> Int {
  boundaries
  |> list.filter(fn(b) { b > pos })
  |> list.first
  |> result.unwrap(byte_len)
}

/// Slice `[start, start + len)` bytes of `bytes` back into a String. The range
/// always lands on UTF-8 boundaries (offsets come from edit/insert positions and
/// byte lengths), so a slice/decode failure can only mean an out-of-range
/// request, for which the empty string is the safe fragment.
fn slice_bytes(bytes: BitArray, start: Int, len: Int) -> String {
  case bit_array.slice(bytes, start, len) {
    Ok(slice) ->
      case bit_array.to_string(slice) {
        Ok(text) -> text
        Error(Nil) -> ""
      }
    Error(Nil) -> ""
  }
}

// --- Segment generation -----------------------------------------------------

/// Convert a piece list into mapping segments, threading the running generated
/// position. `gen_line` / `gen_col` accumulate across the whole materialized
/// output (and, for bundles, across sources). Returns the produced segments plus
/// the generated position just past the last piece.
fn pieces_to_segments(
  pieces: List(Piece),
  index: position_index.PositionIndex,
  source_idx: Int,
  gen_line: Int,
  gen_col: Int,
) -> #(List(Segment), Int, Int) {
  list.fold(pieces, #([], gen_line, gen_col), fn(acc, piece) {
    let #(segs, gline, gcol) = acc
    case piece.orig {
      None -> {
        let #(next_line, next_col) = advance(gline, gcol, piece.text)
        #(segs, next_line, next_col)
      }
      Some(offset) -> {
        let new_segs =
          slice_segments(piece.text, offset, index, source_idx, gline, gcol)
        let #(next_line, next_col) = advance(gline, gcol, piece.text)
        #(list.append(new_segs, segs), next_line, next_col)
      }
    }
  })
}

/// Produce one segment per generated line covered by a surviving slice. The
/// first line maps the slice's start (at the current generated column); each
/// subsequent line (after a newline in the slice) maps the corresponding
/// original line start at generated column 0. `hires` is false, so there is one
/// segment per line, not per token.
fn slice_segments(
  text: String,
  offset: Int,
  index: position_index.PositionIndex,
  source_idx: Int,
  gen_line: Int,
  gen_col: Int,
) -> List(Segment) {
  let lines = string.split(text, "\n")
  let #(segs, _byte_off) =
    list.index_fold(lines, #([], 0), fn(state, line, i) {
      let #(acc, byte_off) = state
      let pos = position_index.lookup(index, offset + byte_off)
      let seg_gen_col = case i {
        0 -> gen_col
        _ -> 0
      }
      let seg =
        codec.Segment(
          gen_line: gen_line + i,
          gen_col: seg_gen_col,
          source_idx: source_idx,
          orig_line: pos.line,
          orig_col: pos.column,
        )
      // +1 for the '\n' that separated this line from the next.
      let next_off = byte_off + byte_size(line) + 1
      #([seg, ..acc], next_off)
    })
  segs
}

/// Advance a generated `(line, col)` position past `text`. Columns are UTF-16
/// code units; lines increment per `\n`.
fn advance(gen_line: Int, gen_col: Int, text: String) -> #(Int, Int) {
  case string.split(text, "\n") {
    [] -> #(gen_line, gen_col)
    [single] -> #(gen_line, gen_col + utf16_len(single))
    lines -> {
      let newlines = list.length(lines) - 1
      let last = list.last(lines) |> result.unwrap("")
      #(gen_line + newlines, utf16_len(last))
    }
  }
}

fn byte_size(text: String) -> Int {
  bit_array.byte_size(bit_array.from_string(text))
}

/// Length of `text` in UTF-16 code units: astral codepoints (>= U+10000) count
/// as 2 (a surrogate pair), everything else as 1.
fn utf16_len(text: String) -> Int {
  text
  |> string.to_utf_codepoints
  |> list.fold(0, fn(acc, cp) {
    case string.utf_codepoint_to_int(cp) >= 0x10000 {
      True -> acc + 2
      False -> acc + 1
    }
  })
}

// --- Single-source map ------------------------------------------------------

/// Generate a Source Map v3 for a single edited source. `filename` is recorded
/// as the sole entry of `sources`; `include_content` controls whether the
/// original text is embedded in `sourcesContent`.
pub fn generate_map(
  ms: MagicString,
  filename: String,
  opts: MapOptions,
) -> SourceMap {
  let index = position_index.new(ms.source)
  let pieces = materialize(ms)
  let #(segments, _line, _col) = pieces_to_segments(pieces, index, 0, 0, 0)
  let sources_content = case opts.include_content {
    True -> [Some(ms.source)]
    False -> [None]
  }
  codec.SourceMap(
    version: 3,
    file: opts.file,
    source_root: opts.source_root,
    sources: [filename],
    sources_content: sources_content,
    names: [],
    mappings: codec.generate_mappings(segments),
  )
}

/// Options shared by `generate_map` and `bundle_generate_map`.
pub type MapOptions {
  MapOptions(
    file: Option(String),
    source_root: Option(String),
    include_content: Bool,
  )
}

/// Sensible defaults: no `file`, no `sourceRoot`, embed original content.
pub fn default_map_options() -> MapOptions {
  MapOptions(file: None, source_root: None, include_content: True)
}

// --- Bundle -----------------------------------------------------------------

/// One source within a bundle: its display `filename` (entered into `sources`),
/// its original `content`, and the `MagicString` describing its edits.
type BundleSource {
  BundleSource(filename: String, content: String, ms: MagicString)
}

/// A concatenation of edited sources. Materializes to the joined output text and
/// a single Source Map v3 spanning every source.
pub opaque type Bundle {
  // Stored newest-first for O(1) `add_source`; reversed to output order on read.
  Bundle(sources_rev: List(BundleSource))
}

/// An empty bundle.
pub fn bundle() -> Bundle {
  Bundle(sources_rev: [])
}

/// Append a source to the bundle. `filename` is the name recorded in the map's
/// `sources`; `content` is the original text; `ms` is its edit log.
pub fn add_source(
  b: Bundle,
  filename: String,
  content: String,
  ms: MagicString,
) -> Bundle {
  Bundle(sources_rev: [BundleSource(filename:, content:, ms:), ..b.sources_rev])
}

fn bundle_sources(b: Bundle) -> List(BundleSource) {
  list.reverse(b.sources_rev)
}

/// Materialize the bundle output: each source's edited text, joined by newlines
/// (one line break between sources).
pub fn bundle_to_string(b: Bundle) -> String {
  bundle_sources(b)
  |> list.map(fn(src) { to_string(src.ms) })
  |> string.join("\n")
}

/// Generate one Source Map v3 spanning every source in output order.
///
/// Walks each source's pieces, offsetting their segments into chunk-space: the
/// generated line/column accumulates across the whole output, and the newline
/// joining each source to the next advances the generated line by one (and
/// resets the column). Each source is assigned its index in `sources`.
pub fn bundle_generate_map(b: Bundle, opts: MapOptions) -> SourceMap {
  let srcs = bundle_sources(b)
  let #(segments, _line, _col, _idx) =
    list.fold(srcs, #([], 0, 0, 0), fn(acc, src) {
      let #(segs, gen_line, _gen_col, source_idx) = acc
      let index = position_index.new(src.content)
      let pieces = materialize(src.ms)
      let #(new_segs, end_line, _end_col) =
        pieces_to_segments(pieces, index, source_idx, gen_line, 0)
      // The separator newline between this source and the next advances one
      // generated line and resets the column.
      #(list.append(new_segs, segs), end_line + 1, 0, source_idx + 1)
    })
  let sources = list.map(srcs, fn(src) { src.filename })
  let sources_content =
    list.map(srcs, fn(src) {
      case opts.include_content {
        True -> Some(src.content)
        False -> None
      }
    })
  codec.SourceMap(
    version: 3,
    file: opts.file,
    source_root: opts.source_root,
    sources: sources,
    sources_content: sources_content,
    names: [],
    mappings: codec.generate_mappings(segments),
  )
}
