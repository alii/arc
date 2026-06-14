//// Byte-offset -> (line, UTF-16 column) conversion for one source file.
////
//// Arc strings are UTF-8 binaries, but Source Map v3 columns are counted in
//// UTF-16 code units. For non-ASCII text a byte offset is therefore NOT the
//// same as a column. AST `Span`s keep raw byte offsets so this conversion is
//// done exactly once, at map-emit time, via a `PositionIndex` built per source.
////
//// Build once with `new`, then call `lookup` for each byte offset. Line and
//// column are both 0-based, matching the Source Map v3 mappings convention.

import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/list
import gleam/result
import gleam/string

/// A 0-based `(line, column)` position. `column` is a count of UTF-16 code
/// units from the start of the line, per the Source Map v3 spec.
pub type Position {
  Position(line: Int, column: Int)
}

/// Precomputed line index for one source. `line_starts` holds the byte offset
/// of the first byte of each line (ascending, always starting with 0). `source`
/// and `byte_len` are kept so columns can be measured in UTF-16 units over a
/// byte range without re-encoding the whole file.
pub opaque type PositionIndex {
  PositionIndex(source: BitArray, line_starts: List(Int), byte_len: Int)
}

/// Build a `PositionIndex` for `source`. A line begins at byte 0 and after each
/// `\n` (0x0A) byte; a `\r\n` sequence is split by its `\n`, so the carriage
/// return is the last byte of the preceding line. Scanning for the 0x0A byte is
/// UTF-8 safe because 0x0A never appears inside a multibyte sequence.
pub fn new(source: String) -> PositionIndex {
  let bytes = bit_array.from_string(source)
  let byte_len = bit_array.byte_size(bytes)
  let line_starts = list.reverse(line_starts_loop(bytes, 0, [0]))
  PositionIndex(source: bytes, line_starts:, byte_len:)
}

/// Number of lines in the indexed source (always at least 1).
pub fn line_count(index: PositionIndex) -> Int {
  list.length(index.line_starts)
}

/// Convert a byte offset into a 0-based `(line, UTF-16 column)` position.
/// Offsets are clamped into `[0, byte_len]`, so an end-exclusive span boundary
/// at end-of-file resolves to the position just past the last byte.
pub fn lookup(index: PositionIndex, byte_offset: Int) -> Position {
  let offset = int.clamp(byte_offset, min: 0, max: index.byte_len)
  let #(line, line_start) = find_line(index.line_starts, offset, 0, 0)
  let column = utf16_count(index.source, line_start, offset)
  Position(line:, column:)
}

/// Walk the bytes recording the offset that begins each line. `acc` is built in
/// reverse (newest first) and reversed by the caller.
fn line_starts_loop(bytes: BitArray, pos: Int, acc: List(Int)) -> List(Int) {
  case bytes {
    <<>> -> acc
    <<0x0A, rest:bits>> -> line_starts_loop(rest, pos + 1, [pos + 1, ..acc])
    <<_, rest:bits>> -> line_starts_loop(rest, pos + 1, acc)
    _ -> acc
  }
}

/// Find the greatest line start `<= offset`. Returns `(line_index, line_start)`
/// where `line_index` is how many starts precede or equal `offset`, minus one.
/// `line_starts` always begins with 0 and `offset >= 0`, so at least one start
/// qualifies and `idx >= 1` at return.
fn find_line(
  line_starts: List(Int),
  offset: Int,
  idx: Int,
  cur_start: Int,
) -> #(Int, Int) {
  case line_starts {
    [] -> #(idx - 1, cur_start)
    [start, ..rest] -> {
      use <- bool.guard(start > offset, #(idx - 1, cur_start))
      find_line(rest, offset, idx + 1, start)
    }
  }
}

/// Count UTF-16 code units in the byte range `[start, end)`. Falls back to the
/// byte distance only if the range is not on codepoint boundaries (which should
/// not happen for spans/line starts produced from valid source).
fn utf16_count(bytes: BitArray, start: Int, end: Int) -> Int {
  let byte_len = end - start
  let counted = {
    use slice <- result.try(bit_array.slice(bytes, start, byte_len))
    use text <- result.map(bit_array.to_string(slice))
    utf16_len(text)
  }
  result.unwrap(counted, byte_len)
}

/// Length of a string measured in UTF-16 code units: astral codepoints
/// (>= U+10000) take a surrogate pair (2 units), all others take 1.
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
