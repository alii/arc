/// Byte-offset access into a UTF-8 source binary.
///
/// The lexer, the regex validator and the parser's source re-scans all index
/// into the same source binary by BYTE offset (never by grapheme), so the two
/// primitives they need — "the ASCII character here" and "the text of this
/// byte range" — live here rather than in whichever module happened to need
/// them first. Both signal absence with `None`: there is no empty character
/// and no empty slice sentinel, so a caller can never confuse "nothing here"
/// with real text.
import gleam/bit_array
import gleam/option.{type Option, None}

/// The lone ASCII character at byte `pos`, or `None` when there isn't one —
/// `pos` is out of bounds, or the byte there belongs to a multi-byte code
/// point.
pub fn ascii_at(bytes: BitArray, pos: Int) -> Option(String) {
  case bit_array.slice(bytes, pos, 1) {
    Ok(<<byte>>) if byte < 0x80 ->
      option.from_result(bit_array.to_string(<<byte>>))
    Ok(_) | Error(Nil) -> None
  }
}

/// The text of the byte range `[start, start + len)`, or `None` when the range
/// runs off the end of `bytes` or does not begin and end on UTF-8 boundaries.
pub fn slice(bytes: BitArray, start: Int, len: Int) -> Option(String) {
  case bit_array.slice(bytes, start, len) {
    Ok(chunk) -> option.from_result(bit_array.to_string(chunk))
    Error(Nil) -> None
  }
}
