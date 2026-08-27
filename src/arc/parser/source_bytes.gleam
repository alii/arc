import gleam/bit_array
import gleam/option.{type Option, None}

pub fn ascii_at(bytes: BitArray, pos: Int) -> Option(String) {
  case bit_array.slice(bytes, pos, 1) {
    Ok(<<byte>>) if byte < 0x80 ->
      option.from_result(bit_array.to_string(<<byte>>))
    Ok(_) | Error(Nil) -> None
  }
}

pub fn slice(bytes: BitArray, start: Int, len: Int) -> Option(String) {
  case bit_array.slice(bytes, start, len) {
    Ok(chunk) -> option.from_result(bit_array.to_string(chunk))
    Error(Nil) -> None
  }
}
