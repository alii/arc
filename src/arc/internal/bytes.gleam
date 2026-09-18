import gleam/option.{type Option, None, Some}

pub fn byte_at(bytes: BitArray, pos: Int) -> Int {
  case bytes {
    <<_:bytes-size(pos), b, _:bytes>> -> b
    _ -> -1
  }
}

pub fn ascii_at(bytes: BitArray, pos: Int) -> Option(String) {
  case bytes {
    <<_:bytes-size(pos), b, _:bytes>> if b < 0x80 ->
      Some(unsafe_slice(bytes, pos, 1))
    _ -> None
  }
}

// bytes is a BitArray or a String; offsets are clamped char boundaries
@external(erlang, "arc_bytes_ffi", "unsafe_slice")
pub fn unsafe_slice(bytes: bytes, start: Int, len: Int) -> String

@external(erlang, "arc_bytes_ffi", "drop_start")
pub fn drop_start(bytes: bytes, start: Int) -> bytes

// may return past the end, loops rely on it
@external(erlang, "arc_bytes_ffi", "next_char_boundary")
pub fn next_char_boundary(bytes: bytes, pos: Int) -> Int
