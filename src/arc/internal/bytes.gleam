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

// offsets are clamped char boundaries in valid utf-8, so no revalidation
@external(erlang, "arc_bytes_ffi", "unsafe_slice")
pub fn unsafe_slice(bytes: BitArray, start: Int, len: Int) -> String

@external(erlang, "arc_bytes_ffi", "drop_start")
pub fn drop_start(bytes: BitArray, start: Int) -> BitArray
