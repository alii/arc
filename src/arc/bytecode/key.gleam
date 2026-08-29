import gleam/bit_array
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}

// §6.1.7 max array index is 2^32 - 2
pub const max_array_index = 4_294_967_294

pub const max_array_length = 4_294_967_295

// what the compiler writes into a template key slot
pub type SourceKey {
  SourceIndex(n: Int)
  SourceName(name: String)
}

// runtime key: index i is -(i+1), else name number * 4 + kind
pub type Key =
  Int

pub const kind_name = 0

pub const kind_symbol = 1

pub const kind_private = 2

pub fn index(i: Int) -> Key {
  -i - 1
}

pub fn is_index(k: Key) -> Bool {
  k < 0
}

pub fn index_of(k: Key) -> Int {
  -k - 1
}

pub fn name(number: Int) -> Key {
  int.bitwise_shift_left(number, 2)
}

pub fn name_number(k: Key) -> Int {
  int.bitwise_shift_right(k, 2)
}

pub fn kind(k: Key) -> Int {
  int.bitwise_and(k, 3)
}

pub fn is_name(k: Key) -> Bool {
  k >= 0 && int.bitwise_and(k, 3) == kind_name
}

pub fn private(uid: Int) -> Key {
  int.bitwise_shift_left(uid, 2) + kind_private
}

pub fn is_private(k: Key) -> Bool {
  k >= 0 && int.bitwise_and(k, 3) == kind_private
}

// §6.1.7 the one canonical array index rule for text
pub fn index_of_text(s: String) -> Option(Int) {
  case bit_array.from_string(s) {
    <<48>> -> Some(0)
    <<c, _:bytes>> if c >= 49 && c <= 57 ->
      case int.parse(s) {
        Ok(n) if n <= max_array_index ->
          case int.to_string(n) == s {
            True -> Some(n)
            False -> None
          }
        _ -> None
      }
    _ -> None
  }
}

pub fn source_key(s: String) -> SourceKey {
  case index_of_text(s) {
    Some(n) -> SourceIndex(n)
    None -> SourceName(s)
  }
}

pub fn is_array_index(n: Int) -> Bool {
  n >= 0 && n <= max_array_index
}

pub fn array_index_of_float(f: Float) -> Option(Int) {
  // normalizes -0.0 to 0.0
  let n = f +. 0.0
  let i = float.truncate(n)
  case int.to_float(i) == n && i >= 0 && i <= max_array_index {
    True -> Some(i)
    False -> None
  }
}

pub fn source_key_text(k: SourceKey) -> String {
  case k {
    SourceIndex(n) -> int.to_string(n)
    SourceName(s) -> s
  }
}
