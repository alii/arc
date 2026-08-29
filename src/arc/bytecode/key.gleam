import gleam/bit_array
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string

// §6.1.7 max array index is 2^32 - 2
pub const max_array_index = 4_294_967_294

pub const max_array_length = 4_294_967_295

pub type PropertyKey {
  Index(n: Int)
  Named(name: String)
  Private(text: String)
}

// §7.1.21 canonical numeric index string plus range check
pub fn canonical_key(s: String) -> PropertyKey {
  // digit guard: int.parse raises and catches on non-numeric keys
  case bit_array.from_string(s) {
    <<c, _:bytes>> if c >= 48 && c <= 57 ->
      case int.parse(s) {
        Ok(n) if n >= 0 && n <= max_array_index ->
          case int.to_string(n) == s {
            True -> Index(n)
            False -> Named(s)
          }
        _ -> Named(s)
      }
    _ -> Named(s)
  }
}

pub fn index_key(n: Int) -> PropertyKey {
  case n >= 0 && n <= max_array_index {
    True -> Index(n)
    False -> Named(int.to_string(n))
  }
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

// for humans; use key_to_text when the string is data
pub fn key_display_string(key: PropertyKey) -> String {
  case key {
    Index(n) -> int.to_string(n)
    Named(name) -> name
    Private(text) -> private_display_name(text)
  }
}

pub fn key_to_text(key: PropertyKey) -> String {
  case key {
    Index(n) -> int.to_string(n)
    Named(s) -> s
    Private(text) -> text
  }
}

const uid_separator = "\u{0}"

pub fn private_key(name: String) -> PropertyKey {
  Private(name)
}

pub fn private_key_text(name: String, uid: Int) -> String {
  name <> uid_separator <> int.to_string(uid)
}

pub fn is_private_key(key: PropertyKey) -> Bool {
  case key {
    Private(_) -> True
    Index(_) | Named(_) -> False
  }
}

pub fn private_display_name(key_text: String) -> String {
  case string.split_once(key_text, uid_separator) {
    Ok(#(name, _uid)) -> name
    Error(Nil) -> key_text
  }
}
