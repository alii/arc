import gleam/bit_array
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string

// §6.1.7 max array index is 2^32 - 2
pub const max_array_index = 4_294_967_294

pub const max_array_length = 4_294_967_295

/// `id` is a number worked out from the text, the same for the same text
/// everywhere. Build these with `named` and `private`, never by hand. Two
/// texts can share an id, so equal ids alone do not mean equal keys
pub type PropertyKey {
  Index(n: Int)
  Named(id: Int, name: String)
  Private(id: Int, text: String)
}

/// what a map of properties files the key under: its id, or for an index a
/// negative number, which no id is
pub fn id(key: PropertyKey) -> Int {
  case key {
    Named(id:, ..) | Private(id:, ..) -> id
    Index(n) -> -1 - n
  }
}

pub fn named(name: String) -> PropertyKey {
  Named(hash(name) * 2, name)
}

// odd, so that a #name and a name never share an id
pub fn private(text: String) -> PropertyKey {
  Private(hash(text) * 2 + 1, text)
}

@external(erlang, "arc_bytecode_key_ffi", "hash")
fn hash(text: String) -> Int

// §7.1.21 canonical numeric index string plus range check
pub fn canonical(s: String) -> PropertyKey {
  // digit guard: int.parse raises and catches on non-numeric keys
  case bit_array.from_string(s) {
    <<c, _:bytes>> if c >= 48 && c <= 57 ->
      case int.parse(s) {
        Ok(n) if n >= 0 && n <= max_array_index ->
          case int.to_string(n) == s {
            True -> Index(n)
            False -> named(s)
          }
        _ -> named(s)
      }
    _ -> named(s)
  }
}

pub fn index(n: Int) -> PropertyKey {
  case n >= 0 && n <= max_array_index {
    True -> Index(n)
    False -> named(int.to_string(n))
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

// for humans; use to_text when the string is data
pub fn display_text(key: PropertyKey) -> String {
  case key {
    Index(n) -> int.to_string(n)
    Named(name:, ..) -> name
    Private(text:, ..) -> private_display_name(text)
  }
}

pub fn to_text(key: PropertyKey) -> String {
  case key {
    Index(n) -> int.to_string(n)
    Named(name:, ..) -> name
    Private(text:, ..) -> text
  }
}

const uid_separator = "\u{0}"

pub fn private_text(name: String, uid: Int) -> String {
  name <> uid_separator <> int.to_string(uid)
}

pub fn private_display_name(key_text: String) -> String {
  case string.split_once(key_text, uid_separator) {
    Ok(#(name, _uid)) -> name
    Error(Nil) -> key_text
  }
}
