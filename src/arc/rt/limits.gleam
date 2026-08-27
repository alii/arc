import arc/rt/js_string
import gleam/bool
import gleam/result
import gleam/string

// cap for methods that materialize o(length) data
pub const max_iteration = 10_000_000

// past this index arrays go sparse
pub const max_dense_index = 10_000_000

pub const max_safe_integer = 9_007_199_254_740_991

// 256mb
pub const max_string_bytes = 268_435_456

pub const max_call_depth = 10_000

// bounds proto walks against trap loops
pub const max_prototype_depth = 1000

pub fn repeat(s: String, count: Int) -> Result(String, Nil) {
  case string.byte_size(s) * count > max_string_bytes {
    True -> Error(Nil)
    False -> Ok(js_string.repeat(s, count))
  }
}

pub fn pad_start(s: String, to: Int, with: String) -> Result(String, Nil) {
  use fill <- result.map(pad_filler(s, to, with))
  fill <> s
}

pub fn pad_end(s: String, to: Int, with: String) -> Result(String, Nil) {
  use fill <- result.map(pad_filler(s, to, with))
  s <> fill
}

fn pad_filler(s: String, to: Int, with: String) -> Result(String, Nil) {
  let needed = to - js_string.length(s)
  use <- bool.guard(needed <= 0 || with == "", Ok(""))
  let with_len = js_string.length(with)
  let copies = needed / with_len
  let tail = js_string.slice(with, 0, needed % with_len)
  let bytes =
    string.byte_size(s)
    + copies
    * string.byte_size(with)
    + string.byte_size(tail)
  case bytes > max_string_bytes {
    True -> Error(Nil)
    False -> Ok(js_string.repeat(with, copies) <> tail)
  }
}

pub fn join(parts: List(String), sep: String) -> Result(String, Nil) {
  let sep_size = string.byte_size(sep)
  case estimate_join(parts, sep_size, 0) > max_string_bytes {
    True -> Error(Nil)
    False -> Ok(string.join(parts, sep))
  }
}

fn estimate_join(parts: List(String), sep_size: Int, acc: Int) -> Int {
  case parts {
    [] -> acc
    [p] -> acc + string.byte_size(p)
    [p, ..rest] ->
      estimate_join(rest, sep_size, acc + string.byte_size(p) + sep_size)
  }
}
