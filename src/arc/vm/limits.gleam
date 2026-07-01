//// Engine-wide resource limits and bounded primitives.
////
//// Builtins MUST NOT call gleam/string.repeat or pad_* directly — use the
//// bounded wrappers here. They estimate output size upfront and return
//// Error(Nil) if it would exceed max_string_bytes, so pathological inputs
//// (`"x".repeat(2**30)`) fail fast instead of OOMing the BEAM process.

import gleam/string

/// Practical cap on iteration for methods that must materialize O(length)
/// data (join, toLocaleString, keys/values/entries, fill, toReversed, sort).
/// Matches the FFI's MAX_DENSE_ELEMENTS. Beyond this, a sparse `Array(2**31)`
/// would allocate billions of cons cells and OOM the BEAM process before
/// max_heap_size can catch it — the GC check runs after allocation, by which
/// point the heap has already overshot. V8 throws "Invalid string length"
/// for the same reason on `Array(2**31).join()`.
pub const max_iteration = 10_000_000

/// Largest index the dense (`:array`-backed) element representation will
/// hold. Mirrors the Erlang FFI's MAX_DENSE_ELEMENTS cap in arc_vm_ffi.erl.
/// `elements.set` enforces this: an index at or past it promotes the array
/// to the sparse dict representation instead of ever reaching the FFI, so
/// the FFI never has to (and does not) silently drop an out-of-range write.
pub const max_dense_index = 10_000_000

/// 2^53 - 1: Number.MAX_SAFE_INTEGER. Spec cap on array-like `.length`.
pub const max_safe_integer = 9_007_199_254_740_991

/// Max string size in bytes before "Invalid string length" RangeError.
/// V8 uses ~2^28-2^29 chars (512MB-1GB). We use 256MB — generous for tests.
pub const max_string_bytes = 268_435_456

/// Max VM call stack depth before "Maximum call stack size exceeded".
pub const max_call_depth = 10_000

/// Bounded string.repeat. Returns Error(Nil) if `byte_size(s) * count`
/// would exceed max_string_bytes.
pub fn repeat(s: String, count: Int) -> Result(String, Nil) {
  case string.byte_size(s) * count > max_string_bytes {
    True -> Error(Nil)
    False -> Ok(string.repeat(s, count))
  }
}

/// Bounded pad. Returns Error(Nil) if target_len (in bytes, approximating
/// chars for ASCII fillers) would exceed max_string_bytes.
pub fn pad_start(s: String, to: Int, with: String) -> Result(String, Nil) {
  case to > max_string_bytes {
    True -> Error(Nil)
    False -> Ok(string.pad_start(s, to, with))
  }
}

pub fn pad_end(s: String, to: Int, with: String) -> Result(String, Nil) {
  case to > max_string_bytes {
    True -> Error(Nil)
    False -> Ok(string.pad_end(s, to, with))
  }
}

/// Bounded join. Returns Error(Nil) if the sum of part sizes + separator
/// overhead would exceed max_string_bytes. O(n) pre-scan before the join.
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
