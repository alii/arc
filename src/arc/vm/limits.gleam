//// Engine-wide resource limits and bounded primitives.
////
//// Builtins MUST NOT call gleam/string.repeat or pad_* directly — use the
//// bounded wrappers here. They estimate output size upfront and return
//// Error(Nil) if it would exceed max_string_bytes, so pathological inputs
//// (`"x".repeat(2**30)`) fail fast instead of OOMing the BEAM process.

import gleam/bool
import gleam/int
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

/// Bounded pad. Returns Error(Nil) if the padded output would exceed
/// max_string_bytes.
pub fn pad_start(s: String, to: Int, with: String) -> Result(String, Nil) {
  bounded_pad(s, to, with, string.pad_start)
}

pub fn pad_end(s: String, to: Int, with: String) -> Result(String, Nil) {
  bounded_pad(s, to, with, string.pad_end)
}

/// Guard on the *byte* size of the padded result, not the grapheme count `to`.
/// Comparing `to` (graphemes) against max_string_bytes (bytes) lets a
/// multi-byte filler like "\u{10000}" (4 bytes/grapheme) build a string up to
/// 4x over the cap. gleam/string.pad_* emits exactly the `needed` missing
/// graphemes: floor(needed / length(with)) whole copies of the filler plus a
/// prefix slice of one more, so the padding is bounded by
/// ceil(needed / length(with)) whole copies. Multiplying `needed` by the byte
/// size of the WHOLE filler instead would over-estimate by a factor of
/// length(with) and spuriously reject spec-valid pads.
fn bounded_pad(
  s: String,
  to: Int,
  with: String,
  pad: fn(String, Int, String) -> String,
) -> Result(String, Nil) {
  // §22.1.3.16.1 StringPad step 4: an empty filler pads nothing — the result
  // is `s` no matter how large `to` is, so it must never be length-rejected.
  use <- bool.guard(with == "", Ok(s))
  let needed = int.max(0, to - string.length(s))
  let with_len = string.length(with)
  // ceil(needed / with_len) copies; 0 when no padding is needed, so a string
  // already at the cap is never rejected when `to` doesn't grow it.
  let pad_bytes = { needed + with_len - 1 } / with_len * string.byte_size(with)
  let estimated_bytes = string.byte_size(s) + pad_bytes
  case estimated_bytes > max_string_bytes {
    True -> Error(Nil)
    False -> Ok(pad(s, to, with))
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
