//// Engine-wide resource limits and bounded primitives.
////
//// Builtins MUST NOT call gleam/string.repeat or pad_* directly — use the
//// bounded wrappers here. They estimate output size upfront and return
//// Error(Nil) if it would exceed max_string_bytes, so pathological inputs
//// (`"x".repeat(2**30)`) fail fast instead of OOMing the BEAM process.

import arc/rt/js_string
import gleam/bool
import gleam/result
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
/// hold. This constant is the ONLY copy of the dense/sparse policy — the FFI
/// no longer duplicates it. `elements.set` enforces it: an index at or past it
/// promotes the array to the sparse dict representation instead of ever
/// reaching the FFI, so the FFI never has to (and does not) silently drop an
/// out-of-range write.
pub const max_dense_index = 10_000_000

/// 2^53 - 1: Number.MAX_SAFE_INTEGER. Spec cap on array-like `.length`.
pub const max_safe_integer = 9_007_199_254_740_991

/// Max string size in bytes before "Invalid string length" RangeError.
/// V8 uses ~2^28-2^29 chars (512MB-1GB). We use 256MB — generous for tests.
pub const max_string_bytes = 268_435_456

/// Max VM call stack depth before "Maximum call stack size exceeded".
pub const max_call_depth = 10_000

/// Max prototype-chain hops a trap-aware walk will take before giving up. A
/// `getPrototypeOf` trap that returns a fresh proxy each call would otherwise
/// spin these walks forever — they never re-enter the JS call stack, so
/// `max_call_depth` never sees them. Bounds `enumerate_chain` (for-in — stops
/// silently; §14.7.5.10 note permits an implementation-defined cap) and the
/// OrdinaryHasInstance / isPrototypeOf walks (which throw RangeError at the
/// bound, matching V8's stack-limit check in `HasInPrototypeChain`).
pub const max_prototype_depth = 1000

/// Bounded string repeat. Returns Error(Nil) if `byte_size(s) * count`
/// would exceed max_string_bytes.
pub fn repeat(s: String, count: Int) -> Result(String, Nil) {
  case string.byte_size(s) * count > max_string_bytes {
    True -> Error(Nil)
    False -> Ok(js_string.repeat(s, count))
  }
}

/// Bounded §22.1.3.17.1 StringPad at start. Returns Error(Nil) if the
/// padded output would exceed max_string_bytes.
pub fn pad_start(s: String, to: Int, with: String) -> Result(String, Nil) {
  use fill <- result.map(pad_filler(s, to, with))
  fill <> s
}

pub fn pad_end(s: String, to: Int, with: String) -> Result(String, Nil) {
  use fill <- result.map(pad_filler(s, to, with))
  s <> fill
}

/// StringPad steps 3-9: the filler repeated and truncated to bring `s` up to
/// `to` JS-string units (not grapheme clusters); "" when `s` is already long
/// enough or the filler is empty (steps 4-5, never length-rejected). The
/// guard is on the BYTE size of the exact result.
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
