//// The Number primitive library — §6.1.6.1 (Number type operations) plus the
//// §7.1.6/§7.1.7 integer reductions.
////
//// Nothing here knows what a JsValue is: it is pure `JsNum`/`Float`/`Int`
//// arithmetic with IEEE 754 semantics spelled out (NaN, ±Infinity and -0 are
//// constructors, not float bit patterns), so the special-value arms cannot be
//// forgotten. `ops/operators` (the `-`/`*`/`**`/`<<` dispatcher),
//// `ops/coerce` (ToInt32/ToUint32) and `builtins/math` are all callers.

import arc/vm/value.{type JsNum, Finite, Infinity, NaN, NegInfinity}
import gleam/float
import gleam/int
import gleam/option

// ============================================================================
// JsNum arithmetic — IEEE 754 semantics without BEAM floats for special values
// ============================================================================

pub fn num_add(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, NegInfinity | NegInfinity, Infinity -> NaN
    Infinity, _ | _, Infinity -> Infinity
    NegInfinity, _ | _, NegInfinity -> NegInfinity
    Finite(x), Finite(y) -> Finite(x +. y)
  }
}

pub fn num_sub(a: JsNum, b: JsNum) -> JsNum {
  num_add(a, num_negate(b))
}

pub fn num_mul(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    // ±0 * ±Infinity -> NaN. Never pattern-match a `0.0` literal: on
    // OTP >= 27 it does not match -0.0, so use a guard that catches both.
    Infinity, Finite(x) | Finite(x), Infinity if x >=. 0.0 && x <=. 0.0 -> NaN
    NegInfinity, Finite(x) | Finite(x), NegInfinity if x >=. 0.0 && x <=. 0.0 ->
      NaN
    Infinity, Finite(x) | Finite(x), Infinity ->
      case x >. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    NegInfinity, Finite(x) | Finite(x), NegInfinity ->
      case x >. 0.0 {
        True -> NegInfinity
        False -> Infinity
      }
    Infinity, Infinity | NegInfinity, NegInfinity -> Infinity
    Infinity, NegInfinity | NegInfinity, Infinity -> NegInfinity
    Finite(x), Finite(y) -> Finite(x *. y)
  }
}

pub fn num_div(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, Infinity
    | Infinity, NegInfinity
    | NegInfinity, Infinity
    | NegInfinity, NegInfinity
    -> NaN
    Infinity, Finite(x) ->
      case is_negative_float(x) {
        True -> NegInfinity
        False -> Infinity
      }
    NegInfinity, Finite(x) ->
      case is_negative_float(x) {
        True -> Infinity
        False -> NegInfinity
      }
    Finite(x), Infinity ->
      case is_negative_float(x) {
        True -> Finite(-0.0)
        False -> Finite(0.0)
      }
    Finite(x), NegInfinity ->
      case is_negative_float(x) {
        True -> Finite(0.0)
        False -> Finite(-0.0)
      }
    // x / ±0: 0/0 → NaN; nonzero/±0 → ±Infinity by XOR of the operand signs.
    // Never pattern-match a `0.0`/`-0.0` literal for the divisor — on OTP < 27
    // both literals match both zeros and `1 / -0` picks the +0 arm.
    Finite(x), Finite(y) if y >=. 0.0 && y <=. 0.0 ->
      case x >=. 0.0 && x <=. 0.0 {
        True -> NaN
        False ->
          case is_negative_float(x) != is_neg_zero(y) {
            True -> NegInfinity
            False -> Infinity
          }
      }
    Finite(x), Finite(y) -> Finite(x /. y)
  }
}

pub fn num_mod(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, _ | NegInfinity, _ -> NaN
    _, Infinity | _, NegInfinity -> a
    Finite(x), Finite(y) ->
      // +. 0.0 normalizes -0.0 so the zero-divisor check catches both zeros
      // (BEAM == on floats is =:=, which distinguishes them).
      case y +. 0.0 == 0.0 {
        True -> NaN
        // §6.1.6.1.6 Number::remainder is C fmod: result is exact and takes
        // the SIGN OF THE DIVIDEND — -1 % -1 is -0, -0 % 5 is -0. The old
        // truncate-based formula returned +0 for all zero results and lost
        // precision for |x/y| beyond Int->Float range.
        False -> Finite(fmod(x, y))
      }
  }
}

@external(erlang, "math", "fmod")
fn fmod(x: Float, y: Float) -> Float

pub fn num_negate(n: JsNum) -> JsNum {
  case n {
    Finite(x) -> Finite(float.negate(x))
    NaN -> NaN
    Infinity -> NegInfinity
    NegInfinity -> Infinity
  }
}

/// Exponentiation per ES2024 §6.1.6.1.3 Number::exponentiate. This is THE
/// implementation — the `**` operator and `Math.pow` both route here.
pub fn num_exp(base: JsNum, exp: JsNum) -> JsNum {
  case base, exp {
    // 1. If exponent is NaN, return NaN
    _, NaN -> NaN
    // 2. If exponent is +0 or -0, return 1 (even for NaN/Infinity base).
    // Guard, not a `Finite(0.0)` literal pattern: on OTP >= 27 the literal
    // only matches +0.0, so a -0.0 exponent would fall through to the
    // finite/infinite arms below and produce the wrong answer.
    _, Finite(e) if e >=. 0.0 && e <=. 0.0 -> Finite(1.0)
    // 3. If base is NaN, return NaN
    NaN, _ -> NaN
    // 4-5. ±Infinity base with ±Infinity exponent
    Infinity, Infinity -> Infinity
    Infinity, NegInfinity -> Finite(0.0)
    NegInfinity, Infinity -> Infinity
    NegInfinity, NegInfinity -> Finite(0.0)
    // abs(base) vs 1 with ±Infinity exponent
    Finite(x), Infinity -> num_exp_finite_inf(x)
    Finite(x), NegInfinity -> num_exp_finite_neginf(x)
    // 6-7. Base is +Infinity
    Infinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    // 8-11. Base is -Infinity
    NegInfinity, Finite(y) ->
      case y >. 0.0 {
        True ->
          case is_odd_integer(y) {
            True -> NegInfinity
            False -> Infinity
          }
        False ->
          case is_odd_integer(y) {
            True -> Finite(-0.0)
            False -> Finite(0.0)
          }
      }
    // 12-17. Base is ±0
    Finite(x), Finite(y) -> num_exp_finite(x, y)
  }
}

fn num_exp_finite_inf(x: Float) -> JsNum {
  let abs_x = float.absolute_value(x)
  case abs_x >. 1.0 {
    True -> Infinity
    False ->
      case abs_x <. 1.0 {
        True -> Finite(0.0)
        // abs == 1
        False -> NaN
      }
  }
}

fn num_exp_finite_neginf(x: Float) -> JsNum {
  let abs_x = float.absolute_value(x)
  case abs_x >. 1.0 {
    True -> Finite(0.0)
    False ->
      case abs_x <. 1.0 {
        True -> Infinity
        // abs == 1
        False -> NaN
      }
  }
}

fn num_exp_finite(x: Float, y: Float) -> JsNum {
  case is_neg_zero(x) {
    // Base is -0
    True ->
      case y >. 0.0 {
        True ->
          case is_odd_integer(y) {
            True -> Finite(-0.0)
            False -> Finite(0.0)
          }
        False ->
          case is_odd_integer(y) {
            True -> NegInfinity
            False -> Infinity
          }
      }
    False ->
      case x == 0.0 {
        // Base is +0
        True ->
          case y >. 0.0 {
            True -> Finite(0.0)
            False -> Infinity
          }
        // Normal finite case
        False ->
          case x <. 0.0 {
            True ->
              case value.integral_int(y) {
                // pow_total returns ±Infinity (sign from the odd/even
                // integer exponent) when |x^y| overflows a 64-bit float.
                option.Some(_) -> pow_total(x, y)
                // Non-integer exponent with negative base
                option.None -> NaN
              }
            // pow_total returns +Infinity when x^y overflows.
            False -> pow_total(x, y)
          }
      }
  }
}

/// Check if a float is an odd integer.
fn is_odd_integer(f: Float) -> Bool {
  value.integral_int(f)
  |> option.map(int.is_odd)
  |> option.unwrap(False)
}

/// `math:pow/2` made total: Erlang raises `badarith` when the true result
/// overflows a 64-bit float (or the base is negative with a non-integer
/// exponent), so the FFI catches it and returns the JsNum the spec requires
/// instead of crashing the VM.
@external(erlang, "arc_math_ffi", "pow")
pub fn pow_total(base: Float, exp: Float) -> JsNum

// ============================================================================
// Signed-zero predicates
// ============================================================================

/// True for both +0.0 and -0.0 (companion to `is_neg_zero`).
///
/// Always use this instead of a `0.0` literal in a case pattern or an
/// `x == 0.0` comparison: on OTP >= 27 both of those compile to `=:=`
/// semantics and are True/matched only for +0.0, silently missing -0.0.
/// The `>=.`/`<=.` pair is an arithmetic comparison, which treats the two
/// zeros as equal. Usable anywhere; in a guard position (where function
/// calls are not allowed) inline the same expression:
/// `Finite(n) if n >=. 0.0 && n <=. 0.0 -> ...`
pub fn is_zero(x: Float) -> Bool {
  x >=. 0.0 && x <=. 0.0
}

@external(erlang, "arc_math_ffi", "is_neg_zero")
pub fn is_neg_zero(x: Float) -> Bool

/// Check if a float is negative (including -0.0) — i.e. its IEEE sign bit is
/// set. THE test to use whenever a sign decision must treat -0 as negative:
/// a bare `x <. 0.0` (or its inverse `x >=. 0.0`) is False for -0.0 and
/// silently picks the +0 branch.
pub fn is_negative_float(x: Float) -> Bool {
  x <. 0.0 || is_neg_zero(x)
}

// ============================================================================
// Integer reductions — §7.1.6 ToInt32 / §7.1.7 ToUint32
// ============================================================================

/// §7.1.6 ToInt32 of an already-ToNumber'd value: NaN/±∞ → 0, finite →
/// truncate toward zero then wrap modulo 2^32 with sign extension. This is
/// THE ToInt32 — coerce.gleam's `try_to_int32` and every builtin route here.
pub fn num_to_int32(n: JsNum) -> Int {
  case n {
    NaN | Infinity | NegInfinity -> 0
    Finite(f) -> wrap_int32(float.truncate(f))
  }
}

/// §7.1.7 ToUint32 of an already-ToNumber'd value: NaN/±∞ → 0, finite →
/// truncate toward zero then reduce modulo 2^32 (result in [0, 2^32)).
pub fn num_to_uint32(n: JsNum) -> Int {
  case n {
    NaN | Infinity | NegInfinity -> 0
    Finite(f) -> wrap_uint32(float.truncate(f))
  }
}

/// Wrap an exact Int to a signed 32-bit value: reduce modulo 2^32, then sign
/// extend. Work on Ints stays on Ints — never round-trip an intermediate
/// (e.g. a `<<` result or Math.imul's product) through a Float first, or the
/// low 32 bits get rounded away once the magnitude passes 2^53.
pub fn wrap_int32(i: Int) -> Int {
  let wrapped = wrap_uint32(i)
  // Sign extend if needed
  case wrapped > 0x7FFFFFFF {
    True -> wrapped - 0x100000000
    False -> wrapped
  }
}

/// Wrap an exact Int to an unsigned 32-bit value (see `wrap_int32`).
/// Erlang's `band` on negatives uses infinite two's complement, so this is
/// a true modulo-2^32 reduction for any sign.
pub fn wrap_uint32(i: Int) -> Int {
  int.bitwise_and(i, 0xFFFFFFFF)
}
