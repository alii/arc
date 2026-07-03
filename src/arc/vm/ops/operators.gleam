import arc/vm/binop.{
  type ArithOp, type BitwiseOp, type CompareOp, type EqualityOp, type PureBinOp,
  AndOp, ArithDiv, ArithExp, ArithMod, ArithMul, ArithSub, BitAnd, BitOr,
  BitXor, Div, Eq, EqOp, Exp, Gt, GtCmp, GtEq, GtEqCmp, Lt, LtCmp, LtEq,
  LtEqCmp, Mod, Mul, NotEq, NotEqOp, OrOp, Shl, ShlOp, Shr, ShrOp, StrictEq,
  StrictEqOp, StrictNotEq, StrictNotEqOp, Sub, UShr, UShrOp, XorOp,
}
import arc/vm/opcode.{type UnaryOpKind, BitNot, LogicalNot, Neg, Pos, Void}
import arc/vm/value.{
  type JsNum, type JsValue, BigInt, Finite, Infinity, JsBigInt, JsBool, JsNumber,
  JsString, NaN, NegInfinity,
}
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string

// ============================================================================
// Binary and unary operator dispatch
// ============================================================================

/// A failed operator / coercion. The constructor names the error class the
/// interpreter must throw, so a producer cannot demote a spec-mandated
/// RangeError to a TypeError by forgetting a string prefix.
pub type OpError {
  OpTypeError(msg: String)
  OpRangeError(msg: String)
}

/// Execute a PURE binary operation on two already-primitive JsValues.
///
/// The `PureBinOp` argument (rather than the full 22-variant `BinOpKind`) is
/// what makes this total: `Add`, `In` and `InstanceOf` are handled by the
/// interpreter and are simply not spellable here.
///
/// This single exhaustive `case` is also where a `PureBinOp` narrows to the
/// family that evaluates it, so `exec_arith` and friends can only ever be
/// handed an operator they actually implement.
pub fn exec_binop(
  kind: PureBinOp,
  left: JsValue,
  right: JsValue,
) -> Result(JsValue, OpError) {
  case kind {
    Sub -> exec_arith(ArithSub, left, right)
    Mul -> exec_arith(ArithMul, left, right)
    Div -> exec_arith(ArithDiv, left, right)
    Mod -> exec_arith(ArithMod, left, right)
    Exp -> exec_arith(ArithExp, left, right)
    BitAnd -> exec_bitwise(AndOp, left, right)
    BitOr -> exec_bitwise(OrOp, left, right)
    BitXor -> exec_bitwise(XorOp, left, right)
    Shl -> exec_bitwise(ShlOp, left, right)
    Shr -> exec_bitwise(ShrOp, left, right)
    UShr -> exec_bitwise(UShrOp, left, right)
    Eq -> Ok(JsBool(exec_equality(EqOp, left, right)))
    NotEq -> Ok(JsBool(exec_equality(NotEqOp, left, right)))
    StrictEq -> Ok(JsBool(exec_equality(StrictEqOp, left, right)))
    StrictNotEq -> Ok(JsBool(exec_equality(StrictNotEqOp, left, right)))
    Lt -> exec_compare(LtCmp, left, right)
    LtEq -> exec_compare(LtEqCmp, left, right)
    Gt -> exec_compare(GtCmp, left, right)
    GtEq -> exec_compare(GtEqCmp, left, right)
  }
}

fn exec_arith(
  op: ArithOp,
  left: JsValue,
  right: JsValue,
) -> Result(JsValue, OpError) {
  case left, right {
    // §6.1.6.2 BigInt arithmetic.
    JsBigInt(BigInt(a)), JsBigInt(BigInt(b)) -> bigint_arith(op, a, b)
    // §13.15.4 ApplyStringOrNumericBinaryOperator step 7.a:
    // Type(lnum) ≠ Type(rnum) → TypeError.
    JsBigInt(_), _ | _, JsBigInt(_) -> Error(bigint_mix_error)
    // Fast path: both operands are already Numbers — to_number is the
    // identity, so call the JsNum op directly without combinators or
    // closure indirection.
    JsNumber(a), JsNumber(b) ->
      Ok(
        JsNumber(case op {
          ArithSub -> num_sub(a, b)
          ArithMul -> num_mul(a, b)
          ArithDiv -> num_div(a, b)
          ArithMod -> num_mod(a, b)
          ArithExp -> num_exp(a, b)
        }),
      )
    _, _ -> num_binop(left, right, arith_num_op(op))
  }
}

fn arith_num_op(op: ArithOp) -> fn(JsNum, JsNum) -> JsNum {
  case op {
    ArithSub -> num_sub
    ArithMul -> num_mul
    ArithDiv -> num_div
    ArithMod -> num_mod
    ArithExp -> num_exp
  }
}

/// Bitwise / shifts — BigInt×BigInt operates on arbitrary precision
/// (§6.1.6.2), mixed types throw, Number path converts to i32.
fn exec_bitwise(
  op: BitwiseOp,
  left: JsValue,
  right: JsValue,
) -> Result(JsValue, OpError) {
  case left, right {
    JsBigInt(BigInt(a)), JsBigInt(BigInt(b)) -> bigint_bitwise(op, a, b)
    JsBigInt(_), _ | _, JsBigInt(_) -> Error(bigint_mix_error)
    _, _ ->
      case op {
        AndOp -> bitwise_binop(left, right, int.bitwise_and)
        OrOp -> bitwise_binop(left, right, int.bitwise_or)
        XorOp -> bitwise_binop(left, right, int.bitwise_exclusive_or)
        // §6.1.6.1.9 Number::leftShift: the raw shift of an int32 by up
        // to 31 bits needs up to 62 bits on the BEAM's unbounded Ints, so
        // the result MUST be re-wrapped to int32 (`(1<<31)|0` is
        // -2147483648, not 2147483648).
        ShlOp -> {
          use a, b <- bitwise_binop(left, right)
          wrap_int32(int.bitwise_shift_left(a, int.bitwise_and(b, 31)))
        }
        ShrOp -> {
          use a, b <- bitwise_binop(left, right)
          int.bitwise_shift_right(a, int.bitwise_and(b, 31))
        }
        UShrOp -> {
          use a, b <- bitwise_binop(left, right)
          int.bitwise_shift_right(
            int.bitwise_and(a, 0xFFFFFFFF),
            int.bitwise_and(b, 31),
          )
        }
      }
  }
}

fn exec_equality(op: EqualityOp, left: JsValue, right: JsValue) -> Bool {
  case op {
    StrictEqOp -> value.strict_equal(left, right)
    StrictNotEqOp -> !value.strict_equal(left, right)
    EqOp -> loose_equal(left, right)
    NotEqOp -> !loose_equal(left, right)
  }
}

/// Relational: fast path for Number × Number avoids the per-op pred
/// closure and the to_number round-trip in compare_values.
fn exec_compare(
  op: CompareOp,
  left: JsValue,
  right: JsValue,
) -> Result(JsValue, OpError) {
  case left, right {
    JsNumber(a), JsNumber(b) ->
      Ok(
        JsBool(case op {
          LtCmp -> num_lt(a, b)
          LtEqCmp -> num_lt_eq(a, b)
          GtCmp -> num_gt(a, b)
          GtEqCmp -> num_gt_eq(a, b)
        }),
      )
    _, _ -> compare_values(left, right, compare_pred(op))
  }
}

/// The predicate a relational operator applies to an ordering. Every arm is
/// exhaustive over `CompareOrd`, so `UnorderedOrd` (a NaN operand) cannot be
/// forgotten: it is `false` for all four operators.
fn compare_pred(op: CompareOp) -> fn(CompareOrd) -> Bool {
  case op {
    LtCmp -> fn(ord) { ord == LtOrd }
    GtCmp -> fn(ord) { ord == GtOrd }
    LtEqCmp -> fn(ord) {
      case ord {
        LtOrd | EqOrd -> True
        GtOrd | UnorderedOrd -> False
      }
    }
    GtEqCmp -> fn(ord) {
      case ord {
        GtOrd | EqOrd -> True
        LtOrd | UnorderedOrd -> False
      }
    }
  }
}

/// Execute a unary operation.
pub fn exec_unaryop(
  kind: UnaryOpKind,
  operand: JsValue,
) -> Result(JsValue, OpError) {
  case kind, operand {
    // §6.1.6.2.1 BigInt::unaryMinus.
    Neg, JsBigInt(BigInt(n)) -> Ok(JsBigInt(BigInt(0 - n)))
    Neg, _ -> {
      use n <- result.map(to_number(operand))
      JsNumber(num_negate(n))
    }
    // §13.5.6 unary + applies ToNumber, which throws on BigInt — the
    // to_number call below produces that TypeError.
    Pos, _ -> {
      use n <- result.map(to_number(operand))
      JsNumber(n)
    }
    // §6.1.6.2.2 BigInt::bitwiseNOT: -n - 1.
    BitNot, JsBigInt(BigInt(n)) -> Ok(JsBigInt(BigInt(-1 - n)))
    BitNot, _ -> {
      use n <- result.map(to_number(operand))
      value.from_int(int.bitwise_not(num_to_int32(n)))
    }
    LogicalNot, _ -> Ok(JsBool(!value.is_truthy(operand)))
    Void, _ -> Ok(value.JsUndefined)
  }
}

// ============================================================================
// BigInt arithmetic — §6.1.6.2 (arbitrary precision; BEAM ints are exact)
// ============================================================================

/// §13.15.4 step 7.a / V8 message for BigInt × Number operand mixing.
const bigint_mix_error = OpTypeError(
  "Cannot mix BigInt and other types, use explicit conversions",
)

/// ToNumber for an operator operand. Both refusals ToNumber can report are
/// TypeErrors — but the class is chosen HERE, per variant, so a future
/// `ToNumberError` variant that the spec classes differently is a compile
/// error rather than a silently demoted RangeError.
fn to_number(v: JsValue) -> Result(JsNum, OpError) {
  case value.to_number(v) {
    Ok(n) -> Ok(n)
    Error(value.BigIntNotConvertible) ->
      Error(OpTypeError("Cannot convert BigInt to number"))
    Error(value.SymbolNotConvertible) ->
      Error(OpTypeError("Cannot convert Symbol to number"))
  }
}

fn bigint_arith(kind: ArithOp, a: Int, b: Int) -> Result(JsValue, OpError) {
  case kind {
    ArithSub -> Ok(JsBigInt(BigInt(a - b)))
    ArithMul -> Ok(JsBigInt(BigInt(a * b)))
    // §6.1.6.2.5 BigInt::divide — truncating division; 0n divisor throws
    // RangeError. Gleam's Int `/` is Erlang div (truncates toward zero).
    ArithDiv ->
      case b {
        0 -> Error(OpRangeError("Division by zero"))
        _ -> Ok(JsBigInt(BigInt(a / b)))
      }
    // §6.1.6.2.6 BigInt::remainder — sign of dividend; 0n divisor throws
    // RangeError. Gleam's Int `%` is Erlang rem (sign of dividend).
    ArithMod ->
      case b {
        0 -> Error(OpRangeError("Division by zero"))
        _ -> Ok(JsBigInt(BigInt(a % b)))
      }
    // §6.1.6.2.3 BigInt::exponentiate — negative exponent throws RangeError.
    ArithExp ->
      case b < 0 {
        True -> Error(OpRangeError("Exponent must be non-negative"))
        False -> Ok(JsBigInt(BigInt(bigint_pow(a, b, 1))))
      }
  }
}

/// Integer exponentiation by squaring (exponent is non-negative).
fn bigint_pow(base: Int, exp: Int, acc: Int) -> Int {
  case exp {
    0 -> acc
    _ ->
      case exp % 2 {
        1 -> bigint_pow(base * base, exp / 2, acc * base)
        _ -> bigint_pow(base * base, exp / 2, acc)
      }
  }
}

fn bigint_bitwise(kind: BitwiseOp, a: Int, b: Int) -> Result(JsValue, OpError) {
  case kind {
    // Erlang band/bor/bxor on arbitrary-precision ints follow infinite
    // two's-complement semantics — exactly §6.1.6.2.17-19.
    AndOp -> Ok(JsBigInt(BigInt(int.bitwise_and(a, b))))
    OrOp -> Ok(JsBigInt(BigInt(int.bitwise_or(a, b))))
    XorOp -> Ok(JsBigInt(BigInt(int.bitwise_exclusive_or(a, b))))
    // §6.1.6.2.9/10: shifts are arbitrary-precision; Erlang bsl/bsr accept
    // negative counts and shift the other way, matching leftShift(x, -y).
    ShlOp -> Ok(JsBigInt(BigInt(int.bitwise_shift_left(a, b))))
    ShrOp -> Ok(JsBigInt(BigInt(int.bitwise_shift_right(a, b))))
    // §6.1.6.2.11 BigInt::unsignedRightShift always throws.
    UShrOp ->
      Error(OpTypeError("BigInts have no unsigned right shift, use >> instead"))
  }
}

// ============================================================================
// Loose equality with BigInt support — §7.2.14 IsLooselyEqual
// ============================================================================

/// §7.2.14 steps 6-9, 10-11: BigInt × Number/String/Boolean comparisons.
/// Everything else delegates to value.abstract_equal.
fn loose_equal(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    // Steps 10/11: Boolean operand → ToNumber, then redo.
    JsBool(a), JsBigInt(_) ->
      loose_equal(JsNumber(Finite(bool_to_float(a))), right)
    JsBigInt(_), JsBool(b) ->
      loose_equal(left, JsNumber(Finite(bool_to_float(b))))
    // Steps 6/7: BigInt × String via StringToBigInt.
    JsBigInt(BigInt(a)), JsString(s) ->
      case string_to_bigint(s) {
        Some(b) -> a == b
        None -> False
      }
    JsString(_), JsBigInt(_) -> loose_equal(right, left)
    // Step 12: BigInt × Number — equal mathematical values only.
    JsBigInt(BigInt(a)), JsNumber(n) -> bigint_equals_number(a, n)
    JsNumber(n), JsBigInt(BigInt(b)) -> bigint_equals_number(b, n)
    _, _ -> value.abstract_equal(left, right)
  }
}

fn bool_to_float(b: Bool) -> Float {
  case b {
    True -> 1.0
    False -> 0.0
  }
}

/// §7.2.14 step 12: ℝ(BigInt) == ℝ(Number)? False for NaN/±Infinity and
/// any Number with a fractional part.
fn bigint_equals_number(a: Int, n: JsNum) -> Bool {
  compare_bigint_num(a, n) == Some(EqOrd)
}

/// §7.1.14 StringToBigInt — decimal (with sign) or 0x/0o/0b prefixed;
/// empty/whitespace-only → 0; anything else fails (None).
fn string_to_bigint(s: String) -> Option(Int) {
  let s = string.trim(s)
  case s {
    "" -> Some(0)
    "0x" <> rest | "0X" <> rest -> parse_bigint_radix_digits(rest, 16)
    "0o" <> rest | "0O" <> rest -> parse_bigint_radix_digits(rest, 8)
    "0b" <> rest | "0B" <> rest -> parse_bigint_radix_digits(rest, 2)
    _ -> int.parse(s) |> option.from_result
  }
}

/// Digits after a 0x/0o/0b prefix. The grammar (§7.1.14
/// NonDecimalIntegerLiteral) has no SignedInteger, so a sign is a failure
/// even though int.base_parse would accept it.
fn parse_bigint_radix_digits(digits: String, base: Int) -> Option(Int) {
  case digits {
    "-" <> _ | "+" <> _ -> None
    _ -> int.base_parse(digits, base) |> option.from_result
  }
}

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

fn num_sub(a: JsNum, b: JsNum) -> JsNum {
  num_add(a, num_negate(b))
}

fn num_mul(a: JsNum, b: JsNum) -> JsNum {
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

fn num_div(a: JsNum, b: JsNum) -> JsNum {
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
    // 0 / 0 = NaN (covers both ±0 / ±0)
    Finite(0.0), Finite(0.0)
    | Finite(-0.0), Finite(0.0)
    | Finite(0.0), Finite(-0.0)
    | Finite(-0.0), Finite(-0.0)
    -> NaN
    // x / ±0 = ±Infinity (sign depends on both operands)
    Finite(x), Finite(0.0) ->
      case x >. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    Finite(x), Finite(-0.0) ->
      case x >. 0.0 {
        True -> NegInfinity
        False -> Infinity
      }
    Finite(x), Finite(y) -> Finite(x /. y)
  }
}

/// Check if a float is negative (including -0.0) — i.e. its IEEE sign bit is
/// set. THE test to use whenever a sign decision must treat -0 as negative:
/// a bare `x <. 0.0` (or its inverse `x >=. 0.0`) is False for -0.0 and
/// silently picks the +0 branch.
pub fn is_negative_float(x: Float) -> Bool {
  x <. 0.0 || is_neg_zero(x)
}

fn num_mod(a: JsNum, b: JsNum) -> JsNum {
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
                Some(_) -> pow_total(x, y)
                // Non-integer exponent with negative base
                None -> NaN
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
fn pow_total(base: Float, exp: Float) -> JsNum

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

pub fn num_negate(n: JsNum) -> JsNum {
  case n {
    Finite(x) -> Finite(float.negate(x))
    NaN -> NaN
    Infinity -> NegInfinity
    NegInfinity -> Infinity
  }
}

/// Apply a JsNum binary operation after coercing both operands to numbers.
pub fn num_binop(
  left: JsValue,
  right: JsValue,
  op: fn(JsNum, JsNum) -> JsNum,
) -> Result(JsValue, OpError) {
  case left, right {
    // Fast path: to_number on a JsNumber is the identity — skip the
    // result combinators (and their closures) for the common case.
    JsNumber(a), JsNumber(b) -> Ok(JsNumber(op(a, b)))
    _, _ -> {
      use a <- result.try(to_number(left))
      use b <- result.map(to_number(right))
      JsNumber(op(a, b))
    }
  }
}

// Relational fast paths for Number × Number (§7.2.13 IsLessThan with both
// operands Numbers): NaN on either side is unordered (`compare_nums` returns
// `UnorderedOrd`) → undefined → false. These are the hottest comparisons in
// the interpreter (`i < n` loop conditions), so they compare atoms directly
// rather than going through `compare_pred`'s closure.
fn num_lt(a: JsNum, b: JsNum) -> Bool {
  compare_nums(a, b) == LtOrd
}

fn num_lt_eq(a: JsNum, b: JsNum) -> Bool {
  case compare_nums(a, b) {
    LtOrd | EqOrd -> True
    GtOrd | UnorderedOrd -> False
  }
}

fn num_gt(a: JsNum, b: JsNum) -> Bool {
  compare_nums(a, b) == GtOrd
}

fn num_gt_eq(a: JsNum, b: JsNum) -> Bool {
  case compare_nums(a, b) {
    GtOrd | EqOrd -> True
    LtOrd | UnorderedOrd -> False
  }
}

/// Apply a bitwise binary operation (convert to i32, operate, convert back).
fn bitwise_binop(
  left: JsValue,
  right: JsValue,
  op: fn(Int, Int) -> Int,
) -> Result(JsValue, OpError) {
  use a <- result.try(to_number(left))
  use b <- result.map(to_number(right))
  value.from_int(op(num_to_int32(a), num_to_int32(b)))
}

// ============================================================================
// Comparison
// ============================================================================

/// Comparison order for relational ops. `UnorderedOrd` is the NaN case: it is
/// a variant rather than an `Option` wrapper so the hot `compare_nums` path
/// stays allocation-free, while every predicate over a `CompareOrd` still has
/// to say what it means (§7.2.13 IsLessThan returns undefined → false).
type CompareOrd {
  LtOrd
  EqOrd
  GtOrd
  UnorderedOrd
}

/// Compare two values for relational operators (<, <=, >, >=).
fn compare_values(
  left: JsValue,
  right: JsValue,
  pred: fn(CompareOrd) -> Bool,
) -> Result(JsValue, OpError) {
  case left, right {
    JsString(a), JsString(b) -> {
      let ord = case string.compare(a, b) {
        order.Lt -> LtOrd
        order.Eq -> EqOrd
        order.Gt -> GtOrd
      }
      Ok(JsBool(pred(ord)))
    }
    // §7.2.13 IsLessThan steps 3.c/d: BigInt × String goes through
    // StringToBigInt; an unparseable string yields undefined → false.
    JsBigInt(BigInt(a)), JsString(s) ->
      case string_to_bigint(s) {
        Some(b) -> Ok(JsBool(pred(compare_ints(a, b))))
        None -> Ok(JsBool(False))
      }
    JsString(s), JsBigInt(BigInt(b)) ->
      case string_to_bigint(s) {
        Some(a) -> Ok(JsBool(pred(compare_ints(a, b))))
        None -> Ok(JsBool(False))
      }
    JsBigInt(BigInt(a)), JsBigInt(BigInt(b)) ->
      Ok(JsBool(pred(compare_ints(a, b))))
    // §7.2.13 step 4.e: BigInt × Number compares mathematical values;
    // NaN on either side → undefined → false.
    JsBigInt(BigInt(a)), _ -> {
      use n <- result.map(to_number(right))
      case compare_bigint_num(a, n) {
        Some(ord) -> JsBool(pred(ord))
        None -> JsBool(False)
      }
    }
    _, JsBigInt(BigInt(b)) -> {
      use n <- result.map(to_number(left))
      case compare_bigint_num(b, n) {
        // n <op> b: flip the BigInt-vs-Number ordering.
        Some(ord) -> JsBool(pred(flip_ord(ord)))
        None -> JsBool(False)
      }
    }
    _, _ -> {
      use a <- result.try(to_number(left))
      use b <- result.map(to_number(right))
      // NaN on either side is unordered → undefined → false (`pred` says so).
      JsBool(pred(compare_nums(a, b)))
    }
  }
}

fn compare_ints(a: Int, b: Int) -> CompareOrd {
  case int.compare(a, b) {
    order.Lt -> LtOrd
    order.Eq -> EqOrd
    order.Gt -> GtOrd
  }
}

fn flip_ord(ord: CompareOrd) -> CompareOrd {
  case ord {
    LtOrd -> GtOrd
    GtOrd -> LtOrd
    EqOrd -> EqOrd
    UnorderedOrd -> UnorderedOrd
  }
}

/// §7.2.13 step 4.e: compare a BigInt's mathematical value against a Number.
/// None means incomparable (NaN).
fn compare_bigint_num(a: Int, n: JsNum) -> Option(CompareOrd) {
  case n {
    NaN -> None
    Infinity -> Some(LtOrd)
    NegInfinity -> Some(GtOrd)
    Finite(f) -> {
      // Compare against floor(f): floats ≥ 2^52 are integral, so floor and
      // the Int round-trip are exact; smaller floats fit Int exactly too.
      let fl = float.floor(f)
      let t = value.float_to_int(fl)
      case a < t, a > t {
        True, _ -> Some(LtOrd)
        _, True -> Some(GtOrd)
        // a == floor(f): a < f exactly when f has a fractional part.
        False, False ->
          case f >. fl {
            True -> Some(LtOrd)
            False -> Some(EqOrd)
          }
      }
    }
  }
}

/// Compare two JsNums. `UnorderedOrd` means either operand is NaN, which every
/// relational operator turns into `false` (§7.2.13 IsLessThan returns
/// undefined). Reporting it as its own variant rather than a fabricated `EqOrd`
/// means a caller cannot forget the NaN case.
fn compare_nums(a: JsNum, b: JsNum) -> CompareOrd {
  case a, b {
    NaN, _ | _, NaN -> UnorderedOrd
    Infinity, Infinity | NegInfinity, NegInfinity -> EqOrd
    Infinity, _ -> GtOrd
    _, Infinity -> LtOrd
    NegInfinity, _ -> LtOrd
    _, NegInfinity -> GtOrd
    // §7.2.13 steps 4.f-4.h: -0 and +0 are neither less nor greater than one
    // another. Never decide this with `x == y` (Gleam `==` on Floats is
    // Erlang `=:=`, which on OTP >= 27 says -0.0 /= 0.0, so `-0 <= 0` came
    // out False and `-0 > 0` came out True). `<.`/`>.` are the arithmetic
    // comparisons, and both are False for the two zeros — hence EqOrd.
    Finite(x), Finite(y) ->
      case x <. y, x >. y {
        True, _ -> LtOrd
        _, True -> GtOrd
        _, _ -> EqOrd
      }
  }
}

// ============================================================================
// Helpers
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
