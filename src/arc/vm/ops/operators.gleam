import arc/vm/builtins/math as builtins_math
import arc/vm/opcode.{
  type BinOpKind, type UnaryOpKind, Add, BitAnd, BitNot, BitOr, BitXor, Div, Eq,
  Exp, Gt, GtEq, LogicalNot, Lt, LtEq, Mod, Mul, Neg, NotEq, Pos, ShiftLeft,
  ShiftRight, StrictEq, StrictNotEq, Sub, UShiftRight, Void,
}
import arc/vm/value.{
  type JsNum, type JsValue, BigInt, Finite, Infinity, JsBigInt, JsBool, JsNumber,
  JsString, NaN, NegInfinity,
}
import gleam/float
import gleam/int
import gleam/list
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

/// Execute a binary operation on two JsValues.
pub fn exec_binop(
  kind: BinOpKind,
  left: JsValue,
  right: JsValue,
) -> Result(JsValue, OpError) {
  case kind {
    // Add is handled directly in the BinOp dispatcher with ToPrimitive
    Add -> panic as "Add should be handled in BinOp dispatcher"
    Sub | Mul | Div | Mod | Exp ->
      case left, right {
        // §6.1.6.2 BigInt arithmetic.
        JsBigInt(BigInt(a)), JsBigInt(BigInt(b)) -> bigint_arith(kind, a, b)
        // §13.15.4 ApplyStringOrNumericBinaryOperator step 7.a:
        // Type(lnum) ≠ Type(rnum) → TypeError.
        JsBigInt(_), _ | _, JsBigInt(_) -> Error(bigint_mix_error)
        // Fast path: both operands are already Numbers — to_number is the
        // identity, so call the JsNum op directly without combinators or
        // closure indirection.
        JsNumber(a), JsNumber(b) ->
          Ok(
            JsNumber(case kind {
              Sub -> num_sub(a, b)
              Mul -> num_mul(a, b)
              Div -> num_div(a, b)
              Mod -> num_mod(a, b)
              _ -> num_exp(a, b)
            }),
          )
        _, _ ->
          case kind {
            Sub -> num_binop(left, right, num_sub)
            Mul -> num_binop(left, right, num_mul)
            Div -> num_binop(left, right, num_div)
            Mod -> num_binop(left, right, num_mod)
            _ -> num_binop(left, right, num_exp)
          }
      }

    // Bitwise / shifts — BigInt×BigInt operates on arbitrary precision
    // (§6.1.6.2), mixed types throw, Number path converts to i32.
    BitAnd | BitOr | BitXor | ShiftLeft | ShiftRight | UShiftRight ->
      case left, right {
        JsBigInt(BigInt(a)), JsBigInt(BigInt(b)) -> bigint_bitwise(kind, a, b)
        JsBigInt(_), _ | _, JsBigInt(_) -> Error(bigint_mix_error)
        _, _ ->
          case kind {
            BitAnd -> bitwise_binop(left, right, int.bitwise_and)
            BitOr -> bitwise_binop(left, right, int.bitwise_or)
            BitXor -> bitwise_binop(left, right, int.bitwise_exclusive_or)
            ShiftLeft -> {
              use a, b <- bitwise_binop(left, right)
              int.bitwise_shift_left(a, int.bitwise_and(b, 31))
            }
            ShiftRight -> {
              use a, b <- bitwise_binop(left, right)
              int.bitwise_shift_right(a, int.bitwise_and(b, 31))
            }
            _ -> {
              use a, b <- bitwise_binop(left, right)
              int.bitwise_shift_right(
                int.bitwise_and(a, 0xFFFFFFFF),
                int.bitwise_and(b, 31),
              )
            }
          }
      }

    // Comparison
    StrictEq -> Ok(JsBool(value.strict_equal(left, right)))
    StrictNotEq -> Ok(JsBool(!value.strict_equal(left, right)))
    Eq -> Ok(JsBool(loose_equal(left, right)))
    NotEq -> Ok(JsBool(!loose_equal(left, right)))

    // Relational: fast path for Number × Number avoids the per-op pred
    // closure and the to_number round-trip in compare_values.
    Lt ->
      case left, right {
        JsNumber(a), JsNumber(b) -> Ok(JsBool(num_lt(a, b)))
        _, _ -> {
          use ord <- compare_values(left, right)
          ord == LtOrd
        }
      }
    LtEq ->
      case left, right {
        JsNumber(a), JsNumber(b) -> Ok(JsBool(num_lt_eq(a, b)))
        _, _ -> {
          use ord <- compare_values(left, right)
          ord == LtOrd || ord == EqOrd
        }
      }
    Gt ->
      case left, right {
        JsNumber(a), JsNumber(b) -> Ok(JsBool(num_gt(a, b)))
        _, _ -> {
          use ord <- compare_values(left, right)
          ord == GtOrd
        }
      }
    GtEq ->
      case left, right {
        JsNumber(a), JsNumber(b) -> Ok(JsBool(num_gt_eq(a, b)))
        _, _ -> {
          use ord <- compare_values(left, right)
          ord == GtOrd || ord == EqOrd
        }
      }

    // In and InstanceOf handled in BinOp dispatcher (needs heap access)
    opcode.In | opcode.InstanceOf ->
      panic as "in/instanceof are handled in the BinOp dispatcher"
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

/// ToNumber for an operator operand. value.to_number only fails on values
/// ToNumber rejects (BigInt, Symbol), and that failure is always a TypeError.
fn to_number(v: JsValue) -> Result(JsNum, OpError) {
  result.map_error(value.to_number(v), OpTypeError)
}

fn bigint_arith(kind: BinOpKind, a: Int, b: Int) -> Result(JsValue, OpError) {
  case kind {
    Sub -> Ok(JsBigInt(BigInt(a - b)))
    Mul -> Ok(JsBigInt(BigInt(a * b)))
    // §6.1.6.2.5 BigInt::divide — truncating division; 0n divisor throws
    // RangeError. Gleam's Int `/` is Erlang div (truncates toward zero).
    Div ->
      case b {
        0 -> Error(OpRangeError("Division by zero"))
        _ -> Ok(JsBigInt(BigInt(a / b)))
      }
    // §6.1.6.2.6 BigInt::remainder — sign of dividend; 0n divisor throws
    // RangeError. Gleam's Int `%` is Erlang rem (sign of dividend).
    Mod ->
      case b {
        0 -> Error(OpRangeError("Division by zero"))
        _ -> Ok(JsBigInt(BigInt(a % b)))
      }
    // §6.1.6.2.3 BigInt::exponentiate — negative exponent throws RangeError.
    Exp ->
      case b < 0 {
        True -> Error(OpRangeError("Exponent must be non-negative"))
        False -> Ok(JsBigInt(BigInt(bigint_pow(a, b, 1))))
      }
    _ -> Error(OpTypeError("BigInt arithmetic: unreachable operator"))
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

fn bigint_bitwise(kind: BinOpKind, a: Int, b: Int) -> Result(JsValue, OpError) {
  case kind {
    // Erlang band/bor/bxor on arbitrary-precision ints follow infinite
    // two's-complement semantics — exactly §6.1.6.2.17-19.
    BitAnd -> Ok(JsBigInt(BigInt(int.bitwise_and(a, b))))
    BitOr -> Ok(JsBigInt(BigInt(int.bitwise_or(a, b))))
    BitXor -> Ok(JsBigInt(BigInt(int.bitwise_exclusive_or(a, b))))
    // §6.1.6.2.9/10: shifts are arbitrary-precision; Erlang bsl/bsr accept
    // negative counts and shift the other way, matching leftShift(x, -y).
    ShiftLeft -> Ok(JsBigInt(BigInt(int.bitwise_shift_left(a, b))))
    ShiftRight -> Ok(JsBigInt(BigInt(int.bitwise_shift_right(a, b))))
    // §6.1.6.2.11 BigInt::unsignedRightShift always throws.
    UShiftRight ->
      Error(OpTypeError("BigInts have no unsigned right shift, use >> instead"))
    _ -> Error(OpTypeError("BigInt bitwise: unreachable operator"))
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
    Infinity, Finite(0.0) | Finite(0.0), Infinity -> NaN
    NegInfinity, Finite(0.0) | Finite(0.0), NegInfinity -> NaN
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

/// Check if a float is negative (including -0.0).
fn is_negative_float(x: Float) -> Bool {
  x <. 0.0 || builtins_math.is_neg_zero(x)
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

fn num_exp(a: JsNum, b: JsNum) -> JsNum {
  builtins_math.num_exp(a, b)
}

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
// operands Numbers): NaN on either side → undefined → false.
fn num_lt(a: JsNum, b: JsNum) -> Bool {
  case a, b {
    NaN, _ | _, NaN -> False
    _, _ -> compare_nums(a, b) == LtOrd
  }
}

fn num_lt_eq(a: JsNum, b: JsNum) -> Bool {
  case a, b {
    NaN, _ | _, NaN -> False
    _, _ -> compare_nums(a, b) != GtOrd
  }
}

fn num_gt(a: JsNum, b: JsNum) -> Bool {
  case a, b {
    NaN, _ | _, NaN -> False
    _, _ -> compare_nums(a, b) == GtOrd
  }
}

fn num_gt_eq(a: JsNum, b: JsNum) -> Bool {
  case a, b {
    NaN, _ | _, NaN -> False
    _, _ -> compare_nums(a, b) != LtOrd
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

/// Comparison order for relational ops.
type CompareOrd {
  LtOrd
  EqOrd
  GtOrd
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
      use b <- result.try(to_number(right))
      case a, b {
        NaN, _ | _, NaN -> Ok(JsBool(False))
        _, _ -> Ok(JsBool(pred(compare_nums(a, b))))
      }
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

/// Compare two JsNums (neither is NaN).
fn compare_nums(a: JsNum, b: JsNum) -> CompareOrd {
  case a, b {
    Infinity, Infinity | NegInfinity, NegInfinity -> EqOrd
    Infinity, _ -> GtOrd
    _, Infinity -> LtOrd
    NegInfinity, _ -> LtOrd
    _, NegInfinity -> GtOrd
    Finite(x), Finite(y) ->
      case x == y {
        True -> EqOrd
        False ->
          case x <. y {
            True -> LtOrd
            False -> GtOrd
          }
      }
    // NaN cases handled by caller
    NaN, _ | _, NaN -> EqOrd
  }
}

// ============================================================================
// Helpers
// ============================================================================

/// Convert JsNum to int32 (JS ToInt32).
pub fn num_to_int32(n: JsNum) -> Int {
  case n {
    NaN | Infinity | NegInfinity -> 0
    Finite(f) -> {
      let i = float.truncate(f)
      // Wrap to 32 bits
      let wrapped = int.bitwise_and(i, 0xFFFFFFFF)
      // Sign extend if needed
      case wrapped > 0x7FFFFFFF {
        True -> wrapped - 0x100000000
        False -> wrapped
      }
    }
  }
}

// ============================================================================
// URI encoding/decoding FFI
// ============================================================================

@external(erlang, "arc_uri_ffi", "encode")
pub fn uri_encode(str: String, preserve_uri_chars: Bool) -> String

@external(erlang, "arc_uri_ffi", "decode")
pub fn uri_decode(str: String) -> String

// ============================================================================
// AnnexB escape / unescape (B.2.1.1 / B.2.1.2)
// ============================================================================

/// Characters that escape() preserves as-is (unreserved set).
/// Per B.2.1.1: A-Z, a-z, 0-9, @, *, _, +, -, ., /
fn is_escape_safe(cp: Int) -> Bool {
  // A-Z
  { cp >= 65 && cp <= 90 }
  // a-z
  || { cp >= 97 && cp <= 122 }
  // 0-9
  || { cp >= 48 && cp <= 57 }
  // @
  || cp == 64
  // *
  || cp == 42
  // _
  || cp == 95
  // +
  || cp == 43
  // -
  || cp == 45
  // .
  || cp == 46
  // /
  || cp == 47
}

/// Format an integer as uppercase hex with at least `width` digits.
fn to_hex_upper(n: Int, width: Int) -> String {
  let hex =
    int.to_base_string(n, 16) |> result.unwrap("0") |> string.uppercase()
  let pad = width - string.length(hex)
  case pad > 0 {
    True -> string.repeat("0", pad) <> hex
    False -> hex
  }
}

/// ES AnnexB B.2.1.1 escape ( string )
pub fn js_escape(input: String) -> String {
  string.to_utf_codepoints(input)
  |> list.map(fn(cp) {
    let code = string.utf_codepoint_to_int(cp)
    case is_escape_safe(code) {
      True -> string.from_utf_codepoints([cp])
      False ->
        case code < 256 {
          True -> "%" <> to_hex_upper(code, 2)
          False -> "%u" <> to_hex_upper(code, 4)
        }
    }
  })
  |> string.join("")
}

/// ES AnnexB B.2.1.2 unescape ( string )
pub fn js_unescape(input: String) -> String {
  js_unescape_loop(string.to_graphemes(input), "")
}

fn js_unescape_loop(chars: List(String), acc: String) -> String {
  case chars {
    [] -> acc
    ["%", "u", a, b, c, d, ..rest] -> {
      let hex = a <> b <> c <> d
      case int.base_parse(hex, 16) {
        Ok(code) ->
          case string.utf_codepoint(code) {
            Ok(cp) ->
              js_unescape_loop(rest, acc <> string.from_utf_codepoints([cp]))
            Error(Nil) -> js_unescape_loop(rest, acc <> "%u" <> hex)
          }
        Error(Nil) -> js_unescape_loop([a, b, c, d, ..rest], acc <> "%u")
      }
    }
    ["%", a, b, ..rest] -> {
      let hex = a <> b
      case int.base_parse(hex, 16) {
        Ok(code) ->
          case string.utf_codepoint(code) {
            Ok(cp) ->
              js_unescape_loop(rest, acc <> string.from_utf_codepoints([cp]))
            Error(Nil) -> js_unescape_loop(rest, acc <> "%" <> hex)
          }
        Error(Nil) -> js_unescape_loop([a, b, ..rest], acc <> "%")
      }
    }
    [c, ..rest] -> js_unescape_loop(rest, acc <> c)
  }
}
