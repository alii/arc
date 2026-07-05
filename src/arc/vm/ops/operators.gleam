import arc/vm/binop.{
  type ArithOp, type BitwiseOp, type CompareOp, type EqualityOp, type PureBinOp,
  AndOp, ArithDiv, ArithExp, ArithMod, ArithMul, ArithSub, BitAnd, BitOr, BitXor,
  Div, Eq, EqOp, Exp, Gt, GtCmp, GtEq, GtEqCmp, Lt, LtCmp, LtEq, LtEqCmp, Mod,
  Mul, NotEq, NotEqOp, OrOp, Shl, ShlOp, Shr, ShrOp, StrictEq, StrictEqOp,
  StrictNotEq, StrictNotEqOp, Sub, UShr, UShrOp, XorOp,
}
import arc/vm/heap.{type Heap}
import arc/vm/opcode.{type UnaryOpKind, BitNot, LogicalNot, Neg, Pos, Void}

// The Number primitive library. `ops/operators` is the operator DISPATCHER;
// the IEEE 754 arithmetic and the ToInt32/ToUint32 reductions it applies live
// in `ops/numeric`, where `builtins/math` and `ops/coerce` also reach for them.
import arc/vm/ops/numeric.{
  num_div, num_exp, num_mod, num_mul, num_negate, num_sub, num_to_int32,
  num_to_uint32, wrap_int32, wrap_uint32,
}
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
/// (§6.1.6.2), mixed types throw, Number path converts to i32/u32.
///
/// Every Number arm goes through `int32_binop`/`uint32_binop`, which own the
/// wrap: an operator implementation here can only describe the raw Int
/// operation, never whether its result is re-wrapped.
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
        AndOp -> int32_binop(left, right, int.bitwise_and)
        OrOp -> int32_binop(left, right, int.bitwise_or)
        XorOp -> int32_binop(left, right, int.bitwise_exclusive_or)
        // §6.1.6.1.9 Number::leftShift: the raw shift of an int32 by up
        // to 31 bits needs up to 62 bits on the BEAM's unbounded Ints —
        // `int32_binop`'s wrap is what makes `(1<<31)|0` -2147483648
        // rather than 2147483648.
        ShlOp -> {
          use a, b <- int32_binop(left, right)
          int.bitwise_shift_left(a, int.bitwise_and(b, 31))
        }
        ShrOp -> {
          use a, b <- int32_binop(left, right)
          int.bitwise_shift_right(a, int.bitwise_and(b, 31))
        }
        // §6.1.6.1.11 Number::unsignedRightShift is the ONE bitwise op whose
        // operand and result are unsigned — hence its own combinator.
        UShrOp -> {
          use a, b <- uint32_binop(left, right)
          int.bitwise_shift_right(a, int.bitwise_and(b, 31))
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

/// §7.2.14 IsLooselyEqual (`==`), on primitives. THE loose-equality entry point
/// of the engine: object operands are ToPrimitive'd by the caller (see
/// `interpreter.is_eq_coercible`), everything else lands here. It is complete —
/// including the BigInt arms — so there is no half-right sibling to reach for.
pub fn loose_equal(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    // Steps 1-2: same type → strict equality. null == undefined (steps 3/4).
    value.JsNull, value.JsNull
    | value.JsUndefined, value.JsUndefined
    | value.JsNull, value.JsUndefined
    | value.JsUndefined, value.JsNull
    -> True
    JsNumber(_), JsNumber(_)
    | JsBool(_), JsBool(_)
    | JsString(_), JsString(_)
    | value.JsObject(_), value.JsObject(_)
    | value.JsSymbol(_), value.JsSymbol(_)
    | JsBigInt(_), JsBigInt(_)
    -> value.strict_equal(left, right)
    // Steps 10/11: Boolean operand → ToNumber, then redo.
    JsBool(a), _ -> loose_equal(JsNumber(Finite(bool_to_float(a))), right)
    _, JsBool(b) -> loose_equal(left, JsNumber(Finite(bool_to_float(b))))
    // Steps 6/7: BigInt × String via StringToBigInt.
    JsBigInt(BigInt(a)), JsString(s) ->
      case value.string_to_bigint(s) {
        Some(b) -> a == b
        None -> False
      }
    JsString(_), JsBigInt(_) -> loose_equal(right, left)
    // Step 12: BigInt × Number — equal mathematical values only.
    JsBigInt(BigInt(a)), JsNumber(n) -> bigint_equals_number(a, n)
    JsNumber(n), JsBigInt(BigInt(b)) -> bigint_equals_number(b, n)
    // Steps 5/8/9: Number × String → ToNumber(String), which is total on a
    // String (an unparseable one is NaN, and NaN is strictly-equal to
    // nothing) — no Result to unwrap, no error class to swallow.
    JsNumber(_), JsString(s) ->
      value.strict_equal(left, JsNumber(value.string_to_number(s)))
    JsString(_), JsNumber(_) -> loose_equal(right, left)
    _, _ -> False
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

// ============================================================================
// Coercing combinators — the operand ToNumber/ToInt32/ToUint32 wrappers
// ============================================================================

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

/// Apply a signed 32-bit bitwise operation: ToInt32 both operands, run `op` on
/// the exact BEAM Ints, then wrap the RESULT back to int32.
///
/// The wrap belongs to the combinator, not to `op`: the BEAM's Ints are
/// unbounded, so a `<<` (or any future op) whose raw result overflows 32 bits
/// would otherwise leak a value no int32 can hold — `(1 << 31) | 0` must be
/// -2147483648. `op` therefore only ever describes the raw operation, and a
/// new i32 operator cannot forget the wrap.
fn int32_binop(
  left: JsValue,
  right: JsValue,
  op: fn(Int, Int) -> Int,
) -> Result(JsValue, OpError) {
  use a <- result.try(to_number(left))
  use b <- result.map(to_number(right))
  value.from_int(wrap_int32(op(num_to_int32(a), num_to_int32(b))))
}

/// `int32_binop`'s unsigned twin, for §6.1.6.1.11 Number::unsignedRightShift:
/// operands are ToUint32'd and the result is wrapped back to uint32.
fn uint32_binop(
  left: JsValue,
  right: JsValue,
  op: fn(Int, Int) -> Int,
) -> Result(JsValue, OpError) {
  use a <- result.try(to_number(left))
  use b <- result.map(to_number(right))
  value.from_int(wrap_uint32(op(num_to_uint32(a), num_to_uint32(b))))
}

// ============================================================================
// Comparison
// ============================================================================

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
      case value.string_to_bigint(s) {
        Some(b) -> Ok(JsBool(pred(compare_ints(a, b))))
        None -> Ok(JsBool(False))
      }
    JsString(s), JsBigInt(BigInt(b)) ->
      case value.string_to_bigint(s) {
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
// typeof
// ============================================================================

/// ES2024 §13.5.3 The typeof Operator
///
/// Table 41 — typeof Operator Results:
///
///   Type of val                              Result
///   ─────────────────────────────────────────────────────
///   Undefined                                "undefined"
///   Null                                     "object"
///   Boolean                                  "boolean"
///   Number                                   "number"
///   String                                   "string"
///   Symbol                                   "symbol"
///   BigInt                                   "bigint"
///   Object (does not implement [[Call]])      "object"
///   Object (implements [[Call]])              "function"
///
/// JsUninitialized (TDZ sentinel, not in spec) maps to "undefined".
/// This matches V8/SpiderMonkey behavior where accessing a TDZ variable throws
/// a ReferenceError before typeof ever runs, but our compiler may allow typeof
/// on uninitialized bindings as a defensive measure.
pub fn typeof(heap: Heap(ctx, host), val: JsValue) -> String {
  case val {
    // Table 41 row 1: Undefined → "undefined"
    // Also handles JsUninitialized (internal TDZ sentinel, not in spec)
    value.JsUndefined | value.JsUninitialized -> "undefined"
    // Table 41 row 2: Null → "object"
    value.JsNull -> "object"
    // Table 41 row 3: Boolean → "boolean"
    value.JsBool(_) -> "boolean"
    // Table 41 row 4: Number → "number"
    value.JsNumber(_) -> "number"
    // Table 41 row 5: String → "string"
    value.JsString(_) -> "string"
    // Table 41 row 8: BigInt → "bigint"
    value.JsBigInt(_) -> "bigint"
    // Table 41 row 7: Symbol → "symbol"
    value.JsSymbol(_) -> "symbol"
    // Table 41 rows 9-10: Object — check for [[Call]]
    value.JsObject(ref) ->
      case heap.read(heap, ref) {
        // Row 10: Object implements [[Call]] → "function"
        Some(value.ObjectSlot(kind: value.FunctionObject(..), ..))
        | Some(value.ObjectSlot(kind: value.NativeFunction(..), ..))
        | // Proxy: has [[Call]] iff target was callable at creation (§10.5.15);
          // survives revocation (typeof of revoked function proxy = "function").
          Some(value.ObjectSlot(kind: value.ProxyObject(callable: True, ..), ..)) ->
          "function"

        // Row 9: Object does not implement [[Call]] → "object"
        _ -> "object"
      }
  }
}
