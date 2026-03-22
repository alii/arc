import arc/vm/opcode.{
  type BinOpKind, type UnaryOpKind, Add, BitAnd, BitNot, BitOr, BitXor, Div, Eq,
  Exp, Gt, GtEq, LogicalNot, Lt, LtEq, Mod, Mul, Neg, NotEq, Pos, ShiftLeft,
  ShiftRight, StrictEq, StrictNotEq, Sub, UShiftRight, Void,
}
import arc/vm/value.{
  type JsNum, type JsValue, Finite, Infinity, JsBool, JsNumber, JsString, NaN,
  NegInfinity,
}
import gleam/float
import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/string

// ============================================================================
// Binary and unary operator dispatch
// ============================================================================

/// Execute a binary operation on two JsValues.
pub fn exec_binop(
  kind: BinOpKind,
  left: JsValue,
  right: JsValue,
) -> Result(JsValue, String) {
  case kind {
    // Add is handled directly in the BinOp dispatcher with ToPrimitive
    Add -> panic as "Add should be handled in BinOp dispatcher"
    Sub -> num_binop(left, right, num_sub)
    Mul -> num_binop(left, right, num_mul)
    Div -> num_binop(left, right, num_div)
    Mod -> num_binop(left, right, num_mod)
    Exp -> num_binop(left, right, num_exp)

    // Bitwise — convert to i32, operate, convert back
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
    UShiftRight -> {
      use a, b <- bitwise_binop(left, right)
      int.bitwise_shift_right(
        int.bitwise_and(a, 0xFFFFFFFF),
        int.bitwise_and(b, 31),
      )
    }

    // Comparison
    StrictEq -> Ok(JsBool(value.strict_equal(left, right)))
    StrictNotEq -> Ok(JsBool(!value.strict_equal(left, right)))
    Eq -> Ok(JsBool(value.abstract_equal(left, right)))
    NotEq -> Ok(JsBool(!value.abstract_equal(left, right)))

    Lt -> {
      use ord <- compare_values(left, right)
      ord == LtOrd
    }
    LtEq -> {
      use ord <- compare_values(left, right)
      ord == LtOrd || ord == EqOrd
    }
    Gt -> {
      use ord <- compare_values(left, right)
      ord == GtOrd
    }
    GtEq -> {
      use ord <- compare_values(left, right)
      ord == GtOrd || ord == EqOrd
    }

    // In and InstanceOf handled in BinOp dispatcher (needs heap access)
    opcode.In -> Error("in: unreachable — handled in dispatcher")
    opcode.InstanceOf ->
      Error("instanceof: unreachable — handled in dispatcher")
  }
}

/// Execute a unary operation.
pub fn exec_unaryop(
  kind: UnaryOpKind,
  operand: JsValue,
) -> Result(JsValue, String) {
  case kind {
    Neg -> {
      use n <- result.map(value.to_number(operand))
      JsNumber(num_negate(n))
    }
    Pos -> {
      use n <- result.map(value.to_number(operand))
      JsNumber(n)
    }
    BitNot -> {
      use n <- result.map(value.to_number(operand))
      JsNumber(Finite(int.to_float(int.bitwise_not(num_to_int32(n)))))
    }
    LogicalNot -> Ok(JsBool(!value.is_truthy(operand)))
    Void -> Ok(value.JsUndefined)
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
      case x >=. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    NegInfinity, Finite(x) ->
      case x >=. 0.0 {
        True -> NegInfinity
        False -> Infinity
      }
    Finite(_), Infinity | Finite(_), NegInfinity -> Finite(0.0)
    Finite(0.0), Finite(0.0) -> NaN
    Finite(x), Finite(0.0) ->
      case x >. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    Finite(x), Finite(y) -> Finite(x /. y)
  }
}

fn num_mod(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, _ | NegInfinity, _ -> NaN
    _, Infinity | _, NegInfinity -> a
    Finite(_), Finite(0.0) -> NaN
    Finite(0.0), Finite(_) -> Finite(0.0)
    Finite(x), Finite(y) ->
      Finite(x -. int.to_float(float.truncate(x /. y)) *. y)
  }
}

fn num_exp(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    _, Finite(0.0) -> Finite(1.0)
    _, NaN -> NaN
    NaN, _ -> NaN
    Finite(x), Finite(y) -> Finite(float_power(x, y))
    Infinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    NegInfinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    _, Infinity -> NaN
    _, NegInfinity -> NaN
  }
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
) -> Result(JsValue, String) {
  use a <- result.try(value.to_number(left))
  use b <- result.map(value.to_number(right))
  JsNumber(op(a, b))
}

/// Apply a bitwise binary operation (convert to i32, operate, convert back).
fn bitwise_binop(
  left: JsValue,
  right: JsValue,
  op: fn(Int, Int) -> Int,
) -> Result(JsValue, String) {
  use a <- result.try(value.to_number(left))
  use b <- result.map(value.to_number(right))
  JsNumber(Finite(int.to_float(op(num_to_int32(a), num_to_int32(b)))))
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
) -> Result(JsValue, String) {
  case left, right {
    JsString(a), JsString(b) -> {
      let ord = case string.compare(a, b) {
        order.Lt -> LtOrd
        order.Eq -> EqOrd
        order.Gt -> GtOrd
      }
      Ok(JsBool(pred(ord)))
    }
    _, _ -> {
      use a <- result.try(value.to_number(left))
      use b <- result.try(value.to_number(right))
      case a, b {
        NaN, _ | _, NaN -> Ok(JsBool(False))
        _, _ -> Ok(JsBool(pred(compare_nums(a, b))))
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

/// Convert a primitive JsValue to JsNum for arithmetic (ToNumber lite).
pub fn to_number_for_binop(val: JsValue) -> JsNum {
  value.to_number(val) |> result.unwrap(NaN)
}

// ============================================================================
// Float helpers — only power needs FFI now
// ============================================================================

@external(erlang, "math", "pow")
@external(javascript, "../../../arc_math_ffi.mjs", "float_power")
pub fn float_power(base: Float, exp: Float) -> Float

// ============================================================================
// URI encoding/decoding FFI
// ============================================================================

@external(erlang, "arc_uri_ffi", "encode")
@external(javascript, "../../../arc_uri_ffi.mjs", "encode")
pub fn uri_encode(str: String, preserve_uri_chars: Bool) -> String

@external(erlang, "arc_uri_ffi", "decode")
@external(javascript, "../../../arc_uri_ffi.mjs", "decode")
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
