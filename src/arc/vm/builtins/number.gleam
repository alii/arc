import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/math as builtins_math
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type JsNum, type JsValue, type Ref, Finite, Infinity, JsNumber, JsObject,
  JsString, JsUndefined, NaN, NativeIsFinite, NativeIsNaN,
  NativeNumberConstructor, NativeNumberIsFinite, NativeNumberIsInteger,
  NativeNumberIsNaN, NativeNumberParseFloat, NativeNumberParseInt,
  NativeNumberPrototypeToString, NativeNumberPrototypeValueOf, NativeParseFloat,
  NativeParseInt, NegInfinity, NumberObject, ObjectSlot,
}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Set up Number constructor + Number.prototype + global parseInt/parseFloat/isNaN/isFinite.
/// Returns #(Heap, BuiltinType, parse_int_ref, parse_float_ref, is_nan_ref, is_finite_ref).
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType, Ref, Ref, Ref, Ref) {
  // Static methods on Number constructor
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #("isNaN", NativeNumberIsNaN, 1),
      #("isFinite", NativeNumberIsFinite, 1),
      #("isInteger", NativeNumberIsInteger, 1),
      #("parseInt", NativeNumberParseInt, 2),
      #("parseFloat", NativeNumberParseFloat, 1),
    ])

  // Static constants
  let constants = [
    #("NaN", value.data(JsNumber(NaN))),
    #("POSITIVE_INFINITY", value.data(JsNumber(Infinity))),
    #("NEGATIVE_INFINITY", value.data(JsNumber(NegInfinity))),
    #("MAX_SAFE_INTEGER", value.data(JsNumber(Finite(9_007_199_254_740_991.0)))),
    #(
      "MIN_SAFE_INTEGER",
      value.data(JsNumber(Finite(-9_007_199_254_740_991.0))),
    ),
    #("EPSILON", value.data(JsNumber(Finite(2.220446049250313e-16)))),
  ]

  // Global utility functions (separate refs — these are standalone globals)
  let #(h, parse_int_ref) =
    common.alloc_native_fn(h, function_proto, NativeParseInt, "parseInt", 2)
  let #(h, parse_float_ref) =
    common.alloc_native_fn(h, function_proto, NativeParseFloat, "parseFloat", 1)
  let #(h, is_nan_ref) =
    common.alloc_native_fn(h, function_proto, NativeIsNaN, "isNaN", 1)
  let #(h, is_finite_ref) =
    common.alloc_native_fn(h, function_proto, NativeIsFinite, "isFinite", 1)

  // Number.prototype methods
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("valueOf", NativeNumberPrototypeValueOf, 0),
      #("toString", NativeNumberPrototypeToString, 1),
    ])

  let ctor_props = list.append(constants, static_methods)
  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(_) { NativeNumberConstructor },
      "Number",
      1,
      ctor_props,
    )

  #(h, bt, parse_int_ref, parse_float_ref, is_nan_ref, is_finite_ref)
}

/// Number(value) — ES2024 §21.1.1.1
///
/// When called as a function (not as a constructor):
///   1. If value is not present, let n be +0.
///   2. Else, let n be ? ToNumber(value).
///   3. (If called as constructor, would create wrapper — not handled here.)
///   4. Return n.
///
/// Note: Constructor semantics (new Number(value)) are handled separately
/// in vm.gleam's construct path, which wraps the result in a NumberObject.
pub fn call_as_function(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: If no arguments, n = +0
  // Step 2: Else, n = ToNumber(value)
  let result = case args {
    [] -> JsNumber(Finite(0.0))
    [val, ..] -> JsNumber(builtins_math.to_number(val))
  }
  #(state, Ok(result))
}

/// parseInt(string, radix) — ES2024 §19.2.5
///
///   1. Let inputString be ? ToString(string).
///   2. Let S be ! TrimString(inputString, START).
///   3. Let sign be 1.
///   4. If S is not empty and S[0] is U+002D (-), set sign to -1.
///   5. If S is not empty and S[0] is U+002B (+) or U+002D (-), remove S[0].
///   6. Let R be ? ToInt32(radix).
///   7. Let stripPrefix be true.
///   8. If R != 0, then
///      a. If R < 2 or R > 36, return NaN.
///      b. If R != 16, set stripPrefix to false.
///   9. Else, set R to 10.
///  10. If stripPrefix is true, then
///      a. If S has length >= 2 and starts with "0x" or "0X",
///         remove first 2 chars and set R to 16.
///  11. If S contains a character not a radix-R digit, let end be the
///      index of the first such character; else let end be the length of S.
///  12. Let Z be the substring of S from 0 to end.
///  13. If Z is empty, return NaN.
///  14. Let mathInt be the mathematical integer from Z in radix R.
///  15. If mathInt = 0 and S[0] was -, return -0.
///  16. Return sign * mathInt.
///
pub fn parse_int(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: Let inputString be ? ToString(string).
  // Step 2: Let S be TrimString(inputString, START).
  let str_result = case args {
    [val, ..] -> {
      use #(s, state) <- result.map(frame.to_string(state, val))
      // Step 2: TrimString(inputString, START) — leading whitespace only
      #(string.trim_start(s), state)
    }
    [] -> Ok(#("", state))
  }
  case str_result {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(str, state)) -> {
      // Steps 6-9: Determine radix R.
      // If R is 0, NaN, or Infinity, default to 10.
      let radix = case args {
        [_, r, ..] ->
          case builtins_math.to_number(r) {
            Finite(n) -> {
              let r = float.truncate(n)
              case r == 0 {
                True -> 10
                False -> r
              }
            }
            _ -> 10
          }
        _ -> 10
      }
      // Step 10: If stripPrefix, check for "0x"/"0X" prefix.
      // Also applies when radix was explicitly 10 (matches spec: radix 0 -> 10,
      // then stripPrefix is true).
      let #(str, radix) = case radix {
        16 ->
          case string.starts_with(str, "0x") || string.starts_with(str, "0X") {
            True -> #(string.drop_start(str, 2), 16)
            False -> #(str, 16)
          }
        10 ->
          case string.starts_with(str, "0x") || string.starts_with(str, "0X") {
            True -> #(string.drop_start(str, 2), 16)
            False -> #(str, 10)
          }
        _ -> #(str, radix)
      }
      // Step 8a: If R < 2 or R > 36, return NaN.
      case radix >= 2 && radix <= 36 {
        False -> #(state, Ok(JsNumber(NaN)))
        True -> {
          // Steps 3-5, 11-16: Parse sign + digits in parse_int_digits.
          let result = parse_int_digits(str, radix)
          #(state, Ok(JsNumber(result)))
        }
      }
    }
  }
}

/// parseFloat(string) — ES2024 §19.2.4
///
///   1. Let inputString be ? ToString(string).
///   2. Let trimmedString be ! TrimString(inputString, START).
///   3. If neither trimmedString nor any prefix of trimmedString satisfies
///      the syntax of a StrDecimalLiteral, return NaN.
///   4. Let numberString be the longest prefix of trimmedString that
///      satisfies the syntax of a StrDecimalLiteral.
///   5. Let parsedNumber be ParseText(numberString, StrDecimalLiteral).
///   6. Return StringNumericValue of parsedNumber.
///
/// StrDecimalLiteral includes: "Infinity", decimal literals with optional
/// sign, integer literals. Does NOT include "0x" hex, "0o" octal, "0b"
/// binary, or BigInt "n" suffix.
///
/// TODO(Deviation): Does not implement longest-prefix parsing — the full trimmed
/// string is attempted. E.g. parseFloat("123abc") should return 123 but
/// this implementation returns NaN.
pub fn parse_float(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: Let inputString be ? ToString(string).
  // Step 2: Let trimmedString be TrimString(inputString, START).
  let str_result = case args {
    [val, ..] -> {
      use #(s, state) <- result.map(frame.to_string(state, val))
      // Step 2: TrimString(inputString, START) — leading whitespace only
      #(string.trim_start(s), state)
    }
    [] -> Ok(#("", state))
  }
  case str_result {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(str, state)) ->
      // Steps 3-6: Parse as StrDecimalLiteral.
      // Handle "Infinity"/"+Infinity"/"-Infinity" specially.
      case str {
        "Infinity" | "+Infinity" -> #(state, Ok(JsNumber(Infinity)))
        "-Infinity" -> #(state, Ok(JsNumber(NegInfinity)))
        _ ->
          case gleam_stdlib_parse_float(str) {
            Ok(n) -> #(state, Ok(JsNumber(Finite(n))))
            Error(_) ->
              case int.parse(str) {
                Ok(n) -> #(state, Ok(JsNumber(Finite(int.to_float(n)))))
                Error(_) -> #(state, Ok(JsNumber(NaN)))
              }
          }
      }
  }
}

/// isNaN(number) — ES2024 §19.2.3
///
///   1. Let num be ? ToNumber(number).
///   2. If num is NaN, return true.
///   3. Otherwise, return false.
///
/// Note: Unlike Number.isNaN, this coerces the argument via ToNumber first.
/// So isNaN("hello") is true (ToNumber("hello") = NaN), but
/// Number.isNaN("hello") is false (not a Number type at all).
pub fn js_is_nan(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Step 1: Let num be ? ToNumber(number).
  let num = builtins_math.to_number(val)
  // Steps 2-3: If num is NaN, return true; else false.
  let result = case num {
    NaN -> value.JsBool(True)
    _ -> value.JsBool(False)
  }
  #(state, Ok(result))
}

/// isFinite(number) — ES2024 §19.2.2
///
///   1. Let num be ? ToNumber(number).
///   2. If num is not finite (i.e. NaN, +Inf, or -Inf), return false.
///   3. Otherwise, return true.
///
/// Note: Unlike Number.isFinite, this coerces via ToNumber first.
/// So isFinite("42") is true, but Number.isFinite("42") is false.
pub fn js_is_finite(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Step 1: Let num be ? ToNumber(number).
  let num = builtins_math.to_number(val)
  // Steps 2-3: If num is finite, return true; else false.
  let result = case num {
    Finite(_) -> value.JsBool(True)
    _ -> value.JsBool(False)
  }
  #(state, Ok(result))
}

/// Number.isNaN(number) — ES2024 §21.1.2.4
///
///   1. If number is not a Number, return false.
///   2. If number is NaN, return true.
///   3. Otherwise, return false.
pub fn number_is_nan(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-3: Only true if argument is literally the Number value NaN.
  let result = case args {
    [JsNumber(NaN), ..] -> value.JsBool(True)
    _ -> value.JsBool(False)
  }
  #(state, Ok(result))
}

/// Number.isFinite(number) — ES2024 §21.1.2.1
///
///   1. If number is not a Number, return false.
///   2. If number is not finite (NaN, +Inf, -Inf), return false.
///   3. Otherwise, return true.
pub fn number_is_finite(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-3: Only true if argument is a Number and is finite.
  let result = case args {
    [JsNumber(Finite(_)), ..] -> value.JsBool(True)
    _ -> value.JsBool(False)
  }
  #(state, Ok(result))
}

/// Number.isInteger(number) — ES2024 §21.1.2.3
///
///   1. If number is not a Number, return false.
///   2. If number is not finite (NaN, +Inf, -Inf), return false.
///   3. Let integer be truncate(number) (i.e. round toward zero).
///   4. If integer != number, return false.
///   5. Return true.
pub fn number_is_integer(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let result = case args {
    // Steps 1-2: Must be a finite Number.
    [JsNumber(Finite(n)), ..] -> {
      // Steps 3-5: truncate(n) == n means no fractional part.
      let truncated = int.to_float(float.truncate(n))
      value.JsBool(truncated == n)
    }
    _ -> value.JsBool(False)
  }
  #(state, Ok(result))
}

/// Number.prototype.valueOf() — ES2024 §21.1.3.7
///
///   1. Return ? thisNumberValue(this value).
///
/// thisNumberValue (§21.1.3) either returns the Number primitive directly
/// or unwraps [[NumberData]] from a Number wrapper object, else throws
/// TypeError.
pub fn number_value_of(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: Return ? thisNumberValue(this value).
  case this_number_value(state, this) {
    Some(n) -> #(state, Ok(JsNumber(n)))
    None ->
      frame.type_error(
        state,
        "Number.prototype.valueOf requires that 'this' be a Number",
      )
  }
}

/// Number.prototype.toString([radix]) — ES2024 §21.1.3.6
///
///   1. Let x be ? thisNumberValue(this value).
///   2. If radix is undefined, let radixMV be 10.
///   3. Else, let radixMV be ? ToIntegerOrInfinity(radix).
///   4. If radixMV is not in the inclusive interval from 2 to 36, throw
///      a RangeError exception.
///   5. Return Number::toString(x, radixMV).
///
/// Number::toString(x, radix):
///   - If radix is 10, return ! ToString(x) (standard decimal formatting).
///   - Else, return the String representation of x using the specified radix.
///     NaN, +Infinity, -Infinity ignore the radix and use their canonical
///     string forms.
///
/// Note: Non-integer values with non-10 radix fall back to decimal
/// formatting. Proper fractional radix conversion is not implemented.
pub fn number_to_string(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: Let x be ? thisNumberValue(this value).
  case this_number_value(state, this) {
    None ->
      frame.type_error(
        state,
        "Number.prototype.toString requires that 'this' be a Number",
      )
    Some(n) -> {
      // Steps 2-3: radix defaults to 10; undefined -> 10.
      let radix = case args {
        [] | [JsUndefined, ..] -> 10
        [r, ..] ->
          case builtins_math.to_number(r) {
            Finite(f) -> float.truncate(f)
            _ -> 10
          }
      }
      // Step 4: If radixMV not in [2, 36], throw RangeError.
      case radix >= 2 && radix <= 36 {
        False -> range_error(state, "toString() radix must be between 2 and 36")
        // Step 5: Return Number::toString(x, radixMV).
        True -> #(state, Ok(JsString(format_number_radix(n, radix))))
      }
    }
  }
}

/// Number::toString(x, radixMV) helper — ES2024 §21.1.3.6 step 5.
///
/// Per spec, NaN/+Infinity/-Infinity always use their canonical string forms
/// regardless of radix. For finite values:
///   - Radix 10: use standard Number::toString (decimal formatting).
///   - Other radix: convert the integer part to the specified base using
///     lowercase digits (a-z for 10-35).
///
/// Note: Non-integer values with non-10 radix fall back to decimal.
/// The spec requires proper fractional digit conversion (e.g. 3.5 in base 16
/// should produce "3.8"), but this is rarely used and complex to implement.
fn format_number_radix(n: JsNum, radix: Int) -> String {
  case n {
    NaN -> "NaN"
    Infinity -> "Infinity"
    NegInfinity -> "-Infinity"
    Finite(f) ->
      case radix {
        10 -> value.js_format_number(f)
        _ -> {
          let truncated = float.truncate(f)
          case int.to_float(truncated) == f {
            // Integer value — use radix conversion. int.to_base_string
            // returns uppercase (via erlang integer_to_binary/2); JS wants
            // lowercase. The function handles sign ("-ff" for -255) which
            // matches JS semantics.
            True ->
              int.to_base_string(truncated, radix)
              |> result.map(string.lowercase)
              // radix already validated as 2-36 by caller
              |> result.unwrap(value.js_format_number(f))
            // Non-integer with non-10 radix — fall back to decimal.
            False -> value.js_format_number(f)
          }
        }
      }
  }
}

/// thisNumberValue(value) — ES2024 §21.1.3
///
///   1. If value is a Number, return value.
///   2. If value is an Object and value has a [[NumberData]] internal slot,
///      then
///      a. Let n be value.[[NumberData]].
///      b. Assert: n is a Number.
///      c. Return n.
///   3. Throw a TypeError exception.
///
/// Used by Number.prototype.valueOf and Number.prototype.toString to
/// unwrap `this`. Returns None instead of throwing — caller is responsible
/// for producing the TypeError.
fn this_number_value(state: State, this: JsValue) -> Option(JsNum) {
  case this {
    // Step 1: If value is a Number, return value.
    JsNumber(n) -> Some(n)
    // Step 2: If value is an Object with [[NumberData]], return it.
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: NumberObject(value: n), ..)) -> Some(n)
        _ -> None
      }
    // Step 3: (caller throws TypeError)
    _ -> None
  }
}

fn range_error(state: State, msg: String) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, err) = common.make_range_error(state.heap, state.builtins, msg)
  #(State(..state, heap:), Error(err))
}

// ============================================================================
// Internal helpers
// ============================================================================

/// parseInt digit parsing — implements ES2024 §19.2.5 steps 3-5, 11-16.
///
/// Steps 3-5: Handle leading sign (+ or -).
/// Steps 11-12: Parse digits until first non-radix-R character.
/// Step 13: If no valid digits found, return NaN.
/// Steps 14-16: Compute mathematical integer value with sign.
///
fn parse_int_digits(s: String, radix: Int) -> value.JsNum {
  // Steps 3-5: Handle leading sign.
  let #(s, negative) = case string.first(s) {
    Ok("-") -> #(string.drop_start(s, 1), True)
    Ok("+") -> #(string.drop_start(s, 1), False)
    _ -> #(s, False)
  }
  // Steps 11-14: Parse valid digits, stop at first invalid character.
  let graphemes = string.to_graphemes(s)
  case parse_digits_loop(graphemes, radix, 0, False) {
    // Step 13: If Z is empty, return NaN.
    None -> NaN
    // Steps 14-16: Apply sign and return.
    Some(n) ->
      case negative {
        // Step 15: If sign = -1 and mathInt = 0, return -0.
        True if n == 0 -> Finite(-0.0)
        True -> Finite(int.to_float(-n))
        False -> Finite(int.to_float(n))
      }
  }
}

/// parseInt digit accumulation loop — ES2024 §19.2.5 steps 11-14.
///
/// Iterates through characters, accumulating digits valid in the given radix.
/// Stops at the first character that is not a valid radix-R digit (step 11).
/// Returns None if no valid digits were found (step 13: Z is empty).
fn parse_digits_loop(
  graphemes: List(String),
  radix: Int,
  acc: Int,
  found_any: Bool,
) -> Option(Int) {
  case graphemes {
    [] ->
      case found_any {
        True -> Some(acc)
        False -> None
      }
    [ch, ..rest] ->
      case digit_value(ch) {
        Some(d) if d < radix ->
          parse_digits_loop(rest, radix, acc * radix + d, True)
        _ ->
          case found_any {
            True -> Some(acc)
            False -> None
          }
      }
  }
}

/// Map a character to its digit value for parseInt radix conversion.
/// Supports 0-9 (values 0-9) and a-z/A-Z (values 10-35), covering
/// all radixes from 2 to 36. The caller checks `d < radix` to reject
/// digits outside the current radix.
fn digit_value(ch: String) -> Option(Int) {
  case ch {
    "0" -> Some(0)
    "1" -> Some(1)
    "2" -> Some(2)
    "3" -> Some(3)
    "4" -> Some(4)
    "5" -> Some(5)
    "6" -> Some(6)
    "7" -> Some(7)
    "8" -> Some(8)
    "9" -> Some(9)
    "a" | "A" -> Some(10)
    "b" | "B" -> Some(11)
    "c" | "C" -> Some(12)
    "d" | "D" -> Some(13)
    "e" | "E" -> Some(14)
    "f" | "F" -> Some(15)
    "g" | "G" -> Some(16)
    "h" | "H" -> Some(17)
    "i" | "I" -> Some(18)
    "j" | "J" -> Some(19)
    "k" | "K" -> Some(20)
    "l" | "L" -> Some(21)
    "m" | "M" -> Some(22)
    "n" | "N" -> Some(23)
    "o" | "O" -> Some(24)
    "p" | "P" -> Some(25)
    "q" | "Q" -> Some(26)
    "r" | "R" -> Some(27)
    "s" | "S" -> Some(28)
    "t" | "T" -> Some(29)
    "u" | "U" -> Some(30)
    "v" | "V" -> Some(31)
    "w" | "W" -> Some(32)
    "x" | "X" -> Some(33)
    "y" | "Y" -> Some(34)
    "z" | "Z" -> Some(35)
    _ -> None
  }
}

@external(erlang, "gleam_stdlib", "parse_float")
fn gleam_stdlib_parse_float(s: String) -> Result(Float, Nil)
