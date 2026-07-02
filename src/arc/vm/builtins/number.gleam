import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/ops/coerce
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type JsNum, type JsValue, type NumberNativeFn, type Ref, Dispatch, Finite,
  GlobalIsFinite, GlobalIsNaN, GlobalParseFloat, GlobalParseInt, Infinity,
  JsNumber, JsObject, JsString, JsUndefined, NaN, NegInfinity, NumberConstructor,
  NumberIsFinite, NumberIsInteger, NumberIsNaN, NumberIsSafeInteger,
  NumberNative, NumberObject, NumberParseFloat, NumberParseInt,
  NumberPrototypeToExponential, NumberPrototypeToFixed,
  NumberPrototypeToPrecision, NumberPrototypeToString, NumberPrototypeValueOf,
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
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType, Ref, Ref, Ref, Ref) {
  // Static methods on Number constructor
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #("isNaN", NumberNative(NumberIsNaN), 1),
      #("isFinite", NumberNative(NumberIsFinite), 1),
      #("isInteger", NumberNative(NumberIsInteger), 1),
      #("isSafeInteger", NumberNative(NumberIsSafeInteger), 1),
      #("parseInt", NumberNative(NumberParseInt), 2),
      #("parseFloat", NumberNative(NumberParseFloat), 1),
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
    // §21.1.2.6 Number.MAX_VALUE: largest finite IEEE 754 double.
    #("MAX_VALUE", value.data(JsNumber(Finite(1.7976931348623157e308)))),
    // §21.1.2.9 Number.MIN_VALUE: smallest positive denormal double.
    #("MIN_VALUE", value.data(JsNumber(Finite(5.0e-324)))),
  ]

  // Global utility functions (separate refs — these are standalone globals)
  let #(h, parse_int_ref) =
    common.alloc_native_fn(
      h,
      function_proto,
      NumberNative(GlobalParseInt),
      "parseInt",
      2,
    )
  let #(h, parse_float_ref) =
    common.alloc_native_fn(
      h,
      function_proto,
      NumberNative(GlobalParseFloat),
      "parseFloat",
      1,
    )
  let #(h, is_nan_ref) =
    common.alloc_native_fn(
      h,
      function_proto,
      NumberNative(GlobalIsNaN),
      "isNaN",
      1,
    )
  let #(h, is_finite_ref) =
    common.alloc_native_fn(
      h,
      function_proto,
      NumberNative(GlobalIsFinite),
      "isFinite",
      1,
    )

  // Number.prototype methods
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("valueOf", NumberNative(NumberPrototypeValueOf), 0),
      #("toString", NumberNative(NumberPrototypeToString), 1),
      #("toFixed", NumberNative(NumberPrototypeToFixed), 1),
      #("toPrecision", NumberNative(NumberPrototypeToPrecision), 1),
      #("toExponential", NumberNative(NumberPrototypeToExponential), 1),
    ])

  let ctor_props = list.append(constants, static_methods)
  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(_) { Dispatch(NumberNative(NumberConstructor)) },
      "Number",
      1,
      ctor_props,
    )

  // ES2024 §21.1.3: The Number prototype object has a [[NumberData]] internal
  // slot with value +0. Update from OrdinaryObject to NumberObject.
  let h = heap.update_kind(h, bt.prototype, NumberObject(value: Finite(0.0)))

  #(h, bt, parse_int_ref, parse_float_ref, is_nan_ref, is_finite_ref)
}

/// Per-module dispatch for Number native functions.
pub fn dispatch(
  native: NumberNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    NumberConstructor -> call_as_function(args, state)
    NumberIsNaN -> number_is_nan(args, state)
    NumberIsFinite -> number_is_finite(args, state)
    NumberIsInteger -> number_is_integer(args, state)
    NumberParseInt -> parse_int(args, state)
    NumberParseFloat -> parse_float(args, state)
    NumberPrototypeValueOf -> number_value_of(this, args, state)
    NumberPrototypeToString -> number_to_string(this, args, state)
    GlobalParseInt -> parse_int(args, state)
    GlobalParseFloat -> parse_float(args, state)
    GlobalIsNaN -> js_is_nan(args, state)
    GlobalIsFinite -> js_is_finite(args, state)
    NumberIsSafeInteger -> number_is_safe_integer(args, state)
    NumberPrototypeToFixed -> number_to_fixed(this, args, state)
    NumberPrototypeToPrecision -> number_to_precision(this, args, state)
    NumberPrototypeToExponential -> number_to_exponential(this, args, state)
  }
}

/// Number(value) — ES2024 §21.1.1.1
///
/// When called as a function (not as a constructor):
///   1. If value is present, then
///      a. Let prim be ? ToNumeric(value).
///      b. If prim is a BigInt, let n be 𝔽(ℝ(prim)).
///      c. Otherwise, let n be prim.
///   2. Else, let n be +0.
///   3. (If called as constructor, would create wrapper — not handled here.)
///   4. Return n.
///
/// Note: Constructor semantics (new Number(value)) are handled separately
/// in exec/call.gleam's do_construct, which wraps the result in a NumberObject.
fn call_as_function(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: If no arguments, n = +0
  // Step 2: Else, n = ToNumeric(value): ToPrimitive first, and a BigInt
  // primitive becomes 𝔽(ℝ(prim)) (NumberConstructor step 1.a) — unlike
  // ToNumber, which throws for BigInt.
  case args {
    [] -> #(state, Ok(JsNumber(Finite(0.0))))
    [val, ..] ->
      case coerce.to_primitive(state, val, coerce.NumberHint) {
        Error(#(thrown, state)) -> #(state, Error(thrown))
        Ok(#(value.JsBigInt(value.BigInt(n)), state)) -> #(
          state,
          Ok(JsNumber(bigint_to_float(n))),
        )
        Ok(#(prim, state)) ->
          case coerce.js_to_number(state, prim) {
            Ok(#(n, state)) -> #(state, Ok(JsNumber(n)))
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
      }
  }
}

/// 𝔽(ℝ(bigint)) — correctly rounded, with ±Infinity beyond the double range.
fn bigint_to_float(n: Int) -> JsNum {
  value.num_from_int(n)
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
fn parse_int(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: Let inputString be ? ToString(string).
  // Step 2: Let S be TrimString(inputString, START).
  let str_result = case args {
    [val, ..] -> {
      use #(s, state) <- result.map(coerce.js_to_string(state, val))
      // Step 2: TrimString(inputString, START) — leading whitespace only
      #(string.trim_start(s), state)
    }
    [] -> Ok(#("", state))
  }
  use str, state <- state.try_op(str_result)
  // Steps 6-9: Determine radix R via ToInt32(radix).
  // Step 6 is ToInt32 (§7.1.6), NOT ToIntegerOrInfinity: ToInt32 maps NaN
  // and ±∞ to +0, and step 8 then turns R = 0 into the default radix 10.
  // The full ToNumber runs first (ToPrimitive on objects, honoring an
  // overridden valueOf), so this site keeps its own non-finite → 0 mapping
  // rather than the saturated coerce.jsnum_to_integer_or_infinity.
  let radix_val = case args {
    [_, r, ..] -> r
    _ -> JsUndefined
  }
  use radix_num, state <- coerce.try_to_number(state, radix_val)
  let radix_int = case radix_num {
    Finite(x) -> value.float_to_int(x)
    NaN | Infinity | NegInfinity -> 0
  }
  // Steps 3-5: Strip the sign BEFORE the prefix check so "-0x10" still
  // reaches the "0x" path (the prefix follows the sign in the grammar).
  let #(str, negative) = case string.first(str) {
    Ok("-") -> #(string.drop_start(str, 1), True)
    Ok("+") -> #(string.drop_start(str, 1), False)
    _ -> #(str, False)
  }
  // Steps 7-9: R = 0 means "radix was unspecified" (undefined, NaN, ±∞ all
  // reach here as 0), which defaults to 10 WITH prefix detection. An
  // explicit 16 also keeps prefix detection; any other explicit radix must
  // never have "0x" stripped (step 8.b), even radix 10.
  let #(radix, strip_prefix) = case radix_int {
    0 -> #(10, True)
    16 -> #(16, True)
    n -> #(n, False)
  }
  // Step 10: A "0x"/"0X" prefix (only when stripPrefix) forces radix 16.
  let has_hex_prefix =
    string.starts_with(str, "0x") || string.starts_with(str, "0X")
  let #(str, radix) = case strip_prefix && has_hex_prefix {
    True -> #(string.drop_start(str, 2), 16)
    False -> #(str, radix)
  }
  // Step 8a: If R < 2 or R > 36, return NaN.
  case radix >= 2 && radix <= 36 {
    False -> #(state, Ok(JsNumber(NaN)))
    True -> {
      // Steps 11-16: Parse the longest digit prefix and apply the sign.
      let result = parse_int_digits(str, radix, negative)
      #(state, Ok(JsNumber(result)))
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
fn parse_float(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: Let inputString be ? ToString(string).
  // Step 2: Let trimmedString be TrimString(inputString, START).
  let str_result = case args {
    [val, ..] -> {
      use #(s, state) <- result.map(coerce.js_to_string(state, val))
      // Step 2: TrimString(inputString, START) — leading whitespace only
      #(string.trim_start(s), state)
    }
    [] -> Ok(#("", state))
  }
  use str, state <- state.try_op(str_result)
  // Steps 3-6: Take the longest StrDecimalLiteral prefix and convert it.
  #(state, Ok(JsNumber(parse_decimal_string(str))))
}

/// parseFloat steps 3-6: find the longest prefix of the (already trimmed)
/// string that satisfies StrDecimalLiteral; NaN when no prefix does.
///
/// The matched slice is a subset of the StringNumericLiteral grammar (it can
/// never be empty, whitespace, or a "0x"/"0o"/"0b" form), so its numeric
/// value is exactly value.string_to_number's — one shared string→float path
/// that already saturates beyond-double-range digit strings (e.g. 400 nines)
/// to ±Infinity via value.num_from_int instead of crashing erlang:float/1.
fn parse_decimal_string(str: String) -> JsNum {
  case scan_decimal_literal(str) {
    0 -> NaN
    len -> value.string_to_number(string.slice(str, 0, len))
  }
}

/// Longest-prefix StrDecimalLiteral scanner (§19.2.4 steps 3-4).
///
/// Grammar (§7.1.4.1 StrDecimalLiteral, sign made explicit):
///   [+|-] ( Infinity
///         | DecimalDigits [. DecimalDigits_opt] ExponentPart_opt
///         | . DecimalDigits ExponentPart_opt )
///
/// Returns how many leading graphemes of `s` form the longest valid
/// literal, or 0 when no prefix matches. The walk stops at the first
/// grapheme that cannot extend the literal, so e.g. "1.2.3" → 3 ("1.2")
/// and "1foo" → 1 ("1").
fn scan_decimal_literal(s: String) -> Int {
  let graphemes = string.to_graphemes(s)
  let #(sign_len, rest) = case graphemes {
    ["+", ..r] | ["-", ..r] -> #(1, r)
    _ -> #(0, graphemes)
  }
  case rest {
    // "Infinity" — any trailing garbage is simply not part of the match.
    ["I", "n", "f", "i", "n", "i", "t", "y", ..] -> sign_len + 8
    _ ->
      case scan_unsigned_decimal(rest) {
        // A bare sign with no literal after it matches nothing.
        0 -> 0
        n -> sign_len + n
      }
  }
}

/// Length of the longest StrUnsignedDecimalLiteral (minus Infinity, handled
/// by the caller) at the head of `gs`, or 0 when none matches. A mantissa
/// needs at least one digit on either side of the (optional) dot; the
/// exponent only counts when it is complete (`e`/`E`, optional sign, 1+
/// digits) — "5e" matches just "5".
fn scan_unsigned_decimal(gs: List(String)) -> Int {
  let #(icount, after_int) = scan_digit_run(gs, 0)
  let #(mantissa_len, after_mantissa) = case after_int {
    [".", ..after_dot] -> {
      let #(fcount, after_frac) = scan_digit_run(after_dot, 0)
      case icount + fcount > 0 {
        True -> #(icount + 1 + fcount, after_frac)
        // "." alone (or "+.") is not a mantissa.
        False -> #(0, after_frac)
      }
    }
    _ -> #(icount, after_int)
  }
  case mantissa_len {
    0 -> 0
    _ -> mantissa_len + scan_exponent_length(after_mantissa)
  }
}

/// Consume a run of ASCII decimal digits; returns #(count, remainder).
fn scan_digit_run(gs: List(String), count: Int) -> #(Int, List(String)) {
  case gs {
    ["0", ..rest]
    | ["1", ..rest]
    | ["2", ..rest]
    | ["3", ..rest]
    | ["4", ..rest]
    | ["5", ..rest]
    | ["6", ..rest]
    | ["7", ..rest]
    | ["8", ..rest]
    | ["9", ..rest] -> scan_digit_run(rest, count + 1)
    _ -> #(count, gs)
  }
}

/// Length of a complete ExponentPart (`e`/`E`, optional sign, 1+ digits) at
/// the head of `gs`, or 0 when it is absent or incomplete ("e", "e+").
fn scan_exponent_length(gs: List(String)) -> Int {
  case gs {
    ["e", ..rest] | ["E", ..rest] -> {
      let #(sign_len, digits) = case rest {
        ["+", ..r] | ["-", ..r] -> #(1, r)
        _ -> #(0, rest)
      }
      case scan_digit_run(digits, 0) {
        #(0, _) -> 0
        #(dcount, _) -> 1 + sign_len + dcount
      }
    }
    _ -> 0
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
fn js_is_nan(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let val = helpers.first_arg_or_undefined(args)
  // Step 1: Let num be ? ToNumber(number) — ToPrimitive re-entry for
  // objects, TypeError for Symbol/BigInt.
  use num, state <- coerce.try_to_number(state, val)
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
fn js_is_finite(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let val = helpers.first_arg_or_undefined(args)
  // Step 1: Let num be ? ToNumber(number) — ToPrimitive re-entry for
  // objects, TypeError for Symbol/BigInt.
  use num, state <- coerce.try_to_number(state, val)
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
fn number_is_nan(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
fn number_is_finite(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
fn number_is_integer(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
fn number_value_of(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: Return ? thisNumberValue(this value).
  use n, state <- require_number(this, state, "valueOf")
  #(state, Ok(JsNumber(n)))
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
fn number_to_string(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: Let x be ? thisNumberValue(this value).
  use n, state <- require_number(this, state, "toString")
  // Steps 2-3: radix defaults to 10; undefined -> 10.
  let radix_arg = case args {
    [] | [JsUndefined, ..] -> JsNumber(Finite(10.0))
    [r, ..] -> r
  }
  // Step 3: radixMV = ToIntegerOrInfinity(radix). ±∞ saturate to
  // ±(2^53 - 1) and NaN → 0, so step 4's range check sees them.
  use radix, state <- coerce.try_to_integer_or_infinity(state, radix_arg)
  // Step 4: If radixMV not in [2, 36], throw RangeError.
  case radix >= 2 && radix <= 36 {
    False ->
      state.range_error(state, "toString() radix must be between 2 and 36")
    // Step 5: Return Number::toString(x, radixMV).
    True -> #(state, Ok(JsString(value.format_number_radix(n, radix))))
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
fn this_number_value(state: State(host), this: JsValue) -> Option(JsNum) {
  case this {
    // Step 1: If value is a Number, return value.
    JsNumber(n) -> Some(n)
    // Step 2: If value is an Object with [[NumberData]], return it.
    JsObject(ref) -> heap.read_number_object(state.heap, ref)
    // Step 3: (caller throws TypeError)
    _ -> None
  }
}

/// Number.isSafeInteger(number) — ES2024 §21.1.2.5
fn number_is_safe_integer(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let result = case args {
    [JsNumber(Finite(n)), ..] -> {
      let truncated = int.to_float(float.truncate(n))
      value.JsBool(
        truncated == n
        && n >=. -9_007_199_254_740_991.0
        && n <=. 9_007_199_254_740_991.0,
      )
    }
    _ -> value.JsBool(False)
  }
  #(state, Ok(result))
}

/// Number.prototype.toFixed(fractionDigits) — ES2024 §21.1.3.3
fn number_to_fixed(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use n, state <- require_number(this, state, "toFixed")
  // Step 2: f = ToIntegerOrInfinity(fractionDigits). undefined → NaN → 0;
  // ±∞ saturate out of [0, 100] so step 3's RangeError fires.
  use f, state <- coerce.try_to_integer_or_infinity(
    state,
    helpers.first_arg_or_undefined(args),
  )
  case f < 0 || f > 100 {
    True ->
      state.range_error(
        state,
        "toFixed() digits argument must be between 0 and 100",
      )
    False -> {
      // Step 10: if |x| >= 1e21 the result is ToString(x), not fixed
      // notation (mirrors number_to_precision's undefined branch), so the
      // FFI only ever sees fixed-notation magnitudes.
      let format = fn(x) {
        case float.absolute_value(x) >=. 1.0e21 {
          True -> value.format_number_radix(Finite(x), 10)
          False -> format_to_fixed(x, f)
        }
      }
      #(state, Ok(JsString(format_non_finite(n, format))))
    }
  }
}

/// Number.prototype.toExponential(fractionDigits) — ES2024 §21.1.3.2
fn number_to_exponential(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use n, state <- require_number(this, state, "toExponential")
  case args {
    // Step 6.c: fractionDigits undefined — as many significant digits as
    // needed to uniquely represent the value ("k is as small as possible").
    [JsUndefined, ..] | [] -> #(
      state,
      Ok(JsString(format_non_finite(n, format_to_exponential_auto))),
    )
    [v, ..] -> {
      // Step 2: f = ToIntegerOrInfinity(fractionDigits); ±∞ saturate out of
      // [0, 100] so step 5's RangeError fires.
      use f, state <- coerce.try_to_integer_or_infinity(state, v)
      case f < 0 || f > 100 {
        True ->
          state.range_error(
            state,
            "toExponential() argument must be between 0 and 100",
          )
        False -> #(
          state,
          Ok(JsString(format_non_finite(n, format_to_exponential(_, f)))),
        )
      }
    }
  }
}

/// Number.prototype.toPrecision(precision) — ES2024 §21.1.3.5
fn number_to_precision(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use n, state <- require_number(this, state, "toPrecision")
  case args {
    // If precision is undefined, behave as toString.
    [JsUndefined, ..] | [] -> #(
      state,
      Ok(JsString(value.format_number_radix(n, 10))),
    )
    [v, ..] -> {
      // Step 3: p = ToIntegerOrInfinity(precision); ±∞ saturate out of
      // [1, 100] so step 5's RangeError fires.
      use p, state <- coerce.try_to_integer_or_infinity(state, v)
      case p < 1 || p > 100 {
        True ->
          state.range_error(
            state,
            "toPrecision() argument must be between 1 and 100",
          )
        False -> #(
          state,
          Ok(JsString(format_non_finite(n, format_to_precision(_, p)))),
        )
      }
    }
  }
}

// ============================================================================
// Internal helpers
// ============================================================================

/// Unwrap `this` as a Number or return a TypeError.
/// CPS-style — call with `use n, state <- require_number(this, state, "method")`.
fn require_number(
  this: JsValue,
  state: State(host),
  method: String,
  cont: fn(JsNum, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this_number_value(state, this) {
    Some(n) -> cont(n, state)
    None ->
      state.type_error(
        state,
        "Number.prototype." <> method <> " requires that 'this' be a Number",
      )
  }
}

/// Stringify NaN/Infinity canonically, else apply `f` to the finite float.
fn format_non_finite(n: JsNum, f: fn(Float) -> String) -> String {
  case n {
    NaN -> "NaN"
    Infinity -> "Infinity"
    NegInfinity -> "-Infinity"
    Finite(x) -> f(x)
  }
}

/// parseInt digit parsing — implements ES2024 §19.2.5 steps 11-16.
///
/// The caller has already trimmed, stripped the sign (steps 3-5, passed as
/// `negative`) and stripped any "0x" prefix, so `s` starts at the digits.
///
/// Steps 11-12: Parse digits until first non-radix-R character.
/// Step 13: If no valid digits found, return NaN.
/// Steps 14-16: Compute mathematical integer value with sign.
///
fn parse_int_digits(s: String, radix: Int, negative: Bool) -> value.JsNum {
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
        // num_from_int saturates beyond-double-range digit strings (e.g.
        // parseInt of 400 nines) to +/-Infinity instead of crashing.
        True -> value.num_from_int(-n)
        False -> value.num_from_int(n)
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

@external(erlang, "arc_number_ffi", "format_to_fixed")
fn format_to_fixed(x: Float, digits: Int) -> String

@external(erlang, "arc_number_ffi", "format_to_exponential")
fn format_to_exponential(x: Float, fraction_digits: Int) -> String

/// toExponential() with no fractionDigits argument: shortest-round-trip
/// digits. A separate entry point (rather than an in-band sentinel) so a
/// user-passed -1 hits the RangeError check instead of meaning "auto".
@external(erlang, "arc_number_ffi", "format_to_exponential_auto")
fn format_to_exponential_auto(x: Float) -> String

@external(erlang, "arc_number_ffi", "format_to_precision")
fn format_to_precision(x: Float, precision: Int) -> String
