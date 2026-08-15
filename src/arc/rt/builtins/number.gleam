//// `rt_builtins/number` — Number constructor + %Number.prototype% + the
//// four coercing global functions parseInt/parseFloat/isNaN/isFinite
//// (ES2024 §21.1 / §19.2). Port of `arc/vm/builtins/number.gleam` over the
//// threaded `Agent` model (D7/R1).

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsNum, type JsVal,
  type NumberNative, GlobalIsFinite, GlobalIsNaN, GlobalN, GlobalParseFloat,
  GlobalParseInt, HintNumber, JFloat, JInt, JNan, JNegInf, JPosInf, KBig,
  KHandle, KNum, KUndef, NumberConstructor, NumberIsFinite, NumberIsInteger,
  NumberIsNaN, NumberIsSafeInteger, NumberN, NumberObj,
  NumberPrototypeToExponential, NumberPrototypeToFixed,
  NumberPrototypeToLocaleString, NumberPrototypeToPrecision,
  NumberPrototypeToString, NumberPrototypeValueOf, SObject, classify, mk_bool,
  mk_number, mk_object, mk_string,
} as rt_types
import arc/rt/val as rt_val
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// What `init` returns: the Number type plus the four global function
/// objects it allocates (§21.1.2.12/.13 make parseInt/parseFloat SHARED
/// between the constructor and the global object). Named record so swapping
/// two structurally identical `Handle`s cannot typecheck.
pub type NumberBuiltins {
  NumberBuiltins(
    pair: BuiltinPair,
    parse_int: Handle,
    parse_float: Handle,
    is_nan: Handle,
    is_finite: Handle,
  )
}

/// Set up Number constructor + Number.prototype + the four globals.
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(NumberBuiltins, Agent) {
  // Global utility functions. parseInt/parseFloat are allocated FIRST because
  // §21.1.2.13/.12 require `Number.parseInt === parseInt` — the constructor
  // installs these very refs rather than allocating twins.
  let #(parse_int_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      GlobalN(GlobalParseInt),
      "parseInt",
      2,
    )
  let #(parse_float_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      GlobalN(GlobalParseFloat),
      "parseFloat",
      1,
    )
  // Number.isNaN / Number.isFinite are deliberately NOT the globals: they
  // skip ToNumber coercion (§21.1.2.2/.4).
  let #(is_nan_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      GlobalN(GlobalIsNaN),
      "isNaN",
      1,
    )
  let #(is_finite_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      GlobalN(GlobalIsFinite),
      "isFinite",
      1,
    )
  // Static methods on Number constructor.
  let #(static_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("isNaN", NumberN(NumberIsNaN), 1),
      #("isFinite", NumberN(NumberIsFinite), 1),
      #("isInteger", NumberN(NumberIsInteger), 1),
      #("isSafeInteger", NumberN(NumberIsSafeInteger), 1),
    ])
  // Same shape alloc_methods produces, but pointing at the SHARED global refs.
  let #(pi_p, st) = common.builtin_property(st, mk_object(parse_int_ref))
  let #(pf_p, st) = common.builtin_property(st, mk_object(parse_float_ref))
  let shared_globals = [#("parseInt", pi_p), #("parseFloat", pf_p)]
  // Static constants {W:F, E:F, C:F}.
  let #(constants, st) =
    data_constants(st, [
      #("NaN", JNan),
      #("POSITIVE_INFINITY", JPosInf),
      #("NEGATIVE_INFINITY", JNegInf),
      #("MAX_SAFE_INTEGER", JFloat(9_007_199_254_740_991.0)),
      #("MIN_SAFE_INTEGER", JFloat(-9_007_199_254_740_991.0)),
      #("EPSILON", JFloat(2.220446049250313e-16)),
      #("MAX_VALUE", JFloat(1.7976931348623157e308)),
      #("MIN_VALUE", JFloat(5.0e-324)),
    ])
  // Number.prototype methods.
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("valueOf", NumberN(NumberPrototypeValueOf), 0),
      #("toString", NumberN(NumberPrototypeToString), 1),
      #("toFixed", NumberN(NumberPrototypeToFixed), 1),
      #("toPrecision", NumberN(NumberPrototypeToPrecision), 1),
      #("toExponential", NumberN(NumberPrototypeToExponential), 1),
      #("toLocaleString", NumberN(NumberPrototypeToLocaleString), 0),
    ])
  let ctor_props =
    list.append(constants, list.append(static_methods, shared_globals))
  // §21.1.3: the Number prototype object is itself a Number object with
  // [[NumberData]] = +0.
  let #(bt, st) =
    common.init_wrapper_type(
      st,
      object_proto,
      fn_proto,
      proto_methods,
      fn(_) { NumberN(NumberConstructor) },
      "Number",
      1,
      ctor_props,
      proto_kind: NumberObj(value: JInt(0)),
    )
  #(
    NumberBuiltins(
      pair: bt,
      parse_int: parse_int_ref,
      parse_float: parse_float_ref,
      is_nan: is_nan_ref,
      is_finite: is_finite_ref,
    ),
    st,
  )
}

fn data_constants(
  st: Agent,
  specs: List(#(String, JsNum)),
) -> #(List(#(String, rt_types.Property)), Agent) {
  case specs {
    [] -> #([], st)
    [#(name, n), ..rest] -> {
      let #(prop, st) = common.data_prop(st, mk_number(n))
      let #(tail, st) = data_constants(st, rest)
      #([#(name, prop), ..tail], st)
    }
  }
}

/// Per-module dispatch for Number native functions.
pub fn dispatch(
  st: Agent,
  native: NumberNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    NumberConstructor -> call_as_function(st, args)
    NumberIsNaN -> number_is_nan(st, args)
    NumberIsFinite -> number_is_finite(st, args)
    NumberIsInteger -> number_is_integer(st, args)
    NumberIsSafeInteger -> number_is_safe_integer(st, args)
    NumberPrototypeValueOf -> number_value_of(st, this)
    NumberPrototypeToString -> number_to_string(st, this, args)
    NumberPrototypeToFixed -> number_to_fixed(st, this, args)
    NumberPrototypeToPrecision -> number_to_precision(st, this, args)
    NumberPrototypeToExponential -> number_to_exponential(st, this, args)
    NumberPrototypeToLocaleString -> number_to_locale_string(st, this)
  }
}

/// §21.1.3.4 Number.prototype.toLocaleString — no-Intl fallback: same as
/// toString(10). Arguments (locales/options) ignored.
fn number_to_locale_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let n = this_number_value(st, this, "toLocaleString")
  #(mk_string(rt_val.format_jsnum(n)), st)
}

/// §21.1.1.1 Number(value) called as a function. `new Number` is intercepted
/// in `t_construct`. Step 1.a: n = ToNumeric(value); BigInt → 𝔽(ℝ(prim)).
fn call_as_function(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case args {
    [] -> #(mk_number(JInt(0)), st)
    [val, ..] -> {
      let #(prim, st) = rt_val.t_to_primitive(st, val, HintNumber)
      case classify(prim) {
        KBig(n) -> #(mk_number(rt_val.num_from_int(n)), st)
        _ -> {
          let #(n, st) = rt_val.t_to_number(st, prim)
          #(mk_number(n), st)
        }
      }
    }
  }
}

// ── §19.2 global functions ──────────────────────────────────────────────────

/// §19.2.5 parseInt(string, radix). Returns the JsNum so callers rendering it
/// (Console %i) cannot mistake a non-Number for a parse result.
pub fn parse_int_value(
  st: Agent,
  val: JsVal,
  radix_val: JsVal,
) -> #(JsNum, Agent) {
  // Steps 1-2: inputString = ? ToString(string); TrimString(_, START).
  let #(s, st) = rt_val.t_to_string(st, val)
  let str = trim_leading_js_ws(s)
  // Step 6: R = ToInt32(radix), NOT ToIntegerOrInfinity — NaN/±∞ → +0 (then
  // step 8 turns 0 into default radix 10) and wraps modulo 2^32.
  let #(radix_num, st) = rt_val.t_to_number(st, radix_val)
  let radix_int = rt_val.num_to_int32(radix_num)
  // Steps 3-5: strip sign BEFORE prefix check so "-0x10" reaches "0x".
  let #(str, negative) = case string.first(str) {
    Ok("-") -> #(string.drop_start(str, 1), True)
    Ok("+") -> #(string.drop_start(str, 1), False)
    _ -> #(str, False)
  }
  // Steps 7-9: R = 0 → default 10 WITH prefix detection; explicit 16 keeps
  // prefix detection; any other explicit radix never strips "0x".
  let #(radix, strip_prefix) = case radix_int {
    0 -> #(10, True)
    16 -> #(16, True)
    n -> #(n, False)
  }
  let has_hex_prefix =
    string.starts_with(str, "0x") || string.starts_with(str, "0X")
  let #(str, radix) = case strip_prefix && has_hex_prefix {
    True -> #(string.drop_start(str, 2), 16)
    False -> #(str, radix)
  }
  // Step 8a: R < 2 or R > 36 → NaN. Steps 11-16: parse digit prefix.
  case radix >= 2 && radix <= 36 {
    False -> #(JNan, st)
    True -> #(parse_int_digits(str, radix, negative), st)
  }
}

/// §19.2.4 parseFloat(string).
pub fn parse_float_value(st: Agent, val: JsVal) -> #(JsNum, Agent) {
  let #(s, st) = rt_val.t_to_string(st, val)
  #(parse_decimal_string(trim_leading_js_ws(s)), st)
}

/// §19.2.3 isNaN(number) — coerces via ToNumber first.
pub fn js_is_nan(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(num, st) = rt_val.t_to_number(st, helpers.first_arg_or_undefined(args))
  case num {
    JNan -> #(mk_bool(True), st)
    _ -> #(mk_bool(False), st)
  }
}

/// §19.2.2 isFinite(number) — coerces via ToNumber first.
pub fn js_is_finite(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(num, st) = rt_val.t_to_number(st, helpers.first_arg_or_undefined(args))
  case num {
    JInt(_) | JFloat(_) -> #(mk_bool(True), st)
    _ -> #(mk_bool(False), st)
  }
}

// ── §21.1.2 Number static methods ───────────────────────────────────────────

/// §21.1.2.4 Number.isNaN — no coercion.
fn number_is_nan(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KNum(JNan) -> #(mk_bool(True), st)
    _ -> #(mk_bool(False), st)
  }
}

/// §21.1.2.1 Number.isFinite — no coercion.
fn number_is_finite(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KNum(JInt(_)) | KNum(JFloat(_)) -> #(mk_bool(True), st)
    _ -> #(mk_bool(False), st)
  }
}

/// §21.1.2.3 Number.isInteger.
fn number_is_integer(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KNum(JInt(_)) -> #(mk_bool(True), st)
    KNum(JFloat(f)) -> #(mk_bool(option.is_some(rt_val.integral_int(f))), st)
    _ -> #(mk_bool(False), st)
  }
}

/// §21.1.2.5 Number.isSafeInteger.
fn number_is_safe_integer(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let is_safe = fn(i: Int) {
    i >= -9_007_199_254_740_991 && i <= 9_007_199_254_740_991
  }
  case classify(helpers.first_arg_or_undefined(args)) {
    KNum(JInt(i)) -> #(mk_bool(is_safe(i)), st)
    KNum(JFloat(f)) ->
      case rt_val.integral_int(f) {
        Some(i) -> #(mk_bool(is_safe(i)), st)
        None -> #(mk_bool(False), st)
      }
    _ -> #(mk_bool(False), st)
  }
}

// ── §21.1.3 Number.prototype methods ────────────────────────────────────────

/// §21.1.3.7 Number.prototype.valueOf.
fn number_value_of(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  #(mk_number(this_number_value(st, this, "valueOf")), st)
}

/// §21.1.3.6 Number.prototype.toString([radix]).
fn number_to_string(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let n = this_number_value(st, this, "toString")
  // Steps 2-3: radix defaults to 10; ToIntegerOrInfinity otherwise.
  let #(radix, st) = case args {
    [] -> #(10, st)
    [r, ..] ->
      case classify(r) {
        KUndef -> #(10, st)
        _ -> rt_val.t_to_integer_or_infinity(st, r)
      }
  }
  case radix >= 2 && radix <= 36 {
    False ->
      rt_val.t_throw_range_error(
        st,
        "toString() radix must be between 2 and 36",
      )
    True -> #(mk_string(format_number_radix(n, radix)), st)
  }
}

/// §21.1.3.3 Number.prototype.toFixed(fractionDigits).
fn number_to_fixed(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let n = this_number_value(st, this, "toFixed")
  let #(f, st) =
    rt_val.t_to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  case f < 0 || f > 100 {
    True ->
      rt_val.t_throw_range_error(
        st,
        "toFixed() digits argument must be between 0 and 100",
      )
    False -> {
      // Step 10: |x| >= 1e21 → ToString(x), not fixed notation.
      let format = fn(x) {
        case float.absolute_value(x) >=. 1.0e21 {
          True -> rt_val.js_format_float(x)
          False -> format_to_fixed(x, f)
        }
      }
      #(mk_string(format_non_finite(n, format)), st)
    }
  }
}

/// §21.1.3.2 Number.prototype.toExponential(fractionDigits).
fn number_to_exponential(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let n = this_number_value(st, this, "toExponential")
  let arg = helpers.first_arg_or_undefined(args)
  case classify(arg) {
    // Step 6.c: undefined → shortest round-trip digits.
    KUndef -> #(mk_string(format_non_finite(n, format_to_exponential_auto)), st)
    _ -> {
      let #(f, st) = rt_val.t_to_integer_or_infinity(st, arg)
      // Step 4 BEFORE step 5's range check: non-finite `this` returns
      // Number::toString(x, 10) even when fractionDigits is out of range.
      case n {
        JInt(_) | JFloat(_) ->
          case f < 0 || f > 100 {
            True ->
              rt_val.t_throw_range_error(
                st,
                "toExponential() argument must be between 0 and 100",
              )
            False -> #(
              mk_string(format_non_finite(n, format_to_exponential(_, f))),
              st,
            )
          }
        JNan | JPosInf | JNegInf -> #(mk_string(rt_val.format_jsnum(n)), st)
      }
    }
  }
}

/// §21.1.3.5 Number.prototype.toPrecision(precision).
fn number_to_precision(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let n = this_number_value(st, this, "toPrecision")
  let arg = helpers.first_arg_or_undefined(args)
  case classify(arg) {
    // precision undefined → behave as toString.
    KUndef -> #(mk_string(rt_val.format_jsnum(n)), st)
    _ -> {
      let #(p, st) = rt_val.t_to_integer_or_infinity(st, arg)
      // Step 4 BEFORE step 5's range check.
      case n {
        JInt(_) | JFloat(_) ->
          case p < 1 || p > 100 {
            True ->
              rt_val.t_throw_range_error(
                st,
                "toPrecision() argument must be between 1 and 100",
              )
            False -> #(
              mk_string(format_non_finite(n, format_to_precision(_, p))),
              st,
            )
          }
        JNan | JPosInf | JNegInf -> #(mk_string(rt_val.format_jsnum(n)), st)
      }
    }
  }
}

/// §21.1.3 thisNumberValue(value): Number primitive or [[NumberData]] slot.
fn this_number_value(st: Agent, this: JsVal, method: String) -> JsNum {
  case classify(this) {
    KNum(n) -> n
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: NumberObj(value: n), ..) -> n
        _ -> not_a_number(st, method)
      }
    _ -> not_a_number(st, method)
  }
}

fn not_a_number(st: Agent, method: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "Number.prototype." <> method <> " requires that 'this' be a Number",
  )
}

// ── internal helpers ────────────────────────────────────────────────────────

/// Number::toString(x, radix). Radix 10 → the canonical formatter; NaN/±∞
/// use their canonical string forms regardless of radix.
fn format_number_radix(n: JsNum, base: Int) -> String {
  case n, base {
    _, 10 -> rt_val.format_jsnum(n)
    JNan, _ -> "NaN"
    JPosInf, _ -> "Infinity"
    JNegInf, _ -> "-Infinity"
    JInt(i), _ -> {
      let assert Ok(s) = int.to_base_string(i, base)
      string.lowercase(s)
    }
    JFloat(f), _ -> format_float_radix(f, base)
  }
}

/// Stringify NaN/±∞ canonically, else apply `f` to the finite float.
fn format_non_finite(n: JsNum, f: fn(Float) -> String) -> String {
  case n {
    JNan -> "NaN"
    JPosInf -> "Infinity"
    JNegInf -> "-Infinity"
    JInt(i) -> f(int.to_float(i))
    JFloat(x) -> f(x)
  }
}

/// parseFloat steps 3-6: longest StrDecimalLiteral prefix; NaN when none.
fn parse_decimal_string(str: String) -> JsNum {
  // Code points, NOT graphemes: every StrDecimalLiteral token is ASCII, so a
  // combining mark must terminate the literal AFTER the digit.
  let chars = to_codepoint_chars(str)
  case scan_decimal_literal(chars) {
    0 -> JNan
    len -> rt_val.string_to_number(string.concat(list.take(chars, len)))
  }
}

/// Split into single-code-point strings (combining marks stay separate).
fn to_codepoint_chars(s: String) -> List(String) {
  use cp <- list.map(string.to_utf_codepoints(s))
  string.from_utf_codepoints([cp])
}

/// Longest-prefix StrDecimalLiteral scanner (§19.2.4 steps 3-4).
fn scan_decimal_literal(chars: List(String)) -> Int {
  let #(sign_len, rest) = case chars {
    ["+", ..r] | ["-", ..r] -> #(1, r)
    _ -> #(0, chars)
  }
  case rest {
    ["I", "n", "f", "i", "n", "i", "t", "y", ..] -> sign_len + 8
    _ ->
      case scan_unsigned_decimal(rest) {
        0 -> 0
        n -> sign_len + n
      }
  }
}

/// Length of the longest StrUnsignedDecimalLiteral (minus Infinity) prefix.
fn scan_unsigned_decimal(gs: List(String)) -> Int {
  let #(icount, after_int) = scan_digit_run(gs, 0)
  let #(mantissa_len, after_mantissa) = case after_int {
    [".", ..after_dot] -> {
      let #(fcount, after_frac) = scan_digit_run(after_dot, 0)
      case icount + fcount > 0 {
        True -> #(icount + 1 + fcount, after_frac)
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

fn scan_digit_run(gs: List(String), count: Int) -> #(Int, List(String)) {
  case gs {
    [ch, ..rest] ->
      case digit_value(ch) {
        Some(_) -> scan_digit_run(rest, count + 1)
        None -> #(count, gs)
      }
    [] -> #(count, gs)
  }
}

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

/// parseInt steps 11-16: parse the longest digit prefix and apply sign.
fn parse_int_digits(s: String, radix: Int, negative: Bool) -> JsNum {
  case parse_digits_loop(to_codepoint_chars(s), radix, 0, False) {
    None -> JNan
    Some(n) ->
      case negative {
        // Step 15: sign = -1 and mathInt = 0 → -0.
        True if n == 0 -> JFloat(-0.0)
        True -> rt_val.num_from_int(-n)
        False -> rt_val.num_from_int(n)
      }
  }
}

fn parse_digits_loop(
  chars: List(String),
  radix: Int,
  acc: Int,
  found_any: Bool,
) -> Option(Int) {
  case chars {
    [] ->
      case found_any {
        True -> Some(acc)
        False -> None
      }
    [ch, ..rest] ->
      case alnum_value(ch) {
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

/// Value of an ASCII decimal digit (0-9).
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
    _ -> None
  }
}

/// Value of an ASCII alphanumeric (0-9, a-z, A-Z) as a base-36 digit.
fn alnum_value(ch: String) -> Option(Int) {
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

// ── FFI (port arc_number_ffi.erl) ───────────────────────────────────────────

@external(erlang, "arc_rt_string_ffi", "trim_leading_js_ws")
fn trim_leading_js_ws(s: String) -> String

@external(erlang, "arc_rt_number_ffi", "format_to_fixed")
fn format_to_fixed(x: Float, digits: Int) -> String

@external(erlang, "arc_rt_number_ffi", "format_to_exponential")
fn format_to_exponential(x: Float, fraction_digits: Int) -> String

@external(erlang, "arc_rt_number_ffi", "format_to_exponential_auto")
fn format_to_exponential_auto(x: Float) -> String

@external(erlang, "arc_rt_number_ffi", "format_to_precision")
fn format_to_precision(x: Float, precision: Int) -> String

@external(erlang, "arc_rt_number_ffi", "format_float_radix")
fn format_float_radix(x: Float, base: Int) -> String
