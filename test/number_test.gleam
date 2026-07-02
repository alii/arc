/// Numeric-literal and string-escape cooking tests.
import arc/engine.{Returned}
import arc/parser/number
import arc/vm/value.{Finite, Infinity, JsNumber, JsString, NaN, NegInfinity}

/// Largest finite IEEE double — the value an overflowing literal currently
/// clamps to. This is a KNOWN SPEC DEVIATION, not the correct value: see
/// number_exponent_overflow_clamps_test.
const max_double = 1.7976931348623157e308

/// Helper: eval source on a fresh engine, assert normal completion, return
/// the completion value.
fn eval(source: String) -> value.JsValue {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  value
}

// ----------------------------------------------------------------------------
// parse_js_number — dot/exponent normalization
// ----------------------------------------------------------------------------

pub fn number_dot_before_exponent_test() {
  assert number.parse_js_number("1.e3") == 1000.0
  assert number.parse_js_number("2.E5") == 200_000.0
  assert number.parse_js_number("1_0.e2") == 1000.0
}

pub fn number_bare_dot_forms_test() {
  assert number.parse_js_number("1.") == 1.0
  assert number.parse_js_number(".5") == 0.5
  assert number.parse_js_number(".5e2") == 50.0
  assert number.parse_js_number("1.5e2") == 150.0
}

// ----------------------------------------------------------------------------
// parse_js_number — out-of-range exponents
// ----------------------------------------------------------------------------

pub fn number_exponent_overflow_clamps_test() {
  // KNOWN SPEC DEVIATION — this pins the current WRONG value, it is not the
  // fixed behaviour. Per ES2024 the value of `1e400` is +Infinity, and the VM
  // can express it (value.JsNum has Infinity; value.num_from_int already
  // returns it for the same overflow). We clamp only because
  // ast.NumberLiteral stores a plain Float, which on BEAM has no infinity.
  // The clamp is still strictly better than the prior behaviour (0.0), and
  // this test exists so a regression back to 0.0 is caught. Once
  // NumberLiteral carries a value.JsNum through emit's push_const, these
  // must become Infinity.
  assert number.parse_js_number("1e400") == max_double
  assert number.parse_js_number("1.5e400") == max_double
}

pub fn number_exponent_underflow_is_zero_test() {
  assert number.parse_js_number("1e-400") == 0.0
}

// ----------------------------------------------------------------------------
// parse_js_number — radix prefixes and separators
// ----------------------------------------------------------------------------

pub fn number_radix_prefix_test() {
  assert number.parse_js_number("0xFF") == 255.0
  assert number.parse_js_number("0XF_F") == 255.0
  assert number.parse_js_number("0o17") == 15.0
  assert number.parse_js_number("0b1_01") == 5.0
  assert number.parse_js_number("0") == 0.0
  assert number.parse_js_number("1_000") == 1000.0
}

pub fn bigint_radix_prefix_test() {
  assert number.parse_js_bigint("0xffn") == 255
  assert number.parse_js_bigint("0B101n") == 5
  assert number.parse_js_bigint("1_000n") == 1000
}

// ----------------------------------------------------------------------------
// End to end: lexer -> parser -> VM
// ----------------------------------------------------------------------------

pub fn eval_dot_exponent_literal_test() {
  assert eval("1.e3") == JsNumber(Finite(1000.0))
  assert eval("2.E5") == JsNumber(Finite(200_000.0))
}

pub fn eval_overflowing_literal_test() {
  // KNOWN SPEC DEVIATION — pins the current WRONG value so the pre-existing
  // bug (`1e400` cooking to 0) cannot come back; it is NOT the fixed
  // behaviour. Per spec this must be JsNumber(Infinity): today
  // `1e400 === Number.MAX_VALUE` is true, `1/1e400` is nonzero, and
  // `(1e400).toString()` is not "Infinity". Blocked on ast.NumberLiteral
  // carrying a value.JsNum instead of a bare Float (see
  // number.overflow_clamp).
  assert eval("1e400") == JsNumber(Finite(max_double))
}

// ----------------------------------------------------------------------------
// ToNumber / parseInt / parseFloat on beyond-double-range digit strings
//
// §7.1.4.1 / §19.2.5: these must yield Infinity. These parse sites used to
// call int.to_float on a >1.8e308 bignum, which is an uncaught erlang badarg
// (erlang:float/1) that killed the whole VM; they now route the Int→JsNum
// conversion through value.num_from_int, which saturates to ±Infinity.
// ----------------------------------------------------------------------------

pub fn eval_number_of_huge_decimal_string_is_infinity_test() {
  assert eval("Number('1' + '0'.repeat(400))") == JsNumber(Infinity)
  assert eval("Number('-' + '9'.repeat(400))") == JsNumber(NegInfinity)
}

pub fn eval_number_of_huge_radix_string_is_infinity_test() {
  assert eval("Number('0x' + 'f'.repeat(300))") == JsNumber(Infinity)
  assert eval("Number('0b' + '1'.repeat(2000))") == JsNumber(Infinity)
  assert eval("Number('0o' + '7'.repeat(500))") == JsNumber(Infinity)
}

pub fn eval_parse_int_huge_digit_string_is_infinity_test() {
  assert eval("parseInt('9'.repeat(400))") == JsNumber(Infinity)
  assert eval("parseInt('-' + '9'.repeat(400))") == JsNumber(NegInfinity)
  assert eval("parseInt('f'.repeat(300), 16)") == JsNumber(Infinity)
}

pub fn eval_parse_float_huge_digit_string_is_infinity_test() {
  assert eval("parseFloat('9'.repeat(400))") == JsNumber(Infinity)
  assert eval("parseFloat('-' + '9'.repeat(400))") == JsNumber(NegInfinity)
}

// Same bug class in JSON.parse: a dot-less digit string is rejected by the
// float parser, so it fell into `int.parse |> int.to_float` on a bignum.
// Per §25.5.1 the mathematical value rounds to +Infinity.
pub fn eval_json_parse_huge_integer_is_infinity_test() {
  assert eval("JSON.parse('1' + '0'.repeat(400))") == JsNumber(Infinity)
  assert eval("JSON.parse('-' + '9'.repeat(400))") == JsNumber(NegInfinity)
}

// Same bug class in Intl.DurationFormat's ISO-8601 duration-string parser: an
// unbounded digit run hit the bare int.to_float fallback. A beyond-range
// field value is not a valid duration, so it must throw a RangeError — not
// kill the VM.
pub fn eval_intl_duration_format_huge_component_throws_range_error_test() {
  let src =
    "try { new Intl.DurationFormat('en').format('PT' + '9'.repeat(400) + 'S') }"
    <> " catch (e) { e.name }"
  assert eval(src) == JsString("RangeError")
}

// Same bug class in Intl.NumberFormat's BigInt arm (ToIntlMathematicalValue,
// approximated with ToNumber). KNOWN SPEC DEVIATION: real engines format the
// exact mathematical value of the BigInt; we saturate a beyond-double BigInt
// to Infinity (like the Number(bigint) coercion does). That loses precision
// but must not crash the VM the way the previous bare int.to_float did.
pub fn eval_intl_number_format_huge_bigint_test() {
  let src =
    "var nf = new Intl.NumberFormat('en');"
    <> " nf.format(10n ** 400n) === nf.format(Infinity)"
  assert eval(src) == value.JsBool(True)
}

// ----------------------------------------------------------------------------
// parseInt — §19.2.5 sign / radix / "0x" prefix ordering
// ----------------------------------------------------------------------------

pub fn eval_parse_int_sign_before_hex_prefix_test() {
  // Steps 3-5 strip the sign BEFORE step 10's "0x" prefix check.
  assert eval("parseInt('-0x10')") == JsNumber(Finite(-16.0))
  assert eval("parseInt(' +0x10')") == JsNumber(Finite(16.0))
}

pub fn eval_parse_int_explicit_radix_and_hex_prefix_test() {
  // Step 8.b: only an unspecified radix or an explicit 16 strips "0x".
  assert eval("parseInt('0x10', 8)") == JsNumber(Finite(0.0))
  assert eval("parseInt('0x10', 10)") == JsNumber(Finite(0.0))
  assert eval("parseInt('0x10', 16)") == JsNumber(Finite(16.0))
  // Step 8/9: radix 0 means "unspecified" (default 10), not "radix zero".
  assert eval("parseInt('12', 0)") == JsNumber(Finite(12.0))
}

// ----------------------------------------------------------------------------
// parseFloat — §19.2.4 longest StrDecimalLiteral prefix
// ----------------------------------------------------------------------------

pub fn eval_parse_float_literal_forms_test() {
  assert eval("parseFloat('.5')") == JsNumber(Finite(0.5))
  assert eval("parseFloat('1e5')") == JsNumber(Finite(100_000.0))
  assert eval("parseFloat('1.')") == JsNumber(Finite(1.0))
  assert eval("parseFloat('+.5e+3')") == JsNumber(Finite(500.0))
}

pub fn eval_parse_float_longest_prefix_test() {
  assert eval("parseFloat('Infinityx')") == JsNumber(Infinity)
  assert eval("parseFloat('1.2.3')") == JsNumber(Finite(1.2))
  assert eval("parseFloat('1foo')") == JsNumber(Finite(1.0))
  // An incomplete exponent is not part of the match.
  assert eval("parseFloat('5e')") == JsNumber(Finite(5.0))
}

pub fn eval_parse_float_no_match_is_nan_test() {
  assert eval("parseFloat('e5')") == JsNumber(NaN)
  assert eval("parseFloat('.')") == JsNumber(NaN)
  assert eval("parseFloat('')") == JsNumber(NaN)
}

pub fn eval_legacy_octal_escape_above_7f_test() {
  // '\251' is U+00A9 (©): must cook to valid UTF-8, not a raw 0xA9 byte.
  assert eval("'\\251'") == JsString("©")
  assert eval("'\\251'.charCodeAt(0)") == JsNumber(Finite(169.0))
  assert eval("'\\251'.length") == JsNumber(Finite(1.0))
}
