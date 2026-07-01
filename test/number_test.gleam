/// Numeric-literal and string-escape cooking tests.
import arc/engine.{Returned}
import arc/parser/number
import arc/vm/value.{Finite, JsNumber, JsString}

/// Largest finite IEEE double — the value an overflowing literal clamps to
/// (BEAM floats cannot express Infinity).
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
  // The JS value is Infinity; the AST stores a plain Float and BEAM has no
  // float infinity, so overflow clamps to the largest finite double (the
  // same policy huge integer literals already use).
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
  assert eval("1e400") == JsNumber(Finite(max_double))
}

pub fn eval_legacy_octal_escape_above_7f_test() {
  // '\251' is U+00A9 (©): must cook to valid UTF-8, not a raw 0xA9 byte.
  assert eval("'\\251'") == JsString("©")
  assert eval("'\\251'.charCodeAt(0)") == JsNumber(Finite(169.0))
  assert eval("'\\251'.length") == JsNumber(Finite(1.0))
}
