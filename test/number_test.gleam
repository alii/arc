import arc/engine.{
  type JsValueKind, Finite, Infinity, JsBool, JsNumber, JsString, NaN,
  NegInfinity, Returned,
}
import arc/parser/ast.{FiniteNumber, InfiniteNumber}
import arc/parser/number.{BigIntValue, NumberValue}
import gleam/string

fn eval(source: String) -> JsValueKind {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  engine.classify(value)
}

fn num(raw: String) -> Float {
  let assert Ok(NumberValue(FiniteNumber(f))) =
    number.parse_numeric_literal(raw)
  f
}

pub fn number_dot_before_exponent_test() {
  assert num("1.e3") == 1000.0
  assert num("2.E5") == 200_000.0
  assert num("1_0.e2") == 1000.0
}

pub fn number_bare_dot_forms_test() {
  assert num("1.") == 1.0
  assert num(".5") == 0.5
  assert num(".5e2") == 50.0
  assert num("1.5e2") == 150.0
}

pub fn number_exponent_overflow_is_infinity_test() {
  assert number.parse_numeric_literal("1e400")
    == Ok(NumberValue(InfiniteNumber))
  assert number.parse_numeric_literal("1.5e400")
    == Ok(NumberValue(InfiniteNumber))
  assert number.parse_numeric_literal("1" <> string.repeat("0", 400))
    == Ok(NumberValue(InfiniteNumber))
}

pub fn number_exponent_underflow_is_zero_test() {
  assert num("1e-400") == 0.0
}

pub fn number_radix_prefix_test() {
  assert num("0xFF") == 255.0
  assert num("0XF_F") == 255.0
  assert num("0o17") == 15.0
  assert num("0b1_01") == 5.0
  assert num("0") == 0.0
  assert num("1_000") == 1000.0
}

pub fn number_legacy_octal_is_base_eight_test() {
  assert num("010") == 8.0
  assert num("0777") == 511.0
  assert num("00") == 0.0
}

pub fn number_non_octal_decimal_is_base_ten_test() {
  assert num("08") == 8.0
  assert num("09") == 9.0
  assert num("08.5") == 8.5
}

pub fn number_bigint_test() {
  assert number.parse_numeric_literal("0xffn") == Ok(BigIntValue(255))
  assert number.parse_numeric_literal("0B101n") == Ok(BigIntValue(5))
  assert number.parse_numeric_literal("1_000n") == Ok(BigIntValue(1000))
  assert number.parse_numeric_literal("0n") == Ok(BigIntValue(0))
}

pub fn number_malformed_is_typed_error_test() {
  assert number.parse_numeric_literal("0x") == Error(number.EmptyDigits)
  assert number.parse_numeric_literal("0xZZ")
    == Error(number.NotANumericLiteral("ZZ"))
  assert number.parse_numeric_literal("0b12")
    == Error(number.NotANumericLiteral("12"))
}

pub fn eval_dot_exponent_literal_test() {
  assert eval("1.e3") == JsNumber(Finite(1000.0))
  assert eval("2.E5") == JsNumber(Finite(200_000.0))
}

pub fn eval_overflowing_literal_test() {
  assert eval("1e400") == JsNumber(Infinity)
  assert eval("1e400 === Number.MAX_VALUE") == JsBool(False)
  assert eval("(1e400).toString()") == JsString("Infinity")
}

pub fn eval_legacy_octal_number_test() {
  assert eval("010") == JsNumber(Finite(8.0))
  assert eval("0777") == JsNumber(Finite(511.0))
  assert eval("08") == JsNumber(Finite(8.0))
}

pub fn eval_legacy_octal_number_strict_is_syntax_error_test() {
  assert eval("try { eval('\"use strict\"; 010') } catch (e) { e.name }")
    == JsString("SyntaxError")
}

pub fn eval_non_octal_decimal_escape_test() {
  assert eval("'\\8'") == JsString("8")
  assert eval("'\\9'") == JsString("9")
}

pub fn eval_non_octal_decimal_escape_strict_is_syntax_error_test() {
  assert eval("try { eval('\"use strict\"; \"\\\\8\"') } catch (e) { e.name }")
    == JsString("SyntaxError")
  assert eval("try { eval('\"use strict\"; \"\\\\9\"') } catch (e) { e.name }")
    == JsString("SyntaxError")
}

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

pub fn eval_json_parse_huge_integer_is_infinity_test() {
  assert eval("JSON.parse('1' + '0'.repeat(400))") == JsNumber(Infinity)
  assert eval("JSON.parse('-' + '9'.repeat(400))") == JsNumber(NegInfinity)
}

pub fn eval_intl_duration_format_huge_component_throws_range_error_test() {
  let src =
    "try { new Intl.DurationFormat('en').format('PT' + '9'.repeat(400) + 'S') }"
    <> " catch (e) { e.name }"
  assert eval(src) == JsString("RangeError")
}

// known deviation: beyond-double bigint formats as infinity
pub fn eval_intl_number_format_huge_bigint_test() {
  let src =
    "var nf = new Intl.NumberFormat('en');"
    <> " nf.format(10n ** 400n) === nf.format(Infinity)"
  assert eval(src) == JsBool(True)
}

pub fn eval_parse_int_sign_before_hex_prefix_test() {
  assert eval("parseInt('-0x10')") == JsNumber(Finite(-16.0))
  assert eval("parseInt(' +0x10')") == JsNumber(Finite(16.0))
}

pub fn eval_parse_int_explicit_radix_and_hex_prefix_test() {
  assert eval("parseInt('0x10', 8)") == JsNumber(Finite(0.0))
  assert eval("parseInt('0x10', 10)") == JsNumber(Finite(0.0))
  assert eval("parseInt('0x10', 16)") == JsNumber(Finite(16.0))
  assert eval("parseInt('12', 0)") == JsNumber(Finite(12.0))
}

pub fn eval_parse_int_radix_is_to_int32_test() {
  assert eval("parseInt('f', 2**32 + 16)") == JsNumber(Finite(15.0))
  assert eval("parseInt('12', 2**32 + 8)") == JsNumber(Finite(10.0))
  assert eval("parseInt('10', 2**32 + 1)") == JsNumber(NaN)
  assert eval("parseInt('10', 2**31)") == JsNumber(NaN)
}

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
  assert eval("parseFloat('5e')") == JsNumber(Finite(5.0))
}

pub fn eval_parse_float_no_match_is_nan_test() {
  assert eval("parseFloat('e5')") == JsNumber(NaN)
  assert eval("parseFloat('.')") == JsNumber(NaN)
  assert eval("parseFloat('')") == JsNumber(NaN)
}

pub fn eval_parse_combining_mark_terminates_literal_test() {
  assert eval("parseFloat('1' + String.fromCharCode(0x0301))")
    == JsNumber(Finite(1.0))
  assert eval("parseFloat('1.5' + String.fromCharCode(0x0300))")
    == JsNumber(Finite(1.5))
  assert eval("parseInt('19' + String.fromCharCode(0x0301))")
    == JsNumber(Finite(19.0))
  assert eval("parseFloat(String.fromCharCode(0x0301) + '1')") == JsNumber(NaN)
}

pub fn eval_legacy_octal_escape_above_7f_test() {
  assert eval("'\\251'") == JsString("©")
  assert eval("'\\251'.charCodeAt(0)") == JsNumber(Finite(169.0))
  assert eval("'\\251'.length") == JsNumber(Finite(1.0))
}

pub fn eval_number_to_string_fractional_radix_test() {
  assert eval("(3.5).toString(16)") == JsString("3.8")
  assert eval("(-255.5).toString(16)") == JsString("-ff.8")
  assert eval("(0.1).toString(2)")
    == JsString("0.0001100110011001100110011001100110011001100110011001101")
  assert eval("(0.1).toString(3)")
    == JsString("0.0022002200220022002200220022002201")
  assert eval("(1234.5678).toString(36)") == JsString("ya.kfv9yqdpm")
  assert eval("(255).toString(16)") == JsString("ff")
  assert eval("(NaN).toString(2)") == JsString("NaN")
  assert eval("(-0).toString(2)") == JsString("0")
}

pub fn eval_to_string_radix_range_test() {
  assert eval("try { (3).toString(1) } catch (e) { e.name }")
    == JsString("RangeError")
  assert eval("try { (3n).toString(37) } catch (e) { e.name }")
    == JsString("RangeError")
}

pub fn eval_bigint_to_locale_string_test() {
  assert eval("(1234567n).toLocaleString('de-DE')") == JsString("1.234.567")
  assert eval("(1234567n).toLocaleString('en-US')") == JsString("1,234,567")
  assert eval("[1234567n].toLocaleString('de-DE')") == JsString("1.234.567")
  assert eval("(255n).toLocaleString(16)") == JsString("255")
  assert eval("(255n).toLocaleString()") == JsString("255")
  assert eval("(255n).toString(16)") == JsString("ff")
  assert eval("try { (1n).toLocaleString('en_US') } catch (e) { e.name }")
    == JsString("RangeError")
}
