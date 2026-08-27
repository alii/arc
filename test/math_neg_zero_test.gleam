// beam: -0.0 >=. 0.0 is true, 0.0 =:= -0.0 is false

import arc/engine.{type JsValueKind, JsBool, Returned}

fn eval(source: String) -> JsValueKind {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  engine.classify(value)
}

pub fn arithmetic_overflow_is_infinity_test() {
  assert eval("1e308 * 10 === Infinity") == JsBool(True)
  assert eval("-1e308 * 10 === -Infinity") == JsBool(True)
  assert eval("1e308 + 1e308 === Infinity") == JsBool(True)
  assert eval("-1e308 - 1e308 === -Infinity") == JsBool(True)
  assert eval("Number.MAX_VALUE * 2 === Infinity") == JsBool(True)
  assert eval("1e308 / -1e-10 === -Infinity") == JsBool(True)
  assert eval("var x = 1e308; x *= -10; x === -Infinity") == JsBool(True)
}

pub fn math_hypot_intermediate_overflow_test() {
  assert eval("Math.hypot(1e200, 1e200) === 1.414213562373095e200")
    == JsBool(True)
  assert eval("Math.hypot(1e154, 1e154) === 1.4142135623730953e154")
    == JsBool(True)
  assert eval("Math.hypot(1.5e308, 1.5e308) === Infinity") == JsBool(True)
  assert eval("Math.hypot(5e-324, 5e-324) === 5e-324") == JsBool(True)
  assert eval("Math.hypot(3, 4) === 5") == JsBool(True)
  assert eval("Math.hypot() === 0") == JsBool(True)
  assert eval("Math.hypot(NaN, Infinity) === Infinity") == JsBool(True)
  assert eval("Number.isNaN(Math.hypot(NaN, 1))") == JsBool(True)
}

pub fn math_atan2_negative_zero_y_test() {
  assert eval("Object.is(Math.atan2(-0, Infinity), -0)") == JsBool(True)
  assert eval("Object.is(Math.atan2(0, Infinity), 0)") == JsBool(True)
  assert eval("Math.atan2(-0, -Infinity) === -Math.PI") == JsBool(True)
  assert eval("Math.atan2(0, -Infinity) === Math.PI") == JsBool(True)
}

pub fn math_abs_negative_zero_test() {
  assert eval("Object.is(Math.abs(-0), 0)") == JsBool(True)
  assert eval("Object.is(Math.abs(0), 0)") == JsBool(True)
  assert eval("Math.abs(-5) === 5") == JsBool(True)
  assert eval("Math.abs(-Infinity) === Infinity") == JsBool(True)
}

pub fn math_pow_negative_zero_base_test() {
  assert eval("Math.pow(-0, -1) === -Infinity") == JsBool(True)
  assert eval("(-0) ** -1 === -Infinity") == JsBool(True)
  assert eval("Math.pow(-0, -2) === Infinity") == JsBool(True)
  assert eval("Math.pow(0, -1) === Infinity") == JsBool(True)
  assert eval("Number.isNaN(Math.pow(-2, 0.5))") == JsBool(True)
  assert eval("Math.pow(-0, -0) === 1") == JsBool(True)
}

pub fn bigint_negative_zero_test() {
  assert eval("BigInt(-0) === 0n") == JsBool(True)
  assert eval("BigInt(0) === 0n") == JsBool(True)
  assert eval("BigInt(-5) === -5n") == JsBool(True)
  assert eval(
      "(() => { try { BigInt(1.5); return false } catch (e) { return e instanceof RangeError } })()",
    )
    == JsBool(True)
}

pub fn math_extremum_zero_ties_test() {
  assert eval("Object.is(Math.max(0, -0), 0)") == JsBool(True)
  assert eval("Object.is(Math.max(-0, 0), 0)") == JsBool(True)
  assert eval("Object.is(Math.min(0, -0), -0)") == JsBool(True)
  assert eval("Object.is(Math.min(-0, 0), -0)") == JsBool(True)
  assert eval("Math.max() === -Infinity") == JsBool(True)
  assert eval("Math.min() === Infinity") == JsBool(True)
  assert eval("Math.max(5, -Infinity) === 5") == JsBool(True)
  assert eval("Math.min(-1, Infinity) === -1") == JsBool(True)
  assert eval("Number.isNaN(Math.max(1, NaN))") == JsBool(True)
}

pub fn math_negative_zero_preserving_test() {
  assert eval("Object.is(Math.round(-0.3), -0)") == JsBool(True)
  assert eval("Object.is(Math.round(-0.5), -0)") == JsBool(True)
  assert eval("Math.round(0.5) === 1") == JsBool(True)
  assert eval("Object.is(Math.trunc(-0.5), -0)") == JsBool(True)
  assert eval("Object.is(Math.sign(-0), -0)") == JsBool(True)
  assert eval("Object.is(Math.cbrt(-0), -0)") == JsBool(True)
}

pub fn string_from_code_point_negative_zero_test() {
  assert eval("String.fromCodePoint(-0) === '\\u0000'") == JsBool(True)
  assert eval("String.fromCodePoint(0) === '\\u0000'") == JsBool(True)
  assert eval("String.fromCodePoint(65) === 'A'") == JsBool(True)
  assert eval(range_error("String.fromCodePoint(0.5)")) == JsBool(True)
  assert eval(range_error("String.fromCodePoint(-1)")) == JsBool(True)
  assert eval(range_error("String.fromCodePoint(0x110000)")) == JsBool(True)
}

pub fn array_constructor_negative_zero_length_test() {
  assert eval("new Array(-0).length === 0") == JsBool(True)
  assert eval("Array(-0).length === 0") == JsBool(True)
  assert eval("new Array(0).length === 0") == JsBool(True)
  assert eval("new Array(3).length === 3") == JsBool(True)
  assert eval("new Array(4294967295).length === 4294967295") == JsBool(True)
  assert eval(range_error("new Array(1.5)")) == JsBool(True)
  assert eval(range_error("new Array(-1)")) == JsBool(True)
  assert eval(range_error("new Array(4294967296)")) == JsBool(True)
}

pub fn array_set_length_bounds_test() {
  assert eval("const a = []; a.length = -0; a.length === 0") == JsBool(True)
  assert eval("const a = []; a.length = 4294967295; a.length === 4294967295")
    == JsBool(True)
  assert eval(range_error("const a = []; a.length = 4294967296"))
    == JsBool(True)
  assert eval(range_error("const a = []; a.length = 1e21")) == JsBool(True)
  assert eval(range_error("const a = []; a.length = -1")) == JsBool(True)
  assert eval(range_error("const a = []; a.length = 1.5")) == JsBool(True)
  assert eval(range_error(
      "Object.defineProperty([], 'length', { value: 4294967296 })",
    ))
    == JsBool(True)
}

pub fn number_is_integer_negative_zero_test() {
  assert eval("Number.isInteger(-0)") == JsBool(True)
  assert eval("Number.isSafeInteger(-0)") == JsBool(True)
  assert eval("Number.isInteger(1.5) === false") == JsBool(True)
  assert eval("Number.isSafeInteger(2 ** 53) === false") == JsBool(True)
}

pub fn temporal_instant_negative_zero_epoch_test() {
  assert eval(
      "Temporal.Instant.fromEpochMilliseconds(-0).epochMilliseconds === 0",
    )
    == JsBool(True)
  assert eval(range_error("Temporal.Instant.fromEpochMilliseconds(1.5)"))
    == JsBool(True)
}

fn range_error(expr: String) -> String {
  "(() => { try { "
  <> expr
  <> "; return false } catch (e) { return e instanceof RangeError } })()"
}
