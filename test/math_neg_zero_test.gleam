/// ±0, integrality and float-overflow regressions in Math / BigInt.
///
/// Every case here was wrong — or, for `Math.hypot`, killed the whole BEAM
/// process with a `badarith` that is NOT a JS throw — before the negative-zero
/// and integrality predicates were unified onto `operators.is_negative_float`,
/// `value.integral_int` and the total `arc_math_ffi` wrappers.
///
/// The recurring trap: on the BEAM `-0.0 >=. 0.0` is True, `-0.0 <. 0.0` is
/// False, and `0.0 =:= -0.0` is False. So a bare comparison reads -0 as
/// POSITIVE, while a bare `=:=` reads -0 as NON-INTEGRAL.
import arc/engine.{Returned}
import arc/vm/value.{JsBool}

/// Eval `source` on a fresh engine, asserting normal completion.
fn eval(source: String) -> value.JsValue {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  value
}

/// §21.3.2.18 Math.hypot. The old fold squared and summed raw Floats inline,
/// so an intermediate overflow raised `badarith` and took the runtime process
/// down — for arguments whose true result is perfectly finite.
pub fn math_hypot_intermediate_overflow_test() {
  // Would previously badarith: 1e200 * 1e200 overflows a 64-bit float even
  // though sqrt(2)*1e200 is finite.
  assert eval("Math.hypot(1e200, 1e200) === 1.414213562373095e200")
    == JsBool(True)
  assert eval("Math.hypot(1e154, 1e154) === 1.4142135623730953e154")
    == JsBool(True)
  // Genuinely out of range → +Infinity, not a crash.
  assert eval("Math.hypot(1.5e308, 1.5e308) === Infinity") == JsBool(True)
  // The other direction: squaring a subnormal underflowed to 0, so the old
  // fold answered 0 here. Scaling by the max magnitude keeps the value.
  assert eval("Math.hypot(5e-324, 5e-324) === 5e-324") == JsBool(True)
  // Ordinary cases still exact.
  assert eval("Math.hypot(3, 4) === 5") == JsBool(True)
  assert eval("Math.hypot() === 0") == JsBool(True)
  // §21.3.2.18 step 3: ±Infinity beats NaN.
  assert eval("Math.hypot(NaN, Infinity) === Infinity") == JsBool(True)
  assert eval("Number.isNaN(Math.hypot(NaN, 1))") == JsBool(True)
}

/// §21.3.2.8 Math.atan2 steps 12-15: the result takes y's sign, and a -0 y is
/// NEGATIVE. The old `yv >=. 0.0` test read -0 as positive.
pub fn math_atan2_negative_zero_y_test() {
  assert eval("Object.is(Math.atan2(-0, Infinity), -0)") == JsBool(True)
  assert eval("Object.is(Math.atan2(0, Infinity), 0)") == JsBool(True)
  assert eval("Math.atan2(-0, -Infinity) === -Math.PI") == JsBool(True)
  assert eval("Math.atan2(0, -Infinity) === Math.PI") == JsBool(True)
}

/// §21.3.2.1 step 4: "If x is -0𝔽, return +0𝔽". `float.absolute_value` is
/// `case x >=. 0.0 { True -> x .. }`, so it handed -0.0 straight back.
pub fn math_abs_negative_zero_test() {
  assert eval("Object.is(Math.abs(-0), 0)") == JsBool(True)
  assert eval("Object.is(Math.abs(0), 0)") == JsBool(True)
  assert eval("Math.abs(-5) === 5") == JsBool(True)
  assert eval("Math.abs(-Infinity) === Infinity") == JsBool(True)
}

/// A -0 base is negative, so an overflowing pow must take its sign. The old
/// Erlang guard `when Base < 0` is False for -0.0 and fell into the
/// unsigned `-> infinity` catch-all.
pub fn math_pow_negative_zero_base_test() {
  assert eval("Math.pow(-0, -1) === -Infinity") == JsBool(True)
  assert eval("(-0) ** -1 === -Infinity") == JsBool(True)
  // Even exponent → +Infinity; a +0 base is unsigned.
  assert eval("Math.pow(-0, -2) === Infinity") == JsBool(True)
  assert eval("Math.pow(0, -1) === Infinity") == JsBool(True)
  // A negative base with a non-integer exponent has no real result.
  assert eval("Number.isNaN(Math.pow(-2, 0.5))") == JsBool(True)
  // ±0 exponent is always 1, even for a -0 base.
  assert eval("Math.pow(-0, -0) === 1") == JsBool(True)
}

/// §4.4.31 IsIntegralNumber. `int.to_float(i) == f` compiles to BEAM `=:=`,
/// and `0.0 =:= -0.0` is False — so -0 read as non-integral and BigInt(-0)
/// threw a RangeError instead of yielding 0n.
pub fn bigint_negative_zero_test() {
  assert eval("BigInt(-0) === 0n") == JsBool(True)
  assert eval("BigInt(0) === 0n") == JsBool(True)
  assert eval("BigInt(-5) === -5n") == JsBool(True)
  // A genuinely fractional Number still throws.
  assert eval(
      "(() => { try { BigInt(1.5); return false } catch (e) { return e instanceof RangeError } })()",
    )
    == JsBool(True)
}

/// §21.3.2.24/25: Math.max(+0, -0) is +0 and Math.min(+0, -0) is -0. The
/// seed, the dominant value and the tie-break must agree; they now all come
/// out of one `case which`.
pub fn math_extremum_zero_ties_test() {
  assert eval("Object.is(Math.max(0, -0), 0)") == JsBool(True)
  assert eval("Object.is(Math.max(-0, 0), 0)") == JsBool(True)
  assert eval("Object.is(Math.min(0, -0), -0)") == JsBool(True)
  assert eval("Object.is(Math.min(-0, 0), -0)") == JsBool(True)
  // Empty-arg identities, and the ±Infinity dominance the seed must not eat.
  assert eval("Math.max() === -Infinity") == JsBool(True)
  assert eval("Math.min() === Infinity") == JsBool(True)
  assert eval("Math.max(5, -Infinity) === 5") == JsBool(True)
  assert eval("Math.min(-1, Infinity) === -1") == JsBool(True)
  assert eval("Number.isNaN(Math.max(1, NaN))") == JsBool(True)
}

/// Math.round/trunc/sign preserve -0 through the shared sign predicate.
pub fn math_negative_zero_preserving_test() {
  assert eval("Object.is(Math.round(-0.3), -0)") == JsBool(True)
  assert eval("Object.is(Math.round(-0.5), -0)") == JsBool(True)
  assert eval("Math.round(0.5) === 1") == JsBool(True)
  assert eval("Object.is(Math.trunc(-0.5), -0)") == JsBool(True)
  assert eval("Object.is(Math.sign(-0), -0)") == JsBool(True)
  assert eval("Object.is(Math.cbrt(-0), -0)") == JsBool(True)
}

/// §22.1.2.2 String.fromCodePoint step 2b requires an integral Number, and -0
/// IS integral: `String.fromCodePoint(-0)` is U+0000, not a RangeError. The
/// open-coded `int.to_float(i) == f` read -0 as fractional and threw.
pub fn string_from_code_point_negative_zero_test() {
  assert eval("String.fromCodePoint(-0) === '\\u0000'") == JsBool(True)
  assert eval("String.fromCodePoint(0) === '\\u0000'") == JsBool(True)
  assert eval("String.fromCodePoint(65) === 'A'") == JsBool(True)
  // Genuinely fractional / out-of-range code points still throw.
  assert eval(range_error("String.fromCodePoint(0.5)")) == JsBool(True)
  assert eval(range_error("String.fromCodePoint(-1)")) == JsBool(True)
  assert eval(range_error("String.fromCodePoint(0x110000)")) == JsBool(True)
}

/// §23.1.1.1 step 5b compares intLen to len with SameValueZero, and
/// SameValueZero(0, -0) is true — so `Array(-0)` is a length-0 array, not a
/// RangeError. The open-coded `int.to_float(len) == n` read -0 as fractional
/// and threw.
pub fn array_constructor_negative_zero_length_test() {
  assert eval("new Array(-0).length === 0") == JsBool(True)
  assert eval("Array(-0).length === 0") == JsBool(True)
  assert eval("new Array(0).length === 0") == JsBool(True)
  assert eval("new Array(3).length === 3") == JsBool(True)
  assert eval("new Array(4294967295).length === 4294967295") == JsBool(True)
  // Non-integral, negative and >= 2^32 lengths still throw.
  assert eval(range_error("new Array(1.5)")) == JsBool(True)
  assert eval(range_error("new Array(-1)")) == JsBool(True)
  assert eval(range_error("new Array(4294967296)")) == JsBool(True)
}

/// `arr.length = n` (§10.4.2.4 ArraySetLength) is a validator distinct from the
/// `Array(n)` constructor above, and it had drifted: it accepted ANY
/// non-negative integer, so `arr.length = 2 ** 32` silently produced a
/// 2^32-length array, while `arr.length = -0` threw. Both now route through
/// `value.array_length`.
pub fn array_set_length_bounds_test() {
  assert eval("const a = []; a.length = -0; a.length === 0") == JsBool(True)
  assert eval("const a = []; a.length = 4294967295; a.length === 4294967295")
    == JsBool(True)
  // >= 2^32 is not a uint32: ToUint32(len) !== ToNumber(len) → RangeError.
  assert eval(range_error("const a = []; a.length = 4294967296"))
    == JsBool(True)
  assert eval(range_error("const a = []; a.length = 1e21")) == JsBool(True)
  assert eval(range_error("const a = []; a.length = -1")) == JsBool(True)
  assert eval(range_error("const a = []; a.length = 1.5")) == JsBool(True)
  // defineProperty agrees with the assignment path.
  assert eval(range_error(
      "Object.defineProperty([], 'length', { value: 4294967296 })",
    ))
    == JsBool(True)
}

/// IsIntegralNumber(-0) is true, so Number.isInteger(-0) and
/// Number.isSafeInteger(-0) are too. The open-coded
/// `int.to_float(float.truncate(n)) == n` reads -0 as non-integral (see the
/// module header) — both wrongly returned false.
pub fn number_is_integer_negative_zero_test() {
  assert eval("Number.isInteger(-0)") == JsBool(True)
  assert eval("Number.isSafeInteger(-0)") == JsBool(True)
  assert eval("Number.isInteger(1.5) === false") == JsBool(True)
  assert eval("Number.isSafeInteger(2 ** 53) === false") == JsBool(True)
}

/// Temporal.Instant.fromEpochMilliseconds(-0) is the epoch, not a "not an
/// integral number" RangeError.
pub fn temporal_instant_negative_zero_epoch_test() {
  assert eval(
      "Temporal.Instant.fromEpochMilliseconds(-0).epochMilliseconds === 0",
    )
    == JsBool(True)
  assert eval(range_error("Temporal.Instant.fromEpochMilliseconds(1.5)"))
    == JsBool(True)
}

/// `expr` throws a RangeError — as a JS expression evaluating to a boolean.
fn range_error(expr: String) -> String {
  "(() => { try { "
  <> expr
  <> "; return false } catch (e) { return e instanceof RangeError } })()"
}
