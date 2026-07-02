/// §7.1.5 ToIntegerOrInfinity conformance tests for the single canonical
/// implementation in `arc/vm/ops/coerce`. These pin the two properties the
/// per-module copies used to violate:
///   - ±∞/NaN must reach a builtin's range guard (saturated, not replaced by
///     a caller-chosen default), so the spec-mandated RangeErrors fire.
///   - ±∞ saturated to ±(2^53 - 1) still clamps to the spec's explicit
///     "+∞ → len / -∞ → 0" index branches.
import arc/engine.{Returned}
import arc/vm/value.{JsBool, JsString}

/// Eval source on a fresh engine, assert normal completion, return the value.
fn eval(source: String) -> value.JsValue {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  value
}

/// Eval an expression expected to throw; return the thrown error's `name`
/// (e.g. "RangeError"), or "no throw" if it completed normally.
fn thrown_name(expr: String) -> value.JsValue {
  eval(
    "(function () { try { "
    <> expr
    <> "; return 'no throw'; } catch (e) { return e.name; } })()",
  )
}

pub fn to_string_infinite_radix_throws_range_error_test() {
  // §21.1.3.6 step 4: radixMV = ToIntegerOrInfinity(Infinity) is outside
  // [2, 36]. An infinity-erasing coercion would substitute the default
  // radix 10 and return "255" instead.
  assert thrown_name("(255).toString(Infinity)") == JsString("RangeError")
}

pub fn to_fixed_infinite_digits_throws_range_error_test() {
  // §21.1.3.3 step 3: f = ToIntegerOrInfinity(Infinity) is outside [0, 100].
  assert thrown_name("(1).toFixed(Infinity)") == JsString("RangeError")
}

pub fn slice_negative_infinity_start_is_zero_test() {
  // §23.1.3.28 step 3: relativeStart = -∞ → k = 0.
  assert eval("JSON.stringify([1,2,3].slice(-Infinity))") == JsString("[1,2,3]")
}

pub fn string_at_negative_infinity_is_undefined_test() {
  // §22.1.3.1: relativeIndex = -∞ → k = len + relativeIndex < 0 → undefined.
  assert eval("'abc'.at(-Infinity) === undefined") == JsBool(True)
}
