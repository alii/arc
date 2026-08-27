//// §7.1.5 tointegerorinfinity keeps infinities for range checks

import arc/engine.{type JsValueKind, JsBool, JsString, Returned}

fn eval(source: String) -> JsValueKind {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  engine.classify(value)
}

fn thrown_name(expr: String) -> JsValueKind {
  eval(
    "(function () { try { "
    <> expr
    <> "; return 'no throw'; } catch (e) { return e.name; } })()",
  )
}

pub fn to_string_infinite_radix_throws_range_error_test() {
  assert thrown_name("(255).toString(Infinity)") == JsString("RangeError")
}

pub fn to_fixed_infinite_digits_throws_range_error_test() {
  assert thrown_name("(1).toFixed(Infinity)") == JsString("RangeError")
}

pub fn slice_negative_infinity_start_is_zero_test() {
  assert eval("JSON.stringify([1,2,3].slice(-Infinity))") == JsString("[1,2,3]")
}

pub fn string_at_negative_infinity_is_undefined_test() {
  assert eval("'abc'.at(-Infinity) === undefined") == JsBool(True)
}
