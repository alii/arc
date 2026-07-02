//// Regression tests for `scope.param_shim`'s synthetic naming.
////
//// The shims `sb_insert_param_shims` inserts for a non-simple parameter
//// list used to be named `$param_N` — a VALID JavaScript identifier. A
//// user formal literally spelled `$param_N` therefore collided with the
//// shim: `sb_insert_param_shims` shifts the already-declared user formals
//// past the shims and then `dict.insert`s the shims at indices 0..N-1, so
//// the shim's insert silently CLOBBERED the same-named user binding. The
//// user name then resolved to the shim's slot, mis-wiring the destructuring
//// prologue. `param_shim` now mints `<paramN>` — angle brackets make it
//// impossible as a JS identifier (same convention as the `<withN_M>`
//// with-object holder), so the collision cannot happen.

import arc/engine.{Returned}
import arc/vm/value.{type JsValue, JsString}

/// Eval `source` on a fresh engine and return the completion value.
fn assert_eval(source: String) -> JsValue {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  value
}

/// `$param_0` and `$param_1` as USER formal names in a non-simple parameter
/// list (the default on `$param_0` makes the list non-simple, so shims are
/// inserted for both positions).
///
/// With the old `$param_N` shim naming, BOTH user bindings were clobbered
/// by the same-named shims. The prologue's step for position 0 then wrote
/// `$param_1`'s value into slot 1 — the very slot the position-1 shim reads
/// next — so `$param_0`'s `undefined` was never seen, its default was
/// skipped, and `f(7)` returned "7,7" instead of "1,7".
pub fn user_param_named_like_shim_test() {
  assert assert_eval(
      "function f($param_1, $param_0 = 1) {
         return '' + $param_0 + ',' + $param_1;
       }
       f(7)",
    )
    == JsString("1,7")
}

/// The single-collision case: one user formal literally named `$param_0`.
/// It must behave like any other parameter — the passed argument wins, the
/// default applies only when the argument is absent — and its value must
/// stay independent of the sibling destructured parameter.
pub fn user_param_named_param_0_test() {
  assert assert_eval(
      "function f($param_0 = 'd', [x] = ['y']) {
         return $param_0 + ':' + x;
       }
       f() + '|' + f('a', ['b'])",
    )
    == JsString("d:y|a:b")
}
