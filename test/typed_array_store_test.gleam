//// §10.4.5.16 IntegerIndexedElementSet regression tests.
////
//// Typed-array element stores must run the CANONICAL ToNumber / ToBigInt
//// (ops/coerce, reached through the RealmCtx to_number_fn / to_bigint_fn
//// hooks) — not a private re-implementation. These pin the two deviations
//// the old private copy had, so a re-fork of the coercion path shows up
//// as a unit-test failure and not just a test262 delta:
////   1. §7.1.14 StringToBigInt accepts 0x/0o/0b prefixes.
////   2. IsCallable recognizes callable proxies, so a Proxy-wrapped valueOf
////      participates in OrdinaryToPrimitive.

import arc/engine
import arc/vm/ops/object
import arc/vm/value.{DataProperty, Finite, JsNumber, JsString, Named}
import gleam/option.{Some}

/// Run `source` on a fresh engine and return the value of the global
/// variable `name` afterwards. Asserting on a global (rather than the
/// completion value) keeps the test independent of the engine.eval
/// completion type.
fn global_after(source: String, name: String) -> value.JsValue {
  let eng = engine.new()
  let assert Ok(#(_, eng)) = engine.eval(eng, source)
  let assert Some(DataProperty(value: v, ..)) =
    object.get_own_property(engine.heap(eng), engine.global(eng), Named(name))
  v
}

pub fn bigint_store_accepts_radix_prefixed_strings_test() {
  // StringToBigInt (§7.1.14) is not decimal-only: 0x/0o/0b must parse.
  assert global_after(
      "var a = new BigInt64Array(3);
       a[0] = '0x10'; a[1] = '0o17'; a[2] = '0b101';
       var out = String(a[0]) + ',' + String(a[1]) + ',' + String(a[2])",
      "out",
    )
    == JsString("16,15,5")
}

pub fn bigint_store_rejects_malformed_string_with_syntax_error_test() {
  // §7.1.13 ToBigInt: StringToBigInt failure is a SyntaxError, not TypeError.
  assert global_after(
      "var a = new BigInt64Array(1);
       var out;
       try { a[0] = '0xZZ'; out = 'no-throw' } catch (e) { out = e.constructor.name }",
      "out",
    )
    == JsString("SyntaxError")
}

pub fn store_runs_callable_proxy_value_of_test() {
  // IsCallable(proxy with callable target) is true (§10.5.15): a stored
  // object whose valueOf is a callable Proxy must be converted through it,
  // not silently coerced to NaN/0.
  assert global_after(
      "var t = new Int32Array(1);
       t[0] = { valueOf: new Proxy(function() { return 42 }, {}) };
       var out = t[0]",
      "out",
    )
    == JsNumber(Finite(42.0))
}

pub fn store_throws_on_non_callable_to_primitive_test() {
  // §7.1.1 GetMethod: a non-null, non-callable @@toPrimitive is a TypeError
  // (the old private ToPrimitive silently fell through to valueOf).
  assert global_after(
      "var t = new Int32Array(1);
       var o = { valueOf: function() { return 1 } };
       o[Symbol.toPrimitive] = 42;
       var out;
       try { t[0] = o; out = 'no-throw' } catch (e) { out = e.constructor.name }",
      "out",
    )
    == JsString("TypeError")
}
