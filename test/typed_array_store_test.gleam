import arc/engine.{type JsValueKind, Finite, JsNumber, JsString}
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{DataProperty, StringKey}
import gleam/option.{Some}

fn global_after(source: String, name: String) -> JsValueKind {
  let eng = engine.new()
  let assert Ok(#(_, eng)) = engine.eval(eng, source)
  let assert Some(k) = rt_store.t_find_key(engine.heap(eng), name)
  let assert #(Some(DataProperty(value: v, ..)), _) =
    rt_obj.t_get_own_property(
      engine.heap(eng),
      engine.global(eng),
      StringKey(k),
    )
  engine.classify(v)
}

pub fn bigint_store_accepts_radix_prefixed_strings_test() {
  assert global_after(
      "var a = new BigInt64Array(3);
       a[0] = '0x10'; a[1] = '0o17'; a[2] = '0b101';
       var out = String(a[0]) + ',' + String(a[1]) + ',' + String(a[2])",
      "out",
    )
    == JsString("16,15,5")
}

pub fn bigint_store_rejects_malformed_string_with_syntax_error_test() {
  assert global_after(
      "var a = new BigInt64Array(1);
       var out;
       try { a[0] = '0xZZ'; out = 'no-throw' } catch (e) { out = e.constructor.name }",
      "out",
    )
    == JsString("SyntaxError")
}

pub fn store_runs_callable_proxy_value_of_test() {
  assert global_after(
      "var t = new Int32Array(1);
       t[0] = { valueOf: new Proxy(function() { return 42 }, {}) };
       var out = t[0]",
      "out",
    )
    == JsNumber(Finite(42.0))
}

pub fn store_throws_on_non_callable_to_primitive_test() {
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
