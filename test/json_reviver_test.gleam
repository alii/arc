/// JSON.parse reviver — §25.5.1.1 InternalizeJSONProperty.
import arc/engine.{Returned, Threw}
import arc/vm/value.{type JsValue, JsString}

/// Helper: eval source on a fresh engine, assert normal completion, return
/// the completion value.
fn eval(source: String) -> JsValue {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  value
}

/// Helper: eval source, assert it threw, return the thrown value.
fn eval_throw(source: String) -> JsValue {
  let assert Ok(#(Threw(error:), _)) = engine.eval(engine.new(), source)
  error
}

pub fn reviver_transforms_values_test() {
  // The reviver visits every value bottom-up; numbers get doubled.
  assert eval(
      "JSON.stringify(JSON.parse('{\"a\":1,\"b\":{\"c\":2}}',
         function (k, v) { return typeof v === 'number' ? v * 2 : v; }))",
    )
    == JsString("{\"a\":2,\"b\":{\"c\":4}}")
}

pub fn reviver_undefined_deletes_key_test() {
  assert eval(
      "JSON.stringify(JSON.parse('{\"a\":1,\"b\":2}',
         function (k, v) { return k === 'a' ? undefined : v; }))",
    )
    == JsString("{\"b\":2}")
}

pub fn reviver_undefined_deletes_array_element_test() {
  // A deleted array index becomes a hole — length is unchanged and the hole
  // stringifies as null.
  assert eval(
      "var a = JSON.parse('[1,2,3]',
         function (k, v) { return v === 2 ? undefined : v; });
       a.length + '/' + (1 in a) + '/' + JSON.stringify(a)",
    )
    == JsString("3/false/[1,null,3]")
}

pub fn reviver_receives_holder_as_this_test() {
  // The root call's holder is { "": parsedValue }; nested calls get the
  // containing object/array.
  assert eval(
      "var seen = [];
       JSON.parse('{\"a\":1}', function (k, v) {
         seen.push(k + ':' + JSON.stringify(this));
         return v;
       });
       seen.join('|')",
    )
    == JsString("a:{\"a\":1}|:{\"\":{\"a\":1}}")
}

pub fn reviver_root_key_is_empty_string_test() {
  assert eval(
      "JSON.parse('7', function (k, v) { return k === '' ? 'root' : 'nested'; })",
    )
    == JsString("root")
}

pub fn reviver_throw_propagates_test() {
  assert eval_throw("JSON.parse('{\"a\":1}', function () { throw 'boom'; })")
    == JsString("boom")
}

pub fn non_callable_reviver_is_ignored_test() {
  // §25.5.1 step 7 gates on IsCallable — a non-callable second argument is
  // simply not a reviver.
  assert eval("JSON.stringify(JSON.parse('{\"a\":1}', 42))")
    == JsString("{\"a\":1}")
  assert eval("JSON.stringify(JSON.parse('{\"a\":1}', {}))")
    == JsString("{\"a\":1}")
}

pub fn reviver_can_replace_with_object_test() {
  assert eval(
      "JSON.stringify(JSON.parse('[1]',
         function (k, v) { return k === '0' ? { n: v } : v; }))",
    )
    == JsString("[{\"n\":1}]")
}
