import arc/engine.{Returned}
import arc/vm/value.{JsString}

// ----------------------------------------------------------------------------
// Object builtins that funnel result properties through
// CreateDataPropertyOrThrow (§7.3.7): Object.groupBy, Object.fromEntries,
// plus Object.assign's throwing [[Set]] (§20.1.2.1 step 3.a.iii.2.b).
// ----------------------------------------------------------------------------

/// Helper: eval on a fresh engine, assert normal completion, return the value.
fn eval_js(source: String) -> value.JsValue {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  value
}

pub fn group_by_keys_are_enumerable_test() {
  // Each group is a writable/enumerable/configurable data property, so
  // Object.keys sees it (it used to be created non-enumerable → []).
  assert eval_js(
      "Object.keys(Object.groupBy([1], function (x) { return 'a'; })).join(',')",
    )
    == JsString("a")
}

pub fn group_by_first_occurrence_order_test() {
  // Group keys appear in first-occurrence order, not sorted/dict order.
  assert eval_js(
      "Object.keys(Object.groupBy([1, 2, 3, 4], function (x) { return x % 2 ? 'odd' : 'even'; })).join('|')",
    )
    == JsString("odd|even")
}

pub fn group_by_descriptor_shape_test() {
  assert eval_js(
      "var g = Object.groupBy(['x'], function () { return 'k'; });
       var d = Object.getOwnPropertyDescriptor(g, 'k');
       '' + d.writable + ',' + d.enumerable + ',' + d.configurable + ',' + d.value.join('');",
    )
    == JsString("true,true,true,x")
}

pub fn group_by_symbol_key_test() {
  // GroupBy uses ToPropertyKey, so a symbol groups under the symbol itself.
  assert eval_js(
      "var s = Symbol('k');
       var g = Object.groupBy([1, 2], function (x) { return x === 1 ? s : 'other'; });
       g[s].join(',') + '|' + Object.keys(g).join(',');",
    )
    == JsString("1|other")
}

pub fn assign_to_frozen_target_throws_test() {
  // §20.1.2.1 step 3.a.iii.2.b: Perform ? Set(to, k, v, true) — a false
  // [[Set]] (frozen target) throws instead of being silently discarded.
  assert eval_js(
      "(function () {
         try { Object.assign(Object.freeze({ x: 1 }), { x: 2 }); return 'no throw'; }
         catch (e) { return e.name; }
       })()",
    )
    == JsString("TypeError")
}

pub fn from_entries_duplicate_key_keeps_position_test() {
  // CreateDataPropertyOrThrow per entry: a duplicate key overwrites the
  // value but keeps its original insertion position.
  assert eval_js(
      "var o = Object.fromEntries([['a', 1], ['b', 2], ['a', 3]]);
       Object.keys(o).join(',') + '|' + o.a + o.b;",
    )
    == JsString("a,b|32")
}
