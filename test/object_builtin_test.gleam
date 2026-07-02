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

pub fn from_entries_ignores_object_prototype_get_pollution_test() {
  // §7.3.5 CreateDataProperty builds the Property Descriptor RECORD directly
  // and never runs ToPropertyDescriptor over a descriptor OBJECT, so a plain
  // `get` on %Object.prototype% (legal user code — MooTools-era mixins) must
  // be invisible. A funnel that materializes a real POJO descriptor and
  // re-parses it inherits `get` and throws "Getter must be a function".
  assert eval_js(
      "Object.prototype.get = 42;
       var r = JSON.stringify(Object.fromEntries([['a', 1]]));
       delete Object.prototype.get;
       r;",
    )
    == JsString("{\"a\":1}")
}

pub fn from_entries_ignores_object_prototype_set_pollution_test() {
  // Same as above for an inherited callable `set` — must not be seen as an
  // accessor field on the internal data-property descriptor.
  assert eval_js(
      "Object.prototype.set = function () {};
       var r = JSON.stringify(Object.fromEntries([['a', 1]]));
       delete Object.prototype.set;
       r;",
    )
    == JsString("{\"a\":1}")
}

pub fn group_by_ignores_object_prototype_get_pollution_test() {
  // Object.groupBy funnels through the same CreateDataPropertyOrThrow.
  assert eval_js(
      "Object.prototype.get = function () { throw new Error('boom'); };
       var r = JSON.stringify(Object.groupBy([1, 2], function () { return 'k'; }));
       delete Object.prototype.get;
       r;",
    )
    == JsString("{\"k\":[1,2]}")
}
