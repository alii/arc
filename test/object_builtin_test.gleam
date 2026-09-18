import arc/engine.{Returned}
import arc/rt/types.{type JsValKind, KStr}
import rt_helpers

fn eval_js(source: String) -> JsValKind {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  rt_helpers.classify(value)
}

pub fn group_by_keys_are_enumerable_test() {
  assert eval_js(
      "Object.keys(Object.groupBy([1], function (x) { return 'a'; })).join(',')",
    )
    == KStr("a")
}

pub fn group_by_first_occurrence_order_test() {
  assert eval_js(
      "Object.keys(Object.groupBy([1, 2, 3, 4], function (x) { return x % 2 ? 'odd' : 'even'; })).join('|')",
    )
    == KStr("odd|even")
}

pub fn group_by_descriptor_shape_test() {
  assert eval_js(
      "var g = Object.groupBy(['x'], function () { return 'k'; });
       var d = Object.getOwnPropertyDescriptor(g, 'k');
       '' + d.writable + ',' + d.enumerable + ',' + d.configurable + ',' + d.value.join('');",
    )
    == KStr("true,true,true,x")
}

pub fn group_by_symbol_key_test() {
  assert eval_js(
      "var s = Symbol('k');
       var g = Object.groupBy([1, 2], function (x) { return x === 1 ? s : 'other'; });
       g[s].join(',') + '|' + Object.keys(g).join(',');",
    )
    == KStr("1|other")
}

pub fn assign_to_frozen_target_throws_test() {
  assert eval_js(
      "(function () {
         try { Object.assign(Object.freeze({ x: 1 }), { x: 2 }); return 'no throw'; }
         catch (e) { return e.name; }
       })()",
    )
    == KStr("TypeError")
}

pub fn from_entries_duplicate_key_keeps_position_test() {
  assert eval_js(
      "var o = Object.fromEntries([['a', 1], ['b', 2], ['a', 3]]);
       Object.keys(o).join(',') + '|' + o.a + o.b;",
    )
    == KStr("a,b|32")
}

pub fn from_entries_ignores_object_prototype_get_pollution_test() {
  assert eval_js(
      "Object.prototype.get = 42;
       var r = JSON.stringify(Object.fromEntries([['a', 1]]));
       delete Object.prototype.get;
       r;",
    )
    == KStr("{\"a\":1}")
}

pub fn from_entries_ignores_object_prototype_set_pollution_test() {
  assert eval_js(
      "Object.prototype.set = function () {};
       var r = JSON.stringify(Object.fromEntries([['a', 1]]));
       delete Object.prototype.set;
       r;",
    )
    == KStr("{\"a\":1}")
}

pub fn group_by_ignores_object_prototype_get_pollution_test() {
  assert eval_js(
      "Object.prototype.get = function () { throw new Error('boom'); };
       var r = JSON.stringify(Object.groupBy([1, 2], function () { return 'k'; }));
       delete Object.prototype.get;
       r;",
    )
    == KStr("{\"k\":[1,2]}")
}
