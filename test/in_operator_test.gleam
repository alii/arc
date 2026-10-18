import arc/engine.{Returned}
import arc/interp/kernel
import arc/rt/obj as rt_obj
import arc/rt/types.{mk_int, mk_string}
import arc/rt/val.{is_miss}
import rt_helpers

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

// `in` answers in the loop where it can; Reflect.has always takes the full
// lookup, so the two must agree on every pair
pub fn in_agrees_with_reflect_has_test() {
  assert run(
      "
  class Base { inherited() {} get viaGetter() { return 1; } static s() {} }
  class Derived extends Base { constructor() { super(); this.own = 1; } }
  function fn(a, b) {}
  var deleted = { gone: 1, stays: 2 }; delete deleted.gone;
  var many = {}; for (var i = 0; i < 100; i++) many['k' + i] = i;
  var holey = [1, , 3];
  var patched = [1, 2]; Object.defineProperty(patched, 1, { get() { return 9; } });
  var seen = [];
  var proxy = new Proxy({ real: 1 }, { has(t, k) { seen.push(k); return k === 'fake'; } });
  var overProxy = Object.create(proxy);
  var sym = Symbol('s');
  var objects = [
    { alpha: 1, beta: undefined }, Object.create(null), Object.create({ up: 1 }),
    new Derived(), Derived, Base, fn, () => 1, fn.bind(null), deleted, many,
    [1, 2, 3], holey, patched, [], new String('abc'), new Uint8Array(2),
    new Map(), /x/g, new Date(0), globalThis, Object.prototype, proxy, overProxy,
    { [sym]: 1 }, { 0: 'a', 1: 'b' }, (function () { return arguments; })(1, 2),
    Object.freeze({ frozen: 1 }), { get acc() { return 1; } }, { __proto__: null, x: 1 },
  ];
  var keys = [
    'alpha', 'beta', 'up', 'own', 'inherited', 'viaGetter', 's', 'gone', 'stays',
    'k0', 'k99', 'k100', 'length', 'name', 'prototype', 'constructor', 'toString',
    'hasOwnProperty', '__proto__', 'real', 'fake', 'frozen', 'acc', 'x', 'lastIndex',
    'size', 'Array', 'undefined', '', '0', '1', '2', '3', '-0', '1.5', '01',
    '4294967295', 0, 1, 2, 3, -1, 1.5, 4294967294, 4294967295, sym, Symbol.iterator,
    null, undefined, true, { toString() { return 'alpha'; } },
  ];
  var wrong = [];
  for (var o = 0; o < objects.length; o++) {
    for (var k = 0; k < keys.length; k++) {
      var obj = objects[o], key = keys[k];
      if ((key in obj) !== Reflect.has(obj, key)) wrong.push(o + ':' + String(key));
    }
  }
  wrong.join(' ') + '|' + (seen.length > 0)
  ",
    )
    == "'|true'"
}

pub fn in_still_throws_where_it_must_test() {
  assert run(
      "
  var out = [];
  function attempt(f) { try { out.push(String(f())); } catch (e) { out.push(e.constructor.name); } }
  attempt(function () { var k = 'a', o = 1; return k in o; });
  attempt(function () { var k = 'a', o = 'str'; return k in o; });
  attempt(function () { var k = 'a', o = null; return k in o; });
  attempt(function () { var k = 'a', o; return k in o; });
  attempt(function () { var o = {}; var r = k in o; let k = 'a'; return r; });
  attempt(function () { var k = 'a'; var r = k in o; let o = {}; return r; });
  attempt(function () { var k = { toString() { throw new RangeError('x'); } }, o = {}; return k in o; });
  out.join()
  ",
    )
    == "'TypeError,TypeError,TypeError,TypeError,ReferenceError,ReferenceError,RangeError'"
}

pub fn kernel_answers_plain_chains_and_misses_the_rest_test() {
  let st = rt_helpers.agent()
  let #(obj, st) = rt_obj.new_object_literal(st)
  assert !is_miss(kernel.has_property(st, mk_string("toString"), obj))
  assert !is_miss(kernel.has_property(st, mk_string("absent"), obj))
  assert is_miss(kernel.has_property(st, mk_int(0), obj))
  assert is_miss(kernel.has_property(st, obj, obj))
  assert is_miss(kernel.has_property(st, mk_string("a"), mk_int(1)))
}
