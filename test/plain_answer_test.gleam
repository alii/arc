import arc/engine.{Returned}

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

// a direct call may be answered without the full call; the same function
// reached through Reflect.apply never is, so the two must agree
pub fn short_answers_agree_with_the_full_call_test() {
  assert run(
      "
  var obj = {}, sym = Symbol('s'), reg = Symbol.for('r');
  var wm = new WeakMap([[obj, 'held']]), m = new Map([['a', 1], [NaN, 'nan'], [0, 'zero'], [obj, 'o']]);
  var st = new Set(['a', NaN, 0, obj]);
  wm.set(sym, 'by symbol');
  var gone = new ArrayBuffer(8), onGone = new Uint8Array(gone); gone.transfer();
  var grow = new ArrayBuffer(4, { maxByteLength: 8 }), tracking = new Uint16Array(grow), fixed = new Uint8Array(grow, 0, 4);
  var revoked = Proxy.revocable([], {}); revoked.revoke();
  var receivers = [wm, m, st, 'héllo', 'abc', '', new String('xyz'), [1, 'a', NaN, obj], [1, , 3], [],
    new Uint8Array(3), onGone, tracking, fixed, obj, null, undefined, 5, new Proxy([], {}), revoked.proxy,
    new Proxy(m, {}), new WeakSet(), Object.create(m)];
  var args = [obj, sym, reg, 'a', NaN, 0, -0, 1, 2, -1, 1.5, 100, '1', undefined, null, {}, [], 1n,
    { valueOf() { return 1; } }];
  var lengthGetter = Object.getOwnPropertyDescriptor(Object.getPrototypeOf(Uint8Array.prototype), 'length').get;
  var wrong = [];
  function same(name, direct, full) {
    var a, b;
    try { a = direct(); } catch (e) { a = e.constructor.name; }
    try { b = full(); } catch (e) { b = e.constructor.name; }
    if (!Object.is(a, b)) wrong.push(name + ' ' + String(a) + ' vs ' + String(b));
  }
  var methods = [[WeakMap.prototype, 'get'], [WeakMap.prototype, 'has'], [Map.prototype, 'get'], [Map.prototype, 'has'],
    [Set.prototype, 'has'], [String.prototype, 'charCodeAt'], [Array.prototype, 'includes'], [Array.prototype, 'indexOf']];
  function check() {
    for (var r = 0; r < receivers.length; r++) {
      for (var i = 0; i < methods.length; i++) {
        for (var a = 0; a < args.length; a++) {
          let recv = receivers[r], f = methods[i][0][methods[i][1]], arg = args[a];
          let holder = { f: f };
          same(methods[i][1] + ':' + r + ':' + a, function () { return f.call(recv, arg); }, function () { return Reflect.apply(f, recv, [arg]); });
        }
      }
      let recv = receivers[r];
      same('isArray:' + r, function () { return Array.isArray(recv); }, function () { return Reflect.apply(Array.isArray, null, [recv]); });
      if (recv != null) same('length:' + r, function () { return recv.length; }, function () { return Reflect.get(Object(recv), 'length', recv); });
    }
  }
  check(); grow.resize(8); check(); grow.resize(1); check();
  // called as methods, which is the shape the loop answers
  wrong.push(wm.get(obj), wm.has(sym), wm.get(reg), m.get(NaN), m.get(-0), m.has('b'), st.has(obj), st.has(-0),
    'héllo'.charCodeAt(1), 'abc'.charCodeAt(3), 'abc'.charCodeAt(-1), [1, NaN].includes(NaN), [1, NaN].indexOf(NaN),
    [1, , 3].includes(undefined), [1, , 3].indexOf(undefined), Array.isArray([]), Array.isArray(new Proxy([], {})),
    new Uint8Array(3).length, onGone.length, tracking.length, fixed.length);
  wrong.join()
  ",
    )
    == "'held,true,,nan,zero,false,true,true,233,NaN,NaN,true,-1,true,-1,true,true,3,0,0,0'"
}
