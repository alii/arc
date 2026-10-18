import arc/engine.{Returned}

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

// {...source} against the same copy written out with Reflect, twice over so
// that the second round finds the shapes the first one made
pub fn spread_agrees_with_a_copy_written_out_test() {
  assert run(
      "
  var sym = Symbol('s');
  class K { constructor() { this.x = 1; this.y = 2; } get z() { return 3; } }
  var hidden = Object.defineProperty({ a: 1 }, 'h', { value: 2, enumerable: false });
  var getter = { a: 1, get g() { return 'got'; }, b: 2 };
  var deleted = { a: 1, b: 2, c: 3 }; delete deleted.b;
  var many = {}; for (var i = 0; i < 100; i++) many['k' + i] = i;
  var proto = JSON.parse('{\"__proto__\": 1, \"a\": 2}');
  var log = [];
  var proxy = new Proxy({ p: 1, q: 2 }, { ownKeys(t) { log.push('keys'); return Reflect.ownKeys(t); }, get(t, k) { log.push(String(k)); return t[k]; } });
  var sources = [{}, { a: 1 }, { a: 1, b: 2, c: 3 }, { b: 2, a: 1 }, new K(), hidden, getter, deleted, many, proto, proxy,
    { [sym]: 's', a: 1 }, { 1: 'one', 0: 'zero', a: 'a' }, [1, 2], 'str', 5, null, undefined, true, Object.create({ up: 1 }),
    Object.freeze({ f: 1 }), function () {}, new Map([[1, 2]])];
  function copy(target, source) {
    if (source == null) return target;
    var from = Object(source), keys = Reflect.ownKeys(from);
    for (var i = 0; i < keys.length; i++) {
      var d = Reflect.getOwnPropertyDescriptor(from, keys[i]);
      if (d && d.enumerable) Object.defineProperty(target, keys[i], { value: from[keys[i]], writable: true, enumerable: true, configurable: true });
    }
    return target;
  }
  function show(o) {
    return Reflect.ownKeys(o).map(function (k) { var d = Reflect.getOwnPropertyDescriptor(o, k);
      return String(k) + '=' + String(d.value) + (d.writable ? 'w' : '') + (d.enumerable ? 'e' : '') + (d.configurable ? 'c' : ''); }).join() + '|' + (Object.getPrototypeOf(o) === Object.prototype);
  }
  var wrong = [];
  for (var round = 0; round < 3; round++) {
    for (var s = 0; s < sources.length; s++) {
      var src = sources[s];
      var cases = [[{ ...src }, copy({}, src)], [{ ...src, a: 'after' }, copy({}, src)], [{ a: 'before', z: 0, ...src }, copy({ a: 'before', z: 0 }, src)],
        [{ ...src, ...src }, copy(copy({}, src), src)], [{ ...src, ...{ a: 9, n: 1 } }, copy(copy({}, src), { a: 9, n: 1 })]];
      cases[1][1].a = 'after';
      for (var c = 0; c < cases.length; c++) if (show(cases[c][0]) !== show(cases[c][1])) wrong.push(round + ':' + s + ':' + c);
    }
  }
  var a = { x: 1, y: 2 }, b = { ...a }; b.x = 10; b.w = 3;
  wrong.push(a.x, Object.keys(a).join(''), b.x, Object.keys(b).join(''));
  wrong.join()
  ",
    )
    == "'1,xy,10,xyw'"
}
