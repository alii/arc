import arc/engine.{Returned}

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

// obj[key] answers in the loop where it can; Reflect.get always takes the
// full lookup, so the two must agree on every pair
pub fn computed_read_agrees_with_reflect_get_test() {
  assert run(
      "
  var sym = Symbol('own'), up = Symbol('up'), acc = Symbol('acc'), reg = Symbol.for('reg');
  var proto = { [up]: 'from proto', get [acc]() { return 'got ' + (this === proto); } };
  class K { static [sym] = 'static'; [sym]() { return 1; } }
  var buf = new ArrayBuffer(16);
  new Uint8Array(buf).set([1, 2, 3, 4, 5, 6, 7, 8, 250, 251, 252, 253, 254, 255, 0, 9]);
  var growable = new ArrayBuffer(4, { maxByteLength: 16 });
  var tracking = new Uint8Array(growable), fixedOnGrowable = new Uint8Array(growable, 0, 4);
  tracking.set([9, 8, 7, 6]);
  var gone = new ArrayBuffer(8), onGone = new Uint8Array(gone); gone.transfer();
  var shrunk = new ArrayBuffer(8, { maxByteLength: 8 }), onShrunk = new Uint8Array(shrunk, 0, 8); shrunk.resize(2);
  var tagged = new Uint8Array(2); tagged[sym] = 'on typed'; tagged.named = 'n';
  var traps = [];
  var proxy = new Proxy({ [sym]: 'target' }, { get(t, k, r) { traps.push(k); return Reflect.get(t, k, r); } });
  var objects = [
    { [sym]: 'own', a: 1 }, { a: 1 }, Object.create(proto), Object.create(Object.create(proto)),
    Object.create(null), K, new K(), [1, 2], function () {}, new Map(), proxy, Object.create(proxy),
    new Uint8Array(buf), new Int8Array(buf), new Uint8ClampedArray(buf), new Uint16Array(buf),
    new Int16Array(buf, 2, 3), new Uint32Array(buf), new Int32Array(buf, 4), new Float32Array(buf),
    new Float64Array(buf), new BigInt64Array(buf), new BigUint64Array(buf, 8), new Uint8Array(0),
    tracking, fixedOnGrowable, onGone, onShrunk, tagged, Object.create(new Uint8Array(buf)),
    { [reg]: 'registered' }, globalThis, new String('abc'), (function () { return arguments; })(7, 8),
  ];
  var keys = [sym, up, acc, reg, Symbol.iterator, Symbol.toStringTag, Symbol('fresh'),
    0, 1, 2, 3, 4, 7, 8, 15, 16, 100, 4294967294, 4294967295, 4294967296, -1, -0, 1.5, NaN, Infinity,
    '0', '1', '15', '16', '-0', '1.5', '01', 'NaN', 'Infinity', 'length', 'named', 'a', 'byteLength'];
  var wrong = [];
  function check() {
    for (var o = 0; o < objects.length; o++) {
      for (var k = 0; k < keys.length; k++) {
        var obj = objects[o], key = keys[k];
        var direct, full;
        try { direct = obj[key]; } catch (e) { direct = e.constructor.name; }
        try { full = Reflect.get(obj, key); } catch (e) { full = e.constructor.name; }
        if (!Object.is(direct, full)) wrong.push(o + ':' + String(key));
      }
    }
  }
  check();
  growable.resize(12); check();
  growable.resize(2); check();
  wrong.join(' ') + '|' + (traps.length > 0)
  ",
    )
    == "'|true'"
}

pub fn typed_array_reads_have_the_right_values_test() {
  assert run(
      "
  var buf = new ArrayBuffer(8);
  new Uint8Array(buf).set([255, 1, 128, 0, 0, 0, 240, 63]);
  var out = [];
  function first(a) { var i = 0; return a[i]; }
  function last(a) { var i = a.length - 1; return a[i]; }
  [Uint8Array, Int8Array, Uint8ClampedArray, Uint16Array, Int16Array, Uint32Array, Int32Array,
   Float32Array, Float64Array, BigInt64Array, BigUint64Array].forEach(function (T) {
    var a = new T(buf); out.push(String(first(a)) + '/' + String(last(a)));
  });
  out.join()
  ",
    )
    == "'255/63,-1/63,255/63,511/16368,511/16368,8389119/1072693248,8389119/1072693248,1.1755659571738145e-38/1.875,1.0000000018627586/1.0000000018627586,4607182418808406527/4607182418808406527,4607182418808406527/4607182418808406527'"
}
