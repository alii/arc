import arc/engine.{Returned}

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

// includes and indexOf against a plain loop written with the spec's own
// comparisons, over arrays and values that compare in every different way
pub fn includes_and_index_of_agree_with_a_plain_loop_test() {
  assert run(
      "
  var obj = {}, sym = Symbol('s');
  var values = [0, -0, 1, 1.5, NaN, Infinity, -Infinity, '', 'a', 'é', '1', true, false, null,
    undefined, obj, {}, sym, Symbol('s'), 10n, 10, 4294967296, 1e21];
  var holey = [1, , 3, undefined]; var long = []; for (var i = 0; i < 100; i++) long.push(i % 7 === 0 ? 'x' + i : i);
  var withProp = [1, 2, 3]; withProp.extra = 1;
  var getter = [1, 2, 3]; Object.defineProperty(getter, 1, { get() { return 'got'; } });
  var inherited = [1, , 3]; Object.setPrototypeOf(inherited, Object.assign(Object.create(Array.prototype), { 1: 'up' }));
  var sparse = []; sparse[5000] = 'far';
  var arrays = [values, [], [NaN], [0], [-0], holey, long, withProp, getter, inherited, sparse,
    values.slice().reverse(), [1, 1.0, '1'], [obj, obj]];
  var searches = values.concat(['got', 'up', 'far', 'x98', 99, 'x0']);
  function sameValueZero(a, b) { return a === b || (a !== a && b !== b); }
  var wrong = [];
  for (var a = 0; a < arrays.length; a++) {
    for (var s = 0; s < searches.length; s++) {
      var arr = arrays[a], v = searches[s], inc = false, idx = -1;
      for (var i = 0; i < arr.length; i++) {
        if (!inc && sameValueZero(arr[i], v)) inc = true;
        if (idx < 0 && i in arr && arr[i] === v) idx = i;
      }
      if (arr.includes(v) !== inc) wrong.push('includes ' + a + ':' + String(v));
      if (arr.indexOf(v) !== idx) wrong.push('indexOf ' + a + ':' + String(v));
    }
  }
  wrong.push([1, 2, 3].includes(1, 1), [1, 2, 3].indexOf(3, -1), [1, 2].includes(), [undefined].includes(),
    Array.prototype.includes.call({ length: 2, 0: 'a', 1: 'b' }, 'b'), Array.prototype.indexOf.call('abc', 'c'));
  wrong.join()
  ",
    )
    == "'false,2,false,true,true,2'"
}
