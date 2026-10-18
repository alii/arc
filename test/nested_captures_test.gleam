import arc/engine.{Returned}

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

// a function with two or more captures keeps them in one tuple, out of its
// frame's slots
pub fn captures_read_write_and_pass_on_test() {
  assert run(
      "
  var out = [];
  function attempt(f) { try { out.push(String(f())); } catch (e) { out.push(e.constructor.name); } }
  (function () {
    let a = 1, b = 2, c = 3; const k = 'k';
    function named() { return typeof named + a + b; }
    attempt(() => a + b + c + k);
    attempt(() => { a += 10; b = a + b; c++; return [a, b, c]; });
    attempt(() => [a, b, c]);
    attempt(() => { k = 'no'; return a + b; });
    attempt(named);
    attempt(() => (() => (() => a + b + c)())());
    attempt(() => { var x = a, y = b, z = c; x += y; y += z; z += x; return [x, y, z]; });
    attempt(function (p = a, q = () => b + c) { var r = p + q(); return r; });
    attempt(function () { return arguments.length + a + b; });
    attempt(() => { var fs = []; for (let i = 0; i < 3; i++) fs.push(() => i + a + b); return fs.map(f => f()); });
    attempt(() => eval('a + b'));
    attempt(() => { var o = { a: 'with' }; return new Function('o', 'with (o) { return a; }')(o) + a + b; });
    attempt(() => early + a + b);
    let early = 'late';
    attempt(() => early + a + b);
    attempt(() => typeof missing + a + b);
    attempt(() => { try { throw a; } catch (e) { return e + b; } finally { c = 100; } });
    attempt(() => c + a);
    attempt(() => named.name + named.length + a + b);
    attempt(() => { var t = 0; for (var i = 0; i < 5; i++) { t += a; t -= b; } return t; });
  })();
  var obj = { v: 5, m(x, y) { var f = () => this.v + x + y; var g = () => () => this.v * x * y; return [f(), g()()]; } };
  attempt(() => obj.m(2, 3));
  class A { constructor(n) { this.n = n; } }
  (function () {
    var p = 1, q = 2;
    class B extends A { constructor() { var init = () => super(p + q); init(); this.z = p; } get sum() { return this.n + p + q; } static s() { return p * q; } #h = p + q; h() { return this.#h; } }
    attempt(() => { var b = new B(); return [b.n, b.z, b.sum, B.s(), b.h()]; });
    class C extends A { constructor() { var twice = () => super(p + q); twice(); twice(); } }
    attempt(() => new C());
  })();
  (function () {
    var s = 1, t = 2, u = 3;
    function* g() { var l = s; yield l + t; l += u; s = 10; yield l + s; return t; }
    attempt(() => [...g()]);
    attempt(() => s);
    async function f() { var l = s + t; await null; l += u; t = 20; await null; out.push('async ' + (l + t)); }
    f();
  })();
  out
  ",
    )
    == "[ '6k', '11,13,4', '11,13,4', 'TypeError', 'function1113', '28', '24,17,28', '28', '24', '24,25,26', '24', 'with1113', 'ReferenceError', 'late1113', 'undefined1113', '24', '111', 'named01113', '-10', '10,30', '3,1,6,2,3', 'ReferenceError', '3,14', '10', 'async 35' ]"
}
