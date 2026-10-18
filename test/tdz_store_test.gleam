import arc/engine.{Returned}

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

// the compiler drops the uninitialised mark where nothing can see it; every
// way of seeing it must still throw
pub fn reads_before_initialisation_still_throw_test() {
  assert run(
      "
  var out = [];
  function attempt(f) { try { out.push(String(f())); } catch (e) { out.push(e.constructor.name); } }
  function thrower() { throw new RangeError('x'); }
  attempt(function () { let x = x; return x; });
  attempt(function () { let x = x + 1; return x; });
  attempt(function () { const x = typeof x; return x; });
  attempt(function () { x = 1; let x; return x; });
  attempt(function () { let r = x; let x = 1; return r; });
  attempt(function () { let a = 1, b = c, c = 2; return b; });
  attempt(function () { let x = (function () { return x; })(); return x; });
  attempt(function () { let x = [1].map(function () { return x; }); return x; });
  attempt(function () { let f = function () { return y; }; let r = f(); let y = 1; return r; });
  attempt(function () { switch (1) { case 0: let x = 1; case 1: return x; } });
  attempt(function () { switch (0) { case 0: let x = 1; case 1: return x; } });
  attempt(function () { try { thrower(); let x = 1; } catch (e) { return typeof x; } });
  attempt(function () { try { thrower(); } catch (e) { return x; } let x = 1; });
  attempt(function () { try { let x = thrower(); } catch (e) { let y = 2; return y; } });
  attempt(function () { for (var i = 0; i < 2; i++) { if (i == 1) return x; let x = i; } });
  attempt(function () { var seen = []; for (var i = 0; i < 3; i++) { let x = i * 2; seen.push(x); } return seen; });
  attempt(function () { var seen = []; for (var i = 0; i < 3; i++) { let x; seen.push(x); x = i; } return seen; });
  attempt(function () { let x = eval('x'); return x; });
  attempt(function () { let x = eval('1 + 1'); return x; });
  attempt(function () { class C { static v = C.name; } return C.v; });
  attempt(function () { class C extends (function () { return C; })() { } });
  attempt(function () { let { a = b, b } = {}; return a; });
  attempt(function () { let [a, b = a] = [1]; return a + b; });
  attempt(function (a = b, b) { return a; });
  out.join()
  ",
    )
    == "'ReferenceError,ReferenceError,ReferenceError,ReferenceError,ReferenceError,ReferenceError,ReferenceError,ReferenceError,ReferenceError,ReferenceError,1,undefined,ReferenceError,2,ReferenceError,0,2,4,,,,ReferenceError,2,C,ReferenceError,ReferenceError,2,ReferenceError'"
}

pub fn async_and_generator_bodies_test() {
  assert run(
      "
  var out = [];
  async function a() { const x = await 1; const y = await Promise.resolve(x + 1); return x + y; }
  async function b() { try { const x = await Promise.reject(new RangeError('r')); return x; } catch (e) { const z = e.constructor.name; return z; } }
  async function c() { try { const r = x; const x = await 1; return r; } catch (e) { return e.constructor.name; } }
  function* g() { let x = yield 1; let y = yield x; return x + y; }
  var it = g(); it.next(); it.next(10); out.push(it.next(5).value);
  a().then(function (v) { out.push(v); });
  b().then(function (v) { out.push(v); });
  c().then(function (v) { out.push(v); });
  out
  ",
    )
    == "[ 15, 'ReferenceError', 'RangeError', 3 ]"
}
