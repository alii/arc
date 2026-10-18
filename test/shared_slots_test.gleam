import arc/engine.{Returned}

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

// blocks that are never live together share slots; nothing of one block may
// show through in the next
pub fn sibling_blocks_do_not_see_each_other_test() {
  assert run(
      "
  var out = [];
  function attempt(f) { try { out.push(String(f())); } catch (e) { out.push(e.constructor.name); } }
  attempt(function () { { let a = 1; } { let r = b; let b = 2; return r; } });
  attempt(function () { { let a = 1; } { b = 5; let b; return b; } });
  attempt(function () { { let a = 1; } { let b; return b; } });
  attempt(function () { { let a = 1; } { return typeof b; let b; } });
  attempt(function () { { const a = 1; } { let b = 2; b = 3; return b; } });
  attempt(function () { { let a = 1; } { const b = 2; b = 3; return b; } });
  attempt(function () { var fs = []; { let a = 1; fs.push(function () { return a; }); } { let b = 2; fs.push(function () { return b; }); } return fs[0]() + ',' + fs[1](); });
  attempt(function () { var f; { let a = 1; f = function () { return a++; }; } { let b = 100; b++; } f(); return f(); });
  attempt(function () { var fs = []; for (let i = 0; i < 3; i++) { let d = i * 2; fs.push(function () { return i + d; }); } for (let j = 10; j < 12; j++) { let e = j; fs.push(function () { return j + e; }); } return fs.map(function (f) { return f(); }); });
  attempt(function () { var keep = 'kept'; { let a = 1, b = 2, c = 3; } { let d = 4; } return keep; });
  attempt(function (p) { { let a = 1, b = 2; } { let c = 3; } return p; });
  attempt(function () { try { let a = 1; throw a; } catch (e) { let b = e + 1; return b; } finally { let c = 9; out.push(c); } });
  attempt(function () { switch (1) { case 1: { let a = 'one'; out.push(a); } case 2: { let b = 'two'; return b; } } });
  attempt(function () { { let a = 1; { let b = 2; { let c = 3; return a + b + c; } } } });
  attempt(function () { var t = 0; { let a = 1; { let b = 2; t += a + b; } { let c = 3; t += a + c; } t += a; } return t; });
  attempt(function () { { let a = 'A'; } { let b = 'B'; return eval('b'); } });
  attempt(function () { { let a = 'A'; return eval('a'); } { let b = 'B'; } });
  attempt(function () { { function inner() { return 'in'; } } { let z = 1; } return inner(); });
  attempt(function () { { class A { static v = 1; } out.push(A.v); } { class B { static v = 2; } return B.v; } });
  attempt(function () { label: { let a = 1; break label; } { let b; return b; } });
  out.join()
  ",
    )
    == "'ReferenceError,ReferenceError,undefined,ReferenceError,3,TypeError,1,2,2,0,3,6,20,22,kept,undefined,9,2,one,two,6,8,B,A,in,1,2,undefined'"
}

pub fn generators_and_async_keep_their_blocks_apart_test() {
  assert run(
      "
  var out = [];
  function* g() { { let a = yield 1; out.push(a); } { let b = yield 2; out.push(b); } { let c; out.push(c); } }
  var it = g(); it.next(); it.next('a'); it.next('b');
  async function f() { { const a = await 1; out.push(a); } { let b; out.push(b); b = await 2; out.push(b); } }
  f();
  out
  ",
    )
    == "[ 'a', 'b', undefined, 1, undefined, 2 ]"
}
