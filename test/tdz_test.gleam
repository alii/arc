import arc/compiler
import arc/dis
import arc/engine.{Returned}
import gleam/string

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

fn dis_js(source: String) -> String {
  let assert Ok(template) =
    compiler.compile_source(compiler.ScriptSource, source)
  dis.disassemble(template)
}

// a let nothing can reach early is not first marked as uninitialized
pub fn a_let_declared_before_any_use_has_no_marker_test() {
  let text =
    dis_js("function f(a) { let x = a + 1; { const y = x; return y } }")
  assert !string.contains(text, "<uninitialized>")
  let text = dis_js("function f(a) { x; let x = a + 1; }")
  assert string.contains(text, "<uninitialized>")
}

pub fn every_early_use_still_throws_test() {
  assert run(
      "
  var out = [];
  function attempt(f) { try { out.push(String(f())); } catch (e) { out.push(e.constructor.name); } }
  attempt(() => { x; let x = 1; return x; });
  attempt(() => { x = 2; let x = 1; return x; });
  attempt(() => { typeof x; let x = 1; return x; });
  attempt(() => { let x = x; return x; });
  attempt(() => { let x = x + 1; return x; });
  attempt(() => { const x = (() => x)(); return x; });
  attempt(() => { let a = b, b = 1; return a; });
  attempt(() => { let { a = a } = {}; return a; });
  attempt(() => { let [a = b, b] = []; return a; });
  attempt(() => { { y; } let y = 1; return y; });
  attempt(() => { const f = () => z; f(); let z = 1; return z; });
  attempt(() => { g(); let w = 1; function g() { return w; } return w; });
  attempt(() => { switch (1) { case 0: let s = 1; case 1: return s; } });
  attempt(() => { switch (1) { case 0: let s = 1; break; case 1: s = 2; return s; } });
  attempt(() => { for (var i = 0; i < 2; i++) { if (i == 1) q; let q = i; } return 'no'; });
  attempt(() => { for (var i = 0; i < 2; i++) { if (i == 1) q = 5; let q = i; } return 'no'; });
  attempt(() => { eval('e'); let e = 1; return e; });
  attempt(() => { x++; let x = 1; return x; });
  attempt(() => { [x] = [1]; let x; return x; });
  attempt(() => { for (let k of [k]) {} return 'no'; });
  attempt(() => { for (let k = k; ;) { return 'no'; } });
  out.join(' ')
  ",
    )
    == "'" <> string.repeat("ReferenceError ", 20) <> "ReferenceError'"
}

pub fn later_uses_see_the_value_test() {
  assert run(
      "
  var out = [];
  function attempt(f) { try { out.push(JSON.stringify(f())); } catch (e) { out.push(e.constructor.name); } }
  attempt(() => { let x; return [x]; });
  attempt(() => { let r = []; for (var i = 0; i < 3; i++) { let x; r.push(x); x = i; } return r; });
  attempt(() => { let r = []; for (var i = 0; i < 3; i++) { let x = i, y = x + 1; r.push(x + y); } return r; });
  attempt(() => { { let a = 1; } { let b; return [b]; } });
  attempt(() => { { let a = 1; } { let b = 2, c; return [b, c]; } });
  attempt(() => { let fs = []; for (var i = 0; i < 2; i++) { let x = i; fs.push(() => x); } return fs.map(f => f()); });
  attempt(() => { const { a, b: [c] } = { a: 1, b: [2] }; return [a, c]; });
  attempt(() => { let x = 1; { let x = 2; } return x; });
  attempt(() => { let n = 0; function* g() { let a = 1; yield a; let b = a + 1; yield b; } for (const v of g()) n += v; return n; });
  attempt(() => { try { throw 1 } catch (e) { let x = e; return x; } });
  attempt(() => { switch (0) { case 0: let s = 1; case 1: return s; } });
  out.join(' ')
  ",
    )
    == "'[null] [null,null,null] [1,3,5] [null] [2,null] [0,1] [1,2] 1 3 1 1'"
}
