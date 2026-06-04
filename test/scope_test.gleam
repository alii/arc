import arc/parser

fn should_fail(source: String) -> Bool {
  case parser.parse(source, parser.Script) {
    Ok(_) -> False
    Error(_) -> True
  }
}

fn should_pass(source: String) -> Bool {
  case parser.parse(source, parser.Script) {
    Ok(_) -> True
    Error(_) -> False
  }
}

pub fn scope_test() {
  // Cross-statement duplicates (should fail)
  assert should_fail("let a; let a;")
  assert should_fail("let a; var a;")
  assert should_fail("var a; let a;")
  assert should_fail("{ var a; const a = 1; }")
  assert should_fail("{ let a; { var a; } }")

  // Function param + body
  assert should_fail("function a(b){ let b; }")
  assert should_fail("(a) => { let a; }")

  // Switch cases share scope
  assert should_fail("switch(1) { case 2: let a; case 3: let a; }")

  // Catch clause
  assert should_fail("try {} catch(a) { let a; }")

  // for destructuring
  assert should_fail("for(const {a, a} of 1);")

  // Block-level function declarations - duplicates should fail
  assert should_fail("{ function a(){} function a(){} }")
  assert should_fail("{ function a(){} function* a(){} }")

  // Valid cases that should pass
  assert should_pass("function a() {} function a() {}")
  assert should_pass("function a(b, b) {}")
  assert should_pass("function a([b]) { var b; }")
  assert should_pass("var a; var a;")
  assert should_pass("for(let a;;); let a;")
}

/// Function boundaries and the for-head must not leak the enclosing
/// let/const declaration context into params, bodies, or later statements.
pub fn lexical_decl_boundary_test() {
  // Same-named function-expression params inside a for body
  assert should_pass(
    "for (let i = 0; i < 1; i++) { f(function (x) {}); f(function (x) {}); }",
  )
  assert should_pass(
    "for (let x of [1]) { f(function (x) {}); f(function (x) {}); }",
  )
  // Same-named params across initializers of one lexical declaration
  assert should_pass(
    "let f = function (x) { return x; }, g = function (x) { return x; };",
  )
  assert should_pass("let f = (x) => x, g = (x) => x;")
  assert should_pass("let o = { m(x) {} }, p = { m(x) {} };")
  // Arrow params inside destructuring default values
  assert should_pass("let [a = (x) => x, b = (x) => x] = [];")
  // Catch params and var in a for body must not collide with the head
  assert should_pass(
    "for (let i = 0; i < 1; i++) { try {} catch (e) {} try {} catch (e) {} }",
  )
  assert should_pass("for (let i = 0; i < 1; i++) { var a1; var a1; }")
  // Body block may shadow the for-head binding
  assert should_pass("for (let i = 0; i < 1; i++) { let i = 1; }")

  // Duplicate detection must survive a function boundary in an initializer
  assert should_fail("let a = function () {}, a = 2;")
  assert should_fail("let a = (x) => x, a = 2;")
  assert should_fail("for (let a = 1, a = 2;;);")
}
