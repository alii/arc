import lumen/parser

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
