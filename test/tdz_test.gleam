import arc/engine.{Returned}

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

pub fn a_write_in_a_case_entered_past_the_let_throws_test() {
  assert run(
      "
  (function () {
    try { switch (1) { case 0: let s = 1; break; case 1: s = 2; return s; } }
    catch (e) { return e.constructor.name; }
  })()",
    )
    == "'ReferenceError'"
}
