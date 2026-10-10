import arc/compiler
import arc/dis
import arc/engine.{Returned}
import gleam/int
import gleam/list
import gleam/string

fn eval_string(source: String) -> String {
  let assert Ok(#(Returned(value), eng)) = engine.eval(engine.new(), source)
  engine.inspect(eng, value)
}

fn names(prefix: String, count: Int) -> List(String) {
  list.index_map(list.repeat(prefix, count), fn(prefix, i) {
    prefix <> int.to_string(i)
  })
}

fn each(names: List(String), write: fn(String, Int) -> String) -> String {
  list.index_map(names, write) |> string.join(" ")
}

// every name is assigned from a closure, so every one needs a box
fn many_boxes(count: Int) -> String {
  let vars = names("v", count)
  let lets = names("l", count)
  "(function () {
    var early;
    try { peek() } catch (error) { early = error.name }
    function peek() { return l1 }
    " <> each(vars, fn(name, i) {
    "var " <> name <> " = " <> int.to_string(i) <> ";"
  }) <> "
    " <> each(lets, fn(name, i) {
    "let " <> name <> " = " <> int.to_string(i) <> ";"
  }) <> "
    function bump() { " <> each(list.append(vars, lets), fn(name, _) {
    name <> "++;"
  }) <> " }
    bump(); bump();
    return [early, v0, v" <> int.to_string(count - 1) <> ", l0, l" <> int.to_string(
    count - 1,
  ) <> ", peek()].join();
  })()"
}

fn disassembly(source: String) -> String {
  let assert Ok(template) =
    compiler.compile_source(compiler.ScriptSource, source)
  dis.disassemble(template)
}

pub fn many_boxed_variables_keep_their_values_test() {
  assert eval_string(many_boxes(100)) == "'ReferenceError,2,101,2,101,3'"
}

pub fn many_boxed_parameters_keep_their_arguments_test() {
  let params = names("p", 70)
  let source = "(function (" <> string.join(params, ", ") <> ") {
      function bump() { " <> each(params, fn(name, _) { name <> "++;" }) <> " }
      bump();
      return p0 + ',' + p69 + ',' + arguments.length;
    })(" <> string.join(names("", 70), ", ") <> ")"
  assert eval_string(source) == "'1,70,70'"
}

pub fn a_scope_with_many_boxes_makes_them_in_one_operation_test() {
  let text = disassembly(many_boxes(100))
  assert string.contains(text, "BoxLocals(")
}

pub fn a_scope_with_few_boxes_compiles_as_before_test() {
  let text = disassembly(many_boxes(8))
  assert string.contains(text, "BoxLocal(")
  assert !string.contains(text, "BoxLocals(")
  assert eval_string(many_boxes(8)) == "'ReferenceError,2,9,2,9,3'"
}
