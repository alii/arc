import arc/esm
import arc/parser
import arc/parser/ast
import gleam/list

fn requested(source: String) -> List(#(String, esm.Phase)) {
  let assert Ok(#(ast.Module(items), _sb)) = parser.parse(source, parser.Module)
  esm.analyze(items).requested
  |> list.map(fn(request) { #(esm.raw_text(request.specifier), request.phase) })
}

pub fn empty_re_export_requests_source_test() {
  assert requested("export {} from \"./m.mjs\";\n")
    == [#("./m.mjs", esm.Evaluation)]
}

pub fn re_export_requests_source_once_test() {
  assert requested("export { a, b as c } from \"./m.mjs\";\n")
    == [#("./m.mjs", esm.Evaluation)]
}

pub fn local_export_requests_nothing_test() {
  assert requested("export const a = 1;\nexport {} ;\n") == []
}

pub fn export_star_requests_source_test() {
  assert requested(
      "export * from \"./m.mjs\";\nexport * as ns from \"./n.mjs\";\n",
    )
    == [#("./m.mjs", esm.Evaluation), #("./n.mjs", esm.Evaluation)]
}

pub fn defer_then_re_export_is_eager_test() {
  assert requested(
      "import defer * as ns from \"./m.mjs\";\nexport {} from \"./m.mjs\";\n",
    )
    == [#("./m.mjs", esm.Evaluation)]
}
