//// Unit tests for `esm.analyze` — the static import/export summary of a
//// parsed module. Focused on [[RequestedModules]] (§16.2.1.3 ModuleRequests),
//// where the mapping from declarations to requests is easy to get wrong.

import arc/esm
import arc/parser
import arc/parser/ast
import gleam/list

fn requested(source: String) -> List(#(String, esm.Phase)) {
  let assert Ok(#(ast.Module(items), _sb)) = parser.parse(source, parser.Module)
  esm.analyze(items).requested
  |> list.map(fn(request) { #(esm.raw_text(request.specifier), request.phase) })
}

/// §16.2.1.3: `export {} from "m"` re-exports nothing but STILL requests `m`,
/// so the dependency is fetched, linked and evaluated for its side effects.
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

/// An eager reference forces evaluation phase even when a `defer` import of
/// the same specifier came first.
pub fn defer_then_re_export_is_eager_test() {
  assert requested(
      "import defer * as ns from \"./m.mjs\";\nexport {} from \"./m.mjs\";\n",
    )
    == [#("./m.mjs", esm.Evaluation)]
}
