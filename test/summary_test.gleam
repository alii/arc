import arc/module/specifier
import arc/module/summary
import arc/parser
import arc/parser/ast
import gleam/list

fn requested(source: String) -> List(#(String, summary.Phase)) {
  let assert Ok(#(ast.Module(items), _sb)) = parser.parse(source, parser.Module)
  summary.analyze(items).requested
  |> list.map(fn(request) {
    #(specifier.raw_text(request.request.specifier), request.phase)
  })
}

pub fn empty_re_export_requests_source_test() {
  assert requested("export {} from \"./m.mjs\";\n")
    == [#("./m.mjs", summary.Evaluation)]
}

pub fn re_export_requests_source_once_test() {
  assert requested("export { a, b as c } from \"./m.mjs\";\n")
    == [#("./m.mjs", summary.Evaluation)]
}

pub fn local_export_requests_nothing_test() {
  assert requested("export const a = 1;\nexport {} ;\n") == []
}

pub fn export_star_requests_source_test() {
  assert requested(
      "export * from \"./m.mjs\";\nexport * as ns from \"./n.mjs\";\n",
    )
    == [#("./m.mjs", summary.Evaluation), #("./n.mjs", summary.Evaluation)]
}

pub fn defer_then_re_export_is_eager_test() {
  assert requested(
      "import defer * as ns from \"./m.mjs\";\nexport {} from \"./m.mjs\";\n",
    )
    == [#("./m.mjs", summary.Evaluation)]
}

fn requests(source: String) -> List(specifier.Request) {
  let assert Ok(#(ast.Module(items), _sb)) = parser.parse(source, parser.Module)
  summary.analyze(items).requested |> list.map(fn(request) { request.request })
}

pub fn attributes_distinguish_requests_test() {
  let json = [ast.ImportAttribute(key: "type", value: "json")]
  assert requests(
      "import a from './m';\nimport b from './m' with { type: 'json' };\nexport * from './m' with { type: 'json' };\n",
    )
    == [
      specifier.Request(specifier.raw("./m"), []),
      specifier.Request(specifier.raw("./m"), json),
    ]
}

pub fn attributes_are_sorted_by_key_test() {
  assert requests("import x from './m' with { b: '1', 'a': '2' };\n")
    == [
      specifier.Request(specifier.raw("./m"), [
        ast.ImportAttribute(key: "a", value: "2"),
        ast.ImportAttribute(key: "b", value: "1"),
      ]),
    ]
}

pub fn duplicate_attribute_keys_are_a_syntax_error_test() {
  let assert Error(err) =
    parser.parse(
      "import x from './m' with { type: 'json', 'typ\\u0065': '' };",
      parser.Module,
    )
  assert parser.error_to_string(err) == "Duplicate import attribute 'type'"
}
