import arc/parser
import arc/parser/ast
import gleam/bit_array
import gleam/list

fn slice_span(source: String, span: ast.Span) -> String {
  let bytes = bit_array.from_string(source)
  let assert Ok(slice) =
    bit_array.slice(bytes, span.start, span.end - span.start)
  let assert Ok(text) = bit_array.to_string(slice)
  text
}

fn import_specifiers(source: String) -> List(ast.ImportSpecifier) {
  let assert Ok(#(ast.Module(items), _sb)) = parser.parse(source, parser.Module)
  let assert Ok(specs) =
    list.find_map(items, fn(item) {
      case item {
        ast.ImportDeclaration(specifiers:, ..) -> Ok(specifiers)
        _ -> Error(Nil)
      }
    })
  specs
}

fn export_specifiers(source: String) -> List(ast.ExportSpecifier) {
  let assert Ok(#(ast.Module(items), _sb)) = parser.parse(source, parser.Module)
  let assert Ok(specs) =
    list.find_map(items, fn(item) {
      case item {
        ast.ExportNamed(specifiers:, ..) -> Ok(specifiers)
        _ -> Error(Nil)
      }
    })
  specs
}

pub fn import_named_local_span_test() {
  let source = "import { foo as bar } from \"mod\";\n"
  let assert [ast.ImportNamedSpecifier(imported:, local:, local_span:)] =
    import_specifiers(source)
  assert imported == "foo"
  assert local == "bar"
  assert slice_span(source, local_span) == "bar"
}

pub fn import_named_no_alias_local_span_test() {
  let source = "import { baz } from \"mod\";\n"
  let assert [ast.ImportNamedSpecifier(imported:, local:, local_span:)] =
    import_specifiers(source)
  assert imported == "baz"
  assert local == "baz"
  assert slice_span(source, local_span) == "baz"
}

pub fn import_default_local_span_test() {
  let source = "import theDefault from \"mod\";\n"
  let assert [ast.ImportDefaultSpecifier(local:, local_span:)] =
    import_specifiers(source)
  assert local == "theDefault"
  assert slice_span(source, local_span) == "theDefault"
}

pub fn import_namespace_local_span_test() {
  let source = "import * as ns from \"mod\";\n"
  let assert [ast.ImportNamespaceSpecifier(local:, local_span:)] =
    import_specifiers(source)
  assert local == "ns"
  assert slice_span(source, local_span) == "ns"
}

pub fn export_named_local_span_test() {
  let source = "const local = 1;\nexport { local as exported };\n"
  let assert [ast.ExportSpecifier(local:, exported:, local_span:)] =
    export_specifiers(source)
  assert local == "local"
  assert exported == "exported"
  assert slice_span(source, local_span) == "local"
}

pub fn export_named_no_alias_local_span_test() {
  let source = "const value = 1;\nexport { value };\n"
  let assert [ast.ExportSpecifier(local:, exported:, local_span:)] =
    export_specifiers(source)
  assert local == "value"
  assert exported == "value"
  assert slice_span(source, local_span) == "value"
}
