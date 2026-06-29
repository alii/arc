/// Test262 parser conformance tests.
/// Each test directory runs all .js files in parallel.
import arc/parser
import arc/parser/ast
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile
import test_runner

/// Tests in the fail/ directory that are outdated due to spec changes.
/// These expect parse errors for syntax that is now valid in modern JS.
const outdated_fail_tests = [
  // Class fields (ES2022) — class body with bare field name or field initializer
  // is valid syntax now, but test262-parser-tests predates this spec addition.
  "98204d734f8c72b3.js",
  "ef81b93cf9bdb4ec.js",
  // `func() = 4` — Annex B AssignmentTargetType ~web-compat~: a CallExpression
  // assignment target parses in sloppy mode and throws ReferenceError at
  // runtime. test262 annexB/language/expressions/assignmenttargettype/*
  // requires parse success; this fixture predates the Annex B addition.
  "a8beb1480f385441.js",
]

/// Determine parse mode from filename — files with ".module." parse as module.
fn parse_mode(filename: String) -> parser.ParseMode {
  case string.contains(filename, ".module.") {
    True -> parser.Module
    False -> parser.Script
  }
}

/// For "pass" tests: parsing should succeed.
fn pass_test_fn(filename: String, source: String) -> Result(Nil, String) {
  let mode = parse_mode(filename)
  case parser.parse(source, mode) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(parser.parse_error_to_string(err))
  }
}

/// For "fail" tests: parsing should produce an error.
/// Outdated tests (valid in modern JS) are skipped.
fn fail_test_fn(filename: String, source: String) -> Result(Nil, String) {
  case list.contains(outdated_fail_tests, filename) {
    True -> Ok(Nil)
    False -> {
      let mode = parse_mode(filename)
      case parser.parse(source, mode) {
        Error(_) -> Ok(Nil)
        Ok(_) -> Error("Expected parse error, got success")
      }
    }
  }
}

/// For "early" tests: parsing should produce an early error.
fn early_test_fn(filename: String, source: String) -> Result(Nil, String) {
  let mode = parse_mode(filename)
  case parser.parse(source, mode) {
    Error(_) -> Ok(Nil)
    Ok(_) -> Error("Expected early error, got success")
  }
}

pub fn pass_test() {
  run_file_tests("vendor/test262-parser-tests/pass", pass_test_fn)
}

pub fn fail_test() {
  run_file_tests("vendor/test262-parser-tests/fail", fail_test_fn)
}

pub fn early_test() {
  run_file_tests("vendor/test262-parser-tests/early", early_test_fn)
}

/// Slice `source` by a half-open `[start, end)` byte span and decode as UTF-8.
fn slice_span(source: String, span: ast.Span) -> Result(String, Nil) {
  let bytes = bit_array.from_string(source)
  let ast.Span(start:, end:) = span
  use sliced <- result.try(bit_array.slice(bytes, start, end - start))
  bit_array.to_string(sliced)
}

/// The span on an ImportDeclaration must slice back to the exact original text,
/// from the `import` keyword through the module-specifier string (inclusive of
/// its closing quote), for every import form including non-ASCII source.
pub fn import_declaration_span_round_trip_test() {
  let cases = [
    #("import \"mod\";", "import \"mod\""),
    #("import a from \"mod\";", "import a from \"mod\""),
    #("import * as ns from \"mod\";", "import * as ns from \"mod\""),
    #("import { a, b } from \"mod\";", "import { a, b } from \"mod\""),
    #("import a, { b } from \"mod\";", "import a, { b } from \"mod\""),
    #("import a, * as ns from \"mod\";", "import a, * as ns from \"mod\""),
    #("import defer * as ns from \"mod\";", "import defer * as ns from \"mod\""),
    // Non-ASCII before the import exercises byte-offset (not char) spans.
    #("const π = 1;\nimport \"m\";", "import \"m\""),
  ]
  let errors =
    list.filter_map(cases, fn(c) {
      let #(src, expected) = c
      case import_decl_span_text(src) {
        Ok(text) if text == expected -> Error(Nil)
        Ok(text) -> Ok(src <> " -> got " <> string.inspect(text))
        Error(reason) -> Ok(src <> " -> " <> reason)
      }
    })
  case errors {
    [] -> Nil
    _ -> {
      list.each(errors, fn(e) { io.println("  FAIL: " <> e) })
      panic as {
        int.to_string(list.length(errors)) <> " span round-trips failed"
      }
    }
  }
}

/// Parse `src` as a module, find the first ImportDeclaration, and return the
/// source text its span points at.
fn import_decl_span_text(src: String) -> Result(String, String) {
  use #(program, _sb) <- result.try(
    parser.parse(src, parser.Module)
    |> result.map_error(parser.parse_error_to_string),
  )
  let span = case program {
    ast.Module(body) ->
      list.find_map(body, fn(item) {
        case item {
          ast.ImportDeclaration(span:, ..) -> Ok(span)
          _ -> Error(Nil)
        }
      })
    ast.Script(_) -> Error(Nil)
  }
  use span <- result.try(
    span |> result.replace_error("no ImportDeclaration found"),
  )
  slice_span(src, span)
  |> result.replace_error("span slice did not decode as UTF-8")
}

/// The span on an ExportDefaultDeclaration must slice back to the exact
/// original text, from the `export` keyword through the declaration (or the
/// expression plus its terminating semicolon), for every default-export form
/// including non-ASCII source.
pub fn export_default_span_round_trip_test() {
  let cases = [
    #("export default 1;", "export default 1;"),
    #("export default 1 + 2;", "export default 1 + 2;"),
    // ASI: no semicolon — span ends at the expression.
    #("export default 42", "export default 42"),
    #("export default function () {}", "export default function () {}"),
    #("export default function foo() {}", "export default function foo() {}"),
    #("export default class {}", "export default class {}"),
    #("export default class Foo {}", "export default class Foo {}"),
    #(
      "export default async function () {}",
      "export default async function () {}",
    ),
    #("export default { a: 1 };", "export default { a: 1 };"),
    // Non-ASCII before the export exercises byte-offset (not char) spans.
    #("const π = 1;\nexport default π;", "export default π;"),
  ]
  let errors =
    list.filter_map(cases, fn(c) {
      let #(src, expected) = c
      case export_default_span_text(src) {
        Ok(text) if text == expected -> Error(Nil)
        Ok(text) -> Ok(src <> " -> got " <> string.inspect(text))
        Error(reason) -> Ok(src <> " -> " <> reason)
      }
    })
  case errors {
    [] -> Nil
    _ -> {
      list.each(errors, fn(e) { io.println("  FAIL: " <> e) })
      panic as {
        int.to_string(list.length(errors)) <> " span round-trips failed"
      }
    }
  }
}

/// Parse `src` as a module, find the first ExportDefaultDeclaration, and return
/// the source text its span points at.
fn export_default_span_text(src: String) -> Result(String, String) {
  use #(program, _sb) <- result.try(
    parser.parse(src, parser.Module)
    |> result.map_error(parser.parse_error_to_string),
  )
  let span = case program {
    ast.Module(body) ->
      list.find_map(body, fn(item) {
        case item {
          ast.ExportDefaultDeclaration(span:, ..) -> Ok(span)
          _ -> Error(Nil)
        }
      })
    ast.Script(_) -> Error(Nil)
  }
  use span <- result.try(
    span |> result.replace_error("no ExportDefaultDeclaration found"),
  )
  slice_span(src, span)
  |> result.replace_error("span slice did not decode as UTF-8")
}

/// The `local_span` on each import/export local-binding identifier must slice
/// back to the exact binding text — the name introduced into scope (the alias
/// after `as`), not the imported/exported name. Covers default, namespace,
/// named (aliased and bare), export specifiers, and a non-ASCII binding.
pub fn binding_span_round_trip_test() {
  let cases = [
    // src, kind ("import"/"export"), list of (binding-name, sliced-text)
    #("import a from \"mod\";", "import", [#("a", "a")]),
    #("import * as ns from \"mod\";", "import", [#("ns", "ns")]),
    #("import { a } from \"mod\";", "import", [#("a", "a")]),
    #("import { a as b } from \"mod\";", "import", [#("b", "b")]),
    #("import x, { a, b as c } from \"mod\";", "import", [
      #("x", "x"),
      #("a", "a"),
      #("c", "c"),
    ]),
    // Non-ASCII binding exercises byte-offset (not char) spans.
    #("import { π } from \"mod\";", "import", [#("π", "π")]),
    #("const a = 1;\nexport { a };", "export", [#("a", "a")]),
    #("const a = 1;\nexport { a as b };", "export", [#("a", "a")]),
    #("const π = 1;\nexport { π };", "export", [#("π", "π")]),
  ]
  let errors =
    list.filter_map(cases, fn(c) {
      let #(src, kind, expected) = c
      case binding_span_texts(src, kind) {
        Ok(got) if got == expected -> Error(Nil)
        Ok(got) -> Ok(src <> " -> got " <> string.inspect(got))
        Error(reason) -> Ok(src <> " -> " <> reason)
      }
    })
  case errors {
    [] -> Nil
    _ -> {
      list.each(errors, fn(e) { io.println("  FAIL: " <> e) })
      panic as {
        int.to_string(list.length(errors)) <> " binding span round-trips failed"
      }
    }
  }
}

/// Parse `src` as a module and return, for the first import or export
/// declaration with specifiers, the `(local, sliced-text)` pair of each
/// local-binding identifier sliced from its `local_span`.
fn binding_span_texts(
  src: String,
  kind: String,
) -> Result(List(#(String, String)), String) {
  use #(program, _sb) <- result.try(
    parser.parse(src, parser.Module)
    |> result.map_error(parser.parse_error_to_string),
  )
  let spans = case program, kind {
    ast.Module(body), "import" ->
      list.find_map(body, fn(item) {
        case item {
          ast.ImportDeclaration(specifiers: [_, ..] as specs, ..) ->
            Ok(list.map(specs, import_specifier_binding))
          _ -> Error(Nil)
        }
      })
    ast.Module(body), _ ->
      list.find_map(body, fn(item) {
        case item {
          ast.ExportNamedDeclaration(specifiers: [_, ..] as specs, ..) ->
            Ok(
              list.map(specs, fn(spec) {
                let ast.ExportSpecifier(local:, local_span:, ..) = spec
                #(local, local_span)
              }),
            )
          _ -> Error(Nil)
        }
      })
    ast.Script(_), _ -> Error(Nil)
  }
  use spans <- result.try(
    spans |> result.replace_error("no declaration with specifiers found"),
  )
  list.try_map(spans, fn(pair) {
    let #(name, span) = pair
    slice_span(src, span)
    |> result.map(fn(text) { #(name, text) })
    |> result.replace_error("binding span did not decode as UTF-8")
  })
}

/// The local binding name + its `local_span` for one import specifier.
fn import_specifier_binding(spec: ast.ImportSpecifier) -> #(String, ast.Span) {
  case spec {
    ast.ImportDefaultSpecifier(local:, local_span:) -> #(local, local_span)
    ast.ImportNamespaceSpecifier(local:, local_span:) -> #(local, local_span)
    ast.ImportNamedSpecifier(local:, local_span:, ..) -> #(local, local_span)
  }
}

fn run_file_tests(
  dir: String,
  test_fn: fn(String, String) -> Result(Nil, String),
) {
  case test_runner.list_files(dir) {
    Error(err) -> panic as { "Could not list files in " <> dir <> ": " <> err }
    Ok(files) -> {
      let errors =
        test_runner.run_parallel(files, fn(filename) {
          case simplifile.read(dir <> "/" <> filename) {
            Error(err) -> Error("read error: " <> string.inspect(err))
            Ok(source) -> test_fn(filename, source)
          }
        })
      case errors {
        [] -> Nil
        _ -> {
          list.each(errors, fn(e) {
            let #(file, reason) = e
            io.println("  FAIL: " <> file <> " — " <> reason)
          })
          panic as {
            int.to_string(list.length(errors)) <> " tests failed in " <> dir
          }
        }
      }
    }
  }
}

/// Regression: bindings inside a method *body* are not formal parameters.
/// `in_method` stays true through the whole method body, so the param-dup
/// accumulator must gate on `in_formal_params` only — two sibling
/// `for (const id of …)` loops in a method are distinct block-scoped bindings,
/// not duplicate params. Before the fix the second `id` was wrongly rejected as
/// a duplicate parameter (this is what broke booting a class service whose
/// method reused a loop variable).
pub fn method_body_bindings_are_not_params_test() {
  let results = [
    // The regression: sibling for-of-const in a method / function body parse.
    expect_parses(
      "class C { reset() { for (const id of [1]) { id; } for (const id of [2]) { id; } } }",
      parser.Script,
    ),
    expect_parses(
      "function f() { for (const id of [1]) {} for (const id of [2]) {} }",
      parser.Script,
    ),
    // Genuine duplicate *params* are still rejected — in methods (always) and
    // strict-mode functions — so the fix didn't loosen real dup detection.
    expect_dup_param("class C { m(id, id) {} }", parser.Script),
    expect_dup_param("function f(id, id) { return id; }", parser.Module),
  ]
  let errors =
    list.filter_map(results, fn(r) {
      case r {
        Ok(Nil) -> Error(Nil)
        Error(reason) -> Ok(reason)
      }
    })
  case errors {
    [] -> Nil
    _ -> {
      list.each(errors, fn(e) { io.println("  FAIL: " <> e) })
      panic as {
        int.to_string(list.length(errors))
        <> " method-body-binding cases failed"
      }
    }
  }
}

/// Parsing `src` must succeed; returns the failure reason otherwise.
fn expect_parses(src: String, mode: parser.ParseMode) -> Result(Nil, String) {
  case parser.parse(src, mode) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(src <> " -> " <> parser.parse_error_to_string(err))
  }
}

/// Parsing `src` must fail with a duplicate-parameter error.
fn expect_dup_param(
  src: String,
  mode: parser.ParseMode,
) -> Result(Nil, String) {
  case parser.parse(src, mode) {
    Ok(_) -> Error(src <> " -> parsed; expected a duplicate-param error")
    Error(err) -> {
      let msg = parser.parse_error_to_string(err)
      case string.contains(msg, "Duplicate parameter") {
        True -> Ok(Nil)
        False -> Error(src <> " -> wrong error: " <> msg)
      }
    }
  }
}
