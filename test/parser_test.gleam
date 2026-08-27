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

// fixtures expecting errors for syntax now valid
const outdated_fail_tests = [
  "98204d734f8c72b3.js",
  "ef81b93cf9bdb4ec.js",
  "a8beb1480f385441.js",
  "79f882da06f88c9f.js",
  "0d5e450f1da8a92a.js",
  "92b6af54adef3624.js",
  "748656edbfb2d0bb.js",
]

fn parse_mode(filename: String) -> parser.ParseMode {
  case string.contains(filename, ".module.") {
    True -> parser.Module
    False -> parser.Script
  }
}

fn pass_test_fn(filename: String, source: String) -> Result(Nil, String) {
  let mode = parse_mode(filename)
  case parser.parse(source, mode) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(parser.parse_error_to_string(err))
  }
}

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

fn slice_span(source: String, span: ast.Span) -> Result(String, Nil) {
  let bytes = bit_array.from_string(source)
  let ast.Span(start:, end:) = span
  use sliced <- result.try(bit_array.slice(bytes, start, end - start))
  bit_array.to_string(sliced)
}

pub fn import_declaration_span_round_trip_test() {
  let cases = [
    #("import \"mod\";", "import \"mod\""),
    #("import a from \"mod\";", "import a from \"mod\""),
    #("import * as ns from \"mod\";", "import * as ns from \"mod\""),
    #("import { a, b } from \"mod\";", "import { a, b } from \"mod\""),
    #("import a, { b } from \"mod\";", "import a, { b } from \"mod\""),
    #("import a, * as ns from \"mod\";", "import a, * as ns from \"mod\""),
    #("import defer * as ns from \"mod\";", "import defer * as ns from \"mod\""),
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

pub fn export_default_span_round_trip_test() {
  let cases = [
    #("export default 1;", "export default 1;"),
    #("export default 1 + 2;", "export default 1 + 2;"),
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

pub fn binding_span_round_trip_test() {
  let cases = [
    #("import a from \"mod\";", "import", [#("a", "a")]),
    #("import * as ns from \"mod\";", "import", [#("ns", "ns")]),
    #("import { a } from \"mod\";", "import", [#("a", "a")]),
    #("import { a as b } from \"mod\";", "import", [#("b", "b")]),
    #("import x, { a, b as c } from \"mod\";", "import", [
      #("x", "x"),
      #("a", "a"),
      #("c", "c"),
    ]),
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
          ast.ExportNamed(specifiers: [_, ..] as specs, ..) ->
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

pub fn method_body_bindings_are_not_params_test() {
  let results = [
    expect_parses(
      "class C { reset() { for (const id of [1]) { id; } for (const id of [2]) { id; } } }",
      parser.Script,
    ),
    expect_parses(
      "function f() { for (const id of [1]) {} for (const id of [2]) {} }",
      parser.Script,
    ),
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

pub fn committed_point_parse_errors_test() {
  let results = [
    expect_parse_error("class A extends", parser.Script),
    expect_parse_error("class A extends {#x} {}", parser.Script),
    expect_parse_error("class A extends (oops {}", parser.Script),
    expect_parse_error("let [x = ] = y;", parser.Script),
    expect_parse_error("let {x = } = y;", parser.Script),
    expect_parse_error("function f(a = ) {}", parser.Script),
    expect_parse_error("for (var a = 1,;;) {}", parser.Script),
    expect_parse_error("var a = 1,", parser.Script),
    expect_parses("class A extends B {}", parser.Script),
    expect_parses("let [x = 1] = y;", parser.Script),
    expect_parses("for (var a = 1, b = 2;;) {}", parser.Script),
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
        <> " committed-point parse-error cases failed"
      }
    }
  }
}

pub fn regex_literal_resyncs_token_stream_test() {
  let results = [
    expect_parses("s.replace(/'/g, \"\\\\'\");", parser.Script),
    expect_parses("s.replace(/`/g, \"x\"); var t = `q`;", parser.Script),
    expect_parses("`${s.replace(/'/g, \"\\\\'\")}`;", parser.Script),
    expect_parses("`[${s.replace(/'/g, \"\\\\'\")}]`;", parser.Script),
    expect_parses("`${a / b} ${(a) / [1][0]}`;", parser.Script),
    expect_parses("`${i++ / 2} ${j / 3}`;", parser.Script),
    expect_parses("`${i-- / 2} ${j / 3}`;", parser.Script),
    expect_parses("`${typeof /'/}`;", parser.Script),
    expect_parses(
      "`${s.replace(/x/g, \"y\")}`;\nvar a = 1;\nvar b = 2;\nvar c = 3;\n"
        <> "var d = 4;\nvar e = 5;\nconsole.log(a, b, c, d, e);",
      parser.Script,
    ),
    expect_parses("`${o.in / 2} ${x / 3}`;", parser.Script),
    // phantom comment from a regex body forces a re-lex
    expect_parses("var r = s.split(/\\//); f(r);", parser.Script),
    expect_parses(
      "var r = s.split(/[/*]/);\nf(r);\ng(); /* trailing */",
      parser.Script,
    ),
    expect_parses("`${q /* don't } panic */ + 1}`;", parser.Script),
    expect_parses("`${q + // it's fine\n 2}`;", parser.Script),
    expect_parses("`${s.replace(/* sep */ /'/g, \"#\")}`;", parser.Script),
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
        int.to_string(list.length(errors)) <> " regex token-resync cases failed"
      }
    }
  }
}

pub fn lazy_lexer_error_reporting_test() {
  let results = [
    expect_parse_error_containing("var x = 0x;", parser.Script, "illegal token"),
    expect_parse_error_containing(
      "x\n/*",
      parser.Script,
      "Unterminated block comment",
    ),
    expect_parse_error_containing(
      "x /*",
      parser.Script,
      "Unterminated block comment",
    ),
    expect_parse_error_containing(
      "var o = { /*",
      parser.Script,
      "Unterminated block comment",
    ),
    expect_parse_error_containing(
      "a . /*",
      parser.Script,
      "Unterminated block comment",
    ),
    expect_parses("x = /0x/;", parser.Script),
    expect_parses("var m = \"s\".match(/\"\\x\"/);", parser.Script),
    expect_parses("var t = `${a} /* ${b}`;", parser.Script),
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
        int.to_string(list.length(errors)) <> " lazy lexer-error cases failed"
      }
    }
  }
}

fn expect_parses(src: String, mode: parser.ParseMode) -> Result(Nil, String) {
  case parser.parse(src, mode) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(src <> " -> " <> parser.parse_error_to_string(err))
  }
}

fn expect_parse_error(
  src: String,
  mode: parser.ParseMode,
) -> Result(Nil, String) {
  case parser.parse(src, mode) {
    Ok(_) -> Error(src <> " -> parsed; expected a SyntaxError")
    Error(_) -> Ok(Nil)
  }
}

fn expect_dup_param(
  src: String,
  mode: parser.ParseMode,
) -> Result(Nil, String) {
  expect_parse_error_containing(src, mode, "Duplicate parameter")
}

fn expect_parse_error_containing(
  src: String,
  mode: parser.ParseMode,
  needle: String,
) -> Result(Nil, String) {
  case parser.parse(src, mode) {
    Ok(_) -> Error(src <> " -> parsed; expected: " <> needle)
    Error(err) -> {
      let msg = parser.parse_error_to_string(err)
      case string.contains(msg, needle) {
        True -> Ok(Nil)
        False -> Error(src <> " -> wrong error: " <> msg)
      }
    }
  }
}

pub fn continue_label_must_denote_iteration_statement_test() {
  let non_iteration = "does not denote an iteration statement"
  let results = [
    expect_parse_error_containing(
      "while (1) { x: { continue x; } }",
      parser.Script,
      non_iteration,
    ),
    expect_parse_error_containing(
      "do { x: { continue x; } } while (0);",
      parser.Script,
      non_iteration,
    ),
    expect_parse_error_containing(
      "x: while (1) { y: { continue y; } }",
      parser.Script,
      non_iteration,
    ),
    expect_parse_error_containing(
      "a: switch (1) { default: while (1) { continue a; } }",
      parser.Script,
      non_iteration,
    ),
    expect_parses("x: { break x; }", parser.Script),
    expect_parses("while (1) { x: { break x; } }", parser.Script),
    expect_parses("a: while (1) { continue a; }", parser.Script),
    expect_parses("a: b: while (1) { continue a; }", parser.Script),
    expect_parses("a: b: c: for (;;) { continue b; }", parser.Script),
    expect_parses("a: b: do { continue a; } while (0);", parser.Script),
    expect_parses("a: for (x in {}) { continue a; }", parser.Script),
    expect_parses("a: while (1) { b: { continue a; } }", parser.Script),
    expect_parse_error_containing(
      "a: a: while (1) {}",
      parser.Script,
      "Duplicate label",
    ),
    expect_parse_error_containing(
      "a: b: a: while (1) {}",
      parser.Script,
      "Duplicate label",
    ),
    expect_parse_error_containing(
      "a: { a: ; }",
      parser.Script,
      "Duplicate label",
    ),
    expect_parse_error_containing(
      "while (1) { continue nope; }",
      parser.Script,
      "Undefined label",
    ),
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
        int.to_string(list.length(errors)) <> " continue-label cases failed"
      }
    }
  }
}

fn report(results: List(Result(Nil, String)), label: String) -> Nil {
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
      panic as { int.to_string(list.length(errors)) <> " " <> label }
    }
  }
}

pub fn function_boundary_resets_param_state_test() {
  [
    expect_parses("function f({a}) { x => {\"use strict\"} }", parser.Script),
    expect_parses("function f({a}) { (x) => {\"use strict\"} }", parser.Script),
    expect_parses("try{}catch({a}){ x => {\"use strict\"} }", parser.Script),
    expect_parses(
      "function f({a}) { function g(){\"use strict\"} }",
      parser.Script,
    ),
    expect_parses("function f(eval) { x => {\"use strict\"} }", parser.Script),
    expect_parse_error_containing(
      "function f({a}) { \"use strict\" }",
      parser.Script,
      "use strict",
    ),
    expect_parses(
      "function eval() { function g(){\"use strict\"} }",
      parser.Script,
    ),
    expect_parse_error(
      "function g() { function eval(){\"use strict\"} }",
      parser.Script,
    ),
  ]
  |> report("function-boundary param-state cases failed")
}

pub fn arrow_preserves_enclosing_cover_grammar_errors_test() {
  [
    expect_parse_error("({a = 1}, () => {});", parser.Script),
    expect_parse_error("({a = 1}) => {}, ({a = 1})", parser.Script),
    expect_parse_error("[{a = 1}, () => {}];", parser.Script),
    expect_parse_error(
      "({__proto__: a, __proto__: b}, () => {});",
      parser.Script,
    ),
    expect_parses("({a = 1}) => {};", parser.Script),
    expect_parses("({__proto__: a, __proto__: b}) => {};", parser.Script),
    expect_parses("({a = 1} = b);", parser.Script),
    expect_parses("({__proto__: a, __proto__: b} = c);", parser.Script),
    expect_parse_error("() => ({a = 1});", parser.Script),
    expect_parse_error("() => ({__proto__: 1, __proto__: 2});", parser.Script),
    expect_parse_error("x = () => ({a = 1});", parser.Script),
    expect_parse_error("function f(x = {a = 1}) {}", parser.Script),
    expect_parse_error("(function(x = {a = 1}) {});", parser.Script),
    expect_parse_error("class C { m(x = {a = 1}) {} }", parser.Script),
    expect_parse_error("var o = { set x(v = {a = 1}) {} };", parser.Script),
    expect_parse_error(
      "function f(x = {__proto__: 1, __proto__: 2}) {}",
      parser.Script,
    ),
    expect_parses("function f(x = {a: 1}) {}", parser.Script),
    expect_parses("var o = { set x(v = 1) {} };", parser.Script),
    expect_parses("(x = {a: 1}) => {};", parser.Script),
    expect_parse_error("function f() { return {a = 1}; }", parser.Script),
    expect_parse_error(
      "function f() { return {__proto__: 1, __proto__: 2}; }",
      parser.Script,
    ),
    expect_parse_error("function f() { throw {a = 1}; }", parser.Script),
    expect_parse_error("function f() { var x = {a = 1}; }", parser.Script),
    expect_parse_error("function f() { if ({a = 1}) {} }", parser.Script),
    expect_parse_error("class C { m() { return {a = 1}; } }", parser.Script),
    expect_parse_error("(() => { return {a = 1}; });", parser.Script),
    expect_parse_error("class C { static { ({a = 1}); } }", parser.Script),
    expect_parse_error("throw {a = 1};", parser.Script),
    expect_parse_error("var x = {a = 1};", parser.Script),
    expect_parse_error("var x = {__proto__: 1, __proto__: 2};", parser.Script),
    expect_parses("var x = {a: 1};", parser.Script),
    expect_parses("var {a = 1} = b;", parser.Script),
    expect_parses("function f() { return {a: 1}; }", parser.Script),
    expect_parses("function f() { var {a = 1} = b; }", parser.Script),
    expect_parses("function f() { return ({a = 1} = b); }", parser.Script),
    expect_parses("function f() { return ({a = 1}) => a; }", parser.Script),
  ]
  |> report("cover-grammar boundary cases failed")
}

pub fn for_head_no_in_stops_at_function_boundary_test() {
  [
    expect_parses(
      "for (let x = function(){ return 'a' in {} };;) break;",
      parser.Script,
    ),
    expect_parses(
      "for (let x = { get f() { return 'a' in {} } };;) break;",
      parser.Script,
    ),
    expect_parses(
      "for (let x = class { m(){ return 'a' in {} } };;) break;",
      parser.Script,
    ),
    expect_parses("for (let x = () => { 'a' in {} };;) break;", parser.Script),
    expect_parses(
      "for (var x = class { f = 'a' in {} };;) break;",
      parser.Script,
    ),
    expect_parses(
      "for (var x = class { static f = 'a' in {} };;) break;",
      parser.Script,
    ),
    expect_parses("for (var x = 0 ? 'a' in {} : 1;;) break;", parser.Script),
    expect_parse_error(
      "for (var x = 0 ? 1 : 'a' in {};;) break;",
      parser.Script,
    ),
    expect_parse_error("for (var x = 1, y = 'a' in {};;) break;", parser.Script),
    expect_parse_error("for (let x = 1, y = 'a' in {};;) break;", parser.Script),
    expect_parse_error("for (var x, y = 'a' in {};;) break;", parser.Script),
    expect_parses("for (let f = (a = 'a' in {}) => a;;) break;", parser.Script),
    expect_parses(
      "for (let f = function(a = 'a' in {}){};;) break;",
      parser.Script,
    ),
    expect_parse_error("for (let f = a => 'a' in {};;) break;", parser.Script),
    expect_parse_error("for (let f = () => 'a' in {};;) break;", parser.Script),
    expect_parses("let f = a => 'a' in {};", parser.Script),
    expect_parses("for (var x = ('a' in {});;) break;", parser.Script),
    expect_parses("for (var x = ['a' in {}];;) break;", parser.Script),
    expect_parses("for (var x = f('a' in {});;) break;", parser.Script),
    expect_parses("for (var x = {}['a' in {}];;) break;", parser.Script),
    expect_parses("for (var x = `${'a' in {}}`;;) break;", parser.Script),
    expect_parses("for (var x = import('a' in {});;) break;", parser.Script),
    expect_parse_error("for (var x = ('a') in {};;) break;", parser.Script),
    expect_parse_error(
      "for (var x = function(){ 'a' in {} } in {};;) break;",
      parser.Script,
    ),
  ]
  |> report("for-head [In] boundary cases failed")
}
