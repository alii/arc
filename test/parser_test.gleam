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
  // `"\\8"` / `"\\9"` — NonOctalDecimalEscapeSequence (ES2019, spec B.1.2):
  // legal in sloppy-mode strings (`'\\8' === '8'`), a SyntaxError only in
  // strict code. These fixtures predate that spec addition. See
  // lexer.Token.annex_b_legacy.
  "79f882da06f88c9f.js",
  "0d5e450f1da8a92a.js",
  "92b6af54adef3624.js",
  "748656edbfb2d0bb.js",
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

/// Regression: at grammatically committed points the parser must report the
/// inner ParseError instead of silently backtracking to a different AST (or
/// to a misleading error at an earlier position). The committed points are:
/// `extends` (a ClassHeritage LeftHandSideExpression is mandatory), `=` in a
/// binding pattern (an initializer is mandatory), and `,` in a `for(;;)`
/// declaration head (another declarator is mandatory).
pub fn committed_point_parse_errors_test() {
  let results = [
    // (a) Class heritage: before the fix `class A extends {#x} {}` silently
    // became `class A { #x }` followed by an empty block statement.
    expect_parse_error("class A extends", parser.Script),
    expect_parse_error("class A extends {#x} {}", parser.Script),
    expect_parse_error("class A extends (oops {}", parser.Script),
    // (b) Pattern default: the `=` commits to an initializer expression.
    expect_parse_error("let [x = ] = y;", parser.Script),
    expect_parse_error("let {x = } = y;", parser.Script),
    expect_parse_error("function f(a = ) {}", parser.Script),
    // (c) Declarator list in a classic for head: `,` commits to a declarator.
    expect_parse_error("for (var a = 1,;;) {}", parser.Script),
    expect_parse_error("var a = 1,", parser.Script),
    // Sanity: the committed constructs still parse when well-formed.
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

/// Regression: a regex literal is re-scanned from source by the parser, but
/// the up-front lexer has no expression context — a quote or backtick in a
/// regex body (`/'/g`) opens a phantom string/template token that swallows
/// the real tokens after the regex, so the parser must re-lex from the end
/// of the regex instead of resuming on the poisoned stream. The same body
/// inside a `${…}` substitution additionally exercises the template
/// splitter's regex scanning (a `'` in a regex must not start a string, and
/// `/` after an operand must still be division).
pub fn regex_literal_resyncs_token_stream_test() {
  let results = [
    // Plain code: the `'` inside the regex used to swallow `, "\\'"`.
    expect_parses("s.replace(/'/g, \"\\\\'\");", parser.Script),
    // Backtick in a regex body: the phantom token is a template literal.
    expect_parses("s.replace(/`/g, \"x\"); var t = `q`;", parser.Script),
    // Same regexes inside a template substitution.
    expect_parses("`${s.replace(/'/g, \"\\\\'\")}`;", parser.Script),
    expect_parses("`[${s.replace(/'/g, \"\\\\'\")}]`;", parser.Script),
    // `/` after an operand inside a substitution is still division —
    // including after a postfix ++/--.
    expect_parses("`${a / b} ${(a) / [1][0]}`;", parser.Script),
    expect_parses("`${i++ / 2} ${j / 3}`;", parser.Script),
    expect_parses("`${i-- / 2} ${j / 3}`;", parser.Script),
    // Regex heads after an operator keyword inside a substitution.
    expect_parses("`${typeof /'/}`;", parser.Script),
    // A regex inside a substitution re-scans within the SUBSTITUTION
    // source; the enclosing file's scanning state must survive it — the
    // trailing statements exist to drain the prefetch window afterwards.
    expect_parses(
      "`${s.replace(/x/g, \"y\")}`;\nvar a = 1;\nvar b = 2;\nvar c = 3;\n"
        <> "var d = 4;\nvar e = 5;\nconsole.log(a, b, c, d, e);",
      parser.Script,
    ),
    // A keyword used as a PROPERTY NAME is an operand: `/` is division.
    expect_parses("`${o.in / 2} ${x / 3}`;", parser.Script),
    // Phantom COMMENT from a regex body: `/\\//` lexes up front as `\\` +
    // a `//` line comment that swallows the rest of the line, and the `/*`
    // in `/[/*]/` as a block comment that swallows source up to the next
    // real `*/` — both leave a positional GAP (comments produce no token),
    // so the parser must re-lex, not positionally skip. (An unterminated
    // phantom `/*` — no later `*/` anywhere — still fails at lex time;
    // fixing that needs regex context in the lexer itself.)
    expect_parses("var r = s.split(/\\//); f(r);", parser.Script),
    expect_parses(
      "var r = s.split(/[/*]/);\nf(r);\ng(); /* trailing */",
      parser.Script,
    ),
    // Comments inside a substitution: quotes / braces in them are inert.
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

/// With on-demand lexing, hard lexer errors are materialised as sentinel
/// tokens (see the parser's ensure_current) and must surface with the
/// LEXER's message at the lexer's position — while lexically broken text
/// inside a regex body or template quasi (source the parser re-scans and
/// jumps over) must not surface at all.
pub fn lazy_lexer_error_reporting_test() {
  let results = [
    // `0x` degrades to a LENIENT Illegal token (not a hard error), same
    // as it always has: a generic unexpected-token report.
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
    // …including where a grammar-specific "expected X" error would
    // otherwise mask it.
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
    // Lexically-invalid text INSIDE a regex body is fine: the body is
    // re-scanned as a regex, never lexed as tokens.
    expect_parses("x = /0x/;", parser.Script),
    expect_parses("var m = \"s\".match(/\"\\x\"/);", parser.Script),
    // …and so is template text after a substitution (`/*` in a quasi).
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

/// Parsing `src` must succeed; returns the failure reason otherwise.
fn expect_parses(src: String, mode: parser.ParseMode) -> Result(Nil, String) {
  case parser.parse(src, mode) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(src <> " -> " <> parser.parse_error_to_string(err))
  }
}

/// Parsing `src` must fail (with any ParseError).
fn expect_parse_error(
  src: String,
  mode: parser.ParseMode,
) -> Result(Nil, String) {
  case parser.parse(src, mode) {
    Ok(_) -> Error(src <> " -> parsed; expected a SyntaxError")
    Error(_) -> Ok(Nil)
  }
}

/// Parsing `src` must fail with a duplicate-parameter error.
fn expect_dup_param(
  src: String,
  mode: parser.ParseMode,
) -> Result(Nil, String) {
  expect_parse_error_containing(src, mode, "Duplicate parameter")
}

/// Parsing `src` must fail with an error whose message contains `needle`.
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

/// ES2024 §14.9.1 Early Errors: `continue L` must target a label that
/// denotes an IterationStatement. The kind of a label is a property of the
/// whole `a: b: …` chain in front of the statement, not of the single
/// innermost label, so every label of a chain that prefixes a loop is a
/// legal continue target.
pub fn continue_label_must_denote_iteration_statement_test() {
  let non_iteration = "does not denote an iteration statement"
  let results = [
    // A label on a plain (non-loop) statement is not a continue target...
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
    // ...but it is still a legal break target.
    expect_parses("x: { break x; }", parser.Script),
    expect_parses("while (1) { x: { break x; } }", parser.Script),
    // EVERY label of a chain that prefixes a loop is a continue target.
    expect_parses("a: while (1) { continue a; }", parser.Script),
    expect_parses("a: b: while (1) { continue a; }", parser.Script),
    expect_parses("a: b: c: for (;;) { continue b; }", parser.Script),
    expect_parses("a: b: do { continue a; } while (0);", parser.Script),
    expect_parses("a: for (x in {}) { continue a; }", parser.Script),
    // A loop label stays a continue target through nested plain labels.
    expect_parses("a: while (1) { b: { continue a; } }", parser.Script),
    // Duplicate labels are still rejected, within one chain and across
    // nested statements.
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
    // Undefined labels are still rejected.
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

/// Report the collected `expect_*` results, panicking with `label` when any
/// of them failed.
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

/// Formal-parameter state (`has_non_simple_param`, `param_bound_names`, …)
/// belongs to the function it describes: a nested function or arrow must not
/// see the ENCLOSING parameter list's state, or its own perfectly legal
/// `"use strict"` directive gets rejected as "misplaced".
pub fn function_boundary_resets_param_state_test() {
  [
    // The arrow's own params are simple, so its `"use strict"` is legal even
    // though the enclosing function / catch clause has a destructured param.
    expect_parses("function f({a}) { x => {\"use strict\"} }", parser.Script),
    expect_parses("function f({a}) { (x) => {\"use strict\"} }", parser.Script),
    expect_parses("try{}catch({a}){ x => {\"use strict\"} }", parser.Script),
    expect_parses(
      "function f({a}) { function g(){\"use strict\"} }",
      parser.Script,
    ),
    // A sloppy `eval` param must not be retro-validated against a nested
    // arrow's `"use strict"`.
    expect_parses("function f(eval) { x => {\"use strict\"} }", parser.Script),
    // ...but the enclosing function's OWN non-simple params still reject it.
    expect_parse_error_containing(
      "function f({a}) { \"use strict\" }",
      parser.Script,
      "use strict",
    ),
    // The nested function's own name is what a retroactive "use strict"
    // checks — not the enclosing one's.
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

/// The deferred cover-grammar early errors (`{a = 1}` shorthand default,
/// duplicate `__proto__`) belong to the expression that owes them. An arrow
/// function nested inside that expression clears them for its own params and
/// body — and must hand them straight back on the way out.
pub fn arrow_preserves_enclosing_cover_grammar_errors_test() {
  [
    // The comma/array expression still owes the deferred error after the arrow.
    expect_parse_error("({a = 1}, () => {});", parser.Script),
    expect_parse_error("({a = 1}) => {}, ({a = 1})", parser.Script),
    expect_parse_error("[{a = 1}, () => {}];", parser.Script),
    expect_parse_error(
      "({__proto__: a, __proto__: b}, () => {});",
      parser.Script,
    ),
    // Arrow params ARE a destructuring context: neither error applies there.
    expect_parses("({a = 1}) => {};", parser.Script),
    expect_parses("({__proto__: a, __proto__: b}) => {};", parser.Script),
    expect_parses("({a = 1} = b);", parser.Script),
    expect_parses("({__proto__: a, __proto__: b} = c);", parser.Script),
    // A concise arrow body is an ordinary expression, so it owes the error
    // itself rather than leaking it to the enclosing statement.
    expect_parse_error("() => ({a = 1});", parser.Script),
    expect_parse_error("() => ({__proto__: 1, __proto__: 2});", parser.Script),
    expect_parse_error("x = () => ({a = 1});", parser.Script),
    // Parameter DEFAULTS are ordinary expressions too, in every param list.
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
    // A function body is a hard boundary for the deferred flags, so an
    // expression inside it must raise its own error rather than have it
    // silently dropped by restore_outer_context.
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
    // Same statements at script top level, where there is no function body
    // to backstop them.
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

/// The grammar's [In] parameter — off inside a `for` head so `in` reads as the
/// for-in keyword — is a nesting-sensitive Ctx flag: a function boundary is
/// always [+In] and must not inherit the head's restriction, and the head must
/// get its restriction back once the nested function is done.
pub fn for_head_no_in_stops_at_function_boundary_test() {
  [
    // A function/method/arrow BODY nested in the head is [+In].
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
    // FormalParameters take no [In] parameter — defaults are always [+In].
    expect_parses("for (let f = (a = 'a' in {}) => a;;) break;", parser.Script),
    expect_parses(
      "for (let f = function(a = 'a' in {}){};;) break;",
      parser.Script,
    ),
    // §15.3 ArrowFunction[?In]: a CONCISE body inherits the head's [~In].
    expect_parse_error("for (let f = a => 'a' in {};;) break;", parser.Script),
    expect_parse_error("for (let f = () => 'a' in {};;) break;", parser.Script),
    // ...and outside a for head, the same concise body is fine.
    expect_parses("let f = a => 'a' in {};", parser.Script),
    // Bracketed sub-expressions in the head are [+In]...
    expect_parses("for (var x = ('a' in {});;) break;", parser.Script),
    expect_parses("for (var x = ['a' in {}];;) break;", parser.Script),
    expect_parses("for (var x = f('a' in {});;) break;", parser.Script),
    expect_parses("for (var x = {}['a' in {}];;) break;", parser.Script),
    expect_parses("for (var x = `${'a' in {}}`;;) break;", parser.Script),
    expect_parses("for (var x = import('a' in {});;) break;", parser.Script),
    // ...but the restriction comes back for the rest of the head: this `in`
    // still reads as the for-in keyword (so: for-in with an initializer),
    // rather than as a relational operator that would make `;;` legal.
    expect_parse_error("for (var x = ('a') in {};;) break;", parser.Script),
    expect_parse_error(
      "for (var x = function(){ 'a' in {} } in {};;) break;",
      parser.Script,
    ),
  ]
  |> report("for-head [In] boundary cases failed")
}
