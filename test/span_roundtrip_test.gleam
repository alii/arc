//// Round-trip tests for byte spans on expression nodes.
////
//// Every `ast.Expression` variant carries a `span: Span` covering the whole
//// expression (including its delimiters — parens, brackets, the closing `)`
//// of a call). Slicing the original source by that span must reproduce the
//// exact source text of the expression. This is the keystone guarantee a
//// bundler relies on to locate and rewrite each occurrence, and what the
//// emitter relies on for column-precise errors.
////
//// Template-literal substitution expressions (`${…}`) are parsed from the
//// SAME on-demand scanner as everything else, so their spans are absolute
//// byte offsets into the outer source and round-trip like any other
//// expression (see template_substitution_expression_span_roundtrip_test).

import arc/parser
import arc/parser/ast
import gleam/bit_array
import gleam/list

/// Slice the original source by a half-open byte span and decode to text.
fn slice_span(source: String, span: ast.Span) -> String {
  let bytes = bit_array.from_string(source)
  let assert Ok(slice) =
    bit_array.slice(bytes, span.start, span.end - span.start)
  let assert Ok(text) = bit_array.to_string(slice)
  text
}

/// Parse a script and return its statement list.
fn script_statements(source: String) -> List(ast.StmtWithLine) {
  let assert Ok(#(ast.Script(body), _sb)) = parser.parse(source, parser.Script)
  body
}

/// The expression of the first (and only) ExpressionStatement in a script.
/// Test sources are constructed so the whole script is a single expression
/// statement, keeping this a direct destructure rather than a deep walk.
fn first_expr(source: String) -> ast.Expression {
  let assert [ast.StmtWithLine(statement: stmt, ..)] = script_statements(source)
  let assert ast.ExpressionStatement(expression: expr, ..) = stmt
  expr
}

/// As `first_expr`, but parses in Module mode so top-level `await` is legal.
fn first_module_expr(source: String) -> ast.Expression {
  let assert Ok(#(ast.Module(body), _sb)) = parser.parse(source, parser.Module)
  let assert [ast.StatementItem(ast.StmtWithLine(statement: stmt, ..))] = body
  let assert ast.ExpressionStatement(expression: expr, ..) = stmt
  expr
}

/// The expression of the single statement inside a generator function body.
/// Used for `yield`, which is only legal inside a generator. Spans inside the
/// body are still byte offsets into the outer source (the body is not
/// re-tokenized), so they round-trip against `source` directly.
fn first_generator_body_expr(source: String) -> ast.Expression {
  let assert [ast.StmtWithLine(statement: decl, ..)] = script_statements(source)
  let assert ast.FunctionDeclaration(body: inner, ..) = decl
  let assert [ast.StmtWithLine(statement: stmt, ..)] = inner
  let assert ast.ExpressionStatement(expression: expr, ..) = stmt
  expr
}

/// The binding pattern of the first declarator in a single
/// `let`/`const`/`var` declaration.
fn first_declarator_pattern(source: String) -> ast.Pattern {
  let assert [ast.StmtWithLine(statement: stmt, ..)] = script_statements(source)
  let assert ast.VariableDeclaration(declarations: decls, ..) = stmt
  let assert [ast.VariableDeclarator(id: pattern, ..)] = decls
  pattern
}

// ---- Identifier (reference) spans -----------------------------------------

pub fn identifier_span_ascii_roundtrip_test() {
  let source = "answer;\n"
  let assert ast.Identifier(name:, span:) = first_expr(source)
  assert name == "answer"
  assert slice_span(source, span) == "answer"
}

pub fn identifier_span_non_ascii_roundtrip_test() {
  // `café` — the trailing `é` is a 2-byte UTF-8 sequence (C3 A9), so the byte
  // span must extend one byte past the visible character count.
  let source = "café;\n"
  let assert ast.Identifier(name:, span:) = first_expr(source)
  assert name == "café"
  assert slice_span(source, span) == "café"
}

pub fn identifier_span_multibyte_roundtrip_test() {
  // A fully non-Latin identifier: `日本語` is three 3-byte CJK code points.
  let source = "日本語;\n"
  let assert ast.Identifier(name:, span:) = first_expr(source)
  assert name == "日本語"
  assert slice_span(source, span) == "日本語"
}

// ---- IdentifierPattern (binding) spans ------------------------------------

pub fn identifier_pattern_span_ascii_roundtrip_test() {
  let source = "let total = 1;\n"
  let assert ast.IdentifierPattern(name:, span:) =
    first_declarator_pattern(source)
  assert name == "total"
  assert slice_span(source, span) == "total"
}

pub fn identifier_pattern_span_non_ascii_roundtrip_test() {
  // `naïve` contains `ï` (2-byte C3 AF). The pattern span must cover all bytes.
  let source = "const naïve = 1;\n"
  let assert ast.IdentifierPattern(name:, span:) =
    first_declarator_pattern(source)
  assert name == "naïve"
  assert slice_span(source, span) == "naïve"
}

pub fn identifier_pattern_span_multibyte_roundtrip_test() {
  // Greek `λ` plus an emoji-free CJK suffix to keep multi-byte coverage broad.
  let source = "var λ値 = 1;\n"
  let assert ast.IdentifierPattern(name:, span:) =
    first_declarator_pattern(source)
  assert name == "λ値"
  assert slice_span(source, span) == "λ値"
}

/// A non-ASCII binding introduced inside a destructuring pattern: its span must
/// still round-trip even though it is nested below an object pattern.
pub fn destructured_identifier_pattern_span_roundtrip_test() {
  let source = "let { café } = obj;\n"
  let assert [ast.StmtWithLine(statement: stmt, ..)] = script_statements(source)
  let assert ast.VariableDeclaration(declarations: decls, ..) = stmt
  let assert [ast.VariableDeclarator(id: pattern, ..)] = decls
  let assert ast.ObjectPattern(properties: props) = pattern
  let assert Ok(local) =
    list.find_map(props, fn(prop) {
      case prop {
        ast.PatternProperty(value: ast.IdentifierPattern(name:, span:), ..) ->
          Ok(#(name, span))
        _ -> Error(Nil)
      }
    })
  let #(name, span) = local
  assert name == "café"
  assert slice_span(source, span) == "café"
}

// ---- Expression spans: leaf literals --------------------------------------
//
// All tests below rely on the universal `.span` accessor that Gleam exposes
// once every `Expression` variant carries a `span: Span` field of identical
// name and type.

pub fn number_literal_span_roundtrip_test() {
  let source = "42;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "42"
}

pub fn number_literal_span_leading_whitespace_test() {
  // Span must skip leading trivia and stop before the trailing `;`.
  let source = "  3.14159;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "3.14159"
}

pub fn string_literal_span_roundtrip_test() {
  // Span covers the quotes — slicing returns the source text, not the
  // decoded value.
  let source = "'hi';\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "'hi'"
}

pub fn boolean_literal_span_roundtrip_test() {
  let source = "true;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "true"
}

pub fn null_literal_span_roundtrip_test() {
  let source = "null;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "null"
}

pub fn this_expression_span_roundtrip_test() {
  let source = "this;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "this"
}

pub fn template_literal_span_roundtrip_test() {
  // Outer span covers the whole backtick token. Inner `${…}` expression
  // spans are slice-relative (see module doc) and intentionally not asserted.
  let source = "`a ${x} b`;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "`a ${x} b`"
}

pub fn regexp_literal_span_roundtrip_test() {
  let source = "/ab+c/gi;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "/ab+c/gi"
}

// ---- Expression spans: operators ------------------------------------------

pub fn binary_expression_span_roundtrip_test() {
  let source = "a + b;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "a + b"
}

pub fn binary_expression_nested_span_roundtrip_test() {
  // `*` binds tighter than `+`, so the tree is `a + (b * c)`. The outer span
  // covers the whole expression; the inner right operand's span covers
  // exactly `b * c` — left-recursive span composition via `left.span.start`.
  let source = "a + b * c;\n"
  let outer = first_expr(source)
  assert slice_span(source, outer.span) == "a + b * c"
  let assert ast.BinaryExpression(right: inner, ..) = outer
  assert slice_span(source, inner.span) == "b * c"
}

pub fn logical_expression_span_roundtrip_test() {
  let source = "a && b;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "a && b"
}

pub fn unary_expression_span_roundtrip_test() {
  // Span runs from the operator token through the end of the argument.
  let source = "!x;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "!x"
}

pub fn unary_keyword_span_roundtrip_test() {
  let source = "typeof x;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "typeof x"
}

pub fn update_postfix_span_roundtrip_test() {
  // Postfix: span runs from the argument start through the `++` operator.
  let source = "x++;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "x++"
}

pub fn update_prefix_span_roundtrip_test() {
  let source = "++x;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "++x"
}

pub fn conditional_expression_span_roundtrip_test() {
  let source = "a ? b : c;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "a ? b : c"
}

pub fn assignment_expression_span_roundtrip_test() {
  let source = "a = b;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "a = b"
}

pub fn sequence_expression_span_roundtrip_test() {
  let source = "a, b, c;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "a, b, c"
}

// ---- Expression spans: member / call / new --------------------------------

pub fn member_expression_span_roundtrip_test() {
  let source = "obj.prop;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "obj.prop"
}

pub fn computed_member_span_roundtrip_test() {
  // Span includes the closing `]`.
  let source = "a[i];\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "a[i]"
}

pub fn optional_member_span_roundtrip_test() {
  let source = "a?.b;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "a?.b"
}

pub fn call_expression_span_roundtrip_test() {
  // Span runs from the callee start through the closing `)`.
  let source = "f(a, b);\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "f(a, b)"
}

pub fn chained_call_span_roundtrip_test() {
  // Left-recursive suffix chain: each link extends the span to the new end.
  let source = "f(a).g(b);\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "f(a).g(b)"
}

pub fn new_expression_span_roundtrip_test() {
  // Span runs from `new` through the closing `)`.
  let source = "new X(1);\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "new X(1)"
}

pub fn new_expression_no_args_span_roundtrip_test() {
  // `new` without an argument list — span ends at the callee.
  let source = "new X;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "new X"
}

// ---- Expression spans: bracketed primaries --------------------------------

pub fn array_expression_span_roundtrip_test() {
  let source = "[1, 2];\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "[1, 2]"
}

pub fn parenthesized_expression_span_roundtrip_test() {
  // Span includes both parens — `(x)`, not `x`.
  let source = "(x);\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "(x)"
}

pub fn object_expression_span_roundtrip_test() {
  // `{` at statement position opens a block, so wrap in parens. The outer
  // ParenthesizedExpression spans `({a: 1})`; the inner ObjectExpression
  // spans exactly `{a: 1}` including both braces.
  let source = "({a: 1});\n"
  let paren = first_expr(source)
  assert slice_span(source, paren.span) == "({a: 1})"
  let assert ast.ParenthesizedExpression(expression: obj, ..) = paren
  assert slice_span(source, obj.span) == "{a: 1}"
}

pub fn arrow_function_span_roundtrip_test() {
  // Span runs from the opening `(` of the parameter list through the body.
  let source = "(x) => x;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "(x) => x"
}

pub fn arrow_function_paren_free_span_roundtrip_test() {
  // Single bare-identifier parameter, no parens — span starts at the
  // identifier.
  let source = "x => x + 1;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "x => x + 1"
}

pub fn async_arrow_function_span_roundtrip_test() {
  // Span starts at `async`, not at `(`.
  let source = "async (x) => x;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "async (x) => x"
}

// ---- Expression spans: contextual keywords --------------------------------

pub fn await_expression_span_roundtrip_test() {
  // Top-level `await` is only legal in Module goal. Span runs from `await`
  // through the end of the argument.
  let source = "await x;\n"
  let expr = first_module_expr(source)
  assert slice_span(source, expr.span) == "await x"
}

pub fn yield_expression_span_roundtrip_test() {
  // `yield` is only legal inside a generator. The body is not re-tokenized,
  // so the inner expression's span is still an offset into the outer source.
  let source = "function* g() { yield x; }\n"
  let expr = first_generator_body_expr(source)
  assert slice_span(source, expr.span) == "yield x"
}

pub fn yield_no_argument_span_roundtrip_test() {
  // Bare `yield;` — span covers exactly the `yield` keyword.
  let source = "function* g() { yield; }\n"
  let expr = first_generator_body_expr(source)
  assert slice_span(source, expr.span) == "yield"
}

// ---- Template substitution expression spans -------------------------------

/// Substitution expressions are lexed by the same on-demand scanner as the
/// enclosing source (they are NOT re-tokenized from a sliced inner string),
/// so their spans are absolute and must round-trip against the outer source
/// — the guarantee a bundler needs to rewrite code inside `${…}`.
pub fn template_substitution_expression_span_roundtrip_test() {
  // Non-ASCII before the template exercises byte (not char) offsets.
  let source = "const π = 1;\n`a${foo(π) + 1}b${  bar  }c`;\n"
  let assert [_, ast.StmtWithLine(statement: stmt, ..)] =
    script_statements(source)
  let assert ast.ExpressionStatement(
    expression: ast.TemplateLiteral(expressions: [first, second, ..], ..),
    ..,
  ) = stmt
  assert slice_span(source, first.span) == "foo(π) + 1"
  assert slice_span(source, second.span) == "bar"
}
