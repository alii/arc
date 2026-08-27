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

fn script_statements(source: String) -> List(ast.StmtWithLine) {
  let assert Ok(#(ast.Script(body), _sb)) = parser.parse(source, parser.Script)
  body
}

fn first_expr(source: String) -> ast.Expression {
  let assert [ast.StmtWithLine(statement: stmt, ..)] = script_statements(source)
  let assert ast.ExpressionStatement(expression: expr, ..) = stmt
  expr
}

fn first_module_expr(source: String) -> ast.Expression {
  let assert Ok(#(ast.Module(body), _sb)) = parser.parse(source, parser.Module)
  let assert [ast.StatementItem(ast.StmtWithLine(statement: stmt, ..))] = body
  let assert ast.ExpressionStatement(expression: expr, ..) = stmt
  expr
}

fn first_generator_body_expr(source: String) -> ast.Expression {
  let assert [ast.StmtWithLine(statement: decl, ..)] = script_statements(source)
  let assert ast.FunctionDeclaration(body: inner, ..) = decl
  let assert [ast.StmtWithLine(statement: stmt, ..)] = inner
  let assert ast.ExpressionStatement(expression: expr, ..) = stmt
  expr
}

fn first_declarator_pattern(source: String) -> ast.Pattern {
  let assert [ast.StmtWithLine(statement: stmt, ..)] = script_statements(source)
  let assert ast.VariableDeclaration(declarations: decls, ..) = stmt
  let assert [ast.VariableDeclarator(id: pattern, ..)] = decls
  pattern
}

pub fn identifier_span_ascii_roundtrip_test() {
  let source = "answer;\n"
  let assert ast.Identifier(name:, span:) = first_expr(source)
  assert name == "answer"
  assert slice_span(source, span) == "answer"
}

pub fn identifier_span_non_ascii_roundtrip_test() {
  let source = "café;\n"
  let assert ast.Identifier(name:, span:) = first_expr(source)
  assert name == "café"
  assert slice_span(source, span) == "café"
}

pub fn identifier_span_multibyte_roundtrip_test() {
  let source = "日本語;\n"
  let assert ast.Identifier(name:, span:) = first_expr(source)
  assert name == "日本語"
  assert slice_span(source, span) == "日本語"
}

pub fn identifier_pattern_span_ascii_roundtrip_test() {
  let source = "let total = 1;\n"
  let assert ast.IdentifierPattern(name:, span:) =
    first_declarator_pattern(source)
  assert name == "total"
  assert slice_span(source, span) == "total"
}

pub fn identifier_pattern_span_non_ascii_roundtrip_test() {
  let source = "const naïve = 1;\n"
  let assert ast.IdentifierPattern(name:, span:) =
    first_declarator_pattern(source)
  assert name == "naïve"
  assert slice_span(source, span) == "naïve"
}

pub fn identifier_pattern_span_multibyte_roundtrip_test() {
  let source = "var λ値 = 1;\n"
  let assert ast.IdentifierPattern(name:, span:) =
    first_declarator_pattern(source)
  assert name == "λ値"
  assert slice_span(source, span) == "λ値"
}

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

pub fn number_literal_span_roundtrip_test() {
  let source = "42;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "42"
}

pub fn number_literal_span_leading_whitespace_test() {
  let source = "  3.14159;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "3.14159"
}

pub fn string_literal_span_roundtrip_test() {
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
  let source = "`a ${x} b`;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "`a ${x} b`"
}

pub fn regexp_literal_span_roundtrip_test() {
  let source = "/ab+c/gi;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "/ab+c/gi"
}

pub fn binary_expression_span_roundtrip_test() {
  let source = "a + b;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "a + b"
}

pub fn binary_expression_nested_span_roundtrip_test() {
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

pub fn member_expression_span_roundtrip_test() {
  let source = "obj.prop;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "obj.prop"
}

pub fn computed_member_span_roundtrip_test() {
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
  let source = "f(a, b);\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "f(a, b)"
}

pub fn chained_call_span_roundtrip_test() {
  let source = "f(a).g(b);\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "f(a).g(b)"
}

pub fn new_expression_span_roundtrip_test() {
  let source = "new X(1);\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "new X(1)"
}

pub fn new_expression_no_args_span_roundtrip_test() {
  let source = "new X;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "new X"
}

pub fn array_expression_span_roundtrip_test() {
  let source = "[1, 2];\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "[1, 2]"
}

pub fn parenthesized_expression_span_roundtrip_test() {
  let source = "(x);\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "(x)"
}

pub fn object_expression_span_roundtrip_test() {
  let source = "({a: 1});\n"
  let paren = first_expr(source)
  assert slice_span(source, paren.span) == "({a: 1})"
  let assert ast.ParenthesizedExpression(expression: obj, ..) = paren
  assert slice_span(source, obj.span) == "{a: 1}"
}

pub fn arrow_function_span_roundtrip_test() {
  let source = "(x) => x;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "(x) => x"
}

pub fn arrow_function_paren_free_span_roundtrip_test() {
  let source = "x => x + 1;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "x => x + 1"
}

pub fn async_arrow_function_span_roundtrip_test() {
  let source = "async (x) => x;\n"
  let expr = first_expr(source)
  assert slice_span(source, expr.span) == "async (x) => x"
}

pub fn await_expression_span_roundtrip_test() {
  let source = "await x;\n"
  let expr = first_module_expr(source)
  assert slice_span(source, expr.span) == "await x"
}

pub fn yield_expression_span_roundtrip_test() {
  let source = "function* g() { yield x; }\n"
  let expr = first_generator_body_expr(source)
  assert slice_span(source, expr.span) == "yield x"
}

pub fn yield_no_argument_span_roundtrip_test() {
  let source = "function* g() { yield; }\n"
  let expr = first_generator_body_expr(source)
  assert slice_span(source, expr.span) == "yield"
}

pub fn template_substitution_expression_span_roundtrip_test() {
  let source = "const π = 1;\n`a${foo(π) + 1}b${  bar  }c`;\n"
  let assert [_, ast.StmtWithLine(statement: stmt, ..)] =
    script_statements(source)
  let assert ast.ExpressionStatement(
    expression: ast.TemplateLiteral(parts:, ..),
    ..,
  ) = stmt
  let assert [first, second, ..] = ast.template_expressions(parts)
  assert slice_span(source, first.span) == "foo(π) + 1"
  assert slice_span(source, second.span) == "bar"
}
