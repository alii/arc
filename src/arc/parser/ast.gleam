import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Program {
  Script(body: List(StmtWithLine))
  Module(body: List(ModuleItem))
}

// half-open [start, end) utf-8 byte offsets
pub type Span {
  Span(start: Int, end: Int)
}

pub type NamedBinding {
  NamedBinding(name: String, span: Span)
}

// cooked is None on an invalid escape (tagged only)
pub type TemplateQuasi {
  TemplateQuasi(cooked: Option(String), raw: String)
}

pub type TemplateParts(quasi) {
  TemplateParts(head: quasi, tail: List(#(Expression, quasi)))
}

pub fn template_quasis(parts: TemplateParts(a)) -> List(a) {
  [parts.head, ..list.map(parts.tail, fn(part) { part.1 })]
}

pub fn template_expressions(parts: TemplateParts(a)) -> List(Expression) {
  list.map(parts.tail, fn(part) { part.0 })
}

pub fn map_template_quasis(
  parts: TemplateParts(a),
  with f: fn(a) -> b,
) -> TemplateParts(b) {
  TemplateParts(
    head: f(parts.head),
    tail: list.map(parts.tail, fn(part) { #(part.0, f(part.1)) }),
  )
}

pub fn try_map_template_quasis(
  parts: TemplateParts(a),
  with f: fn(a) -> Result(b, e),
) -> Result(TemplateParts(b), e) {
  use head <- result.try(f(parts.head))
  use tail <- result.map(
    list.try_map(parts.tail, fn(part) {
      use quasi <- result.map(f(part.1))
      #(part.0, quasi)
    }),
  )
  TemplateParts(head:, tail:)
}

pub type MetaPropertyKind {
  NewTarget
  ImportMeta
}

pub type MemberProperty {
  Dot(name: String, span: Span)
  Bracket(expression: Expression)
}

pub type StmtWithLine {
  StmtWithLine(line: Int, statement: Statement)
}

pub type ModuleItem {
  StatementItem(StmtWithLine)
  ImportDeclaration(
    specifiers: List(ImportSpecifier),
    source: StringLiteral,
    phase: ImportPhase,
    span: Span,
  )
  ExportDeclaration(declaration: Declaration, line: Int, span: Span)
  ExportNamed(
    specifiers: List(ExportSpecifier),
    source: Option(StringLiteral),
    span: Span,
  )
  ExportDefaultDeclaration(declaration: Expression, line: Int, span: Span)
  ExportAllDeclaration(
    exported: Option(String),
    source: StringLiteral,
    span: Span,
  )
}

pub type Declaration {
  DeclVariable(kind: VariableKind, declarations: List(VariableDeclarator))
  DeclFunction(function: FunctionLiteral)
  DeclClass(
    name: Option(NamedBinding),
    super_class: Option(Expression),
    body: List(ClassElement),
  )
}

pub fn declaration_to_statement(decl: Declaration) -> Statement {
  case decl {
    DeclVariable(kind:, declarations:) ->
      VariableDeclaration(kind:, declarations:)
    DeclFunction(function: FunctionLiteral(
      name:,
      params:,
      body:,
      is_generator:,
      is_async:,
    )) -> FunctionDeclaration(name:, params:, body:, is_generator:, is_async:)
    DeclClass(name:, super_class:, body:) ->
      ClassDeclaration(name:, super_class:, body:)
  }
}

pub type ImportSpecifier {
  ImportDefaultSpecifier(local: String, local_span: Span)
  ImportNamespaceSpecifier(local: String, local_span: Span)
  ImportNamedSpecifier(imported: String, local: String, local_span: Span)
}

pub type ExportSpecifier {
  ExportSpecifier(local: String, exported: String, local_span: Span)
}

pub type StringLiteral {
  StringLit(value: String)
}

pub type Statement {
  EmptyStatement
  // directive: raw source text, for "use strict" detection
  ExpressionStatement(expression: Expression, directive: Option(String))
  BlockStatement(body: List(StmtWithLine))
  VariableDeclaration(
    kind: VariableKind,
    declarations: List(VariableDeclarator),
  )
  ReturnStatement(argument: Option(Expression))
  IfStatement(
    condition: Expression,
    consequent: Statement,
    alternate: Option(Statement),
  )
  ThrowStatement(argument: Expression)
  WhileStatement(condition: Expression, body: Statement)
  DoWhileStatement(condition: Expression, body: Statement)
  ForStatement(
    init: Option(ForInit),
    condition: Option(Expression),
    update: Option(Expression),
    body: Statement,
  )
  ForInStatement(left: ForInit, right: Expression, body: Statement)
  ForOfStatement(
    left: ForInit,
    right: Expression,
    body: Statement,
    is_await: Bool,
  )
  SwitchStatement(discriminant: Expression, cases: List(SwitchCase))
  TryStatement(block: List(StmtWithLine), tail: TryTail)
  BreakStatement(label: Option(String))
  ContinueStatement(label: Option(String))
  DebuggerStatement
  LabeledStatement(label: String, body: Statement)
  WithStatement(object: Expression, body: Statement)
  FunctionDeclaration(
    name: Option(NamedBinding),
    params: List(Pattern),
    body: List(StmtWithLine),
    is_generator: Bool,
    is_async: Bool,
  )
  ClassDeclaration(
    name: Option(NamedBinding),
    super_class: Option(Expression),
    body: List(ClassElement),
  )
}

pub type ForInit {
  ForInitExpression(Expression)
  ForInitDeclaration(kind: VariableKind, declarations: List(VariableDeclarator))
  ForInitPattern(Pattern)
}

pub type SwitchCase {
  SwitchCase(condition: Option(Expression), consequent: List(StmtWithLine))
}

pub type CatchClause {
  CatchClause(param: Option(Pattern), body: List(StmtWithLine))
}

pub type TryTail {
  TryCatch(handler: CatchClause)
  TryFinally(finalizer: List(StmtWithLine))
  TryCatchFinally(handler: CatchClause, finalizer: List(StmtWithLine))
}

pub type FunctionLiteral {
  FunctionLiteral(
    name: Option(NamedBinding),
    params: List(Pattern),
    body: List(StmtWithLine),
    is_generator: Bool,
    is_async: Bool,
  )
}

pub type ClassElement {
  ClassMethod(
    key: PropertyKey,
    value: FunctionLiteral,
    kind: MethodKind,
    is_static: Bool,
  )
  ClassField(key: PropertyKey, value: Option(Expression), is_static: Bool)
  StaticBlock(body: List(StmtWithLine))
}

pub type MethodKind {
  MethodConstructor
  MethodMethod
  MethodGet
  MethodSet
}

pub type VariableKind {
  Let
  Const
  Var
  Using
  AwaitUsing
}

pub type VariableDeclarator {
  VariableDeclarator(id: Pattern, init: Option(Expression))
}

pub type LiteralNumber {
  FiniteNumber(value: Float)
  InfiniteNumber
}

// span must stay the first field of every variant
pub type Expression {
  Identifier(span: Span, name: String)
  NumberLiteral(span: Span, value: LiteralNumber)
  BigIntLiteral(span: Span, value: Int)
  StringExpression(span: Span, value: String)
  BooleanLiteral(span: Span, value: Bool)
  NullLiteral(span: Span)
  UndefinedExpression(span: Span)
  BinaryExpression(
    span: Span,
    operator: BinaryOp,
    left: Expression,
    right: Expression,
  )
  LogicalExpression(
    span: Span,
    operator: LogicalOp,
    left: Expression,
    right: Expression,
  )
  UnaryExpression(span: Span, operator: UnaryOp, argument: Expression)
  UpdateExpression(
    span: Span,
    operator: UpdateOp,
    prefix: Bool,
    argument: Expression,
  )
  AssignmentExpression(
    span: Span,
    operator: AssignmentOp,
    left: Expression,
    right: Expression,
  )
  CallExpression(span: Span, callee: Expression, arguments: List(Expression))
  MemberExpression(span: Span, object: Expression, property: MemberProperty)
  OptionalMemberExpression(
    span: Span,
    object: Expression,
    property: MemberProperty,
  )
  OptionalCallExpression(
    span: Span,
    callee: Expression,
    arguments: List(Expression),
  )
  ConditionalExpression(
    span: Span,
    condition: Expression,
    consequent: Expression,
    alternate: Expression,
  )
  NewExpression(span: Span, callee: Expression, arguments: List(Expression))
  ThisExpression(span: Span)
  SuperExpression(span: Span)
  ArrayExpression(span: Span, elements: List(Option(Expression)))
  ObjectExpression(span: Span, properties: List(Property))
  FunctionExpression(
    span: Span,
    name: Option(NamedBinding),
    params: List(Pattern),
    body: List(StmtWithLine),
    is_generator: Bool,
    is_async: Bool,
  )
  ArrowFunctionExpression(
    span: Span,
    params: List(Pattern),
    body: ArrowBody,
    is_async: Bool,
  )
  ClassExpression(
    span: Span,
    name: Option(NamedBinding),
    super_class: Option(Expression),
    body: List(ClassElement),
  )
  YieldExpression(span: Span, argument: Option(Expression), is_delegate: Bool)
  AwaitExpression(span: Span, argument: Expression)
  SequenceExpression(span: Span, expressions: List(Expression))
  SpreadElement(span: Span, argument: Expression)
  TemplateLiteral(span: Span, parts: TemplateParts(String))
  TaggedTemplateExpression(
    span: Span,
    tag: Expression,
    parts: TemplateParts(TemplateQuasi),
  )
  MetaProperty(span: Span, kind: MetaPropertyKind)
  ImportExpression(
    span: Span,
    source: Expression,
    options: Option(Expression),
    phase: ImportPhase,
  )
  RegExpLiteral(span: Span, pattern: String, flags: String)
  ParenthesizedExpression(span: Span, expression: Expression)
  // compiler-only, never produced by the parser
  IntrinsicTemplateObject(span: Span, site: Int, quasis: List(TemplateQuasi))
}

pub fn expression_span(e: Expression) -> Span {
  e.span
}

pub fn binding_name(binding: Option(NamedBinding)) -> Option(String) {
  option.map(binding, fn(b) { b.name })
}

pub type ImportPhase {
  PhaseEvaluation
  PhaseSource
  PhaseDefer
}

pub type ArrowBody {
  ArrowBodyExpression(Expression)
  ArrowBodyBlock(List(StmtWithLine))
}

pub type PropertyKey {
  KeyIdentifier(name: String, span: Span)
  KeyString(value: String, span: Span)
  KeyNumber(value: LiteralNumber, span: Span)
  KeyBigInt(value: Int, span: Span)
  KeyPrivate(name: String, span: Span)
  KeyComputed(expression: Expression)
}

pub fn property_key_span(key: PropertyKey) -> Span {
  case key {
    KeyIdentifier(span:, ..)
    | KeyString(span:, ..)
    | KeyNumber(span:, ..)
    | KeyBigInt(span:, ..)
    | KeyPrivate(span:, ..) -> span
    KeyComputed(expression:) -> expression.span
  }
}

pub fn property_key_static_name(key: PropertyKey) -> Option(String) {
  case key {
    KeyIdentifier(name:, ..) -> Some(name)
    KeyString(value:, ..) -> Some(value)
    KeyNumber(..) | KeyBigInt(..) | KeyPrivate(..) | KeyComputed(..) -> None
  }
}

pub type Property {
  InitProperty(key: PropertyKey, value: Expression, shorthand: Bool)
  MethodProperty(key: PropertyKey, value: FunctionLiteral)
  AccessorProperty(key: PropertyKey, value: FunctionLiteral, kind: AccessorKind)
  SpreadProperty(argument: Expression)
}

pub type AccessorKind {
  GetAccessor
  SetAccessor
}

pub type UpdateOp {
  Increment
  Decrement
}

pub type Pattern {
  IdentifierPattern(name: String, span: Span)
  ArrayPattern(elements: List(Option(Pattern)))
  ObjectPattern(properties: List(PatternProperty))
  AssignmentPattern(left: Pattern, right: Expression)
  RestElement(argument: Pattern)
}

pub type PatternProperty {
  PatternProperty(key: PropertyKey, value: Pattern, shorthand: Bool)
  RestProperty(name: String, span: Span)
}

// §8.2.1 boundnames, in source order
pub fn pattern_bound_names(p: Pattern) -> List(String) {
  case p {
    IdentifierPattern(name:, ..) -> [name]
    ArrayPattern(elements:) ->
      list.flat_map(elements, fn(element) {
        option.map(element, pattern_bound_names) |> option.unwrap([])
      })
    ObjectPattern(properties:) ->
      list.flat_map(properties, fn(property) {
        case property {
          PatternProperty(value:, ..) -> pattern_bound_names(value)
          RestProperty(name:, ..) -> [name]
        }
      })
    AssignmentPattern(left:, ..) -> pattern_bound_names(left)
    RestElement(argument:) -> pattern_bound_names(argument)
  }
}

pub type BinaryOp {
  Add
  Subtract
  Multiply
  Divide
  Modulo
  Exponentiation
  StrictEqual
  StrictNotEqual
  Equal
  NotEqual
  LessThan
  GreaterThan
  LessThanEqual
  GreaterThanEqual
  LeftShift
  RightShift
  UnsignedRightShift
  BitwiseAnd
  BitwiseOr
  BitwiseXor
  In
  InstanceOf
}

pub type LogicalOp {
  LogicalAnd
  LogicalOr
  NullishCoalescing
}

pub type UnaryOp {
  Negate
  UnaryPlus
  LogicalNot
  BitwiseNot
  TypeOf
  Void
  Delete
}

pub type AssignmentOp {
  Assign
  AddAssign
  SubtractAssign
  MultiplyAssign
  DivideAssign
  ModuloAssign
  ExponentiationAssign
  LeftShiftAssign
  RightShiftAssign
  UnsignedRightShiftAssign
  BitwiseAndAssign
  BitwiseOrAssign
  BitwiseXorAssign
  LogicalAndAssign
  LogicalOrAssign
  NullishCoalesceAssign
}
