/// Core AST types for the Arc JavaScript parser.
/// Based on the ESTree specification, adapted for Gleam's type system.
import gleam/option.{type Option}

pub type Program {
  Script(body: List(StmtWithLine))
  Module(body: List(ModuleItem))
}

/// A half-open `[start, end)` range of UTF-8 byte offsets into the original
/// source text. Slicing the source by these offsets must reproduce the exact
/// original text of the node. Byte offsets (not UTF-16 columns) are stored so
/// the conversion to Source Map v3 columns happens once, at map-emit time.
pub type Span {
  Span(start: Int, end: Int)
}

/// A statement paired with the 1-based source line where it begins. Statement
/// *lists* (block bodies, program bodies, switch-case bodies) hold `Stmt` so the
/// compiler can emit `SetLine` ops and build line numbers for `Error.stack`.
/// Sub-statements held singly (an `if` consequent, a loop body) stay bare
/// `Statement` and inherit the enclosing line.
pub type StmtWithLine {
  StmtWithLine(line: Int, statement: Statement)
}

pub type ModuleItem {
  StatementItem(StmtWithLine)
  /// `phase` is per-declaration because the grammar can't mix phases:
  /// `import defer` only takes the namespace form (PhaseDefer) and
  /// `import source` only a single binding (PhaseSource); everything else
  /// is PhaseEvaluation. Mirrors ESTree's ImportDeclaration.phase.
  ImportDeclaration(
    specifiers: List(ImportSpecifier),
    source: StringLiteral,
    phase: ImportPhase,
    span: Span,
  )
  ExportNamedDeclaration(
    declaration: Option(Statement),
    specifiers: List(ExportSpecifier),
    source: Option(StringLiteral),
    span: Span,
  )
  ExportDefaultDeclaration(declaration: Expression, span: Span)
  ExportAllDeclaration(
    exported: Option(String),
    source: StringLiteral,
    span: Span,
  )
}

pub type ImportSpecifier {
  ImportDefaultSpecifier(local: String, local_span: Span)
  /// `import * as local` / `import defer * as local` — eager vs deferred is
  /// the enclosing declaration's `phase`. `local_span` covers the `local`
  /// binding identifier only (not the `* as` prefix).
  ImportNamespaceSpecifier(local: String, local_span: Span)
  ImportNamedSpecifier(imported: String, local: String, local_span: Span)
}

/// `local_span` is the byte span of the local-binding identifier — the name
/// before `as` in `export { local as exported }`, or the whole identifier in
/// `export { local }`. It is a reference to a module-local binding.
pub type ExportSpecifier {
  ExportSpecifier(local: String, exported: String, local_span: Span)
}

pub type StringLiteral {
  StringLit(value: String)
}

pub type Statement {
  EmptyStatement
  /// `directive` holds the raw (un-decoded) string content when this statement
  /// is a lone string-literal expression (a Directive in the spec sense), else
  /// None. Directive prologue detection ("use strict") must compare the raw
  /// text, since an escaped/continued literal like `'use strict'` is not a
  /// directive even though its decoded value is "use strict".
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
  TryStatement(
    block: Statement,
    handler: Option(CatchClause),
    finalizer: Option(Statement),
  )
  BreakStatement(label: Option(String))
  ContinueStatement(label: Option(String))
  DebuggerStatement
  LabeledStatement(label: String, body: Statement)
  WithStatement(object: Expression, body: Statement)
  /// `name_span` is the byte span of the declaration's name identifier (the
  /// binding introduced by `function f`), or `None` when the declaration is
  /// anonymous (only possible for `export default function`). Slicing the
  /// source by this span returns the exact name text.
  FunctionDeclaration(
    name: Option(String),
    name_span: Option(Span),
    params: List(Pattern),
    body: Statement,
    is_generator: Bool,
    is_async: Bool,
  )
  /// `name_span` is the byte span of the class name identifier, or `None` for
  /// an anonymous class (`export default class`).
  ClassDeclaration(
    name: Option(String),
    name_span: Option(Span),
    super_class: Option(Expression),
    body: List(ClassElement),
  )
  /// Internal-only — never produced by the parser. Synthesized by class
  /// compilation for §7.3.32 DefineField (CreateDataPropertyOrThrow on `this`).
  /// Unlike `this.x = v` (which uses [[Set]] and triggers prototype setters),
  /// this compiles to IrDefineField / IrDefineFieldComputed ([[DefineOwnProperty]]).
  ClassFieldInit(key: Expression, value: Option(Expression), computed: Bool)
}

pub type ForInit {
  ForInitExpression(Expression)
  ForInitDeclaration(Statement)
  ForInitPattern(Pattern)
}

pub type SwitchCase {
  SwitchCase(condition: Option(Expression), consequent: List(StmtWithLine))
}

pub type CatchClause {
  CatchClause(param: Option(Pattern), body: Statement)
}

pub type ClassElement {
  ClassMethod(
    key: Expression,
    value: Expression,
    kind: MethodKind,
    is_static: Bool,
    computed: Bool,
  )
  ClassField(
    key: Expression,
    value: Option(Expression),
    is_static: Bool,
    computed: Bool,
  )
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
  /// `using x = expr` — Explicit Resource Management. Const-like binding
  /// whose resource is disposed ([Symbol.dispose]) when the containing
  /// block/loop/function body completes. Desugared by the compiler before
  /// op emission — emit never sees this kind in a compiled declaration.
  Using
  /// `await using x = expr` — async variant ([Symbol.asyncDispose], awaited).
  AwaitUsing
}

pub type VariableDeclarator {
  VariableDeclarator(id: Pattern, init: Option(Expression))
}

/// Every variant carries `span: Span` as its FIRST field — the half-open
/// `[start, end)` UTF-8 byte range of the WHOLE expression in the original
/// source, including delimiters: `(x)` spans both parens, `f(a)` runs from
/// `f` to `)`, `a + b` from start of `a` to end of `b`. Span is positionally
/// first (not last) because Gleam's shared-field accessor requires the field
/// at the same name, type AND position across every variant; first is the
/// only position common to all 36 constructors. This enables the universal
/// accessor `expr.span` for any `Expression` value. Construction sites should
/// use labelled arguments (`Identifier(name:, span:)`) so field order is
/// irrelevant; pattern-match sites that bind other fields positionally must
/// lead with `_,` for the span or use labelled `..` patterns.
pub type Expression {
  /// A reference to a binding by name. `span` is the half-open byte range of
  /// the identifier token in the source, so a bundler can locate and rewrite
  /// each occurrence (e.g. an imported reference `x` → member access `mod.x`).
  Identifier(span: Span, name: String)
  NumberLiteral(span: Span, value: Float)
  /// BigInt literal (`7n`, `0xFFn`). Value is exact — BEAM ints are bignums.
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
    operator: BinaryOp,
    left: Expression,
    right: Expression,
  )
  UnaryExpression(
    span: Span,
    operator: UnaryOp,
    prefix: Bool,
    argument: Expression,
  )
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
  MemberExpression(
    span: Span,
    object: Expression,
    property: Expression,
    computed: Bool,
  )
  OptionalMemberExpression(
    span: Span,
    object: Expression,
    property: Expression,
    computed: Bool,
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
  /// `name_span` is the byte span of the optional self-name identifier in a
  /// named function expression (`const f = function g() {}` -> span of `g`),
  /// or `None` for an anonymous function expression. The self-name binding is
  /// visible only inside the expression body (§13.2.5.5).
  FunctionExpression(
    span: Span,
    name: Option(String),
    name_span: Option(Span),
    params: List(Pattern),
    body: Statement,
    is_generator: Bool,
    is_async: Bool,
  )
  ArrowFunctionExpression(
    span: Span,
    params: List(Pattern),
    body: ArrowBody,
    is_async: Bool,
  )
  /// `name_span` is the byte span of the optional class-expression name
  /// (`const C = class D {}` -> span of `D`), or `None` when anonymous. The
  /// name binding is visible only inside the class body.
  ClassExpression(
    span: Span,
    name: Option(String),
    name_span: Option(Span),
    super_class: Option(Expression),
    body: List(ClassElement),
  )
  YieldExpression(span: Span, argument: Option(Expression), is_delegate: Bool)
  AwaitExpression(span: Span, argument: Expression)
  SequenceExpression(span: Span, expressions: List(Expression))
  SpreadElement(span: Span, argument: Expression)
  TemplateLiteral(
    span: Span,
    quasis: List(String),
    expressions: List(Expression),
  )
  /// Tagged template: tag`raw0 ${e0} raw1`. `cooked` holds the decoded
  /// template values (None when a quasi contains an invalid escape sequence —
  /// legal in tagged templates, the cooked entry becomes undefined). `raw`
  /// holds the verbatim source text of each quasi (line endings normalized
  /// to LF per the spec's TRV definition).
  TaggedTemplateExpression(
    span: Span,
    tag: Expression,
    cooked: List(Option(String)),
    raw: List(String),
    expressions: List(Expression),
  )
  MetaProperty(span: Span, meta: String, property: String)
  ImportExpression(
    span: Span,
    source: Expression,
    options: Option(Expression),
    phase: ImportPhase,
  )
  RegExpLiteral(span: Span, pattern: String, flags: String)
  /// Preserves parenthesization so the compiler can distinguish `x` from `(x)`.
  /// Needed for ES spec §13.15.2: IsIdentifierRef returns false for
  /// CoverParenthesizedExpressionAndArrowParameterList.
  ParenthesizedExpression(span: Span, expression: Expression)
  /// Internal-only — never produced by the parser. Synthesized by the
  /// compiler when lowering TaggedTemplateExpression: evaluates to the
  /// per-site cached template object (GetTemplateObject, §13.2.8.4).
  /// `site` is a globally unique call-site id baked in at compile time.
  IntrinsicTemplateObject(
    span: Span,
    site: Int,
    cooked: List(Option(String)),
    raw: List(String),
  )
}

/// Universal accessor for the source span of any `Expression`. Equivalent to
/// `e.span` (which works because every variant carries `span: Span` at the
/// same first position) — kept as a named function for use as a first-class
/// value in `list.map` and similar.
pub fn expression_span(e: Expression) -> Span {
  e.span
}

/// Module request phase, shared by static ImportDeclarations and dynamic
/// ImportCalls (§13.3.10): evaluation (`import x from "m"` / `import(x)`),
/// source (`import source x from "m"` / `import.source(x)`, the
/// source-phase-imports proposal), defer (`import defer * as x from "m"` /
/// `import.defer(x)`, the defer-import-eval proposal).
pub type ImportPhase {
  PhaseEvaluation
  PhaseSource
  PhaseDefer
}

pub type ArrowBody {
  ArrowBodyExpression(Expression)
  ArrowBodyBlock(Statement)
}

pub type Property {
  Property(
    key: Expression,
    value: Expression,
    kind: PropertyKind,
    computed: Bool,
    shorthand: Bool,
    method: Bool,
  )
  SpreadProperty(argument: Expression)
}

pub type PropertyKind {
  Init
  Get
  Set
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
  PatternProperty(
    key: Expression,
    value: Pattern,
    computed: Bool,
    shorthand: Bool,
  )
  RestProperty(argument: Pattern)
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
  LogicalAnd
  LogicalOr
  NullishCoalescing
  In
  InstanceOf
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
