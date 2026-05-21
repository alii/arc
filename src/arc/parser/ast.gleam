/// Core AST types for the Arc JavaScript parser.
/// Based on the ESTree specification, adapted for Gleam's type system.
import gleam/option.{type Option}

pub type Program {
  Script(body: List(StmtWithLine))
  Module(body: List(ModuleItem))
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
  ImportDeclaration(specifiers: List(ImportSpecifier), source: StringLiteral)
  ExportNamedDeclaration(
    declaration: Option(Statement),
    specifiers: List(ExportSpecifier),
    source: Option(StringLiteral),
  )
  ExportDefaultDeclaration(declaration: Expression)
  ExportAllDeclaration(exported: Option(String), source: StringLiteral)
}

pub type ImportSpecifier {
  ImportDefaultSpecifier(local: String)
  ImportNamespaceSpecifier(local: String)
  ImportNamedSpecifier(imported: String, local: String)
}

pub type ExportSpecifier {
  ExportSpecifier(local: String, exported: String)
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
  FunctionDeclaration(
    name: Option(String),
    params: List(Pattern),
    body: Statement,
    is_generator: Bool,
    is_async: Bool,
  )
  ClassDeclaration(
    name: Option(String),
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
}

pub type VariableDeclarator {
  VariableDeclarator(id: Pattern, init: Option(Expression))
}

pub type Expression {
  Identifier(name: String)
  NumberLiteral(value: Float)
  StringExpression(value: String)
  BooleanLiteral(value: Bool)
  NullLiteral
  UndefinedExpression
  BinaryExpression(operator: BinaryOp, left: Expression, right: Expression)
  LogicalExpression(operator: BinaryOp, left: Expression, right: Expression)
  UnaryExpression(operator: UnaryOp, prefix: Bool, argument: Expression)
  UpdateExpression(operator: UpdateOp, prefix: Bool, argument: Expression)
  AssignmentExpression(
    operator: AssignmentOp,
    left: Expression,
    right: Expression,
  )
  CallExpression(callee: Expression, arguments: List(Expression))
  MemberExpression(object: Expression, property: Expression, computed: Bool)
  OptionalMemberExpression(
    object: Expression,
    property: Expression,
    computed: Bool,
  )
  OptionalCallExpression(callee: Expression, arguments: List(Expression))
  ConditionalExpression(
    condition: Expression,
    consequent: Expression,
    alternate: Expression,
  )
  NewExpression(callee: Expression, arguments: List(Expression))
  ThisExpression
  SuperExpression
  ArrayExpression(elements: List(Option(Expression)))
  ObjectExpression(properties: List(Property))
  FunctionExpression(
    name: Option(String),
    params: List(Pattern),
    body: Statement,
    is_generator: Bool,
    is_async: Bool,
  )
  ArrowFunctionExpression(
    params: List(Pattern),
    body: ArrowBody,
    is_async: Bool,
  )
  ClassExpression(
    name: Option(String),
    super_class: Option(Expression),
    body: List(ClassElement),
  )
  YieldExpression(argument: Option(Expression), is_delegate: Bool)
  AwaitExpression(argument: Expression)
  SequenceExpression(expressions: List(Expression))
  SpreadElement(argument: Expression)
  TemplateLiteral(quasis: List(String), expressions: List(Expression))
  TaggedTemplateExpression(tag: Expression, quasi: Expression)
  MetaProperty(meta: String, property: String)
  ImportExpression(source: Expression, options: Option(Expression))
  RegExpLiteral(pattern: String, flags: String)
  /// Preserves parenthesization so the compiler can distinguish `x` from `(x)`.
  /// Needed for ES spec §13.15.2: IsIdentifierRef returns false for
  /// CoverParenthesizedExpressionAndArrowParameterList.
  ParenthesizedExpression(expression: Expression)
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
  IdentifierPattern(name: String)
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
