/// Core AST types for the Arc JavaScript parser.
/// Based on the ESTree specification, adapted for Gleam's type system.
import gleam/list
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

/// A binding identifier together with the byte span of its name token in the
/// source. Used wherever a declaration/expression carries an OPTIONAL name
/// (function/class declarations and expressions): pairing the two makes
/// "a name without its span" (or vice versa) unrepresentable — either both
/// are present (`Some(NamedBinding(..))`) or neither is (`None`). Slicing the
/// source by `span` returns the exact name text.
pub type NamedBinding {
  NamedBinding(name: String, span: Span)
}

/// One quasi (literal chunk) of a tagged template. `cooked` is the decoded
/// template value — `None` when the quasi contains an invalid escape
/// sequence, which is legal in tagged templates and yields `undefined`
/// (§12.9.6). `raw` is the verbatim source text of the quasi (line endings
/// normalized to LF per the spec's TRV definition). Pairing them per-quasi
/// makes a cooked/raw length mismatch unrepresentable.
pub type TemplateQuasi {
  TemplateQuasi(cooked: Option(String), raw: String)
}

/// Which meta property a `MetaProperty` expression is. The grammar only has
/// these two (§13.3.12 `new.target`, §13.3.13 `import.meta`), so anything
/// else is unrepresentable.
pub type MetaPropertyKind {
  NewTarget
  ImportMeta
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
  /// `export <VariableStatement | Declaration>` (§16.2.3.1, first
  /// production). Split from `ExportNamed` because no production has both a
  /// declaration and specifiers/`from` — folding them into one node made
  /// three states representable that the grammar cannot spell.
  ///
  /// `line` is the 1-based source line of the `export` keyword. The compiler
  /// unwraps this item to a bare statement (`ast_util.module_items_to_stmts`)
  /// and needs a real line to tag it with; `0` is the emitter's "no line
  /// info" sentinel, so a runtime error thrown from `export function f(){}`
  /// would otherwise have no line at all.
  ExportDeclaration(declaration: Declaration, line: Int, span: Span)
  /// `export { a, b as c }` and `export { a } from "m"` / `export {} from "m"`
  /// (§16.2.3.1, remaining productions). Never carries a declaration.
  ExportNamed(
    specifiers: List(ExportSpecifier),
    source: Option(StringLiteral),
    span: Span,
  )
  /// `line` is the 1-based source line of the `export` keyword — see
  /// `ExportDeclaration.line`.
  ExportDefaultDeclaration(declaration: Expression, line: Int, span: Span)
  ExportAllDeclaration(
    exported: Option(String),
    source: StringLiteral,
    span: Span,
  )
}

/// The three declaration forms `export` admits (§16.2.3.1: `export`
/// VariableStatement, `export` Declaration). Modelling them as their own type
/// — rather than `Option(Statement)` on the export node — makes
/// `export while (x) {}` and friends unspellable, so no consumer needs an
/// impossible arm to unwrap an exported declaration.
pub type Declaration {
  DeclVariable(kind: VariableKind, declarations: List(VariableDeclarator))
  DeclFunction(function: FunctionLiteral)
  DeclClass(
    name: Option(NamedBinding),
    super_class: Option(Expression),
    body: List(ClassElement),
  )
}

/// Re-wrap an exported declaration as the equivalent bare `Statement`, which
/// is what the compiler lowers a module body to. Total — every `Declaration`
/// has exactly one `Statement` counterpart.
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
  /// `block` and `finalizer` are the raw statement lists of the `{...}`
  /// Blocks the grammar requires (§14.15) — not `Statement`s. A `try` whose
  /// block "wasn't a Block" is unspellable, so no consumer needs a fallback.
  TryStatement(
    block: List(StmtWithLine),
    handler: Option(CatchClause),
    finalizer: Option(List(StmtWithLine)),
  )
  BreakStatement(label: Option(String))
  ContinueStatement(label: Option(String))
  DebuggerStatement
  LabeledStatement(label: String, body: Statement)
  WithStatement(object: Expression, body: Statement)
  /// `name` is the declaration's name identifier (the binding introduced by
  /// `function f`) with its source span, or `None` when the declaration is
  /// anonymous (only possible for `export default function`).
  FunctionDeclaration(
    name: Option(NamedBinding),
    params: List(Pattern),
    body: List(StmtWithLine),
    is_generator: Bool,
    is_async: Bool,
  )
  /// `name` is the class name identifier with its source span, or `None` for
  /// an anonymous class (`export default class`).
  ClassDeclaration(
    name: Option(NamedBinding),
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
  /// A `var`/`let`/`const`/`using`/`await using` declaration in a for head.
  /// Carries the declaration payload directly (rather than a `Statement`) so
  /// a for-init can never hold anything but a variable declaration.
  ForInitDeclaration(kind: VariableKind, declarations: List(VariableDeclarator))
  ForInitPattern(Pattern)
}

pub type SwitchCase {
  SwitchCase(condition: Option(Expression), consequent: List(StmtWithLine))
}

/// `body` is the raw statement list of the catch Block (§14.15) — the
/// grammar admits nothing else there.
pub type CatchClause {
  CatchClause(param: Option(Pattern), body: List(StmtWithLine))
}

/// The function a method definition (§15.4) or a `function`/`class` element
/// denotes, WITHOUT the expression wrapper. `ClassMethod.value` holds one of
/// these rather than an `Expression`, so "a class method whose value is not a
/// function" is unrepresentable and no consumer needs a fallback arm.
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
    key: Expression,
    value: FunctionLiteral,
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

/// The Number value a NumericLiteral denotes. Isomorphic to the subset of
/// `vm/value.JsNum` a literal can reach: a literal carries no sign (unary
/// minus is a separate operator) and can never denote NaN, so the only
/// non-finite value it takes is +Infinity — `1e400`, whose mathematical value
/// overflows the IEEE double range. BEAM's Float has no infinity, hence the
/// dedicated constructor rather than clamping to Number.MAX_VALUE.
pub type LiteralNumber {
  FiniteNumber(value: Float)
  InfiniteNumber
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
  NumberLiteral(span: Span, value: LiteralNumber)
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
  /// `name` is the optional self-name identifier (with its span) in a named
  /// function expression (`const f = function g() {}` -> `g`), or `None` for
  /// an anonymous function expression. The self-name binding is visible only
  /// inside the expression body (§13.2.5.5).
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
  /// `name` is the optional class-expression name (with its span,
  /// `const C = class D {}` -> `D`), or `None` when anonymous. The name
  /// binding is visible only inside the class body.
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
  TemplateLiteral(
    span: Span,
    quasis: List(String),
    expressions: List(Expression),
  )
  /// Tagged template: tag`raw0 ${e0} raw1`. Each quasi carries its cooked
  /// value (None for an invalid escape sequence — legal in tagged templates,
  /// the entry becomes undefined) alongside its verbatim raw text.
  TaggedTemplateExpression(
    span: Span,
    tag: Expression,
    quasis: List(TemplateQuasi),
    expressions: List(Expression),
  )
  /// `new.target` / `import.meta` (§13.3.12, §13.3.13).
  MetaProperty(span: Span, kind: MetaPropertyKind)
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
  IntrinsicTemplateObject(span: Span, site: Int, quasis: List(TemplateQuasi))
}

/// Universal accessor for the source span of any `Expression`. Equivalent to
/// `e.span` (which works because every variant carries `span: Span` at the
/// same first position) — kept as a named function for use as a first-class
/// value in `list.map` and similar.
pub fn expression_span(e: Expression) -> Span {
  e.span
}

/// The plain name text of an optional `NamedBinding` — for the many consumers
/// (emit, esm, compiler) that only need the identifier, not its span.
pub fn binding_name(binding: Option(NamedBinding)) -> Option(String) {
  option.map(binding, fn(b) { b.name })
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
  /// The raw statement list of the arrow's `{...}` body — the grammar admits
  /// nothing else, so consumers never need a "wasn't a Block" fallback.
  ArrowBodyBlock(List(StmtWithLine))
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

/// §8.2.1 BoundNames of a BindingPattern, in source order. This is the single
/// place that answers "which identifiers does this pattern bind?" — use it
/// instead of matching `IdentifierPattern` at a call site, which silently
/// drops every name bound by a destructuring pattern.
pub fn pattern_bound_names(p: Pattern) -> List(String) {
  case p {
    IdentifierPattern(name:, ..) -> [name]
    ArrayPattern(elements:) ->
      // Elisions (`[a, , b]`) are `None` slots — they bind nothing.
      list.flat_map(elements, fn(element) {
        option.map(element, pattern_bound_names) |> option.unwrap([])
      })
    ObjectPattern(properties:) ->
      list.flat_map(properties, fn(property) {
        case property {
          PatternProperty(value:, ..) -> pattern_bound_names(value)
          RestProperty(argument:) -> pattern_bound_names(argument)
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

/// Short-circuiting operator of a `LogicalExpression` (`&&` / `||` / `??`).
/// Separate from `BinaryOp` so a LogicalExpression can never carry an
/// arithmetic/relational operator (and vice versa).
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
