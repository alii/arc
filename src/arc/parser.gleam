/// JavaScript parser for Arc.
/// Recursive descent parser for ES2023+ strict mode JavaScript.
///
/// The parser lexes ON DEMAND from a scanner cursor (`lexer.Scanner`),
/// holding only a small prefetch window of upcoming tokens — never the
/// whole file. That is what lets it re-scan the constructs a standalone
/// token grammar cannot classify (regex literals, template continuations)
/// from source and simply continue after them.
/// Uses Pratt parsing for expression precedence.
///
/// Submodules:
///   parser/error        — ParseError type, formatting, position extraction
///   parser/token        — TokenKind classification and conversion
///   parser/number       — Numeric literal classification (Number vs BigInt)
///   parser/regex        — Regex literal scanning and /u validation
///
/// The mutually-recursive core (statements ↔ expressions ↔ patterns) lives
/// here since Gleam doesn't support cross-module recursion.
import arc/compiler/ast_util
import arc/compiler/scope
import arc/parser/ast
import arc/parser/error.{
  ArgumentsInClassFieldInit, ArgumentsInStaticBlock, AwaitInAsyncFunction,
  AwaitInModule, AwaitInStaticBlock, BreakOutsideLoopOrSwitch,
  ClassConstructorAsync, ClassConstructorGenerator, ClassConstructorNotGetter,
  ClassConstructorNotSetter, ClassDuplicateConstructor, CoalesceMixedWithLogical,
  ContinueOutsideLoop, ContinueToNonIterationLabel, DeletePrivateName,
  DeleteUnqualifiedStrictMode, DestructuringMissingInitializer,
  DuplicateBindingLexical, DuplicateDefaultCase, DuplicateExport,
  DuplicateImportBinding, DuplicateLabel, DuplicateParamNameStrictMode,
  DuplicateParameterName, DuplicatePrivateName, DuplicateProtoProperty,
  EnumReservedWord, EscapedReservedWord, EvalArgsAssignStrictMode,
  ExpectedAfterOptionalChain, ExpectedAsOrFromAfterExportStar,
  ExpectedBindingPattern, ExpectedBraceOrStarAfterComma,
  ExpectedCallOrDotAfterImport, ExpectedCaseDefaultOrBrace,
  ExpectedCloseAfterSetter, ExpectedCommaOrBraceInExport,
  ExpectedCommaOrBraceInImport, ExpectedCommaOrBraceInObject,
  ExpectedCommaOrBracket, ExpectedCommaOrBracketInExpr,
  ExpectedCommaOrCloseParen, ExpectedCommaOrObjectClose, ExpectedExportAlias,
  ExpectedExportSpecifierName, ExpectedForDeclSeparator,
  ExpectedForHeadSeparator, ExpectedForSeparator, ExpectedFromOrComma,
  ExpectedFunctionAfterAsync, ExpectedIdentifier, ExpectedIdentifierAfterDot,
  ExpectedImportMeta, ExpectedImportSpecifier, ExpectedImportSpecifierName,
  ExpectedModuleSpecifier, ExpectedNewTarget, ExpectedPropertyName,
  ExpectedSemicolon, ExpectedToken, ExportNotTopLevel, FieldNamedConstructor,
  ForInInitializer, ForOfInitializer, FunctionDeclInLabelBody,
  FunctionDeclInSingleStatement, GeneratorDeclLabeled, GetterNoParams,
  IdentifierAlreadyDeclared, ImportNotTopLevel, InvalidAssignmentLhs,
  InvalidDestructuringTarget, InvalidForInLhs, InvalidForOfLhs,
  InvalidLhsPrefixOp, InvalidPostfixLhs, InvalidRestBinding,
  InvalidTemplateEscape, LetBindingInLexicalDecl, LetIdentifierStrictMode,
  LexicalDeclInLabel, LexicalDeclInSingleStatement, MalformedNumericLiteral,
  MisplacedUseStrictDirective, MissingCatchOrFinally, MissingConstInitializer,
  NewTargetOutsideFunction, OctalEscapeStrictMode, OctalLiteralStrictMode,
  PrivateNameAsPropertyKey, PrivateNameConstructor, ReservedWordImportBinding,
  ReservedWordStrictMode, RestDefaultInitializer, RestMustBeLast,
  RestTrailingComma, ReturnOutsideFunction, SetterExactlyOneParam, SetterNoRest,
  ShorthandDefaultOutsideDestructuring, StaticPrototype,
  StaticReservedStrictMode, StrictModeAssignment, StrictModeBindingName,
  StrictModeModification, StrictModeParamName, SuperCallNotInDerivedConstructor,
  SuperPrivateName, SuperPropertyNotInMethod, ThrowLineBreak,
  UndeclaredExportBinding, UndeclaredPrivateName, UndefinedLabel,
  UnexpectedAfterExport, UnexpectedCloseBrace, UnexpectedCloseParen,
  UnexpectedExport, UnexpectedSuper, UnexpectedToken,
  UnicodeEscapeInMetaProperty, UnterminatedTemplateSubstitution,
  UsingAtScriptTopLevel, UsingInCaseClause, UsingInForIn,
  UsingMissingInitializer, UsingPatternBinding, WithNotAllowedStrictMode,
  YieldInFormalParameter, YieldInGenerator, YieldReservedStrictMode, lex_error,
  regexp_syntax_error,
}
import arc/parser/lexer.{
  type Token, type TokenKind, AmpersandAmpersandEqual, AmpersandEqual, Arrow, As,
  Async, Await, Bang, Break, CaretEqual, Case, Catch, Class, Colon, Comma, Const,
  Continue, Debugger, Default, Delete, Do, Dot, DotDotDot, Else, Eof, Equal,
  Export, Extends, Finally, For, From, Function, GreaterThanGreaterThanEqual,
  GreaterThanGreaterThanGreaterThanEqual, Identifier, If, Illegal, Import, In,
  KFalse, KString, KTrue, LeftBrace, LeftBracket, LeftParen,
  LessThanLessThanEqual, Let, LexFailure, Minus, MinusEqual, MinusMinus, New,
  Null, Number, Of, PercentEqual, PipeEqual, PipePipeEqual, Plus, PlusEqual,
  PlusPlus, Question, QuestionDot, QuestionQuestionEqual, Return, RightBrace,
  RightBracket, RightParen, Semicolon, Slash, SlashEqual, Star, StarEqual,
  StarStar, StarStarEqual, Static, Super, Switch, TemplateHead, TemplateLiteral,
  This, Throw, Tilde, Try, Typeof, Undefined, Var, Void, While, With, Yield,
}
import arc/parser/number
import arc/parser/regex
import arc/parser/source_bytes
import arc/parser/token.{
  Binary, BinaryOperator, Coalesce, ShortCircuit, assignment_op, binary_operator,
  is_contextual_keyword, is_identifier_or_keyword, is_keyword_as_identifier,
  is_reserved_word_kind,
}
import arc/vm/lexical
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

/// Decode JS string literal escape sequences (\n, \t, \xNN, \uNNNN, etc.)
/// per ES2024 §12.9.4. The lexer validates escape syntax; this transforms
/// the raw content into the cooked string value.
@external(erlang, "arc_escape_ffi", "decode_string_escapes")
fn decode_string_escapes(raw: String) -> String

/// Compute a template quasi's Template Value (TV, ES2024 §12.9.6) from its
/// raw text. Error(Nil) when the quasi contains an escape sequence that is
/// invalid in templates — a SyntaxError for untagged templates, an undefined
/// cooked entry for tagged ones.
@external(erlang, "arc_escape_ffi", "cook_template_string")
fn cook_template_string(raw: String) -> Result(String, Nil)

pub type ParseMode {
  Script
  Module
}

// Re-export ParseError type and helpers for callers that import arc/parser.
// The actual type lives in arc/parser/error.
pub type ParseError =
  error.ParseError

pub fn parse_error_to_string(err: ParseError) -> String {
  error.parse_error_to_string(err)
}

pub fn parse_error_pos(err: ParseError) -> Int {
  error.parse_error_pos(err)
}

/// What kind of binding declaration we are currently parsing.
/// Controls how names are registered in the block scope.
type BindingKind {
  /// Not inside any declaration binding.
  BindingNone
  /// Inside a var declaration.
  BindingVar
  /// Inside a let/const/using declaration. Carries the scope binding
  /// kind so `register_scope_binding` can pass it through to
  /// `sb_declare` — `const`/`using`/`await using` must land as
  /// ConstBinding (legacy `declare_direct_lexicals`), not LetBinding,
  /// or emit's `IrThrowConstAssign` / `IrDeclareGlobalLex(is_const)`
  /// paths never fire.
  ///
  /// `bound` is the set of binding names seen so far in THIS declaration,
  /// used to reject `let a, a;` (§14.3.1). It lives on the variant, not on
  /// `Ctx`, so a `BindingVar`/`BindingParam`/`BindingNone` context can
  /// never carry stale lexical bound-names; and "`let` is forbidden as a
  /// binding name" (§13.3.1.1) is exactly `BindingLexical(..)` rather than
  /// a separately-maintained `in_lexical_decl` flag that could disagree.
  BindingLexical(kind: scope.BindingKind, bound: Set(String))
  /// Inside formal parameter parsing (treated as lexical for scope purposes).
  BindingParam
}

/// Whether the parser is currently inside a let/const/using declarator list.
fn in_lexical_decl(ctx: Ctx) -> Bool {
  case ctx.binding_kind {
    BindingLexical(..) -> True
    BindingNone | BindingVar | BindingParam -> False
  }
}

/// What the statement behind a label is. `continue L` is only legal when L
/// denotes an IterationStatement (ES2024 §14.9.1 Early Errors); `break L`
/// accepts any label in scope.
type LabelKind {
  /// The label (or the whole `a: b: …` chain it belongs to) directly
  /// prefixes a `for` / `while` / `do` statement.
  LoopLabel
  /// The label prefixes any other statement.
  PlainLabel
}

/// Which statement a `break L` / `continue L` label reference comes from.
/// Decides how strict `parse_optional_label` is about the label's kind.
type LabelUse {
  BreakLabel
  ContinueLabel
}

/// The `get` / `set` accessor prefix (if any) detected ahead of a method
/// definition's property name, in both object literals and class bodies.
type AccessorPrefix {
  /// No `get`/`set` prefix — a plain method (or field / data property).
  NoAccessor
  /// `get name(...) { ... }`
  GetPrefix
  /// `set name(...) { ... }`
  SetPrefix
}

/// The nesting-sensitive parsing context.
///
/// Everything a function boundary saves and restores lives here, as ONE
/// record: `enter_function_context` overwrites it for the body and
/// `restore_outer_context` puts the outer record back wholesale
/// (`ctx: outer.ctx`). Adding a field here therefore restores it
/// automatically — there is no per-field list to forget an entry in.
/// (That is exactly how a nested function body's retroactive
/// `"use strict"` used to leak `strict: True` into the enclosing sloppy
/// scope: `strict` was missing from the hand-written restore list; and how
/// `has_non_simple_param` used to leak OUT of `function f({a}) { … }`'s
/// parameter list into a nested arrow's body, rejecting the arrow's
/// perfectly legal `"use strict"` directive.)
///
/// Corollary: a `P` field that must survive across `restore_outer_context`
/// (e.g. `class_private_depth`, the accumulator lists/sets, `sb`) must
/// stay OUT of this record.
type Ctx {
  Ctx(
    strict: Bool,
    // The grammar's [In] parameter: False only inside a `for` head's
    // initializer, where `in` must read as the for-in keyword rather than a
    // relational operator. Nesting-sensitive: a function/method boundary is
    // always [+In] (params and body), and a `for` head nested inside one
    // must not leak its restriction back out — hence a Ctx field, restored
    // wholesale by `restore_outer_context`. Expression brackets that
    // re-enable it (`[…]`, `(…)`, arguments, literals) go through
    // `with_allow_in`.
    allow_in: Bool,
    // Context tracking for semantic validation
    function_depth: Int,
    loop_depth: Int,
    switch_depth: Int,
    // Labels currently in scope, innermost first, each tagged with whether
    // it names an IterationStatement (see LabelKind). Reset at function
    // boundaries.
    label_set: List(#(String, LabelKind)),
    in_generator: Bool,
    in_async: Bool,
    // §15.7.1 ClassStaticBlock: [+Await] but ContainsAwait/ContainsArguments
    // must be false. Distinct from in_async because `await expr` is also
    // forbidden (not just await-as-identifier). Cleared at function boundaries.
    in_static_block: Bool,
    // §15.7.10 FieldDefinition: ContainsArguments of Initializer must be
    // false. Set while parsing a class field initializer; arrows inherit it,
    // ordinary function boundaries clear it.
    in_class_field_init: Bool,
    // Whether we are inside a method definition (class or object literal).
    // Methods always forbid duplicate parameter names even in sloppy mode.
    in_method: Bool,
    // new.target is allowed in any function/method body (arrows inherit from
    // enclosing). Distinct from function_depth>0 because arrows increment
    // function_depth but must NOT enable new.target on their own.
    allow_new_target: Bool,
    // super() is only allowed in constructors of derived classes
    allow_super_call: Bool,
    // super.x is allowed in any method (class or object literal)
    allow_super_property: Bool,
    // What kind of binding we are currently parsing. `BindingLexical` also
    // carries the names already bound by the current let/const declarator
    // list — the two used to be separate `in_lexical_decl`/`decl_bound_names`
    // Ctx fields that had to be saved and restored in lockstep with this one.
    binding_kind: BindingKind,
    // Whether we are inside a block scope (as opposed to function/script
    // top-level). Affects function declaration scoping: in blocks, function
    // decls are lexical; at function/script top level in sloppy mode, they
    // are var-like.
    in_block: Bool,
    module_top_level: Bool,
    // Whether we are in a single-statement position (body of if/for/while/
    // do-while/with). Used to forbid labeled function declarations in these
    // contexts per spec Annex B 3.4.
    in_single_stmt_pos: Bool,
    // ---- Deferred cover-grammar early errors -------------------------
    // Both are raised only when the covering expression is consumed as an
    // ordinary expression rather than as a destructuring pattern; a
    // function boundary neither inherits nor exports them, so they belong
    // here (see `check_cover_grammar_errors` for the consumption points).
    //
    // Whether the expression being parsed contains a cover-grammar
    // initializer like `{a = 0}` in an object literal. This is only valid
    // if the expression ends up as a destructuring pattern (assignment LHS
    // or arrow params). If the expression is used as a normal expression,
    // this must trigger an error.
    has_cover_initializer: Bool,
    // Position of a duplicate non-computed `__proto__` property in the object
    // literal being parsed, if any. §13.2.5.1's duplicate-__proto__ early
    // error is NOT applied when the ObjectLiteral covers an
    // ObjectAssignmentPattern (`({__proto__: a, __proto__: b} = obj)`), so —
    // like has_cover_initializer — the error is deferred: raised when the
    // expression is used as a plain expression, cleared when it's consumed
    // as a destructuring pattern.
    dup_proto_pos: Option(Int),
    // ---- Formal-parameter parse state --------------------------------
    // Whether we are inside formal parameter parsing. When true, yield
    // expressions are forbidden even inside generators.
    in_formal_params: Bool,
    // Whether we are inside arrow function parameter parsing. When true,
    // duplicate parameter names are always an error (even in sloppy mode).
    in_arrow_params: Bool,
    // Whether the current formal parameter list contains non-simple params
    // (destructuring, defaults, or rest). When true, duplicate parameter
    // names are always forbidden even in sloppy mode.
    has_non_simple_param: Bool,
    // Parameter names accumulated during formal parameter parsing.
    // Used to detect duplicate parameter names across all params including
    // destructured ones. Populated when in_formal_params is True.
    param_bound_names: List(String),
    // Function name saved during function parsing. When
    // check_use_strict_in_body retroactively enables strict mode, this name
    // is validated to reject eval/arguments.
    pending_strict_name: Option(String),
    // Whether we are currently parsing an export var/let/const declaration.
    // When true, each binding name is also registered as an export name.
    // Cleared at function boundaries so a nested function's params (e.g.
    // `export const {a = function(r){}} = obj`) don't register `r` as an
    // export.
    in_export_decl: Bool,
    // Whether the next statement is DIRECTLY in a CaseClause/DefaultClause
    // statement list (not nested in a block). using/await-using declarations
    // are early errors there. Reset by blocks and function boundaries.
    in_case_clause: Bool,
  )
}

/// Internal parser state: the upcoming-token window + the scanner it is
/// filled from + the line of the last consumed token.
type P {
  P(
    // Prefetched upcoming tokens (a bounded window, NOT the whole file):
    // `advance` tops it up from `scan` on demand. Always ends at the first
    // Eof it reaches; nothing past the window has been lexed yet, so the
    // parser can re-scan context-dependent constructs (regex literals,
    // template parts) from source and just continue after them.
    tokens: List(Token),
    // The on-demand lexing cursor the window is filled from.
    scan: lexer.Scanner,
    mode: ParseMode,
    prev_line: Int,
    // Byte offset just past the last token consumed by `advance`
    // (pos + raw_len). Used to close a `Span` whose start was captured
    // before parsing, in O(1) without re-measuring the token stream.
    prev_end: Int,
    bytes: BitArray,
    // The nesting-sensitive context, saved/restored WHOLE at function
    // boundaries. See `Ctx`.
    ctx: Ctx,
    // §15.7.1 AllPrivateIdentifiersValid: count of enclosing class bodies.
    // 0 outside any class. Function boundaries do NOT reset it - private
    // access is purely lexical.
    class_private_depth: Int,
    // Unresolved `#name` references: (name, class_private_depth at the
    // reference, position). parse_class_tail resolves refs made inside its
    // body against the declared names and demotes the rest to the outer
    // depth; parse_script/parse_module reject leftovers.
    private_refs: List(#(String, Int, Int)),
    // Direct eval only: private names ("#x") valid via the CALLER's
    // PrivateEnvironment (§19.2.1.1 PerformEval step 5). Refs to these
    // survive resolve_private_refs / the final check instead of erroring.
    // Empty for scripts/modules/indirect eval.
    outer_private_names: List(String),
    // Whether the last parsed expression is a valid assignment/update target
    // (identifier, member expression). Set by expression parsers, checked by
    // assignment operators and ++/--.
    last_expr_assignable: Bool,
    // Whether the last parsed expression was a plain assignment (target = expr).
    // This is needed to distinguish `{a: b = 1}` (valid pattern: b is target,
    // 1 is default) from `{a: 0}` (invalid pattern: literal). Both have
    // last_expr_assignable=False, but only the assignment covers AssignmentPattern.
    last_expr_is_assignment: Bool,
    // Whether the expression being parsed contains a pattern that would be
    // invalid as a destructuring assignment target. Examples:
    // - [...x, y] (rest element not last in array)
    // - [...0] (non-assignable rest target)
    // - ({get a(){}} = 0) (getter/setter/method in destructuring)
    // - ({a: 0} = 0) (non-assignable property value)
    // Set during expression parsing, checked when accepting destructuring LHS.
    has_invalid_pattern: Bool,
    // Module-level tracking: exported names seen so far.
    // Used to detect duplicate exports like `export {a}; export const a = 1;`.
    export_names: Set(String),
    // Module-level tracking: local names referenced in export specifiers.
    // Each entry is #(local_name, pos). Validated after module parsing.
    // NOT populated for re-exports (export { x } from "module").
    export_local_refs: List(#(String, Int)),
    // Module-level tracking: import binding names seen so far in current
    // import declaration. Used to detect duplicate import bindings like
    // `import {a, a} from "m";`.
    import_bindings: Set(String),
    // Name of the last parsed simple identifier expression (e.g. "eval",
    // "arguments", or any variable name). Cleared by member access, calls,
    // and compound expressions.  Used to check strict-mode restrictions on
    // eval/arguments as operands of ++/-- and for-in/of LHS.
    last_expr_name: Option(String),
    // --- Parse-time scope tree (V8 model) -----------------------------
    // Replaces the old per-block scope_lexical/scope_var/scope_params/
    // scope_funcs/outer_lexical Sets. The parser threads a single
    // `ScopeBuilder` accumulator: `sb_push` at every scope-introducing
    // construct, `sb_declare` at every binding, `sb_enter(outer_id)` at
    // every closing brace.
    // Early-error duplicate-binding checks read the builder's
    // `scopes` dict via `sb_lexical_conflict` / `sb_var_conflicts_lexical`.
    // Accumulated scopes/refs flow FORWARD across the close (only the
    // `current`/`current_fn` cursors move), so a `}` does not discard
    // the inner scope's recorded declarations. Replaces the post-parse
    // scope.analyze AST walks: scopes, bindings and references are recorded
    // here as they are parsed; finalize converts to a ScopeTree.
    sb: scope.ScopeBuilder,
  )
}

/// Parse `then` with the grammar's [In] parameter forced to `value`, then put
/// the CALLER's value back on the returned parser.
///
/// This is the ONE place `ctx.allow_in` is restored, so no call site can pick
/// a slightly different restore point (a bracketed sub-expression that forgot
/// to restore would silently re-enable `in` for the rest of a `for` head, or
/// vice-versa). Function/arrow boundaries do NOT need it — `allow_in` lives on
/// `Ctx`, which `restore_outer_context` puts back wholesale.
///
/// `value: True` at every expression bracket that reintroduces [+In] inside a
/// `for` head (`[…]`, `(…)`, call/`import(…)` arguments, array/object
/// literals, template substitutions); `value: False` at the `for` head itself.
fn with_allow_in(
  p: P,
  value: Bool,
  then: fn(P) -> Result(#(P, a), ParseError),
) -> Result(#(P, a), ParseError) {
  let saved = p.ctx.allow_in
  use #(p, parsed) <- result.map(then(
    P(..p, ctx: Ctx(..p.ctx, allow_in: value)),
  ))
  #(P(..p, ctx: Ctx(..p.ctx, allow_in: saved)), parsed)
}

/// Helper: set last_expr_assignable to False on Ok results.
fn set_not_assignable(
  res: Result(#(P, ast.Expression), ParseError),
) -> Result(#(P, ast.Expression), ParseError) {
  use #(p, expr) <- result.map(res)
  #(P(..p, last_expr_assignable: False, last_expr_is_assignment: False), expr)
}

/// Helper: advance past the current single-token primary expression and wrap
/// `expr` as a non-assignable Ok result. Used for the literal/this/regex arms
/// of parse_primary_expression.
fn ok_lit(
  p: P,
  expr: ast.Expression,
) -> Result(#(P, ast.Expression), ParseError) {
  Ok(#(P(..advance(p), last_expr_assignable: False), expr))
}

/// The two declaration forms `export default` admits (§16.2.3.1:
/// HoistableDeclaration / ClassDeclaration, both with an OPTIONAL name).
/// Narrower than `ast.Statement`, so `default_export_expr` /
/// `default_export_name` below are total — the old `Statement`-shaped
/// helpers each carried a `_ ->` fallback for statements the grammar can
/// never put there.
type DefaultExportDecl {
  DefaultFn(function: ast.FunctionLiteral)
  DefaultClass(
    name: Option(ast.NamedBinding),
    super_class: Option(ast.Expression),
    body: List(ast.ClassElement),
  )
}

/// The declaration re-expressed as the expression `ExportDefaultDeclaration`
/// holds. `decl_span` is the whole-declaration byte span (start at
/// `function`/`async`/`class`, end at the closing `}`) supplied by the caller
/// — the parse helpers return the payload without a whole-node span.
fn default_export_expr(
  decl: DefaultExportDecl,
  decl_span: ast.Span,
) -> ast.Expression {
  case decl {
    DefaultFn(function: ast.FunctionLiteral(
      name:,
      params:,
      body:,
      is_generator:,
      is_async:,
    )) ->
      ast.FunctionExpression(
        name:,
        params:,
        body:,
        is_generator:,
        is_async:,
        span: decl_span,
      )
    DefaultClass(name:, super_class:, body:) ->
      ast.ClassExpression(name:, super_class:, body:, span: decl_span)
  }
}

/// The declaration's own binding name, `None` for the anonymous forms.
fn default_export_name(decl: DefaultExportDecl) -> Option(ast.NamedBinding) {
  case decl {
    DefaultFn(function:) -> function.name
    DefaultClass(name:, ..) -> name
  }
}

// ---- Public API ----

/// Build the initial parser state over an on-demand scanner. CPS so
/// callers write `use p <- init_parser(source, mode)`.
///
/// Nothing is lexed up front: the token window is filled lazily as the
/// parse advances, so a lexer error is reported when (and only if) the
/// parse reaches its position — see `ensure_current`.
fn init_parser(
  source: String,
  mode: ParseMode,
  cont: fn(P) -> Result(a, ParseError),
) -> Result(a, ParseError) {
  let bytes = bit_array.from_string(source)
  let lex_mode = case mode {
    Module -> lexer.LexModule
    Script -> lexer.LexScript
  }
  {
    cont(
      ensure_current(P(
        tokens: [],
        scan: lexer.scanner_at(bytes, 0, 1, lex_mode),
        mode: mode,
        prev_line: 1,
        prev_end: 0,
        bytes:,
        ctx: Ctx(
          strict: mode == Module,
          allow_in: True,
          function_depth: 0,
          loop_depth: 0,
          switch_depth: 0,
          label_set: [],
          in_generator: False,
          in_async: False,
          in_static_block: False,
          in_class_field_init: False,
          in_method: False,
          allow_new_target: False,
          allow_super_call: False,
          allow_super_property: False,
          binding_kind: BindingNone,
          in_block: False,
          module_top_level: False,
          in_single_stmt_pos: False,
          has_cover_initializer: False,
          dup_proto_pos: None,
          in_formal_params: False,
          in_arrow_params: False,
          has_non_simple_param: False,
          param_bound_names: [],
          pending_strict_name: None,
          in_export_decl: False,
          in_case_clause: False,
        ),
        class_private_depth: 0,
        private_refs: [],
        outer_private_names: [],
        last_expr_assignable: False,
        last_expr_is_assignment: False,
        has_invalid_pattern: False,
        export_names: set.new(),
        export_local_refs: [],
        import_bindings: set.new(),
        last_expr_name: None,
        sb: scope.sb_init(
          case mode {
            Module -> scope.Module
            Script -> scope.Script
          },
          mode == Module,
        ),
      )),
    )
  }
}

/// Parse JavaScript source code into an AST + parse-time scope tree.
///
/// Returns the `ast.Program` paired with the `scope.ScopeBuilder`
/// accumulated during the parse — the ScopeBuilder holds every scope,
/// binding and unresolved reference the parser encountered, ready for
/// `scope.finalize` to allocate slots and resolve captures. Callers that
/// only need the AST (tests, tooling) destructure and discard the
/// builder; the compile pipeline threads it to `scope.finalize`.
///
/// The compile pipeline uses `parse_script` / `parse_module` instead: those
/// hand back the BODY the matching `compiler.compile*` entry point consumes,
/// so no caller has to unwrap (or mis-match) a `Program` variant.
pub fn parse(
  source: String,
  mode: ParseMode,
) -> Result(#(ast.Program, scope.ScopeBuilder), ParseError) {
  case mode {
    Script -> {
      use #(body, sb) <- result.map(parse_script(source))
      #(ast.Script(body:), sb)
    }
    Module -> {
      use #(items, sb) <- result.map(parse_module(source))
      #(ast.Module(body: items), sb)
    }
  }
}

/// Parse source under the Script goal symbol, returning the statement list
/// (`compiler.compile` / `compile_repl` / `compile_eval` take exactly this)
/// paired with the parse-time scope builder.
pub fn parse_script(
  source: String,
) -> Result(#(List(ast.StmtWithLine), scope.ScopeBuilder), ParseError) {
  use p <- init_parser(source, Script)
  script_body(p)
}

/// Parse source under the Module goal symbol, returning the module-item list
/// (`compiler.compile_module` and `esm.analyze` take exactly this) paired
/// with the parse-time scope builder.
pub fn parse_module(
  source: String,
) -> Result(#(List(ast.ModuleItem), scope.ScopeBuilder), ParseError) {
  use p <- init_parser(source, Module)
  module_body(p)
}

/// Parse a direct-eval Script with `allow_new_target` inherited from the
/// caller's syntax permissions (§19.2.1.1 PerformEval step 9: when the eval
/// call is inside a function, `new.target` is not an early error). P11 will
/// extend this to the remaining `SyntaxPerms` flags; for now only
/// `new_target` is threaded so the P2 `function_depth>0` → `allow_new_target`
/// migration doesn't regress field-init eval tests that previously parsed by
/// accident.
pub fn parse_direct_eval(
  source: String,
  allow_new_target allow_new_target: Bool,
  allow_super_property allow_super_property: Bool,
  allow_super_call allow_super_call: Bool,
  allow_arguments allow_arguments: Bool,
  outer_private_names outer_private_names: List(String),
) -> Result(#(List(ast.StmtWithLine), scope.ScopeBuilder), ParseError) {
  use p <- init_parser(source, Script)
  // §19.2.1.1 PerformEval step 5: direct eval inside a method may legally
  // reference the caller's private names — `outer_private_names` is the
  // caller's PrivateEnvironment chain at the eval call site. Any other
  // unresolved private reference is the usual AllPrivateIdentifiersValid
  // SyntaxError.
  //
  // `allow_arguments: False` (field initializers, static blocks) re-uses the
  // in_class_field_init ContainsArguments rejection - the exact error
  // variant differs from the static-block one but both are SyntaxErrors.
  script_body(
    P(
      ..p,
      ctx: Ctx(
        ..p.ctx,
        allow_new_target:,
        allow_super_property:,
        allow_super_call:,
        in_class_field_init: !allow_arguments,
      ),
      outer_private_names:,
    ),
  )
}

// ---- Script / Module entry points ----

fn script_body(
  p: P,
) -> Result(#(List(ast.StmtWithLine), scope.ScopeBuilder), ParseError) {
  use p <- result.try(check_use_strict_at_start(p))
  use #(p_final, stmts) <- result.try(parse_statement_list(p, True, []))
  use Nil <- result.try(check_unresolved_private_refs(p_final))
  // Root scope close: reorder root's children to hoist order so
  // emit_program's child_fn_cursor (seeded from children_at[0]) pops
  // top-level FunctionDeclarations before fn-exprs/arrows/classes.
  let sb = scope.sb_reorder_block_children(p_final.sb, scope.root_scope_id)
  Ok(#(stmts, sb))
}

fn module_body(
  p: P,
) -> Result(#(List(ast.ModuleItem), scope.ScopeBuilder), ParseError) {
  use #(p_final, items) <- result.try(parse_module_body(p, []))
  use Nil <- result.try(validate_export_local_refs(p_final))
  use Nil <- result.try(check_unresolved_private_refs(p_final))
  // Root scope close: reorder root's children to hoist order.
  let sb = scope.sb_reorder_block_children(p_final.sb, scope.root_scope_id)
  Ok(#(items, sb))
}

fn parse_module_body(
  p: P,
  acc: List(ast.ModuleItem),
) -> Result(#(P, List(ast.ModuleItem)), ParseError) {
  let p = P(..p, ctx: Ctx(..p.ctx, module_top_level: True))
  case peek(p) {
    Eof -> Ok(#(p, list.reverse(acc)))
    Import ->
      // `import.meta` / `import(...)` at module top level are expression
      // statements (ImportMeta / ImportCall), not ImportDeclarations.
      case peek_at(p, 1) {
        Dot | LeftParen -> {
          let line = line_of(p)
          use #(p2, stmt) <- result.try(parse_statement(p))
          parse_module_body(p2, [
            ast.StatementItem(ast.StmtWithLine(line, stmt)),
            ..acc
          ])
        }
        _ -> {
          use #(p2, item) <- result.try(parse_import_declaration(p))
          parse_module_body(p2, [item, ..acc])
        }
      }
    Export -> {
      use #(p2, item) <- result.try(parse_export_declaration(p))
      parse_module_body(p2, [item, ..acc])
    }
    _ -> {
      let line = line_of(p)
      use #(p2, stmt) <- result.try(parse_statement(p))
      parse_module_body(p2, [
        ast.StatementItem(ast.StmtWithLine(line, stmt)),
        ..acc
      ])
    }
  }
}

/// Validate that every local name referenced in `export { name }` specifiers
/// is actually declared somewhere in the module scope.
fn validate_export_local_refs(p: P) -> Result(Nil, ParseError) {
  use #(name, pos) <- list.try_each(p.export_local_refs)
  let declared =
    scope.sb_root_has(p.sb, name) || set.contains(p.import_bindings, name)
  case declared {
    True -> Ok(Nil)
    False -> Error(UndeclaredExportBinding(pos, name))
  }
}

// ---- Statement parsing ----

fn parse_statement_list(
  p: P,
  top_level: Bool,
  acc: List(ast.StmtWithLine),
) -> Result(#(P, List(ast.StmtWithLine)), ParseError) {
  case peek(p) {
    Eof -> Ok(#(p, list.reverse(acc)))
    RightBrace -> {
      use <- bool.guard(top_level, Error(UnexpectedCloseBrace(pos_of(p))))
      Ok(#(p, list.reverse(acc)))
    }
    _ -> {
      let line = line_of(p)
      use #(p2, stmt) <- result.try(parse_statement(p))
      parse_statement_list(p2, top_level, [ast.StmtWithLine(line, stmt), ..acc])
    }
  }
}

fn parse_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  case peek(p) {
    Illegal | LexFailure(_) -> Error(illegal_token_error(p))
    LeftBrace -> parse_block_statement(p)
    Var | Const -> parse_variable_declaration(p)
    Let -> {
      // 'let' starts a declaration when followed by a binding pattern;
      // otherwise it's an identifier expression (sloppy mode compatibility)
      case
        peek_at(p, 1) == LeftBrace
        || peek_at(p, 1) == LeftBracket
        || is_identifier_or_keyword(peek_at(p, 1))
      {
        True -> parse_variable_declaration(p)
        False -> parse_expression_statement(p)
      }
    }
    If -> parse_if_statement(p)
    While -> parse_while_statement(p)
    Do -> parse_do_while_statement(p)
    For -> parse_for_statement(p)
    Return -> parse_return_statement(p)
    Break -> parse_break_statement(p)
    Continue -> parse_continue_statement(p)
    Throw -> parse_throw_statement(p)
    Try -> parse_try_statement(p)
    Switch -> parse_switch_statement(p)
    Function -> parse_function_declaration(p, True, False)
    Class -> parse_class_declaration(p)
    Semicolon -> Ok(#(advance(p), ast.EmptyStatement))
    Debugger -> {
      let p2 = advance(p)
      use p3 <- result.try(eat_semicolon(p2))
      Ok(#(p3, ast.DebuggerStatement))
    }
    With -> parse_with_statement(p)
    Async -> {
      case peek_at(p, 1) {
        Function -> parse_function_declaration(p, True, True)
        Colon -> parse_labeled_statement(p)
        _ -> parse_expression_statement(p)
      }
    }
    Yield | Await -> {
      case peek_at(p, 1) {
        Colon -> parse_labeled_statement(p)
        _ ->
          case peek(p) == Await && is_await_using_decl_start(p) {
            True -> parse_using_declaration(p, is_await: True)
            False -> parse_expression_statement(p)
          }
      }
    }
    Import -> {
      // import.meta or import() in script mode
      case peek_at(p, 1) {
        Dot | LeftParen -> parse_expression_statement(p)
        _ -> {
          case p.mode {
            // Top-level imports handled by parse_module_body
            Module -> Error(ImportNotTopLevel(pos_of(p)))
            Script -> parse_expression_statement(p)
          }
        }
      }
    }
    Export -> {
      case p.mode {
        // Top-level exports handled by parse_module_body
        Module -> Error(ExportNotTopLevel(pos_of(p)))
        Script -> Error(UnexpectedExport(pos_of(p)))
      }
    }
    // Labeled statement, using declaration, or expression statement
    Identifier -> {
      case peek_at(p, 1) {
        Colon -> parse_labeled_statement(p)
        _ ->
          case is_using_decl_start(p, 0) {
            True -> parse_using_declaration(p, is_await: False)
            False -> parse_expression_statement(p)
          }
      }
    }
    _ -> parse_expression_statement(p)
  }
}

/// UsingDeclaration lookahead at token offset `at`: the `using` contextual
/// keyword followed (same line — `using \n x` is the identifier `using` via
/// ASI) by a binding-identifier-capable token. `using [` / `using {` are NOT
/// declarations (destructuring is not allowed; `using[x]` is element access).
fn is_using_decl_start(p: P, at: Int) -> Bool {
  peek_at(p, at) == Identifier
  && peek_value_at(p, at) == "using"
  && token_line_at(p, at + 1) == token_line_at(p, at)
  && is_binding_ident_token(peek_at(p, at + 1))
}

/// AwaitUsingDeclaration lookahead: `await [no LT] using [no LT] binding`,
/// only where `await` is a keyword (async context or module top level —
/// otherwise `await` is an ordinary identifier and this is an expression).
fn is_await_using_decl_start(p: P) -> Bool {
  { p.ctx.in_async || p.mode == Module }
  && !p.ctx.in_static_block
  && token_line_at(p, 1) == token_line_at(p, 0)
  && is_using_decl_start(p, 1)
}

/// Tokens usable as a BindingIdentifier in a using declaration. `using let`
/// and `using await` ARE treated as declaration starts; binding `let` is
/// then rejected as in let/const, and `await` is rejected exactly where it
/// is reserved (modules, async bodies) by check_binding_identifier.
fn is_binding_ident_token(kind: TokenKind) -> Bool {
  kind == Identifier || is_contextual_keyword(kind)
}

/// Early errors for using/await-using declarations in statement position:
/// not in single-statement position (it's a Declaration), not directly in a
/// CaseClause/DefaultClause list, and (Script goal) not at the top level —
/// it must be contained within a Block / ForStatement / FunctionBody /
/// ClassStaticBlockBody etc. Module top level is allowed.
fn check_using_placement(p: P) -> Result(Nil, ParseError) {
  use <- bool.guard(
    p.ctx.in_single_stmt_pos,
    Error(LexicalDeclInSingleStatement(pos_of(p))),
  )
  use <- bool.guard(p.ctx.in_case_clause, Error(UsingInCaseClause(pos_of(p))))
  let script_top_level =
    p.mode == Script
    && p.ctx.function_depth == 0
    && !p.ctx.in_block
    && !p.ctx.in_static_block
  use <- bool.guard(script_top_level, Error(UsingAtScriptTopLevel(pos_of(p))))
  Ok(Nil)
}

/// Parse `using x = expr [, y = expr]* ;` (or the `await using` form, with
/// the leading `await` already verified by is_await_using_decl_start).
fn parse_using_declaration(
  p: P,
  is_await is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  use Nil <- result.try(check_using_placement(p))
  // Consume `await`? `using`.
  let p2 = case is_await {
    True -> advance(advance(p))
    False -> advance(p)
  }
  let p2 = enter_lexical_decl_context(p2, scope.ConstBinding)
  use #(p3, declarations) <- result.try(parse_using_declarator_list(p2, []))
  use p4 <- result.try(eat_semicolon(
    P(
      ..p3,
      ctx: Ctx(
        ..p3.ctx,
        binding_kind: p.ctx.binding_kind,
        in_export_decl: False,
      ),
    ),
  ))
  let kind = case is_await {
    True -> ast.AwaitUsing
    False -> ast.Using
  }
  Ok(#(p4, ast.VariableDeclaration(kind:, declarations:)))
}

fn parse_using_declarator_list(
  p: P,
  acc: List(ast.VariableDeclarator),
) -> Result(#(P, List(ast.VariableDeclarator)), ParseError) {
  use #(p2, pattern) <- result.try(parse_using_binding(p))
  case peek(p2) {
    Equal -> {
      use #(p3, init_expr) <- result.try(
        parse_assignment_expression(advance(p2)),
      )
      let decl = ast.VariableDeclarator(id: pattern, init: Some(init_expr))
      case peek(p3) {
        Comma -> parse_using_declarator_list(advance(p3), [decl, ..acc])
        _ -> Ok(#(p3, list.reverse([decl, ..acc])))
      }
    }
    _ -> Error(UsingMissingInitializer(pos_of(p2)))
  }
}

/// A using declaration binds identifiers only — never patterns.
fn parse_using_binding(p: P) -> Result(#(P, ast.Pattern), ParseError) {
  case peek(p) {
    LeftBracket | LeftBrace -> Error(UsingPatternBinding(pos_of(p)))
    kind ->
      case kind == Identifier || is_contextual_keyword(kind) {
        True -> validate_and_register_binding(p, peek_value(p))
        False -> Error(error_at_current(p, ExpectedBindingPattern(pos_of(p))))
      }
  }
}

/// Parse a statement in a single-statement context (if/else/while/do-while/for/with body).
/// Lexical declarations (let/const) are not allowed in these positions.
/// `allow_fn` controls whether bare function declarations are allowed (Annex B for `if`).
fn parse_single_statement(
  p: P,
  allow_fn: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  // in_single_stmt_pos must not leak to statements parsed AFTER this one —
  // restore the surrounding value on the way out.
  let saved = p.ctx.in_single_stmt_pos
  use #(p_out, stmt) <- result.map(parse_single_statement_inner(p, allow_fn))
  #(P(..p_out, ctx: Ctx(..p_out.ctx, in_single_stmt_pos: saved)), stmt)
}

fn parse_single_statement_inner(
  p: P,
  allow_fn: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  let p =
    P(..p, ctx: Ctx(..p.ctx, in_single_stmt_pos: True, module_top_level: False))
  case peek(p) {
    Const -> Error(LexicalDeclInSingleStatement(pos_of(p)))
    Let -> {
      let next = peek_at(p, 1)
      let decl_starter =
        next == LeftBrace
        || next == LeftBracket
        || is_identifier_or_keyword(next)
      // §13.4 ExpressionStatement lookahead excludes only `let [` outright.
      // `let {` / `let ident` with a LineTerminator after `let` parse as the
      // identifier expression `let` + ASI (LexicalDeclaration is not in the
      // grammar in single-statement position).
      let newline_after_let = token_line_at(p, 1) > token_line_at(p, 0)
      case next == LeftBracket || { decl_starter && !newline_after_let } {
        True -> Error(LexicalDeclInSingleStatement(pos_of(p)))
        False ->
          case decl_starter {
            True -> parse_expression_statement(p)
            False -> parse_statement(p)
          }
      }
    }
    Function ->
      case allow_fn && !p.ctx.strict {
        // Annex B §B.3.3: sloppy `if (c) function f(){}` parses AS IF
        // `if (c) { function f(){} }` — push a synthetic Block scope so
        // register_function_name's in_block path fires (LetBinding for
        // the name + annexb_candidate for the var-twin) instead of the
        // in_single_stmt_pos early-return at register_function_name:1321.
        // emit_if's `block_wrap_fn_decl` wraps the bare FunctionDeclaration
        // in a BlockStatement on the emit side, and emit_block then consumes
        // exactly this scope (block_has_declarations → True for
        // FunctionDeclaration, so the elision guard does not skip it).
        True -> {
          let #(sb, block_id) = scope.sb_push(p.sb, scope.Block)
          let p_inner =
            P(
              ..p,
              sb:,
              ctx: Ctx(..p.ctx, in_block: True, in_single_stmt_pos: False),
            )
          use #(p2, stmt) <- result.map(parse_statement(p_inner))
          let sb =
            scope.sb_close_block(p2.sb, block_id)
            |> scope.sb_enter(p.sb.current)
          #(P(..p2, sb:, ctx: Ctx(..p2.ctx, in_block: p.ctx.in_block)), stmt)
        }
        // In strict mode or in iteration/with context, function decls are forbidden
        False -> Error(FunctionDeclInSingleStatement(pos_of(p)))
      }
    // Async function declarations are never legal in single-statement
    // position — Annex B §B.3.3 extends plain functions only.
    Async ->
      case peek_at(p, 1) {
        Function -> Error(FunctionDeclInSingleStatement(pos_of(p)))
        _ -> parse_statement(p)
      }
    Class -> Error(LexicalDeclInSingleStatement(pos_of(p)))
    _ -> parse_statement(p)
  }
}

/// Enter a new Block scope: push a `scope.Block` onto the builder so
/// declarations land in a fresh scope. Used directly by the for-head
/// sub-scopes; block/switch/catch inline the equivalent so their per-caller
/// extras fold into one P construction.
fn enter_block_scope(p: P) -> P {
  let #(sb, _id) = scope.sb_push(p.sb, scope.Block)
  P(..p, sb:, ctx: Ctx(..p.ctx, in_block: True))
}

/// Restore the enclosing block-scope tracking state after parsing an inner
/// block. `before` is the parser state captured immediately before the
/// matching enter_block_scope. Keeps the inner block's accumulated
/// scopes/refs (`p.sb`'s dicts flow forward); restores ONLY the
/// `current`/`current_fn` cursors from `saved.sb`.
fn restore_block_scope(after p: P, before saved: P) -> P {
  // Flip children_at[for-head] from prepend (reverse-source) order to
  // source order before the pop. The for-head Block scope can never
  // host a FunctionDeclaration (init/cond/update are expressions; body
  // is a single nested Block child), so the partition step in
  // `sb_reorder_block_children` is a no-op and this is just the
  // reverse. Required by finalize's `children_at` contract — every
  // scope's child list must arrive in final (source = emit-cursor)
  // order; the legacy `declare_for_classic` walks init→cond→update→body.
  let sb = scope.sb_reorder_block_children(p.sb, p.sb.current)
  P(
    ..p,
    sb: scope.sb_enter(sb, saved.sb.current),
    ctx: Ctx(..p.ctx, in_block: saved.ctx.in_block),
  )
}

fn parse_block_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  use #(p2, stmts) <- result.map(parse_block_body(p))
  #(p2, ast.BlockStatement(body: stmts))
}

/// The `{ StatementList }` of a Block, as its raw statement list. Every
/// production whose body is a Block but whose AST node holds the statements
/// directly (function / arrow / try / catch / method bodies) parses through
/// this rather than `parse_block_statement`, so no consumer has to
/// re-destructure a `BlockStatement` it already knows is there.
fn parse_block_body(p: P) -> Result(#(P, List(ast.StmtWithLine)), ParseError) {
  // `{}` fast path: an empty block declares nothing, so the block-scope
  // context construction + restore below would be a no-op — skip it. Hot on
  // `{}`-dense code (staging/sm/regress/regress-610026.js evals 2^21+ empty
  // blocks). Mirrors the slow path's net effect on P exactly: position
  // advances past both braces; in_single_stmt_pos/in_case_clause end up
  // False (the slow path's restore deliberately leaves them as the inner
  // context set them).
  case peek(p), peek_at(p, 1) {
    LeftBrace, RightBrace -> {
      let p2 = advance(advance(p))
      Ok(
        #(
          P(
            ..p2,
            ctx: Ctx(..p2.ctx, in_single_stmt_pos: False, in_case_clause: False),
          ),
          [],
        ),
      )
    }
    _, _ -> parse_block_body_slow(p)
  }
}

fn parse_block_body_slow(
  p: P,
) -> Result(#(P, List(ast.StmtWithLine)), ParseError) {
  use p2 <- result.try(expect(p, LeftBrace))
  // Enter block scope (enter_block_scope inlined so the per-caller extras
  // fold into one P construction). Param-name conflicts apply only to the
  // function body's (or catch block's) DIRECT statement list — a nested
  // block may freely shadow a parameter (`function f(a) { { let a; } }`
  // is legal).
  let #(sb, block_id) = scope.sb_push(p2.sb, scope.Block)
  let p_inner =
    P(
      ..p2,
      sb:,
      ctx: Ctx(
        ..p2.ctx,
        in_block: True,
        in_single_stmt_pos: False,
        module_top_level: False,
        in_case_clause: False,
      ),
    )
  use #(p3, stmts) <- result.try(parse_statement_list(p_inner, False, []))
  use p4 <- result.try(expect(p3, RightBrace))
  // Close the block scope (Option B): EITHER prune it (no declarations —
  // emit_block applies the same elision so the tree must drop the node)
  // OR reorder its children_at to hoist order so emit's positional
  // child_fn_cursor stays in lockstep. Then pop.
  let sb =
    scope.sb_close_block(p4.sb, block_id)
    |> scope.sb_enter(p2.sb.current)
  Ok(#(
    // restore_block_scope inlined: keep p4.sb's accumulated scopes/refs;
    // restore only the current/current_fn cursors to the outer scope.
    P(
      ..p4,
      sb:,
      ctx: Ctx(
        ..p4.ctx,
        in_block: p2.ctx.in_block,
        module_top_level: p2.ctx.module_top_level,
      ),
    ),
    stmts,
  ))
}

fn parse_variable_declaration(p: P) -> Result(#(P, ast.Statement), ParseError) {
  use #(p2, decl) <- result.map(parse_variable_declaration_decl(p))
  #(p2, ast.declaration_to_statement(decl))
}

/// A `var`/`let`/`const` declaration returned as an `ast.Declaration` — the
/// shape `export <decl>` needs. Statement position wraps it back up above.
fn parse_variable_declaration_decl(
  p: P,
) -> Result(#(P, ast.Declaration), ParseError) {
  // var/let/const — convert the head token to its ast.VariableKind ONCE here;
  // everything downstream is typed on VariableKind. Callers only reach here
  // with peek ∈ {Var, Let, Const}; assert it so a wrong head token panics
  // instead of silently becoming `var`.
  let kind = case peek(p) {
    Let -> ast.Let
    Const -> ast.Const
    Var -> ast.Var
    _ ->
      panic as "parser: parse_variable_declaration_decl entered with non-var/let/const head token"
  }
  let p2 = advance(p)
  let p2 = case kind {
    ast.Let -> enter_lexical_decl_context(p2, scope.LetBinding)
    ast.Const | ast.Using | ast.AwaitUsing ->
      enter_lexical_decl_context(p2, scope.ConstBinding)
    ast.Var -> P(..p2, ctx: Ctx(..p2.ctx, binding_kind: BindingVar))
  }
  use #(p3, declarations) <- result.try(
    parse_variable_declarator_list(p2, kind, []),
  )
  use p4 <- result.try(eat_semicolon(
    P(
      ..p3,
      ctx: Ctx(
        ..p3.ctx,
        binding_kind: p.ctx.binding_kind,
        in_export_decl: False,
      ),
    ),
  ))
  Ok(#(p4, ast.DeclVariable(kind:, declarations:)))
}

fn parse_variable_declarator_list(
  p: P,
  kind: ast.VariableKind,
  acc: List(ast.VariableDeclarator),
) -> Result(#(P, List(ast.VariableDeclarator)), ParseError) {
  use #(p2, decl) <- result.try(parse_variable_declarator(p, kind))
  case peek(p2) {
    Comma -> parse_variable_declarator_list(advance(p2), kind, [decl, ..acc])
    _ -> Ok(#(p2, list.reverse([decl, ..acc])))
  }
}

fn parse_variable_declarator(
  p: P,
  kind: ast.VariableKind,
) -> Result(#(P, ast.VariableDeclarator), ParseError) {
  let is_destructuring = case peek(p) {
    LeftBracket | LeftBrace -> True
    _ -> False
  }
  use #(p2, pattern) <- result.try(parse_binding_pattern(p))
  case peek(p2) {
    Equal -> {
      // The initializer is an ordinary expression: any bindings it introduces
      // (arrow/function params, function-expression names) live inside a
      // function boundary, whose fresh Ctx clears `in_export_decl` — so they
      // are never mistaken for exported names. `restore_outer_context` puts
      // the flag back for any later declarators in the same declaration.
      let init_start = pos_of(p2)
      use #(p3, init_expr) <- result.try(
        parse_assignment_expression(advance(p2)),
      )
      // The initializer is an ordinary expression (the declarator's TARGET is
      // a binding pattern, which never sets these flags), so a cover grammar
      // it left unconsumed is due now: `var x = {a = 1};` is a SyntaxError.
      use Nil <- result.try(check_cover_grammar_errors(p3, init_start))
      let p3 = P(..p3, sb: sb_mark_pattern_assigned(p3.sb, pattern))
      Ok(#(p3, ast.VariableDeclarator(id: pattern, init: Some(init_expr))))
    }
    _ -> {
      use <- bool.guard(
        kind == ast.Const,
        Error(MissingConstInitializer(pos_of(p2))),
      )
      use <- bool.guard(
        is_destructuring,
        Error(DestructuringMissingInitializer(pos_of(p2))),
      )
      Ok(#(p2, ast.VariableDeclarator(id: pattern, init: None)))
    }
  }
}

fn parse_binding_pattern(p: P) -> Result(#(P, ast.Pattern), ParseError) {
  case peek(p) {
    Identifier -> validate_and_register_binding(p, peek_value(p))
    LeftBracket -> parse_array_binding_pattern(p)
    LeftBrace -> parse_object_binding_pattern(p)
    _ ->
      case is_contextual_keyword(peek(p)) {
        True -> validate_and_register_binding(p, peek_value(p))
        False -> Error(error_at_current(p, ExpectedBindingPattern(pos_of(p))))
      }
  }
}

/// Validate a binding identifier and register it in all relevant scopes.
fn validate_and_register_binding(
  p: P,
  val: String,
) -> Result(#(P, ast.Pattern), ParseError) {
  use Nil <- result.try(check_binding_identifier(p, val))
  use p <- result.try(check_duplicate_binding(p, val))
  use p <- result.try(accumulate_param_name(p, val))
  use p <- result.try(register_scope_binding(p, val))
  use p <- result.try(check_export_binding(p, val))
  Ok(#(advance(p), ast.IdentifierPattern(name: val, span: span_of(p))))
}

/// Like validate_and_register_binding but takes separate parser states for
/// identifier validation (check_p) vs scope registration (scope_p), and
/// does not advance. Used for object binding property shorthands.
fn validate_and_register_binding_no_advance(
  check_p: P,
  scope_p: P,
  val: String,
) -> Result(#(P, ast.Pattern), ParseError) {
  use Nil <- result.try(check_binding_identifier(check_p, val))
  use p <- result.try(check_duplicate_binding(scope_p, val))
  use p <- result.try(accumulate_param_name(p, val))
  use p <- result.try(register_scope_binding(p, val))
  use p <- result.try(check_export_binding(p, val))
  Ok(#(p, ast.IdentifierPattern(name: val, span: span_of(check_p))))
}

/// Accumulate a binding name in param_bound_names when inside formal params or
/// arrow params. Checks for duplicate param names across all params including
/// those inside destructured patterns.
///
/// NOT gated on `in_method`: that flag stays true through the whole method
/// *body* (it is only restored on method exit, see `restore_outer_context`), so
/// gating accumulation on it here would wrongly treat ordinary body bindings —
/// e.g. two sibling `for (const id of …)` loops — as duplicate *parameters*.
/// Method params are already covered by `in_formal_params`; `in_method` still
/// tightens the strict-mode dup check below.
fn accumulate_param_name(p: P, name: String) -> Result(P, ParseError) {
  let bind = fn() {
    Ok(
      P(
        ..p,
        ctx: Ctx(..p.ctx, param_bound_names: [name, ..p.ctx.param_bound_names]),
      ),
    )
  }
  case p.ctx.in_formal_params || p.ctx.in_arrow_params {
    True ->
      case list.contains(p.ctx.param_bound_names, name) {
        True ->
          // In strict mode, arrow params, methods, or non-simple params: reject dups
          case
            p.ctx.strict
            || p.ctx.in_arrow_params
            || p.ctx.in_method
            || p.ctx.has_non_simple_param
          {
            True -> Error(DuplicateParameterName(pos_of(p), name))
            // Sloppy non-arrow non-method: dups allowed for simple params.
            // We still accumulate for retroactive strict check.
            False -> bind()
          }
        False -> bind()
      }
    False -> Ok(p)
  }
}

/// Validate an identifier used as a binding name (let/const/var/param/catch)
/// An `Identifier` token whose name spells an always-reserved word can only
/// have arisen from a \u escape (the lexer maps unescaped reserved words to
/// keyword tokens). Such an escaped keyword is never a valid binding or
/// reference. ES2024 §13.1.1 makes it an early SyntaxError. yield/await are
/// excluded here — they are contextual and validated elsewhere.
fn check_not_escaped_reserved_word(
  p: P,
  name: String,
) -> Result(Nil, ParseError) {
  case is_reserved_word_kind(lexer.keyword_or_identifier(name)) {
    True -> Error(EscapedReservedWord(pos_of(p), name))
    False -> Ok(Nil)
  }
}

/// Reserved-name checks shared by IdentifierReference and BindingIdentifier
/// (§13.1.1): escaped always-reserved words, `enum`, strict-mode future
/// reserved words, and the contextual yield/await guards. Callers layer
/// their context-specific checks on the Ok result.
fn check_reserved_identifier_common(
  p: P,
  name: String,
) -> Result(Nil, ParseError) {
  use Nil <- result.try(check_not_escaped_reserved_word(p, name))
  case name {
    "enum" -> Error(EnumReservedWord(pos_of(p)))
    "implements"
    | "interface"
    | "package"
    | "private"
    | "protected"
    | "public"
    | "static" ->
      case p.ctx.strict {
        True -> Error(ReservedWordStrictMode(pos_of(p), name))
        False -> Ok(Nil)
      }
    "yield" -> {
      use <- bool.guard(p.ctx.strict, Error(YieldReservedStrictMode(pos_of(p))))
      use <- bool.guard(p.ctx.in_generator, Error(YieldInGenerator(pos_of(p))))
      Ok(Nil)
    }
    "await" -> {
      use <- bool.guard(p.mode == Module, Error(AwaitInModule(pos_of(p))))
      use <- bool.guard(p.ctx.in_async, Error(AwaitInAsyncFunction(pos_of(p))))
      Ok(Nil)
    }
    _ -> Ok(Nil)
  }
}

/// Validate a name used as an IdentifierReference (§13.1.1). Used for primary
/// expressions and object literal shorthand properties.
fn check_identifier_reference(p: P, name: String) -> Result(Nil, ParseError) {
  use Nil <- result.try(check_reserved_identifier_common(p, name))
  case name {
    "let" ->
      case p.ctx.strict {
        True -> Error(ReservedWordStrictMode(pos_of(p), name))
        False -> Ok(Nil)
      }
    "await" ->
      case p.ctx.in_static_block {
        True -> Error(AwaitInStaticBlock(pos_of(p)))
        False -> Ok(Nil)
      }
    "arguments" -> {
      // §15.7.1 ClassStaticBlockBody / §15.7.10 FieldDefinition Initializer:
      // ContainsArguments must be false.
      use <- bool.guard(
        p.ctx.in_static_block,
        Error(ArgumentsInStaticBlock(pos_of(p))),
      )
      use <- bool.guard(
        p.ctx.in_class_field_init,
        Error(ArgumentsInClassFieldInit(pos_of(p))),
      )
      Ok(Nil)
    }
    _ -> Ok(Nil)
  }
}

fn check_binding_identifier(p: P, name: String) -> Result(Nil, ParseError) {
  use Nil <- result.try(check_reserved_identifier_common(p, name))
  case name {
    "eval" | "arguments" ->
      case p.ctx.strict {
        True -> Error(StrictModeBindingName(pos_of(p), name))
        False -> Ok(Nil)
      }
    "let" ->
      case p.ctx.strict || in_lexical_decl(p.ctx) {
        True -> Error(LetBindingInLexicalDecl(pos_of(p)))
        False -> Ok(Nil)
      }
    _ -> Ok(Nil)
  }
}

/// Check if a binding name is a duplicate within the current let/const
/// declaration (`let a, a;` — §14.3.1). Returns Ok(p) with the name added to
/// the declaration's `bound` set. Only a `BindingLexical` context has such a
/// set to check against, so var/param/no-declaration contexts are a no-op by
/// construction rather than by a separate flag.
fn check_duplicate_binding(p: P, name: String) -> Result(P, ParseError) {
  case p.ctx.binding_kind {
    BindingLexical(kind:, bound:) ->
      case set.contains(bound, name) {
        True -> Error(DuplicateBindingLexical(pos_of(p), name))
        False ->
          Ok(
            P(
              ..p,
              ctx: Ctx(
                ..p.ctx,
                binding_kind: BindingLexical(
                  kind:,
                  bound: set.insert(bound, name),
                ),
              ),
            ),
          )
      }
    BindingNone | BindingVar | BindingParam -> Ok(p)
  }
}

/// Register a name as a lexical (let/const/class-like) binding in the
/// current block scope. §14.2.1: conflicts with anything already bound
/// in this scope (let/const/class/param/catch) OR any `var` whose hoist
/// path passed through this scope (`hoisted_vars`) — covers
/// `{ var x; let x }` AND `{ {var x} let x }` where the var binding
/// itself lives in the function scope, not here.
///
/// Exemption: the implicit `arguments` placeholder (a synthetic
/// VarBinding — recorded BEFORE the body so its slot index
/// matches legacy `declare_var_boundary_body`) does not block a
/// sloppy-mode `let arguments` at function top level. Legacy
/// `add_binding` first-wins lets the implicit VarBinding survive and
/// the later let is a silent no-op; `sb_declare` below reproduces the
/// no-op, `sb_only_implicit_arguments` reproduces the no-error.
fn register_lexical_name(
  p: P,
  name: String,
  kind: scope.BindingKind,
  pos: Int,
) -> Result(P, ParseError) {
  use <- bool.guard(
    scope.sb_lexical_conflict(p.sb, name)
      && !scope.sb_only_implicit_arguments(p.sb, name),
    Error(IdentifierAlreadyDeclared(pos, name)),
  )
  Ok(P(..p, sb: scope.sb_declare(p.sb, name, kind, synthetic: False)))
}

/// Register a binding name in the current block scope.
/// Checks for conflicts based on binding_kind:
/// - BindingLexical: conflicts with anything in current scope
/// - BindingParam: adds to current scope as ParamBinding
/// - BindingVar: conflicts with let/const in any enclosing block up to
///   the function boundary (var can re-declare var, params, and
///   sloppy-mode function decls)
/// - BindingNone: no scope registration
fn register_scope_binding(p: P, name: String) -> Result(P, ParseError) {
  case p.ctx.binding_kind {
    BindingLexical(kind:, ..) -> register_lexical_name(p, name, kind, pos_of(p))
    BindingParam ->
      Ok(
        P(
          ..p,
          sb: scope.sb_declare(p.sb, name, scope.ParamBinding, synthetic: False),
        ),
      )
    BindingVar -> {
      // §14.3.2: a `var` conflicts with a let/const/class in any enclosing
      // block scope up to (and including) the function boundary.
      // §16.2.1.1: at Module root it ALSO conflicts with a top-level
      // function declaration (LexicallyDeclaredName, but recorded as
      // VarBinding so `sb_var_conflicts_lexical` alone misses it).
      use <- bool.guard(
        scope.sb_var_conflicts_lexical(p.sb, name)
          || scope.sb_var_conflicts_module_fn(p.sb, name),
        Error(IdentifierAlreadyDeclared(pos_of(p), name)),
      )
      // sb_declare_var: real VarBinding lands in current_fn AND each
      // intermediate block records `name` in its hoisted_vars so a
      // later let/const there is rejected (§14.2.1).
      Ok(P(..p, sb: scope.sb_declare_var(p.sb, name, synthetic: False)))
    }
    BindingNone -> Ok(p)
  }
}

/// Register a function declaration name in the current scope.
/// Inside a block or at module top level, function declarations are lexical.
/// At function/script top-level they are var-like (can duplicate) — even in
/// strict mode.
///
/// `is_plain` is False for generator / async / async-generator declarations:
/// Annex B §B.3.2's web-compat block-function var promotion applies ONLY to
/// plain FunctionDeclarations, so the others stay strictly block-scoped
/// even in sloppy mode (switch/scope-lex-{async-,}generator.js).
fn register_function_name(
  p: P,
  name: String,
  name_pos: Int,
  is_plain: Bool,
) -> Result(P, ParseError) {
  // Annex B §B.3.1: in sloppy mode `if (c) function f() {}` behaves as if
  // the declaration were wrapped in its own Block — the name is lexically
  // scoped to that synthetic block, so it can never clash with surrounding
  // declarations (`let f; if (c) function f() {}` is legal).
  use <- bool.guard(!p.ctx.strict && p.ctx.in_single_stmt_pos, Ok(p))
  // §16.1.1: a script's LexicallyDeclaredNames EXCLUDE top-level function
  // declarations (they are VarDeclaredNames), so `var f; function f() {}` is
  // legal at script/function top level even in strict mode. Module top level
  // is different: §16.2.1.1 module LexicallyDeclaredNames INCLUDE them — so
  // the early-error CHECK is lexical there, but the recorded BindingKind is
  // still VarBinding (matching `declare_var_boundary_body`'s
  // `direct_fn_names → VarBinding` for Module roots, which have
  // `vars_are_local = True`): emit hoists the closure with a plain store,
  // no TDZ slot.
  case p.ctx.in_block, p.ctx.module_top_level {
    // Module top-level: lexical conflict check, var-like binding kind.
    // Recorded via plain `sb_declare` (NOT `sb_declare_var`) so the name
    // lands in root.bindings WITHOUT root.hoisted_vars — that absence is
    // the discriminator `sb_var_conflicts_module_fn` uses to reject a
    // later `var f` after `function f(){}` (§16.2.1.1).
    False, True -> {
      use <- bool.guard(
        scope.sb_lexical_conflict(p.sb, name)
          && !scope.sb_only_implicit_arguments(p.sb, name),
        Error(IdentifierAlreadyDeclared(name_pos, name)),
      )
      Ok(
        P(
          ..p,
          sb: scope.sb_declare(p.sb, name, scope.VarBinding, synthetic: False),
        ),
      )
    }
    // Block scope: function decls are lexical (§14.2.2). Annex B §B.3.2:
    // in sloppy mode a block-level function ALSO becomes a candidate for
    // var-hoisting into the enclosing function scope (decided by
    // `finalize` once it knows whether a same-named lexical blocks the
    // promotion).
    True, _ -> {
      use p2 <- result.map(register_lexical_name(
        p,
        name,
        scope.LetBinding,
        name_pos,
      ))
      case p.ctx.in_block && !p.ctx.strict && is_plain {
        False -> p2
        True -> P(..p2, sb: scope.sb_annexb_candidate(p2.sb, name))
      }
    }
    False, False -> {
      // Script/function top-level: var-like, only conflicts with let/const
      use <- bool.guard(
        scope.sb_current_has_kind(p.sb, name, scope.LetBinding)
          || scope.sb_current_has_kind(p.sb, name, scope.ConstBinding),
        Error(IdentifierAlreadyDeclared(name_pos, name)),
      )
      Ok(P(..p, sb: scope.sb_declare_var(p.sb, name, synthetic: False)))
    }
  }
}

/// Check if an export name is a duplicate. Only applies in Module mode.
/// Returns Ok(p) with the name added to export_names, or Error if duplicate.
fn check_duplicate_export(p: P, name: String) -> Result(P, ParseError) {
  case p.mode {
    Module ->
      case set.contains(p.export_names, name) {
        True -> Error(DuplicateExport(pos_of(p), name))
        False -> Ok(P(..p, export_names: set.insert(p.export_names, name)))
      }
    Script -> Ok(p)
  }
}

/// Check if an import binding name is a duplicate. Only applies in Module mode.
/// Returns Ok(p) with the name added to import_bindings AND sb_declared
/// as a `ConstBinding` in the module (root) scope (§16.2.1.5.5: imported
/// bindings are immutable). Errors on duplicate.
fn check_duplicate_import_binding(p: P, name: String) -> Result(P, ParseError) {
  case p.mode {
    Module ->
      case set.contains(p.import_bindings, name) {
        True -> Error(DuplicateImportBinding(pos_of(p), name))
        False -> {
          let p =
            P(
              ..p,
              sb: scope.sb_declare(
                p.sb,
                name,
                scope.ConstBinding,
                synthetic: False,
              ),
            )
          Ok(P(..p, import_bindings: set.insert(p.import_bindings, name)))
        }
      }
    Script -> Ok(p)
  }
}

/// Check if a token kind is a reserved word that can NEVER be a binding identifier.
/// Validate that the local binding name in an import specifier is not a
/// reserved word. The imported name (LHS of 'as') can be any IdentifierName
/// including keywords, but the local binding must be a valid BindingIdentifier.
fn check_import_binding_name(
  p: P,
  binding_name: String,
  binding_token_kind: TokenKind,
) -> Result(Nil, ParseError) {
  case is_reserved_word_kind(binding_token_kind) {
    True -> Error(ReservedWordImportBinding(pos_of(p), binding_name))
    False -> check_binding_identifier(p, binding_name)
  }
}

/// When inside an export var/let/const declaration (in_export_decl is True),
/// register the binding name as an exported name and check for duplicates.
fn check_export_binding(p: P, name: String) -> Result(P, ParseError) {
  case p.ctx.in_export_decl {
    True -> check_duplicate_export(p, name)
    False -> Ok(p)
  }
}

/// If the current token is `=`, parse a default initializer and wrap `pat`
/// in an AssignmentPattern; otherwise return `pat` unchanged. The `=` commits
/// to an initializer (there is no alternative production), so a parse error
/// in it is propagated rather than backtracked over — `let [x = ] = y` must
/// report the bad initializer, not pretend there was no default.
fn parse_pattern_default(
  p: P,
  pat: ast.Pattern,
) -> Result(#(P, ast.Pattern), ParseError) {
  case peek(p) {
    Equal -> {
      use #(p2, default_expr) <- result.map(
        parse_assignment_expression(advance(p)),
      )
      #(p2, ast.AssignmentPattern(left: pat, right: default_expr))
    }
    _ -> Ok(#(p, pat))
  }
}

fn parse_array_binding_pattern(p: P) -> Result(#(P, ast.Pattern), ParseError) {
  use p2 <- result.try(expect(p, LeftBracket))
  parse_array_binding_elements(p2, [])
}

fn parse_array_binding_elements(
  p: P,
  acc: List(Option(ast.Pattern)),
) -> Result(#(P, ast.Pattern), ParseError) {
  case peek(p) {
    RightBracket ->
      Ok(#(advance(p), ast.ArrayPattern(elements: list.reverse(acc))))
    Comma ->
      // Elision (hole) in array pattern
      parse_array_binding_elements(advance(p), [None, ..acc])
    DotDotDot -> {
      let p2 = advance(p)
      use #(p3, inner_pat) <- result.try(parse_binding_pattern(p2))
      // Rest element must be last — no trailing comma allowed
      case peek(p3) {
        Comma -> Error(RestTrailingComma(pos_of(p3)))
        _ -> {
          use p4 <- result.try(expect(p3, RightBracket))
          let rest = ast.RestElement(argument: inner_pat)
          Ok(#(
            p4,
            ast.ArrayPattern(elements: list.reverse([Some(rest), ..acc])),
          ))
        }
      }
    }
    _ -> {
      use #(p2, pat) <- result.try(parse_binding_pattern(p))
      use #(p3, final_pat) <- result.try(parse_pattern_default(p2, pat))
      case peek(p3) {
        Comma ->
          parse_array_binding_elements(advance(p3), [Some(final_pat), ..acc])
        RightBracket ->
          Ok(#(
            advance(p3),
            ast.ArrayPattern(elements: list.reverse([Some(final_pat), ..acc])),
          ))
        _ -> Error(ExpectedCommaOrBracket(pos_of(p3)))
      }
    }
  }
}

fn parse_object_binding_pattern(p: P) -> Result(#(P, ast.Pattern), ParseError) {
  use p2 <- result.try(expect(p, LeftBrace))
  parse_object_binding_properties(p2, [])
}

fn parse_object_binding_properties(
  p: P,
  acc: List(ast.PatternProperty),
) -> Result(#(P, ast.Pattern), ParseError) {
  case peek(p) {
    RightBrace ->
      Ok(#(advance(p), ast.ObjectPattern(properties: list.reverse(acc))))
    DotDotDot -> {
      let p2 = advance(p)
      let kind = peek(p2)
      // §13.3.3: BindingRestProperty is `... BindingIdentifier` — no nested
      // pattern (unlike array-rest BindingRestElement, which allows one).
      use Nil <- result.try(case kind {
        LeftBrace | LeftBracket -> Error(InvalidRestBinding(pos_of(p2)))
        Identifier -> Ok(Nil)
        _ ->
          case is_contextual_keyword(kind) {
            True -> Ok(Nil)
            False -> Error(ExpectedIdentifier(pos_of(p2)))
          }
      })
      let name = peek_value(p2)
      let span = span_of(p2)
      use #(p3, _ident_pat) <- result.try(validate_and_register_binding(
        p2,
        name,
      ))
      let rest = ast.RestProperty(name:, span:)
      // Rest must be last — trailing comma is a SyntaxError.
      case peek(p3) {
        Comma -> Error(RestTrailingComma(pos_of(p3)))
        _ -> {
          use p4 <- result.map(expect(p3, RightBrace))
          #(p4, ast.ObjectPattern(properties: list.reverse([rest, ..acc])))
        }
      }
    }
    _ -> {
      use #(p2, prop) <- result.try(parse_object_binding_property(p))
      case peek(p2) {
        Comma -> parse_object_binding_properties(advance(p2), [prop, ..acc])
        RightBrace ->
          Ok(#(
            advance(p2),
            ast.ObjectPattern(properties: list.reverse([prop, ..acc])),
          ))
        _ -> Error(ExpectedCommaOrObjectClose(pos_of(p2)))
      }
    }
  }
}

fn parse_object_binding_property(
  p: P,
) -> Result(#(P, ast.PatternProperty), ParseError) {
  // Could be: ident, ident = default, ident: pattern, [computed]: pattern
  // Check if current token is valid as shorthand binding (identifier or contextual keyword)
  let prop_kind = peek(p)
  let prop_name = peek_value(p)
  let is_valid_shorthand = case prop_kind {
    Identifier -> True
    _ -> is_contextual_keyword(prop_kind)
  }
  use #(p2, key) <- result.try(parse_property_name(p))
  use Nil <- result.try(reject_private_property_key(p, key))
  case peek(p2) {
    Colon -> {
      // property: pattern
      use #(p4, val_pat) <- result.try(parse_binding_pattern(advance(p2)))
      use #(p5, final_pat) <- result.map(parse_pattern_default(p4, val_pat))
      #(p5, ast.PatternProperty(key:, value: final_pat, shorthand: False))
    }
    next -> {
      // shorthand binding (with optional default) — validate the name as a binding identifier
      case is_valid_shorthand {
        False -> Error(UnexpectedToken(pos_of(p), prop_kind))
        True -> {
          use #(p3, _) <- result.try(validate_and_register_binding_no_advance(
            p,
            p2,
            prop_name,
          ))
          let ident = ast.IdentifierPattern(name: prop_name, span: span_of(p))
          use #(p4, value) <- result.map(case next {
            Equal -> {
              use #(p4, default) <- result.map(
                parse_assignment_expression(advance(p3)),
              )
              #(p4, ast.AssignmentPattern(left: ident, right: default))
            }
            _ -> Ok(#(p3, ident))
          })
          #(p4, ast.PatternProperty(key:, value: value, shorthand: True))
        }
      }
    }
  }
}

/// The current `Number` token as its AST literal — a NumberLiteral or a
/// BigIntLiteral, decided by `number.parse_numeric_literal` (which owns the
/// trailing-`n` test, the radix prefixes and Annex B legacy octal). Malformed
/// text is a hard SyntaxError, not a silently-cooked 0.
fn numeric_literal(p: P) -> Result(ast.Expression, ParseError) {
  let span = span_of(p)
  case number.parse_numeric_literal(peek_value(p)) {
    Ok(number.NumberValue(n)) -> Ok(ast.NumberLiteral(value: n, span:))
    Ok(number.BigIntValue(i)) -> Ok(ast.BigIntLiteral(value: i, span:))
    Error(err) -> Error(MalformedNumericLiteral(pos_of(p), err))
  }
}

/// The cooked value of the current string token, rejecting the Annex B escape
/// forms (`\07` legacy octal, `\8`/`\9` non-octal decimal) that strict code
/// forbids as an early SyntaxError (§12.9.4.1). The lexer accepts both forms
/// unconditionally, so this is the ONLY place they are rejected — every
/// consumer of a `KString` token's value must go through here (or through
/// `module_specifier_value`) or the check is silently skipped.
fn string_literal_value(p: P) -> Result(String, ParseError) {
  string_token_value(p, p.ctx.strict)
}

/// The cooked value of the current ModuleSpecifier token. Module code is
/// always strict, so the Annex B escape ban applies regardless of `ctx.strict`.
fn module_specifier_value(p: P) -> Result(String, ParseError) {
  string_token_value(p, True)
}

/// The value of an import/export specifier name (`{ x }`, `{ x as y }`,
/// `export * as y`). A ModuleExportName may be a StringLiteral, whose *string
/// value* — not its raw text — is the name, so escapes must be decoded and the
/// strict-only Annex B forms rejected. Identifier/keyword forms are passed
/// through untouched, exactly as before.
fn specifier_name_value(p: P) -> Result(String, ParseError) {
  case peek(p) {
    KString -> module_specifier_value(p)
    _ -> Ok(peek_value(p))
  }
}

fn string_token_value(p: P, strict: Bool) -> Result(String, ParseError) {
  use <- bool.guard(
    strict && peek_annex_b_legacy(p),
    Error(OctalEscapeStrictMode(pos_of(p))),
  )
  Ok(decode_string_escapes(peek_value(p)))
}

/// The current `Number` token as a property key — a `KeyNumber` or a
/// `KeyBigInt`, decided by `number.parse_numeric_literal` exactly as
/// `numeric_literal` decides between NumberLiteral and BigIntLiteral.
fn numeric_property_key(p: P) -> Result(ast.PropertyKey, ParseError) {
  let span = span_of(p)
  case number.parse_numeric_literal(peek_value(p)) {
    Ok(number.NumberValue(n)) -> Ok(ast.KeyNumber(value: n, span:))
    Ok(number.BigIntValue(i)) -> Ok(ast.KeyBigInt(value: i, span:))
    Error(err) -> Error(MalformedNumericLiteral(pos_of(p), err))
  }
}

/// An IdentifierName property key. Private names lex as Identifier tokens with
/// a `#` prefix, so that prefix is what separates `KeyPrivate` from
/// `KeyIdentifier` — this is the ONE place the distinction is drawn.
fn identifier_property_key(name: String, span: ast.Span) -> ast.PropertyKey {
  case name {
    "#" <> _ -> ast.KeyPrivate(name:, span:)
    _ -> ast.KeyIdentifier(name:, span:)
  }
}

/// Parse a PropertyName / ClassElementName into the `ast.PropertyKey` shape it
/// syntactically is — the parser is the only place that knows, so downstream
/// consumers never re-derive "was this computed?" from the key expression.
fn parse_property_name(p: P) -> Result(#(P, ast.PropertyKey), ParseError) {
  case peek(p) {
    Identifier ->
      Ok(#(advance(p), identifier_property_key(peek_value(p), span_of(p))))
    Number -> {
      use <- bool.guard(
        p.ctx.strict && peek_annex_b_legacy(p),
        Error(OctalLiteralStrictMode(pos_of(p))),
      )
      use key <- result.map(numeric_property_key(p))
      #(advance(p), key)
    }
    KString -> {
      use value <- result.map(string_literal_value(p))
      #(advance(p), ast.KeyString(value:, span: span_of(p)))
    }
    LeftBracket -> {
      // ComputedPropertyName : [ AssignmentExpression[+In] ] - brackets reset
      // the for-head no-in restriction: `for (C = class { get ['x' in o]()...`.
      use #(p4, expr) <- result.map({
        use p2 <- with_allow_in(advance(p), True)
        use #(p3, expr) <- result.try(parse_assignment_expression(p2))
        use p4 <- result.map(expect(p3, RightBracket))
        #(p4, expr)
      })
      #(p4, ast.KeyComputed(expr))
    }
    // Keywords that can be used as property names
    _ ->
      case is_identifier_or_keyword(peek(p)) {
        True ->
          Ok(#(advance(p), identifier_property_key(peek_value(p), span_of(p))))
        False -> Error(error_at_current(p, ExpectedPropertyName(pos_of(p))))
      }
  }
}

fn parse_if_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, condition) <- result.try(parse_expression(p3))
  use p5 <- result.try(expect(p4, RightParen))
  use #(p6, consequent) <- result.try(parse_single_statement(p5, True))
  case peek(p6) {
    Else -> {
      use #(p7, alternate) <- result.try(parse_single_statement(
        advance(p6),
        True,
      ))
      Ok(#(
        p7,
        ast.IfStatement(
          condition:,
          consequent:,
          alternate: option.Some(alternate),
        ),
      ))
    }
    _ ->
      Ok(#(p6, ast.IfStatement(condition:, consequent:, alternate: option.None)))
  }
}

fn parse_while_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, condition) <- result.try(parse_expression(p3))
  use p5 <- result.try(expect(p4, RightParen))
  let p5 = set_loop_depth(p5, p5.ctx.loop_depth + 1)
  use #(p6, body) <- result.try(parse_single_statement(p5, False))
  Ok(#(
    set_loop_depth(p6, p.ctx.loop_depth),
    ast.WhileStatement(condition:, body:),
  ))
}

fn parse_do_while_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  let p2 = set_loop_depth(p2, p2.ctx.loop_depth + 1)
  use #(p3, body) <- result.try(parse_single_statement(p2, False))
  use p4 <- result.try(expect(p3, While))
  use p5 <- result.try(expect(p4, LeftParen))
  use #(p6, condition) <- result.try(parse_expression(p5))
  use p7 <- result.try(expect(p6, RightParen))
  // Do-while has a special ASI rule: a semicolon is always
  // inserted after the closing ) if one is not present,
  // even without a line break (spec 11.9.1 rule 3)
  let p8 = case peek(p7) {
    Semicolon -> advance(p7)
    _ -> p7
  }
  Ok(#(
    set_loop_depth(p8, p.ctx.loop_depth),
    ast.DoWhileStatement(condition:, body:),
  ))
}

fn parse_for_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  // Check for for-await
  let #(p2, is_await) = case peek(p2) {
    Await -> #(advance(p2), True)
    _ -> #(p2, False)
  }
  // Increment loop_depth for the body (parsed inside parse_for_head)
  let p2 = set_loop_depth(p2, p2.ctx.loop_depth + 1)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, stmt) <- result.try(parse_for_head(p3, is_await))
  Ok(#(set_loop_depth(p4, p.ctx.loop_depth), stmt))
}

/// Overwrite `ctx.loop_depth` — used to enter/exit the body of an
/// IterationStatement (while / do-while / for).
fn set_loop_depth(p: P, depth: Int) -> P {
  P(..p, ctx: Ctx(..p.ctx, loop_depth: depth))
}

fn parse_for_head(
  p: P,
  is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  case peek(p) {
    Semicolon -> {
      // for(;;)
      let p2 = advance(p)
      parse_for_classic_rest(p2, None)
    }
    Var -> parse_for_declaration(p, is_await)
    Const -> parse_for_declaration_scoped(p, is_await)
    Let -> {
      // 'let' is a declaration if followed by a binding pattern,
      // otherwise it's an identifier (e.g., for (let in obj))
      case peek_at(p, 1) {
        Identifier | LeftBrace | LeftBracket ->
          parse_for_declaration_scoped(p, is_await)
        _ ->
          case is_contextual_keyword(peek_at(p, 1)) {
            True -> parse_for_declaration_scoped(p, is_await)
            False -> parse_for_expression(p, is_await)
          }
      }
    }
    // `for (using x of …)` / `for (using x = …;;)`. The for-of grammar's
    // [lookahead ≠ of] keeps `for (using of …)` an identifier loop variable —
    // but `for (using of = …;;)` (classic head) IS a declaration binding
    // `of`, like `for (let of = …;;)`.
    Identifier ->
      case
        is_using_decl_start(p, 0)
        && { peek_at(p, 1) != Of || peek_at(p, 2) == Equal }
      {
        True -> parse_for_using_scoped(p, is_await, is_await_using: False)
        False -> parse_for_expression(p, is_await)
      }
    // §14.7.5 lookahead restriction: the exact token pair `async of` is
    // forbidden as a ForInOf LHS (`for (async of [1])` is a SyntaxError).
    // Escaped `async` is allowed, `for await (async of …)` is allowed,
    // and `async of => {}` (a classic-for async arrow init) is allowed.
    Async ->
      case
        !is_await
        && !peek_had_escape(p)
        && peek_at(p, 1) == Of
        && peek_at(p, 2) != Arrow
      {
        True -> Error(InvalidForOfLhs(pos_of(p)))
        False -> parse_for_expression(p, is_await)
      }
    // `for (await using x of …)` — unlike plain `using`, `await using of`
    // IS an await-using declaration binding `of`.
    Await ->
      case is_await_using_decl_start(p) {
        True -> parse_for_using_scoped(p, is_await, is_await_using: True)
        False -> parse_for_expression(p, is_await)
      }
    _ -> parse_for_expression(p, is_await)
  }
}

/// Sub-scope wrapper for using declarations in for heads (mirrors
/// parse_for_declaration_scoped — head names live in their own scope).
fn parse_for_using_scoped(
  p: P,
  is_await: Bool,
  is_await_using is_await_using: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  use #(p2, stmt) <- result.map(parse_for_using_declaration(
    enter_block_scope(p),
    is_await,
    is_await_using,
  ))
  #(restore_block_scope(p2, p) |> exit_for_decl_context(p), stmt)
}

fn parse_for_using_declaration(
  p: P,
  is_await: Bool,
  is_await_using: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  // Consume `await`? `using`.
  let p2 = case is_await_using {
    True -> advance(advance(p))
    False -> advance(p)
  }
  let p2 = enter_lexical_decl_context(p2, scope.ConstBinding)
  use #(p3, pattern) <- result.try(parse_using_binding(p2))
  let kind = case is_await_using {
    True -> ast.AwaitUsing
    False -> ast.Using
  }
  case peek(p3) {
    In -> Error(UsingInForIn(pos_of(p3)))
    Of -> {
      let decl =
        ast.ForInitDeclaration(kind:, declarations: [
          ast.VariableDeclarator(id: pattern, init: None),
        ])
      parse_for_in_of_rest(exit_for_decl_context(p3, p), decl, True, is_await)
    }
    Equal -> {
      // Classic for head: `for (using x = a[, y = b]*; cond; update)`.
      // The declarator list is [~In] so `for (using x = a in b;;)` never
      // reads as a relational expression.
      use #(p6, declarators) <- result.try({
        use p4 <- with_allow_in(advance(p3), False)
        use #(p5, init_expr) <- result.try(parse_assignment_expression(p4))
        let first = ast.VariableDeclarator(id: pattern, init: Some(init_expr))
        use #(p6, rest) <- result.map(parse_using_remaining_declarators(p5, []))
        #(p6, [first, ..rest])
      })
      let decl = ast.ForInitDeclaration(kind:, declarations: declarators)
      use p7 <- result.try(expect(p6, Semicolon))
      parse_for_classic_rest(exit_for_decl_context(p7, p), Some(decl))
    }
    _ -> Error(UsingMissingInitializer(pos_of(p3)))
  }
}

fn parse_using_remaining_declarators(
  p: P,
  acc: List(ast.VariableDeclarator),
) -> Result(#(P, List(ast.VariableDeclarator)), ParseError) {
  case peek(p) {
    Comma -> parse_using_declarator_list(advance(p), acc)
    _ -> Ok(#(p, list.reverse(acc)))
  }
}

/// Wrapper around parse_for_declaration that creates a sub-scope for
/// let/const declarations. The declaration names live in this sub-scope,
/// not the enclosing block, so `for(let a;;); let a;` is valid.
fn parse_for_declaration_scoped(
  p: P,
  is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  // For-head let/const names live in a sub-scope, not the enclosing block,
  // so `for(let a;;); let a;` is valid.
  use #(p2, stmt) <- result.map(parse_for_declaration(
    enter_block_scope(p),
    is_await,
  ))
  #(restore_block_scope(p2, p) |> exit_for_decl_context(p), stmt)
}

/// Leave the for-head declaration context once its declarator list is fully
/// parsed, before the in/of right-hand side, condition, update, or body.
/// Otherwise the for-head's `BindingLexical` (and its accumulated `bound`
/// names) leaks into the rest of the statement and unrelated bindings
/// (params, catch params, var) are dup-checked against the for-head names.
fn exit_for_decl_context(p: P, outer: P) -> P {
  P(..p, ctx: Ctx(..p.ctx, binding_kind: outer.ctx.binding_kind))
}

/// Enter a let/const/using declarator-list context: `binding_kind` records
/// which kind the declarators bind as, and starts the same-declaration
/// duplicate-name check with an empty `bound` set.
fn enter_lexical_decl_context(p: P, kind: scope.BindingKind) -> P {
  P(
    ..p,
    ctx: Ctx(..p.ctx, binding_kind: BindingLexical(kind:, bound: set.new())),
  )
}

fn parse_for_declaration(
  p: P,
  is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  // var/let/const — convert the head token to its ast.VariableKind ONCE here;
  // the rest of the for-head parse is typed on VariableKind. Callers only
  // reach here with peek ∈ {Var, Let, Const}; assert it so a wrong head token
  // panics instead of silently becoming `var`.
  let kind = case peek(p) {
    Let -> ast.Let
    Const -> ast.Const
    Var -> ast.Var
    _ ->
      panic as "parser: parse_for_declaration entered with non-var/let/const head token"
  }
  let p2 = advance(p)
  let is_destr = peek(p2) == LeftBrace || peek(p2) == LeftBracket
  let p2 = case kind {
    ast.Let -> enter_lexical_decl_context(p2, scope.LetBinding)
    ast.Const | ast.Using | ast.AwaitUsing ->
      enter_lexical_decl_context(p2, scope.ConstBinding)
    ast.Var -> P(..p2, ctx: Ctx(..p2.ctx, binding_kind: BindingVar))
  }
  // B.3.4: a `for(var <pat> of …)` head's bound names must not collide
  // with an enclosing catch parameter. Only the names introduced BY this
  // for-of head count — any same-named binding that already existed in
  // the function scope (a function param, or an earlier `var`) is fine.
  let catch_params = scope.sb_nearest_catch_params(p2.sb)
  use #(p3, pattern) <- result.try(parse_for_binding_or_declarator(p2))
  let decl =
    ast.ForInitDeclaration(kind:, declarations: [
      ast.VariableDeclarator(id: pattern, init: None),
    ])
  // for-in/of iteration writes the head binding each step (§14.7.5.9).
  let mark_assigned = fn(px: P) {
    P(..px, sb: sb_mark_pattern_assigned(px.sb, pattern))
  }
  case peek(p3) {
    In ->
      parse_for_in_of_rest(
        mark_assigned(exit_for_decl_context(p3, p)),
        decl,
        False,
        False,
      )
    Of -> {
      // B.3.4: for-of var bindings must not shadow catch parameters
      use Nil <- result.try(case kind {
        ast.Var ->
          check_new_vars_vs_params(
            ast.pattern_bound_names(pattern),
            catch_params,
            pos_of(p3),
          )
        _ -> Ok(Nil)
      })
      parse_for_in_of_rest(
        mark_assigned(exit_for_decl_context(p3, p)),
        decl,
        True,
        is_await,
      )
    }
    Semicolon | Comma ->
      case kind {
        ast.Const -> Error(MissingConstInitializer(pos_of(p3)))
        _ ->
          case is_destr {
            True -> Error(DestructuringMissingInitializer(pos_of(p3)))
            False -> finish_for_classic_decl(p3, p, kind, pattern, None)
          }
      }
    Equal -> {
      // Disable 'in' as binary op in initializer so for(var x = a in b)
      // is for-in, not a binary expression
      use #(p5, init_expr) <- result.try(with_allow_in(
        advance(p3),
        False,
        parse_assignment_expression,
      ))
      case peek(p5) {
        In ->
          // for-in with initializer: always forbidden
          Error(ForInInitializer(pos_of(p5)))
        Of -> Error(ForOfInitializer(pos_of(p5)))
        Semicolon | Comma ->
          finish_for_classic_decl(p5, p, kind, pattern, Some(init_expr))
        _ -> Error(ExpectedForHeadSeparator(pos_of(p5)))
      }
    }
    _ -> Error(ExpectedForDeclSeparator(pos_of(p3)))
  }
}

/// Tail of `for (var/let/const ...; ...; ...)` once the first declarator's
/// pattern (and optional initializer) is known and the head is committed to
/// classic form: collect any `, name = init` declarators, consume the
/// trailing `;`, drop the for-decl context, then parse condition/update/body.
///
/// §14.7.4: the WHOLE `VariableDeclarationList` of a for head is [~In], not
/// just the first declarator, so the remaining declarators parse under
/// `with_allow_in(False)` too.
fn finish_for_classic_decl(
  p: P,
  outer: P,
  kind: ast.VariableKind,
  pattern: ast.Pattern,
  init: Option(ast.Expression),
) -> Result(#(P, ast.Statement), ParseError) {
  let first = ast.VariableDeclarator(id: pattern, init:)
  use #(p2, rest) <- result.try({
    use p <- with_allow_in(p, False)
    parse_remaining_declarators(p, kind, [])
  })
  let decl = ast.ForInitDeclaration(kind:, declarations: [first, ..rest])
  use p3 <- result.try(expect(p2, Semicolon))
  parse_for_classic_rest(exit_for_decl_context(p3, outer), Some(decl))
}

/// §13.15.1 / §13.15.5 early errors: in strict mode, `eval` and `arguments`
/// are not valid destructuring assignment targets. Walks a cover-grammar
/// array/object literal the way it will be reinterpreted as an
/// AssignmentPattern (engine262-style) and reports whether eval/arguments
/// appears in an actual TARGET position: array element, object property
/// value, rest target, or left of a default. Non-target positions —
/// initializer right-hand sides, computed keys, member-expression bases —
/// are not flagged. Non-literal expressions return False (other checks
/// handle them).
fn pattern_has_eval_args_target(expr: ast.Expression) -> Bool {
  case expr {
    ast.ArrayExpression(elements:, ..) ->
      list.any(elements, fn(elem) {
        case elem {
          None -> False
          Some(ast.SpreadElement(argument:, ..)) ->
            destructuring_target_is_eval_args(argument)
          Some(e) -> pattern_element_has_eval_args_target(e)
        }
      })
    ast.ObjectExpression(properties:, ..) ->
      list.any(properties, fn(prop) {
        case prop {
          ast.InitProperty(value:, ..) ->
            pattern_element_has_eval_args_target(value)
          ast.SpreadProperty(argument:) ->
            destructuring_target_is_eval_args(argument)
          // Methods/accessors are never valid destructuring targets — the
          // parser has already flagged the literal as an invalid pattern.
          ast.MethodProperty(..) | ast.AccessorProperty(..) -> False
        }
      })
    _ -> False
  }
}

/// One pattern element / property value: either `target` or
/// `target = default`. The default expression is NOT a target position.
fn pattern_element_has_eval_args_target(expr: ast.Expression) -> Bool {
  case expr {
    ast.AssignmentExpression(operator: ast.Assign, left:, ..) ->
      destructuring_target_is_eval_args(left)
    _ -> destructuring_target_is_eval_args(expr)
  }
}

/// Record every leaf IDENTIFIER assignment target inside `lhs` via
/// `scope.sb_assign_ref` so scope analysis can skip boxing never-
/// reassigned captures. Walks a cover-grammar array/object literal the
/// same way `pattern_has_eval_args_target` does; member expressions and
/// non-identifier leaves are ignored (not variable writes).
fn sb_mark_assign_targets(
  sb: scope.ScopeBuilder,
  lhs: ast.Expression,
) -> scope.ScopeBuilder {
  case lhs {
    ast.Identifier(name:, ..) -> scope.sb_assign_ref(sb, name)
    ast.ParenthesizedExpression(expression:, ..) ->
      sb_mark_assign_targets(sb, expression)
    ast.ArrayExpression(elements:, ..) ->
      list.fold(elements, sb, fn(sb, elem) {
        case elem {
          None -> sb
          Some(ast.SpreadElement(argument:, ..)) ->
            sb_mark_assign_targets(sb, argument)
          Some(e) -> sb_mark_assign_element(sb, e)
        }
      })
    ast.ObjectExpression(properties:, ..) ->
      list.fold(properties, sb, fn(sb, prop) {
        case prop {
          ast.InitProperty(value:, ..) -> sb_mark_assign_element(sb, value)
          ast.SpreadProperty(argument:) ->
            sb_mark_assign_targets(sb, argument)
          ast.MethodProperty(..) | ast.AccessorProperty(..) -> sb
        }
      })
    _ -> sb
  }
}

fn sb_mark_assign_element(
  sb: scope.ScopeBuilder,
  expr: ast.Expression,
) -> scope.ScopeBuilder {
  case expr {
    ast.AssignmentExpression(operator: ast.Assign, left:, ..) ->
      sb_mark_assign_targets(sb, left)
    _ -> sb_mark_assign_targets(sb, expr)
  }
}

/// `sb_assign_ref` every leaf name of a BINDING pattern. A declarator init
/// (`var x = e`) or for-in/of head is a write to each bound name; when a
/// `var` name collides with an outer ParamBinding the declare is a no-op,
/// so this is the only signal `never_box_names` sees.
fn sb_mark_pattern_assigned(
  sb: scope.ScopeBuilder,
  pattern: ast.Pattern,
) -> scope.ScopeBuilder {
  list.fold(ast.pattern_bound_names(pattern), sb, scope.sb_assign_ref)
}

/// DestructuringAssignmentTarget: a bare (possibly parenthesized)
/// eval/arguments identifier is the early error; nested array/object
/// literals recurse; member expressions (`eval.x`) are simple assignment
/// targets per §13.15.5 and stay legal.
fn destructuring_target_is_eval_args(expr: ast.Expression) -> Bool {
  case expr {
    ast.Identifier(name: "eval", ..) | ast.Identifier(name: "arguments", ..) ->
      True
    ast.ParenthesizedExpression(expression:, ..) ->
      destructuring_target_is_eval_args(expression)
    ast.ArrayExpression(..) | ast.ObjectExpression(..) ->
      pattern_has_eval_args_target(expr)
    _ -> False
  }
}

fn parse_for_expression(
  p: P,
  is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  // Save starting token to detect bare destructuring patterns
  let start_token = peek(p)
  // Disable 'in' as binary operator inside for-head so that
  // for (x in obj) is parsed as for-in, not binary expression;
  // `with_allow_in` puts it back for the rest of the for statement.
  use #(p2, expr) <- result.try(with_allow_in(p, False, parse_expression))
  {
    case peek(p2) {
      Semicolon ->
        // A `for(;;)` init is an ordinary expression, never a destructuring
        // pattern, so any deferred cover-grammar error is now due. (The
        // shorthand-default case reports InvalidDestructuringTarget here
        // rather than the generic ShorthandDefaultOutsideDestructuring.)
        case p2.ctx.has_cover_initializer, p2.ctx.dup_proto_pos {
          True, _ -> Error(InvalidDestructuringTarget(pos_of(p2)))
          False, Some(pos) -> Error(DuplicateProtoProperty(pos))
          False, None ->
            parse_for_classic_rest(
              advance(p2),
              Some(ast.ForInitExpression(expr)),
            )
        }
      In | Of -> {
        // Validate LHS is a valid assignment target for for-in/of.
        // Valid: identifiers, member exprs, bare destructuring {a}/[a].
        // Invalid: parenthesized patterns ({a})/([a]), unary, binary, etc.
        let is_bare_pattern =
          start_token == LeftBrace || start_token == LeftBracket
        let let_of_forbidden = start_token == Let && peek(p2) == Of
        let left = ast.ForInitExpression(expr)
        case
          !let_of_forbidden
          && {
            p2.last_expr_assignable
            || { is_bare_pattern && !p2.has_invalid_pattern }
            || is_web_compat_call_target(p2, expr)
          }
        {
          True ->
            // §13.15.1: in strict mode, eval/arguments as a destructuring
            // assignment TARGET is an early error — `for ({ eval } of …)` /
            // `for ([arguments] of …)`. Walk the LHS as an AssignmentPattern:
            // eval/arguments in non-target positions (initializer RHS,
            // computed keys, member-expression bases) stays legal.
            case
              is_bare_pattern
              && p2.ctx.strict
              && pattern_has_eval_args_target(expr)
            {
              True -> Error(EvalArgsAssignStrictMode(pos_of(p2)))
              False -> {
                // LHS accepted as assignment pattern — clear cover-grammar flags
                // so they don't leak into the for-body (e.g. `for ({x=1} of …) {…}`).
                let p2 =
                  P(
                    ..p2,
                    sb: sb_mark_assign_targets(p2.sb, expr),
                    has_invalid_pattern: False,
                    ctx: Ctx(
                      ..p2.ctx,
                      has_cover_initializer: False,
                      dup_proto_pos: None,
                    ),
                  )
                case peek(p2) {
                  In -> parse_for_in_of_rest(p2, left, False, False)
                  _ -> parse_for_in_of_rest(p2, left, True, is_await)
                }
              }
            }
          False ->
            case peek(p2) {
              In -> Error(InvalidForInLhs(pos_of(p2)))
              _ -> Error(InvalidForOfLhs(pos_of(p2)))
            }
        }
      }
      _ -> Error(ExpectedForSeparator(pos_of(p2)))
    }
  }
}

fn parse_for_binding_or_declarator(
  p: P,
) -> Result(#(P, ast.Pattern), ParseError) {
  parse_binding_pattern(p)
}

/// Collect the `, name = init` declarators that follow the first declarator
/// of a classic `for (var/let/const …;;)` head. A `,` commits to another
/// declarator (there is no alternative production), so a parse error there
/// is propagated rather than swallowed into a shorter declaration list.
fn parse_remaining_declarators(
  p: P,
  kind: ast.VariableKind,
  acc: List(ast.VariableDeclarator),
) -> Result(#(P, List(ast.VariableDeclarator)), ParseError) {
  case peek(p) {
    Comma -> {
      use #(p2, decl) <- result.try(parse_variable_declarator(advance(p), kind))
      parse_remaining_declarators(p2, kind, [decl, ..acc])
    }
    _ -> Ok(#(p, list.reverse(acc)))
  }
}

fn parse_for_in_of_rest(
  p: P,
  left: ast.ForInit,
  is_of: Bool,
  is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  use #(p3, right) <- result.try(case is_of {
    True -> parse_assignment_expression(p2)
    False -> parse_expression(p2)
  })
  use p4 <- result.try(expect(p3, RightParen))
  use #(p5, body) <- result.map(parse_single_statement(p4, False))
  case is_of {
    True -> #(p5, ast.ForOfStatement(left:, right:, body:, is_await:))
    False -> #(p5, ast.ForInStatement(left:, right:, body:))
  }
}

fn parse_for_classic_rest(
  p: P,
  init: Option(ast.ForInit),
) -> Result(#(P, ast.Statement), ParseError) {
  // condition (optional)
  case peek(p) {
    Semicolon -> parse_for_classic_update(advance(p), init, None)
    _ -> {
      use #(p2, condition) <- result.try(parse_expression(p))
      use p3 <- result.try(expect(p2, Semicolon))
      parse_for_classic_update(p3, init, Some(condition))
    }
  }
}

fn parse_for_classic_update(
  p: P,
  init: Option(ast.ForInit),
  condition: Option(ast.Expression),
) -> Result(#(P, ast.Statement), ParseError) {
  // update expression (optional)
  case peek(p) {
    RightParen -> {
      let p2 = advance(p)
      use #(p3, body) <- result.try(parse_single_statement(p2, False))
      Ok(#(p3, ast.ForStatement(init:, condition:, update: None, body:)))
    }
    _ -> {
      use #(p2, update) <- result.try(parse_expression(p))
      use p3 <- result.try(expect(p2, RightParen))
      use #(p4, body) <- result.try(parse_single_statement(p3, False))
      Ok(#(p4, ast.ForStatement(init:, condition:, update: Some(update), body:)))
    }
  }
}

fn parse_return_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  case p.ctx.function_depth > 0 {
    False -> Error(ReturnOutsideFunction(pos_of(p)))
    True -> parse_return_statement_body(p)
  }
}

fn parse_return_statement_body(
  p: P,
) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  case peek(p2) {
    Semicolon -> Ok(#(advance(p2), ast.ReturnStatement(argument: option.None)))
    RightBrace | Eof -> Ok(#(p2, ast.ReturnStatement(argument: option.None)))
    _ ->
      case has_line_break_before(p2) {
        True -> Ok(#(p2, ast.ReturnStatement(argument: option.None)))
        False -> {
          let start = pos_of(p2)
          use #(p3, expr) <- result.try(parse_expression(p2))
          // A returned value is an ordinary expression, never a destructuring
          // pattern: `return {a = 1};` is a SyntaxError.
          use Nil <- result.try(check_cover_grammar_errors(p3, start))
          use p4 <- result.try(eat_semicolon(p3))
          Ok(#(p4, ast.ReturnStatement(argument: option.Some(expr))))
        }
      }
  }
}

/// Parse the optional label after `break`/`continue`, handling ASI and the
/// no-line-terminator restriction. Validates the label exists in scope and —
/// for `continue` — that it denotes an IterationStatement (ES2024 §14.9.1).
fn parse_optional_label(
  p: P,
  label_use: LabelUse,
) -> Result(#(P, Option(String)), ParseError) {
  case peek(p) {
    Semicolon -> Ok(#(advance(p), option.None))
    Identifier ->
      case has_line_break_before(p) {
        True -> Ok(#(p, option.None))
        False -> {
          let label = peek_value(p)
          use Nil <- result.try(check_label_target(p, label, label_use))
          use p2 <- result.map(eat_semicolon(advance(p)))
          #(p2, option.Some(label))
        }
      }
    _ -> {
      use p2 <- result.map(eat_semicolon(p))
      #(p2, option.None)
    }
  }
}

/// Reject a `break L` / `continue L` whose label is out of scope, and a
/// `continue L` whose label does not denote an IterationStatement.
fn check_label_target(
  p: P,
  label: String,
  label_use: LabelUse,
) -> Result(Nil, ParseError) {
  case find_label(p.ctx.label_set, label), label_use {
    None, _ -> Error(UndefinedLabel(pos_of(p), label))
    Some(_), BreakLabel -> Ok(Nil)
    Some(LoopLabel), ContinueLabel -> Ok(Nil)
    Some(PlainLabel), ContinueLabel ->
      Error(ContinueToNonIterationLabel(pos_of(p), label))
  }
}

fn parse_break_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  use #(p2, label) <- result.try(parse_optional_label(advance(p), BreakLabel))
  case label {
    option.None ->
      case p.ctx.loop_depth > 0 || p.ctx.switch_depth > 0 {
        False -> Error(BreakOutsideLoopOrSwitch(pos_of(p)))
        True -> Ok(#(p2, ast.BreakStatement(label: option.None)))
      }
    option.Some(_) -> Ok(#(p2, ast.BreakStatement(label:)))
  }
}

fn parse_continue_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  use <- bool.guard(
    p.ctx.loop_depth <= 0,
    Error(ContinueOutsideLoop(pos_of(p))),
  )
  use #(p2, label) <- result.map(parse_optional_label(advance(p), ContinueLabel))
  #(p2, ast.ContinueStatement(label:))
}

fn parse_throw_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  // throw must have expression on same line (no line terminator after throw)
  use <- bool.guard(
    has_line_break_before(p2),
    Error(ThrowLineBreak(pos_of(p2))),
  )
  let start = pos_of(p2)
  use #(p3, expr) <- result.try(parse_expression(p2))
  // A thrown value is an ordinary expression, never a destructuring pattern:
  // `throw {a = 1};` is a SyntaxError.
  use Nil <- result.try(check_cover_grammar_errors(p3, start))
  use p4 <- result.try(eat_semicolon(p3))
  Ok(#(p4, ast.ThrowStatement(argument: expr)))
}

fn parse_try_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  use #(p3, block) <- result.try(parse_block_body(p2))
  use #(p4, handler) <- result.try(parse_catch_clause(p3))
  use #(p5, finalizer) <- result.try(case peek(p4) {
    Finally -> {
      use #(p, b) <- result.map(parse_block_body(advance(p4)))
      #(p, option.Some(b))
    }
    _ -> Ok(#(p4, option.None))
  })
  use tail <- result.map(case handler, finalizer {
    option.None, option.None -> Error(MissingCatchOrFinally(pos_of(p5)))
    option.Some(handler), option.None -> Ok(ast.TryCatch(handler:))
    option.None, option.Some(finalizer) -> Ok(ast.TryFinally(finalizer:))
    option.Some(handler), option.Some(finalizer) ->
      Ok(ast.TryCatchFinally(handler:, finalizer:))
  })
  #(p5, ast.TryStatement(block:, tail:))
}

fn parse_catch_clause(
  p: P,
) -> Result(#(P, option.Option(ast.CatchClause)), ParseError) {
  use <- bool.guard(peek(p) != Catch, Ok(#(p, option.None)))
  let p2 = advance(p)
  case peek(p2) {
    LeftParen -> {
      let p3 = advance(p2)
      // Push a Catch scope. BindingParam puts the catch param into the
      // catch scope as a ParamBinding (sb_declare), which allows var
      // redeclaration but blocks let/const redeclaration.
      let #(sb, catch_id) = scope.sb_push(p3.sb, scope.Catch)
      let p_inner =
        P(
          ..p3,
          sb:,
          ctx: Ctx(
            ..p3.ctx,
            in_block: True,
            binding_kind: BindingParam,
            // Enable dup param detection for catch destructured bindings
            in_formal_params: True,
            param_bound_names: [],
            has_non_simple_param: True,
          ),
        )
      use #(p4, param) <- result.try(parse_binding_pattern(p_inner))
      // §B.3.4: record whether the catch param is a bare identifier (or
      // absent) — a destructured catch param BLOCKS Annex B promotion.
      let simple = case param {
        ast.IdentifierPattern(..) -> True
        _ -> False
      }
      let p4 =
        P(
          ..p4,
          sb: scope.sb_update_current(p4.sb, fn(s) {
            scope.RawScope(..s, catch_param_simple: simple)
          }),
        )
      // A catch clause is NOT a function boundary, so the formal-parameter
      // state it borrowed for the binding must be handed back explicitly —
      // `p3.ctx` is the state at the `(`.
      use p5 <- result.try(expect(
        P(
          ..p4,
          ctx: Ctx(
            ..p4.ctx,
            binding_kind: BindingNone,
            in_formal_params: p3.ctx.in_formal_params,
            param_bound_names: p3.ctx.param_bound_names,
            has_non_simple_param: p3.ctx.has_non_simple_param,
          ),
        ),
        RightParen,
      ))
      // §14.15.2-3: the catch parameter lives in its OWN scope (catchEnv);
      // the Block that follows gets its own child env via the ordinary
      // Block path (parse_block_body → emit_block), so a closure in the
      // parameter's destructuring default can never see the body's lexical
      // declarations, and the parser/emit scope cursors agree on the body
      // Block (emit_block enters it iff it has declarations —
      // parse_block_body prunes it in lockstep).
      use #(p6, body) <- result.map(parse_block_body(p5))
      #(
        // restore_block_scope inlined: keep p6.sb's accumulated state;
        // restore only the current/current_fn cursors. The Catch's
        // children (param-default expression scopes, then the body Block)
        // were prepended newest-first by sb_push — flip them to the
        // source order finalize's children_at contract requires (this was
        // previously parse_function_body_block's reorder).
        P(
          ..p6,
          sb: scope.sb_reorder_block_children(p6.sb, catch_id)
            |> scope.sb_enter(p3.sb.current),
          ctx: Ctx(
            ..p6.ctx,
            in_block: p3.ctx.in_block,
            binding_kind: p3.ctx.binding_kind,
          ),
        ),
        option.Some(ast.CatchClause(param: option.Some(param), body:)),
      )
    }
    _ -> {
      // catch without binding — Block is required
      use #(p3, body) <- result.map(parse_block_body(p2))
      #(p3, option.Some(ast.CatchClause(param: option.None, body:)))
    }
  }
}

fn parse_switch_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, discriminant) <- result.try(parse_expression(p3))
  use p5 <- result.try(expect(p4, RightParen))
  use p6 <- result.try(expect(p5, LeftBrace))
  // Enter switch scope: ONE Block scope around all cases. Like nested
  // blocks, the CaseBlock scope may shadow params/catch params
  // (`catch (f) { switch (1) { case 1: function f() {} } }`).
  let #(sb, switch_id) = scope.sb_push(p6.sb, scope.Block)
  let p_inner =
    P(
      ..p6,
      sb:,
      ctx: Ctx(..p6.ctx, in_block: True, switch_depth: p6.ctx.switch_depth + 1),
    )
  use #(p7, cases) <- result.try(parse_switch_cases(p_inner, False, []))
  // Reorder children_at[switch_id] into emit_switch's consumption order
  // ([fn-decls from all case bodies] ++ [case tests] ++ [rest]) — see
  // scope.sb_reorder_switch_children. parse_switch_cases tagged test-children
  // with TagSwitchTest. The switch Block scope is NEVER pruned: emit_switch
  // unconditionally calls enter_scope, so the tree must keep the node even
  // when no case body declares anything.
  let sb =
    scope.sb_reorder_switch_children(p7.sb, switch_id)
    |> scope.sb_enter(p6.sb.current)
  Ok(#(
    // restore_block_scope inlined.
    P(
      ..p7,
      sb:,
      ctx: Ctx(
        ..p7.ctx,
        in_block: p6.ctx.in_block,
        switch_depth: p6.ctx.switch_depth,
      ),
    ),
    ast.SwitchStatement(discriminant:, cases: list.reverse(cases)),
  ))
}

fn parse_switch_cases(
  p: P,
  has_default: Bool,
  case_acc: List(ast.SwitchCase),
) -> Result(#(P, List(ast.SwitchCase)), ParseError) {
  case peek(p) {
    RightBrace -> Ok(#(advance(p), case_acc))
    Case -> {
      let p2 = advance(p)
      // emit_switch evaluates ALL case tests AFTER hoisted fn-decls but
      // BEFORE any case body (§14.12.4 CaseBlockEvaluation). Tag every
      // direct child scope pushed while parsing this test expression so
      // sb_reorder_switch_children can partition them into the middle
      // group. sb.current is the switch's Block scope here.
      let switch_id = p2.sb.current
      let mark = scope.sb_children_raw(p2.sb, switch_id)
      use #(p3, condition) <- result.try(parse_expression(p2))
      let p3 =
        P(
          ..p3,
          sb: scope.sb_tag_children_since(
            p3.sb,
            switch_id,
            mark,
            scope.TagSwitchTest,
          ),
        )
      use p4 <- result.try(expect(p3, Colon))
      parse_switch_case_stmts(p4, has_default, Some(condition), [], case_acc)
    }
    Default -> {
      case has_default {
        True -> Error(DuplicateDefaultCase(pos_of(p)))
        False -> {
          let p2 = advance(p)
          use p3 <- result.try(expect(p2, Colon))
          parse_switch_case_stmts(p3, True, None, [], case_acc)
        }
      }
    }
    _ -> Error(ExpectedCaseDefaultOrBrace(pos_of(p)))
  }
}

fn parse_switch_case_stmts(
  p: P,
  has_default: Bool,
  condition: Option(ast.Expression),
  stmt_acc: List(ast.StmtWithLine),
  case_acc: List(ast.SwitchCase),
) -> Result(#(P, List(ast.SwitchCase)), ParseError) {
  case peek(p) {
    RightBrace | Case | Default -> {
      let case_node =
        ast.SwitchCase(condition: condition, consequent: list.reverse(stmt_acc))
      parse_switch_cases(p, has_default, [case_node, ..case_acc])
    }
    _ -> {
      let line = line_of(p)
      // Statements here are DIRECTLY in the CaseClause/DefaultClause list —
      // using/await-using declarations are early errors in this position.
      use #(p2, stmt) <- result.try(parse_statement(
        P(..p, ctx: Ctx(..p.ctx, in_case_clause: True)),
      ))
      parse_switch_case_stmts(
        P(..p2, ctx: Ctx(..p2.ctx, in_case_clause: p.ctx.in_case_clause)),
        has_default,
        condition,
        [ast.StmtWithLine(line, stmt), ..stmt_acc],
        case_acc,
      )
    }
  }
}

/// Common head for function declarations and expressions: consume
/// `[async] function [*] [name]`. Returns the state AFTER the optional
/// name, the state AT the name token (for
/// `pos_of`/`span_of`), the generator flag, and the name (empty when
/// absent — the caller passes it to `enter_function_context` as the
/// body's `pending_strict_name`). `inner_name_ctx` validates the optional name against the
/// FUNCTION'S OWN is_generator/is_async (function expressions, whose name
/// is scoped to the body — `function yield(){}` is valid inside a
/// generator) instead of the caller's (function declarations).
fn parse_function_head(
  p: P,
  is_async: Bool,
  inner_name_ctx: Bool,
) -> Result(#(P, P, Bool, String), ParseError) {
  let p2 = case is_async {
    True -> advance(advance(p))
    False -> advance(p)
  }
  let is_generator = peek(p2) == Star
  let p3 = case is_generator {
    True -> advance(p2)
    False -> p2
  }
  let func_name = get_simple_binding_name(p3)
  let p_for_name = case inner_name_ctx {
    True ->
      P(
        ..p3,
        ctx: Ctx(..p3.ctx, in_generator: is_generator, in_async: is_async),
      )
    False -> p3
  }
  use p4 <- result.map(eat_optional_name(p_for_name))
  #(p4, p3, is_generator, func_name)
}

/// A `function` declaration in statement position. `parse_function_decl_impl`
/// yields the bare `FunctionLiteral` because the two export forms
/// (`export function f(){}`, `export default function(){}`) need it unwrapped.
fn parse_function_declaration(
  p: P,
  name_required: Bool,
  is_async: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  use #(p2, function) <- result.map(parse_function_decl_impl(
    p,
    name_required,
    is_async,
  ))
  #(p2, ast.declaration_to_statement(ast.DeclFunction(function:)))
}

fn parse_function_decl_impl(
  p: P,
  name_required: Bool,
  is_async: Bool,
) -> Result(#(P, ast.FunctionLiteral), ParseError) {
  use #(p4, p3, is_generator, func_name) <- result.try(parse_function_head(
    p,
    is_async,
    False,
  ))
  // Function declarations require a name (unless export default)
  use <- bool.guard(
    func_name == "" && name_required,
    Error(ExpectedIdentifier(pos_of(p3))),
  )
  // enter_function_context pushes the Function scope; tag it TagFnDecl
  // so sb_reorder_block_children at the enclosing block's `}` hoists it
  // ahead of sibling fn-expressions / arrows / classes — matching
  // emit.gleam's collect_hoisted_funcs consumption order.
  //
  // Tag ONLY when the resulting AST node is one emit's
  // `collect_hoisted_funcs` treats as hoisted — i.e. a
  // direct statement-list FunctionDeclaration after `peel_labels`. Two
  // syntactic positions reach here but are NOT direct-list members in
  // the lowered AST: (a) anonymous `export default function(){}` lowers
  // via `ast_util.module_items_to_stmts` to an ExpressionStatement, not a
  // FunctionDeclaration; (b) Annex-B sloppy `if (c) function f(){}`
  // wraps the decl in IfStatement, which `peel_labels` does not unwrap.
  // `in_single_stmt_pos` was set on the OUTER state by
  // `parse_single_statement_inner` and is reset by
  // `enter_function_context`, so check `p4` BEFORE entering.
  let is_hoisted_decl = func_name != "" && !p4.ctx.in_single_stmt_pos
  let p_fn =
    enter_function_context(
      p4,
      is_generator,
      is_async,
      string.to_option(func_name),
    )
  let p_fn = case is_hoisted_decl {
    True ->
      P(
        ..p_fn,
        sb: scope.sb_set_source_tag(p_fn.sb, p_fn.sb.current, scope.TagFnDecl),
      )
    False -> p_fn
  }
  use #(p5, params, body) <- result.try(
    parse_function_params_and_body(p_fn) |> restore_context_fn(p),
  )
  // Register function name in outer scope
  let p6 = case func_name {
    "" -> Ok(p5)
    name ->
      register_function_name(p5, name, pos_of(p3), !is_generator && !is_async)
  }
  use p7 <- result.try(p6)
  // The name identifier is the current token of `p3` (consumed above by
  // `eat_optional_name`), so `span_of(p3)` covers exactly the name text.
  let name_opt = optional_named_binding(func_name, span_of(p3))
  Ok(#(
    p7,
    ast.FunctionLiteral(
      name: name_opt,
      params: params,
      body: body,
      is_generator: is_generator,
      is_async: is_async,
    ),
  ))
}

fn parse_function_params_and_body(
  p: P,
) -> Result(#(P, List(ast.Pattern), List(ast.StmtWithLine)), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  // Parse params with BindingParam so names land in the function scope.
  // `enter_function_context` already zeroed the param state — this only
  // opens the parameter list.
  let p2 =
    P(
      ..p2,
      ctx: Ctx(..p2.ctx, in_formal_params: True, binding_kind: BindingParam),
    )
  use #(p3, params) <- result.try(parse_formal_parameters(p2))
  let p3 =
    P(
      ..p3,
      ctx: Ctx(..p3.ctx, in_formal_params: False, binding_kind: BindingNone),
    )
  use p4 <- result.try(expect(p3, RightParen))
  // Check for "use strict" directive at start of function body.
  // If transitioning to strict, retroactively check for dup params.
  let was_strict = p4.ctx.strict
  use p5 <- result.try(check_use_strict_in_body(p4))
  // §10.2.11 step 28: a non-simple parameter list (any default /
  // destructuring among the FIXED formals) routes positional args
  // through `<paramN>` shims; the user names become TDZ LetBindings
  // initialized by emit's destructuring prologue. Done HERE (after
  // the param names are already sb_declared, before `arguments`) so
  // the shims occupy declaration-indices 0..arity-1 — the runtime
  // calling convention writes arg N to the Nth declared slot.
  let p5 = P(..p5, sb: declare_param_shims(p5.sb, params))
  // §10.2.11 step 18: every non-arrow function has an implicit
  // `arguments` binding. Declared HERE (after params, before body) so
  // its RawBinding.index — and therefore the slot `finalize` assigns —
  // matches legacy `declare_var_boundary_body`, which records
  // `arguments` between the param bindings and the body's
  // direct_fn_names / hoisted_vars / lexicals. `sb_declare` is
  // first-wins so a same-named param suppresses it; a body-level
  // `let arguments` / `function arguments(){}` is a no-op against this
  // binding (matching legacy `add_binding`), and the conflict check in
  // `register_lexical_name` exempts it via `sb_only_implicit_arguments`
  // so the prior parser's no-error behavior is preserved. Arrows skip
  // this path entirely — they go through `parse_arrow_body`, not here.
  let p5 =
    P(
      ..p5,
      sb: scope.sb_declare(
        p5.sb,
        "arguments",
        scope.VarBinding,
        synthetic: True,
      ),
    )
  use #(p6, body) <- result.try(case !was_strict && p5.ctx.strict {
    True -> {
      use Nil <- result.try(check_pending_strict_function_name(p5))
      use Nil <- result.try(check_param_names_for_dups(p5))
      parse_fn_body_maybe_var_boundary(p5, params)
    }
    False -> parse_fn_body_maybe_var_boundary(p5, params)
  })
  Ok(#(p6, params, body))
}

/// When `params` is non-simple (§10.2.11 — any default / destructuring
/// among the FIXED formals), insert `<paramN>` shim ParamBindings at
/// the FRONT of the current function scope and shift the
/// already-declared formal names (recorded as ParamBinding by
/// `register_scope_binding` during `parse_formal_parameters`) past
/// them. The runtime calling convention writes positional arg N into
/// the Nth declared slot, so the shims must own indices 0..arity-1;
/// emit.compile_function_body then `emit_var_get`s each shim and
/// runs `emit_destructuring_bind` into the user names. No-op for
/// simple lists. Shared predicate (`ast_util.all_simple_params` over
/// `split_trailing_rest`'s fixed half) with emit.gleam — the two
/// MUST agree.
fn declare_param_shims(
  sb: scope.ScopeBuilder,
  params: List(ast.Pattern),
) -> scope.ScopeBuilder {
  case fixed_params_non_simple(params) {
    False -> sb
    True -> {
      let #(fixed, _rest) = ast_util.split_trailing_rest(params)
      scope.sb_insert_param_shims(sb, list.length(fixed))
    }
  }
}

/// §10.2.11: the FIXED formal list (trailing rest excluded) is non-simple —
/// it contains a default or a destructuring pattern. Single predicate for
/// the parser's two users (`declare_param_shims` and the var-boundary body
/// push in `parse_fn_body_maybe_var_boundary`); emit.compile_function_body
/// computes the identical `non_simple_fixed` over the same AST. They MUST
/// agree — emit consumes the body scope positionally.
fn fixed_params_non_simple(params: List(ast.Pattern)) -> Bool {
  let #(fixed, _rest) = ast_util.split_trailing_rest(params)
  !ast_util.all_simple_params(fixed)
}

/// Parse a function body block WITHOUT creating a new block scope.
/// The function's params are already in the function scope, so the body
/// shares the same scope as the params.
fn parse_function_body_block(
  p: P,
) -> Result(#(P, List(ast.StmtWithLine)), ParseError) {
  use p2 <- result.try(expect(p, LeftBrace))
  // Snapshot the enclosing scope's children BEFORE the body so the
  // hoist-reorder at `}` only repositions body-statement children —
  // param-default / catch-param-default scopes pushed by the caller
  // before this point must stay at the front in source order
  // (declare_var_boundary_body walks declare_pattern_exprs THEN
  // declare_stmts_hoist_order; emit's compile_function_body evaluates
  // param defaults before collect_hoisted_funcs).
  let body_id = p2.sb.current
  let mark = scope.sb_children_raw(p2.sb, body_id)
  use #(p3, stmts) <- result.try(parse_statement_list(p2, False, []))
  // Backstop for the deferred cover-grammar errors: `restore_outer_context`
  // is about to hand the enclosing `Ctx` back wholesale, dropping whatever
  // this body set. `enter_function_context` zeroed both flags on the way in,
  // so anything still set here is genuinely owed by an expression INSIDE this
  // body that no consumption point claimed (`function f(){ return {a=1}; }`) —
  // never the enclosing expression's debt, so this cannot false-positive on
  // `({a = 1}) => a` or `({a = 1} = b)`. Reported at the closing brace; the
  // wired consumption points below give sharper positions where they apply.
  use Nil <- result.try(check_cover_grammar_errors(p3, pos_of(p3)))
  use p4 <- result.try(expect(p3, RightBrace))
  // Reorder body children to hoist order (FunctionDeclarations first,
  // then everything else in source order). `sb.current` is the
  // function / catch / static-block scope the caller pushed; this is
  // the ONE chokepoint every `{...}` body that does NOT introduce its
  // own Block scope flows through, so the reorder lives here rather
  // than at each caller. The caller's sb_enter / restore_outer_context
  // runs after.
  let p4 = P(..p4, sb: scope.sb_reorder_body_children(p4.sb, body_id, mark))
  Ok(#(p4, stmts))
}

/// §10.2.11 step 28: when the fixed formal list is non-simple, the body's
/// VarDeclaredNames / top-level LexicallyDeclaredNames / hoisted
/// FunctionDeclarations live in a SEPARATE var-boundary Block scope (child
/// of the Function scope, which keeps the params, the `<paramN>` shims,
/// `arguments`, and the NFE self-name) so closures created by parameter
/// initializers cannot see body declarations. Simple lists keep the legacy
/// single-scope shape. emit.compile_function_body `enter_scope`s this Block
/// iff its `non_simple_fixed` is true — the predicates MUST stay in
/// lockstep, and the body scope must never be pruned.
fn parse_fn_body_maybe_var_boundary(
  p: P,
  params: List(ast.Pattern),
) -> Result(#(P, List(ast.StmtWithLine)), ParseError) {
  case fixed_params_non_simple(params) {
    False -> parse_function_body_block(p)
    True -> {
      let fn_id = p.sb.current
      let #(sb, _body_id) = scope.sb_push_var_boundary(p.sb)
      use #(p2, body) <- result.map(parse_function_body_block(P(..p, sb:)))
      // Re-enter the Function scope, then flip `children_at[fn root]`
      // (param-default scopes ++ this body scope, in sb_push's
      // newest-first order) to source order. parse_function_body_block
      // reordered the BODY scope's children; in the legacy single-scope
      // shape that same call reordered the fn root itself, so this
      // replaces it. No direct child of the fn root is TagFnDecl-tagged
      // (FunctionDeclaration statements can only appear inside the body),
      // so the hoist partition is the plain source-order reverse that
      // finalize's children_at contract requires.
      let sb = scope.sb_enter(p2.sb, fn_id)
      #(P(..p2, sb: scope.sb_reorder_block_children(sb, fn_id)), body)
    }
  }
}

/// Retroactively check the FUNCTION NAME (`ctx.pending_strict_name`, set by
/// `parse_function_head`) when "use strict" is discovered in the function
/// body. Catches cases like `function eval() { 'use strict'; }` — that is a
/// binding name, not a parameter name.
fn check_pending_strict_function_name(p: P) -> Result(Nil, ParseError) {
  case p.ctx.pending_strict_name {
    None -> Ok(Nil)
    Some(name) ->
      case strict_binding_violation(name) {
        Some(_) -> Error(StrictModeBindingName(pos_of(p), name))
        None -> Ok(Nil)
      }
  }
}

/// Retroactively check param names when "use strict" is discovered in body.
/// This catches cases like `function a(yield){ 'use strict'; }` where the
/// param was accepted before strict mode was activated.
fn check_param_names_for_dups(p: P) -> Result(Nil, ParseError) {
  check_param_names_list(p, p.ctx.param_bound_names, set.new())
}

fn check_param_names_list(
  p: P,
  names: List(String),
  seen: Set(String),
) -> Result(Nil, ParseError) {
  case names {
    [] -> Ok(Nil)
    [name, ..rest] -> {
      // Check for strict-mode reserved binding names
      case strict_binding_violation(name) {
        Some(kind) -> Error(strict_name_error(kind, name, pos_of(p)))
        None ->
          // Check for duplicates
          case set.contains(seen, name) {
            True -> Error(DuplicateParamNameStrictMode(pos_of(p), name))
            False -> check_param_names_list(p, rest, set.insert(seen, name))
          }
      }
    }
  }
}

/// Mark the param list as non-simple and retroactively check for duplicate
/// parameter names. Called when a destructured pattern, rest, or default
/// is encountered in the parameter list.
fn mark_non_simple_params(p: P) -> Result(P, ParseError) {
  let p = P(..p, ctx: Ctx(..p.ctx, has_non_simple_param: True))
  // Retroactively check accumulated names for duplicates
  use Nil <- result.try(check_param_names_for_dups_only(p))
  Ok(p)
}

/// Check param_bound_names for duplicate names only (no strict binding checks).
/// Used when transitioning to non-simple params to retroactively detect
/// duplicates like function(a, a, [b]){}.
fn check_param_names_for_dups_only(p: P) -> Result(Nil, ParseError) {
  check_names_for_dups_loop(p, p.ctx.param_bound_names, set.new())
}

fn check_names_for_dups_loop(
  p: P,
  remaining: List(String),
  seen: Set(String),
) -> Result(Nil, ParseError) {
  case remaining {
    [] -> Ok(Nil)
    [name, ..rest] ->
      case set.contains(seen, name) {
        True -> Error(DuplicateParameterName(pos_of(p), name))
        False -> check_names_for_dups_loop(p, rest, set.insert(seen, name))
      }
  }
}

/// The two ways a name can be forbidden as a strict-mode binding
/// (§13.1.1 Early Errors) — they report different messages, so the
/// classification travels with the answer.
type StrictNameKind {
  EvalOrArguments
  ReservedWord
}

/// The ONE list of names forbidden as bindings in strict mode.
/// `Some(kind)` when `name` is one of them.
fn strict_binding_violation(name: String) -> Option(StrictNameKind) {
  case name {
    "eval" | "arguments" -> Some(EvalOrArguments)
    "yield"
    | "implements"
    | "interface"
    | "package"
    | "private"
    | "protected"
    | "public"
    | "static"
    | "let" -> Some(ReservedWord)
    _ -> None
  }
}

/// The error a forbidden name reports when it appears as a PARAMETER name.
/// (A forbidden FUNCTION name reports `StrictModeBindingName` instead — see
/// `check_pending_strict_function_name`.)
fn strict_name_error(
  kind: StrictNameKind,
  name: String,
  pos: Int,
) -> ParseError {
  case kind {
    EvalOrArguments -> StrictModeParamName(pos, name)
    ReservedWord -> ReservedWordStrictMode(pos, name)
  }
}

/// Getter must have exactly 0 parameters
fn parse_getter_params_and_body(
  p: P,
) -> Result(#(P, List(ast.Pattern), List(ast.StmtWithLine)), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  case peek(p2) {
    RightParen -> {
      let p3 = advance(p2)
      // Check for "use strict" directive in getter body
      use p3 <- result.try(check_use_strict_in_body(p3))
      // §10.2.11 step 18: accessors are ordinary functions — the getter
      // body's `arguments` is its OWN arguments object, not a free name.
      // Mirrors `parse_function_params_and_body`'s implicit declare.
      let p3 =
        P(
          ..p3,
          sb: scope.sb_declare(
            p3.sb,
            "arguments",
            scope.VarBinding,
            synthetic: True,
          ),
        )
      use #(p4, body) <- result.try(parse_function_body_block(p3))
      Ok(#(p4, [], body))
    }
    _ -> Error(GetterNoParams(pos_of(p2)))
  }
}

/// Setter must have exactly 1 simple parameter (no rest)
fn parse_setter_params_and_body(
  p: P,
) -> Result(#(P, List(ast.Pattern), List(ast.StmtWithLine)), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  let p2 =
    P(
      ..p2,
      ctx: Ctx(..p2.ctx, in_formal_params: True, binding_kind: BindingParam),
    )
  case peek(p2) {
    RightParen -> Error(SetterExactlyOneParam(pos_of(p2)))
    DotDotDot -> Error(SetterNoRest(pos_of(p2)))
    _ -> {
      // Check if param is destructured (non-simple)
      let param_name = get_simple_binding_name(p2)
      let p2 = case param_name == "" {
        True -> P(..p2, ctx: Ctx(..p2.ctx, has_non_simple_param: True))
        False -> p2
      }
      use #(p3, pat) <- result.try(parse_binding_pattern(p2))
      // Default value makes params non-simple
      let p3 = case peek(p3) {
        Equal -> P(..p3, ctx: Ctx(..p3.ctx, has_non_simple_param: True))
        _ -> p3
      }
      let default_pos = pos_of(p3)
      use #(p4, final_pat) <- result.try(parse_pattern_default(p3, pat))
      // The default value is an ordinary expression: `set x(v = {a = 1}) {}`
      // is a SyntaxError. (`parse_formal_parameters` does the same for every
      // other parameter list; a setter's single formal bypasses it.)
      use Nil <- result.try(check_cover_grammar_errors(p4, default_pos))
      case peek(p4) {
        RightParen -> {
          let p5 =
            P(
              ..advance(p4),
              ctx: Ctx(
                ..p4.ctx,
                in_formal_params: False,
                binding_kind: BindingNone,
              ),
            )
          // Check for "use strict" directive
          use p5 <- result.try(check_use_strict_in_body(p5))
          // §10.2.11 step 28: a setter's single PropertySetParameterList
          // formal can be non-simple (`set x({a}) {}` / `set x(v=1) {}`).
          // emit.compile_function_body routes it through the `<param0>`
          // shim and resolves that name via scope.lookup, so it must be
          // declared here exactly as in `parse_function_params_and_body`.
          let p5 = P(..p5, sb: declare_param_shims(p5.sb, [final_pat]))
          // §10.2.11 step 18: like getters (and every non-arrow function),
          // a setter body has its own implicit `arguments` binding —
          // declared after the param + shims, before the body, exactly as
          // `parse_function_params_and_body` does (first-declaration-wins
          // against a parameter literally named `arguments`).
          let p5 =
            P(
              ..p5,
              sb: scope.sb_declare(
                p5.sb,
                "arguments",
                scope.VarBinding,
                synthetic: True,
              ),
            )
          use #(p6, body) <- result.try(
            parse_fn_body_maybe_var_boundary(p5, [final_pat]),
          )
          Ok(#(p6, [final_pat], body))
        }
        Comma -> Error(SetterExactlyOneParam(pos_of(p4)))
        _ -> Error(ExpectedCloseAfterSetter(pos_of(p4)))
      }
    }
  }
}

/// Dispatch to getter/setter/regular-method params+body parser, entering the
/// method context first and restoring the outer context after. Accessors
/// (get/set) never carry generator/async/constructor flags.
fn parse_method_params_body(
  p: P,
  outer: P,
  accessor_kind: AccessorPrefix,
  is_generator: Bool,
  is_async: Bool,
  is_constructor: Bool,
  has_extends: Bool,
) -> Result(#(P, List(ast.Pattern), List(ast.StmtWithLine)), ParseError) {
  let is_accessor = accessor_kind != NoAccessor
  let ctx =
    enter_method_context(
      p,
      is_generator && !is_accessor,
      is_async && !is_accessor,
      is_constructor && !is_accessor,
      has_extends,
    )
  case accessor_kind {
    GetPrefix -> parse_getter_params_and_body(ctx)
    SetPrefix -> parse_setter_params_and_body(ctx)
    NoAccessor -> parse_function_params_and_body(ctx)
  }
  |> restore_context_fn(outer)
}

fn parse_formal_parameters(
  p: P,
) -> Result(#(P, List(ast.Pattern)), ParseError) {
  case peek(p) {
    RightParen -> Ok(#(p, []))
    _ -> {
      let start = pos_of(p)
      use #(p2, params) <- result.try(
        parse_formal_parameter_list(p, set.new(), []),
      )
      // Parameter DEFAULTS are ordinary expressions (the parameter *targets*
      // are binding patterns, which never set these flags), so a cover
      // grammar left unconsumed by them is due now: `function f(x = {a=1}){}`
      // and `f(x = {__proto__:1, __proto__:2}){}` are SyntaxErrors. The
      // enclosing function boundary zeroed both flags, so nothing an outer
      // expression owes can be misattributed here.
      use Nil <- result.map(check_cover_grammar_errors(p2, start))
      #(p2, params)
    }
  }
}

fn parse_formal_parameter_list(
  p: P,
  seen: Set(String),
  acc: List(ast.Pattern),
) -> Result(#(P, List(ast.Pattern)), ParseError) {
  case peek(p) {
    DotDotDot -> {
      // Rest parameter — must be last, no default, no trailing params.
      // Rest makes the param list non-simple.
      use p <- result.try(mark_non_simple_params(p))
      let p2 = advance(p)
      let param_name = get_simple_binding_name(p2)
      use Nil <- result.try(check_duplicate_param(p2, param_name, seen))
      use #(p3, inner_pat) <- result.try(parse_binding_pattern(p2))
      // Rest cannot have default
      case peek(p3) {
        Equal -> Error(RestDefaultInitializer(pos_of(p3)))
        Comma -> Error(RestMustBeLast(pos_of(p3)))
        _ -> {
          let rest = ast.RestElement(argument: inner_pat)
          Ok(#(p3, list.reverse([rest, ..acc])))
        }
      }
    }
    _ -> {
      // Track simple identifier param names for duplicate checking
      let param_name = get_simple_binding_name(p)
      // If param is destructured (not a simple identifier), mark non-simple
      let is_non_simple = param_name == ""
      let p = case is_non_simple && !p.ctx.has_non_simple_param {
        True -> mark_non_simple_params(p)
        False -> Ok(p)
      }
      use p <- result.try(p)
      parse_formal_param_after_dup_check(p, param_name, seen, acc)
    }
  }
}

/// Parse a formal parameter after the non-simple check has been done.
fn parse_formal_param_after_dup_check(
  p: P,
  param_name: String,
  seen: Set(String),
  acc: List(ast.Pattern),
) -> Result(#(P, List(ast.Pattern)), ParseError) {
  use Nil <- result.try(check_duplicate_param(p, param_name, seen))
  let new_seen = case param_name {
    "" -> seen
    name -> set.insert(seen, name)
  }
  use #(p2, pat) <- result.try(parse_binding_pattern(p))
  // Optional default — makes param list non-simple
  case peek(p2) {
    Equal -> {
      let p2 = case !p2.ctx.has_non_simple_param {
        True -> mark_non_simple_params(p2)
        False -> Ok(p2)
      }
      use p2b <- result.try(p2)
      parse_formal_param_default(p2b, new_seen, pat, acc)
    }
    _ -> parse_formal_param_rest(p2, new_seen, [pat, ..acc])
  }
}

/// Parse a parameter default value and continue.
fn parse_formal_param_default(
  p: P,
  seen: Set(String),
  pat: ast.Pattern,
  acc: List(ast.Pattern),
) -> Result(#(P, List(ast.Pattern)), ParseError) {
  use #(p2, final_pat) <- result.try(parse_pattern_default(p, pat))
  parse_formal_param_rest(p2, seen, [final_pat, ..acc])
}

/// Continue parsing formal parameter list after a single parameter.
fn parse_formal_param_rest(
  p: P,
  seen: Set(String),
  acc: List(ast.Pattern),
) -> Result(#(P, List(ast.Pattern)), ParseError) {
  case peek(p) {
    Comma ->
      case peek_at(p, 1) {
        RightParen -> Ok(#(advance(p), list.reverse(acc)))
        _ -> parse_formal_parameter_list(advance(p), seen, acc)
      }
    _ -> Ok(#(p, list.reverse(acc)))
  }
}

/// Get the simple binding name if the current token is a simple identifier.
/// Returns "" for destructured patterns or non-identifier tokens.
fn get_simple_binding_name(p: P) -> String {
  case peek(p) {
    Identifier -> peek_value(p)
    _ ->
      case is_contextual_keyword(peek(p)) {
        True -> peek_value(p)
        False -> ""
      }
  }
}

/// Convert an optional name (empty string = absent) and its span into an
/// `Option(NamedBinding)` for AST construction.
fn optional_named_binding(
  name: String,
  span: ast.Span,
) -> Option(ast.NamedBinding) {
  case name {
    "" -> None
    n -> Some(ast.NamedBinding(name: n, span:))
  }
}

/// Check if a parameter name is a duplicate.
/// In strict mode, methods, or arrow functions, duplicate params are forbidden.
/// Also forbidden when params contain non-simple patterns (destructuring, rest, defaults).
fn check_duplicate_param(
  p: P,
  name: String,
  seen: Set(String),
) -> Result(Nil, ParseError) {
  use <- bool.guard(name == "", Ok(Nil))
  let must_be_unique =
    p.ctx.strict
    || p.ctx.in_arrow_params
    || p.ctx.in_method
    || p.ctx.has_non_simple_param
  use <- bool.guard(
    must_be_unique && set.contains(seen, name),
    Error(DuplicateParameterName(pos_of(p), name)),
  )
  Ok(Nil)
}

/// B.3.4: a for-of `var` binding must not shadow a same-named catch
/// parameter. `head_names` is the names bound by the for-of head's
/// pattern; `catch_params` is the nearest enclosing catch scope's
/// parameter names (empty when not inside a catch). Errors on the
/// intersection — only names this for-of head ITSELF declares are
/// checked, never pre-existing function-scope bindings.
fn check_new_vars_vs_params(
  head_names: List(String),
  catch_params: List(String),
  pos: Int,
) -> Result(Nil, ParseError) {
  use name <- list.try_each(head_names)
  case list.contains(catch_params, name) {
    True -> Error(IdentifierAlreadyDeclared(pos, name))
    False -> Ok(Nil)
  }
}

fn parse_class_declaration(p: P) -> Result(#(P, ast.Statement), ParseError) {
  use #(p2, decl) <- result.map(parse_class_decl_impl(p))
  #(p2, ast.declaration_to_statement(decl))
}

/// A named `class` declaration returned as an `ast.Declaration` — the shape
/// `export class C {}` needs. The anonymous form is only reachable through
/// `export default class {}`, which builds a `DefaultClass` instead.
fn parse_class_decl_impl(p: P) -> Result(#(P, ast.Declaration), ParseError) {
  // Class declarations are always lexical bindings (like let/const).
  use #(p2, name, super_class, body) <- result.map(parse_class_head_and_tail(
    p,
    True,
    True,
  ))
  #(p2, ast.DeclClass(name:, super_class:, body:))
}

/// Shared core for class declarations and class expressions: starting at the
/// `class` keyword, optionally consume the binding identifier (validated under
/// strict-mode rules — class names are always strict), optionally register it
/// as a lexical binding, then parse the heritage clause and body. Callers wrap
/// the returned pieces in ClassDeclaration / ClassExpression.
fn parse_class_head_and_tail(
  p: P,
  name_required: Bool,
  register_name: Bool,
) -> Result(
  #(P, Option(ast.NamedBinding), Option(ast.Expression), List(ast.ClassElement)),
  ParseError,
) {
  let p2 = advance(p)
  let is_name = peek(p2) == Identifier || is_contextual_keyword(peek(p2))
  case is_name {
    True -> {
      let name = peek_value(p2)
      // The class name identifier is the current token of `p2`.
      let name_span = span_of(p2)
      use Nil <- result.try(check_binding_identifier(
        P(..p2, ctx: Ctx(..p2.ctx, strict: True)),
        name,
      ))
      use p3 <- result.try(case register_name {
        True -> register_lexical_name(p2, name, scope.LetBinding, pos_of(p2))
        False -> Ok(p2)
      })
      use #(p4, super_class, body) <- result.map(parse_class_tail(
        advance(p3),
        Some(name),
      ))
      #(p4, Some(ast.NamedBinding(name:, span: name_span)), super_class, body)
    }
    False -> {
      use <- bool.guard(name_required, Error(ExpectedIdentifier(pos_of(p2))))
      use #(p3, super_class, body) <- result.map(parse_class_tail(p2, None))
      #(p3, None, super_class, body)
    }
  }
}

/// Per-class scope-tree context threaded through the body parse so each
/// element knows which pre-created synthetic shell to parent its nested
/// scopes under (matching `scope.declare_class`'s `fold_class_body` order).
type ClassScopeCtx {
  ClassScopeCtx(
    class_id: scope.ScopeId,
    init_id: scope.ScopeId,
    static_id: scope.ScopeId,
  )
}

/// Per-element record of which scope ids a class element introduced as
/// DIRECT children of the ClassBody scope. `key_scopes` are scopes pushed
/// while parsing a computed `[expr]` key (step 4 of `fold_class_body`).
///
/// A `ClassMethod` ALWAYS pushed exactly one Function scope for its body
/// (steps 2/5/6), so `MethodScopes` carries that id unconditionally; a
/// `ClassField` / `StaticBlock` never pushes one (they parent their body
/// scopes under `init_id`/`static_id` during the parse), so
/// `NonMethodScopes` has no place to put one. The old single-variant shape
/// with `method_fn_id: Option(ScopeId)` let a field claim a method's
/// function-scope id and forced a `[] -> None` fallback at the one site
/// that reads it back.
type ClassElementScopes {
  MethodScopes(key_scopes: List(scope.ScopeId), method_fn_id: scope.ScopeId)
  NonMethodScopes(key_scopes: List(scope.ScopeId))
}

const no_element_scopes = NonMethodScopes(key_scopes: [])

/// New direct children of `parent_id` that have appeared since `before`
/// (a snapshot taken via `scope.sb_children_raw`). Returned in push order
/// (oldest first). `children_at` stores newest-first, so the new ids are
/// the prefix of the current list.
fn class_new_children(
  sb: scope.ScopeBuilder,
  parent_id: scope.ScopeId,
  before: List(scope.ScopeId),
) -> List(scope.ScopeId) {
  let now = scope.sb_children_raw(sb, parent_id)
  list.take(now, list.length(now) - list.length(before)) |> list.reverse
}

fn parse_class_tail(
  p: P,
  name: Option(String),
) -> Result(#(P, Option(ast.Expression), List(ast.ClassElement)), ParseError) {
  // Class bodies (and extends clause) are always strict
  let saved_strict = p.ctx.strict
  // Save the OUTER scope so the matching `sb_enter` at the closing `}`
  // returns to it (accumulated scopes/refs flow forward).
  let outer_current = p.sb.current
  // Push the ClassBody scope BEFORE parsing the heritage: §15.7.14 sets
  // classScope as the running env first, so `class C extends C {}` TDZs
  // and any nested scopes in the heritage are children of the ClassBody
  // (declare_class step 3 walks the heritage with `class_id`).
  let #(sb, class_id) = scope.sb_push(p.sb, scope.ClassBody)
  let p = P(..p, sb:, ctx: Ctx(..p.ctx, strict: True))
  // Optional extends. Once `extends` is consumed a LeftHandSideExpression is
  // mandatory (ClassHeritage has no alternative production), so a parse error
  // in it is the user's error and must be propagated — backtracking to "no
  // heritage" would silently turn `class A extends {#x} {}` into
  // `class A { #x }` followed by an empty block.
  let has_extends = peek(p) == Extends
  use #(p2, super_class) <- result.try(case has_extends {
    True -> {
      use #(p2, expr) <- result.map(parse_left_hand_side_expression(advance(p)))
      #(p2, Some(expr))
    }
    False -> Ok(#(p, None))
  })
  // All direct children of class_id at this point came from the heritage
  // expression (oldest-first source order).
  let heritage_scopes = scope.sb_children_raw(p2.sb, class_id) |> list.reverse
  use p3 <- result.try(expect(p2, LeftBrace))
  // Pre-create the instance-field-init and static-init Function shells
  // (declare_class steps 1 and 7) so field initializers / static blocks
  // can parent their nested scopes under the correct shell as they are
  // parsed. Both are no-param non-arrow Functions, children of class_id.
  // Unneeded shells are discarded at the closing `}`.
  let #(sb, init_id) = scope.sb_push(p3.sb, scope.Function)
  let sb = scope.sb_enter(sb, class_id)
  let #(sb, static_id) = scope.sb_push(sb, scope.Function)
  let sb = scope.sb_enter(sb, class_id)
  let ctx = ClassScopeCtx(class_id:, init_id:, static_id:)
  // ClassHeritage above was parsed at the OUTER private depth (§15.7.14:
  // heritage is evaluated with outerPrivateEnvironment); the body is one
  // level deeper.
  let outer_depth = p3.class_private_depth
  let p3 = P(..p3, sb:, class_private_depth: outer_depth + 1)
  use #(p4, rev_tagged, declared) <- result.try(
    parse_class_body(p3, ctx, has_extends, False, dict.new(), []),
  )
  use p4 <- result.try(resolve_private_refs(p4, outer_depth, declared))
  let tagged = list.reverse(rev_tagged)
  let elements = list.map(tagged, fn(pair) { pair.0 })
  // 7-step reorder: produce children_at[class_id] in the EXACT order
  // `declare_class` would have, so emit.gleam's positional cursor stays
  // in lockstep.
  let sb =
    class_scope_finalize(p4.sb, ctx, name, has_extends, heritage_scopes, tagged)
  // Leave the ClassBody scope.
  let sb = scope.sb_enter(sb, outer_current)
  Ok(#(
    P(
      ..p4,
      sb:,
      ctx: Ctx(..p4.ctx, strict: saved_strict),
      class_private_depth: outer_depth,
    ),
    super_class,
    elements,
  ))
}

/// Port of `scope.declare_class`'s 7-step `fold_class_body` ordering,
/// applied to the ScopeBuilder at the class's closing `}` so that
/// `children_at[class_id]` matches the legacy DECLARE pass exactly:
///
///   (1) instance-field-init Function shell — IF any instance fields OR
///       instance private methods exist
///   (2) constructor Function — ALWAYS, even when synthetic
///   (3) heritage-expression nested scopes
///   (4) every computed-element-key's nested scopes, source order
///   (5)+(6) instance-method then static-method Function scopes
///   (7) static-init Function shell — IF any static fields / static blocks
///
/// Also declares the per-class synthetic ConstBindings
/// (`ast_util.class_body_bindings`) into the ClassBody scope.
fn class_scope_finalize(
  sb: scope.ScopeBuilder,
  ctx: ClassScopeCtx,
  name: Option(String),
  has_super_class: Bool,
  heritage_scopes: List(scope.ScopeId),
  tagged: List(#(ast.ClassElement, ClassElementScopes)),
) -> scope.ScopeBuilder {
  let elements = list.map(tagged, fn(pair) { pair.0 })
  // Inner class-name const + <class_fields_init> + private names +
  // private-method-fn stash + computed-key stash — same slot order the
  // emitter looks them up by (scope_id, name).
  let sb =
    list.fold(ast_util.class_body_bindings(name, elements), sb, fn(sb, n) {
      scope.sb_declare_in(
        sb,
        ctx.class_id,
        n,
        scope.ConstBinding,
        synthetic: True,
      )
    })
  // (4) every computed key, source order — flatten across elements.
  let key_scopes = list.flat_map(tagged, fn(pair) { { pair.1 }.key_scopes })
  // (5)+(6)+(2) partition method Function ids by kind/static-ness. The
  // buckets MUST match `ast_util.classify_class_body`'s exactly — emit
  // consumes these child scopes with a positional cursor — so both sides
  // ask the same exported predicates rather than re-deriving the shapes.
  let #(ctor_fn, instance_methods, static_methods) =
    list.fold(tagged, #(None, [], []), fn(acc, pair) {
      let #(ctor, im, sm) = acc
      let #(element, scopes) = pair
      case scopes {
        NonMethodScopes(..) -> acc
        MethodScopes(method_fn_id: id, ..) ->
          case ast_util.class_element_bucket(element) {
            ast_util.CeCtor -> #(Some(id), im, sm)
            ast_util.CeInstanceMethod -> #(ctor, [id, ..im], sm)
            ast_util.CeStaticMethod -> #(ctor, im, [id, ..sm])
            // MethodScopes tags only ClassMethod elements, so these arms are
            // unreachable in practice — listed for exhaustiveness.
            ast_util.CeInstanceField | ast_util.CeStaticElement -> acc
          }
      }
    })
  let instance_methods = list.reverse(instance_methods)
  let static_methods = list.reverse(static_methods)
  // (1) `needs_instance_init` — an instance field, or an instance private
  // method (§7.3.29 installs those from the field-init fn too).
  let needs_instance_init =
    list.any(elements, fn(el) {
      case el {
        ast.ClassMethod(key: ast.KeyPrivate(..), is_static: False, ..) -> True
        _ -> ast_util.is_instance_field(el)
      }
    })
  // (7) any static fields / static blocks — the same predicate
  // `classify_class_body` buckets `static_elements` with.
  let needs_static_init = list.any(elements, ast_util.is_static_element)
  // Discard pre-created shells the class doesn't need (declare_class
  // would not have created them); for kept shells, flip their
  // `children_at` from sb_push's newest-first to the oldest-first
  // emission order finalize hands to emit.gleam (these synthetic shells
  // never reach a per-scope reorder helper since no source `}` closes
  // them), and seed the synthetic refs/bindings emit.gleam will read
  // when it generates the shell's body — these don't appear in source
  // so parser-references never sees them (port of `ref_class`).
  let sb = case needs_instance_init {
    True ->
      class_seed_field_shell(sb, ctx.init_id, tagged, False)
      |> scope.sb_set_children(
        ctx.init_id,
        scope.sb_children_raw(sb, ctx.init_id) |> list.reverse,
      )
    False -> scope.sb_discard(sb, ctx.init_id)
  }
  let sb = case needs_static_init {
    True ->
      class_seed_field_shell(sb, ctx.static_id, tagged, True)
      |> scope.sb_set_children(
        ctx.static_id,
        scope.sb_children_raw(sb, ctx.static_id) |> list.reverse,
      )
    False -> scope.sb_discard(sb, ctx.static_id)
  }
  // (2) constructor — ALWAYS one Function child. Use the explicit
  // constructor's scope if one was parsed; otherwise mint a synthetic
  // empty Function child of class_id now.
  let #(sb, ctor_id) = case ctor_fn {
    Some(id) -> #(sb, id)
    None -> {
      let sb = scope.sb_enter(sb, ctx.class_id)
      let #(sb, id) = scope.sb_push(sb, scope.Function)
      #(sb, id)
    }
  }
  let sb =
    class_seed_ctor_shell(
      sb,
      ctor_id,
      needs_instance_init,
      option.is_none(ctor_fn),
      has_super_class,
    )
  // Assemble children_at[class_id] in the 7-step order. finalize hands
  // `children_at` to emit.gleam unchanged (oldest-first emission order).
  let init_part = case needs_instance_init {
    True -> [ctx.init_id]
    False -> []
  }
  let static_part = case needs_static_init {
    True -> [ctx.static_id]
    False -> []
  }
  let ordered =
    list.flatten([
      init_part,
      [ctor_id],
      heritage_scopes,
      key_scopes,
      instance_methods,
      static_methods,
      static_part,
    ])
  scope.sb_set_children(sb, ctx.class_id, ordered)
  |> scope.sb_enter(ctx.class_id)
}

/// Record a ref to the field-key stash const an emitted ClassFieldInit
/// reads: `\u{0}ck:N` for a computed key, the bare `#name` for a private
/// key, nothing for a public literal key (`ref_field_key` port).
fn class_ref_field_key(
  sb: scope.ScopeBuilder,
  key: ast.PropertyKey,
  idx: Int,
) -> scope.ScopeBuilder {
  case key {
    ast.KeyComputed(..) -> scope.sb_ref(sb, ast_util.computed_field_const(idx))
    ast.KeyPrivate(name:, ..) -> scope.sb_ref(sb, name)
    ast.KeyIdentifier(..) | ast.KeyString(..) | ast.KeyNumber(..) -> sb
    ast.KeyBigInt(..) -> sb
  }
}

/// Seed an instance/static field-init shell with the synthetic
/// refs/bindings emit.gleam's `compile_class_init_fn` will read — none
/// of these appear in source so the parse-time ref recorder never sees
/// them. Port of `ref_class`'s `instance_init`/`static_init` arms +
/// `declare_var_boundary_body` for an empty no-param non-arrow Function.
fn class_seed_field_shell(
  sb: scope.ScopeBuilder,
  shell_id: scope.ScopeId,
  tagged: List(#(ast.ClassElement, ClassElementScopes)),
  is_static: Bool,
) -> scope.ScopeBuilder {
  let sb = scope.sb_enter(sb, shell_id)
  // §10.2.11 step 22 — non-arrow Function gets `arguments`.
  let sb =
    scope.sb_declare_in(
      sb,
      shell_id,
      "arguments",
      scope.VarBinding,
      synthetic: True,
    )
  // Every ClassFieldInit emits `get_this`.
  let sb = scope.sb_lexical_ref(sb, lexical.RefThis)
  // §7.3.29 PrivateMethodOrAccessorAdd: each instance private
  // method/accessor reads `#x` and its closure stash const.
  let sb = case is_static {
    True -> sb
    False ->
      list.fold(tagged, sb, fn(sb, pair) {
        case pair.0 {
          ast.ClassMethod(
            key: ast.KeyPrivate(name:, ..),
            kind:,
            is_static: False,
            ..,
          ) ->
            sb
            |> scope.sb_ref(name)
            |> scope.sb_ref(ast_util.private_fn_const(kind, name))
          _ -> sb
        }
      })
  }
  // Each matching ClassFieldInit reads its stashed key.
  list.index_fold(tagged, sb, fn(sb, pair, idx) {
    case pair.0 {
      ast.ClassField(key:, is_static: s, ..) if s == is_static ->
        class_ref_field_key(sb, key, idx)
      _ -> sb
    }
  })
}

/// Seed the constructor scope — port of `ref_class`'s `ctor` arm. For an
/// explicit constructor the body's own refs were already recorded at
/// parse time; this only adds the synthetic `<class_fields_init>`/`this`
/// the emitter inserts before the user body, and the
/// `super(...arguments)` refs for a synthetic derived constructor.
fn class_seed_ctor_shell(
  sb: scope.ScopeBuilder,
  ctor_id: scope.ScopeId,
  needs_instance_init: Bool,
  is_synthetic: Bool,
  has_super_class: Bool,
) -> scope.ScopeBuilder {
  let sb = scope.sb_enter(sb, ctor_id)
  // Synthetic ctor never went through parse_function_params_and_body,
  // so `arguments` was never declared.
  let sb = case is_synthetic {
    True ->
      scope.sb_declare_in(
        sb,
        ctor_id,
        "arguments",
        scope.VarBinding,
        synthetic: True,
      )
    False -> sb
  }
  // emit_field_init_call reads `<class_fields_init>` + `this`.
  let sb = case needs_instance_init {
    True ->
      sb
      |> scope.sb_ref(ast_util.class_fields_init)
      |> scope.sb_lexical_ref(lexical.RefThis)
    False -> sb
  }
  // Synthetic derived ctor body is `super(...arguments)`.
  case is_synthetic && has_super_class {
    True ->
      sb
      |> scope.sb_lexical_ref(lexical.RefActiveFunc)
      |> scope.sb_lexical_ref(lexical.RefNewTarget)
      |> scope.sb_lexical_ref(lexical.RefThis)
      |> scope.sb_ref("arguments")
    False -> sb
  }
}

/// How a private name has been declared so far in a class body — used for
/// the §15.7.1 duplicate-private-name early error. A getter and setter pair
/// (with matching static-ness) may share a name; everything else may not.
type PrivateNameKind {
  PrivateGet
  PrivateSet
  PrivateGetSet
  PrivateOther
}

fn parse_class_body(
  p: P,
  ctx: ClassScopeCtx,
  has_extends: Bool,
  has_constructor: Bool,
  private_names: Dict(String, #(Bool, PrivateNameKind)),
  acc: List(#(ast.ClassElement, ClassElementScopes)),
) -> Result(
  #(
    P,
    List(#(ast.ClassElement, ClassElementScopes)),
    Dict(String, #(Bool, PrivateNameKind)),
  ),
  ParseError,
) {
  case peek(p) {
    RightBrace -> Ok(#(advance(p), acc, private_names))
    Semicolon ->
      parse_class_body(
        advance(p),
        ctx,
        has_extends,
        has_constructor,
        private_names,
        acc,
      )
    _ -> {
      use #(p2, found_constructor, element, el_scopes) <- result.try(
        parse_class_element(p, ctx, has_extends, has_constructor),
      )
      use private_names <- result.try(register_private_name(
        p2,
        private_names,
        element,
      ))
      parse_class_body(
        p2,
        ctx,
        has_extends,
        has_constructor || found_constructor,
        private_names,
        [#(element, el_scopes), ..acc],
      )
    }
  }
}

/// §15.7.1 ClassBody: PrivateBoundIdentifiers must not contain duplicates,
/// except a single getter/setter pair with the same static-ness.
fn register_private_name(
  p: P,
  private_names: Dict(String, #(Bool, PrivateNameKind)),
  element: ast.ClassElement,
) -> Result(Dict(String, #(Bool, PrivateNameKind)), ParseError) {
  case private_element_info(element) {
    None -> Ok(private_names)
    Some(#(name, is_static, kind)) ->
      case dict.get(private_names, name) {
        Error(Nil) -> Ok(dict.insert(private_names, name, #(is_static, kind)))
        Ok(#(prev_static, prev_kind)) ->
          case prev_static == is_static, prev_kind, kind {
            True, PrivateGet, PrivateSet | True, PrivateSet, PrivateGet ->
              Ok(dict.insert(private_names, name, #(is_static, PrivateGetSet)))
            _, _, _ -> Error(DuplicatePrivateName(pos_of(p), name))
          }
      }
  }
}

/// Extract `#name`, static-ness, and accessor kind from a class element with
/// a private (non-computed `#`-prefixed) name. None for public elements.
fn private_element_info(
  element: ast.ClassElement,
) -> Option(#(String, Bool, PrivateNameKind)) {
  case element {
    ast.ClassMethod(key: ast.KeyPrivate(name:, ..), kind:, is_static:, ..) -> {
      let private_kind = case kind {
        ast.MethodGet -> PrivateGet
        ast.MethodSet -> PrivateSet
        ast.MethodMethod | ast.MethodConstructor -> PrivateOther
      }
      Some(#(name, is_static, private_kind))
    }
    ast.ClassField(key: ast.KeyPrivate(name:, ..), is_static:, ..) ->
      Some(#(name, is_static, PrivateOther))
    ast.ClassMethod(..) | ast.ClassField(..) | ast.StaticBlock(..) -> None
  }
}

fn parse_class_element(
  p: P,
  ctx: ClassScopeCtx,
  has_extends: Bool,
  has_constructor: Bool,
) -> Result(#(P, Bool, ast.ClassElement, ClassElementScopes), ParseError) {
  // Skip static keyword — but only as a modifier, not as a method name
  let is_static = case peek(p) {
    Static ->
      case peek_at(p, 1) {
        // static followed by ( = ; } means it's a method/field name, not modifier
        LeftParen | Equal | Semicolon | RightBrace -> False
        _ -> True
      }
    _ -> False
  }
  let p2 = case is_static {
    True -> advance(p)
    False -> p
  }
  // §15.7.1 ClassStaticBlock: `static { ... }`. Distinct from a method named
  // `static` (handled by the LeftParen guard above) and from a computed-key
  // static method `static [k](){}` (LeftBracket).
  use <- bool.lazy_guard(is_static && peek(p2) == LeftBrace, fn() {
    // declare_class step (7): each static block becomes an arrow Function
    // child of the static-init shell. Re-enter that shell so the Function
    // pushed by enter_static_block_context parents under it; then re-tag
    // the pushed scope from ClassStaticBlock to Function + is_arrow.
    let p2_static = P(..p2, sb: scope.sb_enter(p2.sb, ctx.static_id))
    let p_body = enter_static_block_context(p2_static)
    let sb =
      scope.sb_update_current(p_body.sb, fn(s) {
        scope.RawScope(..s, kind: scope.Function)
      })
      |> scope.sb_update_current_fn(fn(fi) {
        scope.RawFunctionInfo(..fi, is_arrow: True)
      })
    let p_body = P(..p_body, sb:)
    // parse_function_body_block (NOT parse_block_body): the body's
    // direct lexicals/children must land in the arrow Function scope itself
    // with no intervening Block — declare_class step (7) does
    // `declare_var_boundary_body(b, arrow_id, None, [], body, is_arrow: True)`,
    // and emit's compile_class_init_fn consumes children_at[arrow_id]
    // directly. parse_block_body would push a Block child that survives
    // pruning whenever the body has a `let`/`const`/`class`/`function`,
    // desyncing emit's positional cursor and misplacing slot lookups.
    use #(p3, block) <- result.map(parse_function_body_block(p_body))
    // restore_outer_context restores the whole `Ctx` (incl. allow_super_*)
    // from p2_static, whose ctx is p2's, and would leave the cursor at
    // p2_static's scope (= static_id); re-enter class_id instead so the
    // NEXT class element parents under the ClassBody.
    let p3 =
      P(
        ..restore_outer_context(p3, p2_static),
        sb: scope.sb_enter(p3.sb, ctx.class_id),
      )
    #(p3, False, ast.StaticBlock(body: block), no_element_scopes)
  })
  // async / get / set / * modifiers. Each keyword is a name (not a modifier)
  // when followed by `( = ; }`. `Star` also terminates get/set: a getter or
  // setter can never be a generator, so `get *...` is a FIELD named "get"
  // followed by a generator method — legal only via ASI (line break before
  // `*`), which eat_semicolon enforces on the field path
  // (test262 grammar-field-named-get-followed-by-generator-asi).
  let #(p5, is_method_async, class_accessor_kind, is_generator) =
    parse_method_prefix(
      p2,
      fn(t) {
        case t {
          LeftParen | Equal | Semicolon | RightBrace -> True
          _ -> False
        }
      },
      True,
    )
  parse_class_element_body(
    p,
    p5,
    ctx,
    has_extends,
    has_constructor,
    is_method_async,
    is_generator,
    class_accessor_kind,
    is_static,
  )
}

/// Parse the property name, params, and body of a class element.
/// Returns the updated parser and whether this element was a constructor.
fn parse_class_element_body(
  outer_p: P,
  p5: P,
  ctx: ClassScopeCtx,
  has_extends: Bool,
  has_constructor: Bool,
  is_method_async: Bool,
  is_generator: Bool,
  class_accessor_kind: AccessorPrefix,
  is_static: Bool,
) -> Result(#(P, Bool, ast.ClassElement, ClassElementScopes), ParseError) {
  // Snapshot class_id's children before the key parse so any nested
  // scopes from a computed `[expr]` key (declare_class step 4) can be
  // identified by diff afterwards.
  let key_before = scope.sb_children_raw(p5.sb, ctx.class_id)
  use #(p6, key) <- result.try(parse_property_name(p5))
  let key_scopes = class_new_children(p6.sb, ctx.class_id, key_before)
  // All PropName-based early errors (§15.7.1) key off the *decoded*
  // PropertyKey — a string-literal PropName's PropName is its SV, so
  // `"constructor"` and `constructor` name the same member.
  let static_name = ast.property_key_static_name(key)
  // §15.7.1: PropName of a static ClassElement may not be "prototype".
  use <- bool.guard(
    is_static && static_name == Some("prototype"),
    Error(StaticPrototype(pos_of(p5))),
  )
  // §15.7.1: a non-static MethodDefinition whose PropName is "constructor"
  // is the class's constructor — SpecialMethod (get/set/*/async) and
  // duplicates are early errors.
  let is_constructor_name = static_name == Some("constructor")
  let is_constructor = !is_static && is_constructor_name
  use Nil <- result.try(case is_constructor {
    True ->
      case class_accessor_kind {
        GetPrefix -> Error(ClassConstructorNotGetter(pos_of(p5)))
        SetPrefix -> Error(ClassConstructorNotSetter(pos_of(p5)))
        NoAccessor -> {
          use <- bool.guard(
            is_generator,
            Error(ClassConstructorGenerator(pos_of(p5))),
          )
          use <- bool.guard(
            is_method_async,
            Error(ClassConstructorAsync(pos_of(p5))),
          )
          use <- bool.guard(
            has_constructor,
            Error(ClassDuplicateConstructor(pos_of(p5))),
          )
          Ok(Nil)
        }
      }
    False -> Ok(Nil)
  })
  // §15.7.1 ClassElementName : PrivateIdentifier — it is a Syntax Error if
  // StringValue is "#constructor". Applies to methods and fields alike.
  // A string-literal key `"#constructor"` is rejected too, matching V8/JSC.
  let is_private_constructor = case key {
    ast.KeyPrivate(name: "#constructor", ..)
    | ast.KeyString(value: "#constructor", ..) -> True
    _ -> False
  }
  use <- bool.guard(
    is_private_constructor,
    Error(PrivateNameConstructor(pos_of(p6))),
  )
  case peek(p6) {
    LeftParen -> {
      // Method — validate getter/setter params
      let method_kind = case is_constructor {
        True -> ast.MethodConstructor
        False ->
          case class_accessor_kind {
            GetPrefix -> ast.MethodGet
            SetPrefix -> ast.MethodSet
            NoAccessor -> ast.MethodMethod
          }
      }
      // parse_method_params_body → enter_method_context →
      // enter_function_context does sb_push(Function) under sb.current
      // (= class_id), so the method's Function scope is a direct child of
      // class_id (declare_class steps 2/5/6). Capture its id by diff.
      let body_before = scope.sb_children_raw(p6.sb, ctx.class_id)
      use #(p7, params, body) <- result.try(parse_method_params_body(
        p6,
        outer_p,
        class_accessor_kind,
        is_generator,
        is_method_async,
        is_constructor,
        has_extends,
      ))
      // `parse_method_params_body` always pushed exactly one Function scope
      // as a direct child of class_id, so this list is never empty.
      let assert [method_fn_id, ..] =
        class_new_children(p7.sb, ctx.class_id, body_before)
        as "parser: class method body pushed no Function scope"
      Ok(#(
        p7,
        is_constructor,
        ast.ClassMethod(
          key:,
          value: ast.FunctionLiteral(
            name: None,
            params: params,
            body: body,
            is_generator: is_generator,
            is_async: is_method_async,
          ),
          kind: method_kind,
          is_static: is_static,
        ),
        MethodScopes(key_scopes:, method_fn_id:),
      ))
    }
    _ -> {
      // §15.7.1: PropName of a FieldDefinition may not be "constructor"
      // (static or not).
      use <- bool.guard(
        is_constructor_name,
        Error(FieldNamedConstructor(pos_of(p6))),
      )
      // Field, optionally with `= initializer`. The initializer runs as a
      // method-like body, so super.x and new.target are allowed; super() is
      // not. §15.7.10: ContainsArguments of the initializer must be false
      // (in_class_field_init). declare_class steps (1)/(7): nested scopes
      // from the value expression are children of the instance-init /
      // static-init shell, not of the ClassBody — re-enter the appropriate
      // shell for the duration of the value parse.
      use #(p8, value) <- result.try(case peek(p6) {
        Equal -> {
          let shell_id = case is_static {
            True -> ctx.static_id
            False -> ctx.init_id
          }
          let p7 =
            P(
              ..advance(p6),
              sb: scope.sb_enter(p6.sb, shell_id),
              ctx: Ctx(
                ..p6.ctx,
                allow_super_property: True,
                allow_super_call: False,
                allow_new_target: True,
                in_class_field_init: True,
              ),
            )
          // §15.7: FieldDefinition's Initializer is [+In] — a class inside a
          // `for` head must not leak the head's [~In] into a field initializer.
          use #(p8, init) <- result.map(with_allow_in(
            p7,
            True,
            parse_assignment_expression,
          ))
          #(
            P(
              ..p8,
              sb: scope.sb_enter(p8.sb, ctx.class_id),
              ctx: Ctx(
                ..p8.ctx,
                allow_super_property: outer_p.ctx.allow_super_property,
                allow_super_call: outer_p.ctx.allow_super_call,
                allow_new_target: outer_p.ctx.allow_new_target,
                in_class_field_init: outer_p.ctx.in_class_field_init,
              ),
            ),
            Some(init),
          )
        }
        _ -> Ok(#(p6, None))
      })
      use p9 <- result.try(eat_semicolon(p8))
      Ok(#(
        p9,
        False,
        ast.ClassField(key:, value: value, is_static: is_static),
        NonMethodScopes(key_scopes:),
      ))
    }
  }
}

fn check_label_identifier(p: P, label: String) -> Result(Nil, ParseError) {
  case label {
    "yield" -> {
      use <- bool.guard(p.ctx.strict, Error(YieldReservedStrictMode(pos_of(p))))
      use <- bool.guard(p.ctx.in_generator, Error(YieldInGenerator(pos_of(p))))
      Ok(Nil)
    }
    "await" -> {
      use <- bool.guard(p.mode == Module, Error(AwaitInModule(pos_of(p))))
      use <- bool.guard(p.ctx.in_async, Error(AwaitInAsyncFunction(pos_of(p))))
      Ok(Nil)
    }
    _ -> Ok(Nil)
  }
}

fn parse_labeled_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  parse_label_chain(p, [])
}

/// Whether the parser is positioned at another `Label :` link of a label
/// chain. Mirrors the label dispatch in `parse_statement`.
fn at_label_start(p: P) -> Bool {
  case peek(p) {
    Identifier | Async | Yield | Await -> peek_at(p, 1) == Colon
    _ -> False
  }
}

/// Parse a whole `L1: L2: … Ln: Statement` label chain.
///
/// The chain is collected up-front (rather than one label per recursive
/// descent into `parse_statement`) because a label's kind is a property of
/// the CHAIN, not of the single label it follows: when the chain directly
/// prefixes an IterationStatement, `continue L` may target ANY of its
/// labels (ES2024 §14.13.1), so every one of them must be a LoopLabel.
///
/// `collected` holds the label names seen so far, innermost first.
fn parse_label_chain(
  p: P,
  collected: List(String),
) -> Result(#(P, ast.Statement), ParseError) {
  // identifier : ...
  let label = peek_value(p)
  // Check if yield/await are used as labels in restricted contexts
  use Nil <- result.try(check_label_identifier(p, label))
  // Check for duplicate labels — against the enclosing label set and the
  // labels already collected from this chain (`a: a: ;`).
  let duplicate =
    option.is_some(find_label(p.ctx.label_set, label))
    || list.contains(collected, label)
  use <- bool.guard(duplicate, Error(DuplicateLabel(pos_of(p), label)))
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, Colon))
  let collected = [label, ..collected]
  case at_label_start(p3) {
    True -> parse_label_chain(p3, collected)
    False -> parse_labeled_statement_body(p3, collected)
  }
}

/// Parse the statement behind a label chain. `labels` is innermost first;
/// every label gets the same LabelKind, decided by the statement's leading
/// token.
fn parse_labeled_statement_body(
  p3: P,
  labels: List(String),
) -> Result(#(P, ast.Statement), ParseError) {
  {
    // Determine if the labeled statement is a loop
    let kind = case peek(p3) {
      While | Do | For -> LoopLabel
      _ -> PlainLabel
    }
    let outer_labels = p3.ctx.label_set
    let p3 =
      P(
        ..p3,
        ctx: Ctx(
          ..p3.ctx,
          label_set: list.append(
            list.map(labels, fn(label) { #(label, kind) }),
            outer_labels,
          ),
        ),
      )
    let wrap_label = fn(res) {
      use #(inner_p, stmt) <- result.map(res)
      let labeled =
        list.fold(labels, stmt, fn(body, label) {
          ast.LabeledStatement(label:, body:)
        })
      #(P(..inner_p, ctx: Ctx(..inner_p.ctx, label_set: outer_labels)), labeled)
    }
    // Check for labeled generator declarations (always forbidden),
    // labeled lexical declarations (always forbidden),
    // and labeled function declarations (forbidden in strict mode)
    case peek(p3) {
      Const -> Error(LexicalDeclInLabel(pos_of(p3)))
      Let ->
        case
          peek_at(p3, 1) == LeftBrace
          || peek_at(p3, 1) == LeftBracket
          || is_identifier_or_keyword(peek_at(p3, 1))
        {
          True -> Error(LexicalDeclInLabel(pos_of(p3)))
          False -> wrap_label(parse_statement(p3))
        }
      Function -> {
        use <- bool.guard(
          peek_at(p3, 1) == Star,
          Error(GeneratorDeclLabeled(pos_of(p3))),
        )
        use <- bool.guard(
          p3.ctx.strict || p3.ctx.in_single_stmt_pos,
          Error(FunctionDeclInLabelBody(pos_of(p3))),
        )
        wrap_label(parse_statement(p3))
      }
      // `label: using x = …` / `label: await using x = …` — a labelled item
      // must be a Statement, never a using declaration.
      Identifier ->
        case is_using_decl_start(p3, 0) {
          True -> Error(LexicalDeclInLabel(pos_of(p3)))
          False -> wrap_label(parse_statement(p3))
        }
      Await ->
        case is_await_using_decl_start(p3) {
          True -> Error(LexicalDeclInLabel(pos_of(p3)))
          False -> wrap_label(parse_statement(p3))
        }
      _ -> wrap_label(parse_statement(p3))
    }
  }
}

fn parse_with_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  use <- bool.guard(p.ctx.strict, Error(WithNotAllowedStrictMode(pos_of(p))))
  parse_with_statement_body(p)
}

fn parse_with_statement_body(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, object) <- result.try(parse_expression(p3))
  use p5 <- result.try(expect(p4, RightParen))
  // Push a With scope: `sb_push_with` mints its `<withN_M>` holder name onto
  // the `With(holder)` kind and declares the holder binding inside the new
  // scope, so child closures can capture the with-object and `scope.lookup`
  // can find the holder's slot.
  let #(sb, with_id) = scope.sb_push_with(p5.sb)
  use #(p6, body) <- result.try(parse_single_statement(P(..p5, sb:), False))
  // Flip children_at[with_id] to source order before the pop — the With
  // body is a single statement so no FunctionDeclaration can be a direct
  // child here; the partition is a no-op and this is just the reverse
  // that finalize's `children_at` contract requires.
  let sb = scope.sb_reorder_block_children(p6.sb, with_id)
  Ok(#(
    P(..p6, sb: scope.sb_enter(sb, p5.sb.current)),
    ast.WithStatement(object:, body:),
  ))
}

/// Raise the deferred cover-grammar early errors (`{a = 0}` shorthand
/// default, duplicate `__proto__`) of the expression just parsed, now that
/// it is known NOT to have been consumed as a destructuring pattern.
///
/// Wired into the ordinary-expression consumption points that would otherwise
/// let the deferred error escape: expression statements, the `for(;;)` head, a
/// concise arrow body, formal-parameter (and setter-parameter) defaults, `return`
/// / `throw` arguments, and variable-declarator initializers. A function BODY is
/// also a hard boundary — `restore_outer_context` discards these flags — so
/// `parse_function_body_block` runs a backstop check at the closing brace to
/// catch any consumption point not enumerated above.
///
/// `pos` is where the SyntaxError is reported for the shorthand-default case
/// (the `__proto__` case carries the duplicate property's own recorded position).
fn check_cover_grammar_errors(p: P, pos: Int) -> Result(Nil, ParseError) {
  case p.ctx.has_cover_initializer, p.ctx.dup_proto_pos {
    True, _ -> Error(ShorthandDefaultOutsideDestructuring(pos))
    False, Some(dup_pos) -> Error(DuplicateProtoProperty(dup_pos))
    False, None -> Ok(Nil)
  }
}

fn parse_expression_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  // Capture the raw string content before parsing, so a lone string-literal
  // statement can be recorded as a Directive with its un-decoded text.
  let directive_raw = case peek(p) {
    KString -> option.Some(peek_value(p))
    _ -> option.None
  }
  use #(p2, expr) <- result.try(parse_expression(p))
  use Nil <- result.try(check_cover_grammar_errors(p2, pos_of(p)))
  use p3 <- result.try(eat_semicolon(p2))
  // A Directive is an expression statement whose expression is exactly a
  // string literal (not e.g. a concatenation), so only tag it then.
  let directive = case expr {
    ast.StringExpression(..) -> directive_raw
    _ -> option.None
  }
  Ok(#(p3, ast.ExpressionStatement(expression: expr, directive:)))
}

// ---- Expression parsing (Pratt parser) ----

fn parse_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  // Comma-separated expression list
  use #(p2, first_expr) <- result.try(parse_assignment_expression(p))
  case peek(p2) {
    Comma ->
      case peek_at(p2, 1) {
        // Don't consume comma if it's the end of something
        RightParen | RightBracket | RightBrace | Eof -> Ok(#(p2, first_expr))
        _ -> {
          let p3 = advance(p2)
          use #(p4, rest_expr) <- result.try(parse_expression(p3))
          // Comma expression is not a valid assignment target
          Ok(#(
            P(..p4, last_expr_assignable: False),
            ast.SequenceExpression(
              expressions: [first_expr, rest_expr],
              span: ast.Span(ast.expression_span(first_expr).start, p4.prev_end),
            ),
          ))
        }
      }
    _ -> Ok(#(p2, first_expr))
  }
}

fn parse_assignment_expression(
  p: P,
) -> Result(#(P, ast.Expression), ParseError) {
  case peek(p) {
    Yield -> {
      // Only parse yield expression inside generators.
      // Outside generators, yield is an identifier (sloppy mode) or reserved
      // word (strict mode), both handled by parse_assignment_expression_inner.
      case p.ctx.in_generator {
        False -> parse_assignment_expression_inner(p)
        True ->
          case peek_at(p, 1) {
            // If followed by assignment or member access, treat yield as
            // identifier reference (errors since we're in a generator)
            Equal
            | PlusEqual
            | MinusEqual
            | StarEqual
            | StarStarEqual
            | PercentEqual
            | AmpersandEqual
            | PipeEqual
            | CaretEqual
            | LessThanLessThanEqual
            | GreaterThanGreaterThanEqual
            | GreaterThanGreaterThanGreaterThanEqual
            | AmpersandAmpersandEqual
            | PipePipeEqual
            | QuestionQuestionEqual
            | Dot
            | QuestionDot
            | Arrow -> parse_assignment_expression_inner(p)
            // NOTE: LeftBracket is NOT in this list - inside a generator
            // `yield [...]` is a YieldExpression whose operand is an array
            // literal (`yield [...yield]`), never member access on an
            // identifier named yield (yield is not an IdentifierReference
            // in generator bodies).
            _ -> parse_yield_expression(p)
          }
      }
    }
    _ -> parse_assignment_expression_inner(p)
  }
}

fn parse_assignment_expression_inner(
  p: P,
) -> Result(#(P, ast.Expression), ParseError) {
  // Try arrow function: (params) => body or ident => body
  case try_arrow_function(p) {
    Ok(#(p2, arrow_expr)) -> Ok(#(p2, arrow_expr))
    // A ParseError raised after the `=>` was consumed is a real error in a
    // committed arrow function — report it. Backtracking here would swallow
    // it and re-parse the arrow head as a plain expression, producing a
    // misleading error at a later position.
    Error(ArrowError(e)) -> Error(e)
    Error(NotAnArrow) -> {
      // In strict mode, check if this is a simple assignment to eval/arguments
      // before parsing — if current token is eval/arguments and next is assignment op
      case p.ctx.strict {
        True ->
          case peek(p) {
            Identifier ->
              case peek_value(p) {
                "eval" | "arguments" -> {
                  let name = peek_value(p)
                  case option.is_some(assignment_op(peek_at(p, 1))) {
                    True -> Error(StrictModeAssignment(pos_of(p), name))
                    False -> parse_assignment_rhs(p)
                  }
                }
                _ -> parse_assignment_rhs(p)
              }
            _ -> parse_assignment_rhs(p)
          }
        False -> parse_assignment_rhs(p)
      }
    }
  }
}

fn parse_assignment_rhs(p: P) -> Result(#(P, ast.Expression), ParseError) {
  // Remember the LHS start token for destructuring pattern detection
  let lhs_start = peek(p)
  use #(p2, lhs_expr) <- result.try(parse_conditional_expression(p))
  case peek(p2) {
    Equal ->
      case p2.last_expr_assignable {
        True -> {
          // §13.15.1: eval/arguments is not a valid assignment target in
          // strict mode even when parenthesized — `(eval) = 1`. The direct
          // (unparenthesized) form is caught before the LHS is parsed.
          use Nil <- result.try(check_strict_restricted_target(p2, lhs_expr))
          // Assignment result is not a valid assignment target, but
          // IS a valid cover for AssignmentPattern (target = default)
          finish_assignment(p2, lhs_expr, ast.Assign, Some(True))
        }
        // For plain =, allow object/array patterns as LHS
        False ->
          case lhs_start {
            LeftBrace | LeftBracket ->
              case p2.has_invalid_pattern {
                True -> Error(InvalidDestructuringTarget(pos_of(p2)))
                False ->
                  // §13.15.1: strict-mode eval/arguments as a destructuring
                  // assignment target — walk the LHS as an AssignmentPattern
                  // so non-target uses (`[x = eval] = []`) stay legal.
                  case p.ctx.strict && pattern_has_eval_args_target(lhs_expr) {
                    True -> Error(EvalArgsAssignStrictMode(pos_of(p2)))
                    False ->
                      finish_assignment(
                        P(
                          ..p2,
                          has_invalid_pattern: False,
                          ctx: Ctx(..p2.ctx, dup_proto_pos: None),
                        ),
                        lhs_expr,
                        ast.Assign,
                        Some(True),
                      )
                  }
              }
            _ ->
              case is_web_compat_call_target(p2, lhs_expr) {
                True -> finish_assignment(p2, lhs_expr, ast.Assign, Some(False))
                False -> Error(InvalidAssignmentLhs(pos_of(p2)))
              }
          }
      }
    _ ->
      case assignment_op(peek(p2)) {
        Some(op) -> {
          // Compound assignment — LHS must be a simple assignment target.
          // Annex B ~web-compat~ call targets are allowed for `op=` but NOT
          // for logical assignment (&&=, ||=, ??=), which stay early errors.
          let web_compat_ok = case op {
            ast.LogicalAndAssign
            | ast.LogicalOrAssign
            | ast.NullishCoalesceAssign -> False
            _ -> is_web_compat_call_target(p2, lhs_expr)
          }
          case p2.last_expr_assignable || web_compat_ok {
            True -> {
              use Nil <- result.try(check_strict_restricted_target(p2, lhs_expr))
              finish_assignment(p2, lhs_expr, op, None)
            }
            False -> Error(InvalidAssignmentLhs(pos_of(p2)))
          }
        }
        // Not an assignment — clear the flag so it doesn't leak
        // from a previous sibling expression (e.g. [a = 1, 0])
        None -> Ok(#(P(..p2, last_expr_is_assignment: False), lhs_expr))
      }
  }
}

/// Common tail for the four success branches of parse_assignment_rhs:
/// advance past the `=`/`op=` token, parse the RHS, build the
/// AssignmentExpression, and clear `last_expr_assignable`.
/// `last_is_assignment` sets the result flag; None preserves the RHS parse's
/// value (compound assignment leaves it untouched).
fn finish_assignment(
  p2: P,
  lhs: ast.Expression,
  op: ast.AssignmentOp,
  last_is_assignment: Option(Bool),
) -> Result(#(P, ast.Expression), ParseError) {
  let p2 = P(..p2, sb: sb_mark_assign_targets(p2.sb, lhs))
  let p3 = advance(P(..p2, ctx: Ctx(..p2.ctx, has_cover_initializer: False)))
  use #(p4, rhs) <- result.map(parse_assignment_expression(p3))
  let p_out = case last_is_assignment {
    Some(flag) ->
      P(..p4, last_expr_assignable: False, last_expr_is_assignment: flag)
    None -> P(..p4, last_expr_assignable: False)
  }
  #(
    p_out,
    ast.AssignmentExpression(
      operator: op,
      left: lhs,
      right: rhs,
      span: ast.Span(ast.expression_span(lhs).start, p4.prev_end),
    ),
  )
}

/// §13.15.1 strict-mode early error: assignment to eval/arguments, including
/// the parenthesized form `(eval) = 1`. Direct unparenthesized simple
/// assignment is also caught before the LHS is parsed.
fn check_strict_restricted_target(
  p: P,
  lhs: ast.Expression,
) -> Result(Nil, ParseError) {
  case p.ctx.strict, ast_util.unwrap_parens(lhs) {
    True, ast.Identifier(name:, ..) ->
      case name {
        "eval" | "arguments" -> Error(StrictModeAssignment(pos_of(p), name))
        _ -> Ok(Nil)
      }
    _, _ -> Ok(Nil)
  }
}

/// Annex B web-compat AssignmentTargetType (~web-compat~): in sloppy mode a
/// CallExpression is accepted as an assignment/update/for-in-of target at
/// parse time; the runtime evaluates the call and then throws ReferenceError.
/// The ~web-compat~ AssignmentTargetType attaches only to
/// `CallExpression : CoverCallExpressionAndAsyncArrowHead` and
/// `CallExpression Arguments` — never to OptionalExpression, whose
/// AssignmentTargetType is ~invalid~ (§13.3.1.1). So `a?.b() = 1` stays an
/// early SyntaxError even in sloppy mode (test262
/// language/expressions/optional-chaining/static-semantics-simple-assignment.js).
fn is_web_compat_call_target(p: P, lhs: ast.Expression) -> Bool {
  case p.ctx.strict, ast_util.unwrap_parens(lhs) {
    False, ast.CallExpression(callee:, ..) ->
      !ast_util.chain_has_optional(callee)
    _, _ -> False
  }
}

/// Outcome of a speculative arrow-function parse.
///
/// `NotAnArrow` means the tokens do not form an arrow head (or the head is
/// ambiguous with a parenthesized expression / call and failed to parse as
/// arrow parameters): the caller must backtrack and parse a non-arrow
/// expression instead. `ArrowError` carries a real `ParseError` raised
/// AFTER arrow-ness was committed (the `=>` token was consumed): the caller
/// must propagate it, never backtrack over it.
type ArrowAttempt {
  NotAnArrow
  ArrowError(ParseError)
}

fn try_arrow_function(p: P) -> Result(#(P, ast.Expression), ArrowAttempt) {
  case peek(p) {
    // async (...) => or async ident =>
    Async -> {
      case peek_at(p, 1) {
        LeftParen -> try_paren_arrow(p, advance(advance(p)), True)
        Identifier ->
          case peek_at(p, 2) {
            Arrow -> try_single_ident_arrow(p, advance(p), True)
            _ -> Error(NotAnArrow)
          }
        _ -> Error(NotAnArrow)
      }
    }
    Identifier | Yield | Await ->
      case peek_at(p, 1) {
        Arrow -> try_single_ident_arrow(p, p, False)
        _ -> Error(NotAnArrow)
      }
    // (a, b) => is the hardest case — vs (a, b) as expression.
    // We speculatively parse and backtrack if it's not an arrow.
    LeftParen -> try_paren_arrow(p, advance(p), False)
    _ -> Error(NotAnArrow)
  }
}

/// Parse `ident => …` or `async ident => …` once the caller has confirmed
/// the `Arrow` lookahead. `ident_p` is positioned on the identifier token;
/// `outer` is the parser at the very start of the expression (the `async`
/// token, or the identifier itself when not async) — used for error
/// positions and for `finish_arrow` to restore the enclosing context.
fn try_single_ident_arrow(
  outer: P,
  ident_p: P,
  is_async: Bool,
) -> Result(#(P, ast.Expression), ArrowAttempt) {
  let name = peek_value(ident_p)
  // Async-arrow params are [+Await] — `await` (incl. an escaped `await`,
  // which lexes as Identifier) is reserved as a binding name there. The
  // non-async case checks against the enclosing context unchanged.
  let check_p = case is_async {
    True -> P(..outer, ctx: Ctx(..outer.ctx, in_async: True))
    False -> outer
  }
  // The `=>` has not been consumed yet, so arrow-ness is not committed: a
  // name that can't be a binding identifier here means "not an arrow head",
  // and the caller backtracks to parse a non-arrow expression.
  use Nil <- result.try(
    check_binding_identifier(check_p, name)
    |> result.replace_error(NotAnArrow),
  )
  let p2 = advance(ident_p)
  // No line terminator allowed before =>
  case has_line_break_before(p2) {
    True -> Error(NotAnArrow)
    False -> {
      let p3 = enter_arrow_context(advance(p2), is_async, [name])
      // Save param name for the retroactive strict-mode check in the body.
      // Set AFTER entering the arrow's context, which zeroes the boundary
      // state (including `param_bound_names`) as it does for every function.
      let p3 = P(..p3, ctx: Ctx(..p3.ctx, param_bound_names: [name]))
      // A bare-identifier param list is always simple — passed through for
      // the (no-op) var-boundary predicate only.
      let params = [ast.IdentifierPattern(name: name, span: span_of(ident_p))]
      finish_arrow(parse_arrow_body(p3, params), outer, is_async, params)
    }
  }
}

/// Speculatively parse `(...) =>` (or `async (...) =>` when is_async).
/// `p_params` is the parser positioned just past the opening paren; `outer`
/// is the parser at the start of the whole expression, used for error
/// positions and to restore context after the body.
fn try_paren_arrow(
  outer: P,
  p_params: P,
  is_async: Bool,
) -> Result(#(P, ast.Expression), ArrowAttempt) {
  // Enter the arrow's function context BEFORE parsing parameters so the
  // arrow's Function scope is pushed before param-default expressions are
  // parsed — their bindings/refs then record under the arrow's scope, not
  // the parent's. This is speculative: every failure path below returns an
  // Error and the caller resumes from the original `outer` P, so the whole
  // immutable parser state (including the pushed scope) is discarded for
  // free — no ExpressionScope-style buffering needed.
  let p_ctx = enter_arrow_context(p_params, is_async, [])
  // Arrow params always check for duplicate names (even sloppy mode).
  // We set in_arrow_params to True to enable unconditional dup checking
  // without enabling all strict-mode restrictions.
  let p_arrow =
    P(
      ..p_ctx,
      ctx: Ctx(
        ..p_ctx.ctx,
        in_arrow_params: True,
        in_formal_params: True,
        binding_kind: BindingParam,
        // §15.3 ArrowParameters[?Yield, ?Await] — arrow params inherit the
        // ENCLOSING yield/await context (unlike full functions, whose params
        // use the function's own context). enter_arrow_context above set the
        // BODY's context; restore the enclosing flags for the duration of
        // param parsing so `yield`/`await`/`arguments` in defaults behave as
        // before. Async-arrow formals refine to [+Await] via §15.9
        // AsyncArrowHead, hence the `|| is_async`.
        in_generator: p_params.ctx.in_generator,
        in_async: p_params.ctx.in_async || is_async,
        in_static_block: p_params.ctx.in_static_block,
        in_class_field_init: p_params.ctx.in_class_field_init,
        // FormalParameters take no [In] parameter — a default initializer is
        // always [+In], even inside a `for` head:
        // `for (let f = (a = 'a' in {}) => a;;) …`. The BODY reverts to the
        // arrow's inherited [In] below.
        allow_in: True,
      ),
    )
  case parse_formal_parameters(p_arrow) {
    Ok(#(p3, params)) ->
      case
        expect(
          P(
            ..p3,
            ctx: Ctx(
              ..p3.ctx,
              in_arrow_params: False,
              in_formal_params: False,
              binding_kind: BindingNone,
            ),
          ),
          RightParen,
        )
      {
        Ok(p4) ->
          case peek(p4) {
            Arrow ->
              // Line terminator before => is not allowed
              case has_line_break_before(p4) {
                True -> Error(NotAnArrow)
                False -> {
                  // Already inside the arrow's function context (entered
                  // above before params); switch the four enclosing-context
                  // flags that were temporarily restored for param parsing
                  // back to the arrow body's own values. The arrow's
                  // ParamBindings are already in p4.sb (started empty in
                  // p_ctx, populated by parse_formal_parameters); for a
                  // non-simple list, retrofit `<paramN>` shims at the
                  // front and rekind the user names to LetBinding so
                  // the runtime arg→slot layout and emit's
                  // destructuring prologue agree.
                  let p5 =
                    P(
                      ..advance(p4),
                      sb: declare_param_shims(p4.sb, params),
                      ctx: Ctx(
                        ..p4.ctx,
                        in_generator: p_ctx.ctx.in_generator,
                        in_async: p_ctx.ctx.in_async,
                        in_static_block: p_ctx.ctx.in_static_block,
                        in_class_field_init: p_ctx.ctx.in_class_field_init,
                        allow_in: p_ctx.ctx.allow_in,
                      ),
                    )
                  finish_arrow(
                    parse_arrow_body(p5, params),
                    outer,
                    is_async,
                    params,
                  )
                }
              }
            // No `=>` after the parenthesized list: an ordinary
            // parenthesized expression / call arguments — backtrack.
            _ -> Error(NotAnArrow)
          }
        // The speculative parameter parse consumed something that isn't a
        // `)`-terminated arrow head — backtrack.
        Error(_speculative_error) -> Error(NotAnArrow)
      }
    // Not parseable as formal parameters (e.g. `(a.b)`) — backtrack and
    // re-parse as a parenthesized expression.
    Error(_speculative_error) -> Error(NotAnArrow)
  }
}

/// Finish a COMMITTED arrow function: the `=>` has been consumed and
/// `body_result` is the parse of its body. A body error is a real error in
/// this arrow (`ArrowError`), never a reason to backtrack.
fn finish_arrow(
  body_result: Result(#(P, ast.ArrowBody), ParseError),
  outer: P,
  is_async: Bool,
  params: List(ast.Pattern),
) -> Result(#(P, ast.Expression), ArrowAttempt) {
  use #(p_body, body) <- result.try(result.map_error(body_result, ArrowError))
  // Span: `outer` is positioned at the arrow's first source token (`async`
  // for async arrows, otherwise `(` or the bare-identifier param), and the
  // body ends at the last token consumed by parse_arrow_body. Capture the
  // end byte BEFORE restore_outer_context so the span is independent of any
  // future prev_end adjustment in the restore.
  let body_end = p_body.prev_end
  // `ctx: outer.ctx` also hands the ENCLOSING expression back its deferred
  // cover-grammar errors, which the arrow's own params/body context zeroed:
  // `({a = 1}, () => {})` still owes a SyntaxError after the arrow.
  let p_restored = restore_outer_context(p_body, outer)
  // §13.15.1: AssignmentTargetType of ArrowFunction is ~invalid~ — clear the
  // assignable flag leaked from the arrow body's final expression so
  // `(x => x) = 1` is rejected.
  Ok(#(
    P(..p_restored, last_expr_assignable: False, last_expr_is_assignment: False),
    ast.ArrowFunctionExpression(
      params:,
      body:,
      is_async:,
      span: ast.Span(start: pos_of(outer), end: body_end),
    ),
  ))
}

fn parse_arrow_body(
  p: P,
  params: List(ast.Pattern),
) -> Result(#(P, ast.ArrowBody), ParseError) {
  // The cover-grammar flags are already clear here: arrow params are a valid
  // destructuring context (`({a = 0}) => …` is valid; a duplicate __proto__
  // in an object BINDING pattern is likewise not an early error), and
  // `enter_arrow_context` zeroed the enclosing expression's flags — which
  // `finish_arrow`'s `restore_outer_context` will hand straight back.
  case peek(p) {
    LeftBrace -> {
      // §14.2 ConciseBody : { FunctionBody } — a BLOCK body takes no [In]
      // parameter, so it is [+In] even inside a `for` head (only the
      // ExpressionBody arm below inherits the arrow's [In]).
      let p = P(..p, ctx: Ctx(..p.ctx, allow_in: True))
      // Check for "use strict" directive in arrow body.
      // A simple param list shares the arrow's Function scope with the
      // body (parse_function_body_block); a non-simple one gets the
      // §10.2.11 step-28 var-boundary body scope.
      use p <- result.try(check_use_strict_in_body(p))
      use #(p2, body_stmt) <- result.try(parse_fn_body_maybe_var_boundary(
        p,
        params,
      ))
      Ok(#(p2, ast.ArrowBodyBlock(body_stmt)))
    }
    _ -> {
      let start = pos_of(p)
      use #(p2, expr) <- result.try(parse_assignment_expression(p))
      // A concise body is an ordinary expression, and `finish_arrow` is about
      // to drop this context — so `() => ({a = 1})` must raise its deferred
      // cover-grammar error HERE, not leak it to the enclosing statement.
      use Nil <- result.try(check_cover_grammar_errors(p2, start))
      // Flip children_at[arrow-fn] to source order. The block-body
      // branch above goes through `parse_function_body_block` which
      // does the equivalent `sb_reorder_body_children`; this branch
      // returns straight to `restore_outer_context`'s bare `sb_enter`,
      // so without this the arrow's children (param-default scopes +
      // any scopes inside the body expression) would stay
      // reverse-source. No FunctionDeclaration can appear in either
      // position so the partition is a no-op — this is just the
      // reverse that finalize's contract requires. `p.sb.current` is
      // the arrow's Function scope (pushed by `enter_arrow_context`
      // before this function was called).
      let sb = scope.sb_reorder_block_children(p2.sb, p.sb.current)
      Ok(#(P(..p2, sb:), ast.ArrowBodyExpression(expr)))
    }
  }
}

fn parse_yield_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  // Yield expressions are forbidden in formal parameter defaults
  case p.ctx.in_formal_params {
    True -> Error(YieldInFormalParameter(pos_of(p)))
    False -> parse_yield_expression_inner(p)
  }
}

fn parse_yield_expression_inner(
  p: P,
) -> Result(#(P, ast.Expression), ParseError) {
  let start = pos_of(p)
  let p2 = advance(p)
  // Bare `yield` with no argument — span covers just the `yield` keyword.
  let bare =
    ast.YieldExpression(
      argument: None,
      is_delegate: False,
      span: span_from(start, p2),
    )
  // No line terminator is permitted between `yield` and its operand (or `*`).
  use <- bool.guard(has_line_break_before(p2), Ok(#(p2, bare)))
  case peek(p2) {
    Semicolon | RightParen | RightBracket | RightBrace | Eof | Comma | Colon ->
      Ok(#(p2, bare))
    // yield* delegate — consume `*` then parse the operand.
    Star ->
      yield_with_arg(start, True, parse_assignment_expression(advance(p2)))
    // yield /regex/ — slash after yield starts a regex in generator context.
    Slash | SlashEqual -> yield_with_arg(start, False, parse_regex_literal(p2))
    _ -> yield_with_arg(start, False, parse_assignment_expression(p2))
  }
}

fn yield_with_arg(
  start: Int,
  is_delegate: Bool,
  parsed: Result(#(P, ast.Expression), ParseError),
) -> Result(#(P, ast.Expression), ParseError) {
  use #(p, arg) <- result.map(parsed)
  #(
    p,
    ast.YieldExpression(
      argument: Some(arg),
      is_delegate:,
      span: span_from(start, p),
    ),
  )
}

fn parse_conditional_expression(
  p: P,
) -> Result(#(P, ast.Expression), ParseError) {
  use #(p2, test_expr) <- result.try(parse_binary_expression(p, 0))
  case peek(p2) {
    Question -> {
      let p3 = advance(p2)
      // §13.14: the middle operand is AssignmentExpression[+In] regardless of
      // the inherited [In]; only the alternative inherits [?In].
      use #(p4, consequent) <- result.try(with_allow_in(
        p3,
        True,
        parse_assignment_expression,
      ))
      use p5 <- result.try(expect(p4, Colon))
      // Conditional expression is not a valid assignment target
      use #(p6, alternate) <- result.try(parse_assignment_expression(p5))
      Ok(#(
        P(..p6, last_expr_assignable: False, last_expr_is_assignment: False),
        ast.ConditionalExpression(
          condition: test_expr,
          consequent:,
          alternate:,
          span: ast.Span(ast.expression_span(test_expr).start, p6.prev_end),
        ),
      ))
    }
    _ -> Ok(#(p2, test_expr))
  }
}

/// Pratt parser for binary expressions.
/// `min_prec` is the minimum precedence to continue parsing.
fn parse_binary_expression(
  p: P,
  min_prec: Int,
) -> Result(#(P, ast.Expression), ParseError) {
  use #(p2, left) <- result.try(parse_unary_expression(p))
  parse_binary_rhs(p2, left, min_prec)
}

fn parse_binary_rhs(
  p: P,
  left: ast.Expression,
  min_prec: Int,
) -> Result(#(P, ast.Expression), ParseError) {
  let tok = peek(p)
  case binary_operator(tok, p.ctx.allow_in) {
    // Not a binary/logical operator (or `in` while `allow_in` is off).
    None -> Ok(#(p, left))
    Some(BinaryOperator(precedence:, op:)) ->
      case precedence > min_prec {
        False -> Ok(#(p, left))
        True -> {
          let op_pos = pos_of(p)
          let p2 = advance(p)
          // Right-associative for **
          let next_min = case tok {
            StarStar -> precedence - 1
            _ -> precedence
          }
          use #(p3, right) <- result.try(parse_binary_expression(p2, next_min))
          let span = ast.Span(ast.expression_span(left).start, p3.prev_end)
          // `BinOrLogical` splits `Coalesce` from `ShortCircuit` precisely so
          // this `case op` cannot compile without an arm for each — the
          // §13.13.1 mixing check below can never be silently dropped.
          use expr <- result.try(case op {
            Binary(op) ->
              Ok(ast.BinaryExpression(operator: op, left:, right:, span:))
            ShortCircuit(op) ->
              // §13.13.1: `||`/`&&` may not have an unparenthesized `??` on
              // the left. (Right is impossible: `??` binds no tighter than
              // `||`, so the rhs parse at `next_min ≥ 1` never consumes it.)
              case left {
                ast.LogicalExpression(operator: ast.NullishCoalescing, ..) ->
                  Error(CoalesceMixedWithLogical(op_pos))
                _ ->
                  Ok(ast.LogicalExpression(operator: op, left:, right:, span:))
              }
            Coalesce ->
              // §13.13.1: `??` may not have an unparenthesized `||`/`&&` on
              // either side. `&&` binds tighter, so it CAN reach `right`.
              case left, right {
                ast.LogicalExpression(operator: ast.LogicalOr, ..), _
                | ast.LogicalExpression(operator: ast.LogicalAnd, ..), _
                | _, ast.LogicalExpression(operator: ast.LogicalOr, ..)
                | _, ast.LogicalExpression(operator: ast.LogicalAnd, ..)
                -> Error(CoalesceMixedWithLogical(op_pos))
                _, _ ->
                  Ok(ast.LogicalExpression(
                    operator: ast.NullishCoalescing,
                    left:,
                    right:,
                    span:,
                  ))
              }
          })
          parse_binary_rhs(P(..p3, last_expr_assignable: False), expr, min_prec)
        }
      }
  }
}

fn parse_unary_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  // Every prefix-operator branch starts at the current token, so capture the
  // span start once. Branches that fall through to parse_postfix_expression
  // simply ignore it.
  let start = pos_of(p)
  let unary = fn(p2, op) {
    use #(p3, arg) <- result.try(parse_unary_expression(p2))
    Ok(#(
      P(..p3, last_expr_assignable: False, last_expr_is_assignment: False),
      ast.UnaryExpression(
        operator: op,
        argument: arg,
        span: span_from(start, p3),
      ),
    ))
  }
  case peek(p) {
    Delete -> {
      let p2 = advance(p)
      use #(p3, expr) <- result.try(unary(p2, ast.Delete))
      let operand = delete_operand(expr)
      // §13.5.1.1 early errors — check the parsed AST, not tokens:
      //   - `delete x` (bare identifier, through parens) → strict-mode error
      //   - `delete expr.#priv` (private name, through parens) → strict-mode error
      //     (private names only occur in class bodies, which are always strict)
      use <- bool.guard(
        p.ctx.strict && is_bare_identifier(operand),
        Error(DeleteUnqualifiedStrictMode(start)),
      )
      use <- bool.guard(
        is_private_name_access(operand),
        Error(DeletePrivateName(start)),
      )
      Ok(#(p3, expr))
    }
    Bang -> unary(advance(p), ast.LogicalNot)
    Tilde -> unary(advance(p), ast.BitwiseNot)
    Typeof -> unary(advance(p), ast.TypeOf)
    Void -> unary(advance(p), ast.Void)
    Minus -> unary(advance(p), ast.Negate)
    Plus -> unary(advance(p), ast.UnaryPlus)
    PlusPlus | MinusMinus -> {
      let op = case peek(p) {
        PlusPlus -> ast.Increment
        _ -> ast.Decrement
      }
      let p2 = advance(p)
      use #(p3, arg) <- result.try(parse_unary_expression(p2))
      finish_update_expr(p3, arg, op, True, start, start)
    }
    Await ->
      case p.ctx.in_async || p.mode == Module {
        True -> {
          // §15.7.1: ContainsAwait of ClassStaticBlockBody must be false.
          use <- bool.guard(
            p.ctx.in_static_block,
            Error(AwaitInStaticBlock(start)),
          )
          let p2 = advance(p)
          use #(p3, arg) <- result.try(parse_unary_expression(p2))
          Ok(#(
            P(..p3, last_expr_assignable: False, last_expr_is_assignment: False),
            ast.AwaitExpression(argument: arg, span: span_from(start, p3)),
          ))
        }
        // Outside async functions and modules `await` is not a reserved word
        // — it parses as an ordinary IdentifierReference (§13.1).
        False -> parse_postfix_expression(p)
      }
    _ -> parse_postfix_expression(p)
  }
}

/// Unwrap the UnaryExpression(Delete, ...) to get at the operand for
/// §13.5.1.1 early-error checks.
fn delete_operand(expr: ast.Expression) -> ast.Expression {
  case expr {
    ast.UnaryExpression(operator: ast.Delete, argument:, ..) ->
      ast_util.unwrap_parens(argument)
    _ -> expr
  }
}

fn is_bare_identifier(expr: ast.Expression) -> Bool {
  case expr {
    ast.Identifier(..) -> True
    _ -> False
  }
}

/// `expr.#priv` or `expr?.#priv` — private names lex as Identifier tokens
/// with a "#" prefix, so check the property name's first char.
fn is_private_name_access(expr: ast.Expression) -> Bool {
  case expr {
    ast.MemberExpression(property: ast.Dot(name:, ..), ..)
    | ast.OptionalMemberExpression(property: ast.Dot(name:, ..), ..) ->
      string.starts_with(name, "#")
    _ -> False
  }
}

/// Record a `#name` reference (member access `o.#x` / `o?.#x`, or the bare
/// `#x in o` form) for the §15.7.1 AllPrivateIdentifiersValid early error.
/// No-op for ordinary names.
fn note_private_ref(p: P, name: String) -> P {
  case name {
    "#" <> _ ->
      P(..p, private_refs: [
        #(name, p.class_private_depth, pos_of(p)),
        ..p.private_refs
      ])
    _ -> p
  }
}

/// `super.#x` is an unconditional early SyntaxError - private names are
/// never valid on super references (§13.3).
fn check_super_private(
  p: P,
  object: ast.Expression,
  name: String,
) -> Result(Nil, ParseError) {
  case object, name {
    ast.SuperExpression(..), "#" <> _ -> Error(SuperPrivateName(pos_of(p)))
    _, _ -> Ok(Nil)
  }
}

/// Object literal / pattern property keys can never be private names:
/// `{ #x: 1 }` and `const { #x: y } = o` are SyntaxErrors.
fn reject_private_property_key(
  p: P,
  key: ast.PropertyKey,
) -> Result(Nil, ParseError) {
  case key {
    ast.KeyPrivate(..) -> Error(PrivateNameAsPropertyKey(pos_of(p)))
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyComputed(..) -> Ok(Nil)
  }
}

/// End-of-class-body step of AllPrivateIdentifiersValid: drop refs made
/// inside this class body that this class declares, demote the rest to the
/// outer depth (an enclosing class body may still declare them), and reject
/// everything once there is no enclosing class body left.
fn resolve_private_refs(
  p: P,
  outer_depth: Int,
  declared: Dict(String, #(Bool, PrivateNameKind)),
) -> Result(P, ParseError) {
  let my_depth = outer_depth + 1
  let remaining =
    list.filter_map(p.private_refs, fn(ref) {
      let #(name, depth, pos) = ref
      case depth >= my_depth {
        True ->
          case dict.has_key(declared, name) {
            True -> Error(Nil)
            False -> Ok(#(name, outer_depth, pos))
          }
        False -> Ok(ref)
      }
    })
  case outer_depth {
    // No enclosing class body left: only the caller's PrivateEnvironment
    // (direct eval, §19.2.1.1 step 5) can still legitimize a reference.
    0 ->
      case unresolved_outside_eval_env(p, remaining) {
        [#(name, _, pos), ..] -> Error(UndeclaredPrivateName(pos, name))
        [] -> Ok(P(..p, private_refs: []))
      }
    _ -> Ok(P(..p, private_refs: remaining))
  }
}

/// Top-level AllPrivateIdentifiersValid: any reference that survived every
/// class boundary is a SyntaxError — unless it names a private name from the
/// caller's PrivateEnvironment (direct eval only, see outer_private_names).
fn check_unresolved_private_refs(p: P) -> Result(Nil, ParseError) {
  case unresolved_outside_eval_env(p, p.private_refs) {
    [] -> Ok(Nil)
    [#(name, _, pos), ..] -> Error(UndeclaredPrivateName(pos, name))
  }
}

/// Refs not covered by the direct-eval caller's private environment.
fn unresolved_outside_eval_env(
  p: P,
  refs: List(#(String, Int, Int)),
) -> List(#(String, Int, Int)) {
  case p.outer_private_names {
    [] -> refs
    outer -> list.filter(refs, fn(ref) { !list.contains(outer, ref.0) })
  }
}

fn parse_postfix_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  use #(p2, expr) <- result.try(parse_left_hand_side_expression(p))
  case peek(p2) {
    PlusPlus | MinusMinus -> {
      use <- bool.guard(has_line_break_before(p2), Ok(#(p2, expr)))
      let op = case peek(p2) {
        PlusPlus -> ast.Increment
        _ -> ast.Decrement
      }
      let err_pos = pos_of(p2)
      finish_update_expr(advance(p2), expr, op, False, expr.span.start, err_pos)
    }
    _ -> Ok(#(p2, expr))
  }
}

/// Validate the operand of `++`/`--` and build the UpdateExpression. `p` is
/// the final parser state — `advance` preserves last_expr_*, so postfix
/// callers advance past the operator before calling.
fn finish_update_expr(
  p: P,
  arg: ast.Expression,
  op: ast.UpdateOp,
  prefix: Bool,
  span_start: Int,
  err_pos: Int,
) -> Result(#(P, ast.Expression), ParseError) {
  case p.last_expr_assignable || is_web_compat_call_target(p, arg), prefix {
    False, True -> Error(InvalidLhsPrefixOp(err_pos))
    False, False -> Error(InvalidPostfixLhs(err_pos))
    True, _ ->
      case p.ctx.strict, p.last_expr_name {
        True, Some("eval" as n) | True, Some("arguments" as n) ->
          Error(StrictModeModification(err_pos, n))
        _, _ ->
          Ok(#(
            P(
              ..p,
              last_expr_assignable: False,
              sb: sb_mark_assign_targets(p.sb, arg),
            ),
            ast.UpdateExpression(
              operator: op,
              prefix:,
              argument: arg,
              span: span_from(span_start, p),
            ),
          ))
      }
  }
}

fn parse_left_hand_side_expression(
  p: P,
) -> Result(#(P, ast.Expression), ParseError) {
  case peek(p) {
    New -> parse_new_expression(p)
    _ -> parse_call_expression(p)
  }
}

fn parse_new_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let p2 = advance(p)
  case peek(p2) {
    Dot -> {
      // new.target (MetaProperty)
      let p3 = advance(p2)
      case peek(p3) {
        Identifier -> {
          case peek_value(p3), peek_raw_len(p3) != 6 {
            // new.target must be spelled literally — unicode escapes forbidden
            "target", True -> Error(UnicodeEscapeInMetaProperty(pos_of(p3)))
            "target", False ->
              case p.ctx.allow_new_target {
                True -> {
                  // Span covers `new.target` — from the `new` keyword's
                  // start to just past the `target` identifier.
                  let p4 = advance(p3)
                  let p4 =
                    P(
                      ..p4,
                      sb: scope.sb_lexical_ref(p4.sb, lexical.RefNewTarget),
                    )
                  let meta =
                    ast.MetaProperty(
                      kind: ast.NewTarget,
                      span: span_from(pos_of(p), p4),
                    )
                  parse_call_chain(p4, meta)
                }
                False -> Error(NewTargetOutsideFunction(pos_of(p)))
              }
            other, _ -> Error(ExpectedNewTarget(pos_of(p3), Some(other)))
          }
        }
        _ -> Error(ExpectedNewTarget(pos_of(p3), None))
      }
    }
    New -> {
      // new new Foo()
      use #(p3, inner) <- result.try(parse_new_expression(p2))
      parse_call_chain(p3, inner)
    }
    _ -> {
      // NewExpression span: `new` keyword start → end of arguments (`)`) or,
      // for the no-paren form, end of the callee/template chain.
      let start = pos_of(p)
      use #(p3, callee_base) <- result.try(parse_primary_expression(p2))
      let #(p4, callee) = parse_member_chain(p3, callee_base)
      case peek(p4) {
        // Tagged template binds tighter than `new` (§13.3 MemberExpression :
        // MemberExpression TemplateLiteral): `new tag`tpl`` constructs the
        // RESULT of the tagged-template call, with optional arguments after.
        TemplateLiteral | TemplateHead -> {
          use #(p5, tagged) <- result.try(parse_member_templates(p4, callee))
          finish_new(p5, start, tagged)
        }
        _ -> finish_new(p4, start, callee)
      }
    }
  }
}

/// Build the NewExpression once the callee is fully parsed. Handles both
/// `new Foo(args)` (continues into a call chain — member access may make it
/// assignable again) and bare `new Foo`. Result itself is never assignable.
fn finish_new(
  p: P,
  start: Int,
  callee: ast.Expression,
) -> Result(#(P, ast.Expression), ParseError) {
  case peek(p) {
    LeftParen -> {
      use #(p2, args) <- result.try(parse_arguments(p))
      let new_expr =
        ast.NewExpression(callee:, arguments: args, span: span_from(start, p2))
      parse_call_chain(P(..p2, last_expr_assignable: False), new_expr)
    }
    _ ->
      Ok(#(
        P(..p, last_expr_assignable: False),
        ast.NewExpression(callee:, arguments: [], span: span_from(start, p)),
      ))
  }
}

fn parse_call_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let parsed = case peek(p) {
    Super -> {
      let super_span = span_of(p)
      let p2 = advance(p)
      case peek(p2) {
        LeftParen ->
          case p.ctx.allow_super_call {
            True -> {
              use #(p3, args) <- result.try(parse_arguments(p2))
              // §13.3.7.1 SuperCall: reads the active function (for
              // [[GetPrototypeOf]]) and new.target, then binds `this`.
              // emit.gleam: get_lexical(RefActiveFunc) +
              // get_lexical(RefNewTarget) + set_this. The follow-up
              // emit_field_init_call reads `<class_fields_init>` from
              // the class scope, so record that as a name ref so an
              // arrow `(() => super())()` inside a derived ctor
              // captures the const.
              let sb =
                p3.sb
                |> scope.sb_lexical_ref(lexical.RefActiveFunc)
                |> scope.sb_lexical_ref(lexical.RefNewTarget)
                |> scope.sb_lexical_ref(lexical.RefThis)
                |> scope.sb_ref(ast_util.class_fields_init)
              Ok(#(
                P(..p3, sb:),
                ast.CallExpression(
                  callee: ast.SuperExpression(span: super_span),
                  arguments: args,
                  span: span_from(super_span.start, p3),
                ),
              ))
            }
            False -> Error(SuperCallNotInDerivedConstructor(pos_of(p)))
          }
        Dot | LeftBracket ->
          case p.ctx.allow_super_property {
            True -> {
              // §13.3.7.3 MakeSuperPropertyReference — see the
              // parse_primary_expression Super arm.
              let sb =
                p2.sb
                |> scope.sb_lexical_ref(lexical.RefHomeObject)
                |> scope.sb_lexical_ref(lexical.RefThis)
              Ok(#(P(..p2, sb:), ast.SuperExpression(span: super_span)))
            }
            False -> Error(SuperPropertyNotInMethod(pos_of(p)))
          }
        _ -> Error(UnexpectedSuper(pos_of(p)))
      }
    }
    Import -> {
      let import_start = pos_of(p)
      let p2 = advance(p)
      case peek(p2) {
        LeftParen -> {
          // ImportCall arguments are AssignmentExpression[+In] — re-enable
          // `in` even inside a for-statement init (restored after `)`).
          use p3 <- with_allow_in(advance(p2), True)
          use #(p4, source_expr) <- result.try(parse_assignment_expression(p3))
          // Optional second argument (import attributes)
          use #(p5, options) <- result.try(case peek(p4) {
            Comma ->
              case peek_at(p4, 1) {
                RightParen -> Ok(#(advance(p4), None))
                _ -> {
                  use #(p_attrs, attrs) <- result.map(
                    parse_assignment_expression(advance(p4)),
                  )
                  // Trailing comma after the options argument:
                  // import(x, opts,) — §13.3.10 ImportCall grammar allows it.
                  let p_attrs = case peek(p_attrs) {
                    Comma ->
                      case peek_at(p_attrs, 1) {
                        RightParen -> advance(p_attrs)
                        _ -> p_attrs
                      }
                    _ -> p_attrs
                  }
                  #(p_attrs, Some(attrs))
                }
              }
            _ -> Ok(#(p4, None))
          })
          use p6 <- result.map(expect(p5, RightParen))
          #(
            p6,
            ast.ImportExpression(
              source: source_expr,
              options:,
              phase: ast.PhaseEvaluation,
              span: span_from(import_start, p6),
            ),
          )
        }
        Dot -> {
          // import.meta / import.source(...) / import.defer(...)
          let p3 = advance(p2)
          // `meta`, `source` and `defer` are terminal symbols (§5.1.5) — they
          // must appear literally, so a \u escape in them is a SyntaxError.
          case peek(p3), peek_value(p3), peek_had_escape(p3) {
            Identifier, "meta", False -> {
              // Span covers `import.meta` — from the `import` keyword's
              // start to just past the `meta` identifier.
              let p4 = advance(p3)
              Ok(#(
                p4,
                ast.MetaProperty(
                  kind: ast.ImportMeta,
                  span: span_from(pos_of(p), p4),
                ),
              ))
            }
            // §13.3.10 ImportCall: import . source ( AssignmentExpression ,opt )
            // — only valid as a call; bare `import.source` is a SyntaxError.
            Identifier, "source", False ->
              case peek_at(p3, 1) {
                LeftParen ->
                  parse_phase_import_call(p3, import_start, ast.PhaseSource)
                _ -> Error(ExpectedImportMeta(pos_of(p3), Some("source")))
              }
            // import . defer ( AssignmentExpression ,opt )
            Identifier, "defer", False ->
              case peek_at(p3, 1) {
                LeftParen ->
                  parse_phase_import_call(p3, import_start, ast.PhaseDefer)
                _ -> Error(ExpectedImportMeta(pos_of(p3), Some("defer")))
              }
            Identifier, other, _ ->
              Error(ExpectedImportMeta(pos_of(p3), Some(other)))
            _, _, _ -> Error(ExpectedImportMeta(pos_of(p3), None))
          }
        }
        _ -> Error(ExpectedCallOrDotAfterImport(pos_of(p2)))
      }
    }
    _ -> parse_primary_expression(p)
  }
  use #(p2, expr) <- result.try(parsed)
  parse_call_chain(p2, expr)
}

/// §13.3.10 ImportCall phase forms: `import.source(expr)` / `import.defer(expr)`.
/// `p` is positioned at the `source`/`defer` identifier with `(` next;
/// `import_start` is the byte offset of the leading `import` keyword (for the
/// resulting ImportExpression's span). Exactly one AssignmentExpression
/// argument, optional trailing comma, no options.
fn parse_phase_import_call(
  p: P,
  import_start: Int,
  phase: ast.ImportPhase,
) -> Result(#(P, ast.Expression), ParseError) {
  // Skip the phase identifier and the '('. The argument is
  // AssignmentExpression[+In] — re-enable `in` (restored after `)`).
  use p2 <- with_allow_in(advance(advance(p)), True)
  use #(p3, source_expr) <- result.try(parse_assignment_expression(p2))
  // Optional trailing comma: import.source(x,)
  let p4 = case peek(p3) {
    Comma ->
      case peek_at(p3, 1) {
        RightParen -> advance(p3)
        _ -> p3
      }
    _ -> p3
  }
  use p5 <- result.map(expect(p4, RightParen))
  #(
    p5,
    ast.ImportExpression(
      source: source_expr,
      options: None,
      phase:,
      span: span_from(import_start, p5),
    ),
  )
}

fn parse_call_chain(
  p: P,
  callee: ast.Expression,
) -> Result(#(P, ast.Expression), ParseError) {
  // Left-recursive suffix production: every wrapped node spans from the
  // ORIGINAL base's start (`callee.span.start`, the universal accessor) to
  // just past the suffix this iteration consumes (`)`, prop ident, `]`,
  // template). Each recursion passes the wrapped node back as the new
  // `callee`, so `start` walks along unchanged.
  let start = callee.span.start
  case peek(p) {
    LeftParen -> {
      // Call expression — not a valid assignment target
      use #(p2, args) <- result.try(parse_arguments(p))
      // §19.2.1 direct eval: a CallExpression whose callee is the
      // identifier `eval` (parens transparent). Mark the SCOPE so
      // `analyze_captures` propagates eval-poisoning to ancestors.
      let p2 = case ast_util.unwrap_parens(callee) {
        ast.Identifier(name: "eval", ..) ->
          P(..p2, sb: scope.sb_mark_eval(p2.sb))
        _ -> p2
      }
      let expr =
        ast.CallExpression(
          callee: callee,
          arguments: args,
          span: span_from(start, p2),
        )
      parse_call_chain(P(..p2, last_expr_assignable: False), expr)
    }
    Dot | LeftBracket -> {
      use #(p2, expr) <- result.try(parse_member_suffix(p, callee, start))
      parse_call_chain(p2, expr)
    }
    QuestionDot ->
      // Optional chaining — NEVER a valid assignment target. `?.(args)` is
      // an optional CALL; `?.x` / `?.[e]` are plain member suffixes.
      case peek_at(p, 1) {
        LeftParen -> {
          let p2 = advance(p)
          use #(p3, args) <- result.try(parse_arguments(p2))
          let expr =
            ast.OptionalCallExpression(
              callee: callee,
              arguments: args,
              span: span_from(start, p3),
            )
          parse_call_chain(P(..p3, last_expr_assignable: False), expr)
        }
        _ -> {
          use #(p2, expr) <- result.try(parse_member_suffix(p, callee, start))
          parse_call_chain(p2, expr)
        }
      }
    TemplateLiteral | TemplateHead -> {
      use #(p2, expr) <- result.try(parse_tagged_template(p, callee))
      parse_call_chain(p2, expr)
    }
    _ -> Ok(#(p, callee))
  }
}

/// Parse ONE member-access suffix (`.x`, `[e]`, `?.x`, `?.[e]`) at `p` and
/// build the (Optional)MemberExpression. Does NOT handle calls (`(args)`,
/// `?.(args)`) or templates — callers dispatch those. Shared single-step
/// dispatch for parse_call_chain (propagates errors) and parse_member_chain
/// (backtracks on error).
fn parse_member_suffix(
  p: P,
  object: ast.Expression,
  start: Int,
) -> Result(#(P, ast.Expression), ParseError) {
  case peek(p) {
    Dot -> {
      let p2 = advance(p)
      case is_identifier_or_keyword(peek(p2)) {
        True -> {
          let prop_name = peek_value(p2)
          use Nil <- result.try(check_super_private(p2, object, prop_name))
          Ok(finish_dot_member(p2, object, start, prop_name, False))
        }
        False ->
          Error(error_at_current(p2, ExpectedIdentifierAfterDot(pos_of(p2))))
      }
    }
    LeftBracket -> parse_bracket_member(p, object, start, False)
    QuestionDot -> {
      let p2 = advance(p)
      case peek(p2) {
        LeftBracket -> parse_bracket_member(p2, object, start, True)
        _ ->
          case is_identifier_or_keyword(peek(p2)) {
            True ->
              Ok(finish_dot_member(p2, object, start, peek_value(p2), True))
            False ->
              Error(error_at_current(p2, ExpectedAfterOptionalChain(pos_of(p2))))
          }
      }
    }
    // Unreachable — callers only dispatch on Dot / LeftBracket / QuestionDot.
    _ -> Ok(#(p, object))
  }
}

/// Parse `[ Expression ]` for a computed member access and build the
/// resulting (Optional)MemberExpression. `p` is at `[`; returns parser past
/// `]` with `last_expr_assignable` set (True for plain, False for optional).
/// Shared by the LeftBracket arms of parse_call_chain and parse_member_chain.
fn parse_bracket_member(
  p: P,
  object: ast.Expression,
  start: Int,
  optional: Bool,
) -> Result(#(P, ast.Expression), ParseError) {
  use p2 <- with_allow_in(advance(p), True)
  use #(p3, expression) <- result.try(parse_expression(p2))
  use p4 <- result.map(expect(p3, RightBracket))
  let span = span_from(start, p4)
  let property = ast.Bracket(expression:)
  case optional {
    False -> #(
      P(..p4, last_expr_assignable: True),
      ast.MemberExpression(object:, property:, span:),
    )
    True -> #(
      P(..p4, last_expr_assignable: False),
      ast.OptionalMemberExpression(object:, property:, span:),
    )
  }
}

/// Build the `(Optional)MemberExpression` for a `.prop` / `?.prop` access.
/// `p` is at the property identifier (already peeked as `prop_name`); returns
/// parser past it with `last_expr_assignable` set. Shared by the Dot /
/// QuestionDot-identifier arms of parse_call_chain and parse_member_chain.
fn finish_dot_member(
  p: P,
  object: ast.Expression,
  start: Int,
  prop_name: String,
  optional: Bool,
) -> #(P, ast.Expression) {
  let p = note_private_ref(p, prop_name)
  // Private member access `obj.#x` / `obj?.#x` is lowered by the emitter to
  // a var-get of the class-scope `#x` const, so record it as an unresolved
  // ref just like a bare identifier (the legacy REFERENCE pass did this in
  // ref_expr's MemberExpression arm). Ordinary property names are NOT refs.
  let p = case prop_name {
    "#" <> _ -> P(..p, sb: scope.sb_ref(p.sb, prop_name))
    _ -> p
  }
  let property = ast.Dot(name: prop_name, span: span_of(p))
  let p2 = advance(p)
  let span = span_from(start, p2)
  case optional {
    False -> #(
      P(..p2, last_expr_assignable: True),
      ast.MemberExpression(object:, property:, span:),
    )
    True -> #(
      P(..p2, last_expr_assignable: False),
      ast.OptionalMemberExpression(object:, property:, span:),
    )
  }
}

/// Parse a tagged-template suffix `tag\`...\`` at the TemplateLiteral token.
/// `p` is at the template; returns parser past it (last_expr_assignable
/// cleared — tagged templates are never assignment targets) and the wrapped
/// TaggedTemplateExpression spanning from the tag's start. Shared by
/// parse_call_chain and parse_member_templates.
fn parse_tagged_template(
  p: P,
  tag: ast.Expression,
) -> Result(#(P, ast.Expression), ParseError) {
  use #(p2, raw_parts) <- result.map(parse_template_spans(p))
  // §12.9.6: an invalid escape in a TAGGED template's quasi is legal — its
  // cooked value is undefined (None); the raw text is always available.
  let parts =
    ast.map_template_quasis(raw_parts, fn(q) {
      case cook_template_string(q) {
        Ok(s) -> ast.TemplateQuasi(cooked: Some(s), raw: q)
        Error(Nil) -> ast.TemplateQuasi(cooked: None, raw: q)
      }
    })
  let expr =
    ast.TaggedTemplateExpression(
      tag:,
      parts:,
      span: span_from(tag.span.start, p2),
    )
  #(P(..p2, last_expr_assignable: False), expr)
}

/// Consume a run of `MemberExpression TemplateLiteral` productions (tagged
/// templates inside a `new` callee), interleaved with member accesses:
/// `new tag`a`.b`c`(...)`. Returns the expression and the parser positioned
/// at the (optional) constructor Arguments.
fn parse_member_templates(
  p: P,
  callee: ast.Expression,
) -> Result(#(P, ast.Expression), ParseError) {
  case peek(p) {
    TemplateLiteral | TemplateHead -> {
      use #(p2, expr) <- result.try(parse_tagged_template(p, callee))
      let #(p3, expr) = parse_member_chain(p2, expr)
      parse_member_templates(p3, expr)
    }
    _ -> Ok(#(p, callee))
  }
}

fn parse_member_chain(p: P, object: ast.Expression) -> #(P, ast.Expression) {
  // Same left-recursive span pattern as parse_call_chain, but TOTAL: stops
  // at `(` / template (so `new Foo(args)` keeps the args for NewExpression)
  // and backtracks on any suffix parse error — the outer call chain will
  // re-encounter and report it. QuestionDot is NOT dispatched: §13.3 —
  // a NewExpression callee is a MemberExpression, never an OptionalChain,
  // so `new a?.b` must surface as a SyntaxError downstream.
  case peek(p) {
    Dot | LeftBracket ->
      case parse_member_suffix(p, object, object.span.start) {
        Ok(#(p2, expr)) -> parse_member_chain(p2, expr)
        Error(_) -> #(p, object)
      }
    _ -> #(p, object)
  }
}

fn parse_arguments(p: P) -> Result(#(P, List(ast.Expression)), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  parse_comma_list(
    p2,
    [],
    RightParen,
    parse_argument,
    ExpectedCommaOrCloseParen,
  )
}

fn parse_argument(p: P) -> Result(#(P, ast.Expression), ParseError) {
  // Arguments are AssignmentExpression[+In]; with_allow_in puts the caller's
  // (possibly for-head) [In] back afterwards.
  use p <- with_allow_in(p, True)
  case peek(p) {
    DotDotDot -> {
      let start = pos_of(p)
      use #(p2, arg_expr) <- result.map(parse_assignment_expression(advance(p)))
      #(p2, ast.SpreadElement(argument: arg_expr, span: span_from(start, p2)))
    }
    _ -> parse_assignment_expression(p)
  }
}

fn parse_primary_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  // Default to no name — only the Identifier branch sets Some(name).
  let p = P(..p, last_expr_name: None)
  case peek(p) {
    // A hard lexer error, materialised as a zero-length LexFailure token
    // (lexing is on demand — see ensure_current): report ITS message.
    Illegal | LexFailure(_) -> Error(illegal_token_error(p))
    Identifier -> {
      let val = peek_value(p)
      // §13.1.1 IdentifierReference early errors: escaped always-reserved
      // words (the lexer only emits an Identifier token spelling a reserved
      // word when the source contained a \u escape), strict-mode future
      // reserved words, contextual yield/await, and `arguments` in class
      // static blocks / field initializers. Property names and member
      // accesses use other paths, so they still allow escaped reserved words.
      use Nil <- result.try(check_identifier_reference(p, val))
      let p = note_private_ref(p, val)
      // V8 VariableProxy / Scope::AddUnresolved — record the bare ref
      // against the current scope for later resolution in `finalize`.
      // A bare private name `#x` only reaches this arm in the `#x in obj`
      // brand-check form; `obj.#x` is recorded in finish_dot_member.
      let p = P(..p, sb: scope.sb_ref(p.sb, val))
      Ok(#(
        P(..advance(p), last_expr_assignable: True, last_expr_name: Some(val)),
        ast.Identifier(name: val, span: span_of(p)),
      ))
    }
    Number -> {
      use <- bool.guard(
        p.ctx.strict && peek_annex_b_legacy(p),
        Error(OctalLiteralStrictMode(pos_of(p))),
      )
      use lit <- result.map(numeric_literal(p))
      #(P(..advance(p), last_expr_assignable: False), lit)
    }
    KString -> {
      use value <- result.map(string_literal_value(p))
      #(
        P(..advance(p), last_expr_assignable: False),
        ast.StringExpression(value:, span: span_of(p)),
      )
    }
    KTrue -> ok_lit(p, ast.BooleanLiteral(value: True, span: span_of(p)))
    KFalse -> ok_lit(p, ast.BooleanLiteral(value: False, span: span_of(p)))
    Null -> ok_lit(p, ast.NullLiteral(span: span_of(p)))
    Undefined -> ok_lit(p, ast.UndefinedExpression(span: span_of(p)))
    TemplateLiteral | TemplateHead -> {
      let start = pos_of(p)
      use #(p, raw_parts) <- result.try(parse_template_spans(p))
      // §12.9.6: an undefined TV (invalid escape) is only legal in TAGGED
      // templates; in a plain template literal it is a SyntaxError.
      use parts <- result.map(
        ast.try_map_template_quasis(raw_parts, fn(q) {
          case cook_template_string(q) {
            Ok(s) -> Ok(s)
            Error(Nil) -> Error(InvalidTemplateEscape(start))
          }
        }),
      )
      #(
        P(..p, last_expr_assignable: False),
        ast.TemplateLiteral(parts:, span: ast.Span(start:, end: p.prev_end)),
      )
    }
    This ->
      ok_lit(
        P(..p, sb: scope.sb_lexical_ref(p.sb, lexical.RefThis)),
        ast.ThisExpression(span: span_of(p)),
      )
    Super -> {
      // super.x and super[x] can appear in parenthesized contexts
      // like new (super.x) or arrow body () => (super.c)
      // Note: super() call is only handled in parse_call_expression
      let next = peek_at(p, 1)
      case next {
        Dot | LeftBracket ->
          case p.ctx.allow_super_property {
            True -> {
              // §13.3.7.3 MakeSuperPropertyReference reads [[HomeObject]]
              // for the prototype lookup and `this` as the receiver
              // (emit.gleam: get_lexical(RefHomeObject) + get_this).
              let sb =
                p.sb
                |> scope.sb_lexical_ref(lexical.RefHomeObject)
                |> scope.sb_lexical_ref(lexical.RefThis)
              Ok(#(P(..advance(p), sb:), ast.SuperExpression(span: span_of(p))))
            }
            False -> Error(UnexpectedSuper(pos_of(p)))
          }
        _ -> Error(UnexpectedSuper(pos_of(p)))
      }
    }
    LeftParen -> {
      let start = pos_of(p)
      let p2 = advance(p)
      case peek(p2) {
        RightParen -> {
          // Empty parens — only valid as arrow function params.
          // If we got here, try_arrow_function already failed to find =>,
          // so this is an error.
          Error(UnexpectedCloseParen(pos_of(p)))
        }
        _ -> {
          // A parenthesized Expression is [+In] regardless of the enclosing
          // for-head; with_allow_in puts the caller's [In] back on the way out.
          use p2 <- with_allow_in(p2, True)
          use #(p3, expr) <- result.try(parse_expression(p2))
          use p4 <- result.map(expect(p3, RightParen))
          // Preserve parenthesization in the AST so the compiler can
          // distinguish `x` from `(x)` for IsIdentifierRef (§13.15.2).
          #(
            p4,
            ast.ParenthesizedExpression(
              expression: expr,
              span: span_from(start, p4),
            ),
          )
        }
      }
    }
    LeftBracket -> parse_array_literal(p) |> set_not_assignable
    LeftBrace -> parse_object_literal(p) |> set_not_assignable
    Function -> parse_function_expression(p, is_async: False)
    Class -> parse_class_expression(p)
    Async ->
      case peek_at(p, 1) {
        Function -> parse_function_expression(p, is_async: True)
        _ -> contextual_ident_ok(p)
      }
    Slash -> {
      // Could be a regex literal — but the lexer tokenized it as Slash.
      // For now, try to consume as regex. We'll improve this later.
      parse_regex_literal(p)
    }
    SlashEqual -> {
      // /= at the start of an expression is a regex starting with =
      parse_regex_literal(p)
    }
    New -> parse_new_expression(p)
    _ ->
      case is_contextual_keyword(peek(p)) {
        True ->
          // Check for contextual keywords that are restricted in certain contexts
          case peek(p) {
            Yield ->
              case p.ctx.strict {
                True -> Error(YieldReservedStrictMode(pos_of(p)))
                False ->
                  case p.ctx.in_generator {
                    True -> Error(YieldInGenerator(pos_of(p)))
                    False -> contextual_ident_ok(p)
                  }
              }
            Await ->
              case p.mode {
                Module -> Error(AwaitInModule(pos_of(p)))
                Script ->
                  case p.ctx.in_async {
                    True -> Error(AwaitInAsyncFunction(pos_of(p)))
                    False -> contextual_ident_ok(p)
                  }
              }
            Let ->
              case p.ctx.strict {
                True -> Error(LetIdentifierStrictMode(pos_of(p)))
                False -> contextual_ident_ok(p)
              }
            Static ->
              case p.ctx.strict {
                True -> Error(StaticReservedStrictMode(pos_of(p)))
                False -> contextual_ident_ok(p)
              }
            _ -> contextual_ident_ok(p)
          }
        False -> Error(UnexpectedToken(pos_of(p), peek(p)))
      }
  }
}

/// Success path for a contextual keyword (or `async`) used as a plain
/// IdentifierReference in primary-expression position.
fn contextual_ident_ok(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let name = peek_value(p)
  Ok(#(
    P(..advance(p), last_expr_assignable: True, sb: scope.sb_ref(p.sb, name)),
    ast.Identifier(name:, span: span_of(p)),
  ))
}

fn parse_array_literal(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let start = pos_of(p)
  let p2 = advance(p)
  // An array literal's elements are [+In] even inside a for-head.
  use p2 <- with_allow_in(p2, True)
  // Cover-grammar pattern-validity flags are scoped per literal: reset so
  // a previous `[4]` or `{m(){}}` doesn't poison this literal's check.
  // parse_array_elements then accumulates per-element validity fresh.
  let p2 = P(..p2, has_invalid_pattern: False)
  use #(p3, elems) <- result.map(parse_array_elements(p2, []))
  #(
    p3,
    ast.ArrayExpression(
      elements: list.reverse(elems),
      span: span_from(start, p3),
    ),
  )
}

/// Cover-grammar: does a just-parsed array/object element make the enclosing
/// literal invalid as a destructuring pattern?
/// - simple LHS target (ident/member), or `target = default` when
///   `allow_default` → valid (ignore any inner has_invalid_pattern,
///   e.g. `{m(){}}.y`)
/// - nested `[...]`/`{...}` (by `start_token`) → propagate the nested flag
/// - anything else (literal, call, etc.) → invalid
fn cover_elem_invalid(
  p: P,
  start_token: TokenKind,
  allow_default: Bool,
) -> Bool {
  case
    p.last_expr_assignable || { allow_default && p.last_expr_is_assignment }
  {
    True -> False
    False ->
      case start_token {
        LeftBrace | LeftBracket -> p.has_invalid_pattern
        _ -> True
      }
  }
}

fn parse_array_elements(
  p: P,
  acc: List(Option(ast.Expression)),
) -> Result(#(P, List(Option(ast.Expression))), ParseError) {
  case peek(p) {
    RightBracket -> Ok(#(advance(p), acc))
    Comma -> parse_array_elements(advance(p), [None, ..acc])
    DotDotDot -> {
      let saved_invalid = p.has_invalid_pattern
      let spread_pos = pos_of(p)
      let spread_start = peek_at(p, 1)
      let p2 = advance(p)
      use #(p3, expr) <- result.try(parse_assignment_expression(p2))
      // Rest element can't have `= default`, so allow_default: False.
      let elem_invalid = cover_elem_invalid(p3, spread_start, False)
      let p3 = P(..p3, has_invalid_pattern: saved_invalid || elem_invalid)
      let elem =
        Some(ast.SpreadElement(argument: expr, span: span_from(spread_pos, p3)))
      case peek(p3) {
        Comma -> {
          // Rest/spread followed by more elements — valid as expression
          // (spread) but invalid as destructuring (rest must be last)
          let p4 = P(..advance(p3), has_invalid_pattern: True)
          parse_array_elements(p4, [elem, ..acc])
        }
        RightBracket -> Ok(#(advance(p3), [elem, ..acc]))
        _ -> Error(ExpectedCommaOrBracketInExpr(pos_of(p3)))
      }
    }
    _ -> {
      let saved_invalid = p.has_invalid_pattern
      let elem_start = peek(p)
      use #(p2, expr) <- result.try(parse_assignment_expression(p))
      let elem_invalid = cover_elem_invalid(p2, elem_start, True)
      let p2 = P(..p2, has_invalid_pattern: saved_invalid || elem_invalid)
      case peek(p2) {
        Comma -> parse_array_elements(advance(p2), [Some(expr), ..acc])
        RightBracket -> Ok(#(advance(p2), [Some(expr), ..acc]))
        _ -> Error(ExpectedCommaOrBracketInExpr(pos_of(p2)))
      }
    }
  }
}

fn parse_object_literal(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let start = pos_of(p)
  let p2 = advance(p)
  // An object literal's property values are [+In] even inside a for-head.
  use p2 <- with_allow_in(p2, True)
  // Cover-grammar pattern-validity flags are scoped per literal: reset so
  // a sibling literal's invalidity doesn't poison this one's check.
  let p2 = P(..p2, has_invalid_pattern: False)
  use #(p3, props) <- result.map(parse_object_properties(p2, False, []))
  #(
    p3,
    ast.ObjectExpression(
      properties: list.reverse(props),
      span: span_from(start, p3),
    ),
  )
}

fn parse_object_properties(
  p: P,
  has_proto: Bool,
  acc: List(ast.Property),
) -> Result(#(P, List(ast.Property)), ParseError) {
  case peek(p) {
    RightBrace -> Ok(#(advance(p), acc))
    DotDotDot -> {
      let p2 = advance(p)
      use #(p3, expr) <- result.try(parse_assignment_expression(p2))
      let prop = ast.SpreadProperty(argument: expr)
      case peek(p3) {
        // Anything after a spread (including a trailing comma) is fine in an
        // object literal but makes it invalid as a destructuring pattern:
        // ObjectAssignmentPattern requires the rest element to be last.
        Comma ->
          parse_object_properties(
            advance(P(..p3, has_invalid_pattern: True)),
            has_proto,
            [prop, ..acc],
          )
        RightBrace -> Ok(#(advance(p3), [prop, ..acc]))
        _ -> Error(ExpectedCommaOrBraceInObject(pos_of(p3)))
      }
    }
    _ -> {
      use #(p2, prop) <- result.try(parse_object_property(p))
      // Duplicate non-computed __proto__ (§13.2.5.1) — applies only to
      // `PropertyName : AssignmentExpression`; shorthand, methods, accessors,
      // and computed keys are excluded. Checked on the *decoded* PropertyKey
      // so `"__proto__"` matches. Deferred like has_cover_initializer: the
      // early error is skipped when this object literal is consumed as an
      // ObjectAssignmentPattern.
      let is_proto = case prop {
        ast.InitProperty(key:, shorthand: False, ..) ->
          ast.property_key_static_name(key) == Some("__proto__")
        _ -> False
      }
      let p2 = case is_proto && has_proto, p2.ctx.dup_proto_pos {
        True, None ->
          P(..p2, ctx: Ctx(..p2.ctx, dup_proto_pos: Some(pos_of(p))))
        _, _ -> p2
      }
      case peek(p2) {
        Comma ->
          parse_object_properties(advance(p2), has_proto || is_proto, [
            prop,
            ..acc
          ])
        RightBrace -> Ok(#(advance(p2), [prop, ..acc]))
        _ -> Error(ExpectedCommaOrBraceInObject(pos_of(p2)))
      }
    }
  }
}

/// Detect the `async` / `get` / `set` / `*` modifiers ahead of a method
/// definition's property name. Each keyword is treated as the property name
/// (not a modifier) when the following token satisfies `is_terminator`.
/// `star_ends_accessor`: when True, `get *` / `set *` is a property named
/// get/set (classes — fields followed by a generator via ASI); when False,
/// `*` after get/set is consumed as a generator marker (object literals).
/// An escaped `get`/`set` is never the contextual keyword (§12.7.2).
fn parse_method_prefix(
  p: P,
  is_terminator: fn(TokenKind) -> Bool,
  star_ends_accessor: Bool,
) -> #(P, Bool, AccessorPrefix, Bool) {
  let is_async = case peek(p) {
    Async -> !is_terminator(peek_at(p, 1))
    _ -> False
  }
  let p = case is_async {
    True -> advance(p)
    False -> p
  }
  let accessor_kind = case peek(p), peek_had_escape(p) {
    Identifier, False -> {
      let prefix = case peek_value(p) {
        "get" -> GetPrefix
        "set" -> SetPrefix
        _ -> NoAccessor
      }
      case prefix {
        NoAccessor -> NoAccessor
        GetPrefix | SetPrefix -> {
          let next = peek_at(p, 1)
          case is_terminator(next) || { star_ends_accessor && next == Star } {
            True -> NoAccessor
            False -> prefix
          }
        }
      }
    }
    _, _ -> NoAccessor
  }
  let p = case accessor_kind {
    NoAccessor -> p
    GetPrefix | SetPrefix -> advance(p)
  }
  let is_generator = peek(p) == Star
  let p = case is_generator {
    True -> advance(p)
    False -> p
  }
  #(p, is_async, accessor_kind, is_generator)
}

fn parse_object_property(p: P) -> Result(#(P, ast.Property), ParseError) {
  // async / get / set / * modifiers. Each keyword is a name (not a modifier)
  // when followed by `( , } :`.
  let #(p4, has_async, accessor_kind, is_generator) =
    parse_method_prefix(
      p,
      fn(t) {
        case t {
          LeftParen | Comma | RightBrace | Colon -> True
          _ -> False
        }
      },
      False,
    )
  // Property name (identifier, string, number, computed)
  // Remember token kind for shorthand validation — only identifiers/contextual
  // keywords can be shorthand properties.
  let prop_name_kind = peek(p4)
  let prop_name_value = peek_value(p4)
  let is_valid_shorthand = case prop_name_kind {
    Identifier -> True
    _ -> is_contextual_keyword(prop_name_kind)
  }
  use #(p5, key) <- result.try(parse_property_name(p4))
  use Nil <- result.try(reject_private_property_key(p4, key))
  // Generator shorthand (*name) must be a method — must have (
  case is_generator && peek(p5) != LeftParen {
    True -> Error(UnexpectedToken(pos_of(p5), peek(p5)))
    False ->
      parse_object_property_value(
        p,
        p5,
        has_async,
        accessor_kind,
        is_generator,
        prop_name_kind,
        prop_name_value,
        is_valid_shorthand,
        key,
      )
  }
}

fn parse_object_property_value(
  p: P,
  p5: P,
  has_async: Bool,
  accessor_kind: AccessorPrefix,
  is_generator: Bool,
  prop_name_kind: TokenKind,
  prop_name_value: String,
  is_valid_shorthand: Bool,
  key: ast.PropertyKey,
) -> Result(#(P, ast.Property), ParseError) {
  case peek(p5) {
    LeftParen -> {
      // Method — validate getter/setter params
      // Object methods allow super.x but not super()
      // Methods make the object invalid as destructuring target
      let p5 = P(..p5, has_invalid_pattern: True)
      use #(p6, params, body) <- result.map(parse_method_params_body(
        p5,
        p,
        accessor_kind,
        is_generator,
        has_async,
        False,
        False,
      ))
      let fn_lit =
        ast.FunctionLiteral(
          name: None,
          params: params,
          body: body,
          is_generator: is_generator,
          is_async: has_async,
        )
      let prop = case accessor_kind {
        GetPrefix ->
          ast.AccessorProperty(key:, value: fn_lit, kind: ast.GetAccessor)
        SetPrefix ->
          ast.AccessorProperty(key:, value: fn_lit, kind: ast.SetAccessor)
        NoAccessor -> ast.MethodProperty(key:, value: fn_lit)
      }
      #(p6, prop)
    }
    Colon -> {
      // key: value — mark invalid pattern if value is not a valid
      // destructuring component. Same per-element isolation as
      // parse_array_elements: a simple LHS target shadows inner
      // invalid-pattern flags (e.g. `{k: {m(){}}.y}` is valid).
      let saved_invalid = p5.has_invalid_pattern
      let p6 = advance(p5)
      let value_start = peek(p6)
      use #(p7, expr) <- result.try(parse_assignment_expression(p6))
      let elem_invalid = cover_elem_invalid(p7, value_start, True)
      let p7 = P(..p7, has_invalid_pattern: saved_invalid || elem_invalid)
      Ok(#(p7, ast.InitProperty(key:, value: expr, shorthand: False)))
    }
    tok -> {
      // Shorthand `{ x }` or shorthand-with-default `{ x = d }` (cover
      // grammar — the latter only valid in destructuring / arrow params).
      // Both require the key to be a plain identifier.
      case is_valid_shorthand {
        False -> Error(UnexpectedToken(pos_of(p5), prop_name_kind))
        True -> {
          // Shorthand is an IdentifierReference — apply §13.1.1 early
          // errors (escaped reserved words, strict future-reserved,
          // yield/await, arguments in static blocks / field initializers).
          use Nil <- result.try(check_identifier_reference(p5, prop_name_value))
          let p5 = P(..p5, sb: scope.sb_ref(p5.sb, prop_name_value))
          // The value identifier is the same token as the key (shorthand is
          // only reachable for an identifier key), so it is rebuilt from the
          // key's name and span. For `= default`, wrap it in an
          // AssignmentExpression and set the cover flag so the caller can
          // reject if not destructuring.
          let key_span = ast.property_key_span(key)
          let key_ident = ast.Identifier(name: prop_name_value, span: key_span)
          use #(p7, value) <- result.map(case tok {
            Equal -> {
              let p6 = advance(p5)
              use #(p7, rhs) <- result.map(parse_assignment_expression(p6))
              #(
                P(..p7, ctx: Ctx(..p7.ctx, has_cover_initializer: True)),
                ast.AssignmentExpression(
                  operator: ast.Assign,
                  left: key_ident,
                  right: rhs,
                  span: ast.Span(key_span.start, p7.prev_end),
                ),
              )
            }
            _ -> Ok(#(p5, key_ident))
          })
          #(p7, ast.InitProperty(key:, value:, shorthand: True))
        }
      }
    }
  }
}

fn parse_function_expression(
  p: P,
  is_async is_async: Bool,
) -> Result(#(P, ast.Expression), ParseError) {
  // Span starts at `function` (or `async` for async function expressions) —
  // the caller positions `p` at that first keyword.
  let start = pos_of(p)
  use #(p4, p3, is_generator, func_name) <- result.try(parse_function_head(
    p,
    is_async,
    True,
  ))
  // `enter_function_context` overwrites in_generator/in_async itself, so the
  // inner-context override applied to `p4` for name validation is harmless.
  let p_inner =
    enter_function_context(
      p4,
      is_generator,
      is_async,
      string.to_option(func_name),
    )
  let fn_scope = p_inner.sb.current
  use #(p5, params, body) <- result.try(
    parse_function_params_and_body(p_inner) |> restore_context_fn(p),
  )
  // §15.2.6 NFE self-binding: a named function expression's name is
  // visible from within its own body as an immutable `FnNameBinding`
  // (NOT in the enclosing scope — contrast FunctionDeclaration, which
  // registers via `register_function_name` in the OUTER scope).
  // Declared into the captured function scope AFTER the body so it
  // never trips a §14.2.1 early error against a body-level
  // `let f` / `var f` (both legal — the spec puts the NFE name in a
  // separate wrapper env). `sb_declare_in` is first-wins: a body
  // declaration of the same name survives.
  let p5 = case func_name {
    "" -> p5
    name ->
      P(
        ..p5,
        sb: scope.sb_declare_in(
          p5.sb,
          fn_scope,
          name,
          scope.FnNameBinding,
          synthetic: True,
        ),
      )
  }
  // The optional self-name identifier is the current token of `p3`.
  let name_opt = optional_named_binding(func_name, span_of(p3))
  Ok(#(
    p5,
    ast.FunctionExpression(
      name: name_opt,
      params: params,
      body: body,
      is_generator: is_generator,
      is_async: is_async,
      span: span_from(start, p5),
    ),
  ))
}

fn parse_class_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let start = pos_of(p)
  use #(p2, name, super_class, body) <- result.map(parse_class_head_and_tail(
    p,
    False,
    False,
  ))
  #(
    p2,
    ast.ClassExpression(name:, super_class:, body:, span: span_from(start, p2)),
  )
}

fn parse_regex_literal(p: P) -> Result(#(P, ast.Expression), ParseError) {
  // Re-lex from source as a regex literal.
  // The current token is Slash or SlashEqual at some position.
  let start_pos = pos_of(p)
  // Scan the regex body starting after the opening /
  let body_start = start_pos + 1
  case regex.scan_regex_source(p.bytes, body_start) {
    Ok(end_pos) -> {
      // end_pos is past the closing /, now skip optional flags
      use #(flags_end, flags) <- result.try(
        regex.skip_regex_flags(p.bytes, end_pos)
        |> result.map_error(regexp_syntax_error),
      )
      // Validate the body [body_start, end_pos - 1) against the ECMAScript
      // Pattern grammar (Annex B extended grammar without u/v, strict
      // grammar with it), reporting parse-time early errors.
      use Nil <- result.try(
        regex.validate_pattern(p.bytes, body_start, end_pos - 1, flags)
        |> result.map_error(regexp_syntax_error),
      )
      // Extract pattern body and flags as strings
      // The body was scanned from `p.bytes` at code-point boundaries, so the
      // slice is always in-bounds and valid text.
      let assert Some(pattern) =
        source_bytes.slice(p.bytes, body_start, end_pos - 1 - body_start)
        as "parser: regex body slice out of range"
      // `flags.flags` is in source order, so this is the source text after `/`.
      let flags_str = string.join(flags.flags, "")
      // The token window at/past the `/` was lexed with no expression
      // context, so it is garbage (a quote, backtick or `/*` in the body
      // may have opened a phantom string / template / comment). Lexing is
      // on demand: throw the window away and continue just past the flags.
      let p2 = jump_to(p, flags_end)
      // Span covers the whole `/pattern/flags` source slice — the regex was
      // re-scanned byte-wise, so use the exact byte bounds.
      let span = ast.Span(start: start_pos, end: flags_end)
      Ok(#(p2, ast.RegExpLiteral(pattern: pattern, flags: flags_str, span:)))
    }
    Error(e) -> Error(regexp_syntax_error(e))
  }
}

/// Discard the token window and continue lexing at byte `from`, which sits
/// on the current token's line (used after re-scanning a construct the
/// token grammar cannot classify itself — a regex literal, a template
/// part — none of which spans a line terminator).
fn jump_to(p: P, from: Int) -> P {
  let line = line_of(p)
  ensure_current(
    P(
      ..p,
      tokens: [],
      scan: lexer.scanner_at(p.bytes, from, line, p.scan.mode),
      prev_line: line,
      prev_end: from,
    ),
  )
}

// ---- Import/Export ----

/// Helper: expect 'from' keyword followed by a string module specifier, then eat semicolon.
/// Returns the parser state and the parsed StringLiteral source.
/// Parse `from "module"` and return the specifier plus the byte offset just
/// past the module-specifier string token (its `pos + raw_len`). The end offset
/// is the half-open end of the enclosing import declaration's span.
fn expect_from_module_specifier(
  p: P,
) -> Result(#(P, ast.StringLiteral, Int), ParseError) {
  use p2 <- result.try(expect(p, From))
  case peek(p2) {
    KString -> {
      use value <- result.try(module_specifier_value(p2))
      let spec_end = pos_of(p2) + peek_raw_len(p2)
      use p3 <- result.try(skip_import_attributes(advance(p2)))
      use p4 <- result.map(eat_semicolon(p3))
      #(p4, ast.StringLit(value:), spec_end)
    }
    _ -> Error(ExpectedModuleSpecifier(pos_of(p2)))
  }
}

/// Import-attributes proposal: an optional `with { }` clause after the module
/// specifier. This host supports no import attributes (§13.3.10
/// AllImportAttributesSupported is false for every key), so only an EMPTY
/// attribute list parses; any attribute entry is a SyntaxError here, which
/// matches the spec's eventual rejection of unsupported attributes at load
/// time as closely as the static pipeline allows.
fn skip_import_attributes(p: P) -> Result(P, ParseError) {
  case peek(p) {
    With -> {
      use p2 <- result.try(expect(advance(p), LeftBrace))
      expect(p2, RightBrace)
    }
    _ -> Ok(p)
  }
}

/// Parse the `from "module"` tail of an import declaration and build the
/// resulting `ImportDeclaration` node from the already-parsed specifiers.
fn finish_import_from(
  p: P,
  span_start: Int,
  phase: ast.ImportPhase,
  specifiers: List(ast.ImportSpecifier),
) -> Result(#(P, ast.ModuleItem), ParseError) {
  use #(p2, source, span_end) <- result.map(expect_from_module_specifier(p))
  #(
    p2,
    ast.ImportDeclaration(
      specifiers:,
      source:,
      phase:,
      span: ast.Span(start: span_start, end: span_end),
    ),
  )
}

/// Parse the `* as name from "mod"` tail of an import declaration. `p` is
/// positioned just past the `*`; `leading` carries any specifiers that
/// precede the namespace one (the default binding in `import d, * as ns`).
fn parse_namespace_import_tail(
  p: P,
  span_start: Int,
  phase: ast.ImportPhase,
  leading: List(ast.ImportSpecifier),
) -> Result(#(P, ast.ModuleItem), ParseError) {
  use p2 <- result.try(expect(p, As))
  let binding_name = peek_value(p2)
  let binding_span = span_of(p2)
  use p3 <- result.try(expect_identifier(p2))
  use p4 <- result.try(check_duplicate_import_binding(p3, binding_name))
  let ns =
    ast.ImportNamespaceSpecifier(local: binding_name, local_span: binding_span)
  finish_import_from(p4, span_start, phase, list.append(leading, [ns]))
}

fn parse_import_declaration(p: P) -> Result(#(P, ast.ModuleItem), ParseError) {
  // `p` is positioned on the `import` keyword; the span starts at its offset.
  let span_start = pos_of(p)
  let p2 = advance(p)
  // Source-phase imports proposal: `import source ImportedBinding FromClause ;`
  // Only when `source` is followed by a binding and then `from` — otherwise
  // `import source from "m"` is a default import whose binding is `source`.
  let is_source_phase =
    peek(p2) == Identifier
    && peek_value(p2) == "source"
    && is_identifier_or_keyword(peek_at(p2, 1))
    && peek_at(p2, 2) == From
  use <- bool.lazy_guard(is_source_phase, fn() {
    parse_source_phase_import(p2, span_start)
  })
  // Defer-import-eval proposal: `import defer NameSpaceImport FromClause ;`
  // Only when `defer` is followed by `*` — otherwise `import defer from "m"`
  // is a default import whose binding is `defer`. `defer` is a grammar
  // terminal symbol (§5.1.5), so it must appear literally — a \u escape in it
  // is a SyntaxError (here: not recognized as the defer phase, and `import
  // defer * as ns` then fails to parse as any other import form).
  let is_defer_phase =
    peek(p2) == Identifier
    && peek_value(p2) == "defer"
    && !peek_had_escape(p2)
    && peek_at(p2, 1) == Star
  use <- bool.lazy_guard(is_defer_phase, fn() {
    parse_namespace_import_tail(
      advance(advance(p2)),
      span_start,
      ast.PhaseDefer,
      [],
    )
  })
  case peek(p2) {
    KString -> {
      // import "module"
      use value <- result.try(module_specifier_value(p2))
      let span_end = pos_of(p2) + peek_raw_len(p2)
      use p3 <- result.map(eat_semicolon(advance(p2)))
      #(
        p3,
        ast.ImportDeclaration(
          specifiers: [],
          source: ast.StringLit(value:),
          phase: ast.PhaseEvaluation,
          span: ast.Span(start: span_start, end: span_end),
        ),
      )
    }
    Star ->
      // import * as name from "module"
      parse_namespace_import_tail(
        advance(p2),
        span_start,
        ast.PhaseEvaluation,
        [],
      )
    LeftBrace -> {
      // import { a, b } from "module"
      let p3 = advance(p2)
      use #(p4, specifiers) <- result.try(parse_import_specifiers(p3))
      finish_import_from(p4, span_start, ast.PhaseEvaluation, specifiers)
    }
    other_kind -> {
      // import defaultExport from "module"
      // or import defaultExport, { ... } from "module"
      // or import defaultExport, * as name from "module"
      // The ImportedDefaultBinding may be any BindingIdentifier, including
      // contextual keywords like `from`, `as`, `of`, `async` (test262:
      // `import from from '...'`). Reserved words are rejected by
      // check_import_binding_name.
      use <- bool.guard(
        !is_identifier_or_keyword(other_kind),
        Error(ExpectedImportSpecifier(pos_of(p2))),
      )
      let default_name = peek_value(p2)
      use Nil <- result.try(check_import_binding_name(
        p2,
        default_name,
        other_kind,
      ))
      use p2b <- result.try(check_duplicate_import_binding(p2, default_name))
      let default_spec =
        ast.ImportDefaultSpecifier(local: default_name, local_span: span_of(p2))
      let p3 = advance(p2b)
      case peek(p3) {
        Comma -> {
          let p4 = advance(p3)
          case peek(p4) {
            Star ->
              parse_namespace_import_tail(
                advance(p4),
                span_start,
                ast.PhaseEvaluation,
                [default_spec],
              )
            LeftBrace -> {
              let p5 = advance(p4)
              use #(p6, named_specs) <- result.try(parse_import_specifiers(p5))
              finish_import_from(p6, span_start, ast.PhaseEvaluation, [
                default_spec,
                ..named_specs
              ])
            }
            _ -> Error(ExpectedBraceOrStarAfterComma(pos_of(p4)))
          }
        }
        From ->
          finish_import_from(p3, span_start, ast.PhaseEvaluation, [
            default_spec,
          ])
        _ -> Error(ExpectedFromOrComma(pos_of(p3)))
      }
    }
  }
}

/// Source-phase imports proposal: `import source ImportedBinding FromClause ;`
/// `p` is positioned on the `source` identifier. The binding itself is not
/// modeled in the AST yet (GetModuleSource always throws for source text
/// modules anyway, §16.2.1.7.2); the declaration still records the module
/// request so resolution/linking of the specifier behaves per spec.
fn parse_source_phase_import(
  p: P,
  span_start: Int,
) -> Result(#(P, ast.ModuleItem), ParseError) {
  // Past `source`, onto the ImportedBinding.
  let p2 = advance(p)
  let binding_name = peek_value(p2)
  let binding_kind = peek(p2)
  use Nil <- result.try(check_import_binding_name(
    p2,
    binding_name,
    binding_kind,
  ))
  use p3 <- result.try(check_duplicate_import_binding(p2, binding_name))
  finish_import_from(advance(p3), span_start, ast.PhaseSource, [])
}

/// Parse a comma-separated list with optional trailing comma, terminated by
/// `close`. Used for call arguments, import specifiers, and export specifiers.
fn parse_comma_list(
  p: P,
  acc: List(a),
  close: TokenKind,
  parse_one: fn(P) -> Result(#(P, a), ParseError),
  err: fn(Int) -> ParseError,
) -> Result(#(P, List(a)), ParseError) {
  case peek(p) {
    t if t == close -> Ok(#(advance(p), list.reverse(acc)))
    _ -> {
      use #(p2, item) <- result.try(parse_one(p))
      let acc = [item, ..acc]
      case peek(p2) {
        Comma ->
          case peek_at(p2, 1) == close {
            True -> Ok(#(advance(advance(p2)), list.reverse(acc)))
            False -> parse_comma_list(advance(p2), acc, close, parse_one, err)
          }
        t if t == close -> Ok(#(advance(p2), list.reverse(acc)))
        _ -> Error(err(pos_of(p2)))
      }
    }
  }
}

fn parse_import_specifiers(
  p: P,
) -> Result(#(P, List(ast.ImportSpecifier)), ParseError) {
  parse_comma_list(
    p,
    [],
    RightBrace,
    parse_import_specifier,
    ExpectedCommaOrBraceInImport,
  )
}

/// True when `kind` can appear as a name in an import/export `{ ... }` list
/// or after `as` — identifiers, string literals, and keywords used as
/// identifiers (e.g. `import { default as x }`).
fn is_specifier_name(kind: TokenKind) -> Bool {
  kind == Identifier || kind == KString || is_keyword_as_identifier(kind)
}

fn parse_import_specifier(
  p: P,
) -> Result(#(P, ast.ImportSpecifier), ParseError) {
  // name or name as alias or "string" as alias
  // The local binding name is the alias (after 'as') or the original name.
  case is_specifier_name(peek(p)) {
    False -> Error(ExpectedImportSpecifierName(pos_of(p)))
    True -> {
      use imported_name <- result.try(specifier_name_value(p))
      let p2 = advance(p)
      case peek(p2) {
        As -> {
          // The alias is the local binding name — must be a valid binding identifier
          let p3 = advance(p2)
          use p4 <- result.try(expect_identifier(p3))
          finish_import_named_specifier(p3, p4, imported_name)
        }
        // No alias: the original name is the local binding — must be valid
        _ -> finish_import_named_specifier(p, p2, imported_name)
      }
    }
  }
}

fn finish_import_named_specifier(
  check_p: P,
  state_p: P,
  imported: String,
) -> Result(#(P, ast.ImportSpecifier), ParseError) {
  let local = peek_value(check_p)
  let local_span = span_of(check_p)
  use Nil <- result.try(check_import_binding_name(check_p, local, peek(check_p)))
  use p <- result.map(check_duplicate_import_binding(state_p, local))
  #(p, ast.ImportNamedSpecifier(imported:, local:, local_span:))
}

/// Parse "export function name(){}" or "export async function name(){}".
/// Extracts the function name and registers it as an export name before parsing.
fn parse_export_named_function(
  p: P,
  is_async: Bool,
) -> Result(#(P, ast.Declaration), ParseError) {
  // Skip past "async" and "function" keywords to find the name
  let name_offset = case is_async {
    True -> 2
    False -> 1
  }
  // Check for generator star
  let name_offset = case peek_at(p, name_offset) == Star {
    True -> name_offset + 1
    False -> name_offset
  }
  // The name is optional (for export default), but here it should be present
  let export_name = peek_value_at(p, name_offset)
  let checked = case export_name != "" {
    True -> check_duplicate_export(p, export_name)
    False -> Ok(p)
  }
  use p2 <- result.try(checked)
  use #(p3, function) <- result.map(parse_function_decl_impl(p2, True, is_async))
  #(p3, ast.DeclFunction(function:))
}

/// Wrap a parsed declaration as an ExportDeclaration module item.
/// `before` is the parser state positioned at the `export` keyword, so the span
/// starts there and ends just past the last token the declaration consumed, and
/// `line` is the line the `export` keyword sits on — the compiler tags the
/// unwrapped declaration with it (see `ast.ExportDeclaration`).
fn export_named_decl(
  before: P,
  parsed: #(P, ast.Declaration),
) -> #(P, ast.ModuleItem) {
  let #(p, declaration) = parsed
  #(
    p,
    ast.ExportDeclaration(
      declaration:,
      line: line_of(before),
      span: ast.Span(start: pos_of(before), end: consumed_end(before, p)),
    ),
  )
}

/// Parse "export class name {}".
/// Extracts the class name and registers it as an export name before parsing.
fn parse_export_named_class(p: P) -> Result(#(P, ast.Declaration), ParseError) {
  // The name follows "class" keyword
  let export_name = peek_value_at(p, 1)
  case export_name != "" {
    True -> {
      use p2 <- result.try(check_duplicate_export(p, export_name))
      parse_class_decl_impl(p2)
    }
    False -> parse_class_decl_impl(p)
  }
}

/// `export default function(){}` / `export default async function(){}` — the
/// name is optional here and nowhere else.
fn parse_default_fn(
  p: P,
  is_async: Bool,
) -> Result(#(P, DefaultExportDecl), ParseError) {
  use #(p2, function) <- result.map(parse_function_decl_impl(p, False, is_async))
  #(p2, DefaultFn(function:))
}

/// `export default class {}` — the name is optional here and nowhere else.
fn parse_default_class(p: P) -> Result(#(P, DefaultExportDecl), ParseError) {
  use #(p2, name, super_class, body) <- result.map(parse_class_head_and_tail(
    p,
    False,
    True,
  ))
  #(p2, DefaultClass(name:, super_class:, body:))
}

// Shared tail for `export default <function|class|async function>` — parses
// the declaration with `parse`, converts it to the equivalent expression, and
// wraps it in ExportDefaultDeclaration. `parse` returns a `DefaultExportDecl`,
// not a `Statement`, so no arm here has to cope with a statement the grammar
// cannot put after `export default`.
fn finish_export_default_decl(
  p_export: P,
  p_decl: P,
  parse: fn(P) -> Result(#(P, DefaultExportDecl), ParseError),
) -> Result(#(P, ast.ModuleItem), ParseError) {
  let decl_start = pos_of(p_decl)
  use #(p4, decl) <- result.map(parse(p_decl))
  let decl_span = span_from(decl_start, p4)
  // §16.2.3.7 BoundNames: an anonymous `export default function/class {…}`
  // declares the synthetic `*default*` binding at module scope. Named
  // declarations already registered their own name via register_function_name
  // / parse_class_decl_impl and that name IS the exported binding — no
  // `*default*` for those. VarBinding (not ConstBinding) per the
  // emit.gleam:425-427 contract so the synthetic store is a plain assignment;
  // linker_seeded already lists `*default*` so the prologue skips its seed.
  let p4 = case default_export_name(decl) {
    None ->
      P(
        ..p4,
        sb: scope.sb_declare(
          p4.sb,
          scope.default_export,
          scope.VarBinding,
          synthetic: True,
        ),
      )
    Some(_) -> p4
  }
  #(
    p4,
    ast.ExportDefaultDeclaration(
      declaration: default_export_expr(decl, decl_span),
      line: line_of(p_export),
      span: ast.Span(start: pos_of(p_export), end: consumed_end(p_export, p4)),
    ),
  )
}

// Shared tail for `export default <AssignmentExpression>;`.
fn finish_export_default_expr(
  p_export: P,
  p_expr: P,
) -> Result(#(P, ast.ModuleItem), ParseError) {
  use #(p4, expr) <- result.try(parse_assignment_expression(p_expr))
  use p5 <- result.map(eat_semicolon(p4))
  // §16.2.3.7 BoundNames: `export default <AssignmentExpression>` declares
  // the synthetic `*default*` binding at module scope. VarBinding so the
  // emitter's synthetic store (emit.gleam:425-427) is a plain assignment;
  // linker_seeded already lists `*default*` so the prologue skips its seed
  // and analyze_captures forces the box.
  let p5 =
    P(
      ..p5,
      sb: scope.sb_declare(
        p5.sb,
        scope.default_export,
        scope.VarBinding,
        synthetic: True,
      ),
    )
  #(
    p5,
    ast.ExportDefaultDeclaration(
      declaration: expr,
      line: line_of(p_export),
      span: ast.Span(start: pos_of(p_export), end: consumed_end(p_export, p5)),
    ),
  )
}

// Shared tail for `export * [as name] from "module"`. `p` must be positioned
// at the module-specifier string token (i.e. after `from` has been consumed).
fn finish_export_all(
  p: P,
  span_start: Int,
  exported: Option(String),
) -> Result(#(P, ast.ModuleItem), ParseError) {
  case peek(p) {
    KString -> {
      use value <- result.try(module_specifier_value(p))
      // Span ends at the end of the module-specifier string token.
      let span_end = pos_of(p) + peek_raw_len(p)
      use p2 <- result.map(eat_semicolon(advance(p)))
      #(
        p2,
        ast.ExportAllDeclaration(
          exported:,
          source: ast.StringLit(value:),
          span: ast.Span(start: span_start, end: span_end),
        ),
      )
    }
    _ -> Error(ExpectedModuleSpecifier(pos_of(p)))
  }
}

fn parse_export_declaration(p: P) -> Result(#(P, ast.ModuleItem), ParseError) {
  let p2 = advance(p)
  case peek(p2) {
    Default -> {
      use p2b <- result.try(check_duplicate_export(p2, "default"))
      let p3 = advance(p2b)
      case peek(p3) {
        Function ->
          finish_export_default_decl(p, p3, parse_default_fn(_, False))
        Class -> finish_export_default_decl(p, p3, parse_default_class)
        Async ->
          case peek_at(p3, 1) {
            Function ->
              finish_export_default_decl(p, p3, parse_default_fn(_, True))
            _ -> finish_export_default_expr(p, p3)
          }
        _ -> finish_export_default_expr(p, p3)
      }
    }
    Var | Let | Const ->
      result.map(
        parse_variable_declaration_decl(
          P(..p2, ctx: Ctx(..p2.ctx, in_export_decl: True)),
        ),
        export_named_decl(p, _),
      )
    Function ->
      result.map(parse_export_named_function(p2, False), export_named_decl(p, _))
    Class -> result.map(parse_export_named_class(p2), export_named_decl(p, _))
    Async ->
      case peek_at(p2, 1) {
        Function ->
          result.map(parse_export_named_function(p2, True), export_named_decl(
            p,
            _,
          ))
        _ -> Error(ExpectedFunctionAfterAsync(pos_of(p2)))
      }
    Star -> {
      // export * from "module"
      // Span starts at the `export` keyword (p, before it was advanced to p2).
      let span_start = pos_of(p)
      let p3 = advance(p2)
      case peek(p3) {
        As -> {
          // export * as name from "module"
          let p4 = advance(p3)
          use exported_value <- result.try(specifier_name_value(p4))
          let p5 = case is_specifier_name(peek(p4)) {
            True -> advance(p4)
            False -> p4
          }
          use p5b <- result.try(check_duplicate_export(p5, exported_value))
          use p6 <- result.try(expect(p5b, From))
          finish_export_all(p6, span_start, Some(exported_value))
        }
        From -> finish_export_all(advance(p3), span_start, None)
        _ -> Error(ExpectedAsOrFromAfterExportStar(pos_of(p3)))
      }
    }
    LeftBrace -> {
      // export { a, b } or export { a, b } from "module"
      let p3 = advance(p2)
      // Save refs so we can revert if this is a re-export (from "module")
      let saved_local_refs = p3.export_local_refs
      use #(p4, specifiers) <- result.try(parse_export_specifiers(p3))
      case peek(p4) {
        From -> {
          // Re-export: local names don't need to be declared in this module
          let p4 = P(..p4, export_local_refs: saved_local_refs)
          let p5 = advance(p4)
          case peek(p5) {
            KString -> {
              use value <- result.try(module_specifier_value(p5))
              use p6 <- result.map(eat_semicolon(advance(p5)))
              #(
                p6,
                ast.ExportNamed(
                  specifiers:,
                  source: Some(ast.StringLit(value:)),
                  span: ast.Span(start: pos_of(p), end: consumed_end(p, p6)),
                ),
              )
            }
            _ -> Error(ExpectedModuleSpecifier(pos_of(p5)))
          }
        }
        _ -> {
          use p5 <- result.try(eat_semicolon(p4))
          Ok(#(
            p5,
            ast.ExportNamed(
              specifiers:,
              source: None,
              span: ast.Span(start: pos_of(p), end: consumed_end(p, p5)),
            ),
          ))
        }
      }
    }
    _ -> Error(UnexpectedAfterExport(pos_of(p2)))
  }
}

fn parse_export_specifiers(
  p: P,
) -> Result(#(P, List(ast.ExportSpecifier)), ParseError) {
  parse_comma_list(
    p,
    [],
    RightBrace,
    parse_export_specifier,
    ExpectedCommaOrBraceInExport,
  )
}

fn parse_export_specifier(
  p: P,
) -> Result(#(P, ast.ExportSpecifier), ParseError) {
  case is_specifier_name(peek(p)) {
    False -> Error(ExpectedExportSpecifierName(pos_of(p)))
    True -> {
      use local <- result.try(specifier_name_value(p))
      // The local-binding identifier is the current token (the name before
      // `as`, or the whole identifier when there is no alias).
      let local_span = span_of(p)
      let local_pos = pos_of(p)
      // Resolve the exported name and the parser positioned at that name's
      // token (for the duplicate-export error position): either the alias
      // after `as`, or the local identifier itself when there is no alias.
      use #(p3, exported) <- result.try(case peek(advance(p)) {
        As -> {
          let p3 = advance(advance(p))
          case is_specifier_name(peek(p3)) {
            True -> {
              use exported <- result.map(specifier_name_value(p3))
              #(p3, exported)
            }
            False ->
              Error(error_at_current(p3, ExpectedExportAlias(pos_of(p3))))
          }
        }
        _ -> Ok(#(p, local))
      })
      use p4 <- result.try(check_duplicate_export(p3, exported))
      // Track local name for undeclared-export validation
      let p5 =
        P(..p4, export_local_refs: [#(local, local_pos), ..p4.export_local_refs])
      Ok(#(advance(p5), ast.ExportSpecifier(local:, exported:, local_span:)))
    }
  }
}

// ---- Strict mode detection ----

/// Check if the next tokens form a { body } with "use strict" in the
/// directive prologue. If so, set strict mode. Does NOT consume tokens.
/// Also checks for retroactive octal escape errors and parameter name errors.
fn check_use_strict_in_body(p: P) -> Result(P, ParseError) {
  case p.ctx.strict {
    True ->
      // FunctionBodyContainsUseStrict + non-simple parameter list is a
      // SyntaxError even when the function is ALREADY strict (class methods,
      // nested strict functions) - the sloppy path catches this in
      // check_retroactive_params, this catches the already-strict case.
      case p.ctx.has_non_simple_param {
        True ->
          case
            peek(p) == LeftBrace
            && prologue_has_use_strict(look_skip(look_from(p)))
          {
            True -> Error(MisplacedUseStrictDirective(pos_of(p)))
            False -> Ok(p)
          }
        False -> Ok(p)
      }
    False ->
      case peek(p) {
        LeftBrace -> scan_directive_prologue(p, look_skip(look_from(p)))
        _ -> Ok(p)
      }
  }
}

/// A pure lookahead cursor over the upcoming tokens: it yields the
/// already-lexed current token(s) first and then keeps LEXING from the
/// scanner WITHOUT retaining anything — the parser's own state is
/// untouched, and whatever it lexes is thrown away.
///
/// This is the ONLY way the parser looks past its current token (see
/// `upcoming` for the grammar's bounded lookahead and the
/// directive-prologue scans for the unbounded one), which is what keeps
/// the ordinary scan from ever running ahead of the parse — the invariant
/// the re-scanned constructs (regex literals, template continuations)
/// depend on, and the reason no source the parser jumps over is ever
/// touched by the token scan at all.
type Look {
  Look(tokens: List(Token), scan: lexer.Scanner)
}

/// The lookahead cursor positioned at the parser's current token.
fn look_from(p: P) -> Look {
  Look(tokens: p.tokens, scan: p.scan)
}

/// The next token of the lookahead (Eof forever at the end of input) and
/// the advanced cursor. A hard lexer error ahead surfaces as the lexer's
/// Illegal sentinel — never accepted by these lookaheads — exactly as the
/// real window would materialise it.
fn look_next(look: Look) -> #(Token, Look) {
  case look.tokens {
    [token, ..rest] -> #(token, Look(..look, tokens: rest))
    [] -> {
      let #(token, scan) = lexer.scan_next(look.scan)
      #(token, Look(tokens: [], scan:))
    }
  }
}

/// The `n`-th token of the lookahead (0 = its first token).
fn look_at(look: Look, n: Int) -> Token {
  let #(token, look) = look_next(look)
  case n <= 0 || token.kind == Eof {
    True -> token
    False -> look_at(look, n - 1)
  }
}

/// Drop one lookahead token.
fn look_skip(look: Look) -> Look {
  let #(_, look) = look_next(look)
  look
}

/// Skip the optional `;` terminating a directive.
fn look_skip_semicolon(look: Look) -> Look {
  let #(token, after) = look_next(look)
  case token.kind {
    Semicolon -> after
    // Not a `;`: put the freshly seen token back rather than dropping the
    // cursor that already lexed it.
    _ -> Look(tokens: [token, ..after.tokens], scan: after.scan)
  }
}

/// Walk the directive prologue at the lookahead cursor (pure lookahead —
/// nothing is consumed): `Some(the directive tokens seen BEFORE it)`
/// when a "use strict" directive is present, `None` otherwise. The one
/// walk both the boolean check and strict-mode activation share.
fn prologue_use_strict(look: Look, seen: List(Token)) -> Option(List(Token)) {
  let #(token, look) = look_next(look)
  case token.kind {
    KString ->
      case token.value {
        "use strict" -> Some(seen)
        _ -> prologue_use_strict(look_skip_semicolon(look), [token, ..seen])
      }
    _ -> None
  }
}

/// Whether the directive prologue at the lookahead cursor contains a
/// "use strict" directive.
fn prologue_has_use_strict(look: Look) -> Bool {
  option.is_some(prologue_use_strict(look, []))
}

/// Check if program body starts with "use strict" directive.
fn check_use_strict_at_start(p: P) -> Result(P, ParseError) {
  case p.ctx.strict {
    True -> Ok(p)
    False -> scan_directive_prologue(p, look_from(p))
  }
}

/// Scan the directive prologue (pure lookahead from `look`) for
/// "use strict". If found, set strict mode and check retroactive octal
/// escapes and parameter names.
fn scan_directive_prologue(p: P, look: Look) -> Result(P, ParseError) {
  case prologue_use_strict(look, []) {
    None -> Ok(p)
    Some(seen_directives) -> {
      use Nil <- result.try(check_retroactive_octals(p, seen_directives))
      let p = P(..p, ctx: Ctx(..p.ctx, strict: True))
      check_retroactive_params(p)
    }
  }
}

/// Check if any directive string seen before the "use strict" contained an
/// Annex B escape form (`\07` legacy octal, or `\8`/`\9` non-octal decimal)
/// that the directive retroactively forbids. The lexer flagged those tokens
/// as it scanned them (`lexer.Token.annex_b_legacy`).
fn check_retroactive_octals(
  p: P,
  seen_directives: List(Token),
) -> Result(Nil, ParseError) {
  use <- bool.guard(
    list.any(seen_directives, fn(token) { token.annex_b_legacy }),
    Error(OctalEscapeStrictMode(pos_of(p))),
  )
  Ok(Nil)
}

/// When "use strict" is found in a function body, retroactively validate
/// parameter names that were parsed in sloppy mode.
/// Also rejects "use strict" when params are non-simple (destructuring/rest/default).
fn check_retroactive_params(p: P) -> Result(P, ParseError) {
  case p.ctx.has_non_simple_param {
    True -> Error(MisplacedUseStrictDirective(pos_of(p)))
    False -> validate_retroactive_param_names(p, p.ctx.param_bound_names)
  }
}

/// Validate parameter names against strict mode rules.
fn validate_retroactive_param_names(
  p: P,
  names: List(String),
) -> Result(P, ParseError) {
  case names {
    [] -> Ok(p)
    [name, ..rest] ->
      case strict_binding_violation(name) {
        Some(kind) -> Error(strict_name_error(kind, name, pos_of(p)))
        None -> validate_retroactive_param_names(p, rest)
      }
  }
}

// ---- Context helpers ----

/// Enter a new function context — resets loop/switch/labels, increments function_depth
///
/// The whole outer `Ctx` is put back by `restore_outer_context` at the
/// matching function exit; `strict` is the only field carried IN (a nested
/// function inherits the enclosing strictness). This is therefore the ONE
/// place a nested function's boundary state is initialised: no caller may
/// hand-save-and-restore a `Ctx` field around a nested parse.
///
/// `strict_name` is the function's own BindingIdentifier (`None` for arrows,
/// methods and static blocks, which have no binding name of their own) —
/// remembered so a retroactive `"use strict"` in the body can reject
/// `function eval() { "use strict"; }`.
fn enter_function_context(
  p: P,
  is_generator: Bool,
  is_async: Bool,
  strict_name: Option(String),
) -> P {
  let #(sb, _id) = scope.sb_push(p.sb, scope.Function)
  P(
    ..p,
    ctx: Ctx(
      strict: p.ctx.strict,
      // A function's parameters and body are always [+In] — an enclosing
      // `for` head's no-`in` restriction stops at the boundary
      // (`for (let x = function(){ return 'a' in {} };;) …` is legal).
      // Arrows override this: their ConciseBody is [?In] (see
      // `enter_arrow_context`).
      allow_in: True,
      function_depth: p.ctx.function_depth + 1,
      loop_depth: 0,
      switch_depth: 0,
      label_set: [],
      in_generator: is_generator,
      in_async: is_async,
      in_static_block: False,
      in_class_field_init: False,
      in_method: False,
      allow_new_target: True,
      allow_super_call: False,
      allow_super_property: False,
      // A function boundary is never part of an enclosing let/const
      // declaration: params and body bindings must not collide with outer
      // declarator names, so `binding_kind` resets to BindingNone (which,
      // not being BindingLexical, carries no `bound` names to collide with).
      binding_kind: BindingNone,
      in_block: False,
      module_top_level: False,
      in_single_stmt_pos: False,
      // A function body neither inherits nor exports the enclosing
      // expression's deferred cover-grammar errors, and starts with an
      // empty formal-parameter list of its own.
      has_cover_initializer: False,
      dup_proto_pos: None,
      in_formal_params: False,
      in_arrow_params: False,
      has_non_simple_param: False,
      param_bound_names: [],
      pending_strict_name: strict_name,
      in_export_decl: False,
      in_case_clause: False,
    ),
    // Push a fresh Function scope; the new scope's id is now sb.current
    // and sb.current_fn.
    sb:,
  )
}

/// Arrow-function context: a function boundary, but unlike full functions
/// arrows inherit `super`/`new.target` from the enclosing scope. Arrows also
/// have no own `arguments`, so the class-field-init / static-block
/// `arguments` restriction (ContainsArguments) survives the boundary —
/// whereas §15.3.5 Contains does NOT descend into arrows for `await`, so
/// `in_static_block` is folded into `in_class_field_init` (which only drives
/// the `arguments` check) rather than restored.
fn enter_arrow_context(p: P, is_async: Bool, param_names: List(String)) -> P {
  let inner = enter_function_context(p, False, is_async, None)
  // Mark the new function scope as an arrow so finalize knows it does NOT
  // own lexical pseudo-slots; declare each param into the arrow's scope
  // (try_single_ident_arrow reads the bare identifier before the scope is
  // pushed, so it doesn't go through register_scope_binding; try_paren_arrow
  // pushes the scope first and passes [] here — its params are declared via
  // register_scope_binding during parse_formal_parameters).
  let sb =
    scope.sb_update_current_fn(inner.sb, fn(fi) {
      scope.RawFunctionInfo(..fi, is_arrow: True)
    })
  let sb =
    list.fold(param_names, sb, fn(acc, name) {
      scope.sb_declare(acc, name, scope.ParamBinding, synthetic: False)
    })
  P(
    ..inner,
    sb:,
    ctx: Ctx(
      ..inner.ctx,
      allow_super_call: p.ctx.allow_super_call,
      allow_super_property: p.ctx.allow_super_property,
      allow_new_target: p.ctx.allow_new_target,
      in_class_field_init: p.ctx.in_class_field_init || p.ctx.in_static_block,
      // §15.3 ArrowFunction[?In]: the ConciseBody inherits [In] from the
      // enclosing expression, so `for (let f = a => 'a' in {};;)` is a
      // SyntaxError just as `for (let x = 'a' in {};;)` is. (Arrow PARAMS
      // are still [+In] — `try_paren_arrow` sets that for their duration.)
      allow_in: p.ctx.allow_in,
    ),
  )
}

fn enter_method_context(
  p: P,
  is_generator: Bool,
  is_async: Bool,
  is_constructor: Bool,
  has_super_class: Bool,
) -> P {
  let inner = enter_function_context(p, is_generator, is_async, None)
  P(
    ..inner,
    ctx: Ctx(
      ..inner.ctx,
      allow_super_call: is_constructor && has_super_class,
      allow_super_property: True,
      in_method: True,
    ),
  )
}

/// §15.7.1 ClassStaticBlock — function-like boundary: resets loop/switch/
/// labels (break/continue can't cross it), forbids `return` (function_depth=0),
/// `[~Yield, +Await, ~Return]` parsing context. `in_async: True` reserves
/// `await` as identifier via existing checks; `in_static_block: True` rejects
/// AwaitExpression and `arguments` reference.
fn enter_static_block_context(p: P) -> P {
  // enter_function_context pushes a Function scope; for §15.7.14 a static
  // block is its own ClassStaticBlock function-kind scope, so re-tag it.
  let inner = enter_function_context(p, False, True, None)
  let sb =
    scope.sb_update_current(inner.sb, fn(s) {
      scope.RawScope(..s, kind: scope.ClassStaticBlock, is_strict: True)
    })
  P(
    ..inner,
    sb:,
    ctx: Ctx(
      ..inner.ctx,
      function_depth: 0,
      in_static_block: True,
      allow_super_property: True,
    ),
  )
}

fn restore_context_fn(
  res: Result(#(P, List(ast.Pattern), List(ast.StmtWithLine)), ParseError),
  outer: P,
) -> Result(#(P, List(ast.Pattern), List(ast.StmtWithLine)), ParseError) {
  use #(p, params, body) <- result.map(res)
  #(restore_outer_context(p, outer), params, body)
}

/// Leave a function body: put back the enclosing parsing context WHOLE.
///
/// `ctx: outer.ctx` restores every nesting-sensitive flag/counter at once —
/// including `strict`, which a body's retroactive `"use strict"` directive
/// may have flipped and which must NOT leak into the enclosing sloppy code
/// (`function f(){"use strict"} with({}) {}` is legal), the formal-parameter
/// state, and the deferred cover-grammar errors the enclosing expression may
/// still owe (`({a = 1}, () => {})` is a SyntaxError). Because the whole
/// `Ctx` record is copied there is no per-field restore list to keep in
/// sync with `Ctx`'s (and `enter_function_context`'s) field set.
///
/// The scope builder is NOT part of `Ctx`: its accumulated scopes/refs must
/// flow forward across the close, so only the cursor moves back to
/// `outer.sb.current` (`sb_enter` re-derives `current_fn` from it).
fn restore_outer_context(p: P, outer: P) -> P {
  P(..p, ctx: outer.ctx, sb: scope.sb_enter(p.sb, outer.sb.current))
}

// ---- Label helpers ----

/// Look up a label in the enclosing label set. Returns the kind of the
/// innermost label with that name, or None if no such label is in scope.
fn find_label(
  labels: List(#(String, LabelKind)),
  name: String,
) -> Option(LabelKind) {
  case labels {
    [] -> None
    [#(n, kind), ..] if n == name -> Some(kind)
    [_, ..rest] -> find_label(rest, name)
  }
}

// ---- Utilities ----

fn peek(p: P) -> TokenKind {
  case p.tokens {
    [lexer.Token(kind: k, ..), ..] -> k
    [] -> Eof
  }
}

/// The `n`-th upcoming token (0 = the current token). Depths >= 1 are a
/// bounded, pure lookahead that LEXES ahead on demand without retaining
/// the result — the ordinary scan never runs ahead of the parse (see
/// `advance`), so nothing is ever lexed past a construct the parser
/// re-scans from source (a regex literal's body, a template span) unless
/// a grammar rule explicitly looks there. The grammar's deepest lookahead
/// is 3 (`export async function *` name extraction). At end of input the
/// Eof token is returned for every deeper `n`.
fn upcoming(p: P, n: Int) -> Token {
  look_at(look_from(p), n)
}

fn peek_at(p: P, n: Int) -> TokenKind {
  case n {
    0 -> peek(p)
    _ -> {
      let lexer.Token(kind: k, ..) = upcoming(p, n)
      k
    }
  }
}

/// Parse an entire template literal starting at the current
/// TemplateLiteral (no-substitution) or TemplateHead token: its RAW quasi
/// texts (line-ending normalized, escapes undecoded — cooking is the
/// caller's job) and its substitution expressions, with the parser
/// advanced past the closing backtick.
///
/// §13.2.8: each substitution is Expression[+In]. Its tokens come from the
/// ordinary on-demand scanner (a TemplateHead token ends just past `${`,
/// so the window continues INSIDE the substitution), the ordinary grammar
/// parses it — strings, objects, regexes and nested templates in it are
/// real tokens — and at its closing `}` the next span is re-scanned from
/// source (`template_continuation`): the token grammar alone cannot know
/// that a `}` re-enters a template.
fn parse_template_spans(
  p: P,
) -> Result(#(P, ast.TemplateParts(String)), ParseError) {
  case peek(p) {
    // `…` — complete, no substitutions.
    TemplateLiteral ->
      Ok(#(
        advance(p),
        ast.TemplateParts(head: template_span_raw(p, 1), tail: []),
      ))
    // `…${ — one or more substitutions follow. The last-expression flags
    // and the enclosing [In] context are none of the substitutions'
    // business: restore them once the whole template is consumed.
    _ -> {
      let saved_assignable = p.last_expr_assignable
      let saved_is_assignment = p.last_expr_is_assignment
      let head = template_span_raw(p, 2)
      use #(p, rev_tail) <- result.map({
        use p <- with_allow_in(advance(p), True)
        parse_template_substitutions(p, [])
      })
      #(
        P(
          ..p,
          last_expr_assignable: saved_assignable,
          last_expr_is_assignment: saved_is_assignment,
        ),
        ast.TemplateParts(head:, tail: list.reverse(rev_tail)),
      )
    }
  }
}

/// Parse one `${ Expression }` — the parser sits on the first token INSIDE
/// the substitution — plus the template span at its `}`, recursing while
/// spans keep ending in `${`. Returns the `#(expression, quasi)` pairs of
/// `TemplateParts.tail`, in reverse source order.
fn parse_template_substitutions(
  p: P,
  rev_tail: List(#(ast.Expression, String)),
) -> Result(#(P, List(#(ast.Expression, String))), ParseError) {
  // [In] is already forced on for the whole template by the `with_allow_in`
  // in `parse_template_spans`.
  use #(p, expr) <- result.try(parse_expression(
    P(..p, last_expr_assignable: False, last_expr_is_assignment: False),
  ))
  case peek(p) {
    RightBrace -> {
      let p = template_continuation(p)
      case peek(p) {
        // }…${ — another substitution follows.
        TemplateHead ->
          parse_template_substitutions(advance(p), [
            #(expr, template_span_raw(p, 2)),
            ..rev_tail
          ])
        // }…` — the template ends.
        TemplateLiteral ->
          Ok(#(advance(p), [#(expr, template_span_raw(p, 1)), ..rev_tail]))
        // The continuation hit end of input: unterminated template.
        _ -> Error(UnterminatedTemplateSubstitution(pos_of(p)))
      }
    }
    other ->
      Error(error_at_current(p, ExpectedToken(pos_of(p), RightBrace, other)))
  }
}

/// At the `}` closing a template substitution, re-scan the next template
/// span from source and make it the current token. The token window past
/// the `}` is garbage — it was lexed without knowing that the `}`
/// re-enters a template — so it is discarded, exactly like the window
/// past a re-scanned regex literal.
fn template_continuation(p: P) -> P {
  let #(token, scan) =
    lexer.scan_template_continuation(
      p.bytes,
      pos_of(p),
      line_of(p),
      p.scan.mode,
    )
  P(..p, tokens: [token], scan:)
}

/// The current template-span token's RAW quasi text (§12.9.6 TRV): the
/// source between its delimiters — one leading char (`` ` `` or `}`) and
/// `trailing` chars (1 for a closing `` ` ``, 2 for `${`), all ASCII, so
/// the slice is byte-exact — with line-terminator sequences normalized
/// (<CR><LF> and <CR> → <LF>).
fn template_span_raw(p: P, trailing: Int) -> String {
  let assert Some(raw) =
    source_bytes.slice(p.bytes, pos_of(p) + 1, peek_raw_len(p) - 1 - trailing)
    as "parser: template quasi slice out of range"
  raw
  |> string.replace("\r\n", "\n")
  |> string.replace("\r", "\n")
}

fn peek_value(p: P) -> String {
  case p.tokens {
    [lexer.Token(value: v, ..), ..] -> v
    [] -> ""
  }
}

fn peek_value_at(p: P, n: Int) -> String {
  case n {
    0 -> peek_value(p)
    _ -> {
      let lexer.Token(kind: kind, value: v, ..) = upcoming(p, n)
      case kind {
        Eof -> ""
        _ -> v
      }
    }
  }
}

/// True if the current token's source contained a unicode escape. A contextual
/// keyword (get/set/async/of/static/…) written with an escape must not be
/// recognized as that keyword (ES2024 §12.7.2 / §13.1).
fn peek_had_escape(p: P) -> Bool {
  case p.tokens {
    [lexer.Token(had_escape: e, ..), ..] -> e
    [] -> False
  }
}

/// True if the current token used one of the Annex B legacy forms strict code
/// forbids: a leading-zero numeric literal (`010`, `08`) on a `Number` token,
/// or a legacy octal / `\8` / `\9` escape on a `KString` token. The lexer
/// decides this while scanning the token — see `lexer.Token.annex_b_legacy`.
fn peek_annex_b_legacy(p: P) -> Bool {
  case p.tokens {
    [lexer.Token(annex_b_legacy: legacy, ..), ..] -> legacy
    [] -> False
  }
}

fn peek_raw_len(p: P) -> Int {
  case p.tokens {
    [lexer.Token(raw_len: rl, ..), ..] -> rl
    [] -> 0
  }
}

fn pos_of(p: P) -> Int {
  case p.tokens {
    [lexer.Token(pos: pos, ..), ..] -> pos
    [] -> 0
  }
}

/// Half-open byte span [pos, pos + raw_len) of the current token. Used to
/// attach source spans to import/export local-binding identifiers so the
/// bundler can map them back to the original source. Slicing the source by
/// this span returns the identifier's exact original text.
fn span_of(p: P) -> ast.Span {
  case p.tokens {
    [lexer.Token(pos: pos, raw_len: raw_len, ..), ..] ->
      ast.Span(start: pos, end: pos + raw_len)
    [] -> ast.Span(start: 0, end: 0)
  }
}

/// Byte offset just past the last token consumed between `before` and `after`.
/// Used to close a `Span` whose start was captured before parsing: the parser
/// state carries no end position, so we recover it from the consumed-token
/// delta. Falls back to the next token's start (or 0) when nothing was consumed.
fn consumed_end(before: P, after: P) -> Int {
  // `advance` records each consumed token's end (pos + raw_len) in
  // `prev_end`, so the end of the last token consumed between `before` and
  // `after` is simply `after.prev_end` — O(1), no token-stream traversal.
  // Token positions strictly increase, so an unchanged `prev_end` means no
  // token was consumed; fall back to the next token's start in that case.
  case after.prev_end == before.prev_end {
    True -> pos_of(after)
    False -> after.prev_end
  }
}

/// Close a multi-token expression span: `start` is the byte captured before
/// descent — `left.span.start` for left-recursive productions (binary, call,
/// member chains), `pos_of(p_before)` for prefix productions (unary, `new`,
/// bracketed literals) — and the end is `p_after.prev_end`, the byte just
/// past the last token the production consumed.
fn span_from(start: Int, p_after: P) -> ast.Span {
  ast.Span(start:, end: p_after.prev_end)
}

/// 1-based source line of the current token, used to tag parsed statements
/// for stack traces. Falls back to the previous token's line at EOF.
fn line_of(p: P) -> Int {
  case p.tokens {
    [lexer.Token(line: line, ..), ..] -> line
    [] -> p.prev_line
  }
}

/// Consume the current token and lex the next one (when none is buffered
/// already — after a template continuation the tail span is).
///
/// NOTHING is ever lexed beyond the token the parse is at, except through
/// an explicit bounded lookahead (`upcoming`), which retains nothing. That
/// is the invariant the on-demand design rests on (QuickJS / V8 keep at
/// most one pending token for the same reason): the ordinary token scan
/// never touches source the parser re-scans and jumps over — a regex
/// literal's body, a template span after a substitution's `}` — so no
/// garbage token, however expensive its scan would be, is ever produced.
fn advance(p: P) -> P {
  case p.tokens {
    [lexer.Token(line: line, pos: pos, raw_len: rl, ..), ..rest] ->
      case rest {
        [] -> {
          let #(token, scan) = lexer.scan_next(p.scan)
          P(..p, tokens: [token], scan:, prev_line: line, prev_end: pos + rl)
        }
        _ -> P(..p, tokens: rest, prev_line: line, prev_end: pos + rl)
      }
    [] -> p
  }
}

/// Ensure the current token is present (lexing exactly one if the window
/// is empty): used to build the window at parser start and after a source
/// re-scan (regex literal, template continuation) discarded it.
///
/// A hard lexer error is materialised by the lexer as a zero-length
/// `LexFailure` token (carrying the typed LexError) followed by Eof: no
/// grammar production accepts it, so the parse fails at exactly the lexer
/// error's position with its message (see illegal_token_error) — and
/// errors inside source the parser jumps over (a regex body) never
/// surface at all.
fn ensure_current(p: P) -> P {
  case p.tokens {
    [] -> {
      let #(token, scan) = lexer.scan_next(p.scan)
      P(..p, tokens: [token], scan:)
    }
    _ -> p
  }
}

fn expect(p: P, kind: TokenKind) -> Result(P, ParseError) {
  case peek(p) == kind {
    True -> Ok(advance(p))
    False ->
      case peek(p) {
        Illegal | LexFailure(_) -> Error(illegal_token_error(p))
        found -> Error(ExpectedToken(pos_of(p), kind, found))
      }
  }
}

fn expect_identifier(p: P) -> Result(P, ParseError) {
  case peek(p) {
    Identifier -> Ok(advance(p))
    Illegal | LexFailure(_) -> Error(illegal_token_error(p))
    _ ->
      case is_keyword_as_identifier(peek(p)) {
        True -> Ok(advance(p))
        False -> Error(ExpectedIdentifier(pos_of(p)))
      }
  }
}

/// The error for the current token being one of the two illegal-token kinds.
/// The on-demand lexer's hard-error sentinel (see ensure_current) carries the
/// lexer's own typed `LexError` on its KIND (`LexFailure`), so it is reported
/// exactly as the old whole-file lex pass reported it. A lenient `Illegal`
/// token (a stray character the lexer tolerates because a regex body could
/// have made it legal) has no error to carry and keeps the generic
/// unexpected-token report.
fn illegal_token_error(p: P) -> ParseError {
  case peek(p) {
    LexFailure(err) -> lex_error(err)
    kind -> UnexpectedToken(pos_of(p), kind)
  }
}

/// Report an unexpected current token: the LEXER's own message when the
/// token is a hard-lex-error sentinel (see illegal_token_error), the
/// caller's grammar-specific `otherwise` error for a real token. Every
/// site that rejects an arbitrary unexpected token should go through
/// this, or a lexer error behind it gets masked by a misleading
/// "expected X" report.
fn error_at_current(p: P, otherwise: ParseError) -> ParseError {
  case peek(p) {
    Illegal | LexFailure(_) -> illegal_token_error(p)
    _ -> otherwise
  }
}

fn eat_semicolon(p: P) -> Result(P, ParseError) {
  case peek(p) {
    Semicolon -> Ok(advance(p))
    RightBrace | Eof -> Ok(p)
    // A hard lexer error right after the statement (see ensure_current):
    // when ASI does not save it, report the LEXER's message, not a
    // misleading "Expected ';'" pointing at it.
    Illegal | LexFailure(_) ->
      case has_line_break_before(p) {
        True -> Ok(p)
        False -> Error(illegal_token_error(p))
      }
    _ ->
      // ASI: insert semicolon if there's a line break before the current token
      case has_line_break_before(p) {
        True -> Ok(p)
        False -> Error(ExpectedSemicolon(pos_of(p)))
      }
  }
}

/// Check if there's a line break between the previous token and the current token.
/// Used for ASI (Automatic Semicolon Insertion) and restricted productions
/// (return, throw, break, continue, postfix ++/--).
fn has_line_break_before(p: P) -> Bool {
  case p.tokens {
    [lexer.Token(line: current_line, ..), ..] -> current_line > p.prev_line
    [] -> True
  }
}

/// Source line of the n-th upcoming token (-1 past EOF). Used for the
/// `let`-ASI lookahead in single-statement position.
fn token_line_at(p: P, n: Int) -> Int {
  let lexer.Token(kind: kind, line: line, ..) = upcoming(p, n)
  case kind {
    // Past the end of input every deeper token is the Eof token; keep the
    // old answer for a missing token so line comparisons never pair with
    // it.
    Eof -> -1
    _ -> line
  }
}

/// Contextual keywords that can be used as identifiers in non-strict mode.
/// Try to consume an optional name (identifier or contextual keyword) for
/// function/class declarations. Returns advanced state if found, original if not.
fn eat_optional_name(p: P) -> Result(P, ParseError) {
  let is_name = peek(p) == Identifier || is_contextual_keyword(peek(p))
  case is_name {
    True -> {
      let name = peek_value(p)
      use Nil <- result.try(check_binding_identifier(p, name))
      Ok(advance(p))
    }
    False -> Ok(p)
  }
}
