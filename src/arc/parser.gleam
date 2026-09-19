import arc/bytecode/lexical
import arc/compiler/ast_util
import arc/compiler/scope
import arc/compiler/scope_builder
import arc/internal/bytes
import arc/module/summary
import arc/parser/ast
import arc/parser/class_scopes
import arc/parser/error.{
  ArgumentsInClassFieldInit, ArgumentsInStaticBlock, AwaitInAsyncFunction,
  AwaitInFormalParameter, AwaitInModule, AwaitInStaticBlock,
  BreakOutsideLoopOrSwitch, ClassConstructorAsync, ClassConstructorGenerator,
  ClassConstructorNotGetter, ClassConstructorNotSetter,
  ClassDuplicateConstructor, CoalesceMixedWithLogical, ContinueOutsideLoop,
  ContinueToNonIterationLabel, DeletePrivateName, DeleteUnqualifiedStrictMode,
  DestructuringMissingInitializer, DuplicateBindingLexical, DuplicateDefaultCase,
  DuplicateExport, DuplicateImportBinding, DuplicateLabel,
  DuplicateParamNameStrictMode, DuplicateParameterName, DuplicatePrivateName,
  DuplicateProtoProperty, EnumReservedWord, EscapedReservedWord,
  EvalArgsAssignStrictMode, ExpectedAfterOptionalChain,
  ExpectedAsOrFromAfterExportStar, ExpectedBindingPattern,
  ExpectedBraceOrStarAfterComma, ExpectedCallOrDotAfterImport,
  ExpectedCaseDefaultOrBrace, ExpectedCloseAfterSetter,
  ExpectedCommaOrBraceInExport, ExpectedCommaOrBraceInImport,
  ExpectedCommaOrBraceInObject, ExpectedCommaOrBracket,
  ExpectedCommaOrBracketInExpr, ExpectedCommaOrCloseParen,
  ExpectedCommaOrObjectClose, ExpectedExportAlias, ExpectedExportSpecifierName,
  ExpectedForDeclSeparator, ExpectedForHeadSeparator, ExpectedForSeparator,
  ExpectedFromOrComma, ExpectedFunctionAfterAsync, ExpectedIdentifier,
  ExpectedIdentifierAfterDot, ExpectedImportMeta, ExpectedImportSpecifier,
  ExpectedImportSpecifierName, ExpectedModuleSpecifier, ExpectedNewTarget,
  ExpectedPropertyName, ExpectedSemicolon, ExpectedToken, ExportNotTopLevel,
  FieldNamedConstructor, ForInInitializer, ForOfInitializer,
  FunctionDeclInLabelBody, FunctionDeclInSingleStatement, GeneratorDeclLabeled,
  GetterNoParams, IdentifierAlreadyDeclared, ImportMetaOutsideModule,
  ImportNotTopLevel, InvalidAssignmentLhs, InvalidDestructuringTarget,
  InvalidForInLhs, InvalidForOfLhs, InvalidLhsPrefixOp, InvalidPostfixLhs,
  InvalidRestBinding, InvalidTemplateEscape, LetBindingInLexicalDecl,
  LetIdentifierStrictMode, LexicalDeclInLabel, LexicalDeclInSingleStatement,
  MalformedNumericLiteral, MisplacedUseStrictDirective, MissingCatchOrFinally,
  MissingConstInitializer, NewTargetOutsideFunction, OctalEscapeStrictMode,
  OctalLiteralStrictMode, PrivateNameAsPropertyKey, PrivateNameConstructor,
  PrivateNameNotInBrandCheck, ReservedWordImportBinding, ReservedWordStrictMode,
  RestDefaultInitializer, RestMustBeLast, RestTrailingComma,
  ReturnOutsideFunction, SetterExactlyOneParam, SetterNoRest,
  ShorthandDefaultOutsideDestructuring, StaticPrototype,
  StaticReservedStrictMode, StrictModeAssignment, StrictModeBindingName,
  StrictModeModification, StrictModeParamName, SuperCallNotInDerivedConstructor,
  SuperPrivateName, SuperPropertyNotInMethod, TemplateInOptionalChain,
  ThrowLineBreak, UnaryBeforeExponentiation, UndeclaredExportBinding,
  UndeclaredPrivateName, UndefinedLabel, UnexpectedAfterExport,
  UnexpectedCloseBrace, UnexpectedCloseParen, UnexpectedExport, UnexpectedSuper,
  UnexpectedToken, UnicodeEscapeInMetaProperty, UnterminatedTemplateSubstitution,
  UsingAtScriptTopLevel, UsingInCaseClause, UsingInForIn,
  UsingMissingInitializer, UsingPatternBinding, WithNotAllowedStrictMode,
  YieldInFormalParameter, YieldInGenerator, YieldReservedStrictMode, lex_error,
  regexp_syntax_error,
}
import arc/parser/lexer
import arc/parser/number
import arc/parser/regex
import arc/parser/token.{
  type Token, type TokenKind, AmpersandAmpersandEqual, AmpersandEqual, Arrow, As,
  Async, Await, Bang, Binary, BinaryOperator, Break, CaretEqual, Case, Catch,
  Class, Coalesce, Colon, Comma, Const, Continue, Debugger, Default, Delete, Do,
  Dot, DotDotDot, Else, Eof, Equal, Export, Extends, FalseLiteral, Finally, For,
  From, Function, GreaterThanGreaterThanEqual,
  GreaterThanGreaterThanGreaterThanEqual, Identifier, If, Illegal, Import, In,
  LeftBrace, LeftBracket, LeftParen, LessThanLessThanEqual, Let, LexFailure,
  Minus, MinusEqual, MinusMinus, New, Null, Number, Of, PercentEqual, PipeEqual,
  PipePipeEqual, Plus, PlusEqual, PlusPlus, Question, QuestionDot,
  QuestionQuestionEqual, Return, RightBrace, RightBracket, RightParen, Semicolon,
  ShortCircuit, Slash, SlashEqual, Star, StarEqual, StarStar, StarStarEqual,
  Static, StringLiteral, Super, Switch, TemplateHead, TemplateLiteral, This,
  Throw, Tilde, TrueLiteral, Try, Typeof, Undefined, Var, Void, While, With,
  Yield, assignment_op, binary_operator, is_contextual_keyword,
  is_identifier_or_keyword, is_keyword_as_identifier, is_reserved_word_kind,
}
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

// §12.9.4 cook string escapes
@external(erlang, "arc_parser_ffi", "decode_string_escapes")
fn decode_string_escapes(raw: String) -> String

// §12.9.6 template value; Error(Nil) on an invalid escape
@external(erlang, "arc_parser_ffi", "cook_template_string")
fn cook_template_string(raw: String) -> Result(String, Nil)

pub type ParseMode {
  Script
  Module
}

pub type ParseError =
  error.ParseError

pub fn error_to_string(err: ParseError) -> String {
  error.to_string(err)
}

type DeclarationContext {
  NotDeclaring
  DeclaringVar
  // bound_so_far: §14.3.1 BoundNames of this BindingList
  DeclaringLexical(kind: scope.BindingKind, bound_so_far: Set(String))
  DeclaringParam
}

fn in_lexical_declaration(ctx: GrammarContext) -> Bool {
  case ctx.declaring {
    DeclaringLexical(..) -> True
    NotDeclaring | DeclaringVar | DeclaringParam -> False
  }
}

// continue L needs a loop label (§14.9.1)
type LabelKind {
  LoopLabel
  PlainLabel
}

type LabelUse {
  BreakLabel
  ContinueLabel
}

type AccessorPrefix {
  NoAccessor
  GetPrefix
  SetPrefix
}

type MethodPrefix {
  MethodPrefix(is_async: Bool, accessor: AccessorPrefix, is_generator: Bool)
}

type AssignmentKind {
  PlainAssign
  CallTargetAssign
  CompoundAssign
}

type PrivateNameRef {
  PrivateNameRef(name: String, class_depth: Int, pos: Int)
}

// saved and restored whole at function boundaries, see restore_outer_context
type GrammarContext {
  GrammarContext(
    strict: Bool,
    // grammar [In] flag, false only in a for head init
    allow_in: Bool,
    function_depth: Int,
    loop_depth: Int,
    switch_depth: Int,
    label_set: List(#(String, LabelKind)),
    in_generator: Bool,
    in_async: Bool,
    // §15.7.1: also forbids await expr, unlike in_async
    in_static_block: Bool,
    // §15.7.10 ContainsArguments; arrows inherit it
    in_class_field_init: Bool,
    in_method: Bool,
    // arrows bump function_depth but do not enable new.target
    allow_new_target: Bool,
    allow_super_call: Bool,
    allow_super_property: Bool,
    declaring: DeclarationContext,
    in_block: Bool,
    module_top_level: Bool,
    in_single_stmt_pos: Bool,
    // deferred cover-grammar errors, see check_cover_grammar_errors
    has_cover_initializer: Bool,
    dup_proto_pos: Option(Int),
    in_formal_params: Bool,
    in_catch_param: Bool,
    in_arrow_params: Bool,
    has_non_simple_param: Bool,
    param_bound_names: List(String),
    pending_strict_name: Option(String),
    in_export_decl: Bool,
    in_case_clause: Bool,
  )
}

type Parser {
  Parser(
    // bounded prefetch window, not the whole file
    tokens: List(Token),
    scan: lexer.Scanner,
    mode: ParseMode,
    prev_line: Int,
    prev_end: Int,
    bytes: BitArray,
    ctx: GrammarContext,
    // §15.7.1 enclosing class body count, not reset by functions
    class_body_depth: Int,
    // unresolved #name refs
    private_refs: List(PrivateNameRef),
    // direct eval only: the caller's private names (§19.2.1.1 step 5)
    outer_private_names: List(String),
    last_expr_assignable: Bool,
    // tells {a: b = 1} (pattern ok) from {a: 0}
    last_expr_is_assignment: Bool,
    literal_invalid_as_pattern: Bool,
    export_names: Set(String),
    export_local_refs: List(#(String, Int)),
    import_bindings: Set(String),
    last_expr_name: Option(String),
    scopes: scope_builder.ScopeBuilder,
  )
}

// [+In] for the duration of then
fn allowing_in(
  p: Parser,
  then: fn(Parser) -> Result(#(Parser, a), ParseError),
) -> Result(#(Parser, a), ParseError) {
  with_allow_in(p, allow_in: True, then:)
}

// [~In] for the duration of then, as in a for head
fn disallowing_in(
  p: Parser,
  then: fn(Parser) -> Result(#(Parser, a), ParseError),
) -> Result(#(Parser, a), ParseError) {
  with_allow_in(p, allow_in: False, then:)
}

fn with_allow_in(
  p: Parser,
  allow_in allow_in: Bool,
  then then: fn(Parser) -> Result(#(Parser, a), ParseError),
) -> Result(#(Parser, a), ParseError) {
  let saved = p.ctx.allow_in
  use <- bool.lazy_guard(saved == allow_in, fn() { then(p) })
  use #(p, parsed) <- result.map(then(
    Parser(..p, ctx: GrammarContext(..p.ctx, allow_in:)),
  ))
  #(Parser(..p, ctx: GrammarContext(..p.ctx, allow_in: saved)), parsed)
}

fn set_not_assignable(
  res: Result(#(Parser, ast.Expression), ParseError),
) -> Result(#(Parser, ast.Expression), ParseError) {
  use #(p, expr) <- result.map(res)
  #(
    Parser(..p, last_expr_assignable: False, last_expr_is_assignment: False),
    expr,
  )
}

fn accept_literal(
  p: Parser,
  expr: ast.Expression,
) -> Result(#(Parser, ast.Expression), ParseError) {
  Ok(#(Parser(..advance(p), last_expr_assignable: False), expr))
}

type DefaultExportDecl {
  DefaultFn(function: ast.FunctionLiteral)
  DefaultClass(
    name: Option(ast.NamedBinding),
    super_class: Option(ast.Expression),
    body: List(ast.ClassElement),
  )
}

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

fn default_export_name(decl: DefaultExportDecl) -> Option(ast.NamedBinding) {
  case decl {
    DefaultFn(function:) -> function.name
    DefaultClass(name:, ..) -> name
  }
}

fn init_parser(
  source: String,
  mode: ParseMode,
  cont: fn(Parser) -> Result(a, ParseError),
) -> Result(a, ParseError) {
  let bytes = bit_array.from_string(source)
  let #(source_kind, code_kind) = case mode {
    Module -> #(lexer.ModuleSource, scope.Module)
    Script -> #(lexer.ScriptSource, scope.Script)
  }
  cont(
    ensure_current(Parser(
      tokens: [],
      scan: lexer.scanner_at(bytes, 0, 1, source_kind),
      mode:,
      prev_line: 1,
      prev_end: 0,
      bytes:,
      ctx: GrammarContext(
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
        declaring: NotDeclaring,
        in_block: False,
        module_top_level: False,
        in_single_stmt_pos: False,
        has_cover_initializer: False,
        dup_proto_pos: None,
        in_formal_params: False,
        in_catch_param: False,
        in_arrow_params: False,
        has_non_simple_param: False,
        param_bound_names: [],
        pending_strict_name: None,
        in_export_decl: False,
        in_case_clause: False,
      ),
      class_body_depth: 0,
      private_refs: [],
      outer_private_names: [],
      last_expr_assignable: False,
      last_expr_is_assignment: False,
      literal_invalid_as_pattern: False,
      export_names: set.new(),
      export_local_refs: [],
      import_bindings: set.new(),
      last_expr_name: None,
      scopes: scope_builder.init(code_kind),
    )),
  )
}

pub fn parse(
  source: String,
  mode: ParseMode,
) -> Result(#(ast.Program, scope_builder.ScopeBuilder), ParseError) {
  case mode {
    Script -> {
      use #(body, scopes) <- result.map(parse_script(source))
      #(ast.Script(body:), scopes)
    }
    Module -> {
      use #(items, scopes) <- result.map(parse_module(source))
      #(ast.Module(body: items), scopes)
    }
  }
}

pub fn parse_script(
  source: String,
) -> Result(#(List(ast.StmtWithLine), scope_builder.ScopeBuilder), ParseError) {
  use p <- init_parser(source, Script)
  script_body(p)
}

pub fn parse_module(
  source: String,
) -> Result(#(List(ast.ModuleItem), scope_builder.ScopeBuilder), ParseError) {
  use p <- init_parser(source, Module)
  module_body(p)
}

pub fn parse_direct_eval(
  source: String,
  strict strict: Bool,
  allow_new_target allow_new_target: Bool,
  allow_super_property allow_super_property: Bool,
  allow_super_call allow_super_call: Bool,
  allow_arguments allow_arguments: Bool,
  outer_private_names outer_private_names: List(String),
) -> Result(#(List(ast.StmtWithLine), scope_builder.ScopeBuilder), ParseError) {
  use p <- init_parser(source, Script)
  script_body(
    Parser(
      ..p,
      ctx: GrammarContext(
        ..p.ctx,
        strict:,
        allow_new_target:,
        allow_super_property:,
        allow_super_call:,
        in_class_field_init: !allow_arguments,
      ),
      outer_private_names:,
    ),
  )
}

fn script_body(
  p: Parser,
) -> Result(#(List(ast.StmtWithLine), scope_builder.ScopeBuilder), ParseError) {
  use p <- result.try(apply_directive_prologue(p))
  use #(p_final, stmts) <- result.try(
    parse_statement_list(p, top_level: True, acc: []),
  )
  use Nil <- result.try(check_unresolved_private_refs(p_final))
  let scopes =
    scope_builder.reorder_block_children(p_final.scopes, scope.root_scope_id)
  Ok(#(stmts, scopes))
}

fn module_body(
  p: Parser,
) -> Result(#(List(ast.ModuleItem), scope_builder.ScopeBuilder), ParseError) {
  use #(p_final, items) <- result.try(parse_module_body(p, []))
  use Nil <- result.try(validate_export_local_refs(p_final))
  use Nil <- result.try(check_unresolved_private_refs(p_final))
  let scopes =
    scope_builder.reorder_block_children(p_final.scopes, scope.root_scope_id)
  Ok(#(items, scopes))
}

fn parse_module_body(
  p: Parser,
  acc: List(ast.ModuleItem),
) -> Result(#(Parser, List(ast.ModuleItem)), ParseError) {
  let p = Parser(..p, ctx: GrammarContext(..p.ctx, module_top_level: True))
  let statement_item = fn() {
    use #(p2, stmt) <- result.map(parse_statement(p))
    #(p2, ast.StatementItem(ast.StmtWithLine(line_of(p), stmt)))
  }
  use <- bool.guard(peek(p) == Eof, Ok(#(p, list.reverse(acc))))
  use #(p2, item) <- result.try(case peek(p) {
    Import ->
      case peek_at(p, 1) {
        Dot | LeftParen -> statement_item()
        _ -> parse_import_declaration(p)
      }
    Export -> parse_export_declaration(p)
    _ -> statement_item()
  })
  parse_module_body(p2, [item, ..acc])
}

fn validate_export_local_refs(p: Parser) -> Result(Nil, ParseError) {
  use #(name, pos) <- list.try_each(p.export_local_refs)
  let declared =
    scope_builder.root_has(p.scopes, name)
    || set.contains(p.import_bindings, name)
  case declared {
    True -> Ok(Nil)
    False -> Error(UndeclaredExportBinding(pos, name))
  }
}

fn parse_statement_list(
  p: Parser,
  top_level top_level: Bool,
  acc acc: List(ast.StmtWithLine),
) -> Result(#(Parser, List(ast.StmtWithLine)), ParseError) {
  case peek(p) {
    Eof -> Ok(#(p, list.reverse(acc)))
    RightBrace -> {
      use <- bool.guard(top_level, Error(UnexpectedCloseBrace(pos_of(p))))
      Ok(#(p, list.reverse(acc)))
    }
    _ -> {
      let line = line_of(p)
      use #(p2, stmt) <- result.try(parse_statement(p))
      parse_statement_list(p2, top_level:, acc: [
        ast.StmtWithLine(line, stmt),
        ..acc
      ])
    }
  }
}

// let starts a declaration when a binding could follow
fn let_starts_declaration(p: Parser) -> Bool {
  let next = peek_at(p, 1)
  next == LeftBrace || next == LeftBracket || is_identifier_or_keyword(next)
}

// §13.4: after a label or in single statement position; let [ always
fn let_declaration_forbidden_here(p: Parser) -> Bool {
  peek_at(p, 1) == LeftBracket
  || { let_starts_declaration(p) && token_line_at(p, 1) <= token_line_at(p, 0) }
}

fn parse_statement(p: Parser) -> Result(#(Parser, ast.Statement), ParseError) {
  case peek(p) {
    Illegal | LexFailure(_) -> Error(illegal_token_error(p))
    LeftBrace -> parse_block_statement(p)
    Var | Const -> parse_variable_declaration(p)
    Let ->
      case let_starts_declaration(p) {
        True -> parse_variable_declaration(p)
        False -> parse_expression_statement(p)
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
    Function -> parse_function_statement(p, is_async: False)
    Class -> parse_class_statement(p)
    Semicolon -> Ok(#(advance(p), ast.EmptyStatement))
    Debugger -> {
      use p2 <- result.map(eat_semicolon(advance(p)))
      #(p2, ast.DebuggerStatement)
    }
    With -> parse_with_statement(p)
    Async ->
      case async_function_start(p), peek_at(p, 1) {
        True, _ -> parse_function_statement(p, is_async: True)
        False, Colon -> parse_labeled_statement(p, [])
        False, _ -> parse_expression_statement(p)
      }
    Yield | Await ->
      case peek_at(p, 1) {
        Colon -> parse_labeled_statement(p, [])
        _ ->
          case peek(p) == Await && is_await_using_decl_start(p) {
            True -> parse_using_declaration(p, is_await: True)
            False -> parse_expression_statement(p)
          }
      }
    Import ->
      case peek_at(p, 1), p.mode {
        Dot, _ | LeftParen, _ | _, Script -> parse_expression_statement(p)
        _, Module -> Error(ImportNotTopLevel(pos_of(p)))
      }
    Export ->
      case p.mode {
        Module -> Error(ExportNotTopLevel(pos_of(p)))
        Script -> Error(UnexpectedExport(pos_of(p)))
      }
    Identifier ->
      case peek_at(p, 1) {
        Colon -> parse_labeled_statement(p, [])
        _ ->
          case is_using_decl_start(p, 0) {
            True -> parse_using_declaration(p, is_await: False)
            False -> parse_expression_statement(p)
          }
      }
    _ -> parse_expression_statement(p)
  }
}

// using [ and using { are not declarations
fn is_using_decl_start(p: Parser, at: Int) -> Bool {
  peek_at(p, at) == Identifier
  && peek_value_at(p, at) == "using"
  && token_line_at(p, at + 1) == token_line_at(p, at)
  && is_binding_ident_token(peek_at(p, at + 1))
}

fn is_await_using_decl_start(p: Parser) -> Bool {
  { p.ctx.in_async || p.mode == Module }
  && !p.ctx.in_static_block
  && token_line_at(p, 1) == token_line_at(p, 0)
  && is_using_decl_start(p, 1)
}

fn is_binding_ident_token(kind: TokenKind) -> Bool {
  kind == Identifier || is_contextual_keyword(kind)
}

// script top level, case clauses and single statements reject using
fn check_using_placement(p: Parser) -> Result(Nil, ParseError) {
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

fn parse_using_declaration(
  p: Parser,
  is_await is_await: Bool,
) -> Result(#(Parser, ast.Statement), ParseError) {
  use Nil <- result.try(check_using_placement(p))
  let p2 = case is_await {
    True -> advance(advance(p))
    False -> advance(p)
  }
  let p2 = enter_lexical_decl_context(p2, scope.ConstBinding)
  use #(p3, declarations) <- result.try(parse_using_declarator_list(p2, []))
  use p4 <- result.try(eat_semicolon(exit_declaration_context(p3, p)))
  let kind = case is_await {
    True -> ast.AwaitUsing
    False -> ast.Using
  }
  Ok(#(p4, ast.VariableDeclaration(kind:, declarations:)))
}

fn parse_using_declarator_list(
  p: Parser,
  acc: List(ast.VariableDeclarator),
) -> Result(#(Parser, List(ast.VariableDeclarator)), ParseError) {
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

fn parse_using_binding(
  p: Parser,
) -> Result(#(Parser, ast.Pattern), ParseError) {
  case peek(p) {
    LeftBracket | LeftBrace -> Error(UsingPatternBinding(pos_of(p)))
    kind ->
      case is_binding_ident_token(kind) {
        True -> validate_and_register_binding(p, peek_value(p))
        False -> Error(error_at_current(p, ExpectedBindingPattern(pos_of(p))))
      }
  }
}

fn parse_single_statement(
  p: Parser,
  allow_annex_b_function allow_annex_b_function: Bool,
) -> Result(#(Parser, ast.Statement), ParseError) {
  // must not leak to later statements
  let saved = p.ctx.in_single_stmt_pos
  use #(p_out, stmt) <- result.map(parse_single_statement_in_position(
    Parser(
      ..p,
      ctx: GrammarContext(
        ..p.ctx,
        in_single_stmt_pos: True,
        module_top_level: False,
      ),
    ),
    allow_annex_b_function:,
  ))
  #(
    Parser(..p_out, ctx: GrammarContext(..p_out.ctx, in_single_stmt_pos: saved)),
    stmt,
  )
}

fn parse_single_statement_in_position(
  p: Parser,
  allow_annex_b_function allow_annex_b_function: Bool,
) -> Result(#(Parser, ast.Statement), ParseError) {
  case peek(p) {
    Const -> Error(LexicalDeclInSingleStatement(pos_of(p)))
    Let -> {
      use <- bool.guard(
        let_declaration_forbidden_here(p),
        Error(LexicalDeclInSingleStatement(pos_of(p))),
      )
      case let_starts_declaration(p) {
        True -> parse_expression_statement(p)
        False -> parse_statement(p)
      }
    }
    Function ->
      case allow_annex_b_function && !p.ctx.strict && peek_at(p, 1) != Star {
        // annex b §B.3.3: parse as if wrapped in a block
        True -> {
          let #(scopes, block_id) = scope_builder.push(p.scopes, scope.Block)
          let p_inner =
            Parser(
              ..p,
              scopes:,
              ctx: GrammarContext(
                ..p.ctx,
                in_block: True,
                in_single_stmt_pos: False,
              ),
            )
          use #(p2, stmt) <- result.map(parse_statement(p_inner))
          let scopes =
            scope_builder.close_block(p2.scopes, block_id)
            |> scope_builder.enter(p.scopes.current)
          #(
            Parser(
              ..p2,
              scopes:,
              ctx: GrammarContext(..p2.ctx, in_block: p.ctx.in_block),
            ),
            stmt,
          )
        }
        False -> Error(FunctionDeclInSingleStatement(pos_of(p)))
      }
    // annex b §B.3.3 covers plain functions only
    Async ->
      case async_function_start(p) {
        True -> Error(FunctionDeclInSingleStatement(pos_of(p)))
        False -> parse_statement(p)
      }
    Class -> Error(LexicalDeclInSingleStatement(pos_of(p)))
    _ -> parse_statement(p)
  }
}

fn enter_block_scope(p: Parser) -> Parser {
  let #(scopes, _id) = scope_builder.push(p.scopes, scope.Block)
  Parser(..p, scopes:, ctx: GrammarContext(..p.ctx, in_block: True))
}

fn restore_block_scope(after p: Parser, before saved: Parser) -> Parser {
  // flip for-head children to source order for finalize
  let scopes = scope_builder.reorder_block_children(p.scopes, p.scopes.current)
  Parser(
    ..p,
    scopes: scope_builder.enter(scopes, saved.scopes.current),
    ctx: GrammarContext(..p.ctx, in_block: saved.ctx.in_block),
  )
}

fn parse_block_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  use #(p2, stmts) <- result.map(parse_block_body(p))
  #(p2, ast.BlockStatement(body: stmts))
}

fn parse_block_body(
  p: Parser,
) -> Result(#(Parser, List(ast.StmtWithLine)), ParseError) {
  // an empty block declares nothing, so no scope push
  case peek(p), peek_at(p, 1) {
    LeftBrace, RightBrace -> {
      let p2 = advance(advance(p))
      Ok(
        #(
          Parser(
            ..p2,
            ctx: GrammarContext(
              ..p2.ctx,
              in_single_stmt_pos: False,
              in_case_clause: False,
            ),
          ),
          [],
        ),
      )
    }
    _, _ -> parse_scoped_block_body(p)
  }
}

fn parse_scoped_block_body(
  p: Parser,
) -> Result(#(Parser, List(ast.StmtWithLine)), ParseError) {
  use p2 <- result.try(expect(p, LeftBrace))
  let #(scopes, block_id) = scope_builder.push(p2.scopes, scope.Block)
  let p_inner =
    Parser(
      ..p2,
      scopes:,
      ctx: GrammarContext(
        ..p2.ctx,
        in_block: True,
        in_single_stmt_pos: False,
        module_top_level: False,
        in_case_clause: False,
      ),
    )
  use #(p3, stmts) <- result.try(
    parse_statement_list(p_inner, top_level: False, acc: []),
  )
  use p4 <- result.try(expect(p3, RightBrace))
  // prune or reorder in lockstep with emit_block
  let scopes =
    scope_builder.close_block(p4.scopes, block_id)
    |> scope_builder.enter(p2.scopes.current)
  Ok(#(
    Parser(
      ..p4,
      scopes:,
      ctx: GrammarContext(
        ..p4.ctx,
        in_block: p2.ctx.in_block,
        module_top_level: p2.ctx.module_top_level,
      ),
    ),
    stmts,
  ))
}

fn parse_variable_declaration(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  use #(p2, decl) <- result.map(parse_variable_declaration_decl(p))
  #(p2, ast.declaration_to_statement(decl))
}

fn parse_variable_declaration_decl(
  p: Parser,
) -> Result(#(Parser, ast.Declaration), ParseError) {
  let #(p2, kind) = variable_declaration_head(p)
  use #(p3, declarations) <- result.try(
    parse_variable_declarator_list(p2, kind, []),
  )
  use p4 <- result.try(eat_semicolon(exit_declaration_context(p3, p)))
  Ok(#(p4, ast.DeclareVariable(kind:, declarations:)))
}

// consumes var, let or const and enters its declaration context
fn variable_declaration_head(p: Parser) -> #(Parser, ast.VariableKind) {
  let kind = case peek(p) {
    Let -> ast.Let
    Const -> ast.Const
    Var -> ast.Var
    _ -> panic as "parser: declaration head is not var, let or const"
  }
  let p = advance(p)
  let p = case kind {
    ast.Let -> enter_lexical_decl_context(p, scope.LetBinding)
    ast.Const | ast.Using | ast.AwaitUsing ->
      enter_lexical_decl_context(p, scope.ConstBinding)
    ast.Var ->
      Parser(..p, ctx: GrammarContext(..p.ctx, declaring: DeclaringVar))
  }
  #(p, kind)
}

fn enter_lexical_decl_context(p: Parser, kind: scope.BindingKind) -> Parser {
  Parser(
    ..p,
    ctx: GrammarContext(
      ..p.ctx,
      declaring: DeclaringLexical(kind:, bound_so_far: set.new()),
    ),
  )
}

// drop the declaration context before what follows
fn exit_declaration_context(p: Parser, outer: Parser) -> Parser {
  Parser(..p, ctx: GrammarContext(..p.ctx, declaring: outer.ctx.declaring))
}

fn parse_variable_declarator_list(
  p: Parser,
  kind: ast.VariableKind,
  acc: List(ast.VariableDeclarator),
) -> Result(#(Parser, List(ast.VariableDeclarator)), ParseError) {
  use #(p2, decl) <- result.try(parse_variable_declarator(p, kind))
  case peek(p2) {
    Comma -> parse_variable_declarator_list(advance(p2), kind, [decl, ..acc])
    _ -> Ok(#(p2, list.reverse([decl, ..acc])))
  }
}

fn parse_variable_declarator(
  p: Parser,
  kind: ast.VariableKind,
) -> Result(#(Parser, ast.VariableDeclarator), ParseError) {
  let is_destructuring = case peek(p) {
    LeftBracket | LeftBrace -> True
    _ -> False
  }
  use #(p2, pattern) <- result.try(parse_binding_pattern(p))
  case peek(p2) {
    Equal -> {
      let init_start = pos_of(p2)
      use #(p3, init_expr) <- result.try(
        parse_assignment_expression(advance(p2)),
      )
      use Nil <- result.try(check_cover_grammar_errors(p3, init_start))
      let p3 =
        Parser(
          ..p3,
          scopes: class_scopes.mark_pattern_assigned(p3.scopes, pattern),
        )
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

fn parse_binding_pattern(
  p: Parser,
) -> Result(#(Parser, ast.Pattern), ParseError) {
  case peek(p) {
    LeftBracket -> parse_array_binding_pattern(p)
    LeftBrace -> parse_object_binding_pattern(p)
    kind ->
      case is_binding_ident_token(kind) {
        True -> validate_and_register_binding(p, peek_value(p))
        False -> Error(error_at_current(p, ExpectedBindingPattern(pos_of(p))))
      }
  }
}

fn validate_and_register_binding(
  p: Parser,
  name: String,
) -> Result(#(Parser, ast.Pattern), ParseError) {
  use #(p, pattern) <- result.map(register_binding_named_at(p, p, name))
  #(advance(p), pattern)
}

// name_at supplies the error position and span, p the state
fn register_binding_named_at(
  name_at: Parser,
  p: Parser,
  name: String,
) -> Result(#(Parser, ast.Pattern), ParseError) {
  use Nil <- result.try(check_binding_identifier(name_at, name))
  use p <- result.try(declare_in_binding_list(p, name))
  use p <- result.try(accumulate_param_name(p, name))
  use p <- result.try(register_scope_binding(p, name))
  use p <- result.try(declare_export_if_exporting(p, name))
  Ok(#(p, ast.IdentifierPattern(name:, span: span_of(name_at))))
}

fn params_must_be_unique(ctx: GrammarContext) -> Bool {
  ctx.strict || ctx.in_arrow_params || ctx.in_method || ctx.has_non_simple_param
}

// gated on the param flags: in_method stays set in the body
fn accumulate_param_name(
  p: Parser,
  name: String,
) -> Result(Parser, ParseError) {
  use <- bool.guard(!p.ctx.in_formal_params && !p.ctx.in_arrow_params, Ok(p))
  use <- bool.guard(
    params_must_be_unique(p.ctx) && list.contains(p.ctx.param_bound_names, name),
    Error(DuplicateParameterName(pos_of(p), name)),
  )
  Ok(
    Parser(
      ..p,
      ctx: GrammarContext(..p.ctx, param_bound_names: [
        name,
        ..p.ctx.param_bound_names
      ]),
    ),
  )
}

// an Identifier spelling a reserved word came from a \u escape
fn check_not_escaped_reserved_word(
  p: Parser,
  name: String,
) -> Result(Nil, ParseError) {
  case is_reserved_word_kind(token.keyword_or_identifier(name)) {
    True -> Error(EscapedReservedWord(pos_of(p), name))
    False -> Ok(Nil)
  }
}

// §13.1.1 shared reserved-name checks
fn check_reserved_identifier_common(
  p: Parser,
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

// §13.1.1 identifier reference
fn check_identifier_reference(
  p: Parser,
  name: String,
) -> Result(Nil, ParseError) {
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

fn check_binding_identifier(
  p: Parser,
  name: String,
) -> Result(Nil, ParseError) {
  use Nil <- result.try(check_reserved_identifier_common(p, name))
  case name {
    "eval" | "arguments" ->
      case p.ctx.strict {
        True -> Error(StrictModeBindingName(pos_of(p), name))
        False -> Ok(Nil)
      }
    "let" ->
      case p.ctx.strict || in_lexical_declaration(p.ctx) {
        True -> Error(LetBindingInLexicalDecl(pos_of(p)))
        False -> Ok(Nil)
      }
    _ -> Ok(Nil)
  }
}

fn declare_in_binding_list(
  p: Parser,
  name: String,
) -> Result(Parser, ParseError) {
  case p.ctx.declaring {
    DeclaringLexical(kind:, bound_so_far:) ->
      case set.contains(bound_so_far, name) {
        True -> Error(DuplicateBindingLexical(pos_of(p), name))
        False ->
          Ok(
            Parser(
              ..p,
              ctx: GrammarContext(
                ..p.ctx,
                declaring: DeclaringLexical(
                  kind:,
                  bound_so_far: set.insert(bound_so_far, name),
                ),
              ),
            ),
          )
      }
    NotDeclaring | DeclaringVar | DeclaringParam -> Ok(p)
  }
}

// §14.2.1; the implicit arguments placeholder is exempt
fn register_lexical_name(
  p: Parser,
  name: String,
  kind: scope.BindingKind,
  pos: Int,
) -> Result(Parser, ParseError) {
  use <- bool.guard(
    scope_builder.lexical_conflict(p.scopes, name)
      && !scope_builder.only_implicit_arguments(p.scopes, name),
    Error(IdentifierAlreadyDeclared(pos, name)),
  )
  Ok(
    Parser(
      ..p,
      scopes: scope_builder.declare(p.scopes, name, kind, synthetic: False),
    ),
  )
}

fn register_scope_binding(
  p: Parser,
  name: String,
) -> Result(Parser, ParseError) {
  case p.ctx.declaring {
    DeclaringLexical(kind:, ..) ->
      register_lexical_name(p, name, kind, pos_of(p))
    DeclaringParam ->
      Ok(
        Parser(
          ..p,
          scopes: scope_builder.declare(
            p.scopes,
            name,
            scope.ParamBinding,
            synthetic: False,
          ),
        ),
      )
    DeclaringVar -> {
      // §14.3.2, and §16.2.1.1 at module root
      use <- bool.guard(
        scope_builder.var_conflicts_lexical(p.scopes, name)
          || scope_builder.var_conflicts_module_fn(p.scopes, name),
        Error(IdentifierAlreadyDeclared(pos_of(p), name)),
      )
      Ok(Parser(..p, scopes: scope_builder.declare_var(p.scopes, name)))
    }
    NotDeclaring -> Ok(p)
  }
}

// is_plain: annex b §B.3.2 applies to plain functions only
fn register_function_name(
  p: Parser,
  name: String,
  name_pos: Int,
  is_plain is_plain: Bool,
) -> Result(Parser, ParseError) {
  // annex b §B.3.1: synthetic block, never clashes
  use <- bool.guard(!p.ctx.strict && p.ctx.in_single_stmt_pos, Ok(p))
  // §16.1.1 script vs §16.2.1.1 module top-level functions
  case p.ctx.in_block, p.ctx.module_top_level {
    // no hoisted_vars entry: that is what marks a module function
    False, True -> register_lexical_name(p, name, scope.VarBinding, name_pos)
    // §14.2.2 lexical; annex b §B.3.2 var-hoist candidate
    True, _ -> {
      use p2 <- result.map(register_lexical_name(
        p,
        name,
        scope.LetBinding,
        name_pos,
      ))
      case !p.ctx.strict && is_plain {
        False -> p2
        True ->
          Parser(..p2, scopes: scope_builder.annexb_candidate(p2.scopes, name))
      }
    }
    False, False -> {
      use <- bool.guard(
        scope_builder.current_has_kind(p.scopes, name, scope.LetBinding)
          || scope_builder.current_has_kind(p.scopes, name, scope.ConstBinding),
        Error(IdentifierAlreadyDeclared(name_pos, name)),
      )
      Ok(Parser(..p, scopes: scope_builder.declare_var(p.scopes, name)))
    }
  }
}

fn declare_export_name(p: Parser, name: String) -> Result(Parser, ParseError) {
  use <- bool.guard(p.mode == Script, Ok(p))
  use <- bool.guard(
    set.contains(p.export_names, name),
    Error(DuplicateExport(pos_of(p), name)),
  )
  Ok(Parser(..p, export_names: set.insert(p.export_names, name)))
}

fn declare_import_binding(
  p: Parser,
  name: String,
) -> Result(Parser, ParseError) {
  use <- bool.guard(p.mode == Script, Ok(p))
  use <- bool.guard(
    set.contains(p.import_bindings, name),
    Error(DuplicateImportBinding(pos_of(p), name)),
  )
  Ok(
    Parser(
      ..p,
      scopes: scope_builder.declare(
        p.scopes,
        name,
        scope.ConstBinding,
        synthetic: False,
      ),
      import_bindings: set.insert(p.import_bindings, name),
    ),
  )
}

fn check_import_binding_name(p: Parser) -> Result(Nil, ParseError) {
  let name = peek_value(p)
  case is_reserved_word_kind(peek(p)) {
    True -> Error(ReservedWordImportBinding(pos_of(p), name))
    False -> check_binding_identifier(p, name)
  }
}

fn declare_export_if_exporting(
  p: Parser,
  name: String,
) -> Result(Parser, ParseError) {
  case p.ctx.in_export_decl {
    True -> declare_export_name(p, name)
    False -> Ok(p)
  }
}

// = commits: errors propagate, no backtrack
fn parse_pattern_default(
  p: Parser,
  pat: ast.Pattern,
) -> Result(#(Parser, ast.Pattern), ParseError) {
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

fn parse_array_binding_pattern(
  p: Parser,
) -> Result(#(Parser, ast.Pattern), ParseError) {
  use p2 <- result.try(expect(p, LeftBracket))
  parse_array_binding_elements(p2, [])
}

fn parse_array_binding_elements(
  p: Parser,
  acc: List(Option(ast.Pattern)),
) -> Result(#(Parser, ast.Pattern), ParseError) {
  case peek(p) {
    RightBracket ->
      Ok(#(advance(p), ast.ArrayPattern(elements: list.reverse(acc))))
    Comma -> parse_array_binding_elements(advance(p), [None, ..acc])
    DotDotDot -> {
      let p2 = advance(p)
      use #(p3, inner_pat) <- result.try(parse_binding_pattern(p2))
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

fn parse_object_binding_pattern(
  p: Parser,
) -> Result(#(Parser, ast.Pattern), ParseError) {
  use p2 <- result.try(expect(p, LeftBrace))
  parse_object_binding_properties(p2, [])
}

fn parse_object_binding_properties(
  p: Parser,
  acc: List(ast.PatternProperty),
) -> Result(#(Parser, ast.Pattern), ParseError) {
  case peek(p) {
    RightBrace ->
      Ok(#(advance(p), ast.ObjectPattern(properties: list.reverse(acc))))
    DotDotDot -> {
      let p2 = advance(p)
      let kind = peek(p2)
      // §13.3.3: object rest is an identifier only
      use <- bool.guard(
        kind == LeftBrace || kind == LeftBracket,
        Error(InvalidRestBinding(pos_of(p2))),
      )
      use <- bool.guard(
        !is_binding_ident_token(kind),
        Error(ExpectedIdentifier(pos_of(p2))),
      )
      let name = peek_value(p2)
      let span = span_of(p2)
      use #(p3, _ident_pat) <- result.try(validate_and_register_binding(
        p2,
        name,
      ))
      let rest = ast.RestProperty(name:, span:)
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
  p: Parser,
) -> Result(#(Parser, ast.PatternProperty), ParseError) {
  let shorthand_name = simple_binding_name(p)
  use #(p2, key) <- result.try(parse_property_name(p))
  use Nil <- result.try(reject_private_property_key(p, key))
  case peek(p2), shorthand_name {
    Colon, _ -> {
      use #(p4, val_pat) <- result.try(parse_binding_pattern(advance(p2)))
      use #(p5, final_pat) <- result.map(parse_pattern_default(p4, val_pat))
      #(p5, ast.PatternProperty(key:, value: final_pat, shorthand: False))
    }
    _, None -> Error(UnexpectedToken(pos_of(p), peek(p)))
    _, Some(prop_name) -> {
      use #(p3, ident) <- result.try(register_binding_named_at(p, p2, prop_name))
      use #(p4, value) <- result.map(parse_pattern_default(p3, ident))
      #(p4, ast.PatternProperty(key:, value:, shorthand: True))
    }
  }
}

// annex b legacy octal forms are strict mode errors
fn check_legacy_octal_literal(p: Parser) -> Result(Nil, ParseError) {
  use <- bool.guard(
    p.ctx.strict && peek_annex_b_legacy(p),
    Error(OctalLiteralStrictMode(pos_of(p))),
  )
  Ok(Nil)
}

fn numeric_literal(p: Parser) -> Result(ast.Expression, ParseError) {
  let span = span_of(p)
  case number.parse_numeric_literal(peek_value(p)) {
    Ok(number.ParsedNumber(n)) -> Ok(ast.NumberLiteral(value: n, span:))
    Ok(number.ParsedBigInt(i)) -> Ok(ast.BigIntLiteral(value: i, span:))
    Error(err) -> Error(MalformedNumericLiteral(pos_of(p), err))
  }
}

// the only place annex b string escapes are rejected
fn string_literal_value(p: Parser) -> Result(String, ParseError) {
  string_token_value(p, strict: p.ctx.strict)
}

// module code is always strict
fn module_specifier_value(p: Parser) -> Result(String, ParseError) {
  string_token_value(p, strict: True)
}

// string export names use their cooked value
fn specifier_name_value(p: Parser) -> Result(String, ParseError) {
  case peek(p) {
    StringLiteral -> module_specifier_value(p)
    _ -> Ok(peek_value(p))
  }
}

fn string_token_value(
  p: Parser,
  strict strict: Bool,
) -> Result(String, ParseError) {
  use <- bool.guard(
    strict && peek_annex_b_legacy(p),
    Error(OctalEscapeStrictMode(pos_of(p))),
  )
  Ok(decode_string_escapes(peek_value(p)))
}

fn numeric_property_key(p: Parser) -> Result(ast.PropertyName, ParseError) {
  let span = span_of(p)
  case number.parse_numeric_literal(peek_value(p)) {
    Ok(number.ParsedNumber(n)) -> Ok(ast.NumberName(value: n, span:))
    Ok(number.ParsedBigInt(i)) -> Ok(ast.BigIntName(value: i, span:))
    Error(err) -> Error(MalformedNumericLiteral(pos_of(p), err))
  }
}

// private names lex as # prefixed identifiers
fn identifier_property_key(name: String, span: ast.Span) -> ast.PropertyName {
  case name {
    "#" <> _ -> ast.PrivateName(name:, span:)
    _ -> ast.IdentifierName(name:, span:)
  }
}

fn parse_property_name(
  p: Parser,
) -> Result(#(Parser, ast.PropertyName), ParseError) {
  case peek(p) {
    Identifier ->
      Ok(#(advance(p), identifier_property_key(peek_value(p), span_of(p))))
    Number -> {
      use Nil <- result.try(check_legacy_octal_literal(p))
      use key <- result.map(numeric_property_key(p))
      #(advance(p), key)
    }
    StringLiteral -> {
      use value <- result.map(string_literal_value(p))
      #(advance(p), ast.StringName(value:, span: span_of(p)))
    }
    LeftBracket -> {
      // computed key is [+In]
      use #(p4, expr) <- result.map({
        use p2 <- allowing_in(advance(p))
        use #(p3, expr) <- result.try(parse_assignment_expression(p2))
        use p4 <- result.map(expect(p3, RightBracket))
        #(p4, expr)
      })
      #(p4, ast.ComputedName(expr))
    }
    _ ->
      case is_identifier_or_keyword(peek(p)) {
        True ->
          Ok(#(advance(p), identifier_property_key(peek_value(p), span_of(p))))
        False -> Error(error_at_current(p, ExpectedPropertyName(pos_of(p))))
      }
  }
}

fn parse_if_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, condition) <- result.try(parse_expression(p3))
  use p5 <- result.try(expect(p4, RightParen))
  use #(p6, consequent) <- result.try(parse_single_statement(
    p5,
    allow_annex_b_function: True,
  ))
  use #(p7, alternate) <- result.map(case peek(p6) {
    Else -> {
      use #(p7, alternate) <- result.map(parse_single_statement(
        advance(p6),
        allow_annex_b_function: True,
      ))
      #(p7, Some(alternate))
    }
    _ -> Ok(#(p6, None))
  })
  #(p7, ast.IfStatement(condition:, consequent:, alternate:))
}

fn parse_while_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, condition) <- result.try(parse_expression(p3))
  use p5 <- result.try(expect(p4, RightParen))
  let p5 = set_loop_depth(p5, p5.ctx.loop_depth + 1)
  use #(p6, body) <- result.try(parse_single_statement(
    p5,
    allow_annex_b_function: False,
  ))
  Ok(#(
    set_loop_depth(p6, p.ctx.loop_depth),
    ast.WhileStatement(condition:, body:),
  ))
}

fn parse_do_while_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  let p2 = advance(p)
  let p2 = set_loop_depth(p2, p2.ctx.loop_depth + 1)
  use #(p3, body) <- result.try(parse_single_statement(
    p2,
    allow_annex_b_function: False,
  ))
  use p4 <- result.try(expect(p3, While))
  use p5 <- result.try(expect(p4, LeftParen))
  use #(p6, condition) <- result.try(parse_expression(p5))
  use p7 <- result.try(expect(p6, RightParen))
  // asi after do-while ) even without a newline
  let p8 = case peek(p7) {
    Semicolon -> advance(p7)
    _ -> p7
  }
  Ok(#(
    set_loop_depth(p8, p.ctx.loop_depth),
    ast.DoWhileStatement(condition:, body:),
  ))
}

fn parse_for_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  let p2 = advance(p)
  let #(p2, is_await) = case peek(p2) {
    Await -> #(advance(p2), True)
    _ -> #(p2, False)
  }
  let p2 = set_loop_depth(p2, p2.ctx.loop_depth + 1)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, stmt) <- result.try(parse_for_head(p3, is_await:))
  Ok(#(set_loop_depth(p4, p.ctx.loop_depth), stmt))
}

fn set_loop_depth(p: Parser, depth: Int) -> Parser {
  Parser(..p, ctx: GrammarContext(..p.ctx, loop_depth: depth))
}

fn parse_for_head(
  p: Parser,
  is_await is_await: Bool,
) -> Result(#(Parser, ast.Statement), ParseError) {
  let scoped_declaration = fn() {
    in_for_head_scope(p, parse_for_declaration(_, is_await:))
  }
  case peek(p) {
    Semicolon -> parse_for_classic_rest(advance(p), None)
    Var -> parse_for_declaration(p, is_await:)
    Const -> scoped_declaration()
    Let -> {
      let next = peek_at(p, 1)
      case
        next == LeftBrace || next == LeftBracket || is_binding_ident_token(next)
      {
        True -> scoped_declaration()
        False -> parse_for_expression(p, is_await:)
      }
    }
    // for (using of = ..;;) binds of, for (using of x) does not
    Identifier ->
      case
        is_using_decl_start(p, 0)
        && { peek_at(p, 1) != Of || peek_at(p, 2) == Equal }
      {
        True ->
          in_for_head_scope(p, parse_for_using_declaration(
            _,
            is_await:,
            is_await_using: False,
          ))
        False -> parse_for_expression(p, is_await:)
      }
    // §14.7.5: literal async of is forbidden here
    Async ->
      case
        !is_await
        && !peek_had_escape(p)
        && peek_at(p, 1) == Of
        && peek_at(p, 2) != Arrow
      {
        True -> Error(InvalidForOfLhs(pos_of(p)))
        False -> parse_for_expression(p, is_await:)
      }
    // await using of IS a declaration binding of
    Await ->
      case is_await_using_decl_start(p) {
        True ->
          in_for_head_scope(p, parse_for_using_declaration(
            _,
            is_await:,
            is_await_using: True,
          ))
        False -> parse_for_expression(p, is_await:)
      }
    _ -> parse_for_expression(p, is_await:)
  }
}

// head names get their own scope: for(let a;;); let a; is valid
fn in_for_head_scope(
  p: Parser,
  parse: fn(Parser) -> Result(#(Parser, ast.Statement), ParseError),
) -> Result(#(Parser, ast.Statement), ParseError) {
  use #(p2, stmt) <- result.map(parse(enter_block_scope(p)))
  #(restore_block_scope(p2, p) |> exit_declaration_context(p), stmt)
}

fn parse_for_using_declaration(
  p: Parser,
  is_await is_await: Bool,
  is_await_using is_await_using: Bool,
) -> Result(#(Parser, ast.Statement), ParseError) {
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
      parse_for_in_of_rest(
        exit_declaration_context(p3, p),
        decl,
        is_of: True,
        is_await:,
      )
    }
    Equal -> {
      // classic head declarators are [~In]
      use #(p6, declarators) <- result.try({
        use p4 <- disallowing_in(advance(p3))
        use #(p5, init_expr) <- result.try(parse_assignment_expression(p4))
        let first = ast.VariableDeclarator(id: pattern, init: Some(init_expr))
        use #(p6, rest) <- result.map(parse_using_remaining_declarators(p5, []))
        #(p6, [first, ..rest])
      })
      let decl = ast.ForInitDeclaration(kind:, declarations: declarators)
      use p7 <- result.try(expect(p6, Semicolon))
      parse_for_classic_rest(exit_declaration_context(p7, p), Some(decl))
    }
    _ -> Error(UsingMissingInitializer(pos_of(p3)))
  }
}

fn parse_using_remaining_declarators(
  p: Parser,
  acc: List(ast.VariableDeclarator),
) -> Result(#(Parser, List(ast.VariableDeclarator)), ParseError) {
  case peek(p) {
    Comma -> parse_using_declarator_list(advance(p), acc)
    _ -> Ok(#(p, list.reverse(acc)))
  }
}

fn parse_for_declaration(
  p: Parser,
  is_await is_await: Bool,
) -> Result(#(Parser, ast.Statement), ParseError) {
  let #(p2, kind) = variable_declaration_head(p)
  let is_destr = peek(p2) == LeftBrace || peek(p2) == LeftBracket
  // B.3.4: for-of var names vs enclosing catch param
  let catch_params = scope_builder.nearest_catch_params(p2.scopes)
  use #(p3, pattern) <- result.try(parse_binding_pattern(p2))
  let decl =
    ast.ForInitDeclaration(kind:, declarations: [
      ast.VariableDeclarator(id: pattern, init: None),
    ])
  // §14.7.5.9: head binding written each iteration
  let p_in_of = fn() {
    let p = exit_declaration_context(p3, p)
    Parser(..p, scopes: class_scopes.mark_pattern_assigned(p.scopes, pattern))
  }
  case peek(p3) {
    In -> parse_for_in_of_rest(p_in_of(), decl, is_of: False, is_await: False)
    Of -> {
      use Nil <- result.try(case kind {
        ast.Var ->
          check_for_of_var_vs_catch_param(
            ast.pattern_bound_names(pattern),
            catch_params,
            pos_of(p3),
          )
        _ -> Ok(Nil)
      })
      parse_for_in_of_rest(p_in_of(), decl, is_of: True, is_await:)
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
      // [~In] so for (var x = a in b) is for-in
      use #(p5, init_expr) <- result.try(disallowing_in(
        advance(p3),
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

// §14.7.4: the whole declarator list is [~In]
fn finish_for_classic_decl(
  p: Parser,
  outer: Parser,
  kind: ast.VariableKind,
  pattern: ast.Pattern,
  init: Option(ast.Expression),
) -> Result(#(Parser, ast.Statement), ParseError) {
  let first = ast.VariableDeclarator(id: pattern, init:)
  use #(p2, rest) <- result.try({
    use p <- disallowing_in(p)
    parse_remaining_declarators(p, kind, [])
  })
  let decl = ast.ForInitDeclaration(kind:, declarations: [first, ..rest])
  use p3 <- result.try(expect(p2, Semicolon))
  parse_for_classic_rest(exit_declaration_context(p3, outer), Some(decl))
}

// §13.15.1/§13.15.5 eval/arguments in target positions only
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
          ast.MethodProperty(..) | ast.AccessorProperty(..) -> False
        }
      })
    _ -> False
  }
}

fn pattern_element_has_eval_args_target(expr: ast.Expression) -> Bool {
  case expr {
    ast.AssignmentExpression(operator: ast.Assign, left:, ..) ->
      destructuring_target_is_eval_args(left)
    _ -> destructuring_target_is_eval_args(expr)
  }
}

// eval.x stays legal (§13.15.5)
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
  p: Parser,
  is_await is_await: Bool,
) -> Result(#(Parser, ast.Statement), ParseError) {
  let start_token = peek(p)
  // [~In] inside the for head
  use #(p2, expr) <- result.try(disallowing_in(p, parse_expression))
  case peek(p2) {
    Semicolon ->
      // for(;;) init is never a pattern: cover errors due
      case p2.ctx.has_cover_initializer, p2.ctx.dup_proto_pos {
        True, _ -> Error(InvalidDestructuringTarget(pos_of(p2)))
        False, Some(pos) -> Error(DuplicateProtoProperty(pos))
        False, None ->
          parse_for_classic_rest(advance(p2), Some(ast.ForInitExpression(expr)))
      }
    In | Of -> {
      let is_of = peek(p2) == Of
      let is_bare_pattern =
        start_token == LeftBrace || start_token == LeftBracket
      let let_of_forbidden = start_token == Let && is_of
      let valid_target =
        !let_of_forbidden
        && {
          p2.last_expr_assignable
          || { is_bare_pattern && !p2.literal_invalid_as_pattern }
          || is_web_compat_call_target(p2, expr)
        }
      use <- bool.lazy_guard(!valid_target, fn() {
        case is_of {
          False -> Error(InvalidForInLhs(pos_of(p2)))
          True -> Error(InvalidForOfLhs(pos_of(p2)))
        }
      })
      use <- bool.guard(
        is_bare_pattern && p2.ctx.strict && pattern_has_eval_args_target(expr),
        Error(EvalArgsAssignStrictMode(pos_of(p2))),
      )
      // clear cover flags so they do not leak into the body
      let p2 =
        Parser(
          ..p2,
          scopes: class_scopes.mark_assign_targets(p2.scopes, expr),
          literal_invalid_as_pattern: False,
          ctx: GrammarContext(
            ..p2.ctx,
            has_cover_initializer: False,
            dup_proto_pos: None,
          ),
        )
      let left = ast.ForInitExpression(expr)
      parse_for_in_of_rest(p2, left, is_of:, is_await: is_of && is_await)
    }
    _ -> Error(ExpectedForSeparator(pos_of(p2)))
  }
}

// , commits to another declarator: propagate errors
fn parse_remaining_declarators(
  p: Parser,
  kind: ast.VariableKind,
  acc: List(ast.VariableDeclarator),
) -> Result(#(Parser, List(ast.VariableDeclarator)), ParseError) {
  case peek(p) {
    Comma -> {
      use #(p2, decl) <- result.try(parse_variable_declarator(advance(p), kind))
      parse_remaining_declarators(p2, kind, [decl, ..acc])
    }
    _ -> Ok(#(p, list.reverse(acc)))
  }
}

fn parse_for_in_of_rest(
  p: Parser,
  left: ast.ForInit,
  is_of is_of: Bool,
  is_await is_await: Bool,
) -> Result(#(Parser, ast.Statement), ParseError) {
  let p2 = advance(p)
  use #(p3, right) <- result.try(case is_of {
    True -> parse_assignment_expression(p2)
    False -> parse_expression(p2)
  })
  use p4 <- result.try(expect(p3, RightParen))
  use #(p5, body) <- result.map(parse_single_statement(
    p4,
    allow_annex_b_function: False,
  ))
  case is_of {
    True -> #(p5, ast.ForOfStatement(left:, right:, body:, is_await:))
    False -> #(p5, ast.ForInStatement(left:, right:, body:))
  }
}

fn parse_for_classic_rest(
  p: Parser,
  init: Option(ast.ForInit),
) -> Result(#(Parser, ast.Statement), ParseError) {
  use #(p2, condition) <- result.try(case peek(p) {
    Semicolon -> Ok(#(advance(p), None))
    _ -> {
      use #(p2, condition) <- result.try(parse_expression(p))
      use p3 <- result.map(expect(p2, Semicolon))
      #(p3, Some(condition))
    }
  })
  use #(p3, update) <- result.try(case peek(p2) {
    RightParen -> Ok(#(advance(p2), None))
    _ -> {
      use #(p3, update) <- result.try(parse_expression(p2))
      use p4 <- result.map(expect(p3, RightParen))
      #(p4, Some(update))
    }
  })
  use #(p4, body) <- result.map(parse_single_statement(
    p3,
    allow_annex_b_function: False,
  ))
  #(p4, ast.ForStatement(init:, condition:, update:, body:))
}

fn parse_return_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  use <- bool.guard(
    p.ctx.function_depth <= 0,
    Error(ReturnOutsideFunction(pos_of(p))),
  )
  let p2 = advance(p)
  let bare = Ok(#(p2, ast.ReturnStatement(argument: None)))
  case peek(p2) {
    Semicolon -> Ok(#(advance(p2), ast.ReturnStatement(argument: None)))
    RightBrace | Eof -> bare
    _ -> {
      use <- bool.guard(has_line_break_before(p2), bare)
      let start = pos_of(p2)
      use #(p3, expr) <- result.try(parse_expression(p2))
      use Nil <- result.try(check_cover_grammar_errors(p3, start))
      use p4 <- result.map(eat_semicolon(p3))
      #(p4, ast.ReturnStatement(argument: Some(expr)))
    }
  }
}

fn parse_optional_label(
  p: Parser,
  label_use: LabelUse,
) -> Result(#(Parser, Option(String)), ParseError) {
  case peek(p) {
    Semicolon -> Ok(#(advance(p), None))
    Identifier -> {
      use <- bool.guard(has_line_break_before(p), Ok(#(p, None)))
      let label = peek_value(p)
      use Nil <- result.try(check_label_target(p, label, label_use))
      use p2 <- result.map(eat_semicolon(advance(p)))
      #(p2, Some(label))
    }
    _ -> {
      use p2 <- result.map(eat_semicolon(p))
      #(p2, None)
    }
  }
}

fn check_label_target(
  p: Parser,
  label: String,
  label_use: LabelUse,
) -> Result(Nil, ParseError) {
  case list.key_find(p.ctx.label_set, label), label_use {
    Error(Nil), _ -> Error(UndefinedLabel(pos_of(p), label))
    Ok(_), BreakLabel -> Ok(Nil)
    Ok(LoopLabel), ContinueLabel -> Ok(Nil)
    Ok(PlainLabel), ContinueLabel ->
      Error(ContinueToNonIterationLabel(pos_of(p), label))
  }
}

fn parse_break_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  use #(p2, label) <- result.try(parse_optional_label(advance(p), BreakLabel))
  let inside_target = p.ctx.loop_depth > 0 || p.ctx.switch_depth > 0
  use <- bool.guard(
    option.is_none(label) && !inside_target,
    Error(BreakOutsideLoopOrSwitch(pos_of(p))),
  )
  Ok(#(p2, ast.BreakStatement(label:)))
}

fn parse_continue_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  use <- bool.guard(
    p.ctx.loop_depth <= 0,
    Error(ContinueOutsideLoop(pos_of(p))),
  )
  use #(p2, label) <- result.map(parse_optional_label(advance(p), ContinueLabel))
  #(p2, ast.ContinueStatement(label:))
}

fn parse_throw_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  let p2 = advance(p)
  // no line terminator after throw
  use <- bool.guard(
    has_line_break_before(p2),
    Error(ThrowLineBreak(pos_of(p2))),
  )
  let start = pos_of(p2)
  use #(p3, expr) <- result.try(parse_expression(p2))
  use Nil <- result.try(check_cover_grammar_errors(p3, start))
  use p4 <- result.try(eat_semicolon(p3))
  Ok(#(p4, ast.ThrowStatement(argument: expr)))
}

fn parse_try_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  let p2 = advance(p)
  let p2 = Parser(..p2, scopes: scope_builder.enter_try(p2.scopes))
  use #(p3, block) <- result.try(parse_block_body(p2))
  use #(p4, handler) <- result.try(parse_catch_clause(p3))
  use #(p5, finalizer) <- result.try(case peek(p4) {
    Finally -> {
      use #(p, b) <- result.map(parse_block_body(advance(p4)))
      #(p, Some(b))
    }
    _ -> Ok(#(p4, None))
  })
  let p5 = Parser(..p5, scopes: scope_builder.leave_try(p5.scopes))
  use tail <- result.map(case handler, finalizer {
    None, None -> Error(MissingCatchOrFinally(pos_of(p5)))
    Some(handler), None -> Ok(ast.TryCatch(handler:))
    None, Some(finalizer) -> Ok(ast.TryFinally(finalizer:))
    Some(handler), Some(finalizer) ->
      Ok(ast.TryCatchFinally(handler:, finalizer:))
  })
  #(p5, ast.TryStatement(block:, tail:))
}

fn parse_catch_clause(
  p: Parser,
) -> Result(#(Parser, Option(ast.CatchClause)), ParseError) {
  use <- bool.guard(peek(p) != Catch, Ok(#(p, None)))
  let p2 = advance(p)
  case peek(p2) {
    LeftParen -> {
      let p3 = advance(p2)
      let #(scopes, catch_id) = scope_builder.push(p3.scopes, scope.Catch)
      use #(p4, param) <- result.try(parse_catch_parameter(
        Parser(..p3, scopes:),
      ))
      use p5 <- result.try(expect(p4, RightParen))
      // §14.15.2: catch param scope is separate from the block
      use #(p6, body) <- result.map(parse_block_body(p5))
      #(
        // flip catch children to source order for finalize
        Parser(
          ..p6,
          scopes: scope_builder.reorder_block_children(p6.scopes, catch_id)
            |> scope_builder.enter(p3.scopes.current),
          ctx: GrammarContext(
            ..p6.ctx,
            in_block: p3.ctx.in_block,
            declaring: p3.ctx.declaring,
          ),
        ),
        Some(ast.CatchClause(param: Some(param), body:)),
      )
    }
    _ -> {
      use #(p3, body) <- result.map(parse_block_body(p2))
      #(p3, Some(ast.CatchClause(param: None, body:)))
    }
  }
}

// catch is not a function boundary; param state is handed back
fn parse_catch_parameter(
  p: Parser,
) -> Result(#(Parser, ast.Pattern), ParseError) {
  let p_inner =
    Parser(
      ..p,
      ctx: GrammarContext(
        ..p.ctx,
        in_block: True,
        declaring: DeclaringParam,
        in_formal_params: True,
        in_catch_param: True,
        param_bound_names: [],
        has_non_simple_param: True,
      ),
    )
  use #(p2, param) <- result.map(parse_binding_pattern(p_inner))
  // §B.3.4: destructured catch param blocks annex b promotion
  let simple = case param {
    ast.IdentifierPattern(..) -> True
    _ -> False
  }
  let scopes =
    scope_builder.update_current(p2.scopes, fn(s) {
      scope_builder.RawScope(..s, catch_param_simple: simple)
    })
  let ctx =
    GrammarContext(
      ..p2.ctx,
      declaring: NotDeclaring,
      in_formal_params: p.ctx.in_formal_params,
      in_catch_param: p.ctx.in_catch_param,
      param_bound_names: p.ctx.param_bound_names,
      has_non_simple_param: p.ctx.has_non_simple_param,
    )
  #(Parser(..p2, scopes:, ctx:), param)
}

fn parse_switch_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, discriminant) <- result.try(parse_expression(p3))
  use p5 <- result.try(expect(p4, RightParen))
  use p6 <- result.try(expect(p5, LeftBrace))
  // one block scope around all cases; may shadow params
  let #(scopes, switch_id) = scope_builder.push(p6.scopes, scope.Block)
  let p_inner =
    Parser(
      ..p6,
      scopes:,
      ctx: GrammarContext(
        ..p6.ctx,
        in_block: True,
        switch_depth: p6.ctx.switch_depth + 1,
      ),
    )
  use #(p7, cases) <- result.try(
    parse_switch_cases(p_inner, has_default: False, acc: []),
  )
  // never pruned: emit_switch always enters this scope
  let scopes =
    scope_builder.reorder_switch_children(p7.scopes, switch_id)
    |> scope_builder.enter(p6.scopes.current)
  Ok(#(
    Parser(
      ..p7,
      scopes:,
      ctx: GrammarContext(
        ..p7.ctx,
        in_block: p6.ctx.in_block,
        switch_depth: p6.ctx.switch_depth,
      ),
    ),
    ast.SwitchStatement(discriminant:, cases: list.reverse(cases)),
  ))
}

// acc is newest first; the caller reverses
fn parse_switch_cases(
  p: Parser,
  has_default has_default: Bool,
  acc acc: List(ast.SwitchCase),
) -> Result(#(Parser, List(ast.SwitchCase)), ParseError) {
  case peek(p) {
    RightBrace -> Ok(#(advance(p), acc))
    Case -> {
      let p2 = advance(p)
      // §14.12.4: case tests run before any case body
      let switch_id = p2.scopes.current
      let mark = scope_builder.children_newest_first(p2.scopes, switch_id)
      use #(p3, condition) <- result.try(parse_expression(p2))
      let p3 =
        Parser(
          ..p3,
          scopes: scope_builder.tag_children_since(
            p3.scopes,
            switch_id,
            mark,
            scope_builder.SwitchTestSource,
          ),
        )
      use p4 <- result.try(expect(p3, Colon))
      use #(p5, consequent) <- result.try(parse_case_clause_body(p4, []))
      parse_switch_cases(p5, has_default:, acc: [
        ast.SwitchCase(condition: Some(condition), consequent:),
        ..acc
      ])
    }
    Default -> {
      use <- bool.guard(has_default, Error(DuplicateDefaultCase(pos_of(p))))
      use p3 <- result.try(expect(advance(p), Colon))
      use #(p4, consequent) <- result.try(parse_case_clause_body(p3, []))
      parse_switch_cases(p4, has_default: True, acc: [
        ast.SwitchCase(condition: None, consequent:),
        ..acc
      ])
    }
    _ -> Error(ExpectedCaseDefaultOrBrace(pos_of(p)))
  }
}

fn parse_case_clause_body(
  p: Parser,
  acc: List(ast.StmtWithLine),
) -> Result(#(Parser, List(ast.StmtWithLine)), ParseError) {
  case peek(p) {
    RightBrace | Case | Default -> Ok(#(p, list.reverse(acc)))
    _ -> {
      let line = line_of(p)
      use #(p2, stmt) <- result.try(parse_statement(
        Parser(..p, ctx: GrammarContext(..p.ctx, in_case_clause: True)),
      ))
      let p2 =
        Parser(
          ..p2,
          ctx: GrammarContext(..p2.ctx, in_case_clause: p.ctx.in_case_clause),
        )
      parse_case_clause_body(p2, [ast.StmtWithLine(line, stmt), ..acc])
    }
  }
}

type FunctionHead {
  FunctionHead(
    after_name: Parser,
    is_generator: Bool,
    name: Option(String),
    name_span: ast.Span,
  )
}

// name_binds_inside: expressions validate the name with their own flags
fn parse_function_head(
  p: Parser,
  is_async is_async: Bool,
  name_binds_inside name_binds_inside: Bool,
) -> Result(FunctionHead, ParseError) {
  let p2 = case is_async {
    True -> advance(advance(p))
    False -> advance(p)
  }
  let is_generator = peek(p2) == Star
  let p3 = case is_generator {
    True -> advance(p2)
    False -> p2
  }
  let p_for_name = case name_binds_inside {
    True ->
      Parser(
        ..p3,
        ctx: GrammarContext(
          ..p3.ctx,
          in_generator: is_generator,
          in_async: is_async,
        ),
      )
    False -> p3
  }
  use p4 <- result.map(eat_optional_name(p_for_name))
  FunctionHead(
    after_name: p4,
    is_generator:,
    name: simple_binding_name(p3),
    name_span: span_of(p3),
  )
}

fn function_head_binding(head: FunctionHead) -> Option(ast.NamedBinding) {
  use name <- option.map(head.name)
  ast.NamedBinding(name:, span: head.name_span)
}

fn parse_function_statement(
  p: Parser,
  is_async is_async: Bool,
) -> Result(#(Parser, ast.Statement), ParseError) {
  use #(p2, function) <- result.map(parse_function_declaration(
    p,
    name_required: True,
    is_async:,
  ))
  #(p2, ast.declaration_to_statement(ast.DeclareFunction(function:)))
}

fn parse_function_declaration(
  p: Parser,
  name_required name_required: Bool,
  is_async is_async: Bool,
) -> Result(#(Parser, ast.FunctionLiteral), ParseError) {
  use head <- result.try(parse_function_head(
    p,
    is_async:,
    name_binds_inside: False,
  ))
  let FunctionHead(after_name: p4, is_generator:, name:, name_span:) = head
  use <- bool.guard(
    option.is_none(name) && name_required,
    Error(ExpectedIdentifier(name_span.start)),
  )
  // tag only what emit's collect_hoisted_funcs hoists
  let is_hoisted_decl = option.is_some(name) && !p4.ctx.in_single_stmt_pos
  let p_fn =
    enter_function_context(p4, is_generator:, is_async:, strict_name: name)
  let p_fn = case is_hoisted_decl {
    True ->
      Parser(
        ..p_fn,
        scopes: scope_builder.set_source_tag(
          p_fn.scopes,
          p_fn.scopes.current,
          scope_builder.FnDeclSource,
        ),
      )
    False -> p_fn
  }
  use #(p5, params, body) <- result.try(
    parse_function_params_and_body(p_fn) |> restore_context_fn(p),
  )
  use p6 <- result.map(case name {
    None -> Ok(p5)
    Some(name) ->
      register_function_name(
        p5,
        name,
        name_span.start,
        is_plain: !is_generator && !is_async,
      )
  })
  let function =
    ast.FunctionLiteral(
      name: function_head_binding(head),
      params:,
      body:,
      is_generator:,
      is_async:,
    )
  #(p6, function)
}

fn parse_function_params_and_body(
  p: Parser,
) -> Result(#(Parser, List(ast.Pattern), List(ast.StmtWithLine)), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  let p2 =
    Parser(
      ..p2,
      ctx: GrammarContext(
        ..p2.ctx,
        in_formal_params: True,
        declaring: DeclaringParam,
      ),
    )
  use #(p3, params) <- result.try(parse_formal_parameters(p2))
  let p3 =
    Parser(
      ..p3,
      ctx: GrammarContext(
        ..p3.ctx,
        in_formal_params: False,
        declaring: NotDeclaring,
      ),
    )
  use p4 <- result.try(expect(p3, RightParen))
  use #(p5, body) <- result.map(begin_function_body(p4, params))
  #(p5, params, body)
}

// p is at the body brace, params already consumed
fn begin_function_body(
  p: Parser,
  params: List(ast.Pattern),
) -> Result(#(Parser, List(ast.StmtWithLine)), ParseError) {
  let was_strict = p.ctx.strict
  use p <- result.try(apply_body_use_strict(p))
  // §10.2.11 step 28: shims take slots 0..arity-1
  let scopes = class_scopes.declare_param_shims(p.scopes, params)
  // §10.2.11 step 18: implicit arguments, slot order matters
  let scopes =
    scope_builder.declare(
      scopes,
      "arguments",
      scope.VarBinding,
      synthetic: True,
    )
  let p = Parser(..p, scopes:)
  use Nil <- result.try(case !was_strict && p.ctx.strict {
    True -> {
      use Nil <- result.try(check_pending_strict_function_name(p))
      check_param_names_for_dups(p)
    }
    False -> Ok(Nil)
  })
  parse_function_body(p, params)
}

fn parse_braced_body(
  p: Parser,
) -> Result(#(Parser, List(ast.StmtWithLine)), ParseError) {
  use p2 <- result.try(expect(p, LeftBrace))
  // snapshot so the reorder only moves body children
  let body_id = p2.scopes.current
  let mark = scope_builder.children_newest_first(p2.scopes, body_id)
  use #(p3, stmts) <- result.try(
    parse_statement_list(p2, top_level: False, acc: []),
  )
  // backstop for deferred cover-grammar errors
  use Nil <- result.try(check_cover_grammar_errors(p3, pos_of(p3)))
  use p4 <- result.try(expect(p3, RightBrace))
  // the one chokepoint for bodies without their own block scope
  let p4 =
    Parser(
      ..p4,
      scopes: scope_builder.reorder_body_children(p4.scopes, body_id, mark),
    )
  Ok(#(p4, stmts))
}

// §10.2.11 step 28 body scope; lockstep with emit, never pruned
fn parse_function_body(
  p: Parser,
  params: List(ast.Pattern),
) -> Result(#(Parser, List(ast.StmtWithLine)), ParseError) {
  case ast_util.fixed_params_non_simple(params) {
    False -> parse_braced_body(p)
    True -> {
      let fn_id = p.scopes.current
      let #(scopes, _body_id) = scope_builder.push_var_boundary(p.scopes)
      use #(p2, body) <- result.map(parse_braced_body(Parser(..p, scopes:)))
      // flip fn root children to source order
      let scopes = scope_builder.enter(p2.scopes, fn_id)
      #(
        Parser(
          ..p2,
          scopes: scope_builder.reorder_block_children(scopes, fn_id),
        ),
        body,
      )
    }
  }
}

fn check_pending_strict_function_name(p: Parser) -> Result(Nil, ParseError) {
  case p.ctx.pending_strict_name {
    None -> Ok(Nil)
    Some(name) ->
      case strict_binding_violation(name) {
        Some(_) -> Error(StrictModeBindingName(pos_of(p), name))
        None -> Ok(Nil)
      }
  }
}

// the prologue already rejected reserved names; only dups remain
fn check_param_names_for_dups(p: Parser) -> Result(Nil, ParseError) {
  case first_duplicate(p.ctx.param_bound_names) {
    Some(name) -> Error(DuplicateParamNameStrictMode(pos_of(p), name))
    None -> Ok(Nil)
  }
}

// a non-simple list makes earlier duplicates an error too
fn mark_non_simple_params(p: Parser) -> Result(Parser, ParseError) {
  case first_duplicate(p.ctx.param_bound_names) {
    Some(name) -> Error(DuplicateParameterName(pos_of(p), name))
    None ->
      Ok(Parser(..p, ctx: GrammarContext(..p.ctx, has_non_simple_param: True)))
  }
}

fn first_duplicate(names: List(String)) -> Option(String) {
  first_duplicate_loop(names, set.new())
}

fn first_duplicate_loop(
  names: List(String),
  seen: Set(String),
) -> Option(String) {
  case names {
    [] -> None
    [name, ..rest] ->
      case set.contains(seen, name) {
        True -> Some(name)
        False -> first_duplicate_loop(rest, set.insert(seen, name))
      }
  }
}

fn first_strict_violation(
  names: List(String),
) -> Option(#(String, StrictNameKind)) {
  list.find_map(names, fn(name) {
    case strict_binding_violation(name) {
      Some(kind) -> Ok(#(name, kind))
      None -> Error(Nil)
    }
  })
  |> option.from_result
}

type StrictNameKind {
  EvalOrArguments
  ReservedWord
}

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

fn parse_getter_params_and_body(
  p: Parser,
) -> Result(#(Parser, List(ast.Pattern), List(ast.StmtWithLine)), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  use <- bool.guard(peek(p2) != RightParen, Error(GetterNoParams(pos_of(p2))))
  use #(p3, body) <- result.map(begin_function_body(advance(p2), []))
  #(p3, [], body)
}

fn parse_setter_params_and_body(
  p: Parser,
) -> Result(#(Parser, List(ast.Pattern), List(ast.StmtWithLine)), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  let p2 =
    Parser(
      ..p2,
      ctx: GrammarContext(
        ..p2.ctx,
        in_formal_params: True,
        declaring: DeclaringParam,
      ),
    )
  let set_non_simple = fn(p: Parser, non_simple) {
    use <- bool.guard(!non_simple, p)
    Parser(..p, ctx: GrammarContext(..p.ctx, has_non_simple_param: True))
  }
  case peek(p2) {
    RightParen -> Error(SetterExactlyOneParam(pos_of(p2)))
    DotDotDot -> Error(SetterNoRest(pos_of(p2)))
    _ -> {
      let p2 = set_non_simple(p2, option.is_none(simple_binding_name(p2)))
      use #(p3, pat) <- result.try(parse_binding_pattern(p2))
      let p3 = set_non_simple(p3, peek(p3) == Equal)
      let default_pos = pos_of(p3)
      use #(p4, final_pat) <- result.try(parse_pattern_default(p3, pat))
      use Nil <- result.try(check_cover_grammar_errors(p4, default_pos))
      case peek(p4) {
        RightParen -> {
          let p5 =
            Parser(
              ..advance(p4),
              ctx: GrammarContext(
                ..p4.ctx,
                in_formal_params: False,
                declaring: NotDeclaring,
              ),
            )
          use #(p6, body) <- result.map(begin_function_body(p5, [final_pat]))
          #(p6, [final_pat], body)
        }
        Comma -> Error(SetterExactlyOneParam(pos_of(p4)))
        _ -> Error(ExpectedCloseAfterSetter(pos_of(p4)))
      }
    }
  }
}

fn parse_method_params_body(
  p: Parser,
  outer: Parser,
  prefix: MethodPrefix,
  is_constructor is_constructor: Bool,
  has_extends has_extends: Bool,
) -> Result(#(Parser, List(ast.Pattern), List(ast.StmtWithLine)), ParseError) {
  let is_plain = prefix.accessor == NoAccessor
  let ctx =
    enter_method_context(
      p,
      is_generator: prefix.is_generator && is_plain,
      is_async: prefix.is_async && is_plain,
      is_constructor: is_constructor && is_plain,
      has_super_class: has_extends,
    )
  case prefix.accessor {
    GetPrefix -> parse_getter_params_and_body(ctx)
    SetPrefix -> parse_setter_params_and_body(ctx)
    NoAccessor -> parse_function_params_and_body(ctx)
  }
  |> restore_context_fn(outer)
}

fn parse_formal_parameters(
  p: Parser,
) -> Result(#(Parser, List(ast.Pattern)), ParseError) {
  case peek(p) {
    RightParen -> Ok(#(p, []))
    _ -> {
      let start = pos_of(p)
      use #(p2, params) <- result.try(
        parse_formal_parameter_list(p, set.new(), []),
      )
      // parameter defaults owe their cover-grammar errors now
      use Nil <- result.map(check_cover_grammar_errors(p2, start))
      #(p2, params)
    }
  }
}

fn parse_formal_parameter_list(
  p: Parser,
  seen: Set(String),
  acc: List(ast.Pattern),
) -> Result(#(Parser, List(ast.Pattern)), ParseError) {
  case peek(p) {
    DotDotDot -> {
      use p <- result.try(ensure_non_simple_params(p))
      let p2 = advance(p)
      use Nil <- result.try(check_duplicate_param(
        p2,
        simple_binding_name(p2),
        seen,
      ))
      use #(p3, inner_pat) <- result.try(parse_binding_pattern(p2))
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
      let param_name = simple_binding_name(p)
      use p <- result.try(case param_name {
        None -> ensure_non_simple_params(p)
        Some(_) -> Ok(p)
      })
      use Nil <- result.try(check_duplicate_param(p, param_name, seen))
      let seen = case param_name {
        None -> seen
        Some(name) -> set.insert(seen, name)
      }
      use #(p2, pat) <- result.try(parse_binding_pattern(p))
      use p2 <- result.try(case peek(p2) {
        Equal -> ensure_non_simple_params(p2)
        _ -> Ok(p2)
      })
      use #(p3, final_pat) <- result.try(parse_pattern_default(p2, pat))
      parse_formal_param_rest(p3, seen, [final_pat, ..acc])
    }
  }
}

fn ensure_non_simple_params(p: Parser) -> Result(Parser, ParseError) {
  case p.ctx.has_non_simple_param {
    True -> Ok(p)
    False -> mark_non_simple_params(p)
  }
}

fn parse_formal_param_rest(
  p: Parser,
  seen: Set(String),
  acc: List(ast.Pattern),
) -> Result(#(Parser, List(ast.Pattern)), ParseError) {
  case peek(p) {
    Comma ->
      case peek_at(p, 1) {
        RightParen -> Ok(#(advance(p), list.reverse(acc)))
        _ -> parse_formal_parameter_list(advance(p), seen, acc)
      }
    _ -> Ok(#(p, list.reverse(acc)))
  }
}

fn simple_binding_name(p: Parser) -> Option(String) {
  case is_binding_ident_token(peek(p)) {
    True -> Some(peek_value(p))
    False -> None
  }
}

fn check_duplicate_param(
  p: Parser,
  name: Option(String),
  seen: Set(String),
) -> Result(Nil, ParseError) {
  case name {
    None -> Ok(Nil)
    Some(name) -> {
      use <- bool.guard(
        params_must_be_unique(p.ctx) && set.contains(seen, name),
        Error(DuplicateParameterName(pos_of(p), name)),
      )
      Ok(Nil)
    }
  }
}

// annex b §B.3.4
fn check_for_of_var_vs_catch_param(
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

fn parse_class_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  use #(p2, decl) <- result.map(parse_class_declaration(p))
  #(p2, ast.declaration_to_statement(decl))
}

fn parse_class_declaration(
  p: Parser,
) -> Result(#(Parser, ast.Declaration), ParseError) {
  use #(p2, ClassSyntax(name:, super_class:, body:)) <- result.map(
    parse_class_head_and_tail(p, name_required: True, register_name: True),
  )
  #(p2, ast.DeclareClass(name:, super_class:, body:))
}

fn parse_class_head_and_tail(
  p: Parser,
  name_required name_required: Bool,
  register_name register_name: Bool,
) -> Result(#(Parser, ClassSyntax), ParseError) {
  let p2 = advance(p)
  case simple_binding_name(p2) {
    Some(name) -> {
      let name_span = span_of(p2)
      use Nil <- result.try(check_binding_identifier(
        Parser(..p2, ctx: GrammarContext(..p2.ctx, strict: True)),
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
      let name = Some(ast.NamedBinding(name:, span: name_span))
      #(p4, ClassSyntax(name:, super_class:, body:))
    }
    None -> {
      use <- bool.guard(name_required, Error(ExpectedIdentifier(pos_of(p2))))
      use #(p3, super_class, body) <- result.map(parse_class_tail(p2, None))
      #(p3, ClassSyntax(name: None, super_class:, body:))
    }
  }
}

type ClassSyntax {
  ClassSyntax(
    name: Option(ast.NamedBinding),
    super_class: Option(ast.Expression),
    body: List(ast.ClassElement),
  )
}

// private names declared in one class body (§15.7.1)
type DeclaredPrivateName {
  DeclaredPrivateName(is_static: Bool, kind: PrivateNameKind)
}

type PrivateNameKind {
  PrivateGet
  PrivateSet
  PrivateGetSet
  PrivateOther
}

fn parse_class_tail(
  p: Parser,
  name: Option(String),
) -> Result(
  #(Parser, Option(ast.Expression), List(ast.ClassElement)),
  ParseError,
) {
  let saved_strict = p.ctx.strict
  let outer_current = p.scopes.current
  // §15.7.14: class scope is pushed before the heritage
  let #(scopes, class_id) = scope_builder.push(p.scopes, scope.ClassBody)
  let p = Parser(..p, scopes:, ctx: GrammarContext(..p.ctx, strict: True))
  // once extends is consumed errors propagate, no backtrack
  let has_extends = peek(p) == Extends
  use #(p2, super_class) <- result.try(case has_extends {
    True -> {
      use #(p2, expr) <- result.map(parse_left_hand_side_expression(advance(p)))
      #(p2, Some(expr))
    }
    False -> Ok(#(p, None))
  })
  let heritage_scopes =
    scope_builder.children_newest_first(p2.scopes, class_id) |> list.reverse
  use p3 <- result.try(expect(p2, LeftBrace))
  // pre-create init shells; unneeded ones dropped at }
  let #(scopes, init_id) = scope_builder.push(p3.scopes, scope.Function)
  let scopes = scope_builder.enter(scopes, class_id)
  let #(scopes, static_id) = scope_builder.push(scopes, scope.Function)
  let scopes = scope_builder.enter(scopes, class_id)
  let ids = class_scopes.ClassScopeIds(class_id:, init_id:, static_id:)
  // §15.7.14: heritage uses the outer private depth
  let outer_depth = p3.class_body_depth
  let p3 = Parser(..p3, scopes:, class_body_depth: outer_depth + 1)
  use #(p4, rev_parsed, declared) <- result.try(
    parse_class_body(
      p3,
      ids,
      has_extends:,
      has_constructor: False,
      private_names: dict.new(),
      acc: [],
    ),
  )
  use p4 <- result.try(resolve_private_refs(p4, outer_depth, declared))
  let parsed = list.reverse(rev_parsed)
  let elements = list.map(parsed, fn(el) { el.element })
  // child order must match class_scope_finalize for emit's cursor
  let scopes =
    class_scopes.class_scope_finalize(
      p4.scopes,
      ids,
      name,
      has_super_class: has_extends,
      heritage_scopes:,
      parsed:,
    )
  let scopes = scope_builder.enter(scopes, outer_current)
  Ok(#(
    Parser(
      ..p4,
      scopes:,
      ctx: GrammarContext(..p4.ctx, strict: saved_strict),
      class_body_depth: outer_depth,
    ),
    super_class,
    elements,
  ))
}

fn parse_class_body(
  p: Parser,
  ids: class_scopes.ClassScopeIds,
  has_extends has_extends: Bool,
  has_constructor has_constructor: Bool,
  private_names private_names: Dict(String, DeclaredPrivateName),
  acc acc: List(class_scopes.ParsedClassElement),
) -> Result(
  #(
    Parser,
    List(class_scopes.ParsedClassElement),
    Dict(String, DeclaredPrivateName),
  ),
  ParseError,
) {
  case peek(p) {
    RightBrace -> Ok(#(advance(p), acc, private_names))
    Semicolon ->
      parse_class_body(
        advance(p),
        ids,
        has_extends:,
        has_constructor:,
        private_names:,
        acc:,
      )
    _ -> {
      use #(p2, parsed) <- result.try(parse_class_element(
        p,
        ids,
        has_extends:,
        has_constructor:,
      ))
      use private_names <- result.try(register_private_name(
        p2,
        private_names,
        parsed.element,
      ))
      let found_constructor = case parsed.element {
        ast.ClassMethod(kind: ast.ConstructorMethod, ..) -> True
        _ -> False
      }
      parse_class_body(
        p2,
        ids,
        has_extends:,
        has_constructor: has_constructor || found_constructor,
        private_names:,
        acc: [parsed, ..acc],
      )
    }
  }
}

// §15.7.1: only a getter+setter pair may share a private name
fn register_private_name(
  p: Parser,
  private_names: Dict(String, DeclaredPrivateName),
  element: ast.ClassElement,
) -> Result(Dict(String, DeclaredPrivateName), ParseError) {
  case private_element_name(element) {
    None -> Ok(private_names)
    Some(#(name, declared)) ->
      case dict.get(private_names, name) {
        Error(Nil) -> Ok(dict.insert(private_names, name, declared))
        Ok(previous) ->
          case
            previous.is_static == declared.is_static,
            previous.kind,
            declared.kind
          {
            True, PrivateGet, PrivateSet | True, PrivateSet, PrivateGet ->
              Ok(dict.insert(
                private_names,
                name,
                DeclaredPrivateName(..declared, kind: PrivateGetSet),
              ))
            _, _, _ -> Error(DuplicatePrivateName(pos_of(p), name))
          }
      }
  }
}

fn private_element_name(
  element: ast.ClassElement,
) -> Option(#(String, DeclaredPrivateName)) {
  case element {
    ast.ClassMethod(key: ast.PrivateName(name:, ..), kind:, is_static:, ..) -> {
      let kind = case kind {
        ast.GetterMethod -> PrivateGet
        ast.SetterMethod -> PrivateSet
        ast.PlainMethod | ast.ConstructorMethod -> PrivateOther
      }
      Some(#(name, DeclaredPrivateName(is_static:, kind:)))
    }
    ast.ClassField(key: ast.PrivateName(name:, ..), is_static:, ..) ->
      Some(#(name, DeclaredPrivateName(is_static:, kind: PrivateOther)))
    ast.ClassMethod(..) | ast.ClassField(..) | ast.StaticBlock(..) -> None
  }
}

// in a class body a keyword is a name when followed by ( = ; }
fn ends_class_element_name(kind: TokenKind) -> Bool {
  case kind {
    LeftParen | Equal | Semicolon | RightBrace -> True
    _ -> False
  }
}

fn parse_class_element(
  p: Parser,
  ids: class_scopes.ClassScopeIds,
  has_extends has_extends: Bool,
  has_constructor has_constructor: Bool,
) -> Result(#(Parser, class_scopes.ParsedClassElement), ParseError) {
  let is_static = peek(p) == Static && !ends_class_element_name(peek_at(p, 1))
  let p2 = case is_static {
    True -> advance(p)
    False -> p
  }
  // §15.7.1 static block, not a method named static
  use <- bool.lazy_guard(is_static && peek(p2) == LeftBrace, fn() {
    parse_static_block(p2, ids)
  })
  // get * is a field named get plus an asi generator
  let #(p3, prefix) =
    parse_method_prefix(p2, ends_class_element_name, star_ends_accessor: True)
  // snapshot to diff out computed-key scopes
  let key_before = scope_builder.children_newest_first(p3.scopes, ids.class_id)
  use #(p4, key) <- result.try(parse_property_name(p3))
  let key_scopes =
    class_scopes.class_new_children(p4.scopes, ids.class_id, key_before)
  // §15.7.1 checks use the decoded key
  let static_name = ast.static_name(key)
  use <- bool.guard(
    is_static && static_name == Some("prototype"),
    Error(StaticPrototype(pos_of(p3))),
  )
  let is_constructor_name = static_name == Some("constructor")
  let is_constructor = !is_static && is_constructor_name
  use Nil <- result.try(case is_constructor {
    True -> check_constructor_prefix(prefix, has_constructor:, pos: pos_of(p3))
    False -> Ok(Nil)
  })
  use <- bool.guard(
    is_private_constructor_key(key),
    Error(PrivateNameConstructor(pos_of(p4))),
  )
  case peek(p4) {
    LeftParen -> {
      use #(p5, value, method_fn_id) <- result.map(parse_class_method_value(
        p4,
        p,
        ids,
        prefix,
        is_constructor:,
        has_extends:,
      ))
      let kind = case is_constructor, prefix.accessor {
        True, _ -> ast.ConstructorMethod
        False, GetPrefix -> ast.GetterMethod
        False, SetPrefix -> ast.SetterMethod
        False, NoAccessor -> ast.PlainMethod
      }
      let element = ast.ClassMethod(key:, value:, kind:, is_static:)
      #(
        p5,
        class_scopes.ParsedClassElement(
          element,
          class_scopes.MethodScopes(key_scopes:, method_fn_id:),
        ),
      )
    }
    _ -> {
      // §15.7.1: field named constructor is forbidden
      use <- bool.guard(
        is_constructor_name,
        Error(FieldNamedConstructor(pos_of(p4))),
      )
      use #(p5, value) <- result.map(parse_class_field_rest(
        p4,
        p,
        ids,
        is_static:,
      ))
      let element = ast.ClassField(key:, value:, is_static:)
      #(
        p5,
        class_scopes.ParsedClassElement(
          element,
          class_scopes.NonMethodScopes(key_scopes:),
        ),
      )
    }
  }
}

// step (7): a static block is an arrow child of the static shell
fn parse_static_block(
  p: Parser,
  ids: class_scopes.ClassScopeIds,
) -> Result(#(Parser, class_scopes.ParsedClassElement), ParseError) {
  let p_static =
    Parser(..p, scopes: scope_builder.enter(p.scopes, ids.static_id))
  let p_body = enter_static_block_context(p_static)
  let scopes =
    scope_builder.update_current(p_body.scopes, fn(s) {
      scope_builder.RawScope(..s, kind: scope.Function)
    })
    |> scope_builder.update_current_fn(fn(fi) {
      scope_builder.RawFunctionInfo(..fi, is_arrow: True)
    })
  // not parse_block_body: no block between arrow scope and body
  use #(p2, block) <- result.map(parse_braced_body(Parser(..p_body, scopes:)))
  // re-enter class_id so the next element parents correctly
  let p2 =
    Parser(
      ..restore_outer_context(p2, p_static),
      scopes: scope_builder.enter(p2.scopes, ids.class_id),
    )
  #(
    p2,
    class_scopes.ParsedClassElement(
      ast.StaticBlock(body: block),
      class_scopes.NonMethodScopes([]),
    ),
  )
}

// §15.7.1: constructor must be a plain method, declared once
fn check_constructor_prefix(
  prefix: MethodPrefix,
  has_constructor has_constructor: Bool,
  pos pos: Int,
) -> Result(Nil, ParseError) {
  case prefix.accessor {
    GetPrefix -> Error(ClassConstructorNotGetter(pos))
    SetPrefix -> Error(ClassConstructorNotSetter(pos))
    NoAccessor -> {
      use <- bool.guard(
        prefix.is_generator,
        Error(ClassConstructorGenerator(pos)),
      )
      use <- bool.guard(prefix.is_async, Error(ClassConstructorAsync(pos)))
      use <- bool.guard(has_constructor, Error(ClassDuplicateConstructor(pos)))
      Ok(Nil)
    }
  }
}

// §15.7.1: #constructor is forbidden
fn is_private_constructor_key(key: ast.PropertyName) -> Bool {
  case key {
    ast.PrivateName(name: "#constructor", ..)
    | ast.StringName(value: "#constructor", ..) -> True
    _ -> False
  }
}

// p is at the ( after the method name
fn parse_class_method_value(
  p: Parser,
  outer: Parser,
  ids: class_scopes.ClassScopeIds,
  prefix: MethodPrefix,
  is_constructor is_constructor: Bool,
  has_extends has_extends: Bool,
) -> Result(#(Parser, ast.FunctionLiteral, scope.ScopeId), ParseError) {
  // method scope is a direct child of class_id; capture by diff
  let body_before = scope_builder.children_newest_first(p.scopes, ids.class_id)
  use #(p2, params, body) <- result.map(parse_method_params_body(
    p,
    outer,
    prefix,
    is_constructor:,
    has_extends:,
  ))
  // never empty: a method pushed exactly one function scope
  let assert [method_fn_id, ..] =
    class_scopes.class_new_children(p2.scopes, ids.class_id, body_before)
    as "parser: class method body pushed no Function scope"
  let value =
    ast.FunctionLiteral(
      name: None,
      params:,
      body:,
      is_generator: prefix.is_generator,
      is_async: prefix.is_async,
    )
  #(p2, value, method_fn_id)
}

// p is just past the field name; consumes through the terminator
fn parse_class_field_rest(
  p: Parser,
  outer: Parser,
  ids: class_scopes.ClassScopeIds,
  is_static is_static: Bool,
) -> Result(#(Parser, Option(ast.Expression)), ParseError) {
  use #(p2, value) <- result.try(case peek(p) {
    Equal -> {
      let shell_id = case is_static {
        True -> ids.static_id
        False -> ids.init_id
      }
      // field initializer is [+In]
      use #(p2, init) <- result.map(allowing_in(
        enter_field_initializer_context(advance(p), shell_id),
        parse_assignment_expression,
      ))
      #(exit_field_initializer_context(p2, outer, ids.class_id), Some(init))
    }
    _ -> Ok(#(p, None))
  })
  use p3 <- result.map(eat_semicolon(p2))
  #(p3, value)
}

// initializer scopes parent under the init shell, not the class body
fn enter_field_initializer_context(
  p: Parser,
  shell_id: scope.ScopeId,
) -> Parser {
  Parser(
    ..p,
    scopes: scope_builder.enter(p.scopes, shell_id),
    ctx: GrammarContext(
      ..p.ctx,
      allow_super_property: True,
      allow_super_call: False,
      allow_new_target: True,
      in_class_field_init: True,
    ),
  )
}

fn exit_field_initializer_context(
  p: Parser,
  outer: Parser,
  class_id: scope.ScopeId,
) -> Parser {
  Parser(
    ..p,
    scopes: scope_builder.enter(p.scopes, class_id),
    ctx: GrammarContext(
      ..p.ctx,
      allow_super_property: outer.ctx.allow_super_property,
      allow_super_call: outer.ctx.allow_super_call,
      allow_new_target: outer.ctx.allow_new_target,
      in_class_field_init: outer.ctx.in_class_field_init,
    ),
  )
}

fn async_function_start(p: Parser) -> Bool {
  peek_at(p, 1) == Function && token_line_at(p, 1) == token_line_at(p, 0)
}

fn at_label_start(p: Parser) -> Bool {
  case peek(p) {
    Identifier | Async | Yield | Await -> peek_at(p, 1) == Colon
    _ -> False
  }
}

// continue may target any label of a loop's chain (§14.13.1)
fn parse_labeled_statement(
  p: Parser,
  collected: List(String),
) -> Result(#(Parser, ast.Statement), ParseError) {
  let label = peek_value(p)
  use Nil <- result.try(check_reserved_identifier_common(p, label))
  let duplicate =
    result.is_ok(list.key_find(p.ctx.label_set, label))
    || list.contains(collected, label)
  use <- bool.guard(duplicate, Error(DuplicateLabel(pos_of(p), label)))
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, Colon))
  let collected = [label, ..collected]
  case at_label_start(p3) {
    True -> parse_labeled_statement(p3, collected)
    False -> parse_labeled_statement_body(p3, collected)
  }
}

fn parse_labeled_statement_body(
  p: Parser,
  labels: List(String),
) -> Result(#(Parser, ast.Statement), ParseError) {
  let kind = case peek(p) {
    While | Do | For -> LoopLabel
    _ -> PlainLabel
  }
  let outer_labels = p.ctx.label_set
  let label_set =
    list.append(list.map(labels, fn(label) { #(label, kind) }), outer_labels)
  let p = Parser(..p, ctx: GrammarContext(..p.ctx, label_set:))
  let wrap_label = fn(res) {
    use #(inner_p, stmt) <- result.map(res)
    let labeled =
      list.fold(labels, stmt, fn(body, label) {
        ast.LabeledStatement(label:, body:)
      })
    #(
      Parser(
        ..inner_p,
        ctx: GrammarContext(..inner_p.ctx, label_set: outer_labels),
      ),
      labeled,
    )
  }
  case peek(p) {
    Const -> Error(LexicalDeclInLabel(pos_of(p)))
    Let -> {
      use <- bool.guard(
        let_declaration_forbidden_here(p),
        Error(LexicalDeclInLabel(pos_of(p))),
      )
      case let_starts_declaration(p) {
        True -> wrap_label(parse_expression_statement(p))
        False -> wrap_label(parse_statement(p))
      }
    }
    Function -> {
      use <- bool.guard(
        peek_at(p, 1) == Star,
        Error(GeneratorDeclLabeled(pos_of(p))),
      )
      use <- bool.guard(
        p.ctx.strict || p.ctx.in_single_stmt_pos,
        Error(FunctionDeclInLabelBody(pos_of(p))),
      )
      wrap_label(parse_statement(p))
    }
    Class -> Error(LexicalDeclInLabel(pos_of(p)))
    // only a plain function may be a labelled item
    Async ->
      case async_function_start(p) {
        True -> Error(FunctionDeclInLabelBody(pos_of(p)))
        False -> wrap_label(parse_statement(p))
      }
    Identifier ->
      case is_using_decl_start(p, 0) {
        True -> Error(LexicalDeclInLabel(pos_of(p)))
        False -> wrap_label(parse_statement(p))
      }
    Await ->
      case is_await_using_decl_start(p) {
        True -> Error(LexicalDeclInLabel(pos_of(p)))
        False -> wrap_label(parse_statement(p))
      }
    _ -> wrap_label(parse_statement(p))
  }
}

fn parse_with_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  use <- bool.guard(p.ctx.strict, Error(WithNotAllowedStrictMode(pos_of(p))))
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, object) <- result.try(parse_expression(p3))
  use p5 <- result.try(expect(p4, RightParen))
  let #(scopes, with_id) = scope_builder.push_with(p5.scopes)
  use #(p6, body) <- result.try(parse_single_statement(
    Parser(..p5, scopes:),
    allow_annex_b_function: False,
  ))
  // flip children to source order for finalize
  let scopes = scope_builder.reorder_block_children(p6.scopes, with_id)
  Ok(#(
    Parser(..p6, scopes: scope_builder.enter(scopes, p5.scopes.current)),
    ast.WithStatement(object:, body:),
  ))
}

// raise deferred cover-grammar errors once known not a pattern
fn check_cover_grammar_errors(p: Parser, pos: Int) -> Result(Nil, ParseError) {
  case p.ctx.has_cover_initializer, p.ctx.dup_proto_pos {
    True, _ -> Error(ShorthandDefaultOutsideDestructuring(pos))
    False, Some(dup_pos) -> Error(DuplicateProtoProperty(dup_pos))
    False, None -> Ok(Nil)
  }
}

fn parse_expression_statement(
  p: Parser,
) -> Result(#(Parser, ast.Statement), ParseError) {
  // raw text needed for directives
  let directive_raw = case peek(p) {
    StringLiteral -> Some(peek_value(p))
    _ -> None
  }
  use #(p2, expr) <- result.try(parse_expression(p))
  use Nil <- result.try(check_cover_grammar_errors(p2, pos_of(p)))
  use p3 <- result.try(eat_semicolon(p2))
  // directive only if exactly a string literal
  let directive = case expr {
    ast.StringLiteral(..) -> directive_raw
    _ -> None
  }
  Ok(#(p3, ast.ExpressionStatement(expression: expr, directive:)))
}

fn parse_expression(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  use #(p2, first_expr) <- result.try(parse_assignment_expression(p))
  case peek(p2) {
    Comma ->
      case peek_at(p2, 1) {
        RightParen | RightBracket | RightBrace | Eof -> Ok(#(p2, first_expr))
        _ -> {
          let p3 = advance(p2)
          use #(p4, rest_expr) <- result.try(parse_expression(p3))
          Ok(#(
            Parser(..p4, last_expr_assignable: False),
            ast.SequenceExpression(
              expressions: [first_expr, rest_expr],
              span: ast.Span(first_expr.span.start, p4.prev_end),
            ),
          ))
        }
      }
    _ -> Ok(#(p2, first_expr))
  }
}

fn parse_assignment_expression(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  case peek(p) {
    Yield -> {
      case p.ctx.in_generator {
        False -> parse_arrow_or_assignment(p)
        True ->
          case peek_at(p, 1) {
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
            | Arrow -> parse_arrow_or_assignment(p)
            // no LeftBracket: yield [..] is a yield expression
            _ -> parse_yield_expression(p)
          }
      }
    }
    _ -> parse_arrow_or_assignment(p)
  }
}

fn parse_arrow_or_assignment(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  case try_arrow_function(p) {
    Ok(#(p2, arrow_expr)) -> Ok(#(p2, arrow_expr))
    // error after => is committed, never backtrack
    Error(ArrowError(e)) -> Error(e)
    Error(NotAnArrow) -> {
      let name = peek_value(p)
      let strict_eval_assignment =
        p.ctx.strict
        && peek(p) == Identifier
        && { name == "eval" || name == "arguments" }
        && option.is_some(assignment_op(peek_at(p, 1)))
      use <- bool.guard(
        strict_eval_assignment,
        Error(StrictModeAssignment(pos_of(p), name)),
      )
      parse_assignment_or_conditional(p)
    }
  }
}

fn parse_assignment_or_conditional(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let lhs_start = peek(p)
  use #(p2, lhs) <- result.try(parse_conditional_expression(p))
  case peek(p2), assignment_op(peek(p2)) {
    Equal, _ -> {
      use #(p2, kind) <- result.try(check_plain_assignment_target(
        p2,
        lhs,
        lhs_start,
      ))
      finish_assignment(p2, lhs, ast.Assign, kind)
    }
    _, Some(op) -> {
      // annex b call targets ok for op= but not logical assignment
      let web_compat_ok = case op {
        ast.LogicalAndAssign
        | ast.LogicalOrAssign
        | ast.NullishCoalesceAssign -> False
        _ -> is_web_compat_call_target(p2, lhs)
      }
      use <- bool.guard(
        !p2.last_expr_assignable && !web_compat_ok,
        Error(InvalidAssignmentLhs(pos_of(p2))),
      )
      use Nil <- result.try(check_strict_restricted_target(p2, lhs))
      finish_assignment(p2, lhs, op, CompoundAssign)
    }
    // clear so it does not leak from a sibling
    _, None ->
      case p2.last_expr_is_assignment {
        True -> Ok(#(Parser(..p2, last_expr_is_assignment: False), lhs))
        False -> Ok(#(p2, lhs))
      }
  }
}

// lhs must be a reference, a literal pattern or an annex b call
fn check_plain_assignment_target(
  p: Parser,
  lhs: ast.Expression,
  lhs_start: TokenKind,
) -> Result(#(Parser, AssignmentKind), ParseError) {
  use <- bool.lazy_guard(p.last_expr_assignable, fn() {
    use Nil <- result.map(check_strict_restricted_target(p, lhs))
    #(p, PlainAssign)
  })
  case lhs_start {
    LeftBrace | LeftBracket -> {
      use <- bool.guard(
        p.literal_invalid_as_pattern,
        Error(InvalidDestructuringTarget(pos_of(p))),
      )
      use <- bool.guard(
        p.ctx.strict && pattern_has_eval_args_target(lhs),
        Error(EvalArgsAssignStrictMode(pos_of(p))),
      )
      let p =
        Parser(
          ..p,
          literal_invalid_as_pattern: False,
          ctx: GrammarContext(..p.ctx, dup_proto_pos: None),
        )
      Ok(#(p, PlainAssign))
    }
    _ -> {
      use <- bool.guard(
        !is_web_compat_call_target(p, lhs),
        Error(InvalidAssignmentLhs(pos_of(p))),
      )
      Ok(#(p, CallTargetAssign))
    }
  }
}

// p is at the operator; PlainAssign lets {a: b = 1} cover a pattern
fn finish_assignment(
  p: Parser,
  lhs: ast.Expression,
  op: ast.AssignmentOp,
  kind: AssignmentKind,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let p =
    Parser(
      ..p,
      scopes: class_scopes.mark_assign_targets(p.scopes, lhs),
      ctx: GrammarContext(..p.ctx, has_cover_initializer: False),
    )
  use #(p2, rhs) <- result.map(parse_assignment_expression(advance(p)))
  let p_out = case kind {
    PlainAssign ->
      Parser(..p2, last_expr_assignable: False, last_expr_is_assignment: True)
    CallTargetAssign ->
      Parser(..p2, last_expr_assignable: False, last_expr_is_assignment: False)
    CompoundAssign -> Parser(..p2, last_expr_assignable: False)
  }
  #(
    p_out,
    ast.AssignmentExpression(
      operator: op,
      left: lhs,
      right: rhs,
      span: ast.Span(lhs.span.start, p2.prev_end),
    ),
  )
}

// §13.15.1, including (eval) = 1
fn check_strict_restricted_target(
  p: Parser,
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

// annex b web-compat call target; never optional chains (§13.3.1.1)
fn is_web_compat_call_target(p: Parser, lhs: ast.Expression) -> Bool {
  case p.ctx.strict, ast_util.unwrap_parens(lhs) {
    False, ast.CallExpression(callee:, ..) ->
      !ast_util.chain_has_optional(callee)
    _, _ -> False
  }
}

// ArrowError is committed (after =>): propagate, never backtrack
type ArrowAttempt {
  NotAnArrow
  ArrowError(ParseError)
}

fn try_arrow_function(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ArrowAttempt) {
  case peek(p) {
    // §15.9: no line terminator after async
    Async -> {
      let same_line = token_line_at(p, 1) == token_line_at(p, 0)
      case peek_at(p, 1) {
        LeftParen if same_line ->
          case paren_arrow_ahead(look_skip(look_skip(look_from(p)))) {
            True -> try_paren_arrow(p, advance(advance(p)), is_async: True)
            False -> Error(NotAnArrow)
          }
        Arrow -> try_single_ident_arrow(p, p, is_async: False)
        next if same_line ->
          case is_arrow_param_name(next), peek_at(p, 2) {
            True, Arrow -> try_single_ident_arrow(p, advance(p), is_async: True)
            _, _ -> Error(NotAnArrow)
          }
        _ -> Error(NotAnArrow)
      }
    }
    Identifier | Yield | Await | Of | From | As | Let | Static ->
      case peek_at(p, 1) {
        Arrow -> try_single_ident_arrow(p, p, is_async: False)
        _ -> Error(NotAnArrow)
      }
    LeftParen ->
      case paren_arrow_ahead(look_skip(look_from(p))) {
        True -> try_paren_arrow(p, advance(p), is_async: False)
        False -> Error(NotAnArrow)
      }
    _ -> Error(NotAnArrow)
  }
}

// cheap bracket-balance scan like quickjs js_parse_skip_parens_token
fn paren_arrow_ahead(look: Look) -> Bool {
  let #(first, look) = look_next(look)
  case first.kind {
    RightParen | DotDotDot -> True
    LeftBracket | LeftBrace -> balanced_arrow_ahead(look, 2, 64)
    kind ->
      is_binding_ident_token(kind)
      && {
        let #(second, look) = look_next(look)
        case second.kind {
          RightParen -> { look_next(look).0 }.kind == Arrow
          Comma | Equal -> balanced_arrow_ahead(look, 1, 64)
          _ -> False
        }
      }
  }
}

fn balanced_arrow_ahead(look: Look, depth: Int, budget: Int) -> Bool {
  use <- bool.guard(budget <= 0, True)
  let #(token, look) = look_next(look)
  case token.kind {
    LeftParen | LeftBracket | LeftBrace ->
      balanced_arrow_ahead(look, depth + 1, budget - 1)
    RightParen if depth == 1 -> { look_next(look).0 }.kind == Arrow
    RightBracket | RightBrace if depth == 1 -> False
    RightParen | RightBracket | RightBrace ->
      balanced_arrow_ahead(look, depth - 1, budget - 1)
    Slash | SlashEqual | TemplateHead | Illegal | LexFailure(_) | Eof -> True
    _ -> balanced_arrow_ahead(look, depth, budget - 1)
  }
}

fn is_arrow_param_name(kind: TokenKind) -> Bool {
  case kind {
    Identifier | Yield | Await | Of | From | As | Let | Static | Async -> True
    _ -> False
  }
}

fn try_single_ident_arrow(
  outer: Parser,
  ident_p: Parser,
  is_async is_async: Bool,
) -> Result(#(Parser, ast.Expression), ArrowAttempt) {
  let name = peek_value(ident_p)
  // async arrow params are [+Await]
  let check_p = case is_async {
    True -> Parser(..outer, ctx: GrammarContext(..outer.ctx, in_async: True))
    False -> outer
  }
  // => not consumed yet, so not-a-binding means not an arrow
  use Nil <- result.try(
    check_binding_identifier(check_p, name)
    |> result.replace_error(NotAnArrow),
  )
  let p2 = advance(ident_p)
  use <- bool.guard(has_line_break_before(p2), Error(NotAnArrow))
  let p3 = enter_arrow_context(advance(p2), is_async:, param_names: [name])
  // set after entering context, which zeroes param state
  let p3 =
    Parser(..p3, ctx: GrammarContext(..p3.ctx, param_bound_names: [name]))
  let params = [ast.IdentifierPattern(name:, span: span_of(ident_p))]
  finish_arrow(parse_arrow_body(p3, params), outer, is_async:, params:)
}

// any failure before => means not an arrow: backtrack
fn try_paren_arrow(
  outer: Parser,
  p_params: Parser,
  is_async is_async: Bool,
) -> Result(#(Parser, ast.Expression), ArrowAttempt) {
  // push arrow scope before params; failure discards it for free
  let p_ctx = enter_arrow_context(p_params, is_async:, param_names: [])
  // arrow params always reject duplicates
  let p_arrow =
    Parser(
      ..p_ctx,
      ctx: GrammarContext(
        ..p_ctx.ctx,
        in_arrow_params: True,
        in_formal_params: True,
        declaring: DeclaringParam,
        // §15.3: arrow params use the enclosing yield/await context
        in_generator: p_params.ctx.in_generator,
        in_async: p_params.ctx.in_async || is_async,
        in_static_block: p_params.ctx.in_static_block,
        in_class_field_init: p_params.ctx.in_class_field_init,
        // param defaults are always [+In]
        allow_in: True,
      ),
    )
  use #(p3, params) <- result.try(
    parse_formal_parameters(p_arrow) |> result.replace_error(NotAnArrow),
  )
  let p3 =
    Parser(
      ..p3,
      ctx: GrammarContext(
        ..p3.ctx,
        in_arrow_params: False,
        in_formal_params: False,
        declaring: NotDeclaring,
      ),
    )
  use p4 <- result.try(
    expect(p3, RightParen) |> result.replace_error(NotAnArrow),
  )
  use <- bool.guard(
    peek(p4) != Arrow || has_line_break_before(p4),
    Error(NotAnArrow),
  )
  // switch the borrowed flags back to the arrow body values
  let p5 =
    Parser(
      ..advance(p4),
      scopes: class_scopes.declare_param_shims(p4.scopes, params),
      ctx: GrammarContext(
        ..p4.ctx,
        in_generator: p_ctx.ctx.in_generator,
        in_async: p_ctx.ctx.in_async,
        in_static_block: p_ctx.ctx.in_static_block,
        in_class_field_init: p_ctx.ctx.in_class_field_init,
        allow_in: p_ctx.ctx.allow_in,
      ),
    )
  finish_arrow(parse_arrow_body(p5, params), outer, is_async:, params:)
}

fn finish_arrow(
  body_result: Result(#(Parser, ast.ArrowBody), ParseError),
  outer: Parser,
  is_async is_async: Bool,
  params params: List(ast.Pattern),
) -> Result(#(Parser, ast.Expression), ArrowAttempt) {
  use #(p_body, body) <- result.try(result.map_error(body_result, ArrowError))
  let body_end = p_body.prev_end
  let p_restored = restore_outer_context(p_body, outer)
  // §13.15.1: (x => x) = 1 is invalid
  Ok(#(
    Parser(
      ..p_restored,
      last_expr_assignable: False,
      last_expr_is_assignment: False,
    ),
    ast.ArrowFunctionExpression(
      params:,
      body:,
      is_async:,
      span: ast.Span(start: pos_of(outer), end: body_end),
    ),
  ))
}

fn parse_arrow_body(
  p: Parser,
  params: List(ast.Pattern),
) -> Result(#(Parser, ast.ArrowBody), ParseError) {
  case peek(p) {
    LeftBrace -> {
      // block body is [+In]; expression body inherits
      let p = Parser(..p, ctx: GrammarContext(..p.ctx, allow_in: True))
      use p <- result.try(apply_body_use_strict(p))
      use #(p2, body_stmt) <- result.try(parse_function_body(p, params))
      Ok(#(p2, ast.ArrowBodyBlock(body_stmt)))
    }
    _ -> {
      let start = pos_of(p)
      use #(p2, expr) <- result.try(parse_assignment_expression(p))
      // raise deferred cover errors before the context is dropped
      use Nil <- result.try(check_cover_grammar_errors(p2, start))
      // flip arrow children to source order for finalize
      let scopes =
        scope_builder.reorder_block_children(p2.scopes, p.scopes.current)
      Ok(#(Parser(..p2, scopes:), ast.ArrowBodyExpression(expr)))
    }
  }
}

fn parse_yield_expression(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  use <- bool.guard(
    p.ctx.in_formal_params && !p.ctx.in_catch_param,
    Error(YieldInFormalParameter(pos_of(p))),
  )
  let start = pos_of(p)
  let p2 = advance(p)
  let bare =
    ast.YieldExpression(
      argument: None,
      is_delegate: False,
      span: span_from(start, p2),
    )
  // no line terminator between yield and operand
  use <- bool.guard(has_line_break_before(p2), Ok(#(p2, bare)))
  case peek(p2) {
    Semicolon | RightParen | RightBracket | RightBrace | Eof | Comma | Colon ->
      Ok(#(p2, bare))
    Star ->
      parse_assignment_expression(advance(p2))
      |> yield_with_arg(start, is_delegate: True)
    // slash after yield starts a regex
    Slash | SlashEqual ->
      parse_regex_literal(p2) |> yield_with_arg(start, is_delegate: False)
    _ ->
      parse_assignment_expression(p2)
      |> yield_with_arg(start, is_delegate: False)
  }
}

fn yield_with_arg(
  parsed: Result(#(Parser, ast.Expression), ParseError),
  start: Int,
  is_delegate is_delegate: Bool,
) -> Result(#(Parser, ast.Expression), ParseError) {
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
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  use #(p2, test_expr) <- result.try(parse_binary_expression(p, 0))
  case peek(p2) {
    Question -> {
      let p3 = advance(p2)
      // §13.14: middle operand is always [+In]
      use #(p4, consequent) <- result.try(allowing_in(
        p3,
        parse_assignment_expression,
      ))
      use p5 <- result.try(expect(p4, Colon))
      use #(p6, alternate) <- result.try(parse_assignment_expression(p5))
      Ok(#(
        Parser(
          ..p6,
          last_expr_assignable: False,
          last_expr_is_assignment: False,
        ),
        ast.ConditionalExpression(
          condition: test_expr,
          consequent:,
          alternate:,
          span: ast.Span(test_expr.span.start, p6.prev_end),
        ),
      ))
    }
    _ -> Ok(#(p2, test_expr))
  }
}

fn parse_binary_expression(
  p: Parser,
  min_prec: Int,
) -> Result(#(Parser, ast.Expression), ParseError) {
  use #(p2, left) <- result.try(parse_unary_expression(p))
  parse_binary_rhs(p2, left, min_prec)
}

fn parse_binary_rhs(
  p: Parser,
  left: ast.Expression,
  min_prec: Int,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let tok = peek(p)
  // §13.10: bare #x is only valid left of in
  let bare_private = ast_util.is_bare_private_name(left)
  case binary_operator(tok, p.ctx.allow_in) {
    Some(BinaryOperator(precedence:, op:)) if precedence > min_prec -> {
      let op_pos = pos_of(p)
      use <- bool.guard(
        bare_private && tok != In,
        Error(PrivateNameNotInBrandCheck(op_pos)),
      )
      // §13.6: unary/await left of ** is an error
      use <- bool.guard(
        tok == StarStar && is_unary_operand(left),
        Error(UnaryBeforeExponentiation(op_pos)),
      )
      let p2 = advance(p)
      // ** is right-associative
      let next_min = case tok {
        StarStar -> precedence - 1
        _ -> precedence
      }
      use #(p3, right) <- result.try(parse_binary_expression(p2, next_min))
      let span = ast.Span(left.span.start, p3.prev_end)
      use expr <- result.try(binary_node(op, left, right, span, op_pos))
      parse_binary_rhs(
        Parser(..p3, last_expr_assignable: False),
        expr,
        min_prec,
      )
    }
    _ -> {
      use <- bool.guard(
        bare_private,
        Error(PrivateNameNotInBrandCheck(pos_of(p))),
      )
      Ok(#(p, left))
    }
  }
}

// separate so the §13.13.1 check cannot be dropped
fn binary_node(
  op: token.BinOrLogical,
  left: ast.Expression,
  right: ast.Expression,
  span: ast.Span,
  op_pos: Int,
) -> Result(ast.Expression, ParseError) {
  case op {
    Binary(op) -> Ok(ast.BinaryExpression(operator: op, left:, right:, span:))
    ShortCircuit(op) ->
      // §13.13.1: no unparenthesized ?? mixed with || &&
      case left {
        ast.LogicalExpression(operator: ast.NullishCoalescing, ..) ->
          Error(CoalesceMixedWithLogical(op_pos))
        _ -> Ok(ast.LogicalExpression(operator: op, left:, right:, span:))
      }
    Coalesce ->
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
  }
}

fn is_unary_operand(expr: ast.Expression) -> Bool {
  case expr {
    ast.UnaryExpression(..) | ast.AwaitExpression(..) -> True
    _ -> False
  }
}

fn parse_unary_expression(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let start = pos_of(p)
  let unary = fn(p2, op) {
    use #(p3, arg) <- result.try(parse_unary_expression(p2))
    use <- bool.guard(
      ast_util.is_bare_private_name(arg),
      Error(PrivateNameNotInBrandCheck(pos_of(p2))),
    )
    Ok(#(
      Parser(..p3, last_expr_assignable: False, last_expr_is_assignment: False),
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
      // §13.5.1.1 delete early errors, through parens
      use <- bool.guard(
        p.ctx.strict && ast_util.is_bare_identifier(operand),
        Error(DeleteUnqualifiedStrictMode(start)),
      )
      use <- bool.guard(
        ast_util.is_private_name_access(operand),
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
      finish_update_expr(
        p3,
        arg,
        op,
        prefix: True,
        span_start: start,
        at: start,
      )
    }
    Await ->
      case p.ctx.in_async || p.mode == Module {
        True -> parse_await_expression(p)
        // await is an identifier outside async and modules
        False -> parse_postfix_expression(p)
      }
    _ -> parse_postfix_expression(p)
  }
}

fn parse_await_expression(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let start = pos_of(p)
  // §15.7.1: no await in a static block
  use <- bool.guard(p.ctx.in_static_block, Error(AwaitInStaticBlock(start)))
  // §15.8.1: no await in formal parameters
  use <- bool.guard(
    p.ctx.in_formal_params && !p.ctx.in_catch_param,
    Error(AwaitInFormalParameter(start)),
  )
  use #(p2, arg) <- result.map(parse_unary_expression(advance(p)))
  #(
    Parser(..p2, last_expr_assignable: False, last_expr_is_assignment: False),
    ast.AwaitExpression(argument: arg, span: span_from(start, p2)),
  )
}

fn delete_operand(expr: ast.Expression) -> ast.Expression {
  case expr {
    ast.UnaryExpression(operator: ast.Delete, argument:, ..) ->
      ast_util.unwrap_parens(argument)
    _ -> expr
  }
}

// §15.7.1 AllPrivateIdentifiersValid ref recording
fn note_private_ref(p: Parser, name: String) -> Parser {
  case name {
    "#" <> _ -> {
      let ref =
        PrivateNameRef(name:, class_depth: p.class_body_depth, pos: pos_of(p))
      Parser(..p, private_refs: [ref, ..p.private_refs])
    }
    _ -> p
  }
}

// super.#x is always a syntax error (§13.3)
fn check_super_private(
  p: Parser,
  object: ast.Expression,
  name: String,
) -> Result(Nil, ParseError) {
  case object, name {
    ast.SuperExpression(..), "#" <> _ -> Error(SuperPrivateName(pos_of(p)))
    _, _ -> Ok(Nil)
  }
}

// { #x: 1 } is a syntax error
fn reject_private_property_key(
  p: Parser,
  key: ast.PropertyName,
) -> Result(Nil, ParseError) {
  case key {
    ast.PrivateName(..) -> Error(PrivateNameAsPropertyKey(pos_of(p)))
    ast.IdentifierName(..)
    | ast.StringName(..)
    | ast.NumberName(..)
    | ast.BigIntName(..)
    | ast.ComputedName(..) -> Ok(Nil)
  }
}

// end of class body step of AllPrivateIdentifiersValid
fn resolve_private_refs(
  p: Parser,
  outer_depth: Int,
  declared: Dict(String, DeclaredPrivateName),
) -> Result(Parser, ParseError) {
  let my_depth = outer_depth + 1
  // unresolved refs now belong to the enclosing class
  let remaining =
    list.filter_map(p.private_refs, fn(ref) {
      case ref.class_depth >= my_depth, dict.has_key(declared, ref.name) {
        True, True -> Error(Nil)
        True, False -> Ok(PrivateNameRef(..ref, class_depth: outer_depth))
        False, _ -> Ok(ref)
      }
    })
  case outer_depth {
    // only direct eval's private environment can still legitimize these
    0 -> {
      use Nil <- result.map(check_unresolved_private_refs(
        Parser(..p, private_refs: remaining),
      ))
      Parser(..p, private_refs: [])
    }
    _ -> Ok(Parser(..p, private_refs: remaining))
  }
}

fn check_unresolved_private_refs(p: Parser) -> Result(Nil, ParseError) {
  let unresolved = case p.outer_private_names {
    [] -> p.private_refs
    outer ->
      list.filter(p.private_refs, fn(ref) { !list.contains(outer, ref.name) })
  }
  case unresolved {
    [] -> Ok(Nil)
    [ref, ..] -> Error(UndeclaredPrivateName(ref.pos, ref.name))
  }
}

fn parse_postfix_expression(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  use #(p2, expr) <- result.try(parse_left_hand_side_expression(p))
  case peek(p2) {
    PlusPlus | MinusMinus -> {
      use <- bool.guard(has_line_break_before(p2), Ok(#(p2, expr)))
      let op = case peek(p2) {
        PlusPlus -> ast.Increment
        _ -> ast.Decrement
      }
      finish_update_expr(
        advance(p2),
        expr,
        op,
        prefix: False,
        span_start: expr.span.start,
        at: pos_of(p2),
      )
    }
    _ -> Ok(#(p2, expr))
  }
}

// advance preserves last_expr_*, so postfix callers advance first
fn finish_update_expr(
  p: Parser,
  arg: ast.Expression,
  op: ast.UpdateOp,
  prefix prefix: Bool,
  span_start span_start: Int,
  at err_pos: Int,
) -> Result(#(Parser, ast.Expression), ParseError) {
  case p.last_expr_assignable || is_web_compat_call_target(p, arg), prefix {
    False, True -> Error(InvalidLhsPrefixOp(err_pos))
    False, False -> Error(InvalidPostfixLhs(err_pos))
    True, _ ->
      case p.ctx.strict, p.last_expr_name {
        True, Some("eval" as n) | True, Some("arguments" as n) ->
          Error(StrictModeModification(err_pos, n))
        _, _ ->
          Ok(#(
            Parser(
              ..p,
              last_expr_assignable: False,
              scopes: class_scopes.mark_assign_targets(p.scopes, arg),
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
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  case peek(p) {
    New -> parse_new_expression(p)
    _ -> parse_call_expression(p)
  }
}

fn parse_new_expression(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let p2 = advance(p)
  case peek(p2) {
    Dot -> {
      use #(p3, meta) <- result.try(parse_new_target(advance(p2), pos_of(p)))
      parse_call_chain(p3, meta)
    }
    New -> {
      use #(p3, inner) <- result.try(parse_new_expression(p2))
      parse_call_chain(p3, inner)
    }
    _ -> {
      let start = pos_of(p)
      use #(p3, callee_base) <- result.try(parse_primary_expression(p2))
      let #(p4, callee) = parse_member_chain(p3, callee_base)
      case peek(p4) {
        // §13.3: tagged template binds tighter than new
        TemplateLiteral | TemplateHead -> {
          use #(p5, tagged) <- result.try(parse_member_templates(p4, callee))
          finish_new(p5, start, tagged)
        }
        _ -> finish_new(p4, start, callee)
      }
    }
  }
}

// p is past new . and start is the position of new
fn parse_new_target(
  p: Parser,
  start: Int,
) -> Result(#(Parser, ast.Expression), ParseError) {
  case peek(p), peek_value(p) {
    Identifier, "target" -> {
      // no unicode escapes in new.target
      use <- bool.guard(
        peek_had_escape(p),
        Error(UnicodeEscapeInMetaProperty(pos_of(p))),
      )
      use <- bool.guard(
        !p.ctx.allow_new_target,
        Error(NewTargetOutsideFunction(start)),
      )
      let p2 = advance(p)
      let scopes = scope_builder.lexical_ref(p2.scopes, lexical.NewTargetRef)
      let meta =
        ast.MetaProperty(kind: ast.NewTarget, span: span_from(start, p2))
      Ok(#(Parser(..p2, scopes:), meta))
    }
    Identifier, other -> Error(ExpectedNewTarget(pos_of(p), Some(other)))
    _, _ -> Error(ExpectedNewTarget(pos_of(p), None))
  }
}

fn finish_new(
  p: Parser,
  start: Int,
  callee: ast.Expression,
) -> Result(#(Parser, ast.Expression), ParseError) {
  case peek(p) {
    LeftParen -> {
      use #(p2, args) <- result.try(parse_arguments(p))
      let new_expr =
        ast.NewExpression(callee:, arguments: args, span: span_from(start, p2))
      parse_call_chain(Parser(..p2, last_expr_assignable: False), new_expr)
    }
    _ ->
      Ok(#(
        Parser(..p, last_expr_assignable: False),
        ast.NewExpression(callee:, arguments: [], span: span_from(start, p)),
      ))
  }
}

fn parse_call_expression(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let parsed = case peek(p) {
    Super -> parse_super_expression(p)
    Import -> parse_import_call_or_meta(p)
    _ -> parse_primary_expression(p)
  }
  use #(p2, expr) <- result.try(parsed)
  // [{a = 0}.x] = [] is a syntax error
  let literal_cover_error =
    !p.ctx.has_cover_initializer
    && p2.ctx.has_cover_initializer
    && at_suffix_start(p2)
    && case expr {
      ast.ObjectExpression(..) | ast.ArrayExpression(..) -> True
      _ -> False
    }
  use <- bool.guard(
    literal_cover_error,
    Error(ShorthandDefaultOutsideDestructuring(pos_of(p))),
  )
  parse_call_chain(p2, expr)
}

fn parse_super_expression(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let super_span = span_of(p)
  let p2 = advance(p)
  case peek(p2) {
    LeftParen -> {
      use <- bool.guard(
        !p.ctx.allow_super_call,
        Error(SuperCallNotInDerivedConstructor(pos_of(p))),
      )
      use #(p3, args) <- result.map(parse_arguments(p2))
      // class_fields_init too, so arrows in derived ctors capture it
      let scopes =
        class_scopes.super_call_refs(p3.scopes)
        |> scope_builder.ref(ast_util.class_fields_init)
      let call =
        ast.CallExpression(
          callee: ast.SuperExpression(span: super_span),
          arguments: args,
          span: span_from(super_span.start, p3),
        )
      #(Parser(..p3, scopes:), call)
    }
    Dot | LeftBracket ->
      super_property_reference(
        p2,
        super_span,
        SuperPropertyNotInMethod(pos_of(p)),
      )
    _ -> Error(UnexpectedSuper(pos_of(p)))
  }
}

// p is past super, at . or [
fn super_property_reference(
  p: Parser,
  span: ast.Span,
  not_allowed: ParseError,
) -> Result(#(Parser, ast.Expression), ParseError) {
  use <- bool.guard(!p.ctx.allow_super_property, Error(not_allowed))
  let scopes =
    p.scopes
    |> scope_builder.lexical_ref(lexical.HomeObjectRef)
    |> scope_builder.lexical_ref(lexical.ThisRef)
  Ok(#(Parser(..p, scopes:), ast.SuperExpression(span:)))
}

fn parse_import_call_or_meta(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let import_start = pos_of(p)
  let p2 = advance(p)
  case peek(p2) {
    LeftParen -> parse_import_call(p2, import_start)
    Dot -> {
      let p3 = advance(p2)
      // meta/source/defer must be unescaped (§5.1.5)
      case peek(p3), peek_value(p3), peek_had_escape(p3) {
        Identifier, "meta", False -> {
          use <- bool.guard(
            p.mode == Script,
            Error(ImportMetaOutsideModule(import_start)),
          )
          let p4 = advance(p3)
          let meta =
            ast.MetaProperty(
              kind: ast.ImportMeta,
              span: span_from(import_start, p4),
            )
          Ok(#(p4, meta))
        }
        // bare import.source is a syntax error
        Identifier, "source", False ->
          case peek_at(p3, 1) {
            LeftParen ->
              parse_phase_import_call(p3, import_start, ast.PhaseSource)
            _ -> Error(ExpectedImportMeta(pos_of(p3), Some("source")))
          }
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

// §13.3.10 import(x) / import(x, options); p is at (
fn parse_import_call(
  p: Parser,
  import_start: Int,
) -> Result(#(Parser, ast.Expression), ParseError) {
  // import() arguments are [+In]
  use p2 <- allowing_in(advance(p))
  use #(p3, source_expr) <- result.try(parse_assignment_expression(p2))
  use #(p4, options) <- result.try(case peek(p3), peek_at(p3, 1) {
    Comma, RightParen -> Ok(#(advance(p3), None))
    Comma, _ -> {
      use #(p4, attrs) <- result.map(parse_assignment_expression(advance(p3)))
      // import(x, opts,) is allowed
      #(skip_trailing_comma(p4), Some(attrs))
    }
    _, _ -> Ok(#(p3, None))
  })
  use p5 <- result.map(expect(p4, RightParen))
  #(
    p5,
    ast.ImportExpression(
      source: source_expr,
      options:,
      phase: ast.PhaseEvaluation,
      span: span_from(import_start, p5),
    ),
  )
}

// a , directly before ) is consumed
fn skip_trailing_comma(p: Parser) -> Parser {
  case peek(p), peek_at(p, 1) {
    Comma, RightParen -> advance(p)
    _, _ -> p
  }
}

fn at_suffix_start(p: Parser) -> Bool {
  case peek(p) {
    Dot
    | LeftBracket
    | LeftParen
    | QuestionDot
    | TemplateLiteral
    | TemplateHead -> True
    _ -> False
  }
}

// §13.3.10 import.source(x) / import.defer(x)
fn parse_phase_import_call(
  p: Parser,
  import_start: Int,
  phase: ast.ImportPhase,
) -> Result(#(Parser, ast.Expression), ParseError) {
  use p2 <- allowing_in(advance(advance(p)))
  use #(p3, source_expr) <- result.try(parse_assignment_expression(p2))
  let p4 = skip_trailing_comma(p3)
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
  p: Parser,
  callee: ast.Expression,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let start = callee.span.start
  case peek(p) {
    LeftParen -> {
      use #(p2, args) <- result.try(parse_arguments(p))
      // §19.2.1 direct eval poisons the scope
      let p2 = case ast_util.unwrap_parens(callee) {
        ast.Identifier(name: "eval", ..) ->
          Parser(..p2, scopes: scope_builder.mark_eval(p2.scopes))
        _ -> p2
      }
      let expr =
        ast.CallExpression(callee:, arguments: args, span: span_from(start, p2))
      parse_call_chain(Parser(..p2, last_expr_assignable: False), expr)
    }
    Dot | LeftBracket -> {
      use #(p2, expr) <- result.try(parse_member_suffix(p, callee, start))
      parse_call_chain(p2, expr)
    }
    QuestionDot ->
      // optional chain is never assignable
      case peek_at(p, 1) {
        LeftParen -> {
          let p2 = advance(p)
          use #(p3, args) <- result.try(parse_arguments(p2))
          let expr =
            ast.OptionalCallExpression(
              callee:,
              arguments: args,
              span: span_from(start, p3),
            )
          parse_call_chain(Parser(..p3, last_expr_assignable: False), expr)
        }
        _ -> {
          use #(p2, expr) <- result.try(parse_member_suffix(p, callee, start))
          parse_call_chain(p2, expr)
        }
      }
    TemplateLiteral | TemplateHead -> {
      // §13.3.1.1: no tagged template in an optional chain
      use <- bool.guard(
        ast_util.chain_has_optional(callee),
        Error(TemplateInOptionalChain(pos_of(p))),
      )
      use #(p2, expr) <- result.try(parse_tagged_template(p, callee))
      parse_call_chain(p2, expr)
    }
    _ -> Ok(#(p, callee))
  }
}

// p is at . [ or ?.
fn parse_member_suffix(
  p: Parser,
  object: ast.Expression,
  start: Int,
) -> Result(#(Parser, ast.Expression), ParseError) {
  case peek(p) {
    Dot -> {
      let p2 = advance(p)
      case is_identifier_or_keyword(peek(p2)) {
        True -> {
          let prop_name = peek_value(p2)
          use Nil <- result.try(check_super_private(p2, object, prop_name))
          Ok(finish_dot_member(p2, object, start, prop_name, optional: False))
        }
        False ->
          Error(error_at_current(p2, ExpectedIdentifierAfterDot(pos_of(p2))))
      }
    }
    LeftBracket -> parse_bracket_member(p, object, start, optional: False)
    QuestionDot -> {
      let p2 = advance(p)
      case peek(p2) {
        LeftBracket -> parse_bracket_member(p2, object, start, optional: True)
        _ ->
          case is_identifier_or_keyword(peek(p2)) {
            True -> {
              let prop_name = peek_value(p2)
              Ok(finish_dot_member(p2, object, start, prop_name, optional: True))
            }
            False ->
              Error(error_at_current(p2, ExpectedAfterOptionalChain(pos_of(p2))))
          }
      }
    }
    _ -> panic as "parser: parse_member_suffix called off a member suffix"
  }
}

fn parse_bracket_member(
  p: Parser,
  object: ast.Expression,
  start: Int,
  optional optional: Bool,
) -> Result(#(Parser, ast.Expression), ParseError) {
  use p2 <- allowing_in(advance(p))
  use #(p3, expression) <- result.try(parse_expression(p2))
  use p4 <- result.map(expect(p3, RightBracket))
  let span = span_from(start, p4)
  let property = ast.Bracket(expression:)
  case optional {
    False -> #(
      Parser(..p4, last_expr_assignable: True),
      ast.MemberExpression(object:, property:, span:),
    )
    True -> #(
      Parser(..p4, last_expr_assignable: False),
      ast.OptionalMemberExpression(object:, property:, span:),
    )
  }
}

fn finish_dot_member(
  p: Parser,
  object: ast.Expression,
  start: Int,
  prop_name: String,
  optional optional: Bool,
) -> #(Parser, ast.Expression) {
  let p = note_private_ref(p, prop_name)
  // obj.#x is a ref to the class-scope #x const
  let p = case prop_name {
    "#" <> _ -> Parser(..p, scopes: scope_builder.ref(p.scopes, prop_name))
    _ -> p
  }
  let property = ast.Dot(name: prop_name, span: span_of(p))
  let p2 = advance(p)
  let span = span_from(start, p2)
  case optional {
    False -> #(
      Parser(..p2, last_expr_assignable: True),
      ast.MemberExpression(object:, property:, span:),
    )
    True -> #(
      Parser(..p2, last_expr_assignable: False),
      ast.OptionalMemberExpression(object:, property:, span:),
    )
  }
}

fn parse_tagged_template(
  p: Parser,
  tag: ast.Expression,
) -> Result(#(Parser, ast.Expression), ParseError) {
  use #(p2, raw_parts) <- result.map(parse_template_spans(p))
  // §12.9.6: invalid escape is legal in tagged templates
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
  #(Parser(..p2, last_expr_assignable: False), expr)
}

// §13.2.8: substitutions are [+In]; spans are rescanned from source
fn parse_template_spans(
  p: Parser,
) -> Result(#(Parser, ast.TemplateParts(String)), ParseError) {
  case peek(p) {
    TemplateLiteral ->
      Ok(#(
        advance(p),
        ast.TemplateParts(head: template_span_raw(p, 1), tail: []),
      ))
    // restore last-expr flags and [In] after the whole template
    _ -> {
      let saved_assignable = p.last_expr_assignable
      let saved_is_assignment = p.last_expr_is_assignment
      let head = template_span_raw(p, 2)
      use #(p, rev_tail) <- result.map({
        use p <- allowing_in(advance(p))
        parse_template_substitutions(p, [])
      })
      #(
        Parser(
          ..p,
          last_expr_assignable: saved_assignable,
          last_expr_is_assignment: saved_is_assignment,
        ),
        ast.TemplateParts(head:, tail: list.reverse(rev_tail)),
      )
    }
  }
}

fn parse_template_substitutions(
  p: Parser,
  rev_tail: List(ast.TemplateSpan(String)),
) -> Result(#(Parser, List(ast.TemplateSpan(String))), ParseError) {
  use #(p, expr) <- result.try(parse_expression(
    Parser(..p, last_expr_assignable: False, last_expr_is_assignment: False),
  ))
  case peek(p) {
    RightBrace -> {
      let p = template_continuation(p)
      case peek(p) {
        TemplateHead ->
          parse_template_substitutions(advance(p), [
            ast.TemplateSpan(expr, template_span_raw(p, 2)),
            ..rev_tail
          ])
        TemplateLiteral ->
          Ok(
            #(advance(p), [
              ast.TemplateSpan(expr, template_span_raw(p, 1)),
              ..rev_tail
            ]),
          )
        // unterminated template
        _ -> Error(UnterminatedTemplateSubstitution(pos_of(p)))
      }
    }
    other ->
      Error(error_at_current(p, ExpectedToken(pos_of(p), RightBrace, other)))
  }
}

// window past } is garbage, rescan like a regex
fn template_continuation(p: Parser) -> Parser {
  let #(token, scan) =
    lexer.scan_template_continuation(
      p.bytes,
      pos_of(p),
      line_of(p),
      p.scan.source_kind,
    )
  Parser(..p, tokens: [token], scan:)
}

// raw quasi text, line terminators normalized (§12.9.6 trv)
fn template_span_raw(p: Parser, trailing: Int) -> String {
  bytes.unsafe_slice(p.bytes, pos_of(p) + 1, peek_raw_len(p) - 1 - trailing)
  |> string.replace("\r\n", "\n")
  |> string.replace("\r", "\n")
}

fn parse_member_templates(
  p: Parser,
  callee: ast.Expression,
) -> Result(#(Parser, ast.Expression), ParseError) {
  case peek(p) {
    TemplateLiteral | TemplateHead -> {
      use #(p2, expr) <- result.try(parse_tagged_template(p, callee))
      let #(p3, expr) = parse_member_chain(p2, expr)
      parse_member_templates(p3, expr)
    }
    _ -> Ok(#(p, callee))
  }
}

fn parse_member_chain(
  p: Parser,
  object: ast.Expression,
) -> #(Parser, ast.Expression) {
  // no ?. here: new a?.b must fail downstream (§13.3)
  case peek(p) {
    Dot | LeftBracket ->
      case parse_member_suffix(p, object, object.span.start) {
        Ok(#(p2, expr)) -> parse_member_chain(p2, expr)
        // the caller reparses this suffix and reports it
        Error(_reported_by_caller) -> #(p, object)
      }
    _ -> #(p, object)
  }
}

fn parse_arguments(
  p: Parser,
) -> Result(#(Parser, List(ast.Expression)), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  parse_comma_list(
    p2,
    [],
    RightParen,
    parse_argument,
    ExpectedCommaOrCloseParen,
  )
}

fn parse_argument(p: Parser) -> Result(#(Parser, ast.Expression), ParseError) {
  // arguments are [+In]
  use p <- allowing_in(p)
  case peek(p) {
    DotDotDot -> {
      let start = pos_of(p)
      use #(p2, arg_expr) <- result.map(parse_assignment_expression(advance(p)))
      #(p2, ast.SpreadElement(argument: arg_expr, span: span_from(start, p2)))
    }
    _ -> parse_assignment_expression(p)
  }
}

fn parse_primary_expression(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  case peek(p) {
    Identifier -> {
      let val = peek_value(p)
      // §13.1.1 identifier reference early errors
      use Nil <- result.try(check_identifier_reference(p, val))
      // bare #x only reaches here as #x in obj
      case val {
        "#" <> _ ->
          case peek_at(p, 1) {
            In -> identifier_reference(note_private_ref(p, val))
            _ -> Error(PrivateNameNotInBrandCheck(pos_of(p)))
          }
        _ -> identifier_reference(p)
      }
    }
    _ -> parse_primary_non_identifier(Parser(..p, last_expr_name: None))
  }
}

// also used for contextual keywords, never eval or arguments
fn identifier_reference(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let name = peek_value(p)
  Ok(#(
    advance(
      Parser(
        ..p,
        scopes: scope_builder.ref(p.scopes, name),
        last_expr_assignable: True,
        last_expr_name: Some(name),
      ),
    ),
    ast.Identifier(name:, span: span_of(p)),
  ))
}

fn parse_primary_non_identifier(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  case peek(p) {
    // hard lexer error token: report its message
    Illegal | LexFailure(_) -> Error(illegal_token_error(p))
    Number -> {
      use Nil <- result.try(check_legacy_octal_literal(p))
      use lit <- result.map(numeric_literal(p))
      #(Parser(..advance(p), last_expr_assignable: False), lit)
    }
    StringLiteral -> {
      use value <- result.map(string_literal_value(p))
      #(
        Parser(..advance(p), last_expr_assignable: False),
        ast.StringLiteral(value:, span: span_of(p)),
      )
    }
    TrueLiteral ->
      accept_literal(p, ast.BooleanLiteral(value: True, span: span_of(p)))
    FalseLiteral ->
      accept_literal(p, ast.BooleanLiteral(value: False, span: span_of(p)))
    Null -> accept_literal(p, ast.NullLiteral(span: span_of(p)))
    Undefined -> accept_literal(p, ast.UndefinedExpression(span: span_of(p)))
    TemplateLiteral | TemplateHead -> {
      let start = pos_of(p)
      use #(p, raw_parts) <- result.try(parse_template_spans(p))
      // §12.9.6: invalid escape only legal when tagged
      use parts <- result.map(
        ast.try_map_template_quasis(raw_parts, fn(q) {
          case cook_template_string(q) {
            Ok(s) -> Ok(s)
            Error(Nil) -> Error(InvalidTemplateEscape(start))
          }
        }),
      )
      #(
        Parser(..p, last_expr_assignable: False),
        ast.TemplateLiteral(parts:, span: ast.Span(start:, end: p.prev_end)),
      )
    }
    This ->
      accept_literal(
        Parser(
          ..p,
          scopes: scope_builder.lexical_ref(p.scopes, lexical.ThisRef),
        ),
        ast.ThisExpression(span: span_of(p)),
      )
    Super ->
      case peek_at(p, 1) {
        Dot | LeftBracket ->
          super_property_reference(
            advance(p),
            span_of(p),
            UnexpectedSuper(pos_of(p)),
          )
        _ -> Error(UnexpectedSuper(pos_of(p)))
      }
    LeftParen -> {
      let start = pos_of(p)
      let p2 = advance(p)
      // () without => is an error
      use <- bool.guard(
        peek(p2) == RightParen,
        Error(UnexpectedCloseParen(pos_of(p))),
      )
      // parenthesized expression is [+In]
      use p2 <- allowing_in(p2)
      use #(p3, expr) <- result.try(parse_expression(p2))
      use p4 <- result.map(expect(p3, RightParen))
      // keep parens for IsIdentifierRef (§13.15.2)
      let expr =
        ast.ParenthesizedExpression(
          expression: expr,
          span: span_from(start, p4),
        )
      #(p4, expr)
    }
    LeftBracket -> parse_array_literal(p) |> set_not_assignable
    LeftBrace -> parse_object_literal(p) |> set_not_assignable
    Function -> parse_function_expression(p, is_async: False)
    Class -> parse_class_expression(p)
    Async ->
      case async_function_start(p) {
        True -> parse_function_expression(p, is_async: True)
        False -> identifier_reference(p)
      }
    // / and /= here start a regex: relex it
    Slash | SlashEqual -> parse_regex_literal(p)
    New -> parse_new_expression(p)
    Yield -> {
      use <- bool.guard(p.ctx.strict, Error(YieldReservedStrictMode(pos_of(p))))
      use <- bool.guard(p.ctx.in_generator, Error(YieldInGenerator(pos_of(p))))
      identifier_reference(p)
    }
    Await -> {
      use <- bool.guard(p.mode == Module, Error(AwaitInModule(pos_of(p))))
      use <- bool.guard(p.ctx.in_async, Error(AwaitInAsyncFunction(pos_of(p))))
      identifier_reference(p)
    }
    Let -> {
      use <- bool.guard(p.ctx.strict, Error(LetIdentifierStrictMode(pos_of(p))))
      identifier_reference(p)
    }
    Static -> {
      use <- bool.guard(
        p.ctx.strict,
        Error(StaticReservedStrictMode(pos_of(p))),
      )
      identifier_reference(p)
    }
    other ->
      case is_contextual_keyword(other) {
        True -> identifier_reference(p)
        False -> Error(UnexpectedToken(pos_of(p), other))
      }
  }
}

fn parse_array_literal(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let start = pos_of(p)
  let p2 = advance(p)
  // elements are [+In]
  use p2 <- allowing_in(p2)
  // pattern flags are per literal: reset
  let p2 = Parser(..p2, literal_invalid_as_pattern: False)
  use #(p3, elems) <- result.map(parse_array_elements(p2, []))
  #(
    p3,
    ast.ArrayExpression(
      elements: list.reverse(elems),
      span: span_from(start, p3),
    ),
  )
}

// does the element just parsed break the enclosing pattern
fn element_breaks_pattern(
  p: Parser,
  start_token: TokenKind,
  allow_default allow_default: Bool,
) -> Bool {
  let valid_target =
    p.last_expr_assignable || { allow_default && p.last_expr_is_assignment }
  case valid_target, start_token {
    True, _ -> False
    False, LeftBrace | False, LeftBracket -> p.literal_invalid_as_pattern
    False, _ -> True
  }
}

// one element, folding its verdict into the literal's flag
fn parse_cover_element(
  p: Parser,
  invalid_before invalid_before: Bool,
  allow_default allow_default: Bool,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let start_token = peek(p)
  use #(p2, expr) <- result.map(parse_assignment_expression(p))
  let invalid =
    invalid_before || element_breaks_pattern(p2, start_token, allow_default:)
  #(Parser(..p2, literal_invalid_as_pattern: invalid), expr)
}

fn parse_array_elements(
  p: Parser,
  acc: List(Option(ast.Expression)),
) -> Result(#(Parser, List(Option(ast.Expression))), ParseError) {
  let invalid_before = p.literal_invalid_as_pattern
  case peek(p) {
    RightBracket -> Ok(#(advance(p), acc))
    Comma -> parse_array_elements(advance(p), [None, ..acc])
    DotDotDot -> {
      let spread_pos = pos_of(p)
      // rest cannot take a default
      use #(p3, expr) <- result.try(parse_cover_element(
        advance(p),
        invalid_before:,
        allow_default: False,
      ))
      let elem =
        Some(ast.SpreadElement(argument: expr, span: span_from(spread_pos, p3)))
      case peek(p3) {
        Comma -> {
          // spread not last: fine as expression, invalid as pattern
          let p4 = Parser(..advance(p3), literal_invalid_as_pattern: True)
          parse_array_elements(p4, [elem, ..acc])
        }
        RightBracket -> Ok(#(advance(p3), [elem, ..acc]))
        _ -> Error(ExpectedCommaOrBracketInExpr(pos_of(p3)))
      }
    }
    _ -> {
      use #(p2, expr) <- result.try(parse_cover_element(
        p,
        invalid_before:,
        allow_default: True,
      ))
      case peek(p2) {
        Comma -> parse_array_elements(advance(p2), [Some(expr), ..acc])
        RightBracket -> Ok(#(advance(p2), [Some(expr), ..acc]))
        _ -> Error(ExpectedCommaOrBracketInExpr(pos_of(p2)))
      }
    }
  }
}

fn parse_object_literal(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let start = pos_of(p)
  let p2 = advance(p)
  // values are [+In]
  use p2 <- allowing_in(p2)
  // pattern flags are per literal: reset
  let p2 = Parser(..p2, literal_invalid_as_pattern: False)
  use #(p3, props) <- result.map(
    parse_object_properties(p2, has_proto: False, acc: []),
  )
  #(
    p3,
    ast.ObjectExpression(
      properties: list.reverse(props),
      span: span_from(start, p3),
    ),
  )
}

fn parse_object_properties(
  p: Parser,
  has_proto has_proto: Bool,
  acc acc: List(ast.Property),
) -> Result(#(Parser, List(ast.Property)), ParseError) {
  case peek(p) {
    RightBrace -> Ok(#(advance(p), acc))
    DotDotDot -> {
      // §13.15.5: object rest must be a simple target
      let invalid_before = p.literal_invalid_as_pattern
      let p2 = advance(p)
      use #(p3, expr) <- result.try(parse_assignment_expression(p2))
      let p3 =
        Parser(
          ..p3,
          literal_invalid_as_pattern: invalid_before || !p3.last_expr_assignable,
        )
      let prop = ast.SpreadProperty(argument: expr)
      case peek(p3) {
        // anything after spread invalidates the pattern
        Comma ->
          parse_object_properties(
            advance(Parser(..p3, literal_invalid_as_pattern: True)),
            has_proto:,
            acc: [prop, ..acc],
          )
        RightBrace -> Ok(#(advance(p3), [prop, ..acc]))
        _ -> Error(ExpectedCommaOrBraceInObject(pos_of(p3)))
      }
    }
    _ -> {
      use #(p2, prop) <- result.try(parse_object_property(p))
      // §13.2.5.1 duplicate __proto__, deferred for patterns
      let is_proto = case prop {
        ast.InitProperty(key:, shorthand: False, ..) ->
          ast.static_name(key) == Some("__proto__")
        _ -> False
      }
      let p2 = case is_proto && has_proto, p2.ctx.dup_proto_pos {
        True, None ->
          Parser(
            ..p2,
            ctx: GrammarContext(..p2.ctx, dup_proto_pos: Some(pos_of(p))),
          )
        _, _ -> p2
      }
      let has_proto = has_proto || is_proto
      case peek(p2) {
        Comma ->
          parse_object_properties(advance(p2), has_proto:, acc: [prop, ..acc])
        RightBrace -> Ok(#(advance(p2), [prop, ..acc]))
        _ -> Error(ExpectedCommaOrBraceInObject(pos_of(p2)))
      }
    }
  }
}

// star_ends_accessor: in classes get * is a field named get
fn parse_method_prefix(
  p: Parser,
  ends_name: fn(TokenKind) -> Bool,
  star_ends_accessor star_ends_accessor: Bool,
) -> #(Parser, MethodPrefix) {
  let is_async =
    peek(p) == Async
    && !ends_name(peek_at(p, 1))
    && token_line_at(p, 1) == token_line_at(p, 0)
  let p = case is_async {
    True -> advance(p)
    False -> p
  }
  // escaped get/set is never the keyword (§12.7.2)
  let accessor = case peek(p), peek_value(p), peek_had_escape(p) {
    Identifier, "get", False -> GetPrefix
    Identifier, "set", False -> SetPrefix
    _, _, _ -> NoAccessor
  }
  let accessor = case accessor {
    NoAccessor -> NoAccessor
    GetPrefix | SetPrefix -> {
      let next = peek_at(p, 1)
      case ends_name(next) || { star_ends_accessor && next == Star } {
        True -> NoAccessor
        False -> accessor
      }
    }
  }
  let p = case accessor {
    NoAccessor -> p
    GetPrefix | SetPrefix -> advance(p)
  }
  let is_generator = peek(p) == Star
  let p = case is_generator {
    True -> advance(p)
    False -> p
  }
  #(p, MethodPrefix(is_async:, accessor:, is_generator:))
}

// in an object literal a keyword is a name when followed by ( , } :
fn ends_object_property_name(kind: TokenKind) -> Bool {
  case kind {
    LeftParen | Comma | RightBrace | Colon -> True
    _ -> False
  }
}

fn parse_object_property(
  p: Parser,
) -> Result(#(Parser, ast.Property), ParseError) {
  let #(p2, prefix) =
    parse_method_prefix(p, ends_object_property_name, star_ends_accessor: False)
  let key_token = peek(p2)
  let shorthand_name = simple_binding_name(p2)
  use #(p3, key) <- result.try(parse_property_name(p2))
  use Nil <- result.try(reject_private_property_key(p2, key))
  // *name must be a method
  use <- bool.lazy_guard(prefix.is_generator && peek(p3) != LeftParen, fn() {
    Error(UnexpectedToken(pos_of(p3), peek(p3)))
  })
  case peek(p3) {
    LeftParen -> {
      let p3 = Parser(..p3, literal_invalid_as_pattern: True)
      use #(p4, params, body) <- result.map(parse_method_params_body(
        p3,
        p,
        prefix,
        is_constructor: False,
        has_extends: False,
      ))
      let value =
        ast.FunctionLiteral(
          name: None,
          params:,
          body:,
          is_generator: prefix.is_generator,
          is_async: prefix.is_async,
        )
      let prop = case prefix.accessor {
        GetPrefix -> ast.AccessorProperty(key:, value:, kind: ast.GetAccessor)
        SetPrefix -> ast.AccessorProperty(key:, value:, kind: ast.SetAccessor)
        NoAccessor -> ast.MethodProperty(key:, value:)
      }
      #(p4, prop)
    }
    Colon -> {
      // simple target shadows inner invalid-pattern flags
      use #(p4, value) <- result.map(parse_cover_element(
        advance(p3),
        invalid_before: p3.literal_invalid_as_pattern,
        allow_default: True,
      ))
      #(p4, ast.InitProperty(key:, value:, shorthand: False))
    }
    next -> {
      let modified = prefix.is_async || prefix.accessor != NoAccessor
      case shorthand_name, modified {
        Some(name), False ->
          parse_shorthand_property(p3, name, key, has_default: next == Equal)
        _, _ -> Error(UnexpectedToken(pos_of(p3), key_token))
      }
    }
  }
}

// { a } or { a = 1 }: p is past the name
fn parse_shorthand_property(
  p: Parser,
  name: String,
  key: ast.PropertyName,
  has_default has_default: Bool,
) -> Result(#(Parser, ast.Property), ParseError) {
  // shorthand is an identifier reference (§13.1.1)
  use Nil <- result.try(check_identifier_reference(p, name))
  let p = Parser(..p, scopes: scope_builder.ref(p.scopes, name))
  let key_span = ast.property_name_span(key)
  let key_ident = ast.Identifier(name:, span: key_span)
  use #(p2, value) <- result.map(case has_default {
    True -> {
      use #(p2, rhs) <- result.map(parse_assignment_expression(advance(p)))
      let value =
        ast.AssignmentExpression(
          operator: ast.Assign,
          left: key_ident,
          right: rhs,
          span: ast.Span(key_span.start, p2.prev_end),
        )
      let ctx = GrammarContext(..p2.ctx, has_cover_initializer: True)
      #(Parser(..p2, ctx:), value)
    }
    False -> Ok(#(p, key_ident))
  })
  #(p2, ast.InitProperty(key:, value:, shorthand: True))
}

fn parse_function_expression(
  p: Parser,
  is_async is_async: Bool,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let start = pos_of(p)
  use head <- result.try(parse_function_head(
    p,
    is_async:,
    name_binds_inside: True,
  ))
  let FunctionHead(after_name: p4, is_generator:, name:, ..) = head
  let p_inner =
    enter_function_context(p4, is_generator:, is_async:, strict_name: name)
  let fn_scope = p_inner.scopes.current
  use #(p5, params, body) <- result.map(
    parse_function_params_and_body(p_inner) |> restore_context_fn(p),
  )
  // §15.2.6 nfe name: own scope, declared after body, first wins
  let p5 = case name {
    None -> p5
    Some(name) ->
      Parser(
        ..p5,
        scopes: scope_builder.declare_in(
          p5.scopes,
          fn_scope,
          name,
          scope.FnNameBinding,
          synthetic: True,
        ),
      )
  }
  let function =
    ast.FunctionExpression(
      name: function_head_binding(head),
      params:,
      body:,
      is_generator:,
      is_async:,
      span: span_from(start, p5),
    )
  #(p5, function)
}

fn parse_class_expression(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  let start = pos_of(p)
  use #(p2, ClassSyntax(name:, super_class:, body:)) <- result.map(
    parse_class_head_and_tail(p, name_required: False, register_name: False),
  )
  #(
    p2,
    ast.ClassExpression(name:, super_class:, body:, span: span_from(start, p2)),
  )
}

fn parse_regex_literal(
  p: Parser,
) -> Result(#(Parser, ast.Expression), ParseError) {
  // relex from source as a regex literal
  let start_pos = pos_of(p)
  let body_start = start_pos + 1
  use end_pos <- result.try(
    regex.skip_body(p.bytes, body_start)
    |> result.map_error(regexp_syntax_error),
  )
  use #(flags_end, flags) <- result.try(
    regex.skip_flags(p.bytes, end_pos)
    |> result.map_error(regexp_syntax_error),
  )
  // pattern early errors; annex b grammar unless u/v
  use Nil <- result.try(
    regex.validate_pattern(p.bytes, body_start, end_pos - 1, flags)
    |> result.map_error(regexp_syntax_error),
  )
  let pattern =
    bytes.unsafe_slice(p.bytes, body_start, end_pos - 1 - body_start)
  // window past / is garbage; relex after the flags
  let p2 = rescan_from(p, flags_end)
  let span = ast.Span(start: start_pos, end: flags_end)
  Ok(#(p2, ast.RegExpLiteral(pattern:, flags: flags.text, span:)))
}

// pos is on the current line; rescanned constructs never span lines
fn rescan_from(p: Parser, pos: Int) -> Parser {
  let line = line_of(p)
  ensure_current(
    Parser(
      ..p,
      tokens: [],
      scan: lexer.scanner_at(p.bytes, pos, line, p.scan.source_kind),
      prev_line: line,
      prev_end: pos,
    ),
  )
}

// the "x" after from: its cooked value and end position
type ModuleSpecifier {
  ModuleSpecifier(value: String, end: Int)
}

fn parse_module_specifier(
  p: Parser,
) -> Result(#(Parser, ModuleSpecifier), ParseError) {
  use <- bool.guard(
    peek(p) != StringLiteral,
    Error(ExpectedModuleSpecifier(pos_of(p))),
  )
  use value <- result.map(module_specifier_value(p))
  #(advance(p), ModuleSpecifier(value:, end: pos_of(p) + peek_raw_len(p)))
}

fn expect_from_module_specifier(
  p: Parser,
) -> Result(#(Parser, ModuleSpecifier), ParseError) {
  use p2 <- result.try(expect(p, From))
  use #(p3, specifier) <- result.try(parse_module_specifier(p2))
  use p4 <- result.try(skip_import_attributes(p3))
  use p5 <- result.map(eat_semicolon(p4))
  #(p5, specifier)
}

// no import attributes supported: only an empty with {} parses
fn skip_import_attributes(p: Parser) -> Result(Parser, ParseError) {
  case peek(p) {
    With -> {
      use p2 <- result.try(expect(advance(p), LeftBrace))
      expect(p2, RightBrace)
    }
    _ -> Ok(p)
  }
}

fn finish_import_from(
  p: Parser,
  span_start: Int,
  phase: ast.ImportPhase,
  specifiers: List(ast.ImportSpecifier),
) -> Result(#(Parser, ast.ModuleItem), ParseError) {
  use #(p2, ModuleSpecifier(source, span_end)) <- result.map(
    expect_from_module_specifier(p),
  )
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

fn parse_namespace_import_tail(
  p: Parser,
  span_start: Int,
  phase: ast.ImportPhase,
  leading: List(ast.ImportSpecifier),
) -> Result(#(Parser, ast.ModuleItem), ParseError) {
  use p2 <- result.try(expect(p, As))
  let binding_name = peek_value(p2)
  let binding_span = span_of(p2)
  use p3 <- result.try(expect_identifier(p2))
  use p4 <- result.try(declare_import_binding(p3, binding_name))
  let ns =
    ast.ImportNamespaceSpecifier(local: binding_name, local_span: binding_span)
  finish_import_from(p4, span_start, phase, list.append(leading, [ns]))
}

fn parse_import_declaration(
  p: Parser,
) -> Result(#(Parser, ast.ModuleItem), ParseError) {
  let span_start = pos_of(p)
  let p2 = advance(p)
  // import source x from: only when a binding then from follow
  let is_source_phase =
    peek(p2) == Identifier
    && peek_value(p2) == "source"
    && is_identifier_or_keyword(peek_at(p2, 1))
    && peek_at(p2, 2) == From
  use <- bool.lazy_guard(is_source_phase, fn() {
    parse_source_phase_import(p2, span_start)
  })
  // import defer: only when * follows; escapes rejected (§5.1.5)
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
    StringLiteral -> {
      use #(p3, ModuleSpecifier(value, span_end)) <- result.try(
        parse_module_specifier(p2),
      )
      use p4 <- result.map(eat_semicolon(p3))
      #(
        p4,
        ast.ImportDeclaration(
          specifiers: [],
          source: value,
          phase: ast.PhaseEvaluation,
          span: ast.Span(start: span_start, end: span_end),
        ),
      )
    }
    Star ->
      parse_namespace_import_tail(
        advance(p2),
        span_start,
        ast.PhaseEvaluation,
        [],
      )
    LeftBrace -> {
      let p3 = advance(p2)
      use #(p4, specifiers) <- result.try(parse_import_specifiers(p3))
      finish_import_from(p4, span_start, ast.PhaseEvaluation, specifiers)
    }
    _ -> parse_default_import(p2, span_start)
  }
}

// import x from / import x, * as ns from / import x, { .. } from
fn parse_default_import(
  p: Parser,
  span_start: Int,
) -> Result(#(Parser, ast.ModuleItem), ParseError) {
  // default binding may be a contextual keyword like from
  use <- bool.guard(
    !is_identifier_or_keyword(peek(p)),
    Error(ExpectedImportSpecifier(pos_of(p))),
  )
  let default_name = peek_value(p)
  use Nil <- result.try(check_import_binding_name(p))
  use p2 <- result.try(declare_import_binding(p, default_name))
  let default_spec =
    ast.ImportDefaultSpecifier(local: default_name, local_span: span_of(p))
  let p3 = advance(p2)
  case peek(p3), peek_at(p3, 1) {
    Comma, Star ->
      parse_namespace_import_tail(
        advance(advance(p3)),
        span_start,
        ast.PhaseEvaluation,
        [default_spec],
      )
    Comma, LeftBrace -> {
      let p5 = advance(advance(p3))
      use #(p6, named_specs) <- result.try(parse_import_specifiers(p5))
      finish_import_from(p6, span_start, ast.PhaseEvaluation, [
        default_spec,
        ..named_specs
      ])
    }
    Comma, _ -> Error(ExpectedBraceOrStarAfterComma(pos_of(advance(p3))))
    From, _ ->
      finish_import_from(p3, span_start, ast.PhaseEvaluation, [default_spec])
    _, _ -> Error(ExpectedFromOrComma(pos_of(p3)))
  }
}

// source phase import; binding not modeled in the ast yet
fn parse_source_phase_import(
  p: Parser,
  span_start: Int,
) -> Result(#(Parser, ast.ModuleItem), ParseError) {
  let p2 = advance(p)
  use Nil <- result.try(check_import_binding_name(p2))
  use p3 <- result.try(declare_import_binding(p2, peek_value(p2)))
  finish_import_from(advance(p3), span_start, ast.PhaseSource, [])
}

fn parse_comma_list(
  p: Parser,
  acc: List(a),
  close: TokenKind,
  parse_one: fn(Parser) -> Result(#(Parser, a), ParseError),
  err: fn(Int) -> ParseError,
) -> Result(#(Parser, List(a)), ParseError) {
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
  p: Parser,
) -> Result(#(Parser, List(ast.ImportSpecifier)), ParseError) {
  parse_comma_list(
    p,
    [],
    RightBrace,
    parse_import_specifier,
    ExpectedCommaOrBraceInImport,
  )
}

fn is_specifier_name(kind: TokenKind) -> Bool {
  kind == Identifier || kind == StringLiteral || is_keyword_as_identifier(kind)
}

fn parse_import_specifier(
  p: Parser,
) -> Result(#(Parser, ast.ImportSpecifier), ParseError) {
  use <- bool.guard(
    !is_specifier_name(peek(p)),
    Error(ExpectedImportSpecifierName(pos_of(p))),
  )
  use imported_name <- result.try(specifier_name_value(p))
  let p2 = advance(p)
  case peek(p2) {
    As -> {
      let p3 = advance(p2)
      use p4 <- result.try(expect_identifier(p3))
      finish_import_named_specifier(p3, p4, imported_name)
    }
    _ -> finish_import_named_specifier(p, p2, imported_name)
  }
}

// name_at is at the local name; p carries the state past it
fn finish_import_named_specifier(
  name_at: Parser,
  p: Parser,
  imported: String,
) -> Result(#(Parser, ast.ImportSpecifier), ParseError) {
  let local = peek_value(name_at)
  let local_span = span_of(name_at)
  use Nil <- result.try(check_import_binding_name(name_at))
  use p <- result.map(declare_import_binding(p, local))
  #(p, ast.ImportNamedSpecifier(imported:, local:, local_span:))
}

fn parse_export_named_function(
  p: Parser,
  is_async is_async: Bool,
) -> Result(#(Parser, ast.Declaration), ParseError) {
  let name_offset = case is_async {
    True -> 2
    False -> 1
  }
  let name_offset = case peek_at(p, name_offset) == Star {
    True -> name_offset + 1
    False -> name_offset
  }
  use p2 <- result.try(declare_export_name_ahead(p, name_offset))
  use #(p3, function) <- result.map(parse_function_declaration(
    p2,
    name_required: True,
    is_async:,
  ))
  #(p3, ast.DeclareFunction(function:))
}

// the declaration parse that follows reports a missing name
fn declare_export_name_ahead(
  p: Parser,
  offset: Int,
) -> Result(Parser, ParseError) {
  case peek_value_at(p, offset) {
    "" -> Ok(p)
    name -> declare_export_name(p, name)
  }
}

fn export_named_decl(
  before: Parser,
  parsed: #(Parser, ast.Declaration),
) -> #(Parser, ast.ModuleItem) {
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

fn parse_export_named_class(
  p: Parser,
) -> Result(#(Parser, ast.Declaration), ParseError) {
  use p2 <- result.try(declare_export_name_ahead(p, 1))
  parse_class_declaration(p2)
}

fn parse_default_fn(
  p: Parser,
  is_async is_async: Bool,
) -> Result(#(Parser, DefaultExportDecl), ParseError) {
  use #(p2, function) <- result.map(parse_function_declaration(
    p,
    name_required: False,
    is_async:,
  ))
  #(p2, DefaultFn(function:))
}

fn parse_default_class(
  p: Parser,
) -> Result(#(Parser, DefaultExportDecl), ParseError) {
  use #(p2, ClassSyntax(name:, super_class:, body:)) <- result.map(
    parse_class_head_and_tail(p, name_required: False, register_name: True),
  )
  #(p2, DefaultClass(name:, super_class:, body:))
}

// §16.2.3.7 *default* binding, VarBinding per emit
fn declare_default_export(p: Parser) -> Parser {
  let scopes =
    scope_builder.declare(
      p.scopes,
      summary.default_export_local_name,
      scope.VarBinding,
      synthetic: True,
    )
  Parser(..p, scopes:)
}

fn finish_export_default_decl(
  p_export: Parser,
  p_decl: Parser,
  parse: fn(Parser) -> Result(#(Parser, DefaultExportDecl), ParseError),
) -> Result(#(Parser, ast.ModuleItem), ParseError) {
  let decl_start = pos_of(p_decl)
  use #(p4, decl) <- result.map(parse(p_decl))
  let decl_span = span_from(decl_start, p4)
  let p4 = case default_export_name(decl) {
    None -> declare_default_export(p4)
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

fn finish_export_default_expr(
  p_export: Parser,
  p_expr: Parser,
) -> Result(#(Parser, ast.ModuleItem), ParseError) {
  use #(p4, expr) <- result.try(parse_assignment_expression(p_expr))
  use p5 <- result.map(eat_semicolon(p4))
  let p5 = declare_default_export(p5)
  #(
    p5,
    ast.ExportDefaultDeclaration(
      declaration: expr,
      line: line_of(p_export),
      span: ast.Span(start: pos_of(p_export), end: consumed_end(p_export, p5)),
    ),
  )
}

// p is at the module specifier string
fn finish_export_all(
  p: Parser,
  span_start: Int,
  exported: Option(String),
) -> Result(#(Parser, ast.ModuleItem), ParseError) {
  use #(p2, ModuleSpecifier(value, span_end)) <- result.try(
    parse_module_specifier(p),
  )
  use p3 <- result.map(eat_semicolon(p2))
  #(
    p3,
    ast.ExportAllDeclaration(
      exported:,
      source: value,
      span: ast.Span(start: span_start, end: span_end),
    ),
  )
}

fn parse_export_declaration(
  p: Parser,
) -> Result(#(Parser, ast.ModuleItem), ParseError) {
  let p2 = advance(p)
  case peek(p2) {
    Default -> {
      use p2b <- result.try(declare_export_name(p2, "default"))
      let p3 = advance(p2b)
      case peek(p3) {
        Function ->
          finish_export_default_decl(p, p3, parse_default_fn(_, is_async: False))
        Class -> finish_export_default_decl(p, p3, parse_default_class)
        Async ->
          case peek_at(p3, 1) {
            Function ->
              finish_export_default_decl(p, p3, parse_default_fn(
                _,
                is_async: True,
              ))
            _ -> finish_export_default_expr(p, p3)
          }
        _ -> finish_export_default_expr(p, p3)
      }
    }
    Var | Let | Const -> {
      // bindings declared under this flag also become export names
      let exporting =
        Parser(..p2, ctx: GrammarContext(..p2.ctx, in_export_decl: True))
      use #(p3, declaration) <- result.map(parse_variable_declaration_decl(
        exporting,
      ))
      let p3 =
        Parser(..p3, ctx: GrammarContext(..p3.ctx, in_export_decl: False))
      export_named_decl(p, #(p3, declaration))
    }
    Function ->
      parse_export_named_function(p2, is_async: False)
      |> result.map(export_named_decl(p, _))
    Class -> result.map(parse_export_named_class(p2), export_named_decl(p, _))
    Async ->
      case peek_at(p2, 1) {
        Function ->
          parse_export_named_function(p2, is_async: True)
          |> result.map(export_named_decl(p, _))
        _ -> Error(ExpectedFunctionAfterAsync(pos_of(p2)))
      }
    Star -> parse_export_star(advance(p2), pos_of(p))
    LeftBrace -> parse_export_list(advance(p2), p)
    _ -> Error(UnexpectedAfterExport(pos_of(p2)))
  }
}

// export * from / export * as name from; p is past the *
fn parse_export_star(
  p: Parser,
  span_start: Int,
) -> Result(#(Parser, ast.ModuleItem), ParseError) {
  case peek(p) {
    As -> {
      let p2 = advance(p)
      use exported <- result.try(specifier_name_value(p2))
      let p3 = case is_specifier_name(peek(p2)) {
        True -> advance(p2)
        False -> p2
      }
      use p4 <- result.try(declare_export_name(p3, exported))
      use p5 <- result.try(expect(p4, From))
      finish_export_all(p5, span_start, Some(exported))
    }
    From -> finish_export_all(advance(p), span_start, None)
    _ -> Error(ExpectedAsOrFromAfterExportStar(pos_of(p)))
  }
}

// export { .. } or export { .. } from; p is past the {
fn parse_export_list(
  p: Parser,
  p_export: Parser,
) -> Result(#(Parser, ast.ModuleItem), ParseError) {
  use #(p2, specifiers) <- result.try(parse_export_specifiers(p))
  let span = fn(p_end) {
    ast.Span(start: pos_of(p_export), end: consumed_end(p_export, p_end))
  }
  case peek(p2) {
    From -> {
      use #(p3, ModuleSpecifier(value:, ..)) <- result.try(
        parse_module_specifier(advance(p2)),
      )
      use p4 <- result.map(eat_semicolon(p3))
      let source = Some(value)
      #(p4, ast.ExportNamed(specifiers:, source:, span: span(p4)))
    }
    _ -> {
      use p3 <- result.map(eat_semicolon(p2))
      // only local exports must resolve to module bindings
      let export_local_refs =
        list.fold(specifiers, p3.export_local_refs, fn(refs, specifier) {
          [#(specifier.local, specifier.local_span.start), ..refs]
        })
      let p3 = Parser(..p3, export_local_refs:)
      #(p3, ast.ExportNamed(specifiers:, source: None, span: span(p3)))
    }
  }
}

fn parse_export_specifiers(
  p: Parser,
) -> Result(#(Parser, List(ast.ExportSpecifier)), ParseError) {
  parse_comma_list(
    p,
    [],
    RightBrace,
    parse_export_specifier,
    ExpectedCommaOrBraceInExport,
  )
}

fn parse_export_specifier(
  p: Parser,
) -> Result(#(Parser, ast.ExportSpecifier), ParseError) {
  use <- bool.guard(
    !is_specifier_name(peek(p)),
    Error(ExpectedExportSpecifierName(pos_of(p))),
  )
  use local <- result.try(specifier_name_value(p))
  let local_span = span_of(p)
  let p2 = advance(p)
  // p3 stays on the last token of the specifier
  use #(p3, exported) <- result.try(case peek(p2) {
    As -> {
      let p3 = advance(p2)
      use <- bool.lazy_guard(!is_specifier_name(peek(p3)), fn() {
        Error(error_at_current(p3, ExpectedExportAlias(pos_of(p3))))
      })
      use exported <- result.map(specifier_name_value(p3))
      #(p3, exported)
    }
    _ -> Ok(#(p, local))
  })
  use p4 <- result.map(declare_export_name(p3, exported))
  #(advance(p4), ast.ExportSpecifier(local:, exported:, local_span:))
}

// p is at a body {: apply its directive prologue
fn apply_body_use_strict(p: Parser) -> Result(Parser, ParseError) {
  use <- bool.guard(peek(p) != LeftBrace, Ok(p))
  let body = look_skip(look_from(p))
  case p.ctx.strict, p.ctx.has_non_simple_param {
    False, _ -> scan_directive_prologue(p, body)
    // use strict + non-simple params errors even if already strict
    True, True -> {
      use <- bool.guard(
        prologue_has_use_strict(body),
        Error(MisplacedUseStrictDirective(pos_of(p))),
      )
      Ok(p)
    }
    True, False -> Ok(p)
  }
}

// pure lookahead; never advances the real scan (rescan invariant)
type Look {
  Look(tokens: List(Token), scan: lexer.Scanner)
}

fn look_from(p: Parser) -> Look {
  Look(tokens: p.tokens, scan: p.scan)
}

// lexer errors surface as Illegal, never accepted here
fn look_next(look: Look) -> #(Token, Look) {
  case look.tokens {
    [token, ..rest] -> #(token, Look(..look, tokens: rest))
    [] -> {
      let #(token, scan) = lexer.scan_next(look.scan)
      #(token, Look(tokens: [], scan:))
    }
  }
}

fn look_at(look: Look, n: Int) -> Token {
  let #(token, look) = look_next(look)
  case n <= 0 || token.kind == Eof {
    True -> token
    False -> look_at(look, n - 1)
  }
}

fn look_skip(look: Look) -> Look {
  let #(_, look) = look_next(look)
  look
}

fn look_skip_semicolon(look: Look) -> Look {
  let #(token, after) = look_next(look)
  case token.kind {
    Semicolon -> after
    _ -> Look(tokens: [token, ..after.tokens], scan: after.scan)
  }
}

// Some(directives before use strict) when present
fn prologue_use_strict(look: Look, seen: List(Token)) -> Option(List(Token)) {
  let #(token, look) = look_next(look)
  case token.kind {
    StringLiteral ->
      case token.value {
        "use strict" -> Some(seen)
        _ -> prologue_use_strict(look_skip_semicolon(look), [token, ..seen])
      }
    _ -> None
  }
}

fn prologue_has_use_strict(look: Look) -> Bool {
  option.is_some(prologue_use_strict(look, []))
}

fn apply_directive_prologue(p: Parser) -> Result(Parser, ParseError) {
  case p.ctx.strict {
    True -> Ok(p)
    False -> scan_directive_prologue(p, look_from(p))
  }
}

fn scan_directive_prologue(
  p: Parser,
  look: Look,
) -> Result(Parser, ParseError) {
  case prologue_use_strict(look, []) {
    None -> Ok(p)
    Some(seen_directives) -> {
      use Nil <- result.try(check_retroactive_octals(p, seen_directives))
      let p = Parser(..p, ctx: GrammarContext(..p.ctx, strict: True))
      check_retroactive_params(p)
    }
  }
}

// annex b escapes before use strict are retroactively illegal
fn check_retroactive_octals(
  p: Parser,
  seen_directives: List(Token),
) -> Result(Nil, ParseError) {
  use <- bool.guard(
    list.any(seen_directives, fn(token) { token.annex_b_legacy }),
    Error(OctalEscapeStrictMode(pos_of(p))),
  )
  Ok(Nil)
}

// params already parsed become strict retroactively
fn check_retroactive_params(p: Parser) -> Result(Parser, ParseError) {
  use <- bool.guard(
    p.ctx.has_non_simple_param,
    Error(MisplacedUseStrictDirective(pos_of(p))),
  )
  case first_strict_violation(p.ctx.param_bound_names) {
    Some(#(name, kind)) -> Error(strict_name_error(kind, name, pos_of(p)))
    None -> Ok(p)
  }
}

// the one place nested function state is initialised; only strict carries in
fn enter_function_context(
  p: Parser,
  is_generator is_generator: Bool,
  is_async is_async: Bool,
  strict_name strict_name: Option(String),
) -> Parser {
  let #(scopes, _id) = scope_builder.push(p.scopes, scope.Function)
  Parser(
    ..p,
    ctx: GrammarContext(
      strict: p.ctx.strict,
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
      declaring: NotDeclaring,
      in_block: False,
      module_top_level: False,
      in_single_stmt_pos: False,
      has_cover_initializer: False,
      dup_proto_pos: None,
      in_formal_params: False,
      in_catch_param: False,
      in_arrow_params: False,
      has_non_simple_param: False,
      param_bound_names: [],
      pending_strict_name: strict_name,
      in_export_decl: False,
      in_case_clause: False,
    ),
    scopes:,
  )
}

// arrows inherit super/new.target and the arguments restriction
fn enter_arrow_context(
  p: Parser,
  is_async is_async: Bool,
  param_names param_names: List(String),
) -> Parser {
  let inner =
    enter_function_context(p, is_generator: False, is_async:, strict_name: None)
  // arrow scopes own no lexical pseudo-slots
  let scopes =
    scope_builder.update_current_fn(inner.scopes, fn(fi) {
      scope_builder.RawFunctionInfo(..fi, is_arrow: True)
    })
  let scopes =
    list.fold(param_names, scopes, fn(acc, name) {
      scope_builder.declare(acc, name, scope.ParamBinding, synthetic: False)
    })
  Parser(
    ..inner,
    scopes:,
    ctx: GrammarContext(
      ..inner.ctx,
      allow_super_call: p.ctx.allow_super_call,
      allow_super_property: p.ctx.allow_super_property,
      allow_new_target: p.ctx.allow_new_target,
      in_class_field_init: p.ctx.in_class_field_init || p.ctx.in_static_block,
      // §15.3: concise body inherits [In]
      allow_in: p.ctx.allow_in,
    ),
  )
}

fn enter_method_context(
  p: Parser,
  is_generator is_generator: Bool,
  is_async is_async: Bool,
  is_constructor is_constructor: Bool,
  has_super_class has_super_class: Bool,
) -> Parser {
  let inner =
    enter_function_context(p, is_generator:, is_async:, strict_name: None)
  Parser(
    ..inner,
    ctx: GrammarContext(
      ..inner.ctx,
      allow_super_call: is_constructor && has_super_class,
      allow_super_property: True,
      in_method: True,
    ),
  )
}

// §15.7.1 static block: [~Yield, +Await, ~Return]
fn enter_static_block_context(p: Parser) -> Parser {
  // §15.7.14: retag as ClassStaticBlock scope
  let inner =
    enter_function_context(
      p,
      is_generator: False,
      is_async: True,
      strict_name: None,
    )
  let scopes =
    scope_builder.update_current(inner.scopes, fn(s) {
      scope_builder.RawScope(..s, kind: scope.ClassStaticBlock, is_strict: True)
    })
  Parser(
    ..inner,
    scopes:,
    ctx: GrammarContext(
      ..inner.ctx,
      function_depth: 0,
      in_static_block: True,
      allow_super_property: True,
    ),
  )
}

fn restore_context_fn(
  res: Result(#(Parser, List(ast.Pattern), List(ast.StmtWithLine)), ParseError),
  outer: Parser,
) -> Result(#(Parser, List(ast.Pattern), List(ast.StmtWithLine)), ParseError) {
  use #(p, params, body) <- result.map(res)
  #(restore_outer_context(p, outer), params, body)
}

// restore ctx whole; scopes flow forward, only the cursor moves back
fn restore_outer_context(p: Parser, outer: Parser) -> Parser {
  Parser(
    ..p,
    ctx: outer.ctx,
    scopes: scope_builder.enter(p.scopes, outer.scopes.current),
  )
}

fn peek(p: Parser) -> TokenKind {
  case p.tokens {
    [token.Token(kind: k, ..), ..] -> k
    [] -> Eof
  }
}

// bounded pure lookahead; deepest grammar lookahead is 3
fn peek_token_at(p: Parser, n: Int) -> Token {
  look_at(look_from(p), n)
}

fn peek_at(p: Parser, n: Int) -> TokenKind {
  case n, p.tokens {
    0, _ -> peek(p)
    1, [_, token.Token(kind: k, ..), ..] -> k
    _, _ -> {
      let token.Token(kind: k, ..) = peek_token_at(p, n)
      k
    }
  }
}

fn peek_value(p: Parser) -> String {
  case p.tokens {
    [token.Token(value: v, ..), ..] -> v
    [] -> ""
  }
}

fn peek_value_at(p: Parser, n: Int) -> String {
  case n {
    0 -> peek_value(p)
    _ -> {
      let token.Token(kind:, value:, ..) = peek_token_at(p, n)
      case kind {
        Eof -> ""
        _ -> value
      }
    }
  }
}

// escaped contextual keywords are not keywords (§12.7.2)
fn peek_had_escape(p: Parser) -> Bool {
  case p.tokens {
    [token.Token(had_escape: e, ..), ..] -> e
    [] -> False
  }
}

// annex b legacy forms strict code forbids; decided by the lexer
fn peek_annex_b_legacy(p: Parser) -> Bool {
  case p.tokens {
    [token.Token(annex_b_legacy: legacy, ..), ..] -> legacy
    [] -> False
  }
}

fn peek_raw_len(p: Parser) -> Int {
  case p.tokens {
    [token.Token(raw_len: rl, ..), ..] -> rl
    [] -> 0
  }
}

fn pos_of(p: Parser) -> Int {
  case p.tokens {
    [token.Token(pos:, ..), ..] -> pos
    [] -> 0
  }
}

fn span_of(p: Parser) -> ast.Span {
  case p.tokens {
    [token.Token(pos:, raw_len:, ..), ..] ->
      ast.Span(start: pos, end: pos + raw_len)
    [] -> ast.Span(start: 0, end: 0)
  }
}

// after.prev_end, or next token start when nothing was consumed
fn consumed_end(before: Parser, after: Parser) -> Int {
  case after.prev_end == before.prev_end {
    True -> pos_of(after)
    False -> after.prev_end
  }
}

fn span_from(start: Int, p_after: Parser) -> ast.Span {
  ast.Span(start:, end: p_after.prev_end)
}

// falls back to the previous line at eof
fn line_of(p: Parser) -> Int {
  case p.tokens {
    [token.Token(line:, ..), ..] -> line
    [] -> p.prev_line
  }
}

fn advance(p: Parser) -> Parser {
  case p.tokens {
    [token.Token(line:, pos:, raw_len:, ..), ..rest] -> {
      let prev_end = pos + raw_len
      case rest {
        [] -> {
          let #(token, scan) = lexer.scan_next(p.scan)
          refill_window(p, token, scan, line, prev_end)
        }
        [token] -> refill_window(p, token, p.scan, line, prev_end)
        _ -> Parser(..p, tokens: rest, prev_line: line, prev_end:)
      }
    }
    [] -> p
  }
}

// one token of lookahead, never past a token the parser may rescan
fn refill_window(
  p: Parser,
  token: Token,
  scan: lexer.Scanner,
  prev_line: Int,
  prev_end: Int,
) -> Parser {
  case may_rescan(token.kind) {
    True -> Parser(..p, tokens: [token], scan:, prev_line:, prev_end:)
    False -> {
      let #(next, scan) = lexer.scan_next(scan)
      Parser(..p, tokens: [token, next], scan:, prev_line:, prev_end:)
    }
  }
}

// one token lookahead, never past / /= or }
fn may_rescan(kind: TokenKind) -> Bool {
  case kind {
    Slash | SlashEqual | RightBrace | Eof | LexFailure(_) -> True
    _ -> False
  }
}

// lexer errors arrive as a zero-length LexFailure token
fn ensure_current(p: Parser) -> Parser {
  case p.tokens {
    [] -> {
      let #(token, scan) = lexer.scan_next(p.scan)
      Parser(..p, tokens: [token], scan:)
    }
    _ -> p
  }
}

fn expect(p: Parser, kind: TokenKind) -> Result(Parser, ParseError) {
  case peek(p) {
    found if found == kind -> Ok(advance(p))
    found -> Error(error_at_current(p, ExpectedToken(pos_of(p), kind, found)))
  }
}

fn expect_identifier(p: Parser) -> Result(Parser, ParseError) {
  case is_identifier_or_keyword(peek(p)) {
    True -> Ok(advance(p))
    False -> Error(error_at_current(p, ExpectedIdentifier(pos_of(p))))
  }
}

// LexFailure carries the lexer error; Illegal does not
fn illegal_token_error(p: Parser) -> ParseError {
  case peek(p) {
    LexFailure(err) -> lex_error(err)
    kind -> UnexpectedToken(pos_of(p), kind)
  }
}

// route unexpected tokens here or lexer errors get masked
fn error_at_current(p: Parser, otherwise: ParseError) -> ParseError {
  case peek(p) {
    Illegal | LexFailure(_) -> illegal_token_error(p)
    _ -> otherwise
  }
}

fn eat_semicolon(p: Parser) -> Result(Parser, ParseError) {
  case peek(p) {
    Semicolon -> Ok(advance(p))
    RightBrace | Eof -> Ok(p)
    // prefer the lexer message over expected ;
    Illegal | LexFailure(_) ->
      case has_line_break_before(p) {
        True -> Ok(p)
        False -> Error(illegal_token_error(p))
      }
    _ ->
      case has_line_break_before(p) {
        True -> Ok(p)
        False -> Error(ExpectedSemicolon(pos_of(p)))
      }
  }
}

fn has_line_break_before(p: Parser) -> Bool {
  case p.tokens {
    [token.Token(line: current_line, ..), ..] -> current_line > p.prev_line
    [] -> True
  }
}

fn token_line_at(p: Parser, n: Int) -> Int {
  let token.Token(kind:, line:, ..) = peek_token_at(p, n)
  case kind {
    // -1 past eof so line comparisons never match
    Eof -> -1
    _ -> line
  }
}

fn eat_optional_name(p: Parser) -> Result(Parser, ParseError) {
  case simple_binding_name(p) {
    Some(name) -> {
      use Nil <- result.map(check_binding_identifier(p, name))
      advance(p)
    }
    None -> Ok(p)
  }
}
