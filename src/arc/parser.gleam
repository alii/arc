import arc/bytecode/lexical
import arc/compiler/ast_util
import arc/compiler/scope
import arc/parser/ast
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
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

// §12.9.4 cook string escapes
@external(erlang, "arc_escape_ffi", "decode_string_escapes")
fn decode_string_escapes(raw: String) -> String

// §12.9.6 template value; Error(Nil) on an invalid escape
@external(erlang, "arc_escape_ffi", "cook_template_string")
fn cook_template_string(raw: String) -> Result(String, Nil)

pub type ParseMode {
  Script
  Module
}

pub type ParseError =
  error.ParseError

pub fn parse_error_to_string(err: ParseError) -> String {
  error.parse_error_to_string(err)
}

pub fn parse_error_pos(err: ParseError) -> Int {
  error.parse_error_pos(err)
}

type BindingKind {
  BindingNone
  BindingVar
  // bound: names so far in this declaration (§14.3.1)
  BindingLexical(kind: scope.BindingKind, bound: Set(String))
  BindingParam
}

fn in_lexical_decl(ctx: Ctx) -> Bool {
  case ctx.binding_kind {
    BindingLexical(..) -> True
    BindingNone | BindingVar | BindingParam -> False
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

// saved and restored whole at function boundaries, see restore_outer_context
type Ctx {
  Ctx(
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
    binding_kind: BindingKind,
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

type P {
  P(
    // bounded prefetch window, not the whole file
    tokens: List(Token),
    scan: lexer.Scanner,
    mode: ParseMode,
    prev_line: Int,
    prev_end: Int,
    bytes: BitArray,
    ctx: Ctx,
    // §15.7.1 enclosing class body count, not reset by functions
    class_private_depth: Int,
    // unresolved #name refs: (name, depth, pos)
    private_refs: List(#(String, Int, Int)),
    // direct eval only: the caller's private names (§19.2.1.1 step 5)
    outer_private_names: List(String),
    last_expr_assignable: Bool,
    // tells {a: b = 1} (pattern ok) from {a: 0}
    last_expr_is_assignment: Bool,
    has_invalid_pattern: Bool,
    export_names: Set(String),
    export_local_refs: List(#(String, Int)),
    import_bindings: Set(String),
    last_expr_name: Option(String),
    sb: scope.ScopeBuilder,
  )
}

// the one place ctx.allow_in is restored
fn with_allow_in(
  p: P,
  value: Bool,
  then: fn(P) -> Result(#(P, a), ParseError),
) -> Result(#(P, a), ParseError) {
  let saved = p.ctx.allow_in
  use <- bool.lazy_guard(saved == value, fn() { then(p) })
  use #(p, parsed) <- result.map(then(
    P(..p, ctx: Ctx(..p.ctx, allow_in: value)),
  ))
  #(P(..p, ctx: Ctx(..p.ctx, allow_in: saved)), parsed)
}

fn set_not_assignable(
  res: Result(#(P, ast.Expression), ParseError),
) -> Result(#(P, ast.Expression), ParseError) {
  use #(p, expr) <- result.map(res)
  #(P(..p, last_expr_assignable: False, last_expr_is_assignment: False), expr)
}

fn ok_lit(
  p: P,
  expr: ast.Expression,
) -> Result(#(P, ast.Expression), ParseError) {
  Ok(#(P(..advance(p), last_expr_assignable: False), expr))
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
          in_catch_param: False,
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

pub fn parse_script(
  source: String,
) -> Result(#(List(ast.StmtWithLine), scope.ScopeBuilder), ParseError) {
  use p <- init_parser(source, Script)
  script_body(p)
}

pub fn parse_module(
  source: String,
) -> Result(#(List(ast.ModuleItem), scope.ScopeBuilder), ParseError) {
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
) -> Result(#(List(ast.StmtWithLine), scope.ScopeBuilder), ParseError) {
  use p <- init_parser(source, Script)
  script_body(
    P(
      ..p,
      ctx: Ctx(
        ..p.ctx,
        strict: p.ctx.strict || strict,
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
  p: P,
) -> Result(#(List(ast.StmtWithLine), scope.ScopeBuilder), ParseError) {
  use p <- result.try(check_use_strict_at_start(p))
  use #(p_final, stmts) <- result.try(parse_statement_list(p, True, []))
  use Nil <- result.try(check_unresolved_private_refs(p_final))
  let sb = scope.sb_reorder_block_children(p_final.sb, scope.root_scope_id)
  Ok(#(stmts, sb))
}

fn module_body(
  p: P,
) -> Result(#(List(ast.ModuleItem), scope.ScopeBuilder), ParseError) {
  use #(p_final, items) <- result.try(parse_module_body(p, []))
  use Nil <- result.try(validate_export_local_refs(p_final))
  use Nil <- result.try(check_unresolved_private_refs(p_final))
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

fn validate_export_local_refs(p: P) -> Result(Nil, ParseError) {
  use #(name, pos) <- list.try_each(p.export_local_refs)
  let declared =
    scope.sb_root_has(p.sb, name) || set.contains(p.import_bindings, name)
  case declared {
    True -> Ok(Nil)
    False -> Error(UndeclaredExportBinding(pos, name))
  }
}

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
      case async_function_start(p), peek_at(p, 1) {
        True, _ -> parse_function_declaration(p, True, True)
        False, Colon -> parse_labeled_statement(p)
        False, _ -> parse_expression_statement(p)
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
      case peek_at(p, 1) {
        Dot | LeftParen -> parse_expression_statement(p)
        _ -> {
          case p.mode {
            Module -> Error(ImportNotTopLevel(pos_of(p)))
            Script -> parse_expression_statement(p)
          }
        }
      }
    }
    Export -> {
      case p.mode {
        Module -> Error(ExportNotTopLevel(pos_of(p)))
        Script -> Error(UnexpectedExport(pos_of(p)))
      }
    }
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

// using [ and using { are not declarations
fn is_using_decl_start(p: P, at: Int) -> Bool {
  peek_at(p, at) == Identifier
  && peek_value_at(p, at) == "using"
  && token_line_at(p, at + 1) == token_line_at(p, at)
  && is_binding_ident_token(peek_at(p, at + 1))
}

fn is_await_using_decl_start(p: P) -> Bool {
  { p.ctx.in_async || p.mode == Module }
  && !p.ctx.in_static_block
  && token_line_at(p, 1) == token_line_at(p, 0)
  && is_using_decl_start(p, 1)
}

fn is_binding_ident_token(kind: TokenKind) -> Bool {
  kind == Identifier || is_contextual_keyword(kind)
}

// script top level, case clauses and single statements reject using
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

fn parse_using_declaration(
  p: P,
  is_await is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  use Nil <- result.try(check_using_placement(p))
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

fn parse_single_statement(
  p: P,
  allow_fn: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  // must not leak to later statements
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
      // §13.4: only let [ is excluded outright
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
      case allow_fn && !p.ctx.strict && peek_at(p, 1) != Star {
        // annex b §B.3.3: parse as if wrapped in a block
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

fn enter_block_scope(p: P) -> P {
  let #(sb, _id) = scope.sb_push(p.sb, scope.Block)
  P(..p, sb:, ctx: Ctx(..p.ctx, in_block: True))
}

fn restore_block_scope(after p: P, before saved: P) -> P {
  // flip for-head children to source order for finalize
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

fn parse_block_body(p: P) -> Result(#(P, List(ast.StmtWithLine)), ParseError) {
  // fast path: an empty block declares nothing
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
  // only the direct body list conflicts with params
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
  // prune or reorder in lockstep with emit_block
  let sb =
    scope.sb_close_block(p4.sb, block_id)
    |> scope.sb_enter(p2.sb.current)
  Ok(#(
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

fn parse_variable_declaration_decl(
  p: P,
) -> Result(#(P, ast.Declaration), ParseError) {
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
      let init_start = pos_of(p2)
      use #(p3, init_expr) <- result.try(
        parse_assignment_expression(advance(p2)),
      )
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

// not gated on in_method: it stays true through the body
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
          case
            p.ctx.strict
            || p.ctx.in_arrow_params
            || p.ctx.in_method
            || p.ctx.has_non_simple_param
          {
            True -> Error(DuplicateParameterName(pos_of(p), name))
            False -> bind()
          }
        False -> bind()
      }
    False -> Ok(p)
  }
}

// an Identifier spelling a reserved word came from a \u escape
fn check_not_escaped_reserved_word(
  p: P,
  name: String,
) -> Result(Nil, ParseError) {
  case is_reserved_word_kind(lexer.keyword_or_identifier(name)) {
    True -> Error(EscapedReservedWord(pos_of(p), name))
    False -> Ok(Nil)
  }
}

// §13.1.1 shared reserved-name checks
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

// §13.1.1 identifier reference
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

// §14.2.1; the implicit arguments placeholder is exempt
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
      // §14.3.2, and §16.2.1.1 at module root
      use <- bool.guard(
        scope.sb_var_conflicts_lexical(p.sb, name)
          || scope.sb_var_conflicts_module_fn(p.sb, name),
        Error(IdentifierAlreadyDeclared(pos_of(p), name)),
      )
      Ok(P(..p, sb: scope.sb_declare_var(p.sb, name, synthetic: False)))
    }
    BindingNone -> Ok(p)
  }
}

// is_plain: annex b §B.3.2 applies to plain functions only
fn register_function_name(
  p: P,
  name: String,
  name_pos: Int,
  is_plain: Bool,
) -> Result(P, ParseError) {
  // annex b §B.3.1: synthetic block, never clashes
  use <- bool.guard(!p.ctx.strict && p.ctx.in_single_stmt_pos, Ok(p))
  // §16.1.1 script vs §16.2.1.1 module top-level functions
  case p.ctx.in_block, p.ctx.module_top_level {
    // plain sb_declare: no hoisted_vars entry marks a module function
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
    // §14.2.2 lexical; annex b §B.3.2 var-hoist candidate
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
      use <- bool.guard(
        scope.sb_current_has_kind(p.sb, name, scope.LetBinding)
          || scope.sb_current_has_kind(p.sb, name, scope.ConstBinding),
        Error(IdentifierAlreadyDeclared(name_pos, name)),
      )
      Ok(P(..p, sb: scope.sb_declare_var(p.sb, name, synthetic: False)))
    }
  }
}

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

fn check_export_binding(p: P, name: String) -> Result(P, ParseError) {
  case p.ctx.in_export_decl {
    True -> check_duplicate_export(p, name)
    False -> Ok(p)
  }
}

// = commits: errors propagate, no backtrack
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
      // §13.3.3: object rest is an identifier only
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
      use #(p4, val_pat) <- result.try(parse_binding_pattern(advance(p2)))
      use #(p5, final_pat) <- result.map(parse_pattern_default(p4, val_pat))
      #(p5, ast.PatternProperty(key:, value: final_pat, shorthand: False))
    }
    next -> {
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

fn numeric_literal(p: P) -> Result(ast.Expression, ParseError) {
  let span = span_of(p)
  case number.parse_numeric_literal(peek_value(p)) {
    Ok(number.NumberValue(n)) -> Ok(ast.NumberLiteral(value: n, span:))
    Ok(number.BigIntValue(i)) -> Ok(ast.BigIntLiteral(value: i, span:))
    Error(err) -> Error(MalformedNumericLiteral(pos_of(p), err))
  }
}

// the only place annex b string escapes are rejected
fn string_literal_value(p: P) -> Result(String, ParseError) {
  string_token_value(p, p.ctx.strict)
}

// module code is always strict
fn module_specifier_value(p: P) -> Result(String, ParseError) {
  string_token_value(p, True)
}

// string export names use their cooked value
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

fn numeric_property_key(p: P) -> Result(ast.PropertyKey, ParseError) {
  let span = span_of(p)
  case number.parse_numeric_literal(peek_value(p)) {
    Ok(number.NumberValue(n)) -> Ok(ast.KeyNumber(value: n, span:))
    Ok(number.BigIntValue(i)) -> Ok(ast.KeyBigInt(value: i, span:))
    Error(err) -> Error(MalformedNumericLiteral(pos_of(p), err))
  }
}

// private names lex as # prefixed identifiers
fn identifier_property_key(name: String, span: ast.Span) -> ast.PropertyKey {
  case name {
    "#" <> _ -> ast.KeyPrivate(name:, span:)
    _ -> ast.KeyIdentifier(name:, span:)
  }
}

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
      // computed key is [+In]
      use #(p4, expr) <- result.map({
        use p2 <- with_allow_in(advance(p), True)
        use #(p3, expr) <- result.try(parse_assignment_expression(p2))
        use p4 <- result.map(expect(p3, RightBracket))
        #(p4, expr)
      })
      #(p4, ast.KeyComputed(expr))
    }
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

fn parse_for_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  let #(p2, is_await) = case peek(p2) {
    Await -> #(advance(p2), True)
    _ -> #(p2, False)
  }
  let p2 = set_loop_depth(p2, p2.ctx.loop_depth + 1)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, stmt) <- result.try(parse_for_head(p3, is_await))
  Ok(#(set_loop_depth(p4, p.ctx.loop_depth), stmt))
}

fn set_loop_depth(p: P, depth: Int) -> P {
  P(..p, ctx: Ctx(..p.ctx, loop_depth: depth))
}

fn parse_for_head(
  p: P,
  is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  case peek(p) {
    Semicolon -> {
      let p2 = advance(p)
      parse_for_classic_rest(p2, None)
    }
    Var -> parse_for_declaration(p, is_await)
    Const -> parse_for_declaration_scoped(p, is_await)
    Let -> {
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
    // for (using of = ..;;) binds of, for (using of x) does not
    Identifier ->
      case
        is_using_decl_start(p, 0)
        && { peek_at(p, 1) != Of || peek_at(p, 2) == Equal }
      {
        True -> parse_for_using_scoped(p, is_await, is_await_using: False)
        False -> parse_for_expression(p, is_await)
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
        False -> parse_for_expression(p, is_await)
      }
    // await using of IS a declaration binding of
    Await ->
      case is_await_using_decl_start(p) {
        True -> parse_for_using_scoped(p, is_await, is_await_using: True)
        False -> parse_for_expression(p, is_await)
      }
    _ -> parse_for_expression(p, is_await)
  }
}

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
      // classic head declarators are [~In]
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

fn parse_for_declaration_scoped(
  p: P,
  is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  // head names get their own scope: for(let a;;); let a; is valid
  use #(p2, stmt) <- result.map(parse_for_declaration(
    enter_block_scope(p),
    is_await,
  ))
  #(restore_block_scope(p2, p) |> exit_for_decl_context(p), stmt)
}

// drop the for-head binding context before the rest
fn exit_for_decl_context(p: P, outer: P) -> P {
  P(..p, ctx: Ctx(..p.ctx, binding_kind: outer.ctx.binding_kind))
}

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
  // B.3.4: for-of var names vs enclosing catch param
  let catch_params = scope.sb_nearest_catch_params(p2.sb)
  use #(p3, pattern) <- result.try(parse_for_binding_or_declarator(p2))
  let decl =
    ast.ForInitDeclaration(kind:, declarations: [
      ast.VariableDeclarator(id: pattern, init: None),
    ])
  // §14.7.5.9: head binding written each iteration
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
      // [~In] so for (var x = a in b) is for-in
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

// §14.7.4: the whole declarator list is [~In]
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
          ast.SpreadProperty(argument:) -> sb_mark_assign_targets(sb, argument)
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

// the only signal never_box_names sees for var/param collisions
fn sb_mark_pattern_assigned(
  sb: scope.ScopeBuilder,
  pattern: ast.Pattern,
) -> scope.ScopeBuilder {
  list.fold(ast.pattern_bound_names(pattern), sb, scope.sb_assign_ref)
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
  p: P,
  is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  let start_token = peek(p)
  // [~In] inside the for head
  use #(p2, expr) <- result.try(with_allow_in(p, False, parse_expression))
  {
    case peek(p2) {
      Semicolon ->
        // for(;;) init is never a pattern: cover errors due
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
            case
              is_bare_pattern
              && p2.ctx.strict
              && pattern_has_eval_args_target(expr)
            {
              True -> Error(EvalArgsAssignStrictMode(pos_of(p2)))
              False -> {
                // clear cover flags so they do not leak into the body
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

// , commits to another declarator: propagate errors
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
          use Nil <- result.try(check_cover_grammar_errors(p3, start))
          use p4 <- result.try(eat_semicolon(p3))
          Ok(#(p4, ast.ReturnStatement(argument: option.Some(expr))))
        }
      }
  }
}

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

fn parse_try_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  let p2 = P(..p2, sb: scope.sb_enter_try(p2.sb))
  use #(p3, block) <- result.try(parse_block_body(p2))
  use #(p4, handler) <- result.try(parse_catch_clause(p3))
  use #(p5, finalizer) <- result.try(case peek(p4) {
    Finally -> {
      use #(p, b) <- result.map(parse_block_body(advance(p4)))
      #(p, option.Some(b))
    }
    _ -> Ok(#(p4, option.None))
  })
  let p5 = P(..p5, sb: scope.sb_leave_try(p5.sb))
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
      let #(sb, catch_id) = scope.sb_push(p3.sb, scope.Catch)
      let p_inner =
        P(
          ..p3,
          sb:,
          ctx: Ctx(
            ..p3.ctx,
            in_block: True,
            binding_kind: BindingParam,
            in_formal_params: True,
            in_catch_param: True,
            param_bound_names: [],
            has_non_simple_param: True,
          ),
        )
      use #(p4, param) <- result.try(parse_binding_pattern(p_inner))
      // §B.3.4: destructured catch param blocks annex b promotion
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
      // catch is not a function boundary; hand param state back
      use p5 <- result.try(expect(
        P(
          ..p4,
          ctx: Ctx(
            ..p4.ctx,
            binding_kind: BindingNone,
            in_formal_params: p3.ctx.in_formal_params,
            in_catch_param: p3.ctx.in_catch_param,
            param_bound_names: p3.ctx.param_bound_names,
            has_non_simple_param: p3.ctx.has_non_simple_param,
          ),
        ),
        RightParen,
      ))
      // §14.15.2: catch param scope is separate from the block
      use #(p6, body) <- result.map(parse_block_body(p5))
      #(
        // flip catch children to source order for finalize
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
  // one block scope around all cases; may shadow params
  let #(sb, switch_id) = scope.sb_push(p6.sb, scope.Block)
  let p_inner =
    P(
      ..p6,
      sb:,
      ctx: Ctx(..p6.ctx, in_block: True, switch_depth: p6.ctx.switch_depth + 1),
    )
  use #(p7, cases) <- result.try(parse_switch_cases(p_inner, False, []))
  // never pruned: emit_switch always enters this scope
  let sb =
    scope.sb_reorder_switch_children(p7.sb, switch_id)
    |> scope.sb_enter(p6.sb.current)
  Ok(#(
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
      // §14.12.4: case tests run before any case body
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

// inner_name_ctx: expressions validate the name with their own flags
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
  use <- bool.guard(
    func_name == "" && name_required,
    Error(ExpectedIdentifier(pos_of(p3))),
  )
  // tag only what emit's collect_hoisted_funcs hoists
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
  let p6 = case func_name {
    "" -> Ok(p5)
    name ->
      register_function_name(p5, name, pos_of(p3), !is_generator && !is_async)
  }
  use p7 <- result.try(p6)
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
  let was_strict = p4.ctx.strict
  use p5 <- result.try(check_use_strict_in_body(p4))
  // §10.2.11 step 28: shims take slots 0..arity-1
  let p5 = P(..p5, sb: declare_param_shims(p5.sb, params))
  // §10.2.11 step 18: implicit arguments, slot order matters
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

// must agree with emit.compile_function_body
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

// §10.2.11; must match emit's non_simple_fixed
fn fixed_params_non_simple(params: List(ast.Pattern)) -> Bool {
  let #(fixed, _rest) = ast_util.split_trailing_rest(params)
  !ast_util.all_simple_params(fixed)
}

fn parse_function_body_block(
  p: P,
) -> Result(#(P, List(ast.StmtWithLine)), ParseError) {
  use p2 <- result.try(expect(p, LeftBrace))
  // snapshot so the reorder only moves body children
  let body_id = p2.sb.current
  let mark = scope.sb_children_raw(p2.sb, body_id)
  use #(p3, stmts) <- result.try(parse_statement_list(p2, False, []))
  // backstop for deferred cover-grammar errors
  use Nil <- result.try(check_cover_grammar_errors(p3, pos_of(p3)))
  use p4 <- result.try(expect(p3, RightBrace))
  // the one chokepoint for bodies without their own block scope
  let p4 = P(..p4, sb: scope.sb_reorder_body_children(p4.sb, body_id, mark))
  Ok(#(p4, stmts))
}

// §10.2.11 step 28 body scope; lockstep with emit, never pruned
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
      // flip fn root children to source order
      let sb = scope.sb_enter(p2.sb, fn_id)
      #(P(..p2, sb: scope.sb_reorder_block_children(sb, fn_id)), body)
    }
  }
}

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
      case strict_binding_violation(name) {
        Some(kind) -> Error(strict_name_error(kind, name, pos_of(p)))
        None ->
          case set.contains(seen, name) {
            True -> Error(DuplicateParamNameStrictMode(pos_of(p), name))
            False -> check_param_names_list(p, rest, set.insert(seen, name))
          }
      }
    }
  }
}

fn mark_non_simple_params(p: P) -> Result(P, ParseError) {
  let p = P(..p, ctx: Ctx(..p.ctx, has_non_simple_param: True))
  use Nil <- result.try(check_param_names_for_dups_only(p))
  Ok(p)
}

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
  p: P,
) -> Result(#(P, List(ast.Pattern), List(ast.StmtWithLine)), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  case peek(p2) {
    RightParen -> {
      let p3 = advance(p2)
      use p3 <- result.try(check_use_strict_in_body(p3))
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
      let param_name = get_simple_binding_name(p2)
      let p2 = case param_name == "" {
        True -> P(..p2, ctx: Ctx(..p2.ctx, has_non_simple_param: True))
        False -> p2
      }
      use #(p3, pat) <- result.try(parse_binding_pattern(p2))
      let p3 = case peek(p3) {
        Equal -> P(..p3, ctx: Ctx(..p3.ctx, has_non_simple_param: True))
        _ -> p3
      }
      let default_pos = pos_of(p3)
      use #(p4, final_pat) <- result.try(parse_pattern_default(p3, pat))
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
          use p5 <- result.try(check_use_strict_in_body(p5))
          // §10.2.11 steps 28 and 18, as parse_function_params_and_body
          let p5 = P(..p5, sb: declare_param_shims(p5.sb, [final_pat]))
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
      // parameter defaults owe their cover-grammar errors now
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
      use p <- result.try(mark_non_simple_params(p))
      let p2 = advance(p)
      let param_name = get_simple_binding_name(p2)
      use Nil <- result.try(check_duplicate_param(p2, param_name, seen))
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
      let param_name = get_simple_binding_name(p)
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

fn parse_formal_param_default(
  p: P,
  seen: Set(String),
  pat: ast.Pattern,
  acc: List(ast.Pattern),
) -> Result(#(P, List(ast.Pattern)), ParseError) {
  use #(p2, final_pat) <- result.try(parse_pattern_default(p, pat))
  parse_formal_param_rest(p2, seen, [final_pat, ..acc])
}

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

fn optional_named_binding(
  name: String,
  span: ast.Span,
) -> Option(ast.NamedBinding) {
  case name {
    "" -> None
    n -> Some(ast.NamedBinding(name: n, span:))
  }
}

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

// annex b §B.3.4
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

fn parse_class_decl_impl(p: P) -> Result(#(P, ast.Declaration), ParseError) {
  use #(p2, name, super_class, body) <- result.map(parse_class_head_and_tail(
    p,
    True,
    True,
  ))
  #(p2, ast.DeclClass(name:, super_class:, body:))
}

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

// matches scope.declare_class fold_class_body order
type ClassScopeCtx {
  ClassScopeCtx(
    class_id: scope.ScopeId,
    init_id: scope.ScopeId,
    static_id: scope.ScopeId,
  )
}

// key_scopes: scopes pushed while parsing a computed key
type ClassElementScopes {
  MethodScopes(key_scopes: List(scope.ScopeId), method_fn_id: scope.ScopeId)
  NonMethodScopes(key_scopes: List(scope.ScopeId))
}

const no_element_scopes = NonMethodScopes(key_scopes: [])

// children_at is newest-first, so new ids are the prefix
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
  let saved_strict = p.ctx.strict
  let outer_current = p.sb.current
  // §15.7.14: class scope is pushed before the heritage
  let #(sb, class_id) = scope.sb_push(p.sb, scope.ClassBody)
  let p = P(..p, sb:, ctx: Ctx(..p.ctx, strict: True))
  // once extends is consumed errors propagate, no backtrack
  let has_extends = peek(p) == Extends
  use #(p2, super_class) <- result.try(case has_extends {
    True -> {
      use #(p2, expr) <- result.map(parse_left_hand_side_expression(advance(p)))
      #(p2, Some(expr))
    }
    False -> Ok(#(p, None))
  })
  let heritage_scopes = scope.sb_children_raw(p2.sb, class_id) |> list.reverse
  use p3 <- result.try(expect(p2, LeftBrace))
  // pre-create init shells; unneeded ones dropped at }
  let #(sb, init_id) = scope.sb_push(p3.sb, scope.Function)
  let sb = scope.sb_enter(sb, class_id)
  let #(sb, static_id) = scope.sb_push(sb, scope.Function)
  let sb = scope.sb_enter(sb, class_id)
  let ctx = ClassScopeCtx(class_id:, init_id:, static_id:)
  // §15.7.14: heritage uses the outer private depth
  let outer_depth = p3.class_private_depth
  let p3 = P(..p3, sb:, class_private_depth: outer_depth + 1)
  use #(p4, rev_tagged, declared) <- result.try(
    parse_class_body(p3, ctx, has_extends, False, dict.new(), []),
  )
  use p4 <- result.try(resolve_private_refs(p4, outer_depth, declared))
  let tagged = list.reverse(rev_tagged)
  let elements = list.map(tagged, fn(pair) { pair.0 })
  // child order must match declare_class for emit's cursor
  let sb =
    class_scope_finalize(p4.sb, ctx, name, has_extends, heritage_scopes, tagged)
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

// 7-step child order of scope.declare_class; emit reads it positionally
fn class_scope_finalize(
  sb: scope.ScopeBuilder,
  ctx: ClassScopeCtx,
  name: Option(String),
  has_super_class: Bool,
  heritage_scopes: List(scope.ScopeId),
  tagged: List(#(ast.ClassElement, ClassElementScopes)),
) -> scope.ScopeBuilder {
  let elements = list.map(tagged, fn(pair) { pair.0 })
  // same slot order the emitter looks up
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
  // (4) computed keys, source order
  let key_scopes = list.flat_map(tagged, fn(pair) { { pair.1 }.key_scopes })
  // (5)(6)(2) buckets must match ast_util.classify_class_body
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
            // unreachable in practice
            ast_util.CeInstanceField | ast_util.CeStaticElement -> acc
          }
      }
    })
  let instance_methods = list.reverse(instance_methods)
  let static_methods = list.reverse(static_methods)
  // (1) instance init needed (§7.3.29)
  let needs_instance_init =
    list.any(elements, fn(el) {
      case el {
        ast.ClassMethod(key: ast.KeyPrivate(..), is_static: False, ..) -> True
        _ -> ast_util.is_instance_field(el)
      }
    })
  // (7) static init needed
  let needs_static_init = list.any(elements, ast_util.is_static_element)
  // drop unneeded shells, flip kept ones, seed synthetic refs
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
  // (2) constructor: always one function child
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

// ref to the field-key stash const emit reads
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

// seed synthetic refs emit's compile_class_init_fn reads
fn class_seed_field_shell(
  sb: scope.ScopeBuilder,
  shell_id: scope.ScopeId,
  tagged: List(#(ast.ClassElement, ClassElementScopes)),
  is_static: Bool,
) -> scope.ScopeBuilder {
  let sb = scope.sb_enter(sb, shell_id)
  // §10.2.11 step 22
  let sb =
    scope.sb_declare_in(
      sb,
      shell_id,
      "arguments",
      scope.VarBinding,
      synthetic: True,
    )
  let sb = scope.sb_lexical_ref(sb, lexical.RefThis)
  // §7.3.29 private methods read #x and its stash
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
  list.index_fold(tagged, sb, fn(sb, pair, idx) {
    case pair.0 {
      ast.ClassField(key:, is_static: s, ..) if s == is_static ->
        class_ref_field_key(sb, key, idx)
      _ -> sb
    }
  })
}

// synthetic refs the emitter adds to the constructor
fn class_seed_ctor_shell(
  sb: scope.ScopeBuilder,
  ctor_id: scope.ScopeId,
  needs_instance_init: Bool,
  is_synthetic: Bool,
  has_super_class: Bool,
) -> scope.ScopeBuilder {
  let sb =
    scope.sb_enter(sb, ctor_id)
    |> scope.sb_update_current_fn(fn(fi) {
      scope.RawFunctionInfo(..fi, is_derived_constructor: has_super_class)
    })
  // synthetic ctor never declared arguments
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
  let sb = case needs_instance_init {
    True ->
      sb
      |> scope.sb_ref(ast_util.class_fields_init)
      |> scope.sb_lexical_ref(lexical.RefThis)
    False -> sb
  }
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

// §15.7.1: only a getter+setter pair may share a private name
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
  let is_static = case peek(p) {
    Static ->
      case peek_at(p, 1) {
        // static is a name when followed by ( = ; }
        LeftParen | Equal | Semicolon | RightBrace -> False
        _ -> True
      }
    _ -> False
  }
  let p2 = case is_static {
    True -> advance(p)
    False -> p
  }
  // §15.7.1 static block, not a method named static
  use <- bool.lazy_guard(is_static && peek(p2) == LeftBrace, fn() {
    // step (7): static block is an arrow child of the static shell
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
    // not parse_block_body: no block between arrow scope and body
    use #(p3, block) <- result.map(parse_function_body_block(p_body))
    // re-enter class_id so the next element parents correctly
    let p3 =
      P(
        ..restore_outer_context(p3, p2_static),
        sb: scope.sb_enter(p3.sb, ctx.class_id),
      )
    #(p3, False, ast.StaticBlock(body: block), no_element_scopes)
  })
  // get * is a field named get plus an asi generator
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
  // snapshot to diff out computed-key scopes
  let key_before = scope.sb_children_raw(p5.sb, ctx.class_id)
  use #(p6, key) <- result.try(parse_property_name(p5))
  let key_scopes = class_new_children(p6.sb, ctx.class_id, key_before)
  // §15.7.1 checks use the decoded key
  let static_name = ast.property_key_static_name(key)
  // §15.7.1: static prototype is forbidden
  use <- bool.guard(
    is_static && static_name == Some("prototype"),
    Error(StaticPrototype(pos_of(p5))),
  )
  // §15.7.1: constructor must be a plain method, once
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
  // §15.7.1: #constructor is forbidden
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
      let method_kind = case is_constructor {
        True -> ast.MethodConstructor
        False ->
          case class_accessor_kind {
            GetPrefix -> ast.MethodGet
            SetPrefix -> ast.MethodSet
            NoAccessor -> ast.MethodMethod
          }
      }
      // method scope is a direct child of class_id; capture by diff
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
      // never empty: a method pushed exactly one function scope
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
      // §15.7.1: field named constructor is forbidden
      use <- bool.guard(
        is_constructor_name,
        Error(FieldNamedConstructor(pos_of(p6))),
      )
      // initializer scopes parent under the init shell, not the class body
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
          // field initializer is [+In]
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
  check_reserved_identifier_common(p, label)
}

fn parse_labeled_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  parse_label_chain(p, [])
}

fn async_function_start(p: P) -> Bool {
  peek_at(p, 1) == Function && token_line_at(p, 1) == token_line_at(p, 0)
}

fn at_label_start(p: P) -> Bool {
  case peek(p) {
    Identifier | Async | Yield | Await -> peek_at(p, 1) == Colon
    _ -> False
  }
}

// continue may target any label of a loop's chain (§14.13.1)
fn parse_label_chain(
  p: P,
  collected: List(String),
) -> Result(#(P, ast.Statement), ParseError) {
  let label = peek_value(p)
  use Nil <- result.try(check_label_identifier(p, label))
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

fn parse_labeled_statement_body(
  p3: P,
  labels: List(String),
) -> Result(#(P, ast.Statement), ParseError) {
  {
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
    case peek(p3) {
      Const -> Error(LexicalDeclInLabel(pos_of(p3)))
      // same let lookahead as single-statement position
      Let -> {
        let next = peek_at(p3, 1)
        let decl_starter =
          next == LeftBrace
          || next == LeftBracket
          || is_identifier_or_keyword(next)
        let newline_after_let = token_line_at(p3, 1) > token_line_at(p3, 0)
        case next == LeftBracket || { decl_starter && !newline_after_let } {
          True -> Error(LexicalDeclInLabel(pos_of(p3)))
          False ->
            case decl_starter {
              True -> wrap_label(parse_expression_statement(p3))
              False -> wrap_label(parse_statement(p3))
            }
        }
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
      Class -> Error(LexicalDeclInLabel(pos_of(p3)))
      // only a plain function may be a labelled item
      Async ->
        case async_function_start(p3) {
          True -> Error(FunctionDeclInLabelBody(pos_of(p3)))
          False -> wrap_label(parse_statement(p3))
        }
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
  let #(sb, with_id) = scope.sb_push_with(p5.sb)
  use #(p6, body) <- result.try(parse_single_statement(P(..p5, sb:), False))
  // flip children to source order for finalize
  let sb = scope.sb_reorder_block_children(p6.sb, with_id)
  Ok(#(
    P(..p6, sb: scope.sb_enter(sb, p5.sb.current)),
    ast.WithStatement(object:, body:),
  ))
}

// raise deferred cover-grammar errors once known not a pattern
fn check_cover_grammar_errors(p: P, pos: Int) -> Result(Nil, ParseError) {
  case p.ctx.has_cover_initializer, p.ctx.dup_proto_pos {
    True, _ -> Error(ShorthandDefaultOutsideDestructuring(pos))
    False, Some(dup_pos) -> Error(DuplicateProtoProperty(dup_pos))
    False, None -> Ok(Nil)
  }
}

fn parse_expression_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  // raw text needed for directives
  let directive_raw = case peek(p) {
    KString -> option.Some(peek_value(p))
    _ -> option.None
  }
  use #(p2, expr) <- result.try(parse_expression(p))
  use Nil <- result.try(check_cover_grammar_errors(p2, pos_of(p)))
  use p3 <- result.try(eat_semicolon(p2))
  // directive only if exactly a string literal
  let directive = case expr {
    ast.StringExpression(..) -> directive_raw
    _ -> option.None
  }
  Ok(#(p3, ast.ExpressionStatement(expression: expr, directive:)))
}

fn parse_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  use #(p2, first_expr) <- result.try(parse_assignment_expression(p))
  case peek(p2) {
    Comma ->
      case peek_at(p2, 1) {
        RightParen | RightBracket | RightBrace | Eof -> Ok(#(p2, first_expr))
        _ -> {
          let p3 = advance(p2)
          use #(p4, rest_expr) <- result.try(parse_expression(p3))
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
      case p.ctx.in_generator {
        False -> parse_assignment_expression_inner(p)
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
            | Arrow -> parse_assignment_expression_inner(p)
            // no LeftBracket: yield [..] is a yield expression
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
  case try_arrow_function(p) {
    Ok(#(p2, arrow_expr)) -> Ok(#(p2, arrow_expr))
    // error after => is committed, never backtrack
    Error(ArrowError(e)) -> Error(e)
    Error(NotAnArrow) -> {
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
  let lhs_start = peek(p)
  use #(p2, lhs_expr) <- result.try(parse_conditional_expression(p))
  case peek(p2) {
    Equal ->
      case p2.last_expr_assignable {
        True -> {
          use Nil <- result.try(check_strict_restricted_target(p2, lhs_expr))
          // not assignable but covers target = default
          finish_assignment(p2, lhs_expr, ast.Assign, Some(True))
        }
        False ->
          case lhs_start {
            LeftBrace | LeftBracket ->
              case p2.has_invalid_pattern {
                True -> Error(InvalidDestructuringTarget(pos_of(p2)))
                False ->
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
          // annex b call targets ok for op= but not logical assignment
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
        // clear so it does not leak from a sibling
        None ->
          case p2.last_expr_is_assignment {
            True -> Ok(#(P(..p2, last_expr_is_assignment: False), lhs_expr))
            False -> Ok(#(p2, lhs_expr))
          }
      }
  }
}

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

// §13.15.1, including (eval) = 1
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

// annex b web-compat call target; never optional chains (§13.3.1.1)
fn is_web_compat_call_target(p: P, lhs: ast.Expression) -> Bool {
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

fn try_arrow_function(p: P) -> Result(#(P, ast.Expression), ArrowAttempt) {
  case peek(p) {
    // §15.9: no line terminator after async
    Async -> {
      let same_line = token_line_at(p, 1) == token_line_at(p, 0)
      case peek_at(p, 1) {
        LeftParen if same_line ->
          case paren_arrow_ahead(look_skip(look_skip(look_from(p)))) {
            True -> try_paren_arrow(p, advance(advance(p)), True)
            False -> Error(NotAnArrow)
          }
        Arrow -> try_single_ident_arrow(p, p, False)
        next if same_line ->
          case is_arrow_param_name(next), peek_at(p, 2) {
            True, Arrow -> try_single_ident_arrow(p, advance(p), True)
            _, _ -> Error(NotAnArrow)
          }
        _ -> Error(NotAnArrow)
      }
    }
    Identifier | Yield | Await | Of | From | As | Let | Static ->
      case peek_at(p, 1) {
        Arrow -> try_single_ident_arrow(p, p, False)
        _ -> Error(NotAnArrow)
      }
    LeftParen ->
      case paren_arrow_ahead(look_skip(look_from(p))) {
        True -> try_paren_arrow(p, advance(p), False)
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
  outer: P,
  ident_p: P,
  is_async: Bool,
) -> Result(#(P, ast.Expression), ArrowAttempt) {
  let name = peek_value(ident_p)
  // async arrow params are [+Await]
  let check_p = case is_async {
    True -> P(..outer, ctx: Ctx(..outer.ctx, in_async: True))
    False -> outer
  }
  // => not consumed yet, so not-a-binding means not an arrow
  use Nil <- result.try(
    check_binding_identifier(check_p, name)
    |> result.replace_error(NotAnArrow),
  )
  let p2 = advance(ident_p)
  case has_line_break_before(p2) {
    True -> Error(NotAnArrow)
    False -> {
      let p3 = enter_arrow_context(advance(p2), is_async, [name])
      // set after entering context, which zeroes param state
      let p3 = P(..p3, ctx: Ctx(..p3.ctx, param_bound_names: [name]))
      let params = [ast.IdentifierPattern(name: name, span: span_of(ident_p))]
      finish_arrow(parse_arrow_body(p3, params), outer, is_async, params)
    }
  }
}

fn try_paren_arrow(
  outer: P,
  p_params: P,
  is_async: Bool,
) -> Result(#(P, ast.Expression), ArrowAttempt) {
  // push arrow scope before params; failure discards it for free
  let p_ctx = enter_arrow_context(p_params, is_async, [])
  // arrow params always reject duplicates
  let p_arrow =
    P(
      ..p_ctx,
      ctx: Ctx(
        ..p_ctx.ctx,
        in_arrow_params: True,
        in_formal_params: True,
        binding_kind: BindingParam,
        // §15.3: arrow params use the enclosing yield/await context
        in_generator: p_params.ctx.in_generator,
        in_async: p_params.ctx.in_async || is_async,
        in_static_block: p_params.ctx.in_static_block,
        in_class_field_init: p_params.ctx.in_class_field_init,
        // param defaults are always [+In]
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
              case has_line_break_before(p4) {
                True -> Error(NotAnArrow)
                False -> {
                  // switch the borrowed flags back to the arrow body values
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
            _ -> Error(NotAnArrow)
          }
        Error(_speculative_error) -> Error(NotAnArrow)
      }
    Error(_speculative_error) -> Error(NotAnArrow)
  }
}

fn finish_arrow(
  body_result: Result(#(P, ast.ArrowBody), ParseError),
  outer: P,
  is_async: Bool,
  params: List(ast.Pattern),
) -> Result(#(P, ast.Expression), ArrowAttempt) {
  use #(p_body, body) <- result.try(result.map_error(body_result, ArrowError))
  let body_end = p_body.prev_end
  let p_restored = restore_outer_context(p_body, outer)
  // §13.15.1: (x => x) = 1 is invalid
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
  case peek(p) {
    LeftBrace -> {
      // block body is [+In]; expression body inherits
      let p = P(..p, ctx: Ctx(..p.ctx, allow_in: True))
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
      // raise deferred cover errors before the context is dropped
      use Nil <- result.try(check_cover_grammar_errors(p2, start))
      // flip arrow children to source order for finalize
      let sb = scope.sb_reorder_block_children(p2.sb, p.sb.current)
      Ok(#(P(..p2, sb:), ast.ArrowBodyExpression(expr)))
    }
  }
}

fn parse_yield_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  case p.ctx.in_formal_params && !p.ctx.in_catch_param {
    True -> Error(YieldInFormalParameter(pos_of(p)))
    False -> parse_yield_expression_inner(p)
  }
}

fn parse_yield_expression_inner(
  p: P,
) -> Result(#(P, ast.Expression), ParseError) {
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
      yield_with_arg(start, True, parse_assignment_expression(advance(p2)))
    // slash after yield starts a regex
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
      // §13.14: middle operand is always [+In]
      use #(p4, consequent) <- result.try(with_allow_in(
        p3,
        True,
        parse_assignment_expression,
      ))
      use p5 <- result.try(expect(p4, Colon))
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
  // §13.10: bare #x is only valid left of in
  let bare_private = case left {
    ast.Identifier(name: "#" <> _, ..) -> True
    _ -> False
  }
  case binary_operator(tok, p.ctx.allow_in) {
    None ->
      case bare_private {
        True -> Error(PrivateNameNotInBrandCheck(pos_of(p)))
        False -> Ok(#(p, left))
      }
    Some(BinaryOperator(precedence:, op:)) ->
      case precedence > min_prec {
        False ->
          case bare_private {
            True -> Error(PrivateNameNotInBrandCheck(pos_of(p)))
            False -> Ok(#(p, left))
          }
        True -> {
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
          let span = ast.Span(ast.expression_span(left).start, p3.prev_end)
          // split so the §13.13.1 check cannot be dropped
          use expr <- result.try(case op {
            Binary(op) ->
              Ok(ast.BinaryExpression(operator: op, left:, right:, span:))
            ShortCircuit(op) ->
              // §13.13.1: no unparenthesized ?? mixed with || &&
              case left {
                ast.LogicalExpression(operator: ast.NullishCoalescing, ..) ->
                  Error(CoalesceMixedWithLogical(op_pos))
                _ ->
                  Ok(ast.LogicalExpression(operator: op, left:, right:, span:))
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
          })
          parse_binary_rhs(P(..p3, last_expr_assignable: False), expr, min_prec)
        }
      }
  }
}

fn is_bare_private_name(expr: ast.Expression) -> Bool {
  case expr {
    ast.Identifier(name: "#" <> _, ..) -> True
    _ -> False
  }
}

fn is_unary_operand(expr: ast.Expression) -> Bool {
  case expr {
    ast.UnaryExpression(..) | ast.AwaitExpression(..) -> True
    _ -> False
  }
}

fn parse_unary_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let start = pos_of(p)
  let unary = fn(p2, op) {
    use #(p3, arg) <- result.try(parse_unary_expression(p2))
    use <- bool.guard(
      is_bare_private_name(arg),
      Error(PrivateNameNotInBrandCheck(pos_of(p2))),
    )
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
      // §13.5.1.1 delete early errors, through parens
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
          // §15.7.1: no await in a static block
          use <- bool.guard(
            p.ctx.in_static_block,
            Error(AwaitInStaticBlock(start)),
          )
          // §15.8.1: no await in formal parameters
          use <- bool.guard(
            p.ctx.in_formal_params && !p.ctx.in_catch_param,
            Error(AwaitInFormalParameter(start)),
          )
          let p2 = advance(p)
          use #(p3, arg) <- result.try(parse_unary_expression(p2))
          Ok(#(
            P(..p3, last_expr_assignable: False, last_expr_is_assignment: False),
            ast.AwaitExpression(argument: arg, span: span_from(start, p3)),
          ))
        }
        // await is an identifier outside async and modules
        False -> parse_postfix_expression(p)
      }
    _ -> parse_postfix_expression(p)
  }
}

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

fn is_private_name_access(expr: ast.Expression) -> Bool {
  case expr {
    ast.MemberExpression(property: ast.Dot(name:, ..), ..)
    | ast.OptionalMemberExpression(property: ast.Dot(name:, ..), ..) ->
      string.starts_with(name, "#")
    _ -> False
  }
}

// §15.7.1 AllPrivateIdentifiersValid ref recording
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

// super.#x is always a syntax error (§13.3)
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

// { #x: 1 } is a syntax error
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

// end of class body step of AllPrivateIdentifiersValid
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
    // only direct eval's private environment can still legitimize these
    0 ->
      case unresolved_outside_eval_env(p, remaining) {
        [#(name, _, pos), ..] -> Error(UndeclaredPrivateName(pos, name))
        [] -> Ok(P(..p, private_refs: []))
      }
    _ -> Ok(P(..p, private_refs: remaining))
  }
}

fn check_unresolved_private_refs(p: P) -> Result(Nil, ParseError) {
  case unresolved_outside_eval_env(p, p.private_refs) {
    [] -> Ok(Nil)
    [#(name, _, pos), ..] -> Error(UndeclaredPrivateName(pos, name))
  }
}

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

// advance preserves last_expr_*, so postfix callers advance first
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
      let p3 = advance(p2)
      case peek(p3) {
        Identifier -> {
          case peek_value(p3), peek_raw_len(p3) != 6 {
            // no unicode escapes in new.target
            "target", True -> Error(UnicodeEscapeInMetaProperty(pos_of(p3)))
            "target", False ->
              case p.ctx.allow_new_target {
                True -> {
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
              // record <class_fields_init> ref so arrows in derived ctors capture it
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
          // import() arguments are [+In]
          use p3 <- with_allow_in(advance(p2), True)
          use #(p4, source_expr) <- result.try(parse_assignment_expression(p3))
          use #(p5, options) <- result.try(case peek(p4) {
            Comma ->
              case peek_at(p4, 1) {
                RightParen -> Ok(#(advance(p4), None))
                _ -> {
                  use #(p_attrs, attrs) <- result.map(
                    parse_assignment_expression(advance(p4)),
                  )
                  // import(x, opts,) is allowed
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
          let p3 = advance(p2)
          // meta/source/defer must be unescaped (§5.1.5)
          case peek(p3), peek_value(p3), peek_had_escape(p3) {
            Identifier, "meta", False -> {
              use <- bool.guard(
                p.mode == Script,
                Error(ImportMetaOutsideModule(pos_of(p))),
              )
              let p4 = advance(p3)
              Ok(#(
                p4,
                ast.MetaProperty(
                  kind: ast.ImportMeta,
                  span: span_from(pos_of(p), p4),
                ),
              ))
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

fn at_suffix_start(p: P) -> Bool {
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
  p: P,
  import_start: Int,
  phase: ast.ImportPhase,
) -> Result(#(P, ast.Expression), ParseError) {
  use p2 <- with_allow_in(advance(advance(p)), True)
  use #(p3, source_expr) <- result.try(parse_assignment_expression(p2))
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
  let start = callee.span.start
  case peek(p) {
    LeftParen -> {
      use #(p2, args) <- result.try(parse_arguments(p))
      // §19.2.1 direct eval poisons the scope
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
      // optional chain is never assignable
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
      // §13.3.1.1: no tagged template in an optional chain
      use <- bool.guard(
        in_optional_chain(callee),
        Error(TemplateInOptionalChain(pos_of(p))),
      )
      use #(p2, expr) <- result.try(parse_tagged_template(p, callee))
      parse_call_chain(p2, expr)
    }
    _ -> Ok(#(p, callee))
  }
}

fn in_optional_chain(expr: ast.Expression) -> Bool {
  case expr {
    ast.OptionalMemberExpression(..) | ast.OptionalCallExpression(..) -> True
    ast.MemberExpression(object:, ..) -> in_optional_chain(object)
    ast.CallExpression(callee:, ..) -> in_optional_chain(callee)
    ast.TaggedTemplateExpression(tag:, ..) -> in_optional_chain(tag)
    _ -> False
  }
}

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
    // unreachable
    _ -> Ok(#(p, object))
  }
}

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

fn finish_dot_member(
  p: P,
  object: ast.Expression,
  start: Int,
  prop_name: String,
  optional: Bool,
) -> #(P, ast.Expression) {
  let p = note_private_ref(p, prop_name)
  // obj.#x is a ref to the class-scope #x const
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

fn parse_tagged_template(
  p: P,
  tag: ast.Expression,
) -> Result(#(P, ast.Expression), ParseError) {
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
  #(P(..p2, last_expr_assignable: False), expr)
}

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
  // no ?. here: new a?.b must fail downstream (§13.3)
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
  // arguments are [+In]
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
  case peek(p) {
    Identifier -> {
      let val = peek_value(p)
      // §13.1.1 identifier reference early errors
      use Nil <- result.try(check_identifier_reference(p, val))
      // bare #x only reaches here as #x in obj
      case val {
        "#" <> _ ->
          case peek_at(p, 1) {
            In -> identifier_reference(note_private_ref(p, val), val)
            _ -> Error(PrivateNameNotInBrandCheck(pos_of(p)))
          }
        _ -> identifier_reference(p, val)
      }
    }
    _ -> parse_primary_non_identifier(P(..p, last_expr_name: None))
  }
}

fn identifier_reference(
  p: P,
  name: String,
) -> Result(#(P, ast.Expression), ParseError) {
  Ok(#(
    advance(
      P(
        ..p,
        sb: scope.sb_ref(p.sb, name),
        last_expr_assignable: True,
        last_expr_name: Some(name),
      ),
    ),
    ast.Identifier(name:, span: span_of(p)),
  ))
}

fn parse_primary_non_identifier(
  p: P,
) -> Result(#(P, ast.Expression), ParseError) {
  case peek(p) {
    // hard lexer error token: report its message
    Illegal | LexFailure(_) -> Error(illegal_token_error(p))
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
      let next = peek_at(p, 1)
      case next {
        Dot | LeftBracket ->
          case p.ctx.allow_super_property {
            True -> {
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
          // () without => is an error
          Error(UnexpectedCloseParen(pos_of(p)))
        }
        _ -> {
          // parenthesized expression is [+In]
          use p2 <- with_allow_in(p2, True)
          use #(p3, expr) <- result.try(parse_expression(p2))
          use p4 <- result.map(expect(p3, RightParen))
          // keep parens for IsIdentifierRef (§13.15.2)
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
      case async_function_start(p) {
        True -> parse_function_expression(p, is_async: True)
        False -> contextual_ident_ok(p)
      }
    Slash -> {
      // slash here starts a regex: relex it
      parse_regex_literal(p)
    }
    SlashEqual -> {
      // /= starts a regex beginning with =
      parse_regex_literal(p)
    }
    New -> parse_new_expression(p)
    _ ->
      case is_contextual_keyword(peek(p)) {
        True ->
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
  // elements are [+In]
  use p2 <- with_allow_in(p2, True)
  // pattern flags are per literal: reset
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
      // rest cannot take a default
      let elem_invalid = cover_elem_invalid(p3, spread_start, False)
      let p3 = P(..p3, has_invalid_pattern: saved_invalid || elem_invalid)
      let elem =
        Some(ast.SpreadElement(argument: expr, span: span_from(spread_pos, p3)))
      case peek(p3) {
        Comma -> {
          // spread not last: fine as expression, invalid as pattern
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
  // values are [+In]
  use p2 <- with_allow_in(p2, True)
  // pattern flags are per literal: reset
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
      // §13.15.5: object rest must be a simple target
      let saved_invalid = p.has_invalid_pattern
      let p2 = advance(p)
      use #(p3, expr) <- result.try(parse_assignment_expression(p2))
      let p3 =
        P(..p3, has_invalid_pattern: saved_invalid || !p3.last_expr_assignable)
      let prop = ast.SpreadProperty(argument: expr)
      case peek(p3) {
        // anything after spread invalidates the pattern
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
      // §13.2.5.1 duplicate __proto__, deferred for patterns
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

// star_ends_accessor: in classes get * is a field named get
// escaped get/set is never the keyword (§12.7.2)
fn parse_method_prefix(
  p: P,
  is_terminator: fn(TokenKind) -> Bool,
  star_ends_accessor: Bool,
) -> #(P, Bool, AccessorPrefix, Bool) {
  let is_async = case peek(p) {
    Async ->
      !is_terminator(peek_at(p, 1))
      && token_line_at(p, 1) == token_line_at(p, 0)
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
  // keyword is a name when followed by ( , } :
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
  let prop_name_kind = peek(p4)
  let prop_name_value = peek_value(p4)
  let is_valid_shorthand = case prop_name_kind {
    Identifier -> True
    _ -> is_contextual_keyword(prop_name_kind)
  }
  use #(p5, key) <- result.try(parse_property_name(p4))
  use Nil <- result.try(reject_private_property_key(p4, key))
  // *name must be a method
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
      // simple target shadows inner invalid-pattern flags
      let saved_invalid = p5.has_invalid_pattern
      let p6 = advance(p5)
      let value_start = peek(p6)
      use #(p7, expr) <- result.try(parse_assignment_expression(p6))
      let elem_invalid = cover_elem_invalid(p7, value_start, True)
      let p7 = P(..p7, has_invalid_pattern: saved_invalid || elem_invalid)
      Ok(#(p7, ast.InitProperty(key:, value: expr, shorthand: False)))
    }
    tok -> {
      let modified = has_async || accessor_kind != NoAccessor
      case is_valid_shorthand && !modified {
        False -> Error(UnexpectedToken(pos_of(p5), prop_name_kind))
        True -> {
          // shorthand is an identifier reference (§13.1.1)
          use Nil <- result.try(check_identifier_reference(p5, prop_name_value))
          let p5 = P(..p5, sb: scope.sb_ref(p5.sb, prop_name_value))
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
  let start = pos_of(p)
  use #(p4, p3, is_generator, func_name) <- result.try(parse_function_head(
    p,
    is_async,
    True,
  ))
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
  // §15.2.6 nfe name: own scope, declared after body, first wins
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
  // relex from source as a regex literal
  let start_pos = pos_of(p)
  let body_start = start_pos + 1
  case regex.scan_regex_source(p.bytes, body_start) {
    Ok(end_pos) -> {
      use #(flags_end, flags) <- result.try(
        regex.skip_regex_flags(p.bytes, end_pos)
        |> result.map_error(regexp_syntax_error),
      )
      // pattern early errors; annex b grammar unless u/v
      use Nil <- result.try(
        regex.validate_pattern(p.bytes, body_start, end_pos - 1, flags)
        |> result.map_error(regexp_syntax_error),
      )
      let assert Some(pattern) =
        source_bytes.slice(p.bytes, body_start, end_pos - 1 - body_start)
        as "parser: regex body slice out of range"
      let flags_str = string.join(flags.flags, "")
      // window past / is garbage; relex after the flags
      let p2 = jump_to(p, flags_end)
      let span = ast.Span(start: start_pos, end: flags_end)
      Ok(#(p2, ast.RegExpLiteral(pattern: pattern, flags: flags_str, span:)))
    }
    Error(e) -> Error(regexp_syntax_error(e))
  }
}

// from is on the current line; rescanned constructs never span lines
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

// no import attributes supported: only an empty with {} parses
fn skip_import_attributes(p: P) -> Result(P, ParseError) {
  case peek(p) {
    With -> {
      use p2 <- result.try(expect(advance(p), LeftBrace))
      expect(p2, RightBrace)
    }
    _ -> Ok(p)
  }
}

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
    KString -> {
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
    other_kind -> {
      // default binding may be a contextual keyword like from
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

// source phase import; binding not modeled in the ast yet
fn parse_source_phase_import(
  p: P,
  span_start: Int,
) -> Result(#(P, ast.ModuleItem), ParseError) {
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

fn is_specifier_name(kind: TokenKind) -> Bool {
  kind == Identifier || kind == KString || is_keyword_as_identifier(kind)
}

fn parse_import_specifier(
  p: P,
) -> Result(#(P, ast.ImportSpecifier), ParseError) {
  case is_specifier_name(peek(p)) {
    False -> Error(ExpectedImportSpecifierName(pos_of(p)))
    True -> {
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

fn parse_export_named_function(
  p: P,
  is_async: Bool,
) -> Result(#(P, ast.Declaration), ParseError) {
  let name_offset = case is_async {
    True -> 2
    False -> 1
  }
  let name_offset = case peek_at(p, name_offset) == Star {
    True -> name_offset + 1
    False -> name_offset
  }
  let export_name = peek_value_at(p, name_offset)
  let checked = case export_name != "" {
    True -> check_duplicate_export(p, export_name)
    False -> Ok(p)
  }
  use p2 <- result.try(checked)
  use #(p3, function) <- result.map(parse_function_decl_impl(p2, True, is_async))
  #(p3, ast.DeclFunction(function:))
}

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

fn parse_export_named_class(p: P) -> Result(#(P, ast.Declaration), ParseError) {
  let export_name = peek_value_at(p, 1)
  case export_name != "" {
    True -> {
      use p2 <- result.try(check_duplicate_export(p, export_name))
      parse_class_decl_impl(p2)
    }
    False -> parse_class_decl_impl(p)
  }
}

fn parse_default_fn(
  p: P,
  is_async: Bool,
) -> Result(#(P, DefaultExportDecl), ParseError) {
  use #(p2, function) <- result.map(parse_function_decl_impl(p, False, is_async))
  #(p2, DefaultFn(function:))
}

fn parse_default_class(p: P) -> Result(#(P, DefaultExportDecl), ParseError) {
  use #(p2, name, super_class, body) <- result.map(parse_class_head_and_tail(
    p,
    False,
    True,
  ))
  #(p2, DefaultClass(name:, super_class:, body:))
}

fn finish_export_default_decl(
  p_export: P,
  p_decl: P,
  parse: fn(P) -> Result(#(P, DefaultExportDecl), ParseError),
) -> Result(#(P, ast.ModuleItem), ParseError) {
  let decl_start = pos_of(p_decl)
  use #(p4, decl) <- result.map(parse(p_decl))
  let decl_span = span_from(decl_start, p4)
  // §16.2.3.7: anonymous default declares *default*; VarBinding per emit
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

fn finish_export_default_expr(
  p_export: P,
  p_expr: P,
) -> Result(#(P, ast.ModuleItem), ParseError) {
  use #(p4, expr) <- result.try(parse_assignment_expression(p_expr))
  use p5 <- result.map(eat_semicolon(p4))
  // §16.2.3.7 *default* binding, VarBinding per emit
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

// p is at the module specifier string
fn finish_export_all(
  p: P,
  span_start: Int,
  exported: Option(String),
) -> Result(#(P, ast.ModuleItem), ParseError) {
  case peek(p) {
    KString -> {
      use value <- result.try(module_specifier_value(p))
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
      let span_start = pos_of(p)
      let p3 = advance(p2)
      case peek(p3) {
        As -> {
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
      let p3 = advance(p2)
      let saved_local_refs = p3.export_local_refs
      use #(p4, specifiers) <- result.try(parse_export_specifiers(p3))
      case peek(p4) {
        From -> {
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
      let local_span = span_of(p)
      let local_pos = pos_of(p)
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
      let p5 =
        P(..p4, export_local_refs: [#(local, local_pos), ..p4.export_local_refs])
      Ok(#(advance(p5), ast.ExportSpecifier(local:, exported:, local_span:)))
    }
  }
}

fn check_use_strict_in_body(p: P) -> Result(P, ParseError) {
  case p.ctx.strict {
    True ->
      // use strict + non-simple params errors even if already strict
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

// pure lookahead; never advances the real scan (rescan invariant)
type Look {
  Look(tokens: List(Token), scan: lexer.Scanner)
}

fn look_from(p: P) -> Look {
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
    KString ->
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

fn check_use_strict_at_start(p: P) -> Result(P, ParseError) {
  case p.ctx.strict {
    True -> Ok(p)
    False -> scan_directive_prologue(p, look_from(p))
  }
}

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

// annex b escapes before use strict are retroactively illegal
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

fn check_retroactive_params(p: P) -> Result(P, ParseError) {
  case p.ctx.has_non_simple_param {
    True -> Error(MisplacedUseStrictDirective(pos_of(p)))
    False -> validate_retroactive_param_names(p, p.ctx.param_bound_names)
  }
}

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

// the one place nested function state is initialised; only strict carries in
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
      binding_kind: BindingNone,
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
    sb:,
  )
}

// arrows inherit super/new.target and the arguments restriction
fn enter_arrow_context(p: P, is_async: Bool, param_names: List(String)) -> P {
  let inner = enter_function_context(p, False, is_async, None)
  // arrow scopes own no lexical pseudo-slots
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
      // §15.3: concise body inherits [In]
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

// §15.7.1 static block: [~Yield, +Await, ~Return]
fn enter_static_block_context(p: P) -> P {
  // §15.7.14: retag as ClassStaticBlock scope
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

// restore ctx whole; sb flows forward, only the cursor moves back
fn restore_outer_context(p: P, outer: P) -> P {
  P(..p, ctx: outer.ctx, sb: scope.sb_enter(p.sb, outer.sb.current))
}

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

fn peek(p: P) -> TokenKind {
  case p.tokens {
    [lexer.Token(kind: k, ..), ..] -> k
    [] -> Eof
  }
}

// bounded pure lookahead; deepest grammar lookahead is 3
fn upcoming(p: P, n: Int) -> Token {
  look_at(look_from(p), n)
}

fn peek_at(p: P, n: Int) -> TokenKind {
  case n, p.tokens {
    0, _ -> peek(p)
    1, [_, lexer.Token(kind: k, ..), ..] -> k
    _, _ -> {
      let lexer.Token(kind: k, ..) = upcoming(p, n)
      k
    }
  }
}

// §13.2.8: substitutions are [+In]; spans are rescanned from source
fn parse_template_spans(
  p: P,
) -> Result(#(P, ast.TemplateParts(String)), ParseError) {
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

fn parse_template_substitutions(
  p: P,
  rev_tail: List(#(ast.Expression, String)),
) -> Result(#(P, List(#(ast.Expression, String))), ParseError) {
  use #(p, expr) <- result.try(parse_expression(
    P(..p, last_expr_assignable: False, last_expr_is_assignment: False),
  ))
  case peek(p) {
    RightBrace -> {
      let p = template_continuation(p)
      case peek(p) {
        TemplateHead ->
          parse_template_substitutions(advance(p), [
            #(expr, template_span_raw(p, 2)),
            ..rev_tail
          ])
        TemplateLiteral ->
          Ok(#(advance(p), [#(expr, template_span_raw(p, 1)), ..rev_tail]))
        // unterminated template
        _ -> Error(UnterminatedTemplateSubstitution(pos_of(p)))
      }
    }
    other ->
      Error(error_at_current(p, ExpectedToken(pos_of(p), RightBrace, other)))
  }
}

// window past } is garbage, rescan like a regex
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

// raw quasi text, line terminators normalized (§12.9.6 trv)
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

// escaped contextual keywords are not keywords (§12.7.2)
fn peek_had_escape(p: P) -> Bool {
  case p.tokens {
    [lexer.Token(had_escape: e, ..), ..] -> e
    [] -> False
  }
}

// annex b legacy forms strict code forbids; decided by the lexer
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

fn span_of(p: P) -> ast.Span {
  case p.tokens {
    [lexer.Token(pos: pos, raw_len: raw_len, ..), ..] ->
      ast.Span(start: pos, end: pos + raw_len)
    [] -> ast.Span(start: 0, end: 0)
  }
}

// after.prev_end, or next token start when nothing was consumed
fn consumed_end(before: P, after: P) -> Int {
  case after.prev_end == before.prev_end {
    True -> pos_of(after)
    False -> after.prev_end
  }
}

fn span_from(start: Int, p_after: P) -> ast.Span {
  ast.Span(start:, end: p_after.prev_end)
}

// falls back to the previous line at eof
fn line_of(p: P) -> Int {
  case p.tokens {
    [lexer.Token(line: line, ..), ..] -> line
    [] -> p.prev_line
  }
}

// never lex past a token the parser may rescan
fn advance(p: P) -> P {
  case p.tokens {
    [lexer.Token(line: line, pos: pos, raw_len: rl, ..), ..rest] ->
      case rest {
        [] -> {
          let #(token, scan) = lexer.scan_next(p.scan)
          case may_rescan(token.kind) {
            True ->
              P(
                ..p,
                tokens: [token],
                scan:,
                prev_line: line,
                prev_end: pos + rl,
              )
            False -> {
              let #(next, scan) = lexer.scan_next(scan)
              P(
                ..p,
                tokens: [token, next],
                scan:,
                prev_line: line,
                prev_end: pos + rl,
              )
            }
          }
        }
        [token] ->
          case may_rescan(token.kind) {
            True -> P(..p, tokens: rest, prev_line: line, prev_end: pos + rl)
            False -> {
              let #(next, scan) = lexer.scan_next(p.scan)
              P(
                ..p,
                tokens: [token, next],
                scan:,
                prev_line: line,
                prev_end: pos + rl,
              )
            }
          }
        _ -> P(..p, tokens: rest, prev_line: line, prev_end: pos + rl)
      }
    [] -> p
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

// LexFailure carries the lexer error; Illegal does not
fn illegal_token_error(p: P) -> ParseError {
  case peek(p) {
    LexFailure(err) -> lex_error(err)
    kind -> UnexpectedToken(pos_of(p), kind)
  }
}

// route unexpected tokens here or lexer errors get masked
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

fn has_line_break_before(p: P) -> Bool {
  case p.tokens {
    [lexer.Token(line: current_line, ..), ..] -> current_line > p.prev_line
    [] -> True
  }
}

fn token_line_at(p: P, n: Int) -> Int {
  let lexer.Token(kind: kind, line: line, ..) = upcoming(p, n)
  case kind {
    // -1 past eof so line comparisons never match
    Eof -> -1
    _ -> line
  }
}

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
