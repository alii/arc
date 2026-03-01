/// JavaScript parser for Arc.
/// Recursive descent parser for ES2023+ strict mode JavaScript.
///
/// The parser consumes a token list and validates syntax.
/// Uses Pratt parsing for expression precedence.
import arc/ast
import arc/lexer.{
  type Token, type TokenKind, Ampersand, AmpersandAmpersand,
  AmpersandAmpersandEqual, AmpersandEqual, Arrow, As, Async, Await, Bang,
  BangEqual, BangEqualEqual, Break, Caret, CaretEqual, Case, Catch, Class, Colon,
  Comma, Const, Continue, Debugger, Default, Delete, Do, Dot, DotDotDot, Else,
  Eof, Equal, EqualEqual, EqualEqualEqual, Export, Extends, Finally, For, From,
  Function, GreaterThan, GreaterThanEqual, GreaterThanGreaterThan,
  GreaterThanGreaterThanEqual, GreaterThanGreaterThanGreaterThan,
  GreaterThanGreaterThanGreaterThanEqual, Identifier, If, Import, In, Instanceof,
  KFalse, KString, KTrue, LeftBrace, LeftBracket, LeftParen, LessThan,
  LessThanEqual, LessThanLessThan, LessThanLessThanEqual, Let, Minus, MinusEqual,
  MinusMinus, New, Null, Number, Of, Percent, PercentEqual, Pipe, PipeEqual,
  PipePipe, PipePipeEqual, Plus, PlusEqual, PlusPlus, Question, QuestionDot,
  QuestionQuestion, QuestionQuestionEqual, RegularExpression, Return, RightBrace,
  RightBracket, RightParen, Semicolon, Slash, SlashEqual, Star, StarEqual,
  StarStar, StarStarEqual, Static, Super, Switch, TemplateLiteral, This, Throw,
  Tilde, Try, Typeof, Undefined, Var, Void, While, With, Yield,
}
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

pub type ParseMode {
  Script
  Module
}

// NOTE FOR OTHER AGENTS: ParseError uses typed variants (like LexError in lexer.gleam).
// Use parse_error_to_string(e) and parse_error_pos(e) to read errors.
// Add new variants here instead of using string messages.
pub type ParseError {
  LexerError(message: String, pos: Int)
  ExpectedToken(expected: String, got: String, pos: Int)
  ExpectedIdentifier(pos: Int)
  ExpectedSemicolon(pos: Int)
  ExpectedBindingPattern(pos: Int)
  ExpectedPropertyName(pos: Int)
  ExpectedImportSpecifierName(pos: Int)
  ExpectedExportSpecifierName(pos: Int)
  ExpectedCaseDefaultOrBrace(pos: Int)
  UnexpectedCloseBrace(pos: Int)
  UnexpectedExport(pos: Int)
  UnexpectedSuper(pos: Int)
  UnexpectedCloseParen(pos: Int)
  UnexpectedToken(token: String, pos: Int)
  NotAnArrowFunction(pos: Int)
  ReturnOutsideFunction(pos: Int)
  BreakOutsideLoopOrSwitch(pos: Int)
  ContinueOutsideLoop(pos: Int)
  ReservedWordStrictMode(name: String, pos: Int)
  YieldReservedStrictMode(pos: Int)
  LetIdentifierStrictMode(pos: Int)
  StaticReservedStrictMode(pos: Int)
  WithNotAllowedStrictMode(pos: Int)
  DeleteUnqualifiedStrictMode(pos: Int)
  OctalEscapeStrictMode(pos: Int)
  OctalLiteralStrictMode(pos: Int)
  YieldInGenerator(pos: Int)
  AwaitInModule(pos: Int)
  AwaitInAsyncFunction(pos: Int)
  EnumReservedWord(pos: Int)
  DuplicateParameterName(name: String, pos: Int)
  DuplicateBindingLexical(name: String, pos: Int)
  DuplicateExport(name: String, pos: Int)
  DuplicateImportBinding(name: String, pos: Int)
  DuplicateLabel(label: String, pos: Int)
  DuplicateProtoProperty(pos: Int)
  IdentifierAlreadyDeclared(name: String, pos: Int)
  LexicalDeclInSingleStatement(pos: Int)
  YieldInFormalParameter(pos: Int)
  InvalidLhsPrefixOp(pos: Int)
  SuperCallNotInDerivedConstructor(pos: Int)
  SuperPropertyNotInMethod(pos: Int)
  NewTargetOutsideFunction(pos: Int)
  MissingConstInitializer(pos: Int)
  RestTrailingComma(pos: Int)
  ExpectedForHeadSeparator(pos: Int)
  MissingCatchOrFinally(pos: Int)
  StrictModeModification(name: String, pos: Int)
  ExpectedModuleSpecifier(pos: Int)
  DestructuringMissingInitializer(pos: Int)
  ExpectedCommaOrBracket(pos: Int)
  SetterExactlyOneParam(pos: Int)
  ClassConstructorNotGetter(pos: Int)
  ExpectedCommaOrObjectClose(pos: Int)
  ExpectedForDeclSeparator(pos: Int)
  ExpectedCloseAfterSetter(pos: Int)
  ClassConstructorNotSetter(pos: Int)
  InvalidForInOfLhs(kind: String, pos: Int)
  ExpectedForSeparator(pos: Int)
  UndefinedLabel(label: String, pos: Int)
  ThrowLineBreak(pos: Int)
  GetterNoParams(pos: Int)
  SetterNoRest(pos: Int)
  RestMustBeLast(pos: Int)
  ClassConstructorGenerator(pos: Int)
  ClassConstructorAsync(pos: Int)
  ClassDuplicateConstructor(pos: Int)
  StaticPrototype(pos: Int)
  LexicalDeclInLabel(pos: Int)
  GeneratorDeclLabeled(pos: Int)
  InvalidDestructuringTarget(pos: Int)
  InvalidAssignmentLhs(pos: Int)
  ExpectedNewTarget(pos: Int)
  ExpectedImportMeta(pos: Int)
  ExpectedCallOrDotAfterImport(pos: Int)
  ExpectedIdentifierAfterDot(pos: Int)
  ExpectedAfterOptionalChain(pos: Int)
  ExpectedCommaOrCloseParen(pos: Int)
  ExpectedCommaOrBracketInArray(pos: Int)
  ExpectedCommaOrBracketInExpr(pos: Int)
  ExpectedCommaOrBraceInObject(pos: Int)
  ExpectedCommaOrBraceInObjectLiteral(pos: Int)
  ExpectedBraceOrStarAfterComma(pos: Int)
  ExpectedFromOrComma(pos: Int)
  ExpectedImportSpecifier(pos: Int)
  ExpectedCommaOrBraceInImport(pos: Int)
  ExpectedFunctionAfterAsync(pos: Int)
  ExpectedAsOrFromAfterExportStar(pos: Int)
  UnexpectedAfterExport(pos: Int)
  ExpectedCommaOrBraceInExport(pos: Int)
  ExpectedExportAlias(pos: Int)
  FunctionDeclInSingleStatement(pos: Int)
  StrictModeBindingName(name: String, pos: Int)
  LetBindingInLexicalDecl(pos: Int)
  ForInInitializer(pos: Int)
  ForOfInitializer(pos: Int)
  StrictModeParamName(name: String, pos: Int)
  RestDefaultInitializer(pos: Int)
  FunctionDeclInLabelBody(pos: Int)
  ShorthandDefaultOutsideDestructuring(pos: Int)
  StrictModeAssignment(name: String, pos: Int)
  EvalArgsAssignStrictMode(pos: Int)
  InvalidPostfixLhs(pos: Int)
  ExpectedNewTargetGot(got: String, pos: Int)
  ExpectedImportMetaGot(got: String, pos: Int)
  StrictModeModifyRestricted(name: String, pos: Int)
  ExpectedIdentifierAsString(name: String, pos: Int)
  DuplicateParamNameStrictMode(name: String, pos: Int)
  ReservedWordImportBinding(name: String, pos: Int)
  DuplicateDefaultCase(pos: Int)
  UndeclaredExportBinding(name: String, pos: Int)
  ImportNotTopLevel(pos: Int)
  ExportNotTopLevel(pos: Int)
  UnicodeEscapeInMetaProperty(pos: Int)
}

pub fn parse_error_to_string(error: ParseError) -> String {
  case error {
    LexerError(message:, ..) -> message
    ExpectedToken(expected:, got:, ..) ->
      "Expected " <> expected <> " but got " <> got
    ExpectedIdentifier(_) -> "Expected identifier"
    ExpectedSemicolon(_) -> "Expected ';'"
    ExpectedBindingPattern(_) -> "Expected binding pattern"
    ExpectedPropertyName(_) -> "Expected property name"
    ExpectedImportSpecifierName(_) -> "Expected import specifier name"
    ExpectedExportSpecifierName(_) -> "Expected export specifier name"
    ExpectedCaseDefaultOrBrace(_) -> "Expected 'case', 'default', or '}'"
    UnexpectedCloseBrace(_) -> "Unexpected '}'"
    UnexpectedExport(_) -> "Unexpected 'export'"
    UnexpectedSuper(_) -> "Unexpected 'super'"
    UnexpectedCloseParen(_) -> "Unexpected token ')'"
    UnexpectedToken(token:, ..) -> "Unexpected token: " <> token
    NotAnArrowFunction(_) -> "Not an arrow function"
    ReturnOutsideFunction(_) -> "'return' outside of function"
    BreakOutsideLoopOrSwitch(_) -> "'break' outside of loop or switch"
    ContinueOutsideLoop(_) -> "'continue' outside of loop"
    ReservedWordStrictMode(name:, ..) ->
      "'" <> name <> "' is a reserved word in strict mode"
    YieldReservedStrictMode(_) -> "'yield' is a reserved word in strict mode"
    LetIdentifierStrictMode(_) ->
      "'let' cannot be used as identifier in strict mode"
    StaticReservedStrictMode(_) -> "'static' is a reserved word in strict mode"
    WithNotAllowedStrictMode(_) -> "'with' not allowed in strict mode"
    DeleteUnqualifiedStrictMode(_) ->
      "Cannot delete unqualified identifier in strict mode"
    OctalEscapeStrictMode(_) ->
      "Octal escape sequences are not allowed in strict mode"
    OctalLiteralStrictMode(_) -> "Octal literals are not allowed in strict mode"
    YieldInGenerator(_) -> "'yield' cannot be used as identifier in generator"
    AwaitInModule(_) -> "'await' cannot be used as identifier in module"
    AwaitInAsyncFunction(_) ->
      "'await' cannot be used as identifier in async function"
    EnumReservedWord(_) -> "'enum' is a reserved word"
    DuplicateParameterName(name:, ..) ->
      "Duplicate parameter name '" <> name <> "' not allowed"
    DuplicateBindingLexical(name:, ..) ->
      "Duplicate binding '" <> name <> "' in lexical declaration"
    DuplicateExport(name:, ..) -> "Duplicate export of '" <> name <> "'"
    DuplicateImportBinding(name:, ..) ->
      "Duplicate import binding '" <> name <> "'"
    DuplicateLabel(label:, ..) -> "Duplicate label '" <> label <> "'"
    DuplicateProtoProperty(_) ->
      "Duplicate '__proto__' property in object literal"
    IdentifierAlreadyDeclared(name:, ..) ->
      "Identifier '" <> name <> "' has already been declared"
    LexicalDeclInSingleStatement(_) ->
      "Lexical declaration cannot appear in a single-statement context"
    YieldInFormalParameter(_) ->
      "Yield expression not allowed in formal parameter"
    InvalidLhsPrefixOp(_) ->
      "Invalid left-hand side expression in prefix operation"
    SuperCallNotInDerivedConstructor(_) ->
      "'super()' is only valid in a derived class constructor"
    SuperPropertyNotInMethod(_) ->
      "'super' property access is only valid inside a method"
    NewTargetOutsideFunction(_) -> "'new.target' outside of function"
    MissingConstInitializer(_) -> "Missing initializer in const declaration"
    RestTrailingComma(_) -> "Rest element may not have a trailing comma"
    ExpectedForHeadSeparator(_) -> "Expected 'in', 'of', ';', or ','"
    MissingCatchOrFinally(_) -> "Missing catch or finally after try"
    StrictModeModification(name:, ..) ->
      "'" <> name <> "' cannot be modified in strict mode"
    ExpectedModuleSpecifier(_) -> "Expected module specifier"
    DestructuringMissingInitializer(_) ->
      "Destructuring declaration must have an initializer"
    ExpectedCommaOrBracket(_) -> "Expected ',' or ']' in array destructuring"
    SetterExactlyOneParam(_) -> "Setter must have exactly one parameter"
    ClassConstructorNotGetter(_) -> "Class constructor may not be a getter"
    ExpectedCommaOrObjectClose(_) ->
      "Expected ',' or '}' in object destructuring"
    ExpectedForDeclSeparator(_) -> "Expected 'in', 'of', ';', '=', or ','"
    ExpectedCloseAfterSetter(_) -> "Expected ')' after setter parameter"
    ClassConstructorNotSetter(_) -> "Class constructor may not be a setter"
    InvalidForInOfLhs(kind:, ..) ->
      "Invalid left-hand side in for-" <> kind <> " statement"
    ExpectedForSeparator(_) -> "Expected ';', 'in', or 'of' in for statement"
    UndefinedLabel(label:, ..) -> "Undefined label '" <> label <> "'"
    ThrowLineBreak(_) ->
      "No line break is allowed between 'throw' and its expression"
    GetterNoParams(_) -> "Getter must have no parameters"
    SetterNoRest(_) -> "Setter parameter cannot be a rest parameter"
    RestMustBeLast(_) -> "Rest parameter must be last formal parameter"
    ClassConstructorGenerator(_) -> "Class constructor may not be a generator"
    ClassConstructorAsync(_) -> "Class constructor may not be an async method"
    ClassDuplicateConstructor(_) -> "A class may only have one constructor"
    StaticPrototype(_) ->
      "Classes may not have a static property named 'prototype'"
    LexicalDeclInLabel(_) ->
      "Lexical declaration cannot appear in a labeled statement"
    GeneratorDeclLabeled(_) -> "Generator declarations cannot be labeled"
    InvalidDestructuringTarget(_) -> "Invalid destructuring assignment target"
    InvalidAssignmentLhs(_) -> "Invalid left-hand side in assignment"
    ExpectedNewTarget(_) -> "Expected 'target' after 'new.'"
    ExpectedImportMeta(_) -> "Expected 'meta' after 'import.'"
    ExpectedCallOrDotAfterImport(_) -> "Expected '(' or '.' after 'import'"
    ExpectedIdentifierAfterDot(_) -> "Expected identifier after '.'"
    ExpectedAfterOptionalChain(_) ->
      "Expected identifier, '[', or '(' after '?.'"
    ExpectedCommaOrCloseParen(_) -> "Expected ',' or ')' in arguments"
    ExpectedCommaOrBracketInArray(_) -> "Expected ',' or ']'"
    ExpectedCommaOrBracketInExpr(_) -> "Expected ',' or ']'"
    ExpectedCommaOrBraceInObject(_) -> "Expected ',' or '}' in object"
    ExpectedCommaOrBraceInObjectLiteral(_) ->
      "Expected ',' or '}' in object literal"
    ExpectedBraceOrStarAfterComma(_) -> "Expected '{' or '*' after ','"
    ExpectedFromOrComma(_) -> "Expected 'from' or ','"
    ExpectedImportSpecifier(_) -> "Expected import specifier"
    ExpectedCommaOrBraceInImport(_) ->
      "Expected ',' or '}' in import specifiers"
    ExpectedFunctionAfterAsync(_) ->
      "Expected 'function' after 'async' in export"
    ExpectedAsOrFromAfterExportStar(_) ->
      "Expected 'as' or 'from' after 'export *'"
    UnexpectedAfterExport(_) -> "Unexpected token after 'export'"
    ExpectedCommaOrBraceInExport(_) ->
      "Expected ',' or '}' in export specifiers"
    ExpectedExportAlias(_) -> "Expected export alias"
    FunctionDeclInSingleStatement(_) ->
      "Function declarations are not allowed in single-statement context"
    StrictModeBindingName(name:, ..) ->
      "'" <> name <> "' cannot be used as a binding name in strict mode"
    LetBindingInLexicalDecl(_) ->
      "'let' cannot be used as a binding name in lexical declaration"
    ForInInitializer(_) ->
      "for-in variable declaration may not have an initializer"
    ForOfInitializer(_) ->
      "for-of variable declaration may not have an initializer"
    StrictModeParamName(name:, ..) ->
      "'" <> name <> "' is not allowed as a parameter name in strict mode"
    RestDefaultInitializer(_) ->
      "Rest parameter may not have a default initializer"
    FunctionDeclInLabelBody(_) ->
      "Function declarations are not allowed as the body of a labeled statement in strict mode"
    ShorthandDefaultOutsideDestructuring(_) ->
      "Shorthand property with default is not valid outside destructuring"
    StrictModeAssignment(name:, ..) ->
      "'" <> name <> "' cannot be assigned to in strict mode"
    EvalArgsAssignStrictMode(_) ->
      "'eval' or 'arguments' cannot be assigned to in strict mode"
    InvalidPostfixLhs(_) ->
      "Invalid left-hand side expression in postfix operation"
    ExpectedNewTargetGot(got:, ..) ->
      "Expected 'target' after 'new.' but got '" <> got <> "'"
    ExpectedImportMetaGot(got:, ..) ->
      "Expected 'meta' after 'import.' but got '" <> got <> "'"
    StrictModeModifyRestricted(name:, ..) ->
      "'" <> name <> "' cannot be modified in strict mode"
    ExpectedIdentifierAsString(name:, ..) ->
      "'" <> name <> "' is a reserved word and cannot be used as an identifier"
    DuplicateParamNameStrictMode(name:, ..) ->
      "Duplicate parameter name '" <> name <> "' not allowed in strict mode"
    ReservedWordImportBinding(name:, ..) ->
      "'"
      <> name
      <> "' is a reserved word and cannot be used as an import binding"
    DuplicateDefaultCase(_) ->
      "More than one default clause in switch statement"
    UndeclaredExportBinding(name:, ..) ->
      "Export '" <> name <> "' is not defined in module scope"
    ImportNotTopLevel(_) ->
      "'import' declarations may only appear at top level of a module"
    ExportNotTopLevel(_) ->
      "'export' declarations may only appear at top level of a module"
    UnicodeEscapeInMetaProperty(_) ->
      "'target' in new.target must not contain unicode escape sequences"
  }
}

pub fn parse_error_pos(error: ParseError) -> Int {
  case error {
    LexerError(pos:, ..) -> pos
    ExpectedToken(pos:, ..) -> pos
    ExpectedIdentifier(pos:) -> pos
    ExpectedSemicolon(pos:) -> pos
    ExpectedBindingPattern(pos:) -> pos
    ExpectedPropertyName(pos:) -> pos
    ExpectedImportSpecifierName(pos:) -> pos
    ExpectedExportSpecifierName(pos:) -> pos
    ExpectedCaseDefaultOrBrace(pos:) -> pos
    UnexpectedCloseBrace(pos:) -> pos
    UnexpectedExport(pos:) -> pos
    UnexpectedSuper(pos:) -> pos
    UnexpectedCloseParen(pos:) -> pos
    UnexpectedToken(pos:, ..) -> pos
    NotAnArrowFunction(pos:) -> pos
    ReturnOutsideFunction(pos:) -> pos
    BreakOutsideLoopOrSwitch(pos:) -> pos
    ContinueOutsideLoop(pos:) -> pos
    ReservedWordStrictMode(pos:, ..) -> pos
    YieldReservedStrictMode(pos:) -> pos
    LetIdentifierStrictMode(pos:) -> pos
    StaticReservedStrictMode(pos:) -> pos
    WithNotAllowedStrictMode(pos:) -> pos
    DeleteUnqualifiedStrictMode(pos:) -> pos
    OctalEscapeStrictMode(pos:) -> pos
    OctalLiteralStrictMode(pos:) -> pos
    YieldInGenerator(pos:) -> pos
    AwaitInModule(pos:) -> pos
    AwaitInAsyncFunction(pos:) -> pos
    EnumReservedWord(pos:) -> pos
    DuplicateParameterName(pos:, ..) -> pos
    DuplicateBindingLexical(pos:, ..) -> pos
    DuplicateExport(pos:, ..) -> pos
    DuplicateImportBinding(pos:, ..) -> pos
    DuplicateLabel(pos:, ..) -> pos
    DuplicateProtoProperty(pos:) -> pos
    IdentifierAlreadyDeclared(pos:, ..) -> pos
    LexicalDeclInSingleStatement(pos:) -> pos
    YieldInFormalParameter(pos:) -> pos
    InvalidLhsPrefixOp(pos:) -> pos
    SuperCallNotInDerivedConstructor(pos:) -> pos
    SuperPropertyNotInMethod(pos:) -> pos
    NewTargetOutsideFunction(pos:) -> pos
    MissingConstInitializer(pos:) -> pos
    RestTrailingComma(pos:) -> pos
    ExpectedForHeadSeparator(pos:) -> pos
    MissingCatchOrFinally(pos:) -> pos
    StrictModeModification(pos:, ..) -> pos
    ExpectedModuleSpecifier(pos:) -> pos
    DestructuringMissingInitializer(pos:) -> pos
    ExpectedCommaOrBracket(pos:) -> pos
    SetterExactlyOneParam(pos:) -> pos
    ClassConstructorNotGetter(pos:) -> pos
    ExpectedCommaOrObjectClose(pos:) -> pos
    ExpectedForDeclSeparator(pos:) -> pos
    ExpectedCloseAfterSetter(pos:) -> pos
    ClassConstructorNotSetter(pos:) -> pos
    InvalidForInOfLhs(pos:, ..) -> pos
    ExpectedForSeparator(pos:) -> pos
    UndefinedLabel(pos:, ..) -> pos
    ThrowLineBreak(pos:) -> pos
    GetterNoParams(pos:) -> pos
    SetterNoRest(pos:) -> pos
    RestMustBeLast(pos:) -> pos
    ClassConstructorGenerator(pos:) -> pos
    ClassConstructorAsync(pos:) -> pos
    ClassDuplicateConstructor(pos:) -> pos
    StaticPrototype(pos:) -> pos
    LexicalDeclInLabel(pos:) -> pos
    GeneratorDeclLabeled(pos:) -> pos
    InvalidDestructuringTarget(pos:) -> pos
    InvalidAssignmentLhs(pos:) -> pos
    ExpectedNewTarget(pos:) -> pos
    ExpectedImportMeta(pos:) -> pos
    ExpectedCallOrDotAfterImport(pos:) -> pos
    ExpectedIdentifierAfterDot(pos:) -> pos
    ExpectedAfterOptionalChain(pos:) -> pos
    ExpectedCommaOrCloseParen(pos:) -> pos
    ExpectedCommaOrBracketInArray(pos:) -> pos
    ExpectedCommaOrBracketInExpr(pos:) -> pos
    ExpectedCommaOrBraceInObject(pos:) -> pos
    ExpectedCommaOrBraceInObjectLiteral(pos:) -> pos
    ExpectedBraceOrStarAfterComma(pos:) -> pos
    ExpectedFromOrComma(pos:) -> pos
    ExpectedImportSpecifier(pos:) -> pos
    ExpectedCommaOrBraceInImport(pos:) -> pos
    ExpectedFunctionAfterAsync(pos:) -> pos
    ExpectedAsOrFromAfterExportStar(pos:) -> pos
    UnexpectedAfterExport(pos:) -> pos
    ExpectedCommaOrBraceInExport(pos:) -> pos
    ExpectedExportAlias(pos:) -> pos
    FunctionDeclInSingleStatement(pos:) -> pos
    StrictModeBindingName(pos:, ..) -> pos
    LetBindingInLexicalDecl(pos:) -> pos
    ForInInitializer(pos:) -> pos
    ForOfInitializer(pos:) -> pos
    StrictModeParamName(pos:, ..) -> pos
    RestDefaultInitializer(pos:) -> pos
    FunctionDeclInLabelBody(pos:) -> pos
    ShorthandDefaultOutsideDestructuring(pos:) -> pos
    StrictModeAssignment(pos:, ..) -> pos
    EvalArgsAssignStrictMode(pos:) -> pos
    InvalidPostfixLhs(pos:) -> pos
    ExpectedNewTargetGot(pos:, ..) -> pos
    ExpectedImportMetaGot(pos:, ..) -> pos
    StrictModeModifyRestricted(pos:, ..) -> pos
    ExpectedIdentifierAsString(pos:, ..) -> pos
    DuplicateParamNameStrictMode(pos:, ..) -> pos
    ReservedWordImportBinding(pos:, ..) -> pos
    DuplicateDefaultCase(pos:) -> pos
    UndeclaredExportBinding(pos:, ..) -> pos
    ImportNotTopLevel(pos:) -> pos
    ExportNotTopLevel(pos:) -> pos
    UnicodeEscapeInMetaProperty(pos:) -> pos
  }
}

/// What kind of binding declaration we are currently parsing.
/// Controls how names are registered in the block scope.
type BindingKind {
  /// Not inside any declaration binding.
  BindingNone
  /// Inside a var declaration.
  BindingVar
  /// Inside a let/const declaration.
  BindingLexical
  /// Inside formal parameter parsing (treated as lexical for scope purposes).
  BindingParam
}

/// Internal parser state: remaining tokens + the line of the last consumed token.
type P {
  P(
    tokens: List(Token),
    mode: ParseMode,
    prev_line: Int,
    allow_in: Bool,
    source: String,
    bytes: BitArray,
    // Context tracking for semantic validation
    function_depth: Int,
    loop_depth: Int,
    switch_depth: Int,
    in_generator: Bool,
    in_async: Bool,
    strict: Bool,
    label_set: List(#(String, Bool)),
    // super() is only allowed in constructors of derived classes
    allow_super_call: Bool,
    // super.x is allowed in any method (class or object literal)
    allow_super_property: Bool,
    // Whether the last parsed expression is a valid assignment/update target
    // (identifier, member expression). Set by expression parsers, checked by
    // assignment operators and ++/--.
    last_expr_assignable: Bool,
    // Whether the last parsed expression was a plain assignment (target = expr).
    // This is needed to distinguish `{a: b = 1}` (valid pattern: b is target,
    // 1 is default) from `{a: 0}` (invalid pattern: literal). Both have
    // last_expr_assignable=False, but only the assignment covers AssignmentPattern.
    last_expr_is_assignment: Bool,
    // Whether we are currently parsing the bindings of a let/const declaration.
    // When true, "let" is forbidden as a binding name even in sloppy mode.
    in_lexical_decl: Bool,
    // Binding names seen so far in the current let/const declaration.
    // Used to detect duplicate bindings like `let a, a;`.
    // Only checked when in_lexical_decl is True.
    decl_bound_names: Set(String),
    // Whether the expression being parsed contains a cover-grammar initializer
    // like `{a = 0}` in an object literal. This is only valid if the
    // expression ends up as a destructuring pattern (assignment LHS or arrow
    // params). If the expression is used as a normal expression, this must
    // trigger an error.
    has_cover_initializer: Bool,
    // Whether we are inside formal parameter parsing. When true, yield
    // expressions are forbidden even inside generators.
    in_formal_params: Bool,
    // Whether we are inside arrow function parameter parsing. When true,
    // duplicate parameter names are always an error (even in sloppy mode).
    in_arrow_params: Bool,
    // Whether we are inside a method definition (class or object literal).
    // Methods always forbid duplicate parameter names even in sloppy mode.
    in_method: Bool,
    // Whether the current formal parameter list contains non-simple params
    // (destructuring, defaults, or rest). When true, duplicate parameter
    // names are always forbidden even in sloppy mode.
    has_non_simple_param: Bool,
    // Parameter names accumulated during formal parameter parsing.
    // Used to detect duplicate parameter names across all params including
    // destructured ones. Populated when in_formal_params is True.
    param_bound_names: List(String),
    // Whether we are in a single-statement position (body of if/for/while/
    // do-while/with). Used to forbid labeled function declarations in these
    // contexts per spec Annex B 3.4.
    in_single_stmt_pos: Bool,
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
    // Whether we are currently parsing an export var/let/const declaration.
    // When true, each binding name is also registered as an export name.
    in_export_decl: Bool,
    // Name of the last parsed simple identifier expression (e.g. "eval",
    // "arguments", or any variable name). Cleared by member access, calls,
    // and compound expressions.  Used to check strict-mode restrictions on
    // eval/arguments as operands of ++/-- and for-in/of LHS.
    last_expr_name: Option(String),
    // Whether eval or arguments appeared as a simple identifier inside an
    // array/object literal that could become a destructuring assignment
    // target. Checked in strict mode when `=` follows `[…]` or `{…}`.
    has_eval_args_target: Bool,
    // Function name saved during function parsing. When
    // check_use_strict_in_body retroactively enables strict mode, this name
    // is validated to reject eval/arguments.
    pending_strict_name: Option(String),
    // --- Block scope tracking for cross-statement duplicate detection ---
    // Lexical (let/const) binding names in the current block scope.
    scope_lexical: Set(String),
    // Var binding names in the current function scope.
    // Duplicate var is always allowed, so this is only checked against lexical.
    scope_var: Set(String),
    // Parameter names in the current function scope.
    // var does NOT conflict with params, but let/const does.
    scope_params: Set(String),
    // Function declaration names in the current scope (sloppy mode only).
    // In sloppy mode, duplicate function declarations are allowed.
    // In strict mode, function declarations go into scope_lexical instead.
    scope_funcs: Set(String),
    // Lexical names from parent block scopes up to the function boundary.
    // Used to detect var declarations that conflict with outer let/const.
    outer_lexical: Set(String),
    // What kind of binding we are currently parsing.
    binding_kind: BindingKind,
    // Whether we are inside a block scope (as opposed to function/script top-level).
    // Affects function declaration scoping: in blocks, function decls are lexical;
    // at function/script top level in sloppy mode, they are var-like.
    in_block: Bool,
    module_top_level: Bool,
  )
}

/// Helper: set last_expr_assignable to False on Ok results.
fn set_not_assignable(
  res: Result(#(P, ast.Expression), ParseError),
) -> Result(#(P, ast.Expression), ParseError) {
  use #(p, expr) <- result.map(res)
  #(P(..p, last_expr_assignable: False, last_expr_is_assignment: False), expr)
}

/// Adapter: extract just P from an expression result.
/// Used by callers not yet converted to consume expressions.
fn expr_p(
  res: Result(#(P, ast.Expression), ParseError),
) -> Result(P, ParseError) {
  use #(p, _) <- result.map(res)
  p
}

/// Convert a declaration statement to an expression for export default.
/// FunctionDeclaration -> FunctionExpression, ClassDeclaration -> ClassExpression.
fn statement_to_default_export_expr(stmt: ast.Statement) -> ast.Expression {
  case stmt {
    ast.FunctionDeclaration(name:, params:, body:, is_generator:, is_async:) ->
      ast.FunctionExpression(name:, params:, body:, is_generator:, is_async:)
    ast.ClassDeclaration(name:, super_class:, body:) ->
      ast.ClassExpression(name:, super_class:, body:)
    _ -> ast.Identifier("*default*")
  }
}

// ---- Public API ----

/// Parse JavaScript source code into an AST.
/// Returns Ok(ast.Program) on valid parse, Error on parse failure.
pub fn parse(source: String, mode: ParseMode) -> Result(ast.Program, ParseError) {
  let tokenize_fn = case mode {
    Module -> lexer.tokenize_module
    Script -> lexer.tokenize
  }
  case tokenize_fn(source) {
    Error(e) ->
      Error(LexerError(lexer.lex_error_to_string(e), lexer.lex_error_pos(e)))
    Ok(tokens) -> {
      let p =
        P(
          tokens: tokens,
          mode: mode,
          prev_line: 1,
          allow_in: True,
          source: source,
          bytes: bit_array.from_string(source),
          function_depth: 0,
          loop_depth: 0,
          switch_depth: 0,
          in_generator: False,
          in_async: False,
          strict: mode == Module,
          label_set: [],
          allow_super_call: False,
          allow_super_property: False,
          last_expr_assignable: False,
          last_expr_is_assignment: False,
          in_lexical_decl: False,
          decl_bound_names: set.new(),
          has_cover_initializer: False,
          in_formal_params: False,
          in_arrow_params: False,
          in_method: False,
          has_non_simple_param: False,
          param_bound_names: [],
          in_single_stmt_pos: False,
          has_invalid_pattern: False,
          export_names: set.new(),
          export_local_refs: [],
          import_bindings: set.new(),
          in_export_decl: False,
          last_expr_name: None,
          has_eval_args_target: False,
          pending_strict_name: None,
          scope_lexical: set.new(),
          scope_var: set.new(),
          scope_params: set.new(),
          scope_funcs: set.new(),
          outer_lexical: set.new(),
          binding_kind: BindingNone,
          in_block: False,
          module_top_level: False,
        )
      case mode {
        Script -> parse_script(p)
        Module -> parse_module(p)
      }
    }
  }
}

// ---- Script / Module entry points ----

fn parse_script(p: P) -> Result(ast.Program, ParseError) {
  use p <- result.try(check_use_strict_at_start(p))
  use #(_p, stmts) <- result.try(parse_statement_list(p, True, []))
  Ok(ast.Script(body: stmts))
}

fn parse_module(p: P) -> Result(ast.Program, ParseError) {
  use #(p_final, items) <- result.try(parse_module_body(p, []))
  use Nil <- result.try(validate_export_local_refs(p_final))
  Ok(ast.Module(body: items))
}

fn parse_module_body(
  p: P,
  acc: List(ast.ModuleItem),
) -> Result(#(P, List(ast.ModuleItem)), ParseError) {
  let p = P(..p, module_top_level: True)
  case peek(p) {
    Eof -> Ok(#(p, list.reverse(acc)))
    Import -> {
      use #(p2, item) <- result.try(parse_import_declaration(p))
      parse_module_body(p2, [item, ..acc])
    }
    Export -> {
      use #(p2, item) <- result.try(parse_export_declaration(p))
      parse_module_body(p2, [item, ..acc])
    }
    _ -> {
      use #(p2, stmt) <- result.try(parse_statement(p))
      parse_module_body(p2, [ast.StatementItem(stmt), ..acc])
    }
  }
}

/// Validate that every local name referenced in `export { name }` specifiers
/// is actually declared somewhere in the module scope.
fn validate_export_local_refs(p: P) -> Result(Nil, ParseError) {
  validate_export_refs_loop(
    p.export_local_refs,
    p.scope_var,
    p.scope_lexical,
    p.scope_funcs,
    p.import_bindings,
  )
}

fn validate_export_refs_loop(
  refs: List(#(String, Int)),
  scope_var: Set(String),
  scope_lexical: Set(String),
  scope_funcs: Set(String),
  import_bindings: Set(String),
) -> Result(Nil, ParseError) {
  case refs {
    [] -> Ok(Nil)
    [#(name, pos), ..rest] -> {
      let declared =
        set.contains(scope_var, name)
        || set.contains(scope_lexical, name)
        || set.contains(scope_funcs, name)
        || set.contains(import_bindings, name)
      case declared {
        True ->
          validate_export_refs_loop(
            rest,
            scope_var,
            scope_lexical,
            scope_funcs,
            import_bindings,
          )
        False -> Error(UndeclaredExportBinding(name, pos))
      }
    }
  }
}

// ---- Statement parsing ----

fn parse_statement_list(
  p: P,
  top_level: Bool,
  acc: List(ast.Statement),
) -> Result(#(P, List(ast.Statement)), ParseError) {
  case peek(p) {
    Eof -> Ok(#(p, list.reverse(acc)))
    RightBrace -> {
      use <- bool.guard(top_level, Error(UnexpectedCloseBrace(pos_of(p))))
      Ok(#(p, list.reverse(acc)))
    }
    _ -> {
      use #(p2, stmt) <- result.try(parse_statement(p))
      parse_statement_list(p2, top_level, [stmt, ..acc])
    }
  }
}

fn parse_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  case peek(p) {
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
    Function -> parse_function_declaration(p)
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
        Function -> parse_async_function_declaration(p)
        Colon -> parse_labeled_statement(p)
        _ -> parse_expression_statement(p)
      }
    }
    Yield | Await -> {
      case peek_at(p, 1) {
        Colon -> parse_labeled_statement(p)
        _ -> parse_expression_statement(p)
      }
    }
    Import -> {
      // import.meta or import() in script mode
      case peek_at(p, 1) {
        Dot | LeftParen -> parse_expression_statement(p)
        _ -> {
          case p.mode {
            Module ->
              case p.module_top_level {
                // Top-level imports handled by parse_module_body
                True -> Error(ImportNotTopLevel(pos_of(p)))
                False -> Error(ImportNotTopLevel(pos_of(p)))
              }
            Script -> parse_expression_statement(p)
          }
        }
      }
    }
    Export -> {
      case p.mode {
        Module ->
          case p.module_top_level {
            // Top-level exports handled by parse_module_body
            True -> Error(ExportNotTopLevel(pos_of(p)))
            False -> Error(ExportNotTopLevel(pos_of(p)))
          }
        Script -> Error(UnexpectedExport(pos_of(p)))
      }
    }
    // Labeled statement or expression statement
    Identifier -> {
      case peek_at(p, 1) {
        Colon -> parse_labeled_statement(p)
        _ -> parse_expression_statement(p)
      }
    }
    _ -> parse_expression_statement(p)
  }
}

/// Parse a statement in a single-statement context (if/else/while/do-while/for/with body).
/// Lexical declarations (let/const) are not allowed in these positions.
/// `allow_fn` controls whether bare function declarations are allowed (Annex B for `if`).
fn parse_single_statement(
  p: P,
  allow_fn: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  let p = P(..p, in_single_stmt_pos: True, module_top_level: False)
  case peek(p) {
    Const -> Error(LexicalDeclInSingleStatement(pos_of(p)))
    Let ->
      case
        peek_at(p, 1) == LeftBrace
        || peek_at(p, 1) == LeftBracket
        || is_identifier_or_keyword(peek_at(p, 1))
      {
        True -> Error(LexicalDeclInSingleStatement(pos_of(p)))
        False -> parse_statement(p)
      }
    Function ->
      case allow_fn && !p.strict {
        // Annex B: bare function declaration allowed in if body (sloppy mode)
        True -> parse_statement(p)
        // In strict mode or in iteration/with context, function decls are forbidden
        False -> Error(FunctionDeclInSingleStatement(pos_of(p)))
      }
    Class -> Error(LexicalDeclInSingleStatement(pos_of(p)))
    _ -> parse_statement(p)
  }
}

fn parse_block_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  use p2 <- result.try(expect(p, LeftBrace))
  // Enter block scope: new empty scope with current names pushed to outer
  let p_inner =
    P(
      ..p2,
      scope_lexical: set.new(),
      scope_var: set.new(),
      scope_funcs: set.new(),
      outer_lexical: p2.scope_lexical
        |> set.union(p2.scope_funcs)
        |> set.union(p2.outer_lexical),
      in_single_stmt_pos: False,
      in_block: True,
      module_top_level: False,
    )
  use #(p3, stmts) <- result.try(parse_statement_list(p_inner, False, []))
  use p4 <- result.try(expect(p3, RightBrace))
  // Restore enclosing scope from p2 (immutable -- no saving needed)
  Ok(#(
    P(
      ..p4,
      scope_lexical: p2.scope_lexical,
      scope_var: p2.scope_var,
      scope_funcs: p2.scope_funcs,
      outer_lexical: p2.outer_lexical,
      in_block: p.in_block,
      module_top_level: p.module_top_level,
    ),
    ast.BlockStatement(body: stmts),
  ))
}

fn parse_variable_declaration(p: P) -> Result(#(P, ast.Statement), ParseError) {
  // var/let/const
  let kind = peek(p)
  let is_lexical = kind == Let || kind == Const
  let p2 = advance(p)
  let p2 = case is_lexical {
    True ->
      P(
        ..p2,
        in_lexical_decl: True,
        decl_bound_names: set.new(),
        binding_kind: BindingLexical,
      )
    False -> P(..p2, binding_kind: BindingVar)
  }
  use #(p3, declarations) <- result.try(
    parse_variable_declarator_list(p2, kind, []),
  )
  use p4 <- result.try(eat_semicolon(
    P(
      ..p3,
      in_lexical_decl: p.in_lexical_decl,
      decl_bound_names: p.decl_bound_names,
      binding_kind: p.binding_kind,
      in_export_decl: False,
    ),
  ))
  let ast_kind = case kind {
    Let -> ast.Let
    Const -> ast.Const
    _ -> ast.Var
  }
  Ok(#(p4, ast.VariableDeclaration(kind: ast_kind, declarations:)))
}

fn parse_variable_declarator_list(
  p: P,
  kind: TokenKind,
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
  kind: TokenKind,
) -> Result(#(P, ast.VariableDeclarator), ParseError) {
  let is_destructuring = case peek(p) {
    LeftBracket | LeftBrace -> True
    _ -> False
  }
  use #(p2, pattern) <- result.try(parse_binding_pattern(p))
  case peek(p2) {
    Equal -> {
      use #(p3, init_expr) <- result.try(
        parse_assignment_expression(advance(p2)),
      )
      Ok(#(p3, ast.VariableDeclarator(id: pattern, init: Some(init_expr))))
    }
    _ -> {
      use <- bool.guard(
        kind == Const,
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
        False -> Error(ExpectedBindingPattern(pos_of(p)))
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
  Ok(#(advance(p), ast.IdentifierPattern(name: val)))
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
  Ok(#(p, ast.IdentifierPattern(name: val)))
}

/// Accumulate a binding name in param_bound_names when inside formal params,
/// arrow params, or methods. Checks for duplicate param names across all
/// params including those inside destructured patterns.
fn accumulate_param_name(p: P, name: String) -> Result(P, ParseError) {
  case p.in_formal_params || p.in_arrow_params || p.in_method {
    True ->
      case list.contains(p.param_bound_names, name) {
        True ->
          // In strict mode, arrow params, methods, or non-simple params: reject dups
          case
            p.strict
            || p.in_arrow_params
            || p.in_method
            || p.has_non_simple_param
          {
            True -> Error(DuplicateParameterName(name, pos_of(p)))
            // Sloppy non-arrow non-method: dups allowed for simple params.
            // We still accumulate for retroactive strict check.
            False ->
              Ok(P(..p, param_bound_names: [name, ..p.param_bound_names]))
          }
        False -> Ok(P(..p, param_bound_names: [name, ..p.param_bound_names]))
      }
    False -> Ok(p)
  }
}

/// Validate an identifier used as a binding name (let/const/var/param/catch)
fn check_binding_identifier(p: P, name: String) -> Result(Nil, ParseError) {
  case name {
    "enum" -> Error(EnumReservedWord(pos_of(p)))
    "eval" | "arguments" ->
      case p.strict {
        True -> Error(StrictModeBindingName(name, pos_of(p)))
        False -> Ok(Nil)
      }
    "implements"
    | "interface"
    | "package"
    | "private"
    | "protected"
    | "public"
    | "static" ->
      case p.strict {
        True -> Error(ReservedWordStrictMode(name, pos_of(p)))
        False -> Ok(Nil)
      }
    "let" ->
      case p.strict || p.in_lexical_decl {
        True -> Error(LetBindingInLexicalDecl(pos_of(p)))
        False -> Ok(Nil)
      }
    "yield" -> {
      use <- bool.guard(p.strict, Error(YieldReservedStrictMode(pos_of(p))))
      use <- bool.guard(p.in_generator, Error(YieldInGenerator(pos_of(p))))
      Ok(Nil)
    }
    "await" -> {
      use <- bool.guard(p.mode == Module, Error(AwaitInModule(pos_of(p))))
      use <- bool.guard(p.in_async, Error(AwaitInAsyncFunction(pos_of(p))))
      Ok(Nil)
    }
    _ -> Ok(Nil)
  }
}

/// Check if a binding name is a duplicate within the current let/const declaration.
/// Returns Ok(p) with the name added to decl_bound_names, or Error if duplicate.
/// Only checks when in_lexical_decl is True (i.e., inside let/const, not var).
fn check_duplicate_binding(p: P, name: String) -> Result(P, ParseError) {
  case p.in_lexical_decl {
    True ->
      case set.contains(p.decl_bound_names, name) {
        True -> Error(DuplicateBindingLexical(name, pos_of(p)))
        False ->
          Ok(P(..p, decl_bound_names: set.insert(p.decl_bound_names, name)))
      }
    False -> Ok(p)
  }
}

/// Register a binding name in the current block scope.
/// Checks for conflicts based on binding_kind:
/// - BindingLexical: conflicts with scope_lexical, scope_var, scope_params, scope_funcs
/// - BindingParam: conflicts with scope_params; adds to scope_params
/// - BindingVar: conflicts with scope_lexical and outer_lexical only
///   (var can re-declare var, params, and sloppy-mode function decls)
/// - BindingNone: no scope registration
fn register_scope_binding(p: P, name: String) -> Result(P, ParseError) {
  case p.binding_kind {
    BindingLexical -> {
      use <- bool.guard(
        set.contains(p.scope_lexical, name)
          || set.contains(p.scope_var, name)
          || set.contains(p.scope_params, name)
          || set.contains(p.scope_funcs, name),
        Error(IdentifierAlreadyDeclared(name, pos_of(p))),
      )
      Ok(P(..p, scope_lexical: set.insert(p.scope_lexical, name)))
    }
    BindingParam -> Ok(P(..p, scope_params: set.insert(p.scope_params, name)))
    BindingVar -> {
      use <- bool.guard(
        set.contains(p.scope_lexical, name)
          || set.contains(p.outer_lexical, name),
        Error(IdentifierAlreadyDeclared(name, pos_of(p))),
      )
      // set.insert is idempotent so no need to guard against re-declaration
      Ok(P(..p, scope_var: set.insert(p.scope_var, name)))
    }
    BindingNone -> Ok(p)
  }
}

/// Register a function declaration name in the current scope.
/// In strict mode or inside a block, function declarations are lexical.
/// At function/script top-level in sloppy mode, they are var-like (can duplicate).
fn register_function_name(
  p: P,
  name: String,
  name_pos: Int,
) -> Result(P, ParseError) {
  case p.strict || p.in_block {
    True -> {
      // Strict mode or block scope: function decls are lexical
      // In sloppy+block+single-stmt (Annex B function-in-if), function acts
      // like var and can shadow catch params — skip scope_params check
      let check_params = p.strict || !p.in_single_stmt_pos
      let params_conflict = check_params && set.contains(p.scope_params, name)
      use <- bool.guard(
        set.contains(p.scope_lexical, name)
          || set.contains(p.scope_var, name)
          || params_conflict
          || set.contains(p.scope_funcs, name),
        Error(IdentifierAlreadyDeclared(name, name_pos)),
      )
      Ok(P(..p, scope_lexical: set.insert(p.scope_lexical, name)))
    }
    False -> {
      // Sloppy mode at function/script top-level: only conflict with let/const
      use <- bool.guard(
        set.contains(p.scope_lexical, name),
        Error(IdentifierAlreadyDeclared(name, name_pos)),
      )
      Ok(P(..p, scope_funcs: set.insert(p.scope_funcs, name)))
    }
  }
}

/// Register a class declaration name in the current scope.
/// Class declarations are always lexical bindings (like let/const).
/// They conflict with scope_lexical, scope_var, scope_params, and scope_funcs.
fn register_class_name(
  p: P,
  name: String,
  name_pos: Int,
) -> Result(P, ParseError) {
  use <- bool.guard(
    set.contains(p.scope_lexical, name)
      || set.contains(p.scope_var, name)
      || set.contains(p.scope_params, name)
      || set.contains(p.scope_funcs, name),
    Error(IdentifierAlreadyDeclared(name, name_pos)),
  )
  Ok(P(..p, scope_lexical: set.insert(p.scope_lexical, name)))
}

/// Check if an export name is a duplicate. Only applies in Module mode.
/// Returns Ok(p) with the name added to export_names, or Error if duplicate.
fn check_duplicate_export(p: P, name: String) -> Result(P, ParseError) {
  case p.mode {
    Module ->
      case set.contains(p.export_names, name) {
        True -> Error(DuplicateExport(name, pos_of(p)))
        False -> Ok(P(..p, export_names: set.insert(p.export_names, name)))
      }
    Script -> Ok(p)
  }
}

/// Check if an import binding name is a duplicate. Only applies in Module mode.
/// Returns Ok(p) with the name added to import_bindings, or Error if duplicate.
fn check_duplicate_import_binding(p: P, name: String) -> Result(P, ParseError) {
  case p.mode {
    Module ->
      case set.contains(p.import_bindings, name) {
        True -> Error(DuplicateImportBinding(name, pos_of(p)))
        False ->
          Ok(P(..p, import_bindings: set.insert(p.import_bindings, name)))
      }
    Script -> Ok(p)
  }
}

/// Check if a token kind is a reserved word that can NEVER be a binding identifier.
fn is_reserved_word_kind(kind: TokenKind) -> Bool {
  case kind {
    Break
    | Case
    | Catch
    | Class
    | Const
    | Continue
    | Debugger
    | Default
    | Delete
    | Do
    | Else
    | Export
    | Extends
    | Finally
    | For
    | Function
    | If
    | Import
    | In
    | Instanceof
    | New
    | Return
    | Super
    | Switch
    | This
    | Throw
    | Try
    | Typeof
    | Var
    | Void
    | While
    | With
    | Null
    | KTrue
    | KFalse -> True
    _ -> False
  }
}

/// Validate that the local binding name in an import specifier is not a
/// reserved word. The imported name (LHS of 'as') can be any IdentifierName
/// including keywords, but the local binding must be a valid BindingIdentifier.
fn check_import_binding_name(
  p: P,
  binding_name: String,
  binding_token_kind: TokenKind,
) -> Result(Nil, ParseError) {
  case is_reserved_word_kind(binding_token_kind) {
    True -> Error(ReservedWordImportBinding(binding_name, pos_of(p)))
    False -> check_binding_identifier(p, binding_name)
  }
}

/// When inside an export var/let/const declaration (in_export_decl is True),
/// register the binding name as an exported name and check for duplicates.
fn check_export_binding(p: P, name: String) -> Result(P, ParseError) {
  case p.in_export_decl {
    True -> check_duplicate_export(p, name)
    False -> Ok(p)
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
      // Optional default value
      let #(p3, final_pat) = case peek(p2) {
        Equal ->
          case parse_assignment_expression(advance(p2)) {
            Ok(#(p_val, default_expr)) -> #(
              p_val,
              ast.AssignmentPattern(left: pat, right: default_expr),
            )
            Error(_) -> #(p2, pat)
          }
        _ -> #(p2, pat)
      }
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
      use #(p3, inner_pat) <- result.try(parse_binding_pattern(p2))
      let rest = ast.RestProperty(argument: inner_pat)
      let p4 = case peek(p3) {
        Comma -> advance(p3)
        _ -> p3
      }
      use p5 <- result.try(expect(p4, RightBrace))
      Ok(#(p5, ast.ObjectPattern(properties: list.reverse([rest, ..acc]))))
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
  use #(p2, key_expr, is_computed) <- result.try(parse_property_name(p))
  case peek(p2) {
    Colon -> {
      // property: pattern
      use #(p4, val_pat) <- result.try(parse_binding_pattern(advance(p2)))
      // Optional default value
      let #(p5, final_pat) = case peek(p4) {
        Equal ->
          case parse_assignment_expression(advance(p4)) {
            Ok(#(p_val, default_expr)) -> #(
              p_val,
              ast.AssignmentPattern(left: val_pat, right: default_expr),
            )
            Error(_) -> #(p4, val_pat)
          }
        _ -> #(p4, val_pat)
      }
      Ok(#(
        p5,
        ast.PatternProperty(
          key: key_expr,
          value: final_pat,
          computed: is_computed,
          shorthand: False,
        ),
      ))
    }
    Equal -> {
      // shorthand with default — validate the name as a binding identifier
      case is_valid_shorthand {
        False ->
          Error(UnexpectedToken(token_kind_to_string(prop_kind), pos_of(p)))
        True -> {
          use #(p2c, _) <- result.try(validate_and_register_binding_no_advance(
            p,
            p2,
            prop_name,
          ))
          {
            use #(p3, default_expr) <- result.map(
              parse_assignment_expression(advance(p2c)),
            )
            #(
              p3,
              ast.PatternProperty(
                key: key_expr,
                value: ast.AssignmentPattern(
                  left: ast.IdentifierPattern(name: prop_name),
                  right: default_expr,
                ),
                computed: False,
                shorthand: True,
              ),
            )
          }
        }
      }
    }
    _ -> {
      // shorthand binding — validate the name as a binding identifier
      case is_valid_shorthand {
        False ->
          Error(UnexpectedToken(token_kind_to_string(prop_kind), pos_of(p)))
        True -> {
          use #(p3, _) <- result.try(validate_and_register_binding_no_advance(
            p,
            p2,
            prop_name,
          ))
          Ok(#(
            p3,
            ast.PatternProperty(
              key: key_expr,
              value: ast.IdentifierPattern(name: prop_name),
              computed: False,
              shorthand: True,
            ),
          ))
        }
      }
    }
  }
}

/// Parse a property name and return (parser, key_expression, is_computed).
fn parse_property_name(p: P) -> Result(#(P, ast.Expression, Bool), ParseError) {
  case peek(p) {
    Identifier -> {
      let name = peek_value(p)
      Ok(#(advance(p), ast.Identifier(name: name), False))
    }
    Number -> {
      use <- bool.guard(
        p.strict && is_legacy_octal_number(peek_value(p)),
        Error(OctalLiteralStrictMode(pos_of(p))),
      )
      let raw = peek_value(p)
      Ok(#(advance(p), ast.NumberLiteral(value: parse_js_number(raw)), False))
    }
    KString -> {
      use <- bool.guard(
        p.strict && has_legacy_octal_escape(peek_value(p)),
        Error(OctalEscapeStrictMode(pos_of(p))),
      )
      Ok(#(advance(p), ast.StringExpression(value: peek_value(p)), False))
    }
    LeftBracket -> {
      let p2 = advance(p)
      use #(p3, expr) <- result.try(parse_assignment_expression(p2))
      use p4 <- result.try(expect(p3, RightBracket))
      Ok(#(p4, expr, True))
    }
    // Keywords that can be used as property names
    _ ->
      case is_identifier_or_keyword(peek(p)) {
        True -> {
          let name = peek_value(p)
          Ok(#(advance(p), ast.Identifier(name: name), False))
        }
        False -> Error(ExpectedPropertyName(pos_of(p)))
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
  let p5 = P(..p5, loop_depth: p5.loop_depth + 1)
  use #(p6, body) <- result.try(parse_single_statement(p5, False))
  Ok(#(P(..p6, loop_depth: p.loop_depth), ast.WhileStatement(condition:, body:)))
}

fn parse_do_while_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  let p2 = P(..p2, loop_depth: p2.loop_depth + 1)
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
    P(..p8, loop_depth: p.loop_depth),
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
  let p2 = P(..p2, loop_depth: p2.loop_depth + 1)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, stmt) <- result.try(parse_for_head(p3, is_await))
  Ok(#(P(..p4, loop_depth: p.loop_depth), stmt))
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
    _ -> parse_for_expression(p, is_await)
  }
}

/// Wrapper around parse_for_declaration that creates a sub-scope for
/// let/const declarations. The declaration names live in this sub-scope,
/// not the enclosing block, so `for(let a;;); let a;` is valid.
fn parse_for_declaration_scoped(
  p: P,
  is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  // Create a sub-scope: for-loop let/const names don't conflict with enclosing block
  let p2 =
    P(
      ..p,
      scope_lexical: set.new(),
      scope_var: set.new(),
      scope_funcs: set.new(),
      outer_lexical: p.scope_lexical
        |> set.union(p.scope_funcs)
        |> set.union(p.outer_lexical),
      in_block: True,
    )
  // After parsing, restore the original scope from p (immutable -- no saving needed)
  use #(p3, stmt) <- result.map(parse_for_declaration(p2, is_await))
  #(
    P(
      ..p3,
      scope_lexical: p.scope_lexical,
      scope_var: p.scope_var,
      scope_funcs: p.scope_funcs,
      outer_lexical: p.outer_lexical,
      in_block: p.in_block,
      in_lexical_decl: p.in_lexical_decl,
      decl_bound_names: p.decl_bound_names,
      binding_kind: p.binding_kind,
    ),
    stmt,
  )
}

fn parse_for_declaration(
  p: P,
  is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  let kind = peek(p)
  let is_lexical = kind == Let || kind == Const
  let p2 = advance(p)
  let is_destr = peek(p2) == LeftBrace || peek(p2) == LeftBracket
  let p2 = case is_lexical {
    True ->
      P(
        ..p2,
        in_lexical_decl: True,
        decl_bound_names: set.new(),
        binding_kind: BindingLexical,
      )
    False -> P(..p2, binding_kind: BindingVar)
  }
  let var_kind = case kind {
    Var -> ast.Var
    Let -> ast.Let
    Const -> ast.Const
    _ -> ast.Var
  }
  let pre_scope_var = p2.scope_var
  use #(p3, pattern) <- result.try(parse_for_binding_or_declarator(p2))
  case peek(p3) {
    In -> {
      let decl =
        ast.ForInitDeclaration(
          ast.VariableDeclaration(kind: var_kind, declarations: [
            ast.VariableDeclarator(id: pattern, init: None),
          ]),
        )
      parse_for_in_rest(p3, decl)
    }
    Of -> {
      // B.3.4: for-of var bindings must not shadow catch parameters
      use Nil <- result.try(case kind {
        Var ->
          check_new_vars_vs_params(
            p3.scope_var,
            pre_scope_var,
            p3.scope_params,
            pos_of(p3),
          )
        _ -> Ok(Nil)
      })
      let decl =
        ast.ForInitDeclaration(
          ast.VariableDeclaration(kind: var_kind, declarations: [
            ast.VariableDeclarator(id: pattern, init: None),
          ]),
        )
      parse_for_of_rest(p3, decl, is_await)
    }
    Semicolon ->
      case kind {
        Const -> Error(MissingConstInitializer(pos_of(p3)))
        _ ->
          case is_destr {
            True -> Error(DestructuringMissingInitializer(pos_of(p3)))
            False -> {
              let first = ast.VariableDeclarator(id: pattern, init: None)
              let #(p4, rest) = parse_remaining_declarators(p3, kind, [])
              let decl =
                ast.ForInitDeclaration(
                  ast.VariableDeclaration(kind: var_kind, declarations: [
                    first,
                    ..rest
                  ]),
                )
              parse_for_classic_rest(advance(p4), Some(decl))
            }
          }
      }
    Comma ->
      case kind {
        Const -> Error(MissingConstInitializer(pos_of(p3)))
        _ ->
          case is_destr {
            True -> Error(DestructuringMissingInitializer(pos_of(p3)))
            False -> {
              let first = ast.VariableDeclarator(id: pattern, init: None)
              let #(p4, rest) = parse_remaining_declarators(p3, kind, [])
              let decl =
                ast.ForInitDeclaration(
                  ast.VariableDeclaration(kind: var_kind, declarations: [
                    first,
                    ..rest
                  ]),
                )
              use p5 <- result.try(expect(p4, Semicolon))
              parse_for_classic_rest(p5, Some(decl))
            }
          }
      }
    Equal -> {
      let p4 = advance(p3)
      // Disable 'in' as binary op in initializer so for(var x = a in b)
      // is for-in, not a binary expression
      let p4_no_in = P(..p4, allow_in: False)
      use #(p5, init_expr) <- result.try(parse_assignment_expression(p4_no_in))
      let p5 = P(..p5, allow_in: p.allow_in)
      case peek(p5) {
        In ->
          // for-in with initializer: always forbidden
          Error(ForInInitializer(pos_of(p5)))
        Of -> Error(ForOfInitializer(pos_of(p5)))
        Semicolon -> {
          let first = ast.VariableDeclarator(id: pattern, init: Some(init_expr))
          let #(p6, rest) = parse_remaining_declarators(p5, kind, [])
          let decl =
            ast.ForInitDeclaration(
              ast.VariableDeclaration(kind: var_kind, declarations: [
                first,
                ..rest
              ]),
            )
          parse_for_classic_rest(advance(p6), Some(decl))
        }
        Comma -> {
          let first = ast.VariableDeclarator(id: pattern, init: Some(init_expr))
          let #(p6, rest) = parse_remaining_declarators(p5, kind, [])
          let decl =
            ast.ForInitDeclaration(
              ast.VariableDeclaration(kind: var_kind, declarations: [
                first,
                ..rest
              ]),
            )
          use p7 <- result.try(expect(p6, Semicolon))
          parse_for_classic_rest(p7, Some(decl))
        }
        _ -> Error(ExpectedForHeadSeparator(pos_of(p5)))
      }
    }
    _ -> Error(ExpectedForDeclSeparator(pos_of(p3)))
  }
}

fn parse_for_expression(
  p: P,
  is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  // Save starting token to detect bare destructuring patterns
  let start_token = peek(p)
  // Disable 'in' as binary operator inside for-head so that
  // for (x in obj) is parsed as for-in, not binary expression
  let p_no_in = P(..p, allow_in: False)
  use #(p2, expr) <- result.try(parse_expression(p_no_in))
  {
    // Restore allow_in for the rest of the for statement
    let p2 = P(..p2, allow_in: p.allow_in)
    case peek(p2) {
      Semicolon ->
        case p2.has_cover_initializer {
          True -> Error(InvalidDestructuringTarget(pos_of(p2)))
          False ->
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
          }
        {
          True ->
            case peek(p2) {
              In -> parse_for_in_rest(p2, left)
              _ -> parse_for_of_rest(p2, left, is_await)
            }
          False -> {
            let kind = case peek(p2) {
              In -> "in"
              _ -> "of"
            }
            Error(InvalidForInOfLhs(kind, pos_of(p2)))
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

fn parse_remaining_declarators(
  p: P,
  kind: TokenKind,
  acc: List(ast.VariableDeclarator),
) -> #(P, List(ast.VariableDeclarator)) {
  case peek(p) {
    Comma -> {
      let p2 = advance(p)
      case parse_variable_declarator(p2, kind) {
        Ok(#(p3, decl)) -> parse_remaining_declarators(p3, kind, [decl, ..acc])
        Error(_) -> #(p, list.reverse(acc))
      }
    }
    _ -> #(p, list.reverse(acc))
  }
}

fn parse_for_in_rest(
  p: P,
  left: ast.ForInit,
) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  use #(p3, right) <- result.try(parse_expression(p2))
  use p4 <- result.try(expect(p3, RightParen))
  use #(p5, body) <- result.try(parse_single_statement(p4, False))
  Ok(#(p5, ast.ForInStatement(left:, right:, body:)))
}

fn parse_for_of_rest(
  p: P,
  left: ast.ForInit,
  is_await: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  use #(p3, right) <- result.try(parse_assignment_expression(p2))
  use p4 <- result.try(expect(p3, RightParen))
  use #(p5, body) <- result.try(parse_single_statement(p4, False))
  Ok(#(p5, ast.ForOfStatement(left:, right:, body:, is_await:)))
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
  case p.function_depth > 0 {
    False -> Error(ReturnOutsideFunction(pos_of(p)))
    True -> parse_return_statement_body(p)
  }
}

fn parse_return_statement_body(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  case peek(p2) {
    Semicolon -> Ok(#(advance(p2), ast.ReturnStatement(argument: option.None)))
    RightBrace | Eof -> Ok(#(p2, ast.ReturnStatement(argument: option.None)))
    _ ->
      case has_line_break_before(p2) {
        True -> Ok(#(p2, ast.ReturnStatement(argument: option.None)))
        False -> {
          use #(p3, expr) <- result.try(parse_expression(p2))
          use p4 <- result.try(eat_semicolon(p3))
          Ok(#(p4, ast.ReturnStatement(argument: option.Some(expr))))
        }
      }
  }
}

fn parse_break_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  case peek(p2) {
    Semicolon -> {
      case p.loop_depth > 0 || p.switch_depth > 0 {
        False -> Error(BreakOutsideLoopOrSwitch(pos_of(p)))
        True -> Ok(#(advance(p2), ast.BreakStatement(label: option.None)))
      }
    }
    Identifier ->
      case has_line_break_before(p2) {
        True ->
          case p.loop_depth > 0 || p.switch_depth > 0 {
            False -> Error(BreakOutsideLoopOrSwitch(pos_of(p)))
            True -> Ok(#(p2, ast.BreakStatement(label: option.None)))
          }
        False -> {
          let label = peek_value(p2)
          case find_label(p.label_set, label) {
            True -> {
              use p3 <- result.try(eat_semicolon(advance(p2)))
              Ok(#(p3, ast.BreakStatement(label: option.Some(label))))
            }
            False -> Error(UndefinedLabel(label, pos_of(p2)))
          }
        }
      }
    _ ->
      case p.loop_depth > 0 || p.switch_depth > 0 {
        False -> Error(BreakOutsideLoopOrSwitch(pos_of(p)))
        True -> {
          use p3 <- result.try(eat_semicolon(p2))
          Ok(#(p3, ast.BreakStatement(label: option.None)))
        }
      }
  }
}

fn parse_continue_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  case p.loop_depth > 0 {
    False -> Error(ContinueOutsideLoop(pos_of(p)))
    True -> {
      let p2 = advance(p)
      case peek(p2) {
        Semicolon ->
          Ok(#(advance(p2), ast.ContinueStatement(label: option.None)))
        Identifier ->
          case has_line_break_before(p2) {
            True -> Ok(#(p2, ast.ContinueStatement(label: option.None)))
            False -> {
              let label = peek_value(p2)
              case find_label(p.label_set, label) {
                True -> {
                  use p3 <- result.try(eat_semicolon(advance(p2)))
                  Ok(#(p3, ast.ContinueStatement(label: option.Some(label))))
                }
                False -> Error(UndefinedLabel(label, pos_of(p2)))
              }
            }
          }
        _ -> {
          use p3 <- result.try(eat_semicolon(p2))
          Ok(#(p3, ast.ContinueStatement(label: option.None)))
        }
      }
    }
  }
}

fn parse_throw_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  // throw must have expression on same line (no line terminator after throw)
  use <- bool.guard(
    has_line_break_before(p2),
    Error(ThrowLineBreak(pos_of(p2))),
  )
  use #(p3, expr) <- result.try(parse_expression(p2))
  use p4 <- result.try(eat_semicolon(p3))
  Ok(#(p4, ast.ThrowStatement(argument: expr)))
}

fn parse_try_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  use #(p3, try_block) <- result.try(parse_block_statement(p2))
  {
    let has_catch = peek(p3) == Catch
    let #(p4, handler) = case peek(p3) {
      Catch -> {
        let p4 = advance(p3)
        case peek(p4) {
          LeftParen -> {
            let p5 = advance(p4)
            // Parse catch param as lexical binding in a new scope
            let saved_scope_lexical = p5.scope_lexical
            let saved_scope_var = p5.scope_var
            let saved_scope_funcs = p5.scope_funcs
            let saved_scope_params = p5.scope_params
            let saved_outer_lexical = p5.outer_lexical
            let saved_binding_kind = p5.binding_kind
            let p5 =
              P(
                ..p5,
                // Use BindingParam so catch param goes into scope_params,
                // which allows var redeclaration but blocks let/const redeclaration
                binding_kind: BindingParam,
                scope_lexical: set.new(),
                scope_var: set.new(),
                scope_funcs: set.new(),
                scope_params: set.new(),
                outer_lexical: p5.scope_lexical
                  |> set.union(p5.scope_funcs)
                  |> set.union(p5.outer_lexical),
                in_block: True,
                // Enable dup param detection for catch destructured bindings
                in_formal_params: True,
                param_bound_names: [],
                has_non_simple_param: True,
              )
            case
              {
                use #(p6, catch_param) <- result.try(parse_binding_pattern(p5))
                use p7 <- result.try(expect(
                  P(..p6, binding_kind: BindingNone, in_formal_params: False),
                  RightParen,
                ))
                // Use function_body_block to NOT create another scope
                {
                  use #(p8, catch_body) <- result.map(parse_function_body_block(
                    p7,
                  ))
                  #(
                    P(
                      ..p8,
                      scope_lexical: saved_scope_lexical,
                      scope_var: saved_scope_var,
                      scope_funcs: saved_scope_funcs,
                      scope_params: saved_scope_params,
                      outer_lexical: saved_outer_lexical,
                      binding_kind: saved_binding_kind,
                      in_block: p.in_block,
                    ),
                    option.Some(ast.CatchClause(
                      param: option.Some(catch_param),
                      body: catch_body,
                    )),
                  )
                }
              }
            {
              Ok(catch_result) -> catch_result
              Error(_) -> #(p4, option.None)
            }
          }
          LeftBrace -> {
            // catch without binding
            case parse_block_statement(p4) {
              Ok(#(p_val, catch_body)) -> #(
                p_val,
                option.Some(ast.CatchClause(
                  param: option.None,
                  body: catch_body,
                )),
              )
              Error(_) -> #(p4, option.None)
            }
          }
          _ -> #(p4, option.None)
        }
      }
      _ -> #(p3, option.None)
    }
    case peek(p4) {
      Finally -> {
        let p5 = advance(p4)
        use #(p6, finally_block) <- result.try(parse_block_statement(p5))
        Ok(#(
          p6,
          ast.TryStatement(
            block: try_block,
            handler: handler,
            finalizer: option.Some(finally_block),
          ),
        ))
      }
      _ ->
        case has_catch {
          True ->
            Ok(#(
              p4,
              ast.TryStatement(
                block: try_block,
                handler: handler,
                finalizer: option.None,
              ),
            ))
          False -> Error(MissingCatchOrFinally(pos_of(p4)))
        }
    }
  }
}

fn parse_switch_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, discriminant) <- result.try(parse_expression(p3))
  use p5 <- result.try(expect(p4, RightParen))
  use p6 <- result.try(expect(p5, LeftBrace))
  {
    // Enter switch scope: all cases share one scope
    let saved_scope_lexical = p6.scope_lexical
    let saved_scope_var = p6.scope_var
    let saved_scope_funcs = p6.scope_funcs
    let saved_outer_lexical = p6.outer_lexical
    let p6 =
      P(
        ..p6,
        switch_depth: p6.switch_depth + 1,
        scope_lexical: set.new(),
        scope_var: set.new(),
        scope_funcs: set.new(),
        outer_lexical: p6.scope_lexical
          |> set.union(p6.scope_funcs)
          |> set.union(p6.outer_lexical),
        in_block: True,
      )
    use #(p7, cases) <- result.try(parse_switch_cases(p6, False, []))
    Ok(#(
      P(
        ..p7,
        switch_depth: p.switch_depth,
        scope_lexical: saved_scope_lexical,
        scope_var: saved_scope_var,
        scope_funcs: saved_scope_funcs,
        outer_lexical: saved_outer_lexical,
        in_block: p.in_block,
      ),
      ast.SwitchStatement(
        discriminant: discriminant,
        cases: list.reverse(cases),
      ),
    ))
  }
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
      use #(p3, condition) <- result.try(parse_expression(p2))
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
  stmt_acc: List(ast.Statement),
  case_acc: List(ast.SwitchCase),
) -> Result(#(P, List(ast.SwitchCase)), ParseError) {
  case peek(p) {
    RightBrace | Case | Default -> {
      let case_node =
        ast.SwitchCase(condition: condition, consequent: list.reverse(stmt_acc))
      parse_switch_cases(p, has_default, [case_node, ..case_acc])
    }
    _ -> {
      use #(p2, stmt) <- result.try(parse_statement(p))
      parse_switch_case_stmts(
        p2,
        has_default,
        condition,
        [stmt, ..stmt_acc],
        case_acc,
      )
    }
  }
}

fn parse_function_declaration(p: P) -> Result(#(P, ast.Statement), ParseError) {
  parse_function_decl_impl(p, True, False)
}

fn parse_function_declaration_optional_name(
  p: P,
) -> Result(#(P, ast.Statement), ParseError) {
  parse_function_decl_impl(p, False, False)
}

fn parse_function_decl_impl(
  p: P,
  name_required: Bool,
  is_async: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = case is_async {
    True -> advance(advance(p))
    False -> advance(p)
  }
  // Optional generator marker
  let is_generator = peek(p2) == Star
  let p3 = case is_generator {
    True -> advance(p2)
    False -> p2
  }
  // Get the function name (if any) to register in scope
  let func_name = get_simple_binding_name(p3)
  // Function declarations require a name (unless export default)
  case func_name == "" && name_required {
    True -> Error(ExpectedIdentifier(pos_of(p3)))
    False -> parse_function_decl_body(p, p3, func_name, is_generator, is_async)
  }
}

fn parse_function_decl_body(
  p_outer: P,
  p3: P,
  func_name: String,
  is_generator: Bool,
  is_async: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  // Optional name (identifier or contextual keyword like yield, await, let)
  use p4 <- result.try(eat_optional_name(p3))
  // Store function name for retroactive strict mode validation
  let p4 = P(..p4, pending_strict_name: string.to_option(func_name))
  use #(p5, params, body) <- result.try(
    parse_function_params_and_body(enter_function_context(
      p4,
      is_generator,
      is_async,
    ))
    |> restore_context_fn(p_outer),
  )
  // Register function name in outer scope
  let p6 = case func_name {
    "" -> Ok(p5)
    name -> register_function_name(p5, name, pos_of(p3))
  }
  use p7 <- result.try(p6)
  let name_opt = case func_name {
    "" -> None
    n -> Some(n)
  }
  Ok(#(
    p7,
    ast.FunctionDeclaration(
      name: name_opt,
      params: params,
      body: body,
      is_generator: is_generator,
      is_async: is_async,
    ),
  ))
}

fn parse_async_function_declaration(
  p: P,
) -> Result(#(P, ast.Statement), ParseError) {
  parse_function_decl_impl(p, True, True)
}

fn parse_async_function_declaration_optional_name(
  p: P,
) -> Result(#(P, ast.Statement), ParseError) {
  parse_function_decl_impl(p, False, True)
}

fn parse_function_params_and_body(
  p: P,
) -> Result(#(P, List(ast.Pattern), ast.Statement), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  // Parse params with BindingParam so names go into scope_lexical
  let p2 =
    P(
      ..p2,
      in_formal_params: True,
      param_bound_names: [],
      has_non_simple_param: False,
      binding_kind: BindingParam,
    )
  use #(p3, params) <- result.try(parse_formal_parameters(p2))
  let p3 = P(..p3, in_formal_params: False, binding_kind: BindingNone)
  use p4 <- result.try(expect(p3, RightParen))
  // Check for "use strict" directive at start of function body.
  // If transitioning to strict, retroactively check for dup params.
  let was_strict = p4.strict
  use p5 <- result.try(check_use_strict_in_body(p4))
  use #(p6, body) <- result.try(case !was_strict && p5.strict {
    True -> {
      use Nil <- result.try(check_pending_strict_names(p5))
      use Nil <- result.try(check_param_names_for_dups(p5))
      parse_function_body_block(p5)
    }
    False -> parse_function_body_block(p5)
  })
  Ok(#(p6, params, body))
}

/// Parse a function body block WITHOUT creating a new block scope.
/// The function's params are already in scope_lexical, so the body
/// shares the same scope as the params.
fn parse_function_body_block(p: P) -> Result(#(P, ast.Statement), ParseError) {
  use p2 <- result.try(expect(p, LeftBrace))
  use #(p3, stmts) <- result.try(parse_statement_list(p2, False, []))
  use p4 <- result.try(expect(p3, RightBrace))
  Ok(#(p4, ast.BlockStatement(body: stmts)))
}

/// Retroactively check pending strict names (e.g. function name) when
/// "use strict" is discovered in function body. Catches cases like
/// `function eval() { 'use strict'; }`.
fn check_pending_strict_names(p: P) -> Result(Nil, ParseError) {
  case p.pending_strict_name {
    None -> Ok(Nil)
    Some(name) ->
      case is_strict_binding_error(name) {
        True -> Error(StrictModeParamName(name, pos_of(p)))
        False -> Ok(Nil)
      }
  }
}

/// Retroactively check param names when "use strict" is discovered in body.
/// This catches cases like `function a(yield){ 'use strict'; }` where the
/// param was accepted before strict mode was activated.
fn check_param_names_for_dups(p: P) -> Result(Nil, ParseError) {
  check_param_names_list(p, p.param_bound_names, set.new())
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
      case is_strict_binding_error(name) {
        True -> Error(StrictModeParamName(name, pos_of(p)))
        False ->
          // Check for duplicates
          case set.contains(seen, name) {
            True -> Error(DuplicateParamNameStrictMode(name, pos_of(p)))
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
  let p = P(..p, has_non_simple_param: True)
  // Retroactively check accumulated names for duplicates
  use Nil <- result.try(check_param_names_for_dups_only(p))
  Ok(p)
}

/// Check param_bound_names for duplicate names only (no strict binding checks).
/// Used when transitioning to non-simple params to retroactively detect
/// duplicates like function(a, a, [b]){}.
fn check_param_names_for_dups_only(p: P) -> Result(Nil, ParseError) {
  check_names_for_dups_loop(p, p.param_bound_names, set.new())
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
        True -> Error(DuplicateParameterName(name, pos_of(p)))
        False -> check_names_for_dups_loop(p, rest, set.insert(seen, name))
      }
  }
}

/// Check if a name is forbidden as a binding in strict mode.
fn is_strict_binding_error(name: String) -> Bool {
  case name {
    "eval"
    | "arguments"
    | "yield"
    | "implements"
    | "interface"
    | "package"
    | "private"
    | "protected"
    | "public"
    | "static"
    | "let" -> True
    _ -> False
  }
}

/// Getter must have exactly 0 parameters
fn parse_getter_params_and_body(
  p: P,
) -> Result(#(P, List(ast.Pattern), ast.Statement), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  case peek(p2) {
    RightParen -> {
      let p3 = advance(p2)
      // Check for "use strict" directive in getter body
      use p3 <- result.try(check_use_strict_in_body(p3))
      use #(p4, body) <- result.try(parse_function_body_block(p3))
      Ok(#(p4, [], body))
    }
    _ -> Error(GetterNoParams(pos_of(p2)))
  }
}

/// Setter must have exactly 1 simple parameter (no rest)
fn parse_setter_params_and_body(
  p: P,
) -> Result(#(P, List(ast.Pattern), ast.Statement), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  let p2 =
    P(
      ..p2,
      in_formal_params: True,
      param_bound_names: [],
      has_non_simple_param: False,
      binding_kind: BindingParam,
    )
  case peek(p2) {
    RightParen -> Error(SetterExactlyOneParam(pos_of(p2)))
    DotDotDot -> Error(SetterNoRest(pos_of(p2)))
    _ -> {
      // Check if param is destructured (non-simple)
      let param_name = get_simple_binding_name(p2)
      let p2 = case param_name == "" {
        True -> P(..p2, has_non_simple_param: True)
        False -> p2
      }
      use #(p3, pat) <- result.try(parse_binding_pattern(p2))
      // Optional default value
      let #(p4, final_pat) = case peek(p3) {
        Equal -> {
          // Default value makes params non-simple
          let p3 = P(..p3, has_non_simple_param: True)
          case parse_assignment_expression(advance(p3)) {
            Ok(#(p_val, default_expr)) -> #(
              p_val,
              ast.AssignmentPattern(left: pat, right: default_expr),
            )
            Error(_) -> #(p3, pat)
          }
        }
        _ -> #(p3, pat)
      }
      case peek(p4) {
        RightParen -> {
          let p5 =
            P(..advance(p4), in_formal_params: False, binding_kind: BindingNone)
          // Check for "use strict" directive
          use p5 <- result.try(check_use_strict_in_body(p5))
          use #(p6, body) <- result.try(parse_function_body_block(p5))
          Ok(#(p6, [final_pat], body))
        }
        Comma -> Error(SetterExactlyOneParam(pos_of(p4)))
        _ -> Error(ExpectedCloseAfterSetter(pos_of(p4)))
      }
    }
  }
}

fn parse_formal_parameters(p: P) -> Result(#(P, List(ast.Pattern)), ParseError) {
  case peek(p) {
    RightParen -> Ok(#(p, []))
    _ -> parse_formal_parameter_list(p, set.new(), [])
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
      let p = case is_non_simple && !p.has_non_simple_param {
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
      let p2 = case !p2.has_non_simple_param {
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
  let #(p2, final_pat) = case parse_assignment_expression(advance(p)) {
    Ok(#(p_val, default_expr)) -> #(
      p_val,
      ast.AssignmentPattern(left: pat, right: default_expr),
    )
    Error(_) -> #(p, pat)
  }
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
    p.strict || p.in_arrow_params || p.in_method || p.has_non_simple_param
  use <- bool.guard(
    must_be_unique && set.contains(seen, name),
    Error(DuplicateParameterName(name, pos_of(p))),
  )
  Ok(Nil)
}

/// Check if any newly-added scope_var entries (compared to saved set) conflict
/// with scope_params. Used for B.3.4: for-of var bindings must not shadow catch params.
fn check_new_vars_vs_params(
  current_vars: Set(String),
  saved_vars: Set(String),
  params: Set(String),
  pos: Int,
) -> Result(Nil, ParseError) {
  // Names added since the save point that also appear in params
  let conflicts =
    set.difference(current_vars, saved_vars)
    |> set.intersection(params)
    |> set.to_list
  case conflicts {
    [] -> Ok(Nil)
    [name, ..] -> Error(IdentifierAlreadyDeclared(name, pos))
  }
}

fn parse_class_declaration(p: P) -> Result(#(P, ast.Statement), ParseError) {
  parse_class_decl_impl(p, True)
}

fn parse_class_declaration_optional_name(
  p: P,
) -> Result(#(P, ast.Statement), ParseError) {
  parse_class_decl_impl(p, False)
}

fn parse_class_decl_impl(
  p: P,
  name_required: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  let is_name = peek(p2) == Identifier || is_contextual_keyword(peek(p2))
  case is_name {
    True -> {
      let name = peek_value(p2)
      use Nil <- result.try(check_binding_identifier(
        P(..p2, strict: True),
        name,
      ))
      use p3 <- result.try(register_class_name(p2, name, pos_of(p2)))
      use #(p4, super_class, body) <- result.try(parse_class_tail(advance(p3)))
      Ok(#(
        p4,
        ast.ClassDeclaration(
          name: Some(name),
          super_class: super_class,
          body: body,
        ),
      ))
    }
    False -> {
      use <- bool.guard(name_required, Error(ExpectedIdentifier(pos_of(p2))))
      use #(p3, super_class, body) <- result.try(parse_class_tail(p2))
      Ok(#(
        p3,
        ast.ClassDeclaration(name: None, super_class: super_class, body: body),
      ))
    }
  }
}

fn parse_class_tail(
  p: P,
) -> Result(#(P, Option(ast.Expression), List(ast.ClassElement)), ParseError) {
  // Class bodies (and extends clause) are always strict
  let saved_strict = p.strict
  let p = P(..p, strict: True)
  // Optional extends
  let has_extends = peek(p) == Extends
  let #(p2, super_class) = case has_extends {
    True -> {
      let p2 = advance(p)
      case parse_left_hand_side_expression(p2) {
        Ok(#(p_val, expr)) -> #(p_val, Some(expr))
        Error(_) -> #(p2, None)
      }
    }
    False -> #(p, None)
  }
  use p3 <- result.try(expect(p2, LeftBrace))
  use #(p4, elements) <- result.try(
    parse_class_body(p3, has_extends, False, []),
  )
  Ok(#(P(..p4, strict: saved_strict), super_class, list.reverse(elements)))
}

fn parse_class_body(
  p: P,
  has_extends: Bool,
  has_constructor: Bool,
  acc: List(ast.ClassElement),
) -> Result(#(P, List(ast.ClassElement)), ParseError) {
  case peek(p) {
    RightBrace -> Ok(#(advance(p), acc))
    Semicolon -> parse_class_body(advance(p), has_extends, has_constructor, acc)
    _ -> {
      use #(p2, found_constructor, element) <- result.try(parse_class_element(
        p,
        has_extends,
        has_constructor,
      ))
      parse_class_body(p2, has_extends, has_constructor || found_constructor, [
        element,
        ..acc
      ])
    }
  }
}

fn parse_class_element(
  p: P,
  has_extends: Bool,
  has_constructor: Bool,
) -> Result(#(P, Bool, ast.ClassElement), ParseError) {
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
  // Skip async keyword — track if method is async
  let is_method_async = case peek(p2) {
    Async ->
      case peek_at(p2, 1) {
        LeftParen | Equal | Semicolon | RightBrace -> False
        _ -> True
      }
    _ -> False
  }
  let p3 = case is_method_async {
    True -> advance(p2)
    False -> p2
  }
  // Check for getter/setter — track kind for param validation
  let class_accessor_kind = case peek(p3) {
    Identifier -> {
      let val = peek_value(p3)
      case val {
        "get" ->
          case peek_at(p3, 1) {
            LeftParen | Equal | Semicolon | RightBrace -> ""
            _ -> "get"
          }
        "set" ->
          case peek_at(p3, 1) {
            LeftParen | Equal | Semicolon | RightBrace -> ""
            _ -> "set"
          }
        _ -> ""
      }
    }
    _ -> ""
  }
  let p4 = case class_accessor_kind {
    "get" | "set" -> advance(p3)
    _ -> p3
  }
  // Generator marker
  let is_generator = peek(p4) == Star
  let p5 = case is_generator {
    True -> advance(p4)
    False -> p4
  }
  // Check if the property name is "constructor" (non-static only)
  let is_named_constructor = case is_static {
    True -> False
    False ->
      case peek(p5) {
        Identifier ->
          case peek_value(p5) {
            "constructor" -> True
            _ -> False
          }
        KString ->
          case peek_value(p5) {
            "constructor" -> True
            _ -> False
          }
        _ -> False
      }
  }
  // Validate constructor constraints
  case is_named_constructor {
    True -> {
      case class_accessor_kind {
        "get" -> Error(ClassConstructorNotGetter(pos_of(p5)))
        "set" -> Error(ClassConstructorNotSetter(pos_of(p5)))
        _ -> {
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
          parse_class_element_body(
            p,
            p5,
            has_extends,
            is_method_async,
            is_generator,
            class_accessor_kind,
            True,
            is_static,
          )
        }
      }
    }
    False -> {
      // Check for static prototype (forbidden)
      let is_static_prototype = case is_static {
        False -> False
        True ->
          case peek(p5) {
            Identifier ->
              case peek_value(p5) {
                "prototype" -> True
                _ -> False
              }
            KString ->
              case peek_value(p5) {
                "prototype" -> True
                _ -> False
              }
            _ -> False
          }
      }
      use <- bool.guard(is_static_prototype, Error(StaticPrototype(pos_of(p5))))
      parse_class_element_body(
        p,
        p5,
        has_extends,
        is_method_async,
        is_generator,
        class_accessor_kind,
        False,
        is_static,
      )
    }
  }
}

/// Parse the property name, params, and body of a class element.
/// Returns the updated parser and whether this element was a constructor.
fn parse_class_element_body(
  outer_p: P,
  p5: P,
  has_extends: Bool,
  is_method_async: Bool,
  is_generator: Bool,
  class_accessor_kind: String,
  is_constructor: Bool,
  is_static: Bool,
) -> Result(#(P, Bool, ast.ClassElement), ParseError) {
  use #(p6, key_expr, is_computed) <- result.try(parse_property_name(p5))
  case peek(p6) {
    LeftParen -> {
      // Method — validate getter/setter params
      let method_kind = case is_constructor {
        True -> ast.MethodConstructor
        False ->
          case class_accessor_kind {
            "get" -> ast.MethodGet
            "set" -> ast.MethodSet
            _ -> ast.MethodMethod
          }
      }
      use #(p7, params, body) <- result.try(case class_accessor_kind {
        "get" ->
          parse_getter_params_and_body(enter_method_context(
            p6,
            False,
            False,
            False,
            has_extends,
          ))
          |> restore_context_fn(outer_p)
        "set" ->
          parse_setter_params_and_body(enter_method_context(
            p6,
            False,
            False,
            False,
            has_extends,
          ))
          |> restore_context_fn(outer_p)
        _ ->
          parse_function_params_and_body(enter_method_context(
            p6,
            is_generator,
            is_method_async,
            is_constructor,
            has_extends,
          ))
          |> restore_context_fn(outer_p)
      })
      Ok(#(
        p7,
        is_constructor,
        ast.ClassMethod(
          key: key_expr,
          value: ast.FunctionExpression(
            name: None,
            params: params,
            body: body,
            is_generator: is_generator,
            is_async: is_method_async,
          ),
          kind: method_kind,
          is_static: is_static,
          computed: is_computed,
        ),
      ))
    }
    Equal -> {
      // Field with initializer
      let p7 = advance(p6)
      use #(p8, init_expr) <- result.try(parse_assignment_expression(p7))
      use p9 <- result.try(eat_semicolon(p8))
      Ok(#(
        p9,
        False,
        ast.ClassField(
          key: key_expr,
          value: Some(init_expr),
          is_static: is_static,
          computed: is_computed,
        ),
      ))
    }
    Semicolon ->
      Ok(#(
        advance(p6),
        False,
        ast.ClassField(
          key: key_expr,
          value: None,
          is_static: is_static,
          computed: is_computed,
        ),
      ))
    _ -> {
      use p7 <- result.try(eat_semicolon(p6))
      Ok(#(
        p7,
        False,
        ast.ClassField(
          key: key_expr,
          value: None,
          is_static: is_static,
          computed: is_computed,
        ),
      ))
    }
  }
}

fn check_label_identifier(p: P, label: String) -> Result(Nil, ParseError) {
  case label {
    "yield" -> {
      use <- bool.guard(p.strict, Error(YieldReservedStrictMode(pos_of(p))))
      use <- bool.guard(p.in_generator, Error(YieldInGenerator(pos_of(p))))
      Ok(Nil)
    }
    "await" -> {
      use <- bool.guard(p.mode == Module, Error(AwaitInModule(pos_of(p))))
      use <- bool.guard(p.in_async, Error(AwaitInAsyncFunction(pos_of(p))))
      Ok(Nil)
    }
    _ -> Ok(Nil)
  }
}

fn parse_labeled_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  // identifier : statement
  let label = peek_value(p)
  // Check if yield/await are used as labels in restricted contexts
  use Nil <- result.try(check_label_identifier(p, label))
  // Check for duplicate labels
  use <- bool.guard(
    find_label(p.label_set, label),
    Error(DuplicateLabel(label, pos_of(p))),
  )
  parse_labeled_statement_body(p, label)
}

fn parse_labeled_statement_body(
  p: P,
  label: String,
) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, Colon))
  {
    // Determine if the labeled statement is a loop
    let is_loop = case peek(p3) {
      While | Do | For -> True
      _ -> False
    }
    let outer_labels = p3.label_set
    let p3 = P(..p3, label_set: [#(label, is_loop), ..p3.label_set])
    let wrap_label = fn(res) {
      use #(inner_p, stmt) <- result.map(res)
      #(
        P(..inner_p, label_set: outer_labels),
        ast.LabeledStatement(label:, body: stmt),
      )
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
          p3.strict || p3.in_single_stmt_pos,
          Error(FunctionDeclInLabelBody(pos_of(p3))),
        )
        wrap_label(parse_statement(p3))
      }
      _ -> wrap_label(parse_statement(p3))
    }
  }
}

fn parse_with_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  use <- bool.guard(p.strict, Error(WithNotAllowedStrictMode(pos_of(p))))
  parse_with_statement_body(p)
}

fn parse_with_statement_body(p: P) -> Result(#(P, ast.Statement), ParseError) {
  let p2 = advance(p)
  use p3 <- result.try(expect(p2, LeftParen))
  use #(p4, object) <- result.try(parse_expression(p3))
  use p5 <- result.try(expect(p4, RightParen))
  use #(p6, body) <- result.try(parse_single_statement(p5, False))
  Ok(#(p6, ast.WithStatement(object:, body:)))
}

fn parse_expression_statement(p: P) -> Result(#(P, ast.Statement), ParseError) {
  use #(p2, expr) <- result.try(parse_expression(p))
  use <- bool.guard(
    p2.has_cover_initializer,
    Error(ShorthandDefaultOutsideDestructuring(pos_of(p))),
  )
  use p3 <- result.try(eat_semicolon(p2))
  Ok(#(p3, ast.ExpressionStatement(expression: expr)))
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
          case parse_expression(p3) {
            // Comma expression is not a valid assignment target
            Ok(#(p4, rest_expr)) ->
              Ok(#(
                P(..p4, last_expr_assignable: False),
                ast.SequenceExpression(expressions: [first_expr, rest_expr]),
              ))
            // If the next expression fails, the comma was a trailing comma
            Error(_) -> Ok(#(p2, first_expr))
          }
        }
      }
    _ -> Ok(#(p2, first_expr))
  }
}

fn parse_assignment_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  case peek(p) {
    Yield -> {
      // Only parse yield expression inside generators.
      // Outside generators, yield is an identifier (sloppy mode) or reserved
      // word (strict mode), both handled by parse_assignment_expression_inner.
      case p.in_generator {
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
            | LeftBracket
            | QuestionDot
            | Arrow -> parse_assignment_expression_inner(p)
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
    Error(_) -> {
      // In strict mode, check if this is a simple assignment to eval/arguments
      // before parsing — if current token is eval/arguments and next is assignment op
      case p.strict {
        True ->
          case peek(p) {
            Identifier ->
              case peek_value(p) {
                "eval" | "arguments" -> {
                  let name = peek_value(p)
                  case is_assignment_operator(peek_at(p, 1)) {
                    True -> Error(StrictModeAssignment(name, pos_of(p)))
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

fn is_assignment_operator(kind: TokenKind) -> Bool {
  case kind {
    Equal
    | PlusEqual
    | MinusEqual
    | StarEqual
    | StarStarEqual
    | SlashEqual
    | PercentEqual
    | AmpersandEqual
    | PipeEqual
    | CaretEqual
    | LessThanLessThanEqual
    | GreaterThanGreaterThanEqual
    | GreaterThanGreaterThanGreaterThanEqual
    | AmpersandAmpersandEqual
    | PipePipeEqual
    | QuestionQuestionEqual -> True
    _ -> False
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
          let p3 = advance(P(..p2, has_cover_initializer: False))
          // Assignment result is not a valid assignment target, but
          // IS a valid cover for AssignmentPattern (target = default)
          use #(p4, rhs) <- result.try(parse_assignment_expression(p3))
          Ok(#(
            P(..p4, last_expr_assignable: False, last_expr_is_assignment: True),
            ast.AssignmentExpression(
              operator: ast.Assign,
              left: lhs_expr,
              right: rhs,
            ),
          ))
        }
        // For plain =, allow object/array patterns as LHS
        False ->
          case lhs_start {
            LeftBrace | LeftBracket ->
              case p2.has_invalid_pattern {
                True -> Error(InvalidDestructuringTarget(pos_of(p2)))
                False ->
                  case p.strict && p2.has_eval_args_target {
                    True -> Error(EvalArgsAssignStrictMode(pos_of(p2)))
                    False -> {
                      let p3 =
                        advance(
                          P(
                            ..p2,
                            has_cover_initializer: False,
                            has_invalid_pattern: False,
                            has_eval_args_target: False,
                          ),
                        )
                      use #(p4, rhs) <- result.try(parse_assignment_expression(
                        p3,
                      ))
                      Ok(#(
                        P(
                          ..p4,
                          last_expr_assignable: False,
                          last_expr_is_assignment: True,
                        ),
                        ast.AssignmentExpression(
                          operator: ast.Assign,
                          left: lhs_expr,
                          right: rhs,
                        ),
                      ))
                    }
                  }
              }
            _ -> Error(InvalidAssignmentLhs(pos_of(p2)))
          }
      }
    _ ->
      case is_assignment_operator(peek(p2)) {
        True ->
          // Compound assignment — LHS must be a simple assignment target
          case p2.last_expr_assignable {
            True -> {
              let op = token_to_assignment_op(peek(p2))
              let p3 = advance(P(..p2, has_cover_initializer: False))
              use #(p4, rhs) <- result.try(parse_assignment_expression(p3))
              Ok(#(
                P(..p4, last_expr_assignable: False),
                ast.AssignmentExpression(
                  operator: op,
                  left: lhs_expr,
                  right: rhs,
                ),
              ))
            }
            False -> Error(InvalidAssignmentLhs(pos_of(p2)))
          }
        // Not an assignment — clear the flag so it doesn't leak
        // from a previous sibling expression (e.g. [a = 1, 0])
        False -> Ok(#(P(..p2, last_expr_is_assignment: False), lhs_expr))
      }
  }
}

fn try_arrow_function(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let wrap_arrow = fn(
    body_result: Result(#(P, ast.ArrowBody), ParseError),
    outer: P,
    is_async,
    params: List(ast.Pattern),
  ) {
    use #(p_body, body) <- result.try(body_result)
    let p_restored = restore_outer_context(p_body, outer)
    Ok(#(
      p_restored,
      ast.ArrowFunctionExpression(
        params: params,
        body: body,
        is_async: is_async,
      ),
    ))
  }
  case peek(p) {
    // async (...) => or async ident =>
    Async -> {
      case peek_at(p, 1) {
        LeftParen -> {
          let saved = p
          let p2 = advance(advance(p))
          // Arrow params always check for duplicate names
          let p2_arrow =
            P(
              ..p2,
              in_arrow_params: True,
              in_formal_params: True,
              param_bound_names: [],
              has_non_simple_param: False,
              binding_kind: BindingParam,
            )
          case parse_formal_parameters(p2_arrow) {
            Ok(#(p3, params)) ->
              case
                expect(
                  P(
                    ..p3,
                    in_arrow_params: False,
                    in_formal_params: False,
                    binding_kind: BindingNone,
                  ),
                  RightParen,
                )
              {
                Ok(p4) ->
                  case peek(p4) {
                    Arrow ->
                      // Line terminator before => is not allowed
                      case has_line_break_before(p4) {
                        True -> Error(NotAnArrowFunction(pos_of(saved)))
                        False -> {
                          // Save param scope and super flags before enter_function_context resets them
                          // Arrow functions inherit super from their enclosing method/constructor
                          let param_scope = p4.scope_params
                          let saved_super_call = p4.allow_super_call
                          let saved_super_property = p4.allow_super_property
                          let p5 =
                            enter_function_context(advance(p4), False, True)
                          let p5 =
                            P(
                              ..p5,
                              scope_params: param_scope,
                              allow_super_call: saved_super_call,
                              allow_super_property: saved_super_property,
                            )
                          wrap_arrow(parse_arrow_body(p5), p, True, params)
                        }
                      }
                    _ -> Error(NotAnArrowFunction(pos_of(saved)))
                  }
                Error(_) -> Error(NotAnArrowFunction(pos_of(saved)))
              }
            Error(_) -> Error(NotAnArrowFunction(pos_of(saved)))
          }
        }
        Identifier -> {
          case peek_at(p, 2) {
            Arrow -> {
              // Check identifier is valid as a binding name
              let name = peek_value_at(p, 1)
              use Nil <- result.try(check_binding_identifier(p, name))
              {
                // advance past async + ident to land on Arrow
                let p2 = advance(advance(p))
                // Check for line break before =>
                case has_line_break_before(p2) {
                  True -> Error(NotAnArrowFunction(pos_of(p)))
                  False -> {
                    let p3 = advance(p2)
                    let p3 = P(..p3, param_bound_names: [name])
                    let saved_super_call = p3.allow_super_call
                    let saved_super_property = p3.allow_super_property
                    let p3 = enter_function_context(p3, False, True)
                    let p3 =
                      P(
                        ..p3,
                        scope_params: set.from_list([name]),
                        allow_super_call: saved_super_call,
                        allow_super_property: saved_super_property,
                      )
                    wrap_arrow(parse_arrow_body(p3), p, True, [
                      ast.IdentifierPattern(name: name),
                    ])
                  }
                }
              }
            }
            _ -> Error(NotAnArrowFunction(pos_of(p)))
          }
        }
        _ -> Error(NotAnArrowFunction(pos_of(p)))
      }
    }
    Identifier | Yield | Await -> {
      case peek_at(p, 1) {
        Arrow -> {
          // Check identifier is valid as a binding name (eval/arguments/etc)
          let name = peek_value(p)
          use Nil <- result.try(check_binding_identifier(p, name))
          {
            let p2 = advance(p)
            // Check for line break before =>
            case has_line_break_before(p2) {
              True -> Error(NotAnArrowFunction(pos_of(p)))
              False -> {
                let p3 = advance(p2)
                // Save param name for retroactive strict check in body
                let p3 = P(..p3, param_bound_names: [name])
                let saved_super_call = p3.allow_super_call
                let saved_super_property = p3.allow_super_property
                let p3 = enter_function_context(p3, False, False)
                let p3 =
                  P(
                    ..p3,
                    scope_params: set.from_list([name]),
                    allow_super_call: saved_super_call,
                    allow_super_property: saved_super_property,
                  )
                wrap_arrow(parse_arrow_body(p3), p, False, [
                  ast.IdentifierPattern(name: name),
                ])
              }
            }
          }
        }
        _ -> Error(NotAnArrowFunction(pos_of(p)))
      }
    }
    LeftParen -> {
      // Try to parse as arrow parameter list
      // This is the hardest case — (a, b) => vs (a, b) as expression
      // We speculatively parse and backtrack if it's not an arrow
      let saved = p
      let p2 = advance(p)
      // Arrow params always check for duplicate names (even sloppy mode).
      // We set in_arrow_params to True to enable unconditional dup checking
      // without enabling all strict-mode restrictions.
      let p2_arrow =
        P(
          ..p2,
          in_arrow_params: True,
          in_formal_params: True,
          param_bound_names: [],
          has_non_simple_param: False,
          binding_kind: BindingParam,
        )
      case parse_possible_arrow_params(p2_arrow) {
        Ok(#(p3, params)) ->
          case
            expect(
              P(
                ..p3,
                in_arrow_params: False,
                in_formal_params: False,
                binding_kind: BindingNone,
              ),
              RightParen,
            )
          {
            Ok(p4) ->
              case peek(p4) {
                Arrow ->
                  // Line terminator before => is not allowed
                  case has_line_break_before(p4) {
                    True -> Error(NotAnArrowFunction(pos_of(saved)))
                    False -> {
                      // Save param scope and super flags before enter_function_context resets them
                      // Arrow functions inherit super from their enclosing method/constructor
                      let param_scope = p4.scope_params
                      let saved_super_call = p4.allow_super_call
                      let saved_super_property = p4.allow_super_property
                      let p5 = enter_function_context(advance(p4), False, False)
                      let p5 =
                        P(
                          ..p5,
                          scope_params: param_scope,
                          allow_super_call: saved_super_call,
                          allow_super_property: saved_super_property,
                        )
                      wrap_arrow(parse_arrow_body(p5), p, False, params)
                    }
                  }
                _ -> Error(NotAnArrowFunction(pos_of(saved)))
              }
            Error(_) -> Error(NotAnArrowFunction(pos_of(saved)))
          }
        Error(_) -> Error(NotAnArrowFunction(pos_of(saved)))
      }
    }
    _ -> Error(NotAnArrowFunction(pos_of(p)))
  }
}

fn parse_possible_arrow_params(
  p: P,
) -> Result(#(P, List(ast.Pattern)), ParseError) {
  // Parse what could be arrow function parameters
  // This is the same as formal_parameters essentially
  case peek(p) {
    RightParen -> Ok(#(p, []))
    _ -> parse_formal_parameter_list(p, set.new(), [])
  }
}

fn parse_arrow_body(p: P) -> Result(#(P, ast.ArrowBody), ParseError) {
  // Clear cover-grammar flag since arrow params are a valid
  // destructuring context (e.g. ({a = 0}) => ... is valid).
  let p = P(..p, has_cover_initializer: False)
  case peek(p) {
    LeftBrace -> {
      // Check for "use strict" directive in arrow body
      // Use function_body_block so param scope is shared with body
      use p <- result.try(check_use_strict_in_body(p))
      use #(p2, body_stmt) <- result.try(parse_function_body_block(p))
      Ok(#(p2, ast.ArrowBodyBlock(body_stmt)))
    }
    _ -> {
      use #(p2, expr) <- result.try(parse_assignment_expression(p))
      Ok(#(p2, ast.ArrowBodyExpression(expr)))
    }
  }
}

fn parse_yield_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  // Yield expressions are forbidden in formal parameter defaults
  case p.in_formal_params {
    True -> Error(YieldInFormalParameter(pos_of(p)))
    False -> parse_yield_expression_inner(p)
  }
}

fn parse_yield_expression_inner(
  p: P,
) -> Result(#(P, ast.Expression), ParseError) {
  let p2 = advance(p)
  case peek(p2) {
    Semicolon | RightParen | RightBracket | RightBrace | Eof | Comma | Colon ->
      Ok(#(p2, ast.YieldExpression(argument: None, is_delegate: False)))
    Star ->
      // yield * only forms yield* (delegate) with no line break before *
      case has_line_break_before(p2) {
        True ->
          Ok(#(p2, ast.YieldExpression(argument: None, is_delegate: False)))
        False -> {
          let p3 = advance(p2)
          use #(p4, arg) <- result.try(parse_assignment_expression(p3))
          Ok(#(p4, ast.YieldExpression(argument: Some(arg), is_delegate: True)))
        }
      }
    Slash | SlashEqual -> {
      // yield /regex/ — in generator context, slash after yield is regex
      case has_line_break_before(p2) {
        True ->
          Ok(#(p2, ast.YieldExpression(argument: None, is_delegate: False)))
        False -> {
          use #(p3, regex_expr) <- result.try(parse_regex_literal(p2))
          Ok(#(
            p3,
            ast.YieldExpression(argument: Some(regex_expr), is_delegate: False),
          ))
        }
      }
    }
    _ ->
      case has_line_break_before(p2) {
        True ->
          Ok(#(p2, ast.YieldExpression(argument: None, is_delegate: False)))
        False -> {
          use #(p3, arg) <- result.try(parse_assignment_expression(p2))
          Ok(#(p3, ast.YieldExpression(argument: Some(arg), is_delegate: False)))
        }
      }
  }
}

fn parse_conditional_expression(
  p: P,
) -> Result(#(P, ast.Expression), ParseError) {
  use #(p2, test_expr) <- result.try(parse_binary_expression(p, 0))
  case peek(p2) {
    Question -> {
      let p3 = advance(p2)
      use #(p4, consequent) <- result.try(parse_assignment_expression(p3))
      use p5 <- result.try(expect(p4, Colon))
      // Conditional expression is not a valid assignment target
      use #(p6, alternate) <- result.try(parse_assignment_expression(p5))
      Ok(#(
        P(..p6, last_expr_assignable: False, last_expr_is_assignment: False),
        ast.ConditionalExpression(
          condition: test_expr,
          consequent: consequent,
          alternate: alternate,
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
  let prec = binary_precedence(tok, p.allow_in)
  case prec > min_prec {
    True -> {
      let op = token_to_binary_op(tok)
      let p2 = advance(p)
      // Right-associative for **
      let next_min = case tok {
        StarStar -> prec - 1
        _ -> prec
      }
      use #(p3, right) <- result.try(parse_binary_expression(p2, next_min))
      let expr = case is_logical_op(tok) {
        True -> ast.LogicalExpression(operator: op, left: left, right: right)
        False -> ast.BinaryExpression(operator: op, left: left, right: right)
      }
      parse_binary_rhs(P(..p3, last_expr_assignable: False), expr, min_prec)
    }
    False -> Ok(#(p, left))
  }
}

fn binary_precedence(kind: TokenKind, allow_in: Bool) -> Int {
  case kind {
    PipePipe -> 1
    AmpersandAmpersand -> 2
    Pipe -> 3
    Caret -> 4
    Ampersand -> 5
    EqualEqual | BangEqual | EqualEqualEqual | BangEqualEqual -> 6
    LessThan | LessThanEqual | GreaterThan | GreaterThanEqual | Instanceof -> 7
    In ->
      case allow_in {
        True -> 7
        False -> 0
      }
    LessThanLessThan
    | GreaterThanGreaterThan
    | GreaterThanGreaterThanGreaterThan -> 8
    Plus | Minus -> 9
    Star | Slash | Percent -> 10
    StarStar -> 11
    QuestionQuestion -> 1
    _ -> 0
  }
}

fn token_to_binary_op(kind: TokenKind) -> ast.BinaryOp {
  case kind {
    Plus -> ast.Add
    Minus -> ast.Subtract
    Star -> ast.Multiply
    Slash -> ast.Divide
    Percent -> ast.Modulo
    StarStar -> ast.Exponentiation
    EqualEqualEqual -> ast.StrictEqual
    BangEqualEqual -> ast.StrictNotEqual
    EqualEqual -> ast.Equal
    BangEqual -> ast.NotEqual
    LessThan -> ast.LessThan
    GreaterThan -> ast.GreaterThan
    LessThanEqual -> ast.LessThanEqual
    GreaterThanEqual -> ast.GreaterThanEqual
    LessThanLessThan -> ast.LeftShift
    GreaterThanGreaterThan -> ast.RightShift
    GreaterThanGreaterThanGreaterThan -> ast.UnsignedRightShift
    Ampersand -> ast.BitwiseAnd
    Pipe -> ast.BitwiseOr
    Caret -> ast.BitwiseXor
    AmpersandAmpersand -> ast.LogicalAnd
    PipePipe -> ast.LogicalOr
    QuestionQuestion -> ast.NullishCoalescing
    In -> ast.In
    Instanceof -> ast.InstanceOf
    _ -> ast.Add
  }
}

fn is_logical_op(kind: TokenKind) -> Bool {
  case kind {
    AmpersandAmpersand | PipePipe | QuestionQuestion -> True
    _ -> False
  }
}

fn token_to_assignment_op(kind: TokenKind) -> ast.AssignmentOp {
  case kind {
    Equal -> ast.Assign
    PlusEqual -> ast.AddAssign
    MinusEqual -> ast.SubtractAssign
    StarEqual -> ast.MultiplyAssign
    SlashEqual -> ast.DivideAssign
    PercentEqual -> ast.ModuloAssign
    StarStarEqual -> ast.ExponentiationAssign
    LessThanLessThanEqual -> ast.LeftShiftAssign
    GreaterThanGreaterThanEqual -> ast.RightShiftAssign
    GreaterThanGreaterThanGreaterThanEqual -> ast.UnsignedRightShiftAssign
    AmpersandEqual -> ast.BitwiseAndAssign
    PipeEqual -> ast.BitwiseOrAssign
    CaretEqual -> ast.BitwiseXorAssign
    AmpersandAmpersandEqual -> ast.LogicalAndAssign
    PipePipeEqual -> ast.LogicalOrAssign
    QuestionQuestionEqual -> ast.NullishCoalesceAssign
    _ -> ast.Assign
  }
}

fn parse_js_number(raw: String) -> Float {
  // Simple number parsing — handles basic cases
  case raw {
    "0" -> 0.0
    _ ->
      case string.starts_with(raw, "0x") || string.starts_with(raw, "0X") {
        True -> 0.0
        False ->
          case string.starts_with(raw, "0o") || string.starts_with(raw, "0O") {
            True -> 0.0
            False ->
              case
                string.starts_with(raw, "0b") || string.starts_with(raw, "0B")
              {
                True -> 0.0
                False -> {
                  // Remove numeric separators
                  let clean = string.replace(raw, "_", "")
                  // Try float parse first, then int
                  case
                    string.contains(clean, ".")
                    || string.contains(clean, "e")
                    || string.contains(clean, "E")
                  {
                    True ->
                      case gleam_float_parse(clean) {
                        Ok(f) -> f
                        Error(Nil) -> 0.0
                      }
                    False ->
                      case gleam_int_parse(clean) {
                        Ok(i) -> int_to_float(i)
                        Error(Nil) -> 0.0
                      }
                  }
                }
              }
          }
      }
  }
}

fn int_to_float(i: Int) -> Float {
  int.to_float(i)
}

fn gleam_float_parse(s: String) -> Result(Float, Nil) {
  case string.contains(s, ".") {
    True ->
      case catch_float_parse(s) {
        Ok(f) -> Ok(f)
        Error(_) -> Error(Nil)
      }
    False ->
      case string.contains(s, "e") || string.contains(s, "E") {
        True ->
          case catch_float_parse(s) {
            Ok(f) -> Ok(f)
            Error(_) -> Error(Nil)
          }
        False -> Error(Nil)
      }
  }
}

@external(erlang, "arc_parser_ffi", "parse_float")
@external(javascript, "../arc_parser_ffi.mjs", "parse_float")
fn catch_float_parse(s: String) -> Result(Float, Nil)

fn gleam_int_parse(s: String) -> Result(Int, Nil) {
  case string.to_graphemes(s) {
    [] -> Error(Nil)
    _ -> {
      let result =
        list.try_fold(string.to_graphemes(s), 0, fn(acc, ch) {
          case ch {
            "0" -> Ok(acc * 10)
            "1" -> Ok(acc * 10 + 1)
            "2" -> Ok(acc * 10 + 2)
            "3" -> Ok(acc * 10 + 3)
            "4" -> Ok(acc * 10 + 4)
            "5" -> Ok(acc * 10 + 5)
            "6" -> Ok(acc * 10 + 6)
            "7" -> Ok(acc * 10 + 7)
            "8" -> Ok(acc * 10 + 8)
            "9" -> Ok(acc * 10 + 9)
            _ -> Error(Nil)
          }
        })
      result
    }
  }
}

fn parse_unary_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let unary = fn(p2, op) {
    use #(p3, arg) <- result.try(parse_unary_expression(p2))
    Ok(#(
      P(..p3, last_expr_assignable: False, last_expr_is_assignment: False),
      ast.UnaryExpression(operator: op, prefix: True, argument: arg),
    ))
  }
  case peek(p) {
    Delete -> {
      let p2 = advance(p)
      use <- bool.guard(
        p.strict && is_delete_of_identifier(p2, 0),
        Error(DeleteUnqualifiedStrictMode(pos_of(p))),
      )
      unary(p2, ast.Delete)
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
      case p3.last_expr_assignable {
        True ->
          case p.strict, p3.last_expr_name {
            True, Some("eval") | True, Some("arguments") -> {
              let name = option.unwrap(p3.last_expr_name, "")
              Error(StrictModeModification(name, pos_of(p)))
            }
            _, _ ->
              Ok(#(
                P(..p3, last_expr_assignable: False),
                ast.UpdateExpression(operator: op, prefix: True, argument: arg),
              ))
          }
        False -> Error(InvalidLhsPrefixOp(pos_of(p)))
      }
    }
    Await -> {
      let p2 = advance(p)
      use #(p3, arg) <- result.try(parse_unary_expression(p2))
      Ok(#(
        P(..p3, last_expr_assignable: False, last_expr_is_assignment: False),
        ast.AwaitExpression(argument: arg),
      ))
    }
    _ -> parse_postfix_expression(p)
  }
}

/// Check if the delete operand is a (possibly parenthesized) identifier.
/// Looks through nested LeftParen tokens to find an Identifier, then
/// checks for matching RightParen tokens.
fn is_delete_of_identifier(p: P, offset: Int) -> Bool {
  case peek_at(p, offset) {
    Identifier ->
      // Check for matching closing parens
      is_all_right_parens(p, offset + 1, offset)
    LeftParen -> is_delete_of_identifier(p, offset + 1)
    _ -> False
  }
}

/// Check that there are at least  closing parens starting at .
///  is the number of open parens we need to match.
fn is_all_right_parens(p: P, offset: Int, count: Int) -> Bool {
  case count {
    0 -> True
    _ ->
      case peek_at(p, offset) {
        RightParen -> is_all_right_parens(p, offset + 1, count - 1)
        _ -> False
      }
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
      case p2.last_expr_assignable {
        True ->
          case p.strict, p2.last_expr_name {
            True, Some("eval") | True, Some("arguments") -> {
              let name = option.unwrap(p2.last_expr_name, "")
              Error(StrictModeModifyRestricted(name, pos_of(p2)))
            }
            _, _ ->
              Ok(#(
                P(..advance(p2), last_expr_assignable: False),
                ast.UpdateExpression(
                  operator: op,
                  prefix: False,
                  argument: expr,
                ),
              ))
          }
        False -> Error(InvalidPostfixLhs(pos_of(p2)))
      }
    }
    _ -> Ok(#(p2, expr))
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
              case p.function_depth > 0 {
                True -> {
                  let meta = ast.MetaProperty(meta: "new", property: "target")
                  parse_call_chain(advance(p3), meta)
                }
                False -> Error(NewTargetOutsideFunction(pos_of(p)))
              }
            other, _ -> Error(ExpectedNewTargetGot(other, pos_of(p3)))
          }
        }
        _ -> Error(ExpectedNewTarget(pos_of(p3)))
      }
    }
    New -> {
      // new new Foo()
      use #(p3, inner) <- result.try(parse_new_expression(p2))
      parse_call_chain(p3, inner)
    }
    _ -> {
      use #(p3, callee_base) <- result.try(parse_primary_expression(p2))
      let #(p4, callee) = parse_member_chain(p3, callee_base)
      // Optional arguments
      case peek(p4) {
        LeftParen -> {
          // new Foo() — result is not assignable, but call_chain
          // may add member access making it assignable
          use #(p5, args) <- result.try(parse_arguments(p4))
          let new_expr = ast.NewExpression(callee: callee, arguments: args)
          parse_call_chain(P(..p5, last_expr_assignable: False), new_expr)
        }
        // Tagged template on new expression — not assignable
        TemplateLiteral ->
          Ok(#(
            P(..advance(p4), last_expr_assignable: False),
            ast.NewExpression(callee: callee, arguments: []),
          ))
        // new Foo without parens — not assignable
        _ ->
          Ok(#(
            P(..p4, last_expr_assignable: False),
            ast.NewExpression(callee: callee, arguments: []),
          ))
      }
    }
  }
}

fn parse_call_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let parsed = case peek(p) {
    Super -> {
      let p2 = advance(p)
      case peek(p2) {
        LeftParen ->
          case p.allow_super_call {
            True -> {
              use #(p3, args) <- result.try(parse_arguments(p2))
              Ok(#(
                p3,
                ast.CallExpression(callee: ast.SuperExpression, arguments: args),
              ))
            }
            False -> Error(SuperCallNotInDerivedConstructor(pos_of(p)))
          }
        Dot | LeftBracket ->
          case p.allow_super_property {
            True -> Ok(#(p2, ast.SuperExpression))
            False -> Error(SuperPropertyNotInMethod(pos_of(p)))
          }
        _ -> Error(UnexpectedSuper(pos_of(p)))
      }
    }
    Import -> {
      let p2 = advance(p)
      case peek(p2) {
        LeftParen -> {
          let p3 = advance(p2)
          use #(p4, source_expr) <- result.try(parse_assignment_expression(p3))
          // Optional second argument (import attributes)
          let p5 = case peek(p4) {
            Comma ->
              case peek_at(p4, 1) {
                RightParen -> advance(p4)
                _ ->
                  case expr_p(parse_assignment_expression(advance(p4))) {
                    Ok(p_val) -> p_val
                    Error(_) -> advance(p4)
                  }
              }
            _ -> p4
          }
          use p6 <- result.try(expect(p5, RightParen))
          Ok(#(p6, ast.ImportExpression(source: source_expr)))
        }
        Dot -> {
          // import.meta
          let p3 = advance(p2)
          case peek(p3), peek_value(p3) {
            Identifier, "meta" ->
              Ok(#(
                advance(p3),
                ast.MetaProperty(meta: "import", property: "meta"),
              ))
            Identifier, other -> Error(ExpectedImportMetaGot(other, pos_of(p3)))
            _, _ -> Error(ExpectedImportMeta(pos_of(p3)))
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

fn parse_call_chain(
  p: P,
  callee: ast.Expression,
) -> Result(#(P, ast.Expression), ParseError) {
  case peek(p) {
    LeftParen -> {
      // Call expression — not a valid assignment target
      use #(p2, args) <- result.try(parse_arguments(p))
      let expr = ast.CallExpression(callee: callee, arguments: args)
      parse_call_chain(P(..p2, last_expr_assignable: False), expr)
    }
    Dot -> {
      let p2 = advance(p)
      case is_identifier_or_keyword(peek(p2)) {
        True -> {
          let prop_name = peek_value(p2)
          let expr =
            ast.MemberExpression(
              object: callee,
              property: ast.Identifier(name: prop_name),
              computed: False,
            )
          parse_call_chain(P(..advance(p2), last_expr_assignable: True), expr)
        }
        False -> Error(ExpectedIdentifierAfterDot(pos_of(p2)))
      }
    }
    LeftBracket -> {
      let p2 = advance(p)
      let saved_allow_in = p.allow_in
      let p2 = P(..p2, allow_in: True)
      use #(p3, prop_expr) <- result.try(parse_expression(p2))
      use p4 <- result.try(expect(
        P(..p3, allow_in: saved_allow_in),
        RightBracket,
      ))
      let expr =
        ast.MemberExpression(
          object: callee,
          property: prop_expr,
          computed: True,
        )
      // Computed member access — valid assignment target
      parse_call_chain(P(..p4, last_expr_assignable: True), expr)
    }
    QuestionDot -> {
      // Optional chaining — NEVER a valid assignment target
      let p2 = advance(p)
      case peek(p2) {
        LeftParen -> {
          use #(p3, args) <- result.try(parse_arguments(p2))
          let expr = ast.OptionalCallExpression(callee: callee, arguments: args)
          parse_call_chain(P(..p3, last_expr_assignable: False), expr)
        }
        LeftBracket -> {
          let p3 = advance(p2)
          let saved_allow_in = p.allow_in
          let p3 = P(..p3, allow_in: True)
          use #(p4, prop_expr) <- result.try(parse_expression(p3))
          use p5 <- result.try(expect(
            P(..p4, allow_in: saved_allow_in),
            RightBracket,
          ))
          let expr =
            ast.OptionalMemberExpression(
              object: callee,
              property: prop_expr,
              computed: True,
            )
          parse_call_chain(P(..p5, last_expr_assignable: False), expr)
        }
        _ ->
          case is_identifier_or_keyword(peek(p2)) {
            True -> {
              let prop_name = peek_value(p2)
              let expr =
                ast.OptionalMemberExpression(
                  object: callee,
                  property: ast.Identifier(name: prop_name),
                  computed: False,
                )
              parse_call_chain(
                P(..advance(p2), last_expr_assignable: False),
                expr,
              )
            }
            False -> Error(ExpectedAfterOptionalChain(pos_of(p2)))
          }
      }
    }
    TemplateLiteral -> {
      // Tagged template — not a valid assignment target
      let expr =
        ast.TaggedTemplateExpression(
          tag: callee,
          quasi: ast.TemplateLiteral(quasis: [], expressions: []),
        )
      parse_call_chain(P(..advance(p), last_expr_assignable: False), expr)
    }
    _ -> Ok(#(p, callee))
  }
}

fn parse_member_chain(p: P, object: ast.Expression) -> #(P, ast.Expression) {
  case peek(p) {
    Dot -> {
      let p2 = advance(p)
      case is_identifier_or_keyword(peek(p2)) {
        True -> {
          let prop_name = peek_value(p2)
          let expr =
            ast.MemberExpression(
              object: object,
              property: ast.Identifier(name: prop_name),
              computed: False,
            )
          parse_member_chain(P(..advance(p2), last_expr_assignable: True), expr)
        }
        False -> #(p, object)
      }
    }
    LeftBracket -> {
      let saved_allow_in = p.allow_in
      let p_in = P(..advance(p), allow_in: True)
      case
        {
          use #(p2, prop_expr) <- result.try(parse_expression(p_in))
          use p3 <- result.try(expect(
            P(..p2, allow_in: saved_allow_in),
            RightBracket,
          ))
          Ok(#(p3, prop_expr))
        }
      {
        // Computed member access — valid assignment target
        Ok(#(p3, prop_expr)) -> {
          let expr =
            ast.MemberExpression(
              object: object,
              property: prop_expr,
              computed: True,
            )
          parse_member_chain(P(..p3, last_expr_assignable: True), expr)
        }
        Error(_) -> #(p, object)
      }
    }
    QuestionDot -> {
      // Optional chaining — NEVER a valid assignment target
      let p2 = advance(p)
      case peek(p2) {
        Identifier -> {
          let prop_name = peek_value(p2)
          let expr =
            ast.OptionalMemberExpression(
              object: object,
              property: ast.Identifier(name: prop_name),
              computed: False,
            )
          parse_member_chain(
            P(..advance(p2), last_expr_assignable: False),
            expr,
          )
        }
        LeftBracket -> {
          let saved_allow_in = p.allow_in
          let p3_in = P(..advance(p2), allow_in: True)
          case
            {
              use #(p3, prop_expr) <- result.try(parse_expression(p3_in))
              use p4 <- result.try(expect(
                P(..p3, allow_in: saved_allow_in),
                RightBracket,
              ))
              Ok(#(p4, prop_expr))
            }
          {
            Ok(#(p4, prop_expr)) -> {
              let expr =
                ast.OptionalMemberExpression(
                  object: object,
                  property: prop_expr,
                  computed: True,
                )
              parse_member_chain(P(..p4, last_expr_assignable: False), expr)
            }
            Error(_) -> #(p, object)
          }
        }
        _ -> #(p, object)
      }
    }
    _ -> #(p, object)
  }
}

fn parse_arguments(p: P) -> Result(#(P, List(ast.Expression)), ParseError) {
  use p2 <- result.try(expect(p, LeftParen))
  parse_argument_list(p2, [])
}

fn parse_argument_list(
  p: P,
  acc: List(ast.Expression),
) -> Result(#(P, List(ast.Expression)), ParseError) {
  case peek(p) {
    RightParen -> Ok(#(advance(p), list.reverse(acc)))
    _ -> {
      use #(p2, arg) <- result.try(parse_argument(p))
      case peek(p2) {
        Comma ->
          case peek_at(p2, 1) {
            RightParen ->
              Ok(#(advance(advance(p2)), list.reverse([arg, ..acc])))
            _ -> parse_argument_list(advance(p2), [arg, ..acc])
          }
        RightParen -> Ok(#(advance(p2), list.reverse([arg, ..acc])))
        _ -> Error(ExpectedCommaOrCloseParen(pos_of(p2)))
      }
    }
  }
}

fn parse_argument(p: P) -> Result(#(P, ast.Expression), ParseError) {
  // Restore allow_in inside function arguments, then put it back
  let saved_allow_in = p.allow_in
  let p = P(..p, allow_in: True)
  case peek(p) {
    DotDotDot -> {
      use #(p2, arg_expr) <- result.try(parse_assignment_expression(advance(p)))
      Ok(#(
        P(..p2, allow_in: saved_allow_in),
        ast.SpreadElement(argument: arg_expr),
      ))
    }
    _ -> {
      use #(p2, arg_expr) <- result.try(parse_assignment_expression(p))
      Ok(#(P(..p2, allow_in: saved_allow_in), arg_expr))
    }
  }
}

fn parse_primary_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  // Default to no name — only the Identifier branch sets Some(name).
  let p = P(..p, last_expr_name: None)
  case peek(p) {
    Identifier -> {
      let val = peek_value(p)
      case val {
        "enum" -> Error(EnumReservedWord(pos_of(p)))
        "implements"
        | "interface"
        | "package"
        | "private"
        | "protected"
        | "public" -> {
          use <- bool.guard(
            p.strict,
            Error(ReservedWordStrictMode(val, pos_of(p))),
          )
          Ok(#(
            P(
              ..advance(p),
              last_expr_assignable: True,
              last_expr_name: Some(val),
            ),
            ast.Identifier(name: val),
          ))
        }
        _ -> {
          let eval_args_target = case p.strict, val {
            True, "eval" | True, "arguments" -> True
            _, _ -> p.has_eval_args_target
          }
          Ok(#(
            P(
              ..advance(p),
              last_expr_assignable: True,
              last_expr_name: Some(val),
              has_eval_args_target: eval_args_target,
            ),
            ast.Identifier(name: val),
          ))
        }
      }
    }
    Number -> {
      use <- bool.guard(
        p.strict && is_legacy_octal_number(peek_value(p)),
        Error(OctalLiteralStrictMode(pos_of(p))),
      )
      let raw = peek_value(p)
      Ok(#(
        P(..advance(p), last_expr_assignable: False),
        ast.NumberLiteral(value: parse_js_number(raw)),
      ))
    }
    KString -> {
      use <- bool.guard(
        p.strict && has_legacy_octal_escape(peek_value(p)),
        Error(OctalEscapeStrictMode(pos_of(p))),
      )
      Ok(#(
        P(..advance(p), last_expr_assignable: False),
        ast.StringExpression(value: peek_value(p)),
      ))
    }
    KTrue ->
      Ok(#(
        P(..advance(p), last_expr_assignable: False),
        ast.BooleanLiteral(value: True),
      ))
    KFalse ->
      Ok(#(
        P(..advance(p), last_expr_assignable: False),
        ast.BooleanLiteral(value: False),
      ))
    Null -> Ok(#(P(..advance(p), last_expr_assignable: False), ast.NullLiteral))
    Undefined ->
      Ok(#(
        P(..advance(p), last_expr_assignable: False),
        ast.UndefinedExpression,
      ))
    TemplateLiteral -> {
      let raw = peek_value(p)
      use #(quasis, expressions) <- result.map(parse_template_raw(p, raw))
      #(
        P(..advance(p), last_expr_assignable: False),
        ast.TemplateLiteral(quasis:, expressions:),
      )
    }
    This ->
      Ok(#(P(..advance(p), last_expr_assignable: False), ast.ThisExpression))
    Super -> {
      // super.x and super[x] can appear in parenthesized contexts
      // like new (super.x) or arrow body () => (super.c)
      // Note: super() call is only handled in parse_call_expression
      let next = peek_at(p, 1)
      case next {
        Dot | LeftBracket ->
          case p.allow_super_property {
            True -> Ok(#(advance(p), ast.SuperExpression))
            False -> Error(UnexpectedSuper(pos_of(p)))
          }
        _ -> Error(UnexpectedSuper(pos_of(p)))
      }
    }
    LeftParen -> {
      let p2 = advance(p)
      case peek(p2) {
        RightParen -> {
          // Empty parens — only valid as arrow function params.
          // If we got here, try_arrow_function already failed to find =>,
          // so this is an error.
          Error(UnexpectedCloseParen(pos_of(p)))
        }
        _ -> {
          // Restore allow_in inside parenthesized expressions,
          // then put it back when we leave
          let saved_allow_in = p2.allow_in
          let p2 = P(..p2, allow_in: True)
          use #(p3, expr) <- result.try(parse_expression(p2))
          use p4 <- result.try(expect(
            P(..p3, allow_in: saved_allow_in),
            RightParen,
          ))
          Ok(#(p4, expr))
        }
      }
    }
    LeftBracket -> parse_array_literal(p) |> set_not_assignable
    LeftBrace -> parse_object_literal(p) |> set_not_assignable
    Function -> parse_function_expression(p)
    Class -> parse_class_expression(p)
    Async -> {
      case peek_at(p, 1) {
        Function -> parse_async_function_expression(p)
        _ -> {
          let val = peek_value(p)
          Ok(#(
            P(..advance(p), last_expr_assignable: True),
            ast.Identifier(name: val),
          ))
        }
      }
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
    RegularExpression -> {
      // Pre-tokenized regex: value is /pattern/flags
      let raw = peek_value(p)
      let #(pattern, flags) = split_regex_value(raw)
      Ok(#(
        P(..advance(p), last_expr_assignable: False),
        ast.RegExpLiteral(pattern: pattern, flags: flags),
      ))
    }
    New -> parse_new_expression(p)
    _ ->
      case is_contextual_keyword(peek(p)) {
        True -> {
          let val = peek_value(p)
          // Check for contextual keywords that are restricted in certain contexts
          case peek(p) {
            Yield ->
              case p.strict {
                True -> Error(YieldReservedStrictMode(pos_of(p)))
                False ->
                  case p.in_generator {
                    True -> Error(YieldInGenerator(pos_of(p)))
                    False ->
                      Ok(#(
                        P(..advance(p), last_expr_assignable: True),
                        ast.Identifier(name: val),
                      ))
                  }
              }
            Await ->
              case p.mode {
                Module -> Error(AwaitInModule(pos_of(p)))
                Script ->
                  case p.in_async {
                    True -> Error(AwaitInAsyncFunction(pos_of(p)))
                    False ->
                      Ok(#(
                        P(..advance(p), last_expr_assignable: True),
                        ast.Identifier(name: val),
                      ))
                  }
              }
            Let ->
              case p.strict {
                True -> Error(LetIdentifierStrictMode(pos_of(p)))
                False ->
                  Ok(#(
                    P(..advance(p), last_expr_assignable: True),
                    ast.Identifier(name: val),
                  ))
              }
            Static ->
              case p.strict {
                True -> Error(StaticReservedStrictMode(pos_of(p)))
                False ->
                  Ok(#(
                    P(..advance(p), last_expr_assignable: True),
                    ast.Identifier(name: val),
                  ))
              }
            _ ->
              Ok(#(
                P(..advance(p), last_expr_assignable: True),
                ast.Identifier(name: val),
              ))
          }
        }
        False ->
          Error(UnexpectedToken(token_kind_to_string(peek(p)), pos_of(p)))
      }
  }
}

fn parse_array_literal(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let p2 = advance(p)
  // Restore allow_in inside array literals
  let saved_allow_in = p.allow_in
  let p2 = P(..p2, allow_in: True)
  use #(p3, elems) <- result.try(parse_array_elements(p2, []))
  Ok(#(
    P(..p3, allow_in: saved_allow_in),
    ast.ArrayExpression(elements: list.reverse(elems)),
  ))
}

fn parse_array_elements(
  p: P,
  acc: List(Option(ast.Expression)),
) -> Result(#(P, List(Option(ast.Expression))), ParseError) {
  case peek(p) {
    RightBracket -> Ok(#(advance(p), acc))
    Comma -> parse_array_elements(advance(p), [None, ..acc])
    DotDotDot -> {
      let spread_start = peek_at(p, 1)
      let p2 = advance(p)
      use #(p3, expr) <- result.try(parse_assignment_expression(p2))
      // If spread target is not a valid assignment target (e.g. ...0,
      // ...new x), mark as invalid for destructuring. Allow nested
      // patterns like ...{0: b} or ...[a]
      let p3 = case
        p3.last_expr_assignable
        || spread_start == LeftBrace
        || spread_start == LeftBracket
      {
        True -> p3
        False -> P(..p3, has_invalid_pattern: True)
      }
      let elem = Some(ast.SpreadElement(argument: expr))
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
      let elem_start = peek(p)
      use #(p2, expr) <- result.try(parse_assignment_expression(p))
      // Mark invalid pattern if element is not a valid destructuring
      // component. Valid: assignable target, nested pattern ({}/[]),
      // or assignment expression (covers AssignmentPattern: target = default)
      let p2 = case
        p2.last_expr_assignable
        || p2.last_expr_is_assignment
        || elem_start == LeftBrace
        || elem_start == LeftBracket
      {
        True -> p2
        False -> P(..p2, has_invalid_pattern: True)
      }
      case peek(p2) {
        Comma -> parse_array_elements(advance(p2), [Some(expr), ..acc])
        RightBracket -> Ok(#(advance(p2), [Some(expr), ..acc]))
        _ -> Error(ExpectedCommaOrBracketInExpr(pos_of(p2)))
      }
    }
  }
}

fn parse_object_literal(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let p2 = advance(p)
  // Restore allow_in inside object literals
  let saved_allow_in = p.allow_in
  let p2 = P(..p2, allow_in: True)
  use #(p3, props) <- result.try(parse_object_properties(p2, False, []))
  Ok(#(
    P(..p3, allow_in: saved_allow_in),
    ast.ObjectExpression(properties: list.reverse(props)),
  ))
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
        Comma -> parse_object_properties(advance(p3), has_proto, [prop, ..acc])
        RightBrace -> Ok(#(advance(p3), [prop, ..acc]))
        _ -> Error(ExpectedCommaOrBraceInObject(pos_of(p3)))
      }
    }
    _ -> {
      // Check for __proto__ duplicate
      let is_proto = is_proto_property(p)
      case is_proto && has_proto {
        True -> Error(DuplicateProtoProperty(pos_of(p)))
        False -> {
          use #(p2, prop) <- result.try(parse_object_property(p))
          case peek(p2) {
            Comma ->
              parse_object_properties(advance(p2), has_proto || is_proto, [
                prop,
                ..acc
              ])
            RightBrace -> Ok(#(advance(p2), [prop, ..acc]))
            _ -> Error(ExpectedCommaOrBraceInObjectLiteral(pos_of(p2)))
          }
        }
      }
    }
  }
}

fn is_proto_property(p: P) -> Bool {
  // Check if current property is __proto__: value (not shorthand, not method, not computed)
  case peek(p) {
    Identifier ->
      case peek_value(p) {
        "__proto__" ->
          case peek_at(p, 1) {
            Colon -> True
            _ -> False
          }
        _ -> False
      }
    KString -> {
      let val = peek_value(p)
      case val {
        "__proto__" ->
          case peek_at(p, 1) {
            Colon -> True
            _ -> False
          }
        _ -> False
      }
    }
    _ -> False
  }
}

fn parse_object_property(p: P) -> Result(#(P, ast.Property), ParseError) {
  // Handle: get/set/async methods, generators, computed keys, shorthand
  let has_async = case peek(p) {
    Async ->
      case peek_at(p, 1) {
        LeftParen | Comma | RightBrace | Colon -> False
        _ -> True
      }
    _ -> False
  }
  let p2 = case has_async {
    True -> advance(p)
    False -> p
  }

  // get/set accessor — track which kind for param count validation
  let accessor_kind = case peek(p2) {
    Identifier -> {
      let val = peek_value(p2)
      case val {
        "get" ->
          case peek_at(p2, 1) {
            LeftParen | Comma | RightBrace | Colon -> ""
            _ -> "get"
          }
        "set" ->
          case peek_at(p2, 1) {
            LeftParen | Comma | RightBrace | Colon -> ""
            _ -> "set"
          }
        _ -> ""
      }
    }
    _ -> ""
  }
  let p3 = case accessor_kind {
    "get" | "set" -> advance(p2)
    _ -> p2
  }

  // Generator
  let is_generator = peek(p3) == Star
  let p4 = case is_generator {
    True -> advance(p3)
    False -> p3
  }

  // Property name (identifier, string, number, computed)
  // Remember token kind for shorthand validation — only identifiers/contextual
  // keywords can be shorthand properties.
  let prop_name_kind = peek(p4)
  let prop_name_value = peek_value(p4)
  let is_valid_shorthand = case prop_name_kind {
    Identifier -> True
    _ -> is_contextual_keyword(prop_name_kind)
  }
  use #(p5, key_expr, is_computed) <- result.try(parse_property_name(p4))
  // Generator shorthand (*name) must be a method — must have (
  case is_generator && peek(p5) != LeftParen {
    True -> Error(UnexpectedToken(token_kind_to_string(peek(p5)), pos_of(p5)))
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
        key_expr,
        is_computed,
      )
  }
}

fn parse_object_property_value(
  p: P,
  p5: P,
  has_async: Bool,
  accessor_kind: String,
  is_generator: Bool,
  prop_name_kind: TokenKind,
  prop_name_value: String,
  is_valid_shorthand: Bool,
  key_expr: ast.Expression,
  is_computed: Bool,
) -> Result(#(P, ast.Property), ParseError) {
  case peek(p5) {
    LeftParen -> {
      // Method — validate getter/setter params
      // Object methods allow super.x but not super()
      // Methods make the object invalid as destructuring target
      let p5 = P(..p5, has_invalid_pattern: True)
      let #(kind, is_method) = case accessor_kind {
        "get" -> #(ast.Get, False)
        "set" -> #(ast.Set, False)
        _ -> #(ast.Init, True)
      }
      use #(p6, params, body) <- result.try(case accessor_kind {
        "get" ->
          parse_getter_params_and_body(enter_method_context(
            p5,
            False,
            False,
            False,
            False,
          ))
          |> restore_context_fn(p)
        "set" ->
          parse_setter_params_and_body(enter_method_context(
            p5,
            False,
            False,
            False,
            False,
          ))
          |> restore_context_fn(p)
        _ ->
          parse_function_params_and_body(enter_method_context(
            p5,
            is_generator,
            has_async,
            False,
            False,
          ))
          |> restore_context_fn(p)
      })
      Ok(#(
        p6,
        ast.Property(
          key: key_expr,
          value: ast.FunctionExpression(
            name: None,
            params: params,
            body: body,
            is_generator: is_generator,
            is_async: has_async,
          ),
          kind: kind,
          computed: is_computed,
          shorthand: False,
          method: is_method,
        ),
      ))
    }
    Colon -> {
      // key: value — mark invalid pattern if value is not a valid
      // destructuring component. Valid: assignable target, nested
      // pattern ({}/[]), or assignment (covers AssignmentPattern)
      let p6 = advance(p5)
      let value_start = peek(p6)
      use #(p7, expr) <- result.try(parse_assignment_expression(p6))
      let p7 = case
        p7.last_expr_assignable
        || p7.last_expr_is_assignment
        || value_start == LeftBrace
        || value_start == LeftBracket
      {
        True -> p7
        False -> P(..p7, has_invalid_pattern: True)
      }
      Ok(#(
        p7,
        ast.Property(
          key: key_expr,
          value: expr,
          kind: ast.Init,
          computed: is_computed,
          shorthand: False,
          method: False,
        ),
      ))
    }
    Equal -> {
      // Shorthand with default (cover grammar) — only valid in
      // destructuring patterns / arrow params, not in expressions.
      // Set the cover flag so the caller can reject this if needed.
      // Only identifiers can have shorthand default
      case is_valid_shorthand {
        False ->
          Error(UnexpectedToken(
            token_kind_to_string(prop_name_kind),
            pos_of(p5),
          ))
        True -> {
          let p6 = advance(p5)
          use #(p7, default_expr) <- result.try(parse_assignment_expression(p6))
          let eval_target = case p.strict, prop_name_value {
            True, "eval" | True, "arguments" -> True
            _, _ -> p7.has_eval_args_target
          }
          Ok(#(
            P(
              ..p7,
              has_cover_initializer: True,
              has_eval_args_target: eval_target,
            ),
            ast.Property(
              key: key_expr,
              value: ast.AssignmentExpression(
                operator: ast.Assign,
                left: ast.Identifier(name: prop_name_value),
                right: default_expr,
              ),
              kind: ast.Init,
              computed: False,
              shorthand: True,
              method: False,
            ),
          ))
        }
      }
    }
    _ -> {
      // Shorthand property — only valid for identifiers
      case is_valid_shorthand {
        False ->
          Error(UnexpectedToken(
            token_kind_to_string(prop_name_kind),
            pos_of(p5),
          ))
        True -> {
          // In strict mode, eval/arguments as shorthand property marks
          // a potential invalid destructuring target
          let p5 = case p.strict, prop_name_value {
            True, "eval" | True, "arguments" ->
              P(..p5, has_eval_args_target: True)
            _, _ -> p5
          }
          Ok(#(
            p5,
            ast.Property(
              key: key_expr,
              value: ast.Identifier(name: prop_name_value),
              kind: ast.Init,
              computed: False,
              shorthand: True,
              method: False,
            ),
          ))
        }
      }
    }
  }
}

fn parse_function_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let p2 = advance(p)
  // Optional generator
  let is_generator = peek(p2) == Star
  let p3 = case is_generator {
    True -> advance(p2)
    False -> p2
  }
  // Validate the optional name against the INNER context (the expression's own
  // is_generator/is_async), not the outer context. Function expression names
  // are scoped to the expression body, so e.g. `function yield(){}` is valid
  // inside a generator even though `yield` is a keyword there.
  let p3_for_name = P(..p3, in_generator: is_generator, in_async: False)
  let func_name = get_simple_binding_name(p3_for_name)
  use p4 <- result.try(eat_optional_name(p3_for_name))
  // Store function name for retroactive strict mode validation
  let p4 = P(..p4, pending_strict_name: string.to_option(func_name))
  use #(p5, params, body) <- result.try(
    parse_function_params_and_body(enter_function_context(
      // Restore the original token state from p4 but use original context
      P(..p4, in_generator: p3.in_generator, in_async: p3.in_async),
      is_generator,
      False,
    ))
    |> restore_context_fn(p),
  )
  let name_opt = case func_name {
    "" -> None
    n -> Some(n)
  }
  Ok(#(
    p5,
    ast.FunctionExpression(
      name: name_opt,
      params: params,
      body: body,
      is_generator: is_generator,
      is_async: False,
    ),
  ))
}

fn parse_async_function_expression(
  p: P,
) -> Result(#(P, ast.Expression), ParseError) {
  let p2 = advance(advance(p))
  // Optional generator
  let is_generator = peek(p2) == Star
  let p3 = case is_generator {
    True -> advance(p2)
    False -> p2
  }
  // Same as parse_function_expression: validate name against INNER context
  let p3_for_name = P(..p3, in_generator: is_generator, in_async: True)
  let func_name = get_simple_binding_name(p3_for_name)
  use p4 <- result.try(eat_optional_name(p3_for_name))
  let p4 = P(..p4, pending_strict_name: string.to_option(func_name))
  use #(p5, params, body) <- result.try(
    parse_function_params_and_body(enter_function_context(
      P(..p4, in_generator: p3.in_generator, in_async: p3.in_async),
      is_generator,
      True,
    ))
    |> restore_context_fn(p),
  )
  let name_opt = case func_name {
    "" -> None
    n -> Some(n)
  }
  Ok(#(
    p5,
    ast.FunctionExpression(
      name: name_opt,
      params: params,
      body: body,
      is_generator: is_generator,
      is_async: True,
    ),
  ))
}

fn parse_class_expression(p: P) -> Result(#(P, ast.Expression), ParseError) {
  let p2 = advance(p)
  case peek(p2) {
    Extends | LeftBrace -> {
      use #(p3, super_class, body) <- result.try(parse_class_tail(p2))
      Ok(#(
        p3,
        ast.ClassExpression(name: None, super_class: super_class, body: body),
      ))
    }
    _ -> {
      // Class names are always checked in strict context
      let name = get_simple_binding_name(p2)
      use p3 <- result.try(eat_optional_name(P(..p2, strict: True)))
      use #(p4, super_class, body) <- result.try(parse_class_tail(
        P(..p3, strict: p2.strict),
      ))
      let name_opt = case name {
        "" -> None
        n -> Some(n)
      }
      Ok(#(
        p4,
        ast.ClassExpression(
          name: name_opt,
          super_class: super_class,
          body: body,
        ),
      ))
    }
  }
}

fn parse_regex_literal(p: P) -> Result(#(P, ast.Expression), ParseError) {
  // Re-lex from source as a regex literal.
  // The current token is Slash or SlashEqual at some position.
  let start_pos = pos_of(p)
  // Scan the regex body starting after the opening /
  let body_start = start_pos + 1
  case scan_regex_source(p.bytes, body_start, False) {
    Ok(end_pos) -> {
      // end_pos is past the closing /, now skip optional flags
      use #(flags_end, flags) <- result.try(skip_regex_flags(p.bytes, end_pos))
      // If /u flag is present, validate no lone braces in the body
      // body is from body_start to end_pos - 1 (exclusive of closing /)
      use Nil <- result.try(case list.contains(flags, "u") {
        True ->
          validate_regex_unicode_body(p.bytes, body_start, end_pos - 1)
          |> result.map_error(fn(msg) {
            LexerError(message: msg, pos: start_pos)
          })
        False -> Ok(Nil)
      })
      // Extract pattern body and flags as strings
      let pattern =
        byte_slice_source(p.bytes, body_start, end_pos - 1 - body_start)
      let flags_str = string.join(flags, "")
      // Now skip tokens until we're past this regex in the token stream
      use p2 <- result.try(skip_tokens_past(p, flags_end))
      Ok(#(p2, ast.RegExpLiteral(pattern: pattern, flags: flags_str)))
    }
    Error(msg) -> Error(LexerError(message: msg, pos: start_pos))
  }
}

/// Validate regex body with the /u flag.
/// In Unicode mode:
/// 1. `{` must start a valid quantifier ({n}, {n,}, {n,m}) and `}` must close one
/// 2. Quantifiers (*, +, ?, {n,m}) cannot follow assertion groups (?=, ?!, ?<=, ?<!)
/// Braces inside character classes [...] are allowed.
fn validate_regex_unicode_body(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(Nil, String) {
  validate_regex_unicode_loop(bytes, pos, end, False, False)
}

fn validate_regex_unicode_loop(
  bytes: BitArray,
  pos: Int,
  end: Int,
  in_class: Bool,
  after_assertion: Bool,
) -> Result(Nil, String) {
  case pos >= end {
    True -> Ok(Nil)
    False -> {
      let ch = char_at_source(bytes, pos)
      case ch {
        "\\" -> {
          // Skip escaped character — not a quantifier, not an assertion.
          // For \u escapes, skip the full escape sequence length,
          // not just 2 chars, otherwise \u{XXXX} leaves the { exposed
          // and the validator incorrectly rejects it as a lone brace.
          let escape_result = case char_at_source(bytes, pos + 1) {
            "u" ->
              case char_at_source(bytes, pos + 2) {
                "{" -> {
                  // \u{...} — find the closing }, validate codepoint value
                  let after = skip_regex_hex_run(bytes, pos + 3, end)
                  case char_at_source(bytes, after) {
                    "}" -> {
                      // Validate the hex value is <= 0x10FFFF
                      let hex_str =
                        byte_slice_source(bytes, pos + 3, after - { pos + 3 })
                      case parse_hex_value(hex_str) {
                        Ok(val) ->
                          case val > 0x10FFFF {
                            True ->
                              Error(
                                "Invalid regular expression: Unicode escape value > 0x10FFFF",
                              )
                            False -> Ok(after + 1 - pos)
                          }
                        Error(_) -> Ok(2)
                      }
                    }
                    _ -> Ok(2)
                  }
                }
                _ -> {
                  // \uXXXX — check for 4 hex digits
                  case
                    pos + 6 <= end
                    && is_hex_char(char_at_source(bytes, pos + 2))
                    && is_hex_char(char_at_source(bytes, pos + 3))
                    && is_hex_char(char_at_source(bytes, pos + 4))
                    && is_hex_char(char_at_source(bytes, pos + 5))
                  {
                    True -> Ok(6)
                    False -> Ok(2)
                  }
                }
              }
            "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
              // Backreferences (\1-\9) are not allowed in Unicode mode
              Error(
                "Invalid regular expression: decimal escape in Unicode mode",
              )
            _ -> Ok(2)
          }
          use escape_len <- result.try(escape_result)
          validate_regex_unicode_loop(
            bytes,
            pos + escape_len,
            end,
            in_class,
            False,
          )
        }
        "[" -> validate_regex_unicode_loop(bytes, pos + 1, end, True, False)
        "]" ->
          case in_class {
            True ->
              validate_regex_unicode_loop(bytes, pos + 1, end, False, False)
            False ->
              validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
          }
        "(" ->
          case in_class {
            True ->
              validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
            False -> {
              // Check if this is an assertion group: (?=, (?!, (?<=, (?<!
              let is_assertion = is_assertion_group(bytes, pos + 1, end)
              case is_assertion {
                True -> {
                  // Find matching closing paren, then mark after_assertion
                  case find_matching_paren(bytes, pos + 1, end) {
                    Ok(close_pos) ->
                      validate_regex_unicode_loop(
                        bytes,
                        close_pos + 1,
                        end,
                        in_class,
                        True,
                      )
                    // If we can't find matching paren, just continue normally
                    // (the error will be caught elsewhere)
                    Error(_) ->
                      validate_regex_unicode_loop(
                        bytes,
                        pos + 1,
                        end,
                        in_class,
                        False,
                      )
                  }
                }
                False -> {
                  // Regular group — find matching close and mark NOT assertion
                  case find_matching_paren(bytes, pos + 1, end) {
                    Ok(close_pos) ->
                      validate_regex_unicode_loop(
                        bytes,
                        close_pos + 1,
                        end,
                        in_class,
                        False,
                      )
                    Error(_) ->
                      validate_regex_unicode_loop(
                        bytes,
                        pos + 1,
                        end,
                        in_class,
                        False,
                      )
                  }
                }
              }
            }
          }
        // Quantifiers: *, +, ?
        "*" | "+" ->
          case in_class {
            True ->
              validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
            False ->
              case after_assertion {
                True ->
                  Error(
                    "Invalid regular expression: quantifier on assertion in Unicode mode",
                  )
                False -> {
                  // Skip optional ? for lazy quantifier
                  let next_pos = case char_at_source(bytes, pos + 1) {
                    "?" -> pos + 2
                    _ -> pos + 1
                  }
                  validate_regex_unicode_loop(
                    bytes,
                    next_pos,
                    end,
                    in_class,
                    False,
                  )
                }
              }
          }
        "?" ->
          case in_class {
            True ->
              validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
            False ->
              case after_assertion {
                True ->
                  Error(
                    "Invalid regular expression: quantifier on assertion in Unicode mode",
                  )
                False -> {
                  // ? as quantifier — skip optional ? for lazy
                  let next_pos = case char_at_source(bytes, pos + 1) {
                    "?" -> pos + 2
                    _ -> pos + 1
                  }
                  validate_regex_unicode_loop(
                    bytes,
                    next_pos,
                    end,
                    in_class,
                    False,
                  )
                }
              }
          }
        "{" ->
          case in_class {
            True ->
              validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
            False ->
              // Try to parse as valid quantifier: {digits}, {digits,}, {digits,digits}
              case try_parse_quantifier_brace(bytes, pos + 1, end) {
                Ok(after_brace) ->
                  case after_assertion {
                    True ->
                      Error(
                        "Invalid regular expression: quantifier on assertion in Unicode mode",
                      )
                    False -> {
                      // Skip optional ? for lazy quantifier
                      let next_pos = case char_at_source(bytes, after_brace) {
                        "?" -> after_brace + 1
                        _ -> after_brace
                      }
                      validate_regex_unicode_loop(
                        bytes,
                        next_pos,
                        end,
                        in_class,
                        False,
                      )
                    }
                  }
                Error(_) ->
                  Error("Invalid regular expression: lone '{' in Unicode mode")
              }
          }
        "}" ->
          case in_class {
            True ->
              validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
            False ->
              Error("Invalid regular expression: lone '}' in Unicode mode")
          }
        // Any other character resets after_assertion
        _ -> validate_regex_unicode_loop(bytes, pos + 1, end, in_class, False)
      }
    }
  }
}

/// Parse a hex string into an integer value.
fn parse_hex_value(hex_str: String) -> Result(Int, Nil) {
  int.base_parse(hex_str, 16)
}

/// Check if position starts an assertion group: ?=, ?!, ?<=, ?<!
fn is_assertion_group(bytes: BitArray, pos: Int, end: Int) -> Bool {
  case pos >= end {
    True -> False
    False -> {
      let ch = char_at_source(bytes, pos)
      case ch {
        "?" -> {
          let ch2 = char_at_source(bytes, pos + 1)
          case ch2 {
            "=" | "!" -> True
            "<" -> {
              let ch3 = char_at_source(bytes, pos + 2)
              case ch3 {
                "=" | "!" -> True
                _ -> False
              }
            }
            _ -> False
          }
        }
        _ -> False
      }
    }
  }
}

/// Find the matching closing paren for a group, accounting for nesting,
/// character classes, and escapes. pos starts inside the group (after opening `(`).
fn find_matching_paren(bytes: BitArray, pos: Int, end: Int) -> Result(Int, Nil) {
  find_matching_paren_loop(bytes, pos, end, 1, False)
}

fn find_matching_paren_loop(
  bytes: BitArray,
  pos: Int,
  end: Int,
  depth: Int,
  in_class: Bool,
) -> Result(Int, Nil) {
  case pos >= end {
    True -> Error(Nil)
    False -> {
      let ch = char_at_source(bytes, pos)
      case ch {
        "\\" ->
          // Skip escaped char
          find_matching_paren_loop(bytes, pos + 2, end, depth, in_class)
        "[" -> find_matching_paren_loop(bytes, pos + 1, end, depth, True)
        "]" ->
          case in_class {
            True -> find_matching_paren_loop(bytes, pos + 1, end, depth, False)
            False ->
              find_matching_paren_loop(bytes, pos + 1, end, depth, in_class)
          }
        "(" ->
          case in_class {
            True ->
              find_matching_paren_loop(bytes, pos + 1, end, depth, in_class)
            False ->
              find_matching_paren_loop(bytes, pos + 1, end, depth + 1, in_class)
          }
        ")" ->
          case in_class {
            True ->
              find_matching_paren_loop(bytes, pos + 1, end, depth, in_class)
            False ->
              case depth <= 1 {
                True -> Ok(pos)
                False ->
                  find_matching_paren_loop(
                    bytes,
                    pos + 1,
                    end,
                    depth - 1,
                    in_class,
                  )
              }
          }
        _ -> find_matching_paren_loop(bytes, pos + 1, end, depth, in_class)
      }
    }
  }
}

/// Try to parse a valid quantifier starting after the opening `{`.
/// Valid forms: {n}, {n,}, {n,m} where n and m are decimal digits.
/// Returns the position after the closing `}` on success.
fn try_parse_quantifier_brace(
  bytes: BitArray,
  pos: Int,
  end: Int,
) -> Result(Int, Nil) {
  // Must start with at least one digit
  case skip_digits(bytes, pos, end) {
    Ok(after_digits) ->
      case after_digits == pos {
        // No digits found — not a valid quantifier
        True -> Error(Nil)
        False -> {
          let ch = char_at_source(bytes, after_digits)
          case ch {
            "}" ->
              // {n} form
              Ok(after_digits + 1)
            "," -> {
              // Could be {n,} or {n,m}
              let after_comma = after_digits + 1
              case skip_digits(bytes, after_comma, end) {
                Ok(after_digits2) -> {
                  let ch2 = char_at_source(bytes, after_digits2)
                  case ch2 {
                    "}" ->
                      // {n,} or {n,m} form
                      Ok(after_digits2 + 1)
                    _ -> Error(Nil)
                  }
                }
                Error(_) -> Error(Nil)
              }
            }
            _ -> Error(Nil)
          }
        }
      }
    Error(_) -> Error(Nil)
  }
}

/// Skip decimal digits from pos, returning the position after the last digit.
/// Always succeeds (returns pos unchanged if no digits found).
fn skip_digits(bytes: BitArray, pos: Int, end: Int) -> Result(Int, Nil) {
  case pos >= end {
    True -> Ok(pos)
    False -> {
      let ch = char_at_source(bytes, pos)
      case ch {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
          skip_digits(bytes, pos + 1, end)
        _ -> Ok(pos)
      }
    }
  }
}

fn scan_regex_source(
  bytes: BitArray,
  pos: Int,
  in_class: Bool,
) -> Result(Int, String) {
  let ch = char_at_source(bytes, pos)
  case ch {
    "" -> Error("Unterminated regular expression")
    "\n" | "\r" -> Error("Unterminated regular expression")
    "\\" -> {
      // Escaped character — skip next char
      let next = char_at_source(bytes, pos + 1)
      case next {
        "" | "\n" | "\r" -> Error("Unterminated regular expression")
        _ -> scan_regex_source(bytes, pos + 2, in_class)
      }
    }
    "[" -> scan_regex_source(bytes, pos + 1, True)
    "]" ->
      case in_class {
        True -> scan_regex_source(bytes, pos + 1, False)
        False -> scan_regex_source(bytes, pos + 1, in_class)
      }
    "/" ->
      case in_class {
        True -> scan_regex_source(bytes, pos + 1, in_class)
        False -> Ok(pos + 1)
      }
    _ -> scan_regex_source(bytes, pos + 1, in_class)
  }
}

fn skip_regex_flags(
  bytes: BitArray,
  pos: Int,
) -> Result(#(Int, List(String)), ParseError) {
  scan_regex_flags(bytes, pos, [])
}

fn scan_regex_flags(
  bytes: BitArray,
  pos: Int,
  seen: List(String),
) -> Result(#(Int, List(String)), ParseError) {
  let ch = char_at_source(bytes, pos)
  case ch {
    "g" | "i" | "m" | "s" | "u" | "v" | "y" | "d" ->
      case list.contains(seen, ch) {
        True ->
          Error(LexerError(
            "Duplicate regular expression flag '" <> ch <> "'",
            pos,
          ))
        False -> scan_regex_flags(bytes, pos + 1, [ch, ..seen])
      }
    _ -> Ok(#(pos, seen))
  }
}

/// Split a pre-tokenized regex value like "/pattern/flags" into (pattern, flags).
fn split_regex_value(raw: String) -> #(String, String) {
  // Strip leading /
  let body = string.drop_start(raw, 1)
  // Split on "/" and rejoin all but the last segment as the pattern
  let parts = string.split(body, "/")
  case parts {
    [single] -> #(single, "")
    _ -> {
      let assert Ok(flags) = list.last(parts)
      let pattern =
        parts
        |> list.take(list.length(parts) - 1)
        |> string.join("/")
      #(pattern, flags)
    }
  }
}

fn skip_tokens_past(p: P, target_pos: Int) -> Result(P, ParseError) {
  case peek(p) {
    Eof -> Ok(p)
    _ -> {
      let token_end = pos_of(p) + peek_raw_len(p)
      case token_end >= target_pos {
        True -> Ok(advance(p))
        False -> skip_tokens_past(advance(p), target_pos)
      }
    }
  }
}

/// O(1) character access into the source bytes (ASCII only, for regex scanning).
fn char_at_source(bytes: BitArray, pos: Int) -> String {
  case bit_array.slice(bytes, pos, 1) {
    Error(_) -> ""
    Ok(<<byte>>) if byte < 0x80 -> {
      case bit_array.to_string(<<byte>>) {
        Ok(s) -> s
        Error(_) -> ""
      }
    }
    _ -> ""
  }
}

/// O(1) byte slice from the source bytes.
fn byte_slice_source(bytes: BitArray, start: Int, len: Int) -> String {
  case bit_array.slice(bytes, start, len) {
    Ok(s) ->
      case bit_array.to_string(s) {
        Ok(str) -> str
        Error(_) -> ""
      }
    Error(_) -> ""
  }
}

/// Check if a single character is a hex digit (0-9, a-f, A-F).
fn is_hex_char(ch: String) -> Bool {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    "a" | "b" | "c" | "d" | "e" | "f" -> True
    "A" | "B" | "C" | "D" | "E" | "F" -> True
    _ -> False
  }
}

/// Skip consecutive hex digits in the source bytes starting at pos,
/// returning the position after the last hex digit.
fn skip_regex_hex_run(bytes: BitArray, pos: Int, end: Int) -> Int {
  case pos >= end {
    True -> pos
    False ->
      case is_hex_char(char_at_source(bytes, pos)) {
        True -> skip_regex_hex_run(bytes, pos + 1, end)
        False -> pos
      }
  }
}

// ---- Import/Export ----

/// Helper: expect 'from' keyword followed by a string module specifier, then eat semicolon.
/// Returns the parser state and the parsed StringLiteral source.
fn expect_from_module_specifier(
  p: P,
) -> Result(#(P, ast.StringLiteral), ParseError) {
  use p2 <- result.try(expect(p, From))
  case peek(p2) {
    KString -> {
      let value = peek_value(p2)
      use p3 <- result.try(eat_semicolon(advance(p2)))
      Ok(#(p3, ast.StringLit(value:)))
    }
    _ -> Error(ExpectedModuleSpecifier(pos_of(p2)))
  }
}

fn parse_import_declaration(p: P) -> Result(#(P, ast.ModuleItem), ParseError) {
  let p2 = advance(p)
  case peek(p2) {
    KString -> {
      // import "module"
      let value = peek_value(p2)
      use p3 <- result.try(eat_semicolon(advance(p2)))
      Ok(#(
        p3,
        ast.ImportDeclaration(specifiers: [], source: ast.StringLit(value:)),
      ))
    }
    Star -> {
      // import * as name from "module"
      let p3 = advance(p2)
      use p4 <- result.try(expect(p3, As))
      let binding_name = peek_value(p4)
      use p5 <- result.try(expect_identifier(p4))
      use p5b <- result.try(check_duplicate_import_binding(p5, binding_name))
      use #(p6, source) <- result.try(expect_from_module_specifier(p5b))
      Ok(#(
        p6,
        ast.ImportDeclaration(
          specifiers: [ast.ImportNamespaceSpecifier(local: binding_name)],
          source:,
        ),
      ))
    }
    LeftBrace -> {
      // import { a, b } from "module"
      let p3 = advance(p2)
      use #(p4, specifiers) <- result.try(parse_import_specifiers(p3, []))
      use #(p5, source) <- result.try(expect_from_module_specifier(p4))
      Ok(#(p5, ast.ImportDeclaration(specifiers:, source:)))
    }
    Identifier -> {
      // import defaultExport from "module"
      // or import defaultExport, { ... } from "module"
      // or import defaultExport, * as name from "module"
      let default_name = peek_value(p2)
      use p2b <- result.try(check_duplicate_import_binding(p2, default_name))
      let default_spec = ast.ImportDefaultSpecifier(local: default_name)
      let p3 = advance(p2b)
      case peek(p3) {
        Comma -> {
          let p4 = advance(p3)
          case peek(p4) {
            Star -> {
              let p5 = advance(p4)
              use p6 <- result.try(expect(p5, As))
              let ns_name = peek_value(p6)
              use p7 <- result.try(expect_identifier(p6))
              use p7b <- result.try(check_duplicate_import_binding(p7, ns_name))
              use #(p8, source) <- result.try(expect_from_module_specifier(p7b))
              Ok(#(
                p8,
                ast.ImportDeclaration(
                  specifiers: [
                    default_spec,
                    ast.ImportNamespaceSpecifier(local: ns_name),
                  ],
                  source:,
                ),
              ))
            }
            LeftBrace -> {
              let p5 = advance(p4)
              use #(p6, named_specs) <- result.try(
                parse_import_specifiers(p5, []),
              )
              use #(p7, source) <- result.try(expect_from_module_specifier(p6))
              Ok(#(
                p7,
                ast.ImportDeclaration(
                  specifiers: [default_spec, ..named_specs],
                  source:,
                ),
              ))
            }
            _ -> Error(ExpectedBraceOrStarAfterComma(pos_of(p4)))
          }
        }
        From -> {
          use #(p4, source) <- result.try(expect_from_module_specifier(p3))
          Ok(#(p4, ast.ImportDeclaration(specifiers: [default_spec], source:)))
        }
        _ -> Error(ExpectedFromOrComma(pos_of(p3)))
      }
    }
    _ -> Error(ExpectedImportSpecifier(pos_of(p2)))
  }
}

fn parse_import_specifiers(
  p: P,
  acc: List(ast.ImportSpecifier),
) -> Result(#(P, List(ast.ImportSpecifier)), ParseError) {
  case peek(p) {
    RightBrace -> Ok(#(advance(p), list.reverse(acc)))
    _ -> {
      use #(p2, spec) <- result.try(parse_import_specifier(p))
      let acc = [spec, ..acc]
      case peek(p2) {
        Comma ->
          case peek_at(p2, 1) {
            RightBrace -> Ok(#(advance(advance(p2)), list.reverse(acc)))
            _ -> parse_import_specifiers(advance(p2), acc)
          }
        RightBrace -> Ok(#(advance(p2), list.reverse(acc)))
        _ -> Error(ExpectedCommaOrBraceInImport(pos_of(p2)))
      }
    }
  }
}

fn parse_import_specifier(p: P) -> Result(#(P, ast.ImportSpecifier), ParseError) {
  // name or name as alias or "string" as alias
  // The local binding name is the alias (after 'as') or the original name.
  let is_specifier_name =
    peek(p) == Identifier
    || peek(p) == KString
    || is_keyword_as_identifier(peek(p))
  case is_specifier_name {
    False -> Error(ExpectedImportSpecifierName(pos_of(p)))
    True -> {
      let original_name = peek_value(p)
      let original_kind = peek(p)
      let imported_name = original_name
      let p2 = advance(p)
      case peek(p2) {
        As -> {
          let p3 = advance(p2)
          // The alias is the local binding name — must be a valid binding identifier
          let binding_name = peek_value(p3)
          let binding_kind = peek(p3)
          use Nil <- result.try(check_import_binding_name(
            p3,
            binding_name,
            binding_kind,
          ))
          use p4 <- result.try(expect_identifier(p3))
          use p5 <- result.try(check_duplicate_import_binding(p4, binding_name))
          Ok(#(
            p5,
            ast.ImportNamedSpecifier(
              imported: imported_name,
              local: binding_name,
            ),
          ))
        }
        _ -> {
          // No alias: the original name is the local binding — must be valid
          use Nil <- result.try(check_import_binding_name(
            p,
            original_name,
            original_kind,
          ))
          use p3 <- result.try(check_duplicate_import_binding(p2, original_name))
          Ok(#(
            p3,
            ast.ImportNamedSpecifier(
              imported: imported_name,
              local: original_name,
            ),
          ))
        }
      }
    }
  }
}

/// Parse "export function name(){}" or "export async function name(){}".
/// Extracts the function name and registers it as an export name before parsing.
fn parse_export_named_function(
  p: P,
  is_async: Bool,
) -> Result(#(P, ast.Statement), ParseError) {
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
  case export_name != "" {
    True -> {
      use p2 <- result.try(check_duplicate_export(p, export_name))
      case is_async {
        True -> parse_async_function_declaration(p2)
        False -> parse_function_declaration(p2)
      }
    }
    False ->
      case is_async {
        True -> parse_async_function_declaration(p)
        False -> parse_function_declaration(p)
      }
  }
}

/// Parse "export class name {}".
/// Extracts the class name and registers it as an export name before parsing.
fn parse_export_named_class(p: P) -> Result(#(P, ast.Statement), ParseError) {
  // The name follows "class" keyword
  let export_name = peek_value_at(p, 1)
  case export_name != "" {
    True -> {
      use p2 <- result.try(check_duplicate_export(p, export_name))
      parse_class_declaration(p2)
    }
    False -> parse_class_declaration(p)
  }
}

fn parse_export_declaration(p: P) -> Result(#(P, ast.ModuleItem), ParseError) {
  let p2 = advance(p)
  case peek(p2) {
    Default -> {
      use p2b <- result.try(check_duplicate_export(p2, "default"))
      let p3 = advance(p2b)
      case peek(p3) {
        Function -> {
          use #(p4, stmt) <- result.try(
            parse_function_declaration_optional_name(p3),
          )
          Ok(#(
            p4,
            ast.ExportDefaultDeclaration(
              declaration: statement_to_default_export_expr(stmt),
            ),
          ))
        }
        Class -> {
          use #(p4, stmt) <- result.try(parse_class_declaration_optional_name(
            p3,
          ))
          Ok(#(
            p4,
            ast.ExportDefaultDeclaration(
              declaration: statement_to_default_export_expr(stmt),
            ),
          ))
        }
        Async ->
          case peek_at(p3, 1) {
            Function -> {
              use #(p4, stmt) <- result.try(
                parse_async_function_declaration_optional_name(p3),
              )
              Ok(#(
                p4,
                ast.ExportDefaultDeclaration(
                  declaration: statement_to_default_export_expr(stmt),
                ),
              ))
            }
            _ -> {
              use #(p4, expr) <- result.try(parse_assignment_expression(p3))
              use p5 <- result.try(eat_semicolon(p4))
              Ok(#(p5, ast.ExportDefaultDeclaration(declaration: expr)))
            }
          }
        _ -> {
          use #(p4, expr) <- result.try(parse_assignment_expression(p3))
          use p5 <- result.try(eat_semicolon(p4))
          Ok(#(p5, ast.ExportDefaultDeclaration(declaration: expr)))
        }
      }
    }
    Var | Let | Const -> {
      use #(p3, stmt) <- result.try(parse_variable_declaration(
        P(..p2, in_export_decl: True),
      ))
      Ok(#(
        p3,
        ast.ExportNamedDeclaration(
          declaration: Some(stmt),
          specifiers: [],
          source: None,
        ),
      ))
    }
    Function -> {
      use #(p3, stmt) <- result.try(parse_export_named_function(p2, False))
      Ok(#(
        p3,
        ast.ExportNamedDeclaration(
          declaration: Some(stmt),
          specifiers: [],
          source: None,
        ),
      ))
    }
    Class -> {
      use #(p3, stmt) <- result.try(parse_export_named_class(p2))
      Ok(#(
        p3,
        ast.ExportNamedDeclaration(
          declaration: Some(stmt),
          specifiers: [],
          source: None,
        ),
      ))
    }
    Async ->
      case peek_at(p2, 1) {
        Function -> {
          use #(p3, stmt) <- result.try(parse_export_named_function(p2, True))
          Ok(#(
            p3,
            ast.ExportNamedDeclaration(
              declaration: Some(stmt),
              specifiers: [],
              source: None,
            ),
          ))
        }
        _ -> Error(ExpectedFunctionAfterAsync(pos_of(p2)))
      }
    Star -> {
      // export * from "module"
      let p3 = advance(p2)
      case peek(p3) {
        As -> {
          // export * as name from "module"
          let p4 = advance(p3)
          let exported_value = peek_value(p4)
          let p5 = case peek(p4) {
            Identifier -> advance(p4)
            KString -> advance(p4)
            _ ->
              case is_keyword_as_identifier(peek(p4)) {
                True -> advance(p4)
                False -> p4
              }
          }
          use p5b <- result.try(check_duplicate_export(p5, exported_value))
          use p6 <- result.try(expect(p5b, From))
          case peek(p6) {
            KString -> {
              let value = peek_value(p6)
              use p7 <- result.try(eat_semicolon(advance(p6)))
              Ok(#(
                p7,
                ast.ExportAllDeclaration(
                  exported: Some(exported_value),
                  source: ast.StringLit(value:),
                ),
              ))
            }
            _ -> Error(ExpectedModuleSpecifier(pos_of(p6)))
          }
        }
        From -> {
          let p4 = advance(p3)
          case peek(p4) {
            KString -> {
              let value = peek_value(p4)
              use p5 <- result.try(eat_semicolon(advance(p4)))
              Ok(#(
                p5,
                ast.ExportAllDeclaration(
                  exported: None,
                  source: ast.StringLit(value:),
                ),
              ))
            }
            _ -> Error(ExpectedModuleSpecifier(pos_of(p4)))
          }
        }
        _ -> Error(ExpectedAsOrFromAfterExportStar(pos_of(p3)))
      }
    }
    LeftBrace -> {
      // export { a, b } or export { a, b } from "module"
      let p3 = advance(p2)
      // Save refs so we can revert if this is a re-export (from "module")
      let saved_local_refs = p3.export_local_refs
      use #(p4, specifiers) <- result.try(parse_export_specifiers(p3, []))
      case peek(p4) {
        From -> {
          // Re-export: local names don't need to be declared in this module
          let p4 = P(..p4, export_local_refs: saved_local_refs)
          let p5 = advance(p4)
          case peek(p5) {
            KString -> {
              let value = peek_value(p5)
              use p6 <- result.try(eat_semicolon(advance(p5)))
              Ok(#(
                p6,
                ast.ExportNamedDeclaration(
                  declaration: None,
                  specifiers:,
                  source: Some(ast.StringLit(value:)),
                ),
              ))
            }
            _ -> Error(ExpectedModuleSpecifier(pos_of(p5)))
          }
        }
        _ -> {
          use p5 <- result.try(eat_semicolon(p4))
          Ok(#(
            p5,
            ast.ExportNamedDeclaration(
              declaration: None,
              specifiers:,
              source: None,
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
  acc: List(ast.ExportSpecifier),
) -> Result(#(P, List(ast.ExportSpecifier)), ParseError) {
  case peek(p) {
    RightBrace -> Ok(#(advance(p), list.reverse(acc)))
    _ -> {
      use #(p2, spec) <- result.try(parse_export_specifier(p))
      let acc = [spec, ..acc]
      case peek(p2) {
        Comma ->
          case peek_at(p2, 1) {
            RightBrace -> Ok(#(advance(advance(p2)), list.reverse(acc)))
            _ -> parse_export_specifiers(advance(p2), acc)
          }
        RightBrace -> Ok(#(advance(p2), list.reverse(acc)))
        _ -> Error(ExpectedCommaOrBraceInExport(pos_of(p2)))
      }
    }
  }
}

fn parse_export_specifier(p: P) -> Result(#(P, ast.ExportSpecifier), ParseError) {
  let is_specifier_name =
    peek(p) == Identifier
    || peek(p) == KString
    || is_keyword_as_identifier(peek(p))
  case is_specifier_name {
    False -> Error(ExpectedExportSpecifierName(pos_of(p)))
    True -> {
      let local_value = peek_value(p)
      let p2 = advance(p)
      case peek(p2) {
        As -> {
          let p3 = advance(p2)
          let is_alias_name =
            peek(p3) == Identifier
            || peek(p3) == KString
            || is_keyword_as_identifier(peek(p3))
          case is_alias_name {
            True -> {
              let export_name = peek_value(p3)
              let exported_value = export_name
              use p4 <- result.try(check_duplicate_export(p3, export_name))
              // Track local name for undeclared-export validation
              let p4 =
                P(..p4, export_local_refs: [
                  #(local_value, pos_of(p)),
                  ..p4.export_local_refs
                ])
              Ok(#(
                advance(p4),
                ast.ExportSpecifier(
                  local: local_value,
                  exported: exported_value,
                ),
              ))
            }
            False -> Error(ExpectedExportAlias(pos_of(p3)))
          }
        }
        _ -> {
          // No alias: the local name IS the export name
          use p2b <- result.try(check_duplicate_export(p, local_value))
          // Track local name for undeclared-export validation
          Ok(#(
            P(..p2, export_names: p2b.export_names, export_local_refs: [
              #(local_value, pos_of(p)),
              ..p2b.export_local_refs
            ]),
            ast.ExportSpecifier(local: local_value, exported: local_value),
          ))
        }
      }
    }
  }
}

// ---- Strict mode detection ----

/// Check if the next tokens form a { body } with "use strict" in the
/// directive prologue. If so, set strict mode. Does NOT consume tokens.
/// Also checks for retroactive octal escape errors and parameter name errors.
fn check_use_strict_in_body(p: P) -> Result(P, ParseError) {
  case p.strict {
    True -> Ok(p)
    False ->
      case peek(p) {
        LeftBrace -> scan_directive_prologue(p, 1, [])
        _ -> Ok(p)
      }
  }
}

/// Check if program body starts with "use strict" directive.
fn check_use_strict_at_start(p: P) -> Result(P, ParseError) {
  case p.strict {
    True -> Ok(p)
    False -> scan_directive_prologue(p, 0, [])
  }
}

/// Scan the directive prologue looking for "use strict". If found, set strict
/// mode and check retroactive octal escapes and parameter names.
fn scan_directive_prologue(
  p: P,
  offset: Int,
  seen_strings: List(String),
) -> Result(P, ParseError) {
  case peek_at(p, offset) {
    KString -> {
      let val = peek_value_at(p, offset)
      case val {
        "use strict" -> {
          use Nil <- result.try(check_retroactive_octals(p, seen_strings))
          let p = P(..p, strict: True)
          check_retroactive_params(p)
        }
        _ -> {
          let next_offset = case peek_at(p, offset + 1) {
            Semicolon -> offset + 2
            _ -> offset + 1
          }
          scan_directive_prologue(p, next_offset, [val, ..seen_strings])
        }
      }
    }
    _ -> Ok(p)
  }
}

/// Check if any seen directive strings contain legacy octal escapes.
fn check_retroactive_octals(
  p: P,
  seen_strings: List(String),
) -> Result(Nil, ParseError) {
  case seen_strings {
    [] -> Ok(Nil)
    [val, ..rest] ->
      case has_legacy_octal_escape(val) {
        True -> Error(OctalEscapeStrictMode(pos_of(p)))
        False -> check_retroactive_octals(p, rest)
      }
  }
}

/// When "use strict" is found in a function body, retroactively validate
/// parameter names that were parsed in sloppy mode.
/// Also rejects "use strict" when params are non-simple (destructuring/rest/default).
fn check_retroactive_params(p: P) -> Result(P, ParseError) {
  case p.has_non_simple_param {
    True -> Error(UnexpectedToken("use strict", pos_of(p)))
    False -> validate_retroactive_param_names(p, p.param_bound_names)
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
      case name {
        "eval" | "arguments" -> Error(StrictModeParamName(name, pos_of(p)))
        "implements"
        | "interface"
        | "package"
        | "private"
        | "protected"
        | "public"
        | "static"
        | "let"
        | "yield" -> Error(ReservedWordStrictMode(name, pos_of(p)))
        _ -> validate_retroactive_param_names(p, rest)
      }
  }
}

// ---- Context helpers ----

/// Enter a new function context — resets loop/switch/labels, increments function_depth
fn enter_function_context(p: P, is_generator: Bool, is_async: Bool) -> P {
  P(
    ..p,
    function_depth: p.function_depth + 1,
    loop_depth: 0,
    switch_depth: 0,
    label_set: [],
    in_generator: is_generator,
    in_async: is_async,
    allow_super_call: False,
    allow_super_property: False,
    in_single_stmt_pos: False,
    in_method: False,
    // Reset scope for new function body
    scope_lexical: set.new(),
    scope_var: set.new(),
    scope_params: set.new(),
    scope_funcs: set.new(),
    outer_lexical: set.new(),
    binding_kind: BindingNone,
    in_block: False,
    module_top_level: False,
  )
}

fn enter_method_context(
  p: P,
  is_generator: Bool,
  is_async: Bool,
  is_constructor: Bool,
  has_super_class: Bool,
) -> P {
  P(
    ..p,
    function_depth: p.function_depth + 1,
    loop_depth: 0,
    switch_depth: 0,
    label_set: [],
    in_generator: is_generator,
    in_async: is_async,
    allow_super_call: is_constructor && has_super_class,
    allow_super_property: True,
    in_single_stmt_pos: False,
    in_method: True,
    // Reset scope for new method body
    scope_lexical: set.new(),
    scope_var: set.new(),
    scope_params: set.new(),
    scope_funcs: set.new(),
    outer_lexical: set.new(),
    binding_kind: BindingNone,
    in_block: False,
    module_top_level: False,
  )
}

fn restore_context_fn(
  res: Result(#(P, List(ast.Pattern), ast.Statement), ParseError),
  outer: P,
) -> Result(#(P, List(ast.Pattern), ast.Statement), ParseError) {
  use #(p, params, stmt) <- result.map(res)
  #(restore_outer_context(p, outer), params, stmt)
}

fn restore_outer_context(p: P, outer: P) -> P {
  P(
    ..p,
    function_depth: outer.function_depth,
    loop_depth: outer.loop_depth,
    switch_depth: outer.switch_depth,
    label_set: outer.label_set,
    in_generator: outer.in_generator,
    in_async: outer.in_async,
    in_method: outer.in_method,
    scope_lexical: outer.scope_lexical,
    scope_var: outer.scope_var,
    scope_params: outer.scope_params,
    scope_funcs: outer.scope_funcs,
    outer_lexical: outer.outer_lexical,
    binding_kind: outer.binding_kind,
    in_block: outer.in_block,
    in_single_stmt_pos: outer.in_single_stmt_pos,
  )
}

// ---- Label helpers ----

/// Find any label (for break)
fn find_label(labels: List(#(String, Bool)), name: String) -> Bool {
  case labels {
    [] -> False
    [#(n, _), ..] if n == name -> True
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

fn peek_at(p: P, n: Int) -> TokenKind {
  case list_nth(p.tokens, n) {
    Ok(lexer.Token(kind: k, ..)) -> k
    Error(_) -> Eof
  }
}

/// Check if a number literal value is a legacy octal (e.g. 0123, 09)
fn is_legacy_octal_number(value: String) -> Bool {
  case string.first(value) {
    Ok("0") ->
      case string.slice(value, 1, 1) {
        // 0x, 0o, 0b, 0X, 0O, 0B are modern prefixed forms
        "x" | "o" | "b" | "X" | "O" | "B" -> False
        // 0. or 0e/0E are decimal floats
        "." | "e" | "E" -> False
        // 0n is BigInt 0
        "n" -> False
        // Just "0" is fine
        "" -> False
        // 0 followed by digit is legacy octal (01, 07, 08, 09 etc.)
        _ -> string.length(value) > 1
      }
    _ -> False
  }
}

/// Check if a string literal value contains legacy octal escapes (\0-\7 followed by more)
fn has_legacy_octal_escape(value: String) -> Bool {
  check_string_escapes(value, 0, string.length(value))
}

fn check_string_escapes(s: String, pos: Int, end: Int) -> Bool {
  case pos >= end {
    True -> False
    False ->
      case string.slice(s, pos, 1) {
        "\\" ->
          case string.slice(s, pos + 1, 1) {
            "0" ->
              // \0 followed by a digit is legacy octal
              case string.slice(s, pos + 2, 1) {
                "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
                  True
                _ -> check_string_escapes(s, pos + 2, end)
              }
            "1" | "2" | "3" | "4" | "5" | "6" | "7" -> True
            "x" -> check_string_escapes(s, pos + 4, end)
            "u" ->
              case string.slice(s, pos + 2, 1) {
                "{" -> {
                  // Skip to closing brace
                  skip_to_brace(s, pos + 3, end)
                }
                _ -> check_string_escapes(s, pos + 6, end)
              }
            _ -> check_string_escapes(s, pos + 2, end)
          }
        _ -> check_string_escapes(s, pos + 1, end)
      }
  }
}

fn skip_to_brace(s: String, pos: Int, end: Int) -> Bool {
  case pos >= end {
    True -> False
    False ->
      case string.slice(s, pos, 1) {
        "}" -> check_string_escapes(s, pos + 1, end)
        _ -> skip_to_brace(s, pos + 1, end)
      }
  }
}

/// Parse a raw template literal string into quasis and expression ASTs.
/// The raw string includes backticks, e.g. `hello ${x} world`
fn parse_template_raw(
  p: P,
  raw: String,
) -> Result(#(List(String), List(ast.Expression)), ParseError) {
  // Strip leading ` and trailing `
  let len = string.length(raw)
  let inner = string.slice(raw, 1, len - 2)
  // Split into quasis and expression source strings
  let #(quasis, expr_sources) = split_template_parts(inner)
  // Parse each expression source
  use expressions <- result.try(
    list.try_map(expr_sources, fn(src) {
      case lexer.tokenize(src) {
        Error(e) ->
          Error(LexerError(lexer.lex_error_to_string(e), lexer.lex_error_pos(e)))
        Ok(tokens) -> {
          let sub_p =
            P(
              ..p,
              tokens: tokens,
              last_expr_assignable: False,
              last_expr_is_assignment: False,
            )
          use #(_, expr) <- result.map(parse_expression(sub_p))
          expr
        }
      }
    }),
  )
  Ok(#(quasis, expressions))
}

/// Split template inner text (without backticks) into quasi strings and
/// expression source strings. Tracks brace depth for nested `{}`.
fn split_template_parts(inner: String) -> #(List(String), List(String)) {
  let graphemes = string.to_graphemes(inner)
  do_split_template(graphemes, "", [], [], 0, False)
}

fn do_split_template(
  chars: List(String),
  current_quasi: String,
  quasis: List(String),
  expr_sources: List(String),
  brace_depth: Int,
  in_expr: Bool,
) -> #(List(String), List(String)) {
  case chars, in_expr {
    // End of input
    [], False -> #(
      list.reverse([current_quasi, ..quasis]),
      list.reverse(expr_sources),
    )
    [], True ->
      // Unterminated expression (shouldn't happen if lexer is correct)
      #(list.reverse([current_quasi, ..quasis]), list.reverse(expr_sources))
    // In quasi mode: look for ${ to start an expression
    ["\\", next, ..rest], False ->
      // Escaped character in quasi — include both chars, process escape
      do_split_template(
        rest,
        current_quasi <> process_template_escape(next),
        quasis,
        expr_sources,
        brace_depth,
        False,
      )
    ["$", "{", ..rest], False ->
      // Start of expression: save current quasi, begin expression collection
      do_split_template(
        rest,
        "",
        [current_quasi, ..quasis],
        expr_sources,
        0,
        True,
      )
    [ch, ..rest], False ->
      do_split_template(
        rest,
        current_quasi <> ch,
        quasis,
        expr_sources,
        brace_depth,
        False,
      )
    // In expression mode: track brace depth, look for closing }
    ["}", ..rest], True ->
      case brace_depth {
        0 ->
          // End of expression
          do_split_template(
            rest,
            "",
            quasis,
            [current_quasi, ..expr_sources],
            0,
            False,
          )
        _ ->
          do_split_template(
            rest,
            current_quasi <> "}",
            quasis,
            expr_sources,
            brace_depth - 1,
            True,
          )
      }
    ["{", ..rest], True ->
      do_split_template(
        rest,
        current_quasi <> "{",
        quasis,
        expr_sources,
        brace_depth + 1,
        True,
      )
    // Handle string literals inside expressions (to avoid counting braces in strings)
    ["\"", ..rest], True -> {
      let #(str_content, remaining) = collect_string_in_expr(rest, "\"", "\"")
      do_split_template(
        remaining,
        current_quasi <> str_content,
        quasis,
        expr_sources,
        brace_depth,
        True,
      )
    }
    ["'", ..rest], True -> {
      let #(str_content, remaining) = collect_string_in_expr(rest, "'", "'")
      do_split_template(
        remaining,
        current_quasi <> str_content,
        quasis,
        expr_sources,
        brace_depth,
        True,
      )
    }
    [ch, ..rest], True ->
      do_split_template(
        rest,
        current_quasi <> ch,
        quasis,
        expr_sources,
        brace_depth,
        True,
      )
  }
}

/// Collect characters inside a string literal in a template expression,
/// preserving escapes and tracking the closing quote.
fn collect_string_in_expr(
  chars: List(String),
  quote: String,
  acc: String,
) -> #(String, List(String)) {
  case chars {
    [] -> #(acc, [])
    ["\\", next, ..rest] ->
      collect_string_in_expr(rest, quote, acc <> "\\" <> next)
    [ch, ..rest] if ch == quote -> #(acc <> ch, rest)
    [ch, ..rest] -> collect_string_in_expr(rest, quote, acc <> ch)
  }
}

/// Process a single escape sequence in a template quasi.
fn process_template_escape(ch: String) -> String {
  case ch {
    "n" -> "\n"
    "t" -> "\t"
    "r" -> "\r"
    "\\" -> "\\"
    "`" -> "`"
    "$" -> "$"
    "0" -> "\u{0000}"
    _ -> ch
  }
}

fn peek_value(p: P) -> String {
  case p.tokens {
    [lexer.Token(value: v, ..), ..] -> v
    [] -> ""
  }
}

fn peek_value_at(p: P, n: Int) -> String {
  case list_nth(p.tokens, n) {
    Ok(lexer.Token(value: v, ..)) -> v
    Error(_) -> ""
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

fn advance(p: P) -> P {
  case p.tokens {
    [lexer.Token(line: line, ..), ..rest] ->
      P(..p, tokens: rest, prev_line: line)
    [] -> p
  }
}

fn expect(p: P, kind: TokenKind) -> Result(P, ParseError) {
  case peek(p) == kind {
    True -> Ok(advance(p))
    False ->
      Error(ExpectedToken(
        token_kind_to_string(kind),
        token_kind_to_string(peek(p)),
        pos_of(p),
      ))
  }
}

fn expect_identifier(p: P) -> Result(P, ParseError) {
  case peek(p) {
    Identifier -> Ok(advance(p))
    _ ->
      case is_keyword_as_identifier(peek(p)) {
        True -> Ok(advance(p))
        False -> Error(ExpectedIdentifier(pos_of(p)))
      }
  }
}

fn eat_semicolon(p: P) -> Result(P, ParseError) {
  case peek(p) {
    Semicolon -> Ok(advance(p))
    RightBrace | Eof -> Ok(p)
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

fn list_nth(lst: List(a), n: Int) -> Result(a, Nil) {
  case lst, n {
    [first, ..], 0 -> Ok(first)
    [_, ..rest], _ -> list_nth(rest, n - 1)
    [], _ -> Error(Nil)
  }
}

/// Contextual keywords that can be used as identifiers in non-strict mode.
/// These are NOT reserved words — they have special meaning only in specific contexts.
fn is_contextual_keyword(kind: TokenKind) -> Bool {
  case kind {
    Let | Static | Yield | Await | Async | From | As | Of | Undefined -> True
    _ -> False
  }
}

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

/// Check if a token kind is an identifier or any keyword usable as identifier.
fn is_identifier_or_keyword(kind: TokenKind) -> Bool {
  kind == Identifier || is_keyword_as_identifier(kind)
}

fn is_keyword_as_identifier(kind: TokenKind) -> Bool {
  case kind {
    Let
    | Const
    | Var
    | Function
    | Return
    | If
    | Else
    | While
    | Do
    | For
    | Break
    | Continue
    | Switch
    | Case
    | Default
    | Throw
    | Try
    | Catch
    | Finally
    | New
    | Delete
    | Typeof
    | Void
    | In
    | Instanceof
    | This
    | Class
    | Extends
    | Super
    | Import
    | Export
    | From
    | As
    | Of
    | Async
    | Await
    | Yield
    | Null
    | Undefined
    | KTrue
    | KFalse
    | Debugger
    | With
    | Static -> True
    _ -> False
  }
}

/// Returns True for keywords that can NEVER be used as binding identifiers,
fn token_kind_to_string(kind: TokenKind) -> String {
  case kind {
    Number -> "number"
    KString -> "string"
    TemplateLiteral -> "template"
    RegularExpression -> "regex"
    Identifier -> "identifier"
    Var -> "'var'"
    Let -> "'let'"
    Const -> "'const'"
    Function -> "'function'"
    Return -> "'return'"
    If -> "'if'"
    Else -> "'else'"
    While -> "'while'"
    Do -> "'do'"
    For -> "'for'"
    Break -> "'break'"
    Continue -> "'continue'"
    Switch -> "'switch'"
    Case -> "'case'"
    Default -> "'default'"
    Throw -> "'throw'"
    Try -> "'try'"
    Catch -> "'catch'"
    Finally -> "'finally'"
    New -> "'new'"
    Delete -> "'delete'"
    Typeof -> "'typeof'"
    Void -> "'void'"
    In -> "'in'"
    Instanceof -> "'instanceof'"
    This -> "'this'"
    Class -> "'class'"
    Extends -> "'extends'"
    Super -> "'super'"
    Import -> "'import'"
    Export -> "'export'"
    From -> "'from'"
    As -> "'as'"
    Of -> "'of'"
    Async -> "'async'"
    Await -> "'await'"
    Yield -> "'yield'"
    Null -> "'null'"
    Undefined -> "'undefined'"
    KTrue -> "'true'"
    KFalse -> "'false'"
    Debugger -> "'debugger'"
    With -> "'with'"
    Static -> "'static'"
    LeftParen -> "'('"
    RightParen -> "')'"
    LeftBrace -> "'{'"
    RightBrace -> "'}'"
    LeftBracket -> "'['"
    RightBracket -> "']'"
    Semicolon -> "';'"
    Comma -> "','"
    Dot -> "'.'"
    DotDotDot -> "'...'"
    QuestionDot -> "'?.'"
    QuestionQuestion -> "'??'"
    Arrow -> "'=>'"
    Colon -> "':'"
    Plus -> "'+'"
    Minus -> "'-'"
    Star -> "'*'"
    StarStar -> "'**'"
    Slash -> "'/'"
    Percent -> "'%'"
    Ampersand -> "'&'"
    AmpersandAmpersand -> "'&&'"
    Pipe -> "'|'"
    PipePipe -> "'||'"
    Caret -> "'^'"
    Tilde -> "'~'"
    Bang -> "'!'"
    Equal -> "'='"
    EqualEqual -> "'=='"
    EqualEqualEqual -> "'==='"
    BangEqual -> "'!='"
    BangEqualEqual -> "'!=='"
    LessThan -> "'<'"
    LessThanEqual -> "'<='"
    GreaterThan -> "'>'"
    GreaterThanEqual -> "'>='"
    LessThanLessThan -> "'<<'"
    GreaterThanGreaterThan -> "'>>'"
    GreaterThanGreaterThanGreaterThan -> "'>>>'"
    PlusPlus -> "'++'"
    MinusMinus -> "'--'"
    Question -> "'?'"
    Eof -> "end of file"
    _ -> "token"
  }
}
