// NOTE FOR OTHER AGENTS: ParseError uses typed variants (like LexError in lexer.gleam).
// Use parse_error_to_string(e) and parse_error_pos(e) to read errors.
// Add new variants here instead of using string messages.
/// Parse error types and formatting.
/// Split from parser.gleam — the ParseError type, string formatting, and
/// position extraction live here so parser.gleam can focus on parsing logic.
import arc/parser/lexer
import arc/parser/number
import arc/parser/regex
import gleam/option.{type Option, None, Some}

/// EVERY variant carries `pos: Int` as its FIRST field, so `parse_error_pos`
/// is a plain field access and a new variant cannot forget to be listed
/// anywhere. (Same trick `parser/ast.Expression` plays with `span: Span`.)
/// Keep it that way when adding a variant.
pub type ParseError {
  /// A lexer error surfaced through the parser. Carries the lexer's own
  /// typed error rather than a pre-formatted message.
  ///
  /// INVARIANT: `pos` MUST equal `lexer.lex_error_pos(error)` — the lexer's
  /// own offset, never the parser cursor. Build it with `lex_error/1`, which
  /// is the only thing that can get that right; do not call this constructor
  /// directly.
  LexError(pos: Int, error: lexer.LexError)
  /// A regular-expression literal failed scanning or ECMAScript Pattern
  /// validation. Carries `parser/regex`'s own typed error; its message comes
  /// from `regex.pattern_error_message`.
  ///
  /// INVARIANT: `pos` MUST equal `regex.pattern_error_pos(error)`. Build it
  /// with `regexp_syntax_error/1`; do not call this constructor directly.
  RegExpSyntaxError(pos: Int, error: regex.PatternError)
  ExpectedToken(pos: Int, expected: String, got: String)
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
  UnexpectedToken(pos: Int, token: String)
  /// A `Number` token whose text `parser/number` refuses (a digit the radix
  /// does not admit, a radix prefix with no digits, …). The lexer should
  /// never emit such a token; carrying `number`'s own typed error keeps the
  /// message in one place.
  MalformedNumericLiteral(pos: Int, error: number.NumberParseError)
  ReturnOutsideFunction(pos: Int)
  BreakOutsideLoopOrSwitch(pos: Int)
  ContinueOutsideLoop(pos: Int)
  /// `continue L` where L is in scope but does not label an
  /// IterationStatement (ES2024 §14.9.1 Early Errors).
  ContinueToNonIterationLabel(pos: Int, name: String)
  ReservedWordStrictMode(pos: Int, name: String)
  YieldReservedStrictMode(pos: Int)
  LetIdentifierStrictMode(pos: Int)
  StaticReservedStrictMode(pos: Int)
  WithNotAllowedStrictMode(pos: Int)
  DeleteUnqualifiedStrictMode(pos: Int)
  DeletePrivateName(pos: Int)
  UndeclaredPrivateName(pos: Int, name: String)
  SuperPrivateName(pos: Int)
  PrivateNameAsPropertyKey(pos: Int)
  OctalEscapeStrictMode(pos: Int)
  OctalLiteralStrictMode(pos: Int)
  YieldInGenerator(pos: Int)
  AwaitInModule(pos: Int)
  AwaitInAsyncFunction(pos: Int)
  AwaitInStaticBlock(pos: Int)
  ArgumentsInStaticBlock(pos: Int)
  ArgumentsInClassFieldInit(pos: Int)
  PrivateNameConstructor(pos: Int)
  FieldNamedConstructor(pos: Int)
  DuplicatePrivateName(pos: Int, name: String)
  EnumReservedWord(pos: Int)
  EscapedReservedWord(pos: Int, name: String)
  DuplicateParameterName(pos: Int, name: String)
  DuplicateBindingLexical(pos: Int, name: String)
  DuplicateExport(pos: Int, name: String)
  DuplicateImportBinding(pos: Int, name: String)
  DuplicateLabel(pos: Int, label: String)
  DuplicateProtoProperty(pos: Int)
  IdentifierAlreadyDeclared(pos: Int, name: String)
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
  StrictModeModification(pos: Int, name: String)
  ExpectedModuleSpecifier(pos: Int)
  DestructuringMissingInitializer(pos: Int)
  ExpectedCommaOrBracket(pos: Int)
  SetterExactlyOneParam(pos: Int)
  ClassConstructorNotGetter(pos: Int)
  ExpectedCommaOrObjectClose(pos: Int)
  ExpectedForDeclSeparator(pos: Int)
  ExpectedCloseAfterSetter(pos: Int)
  ClassConstructorNotSetter(pos: Int)
  InvalidForInLhs(pos: Int)
  InvalidForOfLhs(pos: Int)
  ExpectedForSeparator(pos: Int)
  UndefinedLabel(pos: Int, label: String)
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
  /// `new.` not followed by `target`. `got` names the offending token when
  /// there is one to name.
  ExpectedNewTarget(pos: Int, got: Option(String))
  /// `import.` not followed by `meta`. `got` names the offending token when
  /// there is one to name.
  ExpectedImportMeta(pos: Int, got: Option(String))
  ExpectedCallOrDotAfterImport(pos: Int)
  ExpectedIdentifierAfterDot(pos: Int)
  ExpectedAfterOptionalChain(pos: Int)
  ExpectedCommaOrCloseParen(pos: Int)
  ExpectedCommaOrBracketInExpr(pos: Int)
  ExpectedCommaOrBraceInObject(pos: Int)
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
  StrictModeBindingName(pos: Int, name: String)
  LetBindingInLexicalDecl(pos: Int)
  ForInInitializer(pos: Int)
  ForOfInitializer(pos: Int)
  StrictModeParamName(pos: Int, name: String)
  RestDefaultInitializer(pos: Int)
  FunctionDeclInLabelBody(pos: Int)
  ShorthandDefaultOutsideDestructuring(pos: Int)
  StrictModeAssignment(pos: Int, name: String)
  EvalArgsAssignStrictMode(pos: Int)
  InvalidPostfixLhs(pos: Int)
  DuplicateParamNameStrictMode(pos: Int, name: String)
  ReservedWordImportBinding(pos: Int, name: String)
  DuplicateDefaultCase(pos: Int)
  UndeclaredExportBinding(pos: Int, name: String)
  ImportNotTopLevel(pos: Int)
  ExportNotTopLevel(pos: Int)
  UnicodeEscapeInMetaProperty(pos: Int)
  /// A template literal quasi whose escape sequence cannot be cooked
  /// (untagged templates only — tagged templates get `undefined` cooked
  /// values instead).
  InvalidTemplateEscape(pos: Int)
  /// A template literal's `${` substitution never reached its closing `}`
  /// (e.g. it was swallowed by an unterminated string or nested template).
  UnterminatedTemplateSubstitution(pos: Int)
  /// A "use strict" directive in the body of a function whose parameter
  /// list is non-simple (defaults / destructuring / rest).
  MisplacedUseStrictDirective(pos: Int)
  /// `using`/`await using` at the top level of a Script (or eval) — early
  /// error: a UsingDeclaration with goal Script must be contained within a
  /// Block, ForStatement, ForInOfStatement, FunctionBody, etc.
  UsingAtScriptTopLevel(pos: Int)
  /// `using`/`await using` directly within a CaseClause/DefaultClause
  /// statement list.
  UsingInCaseClause(pos: Int)
  /// `using x;` — using declarations require an initializer.
  UsingMissingInitializer(pos: Int)
  /// `for (using x in obj)` — using declarations are not allowed in for-in.
  UsingInForIn(pos: Int)
  /// `using [a] = …` / `using {a} = …` — using declarations may only bind
  /// identifiers, never destructuring patterns.
  UsingPatternBinding(pos: Int)
}

pub fn parse_error_to_string(error: ParseError) -> String {
  case error {
    LexError(error:, ..) -> lexer.lex_error_to_string(error)
    RegExpSyntaxError(error:, ..) -> regex.pattern_error_message(error)
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
    MalformedNumericLiteral(error:, ..) -> number.parse_error_message(error)
    ReturnOutsideFunction(_) -> "'return' outside of function"
    BreakOutsideLoopOrSwitch(_) -> "'break' outside of loop or switch"
    ContinueOutsideLoop(_) -> "'continue' outside of loop"
    ContinueToNonIterationLabel(name:, ..) ->
      "Illegal continue statement: '"
      <> name
      <> "' does not denote an iteration statement"
    ReservedWordStrictMode(name:, ..) ->
      "'" <> name <> "' is a reserved word in strict mode"
    YieldReservedStrictMode(_) -> "'yield' is a reserved word in strict mode"
    LetIdentifierStrictMode(_) ->
      "'let' cannot be used as identifier in strict mode"
    StaticReservedStrictMode(_) -> "'static' is a reserved word in strict mode"
    WithNotAllowedStrictMode(_) -> "'with' not allowed in strict mode"
    DeleteUnqualifiedStrictMode(_) ->
      "Cannot delete unqualified identifier in strict mode"
    DeletePrivateName(_) -> "Private fields cannot be deleted"
    UndeclaredPrivateName(name:, ..) ->
      "Private field '" <> name <> "' must be declared in an enclosing class"
    SuperPrivateName(_) ->
      "Unexpected private field: private members are not accessible on super"
    PrivateNameAsPropertyKey(_) -> "Private names are not valid property keys"
    OctalEscapeStrictMode(_) ->
      "Octal escape sequences are not allowed in strict mode"
    OctalLiteralStrictMode(_) -> "Octal literals are not allowed in strict mode"
    YieldInGenerator(_) -> "'yield' cannot be used as identifier in generator"
    AwaitInModule(_) -> "'await' cannot be used as identifier in module"
    AwaitInAsyncFunction(_) ->
      "'await' cannot be used as identifier in async function"
    AwaitInStaticBlock(_) -> "'await' is not allowed in class static block"
    ArgumentsInStaticBlock(_) ->
      "'arguments' is not allowed in class static block"
    ArgumentsInClassFieldInit(_) ->
      "'arguments' is not allowed in class field initializer"
    PrivateNameConstructor(_) ->
      "Class may not have a private element named '#constructor'"
    FieldNamedConstructor(_) ->
      "Classes may not have a field named 'constructor'"
    DuplicatePrivateName(name:, ..) -> "Duplicate private name '" <> name <> "'"
    EnumReservedWord(_) -> "'enum' is a reserved word"
    EscapedReservedWord(name:, ..) ->
      "Keyword '" <> name <> "' must not contain escape sequences"
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
    InvalidForInLhs(_) -> "Invalid left-hand side in for-in statement"
    InvalidForOfLhs(_) -> "Invalid left-hand side in for-of statement"
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
    ExpectedNewTarget(got: None, ..) -> "Expected 'target' after 'new.'"
    ExpectedNewTarget(got: Some(got), ..) ->
      "Expected 'target' after 'new.' but got '" <> got <> "'"
    ExpectedImportMeta(got: None, ..) -> "Expected 'meta' after 'import.'"
    ExpectedImportMeta(got: Some(got), ..) ->
      "Expected 'meta' after 'import.' but got '" <> got <> "'"
    ExpectedCallOrDotAfterImport(_) -> "Expected '(' or '.' after 'import'"
    ExpectedIdentifierAfterDot(_) -> "Expected identifier after '.'"
    ExpectedAfterOptionalChain(_) ->
      "Expected identifier, '[', or '(' after '?.'"
    ExpectedCommaOrCloseParen(_) -> "Expected ',' or ')' in arguments"
    ExpectedCommaOrBracketInExpr(_) -> "Expected ',' or ']'"
    ExpectedCommaOrBraceInObject(_) -> "Expected ',' or '}' in object literal"
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
    InvalidTemplateEscape(_) -> "Invalid escape sequence"
    UnterminatedTemplateSubstitution(_) -> "Unterminated template substitution"
    // Rendered like the `UnexpectedToken("use strict", _)` it replaced so
    // the reported message is unchanged.
    MisplacedUseStrictDirective(_) -> "Unexpected token: use strict"
    UsingAtScriptTopLevel(_) ->
      "'using' declarations are not allowed at the top level of a script"
    UsingInCaseClause(_) ->
      "'using' declarations are not allowed directly within a case or default clause"
    UsingMissingInitializer(_) -> "Missing initializer in using declaration"
    UsingInForIn(_) ->
      "'using' declarations are not allowed in for-in statements"
    UsingPatternBinding(_) ->
      "'using' declarations may only declare identifier bindings"
  }
}

/// The source byte offset the error is reported at. Every variant carries
/// `pos` as its first field, so this cannot fall out of date.
pub fn parse_error_pos(error: ParseError) -> Int {
  error.pos
}

/// Lift a lexer's typed error into a `ParseError` at the position the lexer
/// itself reports. THE ONLY way to build a `LexError` — going through this
/// keeps `pos` and `error` from disagreeing.
pub fn lex_error(err: lexer.LexError) -> ParseError {
  LexError(lexer.lex_error_pos(err), err)
}

/// Lift a regex pattern error into a `ParseError` at the position the regex
/// scanner/validator itself reports. THE ONLY way to build a
/// `RegExpSyntaxError` — going through this keeps `pos` and `error` from
/// disagreeing.
pub fn regexp_syntax_error(err: regex.PatternError) -> ParseError {
  RegExpSyntaxError(regex.pattern_error_pos(err), err)
}
