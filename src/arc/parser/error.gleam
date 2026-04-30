// NOTE FOR OTHER AGENTS: ParseError uses typed variants (like LexError in lexer.gleam).
// Use parse_error_to_string(e) and parse_error_pos(e) to read errors.
// Add new variants here instead of using string messages.
/// Parse error types and formatting.
/// Split from parser.gleam — the ParseError type, string formatting, and
/// position extraction live here so parser.gleam can focus on parsing logic.
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
  DeletePrivateName(pos: Int)
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
    DeletePrivateName(_) -> "Private fields cannot be deleted"
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
    LexerError(pos:, ..)
    | ExpectedToken(pos:, ..)
    | ExpectedIdentifier(pos:)
    | ExpectedSemicolon(pos:)
    | ExpectedBindingPattern(pos:)
    | ExpectedPropertyName(pos:)
    | ExpectedImportSpecifierName(pos:)
    | ExpectedExportSpecifierName(pos:)
    | ExpectedCaseDefaultOrBrace(pos:)
    | UnexpectedCloseBrace(pos:)
    | UnexpectedExport(pos:)
    | UnexpectedSuper(pos:)
    | UnexpectedCloseParen(pos:)
    | UnexpectedToken(pos:, ..)
    | NotAnArrowFunction(pos:)
    | ReturnOutsideFunction(pos:)
    | BreakOutsideLoopOrSwitch(pos:)
    | ContinueOutsideLoop(pos:)
    | ReservedWordStrictMode(pos:, ..)
    | YieldReservedStrictMode(pos:)
    | LetIdentifierStrictMode(pos:)
    | StaticReservedStrictMode(pos:)
    | WithNotAllowedStrictMode(pos:)
    | DeleteUnqualifiedStrictMode(pos:)
    | DeletePrivateName(pos:)
    | OctalEscapeStrictMode(pos:)
    | OctalLiteralStrictMode(pos:)
    | YieldInGenerator(pos:)
    | AwaitInModule(pos:)
    | AwaitInAsyncFunction(pos:)
    | EnumReservedWord(pos:)
    | DuplicateParameterName(pos:, ..)
    | DuplicateBindingLexical(pos:, ..)
    | DuplicateExport(pos:, ..)
    | DuplicateImportBinding(pos:, ..)
    | DuplicateLabel(pos:, ..)
    | DuplicateProtoProperty(pos:)
    | IdentifierAlreadyDeclared(pos:, ..)
    | LexicalDeclInSingleStatement(pos:)
    | YieldInFormalParameter(pos:)
    | InvalidLhsPrefixOp(pos:)
    | SuperCallNotInDerivedConstructor(pos:)
    | SuperPropertyNotInMethod(pos:)
    | NewTargetOutsideFunction(pos:)
    | MissingConstInitializer(pos:)
    | RestTrailingComma(pos:)
    | ExpectedForHeadSeparator(pos:)
    | MissingCatchOrFinally(pos:)
    | StrictModeModification(pos:, ..)
    | ExpectedModuleSpecifier(pos:)
    | DestructuringMissingInitializer(pos:)
    | ExpectedCommaOrBracket(pos:)
    | SetterExactlyOneParam(pos:)
    | ClassConstructorNotGetter(pos:)
    | ExpectedCommaOrObjectClose(pos:)
    | ExpectedForDeclSeparator(pos:)
    | ExpectedCloseAfterSetter(pos:)
    | ClassConstructorNotSetter(pos:)
    | InvalidForInOfLhs(pos:, ..)
    | ExpectedForSeparator(pos:)
    | UndefinedLabel(pos:, ..)
    | ThrowLineBreak(pos:)
    | GetterNoParams(pos:)
    | SetterNoRest(pos:)
    | RestMustBeLast(pos:)
    | ClassConstructorGenerator(pos:)
    | ClassConstructorAsync(pos:)
    | ClassDuplicateConstructor(pos:)
    | StaticPrototype(pos:)
    | LexicalDeclInLabel(pos:)
    | GeneratorDeclLabeled(pos:)
    | InvalidDestructuringTarget(pos:)
    | InvalidAssignmentLhs(pos:)
    | ExpectedNewTarget(pos:)
    | ExpectedImportMeta(pos:)
    | ExpectedCallOrDotAfterImport(pos:)
    | ExpectedIdentifierAfterDot(pos:)
    | ExpectedAfterOptionalChain(pos:)
    | ExpectedCommaOrCloseParen(pos:)
    | ExpectedCommaOrBracketInArray(pos:)
    | ExpectedCommaOrBracketInExpr(pos:)
    | ExpectedCommaOrBraceInObject(pos:)
    | ExpectedCommaOrBraceInObjectLiteral(pos:)
    | ExpectedBraceOrStarAfterComma(pos:)
    | ExpectedFromOrComma(pos:)
    | ExpectedImportSpecifier(pos:)
    | ExpectedCommaOrBraceInImport(pos:)
    | ExpectedFunctionAfterAsync(pos:)
    | ExpectedAsOrFromAfterExportStar(pos:)
    | UnexpectedAfterExport(pos:)
    | ExpectedCommaOrBraceInExport(pos:)
    | ExpectedExportAlias(pos:)
    | FunctionDeclInSingleStatement(pos:)
    | StrictModeBindingName(pos:, ..)
    | LetBindingInLexicalDecl(pos:)
    | ForInInitializer(pos:)
    | ForOfInitializer(pos:)
    | StrictModeParamName(pos:, ..)
    | RestDefaultInitializer(pos:)
    | FunctionDeclInLabelBody(pos:)
    | ShorthandDefaultOutsideDestructuring(pos:)
    | StrictModeAssignment(pos:, ..)
    | EvalArgsAssignStrictMode(pos:)
    | InvalidPostfixLhs(pos:)
    | ExpectedNewTargetGot(pos:, ..)
    | ExpectedImportMetaGot(pos:, ..)
    | StrictModeModifyRestricted(pos:, ..)
    | ExpectedIdentifierAsString(pos:, ..)
    | DuplicateParamNameStrictMode(pos:, ..)
    | ReservedWordImportBinding(pos:, ..)
    | DuplicateDefaultCase(pos:)
    | UndeclaredExportBinding(pos:, ..)
    | ImportNotTopLevel(pos:)
    | ExportNotTopLevel(pos:)
    | UnicodeEscapeInMetaProperty(pos:) -> pos
  }
}
