import arc/parser/lexer.{
  type TokenKind, Ampersand, AmpersandAmpersand, AmpersandAmpersandEqual,
  AmpersandEqual, Arrow, As, Async, Await, Bang, BangEqual, BangEqualEqual,
  Break, Caret, CaretEqual, Case, Catch, Class, Colon, Comma, Const, Continue,
  Debugger, Default, Delete, Do, Dot, DotDotDot, Else, Eof, Equal, EqualEqual,
  EqualEqualEqual, Export, Extends, Finally, For, From, Function, GreaterThan,
  GreaterThanEqual, GreaterThanGreaterThan, GreaterThanGreaterThanEqual,
  GreaterThanGreaterThanGreaterThan, GreaterThanGreaterThanGreaterThanEqual,
  Identifier, If, Illegal, Import, In, Instanceof, KFalse, KString, KTrue,
  LeftBrace, LeftBracket, LeftParen, LessThan, LessThanEqual, LessThanLessThan,
  LessThanLessThanEqual, Let, LexFailure, Minus, MinusEqual, MinusMinus, New,
  Null, Number, Of, Percent, PercentEqual, Pipe, PipeEqual, PipePipe,
  PipePipeEqual, Plus, PlusEqual, PlusPlus, Question, QuestionDot,
  QuestionQuestion, QuestionQuestionEqual, Return, RightBrace, RightBracket,
  RightParen, Semicolon, Slash, SlashEqual, Star, StarEqual, StarStar,
  StarStarEqual, Static, Super, Switch, TemplateHead, TemplateLiteral, This,
  Throw, Tilde, Try, Typeof, Undefined, Var, Void, While, With, Yield,
}
import arc/parser/number
import arc/parser/regex_error
import gleam/option.{type Option, None, Some}

// every variant has pos first so error.pos works
pub type ParseError {
  // build via lex_error only, pos must match the lexer
  LexError(pos: Int, error: lexer.LexError)
  // build via regexp_syntax_error only
  RegExpSyntaxError(pos: Int, error: regex_error.PatternError)
  ExpectedToken(pos: Int, expected: TokenKind, got: TokenKind)
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
  UnexpectedToken(pos: Int, kind: TokenKind)
  MalformedNumericLiteral(pos: Int, error: number.NumberParseError)
  ReturnOutsideFunction(pos: Int)
  BreakOutsideLoopOrSwitch(pos: Int)
  ContinueOutsideLoop(pos: Int)
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
  AwaitInFormalParameter(pos: Int)
  InvalidLhsPrefixOp(pos: Int)
  SuperCallNotInDerivedConstructor(pos: Int)
  SuperPropertyNotInMethod(pos: Int)
  NewTargetOutsideFunction(pos: Int)
  MissingConstInitializer(pos: Int)
  RestTrailingComma(pos: Int)
  InvalidRestBinding(pos: Int)
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
  ExpectedNewTarget(pos: Int, got: Option(String))
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
  InvalidTemplateEscape(pos: Int)
  UnterminatedTemplateSubstitution(pos: Int)
  MisplacedUseStrictDirective(pos: Int)
  UsingAtScriptTopLevel(pos: Int)
  UsingInCaseClause(pos: Int)
  UsingMissingInitializer(pos: Int)
  UsingInForIn(pos: Int)
  UsingPatternBinding(pos: Int)
  CoalesceMixedWithLogical(pos: Int)
  UnaryBeforeExponentiation(pos: Int)
  TemplateInOptionalChain(pos: Int)
  ImportMetaOutsideModule(pos: Int)
  PrivateNameNotInBrandCheck(pos: Int)
}

pub fn parse_error_to_string(error: ParseError) -> String {
  case error {
    LexError(error:, ..) -> lexer.lex_error_to_string(error)
    RegExpSyntaxError(error:, ..) -> regex_error.pattern_error_message(error)
    ExpectedToken(expected:, got:, ..) ->
      "Expected "
      <> token_kind_to_string(expected)
      <> " but got "
      <> token_kind_to_string(got)
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
    UnexpectedToken(kind:, ..) ->
      "Unexpected token: " <> token_kind_to_string(kind)
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
    AwaitInFormalParameter(_) ->
      "Await expression not allowed in formal parameter"
    InvalidLhsPrefixOp(_) ->
      "Invalid left-hand side expression in prefix operation"
    SuperCallNotInDerivedConstructor(_) ->
      "'super()' is only valid in a derived class constructor"
    SuperPropertyNotInMethod(_) ->
      "'super' property access is only valid inside a method"
    NewTargetOutsideFunction(_) -> "'new.target' outside of function"
    MissingConstInitializer(_) -> "Missing initializer in const declaration"
    RestTrailingComma(_) -> "Rest element may not have a trailing comma"
    InvalidRestBinding(_) ->
      "`...` must be followed by an identifier in declaration contexts"
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
    CoalesceMixedWithLogical(_) ->
      "Nullish coalescing operator '??' requires parentheses when mixed with '||' or '&&'"
    ImportMetaOutsideModule(_) -> "Cannot use 'import.meta' outside a module"
    PrivateNameNotInBrandCheck(_) ->
      "Private name must be the left operand of 'in'"
    TemplateInOptionalChain(_) ->
      "Tagged template cannot be used in optional chain"
    UnaryBeforeExponentiation(_) ->
      "Unary operator used immediately before exponentiation expression; parenthesize the operand"
  }
}

pub fn parse_error_pos(error: ParseError) -> Int {
  error.pos
}

pub fn lex_error(err: lexer.LexError) -> ParseError {
  LexError(lexer.lex_error_pos(err), err)
}

pub fn regexp_syntax_error(err: regex_error.PatternError) -> ParseError {
  RegExpSyntaxError(regex_error.pattern_error_pos(err), err)
}

fn token_kind_to_string(kind: TokenKind) -> String {
  case kind {
    Number -> "number"
    KString -> "string"
    TemplateLiteral -> "template"
    TemplateHead -> "template"
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
    PlusEqual -> "'+='"
    MinusEqual -> "'-='"
    StarEqual -> "'*='"
    StarStarEqual -> "'**='"
    SlashEqual -> "'/='"
    PercentEqual -> "'%='"
    AmpersandEqual -> "'&='"
    AmpersandAmpersandEqual -> "'&&='"
    PipeEqual -> "'|='"
    PipePipeEqual -> "'||='"
    CaretEqual -> "'^='"
    QuestionQuestionEqual -> "'??='"
    LessThanLessThanEqual -> "'<<='"
    GreaterThanGreaterThanEqual -> "'>>='"
    GreaterThanGreaterThanGreaterThanEqual -> "'>>>='"
    PlusPlus -> "'++'"
    MinusMinus -> "'--'"
    Question -> "'?'"
    Eof -> "end of file"
    Illegal | LexFailure(_) -> "illegal token"
  }
}
