import arc/parser/ast
import gleam/option.{type Option, None, Some}

pub type Token {
  // had_escape: identifiers only; annex_b_legacy: Number/StringLiteral only
  Token(
    kind: TokenKind,
    value: String,
    pos: Int,
    line: Int,
    raw_len: Int,
    had_escape: Bool,
    annex_b_legacy: Bool,
  )
}

pub type TokenKind {
  Number
  StringLiteral
  TemplateLiteral
  TemplateHead

  Identifier
  Var
  Let
  Const
  Function
  Return
  If
  Else
  While
  Do
  For
  Break
  Continue
  Switch
  Case
  Default
  Throw
  Try
  Catch
  Finally
  New
  Delete
  Typeof
  Void
  In
  Instanceof
  This
  Class
  Extends
  Super
  Import
  Export
  From
  As
  Of
  Async
  Await
  Yield
  Null
  Undefined
  TrueLiteral
  FalseLiteral
  Debugger
  With
  Static

  LeftParen
  RightParen
  LeftBrace
  RightBrace
  LeftBracket
  RightBracket
  Semicolon
  Comma
  Dot
  DotDotDot
  QuestionDot
  QuestionQuestion
  Arrow
  Colon

  Plus
  Minus
  Star
  StarStar
  Slash
  Percent
  Ampersand
  AmpersandAmpersand
  Pipe
  PipePipe
  Caret
  Tilde
  Bang
  Equal
  EqualEqual
  EqualEqualEqual
  BangEqual
  BangEqualEqual
  LessThan
  LessThanEqual
  GreaterThan
  GreaterThanEqual
  LessThanLessThan
  GreaterThanGreaterThan
  GreaterThanGreaterThanGreaterThan
  PlusEqual
  MinusEqual
  StarEqual
  StarStarEqual
  SlashEqual
  PercentEqual
  AmpersandEqual
  AmpersandAmpersandEqual
  PipeEqual
  PipePipeEqual
  CaretEqual
  QuestionQuestionEqual
  LessThanLessThanEqual
  GreaterThanGreaterThanEqual
  GreaterThanGreaterThanGreaterThanEqual
  PlusPlus
  MinusMinus
  Question

  Eof
  // lenient: unclassifiable but legal inside a regex body
  Illegal
  // hard error, zero-length, stops the stream
  LexFailure(error: LexError)
}

pub type LexError {
  UnterminatedBlockComment(pos: Int)
  InvalidEscapeSequence(pos: Int)
  InvalidHexEscapeSequence(pos: Int)
  InvalidUnicodeEscapeSequence(pos: Int)
  ExpectedExponentDigits(pos: Int)
  ExpectedHexDigits(pos: Int)
  ExpectedOctalDigits(pos: Int)
  ExpectedBinaryDigits(pos: Int)
  ConsecutiveNumericSeparator(pos: Int)
  LeadingNumericSeparator(pos: Int)
  TrailingNumericSeparator(pos: Int)
  InvalidBigIntLiteral(pos: Int)
  HtmlCommentInModule(pos: Int)
}

pub fn lex_error_to_string(error: LexError) -> String {
  case error {
    UnterminatedBlockComment(_) -> "Unterminated block comment"
    InvalidEscapeSequence(_) -> "Invalid escape sequence"
    InvalidHexEscapeSequence(_) -> "Invalid hexadecimal escape sequence"
    InvalidUnicodeEscapeSequence(_) -> "Invalid Unicode escape sequence"
    ExpectedExponentDigits(_) -> "Expected digits after exponent indicator"
    ExpectedHexDigits(_) -> "Expected hex digits after 0x"
    ExpectedOctalDigits(_) -> "Expected octal digits after 0o"
    ExpectedBinaryDigits(_) -> "Expected binary digits after 0b"
    ConsecutiveNumericSeparator(_) ->
      "Numeric separator can not be used consecutively"
    LeadingNumericSeparator(_) ->
      "Numeric separator can not be used after leading 0"
    TrailingNumericSeparator(_) -> "Trailing numeric separator"
    InvalidBigIntLiteral(_) ->
      "Invalid BigInt literal: legacy octal and leading-zero literals cannot be BigInts"
    HtmlCommentInModule(_) -> "HTML comments are not allowed in module code"
  }
}

pub fn keyword_or_identifier(word: String) -> TokenKind {
  case word {
    "var" -> Var
    "let" -> Let
    "const" -> Const
    "function" -> Function
    "return" -> Return
    "if" -> If
    "else" -> Else
    "while" -> While
    "do" -> Do
    "for" -> For
    "break" -> Break
    "continue" -> Continue
    "switch" -> Switch
    "case" -> Case
    "default" -> Default
    "throw" -> Throw
    "try" -> Try
    "catch" -> Catch
    "finally" -> Finally
    "new" -> New
    "delete" -> Delete
    "typeof" -> Typeof
    "void" -> Void
    "in" -> In
    "instanceof" -> Instanceof
    "this" -> This
    "class" -> Class
    "extends" -> Extends
    "super" -> Super
    "import" -> Import
    "export" -> Export
    "from" -> From
    "as" -> As
    "of" -> Of
    "async" -> Async
    "await" -> Await
    "yield" -> Yield
    "null" -> Null
    "undefined" -> Undefined
    "true" -> TrueLiteral
    "false" -> FalseLiteral
    "debugger" -> Debugger
    "with" -> With
    "static" -> Static
    _ -> Identifier
  }
}

pub fn token_kind_to_string(kind: TokenKind) -> String {
  case kind {
    Number -> "number"
    StringLiteral -> "string"
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
    TrueLiteral -> "'true'"
    FalseLiteral -> "'false'"
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

pub fn is_reserved_word_kind(kind: TokenKind) -> Bool {
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
    | TrueLiteral
    | FalseLiteral -> True
    _ -> False
  }
}

pub fn is_contextual_keyword(kind: TokenKind) -> Bool {
  case kind {
    Let | Static | Yield | Await | Async | From | As | Of | Undefined -> True
    _ -> False
  }
}

pub fn is_identifier_or_keyword(kind: TokenKind) -> Bool {
  kind == Identifier || is_keyword_as_identifier(kind)
}

pub fn is_keyword_as_identifier(kind: TokenKind) -> Bool {
  is_reserved_word_kind(kind) || is_contextual_keyword(kind)
}

pub type BinaryOperator {
  BinaryOperator(precedence: Int, op: BinOrLogical)
}

pub type BinOrLogical {
  Binary(ast.BinaryOp)
  ShortCircuit(ast.LogicalOp)
  // separate so mixing ?? with || && errors (§13.13.1)
  Coalesce
}

pub fn binary_operator(
  kind: TokenKind,
  allow_in allow_in: Bool,
) -> Option(BinaryOperator) {
  case kind {
    QuestionQuestion -> Some(BinaryOperator(1, Coalesce))
    PipePipe -> Some(BinaryOperator(1, ShortCircuit(ast.LogicalOr)))
    AmpersandAmpersand -> Some(BinaryOperator(2, ShortCircuit(ast.LogicalAnd)))
    Pipe -> Some(BinaryOperator(3, Binary(ast.BitwiseOr)))
    Caret -> Some(BinaryOperator(4, Binary(ast.BitwiseXor)))
    Ampersand -> Some(BinaryOperator(5, Binary(ast.BitwiseAnd)))
    EqualEqual -> Some(BinaryOperator(6, Binary(ast.Equal)))
    BangEqual -> Some(BinaryOperator(6, Binary(ast.NotEqual)))
    EqualEqualEqual -> Some(BinaryOperator(6, Binary(ast.StrictEqual)))
    BangEqualEqual -> Some(BinaryOperator(6, Binary(ast.StrictNotEqual)))
    LessThan -> Some(BinaryOperator(7, Binary(ast.LessThan)))
    LessThanEqual -> Some(BinaryOperator(7, Binary(ast.LessThanEqual)))
    GreaterThan -> Some(BinaryOperator(7, Binary(ast.GreaterThan)))
    GreaterThanEqual -> Some(BinaryOperator(7, Binary(ast.GreaterThanEqual)))
    Instanceof -> Some(BinaryOperator(7, Binary(ast.InstanceOf)))
    In ->
      case allow_in {
        True -> Some(BinaryOperator(7, Binary(ast.In)))
        False -> None
      }
    LessThanLessThan -> Some(BinaryOperator(8, Binary(ast.LeftShift)))
    GreaterThanGreaterThan -> Some(BinaryOperator(8, Binary(ast.RightShift)))
    GreaterThanGreaterThanGreaterThan ->
      Some(BinaryOperator(8, Binary(ast.UnsignedRightShift)))
    Plus -> Some(BinaryOperator(9, Binary(ast.Add)))
    Minus -> Some(BinaryOperator(9, Binary(ast.Subtract)))
    Star -> Some(BinaryOperator(10, Binary(ast.Multiply)))
    Slash -> Some(BinaryOperator(10, Binary(ast.Divide)))
    Percent -> Some(BinaryOperator(10, Binary(ast.Modulo)))
    StarStar -> Some(BinaryOperator(11, Binary(ast.Exponentiation)))
    _ -> None
  }
}

pub fn assignment_op(kind: TokenKind) -> Option(ast.AssignmentOp) {
  case kind {
    Equal -> Some(ast.Assign)
    PlusEqual -> Some(ast.AddAssign)
    MinusEqual -> Some(ast.SubtractAssign)
    StarEqual -> Some(ast.MultiplyAssign)
    SlashEqual -> Some(ast.DivideAssign)
    PercentEqual -> Some(ast.ModuloAssign)
    StarStarEqual -> Some(ast.ExponentiationAssign)
    LessThanLessThanEqual -> Some(ast.LeftShiftAssign)
    GreaterThanGreaterThanEqual -> Some(ast.RightShiftAssign)
    GreaterThanGreaterThanGreaterThanEqual -> Some(ast.UnsignedRightShiftAssign)
    AmpersandEqual -> Some(ast.BitwiseAndAssign)
    PipeEqual -> Some(ast.BitwiseOrAssign)
    CaretEqual -> Some(ast.BitwiseXorAssign)
    AmpersandAmpersandEqual -> Some(ast.LogicalAndAssign)
    PipePipeEqual -> Some(ast.LogicalOrAssign)
    QuestionQuestionEqual -> Some(ast.NullishCoalesceAssign)
    _ -> None
  }
}
