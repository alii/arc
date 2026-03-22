/// Token classification and conversion helpers.
/// Pure functions that map TokenKind → Bool/String/AST enums.
/// Split from parser.gleam to reduce file size.
import arc/ast
import arc/lexer.{
  type TokenKind, Ampersand, AmpersandAmpersand, AmpersandAmpersandEqual,
  AmpersandEqual, Arrow, As, Async, Await, Bang, BangEqual, BangEqualEqual,
  Break, Caret, CaretEqual, Case, Catch, Class, Colon, Comma, Const, Continue,
  Debugger, Default, Delete, Do, Dot, DotDotDot, Else, Eof, Equal, EqualEqual,
  EqualEqualEqual, Export, Extends, Finally, For, From, Function, GreaterThan,
  GreaterThanEqual, GreaterThanGreaterThan, GreaterThanGreaterThanEqual,
  GreaterThanGreaterThanGreaterThan, GreaterThanGreaterThanGreaterThanEqual,
  Identifier, If, Import, In, Instanceof, KFalse, KString, KTrue, LeftBrace,
  LeftBracket, LeftParen, LessThan, LessThanEqual, LessThanLessThan,
  LessThanLessThanEqual, Let, Minus, MinusEqual, MinusMinus, New, Null, Number,
  Of, Percent, PercentEqual, Pipe, PipeEqual, PipePipe, PipePipeEqual, Plus,
  PlusEqual, PlusPlus, Question, QuestionDot, QuestionQuestion,
  QuestionQuestionEqual, RegularExpression, Return, RightBrace, RightBracket,
  RightParen, Semicolon, Slash, SlashEqual, Star, StarEqual, StarStar,
  StarStarEqual, Static, Super, Switch, TemplateLiteral, This, Throw, Tilde, Try,
  Typeof, Undefined, Var, Void, While, With, Yield,
}

/// Returns True for keywords that are ALWAYS reserved and cannot be used as
/// binding identifiers in any context (strict or sloppy).
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
    | KTrue
    | KFalse -> True
    _ -> False
  }
}

/// Contextual keywords that can be used as identifiers in non-strict mode.
/// These are NOT reserved words — they have special meaning only in specific contexts.
pub fn is_contextual_keyword(kind: TokenKind) -> Bool {
  case kind {
    Let | Static | Yield | Await | Async | From | As | Of | Undefined -> True
    _ -> False
  }
}

/// Check if a token kind is an identifier or any keyword usable as identifier.
pub fn is_identifier_or_keyword(kind: TokenKind) -> Bool {
  kind == Identifier || is_keyword_as_identifier(kind)
}

pub fn is_keyword_as_identifier(kind: TokenKind) -> Bool {
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

pub fn is_assignment_operator(kind: TokenKind) -> Bool {
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

pub fn binary_precedence(kind: TokenKind, allow_in: Bool) -> Int {
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

pub fn token_to_binary_op(kind: TokenKind) -> ast.BinaryOp {
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

pub fn is_logical_op(kind: TokenKind) -> Bool {
  case kind {
    AmpersandAmpersand | PipePipe | QuestionQuestion -> True
    _ -> False
  }
}

pub fn token_to_assignment_op(kind: TokenKind) -> ast.AssignmentOp {
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

pub fn token_kind_to_string(kind: TokenKind) -> String {
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
