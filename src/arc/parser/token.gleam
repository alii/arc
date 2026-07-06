/// Token classification and conversion helpers.
/// Pure functions that map TokenKind → Bool/String/AST enums.
/// Split from parser.gleam to reduce file size.
import arc/parser/ast
import arc/parser/lexer.{
  type TokenKind, Ampersand, AmpersandAmpersand, AmpersandAmpersandEqual,
  AmpersandEqual, As, Async, Await, BangEqual, BangEqualEqual, Break, Caret,
  CaretEqual, Case, Catch, Class, Const, Continue, Debugger, Default, Delete, Do,
  Else, Equal, EqualEqual, EqualEqualEqual, Export, Extends, Finally, For, From,
  Function, GreaterThan, GreaterThanEqual, GreaterThanGreaterThan,
  GreaterThanGreaterThanEqual, GreaterThanGreaterThanGreaterThan,
  GreaterThanGreaterThanGreaterThanEqual, Identifier, If, Import, In, Instanceof,
  KFalse, KTrue, LessThan, LessThanEqual, LessThanLessThan,
  LessThanLessThanEqual, Let, Minus, MinusEqual, New, Null, Of, Percent,
  PercentEqual, Pipe, PipeEqual, PipePipe, PipePipeEqual, Plus, PlusEqual,
  QuestionQuestion, QuestionQuestionEqual, Return, Slash, SlashEqual, Star,
  StarEqual, StarStar, StarStarEqual, Static, Super, Switch, This, Throw, Try,
  Typeof, Undefined, Var, Void, While, With, Yield,
}
import gleam/option.{type Option, None, Some}

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
  is_reserved_word_kind(kind) || is_contextual_keyword(kind)
}

/// A binary/logical operator token's Pratt precedence together with the AST
/// operator it produces. Keeping both in ONE value (returned by
/// `binary_operator`) makes it impossible for the precedence table and the
/// token→operator mapping to drift apart: a token either is an operator (and
/// carries both facts) or it isn't (`None`).
pub type BinaryOperator {
  BinaryOperator(precedence: Int, op: BinOrLogical)
}

/// What kind of AST a binary-operator token builds. `Coalesce` is split out
/// from `ShortCircuit` even though both produce a `LogicalExpression`, because
/// §13.13.1 forbids mixing `??` with `||`/`&&` without parentheses — the
/// separate variant forces `parse_binary_rhs` to have a distinct arm for it,
/// so that early error can never be silently forgotten.
pub type BinOrLogical {
  Binary(ast.BinaryOp)
  /// `&&` / `||` — the truthy short-circuit operators.
  ShortCircuit(ast.LogicalOp)
  /// `??` — nullish coalescing.
  Coalesce
}

/// The single table of binary/logical operator tokens.
/// Returns `None` for every token that is not a binary operator, and for `in`
/// when `allow_in` is False (a `for (a in b;;)` head must not treat `in` as a
/// relational operator).
pub fn binary_operator(
  kind: TokenKind,
  allow_in: Bool,
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

/// The single table of assignment-operator tokens: `Some(op)` for `=` and
/// every compound/logical assignment, `None` for every other token.
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
