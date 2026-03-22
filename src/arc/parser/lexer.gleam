/// JavaScript lexer for Arc.
/// Converts source text into a stream of tokens.
/// Operates on raw bytes (UTF-8) for O(1) character access.
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Token {
  Token(kind: TokenKind, value: String, pos: Int, line: Int, raw_len: Int)
}

pub type TokenKind {
  // Literals
  Number
  KString
  TemplateLiteral
  RegularExpression

  // Identifiers & keywords
  Identifier
  // Keywords
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
  KTrue
  KFalse
  Debugger
  With
  Static

  // Punctuation
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

  // Operators
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

  // Special
  Eof
  Illegal
}

pub type LexError {
  UnterminatedBlockComment(pos: Int)
  UnexpectedCharacter(char: String, pos: Int)
  InvalidEscapeSequence(pos: Int)
  InvalidHexEscapeSequence(pos: Int)
  InvalidUnicodeEscapeSequence(pos: Int)
  UnterminatedStringLiteral(pos: Int)
  UnterminatedTemplateLiteral(pos: Int)
  ExpectedExponentDigits(pos: Int)
  ExpectedHexDigits(pos: Int)
  ExpectedOctalDigits(pos: Int)
  ExpectedBinaryDigits(pos: Int)
  InvalidNumber(pos: Int)
  ConsecutiveNumericSeparator(pos: Int)
  LeadingNumericSeparator(pos: Int)
  TrailingNumericSeparator(pos: Int)
  IdentifierAfterNumericLiteral(pos: Int)
  HtmlCommentInModule(pos: Int)
}

pub fn lex_error_to_string(error: LexError) -> String {
  case error {
    UnterminatedBlockComment(_) -> "Unterminated block comment"
    UnexpectedCharacter(char:, ..) -> "Unexpected character: " <> char
    InvalidEscapeSequence(_) -> "Invalid escape sequence"
    InvalidHexEscapeSequence(_) -> "Invalid hexadecimal escape sequence"
    InvalidUnicodeEscapeSequence(_) -> "Invalid Unicode escape sequence"
    UnterminatedStringLiteral(_) -> "Unterminated string literal"
    UnterminatedTemplateLiteral(_) -> "Unterminated template literal"
    ExpectedExponentDigits(_) -> "Expected digits after exponent indicator"
    ExpectedHexDigits(_) -> "Expected hex digits after 0x"
    ExpectedOctalDigits(_) -> "Expected octal digits after 0o"
    ExpectedBinaryDigits(_) -> "Expected binary digits after 0b"
    InvalidNumber(_) -> "Invalid number"
    ConsecutiveNumericSeparator(_) ->
      "Numeric separator can not be used consecutively"
    LeadingNumericSeparator(_) ->
      "Numeric separator can not be used after leading 0"
    TrailingNumericSeparator(_) -> "Trailing numeric separator"
    IdentifierAfterNumericLiteral(_) ->
      "Identifier starts immediately after numeric literal"
    HtmlCommentInModule(_) -> "HTML comments are not allowed in module code"
  }
}

pub fn lex_error_pos(error: LexError) -> Int {
  case error {
    UnterminatedBlockComment(pos:) -> pos
    UnexpectedCharacter(pos:, ..) -> pos
    InvalidEscapeSequence(pos:) -> pos
    InvalidHexEscapeSequence(pos:) -> pos
    InvalidUnicodeEscapeSequence(pos:) -> pos
    UnterminatedStringLiteral(pos:) -> pos
    UnterminatedTemplateLiteral(pos:) -> pos
    ExpectedExponentDigits(pos:) -> pos
    ExpectedHexDigits(pos:) -> pos
    ExpectedOctalDigits(pos:) -> pos
    ExpectedBinaryDigits(pos:) -> pos
    InvalidNumber(pos:) -> pos
    ConsecutiveNumericSeparator(pos:) -> pos
    LeadingNumericSeparator(pos:) -> pos
    TrailingNumericSeparator(pos:) -> pos
    IdentifierAfterNumericLiteral(pos:) -> pos
    HtmlCommentInModule(pos:) -> pos
  }
}

/// Tokenize entire source into a list of tokens.
pub type LexMode {
  LexScript
  LexModule
}

pub fn tokenize(source: String) -> Result(List(Token), LexError) {
  let bytes = bit_array.from_string(source)
  do_tokenize(bytes, 0, 1, [], LexScript)
}

pub fn tokenize_module(source: String) -> Result(List(Token), LexError) {
  let bytes = bit_array.from_string(source)
  do_tokenize(bytes, 0, 1, [], LexModule)
}

fn do_tokenize(
  bytes: BitArray,
  pos: Int,
  line: Int,
  acc: List(Token),
  mode: LexMode,
) -> Result(List(Token), LexError) {
  use #(new_pos, ws_newlines) <- result.try(skip_whitespace_and_comments(
    bytes,
    pos,
    mode,
  ))
  let token_line = line + ws_newlines
  case char_at(bytes, new_pos) {
    "" -> Ok(list.reverse([Token(Eof, "", new_pos, token_line, 0), ..acc]))
    _ -> {
      use token <- result.try(read_token(bytes, new_pos))
      let token = Token(..token, line: token_line)
      let end_pos = token.pos + token.raw_len
      let end_line = case token.kind {
        // Only these token kinds can span multiple lines
        KString | TemplateLiteral -> {
          let raw_value = byte_slice(bytes, token.pos, token.raw_len)
          token_line + count_newlines_in(raw_value)
        }
        _ -> token_line
      }
      do_tokenize(bytes, end_pos, end_line, [token, ..acc], mode)
    }
  }
}

fn count_newlines_in(s: String) -> Int {
  do_count_newlines(bit_array.from_string(s), 0)
}

fn do_count_newlines(bytes: BitArray, count: Int) -> Int {
  case bytes {
    <<13, 10, rest:bytes>> -> do_count_newlines(rest, count + 1)
    <<10, rest:bytes>> -> do_count_newlines(rest, count + 1)
    <<13, rest:bytes>> -> do_count_newlines(rest, count + 1)
    <<_, rest:bytes>> -> do_count_newlines(rest, count)
    _ -> count
  }
}

fn skip_whitespace_and_comments(
  bytes: BitArray,
  pos: Int,
  mode: LexMode,
) -> Result(#(Int, Int), LexError) {
  // line_start: True when at start of input (-->  is valid comment there)
  skip_ws(bytes, pos, 0, pos == 0, mode)
}

fn skip_ws(
  bytes: BitArray,
  pos: Int,
  newlines: Int,
  line_start: Bool,
  mode: LexMode,
) -> Result(#(Int, Int), LexError) {
  case char_at(bytes, pos) {
    // ASCII whitespace (1 byte each)
    " " | "\t" | "\u{000B}" | "\u{000C}" ->
      skip_ws(bytes, pos + 1, newlines, line_start, mode)
    // 2-byte whitespace
    "\u{00A0}" -> skip_ws(bytes, pos + 2, newlines, line_start, mode)
    // 3-byte whitespace
    "\u{FEFF}"
    | "\u{1680}"
    | "\u{2000}"
    | "\u{2001}"
    | "\u{2002}"
    | "\u{2003}"
    | "\u{2004}"
    | "\u{2005}"
    | "\u{2006}"
    | "\u{2007}"
    | "\u{2008}"
    | "\u{2009}"
    | "\u{200A}"
    | "\u{202F}"
    | "\u{205F}"
    | "\u{3000}" -> skip_ws(bytes, pos + 3, newlines, line_start, mode)
    // Line endings
    "\r\n" -> skip_ws(bytes, pos + 2, newlines + 1, True, mode)
    "\n" | "\r" -> skip_ws(bytes, pos + 1, newlines + 1, True, mode)
    "\u{2028}" | "\u{2029}" -> skip_ws(bytes, pos + 3, newlines + 1, True, mode)
    "/" ->
      case char_at(bytes, pos + 1) {
        "/" -> skip_line_comment(bytes, pos + 2, newlines, line_start, mode)
        "*" -> skip_block_comment(bytes, pos + 2, newlines, line_start, mode)
        _ -> Ok(#(pos, newlines))
      }
    "<" ->
      case byte_slice(bytes, pos, 4) {
        "<!--" ->
          case mode {
            LexModule -> Error(HtmlCommentInModule(pos))
            LexScript ->
              skip_line_comment(bytes, pos + 4, newlines, line_start, mode)
          }
        _ -> Ok(#(pos, newlines))
      }
    "-" ->
      case byte_slice(bytes, pos, 3) {
        "-->" ->
          case mode {
            LexModule -> Error(HtmlCommentInModule(pos))
            LexScript ->
              // --> is only a comment at start of a line
              case line_start {
                True ->
                  skip_line_comment(bytes, pos + 3, newlines, line_start, mode)
                False -> Ok(#(pos, newlines))
              }
          }
        _ -> Ok(#(pos, newlines))
      }
    "#" if pos == 0 ->
      case char_at(bytes, pos + 1) {
        "!" -> skip_line_comment(bytes, pos + 2, newlines, line_start, mode)
        _ -> Ok(#(pos, newlines))
      }
    _ -> Ok(#(pos, newlines))
  }
}

fn skip_line_comment(
  bytes: BitArray,
  pos: Int,
  newlines: Int,
  _line_start: Bool,
  mode: LexMode,
) -> Result(#(Int, Int), LexError) {
  case char_at(bytes, pos) {
    "" -> Ok(#(pos, newlines))
    "\r\n" -> skip_ws(bytes, pos + 2, newlines + 1, True, mode)
    "\n" | "\r" -> skip_ws(bytes, pos + 1, newlines + 1, True, mode)
    "\u{2028}" | "\u{2029}" -> skip_ws(bytes, pos + 3, newlines + 1, True, mode)
    _ ->
      skip_line_comment(
        bytes,
        pos + char_width_at(bytes, pos),
        newlines,
        False,
        mode,
      )
  }
}

fn skip_block_comment(
  bytes: BitArray,
  pos: Int,
  newlines: Int,
  line_start: Bool,
  mode: LexMode,
) -> Result(#(Int, Int), LexError) {
  case char_at(bytes, pos) {
    "" -> Error(UnterminatedBlockComment(pos))
    "\r\n" -> skip_block_comment(bytes, pos + 2, newlines + 1, True, mode)
    "\n" | "\r" -> skip_block_comment(bytes, pos + 1, newlines + 1, True, mode)
    "\u{2028}" | "\u{2029}" ->
      skip_block_comment(bytes, pos + 3, newlines + 1, True, mode)
    "*" ->
      case char_at(bytes, pos + 1) {
        "/" -> skip_ws(bytes, pos + 2, newlines, line_start, mode)
        _ -> skip_block_comment(bytes, pos + 1, newlines, line_start, mode)
      }
    _ ->
      skip_block_comment(
        bytes,
        pos + char_width_at(bytes, pos),
        newlines,
        line_start,
        mode,
      )
  }
}

/// Create a token with explicit raw_len (in bytes).
fn tokn(kind: TokenKind, value: String, pos: Int, raw_len: Int) -> Token {
  Token(kind:, value:, pos:, line: 0, raw_len:)
}

fn read_token(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  let ch = char_at(bytes, pos)
  case ch {
    // Single-char punctuation
    "(" -> Ok(tokn(LeftParen, "(", pos, 1))
    ")" -> Ok(tokn(RightParen, ")", pos, 1))
    "{" -> Ok(tokn(LeftBrace, "{", pos, 1))
    "}" -> Ok(tokn(RightBrace, "}", pos, 1))
    "[" -> Ok(tokn(LeftBracket, "[", pos, 1))
    "]" -> Ok(tokn(RightBracket, "]", pos, 1))
    ";" -> Ok(tokn(Semicolon, ";", pos, 1))
    "," -> Ok(tokn(Comma, ",", pos, 1))
    "~" -> Ok(tokn(Tilde, "~", pos, 1))
    ":" -> Ok(tokn(Colon, ":", pos, 1))

    // Dot / spread
    "." -> read_dot(bytes, pos)

    // Operators with multi-char variants
    "+" -> read_plus(bytes, pos)
    "-" -> read_minus(bytes, pos)
    "*" -> read_star(bytes, pos)
    "/" -> read_slash(bytes, pos)
    "%" -> read_percent(bytes, pos)
    "=" -> read_equal(bytes, pos)
    "!" -> read_bang(bytes, pos)
    "<" -> read_less_than(bytes, pos)
    ">" -> read_greater_than(bytes, pos)
    "&" -> read_ampersand(bytes, pos)
    "|" -> read_pipe(bytes, pos)
    "^" -> read_caret(bytes, pos)
    "?" -> read_question(bytes, pos)

    // String literals
    "\"" -> read_string(bytes, pos, "\"")
    "'" -> read_string(bytes, pos, "'")

    // Template literals
    "`" -> read_template_literal(bytes, pos)

    // Numbers
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
      read_number(bytes, pos)

    // Identifiers and keywords
    "\\" ->
      case char_at(bytes, pos + 1) {
        "u" ->
          // Try reading as identifier with unicode escape (\uXXXX or \u{XXXX}).
          // If it fails (e.g. the codepoint isn't a valid identifier char),
          // fall back to Illegal spanning the full escape sequence so the
          // lexer skips past it entirely (avoids IdentifierAfterNumericLiteral
          // errors on sequences like \u{1ffff} inside regex bodies).
          case read_identifier(bytes, pos) {
            Ok(token) -> Ok(token)
            Error(_) -> {
              let escape_span = unicode_escape_span(bytes, pos)
              Ok(Token(
                kind: Illegal,
                value: byte_slice(bytes, pos, escape_span),
                pos: pos,
                line: 0,
                raw_len: escape_span,
              ))
            }
          }
        // Backslash not followed by 'u' — not a valid identifier escape.
        // Produce an Illegal token so the lexer can continue past
        // characters that will be re-scanned as regex body by the parser.
        _ -> Ok(tokn(Illegal, "\\", pos, 1))
      }
    _ ->
      case is_identifier_start(ch) {
        True -> read_identifier(bytes, pos)
        False -> Error(UnexpectedCharacter(ch, pos))
      }
  }
}

// --- Punctuation readers ---

fn read_dot(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "." ->
      case char_at(bytes, pos + 2) {
        "." -> Ok(tokn(DotDotDot, "...", pos, 3))
        _ -> Ok(tokn(Dot, ".", pos, 1))
      }
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
      read_number(bytes, pos)
    _ -> Ok(tokn(Dot, ".", pos, 1))
  }
}

fn read_plus(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "+" -> Ok(tokn(PlusPlus, "++", pos, 2))
    "=" -> Ok(tokn(PlusEqual, "+=", pos, 2))
    _ -> Ok(tokn(Plus, "+", pos, 1))
  }
}

fn read_minus(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "-" -> Ok(tokn(MinusMinus, "--", pos, 2))
    "=" -> Ok(tokn(MinusEqual, "-=", pos, 2))
    _ -> Ok(tokn(Minus, "-", pos, 1))
  }
}

fn read_star(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "*" ->
      case char_at(bytes, pos + 2) {
        "=" -> Ok(tokn(StarStarEqual, "**=", pos, 3))
        _ -> Ok(tokn(StarStar, "**", pos, 2))
      }
    "=" -> Ok(tokn(StarEqual, "*=", pos, 2))
    _ -> Ok(tokn(Star, "*", pos, 1))
  }
}

fn read_slash(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "=" -> Ok(tokn(SlashEqual, "/=", pos, 2))
    _ -> Ok(tokn(Slash, "/", pos, 1))
  }
}

fn read_percent(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "=" -> Ok(tokn(PercentEqual, "%=", pos, 2))
    _ -> Ok(tokn(Percent, "%", pos, 1))
  }
}

fn read_equal(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "=" ->
      case char_at(bytes, pos + 2) {
        "=" -> Ok(tokn(EqualEqualEqual, "===", pos, 3))
        _ -> Ok(tokn(EqualEqual, "==", pos, 2))
      }
    ">" -> Ok(tokn(Arrow, "=>", pos, 2))
    _ -> Ok(tokn(Equal, "=", pos, 1))
  }
}

fn read_bang(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "=" ->
      case char_at(bytes, pos + 2) {
        "=" -> Ok(tokn(BangEqualEqual, "!==", pos, 3))
        _ -> Ok(tokn(BangEqual, "!=", pos, 2))
      }
    _ -> Ok(tokn(Bang, "!", pos, 1))
  }
}

fn read_less_than(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "=" -> Ok(tokn(LessThanEqual, "<=", pos, 2))
    "<" ->
      case char_at(bytes, pos + 2) {
        "=" -> Ok(tokn(LessThanLessThanEqual, "<<=", pos, 3))
        _ -> Ok(tokn(LessThanLessThan, "<<", pos, 2))
      }
    _ -> Ok(tokn(LessThan, "<", pos, 1))
  }
}

fn read_greater_than(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "=" -> Ok(tokn(GreaterThanEqual, ">=", pos, 2))
    ">" ->
      case char_at(bytes, pos + 2) {
        "=" -> Ok(tokn(GreaterThanGreaterThanEqual, ">>=", pos, 3))
        ">" ->
          case char_at(bytes, pos + 3) {
            "=" ->
              Ok(tokn(GreaterThanGreaterThanGreaterThanEqual, ">>>=", pos, 4))
            _ -> Ok(tokn(GreaterThanGreaterThanGreaterThan, ">>>", pos, 3))
          }
        _ -> Ok(tokn(GreaterThanGreaterThan, ">>", pos, 2))
      }
    _ -> Ok(tokn(GreaterThan, ">", pos, 1))
  }
}

fn read_ampersand(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "&" ->
      case char_at(bytes, pos + 2) {
        "=" -> Ok(tokn(AmpersandAmpersandEqual, "&&=", pos, 3))
        _ -> Ok(tokn(AmpersandAmpersand, "&&", pos, 2))
      }
    "=" -> Ok(tokn(AmpersandEqual, "&=", pos, 2))
    _ -> Ok(tokn(Ampersand, "&", pos, 1))
  }
}

fn read_pipe(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "|" ->
      case char_at(bytes, pos + 2) {
        "=" -> Ok(tokn(PipePipeEqual, "||=", pos, 3))
        _ -> Ok(tokn(PipePipe, "||", pos, 2))
      }
    "=" -> Ok(tokn(PipeEqual, "|=", pos, 2))
    _ -> Ok(tokn(Pipe, "|", pos, 1))
  }
}

fn read_caret(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "=" -> Ok(tokn(CaretEqual, "^=", pos, 2))
    _ -> Ok(tokn(Caret, "^", pos, 1))
  }
}

fn read_question(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case char_at(bytes, pos + 1) {
    "?" ->
      case char_at(bytes, pos + 2) {
        "=" -> Ok(tokn(QuestionQuestionEqual, "??=", pos, 3))
        _ -> Ok(tokn(QuestionQuestion, "??", pos, 2))
      }
    "." ->
      // ?. but not ?.digit (that would be ? followed by .5 etc)
      case char_at(bytes, pos + 2) {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
          Ok(tokn(Question, "?", pos, 1))
        _ -> Ok(tokn(QuestionDot, "?.", pos, 2))
      }
    _ -> Ok(tokn(Question, "?", pos, 1))
  }
}

// --- Escape validation helpers ---

fn is_hex_digit(ch: String) -> Bool {
  case ch {
    "0"
    | "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9"
    | "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F" -> True
    _ -> False
  }
}

/// Validate escape sequence starting after the backslash.
/// `pos` points to the character right after `\`.
/// Returns Ok(skip_count) where skip_count is how many bytes to skip total
/// (including the backslash), or Error with a LexError.
fn validate_escape(
  bytes: BitArray,
  pos: Int,
  backslash_pos: Int,
  in_template: Bool,
) -> Result(Int, LexError) {
  let ch = char_at(bytes, pos)
  case ch {
    // \8 and \9 are always invalid
    "8" | "9" -> Error(InvalidEscapeSequence(backslash_pos))

    // Legacy octal escapes \0-\7
    // In templates: always invalid (even tagged templates fail at parse level)
    // In strings: allowed in sloppy mode, strict mode rejection at parser level
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" ->
      case in_template {
        True ->
          // In templates, only \0 NOT followed by a digit is valid (null char)
          case ch {
            "0" ->
              case char_at(bytes, pos + 1) {
                "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
                  Error(InvalidEscapeSequence(backslash_pos))
                _ -> Ok(2)
              }
            _ -> Error(InvalidEscapeSequence(backslash_pos))
          }
        False -> Ok(2)
      }

    // \x must be followed by exactly 2 hex digits
    "x" -> {
      let h1 = char_at(bytes, pos + 1)
      let h2 = char_at(bytes, pos + 2)
      case is_hex_digit(h1) && is_hex_digit(h2) {
        True -> Ok(4)
        False -> Error(InvalidHexEscapeSequence(backslash_pos))
      }
    }

    // \u must be followed by 4 hex digits or {hex_digits} with value <= 0x10FFFF
    "u" -> validate_unicode_escape(bytes, pos + 1, backslash_pos)

    // Line continuations — \r\n is 3 bytes total (\=1, \r\n=2), others are 2
    "\r\n" -> Ok(3)
    "\r" | "\n" -> Ok(2)

    // Standard escapes and all other single-char escapes
    _ -> Ok(1 + char_width_at(bytes, pos))
  }
}

/// Validate \u escape. `pos` points to the char after 'u'.
fn validate_unicode_escape(
  bytes: BitArray,
  pos: Int,
  backslash_pos: Int,
) -> Result(Int, LexError) {
  case char_at(bytes, pos) {
    "{" -> {
      // Braced unicode escape: \u{XXXX}
      // Collect hex digits until }
      let digits_start = pos + 1
      let digits_end = skip_hex_run(bytes, digits_start)
      let digit_count = digits_end - digits_start
      case digit_count == 0 {
        True -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
        False ->
          case char_at(bytes, digits_end) {
            "}" -> {
              // Validate the codepoint value <= 0x10FFFF
              let hex_str = byte_slice(bytes, digits_start, digit_count)
              case int.base_parse(hex_str, 16) {
                Ok(value) ->
                  case value > 0x10FFFF {
                    True -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
                    // Total skip: \ u { digits } = 2 + 1 + digit_count + 1
                    False -> Ok(digit_count + 4)
                  }
                Error(Nil) -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
              }
            }
            _ -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
          }
      }
    }
    _ -> {
      // Non-braced: must be exactly 4 hex digits
      let h1 = char_at(bytes, pos)
      let h2 = char_at(bytes, pos + 1)
      let h3 = char_at(bytes, pos + 2)
      let h4 = char_at(bytes, pos + 3)
      case
        is_hex_digit(h1)
        && is_hex_digit(h2)
        && is_hex_digit(h3)
        && is_hex_digit(h4)
      {
        True -> Ok(6)
        False -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
      }
    }
  }
}

/// Skip consecutive hex digits (no underscores). Used for \u{} validation.
fn skip_hex_run(bytes: BitArray, pos: Int) -> Int {
  case is_hex_digit(char_at(bytes, pos)) {
    True -> skip_hex_run(bytes, pos + 1)
    False -> pos
  }
}

/// Compute the byte span of a \u escape sequence starting at `pos` (the backslash).
/// Returns the number of bytes in the escape: \u{...} or \uXXXX.
/// Falls back to 2 (just \u) if the format doesn't match.
fn unicode_escape_span(bytes: BitArray, pos: Int) -> Int {
  case char_at(bytes, pos + 2) {
    "{" -> {
      // \u{...} — scan to the closing }
      let digits_end = skip_hex_run(bytes, pos + 3)
      case char_at(bytes, digits_end) {
        "}" -> digits_end + 1 - pos
        _ -> 2
      }
    }
    _ -> {
      // \uXXXX — 4 hex digits
      case
        is_hex_digit(char_at(bytes, pos + 2))
        && is_hex_digit(char_at(bytes, pos + 3))
        && is_hex_digit(char_at(bytes, pos + 4))
        && is_hex_digit(char_at(bytes, pos + 5))
      {
        True -> 6
        False -> 2
      }
    }
  }
}

// --- String reader ---

fn read_string(
  bytes: BitArray,
  start: Int,
  quote: String,
) -> Result(Token, LexError) {
  read_string_body(bytes, start + 1, start, quote)
}

fn read_string_body(
  bytes: BitArray,
  pos: Int,
  start: Int,
  quote: String,
) -> Result(Token, LexError) {
  let ch = char_at(bytes, pos)
  case ch {
    "" -> Error(UnterminatedStringLiteral(start))
    "\r\n" | "\n" | "\r" -> Error(UnterminatedStringLiteral(start))
    "\\" -> {
      let next = char_at(bytes, pos + 1)
      case next {
        "" -> Error(UnterminatedStringLiteral(start))
        _ -> {
          use skip <- result.try(validate_escape(bytes, pos + 1, pos, False))
          read_string_body(bytes, pos + skip, start, quote)
        }
      }
    }
    _ ->
      case ch == quote {
        True -> {
          let raw_len = pos - start + 1
          // Store the string content without quotes as the token value
          let content = byte_slice(bytes, start + 1, raw_len - 2)
          Ok(tokn(KString, content, start, raw_len))
        }
        False ->
          read_string_body(bytes, pos + char_width_at(bytes, pos), start, quote)
      }
  }
}

// --- Template literal reader ---

fn read_template_literal(bytes: BitArray, start: Int) -> Result(Token, LexError) {
  read_template_body(bytes, start + 1, start, 0)
}

fn read_template_body(
  bytes: BitArray,
  pos: Int,
  start: Int,
  brace_depth: Int,
) -> Result(Token, LexError) {
  let ch = char_at(bytes, pos)
  case ch {
    "" -> Error(UnterminatedTemplateLiteral(start))
    "\\" -> {
      let next = char_at(bytes, pos + 1)
      case next {
        "" -> Error(UnterminatedTemplateLiteral(start))
        _ -> {
          use skip <- result.try(validate_escape(bytes, pos + 1, pos, True))
          read_template_body(bytes, pos + skip, start, brace_depth)
        }
      }
    }
    "$" ->
      case char_at(bytes, pos + 1) {
        "{" -> read_template_body(bytes, pos + 2, start, brace_depth + 1)
        _ -> read_template_body(bytes, pos + 1, start, brace_depth)
      }
    "{" -> read_template_body(bytes, pos + 1, start, brace_depth + 1)
    "}" ->
      case brace_depth > 0 {
        True -> read_template_body(bytes, pos + 1, start, brace_depth - 1)
        False -> read_template_body(bytes, pos + 1, start, 0)
      }
    "`" ->
      case brace_depth > 0 {
        // Nested template literal inside an expression — skip it
        True -> {
          use inner <- result.try(read_template_literal(bytes, pos))
          let end_pos = inner.pos + inner.raw_len
          read_template_body(bytes, end_pos, start, brace_depth)
        }
        False -> {
          let len = pos - start + 1
          Ok(tokn(TemplateLiteral, byte_slice(bytes, start, len), start, len))
        }
      }
    _ ->
      read_template_body(
        bytes,
        pos + char_width_at(bytes, pos),
        start,
        brace_depth,
      )
  }
}

// --- Number reader ---

fn read_number(bytes: BitArray, start: Int) -> Result(Token, LexError) {
  case char_at(bytes, start) {
    "0" ->
      case char_at(bytes, start + 1) {
        "x" | "X" -> read_hex_number(bytes, start + 2, start)
        "o" | "O" -> read_octal_number(bytes, start + 2, start)
        "b" | "B" -> read_binary_number(bytes, start + 2, start)
        _ -> read_decimal_number(bytes, start)
      }
    "." -> read_decimal_after_dot(bytes, start + 1, start)
    _ -> read_decimal_number(bytes, start)
  }
}

fn read_decimal_number(bytes: BitArray, start: Int) -> Result(Token, LexError) {
  use pos <- result.try(skip_digits(bytes, start))
  // Check for legacy octal (0-prefixed like 01, 07) — don't consume dot
  let is_legacy_octal =
    char_at(bytes, start) == "0"
    && pos - start > 1
    && !has_non_octal(bytes, start + 1, pos)
  case char_at(bytes, pos) {
    "." ->
      case is_legacy_octal {
        True -> finish_number(bytes, start, pos)
        False ->
          case char_at(bytes, pos + 1) {
            // Two dots: include trailing dot in number (123. is a valid float)
            "." -> finish_number(bytes, start, pos + 1)
            _ -> {
              use pos2 <- result.try(skip_digits(bytes, pos + 1))
              read_exponent(bytes, start, pos2)
            }
          }
      }
    "e" | "E" -> read_exponent(bytes, start, pos)
    "n" -> {
      // BigInt
      let end = pos + 1
      use Nil <- result.try(check_after_numeric(bytes, end))
      let len = end - start
      Ok(tokn(Number, byte_slice(bytes, start, len), start, len))
    }
    _ -> finish_number(bytes, start, pos)
  }
}

fn has_non_octal(bytes: BitArray, pos: Int, end: Int) -> Bool {
  case pos >= end {
    True -> False
    False ->
      case char_at(bytes, pos) {
        "8" | "9" -> True
        _ -> has_non_octal(bytes, pos + 1, end)
      }
  }
}

fn read_decimal_after_dot(
  bytes: BitArray,
  pos: Int,
  start: Int,
) -> Result(Token, LexError) {
  use pos2 <- result.try(skip_digits(bytes, pos))
  read_exponent(bytes, start, pos2)
}

fn read_exponent(
  bytes: BitArray,
  start: Int,
  pos: Int,
) -> Result(Token, LexError) {
  case char_at(bytes, pos) {
    "e" | "E" -> {
      let pos2 = case char_at(bytes, pos + 1) {
        "+" | "-" -> pos + 2
        _ -> pos + 1
      }
      use pos3 <- result.try(skip_digits(bytes, pos2))
      case pos3 == pos2 {
        True -> Error(ExpectedExponentDigits(pos))
        False -> finish_number(bytes, start, pos3)
      }
    }
    _ -> finish_number(bytes, start, pos)
  }
}

fn read_hex_number(
  bytes: BitArray,
  pos: Int,
  start: Int,
) -> Result(Token, LexError) {
  use end <- result.try(skip_hex_digits(bytes, pos))
  case end == pos {
    True -> Error(ExpectedHexDigits(start))
    False ->
      case char_at(bytes, end) {
        "n" -> {
          let bigint_end = end + 1
          use Nil <- result.try(check_after_numeric(bytes, bigint_end))
          let len = bigint_end - start
          Ok(tokn(Number, byte_slice(bytes, start, len), start, len))
        }
        _ -> finish_number(bytes, start, end)
      }
  }
}

fn read_octal_number(
  bytes: BitArray,
  pos: Int,
  start: Int,
) -> Result(Token, LexError) {
  use end <- result.try(skip_octal_digits(bytes, pos))
  case end == pos {
    True -> Error(ExpectedOctalDigits(start))
    False ->
      case char_at(bytes, end) {
        "n" -> {
          let bigint_end = end + 1
          use Nil <- result.try(check_after_numeric(bytes, bigint_end))
          let len = bigint_end - start
          Ok(tokn(Number, byte_slice(bytes, start, len), start, len))
        }
        _ -> finish_number(bytes, start, end)
      }
  }
}

fn read_binary_number(
  bytes: BitArray,
  pos: Int,
  start: Int,
) -> Result(Token, LexError) {
  use end <- result.try(skip_binary_digits(bytes, pos))
  case end == pos {
    True -> Error(ExpectedBinaryDigits(start))
    False ->
      case char_at(bytes, end) {
        "n" -> {
          let bigint_end = end + 1
          use Nil <- result.try(check_after_numeric(bytes, bigint_end))
          let len = bigint_end - start
          Ok(tokn(Number, byte_slice(bytes, start, len), start, len))
        }
        _ -> finish_number(bytes, start, end)
      }
  }
}

/// Check that a numeric literal is not immediately followed by an identifier
/// start character or decimal digit. Per the spec, NumericLiteral must not be
/// immediately followed by IdentifierStart or DecimalDigit.
fn check_after_numeric(bytes: BitArray, end: Int) -> Result(Nil, LexError) {
  let next = char_at(bytes, end)
  case next {
    "" -> Ok(Nil)
    _ ->
      case is_identifier_start(next) {
        True -> Error(IdentifierAfterNumericLiteral(end))
        False -> Ok(Nil)
      }
  }
}

fn finish_number(
  bytes: BitArray,
  start: Int,
  end: Int,
) -> Result(Token, LexError) {
  let len = end - start
  case len > 0 {
    True -> {
      use Nil <- result.try(check_after_numeric(bytes, end))
      Ok(tokn(Number, byte_slice(bytes, start, len), start, len))
    }
    False -> Error(InvalidNumber(start))
  }
}

/// Skip decimal digits with numeric separator validation.
/// Returns Ok(end_pos) or Error if separator rules violated.
fn skip_digits(bytes: BitArray, pos: Int) -> Result(Int, LexError) {
  skip_digits_loop(bytes, pos, pos, False)
}

fn skip_digits_loop(
  bytes: BitArray,
  pos: Int,
  start: Int,
  prev_was_sep: Bool,
) -> Result(Int, LexError) {
  case char_at(bytes, pos) {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
      skip_digits_loop(bytes, pos + 1, start, False)
    "_" ->
      case prev_was_sep {
        // Consecutive separators
        True -> Error(ConsecutiveNumericSeparator(pos))
        False ->
          case pos == start {
            // Leading separator
            True -> Error(LeadingNumericSeparator(pos))
            False -> skip_digits_loop(bytes, pos + 1, start, True)
          }
      }
    _ ->
      case prev_was_sep {
        True -> Error(TrailingNumericSeparator(pos - 1))
        False -> Ok(pos)
      }
  }
}

/// Skip hex digits with numeric separator validation.
fn skip_hex_digits(bytes: BitArray, pos: Int) -> Result(Int, LexError) {
  skip_hex_digits_loop(bytes, pos, pos, False)
}

fn skip_hex_digits_loop(
  bytes: BitArray,
  pos: Int,
  start: Int,
  prev_was_sep: Bool,
) -> Result(Int, LexError) {
  case char_at(bytes, pos) {
    "0"
    | "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9"
    | "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F" -> skip_hex_digits_loop(bytes, pos + 1, start, False)
    "_" ->
      case prev_was_sep {
        True -> Error(ConsecutiveNumericSeparator(pos))
        False ->
          case pos == start {
            True -> Error(LeadingNumericSeparator(pos))
            False -> skip_hex_digits_loop(bytes, pos + 1, start, True)
          }
      }
    _ ->
      case prev_was_sep {
        True -> Error(TrailingNumericSeparator(pos - 1))
        False -> Ok(pos)
      }
  }
}

/// Skip octal digits with numeric separator validation.
fn skip_octal_digits(bytes: BitArray, pos: Int) -> Result(Int, LexError) {
  skip_octal_digits_loop(bytes, pos, pos, False)
}

fn skip_octal_digits_loop(
  bytes: BitArray,
  pos: Int,
  start: Int,
  prev_was_sep: Bool,
) -> Result(Int, LexError) {
  case char_at(bytes, pos) {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" ->
      skip_octal_digits_loop(bytes, pos + 1, start, False)
    "_" ->
      case prev_was_sep {
        True -> Error(ConsecutiveNumericSeparator(pos))
        False ->
          case pos == start {
            True -> Error(LeadingNumericSeparator(pos))
            False -> skip_octal_digits_loop(bytes, pos + 1, start, True)
          }
      }
    _ ->
      case prev_was_sep {
        True -> Error(TrailingNumericSeparator(pos - 1))
        False -> Ok(pos)
      }
  }
}

/// Skip binary digits with numeric separator validation.
fn skip_binary_digits(bytes: BitArray, pos: Int) -> Result(Int, LexError) {
  skip_binary_digits_loop(bytes, pos, pos, False)
}

fn skip_binary_digits_loop(
  bytes: BitArray,
  pos: Int,
  start: Int,
  prev_was_sep: Bool,
) -> Result(Int, LexError) {
  case char_at(bytes, pos) {
    "0" | "1" -> skip_binary_digits_loop(bytes, pos + 1, start, False)
    "_" ->
      case prev_was_sep {
        True -> Error(ConsecutiveNumericSeparator(pos))
        False ->
          case pos == start {
            True -> Error(LeadingNumericSeparator(pos))
            False -> skip_binary_digits_loop(bytes, pos + 1, start, True)
          }
      }
    _ ->
      case prev_was_sep {
        True -> Error(TrailingNumericSeparator(pos - 1))
        False -> Ok(pos)
      }
  }
}

// --- Identifier reader ---

/// Build an identifier token from its raw source span (byte positions).
/// If the raw text contains unicode escapes (\uXXXX or \u{XXXX}),
/// the token value is the decoded canonical name and raw_len preserves
/// the original source length for position tracking.
/// Escaped identifiers are always Identifier kind (never keywords).
fn make_identifier_token(bytes: BitArray, start: Int, end: Int) -> Token {
  let raw_len = end - start
  let raw = byte_slice(bytes, start, raw_len)
  case string.contains(raw, "\\") {
    False -> {
      let kind = keyword_or_identifier(raw)
      Token(kind:, value: raw, pos: start, line: 0, raw_len:)
    }
    True -> {
      // Decode unicode escapes to canonical form.
      // Escaped identifiers are always Identifier, never keywords.
      let decoded = decode_identifier_escapes(raw)
      Token(kind: Identifier, value: decoded, pos: start, line: 0, raw_len:)
    }
  }
}

/// Decode unicode escape sequences in an identifier string.
/// Converts \uXXXX and \u{XXXX} to their actual Unicode characters.
fn decode_identifier_escapes(raw: String) -> String {
  decode_id_escapes_loop(raw, "")
}

fn decode_id_escapes_loop(remaining: String, acc: String) -> String {
  // Jump to the next backslash instead of iterating char-by-char
  case string.split_once(remaining, "\\") {
    Error(Nil) -> acc <> remaining
    Ok(#(before, after)) -> {
      // after starts just past the backslash
      case after {
        "u{" <> rest -> {
          // Braced: \u{XXXX} — find closing brace
          case string.split_once(rest, "}") {
            Ok(#(hex_str, after_brace)) ->
              case int.base_parse(hex_str, 16) {
                Ok(cp) ->
                  case string.utf_codepoint(cp) {
                    Ok(codepoint) -> {
                      let char = string.from_utf_codepoints([codepoint])
                      decode_id_escapes_loop(after_brace, acc <> before <> char)
                    }
                    // Already validated, shouldn't happen
                    Error(Nil) ->
                      decode_id_escapes_loop(after_brace, acc <> before)
                  }
                // Already validated
                Error(Nil) -> decode_id_escapes_loop(after_brace, acc <> before)
              }
            Error(Nil) -> acc <> before
          }
        }
        "u" <> rest -> {
          // Non-braced: \uXXXX — exactly 4 hex digits
          let hex_str = string.slice(rest, 0, 4)
          let after_digits = string.drop_start(rest, 4)
          case int.base_parse(hex_str, 16) {
            Ok(cp) ->
              case string.utf_codepoint(cp) {
                Ok(codepoint) -> {
                  let char = string.from_utf_codepoints([codepoint])
                  decode_id_escapes_loop(after_digits, acc <> before <> char)
                }
                Error(Nil) ->
                  decode_id_escapes_loop(after_digits, acc <> before)
              }
            Error(Nil) -> decode_id_escapes_loop(after_digits, acc <> before)
          }
        }
        // Shouldn't happen (already validated)
        _ -> acc <> before
      }
    }
  }
}

fn read_identifier(bytes: BitArray, start: Int) -> Result(Token, LexError) {
  case char_at(bytes, start) {
    "\\" -> {
      // Must be a valid unicode escape that decodes to ID_Start
      use first_end <- result.try(validate_identifier_escape(bytes, start, True))
      use end <- result.try(skip_identifier_chars_checked(bytes, first_end))
      Ok(make_identifier_token(bytes, start, end))
    }
    "#" -> {
      // Private field: # followed by identifier char
      case char_at(bytes, start + 1) {
        "\\" -> {
          use first_end <- result.try(validate_identifier_escape(
            bytes,
            start + 1,
            True,
          ))
          use end <- result.try(skip_identifier_chars_checked(bytes, first_end))
          Ok(make_identifier_token(bytes, start, end))
        }
        ch2 -> {
          // The char after # must be a valid identifier start (not # or \)
          case is_identifier_start_simple(ch2) {
            True -> {
              // # is 1 byte, then skip the first identifier char
              let first_end = start + 1 + char_width_at(bytes, start + 1)
              use end <- result.try(skip_identifier_chars_checked(
                bytes,
                first_end,
              ))
              Ok(make_identifier_token(bytes, start, end))
            }
            False -> Error(UnexpectedCharacter("#", start))
          }
        }
      }
    }
    _ -> {
      let first_end = start + char_width_at(bytes, start)
      use end <- result.try(skip_identifier_chars_checked(bytes, first_end))
      Ok(make_identifier_token(bytes, start, end))
    }
  }
}

/// Validate a unicode escape in an identifier context.
/// `pos` points to the `\` character.
/// `is_start` indicates whether this is the first character (ID_Start) or not (ID_Continue).
/// Returns Ok(end_pos) after the escape, or Error.
fn validate_identifier_escape(
  bytes: BitArray,
  pos: Int,
  is_start: Bool,
) -> Result(Int, LexError) {
  // Must be \u
  case char_at(bytes, pos + 1) {
    "u" -> {
      case char_at(bytes, pos + 2) {
        "{" -> {
          // Braced: \u{XXXX}
          let digits_start = pos + 3
          let digits_end = skip_hex_run(bytes, digits_start)
          let digit_count = digits_end - digits_start
          case digit_count == 0 {
            True -> Error(InvalidUnicodeEscapeSequence(pos))
            False ->
              case char_at(bytes, digits_end) {
                "}" -> {
                  let hex_str = byte_slice(bytes, digits_start, digit_count)
                  case int.base_parse(hex_str, 16) {
                    Ok(cp) ->
                      case cp > 0x10FFFF {
                        True -> Error(InvalidUnicodeEscapeSequence(pos))
                        False ->
                          case validate_identifier_codepoint(cp, is_start) {
                            True -> Ok(digits_end + 1)
                            False -> Error(InvalidUnicodeEscapeSequence(pos))
                          }
                      }
                    Error(Nil) -> Error(InvalidUnicodeEscapeSequence(pos))
                  }
                }
                _ -> Error(InvalidUnicodeEscapeSequence(pos))
              }
          }
        }
        _ -> {
          // Non-braced: \uXXXX — exactly 4 hex digits
          let h1 = char_at(bytes, pos + 2)
          let h2 = char_at(bytes, pos + 3)
          let h3 = char_at(bytes, pos + 4)
          let h4 = char_at(bytes, pos + 5)
          case
            is_hex_digit(h1)
            && is_hex_digit(h2)
            && is_hex_digit(h3)
            && is_hex_digit(h4)
          {
            True -> {
              let hex_str = byte_slice(bytes, pos + 2, 4)
              case int.base_parse(hex_str, 16) {
                Ok(cp) ->
                  case validate_identifier_codepoint(cp, is_start) {
                    True -> Ok(pos + 6)
                    False -> Error(InvalidUnicodeEscapeSequence(pos))
                  }
                Error(_) -> Error(InvalidUnicodeEscapeSequence(pos))
              }
            }
            False -> Error(InvalidUnicodeEscapeSequence(pos))
          }
        }
      }
    }
    _ -> Error(InvalidUnicodeEscapeSequence(pos))
  }
}

/// Check if a decoded codepoint is valid for an identifier position.
/// For ID_Start: must be a letter, _, or $ (or Unicode ID_Start).
/// For ID_Continue: must also allow digits, ZWNJ, ZWJ (or Unicode ID_Continue).
fn validate_identifier_codepoint(cp: Int, is_start: Bool) -> Bool {
  // Reject null (U+0000) and surrogates (U+D800-U+DFFF)
  case cp {
    0 -> False
    _ ->
      case cp >= 0xD800 && cp <= 0xDFFF {
        True -> False
        False ->
          case is_start {
            True ->
              // ID_Start: letters, _, $
              { cp == 0x24 }
              || { cp == 0x5F }
              || { cp >= 0x41 && cp <= 0x5A }
              || { cp >= 0x61 && cp <= 0x7A }
              || { cp > 127 && is_unicode_id_start(cp) }
            False ->
              // ID_Continue: letters, digits, _, $, ZWNJ, ZWJ
              is_cp_id_continue(cp)
          }
      }
  }
}

/// Skip identifier continuation characters with validation.
/// Returns Ok(end_pos) or Error for invalid escapes.
fn skip_identifier_chars_checked(
  bytes: BitArray,
  pos: Int,
) -> Result(Int, LexError) {
  let ch = char_at(bytes, pos)
  case ch {
    "" -> Ok(pos)
    "\\" -> {
      // Try to validate a unicode escape continuation (\uXXXX or \u{XXXX}).
      // If it fails, treat the backslash as the end of the identifier rather
      // than a hard error. This allows the lexer to continue past characters
      // that will be re-scanned as regex body by the parser.
      case validate_identifier_escape(bytes, pos, False) {
        Ok(next_pos) -> skip_identifier_chars_checked(bytes, next_pos)
        Error(_) -> Ok(pos)
      }
    }
    _ ->
      case is_identifier_continue(ch) {
        True ->
          skip_identifier_chars_checked(bytes, pos + char_width_at(bytes, pos))
        False -> Ok(pos)
      }
  }
}

fn is_identifier_start(ch: String) -> Bool {
  case ch {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z" -> True
    "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z" -> True
    "_" | "$" -> True
    "\\" -> True
    "#" -> True
    _ -> {
      // Handle multi-codepoint grapheme clusters (e.g., T + ZWJ)
      let cps = string.to_utf_codepoints(ch)
      case cps {
        [] -> False
        [single] -> {
          let cp = string.utf_codepoint_to_int(single)
          cp > 127 && is_unicode_id_start(cp)
        }
        [first, ..rest] -> {
          let cp = string.utf_codepoint_to_int(first)
          { cp <= 127 || is_unicode_id_start(cp) } && all_id_continue_cps(rest)
        }
      }
    }
  }
}

/// Like is_identifier_start but excludes # and \ (which need special handling).
/// Used to validate the character after # in private field names.
fn is_identifier_start_simple(ch: String) -> Bool {
  case ch {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z" -> True
    "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z" -> True
    "_" | "$" -> True
    "" -> False
    _ -> {
      let cps = string.to_utf_codepoints(ch)
      case cps {
        [] -> False
        [single] -> {
          let cp = string.utf_codepoint_to_int(single)
          cp > 127 && is_unicode_id_start(cp)
        }
        [first, ..rest] -> {
          let cp = string.utf_codepoint_to_int(first)
          { cp <= 127 || is_unicode_id_start(cp) } && all_id_continue_cps(rest)
        }
      }
    }
  }
}

fn is_identifier_continue(ch: String) -> Bool {
  case ch {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z" -> True
    "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z" -> True
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    "_" | "$" -> True
    "\\" -> True
    "\u{200C}" | "\u{200D}" -> True
    _ -> {
      // Handle multi-codepoint grapheme clusters
      let cps = string.to_utf_codepoints(ch)
      all_id_continue_cps(cps)
    }
  }
}

fn all_id_continue_cps(cps: List(UtfCodepoint)) -> Bool {
  case cps {
    [] -> True
    [cp, ..rest] -> {
      let n = string.utf_codepoint_to_int(cp)
      is_cp_id_continue(n) && all_id_continue_cps(rest)
    }
  }
}

fn is_cp_id_continue(n: Int) -> Bool {
  // ASCII fast path
  { n >= 0x61 && n <= 0x7A }
  || { n >= 0x41 && n <= 0x5A }
  || { n >= 0x30 && n <= 0x39 }
  || n == 0x5F
  || n == 0x24
  || n == 0x200C
  || n == 0x200D
  || { n > 127 && is_unicode_id_continue(n) }
}

@external(erlang, "unicode_ffi", "is_id_start")
@external(javascript, "../unicode_ffi.mjs", "is_id_start")
fn is_unicode_id_start(cp: Int) -> Bool

@external(erlang, "unicode_ffi", "is_id_continue")
@external(javascript, "../unicode_ffi.mjs", "is_id_continue")
fn is_unicode_id_continue(cp: Int) -> Bool

fn keyword_or_identifier(word: String) -> TokenKind {
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
    "true" -> KTrue
    "false" -> KFalse
    "debugger" -> Debugger
    "with" -> With
    "static" -> Static
    _ -> Identifier
  }
}

// --- Character utilities (BitArray-based, O(1) access) ---

/// Get the byte width of the UTF-8 character at byte position `pos`.
/// Returns 0 if pos is past the end.
/// Returns 2 for \r\n (treated as single line ending).
fn char_width_at(bytes: BitArray, pos: Int) -> Int {
  case bit_array.slice(bytes, pos, 1) {
    Error(Nil) -> 0
    Ok(<<byte>>) ->
      case byte {
        0x0D ->
          case bit_array.slice(bytes, pos + 1, 1) {
            Ok(<<0x0A>>) -> 2
            _ -> 1
          }
        b if b < 0x80 -> 1
        b if b >= 0xC0 && b < 0xE0 -> 2
        b if b >= 0xE0 && b < 0xF0 -> 3
        b if b >= 0xF0 && b < 0xF8 -> 4
        _ -> 1
      }
    _ -> 0
  }
}

/// Get a single character at byte position `pos` in the UTF-8 byte array.
/// Returns "" if pos is past the end.
/// For \r followed by \n, returns "\r\n" (preserving existing comparison patterns).
fn char_at(bytes: BitArray, pos: Int) -> String {
  let width = char_width_at(bytes, pos)
  case width {
    0 -> ""
    _ -> byte_slice(bytes, pos, width)
  }
}

/// Get a substring from the byte array at [start, start+len).
fn byte_slice(bytes: BitArray, start: Int, len: Int) -> String {
  case bit_array.slice(bytes, start, len) {
    Ok(s) ->
      case bit_array.to_string(s) {
        Ok(str) -> str
        Error(_) -> ""
      }
    Error(_) -> ""
  }
}

/// O(1) byte_size of a String via Erlang BIF.
@external(erlang, "erlang", "byte_size")
pub fn string_byte_size(s: String) -> Int
