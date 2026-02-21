/// JavaScript lexer for Lumen.
/// Converts source text into a stream of tokens.
/// Operates on raw bytes (UTF-8) for performance.
import gleam/bit_array
import gleam/int
import gleam/list
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
  do_tokenize(source, 0, 1, [], LexScript)
}

pub fn tokenize_module(source: String) -> Result(List(Token), LexError) {
  do_tokenize(source, 0, 1, [], LexModule)
}

fn do_tokenize(
  src: String,
  pos: Int,
  line: Int,
  acc: List(Token),
  mode: LexMode,
) -> Result(List(Token), LexError) {
  case skip_whitespace_and_comments(src, pos, mode) {
    Ok(#(new_pos, ws_newlines)) -> {
      let token_line = line + ws_newlines
      case char_at(src, new_pos) {
        "" -> Ok(list.reverse([Token(Eof, "", new_pos, token_line, 0), ..acc]))
        _ ->
          case read_token(src, new_pos) {
            Ok(token) -> {
              let token = Token(..token, line: token_line)
              let end_pos = token.pos + token.raw_len
              let raw_value = slice(src, token.pos, token.raw_len)
              let end_line = token_line + count_newlines_in(raw_value)
              do_tokenize(src, end_pos, end_line, [token, ..acc], mode)
            }
            Error(e) -> Error(e)
          }
      }
    }
    Error(e) -> Error(e)
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
  src: String,
  pos: Int,
  mode: LexMode,
) -> Result(#(Int, Int), LexError) {
  // line_start: True when at start of input (-->  is valid comment there)
  skip_ws(src, pos, 0, pos == 0, mode)
}

fn skip_ws(
  src: String,
  pos: Int,
  newlines: Int,
  line_start: Bool,
  mode: LexMode,
) -> Result(#(Int, Int), LexError) {
  case char_at(src, pos) {
    " "
    | "\t"
    | "\u{000B}"
    | "\u{000C}"
    | "\u{00A0}"
    | "\u{FEFF}"
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
    | "\u{3000}" -> skip_ws(src, pos + 1, newlines, line_start, mode)
    "\r\n" | "\n" | "\r" -> skip_ws(src, pos + 1, newlines + 1, True, mode)
    "\u{2028}" | "\u{2029}" -> skip_ws(src, pos + 1, newlines + 1, True, mode)
    "/" ->
      case char_at(src, pos + 1) {
        "/" -> skip_line_comment(src, pos + 2, newlines, line_start, mode)
        "*" -> skip_block_comment(src, pos + 2, newlines, line_start, mode)
        _ -> Ok(#(pos, newlines))
      }
    "<" ->
      case slice(src, pos, 4) {
        "<!--" ->
          case mode {
            LexModule -> Error(HtmlCommentInModule(pos))
            LexScript ->
              skip_line_comment(src, pos + 4, newlines, line_start, mode)
          }
        _ -> Ok(#(pos, newlines))
      }
    "-" ->
      case slice(src, pos, 3) {
        "-->" ->
          case mode {
            LexModule -> Error(HtmlCommentInModule(pos))
            LexScript ->
              // --> is only a comment at start of a line
              case line_start {
                True ->
                  skip_line_comment(src, pos + 3, newlines, line_start, mode)
                False -> Ok(#(pos, newlines))
              }
          }
        _ -> Ok(#(pos, newlines))
      }
    "#" if pos == 0 ->
      case char_at(src, pos + 1) {
        "!" -> skip_line_comment(src, pos + 2, newlines, line_start, mode)
        _ -> Ok(#(pos, newlines))
      }
    _ -> Ok(#(pos, newlines))
  }
}

fn skip_line_comment(
  src: String,
  pos: Int,
  newlines: Int,
  _line_start: Bool,
  mode: LexMode,
) -> Result(#(Int, Int), LexError) {
  case char_at(src, pos) {
    "" -> Ok(#(pos, newlines))
    "\r\n" | "\n" | "\r" -> skip_ws(src, pos + 1, newlines + 1, True, mode)
    "\u{2028}" | "\u{2029}" -> skip_ws(src, pos + 1, newlines + 1, True, mode)
    _ -> skip_line_comment(src, pos + 1, newlines, False, mode)
  }
}

fn skip_block_comment(
  src: String,
  pos: Int,
  newlines: Int,
  line_start: Bool,
  mode: LexMode,
) -> Result(#(Int, Int), LexError) {
  case char_at(src, pos) {
    "" -> Error(UnterminatedBlockComment(pos))
    "\r\n" | "\n" | "\r" ->
      skip_block_comment(src, pos + 1, newlines + 1, True, mode)
    "\u{2028}" | "\u{2029}" ->
      skip_block_comment(src, pos + 1, newlines + 1, True, mode)
    "*" ->
      case char_at(src, pos + 1) {
        "/" -> skip_ws(src, pos + 2, newlines, line_start, mode)
        _ -> skip_block_comment(src, pos + 1, newlines, line_start, mode)
      }
    _ -> skip_block_comment(src, pos + 1, newlines, line_start, mode)
  }
}

/// Create a token. Line number is set to 0 here — the tokenize loop overwrites it.
fn tok(kind: TokenKind, value: String, pos: Int) -> Token {
  Token(kind:, value:, pos:, line: 0, raw_len: string.length(value))
}

fn read_token(src: String, pos: Int) -> Result(Token, LexError) {
  let ch = char_at(src, pos)
  case ch {
    // Single-char punctuation
    "(" -> Ok(tok(LeftParen, "(", pos))
    ")" -> Ok(tok(RightParen, ")", pos))
    "{" -> Ok(tok(LeftBrace, "{", pos))
    "}" -> Ok(tok(RightBrace, "}", pos))
    "[" -> Ok(tok(LeftBracket, "[", pos))
    "]" -> Ok(tok(RightBracket, "]", pos))
    ";" -> Ok(tok(Semicolon, ";", pos))
    "," -> Ok(tok(Comma, ",", pos))
    "~" -> Ok(tok(Tilde, "~", pos))
    ":" -> Ok(tok(Colon, ":", pos))

    // Dot / spread
    "." -> read_dot(src, pos)

    // Operators with multi-char variants
    "+" -> read_plus(src, pos)
    "-" -> read_minus(src, pos)
    "*" -> read_star(src, pos)
    "/" -> read_slash(src, pos)
    "%" -> read_percent(src, pos)
    "=" -> read_equal(src, pos)
    "!" -> read_bang(src, pos)
    "<" -> read_less_than(src, pos)
    ">" -> read_greater_than(src, pos)
    "&" -> read_ampersand(src, pos)
    "|" -> read_pipe(src, pos)
    "^" -> read_caret(src, pos)
    "?" -> read_question(src, pos)

    // String literals
    "\"" -> read_string(src, pos, "\"")
    "'" -> read_string(src, pos, "'")

    // Template literals
    "`" -> read_template_literal(src, pos)

    // Numbers
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
      read_number(src, pos)

    // Identifiers and keywords
    "\\" ->
      case char_at(src, pos + 1) {
        "u" ->
          // Try reading as identifier with unicode escape (\uXXXX or \u{XXXX}).
          // If it fails (e.g. the codepoint isn't a valid identifier char),
          // fall back to Illegal spanning the full escape sequence so the
          // lexer skips past it entirely (avoids IdentifierAfterNumericLiteral
          // errors on sequences like \u{1ffff} inside regex bodies).
          case read_identifier(src, pos) {
            Ok(token) -> Ok(token)
            Error(_) -> {
              let escape_span = unicode_escape_span(src, pos)
              Ok(Token(
                kind: Illegal,
                value: slice(src, pos, escape_span),
                pos: pos,
                line: 0,
                raw_len: escape_span,
              ))
            }
          }
        // Backslash not followed by 'u' — not a valid identifier escape.
        // Produce an Illegal token so the lexer can continue past
        // characters that will be re-scanned as regex body by the parser.
        _ -> Ok(tok(Illegal, "\\", pos))
      }
    _ ->
      case is_identifier_start(ch) {
        True -> read_identifier(src, pos)
        False -> Error(UnexpectedCharacter(ch, pos))
      }
  }
}

// --- Punctuation readers ---

fn read_dot(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "." ->
      case char_at(src, pos + 2) {
        "." -> Ok(tok(DotDotDot, "...", pos))
        _ -> Ok(tok(Dot, ".", pos))
      }
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
      read_number(src, pos)
    _ -> Ok(tok(Dot, ".", pos))
  }
}

fn read_plus(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "+" -> Ok(tok(PlusPlus, "++", pos))
    "=" -> Ok(tok(PlusEqual, "+=", pos))
    _ -> Ok(tok(Plus, "+", pos))
  }
}

fn read_minus(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "-" -> Ok(tok(MinusMinus, "--", pos))
    "=" -> Ok(tok(MinusEqual, "-=", pos))
    _ -> Ok(tok(Minus, "-", pos))
  }
}

fn read_star(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "*" ->
      case char_at(src, pos + 2) {
        "=" -> Ok(tok(StarStarEqual, "**=", pos))
        _ -> Ok(tok(StarStar, "**", pos))
      }
    "=" -> Ok(tok(StarEqual, "*=", pos))
    _ -> Ok(tok(Star, "*", pos))
  }
}

fn read_slash(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "=" -> Ok(tok(SlashEqual, "/=", pos))
    _ -> Ok(tok(Slash, "/", pos))
  }
}

fn read_percent(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "=" -> Ok(tok(PercentEqual, "%=", pos))
    _ -> Ok(tok(Percent, "%", pos))
  }
}

fn read_equal(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "=" ->
      case char_at(src, pos + 2) {
        "=" -> Ok(tok(EqualEqualEqual, "===", pos))
        _ -> Ok(tok(EqualEqual, "==", pos))
      }
    ">" -> Ok(tok(Arrow, "=>", pos))
    _ -> Ok(tok(Equal, "=", pos))
  }
}

fn read_bang(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "=" ->
      case char_at(src, pos + 2) {
        "=" -> Ok(tok(BangEqualEqual, "!==", pos))
        _ -> Ok(tok(BangEqual, "!=", pos))
      }
    _ -> Ok(tok(Bang, "!", pos))
  }
}

fn read_less_than(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "=" -> Ok(tok(LessThanEqual, "<=", pos))
    "<" ->
      case char_at(src, pos + 2) {
        "=" -> Ok(tok(LessThanLessThanEqual, "<<=", pos))
        _ -> Ok(tok(LessThanLessThan, "<<", pos))
      }
    _ -> Ok(tok(LessThan, "<", pos))
  }
}

fn read_greater_than(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "=" -> Ok(tok(GreaterThanEqual, ">=", pos))
    ">" ->
      case char_at(src, pos + 2) {
        "=" -> Ok(tok(GreaterThanGreaterThanEqual, ">>=", pos))
        ">" ->
          case char_at(src, pos + 3) {
            "=" -> Ok(tok(GreaterThanGreaterThanGreaterThanEqual, ">>>=", pos))
            _ -> Ok(tok(GreaterThanGreaterThanGreaterThan, ">>>", pos))
          }
        _ -> Ok(tok(GreaterThanGreaterThan, ">>", pos))
      }
    _ -> Ok(tok(GreaterThan, ">", pos))
  }
}

fn read_ampersand(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "&" ->
      case char_at(src, pos + 2) {
        "=" -> Ok(tok(AmpersandAmpersandEqual, "&&=", pos))
        _ -> Ok(tok(AmpersandAmpersand, "&&", pos))
      }
    "=" -> Ok(tok(AmpersandEqual, "&=", pos))
    _ -> Ok(tok(Ampersand, "&", pos))
  }
}

fn read_pipe(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "|" ->
      case char_at(src, pos + 2) {
        "=" -> Ok(tok(PipePipeEqual, "||=", pos))
        _ -> Ok(tok(PipePipe, "||", pos))
      }
    "=" -> Ok(tok(PipeEqual, "|=", pos))
    _ -> Ok(tok(Pipe, "|", pos))
  }
}

fn read_caret(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "=" -> Ok(tok(CaretEqual, "^=", pos))
    _ -> Ok(tok(Caret, "^", pos))
  }
}

fn read_question(src: String, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos + 1) {
    "?" ->
      case char_at(src, pos + 2) {
        "=" -> Ok(tok(QuestionQuestionEqual, "??=", pos))
        _ -> Ok(tok(QuestionQuestion, "??", pos))
      }
    "." ->
      // ?. but not ?.digit (that would be ? followed by .5 etc)
      case char_at(src, pos + 2) {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
          Ok(tok(Question, "?", pos))
        _ -> Ok(tok(QuestionDot, "?.", pos))
      }
    _ -> Ok(tok(Question, "?", pos))
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
/// Returns Ok(skip_count) where skip_count is how many chars to skip total
/// (including the backslash), or Error with a LexError.
fn validate_escape(
  src: String,
  pos: Int,
  backslash_pos: Int,
  in_template: Bool,
) -> Result(Int, LexError) {
  let ch = char_at(src, pos)
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
              case char_at(src, pos + 1) {
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
      let h1 = char_at(src, pos + 1)
      let h2 = char_at(src, pos + 2)
      case is_hex_digit(h1) && is_hex_digit(h2) {
        True -> Ok(4)
        False -> Error(InvalidHexEscapeSequence(backslash_pos))
      }
    }

    // \u must be followed by 4 hex digits or {hex_digits} with value <= 0x10FFFF
    "u" -> validate_unicode_escape(src, pos + 1, backslash_pos)

    // Line continuations — skip backslash + newline (all treated as 2 chars)
    "\r\n" | "\r" | "\n" -> Ok(2)

    // Standard escapes and all other single-char escapes — skip 2
    _ -> Ok(2)
  }
}

/// Validate \u escape. `pos` points to the char after 'u'.
fn validate_unicode_escape(
  src: String,
  pos: Int,
  backslash_pos: Int,
) -> Result(Int, LexError) {
  case char_at(src, pos) {
    "{" -> {
      // Braced unicode escape: \u{XXXX}
      // Collect hex digits until }
      let digits_start = pos + 1
      let digits_end = skip_hex_run(src, digits_start)
      let digit_count = digits_end - digits_start
      case digit_count == 0 {
        True -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
        False ->
          case char_at(src, digits_end) {
            "}" -> {
              // Validate the codepoint value <= 0x10FFFF
              let hex_str = slice(src, digits_start, digit_count)
              case int.base_parse(hex_str, 16) {
                Ok(value) ->
                  case value > 0x10FFFF {
                    True -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
                    // Total skip: \ u { digits } = 2 + 1 + digit_count + 1
                    False -> Ok(digit_count + 4)
                  }
                Error(_) -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
              }
            }
            _ -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
          }
      }
    }
    _ -> {
      // Non-braced: must be exactly 4 hex digits
      let h1 = char_at(src, pos)
      let h2 = char_at(src, pos + 1)
      let h3 = char_at(src, pos + 2)
      let h4 = char_at(src, pos + 3)
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
fn skip_hex_run(src: String, pos: Int) -> Int {
  case is_hex_digit(char_at(src, pos)) {
    True -> skip_hex_run(src, pos + 1)
    False -> pos
  }
}

/// Compute the span of a \u escape sequence starting at `pos` (the backslash).
/// Returns the number of characters in the escape: \u{...} or \uXXXX.
/// Falls back to 2 (just \u) if the format doesn't match.
fn unicode_escape_span(src: String, pos: Int) -> Int {
  case char_at(src, pos + 2) {
    "{" -> {
      // \u{...} — scan to the closing }
      let digits_end = skip_hex_run(src, pos + 3)
      case char_at(src, digits_end) {
        "}" -> digits_end + 1 - pos
        _ -> 2
      }
    }
    _ -> {
      // \uXXXX — 4 hex digits
      case
        is_hex_digit(char_at(src, pos + 2))
        && is_hex_digit(char_at(src, pos + 3))
        && is_hex_digit(char_at(src, pos + 4))
        && is_hex_digit(char_at(src, pos + 5))
      {
        True -> 6
        False -> 2
      }
    }
  }
}

// --- String reader ---

fn read_string(
  src: String,
  start: Int,
  quote: String,
) -> Result(Token, LexError) {
  read_string_body(src, start + 1, start, quote)
}

fn read_string_body(
  src: String,
  pos: Int,
  start: Int,
  quote: String,
) -> Result(Token, LexError) {
  let ch = char_at(src, pos)
  case ch {
    "" -> Error(UnterminatedStringLiteral(start))
    "\r\n" | "\n" | "\r" -> Error(UnterminatedStringLiteral(start))
    "\\" -> {
      let next = char_at(src, pos + 1)
      case next {
        "" -> Error(UnterminatedStringLiteral(start))
        _ ->
          case validate_escape(src, pos + 1, pos, False) {
            Ok(skip) -> read_string_body(src, pos + skip, start, quote)
            Error(e) -> Error(e)
          }
      }
    }
    _ ->
      case ch == quote {
        True -> {
          let len = pos - start + 1
          Ok(tok(KString, slice(src, start, len), start))
        }
        False -> read_string_body(src, pos + 1, start, quote)
      }
  }
}

// --- Template literal reader ---

fn read_template_literal(src: String, start: Int) -> Result(Token, LexError) {
  read_template_body(src, start + 1, start, 0)
}

fn read_template_body(
  src: String,
  pos: Int,
  start: Int,
  brace_depth: Int,
) -> Result(Token, LexError) {
  let ch = char_at(src, pos)
  case ch {
    "" -> Error(UnterminatedTemplateLiteral(start))
    "\\" -> {
      let next = char_at(src, pos + 1)
      case next {
        "" -> Error(UnterminatedTemplateLiteral(start))
        _ ->
          case validate_escape(src, pos + 1, pos, True) {
            Ok(skip) -> read_template_body(src, pos + skip, start, brace_depth)
            Error(e) -> Error(e)
          }
      }
    }
    "$" ->
      case char_at(src, pos + 1) {
        "{" -> read_template_body(src, pos + 2, start, brace_depth + 1)
        _ -> read_template_body(src, pos + 1, start, brace_depth)
      }
    "{" -> read_template_body(src, pos + 1, start, brace_depth + 1)
    "}" ->
      case brace_depth > 0 {
        True -> read_template_body(src, pos + 1, start, brace_depth - 1)
        False -> read_template_body(src, pos + 1, start, 0)
      }
    "`" ->
      case brace_depth > 0 {
        // Nested template literal inside an expression — skip it
        True -> {
          case read_template_literal(src, pos) {
            Ok(inner) -> {
              let end_pos = inner.pos + inner.raw_len
              read_template_body(src, end_pos, start, brace_depth)
            }
            Error(e) -> Error(e)
          }
        }
        False -> {
          let len = pos - start + 1
          Ok(tok(TemplateLiteral, slice(src, start, len), start))
        }
      }
    _ -> read_template_body(src, pos + 1, start, brace_depth)
  }
}

// --- Number reader ---

fn read_number(src: String, start: Int) -> Result(Token, LexError) {
  case char_at(src, start) {
    "0" ->
      case char_at(src, start + 1) {
        "x" | "X" -> read_hex_number(src, start + 2, start)
        "o" | "O" -> read_octal_number(src, start + 2, start)
        "b" | "B" -> read_binary_number(src, start + 2, start)
        _ -> read_decimal_number(src, start)
      }
    "." -> read_decimal_after_dot(src, start + 1, start)
    _ -> read_decimal_number(src, start)
  }
}

fn read_decimal_number(src: String, start: Int) -> Result(Token, LexError) {
  case skip_digits(src, start) {
    Error(e) -> Error(e)
    Ok(pos) -> {
      // Check for legacy octal (0-prefixed like 01, 07) — don't consume dot
      let is_legacy_octal =
        char_at(src, start) == "0"
        && pos - start > 1
        && !has_non_octal(src, start + 1, pos)
      case char_at(src, pos) {
        "." ->
          case is_legacy_octal {
            True -> finish_number(src, start, pos)
            False ->
              case char_at(src, pos + 1) {
                // Two dots: include trailing dot in number (123. is a valid float)
                "." -> finish_number(src, start, pos + 1)
                _ ->
                  case skip_digits(src, pos + 1) {
                    Error(e) -> Error(e)
                    Ok(pos2) -> read_exponent(src, start, pos2)
                  }
              }
          }
        "e" | "E" -> read_exponent(src, start, pos)
        "n" -> {
          // BigInt
          let end = pos + 1
          case check_after_numeric(src, end) {
            Ok(_) -> {
              let len = end - start
              Ok(tok(Number, slice(src, start, len), start))
            }
            Error(e) -> Error(e)
          }
        }
        _ -> finish_number(src, start, pos)
      }
    }
  }
}

fn has_non_octal(src: String, pos: Int, end: Int) -> Bool {
  case pos >= end {
    True -> False
    False ->
      case char_at(src, pos) {
        "8" | "9" -> True
        _ -> has_non_octal(src, pos + 1, end)
      }
  }
}

fn read_decimal_after_dot(
  src: String,
  pos: Int,
  start: Int,
) -> Result(Token, LexError) {
  case skip_digits(src, pos) {
    Error(e) -> Error(e)
    Ok(pos2) -> read_exponent(src, start, pos2)
  }
}

fn read_exponent(src: String, start: Int, pos: Int) -> Result(Token, LexError) {
  case char_at(src, pos) {
    "e" | "E" -> {
      let pos2 = case char_at(src, pos + 1) {
        "+" | "-" -> pos + 2
        _ -> pos + 1
      }
      case skip_digits(src, pos2) {
        Error(e) -> Error(e)
        Ok(pos3) ->
          case pos3 == pos2 {
            True -> Error(ExpectedExponentDigits(pos))
            False -> finish_number(src, start, pos3)
          }
      }
    }
    _ -> finish_number(src, start, pos)
  }
}

fn read_hex_number(src: String, pos: Int, start: Int) -> Result(Token, LexError) {
  case skip_hex_digits(src, pos) {
    Error(e) -> Error(e)
    Ok(end) ->
      case end == pos {
        True -> Error(ExpectedHexDigits(start))
        False ->
          case char_at(src, end) {
            "n" -> {
              let bigint_end = end + 1
              case check_after_numeric(src, bigint_end) {
                Ok(_) -> {
                  let len = bigint_end - start
                  Ok(tok(Number, slice(src, start, len), start))
                }
                Error(e) -> Error(e)
              }
            }
            _ -> finish_number(src, start, end)
          }
      }
  }
}

fn read_octal_number(
  src: String,
  pos: Int,
  start: Int,
) -> Result(Token, LexError) {
  case skip_octal_digits(src, pos) {
    Error(e) -> Error(e)
    Ok(end) ->
      case end == pos {
        True -> Error(ExpectedOctalDigits(start))
        False ->
          case char_at(src, end) {
            "n" -> {
              let bigint_end = end + 1
              case check_after_numeric(src, bigint_end) {
                Ok(_) -> {
                  let len = bigint_end - start
                  Ok(tok(Number, slice(src, start, len), start))
                }
                Error(e) -> Error(e)
              }
            }
            _ -> finish_number(src, start, end)
          }
      }
  }
}

fn read_binary_number(
  src: String,
  pos: Int,
  start: Int,
) -> Result(Token, LexError) {
  case skip_binary_digits(src, pos) {
    Error(e) -> Error(e)
    Ok(end) ->
      case end == pos {
        True -> Error(ExpectedBinaryDigits(start))
        False ->
          case char_at(src, end) {
            "n" -> {
              let bigint_end = end + 1
              case check_after_numeric(src, bigint_end) {
                Ok(_) -> {
                  let len = bigint_end - start
                  Ok(tok(Number, slice(src, start, len), start))
                }
                Error(e) -> Error(e)
              }
            }
            _ -> finish_number(src, start, end)
          }
      }
  }
}

/// Check that a numeric literal is not immediately followed by an identifier
/// start character or decimal digit. Per the spec, NumericLiteral must not be
/// immediately followed by IdentifierStart or DecimalDigit.
fn check_after_numeric(src: String, end: Int) -> Result(Nil, LexError) {
  let next = char_at(src, end)
  case next {
    "" -> Ok(Nil)
    _ ->
      case is_identifier_start(next) {
        True -> Error(IdentifierAfterNumericLiteral(end))
        False -> Ok(Nil)
      }
  }
}

fn finish_number(src: String, start: Int, end: Int) -> Result(Token, LexError) {
  let len = end - start
  case len > 0 {
    True ->
      case check_after_numeric(src, end) {
        Ok(_) -> Ok(tok(Number, slice(src, start, len), start))
        Error(e) -> Error(e)
      }
    False -> Error(InvalidNumber(start))
  }
}

/// Skip decimal digits with numeric separator validation.
/// Returns Ok(end_pos) or Error if separator rules violated.
/// `start` is the position where digits begin (for error reporting and
/// checking that at least one digit was consumed).
fn skip_digits(src: String, pos: Int) -> Result(Int, LexError) {
  skip_digits_loop(src, pos, pos, False)
}

fn skip_digits_loop(
  src: String,
  pos: Int,
  start: Int,
  prev_was_sep: Bool,
) -> Result(Int, LexError) {
  case char_at(src, pos) {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
      skip_digits_loop(src, pos + 1, start, False)
    "_" ->
      case prev_was_sep {
        // Consecutive separators
        True -> Error(ConsecutiveNumericSeparator(pos))
        False ->
          case pos == start {
            // Leading separator
            True -> Error(LeadingNumericSeparator(pos))
            False -> skip_digits_loop(src, pos + 1, start, True)
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
fn skip_hex_digits(src: String, pos: Int) -> Result(Int, LexError) {
  skip_hex_digits_loop(src, pos, pos, False)
}

fn skip_hex_digits_loop(
  src: String,
  pos: Int,
  start: Int,
  prev_was_sep: Bool,
) -> Result(Int, LexError) {
  case char_at(src, pos) {
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
    | "F" -> skip_hex_digits_loop(src, pos + 1, start, False)
    "_" ->
      case prev_was_sep {
        True -> Error(ConsecutiveNumericSeparator(pos))
        False ->
          case pos == start {
            True -> Error(LeadingNumericSeparator(pos))
            False -> skip_hex_digits_loop(src, pos + 1, start, True)
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
fn skip_octal_digits(src: String, pos: Int) -> Result(Int, LexError) {
  skip_octal_digits_loop(src, pos, pos, False)
}

fn skip_octal_digits_loop(
  src: String,
  pos: Int,
  start: Int,
  prev_was_sep: Bool,
) -> Result(Int, LexError) {
  case char_at(src, pos) {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" ->
      skip_octal_digits_loop(src, pos + 1, start, False)
    "_" ->
      case prev_was_sep {
        True -> Error(ConsecutiveNumericSeparator(pos))
        False ->
          case pos == start {
            True -> Error(LeadingNumericSeparator(pos))
            False -> skip_octal_digits_loop(src, pos + 1, start, True)
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
fn skip_binary_digits(src: String, pos: Int) -> Result(Int, LexError) {
  skip_binary_digits_loop(src, pos, pos, False)
}

fn skip_binary_digits_loop(
  src: String,
  pos: Int,
  start: Int,
  prev_was_sep: Bool,
) -> Result(Int, LexError) {
  case char_at(src, pos) {
    "0" | "1" -> skip_binary_digits_loop(src, pos + 1, start, False)
    "_" ->
      case prev_was_sep {
        True -> Error(ConsecutiveNumericSeparator(pos))
        False ->
          case pos == start {
            True -> Error(LeadingNumericSeparator(pos))
            False -> skip_binary_digits_loop(src, pos + 1, start, True)
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

/// Build an identifier token from its raw source span.
/// If the raw text contains unicode escapes (\uXXXX or \u{XXXX}),
/// the token value is the decoded canonical name and raw_len preserves
/// the original source length for position tracking.
/// Escaped identifiers are always Identifier kind (never keywords).
fn make_identifier_token(src: String, start: Int, end: Int) -> Token {
  let raw_len = end - start
  let raw = slice(src, start, raw_len)
  case string_contains_backslash(raw, 0) {
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

/// Check if a string contains a backslash character at or after position pos.
fn string_contains_backslash(s: String, pos: Int) -> Bool {
  string.drop_start(s, pos) |> string.contains("\\")
}

/// Decode unicode escape sequences in an identifier string.
/// Converts \uXXXX and \u{XXXX} to their actual Unicode characters.
fn decode_identifier_escapes(raw: String) -> String {
  decode_id_escapes_loop(raw, "")
}

fn decode_id_escapes_loop(remaining: String, acc: String) -> String {
  // Jump to the next backslash instead of iterating char-by-char
  case string.split_once(remaining, "\\") {
    Error(_) -> acc <> remaining
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
                    Error(_) ->
                      decode_id_escapes_loop(after_brace, acc <> before)
                  }
                // Already validated
                Error(_) -> decode_id_escapes_loop(after_brace, acc <> before)
              }
            Error(_) -> acc <> before
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
                Error(_) -> decode_id_escapes_loop(after_digits, acc <> before)
              }
            Error(_) -> decode_id_escapes_loop(after_digits, acc <> before)
          }
        }
        // Shouldn't happen (already validated)
        _ -> acc <> before
      }
    }
  }
}

fn read_identifier(src: String, start: Int) -> Result(Token, LexError) {
  case char_at(src, start) {
    "\\" -> {
      // Must be a valid unicode escape that decodes to ID_Start
      case validate_identifier_escape(src, start, True) {
        Ok(first_end) -> {
          case skip_identifier_chars_checked(src, first_end) {
            Ok(end) -> Ok(make_identifier_token(src, start, end))
            Error(e) -> Error(e)
          }
        }
        Error(e) -> Error(e)
      }
    }
    "#" -> {
      // Private field: # followed by identifier char
      case char_at(src, start + 1) {
        "\\" -> {
          case validate_identifier_escape(src, start + 1, True) {
            Ok(first_end) -> {
              case skip_identifier_chars_checked(src, first_end) {
                Ok(end) -> Ok(make_identifier_token(src, start, end))
                Error(e) -> Error(e)
              }
            }
            Error(e) -> Error(e)
          }
        }
        ch2 -> {
          // The char after # must be a valid identifier start (not # or \)
          case is_identifier_start_simple(ch2) {
            True -> {
              let first_end = start + 2
              case skip_identifier_chars_checked(src, first_end) {
                Ok(end) -> Ok(make_identifier_token(src, start, end))
                Error(e) -> Error(e)
              }
            }
            False -> Error(UnexpectedCharacter("#", start))
          }
        }
      }
    }
    _ -> {
      let first_end = start + 1
      case skip_identifier_chars_checked(src, first_end) {
        Ok(end) -> Ok(make_identifier_token(src, start, end))
        Error(e) -> Error(e)
      }
    }
  }
}

/// Validate a unicode escape in an identifier context.
/// `pos` points to the `\` character.
/// `is_start` indicates whether this is the first character (ID_Start) or not (ID_Continue).
/// Returns Ok(end_pos) after the escape, or Error.
fn validate_identifier_escape(
  src: String,
  pos: Int,
  is_start: Bool,
) -> Result(Int, LexError) {
  // Must be \u
  case char_at(src, pos + 1) {
    "u" -> {
      case char_at(src, pos + 2) {
        "{" -> {
          // Braced: \u{XXXX}
          let digits_start = pos + 3
          let digits_end = skip_hex_run(src, digits_start)
          let digit_count = digits_end - digits_start
          case digit_count == 0 {
            True -> Error(InvalidUnicodeEscapeSequence(pos))
            False ->
              case char_at(src, digits_end) {
                "}" -> {
                  let hex_str = slice(src, digits_start, digit_count)
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
                    Error(_) -> Error(InvalidUnicodeEscapeSequence(pos))
                  }
                }
                _ -> Error(InvalidUnicodeEscapeSequence(pos))
              }
          }
        }
        _ -> {
          // Non-braced: \uXXXX — exactly 4 hex digits
          let h1 = char_at(src, pos + 2)
          let h2 = char_at(src, pos + 3)
          let h3 = char_at(src, pos + 4)
          let h4 = char_at(src, pos + 5)
          case
            is_hex_digit(h1)
            && is_hex_digit(h2)
            && is_hex_digit(h3)
            && is_hex_digit(h4)
          {
            True -> {
              let hex_str = slice(src, pos + 2, 4)
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
fn skip_identifier_chars_checked(src: String, pos: Int) -> Result(Int, LexError) {
  let ch = char_at(src, pos)
  case ch {
    "" -> Ok(pos)
    "\\" -> {
      // Try to validate a unicode escape continuation (\uXXXX or \u{XXXX}).
      // If it fails, treat the backslash as the end of the identifier rather
      // than a hard error. This allows the lexer to continue past characters
      // that will be re-scanned as regex body by the parser.
      case validate_identifier_escape(src, pos, False) {
        Ok(next_pos) -> skip_identifier_chars_checked(src, next_pos)
        Error(_) -> Ok(pos)
      }
    }
    _ ->
      case is_identifier_continue(ch) {
        True -> skip_identifier_chars_checked(src, pos + 1)
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

// --- Character utilities ---

/// Get a single grapheme at position `pos`.
/// Returns "" if pos is past the end.
fn char_at(s: String, pos: Int) -> String {
  case pos < 0 {
    True -> ""
    False -> string.drop_start(s, pos) |> string.slice(0, 1)
  }
}

/// Get a substring. Returns the available characters if fewer than `len`.
fn slice(s: String, start: Int, len: Int) -> String {
  s |> string.drop_start(start) |> string.slice(0, len)
}
