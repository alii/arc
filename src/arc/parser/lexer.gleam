/// JavaScript lexer for Arc.
/// Converts source text into a stream of tokens.
/// Operates on raw bytes (UTF-8) for O(1) character access.
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type Token {
  /// `had_escape` is True when the token's source contained a unicode escape
  /// (only set for identifiers). A contextual keyword written with an escape
  /// (e.g. `get`) is not treated as that keyword by the grammar.
  ///
  /// `lex_error` is `Some(err)` on exactly one kind of token: the zero-length
  /// `Illegal` sentinel a HARD lexer error is materialised into (see
  /// `hard_error_token`). It carries the lexer's typed `LexError` straight
  /// through to the parser — no rendered prose in `value` to re-parse. Every
  /// other token, including a LENIENT `Illegal` (a stray character a regex
  /// body could have made legal), has `None`.
  Token(
    kind: TokenKind,
    value: String,
    pos: Int,
    line: Int,
    raw_len: Int,
    had_escape: Bool,
    lex_error: Option(LexError),
  )
}

pub type TokenKind {
  // Literals. Regex literals never get their own kind: the lexer emits
  // `Slash`/`SlashEqual` and the parser re-lexes the source as a regex when
  // one is grammatically possible (see `parse_regex_literal`).
  //
  // Template literals are lexed as SPANS, parser-driven:
  //   TemplateLiteral   `…`  or  }…`   — a span ending at a backtick
  //                     (a complete no-substitution template, or the TAIL
  //                     of one, §12.9.6 TemplateTail)
  //   TemplateHead      `…${ or  }…${  — a span ending at `${`: a
  //                     substitution follows (TemplateHead / TemplateMiddle)
  // The `}…` forms are only ever produced by `scan_template_continuation`,
  // which the PARSER calls at the `}` closing a substitution — the token
  // grammar alone cannot know a `}` re-enters a template.
  Number
  KString
  TemplateLiteral
  TemplateHead

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

/// The failures the lexer treats as hard errors — every variant here aborts
/// the whole lex. The lexer is deliberately lenient about anything a regex
/// body could legally contain (the parser re-scans regex literals from
/// source): an unexpected character, an unterminated string or template, and
/// an identifier glued onto a numeric literal all become `Illegal` tokens
/// instead, so the surrounding source still lexes and the PARSER rejects any
/// stray `Illegal` it actually reaches outside a regex position.
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

pub fn lex_error_pos(error: LexError) -> Int {
  case error {
    UnterminatedBlockComment(pos:) -> pos
    InvalidEscapeSequence(pos:) -> pos
    InvalidHexEscapeSequence(pos:) -> pos
    InvalidUnicodeEscapeSequence(pos:) -> pos
    ExpectedExponentDigits(pos:) -> pos
    ExpectedHexDigits(pos:) -> pos
    ExpectedOctalDigits(pos:) -> pos
    ExpectedBinaryDigits(pos:) -> pos
    ConsecutiveNumericSeparator(pos:) -> pos
    LeadingNumericSeparator(pos:) -> pos
    TrailingNumericSeparator(pos:) -> pos
    InvalidBigIntLiteral(pos:) -> pos
    HtmlCommentInModule(pos:) -> pos
  }
}

/// Script vs Module lexical goal: modules reject HTML-like comments.
pub type LexMode {
  LexScript
  LexModule
}

/// An incremental scanning cursor: a byte position (and its 1-based source
/// line) inside `bytes`. `scan_next` lexes exactly one token and returns
/// the advanced cursor.
///
/// This is how the PARSER lexes — on demand, token by token — so that when
/// it re-scans something the token grammar cannot classify without
/// grammatical context (a regex literal, a template continuation) it can
/// simply continue from the end of the re-scan. A whole-file up-front pass
/// cannot do that: everything it lexed past such a construct is garbage.
pub type Scanner {
  Scanner(bytes: BitArray, pos: Int, line: Int, mode: LexMode)
}

/// A scanner positioned at byte `pos` of `bytes`, on 1-based line `line`.
pub fn scanner_at(
  bytes: BitArray,
  pos: Int,
  line: Int,
  mode: LexMode,
) -> Scanner {
  Scanner(bytes:, pos:, line:, mode:)
}

/// Lex exactly one token from the scanner's position, threading bare
/// byte/line integers (no allocation beyond the token itself). This is the
/// parser's refill path — it lexes on demand, one token at a time.
///
/// A hard lexer error (unterminated block comment, invalid escape, …) is
/// materialised INTO the stream as a zero-length `Illegal` token carrying
/// the typed `LexError`: no grammar production accepts `Illegal`, so the parser
/// reports a SyntaxError at exactly the error's position — and hard errors
/// inside source the parser never reaches (or jumps over, e.g. a regex
/// body) are never raised at all.
///
/// Returns the token and the scanner just past it. Past end of input it
/// yields Eof forever.
pub fn scan_next(s: Scanner) -> #(Token, Scanner) {
  let Scanner(bytes:, pos:, line:, mode:) = s
  case skip_whitespace_and_comments(bytes, pos, mode) {
    Error(err) -> hard_error_token(err, bytes, pos, line, mode)
    Ok(#(new_pos, ws_newlines, rest)) -> {
      let token_line = line + ws_newlines
      case read_fast_punct(rest) {
        Some(#(kind, value)) -> #(
          Token(kind, value, new_pos, token_line, 1, False, None),
          Scanner(bytes:, pos: new_pos + 1, line: token_line, mode:),
        )
        None ->
          case char_at(bytes, new_pos) {
            "" -> #(
              Token(Eof, "", new_pos, token_line, 0, False, None),
              Scanner(bytes:, pos: new_pos, line: token_line, mode:),
            )
            _ ->
              case read_token(bytes, new_pos) {
                Error(err) ->
                  hard_error_token(err, bytes, new_pos, token_line, mode)
                Ok(token) -> {
                  let token = Token(..token, line: token_line)
                  let end_pos = token.pos + token.raw_len
                  let end_line = case token.kind {
                    // Only these token kinds can span multiple lines
                    KString | TemplateLiteral | TemplateHead -> {
                      let raw = byte_slice(bytes, token.pos, token.raw_len)
                      token_line + count_newlines_in(raw)
                    }
                    _ -> token_line
                  }
                  #(token, Scanner(bytes:, pos: end_pos, line: end_line, mode:))
                }
              }
          }
      }
    }
  }
}

/// A hard lexer error materialised into the token stream: a zero-length
/// Illegal token carrying the typed `LexError` itself, and a scanner parked
/// at end of input so the next `scan_next` yields Eof and the stream stops
/// there.
///
/// `from`/`line` are where the failed token step started; the error may sit
/// lines further down (an unterminated `/*` reports at the `/*`, past any
/// newlines the skip already crossed), and the token's line must be the
/// ERROR's line or ASI misjudges the line break before it.
fn hard_error_token(
  err: LexError,
  bytes: BitArray,
  from: Int,
  line: Int,
  mode: LexMode,
) -> #(Token, Scanner) {
  let epos = lex_error_pos(err)
  let err_line = line + count_newlines_in(byte_slice(bytes, from, epos - from))
  #(
    Token(Illegal, "", epos, err_line, 0, False, Some(err)),
    Scanner(bytes:, pos: bit_array.byte_size(bytes), line: err_line, mode:),
  )
}

/// Tokens recognizable from their first byte alone, with no multi-char
/// variants. Mirrors the single-char punctuation arm of read_token.
fn read_fast_punct(rest: BitArray) -> Option(#(TokenKind, String)) {
  case rest {
    <<0x28, _:bytes>> -> Some(#(LeftParen, "("))
    <<0x29, _:bytes>> -> Some(#(RightParen, ")"))
    <<0x7B, _:bytes>> -> Some(#(LeftBrace, "{"))
    <<0x7D, _:bytes>> -> Some(#(RightBrace, "}"))
    <<0x5B, _:bytes>> -> Some(#(LeftBracket, "["))
    <<0x5D, _:bytes>> -> Some(#(RightBracket, "]"))
    <<0x3B, _:bytes>> -> Some(#(Semicolon, ";"))
    <<0x2C, _:bytes>> -> Some(#(Comma, ","))
    <<0x7E, _:bytes>> -> Some(#(Tilde, "~"))
    <<0x3A, _:bytes>> -> Some(#(Colon, ":"))
    _ -> None
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
) -> Result(#(Int, Int, BitArray), LexError) {
  // line_start: True when at start of input (-->  is valid comment there)
  skip_ws(bytes, pos, 0, pos == 0, mode)
}

fn skip_ws(
  bytes: BitArray,
  pos: Int,
  newlines: Int,
  line_start: Bool,
  mode: LexMode,
) -> Result(#(Int, Int, BitArray), LexError) {
  // Shebang only at byte 0 of the file
  let #(pos, newlines, line_start) = case pos {
    0 ->
      case bytes {
        <<0x23, 0x21, _:bytes>> -> {
          let n = skip_line_inner(drop_bytes(bytes, 2), 0)
          #(2 + n, newlines, False)
        }
        _ -> #(pos, newlines, line_start)
      }
    _ -> #(pos, newlines, line_start)
  }
  case skip_ws_inner(drop_bytes(bytes, pos), 0, newlines, line_start, mode) {
    WsEnd(n, nl, rest) -> Ok(#(pos + n, nl, rest))
    WsBlockUnterminated(n) -> Error(UnterminatedBlockComment(pos + n))
    WsHtmlInModule(n) -> Error(HtmlCommentInModule(pos + n))
  }
}

type WsScan {
  WsEnd(consumed: Int, newlines: Int, rest: BitArray)
  WsBlockUnterminated(at: Int)
  WsHtmlInModule(at: Int)
}

fn skip_ws_inner(
  rest: BitArray,
  n: Int,
  nl: Int,
  ls: Bool,
  mode: LexMode,
) -> WsScan {
  case rest {
    // ASCII whitespace
    <<0x20, tail:bytes>> -> skip_ws_inner(tail, n + 1, nl, ls, mode)
    <<0x09, tail:bytes>> -> skip_ws_inner(tail, n + 1, nl, ls, mode)
    <<0x0B, tail:bytes>> -> skip_ws_inner(tail, n + 1, nl, ls, mode)
    <<0x0C, tail:bytes>> -> skip_ws_inner(tail, n + 1, nl, ls, mode)
    // Line endings
    <<0x0D, 0x0A, tail:bytes>> -> skip_ws_inner(tail, n + 2, nl + 1, True, mode)
    <<0x0A, tail:bytes>> -> skip_ws_inner(tail, n + 1, nl + 1, True, mode)
    <<0x0D, tail:bytes>> -> skip_ws_inner(tail, n + 1, nl + 1, True, mode)
    // Comments
    <<0x2F, 0x2F, tail:bytes>> -> {
      let k = skip_line_inner(tail, 0)
      skip_ws_inner(drop_bytes(tail, k), n + 2 + k, nl, False, mode)
    }
    <<0x2F, 0x2A, tail:bytes>> -> skip_block_inner(tail, n + 2, nl, ls, mode)
    // <!-- HTML comment (script mode only)
    <<0x3C, 0x21, 0x2D, 0x2D, tail:bytes>> ->
      case mode {
        LexModule -> WsHtmlInModule(n)
        LexScript -> {
          let k = skip_line_inner(tail, 0)
          skip_ws_inner(drop_bytes(tail, k), n + 4 + k, nl, False, mode)
        }
      }
    // --> HTML comment (line start only, script mode only)
    <<0x2D, 0x2D, 0x3E, tail:bytes>> if ls ->
      case mode {
        LexModule -> WsHtmlInModule(n)
        LexScript -> {
          let k = skip_line_inner(tail, 0)
          skip_ws_inner(drop_bytes(tail, k), n + 3 + k, nl, False, mode)
        }
      }
    // NBSP U+00A0
    <<0xC2, 0xA0, tail:bytes>> -> skip_ws_inner(tail, n + 2, nl, ls, mode)
    // BOM U+FEFF
    <<0xEF, 0xBB, 0xBF, tail:bytes>> -> skip_ws_inner(tail, n + 3, nl, ls, mode)
    // U+1680
    <<0xE1, 0x9A, 0x80, tail:bytes>> -> skip_ws_inner(tail, n + 3, nl, ls, mode)
    // U+2000..U+200A
    <<0xE2, 0x80, b, tail:bytes>> if b >= 0x80 && b <= 0x8A ->
      skip_ws_inner(tail, n + 3, nl, ls, mode)
    // U+2028, U+2029 (line separators)
    <<0xE2, 0x80, 0xA8, tail:bytes>> ->
      skip_ws_inner(tail, n + 3, nl + 1, True, mode)
    <<0xE2, 0x80, 0xA9, tail:bytes>> ->
      skip_ws_inner(tail, n + 3, nl + 1, True, mode)
    // U+202F
    <<0xE2, 0x80, 0xAF, tail:bytes>> -> skip_ws_inner(tail, n + 3, nl, ls, mode)
    // U+205F
    <<0xE2, 0x81, 0x9F, tail:bytes>> -> skip_ws_inner(tail, n + 3, nl, ls, mode)
    // U+3000
    <<0xE3, 0x80, 0x80, tail:bytes>> -> skip_ws_inner(tail, n + 3, nl, ls, mode)
    other -> WsEnd(n, nl, other)
  }
}

fn skip_line_inner(rest: BitArray, n: Int) -> Int {
  case rest {
    <<0x0D, _:bytes>> -> n
    <<0x0A, _:bytes>> -> n
    <<0xE2, 0x80, 0xA8, _:bytes>> -> n
    <<0xE2, 0x80, 0xA9, _:bytes>> -> n
    <<b, tail:bytes>> if b < 0x80 -> skip_line_inner(tail, n + 1)
    <<b, _, tail:bytes>> if b >= 0xC0 && b < 0xE0 ->
      skip_line_inner(tail, n + 2)
    <<b, _, _, tail:bytes>> if b >= 0xE0 && b < 0xF0 ->
      skip_line_inner(tail, n + 3)
    <<b, _, _, _, tail:bytes>> if b >= 0xF0 && b < 0xF8 ->
      skip_line_inner(tail, n + 4)
    <<_, tail:bytes>> -> skip_line_inner(tail, n + 1)
    _ -> n
  }
}

fn skip_block_inner(
  rest: BitArray,
  n: Int,
  nl: Int,
  ls: Bool,
  mode: LexMode,
) -> WsScan {
  case rest {
    <<0x2A, 0x2F, tail:bytes>> -> skip_ws_inner(tail, n + 2, nl, ls, mode)
    <<0x0D, 0x0A, tail:bytes>> ->
      skip_block_inner(tail, n + 2, nl + 1, True, mode)
    <<0x0A, tail:bytes>> -> skip_block_inner(tail, n + 1, nl + 1, True, mode)
    <<0x0D, tail:bytes>> -> skip_block_inner(tail, n + 1, nl + 1, True, mode)
    <<0xE2, 0x80, 0xA8, tail:bytes>> ->
      skip_block_inner(tail, n + 3, nl + 1, True, mode)
    <<0xE2, 0x80, 0xA9, tail:bytes>> ->
      skip_block_inner(tail, n + 3, nl + 1, True, mode)
    <<b, tail:bytes>> if b < 0x80 -> skip_block_inner(tail, n + 1, nl, ls, mode)
    <<b, _, tail:bytes>> if b >= 0xC0 && b < 0xE0 ->
      skip_block_inner(tail, n + 2, nl, ls, mode)
    <<b, _, _, tail:bytes>> if b >= 0xE0 && b < 0xF0 ->
      skip_block_inner(tail, n + 3, nl, ls, mode)
    <<b, _, _, _, tail:bytes>> if b >= 0xF0 && b < 0xF8 ->
      skip_block_inner(tail, n + 4, nl, ls, mode)
    <<_, tail:bytes>> -> skip_block_inner(tail, n + 1, nl, ls, mode)
    _ -> WsBlockUnterminated(n)
  }
}

/// Create a token with explicit raw_len (in bytes).
fn tokn(kind: TokenKind, value: String, pos: Int, raw_len: Int) -> Token {
  Token(
    kind:,
    value:,
    pos:,
    line: 0,
    raw_len:,
    had_escape: False,
    lex_error: None,
  )
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
      read_number_lenient(bytes, pos)

    // Identifiers and keywords
    "\\" ->
      case char_at(bytes, pos + 1) {
        "u" ->
          // Try reading as identifier with unicode escape (\uXXXX or \u{XXXX}).
          // If it fails (e.g. the codepoint isn't a valid identifier char),
          // fall back to Illegal spanning the full escape sequence so the
          // lexer skips past it entirely and sequences like \u{1ffff} inside
          // regex bodies keep lexing.
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
                had_escape: True,
                lex_error: None,
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
        False -> {
          let width = char_width_at(bytes, pos)
          // A character that starts no token is still legal inside a regex
          // literal (e.g. `/@/`, `/#/`, or the Cf format-control U+180E),
          // which the parser re-scans from source — emit an Illegal token so
          // the lex doesn't fail outright. A stray Illegal token reached
          // outside a regex is rejected by the parser, still a SyntaxError.
          Ok(tokn(Illegal, ch, pos, width))
        }
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
      read_number_lenient(bytes, pos)
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
    // \8 and \9 — NonOctalDecimalEscapeSequence.
    // In templates: a NotEscapeSequence, always invalid.
    // In strings: legal in sloppy mode ('\8' === '8'), rejected in strict code
    // at parser level (see legacy_octal.has_strict_forbidden_escape).
    "8" | "9" ->
      case in_template {
        True -> Error(InvalidEscapeSequence(backslash_pos))
        False -> Ok(2)
      }

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
  let q = case quote {
    "\"" -> 0x22
    _ -> 0x27
  }
  read_string_body(bytes, start + 1, start, q)
}

fn read_string_body(
  bytes: BitArray,
  pos: Int,
  start: Int,
  quote: Int,
) -> Result(Token, LexError) {
  case scan_string_inner(drop_bytes(bytes, pos), 0, quote) {
    StrQuote(n) -> {
      let raw_len = pos + n - start + 1
      let content = byte_slice(bytes, start + 1, raw_len - 2)
      Ok(tokn(KString, content, start, raw_len))
    }
    StrEscape(n) -> {
      let at = pos + n
      case char_at(bytes, at + 1) {
        "" -> Ok(unterminated_quote_token(bytes, start))
        _ -> {
          use skip <- result.try(validate_escape(bytes, at + 1, at, False))
          read_string_body(bytes, at + skip, start, quote)
        }
      }
    }
    // An unterminated string is legal inside a regex literal (`/'/`), which
    // the parser re-scans from source — emit an Illegal token spanning just
    // the quote so the rest of the input still lexes. A stray Illegal token
    // outside a regex is rejected by the parser, still a SyntaxError.
    StrUnterminated -> Ok(unterminated_quote_token(bytes, start))
  }
}

fn unterminated_quote_token(bytes: BitArray, start: Int) -> Token {
  tokn(Illegal, byte_slice(bytes, start, 1), start, 1)
}

type StrScan {
  StrQuote(consumed: Int)
  StrEscape(consumed: Int)
  StrUnterminated
}

fn scan_string_inner(rest: BitArray, n: Int, quote: Int) -> StrScan {
  case rest {
    <<b, _:bytes>> if b == quote -> StrQuote(n)
    <<0x5C, _:bytes>> -> StrEscape(n)
    <<0x0A, _:bytes>> -> StrUnterminated
    <<0x0D, _:bytes>> -> StrUnterminated
    <<b, tail:bytes>> if b < 0x80 -> scan_string_inner(tail, n + 1, quote)
    <<b, _, tail:bytes>> if b >= 0xC0 && b < 0xE0 ->
      scan_string_inner(tail, n + 2, quote)
    <<b, _, _, tail:bytes>> if b >= 0xE0 && b < 0xF0 ->
      scan_string_inner(tail, n + 3, quote)
    <<b, _, _, _, tail:bytes>> if b >= 0xF0 && b < 0xF8 ->
      scan_string_inner(tail, n + 4, quote)
    <<_, tail:bytes>> -> scan_string_inner(tail, n + 1, quote)
    _ -> StrUnterminated
  }
}

// --- Template literal reader ---
//
// Templates are lexed as SPANS (the QuickJS / V8 shape). A span never
// looks inside `${…}`: the substitution's contents are ordinary tokens
// produced by the ordinary scanner, its expression is parsed by the
// ordinary grammar, and at its closing `}` the parser explicitly asks for
// the next span via `scan_template_continuation`. There is therefore no
// brace counting here and no way for a string, comment, regex or nested
// template inside a substitution to confuse the span scan.

/// Lex the template span opening at the backtick at `start`: a complete
/// no-substitution template (TemplateLiteral, `…`) or a head ending at
/// `${` (TemplateHead). Called by the ordinary scanner on a backtick.
fn read_template_literal(
  bytes: BitArray,
  start: Int,
) -> Result(Token, LexError) {
  read_template_span(bytes, start + 1, start)
}

/// Lex the template span that CONTINUES at the `}` closing a substitution
/// — TemplateMiddle / TemplateTail lexically begin with that `}`
/// (§12.9.6). Returns the span token (TemplateHead when another
/// substitution follows, TemplateLiteral when the template ends) and a
/// scanner positioned just past it, its line count advanced over any line
/// terminators inside the span.
///
/// Only the PARSER can call this: a `}` is a plain RightBrace to the token
/// grammar, and only the grammar knows it terminates a substitution.
pub fn scan_template_continuation(
  bytes: BitArray,
  rbrace_pos: Int,
  line: Int,
  mode: LexMode,
) -> Result(#(Token, Scanner), LexError) {
  use token <- result.try(read_template_span(bytes, rbrace_pos + 1, rbrace_pos))
  let token = Token(..token, line:)
  let end_pos = token.pos + token.raw_len
  let raw = byte_slice(bytes, token.pos, token.raw_len)
  Ok(#(token, scanner_at(bytes, end_pos, line + count_newlines_in(raw), mode)))
}

/// Scan one template span starting at `start` (a backtick or the `}` of a
/// substitution), with `pos` just past that opening delimiter. Ends at an
/// unescaped `` ` `` (TemplateLiteral) or `${` (TemplateHead), both
/// included in the token's raw text.
fn read_template_span(
  bytes: BitArray,
  pos: Int,
  start: Int,
) -> Result(Token, LexError) {
  let ch = char_at(bytes, pos)
  case ch {
    // An unterminated template is legal inside a regex literal (`` /`/ ``),
    // which the parser re-scans from source — emit an Illegal token spanning
    // just the opening delimiter so the rest of the input still lexes. A
    // stray Illegal token outside a regex is rejected by the parser.
    "" -> Ok(unterminated_quote_token(bytes, start))
    "\\" -> {
      let next = char_at(bytes, pos + 1)
      case next {
        "" -> Ok(unterminated_quote_token(bytes, start))
        _ ->
          case validate_escape(bytes, pos + 1, pos, True) {
            Ok(skip) -> read_template_span(bytes, pos + skip, start)
            // Invalid escape sequences are LEGAL in tagged templates (the
            // cooked value becomes undefined, §12.9.6); the lexer can't know
            // whether this template is tagged, so it tolerates them and the
            // parser raises the SyntaxError for untagged templates when
            // cooking the quasi. Skip the backslash plus the escape lead-in
            // ("\u{" as a unit so a dangling "{" is not mistaken for one).
            Error(_invalid_escape) ->
              case char_at(bytes, pos + 1), char_at(bytes, pos + 2) {
                "u", "{" -> read_template_span(bytes, pos + 3, start)
                _, _ ->
                  read_template_span(
                    bytes,
                    pos + 1 + char_width_at(bytes, pos + 1),
                    start,
                  )
              }
          }
      }
    }
    "$" ->
      case char_at(bytes, pos + 1) {
        "{" -> {
          let len = pos + 2 - start
          Ok(tokn(TemplateHead, byte_slice(bytes, start, len), start, len))
        }
        _ -> read_template_span(bytes, pos + 1, start)
      }
    "`" -> {
      let len = pos - start + 1
      Ok(tokn(TemplateLiteral, byte_slice(bytes, start, len), start, len))
    }
    _ -> read_template_span(bytes, pos + char_width_at(bytes, pos), start)
  }
}

// --- Number reader ---

/// Lex a numeric literal, but degrade invalid numeric literals into an
/// Illegal token instead of a hard lex error. Sequences like `9A` or `9_$`
/// are legal inside regex bodies (e.g. `/[0-9A-Z]/`, `/[a-z0-9_$]/`), which
/// the parser re-scans from source and skips the pre-lexed tokens over. An
/// Illegal token that the parser actually reaches outside a regex is still
/// rejected as a SyntaxError.
///
/// The Illegal token spans from the literal start up to (excluding) the
/// error position, but always at least one character, so lexing makes
/// progress and never slices into a multi-byte codepoint (every numeric
/// lex error is positioned at an ASCII char or a codepoint boundary).
fn read_number_lenient(bytes: BitArray, start: Int) -> Result(Token, LexError) {
  case read_number(bytes, start) {
    Ok(token) -> Ok(token)
    Error(err) -> {
      let end = int.max(lex_error_pos(err), start + 1)
      Ok(tokn(
        Illegal,
        byte_slice(bytes, start, end - start),
        start,
        end - start,
      ))
    }
  }
}

fn read_number(bytes: BitArray, start: Int) -> Result(Token, LexError) {
  case char_at(bytes, start) {
    "0" ->
      case char_at(bytes, start + 1) {
        "x" | "X" ->
          read_radix_number(
            bytes,
            start + 2,
            start,
            skip_hex_digits,
            ExpectedHexDigits,
          )
        "o" | "O" ->
          read_radix_number(
            bytes,
            start + 2,
            start,
            skip_octal_digits,
            ExpectedOctalDigits,
          )
        "b" | "B" ->
          read_radix_number(
            bytes,
            start + 2,
            start,
            skip_binary_digits,
            ExpectedBinaryDigits,
          )
        _ -> read_decimal_number(bytes, start)
      }
    "." -> read_decimal_after_dot(bytes, start + 1, start)
    _ -> read_decimal_number(bytes, start)
  }
}

fn read_decimal_number(bytes: BitArray, start: Int) -> Result(Token, LexError) {
  use pos <- result.try(skip_digits(bytes, start))
  // 0-prefixed integer: LegacyOctalIntegerLiteral (01, 07) or
  // NonOctalDecimalIntegerLiteral (08, 09). Neither allows numeric
  // separators, and neither can be a BigInt.
  let has_leading_zero = char_at(bytes, start) == "0" && pos - start > 1
  use Nil <- result.try(
    case has_leading_zero && has_separator(bytes, start, pos) {
      True -> Error(LeadingNumericSeparator(start))
      False -> Ok(Nil)
    },
  )
  // Check for legacy octal (0-prefixed like 01, 07) — don't consume dot
  let is_legacy_octal =
    has_leading_zero && !has_non_octal(bytes, start + 1, pos)
  case char_at(bytes, pos) {
    "." ->
      case is_legacy_octal {
        True -> Ok(number_token(bytes, start, pos))
        False ->
          case char_at(bytes, pos + 1) {
            // Two dots: include trailing dot in number (123. is a valid float)
            "." -> Ok(number_token(bytes, start, pos + 1))
            _ -> {
              use pos2 <- result.try(skip_digits(bytes, pos + 1))
              read_exponent(bytes, start, pos2)
            }
          }
      }
    // LegacyOctalIntegerLiteral takes no ExponentPart, so `01e2` is the
    // number `01` followed by IdentifierStart `e` — an Illegal token,
    // matching V8/QuickJS. NonOctalDecimalIntegerLiteral (08, 09) does
    // allow an exponent, and is_legacy_octal is False for those.
    "e" | "E" ->
      case is_legacy_octal {
        True -> Ok(number_token(bytes, start, pos))
        False -> read_exponent(bytes, start, pos)
      }
    "n" -> {
      // BigInt — only `0n` or a literal without a leading zero is valid:
      // 00n, 01n, 08n etc. are syntax errors.
      use <- bool.guard(has_leading_zero, Error(InvalidBigIntLiteral(start)))
      Ok(number_token(bytes, start, pos + 1))
    }
    _ -> Ok(number_token(bytes, start, pos))
  }
}

/// True if the source span [pos, end) contains a numeric separator `_`.
fn has_separator(bytes: BitArray, pos: Int, end: Int) -> Bool {
  case pos >= end {
    True -> False
    False ->
      case char_at(bytes, pos) {
        "_" -> True
        _ -> has_separator(bytes, pos + 1, end)
      }
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
        False -> Ok(number_token(bytes, start, pos3))
      }
    }
    _ -> Ok(number_token(bytes, start, pos))
  }
}

fn read_radix_number(
  bytes: BitArray,
  pos: Int,
  start: Int,
  skip_fn: fn(BitArray, Int) -> Result(Int, LexError),
  err: fn(Int) -> LexError,
) -> Result(Token, LexError) {
  use end <- result.try(skip_fn(bytes, pos))
  case end == pos {
    True -> Error(err(start))
    False ->
      case char_at(bytes, end) {
        "n" -> Ok(number_token(bytes, start, end + 1))
        _ -> Ok(number_token(bytes, start, end))
      }
  }
}

/// Build the token for a numeric literal spanning [start, end) — always a
/// non-empty span, since a number is only ever read starting from a decimal
/// digit (or `.` + digit). Per the spec,
/// NumericLiteral must not be immediately followed by IdentifierStart or
/// DecimalDigit — but inside a regex literal (`/1a/`) the sequence is legal
/// and re-scanned from source by the parser, so emit an Illegal token
/// spanning the number plus the trailing identifier characters instead of
/// failing the whole lex. The parser rejects a stray Illegal token anywhere
/// outside a regex body, which is still a SyntaxError.
fn number_token(bytes: BitArray, start: Int, end: Int) -> Token {
  let next = char_at(bytes, end)
  // A backslash only begins an identifier when it is a valid unicode escape
  // decoding to ID_Start. Sequences like a digit followed by an escape for an
  // ID_Continue-only codepoint occur inside regex literal bodies — those are
  // not IdentifierStart, so the number token ends cleanly before them.
  let id_follows = case next {
    "" -> False
    "\\" -> result.is_ok(validate_identifier_escape(bytes, end, True))
    _ -> is_identifier_start(next)
  }
  case id_follows {
    True -> {
      let id_end = case skip_ident_inner(drop_bytes(bytes, end), 0) {
        IdEnd(n) -> end + n
        IdEscape(n) -> end + n
      }
      // A `\` or `#` directly after the digits consumes no identifier chars
      // above — still span at least one character so the lex makes progress.
      let id_end = int.max(id_end, end + 1)
      let len = id_end - start
      tokn(Illegal, byte_slice(bytes, start, len), start, len)
    }
    False -> {
      let len = end - start
      tokn(Number, byte_slice(bytes, start, len), start, len)
    }
  }
}

/// Skip decimal digits with numeric separator validation.
/// Returns Ok(end_pos) or Error if separator rules violated.
fn skip_digits(bytes: BitArray, pos: Int) -> Result(Int, LexError) {
  skip_digits_loop(bytes, pos, pos, False, is_decimal_digit)
}

/// Skip hex digits with numeric separator validation.
fn skip_hex_digits(bytes: BitArray, pos: Int) -> Result(Int, LexError) {
  skip_digits_loop(bytes, pos, pos, False, is_hex_digit)
}

/// Skip octal digits with numeric separator validation.
fn skip_octal_digits(bytes: BitArray, pos: Int) -> Result(Int, LexError) {
  skip_digits_loop(bytes, pos, pos, False, is_octal_digit)
}

/// Skip binary digits with numeric separator validation.
fn skip_binary_digits(bytes: BitArray, pos: Int) -> Result(Int, LexError) {
  skip_digits_loop(bytes, pos, pos, False, is_binary_digit)
}

fn is_decimal_digit(ch: String) -> Bool {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn is_octal_digit(ch: String) -> Bool {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" -> True
    _ -> False
  }
}

fn is_binary_digit(ch: String) -> Bool {
  ch == "0" || ch == "1"
}

/// Shared scan loop: consume digits accepted by `is_digit`, validating
/// numeric separator rules (no leading, trailing, or consecutive `_`).
fn skip_digits_loop(
  bytes: BitArray,
  pos: Int,
  start: Int,
  prev_was_sep: Bool,
  is_digit: fn(String) -> Bool,
) -> Result(Int, LexError) {
  let ch = char_at(bytes, pos)
  case is_digit(ch) {
    True -> skip_digits_loop(bytes, pos + 1, start, False, is_digit)
    False ->
      case ch {
        "_" ->
          case prev_was_sep {
            // Consecutive separators
            True -> Error(ConsecutiveNumericSeparator(pos))
            False ->
              case pos == start {
                // Leading separator
                True -> Error(LeadingNumericSeparator(pos))
                False -> skip_digits_loop(bytes, pos + 1, start, True, is_digit)
              }
          }
        _ ->
          case prev_was_sep {
            True -> Error(TrailingNumericSeparator(pos - 1))
            False -> Ok(pos)
          }
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
      Token(
        kind:,
        value: raw,
        pos: start,
        line: 0,
        raw_len:,
        had_escape: False,
        lex_error: None,
      )
    }
    True -> {
      // Decode unicode escapes to canonical form.
      // Escaped identifiers are always Identifier, never keywords.
      let decoded = decode_identifier_escapes(raw)
      Token(
        kind: Identifier,
        value: decoded,
        pos: start,
        line: 0,
        raw_len:,
        had_escape: True,
        lex_error: None,
      )
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
            // A lone `#` is legal inside a regex literal (`/#/`), which the
            // parser re-scans from source. Emit an Illegal token — the parser
            // rejects it anywhere outside a regex body, still a SyntaxError.
            False -> Ok(tokn(Illegal, "#", start, 1))
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
pub fn validate_identifier_codepoint(cp: Int, is_start: Bool) -> Bool {
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
  case skip_ident_inner(drop_bytes(bytes, pos), 0) {
    IdEnd(n) -> Ok(pos + n)
    IdEscape(n) ->
      // Try to validate a unicode escape continuation (\uXXXX or \u{XXXX}).
      // If it fails, treat the backslash as the end of the identifier rather
      // than a hard error. This allows the lexer to continue past characters
      // that will be re-scanned as regex body by the parser.
      case validate_identifier_escape(bytes, pos + n, False) {
        Ok(next_pos) -> skip_identifier_chars_checked(bytes, next_pos)
        Error(_) -> Ok(pos + n)
      }
  }
}

type IdScan {
  IdEnd(consumed: Int)
  IdEscape(consumed: Int)
}

fn skip_ident_inner(rest: BitArray, n: Int) -> IdScan {
  case rest {
    <<b, tail:bytes>> if b >= 0x61 && b <= 0x7A -> skip_ident_inner(tail, n + 1)
    <<b, tail:bytes>> if b >= 0x41 && b <= 0x5A -> skip_ident_inner(tail, n + 1)
    <<b, tail:bytes>> if b >= 0x30 && b <= 0x39 -> skip_ident_inner(tail, n + 1)
    <<0x5F, tail:bytes>> -> skip_ident_inner(tail, n + 1)
    <<0x24, tail:bytes>> -> skip_ident_inner(tail, n + 1)
    <<0x5C, _:bytes>> -> IdEscape(n)
    // ZWNJ U+200C, ZWJ U+200D
    <<0xE2, 0x80, 0x8C, tail:bytes>> -> skip_ident_inner(tail, n + 3)
    <<0xE2, 0x80, 0x8D, tail:bytes>> -> skip_ident_inner(tail, n + 3)
    <<b, _:bytes>> if b >= 0x80 -> skip_ident_unicode(rest, n)
    _ -> IdEnd(n)
  }
}

fn skip_ident_unicode(rest: BitArray, n: Int) -> IdScan {
  case rest {
    <<b1, b2, tail:bytes>> if b1 >= 0xC0 && b1 < 0xE0 -> {
      let cp =
        int.bitwise_or(
          int.bitwise_shift_left(int.bitwise_and(b1, 0x1F), 6),
          int.bitwise_and(b2, 0x3F),
        )
      case is_unicode_id_continue(cp) {
        True -> skip_ident_inner(tail, n + 2)
        False -> IdEnd(n)
      }
    }
    <<b1, b2, b3, tail:bytes>> if b1 >= 0xE0 && b1 < 0xF0 -> {
      let cp =
        int.bitwise_or(
          int.bitwise_or(
            int.bitwise_shift_left(int.bitwise_and(b1, 0x0F), 12),
            int.bitwise_shift_left(int.bitwise_and(b2, 0x3F), 6),
          ),
          int.bitwise_and(b3, 0x3F),
        )
      case is_unicode_id_continue(cp) {
        True -> skip_ident_inner(tail, n + 3)
        False -> IdEnd(n)
      }
    }
    <<b1, b2, b3, b4, tail:bytes>> if b1 >= 0xF0 && b1 < 0xF8 -> {
      let cp =
        int.bitwise_or(
          int.bitwise_or(
            int.bitwise_or(
              int.bitwise_shift_left(int.bitwise_and(b1, 0x07), 18),
              int.bitwise_shift_left(int.bitwise_and(b2, 0x3F), 12),
            ),
            int.bitwise_shift_left(int.bitwise_and(b3, 0x3F), 6),
          ),
          int.bitwise_and(b4, 0x3F),
        )
      case is_unicode_id_continue(cp) {
        True -> skip_ident_inner(tail, n + 4)
        False -> IdEnd(n)
      }
    }
    _ -> IdEnd(n)
  }
}

fn is_identifier_start(ch: String) -> Bool {
  case ch {
    "\\" | "#" -> True
    _ -> is_identifier_start_simple(ch)
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

@external(erlang, "arc_unicode_ffi", "is_id_start")
fn is_unicode_id_start(cp: Int) -> Bool

@external(erlang, "arc_unicode_ffi", "is_id_continue")
fn is_unicode_id_continue(cp: Int) -> Bool

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
///
/// The source binary comes from an already-valid Gleam String and every
/// offset the lexer produces is a char boundary, so the FFI skips the UTF-8
/// re-validation that bit_array.to_string would perform on every token.
/// Out-of-range slices return "".
@external(erlang, "arc_parser_ffi", "unsafe_byte_slice")
fn byte_slice(bytes: BitArray, start: Int, len: Int) -> String

/// Tail of the byte array from byte offset `pos`. Out-of-range returns <<>>.
@external(erlang, "arc_parser_ffi", "drop_bytes")
fn drop_bytes(bytes: BitArray, pos: Int) -> BitArray
