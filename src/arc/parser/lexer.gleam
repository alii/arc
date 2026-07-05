/// JavaScript lexer for Arc.
/// Converts source text into a stream of tokens.
/// Operates on raw bytes (UTF-8) for O(1) character access.
import arc/internal/digits
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
  /// `annex_b_legacy` records that this token's source used one of the Annex B
  /// legacy forms that strict code rejects as an early SyntaxError, decided by
  /// the scan that read the token rather than by re-scanning its text later:
  ///   * `Number` — the literal has a leading zero (§B.1.1
  ///     LegacyOctalIntegerLiteral `010`, or NonOctalDecimalIntegerLiteral
  ///     `08`, `09`, `08.5`); §12.9.3.1 forbids both in strict code.
  ///   * `KString` — the literal contains a §B.1.2 LegacyOctalEscapeSequence
  ///     (`\7`, `\012`, `\0` followed by a decimal digit) or a
  ///     NonOctalDecimalEscapeSequence (`\8`, `\9`); §12.9.4.1 forbids both.
  /// Never set on any other kind: templates reject those escapes outright, so
  /// there is nothing for strict mode to reject a second time.
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
  /// The LENIENT sentinel: source the token grammar cannot classify but a
  /// regex body could legally contain (a stray character, an unterminated
  /// quote, `9A`, …). Carries no error — the parser rejects any it actually
  /// reaches with a generic unexpected-token report.
  Illegal
  /// The HARD-error sentinel: a zero-length token materialising a lexer
  /// error into the stream (see `hard_error_token`). It carries the typed
  /// `LexError` on the KIND, so only this variant can have one — no other
  /// token can claim to be a lex failure, and this one cannot forget its
  /// error.
  LexFailure(error: LexError)
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
          Token(kind, value, new_pos, token_line, 1, False, False),
          Scanner(bytes:, pos: new_pos + 1, line: token_line, mode:),
        )
        None ->
          case char_at(bytes, new_pos) {
            "" -> #(
              Token(Eof, "", new_pos, token_line, 0, False, False),
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
/// `LexFailure` token carrying the typed `LexError` itself, and a scanner parked
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
    Token(LexFailure(err), "", epos, err_line, 0, False, False),
    Scanner(bytes:, pos: bit_array.byte_size(bytes), line: err_line, mode:),
  )
}

/// The single-byte punctuation table: tokens recognizable from their first
/// byte alone, with no multi-char variants. THE table — both the fast path
/// (`read_fast_punct`, which runs first) and `read_token` consult it, so the
/// two can never disagree about what `(` lexes to.
fn single_char_punct(byte: Int) -> Option(#(TokenKind, String)) {
  case byte {
    0x28 -> Some(#(LeftParen, "("))
    0x29 -> Some(#(RightParen, ")"))
    0x7B -> Some(#(LeftBrace, "{"))
    0x7D -> Some(#(RightBrace, "}"))
    0x5B -> Some(#(LeftBracket, "["))
    0x5D -> Some(#(RightBracket, "]"))
    0x3B -> Some(#(Semicolon, ";"))
    0x2C -> Some(#(Comma, ","))
    0x7E -> Some(#(Tilde, "~"))
    0x3A -> Some(#(Colon, ":"))
    _ -> None
  }
}

/// `single_char_punct` at the front of `rest` — the scanner's fast path.
fn read_fast_punct(rest: BitArray) -> Option(#(TokenKind, String)) {
  case rest {
    <<b, _:bytes>> -> single_char_punct(b)
    _ -> None
  }
}

/// `single_char_punct` at byte position `pos`.
fn punct_at(bytes: BitArray, pos: Int) -> Option(#(TokenKind, String)) {
  case bit_array.slice(bytes, pos, 1) {
    Ok(<<b>>) -> single_char_punct(b)
    Ok(_) | Error(Nil) -> None
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
    annex_b_legacy: False,
  )
}

fn read_token(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  case punct_at(bytes, pos) {
    // Single-char punctuation (the same table the fast path uses)
    Some(#(kind, value)) -> Ok(tokn(kind, value, pos, 1))
    None -> read_non_punct_token(bytes, pos)
  }
}

fn read_non_punct_token(bytes: BitArray, pos: Int) -> Result(Token, LexError) {
  let ch = char_at(bytes, pos)
  case ch {
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
    "\"" -> read_string(bytes, pos, 0x22)
    "'" -> read_string(bytes, pos, 0x27)

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
            Error(_) -> Ok(bad_escape_token(bytes, pos, pos))
          }
        // Backslash not followed by 'u' — not a valid identifier escape.
        // Produce an Illegal token so the lexer can continue past
        // characters that will be re-scanned as regex body by the parser.
        _ -> Ok(tokn(Illegal, "\\", pos, 1))
      }
    _ ->
      case may_start_identifier_token(ch) {
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

/// A validated escape sequence: how many bytes it spans (backslash included),
/// and whether it is one of the Annex B legacy forms strict code forbids —
/// §B.1.2 LegacyOctalEscapeSequence (`\7`, `\012`, `\0` + a decimal digit) or
/// NonOctalDecimalEscapeSequence (`\8`, `\9`). The escape grammar lives HERE
/// and nowhere else, so no later pass has to walk the raw text again to
/// re-derive either fact.
type Escape {
  Escape(skip: Int, annex_b_legacy: Bool)
}

/// Validate escape sequence starting after the backslash.
/// `pos` points to the character right after `\`.
/// Returns Ok(Escape) with the total byte span (including the backslash),
/// or Error with a LexError.
fn validate_escape(
  bytes: BitArray,
  pos: Int,
  backslash_pos: Int,
  in_template: Bool,
) -> Result(Escape, LexError) {
  let ch = char_at(bytes, pos)
  case ch {
    // \8 and \9 — NonOctalDecimalEscapeSequence.
    // In templates: a NotEscapeSequence, always invalid.
    // In strings: legal in sloppy mode ('\8' === '8'), rejected in strict code
    // at parser level (via the token's `annex_b_legacy` flag).
    "8" | "9" ->
      case in_template {
        True -> Error(InvalidEscapeSequence(backslash_pos))
        False -> Ok(Escape(2, True))
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
              case digits.is_decimal(char_at(bytes, pos + 1)) {
                True -> Error(InvalidEscapeSequence(backslash_pos))
                False -> Ok(Escape(2, False))
              }
            _ -> Error(InvalidEscapeSequence(backslash_pos))
          }
        False ->
          case ch {
            // `\0` alone is the NUL escape, legal in strict code; `\0` followed
            // by any decimal digit is a LegacyOctalEscapeSequence.
            "0" -> Ok(Escape(2, digits.is_decimal(char_at(bytes, pos + 1))))
            _ -> Ok(Escape(2, True))
          }
      }

    // \x must be followed by exactly 2 hex digits
    "x" -> {
      let h1 = char_at(bytes, pos + 1)
      let h2 = char_at(bytes, pos + 2)
      case digits.is_hex(h1) && digits.is_hex(h2) {
        True -> Ok(Escape(4, False))
        False -> Error(InvalidHexEscapeSequence(backslash_pos))
      }
    }

    // \u must be followed by 4 hex digits or {hex_digits} with value <= 0x10FFFF
    "u" -> validate_unicode_escape(bytes, pos + 1, backslash_pos)

    // Line continuations. <CR><LF> is ONE line terminator sequence, so the
    // escape spans 3 bytes (\ + CR + LF); every other terminator spans 2.
    "\r" ->
      case char_at(bytes, pos + 1) {
        "\n" -> Ok(Escape(3, False))
        _ -> Ok(Escape(2, False))
      }
    "\n" -> Ok(Escape(2, False))

    // Standard escapes and all other single-char escapes
    _ -> Ok(Escape(1 + char_width_at(bytes, pos), False))
  }
}

/// Validate \u escape. `pos` points to the char after 'u'.
fn validate_unicode_escape(
  bytes: BitArray,
  pos: Int,
  backslash_pos: Int,
) -> Result(Escape, LexError) {
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
                    False -> Ok(Escape(digit_count + 4, False))
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
        digits.is_hex(h1)
        && digits.is_hex(h2)
        && digits.is_hex(h3)
        && digits.is_hex(h4)
      {
        True -> Ok(Escape(6, False))
        False -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
      }
    }
  }
}

/// Skip consecutive hex digits (no underscores). Used for \u{} validation.
fn skip_hex_run(bytes: BitArray, pos: Int) -> Int {
  case digits.is_hex(char_at(bytes, pos)) {
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
        digits.is_hex(char_at(bytes, pos + 2))
        && digits.is_hex(char_at(bytes, pos + 3))
        && digits.is_hex(char_at(bytes, pos + 4))
        && digits.is_hex(char_at(bytes, pos + 5))
      {
        True -> 6
        False -> 2
      }
    }
  }
}

/// A `\u…` escape that does not decode to a legal identifier character,
/// materialised as a LENIENT `Illegal` token spanning `[start, escape_end)`
/// (`start` may precede the backslash — a private name's `#`). Such an
/// escape is legal inside a regex body (`/\u{1ffff}/u`), which the parser
/// re-scans from source, so it must never abort the whole lex.
fn bad_escape_token(bytes: BitArray, start: Int, escape_pos: Int) -> Token {
  let len = escape_pos + unicode_escape_span(bytes, escape_pos) - start
  Token(
    ..tokn(Illegal, byte_slice(bytes, start, len), start, len),
    had_escape: True,
  )
}

// --- String reader ---

/// `quote` is the delimiter's BYTE (0x22 or 0x27), passed by the caller that
/// already matched it — nothing here re-derives it, so a `` ` `` can never
/// silently be treated as a `'`.
fn read_string(
  bytes: BitArray,
  start: Int,
  quote: Int,
) -> Result(Token, LexError) {
  read_string_body(bytes, start + 1, start, quote, False)
}

/// `annex_b_legacy` accumulates over the escapes already scanned: True once
/// any of them was a legacy octal / non-octal-decimal escape. The strict-mode
/// early error (§12.9.4.1) is decided from this flag on the token, so nothing
/// downstream re-walks the escape grammar.
fn read_string_body(
  bytes: BitArray,
  pos: Int,
  start: Int,
  quote: Int,
  annex_b_legacy: Bool,
) -> Result(Token, LexError) {
  case scan_string_inner(drop_bytes(bytes, pos), 0, quote) {
    StrQuote(n) -> {
      let raw_len = pos + n - start + 1
      let content = byte_slice(bytes, start + 1, raw_len - 2)
      Ok(Token(..tokn(KString, content, start, raw_len), annex_b_legacy:))
    }
    StrEscape(n) -> {
      let at = pos + n
      case char_at(bytes, at + 1) {
        "" -> Ok(unterminated_quote_token(bytes, start))
        _ -> {
          use escape <- result.try(validate_escape(bytes, at + 1, at, False))
          read_string_body(
            bytes,
            at + escape.skip,
            start,
            quote,
            annex_b_legacy || escape.annex_b_legacy,
          )
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
            // A template's escapes are never Annex B legacy forms — those are
            // hard errors above — so nothing to record on the token.
            Ok(escape) -> read_template_span(bytes, pos + escape.skip, start)
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

/// A leading zero on the integer part is exactly the Annex B shape strict
/// code forbids (§12.9.3.1): a LegacyOctalIntegerLiteral (`010`) or a
/// NonOctalDecimalIntegerLiteral (`08`, `09`, and the `08.5` / `09e2` decimals
/// built on one). Recorded on the token here, where the digits were scanned;
/// the parser reads `annex_b_legacy` rather than re-inspecting the text.
fn read_decimal_number(bytes: BitArray, start: Int) -> Result(Token, LexError) {
  use pos <- result.try(skip_digits(bytes, start))
  let has_leading_zero = char_at(bytes, start) == "0" && pos - start > 1
  use token <- result.map(read_decimal_body(bytes, start, pos, has_leading_zero))
  Token(..token, annex_b_legacy: has_leading_zero)
}

/// The digits [start, pos) are already scanned; decide what follows them.
fn read_decimal_body(
  bytes: BitArray,
  start: Int,
  pos: Int,
  has_leading_zero: Bool,
) -> Result(Token, LexError) {
  // 0-prefixed integer: LegacyOctalIntegerLiteral (01, 07) or
  // NonOctalDecimalIntegerLiteral (08, 09). Neither allows numeric
  // separators, and neither can be a BigInt.
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
    "\\" -> result.is_ok(read_identifier_escape(bytes, end, True))
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
  skip_digits_loop(bytes, pos, pos, False, digits.is_decimal)
}

/// Skip hex digits with numeric separator validation.
fn skip_hex_digits(bytes: BitArray, pos: Int) -> Result(Int, LexError) {
  skip_digits_loop(bytes, pos, pos, False, digits.is_hex)
}

/// Skip octal digits with numeric separator validation.
fn skip_octal_digits(bytes: BitArray, pos: Int) -> Result(Int, LexError) {
  skip_digits_loop(bytes, pos, pos, False, digits.is_octal)
}

/// Skip binary digits with numeric separator validation.
fn skip_binary_digits(bytes: BitArray, pos: Int) -> Result(Int, LexError) {
  skip_digits_loop(bytes, pos, pos, False, digits.is_binary)
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

/// Build an identifier token from its source span (byte positions) and its
/// already-decoded canonical `name`. `raw_len` preserves the source length so
/// an escaped identifier keeps its true width for position tracking.
/// Escaped identifiers are always Identifier kind (never keywords).
fn identifier_token(
  start: Int,
  end: Int,
  name: String,
  had_escape: Bool,
) -> Token {
  let kind = case had_escape {
    True -> Identifier
    False -> keyword_or_identifier(name)
  }
  Token(
    kind:,
    value: name,
    pos: start,
    line: 0,
    raw_len: end - start,
    had_escape:,
    annex_b_legacy: False,
  )
}

fn read_identifier(bytes: BitArray, start: Int) -> Result(Token, LexError) {
  case char_at(bytes, start) {
    "\\" -> {
      // Must be a valid unicode escape that decodes to ID_Start
      use #(first_end, head) <- result.try(read_identifier_escape(
        bytes,
        start,
        True,
      ))
      let tail = scan_identifier_tail(bytes, first_end)
      Ok(escaped_head_token(bytes, start, first_end, head, tail))
    }
    "#" -> {
      // Private field: # followed by identifier char
      case char_at(bytes, start + 1) {
        "\\" ->
          // A `#\uZZZZ` whose escape doesn't decode to an ID_Start character
          // must degrade to a lenient Illegal token, exactly like a bare
          // `\uZZZZ` does — a regex body may contain it, and the parser
          // re-scans regex bodies from source.
          case read_identifier_escape(bytes, start + 1, True) {
            Ok(#(first_end, head)) -> {
              let tail = scan_identifier_tail(bytes, first_end)
              Ok(escaped_head_token(bytes, start, first_end, "#" <> head, tail))
            }
            Error(_) -> Ok(bad_escape_token(bytes, start, start + 1))
          }
        ch2 -> {
          // The char after # must be a valid identifier start (not # or \)
          case is_identifier_start(ch2) {
            True -> {
              // # is 1 byte, then skip the first identifier char
              let first_end = start + 1 + char_width_at(bytes, start + 1)
              let tail = scan_identifier_tail(bytes, first_end)
              Ok(plain_head_token(bytes, start, first_end, tail))
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
      let tail = scan_identifier_tail(bytes, first_end)
      Ok(plain_head_token(bytes, start, first_end, tail))
    }
  }
}

/// The identifier's first character came from an escape, so `head` is its
/// decoded text (plus a leading `#` for a private name) and the token can never
/// be a keyword. `first_end` is where the raw tail begins.
fn escaped_head_token(
  bytes: BitArray,
  start: Int,
  first_end: Int,
  head: String,
  tail: IdentTail,
) -> Token {
  case tail {
    NoEscapes(end:) ->
      identifier_token(
        start,
        end,
        head <> byte_slice(bytes, first_end, end - first_end),
        True,
      )
    WithEscapes(end:, text:) -> identifier_token(start, end, head <> text, True)
  }
}

/// The identifier's first character was written literally, so if the tail holds
/// no escapes either the canonical name is exactly the source span.
fn plain_head_token(
  bytes: BitArray,
  start: Int,
  first_end: Int,
  tail: IdentTail,
) -> Token {
  case tail {
    NoEscapes(end:) ->
      identifier_token(start, end, byte_slice(bytes, start, end - start), False)
    WithEscapes(end:, text:) ->
      identifier_token(
        start,
        end,
        byte_slice(bytes, start, first_end - start) <> text,
        True,
      )
  }
}

/// Read a unicode escape in an identifier context. `pos` points to the `\`.
/// `is_start` indicates whether this is the first character (ID_Start) or not
/// (ID_Continue).
///
/// Returns Ok(#(end_pos, decoded_char)) — the position after the escape and the
/// character it denotes. Validating and decoding happen here together, so a
/// caller cannot accept an escape and then decode it a second, differing way.
fn read_identifier_escape(
  bytes: BitArray,
  pos: Int,
  is_start: Bool,
) -> Result(#(Int, String), LexError) {
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
                          decoded_identifier_char(cp, is_start, digits_end + 1)
                          |> result.replace_error(InvalidUnicodeEscapeSequence(
                            pos,
                          ))
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
            digits.is_hex(h1)
            && digits.is_hex(h2)
            && digits.is_hex(h3)
            && digits.is_hex(h4)
          {
            True -> {
              let hex_str = byte_slice(bytes, pos + 2, 4)
              case int.base_parse(hex_str, 16) {
                Ok(cp) ->
                  decoded_identifier_char(cp, is_start, pos + 6)
                  |> result.replace_error(InvalidUnicodeEscapeSequence(pos))
                Error(Nil) -> Error(InvalidUnicodeEscapeSequence(pos))
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

/// An escape is usable in an identifier only if its codepoint is legal at this
/// position and encodable — surrogates fail both, and `validate_identifier_
/// codepoint` already rejects them, so `string.utf_codepoint` cannot fail here.
/// It is still threaded as an error rather than assumed away: this is the one
/// place that turns a codepoint into identifier text.
fn decoded_identifier_char(
  cp: Int,
  is_start: Bool,
  end_pos: Int,
) -> Result(#(Int, String), Nil) {
  use <- bool.guard(!validate_identifier_codepoint(cp, is_start), Error(Nil))
  use codepoint <- result.map(string.utf_codepoint(cp))
  #(end_pos, string.from_utf_codepoints([codepoint]))
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

/// The continuation characters of an identifier, scanned from some `pos`.
/// `NoEscapes` means the canonical text of the span is exactly its source
/// bytes, so callers can slice it; `WithEscapes` carries the decoded text of
/// [pos, end) because the source and the name differ.
type IdentTail {
  NoEscapes(end: Int)
  WithEscapes(end: Int, text: String)
}

/// Scan identifier continuation characters from `pos`, decoding any unicode
/// escapes as they are validated. Never fails: an escape that does not decode
/// to an ID_Continue codepoint simply ends the identifier at the backslash,
/// which lets the lexer continue past characters the parser will re-scan as a
/// regex body.
fn scan_identifier_tail(bytes: BitArray, pos: Int) -> IdentTail {
  scan_identifier_tail_loop(bytes, pos, "", False)
}

fn scan_identifier_tail_loop(
  bytes: BitArray,
  pos: Int,
  acc: String,
  saw_escape: Bool,
) -> IdentTail {
  case skip_ident_inner(drop_bytes(bytes, pos), 0) {
    IdEnd(n) -> finish_identifier_tail(bytes, pos, n, acc, saw_escape)
    IdEscape(n) ->
      case read_identifier_escape(bytes, pos + n, False) {
        Ok(#(next_pos, char)) ->
          scan_identifier_tail_loop(
            bytes,
            next_pos,
            acc <> byte_slice(bytes, pos, n) <> char,
            True,
          )
        // Not an identifier escape — the identifier ends before the backslash.
        Error(_not_an_identifier_escape) ->
          finish_identifier_tail(bytes, pos, n, acc, saw_escape)
      }
  }
}

fn finish_identifier_tail(
  bytes: BitArray,
  pos: Int,
  n: Int,
  acc: String,
  saw_escape: Bool,
) -> IdentTail {
  case saw_escape {
    False -> NoEscapes(end: pos + n)
    True -> WithEscapes(end: pos + n, text: acc <> byte_slice(bytes, pos, n))
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

/// True for characters `read_token` hands to `read_identifier`: a real
/// IdentifierStart, plus the two characters that only *introduce* one — `\`
/// (a unicode escape, which read_identifier still has to decode and check)
/// and `#` (a private name). Neither is itself an IdentifierStart, so anything
/// asking "is this an IdentifierStart?" wants `is_identifier_start`.
fn may_start_identifier_token(ch: String) -> Bool {
  case ch {
    "\\" | "#" -> True
    _ -> is_identifier_start(ch)
  }
}

/// True iff `ch` is an ECMAScript IdentifierStart character.
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

/// Get the byte width of the single UTF-8 character at byte position `pos`.
/// Returns 0 if pos is past the end. Never spans two characters: `\r\n` is
/// two characters and `char_width_at` at the `\r` is 1.
fn char_width_at(bytes: BitArray, pos: Int) -> Int {
  case bit_array.slice(bytes, pos, 1) {
    Error(Nil) -> 0
    Ok(<<byte>>) ->
      case byte {
        b if b < 0x80 -> 1
        b if b >= 0xC0 && b < 0xE0 -> 2
        b if b >= 0xE0 && b < 0xF0 -> 3
        b if b >= 0xF0 && b < 0xF8 -> 4
        _ -> 1
      }
    _ -> 0
  }
}

/// Get the single character (one code point) at byte position `pos` in the
/// UTF-8 byte array. Returns "" if pos is past the end. A `\r` is one
/// character even when a `\n` follows: callers that care about the `\r\n`
/// pair peek at the next byte themselves.
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
/// re-validation that bit_array.to_string would perform on every token
/// (hence `unsafe_`). Out-of-range offsets are clamped into the binary; see
/// arc_bytes_ffi for the one out-of-range policy shared with the regexp
/// bridge.
@external(erlang, "arc_bytes_ffi", "unsafe_slice")
fn byte_slice(bytes: BitArray, start: Int, len: Int) -> String

/// Tail of the byte array from byte offset `pos` (clamped).
@external(erlang, "arc_bytes_ffi", "drop_start")
fn drop_bytes(bytes: BitArray, pos: Int) -> BitArray
