import arc/internal/digits
import arc/internal/utf16
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type Token {
  // had_escape: identifiers only; annex_b_legacy: Number/KString only
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
  KString
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
  KTrue
  KFalse
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

pub fn lex_error_pos(error: LexError) -> Int {
  error.pos
}

pub type LexMode {
  LexScript
  LexModule
}

pub type Scanner {
  Scanner(bytes: BitArray, pos: Int, line: Int, mode: LexMode, rest: BitArray)
}

pub fn scanner_at(
  bytes: BitArray,
  pos: Int,
  line: Int,
  mode: LexMode,
) -> Scanner {
  Scanner(bytes:, pos:, line:, mode:, rest: drop_bytes(bytes, pos))
}

pub fn scan_next(s: Scanner) -> #(Token, Scanner) {
  let Scanner(bytes:, pos:, line:, mode:, rest:) = s
  case skip_ws(rest, pos, mode) {
    WsEnd(n, ws_newlines, rest) -> {
      let new_pos = pos + n
      let token_line = line + ws_newlines
      case read_token(bytes, new_pos, token_line, rest) {
        Error(err) -> hard_error_token(err, bytes, new_pos, token_line, mode)
        Ok(token) -> {
          let raw_len = token.raw_len
          let after = case rest {
            <<_:bytes-size(raw_len), tail:bytes>> -> tail
            _ -> <<>>
          }
          let end_line = case token.kind {
            KString | TemplateLiteral | TemplateHead ->
              token_line
              + count_newlines_in(byte_slice(bytes, new_pos, raw_len))
            _ -> token_line
          }
          #(
            token,
            Scanner(
              bytes:,
              pos: new_pos + raw_len,
              line: end_line,
              mode:,
              rest: after,
            ),
          )
        }
      }
    }
    WsBlockUnterminated(n) ->
      hard_error_token(
        UnterminatedBlockComment(pos + n),
        bytes,
        pos,
        line,
        mode,
      )
    WsHtmlInModule(n) ->
      hard_error_token(HtmlCommentInModule(pos + n), bytes, pos, line, mode)
  }
}

// token line must be the error's line or asi breaks
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
    Scanner(
      bytes:,
      pos: bit_array.byte_size(bytes),
      line: err_line,
      mode:,
      rest: <<>>,
    ),
  )
}

fn count_newlines_in(s: String) -> Int {
  do_count_newlines(bit_array.from_string(s), 0)
}

fn do_count_newlines(bytes: BitArray, count: Int) -> Int {
  case bytes {
    <<13, 10, rest:bytes>> -> do_count_newlines(rest, count + 1)
    <<10, rest:bytes>> -> do_count_newlines(rest, count + 1)
    <<13, rest:bytes>> -> do_count_newlines(rest, count + 1)
    // u+2028 / u+2029
    <<0xE2, 0x80, 0xA8, rest:bytes>> -> do_count_newlines(rest, count + 1)
    <<0xE2, 0x80, 0xA9, rest:bytes>> -> do_count_newlines(rest, count + 1)
    <<_, rest:bytes>> -> do_count_newlines(rest, count)
    _ -> count
  }
}

fn skip_ws(rest: BitArray, pos: Int, mode: LexMode) -> WsScan {
  case pos, rest {
    0, <<0x23, 0x21, tail:bytes>> -> {
      let n = skip_line_inner(tail, 0)
      case skip_ws_inner(drop_bytes(tail, n), 0, 0, False, mode) {
        WsEnd(k, nl, rest) -> WsEnd(2 + n + k, nl, rest)
        WsBlockUnterminated(k) -> WsBlockUnterminated(2 + n + k)
        WsHtmlInModule(k) -> WsHtmlInModule(2 + n + k)
      }
    }
    _, _ -> skip_ws_inner(rest, 0, 0, pos == 0, mode)
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
    <<0x20, tail:bytes>> -> skip_ws_inner(tail, n + 1, nl, ls, mode)
    <<0x09, tail:bytes>> -> skip_ws_inner(tail, n + 1, nl, ls, mode)
    <<0x0B, tail:bytes>> -> skip_ws_inner(tail, n + 1, nl, ls, mode)
    <<0x0C, tail:bytes>> -> skip_ws_inner(tail, n + 1, nl, ls, mode)
    <<0x0D, 0x0A, tail:bytes>> -> skip_ws_inner(tail, n + 2, nl + 1, True, mode)
    <<0x0A, tail:bytes>> -> skip_ws_inner(tail, n + 1, nl + 1, True, mode)
    <<0x0D, tail:bytes>> -> skip_ws_inner(tail, n + 1, nl + 1, True, mode)
    <<0x2F, 0x2F, tail:bytes>> -> {
      let k = skip_line_inner(tail, 0)
      skip_ws_inner(drop_bytes(tail, k), n + 2 + k, nl, False, mode)
    }
    <<0x2F, 0x2A, tail:bytes>> -> skip_block_inner(tail, n + 2, nl, ls, mode)
    // <!-- html comment, script mode only
    <<0x3C, 0x21, 0x2D, 0x2D, tail:bytes>> ->
      case mode {
        LexModule -> WsHtmlInModule(n)
        LexScript -> {
          let k = skip_line_inner(tail, 0)
          skip_ws_inner(drop_bytes(tail, k), n + 4 + k, nl, False, mode)
        }
      }
    // --> html comment, line start, script mode only
    <<0x2D, 0x2D, 0x3E, tail:bytes>> if ls ->
      case mode {
        LexModule -> WsHtmlInModule(n)
        LexScript -> {
          let k = skip_line_inner(tail, 0)
          skip_ws_inner(drop_bytes(tail, k), n + 3 + k, nl, False, mode)
        }
      }
    // nbsp u+00a0
    <<0xC2, 0xA0, tail:bytes>> -> skip_ws_inner(tail, n + 2, nl, ls, mode)
    // bom u+feff
    <<0xEF, 0xBB, 0xBF, tail:bytes>> -> skip_ws_inner(tail, n + 3, nl, ls, mode)
    // u+1680
    <<0xE1, 0x9A, 0x80, tail:bytes>> -> skip_ws_inner(tail, n + 3, nl, ls, mode)
    // u+2000..u+200a
    <<0xE2, 0x80, b, tail:bytes>> if b >= 0x80 && b <= 0x8A ->
      skip_ws_inner(tail, n + 3, nl, ls, mode)
    // u+2028, u+2029 line separators
    <<0xE2, 0x80, 0xA8, tail:bytes>> ->
      skip_ws_inner(tail, n + 3, nl + 1, True, mode)
    <<0xE2, 0x80, 0xA9, tail:bytes>> ->
      skip_ws_inner(tail, n + 3, nl + 1, True, mode)
    // u+202f
    <<0xE2, 0x80, 0xAF, tail:bytes>> -> skip_ws_inner(tail, n + 3, nl, ls, mode)
    // u+205f
    <<0xE2, 0x81, 0x9F, tail:bytes>> -> skip_ws_inner(tail, n + 3, nl, ls, mode)
    // u+3000
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
    <<b, _, tail:bytes>> if b >= 0xC0 && b < 0xE0 -> skip_line_inner(tail, n + 2)
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

fn tokn(
  kind: TokenKind,
  value: String,
  pos: Int,
  raw_len: Int,
  line: Int,
) -> Token {
  Token(
    kind:,
    value:,
    pos:,
    line:,
    raw_len:,
    had_escape: False,
    annex_b_legacy: False,
  )
}

fn read_token(
  bytes: BitArray,
  pos: Int,
  line: Int,
  rest: BitArray,
) -> Result(Token, LexError) {
  case rest {
    <<>> -> Ok(tokn(Eof, "", pos, 0, line))

    <<0x28, _:bytes>> -> Ok(tokn(LeftParen, "(", pos, 1, line))
    <<0x29, _:bytes>> -> Ok(tokn(RightParen, ")", pos, 1, line))
    <<0x7B, _:bytes>> -> Ok(tokn(LeftBrace, "{", pos, 1, line))
    <<0x7D, _:bytes>> -> Ok(tokn(RightBrace, "}", pos, 1, line))
    <<0x5B, _:bytes>> -> Ok(tokn(LeftBracket, "[", pos, 1, line))
    <<0x5D, _:bytes>> -> Ok(tokn(RightBracket, "]", pos, 1, line))
    <<0x3B, _:bytes>> -> Ok(tokn(Semicolon, ";", pos, 1, line))
    <<0x2C, _:bytes>> -> Ok(tokn(Comma, ",", pos, 1, line))
    <<0x7E, _:bytes>> -> Ok(tokn(Tilde, "~", pos, 1, line))
    <<0x3A, _:bytes>> -> Ok(tokn(Colon, ":", pos, 1, line))

    <<0x2E, 0x2E, 0x2E, _:bytes>> -> Ok(tokn(DotDotDot, "...", pos, 3, line))
    <<0x2E, d, _:bytes>> if d >= 0x30 && d <= 0x39 ->
      Ok(read_number_lenient(bytes, pos, line, rest))
    <<0x2E, _:bytes>> -> Ok(tokn(Dot, ".", pos, 1, line))

    <<0x2B, 0x2B, _:bytes>> -> Ok(tokn(PlusPlus, "++", pos, 2, line))
    <<0x2B, 0x3D, _:bytes>> -> Ok(tokn(PlusEqual, "+=", pos, 2, line))
    <<0x2B, _:bytes>> -> Ok(tokn(Plus, "+", pos, 1, line))

    <<0x2D, 0x2D, _:bytes>> -> Ok(tokn(MinusMinus, "--", pos, 2, line))
    <<0x2D, 0x3D, _:bytes>> -> Ok(tokn(MinusEqual, "-=", pos, 2, line))
    <<0x2D, _:bytes>> -> Ok(tokn(Minus, "-", pos, 1, line))

    <<0x2A, 0x2A, 0x3D, _:bytes>> ->
      Ok(tokn(StarStarEqual, "**=", pos, 3, line))
    <<0x2A, 0x2A, _:bytes>> -> Ok(tokn(StarStar, "**", pos, 2, line))
    <<0x2A, 0x3D, _:bytes>> -> Ok(tokn(StarEqual, "*=", pos, 2, line))
    <<0x2A, _:bytes>> -> Ok(tokn(Star, "*", pos, 1, line))

    <<0x2F, 0x3D, _:bytes>> -> Ok(tokn(SlashEqual, "/=", pos, 2, line))
    <<0x2F, _:bytes>> -> Ok(tokn(Slash, "/", pos, 1, line))

    <<0x25, 0x3D, _:bytes>> -> Ok(tokn(PercentEqual, "%=", pos, 2, line))
    <<0x25, _:bytes>> -> Ok(tokn(Percent, "%", pos, 1, line))

    <<0x3D, 0x3D, 0x3D, _:bytes>> ->
      Ok(tokn(EqualEqualEqual, "===", pos, 3, line))
    <<0x3D, 0x3D, _:bytes>> -> Ok(tokn(EqualEqual, "==", pos, 2, line))
    <<0x3D, 0x3E, _:bytes>> -> Ok(tokn(Arrow, "=>", pos, 2, line))
    <<0x3D, _:bytes>> -> Ok(tokn(Equal, "=", pos, 1, line))

    <<0x21, 0x3D, 0x3D, _:bytes>> ->
      Ok(tokn(BangEqualEqual, "!==", pos, 3, line))
    <<0x21, 0x3D, _:bytes>> -> Ok(tokn(BangEqual, "!=", pos, 2, line))
    <<0x21, _:bytes>> -> Ok(tokn(Bang, "!", pos, 1, line))

    <<0x3C, 0x3D, _:bytes>> -> Ok(tokn(LessThanEqual, "<=", pos, 2, line))
    <<0x3C, 0x3C, 0x3D, _:bytes>> ->
      Ok(tokn(LessThanLessThanEqual, "<<=", pos, 3, line))
    <<0x3C, 0x3C, _:bytes>> -> Ok(tokn(LessThanLessThan, "<<", pos, 2, line))
    <<0x3C, _:bytes>> -> Ok(tokn(LessThan, "<", pos, 1, line))

    <<0x3E, 0x3D, _:bytes>> -> Ok(tokn(GreaterThanEqual, ">=", pos, 2, line))
    <<0x3E, 0x3E, 0x3D, _:bytes>> ->
      Ok(tokn(GreaterThanGreaterThanEqual, ">>=", pos, 3, line))
    <<0x3E, 0x3E, 0x3E, 0x3D, _:bytes>> ->
      Ok(tokn(GreaterThanGreaterThanGreaterThanEqual, ">>>=", pos, 4, line))
    <<0x3E, 0x3E, 0x3E, _:bytes>> ->
      Ok(tokn(GreaterThanGreaterThanGreaterThan, ">>>", pos, 3, line))
    <<0x3E, 0x3E, _:bytes>> ->
      Ok(tokn(GreaterThanGreaterThan, ">>", pos, 2, line))
    <<0x3E, _:bytes>> -> Ok(tokn(GreaterThan, ">", pos, 1, line))

    <<0x26, 0x26, 0x3D, _:bytes>> ->
      Ok(tokn(AmpersandAmpersandEqual, "&&=", pos, 3, line))
    <<0x26, 0x26, _:bytes>> -> Ok(tokn(AmpersandAmpersand, "&&", pos, 2, line))
    <<0x26, 0x3D, _:bytes>> -> Ok(tokn(AmpersandEqual, "&=", pos, 2, line))
    <<0x26, _:bytes>> -> Ok(tokn(Ampersand, "&", pos, 1, line))

    <<0x7C, 0x7C, 0x3D, _:bytes>> ->
      Ok(tokn(PipePipeEqual, "||=", pos, 3, line))
    <<0x7C, 0x7C, _:bytes>> -> Ok(tokn(PipePipe, "||", pos, 2, line))
    <<0x7C, 0x3D, _:bytes>> -> Ok(tokn(PipeEqual, "|=", pos, 2, line))
    <<0x7C, _:bytes>> -> Ok(tokn(Pipe, "|", pos, 1, line))

    <<0x5E, 0x3D, _:bytes>> -> Ok(tokn(CaretEqual, "^=", pos, 2, line))
    <<0x5E, _:bytes>> -> Ok(tokn(Caret, "^", pos, 1, line))

    // `?.5` is `?` then `.5`
    <<0x3F, 0x3F, 0x3D, _:bytes>> ->
      Ok(tokn(QuestionQuestionEqual, "??=", pos, 3, line))
    <<0x3F, 0x3F, _:bytes>> -> Ok(tokn(QuestionQuestion, "??", pos, 2, line))
    <<0x3F, 0x2E, d, _:bytes>> if d >= 0x30 && d <= 0x39 ->
      Ok(tokn(Question, "?", pos, 1, line))
    <<0x3F, 0x2E, _:bytes>> -> Ok(tokn(QuestionDot, "?.", pos, 2, line))
    <<0x3F, _:bytes>> -> Ok(tokn(Question, "?", pos, 1, line))

    <<0x22, tail:bytes>> ->
      read_string_body(bytes, tail, pos + 1, pos, 0x22, False, line)
    <<0x27, tail:bytes>> ->
      read_string_body(bytes, tail, pos + 1, pos, 0x27, False, line)

    <<0x60, tail:bytes>> ->
      Ok(read_template_span(bytes, tail, pos + 1, pos, line))

    <<d, _:bytes>> if d >= 0x30 && d <= 0x39 ->
      Ok(read_number_lenient(bytes, pos, line, rest))

    <<b, _:bytes>>
      if { b >= 0x61 && b <= 0x7A }
      || { b >= 0x41 && b <= 0x5A }
      || b == 0x5F
      || b == 0x24
    -> Ok(read_ascii_identifier(bytes, pos, line, rest))
    <<0x5C, 0x75, _:bytes>> ->
      case read_identifier(bytes, pos, line) {
        Ok(token) -> Ok(token)
        Error(_not_an_identifier) -> Ok(bad_escape_token(bytes, pos, pos, line))
      }
    <<0x5C, _:bytes>> -> Ok(tokn(Illegal, "\\", pos, 1, line))
    <<0x23, _:bytes>> -> read_identifier(bytes, pos, line)
    _ -> {
      let ch = char_at(bytes, pos)
      case is_identifier_start(ch) {
        True -> read_identifier(bytes, pos, line)
        False -> {
          let width = char_width_at(bytes, pos)
          Ok(tokn(Illegal, ch, pos, width, line))
        }
      }
    }
  }
}

type Escape {
  Escape(skip: Int, annex_b_legacy: Bool)
}

// pos is just after the backslash; len includes it
fn validate_escape(
  bytes: BitArray,
  pos: Int,
  backslash_pos: Int,
  in_template: Bool,
) -> Result(Escape, LexError) {
  let ch = char_at(bytes, pos)
  case ch {
    "8" | "9" ->
      case in_template {
        True -> Error(InvalidEscapeSequence(backslash_pos))
        False -> Ok(Escape(2, True))
      }

    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" ->
      case in_template {
        True ->
          case ch {
            "0" ->
              case digits.is_decimal_digit(char_at(bytes, pos + 1)) {
                True -> Error(InvalidEscapeSequence(backslash_pos))
                False -> Ok(Escape(2, False))
              }
            _ -> Error(InvalidEscapeSequence(backslash_pos))
          }
        False ->
          case ch {
            "0" ->
              Ok(Escape(2, digits.is_decimal_digit(char_at(bytes, pos + 1))))
            _ -> Ok(Escape(2, True))
          }
      }

    "x" -> {
      let h1 = char_at(bytes, pos + 1)
      let h2 = char_at(bytes, pos + 2)
      case digits.is_hex_digit(h1) && digits.is_hex_digit(h2) {
        True -> Ok(Escape(4, False))
        False -> Error(InvalidHexEscapeSequence(backslash_pos))
      }
    }

    "u" -> validate_unicode_escape(bytes, pos + 1, backslash_pos)

    // crlf continuation spans 3 bytes
    "\r" ->
      case char_at(bytes, pos + 1) {
        "\n" -> Ok(Escape(3, False))
        _ -> Ok(Escape(2, False))
      }
    "\n" -> Ok(Escape(2, False))

    _ -> Ok(Escape(1 + char_width_at(bytes, pos), False))
  }
}

// after_u is the byte after 'u'; syntax only, no range checks
fn scan_unicode_escape(bytes: BitArray, after_u: Int) -> Option(#(Int, Int)) {
  case char_at(bytes, after_u) {
    "{" -> {
      let digits_start = after_u + 1
      let digits_end = skip_hex_run(bytes, digits_start)
      case digits_end > digits_start && char_at(bytes, digits_end) == "}" {
        False -> None
        True ->
          byte_slice(bytes, digits_start, digits_end - digits_start)
          |> int.base_parse(16)
          |> result.map(fn(cp) { #(cp, digits_end + 1) })
          |> option.from_result
      }
    }
    _ ->
      case
        digits.is_hex_digit(char_at(bytes, after_u))
        && digits.is_hex_digit(char_at(bytes, after_u + 1))
        && digits.is_hex_digit(char_at(bytes, after_u + 2))
        && digits.is_hex_digit(char_at(bytes, after_u + 3))
      {
        False -> None
        True ->
          byte_slice(bytes, after_u, 4)
          |> int.base_parse(16)
          |> result.map(fn(cp) { #(cp, after_u + 4) })
          |> option.from_result
      }
  }
}

fn validate_unicode_escape(
  bytes: BitArray,
  pos: Int,
  backslash_pos: Int,
) -> Result(Escape, LexError) {
  case scan_unicode_escape(bytes, pos) {
    None -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
    Some(#(cp, end)) ->
      case cp > 0x10FFFF {
        True -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
        False -> Ok(Escape(end - backslash_pos, False))
      }
  }
}

fn skip_hex_run(bytes: BitArray, pos: Int) -> Int {
  case digits.is_hex_digit(char_at(bytes, pos)) {
    True -> skip_hex_run(bytes, pos + 1)
    False -> pos
  }
}

fn unicode_escape_span(bytes: BitArray, pos: Int) -> Int {
  case scan_unicode_escape(bytes, pos + 2) {
    Some(#(_, end)) -> end - pos
    None -> 2
  }
}

fn bad_escape_token(
  bytes: BitArray,
  start: Int,
  escape_pos: Int,
  line: Int,
) -> Token {
  let len = escape_pos + unicode_escape_span(bytes, escape_pos) - start
  Token(
    ..tokn(Illegal, byte_slice(bytes, start, len), start, len, line),
    had_escape: True,
  )
}

fn read_string_body(
  bytes: BitArray,
  rest: BitArray,
  pos: Int,
  start: Int,
  quote: Int,
  annex_b_legacy: Bool,
  line: Int,
) -> Result(Token, LexError) {
  case scan_string_inner(rest, 0, quote) {
    StrQuote(n) -> {
      let raw_len = pos + n - start + 1
      let content = byte_slice(bytes, start + 1, raw_len - 2)
      Ok(Token(..tokn(KString, content, start, raw_len, line), annex_b_legacy:))
    }
    StrEscape(n) -> {
      let at = pos + n
      case char_at(bytes, at + 1) {
        "" -> Ok(unterminated_quote_token(bytes, start, line))
        _ -> {
          use escape <- result.try(validate_escape(bytes, at + 1, at, False))
          let next = at + escape.skip
          read_string_body(
            bytes,
            drop_bytes(bytes, next),
            next,
            start,
            quote,
            annex_b_legacy || escape.annex_b_legacy,
            line,
          )
        }
      }
    }
    StrUnterminated -> Ok(unterminated_quote_token(bytes, start, line))
  }
}

fn unterminated_quote_token(bytes: BitArray, start: Int, line: Int) -> Token {
  tokn(Illegal, byte_slice(bytes, start, 1), start, 1, line)
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

pub fn scan_template_continuation(
  bytes: BitArray,
  rbrace_pos: Int,
  line: Int,
  mode: LexMode,
) -> #(Token, Scanner) {
  let pos = rbrace_pos + 1
  let token =
    read_template_span(bytes, drop_bytes(bytes, pos), pos, rbrace_pos, line)
  let end_pos = token.pos + token.raw_len
  let raw = byte_slice(bytes, token.pos, token.raw_len)
  #(token, scanner_at(bytes, end_pos, line + count_newlines_in(raw), mode))
}

fn read_template_span(
  bytes: BitArray,
  rest: BitArray,
  pos: Int,
  start: Int,
  line: Int,
) -> Token {
  case rest {
    <<0x5C>> -> unterminated_quote_token(bytes, start, line)
    <<0x5C, _:bytes>> ->
      case validate_escape(bytes, pos + 1, pos, True) {
        Ok(escape) -> {
          let next = pos + escape.skip
          read_template_span(bytes, drop_bytes(bytes, next), next, start, line)
        }
        // invalid escapes are legal in tagged templates, parser decides
        Error(_invalid_escape) ->
          case rest {
            <<0x5C, 0x75, 0x7B, tail:bytes>> ->
              read_template_span(bytes, tail, pos + 3, start, line)
            _ -> {
              let next = pos + 1 + char_width_at(bytes, pos + 1)
              read_template_span(
                bytes,
                drop_bytes(bytes, next),
                next,
                start,
                line,
              )
            }
          }
      }
    <<0x24, 0x7B, _:bytes>> -> {
      let len = pos + 2 - start
      tokn(TemplateHead, byte_slice(bytes, start, len), start, len, line)
    }
    <<0x60, _:bytes>> -> {
      let len = pos - start + 1
      tokn(TemplateLiteral, byte_slice(bytes, start, len), start, len, line)
    }
    <<_, tail:bytes>> -> read_template_span(bytes, tail, pos + 1, start, line)
    _ -> unterminated_quote_token(bytes, start, line)
  }
}

// illegal span is at least one char so lexing progresses
fn read_number_lenient(
  bytes: BitArray,
  start: Int,
  line: Int,
  rest: BitArray,
) -> Token {
  case read_number(bytes, start, line, rest) {
    Ok(token) -> token
    Error(err) -> {
      let end = int.max(lex_error_pos(err), start + 1)
      let len = end - start
      tokn(Illegal, byte_slice(bytes, start, len), start, len, line)
    }
  }
}

fn read_number(
  bytes: BitArray,
  start: Int,
  line: Int,
  rest: BitArray,
) -> Result(Token, LexError) {
  case rest {
    <<0x30, b, tail:bytes>> if b == 0x78 || b == 0x58 ->
      read_radix_number(
        bytes,
        tail,
        start,
        line,
        is_hex_byte,
        ExpectedHexDigits,
      )
    <<0x30, b, tail:bytes>> if b == 0x6F || b == 0x4F ->
      read_radix_number(
        bytes,
        tail,
        start,
        line,
        is_octal_byte,
        ExpectedOctalDigits,
      )
    <<0x30, b, tail:bytes>> if b == 0x62 || b == 0x42 ->
      read_radix_number(
        bytes,
        tail,
        start,
        line,
        is_binary_byte,
        ExpectedBinaryDigits,
      )
    <<0x2E, tail:bytes>> -> {
      use #(pos2, tail2) <- result.try(skip_digits(tail, start + 1))
      read_exponent(bytes, tail2, start, pos2, line)
    }
    _ -> read_decimal_number(bytes, start, line, rest)
  }
}

fn is_hex_byte(b: Int) -> Bool {
  { b >= 0x30 && b <= 0x39 }
  || { b >= 0x61 && b <= 0x66 }
  || { b >= 0x41 && b <= 0x46 }
}

fn is_octal_byte(b: Int) -> Bool {
  b >= 0x30 && b <= 0x37
}

fn is_binary_byte(b: Int) -> Bool {
  b == 0x30 || b == 0x31
}

fn read_decimal_number(
  bytes: BitArray,
  start: Int,
  line: Int,
  rest: BitArray,
) -> Result(Token, LexError) {
  use #(pos, tail) <- result.try(skip_digits(rest, start))
  let has_leading_zero = case rest {
    <<0x30, _:bytes>> -> pos - start > 1
    _ -> False
  }
  use token <- result.map(read_decimal_body(
    bytes,
    tail,
    start,
    pos,
    has_leading_zero,
    line,
  ))
  Token(..token, annex_b_legacy: has_leading_zero)
}

fn read_decimal_body(
  bytes: BitArray,
  rest: BitArray,
  start: Int,
  pos: Int,
  has_leading_zero: Bool,
  line: Int,
) -> Result(Token, LexError) {
  use Nil <- result.try(
    case has_leading_zero && has_separator(bytes, start, pos) {
      True -> Error(LeadingNumericSeparator(start))
      False -> Ok(Nil)
    },
  )
  let is_legacy_octal =
    has_leading_zero && !has_non_octal(bytes, start + 1, pos)
  case rest {
    <<0x2E, tail:bytes>> ->
      case is_legacy_octal, tail {
        True, _ -> Ok(number_token(bytes, rest, start, pos, line))
        False, <<0x2E, _:bytes>> ->
          Ok(number_token(bytes, tail, start, pos + 1, line))
        False, _ -> {
          use #(pos2, tail2) <- result.try(skip_digits(tail, pos + 1))
          read_exponent(bytes, tail2, start, pos2, line)
        }
      }
    // `01e2` is `01` then `e`, matching v8/quickjs
    <<e, _:bytes>> if e == 0x65 || e == 0x45 ->
      case is_legacy_octal {
        True -> Ok(number_token(bytes, rest, start, pos, line))
        False -> read_exponent(bytes, rest, start, pos, line)
      }
    <<0x6E, tail:bytes>> -> {
      // only `0n` may have a leading zero
      use <- bool.guard(has_leading_zero, Error(InvalidBigIntLiteral(start)))
      Ok(number_token(bytes, tail, start, pos + 1, line))
    }
    _ -> Ok(number_token(bytes, rest, start, pos, line))
  }
}

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

fn read_exponent(
  bytes: BitArray,
  rest: BitArray,
  start: Int,
  pos: Int,
  line: Int,
) -> Result(Token, LexError) {
  case rest {
    <<e, tail:bytes>> if e == 0x65 || e == 0x45 -> {
      let #(pos2, tail2) = case tail {
        <<sign, after:bytes>> if sign == 0x2B || sign == 0x2D -> #(
          pos + 2,
          after,
        )
        _ -> #(pos + 1, tail)
      }
      use #(pos3, tail3) <- result.try(skip_digits(tail2, pos2))
      case pos3 == pos2 {
        True -> Error(ExpectedExponentDigits(pos))
        False -> Ok(number_token(bytes, tail3, start, pos3, line))
      }
    }
    _ -> Ok(number_token(bytes, rest, start, pos, line))
  }
}

fn read_radix_number(
  bytes: BitArray,
  rest: BitArray,
  start: Int,
  line: Int,
  is_digit: fn(Int) -> Bool,
  err: fn(Int) -> LexError,
) -> Result(Token, LexError) {
  let pos = start + 2
  use #(end, tail) <- result.try(skip_digits_loop(
    rest,
    pos,
    pos,
    False,
    is_digit,
  ))
  case end == pos {
    True -> Error(err(start))
    False ->
      case tail {
        <<0x6E, tail2:bytes>> ->
          Ok(number_token(bytes, tail2, start, end + 1, line))
        _ -> Ok(number_token(bytes, tail, start, end, line))
      }
  }
}

fn number_token(
  bytes: BitArray,
  rest: BitArray,
  start: Int,
  end: Int,
  line: Int,
) -> Token {
  let id_follows = case rest {
    <<>> -> False
    <<b, _:bytes>>
      if { b >= 0x61 && b <= 0x7A }
      || { b >= 0x41 && b <= 0x5A }
      || b == 0x5F
      || b == 0x24
    -> True
    <<0x5C, _:bytes>> -> result.is_ok(read_identifier_escape(bytes, end, True))
    <<b, _:bytes>> if b < 0x80 -> False
    _ -> is_identifier_start(char_at(bytes, end))
  }
  case id_follows {
    True -> {
      let id_end = case skip_ident_inner(rest, 0) {
        IdEnd(n) -> end + n
        IdEscape(n) -> end + n
      }
      let id_end = int.max(id_end, end + 1)
      let len = id_end - start
      tokn(Illegal, byte_slice(bytes, start, len), start, len, line)
    }
    False -> {
      let len = end - start
      tokn(Number, byte_slice(bytes, start, len), start, len, line)
    }
  }
}

fn skip_digits(rest: BitArray, pos: Int) -> Result(#(Int, BitArray), LexError) {
  skip_digits_loop(rest, pos, pos, False, is_decimal_byte)
}

fn is_decimal_byte(b: Int) -> Bool {
  b >= 0x30 && b <= 0x39
}

fn skip_digits_loop(
  rest: BitArray,
  pos: Int,
  start: Int,
  prev_was_sep: Bool,
  is_digit: fn(Int) -> Bool,
) -> Result(#(Int, BitArray), LexError) {
  case rest {
    <<0x5F, tail:bytes>> ->
      case prev_was_sep {
        True -> Error(ConsecutiveNumericSeparator(pos))
        False ->
          case pos == start {
            True -> Error(LeadingNumericSeparator(pos))
            False -> skip_digits_loop(tail, pos + 1, start, True, is_digit)
          }
      }
    <<b, tail:bytes>> ->
      case is_digit(b) {
        True -> skip_digits_loop(tail, pos + 1, start, False, is_digit)
        False -> digits_end(rest, pos, prev_was_sep)
      }
    _ -> digits_end(rest, pos, prev_was_sep)
  }
}

fn digits_end(
  rest: BitArray,
  pos: Int,
  prev_was_sep: Bool,
) -> Result(#(Int, BitArray), LexError) {
  case prev_was_sep {
    True -> Error(TrailingNumericSeparator(pos - 1))
    False -> Ok(#(pos, rest))
  }
}

fn identifier_token(
  start: Int,
  end: Int,
  name: String,
  had_escape: Bool,
  line: Int,
) -> Token {
  let kind = case had_escape {
    True -> Identifier
    False -> keyword_or_identifier(name)
  }
  Token(
    kind:,
    value: name,
    pos: start,
    line:,
    raw_len: end - start,
    had_escape:,
    annex_b_legacy: False,
  )
}

fn read_ascii_identifier(
  bytes: BitArray,
  start: Int,
  line: Int,
  rest: BitArray,
) -> Token {
  case skip_ident_inner(rest, 0) {
    IdEnd(n) ->
      identifier_token(
        start,
        start + n,
        byte_slice(bytes, start, n),
        False,
        line,
      )
    IdEscape(_) -> {
      let tail = scan_identifier_tail(bytes, start + 1)
      plain_head_token(bytes, start, start + 1, tail, line)
    }
  }
}

fn read_identifier(
  bytes: BitArray,
  start: Int,
  line: Int,
) -> Result(Token, LexError) {
  case char_at(bytes, start) {
    "\\" -> {
      use #(first_end, head) <- result.try(read_identifier_escape(
        bytes,
        start,
        True,
      ))
      let tail = scan_identifier_tail(bytes, first_end)
      Ok(escaped_head_token(bytes, start, first_end, head, tail, line))
    }
    "#" -> {
      case char_at(bytes, start + 1) {
        "\\" ->
          case read_identifier_escape(bytes, start + 1, True) {
            Ok(#(first_end, head)) -> {
              let tail = scan_identifier_tail(bytes, first_end)
              Ok(escaped_head_token(
                bytes,
                start,
                first_end,
                "#" <> head,
                tail,
                line,
              ))
            }
            Error(_not_an_identifier) ->
              Ok(bad_escape_token(bytes, start, start + 1, line))
          }
        ch2 -> {
          case is_identifier_start(ch2) {
            True -> {
              let first_end = start + 1 + char_width_at(bytes, start + 1)
              let tail = scan_identifier_tail(bytes, first_end)
              Ok(plain_head_token(bytes, start, first_end, tail, line))
            }
            False -> Ok(tokn(Illegal, "#", start, 1, line))
          }
        }
      }
    }
    _ -> {
      let first_end = start + char_width_at(bytes, start)
      let tail = scan_identifier_tail(bytes, first_end)
      Ok(plain_head_token(bytes, start, first_end, tail, line))
    }
  }
}

fn escaped_head_token(
  bytes: BitArray,
  start: Int,
  first_end: Int,
  head: String,
  tail: IdentTail,
  line: Int,
) -> Token {
  case tail {
    NoEscapes(end:) ->
      identifier_token(
        start,
        end,
        head <> byte_slice(bytes, first_end, end - first_end),
        True,
        line,
      )
    WithEscapes(end:, text:) ->
      identifier_token(start, end, head <> text, True, line)
  }
}

fn plain_head_token(
  bytes: BitArray,
  start: Int,
  first_end: Int,
  tail: IdentTail,
  line: Int,
) -> Token {
  case tail {
    NoEscapes(end:) ->
      identifier_token(
        start,
        end,
        byte_slice(bytes, start, end - start),
        False,
        line,
      )
    WithEscapes(end:, text:) ->
      identifier_token(
        start,
        end,
        byte_slice(bytes, start, first_end - start) <> text,
        True,
        line,
      )
  }
}

fn read_identifier_escape(
  bytes: BitArray,
  pos: Int,
  is_start: Bool,
) -> Result(#(Int, String), LexError) {
  let bad = Error(InvalidUnicodeEscapeSequence(pos))
  use <- bool.guard(char_at(bytes, pos + 1) != "u", bad)
  case scan_unicode_escape(bytes, pos + 2) {
    None -> bad
    Some(#(cp, end)) ->
      case cp > 0x10FFFF {
        True -> bad
        False ->
          decoded_identifier_char(cp, is_start, end)
          |> result.replace_error(InvalidUnicodeEscapeSequence(pos))
      }
  }
}

fn decoded_identifier_char(
  cp: Int,
  is_start: Bool,
  end_pos: Int,
) -> Result(#(Int, String), Nil) {
  use <- bool.guard(!validate_identifier_codepoint(cp, is_start), Error(Nil))
  use codepoint <- result.map(string.utf_codepoint(cp))
  #(end_pos, string.from_utf_codepoints([codepoint]))
}

pub fn validate_identifier_codepoint(cp: Int, is_start: Bool) -> Bool {
  // reject nul and surrogates
  case cp {
    0 -> False
    _ ->
      case utf16.is_surrogate(cp) {
        True -> False
        False ->
          case is_start {
            True ->
              { cp == 0x24 }
              || { cp == 0x5F }
              || digits.is_ascii_alpha_code(cp)
              || { cp > 127 && is_unicode_id_start(cp) }
            False -> is_cp_id_continue(cp)
          }
      }
  }
}

type IdentTail {
  NoEscapes(end: Int)
  WithEscapes(end: Int, text: String)
}

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
    // zwnj u+200c, zwj u+200d
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
  digits.is_ascii_alnum_code(n)
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

fn char_at(bytes: BitArray, pos: Int) -> String {
  let width = char_width_at(bytes, pos)
  case width {
    0 -> ""
    _ -> byte_slice(bytes, pos, width)
  }
}

// offsets are always char boundaries, so no utf-8 revalidation
@external(erlang, "arc_bytes_ffi", "unsafe_slice")
fn byte_slice(bytes: BitArray, start: Int, len: Int) -> String

@external(erlang, "arc_bytes_ffi", "drop_start")
fn drop_bytes(bytes: BitArray, pos: Int) -> BitArray
