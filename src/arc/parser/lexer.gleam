import arc/internal/digits
import arc/parser/source_bytes.{byte_at}
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

pub type SourceKind {
  ScriptSource
  ModuleSource
}

// rest is always drop_bytes(bytes, pos), kept to avoid reslicing
pub type Scanner {
  Scanner(
    bytes: BitArray,
    pos: Int,
    line: Int,
    source_kind: SourceKind,
    rest: BitArray,
  )
}

pub fn scanner_at(
  bytes: BitArray,
  pos: Int,
  line: Int,
  source_kind: SourceKind,
) -> Scanner {
  Scanner(bytes:, pos:, line:, source_kind:, rest: drop_bytes(bytes, pos))
}

pub fn scan_next(s: Scanner) -> #(Token, Scanner) {
  let Scanner(bytes:, pos:, line:, source_kind:, rest:) = s
  case skip_ws(rest, pos, source_kind) {
    WsEnd(consumed, ws_newlines, rest) -> {
      let new_pos = pos + consumed
      let token_line = line + ws_newlines
      case read_token(bytes, new_pos, token_line, rest) {
        Error(err) ->
          hard_error_token(err, bytes, new_pos, token_line, source_kind)
        Ok(token) -> {
          let raw_len = token.raw_len
          let end_line = case token.kind {
            KString | TemplateLiteral | TemplateHead ->
              token_line + count_newlines_in(bytes, new_pos, raw_len)
            _ -> token_line
          }
          #(
            token,
            Scanner(
              bytes:,
              pos: new_pos + raw_len,
              line: end_line,
              source_kind:,
              rest: drop_bytes(rest, raw_len),
            ),
          )
        }
      }
    }
    WsBlockUnterminated(at) ->
      hard_error_token(
        UnterminatedBlockComment(pos + at),
        bytes,
        pos,
        line,
        source_kind,
      )
    WsHtmlInModule(at) ->
      hard_error_token(
        HtmlCommentInModule(pos + at),
        bytes,
        pos,
        line,
        source_kind,
      )
  }
}

// token line must be the error's line or asi breaks
fn hard_error_token(
  err: LexError,
  bytes: BitArray,
  from: Int,
  line: Int,
  source_kind: SourceKind,
) -> #(Token, Scanner) {
  let error_pos = lex_error_pos(err)
  let error_line = line + count_newlines_in(bytes, from, error_pos - from)
  #(
    Token(LexFailure(err), "", error_pos, error_line, 0, False, False),
    Scanner(
      bytes:,
      pos: bit_array.byte_size(bytes),
      line: error_line,
      source_kind:,
      rest: <<>>,
    ),
  )
}

fn count_newlines_in(bytes: BitArray, from: Int, len: Int) -> Int {
  count_newlines(bit_array.from_string(byte_slice(bytes, from, len)), 0)
}

fn count_newlines(bytes: BitArray, count: Int) -> Int {
  case bytes {
    <<13, 10, rest:bytes>> -> count_newlines(rest, count + 1)
    <<10, rest:bytes>> -> count_newlines(rest, count + 1)
    <<13, rest:bytes>> -> count_newlines(rest, count + 1)
    // u+2028 / u+2029
    <<0xE2, 0x80, 0xA8, rest:bytes>> -> count_newlines(rest, count + 1)
    <<0xE2, 0x80, 0xA9, rest:bytes>> -> count_newlines(rest, count + 1)
    <<_, rest:bytes>> -> count_newlines(rest, count)
    _ -> count
  }
}

type WsScan {
  WsEnd(consumed: Int, newlines: Int, rest: BitArray)
  WsBlockUnterminated(at: Int)
  WsHtmlInModule(at: Int)
}

fn skip_ws(rest: BitArray, pos: Int, source_kind: SourceKind) -> WsScan {
  case pos, rest {
    // #! hashbang
    0, <<0x23, 0x21, tail:bytes>> -> skip_line_comment(tail, 2, 0, source_kind)
    _, _ -> skip_ws_loop(rest, 0, 0, pos == 0, source_kind)
  }
}

fn skip_ws_loop(
  rest: BitArray,
  consumed: Int,
  newlines: Int,
  at_line_start: Bool,
  source_kind: SourceKind,
) -> WsScan {
  case rest {
    <<0x20, tail:bytes>>
    | <<0x09, tail:bytes>>
    | <<0x0B, tail:bytes>>
    | <<0x0C, tail:bytes>> ->
      skip_ws_loop(tail, consumed + 1, newlines, at_line_start, source_kind)
    <<0x0D, 0x0A, tail:bytes>> ->
      skip_ws_loop(tail, consumed + 2, newlines + 1, True, source_kind)
    <<0x0A, tail:bytes>> | <<0x0D, tail:bytes>> ->
      skip_ws_loop(tail, consumed + 1, newlines + 1, True, source_kind)
    <<0x2F, 0x2F, tail:bytes>> ->
      skip_line_comment(tail, consumed + 2, newlines, source_kind)
    <<0x2F, 0x2A, tail:bytes>> ->
      skip_block_comment(
        tail,
        consumed + 2,
        newlines,
        at_line_start,
        source_kind,
      )
    // <!-- html comment, script only
    <<0x3C, 0x21, 0x2D, 0x2D, tail:bytes>> ->
      case source_kind {
        ModuleSource -> WsHtmlInModule(consumed)
        ScriptSource ->
          skip_line_comment(tail, consumed + 4, newlines, source_kind)
      }
    // --> html comment, line start, script only
    <<0x2D, 0x2D, 0x3E, tail:bytes>> if at_line_start ->
      case source_kind {
        ModuleSource -> WsHtmlInModule(consumed)
        ScriptSource ->
          skip_line_comment(tail, consumed + 3, newlines, source_kind)
      }
    // nbsp u+00a0
    <<0xC2, 0xA0, tail:bytes>> ->
      skip_ws_loop(tail, consumed + 2, newlines, at_line_start, source_kind)
    // u+2028, u+2029 line separators
    <<0xE2, 0x80, 0xA8, tail:bytes>> | <<0xE2, 0x80, 0xA9, tail:bytes>> ->
      skip_ws_loop(tail, consumed + 3, newlines + 1, True, source_kind)
    // bom u+feff, u+1680, u+2000..u+200a, u+202f, u+205f, u+3000
    <<0xEF, 0xBB, 0xBF, tail:bytes>>
    | <<0xE1, 0x9A, 0x80, tail:bytes>>
    | <<0xE2, 0x80, 0xAF, tail:bytes>>
    | <<0xE2, 0x81, 0x9F, tail:bytes>>
    | <<0xE3, 0x80, 0x80, tail:bytes>> ->
      skip_ws_loop(tail, consumed + 3, newlines, at_line_start, source_kind)
    <<0xE2, 0x80, b, tail:bytes>> if b >= 0x80 && b <= 0x8A ->
      skip_ws_loop(tail, consumed + 3, newlines, at_line_start, source_kind)
    other -> WsEnd(consumed, newlines, other)
  }
}

fn skip_line_comment(
  rest: BitArray,
  consumed: Int,
  newlines: Int,
  source_kind: SourceKind,
) -> WsScan {
  let comment_len = line_comment_length(rest, 0)
  let after = drop_bytes(rest, comment_len)
  skip_ws_loop(after, consumed + comment_len, newlines, False, source_kind)
}

fn line_comment_length(rest: BitArray, len: Int) -> Int {
  case rest {
    <<0x0D, _:bytes>> | <<0x0A, _:bytes>> -> len
    <<0xE2, 0x80, 0xA8, _:bytes>> | <<0xE2, 0x80, 0xA9, _:bytes>> -> len
    <<b, tail:bytes>> if b < 0x80 -> line_comment_length(tail, len + 1)
    <<b, _, tail:bytes>> if b >= 0xC0 && b < 0xE0 ->
      line_comment_length(tail, len + 2)
    <<b, _, _, tail:bytes>> if b >= 0xE0 && b < 0xF0 ->
      line_comment_length(tail, len + 3)
    <<b, _, _, _, tail:bytes>> if b >= 0xF0 && b < 0xF8 ->
      line_comment_length(tail, len + 4)
    <<_, tail:bytes>> -> line_comment_length(tail, len + 1)
    _ -> len
  }
}

fn skip_block_comment(
  rest: BitArray,
  consumed: Int,
  newlines: Int,
  at_line_start: Bool,
  source_kind: SourceKind,
) -> WsScan {
  case rest {
    <<0x2A, 0x2F, tail:bytes>> ->
      skip_ws_loop(tail, consumed + 2, newlines, at_line_start, source_kind)
    <<0x0D, 0x0A, tail:bytes>> ->
      skip_block_comment(tail, consumed + 2, newlines + 1, True, source_kind)
    <<0x0A, tail:bytes>> | <<0x0D, tail:bytes>> ->
      skip_block_comment(tail, consumed + 1, newlines + 1, True, source_kind)
    <<0xE2, 0x80, 0xA8, tail:bytes>> | <<0xE2, 0x80, 0xA9, tail:bytes>> ->
      skip_block_comment(tail, consumed + 3, newlines + 1, True, source_kind)
    <<b, tail:bytes>> if b < 0x80 ->
      skip_block_comment(
        tail,
        consumed + 1,
        newlines,
        at_line_start,
        source_kind,
      )
    <<b, _, tail:bytes>> if b >= 0xC0 && b < 0xE0 ->
      skip_block_comment(
        tail,
        consumed + 2,
        newlines,
        at_line_start,
        source_kind,
      )
    <<b, _, _, tail:bytes>> if b >= 0xE0 && b < 0xF0 ->
      skip_block_comment(
        tail,
        consumed + 3,
        newlines,
        at_line_start,
        source_kind,
      )
    <<b, _, _, _, tail:bytes>> if b >= 0xF0 && b < 0xF8 ->
      skip_block_comment(
        tail,
        consumed + 4,
        newlines,
        at_line_start,
        source_kind,
      )
    <<_, tail:bytes>> ->
      skip_block_comment(
        tail,
        consumed + 1,
        newlines,
        at_line_start,
        source_kind,
      )
    _ -> WsBlockUnterminated(consumed)
  }
}

fn plain_token(
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
    <<>> -> Ok(plain_token(Eof, "", pos, 0, line))

    <<0x28, _:bytes>> -> Ok(plain_token(LeftParen, "(", pos, 1, line))
    <<0x29, _:bytes>> -> Ok(plain_token(RightParen, ")", pos, 1, line))
    <<0x7B, _:bytes>> -> Ok(plain_token(LeftBrace, "{", pos, 1, line))
    <<0x7D, _:bytes>> -> Ok(plain_token(RightBrace, "}", pos, 1, line))
    <<0x5B, _:bytes>> -> Ok(plain_token(LeftBracket, "[", pos, 1, line))
    <<0x5D, _:bytes>> -> Ok(plain_token(RightBracket, "]", pos, 1, line))
    <<0x3B, _:bytes>> -> Ok(plain_token(Semicolon, ";", pos, 1, line))
    <<0x2C, _:bytes>> -> Ok(plain_token(Comma, ",", pos, 1, line))
    <<0x7E, _:bytes>> -> Ok(plain_token(Tilde, "~", pos, 1, line))
    <<0x3A, _:bytes>> -> Ok(plain_token(Colon, ":", pos, 1, line))

    <<0x2E, 0x2E, 0x2E, _:bytes>> ->
      Ok(plain_token(DotDotDot, "...", pos, 3, line))
    <<0x2E, d, _:bytes>> if d >= 0x30 && d <= 0x39 ->
      Ok(read_number_lenient(bytes, pos, line, rest))
    <<0x2E, _:bytes>> -> Ok(plain_token(Dot, ".", pos, 1, line))

    <<0x2B, 0x2B, _:bytes>> -> Ok(plain_token(PlusPlus, "++", pos, 2, line))
    <<0x2B, 0x3D, _:bytes>> -> Ok(plain_token(PlusEqual, "+=", pos, 2, line))
    <<0x2B, _:bytes>> -> Ok(plain_token(Plus, "+", pos, 1, line))

    <<0x2D, 0x2D, _:bytes>> -> Ok(plain_token(MinusMinus, "--", pos, 2, line))
    <<0x2D, 0x3D, _:bytes>> -> Ok(plain_token(MinusEqual, "-=", pos, 2, line))
    <<0x2D, _:bytes>> -> Ok(plain_token(Minus, "-", pos, 1, line))

    <<0x2A, 0x2A, 0x3D, _:bytes>> ->
      Ok(plain_token(StarStarEqual, "**=", pos, 3, line))
    <<0x2A, 0x2A, _:bytes>> -> Ok(plain_token(StarStar, "**", pos, 2, line))
    <<0x2A, 0x3D, _:bytes>> -> Ok(plain_token(StarEqual, "*=", pos, 2, line))
    <<0x2A, _:bytes>> -> Ok(plain_token(Star, "*", pos, 1, line))

    <<0x2F, 0x3D, _:bytes>> -> Ok(plain_token(SlashEqual, "/=", pos, 2, line))
    <<0x2F, _:bytes>> -> Ok(plain_token(Slash, "/", pos, 1, line))

    <<0x25, 0x3D, _:bytes>> -> Ok(plain_token(PercentEqual, "%=", pos, 2, line))
    <<0x25, _:bytes>> -> Ok(plain_token(Percent, "%", pos, 1, line))

    <<0x3D, 0x3D, 0x3D, _:bytes>> ->
      Ok(plain_token(EqualEqualEqual, "===", pos, 3, line))
    <<0x3D, 0x3D, _:bytes>> -> Ok(plain_token(EqualEqual, "==", pos, 2, line))
    <<0x3D, 0x3E, _:bytes>> -> Ok(plain_token(Arrow, "=>", pos, 2, line))
    <<0x3D, _:bytes>> -> Ok(plain_token(Equal, "=", pos, 1, line))

    <<0x21, 0x3D, 0x3D, _:bytes>> ->
      Ok(plain_token(BangEqualEqual, "!==", pos, 3, line))
    <<0x21, 0x3D, _:bytes>> -> Ok(plain_token(BangEqual, "!=", pos, 2, line))
    <<0x21, _:bytes>> -> Ok(plain_token(Bang, "!", pos, 1, line))

    <<0x3C, 0x3D, _:bytes>> ->
      Ok(plain_token(LessThanEqual, "<=", pos, 2, line))
    <<0x3C, 0x3C, 0x3D, _:bytes>> ->
      Ok(plain_token(LessThanLessThanEqual, "<<=", pos, 3, line))
    <<0x3C, 0x3C, _:bytes>> ->
      Ok(plain_token(LessThanLessThan, "<<", pos, 2, line))
    <<0x3C, _:bytes>> -> Ok(plain_token(LessThan, "<", pos, 1, line))

    <<0x3E, 0x3D, _:bytes>> ->
      Ok(plain_token(GreaterThanEqual, ">=", pos, 2, line))
    <<0x3E, 0x3E, 0x3D, _:bytes>> ->
      Ok(plain_token(GreaterThanGreaterThanEqual, ">>=", pos, 3, line))
    <<0x3E, 0x3E, 0x3E, 0x3D, _:bytes>> ->
      Ok(plain_token(
        GreaterThanGreaterThanGreaterThanEqual,
        ">>>=",
        pos,
        4,
        line,
      ))
    <<0x3E, 0x3E, 0x3E, _:bytes>> ->
      Ok(plain_token(GreaterThanGreaterThanGreaterThan, ">>>", pos, 3, line))
    <<0x3E, 0x3E, _:bytes>> ->
      Ok(plain_token(GreaterThanGreaterThan, ">>", pos, 2, line))
    <<0x3E, _:bytes>> -> Ok(plain_token(GreaterThan, ">", pos, 1, line))

    <<0x26, 0x26, 0x3D, _:bytes>> ->
      Ok(plain_token(AmpersandAmpersandEqual, "&&=", pos, 3, line))
    <<0x26, 0x26, _:bytes>> ->
      Ok(plain_token(AmpersandAmpersand, "&&", pos, 2, line))
    <<0x26, 0x3D, _:bytes>> ->
      Ok(plain_token(AmpersandEqual, "&=", pos, 2, line))
    <<0x26, _:bytes>> -> Ok(plain_token(Ampersand, "&", pos, 1, line))

    <<0x7C, 0x7C, 0x3D, _:bytes>> ->
      Ok(plain_token(PipePipeEqual, "||=", pos, 3, line))
    <<0x7C, 0x7C, _:bytes>> -> Ok(plain_token(PipePipe, "||", pos, 2, line))
    <<0x7C, 0x3D, _:bytes>> -> Ok(plain_token(PipeEqual, "|=", pos, 2, line))
    <<0x7C, _:bytes>> -> Ok(plain_token(Pipe, "|", pos, 1, line))

    <<0x5E, 0x3D, _:bytes>> -> Ok(plain_token(CaretEqual, "^=", pos, 2, line))
    <<0x5E, _:bytes>> -> Ok(plain_token(Caret, "^", pos, 1, line))

    // `?.5` is `?` then `.5`
    <<0x3F, 0x3F, 0x3D, _:bytes>> ->
      Ok(plain_token(QuestionQuestionEqual, "??=", pos, 3, line))
    <<0x3F, 0x3F, _:bytes>> ->
      Ok(plain_token(QuestionQuestion, "??", pos, 2, line))
    <<0x3F, 0x2E, d, _:bytes>> if d >= 0x30 && d <= 0x39 ->
      Ok(plain_token(Question, "?", pos, 1, line))
    <<0x3F, 0x2E, _:bytes>> -> Ok(plain_token(QuestionDot, "?.", pos, 2, line))
    <<0x3F, _:bytes>> -> Ok(plain_token(Question, "?", pos, 1, line))

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
      Ok(read_escaped_identifier(bytes, pos, pos, line))
    <<0x5C, _:bytes>> -> Ok(plain_token(Illegal, "\\", pos, 1, line))
    // #private name
    <<0x23, 0x5C, _:bytes>> ->
      Ok(read_escaped_identifier(bytes, pos, pos + 1, line))
    <<0x23, _:bytes>> ->
      case is_identifier_start(char_at(bytes, pos + 1)) {
        True -> {
          let first_end = pos + 1 + char_width_at(bytes, pos + 1)
          Ok(finish_identifier_token(bytes, pos, first_end, None, line))
        }
        False -> Ok(plain_token(Illegal, "#", pos, 1, line))
      }
    _ -> {
      let ch = char_at(bytes, pos)
      let width = char_width_at(bytes, pos)
      case is_identifier_start(ch) {
        True -> Ok(finish_identifier_token(bytes, pos, pos + width, None, line))
        False -> Ok(plain_token(Illegal, ch, pos, width, line))
      }
    }
  }
}

type Escape {
  Escape(byte_len: Int, annex_b_legacy: Bool)
}

// byte_len includes the backslash
fn validate_escape(
  bytes: BitArray,
  backslash_pos: Int,
  in_template in_template: Bool,
) -> Result(Escape, LexError) {
  let pos = backslash_pos + 1
  case drop_bytes(bytes, pos) {
    // \0 before a digit and \1-\9 are annex b legacy
    <<0x30, d, _:bytes>> if d >= 0x30 && d <= 0x39 ->
      legacy_digit_escape(backslash_pos, in_template)
    <<d, _:bytes>> if d >= 0x31 && d <= 0x39 ->
      legacy_digit_escape(backslash_pos, in_template)
    <<0x30, _:bytes>> -> Ok(Escape(2, False))

    <<0x78, h1, h2, _:bytes>> ->
      case is_hex_byte(h1) && is_hex_byte(h2) {
        True -> Ok(Escape(4, False))
        False -> Error(InvalidHexEscapeSequence(backslash_pos))
      }
    <<0x78, _:bytes>> -> Error(InvalidHexEscapeSequence(backslash_pos))

    <<0x75, _:bytes>> ->
      case valid_unicode_escape(bytes, backslash_pos) {
        Some(escape) -> Ok(Escape(escape.end - backslash_pos, False))
        None -> Error(InvalidUnicodeEscapeSequence(backslash_pos))
      }

    // line continuations, crlf spans 3 bytes
    <<0x0D, 0x0A, _:bytes>> -> Ok(Escape(3, False))
    <<0x0D, _:bytes>> | <<0x0A, _:bytes>> -> Ok(Escape(2, False))

    _ -> Ok(Escape(1 + char_width_at(bytes, pos), False))
  }
}

fn legacy_digit_escape(
  backslash_pos: Int,
  in_template: Bool,
) -> Result(Escape, LexError) {
  case in_template {
    True -> Error(InvalidEscapeSequence(backslash_pos))
    False -> Ok(Escape(2, True))
  }
}

type UnicodeEscape {
  UnicodeEscape(code: Int, end: Int)
}

// \uXXXX or \u{X..} syntax only, callers know the u is there
fn scan_unicode_escape(
  bytes: BitArray,
  backslash_pos: Int,
) -> Option(UnicodeEscape) {
  let after_u = backslash_pos + 2
  case drop_bytes(bytes, after_u) {
    <<0x7B, tail:bytes>> -> {
      let #(digit_count, code) = hex_run(tail, 0, 0)
      case drop_bytes(tail, digit_count) {
        <<0x7D, _:bytes>> if digit_count > 0 ->
          Some(UnicodeEscape(code:, end: after_u + 1 + digit_count + 1))
        _ -> None
      }
    }
    <<a, b, c, d, _:bytes>> ->
      case hex4_value(a, b, c, d) {
        Some(code) -> Some(UnicodeEscape(code:, end: after_u + 4))
        None -> None
      }
    _ -> None
  }
}

fn valid_unicode_escape(
  bytes: BitArray,
  backslash_pos: Int,
) -> Option(UnicodeEscape) {
  case scan_unicode_escape(bytes, backslash_pos) {
    Some(escape) if escape.code <= 0x10FFFF -> Some(escape)
    Some(_) | None -> None
  }
}

fn hex_run(rest: BitArray, len: Int, value: Int) -> #(Int, Int) {
  case rest {
    <<b, tail:bytes>> ->
      case digits.hex_value_code(b) {
        Some(digit) -> hex_run(tail, len + 1, value * 16 + digit)
        None -> #(len, value)
      }
    _ -> #(len, value)
  }
}

fn hex4_value(a: Int, b: Int, c: Int, d: Int) -> Option(Int) {
  case
    digits.hex_value_code(a),
    digits.hex_value_code(b),
    digits.hex_value_code(c),
    digits.hex_value_code(d)
  {
    Some(a), Some(b), Some(c), Some(d) ->
      Some({ { a * 16 + b } * 16 + c } * 16 + d)
    _, _, _, _ -> None
  }
}

// spans the escape even when out of range so lexing progresses past it
fn bad_escape_token(
  bytes: BitArray,
  start: Int,
  backslash_pos: Int,
  line: Int,
) -> Token {
  let escape_end = case scan_unicode_escape(bytes, backslash_pos) {
    Some(escape) -> escape.end
    None -> backslash_pos + 2
  }
  let len = escape_end - start
  Token(
    ..plain_token(Illegal, byte_slice(bytes, start, len), start, len, line),
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
  case scan_to_closing_quote(rest, 0, quote) {
    StrQuote(consumed) -> {
      let raw_len = pos + consumed - start + 1
      let content = byte_slice(bytes, start + 1, raw_len - 2)
      Ok(
        Token(
          ..plain_token(KString, content, start, raw_len, line),
          annex_b_legacy:,
        ),
      )
    }
    StrEscape(consumed) -> {
      let backslash_pos = pos + consumed
      use escape <- result.try(validate_escape(
        bytes,
        backslash_pos,
        in_template: False,
      ))
      let next = backslash_pos + escape.byte_len
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
    StrUnterminated -> Ok(unterminated_quote_token(bytes, start, line))
  }
}

fn unterminated_quote_token(bytes: BitArray, start: Int, line: Int) -> Token {
  plain_token(Illegal, byte_slice(bytes, start, 1), start, 1, line)
}

type StrScan {
  StrQuote(consumed: Int)
  StrEscape(consumed: Int)
  StrUnterminated
}

fn scan_to_closing_quote(rest: BitArray, consumed: Int, quote: Int) -> StrScan {
  case rest {
    <<b, _:bytes>> if b == quote -> StrQuote(consumed)
    <<0x5C>> -> StrUnterminated
    <<0x5C, _:bytes>> -> StrEscape(consumed)
    <<0x0A, _:bytes>> | <<0x0D, _:bytes>> -> StrUnterminated
    <<b, tail:bytes>> if b < 0x80 ->
      scan_to_closing_quote(tail, consumed + 1, quote)
    <<b, _, tail:bytes>> if b >= 0xC0 && b < 0xE0 ->
      scan_to_closing_quote(tail, consumed + 2, quote)
    <<b, _, _, tail:bytes>> if b >= 0xE0 && b < 0xF0 ->
      scan_to_closing_quote(tail, consumed + 3, quote)
    <<b, _, _, _, tail:bytes>> if b >= 0xF0 && b < 0xF8 ->
      scan_to_closing_quote(tail, consumed + 4, quote)
    <<_, tail:bytes>> -> scan_to_closing_quote(tail, consumed + 1, quote)
    _ -> StrUnterminated
  }
}

pub fn scan_template_continuation(
  bytes: BitArray,
  rbrace_pos: Int,
  line: Int,
  source_kind: SourceKind,
) -> #(Token, Scanner) {
  let pos = rbrace_pos + 1
  let token =
    read_template_span(bytes, drop_bytes(bytes, pos), pos, rbrace_pos, line)
  let end_pos = token.pos + token.raw_len
  let end_line = line + count_newlines_in(bytes, token.pos, token.raw_len)
  #(token, scanner_at(bytes, end_pos, end_line, source_kind))
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
    <<0x5C, _:bytes>> -> {
      // invalid escapes are legal in tagged templates, parser decides
      let next = case validate_escape(bytes, pos, in_template: True), rest {
        Ok(escape), _ -> pos + escape.byte_len
        Error(_invalid_escape), <<0x5C, 0x75, 0x7B, _:bytes>> -> pos + 3
        Error(_invalid_escape), _ -> pos + 1 + char_width_at(bytes, pos + 1)
      }
      read_template_span(bytes, drop_bytes(bytes, next), next, start, line)
    }
    <<0x24, 0x7B, _:bytes>> -> {
      let len = pos + 2 - start
      plain_token(TemplateHead, byte_slice(bytes, start, len), start, len, line)
    }
    <<0x60, _:bytes>> -> {
      let len = pos - start + 1
      plain_token(
        TemplateLiteral,
        byte_slice(bytes, start, len),
        start,
        len,
        line,
      )
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
      plain_token(Illegal, byte_slice(bytes, start, len), start, len, line)
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
    <<0x30, letter, tail:bytes>> ->
      case radix_letter(letter) {
        Some(prefix) -> read_radix_number(bytes, tail, start, line, prefix)
        None -> read_decimal_number(bytes, start, line, rest)
      }
    <<0x2E, tail:bytes>> -> {
      use #(pos2, tail2) <- result.try(skip_digits(tail, start + 1))
      read_exponent(bytes, tail2, start, pos2, line)
    }
    _ -> read_decimal_number(bytes, start, line, rest)
  }
}

type RadixPrefix {
  RadixPrefix(is_digit: fn(Int) -> Bool, missing_digits: fn(Int) -> LexError)
}

fn radix_letter(letter: Int) -> Option(RadixPrefix) {
  case letter {
    // x X
    0x78 | 0x58 -> Some(RadixPrefix(is_hex_byte, ExpectedHexDigits))
    // o O
    0x6F | 0x4F -> Some(RadixPrefix(is_octal_byte, ExpectedOctalDigits))
    // b B
    0x62 | 0x42 -> Some(RadixPrefix(is_binary_byte, ExpectedBinaryDigits))
    _ -> None
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

fn is_separator_byte(b: Int) -> Bool {
  b == 0x5F
}

fn is_non_octal_digit_byte(b: Int) -> Bool {
  b == 0x38 || b == 0x39
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
  use <- bool.guard(
    has_leading_zero && digits_contain(bytes, start, pos, is_separator_byte),
    Error(LeadingNumericSeparator(start)),
  )
  let is_legacy_octal =
    has_leading_zero
    && !digits_contain(bytes, start + 1, pos, is_non_octal_digit_byte)
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
    // `0n`, no other leading zero may be a bigint
    <<0x6E, tail:bytes>> ->
      case has_leading_zero {
        True -> Error(InvalidBigIntLiteral(start))
        False -> Ok(number_token(bytes, tail, start, pos + 1, line))
      }
    // legacy octal takes no exponent, `01e2` is `01` then `e2` as in v8
    _ ->
      case is_legacy_octal {
        True -> Ok(number_token(bytes, rest, start, pos, line))
        False -> read_exponent(bytes, rest, start, pos, line)
      }
  }
}

fn digits_contain(
  bytes: BitArray,
  from: Int,
  to: Int,
  wanted: fn(Int) -> Bool,
) -> Bool {
  from < to
  && {
    wanted(byte_at(bytes, from)) || digits_contain(bytes, from + 1, to, wanted)
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
      let #(digits_start, tail2) = case tail {
        <<sign, after:bytes>> if sign == 0x2B || sign == 0x2D -> #(
          pos + 2,
          after,
        )
        _ -> #(pos + 1, tail)
      }
      use #(end, tail3) <- result.try(skip_digits(tail2, digits_start))
      case end == digits_start {
        True -> Error(ExpectedExponentDigits(pos))
        False -> Ok(number_token(bytes, tail3, start, end, line))
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
  prefix: RadixPrefix,
) -> Result(Token, LexError) {
  let pos = start + 2
  use #(end, tail) <- result.try(skip_digits_loop(
    rest,
    pos,
    pos,
    False,
    prefix.is_digit,
  ))
  case end == pos, tail {
    True, _ -> Error(prefix.missing_digits(start))
    False, <<0x6E, after_n:bytes>> ->
      Ok(number_token(bytes, after_n, start, end + 1, line))
    False, _ -> Ok(number_token(bytes, tail, start, end, line))
  }
}

fn number_token(
  bytes: BitArray,
  rest: BitArray,
  start: Int,
  end: Int,
  line: Int,
) -> Token {
  let identifier_follows = case rest {
    <<>> -> False
    <<b, _:bytes>>
      if { b >= 0x61 && b <= 0x7A }
      || { b >= 0x41 && b <= 0x5A }
      || b == 0x5F
      || b == 0x24
    -> True
    <<0x5C, _:bytes>> ->
      option.is_some(decode_identifier_escape(bytes, end, is_start: True))
    <<b, _:bytes>> if b < 0x80 -> False
    _ -> is_identifier_start(char_at(bytes, end))
  }
  case identifier_follows {
    True -> {
      let identifier_end =
        int.max(end + skip_identifier_bytes(rest, 0).consumed, end + 1)
      let len = identifier_end - start
      plain_token(Illegal, byte_slice(bytes, start, len), start, len, line)
    }
    False -> {
      let len = end - start
      plain_token(Number, byte_slice(bytes, start, len), start, len, line)
    }
  }
}

fn skip_digits(rest: BitArray, pos: Int) -> Result(#(Int, BitArray), LexError) {
  skip_digits_loop(rest, pos, pos, False, digits.is_decimal_code)
}

fn skip_digits_loop(
  rest: BitArray,
  pos: Int,
  start: Int,
  prev_was_separator: Bool,
  is_digit: fn(Int) -> Bool,
) -> Result(#(Int, BitArray), LexError) {
  case rest {
    <<0x5F, tail:bytes>> ->
      case prev_was_separator, pos == start {
        True, _ -> Error(ConsecutiveNumericSeparator(pos))
        False, True -> Error(LeadingNumericSeparator(pos))
        False, False -> skip_digits_loop(tail, pos + 1, start, True, is_digit)
      }
    <<b, tail:bytes>> ->
      case is_digit(b) {
        True -> skip_digits_loop(tail, pos + 1, start, False, is_digit)
        False -> digits_end(rest, pos, prev_was_separator)
      }
    _ -> digits_end(rest, pos, prev_was_separator)
  }
}

fn digits_end(
  rest: BitArray,
  pos: Int,
  prev_was_separator: Bool,
) -> Result(#(Int, BitArray), LexError) {
  case prev_was_separator {
    True -> Error(TrailingNumericSeparator(pos - 1))
    False -> Ok(#(pos, rest))
  }
}

fn identifier_token(
  start: Int,
  end: Int,
  name: String,
  line: Int,
  had_escape had_escape: Bool,
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
  case skip_identifier_bytes(rest, 0) {
    RunEnd(consumed) ->
      identifier_token(
        start,
        start + consumed,
        byte_slice(bytes, start, consumed),
        line,
        had_escape: False,
      )
    RunEscape(consumed) ->
      finish_identifier_token(bytes, start, start + consumed, None, line)
  }
}

// start..backslash_pos is a plain prefix such as #
fn read_escaped_identifier(
  bytes: BitArray,
  start: Int,
  backslash_pos: Int,
  line: Int,
) -> Token {
  case decode_identifier_escape(bytes, backslash_pos, is_start: True) {
    Some(#(first_end, char)) -> {
      let head = byte_slice(bytes, start, backslash_pos - start) <> char
      finish_identifier_token(bytes, start, first_end, Some(head), line)
    }
    None -> bad_escape_token(bytes, start, backslash_pos, line)
  }
}

// decoded_head replaces start..first_end when the head had an escape
fn finish_identifier_token(
  bytes: BitArray,
  start: Int,
  first_end: Int,
  decoded_head: Option(String),
  line: Int,
) -> Token {
  let IdentTail(end:, decoded: decoded_tail) =
    scan_identifier_tail(bytes, first_end)
  let had_escape = option.is_some(decoded_head) || option.is_some(decoded_tail)
  let name = case had_escape {
    False -> byte_slice(bytes, start, end - start)
    True ->
      option.unwrap(decoded_head, byte_slice(bytes, start, first_end - start))
      <> option.unwrap(
        decoded_tail,
        byte_slice(bytes, first_end, end - first_end),
      )
  }
  identifier_token(start, end, name, line, had_escape:)
}

fn decode_identifier_escape(
  bytes: BitArray,
  backslash_pos: Int,
  is_start is_start: Bool,
) -> Option(#(Int, String)) {
  case drop_bytes(bytes, backslash_pos + 1) {
    <<0x75, _:bytes>> -> {
      use escape <- option.then(valid_unicode_escape(bytes, backslash_pos))
      use char <- option.map(identifier_char(escape.code, is_start))
      #(escape.end, char)
    }
    _ -> None
  }
}

fn identifier_char(code: Int, is_start: Bool) -> Option(String) {
  use <- bool.guard(!validate_identifier_codepoint(code, is_start), None)
  string.utf_codepoint(code)
  |> option.from_result
  |> option.map(fn(codepoint) { string.from_utf_codepoints([codepoint]) })
}

pub fn validate_identifier_codepoint(cp: Int, is_start: Bool) -> Bool {
  case is_start {
    True -> is_identifier_start_code(cp)
    False -> is_identifier_part_code(cp)
  }
}

type IdentTail {
  IdentTail(end: Int, decoded: Option(String))
}

fn scan_identifier_tail(bytes: BitArray, pos: Int) -> IdentTail {
  scan_identifier_tail_loop(bytes, pos, None)
}

// decoded is the tail text so far once an escape has been seen
fn scan_identifier_tail_loop(
  bytes: BitArray,
  pos: Int,
  decoded: Option(String),
) -> IdentTail {
  case skip_identifier_bytes(drop_bytes(bytes, pos), 0) {
    RunEnd(consumed) -> identifier_tail(bytes, pos, consumed, decoded)
    RunEscape(consumed) -> {
      let backslash_pos = pos + consumed
      case decode_identifier_escape(bytes, backslash_pos, is_start: False) {
        Some(#(next_pos, char)) -> {
          let text =
            option.unwrap(decoded, "")
            <> byte_slice(bytes, pos, consumed)
            <> char
          scan_identifier_tail_loop(bytes, next_pos, Some(text))
        }
        None -> identifier_tail(bytes, pos, consumed, decoded)
      }
    }
  }
}

fn identifier_tail(
  bytes: BitArray,
  pos: Int,
  plain_len: Int,
  decoded: Option(String),
) -> IdentTail {
  let decoded =
    option.map(decoded, fn(text) { text <> byte_slice(bytes, pos, plain_len) })
  IdentTail(end: pos + plain_len, decoded:)
}

type IdentRun {
  RunEnd(consumed: Int)
  RunEscape(consumed: Int)
}

fn skip_identifier_bytes(rest: BitArray, consumed: Int) -> IdentRun {
  case rest {
    <<b, tail:bytes>>
      if { b >= 0x61 && b <= 0x7A }
      || { b >= 0x41 && b <= 0x5A }
      || { b >= 0x30 && b <= 0x39 }
      || b == 0x5F
      || b == 0x24
    -> skip_identifier_bytes(tail, consumed + 1)
    <<0x5C, _:bytes>> -> RunEscape(consumed)
    // zwnj u+200c, zwj u+200d
    <<0xE2, 0x80, 0x8C, tail:bytes>> | <<0xE2, 0x80, 0x8D, tail:bytes>> ->
      skip_identifier_bytes(tail, consumed + 3)
    <<b, _:bytes>> if b >= 0x80 -> skip_identifier_unicode(rest, consumed)
    _ -> RunEnd(consumed)
  }
}

fn skip_identifier_unicode(rest: BitArray, consumed: Int) -> IdentRun {
  case rest {
    <<c:utf8_codepoint, tail:bytes>> -> {
      let code = string.utf_codepoint_to_int(c)
      case is_unicode_id_continue(code) {
        True -> skip_identifier_bytes(tail, consumed + codepoint_width(code))
        False -> RunEnd(consumed)
      }
    }
    _ -> RunEnd(consumed)
  }
}

fn codepoint_width(code: Int) -> Int {
  case code {
    _ if code >= 0x10000 -> 4
    _ if code >= 0x800 -> 3
    _ if code >= 0x80 -> 2
    _ -> 1
  }
}

fn is_identifier_start(ch: String) -> Bool {
  case string.to_utf_codepoints(ch) {
    [c] -> is_identifier_start_code(string.utf_codepoint_to_int(c))
    _ -> False
  }
}

// ascii is decided here, the website build stubs the unicode tables
fn is_identifier_start_code(code: Int) -> Bool {
  digits.is_ascii_alpha_code(code)
  || code == 0x5F
  || code == 0x24
  || { code > 127 && is_unicode_id_start(code) }
}

fn is_identifier_part_code(code: Int) -> Bool {
  digits.is_ascii_alnum_code(code)
  || code == 0x5F
  || code == 0x24
  || code == 0x200C
  || code == 0x200D
  || { code > 127 && is_unicode_id_continue(code) }
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
