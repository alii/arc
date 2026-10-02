// json syntax error text, worded like v8

import gleam/bit_array
import gleam/int
import gleam/list
import gleam/string

pub type JsonParseError {
  // at is a byte offset into the text
  ParseFailure(kind: JsonFailure, at: Int)
  RawJsonEmpty
  RawJsonSurroundingWhitespace
  RawJsonNotPrimitive
}

pub type JsonFailure {
  UnexpectedEnd
  UnexpectedCharacter
  UnterminatedString
  BadControlCharacter
  BadEscapedCharacter
  BadUnicodeEscape
  NoNumberAfterMinus
  UnterminatedFraction
  ExponentMissingNumber
  ExpectedPropertyNameOrBrace
  ExpectedPropertyName
  ExpectedColon
  ExpectedCommaOrBrace
  ExpectedCommaOrBracket
  TrailingContent
  InvalidUtf8
}

// shorter texts are quoted whole
const context_units = 10

pub fn message(error: JsonParseError, source: BitArray) -> String {
  case error {
    RawJsonEmpty -> "JSON.rawJSON text must not be empty"
    RawJsonSurroundingWhitespace ->
      "JSON.rawJSON text must not start or end with whitespace"
    RawJsonNotPrimitive -> "JSON.rawJSON text must not be an object or an array"
    ParseFailure(kind: UnexpectedEnd, ..) -> "Unexpected end of JSON input"
    ParseFailure(kind: InvalidUtf8, ..) -> "Invalid UTF-8 in JSON input"
    ParseFailure(kind: UnexpectedCharacter, at:) -> {
      let before = codepoints(slice_bytes(source, 0, at))
      let after =
        codepoints(slice_bytes(source, at, bit_array.byte_size(source) - at))
      case after {
        [] -> "Unexpected end of JSON input"
        [cp, ..] ->
          case string.utf_codepoint_to_int(cp) {
            0x22 -> "Unexpected string in JSON" <> location(before)
            c if c == 0x2D || c >= 0x30 && c <= 0x39 ->
              "Unexpected number in JSON" <> location(before)
            _ -> unexpected_token(cp, before, after)
          }
      }
    }
    ParseFailure(kind:, at:) ->
      phrase(kind) <> location(codepoints(slice_bytes(source, 0, at)))
  }
}

fn phrase(kind: JsonFailure) -> String {
  case kind {
    UnterminatedString -> "Unterminated string in JSON"
    BadControlCharacter -> "Bad control character in string literal in JSON"
    BadEscapedCharacter -> "Bad escaped character in JSON"
    BadUnicodeEscape -> "Bad Unicode escape in JSON"
    NoNumberAfterMinus -> "No number after minus sign in JSON"
    UnterminatedFraction -> "Unterminated fractional number in JSON"
    ExponentMissingNumber -> "Exponent part is missing a number in JSON"
    ExpectedPropertyNameOrBrace -> "Expected property name or '}' in JSON"
    ExpectedPropertyName -> "Expected double-quoted property name in JSON"
    ExpectedColon -> "Expected ':' after property name in JSON"
    ExpectedCommaOrBrace -> "Expected ',' or '}' after property value in JSON"
    ExpectedCommaOrBracket -> "Expected ',' or ']' after array element in JSON"
    TrailingContent -> "Unexpected non-whitespace character after JSON"
    UnexpectedEnd | UnexpectedCharacter | InvalidUtf8 ->
      "Unexpected token in JSON"
  }
}

fn slice_bytes(source: BitArray, from: Int, length: Int) -> BitArray {
  case bit_array.slice(source, from, length) {
    Ok(part) -> part
    Error(Nil) -> <<>>
  }
}

fn codepoints(bytes: BitArray) -> List(UtfCodepoint) {
  case bit_array.to_string(bytes) {
    Ok(s) -> string.to_utf_codepoints(s)
    Error(Nil) -> []
  }
}

// positions count utf-16 units, as string indexes do
fn units(cps: List(UtfCodepoint)) -> Int {
  list.fold(cps, 0, fn(n, cp) { n + unit_width(cp) })
}

fn unit_width(cp: UtfCodepoint) -> Int {
  case string.utf_codepoint_to_int(cp) > 0xFFFF {
    True -> 2
    False -> 1
  }
}

fn location(before: List(UtfCodepoint)) -> String {
  let #(line, column) = line_and_column(before, 1, 1)
  " at position "
  <> int.to_string(units(before))
  <> " (line "
  <> int.to_string(line)
  <> " column "
  <> int.to_string(column)
  <> ")"
}

fn line_and_column(
  rest: List(UtfCodepoint),
  line: Int,
  column: Int,
) -> #(Int, Int) {
  case rest {
    [] -> #(line, column)
    [cp, ..rest] ->
      case string.utf_codepoint_to_int(cp), rest {
        0x0D, [next, ..after] ->
          case string.utf_codepoint_to_int(next) {
            0x0A -> line_and_column(after, line + 1, 1)
            _ -> line_and_column(rest, line + 1, 1)
          }
        0x0D, [] | 0x0A, _ -> line_and_column(rest, line + 1, 1)
        _, _ -> line_and_column(rest, line, column + unit_width(cp))
      }
  }
}

fn unexpected_token(
  token: UtfCodepoint,
  before: List(UtfCodepoint),
  after: List(UtfCodepoint),
) -> String {
  let whole = string.from_utf_codepoints(list.append(before, after))
  let special = case whole {
    "[object Object]" | "undefined" | "Infinity" | "NaN" -> True
    _ -> False
  }
  let at = units(before)
  let length = at + units(after)
  let head =
    "Unexpected token '" <> string.from_utf_codepoints([token]) <> "', "
  let tail = " is not valid JSON"
  let near_before =
    list.reverse(take_units(list.reverse(before), context_units))
  let near_after = take_units(after, context_units)
  case special, length > context_units * 2 {
    True, _ -> quote(whole) <> tail
    False, False -> head <> quote(whole) <> tail
    False, True ->
      case at < context_units, at < length - context_units {
        True, _ ->
          head
          <> quote(string.from_utf_codepoints(list.append(before, near_after)))
          <> "..."
          <> tail
        False, True ->
          head
          <> "..."
          <> quote(
            string.from_utf_codepoints(list.append(near_before, near_after)),
          )
          <> "..."
          <> tail
        False, False ->
          head
          <> "..."
          <> quote(string.from_utf_codepoints(list.append(near_before, after)))
          <> tail
      }
  }
}

fn quote(s: String) -> String {
  "\"" <> s <> "\""
}

fn take_units(cps: List(UtfCodepoint), n: Int) -> List(UtfCodepoint) {
  case n > 0, cps {
    True, [cp, ..rest] -> [cp, ..take_units(rest, n - unit_width(cp))]
    _, _ -> []
  }
}
