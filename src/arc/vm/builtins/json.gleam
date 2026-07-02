import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/builtins/object as object_builtins
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/coerce
import arc/vm/ops/object as objops
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type JsonNativeFn, type Property, type Ref, Finite, JsBool,
  JsNull, JsNumber, JsObject, JsString, JsUndefined, JsonNative,
  JsonParse, JsonStringify, NaN, NegInfinity, ObjectSlot, OrdinaryObject,
}
import gleam/bit_array
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gleam/string_tree.{type StringTree}

// ============================================================================
// Init — set up the JSON global object
// ============================================================================

/// Set up the JSON global object.
/// JSON is NOT a constructor — it's a plain object with static methods
/// (like Math), per ES2024 S25.5.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), Ref) {
  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("parse", JsonNative(JsonParse), 2),
      #("stringify", JsonNative(JsonStringify), 3),
    ])

  common.init_namespace(h, object_proto, "JSON", methods)
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for JSON native functions.
pub fn dispatch(
  native: JsonNativeFn,
  args: List(JsValue),
  _this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    JsonParse -> json_parse(args, state)
    JsonStringify -> json_stringify(args, state)
  }
}

// ============================================================================
// JSON.parse(text)
// ============================================================================

/// ES2024 S25.5.1 JSON.parse ( text [ , reviver ] )
///
/// DEVIATION from the spec: the optional `reviver` argument (steps 7-10,
/// InternalizeJSONProperty) is accepted but ignored — implementing it is a
/// separate feature.
///
/// Steps:
///   1. Let jsonString be ? ToString(text).
///   2. Parse jsonString as a JSON text as specified in ECMA-404.
///   3. If the parse fails, throw a SyntaxError exception.
///   4. Return the parsed value.
fn json_parse(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: ToString(text)
  let to_string_result = case args {
    [JsString(s), ..] -> Ok(#(s, state))
    [other, ..] -> coerce.js_to_string(state, other)
    [] -> Ok(#("undefined", state))
  }

  use json_str, state <- state.try_op(to_string_result)
  // Step 2: Parse as JSON text — walk the UTF-8 bytes directly.
  // On BEAM a String already is a binary, so this conversion is free, and
  // byte pattern matching avoids per-character grapheme clustering (which
  // goes through unicode_util:gc and allocates a cons cell + sub-binary
  // per character).
  let bytes = bit_array.from_string(json_str)
  case parse_value(bytes) {
    Ok(#(val, rest)) -> {
      // After parsing, skip trailing whitespace and ensure nothing else
      let rest = skip_whitespace(rest)
      case rest {
        <<>> -> {
          // Successfully parsed — materialize the value on the heap
          let #(heap, js_val) = materialize(state.heap, state.builtins, val)
          #(State(..state, heap:), Ok(js_val))
        }
        _ ->
          state.syntax_error(
            state,
            "Unexpected non-whitespace character after JSON",
          )
      }
    }
    // Step 2: If parse fails, throw SyntaxError
    Error(e) -> state.syntax_error(state, json_error_message(e))
  }
}

/// Intermediate parsed JSON value — not yet materialized onto the JS heap.
/// We parse into this first, then walk it to create JsValues/heap objects.
type JsonValue {
  JsonNull
  JsonBool(Bool)
  JsonNumber(value.JsNum)
  JsonString(String)
  JsonArray(List(JsonValue))
  JsonObject(List(#(String, JsonValue)))
}

/// Everything that can go wrong while scanning JSON text.
///
/// The scanner never builds user-facing strings: each `parse_*` function
/// returns one of these, and `json_error_message` renders it at the single
/// point where the SyntaxError is thrown (`json_parse`).
type JsonParseError {
  /// Input ran out where a value / more input was required.
  UnexpectedEnd
  /// A byte that cannot start a JSON value (rendered as the offending char).
  UnexpectedToken(found: String)
  /// A string ran to the end of input without a closing quote.
  UnterminatedString
  /// Input ended in the middle of a backslash escape.
  UnterminatedEscape
  /// An array ran to the end of input without a closing bracket.
  UnterminatedArray
  /// An object ran to the end of input without a closing brace.
  UnterminatedObject
  /// A raw control character (U+0000–U+001F) inside a string.
  ControlCharInString
  /// `\x` where `x` is not one of the eight legal escape characters.
  InvalidEscape(escape: String)
  /// `\u` not followed by exactly four hex digits.
  InvalidUnicodeEscape
  /// A `\uXXXX` (or surrogate pair) that names an invalid codepoint.
  InvalidCodepoint
  /// A number-shaped span that does not match the ECMA-404 number grammar.
  InvalidNumber(raw: String)
  /// A specific punctuator/production was required and something else found.
  Expected(what: String, in_: String)
  /// The input bytes are not valid UTF-8.
  InvalidUtf8
}

/// Render a `JsonParseError` as the SyntaxError message users see.
/// This is the ONLY place parse errors become strings.
fn json_error_message(e: JsonParseError) -> String {
  case e {
    UnexpectedEnd -> "Unexpected end of JSON input"
    UnexpectedToken(found:) -> "Unexpected token '" <> found <> "' in JSON"
    UnterminatedString -> "Unterminated string in JSON"
    UnterminatedEscape -> "Unterminated string escape in JSON"
    UnterminatedArray -> "Unterminated array in JSON"
    UnterminatedObject -> "Unterminated object in JSON"
    ControlCharInString -> "Unexpected control character in JSON string"
    InvalidEscape(escape:) ->
      "Invalid escape character '\\" <> escape <> "' in JSON"
    InvalidUnicodeEscape -> "Invalid Unicode escape in JSON"
    InvalidCodepoint -> "Invalid Unicode codepoint in JSON string"
    InvalidNumber(raw:) -> "Invalid number '" <> raw <> "' in JSON"
    Expected(what:, in_:) -> "Expected " <> what <> " in " <> in_
    InvalidUtf8 -> "Invalid UTF-8 in JSON input"
  }
}

/// Skip whitespace bytes (space, tab, newline, carriage return).
fn skip_whitespace(bytes: BitArray) -> BitArray {
  case bytes {
    <<0x20, rest:bytes>>
    | <<0x09, rest:bytes>>
    | <<0x0A, rest:bytes>>
    | <<0x0D, rest:bytes>> -> skip_whitespace(rest)
    _ -> bytes
  }
}

/// Parse a JSON value from UTF-8 bytes.
fn parse_value(
  bytes: BitArray,
) -> Result(#(JsonValue, BitArray), JsonParseError) {
  let bytes = skip_whitespace(bytes)
  case bytes {
    <<>> -> Error(UnexpectedEnd)
    // "null"
    <<0x6E, 0x75, 0x6C, 0x6C, rest:bytes>> -> Ok(#(JsonNull, rest))
    // "true"
    <<0x74, 0x72, 0x75, 0x65, rest:bytes>> -> Ok(#(JsonBool(True), rest))
    // "false"
    <<0x66, 0x61, 0x6C, 0x73, 0x65, rest:bytes>> -> Ok(#(JsonBool(False), rest))
    // '"'
    <<0x22, rest:bytes>> -> {
      use #(s, rest) <- result.map(parse_string(rest))
      #(JsonString(s), rest)
    }
    // '['
    <<0x5B, rest:bytes>> -> parse_array(rest, [])
    // '{'
    <<0x7B, rest:bytes>> -> parse_object(rest, [])
    // '-' or '0'..'9'
    <<b, _:bytes>> if b == 0x2D || b >= 0x30 && b <= 0x39 -> parse_number(bytes)
    <<c:utf8_codepoint, _:bytes>> ->
      Error(UnexpectedToken(found: string.from_utf_codepoints([c])))
    _ -> Error(InvalidUtf8)
  }
}

/// Result of scanning a string body for its closing quote or first escape.
type StringScan {
  /// Closing quote found after `content_len` bytes; `after` is past the quote.
  FoundQuote(content_len: Int, after: BitArray)
  /// Backslash found after `prefix_len` bytes; `after` is past the backslash.
  FoundEscape(prefix_len: Int, after: BitArray)
  /// ECMA-404: an unescaped control character (U+0000–U+001F) — invalid JSON.
  FoundControlChar
  /// Input ended before the closing quote.
  NoClosingQuote
}

/// Scan forward for the closing quote or the first backslash.
/// Safe on UTF-8: continuation/lead bytes of multi-byte characters are all
/// >= 0x80, so they can never be mistaken for '"' (0x22) or '\\' (0x5C) —
/// or misread as a control character (< 0x20).
fn scan_string(bytes: BitArray, n: Int) -> StringScan {
  case bytes {
    <<0x22, rest:bytes>> -> FoundQuote(n, rest)
    <<0x5C, rest:bytes>> -> FoundEscape(n, rest)
    <<c, rest:bytes>> ->
      case c < 0x20 {
        // ECMA-404: control characters must be written as escape sequences.
        True -> FoundControlChar
        False -> scan_string(rest, n + 1)
      }
    _ -> NoClosingQuote
  }
}

/// Parse the content of a JSON string (after the opening quote).
/// Fast path: no escapes — the result is an O(1) zero-copy sub-binary of
/// the input. Only when a backslash is seen do we build an escape buffer.
fn parse_string(
  bytes: BitArray,
) -> Result(#(String, BitArray), JsonParseError) {
  case scan_string(bytes, 0) {
    FoundQuote(n, after) -> {
      use s <- result.map(take_string(bytes, n))
      #(s, after)
    }
    FoundEscape(n, after) -> {
      use chunk <- result.try(take_string(bytes, n))
      parse_escape(after, string_tree.from_string(chunk))
    }
    FoundControlChar -> Error(ControlCharInString)
    NoClosingQuote -> Error(UnterminatedString)
  }
}

/// Slow path: accumulate decoded string content, appending unescaped spans
/// as whole chunks (one append per span, not per character).
fn parse_string_content(
  bytes: BitArray,
  acc: StringTree,
) -> Result(#(String, BitArray), JsonParseError) {
  case scan_string(bytes, 0) {
    FoundQuote(n, after) -> {
      use chunk <- result.map(take_string(bytes, n))
      #(string_tree.to_string(string_tree.append(acc, chunk)), after)
    }
    FoundEscape(n, after) -> {
      use chunk <- result.try(take_string(bytes, n))
      parse_escape(after, string_tree.append(acc, chunk))
    }
    FoundControlChar -> Error(ControlCharInString)
    NoClosingQuote -> Error(UnterminatedString)
  }
}

/// Decode one escape sequence; `bytes` starts just after the backslash.
fn parse_escape(
  bytes: BitArray,
  acc: StringTree,
) -> Result(#(String, BitArray), JsonParseError) {
  case bytes {
    <<>> -> Error(UnterminatedEscape)
    <<0x22, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "\""))
    <<0x5C, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "\\"))
    <<0x2F, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "/"))
    <<0x62, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "\u{0008}"))
    <<0x66, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "\u{000C}"))
    <<0x6E, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "\n"))
    <<0x72, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "\r"))
    <<0x74, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "\t"))
    <<0x75, rest:bytes>> -> {
      use #(decoded, rest) <- result.try(decode_unicode_escape(rest))
      parse_string_content(rest, string_tree.append(acc, decoded))
    }
    <<c:utf8_codepoint, _:bytes>> ->
      Error(InvalidEscape(escape: string.from_utf_codepoints([c])))
    _ -> Error(UnterminatedEscape)
  }
}

/// Parse a 4-digit hex escape (\uXXXX), returning the integer codepoint value.
///
/// Each digit is read structurally rather than via `int.base_parse`, which
/// accepts a leading sign — `"\u+061"` must be a SyntaxError, not U+0061.
fn parse_unicode_escape(
  bytes: BitArray,
) -> Result(#(Int, BitArray), JsonParseError) {
  case bytes {
    <<a, b, c, d, rest:bytes>> ->
      case hex_digit(a), hex_digit(b), hex_digit(c), hex_digit(d) {
        Some(h1), Some(h2), Some(h3), Some(h4) ->
          Ok(#(h1 * 4096 + h2 * 256 + h3 * 16 + h4, rest))
        _, _, _, _ -> Error(InvalidUnicodeEscape)
      }
    _ -> Error(InvalidUnicodeEscape)
  }
}

/// The value of an ASCII hex-digit byte ('0'-'9', 'A'-'F', 'a'-'f'), or None.
fn hex_digit(byte: Int) -> Option(Int) {
  case byte {
    b if b >= 0x30 && b <= 0x39 -> Some(b - 0x30)
    b if b >= 0x41 && b <= 0x46 -> Some(b - 0x41 + 10)
    b if b >= 0x61 && b <= 0x66 -> Some(b - 0x61 + 10)
    _ -> None
  }
}

/// Decode a \uXXXX escape (possibly a surrogate pair) into a UTF-8 string.
/// Returns #(decoded_string, remaining_bytes). Lone surrogates become U+FFFD.
fn decode_unicode_escape(
  bytes: BitArray,
) -> Result(#(String, BitArray), JsonParseError) {
  use #(cp, rest) <- result.try(parse_unicode_escape(bytes))
  case cp {
    // High surrogate — look for a trailing \uXXXX low surrogate
    high if high >= 0xD800 && high <= 0xDBFF ->
      case parse_low_surrogate(rest) {
        Some(#(low, rest)) -> {
          let combined = { high - 0xD800 } * 0x400 + { low - 0xDC00 } + 0x10000
          codepoint_to_string(combined) |> result.map(fn(s) { #(s, rest) })
        }
        // Lone/unpaired high surrogate → U+FFFD replacement char
        None -> Ok(#("\u{FFFD}", rest))
      }
    // Lone LOW surrogate (never valid on its own) → U+FFFD as well, rather
    // than a parse error: JSON.parse('"\\udc62"') must succeed.
    low if low >= 0xDC00 && low <= 0xDFFF -> Ok(#("\u{FFFD}", rest))
    _ -> codepoint_to_string(cp) |> result.map(fn(s) { #(s, rest) })
  }
}

/// Try to consume "\uXXXX" where XXXX is a low surrogate (DC00-DFFF).
/// Returns None if not present or not a valid low surrogate (caller rewinds).
fn parse_low_surrogate(bytes: BitArray) -> Option(#(Int, BitArray)) {
  case bytes {
    // "\\u"
    <<0x5C, 0x75, rest:bytes>> ->
      case parse_unicode_escape(rest) {
        Ok(#(low, rest)) if low >= 0xDC00 && low <= 0xDFFF -> Some(#(low, rest))
        _ -> None
      }
    _ -> None
  }
}

/// Convert an integer codepoint into a single-char string, or error.
fn codepoint_to_string(codepoint: Int) -> Result(String, JsonParseError) {
  string.utf_codepoint(codepoint)
  |> result.map(fn(cp) { string.from_utf_codepoints([cp]) })
  |> result.replace_error(InvalidCodepoint)
}

/// The three spans of a scanned JSON number, as byte lengths:
///   int_len  — optional '-' plus the integer digits (always > 0)
///   frac_len — '.' plus the fraction digits, or 0 if absent
///   exp_len  — 'e'/'E', optional sign, exponent digits, or 0 if absent
/// The total number span is `int_len + frac_len + exp_len` bytes.
type NumberSpan {
  NumberSpan(int_len: Int, frac_len: Int, exp_len: Int)
}

/// Parse a JSON number: scan the leading bytes against the ECMA-404 number
/// grammar, slice the span out as a sub-binary (O(1)), and convert it.
fn parse_number(
  bytes: BitArray,
) -> Result(#(JsonValue, BitArray), JsonParseError) {
  case scan_number(bytes) {
    Ok(span) -> {
      let len = span.int_len + span.frac_len + span.exp_len
      use num_str <- result.map(take_string(bytes, len))
      #(JsonNumber(number_span_to_num(num_str, span)), drop_bytes(bytes, len))
    }
    // Report the whole number-looking span (e.g. "01", "1e", "-"), not just
    // the prefix that scanned cleanly.
    Error(Nil) -> {
      use raw <- result.try(take_string(bytes, count_number_bytes(bytes, 0)))
      Error(InvalidNumber(raw:))
    }
  }
}

/// Byte-scan the ECMA-404 number grammar
///
///   -? (0 | [1-9][0-9]*) ('.' [0-9]+)? ([eE] [+-]? [0-9]+)?
///
/// This rejects leading zeros ("01"), a bare or trailing '.' (".5", "1."),
/// a bare or dangling exponent ("1e", "1e+"), and signs anywhere but the
/// two allowed positions — none of which the Erlang number parsers reject.
fn scan_number(bytes: BitArray) -> Result(NumberSpan, Nil) {
  let #(bytes, sign_len) = case bytes {
    <<0x2D, rest:bytes>> -> #(rest, 1)
    _ -> #(bytes, 0)
  }
  use #(bytes, digits) <- result.try(scan_integer_digits(bytes))
  use #(bytes, frac_len) <- result.try(scan_fraction(bytes))
  use exp_len <- result.map(scan_exponent(bytes))
  NumberSpan(int_len: sign_len + digits, frac_len:, exp_len:)
}

/// `0 | [1-9][0-9]*` — a leading '0' must not be followed by another digit.
fn scan_integer_digits(bytes: BitArray) -> Result(#(BitArray, Int), Nil) {
  case bytes {
    <<0x30, next, _:bytes>> if next >= 0x30 && next <= 0x39 -> Error(Nil)
    <<0x30, rest:bytes>> -> Ok(#(rest, 1))
    <<b, rest:bytes>> if b >= 0x31 && b <= 0x39 -> {
      let #(rest, n) = scan_digits(rest, 0)
      Ok(#(rest, 1 + n))
    }
    _ -> Error(Nil)
  }
}

/// `('.' [0-9]+)?` — a '.' must be followed by at least one digit.
fn scan_fraction(bytes: BitArray) -> Result(#(BitArray, Int), Nil) {
  case bytes {
    <<0x2E, rest:bytes>> ->
      case scan_digits(rest, 0) {
        #(_, 0) -> Error(Nil)
        #(rest, n) -> Ok(#(rest, 1 + n))
      }
    _ -> Ok(#(bytes, 0))
  }
}

/// `([eE] [+-]? [0-9]+)?` — an 'e' must be followed by (optionally signed)
/// digits.
fn scan_exponent(bytes: BitArray) -> Result(Int, Nil) {
  case bytes {
    <<e, rest:bytes>> if e == 0x65 || e == 0x45 -> {
      let #(rest, sign_len) = case rest {
        <<s, tail:bytes>> if s == 0x2B || s == 0x2D -> #(tail, 1)
        _ -> #(rest, 0)
      }
      case scan_digits(rest, 0) {
        #(_, 0) -> Error(Nil)
        #(_, n) -> Ok(1 + sign_len + n)
      }
    }
    _ -> Ok(0)
  }
}

/// Count a leading run of ASCII digits.
fn scan_digits(bytes: BitArray, n: Int) -> #(BitArray, Int) {
  case bytes {
    <<b, rest:bytes>> if b >= 0x30 && b <= 0x39 -> scan_digits(rest, n + 1)
    _ -> #(bytes, n)
  }
}

/// Count leading bytes that look like they belong to a number ('-' '+' '.'
/// 'e' 'E' or a digit). Only used to report the full malformed span in
/// `InvalidNumber` — the real grammar check is `scan_number`.
fn count_number_bytes(bytes: BitArray, n: Int) -> Int {
  case bytes {
    <<b, rest:bytes>>
      if b == 0x2D
      || b == 0x2B
      || b == 0x2E
      || b == 0x65
      || b == 0x45
      || b >= 0x30
      && b <= 0x39
    -> count_number_bytes(rest, n + 1)
    _ -> n
  }
}

/// Convert a grammar-checked number span into a JsNum.
///
/// Erlang's float parser demands a fraction before any exponent, and its
/// integer parser folds "-0" into 0, so the route depends on which parts the
/// scanner saw:
///   - integer only  → `int.parse` then `value.num_from_int` (a bare
///     `int.to_float` on an arbitrary-precision int, e.g. a 400-digit JSON
///     integer, raises an uncatchable badarg; num_from_int saturates to
///     ±Infinity instead). "-0" goes through the float parser to keep -0.0.
///   - exponent, no fraction → synthesize one: "1e5" → "1.0e5".
///   - fraction present → the span is already valid Erlang float syntax.
///
/// This is total: `scan_number` has already validated the grammar, so the
/// only remaining failure mode is magnitude overflow, which saturates to
/// ±Infinity (§7.1.4.1) instead of being misreported as a syntax error.
fn number_span_to_num(s: String, span: NumberSpan) -> value.JsNum {
  case span.frac_len > 0, span.exp_len > 0 {
    True, _ -> float_or_saturate(s, s)
    False, True -> {
      let mantissa = string.slice(s, 0, span.int_len)
      let exponent = string.drop_start(s, span.int_len)
      float_or_saturate(mantissa <> ".0" <> exponent, s)
    }
    False, False ->
      case s {
        "-0" -> float_or_saturate("-0.0", s)
        _ ->
          case int.parse(s) {
            Ok(n) -> value.num_from_int(n)
            // Unreachable: scan_number only accepts `-? (0 | [1-9][0-9]*)`
            // here, which int.parse always handles (arbitrary precision).
            Error(Nil) -> saturate_to_infinity(s)
          }
      }
  }
}

/// Parse `erlang_syntax` (the span rewritten into Erlang float syntax) as a
/// float. Erlang's parser underflows to 0.0 silently, so after `scan_number`
/// has accepted the grammar the ONLY way it can fail is magnitude overflow —
/// a double can't hold the value. Saturate to ±Infinity, mirroring what
/// `value.num_from_int` does for huge integer spans.
fn float_or_saturate(erlang_syntax: String, original: String) -> value.JsNum {
  case gleam_stdlib_parse_float(erlang_syntax) {
    Ok(f) -> Finite(f)
    Error(Nil) -> saturate_to_infinity(original)
  }
}

/// ±Infinity, signed by the leading '-' of the original number span.
fn saturate_to_infinity(s: String) -> value.JsNum {
  case string.starts_with(s, "-") {
    True -> NegInfinity
    False -> value.Infinity
  }
}

@external(erlang, "gleam_stdlib", "parse_float")
fn gleam_stdlib_parse_float(s: String) -> Result(Float, Nil)

/// Parse a JSON array (after the opening '[').
fn parse_array(
  bytes: BitArray,
  acc: List(JsonValue),
) -> Result(#(JsonValue, BitArray), JsonParseError) {
  let bytes = skip_whitespace(bytes)
  case bytes {
    <<>> -> Error(UnterminatedArray)
    // ']'
    <<0x5D, rest:bytes>> -> Ok(#(JsonArray(list.reverse(acc)), rest))
    _ -> {
      // If not the first element, expect a comma
      let bytes = case acc {
        [] -> Ok(bytes)
        _ ->
          case bytes {
            // ','
            <<0x2C, rest:bytes>> -> Ok(skip_whitespace(rest))
            _ -> Error(Expected(what: "',' or ']'", in_: "array"))
          }
      }
      use bytes <- result.try(bytes)
      use #(val, rest) <- result.try(parse_value(bytes))
      parse_array(rest, [val, ..acc])
    }
  }
}

/// Parse a JSON object (after the opening '{').
fn parse_object(
  bytes: BitArray,
  acc: List(#(String, JsonValue)),
) -> Result(#(JsonValue, BitArray), JsonParseError) {
  let bytes = skip_whitespace(bytes)
  case bytes {
    <<>> -> Error(UnterminatedObject)
    // '}'
    <<0x7D, rest:bytes>> -> Ok(#(JsonObject(list.reverse(acc)), rest))
    _ -> {
      // If not the first entry, expect a comma
      let bytes = case acc {
        [] -> Ok(bytes)
        _ ->
          case bytes {
            // ','
            <<0x2C, rest:bytes>> -> Ok(skip_whitespace(rest))
            _ -> Error(Expected(what: "',' or '}'", in_: "object"))
          }
      }
      use bytes <- result.try(bytes)
      // Parse key (must be a string)
      use rest <- result.try(case skip_whitespace(bytes) {
        // '"'
        <<0x22, rest:bytes>> -> Ok(rest)
        _ -> Error(Expected(what: "string key", in_: "object"))
      })
      use #(key, rest) <- result.try(parse_string(rest))
      use rest <- result.try(case skip_whitespace(rest) {
        // ':'
        <<0x3A, rest:bytes>> -> Ok(rest)
        _ -> Error(Expected(what: "':' after key", in_: "object"))
      })
      use #(val, rest) <- result.try(parse_value(rest))
      parse_object(rest, [#(key, val), ..acc])
    }
  }
}

/// Slice the first `len` bytes off `bytes` as a String.
/// O(1) on BEAM — the slice is a zero-copy sub-binary of the input.
fn take_string(bytes: BitArray, len: Int) -> Result(String, JsonParseError) {
  case bit_array.slice(bytes, 0, len) {
    Ok(slice) -> bit_array.to_string(slice) |> result.replace_error(InvalidUtf8)
    Error(Nil) -> Error(UnexpectedEnd)
  }
}

/// Drop the first `n` bytes of `bytes` (O(1) sub-binary).
fn drop_bytes(bytes: BitArray, n: Int) -> BitArray {
  case bit_array.slice(bytes, n, bit_array.byte_size(bytes) - n) {
    Ok(rest) -> rest
    Error(Nil) -> <<>>
  }
}

/// Materialize a parsed JsonValue into a JsValue, allocating objects on the heap.
fn materialize(
  h: Heap(host),
  b: common.Builtins,
  val: JsonValue,
) -> #(Heap(host), JsValue) {
  case val {
    JsonNull -> #(h, JsNull)
    JsonBool(b_val) -> #(h, JsBool(b_val))
    JsonNumber(n) -> #(h, JsNumber(n))
    JsonString(s) -> #(h, JsString(s))
    JsonArray(items) -> {
      let #(h, js_items) = materialize_list(h, b, items, [])
      let #(h, ref) = common.alloc_array(h, js_items, b.array.prototype)
      #(h, JsObject(ref))
    }
    JsonObject(entries) -> {
      let #(h, props) = materialize_object_entries(h, b, entries, [])
      let #(h, ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: OrdinaryObject,
            // Not dict.from_list: duplicate JSON keys must keep the FIRST
            // occurrence's position with the LAST occurrence's value.
            properties: value.props_dict_from_pairs(props),
            elements: elements.new(),
            prototype: Some(b.object.prototype),
            symbol_properties: [],
            extensible: True,
          ),
        )
      #(h, JsObject(ref))
    }
  }
}

fn materialize_list(
  h: Heap(host),
  b: common.Builtins,
  items: List(JsonValue),
  acc: List(JsValue),
) -> #(Heap(host), List(JsValue)) {
  case items {
    [] -> #(h, list.reverse(acc))
    [item, ..rest] -> {
      let #(h, val) = materialize(h, b, item)
      materialize_list(h, b, rest, [val, ..acc])
    }
  }
}

fn materialize_object_entries(
  h: Heap(host),
  b: common.Builtins,
  entries: List(#(String, JsonValue)),
  acc: List(#(value.PropertyKey, Property)),
) -> #(Heap(host), List(#(value.PropertyKey, Property))) {
  case entries {
    [] -> #(h, list.reverse(acc))
    [#(key, val), ..rest] -> {
      let #(h, js_val) = materialize(h, b, val)
      materialize_object_entries(h, b, rest, [
        #(value.canonical_key(key), value.data_property(js_val)),
        ..acc
      ])
    }
  }
}

// ============================================================================
// JSON.stringify(value)
// ============================================================================

/// ES2024 §25.5.2 JSON.stringify ( value [ , replacer [ , space ] ] )
///
/// Steps 1-3: state setup (stack, indent, ReplacerFunction, PropertyList).
/// Steps 4: replacer processing (function or array of property names).
/// Steps 5-8: space → gap.
/// Steps 9-11: wrapper holder { "": value }.
/// Step 12: ? SerializeJSONProperty(state, "", wrapper) — undefined when the
/// value is not serializable.
fn json_stringify(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let val = helpers.first_arg_or_undefined(args)
  let replacer = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
  let space = helpers.list_at(args, 2) |> option.unwrap(JsUndefined)

  let result = {
    // Step 4: ReplacerFunction / PropertyList.
    use #(#(replacer_fn, property_list), state) <- result.try(build_replacer(
      state,
      replacer,
    ))
    // Steps 5-8: gap.
    use #(gap, state) <- result.try(compute_gap(state, space))
    // Steps 9-11: wrapper = OrdinaryObjectCreate(%Object.prototype%) with
    // CreateDataPropertyOrThrow(wrapper, "", value).
    let #(heap, wrapper) =
      heap.alloc(
        state.heap,
        ObjectSlot(
          kind: OrdinaryObject,
          properties: dict.from_list([
            #(value.canonical_key(""), value.data_property(val)),
          ]),
          elements: elements.new(),
          prototype: Some(state.builtins.object.prototype),
          symbol_properties: [],
          extensible: True,
        ),
      )
    let state = State(..state, heap:)
    let ctx = StringifyCtx(replacer_fn:, property_list:, gap:)
    // Step 12.
    serialize_property(state, ctx, [], "", "", wrapper)
  }
  case result {
    Ok(#(Some(s), state)) -> #(state, Ok(JsString(s)))
    Ok(#(None, state)) -> #(state, Ok(JsUndefined))
    Error(#(err, state)) -> #(state, Error(err))
  }
}

/// Immutable parts of the spec's JSON Serialization Record: ReplacerFunction,
/// PropertyList and Gap. (Stack and Indent are threaded as parameters.)
type StringifyCtx {
  StringifyCtx(
    replacer_fn: Option(JsValue),
    property_list: Option(List(String)),
    gap: String,
  )
}

const circular_msg = "Converting circular structure to JSON"

const revoked_proxy_msg = "Cannot perform 'IsArray' on a proxy that has been revoked"

/// §25.5.2 step 4: derive ReplacerFunction (callable replacer) or
/// PropertyList (array replacer) from the second argument.
fn build_replacer(
  state: State(host),
  replacer: JsValue,
) -> Result(
  #(#(Option(JsValue), Option(List(String))), State(host)),
  #(JsValue, State(host)),
) {
  case replacer {
    JsObject(ref) ->
      case helpers.is_callable(state.heap, replacer) {
        // Step 4.a: IsCallable → ReplacerFunction.
        True -> Ok(#(#(Some(replacer), None), state))
        False ->
          // Step 4.b.i: isArray = ? IsArray(replacer) — a revoked proxy
          // makes IsArray throw a TypeError.
          case objops.is_array_ref(state.heap, ref) {
            Error(Nil) -> coerce.thrown_type_error(state, revoked_proxy_msg)
            Ok(False) -> Ok(#(#(None, None), state))
            // Step 4.b.iii: build PropertyList from the array elements.
            Ok(True) -> {
              use #(len, state) <- result.try(length_of_array_like(state, ref))
              use #(items, state) <- result.map(
                collect_property_list(state, ref, 0, len, set.new(), []),
              )
              #(#(None, Some(items)), state)
            }
          }
      }
    _ -> Ok(#(#(None, None), state))
  }
}

/// §25.5.2 step 4.b.iii.5: for each index k of the replacer array, Get the
/// element and convert it to a property-name string (String/Number primitives
/// and String/Number wrapper objects only), deduplicating.
fn collect_property_list(
  state: State(host),
  ref: Ref,
  k: Int,
  len: Int,
  seen: Set(String),
  acc: List(String),
) -> Result(#(List(String), State(host)), #(JsValue, State(host))) {
  case k >= len {
    True -> Ok(#(list.reverse(acc), state))
    False -> {
      use #(v, state) <- result.try(objops.get_value(
        state,
        ref,
        value.canonical_key(int.to_string(k)),
        JsObject(ref),
      ))
      use #(item, state) <- result.try(replacer_item(state, v))
      case item {
        Some(s) ->
          case set.contains(seen, s) {
            True -> collect_property_list(state, ref, k + 1, len, seen, acc)
            False ->
              collect_property_list(
                state,
                ref,
                k + 1,
                len,
                set.insert(seen, s),
                [s, ..acc],
              )
          }
        None -> collect_property_list(state, ref, k + 1, len, seen, acc)
      }
    }
  }
}

/// One PropertyList item (§25.5.2 step 4.b.iii.5.b-f): String stays, Number
/// is formatted, String/Number wrapper objects go through ToString (which can
/// re-enter user toString/valueOf and throw); everything else is skipped.
fn replacer_item(
  state: State(host),
  v: JsValue,
) -> Result(#(Option(String), State(host)), #(JsValue, State(host))) {
  case v {
    JsString(s) -> Ok(#(Some(s), state))
    JsNumber(_) -> {
      use #(s, state) <- result.map(coerce.js_to_string(state, v))
      #(Some(s), state)
    }
    JsObject(vref) ->
      case heap.read(state.heap, vref) {
        Some(ObjectSlot(kind: value.StringObject(_), ..))
        | Some(ObjectSlot(kind: value.NumberObject(_), ..)) -> {
          use #(s, state) <- result.map(coerce.js_to_string(state, v))
          #(Some(s), state)
        }
        _ -> Ok(#(None, state))
      }
    _ -> Ok(#(None, state))
  }
}

/// §25.5.2 steps 5-8: compute the gap string from the space argument.
/// Number/String wrapper objects are unwrapped via full ToNumber/ToString
/// (observable valueOf/toString calls — abrupt completions propagate).
fn compute_gap(
  state: State(host),
  space: JsValue,
) -> Result(#(String, State(host)), #(JsValue, State(host))) {
  // Step 5: unwrap Number/String wrapper objects.
  use #(space, state) <- result.try(case space {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.NumberObject(_), ..)) -> {
          use #(n, state) <- result.map(coerce.js_to_number(state, space))
          #(JsNumber(n), state)
        }
        Some(ObjectSlot(kind: value.StringObject(_), ..)) -> {
          use #(s, state) <- result.map(coerce.js_to_string(state, space))
          #(JsString(s), state)
        }
        _ -> Ok(#(space, state))
      }
    _ -> Ok(#(space, state))
  })
  let gap = case space {
    // Step 6: Number → min(10, ToIntegerOrInfinity(space)) spaces.
    JsNumber(n) -> {
      let mv = int.min(10, space_to_integer(n))
      case mv < 1 {
        True -> ""
        False -> string.repeat(" ", mv)
      }
    }
    // Step 7: String → first 10 code units.
    JsString(s) ->
      case string.length(s) <= 10 {
        True -> s
        False -> string.slice(s, 0, 10)
      }
    // Step 8: otherwise no gap.
    _ -> ""
  }
  Ok(#(gap, state))
}

/// ToIntegerOrInfinity (§7.1.5) restricted to what the gap computation needs:
/// NaN/-∞ → 0 (both produce an empty gap), +∞ → 10 (the min-10 clamp).
fn space_to_integer(n: value.JsNum) -> Int {
  case n {
    Finite(f) -> float.truncate(f)
    value.Infinity -> 10
    NaN | NegInfinity -> 0
  }
}

/// 2^53 - 1, the ToLength (§7.1.17) upper clamp.
const max_safe_length = 9_007_199_254_740_991

/// LengthOfArrayLike (§7.3.18): ToLength(? Get(obj, "length")). The Get goes
/// through full [[Get]] (proxy traps, getters), ToNumber can re-enter valueOf.
fn length_of_array_like(
  state: State(host),
  ref: Ref,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  use #(len_val, state) <- result.try(objops.get_value(
    state,
    ref,
    value.canonical_key("length"),
    JsObject(ref),
  ))
  use #(n, state) <- result.map(coerce.js_to_number(state, len_val))
  let len = case n {
    Finite(f) -> int.clamp(float.truncate(f), 0, max_safe_length)
    value.Infinity -> max_safe_length
    NaN | NegInfinity -> 0
  }
  #(len, state)
}

/// SerializeJSONProperty (§25.5.2.1).
///
/// `stack` is the list of heap ids of objects currently being serialized
/// (circular detection); `indent` is the current indentation. Returns
/// Some(json) or None when the value must be omitted (undefined / callable /
/// symbol).
fn serialize_property(
  state: State(host),
  ctx: StringifyCtx,
  stack: List(Int),
  indent: String,
  key: String,
  holder: Ref,
) -> Result(#(Option(String), State(host)), #(JsValue, State(host))) {
  // Step 1: value = ? Get(holder, key).
  use #(val, state) <- result.try(objops.get_value(
    state,
    holder,
    value.canonical_key(key),
    JsObject(holder),
  ))
  // Step 2: toJSON — looked up for Objects AND BigInt primitives (GetV).
  use #(val, state) <- result.try(case val {
    JsObject(_) | value.JsBigInt(_) -> {
      use #(to_json, state) <- result.try(objops.get_value_of(
        state,
        val,
        value.canonical_key("toJSON"),
      ))
      case helpers.is_callable(state.heap, to_json) {
        True -> state.call(state, to_json, val, [JsString(key)])
        False -> Ok(#(val, state))
      }
    }
    _ -> Ok(#(val, state))
  })
  // Step 3: ReplacerFunction — called with the holder as `this`.
  use #(val, state) <- result.try(case ctx.replacer_fn {
    Some(rf) -> state.call(state, rf, JsObject(holder), [JsString(key), val])
    None -> Ok(#(val, state))
  })
  // Step 4: unwrap Number/String/Boolean/BigInt wrapper objects.
  use #(val, state) <- result.try(case val {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        // Step 4.a: [[NumberData]] → ToNumber(value) (observable valueOf).
        Some(ObjectSlot(kind: value.NumberObject(_), ..)) -> {
          use #(n, state) <- result.map(coerce.js_to_number(state, val))
          #(JsNumber(n), state)
        }
        // Step 4.b: [[StringData]] → ToString(value) (observable toString).
        Some(ObjectSlot(kind: value.StringObject(_), ..)) -> {
          use #(s, state) <- result.map(coerce.js_to_string(state, val))
          #(JsString(s), state)
        }
        // Step 4.c: [[BooleanData]] → the wrapped boolean directly.
        Some(ObjectSlot(kind: value.BooleanObject(b), ..)) ->
          Ok(#(JsBool(b), state))
        // Step 4.d: [[BigIntData]] → the wrapped BigInt (then step 10 throws).
        Some(ObjectSlot(kind: value.BigIntObject(bi), ..)) ->
          Ok(#(value.JsBigInt(bi), state))
        _ -> Ok(#(val, state))
      }
    _ -> Ok(#(val, state))
  })
  // Steps 5-12: dispatch on the (possibly unwrapped) value.
  case val {
    JsNull -> Ok(#(Some("null"), state))
    JsBool(True) -> Ok(#(Some("true"), state))
    JsBool(False) -> Ok(#(Some("false"), state))
    JsString(s) -> Ok(#(Some(stringify_string(s)), state))
    JsNumber(Finite(n)) -> Ok(#(Some(value.js_format_number(n)), state))
    // Step 9: non-finite numbers serialize as "null".
    JsNumber(NaN) | JsNumber(value.Infinity) | JsNumber(NegInfinity) ->
      Ok(#(Some("null"), state))
    // Step 10: BigInt → TypeError.
    value.JsBigInt(_) ->
      coerce.thrown_type_error(state, "Do not know how to serialize a BigInt")
    // Step 11: non-callable objects recurse; callables are omitted (step 12).
    JsObject(ref) ->
      case helpers.is_callable(state.heap, val) {
        True -> Ok(#(None, state))
        False ->
          case objops.is_array_ref(state.heap, ref) {
            Error(Nil) -> coerce.thrown_type_error(state, revoked_proxy_msg)
            Ok(True) -> {
              use #(s, state) <- result.map(serialize_array(
                state,
                ctx,
                stack,
                indent,
                ref,
              ))
              #(Some(s), state)
            }
            Ok(False) -> {
              use #(s, state) <- result.map(serialize_object(
                state,
                ctx,
                stack,
                indent,
                ref,
              ))
              #(Some(s), state)
            }
          }
      }
    // Step 12: undefined / symbol → omitted.
    JsUndefined | value.JsSymbol(_) | value.JsUninitialized ->
      Ok(#(None, state))
  }
}

/// SerializeJSONObject (§25.5.2.4): keys from PropertyList (if present) or
/// EnumerableOwnPropertyNames(value, key) (proxy ownKeys-trap aware).
fn serialize_object(
  state: State(host),
  ctx: StringifyCtx,
  stack: List(Int),
  indent: String,
  ref: Ref,
) -> Result(#(String, State(host)), #(JsValue, State(host))) {
  // Steps 1-2: circular detection on the serialization stack.
  case list.contains(stack, ref.id) {
    True -> coerce.thrown_type_error(state, circular_msg)
    False -> {
      let stack = [ref.id, ..stack]
      let step_indent = indent <> ctx.gap
      // Steps 5-6: K = PropertyList or EnumerableOwnPropertyNames(value, key).
      use #(keys, state) <- result.try(case ctx.property_list {
        Some(pl) -> Ok(#(pl, state))
        None -> object_builtins.enumerable_string_keys_stateful(state, ref)
      })
      // Step 8: partial = members serialized in order.
      use #(partial, state) <- result.map(
        serialize_members(state, ctx, stack, step_indent, ref, keys, []),
      )
      #(
        finalize_brackets(partial, ctx.gap, step_indent, indent, "{", "}"),
        state,
      )
    }
  }
}

/// §25.5.2.4 step 8: serialize each key; omitted (undefined) members are
/// skipped, present members render as quoted-key ":" [space] value.
fn serialize_members(
  state: State(host),
  ctx: StringifyCtx,
  stack: List(Int),
  step_indent: String,
  ref: Ref,
  keys: List(String),
  acc: List(String),
) -> Result(#(List(String), State(host)), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(#(list.reverse(acc), state))
    [k, ..rest] -> {
      use #(str_p, state) <- result.try(serialize_property(
        state,
        ctx,
        stack,
        step_indent,
        k,
        ref,
      ))
      case str_p {
        Some(s) -> {
          let sep = case ctx.gap {
            "" -> ":"
            _ -> ": "
          }
          serialize_members(state, ctx, stack, step_indent, ref, rest, [
            stringify_string(k) <> sep <> s,
            ..acc
          ])
        }
        None ->
          serialize_members(state, ctx, stack, step_indent, ref, rest, acc)
      }
    }
  }
}

/// SerializeJSONArray (§25.5.2.5): LengthOfArrayLike + per-index Get
/// (proxy-trap aware); omitted elements serialize as "null".
fn serialize_array(
  state: State(host),
  ctx: StringifyCtx,
  stack: List(Int),
  indent: String,
  ref: Ref,
) -> Result(#(String, State(host)), #(JsValue, State(host))) {
  // Steps 1-2: circular detection.
  case list.contains(stack, ref.id) {
    True -> coerce.thrown_type_error(state, circular_msg)
    False -> {
      let stack = [ref.id, ..stack]
      let step_indent = indent <> ctx.gap
      // Step 6: len = ? LengthOfArrayLike(value).
      use #(len, state) <- result.try(length_of_array_like(state, ref))
      use #(partial, state) <- result.map(
        serialize_elements(state, ctx, stack, step_indent, ref, 0, len, []),
      )
      #(
        finalize_brackets(partial, ctx.gap, step_indent, indent, "[", "]"),
        state,
      )
    }
  }
}

/// §25.5.2.5 step 8: serialize indices 0..len-1 in order.
fn serialize_elements(
  state: State(host),
  ctx: StringifyCtx,
  stack: List(Int),
  step_indent: String,
  ref: Ref,
  i: Int,
  len: Int,
  acc: List(String),
) -> Result(#(List(String), State(host)), #(JsValue, State(host))) {
  case i >= len {
    True -> Ok(#(list.reverse(acc), state))
    False -> {
      use #(str_p, state) <- result.try(serialize_property(
        state,
        ctx,
        stack,
        step_indent,
        int.to_string(i),
        ref,
      ))
      // Step 8.b: undefined → "null".
      let s = option.unwrap(str_p, "null")
      serialize_elements(state, ctx, stack, step_indent, ref, i + 1, len, [
        s,
        ..acc
      ])
    }
  }
}

/// Shared tail of SerializeJSONObject/Array: wrap the member strings in
/// brackets, with newline + indent layout when gap is non-empty.
fn finalize_brackets(
  partial: List(String),
  gap: String,
  step_indent: String,
  stepback: String,
  open: String,
  close: String,
) -> String {
  case partial, gap {
    [], _ -> open <> close
    _, "" -> open <> string.join(partial, ",") <> close
    _, _ ->
      open
      <> "\n"
      <> step_indent
      <> string.join(partial, ",\n" <> step_indent)
      <> "\n"
      <> stepback
      <> close
  }
}

/// Stringify a JS string with proper JSON escaping.
///
/// Fast path: byte-scan for characters that need escaping (control chars
/// < 0x20, '"', '\\'). The overwhelming majority of strings are clean, so
/// we return the quoted string directly with no intermediate allocations.
/// Only fall back to the grapheme-walking escape builder when an escapable
/// byte is found. Safe on UTF-8: all multi-byte sequence bytes are >= 0x80,
/// so they can never be mistaken for an escapable ASCII byte.
fn stringify_string(s: String) -> String {
  case needs_json_escape(<<s:utf8>>) {
    False -> "\"" <> s <> "\""
    True ->
      "\"" <> escape_string(string.to_graphemes(s), string_tree.new()) <> "\""
  }
}

/// True if the UTF-8 bytes contain any character that must be escaped in a
/// JSON string: control characters (< 0x20), double quote, or backslash.
fn needs_json_escape(bytes: BitArray) -> Bool {
  case bytes {
    <<b, _:bits>> if b < 0x20 -> True
    <<0x22, _:bits>> -> True
    <<0x5c, _:bits>> -> True
    <<_, rest:bits>> -> needs_json_escape(rest)
    _ -> False
  }
}

fn escape_string(chars: List(String), acc: StringTree) -> String {
  case chars {
    [] -> string_tree.to_string(acc)
    [c, ..rest] -> {
      let escaped = case c {
        // CR LF forms a single grapheme cluster (UAX #29 GB3), so it would
        // miss the single-char clauses below and leak through raw.
        "\r\n" -> "\\r\\n"
        "\"" -> "\\\""
        "\\" -> "\\\\"
        "\n" -> "\\n"
        "\r" -> "\\r"
        "\t" -> "\\t"
        "\u{0008}" -> "\\b"
        "\u{000C}" -> "\\f"
        _ -> {
          // Check for other control characters (0x00-0x1F)
          case string.to_utf_codepoints(c) {
            [cp] -> {
              let code = string.utf_codepoint_to_int(cp)
              case code < 0x20 {
                True -> unicode_escape(code)
                False -> c
              }
            }
            _ -> c
          }
        }
      }
      escape_string(rest, string_tree.append(acc, escaped))
    }
  }
}

/// Format a codepoint as \uXXXX — four LOWERCASE hex digits, per
/// QuoteJSONString's UnicodeEscape (§25.5.2.3).
fn unicode_escape(code: Int) -> String {
  let hex =
    int.to_base_string(code, 16)
    |> result.unwrap("0")
    |> string.lowercase
  let padded = string.pad_start(hex, to: 4, with: "0")
  "\\u" <> padded
}
