import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/coerce
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type JsonNativeFn, type Property, type Ref, ArrayObject,
  DataProperty, Finite, FunctionObject, JsBool, JsNull, JsNumber, JsObject,
  JsString, JsUndefined, JsonNative, JsonParse, JsonStringify, NaN,
  NativeFunction, NegInfinity, ObjectSlot, OrdinaryObject,
}
import gleam/bit_array
import gleam/dict
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
pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("parse", JsonNative(JsonParse), 1),
      #("stringify", JsonNative(JsonStringify), 1),
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
/// Simplified: reviver parameter is not yet implemented.
///
/// Steps:
///   1. Let jsonString be ? ToString(text).
///   2. Parse jsonString as a JSON text as specified in ECMA-404.
///   3. If the parse fails, throw a SyntaxError exception.
///   4. Return the parsed value.
fn json_parse(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
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
          syntax_error(state, "Unexpected non-whitespace character after JSON")
      }
    }
    // Step 2: If parse fails, throw SyntaxError
    Error(msg) -> syntax_error(state, msg)
  }
}

/// Intermediate parsed JSON value — not yet materialized onto the JS heap.
/// We parse into this first, then walk it to create JsValues/heap objects.
type JsonValue {
  JsonNull
  JsonBool(Bool)
  JsonNumber(Float)
  JsonString(String)
  JsonArray(List(JsonValue))
  JsonObject(List(#(String, JsonValue)))
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
fn parse_value(bytes: BitArray) -> Result(#(JsonValue, BitArray), String) {
  let bytes = skip_whitespace(bytes)
  case bytes {
    <<>> -> Error("Unexpected end of JSON input")
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
      Error(
        "Unexpected token '" <> string.from_utf_codepoints([c]) <> "' in JSON",
      )
    _ -> Error("Unexpected token in JSON")
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
  UnterminatedString
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
    _ -> UnterminatedString
  }
}

/// Parse the content of a JSON string (after the opening quote).
/// Fast path: no escapes — the result is an O(1) zero-copy sub-binary of
/// the input. Only when a backslash is seen do we build an escape buffer.
fn parse_string(bytes: BitArray) -> Result(#(String, BitArray), String) {
  case scan_string(bytes, 0) {
    FoundQuote(n, after) -> {
      use s <- result.map(take_string(bytes, n))
      #(s, after)
    }
    FoundEscape(n, after) -> {
      use chunk <- result.try(take_string(bytes, n))
      parse_escape(after, string_tree.from_string(chunk))
    }
    FoundControlChar -> Error("Unexpected control character in JSON string")
    UnterminatedString -> Error("Unterminated string in JSON")
  }
}

/// Slow path: accumulate decoded string content, appending unescaped spans
/// as whole chunks (one append per span, not per character).
fn parse_string_content(
  bytes: BitArray,
  acc: StringTree,
) -> Result(#(String, BitArray), String) {
  case scan_string(bytes, 0) {
    FoundQuote(n, after) -> {
      use chunk <- result.map(take_string(bytes, n))
      #(string_tree.to_string(string_tree.append(acc, chunk)), after)
    }
    FoundEscape(n, after) -> {
      use chunk <- result.try(take_string(bytes, n))
      parse_escape(after, string_tree.append(acc, chunk))
    }
    FoundControlChar -> Error("Unexpected control character in JSON string")
    UnterminatedString -> Error("Unterminated string in JSON")
  }
}

/// Decode one escape sequence; `bytes` starts just after the backslash.
fn parse_escape(
  bytes: BitArray,
  acc: StringTree,
) -> Result(#(String, BitArray), String) {
  case bytes {
    <<>> -> Error("Unterminated string escape in JSON")
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
      Error(
        "Invalid escape character '\\"
        <> string.from_utf_codepoints([c])
        <> "' in JSON",
      )
    _ -> Error("Unterminated string escape in JSON")
  }
}

/// Parse a 4-digit hex escape (\uXXXX), returning the integer codepoint value.
fn parse_unicode_escape(bytes: BitArray) -> Result(#(Int, BitArray), String) {
  case bytes {
    <<a, b, c, d, rest:bytes>> -> {
      let parsed =
        bit_array.to_string(<<a, b, c, d>>)
        |> result.try(int.base_parse(_, 16))
      case parsed {
        Ok(n) -> Ok(#(n, rest))
        Error(Nil) -> Error("Invalid Unicode escape in JSON")
      }
    }
    _ -> Error("Unexpected end of Unicode escape in JSON")
  }
}

/// Decode a \uXXXX escape (possibly a surrogate pair) into a UTF-8 string.
/// Returns #(decoded_string, remaining_bytes). Lone surrogates become U+FFFD.
fn decode_unicode_escape(
  bytes: BitArray,
) -> Result(#(String, BitArray), String) {
  use #(high, rest) <- result.try(parse_unicode_escape(bytes))
  case high >= 0xD800 && high <= 0xDBFF {
    // High surrogate — look for a trailing \uXXXX low surrogate
    True ->
      case parse_low_surrogate(rest) {
        Some(#(low, rest)) -> {
          let combined = { high - 0xD800 } * 0x400 + { low - 0xDC00 } + 0x10000
          codepoint_to_string(combined) |> result.map(fn(s) { #(s, rest) })
        }
        // Lone/unpaired high surrogate → U+FFFD replacement char
        None -> Ok(#("\u{FFFD}", rest))
      }
    False -> codepoint_to_string(high) |> result.map(fn(s) { #(s, rest) })
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
fn codepoint_to_string(codepoint: Int) -> Result(String, String) {
  string.utf_codepoint(codepoint)
  |> result.map(fn(cp) { string.from_utf_codepoints([cp]) })
  |> result.replace_error("Invalid Unicode codepoint in JSON string")
}

/// Parse a JSON number — count the number-byte span, slice it out as a
/// sub-binary (O(1)), and parse that.
fn parse_number(bytes: BitArray) -> Result(#(JsonValue, BitArray), String) {
  let n = count_number_bytes(bytes, 0)
  use num_str <- result.try(take_string(bytes, n))
  let rest = drop_bytes(bytes, n)
  case parse_json_number_string(num_str) {
    Ok(f) -> Ok(#(JsonNumber(f), rest))
    Error(Nil) -> Error("Invalid number '" <> num_str <> "' in JSON")
  }
}

/// Count leading bytes that could be part of a JSON number.
fn count_number_bytes(bytes: BitArray, n: Int) -> Int {
  case bytes {
    // '-' '+' '.' 'e' 'E' or '0'..'9'
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

/// Parse a collected number string into a Float.
fn parse_json_number_string(s: String) -> Result(Float, Nil) {
  // Try parsing as float first, then fall back to int parse → to_float
  gleam_stdlib_parse_float(s)
  |> result.try_recover(fn(_) { int.parse(s) |> result.map(int.to_float) })
}

@external(erlang, "gleam_stdlib", "parse_float")
fn gleam_stdlib_parse_float(s: String) -> Result(Float, Nil)

/// Parse a JSON array (after the opening '[').
fn parse_array(
  bytes: BitArray,
  acc: List(JsonValue),
) -> Result(#(JsonValue, BitArray), String) {
  let bytes = skip_whitespace(bytes)
  case bytes {
    <<>> -> Error("Unterminated array in JSON")
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
            _ -> Error("Expected ',' or ']' in array")
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
) -> Result(#(JsonValue, BitArray), String) {
  let bytes = skip_whitespace(bytes)
  case bytes {
    <<>> -> Error("Unterminated object in JSON")
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
            _ -> Error("Expected ',' or '}' in object")
          }
      }
      use bytes <- result.try(bytes)
      // Parse key (must be a string)
      use rest <- result.try(case skip_whitespace(bytes) {
        // '"'
        <<0x22, rest:bytes>> -> Ok(rest)
        _ -> Error("Expected string key in object")
      })
      use #(key, rest) <- result.try(parse_string(rest))
      use rest <- result.try(case skip_whitespace(rest) {
        // ':'
        <<0x3A, rest:bytes>> -> Ok(rest)
        _ -> Error("Expected ':' after key in object")
      })
      use #(val, rest) <- result.try(parse_value(rest))
      parse_object(rest, [#(key, val), ..acc])
    }
  }
}

/// Slice the first `len` bytes off `bytes` as a String.
/// O(1) on BEAM — the slice is a zero-copy sub-binary of the input.
fn take_string(bytes: BitArray, len: Int) -> Result(String, String) {
  case bit_array.slice(bytes, 0, len) {
    Ok(slice) ->
      bit_array.to_string(slice)
      |> result.replace_error("Invalid UTF-8 in JSON input")
    Error(Nil) -> Error("Unexpected end of JSON input")
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
  h: Heap,
  b: common.Builtins,
  val: JsonValue,
) -> #(Heap, JsValue) {
  case val {
    JsonNull -> #(h, JsNull)
    JsonBool(b_val) -> #(h, JsBool(b_val))
    JsonNumber(n) -> #(h, JsNumber(Finite(n)))
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
            properties: dict.from_list(props),
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
  h: Heap,
  b: common.Builtins,
  items: List(JsonValue),
  acc: List(JsValue),
) -> #(Heap, List(JsValue)) {
  case items {
    [] -> #(h, list.reverse(acc))
    [item, ..rest] -> {
      let #(h, val) = materialize(h, b, item)
      materialize_list(h, b, rest, [val, ..acc])
    }
  }
}

fn materialize_object_entries(
  h: Heap,
  b: common.Builtins,
  entries: List(#(String, JsonValue)),
  acc: List(#(value.PropertyKey, Property)),
) -> #(Heap, List(#(value.PropertyKey, Property))) {
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

/// ES2024 S25.5.2 JSON.stringify ( value [ , replacer [ , space ] ] )
///
/// Simplified: replacer and space parameters are not yet implemented.
///
/// After setup (stack, indent, gap, ReplacerFunction, wrapper), the final step
/// returns ? SerializeJSONProperty(state, "", wrapper) — or undefined if the
/// value is not serializable.
fn json_stringify(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = helpers.first_arg_or_undefined(args)

  case stringify_value(state.heap, val, set.new()) {
    Ok(Some(s)) -> #(state, Ok(JsString(s)))
    Ok(None) -> #(state, Ok(JsUndefined))
    Error(err_msg) -> {
      let #(heap, err) =
        common.make_type_error(state.heap, state.builtins, err_msg)
      #(State(..state, heap:), Error(err))
    }
  }
}

/// Stringify a JsValue. Returns:
///   Ok(Some(json_string)) — successfully serialized
///   Ok(None) — value should be omitted (undefined, function, symbol)
///   Error(msg) — circular reference or other TypeError
fn stringify_value(
  h: Heap,
  val: JsValue,
  seen: Set(Int),
) -> Result(Option(String), String) {
  case val {
    JsNull -> Ok(Some("null"))
    JsBool(True) -> Ok(Some("true"))
    JsBool(False) -> Ok(Some("false"))
    JsNumber(Finite(n)) -> Ok(Some(value.js_format_number(n)))
    JsNumber(NaN) | JsNumber(value.Infinity) | JsNumber(NegInfinity) ->
      Ok(Some("null"))
    JsString(s) -> Ok(Some(stringify_string(s)))
    // undefined, functions, and symbols return None (omitted)
    JsUndefined | value.JsSymbol(_) | value.JsUninitialized -> Ok(None)
    value.JsBigInt(_) -> Error("Do not know how to serialize a BigInt")
    JsObject(ref) -> {
      case set.contains(seen, ref.id) {
        True -> Error("Converting circular structure to JSON")
        False -> {
          let seen = set.insert(seen, ref.id)
          case heap.read(h, ref) {
            Some(ObjectSlot(kind: FunctionObject(..), ..))
            | Some(ObjectSlot(kind: NativeFunction(..), ..)) ->
              // Functions are omitted at top level (return undefined)
              Ok(None)
            Some(ObjectSlot(kind: ArrayObject(length:), elements:, ..)) ->
              stringify_array(h, elements, length, 0, seen, [])
            Some(ObjectSlot(kind: OrdinaryObject, properties:, ..)) ->
              stringify_object(h, dict.to_list(properties), seen, [])
            Some(ObjectSlot(kind: value.StringObject(s), ..)) ->
              // Boxed string — unwrap and stringify as string
              Ok(Some(stringify_string(s)))
            Some(ObjectSlot(kind: value.NumberObject(n), ..)) ->
              // Boxed number — unwrap and stringify as number
              case n {
                Finite(f) -> Ok(Some(value.js_format_number(f)))
                _ -> Ok(Some("null"))
              }
            Some(ObjectSlot(kind: value.BooleanObject(b), ..)) ->
              // Boxed boolean — unwrap
              case b {
                True -> Ok(Some("true"))
                False -> Ok(Some("false"))
              }
            _ ->
              // Other exotic objects (promises, generators, etc.) — treat as empty object
              Ok(Some("{}"))
          }
        }
      }
    }
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

/// Format a codepoint as \uXXXX.
fn unicode_escape(code: Int) -> String {
  let hex = int.to_base_string(code, 16) |> result.unwrap("0")
  let padded = string.pad_start(hex, to: 4, with: "0")
  "\\u" <> padded
}

/// Stringify a JSON array.
fn stringify_array(
  h: Heap,
  elements: value.JsElements,
  length: Int,
  idx: Int,
  seen: Set(Int),
  acc: List(String),
) -> Result(Option(String), String) {
  case idx >= length {
    True -> Ok(Some("[" <> string.join(list.reverse(acc), ",") <> "]"))
    False -> {
      let elem = elements.get(elements, idx)
      case stringify_value(h, elem, seen) {
        Ok(Some(s)) ->
          stringify_array(h, elements, length, idx + 1, seen, [s, ..acc])
        Ok(None) ->
          // undefined/function/symbol in arrays become "null"
          stringify_array(h, elements, length, idx + 1, seen, ["null", ..acc])
        Error(msg) -> Error(msg)
      }
    }
  }
}

/// Stringify a JSON object.
fn stringify_object(
  h: Heap,
  entries: List(#(value.PropertyKey, Property)),
  seen: Set(Int),
  acc: List(String),
) -> Result(Option(String), String) {
  case entries {
    [] -> Ok(Some("{" <> string.join(list.reverse(acc), ",") <> "}"))
    [#(key, DataProperty(value: val, enumerable: True, ..)), ..rest] -> {
      case stringify_value(h, val, seen) {
        Ok(Some(s)) -> {
          let entry = stringify_string(value.key_to_string(key)) <> ":" <> s
          stringify_object(h, rest, seen, [entry, ..acc])
        }
        Ok(None) ->
          // undefined/function/symbol values are omitted from objects
          stringify_object(h, rest, seen, acc)
        Error(msg) -> Error(msg)
      }
    }
    [#(_key, DataProperty(enumerable: False, ..)), ..rest] ->
      // Non-enumerable properties are skipped
      stringify_object(h, rest, seen, acc)
    [#(_key, value.AccessorProperty(..)), ..rest] ->
      // Accessor properties are skipped (would need evaluation)
      stringify_object(h, rest, seen, acc)
  }
}

// ============================================================================
// Error helper
// ============================================================================

/// Create a SyntaxError and return it as an Error result.
fn syntax_error(
  state: State,
  msg: String,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, err) = common.make_syntax_error(state.heap, state.builtins, msg)
  #(State(..state, heap:), Error(err))
}
