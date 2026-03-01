import arc/vm/builtins/common
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type JsonNativeFn, type Property, type Ref, ArrayObject,
  DataProperty, Finite, FunctionObject, JsBool, JsNull, JsNumber, JsObject,
  JsString, JsUndefined, JsonNative, JsonParse, JsonStringify, NaN,
  NativeFunction, NegInfinity, ObjectSlot, OrdinaryObject,
}
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

  let properties = dict.from_list(methods)
  let symbol_properties =
    dict.from_list([
      #(
        value.symbol_to_string_tag,
        value.data(JsString("JSON")) |> value.configurable(),
      ),
    ])

  let #(h, json_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties:,
        elements: js_elements.new(),
        prototype: Some(object_proto),
        symbol_properties:,
        extensible: True,
      ),
    )
  let h = heap.root(h, json_ref)

  #(h, json_ref)
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
    [other, ..] ->
      case frame.to_string(state, other) {
        Ok(#(s, new_state)) -> Ok(#(s, new_state))
        Error(#(thrown, new_state)) -> Error(#(thrown, new_state))
      }
    [] -> Ok(#("undefined", state))
  }

  case to_string_result {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(json_str, state)) -> {
      // Step 2: Parse as JSON text
      let chars = string.to_graphemes(json_str)
      case parse_value(chars) {
        Ok(#(val, rest)) -> {
          // After parsing, skip trailing whitespace and ensure nothing else
          let rest = skip_whitespace(rest)
          case rest {
            [] -> {
              // Successfully parsed — materialize the value on the heap
              let #(heap, js_val) = materialize(state.heap, state.builtins, val)
              #(State(..state, heap:), Ok(js_val))
            }
            _ ->
              syntax_error(
                state,
                "Unexpected non-whitespace character after JSON",
              )
          }
        }
        // Step 3: If parse fails, throw SyntaxError
        Error(msg) -> syntax_error(state, msg)
      }
    }
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

/// Skip whitespace characters (space, tab, newline, carriage return).
fn skip_whitespace(chars: List(String)) -> List(String) {
  case chars {
    [" ", ..rest] | ["\t", ..rest] | ["\n", ..rest] | ["\r", ..rest] ->
      skip_whitespace(rest)
    _ -> chars
  }
}

/// Parse a JSON value from a list of graphemes.
fn parse_value(
  chars: List(String),
) -> Result(#(JsonValue, List(String)), String) {
  let chars = skip_whitespace(chars)
  case chars {
    [] -> Error("Unexpected end of JSON input")
    ["n", ..rest] -> parse_literal(rest, "ull", JsonNull)
    ["t", ..rest] -> parse_literal(rest, "rue", JsonBool(True))
    ["f", ..rest] -> parse_literal(rest, "alse", JsonBool(False))
    ["\"", ..rest] -> {
      use #(s, rest) <- result.map(parse_string_content(rest, string_tree.new()))
      #(JsonString(s), rest)
    }
    ["[", ..rest] -> parse_array(rest, [])
    ["{", ..rest] -> parse_object(rest, [])
    ["-", ..]
    | ["0", ..]
    | ["1", ..]
    | ["2", ..]
    | ["3", ..]
    | ["4", ..]
    | ["5", ..]
    | ["6", ..]
    | ["7", ..]
    | ["8", ..]
    | ["9", ..] -> parse_number(chars)
    [c, ..] -> Error("Unexpected token '" <> c <> "' in JSON")
  }
}

/// Parse a literal keyword (null, true, false) after the first character.
fn parse_literal(
  chars: List(String),
  expected: String,
  val: JsonValue,
) -> Result(#(JsonValue, List(String)), String) {
  let expected_chars = string.to_graphemes(expected)
  case consume_chars(chars, expected_chars) {
    Ok(rest) -> Ok(#(val, rest))
    Error(Nil) -> Error("Unexpected token in JSON")
  }
}

/// Consume expected characters one by one from the input.
fn consume_chars(
  chars: List(String),
  expected: List(String),
) -> Result(List(String), Nil) {
  case expected {
    [] -> Ok(chars)
    [e, ..erest] ->
      case chars {
        [c, ..crest] if c == e -> consume_chars(crest, erest)
        _ -> Error(Nil)
      }
  }
}

/// Parse the content of a JSON string (after the opening quote).
fn parse_string_content(
  chars: List(String),
  acc: StringTree,
) -> Result(#(String, List(String)), String) {
  case chars {
    [] -> Error("Unterminated string in JSON")
    ["\"", ..rest] -> Ok(#(string_tree.to_string(acc), rest))
    ["\\", ..rest] -> {
      case rest {
        [] -> Error("Unterminated string escape in JSON")
        ["\"", ..rest2] ->
          parse_string_content(rest2, string_tree.append(acc, "\""))
        ["\\", ..rest2] ->
          parse_string_content(rest2, string_tree.append(acc, "\\"))
        ["/", ..rest2] ->
          parse_string_content(rest2, string_tree.append(acc, "/"))
        ["b", ..rest2] ->
          parse_string_content(rest2, string_tree.append(acc, "\u{0008}"))
        ["f", ..rest2] ->
          parse_string_content(rest2, string_tree.append(acc, "\u{000C}"))
        ["n", ..rest2] ->
          parse_string_content(rest2, string_tree.append(acc, "\n"))
        ["r", ..rest2] ->
          parse_string_content(rest2, string_tree.append(acc, "\r"))
        ["t", ..rest2] ->
          parse_string_content(rest2, string_tree.append(acc, "\t"))
        ["u", ..rest2] -> {
          case parse_unicode_escape(rest2) {
            Ok(#(codepoint_val, rest3)) -> {
              // Check for surrogate pair
              case codepoint_val >= 0xD800 && codepoint_val <= 0xDBFF {
                True -> {
                  // High surrogate — look for \uXXXX low surrogate
                  case rest3 {
                    ["\\", "u", ..rest4] ->
                      case parse_unicode_escape(rest4) {
                        Ok(#(low, rest5)) if low >= 0xDC00 && low <= 0xDFFF -> {
                          // Combine surrogate pair
                          let combined =
                            { codepoint_val - 0xD800 }
                            * 0x400
                            + { low - 0xDC00 }
                            + 0x10000
                          case string.utf_codepoint(combined) {
                            Ok(cp) ->
                              parse_string_content(
                                rest5,
                                string_tree.append(
                                  acc,
                                  string.from_utf_codepoints([cp]),
                                ),
                              )
                            Error(Nil) ->
                              Error("Invalid Unicode codepoint in JSON string")
                          }
                        }
                        _ ->
                          // Not a valid low surrogate; emit replacement char
                          parse_string_content(
                            rest3,
                            string_tree.append(acc, "\u{FFFD}"),
                          )
                      }
                    _ ->
                      // Lone high surrogate, emit replacement char
                      parse_string_content(
                        rest3,
                        string_tree.append(acc, "\u{FFFD}"),
                      )
                  }
                }
                False ->
                  case string.utf_codepoint(codepoint_val) {
                    Ok(cp) ->
                      parse_string_content(
                        rest3,
                        string_tree.append(
                          acc,
                          string.from_utf_codepoints([cp]),
                        ),
                      )
                    Error(Nil) ->
                      Error("Invalid Unicode codepoint in JSON string")
                  }
              }
            }
            Error(msg) -> Error(msg)
          }
        }
        [c, ..] -> Error("Invalid escape character '\\" <> c <> "' in JSON")
      }
    }
    [c, ..rest] -> parse_string_content(rest, string_tree.append(acc, c))
  }
}

/// Parse a 4-digit hex escape (\uXXXX), returning the integer codepoint value.
fn parse_unicode_escape(
  chars: List(String),
) -> Result(#(Int, List(String)), String) {
  case chars {
    [a, b, c, d, ..rest] -> {
      let hex_str = a <> b <> c <> d
      case int.base_parse(hex_str, 16) {
        Ok(n) -> Ok(#(n, rest))
        Error(Nil) ->
          Error("Invalid Unicode escape '\\u" <> hex_str <> "' in JSON")
      }
    }
    _ -> Error("Unexpected end of Unicode escape in JSON")
  }
}

/// Parse a JSON number.
fn parse_number(
  chars: List(String),
) -> Result(#(JsonValue, List(String)), String) {
  // Collect all characters that could be part of a JSON number
  let #(num_chars, rest) = collect_number_chars(chars, [])
  let num_str = string.join(num_chars, "")
  case parse_json_number_string(num_str) {
    Ok(n) -> Ok(#(JsonNumber(n), rest))
    Error(Nil) -> Error("Invalid number '" <> num_str <> "' in JSON")
  }
}

/// Collect characters that could be part of a JSON number.
fn collect_number_chars(
  chars: List(String),
  acc: List(String),
) -> #(List(String), List(String)) {
  case chars {
    [c, ..rest] ->
      case is_number_char(c) {
        True -> collect_number_chars(rest, [c, ..acc])
        False -> #(list.reverse(acc), chars)
      }
    [] -> #(list.reverse(acc), [])
  }
}

fn is_number_char(c: String) -> Bool {
  case c {
    "-"
    | "+"
    | "."
    | "e"
    | "E"
    | "0"
    | "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9" -> True
    _ -> False
  }
}

/// Parse a collected number string into a Float.
fn parse_json_number_string(s: String) -> Result(Float, Nil) {
  // Try parsing as float first
  case gleam_stdlib_parse_float(s) {
    Ok(f) -> Ok(f)
    Error(Nil) ->
      // Try parsing as integer and converting to float
      case int.parse(s) {
        Ok(n) -> Ok(int.to_float(n))
        Error(Nil) -> Error(Nil)
      }
  }
}

@external(erlang, "gleam_stdlib", "parse_float")
fn gleam_stdlib_parse_float(s: String) -> Result(Float, Nil)

/// Parse a JSON array (after the opening '[').
fn parse_array(
  chars: List(String),
  acc: List(JsonValue),
) -> Result(#(JsonValue, List(String)), String) {
  let chars = skip_whitespace(chars)
  case chars {
    [] -> Error("Unterminated array in JSON")
    ["]", ..rest] -> Ok(#(JsonArray(list.reverse(acc)), rest))
    _ -> {
      // If not the first element, expect a comma
      let chars = case acc {
        [] -> Ok(chars)
        _ ->
          case chars {
            [",", ..rest] -> Ok(skip_whitespace(rest))
            _ -> Error("Expected ',' or ']' in array")
          }
      }
      case chars {
        Error(msg) -> Error(msg)
        Ok(chars) -> {
          case parse_value(chars) {
            Ok(#(val, rest)) -> parse_array(rest, [val, ..acc])
            Error(msg) -> Error(msg)
          }
        }
      }
    }
  }
}

/// Parse a JSON object (after the opening '{').
fn parse_object(
  chars: List(String),
  acc: List(#(String, JsonValue)),
) -> Result(#(JsonValue, List(String)), String) {
  let chars = skip_whitespace(chars)
  case chars {
    [] -> Error("Unterminated object in JSON")
    ["}", ..rest] -> Ok(#(JsonObject(list.reverse(acc)), rest))
    _ -> {
      // If not the first entry, expect a comma
      let chars = case acc {
        [] -> Ok(chars)
        _ ->
          case chars {
            [",", ..rest] -> Ok(skip_whitespace(rest))
            _ -> Error("Expected ',' or '}' in object")
          }
      }
      case chars {
        Error(msg) -> Error(msg)
        Ok(chars) -> {
          // Parse key (must be a string)
          let chars = skip_whitespace(chars)
          case chars {
            ["\"", ..rest] -> {
              case parse_string_content(rest, string_tree.new()) {
                Ok(#(key, rest)) -> {
                  let rest = skip_whitespace(rest)
                  case rest {
                    [":", ..rest] -> {
                      case parse_value(rest) {
                        Ok(#(val, rest)) ->
                          parse_object(rest, [#(key, val), ..acc])
                        Error(msg) -> Error(msg)
                      }
                    }
                    _ -> Error("Expected ':' after key in object")
                  }
                }
                Error(msg) -> Error(msg)
              }
            }
            _ -> Error("Expected string key in object")
          }
        }
      }
    }
  }
}

/// Materialize a parsed JsonValue into a JsValue, allocating objects on the heap.
fn materialize(h: Heap, b: common.Builtins, val: JsonValue) -> #(Heap, JsValue) {
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
            elements: js_elements.new(),
            prototype: Some(b.object.prototype),
            symbol_properties: dict.new(),
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
  acc: List(#(String, Property)),
) -> #(Heap, List(#(String, Property))) {
  case entries {
    [] -> #(h, list.reverse(acc))
    [#(key, val), ..rest] -> {
      let #(h, js_val) = materialize(h, b, val)
      materialize_object_entries(h, b, rest, [
        #(key, value.data_property(js_val)),
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
/// Steps (simplified):
///   1. Let str be ? SerializeJSONProperty(value).
///   2. Return str (or undefined if not serializable).
fn json_stringify(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }

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
fn stringify_string(s: String) -> String {
  "\"" <> escape_string(string.to_graphemes(s), string_tree.new()) <> "\""
}

fn escape_string(chars: List(String), acc: StringTree) -> String {
  case chars {
    [] -> string_tree.to_string(acc)
    [c, ..rest] -> {
      let escaped = case c {
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
      let elem = js_elements.get(elements, idx)
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
  entries: List(#(String, Property)),
  seen: Set(Int),
  acc: List(String),
) -> Result(Option(String), String) {
  case entries {
    [] -> Ok(Some("{" <> string.join(list.reverse(acc), ",") <> "}"))
    [#(key, DataProperty(value: val, enumerable: True, ..)), ..rest] -> {
      case stringify_value(h, val, seen) {
        Ok(Some(s)) -> {
          let entry = stringify_string(key) <> ":" <> s
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
fn syntax_error(state: State, msg: String) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, err) = common.make_syntax_error(state.heap, state.builtins, msg)
  #(State(..state, heap:), Error(err))
}
