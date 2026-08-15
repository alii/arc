//// The `JSON` global namespace (ES2024 §25.5).
////
//// Faithful port of arc/vm/builtins/json.gleam over 2core's threaded
//// Agent. Return-tuple order is `#(JsVal, Agent)` (R1).
//// Simplified vs arc: single-realm (no per-token realm marker) and no
//// json-parse-with-source `context.source` (proposal, not ES2024) — the
//// reviver is called with `(key, value)` only. rawJSON/isRawJSON are
//// implemented (they need no host state).

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsNum, type JsVal, type JsonNative, ArrayObj,
  BigIntObj, BooleanObj, JFloat, JInt, JNan, JNegInf, JPosInf, JsonIsRawJson,
  JsonN, JsonParse, JsonRawJson, JsonStringify, KBig, KBool, KHandle, KNull,
  KNum, KStr, KSym, KUndef, Named, NumberObj, Ordinary, SObject, SShapedObject,
  StringKey, StringObj, classify, index_key, mk_bool, mk_null, mk_number,
  mk_object, mk_string, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
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
/// (like Math), per ES2024 §25.5.
pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(Handle, Agent) {
  let #(methods, st) =
    common.alloc_methods(st, function_proto, [
      #("parse", JsonN(JsonParse), 2),
      #("stringify", JsonN(JsonStringify), 3),
      #("rawJSON", JsonN(JsonRawJson), 1),
      #("isRawJSON", JsonN(JsonIsRawJson), 1),
    ])

  common.init_namespace(st, object_proto, "JSON", methods)
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for JSON native functions.
pub fn dispatch(
  st: Agent,
  native: JsonNative,
  _this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    JsonParse -> json_parse(args, st)
    JsonStringify -> json_stringify(args, st)
    JsonRawJson -> json_raw_json(args, st)
    JsonIsRawJson -> json_is_raw_json(args, st)
  }
}

// ============================================================================
// JSON.parse(text [, reviver])
// ============================================================================

/// ES2024 §25.5.1 JSON.parse ( text [ , reviver ] )
fn json_parse(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  // Step 1: ToString(text).
  let #(json_str, st) =
    rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  // Step 2: Parse as ECMA-404 JSON text.
  let bytes = bit_array.from_string(json_str)
  case parse_value(bytes) {
    Error(e) -> rt_val.t_throw_syntax_error(st, json_error_message(e))
    Ok(#(val, rest)) ->
      case skip_whitespace(rest) {
        <<>> -> {
          // Materialize the parse tree onto the heap.
          let #(unfiltered, st) = materialize(st, val)
          // Steps 7-10: run the reviver, if callable.
          case helpers.list_at(args, 1) {
            Some(reviver) ->
              case rt_call.is_callable(st, reviver) {
                False -> #(unfiltered, st)
                True -> {
                  let #(root, st) = alloc_holder(st, unfiltered)
                  internalize_json_property(st, reviver, root, "")
                }
              }
            None -> #(unfiltered, st)
          }
        }
        _ ->
          rt_val.t_throw_syntax_error(st, json_error_message(TrailingContent))
      }
  }
}

/// InternalizeJSONProperty (§25.5.1.1) — the JSON.parse reviver walk.
fn internalize_json_property(
  st: Agent,
  reviver: JsVal,
  holder: Handle,
  name: String,
) -> #(JsVal, Agent) {
  // Step 1: val = ? Get(holder, name).
  let #(val, st) =
    rt_obj.t_get_prop(
      st,
      mk_object(holder),
      StringKey(rt_types.canonical_key(name)),
    )
  // Steps 4-5: if val is an Object, revive its children in place first.
  let st = case classify(val) {
    KHandle(h) ->
      case is_array_handle(st, h) {
        True -> {
          let #(len, st) = length_of_array_like(st, h)
          internalize_elements(st, reviver, h, 0, len)
        }
        False -> {
          let #(keys, st) = enumerable_string_keys(st, h)
          internalize_keys(st, reviver, h, keys)
        }
      }
    _ -> st
  }
  // Step 6: return ? Call(reviver, holder, « name, val »).
  rt_call.t_call_checked(st, reviver, mk_object(holder), [
    mk_string(name),
    val,
  ])
}

fn internalize_elements(
  st: Agent,
  reviver: JsVal,
  h: Handle,
  i: Int,
  len: Int,
) -> Agent {
  case i >= len {
    True -> st
    False -> {
      let name = int.to_string(i)
      let #(new_elem, st) = internalize_json_property(st, reviver, h, name)
      let st = replace_or_delete(st, h, name, new_elem)
      internalize_elements(st, reviver, h, i + 1, len)
    }
  }
}

fn internalize_keys(
  st: Agent,
  reviver: JsVal,
  h: Handle,
  keys: List(String),
) -> Agent {
  case keys {
    [] -> st
    [p, ..rest] -> {
      let #(new_elem, st) = internalize_json_property(st, reviver, h, p)
      let st = replace_or_delete(st, h, p, new_elem)
      internalize_keys(st, reviver, h, rest)
    }
  }
}

/// §25.5.1.1 steps 2.b.ii.2-3 / 2.c.ii.2-3: undefined result deletes,
/// anything else CreateDataProperty's back. False results are discarded.
fn replace_or_delete(
  st: Agent,
  h: Handle,
  name: String,
  new_element: JsVal,
) -> Agent {
  let key = StringKey(rt_types.canonical_key(name))
  case classify(new_element) {
    KUndef -> {
      let #(_, st) = rt_obj.t_delete_prop(st, h, key)
      st
    }
    _ -> {
      let #(_, st) =
        rt_obj.t_define_own_prop(
          st,
          h,
          key,
          rt_types.ParsedDesc(
            value: Some(new_element),
            get: None,
            set: None,
            writable: Some(True),
            enumerable: Some(True),
            configurable: Some(True),
          ),
        )
      st
    }
  }
}

/// The spec's root holder object: { "": val } with %Object.prototype%.
fn alloc_holder(st: Agent, val: JsVal) -> #(Handle, Agent) {
  let obj_proto = st.realm.object.prototype
  common.alloc_pojo(st, obj_proto, [#("", val)])
}

// ── §25.5.1 scanner ─────────────────────────────────────────────────────────

/// Intermediate parsed JSON value — not yet materialized onto the JS heap.
type JsonValue {
  JsonNull
  JsonBool(Bool)
  JsonNumber(JsNum)
  JsonString(String)
  JsonArray(List(JsonValue))
  JsonObject(List(#(String, JsonValue)))
}

type JsonParseError {
  UnexpectedEnd
  UnexpectedToken(found: String)
  UnterminatedString
  UnterminatedEscape
  UnterminatedArray
  UnterminatedObject
  ControlCharInString
  InvalidEscape(escape: String)
  InvalidUnicodeEscape
  InvalidCodepoint
  InvalidNumber(raw: String)
  Expected(what: String, in_: String)
  InvalidUtf8
  TrailingContent
  RawJsonEmpty
  RawJsonSurroundingWhitespace
  RawJsonNotPrimitive
}

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
    TrailingContent -> "Unexpected non-whitespace character after JSON"
    RawJsonEmpty -> "JSON.rawJSON text must not be empty"
    RawJsonSurroundingWhitespace ->
      "JSON.rawJSON text must not start or end with whitespace"
    RawJsonNotPrimitive -> "JSON.rawJSON text must not be an object or an array"
  }
}

fn skip_whitespace(bytes: BitArray) -> BitArray {
  case bytes {
    <<0x20, rest:bytes>>
    | <<0x09, rest:bytes>>
    | <<0x0a, rest:bytes>>
    | <<0x0d, rest:bytes>> -> skip_whitespace(rest)
    _ -> bytes
  }
}

fn parse_value(
  bytes: BitArray,
) -> Result(#(JsonValue, BitArray), JsonParseError) {
  let bytes = skip_whitespace(bytes)
  case bytes {
    <<>> -> Error(UnexpectedEnd)
    <<0x6e, 0x75, 0x6c, 0x6c, rest:bytes>> -> Ok(#(JsonNull, rest))
    <<0x74, 0x72, 0x75, 0x65, rest:bytes>> -> Ok(#(JsonBool(True), rest))
    <<0x66, 0x61, 0x6c, 0x73, 0x65, rest:bytes>> -> Ok(#(JsonBool(False), rest))
    <<0x22, rest:bytes>> -> {
      use #(s, rest) <- result.map(parse_string(rest))
      #(JsonString(s), rest)
    }
    <<0x5b, rest:bytes>> -> parse_array(rest, [])
    <<0x7b, rest:bytes>> -> parse_object(rest, [])
    <<b, _:bytes>> if b == 0x2d || b >= 0x30 && b <= 0x39 -> parse_number(bytes)
    <<c:utf8_codepoint, _:bytes>> ->
      Error(UnexpectedToken(found: string.from_utf_codepoints([c])))
    _ -> Error(InvalidUtf8)
  }
}

type StringScan {
  FoundQuote(content_len: Int, after: BitArray)
  FoundEscape(prefix_len: Int, after: BitArray)
  FoundControlChar
  NoClosingQuote
}

fn scan_string(bytes: BitArray, n: Int) -> StringScan {
  case bytes {
    <<0x22, rest:bytes>> -> FoundQuote(n, rest)
    <<0x5c, rest:bytes>> -> FoundEscape(n, rest)
    <<c, rest:bytes>> ->
      case c < 0x20 {
        True -> FoundControlChar
        False -> scan_string(rest, n + 1)
      }
    _ -> NoClosingQuote
  }
}

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

fn parse_escape(
  bytes: BitArray,
  acc: StringTree,
) -> Result(#(String, BitArray), JsonParseError) {
  case bytes {
    <<>> -> Error(UnterminatedEscape)
    <<0x22, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "\""))
    <<0x5c, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "\\"))
    <<0x2f, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "/"))
    <<0x62, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "\u{0008}"))
    <<0x66, rest:bytes>> ->
      parse_string_content(rest, string_tree.append(acc, "\u{000C}"))
    <<0x6e, rest:bytes>> ->
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

fn hex_digit(byte: Int) -> Option(Int) {
  case byte {
    b if b >= 0x30 && b <= 0x39 -> Some(b - 0x30)
    b if b >= 0x41 && b <= 0x46 -> Some(b - 0x41 + 10)
    b if b >= 0x61 && b <= 0x66 -> Some(b - 0x61 + 10)
    _ -> None
  }
}

fn decode_unicode_escape(
  bytes: BitArray,
) -> Result(#(String, BitArray), JsonParseError) {
  use #(cp, rest) <- result.try(parse_unicode_escape(bytes))
  case cp >= 0xd800 && cp <= 0xdbff {
    True ->
      case parse_low_surrogate(rest) {
        Some(#(low, rest)) ->
          codepoint_to_string(
            0x10000 + { cp - 0xd800 } * 1024 + { low - 0xdc00 },
          )
          |> result.map(fn(s) { #(s, rest) })
        None -> Ok(#("\u{FFFD}", rest))
      }
    False ->
      case cp >= 0xdc00 && cp <= 0xdfff {
        True -> Ok(#("\u{FFFD}", rest))
        False -> codepoint_to_string(cp) |> result.map(fn(s) { #(s, rest) })
      }
  }
}

fn parse_low_surrogate(bytes: BitArray) -> Option(#(Int, BitArray)) {
  case bytes {
    <<0x5c, 0x75, rest:bytes>> ->
      case parse_unicode_escape(rest) {
        Ok(#(low, rest)) if low >= 0xdc00 && low <= 0xdfff -> Some(#(low, rest))
        _ -> None
      }
    _ -> None
  }
}

fn codepoint_to_string(codepoint: Int) -> Result(String, JsonParseError) {
  string.utf_codepoint(codepoint)
  |> result.map(fn(cp) { string.from_utf_codepoints([cp]) })
  |> result.replace_error(InvalidCodepoint)
}

type NumberSpan {
  NumberSpan(int_len: Int, frac_len: Int, exp_len: Int)
}

fn parse_number(
  bytes: BitArray,
) -> Result(#(JsonValue, BitArray), JsonParseError) {
  case scan_number(bytes) {
    Ok(span) -> {
      let len = span.int_len + span.frac_len + span.exp_len
      use num_str <- result.map(take_string(bytes, len))
      #(JsonNumber(number_span_to_num(num_str, span)), drop_bytes(bytes, len))
    }
    Error(Nil) -> {
      use raw <- result.try(take_string(bytes, count_number_bytes(bytes, 0)))
      Error(InvalidNumber(raw:))
    }
  }
}

fn scan_number(bytes: BitArray) -> Result(NumberSpan, Nil) {
  let #(bytes, sign_len) = case bytes {
    <<0x2d, rest:bytes>> -> #(rest, 1)
    _ -> #(bytes, 0)
  }
  use #(bytes, digits) <- result.try(scan_integer_digits(bytes))
  use #(bytes, frac_len) <- result.try(scan_fraction(bytes))
  use exp_len <- result.map(scan_exponent(bytes))
  NumberSpan(int_len: sign_len + digits, frac_len:, exp_len:)
}

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

fn scan_fraction(bytes: BitArray) -> Result(#(BitArray, Int), Nil) {
  case bytes {
    <<0x2e, rest:bytes>> ->
      case scan_digits(rest, 0) {
        #(_, 0) -> Error(Nil)
        #(rest, n) -> Ok(#(rest, 1 + n))
      }
    _ -> Ok(#(bytes, 0))
  }
}

fn scan_exponent(bytes: BitArray) -> Result(Int, Nil) {
  case bytes {
    <<e, rest:bytes>> if e == 0x65 || e == 0x45 -> {
      let #(rest, sign_len) = case rest {
        <<s, tail:bytes>> if s == 0x2b || s == 0x2d -> #(tail, 1)
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

fn scan_digits(bytes: BitArray, n: Int) -> #(BitArray, Int) {
  case bytes {
    <<b, rest:bytes>> if b >= 0x30 && b <= 0x39 -> scan_digits(rest, n + 1)
    _ -> #(bytes, n)
  }
}

fn count_number_bytes(bytes: BitArray, n: Int) -> Int {
  case bytes {
    <<b, rest:bytes>>
      if b == 0x2d
      || b == 0x2b
      || b == 0x2e
      || b == 0x65
      || b == 0x45
      || b >= 0x30
      && b <= 0x39
    -> count_number_bytes(rest, n + 1)
    _ -> n
  }
}

fn number_span_to_num(s: String, span: NumberSpan) -> JsNum {
  case span.frac_len > 0, span.exp_len > 0 {
    _, _ -> rt_val.string_to_number(s)
  }
}

fn parse_array(
  bytes: BitArray,
  acc: List(JsonValue),
) -> Result(#(JsonValue, BitArray), JsonParseError) {
  let bytes = skip_whitespace(bytes)
  case bytes {
    <<>> -> Error(UnterminatedArray)
    <<0x5d, rest:bytes>> -> Ok(#(JsonArray(list.reverse(acc)), rest))
    _ -> {
      let bytes = case acc {
        [] -> Ok(bytes)
        _ ->
          case bytes {
            <<0x2c, rest:bytes>> -> Ok(skip_whitespace(rest))
            _ -> Error(Expected(what: "',' or ']'", in_: "array"))
          }
      }
      use bytes <- result.try(bytes)
      use #(val, rest) <- result.try(parse_value(bytes))
      parse_array(rest, [val, ..acc])
    }
  }
}

fn parse_object(
  bytes: BitArray,
  acc: List(#(String, JsonValue)),
) -> Result(#(JsonValue, BitArray), JsonParseError) {
  let bytes = skip_whitespace(bytes)
  case bytes {
    <<>> -> Error(UnterminatedObject)
    <<0x7d, rest:bytes>> -> Ok(#(JsonObject(list.reverse(acc)), rest))
    _ -> {
      let bytes = case acc {
        [] -> Ok(bytes)
        _ ->
          case bytes {
            <<0x2c, rest:bytes>> -> Ok(skip_whitespace(rest))
            _ -> Error(Expected(what: "',' or '}'", in_: "object"))
          }
      }
      use bytes <- result.try(bytes)
      use rest <- result.try(case skip_whitespace(bytes) {
        <<0x22, rest:bytes>> -> Ok(rest)
        _ -> Error(Expected(what: "string key", in_: "object"))
      })
      use #(key, rest) <- result.try(parse_string(rest))
      use rest <- result.try(case skip_whitespace(rest) {
        <<0x3a, rest:bytes>> -> Ok(rest)
        _ -> Error(Expected(what: "':' after key", in_: "object"))
      })
      use #(val, rest) <- result.try(parse_value(rest))
      parse_object(rest, [#(key, val), ..acc])
    }
  }
}

fn take_string(bytes: BitArray, len: Int) -> Result(String, JsonParseError) {
  case bit_array.slice(bytes, 0, len) {
    Ok(slice) -> bit_array.to_string(slice) |> result.replace_error(InvalidUtf8)
    Error(Nil) -> Error(UnexpectedEnd)
  }
}

fn drop_bytes(bytes: BitArray, n: Int) -> BitArray {
  case bit_array.slice(bytes, n, bit_array.byte_size(bytes) - n) {
    Ok(rest) -> rest
    Error(Nil) -> <<>>
  }
}

/// Materialize a parsed JsonValue onto the JS heap.
fn materialize(st: Agent, val: JsonValue) -> #(JsVal, Agent) {
  case val {
    JsonNull -> #(mk_null(), st)
    JsonBool(b) -> #(mk_bool(b), st)
    JsonNumber(n) -> #(mk_number(n), st)
    JsonString(s) -> #(mk_string(s), st)
    JsonArray(items) -> {
      let #(elems, st) =
        list.fold(items, #([], st), fn(acc, item) {
          let #(vs, st) = acc
          let #(v, st) = materialize(st, item)
          #([v, ..vs], st)
        })
      let #(h, st) = realm_ops.alloc_array(st, list.reverse(elems))
      #(mk_object(h), st)
    }
    JsonObject(entries) -> {
      let obj_proto = st.realm.object.prototype
      let #(pairs, st) =
        list.fold(entries, #([], st), fn(acc, entry) {
          let #(ps, st) = acc
          let #(k, jv) = entry
          let #(v, st) = materialize(st, jv)
          #([#(k, v), ..ps], st)
        })
      let #(h, st) = common.alloc_pojo(st, obj_proto, list.reverse(pairs))
      #(mk_object(h), st)
    }
  }
}

// ============================================================================
// JSON.rawJSON / JSON.isRawJSON
// ============================================================================

fn json_raw_json(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  let #(json_str, st) =
    rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  case validate_raw_json_text(bit_array.from_string(json_str)) {
    Error(e) -> rt_val.t_throw_syntax_error(st, json_error_message(e))
    Ok(Nil) -> {
      // Frozen null-proto {rawJSON: json_str}. 2core's ObjKind has no
      // RawJsonObject slot; the [[IsRawJSON]] check reads back the frozen
      // null-proto shape + own "rawJSON" data property.
      let #(prop, st) = common.data_prop(st, mk_string(json_str))
      let #(h, st) =
        rt_store.t_cell_new(
          st,
          SObject(
            kind: Ordinary,
            proto: None,
            props: dict.from_list([
              #(Named("rawJSON"), prop),
            ]),
            symbol_props: [],
            elements: rt_types.NoElements,
            extensible: False,
          ),
        )
      #(mk_object(h), st)
    }
  }
}

fn validate_raw_json_text(bytes: BitArray) -> Result(Nil, JsonParseError) {
  use Nil <- result.try(case bit_array.byte_size(bytes) {
    0 -> Error(RawJsonEmpty)
    _ ->
      case first_byte_is_ws(bytes) || last_byte_is_ws(bytes) {
        True -> Error(RawJsonSurroundingWhitespace)
        False -> Ok(Nil)
      }
  })
  use #(parsed, rest) <- result.try(parse_value(bytes))
  use Nil <- result.try(case skip_whitespace(rest) {
    <<>> -> Ok(Nil)
    _ -> Error(TrailingContent)
  })
  case parsed {
    JsonArray(_) | JsonObject(_) -> Error(RawJsonNotPrimitive)
    _ -> Ok(Nil)
  }
}

fn is_json_ws_byte(b: Int) -> Bool {
  b == 0x09 || b == 0x0a || b == 0x0d || b == 0x20
}

fn first_byte_is_ws(bytes: BitArray) -> Bool {
  case bytes {
    <<b, _:bytes>> -> is_json_ws_byte(b)
    _ -> False
  }
}

fn last_byte_is_ws(bytes: BitArray) -> Bool {
  case bit_array.slice(bytes, bit_array.byte_size(bytes) - 1, 1) {
    Ok(<<b>>) -> is_json_ws_byte(b)
    _ -> False
  }
}

fn json_is_raw_json(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  #(mk_bool(is_raw_json(st, helpers.arg_at(args, 0))), st)
}

fn is_raw_json(st: Agent, v: JsVal) -> Bool {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: Ordinary, proto: None, extensible: False, props:, ..) ->
          case dict.get(props, Named("rawJSON")) {
            Ok(rt_types.DataProperty(..)) -> True
            _ -> False
          }
        _ -> False
      }
    _ -> False
  }
}

fn raw_json_text(st: Agent, v: JsVal) -> Option(String) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: Ordinary, proto: None, extensible: False, props:, ..) ->
          case dict.get(props, Named("rawJSON")) {
            Ok(rt_types.DataProperty(value:, ..)) ->
              case classify(value) {
                KStr(s) -> Some(s)
                _ -> None
              }
            _ -> None
          }
        _ -> None
      }
    _ -> None
  }
}

// ============================================================================
// JSON.stringify(value [, replacer [, space]])
// ============================================================================

type Replacer {
  NoReplacer
  ReplacerFn(f: JsVal)
  PropertyList(names: List(String))
}

type StringifyCtx {
  StringifyCtx(replacer: Replacer, gap: String)
}

const circular_msg = "Converting circular structure to JSON"

fn json_stringify(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  let val = helpers.first_arg_or_undefined(args)
  let replacer_arg = helpers.arg_at(args, 1)
  let space = helpers.arg_at(args, 2)
  // Step 4: ReplacerFunction / PropertyList.
  let #(replacer, st) = build_replacer(st, replacer_arg)
  // Steps 5-8: gap.
  let #(gap, st) = compute_gap(st, space)
  // Steps 9-11: wrapper = { "": value }.
  let #(wrapper, st) = alloc_holder(st, val)
  let ctx = StringifyCtx(replacer:, gap:)
  // Step 12.
  case serialize_property(st, ctx, [], "", "", wrapper) {
    #(Some(s), st) -> #(mk_string(s), st)
    #(None, st) -> #(mk_undefined(), st)
  }
}

fn build_replacer(st: Agent, replacer: JsVal) -> #(Replacer, Agent) {
  case classify(replacer) {
    KHandle(h) ->
      case rt_call.is_callable(st, replacer) {
        True -> #(ReplacerFn(replacer), st)
        False ->
          case is_array_handle(st, h) {
            False -> #(NoReplacer, st)
            True -> {
              let #(len, st) = length_of_array_like(st, h)
              let #(items, st) =
                collect_property_list(st, h, 0, len, set.new(), [])
              #(PropertyList(items), st)
            }
          }
      }
    _ -> #(NoReplacer, st)
  }
}

fn collect_property_list(
  st: Agent,
  h: Handle,
  k: Int,
  len: Int,
  seen: Set(String),
  acc: List(String),
) -> #(List(String), Agent) {
  case k >= len {
    True -> #(list.reverse(acc), st)
    False -> {
      let #(v, st) =
        rt_obj.t_get_prop(st, mk_object(h), StringKey(index_key(k)))
      let #(item, st) = replacer_item(st, v)
      case item {
        Some(s) ->
          case set.contains(seen, s) {
            True -> collect_property_list(st, h, k + 1, len, seen, acc)
            False ->
              collect_property_list(st, h, k + 1, len, set.insert(seen, s), [
                s,
                ..acc
              ])
          }
        None -> collect_property_list(st, h, k + 1, len, seen, acc)
      }
    }
  }
}

fn replacer_item(st: Agent, v: JsVal) -> #(Option(String), Agent) {
  case classify(v) {
    KStr(s) -> #(Some(s), st)
    KNum(_) -> {
      let #(s, st) = rt_val.t_to_string(st, v)
      #(Some(s), st)
    }
    KHandle(h) ->
      case obj_kind(st, h) {
        Some(StringObj(_)) | Some(NumberObj(_)) -> {
          let #(s, st) = rt_val.t_to_string(st, v)
          #(Some(s), st)
        }
        _ -> #(None, st)
      }
    _ -> #(None, st)
  }
}

fn compute_gap(st: Agent, space: JsVal) -> #(String, Agent) {
  // Step 5: unwrap Number/String wrapper objects.
  let #(space, st) = case classify(space) {
    KHandle(h) ->
      case obj_kind(st, h) {
        Some(NumberObj(_)) -> {
          let #(n, st) = rt_val.t_to_number(st, space)
          #(mk_number(n), st)
        }
        Some(StringObj(_)) -> {
          let #(s, st) = rt_val.t_to_string(st, space)
          #(mk_string(s), st)
        }
        _ -> #(space, st)
      }
    _ -> #(space, st)
  }
  let gap = case classify(space) {
    KNum(n) -> {
      let mv = int.min(10, rt_val.jsnum_to_integer_or_infinity(n))
      case mv < 1 {
        True -> ""
        False -> string.repeat(" ", mv)
      }
    }
    KStr(s) ->
      case string.length(s) <= 10 {
        True -> s
        False -> string.slice(s, 0, 10)
      }
    _ -> ""
  }
  #(gap, st)
}

/// SerializeJSONProperty (§25.5.2.1).
fn serialize_property(
  st: Agent,
  ctx: StringifyCtx,
  stack: List(Int),
  indent: String,
  key: String,
  holder: Handle,
) -> #(Option(String), Agent) {
  // Step 1: value = ? Get(holder, key).
  let #(val, st) =
    rt_obj.t_get_prop(
      st,
      mk_object(holder),
      StringKey(rt_types.canonical_key(key)),
    )
  // Step 2: toJSON — for Objects and BigInt.
  let #(val, st) = case classify(val) {
    KHandle(_) | KBig(_) -> {
      let #(to_json, st) =
        rt_obj.t_get_prop(st, val, StringKey(Named("toJSON")))
      case rt_call.is_callable(st, to_json) {
        True -> rt_call.t_call_checked(st, to_json, val, [mk_string(key)])
        False -> #(val, st)
      }
    }
    _ -> #(val, st)
  }
  // Step 3: ReplacerFunction.
  let #(val, st) = case ctx.replacer {
    ReplacerFn(rf) ->
      rt_call.t_call_checked(st, rf, mk_object(holder), [
        mk_string(key),
        val,
      ])
    NoReplacer | PropertyList(_) -> #(val, st)
  }
  // Step 4.e: [[IsRawJSON]] box → verbatim.
  case raw_json_text(st, val) {
    Some(text) -> #(Some(text), st)
    None -> {
      // Step 4: unwrap wrapper objects.
      let #(val, st) = case classify(val) {
        KHandle(h) ->
          case obj_kind(st, h) {
            Some(NumberObj(_)) -> {
              let #(n, st) = rt_val.t_to_number(st, val)
              #(mk_number(n), st)
            }
            Some(StringObj(_)) -> {
              let #(s, st) = rt_val.t_to_string(st, val)
              #(mk_string(s), st)
            }
            Some(BooleanObj(b)) -> #(mk_bool(b), st)
            Some(BigIntObj(bi)) -> #(rt_types.mk_bigint(bi), st)
            _ -> #(val, st)
          }
        _ -> #(val, st)
      }
      // Steps 5-12: dispatch.
      case classify(val) {
        KNull -> #(Some("null"), st)
        KBool(True) -> #(Some("true"), st)
        KBool(False) -> #(Some("false"), st)
        KStr(s) -> #(Some(stringify_string(s)), st)
        KNum(n) ->
          case n {
            JInt(_) | JFloat(_) -> #(Some(rt_val.jsnum_to_string(n)), st)
            JNan | JPosInf | JNegInf -> #(Some("null"), st)
          }
        KBig(_) ->
          rt_val.t_throw_type_error(st, "Do not know how to serialize a BigInt")
        KHandle(h) ->
          case rt_call.is_callable(st, val) {
            True -> #(None, st)
            False ->
              case is_array_handle(st, h) {
                True -> {
                  let #(s, st) = serialize_array(st, ctx, stack, indent, h)
                  #(Some(s), st)
                }
                False -> {
                  let #(s, st) = serialize_object(st, ctx, stack, indent, h)
                  #(Some(s), st)
                }
              }
          }
        KUndef | KSym(_) | rt_types.KTdz -> #(None, st)
      }
    }
  }
}

fn serialize_object(
  st: Agent,
  ctx: StringifyCtx,
  stack: List(Int),
  indent: String,
  h: Handle,
) -> #(String, Agent) {
  case list.contains(stack, h.id) {
    True -> rt_val.t_throw_type_error(st, circular_msg)
    False -> {
      let stack = [h.id, ..stack]
      let step_indent = indent <> ctx.gap
      let #(keys, st) = case ctx.replacer {
        PropertyList(names) -> #(names, st)
        NoReplacer | ReplacerFn(_) -> enumerable_string_keys(st, h)
      }
      let #(partial, st) =
        serialize_members(st, ctx, stack, step_indent, h, keys, [])
      #(finalize_brackets(partial, ctx.gap, step_indent, indent, "{", "}"), st)
    }
  }
}

fn serialize_members(
  st: Agent,
  ctx: StringifyCtx,
  stack: List(Int),
  step_indent: String,
  h: Handle,
  keys: List(String),
  acc: List(String),
) -> #(List(String), Agent) {
  case keys {
    [] -> #(list.reverse(acc), st)
    [k, ..rest] -> {
      let #(str_p, st) = serialize_property(st, ctx, stack, step_indent, k, h)
      case str_p {
        Some(s) -> {
          let sep = case ctx.gap {
            "" -> ":"
            _ -> ": "
          }
          serialize_members(st, ctx, stack, step_indent, h, rest, [
            stringify_string(k) <> sep <> s,
            ..acc
          ])
        }
        None -> serialize_members(st, ctx, stack, step_indent, h, rest, acc)
      }
    }
  }
}

fn serialize_array(
  st: Agent,
  ctx: StringifyCtx,
  stack: List(Int),
  indent: String,
  h: Handle,
) -> #(String, Agent) {
  case list.contains(stack, h.id) {
    True -> rt_val.t_throw_type_error(st, circular_msg)
    False -> {
      let stack = [h.id, ..stack]
      let step_indent = indent <> ctx.gap
      let #(len, st) = length_of_array_like(st, h)
      let #(partial, st) =
        serialize_elements(st, ctx, stack, step_indent, h, 0, len, [])
      #(finalize_brackets(partial, ctx.gap, step_indent, indent, "[", "]"), st)
    }
  }
}

fn serialize_elements(
  st: Agent,
  ctx: StringifyCtx,
  stack: List(Int),
  step_indent: String,
  h: Handle,
  i: Int,
  len: Int,
  acc: List(String),
) -> #(List(String), Agent) {
  case i >= len {
    True -> #(list.reverse(acc), st)
    False -> {
      let #(str_p, st) =
        serialize_property(st, ctx, stack, step_indent, int.to_string(i), h)
      let s = option.unwrap(str_p, "null")
      serialize_elements(st, ctx, stack, step_indent, h, i + 1, len, [s, ..acc])
    }
  }
}

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

// ── QuoteJSONString (§25.5.2.3) ────────────────────────────────────────────

type EscapeScan {
  FoundEscapable(n: Int, byte: Int, rest: BitArray)
  AllClean
}

fn scan_escapable(bytes: BitArray, n: Int) -> EscapeScan {
  case bytes {
    <<0x22, rest:bytes>> -> FoundEscapable(n, 0x22, rest)
    <<0x5c, rest:bytes>> -> FoundEscapable(n, 0x5c, rest)
    <<c, rest:bytes>> if c < 0x20 -> FoundEscapable(n, c, rest)
    <<_, rest:bytes>> -> scan_escapable(rest, n + 1)
    _ -> AllClean
  }
}

fn stringify_string(s: String) -> String {
  let bytes = <<s:utf8>>
  case scan_escapable(bytes, 0) {
    AllClean -> "\"" <> s <> "\""
    found -> "\"" <> escape_from(found, bytes, string_tree.new()) <> "\""
  }
}

fn escape_from(scan: EscapeScan, bytes: BitArray, acc: StringTree) -> String {
  case scan {
    AllClean ->
      string_tree.to_string(append_span(acc, bytes, bit_array.byte_size(bytes)))
    FoundEscapable(n, byte, rest) -> {
      let acc =
        string_tree.append(append_span(acc, bytes, n), escape_byte(byte))
      escape_from(scan_escapable(rest, 0), rest, acc)
    }
  }
}

fn escape_byte(byte: Int) -> String {
  case byte {
    0x22 -> "\\\""
    0x5c -> "\\\\"
    0x08 -> "\\b"
    0x09 -> "\\t"
    0x0a -> "\\n"
    0x0c -> "\\f"
    0x0d -> "\\r"
    _ -> unicode_escape(byte)
  }
}

fn append_span(acc: StringTree, bytes: BitArray, n: Int) -> StringTree {
  case n {
    0 -> acc
    _ -> {
      let assert Ok(chunk) =
        bit_array.slice(bytes, 0, n) |> result.try(bit_array.to_string)
      string_tree.append(acc, chunk)
    }
  }
}

fn unicode_escape(code: Int) -> String {
  let assert Ok(hex) = int.to_base_string(code, 16)
  "\\u" <> string.pad_start(string.lowercase(hex), to: 4, with: "0")
}

// ── inline §7.3 helpers not yet on rt_obj ────────────────────────────────

fn obj_kind(st: Agent, h: Handle) -> Option(rt_types.ObjKind) {
  case rt_store.t_cell_get(st, h) {
    SObject(kind:, ..) -> Some(kind)
    // h-shape-slowpath-compat: shaped objects are always Ordinary-kind.
    SShapedObject(..) -> Some(Ordinary)
    _ -> None
  }
}

fn is_array_handle(st: Agent, h: Handle) -> Bool {
  case obj_kind(st, h) {
    Some(ArrayObj(..)) -> True
    Some(rt_types.ProxyObj(target:, revoked: False, ..)) ->
      is_array_handle(st, target)
    _ -> False
  }
}

fn length_of_array_like(st: Agent, h: Handle) -> #(Int, Agent) {
  let #(len_v, st) =
    rt_obj.t_get_prop(st, mk_object(h), StringKey(Named("length")))
  rt_val.t_to_length(st, len_v)
}

/// EnumerableOwnPropertyNames(obj, key) — string-keyed own enumerable props.
fn enumerable_string_keys(st: Agent, h: Handle) -> #(List(String), Agent) {
  let #(keys, st) = rt_obj.t_own_keys(st, h)
  let names =
    list.filter_map(keys, fn(ok) {
      case ok {
        StringKey(pk) ->
          case rt_obj.t_get_own_property(st, h, ok) {
            Some(prop) ->
              case rt_types.prop_enumerable(prop) {
                True -> Ok(rt_types.key_to_text(pk))
                False -> Error(Nil)
              }
            None -> Error(Nil)
          }
        rt_types.SymbolKey(_) -> Error(Nil)
      }
    })
  #(names, st)
}
