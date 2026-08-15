//// The `JSON` global namespace (ES2024 §25.5) plus
//// proposal-json-parse-with-source: the `JSON.parse` reviver receives a
//// third `context` argument whose `source` is the exact literal text of an
//// unmodified primitive, and `JSON.rawJSON` / `JSON.isRawJSON` box verbatim
//// JSON text for `JSON.stringify`.
////
//// Return-tuple order is `#(JsVal, Agent)` (R1).
////
//// Realms: everything a JSON builtin allocates — thrown errors, the parsed
//// objects, the reviver's `context` — comes from the intrinsics of the realm
//// the FUNCTION belongs to (the `realm` id its token carries), not the realm
//// that happens to be running: `otherRealm.JSON.parse('{')` throws
//// `otherRealm.SyntaxError`. `dispatch` enters that realm for the body. User
//// callbacks the builtin re-enters — a reviver, a replacer, a `toJSON` — run
//// back in the CALLER's realm (`call_in_caller_realm`), so what their code
//// creates belongs to the running realm as it did before any realm handling.

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/realm as rt_realm
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsNum, type JsVal, type JsonNative, ArrayObj,
  BigIntObj, BooleanObj, JFloat, JInt, JNan, JNegInf, JPosInf, JsonIsRawJson,
  JsonN, JsonParse, JsonRawJson, JsonStringify, KBig, KBool, KHandle, KNull,
  KNum, KStr, KSym, KUndef, Named, NumberObj, Ordinary, RawJsonObj, SObject,
  SShapedObject, StringKey, StringObj, classify, index_key, mk_bool, mk_null,
  mk_number, mk_object, mk_string, mk_undefined,
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

/// Set up the JSON global object of realm `realm`.
/// JSON is NOT a constructor — it's a plain object with static methods
/// (like Math), per ES2024 §25.5.
pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
  realm: Int,
) -> #(Handle, Agent) {
  let #(methods, st) =
    common.alloc_methods(st, function_proto, [
      #("parse", JsonN(JsonParse(realm:)), 2),
      #("stringify", JsonN(JsonStringify(realm:)), 3),
      #("rawJSON", JsonN(JsonRawJson(realm:)), 1),
      #("isRawJSON", JsonN(JsonIsRawJson(realm:)), 1),
    ])

  common.init_namespace(st, object_proto, "JSON", methods)
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for JSON native functions. The body runs in the
/// function's own realm; `caller` (the realm running at entry) is threaded
/// to every user-callback site. Attribution never looks at the receiver:
/// `JSON.parse.call(otherRealm.JSON, '{}')` is still this realm's parse.
pub fn dispatch(
  st: Agent,
  native: JsonNative,
  _this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let caller = st.realm.id
  use st <- rt_realm.with_realm(st, native.realm)
  case native {
    JsonParse(_) -> json_parse(args, caller, st)
    JsonStringify(_) -> json_stringify(args, caller, st)
    JsonRawJson(_) -> json_raw_json(args, st)
    JsonIsRawJson(_) -> json_is_raw_json(args, st)
  }
}

/// Re-enter user code (a reviver / replacer / toJSON) with realm `caller` —
/// the realm that was running when the JSON builtin was invoked — current
/// again, coming back to the JSON function's own realm afterwards.
fn call_in_caller_realm(
  st: Agent,
  caller: Int,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st <- rt_realm.with_realm(st, caller)
  rt_call.t_call_checked(st, callee, this, args)
}

// ============================================================================
// JSON.parse(text [, reviver])
// ============================================================================

/// ES2024 §25.5.1 JSON.parse ( text [ , reviver ] )
///
/// Steps:
///   1. Let jsonString be ? ToString(text).
///   2. Parse jsonString as a JSON text as specified in ECMA-404.
///   3. If the parse fails, throw a SyntaxError exception.
///   4-6. Materialize the parse result as `unfiltered`.
///   7-9. If IsCallable(reviver): root = OrdinaryObjectCreate(%Object.prototype%),
///        CreateDataPropertyOrThrow(root, "", unfiltered), then return
///        ? InternalizeJSONProperty(root, "", reviver, the root parse node).
///   10. Otherwise return unfiltered.
fn json_parse(args: List(JsVal), caller: Int, st: Agent) -> #(JsVal, Agent) {
  // Step 1: ToString(text).
  let #(json_str, st) =
    rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  // Step 2: Parse as JSON text — walk the UTF-8 bytes directly.
  // On BEAM a String already is a binary, so this conversion is free, and
  // byte pattern matching avoids per-character grapheme clustering (which
  // goes through unicode_util:gc and allocates a cons cell + sub-binary
  // per character).
  let bytes = bit_array.from_string(json_str)
  case parse_value(bytes) {
    // Step 3: If parse fails, throw SyntaxError.
    Error(e) -> rt_val.t_throw_syntax_error(st, json_error_message(e))
    Ok(#(val, rest)) ->
      // After parsing, skip trailing whitespace and ensure nothing else.
      case skip_whitespace(rest) {
        <<>> -> {
          // Successfully parsed — materialize the value on the heap.
          let #(record, st) = materialize(st, val)
          let unfiltered = record_value(record)
          // Steps 7-10: run the reviver, if one was supplied and is callable.
          case helpers.list_at(args, 1) {
            Some(reviver) ->
              case rt_call.is_callable(st, reviver) {
                False -> #(unfiltered, st)
                True -> {
                  let #(root, st) = alloc_holder(st, unfiltered)
                  let ctx = ReviveCtx(reviver:, caller:)
                  internalize_json_property(st, ctx, root, "", Some(record))
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
///
/// Bottom-up, exactly as the spec: when `holder[name]` is an object its
/// children are revived first (an `undefined` result deletes the child, any
/// other result replaces it), and only then is the reviver called for
/// `holder[name]` itself, with `holder` as `this`. Abrupt completions from any
/// Get / Delete / CreateDataProperty / reviver call propagate out of
/// JSON.parse.
///
/// The ES2025 json-parse-with-source amendment adds a third reviver argument
/// and threads the parse tree alongside the walk (`node` here is the spec's
/// JSON Parse Record for `holder[name]`, `empty` when there is none):
///
///   1. Let val be ? Get(holder, name).
///   2. Let context be OrdinaryObjectCreate(%Object.prototype%).
///   3. If node is not empty and node's [[Value]] is val (i.e. no earlier
///      reviver call replaced it), then
///      a. If val is a primitive that came from a literal, perform
///         ! CreateDataPropertyOrThrow(context, "source", the literal's exact
///         source text) — writable, enumerable, configurable.
///      b. The child records of `node` are the ones handed to the recursion.
///      Otherwise `context` gets NO own property and the children get no
///      records: an object/array whose slot was overwritten no longer
///      corresponds to the source text.
///   4-5. Recurse over the elements/keys of `val` as before, passing each
///        child's record (array children by index, object children by key,
///        keys a reviver added get `empty`).
///   6. Return ? Call(reviver, holder, « name, val, context »).
///
/// Recursion here is ordinary Gleam recursion; the re-entrant JS calls go
/// through `call_in_caller_realm`, the same convention `serialize_property`
/// uses to invoke `toJSON` and the replacer.
fn internalize_json_property(
  st: Agent,
  ctx: ReviveCtx,
  holder: Handle,
  name: String,
  node: Option(ParseRecord),
) -> #(JsVal, Agent) {
  // Step 1: val = ? Get(holder, name).
  let #(val, st) =
    rt_obj.t_get_prop(
      st,
      mk_object(holder),
      StringKey(rt_types.canonical_key(name)),
    )
  // Step 3: the parse record only describes `val` while `val` is still the very
  // value that literal materialized into. An earlier reviver call that
  // overwrote this slot leaves the record stale, and a stale record must leak
  // neither a `source` nor its children's records — the replacement's children
  // did not come from that source text.
  let node = fresh_record(node, val)
  // Steps 4-5: if val is an Object, revive its children in place first.
  let st = case classify(val) {
    KHandle(h) ->
      case is_array_handle(st, h) {
        // Step 5.b: indices 0..len-1, len from LengthOfArrayLike.
        True -> {
          let #(len, st) = length_of_array_like(st, h)
          internalize_elements(st, ctx, h, 0, len, record_elements(node))
        }
        // Step 5.c: EnumerableOwnPropertyNames(val, key).
        False -> {
          let #(keys, st) = enumerable_string_keys(st, h)
          internalize_keys(st, ctx, h, keys, record_members(node))
        }
      }
    _ -> st
  }
  // Steps 2-3: the `context` object, carrying `source` only for an unmodified
  // primitive literal.
  let #(context, st) = alloc_context(st, record_source(node))
  // Step 6: return ? Call(reviver, holder, « name, val, context »), back in
  // the realm that was running when JSON.parse was invoked.
  call_in_caller_realm(st, ctx.caller, ctx.reviver, mk_object(holder), [
    mk_string(name),
    val,
    mk_object(context),
  ])
}

/// The reviver walk's fixed context: the reviver and the id of the realm
/// JSON.parse was called from, which every reviver call re-enters.
type ReviveCtx {
  ReviveCtx(reviver: JsVal, caller: Int)
}

/// Step 3: keep the parse record only if it is still the record for `val` —
/// SameValue(record's [[Value]], val). For arrays and objects that is a heap
/// handle identity check, so a reviver that swapped in a *different*
/// array/object (`reviver-call-args-after-forward-modification.js`) drops the
/// record, and with it both the `source` and every child record.
fn fresh_record(node: Option(ParseRecord), val: JsVal) -> Option(ParseRecord) {
  use record <- option.then(node)
  case rt_val.same_value(record_value(record), val) {
    True -> Some(record)
    False -> None
  }
}

/// §25.5.1.1 step 5.b.iii: recurse over array indices 0..len-1 in order,
/// handing each element the parse record it was scanned from (if any).
///
/// The remaining child records travel alongside the index rather than being
/// looked up by it: the walk visits 0..len-1 in order, so popping the head each
/// step is the same record an O(i) `list_at` would find, and the whole walk
/// stays linear instead of quadratic in the array's length. Indices past the
/// records (elements a reviver added, or a `length` larger than the literal)
/// simply run out and get `None`.
fn internalize_elements(
  st: Agent,
  ctx: ReviveCtx,
  h: Handle,
  i: Int,
  len: Int,
  children: List(ParseRecord),
) -> Agent {
  case i >= len {
    True -> st
    False -> {
      let #(child, rest_children) = case children {
        [child, ..rest] -> #(Some(child), rest)
        [] -> #(None, [])
      }
      let name = int.to_string(i)
      let #(new_element, st) =
        internalize_json_property(st, ctx, h, name, child)
      let st = replace_or_delete(st, h, name, new_element)
      internalize_elements(st, ctx, h, i + 1, len, rest_children)
    }
  }
}

/// §25.5.1.1 step 5.c.iii: recurse over the object's own enumerable string
/// keys, handing each the parse record it was scanned from (a key a reviver
/// added has none). `members` is built once per object (see `record_members`),
/// so each key costs one dict lookup rather than a scan of every member.
fn internalize_keys(
  st: Agent,
  ctx: ReviveCtx,
  h: Handle,
  keys: List(String),
  members: dict.Dict(String, ParseRecord),
) -> Agent {
  case keys {
    [] -> st
    [p, ..rest] -> {
      let #(new_element, st) =
        internalize_json_property(
          st,
          ctx,
          h,
          p,
          dict.get(members, p) |> option.from_result,
        )
      let st = replace_or_delete(st, h, p, new_element)
      internalize_keys(st, ctx, h, rest, members)
    }
  }
}

/// §25.5.1.1 steps 2.b.ii.2-3 / 2.c.ii.2-3: an `undefined` result from the
/// reviver deletes the child, anything else is CreateDataProperty'd back.
/// Both spec steps are a bare `Perform ?`, so a `false` [[Delete]] /
/// [[DefineOwnProperty]] result is DISCARDED — a reviver that makes a sibling
/// key non-configurable must not turn the next replacement into a TypeError.
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

/// The spec's root holder object: OrdinaryObjectCreate(%Object.prototype%)
/// with CreateDataPropertyOrThrow(holder, "", val). Both JSON.parse
/// (§25.5.1 steps 7-8) and JSON.stringify (§25.5.2 steps 9-11) start from one.
fn alloc_holder(st: Agent, val: JsVal) -> #(Handle, Agent) {
  // The JSON function's own realm is current (the `dispatch` swap).
  common.alloc_pojo(st, st.realm.object.prototype, [#("", val)])
}

/// InternalizeJSONProperty step 2 (+3.a): OrdinaryObjectCreate(%Object.prototype%),
/// carrying a `source` own data property (writable, enumerable, configurable —
/// what CreateDataPropertyOrThrow gives) when the value came from an unmodified
/// primitive literal, and no own property at all otherwise.
///
/// This is the one place the recorded source bytes are decoded — reached only
/// with a reviver in play, so a plain `JSON.parse` never pays for it.
fn alloc_context(st: Agent, source: Option(BitArray)) -> #(Handle, Agent) {
  let props = case source {
    Some(raw) -> {
      // The slice spans a whole literal of a document that came from a Gleam
      // String, so it is valid UTF-8 by construction. Assert rather than fall
      // back — a broken invariant must not silently hand the reviver a bogus
      // `source`.
      let assert Ok(text) = bit_array.to_string(raw)
      [#("source", mk_string(text))]
    }
    None -> []
  }
  // The JSON function's own realm is current (the `dispatch` swap).
  common.alloc_pojo(st, st.realm.object.prototype, props)
}

// ── §25.5.1 scanner ─────────────────────────────────────────────────────────

/// Intermediate parsed JSON value — not yet materialized onto the JS heap.
/// We parse into this first, then walk it to create JsVals/heap objects.
///
/// Primitive nodes carry `source`: the EXACT source text of the literal they
/// were scanned from — `1.1`, `"foo"` (quotes kept, escapes left undecoded),
/// `null`. That is what the ES2025 json-parse-with-source proposal hands the
/// reviver as `context.source`; only primitives ever expose it, so arrays and
/// objects carry no source of their own.
///
/// It is kept as raw BYTES, not a String, and stays that way until a reviver
/// asks for it (`alloc_context`). `bit_array.slice` is a genuinely O(1)
/// sub-binary, but `bit_array.to_string` validates the whole slice as UTF-8 —
/// so decoding a string literal's source eagerly would re-scan every string in
/// the document, on every JSON.parse, including the overwhelmingly common calls
/// with no reviver at all, where the source is thrown away unread. Slicing here
/// and decoding there costs the byte-scanning parser nothing.
type JsonValue {
  JsonNull(source: BitArray)
  JsonBool(value: Bool, source: BitArray)
  JsonNumber(value: JsNum, source: BitArray)
  JsonString(value: String, source: BitArray)
  JsonArray(List(JsonValue))
  JsonObject(List(#(String, JsonValue)))
}

/// The proposal's JSON Parse Record: a literal node paired with the exact
/// `JsVal` it materialized into ([[Value]]) — the heap `Handle` for arrays and
/// objects — plus its children's records.
///
/// InternalizeJSONProperty compares [[Value]] against whatever is actually
/// sitting in the holder before it hands out either the `source` text or the
/// child records, so a reviver that swaps a slot for a different value cannot
/// make the source of the original literal describe the replacement.
type ParseRecord {
  /// A primitive literal: `[[Value]]` and its exact source text, still as the
  /// undecoded bytes the scanner sliced out (see `JsonValue`).
  PrimRecord(value: JsVal, source: BitArray)
  /// An array literal: the array object it produced, and its elements' records.
  ArrayRecord(value: JsVal, elements: List(ParseRecord))
  /// An object literal: the object it produced, and its members' records.
  ObjectRecord(value: JsVal, entries: List(#(String, ParseRecord)))
}

/// The record's [[Value]]: what materializing this literal produced.
fn record_value(record: ParseRecord) -> JsVal {
  case record {
    PrimRecord(value:, ..)
    | ArrayRecord(value:, ..)
    | ObjectRecord(value:, ..) -> value
  }
}

/// Step 3.a: the literal's exact source text — primitives only. Arrays and
/// objects never expose a `source`, so their reviver `context` stays empty.
fn record_source(record: Option(ParseRecord)) -> Option(BitArray) {
  case record {
    Some(PrimRecord(source:, ..)) -> Some(source)
    Some(ArrayRecord(..)) | Some(ObjectRecord(..)) | None -> None
  }
}

/// The child records of an array literal, in index order — empty for anything
/// else (a primitive, an object, or a slot with no live record at all).
/// `internalize_elements` walks them in lockstep with the indices, so no child
/// is ever looked up by index.
fn record_elements(record: Option(ParseRecord)) -> List(ParseRecord) {
  case record {
    Some(ArrayRecord(elements:, ..)) -> elements
    _ -> []
  }
}

/// The child records of an object literal, keyed by member name and built once
/// per object so `internalize_keys` pays one dict lookup per key instead of a
/// fold over every member. Duplicate keys resolve to the LAST occurrence
/// (later inserts win), matching how `materialize` resolves them.
fn record_members(
  record: Option(ParseRecord),
) -> dict.Dict(String, ParseRecord) {
  case record {
    Some(ObjectRecord(entries:, ..)) ->
      list.fold(entries, dict.new(), fn(acc, entry) {
        dict.insert(acc, entry.0, entry.1)
      })
    _ -> dict.new()
  }
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
    // "null"
    <<0x6e, 0x75, 0x6c, 0x6c, rest:bytes>> ->
      Ok(#(JsonNull(source: <<"null":utf8>>), rest))
    // "true"
    <<0x74, 0x72, 0x75, 0x65, rest:bytes>> ->
      Ok(#(JsonBool(value: True, source: <<"true":utf8>>), rest))
    // "false"
    <<0x66, 0x61, 0x6c, 0x73, 0x65, rest:bytes>> ->
      Ok(#(JsonBool(value: False, source: <<"false":utf8>>), rest))
    // '"'
    <<0x22, rest:bytes>> -> {
      use #(s, rest) <- result.try(parse_string(rest))
      // The literal's source text is everything the string scanner consumed,
      // opening quote included: `rest` is a sub-binary of `bytes`, so the byte
      // lengths differ by exactly the span, and the slice is O(1). It stays
      // undecoded — see `JsonValue`.
      let span = bit_array.byte_size(bytes) - bit_array.byte_size(rest)
      use raw <- result.map(take_bytes(bytes, span))
      #(JsonString(value: s, source: raw), rest)
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

/// Parse a JSON number: scan the leading bytes against the ECMA-404 number
/// grammar, slice the span out as a sub-binary (O(1)), and convert it.
///
/// `scan_number` has already validated the grammar, and an ECMA-404 number is
/// a strict subset of StringNumericLiteral, so `rt_val.string_to_number` is
/// total here: "-0" keeps its sign and magnitude overflow saturates to
/// ±Infinity (§7.1.4.1) instead of being misreported as a syntax error.
fn parse_number(
  bytes: BitArray,
) -> Result(#(JsonValue, BitArray), JsonParseError) {
  case scan_number(bytes) {
    Ok(span) -> {
      let len = span.int_len + span.frac_len + span.exp_len
      use num_str <- result.map(take_string(bytes, len))
      // `num_str` already IS the literal's exact source text; on BEAM a String
      // is a binary, so re-viewing it as bytes is free (no copy, no scan).
      #(
        JsonNumber(
          value: rt_val.string_to_number(num_str),
          source: bit_array.from_string(num_str),
        ),
        drop_bytes(bytes, len),
      )
    }
    // Report the whole number-looking span (e.g. "01", "1e", "-"), not just
    // the prefix that scanned cleanly.
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

/// Parse a JSON array (after the opening '[').
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

/// Parse a JSON object (after the opening '{').
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

/// Slice the first `len` bytes off `bytes` as a String.
/// O(1) on BEAM — the slice is a zero-copy sub-binary of the input.
fn take_string(bytes: BitArray, len: Int) -> Result(String, JsonParseError) {
  case bit_array.slice(bytes, 0, len) {
    Ok(slice) -> bit_array.to_string(slice) |> result.replace_error(InvalidUtf8)
    Error(Nil) -> Error(UnexpectedEnd)
  }
}

/// Slice the first `len` bytes off `bytes`, undecoded.
/// Truly O(1) — a zero-copy sub-binary, with none of the UTF-8 validation
/// `bit_array.to_string` (and hence `take_string`) walks the whole slice for.
fn take_bytes(bytes: BitArray, len: Int) -> Result(BitArray, JsonParseError) {
  bit_array.slice(bytes, 0, len) |> result.replace_error(UnexpectedEnd)
}

/// Drop the first `n` bytes of `bytes` (O(1) sub-binary).
fn drop_bytes(bytes: BitArray, n: Int) -> BitArray {
  case bit_array.slice(bytes, n, bit_array.byte_size(bytes) - n) {
    Ok(rest) -> rest
    Error(Nil) -> <<>>
  }
}

/// Materialize a parsed JsonValue onto the JS heap, returning its parse record:
/// the JsVal produced (`record_value`) plus, for arrays and objects, the
/// records of everything underneath it. The record tree is what feeds
/// InternalizeJSONProperty's `context.source`.
fn materialize(st: Agent, val: JsonValue) -> #(ParseRecord, Agent) {
  case val {
    JsonNull(source:) -> #(PrimRecord(value: mk_null(), source:), st)
    JsonBool(value: b, source:) -> #(PrimRecord(value: mk_bool(b), source:), st)
    JsonNumber(value: n, source:) -> #(
      PrimRecord(value: mk_number(n), source:),
      st,
    )
    JsonString(value: s, source:) -> #(
      PrimRecord(value: mk_string(s), source:),
      st,
    )
    JsonArray(items) -> {
      let #(elements, st) = materialize_list(st, items, [])
      // %Array.prototype% / %Object.prototype% of the JSON function's own
      // realm, which `dispatch` made current.
      let #(h, st) = realm_ops.alloc_array(st, list.map(elements, record_value))
      #(ArrayRecord(value: mk_object(h), elements:), st)
    }
    JsonObject(entries) -> {
      let #(entries, st) = materialize_object_entries(st, entries, [])
      let #(props, st) = props_from_entries(st, entries, dict.new())
      let #(h, st) =
        rt_store.t_cell_new(
          st,
          SObject(
            kind: Ordinary,
            proto: Some(st.realm.object.prototype),
            props:,
            symbol_props: [],
            elements: rt_types.NoElements,
            extensible: True,
          ),
        )
      #(ObjectRecord(value: mk_object(h), entries:), st)
    }
  }
}

fn materialize_list(
  st: Agent,
  items: List(JsonValue),
  acc: List(ParseRecord),
) -> #(List(ParseRecord), Agent) {
  case items {
    [] -> #(list.reverse(acc), st)
    [item, ..rest] -> {
      let #(record, st) = materialize(st, item)
      materialize_list(st, rest, [record, ..acc])
    }
  }
}

fn materialize_object_entries(
  st: Agent,
  entries: List(#(String, JsonValue)),
  acc: List(#(String, ParseRecord)),
) -> #(List(#(String, ParseRecord)), Agent) {
  case entries {
    [] -> #(list.reverse(acc), st)
    [#(name, val), ..rest] -> {
      let #(record, st) = materialize(st, val)
      materialize_object_entries(st, rest, [#(name, record), ..acc])
    }
  }
}

/// The object literal's own properties, keyed by CanonicalNumericIndexString
/// (`{"42":37}` is `Index(42)`, exactly what a later `Get(obj, "42")` looks
/// up). Not `dict.from_list`: duplicate JSON keys must keep the FIRST
/// occurrence's position (its `seq`) with the LAST occurrence's value, as
/// repeated CreateDataProperty on one key would.
fn props_from_entries(
  st: Agent,
  entries: List(#(String, ParseRecord)),
  acc: dict.Dict(rt_types.PropertyKey, rt_types.Property),
) -> #(dict.Dict(rt_types.PropertyKey, rt_types.Property), Agent) {
  case entries {
    [] -> #(acc, st)
    [#(name, record), ..rest] -> {
      let key = rt_types.canonical_key(name)
      let value = record_value(record)
      case dict.get(acc, key) {
        Ok(first) -> {
          let prop =
            rt_types.DataProperty(
              value:,
              writable: True,
              enumerable: True,
              configurable: True,
              seq: rt_types.prop_seq(first),
            )
          props_from_entries(st, rest, dict.insert(acc, key, prop))
        }
        Error(Nil) -> {
          let #(prop, st) = common.data_property(st, value)
          props_from_entries(st, rest, dict.insert(acc, key, prop))
        }
      }
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
      // Steps 5-8: OrdinaryObjectCreate(null, « [[IsRawJSON]] ») +
      // CreateDataPropertyOrThrow + SetIntegrityLevel(frozen): a
      // null-prototype, non-extensible object whose only own property is a
      // non-writable, non-configurable "rawJSON" string.
      let #(seq, st) = rt_store.t_next_prop_seq(st)
      let prop =
        rt_types.DataProperty(
          value: mk_string(json_str),
          writable: False,
          enumerable: True,
          configurable: False,
          seq:,
        )
      let #(h, st) =
        rt_store.t_cell_new(
          st,
          SObject(
            kind: RawJsonObj(raw: json_str),
            proto: None,
            props: dict.from_list([#(Named("rawJSON"), prop)]),
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

/// §JSON.isRawJSON step 1: the [[IsRawJSON]] internal-slot brand check.
fn is_raw_json(st: Agent, v: JsVal) -> Bool {
  option.is_some(raw_json_text(st, v))
}

/// The [[IsRawJSON]] slot's payload — the verbatim JSON source text a
/// `JSON.rawJSON` box carries — or `None` for every other value.
fn raw_json_text(st: Agent, v: JsVal) -> Option(String) {
  case classify(v) {
    KHandle(h) ->
      case obj_kind(st, h) {
        Some(RawJsonObj(raw:)) -> Some(raw)
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

/// `caller` is not the spec's: it is the id of the realm running when
/// JSON.stringify was invoked, re-entered for every `toJSON`/replacer call.
type StringifyCtx {
  StringifyCtx(replacer: Replacer, gap: String, caller: Int)
}

const circular_msg = "Converting circular structure to JSON"

fn json_stringify(
  args: List(JsVal),
  caller: Int,
  st: Agent,
) -> #(JsVal, Agent) {
  let val = helpers.first_arg_or_undefined(args)
  let replacer_arg = helpers.arg_at(args, 1)
  let space = helpers.arg_at(args, 2)
  // Step 4: ReplacerFunction / PropertyList.
  let #(replacer, st) = build_replacer(st, replacer_arg)
  // Steps 5-8: gap.
  let #(gap, st) = compute_gap(st, space)
  // Steps 9-11: wrapper = { "": value }.
  let #(wrapper, st) = alloc_holder(st, val)
  let ctx = StringifyCtx(replacer:, gap:, caller:)
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
  // Step 2: toJSON — for Objects and BigInt. It and the replacer (step 3)
  // are user code: they run back in the caller's realm.
  let #(val, st) = case classify(val) {
    KHandle(_) | KBig(_) -> {
      let #(to_json, st) =
        rt_obj.t_get_prop(st, val, StringKey(Named("toJSON")))
      case rt_call.is_callable(st, to_json) {
        True ->
          call_in_caller_realm(st, ctx.caller, to_json, val, [mk_string(key)])
        False -> #(val, st)
      }
    }
    _ -> #(val, st)
  }
  // Step 3: ReplacerFunction.
  let #(val, st) = case ctx.replacer {
    ReplacerFn(rf) ->
      call_in_caller_realm(st, ctx.caller, rf, mk_object(holder), [
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

/// §7.2.2 IsArray — pierces Proxy exotic objects to their [[ProxyTarget]]
/// (step 3.b) and throws TypeError on a revoked proxy (step 3.a).
fn is_array_handle(st: Agent, h: Handle) -> Bool {
  case obj_kind(st, h) {
    Some(ArrayObj(..)) -> True
    Some(rt_types.ProxyObj(revoked: True, ..)) ->
      rt_val.t_throw_type_error(
        st,
        "Cannot perform 'IsArray' on a proxy that has been revoked",
      )
    Some(rt_types.ProxyObj(target:, ..)) -> is_array_handle(st, target)
    _ -> False
  }
}

fn length_of_array_like(st: Agent, h: Handle) -> #(Int, Agent) {
  let #(len_v, st) =
    rt_obj.t_get_prop(st, mk_object(h), StringKey(Named("length")))
  rt_val.t_to_length(st, len_v)
}

/// EnumerableOwnProperties(obj, key) — string-keyed own enumerable props via
/// [[OwnPropertyKeys]] + per-key [[GetOwnProperty]] (both trap on a proxy).
fn enumerable_string_keys(st: Agent, h: Handle) -> #(List(String), Agent) {
  let #(keys, st) = rt_obj.t_enumerable_own_keys(st, h)
  #(list.map(keys, rt_types.key_to_text), st)
}
