import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/builtins/object as object_builtins
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/js_string
import arc/vm/key
import arc/vm/ops/coerce
import arc/vm/ops/object as objops
import arc/vm/ops/property
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type JsonNativeFn, type Ref, Finite, JsBool, JsNull, JsNumber,
  JsObject, JsString, JsUndefined, JsonIsRawJson, JsonNative, JsonParse,
  JsonRawJson, JsonStringify, NaN, NegInfinity, ObjectSlot, OrdinaryObject,
  RawJsonObject,
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
      // Each native carries this realm's %Function.prototype% as its [[Realm]]
      // marker — see `owner_realm_builtins`.
      #("parse", JsonNative(JsonParse(function_proto)), 2),
      #("stringify", JsonNative(JsonStringify(function_proto)), 3),
      #("rawJSON", JsonNative(JsonRawJson(function_proto)), 1),
      #("isRawJSON", JsonNative(JsonIsRawJson(function_proto)), 1),
    ])

  common.init_namespace(h, object_proto, "JSON", methods)
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for JSON native functions.
///
/// Everything a JSON builtin allocates — thrown errors, the parsed objects,
/// the reviver's `context` — must come from the intrinsics of the realm the
/// *function* belongs to, not the realm that happens to be running:
/// `otherRealm.JSON.rawJSON('')` throws `otherRealm.SyntaxError`, and
/// `otherRealm.JSON.parse('{}')` yields an object whose prototype is
/// `otherRealm.Object.prototype`. Arc never rebinds `state.builtins` on a
/// cross-realm call, so resolve the owning realm from the *callee* — every JSON
/// native token carries its realm's %Function.prototype% as a marker — run the
/// body with that realm's builtins installed, and restore the caller's
/// afterwards. Attribution never looks at the receiver: `JSON.parse.call(
/// otherRealm.JSON, '{}')` is still this realm's parse, and a detached
/// `otherRealm.JSON.rawJSON` still throws `otherRealm.SyntaxError`.
///
/// The swap covers only what the JSON builtin itself allocates. User callbacks
/// the builtin re-enters — a reviver, a replacer, a `toJSON` — must NOT run
/// with the JSON namespace's realm installed: the objects and errors *their*
/// code creates belong to the running realm, exactly as they did before any of
/// this realm handling existed. `call_in_caller_realm` puts the caller's
/// builtins back for the duration of each such call.
pub fn dispatch(
  native: JsonNativeFn,
  args: List(JsValue),
  _this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let caller_builtins = state.builtins
  let state = State(..state, builtins: owner_realm_builtins(state, native))
  let #(state, res) = case native {
    JsonParse(_) -> json_parse(args, caller_builtins, state)
    JsonStringify(_) -> json_stringify(args, caller_builtins, state)
    JsonRawJson(_) -> json_raw_json(args, state)
    JsonIsRawJson(_) -> json_is_raw_json(args, state)
  }
  #(State(..state, builtins: caller_builtins), res)
}

/// Re-enter user code (a reviver / replacer / toJSON) with `caller` — the realm
/// that was running when the JSON builtin was invoked — reinstalled, restoring
/// the JSON function's own realm afterwards. Whatever the callback allocates
/// must come from the running realm's intrinsics, not the JSON namespace's.
fn call_in_caller_realm(
  state: State(host),
  caller: common.Builtins,
  callee: JsValue,
  this: JsValue,
  args: List(JsValue),
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  let owner = state.builtins
  let state = State(..state, builtins: caller)
  case state.call(state, callee, this, args) {
    Ok(#(v, state)) -> Ok(#(v, State(..state, builtins: owner)))
    Error(#(e, state)) -> Error(#(e, State(..state, builtins: owner)))
  }
}

// ============================================================================
// JSON.rawJSON(text)
// ============================================================================

/// JSON.rawJSON ( text ) — proposal-json-parse-with-source.
///
/// Steps:
///   1. Let jsonString be ? ToString(text).
///   2. Throw a SyntaxError if jsonString is empty, or if its first or last
///      code unit is one of U+0009, U+000A, U+000D, U+0020.
///   3. Parse jsonString as a JSON text (ECMA-404); throw a SyntaxError if it
///      is invalid, or if its outermost value is an object or an array.
///   4-8. obj = OrdinaryObjectCreate(null, « [[IsRawJSON]] »),
///        CreateDataPropertyOrThrow(obj, "rawJSON", jsonString),
///        SetIntegrityLevel(obj, frozen), return obj.
fn json_raw_json(
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

  // Steps 2-3: validate the text.
  case validate_raw_json_text(bit_array.from_string(json_str)) {
    Error(e) -> state.syntax_error(state, json_error_message(e))
    Ok(Nil) -> {
      // Steps 4-8: the frozen, null-prototype [[IsRawJSON]] box.
      let #(state, ref) = alloc_raw_json(state, json_str)
      #(state, Ok(JsObject(ref)))
    }
  }
}

/// Steps 2-3 of JSON.rawJSON: the text must be non-empty, must not begin or
/// end with JSON whitespace, and must be a complete JSON *primitive* literal.
/// Rejects with a `JsonParseError` category — the wording is `json_error_message`'s
/// job, exactly like the scanner's own failures.
fn validate_raw_json_text(bytes: BitArray) -> Result(Nil, JsonParseError) {
  use Nil <- result.try(case bit_array.byte_size(bytes) {
    0 -> Error(RawJsonEmpty)
    _ ->
      case first_byte_is_whitespace(bytes) || last_byte_is_whitespace(bytes) {
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

fn is_json_whitespace_byte(byte: Int) -> Bool {
  byte == 0x09 || byte == 0x0a || byte == 0x0d || byte == 0x20
}

fn first_byte_is_whitespace(bytes: BitArray) -> Bool {
  case bit_array.slice(bytes, 0, 1) {
    Ok(<<first>>) -> is_json_whitespace_byte(first)
    Ok(_) -> False
    Error(Nil) -> False
  }
}

fn last_byte_is_whitespace(bytes: BitArray) -> Bool {
  let size = bit_array.byte_size(bytes)
  case bit_array.slice(bytes, size - 1, 1) {
    Ok(<<last>>) -> is_json_whitespace_byte(last)
    Ok(_) -> False
    Error(Nil) -> False
  }
}

/// OrdinaryObjectCreate(null, « [[IsRawJSON]] ») + CreateDataPropertyOrThrow +
/// SetIntegrityLevel(frozen): a null-prototype, non-extensible object whose
/// only own property is a non-writable, non-configurable "rawJSON" string.
fn alloc_raw_json(state: State(host), raw: String) -> #(State(host), Ref) {
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: RawJsonObject(raw:),
        properties: dict.from_list([
          #(
            key.canonical_key("rawJSON"),
            value.DataProperty(
              value: JsString(raw),
              writable: False,
              enumerable: True,
              configurable: False,
              seq: value.next_prop_seq(),
            ),
          ),
        ]),
        elements: elements.new(),
        prototype: None,
        symbol_properties: [],
        extensible: False,
      ),
    )
  #(State(..state, heap:), ref)
}

/// The builtins of the realm the JSON native itself belongs to — its [[Realm]],
/// recovered from the %Function.prototype% marker its token carries (unique per
/// `Builtins`, exactly as `realm.realm_of_function_proto` does for the
/// ShadowRealm methods). The receiver is irrelevant: a built-in runs in its own
/// realm however it was reached. Falls back to the running realm when the marker
/// realm was never reified in `ctx.realms` (only possible for the running realm
/// itself, which the fast path already covers).
fn owner_realm_builtins(
  state: State(host),
  native: JsonNativeFn,
) -> common.Builtins {
  let fn_proto = native.fn_proto
  case state.builtins.function.prototype == fn_proto {
    True -> state.builtins
    False ->
      // Not the running realm; ask the realm registry. A miss means the owning
      // realm was never reified as a RealmSlot, in which case it IS the running
      // realm and `state.builtins` is the right answer.
      state.builtins_of_function_proto(state, fn_proto)
      |> option.map(fn(realm) { realm.1 })
      |> option.unwrap(state.builtins)
  }
}

// ============================================================================
// JSON.isRawJSON(value)
// ============================================================================

/// proposal-json-parse-with-source §JSON.isRawJSON ( O )
///
///   1. If O is an Object and O has an [[IsRawJSON]] internal slot, return true.
///   2. Return false.
///
/// Nothing is coerced, nothing is looked up on the object, and it never throws:
/// a missing argument, a primitive, an ordinary `{ rawJSON: "123" }`, or a
/// Proxy wrapping a rawJSON box all answer false — only the box itself carries
/// the internal slot.
fn json_is_raw_json(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  #(state, Ok(JsBool(is_raw_json(state.heap, helpers.arg_at(args, 0)))))
}

/// True iff `value` is an object carrying the [[IsRawJSON]] internal slot —
/// i.e. a box produced by `JSON.rawJSON`. Used by `JSON.isRawJSON` and by
/// `JSON.stringify` to emit the raw text verbatim.
pub fn is_raw_json(h: Heap(host), value: JsValue) -> Bool {
  option.is_some(raw_json_text(h, value))
}

// ============================================================================
// JSON.parse(text)
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
fn json_parse(
  args: List(JsValue),
  caller_builtins: common.Builtins,
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
          let #(heap, record) = materialize(state.heap, state.builtins, val)
          let unfiltered = record_value(record)
          let state = State(..state, heap:)
          // Steps 7-10: run the reviver, if one was supplied and is callable.
          case helpers.list_at(args, 1) {
            Some(reviver) ->
              case helpers.is_callable(state.heap, reviver) {
                False -> #(state, Ok(unfiltered))
                True -> {
                  let #(state, root) = alloc_holder(state, unfiltered)
                  let ctx = ReviveCtx(reviver:, caller_builtins:)
                  use revived, state <- state.try_op(internalize_json_property(
                    state,
                    ctx,
                    root,
                    "",
                    Some(record),
                  ))
                  #(state, Ok(revived))
                }
              }
            None -> #(state, Ok(unfiltered))
          }
        }
        _ -> state.syntax_error(state, json_error_message(TrailingContent))
      }
    }
    // Step 2: If parse fails, throw SyntaxError
    Error(e) -> state.syntax_error(state, json_error_message(e))
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
/// Recursion here is ordinary Gleam recursion; the re-entrant JS calls go
/// through `state.call`, the same convention `serialize_property` uses to
/// invoke `toJSON` and the replacer.
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
/// Everything else is unchanged §25.5.1.1: the walk is bottom-up, an
/// `undefined` result deletes the child, any other result replaces it, and
/// abrupt completions from any Get / Delete / CreateDataProperty / reviver
/// call propagate out of JSON.parse.
///
/// Recursion here is ordinary Gleam recursion; the re-entrant JS calls go
/// through `state.call`, the same convention `serialize_property` uses to
/// invoke `toJSON` and the replacer.
fn internalize_json_property(
  state: State(host),
  ctx: ReviveCtx,
  holder: Ref,
  name: String,
  node: Option(ParseRecord),
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  // Step 1: val = ? Get(holder, name).
  use #(val, state) <- result.try(objops.get_value(
    state,
    holder,
    key.canonical_key(name),
    JsObject(holder),
  ))
  // Step 3: the parse record only describes `val` while `val` is still the very
  // value that literal materialized into. An earlier reviver call that
  // overwrote this slot leaves the record stale, and a stale record must leak
  // neither a `source` nor its children's records — the replacement's children
  // did not come from that source text.
  let node = fresh_record(node, val)
  // Steps 4-5: if val is an Object, revive its children in place first.
  use state <- result.try(case val {
    JsObject(ref) -> {
      use #(is_arr, state) <- result.try(objops.try_is_array(state, val))
      case is_arr {
        // Step 5.b: indices 0..len-1, len from LengthOfArrayLike.
        True -> {
          use #(len, state) <- result.try(length_of_array_like(state, ref))
          internalize_elements(state, ctx, ref, 0, len, record_elements(node))
        }
        // Step 5.c: EnumerableOwnPropertyNames(val, key).
        False -> {
          use #(keys, state) <- result.try(
            object_builtins.enumerable_string_keys_stateful(state, ref),
          )
          internalize_keys(state, ctx, ref, keys, record_members(node))
        }
      }
    }
    _ -> Ok(state)
  })
  // Steps 2-3: the `context` object, carrying `source` only for an unmodified
  // primitive literal.
  let #(state, context) = alloc_context(state, record_source(node))
  // Step 6: return ? Call(reviver, holder, « name, val, context »).
  call_in_caller_realm(
    state,
    ctx.caller_builtins,
    ctx.reviver,
    JsObject(holder),
    [
      JsString(name),
      val,
      JsObject(context),
    ],
  )
}

/// The reviver, plus the realm that was running when JSON.parse was called —
/// the one the reviver's own allocations must come from (see `dispatch`).
type ReviveCtx {
  ReviveCtx(reviver: JsValue, caller_builtins: common.Builtins)
}

/// Step 3: keep the parse record only if it is still the record for `val` —
/// SameValue(record's [[Value]], val). For arrays and objects that is a heap
/// ref identity check, so a reviver that swapped in a *different* array/object
/// (`reviver-call-args-after-forward-modification.js`) drops the record, and
/// with it both the `source` and every child record.
fn fresh_record(
  node: Option(ParseRecord),
  val: JsValue,
) -> Option(ParseRecord) {
  use record <- option.then(node)
  case value.same_value(record_value(record), val) {
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
  state: State(host),
  ctx: ReviveCtx,
  ref: Ref,
  i: Int,
  len: Int,
  children: List(ParseRecord),
) -> Result(State(host), #(JsValue, State(host))) {
  case i >= len {
    True -> Ok(state)
    False -> {
      let #(child, rest_children) = case children {
        [child, ..rest] -> #(Some(child), rest)
        [] -> #(None, [])
      }
      let name = int.to_string(i)
      use #(new_element, state) <- result.try(internalize_json_property(
        state,
        ctx,
        ref,
        name,
        child,
      ))
      use state <- result.try(replace_or_delete(state, ref, name, new_element))
      internalize_elements(state, ctx, ref, i + 1, len, rest_children)
    }
  }
}

/// §25.5.1.1 step 5.c.iii: recurse over the object's own enumerable string
/// keys, handing each the parse record it was scanned from (a key a reviver
/// added has none). `members` is built once per object (see `record_members`),
/// so each key costs one dict lookup rather than a scan of every member.
fn internalize_keys(
  state: State(host),
  ctx: ReviveCtx,
  ref: Ref,
  keys: List(String),
  members: dict.Dict(String, ParseRecord),
) -> Result(State(host), #(JsValue, State(host))) {
  case keys {
    [] -> Ok(state)
    [p, ..rest] -> {
      use #(new_element, state) <- result.try(internalize_json_property(
        state,
        ctx,
        ref,
        p,
        dict.get(members, p) |> option.from_result,
      ))
      use state <- result.try(replace_or_delete(state, ref, p, new_element))
      internalize_keys(state, ctx, ref, rest, members)
    }
  }
}

/// §25.5.1.1 steps 2.b.ii.2-3 / 2.c.ii.2-3: an `undefined` result from the
/// reviver deletes the child, anything else is CreateDataProperty'd back.
/// Both spec steps are a bare `Perform ?`, so a `false` [[Delete]] /
/// [[DefineOwnProperty]] result is DISCARDED — a reviver that makes a sibling
/// key non-configurable must not turn the next replacement into a TypeError.
/// Hence `create_data_property_bool`, not the OrThrow variant.
fn replace_or_delete(
  state: State(host),
  ref: Ref,
  name: String,
  new_element: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  case new_element {
    JsUndefined -> {
      use #(state, _deleted) <- result.map(objops.delete_property_stateful(
        state,
        ref,
        objops.PkString(key.canonical_key(name)),
      ))
      state
    }
    _ -> {
      use #(state, _defined) <- result.map(
        object_builtins.create_data_property_bool(
          state,
          ref,
          JsString(name),
          new_element,
        ),
      )
      state
    }
  }
}

/// The spec's root holder object: OrdinaryObjectCreate(%Object.prototype%)
/// with CreateDataPropertyOrThrow(holder, "", val). Both JSON.parse
/// (§25.5.1 steps 7-8) and JSON.stringify (§25.5.2 steps 9-11) start from one.
fn alloc_holder(state: State(host), val: JsValue) -> #(State(host), Ref) {
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.from_list([
          #(key.canonical_key(""), value.data_property(val)),
        ]),
        elements: elements.new(),
        prototype: Some(state.builtins.object.prototype),
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(State(..state, heap:), ref)
}

/// InternalizeJSONProperty step 2 (+3.a): OrdinaryObjectCreate(%Object.prototype%),
/// carrying a `source` own data property (writable, enumerable, configurable —
/// what CreateDataPropertyOrThrow gives) when the value came from an unmodified
/// primitive literal, and no own property at all otherwise.
///
/// This is the one place the recorded source bytes are decoded — reached only
/// with a reviver in play, so a plain `JSON.parse` never pays for it.
fn alloc_context(
  state: State(host),
  source: Option(BitArray),
) -> #(State(host), Ref) {
  let properties = case source {
    Some(raw) -> {
      // The slice spans a whole literal of a document that came from a Gleam
      // String, so it is valid UTF-8 by construction. Assert rather than fall
      // back — a broken invariant must not silently hand the reviver a bogus
      // `source`.
      let assert Ok(text) = bit_array.to_string(raw)
      dict.from_list([
        #(key.canonical_key("source"), value.data_property(JsString(text))),
      ])
    }
    None -> dict.new()
  }
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: OrdinaryObject,
        properties:,
        elements: elements.new(),
        prototype: Some(state.builtins.object.prototype),
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(State(..state, heap:), ref)
}

/// Intermediate parsed JSON value — not yet materialized onto the JS heap.
/// We parse into this first, then walk it to create JsValues/heap objects.
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
  JsonNumber(value: value.JsNum, source: BitArray)
  JsonString(value: String, source: BitArray)
  JsonArray(List(JsonValue))
  JsonObject(List(#(String, JsonValue)))
}

/// The proposal's JSON Parse Record: a literal node paired with the exact
/// `JsValue` it materialized into ([[Value]]) — the heap `Ref` for arrays and
/// objects — plus its children's records.
///
/// InternalizeJSONProperty compares [[Value]] against whatever is actually
/// sitting in the holder before it hands out either the `source` text or the
/// child records, so a reviver that swaps a slot for a different value cannot
/// make the source of the original literal describe the replacement.
type ParseRecord {
  /// A primitive literal: `[[Value]]` and its exact source text, still as the
  /// undecoded bytes the scanner sliced out (see `JsonValue`).
  PrimRecord(value: JsValue, source: BitArray)
  /// An array literal: the array object it produced, and its elements' records.
  ArrayRecord(value: JsValue, elements: List(ParseRecord))
  /// An object literal: the object it produced, and its members' records.
  ObjectRecord(value: JsValue, entries: List(#(String, ParseRecord)))
}

/// The record's [[Value]]: what materializing this literal produced.
fn record_value(record: ParseRecord) -> JsValue {
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
/// (later inserts win), matching how `value.props_dict_from_pairs` materializes
/// them.
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
  /// A complete JSON value was scanned but non-whitespace bytes follow it.
  TrailingContent
  /// `JSON.rawJSON("")` — the text is empty.
  RawJsonEmpty
  /// `JSON.rawJSON` text starts or ends with JSON whitespace.
  RawJsonSurroundingWhitespace
  /// `JSON.rawJSON` text parses, but to an object or an array.
  RawJsonNotPrimitive
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
    TrailingContent -> "Unexpected non-whitespace character after JSON"
    RawJsonEmpty -> "JSON.rawJSON text must not be empty"
    RawJsonSurroundingWhitespace ->
      "JSON.rawJSON text must not start or end with whitespace"
    RawJsonNotPrimitive -> "JSON.rawJSON text must not be an object or an array"
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
    <<0x6E, 0x75, 0x6C, 0x6C, rest:bytes>> ->
      Ok(#(JsonNull(source: <<"null":utf8>>), rest))
    // "true"
    <<0x74, 0x72, 0x75, 0x65, rest:bytes>> ->
      Ok(#(JsonBool(value: True, source: <<"true":utf8>>), rest))
    // "false"
    <<0x66, 0x61, 0x6C, 0x73, 0x65, rest:bytes>> ->
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
      // `num_str` already IS the literal's exact source text; on BEAM a String
      // is a binary, so re-viewing it as bytes is free (no copy, no scan).
      #(
        JsonNumber(
          value: number_span_to_num(num_str, span),
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
      // `s` is a JSON number token, so `int_len` counts ASCII characters and
      // codepoint slicing is exact. `string.slice`/`string.drop_start` would
      // segment graphemes here for no reason.
      let mantissa = js_string.slice(s, 0, span.int_len)
      let exponent = js_string.drop_start(s, span.int_len)
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
/// the JsValue produced (`record_value`) plus, for arrays and objects, the
/// records of everything underneath it. The record tree is what feeds
/// InternalizeJSONProperty's `context.source`.
fn materialize(
  h: Heap(host),
  b: common.Builtins,
  val: JsonValue,
) -> #(Heap(host), ParseRecord) {
  case val {
    JsonNull(source:) -> #(h, PrimRecord(value: JsNull, source:))
    JsonBool(value: b_val, source:) -> #(h, PrimRecord(JsBool(b_val), source:))
    JsonNumber(value: n, source:) -> #(h, PrimRecord(JsNumber(n), source:))
    JsonString(value: s, source:) -> #(h, PrimRecord(JsString(s), source:))
    JsonArray(items) -> {
      let #(h, elements) = materialize_list(h, b, items, [])
      let #(h, ref) =
        common.alloc_array(
          h,
          list.map(elements, record_value),
          b.array.prototype,
        )
      #(h, ArrayRecord(value: JsObject(ref), elements:))
    }
    JsonObject(entries) -> {
      let #(h, entries) = materialize_object_entries(h, b, entries, [])
      let props =
        list.map(entries, fn(entry) {
          #(
            key.canonical_key(entry.0),
            value.data_property(record_value(entry.1)),
          )
        })
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
      #(h, ObjectRecord(value: JsObject(ref), entries:))
    }
  }
}

fn materialize_list(
  h: Heap(host),
  b: common.Builtins,
  items: List(JsonValue),
  acc: List(ParseRecord),
) -> #(Heap(host), List(ParseRecord)) {
  case items {
    [] -> #(h, list.reverse(acc))
    [item, ..rest] -> {
      let #(h, record) = materialize(h, b, item)
      materialize_list(h, b, rest, [record, ..acc])
    }
  }
}

fn materialize_object_entries(
  h: Heap(host),
  b: common.Builtins,
  entries: List(#(String, JsonValue)),
  acc: List(#(String, ParseRecord)),
) -> #(Heap(host), List(#(String, ParseRecord))) {
  case entries {
    [] -> #(h, list.reverse(acc))
    [#(name, val), ..rest] -> {
      let #(h, record) = materialize(h, b, val)
      materialize_object_entries(h, b, rest, [#(name, record), ..acc])
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
  caller_builtins: common.Builtins,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let val = helpers.first_arg_or_undefined(args)
  let replacer = helpers.arg_at(args, 1)
  let space = helpers.arg_at(args, 2)

  let result = {
    // Step 4: ReplacerFunction / PropertyList.
    use #(replacer, state) <- result.try(build_replacer(state, replacer))
    // Steps 5-8: gap.
    use #(gap, state) <- result.try(compute_gap(state, space))
    // Steps 9-11: wrapper = OrdinaryObjectCreate(%Object.prototype%) with
    // CreateDataPropertyOrThrow(wrapper, "", value).
    let #(state, wrapper) = alloc_holder(state, val)
    let ctx = StringifyCtx(replacer:, gap:, caller_builtins:)
    // Step 12.
    serialize_property(state, ctx, [], "", "", wrapper)
  }
  case result {
    Ok(#(Some(s), state)) -> #(state, Ok(JsString(s)))
    Ok(#(None, state)) -> #(state, Ok(JsUndefined))
    Error(#(err, state)) -> #(state, Error(err))
  }
}

/// §25.5.2 step 4 makes ReplacerFunction and PropertyList mutually exclusive:
/// a callable replacer sets the first and leaves the second undefined, an
/// array replacer the reverse. One sum type instead of two Options, so
/// "function AND property list" cannot be built.
type Replacer {
  /// The replacer argument was neither callable nor an array.
  NoReplacer
  /// Step 4.a: IsCallable(replacer) → ReplacerFunction.
  ReplacerFn(f: JsValue)
  /// Step 4.b: IsArray(replacer) → PropertyList (deduplicated key names).
  PropertyList(names: List(String))
}

/// Immutable parts of the spec's JSON Serialization Record: the replacer
/// (ReplacerFunction xor PropertyList) and Gap. (Stack and Indent are threaded
/// as parameters.) `caller_builtins` is not the spec's — it is the realm the
/// replacer and any `toJSON` must run in, see `dispatch`.
type StringifyCtx {
  StringifyCtx(
    replacer: Replacer,
    gap: String,
    caller_builtins: common.Builtins,
  )
}

const circular_msg = "Converting circular structure to JSON"

/// §25.5.2 step 4: derive ReplacerFunction (callable replacer) or
/// PropertyList (array replacer) from the second argument.
fn build_replacer(
  state: State(host),
  replacer: JsValue,
) -> Result(#(Replacer, State(host)), #(JsValue, State(host))) {
  case replacer {
    JsObject(ref) ->
      case helpers.is_callable(state.heap, replacer) {
        // Step 4.a: IsCallable → ReplacerFunction.
        True -> Ok(#(ReplacerFn(replacer), state))
        False -> {
          // Step 4.b.i: isArray = ? IsArray(replacer) — a revoked proxy
          // makes IsArray throw a TypeError.
          use #(is_arr, state) <- result.try(objops.try_is_array(
            state,
            replacer,
          ))
          case is_arr {
            False -> Ok(#(NoReplacer, state))
            // Step 4.b.iii: build PropertyList from the array elements.
            True -> {
              use #(len, state) <- result.try(length_of_array_like(state, ref))
              use #(items, state) <- result.map(
                collect_property_list(state, ref, 0, len, set.new(), []),
              )
              #(PropertyList(items), state)
            }
          }
        }
      }
    _ -> Ok(#(NoReplacer, state))
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
        key.index_key(k),
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
    // Step 7: String → first 10 code units. Codepoints, not graphemes: a
    // gap of ten combining marks is ten code units, and `string.slice` would
    // fold them into one cluster and keep the whole string.
    JsString(s) ->
      case js_string.length(s) <= 10 {
        True -> s
        False -> js_string.slice(s, 0, 10)
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

/// LengthOfArrayLike (§7.3.18) — `property.length_of_array_like` is THE
/// implementation; this only supplies the receiver.
fn length_of_array_like(
  state: State(host),
  ref: Ref,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  property.length_of_array_like(state, ref, JsObject(ref))
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
    key.canonical_key(key),
    JsObject(holder),
  ))
  // Step 2: toJSON — looked up for Objects AND BigInt primitives (GetV).
  use #(val, state) <- result.try(case val {
    JsObject(_) | value.JsBigInt(_) -> {
      use #(to_json, state) <- result.try(objops.get_value_of(
        state,
        val,
        key.canonical_key("toJSON"),
      ))
      case helpers.is_callable(state.heap, to_json) {
        True ->
          call_in_caller_realm(state, ctx.caller_builtins, to_json, val, [
            JsString(key),
          ])
        False -> Ok(#(val, state))
      }
    }
    _ -> Ok(#(val, state))
  })
  // Step 3: ReplacerFunction — called with the holder as `this`.
  use #(val, state) <- result.try(case ctx.replacer {
    ReplacerFn(rf) ->
      call_in_caller_realm(state, ctx.caller_builtins, rf, JsObject(holder), [
        JsString(key),
        val,
      ])
    NoReplacer | PropertyList(_) -> Ok(#(val, state))
  })
  // Step 4.e (json-parse-with-source): a [[IsRawJSON]] box is emitted verbatim.
  // The check runs AFTER the toJSON call (step 2) and the replacer call (step
  // 3) — so a BigInt whose toJSON/replacer hands back JSON.rawJSON(...) round-
  // trips instead of throwing at step 10 — and BEFORE the wrapper unwrap and the
  // type dispatch, so the box is never serialized as an ordinary object. The box
  // is frozen with a null prototype, so `Get(value, "rawJSON")` is unobservable
  // and its payload is exactly the validated source text: emitted with no
  // quoting/escaping, and with no effect on gap/indent, the serialization stack
  // (no recursion, hence no circular check) or the replacer paths.
  use <- emit_raw_json(state, raw_json_text(state.heap, val))
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
        False -> {
          use #(is_arr, state) <- result.try(objops.try_is_array(state, val))
          case is_arr {
            True -> {
              use #(s, state) <- result.map(serialize_array(
                state,
                ctx,
                stack,
                indent,
                ref,
              ))
              #(Some(s), state)
            }
            False -> {
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
      }
    // Step 12: undefined / symbol → omitted.
    JsUndefined | value.JsSymbol(_) | value.JsUninitialized ->
      Ok(#(None, state))
  }
}

/// The [[IsRawJSON]] payload of `value`, if it is a `JSON.rawJSON` box: the
/// already-validated JSON source text, which is exactly what
/// `Get(value, "rawJSON")` would return (the box is frozen with a null
/// prototype, so no getter or proxy trap can observe the lookup).
fn raw_json_text(h: Heap(host), val: JsValue) -> Option(String) {
  case val {
    JsObject(ref) -> value.raw_json_text(heap.read(h, ref))
    _ -> None
  }
}

/// SerializeJSONProperty step 4.e: when `raw` is present, the raw JSON text is
/// the serialization of the property — emitted verbatim, no quoting, escaping
/// or recursion. Otherwise fall through to the rest of the algorithm.
fn emit_raw_json(
  state: State(host),
  raw: Option(String),
  otherwise: fn() ->
    Result(#(Option(String), State(host)), #(JsValue, State(host))),
) -> Result(#(Option(String), State(host)), #(JsValue, State(host))) {
  case raw {
    Some(text) -> Ok(#(Some(text), state))
    None -> otherwise()
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
      use #(keys, state) <- result.try(case ctx.replacer {
        PropertyList(names) -> Ok(#(names, state))
        NoReplacer | ReplacerFn(_) ->
          object_builtins.enumerable_string_keys_stateful(state, ref)
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

/// Result of scanning UTF-8 bytes for the next byte QuoteJSONString must
/// escape. The mirror image of `StringScan` on the parse side.
type EscapeScan {
  /// `n` clean bytes, then `byte` (which needs escaping), then `rest`.
  FoundEscapable(n: Int, byte: Int, rest: BitArray)
  /// Every remaining byte can be emitted verbatim.
  AllClean
}

/// Scan forward for the first byte that must be escaped in a JSON string:
/// a control character (< 0x20), '"' or '\\'.
///
/// Safe on UTF-8, and the reason escaping is byte-oriented at all: every byte
/// of a multi-byte sequence is >= 0x80, so it can never be mistaken for an
/// escapable ASCII byte, and a clean span cut at an escapable byte is always
/// itself valid UTF-8.
fn scan_escapable(bytes: BitArray, n: Int) -> EscapeScan {
  case bytes {
    <<0x22, rest:bytes>> -> FoundEscapable(n, 0x22, rest)
    <<0x5c, rest:bytes>> -> FoundEscapable(n, 0x5c, rest)
    <<c, rest:bytes>> if c < 0x20 -> FoundEscapable(n, c, rest)
    <<_, rest:bytes>> -> scan_escapable(rest, n + 1)
    _ -> AllClean
  }
}

/// Stringify a JS string with QuoteJSONString escaping (§25.5.2.3).
///
/// Fast path: the overwhelming majority of strings have nothing to escape, so
/// the single scan finds nothing and `s` is quoted with no intermediate
/// allocation. Otherwise `escape_from` continues from where the scan stopped.
fn stringify_string(s: String) -> String {
  let bytes = <<s:utf8>>
  case scan_escapable(bytes, 0) {
    AllClean -> "\"" <> s <> "\""
    found -> "\"" <> escape_from(found, bytes, string_tree.new()) <> "\""
  }
}

/// Slow path: `scan` is the result of scanning `bytes` from its start. Append
/// the clean span before the escapable byte as one chunk (an O(1) sub-binary,
/// not one append per character), then the escape, then continue on the rest.
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

/// The escape sequence for a byte `scan_escapable` stopped on: '"', '\\', or
/// a control character (< 0x20). Everything below 0x20 without a short form
/// gets \u00XX. This is total over exactly the bytes `scan_escapable` returns.
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

/// Append the first `n` bytes of `bytes` to `acc` as one chunk.
fn append_span(acc: StringTree, bytes: BitArray, n: Int) -> StringTree {
  case n {
    0 -> acc
    _ -> {
      // The slice cannot fail: `bytes` is the UTF-8 encoding of a Gleam String
      // and `n` always lands on an ASCII boundary (`scan_escapable` only stops
      // on bytes < 0x80), so the span is valid UTF-8 and within bounds. Assert
      // rather than fall back — a broken invariant must not silently truncate.
      let assert Ok(chunk) =
        bit_array.slice(bytes, 0, n) |> result.try(bit_array.to_string)
      string_tree.append(acc, chunk)
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
