import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/realm as rt_realm
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsNum, type JsVal, type JsonNative,
  type PropertyKey, ArrayObj, BigIntObj, BooleanObj, Index, JFloat, JInt, JNan,
  JNegInf, JPosInf, JsonIsRawJson, JsonN, JsonParse, JsonRawJson, JsonStringify,
  KBig, KBool, KHandle, KNull, KNum, KStr, KSym, KUndef, Named, NumberObj,
  Ordinary, RawJsonObj, SObject, SShapedObject, StringKey, StringObj, classify,
  index_key, mk_bool, mk_null, mk_number, mk_object, mk_string, mk_undefined,
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

// body runs in the function's realm, callbacks in the caller's
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

// §25.5.1
fn json_parse(args: List(JsVal), caller: Int, st: Agent) -> #(JsVal, Agent) {
  let #(json_str, st) =
    rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  let bytes = bit_array.from_string(json_str)
  let reviver = helpers.arg_at(args, 1)
  // iscallable has no side effects, so it can run before the parse
  let revive = rt_call.is_callable(st, reviver)
  case parse_value(bytes, revive) {
    Error(e) -> rt_val.t_throw_syntax_error(st, json_error_message(e))
    Ok(#(val, rest)) ->
      case skip_whitespace(rest) {
        <<>> -> {
          case revive {
            False -> materialize_plain(st, val)
            True -> {
              let #(record, st) = materialize(st, val)
              let #(root, st) = alloc_holder(st, record_value(record))
              let ctx = ReviveCtx(reviver:, caller:)
              internalize_json_property(st, ctx, root, "", Some(record))
            }
          }
        }
        _ ->
          rt_val.t_throw_syntax_error(st, json_error_message(TrailingContent))
      }
  }
}

// §25.5.1.1 internalizejsonproperty
fn internalize_json_property(
  st: Agent,
  ctx: ReviveCtx,
  holder: Handle,
  name: String,
  node: Option(ParseRecord),
) -> #(JsVal, Agent) {
  let #(val, st) =
    rt_obj.t_get_prop(
      st,
      mk_object(holder),
      StringKey(rt_types.canonical_key(name)),
    )
  let node = fresh_record(node, val)
  let st = case classify(val) {
    KHandle(h) ->
      case is_array_handle(st, h) {
        True -> {
          let #(len, st) = length_of_array_like(st, h)
          internalize_elements(st, ctx, h, 0, len, record_elements(node))
        }
        False -> {
          let #(keys, st) = enumerable_string_keys(st, h)
          let keys = list.map(keys, rt_types.key_to_text)
          internalize_keys(st, ctx, h, keys, record_members(node))
        }
      }
    _ -> st
  }
  let #(context, st) = alloc_context(st, record_source(node))
  call_in_caller_realm(st, ctx.caller, ctx.reviver, mk_object(holder), [
    mk_string(name),
    val,
    mk_object(context),
  ])
}

type ReviveCtx {
  ReviveCtx(reviver: JsVal, caller: Int)
}

// stale record must leak neither source nor children
fn fresh_record(node: Option(ParseRecord), val: JsVal) -> Option(ParseRecord) {
  use record <- option.then(node)
  case rt_val.same_value(record_value(record), val) {
    True -> Some(record)
    False -> None
  }
}

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

// false delete/define results deliberately discarded per spec
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

fn alloc_holder(st: Agent, val: JsVal) -> #(Handle, Agent) {
  common.alloc_pojo(st, st.realm.object.prototype, [#("", val)])
}

fn alloc_context(st: Agent, source: Option(BitArray)) -> #(Handle, Agent) {
  let props = case source {
    Some(raw) -> {
      // valid utf-8 by construction
      let assert Ok(text) = bit_array.to_string(raw)
      [#("source", mk_string(text))]
    }
    None -> []
  }
  common.alloc_pojo(st, st.realm.object.prototype, props)
}

// source stays raw bytes until a reviver asks
type JsonValue {
  JsonNull(source: BitArray)
  JsonBool(value: Bool, source: BitArray)
  JsonNumber(value: JsNum, source: BitArray)
  JsonString(value: String, source: BitArray)
  JsonArray(List(JsonValue))
  JsonObject(List(#(String, JsonValue)))
}

type ParseRecord {
  PrimRecord(value: JsVal, source: BitArray)
  ArrayRecord(value: JsVal, elements: List(ParseRecord))
  ObjectRecord(value: JsVal, entries: List(#(String, ParseRecord)))
}

fn record_value(record: ParseRecord) -> JsVal {
  case record {
    PrimRecord(value:, ..)
    | ArrayRecord(value:, ..)
    | ObjectRecord(value:, ..) -> value
  }
}

fn record_source(record: Option(ParseRecord)) -> Option(BitArray) {
  case record {
    Some(PrimRecord(source:, ..)) -> Some(source)
    Some(ArrayRecord(..)) | Some(ObjectRecord(..)) | None -> None
  }
}

fn record_elements(record: Option(ParseRecord)) -> List(ParseRecord) {
  case record {
    Some(ArrayRecord(elements:, ..)) -> elements
    _ -> []
  }
}

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

@external(erlang, "arc_rt_json_ffi", "parse_value")
fn parse_value(
  bytes: BitArray,
  with_source: Bool,
) -> Result(#(JsonValue, BitArray), JsonParseError)

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

fn materialize_plain(st: Agent, val: JsonValue) -> #(JsVal, Agent) {
  case val {
    JsonNull(..) -> #(mk_null(), st)
    JsonBool(value: b, ..) -> #(mk_bool(b), st)
    JsonNumber(value: n, ..) -> #(mk_number(n), st)
    JsonString(value: s, ..) -> #(mk_string(s), st)
    JsonArray(items) -> {
      let #(values, st) = materialize_plain_list(st, items, [])
      let #(h, st) = realm_ops.alloc_array(st, values)
      #(mk_object(h), st)
    }
    JsonObject(entries) -> {
      let #(entries, st) = materialize_plain_entries(st, entries, [])
      let object_proto = st.realm.object.prototype
      let #(h, st) = {
        use seq <- rt_store.t_cell_new_with(st, list.length(entries))
        let props = case ffi_plain_props(entries, seq) {
          Some(#(props, _seq)) -> props
          None -> plain_props(entries, dict.new(), seq)
        }
        SObject(
          kind: Ordinary,
          proto: Some(object_proto),
          props:,
          symbol_props: [],
          elements: rt_types.NoElements,
          extensible: True,
        )
      }
      #(mk_object(h), st)
    }
  }
}

fn materialize_plain_list(
  st: Agent,
  items: List(JsonValue),
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case items {
    [] -> #(list.reverse(acc), st)
    [item, ..rest] -> {
      let #(v, st) = materialize_plain(st, item)
      materialize_plain_list(st, rest, [v, ..acc])
    }
  }
}

fn materialize_plain_entries(
  st: Agent,
  entries: List(#(String, JsonValue)),
  acc: List(#(String, JsVal)),
) -> #(List(#(String, JsVal)), Agent) {
  case entries {
    [] -> #(list.reverse(acc), st)
    [#(name, val), ..rest] -> {
      let #(v, st) = materialize_plain(st, val)
      materialize_plain_entries(st, rest, [#(name, v), ..acc])
    }
  }
}

@external(erlang, "arc_rt_json_ffi", "plain_props")
fn ffi_plain_props(
  entries: List(#(String, JsVal)),
  seq: Int,
) -> Option(#(dict.Dict(rt_types.PropertyKey, rt_types.Property), Int))

fn plain_props(
  entries: List(#(String, JsVal)),
  acc: dict.Dict(rt_types.PropertyKey, rt_types.Property),
  seq: Int,
) -> dict.Dict(rt_types.PropertyKey, rt_types.Property) {
  case entries {
    [] -> acc
    [#(name, value), ..rest] -> {
      let key = rt_types.canonical_key(name)
      case dict.get(acc, key) {
        Ok(first) -> {
          let prop =
            rt_types.DataProperty(
              value,
              True,
              True,
              True,
              rt_types.prop_seq(first),
            )
          plain_props(rest, dict.insert(acc, key, prop), seq)
        }
        Error(Nil) -> {
          let prop = rt_types.DataProperty(value, True, True, True, seq)
          plain_props(rest, dict.insert(acc, key, prop), seq + 1)
        }
      }
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

// dup keys keep first position, last value
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

fn json_raw_json(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  let #(json_str, st) =
    rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  case validate_raw_json_text(bit_array.from_string(json_str)) {
    Error(e) -> rt_val.t_throw_syntax_error(st, json_error_message(e))
    Ok(Nil) -> {
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
  use #(parsed, rest) <- result.try(parse_value(bytes, False))
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
  option.is_some(raw_json_text(st, v))
}

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

type Replacer {
  NoReplacer
  ReplacerFn(f: JsVal)
  PropertyList(names: List(String))
}

type StringifyCtx {
  StringifyCtx(replacer: Replacer, gap: String, caller: Int)
}

const circular_msg = "Converting circular structure to JSON"

// §25.5.2
fn json_stringify(
  args: List(JsVal),
  caller: Int,
  st: Agent,
) -> #(JsVal, Agent) {
  let val = helpers.first_arg_or_undefined(args)
  let replacer_arg = helpers.arg_at(args, 1)
  let space = helpers.arg_at(args, 2)
  let #(replacer, st) = build_replacer(st, replacer_arg)
  let #(gap, st) = compute_gap(st, space)
  let #(wrapper, st) = alloc_holder(st, val)
  let ctx = StringifyCtx(replacer:, gap:, caller:)
  case serialize_property(st, ctx, [], "", Named(""), wrapper) {
    #(Some(tree), st) ->
      case string_tree.byte_size(tree) > limits.max_string_bytes {
        True -> rt_val.t_throw_range_error(st, "Invalid string length")
        False -> #(mk_string(flatten(tree)), st)
      }
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

// §25.5.2.1 serializejsonproperty
fn serialize_property(
  st: Agent,
  ctx: StringifyCtx,
  stack: List(Int),
  indent: String,
  key: PropertyKey,
  holder: Handle,
) -> #(Option(StringTree), Agent) {
  // canonical keys, so a named key never spells an index
  let #(val, st) = case key {
    Named(name) -> helpers.get_named(st, mk_object(holder), name)
    Index(i) -> helpers.get_index(st, mk_object(holder), i)
    rt_types.Private(_) -> #(mk_undefined(), st)
  }
  let #(val, st) = case classify(val) {
    KHandle(_) | KBig(_) -> {
      let #(to_json, st) = helpers.get_named(st, val, "toJSON")
      case rt_call.is_callable(st, to_json) {
        True ->
          call_in_caller_realm(st, ctx.caller, to_json, val, [
            mk_string(rt_types.key_to_text(key)),
          ])
        False -> #(val, st)
      }
    }
    _ -> #(val, st)
  }
  let #(val, st) = case ctx.replacer {
    ReplacerFn(rf) ->
      call_in_caller_realm(st, ctx.caller, rf, mk_object(holder), [
        mk_string(rt_types.key_to_text(key)),
        val,
      ])
    NoReplacer | PropertyList(_) -> #(val, st)
  }
  serialize_value(st, ctx, stack, indent, val)
}

fn serialize_value(
  st: Agent,
  ctx: StringifyCtx,
  stack: List(Int),
  indent: String,
  val: JsVal,
) -> #(Option(StringTree), Agent) {
  case classify(val) {
    KStr(s) -> #(Some(quote_tree(s)), st)
    KNum(n) ->
      case n {
        JInt(_) | JFloat(_) -> #(
          Some(string_tree.from_string(rt_val.jsnum_to_string(n))),
          st,
        )
        JNan | JPosInf | JNegInf -> #(Some(string_tree.from_string("null")), st)
      }
    KNull -> #(Some(string_tree.from_string("null")), st)
    KBool(True) -> #(Some(string_tree.from_string("true")), st)
    KBool(False) -> #(Some(string_tree.from_string("false")), st)
    KHandle(h) -> serialize_handle(st, ctx, stack, indent, val, h)
    KBig(_) ->
      rt_val.t_throw_type_error(st, "Do not know how to serialize a BigInt")
    KUndef | KSym(_) | rt_types.KTdz -> #(None, st)
  }
}

fn serialize_handle(
  st: Agent,
  ctx: StringifyCtx,
  stack: List(Int),
  indent: String,
  val: JsVal,
  h: Handle,
) -> #(Option(StringTree), Agent) {
  case obj_kind(st, h) {
    Some(RawJsonObj(raw:)) -> #(Some(string_tree.from_string(raw)), st)
    Some(NumberObj(_)) -> {
      let #(n, st) = rt_val.t_to_number(st, val)
      serialize_value(st, ctx, stack, indent, mk_number(n))
    }
    Some(StringObj(_)) -> {
      let #(s, st) = rt_val.t_to_string(st, val)
      #(Some(quote_tree(s)), st)
    }
    Some(BooleanObj(b)) -> serialize_value(st, ctx, stack, indent, mk_bool(b))
    Some(BigIntObj(_)) ->
      rt_val.t_throw_type_error(st, "Do not know how to serialize a BigInt")
    _ ->
      case rt_call.is_callable(st, val) {
        True -> #(None, st)
        False -> {
          let #(tree, st) = case is_array_handle(st, h) {
            True -> serialize_array(st, ctx, stack, indent, h)
            False -> serialize_object(st, ctx, stack, indent, h)
          }
          #(Some(tree), st)
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
) -> #(StringTree, Agent) {
  case list.contains(stack, h.id) {
    True -> rt_val.t_throw_type_error(st, circular_msg)
    False -> {
      let stack = [h.id, ..stack]
      let step_indent = indent <> ctx.gap
      let #(keys, st) = case ctx.replacer {
        PropertyList(names) -> #(list.map(names, rt_types.canonical_key), st)
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
  keys: List(PropertyKey),
  acc: List(StringTree),
) -> #(List(StringTree), Agent) {
  case keys {
    [] -> #(acc, st)
    [k, ..rest] -> {
      let #(str_p, st) = serialize_property(st, ctx, stack, step_indent, k, h)
      case str_p {
        Some(tree) -> {
          let sep = case ctx.gap {
            "" -> ":"
            _ -> ": "
          }
          let member =
            quote_tree(rt_types.key_to_text(k))
            |> string_tree.append(sep)
            |> string_tree.append_tree(tree)
          serialize_members(st, ctx, stack, step_indent, h, rest, [
            member,
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
) -> #(StringTree, Agent) {
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
  acc: List(StringTree),
) -> #(List(StringTree), Agent) {
  case i >= len {
    True -> #(acc, st)
    False -> {
      let #(str_p, st) =
        serialize_property(st, ctx, stack, step_indent, index_key(i), h)
      let item =
        option.lazy_unwrap(str_p, fn() { string_tree.from_string("null") })
      serialize_elements(st, ctx, stack, step_indent, h, i + 1, len, [
        item,
        ..acc
      ])
    }
  }
}

fn finalize_brackets(
  partial_rev: List(StringTree),
  gap: String,
  step_indent: String,
  stepback: String,
  open: String,
  close: String,
) -> StringTree {
  let items = list.reverse(partial_rev)
  case items, gap {
    [], _ -> string_tree.from_strings([open, close])
    _, "" ->
      string_tree.join(items, ",")
      |> string_tree.prepend(open)
      |> string_tree.append(close)
    _, _ ->
      string_tree.join(items, ",\n" <> step_indent)
      |> string_tree.prepend(open <> "\n" <> step_indent)
      |> string_tree.append("\n" <> stepback <> close)
  }
}

@external(erlang, "arc_rt_json_ffi", "quote")
fn quote_tree(s: String) -> StringTree

// all utf-8 binaries already, skips the unicode rescan
@external(erlang, "erlang", "iolist_to_binary")
fn flatten(tree: StringTree) -> String

fn obj_kind(st: Agent, h: Handle) -> Option(rt_types.ObjKind) {
  case rt_store.t_cell_get(st, h) {
    SObject(kind:, ..) -> Some(kind)
    SShapedObject(..) -> Some(Ordinary)
    _ -> None
  }
}

// §7.2.2 isarray, pierces proxies
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

fn enumerable_string_keys(st: Agent, h: Handle) -> #(List(PropertyKey), Agent) {
  case rt_store.t_cell_get(st, h) {
    // shaped slots are all plain enumerable data
    SShapedObject(..) -> {
      let #(keys, st) = rt_obj.t_own_keys(st, h)
      #(list.filter_map(keys, string_key), st)
    }
    SObject(kind: Ordinary, props:, elements: rt_types.NoElements, ..) -> #(
      plain_enumerable_keys(props),
      st,
    )
    _ -> {
      let #(keys, st) = rt_obj.t_enumerable_own_keys(st, h)
      #(keys, st)
    }
  }
}

fn string_key(key: rt_types.ObjectKey) -> Result(PropertyKey, Nil) {
  case key {
    StringKey(pk) -> Ok(pk)
    rt_types.SymbolKey(_) -> Error(Nil)
  }
}

@external(erlang, "arc_rt_json_ffi", "plain_keys")
fn plain_enumerable_keys(
  props: dict.Dict(PropertyKey, rt_types.Property),
) -> List(PropertyKey)
