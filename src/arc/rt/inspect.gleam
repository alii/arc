import arc/internal/ordered_entries
import arc/rt/buffer
import arc/rt/elements
import arc/rt/intl_data
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsElements, type JsVal, type Property,
  type PropertyKey, type TemporalData, ArgumentsObj, ArrayBufferObj,
  ArrayIterator, ArrayObj, AsyncFromSyncIterator, AsyncGeneratorObj, BigIntObj,
  BooleanObj, DataProperty, DataViewObj, DateObj, DisposableStackObj, ErrorObj,
  FinalizationRegistryObj, ForInIterator, GeneratorObj, Index, IntlObj,
  IteratorHelperObj, KBig, KBool, KBound, KBytecode, KCompiled, KHandle, KHost,
  KNative, KNull, KNum, KStr, KSym, KTdz, KUndef, MapIterator, MapObj,
  ModuleNamespace, Named, NumberObj, Ordinary, Private, PromiseObj, ProxyObj,
  RawJsonObj, RegExpObj, SObject, SetIterator, SetObj, Shared, StringIterator,
  StringObj, SymbolObj, TemporalDate, TemporalDateTime, TemporalDuration,
  TemporalInstant, TemporalMonthDay, TemporalObj, TemporalTime,
  TemporalYearMonth, TemporalZonedDateTime, TypedArrayObj, WeakMapObj,
  WeakSetObj, WrapForValidIteratorObj, classify,
} as rt_types
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string

const max_items = 100

// read only: never invokes js, safe on error paths
pub fn inspect(st: Agent, val: JsVal) -> String {
  inspect_inner(st, val, 0, set.new())
}

fn inspect_inner(
  st: Agent,
  val: JsVal,
  depth: Int,
  visited: Set(Handle),
) -> String {
  case classify(val) {
    KUndef -> "undefined"
    KNull -> "null"
    KBool(True) -> "true"
    KBool(False) -> "false"
    KNum(n) -> rt_val.jsnum_to_string(n)
    KStr(s) -> "'" <> escape_string(s) <> "'"
    KSym(id) ->
      "Symbol(" <> option.unwrap(rt_types.symbol_description(id), "") <> ")"
    KBig(n) -> int.to_string(n) <> "n"
    KTdz -> "<uninitialized>"
    KHandle(h) ->
      case set.contains(visited, h) {
        True -> "[Circular]"
        False -> inspect_object(st, h, depth, set.insert(visited, h))
      }
  }
}

fn escape_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("'", "\\'")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
}

fn inspect_object(
  st: Agent,
  h: Handle,
  depth: Int,
  visited: Set(Handle),
) -> String {
  case rt_obj.as_sobject(rt_store.t_cell_get(st, h)) {
    SObject(kind:, props:, elements:, symbol_props:, ..) ->
      case kind {
        ArrayObj(length:) -> inspect_array(st, elements, length, depth, visited)
        KCompiled(..) | KBytecode(..) | KNative(..) | KBound(..) -> {
          let name = case
            rt_obj.t_ordinary_own_property(
              st,
              h,
              rt_types.StringKey(Named("name")),
            )
          {
            Some(DataProperty(value:, ..)) ->
              case classify(value) {
                KStr(n) -> n
                _ -> ""
              }
            _ -> ""
          }
          case name {
            "" -> "[Function (anonymous)]"
            n -> "[Function: " <> n <> "]"
          }
        }
        PromiseObj(_) -> "Promise {}"
        ProxyObj(..) ->
          case rt_val.t_is_callable(st, rt_types.mk_object(h)).0 {
            True -> "[Function (Proxy)]"
            False -> "Proxy {}"
          }
        GeneratorObj(_) -> "Object [Generator] {}"
        AsyncGeneratorObj(_) -> "Object [AsyncGenerator] {}"
        ArgumentsObj(length:, ..) ->
          "[Arguments] " <> inspect_array(st, elements, length, depth, visited)
        StringObj(value: s) -> "[String: '" <> escape_string(s) <> "']"
        NumberObj(value: n) -> "[Number: " <> rt_val.jsnum_to_string(n) <> "]"
        BooleanObj(value: True) -> "[Boolean: true]"
        BooleanObj(value: False) -> "[Boolean: false]"
        BigIntObj(value: bi) -> "[BigInt: " <> int.to_string(bi) <> "n]"
        SymbolObj(value: sym) ->
          "[Symbol: "
          <> inspect_inner(st, rt_types.mk_symbol(sym), depth, visited)
          <> "]"
        MapObj(entries:) ->
          "Map(" <> int.to_string(ordered_entries.size(entries)) <> ")"
        SetObj(entries:) ->
          "Set(" <> int.to_string(ordered_entries.size(entries)) <> ")"
        WeakMapObj(_) -> "WeakMap {}"
        WeakSetObj(_) -> "WeakSet {}"
        ArrayIterator(..) -> "Object [Array Iterator] {}"
        StringIterator(..) -> "Object [String Iterator] {}"
        SetIterator(..) -> "Object [Set Iterator] {}"
        MapIterator(..) -> "Object [Map Iterator] {}"
        AsyncFromSyncIterator(..) -> "Object [Async-from-Sync Iterator] {}"
        ForInIterator(..) -> "[Object]"
        DateObj(ms:) ->
          case ms {
            rt_types.JInt(_) | rt_types.JFloat(_) ->
              "Date(" <> rt_val.jsnum_to_string(ms) <> ")"
            _ -> "Invalid Date"
          }
        RegExpObj(source:, flags:, ..) -> {
          let source = case source {
            "" -> "(?:)"
            p -> p
          }
          "/" <> source <> "/" <> flags
        }
        DataViewObj(..) -> "DataView {}"
        ArrayBufferObj(storage: Shared(..) as storage) ->
          "SharedArrayBuffer { byteLength: "
          <> int.to_string(rt_types.buffer_byte_size(storage))
          <> " }"
        ArrayBufferObj(storage:) ->
          "ArrayBuffer { byteLength: "
          <> int.to_string(rt_types.buffer_byte_size(storage))
          <> " }"
        TypedArrayObj(buffer: buf, elem_kind:, byte_offset:, length:) ->
          rt_types.typed_array_name(elem_kind)
          <> "("
          <> int.to_string(buffer.view_length(
            st,
            buffer.ViewSlot(buffer: buf, elem_kind:, byte_offset:, length:),
          ))
          <> ")"
        IteratorHelperObj(..) -> "[Iterator Helper]"
        WrapForValidIteratorObj(..) -> "[Iterator]"
        ModuleNamespace(exports:) ->
          "[Module: { "
          <> string.join(list.sort(dict.keys(exports), string.compare), ", ")
          <> " }]"
        RawJsonObj(raw:) -> "[RawJSON " <> raw <> "]"
        ErrorObj(_) -> error_display(st, h) |> option.unwrap("[Error]")
        IntlObj(data:, ..) ->
          "[Intl."
          <> intl_data.service_name(intl_data.intl_service(data))
          <> "]"
        TemporalObj(data:) -> temporal_label(data)
        DisposableStackObj(async: False, ..) -> "DisposableStack {}"
        DisposableStackObj(async: True, ..) -> "AsyncDisposableStack {}"
        FinalizationRegistryObj(..) -> "FinalizationRegistry {}"
        rt_types.WeakRefObj(..) -> "WeakRef {}"
        rt_types.ShadowRealmObj(..) -> "ShadowRealm {}"
        Ordinary | rt_types.GlobalObj | KHost(_) -> {
          let body = inspect_plain_object(st, props, depth, visited)
          case list.key_find(symbol_props, rt_types.symbol_to_string_tag) {
            Ok(DataProperty(value:, ..)) ->
              case classify(value) {
                KStr(t) -> "Object [" <> t <> "] " <> body
                _ -> body
              }
            _ -> body
          }
        }
      }
    _ -> "[Object]"
  }
}

fn inspect_array(
  st: Agent,
  elements: JsElements,
  length: Int,
  depth: Int,
  visited: Set(Handle),
) -> String {
  case depth > 2 {
    True -> "[Array]"
    False -> {
      let items =
        inspect_array_loop(st, elements, 0, length, depth, visited, [])
      "[ " <> string.join(items, ", ") <> " ]"
    }
  }
}

fn inspect_array_loop(
  st: Agent,
  elements: JsElements,
  idx: Int,
  length: Int,
  depth: Int,
  visited: Set(Handle),
  acc: List(String),
) -> List(String) {
  case idx >= length, idx >= max_items {
    True, _ -> list.reverse(acc)
    False, True ->
      list.reverse([
        "… " <> int.to_string(length - max_items) <> " more items",
        ..acc
      ])
    False, False -> {
      let item =
        elements.get_option(elements, idx)
        |> option.map(inspect_inner(st, _, depth + 1, visited))
        |> option.unwrap("<empty>")
      inspect_array_loop(st, elements, idx + 1, length, depth, visited, [
        item,
        ..acc
      ])
    }
  }
}

fn inspect_plain_object(
  st: Agent,
  props: Dict(PropertyKey, Property),
  depth: Int,
  visited: Set(Handle),
) -> String {
  case depth > 2 {
    True -> "[Object]"
    False -> {
      let visible =
        ordered_property_pairs(props)
        |> list.filter_map(fn(pair) {
          case pair {
            #(key, DataProperty(enumerable: True, value: val, ..)) ->
              Ok(#(key, val))
            _ -> Error(Nil)
          }
        })
      let total = list.length(visible)
      let entries =
        list.take(visible, max_items)
        |> list.map(fn(pair) {
          let #(key, val) = pair
          rt_types.key_display_string(key)
          <> ": "
          <> inspect_inner(st, val, depth + 1, visited)
        })
      let entries = case total > max_items {
        True ->
          list.append(entries, [
            "… " <> int.to_string(total - max_items) <> " more",
          ])
        False -> entries
      }
      case entries {
        [] -> "{}"
        _ -> "{ " <> string.join(entries, ", ") <> " }"
      }
    }
  }
}

fn ordered_property_pairs(
  props: Dict(PropertyKey, Property),
) -> List(#(PropertyKey, Property)) {
  let #(idx, named) =
    dict.fold(props, #([], []), fn(acc, key, prop) {
      let #(idx, named) = acc
      case key {
        Index(i) -> #([#(i, prop), ..idx], named)
        Named(_) | Private(_) -> #(idx, [#(key, prop), ..named])
      }
    })
  let idx =
    list.sort(idx, fn(a, b) { int.compare(a.0, b.0) })
    |> list.map(fn(pair) { #(Index(pair.0), pair.1) })
  let named =
    list.sort(named, fn(a, b) {
      int.compare(rt_types.prop_seq(a.1), rt_types.prop_seq(b.1))
    })
  list.append(idx, named)
}

pub fn format_error(st: Agent, val: JsVal) -> String {
  case classify(val) {
    KStr(s) -> s
    KHandle(h) -> error_display(st, h) |> option.unwrap(inspect(st, val))
    _ -> inspect(st, val)
  }
}

fn temporal_label(data: TemporalData) -> String {
  case data {
    TemporalInstant(..) -> "Temporal.Instant {}"
    TemporalDate(..) -> "Temporal.PlainDate {}"
    TemporalTime(..) -> "Temporal.PlainTime {}"
    TemporalDateTime(..) -> "Temporal.PlainDateTime {}"
    TemporalYearMonth(..) -> "Temporal.PlainYearMonth {}"
    TemporalMonthDay(..) -> "Temporal.PlainMonthDay {}"
    TemporalDuration(..) -> "Temporal.Duration {}"
    TemporalZonedDateTime(..) -> "Temporal.ZonedDateTime {}"
  }
}

fn error_display(st: Agent, h: Handle) -> Option(String) {
  case rt_store.t_cell_get(st, h) {
    SObject(kind: ErrorObj(stack:), ..) -> {
      let slot_stack = case stack {
        "" -> None
        s -> Some(s)
      }
      Some(case option.or(slot_stack, error_property(st, h, "stack", 100)) {
        Some(s) -> s
        None -> {
          let name =
            error_property(st, h, "name", 100) |> option.unwrap("Error")
          let message =
            error_property(st, h, "message", 100) |> option.unwrap("")
          case name, message {
            "", _ -> message
            _, "" -> name
            _, _ -> name <> ": " <> message
          }
        }
      })
    }
    _ -> None
  }
}

fn error_property(
  st: Agent,
  h: Handle,
  key: String,
  fuel: Int,
) -> Option(String) {
  use <- bool.guard(fuel <= 0, None)
  case rt_obj.as_sobject(rt_store.t_cell_get(st, h)) {
    SObject(props:, proto:, ..) ->
      case dict.get(props, Named(key)) {
        Ok(DataProperty(value:, ..)) ->
          case classify(value) {
            KStr(s) -> Some(s)
            _ -> None
          }
        Ok(_) -> None
        Error(Nil) ->
          case proto {
            Some(parent) -> error_property(st, parent, key, fuel - 1)
            None -> None
          }
      }
    _ -> None
  }
}
