//// Inspect — debugging/REPL representation of JS values.
////
//// Read-only rendering: never invokes JS (no toString/valueOf, no getters),
//// so it is safe to call from error paths and the REPL without observable
//// side effects or VM re-entry. Shared by console.log, uncaught-exception
//// reporting, and diagnostic messages ("x is not a function").

import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/ordered_entries
import arc/vm/key.{type PropertyKey, Named}
import arc/vm/ops/typed_array_elements
import arc/vm/state.{type Heap}
import arc/vm/value.{
  type JsElements, ArrayObject, DataProperty, FunctionObject, GeneratorObject,
  JsNumber, JsObject, JsString, NativeFunction, ObjectSlot, OrdinaryObject,
  PromiseObject,
}
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set
import gleam/string

/// Produce a human-readable representation of a JS value (for REPL / console.log).
/// Read-only — does NOT call toString/valueOf or any JS code.
pub fn inspect(val: value.JsValue, heap: Heap(host)) -> String {
  inspect_inner(val, heap, 0, set.new())
}

fn inspect_inner(
  val: value.JsValue,
  heap: Heap(host),
  depth: Int,
  visited: set.Set(Int),
) -> String {
  case val {
    value.JsUndefined -> "undefined"
    value.JsNull -> "null"
    value.JsBool(True) -> "true"
    value.JsBool(False) -> "false"
    value.JsNumber(value.Finite(n)) -> value.js_format_number(n)
    value.JsNumber(value.NaN) -> "NaN"
    value.JsNumber(value.Infinity) -> "Infinity"
    value.JsNumber(value.NegInfinity) -> "-Infinity"
    value.JsString(s) -> "'" <> escape_string(s) <> "'"
    value.JsSymbol(id) ->
      "Symbol(" <> option.unwrap(value.symbol_description(id), "") <> ")"
    value.JsBigInt(value.BigInt(n)) -> int.to_string(n) <> "n"
    value.JsUninitialized -> "<uninitialized>"
    value.JsObject(value.Ref(id:) as ref) ->
      case set.contains(visited, id) {
        True -> "[Circular]"
        False -> inspect_object(heap, ref, depth, set.insert(visited, id))
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
  heap: Heap(host),
  ref: value.Ref,
  depth: Int,
  visited: set.Set(Int),
) -> String {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind:, properties:, elements:, symbol_properties:, ..)) ->
      case kind {
        ArrayObject(length:) ->
          inspect_array(heap, elements, length, depth, visited)
        FunctionObject(..) | NativeFunction(..) -> {
          let name = case dict.get(properties, Named("name")) {
            Ok(DataProperty(value: JsString(n), ..)) -> n
            _ -> ""
          }
          case name {
            "" -> "[Function (anonymous)]"
            n -> "[Function: " <> n <> "]"
          }
        }
        PromiseObject(_) -> "Promise {}"
        value.ProxyObject(callable: True, ..) -> "[Function (Proxy)]"
        value.ProxyObject(..) -> "Proxy {}"
        GeneratorObject(_) -> "Object [Generator] {}"
        value.AsyncGeneratorObject(_) -> "Object [AsyncGenerator] {}"
        value.ArgumentsObject(length:) ->
          "[Arguments] "
          <> inspect_array(heap, elements, length, depth, visited)
        value.StringObject(value: s) -> "[String: '" <> escape_string(s) <> "']"
        value.NumberObject(value: n) ->
          "[Number: " <> inspect_inner(JsNumber(n), heap, depth, visited) <> "]"
        value.BooleanObject(value: True) -> "[Boolean: true]"
        value.BooleanObject(value: False) -> "[Boolean: false]"
        value.BigIntObject(value: bi) ->
          "[BigInt: "
          <> inspect_inner(value.JsBigInt(bi), heap, depth, visited)
          <> "]"
        value.SymbolObject(value: sym) ->
          "[Symbol: "
          <> inspect_inner(value.JsSymbol(sym), heap, depth, visited)
          <> "]"
        value.MapObject(store:) ->
          "Map(" <> int.to_string(ordered_entries.size(store)) <> ")"
        value.SetObject(store:) ->
          "Set(" <> int.to_string(ordered_entries.size(store)) <> ")"
        value.WeakMapObject(_) -> "WeakMap {}"
        value.WeakSetObject(_) -> "WeakSet {}"
        value.FinalizationRegistryObject(..) -> "FinalizationRegistry {}"
        value.ArrayIteratorObject(..) -> "Object [Array Iterator] {}"
        value.StringIteratorObject(..) -> "Object [String Iterator] {}"
        value.RegExpStringIteratorObject(..) ->
          "Object [RegExp String Iterator] {}"
        value.SetIteratorObject(..) -> "Object [Set Iterator] {}"
        value.MapIteratorObject(..) -> "Object [Map Iterator] {}"
        value.AsyncFromSyncIteratorObject(..) ->
          "Object [Async-from-Sync Iterator] {}"
        value.DateObject(time_value:) ->
          case time_value {
            value.Finite(f) -> "Date(" <> value.js_format_number(f) <> ")"
            _ -> "Invalid Date"
          }
        value.RegExpObject(pattern:, flags:) -> {
          let source = case pattern {
            "" -> "(?:)"
            p -> p
          }
          "/" <> source <> "/" <> flags
        }
        value.DataViewObject(..) -> "DataView {}"
        value.ArrayBufferObject(storage: value.Shared(..) as storage) ->
          "SharedArrayBuffer { byteLength: "
          <> int.to_string(value.buffer_byte_size(storage))
          <> " }"
        // Detached storage has no bytes, so byteLength is +0.
        value.ArrayBufferObject(storage:) ->
          "ArrayBuffer { byteLength: "
          <> int.to_string(value.buffer_byte_size(storage))
          <> " }"
        value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:) ->
          value.typed_array_name(elem_kind)
          <> "("
          <> int.to_string(typed_array_elements.view_length(
            heap,
            typed_array_elements.ViewSlot(
              buffer:,
              elem_kind:,
              byte_offset:,
              length:,
            ),
          ))
          <> ")"
        value.IteratorHelperObject(..) -> "[Iterator Helper]"
        value.WrapForValidIteratorObject(..) -> "[Iterator]"
        value.TemporalDateSlot(..) -> "Temporal.PlainDate {}"
        value.TemporalTimeSlot(..) -> "Temporal.PlainTime {}"
        value.TemporalDateTimeSlot(..) -> "Temporal.PlainDateTime {}"
        value.TemporalYearMonthSlot(..) -> "Temporal.PlainYearMonth {}"
        value.TemporalMonthDaySlot(..) -> "Temporal.PlainMonthDay {}"
        value.TemporalDurationSlot(..) -> "Temporal.Duration {}"
        value.TemporalInstantSlot(..) -> "Temporal.Instant {}"
        value.TemporalZonedDateTimeSlot(..) -> "Temporal.ZonedDateTime {}"
        value.ModuleNamespace(exports:) ->
          "[Module: { "
          <> string.join(list.sort(dict.keys(exports), string.compare), ", ")
          <> " }]"
        value.IteratorRecordObject(..) -> "[Iterator]"
        value.IntlObject(data:) ->
          case value.intl_service(data) {
            value.IntlLocale -> "[Intl.Locale]"
            value.IntlCollator -> "[Intl.Collator]"
            value.IntlNumberFormat -> "[Intl.NumberFormat]"
            value.IntlDateTimeFormat -> "[Intl.DateTimeFormat]"
            value.IntlPluralRules -> "[Intl.PluralRules]"
            value.IntlListFormat -> "[Intl.ListFormat]"
            value.IntlRelativeTimeFormat -> "[Intl.RelativeTimeFormat]"
            value.IntlSegmenter -> "[Intl.Segmenter]"
            value.IntlDisplayNames -> "[Intl.DisplayNames]"
            value.IntlDurationFormat -> "[Intl.DurationFormat]"
            value.IntlSegments -> "[Intl Segments]"
            value.IntlSegmentIterator -> "[Intl Segment Iterator]"
          }
        value.DisposableStackObject(..) -> "DisposableStack {}"
        value.ShadowRealmObject(..) -> "ShadowRealm {}"
        // The rawJSON box's only own property is "rawJSON", so render it as
        // the source text it stands for.
        value.RawJsonObject(raw:) -> "[RawJSON " <> raw <> "]"
        // Host objects have no own properties; they render via their
        // prototype's Symbol.toStringTag (e.g. `Pid {}`), exactly like a
        // tagged ordinary object.
        OrdinaryObject | value.ErrorObject(_) | value.HostObject(_) -> {
          // Error instances render as "Name: message" (or the full stack, once
          // we capture one); everything else as a plain object, prefixed with
          // its Symbol.toStringTag when one is set.
          case error_display(heap, ref) {
            Some(s) -> s
            None -> {
              let body = inspect_plain_object(heap, properties, depth, visited)
              case
                list.key_find(symbol_properties, value.symbol_to_string_tag)
              {
                Ok(DataProperty(value: JsString(t), ..)) ->
                  "Object [" <> t <> "] " <> body
                _ -> body
              }
            }
          }
        }
      }
    _ -> "[Object]"
  }
}

fn inspect_array(
  heap: Heap(host),
  elements: JsElements,
  length: Int,
  depth: Int,
  visited: set.Set(Int),
) -> String {
  case depth > 2 {
    True -> "[Array]"
    False -> {
      let items =
        inspect_array_loop(heap, elements, 0, length, depth, visited, [])
      "[ " <> string.join(items, ", ") <> " ]"
    }
  }
}

fn inspect_array_loop(
  heap: Heap(host),
  elements: JsElements,
  idx: Int,
  length: Int,
  depth: Int,
  visited: set.Set(Int),
  acc: List(String),
) -> List(String) {
  case idx >= length {
    True -> list.reverse(acc)
    False -> {
      let item =
        elements.get_option(elements, idx)
        |> option.map(inspect_inner(_, heap, depth + 1, visited))
        |> option.unwrap("<empty>")
      inspect_array_loop(heap, elements, idx + 1, length, depth, visited, [
        item,
        ..acc
      ])
    }
  }
}

fn inspect_plain_object(
  heap: Heap(host),
  properties: dict.Dict(PropertyKey, value.Property),
  depth: Int,
  visited: set.Set(Int),
) -> String {
  case depth > 2 {
    True -> "[Object]"
    False -> {
      let entries =
        value.ordered_property_pairs(properties)
        |> list.filter_map(fn(pair) {
          let #(key, prop) = pair
          case prop {
            DataProperty(enumerable: True, value: val, ..) ->
              Ok(
                key.key_display_string(key)
                <> ": "
                <> inspect_inner(val, heap, depth + 1, visited),
              )
            _ -> Error(Nil)
          }
        })
      case entries {
        [] -> "{}"
        _ -> "{ " <> string.join(entries, ", ") <> " }"
      }
    }
  }
}

/// Format a value for an uncaught-exception / unhandled-rejection report.
/// Error instances become "Name: message" (or their `stack`, once we capture
/// one); thrown strings are shown raw (browser-style "Uncaught boom"); anything
/// else falls back to `inspect`. Read-only — never invokes JS.
pub fn format_error(val: value.JsValue, heap: Heap(host)) -> String {
  case val {
    JsString(s) -> s
    JsObject(ref) ->
      error_display(heap, ref) |> option.unwrap(inspect(val, heap))
    _ -> inspect(val, heap)
  }
}

/// If `ref` is an Error instance, render it for display, else None.
///
/// Errors with a captured trace render as that trace (it already embeds the
/// "Name: message" header, V8-style); otherwise we synthesize the header from
/// `name` and `message` per `Error.prototype.toString` (§20.5.3.4). The trace
/// lives in the [[ErrorData]] slot (ErrorObject kind); an own `stack` data
/// property (Error.captureStackTrace targets) is honored as a fallback.
fn error_display(heap: Heap(host), ref: value.Ref) -> Option(String) {
  use <- bool.guard(!is_error(heap, ref), None)
  let slot_stack = case heap.read(heap, ref) {
    Some(ObjectSlot(kind: value.ErrorObject(stack:), ..)) if stack != "" ->
      Some(stack)
    _ -> None
  }
  case option.or(slot_stack, error_property(heap, ref, "stack")) {
    Some(stack) -> Some(stack)
    None -> {
      let name = error_property(heap, ref, "name") |> option.unwrap("Error")
      let message = error_property(heap, ref, "message") |> option.unwrap("")
      Some(case name, message {
        "", _ -> message
        _, "" -> name
        _, _ -> name <> ": " <> message
      })
    }
  }
}

/// Read-only test for whether `ref` is an Error instance: the [[ErrorData]]
/// internal slot (ErrorObject kind), or — for error-shaped objects built
/// without the slot — some object in its *prototype* chain owning a `message`
/// property (the marker carried by `Error.prototype`). Checking the prototype
/// chain (not the instance's own properties) correctly excludes plain objects
/// like `{ message: "x" }`.
fn is_error(heap: Heap(host), ref: value.Ref) -> Bool {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind: value.ErrorObject(_), ..)) -> True
    Some(ObjectSlot(prototype: Some(proto_ref), ..)) ->
      prototype_owns_message(heap, proto_ref, 100)
    _ -> False
  }
}

fn prototype_owns_message(heap: Heap(host), ref: value.Ref, fuel: Int) -> Bool {
  use <- bool.guard(fuel <= 0, False)
  case heap.read(heap, ref) {
    Some(ObjectSlot(properties:, prototype:, ..)) ->
      case dict.has_key(properties, Named("message")) {
        True -> True
        False ->
          case prototype {
            Some(parent) -> prototype_owns_message(heap, parent, fuel - 1)
            None -> False
          }
      }
    _ -> False
  }
}

/// Read a string-valued data property by walking the prototype chain (own
/// shadows inherited, like [[Get]]). Returns None when absent or non-string.
fn error_property(
  heap: Heap(host),
  ref: value.Ref,
  key: String,
) -> Option(String) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(properties:, prototype:, ..)) ->
      case dict.get(properties, Named(key)) {
        Ok(DataProperty(value: JsString(s), ..)) -> Some(s)
        Ok(_) -> None
        Error(Nil) ->
          case prototype {
            Some(parent) -> error_property(heap, parent, key)
            None -> None
          }
      }
    _ -> None
  }
}
