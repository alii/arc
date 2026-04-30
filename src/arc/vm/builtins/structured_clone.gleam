//// HTML structured clone (serialize/deserialize) plus the Subject object,
//// whose `.send`/`.receive` methods are mutually recursive with serialize/
//// deserialize and so must live in the same module.

import arc/vm/builtins/common
import arc/vm/builtins/dom_exception
import arc/vm/builtins/process_objects
import arc/vm/builtins/regexp
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type PortableMessage, type Ref, AccessorProperty, DataProperty,
  JsBigInt, JsBool, JsNull, JsNumber, JsObject, JsString, JsSymbol, JsUndefined,
  JsUninitialized, ObjectSlot, OrdinaryObject, PidObject, PortableMessage,
  PrArray, PrBooleanObject, PrDate, PrMap, PrNumberObject, PrObject, PrPid,
  PrRegExp, PrSet, PrStringObject, PrSubject, PvBigInt, PvBool, PvNull, PvNumber,
  PvRef, PvString, PvSymbol, PvUndefined, SelectorObject, SubjectObject,
  WellKnownSymbol,
}
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

// -- FFI declarations --------------------------------------------------------

@external(erlang, "arc_vm_ffi", "send_subject_message")
fn ffi_send_subject(
  pid: value.ErlangPid,
  tag: value.ErlangRef,
  msg: PortableMessage,
) -> Nil

@external(erlang, "arc_vm_ffi", "receive_subject_message")
fn ffi_receive_subject(tag: value.ErlangRef) -> PortableMessage

@external(erlang, "arc_vm_ffi", "receive_subject_message_timeout")
fn ffi_receive_subject_timeout(
  tag: value.ErlangRef,
  timeout: Int,
) -> Result(PortableMessage, Nil)

// -- Subject -----------------------------------------------------------------

/// Allocate a SubjectObject on the heap wrapping an Erlang PID + ref tag.
pub fn alloc_subject_object(
  heap: Heap,
  object_proto: Ref,
  function_proto: Ref,
  pid: value.ErlangPid,
  tag: value.ErlangRef,
) -> #(Heap, JsValue) {
  let #(heap, send_ref) =
    common.alloc_host_fn(heap, function_proto, subject_send, "send", 1)
  let #(heap, receive_ref) =
    common.alloc_host_fn(heap, function_proto, subject_receive, "receive", 0)
  let #(heap, to_string_ref) =
    common.alloc_host_fn(heap, function_proto, subject_to_string, "toString", 0)
  let #(heap, ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        kind: SubjectObject(pid:, tag:),
        properties: common.named_props([
          #("send", value.builtin_property(JsObject(send_ref))),
          #("receive", value.builtin_property(JsObject(receive_ref))),
          #("toString", value.builtin_property(JsObject(to_string_ref))),
        ]),
        elements: elements.new(),
        prototype: Some(object_proto),
        symbol_properties: [common.to_string_tag("Subject")],
        extensible: True,
      ),
    )
  #(heap, JsObject(ref))
}

/// subject.send(msg) — serialize and send a message to this subject's owner.
fn subject_send(
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let msg_arg = case args {
    [m, ..] -> m
    [] -> JsUndefined
  }
  case this {
    JsObject(ref) ->
      case heap.read_subject(state.heap, ref) {
        Some(#(pid, tag)) ->
          case serialize(state.heap, msg_arg, ArcClone) {
            Ok(portable) -> {
              ffi_send_subject(pid, tag, portable)
              #(state, Ok(JsUndefined))
            }
            Error(reason) -> {
              let #(heap, err) =
                common.make_type_error(
                  state.heap,
                  state.builtins,
                  "Subject.send: " <> reason,
                )
              #(State(..state, heap:), Error(err))
            }
          }
        None -> state.type_error(state, "Subject.send: this is not a Subject")
      }
    _ -> state.type_error(state, "Subject.send: this is not a Subject")
  }
}

/// subject.receive(timeout?) — blocking selective receive on this subject's tag.
fn subject_receive(
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read_subject(state.heap, ref) {
        Some(#(_pid, tag)) ->
          case args {
            [JsNumber(value.Finite(n)), ..] -> {
              let ms = value.float_to_int(n)
              case ms >= 0 {
                True ->
                  case ffi_receive_subject_timeout(tag, ms) {
                    Ok(pm) -> {
                      let #(heap, val) =
                        deserialize(state.heap, state.builtins, pm)
                      #(State(..state, heap:), Ok(val))
                    }
                    Error(Nil) -> #(state, Ok(JsUndefined))
                  }
                False -> #(state, Ok(JsUndefined))
              }
            }
            _ -> {
              let pm = ffi_receive_subject(tag)
              let #(heap, val) = deserialize(state.heap, state.builtins, pm)
              #(State(..state, heap:), Ok(val))
            }
          }
        None ->
          state.type_error(state, "Subject.receive: this is not a Subject")
      }
    _ -> state.type_error(state, "Subject.receive: this is not a Subject")
  }
}

/// subject.toString() — returns "Subject<0.83.0>".
fn subject_to_string(
  _args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read_subject(state.heap, ref) {
        Some(#(pid, _tag)) -> #(
          state,
          Ok(JsString("Subject" <> process_objects.ffi_pid_to_string(pid))),
        )
        None ->
          state.type_error(state, "Subject.toString: this is not a Subject")
      }
    _ -> state.type_error(state, "Subject.toString: this is not a Subject")
  }
}

// -- Message serialization ---------------------------------------------------

/// How strictly to enforce HTML §2.7.3 StructuredSerializeInternal.
pub type CloneMode {
  /// HTML structuredClone semantics: reject Symbol primitives, symbol-keyed
  /// properties, Function objects, and Arc platform objects (Pid/Subject/…).
  SpecClone
  /// Arc IPC semantics: also allow well-known symbols, PidObject,
  /// SubjectObject. Everything else as SpecClone.
  ArcClone
}

/// Threaded serialization state:
/// (heap-ref.id → record-id memo, records table, next record id).
type SerCtx =
  #(dict.Dict(Int, Int), dict.Dict(Int, value.PortableRecord), Int)

/// Serialize a JsValue into a PortableMessage per HTML
/// StructuredSerializeInternal. Heap objects are interned into a flat
/// `records` table keyed by integer id so cycles and shared identity survive
/// the round-trip; primitives are inlined.
pub fn serialize(
  heap: Heap,
  val: JsValue,
  mode: CloneMode,
) -> Result(PortableMessage, String) {
  let ctx = #(dict.new(), dict.new(), 0)
  use #(root, #(_memo, records, _next)) <- result.map(serialize_value(
    heap,
    val,
    mode,
    ctx,
  ))
  PortableMessage(root:, records:)
}

fn serialize_value(
  heap: Heap,
  val: JsValue,
  mode: CloneMode,
  ctx: SerCtx,
) -> Result(#(value.PortableValue, SerCtx), String) {
  case val {
    JsUndefined | JsUninitialized -> Ok(#(PvUndefined, ctx))
    JsNull -> Ok(#(PvNull, ctx))
    JsBool(b) -> Ok(#(PvBool(b), ctx))
    JsNumber(n) -> Ok(#(PvNumber(n), ctx))
    JsString(s) -> Ok(#(PvString(s), ctx))
    JsBigInt(n) -> Ok(#(PvBigInt(n), ctx))
    JsSymbol(WellKnownSymbol(id:)) ->
      case mode {
        ArcClone -> Ok(#(PvSymbol(id), ctx))
        SpecClone -> Error(uncloneable("Symbol"))
      }
    JsSymbol(_) -> Error(uncloneable("Symbol"))
    JsObject(ref) -> {
      let #(memo, records, next) = ctx
      case dict.get(memo, ref.id) {
        Ok(existing) -> Ok(#(PvRef(existing), ctx))
        Error(Nil) -> {
          let id = next
          // Memo BEFORE recursing so cycles resolve to PvRef(id).
          let ctx = #(dict.insert(memo, ref.id, id), records, next + 1)
          use #(record, #(memo, records, next)) <- result.map(serialize_object(
            heap,
            ref,
            mode,
            ctx,
          ))
          #(PvRef(id), #(memo, dict.insert(records, id, record), next))
        }
      }
    }
  }
}

fn serialize_object(
  heap: Heap,
  ref: Ref,
  mode: CloneMode,
  ctx: SerCtx,
) -> Result(#(value.PortableRecord, SerCtx), String) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind:, properties:, symbol_properties:, elements: els, ..)) ->
      serialize_kind(heap, kind, properties, symbol_properties, els, mode, ctx)
    // JsObject refs only ever point to ObjectSlot — anything else is heap
    // corruption. Bind so the diagnostic is actionable.
    Some(other) ->
      Error("internal slot " <> string.inspect(other) <> " could not be cloned")
    None -> Error("dangling reference could not be cloned")
  }
}

fn serialize_kind(
  heap: Heap,
  kind: value.ExoticKind(ctx),
  properties: dict.Dict(value.PropertyKey, value.Property),
  symbol_properties: List(#(value.SymbolId, value.Property)),
  els: value.JsElements,
  mode: CloneMode,
  ctx: SerCtx,
) -> Result(#(value.PortableRecord, SerCtx), String) {
  case kind {
    value.ArrayObject(length:) -> {
      use #(items, ctx) <- result.try(
        serialize_indexed(heap, els, length, 0, mode, ctx, []),
      )
      use #(props, ctx) <- result.map(
        serialize_props(heap, dict.to_list(properties), mode, ctx, []),
      )
      #(PrArray(items:, length:, properties: props), ctx)
    }
    OrdinaryObject -> {
      use #(props, ctx) <- result.try(
        serialize_props(heap, dict.to_list(properties), mode, ctx, []),
      )
      use #(syms, ctx) <- result.map(case mode {
        SpecClone -> Ok(#([], ctx))
        ArcClone -> serialize_sym_props(heap, symbol_properties, mode, ctx, [])
      })
      #(PrObject(properties: props, symbol_properties: syms), ctx)
    }
    value.MapObject(entries:, keys_rev:, ..) -> {
      let pairs =
        list.reverse(keys_rev)
        |> list.filter_map(fn(k) {
          dict.get(entries, k)
          |> result.map(fn(v) { #(value.map_key_to_js(k), v) })
        })
      use #(out, ctx) <- result.try(serialize_pairs(heap, pairs, mode, ctx, []))
      use #(props, ctx) <- result.map(
        serialize_props(heap, dict.to_list(properties), mode, ctx, []),
      )
      #(PrMap(entries: out, properties: props), ctx)
    }
    value.SetObject(data:, keys:) -> {
      let vals = list.reverse(keys) |> list.filter_map(dict.get(data, _))
      use #(out, ctx) <- result.try(serialize_list(heap, vals, mode, ctx, []))
      use #(props, ctx) <- result.map(
        serialize_props(heap, dict.to_list(properties), mode, ctx, []),
      )
      #(PrSet(entries: out, properties: props), ctx)
    }
    value.DateObject(time_value:) -> Ok(#(PrDate(time_value), ctx))
    value.RegExpObject(pattern:, flags:) ->
      Ok(#(PrRegExp(pattern:, flags:), ctx))
    value.BooleanObject(value: b) -> Ok(#(PrBooleanObject(b), ctx))
    value.NumberObject(value: n) -> Ok(#(PrNumberObject(n), ctx))
    value.StringObject(value: s) -> Ok(#(PrStringObject(s), ctx))
    PidObject(pid:) ->
      case mode {
        ArcClone -> Ok(#(PrPid(pid), ctx))
        SpecClone -> Error(uncloneable("Pid"))
      }
    SubjectObject(pid:, tag:) ->
      case mode {
        ArcClone -> Ok(#(PrSubject(pid:, tag:), ctx))
        SpecClone -> Error(uncloneable("Subject"))
      }
    value.FunctionObject(..) | value.NativeFunction(..) ->
      Error(uncloneable("Function"))
    value.PromiseObject(..) -> Error(uncloneable("Promise"))
    value.GeneratorObject(..) -> Error(uncloneable("Generator"))
    value.AsyncGeneratorObject(..) -> Error(uncloneable("AsyncGenerator"))
    value.WeakMapObject(..) -> Error(uncloneable("WeakMap"))
    value.WeakSetObject(..) -> Error(uncloneable("WeakSet"))
    SelectorObject(..) -> Error(uncloneable("Selector"))
    value.TimerObject(..) -> Error(uncloneable("Timer"))
    value.SymbolObject(..) -> Error(uncloneable("Symbol"))
    value.ArgumentsObject(..) -> Error(uncloneable("Arguments"))
    value.ArrayIteratorObject(..)
    | value.SetIteratorObject(..)
    | value.MapIteratorObject(..)
    | value.IteratorHelperObject(..)
    | value.WrapForValidIteratorObject(..)
    | value.AsyncFromSyncIteratorObject(..) -> Error(uncloneable("Iterator"))
  }
}

fn uncloneable(name: String) -> String {
  "#<" <> name <> "> could not be cloned"
}

/// Walk array elements 0..length, treating holes as undefined.
fn serialize_indexed(
  heap: Heap,
  els: value.JsElements,
  length: Int,
  i: Int,
  mode: CloneMode,
  ctx: SerCtx,
  acc: List(value.PortableValue),
) -> Result(#(List(value.PortableValue), SerCtx), String) {
  case i >= length {
    True -> Ok(#(list.reverse(acc), ctx))
    False -> {
      let v = elements.get_option(els, i) |> option.unwrap(JsUndefined)
      use #(pv, ctx) <- result.try(serialize_value(heap, v, mode, ctx))
      serialize_indexed(heap, els, length, i + 1, mode, ctx, [pv, ..acc])
    }
  }
}

fn serialize_list(
  heap: Heap,
  vals: List(JsValue),
  mode: CloneMode,
  ctx: SerCtx,
  acc: List(value.PortableValue),
) -> Result(#(List(value.PortableValue), SerCtx), String) {
  case vals {
    [] -> Ok(#(list.reverse(acc), ctx))
    [v, ..rest] -> {
      use #(pv, ctx) <- result.try(serialize_value(heap, v, mode, ctx))
      serialize_list(heap, rest, mode, ctx, [pv, ..acc])
    }
  }
}

fn serialize_pairs(
  heap: Heap,
  pairs: List(#(JsValue, JsValue)),
  mode: CloneMode,
  ctx: SerCtx,
  acc: List(#(value.PortableValue, value.PortableValue)),
) -> Result(
  #(List(#(value.PortableValue, value.PortableValue)), SerCtx),
  String,
) {
  case pairs {
    [] -> Ok(#(list.reverse(acc), ctx))
    [#(k, v), ..rest] -> {
      use #(pk, ctx) <- result.try(serialize_value(heap, k, mode, ctx))
      use #(pv, ctx) <- result.try(serialize_value(heap, v, mode, ctx))
      serialize_pairs(heap, rest, mode, ctx, [#(pk, pv), ..acc])
    }
  }
}

/// Serialize string/index-keyed own properties. Per HTML §2.7.3 step 26.1,
/// accessor and non-enumerable data properties are silently SKIPPED in both
/// modes (only enumerable own data properties are copied).
fn serialize_props(
  heap: Heap,
  entries: List(#(value.PropertyKey, value.Property)),
  mode: CloneMode,
  ctx: SerCtx,
  acc: List(#(value.PropertyKey, value.PortableValue)),
) -> Result(#(List(#(value.PropertyKey, value.PortableValue)), SerCtx), String) {
  case entries {
    [] -> Ok(#(list.reverse(acc), ctx))
    [#(key, DataProperty(value: v, enumerable: True, ..)), ..rest] -> {
      use #(pv, ctx) <- result.try(serialize_value(heap, v, mode, ctx))
      serialize_props(heap, rest, mode, ctx, [#(key, pv), ..acc])
    }
    [#(_, DataProperty(enumerable: False, ..)), ..rest]
    | [#(_, AccessorProperty(..)), ..rest] ->
      serialize_props(heap, rest, mode, ctx, acc)
  }
}

/// Serialize symbol-keyed own properties — ArcClone only. Same skip rules
/// as `serialize_props`.
fn serialize_sym_props(
  heap: Heap,
  entries: List(#(value.SymbolId, value.Property)),
  mode: CloneMode,
  ctx: SerCtx,
  acc: List(#(value.SymbolId, value.PortableValue)),
) -> Result(#(List(#(value.SymbolId, value.PortableValue)), SerCtx), String) {
  case entries {
    [] -> Ok(#(list.reverse(acc), ctx))
    [#(id, DataProperty(value: v, enumerable: True, ..)), ..rest] -> {
      use #(pv, ctx) <- result.try(serialize_value(heap, v, mode, ctx))
      serialize_sym_props(heap, rest, mode, ctx, [#(id, pv), ..acc])
    }
    [#(_, DataProperty(enumerable: False, ..)), ..rest]
    | [#(_, AccessorProperty(..)), ..rest] ->
      serialize_sym_props(heap, rest, mode, ctx, acc)
  }
}

// -- Message deserialization -------------------------------------------------

/// Record-id → heap Ref. Built fully in pass 1 before any child resolution,
/// so cyclic `PvRef`s always hit a live shell.
type DeMemo =
  dict.Dict(Int, Ref)

/// Deserialize a PortableMessage into a JsValue, allocating objects on the heap.
///
/// Two-pass to make cycles and shared identity work:
///   1. Allocate an empty shell for every record, building the id→Ref memo.
///   2. Fill container records' children by resolving PortableValues against
///      the now-complete memo, then mutate each shell in place.
pub fn deserialize(
  heap: Heap,
  builtins: common.Builtins,
  msg: PortableMessage,
) -> #(Heap, JsValue) {
  let PortableMessage(root:, records:) = msg
  // Pass 1: allocate a shell for every record so all refs exist.
  let #(heap, memo) =
    dict.fold(records, #(heap, dict.new()), fn(acc, id, record) {
      let #(heap, memo) = acc
      let #(heap, ref) = alloc_shell(heap, builtins, record)
      #(heap, dict.insert(memo, id, ref))
    })
  // Pass 2: fill container records now that every PvRef is resolvable.
  let heap =
    dict.fold(records, heap, fn(heap, id, record) {
      case dict.get(memo, id) {
        Ok(ref) -> fill_record(heap, memo, ref, record)
        // unreachable — pass 1 inserted every id
        Error(Nil) -> heap
      }
    })
  #(heap, resolve_value(root, memo))
}

/// Pass 1: allocate the right ExoticKind + prototype, no children yet.
/// Leaf kinds (Date/RegExp/wrappers/Pid/Subject) are fully built here since
/// they carry no PortableValue children.
fn alloc_shell(
  heap: Heap,
  builtins: common.Builtins,
  record: value.PortableRecord,
) -> #(Heap, Ref) {
  case record {
    PrObject(..) ->
      common.alloc_wrapper(heap, OrdinaryObject, builtins.object.prototype)
    PrArray(length:, ..) ->
      common.alloc_wrapper(
        heap,
        value.ArrayObject(length:),
        builtins.array.prototype,
      )
    PrMap(..) ->
      common.alloc_wrapper(
        heap,
        value.MapObject(entries: dict.new(), keys_rev: [], keys_len: 0),
        builtins.map.prototype,
      )
    PrSet(..) ->
      common.alloc_wrapper(
        heap,
        value.SetObject(data: dict.new(), keys: []),
        builtins.set.prototype,
      )
    PrDate(time_value) ->
      common.alloc_wrapper(
        heap,
        value.DateObject(time_value:),
        builtins.date.prototype,
      )
    PrRegExp(pattern:, flags:) ->
      regexp.alloc_regexp(heap, builtins.regexp.prototype, pattern, flags)
    PrBooleanObject(b) ->
      common.alloc_wrapper(
        heap,
        value.BooleanObject(b),
        builtins.boolean.prototype,
      )
    PrNumberObject(n) ->
      common.alloc_wrapper(
        heap,
        value.NumberObject(n),
        builtins.number.prototype,
      )
    PrStringObject(s) ->
      common.alloc_wrapper(
        heap,
        value.StringObject(s),
        builtins.string.prototype,
      )
    PrPid(pid) -> {
      let #(heap, v) =
        process_objects.alloc_pid_object(
          heap,
          builtins.object.prototype,
          builtins.function.prototype,
          pid,
        )
      let assert JsObject(ref) = v
      #(heap, ref)
    }
    PrSubject(pid:, tag:) -> {
      let #(heap, v) =
        alloc_subject_object(
          heap,
          builtins.object.prototype,
          builtins.function.prototype,
          pid,
          tag,
        )
      let assert JsObject(ref) = v
      #(heap, ref)
    }
  }
}

/// PortableValue → JsValue once the memo is complete. Pure, no heap writes.
fn resolve_value(pv: value.PortableValue, memo: DeMemo) -> JsValue {
  case pv {
    PvUndefined -> JsUndefined
    PvNull -> JsNull
    PvBool(b) -> JsBool(b)
    PvNumber(n) -> JsNumber(n)
    PvString(s) -> JsString(s)
    PvBigInt(n) -> JsBigInt(n)
    PvSymbol(id) -> JsSymbol(WellKnownSymbol(id:))
    PvRef(id) ->
      case dict.get(memo, id) {
        Ok(ref) -> JsObject(ref)
        // Dangling ref — only possible via a malformed message.
        Error(Nil) -> JsUndefined
      }
  }
}

fn resolve_props(
  entries: List(#(value.PropertyKey, value.PortableValue)),
  memo: DeMemo,
) -> dict.Dict(value.PropertyKey, value.Property) {
  list.map(entries, fn(e) {
    #(e.0, value.data_property(resolve_value(e.1, memo)))
  })
  |> dict.from_list
}

/// Pass 2: fill children of container records. Leaf kinds are no-ops.
fn fill_record(
  heap: Heap,
  memo: DeMemo,
  ref: Ref,
  record: value.PortableRecord,
) -> Heap {
  case record {
    PrObject(properties:, symbol_properties:) -> {
      let props = resolve_props(properties, memo)
      let syms =
        list.map(symbol_properties, fn(e) {
          #(e.0, value.data_property(resolve_value(e.1, memo)))
        })
      use slot <- heap.update(heap, ref)
      case slot {
        ObjectSlot(..) ->
          ObjectSlot(..slot, properties: props, symbol_properties: syms)
        other -> other
      }
    }
    PrArray(items:, properties:, ..) -> {
      let values = list.map(items, resolve_value(_, memo))
      let props = resolve_props(properties, memo)
      use slot <- heap.update(heap, ref)
      case slot {
        ObjectSlot(..) ->
          ObjectSlot(
            ..slot,
            elements: elements.from_list(values),
            properties: props,
          )
        other -> other
      }
    }
    PrMap(entries:, properties:) -> {
      let #(data, keys_rev, keys_len) =
        list.fold(entries, #(dict.new(), [], 0), fn(acc, e) {
          let #(data, keys_rev, n) = acc
          let k = value.js_to_map_key(resolve_value(e.0, memo))
          let v = resolve_value(e.1, memo)
          #(dict.insert(data, k, v), [k, ..keys_rev], n + 1)
        })
      let props = resolve_props(properties, memo)
      use slot <- heap.update(heap, ref)
      case slot {
        ObjectSlot(..) ->
          ObjectSlot(
            ..slot,
            kind: value.MapObject(entries: data, keys_rev:, keys_len:),
            properties: props,
          )
        other -> other
      }
    }
    PrSet(entries:, properties:) -> {
      let #(data, keys) =
        list.fold(entries, #(dict.new(), []), fn(acc, pv) {
          let #(data, keys) = acc
          let v = resolve_value(pv, memo)
          let k = value.js_to_map_key(v)
          #(dict.insert(data, k, v), [k, ..keys])
        })
      let props = resolve_props(properties, memo)
      use slot <- heap.update(heap, ref)
      case slot {
        ObjectSlot(..) ->
          ObjectSlot(
            ..slot,
            kind: value.SetObject(data:, keys:),
            properties: props,
          )
        other -> other
      }
    }
    // Leaf records — fully built in pass 1, nothing to fill.
    PrDate(..)
    | PrRegExp(..)
    | PrBooleanObject(..)
    | PrNumberObject(..)
    | PrStringObject(..)
    | PrPid(..)
    | PrSubject(..) -> heap
  }
}

// -- structuredClone ---------------------------------------------------------

/// HTML structuredClone(value) — serialize with SpecClone semantics then
/// deserialize on the same heap. No transfer-list support. Uncloneable values
/// throw a "DataCloneError" DOMException.
pub fn structured_clone(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = case args {
    [a, ..] -> a
    [] -> JsUndefined
  }
  case serialize(state.heap, val, SpecClone) {
    Ok(msg) -> {
      let #(heap, cloned) = deserialize(state.heap, state.builtins, msg)
      #(State(..state, heap:), Ok(cloned))
    }
    Error(reason) -> {
      let #(heap, err) =
        dom_exception.make(state.heap, state.builtins, "DataCloneError", reason)
      #(State(..state, heap:), Error(err))
    }
  }
}
