import arc/vm/builtins/common
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type ArcNativeFn, type JsValue, type PortableMessage, type Ref, ArcLog,
  ArcNative, ArcPeek, ArcPidToString, ArcReceive, ArcSelf, ArcSend, ArcSleep,
  DataProperty, JsBigInt, JsBool, JsNull, JsNumber, JsObject, JsString, JsSymbol,
  JsUndefined, JsUninitialized, ObjectSlot, OrdinaryObject, PidObject, PmArray,
  PmBigInt, PmBool, PmNull, PmNumber, PmObject, PmPid, PmString, PmSymbol,
  PmUndefined, PromiseFulfilled, PromiseObject, PromisePending, PromiseRejected,
  PromiseSlot,
}
import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set
import gleam/string

// -- FFI declarations --------------------------------------------------------

@external(erlang, "erlang", "self")
fn ffi_self() -> value.ErlangPid

@external(erlang, "arc_vm_ffi", "send_message")
fn ffi_send(pid: value.ErlangPid, msg: PortableMessage) -> Nil

@external(erlang, "arc_vm_ffi", "receive_message_infinite")
fn ffi_receive_infinite() -> PortableMessage

@external(erlang, "arc_vm_ffi", "receive_message_timeout")
fn ffi_receive_timeout(timeout: Int) -> Result(PortableMessage, Nil)

/// Returns the pid in the format `<x.x.x>
@external(erlang, "arc_vm_ffi", "pid_to_string")
pub fn ffi_pid_to_string(pid: value.ErlangPid) -> String

@external(erlang, "arc_vm_ffi", "sleep")
fn ffi_sleep(ms: Int) -> Nil

// -- Init --------------------------------------------------------------------

pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("peek", ArcNative(ArcPeek), 1),
      #("spawn", value.VmNative(value.ArcSpawn), 1),
      #("send", ArcNative(ArcSend), 2),
      #("receive", ArcNative(ArcReceive), 0),
      #("self", ArcNative(ArcSelf), 0),
      #("log", ArcNative(ArcLog), 1),
      #("sleep", ArcNative(ArcSleep), 1),
    ])

  let properties = dict.from_list(methods)
  let symbol_properties =
    dict.from_list([
      #(
        value.symbol_to_string_tag,
        value.data(JsString("Arc")) |> value.configurable(),
      ),
    ])

  let #(h, arc_ref) =
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
  let h = heap.root(h, arc_ref)

  #(h, arc_ref)
}

/// Per-module dispatch for Arc native functions.
pub fn dispatch(
  native: ArcNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    value.ArcPeek -> peek(args, state)
    value.ArcSend -> send(args, state)
    value.ArcReceive -> receive_(args, state)
    value.ArcSelf -> self_(args, state)
    value.ArcLog -> log(args, state)
    value.ArcSleep -> sleep(args, state)
    value.ArcPidToString -> pid_to_string(this, args, state)
  }
}

// -- Arc.peek ----------------------------------------------------------------

/// Non-standard: Arc.peek(promise)
/// Returns {type: 'pending'} | {type: 'resolved', value} | {type: 'rejected', reason}
pub fn peek(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let arg = case args {
    [a, ..] -> a
    [] -> JsUndefined
  }

  case read_promise_state(state.heap, arg) {
    Some(promise_state) -> {
      let props = case promise_state {
        PromisePending -> [#("type", value.data_property(JsString("pending")))]
        PromiseFulfilled(value:) -> [
          #("type", value.data_property(JsString("resolved"))),
          #("value", value.data_property(value)),
        ]
        PromiseRejected(reason:) -> [
          #("type", value.data_property(JsString("rejected"))),
          #("reason", value.data_property(reason)),
        ]
      }

      let #(heap, result_ref) =
        common.alloc_pojo(state.heap, state.builtins.object.prototype, props)
      #(State(..state, heap:), Ok(JsObject(result_ref)))
    }
    None -> {
      let #(heap, err) =
        common.make_type_error(
          state.heap,
          state.builtins,
          "Arc.peek: argument is not a Promise",
        )
      #(State(..state, heap:), Error(err))
    }
  }
}

fn read_promise_state(
  h: Heap,
  val: JsValue,
) -> option.Option(value.PromiseState) {
  use ref <- option.then(case val {
    JsObject(r) -> Some(r)
    _ -> None
  })
  use data_ref <- option.then(case heap.read(h, ref) {
    Some(ObjectSlot(kind: PromiseObject(promise_data:), ..)) ->
      Some(promise_data)
    _ -> None
  })
  case heap.read(h, data_ref) {
    Some(PromiseSlot(state:, ..)) -> Some(state)
    _ -> None
  }
}

// -- Arc.send ----------------------------------------------------------------

/// Sends a message to a BEAM process. The message is serialized into a
/// portable form (only primitives, plain objects, arrays, and PIDs are
/// supported). Returns the sent message value.
/// Throws TypeError if pid is not a Pid or message is not serializable.
pub fn send(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(pid_arg, msg_arg) = case args {
    [p, m, ..] -> #(p, m)
    [p] -> #(p, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }

  let result = {
    use ref <- result.try(case pid_arg {
      JsObject(ref) -> Ok(ref)
      _ -> Error("Arc.send: first argument is not a Pid")
    })

    use pid <- result.try(case heap.read(state.heap, ref) {
      Some(ObjectSlot(kind: PidObject(pid:), ..)) -> Ok(pid)
      _ -> Error("Arc.send: first argument is not a Pid")
    })

    use portable <- result.try(
      serialize(state.heap, msg_arg)
      |> result.map_error(fn(reason) { "Arc.send: " <> reason }),
    )

    ffi_send(pid, portable)
    Ok(msg_arg)
  }

  case result {
    Ok(val) -> #(state, Ok(val))
    Error(msg) -> {
      let #(heap, err) = common.make_type_error(state.heap, state.builtins, msg)
      #(State(..state, heap:), Error(err))
    }
  }
}

// -- Arc.receive -------------------------------------------------------------

/// Non-standard: Arc.receive(timeout?)
/// Blocks the current BEAM process waiting for a message. If timeout is
/// provided (in ms), returns undefined on timeout. Without timeout, blocks
/// forever.
pub fn receive_(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [JsNumber(value.Finite(n)), ..] -> {
      let ms = value.float_to_int(n)
      case ms >= 0 {
        True ->
          case ffi_receive_timeout(ms) {
            Ok(msg) -> {
              let #(heap, val) = deserialize(state.heap, state.builtins, msg)
              #(State(..state, heap:), Ok(val))
            }
            Error(Nil) -> #(state, Ok(JsUndefined))
          }
        False -> #(state, Ok(JsUndefined))
      }
    }
    _ -> {
      // No timeout — block forever
      let msg = ffi_receive_infinite()
      let #(heap, val) = deserialize(state.heap, state.builtins, msg)
      #(State(..state, heap:), Ok(val))
    }
  }
}

// -- Arc.self ----------------------------------------------------------------

/// Non-standard: Arc.self()
/// Returns a Pid object representing the current BEAM process.
pub fn self_(
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let pid = ffi_self()
  let #(heap, pid_val) =
    alloc_pid_object(
      state.heap,
      state.builtins.object.prototype,
      state.builtins.function.prototype,
      pid,
    )
  #(State(..state, heap:), Ok(pid_val))
}

// -- Arc.log -----------------------------------------------------------------

/// Non-standard: Arc.log(...args)
/// Prints values to stdout, space-separated, with a newline.
/// Similar to console.log but available in spawned processes.
pub fn log(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, parts) = log_stringify_args(args, state, [])
  io.println(string.join(parts, " "))
  #(state, Ok(JsUndefined))
}

fn log_stringify_args(
  args: List(JsValue),
  state: State,
  acc: List(String),
) -> #(State, List(String)) {
  case args {
    [] -> #(state, list.reverse(acc))
    [arg, ..rest] -> {
      let #(state, s) = log_stringify_one(arg, state)
      log_stringify_args(rest, state, [s, ..acc])
    }
  }
}

fn log_stringify_one(val: JsValue, state: State) -> #(State, String) {
  case val {
    JsUndefined -> #(state, "undefined")
    JsNull -> #(state, "null")
    JsBool(True) -> #(state, "true")
    JsBool(False) -> #(state, "false")
    JsNumber(value.Finite(n)) -> #(state, value.js_format_number(n))
    JsNumber(value.NaN) -> #(state, "NaN")
    JsNumber(value.Infinity) -> #(state, "Infinity")
    JsNumber(value.NegInfinity) -> #(state, "-Infinity")
    JsString(s) -> #(state, s)
    JsBigInt(value.BigInt(n)) -> #(state, string.inspect(n) <> "n")
    JsSymbol(_) -> #(state, "Symbol()")
    JsUninitialized -> #(state, "undefined")
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: PidObject(pid:), ..)) -> #(
          state,
          "Pid" <> ffi_pid_to_string(pid),
        )
        _ -> {
          // Try to convert to string via toString
          case frame.to_string(state, val) {
            Ok(#(s, state)) -> #(state, s)
            Error(#(_, state)) -> #(state, "[object Object]")
          }
        }
      }
  }
}

// -- Arc.sleep ---------------------------------------------------------------

/// Non-standard: Arc.sleep(ms)
/// Suspends the current BEAM process for the given number of milliseconds.
/// Maps directly to Erlang's timer:sleep/1.
pub fn sleep(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let ms = case args {
    [JsNumber(value.Finite(n)), ..] -> value.float_to_int(n)
    _ -> 0
  }
  case ms > 0 {
    True -> ffi_sleep(ms)
    False -> Nil
  }
  #(state, Ok(JsUndefined))
}

// -- Pid helpers -------------------------------------------------------------

/// Allocate a PidObject on the heap wrapping an Erlang PID.
pub fn alloc_pid_object(
  heap: Heap,
  object_proto: Ref,
  function_proto: Ref,
  pid: value.ErlangPid,
) -> #(Heap, JsValue) {
  let #(heap, to_string_ref) =
    common.alloc_native_fn(
      heap,
      function_proto,
      ArcNative(ArcPidToString),
      "toString",
      0,
    )
  let #(heap, ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        kind: PidObject(pid:),
        properties: dict.from_list([
          #("toString", value.builtin_property(JsObject(to_string_ref))),
        ]),
        elements: js_elements.new(),
        prototype: Some(object_proto),
        symbol_properties: dict.from_list([
          #(
            value.symbol_to_string_tag,
            value.data(JsString("Pid")) |> value.configurable(),
          ),
        ]),
        extensible: True,
      ),
    )
  #(heap, JsObject(ref))
}

/// Pid toString — returns "Pid<0.83.0>" when called on a PidObject.
pub fn pid_to_string(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: PidObject(pid:), ..)) -> #(
          state,
          Ok(JsString("Pid" <> ffi_pid_to_string(pid))),
        )
        _ -> frame.type_error(state, "Dead Pid")
      }
    _ -> frame.type_error(state, "Invalid Pid object")
  }
}

// -- Message serialization ---------------------------------------------------

/// Serialize a JsValue into a PortableMessage for cross-process transfer.
/// Only supports primitives, plain objects, arrays, and PIDs.
/// Returns Error(reason) for unsupported types (functions, promises, etc.).
pub fn serialize(heap: Heap, val: JsValue) -> Result(PortableMessage, String) {
  serialize_inner(heap, val, set.new())
}

fn serialize_inner(
  heap: Heap,
  val: JsValue,
  seen: set.Set(Int),
) -> Result(PortableMessage, String) {
  case val {
    JsUndefined -> Ok(PmUndefined)
    JsNull -> Ok(PmNull)
    JsBool(b) -> Ok(PmBool(b))
    JsNumber(n) -> Ok(PmNumber(n))
    JsString(s) -> Ok(PmString(s))
    JsBigInt(n) -> Ok(PmBigInt(n))
    JsObject(ref) -> serialize_heap_object(heap, ref, seen)
    JsSymbol(id) -> Ok(value.PmSymbol(id))
    JsUninitialized -> Error("cannot send uninitialized value")
  }
}

fn serialize_heap_object(
  heap: Heap,
  ref: Ref,
  seen: set.Set(Int),
) -> Result(PortableMessage, String) {
  case set.contains(seen, ref.id) {
    True -> Error("cannot send circular structure between processes")
    False -> {
      let seen = set.insert(seen, ref.id)
      case heap.read(heap, ref) {
        Some(ObjectSlot(kind: value.ArrayObject(length:), elements:, ..)) ->
          serialize_array(heap, elements, length, 0, seen, [])
        Some(ObjectSlot(
          kind: OrdinaryObject,
          properties:,
          symbol_properties:,
          ..,
        )) -> {
          use props <- result.try(
            serialize_object_props(heap, dict.to_list(properties), seen, []),
          )
          use sym_props <- result.try(
            serialize_symbol_props(
              heap,
              dict.to_list(symbol_properties),
              seen,
              [],
            ),
          )
          Ok(PmObject(properties: props, symbol_properties: sym_props))
        }
        Some(ObjectSlot(kind: PidObject(pid:), ..)) -> Ok(PmPid(pid))
        _ -> Error("cannot send functions or special objects between processes")
      }
    }
  }
}

fn serialize_array(
  heap: Heap,
  elements: value.JsElements,
  length: Int,
  i: Int,
  seen: set.Set(Int),
  acc: List(PortableMessage),
) -> Result(PortableMessage, String) {
  case i >= length {
    True -> Ok(PmArray(list.reverse(acc)))
    False -> {
      let val =
        js_elements.get_option(elements, i) |> option.unwrap(JsUndefined)
      use pm <- result.try(serialize_inner(heap, val, seen))
      serialize_array(heap, elements, length, i + 1, seen, [pm, ..acc])
    }
  }
}

fn serialize_object_props(
  heap: Heap,
  entries: List(#(String, value.Property)),
  seen: set.Set(Int),
  acc: List(#(String, PortableMessage)),
) -> Result(List(#(String, PortableMessage)), String) {
  case entries {
    [] -> Ok(list.reverse(acc))
    [#(key, DataProperty(value: val, enumerable: True, ..)), ..rest] -> {
      use pm <- result.try(serialize_inner(heap, val, seen))
      serialize_object_props(heap, rest, seen, [#(key, pm), ..acc])
    }
    [#(key, DataProperty(enumerable: False, ..)), ..] ->
      Error(
        "cannot send object with non-enumerable property \""
        <> key
        <> "\" between processes",
      )
    [#(key, value.AccessorProperty(..)), ..] ->
      Error(
        "cannot send object with accessor property \""
        <> key
        <> "\" between processes",
      )
  }
}

fn serialize_symbol_props(
  heap: Heap,
  entries: List(#(value.SymbolId, value.Property)),
  seen: set.Set(Int),
  acc: List(#(value.SymbolId, PortableMessage)),
) -> Result(List(#(value.SymbolId, PortableMessage)), String) {
  case entries {
    [] -> Ok(list.reverse(acc))
    [#(key, DataProperty(value: val, ..)), ..rest] -> {
      use pm <- result.try(serialize_inner(heap, val, seen))
      serialize_symbol_props(heap, rest, seen, [#(key, pm), ..acc])
    }
    [#(_key, value.AccessorProperty(..)), ..] ->
      Error(
        "cannot send object with accessor symbol property between processes",
      )
  }
}

// -- Message deserialization -------------------------------------------------

/// Deserialize a PortableMessage into a JsValue, allocating objects on the heap.
pub fn deserialize(
  heap: Heap,
  builtins: common.Builtins,
  msg: PortableMessage,
) -> #(Heap, JsValue) {
  case msg {
    PmUndefined -> #(heap, JsUndefined)
    PmNull -> #(heap, JsNull)
    PmBool(b) -> #(heap, JsBool(b))
    PmNumber(n) -> #(heap, JsNumber(n))
    PmString(s) -> #(heap, JsString(s))
    PmBigInt(n) -> #(heap, JsBigInt(n))
    PmSymbol(id) -> #(heap, JsSymbol(id))
    PmPid(pid) ->
      alloc_pid_object(
        heap,
        builtins.object.prototype,
        builtins.function.prototype,
        pid,
      )
    PmArray(items) -> {
      let #(heap, values) = deserialize_list(heap, builtins, items)
      let #(heap, ref) =
        common.alloc_array(heap, values, builtins.array.prototype)
      #(heap, JsObject(ref))
    }
    PmObject(properties: entries, symbol_properties: sym_entries) -> {
      let #(heap, props) = deserialize_object_entries(heap, builtins, entries)
      let #(heap, sym_props) =
        deserialize_symbol_entries(heap, builtins, sym_entries)
      let #(heap, ref) =
        heap.alloc(
          heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.from_list(props),
            elements: js_elements.new(),
            prototype: Some(builtins.object.prototype),
            symbol_properties: dict.from_list(sym_props),
            extensible: True,
          ),
        )
      #(heap, JsObject(ref))
    }
  }
}

fn deserialize_list(
  heap: Heap,
  builtins: common.Builtins,
  items: List(PortableMessage),
) -> #(Heap, List(JsValue)) {
  let #(heap, rev) =
    list.fold(items, #(heap, []), fn(acc, item) {
      let #(heap, vals) = acc
      let #(heap, val) = deserialize(heap, builtins, item)
      #(heap, [val, ..vals])
    })
  #(heap, list.reverse(rev))
}

fn deserialize_symbol_entries(
  heap: Heap,
  builtins: common.Builtins,
  entries: List(#(value.SymbolId, PortableMessage)),
) -> #(Heap, List(#(value.SymbolId, value.Property))) {
  let #(heap, rev) =
    list.fold(entries, #(heap, []), fn(acc, entry) {
      let #(heap, props) = acc
      let #(key, pm) = entry
      let #(heap, val) = deserialize(heap, builtins, pm)
      #(heap, [#(key, value.data_property(val)), ..props])
    })
  #(heap, list.reverse(rev))
}

fn deserialize_object_entries(
  heap: Heap,
  builtins: common.Builtins,
  entries: List(#(String, PortableMessage)),
) -> #(Heap, List(#(String, value.Property))) {
  let #(heap, rev) =
    list.fold(entries, #(heap, []), fn(acc, entry) {
      let #(heap, props) = acc
      let #(key, pm) = entry
      let #(heap, val) = deserialize(heap, builtins, pm)
      #(heap, [#(key, value.data_property(val)), ..props])
    })
  #(heap, list.reverse(rev))
}
