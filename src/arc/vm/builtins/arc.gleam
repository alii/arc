import arc/vm/builtins/common
import arc/vm/builtins/promise as builtins_promise
import arc/vm/heap.{type Heap}
import arc/vm/internal/elements
import arc/vm/state.{type State, State}
import arc/vm/value.{
  type ArcNativeFn, type JsValue, type MailboxEvent, type PortableMessage,
  type Ref, ArcLog, ArcNative, ArcPeek, ArcPidToString, ArcReceive,
  ArcReceiveAsync, ArcSelf, ArcSend, ArcSetTimeout, ArcSleep, DataProperty,
  JsBigInt, JsBool, JsNull, JsNumber, JsObject, JsString, JsSymbol, JsUndefined,
  JsUninitialized, ObjectSlot, OrdinaryObject, PidObject, PmArray, PmBigInt,
  PmBool, PmNull, PmNumber, PmObject, PmPid, PmString, PmSymbol, PmUndefined,
  PromiseFulfilled, PromisePending, PromiseRejected, SettlePromise, UserMessage,
}
import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string

// -- FFI declarations --------------------------------------------------------
// On the JS target these all panic — there's no BEAM mailbox to back them.

@external(erlang, "erlang", "self")
@external(javascript, "../../../arc_vm_ffi.mjs", "erlang_self")
fn ffi_self() -> value.ErlangPid

@external(erlang, "arc_vm_ffi", "send_message")
@external(javascript, "../../../arc_vm_ffi.mjs", "send_message")
fn ffi_send(pid: value.ErlangPid, msg: MailboxEvent) -> Nil

@external(erlang, "arc_vm_ffi", "receive_user_message")
@external(javascript, "../../../arc_vm_ffi.mjs", "receive_user_message")
fn ffi_receive_user() -> PortableMessage

@external(erlang, "arc_vm_ffi", "receive_user_message_timeout")
@external(javascript, "../../../arc_vm_ffi.mjs", "receive_user_message_timeout")
fn ffi_receive_user_timeout(timeout: Int) -> Result(PortableMessage, Nil)

/// Returns the pid in the format `<x.x.x>
@external(erlang, "arc_vm_ffi", "pid_to_string")
@external(javascript, "../../../arc_vm_ffi.mjs", "pid_to_string")
pub fn ffi_pid_to_string(pid: value.ErlangPid) -> String

@external(erlang, "arc_vm_ffi", "sleep")
@external(javascript, "../../../arc_vm_ffi.mjs", "sleep")
fn ffi_sleep(ms: Int) -> Nil

@external(erlang, "arc_vm_ffi", "send_after")
@external(javascript, "../../../arc_vm_ffi.mjs", "send_after")
fn ffi_send_after(
  ms: Int,
  pid: value.ErlangPid,
  msg: MailboxEvent,
) -> value.ErlangTimerRef

@external(erlang, "arc_vm_ffi", "cancel_timer")
@external(javascript, "../../../arc_vm_ffi.mjs", "cancel_timer")
fn ffi_cancel_timer(tref: value.ErlangTimerRef) -> Bool

// -- Init --------------------------------------------------------------------

pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("peek", ArcNative(ArcPeek), 1),
      #("spawn", value.VmNative(value.ArcSpawn), 1),
      #("send", ArcNative(ArcSend), 2),
      #("receive", ArcNative(ArcReceive), 0),
      #("receiveAsync", ArcNative(ArcReceiveAsync), 0),
      #("setTimeout", ArcNative(ArcSetTimeout), 2),
      #("clearTimeout", ArcNative(value.ArcClearTimeout), 1),
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
        elements: elements.new(),
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
    value.ArcReceiveAsync -> receive_async(args, state)
    value.ArcSetTimeout -> set_timeout(args, state)
    value.ArcClearTimeout -> clear_timeout(args, state)
    value.ArcSelf -> self_(args, state)
    value.ArcLog -> log(args, state)
    value.ArcSleep -> sleep(args, state)
    value.ArcPidToString -> pid_to_string(this, args, state)
  }
}

// -- Arc.peek ----------------------------------------------------------------

/// Arc.peek(promise)
/// Returns {type: 'pending'} | {type: 'resolved', value} | {type: 'rejected', reason}
fn peek(args: List(JsValue), state: State) -> #(State, Result(JsValue, JsValue)) {
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
  use data_ref <- option.then(heap.read_promise_data_ref(h, ref))
  heap.read_promise_state(h, data_ref)
}

// -- Arc.send ----------------------------------------------------------------

/// Sends a message to a BEAM process. The message is serialized into a
/// portable form (only primitives, plain objects, arrays, and PIDs are
/// supported). Returns the sent message value.
/// Throws TypeError if pid is not a Pid or message is not serializable.
fn send(args: List(JsValue), state: State) -> #(State, Result(JsValue, JsValue)) {
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

    use pid <- result.try(
      heap.read_pid(state.heap, ref)
      |> option.to_result("Arc.send: first argument is not a Pid"),
    )

    use portable <- result.try(
      serialize(state.heap, msg_arg)
      |> result.map_error(fn(reason) { "Arc.send: " <> reason }),
    )

    ffi_send(pid, UserMessage(portable))
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

/// Arc.receive(timeout?)
/// Blocks the current BEAM process waiting for a message. If timeout is
/// provided (in ms), returns undefined on timeout. Without timeout, blocks
/// forever.
///
/// Uses selective receive — only matches `UserMessage`, so `SettlePromise`
/// events stay in the mailbox for the event loop. Both blocking and async
/// receive share the same BEAM mailbox with no in-memory buffer.
fn receive_(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [JsNumber(value.Finite(n)), ..] -> {
      let ms = value.float_to_int(n)
      case ms >= 0 {
        True ->
          case ffi_receive_user_timeout(ms) {
            Ok(pm) -> {
              let #(heap, val) = deserialize(state.heap, state.builtins, pm)
              #(State(..state, heap:), Ok(val))
            }
            Error(Nil) -> #(state, Ok(JsUndefined))
          }
        False -> #(state, Ok(JsUndefined))
      }
    }
    _ -> {
      let pm = ffi_receive_user()
      let #(heap, val) = deserialize(state.heap, state.builtins, pm)
      #(State(..state, heap:), Ok(val))
    }
  }
}

// -- Arc.receiveAsync --------------------------------------------------------

/// Arc.receiveAsync(timeout?)
/// Returns a Promise that resolves with the next UserMessage to arrive in
/// this process's mailbox. Unlike `Arc.receive`, this does NOT block the
/// VM — the calling async function suspends at `await`, and other async
/// functions keep running via the event loop.
///
/// If `timeout` (ms) is given, the promise resolves with `undefined` when
/// it elapses without a message — same semantics as blocking
/// `Arc.receive(ms)`. The timeout message and the user message race in
/// the mailbox; whichever the event loop picks first wins, the other
/// becomes a no-op.
///
/// Messages that arrive before a receiver is waiting stay in the BEAM
/// mailbox (the event loop uses selective receive to skip them), so
/// blocking `Arc.receive()` and `receiveAsync` share the same mailbox
/// without any in-memory buffer.
///
/// Requires the event loop to be running (--event-loop flag or Arc.spawn).
/// Throws TypeError if the event loop is not enabled.
fn receive_async(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case state.event_loop {
    False -> {
      let #(heap, err) =
        common.make_type_error(
          state.heap,
          state.builtins,
          "Arc.receiveAsync() requires the event loop (--event-loop flag)",
        )
      #(State(..state, heap:), Error(err))
    }
    True -> receive_async_inner(args, state)
  }
}

fn receive_async_inner(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, obj_ref, data_ref) =
    builtins_promise.create_promise(
      state.heap,
      state.builtins.promise.prototype,
    )
  case args {
    [JsNumber(value.Finite(n)), ..] -> {
      let ms = value.float_to_int(n)
      case ms >= 0 {
        True -> {
          let _ =
            ffi_send_after(ms, ffi_self(), value.ReceiverTimeout(data_ref))
          Nil
        }
        False -> Nil
      }
    }
    _ -> Nil
  }
  #(
    State(
      ..state,
      heap:,
      pending_receivers: list.append(state.pending_receivers, [data_ref]),
      outstanding: state.outstanding + 1,
    ),
    Ok(JsObject(obj_ref)),
  )
}

// -- Arc.setTimeout ----------------------------------------------------------

/// Arc.setTimeout(fn, ms)
/// Schedules `fn` to be called after `ms` milliseconds. Returns undefined.
/// Works by creating a pending promise, telling BEAM to send a
/// SettlePromise message back to self after `ms`, and attaching `fn` as
/// the promise's fulfill handler — so when the timer fires, the event loop
/// resolves the promise, which schedules a reaction job that calls `fn`.
///
/// Requires the event loop to be running (--event-loop flag or Arc.spawn).
/// Throws TypeError if the event loop is not enabled.
fn set_timeout(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case state.event_loop {
    False -> {
      let #(heap, err) =
        common.make_type_error(
          state.heap,
          state.builtins,
          "Arc.setTimeout() requires the event loop (--event-loop flag)",
        )
      #(State(..state, heap:), Error(err))
    }
    True -> set_timeout_inner(args, state)
  }
}

fn set_timeout_inner(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(callback, ms) = case args {
    [cb, JsNumber(value.Finite(n)), ..] -> #(cb, value.float_to_int(n))
    [cb, ..] -> #(cb, 0)
    [] -> #(JsUndefined, 0)
  }
  let ms = case ms < 0 {
    True -> 0
    False -> ms
  }
  let #(heap, _obj_ref, data_ref) =
    builtins_promise.create_promise(
      state.heap,
      state.builtins.promise.prototype,
    )
  let state =
    builtins_promise.perform_promise_then(
      State(..state, heap:),
      data_ref,
      callback,
      JsUndefined,
      JsUndefined,
      JsUndefined,
    )
  let timer_ref =
    ffi_send_after(ms, ffi_self(), SettlePromise(data_ref, Ok(PmUndefined)))
  let #(heap, timer_obj) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: value.TimerObject(timer_ref:, data_ref:),
        properties: dict.new(),
        elements: elements.new(),
        prototype: Some(state.builtins.object.prototype),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  #(
    State(..state, heap:, outstanding: state.outstanding + 1),
    Ok(JsObject(timer_obj)),
  )
}

// -- Arc.clearTimeout --------------------------------------------------------

/// Arc.clearTimeout(timer)
/// Cancels a timer created by `Arc.setTimeout`. If the timer hasn't fired
/// yet, the callback will not be invoked and `outstanding` is decremented.
/// If it already fired (or `timer` isn't a timer), this is a no-op.
fn clear_timeout(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let cancelled =
    args
    |> list.first
    |> option.from_result
    |> option.then(as_timer_ref(state.heap, _))
    |> option.map(ffi_cancel_timer)
    |> option.unwrap(False)
  let state = case cancelled {
    True -> State(..state, outstanding: state.outstanding - 1)
    False -> state
  }
  #(state, Ok(JsUndefined))
}

fn as_timer_ref(h: Heap, val: JsValue) -> Option(value.ErlangTimerRef) {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: value.TimerObject(timer_ref:, ..), ..)) ->
          Some(timer_ref)
        _ -> None
      }
    _ -> None
  }
}

// -- Arc.self ----------------------------------------------------------------

/// Arc.self()
/// Returns a Pid object representing the current BEAM process.
fn self_(
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

/// Arc.log(...args)
/// Prints values to stdout, space-separated, with a newline.
/// Similar to console.log but available in spawned processes.
fn log(args: List(JsValue), state: State) -> #(State, Result(JsValue, JsValue)) {
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
      case heap.read_pid(state.heap, ref) {
        Some(pid) -> #(state, "Pid" <> ffi_pid_to_string(pid))
        None ->
          // Try to convert to string via toString
          case state.to_string(state, val) {
            Ok(#(s, state)) -> #(state, s)
            Error(#(_, state)) -> #(state, "[object Object]")
          }
      }
  }
}

// -- Arc.sleep ---------------------------------------------------------------

/// Arc.sleep(ms)
/// Suspends the current BEAM process for the given number of milliseconds.
/// Maps directly to Erlang's timer:sleep/1.
fn sleep(
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
        elements: elements.new(),
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
fn pid_to_string(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read_pid(state.heap, ref) {
        Some(pid) -> #(state, Ok(JsString("Pid" <> ffi_pid_to_string(pid))))
        None -> state.type_error(state, "Dead Pid")
      }
    _ -> state.type_error(state, "Invalid Pid object")
  }
}

// -- Message serialization ---------------------------------------------------

/// Serialize a JsValue into a PortableMessage for cross-process transfer.
/// Only supports primitives, plain objects, arrays, and PIDs.
/// Returns Error(reason) for unsupported types (functions, promises, etc.).
fn serialize(heap: Heap, val: JsValue) -> Result(PortableMessage, String) {
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
      let val = elements.get_option(elements, i) |> option.unwrap(JsUndefined)
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
            elements: elements.new(),
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
