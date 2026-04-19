import arc/vm/builtins/common
import arc/vm/builtins/dom_exception
import arc/vm/builtins/promise as builtins_promise
import arc/vm/builtins/regexp
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/coerce
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type ArcNativeFn, type JsValue, type MailboxEvent, type PortableMessage,
  type Ref, AccessorProperty, ArcLog, ArcNative, ArcPeek, ArcPidToString,
  ArcSelect, ArcSelectorOn, ArcSelectorReceive, ArcSelf, ArcSetTimeout, ArcSleep,
  ArcSubject, ArcSubjectReceive, ArcSubjectReceiveAsync, ArcSubjectSend,
  ArcSubjectToString, DataProperty, JsBigInt, JsBool, JsNull, JsNumber, JsObject,
  JsString, JsSymbol, JsUndefined, JsUninitialized, ObjectSlot, OrdinaryObject,
  PidObject, PortableMessage, PrArray, PrBooleanObject, PrDate, PrMap,
  PrNumberObject, PrObject, PrPid, PrRegExp, PrSet, PrStringObject, PrSubject,
  PromiseFulfilled, PromisePending, PromiseRejected, PvBigInt, PvBool, PvNull,
  PvNumber, PvRef, PvString, PvSymbol, PvUndefined, SelectorObject,
  SettlePromise, SubjectObject, WellKnownSymbol,
}
import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// -- FFI declarations --------------------------------------------------------

@external(erlang, "erlang", "self")
fn ffi_self() -> value.ErlangPid

/// Returns the pid in the format `<x.x.x>
@external(erlang, "arc_vm_ffi", "pid_to_string")
pub fn ffi_pid_to_string(pid: value.ErlangPid) -> String

@external(erlang, "arc_vm_ffi", "sleep")
fn ffi_sleep(ms: Int) -> Nil

@external(erlang, "arc_vm_ffi", "send_after")
fn ffi_send_after(
  ms: Int,
  pid: value.ErlangPid,
  msg: MailboxEvent,
) -> value.ErlangTimerRef

@external(erlang, "arc_vm_ffi", "cancel_timer")
fn ffi_cancel_timer(tref: value.ErlangTimerRef) -> Bool

// -- Subject FFI declarations ------------------------------------------------

@external(erlang, "erlang", "make_ref")
fn ffi_make_ref() -> value.ErlangRef

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

@external(erlang, "arc_vm_ffi", "select_message")
fn ffi_select(
  ref_map: dict.Dict(value.ErlangRef, Bool),
) -> #(value.ErlangRef, PortableMessage)

@external(erlang, "arc_vm_ffi", "select_message_timeout")
fn ffi_select_timeout(
  ref_map: dict.Dict(value.ErlangRef, Bool),
  timeout: Int,
) -> Result(#(value.ErlangRef, PortableMessage), Nil)

// -- Init --------------------------------------------------------------------

pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("peek", ArcNative(ArcPeek), 1),
      #("spawn", value.VmNative(value.ArcSpawn), 1),
      #("setTimeout", ArcNative(ArcSetTimeout), 2),
      #("clearTimeout", ArcNative(value.ArcClearTimeout), 1),
      #("self", ArcNative(ArcSelf), 0),
      #("log", ArcNative(ArcLog), 1),
      #("sleep", ArcNative(ArcSleep), 1),
      #("subject", ArcNative(ArcSubject), 0),
      #("select", ArcNative(ArcSelect), 0),
    ])

  common.init_namespace(h, object_proto, "Arc", methods)
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
    value.ArcSetTimeout -> set_timeout(args, state)
    value.ArcClearTimeout -> clear_timeout(args, state)
    value.ArcSelf -> self_(args, state)
    value.ArcLog -> log(args, state)
    value.ArcSleep -> sleep(args, state)
    value.ArcPidToString -> pid_to_string(this, args, state)
    value.ArcSubject -> subject(state)
    value.ArcSelect -> select(state)
    value.ArcSelectorOn -> selector_on(this, args, state)
    value.ArcSelectorReceive -> selector_receive(this, args, state)
    value.ArcSubjectSend -> subject_send(this, args, state)
    value.ArcSubjectReceive -> subject_receive(this, args, state)
    ArcSubjectReceiveAsync -> subject_receive_async(this, args, state)
    value.ArcSubjectToString -> subject_to_string(this, state)
    value.ArcStructuredClone -> structured_clone(args, state)
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
    None -> state.type_error(state, "Arc.peek: argument is not a Promise")
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
    False ->
      state.type_error(
        state,
        "Arc.setTimeout() requires the event loop (--event-loop flag)",
      )
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
    ffi_send_after(
      ms,
      ffi_self(),
      SettlePromise(
        data_ref,
        Ok(PortableMessage(root: PvUndefined, records: dict.new())),
      ),
    )
  let #(heap, timer_obj) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: value.TimerObject(timer_ref:, data_ref:),
        properties: dict.new(),
        elements: elements.new(),
        prototype: Some(state.builtins.object.prototype),
        symbol_properties: [],
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
      case log_stringify_object(state.heap, ref) {
        Some(s) -> #(state, s)
        None ->
          // Try to convert to string via toString
          case coerce.js_to_string(state, val) {
            Ok(#(s, state)) -> #(state, s)
            Error(#(_, state)) -> #(state, "[object Object]")
          }
      }
  }
}

/// Check if a heap object has a special log representation (Pid, Subject, etc.).
fn log_stringify_object(h: Heap, ref: Ref) -> Option(String) {
  case heap.read(h, ref) {
    Some(ObjectSlot(kind: PidObject(pid:), ..)) ->
      Some("Pid" <> ffi_pid_to_string(pid))
    Some(ObjectSlot(kind: SubjectObject(pid:, ..), ..)) ->
      Some("Subject" <> ffi_pid_to_string(pid))
    _ -> None
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
        properties: common.named_props([
          #("toString", value.builtin_property(JsObject(to_string_ref))),
        ]),
        elements: elements.new(),
        prototype: Some(object_proto),
        symbol_properties: [common.to_string_tag("Pid")],
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

// -- Subject -----------------------------------------------------------------

/// Arc.subject()
/// Creates a new subject bound to the current process. Returns a Subject object
/// with send(msg) and receive(timeout?) methods. The subject has a unique tag
/// (via erlang:make_ref) so selective receive on it is O(1) for the common case.
fn subject(state: State) -> #(State, Result(JsValue, JsValue)) {
  let pid = ffi_self()
  let tag = ffi_make_ref()
  let #(heap, subject_val) =
    alloc_subject_object(
      state.heap,
      state.builtins.object.prototype,
      state.builtins.function.prototype,
      pid,
      tag,
    )
  #(State(..state, heap:), Ok(subject_val))
}

/// Allocate a SubjectObject on the heap wrapping an Erlang PID + ref tag.
pub fn alloc_subject_object(
  heap: Heap,
  object_proto: Ref,
  function_proto: Ref,
  pid: value.ErlangPid,
  tag: value.ErlangRef,
) -> #(Heap, JsValue) {
  let #(heap, send_ref) =
    common.alloc_native_fn(
      heap,
      function_proto,
      ArcNative(ArcSubjectSend),
      "send",
      1,
    )
  let #(heap, receive_ref) =
    common.alloc_native_fn(
      heap,
      function_proto,
      ArcNative(ArcSubjectReceive),
      "receive",
      0,
    )
  let #(heap, receive_async_ref) =
    common.alloc_native_fn(
      heap,
      function_proto,
      ArcNative(ArcSubjectReceiveAsync),
      "receiveAsync",
      0,
    )
  let #(heap, to_string_ref) =
    common.alloc_native_fn(
      heap,
      function_proto,
      ArcNative(ArcSubjectToString),
      "toString",
      0,
    )
  let #(heap, ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        kind: SubjectObject(pid:, tag:),
        properties: common.named_props([
          #("send", value.builtin_property(JsObject(send_ref))),
          #("receive", value.builtin_property(JsObject(receive_ref))),
          #("receiveAsync", value.builtin_property(JsObject(receive_async_ref))),
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
  this: JsValue,
  args: List(JsValue),
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
  this: JsValue,
  args: List(JsValue),
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
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read_subject(state.heap, ref) {
        Some(#(pid, _tag)) -> #(
          state,
          Ok(JsString("Subject" <> ffi_pid_to_string(pid))),
        )
        None ->
          state.type_error(state, "Subject.toString: this is not a Subject")
      }
    _ -> state.type_error(state, "Subject.toString: this is not a Subject")
  }
}

/// subject.receiveAsync(timeout?) — returns a Promise that resolves with the
/// next message sent to this subject. Non-blocking: the async function suspends
/// at `await` while other async functions keep running via the event loop.
fn subject_receive_async(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case state.event_loop {
    False -> {
      let #(heap, err) =
        common.make_type_error(
          state.heap,
          state.builtins,
          "subject.receiveAsync() requires the event loop (--event-loop flag)",
        )
      #(State(..state, heap:), Error(err))
    }
    True ->
      case this {
        JsObject(ref) ->
          case heap.read_subject(state.heap, ref) {
            Some(#(_pid, tag)) -> {
              let #(h, obj_ref, data_ref) =
                builtins_promise.create_promise(
                  state.heap,
                  state.builtins.promise.prototype,
                )
              // Set up optional timeout
              case args {
                [JsNumber(value.Finite(n)), ..] -> {
                  let ms = value.float_to_int(n)
                  case ms >= 0 {
                    True -> {
                      let _ =
                        ffi_send_after(
                          ms,
                          ffi_self(),
                          value.ReceiverTimeout(data_ref),
                        )
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
                  heap: h,
                  pending_receivers: list.append(state.pending_receivers, [
                    #(data_ref, tag),
                  ]),
                  outstanding: state.outstanding + 1,
                ),
                Ok(JsObject(obj_ref)),
              )
            }
            None ->
              state.type_error(
                state,
                "Subject.receiveAsync: this is not a Subject",
              )
          }
        _ ->
          state.type_error(state, "Subject.receiveAsync: this is not a Subject")
      }
  }
}

// -- Arc.select --------------------------------------------------------------

/// Arc.select() — returns an empty Selector. Chain `.on(subject, mapper?)`
/// to register subjects, then call `.receive()` / `.receive(timeout)` to
/// block until a message arrives on any of them.
fn select(state: State) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, selector) = alloc_selector_object(state, [])
  #(State(..state, heap:), Ok(selector))
}

fn alloc_selector_object(
  state: State,
  entries: List(#(value.ErlangRef, JsValue)),
) -> #(Heap, JsValue) {
  let function_proto = state.builtins.function.prototype
  let #(heap, on_ref) =
    common.alloc_native_fn(
      state.heap,
      function_proto,
      ArcNative(ArcSelectorOn),
      "on",
      1,
    )
  let #(heap, receive_ref) =
    common.alloc_native_fn(
      heap,
      function_proto,
      ArcNative(ArcSelectorReceive),
      "receive",
      0,
    )
  let #(heap, ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        kind: SelectorObject(entries:),
        properties: common.named_props([
          #("on", value.builtin_property(JsObject(on_ref))),
          #("receive", value.builtin_property(JsObject(receive_ref))),
        ]),
        elements: elements.new(),
        prototype: Some(state.builtins.object.prototype),
        symbol_properties: [common.to_string_tag("Selector")],
        extensible: True,
      ),
    )
  #(heap, JsObject(ref))
}

/// selector.on(subject, mapper?) — return a NEW selector with `subject`
/// registered. `mapper` defaults to identity (the raw message is returned).
fn selector_on(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let entries = case this {
    JsObject(ref) -> heap.read_selector(state.heap, ref)
    _ -> None
  }
  case entries {
    None -> state.type_error(state, "Selector.on: this is not a Selector")
    Some(entries) -> {
      let #(subj_arg, mapper_arg) = case args {
        [s, m, ..] -> #(s, m)
        [s] -> #(s, JsUndefined)
        [] -> #(JsUndefined, JsUndefined)
      }
      let tag = case subj_arg {
        JsObject(ref) ->
          heap.read_subject(state.heap, ref)
          |> option.map(fn(p) { p.1 })
        _ -> None
      }
      case tag {
        None ->
          state.type_error(state, "Selector.on: argument is not a Subject")
        Some(tag) -> {
          let #(heap, selector) =
            alloc_selector_object(state, [#(tag, mapper_arg), ..entries])
          #(State(..state, heap:), Ok(selector))
        }
      }
    }
  }
}

/// selector.receive(timeout?) — block until a message arrives on any
/// registered subject, run its mapper (or identity), and return the result.
/// With a timeout, returns `undefined` if it elapses.
fn selector_receive(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let entries = case this {
    JsObject(ref) -> heap.read_selector(state.heap, ref)
    _ -> None
  }
  case entries {
    None -> state.type_error(state, "Selector.receive: this is not a Selector")
    Some([]) ->
      state.type_error(state, "Selector.receive: no subjects registered")
    Some(entries) -> {
      let #(ref_map, handler_map) =
        list.fold(entries, #(dict.new(), dict.new()), fn(acc, entry) {
          let #(rm, hm) = acc
          let #(tag, handler) = entry
          #(dict.insert(rm, tag, True), dict.insert(hm, tag, handler))
        })
      case args {
        [JsNumber(value.Finite(n)), ..] -> {
          let ms = value.float_to_int(n)
          case ms >= 0 {
            False -> #(state, Ok(JsUndefined))
            True ->
              case ffi_select_timeout(ref_map, ms) {
                Ok(#(matched_ref, pm)) ->
                  select_handle_match(state, matched_ref, pm, handler_map)
                Error(Nil) -> #(state, Ok(JsUndefined))
              }
          }
        }
        _ -> {
          let #(matched_ref, pm) = ffi_select(ref_map)
          select_handle_match(state, matched_ref, pm, handler_map)
        }
      }
    }
  }
}

/// Deserialize the matched message and call its mapper if one was registered.
fn select_handle_match(
  state: State,
  matched_ref: value.ErlangRef,
  pm: PortableMessage,
  handler_map: dict.Dict(value.ErlangRef, JsValue),
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, val) = deserialize(state.heap, state.builtins, pm)
  let state = State(..state, heap:)
  case dict.get(handler_map, matched_ref) |> result.unwrap(JsUndefined) {
    JsUndefined -> #(state, Ok(val))
    mapper ->
      case state.call(state, mapper, JsUndefined, [val]) {
        Ok(#(result, state)) -> #(state, Ok(result))
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
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
        alloc_pid_object(
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
fn structured_clone(
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
