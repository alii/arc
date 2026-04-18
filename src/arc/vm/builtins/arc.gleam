import arc/vm/builtins/common
import arc/vm/builtins/promise as builtins_promise
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/coerce
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type ArcNativeFn, type JsValue, type MailboxEvent, type PortableMessage,
  type Ref, ArcLog, ArcNative, ArcPeek, ArcPidToString, ArcSelect, ArcSelf,
  ArcSetTimeout, ArcSleep, ArcSubject, ArcSubjectReceive, ArcSubjectReceiveAsync,
  ArcSubjectSend, ArcSubjectToString, DataProperty, JsBigInt, JsBool, JsNull,
  JsNumber, JsObject, JsString, JsSymbol, JsUndefined, JsUninitialized,
  ObjectSlot, OrdinaryObject, PidObject, PmArray, PmBigInt, PmBool, PmNull,
  PmNumber, PmObject, PmPid, PmString, PmSubject, PmSymbol, PmUndefined,
  PromiseFulfilled, PromisePending, PromiseRejected, SettlePromise,
  SubjectObject,
}
import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
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
      #("select", ArcNative(ArcSelect), 1),
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
    value.ArcSelect -> select(args, state)
    value.ArcSubjectSend -> subject_send(this, args, state)
    value.ArcSubjectReceive -> subject_receive(this, args, state)
    ArcSubjectReceiveAsync -> subject_receive_async(this, args, state)
    value.ArcSubjectToString -> subject_to_string(this, state)
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
    ffi_send_after(ms, ffi_self(), SettlePromise(data_ref, Ok(PmUndefined)))
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
          case serialize(state.heap, msg_arg) {
            Ok(portable) -> {
              ffi_send_subject(pid, tag, portable)
              #(state, Ok(msg_arg))
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

/// Arc.select(s => s.on(subject, handler).on(subject2, handler2).timeout(ms, fn))
/// Blocks until a message arrives on any of the specified subjects, then runs
/// the corresponding handler and returns its result.
fn select(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let callback = case args {
    [cb, ..] -> cb
    [] -> JsUndefined
  }

  // 1. Allocate accumulator array + timeout box on the heap
  let #(heap, acc_ref) =
    common.alloc_array(state.heap, [], state.builtins.array.prototype)
  let #(heap, timeout_ref) = heap.alloc(heap, value.BoxSlot(JsUndefined))

  // 2. Create builder object with .on() and .timeout() methods
  let #(heap, on_fn_ref) =
    common.alloc_host_fn(
      heap,
      state.builtins.function.prototype,
      fn(on_args, this, st) {
        // .on(subject, handler) — push subject and handler to accumulator
        let #(subj_arg, handler_arg) = case on_args {
          [s, h, ..] -> #(s, h)
          [s] -> #(s, JsUndefined)
          [] -> #(JsUndefined, JsUndefined)
        }
        // Append subject and handler as consecutive elements
        let h = st.heap
        case heap.read(h, acc_ref) {
          Some(ObjectSlot(kind: value.ArrayObject(length:), elements: elems, ..)) -> {
            let elems = elements.set(elems, length, subj_arg)
            let elems = elements.set(elems, length + 1, handler_arg)
            let h =
              heap.update(h, acc_ref, fn(slot) {
                case slot {
                  ObjectSlot(..) ->
                    ObjectSlot(
                      ..slot,
                      kind: value.ArrayObject(length: length + 2),
                      elements: elems,
                    )
                  _ -> slot
                }
              })
            #(State(..st, heap: h), Ok(this))
          }
          _ -> #(st, Ok(this))
        }
      },
      "on",
      2,
    )

  let #(heap, timeout_fn_ref) =
    common.alloc_host_fn(
      heap,
      state.builtins.function.prototype,
      fn(t_args, this, st) {
        // .timeout(ms, handler) — store in the timeout box
        let #(ms_arg, handler_arg) = case t_args {
          [m, h, ..] -> #(m, h)
          [m] -> #(m, JsUndefined)
          [] -> #(JsUndefined, JsUndefined)
        }
        // Store as a 2-element: [ms, handler] in the BoxSlot
        let h = st.heap
        let #(h, pair_ref) =
          common.alloc_array(
            h,
            [ms_arg, handler_arg],
            st.builtins.array.prototype,
          )
        let h =
          heap.update(h, timeout_ref, fn(_) {
            value.BoxSlot(JsObject(pair_ref))
          })
        #(State(..st, heap: h), Ok(this))
      },
      "timeout",
      2,
    )

  let #(heap, builder_ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: common.named_props([
          #("on", value.builtin_property(JsObject(on_fn_ref))),
          #("timeout", value.builtin_property(JsObject(timeout_fn_ref))),
        ]),
        elements: elements.new(),
        prototype: Some(state.builtins.object.prototype),
        symbol_properties: [],
        extensible: True,
      ),
    )

  let state = State(..state, heap:)

  // 3. Call the user's builder callback
  let call_result =
    state.call(state, callback, JsUndefined, [JsObject(builder_ref)])
  let state = case call_result {
    Ok(#(_result, st)) -> st
    Error(#(thrown, st)) -> {
      // If callback throws, propagate
      let _ = thrown
      st
    }
  }
  case call_result {
    Error(#(thrown, _)) -> #(state, Error(thrown))
    Ok(_) -> {
      // 4. Read accumulated entries from the array
      select_from_accumulated(state, acc_ref, timeout_ref)
    }
  }
}

/// Read the accumulated .on() entries and timeout, build ref maps, call FFI select.
fn select_from_accumulated(
  state: State,
  acc_ref: Ref,
  timeout_ref: Ref,
) -> #(State, Result(JsValue, JsValue)) {
  // Read the accumulator array: [subject1, handler1, subject2, handler2, ...]
  let entries = case heap.read(state.heap, acc_ref) {
    Some(ObjectSlot(kind: value.ArrayObject(length:), elements: elems, ..)) ->
      read_select_entries(state.heap, elems, 0, length, [])
    _ -> []
  }

  case entries {
    [] -> state.type_error(state, "Arc.select: no subjects registered")
    _ -> {
      // Build ref_map for FFI and handler_map for lookup
      let #(ref_map, handler_map) =
        list.fold(entries, #(dict.new(), dict.new()), fn(acc, entry) {
          let #(rm, hm) = acc
          let #(tag, handler) = entry
          #(dict.insert(rm, tag, True), dict.insert(hm, tag, handler))
        })

      // Read timeout
      let timeout_info = case heap.read(state.heap, timeout_ref) {
        Some(value.BoxSlot(JsObject(pair_ref))) ->
          case heap.read(state.heap, pair_ref) {
            Some(ObjectSlot(kind: value.ArrayObject(..), elements: elems, ..)) -> {
              let ms_val =
                elements.get_option(elems, 0) |> option.unwrap(JsUndefined)
              let handler_val =
                elements.get_option(elems, 1) |> option.unwrap(JsUndefined)
              case ms_val {
                JsNumber(value.Finite(n)) ->
                  Some(#(value.float_to_int(n), handler_val))
                _ -> None
              }
            }
            _ -> None
          }
        _ -> None
      }

      // Call FFI select
      case timeout_info {
        None -> {
          let #(matched_ref, pm) = ffi_select(ref_map)
          select_handle_match(state, matched_ref, pm, handler_map)
        }
        Some(#(ms, timeout_handler)) -> {
          case ffi_select_timeout(ref_map, ms) {
            Ok(#(matched_ref, pm)) ->
              select_handle_match(state, matched_ref, pm, handler_map)
            Error(Nil) -> {
              // Timeout — call the timeout handler
              case state.call(state, timeout_handler, JsUndefined, []) {
                Ok(#(result, state)) -> #(state, Ok(result))
                Error(#(thrown, state)) -> #(state, Error(thrown))
              }
            }
          }
        }
      }
    }
  }
}

/// Deserialize the matched message and call the handler.
fn select_handle_match(
  state: State,
  matched_ref: value.ErlangRef,
  pm: PortableMessage,
  handler_map: dict.Dict(value.ErlangRef, JsValue),
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, val) = deserialize(state.heap, state.builtins, pm)
  let state = State(..state, heap:)
  case dict.get(handler_map, matched_ref) {
    Ok(handler) ->
      case state.call(state, handler, JsUndefined, [val]) {
        Ok(#(result, state)) -> #(state, Ok(result))
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
    Error(Nil) ->
      // Shouldn't happen — the ref came from our map
      #(state, Ok(val))
  }
}

/// Read pairs of [subject, handler] from the accumulator array,
/// extracting ErlangRef tags from SubjectObjects.
fn read_select_entries(
  h: Heap,
  elems: value.JsElements,
  i: Int,
  length: Int,
  acc: List(#(value.ErlangRef, JsValue)),
) -> List(#(value.ErlangRef, JsValue)) {
  case i >= length {
    True -> list.reverse(acc)
    False -> {
      let subj_val = elements.get_option(elems, i) |> option.unwrap(JsUndefined)
      let handler_val =
        elements.get_option(elems, i + 1) |> option.unwrap(JsUndefined)
      case subj_val {
        JsObject(ref) ->
          case heap.read_subject(h, ref) {
            Some(#(_pid, tag)) ->
              read_select_entries(h, elems, i + 2, length, [
                #(tag, handler_val),
                ..acc
              ])
            None -> read_select_entries(h, elems, i + 2, length, acc)
          }
        _ -> read_select_entries(h, elems, i + 2, length, acc)
      }
    }
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
            serialize_symbol_props(heap, symbol_properties, seen, []),
          )
          Ok(PmObject(properties: props, symbol_properties: sym_props))
        }
        Some(ObjectSlot(kind: PidObject(pid:), ..)) -> Ok(PmPid(pid))
        Some(ObjectSlot(kind: SubjectObject(pid:, tag:), ..)) ->
          Ok(PmSubject(pid:, tag:))
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
  entries: List(#(value.PropertyKey, value.Property)),
  seen: set.Set(Int),
  acc: List(#(value.PropertyKey, PortableMessage)),
) -> Result(List(#(value.PropertyKey, PortableMessage)), String) {
  case entries {
    [] -> Ok(list.reverse(acc))
    [#(key, DataProperty(value: val, enumerable: True, ..)), ..rest] -> {
      use pm <- result.try(serialize_inner(heap, val, seen))
      serialize_object_props(heap, rest, seen, [#(key, pm), ..acc])
    }
    [#(key, DataProperty(enumerable: False, ..)), ..] ->
      Error(
        "cannot send object with non-enumerable property \""
        <> value.key_to_string(key)
        <> "\" between processes",
      )
    [#(key, value.AccessorProperty(..)), ..] ->
      Error(
        "cannot send object with accessor property \""
        <> value.key_to_string(key)
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
    PmSubject(pid:, tag:) ->
      alloc_subject_object(
        heap,
        builtins.object.prototype,
        builtins.function.prototype,
        pid,
        tag,
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
            symbol_properties: sym_props,
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
  entries: List(#(value.PropertyKey, PortableMessage)),
) -> #(Heap, List(#(value.PropertyKey, value.Property))) {
  let #(heap, rev) =
    list.fold(entries, #(heap, []), fn(acc, entry) {
      let #(heap, props) = acc
      let #(key, pm) = entry
      let #(heap, val) = deserialize(heap, builtins, pm)
      #(heap, [#(key, value.data_property(val)), ..props])
    })
  #(heap, list.reverse(rev))
}
