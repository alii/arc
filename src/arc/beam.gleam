//// BEAM/OTP primitives as opt-in host functions, plus the Erlang-mailbox
//// macrotask loop that drives them.
////
//// Arc the language has no built-in process model and no macrotask loop —
//// core only knows about Promises and the microtask queue. This module is
//// one embedder: it exposes Erlang processes/subjects/timers as `HostFn`s,
//// and `run` is the loop that blocks on the BEAM mailbox to settle the
//// promises those HostFns hand out via `host.suspend`.
////
////     // everything, under `Arc.*`, with the BEAM loop driving eval
////     let eng = engine.new() |> beam.install("Arc")
////     let assert Ok(#(comp, eng)) = engine.eval_with(eng, src, beam.run)
////
////     // or à la carte
////     let eng =
////       engine.new()
////       |> engine.define_namespace("proc", [
////         #("spawn", 1, beam.spawn),
////         #("self", 0, beam.self),
////       ])

import arc/engine.{type Engine}
import arc/host
import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/console
import arc/vm/builtins/process_objects
import arc/vm/builtins/promise as builtins_promise
import arc/vm/builtins/structured_clone
import arc/vm/exec/event_loop
import arc/vm/exec/interpreter
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/tuple_array
import arc/vm/state.{type Heap, type HostFn, type State, State}
import arc/vm/value.{
  type JsValue, type PortableMessage, type Ref, JsNumber, JsObject, JsString,
  JsUndefined, Named, ObjectSlot, PortableMessage, PromiseFulfilled,
  PromisePending, PromiseRejected, PvUndefined, SelectorObject,
}
import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

// -- Mailbox event protocol --------------------------------------------------

/// Envelope for every message this loop's selective receive accepts. The
/// Erlang side (`arc_vm_ffi.erl`) constructs these as bare tagged tuples,
/// so the variant names here must stay snake_case-compatible.
pub type MailboxEvent {
  /// `setTimeout` / external worker completed: settle the suspended promise.
  SettlePromise(
    data_ref: Ref,
    outcome: Result(PortableMessage, PortableMessage),
  )
  /// `receiveAsync(ms)` deadline fired. If the receiver is still pending,
  /// resolve it with `undefined`; otherwise the message already arrived.
  ReceiverTimeout(data_ref: Ref)
  /// A subject message matched by selective receive while a `receiveAsync`
  /// is pending on that tag.
  SubjectMessage(tag: value.ErlangRef, payload: PortableMessage)
}

// -- FFI ---------------------------------------------------------------------

@external(erlang, "erlang", "spawn")
fn erlang_spawn(fun: fn() -> Nil) -> value.ErlangPid

@external(erlang, "erlang", "self")
fn ffi_self() -> value.ErlangPid

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

@external(erlang, "erlang", "make_ref")
fn ffi_make_ref() -> value.ErlangRef

@external(erlang, "arc_vm_ffi", "select_message")
fn ffi_select(
  ref_map: dict.Dict(value.ErlangRef, Bool),
) -> #(value.ErlangRef, PortableMessage)

@external(erlang, "arc_vm_ffi", "select_message_timeout")
fn ffi_select_timeout(
  ref_map: dict.Dict(value.ErlangRef, Bool),
  timeout: Int,
) -> Result(#(value.ErlangRef, PortableMessage), Nil)

@external(erlang, "arc_vm_ffi", "receive_settle_only")
fn ffi_receive_settle_only() -> MailboxEvent

@external(erlang, "arc_vm_ffi", "receive_settle_or_subject")
fn ffi_receive_settle_or_subject(
  ref_map: dict.Dict(value.ErlangRef, Bool),
) -> MailboxEvent

@external(erlang, "arc_beam_ffi", "receivers_get")
fn receivers_get() -> List(#(Ref, value.ErlangRef))

@external(erlang, "arc_beam_ffi", "receivers_put")
fn receivers_put(l: List(#(Ref, value.ErlangRef))) -> Nil

// -- Macrotask loop ----------------------------------------------------------

/// The BEAM-mailbox macrotask loop. Drain microtasks; if any `host.suspend`
/// promises are still outstanding, block on the Erlang mailbox for the next
/// `MailboxEvent`, `host.resume` the matching promise, and repeat.
///
/// Pass to `engine.eval_with` / `entry.run_with` as the `finish` driver.
pub fn run(s: State) -> State {
  let s = event_loop.drain_jobs(s)
  case state.outstanding(s) {
    0 -> s
    _ -> {
      let receivers = receivers_get()
      let event = case receivers {
        [] -> ffi_receive_settle_only()
        _ -> {
          let ref_map =
            list.fold(receivers, dict.new(), fn(acc, e) {
              dict.insert(acc, e.1, True)
            })
          ffi_receive_settle_or_subject(ref_map)
        }
      }
      run(handle_event(s, event, receivers))
    }
  }
}

fn handle_event(
  state: State,
  event: MailboxEvent,
  receivers: List(#(Ref, value.ErlangRef)),
) -> State {
  case event {
    SettlePromise(data_ref:, outcome: Ok(pm)) -> {
      let #(heap, val) =
        structured_clone.deserialize(state.heap, state.builtins, pm)
      host.resume(State(..state, heap:), data_ref, Ok(val))
    }
    SettlePromise(data_ref:, outcome: Error(pm)) -> {
      let #(heap, reason) =
        structured_clone.deserialize(state.heap, state.builtins, pm)
      host.resume(State(..state, heap:), data_ref, Error(reason))
    }
    ReceiverTimeout(data_ref:) ->
      case list.any(receivers, fn(e) { e.0 == data_ref }) {
        // Message already arrived and retired the receiver — timeout is stale.
        False -> state
        True -> {
          receivers_put(list.filter(receivers, fn(e) { e.0 != data_ref }))
          host.resume(state, data_ref, Ok(JsUndefined))
        }
      }
    SubjectMessage(tag:, payload: pm) ->
      case list.find(receivers, fn(e) { e.1 == tag }) {
        Error(Nil) -> state
        Ok(#(data_ref, _)) -> {
          receivers_put(list.filter(receivers, fn(e) { e.0 != data_ref }))
          let #(heap, val) =
            structured_clone.deserialize(state.heap, state.builtins, pm)
          host.resume(State(..state, heap:), data_ref, Ok(val))
        }
      }
  }
}

// -- install -----------------------------------------------------------------

/// Install every BEAM primitive under one namespace object. Passing `"Arc"`
/// reproduces the legacy baked-in `Arc` global.
pub fn install(eng: Engine, name: String) -> Engine {
  engine.define_namespace(eng, name, namespace())
}

/// Heap-level equivalent of `install` for callers that bootstrap with
/// `builtins.init`/`builtins.globals` directly instead of `engine.new()`
/// (the REPL, the test harness).
pub fn install_globals(
  h: Heap,
  b: Builtins,
  global: Ref,
  name: String,
) -> Heap {
  let #(h, methods) =
    list.fold(namespace(), #(h, []), fn(acc, spec) {
      let #(h, props) = acc
      let #(method_name, arity, impl) = spec
      let #(h, fn_ref) =
        common.alloc_host_fn(h, b.function.prototype, impl, method_name, arity)
      #(h, [#(method_name, value.builtin_property(JsObject(fn_ref))), ..props])
    })
  let #(h, ns_ref) = common.init_namespace(h, b.object.prototype, name, methods)
  use slot <- heap.update(h, global)
  case slot {
    ObjectSlot(properties:, ..) ->
      ObjectSlot(
        ..slot,
        properties: dict.insert(
          properties,
          Named(name),
          value.builtin_property(JsObject(ns_ref)),
        ),
      )
    other -> other
  }
}

/// Method specs for `engine.define_namespace`. Exposed so embedders can
/// concat their own functions onto the same namespace.
pub fn namespace() -> List(#(String, Int, HostFn)) {
  [
    #("spawn", 1, spawn),
    #("self", 0, self),
    #("sleep", 1, sleep),
    #("subject", 0, subject),
    #("select", 0, select),
    #("peek", 1, peek),
    #("setTimeout", 2, set_timeout),
    #("clearTimeout", 1, clear_timeout),
    #("log", 1, log),
  ]
}

// -- spawn -------------------------------------------------------------------

/// `spawn(fn)` — fork a new BEAM process running the JS closure `fn` against
/// a snapshot of the current heap. Returns a `Pid` object.
///
/// The child gets its own VM state and runs to completion (microtasks +
/// this module's mailbox loop) independently. Closed-over values are visible
/// because the heap is copied; mutations after the fork are not.
///
/// Only JS closures are accepted — to spawn a native function, wrap it:
/// `spawn(() => nativeFn())`.
pub fn spawn(
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let fn_arg = case args {
    [a, ..] -> a
    [] -> JsUndefined
  }
  case read_closure(state, fn_arg) {
    None -> state.type_error(state, "spawn: argument is not a function")
    Some(#(template, env_ref)) -> {
      let #(heap, pid_val) =
        process_objects.alloc_pid_object(
          state.heap,
          state.builtins.object.prototype,
          state.builtins.function.prototype,
          erlang_spawn(make_spawner(state, template, env_ref)),
        )
      #(State(..state, heap:), Ok(pid_val))
    }
  }
}

fn read_closure(
  state: State,
  v: JsValue,
) -> option.Option(#(value.FuncTemplate, value.Ref)) {
  use ref <- option.then(case v {
    JsObject(r) -> Some(r)
    _ -> None
  })
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.FunctionObject(func_template:, env:), ..)) ->
      Some(#(func_template, env))
    _ -> None
  }
}

/// Build the thunk handed to `erlang:spawn/1`. Captures a (possibly GC'd)
/// heap snapshot plus everything `interpreter.new_state` needs to stand up a
/// fresh VM in the child, then drives it with this module's `run` loop.
fn make_spawner(
  state: State,
  template: value.FuncTemplate,
  env_ref: value.Ref,
) -> fn() -> Nil {
  // Only pay for a GC if the heap has grown well past the root set —
  // otherwise we'd just be copying builtins.
  let spawn_heap = case
    heap.size(state.heap) > set.size(heap.root_set(state.heap)) * 2
  {
    True -> heap.collect_with_roots(state.heap, set.from_list([env_ref.id]))
    False -> state.heap
  }
  let builtins = state.builtins
  let global_object = state.global_object
  let lexical_globals = state.lexical_globals
  let const_lexical_globals = state.const_lexical_globals
  let symbol_descriptions = state.symbol_descriptions
  let symbol_registry = state.symbol_registry
  fn() {
    let env_values = heap.read_env(spawn_heap, env_ref) |> option.unwrap([])
    let env_count = list.length(env_values)
    let remaining = template.local_count - env_count - template.arity
    let locals =
      list.flatten([
        env_values,
        list.repeat(JsUndefined, template.arity),
        list.repeat(JsUndefined, remaining),
      ])
      |> tuple_array.from_list
    let child =
      interpreter.new_state(
        template,
        locals,
        spawn_heap,
        builtins,
        global_object,
        lexical_globals,
        const_lexical_globals,
        symbol_descriptions,
        symbol_registry,
      )
    case interpreter.execute_inner(child) {
      Ok(#(_, final)) -> {
        let _ = run(final)
        Nil
      }
      Error(_vm_err) -> Nil
    }
  }
}

// -- self --------------------------------------------------------------------

/// `self()` — Pid of the current BEAM process.
pub fn self(
  _args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let pid = ffi_self()
  let #(heap, pid_val) =
    process_objects.alloc_pid_object(
      state.heap,
      state.builtins.object.prototype,
      state.builtins.function.prototype,
      pid,
    )
  #(State(..state, heap:), Ok(pid_val))
}

// -- sleep -------------------------------------------------------------------

/// `sleep(ms)` — block the current process via `timer:sleep/1`.
pub fn sleep(
  args: List(JsValue),
  _this: JsValue,
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

// -- subject -----------------------------------------------------------------

/// `subject()` — new Subject bound to this process (Erlang ref-tagged
/// mailbox channel with `.send` / `.receive` / `.receiveAsync`). The subject
/// has a unique tag (via `erlang:make_ref`) so selective receive on it is
/// O(1) for the common case.
pub fn subject(
  _args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let pid = ffi_self()
  let tag = ffi_make_ref()
  let #(heap, subject_val) =
    structured_clone.alloc_subject_object(
      state.heap,
      state.builtins.object.prototype,
      state.builtins.function.prototype,
      pid,
      tag,
    )
  // `receiveAsync` is added here rather than in `alloc_subject_object`
  // because it depends on this module's macrotask loop. Subjects that
  // arrive via deserialize (from another process) don't get it — you can
  // only async-receive on a subject your own process owns.
  let assert JsObject(subject_ref) = subject_val
  let #(heap, ra_ref) =
    common.alloc_host_fn(
      heap,
      state.builtins.function.prototype,
      subject_receive_async,
      "receiveAsync",
      0,
    )
  let heap =
    heap.update(heap, subject_ref, fn(slot) {
      case slot {
        ObjectSlot(properties:, ..) ->
          ObjectSlot(
            ..slot,
            properties: dict.insert(
              properties,
              Named("receiveAsync"),
              value.builtin_property(JsObject(ra_ref)),
            ),
          )
        other -> other
      }
    })
  #(State(..state, heap:), Ok(subject_val))
}

/// `subject.receiveAsync(timeout?)` — Promise that resolves with the next
/// message on this subject. Non-blocking: `host.suspend` hands out the
/// promise, the (settle ref, subject tag) pair is parked in the process
/// dictionary, and `run` resolves it when the message (or `ReceiverTimeout`)
/// lands in the mailbox.
fn subject_receive_async(
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let tag = case this {
    JsObject(ref) ->
      heap.read_subject(state.heap, ref) |> option.map(fn(p) { p.1 })
    _ -> None
  }
  case tag {
    None ->
      state.type_error(state, "Subject.receiveAsync: this is not a Subject")
    Some(tag) -> {
      let #(state, promise, data_ref) = host.suspend(state)
      receivers_put(list.append(receivers_get(), [#(data_ref, tag)]))
      case args {
        [JsNumber(value.Finite(n)), ..] -> {
          let ms = value.float_to_int(n)
          case ms >= 0 {
            True -> {
              let _ = ffi_send_after(ms, ffi_self(), ReceiverTimeout(data_ref))
              Nil
            }
            False -> Nil
          }
        }
        _ -> Nil
      }
      #(state, Ok(promise))
    }
  }
}

// -- select ------------------------------------------------------------------

/// `select()` — empty Selector; chain `.on(subject, mapper?)` then
/// `.receive(timeout?)` to block until a message arrives on any of them.
pub fn select(
  _args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, selector) = alloc_selector_object(state, [])
  #(State(..state, heap:), Ok(selector))
}

fn alloc_selector_object(
  state: State,
  entries: List(#(value.ErlangRef, JsValue)),
) -> #(Heap, JsValue) {
  let function_proto = state.builtins.function.prototype
  let #(heap, on_ref) =
    common.alloc_host_fn(state.heap, function_proto, selector_on, "on", 1)
  let #(heap, receive_ref) =
    common.alloc_host_fn(heap, function_proto, selector_receive, "receive", 0)
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
  args: List(JsValue),
  this: JsValue,
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
  args: List(JsValue),
  this: JsValue,
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
  let #(heap, val) =
    structured_clone.deserialize(state.heap, state.builtins, pm)
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

// -- peek --------------------------------------------------------------------

/// `peek(promise)` — synchronous promise introspection:
/// `{type: 'pending' | 'resolved' | 'rejected', value?, reason?}`.
pub fn peek(
  args: List(JsValue),
  _this: JsValue,
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
    None -> state.type_error(state, "peek: argument is not a Promise")
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

// -- setTimeout --------------------------------------------------------------

/// `setTimeout(fn, ms)` — `host.suspend` a promise, attach `fn` as its
/// fulfil handler, and have `erlang:send_after/3` post a `SettlePromise`
/// back to this process after `ms`. The `run` loop turns that into
/// `host.resume`, which schedules the reaction job that calls `fn`.
pub fn set_timeout(
  args: List(JsValue),
  _this: JsValue,
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
  let #(state, _promise, data_ref) = host.suspend(state)
  let state =
    builtins_promise.perform_promise_then(
      state,
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
  #(State(..state, heap:), Ok(JsObject(timer_obj)))
}

// -- clearTimeout ------------------------------------------------------------

/// `clearTimeout(timer)` — cancel a pending timer from `setTimeout`. If the
/// timer hasn't fired yet, the callback will not be invoked and `outstanding`
/// is decremented. If it already fired (or `timer` isn't a timer), this is a
/// no-op.
pub fn clear_timeout(
  args: List(JsValue),
  _this: JsValue,
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

// -- log ---------------------------------------------------------------------

/// `log(...args)` — print to stdout. Kept here for spawned processes that
/// don't have `console`; prefer the built-in `console.log` otherwise.
pub fn log(
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  console.print(args, state, io.println)
}
