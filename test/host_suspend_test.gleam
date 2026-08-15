//// `host.suspend` / `host.resume`: a host function hands JS a pending
//// promise, the embedder later queues its settlement as a microtask job, and
//// the one drain settles it and runs the reactions.

import arc/host.{AlreadySettled, Resumed, StaleTicket, State}
import arc/rt/async as rt_async
import arc/rt/gc as rt_gc
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type PromiseState, JInt, KHandle, Named,
  PromiseFulfilled, PromisePending, PromiseRejected, StringKey, classify,
  mk_number, mk_string, mk_undefined,
}
import rt_helpers.{agent, get, global}

fn int(i: Int) -> JsVal {
  mk_number(JInt(i))
}

fn handle(v: JsVal) -> Handle {
  let assert KHandle(h) = classify(v)
  h
}

fn promise_state(st: Agent, promise: JsVal) -> PromiseState {
  rt_async.promise_data(st, handle(promise)).1
}

/// `globalThis.seen = <first arg>`, as a host function value.
fn recorder(s: host.State(Nil)) -> #(host.State(Nil), JsVal) {
  host.function(s, "record", 1, fn(args, _, s) {
    let st = s.agent
    let #(_, st) =
      rt_obj.t_set_prop(
        st,
        global(st, "globalThis").0,
        StringKey(Named("seen")),
        host.first_arg(args),
      )
    #(State(..s, agent: st), Ok(mk_undefined()))
  })
}

fn seen(st: Agent) -> JsVal {
  get(st, global(st, "globalThis").0, "seen").0
}

/// Suspend inside `with_state`, attach the recorder to the promise's fulfil
/// (or, `rejecting`, its reject) side, and hand back the promise and ticket.
fn suspended(rejecting: Bool) -> #(Agent, #(JsVal, host.Ticket)) {
  use s <- host.with_state(agent())
  let s = host.define_global(s, "seen", mk_string("nothing"))
  let #(s, promise, ticket) = host.suspend(s)
  let #(s, on_settle) = recorder(s)
  let #(on_fulfilled, on_rejected) = case rejecting {
    False -> #(on_settle, mk_undefined())
    True -> #(mk_undefined(), on_settle)
  }
  let #(_, st) =
    rt_async.t_promise_then(s.agent, handle(promise), on_fulfilled, on_rejected)
  #(State(..s, agent: st), #(promise, ticket))
}

pub fn resume_settles_on_the_next_drain_test() {
  let #(st, #(promise, ticket)) = suspended(False)
  // Suspended: pending, nothing ran, and the promise survives a collection
  // even though only the embedder holds it.
  let assert PromisePending(_) = promise_state(st, promise)
  assert seen(st) == mk_string("nothing")
  let st = rt_gc.t_collect(st, [])
  assert rt_gc.t_is_live(st, handle(promise))
  // Resume queues; the drain at the end of `with_state` settles and reacts.
  let #(st, outcome) =
    host.with_state(st, fn(s) {
      let #(s, outcome) = host.resume(s, ticket, Ok(int(42)))
      let assert PromisePending(_) = promise_state(s.agent, promise)
      #(s, outcome)
    })
  assert outcome == Resumed
  assert promise_state(st, promise) == PromiseFulfilled(int(42))
  assert seen(st) == int(42)
}

pub fn error_outcome_rejects_test() {
  let #(st, #(promise, ticket)) = suspended(True)
  let #(st, outcome) =
    host.with_state(st, fn(s) { host.resume(s, ticket, Error(mk_string("no"))) })
  assert outcome == Resumed
  assert promise_state(st, promise) == PromiseRejected(mk_string("no"))
  assert seen(st) == mk_string("no")
}

pub fn thenable_outcome_is_assimilated_test() {
  let #(st, #(promise, ticket)) = suspended(False)
  let #(st, outcome) =
    host.with_state(st, fn(s) {
      let promise_ctor = global(s.agent, "Promise").0
      let #(inner, st) =
        rt_helpers.call_method(s.agent, promise_ctor, "resolve", [int(7)])
      host.resume(State(..s, agent: st), ticket, Ok(inner))
    })
  assert outcome == Resumed
  assert promise_state(st, promise) == PromiseFulfilled(int(7))
  assert seen(st) == int(7)
}

pub fn double_resume_is_a_no_op_test() {
  let #(st, #(promise, ticket)) = suspended(False)
  let #(st, outcomes) =
    host.with_state(st, fn(s) {
      let #(s, first) = host.resume(s, ticket, Ok(int(1)))
      // Still pending (the job has not run) yet already spent.
      let #(s, second) = host.resume(s, ticket, Ok(int(2)))
      #(s, #(first, second))
    })
  assert outcomes == #(Resumed, AlreadySettled)
  assert promise_state(st, promise) == PromiseFulfilled(int(1))
  assert seen(st) == int(1)
  // Once settled and drained: still a no-op.
  let #(st, third) =
    host.with_state(st, fn(s) { host.resume(s, ticket, Ok(int(3))) })
  assert third == AlreadySettled
  assert seen(st) == int(1)
}

pub fn resumed_promise_is_collectable_then_stale_test() {
  // Nothing but the ticket references this promise.
  let #(st, #(promise, ticket)) =
    host.with_state(agent(), fn(s) {
      let #(s, promise, ticket) = host.suspend(s)
      #(s, #(promise, ticket))
    })
  let st = rt_gc.t_collect(st, [])
  assert rt_gc.t_is_live(st, handle(promise))
  let #(st, outcome) =
    host.with_state(st, fn(s) { host.resume(s, ticket, Ok(int(1))) })
  assert outcome == Resumed
  // Settled and no longer a root: the next collection reclaims it, after
  // which the ticket names nothing on this agent.
  let st = rt_gc.t_collect(st, [])
  assert !rt_gc.t_is_live(st, handle(promise))
  let #(_, outcome) =
    host.with_state(st, fn(s) { host.resume(s, ticket, Ok(int(2))) })
  assert outcome == StaleTicket
}

pub fn foreign_ticket_is_stale_test() {
  let #(_, ticket) =
    host.with_state(agent(), fn(s) {
      // Push the promise past every id a fresh agent has in use.
      let #(s, _) = host.object(s, [])
      let #(s, _promise, ticket) = host.suspend(s)
      #(s, ticket)
    })
  let #(_, outcome) =
    host.with_state(agent(), fn(s) { host.resume(s, ticket, Ok(int(1))) })
  assert outcome == StaleTicket
}
