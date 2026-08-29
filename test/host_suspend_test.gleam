import arc/bytecode/key.{Named}
import arc/host.{AlreadySettled, Resumed, StaleTicket, State}
import arc/interp/safepoint
import arc/rt/async as rt_async
import arc/rt/gc as rt_gc
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type PromiseState, JInt, KHandle,
  PromiseFulfilled, PromisePending, PromiseRejected, StringKey, classify,
  mk_number, mk_string, mk_undefined,
}
import rt_helpers.{agent, get, global}

fn key() -> host.Key(Nil) {
  host.new_key()
}

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

fn suspended(rejecting: Bool) -> #(Agent, #(JsVal, host.Ticket)) {
  use s <- host.with_state(agent(), key())
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
  let assert PromisePending(_) = promise_state(st, promise)
  assert seen(st) == mk_string("nothing")
  let st = rt_gc.t_collect(st, [])
  assert rt_gc.t_is_live(st, handle(promise))
  let #(st, outcome) =
    host.with_state(st, key(), fn(s) {
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
    host.with_state(st, key(), fn(s) {
      host.resume(s, ticket, Error(mk_string("no")))
    })
  assert outcome == Resumed
  assert promise_state(st, promise) == PromiseRejected(mk_string("no"))
  assert seen(st) == mk_string("no")
}

pub fn thenable_outcome_is_assimilated_test() {
  let #(st, #(promise, ticket)) = suspended(False)
  let #(st, outcome) =
    host.with_state(st, key(), fn(s) {
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
    host.with_state(st, key(), fn(s) {
      let #(s, first) = host.resume(s, ticket, Ok(int(1)))
      let #(s, second) = host.resume(s, ticket, Ok(int(2)))
      #(s, #(first, second))
    })
  assert outcomes == #(Resumed, AlreadySettled)
  assert promise_state(st, promise) == PromiseFulfilled(int(1))
  assert seen(st) == int(1)
  let #(st, third) =
    host.with_state(st, key(), fn(s) { host.resume(s, ticket, Ok(int(3))) })
  assert third == AlreadySettled
  assert seen(st) == int(1)
}

fn bare_suspend() -> #(Agent, #(JsVal, host.Ticket)) {
  use s <- host.with_state(agent(), key())
  let #(s, promise, ticket) = host.suspend(s)
  #(s, #(promise, ticket))
}

pub fn resumed_promise_is_collectable_then_stale_test() {
  let #(st, #(promise, ticket)) = bare_suspend()
  let st = rt_gc.t_collect(st, [])
  assert rt_gc.t_is_live(st, handle(promise))
  let #(st, outcome) =
    host.with_state(st, key(), fn(s) { host.resume(s, ticket, Ok(int(1))) })
  assert outcome == Resumed
  let st = rt_gc.t_collect(st, [])
  assert !rt_gc.t_is_live(st, handle(promise))
  let #(_, outcome) =
    host.with_state(st, key(), fn(s) { host.resume(s, ticket, Ok(int(2))) })
  assert outcome == StaleTicket
}

pub fn held_promise_survives_resume_inside_a_turn_end_test() {
  let #(st, #(promise, ticket)) = bare_suspend()
  let st =
    safepoint.finish_turn(st, [promise], fn(st) {
      let #(st, outcome) =
        host.with_state(st, key(), fn(s) { host.resume(s, ticket, Ok(int(5))) })
      assert outcome == Resumed
      rt_gc.t_collect(st, [])
    })
  assert rt_gc.t_is_live(st, handle(promise))
  assert promise_state(st, promise) == PromiseFulfilled(int(5))
  let st = rt_gc.t_collect(st, [])
  assert !rt_gc.t_is_live(st, handle(promise))
}

pub fn holding_the_promise_does_not_revive_a_spent_ticket_test() {
  let #(st, #(promise, ticket)) = bare_suspend()
  let #(State(agent: st, ..), first) =
    host.resume(host.from_agent(st, key()), ticket, Ok(int(1)))
  let #(st, ids) = rt_gc.t_hold_roots(st, [promise])
  let #(State(agent: st, ..), second) =
    host.resume(host.from_agent(st, key()), ticket, Ok(int(2)))
  assert #(first, second) == #(Resumed, AlreadySettled)
  let st = rt_gc.t_release_roots(rt_async.drain(st), ids)
  assert promise_state(st, promise) == PromiseFulfilled(int(1))
}

pub fn foreign_ticket_is_stale_test() {
  let #(_, ticket) =
    host.with_state(agent(), key(), fn(s) {
      let #(s, _) = host.object(s, [])
      let #(s, _promise, ticket) = host.suspend(s)
      #(s, ticket)
    })
  let #(_, outcome) =
    host.with_state(agent(), key(), fn(s) { host.resume(s, ticket, Ok(int(1))) })
  assert outcome == StaleTicket
}
