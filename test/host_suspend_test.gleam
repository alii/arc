import arc/bytecode/key.{Named}
import arc/host.{AlreadySettled, Context, Resumed, StaleTicket}
import arc/interp/safepoint
import arc/rt/async as rt_async
import arc/rt/gc as rt_gc
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type PromiseState, KHandle,
  PromiseFulfilled, PromisePending, PromiseRejected, StringKey, classify, mk_int,
  mk_string, mk_undefined,
}
import rt_helpers.{agent, get, global}

fn key() -> host.Key(Nil) {
  host.new_key()
}

fn handle(v: JsVal) -> Handle {
  let assert KHandle(h) = classify(v)
  h
}

fn promise_state(st: Agent, promise: JsVal) -> PromiseState {
  rt_async.promise_data(st, handle(promise)).1
}

fn recorder(ctx: host.Context(Nil)) -> #(host.Context(Nil), JsVal) {
  host.function(ctx, "record", 1, fn(args, _, ctx) {
    let st = ctx.agent
    let #(_, st) =
      rt_obj.t_set_prop(
        st,
        global(st, "globalThis").0,
        StringKey(Named("seen")),
        host.first_arg(args),
      )
    #(Context(..ctx, agent: st), Ok(mk_undefined()))
  })
}

fn seen(st: Agent) -> JsVal {
  get(st, global(st, "globalThis").0, "seen").0
}

fn suspended(rejecting rejecting: Bool) -> #(Agent, #(JsVal, host.Ticket)) {
  use ctx <- host.with_context(agent(), key())
  let ctx = host.define_global(ctx, "seen", mk_string("nothing"))
  let #(ctx, promise, ticket) = host.suspend(ctx)
  let #(ctx, on_settle) = recorder(ctx)
  let #(on_fulfilled, on_rejected) = case rejecting {
    False -> #(on_settle, mk_undefined())
    True -> #(mk_undefined(), on_settle)
  }
  let #(_, st) =
    rt_async.t_promise_then(
      ctx.agent,
      handle(promise),
      on_fulfilled,
      on_rejected,
    )
  #(Context(..ctx, agent: st), #(promise, ticket))
}

pub fn resume_settles_on_the_next_drain_test() {
  let #(st, #(promise, ticket)) = suspended(rejecting: False)
  let assert PromisePending(_) = promise_state(st, promise)
  assert seen(st) == mk_string("nothing")
  let st = rt_gc.t_collect(st, [])
  assert rt_gc.t_is_live(st, handle(promise))
  let #(st, outcome) =
    host.with_context(st, key(), fn(ctx) {
      let #(ctx, outcome) = host.resume(ctx, ticket, Ok(mk_int(42)))
      let assert PromisePending(_) = promise_state(ctx.agent, promise)
      #(ctx, outcome)
    })
  assert outcome == Resumed
  assert promise_state(st, promise) == PromiseFulfilled(mk_int(42))
  assert seen(st) == mk_int(42)
}

pub fn error_outcome_rejects_test() {
  let #(st, #(promise, ticket)) = suspended(rejecting: True)
  let #(st, outcome) =
    host.with_context(st, key(), fn(ctx) {
      host.resume(ctx, ticket, Error(mk_string("no")))
    })
  assert outcome == Resumed
  assert promise_state(st, promise) == PromiseRejected(mk_string("no"))
  assert seen(st) == mk_string("no")
}

pub fn thenable_outcome_is_assimilated_test() {
  let #(st, #(promise, ticket)) = suspended(rejecting: False)
  let #(st, outcome) =
    host.with_context(st, key(), fn(ctx) {
      let promise_ctor = global(ctx.agent, "Promise").0
      let #(inner, st) =
        rt_helpers.call_method(ctx.agent, promise_ctor, "resolve", [mk_int(7)])
      host.resume(Context(..ctx, agent: st), ticket, Ok(inner))
    })
  assert outcome == Resumed
  assert promise_state(st, promise) == PromiseFulfilled(mk_int(7))
  assert seen(st) == mk_int(7)
}

pub fn double_resume_is_a_no_op_test() {
  let #(st, #(promise, ticket)) = suspended(rejecting: False)
  let #(st, outcomes) =
    host.with_context(st, key(), fn(ctx) {
      let #(ctx, first) = host.resume(ctx, ticket, Ok(mk_int(1)))
      let #(ctx, second) = host.resume(ctx, ticket, Ok(mk_int(2)))
      #(ctx, #(first, second))
    })
  assert outcomes == #(Resumed, AlreadySettled)
  assert promise_state(st, promise) == PromiseFulfilled(mk_int(1))
  assert seen(st) == mk_int(1)
  let #(st, third) =
    host.with_context(st, key(), fn(ctx) {
      host.resume(ctx, ticket, Ok(mk_int(3)))
    })
  assert third == AlreadySettled
  assert seen(st) == mk_int(1)
}

fn bare_suspend() -> #(Agent, #(JsVal, host.Ticket)) {
  use ctx <- host.with_context(agent(), key())
  let #(ctx, promise, ticket) = host.suspend(ctx)
  #(ctx, #(promise, ticket))
}

pub fn resumed_promise_is_collectable_then_stale_test() {
  let #(st, #(promise, ticket)) = bare_suspend()
  let st = rt_gc.t_collect(st, [])
  assert rt_gc.t_is_live(st, handle(promise))
  let #(st, outcome) =
    host.with_context(st, key(), fn(ctx) {
      host.resume(ctx, ticket, Ok(mk_int(1)))
    })
  assert outcome == Resumed
  let st = rt_gc.t_collect(st, [])
  assert !rt_gc.t_is_live(st, handle(promise))
  let #(_, outcome) =
    host.with_context(st, key(), fn(ctx) {
      host.resume(ctx, ticket, Ok(mk_int(2)))
    })
  assert outcome == StaleTicket
}

pub fn held_promise_survives_resume_inside_a_turn_end_test() {
  let #(st, #(promise, ticket)) = bare_suspend()
  let st =
    safepoint.finish_turn(st, [promise], fn(st) {
      let #(st, outcome) =
        host.with_context(st, key(), fn(ctx) {
          host.resume(ctx, ticket, Ok(mk_int(5)))
        })
      assert outcome == Resumed
      rt_gc.t_collect(st, [])
    })
  assert rt_gc.t_is_live(st, handle(promise))
  assert promise_state(st, promise) == PromiseFulfilled(mk_int(5))
  let st = rt_gc.t_collect(st, [])
  assert !rt_gc.t_is_live(st, handle(promise))
}

pub fn holding_the_promise_does_not_revive_a_spent_ticket_test() {
  let #(st, #(promise, ticket)) = bare_suspend()
  let #(Context(agent: st, ..), first) =
    host.resume(host.from_agent(st, key()), ticket, Ok(mk_int(1)))
  let #(st, ids) = rt_gc.t_hold_roots(st, [promise])
  let #(Context(agent: st, ..), second) =
    host.resume(host.from_agent(st, key()), ticket, Ok(mk_int(2)))
  assert #(first, second) == #(Resumed, AlreadySettled)
  let st = rt_gc.t_release_roots(rt_async.drain(st), ids)
  assert promise_state(st, promise) == PromiseFulfilled(mk_int(1))
}

pub fn foreign_ticket_is_stale_test() {
  let #(_, ticket) =
    host.with_context(agent(), key(), fn(ctx) {
      let #(ctx, _) = host.object(ctx, [])
      let #(ctx, _promise, ticket) = host.suspend(ctx)
      #(ctx, ticket)
    })
  let #(_, outcome) =
    host.with_context(agent(), key(), fn(ctx) {
      host.resume(ctx, ticket, Ok(mk_int(1)))
    })
  assert outcome == StaleTicket
}
