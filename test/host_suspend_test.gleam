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

fn brand() -> host.Brand(Nil) {
  host.new_brand()
}

fn handle(v: JsVal) -> Handle {
  let assert KHandle(h) = classify(v)
  h
}

fn promise_state(st: Agent, promise: JsVal) -> PromiseState {
  rt_async.promise_data(st, handle(promise)).state
}

fn recorder(ctx: host.Context(Nil)) -> #(JsVal, host.Context(Nil)) {
  host.function(ctx, "record", 1, fn(ctx, args, _) {
    let st = ctx.agent
    let #(_, st) =
      rt_obj.set_prop(
        st,
        global(st, "globalThis").0,
        StringKey(Named("seen")),
        host.first_arg(args),
      )
    #(Ok(mk_undefined()), Context(..ctx, agent: st))
  })
}

fn seen(st: Agent) -> JsVal {
  get(st, global(st, "globalThis").0, "seen").0
}

fn suspended(rejecting rejecting: Bool) -> #(#(JsVal, host.Ticket), Agent) {
  use ctx <- host.with_context(agent(), brand())
  let ctx = host.define_global(ctx, "seen", mk_string("nothing"))
  let #(promise, ticket, ctx) = host.suspend(ctx)
  let #(on_settle, ctx) = recorder(ctx)
  let #(on_fulfilled, on_rejected) = case rejecting {
    False -> #(on_settle, mk_undefined())
    True -> #(mk_undefined(), on_settle)
  }
  let #(_, st) =
    rt_async.promise_then(ctx.agent, handle(promise), on_fulfilled, on_rejected)
  #(#(promise, ticket), Context(..ctx, agent: st))
}

pub fn resume_settles_on_the_next_drain_test() {
  let #(#(promise, ticket), st) = suspended(rejecting: False)
  let assert PromisePending(_) = promise_state(st, promise)
  assert seen(st) == mk_string("nothing")
  let st = rt_gc.collect(st, [])
  assert rt_gc.is_live(st, handle(promise))
  let #(outcome, st) =
    host.with_context(st, brand(), fn(ctx) {
      let #(outcome, ctx) = host.resume(ctx, ticket, Ok(mk_int(42)))
      let assert PromisePending(_) = promise_state(ctx.agent, promise)
      #(outcome, ctx)
    })
  assert outcome == Resumed
  assert promise_state(st, promise) == PromiseFulfilled(mk_int(42))
  assert seen(st) == mk_int(42)
}

pub fn error_outcome_rejects_test() {
  let #(#(promise, ticket), st) = suspended(rejecting: True)
  let #(outcome, st) =
    host.with_context(st, brand(), fn(ctx) {
      host.resume(ctx, ticket, Error(mk_string("no")))
    })
  assert outcome == Resumed
  assert promise_state(st, promise) == PromiseRejected(mk_string("no"))
  assert seen(st) == mk_string("no")
}

pub fn thenable_outcome_is_assimilated_test() {
  let #(#(promise, ticket), st) = suspended(rejecting: False)
  let #(outcome, st) =
    host.with_context(st, brand(), fn(ctx) {
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
  let #(#(promise, ticket), st) = suspended(rejecting: False)
  let #(outcomes, st) =
    host.with_context(st, brand(), fn(ctx) {
      let #(first, ctx) = host.resume(ctx, ticket, Ok(mk_int(1)))
      let #(second, ctx) = host.resume(ctx, ticket, Ok(mk_int(2)))
      #(#(first, second), ctx)
    })
  assert outcomes == #(Resumed, AlreadySettled)
  assert promise_state(st, promise) == PromiseFulfilled(mk_int(1))
  assert seen(st) == mk_int(1)
  let #(third, st) =
    host.with_context(st, brand(), fn(ctx) {
      host.resume(ctx, ticket, Ok(mk_int(3)))
    })
  assert third == AlreadySettled
  assert seen(st) == mk_int(1)
}

fn bare_suspend() -> #(#(JsVal, host.Ticket), Agent) {
  use ctx <- host.with_context(agent(), brand())
  let #(promise, ticket, ctx) = host.suspend(ctx)
  #(#(promise, ticket), ctx)
}

pub fn resumed_promise_is_collectable_then_stale_test() {
  let #(#(promise, ticket), st) = bare_suspend()
  let st = rt_gc.collect(st, [])
  assert rt_gc.is_live(st, handle(promise))
  let #(outcome, st) =
    host.with_context(st, brand(), fn(ctx) {
      host.resume(ctx, ticket, Ok(mk_int(1)))
    })
  assert outcome == Resumed
  let st = rt_gc.collect(st, [])
  assert !rt_gc.is_live(st, handle(promise))
  let #(outcome, _) =
    host.with_context(st, brand(), fn(ctx) {
      host.resume(ctx, ticket, Ok(mk_int(2)))
    })
  assert outcome == StaleTicket
}

pub fn held_promise_survives_resume_inside_a_turn_end_test() {
  let #(#(promise, ticket), st) = bare_suspend()
  let st =
    safepoint.finish_turn(st, [promise], fn(st) {
      let #(outcome, st) =
        host.with_context(st, brand(), fn(ctx) {
          host.resume(ctx, ticket, Ok(mk_int(5)))
        })
      assert outcome == Resumed
      rt_gc.collect(st, [])
    })
  assert rt_gc.is_live(st, handle(promise))
  assert promise_state(st, promise) == PromiseFulfilled(mk_int(5))
  let st = rt_gc.collect(st, [])
  assert !rt_gc.is_live(st, handle(promise))
}

pub fn holding_the_promise_does_not_revive_a_spent_ticket_test() {
  let #(#(promise, ticket), st) = bare_suspend()
  let #(first, Context(agent: st, ..)) =
    host.resume(host.from_agent(st, brand()), ticket, Ok(mk_int(1)))
  let #(ids, st) = rt_gc.hold_roots(st, [promise])
  let #(second, Context(agent: st, ..)) =
    host.resume(host.from_agent(st, brand()), ticket, Ok(mk_int(2)))
  assert #(first, second) == #(Resumed, AlreadySettled)
  let st = rt_gc.release_roots(rt_async.drain(st), ids)
  assert promise_state(st, promise) == PromiseFulfilled(mk_int(1))
}

pub fn foreign_ticket_is_stale_test() {
  let #(ticket, _) =
    host.with_context(agent(), brand(), fn(ctx) {
      let #(_, ctx) = host.object(ctx, [])
      let #(_promise, ticket, ctx) = host.suspend(ctx)
      #(ticket, ctx)
    })
  let #(outcome, _) =
    host.with_context(agent(), brand(), fn(ctx) {
      host.resume(ctx, ticket, Ok(mk_int(1)))
    })
  assert outcome == StaleTicket
}
