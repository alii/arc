//// Promise jobs, the microtask drain and unhandled-rejection reporting on
//// the arc/rt runtime.

import arc/host_hooks.{HostHooks}
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/call as rt_call
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type JsVal, HostJob, mk_object, mk_string, mk_undefined,
}
import rt_helpers

/// An agent whose uncaught-report sink records into the test mailbox.
fn recording_agent() -> Agent {
  let assert [] = reports()
  rt_builtins.new_agent(
    HostHooks(..rt_helpers.quiet_hooks(), report_uncaught: rt_helpers.record),
  )
}

fn reports() -> List(String) {
  rt_helpers.recorded()
}

fn promise_static(st: Agent, method: String, arg: JsVal) -> #(JsVal, Agent) {
  let #(promise, st) = rt_helpers.global(st, "Promise")
  rt_helpers.call_method(st, promise, method, [arg])
}

fn new_error(st: Agent, msg: String) -> #(JsVal, Agent) {
  let #(error, st) = rt_helpers.global(st, "Error")
  let #(h, st) = rt_call.t_construct(st, error, [mk_string(msg)], error)
  #(mk_object(h), st)
}

/// A function that records `label` when called and returns undefined.
fn recorder(st: Agent, label: String) -> #(JsVal, Agent) {
  rt_helpers.func(st, fn(st, _args) {
    rt_helpers.record(label)
    #(mk_undefined(), st)
  })
}

pub fn unhandled_rejection_reported_once_at_drain_end_test() {
  let st = recording_agent()
  let #(e, st) = new_error(st, "boom")
  let #(_, st) = promise_static(st, "reject", e)
  let assert [] = reports()
  let st = rt_async.drain(st)
  assert reports() == ["Uncaught (in promise) Error: boom"]
  let _ = rt_async.drain(st)
  let assert [] = reports()
}

pub fn rejection_handled_later_in_same_drain_not_reported_test() {
  let st = recording_agent()
  let #(p, st) = promise_static(st, "reject", mk_string("late"))
  let #(noop, st) = rt_helpers.func(st, fn(st, _) { #(mk_undefined(), st) })
  // A job that runs during the drain attaches the handler before the end.
  let st =
    rt_async.t_enqueue_job(
      st,
      HostJob(fn(st) { rt_helpers.call_method(st, p, "catch", [noop]).1 }),
    )
  let _ = rt_async.drain(st)
  let assert [] = reports()
}

pub fn unhandled_string_rejection_text_test() {
  let st = recording_agent()
  let #(_, st) = promise_static(st, "reject", mk_string("plain"))
  let _ = rt_async.drain(st)
  assert reports() == ["Uncaught (in promise) plain"]
}

pub fn host_job_runs_fifo_with_reaction_jobs_test() {
  let st = recording_agent()
  let #(p, st) = promise_static(st, "resolve", mk_string("v"))
  let #(first, st) = recorder(st, "then1")
  let #(second, st) = recorder(st, "then2")
  let #(_, st) = rt_helpers.call_method(st, p, "then", [first])
  let st =
    rt_async.t_enqueue_job(
      st,
      HostJob(fn(st) {
        rt_helpers.record("host")
        st
      }),
    )
  let #(_, st) = rt_helpers.call_method(st, p, "then", [second])
  let assert [] = reports()
  let _ = rt_async.drain(st)
  assert reports() == ["then1", "host", "then2"]
}

pub fn throwing_host_job_is_reported_test() {
  let st = recording_agent()
  let #(e, st) = new_error(st, "hj")
  let st = rt_async.t_enqueue_job(st, HostJob(rt_store.t_throw(_, e)))
  let _ = rt_async.drain(st)
  assert reports() == ["Uncaught (in promise job) Error: hj"]
}
