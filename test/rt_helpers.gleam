//// Shared fixtures for the arc/rt tests.

import arc/host_hooks.{type HostHooks, HostHooks}
import arc/rt/builtins as rt_builtins
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type CompiledFn, type JsVal, FnFlags, Named, StringKey,
}
import gleam/option.{None}

/// Deterministic hooks that print nothing: fixed clocks, fixed PRNG.
pub fn quiet_hooks() -> HostHooks {
  HostHooks(
    ..host_hooks.default_host_hooks(),
    monotonic_now: fn() { 0 },
    wall_clock_ms: fn() { 0 },
    random: fn() { 0.5 },
    sleep_ms: fn(_) { Nil },
    print: fn(_, _) { Nil },
    report_uncaught: fn(_) { Nil },
  )
}

/// A fresh agent with a full realm on `quiet_hooks`.
pub fn agent() -> Agent {
  rt_builtins.new_agent(quiet_hooks())
}

/// `globalThis[name]`.
pub fn global(st: Agent, name: String) -> #(JsVal, Agent) {
  rt_obj.t_global_get(st, <<name:utf8>>)
}

/// `recv[name]`.
pub fn get(st: Agent, recv: JsVal, name: String) -> #(JsVal, Agent) {
  rt_obj.t_get_prop(st, recv, StringKey(Named(name)))
}

/// `recv[name](...args)`; a throw raises.
pub fn call_method(
  st: Agent,
  recv: JsVal,
  name: String,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(f, st) = get(st, recv, name)
  rt_call.t_call_checked(st, f, recv, args)
}

/// Send `term` to this process's mailbox; read back with `recorded`.
@external(erlang, "rt_helpers_ffi", "record")
pub fn record(term: a) -> Nil

/// Every `record`ed term since the last call, oldest first. The caller
/// names the element type; every term recorded in one test has it.
@external(erlang, "rt_helpers_ffi", "recorded")
pub fn recorded() -> List(a)

@external(erlang, "arc_rt_store_ffi", "identity")
fn as_code(
  f: fn(Agent, rt_call.Frame, List(JsVal)) -> #(JsVal, Agent),
) -> CompiledFn

/// A JS function object whose body is the Gleam `body(st, args)`.
pub fn func(
  st: Agent,
  body: fn(Agent, List(JsVal)) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let flags =
    FnFlags(
      is_constructor: False,
      is_class_constructor: False,
      is_derived_constructor: False,
      is_arrow: True,
      is_method: False,
      is_generator: False,
      is_async: False,
      is_strict: True,
    )
  let code = as_code(fn(st, _frame, args) { body(st, args) })
  let #(h, st) = rt_call.t_fn_new(st, code, [], flags, "f", 0, None, None)
  #(types.mk_object(h), st)
}
