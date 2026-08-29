import arc/bytecode/key.{Named}
import arc/host_hooks.{type HostHooks, HostHooks}
import arc/rt/builtins as rt_builtins
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/types.{type Agent, type CompiledFn, type JsVal, FnFlags, StringKey}
import gleam/option.{None}

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

pub fn agent() -> Agent {
  rt_builtins.new_agent(quiet_hooks())
}

pub fn global(st: Agent, name: String) -> #(JsVal, Agent) {
  rt_obj.t_global_get(st, <<name:utf8>>)
}

pub fn get(st: Agent, recv: JsVal, name: String) -> #(JsVal, Agent) {
  rt_obj.t_get_prop(st, recv, StringKey(Named(name)))
}

pub fn call_method(
  st: Agent,
  recv: JsVal,
  name: String,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(f, st) = get(st, recv, name)
  rt_call.t_call_checked(st, f, recv, args)
}

@external(erlang, "rt_helpers_ffi", "record")
pub fn record(term: a) -> Nil

@external(erlang, "rt_helpers_ffi", "recorded")
pub fn recorded() -> List(a)

@external(erlang, "arc_rt_store_ffi", "identity")
pub fn as_code(
  f: fn(Agent, rt_call.Frame, List(JsVal)) -> #(JsVal, Agent),
) -> CompiledFn

@external(erlang, "erlang", "element")
pub fn frame_at(n: Int, frame: rt_call.Frame) -> JsVal

@external(erlang, "arc_rt_store_ffi", "identity")
pub fn as_frame(t: a) -> rt_call.Frame

@external(erlang, "arc_rt_store_ffi", "identity")
pub fn as_loc(t: a) -> types.Loc

@external(erlang, "rt_helpers_ffi", "counter_sm")
pub fn counter_sm() -> types.SmFn

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
  let #(h, st) = rt_call.t_fn_new(st, code, flags, "f", 0, None, None)
  #(types.mk_object(h), st)
}
