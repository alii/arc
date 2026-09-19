import arc/bytecode/key.{Named}
import arc/host_hooks.{type HostHooks, HostHooks}
import arc/internal/unsafe
import arc/rt/builtins as rt_builtins
import arc/rt/call as rt_call
import arc/rt/lang as rt_lang
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type CompiledCode, type JsVal, type JsValKind, JFloat, JInt, KNum,
  StringKey,
}
import gleam/int

pub fn quiet_hooks() -> HostHooks {
  HostHooks(
    ..host_hooks.default(),
    monotonic_now: fn() { 0 },
    wall_clock_ms: fn() { 0 },
    random: fn() { 0.5 },
    sleep_ms: fn(_) { Nil },
    print: fn(_, _) { Nil },
    report_uncaught: fn(_) { Nil },
  )
}

// ints widened to floats so a test writes one number spelling
pub fn classify(v: JsVal) -> JsValKind {
  case types.classify(v) {
    KNum(JInt(i)) -> KNum(JFloat(int.to_float(i)))
    kind -> kind
  }
}

pub fn agent() -> Agent {
  rt_builtins.new_agent(quiet_hooks())
}

pub fn global(st: Agent, name: String) -> #(JsVal, Agent) {
  rt_lang.global_get(st, <<name:utf8>>)
}

pub fn get(st: Agent, recv: JsVal, name: String) -> #(JsVal, Agent) {
  rt_obj.get_prop(st, recv, StringKey(Named(name)))
}

pub fn call_method(
  st: Agent,
  recv: JsVal,
  name: String,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(f, st) = get(st, recv, name)
  rt_call.call(st, f, recv, args)
}

@external(erlang, "rt_helpers_ffi", "record")
pub fn record(term: a) -> Nil

@external(erlang, "rt_helpers_ffi", "recorded")
pub fn recorded() -> List(a)

pub fn as_code(
  f: fn(Agent, rt_call.Frame, List(JsVal)) -> #(JsVal, Agent),
) -> CompiledCode {
  unsafe.coerce(f)
}

@external(erlang, "erlang", "element")
pub fn frame_at(n: Int, frame: rt_call.Frame) -> JsVal

pub fn as_frame(t: a) -> rt_call.Frame {
  unsafe.coerce(t)
}

pub fn as_locals(t: a) -> types.Locals {
  unsafe.coerce(t)
}

@external(erlang, "rt_helpers_ffi", "counter_state_machine")
pub fn counter_state_machine() -> types.StateMachine

pub fn func(
  st: Agent,
  body: fn(Agent, List(JsVal)) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let #(h, st) = rt_call.new_builtin_function(st, "f", 0, body)
  #(types.mk_object(h), st)
}
