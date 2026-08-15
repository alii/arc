//// Smoke test for the arc/rt runtime copy: bootstrap an Agent, call a
//// builtin through the runtime's own call primitive, and round-trip a
//// property through the object model.

import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type JsVal, HostHooks, JInt, KHandle, KNum, KStr, Named, StringKey,
  classify, mk_number, mk_object, mk_string,
}
import arc/rt/val as rt_val
import gleam/option.{Some}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn t_apply_protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(rt_call.Completion, Agent)

fn hooks() -> types.HostHooks {
  HostHooks(
    monotonic_now: fn() { 0 },
    random: fn() { 0.5 },
    sleep_ms: fn(_) { Nil },
    print: fn(_) { Nil },
  )
}

fn agent() -> Agent {
  rt_builtins.new_agent(hooks())
}

pub fn array_join_via_t_call_test() {
  let st = agent()
  let #(arr, st) =
    rt_obj.t_new_array(st, [
      mk_number(JInt(1)),
      mk_number(JInt(2)),
      mk_number(JInt(3)),
    ])
  let #(join, st) = rt_obj.t_get_prop(st, arr, StringKey(Named("join")))
  let assert KHandle(_) = classify(join)
  let #(result, _st) = rt_call.t_call(st, join, arr, [])
  let assert NormalCompletion(v) = result
  assert classify(v) == KStr("1,2,3")
}

pub fn array_length_test() {
  let st = agent()
  let #(arr, st) =
    rt_obj.t_new_array(st, [
      mk_number(JInt(1)),
      mk_number(JInt(2)),
      mk_number(JInt(3)),
    ])
  let #(len, _st) = rt_obj.t_get_prop(st, arr, StringKey(Named("length")))
  assert classify(len) == KNum(JInt(3))
}

pub fn object_set_get_round_trip_test() {
  let st = agent()
  let #(h, st) = rt_obj.t_new_object(st, Some(st.realm.object.prototype))
  let obj = mk_object(h)
  let key = StringKey(Named("greeting"))
  let #(ok, st) = rt_obj.t_set_prop(st, obj, key, mk_string("hi"))
  assert ok
  let #(v, _st) = rt_obj.t_get_prop(st, obj, key)
  assert classify(v) == KStr("hi")
}

pub fn type_error_is_caught_as_throw_completion_test() {
  let st = agent()
  let #(completion, st) =
    t_apply_protected(st, fn(st) { rt_val.t_throw_type_error(st, "boom") })
  let assert ThrowCompletion(err) = completion
  let assert KHandle(_) = classify(err)
  let #(msg, _st) = rt_obj.t_get_prop(st, err, StringKey(Named("message")))
  assert classify(msg) == KStr("boom")
}
