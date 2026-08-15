//// Core call semantics on the arc/rt runtime: §10.2.1.2 this-binding by
//// strictness and the call-depth RangeError.

import arc/rt/builtins as rt_builtins
import arc/rt/call.{type Frame, NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type CompiledFn, type JsVal, FnFlags, HostHooks, JInt, KHandle,
  KStr, StringKey, canonical_key, classify, mk_null, mk_number, mk_object,
  mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/option.{None}

@external(erlang, "arc_rt_store_ffi", "identity")
fn as_code(f: fn(Agent, Frame, List(JsVal)) -> #(JsVal, Agent)) -> CompiledFn

@external(erlang, "erlang", "element")
fn frame_at(n: Int, frame: Frame) -> JsVal

fn agent() -> Agent {
  rt_builtins.new_agent(
    HostHooks(
      monotonic_now: fn() { 0 },
      random: fn() { 0.5 },
      sleep_ms: fn(_) { Nil },
      print: fn(_) { Nil },
    ),
  )
}

fn flags(strict: Bool) {
  FnFlags(
    is_constructor: False,
    is_class_constructor: False,
    is_derived_constructor: False,
    is_arrow: False,
    is_method: False,
    is_generator: False,
    is_async: False,
    is_strict: strict,
  )
}

/// A function object whose body returns its `this` binding.
fn this_fn(st: Agent, strict: Bool) -> #(JsVal, Agent) {
  let code = as_code(fn(st, frame, _args) { #(frame_at(1, frame), st) })
  let #(h, st) =
    rt_call.t_fn_new(st, code, [], flags(strict), "f", 0, None, None)
  #(mk_object(h), st)
}

fn type_of(st: Agent, v: JsVal) -> String {
  rt_val.t_type_of(st, v).0
}

pub fn sloppy_this_boxes_primitives_test() {
  let st = agent()
  let #(f, st) = this_fn(st, False)
  let #(r, st) = rt_call.t_call_checked(st, f, mk_number(JInt(5)), [])
  assert type_of(st, r) == "object"
  let #(v, st) = rt_val.t_to_number(st, r)
  assert v == JInt(5)
  let #(r, st) = rt_call.t_call_checked(st, f, mk_string("s"), [])
  assert type_of(st, r) == "object"
  let #(r, st) = rt_call.t_call_checked(st, f, mk_undefined(), [])
  assert r == mk_object(st.realm.global_object)
  let #(r, st) = rt_call.t_call_checked(st, f, mk_null(), [])
  assert r == mk_object(st.realm.global_object)
  let #(o, st) = rt_obj.t_new_object_literal(st)
  let #(r, _) = rt_call.t_call_checked(st, f, o, [])
  assert r == o
}

pub fn strict_this_passes_through_test() {
  let st = agent()
  let #(f, st) = this_fn(st, True)
  let #(r, st) = rt_call.t_call_checked(st, f, mk_number(JInt(5)), [])
  assert type_of(st, r) == "number"
  let #(r, st) = rt_call.t_call_checked(st, f, mk_undefined(), [])
  assert r == mk_undefined()
  let #(r, _) = rt_call.t_call_checked(st, f, mk_null(), [])
  assert r == mk_null()
}

/// `[1].map(function f(){ return [1].map(f) })`: unbounded recursion that
/// re-enters through a builtin on every level ends in a catchable RangeError.
pub fn call_depth_range_error_test() {
  let st = agent()
  let code =
    as_code(fn(st, frame, _args) {
      let self = frame_at(2, frame)
      let #(arr, st) = rt_obj.t_new_array(st, [mk_number(JInt(1))])
      rt_call.t_call_method(st, arr, StringKey(canonical_key("map")), [self])
    })
  let #(h, st) = rt_call.t_fn_new(st, code, [], flags(True), "f", 0, None, None)
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_call(st, mk_object(h), mk_undefined(), [])
  let assert KHandle(_) = classify(e)
  let #(name, st) = rt_obj.t_get_prop(st, e, StringKey(canonical_key("name")))
  assert classify(name) == KStr("RangeError")
  let #(msg, st) = rt_obj.t_get_prop(st, e, StringKey(canonical_key("message")))
  assert classify(msg) == KStr("Maximum call stack size exceeded")
  // The agent is usable afterwards and depth accounting is balanced.
  assert st.store.call_depth == 0
  let #(f, st) = this_fn(st, True)
  let assert #(NormalCompletion(_), _) =
    rt_call.t_call(st, f, mk_undefined(), [])
}
