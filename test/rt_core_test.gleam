//// Core semantics on the arc/rt runtime: §10.2.1.2 this-binding by
//// strictness, the call-depth RangeError, and the one-Number invariants
//// (integers past 2^53 - 1 widen to doubles; -0 survives).

import arc/rt/builtins as rt_builtins
import arc/rt/bytecode.{type EnvTuple, type FuncTemplate}
import arc/rt/call.{type Frame, NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/ops as rt_ops
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type CompiledFn, type JsVal, Agent, FnFlags, FrameInfo, HostHooks,
  JFloat, JInt, JNegInf, JsOps, JsStore, KBool, KBytecode, KHandle, KNum, KStr,
  NoElements, SObject, StringKey, canonical_key, classify, mk_null, mk_number,
  mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{None}

@external(erlang, "arc_rt_store_ffi", "identity")
fn as_code(f: fn(Agent, Frame, List(JsVal)) -> #(JsVal, Agent)) -> CompiledFn

@external(erlang, "erlang", "element")
fn frame_at(n: Int, frame: Frame) -> JsVal

@external(erlang, "arc_rt_store_ffi", "identity")
fn template(label: String) -> FuncTemplate

@external(erlang, "arc_rt_store_ffi", "identity")
fn env(vals: List(JsVal)) -> EnvTuple

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

// ── Number: 2^53 widening and -0 ────────────────────────────────────────────

fn int(i: Int) -> JsVal {
  mk_number(JInt(i))
}

fn num(v: JsVal) {
  let assert KNum(n) = classify(v)
  n
}

fn show(st: Agent, v: JsVal) -> String {
  rt_val.t_to_string(st, v).0
}

fn is_minus_zero(st: Agent, v: JsVal) -> Bool {
  let #(q, _) = rt_ops.t_div(st, int(1), v)
  num(q) == JNegInf
}

pub fn integer_results_widen_past_2_53_test() {
  let st = agent()
  let m = int(9_007_199_254_740_991)
  let #(a, st) = rt_ops.t_add(st, m, int(1))
  assert num(a) == JFloat(9_007_199_254_740_992.0)
  let #(b, st) = rt_ops.t_add(st, m, int(2))
  assert rt_val.strict_equal(a, b)
  assert show(st, b) == "9007199254740992"
  let #(c, st) = rt_ops.t_add(st, b, int(1))
  assert show(st, c) == "9007199254740992"
  let #(d, st) = rt_ops.t_sub(st, rt_ops.t_neg(st, m).0, int(2))
  assert show(st, d) == "-9007199254740992"
  let #(e, st) = rt_ops.t_mul(st, m, m)
  assert show(st, e) == "8.112963841460666e+31"
  let #(f, st) = rt_ops.t_mul(st, int(123_456_789), int(987_654_321))
  assert show(st, f) == "121932631112635260"
  // The value ABI itself never hands out a wide integer.
  assert show(st, int(18_014_398_509_481_985)) == "18014398509481984"
  assert num(int(-9_007_199_254_740_993)) == JFloat(-9_007_199_254_740_992.0)
  assert num(int(9_007_199_254_740_991)) == JInt(9_007_199_254_740_991)
}

pub fn minus_zero_survives_integer_arithmetic_test() {
  let st = agent()
  let #(a, st) = rt_ops.t_mul(st, int(0), int(-1))
  assert is_minus_zero(st, a)
  let #(b, st) = rt_ops.t_mul(st, int(-7), int(0))
  assert is_minus_zero(st, b)
  let #(c, st) = rt_ops.t_mul(st, int(0), int(3))
  assert !is_minus_zero(st, c)
  let #(d, st) = rt_ops.t_neg(st, int(0))
  assert is_minus_zero(st, d)
  let #(e, st) = rt_ops.t_add(st, d, d)
  assert is_minus_zero(st, e)
  let #(f, st) = rt_ops.t_add(st, d, int(0))
  assert !is_minus_zero(st, f)
  let #(g, st) = rt_ops.t_sub(st, int(0), int(0))
  assert !is_minus_zero(st, g)
  let #(h, st) = rt_ops.t_mod(st, int(-4), int(2))
  assert is_minus_zero(st, h)
  let #(i, st) = rt_ops.t_mod(st, int(4), int(-2))
  assert !is_minus_zero(st, i)
  let #(j, st) = rt_ops.t_div(st, int(0), int(-5))
  assert is_minus_zero(st, j)
  assert show(st, d) == "0"
  assert !rt_val.same_value(d, int(0))
  assert rt_val.strict_equal(d, int(0))
  let object_is = global(st, "Object") |> get(st, _, "is")
  let #(r, st) = rt_call.t_call_checked(st, object_is, mk_undefined(), [d, c])
  assert classify(r) == KBool(False)
  let stringify = global(st, "JSON") |> get(st, _, "stringify")
  let #(s, st) = rt_call.t_call_checked(st, stringify, mk_undefined(), [a])
  assert classify(s) == KStr("0")
  let round = global(st, "Math") |> get(st, _, "round")
  let #(r, st) =
    rt_call.t_call_checked(st, round, mk_undefined(), [
      mk_number(JFloat(-0.4)),
    ])
  assert is_minus_zero(st, r)
}

fn global(st: Agent, name: String) -> JsVal {
  rt_obj.t_global_get(st, <<name:utf8>>).0
}

fn get(st: Agent, obj: JsVal, name: String) -> JsVal {
  rt_obj.t_get_prop(st, obj, StringKey(canonical_key(name))).0
}

// ── Error.stack from Agent.frames ────────────────────────────────────────────

fn error_stack(st: Agent, msg: String) -> String {
  let ctor = global(st, "Error")
  let #(h, st) = rt_call.t_construct(st, ctor, [mk_string(msg)], ctor)
  let assert KStr(stack) = classify(get(st, mk_object(h), "stack"))
  stack
}

pub fn error_stack_renders_frames_test() {
  let st = agent()
  assert error_stack(st, "x") == "Error: x"
  let st =
    Agent(..st, frames: [
      FrameInfo(name: "inner", script: "script", line: 3),
      FrameInfo(name: "", script: "script", line: 10),
    ])
  assert error_stack(st, "x")
    == "Error: x\n    at inner (script:3)\n    at script:10"
  let type_error = global(st, "TypeError")
  let #(h, st2) = rt_call.t_construct(st, type_error, [], type_error)
  assert classify(get(st2, mk_object(h), "stack"))
    == KStr("TypeError\n    at inner (script:3)\n    at script:10")
  // Error.stackTraceLimit caps the frame count.
  let #(_, st) =
    rt_obj.t_set_prop(
      st,
      global(st, "Error"),
      StringKey(canonical_key("stackTraceLimit")),
      int(1),
    )
  assert error_stack(st, "y") == "Error: y\n    at inner (script:3)"
}

// ── KBytecode cells dispatch through JsOps ───────────────────────────────────

pub fn bytecode_call_and_construct_use_js_ops_test() {
  let st = agent()
  let ops =
    JsOps(
      ..st.store.ops,
      call_bytecode: fn(st, _callee, this, args, _new_target) {
        let assert [a] = args
        let #(sum, st) = rt_ops.t_add(st, this, a)
        #(sum, st)
      },
      construct_bytecode: fn(st: Agent, _callee, _args, _new_target) {
        #(st.realm.array.prototype, st)
      },
    )
  let st = Agent(..st, store: JsStore(..st.store, ops:))
  let kind =
    KBytecode(
      template: template("tpl"),
      env: env([]),
      home_object: None,
      flags: FnFlags(..flags(True), is_constructor: True),
      fields_init: None,
    )
  let #(fh, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind:,
        proto: option.Some(st.realm.function.prototype),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let f = mk_object(fh)
  assert type_of(st, f) == "function"
  let #(r, st) = rt_call.t_call_checked(st, f, int(40), [int(2)])
  assert classify(r) == KNum(JInt(42))
  let #(h, st) = rt_call.t_construct(st, f, [], f)
  assert h == st.realm.array.prototype
}
