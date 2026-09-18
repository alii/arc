import arc/bytecode/key.{canonical_key}
import arc/internal/unsafe
import arc/interp/kernel
import arc/rt/builtins as rt_builtins
import arc/rt/bytecode.{type EnvTuple, type FuncTemplate}
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/obj as rt_obj
import arc/rt/ops as rt_ops
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type JsVal, Agent, BirthSettled, BytecodeFn, FnFlags, FrameInfo,
  JFloat, JInt, JNegInf, JPosInf, JsOps, KBool, KHandle, KNum, KStr, Store,
  StringKey, classify, mk_int, mk_null, mk_number, mk_object, mk_string,
  mk_undefined, plain_object,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{None}
import rt_helpers.{as_code, frame_at}

fn template(label: String) -> FuncTemplate {
  unsafe.coerce(label)
}

fn env(vals: List(JsVal)) -> EnvTuple {
  unsafe.coerce(vals)
}

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks())
}

fn flags(strict strict: Bool) {
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

fn this_fn(st: Agent, strict strict: Bool) -> #(JsVal, Agent) {
  let code = as_code(fn(st, frame, _args) { #(frame_at(1, frame), st) })
  let #(h, st) = rt_call.t_fn_new(st, code, flags(strict), "f", 0, None, None)
  #(mk_object(h), st)
}

fn type_of(st: Agent, v: JsVal) -> String {
  rt_val.type_of(st, v)
}

pub fn sloppy_this_boxes_primitives_test() {
  let st = agent()
  let #(f, st) = this_fn(st, strict: False)
  let #(r, st) = rt_call.t_call(st, f, mk_int(5), [])
  assert type_of(st, r) == "object"
  let #(v, st) = rt_val.t_to_number(st, r)
  assert v == JInt(5)
  let #(r, st) = rt_call.t_call(st, f, mk_string("s"), [])
  assert type_of(st, r) == "object"
  let #(r, st) = rt_call.t_call(st, f, mk_undefined(), [])
  assert r == mk_object(st.realm.global_object)
  let #(r, st) = rt_call.t_call(st, f, mk_null(), [])
  assert r == mk_object(st.realm.global_object)
  let #(o, st) = rt_obj.t_new_object_literal(st)
  let #(r, _) = rt_call.t_call(st, f, o, [])
  assert r == o
}

pub fn strict_this_passes_through_test() {
  let st = agent()
  let #(f, st) = this_fn(st, strict: True)
  let #(r, st) = rt_call.t_call(st, f, mk_int(5), [])
  assert type_of(st, r) == "number"
  let #(r, st) = rt_call.t_call(st, f, mk_undefined(), [])
  assert r == mk_undefined()
  let #(r, _) = rt_call.t_call(st, f, mk_null(), [])
  assert r == mk_null()
}

pub fn call_depth_range_error_test() {
  let st = agent()
  let code =
    as_code(fn(st, frame, _args) {
      let self = frame_at(2, frame)
      let #(arr, st) = rt_obj.t_new_array(st, [mk_int(1)])
      rt_call.t_call_method(st, arr, StringKey(canonical_key("map")), [self])
    })
  let #(h, st) =
    rt_call.t_fn_new(st, code, flags(strict: True), "f", 0, None, None)
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_try_call(st, mk_object(h), mk_undefined(), [])
  let assert KHandle(_) = classify(e)
  let #(name, st) = rt_obj.t_get_prop(st, e, StringKey(canonical_key("name")))
  assert classify(name) == KStr("RangeError")
  let #(msg, st) = rt_obj.t_get_prop(st, e, StringKey(canonical_key("message")))
  assert classify(msg) == KStr("Maximum call stack size exceeded")
  assert st.call_depth == 0
  let #(f, st) = this_fn(st, strict: True)
  let assert #(NormalCompletion(_), _) =
    rt_call.t_try_call(st, f, mk_undefined(), [])
}

fn num(v: JsVal) {
  let assert KNum(n) = classify(v)
  n
}

fn show(st: Agent, v: JsVal) -> String {
  rt_val.t_to_string(st, v).0
}

fn is_minus_zero(st: Agent, v: JsVal) -> Bool {
  let #(q, _) = rt_ops.t_div(st, mk_int(1), v)
  num(q) == JNegInf
}

pub fn integer_results_widen_past_2_53_test() {
  let st = agent()
  let m = mk_int(9_007_199_254_740_991)
  let #(a, st) = rt_ops.t_add(st, m, mk_int(1))
  assert num(a) == JFloat(9_007_199_254_740_992.0)
  let #(b, st) = rt_ops.t_add(st, m, mk_int(2))
  assert rt_val.strict_eq(a, b)
  assert show(st, b) == "9007199254740992"
  let #(c, st) = rt_ops.t_add(st, b, mk_int(1))
  assert show(st, c) == "9007199254740992"
  let #(d, st) = rt_ops.t_sub(st, rt_ops.t_neg(st, m).0, mk_int(2))
  assert show(st, d) == "-9007199254740992"
  let #(e, st) = rt_ops.t_mul(st, m, m)
  assert show(st, e) == "8.112963841460666e+31"
  let #(f, st) = rt_ops.t_mul(st, mk_int(123_456_789), mk_int(987_654_321))
  assert show(st, f) == "121932631112635260"
  assert show(st, mk_int(18_014_398_509_481_985)) == "18014398509481984"
  assert num(mk_int(-9_007_199_254_740_993)) == JFloat(-9_007_199_254_740_992.0)
  assert num(mk_int(9_007_199_254_740_991)) == JInt(9_007_199_254_740_991)
}

pub fn float_overflow_is_infinity_test() {
  let st = agent()
  let big = mk_number(JFloat(1.0e308))
  let max = mk_number(JFloat(1.7976931348623157e308))
  let #(a, st) = rt_ops.t_mul(st, big, mk_int(10))
  assert num(a) == JPosInf
  let #(b, st) = rt_ops.t_mul(st, rt_ops.t_neg(st, big).0, mk_int(10))
  assert num(b) == JNegInf
  let #(c, st) = rt_ops.t_add(st, big, big)
  assert num(c) == JPosInf
  let #(d, st) = rt_ops.t_sub(st, rt_ops.t_neg(st, big).0, big)
  assert num(d) == JNegInf
  let #(e, st) = rt_ops.t_mul(st, max, mk_int(2))
  assert num(e) == JPosInf
  let #(f, st) = rt_ops.t_div(st, big, mk_number(JFloat(1.0e-10)))
  assert num(f) == JPosInf
  let #(g, st) = rt_ops.t_div(st, big, mk_number(JFloat(-1.0e-10)))
  assert num(g) == JNegInf
  let #(h, st) = rt_ops.t_pow(st, mk_int(-10), mk_int(401))
  assert num(h) == JNegInf
  assert num(kernel.add(big, big)) == JPosInf
  assert num(kernel.sub(mk_number(JFloat(-1.0e308)), big)) == JNegInf
  assert num(kernel.mul(big, mk_number(JFloat(-10.0)))) == JNegInf
  assert num(kernel.mul(mk_int(3), mk_int(4))) == JInt(12)
  assert show(st, a) == "Infinity"
}

pub fn minus_zero_survives_integer_arithmetic_test() {
  let st = agent()
  let #(a, st) = rt_ops.t_mul(st, mk_int(0), mk_int(-1))
  assert is_minus_zero(st, a)
  let #(b, st) = rt_ops.t_mul(st, mk_int(-7), mk_int(0))
  assert is_minus_zero(st, b)
  let #(c, st) = rt_ops.t_mul(st, mk_int(0), mk_int(3))
  assert !is_minus_zero(st, c)
  let #(d, st) = rt_ops.t_neg(st, mk_int(0))
  assert is_minus_zero(st, d)
  let #(e, st) = rt_ops.t_add(st, d, d)
  assert is_minus_zero(st, e)
  let #(f, st) = rt_ops.t_add(st, d, mk_int(0))
  assert !is_minus_zero(st, f)
  let #(g, st) = rt_ops.t_sub(st, mk_int(0), mk_int(0))
  assert !is_minus_zero(st, g)
  let #(h, st) = rt_ops.t_mod(st, mk_int(-4), mk_int(2))
  assert is_minus_zero(st, h)
  let #(i, st) = rt_ops.t_mod(st, mk_int(4), mk_int(-2))
  assert !is_minus_zero(st, i)
  let #(j, st) = rt_ops.t_div(st, mk_int(0), mk_int(-5))
  assert is_minus_zero(st, j)
  assert show(st, d) == "0"
  assert !rt_val.same_value(d, mk_int(0))
  assert rt_val.strict_eq(d, mk_int(0))
  let object_is = global(st, "Object") |> get(st, _, "is")
  let #(r, st) = rt_call.t_call(st, object_is, mk_undefined(), [d, c])
  assert classify(r) == KBool(False)
  let stringify = global(st, "JSON") |> get(st, _, "stringify")
  let #(s, st) = rt_call.t_call(st, stringify, mk_undefined(), [a])
  assert classify(s) == KStr("0")
  let round = global(st, "Math") |> get(st, _, "round")
  let #(r, st) =
    rt_call.t_call(st, round, mk_undefined(), [
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
  let #(_, st) =
    rt_obj.t_set_prop(
      st,
      global(st, "Error"),
      StringKey(canonical_key("stackTraceLimit")),
      mk_int(1),
    )
  assert error_stack(st, "y") == "Error: y\n    at inner (script:3)"
}

pub fn bytecode_call_and_construct_use_js_ops_test() {
  let st = agent()
  let ops =
    JsOps(
      ..st.store.ops,
      call_bytecode: fn(st, _callee, _kind, this, args) {
        let assert [a] = args
        let #(sum, st) = rt_ops.t_add(st, this, a)
        #(Ok(sum), st)
      },
      prepare_call: fn(_st, _callee, _kind, _this) {
        panic as "prepare_call not under test"
      },
      construct_bytecode: fn(st: Agent, _callee, _args, _new_target) {
        #(st.realm.array.prototype, st)
      },
    )
  let st = Agent(..st, store: Store(..st.store, ops:))
  let kind =
    BytecodeFn(
      template: template("tpl"),
      env: env([]),
      home_object: None,
      flags: FnFlags(..flags(strict: True), is_constructor: True),
      fields_init: None,
      realm: 0,
      unit_id: 0,
      birth: BirthSettled,
    )
  let #(fh, st) =
    rt_store.t_cell_new(
      st,
      plain_object(kind, option.Some(st.realm.function.prototype), dict.new()),
    )
  let f = mk_object(fh)
  assert type_of(st, f) == "function"
  let #(r, st) = rt_call.t_call(st, f, mk_int(40), [mk_int(2)])
  assert classify(r) == KNum(JInt(42))
  let #(h, st) = rt_call.t_construct(st, f, [], f)
  assert h == st.realm.array.prototype
}

fn handle(v: JsVal) {
  let assert KHandle(h) = classify(v)
  h
}

pub fn weak_map_value_traced_until_key_dies_test() {
  let st = agent()
  let wm_ctor = global(st, "WeakMap")
  let #(wm_h, st) = rt_call.t_construct(st, wm_ctor, [], wm_ctor)
  let wm = mk_object(wm_h)
  let #(k, st) = rt_obj.t_new_object_literal(st)
  let #(v, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) =
    rt_call.t_call_method(st, wm, StringKey(canonical_key("set")), [k, v])
  let st = rt_gc.t_collect(st, [wm_h, handle(k)])
  assert rt_gc.t_is_live(st, handle(v))
  let #(got, st) =
    rt_call.t_call_method(st, wm, StringKey(canonical_key("get")), [k])
  assert got == v
  let st = rt_gc.t_collect(st, [wm_h])
  assert !rt_gc.t_is_live(st, handle(k))
  let #(has, st) =
    rt_call.t_call_method(st, wm, StringKey(canonical_key("has")), [k])
  assert classify(has) == KBool(False)
  let st = rt_gc.t_collect(st, [wm_h])
  assert !rt_gc.t_is_live(st, handle(v))
}

fn call_method(st: Agent, recv: JsVal, name: String, args: List(JsVal)) {
  rt_call.t_call_method(st, recv, StringKey(canonical_key(name)), args)
}

pub fn map_get_or_insert_test() {
  let st = agent()
  let map_ctor = global(st, "Map")
  let #(mh, st) = rt_call.t_construct(st, map_ctor, [], map_ctor)
  let m = mk_object(mh)
  let #(r, st) = call_method(st, m, "getOrInsert", [mk_string("a"), mk_int(1)])
  assert classify(r) == KNum(JInt(1))
  let #(r, st) = call_method(st, m, "getOrInsert", [mk_string("a"), mk_int(2)])
  assert classify(r) == KNum(JInt(1))
  let #(mz, st) = rt_ops.t_neg(st, mk_int(0))
  let #(_, st) = call_method(st, m, "getOrInsert", [mz, mk_string("z")])
  let #(r, st) = call_method(st, m, "get", [mk_int(0)])
  assert classify(r) == KStr("z")
  let #(size, st) = rt_obj.t_get_prop(st, m, StringKey(canonical_key("size")))
  assert classify(size) == KNum(JInt(2))
  let assert #(ThrowCompletion(_), _) =
    rt_call.t_try_call(
      st,
      get(st, m, "getOrInsert"),
      mk_string("not a map"),
      [],
    )
}

pub fn map_get_or_insert_computed_test() {
  let st = agent()
  let map_ctor = global(st, "Map")
  let #(mh, st) = rt_call.t_construct(st, map_ctor, [], map_ctor)
  let m = mk_object(mh)
  let seen_key =
    as_code(fn(st, _frame, args) {
      let assert [k] = args
      let #(q, st) = rt_ops.t_div(st, mk_int(1), k)
      #(mk_string(rt_val.t_to_string(st, q).0), st)
    })
  let #(fh, st) =
    rt_call.t_fn_new(st, seen_key, flags(strict: True), "f", 1, None, None)
  let f = mk_object(fh)
  let #(mz, st) = rt_ops.t_neg(st, mk_int(0))
  let #(r, st) = call_method(st, m, "getOrInsertComputed", [mz, f])
  assert classify(r) == KStr("Infinity")
  let boom =
    as_code(fn(st, _frame, _args) { rt_val.t_throw_type_error(st, "called") })
  let #(bh, st) =
    rt_call.t_fn_new(st, boom, flags(strict: True), "b", 1, None, None)
  let #(r, st) =
    call_method(st, m, "getOrInsertComputed", [mk_int(0), mk_object(bh)])
  assert classify(r) == KStr("Infinity")
  let sneaky =
    as_code(fn(st, _frame, args) {
      let assert [k] = args
      let #(_, st) = call_method(st, m, "set", [k, mk_string("inner")])
      #(mk_string("outer"), st)
    })
  let #(sh, st) =
    rt_call.t_fn_new(st, sneaky, flags(strict: True), "s", 1, None, None)
  let #(r, st) =
    call_method(st, m, "getOrInsertComputed", [mk_string("k"), mk_object(sh)])
  assert classify(r) == KStr("outer")
  let #(r, st) = call_method(st, m, "get", [mk_string("k")])
  assert classify(r) == KStr("outer")
  let #(size, st) = rt_obj.t_get_prop(st, m, StringKey(canonical_key("size")))
  assert classify(size) == KNum(JInt(2))
  let assert #(ThrowCompletion(_), _) =
    rt_call.t_try_call(st, get(st, m, "getOrInsertComputed"), m, [
      mk_int(0),
      mk_int(1),
    ])
}

pub fn map_group_by_test() {
  let st = agent()
  let map_ctor = global(st, "Map")
  let #(items, st) =
    rt_obj.t_new_array(st, [
      mk_int(1),
      mk_int(2),
      mk_int(3),
      mk_int(4),
      mk_number(JFloat(1.5)),
    ])
  let parity =
    as_code(fn(st, _frame, args) {
      let assert [n, ..] = args
      let #(r, st) = rt_ops.t_mod(st, n, mk_int(2))
      case classify(r) {
        KNum(JInt(0)) -> #(mk_string("even"), st)
        KNum(JInt(_)) -> #(mk_string("odd"), st)
        _ -> rt_ops.t_neg(st, mk_int(0))
      }
    })
  let #(ph, st) =
    rt_call.t_fn_new(st, parity, flags(strict: True), "p", 1, None, None)
  let #(g, st) = call_method(st, map_ctor, "groupBy", [items, mk_object(ph)])
  assert type_of(st, g) == "object"
  let #(size, st) = rt_obj.t_get_prop(st, g, StringKey(canonical_key("size")))
  assert classify(size) == KNum(JInt(3))
  let #(odd, st) = call_method(st, g, "get", [mk_string("odd")])
  let #(joined, st) = call_method(st, odd, "join", [])
  assert classify(joined) == KStr("1,3")
  let #(even, st) = call_method(st, g, "get", [mk_string("even")])
  let #(joined, st) = call_method(st, even, "join", [])
  assert classify(joined) == KStr("2,4")
  let #(zero, st) = call_method(st, g, "get", [mk_int(0)])
  let #(joined, st) = call_method(st, zero, "join", [])
  assert classify(joined) == KStr("1.5")
  let #(keys_iter, st) = call_method(st, g, "keys", [])
  let #(first, st) = call_method(st, keys_iter, "next", [])
  assert classify(get(st, first, "value")) == KStr("odd")
  let assert #(ThrowCompletion(_), _) =
    rt_call.t_try_call(st, get(st, map_ctor, "groupBy"), map_ctor, [
      items,
      mk_int(1),
    ])
}
