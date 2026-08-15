//// The embedder API in arc/host over the shared runtime: host functions are
//// `KNative(HostFn(id))` cells whose closures live on `Agent.host_fns`, and
//// host objects are `KHost` cells carrying an erased, typed-on-read payload.

import arc/host.{State}
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/inspect as rt_inspect
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, JInt, KHandle, KHost, KNative, KStr,
  Named, SObject, StringKey, classify, mk_number, mk_object, mk_string,
  mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import rt_helpers.{agent, get, global}

type Payload {
  Pid(Int)
  Holds(Handle)
}

fn int(i: Int) -> JsVal {
  mk_number(JInt(i))
}

fn str(v: JsVal) -> String {
  let assert KStr(s) = classify(v)
  s
}

fn handle(v: JsVal) -> Handle {
  let assert KHandle(h) = classify(v)
  h
}

/// `e.name: e.message` of a thrown error object.
fn describe(st: Agent, e: JsVal) -> String {
  str(get(st, e, "name").0) <> ": " <> str(get(st, e, "message").0)
}

fn twice(args, _this, s: host.State(Payload)) {
  use n, s <- host.validate_integer(s, host.first_arg(args), "n", 0, 100)
  #(s, Ok(int(n * 2)))
}

pub fn define_fn_installs_a_callable_global_test() {
  let st = host.define_fn(host.from_agent(agent()), "twice", 1, twice).agent
  let #(f, st) = global(st, "twice")
  let assert SObject(kind: KNative(tag: types.HostFn(0), ..), ..) =
    rt_store.t_cell_get(st, handle(f))
  assert dict.size(st.host_fns) == 1
  let #(c, st) = rt_call.t_call(st, f, mk_undefined(), [int(21)])
  assert c == NormalCompletion(int(42))
  assert str(get(st, f, "name").0) == "twice"
  assert get(st, f, "length").0 == int(1)
  assert rt_inspect.inspect(st, f) == "[Function: twice]"
}

pub fn error_result_becomes_a_throw_test() {
  let st = host.define_fn(host.from_agent(agent()), "twice", 1, twice).agent
  let #(f, st) = global(st, "twice")
  // Not a number: TypeError naming the JS type.
  let #(c, st) = rt_call.t_call(st, f, mk_undefined(), [mk_string("3")])
  let assert ThrowCompletion(e) = c
  assert describe(st, e)
    == "TypeError: The \"n\" argument must be of type integer. Received type string"
  // A number that is not an integer: RangeError.
  let #(c, st) =
    rt_call.t_call(st, f, mk_undefined(), [mk_number(types.JFloat(1.5))])
  let assert ThrowCompletion(e) = c
  assert describe(st, e)
    == "RangeError: The value of \"n\" is out of range. It must be an integer. Received 1.5"
  let #(c, st) = rt_call.t_call(st, f, mk_undefined(), [mk_number(types.JNan)])
  let assert ThrowCompletion(e) = c
  assert string.ends_with(describe(st, e), "Received NaN")
  // Out of range.
  let #(c, st) = rt_call.t_call(st, f, mk_undefined(), [int(101)])
  let assert ThrowCompletion(e) = c
  assert describe(st, e)
    == "RangeError: The value of \"n\" is out of range. It must be >= 0 and <= 100. Received 101"
}

pub fn validators_unwrap_or_throw_test() {
  let s = host.from_agent(agent())
  let s =
    host.define_fn(s, "shout", 1, fn(args, _, s) {
      use text, s <- host.validate_string(s, host.first_arg(args), "text")
      #(s, Ok(mk_string(string.uppercase(text))))
    })
  let s =
    host.define_fn(s, "flip", 1, fn(args, _, s) {
      use b, s <- host.validate_boolean(s, host.first_arg(args), "flag")
      #(s, Ok(types.mk_bool(!b)))
    })
  let st = s.agent
  let #(shout, st) = global(st, "shout")
  let #(flip, st) = global(st, "flip")
  assert rt_call.t_call(st, shout, mk_undefined(), [mk_string("hi")]).0
    == NormalCompletion(mk_string("HI"))
  assert rt_call.t_call(st, flip, mk_undefined(), [types.mk_bool(True)]).0
    == NormalCompletion(types.mk_bool(False))
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_call(st, shout, mk_undefined(), [int(1)])
  assert describe(st, e)
    == "TypeError: The \"text\" argument must be of type string. Received type number"
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_call(st, flip, mk_undefined(), [mk_undefined()])
  assert describe(st, e)
    == "TypeError: The \"flag\" argument must be of type boolean. Received type undefined"
}

pub fn try_call_calls_back_into_js_test() {
  let s = host.from_agent(agent())
  // apply(fn, x) => fn(x) + 1, propagating fn's throw.
  let s =
    host.define_fn(s, "apply", 2, fn(args, _, s) {
      use r, s <- host.try_call(s, host.first_arg(args), "fn", mk_undefined(), [
        host.arg_at(args, 1),
      ])
      use n, s <- host.validate_integer(s, r, "result", -1000, 1000)
      #(s, Ok(int(n + 1)))
    })
  let s =
    host.define_fn(s, "boom", 0, fn(_, _, s) { host.type_error(s, "boom") })
  let st = s.agent
  let #(apply, st) = global(st, "apply")
  let math_abs = get(st, global(st, "Math").0, "abs").0
  assert rt_call.t_call(st, apply, mk_undefined(), [math_abs, int(-3)]).0
    == NormalCompletion(int(4))
  // Not callable: TypeError from the validator, nothing called.
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_call(st, apply, mk_undefined(), [int(0), int(0)])
  assert describe(st, e)
    == "TypeError: The \"fn\" argument must be of type function. Received type number"
  // The callee's throw propagates out of the host function unchanged.
  let #(boom, st) = global(st, "boom")
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_call(st, apply, mk_undefined(), [boom, int(0)])
  assert describe(st, e) == "TypeError: boom"
}

pub fn namespace_and_helpers_test() {
  let s = host.from_agent(agent())
  let s =
    host.define_namespace(s, "util", [
      #("pair", 2, fn(args, _, s) {
        let #(s, arr) = host.array(s, args)
        #(s, Ok(arr))
      }),
      #("point", 2, fn(args, _, s) {
        let #(s, o) =
          host.object(s, [
            #("x", host.first_arg(args)),
            #("y", host.arg_at(args, 1)),
          ])
        #(s, Ok(o))
      }),
    ])
  let st = s.agent
  let #(util, st) = global(st, "util")
  // Tagged, and its methods are non-enumerable like a built-in namespace's.
  assert rt_inspect.inspect(st, util) == "Object [util] {}"
  assert rt_inspect.inspect(st, get(st, util, "point").0) == "[Function: point]"
  let #(arr, st) = rt_helpers.call_method(st, util, "pair", [int(1), int(2)])
  assert rt_inspect.inspect(st, arr) == "[ 1, 2 ]"
  let #(is_array, st) =
    rt_helpers.call_method(st, global(st, "Array").0, "isArray", [arr])
  assert is_array == types.mk_bool(True)
  let #(p, st) = rt_helpers.call_method(st, util, "point", [int(3), int(4)])
  assert rt_inspect.inspect(st, p) == "{ x: 3, y: 4 }"
}

// ── classes ─────────────────────────────────────────────────────────────────

/// `new Point(x)`: a plain object `{x}` that also records the NewTarget it
/// was constructed under. The dispatcher re-prototypes it.
fn point_ctor(args, _this, s: host.State(Payload)) {
  let #(s, o) =
    host.object(s, [
      #("x", host.first_arg(args)),
      #("nt", host.new_target(s)),
    ])
  #(s, Ok(o))
}

fn point_get_x(_args, this, s: host.State(Payload)) {
  let #(x, st) = get(s.agent, this, "x")
  #(State(..s, agent: st), Ok(x))
}

fn point_origin(_args, this, s: host.State(Payload)) {
  // A static: `this` is the constructor it was invoked on.
  let #(h, st) = rt_call.t_construct(s.agent, this, [int(0)], this)
  #(State(..s, agent: st), Ok(mk_object(h)))
}

fn point_class(s) {
  host.class(s, "Point", 1, point_ctor, [#("getX", 0, point_get_x)], [
    #("origin", 0, point_origin),
  ])
}

pub fn class_constructs_and_reprototypes_test() {
  let #(s, point) = point_class(host.from_agent(agent()))
  let st = s.agent
  assert rt_call.is_constructor(st, point)
  let point_proto = handle(get(st, point, "prototype").0)
  let #(p, st) = rt_call.t_construct(st, point, [int(7)], point)
  let p = mk_object(p)
  // Instance of Point although the ctor allocated a plain object.
  assert rt_obj.t_get_prototype_of(st, handle(p)).0 == Some(point_proto)
  assert get(st, p, "nt").0 == point
  assert rt_helpers.call_method(st, p, "getX", []).0 == int(7)
  assert str(get(st, get(st, p, "constructor").0, "name").0) == "Point"
  // Statics see the constructor as `this`.
  let #(o, st) = rt_helpers.call_method(st, point, "origin", [])
  assert rt_helpers.call_method(st, o, "getX", []).0 == int(0)
  // Called without `new`: same body, NewTarget undefined, no re-prototype.
  let assert #(NormalCompletion(q), st) =
    rt_call.t_call(st, point, mk_undefined(), [int(1)])
  assert get(st, q, "nt").0 == mk_undefined()
  assert rt_obj.t_get_prototype_of(st, handle(q)).0
    == Some(st.realm.object.prototype)
}

pub fn subclass_new_target_picks_the_prototype_test() {
  // What `class Sub extends Point {}` + `new Sub(5)` reaches the host ctor
  // as: Point's [[Construct]] with NewTarget = Sub.
  let #(s, point) = point_class(host.from_agent(agent()))
  let #(s, sub) = host.class(s, "Sub", 1, point_ctor, [], [])
  let st = s.agent
  let sub_proto = handle(get(st, sub, "prototype").0)
  let #(p, st) = rt_call.t_construct(st, point, [int(5)], sub)
  assert rt_obj.t_get_prototype_of(st, p).0 == Some(sub_proto)
  assert get(st, mk_object(p), "nt").0 == sub
  // Inherited static: `Sub.origin()` constructs through `this` = Sub.
  let #(_, st) = rt_obj.t_set_prototype(st, handle(sub), Some(handle(point)))
  let #(o, st) = rt_helpers.call_method(st, sub, "origin", [])
  assert rt_obj.t_get_prototype_of(st, handle(o)).0 == Some(sub_proto)
}

pub fn constructor_must_return_an_object_test() {
  let #(s, bad) =
    host.class(
      host.from_agent(agent()),
      "Bad",
      0,
      fn(_, _, s) { #(s, Ok(int(1))) },
      [],
      [],
    )
  let st = s.agent
  let reflect = global(st, "Reflect").0
  let construct = get(st, reflect, "construct").0
  let #(empty, st) = rt_obj.t_new_array(st, [])
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_call(st, construct, reflect, [bad, empty])
  assert describe(st, e) == "TypeError: host constructor must return an object"
}

// ── host objects ────────────────────────────────────────────────────────────

pub fn host_object_round_trips_typed_test() {
  let s: host.State(Payload) = host.from_agent(agent())
  let #(s, tagged_proto) = host.object(s, [])
  let st =
    rt_obj.t_define_own_data(
      s.agent,
      handle(tagged_proto),
      types.SymbolKey(types.symbol_to_string_tag),
      mk_string("Pid"),
      False,
      False,
      True,
    ).1
  let s = State(..s, agent: st)
  let #(s, pid) = host.alloc_host_object(s, Pid(42), Some(handle(tagged_proto)))
  let #(s, bare) = host.alloc_host_object(s, Pid(7), None)
  let #(s, plain) = host.object(s, [])
  assert host.read_host(s, pid) == Some(Pid(42))
  assert host.read_host(s, bare) == Some(Pid(7))
  assert host.read_host(s, plain) == None
  assert host.read_host(s, int(3)) == None
  let st = s.agent
  let assert SObject(kind: KHost(_), proto: None, ..) =
    rt_store.t_cell_get(st, handle(bare))
  // Renders through the prototype's tag; typeof is "object"; not callable.
  let #(to_string, st) =
    rt_helpers.call_method(
      st,
      get(st, get(st, global(st, "Object").0, "prototype").0, "toString").0,
      "call",
      [pid],
    )
  assert str(to_string) == "[object Pid]"
  assert rt_val.t_type_of(st, pid).0 == "object"
  assert !rt_call.is_callable(st, pid)
}

pub fn gc_traces_handles_inside_payloads_and_closures_test() {
  let s: host.State(Payload) = host.from_agent(agent())
  // An unrooted object reachable only through a host payload.
  let #(s, inner) = host.object(s, [#("k", int(1))])
  let #(s, holder) = host.alloc_host_object(s, Holds(handle(inner)), None)
  // Another reachable only through a registered closure's captured env.
  let #(s, captured) = host.object(s, [])
  let s = host.define_fn(s, "peek", 0, fn(_, _, s) { #(s, Ok(captured)) })
  // And one reachable from nowhere.
  let #(s, garbage) = host.object(s, [])
  let st = rt_store.t_pin_root(s.agent, handle(holder))
  let st = rt_gc.t_collect(st, [])
  assert rt_gc.t_is_live(st, handle(inner))
  assert rt_gc.t_is_live(st, handle(captured))
  assert !rt_gc.t_is_live(st, handle(garbage))
  assert host.read_host(State(..s, agent: st), holder)
    == Some(Holds(handle(inner)))
}

pub fn unregistered_id_is_a_type_error_test() {
  // A cell whose closure was never (re-)registered on this agent.
  let st = agent()
  let #(h, st) =
    rt_call.t_native_new(st, None, types.HostFn(9), "ghost", 0, False)
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_call(st, mk_object(h), mk_undefined(), [])
  assert describe(st, e) == "TypeError: host function #9 is not registered"
}

pub fn with_state_runs_body_and_drains_test() {
  let #(st, seen) =
    host.with_state(agent(), fn(s) {
      // Body allocates and schedules a promise reaction; the drain runs it.
      let #(s, o) = host.object(s, [#("v", int(5))])
      let s = host.define_global(s, "shared", o)
      let st = s.agent
      let promise = global(st, "Promise").0
      let #(p, st) = rt_helpers.call_method(st, promise, "resolve", [int(1)])
      let #(s2, setter) =
        host.function(State(..s, agent: st), "set", 1, fn(args, _, s) {
          let st = s.agent
          let #(_, st) =
            rt_obj.t_set_prop(
              st,
              global(st, "shared").0,
              StringKey(Named("v")),
              host.first_arg(args),
            )
          #(State(..s, agent: st), Ok(mk_undefined()))
        })
      let #(_, st) = rt_helpers.call_method(s2.agent, p, "then", [setter])
      // Not yet: reactions are microtasks.
      assert get(st, global(st, "shared").0, "v").0 == int(5)
      #(State(..s2, agent: st), "done")
    })
  assert seen == "done"
  assert get(st, global(st, "shared").0, "v").0 == int(1)
}
