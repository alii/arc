import arc/bytecode/key.{Named}
import arc/host.{Context}
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/inspect as rt_inspect
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, Handle, HostObj, KHandle, KStr, NativeFn,
  SObject, StringKey, classify, mk_int, mk_number, mk_object, mk_string,
  mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleam/string
import rt_helpers.{agent, get, global}

type Payload {
  Pid(Int)
  Holds(Handle)
}

fn brand() -> host.Brand(Payload) {
  host.new_brand()
}

fn str(v: JsVal) -> String {
  let assert KStr(s) = classify(v)
  s
}

fn handle(v: JsVal) -> Handle {
  let assert KHandle(h) = classify(v)
  h
}

fn describe(st: Agent, e: JsVal) -> String {
  str(get(st, e, "name").0) <> ": " <> str(get(st, e, "message").0)
}

fn twice(ctx: host.Context(Payload), args, _this) {
  use n, ctx <- host.validate_integer(ctx, host.first_arg(args), "n", 0, 100)
  #(Ok(mk_int(n * 2)), ctx)
}

pub fn define_fn_installs_a_callable_global_test() {
  let st =
    host.define_fn(host.from_agent(agent(), brand()), "twice", 1, twice).agent
  let #(f, st) = global(st, "twice")
  let assert SObject(kind: NativeFn(token: types.HostFn(0), ..), ..) =
    rt_store.t_cell_get(st, handle(f))
  assert dict.size(st.host_fns) == 1
  let #(c, st) = rt_call.t_try_call(st, f, mk_undefined(), [mk_int(21)])
  assert c == NormalCompletion(mk_int(42))
  assert str(get(st, f, "name").0) == "twice"
  assert get(st, f, "length").0 == mk_int(1)
  assert rt_inspect.inspect(st, f) == "[Function: twice]"
}

pub fn error_result_becomes_a_throw_test() {
  let st =
    host.define_fn(host.from_agent(agent(), brand()), "twice", 1, twice).agent
  let #(f, st) = global(st, "twice")
  let #(c, st) = rt_call.t_try_call(st, f, mk_undefined(), [mk_string("3")])
  let assert ThrowCompletion(e) = c
  assert describe(st, e)
    == "TypeError: The \"n\" argument must be of type integer. Received type string"
  let #(c, st) =
    rt_call.t_try_call(st, f, mk_undefined(), [mk_number(types.JFloat(1.5))])
  let assert ThrowCompletion(e) = c
  assert describe(st, e)
    == "RangeError: The value of \"n\" is out of range. It must be an integer. Received 1.5"
  let #(c, st) =
    rt_call.t_try_call(st, f, mk_undefined(), [mk_number(types.JNan)])
  let assert ThrowCompletion(e) = c
  assert string.ends_with(describe(st, e), "Received NaN")
  let #(c, st) = rt_call.t_try_call(st, f, mk_undefined(), [mk_int(101)])
  let assert ThrowCompletion(e) = c
  assert describe(st, e)
    == "RangeError: The value of \"n\" is out of range. It must be >= 0 and <= 100. Received 101"
}

pub fn validators_unwrap_or_throw_test() {
  let ctx = host.from_agent(agent(), brand())
  let ctx =
    host.define_fn(ctx, "shout", 1, fn(ctx, args, _) {
      use text, ctx <- host.validate_string(ctx, host.first_arg(args), "text")
      #(Ok(mk_string(string.uppercase(text))), ctx)
    })
  let ctx =
    host.define_fn(ctx, "flip", 1, fn(ctx, args, _) {
      use b, ctx <- host.validate_boolean(ctx, host.first_arg(args), "flag")
      #(Ok(types.mk_bool(!b)), ctx)
    })
  let st = ctx.agent
  let #(shout, st) = global(st, "shout")
  let #(flip, st) = global(st, "flip")
  assert rt_call.t_try_call(st, shout, mk_undefined(), [mk_string("hi")]).0
    == NormalCompletion(mk_string("HI"))
  assert rt_call.t_try_call(st, flip, mk_undefined(), [types.mk_bool(True)]).0
    == NormalCompletion(types.mk_bool(False))
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_try_call(st, shout, mk_undefined(), [mk_int(1)])
  assert describe(st, e)
    == "TypeError: The \"text\" argument must be of type string. Received type number"
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_try_call(st, flip, mk_undefined(), [mk_undefined()])
  assert describe(st, e)
    == "TypeError: The \"flag\" argument must be of type boolean. Received type undefined"
}

pub fn try_call_calls_back_into_js_test() {
  let ctx = host.from_agent(agent(), brand())
  let ctx =
    host.define_fn(ctx, "apply", 2, fn(ctx, args, _) {
      use r, ctx <- host.try_call(
        ctx,
        host.first_arg(args),
        "fn",
        mk_undefined(),
        [
          host.arg_at(args, 1),
        ],
      )
      use n, ctx <- host.validate_integer(ctx, r, "result", -1000, 1000)
      #(Ok(mk_int(n + 1)), ctx)
    })
  let ctx =
    host.define_fn(ctx, "boom", 0, fn(ctx, _, _) {
      host.type_error(ctx, "boom")
    })
  let st = ctx.agent
  let #(apply, st) = global(st, "apply")
  let math_abs = get(st, global(st, "Math").0, "abs").0
  assert rt_call.t_try_call(st, apply, mk_undefined(), [math_abs, mk_int(-3)]).0
    == NormalCompletion(mk_int(4))
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_try_call(st, apply, mk_undefined(), [mk_int(0), mk_int(0)])
  assert describe(st, e)
    == "TypeError: The \"fn\" argument must be of type function. Received type number"
  let #(boom, st) = global(st, "boom")
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_try_call(st, apply, mk_undefined(), [boom, mk_int(0)])
  assert describe(st, e) == "TypeError: boom"
}

pub fn namespace_and_helpers_test() {
  let ctx = host.from_agent(agent(), brand())
  let ctx =
    host.define_namespace(ctx, "util", [
      #("pair", 2, fn(ctx, args, _) {
        let #(arr, ctx) = host.array(ctx, args)
        #(Ok(arr), ctx)
      }),
      #("point", 2, fn(ctx, args, _) {
        let #(o, ctx) =
          host.object(ctx, [
            #("x", host.first_arg(args)),
            #("y", host.arg_at(args, 1)),
          ])
        #(Ok(o), ctx)
      }),
    ])
  let st = ctx.agent
  let #(util, st) = global(st, "util")
  assert rt_inspect.inspect(st, util) == "Object [util] {}"
  assert rt_inspect.inspect(st, get(st, util, "point").0) == "[Function: point]"
  let #(arr, st) =
    rt_helpers.call_method(st, util, "pair", [mk_int(1), mk_int(2)])
  assert rt_inspect.inspect(st, arr) == "[ 1, 2 ]"
  let #(is_array, st) =
    rt_helpers.call_method(st, global(st, "Array").0, "isArray", [arr])
  assert is_array == types.mk_bool(True)
  let #(p, st) =
    rt_helpers.call_method(st, util, "point", [mk_int(3), mk_int(4)])
  assert rt_inspect.inspect(st, p) == "{ x: 3, y: 4 }"
}

fn point_ctor(ctx: host.Context(Payload), args, _this) {
  let #(o, ctx) =
    host.object(ctx, [
      #("x", host.first_arg(args)),
      #("nt", host.new_target(ctx)),
    ])
  #(Ok(o), ctx)
}

fn point_get_x(ctx: host.Context(Payload), _args, this) {
  let #(x, st) = get(ctx.agent, this, "x")
  #(Ok(x), Context(..ctx, agent: st))
}

fn point_origin(ctx: host.Context(Payload), _args, this) {
  let #(h, st) = rt_call.t_construct(ctx.agent, this, [mk_int(0)], this)
  #(Ok(mk_object(h)), Context(..ctx, agent: st))
}

fn point_class(ctx) {
  host.class(ctx, "Point", 1, point_ctor, [#("getX", 0, point_get_x)], [
    #("origin", 0, point_origin),
  ])
}

pub fn class_constructs_and_reprototypes_test() {
  let #(point, ctx) = point_class(host.from_agent(agent(), brand()))
  let st = ctx.agent
  assert rt_call.is_constructor(st, point)
  let point_proto = handle(get(st, point, "prototype").0)
  let #(p, st) = rt_call.t_construct(st, point, [mk_int(7)], point)
  let p = mk_object(p)
  assert rt_obj.t_get_prototype_of(st, handle(p)).0 == Some(point_proto)
  assert get(st, p, "nt").0 == point
  assert rt_helpers.call_method(st, p, "getX", []).0 == mk_int(7)
  assert str(get(st, get(st, p, "constructor").0, "name").0) == "Point"
  let #(o, st) = rt_helpers.call_method(st, point, "origin", [])
  assert rt_helpers.call_method(st, o, "getX", []).0 == mk_int(0)
  let assert #(NormalCompletion(q), st) =
    rt_call.t_try_call(st, point, mk_undefined(), [mk_int(1)])
  assert get(st, q, "nt").0 == mk_undefined()
  assert rt_obj.t_get_prototype_of(st, handle(q)).0
    == Some(st.realm.object.prototype)
}

pub fn subclass_new_target_picks_the_prototype_test() {
  let #(point, ctx) = point_class(host.from_agent(agent(), brand()))
  let #(sub, ctx) = host.class(ctx, "Sub", 1, point_ctor, [], [])
  let st = ctx.agent
  let sub_proto = handle(get(st, sub, "prototype").0)
  let #(p, st) = rt_call.t_construct(st, point, [mk_int(5)], sub)
  assert rt_obj.t_get_prototype_of(st, p).0 == Some(sub_proto)
  assert get(st, mk_object(p), "nt").0 == sub
  let #(_, st) = rt_obj.t_set_prototype_of(st, handle(sub), Some(handle(point)))
  let #(o, st) = rt_helpers.call_method(st, sub, "origin", [])
  assert rt_obj.t_get_prototype_of(st, handle(o)).0 == Some(sub_proto)
}

pub fn constructor_must_return_an_object_test() {
  let #(bad, ctx) =
    host.class(
      host.from_agent(agent(), brand()),
      "Bad",
      0,
      fn(ctx, _, _) { #(Ok(mk_int(1)), ctx) },
      [],
      [],
    )
  let st = ctx.agent
  let reflect = global(st, "Reflect").0
  let construct = get(st, reflect, "construct").0
  let #(empty, st) = rt_obj.t_new_array(st, [])
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_try_call(st, construct, reflect, [bad, empty])
  assert describe(st, e) == "TypeError: host constructor must return an object"
}

pub fn host_object_round_trips_typed_test() {
  let ctx: host.Context(Payload) = host.from_agent(agent(), brand())
  let #(tagged_proto, ctx) = host.object(ctx, [])
  let st =
    rt_obj.t_define_own_data(
      ctx.agent,
      handle(tagged_proto),
      types.SymbolKey(types.symbol_to_string_tag),
      mk_string("Pid"),
      writable: False,
      enumerable: False,
      configurable: True,
    ).1
  let ctx = Context(..ctx, agent: st)
  let #(pid, ctx) =
    host.alloc_host_object(ctx, Pid(42), Some(handle(tagged_proto)))
  let #(bare, ctx) = host.alloc_host_object(ctx, Pid(7), None)
  let #(plain, ctx) = host.object(ctx, [])
  assert host.read_host(ctx, pid) == Some(Pid(42))
  assert host.read_host(ctx, bare) == Some(Pid(7))
  assert host.read_host(ctx, plain) == None
  assert host.read_host(ctx, mk_int(3)) == None
  let st = ctx.agent
  let assert SObject(kind: HostObj(_), proto: None, ..) =
    rt_store.t_cell_get(st, handle(bare))
  let #(to_string, st) =
    rt_helpers.call_method(
      st,
      get(st, get(st, global(st, "Object").0, "prototype").0, "toString").0,
      "call",
      [pid],
    )
  assert str(to_string) == "[object Pid]"
  assert rt_val.type_of(st, pid) == "object"
  assert !rt_val.is_callable(st, pid)
}

pub fn gc_traces_handles_inside_payloads_and_closures_test() {
  let ctx: host.Context(Payload) = host.from_agent(agent(), brand())
  let #(inner, ctx) = host.object(ctx, [#("k", mk_int(1))])
  let #(holder, ctx) = host.alloc_host_object(ctx, Holds(handle(inner)), None)
  let #(captured, ctx) = host.object(ctx, [])
  let ctx =
    host.define_fn(ctx, "peek", 0, fn(ctx, _, _) { #(Ok(captured), ctx) })
  let #(garbage, ctx) = host.object(ctx, [])
  let st = rt_store.t_pin_root(ctx.agent, handle(holder))
  let st = rt_gc.t_collect(st, [])
  assert rt_gc.t_is_live(st, handle(inner))
  assert rt_gc.t_is_live(st, handle(captured))
  assert !rt_gc.t_is_live(st, handle(garbage))
  assert host.read_host(Context(..ctx, agent: st), holder)
    == Some(Holds(handle(inner)))
}

pub fn another_brand_reads_none_not_a_mistyped_value_test() {
  let ctx = host.from_agent(agent(), brand())
  let #(pid, ctx) = host.alloc_host_object(ctx, Pid(42), None)
  let strings: host.Brand(String) = host.new_brand()
  let other = host.from_agent(ctx.agent, strings)
  assert host.read_host(other, pid) == None
  let #(word, other) = host.alloc_host_object(other, "w", None)
  assert host.read_host(Context(..ctx, agent: other.agent), word) == None
  assert host.read_host(host.from_agent(other.agent, brand()), pid) == None
  assert host.read_host(other, word) == Some("w")
  assert host.read_host(Context(..ctx, agent: other.agent), pid)
    == Some(Pid(42))
}

pub fn host_functions_see_the_brand_they_were_defined_under_test() {
  let ctx = host.from_agent(agent(), brand())
  let ctx =
    host.define_fn(ctx, "wrap", 1, fn(ctx, args, _) {
      use n, ctx <- host.validate_integer(
        ctx,
        host.first_arg(args),
        "n",
        0,
        100,
      )
      let #(o, ctx) = host.alloc_host_object(ctx, Pid(n), None)
      #(Ok(o), ctx)
    })
  let ctx =
    host.define_fn(ctx, "unwrap", 1, fn(ctx, args, _) {
      case host.read_host(ctx, host.first_arg(args)) {
        Some(Pid(n)) -> #(Ok(mk_int(n)), ctx)
        Some(Holds(_)) | None -> host.type_error(ctx, "not a pid")
      }
    })
  let st = ctx.agent
  let #(wrapped, st) =
    rt_call.t_call(st, global(st, "wrap").0, mk_undefined(), [mk_int(9)])
  let #(n, st) =
    rt_call.t_call(st, global(st, "unwrap").0, mk_undefined(), [
      wrapped,
    ])
  assert n == mk_int(9)
  let #(_promise, _ticket, ctx) = host.suspend(Context(..ctx, agent: st))
  let st = ctx.agent
  let assert Ok(root) =
    list.find(set.to_list(st.store.pinned_roots), fn(id) {
      case rt_store.t_cell_get(st, Handle(id:)) {
        SObject(kind: HostObj(_), ..) -> True
        _ -> False
      }
    })
  assert host.read_host(ctx, mk_object(Handle(id: root))) == None
}

pub fn unregistered_id_is_a_type_error_test() {
  let st = agent()
  let #(h, st) =
    rt_call.t_native_new(
      st,
      None,
      types.HostFn(9),
      "ghost",
      0,
      constructible: False,
    )
  let assert #(ThrowCompletion(e), st) =
    rt_call.t_try_call(st, mk_object(h), mk_undefined(), [])
  assert describe(st, e) == "TypeError: host function #9 is not registered"
}

pub fn with_context_runs_body_and_drains_test() {
  let #(seen, st) =
    host.with_context(agent(), brand(), fn(ctx) {
      let #(o, ctx) = host.object(ctx, [#("v", mk_int(5))])
      let ctx = host.define_global(ctx, "shared", o)
      let st = ctx.agent
      let promise = global(st, "Promise").0
      let #(p, st) = rt_helpers.call_method(st, promise, "resolve", [mk_int(1)])
      let #(setter, s2) =
        host.function(Context(..ctx, agent: st), "set", 1, fn(ctx, args, _) {
          let st = ctx.agent
          let #(_, st) =
            rt_obj.t_set_prop(
              st,
              global(st, "shared").0,
              StringKey(Named("v")),
              host.first_arg(args),
            )
          #(Ok(mk_undefined()), Context(..ctx, agent: st))
        })
      let #(_, st) = rt_helpers.call_method(s2.agent, p, "then", [setter])
      assert get(st, global(st, "shared").0, "v").0 == mk_int(5)
      #("done", Context(..s2, agent: st))
    })
  assert seen == "done"
  assert get(st, global(st, "shared").0, "v").0 == mk_int(1)
}
