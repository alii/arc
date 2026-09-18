import arc/bytecode/key.{Named}
import arc/engine.{ModuleReturned, Returned}
import arc/host.{type Context, Context}
import arc/module/loader
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type JsVal, JFloat, KNum, KStr, StringKey, mk_int, mk_object, mk_string,
  mk_undefined,
}
import gleam/option.{Some}
import rt_helpers

fn get(
  ctx: Context(host),
  recv: JsVal,
  name: String,
) -> #(JsVal, Context(host)) {
  let #(v, st) = rt_obj.t_get_prop(ctx.agent, recv, StringKey(Named(name)))
  #(v, Context(..ctx, agent: st))
}

fn service_ctor(ctx: Context(host), _args, _this) {
  let #(obj, ctx) = host.object(ctx, [#("id", mk_string("svc-1"))])
  #(Ok(obj), ctx)
}

fn service_who(ctx: Context(host), _args, this) {
  let #(v, ctx) = get(ctx, this, "id")
  #(Ok(v), ctx)
}

fn service_kind(ctx: Context(host), _args, _this) {
  #(Ok(mk_string("service")), ctx)
}

fn service_named(ctx: Context(host), _args, this) {
  let #(v, ctx) = get(ctx, this, "name")
  #(Ok(v), ctx)
}

fn engine_with_service() {
  let #(service, eng) =
    engine.host_class(
      engine.new(),
      "Service",
      0,
      service_ctor,
      [#("who", 0, service_who)],
      [#("kind", 0, service_kind), #("named", 0, service_named)],
    )
  engine.define_global(eng, "Service", service)
}

pub fn host_class_extends_instance_method_test() {
  let eng = engine_with_service()
  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "class Channel extends Service {} new Channel().who()")
  assert rt_helpers.classify(value) == KStr("svc-1")
}

pub fn host_class_instanceof_test() {
  let eng = engine_with_service()
  let assert Ok(#(Returned(value:), _)) =
    engine.eval(
      eng,
      "class Channel extends Service {} String(new Channel() instanceof Service)",
    )
  assert rt_helpers.classify(value) == KStr("true")
}

pub fn host_class_static_inheritance_test() {
  let eng = engine_with_service()
  let assert Ok(#(Returned(value:), _)) =
    engine.eval(
      eng,
      "class Channel extends Service {} Channel.kind() + ',' + Channel.named()",
    )
  assert rt_helpers.classify(value) == KStr("service,Channel")
}

pub fn host_class_subclass_fields_run_after_super_test() {
  let eng = engine_with_service()
  let assert Ok(#(Returned(value:), _)) =
    engine.eval(
      eng,
      "class Channel extends Service { count = 7 } const c = new Channel(); c.id + ':' + c.count",
    )
  assert rt_helpers.classify(value) == KStr("svc-1:7")
}

pub fn host_class_not_a_global_until_placed_test() {
  let #(_service, eng) =
    engine.host_class(engine.new(), "Service", 0, service_ctor, [], [])
  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "typeof globalThis.Service")
  assert rt_helpers.classify(value) == KStr("undefined")
}

pub fn host_fn_mints_callable_value_test() {
  let #(greet, eng) =
    engine.host_fn(engine.new(), "greet", 0, fn(ctx, _a, _t) {
      #(Ok(mk_string("hi")), ctx)
    })
  let eng = engine.define_global(eng, "greet", greet)
  let assert Ok(#(Returned(value:), _)) = engine.eval(eng, "greet()")
  assert rt_helpers.classify(value) == KStr("hi")
}

pub fn with_context_calls_js_function_test() {
  let assert Ok(#(_c, eng)) =
    engine.eval(engine.new(), "globalThis.double = (x) => x * 2;")
  let global = mk_object(engine.global(eng))
  let #(result, _eng) =
    engine.with_context(eng, fn(ctx) {
      let #(double, ctx) = get(ctx, global, "double")
      let assert #(Ok(out), ctx) =
        host.call(ctx, double, mk_undefined(), [mk_int(21)])
      #(out, ctx)
    })
  assert rt_helpers.classify(result) == KNum(JFloat(42.0))
}

fn dance_resolve(raw: String, _ref: String) {
  Ok(raw)
}

fn no_source_loads(_resolved: String) {
  Error(loader.LoadForbidden)
}

fn read_export(eng, ns, name: String) {
  engine.read_export(eng, ns, name) |> option.map(rt_helpers.classify)
}

pub fn host_module_named_import_test() {
  let #(greet, eng) =
    engine.host_fn(engine.new(), "greet", 0, fn(ctx, _a, _t) {
      #(Ok(mk_string("hi")), ctx)
    })
  let eng = engine.register_host_module(eng, "dance", [#("greet", greet)])
  let assert Ok(#(evaluated, eng)) =
    engine.eval_module(
      eng,
      "entry",
      "import { greet } from \"dance\"; export default greet();",
      dance_resolve,
      no_source_loads,
    )
  let assert ModuleReturned(namespace: ns, ..) = evaluated
  assert read_export(eng, ns, "default") == Some(KStr("hi"))
}

pub fn host_module_namespace_import_test() {
  let #(greet, eng) =
    engine.host_fn(engine.new(), "greet", 0, fn(ctx, _a, _t) {
      #(Ok(mk_string("yo")), ctx)
    })
  let eng = engine.register_host_module(eng, "dance", [#("greet", greet)])
  let assert Ok(#(evaluated, eng)) =
    engine.eval_module(
      eng,
      "entry",
      "import * as Dance from \"dance\"; export const r = Dance.greet();",
      dance_resolve,
      no_source_loads,
    )
  let assert ModuleReturned(namespace: ns, ..) = evaluated
  assert read_export(eng, ns, "r") == Some(KStr("yo"))
}

pub fn host_module_class_extends_test() {
  let #(service, eng) =
    engine.host_class(
      engine.new(),
      "Service",
      0,
      service_ctor,
      [#("who", 0, service_who)],
      [],
    )
  let eng = engine.register_host_module(eng, "dance", [#("Service", service)])
  let assert Ok(#(evaluated, eng)) =
    engine.eval_module(
      eng,
      "entry",
      "import { Service } from \"dance\"; class Channel extends Service {} export default new Channel().who();",
      dance_resolve,
      no_source_loads,
    )
  let assert ModuleReturned(namespace: ns, ..) = evaluated
  assert read_export(eng, ns, "default") == Some(KStr("svc-1"))
}

pub fn with_context_threads_heap_back_test() {
  let #(holder, eng) =
    engine.with_context(engine.new(), fn(ctx) {
      host.object(ctx, [#("v", mk_int(9))])
    })
  let #(out, _eng) =
    engine.with_context(eng, fn(ctx) {
      let #(v, ctx) = get(ctx, holder, "v")
      #(v, ctx)
    })
  assert rt_helpers.classify(out) == KNum(JFloat(9.0))
}
