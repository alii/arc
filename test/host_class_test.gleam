//// Tests for the host-class / live-State embedder API: `engine.host_class`,
//// `engine.host_fn`, and `engine.with_state`. These are the capabilities a host
//// uses to provide base classes embedder JS can `extends` and to run host-side
//// work against a live `State` without installing a global shim.

import arc/engine.{Finite, JsNumber, JsString, ModuleReturned, Returned}
import arc/host.{type State, State}
import arc/module/load_error
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type JsVal, JInt, Named, StringKey, mk_number, mk_object, mk_string,
  mk_undefined,
}
import gleam/option.{Some}

/// `recv[name]` (§7.3.2 GetV) from inside host code.
fn get(s: State(host), recv: JsVal, name: String) -> #(JsVal, State(host)) {
  let #(v, agent) = rt_obj.t_get_prop(s.agent, recv, StringKey(Named(name)))
  #(v, State(..s, agent:))
}

// -- host class bodies -------------------------------------------------------

/// Base-class `[[Construct]]`: build the instance and set `this.id`. The
/// class re-prototypes the returned object to `new_target`'s prototype
/// (§10.1.13.1 OrdinaryCreateFromConstructor), so `new Sub()` for a JS
/// `class Sub extends Service` yields a `Sub`.
fn service_ctor(_args, _this, s: State(host)) {
  let #(s, obj) = host.object(s, [#("id", mk_string("svc-1"))])
  #(s, Ok(obj))
}

/// Instance method on the prototype — reads `this.id`.
fn service_who(_args, this, s: State(host)) {
  let #(v, s) = get(s, this, "id")
  #(s, Ok(v))
}

/// Static method — a plain constant.
fn service_kind(_args, _this, s: State(host)) {
  #(s, Ok(mk_string("service")))
}

/// Static method — reads `this.name`, which on an inheriting subclass
/// constructor is the subclass's name.
fn service_named(_args, this, s: State(host)) {
  let #(v, s) = get(s, this, "name")
  #(s, Ok(v))
}

fn engine_with_service() {
  let #(eng, service) =
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

// -- host_class --------------------------------------------------------------

pub fn host_class_extends_instance_method_test() {
  let eng = engine_with_service()
  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "class Channel extends Service {} new Channel().who()")
  // `super()` ran the host ctor (set this.id), the prototype method resolved.
  assert engine.classify(value) == JsString("svc-1")
}

pub fn host_class_instanceof_test() {
  let eng = engine_with_service()
  let assert Ok(#(Returned(value:), _)) =
    engine.eval(
      eng,
      "class Channel extends Service {} String(new Channel() instanceof Service)",
    )
  assert engine.classify(value) == JsString("true")
}

pub fn host_class_static_inheritance_test() {
  let eng = engine_with_service()
  // `kind` is inherited statically (Channel.__proto__ === Service); `named`
  // reads `this.name`, which is the subclass's own name.
  let assert Ok(#(Returned(value:), _)) =
    engine.eval(
      eng,
      "class Channel extends Service {} Channel.kind() + ',' + Channel.named()",
    )
  assert engine.classify(value) == JsString("service,Channel")
}

pub fn host_class_subclass_fields_run_after_super_test() {
  let eng = engine_with_service()
  let assert Ok(#(Returned(value:), _)) =
    engine.eval(
      eng,
      "class Channel extends Service { count = 7 } const c = new Channel(); c.id + ':' + c.count",
    )
  // The host ctor's `this.id` survives the field-initializer pass.
  assert engine.classify(value) == JsString("svc-1:7")
}

pub fn host_class_not_a_global_until_placed_test() {
  // host_class does NOT install a global by itself.
  let #(eng, _service) =
    engine.host_class(engine.new(), "Service", 0, service_ctor, [], [])
  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "typeof globalThis.Service")
  assert engine.classify(value) == JsString("undefined")
}

// -- host_fn -----------------------------------------------------------------

pub fn host_fn_mints_callable_value_test() {
  let #(eng, greet) =
    engine.host_fn(engine.new(), "greet", 0, fn(_a, _t, s) {
      #(s, Ok(mk_string("hi")))
    })
  // Not a global; place it ourselves to prove it's a real callable.
  let eng = engine.define_global(eng, "greet", greet)
  let assert Ok(#(Returned(value:), _)) = engine.eval(eng, "greet()")
  assert engine.classify(value) == JsString("hi")
}

// -- with_state --------------------------------------------------------------

pub fn with_state_calls_js_function_test() {
  let assert Ok(#(_c, eng)) =
    engine.eval(engine.new(), "globalThis.double = (x) => x * 2;")
  let global = mk_object(engine.global(eng))
  let #(_eng, result) =
    engine.with_state(eng, fn(s) {
      let #(double, s) = get(s, global, "double")
      let assert #(s, Ok(out)) =
        host.call(s, double, mk_undefined(), [mk_number(JInt(21))])
      #(s, out)
    })
  assert engine.classify(result) == JsNumber(Finite(42.0))
}

// -- register_host_module ----------------------------------------------------

/// Resolver that canonicalizes "dance" to itself and leaves others as-is; the
/// loader errors for everything, proving no source is fetched for "dance".
fn dance_resolve(raw: String, _ref: String) {
  Ok(raw)
}

fn no_source_loads(_resolved: String) {
  Error(load_error.LoadForbidden)
}

/// `engine.read_export`, classified.
fn read_export(eng, ns, name: String) {
  engine.read_export(eng, ns, name) |> option.map(engine.classify)
}

pub fn host_module_named_import_test() {
  let #(eng, greet) =
    engine.host_fn(engine.new(), "greet", 0, fn(_a, _t, s) {
      #(s, Ok(mk_string("hi")))
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
  assert read_export(eng, ns, "default") == Some(JsString("hi"))
}

pub fn host_module_namespace_import_test() {
  let #(eng, greet) =
    engine.host_fn(engine.new(), "greet", 0, fn(_a, _t, s) {
      #(s, Ok(mk_string("yo")))
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
  assert read_export(eng, ns, "r") == Some(JsString("yo"))
}

pub fn host_module_class_extends_test() {
  // The real shape dance needs: a host class exported from the native module,
  // extended by user code imported from "dance".
  let #(eng, service) =
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
  assert read_export(eng, ns, "default") == Some(JsString("svc-1"))
}

pub fn with_state_threads_heap_back_test() {
  // A value allocated inside with_state must survive into the returned engine.
  let #(eng, holder) =
    engine.with_state(engine.new(), fn(s) {
      host.object(s, [#("v", mk_number(JInt(9)))])
    })
  // Read it back through a second with_state on the SAME engine.
  let #(_eng, out) =
    engine.with_state(eng, fn(s) {
      let #(v, s) = get(s, holder, "v")
      #(s, v)
    })
  assert engine.classify(out) == JsNumber(Finite(9.0))
}
