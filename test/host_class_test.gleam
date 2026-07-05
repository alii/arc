//// Tests for the host-class / live-State embedder API: `engine.define_class`,
//// `engine.host_fn`, and `engine.with_state`. These are the capabilities a host
//// uses to provide base classes embedder JS can `extends` and to run host-side
//// work against a live `State` without installing a global shim.

import arc/engine.{ModuleReturned, Returned}
import arc/module/load_error
import arc/vm/builtins/common
import arc/vm/key.{Named}
import arc/vm/ops/object
import arc/vm/state
import arc/vm/value.{Finite, JsNumber, JsObject, JsString, JsUndefined}
import gleam/option.{Some}

// -- host class bodies -------------------------------------------------------

/// Base-class `[[Construct]]`: allocate `this` with the leaf subclass's
/// prototype (via `new_target`), set `this.id`, return it.
fn service_ctor(_args, _this, s: state.State(host)) {
  // §10.1.13.1 OrdinaryCreateFromConstructor(NewTarget, %Object.prototype%):
  // step 2's `? Get(newTarget, "prototype")` is a real [[Get]], so it can throw.
  use proto, s <- object.proto_from_new_target(
    s,
    s.new_target,
    s.builtins.object.prototype,
  )
  let #(h, obj) = common.alloc_pojo(s.heap, proto, [])
  let s = state.State(..s, heap: h)
  case object.set_value(s, obj, Named("id"), JsString("svc-1"), JsObject(obj)) {
    Ok(#(s, _)) -> #(s, Ok(JsObject(obj)))
    Error(#(thrown, s)) -> #(s, Error(thrown))
  }
}

/// Instance method on the prototype — reads `this.id`.
fn service_who(_args, this, s: state.State(host)) {
  case this {
    JsObject(ref) ->
      case object.get_value(s, ref, Named("id"), this) {
        Ok(#(v, s)) -> #(s, Ok(v))
        Error(#(t, s)) -> #(s, Error(t))
      }
    _ -> #(s, Ok(JsUndefined))
  }
}

/// Static method — a plain constant.
fn service_kind(_args, _this, s: state.State(host)) {
  #(s, Ok(JsString("service")))
}

/// Static method — reads `this.name`, which on an inheriting subclass
/// constructor is the subclass's name.
fn service_named(_args, this, s: state.State(host)) {
  case this {
    JsObject(ref) ->
      case object.get_value(s, ref, Named("name"), this) {
        Ok(#(v, s)) -> #(s, Ok(v))
        Error(#(t, s)) -> #(s, Error(t))
      }
    _ -> #(s, Ok(JsUndefined))
  }
}

fn engine_with_service() {
  let #(eng, service) =
    engine.define_class(
      engine.new(),
      "Service",
      0,
      service_ctor,
      [#("who", 0, service_who)],
      [#("kind", 0, service_kind), #("named", 0, service_named)],
    )
  engine.define_global(eng, "Service", service)
}

// -- define_class ------------------------------------------------------------

pub fn host_class_extends_instance_method_test() {
  let eng = engine_with_service()
  let assert Ok(#(Returned(value: JsString(out)), _)) =
    engine.eval(eng, "class Channel extends Service {} new Channel().who()")
  // `super()` ran the host ctor (set this.id), the prototype method resolved.
  assert out == "svc-1"
}

pub fn host_class_instanceof_test() {
  let eng = engine_with_service()
  let assert Ok(#(Returned(value: JsString(out)), _)) =
    engine.eval(
      eng,
      "class Channel extends Service {} String(new Channel() instanceof Service)",
    )
  assert out == "true"
}

pub fn host_class_static_inheritance_test() {
  let eng = engine_with_service()
  // `kind` is inherited statically (Channel.__proto__ === Service); `named`
  // reads `this.name`, which is the subclass's own name.
  let assert Ok(#(Returned(value: JsString(out)), _)) =
    engine.eval(
      eng,
      "class Channel extends Service {} Channel.kind() + ',' + Channel.named()",
    )
  assert out == "service,Channel"
}

pub fn host_class_subclass_fields_run_after_super_test() {
  let eng = engine_with_service()
  let assert Ok(#(Returned(value: JsString(out)), _)) =
    engine.eval(
      eng,
      "class Channel extends Service { count = 7 } const c = new Channel(); c.id + ':' + c.count",
    )
  // The host ctor's `this.id` survives the field-initializer pass.
  assert out == "svc-1:7"
}

pub fn host_class_not_a_global_until_placed_test() {
  // define_class does NOT install a global by itself.
  let #(eng, _service) =
    engine.define_class(engine.new(), "Service", 0, service_ctor, [], [])
  let assert Ok(#(Returned(value: JsString(out)), _)) =
    engine.eval(eng, "typeof globalThis.Service")
  assert out == "undefined"
}

// -- host_fn -----------------------------------------------------------------

pub fn host_fn_mints_callable_value_test() {
  let #(eng, greet) =
    engine.host_fn(engine.new(), "greet", 0, fn(_a, _t, s) {
      #(s, Ok(JsString("hi")))
    })
  // Not a global; place it ourselves to prove it's a real callable.
  let eng = engine.define_global(eng, "greet", greet)
  let assert Ok(#(Returned(value: JsString(out)), _)) =
    engine.eval(eng, "greet()")
  assert out == "hi"
}

// -- with_state --------------------------------------------------------------

pub fn with_state_calls_js_function_test() {
  let assert Ok(#(_c, eng)) =
    engine.eval(engine.new(), "globalThis.double = (x) => x * 2;")
  let global = engine.global(eng)
  let #(_eng, result) =
    engine.with_state(eng, fn(s) {
      let assert Ok(#(double, s)) =
        object.get_value(s, global, Named("double"), JsObject(global))
      let assert Ok(#(out, s)) =
        state.call(s, double, JsUndefined, [JsNumber(Finite(21.0))])
      #(s, out)
    })
  assert result == JsNumber(Finite(42.0))
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

pub fn host_module_named_import_test() {
  let #(eng, greet) =
    engine.host_fn(engine.new(), "greet", 0, fn(_a, _t, s) {
      #(s, Ok(JsString("hi")))
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
  assert engine.read_export(eng, ns, "default") == Some(JsString("hi"))
}

pub fn host_module_namespace_import_test() {
  let #(eng, greet) =
    engine.host_fn(engine.new(), "greet", 0, fn(_a, _t, s) {
      #(s, Ok(JsString("yo")))
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
  assert engine.read_export(eng, ns, "r") == Some(JsString("yo"))
}

pub fn host_module_class_extends_test() {
  // The real shape dance needs: a host class exported from the native module,
  // extended by user code imported from "dance".
  let #(eng, service) =
    engine.define_class(
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
  assert engine.read_export(eng, ns, "default") == Some(JsString("svc-1"))
}

pub fn with_state_threads_heap_back_test() {
  // A value allocated inside with_state must survive into the returned engine.
  let #(eng, holder) =
    engine.with_state(engine.new(), fn(s) {
      let #(h, ref) =
        common.alloc_pojo(s.heap, s.builtins.object.prototype, [
          #("v", value.builtin_property(JsNumber(Finite(9.0)))),
        ])
      #(state.State(..s, heap: h), JsObject(ref))
    })
  // Read it back through a second with_state on the SAME engine.
  let #(_eng, out) =
    engine.with_state(eng, fn(s) {
      let assert JsObject(ref) = holder
      let assert Ok(#(v, s)) = object.get_value(s, ref, Named("v"), holder)
      #(s, v)
    })
  assert out == JsNumber(Finite(9.0))
}
