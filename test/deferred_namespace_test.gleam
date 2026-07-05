import arc/engine
import arc/module
import arc/module/load_error
import arc/vm/state
import arc/vm/value.{JsString}
import gleam/dict

fn dance_resolve(raw: String, _ref: String) {
  Ok(raw)
}

fn no_source_loads(_resolved: String) {
  Error(load_error.LoadNotFound)
}

/// Link a bundle whose only dependency is an embedder host (synthetic) module,
/// then ask it for `spec`'s Deferred Module Namespace.
fn deferred_namespace_of(
  spec: String,
) -> Result(value.Ref, module.DeferredNamespaceError) {
  let #(eng, greet) =
    engine.host_fn(engine.new(), "greet", 0, fn(_a, _t, s) {
      #(s, Ok(JsString("hi")))
    })
  let hosts =
    dict.from_list([
      #("dance", module.HostModule("dance", [#("greet", greet)])),
    ])
  let assert Ok(bundle) =
    module.compile_bundle_with_hosts(
      "entry",
      "import { greet } from 'dance'; export const r = greet();",
      dance_resolve,
      no_source_loads,
      hosts,
    )
  let #(_eng, out) =
    engine.with_state(eng, fn(s) {
      let assert #(h, Ok(linked)) =
        module.link_for_evaluation(bundle, s.heap, s.builtins)
      let #(h, deferred) =
        module.get_or_create_deferred_namespace(h, s.builtins, linked, spec)
      #(state.State(..s, heap: h), deferred)
    })
  out
}

/// A host (synthetic) module has export cells and a namespace object like any
/// source module, so `import.defer()` of one yields a Deferred Module
/// Namespace — never a "produced no deferred namespace" TypeError.
pub fn deferred_namespace_over_host_module_test() {
  let assert Ok(_proxy_ref) = deferred_namespace_of("dance")
}

/// A specifier the bundle does not contain is reported as such — distinct from
/// "the box we allocated is not a namespace".
pub fn deferred_namespace_of_unknown_specifier_test() {
  assert deferred_namespace_of("nope")
    == Error(module.DeferredSpecifierNotInBundle("nope"))
}
