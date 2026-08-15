//// The module system on the shared runtime at the `Agent` level: linking,
//// namespaces, the realm registry, deferred namespaces and the ImportCall
//// front half.

import arc/interp/dynamic_import
import arc/interp/entry
import arc/interp/module
import arc/interp/module_host
import arc/interp/module_registry as registry
import arc/module/load_error
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/inspect as rt_inspect
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type JsVal, JInt, KNum, KStr, KUndef, Named, PromiseFulfilled,
  PromiseRejected, StringKey, SymbolKey, classify, mk_number, mk_object,
  mk_string, mk_undefined,
}
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import rt_helpers

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks()) |> entry.link
}

fn no_drain(st: Agent) -> Agent {
  st
}

fn dance_resolve(raw: String, _referrer: String) {
  Ok(raw)
}

fn no_source_loads(_resolved: String) {
  Error(load_error.LoadNotFound)
}

fn hosts() {
  dict.from_list([
    #("dance", module.HostModule("dance", [#("greet", mk_number(JInt(7)))])),
  ])
}

fn bundle(entry_source: String) -> module.ModuleBundle {
  let assert Ok(b) =
    module.compile_bundle_with_hosts(
      "entry",
      entry_source,
      dance_resolve,
      no_source_loads,
      hosts(),
    )
  b
}

fn get(st: Agent, recv: JsVal, name: String) -> #(JsVal, Agent) {
  rt_obj.t_get_prop(st, recv, StringKey(Named(name)))
}

// -- Linking -------------------------------------------------------------------

pub fn link_builds_namespaces_over_live_cells_test() {
  let b = bundle("import { greet } from 'dance'; export const r = greet;")
  let assert #(st, Ok(linked)) = module.link_for_evaluation(b, agent())
  let namespaces = module.linked_namespaces(linked, st) |> dict.from_list
  let assert Ok(dance_ns) = dict.get(namespaces, "dance")
  let assert Ok(entry_ns) = dict.get(namespaces, "entry")
  // A host module's cells hold their final values at link time.
  let assert Some(greet) = module.read_export(st, mk_object(dance_ns), "greet")
  assert classify(greet) == KNum(JInt(7))
  // A source module's `const` export is TDZ until its body runs.
  assert module.read_export(st, mk_object(entry_ns), "r") == None
  // The namespace is a real §10.4.6 object: [[Get]] reads the cell.
  let #(v, st) = get(st, mk_object(dance_ns), "greet")
  assert classify(v) == KNum(JInt(7))
  let #(tag, _) =
    rt_obj.t_get_prop(
      st,
      mk_object(dance_ns),
      SymbolKey(types.symbol_to_string_tag),
    )
  assert classify(tag) == KStr("Module")
}

pub fn missing_import_is_a_link_time_syntax_error_test() {
  let b = bundle("import { nope } from 'dance';")
  let assert #(st, Error(module.EvaluationError(err))) =
    module.link_for_evaluation(b, agent())
  assert string.starts_with(rt_inspect.format_error(st, err), "SyntaxError")
}

// -- Evaluation ------------------------------------------------------------------

pub fn evaluation_marks_the_registry_test() {
  let b = bundle("import { greet } from 'dance'; export const r = greet;")
  let assert #(st, Ok(module.EvaluatedBundle(value:, namespace: _))) =
    module.evaluate_bundle(b, agent(), no_drain)
  assert classify(value) == KUndef
  assert registry.read_module_status(st, "entry") == Some(registry.Evaluated)
  assert registry.read_module_error(st, "entry") == None
}

pub fn a_throwing_body_caches_its_error_test() {
  let b = bundle("throw 'boom'; export const r = 1;")
  let assert #(st, Error(module.EvaluationError(thrown))) =
    module.evaluate_bundle(b, agent(), no_drain)
  assert classify(thrown) == KStr("boom")
  assert registry.read_module_status(st, "entry") == None
  let assert Some(cached) = registry.read_module_error(st, "entry")
  assert classify(cached) == KStr("boom")
}

// -- Deferred namespaces ------------------------------------------------------------

fn deferred_namespace_of(spec: String) {
  let b = bundle("import { greet } from 'dance'; export const r = greet;")
  let assert #(st, Ok(linked)) = module.link_for_evaluation(b, agent())
  let #(st, res) = module.get_or_create_deferred_namespace(st, linked, spec)
  #(st, res)
}

/// A host (synthetic) module has export cells and a namespace object like any
/// source module, so `import.defer()` of one yields a Deferred Module
/// Namespace whose trigger evaluates nothing and forwards to the cells.
pub fn deferred_namespace_over_host_module_test() {
  let assert #(st, Ok(proxy)) = deferred_namespace_of("dance")
  let ns = mk_object(proxy)
  // "then" is symbol-like: never triggers, always undefined.
  let #(then_v, st) = get(st, ns, "then")
  assert classify(then_v) == KUndef
  let #(tag, st) =
    rt_obj.t_get_prop(st, ns, SymbolKey(types.symbol_to_string_tag))
  assert classify(tag) == KStr("Deferred Module")
  // A string key triggers EnsureDeferredNamespaceEvaluation, then forwards.
  let #(greet, _) = get(st, ns, "greet")
  assert classify(greet) == KNum(JInt(7))
}

pub fn deferred_namespace_of_unknown_specifier_test() {
  let assert #(_, Error(module.DeferredSpecifierNotInBundle("nope"))) =
    deferred_namespace_of("nope")
}

// -- ImportCall ---------------------------------------------------------------------

fn settled(st: Agent, promise: JsVal) -> Result(JsVal, JsVal) {
  let assert Some(h) = rt_async.as_promise(st, promise)
  case rt_async.promise_data(st, h) {
    #(_, PromiseFulfilled(v), _) -> Ok(v)
    #(_, PromiseRejected(e), _) -> Error(e)
    #(_, _, _) -> panic as "import promise still pending"
  }
}

pub fn import_without_a_hook_rejects_with_type_error_test() {
  let #(p, st) =
    dynamic_import.import_call(agent(), mk_string("./x.js"), mk_undefined())
  let st = rt_async.drain(st)
  let assert Error(e) = settled(st, p)
  assert string.starts_with(rt_inspect.format_error(st, e), "TypeError")
}

pub fn import_with_bad_options_rejects_synchronously_test() {
  let #(p, st) =
    dynamic_import.import_call(agent(), mk_string("./x.js"), mk_number(JInt(1)))
  let assert Error(e) = settled(st, p)
  assert string.contains(rt_inspect.format_error(st, e), "must be an object")
}

pub fn import_source_rejects_with_syntax_error_test() {
  let #(p, st) = dynamic_import.source_import_call(agent(), mk_string("./x.js"))
  let st = rt_async.drain(st)
  let assert Error(e) = settled(st, p)
  assert string.starts_with(rt_inspect.format_error(st, e), "SyntaxError")
}

/// The whole eager pipeline through the host hook: resolve, load, compile,
/// link against the registry, evaluate, resolve the promise with the
/// namespace; and a second import of the same specifier is served from the
/// registry without loading again.
pub fn import_through_the_hook_yields_the_registered_namespace_test() {
  let load = fn(resolved) {
    case resolved {
      "/lib.js" -> Ok("export var v; export function f() {}")
      _ -> Error(load_error.LoadNotFound)
    }
  }
  let resolve = fn(raw: String, _referrer: String) {
    case raw {
      "./lib.js" -> Ok("/lib.js")
      _ -> Error(load_error.ResolveNotFound)
    }
  }
  let st = module_host.install_import_hook(agent(), "/main.js", resolve, load)
  let #(p, st) =
    dynamic_import.import_call(st, mk_string("./lib.js"), mk_undefined())
  let st = rt_async.drain(st)
  let assert Ok(ns) = settled(st, p)
  let assert Some(registered) = registry.read_namespace(st, "/lib.js")
  assert classify(ns) == types.KHandle(registered)
  assert registry.read_module_status(st, "/lib.js") == Some(registry.Evaluated)
  // `var` exports are hoisted: initialized (undefined), not TDZ. The exported
  // function declaration was instantiated at link time.
  let assert Some(v) = module.read_export(st, ns, "v")
  assert classify(v) == KUndef
  let assert Some(f) = module.read_export(st, ns, "f")
  let assert types.KHandle(_) = classify(f)
  // Second import: same namespace, loader not consulted (it would fail).
  let st =
    module_host.install_import_hook(st, "/main.js", resolve, fn(_) {
      Error(load_error.LoadNotFound)
    })
  let #(p2, st) =
    dynamic_import.import_call(st, mk_string("./lib.js"), mk_undefined())
  let st = rt_async.drain(st)
  let assert Ok(ns2) = settled(st, p2)
  assert ns2 == ns
}

pub fn import_of_an_unresolvable_specifier_rejects_test() {
  let #(resolve, load) = module_host.no_imports()
  let st = module_host.install_import_hook(agent(), "/main.js", resolve, load)
  let #(p, st) =
    dynamic_import.import_call(st, mk_string("./lib.js"), mk_undefined())
  let st = rt_async.drain(st)
  let assert Error(e) = settled(st, p)
  let msg = rt_inspect.format_error(st, e)
  assert string.starts_with(msg, "TypeError")
  assert string.contains(msg, "Cannot resolve module")
}

pub fn hook_args_round_trip_test() {
  let args =
    dynamic_import.encode_hook_args(
      "./a.js",
      Some("/m.js"),
      dynamic_import.DeferPhase(mk_number(JInt(1)), mk_number(JInt(2))),
    )
  let assert Ok(dynamic_import.HookCall(specifier:, referrer:, phase:)) =
    dynamic_import.parse_hook_args(args)
  assert specifier == "./a.js"
  assert referrer == Some("/m.js")
  let assert dynamic_import.DeferPhase(resolve_fn:, reject_fn:) = phase
  assert classify(resolve_fn) == KNum(JInt(1))
  assert classify(reject_fn) == KNum(JInt(2))
  let eager =
    dynamic_import.encode_hook_args("./a.js", None, dynamic_import.EagerPhase)
  assert list.length(eager) == 1
}
