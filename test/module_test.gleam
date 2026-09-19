import arc/bytecode/key.{Named}
import arc/interp/entry
import arc/module
import arc/module/dynamic_import
import arc/module/import_hook
import arc/module/loader
import arc/module/registry
import arc/parser/ast
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/inspect as rt_inspect
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type JsVal, JInt, KNum, KStr, KUndef, PromiseFulfilled,
  PromiseRejected, StringKey, SymbolKey, classify, mk_int, mk_object, mk_string,
  mk_undefined,
}
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import rt_helpers

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks()) |> entry.link
}

fn dance_resolve(raw: String, _referrer: String) {
  Ok(raw)
}

fn no_source_loads(_resolved: String, _attributes) {
  Error(loader.LoadNotFound)
}

fn hosts() {
  dict.from_list([
    #("dance", module.HostModule([#("greet", mk_int(7))])),
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
  rt_obj.get_prop(st, recv, StringKey(Named(name)))
}

pub fn link_builds_namespaces_over_live_cells_test() {
  let b = bundle("import { greet } from 'dance'; export const r = greet;")
  let assert #(Ok(linked), st) = module.link_for_evaluation(agent(), b)
  let namespaces = module.linked_namespaces(st, linked) |> dict.from_list
  let assert Ok(dance_ns) = dict.get(namespaces, "dance")
  let assert Ok(entry_ns) = dict.get(namespaces, "entry")
  let assert Some(greet) = module.read_export(st, mk_object(dance_ns), "greet")
  assert classify(greet) == KNum(JInt(7))
  assert module.read_export(st, mk_object(entry_ns), "r") == None
  let #(v, st) = get(st, mk_object(dance_ns), "greet")
  assert classify(v) == KNum(JInt(7))
  let #(tag, _) =
    rt_obj.get_prop(
      st,
      mk_object(dance_ns),
      SymbolKey(types.symbol_to_string_tag),
    )
  assert classify(tag) == KStr("Module")
}

pub fn missing_import_is_a_link_time_syntax_error_test() {
  let b = bundle("import { nope } from 'dance';")
  let assert #(Error(err), st) = module.link_for_evaluation(agent(), b)
  assert string.starts_with(rt_inspect.format_error(st, err), "SyntaxError")
}

pub fn evaluation_marks_the_registry_test() {
  let b = bundle("import { greet } from 'dance'; export const r = greet;")
  let assert #(Ok(module.EvaluatedBundle(value:, namespace: _)), st) =
    module.evaluate_bundle(agent(), b, rt_async.no_drain)
  assert classify(value) == KUndef
  assert registry.read_module_status(st, "entry") == Some(registry.Evaluated)
  assert registry.read_module_error(st, "entry") == None
}

pub fn a_throwing_body_caches_its_error_test() {
  let b = bundle("throw 'boom'; export const r = 1;")
  let assert #(Error(module.EvaluationError(thrown)), st) =
    module.evaluate_bundle(agent(), b, rt_async.no_drain)
  assert classify(thrown) == KStr("boom")
  assert registry.read_module_status(st, "entry") == None
  let assert Some(cached) = registry.read_module_error(st, "entry")
  assert classify(cached) == KStr("boom")
}

fn deferred_namespace_of(spec: String) {
  let b = bundle("import { greet } from 'dance'; export const r = greet;")
  let assert #(Ok(linked), st) = module.link_for_evaluation(agent(), b)
  let #(res, st) = module.get_or_create_deferred_namespace(st, linked, spec)
  #(res, st)
}

pub fn deferred_namespace_over_host_module_test() {
  let assert #(Ok(proxy), st) = deferred_namespace_of("dance")
  let ns = mk_object(proxy)
  let #(then_v, st) = get(st, ns, "then")
  assert classify(then_v) == KUndef
  let #(tag, st) =
    rt_obj.get_prop(st, ns, SymbolKey(types.symbol_to_string_tag))
  assert classify(tag) == KStr("Deferred Module")
  let #(greet, _) = get(st, ns, "greet")
  assert classify(greet) == KNum(JInt(7))
}

pub fn deferred_namespace_of_unknown_specifier_test() {
  let assert #(Error(module.DeferredSpecifierNotInBundle("nope")), _) =
    deferred_namespace_of("nope")
}

fn settled(st: Agent, promise: JsVal) -> Result(JsVal, JsVal) {
  let assert Some(h) = rt_async.as_promise(st, promise)
  case rt_async.promise_data(st, h).state {
    PromiseFulfilled(v) -> Ok(v)
    PromiseRejected(e) -> Error(e)
    _ -> panic as "import promise still pending"
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
    dynamic_import.import_call(agent(), mk_string("./x.js"), mk_int(1))
  let assert Error(e) = settled(st, p)
  assert string.contains(rt_inspect.format_error(st, e), "must be an object")
}

pub fn import_source_rejects_with_syntax_error_test() {
  let #(p, st) = dynamic_import.source_import_call(agent(), mk_string("./x.js"))
  let st = rt_async.drain(st)
  let assert Error(e) = settled(st, p)
  assert string.starts_with(rt_inspect.format_error(st, e), "SyntaxError")
}

pub fn import_through_the_hook_yields_the_registered_namespace_test() {
  let load = fn(resolved, _attributes) {
    case resolved {
      "/lib.js" -> Ok(loader.SourceText("export var v; export function f() {}"))
      _ -> Error(loader.LoadNotFound)
    }
  }
  let resolve = fn(raw: String, _referrer: String) {
    case raw {
      "./lib.js" -> Ok("/lib.js")
      _ -> Error(loader.ResolveNotFound)
    }
  }
  let st = import_hook.install(agent(), "/main.js", resolve, load)
  let #(p, st) =
    dynamic_import.import_call(st, mk_string("./lib.js"), mk_undefined())
  let st = rt_async.drain(st)
  let assert Ok(ns) = settled(st, p)
  let assert Some(registered) = registry.read_namespace(st, "/lib.js")
  assert classify(ns) == types.KHandle(registered)
  assert registry.read_module_status(st, "/lib.js") == Some(registry.Evaluated)
  let assert Some(v) = module.read_export(st, ns, "v")
  assert classify(v) == KUndef
  let assert Some(f) = module.read_export(st, ns, "f")
  let assert types.KHandle(_) = classify(f)
  let st =
    import_hook.install(st, "/main.js", resolve, fn(_, _) {
      Error(loader.LoadNotFound)
    })
  let #(p2, st) =
    dynamic_import.import_call(st, mk_string("./lib.js"), mk_undefined())
  let st = rt_async.drain(st)
  let assert Ok(ns2) = settled(st, p2)
  assert ns2 == ns
}

pub fn import_of_an_unresolvable_specifier_rejects_test() {
  let #(resolve, load) = loader.no_imports()
  let st = import_hook.install(agent(), "/main.js", resolve, load)
  let #(p, st) =
    dynamic_import.import_call(st, mk_string("./lib.js"), mk_undefined())
  let st = rt_async.drain(st)
  let assert Error(e) = settled(st, p)
  let msg = rt_inspect.format_error(st, e)
  assert string.starts_with(msg, "TypeError")
  assert string.contains(msg, "Cannot resolve module")
}

pub fn hook_args_round_trip_test() {
  let json = [ast.ImportAttribute(key: "type", value: "json")]
  let args =
    dynamic_import.encode_hook_args(dynamic_import.HookCall(
      specifier: "./a.js",
      referrer: Some("/m.js"),
      attributes: json,
      phase: dynamic_import.DeferPhase(mk_int(1), mk_int(2)),
    ))
  let assert Ok(dynamic_import.HookCall(
    specifier:,
    referrer:,
    attributes:,
    phase:,
  )) = dynamic_import.parse_hook_args(args)
  assert specifier == "./a.js"
  assert referrer == Some("/m.js")
  assert attributes == json
  let assert dynamic_import.DeferPhase(fulfill:, reject:) = phase
  assert classify(fulfill) == KNum(JInt(1))
  assert classify(reject) == KNum(JInt(2))
  let eager =
    dynamic_import.encode_hook_args(dynamic_import.HookCall(
      specifier: "./a.js",
      referrer: None,
      attributes: [],
      phase: dynamic_import.EagerPhase,
    ))
  let assert Ok(dynamic_import.HookCall(referrer: None, attributes: [], ..)) =
    dynamic_import.parse_hook_args(eager)
}
