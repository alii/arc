//// Module bodies running for real on the bytecode interpreter: live
//// bindings, cyclic function imports, top-level await through the shared
//// microtask drain, thrown bodies, and dynamic `import()` from a script
//// end to end through the host hook and the registry.

import arc/compiler
import arc/interp/dynamic_import
import arc/interp/entry
import arc/module
import arc/module/load_error
import arc/module/registry
import arc/module_host
import arc/parser
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion} as _
import arc/rt/gc as rt_gc
import arc/rt/inspect as rt_inspect
import arc/rt/types.{
  type Agent, type JsVal, Agent, JInt, JsStore, KNum, KStr, PromiseFulfilled,
  PromisePending, PromiseRejected, classify, mk_object,
}
import gleam/dict
import gleam/list
import gleam/option.{Some}
import gleam/set
import gleam/string
import rt_helpers

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks()) |> entry.link
}

/// An agent that collects every 64 allocations, so a module body of any size
/// crosses the root-activation safepoint many times while it runs.
fn small_gc_agent() -> Agent {
  let st = agent() |> rt_gc.t_collect([])
  Agent(..st, store: JsStore(..st.store, gc_threshold: 64))
}

/// A module source whose body allocates well past the small threshold
/// through bytecode calls, then exports `tag`.
fn churning_module(tag: String) -> String {
  "function churn() { let a = []; for (let i = 0; i < 300; i++) a.push({ i }); return a.length }
   export const v = '"
  <> tag
  <> "' + churn() + churn();"
}

fn no_drain(st: Agent) -> Agent {
  st
}

/// A loader over an in-memory file table; specifiers resolve to themselves.
fn files(
  table: List(#(String, String)),
) -> #(module_host.ResolveFn, module_host.LoadFn) {
  let sources = dict.from_list(table)
  #(fn(raw, _referrer) { Ok(raw) }, fn(resolved) {
    dict.get(sources, resolved) |> result_or(load_error.LoadNotFound)
  })
}

fn result_or(r: Result(a, Nil), e: e) -> Result(a, e) {
  case r {
    Ok(v) -> Ok(v)
    Error(Nil) -> Error(e)
  }
}

fn evaluate(
  table: List(#(String, String)),
  finish: module.Finish,
) -> #(Agent, Result(module.EvaluatedBundle, module.ModuleError)) {
  let assert [#(entry_spec, entry_source), ..] = table
  let #(resolve, load) = files(table)
  let assert Ok(bundle) =
    module.compile_bundle(entry_spec, entry_source, resolve, load)
  module.evaluate_bundle(bundle, agent(), finish)
}

fn export(st: Agent, evaluated: module.EvaluatedBundle, name: String) -> JsVal {
  let assert Some(v) =
    module.read_export(st, mk_object(evaluated.namespace), name)
    as { "export " <> name <> " missing or uninitialized" }
  v
}

pub fn imported_binding_is_read_through_the_live_cell_test() {
  let assert #(st, Ok(evaluated)) =
    evaluate(
      [
        #(
          "/main.js",
          "import { x, bump } from '/dep.js'; bump(); export const y = x + 1;",
        ),
        #("/dep.js", "export let x = 40; export function bump() { x = x + 1 }"),
      ],
      rt_async.drain,
    )
  assert classify(export(st, evaluated, "y")) == KNum(JInt(42))
}

pub fn cyclic_function_imports_are_callable_test() {
  let assert #(st, Ok(evaluated)) =
    evaluate(
      [
        #(
          "/a.js",
          "import { b } from '/b.js'; export function a() { return 'a' }; export const r = b();",
        ),
        #(
          "/b.js",
          "import { a } from '/a.js'; export function b() { return a() + 'b' }",
        ),
      ],
      rt_async.drain,
    )
  assert classify(export(st, evaluated, "r")) == KStr("ab")
}

pub fn namespace_import_sees_the_module_object_test() {
  let assert #(st, Ok(evaluated)) =
    evaluate(
      [
        #(
          "/main.js",
          "import * as ns from '/dep.js'; export const keys = Object.keys(ns).join(); export const tag = Object.prototype.toString.call(ns);",
        ),
        #("/dep.js", "export const b = 2; export const a = 1;"),
      ],
      rt_async.drain,
    )
  assert classify(export(st, evaluated, "keys")) == KStr("a,b")
  assert classify(export(st, evaluated, "tag")) == KStr("[object Module]")
}

pub fn a_thrown_body_is_an_evaluation_error_test() {
  let assert #(st, Error(module.EvaluationError(thrown))) =
    evaluate([#("/main.js", "throw new TypeError('nope')")], rt_async.drain)
  assert rt_inspect.format_error(st, thrown)
    |> string.starts_with("TypeError: nope")
  let assert Some(_) = registry.read_module_error(st, "/main.js")
}

pub fn top_level_await_settles_through_the_drain_test() {
  let assert #(st, Ok(evaluated)) =
    evaluate(
      [
        #(
          "/main.js",
          "import { slow } from '/dep.js'; export const v = (await slow()) + (await 2);",
        ),
        #("/dep.js", "export async function slow() { await null; return 40 }"),
      ],
      rt_async.drain,
    )
  assert classify(export(st, evaluated, "v")) == KNum(JInt(42))
  assert registry.read_module_status(st, "/main.js") == Some(registry.Evaluated)
}

pub fn a_rejected_top_level_await_is_an_evaluation_error_test() {
  let assert #(st, Error(module.EvaluationError(thrown))) =
    evaluate(
      [#("/main.js", "await Promise.reject(new RangeError('late'))")],
      rt_async.drain,
    )
  assert rt_inspect.format_error(st, thrown)
    |> string.starts_with("RangeError: late")
}

/// With a non-draining driver (the dynamic-import path) a body parked on
/// top-level await is pending, not failed: the [[TopLevelCapability]]
/// promise comes back and settles once the host drains.
pub fn top_level_await_without_a_drain_is_pending_test() {
  let #(resolve, load) = files([])
  let assert Ok(bundle) =
    module.compile_bundle(
      "/main.js",
      "export const v = await 7;",
      resolve,
      load,
    )
  let assert #(st, Ok(linked)) = module.link_for_evaluation(bundle, agent())
  let assert #(st, _, Error(module.EvaluationPending(promise))) =
    module.evaluate_linked_tracking(linked, st, no_drain, set.new())
  let assert #(_, PromisePending(_), _) = rt_async.promise_data(st, promise)
  // Still mid-body: importers must not re-run it.
  assert registry.read_module_status(st, "/main.js")
    == Some(registry.Evaluating)
  let st = rt_async.drain(st)
  let assert #(_, PromiseFulfilled(_), _) = rt_async.promise_data(st, promise)
  let ns = mk_object(module.entry_namespace_of(linked, st))
  let assert Some(v) = module.read_export(st, ns, "v")
  assert classify(v) == KNum(JInt(7))
}

pub fn a_never_settling_await_is_reported_test() {
  let assert #(st, Error(module.EvaluationError(thrown))) =
    evaluate([#("/main.js", "await new Promise(() => {})")], rt_async.drain)
  assert rt_inspect.format_error(st, thrown)
    |> string.contains("top-level await promise never settled")
}

// -- Dynamic import from a script ------------------------------------------------

fn run_script(st: Agent, source: String) -> Agent {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
  let assert Ok(template) = compiler.compile(body, sb)
  let assert #(NormalCompletion(_), st) = entry.run_script(st, template)
  rt_async.drain(st)
}

fn global_string(st: Agent, name: String) -> String {
  let #(v, st) = rt_helpers.global(st, name)
  case classify(v) {
    KStr(s) -> s
    _ -> panic as { name <> " = " <> rt_inspect.inspect(st, v) }
  }
}

pub fn dynamic_import_from_a_script_test() {
  let #(resolve, load) =
    files([
      #(
        "/lib.js",
        "import { n } from '/dep.js'; export const v = 'lib' + n; export default 1;",
      ),
      #("/dep.js", "export const n = 2;"),
    ])
  let st = module_host.install_import_hook(agent(), "/main.js", resolve, load)
  let st =
    run_script(
      st,
      "var out = 'unset';
       import('/lib.js').then(ns => { out = ns.v + ':' + ns.default + ':' + Object.keys(ns).join() })
         .catch(e => { out = 'rejected ' + e })",
    )
  assert global_string(st, "out") == "lib2:1:default,v"
  // The registry served the record: a second import is the same namespace.
  let st =
    run_script(
      st,
      "var same = 'unset';
       Promise.all([import('/lib.js'), import('/lib.js')]).then(([a, b]) => { same = String(a === b) })",
    )
  assert global_string(st, "same") == "true"
}

pub fn dynamic_import_of_a_throwing_module_rejects_every_time_test() {
  let #(resolve, load) = files([#("/bad.js", "throw new Error('boom')")])
  let st = module_host.install_import_hook(agent(), "/main.js", resolve, load)
  let st =
    run_script(
      st,
      "var first = 'unset', second = 'unset';
       import('/bad.js').catch(e => { first = e.message })
         .then(() => import('/bad.js')).catch(e => { second = e.message })",
    )
  assert global_string(st, "first") == "boom"
  assert global_string(st, "second") == "boom"
}

pub fn dynamic_import_of_a_top_level_await_module_test() {
  let #(resolve, load) =
    files([#("/tla.js", "export const v = await Promise.resolve('waited');")])
  let st = module_host.install_import_hook(agent(), "/main.js", resolve, load)
  let st =
    run_script(
      st,
      "var out = 'unset'; import('/tla.js').then(ns => { out = ns.v })",
    )
  assert global_string(st, "out") == "waited"
  assert registry.read_pending_promise(st, "/tla.js") == option.None
  assert registry.read_module_status(st, "/tla.js") == Some(registry.Evaluated)
}

pub fn nested_dynamic_import_resolves_against_the_importing_module_test() {
  // The resolver records the referrer each request was resolved against.
  let load = fn(resolved) {
    case resolved {
      "/dir/outer.js" -> Ok("export const inner = import('./inner.js');")
      "/dir/inner.js" -> Ok("export const where = 'inner';")
      _ -> Error(load_error.LoadNotFound)
    }
  }
  let resolve = fn(raw: String, referrer: String) {
    rt_helpers.record(#(raw, referrer))
    case raw, referrer {
      "./outer.js", "/main.js" -> Ok("/dir/outer.js")
      "./inner.js", "/dir/outer.js" -> Ok("/dir/inner.js")
      _, _ -> Error(load_error.ResolveNotFound)
    }
  }
  let st = module_host.install_import_hook(agent(), "/main.js", resolve, load)
  let st =
    run_script(
      st,
      "var out = 'unset';
       import('./outer.js').then(ns => ns.inner).then(ns => { out = ns.where })
         .catch(e => { out = 'rejected ' + e })",
    )
  assert global_string(st, "out") == "inner"
  let requests: List(#(String, String)) = rt_helpers.recorded()
  assert list.contains(requests, #("./inner.js", "/dir/outer.js"))
}

pub fn import_defer_links_without_evaluating_test() {
  let #(resolve, load) =
    files([#("/lazy.js", "globalThis.ran = 'yes'; export const v = 1;")])
  let st = module_host.install_import_hook(agent(), "/main.js", resolve, load)
  let st =
    run_script(
      st,
      "var ran = 'no', before = 'unset', v = 'unset', after = 'unset';
       import.defer('/lazy.js').then(ns => { before = ran; v = String(ns.v); after = ran })",
    )
  assert global_string(st, "before") == "no"
  assert global_string(st, "v") == "1"
  assert global_string(st, "after") == "yes"
}

/// A statically imported module that dynamic-imports a relative specifier
/// AFTER its first top-level await still resolves it against itself, not the
/// entry (§16.2.1.8: the resumed execution context's ScriptOrModule).
pub fn dynamic_import_after_top_level_await_keeps_the_module_referrer_test() {
  let load = fn(resolved) {
    case resolved {
      "/dir/dep.js" -> Ok("await null; export const p = import('./sib.js');")
      "/dir/sib.js" -> Ok("export const where = 'sib';")
      _ -> Error(load_error.LoadNotFound)
    }
  }
  let resolve = fn(raw: String, referrer: String) {
    rt_helpers.record(#(raw, referrer))
    case raw, referrer {
      "./dir/dep.js", "/main.js" -> Ok("/dir/dep.js")
      "./sib.js", "/dir/dep.js" -> Ok("/dir/sib.js")
      _, _ -> Error(load_error.ResolveNotFound)
    }
  }
  let st = module_host.install_import_hook(agent(), "/main.js", resolve, load)
  let assert Ok(bundle) =
    module.compile_bundle(
      "/main.js",
      "import { p } from './dir/dep.js';
       export let out = 'unset';
       p.then(ns => { out = ns.where }, e => { out = 'rejected ' + e });",
      resolve,
      load,
    )
  let assert #(st, Ok(evaluated)) =
    module_host.evaluate_bundle_with_registry(st, bundle, rt_async.drain)
  let st = rt_async.drain(st)
  assert classify(export(st, evaluated, "out")) == KStr("sib")
  let requests: List(#(String, String)) = rt_helpers.recorded()
  assert list.contains(requests, #("./sib.js", "/dir/dep.js"))
}

/// The registry's hidden caches hang off the global object; a guest that
/// froze or sealed `globalThis` first must not be able to break the loader.
pub fn dynamic_import_survives_a_non_extensible_global_test() {
  let #(resolve, load) = files([#("/lib.js", "export const v = 'lib';")])
  let st = module_host.install_import_hook(agent(), "/main.js", resolve, load)
  let st =
    run_script(
      st,
      "var out = 'unset';
       Object.preventExtensions(globalThis);
       import('/lib.js').then(ns => { out = ns.v }, e => { out = 'rejected ' + e })",
    )
  assert global_string(st, "out") == "lib"
}

pub fn static_module_survives_a_frozen_global_test() {
  let st = run_script(agent(), "Object.freeze(globalThis)")
  let #(resolve, load) = files([])
  let assert Ok(bundle) =
    module.compile_bundle("/main.js", "export const v = 1;", resolve, load)
  let assert #(st, Ok(evaluated)) =
    module.evaluate_bundle(bundle, st, rt_async.drain)
  assert classify(export(st, evaluated, "v")) == KNum(JInt(1))
}

// -- Dynamic import under a collecting heap ----------------------------------------
//
// The import promise's capability is held only by the import job while the
// imported bodies run; the bodies cross the root-activation safepoint, so the
// job must keep the capability alive itself.

pub fn dynamic_import_survives_collection_during_the_body_test() {
  let #(resolve, load) = files([#("/lib.js", churning_module("lib"))])
  let st =
    module_host.install_import_hook(small_gc_agent(), "/main.js", resolve, load)
  let st =
    run_script(
      st,
      "var out = 'unset';
       import('/lib.js').then(ns => { out = ns.v }, e => { out = 'rejected ' + e })",
    )
  assert global_string(st, "out") == "lib300300"
}

pub fn dynamic_import_of_a_tla_module_survives_collection_test() {
  let #(resolve, load) =
    files([#("/tla.js", churning_module("tla") <> "\nawait null;")])
  let st =
    module_host.install_import_hook(small_gc_agent(), "/main.js", resolve, load)
  let st =
    run_script(
      st,
      "var out = 'unset';
       import('/tla.js').then(ns => { out = ns.v }, e => { out = 'rejected ' + e })",
    )
  assert global_string(st, "out") == "tla300300"
}

pub fn import_defer_with_an_async_dep_survives_collection_test() {
  let #(resolve, load) =
    files([
      #(
        "/lazy.js",
        "import { v as d } from '/dep.js'; export const v = 'lazy:' + d;",
      ),
      #("/dep.js", churning_module("dep") <> "\nawait null;"),
    ])
  let st =
    module_host.install_import_hook(small_gc_agent(), "/main.js", resolve, load)
  let st =
    run_script(
      st,
      "var v = 'unset';
       import.defer('/lazy.js').then(ns => { v = String(ns.v) }, e => { v = 'rejected ' + e })",
    )
  assert global_string(st, "v") == "lazy:dep300300"
}

pub fn rejected_import_promise_reports_nothing_uncaught_test() {
  let st = agent()
  let #(p, st) =
    dynamic_import.import_call(
      st,
      types.mk_string("/x.js"),
      types.mk_undefined(),
    )
  let st = rt_async.drain(st)
  let assert Some(h) = rt_async.as_promise(st, p)
  let assert #(_, PromiseRejected(_), _) = rt_async.promise_data(st, h)
}
