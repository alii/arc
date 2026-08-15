//// Host side of dynamic import(): HostLoadImportedModule (§16.2.1.8).
////
//// The interpreter's DynamicImport opcode (arc/interp/dynamic_import) does
//// the language-level ImportCall steps and then calls a host hook registered
//// as engine state on `Agent.host_fns` under `dynamic_import.import_hook_id`
//// (never a function object or globalThis property, so guest JS can neither
//// observe nor replace the module loader). This module provides that hook:
//// it resolves and loads the requested module source via an
//// embedder-supplied loader, compiles + links + evaluates the module graph
//// through arc/module, and returns the Module Namespace Exotic Object.
////
//// Per spec, HostLoadImportedModule "must perform
//// FinishLoadingImportedModule with the same result each time" for the same
//// (referrer, moduleRequest): both namespaces and evaluation errors are
//// cached in the realm registry, so a module's body runs at most once and
//// repeated imports yield the identical namespace / error.

import arc/interp/dynamic_import
import arc/module
import arc/module/graph
import arc/module/load_error
import arc/module/registry
import arc/rt/async as rt_async
import arc/rt/call as rt_call
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, Agent, HostFnEntry, SyntaxErr, TypeErr,
  mk_object, mk_undefined,
}
import gleam/dict
import gleam/list
import gleam/option
import gleam/set

/// Why an embedder's resolver / loader could not produce a module: the SAME
/// categories flow from a loader all the way to
/// `module.compile_bundle_error_message` without passing through a string.
pub type ResolveError =
  load_error.ResolveError

pub type LoadError =
  load_error.LoadError

/// Resolve a raw specifier against its referrer to the module's canonical
/// specifier: specifier math and existence probing, no source reading.
pub type ResolveFn =
  fn(String, String) -> Result(String, ResolveError)

/// Read the source of a resolved specifier.
pub type LoadFn =
  fn(String) -> Result(String, LoadError)

/// The `#(resolve, load)` pair for a SELF-CONTAINED module: every import is
/// rejected as forbidden, so no source is ever fetched.
pub fn no_imports() -> #(ResolveFn, LoadFn) {
  #(forbid_resolve, forbid_load)
}

pub fn forbid_resolve(
  _raw_specifier: String,
  _referrer: String,
) -> Result(String, ResolveError) {
  Error(load_error.ResolveForbidden)
}

pub fn forbid_load(_resolved: String) -> Result(String, LoadError) {
  Error(load_error.LoadForbidden)
}

/// Install the dynamic-import host hook on `st`. `referrer` is the path of
/// the entry script/module (relative specifiers resolve against it when no
/// module body is active); `resolve` maps specifiers to module identities
/// and `load` reads their sources (a cached import never calls `load`).
///
/// The hook is ENGINE state: an `Agent.host_fns` entry that no function
/// object carries, so it is agent-wide (every realm and activation reaches
/// it), excluded from GC and serialization like the other host functions,
/// and invisible to guest JS.
pub fn install_import_hook(
  st: Agent,
  referrer: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> Agent {
  let hook =
    HostFnEntry(name: "%DynamicImportHook%", call: fn(st, args, _this, _nt) {
      import_module(args, st, referrer, resolve, load)
    })
  Agent(
    ..st,
    host_fns: dict.insert(st.host_fns, dynamic_import.import_hook_id, hook),
  )
}

/// A dynamic-import evaluation runs inside a promise job on the host's own
/// microtask drain, so its bodies never drain nested.
fn no_drain(st: Agent) -> Agent {
  st
}

fn type_error(st: Agent, msg: String) -> #(Agent, Result(JsVal, JsVal)) {
  let #(err, st) = st.store.ops.new_error(st, TypeErr, msg)
  #(st, Error(err))
}

fn syntax_error(st: Agent, msg: String) -> #(Agent, Result(JsVal, JsVal)) {
  let #(err, st) = st.store.ops.new_error(st, SyntaxErr, msg)
  #(st, Error(err))
}

/// The hook body: parse the call, then load, compile, link and evaluate (or,
/// for `import.defer`, link) the requested module graph. For the eager phase
/// the returned `Ok` value settles the import promise; for the defer phase
/// the hook settles the promise itself through its capability and the `Ok`
/// value is meaningless. `Error(thrown)` always means "reject with `thrown`".
fn import_module(
  args: List(JsVal),
  st: Agent,
  entry_referrer: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> #(Agent, Result(JsVal, JsVal)) {
  case dynamic_import.parse_hook_args(args) {
    Error(err) -> type_error(st, dynamic_import.hook_arg_error_message(err))
    Ok(dynamic_import.HookCall(specifier:, referrer:, phase:)) -> {
      // A script-level import() has no active module: fall back to the entry
      // referrer.
      let referrer = option.unwrap(referrer, entry_referrer)
      case resolve(specifier, referrer) {
        Error(err) ->
          type_error(
            st,
            load_error.resolve_failure_message(specifier, referrer, err),
          )
        Ok(resolved) ->
          case phase {
            dynamic_import.DeferPhase(resolve_fn:, reject_fn:) ->
              defer_import_module(
                st,
                resolved,
                resolve,
                load,
                resolve_fn,
                reject_fn,
              )
            dynamic_import.EagerPhase ->
              eager_import_module(st, resolved, resolve, load)
          }
      }
    }
  }
}

/// The `import(specifier)` continuation: repeat a previously settled (or
/// in-flight) result, else load + evaluate the graph.
fn eager_import_module(
  st: Agent,
  resolved: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> #(Agent, Result(JsVal, JsVal)) {
  case registry.read_cache_state(st, resolved) {
    // The error cache wins: a namespace entry is pre-published before
    // evaluation and may be stale after a throw.
    registry.Failed(error:) -> #(st, Error(error))
    // Parked on top-level await: per Evaluate() step 4 a re-import returns
    // the same in-flight promise instead of re-running the body.
    registry.Pending(promise:, deferred: _) -> #(st, Ok(mk_object(promise)))
    // The body completed, or is mid-run (the re-entrant import case).
    registry.Started(namespace:, deferred: _) -> #(st, Ok(mk_object(namespace)))
    // A registered namespace alone is not enough: linking (an earlier
    // `import.defer()`) registers namespaces WITHOUT evaluating.
    registry.LinkedOnly(..) | registry.Absent(..) ->
      evaluate_module(st, resolved, resolve, load)
  }
}

fn evaluate_module(
  st: Agent,
  resolved: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> #(Agent, Result(JsVal, JsVal)) {
  use source <- with_loaded_source(st, resolved, load)
  case module.compile_bundle(resolved, source, resolve, load) {
    Error(err) -> compile_bundle_rejection(st, err)
    Ok(bundle) -> {
      // Evaluate WITHOUT draining: this hook runs inside a promise job on the
      // host's own drain. Bodies parked on top-level await surface as
      // EvaluationPending rather than blocking.
      let #(st, res) = evaluate_bundle_with_registry(st, bundle, no_drain)
      case res {
        Ok(module.EvaluatedBundle(value: _, namespace:)) -> #(
          st,
          Ok(mk_object(namespace)),
        )
        Error(module.EvaluationError(value: thrown)) -> {
          // Repeat the same rejection on every future import of this entry.
          let st = registry.write_module_error(st, resolved, thrown)
          #(st, Error(thrown))
        }
        Error(module.EvaluationPending(promise:)) ->
          pending_module_promise(st, resolved, promise)
        Error(module.NotInBundle(..) as other) ->
          type_error(
            st,
            "Failed to evaluate module '"
              <> resolved
              <> "': "
              <> module.error_message(other, st),
          )
      }
    }
  }
}

/// The `import.defer(specifier)` continuation (ContinueDynamicImport, phase
/// ~defer~): compile + LINK the requested graph against the registry, then
/// Evaluate() each of GatherAsynchronousTransitiveDependencies so a later
/// synchronous trigger never executes top-level await. Resolves with the
/// module's Deferred Module Namespace (cached for identity) once those
/// evaluation promises settle. This arm OWNS the import promise's settlement:
/// every success path calls `resolve_fn` and the returned `Ok` carries
/// nothing.
fn defer_import_module(
  st: Agent,
  resolved: String,
  resolve: ResolveFn,
  load: LoadFn,
  resolve_fn: JsVal,
  reject_fn: JsVal,
) -> #(Agent, Result(JsVal, JsVal)) {
  case registry.read_cache_state(st, resolved) {
    registry.Failed(error:) -> #(st, Error(error))
    registry.Pending(deferred: option.Some(deferred_ns), ..)
    | registry.Started(deferred: option.Some(deferred_ns), ..)
    | registry.LinkedOnly(deferred: option.Some(deferred_ns), ..)
    | registry.Absent(deferred: option.Some(deferred_ns)) ->
      settle_defer_import(st, resolve_fn, mk_object(deferred_ns))
    registry.Pending(deferred: option.None, ..)
    | registry.Started(deferred: option.None, ..)
    | registry.LinkedOnly(deferred: option.None, ..)
    | registry.Absent(deferred: option.None) -> {
      use source <- with_loaded_source(st, resolved, load)
      case module.compile_bundle(resolved, source, resolve, load) {
        Error(err) -> compile_bundle_rejection(st, err)
        Ok(bundle) ->
          case link_bundle_with_registry(st, bundle) {
            #(st, Error(module.EvaluationError(value: thrown))) -> #(
              st,
              Error(thrown),
            )
            #(st, Error(other)) ->
              type_error(
                st,
                "Failed to link module '"
                  <> resolved
                  <> "': "
                  <> module.error_message(other, st),
              )
            #(st, Ok(linked_bundle)) ->
              case
                module.get_or_create_deferred_namespace(
                  st,
                  linked_bundle,
                  resolved,
                )
              {
                #(st, Ok(ns)) -> {
                  let st = registry.write_deferred_namespace(st, resolved, ns)
                  evaluate_deferred_async_deps(
                    st,
                    resolved,
                    mk_object(ns),
                    linked_bundle,
                    resolve_fn,
                    reject_fn,
                  )
                }
                #(st, Error(module.DeferredSpecifierNotInBundle(specifier:))) ->
                  type_error(st, "Cannot find module '" <> specifier <> "'")
              }
          }
      }
    }
  }
}

fn settle_defer_import(
  st: Agent,
  resolve_fn: JsVal,
  value: JsVal,
) -> #(Agent, Result(JsVal, JsVal)) {
  #(call_import_settle_fn(st, resolve_fn, value), Ok(mk_undefined()))
}

/// Read `resolved`'s source, or reject the import with a TypeError.
fn with_loaded_source(
  st: Agent,
  resolved: String,
  load: LoadFn,
  then: fn(String) -> #(Agent, Result(JsVal, JsVal)),
) -> #(Agent, Result(JsVal, JsVal)) {
  case load(resolved) {
    Error(err) -> type_error(st, load_error.load_failure_message(resolved, err))
    Ok(source) -> then(source)
  }
}

/// ContinueDynamicImport's ~defer~ arm, after linking: evaluate the entry's
/// asynchronous transitive dependencies; resolve the import promise with the
/// deferred namespace only after their top-level promises settle.
fn evaluate_deferred_async_deps(
  st: Agent,
  resolved: String,
  ns: JsVal,
  linked_bundle: module.LinkedBundle,
  resolve_fn: JsVal,
  reject_fn: JsVal,
) -> #(Agent, Result(JsVal, JsVal)) {
  case module.evaluate_async_transitive_deps(linked_bundle, st, no_drain) {
    // No async dependency parked on top-level await: resolve immediately.
    #(st, Ok([])) -> settle_defer_import(st, resolve_fn, ns)
    #(st, Ok(pendings)) -> #(
      chain_deferred_settlement(st, ns, pendings, resolve_fn, reject_fn),
      Ok(mk_undefined()),
    )
    #(st, Error(module.EvaluationError(value: thrown))) -> {
      let st = registry.write_module_error(st, resolved, thrown)
      #(st, Error(thrown))
    }
    #(st, Error(module.NotInBundle(..) as other))
    | #(st, Error(module.EvaluationPending(..) as other)) ->
      type_error(
        st,
        "Failed to evaluate async dependencies of module '"
          <> resolved
          <> "': "
          <> module.error_message(other, st),
      )
  }
}

/// Async transitive dependencies of a deferred import are parked on
/// top-level await: chain the import promise's settlement onto their
/// [[TopLevelCapability]] promises (PerformPromiseThen directly, never a
/// `then` lookup: the proposal's SafePerformPromiseAll), resolving with the
/// deferred namespace once every one fulfills, rejecting on the first
/// rejection.
fn chain_deferred_settlement(
  st: Agent,
  ns: JsVal,
  pendings: List(#(String, Handle)),
  resolve_fn: JsVal,
  reject_fn: JsVal,
) -> Agent {
  case pendings {
    [] -> call_import_settle_fn(st, resolve_fn, ns)
    [#(dep_spec, tla_promise), ..rest] -> {
      let #(on_fulfilled, st) = {
        use st, _args <- module.alloc_host_fn(st, "%ContinueDeferredImport%", 0)
        // The dep's body completed (AsyncModuleExecutionFulfilled): record
        // ~evaluated~ so a later deferred-namespace trigger does not see a
        // stuck ~evaluating~ and refuse to run.
        let st = registry.write_module_status(st, dep_spec, registry.Evaluated)
        #(
          mk_undefined(),
          chain_deferred_settlement(st, ns, rest, resolve_fn, reject_fn),
        )
      }
      let #(on_rejected, st) = {
        use st, args <- module.alloc_host_fn(
          st,
          "%ContinueDeferredImportRejected%",
          1,
        )
        let reason = first_or_undefined(args)
        // AsyncModuleExecutionRejected: record the dep's error so later
        // imports and deferred-namespace triggers rethrow it. The entry stays
        // unevaluated and uncached: a later import.defer re-links and surfaces
        // the dep's cached error.
        let st = registry.write_module_error(st, dep_spec, reason)
        #(mk_undefined(), call_import_settle_fn(st, reject_fn, reason))
      }
      let #(_child, st) =
        rt_async.t_promise_then(
          st,
          tla_promise,
          mk_object(on_fulfilled),
          mk_object(on_rejected),
        )
      st
    }
  }
}

fn first_or_undefined(args: List(JsVal)) -> JsVal {
  case args {
    [v, ..] -> v
    [] -> mk_undefined()
  }
}

/// Call one of the import promise's resolving functions (§27.2.1.3: they
/// return undefined and never throw); report through the host sink if one
/// somehow does.
fn call_import_settle_fn(st: Agent, settle_fn: JsVal, arg: JsVal) -> Agent {
  case rt_call.t_call(st, settle_fn, mk_undefined(), [arg]) {
    #(rt_call.NormalCompletion(_), st) -> st
    #(rt_call.ThrowCompletion(thrown), st) -> {
      st.hooks.report_uncaught(
        "arc: import.defer settling function threw: "
        <> module.error_message(module.EvaluationError(thrown), st),
      )
      st
    }
  }
}

/// Link a compiled bundle against the realm registry WITHOUT evaluating any
/// body: registers every new module's namespace and deferred namespace so
/// later imports (eager or deferred, static or dynamic) resolve to the same
/// module records.
fn link_bundle_with_registry(
  st: Agent,
  bundle: module.ModuleBundle,
) -> #(Agent, Result(module.LinkedBundle, module.ModuleError)) {
  let specs = dict.keys(bundle.modules)
  let preexisting = registered(st, specs, registry.read_namespace)
  let preexisting_deferred =
    registered(st, specs, registry.read_deferred_namespace)
  case
    module.link_for_evaluation_reusing(
      bundle,
      st,
      preexisting,
      preexisting_deferred,
    )
  {
    #(st, Error(err)) -> #(st, Error(err))
    #(st, Ok(linked_bundle)) -> {
      let st =
        list.fold(module.linked_namespaces(linked_bundle, st), st, fn(st, pair) {
          let #(spec, ns) = pair
          case dict.has_key(preexisting, spec) {
            True -> st
            False -> registry.write_namespace(st, spec, ns)
          }
        })
      let st =
        list.fold(
          module.linked_deferred_namespaces(linked_bundle, st),
          st,
          fn(st, pair) {
            let #(spec, ns) = pair
            case dict.has_key(preexisting_deferred, spec) {
              True -> st
              False -> registry.write_deferred_namespace(st, spec, ns)
            }
          },
        )
      #(st, Ok(linked_bundle))
    }
  }
}

/// The subset of `specs` a registry cache knows, as specifier → handle.
fn registered(
  st: Agent,
  specs: List(String),
  read: fn(Agent, String) -> option.Option(Handle),
) -> dict.Dict(String, Handle) {
  list.fold(specs, dict.new(), fn(acc, spec) {
    case read(st, spec) {
      option.Some(h) -> dict.insert(acc, spec, h)
      option.None -> acc
    }
  })
}

/// §16.2.1.5.2 Evaluate() step 4 + ContinueDynamicImport: the entry module
/// is parked on top-level await. Build a promise that settles with the
/// module's namespace (or evaluation error) when its [[TopLevelCapability]]
/// settles, publish it in the pending cache so a re-import chains onto the
/// SAME in-flight evaluation, and hand it to the import machinery: the
/// import promise adopts it via the standard resolving functions.
fn pending_module_promise(
  st: Agent,
  resolved: String,
  tla_promise: Handle,
) -> #(Agent, Result(JsVal, JsVal)) {
  // The namespace was pre-published in the registry before any body ran.
  case registry.read_namespace(st, resolved) {
    option.None ->
      type_error(st, "Module '" <> resolved <> "' produced no namespace")
    option.Some(namespace_h) -> {
      let namespace = mk_object(namespace_h)
      let #(#(ns_promise, ns_resolve, ns_reject), st) =
        rt_async.t_new_promise_capability(st)
      // Fulfilled (AsyncModuleExecutionFulfilled): [[Status]] = ~evaluated~,
      // so a later deferred trigger over it is ready; future imports read
      // the namespace cache; the namespace promise fulfills with the
      // namespace itself.
      let #(on_fulfilled, st) = {
        use st, _args <- module.alloc_host_fn(st, "%FinishDynamicImport%", 0)
        let st =
          st
          |> registry.clear_pending_promise(resolved)
          |> registry.write_module_status(resolved, registry.Evaluated)
        #(namespace, st)
      }
      // Rejected: cache the evaluation error (every future import repeats
      // the same rejection) and re-throw so the namespace promise rejects.
      let #(on_rejected, st) = {
        use st, args <- module.alloc_host_fn(
          st,
          "%FinishDynamicImportRejected%",
          1,
        )
        let reason = first_or_undefined(args)
        let st =
          st
          |> registry.clear_pending_promise(resolved)
          |> registry.write_module_error(resolved, reason)
        rt_store.t_throw(st, reason)
      }
      let st =
        rt_async.t_perform_then(
          st,
          tla_promise,
          mk_object(on_fulfilled),
          mk_object(on_rejected),
          mk_object(ns_resolve),
          mk_object(ns_reject),
        )
      let st = registry.write_pending_promise(st, resolved, ns_promise)
      #(st, Ok(mk_object(ns_promise)))
    }
  }
}

/// Evaluate a compiled bundle against the realm-wide module registry,
/// enforcing the §16.2.1.8 module-map invariant at module-record granularity:
///
///   - graph nodes already registered keep their namespace identity and are
///     NOT re-evaluated (their bodies ran in an earlier bundle);
///   - every other node's namespace is registered BEFORE evaluation, so a
///     re-entrant import() during evaluation resolves to the same record;
///   - when evaluation throws, registrations are rolled back for the nodes
///     whose bodies never completed, while completed nodes stay registered.
///
/// Used by both the dynamic-import hook and static module entry points
/// sharing a realm, so `import './a.js'` and `import('./a.js')` yield the
/// same module record.
pub fn evaluate_bundle_with_registry(
  st: Agent,
  bundle: module.ModuleBundle,
  finish: module.Finish,
) -> #(Agent, Result(module.EvaluatedBundle, module.ModuleError)) {
  let specs = dict.keys(bundle.modules)
  let preexisting = registered(st, specs, registry.read_namespace)
  // Link + register every NEW module's namespace (and deferred namespace)
  // before any body runs.
  case link_bundle_with_registry(st, bundle) {
    #(st, Error(err)) -> #(st, Error(err))
    #(st, Ok(linked_bundle)) -> {
      // Already-evaluated = exactly the modules whose bodies have completed;
      // a registered-but-linked-only module (an earlier `import.defer()`)
      // still needs its body run when imported eagerly.
      let already_evaluated =
        list.fold(specs, set.new(), fn(acc, spec) {
          case registry.read_module_status(st, spec) {
            option.Some(registry.Evaluated) -> set.insert(acc, spec)
            option.Some(registry.Evaluating) | option.None -> acc
          }
        })
      let #(st, evaluated, res) =
        module.evaluate_linked_tracking(
          linked_bundle,
          st,
          finish,
          already_evaluated,
        )
      case res {
        Ok(module.EvaluatedBundle(..)) -> #(st, res)
        Error(module.EvaluationError(value:)) -> {
          // Roll back nodes whose bodies never completed. Host (synthetic)
          // modules have no body to leave half-done and their cells are
          // permanently initialized: clearing them would strand this bundle's
          // surviving `import * as ns` bindings.
          let st =
            list.fold(module.source_specifiers(bundle), st, fn(st, spec) {
              case
                dict.has_key(preexisting, spec) || set.contains(evaluated, spec)
              {
                True -> st
                False -> registry.clear_module_registrations(st, spec)
              }
            })
          #(st, Error(module.EvaluationError(value:)))
        }
        // Mid-flight on top-level await: registrations stay (a re-import
        // must resolve to the same record).
        Error(module.EvaluationPending(promise: _))
        | Error(module.NotInBundle(..)) -> #(st, res)
      }
    }
  }
}

/// Turn a `module.compile_bundle` failure into the JS-visible rejection.
/// Per HostLoadImportedModule: parse failures, source-phase imports and
/// compile failures reject with a SyntaxError; a request that cannot be
/// resolved or loaded rejects with a TypeError.
fn compile_bundle_rejection(
  st: Agent,
  err: module.CompileBundleError,
) -> #(Agent, Result(JsVal, JsVal)) {
  case err {
    module.GraphError(error: graph.ParseFailed(..))
    | module.GraphError(error: graph.SourcePhaseUnsupported(..))
    | module.CompileError(..) ->
      syntax_error(st, module.compile_bundle_error_message(err))
    module.GraphError(error: graph.ResolveFailed(..))
    | module.GraphError(error: graph.LoadFailed(..)) ->
      type_error(st, module.compile_bundle_error_message(err))
  }
}
