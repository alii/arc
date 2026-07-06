//// Host side of dynamic import() — HostLoadImportedModule (§16.2.1.8).
////
//// The VM's DynamicImport opcode (see arc/vm/exec/dynamic_import) performs
//// the language-level ImportCall steps and then calls a host hook carried as
//// ENGINE state on the realm's `HostHooks.import_hook` — never a globalThis
//// property, so guest JS can neither observe nor replace the module loader.
//// This module provides that hook: it resolves and loads the requested
//// module source via an embedder-supplied loader, compiles + links +
//// evaluates the module graph through arc/module, and returns the Module
//// Namespace Exotic Object.
////
//// Per spec, "If this operation is called multiple times with the same
//// (referrer, moduleRequest) pair ... it must perform FinishLoadingImportedModule
//// with the same result each time": both successful namespaces and evaluation
//// errors are cached (on hidden global objects), so a module's body runs at
//// most once and repeated imports yield the identical namespace / error.

import arc/module
import arc/module/graph
import arc/module/load_error
import arc/module/registry
import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/promise as builtins_promise
import arc/vm/exec/dynamic_import
import arc/vm/heap
import arc/vm/host_hooks
import arc/vm/internal/job_queue
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{type JsValue, type Ref, JsObject, JsUndefined}
import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleam/string

// The realm-wide module caches (namespaces, deferred namespaces, evaluation
// errors, pending top-level-await promises, statuses) live on hidden global
// objects behind the typed accessors of `arc/module/registry` — shared with
// the module evaluator so a deferred-namespace trigger and a dynamic import
// observe the same state.

/// Why an embedder's resolver / loader could not produce a module —
/// re-exported from `arc/module/load_error`, which the runtime-free graph walk
/// and the AOT compiler share, so the SAME categories flow from a loader all
/// the way to `module.compile_bundle_error_message` without ever passing
/// through a rendered string. Neither carries the specifier it failed on: the
/// caller holds that, and words it via `load_error`'s two message functions.
pub type ResolveError =
  load_error.ResolveError

pub type LoadError =
  load_error.LoadError

/// Resolve a raw specifier against its referrer to the module's canonical
/// specifier — specifier math and existence probing, no source reading.
pub type ResolveFn =
  fn(String, String) -> Result(String, ResolveError)

/// Read the source of a resolved specifier.
pub type LoadFn =
  fn(String) -> Result(String, LoadError)

/// The `#(resolve, load)` pair for a SELF-CONTAINED module: every import is
/// rejected as forbidden, so no source is ever fetched. The one blessed
/// spelling of a loader pair embedders otherwise hand-roll identically each
/// time.
pub fn no_imports() -> #(ResolveFn, LoadFn) {
  #(forbid_resolve, forbid_load)
}

/// A `ResolveFn` that rejects every specifier — see `no_imports`.
pub fn forbid_resolve(
  _raw_specifier: String,
  _referrer: String,
) -> Result(String, ResolveError) {
  Error(load_error.ResolveForbidden)
}

/// A `LoadFn` that rejects every specifier — see `no_imports`.
pub fn forbid_load(_resolved: String) -> Result(String, LoadError) {
  Error(load_error.LoadForbidden)
}

/// Build the dynamic-import host hook. `referrer` is the path of the entry
/// script/module (relative specifiers resolve against it when no module body
/// is active); `resolve` maps specifiers to module identities and `load`
/// reads their sources — a cached import never calls `load` at all.
///
/// The returned function value is ENGINE state: the embedder stores it in the
/// `HostHooks.import_hook` it boots the realm with, and every derived State
/// (eval realms, module bodies, dynamic-import continuations) inherits it.
/// It is NEVER installed as a globalThis property, so guest JS cannot read,
/// replace or delete the module loader. Because nothing else in the heap
/// reaches the hook's function object, it is pinned as a persistent GC root
/// here (like builtins prototypes and template objects).
///
/// The VM passes the active module's resolved specifier as a second argument
/// when one was executing at the ImportCall site, which takes precedence over
/// the install-time referrer (§16.2.1.8's referencingScriptOrModule).
pub fn install_import_hook(
  h: Heap(host),
  b: Builtins,
  referrer: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> #(Heap(host), JsValue) {
  let #(h, hook_ref) =
    common.alloc_host_fn(
      h,
      b.function.prototype,
      fn(args, this, state) {
        import_module(args, this, state, referrer, resolve, load)
      },
      "%DynamicImportHook%",
      1,
    )
  #(heap.root(h, hook_ref), JsObject(hook_ref))
}

/// The hook body: parse the call, then load, compile, link and evaluate (or,
/// for `import.defer`, link) the requested module graph. For `Eager` the
/// returned `Ok` value settles the import promise; for `Defer` the hook
/// settles the promise itself through its capability and the `Ok` value is
/// meaningless. `Error(thrown)` always means "reject with `thrown`".
fn import_module(
  args: List(JsValue),
  _this: JsValue,
  state: State(host),
  entry_referrer: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> #(State(host), Result(JsValue, JsValue)) {
  // The argument encoding is `arc/vm/exec/dynamic_import`'s — decoded there,
  // beside the encoder that built it, so the two cannot drift.
  case dynamic_import.parse_hook_args(args) {
    Error(err) ->
      state.type_error(state, dynamic_import.hook_arg_error_message(err))
    Ok(dynamic_import.HookCall(specifier:, referrer:, phase:)) -> {
      // A script-level import() has no active module: fall back to the realm's
      // entry referrer.
      let referrer = option.unwrap(referrer, entry_referrer)
      case resolve(specifier, referrer) {
        Error(err) ->
          // Resolution failure → TypeError (host resolution error).
          state.type_error(
            state,
            load_error.resolve_failure_message(specifier, referrer, err),
          )
        Ok(resolved) ->
          case phase {
            dynamic_import.DeferPhase(resolve_fn:, reject_fn:) ->
              defer_import_module(
                state,
                resolved,
                resolve,
                load,
                resolve_fn,
                reject_fn,
              )
            dynamic_import.EagerPhase ->
              eager_import_module(state, resolved, resolve, load)
          }
      }
    }
  }
}

/// The `import(specifier)` continuation: repeat a previously settled (or
/// in-flight) result, else load + evaluate the graph.
fn eager_import_module(
  state: State(host),
  resolved: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> #(State(host), Result(JsValue, JsValue)) {
  // A previous import of this module settled (or is mid-evaluation) — repeat
  // the same result. `registry.read_cache_state` holds the precedence rules
  // (error over pending over namespace), shared with the `import.defer()` arm.
  case
    registry.read_cache_state(state.heap, state.ctx.global_object, resolved)
  {
    // The error cache wins: a namespace entry is pre-published before
    // evaluation and may be stale after a throw.
    registry.Failed(error:) -> #(state, Error(error))
    // Parked on top-level await: per Evaluate() step 4 a re-import returns the
    // same in-flight top-level promise instead of re-running the body.
    registry.Pending(promise:, deferred: _) -> #(state, Ok(JsObject(promise)))
    // The body has completed, or is mid-run (the re-entrant import case, which
    // must not re-run it).
    registry.Started(namespace:, deferred: _) -> #(
      state,
      Ok(JsObject(namespace)),
    )
    // A registered namespace alone is not enough: linking (e.g. an earlier
    // `import.defer()`) registers namespaces WITHOUT evaluating.
    registry.LinkedOnly(..) | registry.Absent(..) ->
      evaluate_module(state, resolved, resolve, load)
  }
}

fn evaluate_module(
  state: State(host),
  resolved: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> #(State(host), Result(JsValue, JsValue)) {
  use source <- with_loaded_source(state, resolved, load)
  case module.compile_bundle(resolved, source, resolve, load) {
    Error(err) -> compile_bundle_rejection(state, err)
    Ok(bundle) -> {
      // Evaluate WITHOUT draining a nested event loop: this hook runs inside
      // a promise job on the host's own event loop, and a nested drain would
      // execute host continuations against the module's (empty) lexical
      // context. Any jobs the module bodies enqueued are appended to the
      // host's queue instead, and a body parked on top-level await surfaces
      // as EvaluationPending rather than blocking.
      // The importing realm's host hooks are threaded into every module
      // body's freshly booted State, so a dynamically imported module gets
      // the same embedder capabilities (e.g. a blocking `Atomics.wait`) as
      // the importer.
      let #(h, jobs, result) =
        evaluate_bundle_with_registry(
          state.heap,
          state.builtins,
          state.ctx.global_object,
          bundle,
          state.ctx.host_hooks,
          state.can_block,
          state.ctx.extend_262,
          fn(module_state) { module_state },
        )
      let state =
        State(
          ..state,
          heap: h,
          job_queue: job_queue.append(state.job_queue, jobs),
        )
      case result {
        Ok(module.EvaluatedBundle(value: _, namespace: ns_ref)) -> #(
          state,
          Ok(JsObject(ns_ref)),
        )
        Error(module.EvaluationError(value: thrown)) -> {
          // Repeat the same rejection on every future import of this entry.
          let state = cache_module_error(state, resolved, thrown)
          #(state, Error(thrown))
        }
        Error(module.EvaluationPending(promise_data_ref:)) ->
          pending_module_promise(state, resolved, promise_data_ref)
        Error(module.NotInBundle(..) as other) ->
          state.type_error(
            state,
            "Failed to evaluate module '"
              <> resolved
              <> "': "
              <> module.error_message(other, state.heap),
          )
      }
    }
  }
}

/// The `import.defer(specifier)` continuation (ContinueDynamicImport, phase
/// ~defer~): compile + LINK the requested graph against the realm registry,
/// then GatherAsynchronousTransitiveDependencies and Evaluate() each gathered
/// module — only their async bodies run eagerly, so a later synchronous
/// trigger never executes top-level await. Resolves with the module's
/// Deferred Module Namespace — cached so repeated deferred imports (static
/// or dynamic) of the same module yield the identical object — once those
/// evaluation promises settle (immediately when there are none).
///
/// This is the hook's ~defer~ arm, so it OWNS the import promise's
/// settlement (see `dynamic_import.DeferHookOutcome`): every success path
/// calls `resolve_fn` itself and the returned `Ok` value is meaningless.
/// `Error(thrown)` is an abrupt completion the caller rejects with.
fn defer_import_module(
  state: State(host),
  resolved: String,
  resolve: ResolveFn,
  load: LoadFn,
  resolve_fn: JsValue,
  reject_fn: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  // An already-failed module repeats the same rejection; an already registered
  // deferred namespace is resolved with as-is. Same `registry.read_cache_state`
  // ladder as the eager arm — the deferred namespace's identity is per module
  // record, so it is handed back whatever the body's status is (including
  // "parked on top-level await": the deferred namespace is still THE object).
  case
    registry.read_cache_state(state.heap, state.ctx.global_object, resolved)
  {
    registry.Failed(error:) -> #(state, Error(error))
    registry.Pending(deferred: Some(deferred_ns_ref), ..)
    | registry.Started(deferred: Some(deferred_ns_ref), ..)
    | registry.LinkedOnly(deferred: Some(deferred_ns_ref), ..)
    | registry.Absent(deferred: Some(deferred_ns_ref)) ->
      settle_defer_import(state, resolve_fn, JsObject(deferred_ns_ref))
    registry.Pending(deferred: None, ..)
    | registry.Started(deferred: None, ..)
    | registry.LinkedOnly(deferred: None, ..)
    | registry.Absent(deferred: None) -> {
      use source <- with_loaded_source(state, resolved, load)
      case module.compile_bundle(resolved, source, resolve, load) {
        Error(err) -> compile_bundle_rejection(state, err)
        Ok(bundle) -> {
          // The heap comes back beside the result — always the live one.
          let #(h, link_result) =
            link_bundle_with_registry(
              state.heap,
              state.builtins,
              state.ctx.global_object,
              bundle,
            )
          let state = State(..state, heap: h)
          case link_result {
            Error(module.EvaluationError(value: thrown)) -> #(
              state,
              Error(thrown),
            )
            Error(other) ->
              state.type_error(
                state,
                "Failed to link module '"
                  <> resolved
                  <> "': "
                  <> module.error_message(other, state.heap),
              )
            Ok(linked_bundle) -> {
              let #(h, deferred_ns) =
                module.get_or_create_deferred_namespace(
                  state.heap,
                  state.builtins,
                  linked_bundle,
                  resolved,
                )
              let state = State(..state, heap: h)
              case deferred_ns {
                Ok(ns_ref) -> {
                  let state =
                    State(
                      ..state,
                      heap: registry.write_deferred_namespace(
                        state.heap,
                        state.ctx.global_object,
                        resolved,
                        ns_ref,
                      ),
                    )
                  evaluate_deferred_async_deps(
                    state,
                    resolved,
                    JsObject(ns_ref),
                    linked_bundle,
                    resolve_fn,
                    reject_fn,
                  )
                }
                Error(module.DeferredSpecifierNotInBundle(specifier:)) ->
                  state.type_error(
                    state,
                    "Cannot find module '" <> specifier <> "'",
                  )
              }
            }
          }
        }
      }
    }
  }
}

/// Settle the deferred import's promise with `value` through the capability
/// the VM handed the hook, and hand back the "hook took ownership" normal
/// completion (its `Ok` value carries nothing — see
/// `dynamic_import.DeferHookOutcome`).
fn settle_defer_import(
  state: State(host),
  resolve_fn: JsValue,
  value: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  #(call_import_settle_fn(state, resolve_fn, value), Ok(JsUndefined))
}

/// Read `resolved`'s source for compilation, or reject the import with a
/// TypeError (host load error). Cached imports never get here.
fn with_loaded_source(
  state: State(host),
  resolved: String,
  load: LoadFn,
  then: fn(String) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case load(resolved) {
    Error(err) ->
      state.type_error(state, load_error.load_failure_message(resolved, err))
    Ok(source) -> then(source)
  }
}

/// ContinueDynamicImport's ~defer~ arm, after linking: evaluate the entry's
/// asynchronous transitive dependencies; resolve the import promise with the
/// deferred namespace only after their top-level promises settle. The
/// promise capability is guaranteed present (a `Defer` phase cannot be
/// constructed without it), so "pending async deps but nowhere to settle" is
/// no longer a reachable error.
fn evaluate_deferred_async_deps(
  state: State(host),
  resolved: String,
  ns: JsValue,
  linked_bundle: module.LinkedBundle,
  resolve_fn: JsValue,
  reject_fn: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  // As in evaluate_module: never drain a nested event loop here — bodies run
  // with an identity driver and any jobs they enqueued (including the parked
  // top-level-await continuations) are appended to the host's queue.
  let #(h, jobs, dep_result) =
    module.evaluate_async_transitive_deps(
      linked_bundle,
      state.heap,
      module.BootCtx(
        builtins: state.builtins,
        global_object: state.ctx.global_object,
        host_hooks: state.ctx.host_hooks,
        can_block: state.can_block,
        extend_262: state.ctx.extend_262,
        finish: fn(module_state) { module_state },
      ),
    )
  let state =
    State(..state, heap: h, job_queue: job_queue.append(state.job_queue, jobs))
  case dep_result {
    // No async dependency parked on top-level await — resolve immediately.
    Ok([]) -> settle_defer_import(state, resolve_fn, ns)
    // The chained reactions settle the import promise once every pending
    // dependency's top-level promise does.
    Ok(pendings) -> #(
      chain_deferred_settlement(state, ns, pendings, resolve_fn, reject_fn),
      Ok(JsUndefined),
    )
    Error(module.EvaluationError(value: thrown)) -> {
      // Repeat the same rejection on every future import of this entry.
      let state = cache_module_error(state, resolved, thrown)
      #(state, Error(thrown))
    }
    Error(module.NotInBundle(..) as other)
    | Error(module.EvaluationPending(..) as other) ->
      state.type_error(
        state,
        "Failed to evaluate async dependencies of module '"
          <> resolved
          <> "': "
          <> module.error_message(other, state.heap),
      )
  }
}

/// Async transitive dependencies of a deferred import are parked on
/// top-level await: chain the import promise's settlement onto their
/// [[TopLevelCapability]] promises — PerformPromiseThen directly, never a
/// `then` lookup (the proposal's SafePerformPromiseAll) — resolving with the
/// deferred namespace once every one fulfills, rejecting on the first
/// rejection.
fn chain_deferred_settlement(
  state: State(host),
  ns: JsValue,
  pendings: List(#(String, Ref)),
  resolve_fn: JsValue,
  reject_fn: JsValue,
) -> State(host) {
  case pendings {
    [] -> call_import_settle_fn(state, resolve_fn, ns)
    [#(dep_spec, tla_data_ref), ..rest] -> {
      let #(heap, on_fulfilled) =
        common.alloc_host_fn(
          state.heap,
          state.builtins.function.prototype,
          fn(_args, _this, state) {
            // The dep's body completed (AsyncModuleExecutionFulfilled) —
            // record ~evaluated~ so a later deferred-namespace trigger does
            // not see a stuck ~evaluating~ and refuse to run.
            let heap =
              registry.write_module_status(
                state.heap,
                state.ctx.global_object,
                dep_spec,
                registry.Evaluated,
              )
            let state =
              chain_deferred_settlement(
                State(..state, heap:),
                ns,
                rest,
                resolve_fn,
                reject_fn,
              )
            #(state, Ok(JsUndefined))
          },
          "%ContinueDeferredImport%",
          0,
        )
      let #(heap, on_rejected) =
        common.alloc_host_fn(
          heap,
          state.builtins.function.prototype,
          fn(args, _this, state) {
            let reason = case args {
              [r, ..] -> r
              [] -> JsUndefined
            }
            // AsyncModuleExecutionRejected: record the dep's error so later
            // imports and deferred-namespace triggers rethrow it. The entry
            // itself stays unevaluated and uncached — a later import.defer
            // re-links and surfaces the dep's cached error.
            let state = cache_module_error(state, dep_spec, reason)
            let state = call_import_settle_fn(state, reject_fn, reason)
            #(state, Ok(JsUndefined))
          },
          "%ContinueDeferredImportRejected%",
          1,
        )
      let state = State(..state, heap:)
      builtins_promise.perform_promise_then(
        state,
        tla_data_ref,
        JsObject(on_fulfilled),
        JsObject(on_rejected),
        JsUndefined,
        JsUndefined,
      )
    }
  }
}

/// Call one of the import promise's resolving functions (§27.2.1.3 — they
/// return undefined and never throw); log defensively if one somehow does.
fn call_import_settle_fn(
  state: State(host),
  settle_fn: JsValue,
  arg: JsValue,
) -> State(host) {
  case state.call(state, settle_fn, JsUndefined, [arg]) {
    Ok(#(_, state)) -> state
    Error(#(thrown, state)) -> {
      io.println_error(
        "arc: import.defer settling function threw: " <> string.inspect(thrown),
      )
      state
    }
  }
}

/// Link a compiled bundle against the realm registry (shared with
/// evaluate_bundle_with_registry) WITHOUT evaluating any body: registers
/// every new module's namespace and deferred namespace so later imports —
/// eager or deferred, static or dynamic — resolve to the same module records.
///
/// The heap travels BESIDE the result and is the live one on both paths (a link
/// error's JS error object was allocated in it), so there is nothing to choose
/// between.
fn link_bundle_with_registry(
  h: Heap(host),
  b: Builtins,
  global_object: Ref,
  bundle: module.ModuleBundle,
) -> #(Heap(host), Result(module.LinkedBundle, module.ModuleError)) {
  let specs = dict.keys(bundle.modules)
  let preexisting =
    list.fold(specs, dict.new(), fn(acc, spec) {
      case registry.read_namespace(h, global_object, spec) {
        Some(ns_ref) -> dict.insert(acc, spec, ns_ref)
        None -> acc
      }
    })
  let preexisting_deferred =
    list.fold(specs, dict.new(), fn(acc, spec) {
      case registry.read_deferred_namespace(h, global_object, spec) {
        Some(ns_ref) -> dict.insert(acc, spec, ns_ref)
        None -> acc
      }
    })
  case
    module.link_for_evaluation_reusing(
      bundle,
      h,
      b,
      preexisting,
      preexisting_deferred,
    )
  {
    #(h, Error(err)) -> #(h, Error(err))
    #(h, Ok(linked_bundle)) -> {
      let h =
        list.fold(module.linked_namespaces(linked_bundle, h), h, fn(h, pair) {
          let #(spec, ns_ref) = pair
          case dict.has_key(preexisting, spec) {
            True -> h
            False -> registry.write_namespace(h, global_object, spec, ns_ref)
          }
        })
      let h =
        list.fold(
          module.linked_deferred_namespaces(linked_bundle, h),
          h,
          fn(h, pair) {
            let #(spec, ns_ref) = pair
            case dict.has_key(preexisting_deferred, spec) {
              True -> h
              False ->
                registry.write_deferred_namespace(
                  h,
                  global_object,
                  spec,
                  ns_ref,
                )
            }
          },
        )
      #(h, Ok(linked_bundle))
    }
  }
}

/// §16.2.1.5.2 Evaluate() step 4 + ContinueDynamicImport: the entry module is
/// parked on top-level await. Build a promise that settles with the module's
/// namespace (or evaluation error) when its [[TopLevelCapability]] settles,
/// publish it in the pending cache so a re-import chains onto the SAME
/// in-flight evaluation, and hand it to the import machinery — the import
/// promise adopts it via the standard resolving functions (§27.2.1.3.2).
fn pending_module_promise(
  state: State(host),
  resolved: String,
  tla_data_ref: Ref,
) -> #(State(host), Result(JsValue, JsValue)) {
  // The namespace was pre-published in the registry before any body ran.
  case registry.read_namespace(state.heap, state.ctx.global_object, resolved) {
    Some(namespace_ref) -> {
      let namespace = JsObject(namespace_ref)
      let #(
        heap,
        builtins_promise.PromiseRefs(promise: ns_promise_ref, data: ns_data_ref),
      ) =
        builtins_promise.create_promise(
          state.heap,
          state.builtins.promise.prototype,
        )
      let #(
        heap,
        builtins_promise.ResolvingFns(resolve: ns_resolve, reject: ns_reject),
      ) =
        builtins_promise.create_resolving_functions(
          heap,
          state.builtins.function.prototype,
          ns_promise_ref,
          ns_data_ref,
        )
      // Fulfilled: evaluation completed — future imports read the namespace
      // cache; the namespace promise fulfills with the namespace itself.
      let #(heap, on_fulfilled) =
        common.alloc_host_fn(
          heap,
          state.builtins.function.prototype,
          fn(_args, _this, state) {
            let state = clear_pending_promise(state, resolved)
            #(state, Ok(namespace))
          },
          "%FinishDynamicImport%",
          0,
        )
      // Rejected: cache the evaluation error (every future import repeats
      // the same rejection) and re-throw so the namespace promise rejects.
      let #(heap, on_rejected) =
        common.alloc_host_fn(
          heap,
          state.builtins.function.prototype,
          fn(args, _this, state) {
            let reason = case args {
              [r, ..] -> r
              [] -> JsUndefined
            }
            let state = clear_pending_promise(state, resolved)
            let state = cache_module_error(state, resolved, reason)
            #(state, Error(reason))
          },
          "%FinishDynamicImportRejected%",
          1,
        )
      let state = State(..state, heap:)
      let state =
        builtins_promise.perform_promise_then(
          state,
          tla_data_ref,
          JsObject(on_fulfilled),
          JsObject(on_rejected),
          ns_resolve,
          ns_reject,
        )
      let state =
        State(
          ..state,
          heap: registry.write_pending_promise(
            state.heap,
            state.ctx.global_object,
            resolved,
            ns_promise_ref,
          ),
        )
      #(state, Ok(JsObject(ns_promise_ref)))
    }
    None ->
      state.type_error(
        state,
        "Module '" <> resolved <> "' produced no namespace",
      )
  }
}

/// Record `resolved`'s sticky evaluation error in the realm registry — every
/// later import or deferred-namespace trigger repeats the same rejection.
fn cache_module_error(
  state: State(host),
  resolved: String,
  err: JsValue,
) -> State(host) {
  State(
    ..state,
    heap: registry.write_module_error(
      state.heap,
      state.ctx.global_object,
      resolved,
      err,
    ),
  )
}

/// Drop `resolved`'s in-flight top-level-await promise: its evaluation has
/// settled, so future imports read the namespace or error cache instead.
fn clear_pending_promise(state: State(host), resolved: String) -> State(host) {
  State(
    ..state,
    heap: registry.clear_pending_promise(
      state.heap,
      state.ctx.global_object,
      resolved,
    ),
  )
}

/// Evaluate a compiled bundle against the realm-wide module registry (the
/// hidden cache on the global object), enforcing the §16.2.1.8 module-map
/// invariant at module-record granularity:
///
///   - graph nodes already registered keep their namespace identity and are
///     NOT re-evaluated (their bodies ran in an earlier bundle);
///   - every other node's namespace is registered BEFORE evaluation, so a
///     re-entrant import() during evaluation resolves to the same record;
///   - when evaluation throws, registrations are rolled back for the nodes
///     whose bodies never completed (they may be evaluated later), while
///     completed nodes stay registered (their side effects already happened).
///
/// Used by both the dynamic-import hook and static module entry points
/// sharing a realm, so `import './a.js'` and `import('./a.js')` yield the
/// same module record.
///
/// `host_hooks` are the embedder's host capabilities (Atomics blocking
/// wait / wake delivery). Every module body boots a FRESH State, so the
/// hooks must be threaded into each body's `RealmCtx` here — the
/// dynamic-import hook passes the importing realm's `state.ctx.host_hooks`.
pub fn evaluate_bundle_with_registry(
  h: Heap(host),
  b: Builtins,
  global_object: Ref,
  bundle: module.ModuleBundle,
  host_hooks: host_hooks.HostHooks,
  can_block: Bool,
  extend_262: option.Option(state.Extend262(host)),
  finish: fn(State(host)) -> State(host),
) -> #(
  Heap(host),
  List(value.Job),
  Result(module.EvaluatedBundle, module.ModuleError),
) {
  let specs = dict.keys(bundle.modules)
  let preexisting =
    list.fold(specs, dict.new(), fn(acc, spec) {
      case registry.read_namespace(h, global_object, spec) {
        Some(ns_ref) -> dict.insert(acc, spec, ns_ref)
        None -> acc
      }
    })
  // Link + register every NEW module's namespace (and deferred namespace)
  // before any body runs.
  case link_bundle_with_registry(h, b, global_object, bundle) {
    // The heap comes back beside the result — on the error path it holds the
    // JS error object the `ModuleError` names.
    #(heap, Error(err)) -> #(heap, [], Error(err))
    #(h, Ok(linked_bundle)) -> {
      // Treat as already-evaluated exactly the modules whose bodies have
      // completed (per the heap status registry) — a registered-but-linked-
      // only module (from an earlier `import.defer()`) still needs its body
      // run when imported eagerly.
      let already_evaluated =
        list.fold(specs, set.new(), fn(acc, spec) {
          case registry.read_module_status(h, global_object, spec) {
            Some(registry.Evaluated) -> set.insert(acc, spec)
            Some(registry.Evaluating) | None -> acc
          }
        })
      let #(heap, evaluated, jobs, result) =
        module.evaluate_linked_tracking(
          linked_bundle,
          h,
          module.BootCtx(
            builtins: b,
            global_object:,
            host_hooks:,
            can_block:,
            extend_262:,
            finish:,
          ),
          already_evaluated,
        )
      case result {
        Ok(module.EvaluatedBundle(..)) -> #(heap, jobs, result)
        Error(module.EvaluationError(value:)) -> {
          // Roll back nodes whose bodies never completed — every registration
          // `link_bundle_with_registry` published for them, not just the
          // namespace (a surviving deferred namespace would hand out
          // uninitialized cells to a later `import defer`). Host (synthetic)
          // modules have no body to leave half-done and their cells are
          // permanently initialized: clearing them would strand this bundle's
          // surviving `import * as ns` bindings on a namespace object a later
          // link would no longer reuse.
          let h =
            list.fold(module.source_specifiers(bundle), heap, fn(h, spec) {
              case
                dict.has_key(preexisting, spec) || set.contains(evaluated, spec)
              {
                True -> h
                False ->
                  registry.clear_module_registrations(h, global_object, spec)
              }
            })
          #(h, jobs, Error(module.EvaluationError(value:)))
        }
        // Mid-flight on top-level await: registrations stay (a re-import
        // must resolve to the same record), and the in-flight heap is the
        // live one.
        Error(module.EvaluationPending(promise_data_ref: _))
        | Error(module.NotInBundle(..)) -> #(heap, jobs, result)
      }
    }
  }
}

/// Turn a `module.compile_bundle` failure into the JS-visible rejection value.
/// Per HostLoadImportedModule: parse failures, source-phase imports and
/// compile failures reject with a SyntaxError; a request that cannot be
/// resolved or loaded rejects with a TypeError. `CompileBundleError` carries no
/// heap, so there is no evaluation-error case here to forget about.
fn compile_bundle_rejection(
  state: State(host),
  err: module.CompileBundleError,
) -> #(State(host), Result(JsValue, JsValue)) {
  case err {
    module.GraphError(error: graph.ParseFailed(..))
    | module.GraphError(error: graph.SourcePhaseUnsupported(..))
    | module.CompileError(..) ->
      state.syntax_error(state, module.compile_bundle_error_message(err))
    module.GraphError(error: graph.ResolveFailed(..))
    | module.GraphError(error: graph.LoadFailed(..)) ->
      state.type_error(state, module.compile_bundle_error_message(err))
  }
}
