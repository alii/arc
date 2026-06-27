//// Host side of dynamic import() — HostLoadImportedModule (§16.2.1.8).
////
//// The VM's DynamicImport opcode (see arc/vm/exec/dynamic_import) performs
//// the language-level ImportCall steps and then calls a host hook installed
//// on the global object. This module provides that hook: it resolves and
//// loads the requested module source via an embedder-supplied loader,
//// compiles + links + evaluates the module graph through arc/module, and
//// returns the Module Namespace Exotic Object.
////
//// Per spec, "If this operation is called multiple times with the same
//// (referrer, moduleRequest) pair ... it must perform FinishLoadingImportedModule
//// with the same result each time": both successful namespaces and evaluation
//// errors are cached (on hidden global objects), so a module's body runs at
//// most once and repeated imports yield the identical namespace / error.

import arc/module
import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/promise as builtins_promise
import arc/vm/exec/dynamic_import
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/job_queue
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, DataProperty, JsObject, JsString, JsUndefined, Named,
  ObjectSlot, OrdinaryObject,
}
import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleam/string

/// Hidden global property: resolved specifier → cached namespace object.
const cache_property = "__arc_module_cache__"

/// Hidden global property: resolved specifier → cached evaluation error.
/// Shared with the module evaluator's heap-resident error registry (a
/// deferred-namespace trigger that hits an evaluation error caches it here,
/// and rethrows one cached by an earlier dynamic import — and vice versa).
const error_cache_property = module.error_cache_property

/// Hidden global property: resolved specifier → cached Deferred Module
/// Namespace. `import defer` / `import.defer()` of the same module must
/// yield the identical object ([[DeferredNamespace]] is per module record).
const deferred_cache_property = "__arc_module_deferred__"

/// Hidden global property: resolved specifier → in-flight namespace promise
/// for a module parked on top-level await. A re-import of an
/// ~evaluating-async~ module returns this same promise (Evaluate() step 4 —
/// [[TopLevelCapability]]); cleared when evaluation settles.
const pending_cache_property = "__arc_module_pending__"

/// Resolve a raw specifier against its referrer to the module's canonical
/// specifier — specifier math and existence probing, no source reading.
pub type ResolveFn =
  fn(String, String) -> Result(String, String)

/// Read the source of a resolved specifier.
pub type LoadFn =
  fn(String) -> Result(String, String)

/// Install the dynamic-import host hook on `global_object`. `referrer` is the
/// path of the entry script/module (relative specifiers resolve against it
/// when no module body is active); `resolve` maps specifiers to module
/// identities and `load` reads their sources — a cached import never calls
/// `load` at all. After installation, `import(specifier)` works in any code
/// evaluated against this global. The VM passes the active module's resolved
/// specifier as a second argument when one was executing at the ImportCall
/// site, which takes precedence over the install-time referrer (§16.2.1.8's
/// referencingScriptOrModule).
pub fn install_import_hook(
  h: Heap(host),
  b: Builtins,
  global_object: Ref,
  referrer: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> Heap(host) {
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
  object.define_method_property(
    h,
    global_object,
    Named(dynamic_import.hook_property),
    JsObject(hook_ref),
  )
}

/// The hook body: load, compile, link and evaluate the requested module
/// graph, returning its namespace (or the error to reject the import with).
fn import_module(
  args: List(JsValue),
  _this: JsValue,
  state: State(host),
  entry_referrer: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> #(State(host), Result(JsValue, JsValue)) {
  let specifier = case args {
    [JsString(s), ..] -> s
    _ -> ""
  }
  // The referencingScriptOrModule captured at the ImportCall site (the module
  // whose body contained the import()), else the realm's entry script.
  let referrer = case args {
    [_, JsString(r), ..] -> r
    _ -> entry_referrer
  }
  // Phase marker: `import.defer(specifier)` passes "defer" as a third
  // argument — load + link only, resolve with the Deferred Module Namespace.
  let is_defer = case args {
    [_, _, JsString("defer"), ..] -> True
    _ -> False
  }
  // The DynamicImportDefer opcode passes the import promise's resolving
  // functions as 4th/5th arguments, so the defer continuation can settle
  // AFTER pending async transitive dependencies finish evaluating.
  let capability = case args {
    [_, _, _, resolve_fn, reject_fn, ..] -> Some(#(resolve_fn, reject_fn))
    _ -> None
  }
  case resolve(specifier, referrer) {
    Error(reason) ->
      // Resolution failure → TypeError (host resolution error).
      state.type_error(
        state,
        "Cannot find module '" <> specifier <> "': " <> reason,
      )
    Ok(resolved) if is_defer ->
      defer_import_module(state, resolved, resolve, load, capability)
    Ok(resolved) ->
      // A previous import of this module settled (or is mid-evaluation) —
      // repeat the same result. The error cache wins: a namespace entry is
      // pre-published before evaluation and may be stale after a throw. The
      // pending cache (module parked on top-level await) wins over the
      // namespace cache: per Evaluate() step 4 a re-import returns the same
      // in-flight top-level promise instead of re-running the body.
      case read_cached(state, error_cache_property, resolved) {
        Some(error) -> #(state, Error(error))
        None ->
          case read_cached(state, pending_cache_property, resolved) {
            Some(JsObject(_) as pending_promise) -> #(
              state,
              Ok(pending_promise),
            )
            Some(_) | None -> {
              // A registered namespace alone is not enough: linking (e.g. an
              // earlier `import.defer()`) registers namespaces WITHOUT
              // evaluating. Only short-circuit when the module's body has
              // completed ("evaluated") or is mid-run ("evaluating" — the
              // re-entrant import case, which must not re-run the body).
              let status =
                module.evaluation_status(
                  state.heap,
                  state.ctx.global_object,
                  resolved,
                )
              case read_cached(state, cache_property, resolved), status {
                Some(JsObject(_) as namespace), Some(_) -> #(
                  state,
                  Ok(namespace),
                )
                _, _ -> evaluate_module(state, resolved, resolve, load)
              }
            }
          }
      }
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
    Error(module.ParseError(msg)) -> syntax_error(state, msg)
    Error(module.CompileError(msg)) -> syntax_error(state, msg)
    Error(module.ResolutionError(msg)) -> state.type_error(state, msg)
    Error(module.LinkError(msg)) -> syntax_error(state, msg)
    Error(module.EvaluationError(value: thrown, heap:)) -> #(
      State(..state, heap:),
      Error(thrown),
    )
    Error(module.EvaluationPending(promise_data_ref: _, heap:)) ->
      // compile_bundle never evaluates — unreachable, but keep the heap.
      state.type_error(
        State(..state, heap:),
        "Failed to compile module '" <> resolved <> "'",
      )
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
          fn(module_state) { module_state },
        )
      let state =
        State(
          ..state,
          heap: h,
          job_queue: job_queue.append(state.job_queue, jobs),
        )
      case result {
        Ok(module.EvaluatedBundle(value: _, namespace: Some(namespace), ..)) -> #(
          state,
          Ok(namespace),
        )
        Ok(module.EvaluatedBundle(value: _, namespace: None, ..)) ->
          state.type_error(
            state,
            "Module '" <> resolved <> "' produced no namespace",
          )
        Error(module.EvaluationError(value: thrown, heap: _)) -> {
          // Repeat the same rejection on every future import of this entry.
          let state =
            write_cached(state, error_cache_property, resolved, thrown)
          #(state, Error(thrown))
        }
        Error(module.EvaluationPending(promise_data_ref:, heap: _)) ->
          pending_module_promise(state, resolved, promise_data_ref)
        Error(other) ->
          state.type_error(
            state,
            "Failed to evaluate module '"
              <> resolved
              <> "': "
              <> string.inspect(other),
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
fn defer_import_module(
  state: State(host),
  resolved: String,
  resolve: ResolveFn,
  load: LoadFn,
  capability: option.Option(#(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  // An already-failed module repeats the same rejection; an already
  // registered deferred namespace is returned as-is.
  case read_cached(state, error_cache_property, resolved) {
    Some(error) -> #(state, Error(error))
    None ->
      case read_cached(state, deferred_cache_property, resolved) {
        Some(JsObject(_) as deferred_ns) -> #(state, Ok(deferred_ns))
        Some(_) | None -> {
          use source <- with_loaded_source(state, resolved, load)
          case module.compile_bundle(resolved, source, resolve, load) {
            Error(module.ParseError(msg)) -> syntax_error(state, msg)
            Error(module.CompileError(msg)) -> syntax_error(state, msg)
            Error(module.ResolutionError(msg)) -> state.type_error(state, msg)
            Error(module.LinkError(msg)) -> syntax_error(state, msg)
            Error(module.EvaluationError(value: thrown, heap:)) -> #(
              State(..state, heap:),
              Error(thrown),
            )
            Error(module.EvaluationPending(promise_data_ref: _, heap:)) ->
              // compile_bundle never evaluates — unreachable, keep the heap.
              state.type_error(
                State(..state, heap:),
                "Failed to compile module '" <> resolved <> "'",
              )
            Ok(bundle) ->
              case
                link_bundle_with_registry(
                  state.heap,
                  state.builtins,
                  state.ctx.global_object,
                  bundle,
                )
              {
                Error(#(h, module.EvaluationError(value: thrown, ..))) -> #(
                  State(..state, heap: h),
                  Error(thrown),
                )
                Error(#(h, other)) ->
                  state.type_error(
                    State(..state, heap: h),
                    "Failed to link module '"
                      <> resolved
                      <> "': "
                      <> string.inspect(other),
                  )
                Ok(#(h, linked_bundle)) -> {
                  let #(h, deferred_ns) =
                    module.get_or_create_deferred_namespace(
                      h,
                      state.builtins,
                      linked_bundle,
                      resolved,
                    )
                  let state = State(..state, heap: h)
                  case deferred_ns {
                    Some(JsObject(_) as ns) -> {
                      let state =
                        write_cached(
                          state,
                          deferred_cache_property,
                          resolved,
                          ns,
                        )
                      evaluate_deferred_async_deps(
                        state,
                        resolved,
                        ns,
                        linked_bundle,
                        capability,
                      )
                    }
                    Some(_) | None ->
                      state.type_error(
                        state,
                        "Module '"
                          <> resolved
                          <> "' produced no deferred namespace",
                      )
                  }
                }
              }
          }
        }
      }
  }
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
    Error(reason) ->
      state.type_error(
        state,
        "Cannot load module '" <> resolved <> "': " <> reason,
      )
    Ok(source) -> then(source)
  }
}

/// ContinueDynamicImport's ~defer~ arm, after linking: evaluate the entry's
/// asynchronous transitive dependencies; resolve the import promise with the
/// deferred namespace only after their top-level promises settle.
fn evaluate_deferred_async_deps(
  state: State(host),
  resolved: String,
  ns: JsValue,
  linked_bundle: module.LinkedBundle,
  capability: option.Option(#(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  // As in evaluate_module: never drain a nested event loop here — bodies run
  // with an identity driver and any jobs they enqueued (including the parked
  // top-level-await continuations) are appended to the host's queue.
  let #(h, jobs, dep_result) =
    module.evaluate_async_transitive_deps(
      linked_bundle,
      state.heap,
      state.builtins,
      state.ctx.global_object,
      state.ctx.host_hooks,
      fn(module_state) { module_state },
    )
  let state =
    State(..state, heap: h, job_queue: job_queue.append(state.job_queue, jobs))
  case dep_result {
    // No async dependency parked on top-level await — resolve immediately.
    Ok([]) -> #(state, Ok(ns))
    Ok(pendings) ->
      case capability {
        Some(#(resolve_fn, reject_fn)) -> {
          let state =
            chain_deferred_settlement(
              state,
              ns,
              pendings,
              resolve_fn,
              reject_fn,
            )
          // The reactions above settle the import promise; tell the import
          // job not to.
          #(state, Ok(JsString(dynamic_import.settled_marker)))
        }
        // No resolving functions (hook not invoked via the
        // DynamicImportDefer opcode) — reject rather than resolve before the
        // async dependencies finished evaluating.
        None ->
          state.type_error(
            state,
            "import.defer('"
              <> resolved
              <> "'): missing promise capability for pending async dependencies",
          )
      }
    Error(module.EvaluationError(value: thrown, heap: _)) -> {
      // Repeat the same rejection on every future import of this entry.
      let state = write_cached(state, error_cache_property, resolved, thrown)
      #(state, Error(thrown))
    }
    Error(other) ->
      state.type_error(
        state,
        "Failed to evaluate async dependencies of module '"
          <> resolved
          <> "': "
          <> string.inspect(other),
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
              module.record_module_evaluated(
                state.heap,
                state.ctx.global_object,
                dep_spec,
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
            let heap =
              module.record_module_error(
                state.heap,
                state.ctx.global_object,
                dep_spec,
                reason,
              )
            let state =
              call_import_settle_fn(State(..state, heap:), reject_fn, reason)
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
fn link_bundle_with_registry(
  h: Heap(host),
  b: Builtins,
  global_object: Ref,
  bundle: module.ModuleBundle,
) -> Result(
  #(Heap(host), module.LinkedBundle),
  #(Heap(host), module.ModuleError(host)),
) {
  let specs = dict.keys(bundle.modules)
  let preexisting =
    list.fold(specs, dict.new(), fn(acc, spec) {
      case read_cached_heap(h, global_object, cache_property, spec) {
        Some(JsObject(ns_ref)) -> dict.insert(acc, spec, ns_ref)
        Some(_) | None -> acc
      }
    })
  let preexisting_deferred =
    list.fold(specs, dict.new(), fn(acc, spec) {
      case read_cached_heap(h, global_object, deferred_cache_property, spec) {
        Some(JsObject(ns_ref)) -> dict.insert(acc, spec, ns_ref)
        Some(_) | None -> acc
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
    Error(module.EvaluationError(heap:, ..) as err) -> Error(#(heap, err))
    Error(other) -> Error(#(h, other))
    Ok(#(h, linked_bundle)) -> {
      let h =
        list.fold(module.linked_namespaces(linked_bundle, h), h, fn(h, pair) {
          let #(spec, ns) = pair
          case dict.has_key(preexisting, spec) {
            True -> h
            False ->
              write_cached_heap(h, global_object, cache_property, spec, ns)
          }
        })
      let h =
        list.fold(
          module.linked_deferred_namespaces(linked_bundle, h),
          h,
          fn(h, pair) {
            let #(spec, ns) = pair
            case dict.has_key(preexisting_deferred, spec) {
              True -> h
              False ->
                write_cached_heap(
                  h,
                  global_object,
                  deferred_cache_property,
                  spec,
                  ns,
                )
            }
          },
        )
      Ok(#(h, linked_bundle))
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
  case read_cached(state, cache_property, resolved) {
    Some(JsObject(_) as namespace) -> {
      let #(heap, ns_promise_ref, ns_data_ref) =
        builtins_promise.create_promise(
          state.heap,
          state.builtins.promise.prototype,
        )
      let #(heap, ns_resolve, ns_reject) =
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
            let state =
              write_cached(state, pending_cache_property, resolved, JsUndefined)
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
            let state =
              write_cached(state, pending_cache_property, resolved, JsUndefined)
            let state =
              write_cached(state, error_cache_property, resolved, reason)
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
        write_cached(
          state,
          pending_cache_property,
          resolved,
          JsObject(ns_promise_ref),
        )
      #(state, Ok(JsObject(ns_promise_ref)))
    }
    other -> {
      let state = case other {
        Some(unexpected) -> {
          echo_unexpected_pending_namespace(resolved, unexpected)
          state
        }
        None -> state
      }
      state.type_error(
        state,
        "Module '" <> resolved <> "' produced no namespace",
      )
    }
  }
}

/// Log an unexpected non-object namespace cache entry for a pending module —
/// indicates registry corruption, never expected in normal operation.
fn echo_unexpected_pending_namespace(resolved: String, val: JsValue) -> Nil {
  io.println_error(
    "arc: pending module '"
    <> resolved
    <> "' has non-object namespace cache entry: "
    <> string.inspect(val),
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
  host_hooks: state.HostHooks,
  finish: fn(State(host)) -> State(host),
) -> #(
  Heap(host),
  List(value.Job),
  Result(module.EvaluatedBundle(host), module.ModuleError(host)),
) {
  let specs = dict.keys(bundle.modules)
  let preexisting =
    list.fold(specs, dict.new(), fn(acc, spec) {
      case read_cached_heap(h, global_object, cache_property, spec) {
        Some(JsObject(ns_ref)) -> dict.insert(acc, spec, ns_ref)
        Some(_) | None -> acc
      }
    })
  // Link + register every NEW module's namespace (and deferred namespace)
  // before any body runs.
  case link_bundle_with_registry(h, b, global_object, bundle) {
    // Link errors carry the heap the error object was allocated in.
    Error(#(heap, err)) -> #(heap, [], Error(err))
    Ok(#(h, linked_bundle)) -> {
      // Treat as already-evaluated exactly the modules whose bodies have
      // completed (per the heap status registry) — a registered-but-linked-
      // only module (from an earlier `import.defer()`) still needs its body
      // run when imported eagerly.
      let already_evaluated =
        list.fold(specs, set.new(), fn(acc, spec) {
          case module.evaluation_status(h, global_object, spec) {
            Some("evaluated") -> set.insert(acc, spec)
            _ -> acc
          }
        })
      let #(evaluated, jobs, result) =
        module.evaluate_linked_tracking(
          linked_bundle,
          h,
          b,
          global_object,
          host_hooks,
          finish,
          already_evaluated,
        )
      case result {
        Ok(module.EvaluatedBundle(heap:, ..)) -> #(heap, jobs, result)
        Error(module.EvaluationError(value:, heap:)) -> {
          // Roll back nodes whose bodies never completed.
          let h =
            list.fold(specs, heap, fn(h, spec) {
              case
                dict.has_key(preexisting, spec) || set.contains(evaluated, spec)
              {
                True -> h
                False ->
                  write_cached_heap(
                    h,
                    global_object,
                    cache_property,
                    spec,
                    JsUndefined,
                  )
              }
            })
          #(h, jobs, Error(module.EvaluationError(value:, heap: h)))
        }
        // Mid-flight on top-level await: registrations stay (a re-import
        // must resolve to the same record), and the in-flight heap is the
        // live one.
        Error(module.EvaluationPending(promise_data_ref: _, heap:)) -> #(
          heap,
          jobs,
          result,
        )
        Error(_) -> #(h, jobs, result)
      }
    }
  }
}

/// Allocate a SyntaxError (with stack) and return it as the rejection reason.
fn syntax_error(
  state: State(host),
  msg: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(heap, err) = common.make_syntax_error(state.heap, state.builtins, msg)
  let state =
    state.attach_stack(
      State(..state, heap:),
      err,
      state.error_header("SyntaxError", msg),
    )
  #(state, Error(err))
}

/// Read `key` off the hidden cache object `property` on the global, if both exist.
fn read_cached(
  state: State(host),
  property: String,
  key: String,
) -> option.Option(JsValue) {
  read_cached_heap(state.heap, state.ctx.global_object, property, key)
}

fn read_cached_heap(
  h: Heap(host),
  global_object: Ref,
  property: String,
  key: String,
) -> option.Option(JsValue) {
  case object.get_own_property(h, global_object, Named(property)) {
    Some(DataProperty(value: JsObject(cache_ref), ..)) ->
      case object.get_own_property(h, cache_ref, Named(key)) {
        Some(DataProperty(value: cached, ..)) -> Some(cached)
        _ -> None
      }
    _ -> None
  }
}

/// Write `key` → `val` into the hidden cache object `property` on the global,
/// creating the cache object on first use.
fn write_cached(
  state: State(host),
  property: String,
  key: String,
  val: JsValue,
) -> State(host) {
  let h =
    write_cached_heap(state.heap, state.ctx.global_object, property, key, val)
  State(..state, heap: h)
}

fn write_cached_heap(
  h: Heap(host),
  global_object: Ref,
  property: String,
  key: String,
  val: JsValue,
) -> Heap(host) {
  let #(h, cache_ref) = case
    object.get_own_property(h, global_object, Named(property))
  {
    Some(DataProperty(value: JsObject(cache_ref), ..)) -> #(h, cache_ref)
    _ -> {
      let #(h, cache_ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: elements.new(),
            prototype: None,
            symbol_properties: [],
            extensible: True,
          ),
        )
      let h =
        object.define_method_property(
          h,
          global_object,
          Named(property),
          JsObject(cache_ref),
        )
      #(h, cache_ref)
    }
  }
  let #(h, _) = object.set_property(h, cache_ref, Named(key), val)
  h
}
