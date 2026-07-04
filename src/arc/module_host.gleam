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
import arc/module/registry
import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/promise as builtins_promise
import arc/vm/exec/dynamic_import
import arc/vm/heap
import arc/vm/internal/job_queue
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{type JsValue, type Ref, JsObject, JsString, JsUndefined}
import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set
import gleam/string

// The realm-wide module caches (namespaces, deferred namespaces, evaluation
// errors, pending top-level-await promises, statuses) live on hidden global
// objects behind the typed accessors of `arc/module/registry` — shared with
// the module evaluator so a deferred-namespace trigger and a dynamic import
// observe the same state.

/// Why an embedder's loader could not produce a module. A CATEGORY, never a
/// pre-worded message: the wording lives in `module_load_error_message`, and
/// because failure has its own type a loader can no longer return its error
/// text as an `Ok` value.
pub type ModuleLoadError {
  /// Nothing exists at this resolved specifier.
  NotFound(specifier: String)
  /// This graph is not allowed to import anything.
  ImportsForbidden(specifier: String)
  /// The module exists but its source could not be read (permissions, it is a
  /// directory, an I/O error). `reason` is the host's own description.
  ReadFailed(specifier: String, reason: String)
  /// A raw specifier could not be mapped to a module identity.
  ResolveFailed(raw: String, referrer: String)
  /// A bare specifier ("fs", a URL) this loader gives no meaning to — NOT a
  /// missing file: no path was ever probed.
  UnsupportedBareSpecifier(specifier: String)
}

/// The ONE place a `ModuleLoadError` is worded. Every JS-visible resolve/load
/// rejection message is rendered here, so a loader can no longer invent a
/// category — a self-contradicting "file not found: ./lib (is a directory)"
/// is unrepresentable.
pub fn module_load_error_message(error: ModuleLoadError) -> String {
  case error {
    NotFound(specifier:) -> "Cannot find module '" <> specifier <> "'"
    ImportsForbidden(specifier:) ->
      "Cannot import '" <> specifier <> "': imports are not allowed here"
    ReadFailed(specifier:, reason:) ->
      "Cannot read module '" <> specifier <> "': " <> reason
    ResolveFailed(raw:, referrer:) ->
      "Cannot resolve module '" <> raw <> "' from '" <> referrer <> "'"
    UnsupportedBareSpecifier(specifier:) ->
      "Cannot resolve bare specifier '"
      <> specifier
      <> "': this loader resolves paths only"
  }
}

/// Resolve a raw specifier against its referrer to the module's canonical
/// specifier — specifier math and existence probing, no source reading.
pub type ResolveFn =
  fn(String, String) -> Result(String, ModuleLoadError)

/// Read the source of a resolved specifier.
pub type LoadFn =
  fn(String) -> Result(String, ModuleLoadError)

/// Adapt a typed resolver to the plain-string callback the graph walk takes:
/// that `String` error is an ALREADY-RENDERED message (`graph.ResolveFailed`
/// only ever carries text to print), never a category to re-inspect.
pub fn rendered_resolve(
  resolve: ResolveFn,
) -> fn(String, String) -> Result(String, String) {
  fn(raw, referrer) {
    resolve(raw, referrer) |> result.map_error(module_load_error_message)
  }
}

/// Adapt a typed loader to the plain-string callback the graph walk takes —
/// see `rendered_resolve`.
pub fn rendered_load(load: LoadFn) -> fn(String) -> Result(String, String) {
  fn(specifier) {
    load(specifier) |> result.map_error(module_load_error_message)
  }
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

/// One dynamic-import hook invocation, parsed EXACTLY once from the
/// positional argument list the VM's DynamicImport / DynamicImportDefer
/// opcodes build (see `arc/vm/exec/dynamic_import`). Existence of a value of
/// this type means the call was well-formed: the specifier is a real string
/// (never a silently-defaulted `""`), and a `Defer` phase always carries both
/// resolving functions.
type ImportCall {
  ImportCall(specifier: String, referrer: String, phase: ImportPhase)
}

/// Which HostLoadImportedModule phase was requested.
type ImportPhase {
  /// `import(specifier)`: load, link and EVALUATE the graph. The hook's
  /// return value (the namespace, or an in-flight namespace promise) settles
  /// the import promise via the standard reaction machinery.
  Eager
  /// `import.defer(specifier)` (tc39/proposal-defer-import-eval): load and
  /// LINK only. The hook OWNS the import promise's settlement through these
  /// resolving functions (see `dynamic_import.DeferHookOutcome`) and its
  /// return value carries nothing. The constructor requires BOTH functions:
  /// a defer call without a capability is unrepresentable.
  Defer(resolve_fn: JsValue, reject_fn: JsValue)
}

/// Parse the hook's positional argument list into an `ImportCall`, or the
/// TypeError message to reject the import promise with. This is the ONLY
/// place the arg-list encoding is read; a missing or non-string specifier,
/// or a defer phase without its promise capability, is an immediate
/// rejection — never a silent fallback.
fn parse_import_call(
  args: List(JsValue),
  entry_referrer: String,
) -> Result(ImportCall, String) {
  let defer_marker = dynamic_import.defer_phase_marker
  case args {
    [] -> Error("import hook called without a specifier")
    [JsString(specifier), ..rest] -> {
      // The referencingScriptOrModule captured at the ImportCall site (the
      // module whose body contained the import()); a script-level import()
      // has no active module and falls back to the realm's entry referrer.
      let referrer = case rest {
        [JsString(referrer), ..] -> referrer
        _ -> entry_referrer
      }
      case rest {
        // Eager: at most the referrer follows the specifier.
        [] | [_] -> Ok(ImportCall(specifier:, referrer:, phase: Eager))
        // Defer: the phase marker AND both resolving functions, together.
        [_, JsString(phase), resolve_fn, reject_fn] if phase == defer_marker ->
          Ok(ImportCall(
            specifier:,
            referrer:,
            phase: Defer(resolve_fn:, reject_fn:),
          ))
        [_, JsString(phase), ..] if phase == defer_marker ->
          Error(
            "import hook called with the defer phase but no promise capability",
          )
        _ -> Error("import hook called with unexpected arguments")
      }
    }
    [_, ..] -> Error("import hook called with a non-string specifier")
  }
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
  case parse_import_call(args, entry_referrer) {
    Error(message) -> state.type_error(state, message)
    Ok(ImportCall(specifier:, referrer:, phase:)) ->
      case resolve(specifier, referrer) {
        Error(load_error) ->
          // Resolution failure → TypeError (host resolution error).
          state.type_error(state, module_load_error_message(load_error))
        Ok(resolved) ->
          case phase {
            Defer(resolve_fn:, reject_fn:) ->
              defer_import_module(
                state,
                resolved,
                resolve,
                load,
                resolve_fn,
                reject_fn,
              )
            Eager -> eager_import_module(state, resolved, resolve, load)
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
  // A previous import of this module settled (or is mid-evaluation) —
  // repeat the same result. The error cache wins: a namespace entry is
  // pre-published before evaluation and may be stale after a throw. The
  // pending cache (module parked on top-level await) wins over the
  // namespace cache: per Evaluate() step 4 a re-import returns the same
  // in-flight top-level promise instead of re-running the body.
  case
    registry.read_module_error(state.heap, state.ctx.global_object, resolved)
  {
    Some(error) -> #(state, Error(error))
    None ->
      case
        registry.read_pending_promise(
          state.heap,
          state.ctx.global_object,
          resolved,
        )
      {
        Some(pending_promise_ref) -> #(state, Ok(JsObject(pending_promise_ref)))
        None -> {
          // A registered namespace alone is not enough: linking (e.g. an
          // earlier `import.defer()`) registers namespaces WITHOUT
          // evaluating. Only short-circuit when the module's body has
          // completed (`Evaluated`) or is mid-run (`Evaluating` — the
          // re-entrant import case, which must not re-run the body).
          let status =
            registry.read_module_status(
              state.heap,
              state.ctx.global_object,
              resolved,
            )
          let namespace =
            registry.read_namespace(
              state.heap,
              state.ctx.global_object,
              resolved,
            )
          case namespace, status {
            Some(ns_ref), Some(_) -> #(state, Ok(JsObject(ns_ref)))
            _, _ -> evaluate_module(state, resolved, resolve, load)
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
  case
    module.compile_bundle(
      resolved,
      source,
      rendered_resolve(resolve),
      rendered_load(load),
    )
  {
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
          fn(module_state) { module_state },
        )
      let state =
        State(
          ..state,
          heap: h,
          job_queue: job_queue.append(state.job_queue, jobs),
        )
      case result {
        Ok(module.EvaluatedBundle(value: _, namespace: Some(ns_ref), ..)) -> #(
          state,
          Ok(JsObject(ns_ref)),
        )
        Ok(module.EvaluatedBundle(value: _, namespace: None, ..)) ->
          state.type_error(
            state,
            "Module '" <> resolved <> "' produced no namespace",
          )
        Error(module.EvaluationError(value: thrown, heap: _)) -> {
          // Repeat the same rejection on every future import of this entry.
          let state = cache_module_error(state, resolved, thrown)
          #(state, Error(thrown))
        }
        Error(module.EvaluationPending(promise_data_ref:, heap: _)) ->
          pending_module_promise(state, resolved, promise_data_ref)
        Error(module.NotInBundle(..) as other) ->
          state.type_error(
            state,
            "Failed to evaluate module '"
              <> resolved
              <> "': "
              <> module.error_message(other),
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
  // An already-failed module repeats the same rejection; an already
  // registered deferred namespace is resolved with as-is.
  case
    registry.read_module_error(state.heap, state.ctx.global_object, resolved)
  {
    Some(error) -> #(state, Error(error))
    None ->
      case
        registry.read_deferred_namespace(
          state.heap,
          state.ctx.global_object,
          resolved,
        )
      {
        Some(deferred_ns_ref) ->
          settle_defer_import(state, resolve_fn, JsObject(deferred_ns_ref))
        None -> {
          use source <- with_loaded_source(state, resolved, load)
          case
            module.compile_bundle(
              resolved,
              source,
              rendered_resolve(resolve),
              rendered_load(load),
            )
          {
            Error(err) -> compile_bundle_rejection(state, err)
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
                      <> module.error_message(other),
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
                    Error(module.DeferredNamespaceBoxCorrupt(specifier:)) ->
                      state.type_error(
                        state,
                        "Module '"
                          <> specifier
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
    Error(load_error) ->
      state.type_error(state, module_load_error_message(load_error))
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
      state.builtins,
      state.ctx.global_object,
      state.ctx.host_hooks,
      fn(module_state) { module_state },
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
    Error(module.EvaluationError(value: thrown, heap: _)) -> {
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
          <> module.error_message(other),
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
    Error(module.EvaluationError(heap:, ..) as err) -> Error(#(heap, err))
    Error(other) -> Error(#(h, other))
    Ok(#(h, linked_bundle)) -> {
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
  case registry.read_namespace(state.heap, state.ctx.global_object, resolved) {
    Some(namespace_ref) -> {
      let namespace = JsObject(namespace_ref)
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
      case registry.read_namespace(h, global_object, spec) {
        Some(ns_ref) -> dict.insert(acc, spec, ns_ref)
        None -> acc
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
          case registry.read_module_status(h, global_object, spec) {
            Some(registry.Evaluated) -> set.insert(acc, spec)
            Some(registry.Evaluating) | None -> acc
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
        Error(module.NotInBundle(..)) -> #(h, jobs, result)
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
