//// §13.3.10 Import Calls — runtime semantics for the DynamicImport opcode.
////
//// Implements EvaluateImportCall (§13.3.10.1) from the point where the
//// specifier and options values have already been evaluated (the compiler
//// pushes both before emitting DynamicImport):
////
////   4.  Let promiseCapability be ! NewPromiseCapability(%Promise%).
////   5.  Let specifierString be Completion(ToString(specifier)).
////   6.  IfAbruptRejectPromise(specifierString, promiseCapability).
////   7.  Let attributes be a new empty List.
////   8.  If options is not undefined:
////       a. If options is not an Object, reject with TypeError.
////       b. Let attributesObj be Completion(Get(options, "with")).
////       c. IfAbruptRejectPromise(attributesObj, promiseCapability).
////       d. If attributesObj is not undefined:
////          i.  If attributesObj is not an Object, reject with TypeError.
////          ii. Let entries be Completion(EnumerableOwnProperties(...)).
////          iii. IfAbruptRejectPromise(entries, promiseCapability).
////          iv. Every attribute value must be a String, else TypeError.
////       e. If AllImportAttributesSupported(attributes) is false, reject
////          with TypeError (this host supports no import attributes).
////   9+. HostLoadImportedModule / ContinueDynamicImport — delegated to the
////       embedder's import hook, a host function carried as ENGINE state on
////       `state.ctx.host_hooks.import_hook` (installed by
////       arc/module_host.install_import_hook, never visible to guest JS).
////       The hook returns the module namespace object or throws; the promise
////       is resolved or rejected accordingly. Without a hook, import()
////       rejects with a TypeError (dynamic import unsupported in this
////       context).
////
//// All failures after argument evaluation reject the returned promise —
//// nothing here throws synchronously (IfAbruptRejectPromise semantics).
//// Cross-referenced with QuickJS js_dynamic_import / engine262 ImportCall.
////
//// The VM↔host channel is deliberately NOT smuggled through globalThis
//// properties: the hook, the active referrer, and the ~defer~ "already
//// settled" signal all used to live in guest-reachable strings/properties,
//// which let user JS replace the module loader or forge its resolution root.
//// They are now typed engine state (`HostHooks.import_hook` /
//// `HostHooks.import_referrer`) and a typed job outcome (`DeferHookOutcome`).

import arc/vm/builtins/common
import arc/vm/builtins/object as builtins_object
import arc/vm/builtins/promise as builtins_promise
import arc/vm/exec/promises
import arc/vm/internal/job_queue
import arc/vm/key.{Named}
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type State, type StepResult, State}
import arc/vm/value.{type JsValue, type Ref, JsObject, JsString, JsUndefined}
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

/// Positional phase marker (3rd hook argument) that `import.defer(spec)`
/// passes to the host hook: load + LINK the graph without evaluating it, and
/// settle the import promise with the Deferred Module Namespace. This is a
/// private VM↔host argument encoding — the hook is engine state that guest JS
/// can never call, so the value is not forgeable from JS.
pub const defer_phase_marker = "defer"

/// What the ~defer~ arm of the host hook did with the import promise's
/// capability. The hook is handed the promise's resolving functions and — on
/// any NORMAL return — OWNS settlement: it settles immediately, or wires
/// reactions onto async transitive-dependency evaluation promises that settle
/// later. The import job's only remaining duty is rejecting on an ABRUPT
/// completion (no hook installed, or the hook threw before taking ownership).
/// Encoding this in a type (instead of the old in-band
/// `"__arc_import_settled__"` string returned as a JsValue) makes "the job
/// double-settled" and "a hook return was mistaken for a namespace"
/// unrepresentable, and removes a guest-forgeable sentinel from the protocol.
pub type DeferHookOutcome {
  /// Normal hook return: it took ownership of the capability (already
  /// settled the import promise, or wired the reactions that will). Do
  /// nothing.
  DeferHookSettledCapability
  /// Abrupt completion: no `import_hook` is installed, or the hook threw
  /// before taking ownership of the capability. Reject the import promise
  /// with this value.
  DeferHookRejected(reason: JsValue)
}

/// Run the DynamicImport opcode: `specifier` and `options` were popped off the
/// stack by the caller; pushes the import promise onto `rest_stack`.
pub fn evaluate_import_call(
  state: State(host),
  specifier: JsValue,
  options: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  // Step 4: NewPromiseCapability(%Promise%).
  let #(heap, promise_ref, data_ref) =
    builtins_promise.create_promise(
      state.heap,
      state.builtins.promise.prototype,
    )
  let state = State(..state, heap:)
  // Steps 5-8 run synchronously; an abrupt completion rejects the promise
  // (IfAbruptRejectPromise). Step 9+ (HostLoadImportedModule) is deferred to
  // a job so the module graph loads/evaluates AFTER the currently running
  // synchronous code completes and after previously enqueued microtasks —
  // matching ContinueDynamicImport's promise-job scheduling (cf. QuickJS
  // js_dynamic_import_job).
  let state = case import_request(state, specifier, options) {
    Error(#(reason, state)) ->
      builtins_promise.reject_promise(state, data_ref, reason)
    Ok(#(specifier_string, state)) -> {
      // §16.2.1.8 referencingScriptOrModule, captured synchronously at
      // ImportCall time: the job may only run after the current module body
      // finishes and a differently-referred State is executing.
      let referrer_args =
        capture_referrer(state)
        |> option.map(fn(referrer) { [JsString(referrer)] })
        |> option.unwrap([])
      enqueue_import_job(state, promise_ref, data_ref, fn(state) {
        call_host_hook(state, specifier_string, referrer_args)
      })
    }
  }
  promises.return_promise(state, promise_ref, rest_stack)
}

/// Run the DynamicImportDefer opcode (`import.defer(specifier)`, the
/// defer-import-eval proposal). As `import()`, but the host hook is invoked
/// with the phase marker "defer" as a third argument: it loads and LINKS the
/// module graph without evaluating it, and resolves the promise with the
/// Deferred Module Namespace (whose first relevant access triggers
/// evaluation).
pub fn evaluate_defer_import_call(
  state: State(host),
  specifier: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  let #(heap, promise_ref, data_ref) =
    builtins_promise.create_promise(
      state.heap,
      state.builtins.promise.prototype,
    )
  let state = State(..state, heap:)
  let state = case import_request(state, specifier, JsUndefined) {
    Error(#(reason, state)) ->
      builtins_promise.reject_promise(state, data_ref, reason)
    Ok(#(specifier_string, state)) -> {
      // ContinueDynamicImport's ~defer~ arm chains the import promise's
      // settlement onto async-dependency evaluation promises via
      // PerformPromiseThen (the proposal's SafePerformPromiseAll never looks
      // up `then`) — hand the hook the resolving functions (4th/5th
      // arguments) so it can settle later without routing an intermediate
      // promise through an observable thenable adoption.
      let #(heap, resolve_fn, reject_fn) =
        builtins_promise.create_resolving_functions(
          state.heap,
          state.builtins.function.prototype,
          promise_ref,
          data_ref,
        )
      let state = State(..state, heap:)
      // The phase marker is positional (3rd argument), so a missing referrer
      // is padded with undefined.
      let referrer_arg =
        capture_referrer(state)
        |> option.map(JsString)
        |> option.unwrap(JsUndefined)
      let hook_args = [
        referrer_arg,
        JsString(defer_phase_marker),
        resolve_fn,
        reject_fn,
      ]
      enqueue_defer_import_job(state, reject_fn, fn(state) {
        call_defer_host_hook(state, specifier_string, hook_args)
      })
    }
  }
  promises.return_promise(state, promise_ref, rest_stack)
}

/// Run the DynamicImportSource opcode (`import.source(specifier)`, the
/// source-phase-imports proposal). Steps 5-7 of EvaluateImportCall run as for
/// `import()` — an abrupt ToString rejects the promise with the thrown value.
/// A coercible specifier names a Source Text Module Record, and
/// GetModuleSource for those always throws a SyntaxError (§16.2.1.7.2), so
/// the promise rejects with a SyntaxError from a job (matching
/// ContinueDynamicImport's scheduling). This host never loads the module
/// first: it has no module kinds with a source phase representation, so the
/// outcome for a loadable module is always the same SyntaxError.
pub fn evaluate_source_import_call(
  state: State(host),
  specifier: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  let #(heap, promise_ref, data_ref) =
    builtins_promise.create_promise(
      state.heap,
      state.builtins.promise.prototype,
    )
  let state = State(..state, heap:)
  let state = case import_request(state, specifier, JsUndefined) {
    Error(#(reason, state)) ->
      builtins_promise.reject_promise(state, data_ref, reason)
    Ok(#(_specifier_string, state)) ->
      enqueue_import_job(state, promise_ref, data_ref, fn(state) {
        let #(err, state) =
          state.syntax_error_value(
            state,
            "Module has no source phase representation",
          )
        #(state, Error(err))
      })
  }
  promises.return_promise(state, promise_ref, rest_stack)
}

/// Steps 5-8: coerce the specifier and validate options. `Error(reason)`
/// rejects the import promise with `reason`.
fn import_request(
  state: State(host),
  specifier: JsValue,
  options: JsValue,
) -> Result(#(String, State(host)), #(JsValue, State(host))) {
  // Steps 5-6: specifierString = ToString(specifier), abrupt → reject.
  use #(specifier_string, state) <- result.try(coerce.js_to_string(
    state,
    specifier,
  ))
  use state <- result.map(validate_options(state, options))
  #(specifier_string, state)
}

/// §16.2.1.8 HostLoadImportedModule referrer: the resolved specifier of the
/// module whose body is currently executing, carried as ENGINE state on the
/// realm's host hooks (set on each module body's freshly booted State by
/// `arc/module.run_module_with_referrer`). Captured at ImportCall time (the
/// spec captures referencingScriptOrModule synchronously) — the import job
/// may run after the current module body finishes. `None` = script code.
fn capture_referrer(state: State(host)) -> option.Option(String) {
  state.ctx.host_hooks.import_referrer
}

/// Step 9+ deferred: enqueue a promise job that runs `settle` (loading the
/// module, or producing the phase-specific result) and settles the import
/// promise with its outcome.
fn enqueue_import_job(
  state: State(host),
  promise_ref: Ref,
  data_ref: Ref,
  settle: fn(State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> State(host) {
  let #(heap, resolve_fn, reject_fn) =
    builtins_promise.create_resolving_functions(
      state.heap,
      state.builtins.function.prototype,
      promise_ref,
      data_ref,
    )
  let #(heap, job_fn) =
    common.alloc_host_fn(
      heap,
      state.builtins.function.prototype,
      fn(_args, _this, state) { settle(state) },
      "%ContinueDynamicImport%",
      0,
    )
  // The standard resolving function (§27.2.1.3.2) adopts a thenable
  // namespace (an exported callable `then`) per spec, so plain
  // PromiseReactionJob resolution is sufficient here.
  let job =
    value.PromiseReactionJob(
      handler: value.Handler(JsObject(job_fn)),
      arg: JsUndefined,
      resolve: resolve_fn,
      reject: reject_fn,
    )
  State(..state, heap:, job_queue: job_queue.push(state.job_queue, job))
}

/// As `enqueue_import_job`, but for the ~defer~ phase, whose host hook is
/// handed the import promise's resolving functions and OWNS settlement (see
/// `DeferHookOutcome`). The job therefore never resolves the promise itself —
/// it only rejects when the hook was never entered or threw before taking
/// ownership. This replaces the old in-band `settled_marker` string the hook
/// returned to say "don't also settle": the "hook settled it" case is now a
/// distinct constructor, so forgetting it (or a hook return colliding with
/// the sentinel) is a compile error instead of a hung or double-settled
/// promise.
fn enqueue_defer_import_job(
  state: State(host),
  reject_fn: JsValue,
  settle: fn(State(host)) -> #(State(host), DeferHookOutcome),
) -> State(host) {
  let #(heap, job_fn) =
    common.alloc_host_fn(
      state.heap,
      state.builtins.function.prototype,
      fn(_args, _this, state) {
        let #(state, outcome) = settle(state)
        let state = case outcome {
          DeferHookSettledCapability -> state
          DeferHookRejected(reason) ->
            call_resolving_fn(state, reject_fn, reason)
        }
        #(state, Ok(JsUndefined))
      },
      "%ContinueDynamicImport%",
      0,
    )
  // The job's own child resolve/reject are unused (non-callable sentinels):
  // settlement happens through the hook / reject_fn above, whose
  // [[AlreadyResolved]] flag also makes any double settle a no-op.
  let job =
    value.PromiseReactionJob(
      handler: value.Handler(JsObject(job_fn)),
      arg: JsUndefined,
      resolve: JsUndefined,
      reject: JsUndefined,
    )
  State(..state, heap:, job_queue: job_queue.push(state.job_queue, job))
}

/// Call one of the import promise's resolving functions (§27.2.1.3 — they
/// return undefined and never throw); log defensively if one somehow does.
fn call_resolving_fn(
  state: State(host),
  target: JsValue,
  arg: JsValue,
) -> State(host) {
  case state.call(state, target, JsUndefined, [arg]) {
    Ok(#(_, state)) -> state
    Error(#(thrown, state)) -> {
      io.println_error(
        "arc: import promise resolving function threw: "
        <> string.inspect(thrown),
      )
      state
    }
  }
}

/// Step 8: options must be undefined or an object whose "with" value (if
/// present) is an object of string-valued attributes. This host supports no
/// import attributes, so any attribute entry rejects with a TypeError
/// (AllImportAttributesSupported returns false for every key).
fn validate_options(
  state: State(host),
  options: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  case options {
    JsUndefined -> Ok(state)
    JsObject(options_ref) -> {
      // Step 8.b-c: Get(options, "with"), abrupt → reject.
      case object.get_value(state, options_ref, Named("with"), options) {
        Error(#(thrown, state)) -> Error(#(thrown, state))
        Ok(#(JsUndefined, state)) -> Ok(state)
        Ok(#(JsObject(attributes_ref), state)) ->
          validate_attributes(state, attributes_ref)
        Ok(#(_, state)) ->
          coerce.thrown_type_error(state, "The 'with' option must be an object")
      }
    }
    _ ->
      coerce.thrown_type_error(
        state,
        "The second argument to import() must be an object",
      )
  }
}

/// Step 8.d: EnumerableOwnProperties(attributesObj, key+value) — every value
/// must be a String (step 8.d.iv) — then step 8.e: any present attribute is
/// unsupported by this host (AllImportAttributesSupported is false for every
/// key), so a non-empty attribute list rejects with a TypeError. The value
/// check runs over ALL entries before the supported check, per spec order.
fn validate_attributes(
  state: State(host),
  attributes_ref: Ref,
) -> Result(State(host), #(JsValue, State(host))) {
  // Trap-aware EnumerableOwnProperties: a Proxy attributes object's ownKeys /
  // getOwnPropertyDescriptor traps run (and their throws reject the promise).
  use #(keys, state) <- result.try(
    builtins_object.enumerable_string_keys_stateful(state, attributes_ref),
  )
  // Step 8.d.ii-iv: Get each value (getters can throw → reject); every value
  // must be a String.
  use state <- result.try(
    list.try_fold(keys, state, fn(state, key) {
      case
        object.get_value(
          state,
          attributes_ref,
          key.canonical_key(key),
          JsObject(attributes_ref),
        )
      {
        Error(#(thrown, state)) -> Error(#(thrown, state))
        Ok(#(JsString(_), state)) -> Ok(state)
        Ok(#(_, state)) ->
          coerce.thrown_type_error(
            state,
            "Import attribute values must be strings",
          )
      }
    }),
  )
  // Step 8.e: AllImportAttributesSupported — this host supports none.
  case keys {
    [] -> Ok(state)
    [key, ..] ->
      coerce.thrown_type_error(
        state,
        "Import attribute '" <> key <> "' is not supported",
      )
  }
}

/// Steps 9+ (HostLoadImportedModule): read the embedder's import hook off the
/// realm's ENGINE state (`ctx.host_hooks.import_hook` — never a globalThis
/// property, so guest JS can neither observe nor replace it) and invoke it
/// with the specifier string followed by `extra_args` (the captured referrer,
/// and for `import.defer` the phase marker plus resolving functions).
/// `Ok(returned)` is a normal hook return; `Error(thrown)` is either "no hook
/// installed" (a TypeError) or the value the hook threw.
fn call_host_hook(
  state: State(host),
  specifier_string: String,
  extra_args: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  case state.ctx.host_hooks.import_hook {
    Some(hook_fn) ->
      case
        state.call(state, hook_fn, JsUndefined, [
          JsString(specifier_string),
          ..extra_args
        ])
      {
        Ok(#(namespace, state)) -> #(state, Ok(namespace))
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
    None ->
      state.type_error(state, "Dynamic import is not supported in this context")
  }
}

/// `call_host_hook` for the ~defer~ phase, mapping the raw call outcome onto
/// the typed `DeferHookOutcome` contract: any NORMAL return means the hook
/// took ownership of the import promise's capability (its return VALUE
/// carries nothing — the hook was handed the resolving functions and settles
/// through them), any ABRUPT completion is a value to reject with. There is
/// deliberately no way to express "resolve with the hook's return value"
/// here.
fn call_defer_host_hook(
  state: State(host),
  specifier_string: String,
  hook_args: List(JsValue),
) -> #(State(host), DeferHookOutcome) {
  case call_host_hook(state, specifier_string, hook_args) {
    #(state, Ok(_)) -> #(state, DeferHookSettledCapability)
    #(state, Error(reason)) -> #(state, DeferHookRejected(reason))
  }
}
