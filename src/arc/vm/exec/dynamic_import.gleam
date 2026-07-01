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
////       embedder's import hook (a host function installed on the global
////       object, see arc/module_host.install_import_hook). The hook returns
////       the module namespace object or throws; the promise is resolved or
////       rejected accordingly. Without a hook, import() rejects with a
////       TypeError (dynamic import unsupported in this context).
////
//// All failures after argument evaluation reject the returned promise —
//// nothing here throws synchronously (IfAbruptRejectPromise semantics).
//// Cross-referenced with QuickJS js_dynamic_import / engine262 ImportCall.

import arc/vm/builtins/common
import arc/vm/builtins/object as builtins_object
import arc/vm/builtins/promise as builtins_promise
import arc/vm/exec/promises
import arc/vm/internal/job_queue
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type State, type StepResult, State}
import arc/vm/value.{
  type JsValue, type Ref, DataProperty, JsObject, JsString, JsUndefined, Named,
}
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string

/// Well-known (hidden) global property holding the embedder's dynamic-import
/// host hook: a function called with (specifier_string, referrer?) returning
/// the module namespace object, or throwing the value to reject with.
pub const hook_property = "__arc_dynamic_import__"

/// Well-known (hidden) global property naming the resolved specifier of the
/// module whose body is currently executing (set/restored by the module
/// evaluator). §16.2.1.8 HostLoadImportedModule resolves the requested
/// specifier against this referencingScriptOrModule, not the realm's entry
/// script; we read it at ImportCall time (the spec captures
/// referencingScriptOrModule in step 2, synchronously) and hand it to the
/// host hook.
pub const referrer_property = "__arc_import_referrer__"

/// Sentinel the import hook returns when it has wired the import promise's
/// settlement itself (the ~defer~ phase waiting on async transitive
/// dependency evaluation promises) — the import job must not also settle.
pub const settled_marker = "__arc_import_settled__"

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
      let referrer_args = capture_referrer(state)
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
      let hook_args = case capture_referrer(state) {
        [] -> [JsUndefined, JsString("defer"), resolve_fn, reject_fn]
        [referrer, ..] -> [referrer, JsString("defer"), resolve_fn, reject_fn]
      }
      enqueue_settling_import_job(state, resolve_fn, reject_fn, fn(state) {
        call_host_hook(state, specifier_string, hook_args)
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

/// §16.2.1.8 HostLoadImportedModule referrer: read the marker naming the
/// module whose body is currently executing. Captured at ImportCall time (the
/// spec captures referencingScriptOrModule synchronously) — the import job
/// may run after the current module body finishes and the marker is restored.
fn capture_referrer(state: State(host)) -> List(JsValue) {
  case
    object.get_own_property(
      state.heap,
      state.ctx.global_object,
      Named(referrer_property),
    )
  {
    option.Some(DataProperty(value: JsString(r), ..)) -> [JsString(r)]
    _ -> []
  }
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

/// As `enqueue_import_job`, but the job settles the import promise itself
/// through the provided resolving functions instead of relying on the
/// reaction machinery's child resolution: a hook that defers settlement (the
/// ~defer~ phase waiting on async transitive dependencies) wires the
/// resolving functions into promise reactions and returns `settled_marker`,
/// and the job then leaves the promise pending for those reactions to settle.
fn enqueue_settling_import_job(
  state: State(host),
  resolve_fn: JsValue,
  reject_fn: JsValue,
  settle: fn(State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> State(host) {
  let #(heap, job_fn) =
    common.alloc_host_fn(
      state.heap,
      state.builtins.function.prototype,
      fn(_args, _this, state) {
        let #(state, result) = settle(state)
        let state = case result {
          Ok(JsString(marker)) if marker == settled_marker -> state
          Ok(resolution) -> call_resolving_fn(state, resolve_fn, resolution)
          Error(reason) -> call_resolving_fn(state, reject_fn, reason)
        }
        #(state, Ok(JsUndefined))
      },
      "%ContinueDynamicImport%",
      0,
    )
  // The job's own child resolve/reject are unused (non-callable sentinels):
  // settlement happens through resolve_fn/reject_fn above, whose
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
          value.canonical_key(key),
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

/// Steps 9+ (HostLoadImportedModule): look up the embedder's import hook on
/// the global object and invoke it with the specifier string and the
/// captured referrer (when one was active at the ImportCall site).
fn call_host_hook(
  state: State(host),
  specifier_string: String,
  referrer_args: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  let hook =
    object.get_own_property(
      state.heap,
      state.ctx.global_object,
      Named(hook_property),
    )
  case hook {
    option.Some(DataProperty(value: JsObject(_) as hook_fn, ..)) ->
      case
        state.call(state, hook_fn, JsUndefined, [
          JsString(specifier_string),
          ..referrer_args
        ])
      {
        Ok(#(namespace, state)) -> #(state, Ok(namespace))
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
    _ ->
      state.type_error(state, "Dynamic import is not supported in this context")
  }
}
