//// §13.3.10 Import Calls: runtime semantics for the DynamicImport opcodes.
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
////   9+. HostLoadImportedModule / ContinueDynamicImport: delegated to the
////       embedder's import hook, the `Agent.host_fns` entry registered under
////       `import_hook_id` (installed by arc/interp/module_host, never
////       reachable from guest JS). The hook returns the module namespace
////       object or an error; the promise is resolved or rejected accordingly.
////       Without a hook, import() rejects with a TypeError.
////
//// All failures after argument evaluation reject the returned promise;
//// nothing here raises (IfAbruptRejectPromise semantics). Cross-referenced
//// with QuickJS js_dynamic_import / engine262 ImportCall.

import arc/interp/module_registry as registry
import arc/rt/async as rt_async
import arc/rt/call.{type Completion, NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, HostJob, KHandle, KStr, KUndef, Named,
  StringKey, SyntaxErr, TypeErr, classify, mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}

/// The `Agent.host_fns` id the dynamic-import hook is registered under. Ids
/// the embedder mints for its own natives are never negative, and no function
/// object carries this token: the hook is called from here only.
pub const import_hook_id = -1

/// Positional phase marker (3rd hook argument) that `import.defer(spec)`
/// passes to the host hook: load + LINK the graph without evaluating it, and
/// settle the import promise with the Deferred Module Namespace.
const defer_phase_marker = "defer"

/// Which HostLoadImportedModule phase a hook call requests.
pub type HookPhase {
  /// `import(specifier)`: load, link and EVALUATE the graph. The hook's return
  /// value settles the import promise.
  EagerPhase
  /// `import.defer(specifier)`: load and LINK only. The hook OWNS the import
  /// promise's settlement through these resolving functions: on any normal
  /// return it has settled (or wired the reactions that will) and its return
  /// value carries nothing; the import job only rejects when no hook is
  /// installed or the hook failed before taking ownership.
  DeferPhase(resolve_fn: JsVal, reject_fn: JsVal)
}

/// One well-formed hook invocation: the specifier is a real string and a
/// deferred call carries both resolving functions.
pub type HookCall {
  /// `referrer` is §16.2.1.8's referencingScriptOrModule, the module whose
  /// body contained the `import()`. `None` for a script-level import, where
  /// the host substitutes its own entry referrer.
  HookCall(specifier: String, referrer: Option(String), phase: HookPhase)
}

/// Why a hook argument list did not decode. Every one of these means the VM
/// and the host disagree about the encoding, so they all reject the import
/// promise with a TypeError.
pub type HookArgError {
  MissingSpecifier
  NonStringSpecifier(found: JsVal)
  MissingResolve
  MissingReject
  BadPhase(found: JsVal)
}

pub fn hook_arg_error_message(err: HookArgError) -> String {
  case err {
    MissingSpecifier -> "import hook called without a specifier"
    NonStringSpecifier(_) -> "import hook called with a non-string specifier"
    MissingResolve | MissingReject ->
      "import hook called with the defer phase but no promise capability"
    BadPhase(_) -> "import hook called with unexpected arguments"
  }
}

/// Encode a hook invocation as the positional argument list the host hook
/// receives: `[specifier, referrer?, "defer", resolve, reject]`. The phase
/// marker is positional, so a deferred call with no referrer pads with
/// `undefined`, while an eager call simply omits it. `parse_hook_args` is
/// the inverse and lives beside it so the two cannot drift.
pub fn encode_hook_args(
  specifier: String,
  referrer: Option(String),
  phase: HookPhase,
) -> List(JsVal) {
  case phase {
    EagerPhase ->
      case referrer {
        Some(referrer) -> [mk_string(specifier), mk_string(referrer)]
        None -> [mk_string(specifier)]
      }
    DeferPhase(resolve_fn:, reject_fn:) -> [
      mk_string(specifier),
      referrer |> option.map(mk_string) |> option.unwrap(mk_undefined()),
      mk_string(defer_phase_marker),
      resolve_fn,
      reject_fn,
    ]
  }
}

/// Decode a hook argument list; the ONLY place the encoding is read.
pub fn parse_hook_args(args: List(JsVal)) -> Result(HookCall, HookArgError) {
  case args {
    [] -> Error(MissingSpecifier)
    [first, ..rest] ->
      case classify(first) {
        KStr(specifier) -> parse_hook_tail(specifier, rest)
        _ -> Error(NonStringSpecifier(first))
      }
  }
}

fn parse_hook_tail(
  specifier: String,
  rest: List(JsVal),
) -> Result(HookCall, HookArgError) {
  let referrer = case list.first(rest) |> option.from_result {
    Some(v) ->
      case classify(v) {
        KStr(referrer) -> Some(referrer)
        _ -> None
      }
    None -> None
  }
  case rest {
    // Eager: at most the referrer follows the specifier.
    [] | [_] -> Ok(HookCall(specifier:, referrer:, phase: EagerPhase))
    // Defer: the phase marker AND both resolving functions, together.
    [_, phase, ..capability] ->
      case classify(phase) {
        KStr(marker) if marker == defer_phase_marker ->
          case capability {
            [resolve_fn, reject_fn] ->
              Ok(HookCall(
                specifier:,
                referrer:,
                phase: DeferPhase(resolve_fn:, reject_fn:),
              ))
            [] -> Error(MissingResolve)
            [_] -> Error(MissingReject)
            [_, _, ..] -> Error(BadPhase(phase))
          }
        _ -> Error(BadPhase(phase))
      }
  }
}

// -- FFI seam --------------------------------------------------------------------

/// Run `body` under the runtime's catch, folding a raised JS exception into
/// `ThrowCompletion` (arc_rt_call_ffi:t_apply_protected/2).
@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(Completion, Agent)

// -- The three opcodes -----------------------------------------------------------

/// The DynamicImport opcode: `import(specifier, options)`. Returns the import
/// promise. Never raises: every failure rejects the promise.
pub fn import_call(
  st: Agent,
  specifier: JsVal,
  options: JsVal,
) -> #(JsVal, Agent) {
  // Step 4: NewPromiseCapability(%Promise%).
  let #(promise, st) = rt_async.t_new_promise(st)
  // Steps 5-8 run synchronously; an abrupt completion rejects the promise
  // (IfAbruptRejectPromise). Step 9+ (HostLoadImportedModule) is deferred to
  // a job so the module graph loads/evaluates AFTER the currently running
  // synchronous code completes and after previously enqueued microtasks,
  // matching ContinueDynamicImport's promise-job scheduling (cf. QuickJS
  // js_dynamic_import_job).
  let st = case import_request(st, specifier, options) {
    #(ThrowCompletion(reason), st) ->
      rt_async.t_promise_reject(st, promise, reason)
    #(NormalCompletion(specifier_string), st) -> {
      // §16.2.1.8 referencingScriptOrModule, captured synchronously at
      // ImportCall time: the job may only run after the current module body
      // finishes.
      let hook_args =
        encode_hook_args(
          string_of(specifier_string),
          registry.read_active_referrer(st),
          EagerPhase,
        )
      use st <- enqueue_import_job(st, promise)
      call_host_hook(st, hook_args)
    }
  }
  #(mk_object(promise), st)
}

/// The DynamicImportDefer opcode (`import.defer(specifier)`, the
/// defer-import-eval proposal). As `import()`, but the hook is invoked with
/// the "defer" phase marker and the promise's resolving functions: it loads
/// and LINKS the module graph without evaluating it and resolves the promise
/// with the Deferred Module Namespace itself.
pub fn defer_import_call(st: Agent, specifier: JsVal) -> #(JsVal, Agent) {
  let #(promise, st) = rt_async.t_new_promise(st)
  let st = case import_request(st, specifier, mk_undefined()) {
    #(ThrowCompletion(reason), st) ->
      rt_async.t_promise_reject(st, promise, reason)
    #(NormalCompletion(specifier_string), st) -> {
      // ContinueDynamicImport's ~defer~ arm chains the import promise's
      // settlement onto async-dependency evaluation promises via
      // PerformPromiseThen (the proposal's SafePerformPromiseAll never looks
      // up `then`), so the hook is handed the resolving functions.
      let #(#(resolve_h, reject_h), st) =
        rt_async.alloc_resolving_fns(st, promise)
      let reject_fn = mk_object(reject_h)
      let hook_args =
        encode_hook_args(
          string_of(specifier_string),
          registry.read_active_referrer(st),
          DeferPhase(resolve_fn: mk_object(resolve_h), reject_fn:),
        )
      use st <- enqueue_host_job(st)
      case call_host_hook(st, hook_args) {
        #(st, Ok(_)) -> st
        #(st, Error(reason)) -> call_settle_fn(st, reject_fn, reason)
      }
    }
  }
  #(mk_object(promise), st)
}

/// The DynamicImportSource opcode (`import.source(specifier)`, the
/// source-phase-imports proposal). An abrupt ToString rejects with the
/// thrown value; a coercible specifier names a Source Text Module Record,
/// and GetModuleSource for those always throws a SyntaxError (§16.2.1.7.2),
/// so the promise rejects with a SyntaxError from a job.
pub fn source_import_call(st: Agent, specifier: JsVal) -> #(JsVal, Agent) {
  let #(promise, st) = rt_async.t_new_promise(st)
  let st = case import_request(st, specifier, mk_undefined()) {
    #(ThrowCompletion(reason), st) ->
      rt_async.t_promise_reject(st, promise, reason)
    #(NormalCompletion(_), st) -> {
      use st <- enqueue_import_job(st, promise)
      let #(err, st) =
        st.store.ops.new_error(
          st,
          SyntaxErr,
          "Module has no source phase representation",
        )
      #(st, Error(err))
    }
  }
  #(mk_object(promise), st)
}

// -- Steps 5-8 -----------------------------------------------------------------------

fn string_of(v: JsVal) -> String {
  case classify(v) {
    KStr(s) -> s
    _ -> ""
  }
}

/// Steps 5-8: coerce the specifier and validate options, under the runtime's
/// catch. `NormalCompletion` carries the specifier as a JS string;
/// `ThrowCompletion` is the value to reject the import promise with.
fn import_request(
  st: Agent,
  specifier: JsVal,
  options: JsVal,
) -> #(Completion, Agent) {
  use st <- protected(st)
  // Steps 5-6: specifierString = ToString(specifier), abrupt → reject.
  let #(specifier_string, st) = rt_val.t_to_string(st, specifier)
  let st = validate_options(st, options)
  #(mk_string(specifier_string), st)
}

/// Step 8: options must be undefined or an object whose "with" value (if
/// present) is an object of string-valued attributes. This host supports no
/// import attributes, so any attribute entry throws a TypeError
/// (AllImportAttributesSupported returns false for every key). Raises.
fn validate_options(st: Agent, options: JsVal) -> Agent {
  case classify(options) {
    KUndef -> st
    KHandle(_) -> {
      // Step 8.b-c: Get(options, "with"), abrupt → reject.
      let #(attributes, st) =
        rt_obj.t_get_prop(st, options, StringKey(Named("with")))
      case classify(attributes) {
        KUndef -> st
        KHandle(attributes_h) -> validate_attributes(st, attributes_h)
        _ ->
          rt_val.t_throw_type_error(st, "The 'with' option must be an object")
      }
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "The second argument to import() must be an object",
      )
  }
}

/// Step 8.d: EnumerableOwnProperties(attributesObj, key+value); every value
/// must be a String (step 8.d.iv); then step 8.e: any present attribute is
/// unsupported by this host, so a non-empty attribute list throws a
/// TypeError. The value check runs over ALL entries before the supported
/// check, per spec order. Trap-aware: a Proxy attributes object's ownKeys /
/// getOwnPropertyDescriptor traps run (and their throws reject the promise).
fn validate_attributes(st: Agent, attributes: Handle) -> Agent {
  let #(keys, st) = rt_obj.t_enumerable_own_keys(st, attributes)
  let st =
    list.fold(keys, st, fn(st, key) {
      let #(v, st) =
        rt_obj.t_get_prop(st, mk_object(attributes), StringKey(key))
      case classify(v) {
        KStr(_) -> st
        _ ->
          rt_val.t_throw_type_error(
            st,
            "Import attribute values must be strings",
          )
      }
    })
  case keys {
    [] -> st
    [key, ..] ->
      rt_val.t_throw_type_error(
        st,
        "Import attribute '" <> types.key_to_text(key) <> "' is not supported",
      )
  }
}

// -- Step 9+: the job and the host hook -----------------------------------------

/// Enqueue a promise job that runs `settle` and settles `promise` with its
/// outcome through the promise's own resolving functions (§27.2.1.3), so a
/// thenable namespace (an exported callable `then`) is adopted per spec.
fn enqueue_import_job(
  st: Agent,
  promise: Handle,
  settle: fn(Agent) -> #(Agent, Result(JsVal, JsVal)),
) -> Agent {
  let #(#(resolve_h, reject_h), st) = rt_async.alloc_resolving_fns(st, promise)
  use st <- enqueue_host_job(st)
  case settle(st) {
    #(st, Ok(v)) -> call_settle_fn(st, mk_object(resolve_h), v)
    #(st, Error(reason)) -> call_settle_fn(st, mk_object(reject_h), reason)
  }
}

/// A `HostJob` carries no child capability: settlement happens inside `run`.
fn enqueue_host_job(st: Agent, run: fn(Agent) -> Agent) -> Agent {
  rt_async.t_enqueue_job(st, HostJob(run:))
}

/// Call one of a promise's resolving functions (§27.2.1.3: they return
/// undefined and never throw; a throw would surface through the job's own
/// uncaught report, so the completion is dropped here).
fn call_settle_fn(st: Agent, settle_fn: JsVal, arg: JsVal) -> Agent {
  let #(_, st) = rt_call.t_call(st, settle_fn, mk_undefined(), [arg])
  st
}

/// Steps 9+ (HostLoadImportedModule): invoke the embedder's import hook with
/// `hook_args`. `Ok(returned)` is a normal hook return; `Error(thrown)` is
/// "no hook installed" (a TypeError), the value the hook returned as its
/// error, or one it raised.
fn call_host_hook(
  st: Agent,
  hook_args: List(JsVal),
) -> #(Agent, Result(JsVal, JsVal)) {
  case dict.get(st.host_fns, import_hook_id) {
    Error(Nil) -> {
      let #(err, st) =
        st.store.ops.new_error(
          st,
          TypeErr,
          "Dynamic import is not supported in this context",
        )
      #(st, Error(err))
    }
    Ok(types.HostFnEntry(call:, ..)) -> {
      let outcome =
        protected(st, fn(st) {
          case call(st, hook_args, mk_undefined(), mk_undefined()) {
            #(st, Ok(v)) -> #(v, st)
            #(st, Error(thrown)) -> rt_store.t_throw(st, thrown)
          }
        })
      case outcome {
        #(NormalCompletion(v), st) -> #(st, Ok(v))
        #(ThrowCompletion(thrown), st) -> #(st, Error(thrown))
      }
    }
  }
}
