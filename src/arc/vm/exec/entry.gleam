// ============================================================================
// Public API for the VM — run scripts, modules, REPL sessions
// ============================================================================

import arc/vm/builtins/common.{type Builtins}
import arc/vm/completion.{
  type Completion, NormalCompletion, ThrowCompletion, YieldCompletion,
}
import arc/vm/exec/event_loop
import arc/vm/exec/interpreter
import arc/vm/heap
import arc/vm/internal/tuple_array
import arc/vm/realm
import arc/vm/state.{type Heap, type State, type VmError, State}
import arc/vm/value.{type FuncTemplate, type JsValue, type Ref, JsObject}
import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

// ============================================================================
// Public types
// ============================================================================

/// Result of module evaluation -- includes locals for export extraction.
pub type ModuleResult {
  ModuleOk(value: JsValue, heap: Heap, locals: tuple_array.TupleArray(JsValue))
  ModuleThrow(value: JsValue, heap: Heap)
  ModuleError(error: VmError)
}

/// Persistent REPL environment carried between evaluations.
pub type ReplEnv {
  ReplEnv(
    global_object: Ref,
    lexical_globals: dict.Dict(String, JsValue),
    const_lexical_globals: set.Set(String),
    symbol_descriptions: dict.Dict(value.SymbolId, String),
    symbol_registry: dict.Dict(String, value.SymbolId),
    /// Realm builtins registry, keyed by RealmSlot ref.
    /// Persisted so $262.evalScript/createRealm work across REPL evaluations.
    realms: dict.Dict(Ref, Builtins),
  )
}

// ============================================================================
// Public functions
// ============================================================================

/// Run a function template with a globalThis object, then drain microtasks.
/// No macrotask loop — for that, pass a driver to `run_with`.
pub fn run(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
) -> Result(Completion, VmError) {
  run_with(func, heap, builtins, global_object, event_loop.finish)
}

/// Like `run` but the caller supplies the post-script driver. `finish`
/// receives the State after the top-level script returns and is expected
/// to drain microtasks plus whatever macrotask loop the embedder owns
/// (e.g. `arc/beam.run`). Core's `event_loop.finish` is the no-macrotask
/// default.
pub fn run_with(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  finish: fn(State) -> State,
) -> Result(Completion, VmError) {
  let executed =
    interpreter.init_state(func, heap, builtins, global_object, False)
    |> interpreter.execute_inner()
  use #(settled, drained) <- result.map(settle(executed, finish))
  completion_of(settled, drained.heap)
}

/// Run a module template with its binding cells seeded into local slots.
/// `seeds` are (slot index, BoxSlot ref) pairs: import bindings in capture
/// slots 0..N-1 (each the exporting module's live cell) plus this module's own
/// export cells in their declared slots — ES live bindings (§16.2). Reads/writes
/// go through GetBoxed/PutBoxed. Module `this` is undefined per ES §16.2.1.5.2.
pub fn run_module(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  seeds: List(#(Int, JsValue)),
  finish: fn(State) -> State,
) -> ModuleResult {
  let locals = interpreter.init_module_locals(func, seeds)
  let state =
    interpreter.new_state(
      func,
      locals,
      heap,
      builtins,
      global_object,
      dict.new(),
      set.new(),
      dict.new(),
      dict.new(),
    )
  case settle(interpreter.execute_inner(state), finish) {
    Error(vm_err) -> ModuleError(error: vm_err)
    Ok(#(Ok(val), drained)) ->
      ModuleOk(value: val, heap: drained.heap, locals: drained.locals)
    Ok(#(Error(val), drained)) -> ModuleThrow(value: val, heap: drained.heap)
  }
}

/// Like run, but persists globals across calls.
/// Used by the REPL so var declarations and function definitions survive.
pub fn run_and_drain_repl(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  env: ReplEnv,
) -> Result(#(Completion, ReplEnv), VmError) {
  // §16.1.6 ScriptEvaluation sets envs to globalEnv; script `this` resolves via §9.1.1.4.11 GetThisBinding to [[GlobalThisValue]].
  let this_val = JsObject(env.global_object)
  let locals = interpreter.init_top_level_locals(func, this_val)
  let state =
    State(
      ..interpreter.new_state(
        func,
        locals,
        heap,
        builtins,
        env.global_object,
        env.lexical_globals,
        env.const_lexical_globals,
        env.symbol_descriptions,
        env.symbol_registry,
      ),
      realms: env.realms,
    )
  use #(settled, drained) <- result.map(settle(
    interpreter.execute_inner(state),
    event_loop.drain_jobs,
  ))
  let new_env =
    ReplEnv(
      global_object: drained.global_object,
      lexical_globals: drained.lexical_globals,
      const_lexical_globals: drained.const_lexical_globals,
      symbol_descriptions: drained.symbol_descriptions,
      symbol_registry: drained.symbol_registry,
      realms: drained.realms,
    )
  #(completion_of(settled, drained.heap), new_env)
}

/// Call a function value with `this` and `args`, then run the `finish` driver
/// to drain. The counterpart to `run`/`run_with` for a value you already hold
/// — e.g. a `receive` export read off a module namespace — so an embedder can
/// invoke it the way the engine would, without re-evaluating a script. This is
/// the host-call-then-drain pattern (cf. Node's MakeCallback, QuickJS
/// `JS_Call` + the `JS_ExecutePendingJob` loop): draining happens at this
/// outermost call only, so don't call it from inside a host function — use the
/// re-entrant `state.call` there.
pub fn run_export(
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  finish: fn(State) -> State,
) -> Result(Completion, VmError) {
  let executed =
    interpreter.call_to_completion(
      callee,
      this_val,
      args,
      heap,
      builtins,
      global_object,
    )
  use #(settled, drained) <- result.map(settle(executed, finish))
  completion_of(settled, drained.heap)
}

/// Drain microtasks/macrotasks via `finish`, then narrow a top-level completion
/// to its settled outcome — `Ok(value)` for a normal completion,
/// `Error(thrown)` for a throw — paired with the drained State. A top-level
/// script, module, or embedder call can only finish Normal or Throw, so a
/// Yield/Await reaching here is a compiler bug; this is the single place that
/// invariant is enforced for the whole `run*` family.
fn settle(
  executed: Result(#(Completion, State), VmError),
  finish: fn(State) -> State,
) -> Result(#(Result(JsValue, JsValue), State), VmError) {
  use #(completion, final_state) <- result.try(executed)
  let drained = finish(final_state)
  case completion {
    NormalCompletion(val, _) -> Ok(#(Ok(val), drained))
    ThrowCompletion(val, _) -> Ok(#(Error(val), drained))
    YieldCompletion(_, _) ->
      panic as "YieldCompletion should not appear at top level"
    completion.AwaitCompletion(_, _) ->
      panic as "AwaitCompletion should not appear at top level"
  }
}

/// Rebuild a Completion from a settled outcome and the drained heap.
fn completion_of(settled: Result(JsValue, JsValue), heap: Heap) -> Completion {
  case settled {
    Ok(val) -> NormalCompletion(val, heap)
    Error(thrown) -> ThrowCompletion(thrown, heap)
  }
}

/// Get the fulfilled value of a promise JsValue, or None if not fulfilled.
pub fn promise_result(h: Heap, val: JsValue) -> Option(JsValue) {
  use ref <- option.then(case val {
    JsObject(ref) -> Some(ref)
    _ -> None
  })
  use data_ref <- option.then(heap.read_promise_data_ref(h, ref))
  case heap.read_promise_state(h, data_ref) {
    Some(value.PromiseFulfilled(v)) -> Some(v)
    Some(value.PromiseRejected(r)) -> Some(r)
    _ -> None
  }
}

/// Build a $262 object with evalScript, createRealm, gc methods and a global
/// property. Delegates to realm.build_262.
pub fn build_262(
  h: Heap,
  b: Builtins,
  global_ref: Ref,
  realm_ref: Ref,
) -> #(Heap, Ref) {
  realm.build_262(h, b, global_ref, realm_ref)
}
