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
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, JsObject, JsUndefined,
}
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
  let result =
    interpreter.init_state(func, heap, builtins, global_object, False)
    |> interpreter.execute_inner()
  use #(completion, final_state) <- result.try(result)
  let drained_state = finish(final_state)
  case completion {
    NormalCompletion(val, _) -> Ok(NormalCompletion(val, drained_state.heap))
    ThrowCompletion(val, _) -> Ok(ThrowCompletion(val, drained_state.heap))
    YieldCompletion(_, _) ->
      panic as "YieldCompletion should not appear at script level"
    completion.AwaitCompletion(_, _) ->
      panic as "AwaitCompletion should not appear at script level"
  }
}

/// Run a module template with imports as lexical globals.
/// Module `this` is undefined per ES S16.2.1.5.2.
pub fn run_module_with_imports(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  import_globals: dict.Dict(String, JsValue),
  finish: fn(State) -> State,
) -> ModuleResult {
  let locals = tuple_array.repeat(JsUndefined, func.local_count)
  let state =
    State(
      ..interpreter.new_state(
        func,
        locals,
        heap,
        builtins,
        global_object,
        import_globals,
        set.new(),
        dict.new(),
        dict.new(),
      ),
      this_binding: JsUndefined,
    )
  let result = interpreter.execute_inner(state)
  case result {
    Error(vm_err) -> ModuleError(error: vm_err)
    Ok(#(completion, final_state)) -> {
      let drained_state = finish(final_state)
      case completion {
        NormalCompletion(val, _) ->
          ModuleOk(
            value: val,
            heap: drained_state.heap,
            locals: drained_state.locals,
          )
        ThrowCompletion(val, _) ->
          ModuleThrow(value: val, heap: drained_state.heap)
        YieldCompletion(_, _) ->
          panic as "YieldCompletion should not appear at module level"
        completion.AwaitCompletion(_, _) ->
          panic as "AwaitCompletion should not appear at module level"
      }
    }
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
  let locals = tuple_array.repeat(JsUndefined, func.local_count)
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
      // §16.1.6 ScriptEvaluation sets envs to globalEnv; script `this` resolves via §9.1.1.4.11 GetThisBinding to [[GlobalThisValue]].
      this_binding: JsObject(env.global_object),
    )
  use #(completion, final_state) <- result.try(interpreter.execute_inner(state))
  let drained_state = event_loop.drain_jobs(final_state)
  let new_env =
    ReplEnv(
      global_object: drained_state.global_object,
      lexical_globals: drained_state.lexical_globals,
      const_lexical_globals: drained_state.const_lexical_globals,
      symbol_descriptions: drained_state.symbol_descriptions,
      symbol_registry: drained_state.symbol_registry,
      realms: drained_state.realms,
    )
  case completion {
    NormalCompletion(val, _) ->
      Ok(#(NormalCompletion(val, drained_state.heap), new_env))
    ThrowCompletion(val, _) ->
      Ok(#(ThrowCompletion(val, drained_state.heap), new_env))
    YieldCompletion(_, _) ->
      panic as "YieldCompletion should not appear at script level"
    completion.AwaitCompletion(_, _) ->
      panic as "AwaitCompletion should not appear at script level"
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
