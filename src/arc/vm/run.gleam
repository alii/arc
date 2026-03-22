// ============================================================================
// Public API for the VM — run scripts, modules, REPL sessions
// ============================================================================

import arc/vm/array
import arc/vm/builtins/common.{type Builtins}
import arc/vm/completion.{
  type Completion, NormalCompletion, ThrowCompletion, YieldCompletion,
}
import arc/vm/event_loop
import arc/vm/execute
import arc/vm/frame.{type State, type VmError, State}
import arc/vm/heap.{type Heap}
import arc/vm/realm
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, JsObject, JsUndefined, ObjectSlot,
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
  ModuleOk(value: JsValue, heap: Heap, locals: array.Array(JsValue))
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

/// Run a function template with a globalThis object, then drain jobs.
/// When event_loop is True, runs the mailbox-backed event loop (blocking
/// until `outstanding` hits zero); otherwise just drains the microtask queue.
pub fn run(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  event_loop: Bool,
) -> Result(Completion, VmError) {
  let result =
    execute.init_state(func, heap, builtins, global_object, False, event_loop)
    |> execute.execute_inner()
  use #(completion, final_state) <- result.try(result)
  let drained_state = event_loop.finish(final_state)
  case completion {
    NormalCompletion(val, _) -> Ok(NormalCompletion(val, drained_state.heap))
    ThrowCompletion(val, _) -> Ok(ThrowCompletion(val, drained_state.heap))
    YieldCompletion(_, _) ->
      panic as "YieldCompletion should not appear at script level"
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
  event_loop: Bool,
) -> ModuleResult {
  let locals = array.repeat(JsUndefined, func.local_count)
  let state =
    State(
      ..execute.new_state(
        func,
        locals,
        heap,
        builtins,
        global_object,
        import_globals,
        set.new(),
        dict.new(),
        dict.new(),
        event_loop,
      ),
      this_binding: JsUndefined,
    )
  let result = execute.execute_inner(state)
  case result {
    Error(vm_err) -> ModuleError(error: vm_err)
    Ok(#(completion, final_state)) -> {
      let drained_state = event_loop.finish(final_state)
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
  let locals = array.repeat(JsUndefined, func.local_count)
  let state =
    State(
      ..execute.new_state(
        func,
        locals,
        heap,
        builtins,
        env.global_object,
        env.lexical_globals,
        env.const_lexical_globals,
        env.symbol_descriptions,
        env.symbol_registry,
        False,
      ),
      realms: env.realms,
    )
  use #(completion, final_state) <- result.try(execute.execute_inner(state))
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
  }
}

/// Get the fulfilled value of a promise JsValue, or None if not fulfilled.
pub fn promise_result(h: Heap, val: JsValue) -> Option(JsValue) {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: value.PromiseObject(promise_data:), ..)) ->
          case heap.read(h, promise_data) {
            Some(value.PromiseSlot(state: value.PromiseFulfilled(v), ..)) ->
              Some(v)
            Some(value.PromiseSlot(state: value.PromiseRejected(r), ..)) ->
              Some(r)
            _ -> None
          }
        _ -> None
      }
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

/// Run the event loop. Delegates to event_loop.run_event_loop.
pub fn run_event_loop(state: State) -> State {
  event_loop.run_event_loop(state)
}
