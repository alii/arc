import arc/compiler
import arc/parser
import arc/vm/array
import arc/vm/builtins
import arc/vm/builtins/arc as builtins_arc
import arc/vm/builtins/common.{type Builtins}
import arc/vm/completion.{NormalCompletion, ThrowCompletion, YieldCompletion}
import arc/vm/event_loop
import arc/vm/frame.{type State, type StepResult, type VmError, State}
import arc/vm/heap
import arc/vm/js_elements
import arc/vm/object
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, DataProperty, JsObject, JsUndefined,
  ObjectSlot, OrdinaryObject,
}
import gleam/dict
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/set
import gleam/string

// ============================================================================
// Callback types for VM functions that can't be imported directly
// ============================================================================

pub type ExecuteInnerFn =
  fn(State) -> Result(#(completion.Completion, State), VmError)

pub type CallNativeFn =
  fn(State, value.NativeFnSlot, List(JsValue), List(JsValue), JsValue) ->
    Result(State, #(StepResult, JsValue, heap.Heap))

pub type NewStateFn =
  fn(
    FuncTemplate,
    array.Array(JsValue),
    heap.Heap,
    Builtins,
    Ref,
    dict.Dict(String, JsValue),
    set.Set(String),
    dict.Dict(value.SymbolId, String),
    dict.Dict(String, value.SymbolId),
    Bool,
  ) ->
    State

// ============================================================================
// Arc.spawn — spawn a new BEAM process running a JS closure
// ============================================================================

@external(erlang, "erlang", "spawn")
fn spawn(fun: fn() -> Nil) -> value.ErlangPid

/// Non-standard: Arc.spawn(fn)
/// Spawns a new BEAM process that executes the given JS function.
/// The spawned process gets a snapshot of the current heap, builtins,
/// and closure templates. Returns a Pid object.
pub fn arc_spawn(
  args: List(JsValue),
  state: State,
  execute_inner: ExecuteInnerFn,
  call_native: CallNativeFn,
  new_state: NewStateFn,
) -> #(State, Result(JsValue, JsValue)) {
  let fn_arg = case args {
    [a, ..] -> a
    [] -> JsUndefined
  }

  let result = {
    use fn_ref <- result.try(case fn_arg {
      JsObject(fn_ref) -> Ok(fn_ref)
      _ -> Error("Arc.spawn: argument is not a function object")
    })

    use spawner <- result.try(case heap.read(state.heap, fn_ref) {
      Some(ObjectSlot(
        kind: value.FunctionObject(func_template: callee_template, env: env_ref),
        ..,
      )) ->
        Ok(fn() {
          run_spawned_closure(
            callee_template,
            env_ref,
            state.heap,
            state.builtins,
            state.global_object,
            state.lexical_globals,
            state.const_lexical_globals,
            state.symbol_descriptions,
            state.symbol_registry,
            state.event_loop,
            execute_inner,
            new_state,
          )
        })
      Some(ObjectSlot(kind: value.NativeFunction(native), ..)) ->
        Ok(fn() {
          run_spawned_native(
            native,
            state.func,
            state.heap,
            state.builtins,
            state.global_object,
            state.lexical_globals,
            state.const_lexical_globals,
            state.symbol_descriptions,
            state.symbol_registry,
            state.event_loop,
            call_native,
            new_state,
          )
        })
      _ -> Error("Arc.spawn: argument is not a function")
    })

    let #(h, pid_val) =
      builtins_arc.alloc_pid_object(
        state.heap,
        state.builtins.object.prototype,
        state.builtins.function.prototype,
        spawn(spawner),
      )

    Ok(#(State(..state, heap: h), Ok(pid_val)))
  }

  case result {
    Ok(ret) -> ret
    Error(msg) -> frame.type_error(state, msg)
  }
}

/// Run a JS closure in a standalone BEAM process. Sets up a fresh VM
/// state from the snapshot and executes the function to completion.
fn run_spawned_closure(
  callee_template: FuncTemplate,
  env_ref: value.Ref,
  heap: heap.Heap,
  builtins: Builtins,
  global_object: Ref,
  lexical_globals: dict.Dict(String, JsValue),
  const_lexical_globals: set.Set(String),
  symbol_descriptions: dict.Dict(value.SymbolId, String),
  symbol_registry: dict.Dict(String, value.SymbolId),
  event_loop: Bool,
  execute_inner: ExecuteInnerFn,
  new_state_fn: NewStateFn,
) -> Nil {
  let env_values = heap.read_env(heap, env_ref) |> option.unwrap([])
  let env_count = list.length(env_values)
  let remaining =
    callee_template.local_count - env_count - callee_template.arity
  let padded_args = list.repeat(JsUndefined, callee_template.arity)
  let locals =
    list.flatten([env_values, padded_args, list.repeat(JsUndefined, remaining)])
    |> array.from_list

  let state =
    new_state_fn(
      callee_template,
      locals,
      heap,
      builtins,
      global_object,
      lexical_globals,
      const_lexical_globals,
      symbol_descriptions,
      symbol_registry,
      event_loop,
    )

  case execute_inner(state) {
    Ok(#(_, final_state)) -> {
      let _ = event_loop.finish(final_state)
      Nil
    }
    Error(_) -> Nil
  }
}

/// Run a native function in a standalone BEAM process.
/// Sets up a full VM state (using the caller's func template as context)
/// and drains the job queue after execution.
fn run_spawned_native(
  native: value.NativeFnSlot,
  caller_func: FuncTemplate,
  heap: heap.Heap,
  builtins: Builtins,
  global_object: Ref,
  lexical_globals: dict.Dict(String, JsValue),
  const_lexical_globals: set.Set(String),
  symbol_descriptions: dict.Dict(value.SymbolId, String),
  symbol_registry: dict.Dict(String, value.SymbolId),
  event_loop: Bool,
  call_native: CallNativeFn,
  new_state_fn: NewStateFn,
) -> Nil {
  let locals = array.repeat(JsUndefined, caller_func.local_count)
  let state =
    new_state_fn(
      caller_func,
      locals,
      heap,
      builtins,
      global_object,
      lexical_globals,
      const_lexical_globals,
      symbol_descriptions,
      symbol_registry,
      event_loop,
    )

  case call_native(state, native, [], state.stack, JsUndefined) {
    Ok(final_state) -> {
      let _ = event_loop.finish(final_state)
      Nil
    }
    Error(_) -> Nil
  }
}

// ============================================================================
// $262 — test262 host-defined realm functions
// ============================================================================

/// $262.evalScript(source) — parse and execute a script in the realm
/// associated with the $262 object's __realm__ property.
pub fn eval_script_native(
  args: List(JsValue),
  this: JsValue,
  state: State,
  execute_inner: ExecuteInnerFn,
  new_state_fn: NewStateFn,
) -> #(State, Result(JsValue, JsValue)) {
  let source = case args {
    [s, ..] -> s
    [] -> JsUndefined
  }
  use source_str, state <- frame.try_to_string(state, source)

  // Read the __realm__ property from the $262 object to find the realm
  let realm_result = case this {
    JsObject(this_ref) ->
      case object.get_own_property(state.heap, this_ref, "__realm__") {
        Some(DataProperty(value: JsObject(realm_ref), ..)) ->
          case heap.read(state.heap, realm_ref) {
            Some(value.RealmSlot(
              global_object: realm_global,
              lexical_globals:,
              const_lexical_globals:,
              symbol_descriptions:,
              symbol_registry:,
            )) ->
              case dict.get(state.realms, realm_ref) {
                Ok(realm_builtins) ->
                  Ok(#(
                    realm_builtins,
                    realm_global,
                    realm_ref,
                    lexical_globals,
                    const_lexical_globals,
                    symbol_descriptions,
                    symbol_registry,
                  ))
                Error(Nil) -> Error("evalScript: realm builtins not found")
              }
            _ -> Error("evalScript: __realm__ is not a RealmSlot")
          }
        _ -> Error("evalScript: $262 has no __realm__ property")
      }
    _ -> Error("evalScript: this is not an object")
  }

  case realm_result {
    Error(msg) -> frame.type_error(state, msg)
    Ok(#(
      realm_builtins,
      realm_global,
      realm_ref,
      lexical_globals,
      const_lexical_globals,
      symbol_descriptions,
      symbol_registry,
    )) ->
      case parser.parse(source_str, parser.Script) {
        Error(err) -> {
          let #(h, syntax_err) =
            common.make_syntax_error(
              state.heap,
              realm_builtins,
              parser.parse_error_to_string(err),
            )
          #(State(..state, heap: h), Error(syntax_err))
        }
        Ok(program) ->
          case compiler.compile_repl(program) {
            Error(err) -> {
              let #(h, syntax_err) =
                common.make_syntax_error(
                  state.heap,
                  realm_builtins,
                  string.inspect(err),
                )
              #(State(..state, heap: h), Error(syntax_err))
            }
            Ok(template) -> {
              let locals = array.repeat(JsUndefined, template.local_count)
              let eval_state =
                State(
                  ..new_state_fn(
                    template,
                    locals,
                    state.heap,
                    realm_builtins,
                    realm_global,
                    lexical_globals,
                    const_lexical_globals,
                    symbol_descriptions,
                    symbol_registry,
                    False,
                  ),
                  job_queue: state.job_queue,
                  realms: state.realms,
                )
              case execute_inner(eval_state) {
                Error(vm_err) ->
                  frame.type_error(
                    state,
                    "evalScript: VM error: " <> string.inspect(vm_err),
                  )
                Ok(#(completion, final_eval_state)) -> {
                  // Drain microtasks in the eval realm
                  let drained = event_loop.drain_jobs(final_eval_state)
                  // Update the realm slot with potentially modified lexical globals
                  let updated_realm =
                    value.RealmSlot(
                      global_object: realm_global,
                      lexical_globals: drained.lexical_globals,
                      const_lexical_globals: drained.const_lexical_globals,
                      symbol_descriptions: drained.symbol_descriptions,
                      symbol_registry: drained.symbol_registry,
                    )
                  let h = heap.write(drained.heap, realm_ref, updated_realm)
                  // Propagate heap and job queue back to caller
                  let state =
                    State(
                      ..state,
                      heap: h,
                      job_queue: drained.job_queue,
                      pending_receivers: drained.pending_receivers,
                      outstanding: drained.outstanding,
                      realms: drained.realms,
                    )
                  case completion {
                    NormalCompletion(val, _) -> #(state, Ok(val))
                    ThrowCompletion(thrown, _) -> #(state, Error(thrown))
                    YieldCompletion(_, _) ->
                      frame.type_error(state, "evalScript: unexpected yield")
                  }
                }
              }
            }
          }
      }
  }
}

/// $262.createRealm() — create a fresh realm and return its $262 object.
pub fn create_realm_native(
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Initialize fresh builtins and global object for the new realm
  let #(h, new_builtins) = builtins.init(state.heap)
  let #(h, new_global_ref) = builtins.globals(new_builtins, h)

  // Allocate a RealmSlot for the new realm
  let #(h, realm_ref) =
    heap.alloc(
      h,
      value.RealmSlot(
        global_object: new_global_ref,
        lexical_globals: dict.new(),
        const_lexical_globals: set.new(),
        symbol_descriptions: dict.new(),
        symbol_registry: dict.new(),
      ),
    )
  let h = heap.root(h, realm_ref)

  // Build the $262 object for the new realm
  let #(h, dollar_262_ref) =
    build_262(h, new_builtins, new_global_ref, realm_ref)

  // Install $262 on the new realm's global object
  let #(h, _) =
    object.set_property(h, new_global_ref, "$262", JsObject(dollar_262_ref))

  // Register the realm's builtins
  let realms = dict.insert(state.realms, realm_ref, new_builtins)

  #(State(..state, heap: h, realms:), Ok(JsObject(dollar_262_ref)))
}

/// Build a $262 object with evalScript, createRealm, gc methods and a global
/// property. The realm_ref points to a RealmSlot on the heap.
/// Public so test262_exec.gleam can use it for initial test setup.
pub fn build_262(
  h: heap.Heap,
  b: Builtins,
  global_ref: Ref,
  realm_ref: Ref,
) -> #(heap.Heap, Ref) {
  let func_proto = b.function.prototype

  // Allocate method function objects
  let #(h, eval_script_fn) =
    common.alloc_native_fn(
      h,
      func_proto,
      value.VmNative(value.EvalScript),
      "evalScript",
      1,
    )
  let #(h, create_realm_fn) =
    common.alloc_native_fn(
      h,
      func_proto,
      value.VmNative(value.CreateRealm),
      "createRealm",
      0,
    )
  let #(h, gc_fn) =
    common.alloc_native_fn(h, func_proto, value.VmNative(value.Gc), "gc", 0)

  // Build the $262 object
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.from_list([
          #("global", value.builtin_property(JsObject(global_ref))),
          #("evalScript", value.builtin_property(JsObject(eval_script_fn))),
          #("createRealm", value.builtin_property(JsObject(create_realm_fn))),
          #("gc", value.builtin_property(JsObject(gc_fn))),
          // __realm__ is non-enumerable internal property
          #(
            "__realm__",
            value.data(JsObject(realm_ref)) |> value.configurable(),
          ),
        ]),
        symbol_properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(b.object.prototype),
        extensible: True,
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}
