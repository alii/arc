import arc/compiler
import arc/parser
import arc/vm/builtins
import arc/vm/builtins/arc as builtins_arc
import arc/vm/builtins/common.{type Builtins}
import arc/vm/completion.{NormalCompletion, ThrowCompletion, YieldCompletion}
import arc/vm/exec/event_loop
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/tuple_array
import arc/vm/ops/object
import arc/vm/state.{
  type Heap, type NativeFnSlot, type State, type StepResult, type VmError, State,
}
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, DataProperty, JsObject, JsString,
  JsUndefined, Named, ObjectSlot, OrdinaryObject,
}
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set
import gleam/string

// ============================================================================
// Callback types for VM functions that can't be imported directly
// ============================================================================

pub type ExecuteInnerFn =
  fn(State) -> Result(#(completion.Completion, State), VmError)

pub type CallNativeFn =
  fn(State, NativeFnSlot, List(JsValue), List(JsValue), JsValue) ->
    Result(State, #(StepResult, JsValue, Heap))

pub type NewStateFn =
  fn(
    FuncTemplate,
    tuple_array.TupleArray(JsValue),
    Heap,
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
@external(javascript, "./arc_vm_ffi.mjs", "spawn")
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
      )) -> {
        // GC the heap before spawning — only keep builtins (permanently rooted)
        // and the closure's captured environment. This avoids copying the entire
        // parent heap (with all its temporaries) into every spawned process.
        let spawn_heap =
          heap.collect_with_roots(state.heap, set.from_list([env_ref.id]))
        Ok(fn() {
          run_spawned_closure(
            callee_template:,
            env_ref:,
            heap: spawn_heap,
            builtins: state.builtins,
            global_object: state.global_object,
            lexical_globals: state.lexical_globals,
            const_lexical_globals: state.const_lexical_globals,
            symbol_descriptions: state.symbol_descriptions,
            symbol_registry: state.symbol_registry,
            event_loop: state.event_loop,
            execute_inner:,
            new_state_fn: new_state,
          )
        })
      }
      Some(ObjectSlot(kind: value.NativeFunction(native), ..)) -> {
        let spawn_heap = heap.collect(state.heap)
        Ok(fn() {
          run_spawned_native(
            native:,
            caller_func: state.func,
            heap: spawn_heap,
            builtins: state.builtins,
            global_object: state.global_object,
            lexical_globals: state.lexical_globals,
            const_lexical_globals: state.const_lexical_globals,
            symbol_descriptions: state.symbol_descriptions,
            symbol_registry: state.symbol_registry,
            event_loop: state.event_loop,
            call_native: call_native,
            new_state_fn: new_state,
          )
        })
      }
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
    Error(msg) -> state.type_error(state, msg)
  }
}

/// Run a JS closure in a standalone BEAM process. Sets up a fresh VM
/// state from the snapshot and executes the function to completion.
fn run_spawned_closure(
  callee_template callee_template: FuncTemplate,
  env_ref env_ref: value.Ref,
  heap heap: Heap,
  builtins builtins: Builtins,
  global_object global_object: Ref,
  lexical_globals lexical_globals: dict.Dict(String, JsValue),
  const_lexical_globals const_lexical_globals: set.Set(String),
  symbol_descriptions symbol_descriptions: dict.Dict(value.SymbolId, String),
  symbol_registry symbol_registry: dict.Dict(String, value.SymbolId),
  event_loop event_loop: Bool,
  execute_inner execute_inner: ExecuteInnerFn,
  new_state_fn new_state_fn: NewStateFn,
) -> Nil {
  let env_values = heap.read_env(heap, env_ref) |> option.unwrap([])
  let env_count = list.length(env_values)
  let remaining =
    callee_template.local_count - env_count - callee_template.arity
  let padded_args = list.repeat(JsUndefined, callee_template.arity)
  let locals =
    list.flatten([env_values, padded_args, list.repeat(JsUndefined, remaining)])
    |> tuple_array.from_list

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
  native native: NativeFnSlot,
  caller_func caller_func: FuncTemplate,
  heap heap: Heap,
  builtins builtins: Builtins,
  global_object global_object: Ref,
  lexical_globals lexical_globals: dict.Dict(String, JsValue),
  const_lexical_globals const_lexical_globals: set.Set(String),
  symbol_descriptions symbol_descriptions: dict.Dict(value.SymbolId, String),
  symbol_registry symbol_registry: dict.Dict(String, value.SymbolId),
  event_loop event_loop: Bool,
  call_native call_native: CallNativeFn,
  new_state_fn new_state_fn: NewStateFn,
) -> Nil {
  let locals = tuple_array.repeat(JsUndefined, caller_func.local_count)
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
  use source_str, state <- state.try_to_string(state, source)

  // Read the __realm__ property from the $262 object to find the realm
  let realm_result = case this {
    JsObject(this_ref) ->
      case object.get_own_property(state.heap, this_ref, Named("__realm__")) {
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
    Error(msg) -> state.type_error(state, msg)
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
              let locals = tuple_array.repeat(JsUndefined, template.local_count)
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
                  state.type_error(
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
                      state.type_error(state, "evalScript: unexpected yield")
                    completion.AwaitCompletion(_, _) ->
                      state.type_error(state, "evalScript: unexpected await")
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
    object.set_property(
      h,
      new_global_ref,
      Named("$262"),
      JsObject(dollar_262_ref),
    )

  // Register the realm's builtins
  let realms = dict.insert(state.realms, realm_ref, new_builtins)

  #(State(..state, heap: h, realms:), Ok(JsObject(dollar_262_ref)))
}

/// Build a $262 object with evalScript, createRealm, gc methods and a global
/// property. The realm_ref points to a RealmSlot on the heap.
/// Public so test262_exec.gleam can use it for initial test setup.
pub fn build_262(
  h: Heap,
  b: Builtins,
  global_ref: Ref,
  realm_ref: Ref,
) -> #(Heap, Ref) {
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
          #(Named("global"), value.builtin_property(JsObject(global_ref))),
          #(
            Named("evalScript"),
            value.builtin_property(JsObject(eval_script_fn)),
          ),
          #(
            Named("createRealm"),
            value.builtin_property(JsObject(create_realm_fn)),
          ),
          #(Named("gc"), value.builtin_property(JsObject(gc_fn))),
          // __realm__ is non-enumerable internal property
          #(
            Named("__realm__"),
            value.data(JsObject(realm_ref)) |> value.configurable(),
          ),
        ]),
        symbol_properties: [],
        elements: elements.new(),
        prototype: Some(b.object.prototype),
        extensible: True,
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

// ============================================================================
// eval() and Function() constructor — runtime code evaluation
// ============================================================================

/// Parse + compile + execute source in an isolated global-scope state built
/// from the current realm. Threads heap/globals/job_queue back to caller.
/// Shared core for eval_native and function_constructor_native.
fn run_source_in_current_realm(
  source: String,
  state: State,
  execute_inner: ExecuteInnerFn,
  new_state_fn: NewStateFn,
) -> #(State, Result(JsValue, JsValue)) {
  case parser.parse(source, parser.Script) {
    Error(err) -> {
      let #(h, syntax_err) =
        common.make_syntax_error(
          state.heap,
          state.builtins,
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
              state.builtins,
              string.inspect(err),
            )
          #(State(..state, heap: h), Error(syntax_err))
        }
        Ok(template) -> {
          let locals = tuple_array.repeat(JsUndefined, template.local_count)
          let eval_state =
            State(
              ..new_state_fn(
                template,
                locals,
                state.heap,
                state.builtins,
                state.global_object,
                state.lexical_globals,
                state.const_lexical_globals,
                state.symbol_descriptions,
                state.symbol_registry,
                state.event_loop,
              ),
              job_queue: state.job_queue,
              realms: state.realms,
              // §19.2.1.1 PerformEval: indirect eval runs in global scope,
              // so its `this` is the global object.
              this_binding: JsObject(state.global_object),
            )
          case execute_inner(eval_state) {
            Error(vm_err) ->
              state.type_error(
                state,
                "eval: VM error: " <> string.inspect(vm_err),
              )
            Ok(#(completion, final_state)) -> {
              // Thread VM-global state back to caller
              let state =
                State(
                  ..state.merge_globals(state, final_state, []),
                  heap: final_state.heap,
                  symbol_descriptions: final_state.symbol_descriptions,
                  symbol_registry: final_state.symbol_registry,
                  realms: final_state.realms,
                )
              case completion {
                NormalCompletion(val, _) -> #(state, Ok(val))
                ThrowCompletion(thrown, _) -> #(state, Error(thrown))
                YieldCompletion(_, _) ->
                  state.type_error(state, "eval: unexpected yield")
                completion.AwaitCompletion(_, _) ->
                  state.type_error(state, "eval: unexpected await")
              }
            }
          }
        }
      }
  }
}

/// ES2024 §19.2.1 eval ( x )
/// Indirect eval — runs in global scope only, no access to caller's locals.
/// If x is not a string, returns x unchanged.
pub fn eval_native(
  args: List(JsValue),
  state: State,
  execute_inner: ExecuteInnerFn,
  new_state_fn: NewStateFn,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [JsString(source), ..] ->
      run_source_in_current_realm(source, state, execute_inner, new_state_fn)
    // Non-string first arg: return it unchanged (spec §19.2.1 step 2)
    [x, ..] -> #(state, Ok(x))
    [] -> #(state, Ok(JsUndefined))
  }
}

/// ES2024 §19.2.1.1 PerformEval — the DIRECT eval path.
/// Runs with access to the caller's local variables via boxed-local aliasing:
/// the caller's FuncTemplate.local_names maps name→slot-index, and each such
/// slot holds a BoxSlot ref. We compile the eval'd source with those names as
/// pre-boxed captures in slots 0..N-1, then seed those slots with the caller's
/// box refs so GetBoxed/PutBoxed alias the same heap cells.
///
/// Falls back to indirect eval when the caller has no local_names (e.g. the
/// eval call is at top-level or the compiler couldn't build the table).
pub fn direct_eval_native(
  args: List(JsValue),
  state: State,
  execute_inner: ExecuteInnerFn,
  new_state_fn: NewStateFn,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [JsString(source), ..] ->
      case state.func.local_names {
        None ->
          // No name table: caller wasn't marked (e.g. top-level). Fall back
          // to indirect eval semantics.
          run_source_in_current_realm(
            source,
            state,
            execute_inner,
            new_state_fn,
          )
        Some(name_table) ->
          run_direct_eval(
            source,
            name_table,
            state,
            execute_inner,
            new_state_fn,
          )
      }
    // Non-string first arg: return it unchanged (spec §19.2.1 step 2)
    [x, ..] -> #(state, Ok(x))
    [] -> #(state, Ok(JsUndefined))
  }
}

fn run_direct_eval(
  source: String,
  name_table: List(#(String, Int)),
  state: State,
  execute_inner: ExecuteInnerFn,
  new_state_fn: NewStateFn,
) -> #(State, Result(JsValue, JsValue)) {
  case parser.parse(source, parser.Script) {
    Error(err) -> {
      let #(h, syntax_err) =
        common.make_syntax_error(
          state.heap,
          state.builtins,
          parser.parse_error_to_string(err),
        )
      #(State(..state, heap: h), Error(syntax_err))
    }
    Ok(program) -> {
      // Compile with caller's local names as pre-boxed captures. The eval'd
      // code's slot i corresponds to name_table[i]'s variable.
      let parent_names = list.map(name_table, fn(pair) { pair.0 })
      let caller_strict = state.func.is_strict
      case compiler.compile_eval_direct(program, parent_names, caller_strict) {
        Error(err) -> {
          let #(h, syntax_err) =
            common.make_syntax_error(
              state.heap,
              state.builtins,
              string.inspect(err),
            )
          #(State(..state, heap: h), Error(syntax_err))
        }
        Ok(template) -> {
          // Seed locals[0..N-1] with the caller's box refs (pulled from
          // caller's locals at the indices in name_table). Remaining slots
          // default to undefined.
          let caller_box_refs =
            list.map(name_table, fn(pair) {
              tuple_array.get(pair.1, state.locals)
              |> option.unwrap(JsUndefined)
            })
          let remaining = template.local_count - list.length(caller_box_refs)
          let locals =
            list.append(caller_box_refs, list.repeat(JsUndefined, remaining))
            |> tuple_array.from_list
          // Sloppy: `var` declarations land in the caller's eval_env dict.
          // Allocate lazily so subsequent evals in the same frame share it.
          // Strict: compile_eval_direct rewrites those vars to locals in the
          // eval body, so no eval_env needed.
          let #(h, eval_env) = case caller_strict, state.eval_env {
            True, _ -> #(state.heap, None)
            False, Some(ref) -> #(state.heap, Some(ref))
            False, None -> {
              let #(h, ref) =
                heap.alloc(state.heap, value.EvalEnvSlot(dict.new()))
              #(h, Some(ref))
            }
          }
          let eval_state =
            State(
              ..new_state_fn(
                template,
                locals,
                h,
                state.builtins,
                state.global_object,
                state.lexical_globals,
                state.const_lexical_globals,
                state.symbol_descriptions,
                state.symbol_registry,
                state.event_loop,
              ),
              job_queue: state.job_queue,
              realms: state.realms,
              // Direct eval inherits the caller's `this` (spec §19.2.1.1
              // step 27.a — the calling context's LexicalEnvironment).
              this_binding: state.this_binding,
              eval_env:,
            )
          case execute_inner(eval_state) {
            Error(vm_err) ->
              state.type_error(
                state,
                "eval: VM error: " <> string.inspect(vm_err),
              )
            Ok(#(completion, final_state)) -> {
              let state =
                State(
                  ..state.merge_globals(state, final_state, []),
                  heap: final_state.heap,
                  symbol_descriptions: final_state.symbol_descriptions,
                  symbol_registry: final_state.symbol_registry,
                  realms: final_state.realms,
                  eval_env:,
                )
              case completion {
                NormalCompletion(val, _) -> #(state, Ok(val))
                ThrowCompletion(thrown, _) -> #(state, Error(thrown))
                YieldCompletion(_, _) ->
                  state.type_error(state, "eval: unexpected yield")
                completion.AwaitCompletion(_, _) ->
                  state.type_error(state, "eval: unexpected await")
              }
            }
          }
        }
      }
    }
  }
}

/// Coerce a list of JsValues to strings, threading state.
fn coerce_all_to_string(
  args: List(JsValue),
  state: State,
  acc: List(String),
) -> Result(#(List(String), State), #(JsValue, State)) {
  case args {
    [] -> Ok(#(list.reverse(acc), state))
    [arg, ..rest] -> {
      use #(str, state) <- result.try(state.to_string(state, arg))
      coerce_all_to_string(rest, state, [str, ..acc])
    }
  }
}

/// ES2024 §20.2.1.1 Function ( ...parameterArgs, bodyArg )
/// Last arg is the body, preceding args are parameter names.
/// Builds a function expression source string and evaluates it.
/// Same behavior for `Function(...)` and `new Function(...)`.
pub fn function_constructor_native(
  args: List(JsValue),
  state: State,
  execute_inner: ExecuteInnerFn,
  new_state_fn: NewStateFn,
) -> #(State, Result(JsValue, JsValue)) {
  use str_args, state <- state.try_op(coerce_all_to_string(args, state, []))
  let #(param_strs, body) = case list.reverse(str_args) {
    [] -> #([], "")
    [b, ..params_rev] -> #(list.reverse(params_rev), b)
  }
  let source =
    "(function anonymous("
    <> string.join(param_strs, ",")
    <> ") {\n"
    <> body
    <> "\n})"
  run_source_in_current_realm(source, state, execute_inner, new_state_fn)
}
