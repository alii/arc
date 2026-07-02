import arc/compiler
import arc/compiler/scope
import arc/parser
import arc/parser/ast
import arc/vm/builtins
import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/object as builtins_object
import arc/vm/builtins/promise as builtins_promise
import arc/vm/completion.{NormalCompletion, ThrowCompletion, YieldCompletion}
import arc/vm/exec/event_loop
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/tuple_array
import arc/vm/opcode
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, type VmError, State}
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, DataProperty, JsObject, JsString,
  JsUndefined, Named, ObjectSlot, OrdinaryObject,
}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

// ============================================================================
// Callback types for VM functions that can't be imported directly
// ============================================================================

pub type ExecuteInnerFn(host) =
  fn(State(host)) -> Result(#(completion.Completion, State(host)), VmError)

/// Inline of interpreter.init_top_level_locals (realm.gleam can't import
/// interpreter — cycle). JsUndefined everywhere, then seed the `this` slot.
fn seed_top_level_locals(
  template: FuncTemplate,
  this_val: JsValue,
) -> tuple_array.TupleArray(JsValue) {
  let locals = tuple_array.repeat(JsUndefined, template.local_count)
  case template.lexical.this {
    Some(idx) -> tuple_array.set_unchecked(idx, this_val, locals)
    None -> locals
  }
}

pub type NewStateFn(host) =
  fn(
    FuncTemplate,
    tuple_array.TupleArray(JsValue),
    Heap(host),
    Builtins,
    Ref,
    dict.Dict(String, value.LexicalGlobal),
    dict.Dict(value.SymbolId, String),
    dict.Dict(String, value.SymbolId),
    state.HostHooks,
  ) -> State(host)

// ============================================================================
// $262 — test262 host-defined realm functions
// ============================================================================

/// $262.evalScript(source) — parse and execute a script in the realm
/// associated with the $262 object's __realm__ property.
pub fn eval_script_native(
  args: List(JsValue),
  this: JsValue,
  state: State(host),
  execute_inner: ExecuteInnerFn(host),
  new_state_fn: NewStateFn(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let source = case args {
    [s, ..] -> s
    [] -> JsUndefined
  }
  use source_str, state <- coerce.try_to_string(state, source)

  // Read the __realm__ property from the $262 object to find the realm
  let realm_result = case this {
    JsObject(this_ref) ->
      case object.get_own_property(state.heap, this_ref, Named("__realm__")) {
        Some(DataProperty(value: JsObject(realm_ref), ..)) ->
          case heap.read(state.heap, realm_ref) {
            Some(value.RealmSlot(
              global_object: realm_global,
              lexical_globals:,
              symbol_descriptions:,
              symbol_registry:,
            )) ->
              case dict.get(state.ctx.realms, realm_ref) {
                Ok(realm_builtins) ->
                  Ok(#(
                    realm_builtins,
                    realm_global,
                    realm_ref,
                    lexical_globals,
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
      symbol_descriptions,
      symbol_registry,
    )) -> {
      use template <- compile_or_throw(
        state,
        realm_builtins,
        source_str,
        parser.parse(_, parser.Script),
        compiler.compile_eval,
      )
      // §16.1.6 ScriptEvaluation: script `this` is the realm's global object.
      let locals = seed_top_level_locals(template, JsObject(realm_global))
      // Seed the pending job queue and outstanding host-promise count from
      // the caller. NOT `seed_child`: this child ends in a nested,
      // non-yielding `drain_jobs`, so the caller's Atomics waiters and
      // pending unhandled-rejection reports must stay behind (see
      // `state.seed_draining_child`).
      let eval_state =
        state.seed_draining_child(
          new_state_fn(
            template,
            locals,
            state.heap,
            realm_builtins,
            realm_global,
            lexical_globals,
            symbol_descriptions,
            symbol_registry,
            // Child realm inherits the parent's embedder host capabilities.
            state.ctx.host_hooks,
          ),
          state,
        )
      let eval_state =
        State(
          ..eval_state,
          ctx: state.RealmCtx(
            ..eval_state.ctx,
            realms: state.ctx.realms,
            // Tagged-template cache is keyed by globally unique site ids, so
            // it is shared across realms; thread it in and back out.
            template_objects: state.ctx.template_objects,
          ),
        )
      case execute_inner(eval_state) {
        Error(vm_err) ->
          state.type_error(
            state,
            "evalScript: VM error: " <> state.vm_error_message(vm_err),
          )
        Ok(#(completion, final_eval_state)) -> {
          // Drain microtasks in the eval realm
          let drained = event_loop.drain_jobs(final_eval_state)
          // Update the realm slot with potentially modified lexical globals
          let updated_realm =
            value.RealmSlot(
              global_object: realm_global,
              lexical_globals: drained.ctx.lexical_globals,
              symbol_descriptions: drained.ctx.symbol_descriptions,
              symbol_registry: drained.ctx.symbol_registry,
            )
          let h = heap.write(drained.heap, realm_ref, updated_realm)
          // Propagate the event-loop queues, heap and cross-realm ctx back to
          // the caller. NOT merge_globals: the child realm's lexical globals
          // belong in its RealmSlot (written above), not in the caller's ctx.
          let state =
            State(
              ..state.merge_draining_child(state, drained),
              heap: h,
              ctx: state.RealmCtx(
                ..state.ctx,
                realms: drained.ctx.realms,
                template_objects: drained.ctx.template_objects,
              ),
            )
          case completion {
            NormalCompletion(val) -> #(state, Ok(val))
            ThrowCompletion(thrown) -> #(state, Error(thrown))
            YieldCompletion(_) ->
              state.type_error(state, "evalScript: unexpected yield")
            completion.AwaitCompletion(_) ->
              state.type_error(state, "evalScript: unexpected await")
          }
        }
      }
    }
  }
}

/// $262.createRealm() — create a fresh realm and return its $262 object.
pub fn create_realm_native(
  _this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  let realms = dict.insert(state.ctx.realms, realm_ref, new_builtins)

  #(
    State(..state, heap: h, ctx: state.RealmCtx(..state.ctx, realms:)),
    Ok(JsObject(dollar_262_ref)),
  )
}

/// Build a $262 object with evalScript, createRealm, gc methods and a global
/// property. The realm_ref points to a RealmSlot on the heap.
/// Public so test262_exec.gleam can use it for initial test setup.
pub fn build_262(
  h: Heap(host),
  b: Builtins,
  global_ref: Ref,
  realm_ref: Ref,
) -> #(Heap(host), Ref) {
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
  let #(h, detach_fn) =
    common.alloc_native_fn(
      h,
      func_proto,
      value.ArrayBufferNative(value.DetachArrayBuffer262),
      "detachArrayBuffer",
      1,
    )
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
          #(
            Named("detachArrayBuffer"),
            value.builtin_property(JsObject(detach_fn)),
          ),
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
  // Apply the embedder's $262 extension hook (if registered) — this is how
  // the test262 harness installs its host-side `agent` object on the
  // initial $262, every $262.createRealm() child, and every realm a
  // spawned agent process boots.
  let h = case get_extend_262() {
    Ok(extend) -> extend(h, b, ref)
    Error(Nil) -> h
  }
  #(h, ref)
}

// ============================================================================
// $262 extension hook — embedder-injected extras on every $262 object.
//
// test262's $262.agent.* API is HOST machinery (INTERPRETING.md), not VM
// core: real agent processes block on their mailboxes for broadcasts and
// wake messages, which is embedder territory (the same boundary as the
// Atomics host capabilities — see arc/host.gleam). The test262 harness
// registers a hook here (process-local, like the CanBlock flag read at
// realm boot — see arc_atomics_ffi.erl) and build_262 applies it to EVERY
// $262 it builds: the initial one, each $262.createRealm() child, and the
// $262 of each realm a spawned agent process boots (the harness re-registers
// the hook inside the agent child's process body). Embedders that register
// nothing get a plain $262 without `agent`.
// ============================================================================

/// Embedder extension applied to a freshly built (and rooted) $262 object:
/// receives the heap, the realm's builtins and the $262 ref, and returns
/// the heap with any extra properties installed.
pub type Extend262(host) =
  fn(Heap(host), Builtins, Ref) -> Heap(host)

/// Register the process-local $262 extension hook (a data-only process-
/// dictionary write — see arc_realm_ffi.erl). Freshly spawned processes
/// start with no hook; per-process embedder setup (the harness's per-test
/// worker, an agent child body) must register it before booting a realm.
@external(erlang, "arc_realm_ffi", "set_extend_262")
pub fn set_extend_262(hook: Extend262(host)) -> Nil

/// The registered hook, or Error(Nil) when this process never set one.
@external(erlang, "arc_realm_ffi", "get_extend_262")
fn get_extend_262() -> Result(Extend262(host), Nil)

// ============================================================================
// eval() and Function() constructor — runtime code evaluation
// ============================================================================

/// Run `parse` then `compile` on `source`. On parse or compile failure,
/// allocate a SyntaxError using `builtins` and return it as a thrown
/// completion. CPS so callers write `use template <- compile_or_throw(...)`.
/// `parse` is injected so direct-eval can seed parser context (new.target
/// permission today; full SyntaxPerms in P11) while indirect-eval / Function
/// constructor stay context-free.
fn compile_or_throw(
  state: State(host),
  builtins: Builtins,
  source: String,
  parse: fn(String) ->
    Result(#(ast.Program, scope.ScopeBuilder), parser.ParseError),
  compile: fn(ast.Program, scope.ScopeBuilder) ->
    Result(FuncTemplate, compiler.CompileError),
  cont: fn(FuncTemplate) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let throw_syntax = fn(msg) {
    let #(err, state) =
      state.error_value_with_builtins(state, builtins, state.SyntaxErr, msg)
    #(state, Error(err))
  }
  // Big sources parse+compile in a heap-sized scratch process (see
  // arc_vm_ffi:run_compile_task/2): the token list / AST / IR are large
  // transients the copying GC would otherwise re-copy many times, and only
  // the compact FuncTemplate (or error string) crosses back. Small sources
  // run inline.
  let compiled =
    ffi_run_compile_task(string.byte_size(source), fn() {
      case parse(source) {
        Error(err) -> Error(parser.parse_error_to_string(err))
        Ok(#(program, sb)) ->
          case compile(program, sb) {
            Error(err) -> Error(compiler.error_message(err))
            Ok(template) -> Ok(template)
          }
      }
    })
  case compiled {
    Error(msg) -> throw_syntax(msg)
    Ok(template) -> cont(template)
  }
}

/// See arc_vm_ffi:run_compile_task/2 — runs `task` in a short-lived,
/// heap-pre-sized process when `source_bytes` is large, inline otherwise.
@external(erlang, "arc_vm_ffi", "run_compile_task")
fn ffi_run_compile_task(source_bytes: Int, task: fn() -> a) -> a

/// Build an eval State from `template`/`locals`/`h`, copy the caller's realm
/// table in, execute, merge VM-global state back into the caller, and map
/// the completion to a result. Shared core for indirect and direct eval.
///
/// `eval_env` is the sloppy direct-eval var dict the eval executes with —
/// None for indirect eval and strict direct eval. The caller's final state
/// gets `option.or(eval_env, state.eval_env)`: indirect eval passes None so
/// the caller's own eval_env is preserved, while direct eval threads back
/// the (possibly freshly allocated) env. Strict frames never carry an
/// eval_env (call setup resets it per frame; only sloppy direct eval
/// allocates one), so the strict-direct case also resolves to None.
fn run_eval(
  template: FuncTemplate,
  locals: tuple_array.TupleArray(JsValue),
  h: Heap(host),
  state: State(host),
  eval_env: option.Option(Ref),
  execute_inner: ExecuteInnerFn(host),
  new_state_fn: NewStateFn(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Seed the agent-wide event-loop queues from the caller: merge_globals
  // threads them back after execution, so starting from new_state's empty
  // defaults would drop the caller's pending waiters and rejection reports
  // and zero the outstanding host.suspend count.
  let eval_state =
    State(
      ..state.seed_child(
        new_state_fn(
          template,
          locals,
          h,
          state.builtins,
          state.ctx.global_object,
          state.ctx.lexical_globals,
          state.ctx.symbol_descriptions,
          state.ctx.symbol_registry,
          // The eval realm inherits the caller's embedder host capabilities.
          state.ctx.host_hooks,
        ),
        state,
      ),
      eval_env:,
    )
  let eval_state =
    State(
      ..eval_state,
      ctx: state.RealmCtx(
        ..eval_state.ctx,
        realms: state.ctx.realms,
        // Thread the tagged-template cache in (new_state_fn starts empty);
        // merge_globals threads it back out after execution.
        template_objects: state.ctx.template_objects,
      ),
    )
  case execute_inner(eval_state) {
    Error(vm_err) ->
      state.type_error(
        state,
        "eval: VM error: " <> state.vm_error_message(vm_err),
      )
    Ok(#(completion, final_state)) -> {
      // Thread VM-global state back to caller. merge_globals is the ONE
      // merge path for realm-context state (lexical globals, symbol tables,
      // template cache, realm registry) — only heap/eval_env need patching.
      let merged = state.merge_globals(state, final_state, [])
      let state =
        State(
          ..merged,
          heap: final_state.heap,
          eval_env: option.or(eval_env, state.eval_env),
        )
      case completion {
        NormalCompletion(val) -> #(state, Ok(val))
        ThrowCompletion(thrown) -> #(state, Error(thrown))
        YieldCompletion(_) -> state.type_error(state, "eval: unexpected yield")
        completion.AwaitCompletion(_) ->
          state.type_error(state, "eval: unexpected await")
      }
    }
  }
}

/// Parse + compile + execute source in an isolated global-scope state built
/// from the current realm. Threads heap/globals/job_queue back to caller.
/// Shared core for eval_native and function_constructor_native.
fn run_source_in_current_realm(
  source: String,
  state: State(host),
  execute_inner: ExecuteInnerFn(host),
  new_state_fn: NewStateFn(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use template <- compile_or_throw(
    state,
    state.builtins,
    source,
    parser.parse(_, parser.Script),
    compiler.compile_eval,
  )
  // §19.2.1.1 PerformEval: indirect eval runs in global scope,
  // so its `this` is the global object.
  let locals =
    seed_top_level_locals(template, JsObject(state.ctx.global_object))
  run_eval(
    template,
    locals,
    state.heap,
    state,
    None,
    execute_inner,
    new_state_fn,
  )
}

/// ES2024 §19.2.1 eval ( x )
/// Indirect eval — runs in global scope only, no access to caller's locals.
/// If x is not a string, returns x unchanged.
pub fn eval_native(
  args: List(JsValue),
  state: State(host),
  execute_inner: ExecuteInnerFn(host),
  new_state_fn: NewStateFn(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  param_scope_names: List(String),
  with_names: List(String),
  private_names: List(String),
  state: State(host),
  execute_inner: ExecuteInnerFn(host),
  new_state_fn: NewStateFn(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
            param_scope_names,
            with_names,
            private_names,
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
  param_scope_names: List(String),
  with_names: List(String),
  private_names: List(String),
  state: State(host),
  execute_inner: ExecuteInnerFn(host),
  new_state_fn: NewStateFn(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // A sentinel head entry marks the caller frame's VariableEnvironment as
  // the GLOBAL environment (script/REPL top level) — sloppy eval'd `var`
  // declarations then target the global object, not an eval_env dict.
  let #(caller_is_global, name_table) = case name_table {
    [#(sentinel, -1), ..rest] if sentinel == compiler.global_frame_sentinel -> #(
      True,
      rest,
    )
    _ -> #(False, name_table)
  }
  // Compile with caller's local names as pre-boxed captures. The eval'd
  // code's slot i corresponds to name_table[i]'s variable. The caller's
  // lexical `this` (if any) is threaded as one extra capture after the
  // named ones, so eval('this') aliases the caller's slot.
  let parent_names = list.map(name_table, fn(pair) { pair.0 })
  let parent_slots = state.func.lexical
  let perms = state.func.syntax_perms
  let caller_strict = state.func.is_strict
  use template <- compile_or_throw(
    state,
    state.builtins,
    source,
    parser.parse_direct_eval(
      _,
      allow_new_target: perms.new_target_allowed,
      allow_super_property: perms.super_prop_allowed,
      allow_super_call: perms.super_call_allowed,
      allow_arguments: perms.arguments_allowed,
      outer_private_names: private_names,
    ),
    fn(program, sb) {
      compiler.compile_eval_direct(
        program,
        sb,
        parent_names,
        parent_slots,
        perms,
        caller_strict,
        caller_is_global,
        param_scope_names,
        with_names,
        private_names,
      )
    },
  )
  // Seed locals[0..N-1] with the caller's box refs (pulled from caller's
  // locals at the indices in name_table), then the caller's lexical box refs
  // in canonical order (one per Some entry in parent_slots — same order
  // compile_eval_direct allocates capture slots). Remaining slots default to
  // undefined.
  let caller_box_refs =
    list.map(name_table, fn(pair) {
      tuple_array.get(pair.1, state.locals)
      |> option.unwrap(JsUndefined)
    })
  let lexical_box_refs =
    list.filter_map(opcode.all_lexical_refs, fn(ref) {
      use idx <- result.map(
        opcode.lexical_slot(parent_slots, ref) |> option.to_result(Nil),
      )
      tuple_array.get(idx, state.locals) |> option.unwrap(JsUndefined)
    })
  let caller_box_refs = list.append(caller_box_refs, lexical_box_refs)
  let remaining = template.local_count - list.length(caller_box_refs)
  let locals =
    list.append(caller_box_refs, list.repeat(JsUndefined, remaining))
    |> tuple_array.from_list
  // Sloppy: `var` declarations land in the caller's eval_env dict.
  // Allocate lazily so subsequent evals in the same frame share it.
  // Strict: compile_eval_direct rewrites those vars to locals in the
  // eval body, so no eval_env needed. Global caller: vars go straight to
  // the global object (fallthrough ToGlobal), so no eval_env either.
  let #(h, eval_env) = case caller_strict || caller_is_global, state.eval_env {
    True, _ -> #(state.heap, None)
    False, Some(ref) -> #(state.heap, Some(ref))
    False, None -> {
      let #(h, ref) = heap.alloc(state.heap, value.EvalEnvSlot(dict.new()))
      #(h, Some(ref))
    }
  }
  // Direct eval inherits the caller's `this` (spec §19.2.1.1 step 16.a)
  // via the boxed capture appended to caller_box_refs above.
  run_eval(template, locals, h, state, eval_env, execute_inner, new_state_fn)
}

/// Coerce a list of JsValues to strings, threading state.
fn coerce_all_to_string(
  args: List(JsValue),
  state: State(host),
  acc: List(String),
) -> Result(#(List(String), State(host)), #(JsValue, State(host))) {
  case args {
    [] -> Ok(#(list.reverse(acc), state))
    [arg, ..rest] -> {
      use #(str, state) <- result.try(coerce.js_to_string(state, arg))
      coerce_all_to_string(rest, state, [str, ..acc])
    }
  }
}

/// ES2024 §20.2.1.1 Function ( ...parameterArgs, bodyArg ) and its
/// CreateDynamicFunction (§20.2.1.1.1) siblings — `keyword` selects the
/// flavor: "function", "function*" (GeneratorFunction, §27.3.1.1) or
/// "async function*" (AsyncGeneratorFunction, §27.4.1.1).
/// Last arg is the body, preceding args are parameter names.
/// Builds a function expression source string and evaluates it.
/// Same behavior for `Ctor(...)` and `new Ctor(...)`.
pub fn function_constructor_native(
  args: List(JsValue),
  keyword: String,
  state: State(host),
  execute_inner: ExecuteInnerFn(host),
  new_state_fn: NewStateFn(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use str_args, state <- state.try_op(coerce_all_to_string(args, state, []))
  let #(param_strs, body) = case list.reverse(str_args) {
    [] -> #([], "")
    [b, ..params_rev] -> #(list.reverse(params_rev), b)
  }
  // §20.2.1.1.1 step 16 assembles "function anonymous(" P "\n) {\n" body
  // "\n}", but the spec then calls OrdinaryFunctionCreate directly — so
  // unlike a syntactic named function expression there is NO §13.2.5.5
  // self-name binding: `anonymous` must not resolve inside the body
  // (test262: staging/sm/Function/constructor-binding.js). We therefore
  // compile an ANONYMOUS function expression and apply step 29
  // SetFunctionName(F, "anonymous") to the result afterwards.
  // The newline before ")" matters: a trailing line comment in the last
  // parameter must not comment out the ")" (test262: Function/prototype/
  // toString/Function.js).
  let source =
    "("
    <> keyword
    <> "("
    <> string.join(param_strs, ",")
    <> "\n) {\n"
    <> body
    <> "\n})"
  let #(state, res) =
    run_source_in_current_realm(source, state, execute_inner, new_state_fn)
  case res {
    Ok(JsObject(fn_ref)) -> #(set_function_name_anonymous(state, fn_ref), res)
    _ -> #(state, res)
  }
}

/// §20.2.1.1.1 step 29: SetFunctionName(F, "anonymous") for a freshly
/// created dynamic function — sets both the own "name" data property
/// (created as "" at closure allocation) and the template name that
/// Function.prototype.toString reports.
fn set_function_name_anonymous(state: State(host), fn_ref: Ref) -> State(host) {
  let heap =
    heap.update(state.heap, fn_ref, fn(slot) {
      case slot {
        ObjectSlot(
          kind: value.FunctionObject(func_template:, env:, home_object:),
          properties:,
          ..,
        ) ->
          ObjectSlot(
            ..slot,
            kind: value.FunctionObject(
              func_template: value.FuncTemplate(
                ..func_template,
                name: Some("anonymous"),
              ),
              env:,
              home_object:,
            ),
            properties: dict.insert(
              properties,
              Named("name"),
              // Redefining an existing "name" keeps its creation seq
              // (§10.1.11 — the key keeps its enumeration position).
              case dict.get(properties, Named("name")) {
                Ok(old) ->
                  value.with_seq_of(common.fn_name_property("anonymous"), old)
                Error(Nil) -> common.fn_name_property("anonymous")
              },
            ),
          )
        other -> other
      }
    })
  State(..state, heap:)
}

// ============================================================================
// ShadowRealm (proposal-shadowrealm)
//
// A ShadowRealm instance owns a fresh realm: its own Builtins, global object
// and RealmSlot (the same realm machinery $262.createRealm uses).
// `evaluate` runs a script in that realm and returns the completion value
// through GetWrappedValue: primitives cross the boundary as-is, callables
// cross as wrapped function exotic objects, anything else is a TypeError.
// ============================================================================

/// Per-realm data resolved from a RealmSlot ref + the ctx.realms table.
type RealmRecord {
  RealmRecord(
    builtins: Builtins,
    global: Ref,
    lexical_globals: dict.Dict(String, value.LexicalGlobal),
    symbol_descriptions: dict.Dict(value.SymbolId, String),
    symbol_registry: dict.Dict(String, value.SymbolId),
  )
}

/// Route ShadowRealm natives. Called from dispatch_native.
pub fn shadow_realm_dispatch(
  native: value.ShadowRealmNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
  execute_inner: ExecuteInnerFn(host),
  new_state_fn: NewStateFn(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    value.ShadowRealmConstructor(proto:) ->
      shadow_realm_constructor(proto, state)
    value.ShadowRealmEvaluate(fn_proto:) ->
      shadow_realm_evaluate(
        args,
        this,
        fn_proto,
        state,
        execute_inner,
        new_state_fn,
      )
    value.ShadowRealmImportValue(fn_proto:) ->
      shadow_realm_import_value(args, this, fn_proto, state)
    value.WrappedFunctionCall(target:, caller_realm:, target_realm:) ->
      wrapped_function_call(
        target,
        caller_realm,
        target_realm,
        args,
        this,
        state,
      )
  }
}

/// ShadowRealm ( ) — proposal §3.1.1. Creates the instance and its realm.
fn shadow_realm_constructor(
  proto: Ref,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: If NewTarget is undefined, throw a TypeError.
  case state.new_target {
    JsUndefined ->
      state.type_error(state, "Constructor ShadowRealm requires 'new'")
    new_target -> {
      // Step 2: OrdinaryCreateFromConstructor(NewTarget, %ShadowRealm.prototype%).
      use proto_ref, state <- object.proto_from_new_target(
        state,
        new_target,
        proto,
      )
      // Steps 3-12: CreateRealm + SetRealmGlobalObject + SetDefaultGlobalBindings.
      let #(h, new_builtins) = builtins.init(state.heap)
      let #(h, new_global) = builtins.globals(new_builtins, h)
      let #(h, realm_ref) =
        heap.alloc(
          h,
          value.RealmSlot(
            global_object: new_global,
            lexical_globals: dict.new(),
            symbol_descriptions: dict.new(),
            symbol_registry: dict.new(),
          ),
        )
      let h = heap.root(h, realm_ref)
      let #(h, instance_ref) =
        common.alloc_wrapper(h, value.ShadowRealmObject(realm_ref:), proto_ref)
      let realms = dict.insert(state.ctx.realms, realm_ref, new_builtins)
      #(
        State(..state, heap: h, ctx: state.RealmCtx(..state.ctx, realms:)),
        Ok(JsObject(instance_ref)),
      )
    }
  }
}

/// Brand check: read the [[ShadowRealm]] slot off `this`.
fn shadow_realm_of(state: State(host), this: JsValue) -> Result(Ref, Nil) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.ShadowRealmObject(realm_ref:), ..)) ->
          Ok(realm_ref)
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

/// Resolve a realm ref into its record (RealmSlot fields + Builtins).
fn read_realm(state: State(host), realm_ref: Ref) -> Result(RealmRecord, Nil) {
  case heap.read(state.heap, realm_ref) {
    Some(value.RealmSlot(
      global_object:,
      lexical_globals:,
      symbol_descriptions:,
      symbol_registry:,
    )) ->
      case dict.get(state.ctx.realms, realm_ref) {
        Ok(b) ->
          Ok(RealmRecord(
            builtins: b,
            global: global_object,
            lexical_globals:,
            symbol_descriptions:,
            symbol_registry:,
          ))
        Error(Nil) -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

/// Build a RealmSlot snapshot of the realm the state is currently running in.
fn current_realm_slot(state: State(host)) -> state.HeapSlot(host) {
  value.RealmSlot(
    global_object: state.ctx.global_object,
    lexical_globals: state.ctx.lexical_globals,
    symbol_descriptions: state.ctx.symbol_descriptions,
    symbol_registry: state.ctx.symbol_registry,
  )
}

/// Persist the current ctx into `realm_ref`'s RealmSlot.
fn sync_realm_slot(state: State(host), realm_ref: Ref) -> State(host) {
  State(
    ..state,
    heap: heap.write(state.heap, realm_ref, current_realm_slot(state)),
  )
}

/// Find the RealmSlot whose global object is `global`.
fn find_realm_by_global(state: State(host), global: Ref) -> option.Option(Ref) {
  list.find(dict.keys(state.ctx.realms), fn(rref) {
    case heap.read(state.heap, rref) {
      Some(value.RealmSlot(global_object: g, ..)) -> g == global
      _ -> False
    }
  })
  |> option.from_result
}

/// Return a RealmSlot ref for the realm the state is currently executing in,
/// allocating + registering one if this realm was never reified (e.g. the
/// top-level realm outside the test262 harness). Always syncs the slot with
/// the live ctx so cross-realm re-entry sees fresh lexical globals.
fn ensure_current_realm(state: State(host)) -> #(State(host), Ref) {
  case find_realm_by_global(state, state.ctx.global_object) {
    Some(rref) -> #(sync_realm_slot(state, rref), rref)
    None -> {
      let #(h, rref) = heap.alloc(state.heap, current_realm_slot(state))
      let h = heap.root(h, rref)
      let realms = dict.insert(state.ctx.realms, rref, state.builtins)
      #(
        State(..state, heap: h, ctx: state.RealmCtx(..state.ctx, realms:)),
        rref,
      )
    }
  }
}

/// Resolve the realm a ShadowRealm method belongs to from the
/// %Function.prototype% ref its native token carries (unique per Builtins).
/// Per spec, built-in methods run in their own realm — its intrinsics brand
/// every error and wrapper evaluate/importValue produce. Falls back to the
/// running realm when the marker realm was never reified.
fn realm_of_function_proto(
  state: State(host),
  fn_proto: Ref,
) -> #(State(host), Ref, Builtins) {
  case state.builtins.function.prototype == fn_proto {
    True -> {
      let #(state, rref) = ensure_current_realm(state)
      #(state, rref, state.builtins)
    }
    False -> {
      let found =
        dict.fold(state.ctx.realms, None, fn(acc, rref, b) {
          case acc {
            Some(_) -> acc
            None ->
              case b.function.prototype == fn_proto {
                True -> Some(#(rref, b))
                False -> None
              }
          }
        })
      case found {
        Some(#(rref, b)) -> #(state, rref, b)
        None -> {
          let #(state, rref) = ensure_current_realm(state)
          #(state, rref, state.builtins)
        }
      }
    }
  }
}

/// Run `f` with the VM's realm context switched to `realm_ref` (builtins,
/// global object, lexical globals, symbol tables), then restore the original
/// realm. Mutations on either side are persisted through the RealmSlot heap
/// cells, so nested cross-realm calls observe each other's changes.
fn with_realm(
  state: State(host),
  realm_ref: Ref,
  f: fn(State(host)) -> #(State(host), Result(a, JsValue)),
) -> #(State(host), Result(a, JsValue)) {
  case read_realm(state, realm_ref) {
    Error(Nil) -> {
      let #(err, state) =
        state.type_error_value(
          state,
          "ShadowRealm: realm record missing for cross-realm call",
        )
      #(state, Error(err))
    }
    Ok(target) ->
      case target.global == state.ctx.global_object {
        True -> f(state)
        False -> {
          let #(state, origin_ref) = ensure_current_realm(state)
          let origin_builtins = state.builtins
          // Symbol registry/descriptions are agent-wide — enter with the
          // union so registered symbols keep their identity across realms.
          let entered =
            State(
              ..state,
              builtins: target.builtins,
              ctx: state.RealmCtx(
                ..state.ctx,
                global_object: target.global,
                lexical_globals: target.lexical_globals,
                symbol_descriptions: dict.merge(
                  state.ctx.symbol_descriptions,
                  target.symbol_descriptions,
                ),
                symbol_registry: dict.merge(
                  state.ctx.symbol_registry,
                  target.symbol_registry,
                ),
              ),
            )
          let #(after, res) = f(entered)
          // Persist the target realm's (possibly mutated) globals.
          let after = sync_realm_slot(after, realm_ref)
          // Restore the origin realm, re-reading its slot — nested calls
          // back into the origin may have mutated it. Symbol tables adopt
          // the after-state's union.
          let restored = case heap.read(after.heap, origin_ref) {
            Some(value.RealmSlot(
              global_object:,
              lexical_globals:,
              symbol_descriptions:,
              symbol_registry:,
            )) ->
              State(
                ..after,
                builtins: origin_builtins,
                ctx: state.RealmCtx(
                  ..after.ctx,
                  global_object:,
                  lexical_globals:,
                  symbol_descriptions: dict.merge(
                    symbol_descriptions,
                    after.ctx.symbol_descriptions,
                  ),
                  symbol_registry: dict.merge(
                    symbol_registry,
                    after.ctx.symbol_registry,
                  ),
                ),
              )
            _ -> State(..after, builtins: origin_builtins)
          }
          #(restored, res)
        }
      }
  }
}

/// GetWrappedValue ( realm, value ) — proposal §3.1.4. `dest_realm` is the
/// realm the value is being passed INTO (the new wrapper's [[Realm]]),
/// `src_realm` is the realm the value comes from. TypeErrors use
/// `err_builtins` — the running caller context's realm per spec.
fn get_wrapped_value(
  state: State(host),
  dest_realm: Ref,
  dest_builtins: Builtins,
  err_builtins: Builtins,
  src_realm: Ref,
  val: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  case val {
    JsObject(_) ->
      case object.value_is_callable(state.heap, val) {
        True ->
          wrapped_function_create(
            state,
            val,
            src_realm,
            dest_realm,
            dest_builtins,
            err_builtins,
          )
        False ->
          state.type_error_with_builtins(
            state,
            err_builtins,
            "value crossing the ShadowRealm boundary must be callable or primitive",
          )
      }
    _ -> #(state, Ok(val))
  }
}

/// WrappedFunctionCreate ( callerRealm, Target ) — proposal §2.1.1, including
/// CopyNameAndLength (§2.2). Any abrupt completion from the observable Gets
/// on Target becomes a TypeError in `err_builtins`' realm.
fn wrapped_function_create(
  state: State(host),
  target: JsValue,
  src_realm: Ref,
  dest_realm: Ref,
  dest_builtins: Builtins,
  err_builtins: Builtins,
) -> #(State(host), Result(JsValue, JsValue)) {
  // The name/length Gets are observable (accessors run) — execute them in
  // the target's own realm so getter code resolves globals there.
  let #(state, copied) =
    with_realm(state, src_realm, fn(state) {
      copy_name_and_length(state, target)
    })
  case copied {
    Error(_thrown) ->
      state.type_error_with_builtins(
        state,
        err_builtins,
        "wrapped function could not copy target name and length",
      )
    Ok(#(name, len)) -> {
      let #(h, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: value.NativeFunction(
              value.Dispatch(
                value.ShadowRealmNative(value.WrappedFunctionCall(
                  target:,
                  caller_realm: dest_realm,
                  target_realm: src_realm,
                )),
              ),
              constructible: False,
            ),
            properties: dict.from_list([
              #(
                Named("length"),
                // seq: 0 — birth-time "length", pairs with fn_name_property's
                // constant seq 1 (see common.fn_name_property).
                value.DataProperty(
                  value: value.JsNumber(len),
                  writable: False,
                  enumerable: False,
                  configurable: True,
                  seq: 0,
                ),
              ),
              #(Named("name"), common.fn_name_property(name)),
            ]),
            symbol_properties: [],
            elements: elements.new(),
            prototype: Some(dest_builtins.function.prototype),
            extensible: True,
          ),
        )
      #(State(..state, heap: h), Ok(JsObject(ref)))
    }
  }
}

/// CopyNameAndLength ( F, Target ) — proposal §2.2, steps 2-7 (the reads).
/// Returns the name string and the length JsNum to define on the wrapper.
fn copy_name_and_length(
  state: State(host),
  target: JsValue,
) -> #(State(host), Result(#(String, value.JsNum), JsValue)) {
  case target {
    JsObject(tref) -> {
      // Step 3: targetHasLength = ? HasOwnProperty(Target, "length") —
      // via [[GetOwnProperty]] so proxy getOwnPropertyDescriptor traps
      // (and revoked proxies) are observable.
      use len_desc, state <- state.try_op(
        builtins_object.get_own_property_stateful(
          state,
          tref,
          JsString("length"),
        ),
      )
      let has_len = option.is_some(len_desc)
      // Step 4: if present, targetLen = ? Get(Target, "length").
      use len_val, state <- state.try_op(case has_len {
        True -> object.get_value(state, tref, Named("length"), target)
        False -> Ok(#(JsUndefined, state))
      })
      let len = case len_val {
        value.JsNumber(value.Infinity) -> value.Infinity
        value.JsNumber(value.NegInfinity) -> value.Finite(0.0)
        value.JsNumber(value.Finite(f)) -> {
          // ToIntegerOrInfinity then max(L, 0).
          let l = int.max(float.truncate(f), 0)
          value.Finite(int.to_float(l))
        }
        _ -> value.Finite(0.0)
      }
      // Step 6: targetName = ? Get(Target, "name"); non-strings become "".
      use name_val, state <- state.try_op(object.get_value(
        state,
        tref,
        Named("name"),
        target,
      ))
      let name = case name_val {
        JsString(s) -> s
        _ -> ""
      }
      #(state, Ok(#(name, len)))
    }
    _ -> #(state, Ok(#("", value.Finite(0.0))))
  }
}

/// ShadowRealm.prototype.evaluate ( sourceText ) — proposal §3.3.1.
fn shadow_realm_evaluate(
  args: List(JsValue),
  this: JsValue,
  fn_proto: Ref,
  state: State(host),
  execute_inner: ExecuteInnerFn(host),
  new_state_fn: NewStateFn(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // The method's own realm is the spec's callerRealm: it brands every error
  // and wrapper this call produces (a built-in runs in its own realm even
  // when invoked from another one).
  let #(state, caller_realm_ref, caller_builtins) =
    realm_of_function_proto(state, fn_proto)
  case shadow_realm_of(state, this) {
    Error(Nil) ->
      state.type_error_with_builtins(
        state,
        caller_builtins,
        "ShadowRealm.prototype.evaluate called on incompatible receiver",
      )
    Ok(realm_ref) ->
      // Step 3: If sourceText is not a String, throw a TypeError (no coercion).
      case args {
        [JsString(source), ..] ->
          do_shadow_realm_evaluate(
            source,
            realm_ref,
            caller_realm_ref,
            caller_builtins,
            state,
            execute_inner,
            new_state_fn,
          )
        _ ->
          state.type_error_with_builtins(
            state,
            caller_builtins,
            "ShadowRealm.prototype.evaluate expects a string",
          )
      }
  }
}

/// PerformShadowRealmEval — proposal §3.1.2. Parse in the caller context
/// (SyntaxErrors surface as the caller realm's SyntaxError), execute in the
/// shadow realm, wrap the completion value for the caller.
fn do_shadow_realm_evaluate(
  source: String,
  realm_ref: Ref,
  caller_realm_ref: Ref,
  caller_builtins: Builtins,
  state: State(host),
  execute_inner: ExecuteInnerFn(host),
  new_state_fn: NewStateFn(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case read_realm(state, realm_ref) {
    Error(Nil) ->
      state.type_error_with_builtins(
        state,
        caller_builtins,
        "ShadowRealm: realm record missing for evaluate",
      )
    Ok(realm) -> {
      use template <- compile_or_throw(
        state,
        caller_builtins,
        source,
        parser.parse(_, parser.Script),
        compiler.compile_eval,
      )
      // Sync the running realm's slot so re-entrant calls from the shadow
      // realm see fresh lexical globals.
      let #(state, _running_realm_ref) = ensure_current_realm(state)
      // Script `this` is the shadow realm's global object (§16.1.6).
      let locals = seed_top_level_locals(template, JsObject(realm.global))
      // The Symbol registry (§20.4.2.2 Symbol.for) and descriptions are
      // agent-wide, not per-realm — seed the shadow realm with the union so
      // symbols round-trip across the boundary with identity and description.
      let merged_descriptions =
        dict.merge(state.ctx.symbol_descriptions, realm.symbol_descriptions)
      let merged_registry =
        dict.merge(state.ctx.symbol_registry, realm.symbol_registry)
      // Seed the pending job queue and outstanding host-promise count from
      // the caller. NOT `seed_child`: this child ends in a nested,
      // non-yielding `drain_jobs`, so the caller's Atomics waiters and
      // pending unhandled-rejection reports must stay behind (see
      // `state.seed_draining_child`).
      let eval_state =
        state.seed_draining_child(
          new_state_fn(
            template,
            locals,
            state.heap,
            realm.builtins,
            realm.global,
            realm.lexical_globals,
            merged_descriptions,
            merged_registry,
            // The shadow realm inherits the caller's embedder host
            // capabilities.
            state.ctx.host_hooks,
          ),
          state,
        )
      let eval_state =
        State(
          ..eval_state,
          ctx: state.RealmCtx(
            ..eval_state.ctx,
            realms: state.ctx.realms,
            // Tagged-template cache is keyed by globally unique site ids, so
            // it is shared across realms; thread it in and back out.
            template_objects: state.ctx.template_objects,
          ),
        )
      case execute_inner(eval_state) {
        Error(vm_err) ->
          state.type_error(
            state,
            "ShadowRealm.prototype.evaluate: VM error: "
              <> state.vm_error_message(vm_err),
          )
        Ok(#(completion, final_eval_state)) -> {
          // Drain microtasks in the shadow realm.
          let drained = event_loop.drain_jobs(final_eval_state)
          let updated_realm =
            value.RealmSlot(
              global_object: realm.global,
              lexical_globals: drained.ctx.lexical_globals,
              symbol_descriptions: drained.ctx.symbol_descriptions,
              symbol_registry: drained.ctx.symbol_registry,
            )
          let h = heap.write(drained.heap, realm_ref, updated_realm)
          // Propagate the event-loop queues, heap and cross-realm ctx back to
          // the caller. NOT merge_globals: the shadow realm's lexical globals
          // belong in its RealmSlot (written above), not in the caller's ctx.
          // The drained symbol tables are a superset of the caller's (the
          // eval was seeded with the union) — adopt them agent-wide.
          let state =
            State(
              ..state.merge_draining_child(state, drained),
              heap: h,
              ctx: state.RealmCtx(
                ..state.ctx,
                realms: drained.ctx.realms,
                symbol_descriptions: drained.ctx.symbol_descriptions,
                symbol_registry: drained.ctx.symbol_registry,
                template_objects: drained.ctx.template_objects,
              ),
            )
          case completion {
            // Step 26: GetWrappedValue(callerRealm, result).
            NormalCompletion(val) ->
              get_wrapped_value(
                state,
                caller_realm_ref,
                caller_builtins,
                caller_builtins,
                realm_ref,
                val,
              )
            // Step 25: abrupt completions become the caller realm's TypeError.
            ThrowCompletion(thrown) ->
              state.type_error_with_builtins(
                state,
                caller_builtins,
                "ShadowRealm.prototype.evaluate threw: "
                  <> object.format_error(thrown, state.heap),
              )
            YieldCompletion(_) ->
              state.type_error(state, "evaluate: unexpected yield")
            completion.AwaitCompletion(_) ->
              state.type_error(state, "evaluate: unexpected await")
          }
        }
      }
    }
  }
}

/// Wrapped function exotic object [[Call]] — proposal §2.1.
fn wrapped_function_call(
  target: JsValue,
  caller_realm: Ref,
  target_realm: Ref,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Every TypeError thrown here belongs to F.[[Realm]] (the caller realm).
  let caller_builtins = case dict.get(state.ctx.realms, caller_realm) {
    Ok(b) -> b
    Error(Nil) -> state.builtins
  }
  let target_builtins = case dict.get(state.ctx.realms, target_realm) {
    Ok(b) -> b
    Error(Nil) -> state.builtins
  }
  // Steps 6-7: wrap thisArgument and every argument into the target realm.
  let #(state, wrapped_this_res) =
    get_wrapped_value(
      state,
      target_realm,
      target_builtins,
      caller_builtins,
      caller_realm,
      this,
    )
  case wrapped_this_res {
    Error(thrown) -> #(state, Error(thrown))
    Ok(wrapped_this) -> {
      let #(state, wrapped_args_res) =
        wrap_all(
          state,
          target_realm,
          target_builtins,
          caller_builtins,
          caller_realm,
          args,
          [],
        )
      case wrapped_args_res {
        Error(thrown) -> #(state, Error(thrown))
        Ok(wrapped_args) -> {
          // Step 8: Call(target, wrappedThisArgument, wrappedArgs) in the
          // target function's realm.
          let #(state, call_res) =
            with_realm(state, target_realm, fn(state) {
              case state.call(state, target, wrapped_this, wrapped_args) {
                Ok(#(v, state)) -> #(state, Ok(v))
                Error(#(thrown, state)) -> #(state, Error(thrown))
              }
            })
          case call_res {
            // Step 9: GetWrappedValue(callerRealm, result).
            Ok(result_val) ->
              get_wrapped_value(
                state,
                caller_realm,
                caller_builtins,
                caller_builtins,
                target_realm,
                result_val,
              )
            // Step 10: any abrupt completion becomes the caller realm's
            // TypeError (the original error must not cross the boundary).
            Error(thrown) ->
              state.type_error_with_builtins(
                state,
                caller_builtins,
                "wrapped function threw: "
                  <> object.format_error(thrown, state.heap),
              )
          }
        }
      }
    }
  }
}

/// GetWrappedValue over a list, short-circuiting on the first error.
fn wrap_all(
  state: State(host),
  dest_realm: Ref,
  dest_builtins: Builtins,
  err_builtins: Builtins,
  src_realm: Ref,
  vals: List(JsValue),
  acc: List(JsValue),
) -> #(State(host), Result(List(JsValue), JsValue)) {
  case vals {
    [] -> #(state, Ok(list.reverse(acc)))
    [v, ..rest] -> {
      let #(state, res) =
        get_wrapped_value(
          state,
          dest_realm,
          dest_builtins,
          err_builtins,
          src_realm,
          v,
        )
      case res {
        Ok(w) ->
          wrap_all(
            state,
            dest_realm,
            dest_builtins,
            err_builtins,
            src_realm,
            rest,
            [w, ..acc],
          )
        Error(thrown) -> #(state, Error(thrown))
      }
    }
  }
}

/// ShadowRealm.prototype.importValue ( specifier, exportName ) — §3.3.2.
/// Validation is fully implemented; the actual module load rejects, as a
/// host without a ShadowRealm module loader does (HostLoadImportedModule is
/// allowed to fail — the returned promise rejects with a TypeError).
fn shadow_realm_import_value(
  args: List(JsValue),
  this: JsValue,
  fn_proto: Ref,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // As in evaluate: the method's own realm brands errors and the promise.
  let #(state, _caller_realm_ref, caller_builtins) =
    realm_of_function_proto(state, fn_proto)
  case shadow_realm_of(state, this) {
    Error(Nil) ->
      state.type_error_with_builtins(
        state,
        caller_builtins,
        "ShadowRealm.prototype.importValue called on incompatible receiver",
      )
    Ok(_realm_ref) -> {
      let specifier = case args {
        [s, ..] -> s
        [] -> JsUndefined
      }
      let export_name = case args {
        [_, e, ..] -> e
        _ -> JsUndefined
      }
      // Step 3: ToString(specifier) — abrupt completions propagate as-is.
      use _specifier_str, state <- coerce.try_to_string(state, specifier)
      // Step 4: exportName must already be a String (no coercion).
      case export_name {
        JsString(_) -> {
          let #(err, state) =
            state.error_value_with_builtins(
              state,
              caller_builtins,
              state.TypeErr,
              "ShadowRealm.prototype.importValue: module loading is not "
                <> "available in this host",
            )
          let #(h, promise_ref, data_ref) =
            builtins_promise.create_promise(
              state.heap,
              caller_builtins.promise.prototype,
            )
          let state = State(..state, heap: h)
          let state = builtins_promise.reject_promise(state, data_ref, err)
          #(state, Ok(JsObject(promise_ref)))
        }
        _ ->
          state.type_error_with_builtins(
            state,
            caller_builtins,
            "ShadowRealm.prototype.importValue: exportName must be a string",
          )
      }
    }
  }
}
