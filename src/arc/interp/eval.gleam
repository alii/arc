//// Runtime code evaluation for the bytecode interpreter: §19.2.1.1
//// PerformEval in both flavours, §20.2.1.1.1 CreateDynamicFunction's
//// evaluation step, and §16.1.6 ScriptEvaluation for `$262.evalScript`.
////
//// Direct eval sees the calling frame. Every function that syntactically
//// contains `eval(...)` is compiled with all its locals boxed and carries an
//// `EvalNameTable` (name → slot); the eval source is compiled with those
//// names as pre-boxed captures in slots 0..N-1 (`compile_eval_direct`) and
//// its locals are seeded with the caller's box cells, so reads and writes
//// alias the caller's variables. Sloppy `var`s the eval introduces land in
//// the frame's eval env object (`rt/env`), allocated on first use.
////
//// The loop driver lives above this module (it imports the step function,
//// which imports this), so running a prepared activation to completion is
//// the caller-supplied `Run`.

import arc/compiler
import arc/compiler/scope
import arc/interp/state.{type State, State}
import arc/parser
import arc/parser/ast
import arc/rt/async as rt_async
import arc/rt/bytecode.{
  type FuncTemplate, EvalNameTable, FrameVarEnv, GlobalVarEnv,
}
import arc/rt/env as rt_env
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type EvalKind, type Handle, type JsVal, Agent, DynamicFunction,
  IndirectEval, JsStore, KBytecode, KHandle, KStr, RangeErr, SObject, ScriptEval,
  SyntaxErr, TypeErr, classify, mk_object, mk_undefined,
}
import arc/vm/compile_task
import arc/vm/internal/tuple_array.{type TupleArray}
import arc/vm/lexical
import arc/vm/limits
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Runs a freshly prepared activation until its own call stack empties and
/// reports how it completed: `Ok(value)` or `Error(thrown)`, with the agent
/// it finished in. Supplied by the loop driver.
pub type Run =
  fn(State) -> #(Result(JsVal, JsVal), Agent)

// -- Compilation ---------------------------------------------------------------

type Parse =
  fn(String) ->
    Result(#(List(ast.StmtWithLine), scope.ScopeBuilder), parser.ParseError)

type Compile =
  fn(List(ast.StmtWithLine), scope.ScopeBuilder) ->
    Result(FuncTemplate, compiler.CompileError)

/// Parse and compile `source`, or hand back a SyntaxError allocated in the
/// current realm. Big sources go through the heap-sized scratch process
/// (`compile_task`): only the compact template or the message crosses back.
fn compile_source(
  agent: Agent,
  source: String,
  parse: Parse,
  compile: Compile,
) -> Result(FuncTemplate, #(JsVal, Agent)) {
  let compiled =
    compile_task.run(string.byte_size(source), fn() {
      case parse(source) {
        Error(err) -> Error(parser.parse_error_to_string(err))
        Ok(#(body, sb)) ->
          compile(body, sb)
          |> result.map_error(compiler.error_message)
      }
    })
  result.map_error(compiled, fn(msg) {
    agent.store.ops.new_error(agent, SyntaxErr, msg)
  })
}

// -- Activation set-up -----------------------------------------------------------

/// Locals for a global-scope body: all `undefined` except the lexical `this`
/// slot, which holds the global object (§9.1.1.4.11 GetThisBinding).
fn top_level_locals(template: FuncTemplate, this: JsVal) -> TupleArray(JsVal) {
  let locals = tuple_array.repeat(mk_undefined(), template.local_count)
  case lexical.lexical_slot(template.lexical, lexical.RefThis) {
    Some(idx) -> tuple_array.set_unchecked(idx, this, locals)
    None -> locals
  }
}

/// A root activation of `template` over `locals`: empty stacks, pc 0.
fn activation(
  agent: Agent,
  template: FuncTemplate,
  locals: TupleArray(JsVal),
  this: JsVal,
  eval_env: Option(Handle),
) -> State {
  State(
    agent:,
    pc: 0,
    stack: [],
    locals:,
    code: template.bytecode,
    constants: template.constants,
    func: template,
    call_stack: [],
    try_stack: [],
    this:,
    new_target: mk_undefined(),
    home_object: mk_undefined(),
    call_args: [],
    eval_env:,
  )
}

/// Run `template` as global code of the current realm: `this` is the global
/// object, no eval env.
fn run_global_code(
  agent: Agent,
  template: FuncTemplate,
  run: Run,
) -> #(Result(JsVal, JsVal), Agent) {
  let global = mk_object(agent.realm.global_object)
  run(activation(
    agent,
    template,
    top_level_locals(template, global),
    global,
    None,
  ))
}

/// Run a nested activation entered straight from an opcode arm (direct
/// eval), holding one unit of `call_depth` across it: the calling frame's
/// registers are not among the collector's roots while the nested loop runs,
/// and a positive depth is what keeps the turn-boundary safepoint shut. The
/// same unit enforces `limits.max_call_depth` (RangeError, as a throw of the
/// eval). Entries through `JsOps` already sit inside `t_call`'s bracket.
///
/// On the way out the frames and depth are put back to what they were on
/// entry rather than decremented: a throw that unwinds out of the nested
/// root frame leaves behind whatever the abandoned frames had pushed.
fn run_bracketed(
  agent: Agent,
  make: fn(Agent) -> State,
  run: Run,
) -> #(Result(JsVal, JsVal), Agent) {
  let store = agent.store
  let depth = store.call_depth
  case depth >= limits.max_call_depth {
    True -> {
      let #(err, agent) =
        store.ops.new_error(agent, RangeErr, "Maximum call stack size exceeded")
      #(Error(err), agent)
    }
    False -> {
      let frames = agent.frames
      let entered =
        Agent(..agent, store: JsStore(..store, call_depth: depth + 1))
      let #(res, after) = run(make(entered))
      #(
        res,
        Agent(
          ..after,
          frames:,
          store: JsStore(..after.store, call_depth: depth),
        ),
      )
    }
  }
}

// -- JsOps.eval_hook -------------------------------------------------------------

/// The interpreter's `JsOps.eval_hook`: evaluate `source` in the current
/// realm as `kind` says and return the completion value, raising a throw.
///
/// `IndirectEval` and `DynamicFunction` are eval code (`compile_eval`:
/// configurable global declarations, §19.2.1.3); a dynamic function's
/// closure additionally gets §20.2.1.1.1 step 29 SetFunctionName applied to
/// its template so stack frames and `toString` say "anonymous". `ScriptEval`
/// is script code (`compile`: non-configurable declarations) followed by a
/// microtask checkpoint, as test262's `$262.evalScript` requires.
pub fn eval_hook(
  agent: Agent,
  source: String,
  kind: EvalKind,
  run: Run,
) -> #(JsVal, Agent) {
  let compile = case kind {
    IndirectEval | DynamicFunction -> compiler.compile_eval
    ScriptEval -> compiler.compile
  }
  let outcome = {
    use template <- result.try(compile_source(
      agent,
      source,
      parser.parse_script,
      compile,
    ))
    let #(res, agent) = run_global_code(agent, template, run)
    let agent = case kind {
      ScriptEval -> rt_async.drain(agent)
      IndirectEval | DynamicFunction -> agent
    }
    case res {
      Ok(v) -> Ok(#(v, agent))
      Error(thrown) -> Error(#(thrown, agent))
    }
  }
  case outcome, kind {
    Ok(#(f, agent)), DynamicFunction -> #(f, name_anonymous(agent, f))
    Ok(#(v, agent)), _ -> #(v, agent)
    Error(#(thrown, agent)), _ -> rt_store.t_throw(agent, thrown)
  }
}

/// §20.2.1.1.1 step 29 on the code template: the own `name` property is the
/// constructor's business (`rt/builtins/function`), the template name is
/// only reachable here.
fn name_anonymous(agent: Agent, f: JsVal) -> Agent {
  case classify(f) {
    KHandle(h) ->
      rt_store.t_cell_update(agent, h, fn(slot) {
        case slot {
          SObject(kind: KBytecode(template:, ..) as kind, ..) ->
            SObject(
              ..slot,
              kind: KBytecode(
                ..kind,
                template: bytecode.FuncTemplate(
                  ..template,
                  name: Some("anonymous"),
                ),
              ),
            )
          other -> other
        }
      })
    _ -> agent
  }
}

// -- Direct eval -----------------------------------------------------------------

/// §19.2.1.1 PerformEval for a DIRECT eval: the CallEval opcode found the
/// callee to be %eval%. `caller` is the calling activation with the call's
/// operands already popped; `args` are the call arguments and the three name
/// lists come straight off the opcode (see `opcode.CallEval`). Returns the
/// eval's outcome and the caller as it must continue: agent adopted, and
/// `eval_env` set if this eval had to allocate the frame's var scope.
///
/// Step 2: a non-string argument is returned as is. A caller compiled
/// without a name table (top-level code the compiler did not mark) gets
/// indirect semantics, as before.
pub fn direct_eval(
  caller: State,
  args: List(JsVal),
  param_scope_names: List(String),
  with_names: List(String),
  private_names: List(String),
  run: Run,
) -> #(Result(JsVal, JsVal), State) {
  let source = case args {
    [x, ..] -> x
    [] -> mk_undefined()
  }
  case classify(source), caller.func.local_names {
    KStr(source), Some(names) ->
      run_direct_eval(
        caller,
        source,
        names,
        param_scope_names,
        with_names,
        private_names,
        run,
      )
    KStr(source), None -> {
      let outcome = {
        use template <- result.try(compile_source(
          caller.agent,
          source,
          parser.parse_script,
          compiler.compile_eval,
        ))
        let global = mk_object(caller.agent.realm.global_object)
        let make = fn(agent) {
          activation(
            agent,
            template,
            top_level_locals(template, global),
            global,
            None,
          )
        }
        let #(res, agent) = run_bracketed(caller.agent, make, run)
        Ok(#(res, agent))
      }
      adopt(caller, outcome, caller.eval_env)
    }
    _, _ -> #(Ok(source), caller)
  }
}

fn run_direct_eval(
  caller: State,
  source: String,
  names: bytecode.EvalNameTable,
  param_scope_names: List(String),
  with_names: List(String),
  private_names: List(String),
  run: Run,
) -> #(Result(JsVal, JsVal), State) {
  let EvalNameTable(var_env:, names: name_table) = names
  let func = caller.func
  let code_kind = func.code_kind
  // Steps 6-11: the eval body may use exactly the syntax the caller's body
  // may (new.target / super / arguments), sees the caller's private names,
  // and is strict if the caller is.
  let eval_caller =
    compiler.DirectEvalCaller(
      names: list.map(name_table, fn(pair) { pair.0 }),
      slots: func.lexical,
      code_kind:,
      strictness: case func.is_strict {
        True -> compiler.Strict
        False -> compiler.Sloppy
      },
      var_env:,
      param_scope_names:,
      with_names:,
      private_names:,
    )
  let outcome = {
    use template <- result.try(
      compile_source(
        caller.agent,
        source,
        parser.parse_direct_eval(
          _,
          allow_new_target: lexical.new_target_allowed(code_kind),
          allow_super_property: lexical.super_prop_allowed(code_kind),
          allow_super_call: lexical.super_call_allowed(code_kind),
          allow_arguments: lexical.arguments_allowed(code_kind),
          outer_private_names: private_names,
        ),
        fn(body, sb) { compiler.compile_eval_direct(body, sb, eval_caller) },
      ),
    )
    // Locals 0..N-1 alias the caller's boxed locals in name-table order,
    // then the caller's lexical box cells in `all_lexical_refs` order: the
    // order `compile_eval_direct` allocated capture slots in. The rest start
    // undefined.
    use box_refs <- result.try(caller_box_refs(caller, name_table))
    let padding = template.local_count - list.length(box_refs)
    let locals =
      list.append(box_refs, list.repeat(mk_undefined(), padding))
      |> tuple_array.from_list
    // §19.2.1.3: sloppy eval code declares its vars in the caller's
    // VariableEnvironment. Function caller: the frame's eval env object,
    // shared by every eval in the frame. Global caller: the global object
    // itself (the body was compiled to fall through to it). Strict on
    // either side: the eval body owns its vars as locals.
    let #(eval_env, agent) = case func.is_strict, var_env, caller.eval_env {
      True, _, _ | _, GlobalVarEnv, _ -> #(None, caller.agent)
      False, FrameVarEnv, Some(h) -> #(Some(h), caller.agent)
      False, FrameVarEnv, None -> {
        let #(h, agent) = rt_env.t_new_eval_env(caller.agent)
        #(Some(h), agent)
      }
    }
    // Step 16.a: direct eval keeps the caller's `this` (delivered through
    // the captured lexical slot; the register copy is for frame readers).
    let #(res, agent) =
      run_bracketed(
        agent,
        activation(_, template, locals, caller.this, eval_env),
        run,
      )
    Ok(#(res, agent, eval_env))
  }
  case outcome {
    Ok(#(res, agent, eval_env)) ->
      adopt(caller, Ok(#(res, agent)), option.or(eval_env, caller.eval_env))
    Error(err) -> adopt(caller, Error(err), caller.eval_env)
  }
}

/// Fold an eval outcome back into the caller: adopt the agent either way,
/// keep `eval_env`, and surface a pre-run failure (SyntaxError, missing
/// slot) as the eval's thrown value.
fn adopt(
  caller: State,
  outcome: Result(#(Result(JsVal, JsVal), Agent), #(JsVal, Agent)),
  eval_env: Option(Handle),
) -> #(Result(JsVal, JsVal), State) {
  case outcome {
    Ok(#(res, agent)) -> #(res, State(..caller, agent:, eval_env:))
    Error(#(thrown, agent)) -> #(
      Error(thrown),
      State(..caller, agent:, eval_env:),
    )
  }
}

/// The caller's box cells for a direct eval, in capture-slot order. A slot
/// the caller's locals do not have is a name-table/locals desync, an engine
/// bug: it throws instead of seeding `undefined` into a boxed capture where
/// it would silently read back as an undefined variable.
fn caller_box_refs(
  caller: State,
  name_table: List(#(String, Int)),
) -> Result(List(JsVal), #(JsVal, Agent)) {
  let read = fn(idx) {
    tuple_array.get(idx, caller.locals) |> option.to_result(idx)
  }
  let refs = {
    use named <- result.try(list.try_map(name_table, fn(pair) { read(pair.1) }))
    use lex <- result.map(
      lexical.all_lexical_refs
      |> list.filter_map(fn(ref) {
        lexical.lexical_slot(caller.func.lexical, ref) |> option.to_result(Nil)
      })
      |> list.try_map(read),
    )
    list.append(named, lex)
  }
  result.map_error(refs, fn(idx) {
    caller.agent.store.ops.new_error(
      caller.agent,
      TypeErr,
      "direct eval: local slot " <> int.to_string(idx) <> " missing",
    )
  })
}
