// §19.2.1.1 performeval, direct eval aliases caller box cells

import arc/bytecode/key.{type Key, type SourceKey}
import arc/bytecode/lexical
import arc/compiler
import arc/compiler/compile_task
import arc/compiler/scope
import arc/internal/tuple_array.{type TupleArray}
import arc/interp/load
import arc/interp/state.{type State, State}
import arc/parser
import arc/parser/ast
import arc/rt/async as rt_async
import arc/rt/bytecode.{
  type FuncTemplate, EvalNameTable, FrameVarEnv, GlobalVarEnv,
}
import arc/rt/env as rt_env
import arc/rt/limits
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type EvalKind, type Handle, type JsVal, Agent, DynamicFunction,
  IndirectEval, KBytecode, KHandle, KStr, RangeErr, SObject, ScriptEval,
  SyntaxErr, TypeErr, classify, mk_object, mk_undefined,
}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type Run =
  fn(State) -> #(Result(JsVal, JsVal), Agent)

type Parse =
  fn(String) ->
    Result(#(List(ast.StmtWithLine), scope.ScopeBuilder), parser.ParseError)

type Compile =
  fn(List(ast.StmtWithLine), scope.ScopeBuilder) ->
    Result(FuncTemplate(SourceKey), compiler.CompileError)

// compiles then loads into the agent's heap
fn compile_source(
  agent: Agent,
  source: String,
  parse: Parse,
  compile: Compile,
) -> Result(#(FuncTemplate(Key), Agent), #(JsVal, Agent)) {
  let compiled =
    compile_task.run(string.byte_size(source), fn() {
      case parse(source) {
        Error(err) -> Error(parser.parse_error_to_string(err))
        Ok(#(body, sb)) ->
          compile(body, sb)
          |> result.map_error(compiler.error_message)
      }
    })
  case compiled {
    Ok(template) -> Ok(load.template(agent, template))
    Error(msg) -> Error(agent.store.ops.new_error(agent, SyntaxErr, msg))
  }
}

fn top_level_locals(
  template: FuncTemplate(Key),
  this: JsVal,
) -> TupleArray(JsVal) {
  let locals = tuple_array.repeat(mk_undefined(), template.local_count)
  case lexical.lexical_slot(template.lexical, lexical.RefThis) {
    Some(idx) -> tuple_array.set_unchecked(idx, this, locals)
    None -> locals
  }
}

fn activation(
  agent: Agent,
  template: FuncTemplate(Key),
  locals: TupleArray(JsVal),
  this: JsVal,
  eval_env: Option(Handle),
) -> State {
  let #(unit, agent) = rt_store.t_next_unit_uid(agent)
  State(
    agent:,
    pc: 0,
    stack: [],
    locals:,
    func: template,
    unit:,
    call_stack: [],
    outer_depth: agent.call_depth,
    depth: agent.call_depth,
    try_stack: [],
    this:,
    new_target: mk_undefined(),
    home_object: mk_undefined(),
    call_args: [],
    eval_env:,
  )
}

fn run_global_code(
  agent: Agent,
  template: FuncTemplate(Key),
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

// holds one call_depth unit, caller frame is not a gc root
fn run_bracketed(
  agent: Agent,
  make: fn(Agent) -> State,
  run: Run,
) -> #(Result(JsVal, JsVal), Agent) {
  let depth = agent.call_depth
  case depth >= limits.max_call_depth {
    True -> {
      let #(err, agent) =
        agent.store.ops.new_error(
          agent,
          RangeErr,
          "Maximum call stack size exceeded",
        )
      #(Error(err), agent)
    }
    False -> {
      let frames = agent.frames
      let entered = Agent(..agent, call_depth: depth + 1)
      let #(res, after) = run(make(entered))
      #(res, Agent(..after, frames:, call_depth: depth))
    }
  }
}

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
    use #(template, agent) <- result.try(compile_source(
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
        use #(template, agent) <- result.try(compile_source(
          caller.agent,
          source,
          parser.parse_script,
          compiler.compile_eval,
        ))
        let global = mk_object(agent.realm.global_object)
        let make = fn(agent) {
          activation(
            agent,
            template,
            top_level_locals(template, global),
            global,
            None,
          )
        }
        let #(res, agent) = run_bracketed(agent, make, run)
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
    use #(template, agent) <- result.try(
      compile_source(
        caller.agent,
        source,
        parser.parse_direct_eval(
          _,
          strict: func.is_strict,
          allow_new_target: lexical.new_target_allowed(code_kind),
          allow_super_property: lexical.super_prop_allowed(code_kind),
          allow_super_call: lexical.super_call_allowed(code_kind),
          allow_arguments: lexical.arguments_allowed(code_kind),
          outer_private_names: private_names,
        ),
        fn(body, sb) { compiler.compile_eval_direct(body, sb, eval_caller) },
      ),
    )
    use box_refs <- result.try(caller_box_refs(caller, name_table))
    let padding = template.local_count - list.length(box_refs)
    let locals =
      list.append(box_refs, list.repeat(mk_undefined(), padding))
      |> tuple_array.from_list
    let #(eval_env, agent) = case func.is_strict, var_env, caller.eval_env {
      True, _, _ | _, GlobalVarEnv, _ -> #(None, agent)
      False, FrameVarEnv, Some(h) -> #(Some(h), agent)
      False, FrameVarEnv, None -> {
        let #(h, agent) = rt_env.t_new_eval_env(agent)
        #(Some(h), agent)
      }
    }
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
