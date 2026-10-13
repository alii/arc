import arc/bytecode/lazy
import arc/bytecode/lexical
import arc/compiler
import arc/internal/tuple_array.{type TupleArray}
import arc/interp/kernel
import arc/interp/state.{type State, type StepExit, State}
import arc/rt/bytecode.{type EnvTuple, type FuncTemplate, FuncTemplate}
import arc/rt/closure as rt_closure
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, Agent, BytecodeFn, KHandle, SObject,
  Store, StoreMeta, classify,
}
import gleam/dict
import gleam/option.{None, Some}
import gleam/result
import gleam/string

// the first `count` locals, which are what the closure captured
@external(erlang, "arc_interp_locals_ffi", "captured")
fn captured(locals: TupleArray(JsVal), count: Int) -> EnvTuple

// the locals at `slots`, then the record
@external(erlang, "arc_interp_locals_ffi", "captured_with")
fn captured_with(
  locals: TupleArray(JsVal),
  slots: List(Int),
  record: BitArray,
) -> EnvTuple

// a stub's frame has its function's record where its last capture goes
@external(erlang, "erlang", "element")
fn record_at(position: Int, locals: TupleArray(JsVal)) -> BitArray

/// a closure of a function left uncompiled: the stub its kind shares, with
/// the function's record as its last capture. Once some closure of the same
/// function has been called, later ones are ordinary closures of the real code
pub fn make_closure(
  agent: Agent,
  parent: FuncTemplate,
  unit_id: Int,
  locals: TupleArray(JsVal),
  stub_index: Int,
  record: BitArray,
) -> #(Handle, Agent) {
  let assert Ok(#(start, slots)) = lazy.creation(record)
    as "interp: a lazy function's record does not read back"
  case dict.get(agent.store.meta.lazy_code, #(unit_id, start)) {
    Ok(template) ->
      rt_closure.new_bytecode_function(
        agent,
        template,
        kernel.capture_env(slots, locals),
        unit_id,
      )
    Error(Nil) ->
      rt_closure.new_bytecode_function(
        agent,
        tuple_array.element(stub_index + 1, parent.functions),
        captured_with(locals, slots, record),
        unit_id,
      )
  }
}

/// the first call of such a closure: compiles its function out of the unit's
/// text, or takes what another closure of it compiled, and lays the frame out
/// again for the real code. A stub's frame is its captures, the record last,
/// then this, the function object, the home object and new.target
pub fn first_call(
  state: State,
  bytes: BitArray,
  is_module: Bool,
) -> Result(State, StepExit) {
  let assert lexical.OwnedLexicalSlots(base: env_size) = state.func.lexical
    as "interp: a stub's frame is not laid out as one"
  let agent = state.agent
  use function <- result.try(
    case lazy.unpack(record_at(env_size, state.locals)) {
      Ok(function) -> Ok(function)
      Error(Nil) ->
        state.throw_syntax_error(
          state,
          "a function's record does not read back",
        )
    },
  )
  let key = #(state.unit_id, function.source.start)
  use #(template, agent) <- result.map(
    case dict.get(agent.store.meta.lazy_code, key) {
      Ok(template) -> Ok(#(template, agent))
      Error(Nil) ->
        case
          compiler.compile_function_alone(
            bytes,
            is_module:,
            function:,
            lazy: True,
          )
        {
          Ok(template) -> Ok(#(template, remember(agent, key, template)))
          Error(error) -> state.throw_syntax_error(state, string.inspect(error))
        }
    },
  )
  // a name given at run time is on this closure's own copy of the stub
  let template = case template.name, state.func.name {
    None, Some(_) as name -> FuncTemplate(..template, name:)
    _, _ -> template
  }
  let callee = tuple_array.get_unchecked(env_size + 1, state.locals)
  let env = captured(state.locals, env_size - 1)
  let locals =
    kernel.frame_locals(
      env,
      template.lexical,
      state.this,
      callee,
      state.home_object,
      state.new_target,
      state.call_args,
      template.arity,
      template.local_count,
    )
  State(
    ..state,
    agent: adopt(agent, callee, template, env),
    func: template,
    locals:,
    pc: 0,
  )
}

fn remember(agent: Agent, key: #(Int, Int), template: FuncTemplate) -> Agent {
  let store = agent.store
  let lazy_code = dict.insert(store.meta.lazy_code, key, template)
  Agent(
    ..agent,
    store: Store(..store, meta: StoreMeta(..store.meta, lazy_code:)),
  )
}

// the function object becomes an ordinary closure of the real code
fn adopt(
  agent: Agent,
  callee: JsVal,
  template: FuncTemplate,
  env: EnvTuple,
) -> Agent {
  case classify(callee) {
    KHandle(handle) -> {
      use cell <- rt_store.cell_update(agent, handle)
      case cell {
        SObject(kind: BytecodeFn(..) as kind, ..) ->
          SObject(..cell, kind: BytecodeFn(..kind, template:, env:))
        _ -> cell
      }
    }
    _ -> agent
  }
}
