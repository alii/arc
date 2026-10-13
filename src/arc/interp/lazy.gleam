import arc/bytecode/lazy.{type LazyFunction}
import arc/compiler
import arc/internal/tuple_array.{type TupleArray}
import arc/interp/kernel
import arc/interp/state.{type State, type StepExit, State}
import arc/rt/bytecode.{type EnvTuple, type FuncTemplate}
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type JsVal, Agent, BytecodeFn, KHandle, SObject, Store, StoreMeta,
  classify,
}
import gleam/dict
import gleam/list
import gleam/result
import gleam/string

// the first `count` locals, which are what the closure captured
@external(erlang, "arc_interp_locals_ffi", "captured")
fn captured(locals: TupleArray(JsVal), count: Int) -> EnvTuple

/// the first call of a function left uncompiled: compiles it, or takes what
/// an earlier closure of the same function compiled, and lays this frame out
/// again for the real code. A stub's frame is its captures, then this, the
/// function object, the home object and new.target, then its parameters
pub fn first_call(
  state: State,
  function: LazyFunction,
) -> Result(State, StepExit) {
  let agent = state.agent
  let key = #(state.unit_id, function.source.start)
  use #(template, agent) <- result.map(
    case dict.get(agent.store.meta.lazy_code, key) {
      Ok(template) -> Ok(#(template, agent))
      Error(Nil) ->
        case
          compiler.compile_function_alone(
            function,
            parent_slots: state.func.env_descriptors,
            lazy: True,
          )
        {
          Ok(template) -> Ok(#(template, remember(agent, key, template)))
          Error(error) -> state.throw_syntax_error(state, string.inspect(error))
        }
    },
  )
  let captures = list.length(state.func.env_descriptors)
  let callee = tuple_array.get_unchecked(captures + 1, state.locals)
  let locals =
    kernel.frame_locals(
      captured(state.locals, captures),
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
    agent: adopt(agent, callee, template),
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

// the function object takes the real template, so its next call starts there
fn adopt(agent: Agent, callee: JsVal, template: FuncTemplate) -> Agent {
  case classify(callee) {
    KHandle(handle) -> {
      use cell <- rt_store.cell_update(agent, handle)
      case cell {
        SObject(kind: BytecodeFn(..) as kind, ..) ->
          SObject(..cell, kind: BytecodeFn(..kind, template:))
        _ -> cell
      }
    }
    _ -> agent
  }
}
