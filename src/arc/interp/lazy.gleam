import arc/bytecode/lazy.{type LazyFunction}
import arc/bytecode/lexical
import arc/compiler
import arc/internal/tuple_array.{type TupleArray}
import arc/interp/state.{type State, type StepExit, State}
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type JsVal, Agent, BytecodeFn, KHandle, SObject, Store, StoreMeta,
  classify,
}
import gleam/dict
import gleam/option.{None, Some}
import gleam/result
import gleam/string

@external(erlang, "arc_interp_locals_ffi", "widen")
fn widen(locals: TupleArray(JsVal), count: Int) -> TupleArray(JsVal)

/// the first call of a function left uncompiled: compiles it, or takes what
/// an earlier closure of the same function compiled, and starts this frame
/// again on the real code. The stub and the real template lay the frame out
/// alike, bar how many locals follow the parameters
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
        case compiler.compile_function_alone(function, lazy: True) {
          Ok(template) -> Ok(#(template, remember(agent, key, template)))
          Error(error) -> state.throw_syntax_error(state, string.inspect(error))
        }
    },
  )
  State(
    ..state,
    agent: adopt(agent, state, template),
    func: template,
    locals: widen(state.locals, template.local_count),
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

// the running function object takes the real template, so its next call
// starts there
fn adopt(agent: Agent, state: State, template: FuncTemplate) -> Agent {
  let callee = {
    use slot <- option.map(lexical.slot_of(
      state.func.lexical,
      lexical.ActiveFuncRef,
    ))
    classify(tuple_array.get_unchecked(slot, state.locals))
  }
  case callee {
    Some(KHandle(handle)) -> {
      use cell <- rt_store.cell_update(agent, handle)
      case cell {
        SObject(kind: BytecodeFn(..) as kind, ..) ->
          SObject(..cell, kind: BytecodeFn(..kind, template:))
        _ -> cell
      }
    }
    Some(_) | None -> agent
  }
}
