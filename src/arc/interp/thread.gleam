// threaded code: a function's ops as closures that call one another, built
// the first time it runs

import arc/interp/state.{type Outcome, type State, type VmError}
import arc/rt/bytecode.{type FuncTemplate, FuncTemplate}
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type ObjKind, BytecodeFn, SObject,
  StoreMeta,
}
import gleam/dict
import gleam/result

/// runs from state.pc until the outermost frame finishes or parks
@external(erlang, "arc_interp_thread_ffi", "enter")
pub fn enter(state: State) -> Result(#(Outcome, State), VmError)

@external(erlang, "arc_interp_thread_ffi", "build")
fn build(agent: Agent, template: FuncTemplate) -> #(FuncTemplate, Agent)

/// calls to a threaded plain function of the running realm; `general` takes
/// the ones it can't
@external(erlang, "arc_interp_thread_calls_ffi", "prepare")
pub fn prepare(
  fn_h: Handle,
  kind: ObjKind,
  this: JsVal,
  general: fn(Agent, List(JsVal)) -> #(JsVal, Agent),
) -> fn(Agent, List(JsVal)) -> #(JsVal, Agent)

/// the template of the function at `fn_h`, ready to run. The function keeps
/// it, and so do closures of the same code made later
pub fn for_call(
  agent: Agent,
  fn_h: Handle,
  template: FuncTemplate,
) -> #(FuncTemplate, Agent) {
  case bytecode.is_threaded(template) {
    True -> #(template, agent)
    False -> first_call(agent, fn_h, template)
  }
}

// called by name from threaded code
pub fn first_call(
  agent: Agent,
  fn_h: Handle,
  template: FuncTemplate,
) -> #(FuncTemplate, Agent) {
  let #(threaded, agent) = threaded(agent, template)
  let agent = {
    use cell <- rt_store.cell_update(agent, fn_h)
    case cell {
      SObject(kind: BytecodeFn(..) as kind, ..) ->
        SObject(..cell, kind: BytecodeFn(..kind, template: threaded))
      _ -> cell
    }
  }
  #(threaded, agent)
}

/// the template ready to run, built now unless it was before
pub fn threaded(
  agent: Agent,
  template: FuncTemplate,
) -> #(FuncTemplate, Agent) {
  let meta = agent.store.meta
  case
    dict.get(meta.threaded, template.id),
    dict.get(meta.threaded_before, template.id)
  {
    Ok(threaded), _ -> #(named_as(threaded, template), agent)
    Error(Nil), Ok(threaded) -> #(
      named_as(threaded, template),
      remember(agent, threaded),
    )
    Error(Nil), Error(Nil) -> {
      let #(template, agent) = with_id(agent, template)
      let #(threaded, agent) = build(agent, template)
      #(threaded, remember(agent, threaded))
    }
  }
}

/// the template as it has been threaded lately, or else as it came
pub fn known(agent: Agent, template: FuncTemplate) -> FuncTemplate {
  dict.get(agent.store.meta.threaded, template.id)
  |> result.map(named_as(_, template))
  |> result.unwrap(template)
}

// a name given at run time is on a closure's own copy
fn named_as(threaded: FuncTemplate, template: FuncTemplate) -> FuncTemplate {
  case threaded.name == template.name {
    True -> threaded
    False -> FuncTemplate(..threaded, name: template.name)
  }
}

fn remember(agent: Agent, threaded: FuncTemplate) -> Agent {
  let meta = agent.store.meta
  let known = dict.insert(meta.threaded, threaded.id, threaded)
  rt_store.with_meta(agent, StoreMeta(..meta, threaded: known))
}

/// the template under an id its threaded form can be remembered by
pub fn with_id(agent: Agent, template: FuncTemplate) -> #(FuncTemplate, Agent) {
  case template.id {
    0 -> {
      let meta = agent.store.meta
      let id = meta.next_template_id
      #(
        FuncTemplate(..template, id:),
        rt_store.with_meta(agent, StoreMeta(..meta, next_template_id: id + 1)),
      )
    }
    _ -> #(template, agent)
  }
}
