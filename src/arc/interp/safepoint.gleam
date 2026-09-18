import arc/interp/state.{type State, State}
import arc/rt/async as rt_async
import arc/rt/gc as rt_gc
import arc/rt/types.{type Agent, type JsVal}

// only the outermost activation can see every frame root
pub fn maybe_collect_at_return(state: State) -> State {
  let store = state.agent.store
  case state.outer_depth == 0 && store.alloc_since_gc >= store.gc_threshold {
    True ->
      State(
        ..state,
        agent: rt_gc.t_collect_some(state.agent, state.frame_roots(state)),
      )
    False -> state
  }
}

// held stays rooted while drain collects between jobs
pub fn finish_turn(
  agent: Agent,
  held: List(JsVal),
  drain: fn(Agent) -> Agent,
) -> Agent {
  let #(agent, ids) = rt_gc.t_hold_roots(agent, held)
  let agent = rt_gc.t_maybe_collect(agent)
  let agent = drain(agent)
  rt_gc.t_release_roots(agent, ids)
}

pub fn end_turn(agent: Agent, held: List(JsVal)) -> Agent {
  finish_turn(agent, held, rt_async.drain)
}
