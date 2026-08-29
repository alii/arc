import arc/interp/state.{type State, State} as interp_state
import arc/rt/async as rt_async
import arc/rt/gc as rt_gc
import arc/rt/types.{type Agent, type JsVal}

// only the outermost activation can see every frame root
pub fn maybe_collect_at_return(state: State) -> State {
  let js = state.agent.store
  case
    state.outer_depth == 0
    && js.alloc_since_gc >= js.gc_threshold
    && rt_gc.due(js)
  {
    True ->
      State(
        ..state,
        agent: rt_gc.t_collect_frames(
          state.agent,
          interp_state.frame_roots(state),
          interp_state.frame_terms(state),
          False,
        ),
      )
    False -> state
  }
}

// held stays rooted while drive collects between jobs
pub fn finish_turn(
  agent: Agent,
  held: List(JsVal),
  drive: fn(Agent) -> Agent,
) -> Agent {
  let #(agent, ids) = rt_gc.t_hold_roots(agent, held)
  let agent = rt_gc.t_maybe_collect(agent)
  let agent = drive(agent)
  rt_gc.t_release_roots(agent, ids)
}

pub fn end_turn(agent: Agent, held: List(JsVal)) -> Agent {
  finish_turn(agent, held, rt_async.drain)
}
