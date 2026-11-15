import arc/interp/state.{type State, State}
import arc/rt/async as rt_async
import arc/rt/gc as rt_gc
import arc/rt/types.{type Agent, type JsVal}

// only the outermost activation can see every frame root
pub fn maybe_collect_at_return(state: State) -> State {
  case state.frame.outer_depth == 0 && rt_gc.is_due(state.agent) {
    True ->
      State(
        ..state,
        agent: rt_gc.collect_some(state.agent, state.frame_roots(state)),
      )
    False -> state
  }
}

// held stays rooted while drain collects between jobs
pub fn finish_turn(
  agent: Agent,
  held: List(JsVal),
  drain: rt_async.Drain,
) -> Agent {
  let #(ids, agent) = rt_gc.hold_roots(agent, held)
  let agent = rt_gc.maybe_collect(agent)
  let agent = drain(agent)
  rt_gc.release_roots(agent, ids)
}
