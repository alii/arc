//// The GC safepoints the interpreter owns. The shared collector
//// (`rt/gc.t_collect`) refuses to run while `call_depth > 0` and sees
//// only the store's own roots, so a collection with an interpreter frame
//// live must be triggered BY the interpreter, handing that frame's registers
//// in as extra roots. Two such points exist:
////
////   * `Return` into a flat frame of the outermost activation
////     (`maybe_collect_at_return`), so a long allocation-heavy script sheds
////     garbage while it runs;
////   * the engine's turn end (`finish_turn`), after the top-level body has
////     returned and before / while the ONE microtask drain runs.
////
//// The third safepoint, between microtask jobs, is inside `rt/async.drain`
//// itself: no interpreter frame is live there.

import arc/interp/state.{type State, State} as interp_state
import arc/rt/async as rt_async
import arc/rt/gc as rt_gc
import arc/rt/types.{type Agent, type JsVal}

/// `Return` safepoint, run by the `Return` arm once it has popped back into
/// the caller. Collects when the store has grown past its threshold AND
/// this activation sits at the bottom of the depth bracket (`outer_depth ==
/// 0`, i.e. every unit of `call_depth` is one of its own flat frames): a
/// nested `run_bytecode` under a native (an array callback, a generator
/// body driven by `.next()`, direct eval) holds a unit of its own above the
/// caller's `t_enter_call`, whose registers this `State` cannot see, so it
/// never collects. Roots = the store's own + every frame of this activation
/// (`frame_roots`).
pub fn maybe_collect_at_return(state: State) -> State {
  let js = state.agent.store
  case js.alloc_since_gc >= js.gc_threshold && state.outer_depth == 0 {
    True ->
      State(
        ..state,
        agent: rt_gc.t_collect(state.agent, interp_state.frame_roots(state)),
      )
    False -> state
  }
}

/// Turn-end epilogue shared by every engine entry point (`eval`,
/// `eval_module`, `call`, `with_state`): the top-level activation is gone and
/// has handed `held` (its completion value, plus anything else the caller
/// keeps in Gleam) back, so no frame roots remain. Collect if the store grew
/// past its threshold, then run `drive` — `rt/async.drain`, or an embedder
/// macrotask loop that drains as part of its own cycle. `held` stays rooted
/// for the whole stretch (the drain collects between jobs) and is released
/// on the way out.
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

/// `finish_turn` with the default driver: the one microtask drain.
pub fn end_turn(agent: Agent, held: List(JsVal)) -> Agent {
  finish_turn(agent, held, rt_async.drain)
}
