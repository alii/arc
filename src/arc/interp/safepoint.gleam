//// The GC safepoints the interpreter owns. The shared collector
//// (`rt/gc.t_collect`) refuses to run while `store.call_depth > 0` and sees
//// only the store's own roots, so a collection with an interpreter frame
//// live must be triggered BY the interpreter, handing that frame's registers
//// in as extra roots. Two such points exist:
////
////   * `Return` back into the root activation (`maybe_collect_at_toplevel`),
////     so a long allocation-heavy script sheds garbage while it runs;
////   * the engine's turn end (`finish_turn`), after the top-level body has
////     returned and before / while the ONE microtask drain runs.
////
//// The third safepoint, between microtask jobs, is inside `rt/async.drain`
//// itself: no interpreter frame is live there.

import arc/interp/state.{type State, State} as interp_state
import arc/rt/async as rt_async
import arc/rt/gc as rt_gc
import arc/rt/types.{type Agent, type JsVal}

/// Root-activation `Return` safepoint, run by the `Return` arm once it has
/// popped back into the outermost frame (port of the old interpreter's
/// `maybe_collect_at_toplevel`). Collects when the store has grown past its
/// threshold AND `call_depth == 0`, i.e. this activation is the one the
/// engine entered: a nested `run_bytecode` under a native (an array callback,
/// a generator body driven by `.next()`) runs above the caller's
/// `t_enter_call`, whose registers this `State` cannot see, so it never
/// collects. Roots = the store's own + this frame's registers.
pub fn maybe_collect_at_toplevel(state: State) -> State {
  case state.call_stack {
    [] -> {
      let js = state.agent.store
      case js.call_depth == 0 && js.alloc_since_gc >= js.gc_threshold {
        True ->
          State(
            ..state,
            agent: rt_gc.t_collect(state.agent, interp_state.frame_roots(state)),
          )
        False -> state
      }
    }
    [_, ..] -> state
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
