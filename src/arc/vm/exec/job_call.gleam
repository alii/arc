// ============================================================================
// Calling into JS from a job.
//
// A job (promise reaction, thenable job, host job) has no caller and no
// continuation: nothing to hand a return value to, nothing to propagate an
// abrupt completion to. `call_settlement_fn` is the single place that encodes
// what "call a JS function from a job" means for the whole VM, so the loop
// driver (`arc/vm/exec/event_loop`), the promise builtins
// (`arc/vm/exec/promises`) and dynamic import all behave identically.
//
// This module deliberately owns nothing else — it is a leaf utility, not part
// of the loop driver, so importing it never drags in queue draining.
// ============================================================================

import arc/vm/ops/object
import arc/vm/state.{type State}
import arc/vm/value.{type JsValue, JsUndefined}
import gleam/io

/// Call a function during job execution, fire-and-forget: the return value is
/// discarded (a job has no continuation to hand it to) and an abrupt
/// completion is reported on stderr rather than propagated (a job has no
/// caller to propagate to). Used for the resolve/reject of a child promise
/// after a reaction runs, and by any host that must call into JS from a job.
pub fn call_settlement_fn(
  state: State(host),
  target: JsValue,
  args: List(JsValue),
) -> State(host) {
  case state.call(state, target, JsUndefined, args) {
    Ok(#(_, new_state)) -> new_state
    // `target` is typically a promise-capability resolve/reject function. The
    // native ones never throw, but `Promise.prototype.then` builds the child
    // capability with NewPromiseCapability(SpeciesConstructor(this)), so a
    // user species constructor hands us arbitrary user callables here. A
    // job has no caller to propagate an abrupt completion to, so without
    // this report a throwing user `resolve`/`reject` vanishes silently.
    // There is no promise ref to blame it on (the throw happened AFTER the
    // reaction settled, outside any promise), so `unhandled_rejections` —
    // a list of promise data refs — can't carry it; report it on the same
    // stderr channel `report_unhandled_rejections` uses.
    Error(#(thrown, new_state)) -> {
      io.println_error(
        "Uncaught (in promise job) "
        <> object.format_error(thrown, new_state.heap),
      )
      new_state
    }
  }
}
