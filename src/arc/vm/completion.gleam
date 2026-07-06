import arc/vm/value.{type JsValue}

/// JS-level completion -- either normal return or uncaught exception.
///
/// A completion carries no heap: it is always produced alongside the State it
/// was computed in (`#(Completion, State(host))`), and that State's `heap` is
/// the single source of truth. (Older versions duplicated the heap into every
/// variant, which forced each consumer to choose between two heaps.)
///
/// Coroutine suspensions (yield/await) are NOT completions -- see `Outcome`.
/// Keeping them out of this type means a consumer that only ever receives a
/// finished frame (top-level scripts, eval, promise jobs, re-entrant native
/// calls) has no impossible Yield/Await arms to hand-write.
pub type Completion {
  NormalCompletion(value: JsValue)
  ThrowCompletion(value: JsValue)
}

/// Map a `Completion` to `Result` — `NormalCompletion(v)` → `Ok(v)`,
/// `ThrowCompletion(v)` → `Error(v)`. The one place this mapping lives; a new
/// `Completion` variant is a single compile error here, not one per caller.
pub fn to_result(c: Completion) -> Result(JsValue, JsValue) {
  case c {
    NormalCompletion(v) -> Ok(v)
    ThrowCompletion(v) -> Error(v)
  }
}

/// Which coroutine primitive suspended the frame.
pub type SuspendKind {
  /// Generator yielded a value (or hit InitialYield) -- execution is
  /// suspended, not completed.
  Yield
  /// Async function/generator hit `await` -- suspended waiting on a promise.
  /// Distinct from `Yield` so async generators can tell yield (settle head
  /// request) from await (suspend without settling).
  Await
}

/// What driving the step loop over one frame produced: either the frame ran
/// to a terminal `Completion`, or a coroutine suspended mid-frame (the State
/// returned alongside is the resumable suspension point). Only coroutine
/// drivers (generator / async-function / async-generator resume machinery)
/// may see `Suspended`; a suspension escaping any other frame is an engine
/// bug and belongs on the `VmError` channel as `state.SuspensionLeak`, never
/// in a `Completion`.
pub type Outcome {
  Completed(completion: Completion)
  Suspended(kind: SuspendKind, value: JsValue)
}
