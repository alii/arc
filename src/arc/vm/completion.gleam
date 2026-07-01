import arc/vm/value.{type JsValue}

/// JS-level completion -- either normal return or uncaught exception.
///
/// A completion carries no heap: it is always produced alongside the State it
/// was computed in (`#(Completion, State(host))`), and that State's `heap` is
/// the single source of truth. (Older versions duplicated the heap into every
/// variant, which forced each consumer to choose between two heaps.)
pub type Completion {
  NormalCompletion(value: JsValue)
  ThrowCompletion(value: JsValue)
  /// Generator yielded a value -- execution is suspended, not completed.
  /// The full State is available in the second element of the returned tuple.
  YieldCompletion(value: JsValue)
  /// Async function/generator hit `await` -- suspended waiting on a promise.
  /// Distinct from YieldCompletion so async generators can tell yield
  /// (settle head request) from await (suspend without settling).
  AwaitCompletion(value: JsValue)
}
