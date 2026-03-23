import arc/vm/state.{type Heap}
import arc/vm/value.{type JsValue}

/// JS-level completion -- either normal return or uncaught exception.
pub type Completion {
  NormalCompletion(value: JsValue, heap: Heap)
  ThrowCompletion(value: JsValue, heap: Heap)
  /// Generator yielded a value -- execution is suspended, not completed.
  /// The full State is available in the second element of the returned tuple.
  YieldCompletion(value: JsValue, heap: Heap)
  /// Async function/generator hit `await` -- suspended waiting on a promise.
  /// Distinct from YieldCompletion so async generators can tell yield
  /// (settle head request) from await (suspend without settling).
  AwaitCompletion(value: JsValue, heap: Heap)
}
