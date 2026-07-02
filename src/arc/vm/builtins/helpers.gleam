/// Shared runtime helpers for builtins.
import arc/vm/heap.{type Heap}
import arc/vm/value.{type JsValue, JsObject, JsUndefined, ObjectSlot}
import gleam/option.{type Option, None, Some}

/// ES2024 §7.2.3 IsCallable(argument)
///
/// 1. If argument is not an Object, return false.
/// 2. If argument has a [[Call]] internal method, return true.
/// 3. Return false.
///
/// We check for FunctionObject or NativeFunction object kinds instead of a
/// [[Call]] internal method slot, since our object representation uses tagged
/// kinds rather than method tables.
pub fn is_callable(h: Heap(ctx, host), val: JsValue) -> Bool {
  // Step 1: If argument is not an Object, return false.
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        // Step 2: If argument has a [[Call]] internal method, return true.
        Some(ObjectSlot(kind: value.FunctionObject(..), ..)) -> True
        Some(ObjectSlot(kind: value.NativeFunction(..), ..)) -> True
        // Proxy: callable iff target was callable at creation (§10.5.15).
        Some(ObjectSlot(kind: value.ProxyObject(callable:, ..), ..)) -> callable
        // Step 3: Return false.
        _ -> False
      }
    _ -> False
  }
}

/// Get element at index from a list (0-based). O(n).
/// Non-spec utility — used by get_num_arg for argument access.
pub fn list_at(lst: List(a), idx: Int) -> Option(a) {
  case idx, lst {
    0, [x, ..] -> Some(x)
    n, [_, ..rest] -> list_at(rest, n - 1)
    _, [] -> None
  }
}

/// Get first arg or JsUndefined if the list is empty.
/// Non-spec utility — JS functions default missing args to undefined.
pub fn first_arg_or_undefined(args: List(JsValue)) -> JsValue {
  case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
}
