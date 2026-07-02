/// Shared runtime helpers for builtins.
///
/// Lives one layer above `builtins/common` so it may depend on `state` and
/// `ops/object` (`common` cannot — `state` imports it).
import arc/vm/heap.{type Heap}
import arc/vm/ops/object as ops_object
import arc/vm/state.{type State}
import arc/vm/value.{
  type JsValue, type Ref, JsObject, JsSymbol, JsUndefined, ObjectSlot,
}
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}

/// Shared step-1 guard for native constructors that must not be [[Call]]ed
/// (Map, Set, WeakMap, WeakSet, DOMException, ...):
///
///   1. If NewTarget is undefined, throw a TypeError exception.
///   2. Let O be ? OrdinaryCreateFromConstructor(NewTarget, fallback_proto).
///
/// `do_construct` sets `state.new_target` before native dispatch; a plain
/// call leaves it JsUndefined. Step 2's prototype lookup goes through
/// §10.1.13 GetPrototypeFromConstructor via a real [[Get]] of
/// `new.target.prototype` (accessor `prototype` properties are invoked),
/// falling back to the intrinsic `fallback_proto` when the result is not an
/// object — so subclasses get their own prototype. CPS-style:
///
///   use proto_ref, state <- helpers.require_new_target(state, "Map", proto)
pub fn require_new_target(
  state: State(host),
  ctor: String,
  fallback_proto: Ref,
  cont: fn(Ref, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case state.new_target {
    JsUndefined ->
      state.type_error(state, "Constructor " <> ctor <> " requires 'new'")
    new_target ->
      ops_object.proto_from_new_target(state, new_target, fallback_proto, cont)
  }
}

/// ES2024 §9.13 CanBeHeldWeakly ( v )
///
/// True for objects and non-registered Symbols — a symbol minted by
/// `Symbol.for` lives in the global symbol registry and can never be
/// collected, so it can't be held weakly. The single shared predicate for
/// WeakMap keys, WeakSet members, and FinalizationRegistry targets /
/// unregister tokens (and WeakRef, once implemented).
pub fn can_be_held_weakly(state: State(host), v: JsValue) -> Bool {
  case v {
    JsObject(_) -> True
    JsSymbol(id) -> !list.contains(dict.values(state.ctx.symbol_registry), id)
    _ -> False
  }
}

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
