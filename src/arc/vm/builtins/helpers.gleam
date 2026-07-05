/// Shared runtime helpers for builtins.
///
/// Lives one layer above `builtins/common` so it may depend on `state` and
/// `ops/object` (`common` cannot — `state` imports it).
import arc/vm/heap
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
/// `do_construct` threads the real NewTarget into the native dispatch; a
/// plain [[Call]] dispatches the native body with `state.new_target` forced
/// to JsUndefined (the calling frame's ambient newTarget is never inherited
/// — see `call_native`). Step 2's prototype lookup goes through
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

/// The one heap read behind every branded builtin's RequireInternalSlot check:
/// `this` must be an object whose slot kind `extract` recognises. Yields the
/// extracted internal slot alongside the receiver's `Ref`.
///
/// `require_brand` is this plus the TypeError; use `brand_of` directly only
/// when the failure path isn't a plain throw (Intl's `Thrown` result, say).
pub fn brand_of(
  h: state.Heap(host),
  this: JsValue,
  extract: fn(state.ExoticKind(host)) -> Option(a),
) -> Option(#(a, Ref)) {
  case this {
    JsObject(ref) ->
      case heap.read(h, ref) {
        // Not `option.map(fn(v) { #(v, ref) })`: that closure would allocate on
        // the success path of every branded builtin call (Map.get, Set.has, ...).
        Some(ObjectSlot(kind:, ..)) ->
          case extract(kind) {
            Some(v) -> Some(#(v, ref))
            None -> None
          }
        _ -> None
      }
    _ -> None
  }
}

/// RequireInternalSlot(this, [[Slot]]) — `this` must be an object carrying the
/// brand `extract` recognises, else a TypeError with `msg`. Yields the
/// extracted internal slot and the receiver's `Ref`. CPS-style:
///
///   use store, ref, state <- helpers.require_brand(this, state, msg, extract)
///
/// `msg` is a thunk so the (always-concatenated) message costs nothing on the
/// overwhelmingly common path where the brand matches.
pub fn require_brand(
  this: JsValue,
  state: State(host),
  msg: fn() -> String,
  extract: fn(state.ExoticKind(host)) -> Option(a),
  cont: fn(a, Ref, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case brand_of(state.heap, this, extract) {
    Some(#(v, ref)) -> cont(v, ref, state)
    None -> state.type_error(state, msg())
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
/// The set of callable ObjectKinds lives in `heap.ref_is_callable` — the
/// single source of truth so this, `ops_object.value_is_callable`, and
/// `typeof` cannot drift.
pub fn is_callable(h: state.Heap(host), val: JsValue) -> Bool {
  case val {
    JsObject(ref) -> heap.ref_is_callable(h, ref)
    _ -> False
  }
}

/// §7.2.3 IsCallable gate — TypeError with `msg()` when `val` isn't callable,
/// otherwise continue with it. `msg` is a thunk so its (usually concatenated)
/// message costs nothing on the common callable path — same convention as
/// `require_brand`. CPS-style:
///
///   use cb, state <- helpers.require_callable(state, cb, fn() { "callback…" })
pub fn require_callable(
  state: State(host),
  val: JsValue,
  msg: fn() -> String,
  cont: fn(JsValue, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case is_callable(state.heap, val) {
    True -> cont(val, state)
    False -> state.type_error(state, msg())
  }
}

/// Bool gate for `use` chains: continue when `cond` holds, else `or_else()`.
/// The generic CPS `bool.lazy_guard` — polymorphic in return so it works for
/// any dispatch shape (Atomics, DataView, …), and the `Nil` param binds
/// explicitly per house style: `use Nil <- helpers.guard(ok, fn() { … })`.
pub fn guard(cond: Bool, or_else: fn() -> r, cont: fn(Nil) -> r) -> r {
  case cond {
    True -> cont(Nil)
    False -> or_else()
  }
}

/// `guard`'s Option twin: continue with the value when present, else
/// `or_else()`. `use v <- helpers.some_or(opt, fn() { type_error(…) })`.
pub fn some_or(opt: Option(a), or_else: fn() -> r, cont: fn(a) -> r) -> r {
  case opt {
    Some(v) -> cont(v)
    None -> or_else()
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

/// The i-th argument, or `undefined` when the caller passed fewer — JS's rule
/// that a missing argument is undefined.
///
/// `first_arg_or_undefined` is this same rule at index 0. It stays a separate
/// function because index 0 is overwhelmingly the common case and it can
/// answer without walking the list.
pub fn arg_at(args: List(JsValue), idx: Int) -> JsValue {
  list_at(args, idx) |> option.unwrap(JsUndefined)
}

/// `arg_at(args, 0)` without the walk — the first argument, or `undefined`
/// when there is none.
pub fn first_arg_or_undefined(args: List(JsValue)) -> JsValue {
  case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
}

/// The first two arguments, each `undefined` when the caller passed fewer.
/// One list match, no walk — the shape most two-parameter builtins want.
pub fn two_args_or_undefined(args: List(JsValue)) -> #(JsValue, JsValue) {
  case args {
    [a, b, ..] -> #(a, b)
    [a] -> #(a, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
}

/// The first three arguments, each `undefined` when the caller passed fewer.
pub fn three_args_or_undefined(
  args: List(JsValue),
) -> #(JsValue, JsValue, JsValue) {
  case args {
    [a, b, c, ..] -> #(a, b, c)
    [a, b] -> #(a, b, JsUndefined)
    [a] -> #(a, JsUndefined, JsUndefined)
    [] -> #(JsUndefined, JsUndefined, JsUndefined)
  }
}

/// Lift the internal `Result(#(v, state), #(e, state))` chain shape into the
/// builtin dispatch tuple `#(state, Result(v, e))`. The adapter at the end of
/// a `use ... <- result.try` pipeline that threads state through both arms.
pub fn lift_result(
  r: Result(#(a, State(host)), #(b, State(host))),
) -> #(State(host), Result(a, b)) {
  case r {
    Ok(#(v, state)) -> #(state, Ok(v))
    Error(#(e, state)) -> #(state, Error(e))
  }
}
