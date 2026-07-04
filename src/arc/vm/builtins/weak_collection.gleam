/// Shared skeleton for the two weak keyed collections, WeakMap (§24.3) and
/// WeakSet (§24.4). They differ in exactly three ways: the `ExoticKind`
/// constructor that carries the entry dict, the dict's value type
/// (`JsValue` for WeakMap, `Nil` for WeakSet), and the type name used in
/// error messages. `WeakKind` packages those three; everything else —
/// RequireInternalSlot, the read/mutate discipline, `has`, `delete` and the
/// `new X(iterable)` constructor — lives here once.
///
/// The safety property this module exists to enforce: a caller can never write
/// back a *stale* entry dict over mutations that user code made re-entrantly
/// (a `getOrInsertComputed` callback that `set`s the same key). `require`
/// hands out only a `WeakRef` — no dict — and `WeakRef` is opaque, so the only
/// way to reach the entries is `read_data`/`mutate` at the moment of use, both
/// of which read the live heap slot.
import arc/vm/builtins/common
import arc/vm/builtins/helpers.{first_arg_or_undefined, is_callable}
import arc/vm/heap
import arc/vm/key.{Named}
import arc/vm/ops/object
import arc/vm/state.{type State, State}
import arc/vm/value.{
  type JsValue, type Ref, JsBool, JsNull, JsObject, JsUndefined, ObjectSlot,
}
import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}

/// Everything that distinguishes a WeakMap from a WeakSet.
///
///   - `unwrap` — recognise our own heap slot kind and read its entry dict.
///   - `wrap` — rebuild the slot kind around a new entry dict.
///   - `type_name` — "WeakMap" / "WeakSet", for TypeError messages.
pub type WeakKind(host, v) {
  WeakKind(
    unwrap: fn(state.ExoticKind(host)) -> Option(Dict(JsValue, v)),
    wrap: fn(Dict(JsValue, v)) -> state.ExoticKind(host),
    type_name: String,
  )
}

/// A `Ref` that has been *proved* to point at a weak collection's heap slot.
///
/// Opaque, and constructible only by `require`, so "a ref that isn't a weak
/// collection" cannot reach `read_data`/`mutate` — the assertions in those
/// functions are unreachable by construction rather than a silent no-op.
pub opaque type WeakRef {
  WeakRef(Ref)
}

/// RequireInternalSlot(this, [[WeakMapData]] / [[WeakSetData]]) — proves `this`
/// is a collection of `kind` and hands over a `WeakRef`, or throws a TypeError
/// naming `method`.
///
/// Deliberately does *not* hand over the entry dict: callers must fetch it with
/// `read_data` at the point of use, so nothing can capture a snapshot across a
/// call into user code.
///
/// CPS-style — `use ref, state <- require(kind, this, state, "get")`.
pub fn require(
  kind: WeakKind(host, v),
  this: JsValue,
  state: State(host),
  method: String,
  cont: fn(WeakRef, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let found = case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: slot_kind, ..)) ->
          kind.unwrap(slot_kind) |> option.map(fn(_) { WeakRef(ref) })
        _ -> None
      }
    _ -> None
  }
  case found {
    Some(weak_ref) -> cont(weak_ref, state)
    None ->
      state.type_error(
        state,
        "Method "
          <> kind.type_name
          <> ".prototype."
          <> method
          <> " called on incompatible receiver",
      )
  }
}

/// The collection's *live* entry dict, read straight from the heap.
///
/// Call this at the moment the entries are needed — never before running user
/// code — so a re-entrant mutation is always visible.
pub fn read_data(
  kind: WeakKind(host, v),
  state: State(host),
  weak_ref: WeakRef,
) -> Dict(JsValue, v) {
  let WeakRef(ref) = weak_ref
  // A heap slot's kind never changes after allocation, and a `WeakRef` can only
  // come from `require`, so anything else here is a wiring bug — crash rather
  // than silently reporting an empty collection.
  let assert Some(ObjectSlot(kind: slot_kind, ..)) = heap.read(state.heap, ref)
    as "weak_collection: WeakRef does not point at an object slot"
  let assert Some(data) = kind.unwrap(slot_kind)
    as "weak_collection: WeakRef points at a slot of the wrong kind"
  data
}

/// Read-modify-write the entry dict inside a single heap access.
///
/// Takes a *function* rather than a finished dict on purpose: a caller cannot
/// hand back a dict it captured before running user code, so a re-entrant
/// mutation (a `getOrInsertComputed` callback that sets the same key) can never
/// be silently reverted by a stale write.
pub fn mutate(
  kind: WeakKind(host, v),
  state: State(host),
  weak_ref: WeakRef,
  update: fn(Dict(JsValue, v)) -> Dict(JsValue, v),
) -> State(host) {
  let WeakRef(ref) = weak_ref
  let data = read_data(kind, state, weak_ref)
  let heap = heap.update_kind(state.heap, ref, kind.wrap(update(data)))
  State(..state, heap:)
}

/// ES2024 §24.3.3.4 WeakMap.prototype.has / §24.4.3.3 WeakSet.prototype.has
///
/// Non-weakly-holdable values are never present, so this is a plain lookup with
/// no CanBeHeldWeakly gate.
pub fn has(
  kind: WeakKind(host, v),
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require(kind, this, state, "has")
  let key = first_arg_or_undefined(args)
  #(state, Ok(JsBool(dict.has_key(read_data(kind, state, ref), key))))
}

/// ES2024 §24.3.3.3 WeakMap.prototype.delete / §24.4.3.2 WeakSet.prototype.delete
pub fn delete(
  kind: WeakKind(host, v),
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require(kind, this, state, "delete")
  let key = first_arg_or_undefined(args)
  case dict.has_key(read_data(kind, state, ref), key) {
    True -> #(mutate(kind, state, ref, dict.delete(_, key)), Ok(JsBool(True)))
    False -> #(state, Ok(JsBool(False)))
  }
}

/// ES2024 §24.3.1.1 WeakMap ( [ iterable ] ) / §24.4.1.1 WeakSet ( [ iterable ] )
///
///   1. If NewTarget is undefined, throw a TypeError exception.
///   2. Let coll be ? OrdinaryCreateFromConstructor(NewTarget, "%X.prototype%").
///   3. Set coll's data slot to a new empty List.
///   4. If iterable is undefined or null, return coll.
///   5-6. adder = ? Get(coll, adder_name); must be callable.
///   7. Return ? add_from_iterable(coll, iterable, adder).
///
/// `adder_name` is "set" for WeakMap and "add" for WeakSet; `add_from_iterable`
/// is the corresponding AddEntriesFromIterable / value-iteration helper.
pub fn construct(
  kind: WeakKind(host, v),
  proto: Ref,
  args: List(JsValue),
  state: State(host),
  adder_name: String,
  add_from_iterable: fn(State(host), JsValue, JsValue, JsValue) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: reject a plain call and resolve new.target.prototype.
  use proto, state <- helpers.require_new_target(state, kind.type_name, proto)
  let #(heap, coll_ref) =
    common.alloc_wrapper(state.heap, kind.wrap(dict.new()), proto)
  let state = State(..state, heap:)
  let coll = JsObject(coll_ref)
  case first_arg_or_undefined(args) {
    // Step 4: If iterable is undefined or null, return the collection.
    JsUndefined | JsNull -> #(state, Ok(coll))
    iterable -> {
      // Steps 5-6: adder = ? Get(coll, adder_name); must be callable.
      use adder, state <- state.try_op(object.get_value_of(
        state,
        coll,
        Named(adder_name),
      ))
      case is_callable(state.heap, adder) {
        False ->
          state.type_error(
            state,
            "'"
              <> adder_name
              <> "' property of "
              <> kind.type_name
              <> " is not a function",
          )
        // Step 7: iterate, calling adder(coll, ...) for each entry/value and
        // closing the iterator on any abrupt completion.
        True -> add_from_iterable(state, coll, iterable, adder)
      }
    }
  }
}
