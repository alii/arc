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
/// way to reach the entries is `lookup`/`insert` at the moment of use, both of
/// which read the live heap slot.
///
/// Two proof-carrying tokens keep the discipline honest, both mintable only by
/// the check that establishes them: a `WeakRef` (from `require`) carries the
/// `WeakKind` it was proved against, and a `WeakKey` (from `require_weak_key`)
/// carries a value proved to satisfy §9.13 CanBeHeldWeakly.
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
import gleam/option.{type Option, Some}

/// Everything that distinguishes a WeakMap from a WeakSet.
///
///   - `unwrap` — recognise our own heap slot kind and read its entry dict.
///   - `wrap` — rebuild the slot kind around a new entry dict.
///   - `type_name` — "WeakMap" / "WeakSet", for TypeError messages.
///   - `invalid_key_message` — the TypeError thrown when a key/value fails
///     §9.13 CanBeHeldWeakly. Owned by the kind so `require_weak_key` is the
///     single place that phrases it.
pub type WeakKind(host, v) {
  WeakKind(
    unwrap: fn(state.ExoticKind(host)) -> Option(Dict(JsValue, v)),
    wrap: fn(Dict(JsValue, v)) -> state.ExoticKind(host),
    type_name: String,
    invalid_key_message: String,
  )
}

/// A `Ref` that has been *proved* to point at a weak collection's heap slot,
/// carrying the very `WeakKind` it was proved against.
///
/// Opaque, and constructible only by `require`, so "a ref that isn't a weak
/// collection" cannot reach `lookup`/`insert` — the assertions behind them are
/// unreachable by construction rather than a silent no-op. And
/// because the kind travels *inside* the ref rather than alongside it as a
/// second argument, "a WeakSet's ref read with `weak_map.kind()`" is not
/// expressible either.
pub opaque type WeakRef(host, v) {
  WeakRef(ref: Ref, kind: WeakKind(host, v))
}

/// A `JsValue` that has been *proved* to satisfy §9.13 CanBeHeldWeakly.
///
/// Opaque, and constructible only by `require_weak_key`, so `insert` — the only
/// way to add an entry — cannot be reached with an ungated key: forgetting the
/// gate is a compile error, not a spec violation.
pub opaque type WeakKey {
  WeakKey(JsValue)
}

/// RequireInternalSlot(this, [[WeakMapData]] / [[WeakSetData]]) — proves `this`
/// is a collection of `kind` and hands over a `WeakRef`, or throws a TypeError
/// naming `method`.
///
/// Deliberately does *not* hand over the entry dict: callers reach entries via
/// `lookup` at the point of use, so nothing can capture a snapshot across a
/// call into user code.
///
/// CPS-style — `use ref, state <- require(kind, this, state, "get")`.
pub fn require(
  kind: WeakKind(host, v),
  this: JsValue,
  state: State(host),
  method: String,
  cont: fn(WeakRef(host, v), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use _data, ref, state <- helpers.require_brand(
    this,
    state,
    fn() {
      "Method "
      <> kind.type_name
      <> ".prototype."
      <> method
      <> " called on incompatible receiver"
    },
    kind.unwrap,
  )
  cont(WeakRef(ref:, kind:), state)
}

/// ES2024 §9.13 CanBeHeldWeakly — proves `key` may be held weakly and hands
/// over a `WeakKey`, or throws the ref's kind's TypeError.
///
/// The only way to mint a `WeakKey`, and `insert` takes nothing else, so every
/// insertion path is gated whether or not its author remembered to gate it.
/// Takes the `WeakRef` (not a bare `WeakKind`) so the error message can never
/// disagree with the collection the caller is operating on.
///
/// CPS-style — `use key, state <- require_weak_key(ref, state, key)`.
pub fn require_weak_key(
  ref: WeakRef(host, v),
  state: State(host),
  key: JsValue,
  cont: fn(WeakKey, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let WeakRef(kind:, ..) = ref
  case helpers.can_be_held_weakly(key) {
    True -> cont(WeakKey(key), state)
    False -> state.type_error(state, kind.invalid_key_message)
  }
}

/// The collection's *live* entry dict, read straight from the heap.
///
/// Private: external callers reach entries only via `lookup`/`insert`/`delete`,
/// so nothing outside this module can capture the whole dict as a snapshot.
fn read_data(state: State(host), weak_ref: WeakRef(host, v)) -> Dict(JsValue, v) {
  let WeakRef(ref:, kind:) = weak_ref
  // A heap slot's kind never changes after allocation, and a `WeakRef` can only
  // come from `require`, so anything else here is a wiring bug — crash rather
  // than silently reporting an empty collection.
  let assert Some(ObjectSlot(kind: slot_kind, ..)) = heap.read(state.heap, ref)
    as "weak_collection: WeakRef does not point at an object slot"
  let assert Some(data) = kind.unwrap(slot_kind)
    as "weak_collection: WeakRef points at a slot of the wrong kind"
  data
}

/// Look up `key` in the collection's *live* entry dict.
///
/// Call this at the moment the entry is needed — never before running user
/// code — so a re-entrant mutation is always visible. The only external read
/// path, so callers cannot capture the whole dict.
pub fn lookup(
  state: State(host),
  ref: WeakRef(host, v),
  key: JsValue,
) -> Option(v) {
  dict.get(read_data(state, ref), key) |> option.from_result
}

/// Read-modify-write the entry dict inside a single heap access.
///
/// Takes a *function* rather than a finished dict on purpose: a caller cannot
/// hand back a dict it captured before running user code, so a re-entrant
/// mutation (a `getOrInsertComputed` callback that sets the same key) can never
/// be silently reverted by a stale write.
///
/// Private: the only mutations a weak collection admits are `insert` (which
/// demands a proved `WeakKey`) and `delete` below.
fn mutate(
  state: State(host),
  weak_ref: WeakRef(host, v),
  update: fn(Dict(JsValue, v)) -> Dict(JsValue, v),
) -> State(host) {
  let WeakRef(ref:, kind:) = weak_ref
  let data = read_data(state, weak_ref)
  let heap = heap.update_kind(state.heap, ref, kind.wrap(update(data)))
  State(..state, heap:)
}

/// Add (or overwrite) an entry, re-reading the live entry dict at the moment of
/// the write. Requires a `WeakKey`, so the CanBeHeldWeakly gate has provably run.
pub fn insert(
  state: State(host),
  weak_ref: WeakRef(host, v),
  key: WeakKey,
  val: v,
) -> State(host) {
  let WeakKey(key) = key
  mutate(state, weak_ref, dict.insert(_, key, val))
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
  #(state, Ok(JsBool(dict.has_key(read_data(state, ref), key))))
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
  case dict.has_key(read_data(state, ref), key) {
    True -> #(mutate(state, ref, dict.delete(_, key)), Ok(JsBool(True)))
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
