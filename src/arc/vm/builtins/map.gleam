/// ES2024 §24.1 Map Objects
///
/// Map objects are collections of key/value pairs where both the keys and
/// values may be arbitrary ECMAScript language values. A distinct key value
/// may only occur in one key/value pair within the Map's collection. Distinct
/// key values are discriminated using the SameValueZero comparison algorithm.
///
/// Map objects must be implemented using either hash tables or other mechanisms
/// that, on average, provide access times that are sublinear on the number of
/// elements in the collection.
///
/// Storage: an `OrderedEntries(MapKey, JsValue)` store (see
/// `arc/vm/internal/ordered_entries`) — O(log n) get/set/has/delete plus the
/// spec's append-only [[MapData]] insertion order. delete() removes the
/// record; the seq gap is the spec's emptied record, so a
/// deleted-then-re-added key gets a fresh seq and is revisited by in-flight
/// iterators per §24.1.5. Original JS keys are reconstructed via
/// `map_key_to_js` — the MapKey encoding is lossless modulo -0→+0
/// normalization, which the spec requires anyway (§24.1.3.9 step 4).
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/builtins/iter_protocol
import arc/vm/heap
import arc/vm/internal/ordered_entries.{type OrderedEntries}
import arc/vm/key.{Named}
import arc/vm/ops/object
import arc/vm/ops/operators
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type MapKey, type MapNativeFn, type Ref, Dispatch, JsBool,
  JsObject, JsUndefined, MapConstructor, MapNative, MapObject, MapPrototypeClear,
  MapPrototypeDelete, MapPrototypeEntries, MapPrototypeForEach, MapPrototypeGet,
  MapPrototypeGetSize, MapPrototypeHas, MapPrototypeKeys, MapPrototypeSet,
  MapPrototypeValues, ObjectSlot,
}
import gleam/option.{type Option, None, Some}

// ============================================================================
// Init — set up Map constructor + Map.prototype
// ============================================================================

/// Set up Map constructor + Map.prototype.
///
/// ES2024 §24.1.1: "The Map constructor is %Map%. It is the initial value of
/// the Map property of the global object."
///
/// Map.prototype methods:
///   - get(key)
///   - set(key, value)
///   - has(key)
///   - delete(key)
///   - clear()
///   - forEach(callbackfn [, thisArg])
///
/// Map.prototype.size is an accessor property (getter, no setter).
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  // §24.1.3.4 Map.prototype.entries doubles as §24.1.3.13 [@@iterator];
  // §24.1.3.14 [@@toStringTag] = "Map".
  common.init_keyed_collection(
    h,
    object_proto,
    function_proto,
    [
      #("get", MapNative(MapPrototypeGet), 1),
      #("set", MapNative(MapPrototypeSet), 2),
      #("has", MapNative(MapPrototypeHas), 1),
      #("delete", MapNative(MapPrototypeDelete), 1),
      #("clear", MapNative(MapPrototypeClear), 0),
      #("forEach", MapNative(MapPrototypeForEach), 1),
      #("keys", MapNative(MapPrototypeKeys), 0),
      #("values", MapNative(MapPrototypeValues), 0),
    ],
    "entries",
    MapNative(MapPrototypeEntries),
    [],
    MapNative(MapPrototypeGetSize),
    fn(proto) { Dispatch(MapNative(MapConstructor(proto:))) },
    "Map",
  )
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for Map native functions.
pub fn dispatch(
  native: MapNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    MapConstructor(proto:) -> map_constructor(proto, args, state)
    MapPrototypeGet -> map_get(this, args, state)
    MapPrototypeSet -> map_set(this, args, state)
    MapPrototypeHas -> map_has(this, args, state)
    MapPrototypeDelete -> map_delete(this, args, state)
    MapPrototypeClear -> map_clear(this, state)
    MapPrototypeForEach -> map_for_each(this, args, state)
    MapPrototypeGetSize -> map_get_size(this, state)
    MapPrototypeKeys -> map_iterator(this, state, "keys", value.MapIterKeys)
    MapPrototypeValues ->
      map_iterator(this, state, "values", value.MapIterValues)
    MapPrototypeEntries ->
      map_iterator(this, state, "entries", value.MapIterEntries)
  }
}

// ============================================================================
// Map() constructor — ES2024 §24.1.1.1
// ============================================================================

/// ES2024 §24.1.1.1 Map ( [ iterable ] )
///
/// When called with optional argument iterable:
///   1. If NewTarget is undefined, throw a TypeError exception.
///   2. Let map be ? OrdinaryCreateFromConstructor(NewTarget, "%Map.prototype%",
///      « [[MapData]] »).
///   3. Set map.[[MapData]] to a new empty List.
///   4. If iterable is either undefined or null, return map.
///   5. Let adder be ? Get(map, "set").
///   6. If IsCallable(adder) is false, throw a TypeError exception.
///   7. Return ? AddEntriesFromIterable(map, iterable, adder).
///
/// `? Get(map, "set")` is observable, and the iterable goes through
/// §24.1.1.2 AddEntriesFromIterable (a real GetIterator loop), so
/// `new Map(otherMap)`, generators, Sets of pairs and custom iterables all
/// work.
fn map_constructor(
  proto: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: reject a plain call and resolve new.target.prototype.
  use proto, state <- helpers.require_new_target(state, "Map", proto)
  // Steps 2-3: allocate the map with an empty [[MapData]].
  let #(heap, ref) = alloc_map(state.heap, proto, ordered_entries.new())
  let state = State(..state, heap:)
  let map = JsObject(ref)
  case helpers.first_arg_or_undefined(args) {
    // Step 4: If iterable is undefined or null, return map.
    JsUndefined | value.JsNull -> #(state, Ok(map))
    iterable -> {
      // Steps 5-6: adder = ? Get(map, "set"); must be callable.
      use adder, state <- state.try_op(object.get_value_of(
        state,
        map,
        Named("set"),
      ))
      use adder, state <- helpers.require_callable(state, adder, fn() {
        "'set' property of Map is not a function"
      })
      // Step 7: ? AddEntriesFromIterable(map, iterable, adder).
      iter_protocol.add_entries_from_iterable(state, map, iterable, adder)
    }
  }
}

/// Allocate a Map object on the heap.
fn alloc_map(
  heap: Heap(host),
  proto: Ref,
  store: OrderedEntries(MapKey, JsValue),
) -> #(Heap(host), Ref) {
  common.alloc_wrapper(heap, MapObject(store:), proto)
}

// ============================================================================
// Map.prototype.get(key) — ES2024 §24.1.3.6
// ============================================================================

/// ES2024 §24.1.3.6 Map.prototype.get ( key )
///
///   1. Let M be the this value.
///   2. Perform ? RequireInternalSlot(M, [[MapData]]).
///   3. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
///      a. If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true,
///         return p.[[Value]].
///   4. Return undefined.
fn map_get(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let key_arg = helpers.first_arg_or_undefined(args)
  // Steps 1-2: RequireInternalSlot
  use ref, state <- require_map(this, state, "get")
  // Steps 3-4: Look up key
  let map_key = value.js_to_map_key(key_arg)
  let result =
    ordered_entries.get(read_map_store(state.heap, ref), map_key)
    |> option.unwrap(JsUndefined)
  #(state, Ok(result))
}

// ============================================================================
// Map.prototype.set(key, value) — ES2024 §24.1.3.9
// ============================================================================

/// ES2024 §24.1.3.9 Map.prototype.set ( key, value )
///
///   1. Let M be the this value.
///   2. Perform ? RequireInternalSlot(M, [[MapData]]).
///   3. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
///      a. If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true, then
///         i. Set p.[[Value]] to value.
///         ii. Return M.
///   4. If key is -0𝔽, set key to +0𝔽.
///   5. Let p be the Record { [[Key]]: key, [[Value]]: value }.
///   6. Append p to M.[[MapData]].
///   7. Return M.
///
/// Important: Returns `this` (the Map), NOT the value.
fn map_set(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(key_arg, val_arg) = helpers.two_args_or_undefined(args)
  // Steps 1-2: RequireInternalSlot
  use ref, state <- require_map(this, state, "set")
  let store = read_map_store(state.heap, ref)

  // Step 4 (-0 → +0) happens inside js_to_map_key
  let map_key = value.js_to_map_key(key_arg)
  // Steps 3, 5-6: an existing key keeps its insertion position; a new key —
  // including a deleted-then-re-added one — appends past every live
  // iterator's cursor.
  let store = ordered_entries.insert(store, map_key, val_arg)

  // Write updated MapObject back to heap
  let heap = update_map_data(state.heap, ref, store)

  // Step 7: Return M
  #(State(..state, heap:), Ok(this))
}

// ============================================================================
// Map.prototype.has(key) — ES2024 §24.1.3.7
// ============================================================================

/// ES2024 §24.1.3.7 Map.prototype.has ( key )
///
///   1. Let M be the this value.
///   2. Perform ? RequireInternalSlot(M, [[MapData]]).
///   3. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
///      a. If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true,
///         return true.
///   4. Return false.
fn map_has(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let key_arg = helpers.first_arg_or_undefined(args)
  use ref, state <- require_map(this, state, "has")
  let map_key = value.js_to_map_key(key_arg)
  #(
    state,
    Ok(JsBool(ordered_entries.has(read_map_store(state.heap, ref), map_key))),
  )
}

// ============================================================================
// Map.prototype.delete(key) — ES2024 §24.1.3.3
// ============================================================================

/// ES2024 §24.1.3.3 Map.prototype.delete ( key )
///
///   1. Let M be the this value.
///   2. Perform ? RequireInternalSlot(M, [[MapData]]).
///   3. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
///      a. If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true, then
///         i. Set p.[[Key]] to empty.
///         ii. Set p.[[Value]] to empty.
///         iii. Return true.
///   4. Return false.
///
/// Removes the record entirely; the seq gap left in `order` is the spec's
/// emptied record (skipped by iterator cursors in O(1)).
fn map_delete(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let key_arg = helpers.first_arg_or_undefined(args)
  use ref, state <- require_map(this, state, "delete")
  let store = read_map_store(state.heap, ref)
  let map_key = value.js_to_map_key(key_arg)
  case ordered_entries.delete(store, map_key) {
    #(_store, False) -> #(state, Ok(JsBool(False)))
    #(store, True) -> {
      let heap = update_map_data(state.heap, ref, store)
      #(State(..state, heap:), Ok(JsBool(True)))
    }
  }
}

// ============================================================================
// Map.prototype.clear() — ES2024 §24.1.3.2
// ============================================================================

/// ES2024 §24.1.3.2 Map.prototype.clear ( )
///
///   1. Let M be the this value.
///   2. Perform ? RequireInternalSlot(M, [[MapData]]).
///   3. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
///      a. Set p.[[Key]] to empty.
///      b. Set p.[[Value]] to empty.
///   4. Return undefined.
fn map_clear(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_map(this, state, "clear")
  let store = read_map_store(state.heap, ref)
  // next_seq is preserved by clear(): the spec's records are emptied but
  // appends still land past in-flight iterator cursors, so they remain
  // visited.
  let heap = update_map_data(state.heap, ref, ordered_entries.clear(store))
  #(State(..state, heap:), Ok(JsUndefined))
}

// ============================================================================
// Map.prototype.forEach(callbackfn [, thisArg]) — ES2024 §24.1.3.5
// ============================================================================

/// ES2024 §24.1.3.5 Map.prototype.forEach ( callbackfn [ , thisArg ] )
///
///   1. Let M be the this value.
///   2. Perform ? RequireInternalSlot(M, [[MapData]]).
///   3. If IsCallable(callbackfn) is false, throw a TypeError exception.
///   4. Let entries be M.[[MapData]].
///   5. For each Record { [[Key]], [[Value]] } e of entries, do
///      a. If e.[[Key]] is not empty, then
///         i. Perform ? Call(callbackfn, thisArg, « e.[[Value]], e.[[Key]], M »).
///   6. Return undefined.
///
/// Note: The callback receives (value, key, map) — value first, key second.
///
/// Step order matters and is observable: `Map.prototype.forEach.call({}, 1)`
/// throws the brand TypeError (step 2), not the callback TypeError (step 3).
fn map_for_each(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Extract callbackfn and thisArg
  let #(cb, this_arg) = helpers.two_args_or_undefined(args)

  // Steps 1-2: RequireInternalSlot — before the IsCallable check.
  use ref, state <- require_map(this, state, "forEach")
  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  use cb, state <- helpers.require_callable(state, cb, fn() {
    operators.typeof(state.heap, cb) <> " is not a function"
  })
  // Steps 4-5: LIVE iteration by seq cursor — the source is re-read from
  // the heap each step, so entries the callback deletes before being
  // reached are skipped and entries it adds (including delete + re-add)
  // are visited, per the spec's index-based [[MapData]] walk.
  for_each_loop(state, ref, 0, cb, this_arg, this)
}

/// Inner loop for Map.prototype.forEach — advances a seq cursor over the
/// source Map's live records, re-reading the source each step.
fn for_each_loop(
  state: State(host),
  ref: MapRef,
  cursor: Int,
  cb: JsValue,
  this_arg: JsValue,
  map_this: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  let store = read_map_store(state.heap, ref)
  case ordered_entries.next_from(store, cursor) {
    None -> #(state, Ok(JsUndefined))
    Some(#(next_cursor, map_key, val)) -> {
      // Reconstruct original JS key. map_key_to_js is lossless (-0 already
      // normalized to +0 per spec §24.1.3.9 step 4).
      let original_key = value.map_key_to_js(map_key)
      // Step 5a.i: Call(callbackfn, thisArg, « e.[[Value]], e.[[Key]], M »)
      use _result, state <- state.try_call(state, cb, this_arg, [
        val,
        original_key,
        map_this,
      ])
      for_each_loop(state, ref, next_cursor, cb, this_arg, map_this)
    }
  }
}

// ============================================================================
// get Map.prototype.size — ES2024 §24.1.3.10
// ============================================================================

/// ES2024 §24.1.3.10 get Map.prototype.size
///
///   1. Let M be the this value.
///   2. Perform ? RequireInternalSlot(M, [[MapData]]).
///   3. Let count be 0.
///   4. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
///      a. If p.[[Key]] is not empty, set count to count + 1.
///   5. Return 𝔽(count).
fn map_get_size(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_map(this, state, "size")
  #(
    state,
    Ok(value.from_int(ordered_entries.size(read_map_store(state.heap, ref)))),
  )
}

// ============================================================================
// Map.prototype.keys() / values() / entries() — ES2024 §24.1.3.8/11/4
// ============================================================================

/// CreateMapIterator (§24.1.5.1) — a LIVE iterator over the source Map.
/// The iterator's `kind` controls what .next() yields (key only / value
/// only / [key,value] array); entries added during iteration are visited.
fn map_iterator(
  this: JsValue,
  state: State(host),
  method: String,
  kind: value.MapIterKind,
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_map(this, state, method)
  let #(heap, iter_ref) =
    common.alloc_wrapper(
      state.heap,
      value.MapIteratorObject(
        source: map_ref_value(ref),
        cursor: 0,
        done: False,
        kind:,
      ),
      state.builtins.map_iterator_proto,
    )
  #(State(..state, heap:), Ok(JsObject(iter_ref)))
}

// ============================================================================
// Helpers
// ============================================================================

/// A `Ref` that has been *proved* to point at a Map's heap slot.
///
/// Constructible only by `require_map`, so a Set ref, a MapIterator ref
/// or `Map.prototype` itself cannot reach `read_map_store` / `update_map_data`
/// — passing one is a compile error rather than a silent read of an empty map.
type MapRef {
  MapRef(Ref)
}

/// The raw `Ref` inside a proved `MapRef`, for the few places that need it
/// (allocating a MapIterator over the source Map).
fn map_ref_value(map_ref: MapRef) -> Ref {
  let MapRef(ref) = map_ref
  ref
}

/// Read a proved Map's [[MapData]] out of the heap.
///
/// This is the ONLY way to get at a Map's store: `require_map` hands out a
/// `MapRef`, never a snapshot. Every op re-reads at the exact spec point at
/// which the spec inspects [[MapData]] — the forEach callback runs arbitrary
/// user code that can add to or delete from the receiver mid-iteration.
fn read_map_store(
  h: Heap(host),
  map_ref: MapRef,
) -> OrderedEntries(MapKey, JsValue) {
  // A heap slot's kind never changes after allocation and a `MapRef` can only
  // come from `require_map`, so anything else here is a wiring bug — crash
  // rather than silently reporting an empty map.
  let assert Some(ObjectSlot(kind: MapObject(store:), ..)) =
    heap.read(h, map_ref_value(map_ref))
    as "map: MapRef does not point at a Map slot"
  store
}

/// Update the MapObject data on an existing heap slot.
fn update_map_data(
  h: Heap(host),
  map_ref: MapRef,
  store: OrderedEntries(MapKey, JsValue),
) -> Heap(host) {
  heap.update_kind(h, map_ref_value(map_ref), MapObject(store:))
}

/// RequireInternalSlot(M, [[MapData]]) — proves `this` is a Map and hands
/// over a `MapRef`, or throws a TypeError naming `method`.
///
/// Deliberately hands the continuation only a `MapRef`, never the [[MapData]]
/// store: a store read at method entry is stale the moment any user code runs
/// (forEach's callback). Read the store with `read_map_store` at the point
/// the spec reads it.
///
/// CPS-style — `use ref, state <- require_map(this, state, "get")`.
fn require_map(
  this: JsValue,
  state: State(host),
  method: String,
  cont: fn(MapRef, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use Nil, ref, state <- helpers.require_brand(
    this,
    state,
    fn() {
      "Method Map.prototype." <> method <> " called on incompatible receiver"
    },
    map_brand_of,
  )
  cont(MapRef(ref), state)
}

/// The [[MapData]] brand check handed to `require_brand` — a named function
/// (not an inline lambda) so the hot brand check builds no closure per call.
fn map_brand_of(kind: state.ExoticKind(host)) -> Option(Nil) {
  case kind {
    MapObject(..) -> Some(Nil)
    _ -> None
  }
}
