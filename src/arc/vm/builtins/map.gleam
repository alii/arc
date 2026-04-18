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
/// Storage: `Dict(MapKey, JsValue)` for O(log n) get/set/has/delete, plus a
/// reversed `List(MapKey)` for insertion-order iteration. Keys are stored
/// reversed so set() is O(1) prepend; iteration points reverse once on read.
/// Deleted keys remain as tombstones in the list (skipped via dict lookup at
/// iteration time). Original JS keys are reconstructed via `map_key_to_js` —
/// the MapKey encoding is lossless modulo -0→+0 normalization, which the spec
/// requires anyway (§24.1.3.9 step 4).
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type MapKey, type MapNativeFn, type Ref, Dispatch, JsBool,
  JsObject, JsUndefined, MapConstructor, MapNative, MapObject, MapPrototypeClear,
  MapPrototypeDelete, MapPrototypeEntries, MapPrototypeForEach, MapPrototypeGet,
  MapPrototypeGetSize, MapPrototypeHas, MapPrototypeKeys, MapPrototypeSet,
  MapPrototypeValues, ObjectSlot,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set
import gleam/string

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
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  // Allocate prototype method function objects (entries handled separately so
  // it can alias [Symbol.iterator] to the SAME function object — test262
  // built-ins/Map/prototype/Symbol.iterator.js asserts strict equality).
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("get", MapNative(MapPrototypeGet), 1),
      #("set", MapNative(MapPrototypeSet), 2),
      #("has", MapNative(MapPrototypeHas), 1),
      #("delete", MapNative(MapPrototypeDelete), 1),
      #("clear", MapNative(MapPrototypeClear), 0),
      #("forEach", MapNative(MapPrototypeForEach), 1),
      #("keys", MapNative(MapPrototypeKeys), 0),
      #("values", MapNative(MapPrototypeValues), 0),
    ])

  // §24.1.3.4 Map.prototype.entries — also installed as [@@iterator]
  let #(h, entries_fn) =
    common.alloc_native_fn(
      h,
      function_proto,
      MapNative(MapPrototypeEntries),
      "entries",
      0,
    )
  let entries_prop = value.builtin_property(JsObject(entries_fn))

  // size accessor property (getter, no setter)
  let #(h, getters) =
    common.alloc_getters(h, function_proto, [
      #("size", MapNative(MapPrototypeGetSize)),
    ])
  let proto_methods =
    list.append(getters, [#("entries", entries_prop), ..proto_methods])

  // Build the prototype + constructor using the standard init_type helper.
  // The constructor carries the proto ref so it can set [[Prototype]] on
  // new Map instances.
  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(proto) { Dispatch(MapNative(MapConstructor(proto:))) },
      "Map",
      0,
      [],
    )
  // §24.1.3.14 Map.prototype [ @@toStringTag ] = "Map"
  // { writable: false, enumerable: false, configurable: true }
  let h = common.add_to_string_tag(h, bt.prototype, "Map")
  // §24.1.3.13 Map.prototype [ @@iterator ] — same function object as .entries
  let h =
    common.add_symbol_property(
      h,
      bt.prototype,
      value.symbol_iterator,
      entries_prop,
    )
  #(h, bt)
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for Map native functions.
pub fn dispatch(
  native: MapNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    MapConstructor(proto:) -> map_constructor(proto, args, state)
    MapPrototypeGet -> map_get(this, args, state)
    MapPrototypeSet -> map_set(this, args, state)
    MapPrototypeHas -> map_has(this, args, state)
    MapPrototypeDelete -> map_delete(this, args, state)
    MapPrototypeClear -> map_clear(this, state)
    MapPrototypeForEach -> map_for_each(this, args, state)
    MapPrototypeGetSize -> map_get_size(this, state)
    MapPrototypeKeys -> map_iterator(this, state, value.MapIterKeys)
    MapPrototypeValues -> map_iterator(this, state, value.MapIterValues)
    MapPrototypeEntries -> map_iterator(this, state, value.MapIterEntries)
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
/// Simplifications:
///   - NewTarget check skipped (VM handles constructor call path).
///   - For MVP, iterable support only handles arrays of [key, value] pairs.
///   - Full iterator protocol (Symbol.iterator) not yet implemented.
fn map_constructor(
  proto: Ref,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 4: If iterable is undefined or null, return empty map.
  case args {
    [] | [JsUndefined, ..] | [value.JsNull, ..] -> {
      let #(heap, ref) = alloc_map(state.heap, proto, dict.new(), [], 0)
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    [iterable, ..] ->
      // Step 7: AddEntriesFromIterable (simplified — only array of arrays)
      add_entries_from_iterable(state, proto, iterable, dict.new(), [], 0)
  }
}

/// Allocate a Map object on the heap.
fn alloc_map(
  heap: Heap,
  proto: Ref,
  entries: dict.Dict(MapKey, JsValue),
  keys_rev: List(MapKey),
  keys_len: Int,
) -> #(Heap, Ref) {
  common.alloc_wrapper(heap, MapObject(entries:, keys_rev:, keys_len:), proto)
}

/// Simplified AddEntriesFromIterable — handles array of [key, value] pairs.
///
/// ES2024 §24.1.1.2 AddEntriesFromIterable ( target, iterable, adder ):
///   1. Let iteratorRecord be ? GetIterator(iterable, sync).
///   2. Repeat,
///      a. Let next be ? IteratorStepValue(iteratorRecord).
///      b. If next is done, return target.
///      c. If next is not an Object, throw a TypeError.
///      d. Let k be ? Get(next, "0").
///      e. Let v be ? Get(next, "1").
///      f. Let status be ? Call(adder, target, « k, v »).
///
/// Simplified: only handles Array iterable containing Array pairs.
fn add_entries_from_iterable(
  state: State,
  proto: Ref,
  iterable: JsValue,
  entries: dict.Dict(MapKey, JsValue),
  keys_rev: List(MapKey),
  keys_len: Int,
) -> #(State, Result(JsValue, JsValue)) {
  case iterable {
    JsObject(iter_ref) ->
      case heap.read_array(state.heap, iter_ref) {
        Some(#(length, elements)) ->
          // Iterate through the array entries
          add_entries_loop(
            state,
            proto,
            elements,
            0,
            length,
            entries,
            keys_rev,
            keys_len,
          )
        None ->
          state.type_error(state, "Iterator value is not an entry-like object")
      }
    _ -> state.type_error(state, string.inspect(iterable) <> " is not iterable")
  }
}

/// Loop over array entries for Map constructor.
fn add_entries_loop(
  state: State,
  proto: Ref,
  elements: value.JsElements,
  idx: Int,
  length: Int,
  entries: dict.Dict(MapKey, JsValue),
  keys_rev: List(MapKey),
  keys_len: Int,
) -> #(State, Result(JsValue, JsValue)) {
  case idx >= length {
    True -> {
      // Done — allocate the map with all entries. keys_rev already reversed.
      let #(heap, ref) =
        alloc_map(state.heap, proto, entries, keys_rev, keys_len)
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    False -> {
      let entry = elements.get(elements, idx)
      // Each entry must be an array-like [key, value]
      case entry {
        JsObject(entry_ref) ->
          case heap.read_array(state.heap, entry_ref) {
            Some(#(_, entry_elems)) -> {
              let key = elements.get(entry_elems, 0)
              let val = elements.get(entry_elems, 1)
              let map_key = value.js_to_map_key(key)
              // If key already exists, update value only (keep first-occurrence
              // insertion position per spec)
              let #(keys_rev, keys_len) = case dict.has_key(entries, map_key) {
                True -> #(keys_rev, keys_len)
                False -> #([map_key, ..keys_rev], keys_len + 1)
              }
              let entries = dict.insert(entries, map_key, val)
              add_entries_loop(
                state,
                proto,
                elements,
                idx + 1,
                length,
                entries,
                keys_rev,
                keys_len,
              )
            }
            None ->
              state.type_error(
                state,
                "Iterator value "
                  <> int.to_string(idx)
                  <> " is not an entry-like object",
              )
          }
        _ ->
          state.type_error(
            state,
            "Iterator value "
              <> int.to_string(idx)
              <> " is not an entry-like object",
          )
      }
    }
  }
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let key_arg = case args {
    [k, ..] -> k
    [] -> JsUndefined
  }
  // Steps 1-2: RequireInternalSlot
  use entries, _keys_rev, _keys_len, _ref, state <- require_map(this, state)
  // Steps 3-4: Look up key
  let map_key = value.js_to_map_key(key_arg)
  let result = case dict.get(entries, map_key) {
    Ok(val) -> val
    Error(Nil) -> JsUndefined
  }
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(key_arg, val_arg) = case args {
    [k, v, ..] -> #(k, v)
    [k] -> #(k, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
  // Steps 1-2: RequireInternalSlot
  use entries, keys_rev, keys_len, ref, state <- require_map(this, state)

  // Step 4 (-0 → +0) happens inside js_to_map_key
  let map_key = value.js_to_map_key(key_arg)
  // Step 3: Check if key already exists
  let #(keys_rev, keys_len) = case dict.has_key(entries, map_key) {
    True -> #(keys_rev, keys_len)
    False -> #([map_key, ..keys_rev], keys_len + 1)
  }
  let entries = dict.insert(entries, map_key, val_arg)

  // Compact tombstones/dupes when the list has grown to 2× live entries.
  // Bounds worst-case list bloat under delete+re-add cycles.
  let size = dict.size(entries)
  let #(keys_rev, keys_len) = case keys_len > size * 2 {
    True -> #(compact_keys(keys_rev, entries), size)
    False -> #(keys_rev, keys_len)
  }

  // Write updated MapObject back to heap
  let heap = update_map_data(state.heap, ref, entries, keys_rev, keys_len)

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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let key_arg = case args {
    [k, ..] -> k
    [] -> JsUndefined
  }
  use entries, _keys_rev, _keys_len, _ref, state <- require_map(this, state)
  let map_key = value.js_to_map_key(key_arg)
  #(state, Ok(JsBool(dict.has_key(entries, map_key))))
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
/// Tombstone delete: remove from entries dict only. The key stays in keys_rev
/// and is skipped at iteration time via dict lookup. Compaction in map_set
/// bounds list growth.
fn map_delete(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let key_arg = case args {
    [k, ..] -> k
    [] -> JsUndefined
  }
  use entries, keys_rev, keys_len, ref, state <- require_map(this, state)
  let map_key = value.js_to_map_key(key_arg)
  case dict.has_key(entries, map_key) {
    False -> #(state, Ok(JsBool(False)))
    True -> {
      let entries = dict.delete(entries, map_key)
      // Leave keys_rev untouched — tombstone skipped at iteration time.
      let heap = update_map_data(state.heap, ref, entries, keys_rev, keys_len)
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
fn map_clear(this: JsValue, state: State) -> #(State, Result(JsValue, JsValue)) {
  use _entries, _keys_rev, _keys_len, ref, state <- require_map(this, state)
  let heap = update_map_data(state.heap, ref, dict.new(), [], 0)
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
fn map_for_each(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Extract callbackfn and thisArg
  let #(cb, this_arg) = case args {
    [c, t, ..] -> #(c, t)
    [c] -> #(c, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }

  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  case helpers.is_callable(state.heap, cb) {
    False ->
      state.type_error(
        state,
        common.typeof_value(cb, state.heap) <> " is not a function",
      )
    True -> {
      // Steps 1-2: RequireInternalSlot
      use entries, keys_rev, _keys_len, _ref, state <- require_map(this, state)
      // Steps 4-5: Iterate in insertion order. keys_rev is reversed and may
      // contain tombstones + duplicates from delete+re-add cycles. Dedup by
      // folding newest-first with a seen-set and prepending — result is
      // forward-ordered with each key at its LATEST insertion position.
      let ordered = iteration_order(keys_rev, entries)
      for_each_loop(state, entries, ordered, cb, this_arg, this)
    }
  }
}

/// Inner loop for Map.prototype.forEach — iterates keys in insertion order.
fn for_each_loop(
  state: State,
  entries: dict.Dict(MapKey, JsValue),
  keys: List(MapKey),
  cb: JsValue,
  this_arg: JsValue,
  map_this: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  case keys {
    [] -> #(state, Ok(JsUndefined))
    [map_key, ..rest] ->
      case dict.get(entries, map_key) {
        Error(Nil) ->
          for_each_loop(state, entries, rest, cb, this_arg, map_this)
        Ok(val) -> {
          // Reconstruct original JS key. map_key_to_js is lossless (-0 already
          // normalized to +0 per spec §24.1.3.9 step 4).
          let original_key = value.map_key_to_js(map_key)
          // Step 5a.i: Call(callbackfn, thisArg, « e.[[Value]], e.[[Key]], M »)
          use _result, state <- state.try_call(state, cb, this_arg, [
            val,
            original_key,
            map_this,
          ])
          for_each_loop(state, entries, rest, cb, this_arg, map_this)
        }
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use entries, _keys_rev, _keys_len, _ref, state <- require_map(this, state)
  let size = dict.size(entries)
  #(state, Ok(value.from_int(size)))
}

// ============================================================================
// Map.prototype.keys() / values() / entries() — ES2024 §24.1.3.8/11/4
// ============================================================================

/// CreateMapIterator (§24.1.5.1) — snapshot forward-order (key,value) pairs
/// and wrap in a MapIteratorObject. The iterator's `kind` controls what
/// .next() yields (key only / value only / [key,value] array).
fn map_iterator(
  this: JsValue,
  state: State,
  kind: value.MapIterKind,
) -> #(State, Result(JsValue, JsValue)) {
  use entries, keys_rev, _len, _ref, state <- require_map(this, state)
  let snapshot =
    iteration_order(keys_rev, entries)
    |> list.filter_map(fn(k) {
      dict.get(entries, k) |> result.map(fn(v) { #(value.map_key_to_js(k), v) })
    })
  let #(heap, ref) =
    common.alloc_wrapper(
      state.heap,
      value.MapIteratorObject(remaining: snapshot, kind:),
      state.builtins.map_iterator_proto,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

// ============================================================================
// Helpers
// ============================================================================

/// RequireInternalSlot(M, [[MapData]]) — validates that `this` is a Map object
/// and extracts its internal data.
///
/// Calls `cont` with the entries dict, reversed keys list, tracked length,
/// heap ref, and state. Returns TypeError if `this` is not a Map.
fn require_map(
  this: JsValue,
  state: State,
  cont: fn(dict.Dict(MapKey, JsValue), List(MapKey), Int, Ref, State) ->
    #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: MapObject(entries:, keys_rev:, keys_len:), ..)) ->
          cont(entries, keys_rev, keys_len, ref, state)
        _ ->
          state.type_error(
            state,
            "Method Map.prototype.* called on incompatible receiver",
          )
      }
    _ ->
      state.type_error(
        state,
        "Method Map.prototype.* called on incompatible receiver",
      )
  }
}

/// Update the MapObject data on an existing heap slot.
fn update_map_data(
  h: Heap,
  ref: Ref,
  entries: dict.Dict(MapKey, JsValue),
  keys_rev: List(MapKey),
  keys_len: Int,
) -> Heap {
  heap.update_kind(h, ref, MapObject(entries:, keys_rev:, keys_len:))
}

/// Derive forward iteration order from a reversed, possibly-dirty key list.
/// Walks keys_rev newest-first, prepending live unseen keys to the accumulator.
/// Result is forward-ordered with each key at its most recent insertion
/// position (re-added-after-delete keys appear at their new position, not the
/// stale tombstone position).
fn iteration_order(
  keys_rev: List(MapKey),
  entries: dict.Dict(MapKey, JsValue),
) -> List(MapKey) {
  let #(ordered, _seen) =
    list.fold(keys_rev, #([], set.new()), fn(acc, k) {
      let #(ks, seen) = acc
      case set.contains(seen, k) {
        True -> acc
        False ->
          case dict.has_key(entries, k) {
            False -> acc
            True -> #([k, ..ks], set.insert(seen, k))
          }
      }
    })
  ordered
}

/// Rebuild keys_rev dropping tombstones and duplicates. Same dedup walk as
/// iteration_order but re-reverses the result so it stays in reversed storage
/// order. Called when keys_len exceeds 2× live size.
fn compact_keys(
  keys_rev: List(MapKey),
  entries: dict.Dict(MapKey, JsValue),
) -> List(MapKey) {
  iteration_order(keys_rev, entries) |> list.reverse
}
