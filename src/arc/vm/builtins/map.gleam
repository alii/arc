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
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type MapKey, type MapNativeFn, type Ref, AccessorProperty,
  ArrayObject, JsBool, JsNumber, JsObject, JsUndefined, MapConstructor,
  MapNative, MapObject, MapPrototypeClear, MapPrototypeDelete,
  MapPrototypeForEach, MapPrototypeGet, MapPrototypeGetSize, MapPrototypeHas,
  MapPrototypeSet, ObjectSlot,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
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
  // Allocate prototype method function objects
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("get", MapNative(MapPrototypeGet), 1),
      #("set", MapNative(MapPrototypeSet), 2),
      #("has", MapNative(MapPrototypeHas), 1),
      #("delete", MapNative(MapPrototypeDelete), 1),
      #("clear", MapNative(MapPrototypeClear), 0),
      #("forEach", MapNative(MapPrototypeForEach), 1),
    ])

  // Allocate the size getter function
  let #(h, size_getter_ref) =
    common.alloc_native_fn(
      h,
      function_proto,
      MapNative(MapPrototypeGetSize),
      "get size",
      0,
    )

  // Add the size accessor property to proto_methods
  let proto_methods = [
    #(
      "size",
      AccessorProperty(
        get: Some(JsObject(size_getter_ref)),
        set: None,
        enumerable: False,
        configurable: True,
      ),
    ),
    ..proto_methods
  ]

  // Build the prototype + constructor using the standard init_type helper.
  // The constructor carries the proto ref so it can set [[Prototype]] on
  // new Map instances.
  common.init_type(
    h,
    object_proto,
    function_proto,
    proto_methods,
    fn(proto) { MapNative(MapConstructor(proto:)) },
    "Map",
    0,
    [],
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
  // Steps 2-3: Create empty Map object
  let empty_data = dict.new()
  let empty_keys = []
  let empty_originals = dict.new()

  // Step 4: If iterable is undefined or null, return empty map.
  case args {
    [] | [JsUndefined, ..] | [value.JsNull, ..] -> {
      let #(heap, ref) =
        alloc_map(state.heap, proto, empty_data, empty_keys, empty_originals)
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    [iterable, ..] -> {
      // Step 7: AddEntriesFromIterable (simplified — only array of arrays)
      add_entries_from_iterable(
        state,
        proto,
        iterable,
        empty_data,
        empty_keys,
        empty_originals,
      )
    }
  }
}

/// Allocate a Map object on the heap.
fn alloc_map(
  heap: Heap,
  proto: Ref,
  data: dict.Dict(MapKey, JsValue),
  keys: List(MapKey),
  original_keys: dict.Dict(MapKey, JsValue),
) -> #(Heap, Ref) {
  heap.alloc(
    heap,
    ObjectSlot(
      kind: MapObject(data:, keys:, original_keys:),
      properties: dict.new(),
      elements: js_elements.new(),
      prototype: Some(proto),
      symbol_properties: dict.new(),
      extensible: True,
    ),
  )
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
  data: dict.Dict(MapKey, JsValue),
  keys: List(MapKey),
  original_keys: dict.Dict(MapKey, JsValue),
) -> #(State, Result(JsValue, JsValue)) {
  case iterable {
    JsObject(iter_ref) ->
      case heap.read(state.heap, iter_ref) {
        Some(ObjectSlot(kind: ArrayObject(length:), elements:, ..)) -> {
          // Iterate through the array entries
          add_entries_loop(
            state,
            proto,
            elements,
            0,
            length,
            data,
            keys,
            original_keys,
          )
        }
        _ ->
          frame.type_error(state, "Iterator value is not an entry-like object")
      }
    _ -> frame.type_error(state, string.inspect(iterable) <> " is not iterable")
  }
}

/// Loop over array entries for Map constructor.
fn add_entries_loop(
  state: State,
  proto: Ref,
  elements: value.JsElements,
  idx: Int,
  length: Int,
  data: dict.Dict(MapKey, JsValue),
  keys: List(MapKey),
  original_keys: dict.Dict(MapKey, JsValue),
) -> #(State, Result(JsValue, JsValue)) {
  case idx >= length {
    True -> {
      // Done — allocate the map with all entries
      let #(heap, ref) =
        alloc_map(state.heap, proto, data, list.reverse(keys), original_keys)
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    False -> {
      let entry = js_elements.get(elements, idx)
      // Each entry must be an array-like [key, value]
      case entry {
        JsObject(entry_ref) ->
          case heap.read(state.heap, entry_ref) {
            Some(ObjectSlot(kind: ArrayObject(..), elements: entry_elems, ..)) -> {
              let key = js_elements.get(entry_elems, 0)
              let val = js_elements.get(entry_elems, 1)
              let map_key = value.js_to_map_key(key)
              // If key already exists, update it (last write wins, but keep
              // insertion order position of first occurrence per spec)
              let already_exists = dict.has_key(data, map_key)
              let data = dict.insert(data, map_key, val)
              let original_keys = dict.insert(original_keys, map_key, key)
              let keys = case already_exists {
                True -> keys
                False -> [map_key, ..keys]
              }
              add_entries_loop(
                state,
                proto,
                elements,
                idx + 1,
                length,
                data,
                keys,
                original_keys,
              )
            }
            _ ->
              frame.type_error(
                state,
                "Iterator value "
                  <> int.to_string(idx)
                  <> " is not an entry-like object",
              )
          }
        _ ->
          frame.type_error(
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
  use data, _keys, _original_keys, _ref, state <- require_map(this, state)
  // Steps 3-4: Look up key
  let map_key = value.js_to_map_key(key_arg)
  let result = case dict.get(data, map_key) {
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
///   4. Let p be the Record { [[Key]]: key, [[Value]]: value }.
///   5. Append p to M.[[MapData]].
///   6. Return M.
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
  use data, keys, original_keys, ref, state <- require_map(this, state)

  let map_key = value.js_to_map_key(key_arg)
  // Step 3: Check if key already exists
  let already_exists = dict.has_key(data, map_key)
  // Steps 3-5: Insert or update
  let new_data = dict.insert(data, map_key, val_arg)
  let new_original_keys = dict.insert(original_keys, map_key, key_arg)
  let new_keys = case already_exists {
    True -> keys
    False -> list.append(keys, [map_key])
  }

  // Write updated MapObject back to heap
  let heap =
    update_map_data(state.heap, ref, new_data, new_keys, new_original_keys)

  // Step 6: Return M
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
  use data, _keys, _original_keys, _ref, state <- require_map(this, state)
  let map_key = value.js_to_map_key(key_arg)
  #(state, Ok(JsBool(dict.has_key(data, map_key))))
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
fn map_delete(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let key_arg = case args {
    [k, ..] -> k
    [] -> JsUndefined
  }
  use data, keys, original_keys, ref, state <- require_map(this, state)
  let map_key = value.js_to_map_key(key_arg)
  case dict.has_key(data, map_key) {
    True -> {
      let new_data = dict.delete(data, map_key)
      let new_keys = list.filter(keys, fn(k) { k != map_key })
      let new_original_keys = dict.delete(original_keys, map_key)
      let heap =
        update_map_data(state.heap, ref, new_data, new_keys, new_original_keys)
      #(State(..state, heap:), Ok(JsBool(True)))
    }
    False -> #(state, Ok(JsBool(False)))
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
  use _data, _keys, _original_keys, ref, state <- require_map(this, state)
  let heap = update_map_data(state.heap, ref, dict.new(), [], dict.new())
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
      frame.type_error(
        state,
        common.typeof_value(cb, state.heap) <> " is not a function",
      )
    True -> {
      // Steps 1-2: RequireInternalSlot
      use data, keys, original_keys, _ref, state <- require_map(this, state)
      // Steps 4-5: Iterate entries in insertion order
      for_each_loop(state, data, keys, original_keys, cb, this_arg, this)
    }
  }
}

/// Inner loop for Map.prototype.forEach — iterates keys in insertion order.
fn for_each_loop(
  state: State,
  data: dict.Dict(MapKey, JsValue),
  keys: List(MapKey),
  original_keys: dict.Dict(MapKey, JsValue),
  cb: JsValue,
  this_arg: JsValue,
  map_this: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  case keys {
    [] -> #(state, Ok(JsUndefined))
    [map_key, ..rest] -> {
      // Skip deleted entries (key present in order list but no longer in data)
      case dict.get(data, map_key) {
        Error(Nil) ->
          for_each_loop(
            state,
            data,
            rest,
            original_keys,
            cb,
            this_arg,
            map_this,
          )
        Ok(val) -> {
          // Get original JS key value for the callback
          let original_key = case dict.get(original_keys, map_key) {
            Ok(k) -> k
            Error(Nil) -> JsUndefined
          }
          // Step 5a.i: Call(callbackfn, thisArg, « e.[[Value]], e.[[Key]], M »)
          case frame.call(state, cb, this_arg, [val, original_key, map_this]) {
            Error(#(thrown, state)) -> #(state, Error(thrown))
            Ok(#(_result, state)) ->
              for_each_loop(
                state,
                data,
                rest,
                original_keys,
                cb,
                this_arg,
                map_this,
              )
          }
        }
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
  use data, _keys, _original_keys, _ref, state <- require_map(this, state)
  let size = dict.size(data)
  #(state, Ok(JsNumber(value.Finite(int.to_float(size)))))
}

// ============================================================================
// Helpers
// ============================================================================

/// RequireInternalSlot(M, [[MapData]]) — validates that `this` is a Map object
/// and extracts its internal data.
///
/// Calls `cont` with the map data, keys list, original keys dict, heap ref,
/// and state. Returns TypeError if `this` is not a Map.
fn require_map(
  this: JsValue,
  state: State,
  cont: fn(
    dict.Dict(MapKey, JsValue),
    List(MapKey),
    dict.Dict(MapKey, JsValue),
    Ref,
    State,
  ) ->
    #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: MapObject(data:, keys:, original_keys:), ..)) ->
          cont(data, keys, original_keys, ref, state)
        _ ->
          frame.type_error(
            state,
            "Method Map.prototype.* called on incompatible receiver",
          )
      }
    _ ->
      frame.type_error(
        state,
        "Method Map.prototype.* called on incompatible receiver",
      )
  }
}

/// Update the MapObject data on an existing heap slot.
fn update_map_data(
  h: Heap,
  ref: Ref,
  data: dict.Dict(MapKey, JsValue),
  keys: List(MapKey),
  original_keys: dict.Dict(MapKey, JsValue),
) -> Heap {
  heap.update(h, ref, fn(slot) {
    case slot {
      ObjectSlot(
        properties:,
        elements:,
        prototype:,
        symbol_properties:,
        extensible:,
        ..,
      ) ->
        ObjectSlot(
          kind: MapObject(data:, keys:, original_keys:),
          properties:,
          elements:,
          prototype:,
          symbol_properties:,
          extensible:,
        )
      other -> other
    }
  })
}
