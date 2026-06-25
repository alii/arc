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
/// Storage: `Dict(MapKey, JsValue)` for O(log n) get/set/has/delete, plus the
/// spec's append-only [[MapData]] order modelled with monotonically
/// increasing sequence numbers (`seqs`: key → seq, `order`: seq → key,
/// `next_seq`). delete() removes the record; the seq gap is the spec's
/// emptied record, so a deleted-then-re-added key gets a fresh seq and is
/// revisited by in-flight iterators per §24.1.5. Original JS keys are
/// reconstructed via `map_key_to_js` — the MapKey encoding is lossless modulo
/// -0→+0 normalization, which the spec requires anyway (§24.1.3.9 step 4).
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/property
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
import gleam/option.{None, Some}
import gleam/result
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 4: If iterable is undefined or null, return empty map.
  case args {
    [] | [JsUndefined, ..] | [value.JsNull, ..] -> {
      let #(heap, ref) =
        alloc_map(state.heap, proto, dict.new(), dict.new(), dict.new(), 0)
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    [iterable, ..] ->
      // Step 7: AddEntriesFromIterable (simplified — only array of arrays)
      add_entries_from_iterable(
        state,
        proto,
        iterable,
        dict.new(),
        dict.new(),
        dict.new(),
        0,
      )
  }
}

/// Allocate a Map object on the heap.
fn alloc_map(
  heap: Heap(host),
  proto: Ref,
  entries: dict.Dict(MapKey, JsValue),
  seqs: dict.Dict(MapKey, Int),
  order: dict.Dict(Int, MapKey),
  next_seq: Int,
) -> #(Heap(host), Ref) {
  common.alloc_wrapper(
    heap,
    MapObject(entries:, seqs:, order:, next_seq:),
    proto,
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
  state: State(host),
  proto: Ref,
  iterable: JsValue,
  entries: dict.Dict(MapKey, JsValue),
  seqs: dict.Dict(MapKey, Int),
  order: dict.Dict(Int, MapKey),
  next_seq: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
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
            seqs,
            order,
            next_seq,
          )
        None ->
          state.type_error(state, "Iterator value is not an entry-like object")
      }
    _ -> state.type_error(state, string.inspect(iterable) <> " is not iterable")
  }
}

/// §24.1.1.2 steps 4.e-f: read an entry's [key, value]. Arrays use the fast
/// element store; any other object goes through real [[Get]]s of "0"/"1"
/// (which may run getters/proxy traps and therefore throw).
fn read_entry_kv(
  state: State(host),
  entry_ref: Ref,
) -> Result(#(JsValue, JsValue, State(host)), #(JsValue, State(host))) {
  case heap.read_array(state.heap, entry_ref) {
    Some(#(_, entry_elems)) ->
      Ok(#(elements.get(entry_elems, 0), elements.get(entry_elems, 1), state))
    None -> {
      use #(key, state) <- result.try(property.get_elem_value(
        state,
        entry_ref,
        value.JsString("0"),
      ))
      use #(val, state) <- result.map(property.get_elem_value(
        state,
        entry_ref,
        value.JsString("1"),
      ))
      #(key, val, state)
    }
  }
}

/// Loop over array entries for Map constructor.
fn add_entries_loop(
  state: State(host),
  proto: Ref,
  elements: value.JsElements,
  idx: Int,
  length: Int,
  entries: dict.Dict(MapKey, JsValue),
  seqs: dict.Dict(MapKey, Int),
  order: dict.Dict(Int, MapKey),
  next_seq: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  case idx >= length {
    True -> {
      // Done — allocate the map with all entries.
      let #(heap, ref) =
        alloc_map(state.heap, proto, entries, seqs, order, next_seq)
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    False -> {
      let entry = elements.get(elements, idx)
      // §24.1.1.2 AddEntriesFromIterable steps 4.c-f: any OBJECT is
      // entry-like — k/v come from Get(entry, "0") / Get(entry, "1").
      case entry {
        JsObject(entry_ref) ->
          case read_entry_kv(state, entry_ref) {
            Error(#(thrown, state)) -> #(state, Error(thrown))
            Ok(#(key, val, state)) -> {
              let map_key = value.js_to_map_key(key)
              // If key already exists, update value only (keep first-occurrence
              // insertion position per spec)
              let #(seqs, order, next_seq) = case
                dict.has_key(entries, map_key)
              {
                True -> #(seqs, order, next_seq)
                False -> #(
                  dict.insert(seqs, map_key, next_seq),
                  dict.insert(order, next_seq, map_key),
                  next_seq + 1,
                )
              }
              let entries = dict.insert(entries, map_key, val)
              add_entries_loop(
                state,
                proto,
                elements,
                idx + 1,
                length,
                entries,
                seqs,
                order,
                next_seq,
              )
            }
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let key_arg = helpers.first_arg_or_undefined(args)
  // Steps 1-2: RequireInternalSlot
  use entries, _seqs, _order, _next_seq, _ref, state <- require_map(this, state)
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(key_arg, val_arg) = case args {
    [k, v, ..] -> #(k, v)
    [k] -> #(k, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
  // Steps 1-2: RequireInternalSlot
  use entries, seqs, order, next_seq, ref, state <- require_map(this, state)

  // Step 4 (-0 → +0) happens inside js_to_map_key
  let map_key = value.js_to_map_key(key_arg)
  // Step 3: existing key keeps its insertion position (seq); a new key —
  // including a deleted-then-re-added one — appends at next_seq, past every
  // live iterator's cursor.
  let #(seqs, order, next_seq) = case dict.has_key(entries, map_key) {
    True -> #(seqs, order, next_seq)
    False -> #(
      dict.insert(seqs, map_key, next_seq),
      dict.insert(order, next_seq, map_key),
      next_seq + 1,
    )
  }
  let entries = dict.insert(entries, map_key, val_arg)

  // Write updated MapObject back to heap
  let heap = update_map_data(state.heap, ref, entries, seqs, order, next_seq)

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
  use entries, _seqs, _order, _next_seq, _ref, state <- require_map(this, state)
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
/// Removes the record entirely; the seq gap left in `order` is the spec's
/// emptied record (skipped by iterator cursors in O(1)).
fn map_delete(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let key_arg = helpers.first_arg_or_undefined(args)
  use entries, seqs, order, next_seq, ref, state <- require_map(this, state)
  let map_key = value.js_to_map_key(key_arg)
  case dict.get(seqs, map_key) {
    Error(Nil) -> #(state, Ok(JsBool(False)))
    Ok(seq) -> {
      let entries = dict.delete(entries, map_key)
      let seqs = dict.delete(seqs, map_key)
      let order = dict.delete(order, seq)
      let heap =
        update_map_data(state.heap, ref, entries, seqs, order, next_seq)
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
  use _entries, _seqs, _order, next_seq, ref, state <- require_map(this, state)
  // next_seq is preserved: clear() empties the spec's records but appends
  // still land past in-flight iterator cursors, so they remain visited.
  let heap =
    update_map_data(
      state.heap,
      ref,
      dict.new(),
      dict.new(),
      dict.new(),
      next_seq,
    )
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
      use _entries, _seqs, _order, _next_seq, ref, state <- require_map(
        this,
        state,
      )
      // Steps 4-5: LIVE iteration by seq cursor — the source is re-read from
      // the heap each step, so entries the callback deletes before being
      // reached are skipped and entries it adds (including delete + re-add)
      // are visited, per the spec's index-based [[MapData]] walk.
      for_each_loop(state, ref, 0, cb, this_arg, this)
    }
  }
}

/// Inner loop for Map.prototype.forEach — advances a seq cursor over the
/// source Map's live records, re-reading the source each step.
fn for_each_loop(
  state: State(host),
  ref: Ref,
  cursor: Int,
  cb: JsValue,
  this_arg: JsValue,
  map_this: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  let next = case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: MapObject(entries:, order:, next_seq:, ..), ..)) ->
      value.entry_from_seq(entries, order, cursor, next_seq)
    _ -> None
  }
  case next {
    None -> #(state, Ok(JsUndefined))
    Some(#(seq, map_key, val)) -> {
      // Reconstruct original JS key. map_key_to_js is lossless (-0 already
      // normalized to +0 per spec §24.1.3.9 step 4).
      let original_key = value.map_key_to_js(map_key)
      // Step 5a.i: Call(callbackfn, thisArg, « e.[[Value]], e.[[Key]], M »)
      use _result, state <- state.try_call(state, cb, this_arg, [
        val,
        original_key,
        map_this,
      ])
      for_each_loop(state, ref, seq + 1, cb, this_arg, map_this)
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
  use entries, _seqs, _order, _next_seq, _ref, state <- require_map(this, state)
  let size = dict.size(entries)
  #(state, Ok(value.from_int(size)))
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
  kind: value.MapIterKind,
) -> #(State(host), Result(JsValue, JsValue)) {
  use _entries, _seqs, _order, _next_seq, ref, state <- require_map(this, state)
  let #(heap, iter_ref) =
    common.alloc_wrapper(
      state.heap,
      value.MapIteratorObject(source: ref, cursor: 0, done: False, kind:),
      state.builtins.map_iterator_proto,
    )
  #(State(..state, heap:), Ok(JsObject(iter_ref)))
}

// ============================================================================
// Helpers
// ============================================================================

/// RequireInternalSlot(M, [[MapData]]) — validates that `this` is a Map object
/// and extracts its internal data.
///
/// Calls `cont` with the entries dict, the seqs/order dicts, the next seq
/// number, heap ref, and state. Returns TypeError if `this` is not a Map.
fn require_map(
  this: JsValue,
  state: State(host),
  cont: fn(
    dict.Dict(MapKey, JsValue),
    dict.Dict(MapKey, Int),
    dict.Dict(Int, MapKey),
    Int,
    Ref,
    State(host),
  ) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: MapObject(entries:, seqs:, order:, next_seq:), ..)) ->
          cont(entries, seqs, order, next_seq, ref, state)
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
  h: Heap(host),
  ref: Ref,
  entries: dict.Dict(MapKey, JsValue),
  seqs: dict.Dict(MapKey, Int),
  order: dict.Dict(Int, MapKey),
  next_seq: Int,
) -> Heap(host) {
  heap.update_kind(h, ref, MapObject(entries:, seqs:, order:, next_seq:))
}
