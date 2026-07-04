/// ES2024 §24.2 Set Objects
///
/// A Set is a collection of unique values. Key equality follows the
/// SameValueZero algorithm (NaN === NaN, +0 === -0).
///
/// Stores values in an `OrderedEntries(MapKey, JsValue)` store (see
/// `arc/vm/internal/ordered_entries`) mapping normalized MapKey → original
/// JsValue, which also models the spec's append-only [[SetData]] insertion
/// order. delete() removes the record; the seq gap is the spec's emptied
/// record, so a deleted-then-re-added value gets a fresh seq and is revisited
/// by in-flight iterators per §24.2.5. Iteration points call
/// ordered_entries.live_values to recover forward insertion order.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined}
import arc/vm/builtins/iter_protocol
import arc/vm/heap
import arc/vm/internal/ordered_entries.{type OrderedEntries}
import arc/vm/key.{Named}
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type MapKey, type Ref, type SetNativeFn, Dispatch, Finite,
  JsBool, JsNumber, JsObject, JsUndefined, NaN, ObjectSlot, SetConstructor,
  SetNative, SetObject, SetPrototypeAdd, SetPrototypeClear, SetPrototypeDelete,
  SetPrototypeDifference, SetPrototypeEntries, SetPrototypeForEach,
  SetPrototypeGetSize, SetPrototypeHas, SetPrototypeIntersection,
  SetPrototypeIsDisjointFrom, SetPrototypeIsSubsetOf, SetPrototypeIsSupersetOf,
  SetPrototypeSymmetricDifference, SetPrototypeUnion, SetPrototypeValues,
}
import gleam/option.{type Option, None, Some}

/// Set up Set.prototype and Set constructor.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  // §24.2.3.12 Set.prototype.values doubles as §24.2.3.11 keys and
  // §24.2.3.13 [@@iterator]; §24.2.3.16 [@@toStringTag] = "Set".
  common.init_keyed_collection(
    h,
    object_proto,
    function_proto,
    [
      #("add", SetNative(SetPrototypeAdd), 1),
      #("has", SetNative(SetPrototypeHas), 1),
      #("delete", SetNative(SetPrototypeDelete), 1),
      #("clear", SetNative(SetPrototypeClear), 0),
      #("forEach", SetNative(SetPrototypeForEach), 1),
      #("union", SetNative(SetPrototypeUnion), 1),
      #("intersection", SetNative(SetPrototypeIntersection), 1),
      #("difference", SetNative(SetPrototypeDifference), 1),
      #("symmetricDifference", SetNative(SetPrototypeSymmetricDifference), 1),
      #("isSubsetOf", SetNative(SetPrototypeIsSubsetOf), 1),
      #("isSupersetOf", SetNative(SetPrototypeIsSupersetOf), 1),
      #("isDisjointFrom", SetNative(SetPrototypeIsDisjointFrom), 1),
      #("entries", SetNative(SetPrototypeEntries), 0),
    ],
    "values",
    SetNative(SetPrototypeValues),
    ["keys"],
    SetNative(SetPrototypeGetSize),
    fn(proto) { Dispatch(SetNative(SetConstructor(proto:))) },
    "Set",
  )
}

/// Per-module dispatch for Set native functions.
pub fn dispatch(
  native: SetNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    SetConstructor(proto:) -> construct(proto, args, state)
    SetPrototypeAdd -> set_add(this, args, state)
    SetPrototypeHas -> set_has(this, args, state)
    SetPrototypeDelete -> set_delete(this, args, state)
    SetPrototypeClear -> set_clear(this, state)
    SetPrototypeForEach -> set_for_each(this, args, state)
    SetPrototypeGetSize -> set_size(this, state)
    SetPrototypeUnion -> set_union(this, args, state)
    SetPrototypeIntersection -> set_intersection(this, args, state)
    SetPrototypeDifference -> set_difference(this, args, state)
    SetPrototypeSymmetricDifference ->
      set_symmetric_difference(this, args, state)
    SetPrototypeIsSubsetOf -> set_is_subset_of(this, args, state)
    SetPrototypeIsSupersetOf -> set_is_superset_of(this, args, state)
    SetPrototypeIsDisjointFrom -> set_is_disjoint_from(this, args, state)
    SetPrototypeValues -> set_values(this, state)
    SetPrototypeEntries -> set_entries(this, state)
  }
}

/// ES2024 §24.2.1.1 Set ( [ iterable ] )
///
///   1. If NewTarget is undefined, throw a TypeError exception.
///   2. Let set be ? OrdinaryCreateFromConstructor(NewTarget,
///      "%Set.prototype%", « [[SetData]] »).
///   4. Set set.[[SetData]] to a new empty List.
///   5. If iterable is either undefined or null, return set.
///   6. Let adder be ? Get(set, "add").
///   7. If IsCallable(adder) is false, throw a TypeError exception.
///   8. Repeat: IteratorStepValue, then ? Call(adder, set, « nextValue »),
///      closing the iterator if the adder throws.
///
/// The iterable goes through the real GetIterator protocol, so
/// `new Set(otherSet)`, `new Set(map)`, generators and strings all work,
/// and the (user-overridable) `add` is observably called per value.
fn construct(
  proto: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: reject a plain call and resolve new.target.prototype.
  use proto, state <- helpers.require_new_target(state, "Set", proto)
  // Steps 2-4: allocate the set with an empty [[SetData]].
  let #(heap, set_ref) =
    common.alloc_wrapper(
      state.heap,
      SetObject(store: ordered_entries.new()),
      proto,
    )
  let state = State(..state, heap:)
  let set = JsObject(set_ref)
  case first_arg_or_undefined(args) {
    // Step 5: If iterable is undefined or null, return set.
    JsUndefined | value.JsNull -> #(state, Ok(set))
    iterable -> {
      // Steps 6-7: adder = ? Get(set, "add"); must be callable.
      use adder, state <- state.try_op(object.get_value_of(
        state,
        set,
        Named("add"),
      ))
      case helpers.is_callable(state.heap, adder) {
        False ->
          state.type_error(state, "'add' property of Set is not a function")
        // Step 8: iterate the iterable's values, calling adder(set, v) for
        // each one and closing the iterator on any abrupt completion.
        True ->
          iter_protocol.add_values_from_iterable(state, set, iterable, adder)
      }
    }
  }
}

/// Helper to update a SetObject's data on the heap.
fn update_set(
  h: Heap(host),
  ref: Ref,
  store: OrderedEntries(MapKey, JsValue),
) -> Heap(host) {
  heap.update_kind(h, ref, SetObject(store:))
}

/// Read `ref`'s [[SetData]] out of the heap. `ref` must have been proved a Set
/// by `require_set`, which is why this can't fail.
///
/// This is the ONLY way to get at a Set's store: `require_set` hands out a
/// bare `Ref`, never a snapshot. Every op re-reads at the exact spec point at
/// which the spec inspects [[SetData]], because the set-relation methods call
/// arbitrary user code (`other.size`, `other.has`, `other.keys`, `next`) that
/// can add to or delete from the receiver mid-operation.
fn read_set_store(h: Heap(host), ref: Ref) -> OrderedEntries(MapKey, JsValue) {
  case heap.read(h, ref) {
    Some(ObjectSlot(kind: SetObject(store:), ..)) -> store
    // Unreachable: a heap slot's kind never changes after allocation.
    _ -> ordered_entries.new()
  }
}

/// SetDataAppend — the ONE place a value enters a [[SetData]] store.
///
/// §24.2.3.1 step 3 requires -0 to be stored as +0 (`SetDataHas` compares with
/// SameValueZero, but the *stored* value is what iteration yields), so
/// `[...new Set([-0])][0]` must be `+0`. Normalizing here — rather than only
/// in the MapKey — is what makes that true for every insert path.
///
/// An existing value keeps its insertion position (seq); a new one — including
/// a deleted-then-re-added one — appends past every live iterator's cursor.
fn set_data_append(
  store: OrderedEntries(MapKey, JsValue),
  val: JsValue,
) -> OrderedEntries(MapKey, JsValue) {
  let val = normalize_neg_zero(val)
  ordered_entries.insert(store, value.js_to_map_key(val), val)
}

/// ES2024 §24.2.3.1 Set.prototype.add ( value )
fn set_add(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "add")
  let store = read_set_store(state.heap, ref)
  let store = set_data_append(store, first_arg_or_undefined(args))
  let heap = update_set(state.heap, ref, store)
  #(State(..state, heap:), Ok(this))
}

/// ES2024 §24.2.3.4 Set.prototype.has ( value )
fn set_has(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "has")
  let key = value.js_to_map_key(first_arg_or_undefined(args))
  #(
    state,
    Ok(JsBool(ordered_entries.has(read_set_store(state.heap, ref), key))),
  )
}

/// ES2024 §24.2.3.3 Set.prototype.delete ( value )
///
/// Removes the record entirely; the seq gap left in the store is the spec's
/// emptied record (skipped by iterator cursors in O(1)).
fn set_delete(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "delete")
  let store = read_set_store(state.heap, ref)
  let key = value.js_to_map_key(first_arg_or_undefined(args))
  case ordered_entries.delete(store, key) {
    #(_store, False) -> #(state, Ok(JsBool(False)))
    #(store, True) -> {
      let heap = update_set(state.heap, ref, store)
      #(State(..state, heap:), Ok(JsBool(True)))
    }
  }
}

/// ES2024 §24.2.3.2 Set.prototype.clear ()
fn set_clear(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "clear")
  let store = read_set_store(state.heap, ref)
  // next_seq is preserved by clear(): the spec's records are emptied but
  // appends still land past in-flight iterator cursors, so they remain
  // visited.
  let heap = update_set(state.heap, ref, ordered_entries.clear(store))
  #(State(..state, heap:), Ok(JsUndefined))
}

/// ES2024 §24.2.3.5 get Set.prototype.size
fn set_size(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "size")
  let store = read_set_store(state.heap, ref)
  #(state, Ok(value.from_int(ordered_entries.size(store))))
}

/// ES2024 §24.2.3.6 Set.prototype.forEach ( callbackfn [ , thisArg ] )
fn set_for_each(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "forEach")
  let callback = first_arg_or_undefined(args)
  let this_arg = case args {
    [_, ta, ..] -> ta
    _ -> JsUndefined
  }
  case helpers.is_callable(state.heap, callback) {
    False ->
      state.type_error(
        state,
        "Set.prototype.forEach callback is not a function",
      )
    True -> for_each_loop(state, ref, 0, callback, this_arg, this)
  }
}

/// Iterate over Set entries, calling callback(value, value, set) for each.
/// LIVE iteration by seq cursor — the source is re-read from the heap each
/// step, so entries the callback deletes before being reached are skipped
/// and entries it adds (including delete + re-add) are visited.
fn for_each_loop(
  state: State(host),
  ref: Ref,
  cursor: Int,
  callback: JsValue,
  this_arg: JsValue,
  set_this: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  let store = read_set_store(state.heap, ref)
  case ordered_entries.next_from(store, cursor) {
    None -> #(state, Ok(JsUndefined))
    Some(#(next_cursor, _key, val)) ->
      case state.call(state, callback, this_arg, [val, val, set_this]) {
        Ok(#(_result, new_state)) ->
          for_each_loop(
            new_state,
            ref,
            next_cursor,
            callback,
            this_arg,
            set_this,
          )
        Error(#(thrown, new_state)) -> #(new_state, Error(thrown))
      }
  }
}

/// ES2025 §24.2.3.14 Set.prototype.union ( other )
///
/// Steps 3-5: GetSetRecord and GetKeysIterator both run user code, so the copy
/// of this's [[SetData]] (step 5) is taken *after* them, not at method entry.
fn set_union(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "union")
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  use iter, next_fn, state <- with_keys_iterator(state, rec)
  union_loop(state, iter, next_fn, read_set_store(state.heap, ref))
}

/// §24.2.3.14 steps 6-7: append every key other yields that isn't already in
/// the accumulating result. `this` is never re-read — the spec froze it into
/// resultSetData at step 5.
fn union_loop(
  state: State(host),
  iter: JsValue,
  next_fn: JsValue,
  result: OrderedEntries(MapKey, JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  use next, state <- step_keys(state, iter, next_fn)
  case next {
    None -> alloc_new_set(state, result)
    Some(v) -> union_loop(state, iter, next_fn, set_data_append(result, v))
  }
}

/// ES2025 §24.2.3.7 Set.prototype.intersection ( other )
///
/// Step 4 dispatches on size: with a small `this` we ask other.has() once per
/// element of this; with a large `this` (a huge `other` would mean a huge
/// number of has() calls) we drain other's keys instead and test membership
/// against this. Getting this branch wrong is not just slow — the two paths
/// call *different* user code, which is observable.
fn set_intersection(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "intersection")
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  case ordered_entries.size(read_set_store(state.heap, ref)) <= rec.size {
    True -> intersection_this_loop(state, ref, rec, 0, ordered_entries.new())
    False -> {
      use iter, next_fn, state <- with_keys_iterator(state, rec)
      intersection_other_loop(state, ref, iter, next_fn, ordered_entries.new())
    }
  }
}

/// §24.2.3.7 step 4.b: walk this's records by seq, keeping the ones other.has().
/// Each `has()` may add to or delete from this, so the store is re-read every
/// step (the spec re-reads `O.[[SetData]][index]` and refreshes `thisSize`).
fn intersection_this_loop(
  state: State(host),
  ref: Ref,
  rec: SetRecord,
  cursor: Int,
  result: OrderedEntries(MapKey, JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  let store = read_set_store(state.heap, ref)
  case ordered_entries.next_from(store, cursor) {
    None -> alloc_new_set(state, result)
    Some(#(next_cursor, _key, e)) -> {
      use in_other, state <- set_record_has(state, rec, e)
      let result = case in_other {
        True -> set_data_append(result, e)
        False -> result
      }
      intersection_this_loop(state, ref, rec, next_cursor, result)
    }
  }
}

/// §24.2.3.7 step 5: drain other's keys, keeping the ones this still contains.
/// `next()` is user code, so this's store is re-read on every step.
fn intersection_other_loop(
  state: State(host),
  ref: Ref,
  iter: JsValue,
  next_fn: JsValue,
  result: OrderedEntries(MapKey, JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  use next, state <- step_keys(state, iter, next_fn)
  case next {
    None -> alloc_new_set(state, result)
    Some(v) -> {
      let store = read_set_store(state.heap, ref)
      let result = case ordered_entries.has(store, value.js_to_map_key(v)) {
        True -> set_data_append(result, v)
        False -> result
      }
      intersection_other_loop(state, ref, iter, next_fn, result)
    }
  }
}

/// ES2025 §24.2.3.5 Set.prototype.difference ( other )
///
/// Step 3 copies this's [[SetData]] into the result (after GetSetRecord ran
/// user code); step 4 then *removes* from that copy. Same size dispatch as
/// intersection.
fn set_difference(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "difference")
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  let result = read_set_store(state.heap, ref)
  case ordered_entries.size(result) <= rec.size {
    // Step 4.b iterates the *copy*, not this — a re-entrant has() that mutates
    // this cannot change which elements are visited.
    True ->
      difference_this_loop(
        state,
        rec,
        ordered_entries.live_values(result),
        result,
      )
    False -> {
      use iter, next_fn, state <- with_keys_iterator(state, rec)
      difference_other_loop(state, iter, next_fn, result)
    }
  }
}

fn difference_this_loop(
  state: State(host),
  rec: SetRecord,
  remaining: List(JsValue),
  result: OrderedEntries(MapKey, JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  case remaining {
    [] -> alloc_new_set(state, result)
    [e, ..rest] -> {
      use in_other, state <- set_record_has(state, rec, e)
      let result = case in_other {
        True -> ordered_entries.delete(result, value.js_to_map_key(e)).0
        False -> result
      }
      difference_this_loop(state, rec, rest, result)
    }
  }
}

fn difference_other_loop(
  state: State(host),
  iter: JsValue,
  next_fn: JsValue,
  result: OrderedEntries(MapKey, JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  use next, state <- step_keys(state, iter, next_fn)
  case next {
    None -> alloc_new_set(state, result)
    Some(v) -> {
      let result = ordered_entries.delete(result, value.js_to_map_key(v)).0
      difference_other_loop(state, iter, next_fn, result)
    }
  }
}

/// ES2025 §24.2.3.13 Set.prototype.symmetricDifference ( other )
///
/// Steps 3-4: keys iterator first, then copy this → resultSetData. Then drain
/// other.keys(); for each nextValue, if it's in this remove it from result,
/// else add it. (Membership is tested against `this`, not the mutating result,
/// so an element other yields twice doesn't toggle back in.)
fn set_symmetric_difference(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "symmetricDifference")
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  use iter, next_fn, state <- with_keys_iterator(state, rec)
  symmetric_difference_loop(
    state,
    ref,
    iter,
    next_fn,
    read_set_store(state.heap, ref),
  )
}

fn symmetric_difference_loop(
  state: State(host),
  ref: Ref,
  iter: JsValue,
  next_fn: JsValue,
  result: OrderedEntries(MapKey, JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  use next, state <- step_keys(state, iter, next_fn)
  case next {
    None -> alloc_new_set(state, result)
    Some(v) -> {
      let key = value.js_to_map_key(v)
      // Step 5.b.iii is a LIVE read of this's [[SetData]] — other's next()
      // may have mutated it since the copy was taken.
      let in_this = ordered_entries.has(read_set_store(state.heap, ref), key)
      let result = case in_this {
        // Already gone if other yielded it twice; removal is a no-op then.
        True -> ordered_entries.delete(result, key).0
        False -> set_data_append(result, v)
      }
      symmetric_difference_loop(state, ref, iter, next_fn, result)
    }
  }
}

/// ES2025 §24.2.3.9 Set.prototype.isSubsetOf ( other )
fn set_is_subset_of(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "isSubsetOf")
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  // Step 4: if thisSize > otherRec.size, return false
  case ordered_entries.size(read_set_store(state.heap, ref)) > rec.size {
    True -> #(state, Ok(JsBool(False)))
    False -> this_step_loop(state, ref, rec, 0, False)
  }
}

/// Walk this's live records, calling other.has() on each: answer false as soon
/// as one has() result equals `false_when`, true if none does. Shared by
/// isSubsetOf (§24.2.3.9 steps 5-7, false_when: False — every element must be
/// in other) and isDisjointFrom (§24.2.3.8 step 4.b, false_when: True — no
/// element may be). Each has() is user code, so this's store is re-read every
/// step. There is no iterator to close on the short-circuit exit.
fn this_step_loop(
  state: State(host),
  ref: Ref,
  rec: SetRecord,
  cursor: Int,
  false_when false_when: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  let store = read_set_store(state.heap, ref)
  case ordered_entries.next_from(store, cursor) {
    None -> #(state, Ok(JsBool(True)))
    Some(#(next_cursor, _key, e)) -> {
      use in_other, state <- set_record_has(state, rec, e)
      case in_other == false_when {
        True -> #(state, Ok(JsBool(False)))
        False -> this_step_loop(state, ref, rec, next_cursor, false_when)
      }
    }
  }
}

/// ES2025 §24.2.3.10 Set.prototype.isSupersetOf ( other )
fn set_is_superset_of(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "isSupersetOf")
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  // Step 4: if thisSize < otherRec.size, return false
  case ordered_entries.size(read_set_store(state.heap, ref)) < rec.size {
    True -> #(state, Ok(JsBool(False)))
    False -> {
      // Steps 5-8: step other's keys iterator one value at a time.
      // Draining the whole iterator first would never terminate on an
      // infinite iterator whose first value is already a non-member.
      use iter, next_fn, state <- with_keys_iterator(state, rec)
      other_step_loop(state, ref, iter, next_fn, False)
    }
  }
}

/// Step other's keys iterator, testing each key against this: return true once
/// the iterator is exhausted; on the FIRST key whose membership in this equals
/// `false_when`, close the iterator and return false. Shared by isSupersetOf
/// (§24.2.3.10 steps 7-8, false_when: False — every yielded key must be in
/// this) and isDisjointFrom (§24.2.3.8 step 5, false_when: True — none may be).
/// `next()` is user code, so this's store is re-read every step.
fn other_step_loop(
  state: State(host),
  ref: Ref,
  iter: JsValue,
  next_fn: JsValue,
  false_when false_when: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use next, state <- step_keys(state, iter, next_fn)
  case next {
    None -> #(state, Ok(JsBool(True)))
    Some(v) -> {
      let store = read_set_store(state.heap, ref)
      case ordered_entries.has(store, value.js_to_map_key(v)) == false_when {
        True -> close_keys_iterator_and_answer_false(state, iter)
        False -> other_step_loop(state, ref, iter, next_fn, false_when)
      }
    }
  }
}

/// ES2025 §24.2.3.8 Set.prototype.isDisjointFrom ( other )
///
/// Same size dispatch as intersection: only ask other.has() per element of
/// this when this is the smaller side; otherwise drain other's keys.
fn set_is_disjoint_from(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "isDisjointFrom")
  use rec, state <- get_set_record(first_arg_or_undefined(args), state)
  case ordered_entries.size(read_set_store(state.heap, ref)) <= rec.size {
    True -> this_step_loop(state, ref, rec, 0, True)
    False -> {
      use iter, next_fn, state <- with_keys_iterator(state, rec)
      other_step_loop(state, ref, iter, next_fn, True)
    }
  }
}

/// Short-circuit exit shared by isSupersetOf/isDisjointFrom: close other's
/// keys iterator with a normal completion, then answer false (unless the
/// close itself threw).
fn close_keys_iterator_and_answer_false(
  state: State(host),
  iter: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  case iter_protocol.iterator_close_normal(state, iter) {
    #(state, Ok(Nil)) -> #(state, Ok(JsBool(False)))
    #(state, Error(thrown)) -> #(state, Error(thrown))
  }
}

/// ES2024 §24.2.3.12 Set.prototype.values ()
/// Returns a new Set Iterator object (§24.2.5.1 CreateSetIterator).
fn set_values(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "values")
  alloc_set_iterator(state, ref, value.SetIterValues)
}

/// ES2024 §24.2.3.5 Set.prototype.entries ()
fn set_entries(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_set(this, state, "entries")
  alloc_set_iterator(state, ref, value.SetIterEntries)
}

/// Allocate a LIVE SetIteratorObject over the source Set (§24.2.5.1) —
/// entries added during iteration are visited.
fn alloc_set_iterator(
  state: State(host),
  source: Ref,
  kind: value.SetIterKind,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(heap, ref) =
    common.alloc_wrapper(
      state.heap,
      value.SetIteratorObject(source:, cursor: 0, done: False, kind:),
      state.builtins.set_iterator_proto,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Allocate a new Set object from an ordered-entries store.
fn alloc_new_set(
  state: State(host),
  store: OrderedEntries(MapKey, JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(heap, ref) =
    common.alloc_wrapper(
      state.heap,
      SetObject(store:),
      state.builtins.set.prototype,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

// ---- GetSetRecord + protocol helpers ----

/// Spec's "Set Record" — captured size/has/keys from the other argument.
/// `size` is the post-ToIntegerOrInfinity integer (+∞ saturated to 2^53 - 1).
type SetRecord {
  SetRecord(obj: JsValue, size: Int, has: JsValue, keys: JsValue)
}

/// ES2025 §24.2.1.2 GetSetRecord ( obj )
///
/// Validates `other` is set-like: reads .size (ToNumber → NaN check →
/// ToIntegerOrInfinity → negative check), then .has and .keys (both must be
/// callable). CPS-style — call with `use rec, state <- get_set_record(other, state)`.
fn get_set_record(
  other: JsValue,
  state: State(host),
  cont: fn(SetRecord, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case other {
    JsObject(ref) -> {
      // Step 2: rawSize = Get(obj, "size")
      use raw_size, state <- state.try_op(object.get_value(
        state,
        ref,
        Named("size"),
        other,
      ))
      // Step 3: numSize = ? ToNumber(rawSize) — the real §7.1.4, so
      // {valueOf(){...}} works and Symbol/BigInt throw a TypeError.
      use num, state <- coerce.try_to_number(state, raw_size)
      // Step 4: if numSize is NaN, throw TypeError
      case num {
        NaN -> state.type_error(state, "size is NaN")
        num -> {
          // Step 5: ToIntegerOrInfinity(numSize). ±∞ saturate to
          // ±(2^53 - 1), so -∞ still trips step 6's `< 0` RangeError and
          // +∞ still exceeds every real [[SetData]] size.
          let int_size = coerce.jsnum_to_integer_or_infinity(num)
          // Step 6: if intSize < 0, throw RangeError
          case int_size < 0 {
            True -> state.range_error(state, "size is negative")
            False -> {
              // Step 7-8: has = Get(obj, "has"); IsCallable check
              use has, state <- state.try_op(object.get_value(
                state,
                ref,
                Named("has"),
                other,
              ))
              case helpers.is_callable(state.heap, has) {
                False -> state.type_error(state, "has is not a function")
                True -> {
                  // Step 9-10: keys = Get(obj, "keys"); IsCallable check
                  use keys, state <- state.try_op(object.get_value(
                    state,
                    ref,
                    Named("keys"),
                    other,
                  ))
                  case helpers.is_callable(state.heap, keys) {
                    False -> state.type_error(state, "keys is not a function")
                    True ->
                      cont(
                        SetRecord(obj: other, size: int_size, has:, keys:),
                        state,
                      )
                  }
                }
              }
            }
          }
        }
      }
    }
    _ -> state.type_error(state, "other is not an object")
  }
}

/// §24.2.1.3 GetKeysIterator(rec): call rec.keys(), resolve the returned
/// iterator's .next method, and hand both to the continuation.
/// CPS wrapper — `use iter, next_fn, state <- with_keys_iterator(state, rec)`.
fn with_keys_iterator(
  state: State(host),
  rec: SetRecord,
  cont: fn(JsValue, JsValue, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use iter, state <- state.try_call(state, rec.keys, rec.obj, [])
  case iter {
    JsObject(iter_ref) -> {
      use next_fn, state <- state.try_op(object.get_value(
        state,
        iter_ref,
        Named("next"),
        iter,
      ))
      case helpers.is_callable(state.heap, next_fn) {
        False -> state.type_error(state, "iterator.next is not a function")
        True -> cont(iter, next_fn, state)
      }
    }
    _ -> state.type_error(state, "keys() did not return an object")
  }
}

/// One §7.4.8 IteratorStepValue on a keys iterator: `None` = done,
/// `Some(v)` = the next value (with -0 normalized to +0 per §24.2.1.2
/// step 7.b.ii). CPS — `use next, state <- step_keys(state, iter, next_fn)`.
fn step_keys(
  state: State(host),
  iter: JsValue,
  next_fn: JsValue,
  cont: fn(Option(JsValue), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use result_obj, state <- state.try_call(state, next_fn, iter, [])
  case result_obj {
    JsObject(rref) -> {
      use done, state <- state.try_op(object.get_value(
        state,
        rref,
        Named("done"),
        result_obj,
      ))
      case value.is_truthy(done) {
        True -> cont(None, state)
        False -> {
          use v, state <- state.try_op(object.get_value(
            state,
            rref,
            Named("value"),
            result_obj,
          ))
          cont(Some(normalize_neg_zero(v)), state)
        }
      }
    }
    _ -> state.type_error(state, "iterator result is not an object")
  }
}

/// SetDataKeyToValue helper — -0 normalizes to +0 (SameValueZero semantics).
/// IEEE 754: -0.0 +. 0.0 == +0.0; identity for all other floats.
fn normalize_neg_zero(v: JsValue) -> JsValue {
  case v {
    JsNumber(Finite(f)) -> JsNumber(Finite(f +. 0.0))
    other -> other
  }
}

/// Call rec.has(v), ToBoolean the result.
fn set_record_has(
  state: State(host),
  rec: SetRecord,
  v: JsValue,
  cont: fn(Bool, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use r, state <- state.try_call(state, rec.has, rec.obj, [v])
  cont(value.is_truthy(r), state)
}

// ---- helpers ----

/// RequireInternalSlot(this, [[SetData]]) — proves `this` is a Set, or throws
/// a TypeError naming `method`.
///
/// Deliberately hands the continuation only a `Ref`, never the [[SetData]]
/// store: a store read at method entry is stale the moment any user code runs,
/// and every set-relation method runs user code. Read the store with
/// `read_set_store` at the point the spec reads it.
///
/// CPS-style — call with `use ref, state <- require_set(this, state, "add")`.
fn require_set(
  this: JsValue,
  state: State(host),
  method: String,
  cont: fn(Ref, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use Nil, ref, state <- helpers.require_brand(
    this,
    state,
    fn() {
      "Method Set.prototype." <> method <> " called on incompatible receiver"
    },
    set_brand_of,
  )
  cont(ref, state)
}

/// The [[SetData]] brand check handed to `require_brand` — a named function
/// (not an inline lambda) so the hot brand check builds no closure per call.
fn set_brand_of(kind: state.ExoticKind(host)) -> Option(Nil) {
  case kind {
    SetObject(..) -> Some(Nil)
    _ -> None
  }
}
