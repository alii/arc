//// ES2024 §24.2 Set Objects.
////
//// Stores values in an `OrderedEntries(MapKey, JsVal)` mapping normalized
//// MapKey → original JsVal, which also models the spec's append-only
//// [[SetData]] insertion order. delete() removes the record; the seq gap is
//// the spec's emptied record, so a deleted-then-re-added value is revisited
//// by in-flight iterators per §24.2.5.

import arc/internal/ordered_entries
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{first_arg_or_undefined}
import arc/rt/builtins/iter_protocol.{type IteratorRecord}
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type MapKey,
  type ObjKind, type SetIterKind, type SetNative, JFloat, JInt, JNan, KHandle,
  KNull, KNum, KUndef, Named, NoElements, SObject, SetAdd, SetClear,
  SetConstructor, SetDelete, SetDifference, SetEntries, SetForEach, SetGetSize,
  SetHas, SetIntersection, SetIsDisjointFrom, SetIsSubsetOf, SetIsSupersetOf,
  SetIterEntries, SetIterValues, SetIterator, SetN, SetObj,
  SetSymmetricDifference, SetUnion, SetValues, StringKey, classify,
  js_to_map_key, mk_bool, mk_number, mk_object, mk_undefined, symbol_iterator,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}

// ── init — Set constructor + Set.prototype ──────────────────────────────────

/// Set up %Set% and %Set.prototype%. §24.2.3.12 `values` doubles as
/// §24.2.3.11 `keys` and §24.2.3.13 [@@iterator]; §24.2.3.16 [@@toStringTag]
/// = "Set"; `size` is a get-only accessor.
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("add", SetN(SetAdd), 1),
      #("has", SetN(SetHas), 1),
      #("delete", SetN(SetDelete), 1),
      #("clear", SetN(SetClear), 0),
      #("forEach", SetN(SetForEach), 1),
      #("union", SetN(SetUnion), 1),
      #("intersection", SetN(SetIntersection), 1),
      #("difference", SetN(SetDifference), 1),
      #("symmetricDifference", SetN(SetSymmetricDifference), 1),
      #("isSubsetOf", SetN(SetIsSubsetOf), 1),
      #("isSupersetOf", SetN(SetIsSupersetOf), 1),
      #("isDisjointFrom", SetN(SetIsDisjointFrom), 1),
      #("entries", SetN(SetEntries), 0),
    ])
  // `values` allocated separately so `keys` and [@@iterator] alias the SAME
  // function object.
  let #(values_h, st) =
    common.alloc_rooted_native_fn(st, fn_proto, SetN(SetValues), "values", 0)
  let #(values_prop, st) = common.builtin_property(st, mk_object(values_h))
  let #(keys_prop, st) = common.restamp(st, values_prop)
  let #(size_props, st) =
    common.alloc_getters(st, fn_proto, [#("size", SetN(SetGetSize))])
  let proto_props =
    list.flatten([
      size_props,
      [#("values", values_prop), #("keys", keys_prop)],
      proto_methods,
    ])
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      proto_props,
      fn(proto) { SetN(SetConstructor(proto:)) },
      "Set",
      0,
      [],
    )
  let st = common.add_to_string_tag(st, bt.prototype, "Set")
  let #(iter_prop, st) = common.restamp(st, values_prop)
  let st =
    common.add_symbol_property(st, bt.prototype, symbol_iterator, iter_prop)
  #(bt, st)
}

// ── dispatch ────────────────────────────────────────────────────────────────

pub fn dispatch(
  st: Agent,
  n: SetNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case n {
    SetConstructor(..) ->
      rt_val.t_throw_type_error(st, "Constructor Set requires 'new'")
    SetAdd -> set_add(st, this, args)
    SetHas -> set_has(st, this, args)
    SetDelete -> set_delete(st, this, args)
    SetClear -> set_clear(st, this)
    SetForEach -> set_for_each(st, this, args)
    SetGetSize -> set_size(st, this)
    SetUnion -> set_union(st, this, args)
    SetIntersection -> set_intersection(st, this, args)
    SetDifference -> set_difference(st, this, args)
    SetSymmetricDifference -> set_symmetric_difference(st, this, args)
    SetIsSubsetOf -> set_is_subset_of(st, this, args)
    SetIsSupersetOf -> set_is_superset_of(st, this, args)
    SetIsDisjointFrom -> set_is_disjoint_from(st, this, args)
    SetValues -> set_values(st, this)
    SetEntries -> set_entries(st, this)
  }
}

pub fn dispatch_construct(
  st: Agent,
  n: SetNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case n {
    SetConstructor(..) -> set_constructor(st, args, new_target)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// ── §24.2.1.1 Set ( [ iterable ] ) ──────────────────────────────────────────

fn set_constructor(
  st: Agent,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let #(proto, st) =
    rt_call.get_prototype_from_constructor(st, new_target, fn(r) {
      r.set.prototype
    })
  let #(set_h, st) =
    alloc_kind_cell(st, SetObj(entries: ordered_entries.new()), proto)
  let set_v = mk_object(set_h)
  case classify(first_arg_or_undefined(args)) {
    KUndef | KNull -> #(set_h, st)
    _ -> {
      let iterable = first_arg_or_undefined(args)
      let #(adder, st) = rt_obj.t_get_prop(st, set_v, StringKey(Named("add")))
      case rt_call.is_callable(st, adder) {
        False ->
          rt_val.t_throw_type_error(
            st,
            "'add' property of Set is not a function",
          )
        True -> {
          let #(_set, st) =
            iter_protocol.add_values_from_iterable(st, set_v, iterable, adder)
          #(set_h, st)
        }
      }
    }
  }
}

// ── §24.2.3.1 add / §24.2.3.4 has / §24.2.3.3 delete / §24.2.3.2 clear ──────

fn set_add(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "add")
  let store = read_set_store(st, ref)
  let store = set_data_append(store, first_arg_or_undefined(args))
  #(this, update_set(st, ref, store))
}

fn set_has(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "has")
  let key = js_to_map_key(first_arg_or_undefined(args))
  #(mk_bool(ordered_entries.has(read_set_store(st, ref), key)), st)
}

fn set_delete(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "delete")
  let store = read_set_store(st, ref)
  let key = js_to_map_key(first_arg_or_undefined(args))
  case ordered_entries.delete(store, key) {
    #(_store, False) -> #(mk_bool(False), st)
    #(store, True) -> #(mk_bool(True), update_set(st, ref, store))
  }
}

fn set_clear(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "clear")
  let store = read_set_store(st, ref)
  #(mk_undefined(), update_set(st, ref, ordered_entries.clear(store)))
}

// ── §24.2.3.5 get size / §24.2.3.6 forEach ──────────────────────────────────

fn set_size(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "size")
  #(mk_number(JInt(ordered_entries.size(read_set_store(st, ref)))), st)
}

fn set_for_each(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "forEach")
  let #(cb, this_arg) = helpers.two_args_or_undefined(args)
  use cb <- helpers.require_callable(st, cb, fn() {
    "Set.prototype.forEach callback is not a function"
  })
  for_each_loop(st, ref, 0, cb, this_arg, this)
}

/// LIVE iteration by seq cursor — the source is re-read each step, so entries
/// the callback deletes before being reached are skipped and entries it adds
/// (including delete + re-add) are visited.
fn for_each_loop(
  st: Agent,
  ref: SetRef,
  cursor: Int,
  cb: JsVal,
  this_arg: JsVal,
  set_this: JsVal,
) -> #(JsVal, Agent) {
  let store = read_set_store(st, ref)
  case ordered_entries.next_from(store, cursor) {
    None -> #(mk_undefined(), st)
    Some(#(next_cursor, _key, val)) -> {
      let #(_r, st) =
        rt_call.t_call_checked(st, cb, this_arg, [val, val, set_this])
      for_each_loop(st, ref, next_cursor, cb, this_arg, set_this)
    }
  }
}

// ── §24.2.3.12/5 values() / entries() → CreateSetIterator ───────────────────

fn set_values(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "values")
  alloc_set_iterator(st, ref, SetIterValues)
}

fn set_entries(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "entries")
  alloc_set_iterator(st, ref, SetIterEntries)
}

fn alloc_set_iterator(
  st: Agent,
  source: SetRef,
  kind: SetIterKind,
) -> #(JsVal, Agent) {
  let #(iter_h, st) =
    alloc_kind_cell(
      st,
      SetIterator(target: set_ref_handle(source), index: 0, kind:),
      st.realm.set_iter_proto,
    )
  #(mk_object(iter_h), st)
}

// ── ES2025 §24.2.3.14 union / §24.2.3.7 intersection / §24.2.3.5 difference /
//    §24.2.3.13 symmetricDifference / §24.2.3.9 isSubsetOf /
//    §24.2.3.10 isSupersetOf / §24.2.3.8 isDisjointFrom ───────────────────────

fn set_union(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "union")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  let #(keys, st) = get_keys_iterator(st, rec)
  union_loop(st, keys, read_set_store(st, ref))
}

fn union_loop(
  st: Agent,
  keys: IteratorRecord,
  result: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(JsVal, Agent) {
  let #(next, st) = step_keys(st, keys)
  case next {
    None -> alloc_new_set(st, result)
    Some(v) -> union_loop(st, keys, set_data_append(result, v))
  }
}

fn set_intersection(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "intersection")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  case ordered_entries.size(read_set_store(st, ref)) <= rec.size {
    True -> intersection_this_loop(st, ref, rec, 0, ordered_entries.new())
    False -> {
      let #(keys, st) = get_keys_iterator(st, rec)
      intersection_other_loop(st, ref, keys, ordered_entries.new())
    }
  }
}

fn intersection_this_loop(
  st: Agent,
  ref: SetRef,
  rec: SetRecord,
  cursor: Int,
  result: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(JsVal, Agent) {
  let store = read_set_store(st, ref)
  case ordered_entries.next_from(store, cursor) {
    None -> alloc_new_set(st, result)
    Some(#(next_cursor, _key, e)) -> {
      let #(in_other, st) = set_record_has(st, rec, e)
      let result = case in_other {
        True -> set_data_append(result, e)
        False -> result
      }
      intersection_this_loop(st, ref, rec, next_cursor, result)
    }
  }
}

fn intersection_other_loop(
  st: Agent,
  ref: SetRef,
  keys: IteratorRecord,
  result: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(JsVal, Agent) {
  let #(next, st) = step_keys(st, keys)
  case next {
    None -> alloc_new_set(st, result)
    Some(v) -> {
      let store = read_set_store(st, ref)
      let result = case ordered_entries.has(store, js_to_map_key(v)) {
        True -> set_data_append(result, v)
        False -> result
      }
      intersection_other_loop(st, ref, keys, result)
    }
  }
}

fn set_difference(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "difference")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  let result = read_set_store(st, ref)
  case ordered_entries.size(result) <= rec.size {
    True ->
      difference_this_loop(st, rec, ordered_entries.live_values(result), result)
    False -> {
      let #(keys, st) = get_keys_iterator(st, rec)
      difference_other_loop(st, keys, result)
    }
  }
}

fn difference_this_loop(
  st: Agent,
  rec: SetRecord,
  remaining: List(JsVal),
  result: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(JsVal, Agent) {
  case remaining {
    [] -> alloc_new_set(st, result)
    [e, ..rest] -> {
      let #(in_other, st) = set_record_has(st, rec, e)
      let result = case in_other {
        True -> ordered_entries.delete(result, js_to_map_key(e)).0
        False -> result
      }
      difference_this_loop(st, rec, rest, result)
    }
  }
}

fn difference_other_loop(
  st: Agent,
  keys: IteratorRecord,
  result: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(JsVal, Agent) {
  let #(next, st) = step_keys(st, keys)
  case next {
    None -> alloc_new_set(st, result)
    Some(v) -> {
      let result = ordered_entries.delete(result, js_to_map_key(v)).0
      difference_other_loop(st, keys, result)
    }
  }
}

fn set_symmetric_difference(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "symmetricDifference")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  let #(keys, st) = get_keys_iterator(st, rec)
  symmetric_difference_loop(st, ref, keys, read_set_store(st, ref))
}

fn symmetric_difference_loop(
  st: Agent,
  ref: SetRef,
  keys: IteratorRecord,
  result: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(JsVal, Agent) {
  let #(next, st) = step_keys(st, keys)
  case next {
    None -> alloc_new_set(st, result)
    Some(v) -> {
      let key = js_to_map_key(v)
      // Step 5.b.iii is a LIVE read of this's [[SetData]].
      let in_this = ordered_entries.has(read_set_store(st, ref), key)
      let result = case in_this {
        True -> ordered_entries.delete(result, key).0
        False -> set_data_append(result, v)
      }
      symmetric_difference_loop(st, ref, keys, result)
    }
  }
}

fn set_is_subset_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "isSubsetOf")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  case ordered_entries.size(read_set_store(st, ref)) > rec.size {
    True -> #(mk_bool(False), st)
    False -> this_step_loop(st, ref, rec, 0, False)
  }
}

fn this_step_loop(
  st: Agent,
  ref: SetRef,
  rec: SetRecord,
  cursor: Int,
  false_when: Bool,
) -> #(JsVal, Agent) {
  let store = read_set_store(st, ref)
  case ordered_entries.next_from(store, cursor) {
    None -> #(mk_bool(True), st)
    Some(#(next_cursor, _key, e)) -> {
      let #(in_other, st) = set_record_has(st, rec, e)
      case in_other == false_when {
        True -> #(mk_bool(False), st)
        False -> this_step_loop(st, ref, rec, next_cursor, false_when)
      }
    }
  }
}

fn set_is_superset_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "isSupersetOf")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  case ordered_entries.size(read_set_store(st, ref)) < rec.size {
    True -> #(mk_bool(False), st)
    False -> {
      let #(keys, st) = get_keys_iterator(st, rec)
      other_step_loop(st, ref, keys, False)
    }
  }
}

fn other_step_loop(
  st: Agent,
  ref: SetRef,
  keys: IteratorRecord,
  false_when: Bool,
) -> #(JsVal, Agent) {
  let #(next, st) = step_keys(st, keys)
  case next {
    None -> #(mk_bool(True), st)
    Some(v) -> {
      let store = read_set_store(st, ref)
      case ordered_entries.has(store, js_to_map_key(v)) == false_when {
        True -> {
          let st = iter_protocol.iterator_close_normal(st, keys.iterator)
          #(mk_bool(False), st)
        }
        False -> other_step_loop(st, ref, keys, false_when)
      }
    }
  }
}

fn set_is_disjoint_from(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use ref <- require_set(st, this, "isDisjointFrom")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  case ordered_entries.size(read_set_store(st, ref)) <= rec.size {
    True -> this_step_loop(st, ref, rec, 0, True)
    False -> {
      let #(keys, st) = get_keys_iterator(st, rec)
      other_step_loop(st, ref, keys, True)
    }
  }
}

// ── GetSetRecord + protocol helpers ─────────────────────────────────────────

/// Spec's Set Record — captured size/has/keys from the `other` argument.
/// `size` is post-ToIntegerOrInfinity (+∞ saturated to 2^53-1).
type SetRecord {
  SetRecord(obj: JsVal, size: Int, has: JsVal, keys: JsVal)
}

/// ES2025 §24.2.1.2 GetSetRecord(obj) — validates `other` is set-like: reads
/// .size (ToNumber → NaN check → ToIntegerOrInfinity → negative check), then
/// .has and .keys (both callable). CPS: `use rec, st <- get_set_record(st, o)`.
fn get_set_record(
  st: Agent,
  other: JsVal,
  cont: fn(SetRecord, Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case classify(other) {
    KHandle(_) -> {
      let #(raw_size, st) =
        rt_obj.t_get_prop(st, other, StringKey(Named("size")))
      let #(num, st) = rt_val.t_to_number(st, raw_size)
      case num {
        JNan -> rt_val.t_throw_type_error(st, "size is NaN")
        num -> {
          let int_size = rt_val.jsnum_to_integer_or_infinity(num)
          case int_size < 0 {
            True -> rt_val.t_throw_range_error(st, "size is negative")
            False -> {
              let #(has, st) =
                rt_obj.t_get_prop(st, other, StringKey(Named("has")))
              use has <- helpers.require_callable(st, has, fn() {
                "has is not a function"
              })
              let #(keys, st) =
                rt_obj.t_get_prop(st, other, StringKey(Named("keys")))
              use keys <- helpers.require_callable(st, keys, fn() {
                "keys is not a function"
              })
              cont(SetRecord(obj: other, size: int_size, has:, keys:), st)
            }
          }
        }
      }
    }
    _ -> rt_val.t_throw_type_error(st, "other is not an object")
  }
}

/// §24.2.1.3 GetKeysIterator(rec): call rec.keys(), require the result is an
/// Object, require its .next is callable.
fn get_keys_iterator(st: Agent, rec: SetRecord) -> #(IteratorRecord, Agent) {
  let #(iter, st) = rt_call.t_call_checked(st, rec.keys, rec.obj, [])
  case classify(iter) {
    KHandle(_) -> {
      let #(next_fn, st) = rt_obj.t_get_prop(st, iter, StringKey(Named("next")))
      case rt_call.is_callable(st, next_fn) {
        False ->
          rt_val.t_throw_type_error(st, "iterator.next is not a function")
        True -> #(
          rt_types.IteratorRecord(iterator: iter, next_method: next_fn),
          st,
        )
      }
    }
    _ -> rt_val.t_throw_type_error(st, "keys() did not return an object")
  }
}

/// §7.4.8 IteratorStepValue on a keys IteratorRecord, -0 → +0 on yielded
/// values (§24.2.1.2 step 7.b.ii).
fn step_keys(st: Agent, keys: IteratorRecord) -> #(Option(JsVal), Agent) {
  let #(step, st) = iter_protocol.iterator_step_value(st, keys)
  #(option.map(step, normalize_neg_zero), st)
}

/// Call rec.has(v), ToBoolean the result.
fn set_record_has(st: Agent, rec: SetRecord, v: JsVal) -> #(Bool, Agent) {
  let #(r, st) = rt_call.t_call_checked(st, rec.has, rec.obj, [v])
  #(rt_val.to_boolean(r), st)
}

// ── helpers ─────────────────────────────────────────────────────────────────

/// SetDataAppend — the ONE place a value enters a [[SetData]] store. -0
/// normalizes to +0 (SameValueZero: what iteration YIELDS is the stored value,
/// so `[...new Set([-0])][0]` must be +0).
fn set_data_append(
  store: ordered_entries.OrderedEntries(MapKey, JsVal),
  val: JsVal,
) -> ordered_entries.OrderedEntries(MapKey, JsVal) {
  let val = normalize_neg_zero(val)
  ordered_entries.insert(store, js_to_map_key(val), val)
}

/// -0 → +0; identity otherwise. IEEE 754: -0.0 +. 0.0 == +0.0.
fn normalize_neg_zero(v: JsVal) -> JsVal {
  case classify(v) {
    KNum(JFloat(f)) -> mk_number(JFloat(f +. 0.0))
    _ -> v
  }
}

fn alloc_new_set(
  st: Agent,
  entries: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(JsVal, Agent) {
  let #(h, st) = alloc_kind_cell(st, SetObj(entries:), st.realm.set.prototype)
  #(mk_object(h), st)
}

type SetRef {
  SetRef(Handle)
}

fn set_ref_handle(r: SetRef) -> Handle {
  let SetRef(h) = r
  h
}

fn require_set(
  st: Agent,
  this: JsVal,
  method: String,
  cont: fn(SetRef) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use _nil, h <- helpers.require_brand(
    st,
    this,
    fn() {
      "Method Set.prototype." <> method <> " called on incompatible receiver"
    },
    set_brand_of,
  )
  cont(SetRef(h))
}

fn set_brand_of(kind: ObjKind) -> Option(Nil) {
  case kind {
    SetObj(..) -> Some(Nil)
    _ -> None
  }
}

fn read_set_store(
  st: Agent,
  ref: SetRef,
) -> ordered_entries.OrderedEntries(MapKey, JsVal) {
  let assert SObject(kind: SetObj(entries:), ..) =
    rt_store.t_cell_get(st, set_ref_handle(ref))
    as "set: SetRef does not point at a Set slot"
  entries
}

fn update_set(
  st: Agent,
  ref: SetRef,
  entries: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> Agent {
  rt_store.t_cell_update(st, set_ref_handle(ref), fn(slot) {
    let assert SObject(..) = slot
    SObject(..slot, kind: SetObj(entries:))
  })
}

fn alloc_kind_cell(
  st: Agent,
  kind: ObjKind,
  proto: Handle,
) -> #(Handle, Agent) {
  rt_store.t_cell_new(
    st,
    SObject(
      kind:,
      proto: Some(proto),
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}
