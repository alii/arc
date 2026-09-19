import arc/bytecode/key.{Named}
import arc/internal/ordered_entries
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{first_arg_or_undefined}
import arc/rt/builtins/iter_protocol
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type IteratorRecord, type JsVal,
  type MapKey, type ObjKind, type SetIterKind, type SetNative, JFloat, JNan,
  KNull, KNum, KUndef, SObject, SetAdd, SetClear, SetConstructor, SetDelete,
  SetDifference, SetEntries, SetForEach, SetGetSize, SetHas, SetIntersection,
  SetIsDisjointFrom, SetIsSubsetOf, SetIsSupersetOf, SetIterEntries,
  SetIterValues, SetIterator, SetN, SetObj, SetSymmetricDifference, SetUnion,
  SetValues, StringKey, classify, js_to_map_key, mk_bool, mk_int, mk_number,
  mk_object, mk_undefined, symbol_iterator,
}
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{type Option, None, Some}

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
  // keys and @@iterator must alias the same values function
  let #(values_h, st) =
    common.alloc_rooted_native_fn(st, fn_proto, SetN(SetValues), "values", 0)
  let #(values_prop, st) = rt_store.builtin_property(st, mk_object(values_h))
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
      fn(_) { SetN(SetConstructor) },
      "Set",
      0,
      [],
    )
  let st = common.add_string_tag(st, bt.prototype, "Set")
  let #(iter_prop, st) = common.restamp(st, values_prop)
  let st =
    common.add_symbol_property(st, bt.prototype, symbol_iterator, iter_prop)
  #(bt, st)
}

pub fn dispatch(
  st: Agent,
  n: SetNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case n {
    SetConstructor ->
      rt_val.throw_type_error(st, "Constructor Set requires 'new'")
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
    SetConstructor -> set_constructor(st, args, new_target)
    _ -> rt_val.throw_type_error(st, "not a constructor")
  }
}

// §24.2.1.1 set ( [ iterable ] )
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
    realm_ops.alloc_object(st, SetObj(entries: ordered_entries.new()), proto)
  let set_v = mk_object(set_h)
  case classify(first_arg_or_undefined(args)) {
    KUndef | KNull -> #(set_h, st)
    _ -> {
      let iterable = first_arg_or_undefined(args)
      let #(adder, st) = rt_obj.get_prop(st, set_v, StringKey(Named("add")))
      case rt_val.is_callable(st, adder) {
        False ->
          rt_val.throw_type_error(st, "'add' property of Set is not a function")
        True -> {
          let #(_set, st) =
            iter_protocol.add_values_from_iterable(st, set_v, iterable, adder)
          #(set_h, st)
        }
      }
    }
  }
}

fn set_add(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "add")
  let store = read_set_store(st, set)
  let store = set_data_append(store, first_arg_or_undefined(args))
  #(this, update_set(st, set, store))
}

fn set_has(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "has")
  let key = js_to_map_key(first_arg_or_undefined(args))
  #(mk_bool(ordered_entries.has(read_set_store(st, set), key)), st)
}

fn set_delete(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "delete")
  let store = read_set_store(st, set)
  let key = js_to_map_key(first_arg_or_undefined(args))
  case ordered_entries.delete(store, key) {
    #(False, _store) -> #(mk_bool(False), st)
    #(True, store) -> #(mk_bool(True), update_set(st, set, store))
  }
}

fn set_clear(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "clear")
  let store = read_set_store(st, set)
  #(mk_undefined(), update_set(st, set, ordered_entries.clear(store)))
}

fn set_size(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "size")
  #(mk_int(ordered_entries.size(read_set_store(st, set))), st)
}

fn set_for_each(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "forEach")
  let #(cb, this_arg) = helpers.two_args_or_undefined(args)
  use cb <- helpers.require_callable(st, cb, fn() {
    "Set.prototype.forEach callback is not a function"
  })
  set_for_each_loop(st, set, 0, cb, this_arg, this)
}

// live: store is re-read each step
fn set_for_each_loop(
  st: Agent,
  set: SetHandle,
  cursor: Int,
  cb: JsVal,
  this_arg: JsVal,
  set_this: JsVal,
) -> #(JsVal, Agent) {
  let store = read_set_store(st, set)
  case ordered_entries.next_from(store, cursor) {
    None -> #(mk_undefined(), st)
    Some(#(next_cursor, _key, val)) -> {
      let #(_r, st) = rt_call.call(st, cb, this_arg, [val, val, set_this])
      set_for_each_loop(st, set, next_cursor, cb, this_arg, set_this)
    }
  }
}

fn set_values(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "values")
  alloc_set_iterator(st, set, SetIterValues)
}

fn set_entries(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "entries")
  alloc_set_iterator(st, set, SetIterEntries)
}

fn alloc_set_iterator(
  st: Agent,
  source: SetHandle,
  kind: SetIterKind,
) -> #(JsVal, Agent) {
  let #(iter_h, st) =
    realm_ops.alloc_object(
      st,
      SetIterator(target: source.handle, index: 0, kind:),
      st.realm.set_iter_proto,
    )
  #(mk_object(iter_h), st)
}

fn set_union(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "union")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  let #(keys, st) = get_keys_iterator(st, rec)
  set_union_loop(st, keys, read_set_store(st, set))
}

fn set_union_loop(
  st: Agent,
  keys: IteratorRecord,
  result: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(JsVal, Agent) {
  let #(next, st) = step_keys(st, keys)
  case next {
    None -> alloc_new_set(st, result)
    Some(v) -> set_union_loop(st, keys, set_data_append(result, v))
  }
}

fn set_intersection(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "intersection")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  case ordered_entries.size(read_set_store(st, set)) <= rec.size {
    True -> intersect_walking_this(st, set, rec, 0, ordered_entries.new())
    False -> {
      let #(keys, st) = get_keys_iterator(st, rec)
      intersect_walking_other(st, set, keys, ordered_entries.new())
    }
  }
}

fn intersect_walking_this(
  st: Agent,
  set: SetHandle,
  rec: SetRecord,
  cursor: Int,
  result: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(JsVal, Agent) {
  let store = read_set_store(st, set)
  case ordered_entries.next_from(store, cursor) {
    None -> alloc_new_set(st, result)
    Some(#(next_cursor, _key, e)) -> {
      let #(in_other, st) = set_record_has(st, rec, e)
      let result = case in_other {
        True -> set_data_append(result, e)
        False -> result
      }
      intersect_walking_this(st, set, rec, next_cursor, result)
    }
  }
}

fn intersect_walking_other(
  st: Agent,
  set: SetHandle,
  keys: IteratorRecord,
  result: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(JsVal, Agent) {
  let #(next, st) = step_keys(st, keys)
  case next {
    None -> alloc_new_set(st, result)
    Some(v) -> {
      let store = read_set_store(st, set)
      let result = case ordered_entries.has(store, js_to_map_key(v)) {
        True -> set_data_append(result, v)
        False -> result
      }
      intersect_walking_other(st, set, keys, result)
    }
  }
}

fn set_difference(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "difference")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  let result = read_set_store(st, set)
  case ordered_entries.size(result) <= rec.size {
    True ->
      subtract_walking_this(
        st,
        rec,
        ordered_entries.live_values(result),
        result,
      )
    False -> {
      let #(keys, st) = get_keys_iterator(st, rec)
      subtract_walking_other(st, keys, result)
    }
  }
}

fn subtract_walking_this(
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
        True -> ordered_entries.delete(result, js_to_map_key(e)).1
        False -> result
      }
      subtract_walking_this(st, rec, rest, result)
    }
  }
}

fn subtract_walking_other(
  st: Agent,
  keys: IteratorRecord,
  result: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(JsVal, Agent) {
  let #(next, st) = step_keys(st, keys)
  case next {
    None -> alloc_new_set(st, result)
    Some(v) -> {
      let result = ordered_entries.delete(result, js_to_map_key(v)).1
      subtract_walking_other(st, keys, result)
    }
  }
}

fn set_symmetric_difference(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "symmetricDifference")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  let #(keys, st) = get_keys_iterator(st, rec)
  set_symmetric_difference_loop(st, set, keys, read_set_store(st, set))
}

fn set_symmetric_difference_loop(
  st: Agent,
  set: SetHandle,
  keys: IteratorRecord,
  result: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(JsVal, Agent) {
  let #(next, st) = step_keys(st, keys)
  case next {
    None -> alloc_new_set(st, result)
    Some(v) -> {
      let key = js_to_map_key(v)
      // spec step 5.b.iii is a live read
      let in_this = ordered_entries.has(read_set_store(st, set), key)
      let result = case in_this {
        True -> ordered_entries.delete(result, key).1
        False -> set_data_append(result, v)
      }
      set_symmetric_difference_loop(st, set, keys, result)
    }
  }
}

fn set_is_subset_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "isSubsetOf")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  case ordered_entries.size(read_set_store(st, set)) > rec.size {
    True -> #(mk_bool(False), st)
    False -> check_each_of_this(st, set, rec, 0, false_when: False)
  }
}

fn check_each_of_this(
  st: Agent,
  set: SetHandle,
  rec: SetRecord,
  cursor: Int,
  false_when false_when: Bool,
) -> #(JsVal, Agent) {
  let store = read_set_store(st, set)
  case ordered_entries.next_from(store, cursor) {
    None -> #(mk_bool(True), st)
    Some(#(next_cursor, _key, e)) -> {
      let #(in_other, st) = set_record_has(st, rec, e)
      case in_other == false_when {
        True -> #(mk_bool(False), st)
        False -> check_each_of_this(st, set, rec, next_cursor, false_when)
      }
    }
  }
}

fn set_is_superset_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "isSupersetOf")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  case ordered_entries.size(read_set_store(st, set)) < rec.size {
    True -> #(mk_bool(False), st)
    False -> {
      let #(keys, st) = get_keys_iterator(st, rec)
      check_each_of_other(st, set, keys, false_when: False)
    }
  }
}

fn check_each_of_other(
  st: Agent,
  set: SetHandle,
  keys: IteratorRecord,
  false_when false_when: Bool,
) -> #(JsVal, Agent) {
  let #(next, st) = step_keys(st, keys)
  case next {
    None -> #(mk_bool(True), st)
    Some(v) -> {
      let store = read_set_store(st, set)
      case ordered_entries.has(store, js_to_map_key(v)) == false_when {
        True -> {
          let st = iter_protocol.iterator_close_normal(st, keys.iterator)
          #(mk_bool(False), st)
        }
        False -> check_each_of_other(st, set, keys, false_when)
      }
    }
  }
}

fn set_is_disjoint_from(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use set <- require_set(st, this, "isDisjointFrom")
  use rec, st <- get_set_record(st, first_arg_or_undefined(args))
  case ordered_entries.size(read_set_store(st, set)) <= rec.size {
    True -> check_each_of_this(st, set, rec, 0, false_when: True)
    False -> {
      let #(keys, st) = get_keys_iterator(st, rec)
      check_each_of_other(st, set, keys, false_when: True)
    }
  }
}

type SetRecord {
  SetRecord(obj: JsVal, size: Int, has: JsVal, keys: JsVal)
}

// §24.2.1.2 getsetrecord
fn get_set_record(
  st: Agent,
  other: JsVal,
  cont: fn(SetRecord, Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use Nil <- helpers.guard(rt_val.is_object(other), fn() {
    rt_val.throw_type_error(st, "other is not an object")
  })
  let #(raw_size, st) = rt_obj.get_prop(st, other, StringKey(Named("size")))
  let #(num, st) = rt_val.to_number(st, raw_size)
  use Nil <- helpers.guard(num != JNan, fn() {
    rt_val.throw_type_error(st, "size is NaN")
  })
  let int_size = rt_val.jsnum_to_integer_or_infinity(num)
  use Nil <- helpers.guard(int_size >= 0, fn() {
    rt_val.throw_range_error(st, "size is negative")
  })
  let #(has, st) = rt_obj.get_prop(st, other, StringKey(Named("has")))
  use has <- helpers.require_callable(st, has, fn() { "has is not a function" })
  let #(keys, st) = rt_obj.get_prop(st, other, StringKey(Named("keys")))
  use keys <- helpers.require_callable(st, keys, fn() {
    "keys is not a function"
  })
  cont(SetRecord(obj: other, size: int_size, has:, keys:), st)
}

// §24.2.1.3 getkeysiterator
fn get_keys_iterator(st: Agent, rec: SetRecord) -> #(IteratorRecord, Agent) {
  let #(iter, st) = rt_call.call(st, rec.keys, rec.obj, [])
  use Nil <- helpers.guard(rt_val.is_object(iter), fn() {
    rt_val.throw_type_error(st, "keys() did not return an object")
  })
  let #(next_fn, st) = rt_obj.get_prop(st, iter, StringKey(Named("next")))
  use Nil <- helpers.guard(rt_val.is_callable(st, next_fn), fn() {
    rt_val.throw_type_error(st, "iterator.next is not a function")
  })
  #(types.IteratorRecord(iterator: iter, next_method: next_fn), st)
}

fn step_keys(st: Agent, keys: IteratorRecord) -> #(Option(JsVal), Agent) {
  let #(step, st) = iter_protocol.iterator_step_value(st, keys)
  #(option.map(step, normalize_neg_zero), st)
}

fn set_record_has(st: Agent, rec: SetRecord, v: JsVal) -> #(Bool, Agent) {
  let #(r, st) = rt_call.call(st, rec.has, rec.obj, [v])
  #(rt_val.to_boolean(r), st)
}

// the one place a value enters set data, -0 becomes +0
fn set_data_append(
  store: ordered_entries.OrderedEntries(MapKey, JsVal),
  val: JsVal,
) -> ordered_entries.OrderedEntries(MapKey, JsVal) {
  let val = normalize_neg_zero(val)
  ordered_entries.insert(store, js_to_map_key(val), val)
}

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
  let #(h, st) =
    realm_ops.alloc_object(st, SetObj(entries:), st.realm.set.prototype)
  #(mk_object(h), st)
}

type SetHandle {
  SetHandle(handle: Handle)
}

fn require_set(
  st: Agent,
  this: JsVal,
  method: String,
  cont: fn(SetHandle) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use _nil, h <- helpers.require_brand(
    st,
    this,
    fn() {
      "Method Set.prototype." <> method <> " called on incompatible receiver"
    },
    set_brand_of,
  )
  cont(SetHandle(h))
}

fn set_brand_of(kind: ObjKind) -> Option(Nil) {
  case kind {
    SetObj(..) -> Some(Nil)
    _ -> None
  }
}

fn read_set_store(
  st: Agent,
  set: SetHandle,
) -> ordered_entries.OrderedEntries(MapKey, JsVal) {
  let assert SObject(kind: SetObj(entries:), ..) =
    rt_store.cell_get(st, set.handle)
    as "set: SetHandle does not point at a Set cell"
  entries
}

fn update_set(
  st: Agent,
  set: SetHandle,
  entries: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> Agent {
  rt_store.cell_update(st, set.handle, fn(cell) {
    let assert SObject(..) = cell
    SObject(..cell, kind: SetObj(entries:))
  })
}
