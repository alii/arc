import arc/internal/ordered_entries
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{first_arg_or_undefined, two_args_or_undefined}
import arc/rt/builtins/iter_protocol
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type MapIterKind,
  type MapKey, type MapNative, type ObjKind, JInt, KNull, KUndef, MapClear,
  MapConstructor, MapDelete, MapEntries, MapForEach, MapGet, MapGetOrInsert,
  MapGetOrInsertComputed, MapGetSize, MapGroupBy, MapHas, MapIterEntries,
  MapIterKeys, MapIterValues, MapIterator, MapKeys, MapN, MapObj, MapSet,
  MapValues, Named, NoElements, SObject, StringKey, classify, js_to_map_key,
  map_key_to_js, mk_bool, mk_number, mk_object, mk_undefined, symbol_iterator,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("get", MapN(MapGet), 1),
      #("set", MapN(MapSet), 2),
      #("has", MapN(MapHas), 1),
      #("delete", MapN(MapDelete), 1),
      #("clear", MapN(MapClear), 0),
      #("forEach", MapN(MapForEach), 1),
      #("keys", MapN(MapKeys), 0),
      #("values", MapN(MapValues), 0),
      #("getOrInsert", MapN(MapGetOrInsert), 2),
      #("getOrInsertComputed", MapN(MapGetOrInsertComputed), 2),
    ])
  let #(statics, st) =
    common.alloc_methods(st, fn_proto, [#("groupBy", MapN(MapGroupBy), 2)])
  let #(entries_h, st) =
    common.alloc_rooted_native_fn(st, fn_proto, MapN(MapEntries), "entries", 0)
  let #(entries_prop, st) = common.builtin_property(st, mk_object(entries_h))
  let #(size_props, st) =
    common.alloc_getters(st, fn_proto, [#("size", MapN(MapGetSize))])
  let proto_props =
    list.flatten([size_props, [#("entries", entries_prop)], proto_methods])
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      proto_props,
      fn(proto) { MapN(MapConstructor(proto:)) },
      "Map",
      0,
      statics,
    )
  let st = common.add_to_string_tag(st, bt.prototype, "Map")
  // @@iterator is the same function object as entries
  let #(iter_prop, st) = common.restamp(st, entries_prop)
  let st =
    common.add_symbol_property(st, bt.prototype, symbol_iterator, iter_prop)
  #(bt, st)
}

pub fn dispatch(
  st: Agent,
  n: MapNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case n {
    MapConstructor(..) ->
      rt_val.t_throw_type_error(st, "Constructor Map requires 'new'")
    MapGroupBy -> map_group_by(st, args)
    MapGet -> map_get(st, this, args)
    MapSet -> map_set(st, this, args)
    MapHas -> map_has(st, this, args)
    MapDelete -> map_delete(st, this, args)
    MapClear -> map_clear(st, this)
    MapForEach -> map_for_each(st, this, args)
    MapGetSize -> map_get_size(st, this)
    MapKeys -> map_iterator(st, this, "keys", MapIterKeys)
    MapValues -> map_iterator(st, this, "values", MapIterValues)
    MapEntries -> map_iterator(st, this, "entries", MapIterEntries)
    MapGetOrInsert -> map_get_or_insert(st, this, args)
    MapGetOrInsertComputed -> map_get_or_insert_computed(st, this, args)
  }
}

pub fn dispatch_construct(
  st: Agent,
  n: MapNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case n {
    MapConstructor(..) -> map_constructor(st, args, new_target)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

fn map_constructor(
  st: Agent,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let #(proto, st) =
    rt_call.get_prototype_from_constructor(st, new_target, fn(r) {
      r.map.prototype
    })
  let #(map_h, st) = alloc_map_cell(st, proto, ordered_entries.new())
  let map = mk_object(map_h)
  case classify(first_arg_or_undefined(args)) {
    KUndef | KNull -> #(map_h, st)
    _ -> {
      let iterable = first_arg_or_undefined(args)
      let #(adder, st) = rt_obj.t_get_prop(st, map, StringKey(Named("set")))
      case rt_call.is_callable(st, adder) {
        False ->
          rt_val.t_throw_type_error(
            st,
            "'set' property of Map is not a function",
          )
        True -> {
          let #(_map, st) =
            iter_protocol.add_entries_from_iterable(st, map, iterable, adder)
          #(map_h, st)
        }
      }
    }
  }
}

fn map_group_by(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(items, callback) = two_args_or_undefined(args)
  let #(_, st) = rt_val.t_require_object_coercible(st, items)
  use callback <- helpers.require_callable(st, callback, fn() {
    "Map.groupBy callback is not callable"
  })
  let #(rec, st) = iter_protocol.get_iterator_sync(st, items)
  group_by_loop(st, rec, callback, 0, dict.new(), [])
}

fn group_by_loop(
  st: Agent,
  rec: iter_protocol.IteratorRecord,
  callback: JsVal,
  index: Int,
  groups: dict.Dict(MapKey, List(JsVal)),
  order: List(MapKey),
) -> #(JsVal, Agent) {
  case iter_protocol.iterator_step_value(st, rec) {
    #(None, st) -> group_by_finish(st, groups, list.reverse(order))
    #(Some(item), st) -> {
      use kv, st <- iter_protocol.or_close(st, rec.iterator, fn(st) {
        rt_call.t_call_checked(st, callback, mk_undefined(), [
          item,
          mk_number(JInt(index)),
        ])
      })
      let key = js_to_map_key(kv)
      let #(groups, order) = case dict.get(groups, key) {
        Ok(members) -> #(dict.insert(groups, key, [item, ..members]), order)
        Error(Nil) -> #(dict.insert(groups, key, [item]), [key, ..order])
      }
      group_by_loop(st, rec, callback, index + 1, groups, order)
    }
  }
}

fn group_by_finish(
  st: Agent,
  groups: dict.Dict(MapKey, List(JsVal)),
  order: List(MapKey),
) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  let #(entries, st) =
    list.fold(order, #(ordered_entries.new(), st), fn(acc, key) {
      let #(entries, st) = acc
      let members = dict.get(groups, key) |> result.unwrap([])
      let #(arr_h, st) =
        common.alloc_array(st, list.reverse(members), array_proto)
      #(ordered_entries.insert(entries, key, mk_object(arr_h)), st)
    })
  let #(map_h, st) = alloc_map_cell(st, st.realm.map.prototype, entries)
  #(mk_object(map_h), st)
}

fn map_get(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let key_arg = first_arg_or_undefined(args)
  use ref <- require_map(st, this, "get")
  let map_key = js_to_map_key(key_arg)
  let result =
    ordered_entries.get(read_map_store(st, ref), map_key)
    |> option.unwrap(mk_undefined())
  #(result, st)
}

fn map_set(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(key_arg, val_arg) = two_args_or_undefined(args)
  use ref <- require_map(st, this, "set")
  let store = read_map_store(st, ref)
  let map_key = js_to_map_key(key_arg)
  let store = ordered_entries.insert(store, map_key, val_arg)
  let st = update_map_data(st, ref, store)
  #(this, st)
}

fn map_get_or_insert(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(key_arg, val_arg) = two_args_or_undefined(args)
  use ref <- require_map(st, this, "getOrInsert")
  let store = read_map_store(st, ref)
  let map_key = js_to_map_key(key_arg)
  case ordered_entries.get(store, map_key) {
    Some(existing) -> #(existing, st)
    None -> {
      let st =
        update_map_data(
          st,
          ref,
          ordered_entries.insert(store, map_key, val_arg),
        )
      #(val_arg, st)
    }
  }
}

fn map_get_or_insert_computed(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(key_arg, callback) = two_args_or_undefined(args)
  use ref <- require_map(st, this, "getOrInsertComputed")
  use callback <- helpers.require_callable(st, callback, fn() {
    let #(ty, _) = rt_val.t_type_of(st, callback)
    ty <> " is not a function"
  })
  let map_key = js_to_map_key(key_arg)
  case ordered_entries.get(read_map_store(st, ref), map_key) {
    Some(existing) -> #(existing, st)
    None -> {
      let #(value, st) =
        rt_call.t_call_checked(st, callback, mk_undefined(), [
          map_key_to_js(map_key),
        ])
      let store =
        ordered_entries.insert(read_map_store(st, ref), map_key, value)
      #(value, update_map_data(st, ref, store))
    }
  }
}

fn map_has(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let key_arg = first_arg_or_undefined(args)
  use ref <- require_map(st, this, "has")
  let map_key = js_to_map_key(key_arg)
  #(mk_bool(ordered_entries.has(read_map_store(st, ref), map_key)), st)
}

fn map_delete(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let key_arg = first_arg_or_undefined(args)
  use ref <- require_map(st, this, "delete")
  let store = read_map_store(st, ref)
  let map_key = js_to_map_key(key_arg)
  case ordered_entries.delete(store, map_key) {
    #(_store, False) -> #(mk_bool(False), st)
    #(store, True) -> {
      let st = update_map_data(st, ref, store)
      #(mk_bool(True), st)
    }
  }
}

fn map_clear(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use ref <- require_map(st, this, "clear")
  let store = read_map_store(st, ref)
  let st = update_map_data(st, ref, ordered_entries.clear(store))
  #(mk_undefined(), st)
}

fn map_for_each(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(cb, this_arg) = two_args_or_undefined(args)
  // brand check before callable check, observable
  use ref <- require_map(st, this, "forEach")
  use cb <- helpers.require_callable(st, cb, fn() {
    let #(ty, _) = rt_val.t_type_of(st, cb)
    ty <> " is not a function"
  })
  for_each_loop(st, ref, 0, cb, this_arg, this)
}

fn for_each_loop(
  st: Agent,
  ref: MapRef,
  cursor: Int,
  cb: JsVal,
  this_arg: JsVal,
  map_this: JsVal,
) -> #(JsVal, Agent) {
  let store = read_map_store(st, ref)
  case ordered_entries.next_from(store, cursor) {
    None -> #(mk_undefined(), st)
    Some(#(next_cursor, map_key, val)) -> {
      let original_key = map_key_to_js(map_key)
      let #(_result, st) =
        rt_call.t_call_checked(st, cb, this_arg, [
          val,
          original_key,
          map_this,
        ])
      for_each_loop(st, ref, next_cursor, cb, this_arg, map_this)
    }
  }
}

fn map_get_size(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use ref <- require_map(st, this, "size")
  #(mk_number(JInt(ordered_entries.size(read_map_store(st, ref)))), st)
}

fn map_iterator(
  st: Agent,
  this: JsVal,
  method: String,
  kind: MapIterKind,
) -> #(JsVal, Agent) {
  use ref <- require_map(st, this, method)
  let #(iter_h, st) =
    alloc_kind_cell(
      st,
      MapIterator(target: map_ref_handle(ref), index: 0, kind:),
      st.realm.map_iter_proto,
    )
  #(mk_object(iter_h), st)
}

type MapRef {
  MapRef(Handle)
}

fn map_ref_handle(r: MapRef) -> Handle {
  let MapRef(h) = r
  h
}

fn require_map(
  st: Agent,
  this: JsVal,
  method: String,
  cont: fn(MapRef) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use _nil, h <- helpers.require_brand(
    st,
    this,
    fn() {
      "Method Map.prototype." <> method <> " called on incompatible receiver"
    },
    map_brand_of,
  )
  cont(MapRef(h))
}

fn map_brand_of(kind: ObjKind) -> Option(Nil) {
  case kind {
    MapObj(..) -> Some(Nil)
    _ -> None
  }
}

fn read_map_store(
  st: Agent,
  ref: MapRef,
) -> ordered_entries.OrderedEntries(MapKey, JsVal) {
  let assert SObject(kind: MapObj(entries:), ..) =
    rt_store.t_cell_get(st, map_ref_handle(ref))
    as "map: MapRef does not point at a Map slot"
  entries
}

fn update_map_data(
  st: Agent,
  ref: MapRef,
  entries: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> Agent {
  rt_store.t_cell_update(st, map_ref_handle(ref), fn(slot) {
    let assert SObject(..) = slot
    SObject(..slot, kind: MapObj(entries:))
  })
}

fn alloc_map_cell(
  st: Agent,
  proto: Handle,
  entries: ordered_entries.OrderedEntries(MapKey, JsVal),
) -> #(Handle, Agent) {
  alloc_kind_cell(st, MapObj(entries:), proto)
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
