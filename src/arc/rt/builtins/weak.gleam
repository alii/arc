import arc/bytecode/key.{Named}
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{
  arg_at, first_arg_or_undefined, two_args_or_undefined,
}
import arc/rt/builtins/iter_protocol
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type ObjKind,
  type Realm, type WeakKey, type WeakNative, KHandle, KNull, KSym, KUndef,
  SObject, StringKey, WeakMapConstructor, WeakMapDelete, WeakMapGet,
  WeakMapGetOrInsert, WeakMapGetOrInsertComputed, WeakMapHas, WeakMapObj,
  WeakMapSet, WeakN, WeakObjKey, WeakSetAdd, WeakSetConstructor, WeakSetDelete,
  WeakSetHas, WeakSetObj, WeakSymKey, classify, mk_bool, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(#(BuiltinPair, BuiltinPair), Agent) {
  let #(wm_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("get", WeakN(WeakMapGet), 1),
      #("set", WeakN(WeakMapSet), 2),
      #("has", WeakN(WeakMapHas), 1),
      #("delete", WeakN(WeakMapDelete), 1),
      #("getOrInsert", WeakN(WeakMapGetOrInsert), 2),
      #("getOrInsertComputed", WeakN(WeakMapGetOrInsertComputed), 2),
    ])
  let #(weak_map, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      wm_methods,
      fn(proto) { WeakN(WeakMapConstructor(proto:)) },
      "WeakMap",
      0,
      [],
    )
  let st = common.add_string_tag(st, weak_map.prototype, "WeakMap")
  let #(ws_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("add", WeakN(WeakSetAdd), 1),
      #("has", WeakN(WeakSetHas), 1),
      #("delete", WeakN(WeakSetDelete), 1),
    ])
  let #(weak_set, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      ws_methods,
      fn(proto) { WeakN(WeakSetConstructor(proto:)) },
      "WeakSet",
      0,
      [],
    )
  let st = common.add_string_tag(st, weak_set.prototype, "WeakSet")
  #(#(weak_map, weak_set), st)
}

pub fn dispatch(
  st: Agent,
  n: WeakNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case n {
    WeakMapConstructor(..) ->
      rt_val.throw_type_error(st, "Constructor WeakMap requires 'new'")
    WeakSetConstructor(..) ->
      rt_val.throw_type_error(st, "Constructor WeakSet requires 'new'")
    WeakMapGet -> weak_map_get(st, this, args)
    WeakMapSet -> weak_map_set(st, this, args)
    WeakMapHas -> weak_map_has(st, this, args)
    WeakMapDelete -> weak_map_delete(st, this, args)
    WeakMapGetOrInsert -> weak_map_get_or_insert(st, this, args)
    WeakMapGetOrInsertComputed ->
      weak_map_get_or_insert_computed(st, this, args)
    WeakSetAdd -> weak_set_add(st, this, args)
    WeakSetHas -> weak_set_has(st, this, args)
    WeakSetDelete -> weak_set_delete(st, this, args)
  }
}

pub fn dispatch_construct(
  st: Agent,
  n: WeakNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case n {
    WeakMapConstructor(..) ->
      weak_construct(
        st,
        fn(r: Realm) { r.weak_map.prototype },
        args,
        new_target,
        WeakMapObj(entries: dict.new()),
        "WeakMap",
        "set",
        iter_protocol.add_entries_from_iterable,
      )
    WeakSetConstructor(..) ->
      weak_construct(
        st,
        fn(r: Realm) { r.weak_set.prototype },
        args,
        new_target,
        WeakSetObj(entries: set.new()),
        "WeakSet",
        "add",
        iter_protocol.add_values_from_iterable,
      )
    _ -> rt_val.throw_type_error(st, "not a constructor")
  }
}

fn weak_construct(
  st: Agent,
  intrinsic: fn(Realm) -> Handle,
  args: List(JsVal),
  new_target: JsVal,
  empty_kind: ObjKind,
  type_name: String,
  adder_name: String,
  add_from_iterable: fn(Agent, JsVal, JsVal, JsVal) -> #(JsVal, Agent),
) -> #(Handle, Agent) {
  let #(proto, st) =
    rt_call.get_prototype_from_constructor(st, new_target, intrinsic)
  let #(coll_h, st) = realm_ops.alloc_object(st, empty_kind, proto)
  let coll = mk_object(coll_h)
  case classify(first_arg_or_undefined(args)) {
    KUndef | KNull -> #(coll_h, st)
    _ -> {
      let iterable = first_arg_or_undefined(args)
      let #(adder, st) = rt_obj.get_prop(st, coll, StringKey(Named(adder_name)))
      case rt_val.is_callable(st, adder) {
        False ->
          rt_val.throw_type_error(
            st,
            "'"
              <> adder_name
              <> "' property of "
              <> type_name
              <> " is not a function",
          )
        True -> {
          let #(_coll, st) = add_from_iterable(st, coll, iterable, adder)
          #(coll_h, st)
        }
      }
    }
  }
}

fn weak_map_get(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use weak_map <- require_weak_map(st, this, "get")
  let key = first_arg_or_undefined(args)
  #(lookup_weak_map(st, weak_map, key) |> option.unwrap(mk_undefined()), st)
}

fn weak_map_set(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use weak_map <- require_weak_map(st, this, "set")
  let #(key, val) = two_args_or_undefined(args)
  use wk <- require_weak_key(st, key, "Invalid value used as weak map key")
  #(this, update_weak_map_entries(st, weak_map, dict.insert(_, wk, val)))
}

fn weak_map_has(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use weak_map <- require_weak_map(st, this, "has")
  let key = first_arg_or_undefined(args)
  case to_weak_key(key) {
    Some(wk) -> #(
      mk_bool(dict.has_key(read_weak_map_entries(st, weak_map), wk)),
      st,
    )
    None -> #(mk_bool(False), st)
  }
}

fn weak_map_delete(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use weak_map <- require_weak_map(st, this, "delete")
  let key = first_arg_or_undefined(args)
  case to_weak_key(key) {
    Some(wk) ->
      case dict.has_key(read_weak_map_entries(st, weak_map), wk) {
        True -> #(
          mk_bool(True),
          update_weak_map_entries(st, weak_map, dict.delete(_, wk)),
        )
        False -> #(mk_bool(False), st)
      }
    None -> #(mk_bool(False), st)
  }
}

fn weak_map_get_or_insert(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use weak_map <- require_weak_map(st, this, "getOrInsert")
  let key = first_arg_or_undefined(args)
  use wk <- require_weak_key(st, key, "Invalid value used as weak map key")
  case dict.get(read_weak_map_entries(st, weak_map), wk) {
    Ok(existing) -> #(existing, st)
    Error(Nil) -> {
      let val = arg_at(args, 1)
      #(val, update_weak_map_entries(st, weak_map, dict.insert(_, wk, val)))
    }
  }
}

// callback may insert the same key; re-read before writing
fn weak_map_get_or_insert_computed(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use weak_map <- require_weak_map(st, this, "getOrInsertComputed")
  let key = first_arg_or_undefined(args)
  use wk <- require_weak_key(st, key, "Invalid value used as weak map key")
  let callback = arg_at(args, 1)
  use callback <- helpers.require_callable(st, callback, fn() {
    rt_val.type_of(st, callback) <> " is not a function"
  })
  case dict.get(read_weak_map_entries(st, weak_map), wk) {
    Ok(existing) -> #(existing, st)
    Error(Nil) -> {
      let #(computed, st) = rt_call.call(st, callback, mk_undefined(), [key])
      #(
        computed,
        update_weak_map_entries(st, weak_map, dict.insert(_, wk, computed)),
      )
    }
  }
}

fn weak_set_add(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use weak_set <- require_weak_set(st, this, "add")
  let val = first_arg_or_undefined(args)
  use wk <- require_weak_key(st, val, "Invalid value used in weak set")
  #(this, update_weak_set_entries(st, weak_set, set.insert(_, wk)))
}

fn weak_set_has(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use weak_set <- require_weak_set(st, this, "has")
  let val = first_arg_or_undefined(args)
  case to_weak_key(val) {
    Some(wk) -> #(
      mk_bool(set.contains(read_weak_set_entries(st, weak_set), wk)),
      st,
    )
    None -> #(mk_bool(False), st)
  }
}

fn weak_set_delete(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use weak_set <- require_weak_set(st, this, "delete")
  let val = first_arg_or_undefined(args)
  case to_weak_key(val) {
    Some(wk) ->
      case set.contains(read_weak_set_entries(st, weak_set), wk) {
        True -> #(
          mk_bool(True),
          update_weak_set_entries(st, weak_set, set.delete(_, wk)),
        )
        False -> #(mk_bool(False), st)
      }
    None -> #(mk_bool(False), st)
  }
}

type WeakMapHandle {
  WeakMapHandle(Handle)
}

type WeakSetHandle {
  WeakSetHandle(Handle)
}

fn require_weak_map(
  st: Agent,
  this: JsVal,
  method: String,
  cont: fn(WeakMapHandle) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use _nil, h <- helpers.require_brand(
    st,
    this,
    fn() {
      "Method WeakMap.prototype."
      <> method
      <> " called on incompatible receiver"
    },
    fn(kind) {
      case kind {
        WeakMapObj(..) -> Some(Nil)
        _ -> None
      }
    },
  )
  cont(WeakMapHandle(h))
}

fn require_weak_set(
  st: Agent,
  this: JsVal,
  method: String,
  cont: fn(WeakSetHandle) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use _nil, h <- helpers.require_brand(
    st,
    this,
    fn() {
      "Method WeakSet.prototype."
      <> method
      <> " called on incompatible receiver"
    },
    fn(kind) {
      case kind {
        WeakSetObj(..) -> Some(Nil)
        _ -> None
      }
    },
  )
  cont(WeakSetHandle(h))
}

fn to_weak_key(v: JsVal) -> Option(WeakKey) {
  case classify(v) {
    KHandle(h) -> Some(WeakObjKey(h.id))
    KSym(id) ->
      case types.is_registered_symbol(id) {
        True -> None
        False -> Some(WeakSymKey(id))
      }
    _ -> None
  }
}

fn require_weak_key(
  st: Agent,
  key: JsVal,
  msg: String,
  cont: fn(WeakKey) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case to_weak_key(key) {
    Some(wk) -> cont(wk)
    None -> rt_val.throw_type_error(st, msg)
  }
}

fn read_weak_map_entries(
  st: Agent,
  weak_map: WeakMapHandle,
) -> Dict(WeakKey, JsVal) {
  let WeakMapHandle(h) = weak_map
  let assert SObject(kind: WeakMapObj(entries:), ..) = rt_store.cell_get(st, h)
    as "weak: WeakMapHandle does not point at a WeakMap cell"
  entries
}

fn lookup_weak_map(
  st: Agent,
  weak_map: WeakMapHandle,
  key: JsVal,
) -> Option(JsVal) {
  use wk <- option.then(to_weak_key(key))
  dict.get(read_weak_map_entries(st, weak_map), wk) |> option.from_result
}

// takes a fn so callers cannot write back a stale dict
fn update_weak_map_entries(
  st: Agent,
  weak_map: WeakMapHandle,
  f: fn(Dict(WeakKey, JsVal)) -> Dict(WeakKey, JsVal),
) -> Agent {
  let WeakMapHandle(h) = weak_map
  rt_store.cell_update(st, h, fn(cell) {
    let assert SObject(kind: WeakMapObj(entries:), ..) = cell
    SObject(..cell, kind: WeakMapObj(entries: f(entries)))
  })
}

fn read_weak_set_entries(st: Agent, weak_set: WeakSetHandle) -> Set(WeakKey) {
  let WeakSetHandle(h) = weak_set
  let assert SObject(kind: WeakSetObj(entries:), ..) = rt_store.cell_get(st, h)
    as "weak: WeakSetHandle does not point at a WeakSet cell"
  entries
}

fn update_weak_set_entries(
  st: Agent,
  weak_set: WeakSetHandle,
  f: fn(Set(WeakKey)) -> Set(WeakKey),
) -> Agent {
  let WeakSetHandle(h) = weak_set
  rt_store.cell_update(st, h, fn(cell) {
    let assert SObject(kind: WeakSetObj(entries:), ..) = cell
    SObject(..cell, kind: WeakSetObj(entries: f(entries)))
  })
}
