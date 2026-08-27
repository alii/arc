import arc/rt/builtins/common
import arc/rt/builtins/helpers.{
  arg_at, first_arg_or_undefined, two_args_or_undefined,
}
import arc/rt/builtins/iter_protocol
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type ObjKind,
  type Realm, type WeakKey, type WeakNative, KHandle, KNull, KSym, KUndef, Named,
  NoElements, SObject, StringKey, WeakMapConstructor, WeakMapDelete, WeakMapGet,
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
  let st = common.add_to_string_tag(st, weak_map.prototype, "WeakMap")
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
  let st = common.add_to_string_tag(st, weak_set.prototype, "WeakSet")
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
      rt_val.t_throw_type_error(st, "Constructor WeakMap requires 'new'")
    WeakSetConstructor(..) ->
      rt_val.t_throw_type_error(st, "Constructor WeakSet requires 'new'")
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
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
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
  let #(coll_h, st) = alloc_kind_cell(st, empty_kind, proto)
  let coll = mk_object(coll_h)
  case classify(first_arg_or_undefined(args)) {
    KUndef | KNull -> #(coll_h, st)
    _ -> {
      let iterable = first_arg_or_undefined(args)
      let #(adder, st) =
        rt_obj.t_get_prop(st, coll, StringKey(Named(adder_name)))
      case rt_call.is_callable(st, adder) {
        False ->
          rt_val.t_throw_type_error(
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
  use ref <- require_weak_map(st, this, "get")
  let key = first_arg_or_undefined(args)
  #(lookup_wm(st, ref, key) |> option.unwrap(mk_undefined()), st)
}

fn weak_map_set(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use ref <- require_weak_map(st, this, "set")
  let #(key, val) = two_args_or_undefined(args)
  use wk <- require_weak_key(st, key, "Invalid value used as weak map key")
  #(this, update_wm(st, ref, dict.insert(_, wk, val)))
}

fn weak_map_has(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use ref <- require_weak_map(st, this, "has")
  let key = first_arg_or_undefined(args)
  case to_weak_key(key) {
    Some(wk) -> #(mk_bool(dict.has_key(read_wm(st, ref), wk)), st)
    None -> #(mk_bool(False), st)
  }
}

fn weak_map_delete(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use ref <- require_weak_map(st, this, "delete")
  let key = first_arg_or_undefined(args)
  case to_weak_key(key) {
    Some(wk) ->
      case dict.has_key(read_wm(st, ref), wk) {
        True -> #(mk_bool(True), update_wm(st, ref, dict.delete(_, wk)))
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
  use ref <- require_weak_map(st, this, "getOrInsert")
  let key = first_arg_or_undefined(args)
  use wk <- require_weak_key(st, key, "Invalid value used as weak map key")
  case dict.get(read_wm(st, ref), wk) {
    Ok(existing) -> #(existing, st)
    Error(Nil) -> {
      let val = arg_at(args, 1)
      #(val, update_wm(st, ref, dict.insert(_, wk, val)))
    }
  }
}

// callback may insert the same key; re-read before writing
fn weak_map_get_or_insert_computed(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use ref <- require_weak_map(st, this, "getOrInsertComputed")
  let key = first_arg_or_undefined(args)
  use wk <- require_weak_key(st, key, "Invalid value used as weak map key")
  let callback = arg_at(args, 1)
  use callback <- helpers.require_callable(st, callback, fn() {
    let #(ty, _) = rt_val.t_type_of(st, callback)
    ty <> " is not a function"
  })
  case dict.get(read_wm(st, ref), wk) {
    Ok(existing) -> #(existing, st)
    Error(Nil) -> {
      let #(computed, st) =
        rt_call.t_call_checked(st, callback, mk_undefined(), [key])
      #(computed, update_wm(st, ref, dict.insert(_, wk, computed)))
    }
  }
}

fn weak_set_add(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use ref <- require_weak_set(st, this, "add")
  let val = first_arg_or_undefined(args)
  use wk <- require_weak_key(st, val, "Invalid value used in weak set")
  #(this, update_ws(st, ref, set.insert(_, wk)))
}

fn weak_set_has(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use ref <- require_weak_set(st, this, "has")
  let val = first_arg_or_undefined(args)
  case to_weak_key(val) {
    Some(wk) -> #(mk_bool(set.contains(read_ws(st, ref), wk)), st)
    None -> #(mk_bool(False), st)
  }
}

fn weak_set_delete(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use ref <- require_weak_set(st, this, "delete")
  let val = first_arg_or_undefined(args)
  case to_weak_key(val) {
    Some(wk) ->
      case set.contains(read_ws(st, ref), wk) {
        True -> #(mk_bool(True), update_ws(st, ref, set.delete(_, wk)))
        False -> #(mk_bool(False), st)
      }
    None -> #(mk_bool(False), st)
  }
}

type WMRef {
  WMRef(Handle)
}

type WSRef {
  WSRef(Handle)
}

fn require_weak_map(
  st: Agent,
  this: JsVal,
  method: String,
  cont: fn(WMRef) -> #(JsVal, Agent),
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
  cont(WMRef(h))
}

fn require_weak_set(
  st: Agent,
  this: JsVal,
  method: String,
  cont: fn(WSRef) -> #(JsVal, Agent),
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
  cont(WSRef(h))
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
    None -> rt_val.t_throw_type_error(st, msg)
  }
}

fn read_wm(st: Agent, ref: WMRef) -> Dict(WeakKey, JsVal) {
  let WMRef(h) = ref
  let assert SObject(kind: WeakMapObj(entries:), ..) =
    rt_store.t_cell_get(st, h)
    as "weak: WMRef does not point at a WeakMap slot"
  entries
}

fn lookup_wm(st: Agent, ref: WMRef, key: JsVal) -> Option(JsVal) {
  use wk <- option.then(to_weak_key(key))
  dict.get(read_wm(st, ref), wk) |> option.from_result
}

// takes a fn so callers cannot write back a stale dict
fn update_wm(
  st: Agent,
  ref: WMRef,
  f: fn(Dict(WeakKey, JsVal)) -> Dict(WeakKey, JsVal),
) -> Agent {
  let WMRef(h) = ref
  rt_store.t_cell_update(st, h, fn(slot) {
    let assert SObject(kind: WeakMapObj(entries:), ..) = slot
    SObject(..slot, kind: WeakMapObj(entries: f(entries)))
  })
}

fn read_ws(st: Agent, ref: WSRef) -> Set(WeakKey) {
  let WSRef(h) = ref
  let assert SObject(kind: WeakSetObj(entries:), ..) =
    rt_store.t_cell_get(st, h)
    as "weak: WSRef does not point at a WeakSet slot"
  entries
}

fn update_ws(
  st: Agent,
  ref: WSRef,
  f: fn(Set(WeakKey)) -> Set(WeakKey),
) -> Agent {
  let WSRef(h) = ref
  rt_store.t_cell_update(st, h, fn(slot) {
    let assert SObject(kind: WeakSetObj(entries:), ..) = slot
    SObject(..slot, kind: WeakSetObj(entries: f(entries)))
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
