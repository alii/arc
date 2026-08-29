import arc/bytecode/key.{type Key}
import arc/rt/call as rt_call
import arc/rt/name_keys as nk
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type FnFlags, type Handle, type JsOps, type JsVal,
  type MethodInstallKind, type ObjectKey, type Property, AccessorProperty,
  DataProperty, KBytecode, KCompiled, KHandle, KNull, KTdz, MIGetter, MIMethod,
  MISetter, MIStatic, MIStaticGetter, MIStaticSetter, SObject, StringKey,
  SymbolKey, classify, mk_object, mk_undefined,
} as rt_types
import gleam/dict
import gleam/option.{type Option, None, Some}

fn js_ops(st: Agent) -> JsOps(Agent) {
  st.store.ops
}

fn throw_type_error(st: Agent, msg: String) -> a {
  let #(e, st) = js_ops(st).new_error(st, rt_types.TypeErr, msg)
  rt_store.t_throw(st, e)
}

// a private name value is its key as a js integer
fn private_key_of(v: JsVal) -> Key {
  case classify(v) {
    rt_types.KNum(rt_types.JInt(k)) -> k
    _ -> panic as "rt_class: private name value is not a key"
  }
}

fn private_display_name(st: Agent, k: Key) -> String {
  rt_store.t_key_text(st, k)
}

// §15.7.14 mint a fresh private name
pub fn t_new_private_name(st: Agent, source: String) -> #(JsVal, Agent) {
  let #(k, st) = rt_store.t_new_private_key(st, source)
  #(rt_types.mk_int(k), st)
}

// §15.4.4 makemethod, no-op on native/bound
pub fn t_make_method(st: Agent, fn_h: Handle, home: Handle) -> Agent {
  rt_store.t_cell_update(st, fn_h, fn(slot) {
    case slot {
      SObject(kind: KCompiled(..) as k, ..) ->
        SObject(..slot, kind: KCompiled(..k, home_object: Some(home)))
      SObject(kind: KBytecode(..) as k, ..) ->
        SObject(..slot, kind: KBytecode(..k, home_object: Some(home)))
      _ -> slot
    }
  })
}

pub fn t_set_fields_init(st: Agent, ctor: Handle, init_h: Handle) -> Agent {
  rt_store.t_cell_update(st, ctor, fn(slot) {
    case slot {
      SObject(kind: KCompiled(..) as k, ..) ->
        SObject(..slot, kind: KCompiled(..k, fields_init: Some(init_h)))
      SObject(kind: KBytecode(..) as k, ..) ->
        SObject(..slot, kind: KBytecode(..k, fields_init: Some(init_h)))
      _ -> slot
    }
  })
}

// super: tdz = no extends, null = extends null, handle = extends h
fn class_heritage(st: Agent, super: JsVal) -> #(Option(Handle), Handle, Agent) {
  let realm = st.realm
  case classify(super) {
    KTdz -> #(Some(realm.object.prototype), realm.function.prototype, st)
    KNull -> #(None, realm.function.prototype, st)
    KHandle(parent_h) ->
      case rt_call.is_constructor(st, super) {
        False ->
          throw_type_error(
            st,
            "Class extends value is not a constructor or null",
          )
        True -> {
          let #(pp, st) = rt_obj.t_get_prop(st, super, StringKey(nk.prototype))
          case classify(pp) {
            KHandle(pph) -> #(Some(pph), parent_h, st)
            KNull -> #(None, parent_h, st)
            _ ->
              throw_type_error(
                st,
                "Class extends value does not have valid prototype property",
              )
          }
        }
      }
    _ ->
      throw_type_error(st, "Class extends value is not a constructor or null")
  }
}

// §15.7.14 steps 8-18
pub fn t_class_setup(
  st: Agent,
  ctor: Handle,
  super: JsVal,
) -> #(Handle, Agent) {
  let #(proto_parent, ctor_parent, st) = class_heritage(st, super)
  let #(proto, st) = rt_obj.t_new_object(st, proto_parent)
  let st =
    rt_store.t_cell_update(st, ctor, fn(slot) {
      case slot {
        SObject(kind: KCompiled(..) as k, ..) ->
          SObject(
            ..slot,
            kind: KCompiled(..k, home_object: Some(proto)),
            proto: Some(ctor_parent),
          )
        SObject(kind: KBytecode(..) as k, ..) ->
          SObject(
            ..slot,
            kind: KBytecode(..k, home_object: Some(proto)),
            proto: Some(ctor_parent),
          )
        _ -> slot
      }
    })
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      ctor,
      StringKey(nk.prototype),
      mk_object(proto),
      False,
      False,
      False,
    )
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      proto,
      StringKey(nk.constructor),
      mk_object(ctor),
      True,
      False,
      True,
    )
  #(proto, st)
}

// §14.3.9; enumerable for object literals, not classes
pub fn t_define_method(
  st: Agent,
  target: Handle,
  key: ObjectKey,
  fn_h: Handle,
  kind: MethodInstallKind,
  enumerable: Bool,
) -> Agent {
  let _ = case rt_obj.t_ordinary_own_property(st, target, key) {
    Some(prop) ->
      case rt_types.prop_configurable(prop) {
        False ->
          throw_type_error(
            st,
            "Cannot redefine property: " <> rt_obj.key_text(st, key),
          )
        True -> Nil
      }
    None -> Nil
  }
  let st = t_make_method(st, fn_h, target)
  // only rename when compiled anonymous (computed key)
  let prefix = case kind {
    MIGetter | MIStaticGetter -> "get "
    MISetter | MIStaticSetter -> "set "
    MIMethod | MIStatic -> ""
  }
  let st = set_fn_name_if_empty(st, fn_h, prefix, key_fn_name(st, key))
  let fn_v = mk_object(fn_h)
  case kind {
    MIMethod | MIStatic -> {
      let #(_, st) =
        rt_obj.t_define_own_data(st, target, key, fn_v, True, enumerable, True)
      st
    }
    MIGetter | MIStaticGetter -> {
      let #(_, st) =
        rt_obj.t_define_own_accessor(
          st,
          target,
          key,
          Some(fn_v),
          None,
          enumerable,
          True,
        )
      st
    }
    MISetter | MIStaticSetter -> {
      let #(_, st) =
        rt_obj.t_define_own_accessor(
          st,
          target,
          key,
          None,
          Some(fn_v),
          enumerable,
          True,
        )
      st
    }
  }
}

// symbol key names the fn "[description]"
fn key_fn_name(st: Agent, key: ObjectKey) -> String {
  case key {
    StringKey(pk) -> rt_store.t_key_text(st, pk)
    SymbolKey(sym) ->
      case rt_types.symbol_description(sym) {
        Some(d) -> "[" <> d <> "]"
        None -> ""
      }
  }
}

fn set_fn_name_if_empty(
  st: Agent,
  fn_h: Handle,
  prefix: String,
  name: String,
) -> Agent {
  rt_obj.t_name_if_anonymous(st, fn_h, prefix <> name)
}

// §7.3.28 privatefieldadd, bypasses defineownproperty
pub fn t_private_define(
  st: Agent,
  obj: Handle,
  priv_key: JsVal,
  v: JsVal,
) -> Agent {
  let key = private_key_of(priv_key)
  let st = check_private_add(st, obj, key)
  raw_define_private_data(st, obj, key, v, True)
}

// §7.3.29; home_object already set at class definition
pub fn t_define_private(
  st: Agent,
  obj: Handle,
  priv_key: JsVal,
  fn_v: JsVal,
  kind: MethodInstallKind,
) -> Agent {
  let key = private_key_of(priv_key)
  case kind {
    // non-writable so private set rejects methods
    MIMethod | MIStatic -> {
      let st = check_private_add(st, obj, key)
      raw_define_private_data(st, obj, key, fn_v, False)
    }
    // same accessor half twice is a typeerror
    MIGetter | MIStaticGetter | MISetter | MIStaticSetter -> {
      let is_getter = case kind {
        MIGetter | MIStaticGetter -> True
        _ -> False
      }
      let existing = rt_obj.t_ordinary_own_property(st, obj, StringKey(key))
      let st = case existing {
        None -> check_private_add(st, obj, key)
        Some(AccessorProperty(get:, set:, ..)) ->
          case
            is_getter
            && option.is_some(get)
            || !is_getter
            && option.is_some(set)
          {
            True -> throw_private_double_init(st, key, "private accessor ")
            False -> st
          }
        Some(DataProperty(..)) ->
          throw_private_double_init(st, key, "private accessor ")
      }
      raw_merge_private_accessor(st, obj, key, existing, fn_v, is_getter)
    }
  }
}

fn check_private_add(st: Agent, obj: Handle, key: Key) -> Agent {
  case rt_obj.t_ordinary_own_property(st, obj, StringKey(key)) {
    Some(_) -> throw_private_double_init(st, key, "")
    None ->
      case rt_obj.t_ordinary_is_extensible(st, obj) {
        False ->
          throw_type_error(
            st,
            "Cannot define private member "
              <> private_display_name(st, key)
              <> " on a non-extensible object",
          )
        True -> st
      }
  }
}

fn throw_private_double_init(st: Agent, key: Key, kind: String) -> a {
  throw_type_error(
    st,
    "Cannot initialize "
      <> kind
      <> private_display_name(st, key)
      <> " twice on the same object",
  )
}

fn raw_define_private_data(
  st: Agent,
  obj: Handle,
  key: Key,
  v: JsVal,
  writable: Bool,
) -> Agent {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  rt_store.t_cell_update(st, obj, fn(slot) {
    let assert SObject(props:, ..) as slot = rt_obj.as_sobject(slot)
      as "t_define_private target is not an SObject"
    SObject(
      ..slot,
      props: dict.insert(
        props,
        key,
        DataProperty(
          value: v,
          writable:,
          enumerable: False,
          configurable: True,
          seq:,
        ),
      ),
    )
  })
}

fn raw_merge_private_accessor(
  st: Agent,
  obj: Handle,
  key: Key,
  existing: Option(Property),
  fn_v: JsVal,
  is_getter: Bool,
) -> Agent {
  let #(seq, st) = case existing {
    Some(old) -> #(rt_types.prop_seq(old), st)
    None -> rt_store.t_next_prop_seq(st)
  }
  let #(get, set) = case existing {
    Some(AccessorProperty(get:, set:, ..)) -> #(get, set)
    _ -> #(None, None)
  }
  let #(get, set) = case is_getter {
    True -> #(Some(fn_v), set)
    False -> #(get, Some(fn_v))
  }
  rt_store.t_cell_update(st, obj, fn(slot) {
    let assert SObject(props:, ..) as slot = rt_obj.as_sobject(slot)
      as "t_define_private target is not an SObject"
    SObject(
      ..slot,
      props: dict.insert(
        props,
        key,
        AccessorProperty(
          get:,
          set:,
          enumerable: False,
          configurable: True,
          seq:,
        ),
      ),
    )
  })
}

// §7.3.30 privateget, getter may re-enter js
pub fn t_private_get(
  st: Agent,
  obj: JsVal,
  priv_key: JsVal,
) -> #(JsVal, Agent) {
  let key = private_key_of(priv_key)
  let name = fn() { private_display_name(st, key) }
  case classify(obj) {
    KHandle(h) ->
      case rt_obj.t_ordinary_own_property(st, h, StringKey(key)) {
        Some(DataProperty(value:, ..)) -> #(value, st)
        Some(AccessorProperty(get: Some(getter), ..)) ->
          js_ops(st).call(st, getter, obj, [])
        Some(AccessorProperty(get: None, ..)) ->
          throw_type_error(
            st,
            "'" <> name() <> "' was defined without a getter",
          )
        None ->
          throw_type_error(
            st,
            "Cannot read private member "
              <> name()
              <> " from an object whose class did not declare it",
          )
      }
    _ ->
      throw_type_error(
        st,
        "Cannot read private member " <> name() <> " on non-object",
      )
  }
}

// §7.3.31 privateset
pub fn t_private_set(
  st: Agent,
  obj: JsVal,
  priv_key: JsVal,
  v: JsVal,
) -> #(JsVal, Agent) {
  let key = private_key_of(priv_key)
  let name = fn() { private_display_name(st, key) }
  case classify(obj) {
    KHandle(h) ->
      case rt_obj.t_ordinary_own_property(st, h, StringKey(key)) {
        Some(DataProperty(writable: True, ..)) -> {
          let st =
            rt_store.t_cell_update(st, h, fn(slot) {
              let assert SObject(props:, ..) = slot
              case dict.get(props, key) {
                Ok(DataProperty(seq:, writable:, enumerable:, configurable:, ..)) ->
                  SObject(
                    ..slot,
                    props: dict.insert(
                      props,
                      key,
                      DataProperty(
                        value: v,
                        writable:,
                        enumerable:,
                        configurable:,
                        seq:,
                      ),
                    ),
                  )
                _ -> slot
              }
            })
          #(v, st)
        }
        Some(AccessorProperty(set: Some(setter), ..)) -> {
          let #(_, st) = js_ops(st).call(st, setter, obj, [v])
          #(v, st)
        }
        Some(DataProperty(writable: False, ..))
        | Some(AccessorProperty(set: None, ..)) ->
          throw_type_error(
            st,
            "Cannot write private member "
              <> name()
              <> ": it is a method or has no setter",
          )
        None ->
          throw_type_error(
            st,
            "Cannot write private member "
              <> name()
              <> " to an object whose class did not declare it",
          )
      }
    _ ->
      throw_type_error(
        st,
        "Cannot write private member " <> name() <> " on non-object",
      )
  }
}

// §13.10.1 #x in obj
pub fn t_private_in(st: Agent, obj: JsVal, priv_key: JsVal) -> Bool {
  let key = private_key_of(priv_key)
  case classify(obj) {
    KHandle(h) ->
      option.is_some(rt_obj.t_ordinary_own_property(st, h, StringKey(key)))
    _ ->
      throw_type_error(
        st,
        "Cannot use 'in' operator to search for private name "
          <> private_display_name(st, key)
          <> " in non-object",
      )
  }
}

// super.key read on home.[[prototype]] with receiver as this
// aot passes wire keys
pub fn t_define_method_any(
  st: Agent,
  target: Handle,
  key: k,
  fn_h: Handle,
  kind: MethodInstallKind,
  enumerable: Bool,
) -> Agent {
  let #(key, st) = rt_obj.as_object_key(st, key)
  t_define_method(st, target, key, fn_h, kind, enumerable)
}

pub fn t_super_get_any(
  st: Agent,
  home: Handle,
  receiver: JsVal,
  key: k,
) -> #(JsVal, Agent) {
  let #(key, st) = rt_obj.as_object_key(st, key)
  t_super_get(st, home, receiver, key)
}

pub fn t_super_set_any(
  st: Agent,
  home: Handle,
  receiver: JsVal,
  key: k,
  v: JsVal,
  strict strict: Bool,
) -> #(JsVal, Agent) {
  let #(key, st) = rt_obj.as_object_key(st, key)
  t_super_set(st, home, receiver, key, v, strict)
}

pub fn t_super_get(
  st: Agent,
  home: Handle,
  receiver: JsVal,
  key: ObjectKey,
) -> #(JsVal, Agent) {
  case rt_obj.t_get_proto(st, home) {
    #(Some(base), st) ->
      rt_obj.t_get_prop_with_receiver(st, base, key, receiver)
    #(None, st) ->
      throw_type_error(st, "Cannot read super property when prototype is null")
  }
}

// failure throws only when strict
pub fn t_super_set(
  st: Agent,
  home: Handle,
  receiver: JsVal,
  key: ObjectKey,
  v: JsVal,
  strict strict: Bool,
) -> #(JsVal, Agent) {
  case rt_obj.t_get_proto(st, home) {
    #(Some(base), st) -> {
      let #(ok, st) =
        rt_obj.t_set_prop_with_receiver(st, base, key, v, receiver)
      case ok || !strict {
        True -> #(v, st)
        False ->
          throw_type_error(st, "Cannot assign to read-only super property")
      }
    }
    #(None, st) ->
      throw_type_error(st, "Cannot write super property when prototype is null")
  }
}

// §13.3.7.1 supercall
pub fn t_super_call(
  st: Agent,
  active_func: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case rt_obj.t_get_proto(st, active_func) {
    #(Some(parent), st) ->
      rt_call.t_construct(st, mk_object(parent), args, new_target)
    // null proto means setprototypeof(ctor, null): typeerror
    #(None, st) ->
      throw_type_error(
        st,
        "Super constructor null of derived class is not a constructor",
      )
  }
}

pub fn t_fn_home_object(st: Agent, fn_h: Handle) -> JsVal {
  case rt_store.t_cell_get(st, fn_h) {
    SObject(kind: KCompiled(home_object: Some(h), ..), ..)
    | SObject(kind: KBytecode(home_object: Some(h), ..), ..) -> mk_object(h)
    _ -> mk_undefined()
  }
}

// panics on native/bound, emitter guarantees a closure
pub fn t_fn_flags(st: Agent, fn_h: Handle) -> FnFlags {
  case rt_store.t_cell_get(st, fn_h) {
    SObject(kind: KCompiled(flags:, ..), ..)
    | SObject(kind: KBytecode(flags:, ..), ..) -> flags
    _ -> panic as "t_fn_flags: Handle is not a function closure cell"
  }
}

pub fn t_is_constructor(st: Agent, v: JsVal) -> Bool {
  rt_call.is_constructor(st, v)
}
