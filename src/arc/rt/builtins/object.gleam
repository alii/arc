import arc/bytecode/key
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{first_arg_or_undefined, two_args_or_undefined}
import arc/rt/builtins/iter_protocol
import arc/rt/call as rt_call
import arc/rt/js_string
import arc/rt/name_keys as nk
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type ObjectKey,
  type ObjectNative, type ParsedDesc, type Property, AccessorProperty,
  ArgumentsObj, BooleanObj, DataProperty, DateObj, ErrorObj, JInt, KBig, KBool,
  KBytecode, KCompiled, KHandle, KNative, KNull, KNum, KStr, KSym, KUndef,
  NumberObj, ObjectAssign, ObjectConstructor, ObjectCreate,
  ObjectDefineProperties, ObjectDefineProperty, ObjectEntries, ObjectFreeze,
  ObjectFromEntries, ObjectGetOwnPropertyDescriptor,
  ObjectGetOwnPropertyDescriptors, ObjectGetOwnPropertyNames,
  ObjectGetOwnPropertySymbols, ObjectGetPrototypeOf, ObjectGroupBy, ObjectHasOwn,
  ObjectIs, ObjectIsExtensible, ObjectIsFrozen, ObjectIsSealed, ObjectKeys,
  ObjectN, ObjectPreventExtensions, ObjectPrototypeDefineGetter,
  ObjectPrototypeDefineSetter, ObjectPrototypeHasOwnProperty,
  ObjectPrototypeIsPrototypeOf, ObjectPrototypeLookupGetter,
  ObjectPrototypeLookupSetter, ObjectPrototypePropertyIsEnumerable,
  ObjectPrototypeProtoGetter, ObjectPrototypeProtoSetter,
  ObjectPrototypeToLocaleString, ObjectPrototypeToString, ObjectPrototypeValueOf,
  ObjectSeal, ObjectSetPrototypeOf, ObjectValues, ParsedDesc, ProxyObj,
  RegExpObj, SObject, SShapedObject, StringKey, StringObj, SymbolKey, classify,
  mk_bool, mk_null, mk_number, mk_object, mk_string, mk_symbol, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

const cannot_convert = "Cannot convert undefined or null to object"

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(static_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("getOwnPropertyDescriptor", ObjectN(ObjectGetOwnPropertyDescriptor), 2),
      #("defineProperty", ObjectN(ObjectDefineProperty), 3),
      #("defineProperties", ObjectN(ObjectDefineProperties), 2),
      #("getOwnPropertyNames", ObjectN(ObjectGetOwnPropertyNames), 1),
      #("keys", ObjectN(ObjectKeys), 1),
      #("values", ObjectN(ObjectValues), 1),
      #("entries", ObjectN(ObjectEntries), 1),
      #("create", ObjectN(ObjectCreate), 2),
      #("assign", ObjectN(ObjectAssign), 2),
      #("is", ObjectN(ObjectIs), 2),
      #("hasOwn", ObjectN(ObjectHasOwn), 2),
      #("getPrototypeOf", ObjectN(ObjectGetPrototypeOf), 1),
      #("setPrototypeOf", ObjectN(ObjectSetPrototypeOf), 2),
      #("freeze", ObjectN(ObjectFreeze), 1),
      #("isFrozen", ObjectN(ObjectIsFrozen), 1),
      #("isExtensible", ObjectN(ObjectIsExtensible), 1),
      #("preventExtensions", ObjectN(ObjectPreventExtensions), 1),
      #("fromEntries", ObjectN(ObjectFromEntries), 1),
      #("seal", ObjectN(ObjectSeal), 1),
      #("isSealed", ObjectN(ObjectIsSealed), 1),
      #(
        "getOwnPropertyDescriptors",
        ObjectN(ObjectGetOwnPropertyDescriptors),
        1,
      ),
      #("getOwnPropertySymbols", ObjectN(ObjectGetOwnPropertySymbols), 1),
      #("groupBy", ObjectN(ObjectGroupBy), 2),
    ])
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("hasOwnProperty", ObjectN(ObjectPrototypeHasOwnProperty), 1),
      #("propertyIsEnumerable", ObjectN(ObjectPrototypePropertyIsEnumerable), 1),
      #("toString", ObjectN(ObjectPrototypeToString), 0),
      #("valueOf", ObjectN(ObjectPrototypeValueOf), 0),
      #("isPrototypeOf", ObjectN(ObjectPrototypeIsPrototypeOf), 1),
      #("toLocaleString", ObjectN(ObjectPrototypeToLocaleString), 0),
      #("__defineGetter__", ObjectN(ObjectPrototypeDefineGetter), 2),
      #("__defineSetter__", ObjectN(ObjectPrototypeDefineSetter), 2),
      #("__lookupGetter__", ObjectN(ObjectPrototypeLookupGetter), 1),
      #("__lookupSetter__", ObjectN(ObjectPrototypeLookupSetter), 1),
    ])
  let #(proto_accessor, st) =
    common.alloc_get_set_accessor(
      st,
      fn_proto,
      ObjectN(ObjectPrototypeProtoGetter),
      ObjectN(ObjectPrototypeProtoSetter),
      "__proto__",
    )
  let proto_methods = [#("__proto__", proto_accessor), ..proto_methods]
  common.init_type_on(
    st,
    object_proto,
    fn_proto,
    proto_methods,
    fn(_) { ObjectN(ObjectConstructor) },
    "Object",
    1,
    static_methods,
    True,
  )
}

pub fn dispatch(
  st: Agent,
  native: ObjectNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    ObjectConstructor -> object_ctor(st, args)
    ObjectGetOwnPropertyDescriptor -> get_own_prop_desc(st, args)
    ObjectDefineProperty -> define_property(st, args)
    ObjectDefineProperties -> define_properties(st, args)
    ObjectGetOwnPropertyNames -> own_keys_impl(st, args, False)
    ObjectKeys -> own_keys_impl(st, args, True)
    ObjectValues -> values(st, args)
    ObjectEntries -> entries(st, args)
    ObjectCreate -> create(st, args)
    ObjectAssign -> assign(st, args)
    ObjectIs -> object_is(st, args)
    ObjectHasOwn -> has_own(st, args)
    ObjectGetPrototypeOf -> get_prototype_of(st, args)
    ObjectSetPrototypeOf -> set_prototype_of(st, args)
    ObjectFreeze -> set_integrity_level(st, args, Frozen)
    ObjectIsFrozen -> test_integrity_level(st, args, Frozen)
    ObjectIsExtensible -> is_extensible(st, args)
    ObjectPreventExtensions -> prevent_extensions(st, args)
    ObjectPrototypeHasOwnProperty -> has_own_property(st, this, args)
    ObjectPrototypePropertyIsEnumerable ->
      property_is_enumerable(st, this, args)
    ObjectPrototypeToString -> object_to_string(st, this)
    ObjectPrototypeValueOf -> object_value_of(st, this)
    ObjectFromEntries -> from_entries(st, args)
    ObjectSeal -> set_integrity_level(st, args, Sealed)
    ObjectIsSealed -> test_integrity_level(st, args, Sealed)
    ObjectGetOwnPropertyDescriptors -> get_own_prop_descriptors(st, args)
    ObjectGetOwnPropertySymbols -> get_own_prop_symbols(st, args)
    ObjectPrototypeIsPrototypeOf -> is_prototype_of(st, this, args)
    ObjectPrototypeToLocaleString -> object_to_locale_string(st, this)
    ObjectGroupBy -> group_by(st, args)
    ObjectPrototypeDefineGetter ->
      define_getter_setter(st, this, args, AsGetter)
    ObjectPrototypeDefineSetter ->
      define_getter_setter(st, this, args, AsSetter)
    ObjectPrototypeLookupGetter ->
      lookup_getter_setter(st, this, args, AsGetter)
    ObjectPrototypeLookupSetter ->
      lookup_getter_setter(st, this, args, AsSetter)
    ObjectPrototypeProtoGetter -> get_prototype_of(st, [this])
    ObjectPrototypeProtoSetter -> proto_setter(st, this, args)
  }
}

pub fn dispatch_construct(
  st: Agent,
  n: ObjectNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case n {
    ObjectConstructor -> {
      let r = st.realm
      case classify(new_target) {
        KHandle(nt_h) if nt_h != r.object.constructor -> {
          let #(proto, st) =
            rt_call.get_prototype_from_constructor(
              st,
              new_target,
              rt_call.object_prototype,
            )
          rt_obj.t_new_object(st, Some(proto))
        }
        _ -> {
          let #(v, st) = object_ctor(st, args)
          let assert KHandle(h) = classify(v)
          #(h, st)
        }
      }
    }
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

fn object_ctor(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let object_proto = st.realm.object.prototype
  let arg = first_arg_or_undefined(args)
  case classify(arg) {
    KHandle(_) -> #(arg, st)
    KUndef | KNull -> {
      let #(h, st) = rt_obj.t_new_object(st, Some(object_proto))
      #(mk_object(h), st)
    }
    _ -> {
      let #(h, st) = rt_val.t_to_object(st, arg)
      #(mk_object(h), st)
    }
  }
}

fn get_own_prop_desc(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(target, key_val) = two_args_or_undefined(args)
  case classify(target) {
    KNull | KUndef -> rt_val.t_throw_type_error(st, cannot_convert)
    KHandle(h) -> {
      let #(desc, st) = own_property_by_value(st, h, key_val)
      case desc {
        Some(prop) -> from_property_descriptor(st, prop)
        None -> #(mk_undefined(), st)
      }
    }
    KStr(s) -> {
      let #(key, st) = rt_val.t_find_property_key(st, key_val)
      case string_own_property(s, key) {
        Some(prop) -> from_property_descriptor(st, prop)
        None -> #(mk_undefined(), st)
      }
    }
    _ -> {
      let #(_key, st) = rt_val.t_to_property_key(st, key_val)
      #(mk_undefined(), st)
    }
  }
}

fn from_property_descriptor(st: Agent, prop: Property) -> #(JsVal, Agent) {
  let #(h, st) =
    rt_obj.t_from_property_descriptor(st, rt_obj.parsed_of_property(prop))
  #(mk_object(h), st)
}

fn define_property(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case args {
    [obj, ..rest] ->
      case classify(obj) {
        KHandle(h) -> {
          let key_val = first_arg_or_undefined(rest)
          let desc_val = helpers.arg_at(rest, 1)
          let #(key, st) = rt_val.t_to_property_key(st, key_val)
          let #(parsed, st) = rt_obj.t_to_property_descriptor(st, desc_val)
          let #(ok, st) = rt_obj.t_define_own_prop(st, h, key, parsed)
          case ok {
            True -> #(obj, st)
            False ->
              rt_val.t_throw_type_error(
                st,
                "Cannot define property " <> key_text(st, key),
              )
          }
        }
        _ ->
          rt_val.t_throw_type_error(
            st,
            "Object.defineProperty called on non-object",
          )
      }
    [] ->
      rt_val.t_throw_type_error(
        st,
        "Object.defineProperty called on non-object",
      )
  }
}

fn define_properties(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(target, props_val) = two_args_or_undefined(args)
  case classify(target) {
    KHandle(h) -> define_properties_on(st, h, props_val)
    _ ->
      rt_val.t_throw_type_error(
        st,
        "Object.defineProperties called on non-object",
      )
  }
}

fn define_properties_on(
  st: Agent,
  target_h: Handle,
  props_val: JsVal,
) -> #(JsVal, Agent) {
  case classify(props_val) {
    KHandle(props_h) -> {
      let #(keys, st) = rt_obj.t_own_keys(st, props_h)
      let #(descs, st) =
        collect_descriptors(st, props_h, mk_object(props_h), keys, [])
      apply_descriptors(st, target_h, descs)
    }
    KNull | KUndef -> rt_val.t_throw_type_error(st, cannot_convert)
    KStr("") -> #(mk_object(target_h), st)
    KStr(_) ->
      rt_val.t_throw_type_error(st, "Property description must be an object")
    _ -> #(mk_object(target_h), st)
  }
}

fn collect_descriptors(
  st: Agent,
  props_h: Handle,
  props_v: JsVal,
  keys: List(ObjectKey),
  acc: List(#(ObjectKey, ParsedDesc)),
) -> #(List(#(ObjectKey, ParsedDesc)), Agent) {
  case keys {
    [] -> #(list.reverse(acc), st)
    [k, ..rest] -> {
      let #(prop, st) = rt_obj.t_get_own_property(st, props_h, k)
      let enumerable =
        option.map(prop, rt_types.prop_enumerable) |> option.unwrap(False)
      case enumerable {
        False -> collect_descriptors(st, props_h, props_v, rest, acc)
        True -> {
          let #(desc_val, st) = rt_obj.t_get_prop(st, props_v, k)
          let #(parsed, st) = rt_obj.t_to_property_descriptor(st, desc_val)
          collect_descriptors(st, props_h, props_v, rest, [#(k, parsed), ..acc])
        }
      }
    }
  }
}

fn apply_descriptors(
  st: Agent,
  target_h: Handle,
  descs: List(#(ObjectKey, ParsedDesc)),
) -> #(JsVal, Agent) {
  case descs {
    [] -> #(mk_object(target_h), st)
    [#(k, parsed), ..rest] -> {
      let #(ok, st) = rt_obj.t_define_own_prop(st, target_h, k, parsed)
      case ok {
        True -> apply_descriptors(st, target_h, rest)
        False ->
          rt_val.t_throw_type_error(
            st,
            "Cannot define property " <> key_text(st, k),
          )
      }
    }
  }
}

fn own_keys_impl(
  st: Agent,
  args: List(JsVal),
  enumerable_only: Bool,
) -> #(JsVal, Agent) {
  case classify(first_arg_or_undefined(args)) {
    KHandle(h) -> {
      let #(names, st) = case enumerable_only {
        True -> rt_obj.t_enumerable_own_keys(st, h)
        False -> {
          let #(keys, st) = rt_obj.t_own_keys(st, h)
          let names =
            list.filter_map(keys, fn(k) {
              case k {
                StringKey(pk) -> Ok(pk)
                SymbolKey(_) -> Error(Nil)
              }
            })
          #(names, st)
        }
      }
      ok_array(st, list.map(names, rt_store.t_key_value(st, _)))
    }
    KNull | KUndef -> rt_val.t_throw_type_error(st, cannot_convert)
    KStr(s) -> {
      let index_keys = string_index_keys(0, js_string.length(s))
      let ks = case enumerable_only {
        True -> index_keys
        False -> list.append(index_keys, [mk_string("length")])
      }
      ok_array(st, ks)
    }
    _ -> ok_array(st, [])
  }
}

fn values(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(pairs, st) = own_enumerable_pairs(st, args)
  ok_array(st, list.map(pairs, fn(kv) { kv.1 }))
}

fn entries(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  let #(pairs, st) = own_enumerable_pairs(st, args)
  let #(rows, st) =
    list.fold(pairs, #([], st), fn(acc, kv) {
      let #(rows, st) = acc
      let #(k, v) = kv
      let #(row_h, st) = common.alloc_array(st, [mk_string(k), v], array_proto)
      #([mk_object(row_h), ..rows], st)
    })
  ok_array(st, list.reverse(rows))
}

fn own_enumerable_pairs(
  st: Agent,
  args: List(JsVal),
) -> #(List(#(String, JsVal)), Agent) {
  case classify(first_arg_or_undefined(args)) {
    KHandle(h) -> {
      let #(keys, st) = rt_obj.t_own_keys(st, h)
      collect_enumerable(st, h, keys, [])
    }
    KNull | KUndef -> rt_val.t_throw_type_error(st, cannot_convert)
    KStr(s) -> #(
      list.index_map(js_string.explode(s), fn(ch, idx) {
        #(int.to_string(idx), mk_string(ch))
      }),
      st,
    )
    _ -> #([], st)
  }
}

fn collect_enumerable(
  st: Agent,
  h: Handle,
  keys: List(ObjectKey),
  acc: List(#(String, JsVal)),
) -> #(List(#(String, JsVal)), Agent) {
  case keys {
    [] -> #(list.reverse(acc), st)
    [SymbolKey(_), ..rest] -> collect_enumerable(st, h, rest, acc)
    [StringKey(pk) as k, ..rest] -> {
      let #(prop, st) = rt_obj.t_get_own_property(st, h, k)
      let enumerable =
        option.map(prop, rt_types.prop_enumerable) |> option.unwrap(False)
      case enumerable {
        False -> collect_enumerable(st, h, rest, acc)
        True -> {
          let #(v, st) = rt_obj.t_get_prop(st, mk_object(h), k)
          collect_enumerable(st, h, rest, [
            #(rt_store.t_key_text(st, pk), v),
            ..acc
          ])
        }
      }
    }
  }
}

fn get_own_prop_symbols(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(first_arg_or_undefined(args)) {
    KNull | KUndef -> rt_val.t_throw_type_error(st, cannot_convert)
    KHandle(h) -> {
      let #(keys, st) = rt_obj.t_own_keys(st, h)
      let syms =
        list.filter_map(keys, fn(k) {
          case k {
            SymbolKey(sym) -> Ok(mk_symbol(sym))
            StringKey(_) -> Error(Nil)
          }
        })
      ok_array(st, syms)
    }
    _ -> ok_array(st, [])
  }
}

fn get_own_prop_descriptors(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let object_proto = st.realm.object.prototype
  case classify(first_arg_or_undefined(args)) {
    KNull | KUndef -> rt_val.t_throw_type_error(st, cannot_convert)
    KHandle(h) -> {
      let #(keys, st) = rt_obj.t_own_keys(st, h)
      let #(result_h, st) = rt_obj.t_new_object(st, Some(object_proto))
      descriptors_from_keys(st, h, result_h, keys)
    }
    KStr(s) -> {
      let keys =
        list.append(string_index_object_keys(0, js_string.length(s)), [
          StringKey(nk.length),
        ])
      let #(result_h, st) = rt_obj.t_new_object(st, Some(object_proto))
      let st =
        list.fold(keys, st, fn(st, k) {
          case string_exotic_own_property(s, k) {
            None -> st
            Some(prop) -> {
              let #(desc_v, st) = from_property_descriptor(st, prop)
              let #(_ok, st) =
                rt_obj.t_define_own_data(
                  st,
                  result_h,
                  k,
                  desc_v,
                  True,
                  True,
                  True,
                )
              st
            }
          }
        })
      #(mk_object(result_h), st)
    }
    _ -> {
      let #(result_h, st) = rt_obj.t_new_object(st, Some(object_proto))
      #(mk_object(result_h), st)
    }
  }
}

fn descriptors_from_keys(
  st: Agent,
  src_h: Handle,
  result_h: Handle,
  keys: List(ObjectKey),
) -> #(JsVal, Agent) {
  case keys {
    [] -> #(mk_object(result_h), st)
    [k, ..rest] -> {
      let #(desc, st) = rt_obj.t_get_own_property(st, src_h, k)
      case desc {
        None -> descriptors_from_keys(st, src_h, result_h, rest)
        Some(prop) -> {
          let #(desc_v, st) = from_property_descriptor(st, prop)
          let #(_ok, st) =
            rt_obj.t_define_own_data(st, result_h, k, desc_v, True, True, True)
          descriptors_from_keys(st, src_h, result_h, rest)
        }
      }
    }
  }
}

fn create(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(proto_val, props_val) = two_args_or_undefined(args)
  let proto = case classify(proto_val) {
    KHandle(h) -> Ok(Some(h))
    KNull -> Ok(None)
    _ -> Error(Nil)
  }
  case proto {
    Error(Nil) ->
      rt_val.t_throw_type_error(
        st,
        "Object prototype may only be an Object or null",
      )
    Ok(prototype) -> {
      let #(h, st) = rt_obj.t_new_object(st, prototype)
      case classify(props_val) {
        KUndef -> #(mk_object(h), st)
        _ -> define_properties_on(st, h, props_val)
      }
    }
  }
}

fn assign(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case args {
    [] -> rt_val.t_throw_type_error(st, cannot_convert)
    [target, ..sources] -> {
      let #(target_h, st) = rt_val.t_to_object(st, target)
      let st =
        list.fold(sources, st, fn(st, src) { assign_one(st, target_h, src) })
      #(mk_object(target_h), st)
    }
  }
}

fn assign_one(st: Agent, target_h: Handle, src: JsVal) -> Agent {
  case classify(src) {
    KNull | KUndef -> st
    _ -> {
      let #(src_h, st) = rt_val.t_to_object(st, src)
      let #(keys, st) = rt_obj.t_own_keys(st, src_h)
      list.fold(keys, st, fn(st, k) {
        let #(prop, st) = rt_obj.t_get_own_property(st, src_h, k)
        let enumerable =
          option.map(prop, rt_types.prop_enumerable) |> option.unwrap(False)
        case enumerable {
          False -> st
          True -> {
            let #(v, st) = rt_obj.t_get_prop(st, mk_object(src_h), k)
            let #(ok, st) = rt_obj.t_set_prop(st, mk_object(target_h), k, v)
            case ok {
              True -> st
              False ->
                rt_val.t_throw_type_error(
                  st,
                  "Cannot assign to read only property '"
                    <> key_text(st, k)
                    <> "' of object",
                )
            }
          }
        }
      })
    }
  }
}

fn object_is(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(a, b) = two_args_or_undefined(args)
  #(mk_bool(rt_val.same_value(a, b)), st)
}

fn has_own(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(target, key_val) = two_args_or_undefined(args)
  case classify(target) {
    KHandle(h) -> {
      let #(desc, st) = own_property_by_value(st, h, key_val)
      #(mk_bool(option.is_some(desc)), st)
    }
    KNull | KUndef -> rt_val.t_throw_type_error(st, cannot_convert)
    KStr(s) -> {
      let #(key, st) = rt_val.t_find_property_key(st, key_val)
      #(mk_bool(option.is_some(string_own_property(s, key))), st)
    }
    _ -> {
      let #(_key, st) = rt_val.t_to_property_key(st, key_val)
      #(mk_bool(False), st)
    }
  }
}

fn has_own_property(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let key_val = first_arg_or_undefined(args)
  case classify(this) {
    KHandle(h) -> {
      let #(desc, st) = own_property_by_value(st, h, key_val)
      #(mk_bool(option.is_some(desc)), st)
    }
    KNull | KUndef -> {
      let #(_, st) = rt_val.t_find_property_key(st, key_val)
      rt_val.t_throw_type_error(st, cannot_convert)
    }
    KStr(s) -> {
      let #(key, st) = rt_val.t_find_property_key(st, key_val)
      #(mk_bool(option.is_some(string_own_property(s, key))), st)
    }
    _ -> {
      let #(_, st) = rt_val.t_find_property_key(st, key_val)
      #(mk_bool(False), st)
    }
  }
}

// t_get_own_property that never names an unseen string on a plain object
fn own_property_by_value(
  st: Agent,
  h: Handle,
  key_val: JsVal,
) -> #(Option(Property), Agent) {
  case rt_val.t_find_property_key(st, key_val) {
    #(Ok(key), st) -> rt_obj.t_get_own_property(st, h, key)
    #(Error(text), st) -> rt_obj.t_own_property_by_text(st, h, text)
  }
}

fn string_own_property(
  s: String,
  key: Result(ObjectKey, String),
) -> Option(Property) {
  case key {
    Ok(key) -> string_exotic_own_property(s, key)
    Error(_unseen) -> None
  }
}

fn property_is_enumerable(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let key_val = first_arg_or_undefined(args)
  case classify(this) {
    KHandle(h) -> {
      let #(desc, st) = own_property_by_value(st, h, key_val)
      #(
        mk_bool(
          option.map(desc, rt_types.prop_enumerable) |> option.unwrap(False),
        ),
        st,
      )
    }
    KNull | KUndef -> {
      let #(_, st) = rt_val.t_find_property_key(st, key_val)
      rt_val.t_throw_type_error(st, cannot_convert)
    }
    KStr(s) -> {
      let #(key, st) = rt_val.t_find_property_key(st, key_val)
      #(
        mk_bool(
          string_own_property(s, key)
          |> option.map(rt_types.prop_enumerable)
          |> option.unwrap(False),
        ),
        st,
      )
    }
    _ -> {
      let #(_, st) = rt_val.t_find_property_key(st, key_val)
      #(mk_bool(False), st)
    }
  }
}

fn object_to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KUndef -> #(mk_string("[object Undefined]"), st)
    KNull -> #(mk_string("[object Null]"), st)
    _ -> {
      // isarray in builtin_tag must run before the get
      let fallback = builtin_tag(st, this)
      let #(tag_val, st) =
        rt_obj.t_get_prop(st, this, SymbolKey(rt_types.symbol_to_string_tag))
      let t = case classify(tag_val) {
        KStr(s) -> s
        _ -> fallback
      }
      #(mk_string("[object " <> t <> "]"), st)
    }
  }
}

fn builtin_tag(st: Agent, this: JsVal) -> String {
  case classify(this) {
    KBool(_) -> "Boolean"
    KNum(_) -> "Number"
    KStr(_) -> "String"
    KSym(_) -> "Symbol"
    KBig(_) -> "Object"
    KHandle(h) -> {
      use <- bool.guard(rt_obj.t_is_array(st, h), "Array")
      case rt_store.t_cell_get(st, h) {
        SObject(kind:, ..) ->
          case kind {
            ArgumentsObj(..) -> "Arguments"
            KCompiled(..) | KBytecode(..) | KNative(..) -> "Function"
            rt_types.KBound(..) -> "Function"
            ProxyObj(target:, ..) ->
              case rt_call.is_callable(st, mk_object(target)) {
                True -> "Function"
                False -> "Object"
              }
            ErrorObj(..) -> "Error"
            BooleanObj(..) -> "Boolean"
            NumberObj(..) -> "Number"
            StringObj(..) -> "String"
            DateObj(..) -> "Date"
            RegExpObj(..) -> "RegExp"
            _ -> "Object"
          }
        // h-shape-slowpath-compat
        SShapedObject(..) -> "Object"
        _ -> "Object"
      }
    }
    _ -> "Object"
  }
}

fn object_value_of(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(h, st) = rt_val.t_to_object(st, this)
  #(mk_object(h), st)
}

fn object_to_locale_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KNull | KUndef -> rt_val.t_throw_type_error(st, cannot_convert)
    _ -> rt_call.t_call_method(st, this, StringKey(nk.to_string), [])
  }
}

fn get_prototype_of(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let target = first_arg_or_undefined(args)
  let r = st.realm
  case classify(target) {
    KHandle(h) -> {
      let #(p, st) = rt_obj.t_get_proto(st, h)
      #(
        case p {
          Some(ph) -> mk_object(ph)
          None -> mk_null()
        },
        st,
      )
    }
    KNull | KUndef -> rt_val.t_throw_type_error(st, cannot_convert)
    KNum(_) -> #(mk_object(r.number.prototype), st)
    KStr(_) -> #(mk_object(r.string.prototype), st)
    KBool(_) -> #(mk_object(r.boolean.prototype), st)
    KSym(_) -> #(mk_object(r.symbol.prototype), st)
    KBig(_) -> #(mk_object(r.bigint.prototype), st)
    _ -> #(mk_object(r.object.prototype), st)
  }
}

fn set_prototype_of(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(target, proto_val) = two_args_or_undefined(args)
  let proto = case classify(proto_val) {
    KHandle(h) -> Ok(Some(h))
    KNull -> Ok(None)
    _ -> Error(Nil)
  }
  case classify(target), proto {
    KNull, _ | KUndef, _ -> rt_val.t_throw_type_error(st, cannot_convert)
    _, Error(_) ->
      rt_val.t_throw_type_error(
        st,
        "Object prototype may only be an Object or null",
      )
    KHandle(h), Ok(new_proto) -> {
      let #(status, st) = rt_obj.t_set_prototype_of(st, h, new_proto)
      case status {
        Ok(Nil) -> #(target, st)
        Error(fail) ->
          rt_val.t_throw_type_error(st, rt_obj.set_proto_fail_message(fail))
      }
    }
    _, Ok(_) -> #(target, st)
  }
}

fn proto_setter(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let proto_val = first_arg_or_undefined(args)
  case classify(this), classify(proto_val) {
    KNull, _ | KUndef, _ -> rt_val.t_throw_type_error(st, cannot_convert)
    KHandle(_), KHandle(_) | KHandle(_), KNull -> {
      let #(_v, st) = set_prototype_of(st, [this, proto_val])
      #(mk_undefined(), st)
    }
    _, _ -> #(mk_undefined(), st)
  }
}

fn is_prototype_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case classify(first_arg_or_undefined(args)) {
    KHandle(v_h) ->
      case classify(this) {
        KNull | KUndef -> rt_val.t_throw_type_error(st, cannot_convert)
        KHandle(this_h) -> is_prototype_of_loop(st, v_h, this_h)
        _ -> #(mk_bool(False), st)
      }
    _ -> #(mk_bool(False), st)
  }
}

fn is_prototype_of_loop(
  st: Agent,
  v_h: Handle,
  this_h: Handle,
) -> #(JsVal, Agent) {
  let #(proto, st) = rt_obj.t_get_proto(st, v_h)
  case proto {
    Some(ph) ->
      case ph == this_h {
        True -> #(mk_bool(True), st)
        False -> is_prototype_of_loop(st, ph, this_h)
      }
    None -> #(mk_bool(False), st)
  }
}

type IntegrityLevel {
  Sealed
  Frozen
}

fn set_integrity_level(
  st: Agent,
  args: List(JsVal),
  level: IntegrityLevel,
) -> #(JsVal, Agent) {
  let target = first_arg_or_undefined(args)
  case classify(target) {
    KHandle(h) -> #(target, set_integrity_level_of(st, h, level))
    _ -> #(target, st)
  }
}

fn set_integrity_level_of(
  st: Agent,
  h: Handle,
  level: IntegrityLevel,
) -> Agent {
  let #(ok, st) = rt_obj.t_prevent_extensions(st, h)
  use <- bool.lazy_guard(!ok, fn() {
    rt_val.t_throw_type_error(st, "Cannot prevent extensions")
  })
  let #(keys, st) = rt_obj.t_own_keys(st, h)
  list.fold(keys, st, fn(st, k) { seal_one_key(st, h, k, level) })
}

pub fn freeze(st: Agent, h: Handle) -> Agent {
  set_integrity_level_of(st, h, Frozen)
}

fn seal_one_key(
  st: Agent,
  h: Handle,
  k: ObjectKey,
  level: IntegrityLevel,
) -> Agent {
  let non_configurable =
    ParsedDesc(
      value: None,
      get: None,
      set: None,
      writable: None,
      enumerable: None,
      configurable: Some(False),
    )
  let #(desc, st) = case level {
    Sealed -> #(Some(non_configurable), st)
    Frozen -> {
      let #(current, st) = rt_obj.t_get_own_property(st, h, k)
      #(
        option.map(current, fn(prop) {
          case prop {
            AccessorProperty(..) -> non_configurable
            DataProperty(..) ->
              ParsedDesc(..non_configurable, writable: Some(False))
          }
        }),
        st,
      )
    }
  }
  case desc {
    None -> st
    Some(d) -> {
      let #(ok, st) = rt_obj.t_define_own_prop(st, h, k, d)
      case ok {
        True -> st
        False ->
          rt_val.t_throw_type_error(
            st,
            "Cannot redefine property: " <> key_text(st, k),
          )
      }
    }
  }
}

fn test_integrity_level(
  st: Agent,
  args: List(JsVal),
  level: IntegrityLevel,
) -> #(JsVal, Agent) {
  case classify(first_arg_or_undefined(args)) {
    KHandle(h) -> {
      let #(extensible, st) = rt_obj.t_is_extensible(st, h)
      use <- bool.guard(extensible, #(mk_bool(False), st))
      let #(keys, st) = rt_obj.t_own_keys(st, h)
      let #(ok, st) =
        list.fold(keys, #(True, st), fn(acc, k) {
          let #(ok, st) = acc
          use <- bool.guard(!ok, acc)
          let #(desc, st) = rt_obj.t_get_own_property(st, h, k)
          case desc {
            None -> #(True, st)
            Some(p) -> #(prop_at_integrity_level(p, level), st)
          }
        })
      #(mk_bool(ok), st)
    }
    _ -> #(mk_bool(True), st)
  }
}

fn prop_at_integrity_level(prop: Property, level: IntegrityLevel) -> Bool {
  case level, prop {
    _, AccessorProperty(configurable:, ..) -> !configurable
    Sealed, DataProperty(configurable:, ..) -> !configurable
    Frozen, DataProperty(configurable:, writable:, ..) ->
      !configurable && !writable
  }
}

fn is_extensible(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(first_arg_or_undefined(args)) {
    KHandle(h) -> {
      let #(extensible, st) = rt_obj.t_is_extensible(st, h)
      #(mk_bool(extensible), st)
    }
    _ -> #(mk_bool(False), st)
  }
}

fn prevent_extensions(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let target = first_arg_or_undefined(args)
  case classify(target) {
    KHandle(h) -> {
      let #(ok, st) = rt_obj.t_prevent_extensions(st, h)
      case ok {
        True -> #(target, st)
        False -> rt_val.t_throw_type_error(st, "Cannot prevent extensions")
      }
    }
    _ -> #(target, st)
  }
}

fn from_entries(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let iterable = first_arg_or_undefined(args)
  case classify(iterable) {
    KNull | KUndef -> rt_val.t_throw_type_error(st, cannot_convert)
    _ -> {
      let #(obj_h, st) =
        rt_obj.t_new_object(st, Some(st.realm.object.prototype))
      use st, k, v <- iter_protocol.add_entries_with_sink(
        st,
        mk_object(obj_h),
        iterable,
      )
      let #(key, st) = rt_val.t_to_property_key(st, k)
      let #(_ok, st) =
        rt_obj.t_define_own_data(st, obj_h, key, v, True, True, True)
      st
    }
  }
}

fn group_by(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(items, callback) = two_args_or_undefined(args)
  case rt_call.is_callable(st, callback) {
    False ->
      rt_val.t_throw_type_error(st, "Object.groupBy callback is not callable")
    True -> {
      let #(rec, st) = iter_protocol.get_iterator_sync(st, items)
      group_by_loop(st, rec, callback, 0, dict.new(), [])
    }
  }
}

fn group_by_loop(
  st: Agent,
  rec: iter_protocol.IteratorRecord,
  callback: JsVal,
  index: Int,
  groups: dict.Dict(ObjectKey, List(JsVal)),
  order: List(ObjectKey),
) -> #(JsVal, Agent) {
  case iter_protocol.iterator_step_value(st, rec) {
    #(None, st) -> group_by_finish(st, groups, list.reverse(order))
    #(Some(item), st) -> {
      use key_prim, st <- iter_protocol.or_close(st, rec.iterator, fn(st) {
        let #(kv, st) =
          rt_call.t_call_checked(st, callback, mk_undefined(), [
            item,
            mk_number(JInt(index)),
          ])
        let #(key, st) = rt_val.t_to_property_key(st, kv)
        #(rt_obj.t_object_key_value(st, key), st)
      })
      let #(key, st) = rt_val.t_to_property_key(st, key_prim)
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
  groups: dict.Dict(ObjectKey, List(JsVal)),
  order: List(ObjectKey),
) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  let #(obj_h, st) = rt_obj.t_new_object(st, None)
  let st =
    list.fold(order, st, fn(st, key) {
      let members =
        dict.get(groups, key) |> option.from_result |> option.unwrap([])
      let #(arr_h, st) =
        common.alloc_array(st, list.reverse(members), array_proto)
      let #(_ok, st) =
        rt_obj.t_define_own_data(
          st,
          obj_h,
          key,
          mk_object(arr_h),
          True,
          True,
          True,
        )
      st
    })
  #(mk_object(obj_h), st)
}

type AccessorKind {
  AsGetter
  AsSetter
}

fn define_getter_setter(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  kind: AccessorKind,
) -> #(JsVal, Agent) {
  let #(key_val, accessor) = two_args_or_undefined(args)
  let #(h, st) = rt_val.t_to_object(st, this)
  case rt_call.is_callable(st, accessor) {
    False ->
      rt_val.t_throw_type_error(st, case kind {
        AsGetter -> "Getter must be a function"
        AsSetter -> "Setter must be a function"
      })
    True -> {
      let #(key, st) = rt_val.t_to_property_key(st, key_val)
      let #(get, set) = case kind {
        AsGetter -> #(Some(accessor), None)
        AsSetter -> #(None, Some(accessor))
      }
      let #(ok, st) =
        rt_obj.t_define_own_prop(
          st,
          h,
          key,
          ParsedDesc(
            value: None,
            get:,
            set:,
            writable: None,
            enumerable: Some(True),
            configurable: Some(True),
          ),
        )
      case ok {
        True -> #(mk_undefined(), st)
        False ->
          rt_val.t_throw_type_error(
            st,
            "Cannot define property " <> key_text(st, key),
          )
      }
    }
  }
}

fn lookup_getter_setter(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  kind: AccessorKind,
) -> #(JsVal, Agent) {
  let #(h, st) = rt_val.t_to_object(st, this)
  let #(key, st) = rt_val.t_to_property_key(st, first_arg_or_undefined(args))
  lookup_accessor_chain(st, h, key, kind)
}

fn lookup_accessor_chain(
  st: Agent,
  h: Handle,
  key: ObjectKey,
  kind: AccessorKind,
) -> #(JsVal, Agent) {
  let #(desc, st) = rt_obj.t_get_own_property(st, h, key)
  case desc {
    Some(AccessorProperty(get:, set:, ..)) -> {
      let slot = case kind {
        AsGetter -> get
        AsSetter -> set
      }
      #(option.unwrap(slot, mk_undefined()), st)
    }
    Some(DataProperty(..)) -> #(mk_undefined(), st)
    None -> {
      let #(proto, st) = rt_obj.t_get_proto(st, h)
      case proto {
        Some(ph) -> lookup_accessor_chain(st, ph, key, kind)
        None -> #(mk_undefined(), st)
      }
    }
  }
}

fn string_exotic_own_property(s: String, k: ObjectKey) -> Option(Property) {
  case k {
    StringKey(k) if k == nk.length ->
      Some(DataProperty(
        value: mk_number(JInt(js_string.length(s))),
        writable: False,
        enumerable: False,
        configurable: False,
        seq: 0,
      ))
    StringKey(k) if k < 0 ->
      case js_string.char_at(s, key.index_of(k)) {
        Some(ch) ->
          Some(DataProperty(
            value: mk_string(ch),
            writable: False,
            enumerable: True,
            configurable: False,
            seq: 0,
          ))
        None -> None
      }
    _ -> None
  }
}

fn string_index_keys(i: Int, len: Int) -> List(JsVal) {
  case i >= len {
    True -> []
    False -> [mk_string(int.to_string(i)), ..string_index_keys(i + 1, len)]
  }
}

fn string_index_object_keys(i: Int, len: Int) -> List(ObjectKey) {
  case i >= len {
    True -> []
    False -> [StringKey(key.index(i)), ..string_index_object_keys(i + 1, len)]
  }
}

fn ok_array(st: Agent, values: List(JsVal)) -> #(JsVal, Agent) {
  let #(h, st) = common.alloc_array(st, values, st.realm.array.prototype)
  #(mk_object(h), st)
}

fn key_text(st: Agent, key: ObjectKey) -> String {
  rt_obj.key_text(st, key)
}
