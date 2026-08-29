import arc/bytecode/key.{Named}
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Handle, type JsVal, AccessorProperty, DataProperty, KHandle,
  StringKey, SymbolKey, classify, mk_object, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/option.{type Option, None, Some}

// §9.1.1.2.1 hasbinding, honours @@unscopables
pub fn t_with_has_binding(
  st: Agent,
  obj: Handle,
  name: String,
) -> #(Bool, Agent) {
  let recv = mk_object(obj)
  let #(found, st) = rt_obj.t_has_prop(st, recv, StringKey(Named(name)))
  case found {
    False -> #(False, st)
    True -> {
      let #(unscopables, st) =
        rt_obj.t_get_prop(st, recv, SymbolKey(rt_types.symbol_unscopables))
      case classify(unscopables) {
        KHandle(_) -> {
          let #(blocked, st) =
            rt_obj.t_get_prop(st, unscopables, StringKey(Named(name)))
          #(!rt_val.to_boolean(blocked), st)
        }
        _ -> #(True, st)
      }
    }
  }
}

// §9.1.1.2.6, rechecks hasproperty after unscopables getter
pub fn t_with_get_binding_value(
  st: Agent,
  obj: Handle,
  name: String,
  strict: Bool,
) -> #(JsVal, Agent) {
  let recv = mk_object(obj)
  let key = StringKey(Named(name))
  let #(still, st) = rt_obj.t_has_prop(st, recv, key)
  case still, strict {
    False, True -> rt_val.t_throw_reference_error(st, name <> " is not defined")
    False, False -> #(mk_undefined(), st)
    True, _ -> rt_obj.t_get_prop(st, recv, key)
  }
}

// §9.1.1.2.5, stores to the original object
pub fn t_with_set_mutable_binding(
  st: Agent,
  obj: Handle,
  name: String,
  value: JsVal,
  strict: Bool,
) -> Agent {
  let recv = mk_object(obj)
  let key = StringKey(Named(name))
  let #(still, st) = rt_obj.t_has_prop(st, recv, key)
  let st = case still, strict {
    False, True -> rt_val.t_throw_reference_error(st, name <> " is not defined")
    _, _ -> st
  }
  let #(ok, st) = rt_obj.t_set_prop(st, recv, key, value)
  case ok, strict {
    False, True ->
      rt_val.t_throw_type_error(
        st,
        "Cannot assign to read only property '" <> name <> "' of object",
      )
    _, _ -> st
  }
}

// §9.1.1.2.7
pub fn t_with_delete_binding(
  st: Agent,
  obj: Handle,
  name: String,
) -> #(Bool, Agent) {
  rt_obj.t_delete_prop(st, obj, StringKey(Named(name)))
}

// §9.1.1.4.17; deviation: non-extensible global skips the typeerror
pub fn t_create_global_var_binding(
  st: Agent,
  name: String,
  deletable: Bool,
) -> Agent {
  let global = st.realm.global_object
  let key = StringKey(Named(name))
  let #(own, st) = rt_obj.t_get_own_property(st, global, key)
  case own {
    Some(_) -> st
    None -> {
      let #(_, st) =
        rt_obj.t_define_own_data(
          st,
          global,
          key,
          mk_undefined(),
          True,
          True,
          deletable,
        )
      st
    }
  }
}

// §9.1.1.4.16 + §9.1.1.4.18 declaration half
pub fn t_create_global_fn_binding(
  st: Agent,
  name: String,
  deletable: Bool,
) -> Agent {
  let global = st.realm.global_object
  let key = StringKey(Named(name))
  let #(own, st) = rt_obj.t_get_own_property(st, global, key)
  let define = fn(st) {
    let #(_, st) =
      rt_obj.t_define_own_data(
        st,
        global,
        key,
        mk_undefined(),
        True,
        True,
        deletable,
      )
    st
  }
  case own {
    None -> {
      let #(extensible, st) = rt_obj.t_is_extensible(st, global)
      case extensible {
        True -> define(st)
        False -> not_definable(st, name)
      }
    }
    Some(DataProperty(configurable: True, ..))
    | Some(AccessorProperty(configurable: True, ..)) -> define(st)
    Some(DataProperty(writable: True, enumerable: True, ..)) -> st
    Some(DataProperty(..)) | Some(AccessorProperty(..)) ->
      not_definable(st, name)
  }
}

fn not_definable(st: Agent, name: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "Cannot declare global function '" <> name <> "'",
  )
}

// §9.1.1.4.7 object record half
pub fn t_delete_global_var(st: Agent, name: String) -> #(Bool, Agent) {
  rt_obj.t_delete_prop(st, st.realm.global_object, StringKey(Named(name)))
}

// §19.2.1.3 sloppy direct eval var scope, never escapes to js
pub fn t_new_eval_env(st: Agent) -> #(Handle, Agent) {
  rt_obj.t_new_object(st, None)
}

pub fn eval_env_lookup(st: Agent, env: Handle, name: String) -> Option(JsVal) {
  case rt_obj.t_ordinary_own_property(st, env, StringKey(Named(name))) {
    Some(DataProperty(value:, ..)) -> Some(value)
    _ -> None
  }
}

pub fn eval_env_has(st: Agent, env: Handle, name: String) -> Bool {
  option.is_some(rt_obj.t_ordinary_own_property(st, env, StringKey(Named(name))))
}

pub fn t_eval_env_set(
  st: Agent,
  env: Handle,
  name: String,
  value: JsVal,
) -> Agent {
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      env,
      StringKey(Named(name)),
      value,
      True,
      True,
      True,
    )
  st
}

pub fn t_eval_env_declare(st: Agent, env: Handle, name: String) -> Agent {
  case eval_env_has(st, env, name) {
    True -> st
    False -> t_eval_env_set(st, env, name, mk_undefined())
  }
}
