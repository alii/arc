//// Environment Record operations that are backed by heap objects rather
//// than interpreter registers: the Object Environment Record of a `with`
//// statement (§9.1.1.2), the object half of the Global Environment Record
//// (§9.1.1.4), and the var scope a sloppy direct eval injects into its
//// calling function (§19.2.1.3). Every operation takes the threaded `Agent`
//// and raises through `t_throw` like the rest of the runtime; the
//// interpreter's opcode arms are thin stack shuffles around these.

import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Handle, type JsVal, DataProperty, KHandle, Named, StringKey,
  SymbolKey, classify, mk_object, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/option.{type Option, None, Some}

// ── §9.1.1.2 Object Environment Record (`with`) ─────────────────────────────

/// §9.1.1.2.1 HasBinding(N) for a with environment: HasProperty(obj, N),
/// then, because [[IsWithEnvironment]] is true, Get(obj, @@unscopables) and
/// treat a truthy Get(unscopables, N) as "not bound". Both Gets can run user
/// code (getters, proxy traps).
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

/// §9.1.1.2.6 GetBindingValue(N, S) once HasBinding said yes. HasProperty is
/// re-checked because the @@unscopables getter may have deleted the binding:
/// gone + strict referencing code is a ReferenceError, gone + sloppy reads
/// `undefined`, otherwise Get(obj, N).
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

/// §9.1.1.2.5 SetMutableBinding(N, V, S) once HasBinding said yes (or on a
/// reference base made earlier: the store goes to the ORIGINAL object even
/// if the binding vanished meanwhile, §13.15.2 note). Step 2 re-checks
/// HasProperty: gone + strict is a ReferenceError. A rejected [[Set]] is a
/// TypeError in strict code only (§6.2.5.6 PutValue 3.d).
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

/// §9.1.1.2.7 DeleteBinding(N): obj.[[Delete]](N).
pub fn t_with_delete_binding(
  st: Agent,
  obj: Handle,
  name: String,
) -> #(Bool, Agent) {
  rt_obj.t_delete_prop(st, obj, StringKey(Named(name)))
}

// ── §9.1.1.4 Global Environment Record, object half ─────────────────────────

/// §9.1.1.4.17 CreateGlobalVarBinding(N, D) on the current realm's global
/// object: if N is not already an OWN property (HasOwnProperty, not the
/// prototype chain, so an inherited accessor such as `__proto__` still gets
/// shadowed by an own data binding), define `{value: undefined, W: true,
/// E: true, C: D}`. Script/function GlobalDeclarationInstantiation passes
/// D = false so top-level `var`/function bindings survive `delete`; eval
/// code passes D = true (§19.2.1.3). A non-extensible global refuses the
/// create silently: the spec's up-front CanDeclareGlobalVar TypeError
/// (§16.1.7 step 5.c) is a pre-existing arc gap kept as is.
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

/// §9.1.1.4.7 DeleteBinding(N), object-record half: a real [[Delete]] on the
/// global object. Configurable properties (implicit `x = 1` globals) go and
/// answer true, non-configurable ones answer false, a missing name answers
/// true ([[Delete]] step 2). The declarative half (let/const/class) is the
/// interpreter's: those bindings are never deletable and answer false without
/// reaching here.
pub fn t_delete_global_var(st: Agent, name: String) -> #(Bool, Agent) {
  rt_obj.t_delete_prop(st, st.realm.global_object, StringKey(Named(name)))
}

// ── §19.2.1.3 sloppy direct eval var scope ──────────────────────────────────
// A sloppy direct eval declares its `var`s and functions in the CALLING
// function's variable environment. The caller's own locals are compiled to
// slots, so names the eval introduces live in a side object instead: a
// prototype-less ordinary object, one per activation, allocated the first
// time a sloppy direct eval runs in it and consulted by the Get/Put/Typeof
// EvalVar opcodes before they fall through to the global record. It never
// escapes to user code, so plain own data properties are the whole protocol.

/// Allocate an empty eval var scope.
pub fn t_new_eval_env(st: Agent) -> #(Handle, Agent) {
  rt_obj.t_new_object(st, None)
}

/// The value bound to `name` in `env`, if the eval scope declares it.
pub fn eval_env_lookup(st: Agent, env: Handle, name: String) -> Option(JsVal) {
  case rt_obj.t_ordinary_own_property(st, env, StringKey(Named(name))) {
    Some(DataProperty(value:, ..)) -> Some(value)
    _ -> None
  }
}

/// Does the eval scope declare `name`?
pub fn eval_env_has(st: Agent, env: Handle, name: String) -> Bool {
  option.is_some(rt_obj.t_ordinary_own_property(st, env, StringKey(Named(name))))
}

/// Bind `name` to `value` in the eval scope (create or overwrite).
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

/// EvalDeclarationInstantiation for one `var` name: bind it to `undefined`
/// unless the scope already declares it (a re-declaration keeps the value).
pub fn t_eval_env_declare(st: Agent, env: Handle, name: String) -> Agent {
  case eval_env_has(st, env, name) {
    True -> st
    False -> t_eval_env_set(st, env, name, mk_undefined())
  }
}
