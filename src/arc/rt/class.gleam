//// `rt_class` — class-evaluation runtime ops (SPEC §7.M7).
////
//// Port of arc `interpreter.gleam:3286-3958` (NewPrivateName /
//// SetupDerivedClass / DefineMethod / DefinePrivate*) + `:1711-1876`
//// (private_get/put + get_super_value) + `:5471-5560` (check_private_add /
//// make_method / set_computed_fn_name), re-expressed over the threaded
//// `Agent` model. `t_construct` / `is_constructor` /
//// `t_get_prototype_from_constructor` already live in `rt_call` (M-CALL
//// owns [[Construct]]); this module owns the class-BODY-evaluation ops only.
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).** JRead ops
//// (`t_private_in`, `t_fn_home_object`, `t_fn_flags`, `t_is_constructor`)
//// take `st` to read the store but return a bare value; every other op that
//// touches the store returns `#(V, St')` or bare `St'` (JMutUnit).
////
//// **D7:** every guest-visible failure raises via `rt_store.t_throw` (never
//// `Result`).
////
//// **D9:** private elements are `Private(BitArray)` `PropertyKey`s in the
//// object's own props dict — one mechanism covers fields/methods/accessors;
//// `#x in obj` = own-prop presence. The minted-name `JsVal` wire form is a JS
//// string (`mk_string`) carrying the `<<source, 0, uid>>` bytes so it threads
//// through emitted code as an ordinary value; only `priv_key_of` re-enters it
//// into the `Private` namespace.

import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type FnFlags, type Handle, type JsOps, type JsVal,
  type MethodInstallKind, type ObjectKey, type Property, type PropertyKey,
  AccessorProperty, DataProperty, KBytecode, KCompiled, KHandle, KNull, KStr,
  KTdz, MIGetter, MIMethod, MISetter, MIStatic, MIStaticGetter, MIStaticSetter,
  Named, Private, SObject, StringKey, SymbolKey, classify, mk_object, mk_string,
  mk_undefined,
} as rt_types
import gleam/bit_array
import gleam/dict
import gleam/option.{type Option, None, Some}

// ── private access / throw helpers ──────────────────────────────────────────

/// The seeded `JsOps` upcall table (D17).
fn js_ops(st: Agent) -> JsOps(Agent) {
  st.store.ops
}

/// Allocate a `TypeError(msg)` via `ops.new_error` and RAISE it (D7).
fn throw_type_error(st: Agent, msg: String) -> a {
  let #(e, st) = js_ops(st).new_error(st, rt_types.TypeErr, msg)
  rt_store.t_throw(st, e)
}

/// Storage bytes of a minted private-name `JsVal` (from `t_new_private_name`).
/// The wire form is a JS string carrying `<<source, 0, uid>>` (D9); a
/// non-string here is an M16 emission bug, not a user error.
fn priv_key_bytes(v: JsVal) -> BitArray {
  case classify(v) {
    KStr(s) -> bit_array.from_string(s)
    _ -> panic as "rt_class: private-name JsVal is not KStr (M16 invariant)"
  }
}

/// User-facing text of a `PropertyKey`/`SymbolKey` for TypeError messages.
fn object_key_display(key: ObjectKey) -> String {
  case key {
    StringKey(pk) -> rt_types.key_display_string(pk)
    SymbolKey(sym) -> rt_types.symbol_descriptive_string(sym)
  }
}

// ── C1: private-name minting (§15.7.14 step 5-6) ────────────────────────────

/// Mint a fresh PrivateName for one class-evaluation `#name`. Bumps the
/// threaded `JsStore.private_uid` (D9 — deterministic, replayable). Port of
/// arc `NewPrivateName` (interpreter.gleam:3286) + `mint_private_key`.
pub fn t_new_private_name(st: Agent, source: String) -> #(JsVal, Agent) {
  let #(uid, st) = rt_store.t_next_private_uid(st)
  let bytes = rt_types.private_key_text(source, uid)
  // `<<source:utf8, 0, uid_text:utf8>>` is always valid UTF-8 (NUL is a
  // codepoint), so the storage bytes round-trip losslessly through `KStr`.
  let assert Ok(text) = bit_array.to_string(bytes)
    as "private_key_text is UTF-8 by construction"
  #(mk_string(text), st)
}

// ── C4: MakeMethod (§15.4.4) ────────────────────────────────────────────────

/// §15.4.4 MakeMethod(F, homeObject) — set the function cell's
/// `home_object` to `home` so `super.x` inside it resolves via the home's
/// prototype. No-op on native/bound cells. Port of arc `make_method`
/// (interpreter.gleam:5504-5520). JMutUnit.
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

/// Set the constructor's `[[Fields]]` initializer closure — the synthesized
/// per-instance field-init function M16 emits (SPEC §8 `set_fields_init`).
/// `t_construct` calls it with `this = new_this` (rt_call:543). JMutUnit.
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

// ── C2: class setup (§15.7.14 ClassDefinitionEvaluation) ────────────────────

/// §15.7.14 steps 8-9: validate the heritage value and resolve
/// `#(protoParent, constructorParent)`. `super` encodes the three cases:
/// `KTdz` no `extends` clause; `KNull` `extends null`; `KHandle` `extends h`
/// (must be a constructor whose `.prototype` is an object or null). Anything
/// else, including a real `extends undefined`, is a TypeError.
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
          let #(pp, st) =
            rt_obj.t_get_prop(st, super, StringKey(Named("prototype")))
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

/// §15.7.14 steps 8-18 for an already allocated constructor closure `ctor`
/// (its flags, `name` and `length` come from the closure site): validate the
/// heritage, allocate `proto`, then wire `ctor.[[HomeObject]] = proto`,
/// `ctor.[[Prototype]] = constructorParent`, the non-writable
/// `ctor.prototype` and `proto.constructor`. Returns `proto`. Port of arc
/// `SetupDerivedClass` plus the class arm of `MakeClosure`.
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
      StringKey(Named("prototype")),
      mk_object(proto),
      False,
      False,
      False,
    )
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      proto,
      StringKey(Named("constructor")),
      mk_object(ctor),
      True,
      False,
      True,
    )
  #(proto, st)
}

// ── C3: define method (§14.3.9) ─────────────────────────────────────────────

/// Install a method/accessor on `target` (proto for instance members, ctor
/// for `MIStatic*`, the object for an object literal). Port of arc
/// `DefineMethod`/`DefineMethodComputed`/`DefineAccessor`/
/// `DefineAccessorComputed` (interpreter.gleam:3473-3600). The caller has
/// canonicalized `key` and evaluated the closure. Sets `[[HomeObject]]`,
/// fills the fn's `name` if empty, then defines a configurable data/accessor
/// property (accessor halves merge), enumerable for object literal members
/// (§13.2.5.5) and not for class members (§15.4.5). Throws TypeError on a
/// non-configurable existing own prop (`static ['prototype']`). JMutUnit.
pub fn t_define_method(
  st: Agent,
  target: Handle,
  key: ObjectKey,
  fn_h: Handle,
  kind: MethodInstallKind,
  enumerable: Bool,
) -> Agent {
  // §14.3.9 step 11 DefinePropertyOrThrow: an existing non-configurable own
  // (only `prototype` on the ctor) is a TypeError, not a silent False.
  let _ = case rt_obj.t_ordinary_own_property(st, target, key) {
    Some(prop) ->
      case rt_types.prop_configurable(prop) {
        False ->
          throw_type_error(
            st,
            "Cannot redefine property: " <> object_key_display(key),
          )
        True -> Nil
      }
    None -> Nil
  }
  // §15.4.4 MakeMethod: home_object = target.
  let st = t_make_method(st, fn_h, target)
  // §10.2.9 SetFunctionName: only when the closure was compiled anonymous
  // (computed key — its `name` is "").
  let prefix = case kind {
    MIGetter | MIStaticGetter -> "get "
    MISetter | MIStaticSetter -> "set "
    MIMethod | MIStatic -> ""
  }
  let st = set_fn_name_if_empty(st, fn_h, prefix, key_fn_name(key))
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

/// SetFunctionName step 4: a Symbol key names the function "[description]"
/// (or "" when the symbol has no description); string keys use their display
/// text. arc `symbol_fn_name` + `key_display_string`.
fn key_fn_name(key: ObjectKey) -> String {
  case key {
    StringKey(pk) -> rt_types.key_display_string(pk)
    SymbolKey(sym) ->
      case rt_types.symbol_description(sym) {
        Some(d) -> "[" <> d <> "]"
        None -> ""
      }
  }
}

/// arc `set_computed_fn_name` (interpreter.gleam:5528-5560): overwrite the
/// closure's own `name` iff the current value is the empty string (i.e. the
/// key was computed, so the compiler left it blank).
fn set_fn_name_if_empty(
  st: Agent,
  fn_h: Handle,
  prefix: String,
  name: String,
) -> Agent {
  rt_store.t_cell_update(st, fn_h, fn(slot) {
    case slot {
      SObject(kind: KCompiled(..), props:, ..)
      | SObject(kind: KBytecode(..), props:, ..) ->
        case dict.get(props, Named("name")) {
          Ok(DataProperty(value: v, seq:, ..)) ->
            case classify(v) {
              KStr("") ->
                SObject(
                  ..slot,
                  props: dict.insert(
                    props,
                    Named("name"),
                    DataProperty(
                      value: mk_string(prefix <> name),
                      writable: False,
                      enumerable: False,
                      configurable: True,
                      seq:,
                    ),
                  ),
                )
              _ -> slot
            }
          _ -> slot
        }
      _ -> slot
    }
  })
}

// ── C5: private-element install (§7.3.28/§7.3.29) ───────────────────────────

/// §7.3.28 PrivateFieldAdd — install one instance field `#x = v` during the
/// field-initializer call. Throws on double-init or non-extensible target.
/// Port of arc `DefinePrivateField` + `check_private_add`. Bypasses
/// [[DefineOwnProperty]] (private elements are invisible to integrity levels);
/// writes a raw `{W:T, E:F, C:T}` data prop into the props dict. JMutUnit.
pub fn t_private_define(
  st: Agent,
  obj: Handle,
  priv_key: JsVal,
  v: JsVal,
) -> Agent {
  let bytes = priv_key_bytes(priv_key)
  let st = check_private_add(st, obj, bytes)
  raw_define_private_data(st, obj, Private(bytes), v, True)
}

/// §7.3.29 PrivateMethodOrAccessorAdd — install a shared method/accessor
/// closure on one instance. Port of arc `DefinePrivateMethod` /
/// `DefinePrivateAccessor` (interpreter.gleam:3367-3428). Does NOT set
/// `home_object` (M16 issues `t_make_method` once at class-def time; the
/// per-instance install just copies the shared closure ref). JMutUnit.
pub fn t_define_private(
  st: Agent,
  obj: Handle,
  priv_key: JsVal,
  fn_v: JsVal,
  kind: MethodInstallKind,
) -> Agent {
  let bytes = priv_key_bytes(priv_key)
  let key = Private(bytes)
  case kind {
    // Method: non-writable so `t_private_set`'s method check trips.
    MIMethod | MIStatic -> {
      let st = check_private_add(st, obj, bytes)
      raw_define_private_data(st, obj, key, fn_v, False)
    }
    // Accessor half: merge with the other half from the same class-eval; the
    // SAME half already present is double-init → TypeError.
    MIGetter | MIStaticGetter | MISetter | MIStaticSetter -> {
      let is_getter = case kind {
        MIGetter | MIStaticGetter -> True
        _ -> False
      }
      let existing = rt_obj.t_ordinary_own_property(st, obj, StringKey(key))
      let st = case existing {
        None -> check_private_add(st, obj, bytes)
        Some(AccessorProperty(get:, set:, ..)) ->
          case
            is_getter
            && option.is_some(get)
            || !is_getter
            && option.is_some(set)
          {
            True -> throw_private_double_init(st, bytes, "private accessor ")
            False -> st
          }
        Some(DataProperty(..)) ->
          throw_private_double_init(st, bytes, "private accessor ")
      }
      raw_merge_private_accessor(st, obj, key, existing, fn_v, is_getter)
    }
  }
}

/// arc `check_private_add` (interpreter.gleam:5471-5501): TypeError on
/// double-init or non-extensible target.
fn check_private_add(st: Agent, obj: Handle, bytes: BitArray) -> Agent {
  case rt_obj.t_ordinary_own_property(st, obj, StringKey(Private(bytes))) {
    Some(_) -> throw_private_double_init(st, bytes, "")
    None ->
      case rt_obj.t_ordinary_is_extensible(st, obj) {
        False ->
          throw_type_error(
            st,
            "Cannot define private member "
              <> rt_types.private_display_name(bytes)
              <> " on a non-extensible object",
          )
        True -> st
      }
  }
}

fn throw_private_double_init(st: Agent, bytes: BitArray, kind: String) -> a {
  throw_type_error(
    st,
    "Cannot initialize "
      <> kind
      <> rt_types.private_display_name(bytes)
      <> " twice on the same object",
  )
}

/// arc `object.define_private_data` (object.gleam:1763-1783): raw props-dict
/// insert bypassing [[DefineOwnProperty]].
fn raw_define_private_data(
  st: Agent,
  obj: Handle,
  key: PropertyKey,
  v: JsVal,
  writable: Bool,
) -> Agent {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  rt_store.t_cell_update(st, obj, fn(slot) {
    let assert SObject(props:, ..) = slot
      as "t_private_define target is not an SObject"
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

/// arc `merge_accessor` (object.gleam:1787-1813) for a `Private` key.
fn raw_merge_private_accessor(
  st: Agent,
  obj: Handle,
  key: PropertyKey,
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
    let assert SObject(props:, ..) = slot
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

// ── C6-C8: private get/set/in (§7.3.30-32 / §13.10.1) ───────────────────────

/// §7.3.30 PrivateGet — own-only lookup of `Private(text)`. Port of arc
/// `private_get_dyn` (interpreter.gleam:1734-1776). Getter invocation (via
/// `ops.call`) may re-enter JS.
pub fn t_private_get(
  st: Agent,
  obj: JsVal,
  priv_key: JsVal,
) -> #(JsVal, Agent) {
  let bytes = priv_key_bytes(priv_key)
  let name = rt_types.private_display_name(bytes)
  case classify(obj) {
    KHandle(h) ->
      case rt_obj.t_ordinary_own_property(st, h, StringKey(Private(bytes))) {
        Some(DataProperty(value:, ..)) -> #(value, st)
        Some(AccessorProperty(get: Some(getter), ..)) ->
          js_ops(st).call(st, getter, obj, [])
        Some(AccessorProperty(get: None, ..)) ->
          throw_type_error(st, "'" <> name <> "' was defined without a getter")
        None ->
          throw_type_error(
            st,
            "Cannot read private member "
              <> name
              <> " from an object whose class did not declare it",
          )
      }
    _ ->
      throw_type_error(
        st,
        "Cannot read private member " <> name <> " on non-object",
      )
  }
}

/// §7.3.31 PrivateSet — own-only. A method (non-writable data) or a
/// setter-less accessor throws TypeError regardless of strict mode. Port of
/// arc `private_put_found` (interpreter.gleam:1789-1817). Returns `v`.
pub fn t_private_set(
  st: Agent,
  obj: JsVal,
  priv_key: JsVal,
  v: JsVal,
) -> #(JsVal, Agent) {
  let bytes = priv_key_bytes(priv_key)
  let name = rt_types.private_display_name(bytes)
  let key = Private(bytes)
  case classify(obj) {
    KHandle(h) ->
      case rt_obj.t_ordinary_own_property(st, h, StringKey(key)) {
        Some(DataProperty(writable: True, ..)) -> {
          // Overwrite the value in place (own data, no [[DefineOwnProperty]]).
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
              <> name
              <> ": it is a method or has no setter",
          )
        None ->
          throw_type_error(
            st,
            "Cannot write private member "
              <> name
              <> " to an object whose class did not declare it",
          )
      }
    _ ->
      throw_type_error(
        st,
        "Cannot write private member " <> name <> " on non-object",
      )
  }
}

/// §13.10.1 `#x in obj` — own-only presence check. Port of arc `PrivateInDyn`
/// (interpreter.gleam:3330-3350). Non-object → TypeError. JRead: returns
/// bare `Bool`; the store is only READ.
pub fn t_private_in(st: Agent, obj: JsVal, priv_key: JsVal) -> Bool {
  let bytes = priv_key_bytes(priv_key)
  case classify(obj) {
    KHandle(h) ->
      option.is_some(rt_obj.t_ordinary_own_property(
        st,
        h,
        StringKey(Private(bytes)),
      ))
    _ ->
      throw_type_error(
        st,
        "Cannot use 'in' operator to search for private name "
          <> rt_types.private_display_name(bytes)
          <> " in non-object",
      )
  }
}

// ── C9-C10: super property access (§13.3.7.3 MakeSuperPropertyReference) ────

/// `super.key` read: OrdinaryGet on `home.[[Prototype]]` with `receiver` as
/// `this`. Port of arc `get_super_value` (interpreter.gleam:1846-1876). M16
/// passes `home` from the frame's home_object slot (bound at prologue); `key`
/// is already ToPropertyKey'd (G9).
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

/// `super.key = v`: OrdinarySet on `home.[[Prototype]]` with `receiver` as
/// `this`. Port of arc `PutSuperValue` (interpreter.gleam:4332-4367). `!ok`
/// throws only when `strict` (object-literal super may be sloppy). Returns `v`.
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

// ── C12: super call (§13.3.7.1 SuperCall) ───────────────────────────────────

/// `super(...args)` — [[Construct]] on `active_func.[[Prototype]]` with the
/// derived ctor's `new.target`. Port of arc `emit_super_call` decomposed as
/// GetPrototypeOf + CallConstructor (emit.gleam:4633-4672). The M14/M16
/// caller then binds the returned Handle into the ctor body's `this` local
/// (with a double-super ReferenceError check) and runs the field-init call —
/// those are emit-side, not here.
pub fn t_super_call(
  st: Agent,
  active_func: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case rt_obj.t_get_proto(st, active_func) {
    #(Some(parent), st) ->
      rt_call.t_construct(st, mk_object(parent), args, new_target)
    // A derived ctor's [[Prototype]] was set by `t_class_setup`; `null` here
    // means user code did `Object.setPrototypeOf(Ctor, null)` — TypeError per
    // §13.3.7.1 step 5 (GetSuperConstructor's IsConstructor gate).
    #(None, st) ->
      throw_type_error(
        st,
        "Super constructor null of derived class is not a constructor",
      )
  }
}

// ── C14: KCompiled slot readers (JRead) ─────────────────────────────────────

/// `[[HomeObject]]` of a function cell, or `undefined` for non-functions /
/// unset home. JRead — for M14's prologue emission.
pub fn t_fn_home_object(st: Agent, fn_h: Handle) -> JsVal {
  case rt_store.t_cell_get(st, fn_h) {
    SObject(kind: KCompiled(home_object: Some(h), ..), ..)
    | SObject(kind: KBytecode(home_object: Some(h), ..), ..) -> mk_object(h)
    _ -> mk_undefined()
  }
}

/// `FnFlags` of a function cell. Panics on a native/bound handle — a
/// compiler-emitted call site guarantees the handle is a closure. JRead.
pub fn t_fn_flags(st: Agent, fn_h: Handle) -> FnFlags {
  case rt_store.t_cell_get(st, fn_h) {
    SObject(kind: KCompiled(flags:, ..), ..)
    | SObject(kind: KBytecode(flags:, ..), ..) -> flags
    _ -> panic as "t_fn_flags: Handle is not a function closure cell"
  }
}

// ── C15: IsConstructor (JRead) ──────────────────────────────────────────────

/// §7.2.4 IsConstructor as a bare `Bool` — thin re-export of
/// `rt_call.is_constructor` for the M9 dispatch table. JRead.
pub fn t_is_constructor(st: Agent, v: JsVal) -> Bool {
  rt_call.is_constructor(st, v)
}
