//// Opcode bodies for the compiler's class and object-literal lowering:
//// method/accessor/field definition, private names, `super` property
//// access, derived-class wiring. Each takes the running `State` (operands
//// on the stack) and returns the next state or a throw; the step function's
//// arms are one call each.

import arc/interp/call
import arc/interp/ffi
import arc/interp/state.{
  type State, type StepExit, StackUnderflow, State, VmFailed,
}
import arc/rt/class as rt_class
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type MethodInstallKind, type ObjectKey,
  DataProperty, KHandle, KNull, MIGetter, MIMethod, MISetter, Named, SObject,
  StringKey, classify, mk_bool, mk_null, mk_object,
}
import arc/rt/val as rt_val
import arc/vm/key.{type PropertyKey}
import arc/vm/opcode.{type AccessorKind, Getter, Setter}
import gleam/option.{None, Some}
import gleam/result

fn underflow(state: State, op: String) -> Result(State, StepExit) {
  Error(VmFailed(StackUnderflow(op), state))
}

fn done(state: State, stack: List(JsVal)) -> State {
  State(..state, stack:, pc: state.pc + 1)
}

fn accessor_install_kind(kind: AccessorKind) -> MethodInstallKind {
  case kind {
    Getter -> MIGetter
    Setter -> MISetter
  }
}

/// The runtime `ObjectKey` for a compile-time property key operand.
pub fn object_key(pk: PropertyKey) -> ObjectKey {
  case pk {
    key.Index(n) -> StringKey(types.Index(n))
    key.Named(name) -> StringKey(Named(name))
    key.Private(text) -> StringKey(types.Private(<<text:utf8>>))
  }
}

fn to_property_key(
  state: State,
  v: JsVal,
) -> Result(#(ObjectKey, State), StepExit) {
  ffi.guarded(ffi.guard2(rt_val.t_to_property_key, state.agent, v), state)
}

// -- private names (§15.7.14 step 5-6, §7.3.28-32) ------------------------------

/// NewPrivateName(name): mint this class evaluation's key for `#name`.
pub fn new_private_name(state: State, name: String) -> Result(State, StepExit) {
  let #(k, agent) = rt_class.t_new_private_name(state.agent, name)
  Ok(done(State(..state, agent:), [k, ..state.stack]))
}

/// GetPrivateFieldDyn: [key, obj, ..] → [val, ..]; the `2` form keeps the
/// receiver under the value for a following compound write.
pub fn get_private(
  state: State,
  keep_receiver: Bool,
) -> Result(State, StepExit) {
  case state.stack {
    [priv_key, obj, ..rest] -> {
      use #(v, state) <- result.map(ffi.guarded(
        ffi.guard3(rt_class.t_private_get, state.agent, obj, priv_key),
        state,
      ))
      case keep_receiver {
        True -> done(state, [v, obj, ..rest])
        False -> done(state, [v, ..rest])
      }
    }
    _ -> underflow(state, "GetPrivateFieldDyn")
  }
}

/// PutPrivateFieldDyn: [key, val, obj, ..] → [val, ..].
pub fn put_private(state: State) -> Result(State, StepExit) {
  case state.stack {
    [priv_key, v, obj, ..rest] -> {
      use #(v, state) <- result.map(ffi.guarded(
        ffi.guard4(rt_class.t_private_set, state.agent, obj, priv_key, v),
        state,
      ))
      done(state, [v, ..rest])
    }
    _ -> underflow(state, "PutPrivateFieldDyn")
  }
}

/// PrivateInDyn: [key, obj, ..] → [bool, ..].
pub fn private_in(state: State) -> Result(State, StepExit) {
  case state.stack {
    [priv_key, obj, ..rest] -> {
      use #(found, state) <- result.map(
        call.guarded(state, fn(agent) {
          #(rt_class.t_private_in(agent, obj, priv_key), agent)
        }),
      )
      done(state, [mk_bool(found), ..rest])
    }
    _ -> underflow(state, "PrivateInDyn")
  }
}

/// DefinePrivateField: [val, key, obj, ..] → [obj, ..].
pub fn define_private_field(state: State) -> Result(State, StepExit) {
  case state.stack {
    [v, priv_key, obj, ..rest] -> {
      use obj_h <- with_object(state, obj, "DefinePrivateField")
      use state <- result.map(
        call.guarded_unit(state, rt_class.t_private_define(
          _,
          obj_h,
          priv_key,
          v,
        )),
      )
      done(state, [obj, ..rest])
    }
    _ -> underflow(state, "DefinePrivateField")
  }
}

/// DefinePrivateMethod / DefinePrivateAccessor(kind): [fn, key, obj, ..] →
/// [obj, ..].
pub fn define_private_method(
  state: State,
  kind: MethodInstallKind,
) -> Result(State, StepExit) {
  case state.stack {
    [f, priv_key, obj, ..rest] -> {
      use obj_h <- with_object(state, obj, "DefinePrivateMethod")
      use state <- result.map(
        call.guarded_unit(state, rt_class.t_define_private(
          _,
          obj_h,
          priv_key,
          f,
          kind,
        )),
      )
      done(state, [obj, ..rest])
    }
    _ -> underflow(state, "DefinePrivateMethod")
  }
}

/// DefinePrivateAccessor(kind).
pub fn define_private_accessor(
  state: State,
  kind: AccessorKind,
) -> Result(State, StepExit) {
  define_private_method(state, accessor_install_kind(kind))
}

fn with_object(
  state: State,
  v: JsVal,
  op: String,
  k: fn(Handle) -> Result(State, StepExit),
) -> Result(State, StepExit) {
  case classify(v) {
    KHandle(h) -> k(h)
    _ ->
      Error(VmFailed(state.InternalError(op, "target is not an object"), state))
  }
}

// -- public fields / methods / accessors ---------------------------------------

/// DefineField(key): [value, obj, ..] → [obj, ..]. §7.3.7
/// CreateDataPropertyOrThrow: proxies fire their trap, frozen or
/// non-extensible receivers throw. A non-object receiver is a no-op.
pub fn define_field(state: State, pk: PropertyKey) -> Result(State, StepExit) {
  case state.stack {
    [v, obj, ..rest] ->
      case classify(obj) {
        KHandle(h) -> {
          use state <- result.map(create_data_property_or_throw(
            state,
            h,
            object_key(pk),
            v,
          ))
          done(state, [obj, ..rest])
        }
        _ -> Ok(State(..state, pc: state.pc + 1))
      }
    _ -> underflow(state, "DefineField")
  }
}

/// DefineFieldComputed: [value, key, obj, ..] → [obj, ..], key through
/// ToPropertyKey.
pub fn define_field_computed(state: State) -> Result(State, StepExit) {
  case state.stack {
    [v, k, obj, ..rest] ->
      case classify(obj) {
        KHandle(h) -> {
          use #(okey, state) <- result.try(to_property_key(state, k))
          use state <- result.map(create_data_property_or_throw(
            state,
            h,
            okey,
            v,
          ))
          done(state, [obj, ..rest])
        }
        _ -> Ok(done(state, rest))
      }
    _ -> underflow(state, "DefineFieldComputed")
  }
}

fn create_data_property_or_throw(
  state: State,
  h: Handle,
  okey: ObjectKey,
  v: JsVal,
) -> Result(State, StepExit) {
  use #(ok, state) <- result.try(ffi.guarded(
    ffi.guard7(
      rt_obj.t_define_own_data,
      state.agent,
      h,
      okey,
      v,
      True,
      True,
      True,
    ),
    state,
  ))
  case ok {
    True -> Ok(state)
    False ->
      state.throw_type_error(
        state,
        "Cannot define property "
          <> object_key_text(okey)
          <> ", object is not extensible",
      )
  }
}

fn object_key_text(okey: ObjectKey) -> String {
  case okey {
    StringKey(pk) -> types.key_display_string(pk)
    types.SymbolKey(sym) -> types.symbol_descriptive_string(sym)
  }
}

/// DefineMethod(key): [fn, obj, ..] → [obj, ..]. Non-enumerable, sets
/// [[HomeObject]] (§15.4.4 MakeMethod).
pub fn define_method(state: State, pk: PropertyKey) -> Result(State, StepExit) {
  case state.stack {
    [f, obj, ..rest] ->
      install(state, obj, object_key(pk), f, MIMethod, False, rest)
    _ -> underflow(state, "DefineMethod")
  }
}

/// DefineMethodComputed: [fn, key, obj, ..] → [obj, ..].
pub fn define_method_computed(state: State) -> Result(State, StepExit) {
  case state.stack {
    [f, k, obj, ..rest] -> {
      use #(okey, state) <- result.try(to_property_key(state, k))
      install(state, obj, okey, f, MIMethod, False, rest)
    }
    _ -> underflow(state, "DefineMethodComputed")
  }
}

/// DefineAccessor(key, kind, enumerable): [fn, obj, ..] → [obj, ..].
pub fn define_accessor(
  state: State,
  pk: PropertyKey,
  kind: AccessorKind,
  enumerable: Bool,
) -> Result(State, StepExit) {
  case state.stack {
    [f, obj, ..rest] ->
      install(
        state,
        obj,
        object_key(pk),
        f,
        accessor_install_kind(kind),
        enumerable,
        rest,
      )
    _ -> underflow(state, "DefineAccessor")
  }
}

/// DefineAccessorComputed(kind, enumerable): [fn, key, obj, ..] → [obj, ..].
pub fn define_accessor_computed(
  state: State,
  kind: AccessorKind,
  enumerable: Bool,
) -> Result(State, StepExit) {
  case state.stack {
    [f, k, obj, ..rest] -> {
      use #(okey, state) <- result.try(to_property_key(state, k))
      install(
        state,
        obj,
        okey,
        f,
        accessor_install_kind(kind),
        enumerable,
        rest,
      )
    }
    _ -> underflow(state, "DefineAccessorComputed")
  }
}

/// `rt/class.t_define_method` on an object receiver with a function value;
/// anything else is skipped (the emitter never produces it).
fn install(
  state: State,
  obj: JsVal,
  okey: ObjectKey,
  f: JsVal,
  kind: MethodInstallKind,
  enumerable: Bool,
  rest: List(JsVal),
) -> Result(State, StepExit) {
  case classify(obj), classify(f) {
    KHandle(obj_h), KHandle(fn_h) -> {
      use state <- result.map(
        call.guarded_unit(state, rt_class.t_define_method(
          _,
          obj_h,
          okey,
          fn_h,
          kind,
          enumerable,
        )),
      )
      done(state, [obj, ..rest])
    }
    _, _ -> Ok(State(..state, pc: state.pc + 1))
  }
}

/// MakeMethod: [fn, obj, ..] unchanged; sets fn.[[HomeObject]] = obj.
pub fn make_method(state: State) -> Result(State, StepExit) {
  case state.stack {
    [f, obj, ..] -> {
      let agent = case classify(f), classify(obj) {
        KHandle(fn_h), KHandle(obj_h) ->
          rt_class.t_make_method(state.agent, fn_h, obj_h)
        _, _ -> state.agent
      }
      Ok(State(..state, agent:, pc: state.pc + 1))
    }
    _ -> underflow(state, "MakeMethod")
  }
}

/// SetProto: [val, obj, ..] → [obj, ..]. Annex B §B.3.1 `__proto__: v` on a
/// fresh literal: object or null becomes the [[Prototype]], else ignored.
pub fn set_proto(state: State) -> Result(State, StepExit) {
  case state.stack {
    [v, obj, ..rest] -> {
      let agent = case classify(obj) {
        KHandle(h) -> {
          let #(_, agent) = rt_obj.t_set_proto(state.agent, h, v)
          agent
        }
        _ -> state.agent
      }
      Ok(done(State(..state, agent:), [obj, ..rest]))
    }
    _ -> underflow(state, "SetProto")
  }
}

// -- classes ------------------------------------------------------------------

/// GetPrototypeOf: [obj] → [proto | null]. The second hop of both `super.x`
/// (home object → its prototype) and `super()` (active function → parent
/// constructor). Reads the slot directly (QuickJS OP_get_super).
pub fn get_prototype_of(state: State) -> Result(State, StepExit) {
  case state.stack {
    [obj, ..rest] -> {
      let proto = case classify(obj) {
        KHandle(h) ->
          case ordinary_proto(state.agent, h) {
            Some(p) -> mk_object(p)
            None -> mk_null()
          }
        _ -> mk_null()
      }
      Ok(done(state, [proto, ..rest]))
    }
    _ -> underflow(state, "GetPrototypeOf")
  }
}

fn ordinary_proto(agent: Agent, h: Handle) -> option.Option(Handle) {
  case rt_store.t_cell_get(agent, h) {
    SObject(proto:, ..) -> proto
    types.SShapedObject(proto:, ..) -> proto
    _ -> None
  }
}

/// GetSuperValue: [key, base, this, ..] → [val, ..]. OrdinaryGet on `base`
/// with receiver `this`. The `2` form leaves [val, key', base, this, ..] with
/// the key already through ToPropertyKey for the paired PutSuperValue.
pub fn get_super_value(
  state: State,
  keep_base: Bool,
) -> Result(State, StepExit) {
  case state.stack {
    [k, base, this_val, ..rest] ->
      case classify(base) {
        KHandle(base_h) -> {
          use #(okey, state) <- result.try(to_property_key(state, k))
          use #(v, state) <- result.map(ffi.guarded(
            ffi.guard4(
              rt_obj.t_get_prop_with_receiver,
              state.agent,
              base_h,
              okey,
              this_val,
            ),
            state,
          ))
          case keep_base {
            True ->
              done(state, [
                v,
                rt_obj.object_key_value(okey),
                base,
                this_val,
                ..rest
              ])
            False -> done(state, [v, ..rest])
          }
        }
        // §13.3.7.3 step 5: base is null when the home object's prototype
        // is null (`class C extends null`).
        _ ->
          state.throw_type_error(
            state,
            "Cannot read super property when prototype is null",
          )
      }
    _ -> underflow(state, "GetSuperValue")
  }
}

/// PutSuperValue: [val, key, base, this, ..] → [val, ..]. OrdinarySet on
/// `base` with receiver `this`; a rejected write throws only in strict code
/// (§6.2.5.6 PutValue step 5.c).
pub fn put_super_value(state: State) -> Result(State, StepExit) {
  case state.stack {
    [v, k, base, this_val, ..rest] ->
      case classify(base) {
        KHandle(base_h) -> {
          use #(okey, state) <- result.try(to_property_key(state, k))
          use #(ok, state) <- result.try(ffi.guarded(
            ffi.guard5(
              rt_obj.t_set_prop_with_receiver,
              state.agent,
              base_h,
              okey,
              v,
              this_val,
            ),
            state,
          ))
          case ok, state.func.is_strict {
            False, True ->
              state.throw_type_error(
                state,
                "Cannot assign to read-only super property",
              )
            _, _ -> Ok(done(state, [v, ..rest]))
          }
        }
        _ ->
          state.throw_type_error(
            state,
            "Cannot write super property when prototype is null",
          )
      }
    _ -> underflow(state, "PutSuperValue")
  }
}

/// SetupDerivedClass: [ctor, parent, ..] → [ctor, ..]. §15.7.14 steps 8-9:
/// validate the heritage, then wire `ctor.prototype.[[Prototype]] =
/// parent.prototype`, `ctor.[[HomeObject]] = ctor.prototype` and
/// `ctor.[[Prototype]] = parent`.
pub fn setup_derived_class(state: State) -> Result(State, StepExit) {
  case state.stack {
    [ctor, parent, ..rest] ->
      case classify(ctor), classify(parent) {
        KHandle(ctor_h), KHandle(parent_h) ->
          // Step 8.f: IsConstructor BEFORE Get(superclass, "prototype").
          case rt_class.t_is_constructor(state.agent, parent) {
            False ->
              state.throw_type_error(
                state,
                "Class extends value is not a constructor or null",
              )
            True -> {
              // Step 8.g: protoParent = ? Get(superclass, "prototype").
              use #(pp, state) <- result.try(ffi.guarded(
                ffi.guard3(
                  rt_obj.t_get_prop,
                  state.agent,
                  parent,
                  StringKey(Named("prototype")),
                ),
                state,
              ))
              use proto_parent <- result.try(case classify(pp) {
                KHandle(p) -> Ok(Some(p))
                KNull -> Ok(None)
                _ ->
                  state.throw_type_error(
                    state,
                    "Class extends value does not have valid prototype property",
                  )
              })
              let agent = wire_class(state.agent, ctor_h, proto_parent)
              let #(_, agent) =
                rt_obj.t_set_prototype(agent, ctor_h, Some(parent_h))
              Ok(done(State(..state, agent:), [ctor, ..rest]))
            }
          }
        // `extends null`: prototype chain ends at the class prototype.
        KHandle(ctor_h), KNull -> {
          let agent = wire_class(state.agent, ctor_h, None)
          Ok(done(State(..state, agent:), [ctor, ..rest]))
        }
        _, _ ->
          state.throw_type_error(
            state,
            "Class extends value is not a constructor or null",
          )
      }
    _ -> underflow(state, "SetupDerivedClass")
  }
}

/// `ctor.prototype.[[Prototype]] = proto_parent` and `ctor.[[HomeObject]] =
/// ctor.prototype`, when the closure carries its own `prototype` object.
fn wire_class(
  agent: Agent,
  ctor_h: Handle,
  proto_parent: option.Option(Handle),
) -> Agent {
  case own_prototype_object(agent, ctor_h) {
    None -> agent
    Some(proto_h) -> {
      let agent = rt_class.t_make_method(agent, ctor_h, proto_h)
      let #(_, agent) = rt_obj.t_set_prototype(agent, proto_h, proto_parent)
      agent
    }
  }
}

fn own_prototype_object(agent: Agent, fn_h: Handle) -> option.Option(Handle) {
  case
    rt_obj.t_ordinary_own_property(agent, fn_h, StringKey(Named("prototype")))
  {
    Some(DataProperty(value:, ..)) ->
      case classify(value) {
        KHandle(p) -> Some(p)
        _ -> None
      }
    _ -> None
  }
}
