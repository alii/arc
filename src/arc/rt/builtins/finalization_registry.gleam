// cleanup callbacks never fire, which the spec permits

import arc/rt/builtins/common
import arc/rt/builtins/helpers.{can_be_held_weakly}
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type FinalizationRegistryNative, type Handle,
  type JsVal, type Realm, type Registration, FinalizationRegistryConstructor,
  FinalizationRegistryN, FinalizationRegistryObj,
  FinalizationRegistryPrototypeRegister, FinalizationRegistryPrototypeUnregister,
  KUndef, Registration, SObject, classify, mk_bool, mk_undefined,
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
      #(
        "register",
        FinalizationRegistryN(FinalizationRegistryPrototypeRegister),
        2,
      ),
      #(
        "unregister",
        FinalizationRegistryN(FinalizationRegistryPrototypeUnregister),
        1,
      ),
    ])
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      proto_methods,
      fn(proto) {
        FinalizationRegistryN(FinalizationRegistryConstructor(proto:))
      },
      "FinalizationRegistry",
      1,
      [],
    )
  let st = common.add_string_tag(st, bt.prototype, "FinalizationRegistry")
  #(bt, st)
}

pub fn dispatch(
  st: Agent,
  native: FinalizationRegistryNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    FinalizationRegistryConstructor(..) ->
      rt_val.t_throw_type_error(
        st,
        "Constructor FinalizationRegistry requires 'new'",
      )
    FinalizationRegistryPrototypeRegister -> register(st, this, args)
    FinalizationRegistryPrototypeUnregister -> unregister(st, this, args)
  }
}

pub fn dispatch_construct(
  st: Agent,
  native: FinalizationRegistryNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    FinalizationRegistryConstructor(..) -> construct(st, args, new_target)
    FinalizationRegistryPrototypeRegister
    | FinalizationRegistryPrototypeUnregister ->
      rt_val.t_throw_type_error(st, "not a constructor")
  }
}

fn construct(
  st: Agent,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let callback = helpers.first_arg_or_undefined(args)
  use Nil <- helpers.guard(rt_val.is_callable(st, callback), fn() {
    rt_val.t_throw_type_error(st, "cleanup must be callable")
  })
  let #(proto_h, st) =
    rt_call.get_prototype_from_constructor(st, new_target, fn(realm: Realm) {
      realm.finalization_registry.prototype
    })
  realm_ops.alloc_object(
    st,
    FinalizationRegistryObj(callback:, registrations: []),
    proto_h,
  )
}

fn register(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use registry <- require_registry(st, this, "register")
  let #(target, held, token_arg) = helpers.three_args_or_undefined(args)
  use Nil <- helpers.guard(can_be_held_weakly(target), fn() {
    rt_val.t_throw_type_error(st, "Invalid value used as weak ref target")
  })
  use Nil <- helpers.guard(!rt_val.same_value(target, held), fn() {
    rt_val.t_throw_type_error(st, "target and holdings must not be same")
  })
  case can_be_held_weakly(token_arg), classify(token_arg) {
    False, KUndef -> do_register(st, registry, target, held, None)
    False, _ ->
      rt_val.t_throw_type_error(st, "Invalid value used as unregister token")
    True, _ -> do_register(st, registry, target, held, Some(token_arg))
  }
}

fn do_register(
  st: Agent,
  registry: RegistryRef,
  target: JsVal,
  held: JsVal,
  unregister_token: Option(JsVal),
) -> #(JsVal, Agent) {
  let registration = Registration(target:, held:, unregister_token:)
  // order is unobservable, so prepend
  #(
    mk_undefined(),
    update_registrations(st, registry, fn(rs) { [registration, ..rs] }),
  )
}

fn unregister(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use registry <- require_registry(st, this, "unregister")
  let token = helpers.first_arg_or_undefined(args)
  use Nil <- helpers.guard(can_be_held_weakly(token), fn() {
    rt_val.t_throw_type_error(st, "Invalid value used as unregister token")
  })
  let #(removed, kept) =
    list.partition(read_registrations(st, registry), fn(r) {
      case r.unregister_token {
        Some(t) -> rt_val.same_value(t, token)
        None -> False
      }
    })
  let st = update_registrations(st, registry, fn(_rs) { kept })
  #(mk_bool(removed != []), st)
}

// only built by require_registry
type RegistryRef {
  RegistryRef(Handle)
}

fn require_registry(
  st: Agent,
  this: JsVal,
  method: String,
  cont: fn(RegistryRef) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use _nil, h <- helpers.require_brand(
    st,
    this,
    fn() {
      "FinalizationRegistry.prototype."
      <> method
      <> " called on incompatible receiver"
    },
    fn(kind) {
      case kind {
        FinalizationRegistryObj(..) -> Some(Nil)
        _ -> None
      }
    },
  )
  cont(RegistryRef(h))
}

fn read_registrations(st: Agent, registry: RegistryRef) -> List(Registration) {
  let RegistryRef(h) = registry
  let assert SObject(kind: FinalizationRegistryObj(registrations:, ..), ..) =
    rt_store.t_cell_get(st, h)
    as "finalization_registry: RegistryRef does not point at a registry cell"
  registrations
}

fn update_registrations(
  st: Agent,
  registry: RegistryRef,
  f: fn(List(Registration)) -> List(Registration),
) -> Agent {
  let RegistryRef(h) = registry
  rt_store.t_cell_update(st, h, fn(cell) {
    let assert SObject(
      kind: FinalizationRegistryObj(callback:, registrations:),
      ..,
    ) = cell
    SObject(
      ..cell,
      kind: FinalizationRegistryObj(callback:, registrations: f(registrations)),
    )
  })
}
