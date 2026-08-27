import arc/rt/builtins/common
import arc/rt/builtins/helpers.{can_be_held_weakly}
import arc/rt/call as rt_call
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type Realm,
  type WeakRefNative, NoElements, SObject, WeakRefConstructor, WeakRefN,
  WeakRefObj, WeakRefPrototypeDeref, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{None, Some}

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("deref", WeakRefN(WeakRefPrototypeDeref), 0),
    ])
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      proto_methods,
      fn(_proto) { WeakRefN(WeakRefConstructor) },
      "WeakRef",
      1,
      [],
    )
  let st = common.add_to_string_tag(st, bt.prototype, "WeakRef")
  #(bt, st)
}

pub fn dispatch(
  st: Agent,
  native: WeakRefNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    WeakRefConstructor ->
      rt_val.t_throw_type_error(st, "Constructor WeakRef requires 'new'")
    WeakRefPrototypeDeref -> deref(st, this, args)
  }
}

pub fn dispatch_construct(
  st: Agent,
  native: WeakRefNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    WeakRefConstructor -> construct(st, args, new_target)
    WeakRefPrototypeDeref -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// §26.1.1.1 weakref(target)
fn construct(
  st: Agent,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let target = helpers.first_arg_or_undefined(args)
  use Nil <- helpers.guard(can_be_held_weakly(target), fn() {
    rt_val.t_throw_type_error(st, "Invalid value used as weak ref target")
  })
  let #(proto_h, st) =
    rt_call.get_prototype_from_constructor(st, new_target, fn(realm: Realm) {
      realm.weak_ref.prototype
    })
  rt_store.t_cell_new(
    st,
    SObject(
      kind: WeakRefObj(target: Some(target)),
      proto: Some(proto_h),
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

// §26.1.3.2 weakref.prototype.deref
fn deref(st: Agent, this: JsVal, _args: List(JsVal)) -> #(JsVal, Agent) {
  use target, _h <- helpers.require_brand(
    st,
    this,
    fn() { "WeakRef.prototype.deref called on incompatible receiver" },
    fn(kind) {
      case kind {
        WeakRefObj(target:) -> Some(target)
        _ -> None
      }
    },
  )
  #(option.unwrap(target, mk_undefined()), st)
}
