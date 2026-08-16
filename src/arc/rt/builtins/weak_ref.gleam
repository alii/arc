//// ES2021 §26.1 WeakRef Objects.
////
//// A WeakRef holds its [[WeakRefTarget]] weakly: `gc.mark` does not trace
//// it and `gc.prune_weak` empties the slot once the target's cell has been
//// swept, so `deref` answers `undefined` from then on. Collection only runs
//// at safepoints between jobs, which is what AddToKeptObjects (§9.10.4)
//// guarantees: a target observed during a job stays alive for the rest of
//// that job.

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

/// Set up WeakRef.prototype and the WeakRef constructor.
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

/// Per-module [[Call]] dispatch for WeakRef native functions.
pub fn dispatch(
  st: Agent,
  native: WeakRefNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    // §26.1.1.1 step 1: If NewTarget is undefined, throw a TypeError — a
    // plain call routes here; `new` goes through `dispatch_construct`.
    WeakRefConstructor ->
      rt_val.t_throw_type_error(st, "Constructor WeakRef requires 'new'")
    WeakRefPrototypeDeref -> deref(st, this, args)
  }
}

/// [[Construct]] dispatch: only the constructor is constructible.
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

/// §26.1.1.1 WeakRef ( target )
///
///   1. If NewTarget is undefined, throw a TypeError exception.
///   2. If CanBeHeldWeakly(target) is false, throw a TypeError exception.
///   3. Let weakRef be ? OrdinaryCreateFromConstructor(NewTarget,
///      "%WeakRef.prototype%", « [[WeakRefTarget]] »).
///   4. Perform AddToKeptObjects(target).
///   5. Set weakRef.[[WeakRefTarget]] to target.
///   6. Return weakRef.
fn construct(
  st: Agent,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  // Step 1 is the call/construct split: only `new` reaches here.
  let target = helpers.first_arg_or_undefined(args)
  // Step 2
  use Nil <- helpers.guard(can_be_held_weakly(target), fn() {
    rt_val.t_throw_type_error(st, "Invalid value used as weak ref target")
  })
  // Step 3: GetPrototypeFromConstructor(NewTarget, %WeakRef.prototype%) —
  // the fallback comes from NewTarget's realm.
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

/// §26.1.3.2 WeakRef.prototype.deref ( )
///
///   1-2. RequireInternalSlot(weakRef, [[WeakRefTarget]]).
///   3. Return WeakRefDeref(weakRef): the target when it is not empty
///      (after AddToKeptObjects), else undefined.
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
