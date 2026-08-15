//// ES2021 §26.2 FinalizationRegistry Objects — port of
//// `arc/vm/builtins/finalization_registry.gleam`.
////
//// A FinalizationRegistry lets code request a cleanup callback when a
//// registered target is garbage collected. Each cell's [[WeakRefTarget]] and
//// [[UnregisterToken]] are held weakly (`gc.prune_weak` drops a cell whose
//// target died); the cleanup callback itself never fires, which §9.10.3
//// permits (HostEnqueueFinalizationRegistryCleanupJob is optional). The
//// constructor and the register/unregister bookkeeping follow the spec
//// exactly.

import arc/rt/builtins/common
import arc/rt/builtins/helpers.{can_be_held_weakly}
import arc/rt/call as rt_call
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type FinRegCell, type FinalizationRegistryNative,
  type Handle, type JsVal, FinRegCell, FinalizationRegistryConstructor,
  FinalizationRegistryN, FinalizationRegistryObj,
  FinalizationRegistryPrototypeRegister, FinalizationRegistryPrototypeUnregister,
  KUndef, NoElements, SObject, classify, mk_bool, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}

/// Set up FinalizationRegistry.prototype and the FinalizationRegistry
/// constructor.
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
  let st = common.add_to_string_tag(st, bt.prototype, "FinalizationRegistry")
  #(bt, st)
}

/// Per-module [[Call]] dispatch for FinalizationRegistry native functions.
pub fn dispatch(
  st: Agent,
  native: FinalizationRegistryNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    // §26.2.1.1 step 1: If NewTarget is undefined, throw a TypeError — a
    // plain call routes here; `new` goes through `dispatch_construct`.
    FinalizationRegistryConstructor(..) ->
      rt_val.t_throw_type_error(
        st,
        "Constructor FinalizationRegistry requires 'new'",
      )
    FinalizationRegistryPrototypeRegister -> register(st, this, args)
    FinalizationRegistryPrototypeUnregister -> unregister(st, this, args)
  }
}

/// [[Construct]] dispatch: only the constructor is constructible.
pub fn dispatch_construct(
  st: Agent,
  native: FinalizationRegistryNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    FinalizationRegistryConstructor(proto:) ->
      construct(st, proto, args, new_target)
    FinalizationRegistryPrototypeRegister
    | FinalizationRegistryPrototypeUnregister ->
      rt_val.t_throw_type_error(st, "not a constructor")
  }
}

/// §26.2.1.1 FinalizationRegistry ( cleanupCallback )
///
///   1. If NewTarget is undefined, throw a TypeError exception.
///   2. If IsCallable(cleanupCallback) is false, throw a TypeError exception.
///   3. Let finalizationRegistry be ? OrdinaryCreateFromConstructor(NewTarget,
///      "%FinalizationRegistry.prototype%", « [[Realm]], [[CleanupCallback]],
///      [[Cells]] »).
///   4-5. Set [[CleanupCallback]]; [[Cells]] starts empty.
///   6. Return finalizationRegistry.
fn construct(
  st: Agent,
  proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  // Step 1 is the call/construct split: only `new` reaches here.
  // Step 2
  let callback = helpers.first_arg_or_undefined(args)
  let #(callable, _) = rt_val.t_is_callable(st, callback)
  case callable {
    False -> rt_val.t_throw_type_error(st, "cleanup must be callable")
    True -> {
      // Step 3: GetPrototypeFromConstructor(NewTarget, intrinsic). The realm
      // record has no %FinalizationRegistry% slot, so the intrinsic default
      // is the constructor's own.
      let #(proto_h, st) =
        rt_call.get_prototype_from_constructor(st, new_target, fn(_realm) {
          proto
        })
      rt_store.t_cell_new(
        st,
        SObject(
          kind: FinalizationRegistryObj(callback:, cells: []),
          proto: Some(proto_h),
          props: dict.new(),
          symbol_props: [],
          elements: NoElements,
          extensible: True,
        ),
      )
    }
  }
}

/// §26.2.3.2 FinalizationRegistry.prototype.register ( target, heldValue
/// [ , unregisterToken ] )
///
///   1-2. RequireInternalSlot(finalizationRegistry, [[Cells]]).
///   3. If CanBeHeldWeakly(target) is false, throw a TypeError exception.
///   4. If SameValue(target, heldValue) is true, throw a TypeError exception.
///   5. If CanBeHeldWeakly(unregisterToken) is false, then
///      a. If unregisterToken is not undefined, throw a TypeError exception.
///      b. Set unregisterToken to empty.
///   6-7. Append the new cell to [[Cells]].
///   8. Return undefined.
fn register(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use registry <- require_registry(st, this, "register")
  let #(target, held, token_arg) = helpers.three_args_or_undefined(args)
  // Step 3
  use Nil <- helpers.guard(can_be_held_weakly(target), fn() {
    rt_val.t_throw_type_error(st, "Invalid value used as weak ref target")
  })
  // Step 4
  use Nil <- helpers.guard(!rt_val.same_value(target, held), fn() {
    rt_val.t_throw_type_error(st, "target and holdings must not be same")
  })
  // Step 5
  case can_be_held_weakly(token_arg), classify(token_arg) {
    False, KUndef -> do_register(st, registry, target, held, None)
    False, _ ->
      rt_val.t_throw_type_error(st, "Invalid value used as unregister token")
    True, _ -> do_register(st, registry, target, held, Some(token_arg))
  }
}

/// Steps 6-8 of register — append the cell and return undefined.
fn do_register(
  st: Agent,
  registry: RegistryRef,
  target: JsVal,
  held: JsVal,
  token: Option(JsVal),
) -> #(JsVal, Agent) {
  let cell = FinRegCell(target:, held:, token:)
  // [[Cells]] is append-ordered in the spec; order is unobservable here
  // (no iteration, cleanup never fires), so prepend for O(1).
  #(mk_undefined(), update_cells(st, registry, fn(cells) { [cell, ..cells] }))
}

/// §26.2.3.3 FinalizationRegistry.prototype.unregister ( unregisterToken )
///
///   1-2. RequireInternalSlot(finalizationRegistry, [[Cells]]).
///   3. If CanBeHeldWeakly(unregisterToken) is false, throw a TypeError.
///   4-5. Remove every cell whose [[UnregisterToken]] is not empty and
///        SameValue(cell.[[UnregisterToken]], unregisterToken) is true.
///   6. Return whether any cell was removed.
fn unregister(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use registry <- require_registry(st, this, "unregister")
  let token = helpers.first_arg_or_undefined(args)
  case can_be_held_weakly(token) {
    False ->
      rt_val.t_throw_type_error(st, "Invalid value used as unregister token")
    True -> {
      let #(removed, kept) =
        list.partition(read_cells(st, registry), fn(cell) {
          case cell.token {
            Some(t) -> rt_val.same_value(t, token)
            None -> False
          }
        })
      let st = update_cells(st, registry, fn(_) { kept })
      #(mk_bool(removed != []), st)
    }
  }
}

/// A `Handle` that has been *proved* to point at a FinalizationRegistry's
/// heap slot. Constructible only by `require_registry`, so `read_cells` /
/// `update_cells` cannot be reached with a handle of any other kind.
type RegistryRef {
  RegistryRef(Handle)
}

/// RequireInternalSlot(this, [[Cells]]) — this must be an object with the
/// FinalizationRegistry brand, else TypeError. CPS-style.
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

fn read_cells(st: Agent, registry: RegistryRef) -> List(FinRegCell) {
  let RegistryRef(h) = registry
  let assert SObject(kind: FinalizationRegistryObj(cells:, ..), ..) =
    rt_store.t_cell_get(st, h)
    as "finalization_registry: RegistryRef does not point at a registry slot"
  cells
}

/// Read-modify-write [[Cells]] inside a single heap access, carrying the
/// slot's [[CleanupCallback]] across unchanged.
fn update_cells(
  st: Agent,
  registry: RegistryRef,
  f: fn(List(FinRegCell)) -> List(FinRegCell),
) -> Agent {
  let RegistryRef(h) = registry
  rt_store.t_cell_update(st, h, fn(slot) {
    let assert SObject(kind: FinalizationRegistryObj(callback:, cells:), ..) =
      slot
    SObject(..slot, kind: FinalizationRegistryObj(callback:, cells: f(cells)))
  })
}
