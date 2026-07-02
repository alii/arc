/// ES2021 §26.2 FinalizationRegistry Objects
///
/// A FinalizationRegistry lets code request a cleanup callback when a
/// registered target is garbage collected. In this implementation objects are
/// never collected while reachable from a registry (same deviation as
/// WeakMap/WeakSet), so cleanup callbacks never fire — but the constructor and
/// the register/unregister bookkeeping follow the spec exactly.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{can_be_held_weakly}
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/object
import arc/vm/state.{type State, State}
import arc/vm/value.{
  type FinalizationRegistryNativeFn, type JsValue, type Ref, Dispatch,
  FinRegCell, FinalizationRegistryConstructor, FinalizationRegistryNative,
  FinalizationRegistryObject, FinalizationRegistryPrototypeRegister,
  FinalizationRegistryPrototypeUnregister, JsBool, JsObject, JsUndefined,
  ObjectSlot,
}
import gleam/dict
import gleam/list
import gleam/option.{None, Some}

/// Set up FinalizationRegistry.prototype and the FinalizationRegistry
/// constructor.
pub fn init(
  h: state.Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(state.Heap(host), BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #(
        "register",
        FinalizationRegistryNative(FinalizationRegistryPrototypeRegister),
        2,
      ),
      #(
        "unregister",
        FinalizationRegistryNative(FinalizationRegistryPrototypeUnregister),
        1,
      ),
    ])

  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(proto) {
        Dispatch(
          FinalizationRegistryNative(FinalizationRegistryConstructor(proto:)),
        )
      },
      "FinalizationRegistry",
      1,
      [],
    )
  let h = common.add_to_string_tag(h, bt.prototype, "FinalizationRegistry")
  #(h, bt)
}

/// Per-module dispatch for FinalizationRegistry native functions.
pub fn dispatch(
  native: FinalizationRegistryNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    FinalizationRegistryConstructor(proto:) -> construct(proto, args, state)
    FinalizationRegistryPrototypeRegister -> register(this, args, state)
    FinalizationRegistryPrototypeUnregister -> unregister(this, args, state)
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
  proto: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: do_construct sets state.new_target before native dispatch;
  // a plain call leaves it JsUndefined.
  case state.new_target {
    JsUndefined ->
      state.type_error(state, "Constructor FinalizationRegistry requires 'new'")
    new_target -> {
      // Step 2
      let callback = helpers.first_arg_or_undefined(args)
      case helpers.is_callable(state.heap, callback) {
        False -> state.type_error(state, "cleanup must be callable")
        True -> {
          // Step 3: GetPrototypeFromConstructor(NewTarget, intrinsic) — must
          // use a real [[Get]] so accessor `prototype` properties are invoked.
          use proto_ref, state <- object.proto_from_new_target(
            state,
            new_target,
            proto,
          )
          let #(heap, ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: FinalizationRegistryObject(cells: [], callback:),
                properties: dict.new(),
                elements: elements.new(),
                prototype: Some(proto_ref),
                symbol_properties: [],
                extensible: True,
              ),
            )
          #(State(..state, heap:), Ok(JsObject(ref)))
        }
      }
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
fn register(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use cells, callback, ref, state <- require_registry(this, state, "register")
  let target = helpers.first_arg_or_undefined(args)
  let held = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
  let token_arg = helpers.list_at(args, 2) |> option.unwrap(JsUndefined)
  // Step 3
  case can_be_held_weakly(state, target) {
    False -> state.type_error(state, "Invalid value used as weak ref target")
    True ->
      // Step 4
      case value.same_value(target, held) {
        True -> state.type_error(state, "target and holdings must not be same")
        False ->
          // Step 5
          case can_be_held_weakly(state, token_arg), token_arg {
            False, JsUndefined ->
              do_register(state, cells, callback, ref, target, held, None)
            False, _ ->
              state.type_error(state, "Invalid value used as unregister token")
            True, _ ->
              do_register(
                state,
                cells,
                callback,
                ref,
                target,
                held,
                Some(token_arg),
              )
          }
      }
  }
}

/// Steps 6-8 of register — append the cell and return undefined.
fn do_register(
  state: State(host),
  cells: List(value.FinRegCell),
  callback: JsValue,
  ref: Ref,
  target: JsValue,
  held: JsValue,
  token: option.Option(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  let cell = FinRegCell(target:, held:, token:)
  // [[Cells]] is append-ordered in the spec; order is unobservable here
  // (no iteration, cleanup never fires), so prepend for O(1).
  let kind = FinalizationRegistryObject(cells: [cell, ..cells], callback:)
  let heap = heap.update_kind(state.heap, ref, kind)
  #(State(..state, heap:), Ok(JsUndefined))
}

/// §26.2.3.3 FinalizationRegistry.prototype.unregister ( unregisterToken )
///
///   1-2. RequireInternalSlot(finalizationRegistry, [[Cells]]).
///   3. If CanBeHeldWeakly(unregisterToken) is false, throw a TypeError.
///   4-5. Remove every cell whose [[UnregisterToken]] is not empty and
///        SameValue(cell.[[UnregisterToken]], unregisterToken) is true.
///   6. Return whether any cell was removed.
fn unregister(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use cells, callback, ref, state <- require_registry(this, state, "unregister")
  let token = helpers.first_arg_or_undefined(args)
  case can_be_held_weakly(state, token) {
    False -> state.type_error(state, "Invalid value used as unregister token")
    True -> {
      let #(removed, kept) =
        list.partition(cells, fn(cell) {
          case cell.token {
            Some(t) -> value.same_value(t, token)
            None -> False
          }
        })
      let kind = FinalizationRegistryObject(cells: kept, callback:)
      let heap = heap.update_kind(state.heap, ref, kind)
      #(State(..state, heap:), Ok(JsBool(removed != [])))
    }
  }
}

/// RequireInternalSlot(this, [[Cells]]) — this must be an object with the
/// FinalizationRegistry brand, else TypeError. CPS-style.
fn require_registry(
  this: JsValue,
  state: State(host),
  method: String,
  cont: fn(List(value.FinRegCell), JsValue, Ref, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let found = case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: FinalizationRegistryObject(cells:, callback:), ..)) ->
          Some(#(cells, callback, ref))
        _ -> None
      }
    _ -> None
  }
  case found {
    Some(#(cells, callback, ref)) -> cont(cells, callback, ref, state)
    None ->
      state.type_error(
        state,
        "FinalizationRegistry.prototype."
          <> method
          <> " called on incompatible receiver",
      )
  }
}
