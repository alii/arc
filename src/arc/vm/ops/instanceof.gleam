import arc/vm/heap
import arc/vm/key.{Named}
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type JsValue, JsNull, JsObject, JsUndefined, JsUninitialized, NativeFunction,
  ObjectSlot,
}
import gleam/option.{Some}
import gleam/result

/// ES2024 §13.10.2 InstanceofOperator ( V, target )
///
///   1. If target is not an Object, throw a TypeError exception.
///   2. Let instOfHandler be ? GetMethod(target, @@hasInstance).
///   3. If instOfHandler is not undefined, then
///      a. Return ToBoolean(? Call(instOfHandler, target, « V »)).
///   4. If IsCallable(target) is false, throw a TypeError exception.
///   5. Return ? OrdinaryHasInstance(target, V).
///
/// Step 2 is a real [[Get]]: it walks the prototype chain, fires proxy traps,
/// and finds a user's `static [Symbol.hasInstance]`. Every function inherits
/// %Function.prototype[@@hasInstance]% (§20.2.3.6), whose whole body is
/// OrdinaryHasInstance — recognising it by slot kind lets the common case
/// skip the call without changing anything observable.
pub fn js_instanceof(
  state: State(host),
  left: JsValue,
  constructor: JsValue,
) -> Result(#(Bool, State(host)), #(JsValue, State(host))) {
  case constructor {
    // Step 1: target must be an Object.
    JsObject(ctor_ref) -> {
      // Step 2: instOfHandler = ? GetMethod(target, @@hasInstance).
      use #(handler, state) <- result.try(object.get_symbol_value(
        state,
        ctor_ref,
        value.symbol_has_instance,
        constructor,
      ))
      let ctor_callable = object.value_is_callable(state.heap, constructor)
      case classify_has_instance(state.heap, handler) {
        // Step 3 with the inherited %Function.prototype[@@hasInstance]%: its
        // whole body is `? OrdinaryHasInstance(this, V)`, so run that directly
        // rather than allocating a call frame for it. A non-callable target
        // reaches OrdinaryHasInstance step 1 → false (NOT a TypeError: that is
        // what makes `x instanceof Object.create(Function.prototype)` answer
        // false instead of throwing).
        IntrinsicHandler ->
          case ctor_callable {
            True -> ordinary_has_instance(state, ctor_ref, left)
            False -> Ok(#(False, state))
          }
        // Step 2's GetMethod (§7.3.11 step 2): undefined/null → absent.
        NoHandler ->
          case ctor_callable {
            // Step 5: OrdinaryHasInstance(target, V).
            True -> ordinary_has_instance(state, ctor_ref, left)
            // Step 4: Not callable → TypeError.
            False ->
              state.type_error_op(
                state,
                "Right-hand side of instanceof is not callable",
              )
          }
        // Step 3.a: user-supplied handler — Call(instOfHandler, target, «V»),
        // then ToBoolean the result.
        UserHandler -> {
          use #(res, state) <- result.map(
            state.call(state, handler, constructor, [left]),
          )
          #(value.is_truthy(res), state)
        }
        // GetMethod step 3: present but not callable → TypeError.
        NotCallable ->
          state.type_error_op(
            state,
            "Symbol.hasInstance handler is not callable",
          )
      }
    }
    // Step 1: Not an Object → TypeError.
    _ ->
      state.type_error_op(
        state,
        "Right-hand side of instanceof is not callable",
      )
  }
}

/// What §13.10.2 step 2's `GetMethod(target, @@hasInstance)` came back with.
type HasInstanceHandler {
  /// Steps 2/3 saw undefined or null — fall through to steps 4-5.
  NoHandler
  /// The inherited %Function.prototype[@@hasInstance]% (§20.2.3.6), whose
  /// observable behaviour is `? OrdinaryHasInstance(this, V)` and nothing
  /// else — so `js_instanceof` may inline it instead of calling it.
  IntrinsicHandler
  /// A user-supplied callable — must actually be Called (step 3.a).
  UserHandler
  /// Present but not callable — GetMethod step 3 throws.
  NotCallable
}

fn classify_has_instance(h: Heap(host), val: JsValue) -> HasInstanceHandler {
  case val {
    JsUndefined | JsNull | JsUninitialized -> NoHandler
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(
          kind: NativeFunction(
            native: value.Dispatch(value.VmNative(value.FunctionHasInstance)),
            ..,
          ),
          ..,
        )) -> IntrinsicHandler
        _ ->
          case object.value_is_callable(h, val) {
            True -> UserHandler
            False -> NotCallable
          }
      }
    _ -> NotCallable
  }
}

/// ES2024 §7.3.22 OrdinaryHasInstance ( C, O ), steps 2-7. The caller has
/// already verified that `ctor_ref` is callable (step 1).
pub fn ordinary_has_instance(
  state: State(host),
  ctor_ref: value.Ref,
  left: JsValue,
) -> Result(#(Bool, State(host)), #(JsValue, State(host))) {
  case heap.read(state.heap, ctor_ref) {
    // Step 2: C has a [[BoundTargetFunction]] internal slot →
    // return InstanceofOperator(O, BC).
    Some(ObjectSlot(
      kind: NativeFunction(value.Call(value.BoundFunction(target:, ..)), ..),
      ..,
    )) -> js_instanceof(state, left, JsObject(target))
    _ ->
      // Step 3: If O is not an Object, return false (before the Get —
      // a throwing "prototype" getter must NOT fire for primitives).
      case left {
        JsObject(obj_ref) -> {
          // Step 4: Let P be ? Get(C, "prototype").
          use #(proto_val, state) <- result.try(object.get_value(
            state,
            ctor_ref,
            Named("prototype"),
            JsObject(ctor_ref),
          ))
          case proto_val {
            JsObject(proto_ref) ->
              // Step 7: prototype chain walk — stateful so a proxy on
              // the chain fires its getPrototypeOf trap (§10.5.1).
              instanceof_walk(state, obj_ref, proto_ref)
            _ ->
              // Step 5: If P is not an Object, throw TypeError.
              state.type_error_op(
                state,
                "Function has non-object prototype in instanceof check",
              )
          }
        }
        _ -> Ok(#(False, state))
      }
  }
}

/// ES2024 §7.3.22 OrdinaryHasInstance ( C, O ) — step 7 (prototype chain
/// walk). Stateful: each level goes through [[GetPrototypeOf]], which traps
/// (and may throw) for proxies on the chain.
fn instanceof_walk(
  state: State(host),
  obj_ref: value.Ref,
  target_proto: value.Ref,
) -> Result(#(Bool, State(host)), #(JsValue, State(host))) {
  // Step 6a: Let O be ? O.[[GetPrototypeOf]]().
  use #(proto_val, state) <- result.try(object.get_prototype_of_stateful(
    state,
    obj_ref,
  ))
  case proto_val {
    JsObject(proto_ref) ->
      // Step 6c: SameValue(P, O) — compare by ref identity.
      case proto_ref.id == target_proto.id {
        True -> Ok(#(True, state))
        // Step 6: Repeat — walk up the chain.
        False -> instanceof_walk(state, proto_ref, target_proto)
      }
    // Step 6b: O is null (no prototype) → return false.
    _ -> Ok(#(False, state))
  }
}
