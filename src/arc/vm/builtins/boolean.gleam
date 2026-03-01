import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/frame.{type State}
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type BooleanNativeFn, type JsValue, type Ref, BooleanConstructor,
  BooleanNative, BooleanObject, BooleanPrototypeToString,
  BooleanPrototypeValueOf, JsBool, JsObject, JsString, ObjectSlot,
}
import gleam/option.{type Option, None, Some}

/// Set up Boolean constructor + Boolean.prototype.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("valueOf", BooleanNative(BooleanPrototypeValueOf), 0),
      #("toString", BooleanNative(BooleanPrototypeToString), 0),
    ])
  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(_) { BooleanNative(BooleanConstructor) },
      "Boolean",
      1,
      [],
    )

  // ES2024 §20.3.3: The Boolean prototype object has a [[BooleanData]] internal
  // slot with value false. Update from OrdinaryObject to BooleanObject.
  let h =
    heap.update(h, bt.prototype, fn(slot) {
      case slot {
        ObjectSlot(
          properties:,
          elements:,
          prototype:,
          symbol_properties:,
          extensible:,
          ..,
        ) ->
          ObjectSlot(
            kind: BooleanObject(value: False),
            properties:,
            elements:,
            prototype:,
            symbol_properties:,
            extensible:,
          )
        other -> other
      }
    })

  #(h, bt)
}

/// Per-module dispatch for Boolean native functions.
pub fn dispatch(
  native: BooleanNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    BooleanConstructor -> call_as_function(args, state)
    BooleanPrototypeValueOf -> boolean_value_of(this, args, state)
    BooleanPrototypeToString -> boolean_to_string(this, args, state)
  }
}

/// ES2024 §20.3.1.1 Boolean(value)
/// Called as a function (without new). When called as a constructor,
/// the VM handles wrapping the result in a BooleanObject via CreateNew.
///
/// 1. Let b be ToBoolean(value).
/// 2. If NewTarget is undefined, return b.
/// 3. (constructor path — handled by VM CreateNew opcode)
///
/// Note: ToBoolean is implemented by value.is_truthy. When no argument
/// is provided, value defaults to undefined which is falsy (step 1).
pub fn call_as_function(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: Let b be ToBoolean(value).
  let result = case args {
    [] -> JsBool(False)
    [val, ..] -> JsBool(value.is_truthy(val))
  }
  // Step 2: NewTarget is undefined (called as function), return b.
  #(state, Ok(result))
}

/// ES2024 §20.3.3.3 Boolean.prototype.valueOf()
///
/// 1. Return ? thisBooleanValue(this value).
///
/// Correctly implements spec: delegates entirely to thisBooleanValue,
/// which handles both primitive booleans and Boolean wrapper objects.
/// Throws TypeError if this is neither.
pub fn boolean_value_of(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: Return ? thisBooleanValue(this value).
  case this_boolean_value(state, this) {
    Some(b) -> #(state, Ok(JsBool(b)))
    None ->
      frame.type_error(
        state,
        "Boolean.prototype.valueOf requires that 'this' be a Boolean",
      )
  }
}

/// ES2024 §20.3.3.2 Boolean.prototype.toString()
///
/// 1. Let b be ? thisBooleanValue(this value).
/// 2. If b is true, return "true"; otherwise return "false".
///
/// Correctly implements spec. The two-branch pattern (Some(True)/Some(False))
/// maps directly to step 2.
pub fn boolean_to_string(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: Let b be ? thisBooleanValue(this value).
  case this_boolean_value(state, this) {
    // Step 2: If b is true, return "true"; otherwise return "false".
    Some(True) -> #(state, Ok(JsString("true")))
    Some(False) -> #(state, Ok(JsString("false")))
    // Step 1 threw: thisBooleanValue returned abrupt completion (TypeError).
    None ->
      frame.type_error(
        state,
        "Boolean.prototype.toString requires that 'this' be a Boolean",
      )
  }
}

/// ES2024 §20.3.3.1 thisBooleanValue(value)
///
/// The abstract operation thisBooleanValue takes argument value and returns
/// either a normal completion containing a Boolean or a throw completion.
/// It performs the following steps when called:
///
/// 1. If value is a Boolean, return value.
/// 2. If value is an Object and value has a [[BooleanData]] internal slot, then
///    a. Let b be value.[[BooleanData]].
///    b. Assert: b is a Boolean.
///    c. Return b.
/// 3. Throw a TypeError exception.
///
/// Correctly implements spec. Returns Option(Bool) instead of Result to
/// let callers handle the TypeError with their own message.
fn this_boolean_value(state: State, this: JsValue) -> Option(Bool) {
  case this {
    // Step 1: If value is a Boolean, return value.
    JsBool(b) -> Some(b)
    // Step 2: If value is an Object with [[BooleanData]], return it.
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: BooleanObject(value: b), ..)) -> Some(b)
        _ -> None
      }
    // Step 3: Throw a TypeError exception.
    _ -> None
  }
}
