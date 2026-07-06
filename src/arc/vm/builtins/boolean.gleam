import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/heap
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type BooleanNativeFn, type JsValue, type Ref, BooleanConstructor,
  BooleanNative, BooleanObject, BooleanPrototypeToString,
  BooleanPrototypeValueOf, Dispatch, JsBool, JsObject, JsString,
}
import gleam/option.{None, Some}

/// Set up Boolean constructor + Boolean.prototype.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("valueOf", BooleanNative(BooleanPrototypeValueOf), 0),
      #("toString", BooleanNative(BooleanPrototypeToString), 0),
    ])
  // ES2024 §20.3.3: the Boolean prototype object is itself a Boolean object,
  // with a [[BooleanData]] internal slot whose value is false — hence
  // init_wrapper_type rather than init_type.
  let #(h, bt) =
    common.init_wrapper_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(_) { Dispatch(BooleanNative(BooleanConstructor)) },
      "Boolean",
      1,
      [],
      proto_kind: BooleanObject(value: False),
    )

  #(h, bt)
}

/// Per-module dispatch for Boolean native functions.
pub fn dispatch(
  native: BooleanNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    BooleanConstructor -> call_as_function(args, state)
    BooleanPrototypeValueOf -> boolean_value_of(this, args, state)
    BooleanPrototypeToString -> boolean_to_string(this, args, state)
  }
}

/// ES2024 §20.3.1.1 Boolean(value)
/// Called as a function (without new). When called as a constructor,
/// exec/call.gleam's do_construct intercepts and wraps the result in a
/// BooleanObject before this path is reached.
///
/// 1. Let b be ToBoolean(value).
/// 2. If NewTarget is undefined, return b.
/// 3. (constructor path — handled in do_construct)
///
/// Note: ToBoolean is implemented by value.is_truthy. When no argument
/// is provided, value defaults to undefined which is falsy (step 1).
fn call_as_function(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
fn boolean_value_of(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: Return ? thisBooleanValue(this value).
  use b, state <- state.try_then(this_boolean_value(state, this, "valueOf"))
  #(state, Ok(JsBool(b)))
}

/// ES2024 §20.3.3.2 Boolean.prototype.toString()
///
/// 1. Let b be ? thisBooleanValue(this value).
/// 2. If b is true, return "true"; otherwise return "false".
///
/// Correctly implements spec. The two-branch pattern (Some(True)/Some(False))
/// maps directly to step 2.
fn boolean_to_string(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: Let b be ? thisBooleanValue(this value).
  use b, state <- state.try_then(this_boolean_value(state, this, "toString"))
  // Step 2: If b is true, return "true"; otherwise return "false".
  case b {
    True -> #(state, Ok(JsString("true")))
    False -> #(state, Ok(JsString("false")))
  }
}

/// ES2024 §20.3.3.1 thisBooleanValue(value)
///
/// 1. If value is a Boolean, return value.
/// 2. If value is an Object and value has a [[BooleanData]] internal slot, then
///    a. Let b be value.[[BooleanData]].
///    b. Assert: b is a Boolean.
///    c. Return b.
/// 3. Throw a TypeError exception.
///
/// Same shape as symbol/number/string/bigint's this-X-value: throws the
/// branded TypeError internally so callers just `state.try_then` it.
fn this_boolean_value(
  state: State(host),
  this: JsValue,
  method: String,
) -> #(State(host), Result(Bool, JsValue)) {
  case this {
    // Step 1: If value is a Boolean, return value.
    JsBool(b) -> #(state, Ok(b))
    // Step 2: If value is an Object with [[BooleanData]], return it.
    JsObject(ref) ->
      case heap.read_boolean_object(state.heap, ref) {
        Some(b) -> #(state, Ok(b))
        None -> not_a_boolean(state, method)
      }
    // Step 3: Throw a TypeError exception.
    _ -> not_a_boolean(state, method)
  }
}

fn not_a_boolean(
  state: State(host),
  method: String,
) -> #(State(host), Result(a, JsValue)) {
  state.type_error(
    state,
    "Boolean.prototype." <> method <> " requires that 'this' be a Boolean",
  )
}
