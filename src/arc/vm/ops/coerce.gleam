import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, BigInt, Finite, FunctionObject, Infinity, JsBigInt, JsBool,
  JsNull, JsNumber, JsObject, JsString, JsSymbol, JsUndefined, JsUninitialized,
  NaN, NativeFunction, NegInfinity, ObjectSlot,
}
import gleam/int
import gleam/option.{Some}
import gleam/result

// ============================================================================
// ToPrimitive / ToString with VM re-entry (ES2024 §7.1.1, §7.1.12)
// ============================================================================

pub type ToPrimitiveHint {
  StringHint
  NumberHint
  DefaultHint
}

/// ES2024 §7.1.1 ToPrimitive(input, preferredType)
/// For primitives, returns as-is. For objects, calls Symbol.toPrimitive
/// or falls back to OrdinaryToPrimitive.
pub fn to_primitive(
  state: State,
  val: JsValue,
  hint: ToPrimitiveHint,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case val {
    // Primitives pass through
    JsUndefined
    | JsNull
    | JsBool(_)
    | JsNumber(_)
    | JsString(_)
    | JsSymbol(_)
    | JsBigInt(_)
    | JsUninitialized -> Ok(#(val, state))
    // Objects: try Symbol.toPrimitive, then OrdinaryToPrimitive
    JsObject(ref) -> {
      // §7.1.1 step 1.a: check @@toPrimitive
      use #(exotic_fn, state) <- result.try(object.get_symbol_value(
        state,
        ref,
        value.symbol_to_primitive,
        val,
      ))
      case exotic_fn {
        // @@toPrimitive not found → fall through to OrdinaryToPrimitive
        JsUndefined -> ordinary_to_primitive(state, val, ref, hint)
        _ ->
          case helpers.is_callable(state.heap, exotic_fn) {
            True -> {
              let hint_str = case hint {
                StringHint -> "string"
                NumberHint -> "number"
                DefaultHint -> "default"
              }
              use #(result, new_state) <- result.try(
                state.call(state, exotic_fn, val, [JsString(hint_str)]),
              )
              case result {
                JsObject(_) ->
                  thrown_type_error(
                    new_state,
                    "Cannot convert object to primitive value",
                  )
                _ -> Ok(#(result, new_state))
              }
            }
            False -> thrown_type_error(state, "@@toPrimitive is not callable")
          }
      }
    }
  }
}

/// ES2024 §7.1.1.1 OrdinaryToPrimitive(O, hint)
/// Tries toString/valueOf (or valueOf/toString for number hint).
pub fn ordinary_to_primitive(
  state: State,
  val: JsValue,
  ref: value.Ref,
  hint: ToPrimitiveHint,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  let method_names = case hint {
    StringHint -> ["toString", "valueOf"]
    NumberHint | DefaultHint -> ["valueOf", "toString"]
  }
  try_to_primitive_methods(state, val, ref, method_names)
}

/// Try each method name in order; return the first primitive result.
fn try_to_primitive_methods(
  state: State,
  val: JsValue,
  ref: value.Ref,
  method_names: List(String),
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case method_names {
    [] -> thrown_type_error(state, "Cannot convert object to primitive value")
    [name, ..rest] -> {
      use #(method, state) <- result.try(object.get_value(
        state,
        ref,
        value.Named(name),
        val,
      ))
      case helpers.is_callable(state.heap, method) {
        True -> {
          use #(result, new_state) <- result.try(
            state.call(state, method, val, []),
          )
          case result {
            JsObject(_) -> try_to_primitive_methods(new_state, val, ref, rest)
            _ -> Ok(#(result, new_state))
          }
        }
        False -> try_to_primitive_methods(state, val, ref, rest)
      }
    }
  }
}

/// ES2024 §7.1.12 ToString with VM re-entry for ToPrimitive.
/// For primitives, converts directly. For objects, calls ToPrimitive(string) first.
pub fn js_to_string(
  state: State,
  val: JsValue,
) -> Result(#(String, State), #(JsValue, State)) {
  case val {
    JsObject(_) -> {
      use #(prim, new_state) <- result.try(to_primitive(state, val, StringHint))
      js_to_string(new_state, prim)
    }
    JsSymbol(_) ->
      thrown_type_error(state, "Cannot convert a Symbol value to a string")
    JsString(s) -> Ok(#(s, state))
    JsNumber(Finite(n)) -> Ok(#(value.js_format_number(n), state))
    JsNumber(NaN) -> Ok(#("NaN", state))
    JsNumber(Infinity) -> Ok(#("Infinity", state))
    JsNumber(NegInfinity) -> Ok(#("-Infinity", state))
    JsBool(True) -> Ok(#("true", state))
    JsBool(False) -> Ok(#("false", state))
    JsNull -> Ok(#("null", state))
    JsUndefined -> Ok(#("undefined", state))
    JsUninitialized -> Ok(#("undefined", state))
    JsBigInt(BigInt(n)) -> Ok(#(int.to_string(n), state))
  }
}

/// CPS wrapper for js_to_string. Use with `use` syntax:
///   use str, state <- coerce.try_to_string(state, val)
pub fn try_to_string(
  state: State,
  val: JsValue,
  cont: fn(String, State) -> #(State, Result(b, JsValue)),
) -> #(State, Result(b, JsValue)) {
  case js_to_string(state, val) {
    Ok(#(str, state)) -> cont(str, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// ES2024 §7.1.4 ToNumber with VM re-entry for ToPrimitive.
/// For primitives, converts directly. For objects, calls ToPrimitive(number) first.
pub fn js_to_number(
  state: State,
  val: JsValue,
) -> Result(#(value.JsNum, State), #(JsValue, State)) {
  case val {
    JsObject(_) -> {
      use #(prim, state) <- result.try(to_primitive(state, val, NumberHint))
      js_to_number(state, prim)
    }
    other ->
      case value.to_number(other) {
        Ok(n) -> Ok(#(n, state))
        Error(msg) -> thrown_type_error(state, msg)
      }
  }
}

/// CPS wrapper for js_to_number. Use with `use` syntax:
///   use n, state <- coerce.try_to_number(state, val)
pub fn try_to_number(
  state: State,
  val: JsValue,
  cont: fn(value.JsNum, State) -> #(State, Result(b, JsValue)),
) -> #(State, Result(b, JsValue)) {
  case js_to_number(state, val) {
    Ok(#(n, state)) -> cont(n, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Fast-path unwrap of primitive wrapper objects (Number/String/Boolean) to
/// their [[PrimitiveValue]]. Returns `val` unchanged for any other input.
/// Unlike full ToPrimitive this does NOT honor user-overridden valueOf or
/// @@toPrimitive — use js_to_number/js_to_string for spec-exact coercion.
pub fn unwrap_primitive_wrapper(h: Heap, val: JsValue) -> JsValue {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: value.NumberObject(value: n), ..)) -> JsNumber(n)
        Some(ObjectSlot(kind: value.StringObject(value: s), ..)) -> JsString(s)
        Some(ObjectSlot(kind: value.BooleanObject(value: b), ..)) -> JsBool(b)
        _ -> val
      }
    other -> other
  }
}

/// ES2024 §13.10.2 InstanceofOperator ( V, target )
pub fn js_instanceof(
  state: State,
  left: JsValue,
  constructor: JsValue,
) -> Result(#(Bool, State), #(JsValue, State)) {
  case constructor {
    // Step 1: target must be an Object.
    JsObject(ctor_ref) ->
      case heap.read(state.heap, ctor_ref) {
        // Step 4: IsCallable(target) — we check for function slot kinds.
        Some(ObjectSlot(kind: FunctionObject(..), ..))
        | Some(ObjectSlot(kind: NativeFunction(_), ..)) -> {
          // Step 5: OrdinaryHasInstance(target, V) — inlined below.
          // OrdinaryHasInstance step 4: Let P be ? Get(C, "prototype").
          use #(proto_val, state) <- result.try(object.get_value(
            state,
            ctor_ref,
            value.Named("prototype"),
            constructor,
          ))
          case proto_val {
            JsObject(proto_ref) ->
              // OrdinaryHasInstance step 3: If O is not an Object, return false.
              case left {
                JsObject(obj_ref) ->
                  // OrdinaryHasInstance step 6: prototype chain walk.
                  Ok(#(instanceof_walk(state.heap, obj_ref, proto_ref), state))
                _ -> Ok(#(False, state))
              }
            _ ->
              // OrdinaryHasInstance step 5: If P is not an Object, throw TypeError.
              thrown_type_error(
                state,
                "Function has non-object prototype in instanceof check",
              )
          }
        }
        // Step 4: Not callable → TypeError.
        _ ->
          thrown_type_error(
            state,
            "Right-hand side of instanceof is not callable",
          )
      }
    // Step 1: Not an Object → TypeError.
    _ ->
      thrown_type_error(state, "Right-hand side of instanceof is not callable")
  }
}

/// ES2024 §7.3.22 OrdinaryHasInstance ( C, O ) — step 6 (prototype chain walk)
fn instanceof_walk(
  heap: Heap,
  obj_ref: value.Ref,
  target_proto: value.Ref,
) -> Bool {
  // Step 6a: Get [[Prototype]] of current object.
  case heap.read(heap, obj_ref) {
    Some(ObjectSlot(prototype: Some(proto_ref), ..)) ->
      // Step 6c: SameValue(P, O) — compare by ref identity.
      case proto_ref.id == target_proto.id {
        True -> True
        // Step 6: Repeat — walk up the chain.
        False -> instanceof_walk(heap, proto_ref, target_proto)
      }
    // Step 6b: O is null (no prototype) → return false.
    _ -> False
  }
}

/// Helper to throw a TypeError in functions that return Result(a, #(JsValue, State)).
/// Used by toPrimitive, toString, instanceof, etc.
pub fn thrown_type_error(
  state: State,
  msg: String,
) -> Result(a, #(JsValue, State)) {
  let #(h, err) = common.make_type_error(state.heap, state.builtins, msg)
  Error(#(err, State(..state, heap: h)))
}
