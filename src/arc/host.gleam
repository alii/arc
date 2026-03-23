//// Helpers for writing host functions.
////
//// Two flavours, both designed for `use` syntax:
////
////   Validators — strict type checks that throw TypeError on mismatch.
////   Modeled after Node's `internal/validators`. Error format:
////     The "NAME" argument must be of type EXPECTED. Received type ACTUAL
////
////   Coercers — run the spec ToX abstract operations. These re-enter the VM
////   (ToPrimitive, valueOf/toString calls) and propagate anything thrown.
////
//// Usage:
////
////     fn host_repeat(args, _this, s) {
////       case args {
////         [str, n, ..] -> {
////           use str, s <- host.validate_string(s, str, "str")
////           use n, s <- host.validate_integer(s, n, "count", 0, 1_000_000)
////           #(s, Ok(JsString(string.repeat(str, n))))
////         }
////         _ -> state.type_error(s, "repeat: expected (str, count)")
////       }
////     }

import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/state.{type State}
import arc/vm/value.{
  type JsValue, type Ref, Finite, JsBool, JsNumber, JsObject, JsString,
}
import gleam/int
import gleam/list
import gleam/option

// -- Validators --------------------------------------------------------------

/// Reject unless `val` is a JS string. Unwraps to the Gleam `String`.
pub fn validate_string(
  s: State,
  val: JsValue,
  name: String,
  cont: fn(String, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case val {
    JsString(str) -> cont(str, s)
    _ -> invalid_arg_type(s, name, "string", val)
  }
}

/// Reject unless `val` is callable. Passes the value through unchanged —
/// hand it to `state.try_call` to invoke. Use this when you call the function
/// more than once (validate once, call many). For one-shot calls, `try_call`
/// does both in one step.
pub fn validate_function(
  s: State,
  val: JsValue,
  name: String,
  cont: fn(JsValue, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case helpers.is_callable(s.heap, val) {
    True -> cont(val, s)
    False -> invalid_arg_type(s, name, "function", val)
  }
}

/// Validate callability AND call — one-shot combination of `validate_function`
/// and `state.try_call`. If `callee` isn't callable, throws TypeError with
/// the arg name; otherwise calls it and propagates the result or any throw.
pub fn try_call(
  s: State,
  callee: JsValue,
  name: String,
  this_val: JsValue,
  args: List(JsValue),
  cont: fn(JsValue, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case helpers.is_callable(s.heap, callee) {
    True -> state.try_call(s, callee, this_val, args, cont)
    False -> invalid_arg_type(s, name, "function", callee)
  }
}

/// Reject unless `val` is a finite JS number. Unwraps to `Float`.
/// NaN and ±Infinity are rejected — use `to_number` if you want coercion.
pub fn validate_number(
  s: State,
  val: JsValue,
  name: String,
  cont: fn(Float, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case val {
    JsNumber(Finite(n)) -> cont(n, s)
    _ -> invalid_arg_type(s, name, "number", val)
  }
}

/// Reject unless `val` is an integer-valued JS number within `[min, max]`.
/// Unwraps to `Int`. Throws RangeError if out of bounds, TypeError if not
/// a number.
pub fn validate_integer(
  s: State,
  val: JsValue,
  name: String,
  min: Int,
  max: Int,
  cont: fn(Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case val {
    JsNumber(Finite(n)) -> {
      let i = value.float_to_int(n)
      case int.to_float(i) == n {
        False -> invalid_arg_type(s, name, "integer", val)
        True ->
          case i >= min && i <= max {
            True -> cont(i, s)
            False ->
              state.range_error(
                s,
                "The value of \""
                  <> name
                  <> "\" is out of range. It must be >= "
                  <> int.to_string(min)
                  <> " and <= "
                  <> int.to_string(max)
                  <> ". Received "
                  <> int.to_string(i),
              )
          }
      }
    }
    _ -> invalid_arg_type(s, name, "integer", val)
  }
}

/// Reject unless `val` is a JS boolean. Unwraps to `Bool`.
pub fn validate_boolean(
  s: State,
  val: JsValue,
  name: String,
  cont: fn(Bool, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case val {
    JsBool(b) -> cont(b, s)
    _ -> invalid_arg_type(s, name, "boolean", val)
  }
}

/// Reject unless `val` is a JS object (not null/undefined/primitive).
/// Unwraps to the heap `Ref`.
pub fn validate_object(
  s: State,
  val: JsValue,
  name: String,
  cont: fn(Ref, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case val {
    JsObject(ref) -> cont(ref, s)
    _ -> invalid_arg_type(s, name, "object", val)
  }
}

// -- Coercers ----------------------------------------------------------------

/// ES ToString — coerces any value to a string via ToPrimitive. Re-enters
/// the VM for `valueOf`/`toString` calls; propagates anything thrown.
pub const to_string = state.try_to_string

/// ES ToNumber followed by ToIntegerOrInfinity, clamped to a Gleam `Int`.
/// NaN/undefined become 0; ±Infinity becomes 0. Does NOT re-enter the VM
/// (objects are not coerced — they yield 0).
pub fn to_int(
  s: State,
  val: JsValue,
  cont: fn(Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  cont(helpers.to_number_int(val) |> option.unwrap(0), s)
}

// -- Constructors ------------------------------------------------------------

/// Allocate a JS array from Gleam values. Uses the correct Array.prototype.
pub fn array(s: State, values: List(JsValue)) -> #(State, JsValue) {
  let #(heap, ref) =
    common.alloc_array(s.heap, values, s.builtins.array.prototype)
  #(state.State(..s, heap:), JsObject(ref))
}

/// Allocate a plain JS object from a property list. Uses Object.prototype.
pub fn object(s: State, props: List(#(String, JsValue))) -> #(State, JsValue) {
  let prop_list = list.map(props, fn(p) { #(p.0, value.data_property(p.1)) })
  let #(heap, ref) =
    common.alloc_pojo(s.heap, s.builtins.object.prototype, prop_list)
  #(state.State(..s, heap:), JsObject(ref))
}

// -- Internal ----------------------------------------------------------------

fn invalid_arg_type(
  s: State,
  name: String,
  expected: String,
  received: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  state.type_error(
    s,
    "The \""
      <> name
      <> "\" argument must be of type "
      <> expected
      <> ". Received type "
      <> common.typeof_value(received, s.heap),
  )
}
