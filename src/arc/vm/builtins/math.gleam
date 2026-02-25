import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/frame.{type State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type Ref, Finite, Infinity, JsNumber, JsString, NaN,
  NativeMathAbs, NativeMathCeil, NativeMathCos, NativeMathFloor, NativeMathLog,
  NativeMathMax, NativeMathMin, NativeMathPow, NativeMathRound, NativeMathSin,
  NativeMathSqrt, NativeMathTrunc, NegInfinity, ObjectSlot, OrdinaryObject,
}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{Some}

/// Set up the Math global object.
/// Math is NOT a constructor — it's a plain object with static methods.
pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
  let constants = [
    #("PI", value.data(JsNumber(Finite(3.141592653589793)))),
    #("E", value.data(JsNumber(Finite(2.718281828459045)))),
  ]

  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("pow", NativeMathPow, 2),
      #("abs", NativeMathAbs, 1),
      #("floor", NativeMathFloor, 1),
      #("ceil", NativeMathCeil, 1),
      #("round", NativeMathRound, 1),
      #("trunc", NativeMathTrunc, 1),
      #("sqrt", NativeMathSqrt, 1),
      #("max", NativeMathMax, 2),
      #("min", NativeMathMin, 2),
      #("log", NativeMathLog, 1),
      #("sin", NativeMathSin, 1),
      #("cos", NativeMathCos, 1),
    ])

  let properties = dict.from_list(list.append(methods, constants))
  let symbol_properties =
    dict.from_list([
      #(
        value.symbol_to_string_tag,
        value.data(JsString("Math")) |> value.configurable(),
      ),
    ])

  let #(h, math_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties:,
        elements: js_elements.new(),
        prototype: Some(object_proto),
        symbol_properties:,
        extensible: True,
      ),
    )
  let h = heap.root(h, math_ref)

  #(h, math_ref)
}

// ============================================================================
// Math method implementations
// ============================================================================

/// Math.pow(base, exponent)
pub fn math_pow(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let base = helpers.get_num_arg(args, 0, to_number)
  let exponent = helpers.get_num_arg(args, 1, to_number)
  #(state, Ok(JsNumber(num_exp(base, exponent))))
}

/// Math.abs(x)
pub fn math_abs(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(float.absolute_value(n))
    NaN -> NaN
    Infinity | NegInfinity -> Infinity
  }
}

/// Math.floor(x)
pub fn math_floor(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  finite_passthrough(x, ffi_math_floor)
}

/// Math.ceil(x)
pub fn math_ceil(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  finite_passthrough(x, ffi_math_ceil)
}

/// Math.round(x) — JS round: round half toward +Infinity (NOT banker's rounding)
pub fn math_round(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  finite_passthrough(x, js_round)
}

/// Math.trunc(x)
pub fn math_trunc(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  finite_passthrough(x, fn(n) { int.to_float(value.float_to_int(n)) })
}

/// Math.sqrt(x)
pub fn math_sqrt(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case n <. 0.0 {
        True -> NaN
        False -> Finite(ffi_math_sqrt(n))
      }
    NaN | NegInfinity -> NaN
    Infinity -> Infinity
  }
}

/// Math.max(a, b, ...)
pub fn math_max(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [] -> #(state, Ok(JsNumber(NegInfinity)))
    _ -> {
      let result =
        list.fold(args, NegInfinity, fn(acc, arg) {
          let n = to_number(arg)
          case acc, n {
            NaN, _ | _, NaN -> NaN
            Finite(a), Finite(b) ->
              case a >=. b {
                True -> Finite(a)
                False -> Finite(b)
              }
            Infinity, _ | _, Infinity -> Infinity
            NegInfinity, other | other, NegInfinity -> other
          }
        })
      #(state, Ok(JsNumber(result)))
    }
  }
}

/// Math.min(a, b, ...)
pub fn math_min(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [] -> #(state, Ok(JsNumber(Infinity)))
    _ -> {
      let result =
        list.fold(args, Infinity, fn(acc, arg) {
          let n = to_number(arg)
          case acc, n {
            NaN, _ | _, NaN -> NaN
            Finite(a), Finite(b) ->
              case a <=. b {
                True -> Finite(a)
                False -> Finite(b)
              }
            NegInfinity, _ | _, NegInfinity -> NegInfinity
            Infinity, other | other, Infinity -> other
          }
        })
      #(state, Ok(JsNumber(result)))
    }
  }
}

/// Math.log(x)
pub fn math_log(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case n <. 0.0 {
        True -> NaN
        False ->
          case n == 0.0 {
            True -> NegInfinity
            False -> Finite(ffi_math_log(n))
          }
      }
    NaN | NegInfinity -> NaN
    Infinity -> Infinity
  }
}

/// Math.sin(x)
pub fn math_sin(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  finite_or_nan(x, ffi_math_sin)
}

/// Math.cos(x)
pub fn math_cos(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  finite_or_nan(x, ffi_math_cos)
}

// ============================================================================
// Internal helpers
// ============================================================================

/// Apply a unary JsNum->JsNum function to the first arg.
fn math_unary(
  args: List(JsValue),
  state: State,
  apply: fn(value.JsNum) -> value.JsNum,
) -> #(State, Result(JsValue, JsValue)) {
  let x = helpers.get_num_arg(args, 0, to_number)
  #(state, Ok(JsNumber(apply(x))))
}

/// For ops where Finite(n) -> Finite(f(n)) and non-finite passes through.
fn finite_passthrough(x: value.JsNum, f: fn(Float) -> Float) -> value.JsNum {
  case x {
    Finite(n) -> Finite(f(n))
    other -> other
  }
}

/// For ops where Finite(n) -> Finite(f(n)) and anything else is NaN.
fn finite_or_nan(x: value.JsNum, f: fn(Float) -> Float) -> value.JsNum {
  case x {
    Finite(n) -> Finite(f(n))
    _ -> NaN
  }
}

/// Simplified ToNumber for Math operations.
pub fn to_number(val: JsValue) -> value.JsNum {
  case val {
    JsNumber(n) -> n
    value.JsUndefined -> NaN
    value.JsNull -> Finite(0.0)
    value.JsBool(True) -> Finite(1.0)
    value.JsBool(False) -> Finite(0.0)
    JsString("") -> Finite(0.0)
    JsString("Infinity") -> Infinity
    JsString("-Infinity") -> NegInfinity
    JsString(s) ->
      case parse_float(s) {
        Ok(n) -> Finite(n)
        Error(_) ->
          case parse_int(s) {
            Ok(n) -> Finite(int.to_float(n))
            Error(_) -> NaN
          }
      }
    _ -> NaN
  }
}

/// JS Math.round: round half toward +Infinity.
/// Math.round(-0.5) → 0, Math.round(0.5) → 1
fn js_round(n: Float) -> Float {
  let floored = ffi_math_floor(n)
  case n -. floored >=. 0.5 {
    True -> floored +. 1.0
    False -> floored
  }
}

/// Exponentiation with special-value handling (mirrors vm.gleam's num_exp).
fn num_exp(a: value.JsNum, b: value.JsNum) -> value.JsNum {
  case a, b {
    _, Finite(0.0) -> Finite(1.0)
    _, NaN -> NaN
    NaN, _ -> NaN
    Finite(x), Finite(y) -> Finite(float_power(x, y))
    Infinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    NegInfinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    _, Infinity -> NaN
    _, NegInfinity -> NaN
  }
}

// -- FFI --

@external(erlang, "arc_vm_ffi", "float_power")
fn float_power(base: Float, exp: Float) -> Float

@external(erlang, "arc_vm_ffi", "math_sqrt")
fn ffi_math_sqrt(x: Float) -> Float

@external(erlang, "arc_vm_ffi", "math_log")
fn ffi_math_log(x: Float) -> Float

@external(erlang, "arc_vm_ffi", "math_sin")
fn ffi_math_sin(x: Float) -> Float

@external(erlang, "arc_vm_ffi", "math_cos")
fn ffi_math_cos(x: Float) -> Float

@external(erlang, "arc_vm_ffi", "math_floor")
fn ffi_math_floor(x: Float) -> Float

@external(erlang, "arc_vm_ffi", "math_ceil")
fn ffi_math_ceil(x: Float) -> Float

fn parse_float(s: String) -> Result(Float, Nil) {
  case gleam_stdlib_parse_float(s) {
    Ok(f) -> Ok(f)
    Error(_) -> Error(Nil)
  }
}

@external(erlang, "gleam_stdlib", "parse_float")
fn gleam_stdlib_parse_float(s: String) -> Result(Float, Nil)

fn parse_int(s: String) -> Result(Int, Nil) {
  int.parse(s)
}
