import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/frame.{type State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type MathNativeFn, type Ref, Finite, Infinity, JsNumber,
  JsString, MathAbs, MathAcos, MathAcosh, MathAsin, MathAsinh, MathAtan,
  MathAtan2, MathAtanh, MathCbrt, MathCeil, MathClz32, MathCos, MathCosh,
  MathExp, MathExpm1, MathFloor, MathFround, MathHypot, MathImul, MathLog,
  MathLog10, MathLog1p, MathLog2, MathMax, MathMin, MathNative, MathPow,
  MathRandom, MathRound, MathSign, MathSin, MathSinh, MathSqrt, MathTan,
  MathTanh, MathTrunc, NaN, NegInfinity, ObjectSlot, OrdinaryObject,
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
    #("LN2", value.data(JsNumber(Finite(0.6931471805599453)))),
    #("LN10", value.data(JsNumber(Finite(2.302585092994046)))),
    #("LOG2E", value.data(JsNumber(Finite(1.4426950408889634)))),
    #("LOG10E", value.data(JsNumber(Finite(0.4342944819032518)))),
    #("SQRT2", value.data(JsNumber(Finite(1.4142135623730951)))),
    #("SQRT1_2", value.data(JsNumber(Finite(0.7071067811865476)))),
  ]

  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("pow", MathNative(MathPow), 2),
      #("abs", MathNative(MathAbs), 1),
      #("floor", MathNative(MathFloor), 1),
      #("ceil", MathNative(MathCeil), 1),
      #("round", MathNative(MathRound), 1),
      #("trunc", MathNative(MathTrunc), 1),
      #("sqrt", MathNative(MathSqrt), 1),
      #("max", MathNative(MathMax), 2),
      #("min", MathNative(MathMin), 2),
      #("log", MathNative(MathLog), 1),
      #("sin", MathNative(MathSin), 1),
      #("cos", MathNative(MathCos), 1),
      #("tan", MathNative(MathTan), 1),
      #("asin", MathNative(MathAsin), 1),
      #("acos", MathNative(MathAcos), 1),
      #("atan", MathNative(MathAtan), 1),
      #("atan2", MathNative(MathAtan2), 2),
      #("exp", MathNative(MathExp), 1),
      #("log2", MathNative(MathLog2), 1),
      #("log10", MathNative(MathLog10), 1),
      #("random", MathNative(MathRandom), 0),
      #("sign", MathNative(MathSign), 1),
      #("cbrt", MathNative(MathCbrt), 1),
      #("hypot", MathNative(MathHypot), 2),
      #("fround", MathNative(MathFround), 1),
      #("clz32", MathNative(MathClz32), 1),
      #("imul", MathNative(MathImul), 2),
      #("expm1", MathNative(MathExpm1), 1),
      #("log1p", MathNative(MathLog1p), 1),
      #("sinh", MathNative(MathSinh), 1),
      #("cosh", MathNative(MathCosh), 1),
      #("tanh", MathNative(MathTanh), 1),
      #("asinh", MathNative(MathAsinh), 1),
      #("acosh", MathNative(MathAcosh), 1),
      #("atanh", MathNative(MathAtanh), 1),
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

/// Per-module dispatch for Math native functions.
pub fn dispatch(
  native: MathNativeFn,
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    MathPow -> math_pow(args, state)
    MathAbs -> math_abs(args, state)
    MathFloor -> math_floor(args, state)
    MathCeil -> math_ceil(args, state)
    MathRound -> math_round(args, state)
    MathTrunc -> math_trunc(args, state)
    MathSqrt -> math_sqrt(args, state)
    MathMax -> math_max(args, state)
    MathMin -> math_min(args, state)
    MathLog -> math_log(args, state)
    MathSin -> math_sin(args, state)
    MathCos -> math_cos(args, state)
    MathTan -> math_tan(args, state)
    MathAsin -> math_asin(args, state)
    MathAcos -> math_acos(args, state)
    MathAtan -> math_atan(args, state)
    MathAtan2 -> math_atan2(args, state)
    MathExp -> math_exp(args, state)
    MathLog2 -> math_log2(args, state)
    MathLog10 -> math_log10(args, state)
    MathRandom -> math_random(args, state)
    MathSign -> math_sign(args, state)
    MathCbrt -> math_cbrt(args, state)
    MathHypot -> math_hypot(args, state)
    MathFround -> math_fround(args, state)
    MathClz32 -> math_clz32(args, state)
    MathImul -> math_imul(args, state)
    MathExpm1 -> math_expm1(args, state)
    MathLog1p -> math_log1p(args, state)
    MathSinh -> math_sinh(args, state)
    MathCosh -> math_cosh(args, state)
    MathTanh -> math_tanh(args, state)
    MathAsinh -> math_asinh(args, state)
    MathAcosh -> math_acosh(args, state)
    MathAtanh -> math_atanh(args, state)
  }
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

/// Math.tan(x)
pub fn math_tan(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  finite_or_nan(x, ffi_math_tan)
}

/// Math.asin(x) — domain [-1, 1], returns NaN outside
pub fn math_asin(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case n <. -1.0 || n >. 1.0 {
        True -> NaN
        False -> Finite(ffi_math_asin(n))
      }
    _ -> NaN
  }
}

/// Math.acos(x) — domain [-1, 1], returns NaN outside
pub fn math_acos(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case n <. -1.0 || n >. 1.0 {
        True -> NaN
        False -> Finite(ffi_math_acos(n))
      }
    _ -> NaN
  }
}

/// Math.atan(x)
pub fn math_atan(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(ffi_math_atan(n))
    NaN -> NaN
    Infinity -> Finite(ffi_math_atan2(1.0, 0.0))
    NegInfinity -> Finite(ffi_math_atan2(-1.0, 0.0))
  }
}

/// Math.atan2(y, x)
pub fn math_atan2(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let y = helpers.get_num_arg(args, 0, to_number)
  let x = helpers.get_num_arg(args, 1, to_number)
  let result = case y, x {
    NaN, _ | _, NaN -> NaN
    Finite(yv), Finite(xv) -> Finite(ffi_math_atan2(yv, xv))
    Finite(yv), Infinity ->
      case yv >=. 0.0 {
        True -> Finite(0.0)
        False -> Finite(-0.0)
      }
    Finite(yv), NegInfinity ->
      case yv >=. 0.0 {
        True -> Finite(3.141592653589793)
        False -> Finite(-3.141592653589793)
      }
    Infinity, Finite(_) -> Finite(ffi_math_atan2(1.0, 0.0))
    NegInfinity, Finite(_) -> Finite(ffi_math_atan2(-1.0, 0.0))
    Infinity, Infinity -> Finite(ffi_math_atan2(1.0, 1.0))
    Infinity, NegInfinity -> Finite(ffi_math_atan2(1.0, -1.0))
    NegInfinity, Infinity -> Finite(ffi_math_atan2(-1.0, 1.0))
    NegInfinity, NegInfinity -> Finite(ffi_math_atan2(-1.0, -1.0))
  }
  #(state, Ok(JsNumber(result)))
}

/// Math.exp(x)
pub fn math_exp(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(ffi_math_exp(n))
    NaN -> NaN
    Infinity -> Infinity
    NegInfinity -> Finite(0.0)
  }
}

/// Math.log2(x)
pub fn math_log2(
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
            False -> Finite(ffi_math_log2(n))
          }
      }
    NaN | NegInfinity -> NaN
    Infinity -> Infinity
  }
}

/// Math.log10(x)
pub fn math_log10(
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
            False -> Finite(ffi_math_log10(n))
          }
      }
    NaN | NegInfinity -> NaN
    Infinity -> Infinity
  }
}

/// Math.random() — returns a random float in [0, 1)
pub fn math_random(
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  #(state, Ok(JsNumber(Finite(ffi_rand_uniform()))))
}

/// Math.sign(x) — returns -1, 0, or 1
pub fn math_sign(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case n >. 0.0 {
        True -> Finite(1.0)
        False ->
          case n <. 0.0 {
            True -> Finite(-1.0)
            False -> Finite(n)
          }
      }
    NaN -> NaN
    Infinity -> Finite(1.0)
    NegInfinity -> Finite(-1.0)
  }
}

/// Math.cbrt(x) — cube root
pub fn math_cbrt(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case n <. 0.0 {
        True -> {
          let result = float_power(float.absolute_value(n), 1.0 /. 3.0)
          Finite(float.negate(result))
        }
        False -> Finite(float_power(n, 1.0 /. 3.0))
      }
    NaN -> NaN
    Infinity -> Infinity
    NegInfinity -> NegInfinity
  }
}

/// Math.hypot(a, b, ...) — sqrt(sum of squares) with overflow protection
pub fn math_hypot(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [] -> #(state, Ok(JsNumber(Finite(0.0))))
    _ -> {
      let nums = list.map(args, fn(arg) { to_number(arg) })
      // If any arg is +-Infinity, result is Infinity (even if NaN present)
      let has_infinity =
        list.any(nums, fn(n) {
          case n {
            Infinity | NegInfinity -> True
            _ -> False
          }
        })
      case has_infinity {
        True -> #(state, Ok(JsNumber(Infinity)))
        False -> {
          let has_nan =
            list.any(nums, fn(n) {
              case n {
                NaN -> True
                _ -> False
              }
            })
          case has_nan {
            True -> #(state, Ok(JsNumber(NaN)))
            False -> {
              let sum_sq =
                list.fold(nums, 0.0, fn(acc, n) {
                  case n {
                    Finite(v) -> acc +. { v *. v }
                    _ -> acc
                  }
                })
              #(state, Ok(JsNumber(Finite(ffi_math_sqrt(sum_sq)))))
            }
          }
        }
      }
    }
  }
}

/// Math.fround(x) — round to nearest 32-bit float
pub fn math_fround(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(ffi_fround(n))
    other -> other
  }
}

/// Math.clz32(x) — count leading zeros in 32-bit integer representation
pub fn math_clz32(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let x = helpers.get_num_arg(args, 0, to_number)
  let n = case x {
    Finite(f) -> to_uint32(f)
    _ -> 0
  }
  #(state, Ok(JsNumber(Finite(int.to_float(count_leading_zeros_32(n))))))
}

/// Math.imul(a, b) — 32-bit integer multiplication
pub fn math_imul(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let a = helpers.get_num_arg(args, 0, to_number)
  let b = helpers.get_num_arg(args, 1, to_number)
  let a32 = case a {
    Finite(f) -> to_int32(f)
    _ -> 0
  }
  let b32 = case b {
    Finite(f) -> to_int32(f)
    _ -> 0
  }
  let result = to_int32(int.to_float(a32 * b32))
  #(state, Ok(JsNumber(Finite(int.to_float(result)))))
}

/// Math.expm1(x) — e^x - 1 (more precise for small x)
pub fn math_expm1(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(ffi_math_exp(n) -. 1.0)
    NaN -> NaN
    Infinity -> Infinity
    NegInfinity -> Finite(-1.0)
  }
}

/// Math.log1p(x) — log(1 + x) (more precise for small x)
pub fn math_log1p(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case n <. -1.0 {
        True -> NaN
        False ->
          case n == -1.0 {
            True -> NegInfinity
            False -> Finite(ffi_math_log(1.0 +. n))
          }
      }
    NaN -> NaN
    Infinity -> Infinity
    NegInfinity -> NaN
  }
}

/// Math.sinh(x)
pub fn math_sinh(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(ffi_math_sinh(n))
    other -> other
  }
}

/// Math.cosh(x)
pub fn math_cosh(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(ffi_math_cosh(n))
    NaN -> NaN
    Infinity | NegInfinity -> Infinity
  }
}

/// Math.tanh(x)
pub fn math_tanh(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(ffi_math_tanh(n))
    NaN -> NaN
    Infinity -> Finite(1.0)
    NegInfinity -> Finite(-1.0)
  }
}

/// Math.asinh(x)
pub fn math_asinh(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(ffi_math_asinh(n))
    other -> other
  }
}

/// Math.acosh(x) — domain [1, +Infinity), NaN for < 1
pub fn math_acosh(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case n <. 1.0 {
        True -> NaN
        False -> Finite(ffi_math_acosh(n))
      }
    NaN | NegInfinity -> NaN
    Infinity -> Infinity
  }
}

/// Math.atanh(x) — domain (-1, 1), NaN outside, +-Infinity at +-1
pub fn math_atanh(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case n <. -1.0 || n >. 1.0 {
        True -> NaN
        False ->
          case n == -1.0 {
            True -> NegInfinity
            False ->
              case n == 1.0 {
                True -> Infinity
                False -> Finite(ffi_math_atanh(n))
              }
          }
      }
    _ -> NaN
  }
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

/// ToUint32: convert a float to a 32-bit unsigned integer (ES S7.1.7).
fn to_uint32(f: Float) -> Int {
  let n = value.float_to_int(f)
  int.bitwise_and(n, 0xFFFFFFFF)
}

/// ToInt32: convert a float to a 32-bit signed integer (ES S7.1.6).
fn to_int32(f: Float) -> Int {
  let n = to_uint32(f)
  case n >= 0x80000000 {
    True -> n - 0x100000000
    False -> n
  }
}

/// Count leading zeros in a 32-bit integer.
fn count_leading_zeros_32(n: Int) -> Int {
  count_leading_zeros_loop(n, 31, 0)
}

fn count_leading_zeros_loop(n: Int, bit: Int, count: Int) -> Int {
  case bit < 0 {
    True -> count
    False -> {
      let mask = int.bitwise_shift_left(1, bit)
      case int.bitwise_and(n, mask) != 0 {
        True -> count
        False -> count_leading_zeros_loop(n, bit - 1, count + 1)
      }
    }
  }
}

// -- FFI --

@external(erlang, "math", "pow")
fn float_power(base: Float, exp: Float) -> Float

@external(erlang, "math", "sqrt")
fn ffi_math_sqrt(x: Float) -> Float

@external(erlang, "math", "log")
fn ffi_math_log(x: Float) -> Float

@external(erlang, "math", "sin")
fn ffi_math_sin(x: Float) -> Float

@external(erlang, "math", "cos")
fn ffi_math_cos(x: Float) -> Float

@external(erlang, "math", "floor")
fn ffi_math_floor(x: Float) -> Float

@external(erlang, "math", "ceil")
fn ffi_math_ceil(x: Float) -> Float

@external(erlang, "math", "tan")
fn ffi_math_tan(x: Float) -> Float

@external(erlang, "math", "asin")
fn ffi_math_asin(x: Float) -> Float

@external(erlang, "math", "acos")
fn ffi_math_acos(x: Float) -> Float

@external(erlang, "math", "atan")
fn ffi_math_atan(x: Float) -> Float

@external(erlang, "math", "atan2")
fn ffi_math_atan2(y: Float, x: Float) -> Float

@external(erlang, "math", "exp")
fn ffi_math_exp(x: Float) -> Float

@external(erlang, "math", "log2")
fn ffi_math_log2(x: Float) -> Float

@external(erlang, "math", "log10")
fn ffi_math_log10(x: Float) -> Float

@external(erlang, "math", "sinh")
fn ffi_math_sinh(x: Float) -> Float

@external(erlang, "math", "cosh")
fn ffi_math_cosh(x: Float) -> Float

@external(erlang, "math", "tanh")
fn ffi_math_tanh(x: Float) -> Float

@external(erlang, "math", "asinh")
fn ffi_math_asinh(x: Float) -> Float

@external(erlang, "math", "acosh")
fn ffi_math_acosh(x: Float) -> Float

@external(erlang, "math", "atanh")
fn ffi_math_atanh(x: Float) -> Float

@external(erlang, "rand", "uniform")
fn ffi_rand_uniform() -> Float

@external(erlang, "arc_math_ffi", "fround")
fn ffi_fround(x: Float) -> Float

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
