import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type JsValue, type MathNativeFn, type Ref, Finite, Infinity, JsNumber,
  JsString, MathAbs, MathAcos, MathAcosh, MathAsin, MathAsinh, MathAtan,
  MathAtan2, MathAtanh, MathCbrt, MathCeil, MathClz32, MathCos, MathCosh,
  MathExp, MathExpm1, MathFloor, MathFround, MathHypot, MathImul, MathLog,
  MathLog10, MathLog1p, MathLog2, MathMax, MathMin, MathNative, MathPow,
  MathRandom, MathRound, MathSign, MathSin, MathSinh, MathSqrt, MathTan,
  MathTanh, MathTrunc, NaN, NegInfinity,
}
import gleam/float
import gleam/int
import gleam/list
import gleam/result

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

  common.init_namespace(
    h,
    object_proto,
    "Math",
    list.append(methods, constants),
  )
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
    MathFloor -> finite_passthrough(args, state, ffi_math_floor)
    MathCeil -> finite_passthrough(args, state, ffi_math_ceil)
    MathRound -> finite_passthrough(args, state, js_round)
    MathTrunc -> finite_passthrough(args, state, js_trunc)
    MathSqrt -> math_sqrt(args, state)
    MathMax -> math_max(args, state)
    MathMin -> math_min(args, state)
    MathLog -> log_domain(args, state, ffi_math_log)
    MathSin -> finite_or_nan(args, state, ffi_math_sin)
    MathCos -> finite_or_nan(args, state, ffi_math_cos)
    MathTan -> finite_or_nan(args, state, ffi_math_tan)
    MathAsin -> domain_unit(args, state, ffi_math_asin)
    MathAcos -> domain_unit(args, state, ffi_math_acos)
    MathAtan -> math_atan(args, state)
    MathAtan2 -> math_atan2(args, state)
    MathExp -> math_exp(args, state)
    MathLog2 -> log_domain(args, state, ffi_math_log2)
    MathLog10 -> log_domain(args, state, ffi_math_log10)
    MathRandom -> math_random(args, state)
    MathSign -> math_sign(args, state)
    MathCbrt -> math_cbrt(args, state)
    MathHypot -> math_hypot(args, state)
    MathFround -> finite_passthrough(args, state, ffi_fround)
    MathClz32 -> math_clz32(args, state)
    MathImul -> math_imul(args, state)
    MathExpm1 -> math_expm1(args, state)
    MathLog1p -> math_log1p(args, state)
    MathSinh -> finite_passthrough(args, state, ffi_math_sinh)
    MathCosh -> math_cosh(args, state)
    MathTanh -> math_tanh(args, state)
    MathAsinh -> finite_passthrough(args, state, ffi_math_asinh)
    MathAcosh -> math_acosh(args, state)
    MathAtanh -> math_atanh(args, state)
  }
}

// ============================================================================
// Math method implementations
// ============================================================================

/// Math.pow(base, exponent)
fn math_pow(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let base = helpers.get_num_arg(args, 0, to_number)
  let exponent = helpers.get_num_arg(args, 1, to_number)
  #(state, Ok(JsNumber(num_exp(base, exponent))))
}

/// Math.abs(x)
fn math_abs(
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

/// Math.sqrt(x)
fn math_sqrt(
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

/// Math.max(a, b, ...) — returns -Infinity for no args.
fn math_max(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let result =
    list.fold(args, NegInfinity, fn(acc, arg) {
      case acc, to_number(arg) {
        NaN, _ | _, NaN -> NaN
        Finite(a), Finite(b) if a >=. b -> Finite(a)
        Finite(_), Finite(b) -> Finite(b)
        Infinity, _ | _, Infinity -> Infinity
        NegInfinity, other | other, NegInfinity -> other
      }
    })
  #(state, Ok(JsNumber(result)))
}

/// Math.min(a, b, ...) — returns +Infinity for no args.
fn math_min(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let result =
    list.fold(args, Infinity, fn(acc, arg) {
      case acc, to_number(arg) {
        NaN, _ | _, NaN -> NaN
        Finite(a), Finite(b) if a <=. b -> Finite(a)
        Finite(_), Finite(b) -> Finite(b)
        NegInfinity, _ | _, NegInfinity -> NegInfinity
        Infinity, other | other, Infinity -> other
      }
    })
  #(state, Ok(JsNumber(result)))
}

/// Math.atan(x)
fn math_atan(
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
fn math_atan2(
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
fn math_exp(
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

/// Math.random() — returns a random float in [0, 1)
fn math_random(
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  #(state, Ok(JsNumber(Finite(ffi_rand_uniform()))))
}

/// Math.sign(x) — returns -1, 0, or 1 (preserving ±0)
fn math_sign(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) if n >. 0.0 -> Finite(1.0)
    Finite(n) if n <. 0.0 -> Finite(-1.0)
    Finite(n) -> Finite(n)
    NaN -> NaN
    Infinity -> Finite(1.0)
    NegInfinity -> Finite(-1.0)
  }
}

/// Math.cbrt(x) — cube root
fn math_cbrt(
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

/// Math.hypot(a, b, ...) — sqrt(sum of squares). Per spec, ±∞ beats NaN.
fn math_hypot(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let nums = list.map(args, to_number)
  // Classify in one pass: track presence of ±∞, NaN, and running sum of squares.
  let #(inf, nan, sum_sq) =
    list.fold(nums, #(False, False, 0.0), fn(acc, n) {
      let #(i, na, s) = acc
      case n {
        Infinity | NegInfinity -> #(True, na, s)
        NaN -> #(i, True, s)
        Finite(v) -> #(i, na, s +. v *. v)
      }
    })
  let result = case inf, nan {
    True, _ -> Infinity
    _, True -> NaN
    _, _ -> Finite(ffi_math_sqrt(sum_sq))
  }
  #(state, Ok(JsNumber(result)))
}

/// Math.clz32(x) — count leading zeros in 32-bit integer representation
fn math_clz32(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let x = helpers.get_num_arg(args, 0, to_number)
  let n = case x {
    Finite(f) -> to_uint32(f)
    _ -> 0
  }
  #(state, Ok(value.from_int(count_leading_zeros_32(n))))
}

/// Math.imul(a, b) — 32-bit integer multiplication
fn math_imul(
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
  #(state, Ok(value.from_int(result)))
}

/// Math.expm1(x) — e^x - 1 (more precise for small x)
fn math_expm1(
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
fn math_log1p(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) if n <. -1.0 -> NaN
    Finite(-1.0) -> NegInfinity
    Finite(n) -> Finite(ffi_math_log(1.0 +. n))
    NaN | NegInfinity -> NaN
    Infinity -> Infinity
  }
}

/// Math.cosh(x)
fn math_cosh(
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
fn math_tanh(
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

/// Math.acosh(x) — domain [1, +Infinity), NaN for < 1
fn math_acosh(
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

/// Math.atanh(x) — domain (-1, 1), NaN outside, ±Infinity at ±1
fn math_atanh(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) if n <. -1.0 || n >. 1.0 -> NaN
    Finite(-1.0) -> NegInfinity
    Finite(1.0) -> Infinity
    Finite(n) -> Finite(ffi_math_atanh(n))
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

/// Unary op where Finite(n) -> Finite(f(n)) and non-finite passes through.
fn finite_passthrough(
  args: List(JsValue),
  state: State,
  f: fn(Float) -> Float,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(f(n))
    other -> other
  }
}

/// Unary op where Finite(n) -> Finite(f(n)) and anything else is NaN.
fn finite_or_nan(
  args: List(JsValue),
  state: State,
  f: fn(Float) -> Float,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(f(n))
    _ -> NaN
  }
}

/// Unary log-like op: domain [0, ∞), NaN for <0, -∞ at 0, ∞ passes through.
fn log_domain(
  args: List(JsValue),
  state: State,
  f: fn(Float) -> Float,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) if n <. 0.0 -> NaN
    Finite(0.0) -> NegInfinity
    Finite(n) -> Finite(f(n))
    NaN | NegInfinity -> NaN
    Infinity -> Infinity
  }
}

/// Unary op with domain [-1, 1]: NaN outside, Finite(f(n)) inside.
fn domain_unit(
  args: List(JsValue),
  state: State,
  f: fn(Float) -> Float,
) -> #(State, Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) if n <. -1.0 || n >. 1.0 -> NaN
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
      float.parse(s)
      |> result.try_recover(fn(_) { int.parse(s) |> result.map(int.to_float) })
      |> result.map(Finite)
      |> result.unwrap(NaN)
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

fn js_trunc(n: Float) -> Float {
  int.to_float(value.float_to_int(n))
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
