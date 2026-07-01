import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/ops/coerce
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type JsValue, type MathNativeFn, type Ref, Finite, Infinity, JsNumber,
  MathAbs, MathAcos, MathAcosh, MathAsin, MathAsinh, MathAtan,
  MathAtan2, MathAtanh, MathCbrt, MathCeil, MathClz32, MathCos, MathCosh,
  MathExp, MathExpm1, MathFloor, MathFround, MathHypot, MathImul, MathLog,
  MathLog10, MathLog1p, MathLog2, MathMax, MathMin, MathNative, MathPow,
  MathRandom, MathRound, MathSign, MathSin, MathSinh, MathSqrt, MathTan,
  MathTanh, MathTrunc, NaN, NegInfinity,
}
import gleam/float
import gleam/int
import gleam/list

/// Set up the Math global object.
/// Math is NOT a constructor — it's a plain object with static methods.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), Ref) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
    MathSinh -> neg_zero_preserving(args, state, ffi_math_sinh)
    MathCosh -> math_cosh(args, state)
    MathTanh -> math_tanh(args, state)
    MathAsinh -> neg_zero_preserving(args, state, ffi_math_asinh)
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use base, exponent <- math_binary(args, state)
  num_exp(base, exponent)
}

/// Math.abs(x)
fn math_abs(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Per spec: Math.max(+0, -0) → +0, so a -0 accumulator loses ties.
  use a, b <- math_extremum(args, state, seed: NegInfinity, dominant: Infinity)
  a >=. b && { a >. b || !is_neg_zero(a) }
}

/// Math.min(a, b, ...) — returns +Infinity for no args.
fn math_min(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Per spec: Math.min(+0, -0) → -0, so a -0 argument wins ties.
  use a, b <- math_extremum(args, state, seed: Infinity, dominant: NegInfinity)
  a <=. b && { a <. b || !is_neg_zero(b) }
}

/// Math.max/min fold: `seed` loses to all, `dominant` beats all, `keep_acc` decides finites.
fn math_extremum(
  args: List(JsValue),
  state: State(host),
  seed seed: value.JsNum,
  dominant dominant: value.JsNum,
  keep_acc keep_acc: fn(Float, Float) -> Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  // §21.3.2.24/25 step 1: ? ToNumber every argument (in order) BEFORE the
  // fold — a later argument's valueOf must still run (and may throw) even
  // when an earlier one was already NaN.
  use nums, state <- coerce_args(args, state)
  let result =
    list.fold(nums, seed, fn(acc, num) {
      case acc, num {
        NaN, _ | _, NaN -> NaN
        Finite(a), Finite(b) ->
          case keep_acc(a, b) {
            True -> acc
            False -> Finite(b)
          }
        a, n if a == seed -> n
        _, b if b == seed -> acc
        _, _ -> dominant
      }
    })
  #(state, Ok(JsNumber(result)))
}

/// Math.atan(x)
fn math_atan(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use y, x <- math_binary(args, state)
  case y, x {
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
}

/// Math.exp(x)
fn math_exp(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  #(state, Ok(JsNumber(Finite(ffi_rand_uniform()))))
}

/// Math.sign(x) — returns -1, 0, or 1 (preserving ±0)
fn math_sign(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case is_neg_zero(n) {
        True -> Finite(-0.0)
        False ->
          case n <. 0.0 {
            True -> {
              let result = float_power(float.absolute_value(n), 1.0 /. 3.0)
              Finite(float.negate(result))
            }
            False -> Finite(float_power(n, 1.0 /. 3.0))
          }
      }
    NaN -> NaN
    Infinity -> Infinity
    NegInfinity -> NegInfinity
  }
}

/// Math.hypot(a, b, ...) — sqrt(sum of squares). Per spec, ±∞ beats NaN.
fn math_hypot(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // §21.3.2.18 step 1: ? ToNumber every argument, in order.
  use nums, state <- coerce_args(args, state)
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  let n = case x {
    Finite(f) -> to_uint32(f)
    _ -> 0
  }
  Finite(int.to_float(count_leading_zeros_32(n)))
}

/// Math.imul(a, b) — 32-bit integer multiplication
fn math_imul(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use a, b <- math_binary(args, state)
  let a32 = case a {
    Finite(f) -> to_int32(f)
    _ -> 0
  }
  let b32 = case b {
    Finite(f) -> to_int32(f)
    _ -> 0
  }
  // Wrap the exact Int product. It must NOT be routed through a Float
  // (e.g. `to_int32(int.to_float(a32 * b32))`): the product can need up to
  // 62 bits, so the low 32 bits — the only ones imul keeps — get rounded
  // away by the f64 conversion whenever |product| > 2^53.
  Finite(int.to_float(int_to_int32(a32 * b32)))
}

/// Math.expm1(x) — e^x - 1 (more precise for small x)
fn math_expm1(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case is_neg_zero(n) {
        True -> Finite(-0.0)
        False -> Finite(ffi_math_exp(n) -. 1.0)
      }
    NaN -> NaN
    Infinity -> Infinity
    NegInfinity -> Finite(-1.0)
  }
}

/// Math.log1p(x) — log(1 + x) (more precise for small x)
fn math_log1p(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) if n <. -1.0 -> NaN
    Finite(-1.0) -> NegInfinity
    Finite(n) ->
      case is_neg_zero(n) {
        True -> Finite(-0.0)
        False -> Finite(ffi_math_log(1.0 +. n))
      }
    NaN | NegInfinity -> NaN
    Infinity -> Infinity
  }
}

/// Math.cosh(x)
fn math_cosh(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case is_neg_zero(n) {
        True -> Finite(-0.0)
        False -> Finite(ffi_math_tanh(n))
      }
    NaN -> NaN
    Infinity -> Finite(1.0)
    NegInfinity -> Finite(-1.0)
  }
}

/// Math.acosh(x) — domain [1, +Infinity), NaN for < 1
fn math_acosh(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) if n <. -1.0 || n >. 1.0 -> NaN
    Finite(-1.0) -> NegInfinity
    Finite(1.0) -> Infinity
    Finite(n) ->
      case is_neg_zero(n) {
        True -> Finite(-0.0)
        False -> Finite(ffi_math_atanh(n))
      }
    _ -> NaN
  }
}

// ============================================================================
// Internal helpers
// ============================================================================

/// Apply a unary JsNum->JsNum function to the first arg.
///
/// §7.1.4 ToNumber via coerce.try_to_number: ToPrimitive runs on object
/// arguments (so `Math.abs({valueOf(){return -5}})` is 5) and Symbol/BigInt
/// arguments throw a TypeError, which propagates through the Result.
fn math_unary(
  args: List(JsValue),
  state: State(host),
  apply: fn(value.JsNum) -> value.JsNum,
) -> #(State(host), Result(JsValue, JsValue)) {
  use x, state <- coerce.try_to_number(
    state,
    helpers.first_arg_or_undefined(args),
  )
  #(state, Ok(JsNumber(apply(x))))
}

/// Apply a binary JsNum op to the first two args, coercing each in argument
/// order with §7.1.4 ToNumber (same ToPrimitive/TypeError behaviour as
/// math_unary — the second argument is not coerced if the first throws).
fn math_binary(
  args: List(JsValue),
  state: State(host),
  apply: fn(value.JsNum, value.JsNum) -> value.JsNum,
) -> #(State(host), Result(JsValue, JsValue)) {
  use a, state <- coerce.try_to_number(
    state,
    helpers.first_arg_or_undefined(args),
  )
  use b, state <- coerce.try_to_number(state, second_arg_or_undefined(args))
  #(state, Ok(JsNumber(apply(a, b))))
}

fn second_arg_or_undefined(args: List(JsValue)) -> JsValue {
  case args {
    [_, v, ..] -> v
    _ -> value.JsUndefined
  }
}

/// §7.1.4 ToNumber over a whole argument list, left to right, threading
/// State. The first coercion that throws aborts the rest (its `use`
/// short-circuits), matching the spec's argument-by-argument `?` points.
fn coerce_args(
  args: List(JsValue),
  state: State(host),
  cont: fn(List(value.JsNum), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  coerce_args_loop(args, state, [], cont)
}

fn coerce_args_loop(
  args: List(JsValue),
  state: State(host),
  acc: List(value.JsNum),
  cont: fn(List(value.JsNum), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case args {
    [] -> cont(list.reverse(acc), state)
    [arg, ..rest] -> {
      use n, state <- coerce.try_to_number(state, arg)
      coerce_args_loop(rest, state, [n, ..acc], cont)
    }
  }
}

/// Like finite_passthrough but preserves -0.0 (for sinh, asinh, etc.)
fn neg_zero_preserving(
  args: List(JsValue),
  state: State(host),
  f: fn(Float) -> Float,
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case is_neg_zero(n) {
        True -> Finite(-0.0)
        False -> Finite(f(n))
      }
    other -> other
  }
}

/// Unary op where Finite(n) -> Finite(f(n)) and non-finite passes through.
fn finite_passthrough(
  args: List(JsValue),
  state: State(host),
  f: fn(Float) -> Float,
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(f(n))
    other -> other
  }
}

/// Unary op where Finite(n) -> Finite(f(n)) and anything else is NaN.
fn finite_or_nan(
  args: List(JsValue),
  state: State(host),
  f: fn(Float) -> Float,
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> Finite(f(n))
    _ -> NaN
  }
}

/// Unary log-like op: domain [0, ∞), NaN for <0, -∞ at 0, ∞ passes through.
fn log_domain(
  args: List(JsValue),
  state: State(host),
  f: fn(Float) -> Float,
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    // ±0 → -Infinity. This MUST be a guard, not a `Finite(0.0)` literal
    // pattern: on OTP >= 27 that pattern only matches +0.0, so Math.log(-0)
    // (for which `n <. 0.0` is also False) would fall through to
    // `math:log(-0.0)` and crash the VM with badarith.
    Finite(n) if n >=. 0.0 && n <=. 0.0 -> NegInfinity
    Finite(n) if n <. 0.0 -> NaN
    Finite(n) -> Finite(f(n))
    NaN | NegInfinity -> NaN
    Infinity -> Infinity
  }
}

/// Unary op with domain [-1, 1]: NaN outside, Finite(f(n)) inside.
fn domain_unit(
  args: List(JsValue),
  state: State(host),
  f: fn(Float) -> Float,
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) if n <. -1.0 || n >. 1.0 -> NaN
    Finite(n) -> Finite(f(n))
    _ -> NaN
  }
}

/// JS Math.round: round half toward +Infinity.
/// Math.round(-0.5) → -0, Math.round(-0.3) → -0, Math.round(0.5) → 1
fn js_round(n: Float) -> Float {
  let floored = ffi_math_floor(n)
  let rounded = case n -. floored >=. 0.5 {
    True -> floored +. 1.0
    False -> floored
  }
  // Per spec, a zero result from a negative (or -0) input is -0. This has
  // to be a single post-condition on `rounded`: for n in [-0.5, 0.0) the
  // half-up branch above yields `floored +. 1.0` = -1.0 +. 1.0 = +0.0, so
  // any check attached to only the `floored` branch never sees those inputs.
  case is_zero(rounded) && { n <. 0.0 || is_neg_zero(n) } {
    True -> -0.0
    False -> rounded
  }
}

fn js_trunc(n: Float) -> Float {
  case is_neg_zero(n) {
    // -0 → -0
    True -> n
    False -> {
      let truncated = int.to_float(value.float_to_int(n))
      // Values in (-1, 0) truncate to -0 per spec
      case truncated == 0.0 && n <. 0.0 {
        True -> -0.0
        False -> truncated
      }
    }
  }
}

/// Exponentiation per ES2024 §6.1.6.1.3 Number::exponentiate.
pub fn num_exp(base: value.JsNum, exp: value.JsNum) -> value.JsNum {
  case base, exp {
    // 1. If exponent is NaN, return NaN
    _, NaN -> NaN
    // 2. If exponent is +0 or -0, return 1 (even for NaN/Infinity base).
    // Guard, not a `Finite(0.0)` literal pattern: on OTP >= 27 the literal
    // only matches +0.0, so a -0.0 exponent would fall through to the
    // finite/infinite arms below and produce the wrong answer.
    _, Finite(e) if e >=. 0.0 && e <=. 0.0 -> Finite(1.0)
    // 3. If base is NaN, return NaN
    NaN, _ -> NaN
    // 4-5. ±Infinity base with ±Infinity exponent
    Infinity, Infinity -> Infinity
    Infinity, NegInfinity -> Finite(0.0)
    NegInfinity, Infinity -> Infinity
    NegInfinity, NegInfinity -> Finite(0.0)
    // abs(base) vs 1 with ±Infinity exponent
    Finite(x), Infinity -> num_exp_finite_inf(x)
    Finite(x), NegInfinity -> num_exp_finite_neginf(x)
    // 6-7. Base is +Infinity
    Infinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    // 8-11. Base is -Infinity
    NegInfinity, Finite(y) ->
      case y >. 0.0 {
        True ->
          case is_odd_integer(y) {
            True -> NegInfinity
            False -> Infinity
          }
        False ->
          case is_odd_integer(y) {
            True -> Finite(-0.0)
            False -> Finite(0.0)
          }
      }
    // 12-17. Base is ±0
    Finite(x), Finite(y) -> num_exp_finite(x, y)
  }
}

fn num_exp_finite_inf(x: Float) -> value.JsNum {
  let abs_x = float.absolute_value(x)
  case abs_x >. 1.0 {
    True -> Infinity
    False ->
      case abs_x <. 1.0 {
        True -> Finite(0.0)
        // abs == 1
        False -> NaN
      }
  }
}

fn num_exp_finite_neginf(x: Float) -> value.JsNum {
  let abs_x = float.absolute_value(x)
  case abs_x >. 1.0 {
    True -> Finite(0.0)
    False ->
      case abs_x <. 1.0 {
        True -> Infinity
        // abs == 1
        False -> NaN
      }
  }
}

fn num_exp_finite(x: Float, y: Float) -> value.JsNum {
  case is_neg_zero(x) {
    // Base is -0
    True ->
      case y >. 0.0 {
        True ->
          case is_odd_integer(y) {
            True -> Finite(-0.0)
            False -> Finite(0.0)
          }
        False ->
          case is_odd_integer(y) {
            True -> NegInfinity
            False -> Infinity
          }
      }
    False ->
      case x == 0.0 {
        // Base is +0
        True ->
          case y >. 0.0 {
            True -> Finite(0.0)
            False -> Infinity
          }
        // Normal finite case
        False ->
          case x <. 0.0 {
            True -> {
              let truncated = int.to_float(value.float_to_int(y))
              case truncated == y {
                True -> Finite(float_power(x, y))
                // Non-integer exponent with negative base
                False -> NaN
              }
            }
            False -> Finite(float_power(x, y))
          }
      }
  }
}

/// Check if a float is an odd integer.
fn is_odd_integer(f: Float) -> Bool {
  let truncated = value.float_to_int(f)
  int.to_float(truncated) == f && int.is_odd(truncated)
}

/// Wrap an exact Int to an unsigned 32-bit value.
///
/// Work on Ints stays on Ints: converting an intermediate Int (e.g. the
/// `a32 * b32` product in Math.imul) to a Float before wrapping rounds the
/// low 32 bits away once the magnitude passes 2^53. `to_uint32`/`to_int32`
/// below are the ONLY Float entry points, and they immediately truncate to
/// an Int and delegate here.
fn int_to_uint32(n: Int) -> Int {
  int.bitwise_and(n, 0xFFFFFFFF)
}

/// Wrap an exact Int to a signed 32-bit value (see `int_to_uint32`).
fn int_to_int32(n: Int) -> Int {
  let u = int_to_uint32(n)
  case u >= 0x80000000 {
    True -> u - 0x100000000
    False -> u
  }
}

/// ToUint32: convert a float to a 32-bit unsigned integer (ES S7.1.7).
fn to_uint32(f: Float) -> Int {
  int_to_uint32(value.float_to_int(f))
}

/// ToInt32: convert a float to a 32-bit signed integer (ES S7.1.6).
fn to_int32(f: Float) -> Int {
  int_to_int32(value.float_to_int(f))
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

/// True for both +0.0 and -0.0 (companion to `is_neg_zero`).
///
/// Always use this instead of a `0.0` literal in a case pattern or an
/// `x == 0.0` comparison: on OTP >= 27 both of those compile to `=:=`
/// semantics and are True/matched only for +0.0, silently missing -0.0.
/// The `>=.`/`<=.` pair is an arithmetic comparison, which treats the two
/// zeros as equal. Usable anywhere; in a guard position (where function
/// calls are not allowed) inline the same expression:
/// `Finite(n) if n >=. 0.0 && n <=. 0.0 -> ...`
pub fn is_zero(x: Float) -> Bool {
  x >=. 0.0 && x <=. 0.0
}

// -- FFI --

@external(erlang, "arc_math_ffi", "is_neg_zero")
pub fn is_neg_zero(x: Float) -> Bool

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
