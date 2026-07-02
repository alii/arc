import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/ops/coerce

// `num_exp` (§6.1.6.1.3 Number::exponentiate), the ToInt32/ToUint32
// reductions and the ±0 float helpers live in the ops layer with the rest of
// the JsNum arithmetic — Math.pow/imul/clz32 are just callers. Builtins may
// depend on ops, never the reverse.
import arc/vm/ops/operators.{is_neg_zero, is_zero, num_exp, num_negate}
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type JsValue, type MathNativeFn, type Ref, Finite, Infinity, JsNumber, MathAbs,
  MathAcos, MathAcosh, MathAsin, MathAsinh, MathAtan, MathAtan2, MathAtanh,
  MathCbrt, MathCeil, MathClz32, MathCos, MathCosh, MathExp, MathExpm1,
  MathFloor, MathFround, MathHypot, MathImul, MathLog, MathLog10, MathLog1p,
  MathLog2, MathMax, MathMin, MathNative, MathPow, MathRandom, MathRound,
  MathSign, MathSin, MathSinh, MathSqrt, MathTan, MathTanh, MathTrunc, NaN,
  NegInfinity,
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
    MathFround -> math_fround(args, state)
    MathClz32 -> math_clz32(args, state)
    MathImul -> math_imul(args, state)
    MathExpm1 -> math_expm1(args, state)
    MathLog1p -> math_log1p(args, state)
    MathSinh -> math_sinh(args, state)
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
    // exp_total already yields Infinity when e^n overflows a 64-bit float.
    Finite(n) -> exp_total(n)
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
            // |n|^(1/3) never overflows for a finite n, but pow_total is
            // total so we get a JsNum back: negate it structurally.
            True -> num_negate(pow_total(float.absolute_value(n), 1.0 /. 3.0))
            False -> pow_total(n, 1.0 /. 3.0)
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
  // §7.1.7 ToUint32 (NaN/±∞ → 0).
  let n = operators.num_to_uint32(x)
  Finite(int.to_float(count_leading_zeros_32(n)))
}

/// Math.imul(a, b) — 32-bit integer multiplication
fn math_imul(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use a, b <- math_binary(args, state)
  // §7.1.6 ToInt32 (NaN/±∞ → 0) on each operand.
  let a32 = operators.num_to_int32(a)
  let b32 = operators.num_to_int32(b)
  // Wrap the exact Int product with operators.wrap_int32. It must NOT be
  // routed through a Float (e.g. `num_to_int32` of `int.to_float(a32 * b32)`):
  // the product can need up to 62 bits, so the low 32 bits — the only ones
  // imul keeps — get rounded away by the f64 conversion whenever
  // |product| > 2^53.
  Finite(int.to_float(operators.wrap_int32(a32 * b32)))
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
        False -> expm1_finite(n)
      }
    NaN -> NaN
    Infinity -> Infinity
    NegInfinity -> Finite(-1.0)
  }
}

/// e^n - 1 for a finite non-(-0) n.
///
/// Erlang's `math` module has no `expm1`; the naive `exp(n) -. 1.0` this
/// replaces loses ALL significant digits once |n| is small (e^n rounds to a
/// float adjacent to 1.0 and the subtraction cancels), so it returned 0 for
/// e.g. `Math.expm1(1e-10)`. Kahan's correction recovers full precision
/// from the (already rounded) `u = e^n`: `u - 1` is computed exactly by
/// Sterbenz, then rescaled by `n / ln(u)` — the ratio of the argument we
/// wanted to the argument `u` actually corresponds to.
fn expm1_finite(n: Float) -> value.JsNum {
  case exp_total(n) {
    // e^n rounded to exactly 1: n is tiny, and e^n - 1 == n to full precision.
    Finite(u) if u >=. 1.0 && u <=. 1.0 -> Finite(n)
    Finite(u) -> {
      let um1 = u -. 1.0
      case um1 == -1.0 {
        // e^n rounded to exactly 0 (n very negative): the answer is -1.
        True -> Finite(-1.0)
        // Divide FIRST: `n /. ln(u)` is ~1 (ln(u) is n to within a rounding),
        // so multiplying by `um1` last cannot overflow. The other order,
        // `um1 *. n`, is huge*huge near the top of the range and its
        // intermediate overflows a 64-bit float even though the true result
        // is finite — e.g. `Math.expm1(708)` would badarith on the BEAM.
        False -> Finite(um1 *. { n /. ffi_math_log(u) })
      }
    }
    // e^n overflowed a 64-bit float, so e^n - 1 overflows to the same value.
    Infinity -> Infinity
    // Unreachable from a finite n, but exp_total is total; pass them through.
    NaN -> NaN
    NegInfinity -> NegInfinity
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
        False -> log1p_finite(n)
      }
    NaN | NegInfinity -> NaN
    Infinity -> Infinity
  }
}

/// ln(1 + n) for a finite n > -1 (the caller has already handled n <= -1).
///
/// Erlang's `math` module has no `log1p`; the naive `log(1.0 +. n)` this
/// replaces loses ALL significant digits once |n| is small (`1.0 +. n`
/// rounds away n's low bits, or to exactly 1.0), so it returned 0 for e.g.
/// `Math.log1p(1e-10)`. Kahan's correction recovers full precision from the
/// (already rounded) `u = 1 + n`: `ln(u)` is the answer for the value
/// `u - 1` we actually have (exact by Sterbenz), so rescale by `n / (u-1)`.
///
/// `u` is always > 0 here — for n > -1 the nearest double to `1 + n` is
/// never 0 — so `ffi_math_log(u)` cannot badarith.
fn log1p_finite(n: Float) -> value.JsNum {
  let u = 1.0 +. n
  case u == 1.0 {
    // 1 + n rounded to exactly 1: n is tiny, and ln(1+n) == n to full
    // precision.
    True -> Finite(n)
    // Divide FIRST: `n /. (u - 1)` is ~1 (u - 1 is n to within a rounding),
    // so multiplying by `ln(u)` (|·| <= ~745) last cannot overflow. The
    // other order, `ln(u) *. n`, is ~709*n near the top of the range and its
    // intermediate overflows a 64-bit float even though the true result is
    // finite — e.g. `Math.log1p(1e308)` would badarith on the BEAM.
    False -> Finite(ffi_math_log(u) *. { n /. { u -. 1.0 } })
  }
}

/// Math.fround(x) — round to the nearest 32-bit float and widen back.
/// A finite double whose magnitude exceeds the float32 range rounds to
/// ±Infinity, which is why `ffi_fround` returns a JsNum, not a Float.
fn math_fround(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) -> ffi_fround(n)
    other -> other
  }
}

/// Math.sinh(x)
fn math_sinh(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    Finite(n) ->
      case is_neg_zero(n) {
        True -> Finite(-0.0)
        // sinh_total yields ±Infinity (matching n's sign) once sinh(n)
        // overflows a 64-bit float.
        False -> sinh_total(n)
      }
    other -> other
  }
}

/// Math.cosh(x)
fn math_cosh(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use x <- math_unary(args, state)
  case x {
    // cosh_total already yields Infinity when cosh(n) overflows.
    Finite(n) -> cosh_total(n)
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

/// Like finite_passthrough but preserves -0.0 (for asinh; sinh needs its
/// own `math_sinh` because its FFI must be total over overflow).
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
//
// TOTAL wrappers (arc_math_ffi): every `math` BIF that can OVERFLOW a
// 64-bit float — exp, pow, cosh, sinh — raises `badarith` on the BEAM
// instead of returning ±Infinity, and Math.fround's float32 encoding of an
// out-of-range double decodes to a `badmatch`. Any of those exceptions
// would crash the whole runtime. So those entry points go through
// arc_math_ffi, which catches the exception and returns the `value.JsNum`
// runtime shape directly (`Finite(f)` / `Infinity` / `NegInfinity` / `NaN`).
// Returning `JsNum` rather than `Float` means a caller CANNOT forget the
// overflow case — the compiler rejects treating the result as a plain Float.
//
// Do not add a raw `@external(erlang, "math", ...)` binding for an
// overflow-capable function; route it through arc_math_ffi instead.

@external(erlang, "arc_math_ffi", "exp")
fn exp_total(x: Float) -> value.JsNum

@external(erlang, "arc_math_ffi", "pow")
fn pow_total(base: Float, exp: Float) -> value.JsNum

@external(erlang, "arc_math_ffi", "cosh")
fn cosh_total(x: Float) -> value.JsNum

@external(erlang, "arc_math_ffi", "sinh")
fn sinh_total(x: Float) -> value.JsNum

@external(erlang, "arc_math_ffi", "fround")
fn ffi_fround(x: Float) -> value.JsNum

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

@external(erlang, "math", "log2")
fn ffi_math_log2(x: Float) -> Float

@external(erlang, "math", "log10")
fn ffi_math_log10(x: Float) -> Float

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
