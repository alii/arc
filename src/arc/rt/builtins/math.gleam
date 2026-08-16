//// The `Math` global namespace (ES2024 §21.3).
////
//// Works over the threaded Agent + JsNum shape
//// (`JInt|JFloat|JNan|JPosInf|JNegInf`); every finite arm widens through
//// `finite_to_float` so the arithmetic stays on Floats.
//// Return-tuple order is `#(JsVal, Agent)` (R1); an argument's
//// ToNumber TypeError (Symbol/BigInt) diverges via `t_throw` inside
//// `t_to_number` (D7).

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/types.{
  type Agent, type Handle, type JsNum, type JsVal, type MathNative, JFloat, JInt,
  JNan, JNegInf, JPosInf, MathAbs, MathAcos, MathAcosh, MathAsin, MathAsinh,
  MathAtan, MathAtan2, MathAtanh, MathCbrt, MathCeil, MathClz32, MathCos,
  MathCosh, MathExp, MathExpm1, MathFloor, MathFround, MathHypot, MathImul,
  MathLog, MathLog10, MathLog1p, MathLog2, MathMax, MathMin, MathN, MathPow,
  MathRandom, MathRound, MathSign, MathSin, MathSinh, MathSqrt, MathTan,
  MathTanh, MathTrunc, mk_number,
}
import arc/rt/val as rt_val
import gleam/float
import gleam/int
import gleam/list
import gleam/option

/// Set up the Math global object.
/// Math is NOT a constructor — it's a plain object with static methods.
pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(Handle, Agent) {
  let #(constants, st) =
    list.fold(
      [
        #("PI", 3.141592653589793),
        #("E", 2.718281828459045),
        #("LN2", 0.6931471805599453),
        #("LN10", 2.302585092994046),
        #("LOG2E", 1.4426950408889634),
        #("LOG10E", 0.4342944819032518),
        #("SQRT2", 1.4142135623730951),
        #("SQRT1_2", 0.7071067811865476),
      ],
      #([], st),
      fn(acc, entry) {
        let #(props, st) = acc
        let #(name, f) = entry
        let #(prop, st) = common.data_prop(st, mk_number(JFloat(f)))
        #([#(name, prop), ..props], st)
      },
    )

  let #(methods, st) =
    common.alloc_methods(st, function_proto, [
      #("pow", MathN(MathPow), 2),
      #("abs", MathN(MathAbs), 1),
      #("floor", MathN(MathFloor), 1),
      #("ceil", MathN(MathCeil), 1),
      #("round", MathN(MathRound), 1),
      #("trunc", MathN(MathTrunc), 1),
      #("sqrt", MathN(MathSqrt), 1),
      #("max", MathN(MathMax), 2),
      #("min", MathN(MathMin), 2),
      #("log", MathN(MathLog), 1),
      #("sin", MathN(MathSin), 1),
      #("cos", MathN(MathCos), 1),
      #("tan", MathN(MathTan), 1),
      #("asin", MathN(MathAsin), 1),
      #("acos", MathN(MathAcos), 1),
      #("atan", MathN(MathAtan), 1),
      #("atan2", MathN(MathAtan2), 2),
      #("exp", MathN(MathExp), 1),
      #("log2", MathN(MathLog2), 1),
      #("log10", MathN(MathLog10), 1),
      #("random", MathN(MathRandom), 0),
      #("sign", MathN(MathSign), 1),
      #("cbrt", MathN(MathCbrt), 1),
      #("hypot", MathN(MathHypot), 2),
      #("fround", MathN(MathFround), 1),
      #("clz32", MathN(MathClz32), 1),
      #("imul", MathN(MathImul), 2),
      #("expm1", MathN(MathExpm1), 1),
      #("log1p", MathN(MathLog1p), 1),
      #("sinh", MathN(MathSinh), 1),
      #("cosh", MathN(MathCosh), 1),
      #("tanh", MathN(MathTanh), 1),
      #("asinh", MathN(MathAsinh), 1),
      #("acosh", MathN(MathAcosh), 1),
      #("atanh", MathN(MathAtanh), 1),
    ])

  common.init_namespace(
    st,
    object_proto,
    "Math",
    list.append(methods, constants),
  )
}

/// Per-module dispatch for Math native functions.
pub fn dispatch(
  st: Agent,
  native: MathNative,
  _this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    MathPow -> math_pow(args, st)
    MathAbs -> math_abs(args, st)
    MathFloor -> rounding_passthrough(args, st, ffi_math_floor)
    MathCeil -> rounding_passthrough(args, st, ffi_math_ceil)
    MathRound -> rounding_passthrough(args, st, js_round)
    MathTrunc -> rounding_passthrough(args, st, js_trunc)
    MathSqrt -> math_sqrt(args, st)
    MathMax -> math_max(args, st)
    MathMin -> math_min(args, st)
    MathLog -> log_domain(args, st, ffi_math_log)
    MathSin -> finite_or_nan(args, st, ffi_math_sin)
    MathCos -> finite_or_nan(args, st, ffi_math_cos)
    MathTan -> finite_or_nan(args, st, ffi_math_tan)
    MathAsin -> domain_unit(args, st, ffi_math_asin)
    MathAcos -> domain_unit(args, st, ffi_math_acos)
    MathAtan -> math_atan(args, st)
    MathAtan2 -> math_atan2(args, st)
    MathExp -> math_exp(args, st)
    MathLog2 -> log_domain(args, st, ffi_math_log2)
    MathLog10 -> log_domain(args, st, ffi_math_log10)
    MathRandom -> math_random(st)
    MathSign -> math_sign(args, st)
    MathCbrt -> math_cbrt(args, st)
    MathHypot -> math_hypot(args, st)
    MathFround -> math_fround(args, st)
    MathClz32 -> math_clz32(args, st)
    MathImul -> math_imul(args, st)
    MathExpm1 -> math_expm1(args, st)
    MathLog1p -> math_log1p(args, st)
    MathSinh -> math_sinh(args, st)
    MathCosh -> math_cosh(args, st)
    MathTanh -> math_tanh(args, st)
    MathAsinh -> neg_zero_preserving(args, st, ffi_math_asinh)
    MathAcosh -> math_acosh(args, st)
    MathAtanh -> math_atanh(args, st)
  }
}

// ============================================================================
// Math method implementations
// ============================================================================

/// Math.pow(base, exponent) — §6.1.6.1.3 Number::exponentiate.
fn math_pow(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use a, b <- math_binary(args, st)
  num_exp(a, b)
}

/// Math.abs(x)
///
/// §21.3.2.1 step 4: "If x is -0𝔽, return +0𝔽". `float.absolute_value` gets
/// this wrong — it is `case x >=. 0.0 { True -> x .. }`, and `-0.0 >=. 0.0`
/// is True on the BEAM, so it hands -0.0 straight back. Use the canonical
/// sign predicate instead: `0.0 -. -0.0` is +0.0, and `0.0 -. -5.0` is 5.0.
fn math_abs(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(n) if n < 0 -> JInt(0 - n)
    JInt(_) -> x
    JFloat(n) ->
      case is_negative_float(n) {
        True -> JFloat(0.0 -. n)
        False -> JFloat(n)
      }
    JNan -> JNan
    JPosInf | JNegInf -> JPosInf
  }
}

/// Math.sqrt(x)
fn math_sqrt(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> {
      let n = finite_to_float(x)
      case n <. 0.0 {
        True -> JNan
        False -> JFloat(ffi_math_sqrt(n))
      }
    }
    JNan | JNegInf -> JNan
    JPosInf -> JPosInf
  }
}

/// Which end of the ordering `math_extremum` folds toward.
type Extremum {
  Max
  Min
}

/// Math.max(a, b, ...) — returns -Infinity for no args.
fn math_max(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  math_extremum(args, st, Max)
}

/// Math.min(a, b, ...) — returns +Infinity for no args.
fn math_min(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  math_extremum(args, st, Min)
}

/// Math.max/min fold. The seed (loses to everything), the dominant value
/// (beats everything) and the finite tie-break all come out of ONE `case
/// which` — they must agree, so they cannot be chosen independently.
fn math_extremum(
  args: List(JsVal),
  st: Agent,
  which: Extremum,
) -> #(JsVal, Agent) {
  let #(seed, dominant, keep_acc) = case which {
    // Per spec: Math.max(+0, -0) → +0, so a -0 accumulator loses ties.
    Max -> #(JNegInf, JPosInf, fn(a: Float, b: Float) {
      a >=. b && { a >. b || !rt_val.is_neg_zero(a) }
    })
    // Per spec: Math.min(+0, -0) → -0, so a -0 argument wins ties.
    Min -> #(JPosInf, JNegInf, fn(a: Float, b: Float) {
      a <=. b && { a <. b || !rt_val.is_neg_zero(b) }
    })
  }
  // §21.3.2.24/25 step 1: ? ToNumber every argument (in order) BEFORE the
  // fold — a later argument's valueOf must still run (and may throw) even
  // when an earlier one was already NaN.
  let #(nums, st) = coerce_args(args, st)
  let result =
    list.fold(nums, seed, fn(acc, num) {
      case acc, num {
        JNan, _ | _, JNan -> JNan
        JPosInf, _ | _, JPosInf ->
          case seed {
            JPosInf ->
              case acc == seed {
                True -> num
                False -> acc
              }
            _ -> dominant
          }
        JNegInf, _ | _, JNegInf ->
          case seed {
            JNegInf ->
              case acc == seed {
                True -> num
                False -> acc
              }
            _ -> dominant
          }
        _, _ -> {
          let a = finite_to_float(acc)
          let b = finite_to_float(num)
          case keep_acc(a, b) {
            True -> acc
            False -> num
          }
        }
      }
    })
  #(mk_number(result), st)
}

/// Math.atan(x)
fn math_atan(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> JFloat(ffi_math_atan(finite_to_float(x)))
    JNan -> JNan
    JPosInf -> JFloat(ffi_math_atan2(1.0, 0.0))
    JNegInf -> JFloat(ffi_math_atan2(-1.0, 0.0))
  }
}

/// Math.atan2(y, x)
fn math_atan2(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use y, x <- math_binary(args, st)
  case y, x {
    JNan, _ | _, JNan -> JNan
    JPosInf, JPosInf -> JFloat(ffi_math_atan2(1.0, 1.0))
    JPosInf, JNegInf -> JFloat(ffi_math_atan2(1.0, -1.0))
    JNegInf, JPosInf -> JFloat(ffi_math_atan2(-1.0, 1.0))
    JNegInf, JNegInf -> JFloat(ffi_math_atan2(-1.0, -1.0))
    JPosInf, _ -> JFloat(ffi_math_atan2(1.0, 0.0))
    JNegInf, _ -> JFloat(ffi_math_atan2(-1.0, 0.0))
    // §21.3.2.8 steps 12-15: the result's sign is y's — and a -0 y is
    // NEGATIVE. `yv >=. 0.0` is True for -0.0, so it would hand
    // `Math.atan2(-0, Infinity)` a +0 and `Math.atan2(-0, -Infinity)` a +π.
    _, JPosInf ->
      case is_negative_float(finite_to_float(y)) {
        True -> JFloat(-0.0)
        False -> JFloat(0.0)
      }
    _, JNegInf ->
      case is_negative_float(finite_to_float(y)) {
        True -> JFloat(-3.141592653589793)
        False -> JFloat(3.141592653589793)
      }
    _, _ -> JFloat(ffi_math_atan2(finite_to_float(y), finite_to_float(x)))
  }
}

/// Math.exp(x)
fn math_exp(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    // exp_total already yields JPosInf when e^n overflows a 64-bit float.
    JInt(_) | JFloat(_) -> exp_total(finite_to_float(x))
    JNan -> JNan
    JPosInf -> JPosInf
    JNegInf -> JFloat(0.0)
  }
}

/// Math.random() — a uniform Float in [0, 1) via `HostHooks.random`. Going
/// through the host hook (never `rand:uniform` directly) is what lets the
/// harness seed a deterministic PRNG.
fn math_random(st: Agent) -> #(JsVal, Agent) {
  #(mk_number(JFloat(st.hooks.random())), st)
}

/// Math.sign(x) — returns -1, 0, or 1 (preserving ±0)
fn math_sign(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(n) if n > 0 -> JInt(1)
    JInt(n) if n < 0 -> JInt(-1)
    JInt(_) -> JInt(0)
    JFloat(n) if n >. 0.0 -> JInt(1)
    JFloat(n) if n <. 0.0 -> JInt(-1)
    JFloat(n) -> JFloat(n)
    JNan -> JNan
    JPosInf -> JInt(1)
    JNegInf -> JInt(-1)
  }
}

/// Math.cbrt(x) — cube root
fn math_cbrt(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> {
      let n = finite_to_float(x)
      keep_neg_zero(n, case n <. 0.0 {
        // |n|^(1/3) never overflows for a finite n, but pow_total is
        // total so we get a JsNum back: negate it structurally.
        True -> num_negate(pow_total(float.absolute_value(n), 1.0 /. 3.0))
        False -> pow_total(n, 1.0 /. 3.0)
      })
    }
    JNan -> JNan
    JPosInf -> JPosInf
    JNegInf -> JNegInf
  }
}

/// Math.hypot(a, b, ...) — sqrt(sum of squares). Per spec, ±∞ beats NaN.
fn math_hypot(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  // §21.3.2.18 step 1: ? ToNumber every argument, in order.
  let #(nums, st) = coerce_args(args, st)
  // Classify in one pass: presence of ±∞, of NaN, and the finite magnitudes.
  let #(inf, nan, finites) =
    list.fold(nums, #(False, False, []), fn(acc, n) {
      let #(i, na, vs) = acc
      case n {
        JPosInf | JNegInf -> #(True, na, vs)
        JNan -> #(i, True, vs)
        JInt(_) | JFloat(_) -> #(i, na, [finite_to_float(n), ..vs])
      }
    })
  let result = case inf, nan {
    True, _ -> JPosInf
    _, True -> JNan
    // hypot_total returns JPosInf when the true result overflows a 64-bit
    // float; folding `sum +. v *. v` here instead would badarith (crashing
    // the process, NOT throwing) on inputs as ordinary as
    // `Math.hypot(1e200, 1e200)`, whose true result is finite.
    _, _ -> hypot_total(finites)
  }
  #(mk_number(result), st)
}

/// Math.clz32(x) — count leading zeros in 32-bit integer representation
fn math_clz32(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  // §7.1.7 ToUint32 (NaN/±∞ → 0).
  let n = rt_val.num_to_uint32(x)
  JInt(count_leading_zeros_32(n))
}

/// Math.imul(a, b) — 32-bit integer multiplication
fn math_imul(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use a, b <- math_binary(args, st)
  // §7.1.6 ToInt32 (NaN/±∞ → 0) on each operand.
  let a32 = rt_val.num_to_int32(a)
  let b32 = rt_val.num_to_int32(b)
  // Wrap the exact Int product with wrap_int32. It must NOT be routed
  // through a Float (e.g. `num_to_int32` of `int.to_float(a32 * b32)`): the
  // product can need up to 62 bits, so the low 32 bits — the only ones imul
  // keeps — get rounded away by the f64 conversion whenever |product| > 2^53.
  JInt(rt_val.wrap_int32(a32 * b32))
}

/// Math.expm1(x) — e^x - 1 (more precise for small x)
fn math_expm1(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> {
      let n = finite_to_float(x)
      keep_neg_zero(n, expm1_finite(n))
    }
    JNan -> JNan
    JPosInf -> JPosInf
    JNegInf -> JFloat(-1.0)
  }
}

/// e^n - 1 for a finite non-(-0) n. Erlang's `math` module has no `expm1`;
/// Kahan's correction recovers full precision from `u = e^n`.
fn expm1_finite(n: Float) -> JsNum {
  case exp_total(n) {
    // e^n rounded to exactly 1: n is tiny, and e^n - 1 == n to full precision.
    JFloat(u) if u >=. 1.0 && u <=. 1.0 -> JFloat(n)
    JFloat(u) -> {
      let um1 = u -. 1.0
      case um1 == -1.0 {
        // e^n rounded to exactly 0 (n very negative): the answer is -1.
        True -> JFloat(-1.0)
        // Divide FIRST: `n /. ln(u)` is ~1 (ln(u) is n to within a rounding),
        // so multiplying by `um1` last cannot overflow. `Math.expm1(708)`
        // would badarith on the BEAM in the other order.
        False -> JFloat(um1 *. { n /. ffi_math_log(u) })
      }
    }
    // e^n overflowed a 64-bit float, so e^n - 1 overflows to the same value.
    JPosInf -> JPosInf
    // Unreachable from a finite n, but exp_total is total; pass through.
    other -> other
  }
}

/// Math.log1p(x) — log(1 + x) (more precise for small x)
fn math_log1p(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JNan | JNegInf -> JNan
    JPosInf -> JPosInf
    JInt(_) | JFloat(_) -> {
      let n = finite_to_float(x)
      case n <. -1.0, n == -1.0 {
        True, _ -> JNan
        _, True -> JNegInf
        _, _ -> keep_neg_zero(n, log1p_finite(n))
      }
    }
  }
}

/// ln(1 + n) for a finite n > -1. Kahan's correction — see arc math.gleam.
fn log1p_finite(n: Float) -> JsNum {
  let u = 1.0 +. n
  case u == 1.0 {
    True -> JFloat(n)
    False -> JFloat(ffi_math_log(u) *. { n /. { u -. 1.0 } })
  }
}

/// Math.fround(x) — round to the nearest 32-bit float and widen back.
fn math_fround(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> ffi_fround(finite_to_float(x))
    other -> other
  }
}

/// Math.sinh(x)
fn math_sinh(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> {
      let n = finite_to_float(x)
      keep_neg_zero(n, sinh_total(n))
    }
    other -> other
  }
}

/// Math.cosh(x)
fn math_cosh(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> cosh_total(finite_to_float(x))
    JNan -> JNan
    JPosInf | JNegInf -> JPosInf
  }
}

/// Math.tanh(x)
fn math_tanh(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> {
      let n = finite_to_float(x)
      keep_neg_zero(n, JFloat(ffi_math_tanh(n)))
    }
    JNan -> JNan
    JPosInf -> JFloat(1.0)
    JNegInf -> JFloat(-1.0)
  }
}

/// Math.acosh(x) — domain [1, +Infinity), NaN for < 1
fn math_acosh(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> {
      let n = finite_to_float(x)
      case n <. 1.0 {
        True -> JNan
        False -> JFloat(ffi_math_acosh(n))
      }
    }
    JNan | JNegInf -> JNan
    JPosInf -> JPosInf
  }
}

/// Math.atanh(x) — domain (-1, 1), NaN outside, ±Infinity at ±1
fn math_atanh(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> {
      let n = finite_to_float(x)
      case n <. -1.0 || n >. 1.0, n == -1.0, n == 1.0 {
        True, _, _ -> JNan
        _, True, _ -> JNegInf
        _, _, True -> JPosInf
        _, _, _ -> keep_neg_zero(n, JFloat(ffi_math_atanh(n)))
      }
    }
    _ -> JNan
  }
}

// ============================================================================
// Internal helpers
// ============================================================================

/// Apply a unary JsNum→JsNum function to the first arg (§7.1.4 ToNumber).
fn math_unary(
  args: List(JsVal),
  st: Agent,
  apply: fn(JsNum) -> JsNum,
) -> #(JsVal, Agent) {
  let #(x, st) = rt_val.t_to_number(st, helpers.first_arg_or_undefined(args))
  #(mk_number(apply(x)), st)
}

/// Apply a binary JsNum op to the first two args, coercing each in argument
/// order with §7.1.4 ToNumber.
fn math_binary(
  args: List(JsVal),
  st: Agent,
  apply: fn(JsNum, JsNum) -> JsNum,
) -> #(JsVal, Agent) {
  let #(a_val, b_val) = helpers.two_args_or_undefined(args)
  let #(a, st) = rt_val.t_to_number(st, a_val)
  let #(b, st) = rt_val.t_to_number(st, b_val)
  #(mk_number(apply(a, b)), st)
}

/// §7.1.4 ToNumber over a whole argument list, left to right, threading state.
fn coerce_args(args: List(JsVal), st: Agent) -> #(List(JsNum), Agent) {
  coerce_args_loop(args, st, [])
}

fn coerce_args_loop(
  args: List(JsVal),
  st: Agent,
  acc: List(JsNum),
) -> #(List(JsNum), Agent) {
  case args {
    [] -> #(list.reverse(acc), st)
    [arg, ..rest] -> {
      let #(n, st) = rt_val.t_to_number(st, arg)
      coerce_args_loop(rest, st, [n, ..acc])
    }
  }
}

/// Widen a known-finite JsNum (`JInt|JFloat`) to a Float.
fn finite_to_float(n: JsNum) -> Float {
  case n {
    JInt(i) -> int.to_float(i)
    JFloat(f) -> f
    JNan | JPosInf | JNegInf ->
      panic as "finite_to_float on non-finite JsNum (Math dispatch bug)"
  }
}

/// True iff `n` is IEEE-754 negative — `n < 0` OR `n == -0.0`.
fn is_negative_float(n: Float) -> Bool {
  n <. 0.0 || rt_val.is_neg_zero(n)
}

/// -0 in, -0 out: the odd Math functions must return -0 for a -0 argument.
fn keep_neg_zero(n: Float, result: JsNum) -> JsNum {
  case rt_val.is_neg_zero(n) {
    True -> JFloat(-0.0)
    False -> result
  }
}

/// Like `finite_passthrough` but preserves -0.0 (for asinh).
fn neg_zero_preserving(
  args: List(JsVal),
  st: Agent,
  f: fn(Float) -> Float,
) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> {
      let n = finite_to_float(x)
      keep_neg_zero(n, JFloat(f(n)))
    }
    other -> other
  }
}

/// Rounding op (floor/ceil/round/trunc): a JInt is already its own result;
/// an integral JFloat result in the safe range (not -0) comes back as JInt so
/// `a[Math.floor(x)]` takes the integer index path. Non-finite passes through.
fn rounding_passthrough(
  args: List(JsVal),
  st: Agent,
  f: fn(Float) -> Float,
) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) -> x
    JFloat(n) -> {
      let r = f(n)
      let i = rt_val.float_to_int(r)
      case
        int.to_float(i) == r
        && !rt_val.is_neg_zero(r)
        && i >= 0 - rt_val.max_safe_integer
        && i <= rt_val.max_safe_integer
      {
        True -> JInt(i)
        False -> JFloat(r)
      }
    }
    other -> other
  }
}

/// Unary op where finite n → JFloat(f(n)) and anything else is NaN.
fn finite_or_nan(
  args: List(JsVal),
  st: Agent,
  f: fn(Float) -> Float,
) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> JFloat(f(finite_to_float(x)))
    _ -> JNan
  }
}

/// Unary log-like op: domain [0, ∞), NaN for <0, -∞ at 0, ∞ passes through.
fn log_domain(
  args: List(JsVal),
  st: Agent,
  f: fn(Float) -> Float,
) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JNan | JNegInf -> JNan
    JPosInf -> JPosInf
    JInt(_) | JFloat(_) -> {
      let n = finite_to_float(x)
      // ±0 → -Infinity. Test with `>=. && <=.` NOT a `0.0` literal pattern:
      // on OTP >= 27 that pattern only matches +0.0, so Math.log(-0) would
      // fall through to `math:log(-0.0)` and crash the VM with badarith.
      case n >=. 0.0 && n <=. 0.0, n <. 0.0 {
        True, _ -> JNegInf
        _, True -> JNan
        _, _ -> JFloat(f(n))
      }
    }
  }
}

/// Unary op with domain [-1, 1]: NaN outside, JFloat(f(n)) inside.
fn domain_unit(
  args: List(JsVal),
  st: Agent,
  f: fn(Float) -> Float,
) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> {
      let n = finite_to_float(x)
      case n <. -1.0 || n >. 1.0 {
        True -> JNan
        False -> JFloat(f(n))
      }
    }
    _ -> JNan
  }
}

/// JS Math.round: round half toward +Infinity.
fn js_round(n: Float) -> Float {
  let floored = ffi_math_floor(n)
  let rounded = case n -. floored >=. 0.5 {
    True -> floored +. 1.0
    False -> floored
  }
  case rounded >=. 0.0 && rounded <=. 0.0 && is_negative_float(n) {
    True -> -0.0
    False -> rounded
  }
}

fn js_trunc(n: Float) -> Float {
  case rt_val.is_neg_zero(n) {
    True -> n
    False -> {
      let truncated = int.to_float(rt_val.float_to_int(n))
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

/// §6.1.6.1.3 Number::exponentiate — the shared kernel behind Math.pow.
fn num_exp(base: JsNum, exp: JsNum) -> JsNum {
  case base, exp {
    _, JNan -> JNan
    // Steps 2-3: exponent ±0 → 1, even for NaN base.
    _, JInt(0) -> JInt(1)
    _, JFloat(e) if e >=. 0.0 && e <=. 0.0 -> JInt(1)
    JNan, _ -> JNan
    JPosInf, _ ->
      case exp_is_positive(exp) {
        True -> JPosInf
        False -> JFloat(0.0)
      }
    JNegInf, _ ->
      case exp_is_positive(exp), is_odd_integer(exp) {
        True, True -> JNegInf
        True, False -> JPosInf
        False, True -> JFloat(-0.0)
        False, False -> JFloat(0.0)
      }
    _, JPosInf | _, JNegInf -> {
      let ab = float.absolute_value(finite_to_float(base))
      case ab >. 1.0, ab <. 1.0, exp {
        True, _, JPosInf | _, True, JNegInf -> JPosInf
        _, True, JPosInf | True, _, JNegInf -> JFloat(0.0)
        _, _, _ -> JNan
      }
    }
    // Delegate the finite×finite core to the total FFI wrapper.
    _, _ -> pow_total(finite_to_float(base), finite_to_float(exp))
  }
}

fn exp_is_positive(exp: JsNum) -> Bool {
  case exp {
    JInt(n) -> n > 0
    JFloat(f) -> f >. 0.0
    JPosInf -> True
    JNegInf | JNan -> False
  }
}

fn is_odd_integer(n: JsNum) -> Bool {
  case n {
    JInt(i) -> int.is_odd(i)
    JFloat(f) ->
      case rt_val.integral_int(f) {
        option.Some(i) -> int.is_odd(i)
        option.None -> False
      }
    _ -> False
  }
}

fn num_negate(n: JsNum) -> JsNum {
  case n {
    JNan -> JNan
    JPosInf -> JNegInf
    JNegInf -> JPosInf
    JInt(x) -> JInt(0 - x)
    JFloat(x) -> JFloat(float.negate(x))
  }
}

// ── FFI ─────────────────────────────────────────────────────────────────────
// TOTAL wrappers (arc_rt_math_ffi): every `math` BIF that can OVERFLOW
// a 64-bit float — exp, pow, cosh, sinh — raises `badarith` on the BEAM
// instead of returning ±Infinity. Do not add a raw `@external(erlang, "math",
// ...)` binding for an overflow-capable function; route it through the FFI.

@external(erlang, "arc_rt_math_ffi", "exp")
fn exp_total(x: Float) -> JsNum

@external(erlang, "arc_rt_math_ffi", "pow")
fn pow_total(base: Float, exp: Float) -> JsNum

@external(erlang, "arc_rt_math_ffi", "cosh")
fn cosh_total(x: Float) -> JsNum

@external(erlang, "arc_rt_math_ffi", "sinh")
fn sinh_total(x: Float) -> JsNum

@external(erlang, "arc_rt_math_ffi", "hypot")
fn hypot_total(values: List(Float)) -> JsNum

@external(erlang, "arc_rt_math_ffi", "fround")
fn ffi_fround(x: Float) -> JsNum

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
