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

@external(erlang, "arc_rt_math_ffi", "fast")
fn fast(native: MathNative, args: List(JsVal)) -> JsVal

@external(erlang, "arc_rt_math_ffi", "is_miss")
fn is_miss(v: JsVal) -> Bool

pub fn dispatch(
  st: Agent,
  native: MathNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let v = fast(native, args)
  case is_miss(v) {
    True -> dispatch_slow(st, native, this, args)
    False -> #(v, st)
  }
}

fn dispatch_slow(
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

fn math_pow(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use a, b <- math_binary(args, st)
  num_exp(a, b)
}

// float.absolute_value gets -0.0 wrong
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

type Extremum {
  Max
  Min
}

fn math_max(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  math_extremum(args, st, Max)
}

fn math_min(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  math_extremum(args, st, Min)
}

fn math_extremum(
  args: List(JsVal),
  st: Agent,
  which: Extremum,
) -> #(JsVal, Agent) {
  let #(seed, dominant, keep_acc) = case which {
    Max -> #(JNegInf, JPosInf, fn(a: Float, b: Float) {
      a >=. b && { a >. b || !rt_val.is_neg_zero(a) }
    })
    Min -> #(JPosInf, JNegInf, fn(a: Float, b: Float) {
      a <=. b && { a <. b || !rt_val.is_neg_zero(b) }
    })
  }
  // tonumber every arg before folding, observable
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

fn math_atan(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> JFloat(ffi_math_atan(finite_to_float(x)))
    JNan -> JNan
    JPosInf -> JFloat(ffi_math_atan2(1.0, 0.0))
    JNegInf -> JFloat(ffi_math_atan2(-1.0, 0.0))
  }
}

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
    // sign follows y and -0 counts as negative
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

fn math_exp(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> exp_total(finite_to_float(x))
    JNan -> JNan
    JPosInf -> JPosInf
    JNegInf -> JFloat(0.0)
  }
}

fn math_random(st: Agent) -> #(JsVal, Agent) {
  #(mk_number(JFloat(st.hooks.random())), st)
}

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

fn math_cbrt(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> {
      let n = finite_to_float(x)
      keep_neg_zero(n, case n <. 0.0 {
        True -> num_negate(pow_total(float.absolute_value(n), 1.0 /. 3.0))
        False -> pow_total(n, 1.0 /. 3.0)
      })
    }
    JNan -> JNan
    JPosInf -> JPosInf
    JNegInf -> JNegInf
  }
}

fn math_hypot(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  let #(nums, st) = coerce_args(args, st)
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
    _, _ -> hypot_total(finites)
  }
  #(mk_number(result), st)
}

fn math_clz32(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  let n = rt_val.num_to_uint32(x)
  JInt(count_leading_zeros_32(n))
}

fn math_imul(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use a, b <- math_binary(args, st)
  let a32 = rt_val.num_to_int32(a)
  let b32 = rt_val.num_to_int32(b)
  // stay in ints, a float would drop low bits
  JInt(rt_val.wrap_int32(a32 * b32))
}

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

// kahan's expm1 correction
fn expm1_finite(n: Float) -> JsNum {
  case exp_total(n) {
    JFloat(u) if u >=. 1.0 && u <=. 1.0 -> JFloat(n)
    JFloat(u) -> {
      let um1 = u -. 1.0
      case um1 == -1.0 {
        True -> JFloat(-1.0)
        // divide first or expm1(708) overflows
        False -> JFloat(um1 *. { n /. ffi_math_log(u) })
      }
    }
    JPosInf -> JPosInf
    other -> other
  }
}

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

fn log1p_finite(n: Float) -> JsNum {
  let u = 1.0 +. n
  case u == 1.0 {
    True -> JFloat(n)
    False -> JFloat(ffi_math_log(u) *. { n /. { u -. 1.0 } })
  }
}

fn math_fround(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> ffi_fround(finite_to_float(x))
    other -> other
  }
}

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

fn math_cosh(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  use x <- math_unary(args, st)
  case x {
    JInt(_) | JFloat(_) -> cosh_total(finite_to_float(x))
    JNan -> JNan
    JPosInf | JNegInf -> JPosInf
  }
}

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

fn math_unary(
  args: List(JsVal),
  st: Agent,
  apply: fn(JsNum) -> JsNum,
) -> #(JsVal, Agent) {
  let #(x, st) = rt_val.t_to_number(st, helpers.first_arg_or_undefined(args))
  #(mk_number(apply(x)), st)
}

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

fn finite_to_float(n: JsNum) -> Float {
  case n {
    JInt(i) -> int.to_float(i)
    JFloat(f) -> f
    JNan | JPosInf | JNegInf ->
      panic as "finite_to_float on non-finite JsNum (Math dispatch bug)"
  }
}

fn is_negative_float(n: Float) -> Bool {
  n <. 0.0 || rt_val.is_neg_zero(n)
}

fn keep_neg_zero(n: Float, result: JsNum) -> JsNum {
  case rt_val.is_neg_zero(n) {
    True -> JFloat(-0.0)
    False -> result
  }
}

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
      // a 0.0 pattern only matches +0.0 on otp 27+
      case n >=. 0.0 && n <=. 0.0, n <. 0.0 {
        True, _ -> JNegInf
        _, True -> JNan
        _, _ -> JFloat(f(n))
      }
    }
  }
}

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

fn num_exp(base: JsNum, exp: JsNum) -> JsNum {
  case base, exp {
    _, JNan -> JNan
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

// overflow-capable math bifs badarith, keep them in the ffi

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
