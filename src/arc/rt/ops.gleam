import arc/bytecode/key.{Named}
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsNum, type JsOps, type JsVal, HintDefault,
  HintNumber, JFloat, JInt, JNan, JNegInf, JPosInf, KBig, KBool, KBound, KHandle,
  KNull, KNum, KStr, KSym, KUndef, SObject, StringKey, SymbolKey, classify,
  mk_bigint, mk_number, mk_object, mk_string, symbol_has_instance,
}
import arc/rt/val as rt_val
import gleam/float
import gleam/int
import gleam/option.{None, Some}
import gleam/order
import gleam/string

fn js_ops(st: Agent) -> JsOps(Agent) {
  st.store.ops
}

// §13.10.2 instanceof operator
pub fn t_instance_of(st: Agent, v: JsVal, target: JsVal) -> #(Int, Agent) {
  case classify(target) {
    KHandle(ctor_h) -> {
      let ops = js_ops(st)
      let #(handler, st) =
        ops.get_prop(st, target, SymbolKey(symbol_has_instance))
      case rt_val.is_nullish(handler) {
        True -> {
          let #(callable, st) = rt_val.t_is_callable(st, target)
          case callable {
            True -> t_ordinary_has_instance(st, ctor_h, v)
            False ->
              rt_val.t_throw_type_error(
                st,
                "Right-hand side of instanceof is not callable",
              )
          }
        }
        False -> {
          let #(callable, st) = rt_val.t_is_callable(st, handler)
          case callable {
            True -> {
              let #(res, st) = ops.call(st, handler, target, [v])
              #(bool_int(rt_val.to_boolean(res)), st)
            }
            False ->
              rt_val.t_throw_type_error(
                st,
                "Symbol.hasInstance handler is not callable",
              )
          }
        }
      }
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "Right-hand side of instanceof is not callable",
      )
  }
}

// §7.3.22 steps 2-7, caller checked callable
pub fn t_ordinary_has_instance(
  st: Agent,
  ctor: Handle,
  v: JsVal,
) -> #(Int, Agent) {
  case rt_store.t_cell_get(st, ctor) {
    SObject(kind: KBound(target:, ..), ..) ->
      t_instance_of(st, v, mk_object(target))
    _ ->
      case classify(v) {
        KHandle(obj_h) -> {
          let #(proto_val, st) =
            js_ops(st).get_prop(
              st,
              mk_object(ctor),
              StringKey(Named("prototype")),
            )
          case classify(proto_val) {
            KHandle(proto_h) -> proto_walk(st, obj_h, proto_h, 10_000)
            _ ->
              rt_val.t_throw_type_error(
                st,
                "Function has non-object prototype in instanceof check",
              )
          }
        }
        _ -> #(0, st)
      }
  }
}

// fuel bounds proxy getprototypeof loops
fn proto_walk(
  st: Agent,
  obj: Handle,
  target_proto: Handle,
  fuel: Int,
) -> #(Int, Agent) {
  case fuel <= 0 {
    True -> rt_val.t_throw_range_error(st, "Maximum call stack size exceeded")
    False -> {
      let #(next, st) = rt_obj.t_get_prototype_of(st, obj)
      case next {
        None -> #(0, st)
        Some(proto_h) ->
          case proto_h.id == target_proto.id {
            True -> #(1, st)
            False -> proto_walk(st, proto_h, target_proto, fuel - 1)
          }
      }
    }
  }
}

fn bool_int(b: Bool) -> Int {
  case b {
    True -> 1
    False -> 0
  }
}

type Cmp {
  Lt
  Eq
  Gt
  Undef
}

fn order_to_cmp(o: order.Order) -> Cmp {
  case o {
    order.Lt -> Lt
    order.Eq -> Eq
    order.Gt -> Gt
  }
}

fn cmp_negate(c: Cmp) -> Cmp {
  case c {
    Lt -> Gt
    Gt -> Lt
    Eq -> Eq
    Undef -> Undef
  }
}

fn ncmp(a: JsNum, b: JsNum) -> Cmp {
  case a, b {
    JNan, _ | _, JNan -> Undef
    JPosInf, JPosInf | JNegInf, JNegInf -> Eq
    JPosInf, _ | _, JNegInf -> Gt
    _, JPosInf | JNegInf, _ -> Lt
    JInt(x), JInt(y) -> order_to_cmp(int.compare(x, y))
    JFloat(x), JFloat(y) -> fcmp(x, y)
    JInt(x), JFloat(y) -> fcmp(int.to_float(x), y)
    JFloat(x), JInt(y) -> fcmp(x, int.to_float(y))
  }
}

fn fcmp(a: Float, b: Float) -> Cmp {
  case a <. b {
    True -> Lt
    False ->
      case a >. b {
        True -> Gt
        False -> Eq
      }
  }
}

fn compare_bigint_num(b: Int, n: JsNum) -> Cmp {
  case n {
    JNan -> Undef
    JPosInf -> Lt
    JNegInf -> Gt
    JInt(i) -> order_to_cmp(int.compare(b, i))
    JFloat(f) -> {
      let fl = float.floor(f)
      case int.compare(b, float.truncate(fl)) {
        order.Lt -> Lt
        order.Gt -> Gt
        order.Eq ->
          case fl == f {
            True -> Eq
            False -> Lt
          }
      }
    }
  }
}

// §7.2.13 islessthan, strings compare by utf-8 bytes
fn t_relational_cmp(st: Agent, a: JsVal, b: JsVal) -> #(Cmp, Agent) {
  let #(pa, st) = rt_val.t_to_primitive(st, a, HintNumber)
  let #(pb, st) = rt_val.t_to_primitive(st, b, HintNumber)
  case classify(pa), classify(pb) {
    KStr(sa), KStr(sb) -> #(order_to_cmp(string.compare(sa, sb)), st)
    KBig(x), KStr(sb) -> #(cmp_bigint_str(x, sb), st)
    KStr(sa), KBig(y) -> #(cmp_negate(cmp_bigint_str(y, sa)), st)
    _, _ -> {
      let #(na, st) = rt_val.t_to_numeric(st, pa)
      let #(nb, st) = rt_val.t_to_numeric(st, pb)
      case classify(na), classify(nb) {
        KBig(x), KBig(y) -> #(order_to_cmp(int.compare(x, y)), st)
        KBig(x), KNum(n) -> #(compare_bigint_num(x, n), st)
        KNum(n), KBig(y) -> #(cmp_negate(compare_bigint_num(y, n)), st)
        KNum(x), KNum(y) -> #(ncmp(x, y), st)
        _, _ -> panic as "t_to_numeric returned non-numeric"
      }
    }
  }
}

fn cmp_bigint_str(x: Int, s: String) -> Cmp {
  case rt_val.string_to_bigint(s) {
    Some(y) -> order_to_cmp(int.compare(x, y))
    None -> Undef
  }
}

pub fn t_lt(st: Agent, a: JsVal, b: JsVal) -> #(Int, Agent) {
  let #(c, st) = t_relational_cmp(st, a, b)
  case c {
    Lt -> #(1, st)
    Eq | Gt | Undef -> #(0, st)
  }
}

pub fn t_le(st: Agent, a: JsVal, b: JsVal) -> #(Int, Agent) {
  let #(c, st) = t_relational_cmp(st, a, b)
  case c {
    Lt | Eq -> #(1, st)
    Gt | Undef -> #(0, st)
  }
}

pub fn t_gt(st: Agent, a: JsVal, b: JsVal) -> #(Int, Agent) {
  let #(c, st) = t_relational_cmp(st, a, b)
  case c {
    Gt -> #(1, st)
    Lt | Eq | Undef -> #(0, st)
  }
}

pub fn t_ge(st: Agent, a: JsVal, b: JsVal) -> #(Int, Agent) {
  let #(c, st) = t_relational_cmp(st, a, b)
  case c {
    Gt | Eq -> #(1, st)
    Lt | Undef -> #(0, st)
  }
}

const bigint_mix_error = "Cannot mix BigInt and other types, use explicit conversions"

fn to_numeric_operands(
  st: Agent,
  a: JsVal,
  b: JsVal,
) -> #(JsVal, JsVal, Agent) {
  let #(an, st) = rt_val.t_to_numeric(st, a)
  let #(bn, st) = rt_val.t_to_numeric(st, b)
  #(an, bn, st)
}

fn int32_binop(
  st: Agent,
  a: JsVal,
  b: JsVal,
  big: fn(Int, Int) -> Int,
  op: fn(Int, Int) -> Int,
) -> #(JsVal, Agent) {
  let #(an, bn, st) = to_numeric_operands(st, a, b)
  case classify(an), classify(bn) {
    KBig(x), KBig(y) -> #(mk_bigint(big(x, y)), st)
    KBig(_), _ | _, KBig(_) -> rt_val.t_throw_type_error(st, bigint_mix_error)
    KNum(x), KNum(y) -> {
      let r = op(rt_val.num_to_int32(x), rt_val.num_to_int32(y))
      #(mk_number(JInt(rt_val.wrap_int32(r))), st)
    }
    _, _ -> panic as "ToNumeric returned non-numeric"
  }
}

pub fn t_bitand(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  int32_binop(st, a, b, int.bitwise_and, int.bitwise_and)
}

pub fn t_bitor(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  int32_binop(st, a, b, int.bitwise_or, int.bitwise_or)
}

pub fn t_bitxor(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  int32_binop(st, a, b, int.bitwise_exclusive_or, int.bitwise_exclusive_or)
}

pub fn t_shl(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  int32_binop(st, a, b, int.bitwise_shift_left, fn(x, y) {
    int.bitwise_shift_left(x, int.bitwise_and(y, 31))
  })
}

pub fn t_shr(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  int32_binop(st, a, b, int.bitwise_shift_right, fn(x, y) {
    int.bitwise_shift_right(x, int.bitwise_and(y, 31))
  })
}

pub fn t_ushr(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  let #(an, bn, st) = to_numeric_operands(st, a, b)
  case classify(an), classify(bn) {
    KBig(_), KBig(_) ->
      rt_val.t_throw_type_error(
        st,
        "BigInts have no unsigned right shift, use >> instead",
      )
    KBig(_), _ | _, KBig(_) -> rt_val.t_throw_type_error(st, bigint_mix_error)
    KNum(x), KNum(y) -> {
      let r =
        int.bitwise_shift_right(
          rt_val.num_to_uint32(x),
          int.bitwise_and(rt_val.num_to_uint32(y), 31),
        )
      #(mk_number(JInt(rt_val.wrap_uint32(r))), st)
    }
    _, _ -> panic as "ToNumeric returned non-numeric"
  }
}

pub fn t_bitnot(st: Agent, a: JsVal) -> #(JsVal, Agent) {
  let #(an, st) = rt_val.t_to_numeric(st, a)
  case classify(an) {
    KBig(x) -> #(mk_bigint(-1 - x), st)
    KNum(n) -> #(mk_number(JInt(int.bitwise_not(rt_val.num_to_int32(n)))), st)
    _ -> panic as "ToNumeric returned non-numeric"
  }
}

pub fn strict_eq(a: JsVal, b: JsVal) -> Bool {
  rt_val.strict_equal(a, b)
}

pub fn strict_ne(a: JsVal, b: JsVal) -> Bool {
  case rt_val.strict_equal(a, b) {
    True -> False
    False -> True
  }
}

// §7.2.14 is loosely equal
pub fn t_eq(st: Agent, a: JsVal, b: JsVal) -> #(Int, Agent) {
  case classify(a), classify(b) {
    KUndef, KUndef | KNull, KNull | KNull, KUndef | KUndef, KNull -> #(1, st)
    KBool(_), KBool(_)
    | KNum(_), KNum(_)
    | KStr(_), KStr(_)
    | KBig(_), KBig(_)
    | KSym(_), KSym(_)
    | KHandle(_), KHandle(_)
    -> #(bool_int(rt_val.strict_equal(a, b)), st)
    // bool arms must precede the object arms
    KBool(x), _ -> t_eq(st, mk_number(bool_to_jsnum(x)), b)
    _, KBool(y) -> t_eq(st, a, mk_number(bool_to_jsnum(y)))
    KHandle(_), KNum(_)
    | KHandle(_), KStr(_)
    | KHandle(_), KBig(_)
    | KHandle(_), KSym(_)
    -> {
      let #(ap, st) = rt_val.t_to_primitive(st, a, HintDefault)
      t_eq(st, ap, b)
    }
    KNum(_), KHandle(_)
    | KStr(_), KHandle(_)
    | KBig(_), KHandle(_)
    | KSym(_), KHandle(_)
    -> {
      let #(bp, st) = rt_val.t_to_primitive(st, b, HintDefault)
      t_eq(st, a, bp)
    }
    KBig(x), KStr(s) | KStr(s), KBig(x) ->
      case rt_val.string_to_bigint(s) {
        Some(y) -> #(bool_int(x == y), st)
        None -> #(0, st)
      }
    KBig(x), KNum(n) | KNum(n), KBig(x) -> #(
      bool_int(bigint_equals_number(x, n)),
      st,
    )
    KNum(_), KStr(s) -> #(
      bool_int(rt_val.strict_equal(a, mk_number(rt_val.string_to_number(s)))),
      st,
    )
    KStr(s), KNum(_) -> #(
      bool_int(rt_val.strict_equal(mk_number(rt_val.string_to_number(s)), b)),
      st,
    )
    _, _ -> #(0, st)
  }
}

pub fn t_neq(st: Agent, a: JsVal, b: JsVal) -> #(Int, Agent) {
  let #(r, st) = t_eq(st, a, b)
  #(1 - r, st)
}

fn bigint_equals_number(a: Int, n: JsNum) -> Bool {
  case n {
    JNan | JPosInf | JNegInf -> False
    JInt(i) -> a == i
    JFloat(f) ->
      case rt_val.integral_int(f) {
        Some(i) -> a == i
        None -> False
      }
  }
}

fn bool_to_jsnum(b: Bool) -> JsNum {
  case b {
    True -> JInt(1)
    False -> JInt(0)
  }
}

pub fn t_neg(st: Agent, a: JsVal) -> #(JsVal, Agent) {
  let #(n, st) = rt_val.t_to_numeric(st, a)
  case classify(n) {
    KBig(x) -> #(mk_bigint(0 - x), st)
    KNum(x) -> #(mk_number(num_negate(x)), st)
    _ -> panic as "ToNumeric returned non-numeric"
  }
}

pub fn t_plus(st: Agent, a: JsVal) -> #(JsVal, Agent) {
  let #(n, st) = rt_val.t_to_number(st, a)
  #(mk_number(n), st)
}

pub fn t_in(st: Agent, key: JsVal, obj: JsVal) -> #(Int, Agent) {
  case classify(obj) {
    KHandle(_) -> {
      let #(pk, st) = rt_val.t_to_property_key(st, key)
      let #(found, st) = rt_obj.t_has_prop(st, obj, pk)
      #(bool_int(found), st)
    }
    _ -> {
      let #(tag, st) = rt_val.t_type_of(st, obj)
      rt_val.t_throw_type_error(
        st,
        "Cannot use 'in' operator to search for property in " <> tag,
      )
    }
  }
}

fn zero_aware_sign(n: JsNum) -> Int {
  case n {
    JPosInf -> 1
    JNegInf -> -1
    JNan -> 1
    JInt(i) ->
      case i < 0 {
        True -> -1
        False -> 1
      }
    JFloat(f) ->
      case f <. 0.0 || rt_val.is_neg_zero(f) {
        True -> -1
        False -> 1
      }
  }
}

fn signed_inf(s: Int) -> JsNum {
  case s < 0 {
    True -> JNegInf
    False -> JPosInf
  }
}

fn signed_zero(s: Int) -> JsNum {
  case s < 0 {
    True -> JFloat(float.negate(0.0))
    False -> JFloat(0.0)
  }
}

fn is_zero(n: JsNum) -> Bool {
  case n {
    JInt(0) -> True
    JFloat(f) -> f >=. 0.0 && f <=. 0.0
    JInt(_) | JNan | JPosInf | JNegInf -> False
  }
}

fn finite_to_float(n: JsNum) -> Float {
  case n {
    JInt(i) -> int.to_float(i)
    JFloat(f) -> f
    JNan | JPosInf | JNegInf -> panic as "finite_to_float on non-finite JsNum"
  }
}

// past 2^53 round to nearest double
fn int_result(i: Int) -> JsNum {
  case i > rt_val.max_safe_integer || i < -rt_val.max_safe_integer {
    True -> rt_val.num_from_int(i)
    False -> JInt(i)
  }
}

fn num_negate(n: JsNum) -> JsNum {
  case n {
    JNan -> JNan
    JPosInf -> JNegInf
    JNegInf -> JPosInf
    JInt(0) -> JFloat(-0.0)
    JInt(x) -> JInt(0 - x)
    JFloat(x) -> JFloat(float.negate(x))
  }
}

fn num_add(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    JNan, _ | _, JNan -> JNan
    JPosInf, JNegInf | JNegInf, JPosInf -> JNan
    JPosInf, _ | _, JPosInf -> JPosInf
    JNegInf, _ | _, JNegInf -> JNegInf
    JInt(x), JInt(y) -> int_result(x + y)
    JInt(x), JFloat(y) -> fadd(int.to_float(x), y)
    JFloat(x), JInt(y) -> fadd(x, int.to_float(y))
    JFloat(x), JFloat(y) -> fadd(x, y)
  }
}

@external(erlang, "arc_rt_ops_ffi", "fadd")
fn fadd(a: Float, b: Float) -> JsNum

@external(erlang, "arc_rt_ops_ffi", "fsub")
fn fsub(a: Float, b: Float) -> JsNum

@external(erlang, "arc_rt_ops_ffi", "fmul")
fn fmul(a: Float, b: Float) -> JsNum

@external(erlang, "arc_rt_ops_ffi", "fdiv")
fn fdiv(a: Float, b: Float) -> JsNum

fn num_sub(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    JNan, _ | _, JNan -> JNan
    JPosInf, JPosInf | JNegInf, JNegInf -> JNan
    JPosInf, _ -> JPosInf
    JNegInf, _ -> JNegInf
    _, JPosInf -> JNegInf
    _, JNegInf -> JPosInf
    JInt(x), JInt(y) -> int_result(x - y)
    JInt(x), JFloat(y) -> fsub(int.to_float(x), y)
    JFloat(x), JInt(y) -> fsub(x, int.to_float(y))
    JFloat(x), JFloat(y) -> fsub(x, y)
  }
}

fn inf_times(s: Int, b: JsNum) -> JsNum {
  case b {
    JNan -> JNan
    JPosInf -> signed_inf(s)
    JNegInf -> signed_inf(0 - s)
    JInt(0) -> JNan
    JInt(n) if n < 0 -> signed_inf(0 - s)
    JInt(_) -> signed_inf(s)
    JFloat(f) if f >=. 0.0 && f <=. 0.0 -> JNan
    JFloat(f) if f <. 0.0 -> signed_inf(0 - s)
    JFloat(_) -> signed_inf(s)
  }
}

fn num_mul(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    JNan, _ | _, JNan -> JNan
    JPosInf, _ -> inf_times(1, b)
    JNegInf, _ -> inf_times(-1, b)
    _, JPosInf -> inf_times(1, a)
    _, JNegInf -> inf_times(-1, a)
    JInt(0), JInt(y) if y < 0 -> JFloat(-0.0)
    JInt(x), JInt(0) if x < 0 -> JFloat(-0.0)
    JInt(x), JInt(y) -> int_result(x * y)
    JInt(x), JFloat(y) -> fmul(int.to_float(x), y)
    JFloat(x), JInt(y) -> fmul(x, int.to_float(y))
    JFloat(x), JFloat(y) -> fmul(x, y)
  }
}

fn num_div(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    JNan, _ | _, JNan -> JNan
    JPosInf, JPosInf | JPosInf, JNegInf | JNegInf, JPosInf | JNegInf, JNegInf ->
      JNan
    JPosInf, _ -> signed_inf(zero_aware_sign(b))
    JNegInf, _ -> signed_inf(0 - zero_aware_sign(b))
    _, JPosInf -> signed_zero(zero_aware_sign(a))
    _, JNegInf -> signed_zero(0 - zero_aware_sign(a))
    _, _ ->
      case is_zero(b) {
        True ->
          case is_zero(a) {
            True -> JNan
            False -> signed_inf(zero_aware_sign(a) * zero_aware_sign(b))
          }
        False -> fdiv(finite_to_float(a), finite_to_float(b))
      }
  }
}

@external(erlang, "arc_rt_ops_ffi", "fmod_total")
fn fmod_total(a: Float, b: Float) -> JsNum

fn num_mod(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    JNan, _ | _, JNan -> JNan
    JPosInf, _ | JNegInf, _ -> JNan
    _, JPosInf | _, JNegInf -> a
    JInt(x), JInt(y) if y != 0 ->
      case x % y {
        0 if x < 0 -> JFloat(-0.0)
        r -> JInt(r)
      }
    _, _ ->
      case is_zero(b) {
        True -> JNan
        False -> fmod_total(finite_to_float(a), finite_to_float(b))
      }
  }
}

@external(erlang, "arc_rt_ops_ffi", "pow_total")
fn pow_total(base: Float, exp: Float) -> JsNum

fn is_odd_integer(n: JsNum) -> Bool {
  case n {
    JInt(i) -> int.is_odd(i)
    JFloat(f) ->
      case rt_val.integral_int(f) {
        Some(i) -> int.is_odd(i)
        None -> False
      }
    JNan | JPosInf | JNegInf -> False
  }
}

fn abs_cmp_one(n: JsNum) -> JsNum {
  let af = float.absolute_value(finite_to_float(n))
  case af >. 1.0, af <. 1.0 {
    True, _ -> JPosInf
    _, True -> JFloat(0.0)
    False, False -> JNan
  }
}

fn exp_is_positive(exp: JsNum) -> Bool {
  case exp {
    JInt(e) -> e > 0
    JFloat(e) -> e >. 0.0
    JPosInf -> True
    JNegInf | JNan -> False
  }
}

// §6.1.6.1.3 number exponentiate
fn num_exp(base: JsNum, exp: JsNum) -> JsNum {
  case exp {
    JNan -> JNan
    JInt(0) -> JInt(1)
    JFloat(e) if e >=. 0.0 && e <=. 0.0 -> JInt(1)
    _ ->
      case base {
        JNan -> JNan
        JPosInf ->
          case exp_is_positive(exp) {
            True -> JPosInf
            False -> JFloat(0.0)
          }
        JNegInf ->
          case exp_is_positive(exp), is_odd_integer(exp) {
            True, True -> JNegInf
            True, False -> JPosInf
            False, True -> JFloat(float.negate(0.0))
            False, False -> JFloat(0.0)
          }
        _ ->
          case exp {
            JPosInf -> abs_cmp_one(base)
            JNegInf ->
              case abs_cmp_one(base) {
                JPosInf -> JFloat(0.0)
                JNan -> JNan
                _ -> JPosInf
              }
            _ -> num_exp_finite(base, exp)
          }
      }
  }
}

fn num_exp_finite(base: JsNum, exp: JsNum) -> JsNum {
  let bf = finite_to_float(base)
  let ef = finite_to_float(exp)
  case is_zero(base) {
    True -> {
      let neg0 = rt_val.is_neg_zero(bf)
      case ef >. 0.0, neg0 && is_odd_integer(exp) {
        True, True -> JFloat(float.negate(0.0))
        True, False -> JFloat(0.0)
        False, True -> JNegInf
        False, False -> JPosInf
      }
    }
    False ->
      case bf <. 0.0 {
        True ->
          case rt_val.integral_int(ef) {
            None -> JNan
            Some(_) -> pow_total(bf, ef)
          }
        False -> pow_total(bf, ef)
      }
  }
}

// exp >= 0
fn bigint_pow(base: Int, exp: Int) -> Int {
  bigint_pow_loop(base, exp, 1)
}

fn bigint_pow_loop(base: Int, exp: Int, acc: Int) -> Int {
  case exp {
    0 -> acc
    _ -> {
      let acc = case int.is_odd(exp) {
        True -> acc * base
        False -> acc
      }
      bigint_pow_loop(base * base, exp / 2, acc)
    }
  }
}

pub fn t_add(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  let #(pa, st) = rt_val.t_to_primitive(st, a, HintDefault)
  let #(pb, st) = rt_val.t_to_primitive(st, b, HintDefault)
  case classify(pa), classify(pb) {
    KStr(_), _ | _, KStr(_) -> {
      let #(sa, st) = rt_val.t_to_string(st, pa)
      let #(sb, st) = rt_val.t_to_string(st, pb)
      #(mk_string(sa <> sb), st)
    }
    _, _ -> {
      let #(na, st) = rt_val.t_to_numeric(st, pa)
      let #(nb, st) = rt_val.t_to_numeric(st, pb)
      case classify(na), classify(nb) {
        KBig(x), KBig(y) -> #(mk_bigint(x + y), st)
        KBig(_), _ | _, KBig(_) ->
          rt_val.t_throw_type_error(st, bigint_mix_error)
        KNum(x), KNum(y) -> #(mk_number(num_add(x, y)), st)
        _, _ -> panic as "ToNumeric returned non-numeric"
      }
    }
  }
}

pub fn t_sub(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  let #(na, nb, st) = to_numeric_operands(st, a, b)
  case classify(na), classify(nb) {
    KBig(x), KBig(y) -> #(mk_bigint(x - y), st)
    KBig(_), _ | _, KBig(_) -> rt_val.t_throw_type_error(st, bigint_mix_error)
    KNum(x), KNum(y) -> #(mk_number(num_sub(x, y)), st)
    _, _ -> panic as "ToNumeric returned non-numeric"
  }
}

pub fn t_mul(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  let #(na, nb, st) = to_numeric_operands(st, a, b)
  case classify(na), classify(nb) {
    KBig(x), KBig(y) -> #(mk_bigint(x * y), st)
    KBig(_), _ | _, KBig(_) -> rt_val.t_throw_type_error(st, bigint_mix_error)
    KNum(x), KNum(y) -> #(mk_number(num_mul(x, y)), st)
    _, _ -> panic as "ToNumeric returned non-numeric"
  }
}

pub fn t_div(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  let #(na, nb, st) = to_numeric_operands(st, a, b)
  case classify(na), classify(nb) {
    KBig(_), KBig(0) -> rt_val.t_throw_range_error(st, "Division by zero")
    KBig(x), KBig(y) -> #(mk_bigint(x / y), st)
    KBig(_), _ | _, KBig(_) -> rt_val.t_throw_type_error(st, bigint_mix_error)
    KNum(x), KNum(y) -> #(mk_number(num_div(x, y)), st)
    _, _ -> panic as "ToNumeric returned non-numeric"
  }
}

pub fn t_mod(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  let #(na, nb, st) = to_numeric_operands(st, a, b)
  case classify(na), classify(nb) {
    KBig(_), KBig(0) -> rt_val.t_throw_range_error(st, "Division by zero")
    KBig(x), KBig(y) -> #(mk_bigint(x % y), st)
    KBig(_), _ | _, KBig(_) -> rt_val.t_throw_type_error(st, bigint_mix_error)
    KNum(x), KNum(y) -> #(mk_number(num_mod(x, y)), st)
    _, _ -> panic as "ToNumeric returned non-numeric"
  }
}

pub fn t_pow(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  let #(na, nb, st) = to_numeric_operands(st, a, b)
  case classify(na), classify(nb) {
    KBig(_), KBig(y) if y < 0 ->
      rt_val.t_throw_range_error(st, "Exponent must be non-negative")
    KBig(x), KBig(y) -> #(mk_bigint(bigint_pow(x, y)), st)
    KBig(_), _ | _, KBig(_) -> rt_val.t_throw_type_error(st, bigint_mix_error)
    KNum(x), KNum(y) -> #(mk_number(num_exp(x, y)), st)
    _, _ -> panic as "ToNumeric returned non-numeric"
  }
}
