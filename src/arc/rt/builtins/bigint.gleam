import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BigIntNative, type BuiltinPair, type Handle, type JsVal,
  BigIntAsIntN, BigIntAsUintN, BigIntGlobal, BigIntN, BigIntObj,
  BigIntPrototypeToLocaleString, BigIntPrototypeToString, BigIntPrototypeValueOf,
  HintNumber, JFloat, JInt, KBig, KHandle, KNum, KUndef, SObject, classify,
  mk_bigint, mk_string,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/option.{None, Some}
import gleam/string

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("toString", BigIntN(BigIntPrototypeToString), 0),
      #("toLocaleString", BigIntN(BigIntPrototypeToLocaleString), 0),
      #("valueOf", BigIntN(BigIntPrototypeValueOf), 0),
    ])
  let #(static_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("asIntN", BigIntN(BigIntAsIntN), 2),
      #("asUintN", BigIntN(BigIntAsUintN), 2),
    ])
  let #(proto_h, st) =
    common.init_namespace(st, object_proto, "BigInt", proto_methods)
  common.init_type_on(
    st,
    proto_h,
    fn_proto,
    [],
    fn(_) { BigIntN(BigIntGlobal) },
    "BigInt",
    1,
    static_methods,
    False,
  )
}

pub fn dispatch(
  st: Agent,
  native: BigIntNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    BigIntGlobal -> bigint_global(st, args)
    BigIntAsIntN -> bigint_as_int_n(st, args)
    BigIntAsUintN -> bigint_as_uint_n(st, args)
    BigIntPrototypeToString -> bigint_proto_to_string(st, this, args)
    BigIntPrototypeToLocaleString -> bigint_proto_to_locale_string(st, this)
    BigIntPrototypeValueOf -> bigint_proto_value_of(st, this)
  }
}

fn bigint_global(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let arg = helpers.first_arg_or_undefined(args)
  let #(prim, st) = rt_val.t_to_primitive(st, arg, HintNumber)
  case classify(prim) {
    KNum(JInt(i)) -> #(mk_bigint(i), st)
    KNum(JFloat(f)) ->
      case rt_val.integral_int(f) {
        Some(i) -> #(mk_bigint(i), st)
        None ->
          rt_val.t_throw_range_error(
            st,
            "The number "
              <> rt_val.js_format_float(f)
              <> " cannot be converted to a BigInt because it is not an integer",
          )
      }
    KNum(_) ->
      rt_val.t_throw_range_error(
        st,
        "The number cannot be converted to a BigInt because it is not an integer",
      )
    _ -> {
      let #(n, st) = rt_val.t_to_bigint(st, prim)
      #(mk_bigint(n), st)
    }
  }
}

fn bigint_as_int_n(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(bits_v, bigint_v) = helpers.two_args_or_undefined(args)
  let #(bits, st) =
    rt_val.t_to_index(
      st,
      bits_v,
      "Invalid value: not (convertible to) a safe integer",
    )
  let #(n, st) = rt_val.t_to_bigint(st, bigint_v)
  case bits {
    0 -> #(mk_bigint(0), st)
    _ -> {
      let modulus = int.bitwise_shift_left(1, bits)
      let m = euclid_mod(n, modulus)
      let half = int.bitwise_shift_left(1, bits - 1)
      case m >= half {
        True -> #(mk_bigint(m - modulus), st)
        False -> #(mk_bigint(m), st)
      }
    }
  }
}

fn bigint_as_uint_n(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(bits_v, bigint_v) = helpers.two_args_or_undefined(args)
  let #(bits, st) =
    rt_val.t_to_index(
      st,
      bits_v,
      "Invalid value: not (convertible to) a safe integer",
    )
  let #(n, st) = rt_val.t_to_bigint(st, bigint_v)
  case bits {
    0 -> #(mk_bigint(0), st)
    _ -> #(mk_bigint(euclid_mod(n, int.bitwise_shift_left(1, bits))), st)
  }
}

// result in [0, m), erlang rem keeps dividend sign
fn euclid_mod(n: Int, m: Int) -> Int {
  let r = n % m
  case r < 0 {
    True -> r + m
    False -> r
  }
}

fn this_bigint_value(st: Agent, this: JsVal, method: String) -> Int {
  case classify(this) {
    KBig(n) -> n
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: BigIntObj(value: n), ..) -> n
        _ -> not_a_bigint(st, method)
      }
    _ -> not_a_bigint(st, method)
  }
}

fn not_a_bigint(st: Agent, method: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "BigInt.prototype." <> method <> " requires that 'this' be a BigInt",
  )
}

fn bigint_proto_to_string(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let n = this_bigint_value(st, this, "toString")
  let radix_arg = helpers.first_arg_or_undefined(args)
  let #(radix, st) = case classify(radix_arg) {
    KUndef -> #(10, st)
    _ -> {
      let #(num, st) = rt_val.t_to_number(st, radix_arg)
      #(rt_val.jsnum_to_integer_or_infinity(num), st)
    }
  }
  case radix >= 2 && radix <= 36 {
    True -> #(mk_string(format_bigint_radix(n, radix)), st)
    False ->
      rt_val.t_throw_range_error(
        st,
        "toString() radix must be between 2 and 36",
      )
  }
}

fn format_bigint_radix(n: Int, base: Int) -> String {
  case base {
    10 -> int.to_string(n)
    _ -> {
      let assert Ok(s) = int.to_base_string(n, base)
      string.lowercase(s)
    }
  }
}

fn bigint_proto_to_locale_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let n = this_bigint_value(st, this, "toLocaleString")
  #(mk_string(int.to_string(n)), st)
}

fn bigint_proto_value_of(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  #(mk_bigint(this_bigint_value(st, this, "valueOf")), st)
}
