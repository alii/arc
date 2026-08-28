import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsNum, type JsVal,
  type NumberNative, GlobalIsFinite, GlobalIsNaN, GlobalN, GlobalParseFloat,
  GlobalParseInt, HintNumber, JFloat, JInt, JNan, JNegInf, JPosInf, KBig,
  KHandle, KNum, KUndef, NumberConstructor, NumberIsFinite, NumberIsInteger,
  NumberIsNaN, NumberIsSafeInteger, NumberN, NumberObj,
  NumberPrototypeToExponential, NumberPrototypeToFixed,
  NumberPrototypeToLocaleString, NumberPrototypeToPrecision,
  NumberPrototypeToString, NumberPrototypeValueOf, SObject, classify, mk_bool,
  mk_number, mk_object, mk_string,
} as rt_types
import arc/rt/val as rt_val
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}

pub type NumberBuiltins {
  NumberBuiltins(
    pair: BuiltinPair,
    parse_int: Handle,
    parse_float: Handle,
    is_nan: Handle,
    is_finite: Handle,
  )
}

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(NumberBuiltins, Agent) {
  let #(parse_int_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      GlobalN(GlobalParseInt),
      "parseInt",
      2,
    )
  let #(parse_float_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      GlobalN(GlobalParseFloat),
      "parseFloat",
      1,
    )
  let #(is_nan_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      GlobalN(GlobalIsNaN),
      "isNaN",
      1,
    )
  let #(is_finite_ref, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      GlobalN(GlobalIsFinite),
      "isFinite",
      1,
    )
  let #(static_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("isNaN", NumberN(NumberIsNaN), 1),
      #("isFinite", NumberN(NumberIsFinite), 1),
      #("isInteger", NumberN(NumberIsInteger), 1),
      #("isSafeInteger", NumberN(NumberIsSafeInteger), 1),
    ])
  // number.parseint must be === the global parseint
  let #(pi_p, st) = common.builtin_property(st, mk_object(parse_int_ref))
  let #(pf_p, st) = common.builtin_property(st, mk_object(parse_float_ref))
  let shared_globals = [#("parseInt", pi_p), #("parseFloat", pf_p)]
  let #(constants, st) =
    data_constants(st, [
      #("NaN", JNan),
      #("POSITIVE_INFINITY", JPosInf),
      #("NEGATIVE_INFINITY", JNegInf),
      #("MAX_SAFE_INTEGER", JFloat(9_007_199_254_740_991.0)),
      #("MIN_SAFE_INTEGER", JFloat(-9_007_199_254_740_991.0)),
      #("EPSILON", JFloat(2.220446049250313e-16)),
      #("MAX_VALUE", JFloat(1.7976931348623157e308)),
      #("MIN_VALUE", JFloat(5.0e-324)),
    ])
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("valueOf", NumberN(NumberPrototypeValueOf), 0),
      #("toString", NumberN(NumberPrototypeToString), 1),
      #("toFixed", NumberN(NumberPrototypeToFixed), 1),
      #("toPrecision", NumberN(NumberPrototypeToPrecision), 1),
      #("toExponential", NumberN(NumberPrototypeToExponential), 1),
      #("toLocaleString", NumberN(NumberPrototypeToLocaleString), 0),
    ])
  let ctor_props =
    list.append(constants, list.append(static_methods, shared_globals))
  let #(bt, st) =
    common.init_wrapper_type(
      st,
      object_proto,
      fn_proto,
      proto_methods,
      fn(_) { NumberN(NumberConstructor) },
      "Number",
      1,
      ctor_props,
      proto_kind: NumberObj(value: JInt(0)),
    )
  #(
    NumberBuiltins(
      pair: bt,
      parse_int: parse_int_ref,
      parse_float: parse_float_ref,
      is_nan: is_nan_ref,
      is_finite: is_finite_ref,
    ),
    st,
  )
}

fn data_constants(
  st: Agent,
  specs: List(#(String, JsNum)),
) -> #(List(#(String, rt_types.Property)), Agent) {
  case specs {
    [] -> #([], st)
    [#(name, n), ..rest] -> {
      let #(prop, st) = common.data_prop(st, mk_number(n))
      let #(tail, st) = data_constants(st, rest)
      #([#(name, prop), ..tail], st)
    }
  }
}

pub fn dispatch(
  st: Agent,
  native: NumberNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    NumberConstructor -> call_as_function(st, args)
    NumberIsNaN -> number_is_nan(st, args)
    NumberIsFinite -> number_is_finite(st, args)
    NumberIsInteger -> number_is_integer(st, args)
    NumberIsSafeInteger -> number_is_safe_integer(st, args)
    NumberPrototypeValueOf -> number_value_of(st, this)
    NumberPrototypeToString -> number_to_string(st, this, args)
    NumberPrototypeToFixed -> number_to_fixed(st, this, args)
    NumberPrototypeToPrecision -> number_to_precision(st, this, args)
    NumberPrototypeToExponential -> number_to_exponential(st, this, args)
    NumberPrototypeToLocaleString -> number_to_locale_string(st, this)
  }
}

fn number_to_locale_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let n = this_number_value(st, this, "toLocaleString")
  #(mk_string(rt_val.format_jsnum(n)), st)
}

fn call_as_function(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case args {
    [] -> #(mk_number(JInt(0)), st)
    [val, ..] -> {
      let #(prim, st) = rt_val.t_to_primitive(st, val, HintNumber)
      case classify(prim) {
        KBig(n) -> #(mk_number(rt_val.num_from_int(n)), st)
        _ -> {
          let #(n, st) = rt_val.t_to_number(st, prim)
          #(mk_number(n), st)
        }
      }
    }
  }
}

pub fn js_is_nan(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(num, st) = rt_val.t_to_number(st, helpers.first_arg_or_undefined(args))
  case num {
    JNan -> #(mk_bool(True), st)
    _ -> #(mk_bool(False), st)
  }
}

pub fn js_is_finite(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(num, st) = rt_val.t_to_number(st, helpers.first_arg_or_undefined(args))
  case num {
    JInt(_) | JFloat(_) -> #(mk_bool(True), st)
    _ -> #(mk_bool(False), st)
  }
}

fn number_is_nan(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KNum(JNan) -> #(mk_bool(True), st)
    _ -> #(mk_bool(False), st)
  }
}

fn number_is_finite(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KNum(JInt(_)) | KNum(JFloat(_)) -> #(mk_bool(True), st)
    _ -> #(mk_bool(False), st)
  }
}

fn number_is_integer(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KNum(JInt(_)) -> #(mk_bool(True), st)
    KNum(JFloat(f)) -> #(mk_bool(option.is_some(rt_val.integral_int(f))), st)
    _ -> #(mk_bool(False), st)
  }
}

fn number_is_safe_integer(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let is_safe = fn(i: Int) {
    i >= -9_007_199_254_740_991 && i <= 9_007_199_254_740_991
  }
  case classify(helpers.first_arg_or_undefined(args)) {
    KNum(JInt(i)) -> #(mk_bool(is_safe(i)), st)
    KNum(JFloat(f)) ->
      case rt_val.integral_int(f) {
        Some(i) -> #(mk_bool(is_safe(i)), st)
        None -> #(mk_bool(False), st)
      }
    _ -> #(mk_bool(False), st)
  }
}

fn number_value_of(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  #(mk_number(this_number_value(st, this, "valueOf")), st)
}

fn number_to_string(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let n = this_number_value(st, this, "toString")
  let #(radix, st) = case args {
    [] -> #(10, st)
    [r, ..] ->
      case classify(r) {
        KUndef -> #(10, st)
        _ -> rt_val.t_to_integer_or_infinity(st, r)
      }
  }
  case radix >= 2 && radix <= 36 {
    False ->
      rt_val.t_throw_range_error(
        st,
        "toString() radix must be between 2 and 36",
      )
    True -> #(mk_string(format_number_radix(n, radix)), st)
  }
}

fn number_to_fixed(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let n = this_number_value(st, this, "toFixed")
  let #(f, st) =
    rt_val.t_to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  case f < 0 || f > 100 {
    True ->
      rt_val.t_throw_range_error(
        st,
        "toFixed() digits argument must be between 0 and 100",
      )
    False -> {
      let format = fn(x) {
        case float.absolute_value(x) >=. 1.0e21 {
          True -> rt_val.js_format_float(x)
          False -> format_to_fixed(x, f)
        }
      }
      #(mk_string(format_non_finite(n, format)), st)
    }
  }
}

fn number_to_exponential(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let n = this_number_value(st, this, "toExponential")
  let arg = helpers.first_arg_or_undefined(args)
  case classify(arg) {
    KUndef -> #(mk_string(format_non_finite(n, format_to_exponential_auto)), st)
    _ -> {
      let #(f, st) = rt_val.t_to_integer_or_infinity(st, arg)
      // non-finite check runs before the range check
      case n {
        JInt(_) | JFloat(_) ->
          case f < 0 || f > 100 {
            True ->
              rt_val.t_throw_range_error(
                st,
                "toExponential() argument must be between 0 and 100",
              )
            False -> #(
              mk_string(format_non_finite(n, format_to_exponential(_, f))),
              st,
            )
          }
        JNan | JPosInf | JNegInf -> #(mk_string(rt_val.format_jsnum(n)), st)
      }
    }
  }
}

fn number_to_precision(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let n = this_number_value(st, this, "toPrecision")
  let arg = helpers.first_arg_or_undefined(args)
  case classify(arg) {
    KUndef -> #(mk_string(rt_val.format_jsnum(n)), st)
    _ -> {
      let #(p, st) = rt_val.t_to_integer_or_infinity(st, arg)
      // non-finite check runs before the range check
      case n {
        JInt(_) | JFloat(_) ->
          case p < 1 || p > 100 {
            True ->
              rt_val.t_throw_range_error(
                st,
                "toPrecision() argument must be between 1 and 100",
              )
            False -> #(
              mk_string(format_non_finite(n, format_to_precision(_, p))),
              st,
            )
          }
        JNan | JPosInf | JNegInf -> #(mk_string(rt_val.format_jsnum(n)), st)
      }
    }
  }
}

fn this_number_value(st: Agent, this: JsVal, method: String) -> JsNum {
  case classify(this) {
    KNum(n) -> n
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: NumberObj(value: n), ..) -> n
        _ -> not_a_number(st, method)
      }
    _ -> not_a_number(st, method)
  }
}

fn not_a_number(st: Agent, method: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "Number.prototype." <> method <> " requires that 'this' be a Number",
  )
}

fn format_number_radix(n: JsNum, base: Int) -> String {
  case n, base {
    _, 10 -> rt_val.format_jsnum(n)
    JNan, _ -> "NaN"
    JPosInf, _ -> "Infinity"
    JNegInf, _ -> "-Infinity"
    JInt(i), _ -> format_int_radix(i, base)
    JFloat(f), _ -> format_float_radix(f, base)
  }
}

fn format_non_finite(n: JsNum, f: fn(Float) -> String) -> String {
  case n {
    JNan -> "NaN"
    JPosInf -> "Infinity"
    JNegInf -> "-Infinity"
    JInt(i) -> f(int.to_float(i))
    JFloat(x) -> f(x)
  }
}

@external(erlang, "arc_rt_number_ffi", "format_to_fixed")
fn format_to_fixed(x: Float, digits: Int) -> String

@external(erlang, "arc_rt_number_ffi", "format_to_exponential")
fn format_to_exponential(x: Float, fraction_digits: Int) -> String

@external(erlang, "arc_rt_number_ffi", "format_to_exponential_auto")
fn format_to_exponential_auto(x: Float) -> String

@external(erlang, "arc_rt_number_ffi", "format_to_precision")
fn format_to_precision(x: Float, precision: Int) -> String

@external(erlang, "arc_rt_number_ffi", "format_int_radix")
fn format_int_radix(i: Int, base: Int) -> String

@external(erlang, "arc_rt_number_ffi", "format_float_radix")
fn format_float_radix(x: Float, base: Int) -> String
