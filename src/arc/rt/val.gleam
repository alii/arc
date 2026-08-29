//// §7.1 type conversion and §7.2 comparison

import arc/bytecode/key.{type Key, array_index_of_float}
import arc/rt/name_keys as nk
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type ErrorKind, type Handle, type JsNum, type JsOps, type JsVal,
  type ObjectKey, type SymbolId, type ToPrimHint, HintDefault, HintNumber,
  HintString, JFloat, JInt, JNan, JNegInf, JPosInf, KBig, KBool, KBound,
  KBytecode, KCompiled, KHandle, KNative, KNull, KNum, KStr, KSym, KTdz, KUndef,
  ProxyObj, RangeErr, ReferenceErr, SObject, StringKey, SymbolKey, SyntaxErr,
  TypeErr, classify, mk_number, mk_object, mk_string, symbol_to_primitive,
}
import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

fn require_ops(st: Agent) -> JsOps(Agent) {
  st.store.ops
}

fn t_throw_error(st: Agent, kind: ErrorKind, msg: String) -> a {
  let #(err, st) = require_ops(st).new_error(st, kind, msg)
  rt_store.t_throw(st, err)
}

pub fn t_throw_type_error(st: Agent, msg: String) -> a {
  t_throw_error(st, TypeErr, msg)
}

pub fn t_throw_range_error(st: Agent, msg: String) -> a {
  t_throw_error(st, RangeErr, msg)
}

pub fn t_throw_reference_error(st: Agent, msg: String) -> a {
  t_throw_error(st, ReferenceErr, msg)
}

pub fn t_throw_syntax_error(st: Agent, msg: String) -> a {
  t_throw_error(st, SyntaxErr, msg)
}

// §9.1.1.1.5/6 tdz read throws referenceerror
pub fn t_tdz_check(st: Agent, v: JsVal, name: BitArray) -> Agent {
  case classify(v) {
    KTdz -> {
      let n = bit_array.to_string(name) |> result.unwrap("<name>")
      t_throw_reference_error(
        st,
        "Cannot access '" <> n <> "' before initialization",
      )
    }
    _ -> st
  }
}

// §9.1.1.3.4 this is tdz until super() returns
pub fn t_check_this(st: Agent, v: JsVal) -> Agent {
  case classify(v) {
    KTdz ->
      t_throw_reference_error(
        st,
        "Must call super constructor in derived class before accessing 'this' or returning from derived constructor",
      )
    _ -> st
  }
}

pub fn is_undef(v: JsVal) -> Bool {
  case classify(v) {
    KUndef -> True
    _ -> False
  }
}

pub fn is_null(v: JsVal) -> Bool {
  case classify(v) {
    KNull -> True
    _ -> False
  }
}

pub fn is_nullish(v: JsVal) -> Bool {
  case classify(v) {
    KUndef | KNull -> True
    _ -> False
  }
}

pub fn is_object(v: JsVal) -> Bool {
  case classify(v) {
    KHandle(_) -> True
    _ -> False
  }
}

// §7.1.2 toboolean
pub fn to_boolean(v: JsVal) -> Bool {
  case classify(v) {
    KUndef | KNull | KTdz -> False
    KBool(b) -> b
    KNum(JNan) -> False
    KNum(JInt(n)) -> n != 0
    KNum(JFloat(f)) -> f != 0.0
    KNum(JPosInf) | KNum(JNegInf) -> True
    KStr(s) -> s != ""
    KBig(n) -> n != 0
    KHandle(_) -> True
    KSym(_) -> True
  }
}

pub fn to_boolean_i32(v: JsVal) -> Int {
  case to_boolean(v) {
    True -> 1
    False -> 0
  }
}

pub fn empty_list() -> List(JsVal) {
  []
}

pub fn list_append_one(xs: List(JsVal), x: JsVal) -> List(JsVal) {
  list.append(xs, [x])
}

pub fn float_from_bits(bits: Int) -> JsVal {
  let assert <<f:float-size(64)>> = <<bits:size(64)>>
  mk_number(JFloat(f))
}

pub fn string_concat(a: BitArray, b: BitArray) -> BitArray {
  <<a:bits, b:bits>>
}

pub fn nullish_label(v: JsVal) -> String {
  case classify(v) {
    KNull -> "null"
    _ -> "undefined"
  }
}

// §7.2.3 iscallable, call survives proxy revocation so ignore revoked
pub fn t_is_callable(st: Agent, v: JsVal) -> #(Bool, Agent) {
  case classify(v) {
    KHandle(h) -> #(handle_is_callable(st, h), st)
    _ -> #(False, st)
  }
}

fn handle_is_callable(st: Agent, h: Handle) -> Bool {
  case rt_store.t_cell_get(st, h) {
    SObject(kind: KCompiled(..), ..)
    | SObject(kind: KBytecode(..), ..)
    | SObject(kind: KNative(..), ..)
    | SObject(kind: KBound(..), ..) -> True
    SObject(kind: ProxyObj(target:, ..), ..) -> handle_is_callable(st, target)
    _ -> False
  }
}

// §13.5.3 typeof
pub fn t_type_of(st: Agent, v: JsVal) -> #(String, Agent) {
  case classify(v) {
    KUndef -> #("undefined", st)
    KNull -> #("object", st)
    KBool(_) -> #("boolean", st)
    KNum(_) -> #("number", st)
    KStr(_) -> #("string", st)
    KBig(_) -> #("bigint", st)
    KSym(_) -> #("symbol", st)
    KHandle(h) ->
      case handle_is_callable(st, h) {
        True -> #("function", st)
        False -> #("object", st)
      }
    KTdz -> #("undefined", st)
  }
}

// §7.2.1 requireobjectcoercible
pub fn t_require_object_coercible(st: Agent, v: JsVal) -> #(JsVal, Agent) {
  case classify(v) {
    KNull -> t_throw_type_error(st, "Cannot convert null to object")
    KUndef -> t_throw_type_error(st, "Cannot convert undefined to object")
    _ -> #(v, st)
  }
}

// §7.1.1 toprimitive
pub fn t_to_primitive(
  st: Agent,
  v: JsVal,
  hint: ToPrimHint,
) -> #(JsVal, Agent) {
  case classify(v) {
    KUndef | KNull | KBool(_) | KNum(_) | KStr(_) | KSym(_) | KBig(_) -> #(
      v,
      st,
    )
    KTdz -> panic as "ToPrimitive on the TDZ sentinel"
    KHandle(h) -> {
      let ops = require_ops(st)
      let fast = get_symbol_data(st, v, symbol_to_primitive)
      let #(exotic, st) = case is_miss(fast) {
        True -> ops.get_prop(st, v, SymbolKey(symbol_to_primitive))
        False -> #(fast, st)
      }
      case is_nullish(exotic) {
        True -> t_ordinary_to_primitive(st, h, hint)
        False -> {
          let #(callable, st) = t_is_callable(st, exotic)
          case callable {
            True -> {
              let hint_str = case hint {
                HintString -> "string"
                HintNumber -> "number"
                HintDefault -> "default"
              }
              let #(result, st) = ops.call(st, exotic, v, [mk_string(hint_str)])
              // §7.1.1 step 1.b.iv object result is a typeerror
              case is_object(result) {
                False -> #(result, st)
                True ->
                  t_throw_type_error(
                    st,
                    "Cannot convert object to primitive value",
                  )
              }
            }
            False -> t_throw_type_error(st, "@@toPrimitive is not callable")
          }
        }
      }
    }
  }
}

// data property fast paths ahead of the full get
@external(erlang, "arc_rt_helpers_ffi", "get_symbol_data")
fn get_symbol_data(st: Agent, recv: JsVal, sym: SymbolId) -> JsVal

@external(erlang, "arc_rt_helpers_ffi", "is_miss")
fn is_miss(v: JsVal) -> Bool

@external(erlang, "arc_rt_obj_ffi", "t_get_prop_slow")
fn get_named_data(
  st: Agent,
  recv: JsVal,
  key: Key,
  site: Option(Nil),
) -> #(JsVal, Agent)

// §7.1.1.1 ordinarytoprimitive
pub fn t_ordinary_to_primitive(
  st: Agent,
  h: Handle,
  hint: ToPrimHint,
) -> #(JsVal, Agent) {
  let method_keys = case hint {
    HintString -> [nk.to_string, nk.value_of]
    HintNumber | HintDefault -> [nk.value_of, nk.to_string]
  }
  try_primitive_methods(st, h, method_keys)
}

fn try_primitive_methods(
  st: Agent,
  h: Handle,
  method_keys: List(Key),
) -> #(JsVal, Agent) {
  let receiver = mk_object(h)
  case method_keys {
    [] -> t_throw_type_error(st, "Cannot convert object to primitive value")
    [k, ..rest] -> {
      let ops = require_ops(st)
      let #(method, st) = get_named_data(st, receiver, k, None)
      let #(callable, st) = t_is_callable(st, method)
      case callable {
        True -> {
          let #(result, st) = ops.call(st, method, receiver, [])
          case is_object(result) {
            False -> #(result, st)
            True -> try_primitive_methods(st, h, rest)
          }
        }
        False -> try_primitive_methods(st, h, rest)
      }
    }
  }
}

// §7.2.14 isstrictlyequal
@external(erlang, "arc_rt_val_ffi", "strict_eq")
pub fn strict_equal(left: JsVal, right: JsVal) -> Bool

// §7.2.11 samevalue
pub fn same_value(left: JsVal, right: JsVal) -> Bool {
  case classify(left), classify(right) {
    KNum(JNan), KNum(JNan) -> True
    // erlang =:= distinguishes -0.0 and compares floats exactly
    KNum(JFloat(a)), KNum(JFloat(b)) -> float_same_term(a, b)
    KNum(JInt(a)), KNum(JInt(b)) -> a == b
    KNum(JInt(a)), KNum(JFloat(b)) -> float_same_term(int.to_float(a), b)
    KNum(JFloat(a)), KNum(JInt(b)) -> float_same_term(a, int.to_float(b))
    _, _ -> strict_equal(left, right)
  }
}

// §7.2.12 samevaluezero
@external(erlang, "arc_rt_val_ffi", "same_value_zero")
pub fn same_value_zero(left: JsVal, right: JsVal) -> Bool

@external(erlang, "arc_rt_val_ffi", "float_same_term")
fn float_same_term(a: Float, b: Float) -> Bool

@external(erlang, "arc_rt_val_ffi", "is_neg_zero")
pub fn is_neg_zero(x: Float) -> Bool

pub const max_safe_integer: Int = 9_007_199_254_740_991

pub fn float_to_int(f: Float) -> Int {
  case f <. 0.0 {
    True -> 0 - float.truncate(float.negate(f))
    False -> float.truncate(f)
  }
}

// +. 0.0 normalizes -0.0 before comparing
pub fn integral_int(f: Float) -> Option(Int) {
  let i = float_to_int(f)
  case int.to_float(i) +. 0.0 == f +. 0.0 {
    True -> Some(i)
    False -> None
  }
}

pub fn wrap_uint32(i: Int) -> Int {
  int.bitwise_and(i, 0xFFFFFFFF)
}

// int stays int, a float round trip loses low bits past 2^53
pub fn wrap_int32(i: Int) -> Int {
  let wrapped = wrap_uint32(i)
  case wrapped > 0x7FFFFFFF {
    True -> wrapped - 0x100000000
    False -> wrapped
  }
}

// §7.1.6 toint32 of a number
pub fn num_to_int32(n: JsNum) -> Int {
  case n {
    JNan | JPosInf | JNegInf -> 0
    JInt(i) -> wrap_int32(i)
    JFloat(f) -> wrap_int32(float.truncate(f))
  }
}

// §7.1.7 touint32 of a number
pub fn num_to_uint32(n: JsNum) -> Int {
  case n {
    JNan | JPosInf | JNegInf -> 0
    JInt(i) -> wrap_uint32(i)
    JFloat(f) -> wrap_uint32(float.truncate(f))
  }
}

// §7.1.5, infinities saturate to ±max_safe_integer
pub fn jsnum_to_integer_or_infinity(n: JsNum) -> Int {
  case n {
    JNan -> 0
    JInt(i) -> i
    JFloat(f) -> float_to_int(f)
    JPosInf -> max_safe_integer
    JNegInf -> 0 - max_safe_integer
  }
}

// §7.1.20 tolength
pub fn jsnum_to_length(n: JsNum) -> Int {
  int.clamp(jsnum_to_integer_or_infinity(n), min: 0, max: max_safe_integer)
}

// §6.1.6.1.20 number::tostring
pub fn jsnum_to_string(n: JsNum) -> String {
  case n {
    JNan -> "NaN"
    JPosInf -> "Infinity"
    JNegInf -> "-Infinity"
    JInt(i) -> int.to_string(i)
    JFloat(f) -> js_format_float(f)
  }
}

@external(erlang, "arc_rt_val_ffi", "js_number_to_string")
pub fn js_format_float(f: Float) -> String

pub fn format_jsnum(n: JsNum) -> String {
  jsnum_to_string(n)
}

pub type CoerceError {
  // object input, caller runs toprimitive then retries
  NeedsToPrimitive
  SymbolToNumber
  BigIntToNumber
}

// §7.1.4 tonumber, primitives only
pub fn prim_to_number(v: JsVal) -> Result(JsNum, CoerceError) {
  case classify(v) {
    KNum(n) -> Ok(n)
    KUndef -> Ok(JNan)
    KNull -> Ok(JInt(0))
    KBool(True) -> Ok(JInt(1))
    KBool(False) -> Ok(JInt(0))
    KStr(s) -> Ok(string_to_number(s))
    KBig(_) -> Error(BigIntToNumber)
    KSym(_) -> Error(SymbolToNumber)
    KHandle(_) -> Error(NeedsToPrimitive)
    KTdz -> panic as "ToNumber on TDZ sentinel"
  }
}

// §7.1.17 tostring, primitives only
pub fn prim_to_string(v: JsVal) -> Result(String, CoerceError) {
  case classify(v) {
    KStr(s) -> Ok(s)
    KNum(n) -> Ok(jsnum_to_string(n))
    KBool(True) -> Ok("true")
    KBool(False) -> Ok("false")
    KNull -> Ok("null")
    KUndef -> Ok("undefined")
    KBig(n) -> Ok(int.to_string(n))
    KSym(_) -> Error(SymbolToNumber)
    KHandle(_) -> Error(NeedsToPrimitive)
    KTdz -> panic as "ToString on TDZ sentinel"
  }
}

// §7.1.17 tostring
@external(erlang, "arc_rt_val_ffi", "t_to_string")
pub fn t_to_string(st: Agent, v: JsVal) -> #(String, Agent)

pub fn t_to_string_slow(st: Agent, v: JsVal) -> #(String, Agent) {
  case classify(v) {
    KStr(s) -> #(s, st)
    KNum(n) -> #(jsnum_to_string(n), st)
    KBool(True) -> #("true", st)
    KBool(False) -> #("false", st)
    KNull -> #("null", st)
    KUndef -> #("undefined", st)
    KBig(n) -> #(int.to_string(n), st)
    KSym(_) ->
      t_throw_type_error(st, "Cannot convert a Symbol value to a string")
    // toprimitive never returns an object so this recurs once
    KHandle(_) -> {
      let #(prim, st) = t_to_primitive(st, v, HintString)
      t_to_string(st, prim)
    }
    KTdz -> panic as "ToString on TDZ sentinel"
  }
}

// §7.1.19 topropertykey, symbol check runs after toprimitive
pub fn t_to_property_key(st: Agent, v: JsVal) -> #(ObjectKey, Agent) {
  case classify(v) {
    KHandle(_) -> {
      let #(prim, st) = t_to_primitive(st, v, HintString)
      primitive_to_prop_key(st, prim)
    }
    _ -> primitive_to_prop_key(st, v)
  }
}

// nullish base throws before the key's tostring runs
pub fn t_to_property_key_of(
  st: Agent,
  base: JsVal,
  v: JsVal,
) -> #(ObjectKey, Agent) {
  case classify(base) {
    KNull | KUndef ->
      t_throw_type_error(
        st,
        "Cannot read properties of " <> nullish_label(base),
      )
    _ -> t_to_property_key(st, v)
  }
}

// like t_to_property_key but error(text) instead of naming a new string
pub fn t_find_property_key(
  st: Agent,
  v: JsVal,
) -> #(Result(ObjectKey, String), Agent) {
  let #(prim, st) = case classify(v) {
    KHandle(_) -> t_to_primitive(st, v, HintString)
    _ -> #(v, st)
  }
  let found = fn(st, text) {
    case rt_store.t_find_key(st, text) {
      Some(k) -> #(Ok(StringKey(k)), st)
      None -> #(Error(text), st)
    }
  }
  case classify(prim) {
    KSym(id) -> #(Ok(SymbolKey(id)), st)
    KStr(s) -> found(st, s)
    KNum(JInt(n)) ->
      case key.is_array_index(n) {
        True -> #(Ok(StringKey(key.index(n))), st)
        False -> found(st, int.to_string(n))
      }
    KNum(JFloat(f)) ->
      case array_index_of_float(f) {
        Some(i) -> #(Ok(StringKey(key.index(i))), st)
        None -> found(st, js_format_float(f))
      }
    _ -> {
      let #(s, st) = t_to_string(st, prim)
      found(st, s)
    }
  }
}

// §7.1.19 steps 2-3 for a primitive, allocates the name
fn primitive_to_prop_key(st: Agent, v: JsVal) -> #(ObjectKey, Agent) {
  case classify(v) {
    KSym(id) -> #(SymbolKey(id), st)
    KNum(JInt(n)) -> string_key(rt_store.t_key_of_int(st, n))
    KNum(JFloat(f)) ->
      case array_index_of_float(f) {
        Some(i) -> #(StringKey(key.index(i)), st)
        None -> string_key(rt_store.t_key(st, js_format_float(f)))
      }
    KStr(s) -> string_key(rt_store.t_key(st, s))
    _ -> {
      let #(s, st) = t_to_string(st, v)
      string_key(rt_store.t_key(st, s))
    }
  }
}

fn string_key(pair: #(Int, Agent)) -> #(ObjectKey, Agent) {
  #(StringKey(pair.0), pair.1)
}

// §7.1.4.1.1 stringtonumber
@external(erlang, "arc_rt_val_ffi", "string_to_number")
pub fn string_to_number(s: String) -> JsNum

// §7.1.4.1 strwhitespacechar, not unicode white_space
@external(erlang, "arc_string_ffi", "trim_js_ws")
fn trim_string_ws(s: String) -> String

const nf_two52 = 4_503_599_627_370_496

const nf_two53 = 9_007_199_254_740_992

pub fn int_number(n: Int) -> JsNum {
  case n <= max_safe_integer && n >= -max_safe_integer {
    True -> JInt(n)
    False -> num_from_int(n)
  }
}

// erlang float/1 misrounds past 53 bits so round here
pub fn num_from_int(n: Int) -> JsNum {
  let a = int.absolute_value(n)
  case a < nf_two53 {
    True -> JFloat(int.to_float(n))
    False -> {
      let s = nf_bit_length(a, 0) - 53
      let q0 = int.bitwise_shift_right(a, s)
      let r = a - int.bitwise_shift_left(q0, s)
      let half = int.bitwise_shift_left(1, s - 1)
      let q = case r > half || { r == half && q0 % 2 == 1 } {
        True -> q0 + 1
        False -> q0
      }
      let #(q, s) = case q == nf_two53 {
        True -> #(nf_two52, s + 1)
        False -> #(q, s)
      }
      case 53 + s > 1024 {
        True ->
          case n < 0 {
            True -> JNegInf
            False -> JPosInf
          }
        False -> {
          let f = int.to_float(int.bitwise_shift_left(q, s))
          case n < 0 {
            True -> JFloat(0.0 -. f)
            False -> JFloat(f)
          }
        }
      }
    }
  }
}

fn nf_bit_length(n: Int, acc: Int) -> Int {
  case n == 0 {
    True -> acc
    False -> nf_bit_length(int.bitwise_shift_right(n, 1), acc + 1)
  }
}

// §7.1.14 stringtobigint, none on failure
pub fn string_to_bigint(s: String) -> Option(Int) {
  case trim_string_ws(s) {
    "" -> Some(0)
    "0x" <> rest | "0X" <> rest -> parse_bigint_radix_digits(rest, 16)
    "0o" <> rest | "0O" <> rest -> parse_bigint_radix_digits(rest, 8)
    "0b" <> rest | "0B" <> rest -> parse_bigint_radix_digits(rest, 2)
    t -> int.parse(t) |> option.from_result
  }
}

fn parse_bigint_radix_digits(digits: String, base: Int) -> Option(Int) {
  case digits {
    "-" <> _ | "+" <> _ -> None
    _ -> int.base_parse(digits, base) |> option.from_result
  }
}

// §7.1.4 tonumber
@external(erlang, "arc_rt_val_ffi", "t_to_number")
pub fn t_to_number(st: Agent, v: JsVal) -> #(JsNum, Agent)

pub fn t_to_number_slow(st: Agent, v: JsVal) -> #(JsNum, Agent) {
  case classify(v) {
    KNum(n) -> #(n, st)
    KStr(s) -> #(string_to_number(s), st)
    KBool(True) -> #(JInt(1), st)
    KBool(False) -> #(JInt(0), st)
    KNull -> #(JInt(0), st)
    KUndef -> #(JNan, st)
    KBig(_) -> t_throw_type_error(st, "Cannot convert BigInt to number")
    KSym(_) -> t_throw_type_error(st, "Cannot convert Symbol to number")
    KHandle(_) -> {
      let #(prim, st) = t_to_primitive(st, v, HintNumber)
      t_to_number(st, prim)
    }
    KTdz -> panic as "ToNumber on TDZ sentinel"
  }
}

// §7.1.3 tonumeric
pub fn t_to_numeric(st: Agent, v: JsVal) -> #(JsVal, Agent) {
  case classify(v) {
    KBig(_) -> #(v, st)
    KNum(_) -> #(v, st)
    KStr(s) -> #(mk_number(string_to_number(s)), st)
    KBool(True) -> #(mk_number(JInt(1)), st)
    KBool(False) -> #(mk_number(JInt(0)), st)
    KNull -> #(mk_number(JInt(0)), st)
    KUndef -> #(mk_number(JNan), st)
    KSym(_) -> t_throw_type_error(st, "Cannot convert Symbol to number")
    KHandle(_) -> {
      let #(prim, st) = t_to_primitive(st, v, HintNumber)
      t_to_numeric(st, prim)
    }
    KTdz -> panic as "ToNumeric on TDZ sentinel"
  }
}

// §7.1.13 tobigint
pub fn t_to_bigint(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(prim, st) = t_to_primitive(st, v, HintNumber)
  case classify(prim) {
    KBig(n) -> #(n, st)
    KBool(True) -> #(1, st)
    KBool(False) -> #(0, st)
    KStr(s) ->
      case string_to_bigint(s) {
        Some(n) -> #(n, st)
        // bad string is syntaxerror not typeerror
        None ->
          t_throw_syntax_error(st, "Cannot convert " <> s <> " to a BigInt")
      }
    KNum(_) -> t_throw_type_error(st, "Cannot convert a Number to a BigInt")
    KSym(_) -> t_throw_type_error(st, "Cannot convert a Symbol to a BigInt")
    KNull -> t_throw_type_error(st, "Cannot convert null to a BigInt")
    KUndef -> t_throw_type_error(st, "Cannot convert undefined to a BigInt")
    KHandle(_) | KTdz -> panic as "ToBigInt: ToPrimitive returned non-primitive"
  }
}

// §7.1.18 toobject
pub fn t_to_object(st: Agent, v: JsVal) -> #(Handle, Agent) {
  case classify(v) {
    KHandle(h) -> #(h, st)
    KNull -> t_throw_type_error(st, "Cannot convert null to object")
    KUndef -> t_throw_type_error(st, "Cannot convert undefined to object")
    KTdz -> panic as "ToObject on the TDZ sentinel"
    _ -> require_ops(st).to_object(st, v)
  }
}

// §7.1.6 toint32
pub fn t_to_int32(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = t_to_number(st, v)
  #(num_to_int32(n), st)
}

// §7.1.7 touint32
pub fn t_to_uint32(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = t_to_number(st, v)
  #(num_to_uint32(n), st)
}

// §7.1.5 tointegerorinfinity
@external(erlang, "arc_rt_val_ffi", "t_to_integer_or_infinity")
pub fn t_to_integer_or_infinity(st: Agent, v: JsVal) -> #(Int, Agent)

pub fn t_to_integer_or_infinity_slow(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = t_to_number(st, v)
  #(jsnum_to_integer_or_infinity(n), st)
}

@external(erlang, "arc_rt_val_ffi", "t_to_length")
pub fn t_to_length(st: Agent, v: JsVal) -> #(Int, Agent)

pub fn t_to_length_slow(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = t_to_number(st, v)
  #(jsnum_to_length(n), st)
}

// §7.1.22 toindex, rangeerror outside [0, 2^53-1]
pub fn t_to_index(st: Agent, v: JsVal, err_msg: String) -> #(Int, Agent) {
  case classify(v) {
    KUndef -> #(0, st)
    _ -> {
      let #(num, st) = t_to_number(st, v)
      case num {
        JNan -> #(0, st)
        JPosInf | JNegInf -> t_throw_range_error(st, err_msg)
        JInt(i) ->
          case i < 0 || i > max_safe_integer {
            True -> t_throw_range_error(st, err_msg)
            False -> #(i, st)
          }
        JFloat(f) -> {
          let i = float_to_int(f)
          case i < 0 || i > max_safe_integer {
            True -> t_throw_range_error(st, err_msg)
            False -> #(i, st)
          }
        }
      }
    }
  }
}
