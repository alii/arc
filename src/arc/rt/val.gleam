// §7.1 type conversion and §7.2 comparison

import arc/bytecode/error_kind.{
  type ErrorKind, type JsError, JsError, RangeError, ReferenceError, SyntaxError,
  TypeError,
}
import arc/bytecode/key.{Index, Named, array_index_of_float}
import arc/rt/limits
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsNum, type JsVal, type ObjectKey, type SymbolId,
  type ToPrimHint, BoundFn, BytecodeFn, CompiledFn, HintDefault, HintNumber,
  HintString, JFloat, JInt, JNan, JNegInf, JPosInf, KBig, KBool, KHandle, KNull,
  KNum, KStr, KSym, KTdz, KUndef, NativeFn, ProxyObj, SObject, StringKey,
  SymbolKey, classify, mk_int, mk_number, mk_object, mk_string,
  symbol_to_primitive,
}
import arc/rt/utf8
import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

// allocates the realm's error object without throwing it
pub fn new_error(st: Agent, kind: ErrorKind, msg: String) -> #(JsVal, Agent) {
  st.store.ops.new_error(st, kind, msg)
}

pub fn throw(st: Agent, error: JsError) -> a {
  let JsError(kind:, message:) = error
  throw_kind(st, kind, message)
}

pub fn or_throw(st: Agent, r: Result(a, JsError)) -> a {
  case r {
    Ok(v) -> v
    Error(e) -> throw(st, e)
  }
}

fn throw_kind(st: Agent, kind: ErrorKind, msg: String) -> a {
  let #(err, st) = st.store.ops.new_error(st, kind, msg)
  rt_store.throw(st, err)
}

pub fn throw_type_error(st: Agent, msg: String) -> a {
  throw_kind(st, TypeError, msg)
}

pub fn throw_range_error(st: Agent, msg: String) -> a {
  throw_kind(st, RangeError, msg)
}

pub fn throw_reference_error(st: Agent, msg: String) -> a {
  throw_kind(st, ReferenceError, msg)
}

pub fn throw_syntax_error(st: Agent, msg: String) -> a {
  throw_kind(st, SyntaxError, msg)
}

// §9.1.1.1.5/6 tdz read throws referenceerror
pub fn tdz_check(st: Agent, v: JsVal, name: BitArray) -> Agent {
  case classify(v) {
    KTdz -> {
      let n = bit_array.to_string(name) |> result.unwrap("<name>")
      throw_reference_error(
        st,
        "Cannot access '" <> n <> "' before initialization",
      )
    }
    _ -> st
  }
}

// §9.1.1.3.4 this is tdz until super() returns
pub fn check_this(st: Agent, v: JsVal) -> Agent {
  case classify(v) {
    KTdz ->
      throw_reference_error(
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

@external(erlang, "arc_rt_val_ffi", "is_nullish")
pub fn is_nullish(v: JsVal) -> Bool

pub fn is_object(v: JsVal) -> Bool {
  case classify(v) {
    KHandle(_) -> True
    _ -> False
  }
}

pub fn handle_of(v: JsVal) -> Option(Handle) {
  case classify(v) {
    KHandle(h) -> Some(h)
    _ -> None
  }
}

// §7.1.2 toboolean
@external(erlang, "arc_rt_val_ffi", "to_boolean")
pub fn to_boolean(v: JsVal) -> Bool

// !v as a js boolean
@external(erlang, "arc_rt_val_ffi", "logical_not")
pub fn logical_not(v: JsVal) -> JsVal

pub fn empty_list() -> List(JsVal) {
  []
}

pub fn list_append_one(xs: List(JsVal), x: JsVal) -> List(JsVal) {
  list.append(xs, [x])
}

pub fn nullish_label(v: JsVal) -> String {
  case classify(v) {
    KNull -> "null"
    _ -> "undefined"
  }
}

// §7.2.3 iscallable, call survives proxy revocation so ignore revoked
pub fn is_callable(st: Agent, v: JsVal) -> Bool {
  case classify(v) {
    KHandle(h) -> handle_is_callable(st, h)
    _ -> False
  }
}

fn handle_is_callable(st: Agent, h: Handle) -> Bool {
  case rt_store.cell_get(st, h) {
    SObject(kind: CompiledFn(..), ..)
    | SObject(kind: BytecodeFn(..), ..)
    | SObject(kind: NativeFn(..), ..)
    | SObject(kind: BoundFn(..), ..) -> True
    SObject(kind: ProxyObj(target:, ..), ..) -> handle_is_callable(st, target)
    _ -> False
  }
}

// §13.5.3 typeof
pub fn type_of(st: Agent, v: JsVal) -> String {
  case classify(v) {
    KUndef -> "undefined"
    KNull -> "object"
    KBool(_) -> "boolean"
    KNum(_) -> "number"
    KStr(_) -> "string"
    KBig(_) -> "bigint"
    KSym(_) -> "symbol"
    KHandle(h) ->
      case handle_is_callable(st, h) {
        True -> "function"
        False -> "object"
      }
    KTdz -> "undefined"
  }
}

// §7.2.1 requireobjectcoercible
pub fn require_object_coercible(st: Agent, v: JsVal) -> JsVal {
  case classify(v) {
    KNull -> throw_type_error(st, "Cannot convert null to object")
    KUndef -> throw_type_error(st, "Cannot convert undefined to object")
    _ -> v
  }
}

// §7.1.1 toprimitive
pub fn to_primitive(st: Agent, v: JsVal, hint: ToPrimHint) -> #(JsVal, Agent) {
  case classify(v) {
    KUndef | KNull | KBool(_) | KNum(_) | KStr(_) | KSym(_) | KBig(_) -> #(
      v,
      st,
    )
    KTdz -> panic as "ToPrimitive on the TDZ sentinel"
    KHandle(h) -> {
      let ops = st.store.ops
      let #(exotic, st) = get_symbol(st, v, symbol_to_primitive)
      case is_nullish(exotic) {
        True -> ordinary_to_primitive(st, h, hint)
        False -> {
          case is_callable(st, exotic) {
            True -> {
              let hint_text = case hint {
                HintString -> "string"
                HintNumber -> "number"
                HintDefault -> "default"
              }
              let #(result, st) =
                ops.call(st, exotic, v, [mk_string(hint_text)])
              // §7.1.1 step 1.b.iv object result is a typeerror
              case is_object(result) {
                False -> #(result, st)
                True ->
                  throw_type_error(
                    st,
                    "Cannot convert object to primitive value",
                  )
              }
            }
            False -> throw_type_error(st, "@@toPrimitive is not callable")
          }
        }
      }
    }
  }
}

// the one test for a kernel's miss answer
@external(erlang, "arc_rt_val_ffi", "is_miss")
pub fn is_miss(v: a) -> Bool

// bound here, not in obj, because obj imports val
@external(erlang, "arc_rt_obj_ffi", "get_symbol_data")
fn get_symbol_data(st: Agent, recv: JsVal, sym: SymbolId) -> JsVal

// site is an aot ic slot, none from gleam
@external(erlang, "arc_rt_obj_ffi", "get_named")
pub fn get_named(
  st: Agent,
  recv: JsVal,
  key: String,
  site: Option(Int),
) -> #(JsVal, Agent)

pub fn get_symbol(st: Agent, recv: JsVal, sym: SymbolId) -> #(JsVal, Agent) {
  let v = get_symbol_data(st, recv, sym)
  case is_miss(v) {
    True -> st.store.ops.get_prop(st, recv, SymbolKey(sym))
    False -> #(v, st)
  }
}

// §7.1.1.1 ordinarytoprimitive
pub fn ordinary_to_primitive(
  st: Agent,
  h: Handle,
  hint: ToPrimHint,
) -> #(JsVal, Agent) {
  let method_names = case hint {
    HintString -> ["toString", "valueOf"]
    HintNumber | HintDefault -> ["valueOf", "toString"]
  }
  call_primitive_methods(st, h, method_names)
}

fn call_primitive_methods(
  st: Agent,
  h: Handle,
  method_names: List(String),
) -> #(JsVal, Agent) {
  let receiver = mk_object(h)
  case method_names {
    [] -> throw_type_error(st, "Cannot convert object to primitive value")
    [name, ..rest] -> {
      let ops = st.store.ops
      let #(method, st) = get_named(st, receiver, name, None)
      case is_callable(st, method) {
        True -> {
          let #(result, st) = ops.call(st, method, receiver, [])
          case is_object(result) {
            False -> #(result, st)
            True -> call_primitive_methods(st, h, rest)
          }
        }
        False -> call_primitive_methods(st, h, rest)
      }
    }
  }
}

// §7.2.14 isstrictlyequal
@external(erlang, "arc_rt_val_ffi", "strict_eq")
pub fn strict_eq(left: JsVal, right: JsVal) -> Bool

// §7.2.11 samevalue
pub fn same_value(left: JsVal, right: JsVal) -> Bool {
  case classify(left), classify(right) {
    KNum(JNan), KNum(JNan) -> True
    // erlang =:= distinguishes -0.0 and compares floats exactly
    KNum(JFloat(a)), KNum(JFloat(b)) -> float_same_term(a, b)
    KNum(JInt(a)), KNum(JInt(b)) -> a == b
    KNum(JInt(a)), KNum(JFloat(b)) -> float_same_term(int.to_float(a), b)
    KNum(JFloat(a)), KNum(JInt(b)) -> float_same_term(a, int.to_float(b))
    _, _ -> strict_eq(left, right)
  }
}

// §7.2.12 samevaluezero
@external(erlang, "arc_rt_val_ffi", "same_value_zero")
pub fn same_value_zero(left: JsVal, right: JsVal) -> Bool

@external(erlang, "arc_rt_val_ffi", "float_same_term")
fn float_same_term(a: Float, b: Float) -> Bool

@external(erlang, "arc_rt_val_ffi", "is_neg_zero")
pub fn is_neg_zero(x: Float) -> Bool

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
    JPosInf -> limits.max_safe_integer
    JNegInf -> 0 - limits.max_safe_integer
  }
}

// §7.1.20 tolength
pub fn jsnum_to_length(n: JsNum) -> Int {
  int.clamp(
    jsnum_to_integer_or_infinity(n),
    min: 0,
    max: limits.max_safe_integer,
  )
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

@external(erlang, "arc_rt_val_ffi", "js_format_float")
pub fn js_format_float(f: Float) -> String

pub type CoerceError {
  // object input, caller runs toprimitive then retries
  NeedsToPrimitive
  SymbolNotCoercible
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
    KSym(_) -> Error(SymbolNotCoercible)
    KHandle(_) -> Error(NeedsToPrimitive)
    KTdz -> panic as "ToNumber on TDZ sentinel"
  }
}

// §7.1.17 tostring
@external(erlang, "arc_rt_val_ffi", "to_string")
pub fn to_string(st: Agent, v: JsVal) -> #(String, Agent)

// called by name from arc_rt_val_ffi
pub fn to_string_general(st: Agent, v: JsVal) -> #(String, Agent) {
  case classify(v) {
    KStr(s) -> #(s, st)
    KNum(n) -> #(jsnum_to_string(n), st)
    KBool(True) -> #("true", st)
    KBool(False) -> #("false", st)
    KNull -> #("null", st)
    KUndef -> #("undefined", st)
    KBig(n) -> #(int.to_string(n), st)
    KSym(_) -> throw_type_error(st, "Cannot convert a Symbol value to a string")
    // toprimitive never returns an object so this recurs once
    KHandle(_) -> {
      let #(prim, st) = to_primitive(st, v, HintString)
      to_string(st, prim)
    }
    KTdz -> panic as "ToString on TDZ sentinel"
  }
}

// §7.1.19 topropertykey, symbol check runs after toprimitive
pub fn to_property_key(st: Agent, v: JsVal) -> #(ObjectKey, Agent) {
  case classify(v) {
    KHandle(_) -> {
      let #(prim, st) = to_primitive(st, v, HintString)
      primitive_to_prop_key(st, prim)
    }
    _ -> primitive_to_prop_key(st, v)
  }
}

// nullish base throws before the key's tostring runs
pub fn to_property_key_of(
  st: Agent,
  base: JsVal,
  v: JsVal,
) -> #(ObjectKey, Agent) {
  case classify(base) {
    KNull | KUndef ->
      throw_type_error(st, "Cannot read properties of " <> nullish_label(base))
    _ -> to_property_key(st, v)
  }
}

// §7.1.19 steps 2-3 for a primitive
fn primitive_to_prop_key(st: Agent, v: JsVal) -> #(ObjectKey, Agent) {
  case classify(v) {
    KSym(id) -> #(SymbolKey(id), st)
    KNum(JInt(n)) -> #(StringKey(key.index(n)), st)
    KNum(JFloat(f)) ->
      case array_index_of_float(f) {
        Some(i) -> #(StringKey(Index(i)), st)
        None -> #(StringKey(Named(js_format_float(f))), st)
      }
    KNum(JNan) -> #(StringKey(Named("NaN")), st)
    KNum(JPosInf) -> #(StringKey(Named("Infinity")), st)
    KNum(JNegInf) -> #(StringKey(Named("-Infinity")), st)
    KStr(s) -> #(StringKey(key.canonical(s)), st)
    _ -> {
      let #(s, st) = to_string(st, v)
      #(StringKey(key.canonical(s)), st)
    }
  }
}

// §7.1.4.1.1 stringtonumber; called by name from arc_rt_json_ffi
@external(erlang, "arc_rt_val_ffi", "string_to_number")
pub fn string_to_number(s: String) -> JsNum

const two_pow_52 = 4_503_599_627_370_496

const two_pow_53 = 9_007_199_254_740_992

// called by name from arc_rt_val_ffi
pub fn int_number(n: Int) -> JsNum {
  case n <= limits.max_safe_integer && n >= -limits.max_safe_integer {
    True -> JInt(n)
    False -> num_from_int(n)
  }
}

// float/1 misrounds past 53 bits; called by name from arc_rt_val_ffi
pub fn num_from_int(n: Int) -> JsNum {
  let a = int.absolute_value(n)
  case a < two_pow_53 {
    True -> JFloat(int.to_float(n))
    False -> {
      let s = bit_length(a, 0) - 53
      let q0 = int.bitwise_shift_right(a, s)
      let r = a - int.bitwise_shift_left(q0, s)
      let half = int.bitwise_shift_left(1, s - 1)
      let q = case r > half || { r == half && q0 % 2 == 1 } {
        True -> q0 + 1
        False -> q0
      }
      let #(q, s) = case q == two_pow_53 {
        True -> #(two_pow_52, s + 1)
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

fn bit_length(n: Int, acc: Int) -> Int {
  case n == 0 {
    True -> acc
    False -> bit_length(int.bitwise_shift_right(n, 1), acc + 1)
  }
}

// §7.1.14 stringtobigint, none on failure
pub fn string_to_bigint(s: String) -> Option(Int) {
  case utf8.trim_js_ws(s) {
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
@external(erlang, "arc_rt_val_ffi", "to_number")
pub fn to_number(st: Agent, v: JsVal) -> #(JsNum, Agent)

// called by name from arc_rt_val_ffi
pub fn to_number_general(st: Agent, v: JsVal) -> #(JsNum, Agent) {
  case classify(v) {
    KNum(n) -> #(n, st)
    KStr(s) -> #(string_to_number(s), st)
    KBool(True) -> #(JInt(1), st)
    KBool(False) -> #(JInt(0), st)
    KNull -> #(JInt(0), st)
    KUndef -> #(JNan, st)
    KBig(_) -> throw_type_error(st, "Cannot convert BigInt to number")
    KSym(_) -> throw_type_error(st, "Cannot convert Symbol to number")
    KHandle(_) -> {
      let #(prim, st) = to_primitive(st, v, HintNumber)
      to_number(st, prim)
    }
    KTdz -> panic as "ToNumber on TDZ sentinel"
  }
}

// §7.1.3 tonumeric
pub fn to_numeric(st: Agent, v: JsVal) -> #(JsVal, Agent) {
  case classify(v) {
    KBig(_) -> #(v, st)
    KNum(_) -> #(v, st)
    KStr(s) -> #(mk_number(string_to_number(s)), st)
    KBool(True) -> #(mk_int(1), st)
    KBool(False) -> #(mk_int(0), st)
    KNull -> #(mk_int(0), st)
    KUndef -> #(mk_number(JNan), st)
    KSym(_) -> throw_type_error(st, "Cannot convert Symbol to number")
    KHandle(_) -> {
      let #(prim, st) = to_primitive(st, v, HintNumber)
      to_numeric(st, prim)
    }
    KTdz -> panic as "ToNumeric on TDZ sentinel"
  }
}

// §7.1.13 tobigint
pub fn to_bigint(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(prim, st) = to_primitive(st, v, HintNumber)
  case classify(prim) {
    KBig(n) -> #(n, st)
    KBool(True) -> #(1, st)
    KBool(False) -> #(0, st)
    KStr(s) ->
      case string_to_bigint(s) {
        Some(n) -> #(n, st)
        // bad string is syntaxerror not typeerror
        None -> throw_syntax_error(st, "Cannot convert " <> s <> " to a BigInt")
      }
    KNum(_) -> throw_type_error(st, "Cannot convert a Number to a BigInt")
    KSym(_) -> throw_type_error(st, "Cannot convert a Symbol to a BigInt")
    KNull -> throw_type_error(st, "Cannot convert null to a BigInt")
    KUndef -> throw_type_error(st, "Cannot convert undefined to a BigInt")
    KHandle(_) | KTdz -> panic as "ToBigInt: ToPrimitive returned non-primitive"
  }
}

// §7.1.18 toobject
pub fn to_object(st: Agent, v: JsVal) -> #(Handle, Agent) {
  case classify(v) {
    KHandle(h) -> #(h, st)
    KNull -> throw_type_error(st, "Cannot convert null to object")
    KUndef -> throw_type_error(st, "Cannot convert undefined to object")
    KTdz -> panic as "ToObject on the TDZ sentinel"
    _ -> st.store.ops.to_object(st, v)
  }
}

// §7.1.6 toint32
pub fn to_int32(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = to_number(st, v)
  #(num_to_int32(n), st)
}

// §7.1.7 touint32
pub fn to_uint32(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = to_number(st, v)
  #(num_to_uint32(n), st)
}

// §7.1.5 tointegerorinfinity
@external(erlang, "arc_rt_val_ffi", "to_integer_or_infinity")
pub fn to_integer_or_infinity(st: Agent, v: JsVal) -> #(Int, Agent)

// called by name from arc_rt_val_ffi
pub fn to_integer_or_infinity_general(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = to_number(st, v)
  #(jsnum_to_integer_or_infinity(n), st)
}

@external(erlang, "arc_rt_val_ffi", "to_length")
pub fn to_length(st: Agent, v: JsVal) -> #(Int, Agent)

// called by name from arc_rt_val_ffi
pub fn to_length_general(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = to_number(st, v)
  #(jsnum_to_length(n), st)
}

// §7.1.22 toindex, rangeerror outside [0, 2^53-1]
pub fn to_index(st: Agent, v: JsVal, err_msg: String) -> #(Int, Agent) {
  case classify(v) {
    KUndef -> #(0, st)
    _ -> {
      let #(num, st) = to_number(st, v)
      case num {
        JNan -> #(0, st)
        JPosInf | JNegInf -> throw_range_error(st, err_msg)
        JInt(i) ->
          case i < 0 || i > limits.max_safe_integer {
            True -> throw_range_error(st, err_msg)
            False -> #(i, st)
          }
        JFloat(f) -> {
          let i = float_to_int(f)
          case i < 0 || i > limits.max_safe_integer {
            True -> throw_range_error(st, err_msg)
            False -> #(i, st)
          }
        }
      }
    }
  }
}
