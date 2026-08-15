//// `rt_builtins/bigint` — BigInt global function + %BigInt.prototype%
//// (ES2024 §21.2) over the threaded `Agent` model (D7/R1), including
//// `BigInt.asIntN`/`asUintN` (§21.2.2).

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

/// Set up %BigInt.prototype% (§21.2.3) and the BigInt global function
/// (§21.2.1.1). `constructible: False` because `new BigInt` throws.
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
  // §21.2.2.1/.2 static BigInt.asIntN / asUintN.
  let #(static_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("asIntN", BigIntN(BigIntAsIntN), 2),
      #("asUintN", BigIntN(BigIntAsUintN), 2),
    ])
  // §21.2.3.5 %BigInt.prototype%[@@toStringTag] = "BigInt".
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

/// Per-module dispatch for BigInt native functions.
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

// ── §21.2.1.1 BigInt ( value ) ──────────────────────────────────────────────

fn bigint_global(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let arg = helpers.first_arg_or_undefined(args)
  // Step 2: prim = ToPrimitive(value, number).
  let #(prim, st) = rt_val.t_to_primitive(st, arg, HintNumber)
  case classify(prim) {
    // Step 3: Number → NumberToBigInt (RangeError unless integral).
    KNum(JInt(i)) -> #(mk_bigint(i), st)
    KNum(JFloat(f)) ->
      // §4.4.31 IsIntegralNumber via integral_int (±0-safe: BigInt(-0) → 0n).
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
    // Step 4: otherwise ToBigInt(prim).
    _ -> {
      let #(n, st) = rt_val.t_to_bigint(st, prim)
      #(mk_bigint(n), st)
    }
  }
}

// ── §21.2.2 BigInt static methods ───────────────────────────────────────────

/// §21.2.2.1 BigInt.asIntN ( bits, bigint ) — bigint mod 2^bits, signed.
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

/// §21.2.2.2 BigInt.asUintN ( bits, bigint ) — bigint mod 2^bits, unsigned.
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

/// Euclidean modulo: result is always in [0, m) for m > 0. Erlang's `rem`
/// takes the sign of the dividend (`-1 rem 4 = -1`).
fn euclid_mod(n: Int, m: Int) -> Int {
  let r = n % m
  case r < 0 {
    True -> r + m
    False -> r
  }
}

// ── §21.2.3 %BigInt.prototype% methods ──────────────────────────────────────

/// §21.2.3 thisBigIntValue — a BigInt primitive, or a BigInt wrapper object's
/// [[BigIntData]]; anything else → TypeError.
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

/// §21.2.3.3 BigInt.prototype.toString ( [ radix ] ).
fn bigint_proto_to_string(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let n = this_bigint_value(st, this, "toString")
  let radix_arg = helpers.first_arg_or_undefined(args)
  // Steps 2-3: radixMV = ToIntegerOrInfinity(radix); undefined → 10.
  let #(radix, st) = case classify(radix_arg) {
    KUndef -> #(10, st)
    _ -> {
      let #(num, st) = rt_val.t_to_number(st, radix_arg)
      #(rt_val.jsnum_to_integer_or_infinity(num), st)
    }
  }
  // Step 4: 2..36 → BigInt::toString (lowercase digits, no `n` suffix).
  case radix >= 2 && radix <= 36 {
    True -> #(mk_string(format_bigint_radix(n, radix)), st)
    False ->
      rt_val.t_throw_range_error(
        st,
        "toString() radix must be between 2 and 36",
      )
  }
}

/// BigInt::toString(x, radix) — §6.1.6.2.24.
fn format_bigint_radix(n: Int, base: Int) -> String {
  case base {
    10 -> int.to_string(n)
    _ -> {
      let assert Ok(s) = int.to_base_string(n, base)
      string.lowercase(s)
    }
  }
}

/// §21.2.3.2 BigInt.prototype.toLocaleString — no-Intl fallback: decimal
/// rendering; arguments (locales/options, NOT radix) ignored.
fn bigint_proto_to_locale_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let n = this_bigint_value(st, this, "toLocaleString")
  #(mk_string(int.to_string(n)), st)
}

/// §21.2.3.4 BigInt.prototype.valueOf ( ).
fn bigint_proto_value_of(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  #(mk_bigint(this_bigint_value(st, this, "valueOf")), st)
}
