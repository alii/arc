//// `rt_val` — ES2024 §7.1 value predicates + abstract-op coercions
//// (SPEC §7.M3, D16, D17, R1).
////
//// Every threaded op returns `#(V, Agent)` — value FIRST (R1).
//// `ToPrimitive`'s object case reaches rt_obj/rt_call ONLY through
//// `store.ops.get_prop` / `.call` (D17 upcall — NO direct import cycle).
//// arc's `Result(_, #(err, st))` error channel becomes 2core's diverging
//// `t_throw_*` (D7), so threaded fns return a bare tuple.

import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type ErrorKind, type Handle, type JsNum, type JsOps, type JsVal,
  type ObjectKey, type ToPrimHint, HintDefault, HintNumber, HintString, Index,
  JFloat, JInt, JNan, JNegInf, JPosInf, KBig, KBool, KBound, KBytecode,
  KCompiled, KHandle, KNative, KNull, KNum, KStr, KSym, KTdz, KUndef, Named,
  ProxyObj, RangeErr, ReferenceErr, SObject, StringKey, SymbolKey, SyntaxErr,
  TypeErr, array_index_of_float, canonical_key, classify, index_key, mk_number,
  mk_object, mk_string, symbol_to_primitive,
}
import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

// ── D17 upcall plumbing + error throwers (module-skeleton-errors) ───────────

/// The seeded `JsOps` upcall table (D17).
fn require_ops(st: Agent) -> JsOps(Agent) {
  st.store.ops
}

fn t_throw_error(st: Agent, kind: ErrorKind, msg: String) -> a {
  let #(err, st) = require_ops(st).new_error(st, kind, msg)
  rt_store.t_throw(st, err)
}

/// Allocate a native `TypeError` with `msg` and throw it (diverges — D7).
pub fn t_throw_type_error(st: Agent, msg: String) -> a {
  t_throw_error(st, TypeErr, msg)
}

/// Allocate a native `RangeError` with `msg` and throw it (diverges — D7).
pub fn t_throw_range_error(st: Agent, msg: String) -> a {
  t_throw_error(st, RangeErr, msg)
}

/// Allocate a native `ReferenceError` with `msg` and throw it (diverges — D7).
pub fn t_throw_reference_error(st: Agent, msg: String) -> a {
  t_throw_error(st, ReferenceErr, msg)
}

/// Allocate a native `SyntaxError` with `msg` and throw it (diverges — D7).
pub fn t_throw_syntax_error(st: Agent, msg: String) -> a {
  t_throw_error(st, SyntaxErr, msg)
}

/// SPEC§8 `tdz_check` — §9.1.1.1.5 step 5. If `v` is the TDZ sentinel, throw
/// `ReferenceError: <name> is not defined`; else return `st` unchanged. arc's
/// M12 emits this before every checked lexical write (`write_slot_checked`).
pub fn t_tdz_check(st: Agent, v: JsVal, name: BitArray) -> Agent {
  case classify(v) {
    KTdz -> {
      let n = bit_array.to_string(name) |> result.unwrap("<name>")
      t_throw_reference_error(st, n <> " is not defined")
    }
    _ -> st
  }
}

// ── pure predicates (no state) ──────────────────────────────────────────────

/// `v` is `undefined`. ES2024 §6.1.1 — the Undefined type's sole value.
pub fn is_undef(v: JsVal) -> Bool {
  case classify(v) {
    KUndef -> True
    _ -> False
  }
}

/// `v` is `null`. ES2024 §6.1.2 — the Null type's sole value.
pub fn is_null(v: JsVal) -> Bool {
  case classify(v) {
    KNull -> True
    _ -> False
  }
}

/// `v` is `undefined` OR `null` — ES2024 §7.2.1 GetMethod's "if func is
/// either undefined or null" test.
pub fn is_nullish(v: JsVal) -> Bool {
  case classify(v) {
    KUndef | KNull -> True
    _ -> False
  }
}

/// `v` is an Object (has a heap cell). ES2024 §7.1.1 step 2.d's
/// "if result is not an Object" is `!is_object(result)`.
pub fn is_object(v: JsVal) -> Bool {
  case classify(v) {
    KHandle(_) -> True
    _ -> False
  }
}

/// ES2024 §7.1.2 ToBoolean(argument). Pure — never observes user code.
/// Port of arc `value.gleam` `is_truthy` with the `Finite(Float)` →
/// `JInt|JFloat` split.
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

/// SPEC§8 op-table `truthy` — `to_boolean` as `Int` `1|0` so it drops
/// straight into `ir.If`'s `Int`-condition slot without a compare.
pub fn to_boolean_i32(v: JsVal) -> Int {
  case to_boolean(v) {
    True -> 1
    False -> 0
  }
}

/// SPEC§8 op-table `empty_list` — the `[]` value arc's `anf.cons_list` folds
/// onto (there is no `ir.ConstNil`). Typed `List(JsVal)` so `MakeCons` chains
/// unify; call sites treat it as an opaque `TTerm`.
pub fn empty_list() -> List(JsVal) {
  []
}

/// SPEC§8 op-table `list_append_one` — `[x, ..xs] |> reverse` avoidance for
/// arc's left-to-right args accumulation. Pure list op; no state.
pub fn list_append_one(xs: List(JsVal), x: JsVal) -> List(JsVal) {
  list.append(xs, [x])
}

/// SPEC§8 op-table `float_lit` — reconstruct a `JsVal` number from its raw
/// IEEE-754 binary64 bit pattern (arc's `expr.number_literal` non-smi path,
/// D5: `-0.0` and denormals stay bit-exact).
pub fn float_from_bits(bits: Int) -> JsVal {
  let assert <<f:float-size(64)>> = <<bits:size(64)>>
  mk_number(JFloat(f))
}

/// SPEC§8 op-table `string_concat` — pure string concat for arc's
/// TemplateLiteral / `+` fast path when both sides are already strings.
pub fn string_concat(a: BitArray, b: BitArray) -> BitArray {
  <<a:bits, b:bits>>
}

/// `"null"` if `v` is null, else `"undefined"` — for TypeError messages that
/// name which nullish value was rejected (ES2024 §7.2.1 note). Port of arc
/// `value.gleam` `nullish_label`.
pub fn nullish_label(v: JsVal) -> String {
  case classify(v) {
    KNull -> "null"
    _ -> "undefined"
  }
}

// ── §7.2.3 IsCallable (threaded — reads the heap) ───────────────────────────

/// ES2024 §7.2.3 IsCallable(argument). An Object is callable iff its
/// `ObjKind` carries a [[Call]] slot: `KCompiled | KNative | KBound`, or a
/// `ProxyObj` whose target is callable. 2core's `ProxyObj` stores no cached
/// `callable` flag, so recurse on `target` (§10.5.14 step 9.a) — [[Call]]
/// survives revocation, so do NOT gate on `revoked`.
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

// ── §13.5.3 typeof / §7.2.1 RequireObjectCoercible ──────────────────────────

/// ES2024 §13.5.3 the `typeof` operator. Objects with a [[Call]] internal
/// method are `"function"`; every other object is `"object"`. R1: `#(V, st)`.
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
    // arc operators.gleam:606 — TDZ sentinel maps to "undefined" (compiler
    // may emit typeof on an uninitialized binding as a defensive measure).
    KTdz -> #("undefined", st)
  }
}

/// ES2024 §7.2.1 RequireObjectCoercible(argument). null / undefined throw a
/// TypeError; every other value passes through. R1: `#(V, st)`.
pub fn t_require_object_coercible(st: Agent, v: JsVal) -> #(JsVal, Agent) {
  case classify(v) {
    KNull -> t_throw_type_error(st, "Cannot convert null to object")
    KUndef -> t_throw_type_error(st, "Cannot convert undefined to object")
    _ -> #(v, st)
  }
}

// ── §7.1.1 ToPrimitive (D17 upcall core) ────────────────────────────────────

/// ES2024 §7.1.1 ToPrimitive(input, preferredType). Primitives pass through;
/// objects call `@@toPrimitive` if present (via `ops.get_prop`/`ops.call` —
/// D17), else fall back to `OrdinaryToPrimitive`. An object result from a
/// user `@@toPrimitive` is a TypeError (§7.1.1 step 1.b.iv). Port of arc
/// `coerce.gleam:47-106` with the D7 Result→diverging-throw rewrite.
pub fn t_to_primitive(
  st: Agent,
  v: JsVal,
  hint: ToPrimHint,
) -> #(JsVal, Agent) {
  case classify(v) {
    // Primitives pass through as-is.
    KUndef | KNull | KBool(_) | KNum(_) | KStr(_) | KSym(_) | KBig(_) -> #(
      v,
      st,
    )
    // The TDZ sentinel is not a JS value; every TDZ load throws
    // ReferenceError before reaching an operand — arriving here is an
    // engine bug (a leaked hole), not a recoverable error.
    KTdz -> panic as "ToPrimitive on the TDZ sentinel"
    // Objects: try @@toPrimitive, then OrdinaryToPrimitive.
    KHandle(h) -> {
      let ops = require_ops(st)
      // §7.1.1 step 1.a: exoticToPrim ← GetMethod(input, @@toPrimitive).
      let #(exotic, st) = ops.get_prop(st, v, SymbolKey(symbol_to_primitive))
      case is_nullish(exotic) {
        // GetMethod treats undefined AND null as "not found" → ordinary.
        True -> ordinary_to_primitive(st, h, hint)
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
              // §7.1.1 step 1.b.iv: an object result is a TypeError.
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

/// ES2024 §7.1.1.1 OrdinaryToPrimitive(O, hint). Tries `toString`/`valueOf`
/// (or `valueOf`/`toString` for a number/default hint); returns the first
/// non-object result, else TypeError. Port of arc `coerce.gleam:120-166`.
fn ordinary_to_primitive(
  st: Agent,
  h: Handle,
  hint: ToPrimHint,
) -> #(JsVal, Agent) {
  let method_names = case hint {
    HintString -> ["toString", "valueOf"]
    HintNumber | HintDefault -> ["valueOf", "toString"]
  }
  try_primitive_methods(st, h, method_names)
}

/// Try each method name in order; return the first primitive result. §7.1.1.1's
/// `O` is an Object, so the receiver for both the [[Get]] and the call is
/// `mk_object(h)` — a caller cannot pass a `this` that isn't the object
/// being coerced.
fn try_primitive_methods(
  st: Agent,
  h: Handle,
  method_names: List(String),
) -> #(JsVal, Agent) {
  let receiver = mk_object(h)
  case method_names {
    [] -> t_throw_type_error(st, "Cannot convert object to primitive value")
    [name, ..rest] -> {
      let ops = require_ops(st)
      let #(method, st) = ops.get_prop(st, receiver, StringKey(Named(name)))
      let #(callable, st) = t_is_callable(st, method)
      case callable {
        True -> {
          let #(result, st) = ops.call(st, method, receiver, [])
          // §7.1.1.1 step 5.b.iii: a non-primitive result → next method.
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

// ── §7.2 Testing and Comparison Operations: equality ────────────────────────

/// ES2024 §7.2.14 IsStrictlyEqual (JS `===`). NaN !== NaN; +0 === -0.
/// BEAM's `=:=` distinguishes ±0, so finite floats are normalized by adding
/// 0.0 before comparing (IEEE 754: -0.0 + 0.0 = +0.0). 2core's `JsNum` splits
/// arc's single `Finite(Float)` into `JInt|JFloat`, so cross-shape finites
/// compare via `int.to_float` under the same normalization.
pub fn strict_equal(left: JsVal, right: JsVal) -> Bool {
  case classify(left), classify(right) {
    KUndef, KUndef -> True
    KNull, KNull -> True
    KBool(a), KBool(b) -> a == b
    // NaN !== NaN
    KNum(JNan), _ | _, KNum(JNan) -> False
    // +0 === -0: normalize -0 → +0 via IEEE addition before comparing
    KNum(JFloat(a)), KNum(JFloat(b)) -> a +. 0.0 == b +. 0.0
    KNum(JInt(a)), KNum(JInt(b)) -> a == b
    KNum(JInt(a)), KNum(JFloat(b)) -> int.to_float(a) == b +. 0.0
    KNum(JFloat(a)), KNum(JInt(b)) -> a +. 0.0 == int.to_float(b)
    // ±Infinity: structural equality on the remaining JsNum arms
    KNum(a), KNum(b) -> a == b
    KStr(a), KStr(b) -> a == b
    KBig(a), KBig(b) -> a == b
    // Object identity (same Handle) — covers functions and arrays too
    KHandle(a), KHandle(b) -> a == b
    KSym(a), KSym(b) -> a == b
    _, _ -> False
  }
}

/// ES2024 §7.2.11 SameValue. Like `===`, except NaN equals NaN and +0 does NOT
/// equal -0. Used by Proxy invariant checks and Object.defineProperty.
pub fn same_value(left: JsVal, right: JsVal) -> Bool {
  case classify(left), classify(right) {
    KNum(JNan), KNum(JNan) -> True
    // Erlang term equality (=:=) distinguishes -0.0 from +0.0 (OTP 27+) and
    // compares floats exactly — precisely SameValue's number semantics.
    KNum(JFloat(a)), KNum(JFloat(b)) -> float_same_term(a, b)
    KNum(JInt(a)), KNum(JInt(b)) -> a == b
    // JInt is never -0 (Erlang integers have no signed zero), so promote to
    // Float and let =:= distinguish the JFloat side's sign of zero.
    KNum(JInt(a)), KNum(JFloat(b)) -> float_same_term(int.to_float(a), b)
    KNum(JFloat(a)), KNum(JInt(b)) -> float_same_term(a, int.to_float(b))
    _, _ -> strict_equal(left, right)
  }
}

/// ES2024 §7.2.12 SameValueZero. Like `===`, but NaN equals NaN. ±0 are still
/// equal. Used by Array.prototype.includes and Map/Set key equality.
pub fn same_value_zero(left: JsVal, right: JsVal) -> Bool {
  case classify(left), classify(right) {
    // NaN SameValueZero NaN → true (this is the only difference from ===)
    KNum(JNan), KNum(JNan) -> True
    _, _ -> strict_equal(left, right)
  }
}

/// Erlang `=:=` on floats: exact term equality, distinguishes -0.0 from +0.0.
/// Exists solely for SameValue's ±0-distinguishing number compare above.
@external(erlang, "arc_rt_val_ffi", "float_same_term")
fn float_same_term(a: Float, b: Float) -> Bool

/// True iff `x` is IEEE 754 negative zero (a zero whose sign bit is set).
/// BEAM floats preserve the sign bit but Gleam has no `-0.0` literal, so the
/// sign-bit test lives in the FFI.
@external(erlang, "arc_rt_val_ffi", "is_neg_zero")
pub fn is_neg_zero(x: Float) -> Bool

// ── §7.1.5-7 pure JsNum → Int/String helpers (jsnum-pure-helpers) ───────────
// Ports of arc `numeric.gleam:295-329` + `value.gleam:4419-4440` +
// `coerce.gleam:259-266`. arc's single `Finite(Float)` splits into 2core's
// `JInt|JFloat`; the `JInt` arm stays on Ints — never round-trips a Float.

/// ES2024 §21.1.2.6 Number.MAX_SAFE_INTEGER = 2^53 - 1.
pub const max_safe_integer: Int = 9_007_199_254_740_991

/// Truncate a JS float toward zero to an Int (matches `Math.trunc` /
/// ToIntegerOrInfinity's finite arm). Port of arc `value.gleam:4419-4424`.
pub fn float_to_int(f: Float) -> Int {
  case f <. 0.0 {
    True -> 0 - float.truncate(float.negate(f))
    False -> float.truncate(f)
  }
}

/// `Some(i)` iff `f` is an integral Number (§7.2.6 IsIntegralNumber) whose
/// value is `i`; `None` for a fractional `f`. ±0-safe: `+. 0.0` normalizes
/// -0.0 before comparing (BEAM `=:=` treats `0.0 =:= -0.0` as False). Port
/// of arc `value.gleam:4432-4440`.
pub fn integral_int(f: Float) -> Option(Int) {
  let i = float_to_int(f)
  case int.to_float(i) +. 0.0 == f +. 0.0 {
    True -> Some(i)
    False -> None
  }
}

/// Wrap an exact Int to an unsigned 32-bit value in [0, 2^32). Erlang's
/// `band` on negatives uses infinite two's complement, so this is a true
/// modulo-2^32 reduction for any sign. Port of arc `numeric.gleam:327-329`.
pub fn wrap_uint32(i: Int) -> Int {
  int.bitwise_and(i, 0xFFFFFFFF)
}

/// Wrap an exact Int to a signed 32-bit value: reduce modulo 2^32 then sign
/// extend. Int stays Int — never round-trip through a Float or the low 32
/// bits get rounded away past 2^53. Port of arc `numeric.gleam:314-322`.
pub fn wrap_int32(i: Int) -> Int {
  let wrapped = wrap_uint32(i)
  case wrapped > 0x7FFFFFFF {
    True -> wrapped - 0x100000000
    False -> wrapped
  }
}

/// §7.1.6 ToInt32 of an already-ToNumber'd value: NaN/±∞ → 0, finite →
/// truncate toward zero then wrap modulo 2^32 with sign extension. `JInt`
/// wraps directly (no float round-trip). Port of arc `numeric.gleam:295-300`.
pub fn num_to_int32(n: JsNum) -> Int {
  case n {
    JNan | JPosInf | JNegInf -> 0
    JInt(i) -> wrap_int32(i)
    JFloat(f) -> wrap_int32(float.truncate(f))
  }
}

/// §7.1.7 ToUint32 of an already-ToNumber'd value: NaN/±∞ → 0, finite →
/// truncate toward zero then reduce modulo 2^32 (result in [0, 2^32)).
/// Port of arc `numeric.gleam:303-310`.
pub fn num_to_uint32(n: JsNum) -> Int {
  case n {
    JNan | JPosInf | JNegInf -> 0
    JInt(i) -> wrap_uint32(i)
    JFloat(f) -> wrap_uint32(float.truncate(f))
  }
}

/// §7.1.5 ToIntegerOrInfinity of an already-ToNumber'd value, saturating ±∞
/// to ±`max_safe_integer` so downstream `int.clamp`/`int.min`/`int.max`
/// behave like the spec's explicit "+∞ → len / -∞ → 0" branches. Port of
/// arc `coerce.gleam:259-266`.
pub fn jsnum_to_integer_or_infinity(n: JsNum) -> Int {
  case n {
    JNan -> 0
    JInt(i) -> i
    JFloat(f) -> float_to_int(f)
    JPosInf -> max_safe_integer
    JNegInf -> 0 - max_safe_integer
  }
}

/// §7.1.20 ToLength: ToIntegerOrInfinity clamped to [0, 2^53-1].
pub fn jsnum_to_length(n: JsNum) -> Int {
  int.clamp(jsnum_to_integer_or_infinity(n), min: 0, max: max_safe_integer)
}

/// §6.1.6.1.20 Number::toString(x, 10) — the canonical JS decimal rendering.
/// `JInt` uses `int.to_string` directly (exact); `JFloat` delegates to the
/// FFI shortest-round-trip formatter.
pub fn jsnum_to_string(n: JsNum) -> String {
  case n {
    JNan -> "NaN"
    JPosInf -> "Infinity"
    JNegInf -> "-Infinity"
    JInt(i) -> int.to_string(i)
    JFloat(f) -> js_format_float(f)
  }
}

/// Format a finite Float per ES2024 §6.1.6.1.20 Number::toString. Delegates
/// to the FFI for JS-compatible output (1e21 → "1e+21", 1e-6 → "0.000001",
/// -0 → "0"). Port of arc `value.js_format_number`.
@external(erlang, "arc_rt_val_ffi", "js_number_to_string")
pub fn js_format_float(f: Float) -> String

/// Alias for `jsnum_to_string` — the SPEC §7.M3 name for the pure
/// JsNum→String formatter used by `t_to_string`'s primitive table.
pub fn format_jsnum(n: JsNum) -> String {
  jsnum_to_string(n)
}

// ── §7.1.4 ToNumber, primitive-only (prim-to-number) ────────────────────────

/// Why the primitive-only `ToNumber` table can't just yield a `JsNum`: Symbol
/// and BigInt are non-recoverable TypeErrors (§7.1.4 rows 7-8), and an Object
/// means "run ToPrimitive first, then retry" — a signal, not a value.
pub type CoerceError {
  /// Input is an Object — caller must `t_to_primitive(HintNumber)` then retry.
  NeedsToPrimitive
  /// §7.1.4 row 8: "Cannot convert a Symbol value to a number".
  SymbolToNumber
  /// §7.1.4 row 7: "Cannot convert a BigInt to a number".
  BigIntToNumber
}

/// ES2024 §7.1.4 ToNumber's primitive conversion table. Objects are NOT
/// coerced here (that would need state for `ToPrimitive`); they yield
/// `Error(NeedsToPrimitive)` so the threaded `t_to_number` can retry after
/// the upcall. Port of arc `value.gleam:4543-4560` with arc's `Finite(0.0)`/
/// `Finite(1.0)` mapped to 2core's `JInt(0)`/`JInt(1)`.
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

/// ES2024 §7.1.17 ToString's primitive conversion table. Objects yield
/// `Error(NeedsToPrimitive)` (caller must `t_to_primitive(HintString)` first,
/// then retry); a Symbol is `Error(SymbolToNumber)` — reused as the "Symbol
/// cannot be a coerced" tag, thrown by the caller as "Cannot convert a Symbol
/// value to a string". Port of arc `coerce.gleam:js_to_string`'s post-
/// ToPrimitive match table.
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

// ── §7.1.17 ToString (threaded — objects go through ToPrimitive) ────────────

/// ES2024 §7.1.17 ToString(argument). ToPrimitive with hint "string" first,
/// then a total match on §7.1.17's conversion table over the primitive
/// result — so no re-dispatch, no recursion. Port of arc
/// `coerce.gleam:168-190 js_to_string` with the D7 Result→throw rewrite.
pub fn t_to_string(st: Agent, v: JsVal) -> #(String, Agent) {
  let #(prim, st) = t_to_primitive(st, v, HintString)
  case classify(prim) {
    KStr(s) -> #(s, st)
    KNum(n) -> #(jsnum_to_string(n), st)
    KBool(True) -> #("true", st)
    KBool(False) -> #("false", st)
    KNull -> #("null", st)
    KUndef -> #("undefined", st)
    KBig(n) -> #(int.to_string(n), st)
    KSym(_) ->
      t_throw_type_error(st, "Cannot convert a Symbol value to a string")
    // t_to_primitive never returns an object, and it panics on TDZ.
    KHandle(_) | KTdz -> panic as "ToString: ToPrimitive returned non-primitive"
  }
}

// ── §7.1.19 ToPropertyKey ───────────────────────────────────────────────────

/// ES2024 §7.1.19 ToPropertyKey(argument). ToPrimitive with hint "string";
/// a Symbol result becomes `SymbolKey`, everything else is `! ToString`
/// canonicalized to a `StringKey`. The Symbol check runs on the *primitive*
/// (post-ToPrimitive), so a `@@toPrimitive` returning a Symbol yields a
/// `SymbolKey`, not a "Cannot convert a Symbol value to a string" TypeError.
/// Port of arc `property.gleam:18-82`.
pub fn t_to_property_key(st: Agent, v: JsVal) -> #(ObjectKey, Agent) {
  case classify(v) {
    // Step 1: ToPrimitive is only observable on objects; re-dispatch on the
    // result so step 2's Symbol check sees the primitive.
    KHandle(_) -> {
      let #(prim, st) = t_to_primitive(st, v, HintString)
      primitive_to_prop_key(st, prim)
    }
    _ -> primitive_to_prop_key(st, v)
  }
}

/// Steps 2-3 of §7.1.19 for an already-primitive `v`. Numbers/strings take
/// fast paths so downstream [[Get]]/[[Set]] don't re-stringify: array-index
/// numbers → `Index(n)` (skip stringify), strings → `canonical_key`, else
/// `t_to_string` → `canonical_key`.
fn primitive_to_prop_key(st: Agent, v: JsVal) -> #(ObjectKey, Agent) {
  case classify(v) {
    // Step 2: If key is a Symbol, return key.
    KSym(id) -> #(SymbolKey(id), st)
    // `index_key` already applies the [0, 2^32-2] canonical-array-index check
    // and falls back to `Named(int.to_string(n))` — the JInt-shape sibling of
    // arc's Finite(n) → array_index_of_float / js_format_number split.
    KNum(JInt(n)) -> #(StringKey(index_key(n)), st)
    KNum(JFloat(f)) ->
      case array_index_of_float(f) {
        // Valid array index — skip stringification entirely.
        Some(i) -> #(StringKey(Index(i)), st)
        // Non-index number — stringify (e.g. 1.5 → "1.5", -1 → "-1").
        None -> #(StringKey(Named(js_format_float(f))), st)
      }
    KNum(JNan) -> #(StringKey(Named("NaN")), st)
    KNum(JPosInf) -> #(StringKey(Named("Infinity")), st)
    KNum(JNegInf) -> #(StringKey(Named("-Infinity")), st)
    KStr(s) -> #(StringKey(canonical_key(s)), st)
    // Step 3: ToString(key) — key is a non-Symbol primitive, cannot re-enter.
    _ -> {
      let #(s, st) = t_to_string(st, v)
      #(StringKey(canonical_key(s)), st)
    }
  }
}

// ── §7.1.4.1.1 StringToNumber ───────────────────────────────────────────────

/// A `parse_float` FFI failure: `OutOfRange` when the text is valid float
/// syntax whose magnitude overflows a double; `Invalid` otherwise.
pub type FloatParseError {
  OutOfRange
  Invalid
}

/// A JS decimal literal → Float, with a typed failure: `OutOfRange` when the
/// text is valid float syntax whose magnitude overflows a double, `Invalid`
/// otherwise. Port of arc `parser/number.parse_float` (arc_float_ffi).
@external(erlang, "arc_rt_val_ffi", "parse_float")
pub fn parse_float(s: String) -> Result(Float, FloatParseError)

/// ES2024 §7.1.4.1.1 StringToNumber: trims whitespace, accepts leading + or -,
/// Infinity, scientific notation, leading/trailing decimal point, and
/// hex/oct/bin prefixes. Port of arc `value.gleam:string_to_number`.
pub fn string_to_number(s: String) -> JsNum {
  // Fast path: a plain run of ASCII digits (optional single leading '-').
  // Such strings have no whitespace to trim and can't be float/hex literals,
  // so the general path's failed float.parse attempts (caught badargs) and
  // binary pattern compiles are pure overhead. Very hot via canonical numeric
  // index conversion of array-like string keys ("0", "1", ...).
  case parse_plain_digits(s) {
    Ok(n) -> n
    Error(Nil) -> string_to_number_slow(s)
  }
}

/// Parse a string that is exactly an optional '-' followed by 1..15 ASCII
/// digits. 15 digits keeps the value exactly representable as a float, so the
/// result is identical to the general path. Anything else falls through.
fn parse_plain_digits(s: String) -> Result(JsNum, Nil) {
  case bit_array.from_string(s) {
    // '-' prefix: negate via float.negate so "-0" yields -0.0 like the
    // general path does.
    <<0x2d, rest:bytes>> -> {
      use n <- result.map(accumulate_digits(rest, 0, 0))
      JFloat(float.negate(int.to_float(n)))
    }
    bytes -> {
      use n <- result.map(accumulate_digits(bytes, 0, 0))
      JFloat(int.to_float(n))
    }
  }
}

fn accumulate_digits(
  bytes: BitArray,
  acc: Int,
  count: Int,
) -> Result(Int, Nil) {
  case bytes {
    <<>> if count >= 1 && count <= 15 -> Ok(acc)
    <<d, rest:bytes>> if d >= 0x30 && d <= 0x39 ->
      accumulate_digits(rest, acc * 10 + d - 0x30, count + 1)
    _ -> Error(Nil)
  }
}

/// Single-pass byte-walk over the string: hand-rolled StrWhiteSpace trim
/// (no pattern compiles), grammar validated while scanning so float parsing
/// runs at most once on a known-well-formed literal (no caught badargs).
fn string_to_number_slow(s: String) -> JsNum {
  let bytes = trim_string_ws(bit_array.from_string(s))
  case bytes {
    <<>> -> JFloat(0.0)
    // NonDecimalIntegerLiteral (§7.1.4.1 StringNumericLiteral): hex/octal/
    // binary prefixes. No sign is permitted with these forms.
    <<"0x":utf8, digits:bytes>> | <<"0X":utf8, digits:bytes>> ->
      parse_radix_literal(digits, 16)
    <<"0o":utf8, digits:bytes>> | <<"0O":utf8, digits:bytes>> ->
      parse_radix_literal(digits, 8)
    <<"0b":utf8, digits:bytes>> | <<"0B":utf8, digits:bytes>> ->
      parse_radix_literal(digits, 2)
    <<"-":utf8, rest:bytes>> ->
      case parse_unsigned_literal(rest) {
        Ok(n) -> negate_jsnum(n)
        Error(Nil) -> JNan
      }
    <<"+":utf8, rest:bytes>> ->
      case parse_unsigned_literal(rest) {
        Ok(n) -> n
        Error(Nil) -> JNan
      }
    _ ->
      case parse_unsigned_literal(bytes) {
        Ok(n) -> n
        Error(Nil) -> JNan
      }
  }
}

fn negate_jsnum(n: JsNum) -> JsNum {
  case n {
    JFloat(f) -> JFloat(float.negate(f))
    JInt(i) -> JInt(0 - i)
    JPosInf -> JNegInf
    JNegInf -> JPosInf
    JNan -> JNan
  }
}

/// Trim StrWhiteSpaceChar (§7.1.4.1: WhiteSpace ∪ LineTerminator) from both
/// ends. Note this is NOT Unicode White_Space: U+0085 NEL is excluded and
/// U+FEFF ZWNBSP included, matching the JS spec.
fn trim_string_ws(bytes: BitArray) -> BitArray {
  let bytes = drop_leading_string_ws(bytes)
  let keep = content_length(bytes, 0, 0)
  case keep == bit_array.byte_size(bytes) {
    True -> bytes
    False -> {
      let assert Ok(trimmed) = bit_array.slice(bytes, 0, keep)
        as "keep is always <= byte_size"
      trimmed
    }
  }
}

fn drop_leading_string_ws(bytes: BitArray) -> BitArray {
  case bytes {
    // TAB LF VT FF CR SP
    <<b, rest:bytes>>
      if b == 0x09
      || b == 0x0a
      || b == 0x0b
      || b == 0x0c
      || b == 0x0d
      || b == 0x20
    -> drop_leading_string_ws(rest)
    // U+00A0 NBSP
    <<0xc2, 0xa0, rest:bytes>> -> drop_leading_string_ws(rest)
    // U+1680 OGHAM SPACE MARK
    <<0xe1, 0x9a, 0x80, rest:bytes>> -> drop_leading_string_ws(rest)
    // U+2000..U+200A spaces, U+2028 LS, U+2029 PS, U+202F NNBSP
    <<0xe2, 0x80, b, rest:bytes>>
      if b >= 0x80 && b <= 0x8a || b == 0xa8 || b == 0xa9 || b == 0xaf
    -> drop_leading_string_ws(rest)
    // U+205F MMSP
    <<0xe2, 0x81, 0x9f, rest:bytes>> -> drop_leading_string_ws(rest)
    // U+3000 IDEOGRAPHIC SPACE
    <<0xe3, 0x80, 0x80, rest:bytes>> -> drop_leading_string_ws(rest)
    // U+FEFF ZWNBSP
    <<0xef, 0xbb, 0xbf, rest:bytes>> -> drop_leading_string_ws(rest)
    _ -> bytes
  }
}

/// Byte length of `bytes` up to and including the last byte that is not part
/// of a StrWhiteSpaceChar — i.e. the length after trimming trailing JS
/// whitespace.
fn content_length(bytes: BitArray, idx: Int, last: Int) -> Int {
  case bytes {
    <<>> -> last
    <<b, rest:bytes>>
      if b == 0x09
      || b == 0x0a
      || b == 0x0b
      || b == 0x0c
      || b == 0x0d
      || b == 0x20
    -> content_length(rest, idx + 1, last)
    <<0xc2, 0xa0, rest:bytes>> -> content_length(rest, idx + 2, last)
    <<0xe1, 0x9a, 0x80, rest:bytes>> -> content_length(rest, idx + 3, last)
    <<0xe2, 0x80, b, rest:bytes>>
      if b >= 0x80 && b <= 0x8a || b == 0xa8 || b == 0xa9 || b == 0xaf
    -> content_length(rest, idx + 3, last)
    <<0xe2, 0x81, 0x9f, rest:bytes>> -> content_length(rest, idx + 3, last)
    <<0xe3, 0x80, 0x80, rest:bytes>> -> content_length(rest, idx + 3, last)
    <<0xef, 0xbb, 0xbf, rest:bytes>> -> content_length(rest, idx + 3, last)
    <<_, rest:bytes>> -> content_length(rest, idx + 1, idx + 1)
    _ -> panic as "content_length: input is a UTF-8 string, always byte-aligned"
  }
}

/// Parse a StrUnsignedDecimalLiteral (any sign already stripped by the
/// caller): "Infinity", digits[.digits][exp], .digits[exp] or digits.[exp].
fn parse_unsigned_literal(bytes: BitArray) -> Result(JsNum, Nil) {
  case bytes {
    <<"Infinity":utf8>> -> Ok(JPosInf)
    _ -> {
      let #(icount, after_int) = scan_ascii_digits(bytes, 0)
      case after_int {
        // Entirely digits: an integer literal.
        <<>> if icount > 0 -> parse_integer_literal(bytes)
        <<".":utf8, after_dot:bytes>> -> {
          let #(fcount, after_frac) = scan_ascii_digits(after_dot, 0)
          case icount > 0 || fcount > 0 {
            False -> Error(Nil)
            True -> {
              use Nil <- result.try(check_exponent_part(after_frac))
              parse_decimal_literal(bytes)
            }
          }
        }
        // No dot, trailing bytes after the digits: must be an ExponentPart.
        _ if icount > 0 -> {
          use Nil <- result.try(check_exponent_part(after_int))
          parse_decimal_literal(bytes)
        }
        _ -> Error(Nil)
      }
    }
  }
}

fn scan_ascii_digits(bytes: BitArray, count: Int) -> #(Int, BitArray) {
  case bytes {
    <<d, rest:bytes>> if d >= 0x30 && d <= 0x39 ->
      scan_ascii_digits(rest, count + 1)
    _ -> #(count, bytes)
  }
}

/// Check that whatever trails the mantissa is a well-formed (possibly absent)
/// ExponentPart, and nothing else. Only its validity matters here: the literal
/// text is handed to `parse_float` verbatim, exponent included.
fn check_exponent_part(bytes: BitArray) -> Result(Nil, Nil) {
  case bytes {
    <<>> -> Ok(Nil)
    <<e, digits:bytes>> if e == 0x65 || e == 0x45 -> {
      let valid = case digits {
        <<"+":utf8, ds:bytes>> | <<"-":utf8, ds:bytes>> ->
          nonempty_all_digits(ds)
        _ -> nonempty_all_digits(digits)
      }
      case valid {
        False -> Error(Nil)
        True -> Ok(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

fn nonempty_all_digits(bytes: BitArray) -> Bool {
  case bytes {
    <<d>> if d >= 0x30 && d <= 0x39 -> True
    <<d, rest:bytes>> if d >= 0x30 && d <= 0x39 -> nonempty_all_digits(rest)
    _ -> False
  }
}

/// Convert an already-validated decimal literal (mantissa + optional exponent,
/// no sign) to a Number through the engine's one decimal→double normalizer.
/// A magnitude past the double range is Infinity, not NaN: §7.1.4.1 rounds
/// StringNumericLiteral to the nearest Number, and `Number("1e999")` is
/// +Infinity. The caller re-applies any leading sign.
fn parse_decimal_literal(bytes: BitArray) -> Result(JsNum, Nil) {
  use text <- result.try(bit_array.to_string(bytes))
  case parse_float(text) {
    Ok(f) -> Ok(JFloat(f))
    Error(OutOfRange) -> Ok(JPosInf)
    Error(Invalid) -> Error(Nil)
  }
}

/// Parse an all-digits integer literal. Goes through float syntax first;
/// only literals beyond double range (~1e308, 309+ digits) fall back to
/// arbitrary-precision integer parsing, which `num_from_int` saturates to
/// Infinity per §7.1.4.1 instead of crashing in erlang:float/1.
fn parse_integer_literal(bytes: BitArray) -> Result(JsNum, Nil) {
  use digits <- result.try(bit_array.to_string(bytes))
  case float.parse(digits <> ".0") {
    Ok(f) -> Ok(JFloat(f))
    Error(Nil) -> int.parse(digits) |> result.map(num_from_int)
  }
}

/// Parse the digits of a NonDecimalIntegerLiteral ("0x.." / "0o.." / "0b..").
/// Empty or signed digit sequences are NaN per §7.1.4.1.
fn parse_radix_literal(digits: BitArray, radix: Int) -> JsNum {
  case digits {
    <<>> | <<"-":utf8, _:bytes>> | <<"+":utf8, _:bytes>> -> JNan
    _ -> {
      let parsed =
        bit_array.to_string(digits)
        |> result.try(int.base_parse(_, radix))
      case parsed {
        Ok(n) -> num_from_int(n)
        Error(Nil) -> JNan
      }
    }
  }
}

const nf_two52 = 4_503_599_627_370_496

const nf_two53 = 9_007_199_254_740_992

/// Integer → Number with correct rounding (round-to-nearest, ties-to-even).
/// Erlang's float/1 mis-rounds integers wider than 53 bits, so reduce to a
/// 53-bit mantissa ourselves and convert the (exactly representable) result.
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

/// ES2024 §7.1.14 StringToBigInt — decimal (with sign) or 0x/0o/0b prefixed;
/// empty/whitespace-only → 0; anything else fails (None).
pub fn string_to_bigint(s: String) -> Option(Int) {
  let bytes = trim_string_ws(bit_array.from_string(s))
  case bit_array.to_string(bytes) {
    Error(Nil) -> None
    Ok("") -> Some(0)
    Ok("0x" <> rest) | Ok("0X" <> rest) -> parse_bigint_radix_digits(rest, 16)
    Ok("0o" <> rest) | Ok("0O" <> rest) -> parse_bigint_radix_digits(rest, 8)
    Ok("0b" <> rest) | Ok("0B" <> rest) -> parse_bigint_radix_digits(rest, 2)
    Ok(t) -> int.parse(t) |> option.from_result
  }
}

fn parse_bigint_radix_digits(digits: String, base: Int) -> Option(Int) {
  case digits {
    "-" <> _ | "+" <> _ -> None
    _ -> int.base_parse(digits, base) |> option.from_result
  }
}

// ── §7.1.3-7 / §7.1.18 / §7.1.20 remaining threaded coercions ───────────────
// Ports of arc `coerce.gleam:202-337`. Each = `t_to_primitive`/`t_to_number`
// then a pure table.

/// ES2024 §7.1.4 ToNumber with VM re-entry for ToPrimitive. BigInt and Symbol
/// are TypeErrors (§7.1.4 conversion table). Port of arc
/// `coerce.gleam:202-225 js_to_number` with the D7 Result→throw rewrite.
pub fn t_to_number(st: Agent, v: JsVal) -> #(JsNum, Agent) {
  let #(prim, st) = t_to_primitive(st, v, HintNumber)
  case classify(prim) {
    KNum(n) -> #(n, st)
    KStr(s) -> #(string_to_number(s), st)
    KBool(True) -> #(JInt(1), st)
    KBool(False) -> #(JInt(0), st)
    KNull -> #(JInt(0), st)
    KUndef -> #(JNan, st)
    KBig(_) -> t_throw_type_error(st, "Cannot convert BigInt to number")
    KSym(_) -> t_throw_type_error(st, "Cannot convert Symbol to number")
    KHandle(_) | KTdz -> panic as "ToNumber: ToPrimitive returned non-primitive"
  }
}

/// ES2024 §7.1.3 ToNumeric: ToPrimitive(number hint), then a BigInt is
/// returned as-is; otherwise wrapped ToNumber. Result is always `KNum | KBig`.
pub fn t_to_numeric(st: Agent, v: JsVal) -> #(JsVal, Agent) {
  let #(prim, st) = t_to_primitive(st, v, HintNumber)
  case classify(prim) {
    KBig(_) -> #(prim, st)
    KNum(_) -> #(prim, st)
    KStr(s) -> #(mk_number(string_to_number(s)), st)
    KBool(True) -> #(mk_number(JInt(1)), st)
    KBool(False) -> #(mk_number(JInt(0)), st)
    KNull -> #(mk_number(JInt(0)), st)
    KUndef -> #(mk_number(JNan), st)
    KSym(_) -> t_throw_type_error(st, "Cannot convert Symbol to number")
    KHandle(_) | KTdz ->
      panic as "ToNumeric: ToPrimitive returned non-primitive"
  }
}

/// ES2024 §7.1.13 ToBigInt(argument). ToPrimitive with hint "number", then
/// the §7.1.13 conversion table: BigInt/Bool/String succeed; Number/Symbol/
/// null/undefined → TypeError; a string StringToBigInt rejects → SyntaxError.
/// Port of arc `coerce.gleam:377-405 to_bigint`.
pub fn t_to_bigint(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(prim, st) = t_to_primitive(st, v, HintNumber)
  case classify(prim) {
    KBig(n) -> #(n, st)
    KBool(True) -> #(1, st)
    KBool(False) -> #(0, st)
    KStr(s) ->
      case string_to_bigint(s) {
        Some(n) -> #(n, st)
        // §7.1.13: StringToBigInt returning undefined throws a SyntaxError
        // (not TypeError — that's for Number/Symbol/null/undefined).
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

/// ES2024 §7.1.18 ToObject. `null`/`undefined` → TypeError; objects pass
/// through; primitives box via `ops.to_object` upcall (D17).
pub fn t_to_object(st: Agent, v: JsVal) -> #(Handle, Agent) {
  case classify(v) {
    KHandle(h) -> #(h, st)
    KNull -> t_throw_type_error(st, "Cannot convert null to object")
    KUndef -> t_throw_type_error(st, "Cannot convert undefined to object")
    KTdz -> panic as "ToObject on the TDZ sentinel"
    _ -> require_ops(st).to_object(st, v)
  }
}

/// ES2024 §7.1.6 ToInt32: full ToNumber (ToPrimitive on objects, TypeError on
/// Symbol/BigInt), then modular reduction to [-2^31, 2^31).
pub fn t_to_int32(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = t_to_number(st, v)
  #(num_to_int32(n), st)
}

/// ES2024 §7.1.7 ToUint32: full ToNumber, then modular reduction to [0, 2^32).
pub fn t_to_uint32(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = t_to_number(st, v)
  #(num_to_uint32(n), st)
}

/// ES2024 §7.1.5 ToIntegerOrInfinity: full ToNumber then
/// `jsnum_to_integer_or_infinity` (±∞ saturated to ±(2^53-1)).
pub fn t_to_integer_or_infinity(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = t_to_number(st, v)
  #(jsnum_to_integer_or_infinity(n), st)
}

/// ES2024 §7.1.20 ToLength: full ToNumber then `jsnum_to_length`.
pub fn t_to_length(st: Agent, v: JsVal) -> #(Int, Agent) {
  let #(n, st) = t_to_number(st, v)
  #(jsnum_to_length(n), st)
}

/// ES2024 §7.1.22 ToIndex(value). ToIntegerOrInfinity, then RangeError with
/// caller-supplied `err_msg` if the result is outside [0, 2^53-1]. undefined
/// short-circuits to 0 (ToNumber(undefined) is NaN, no observable steps).
/// Port of arc `coerce.gleam:417-446 to_index` with the D7 Result→throw
/// rewrite and the JInt|JFloat split.
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
