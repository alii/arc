//// `rt_ops` — ES2024 §13 operator surface (SPEC §7.M5, M5.md).
////
//// Port of `arc/src/arc/vm/ops/{operators,numeric,instanceof}.gleam`
//// re-expressed over the threaded `Agent` and `classify`-based value
//// model. Every threaded op returns `#(V, Agent)` — value FIRST (R1).
//// D7: throwing ops RAISE via `rt_val.t_throw_*` (never `Result`).
//// D17: reaches `t_call_checked` ONLY via `js_ops(st).call` (no direct
//// `rt_call` import — cycle). R8: `strict_eq`/`strict_ne` are JPure
//// (no `St`, return `Bool`). M5.md's Erlang-@external facade is semantic
//// reference only — this module is pure Gleam composing `rt_val`.

import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsNum, type JsOps, type JsVal, HintDefault,
  HintNumber, JFloat, JInt, JNan, JNegInf, JPosInf, KBig, KBool, KBound, KHandle,
  KNull, KNum, KStr, KSym, KUndef, Named, SObject, StringKey, SymbolKey,
  classify, mk_bigint, mk_number, mk_object, mk_string, symbol_has_instance,
}
import arc/rt/val as rt_val
import gleam/float
import gleam/int
import gleam/option.{None, Some}
import gleam/order
import gleam/string

/// The seeded `JsOps` upcall table (D17). Same posture as
/// `rt_store.require_js`: unseeded → panic (engine bug).
fn js_ops(st: Agent) -> JsOps(Agent) {
  st.store.ops
}

// ── §13.10.2 InstanceofOperator / §7.3.22 OrdinaryHasInstance ───────────────
// Port of arc `instanceof.gleam:28-211`.

/// ES2024 §13.10.2 InstanceofOperator ( V, target ). Returns `#(1|0, st')`.
/// Port of arc `instanceof.gleam:28-91`. NO IntrinsicHandler shortcut:
/// `NativeToken` has no `FunctionHasInstance` variant yet (M6 seeds it), so
/// every callable @@hasInstance handler goes through `ops.call`.
pub fn t_instance_of(st: Agent, v: JsVal, target: JsVal) -> #(Int, Agent) {
  case classify(target) {
    // Step 1: If target is not an Object, throw a TypeError.
    KHandle(ctor_h) -> {
      let ops = js_ops(st)
      // Step 2: instOfHandler ← ? GetMethod(target, @@hasInstance).
      let #(handler, st) =
        ops.get_prop(st, target, SymbolKey(symbol_has_instance))
      case rt_val.is_nullish(handler) {
        // GetMethod §7.3.11 step 2: undefined/null → absent — steps 4-5.
        True -> {
          let #(callable, st) = rt_val.t_is_callable(st, target)
          case callable {
            // Step 5: ? OrdinaryHasInstance(target, V).
            True -> t_ordinary_has_instance(st, ctor_h, v)
            // Step 4: IsCallable(target) is false → TypeError.
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
            // Step 3.a: ToBoolean(? Call(instOfHandler, target, « V »)).
            True -> {
              let #(res, st) = ops.call(st, handler, target, [v])
              #(bool_int(rt_val.to_boolean(res)), st)
            }
            // GetMethod §7.3.11 step 3: present but not callable → TypeError.
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

/// ES2024 §7.3.22 OrdinaryHasInstance ( C, O ), steps 2-7. Caller has already
/// verified `ctor` is callable (step 1). Port of arc
/// `instanceof.gleam:131-176`.
pub fn t_ordinary_has_instance(
  st: Agent,
  ctor: Handle,
  v: JsVal,
) -> #(Int, Agent) {
  case rt_store.t_cell_get(st, ctor) {
    // Step 2: C has [[BoundTargetFunction]] → InstanceofOperator(O, BC).
    SObject(kind: KBound(target:, ..), ..) ->
      t_instance_of(st, v, mk_object(target))
    _ ->
      // Step 3: If O is not an Object, return false — BEFORE the Get, so a
      // throwing "prototype" getter does NOT fire for primitives.
      case classify(v) {
        KHandle(obj_h) -> {
          // Step 4: P ← ? Get(C, "prototype").
          let #(proto_val, st) =
            js_ops(st).get_prop(
              st,
              mk_object(ctor),
              StringKey(Named("prototype")),
            )
          case classify(proto_val) {
            // Step 7: prototype-chain walk.
            KHandle(proto_h) -> proto_walk(st, obj_h, proto_h, 10_000)
            // Step 5: P is not an Object → TypeError.
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

/// §7.3.22 step 7 prototype-chain walk. Bounded by `fuel`: a `getPrototypeOf`
/// proxy trap that returns a fresh proxy every hop would spin forever without
/// re-entering the JS call stack — same RangeError as V8's stack-limit check
/// in `HasInPrototypeChain`. Port of arc `instanceof.gleam:186-211`.
fn proto_walk(
  st: Agent,
  obj: Handle,
  target_proto: Handle,
  fuel: Int,
) -> #(Int, Agent) {
  case fuel <= 0 {
    True -> rt_val.t_throw_range_error(st, "Maximum call stack size exceeded")
    False -> {
      // Step 6a: O ← ? O.[[GetPrototypeOf]]().
      let #(next, st) = rt_obj.t_get_prototype_of(st, obj)
      case next {
        // Step 6b: O is null → false.
        None -> #(0, st)
        Some(proto_h) ->
          // Step 6c: SameValue(P, O) — Handle identity.
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

// ── ops-num-kernel: comparison primitives (arc numeric.gleam / ──────────────
// operators.gleam:528-554, 2core rt_js_ffi.erl:299-312).

/// Result of the §7.2.13 Abstract Relational Comparison — the four total-order
/// outcomes plus `Undef` (any-NaN case). Local, not `gleam/order`, so `Undef`
/// stays a first-class arm every mapper must handle.
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

/// §6.1.6.1.14 Number::lessThan lifted to a total `Cmp`. NaN → `Undef`;
/// ±0 compare `Eq` (neither `<.` nor `>.` holds). `JInt` promotes via
/// `int.to_float` — matches `rt_val.strict_equal`'s cross-shape rule.
/// Port of 2core rt_js_ffi.erl:299-312 `ncmp`.
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
        // Neither `<` nor `>` — numerically equal (covers +0.0 vs -0.0).
        False -> Eq
      }
  }
}

/// §6.1.6.2.12 BigInt-vs-Number relational compare. `JFloat` arm is exact:
/// compare against `⌊f⌋` as an arbitrary-precision Int (BEAM `trunc/1` on an
/// integral float is exact for any magnitude), then let the fractional part
/// decide the tie. Port of arc `operators.gleam:528-554`.
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
        // b == ⌊f⌋: equal iff f is itself integral, else b < f.
        order.Eq ->
          case fl == f {
            True -> Eq
            False -> Lt
          }
      }
    }
  }
}

// ── §7.2.13 Abstract Relational Comparison (ops-relational) ─────────────────
// Port of arc `operators.gleam` relational ladder (M5.md:88-94).

/// ES2024 §7.2.13 IsLessThan, expressed as a total `Cmp` on `(a, b)`.
/// Operand ToPrimitive is ALWAYS left-first — `t_gt`/`t_ge` do NOT swap
/// operands, they read the returned `Cmp` differently (arc semantics).
/// D10: both-string arm compares UTF-8 BYTES via `gleam/string.compare` —
/// deliberate divergence from spec's UTF-16 code-unit order (matches arc).
fn t_relational_cmp(st: Agent, a: JsVal, b: JsVal) -> #(Cmp, Agent) {
  let #(pa, st) = rt_val.t_to_primitive(st, a, HintNumber)
  let #(pb, st) = rt_val.t_to_primitive(st, b, HintNumber)
  case classify(pa), classify(pb) {
    // Step 3: both String — D10 byte-wise UTF-8 compare.
    KStr(sa), KStr(sb) -> #(order_to_cmp(string.compare(sa, sb)), st)
    // Step 4.a-b: BigInt vs String — StringToBigInt; unparseable → undefined.
    KBig(x), KStr(sb) -> #(cmp_bigint_str(x, sb), st)
    KStr(sa), KBig(y) -> #(cmp_negate(cmp_bigint_str(y, sa)), st)
    // Step 4.c-d: ToNumeric on both primitives.
    _, _ -> {
      let #(na, st) = rt_val.t_to_numeric(st, pa)
      let #(nb, st) = rt_val.t_to_numeric(st, pb)
      case classify(na), classify(nb) {
        KBig(x), KBig(y) -> #(order_to_cmp(int.compare(x, y)), st)
        KBig(x), KNum(n) -> #(compare_bigint_num(x, n), st)
        KNum(n), KBig(y) -> #(cmp_negate(compare_bigint_num(y, n)), st)
        KNum(x), KNum(y) -> #(ncmp(x, y), st)
        // t_to_numeric returns only KBig|KNum (rt_val.gleam:990).
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

/// ES2024 §13.10.1 `<` — Abstract Relational; NaN-involving compare is `0`.
pub fn t_lt(st: Agent, a: JsVal, b: JsVal) -> #(Int, Agent) {
  let #(c, st) = t_relational_cmp(st, a, b)
  case c {
    Lt -> #(1, st)
    Eq | Gt | Undef -> #(0, st)
  }
}

/// ES2024 §13.10.1 `<=` — `Undef` (any NaN) → `0`.
pub fn t_le(st: Agent, a: JsVal, b: JsVal) -> #(Int, Agent) {
  let #(c, st) = t_relational_cmp(st, a, b)
  case c {
    Lt | Eq -> #(1, st)
    Gt | Undef -> #(0, st)
  }
}

/// ES2024 §13.10.1 `>` — computed on `cmp(a, b)` (NO operand swap).
pub fn t_gt(st: Agent, a: JsVal, b: JsVal) -> #(Int, Agent) {
  let #(c, st) = t_relational_cmp(st, a, b)
  case c {
    Gt -> #(1, st)
    Lt | Eq | Undef -> #(0, st)
  }
}

/// ES2024 §13.10.1 `>=` — `Undef` (any NaN) → `0`.
pub fn t_ge(st: Agent, a: JsVal, b: JsVal) -> #(Int, Agent) {
  let #(c, st) = t_relational_cmp(st, a, b)
  case c {
    Gt | Eq -> #(1, st)
    Lt | Undef -> #(0, st)
  }
}

// ── §13.15.4 Bitwise / shift operators (ops-bitwise) ────────────────────────
// Port of arc `operators.gleam:106-138, 284-298, 382-409`. Shared spine
// (M5.md:68): ToPrimitive both LEFT-first (R1 order — object operands may
// re-enter JS with side effects), then ToNumeric both. Result is always
// `KBig | KNum` (rt_val.t_to_numeric guarantee).

const bigint_mix_error = "Cannot mix BigInt and other types, use explicit conversions"

fn to_numeric_operands(
  st: Agent,
  a: JsVal,
  b: JsVal,
) -> #(JsVal, JsVal, Agent) {
  let #(ap, st) = rt_val.t_to_primitive(st, a, HintNumber)
  let #(bp, st) = rt_val.t_to_primitive(st, b, HintNumber)
  let #(an, st) = rt_val.t_to_numeric(st, ap)
  let #(bn, st) = rt_val.t_to_numeric(st, bp)
  #(an, bn, st)
}

/// Apply a signed 32-bit bitwise op: ToInt32 both, run `op` on the exact BEAM
/// Ints, then wrap the RESULT back to int32 — the wrap belongs to the
/// combinator so `(1 << 31) | 0` is -2147483648, not 2147483648. Port of arc
/// `operators.gleam:390-398 int32_binop`.
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

/// ES2024 §13.12 `a & b`. BigInt: infinite two's-complement `band`
/// (§6.1.6.2.20); Number: ToInt32 both, `band`, wrap.
pub fn t_bitand(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  int32_binop(st, a, b, int.bitwise_and, int.bitwise_and)
}

/// ES2024 §13.12 `a | b`.
pub fn t_bitor(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  int32_binop(st, a, b, int.bitwise_or, int.bitwise_or)
}

/// ES2024 §13.12 `a ^ b`.
pub fn t_bitxor(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  int32_binop(st, a, b, int.bitwise_exclusive_or, int.bitwise_exclusive_or)
}

/// ES2024 §13.9.1 `a << b`. BigInt: arbitrary-precision `bsl` — Erlang accepts
/// negative counts (shifts other way), matching §6.1.6.2.9. Number: shift
/// count is `ToInt32(b) & 31` (§6.1.6.1.9).
pub fn t_shl(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  int32_binop(st, a, b, int.bitwise_shift_left, fn(x, y) {
    int.bitwise_shift_left(x, int.bitwise_and(y, 31))
  })
}

/// ES2024 §13.9.2 `a >> b` (arithmetic). Erlang `bsr` on a negative Int is
/// sign-propagating — exactly §6.1.6.1.10.
pub fn t_shr(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  int32_binop(st, a, b, int.bitwise_shift_right, fn(x, y) {
    int.bitwise_shift_right(x, int.bitwise_and(y, 31))
  })
}

/// ES2024 §13.9.3 `a >>> b`. The ONE bitwise op whose Number operand and
/// result are ToUint32 (§6.1.6.1.11), and whose BigInt arm always throws
/// (§6.1.6.2.11). Port of arc `operators.gleam:133-136, 296-297, 402-409`.
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

/// ES2024 §13.5.6 `~a`. BigInt: `-x - 1` (§6.1.6.2.2); Number: ToInt32 then
/// `bnot` — result is already in [-2^31, 2^31) so no wrap needed. Port of arc
/// `operators.gleam:211-216`.
pub fn t_bitnot(st: Agent, a: JsVal) -> #(JsVal, Agent) {
  let #(an, st) = rt_val.t_to_numeric(st, a)
  case classify(an) {
    KBig(x) -> #(mk_bigint(-1 - x), st)
    KNum(n) -> #(mk_number(JInt(int.bitwise_not(rt_val.num_to_int32(n)))), st)
    _ -> panic as "ToNumeric returned non-numeric"
  }
}

// ── §7.2.14 IsLooselyEqual / §7.2.15 IsStrictlyEqual ────────────────────────
// Port of arc `operators.gleam:309-363`.

/// ES2024 §7.2.15 IsStrictlyEqual (`===`). JPure per R8 — no `St` param;
/// object equality is Handle identity, so the store is never read. Thin
/// wrapper over `rt_val.strict_equal` (NaN≠NaN, +0===-0 already handled).
pub fn strict_eq(a: JsVal, b: JsVal) -> Bool {
  rt_val.strict_equal(a, b)
}

/// ES2024 §7.2.15 IsStrictlyEqual, negated (`!==`). JPure per R8.
pub fn strict_ne(a: JsVal, b: JsVal) -> Bool {
  case rt_val.strict_equal(a, b) {
    True -> False
    False -> True
  }
}

/// ES2024 §7.2.14 IsLooselyEqual (`==`). Threaded — the Object arm's
/// ToPrimitive re-enters JS (R1 tuple order). Port of arc
/// `operators.gleam:309-358` with the object-vs-primitive arm folded IN
/// (arc's caller-side `interpreter.is_eq_coercible` is inlined here).
pub fn t_eq(st: Agent, a: JsVal, b: JsVal) -> #(Int, Agent) {
  case classify(a), classify(b) {
    // Step 1: same Type → IsStrictlyEqual. Steps 2-3: null == undefined.
    KUndef, KUndef | KNull, KNull | KNull, KUndef | KUndef, KNull -> #(1, st)
    KBool(_), KBool(_)
    | KNum(_), KNum(_)
    | KStr(_), KStr(_)
    | KBig(_), KBig(_)
    | KSym(_), KSym(_)
    | KHandle(_), KHandle(_)
    -> #(bool_int(rt_val.strict_equal(a, b)), st)
    // Steps 9-10: a Boolean operand becomes ToNumber(bool), then redo. This
    // MUST precede the Object arm — {Bool, Object} is not in step 11/12's
    // operand set, so the bool coerces first and the recursion re-enters as
    // {Number, Object}.
    KBool(x), _ -> t_eq(st, mk_number(bool_to_jsnum(x)), b)
    _, KBool(y) -> t_eq(st, a, mk_number(bool_to_jsnum(y)))
    // Steps 11-12: Object vs {Number, String, BigInt, Symbol} →
    // ToPrimitive(object, default) then redo. null/undef vs Object falls
    // through to the final False (step 14 — not in the primitive set).
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
    // Steps 7-8: BigInt × String — StringToBigInt; unparseable → false.
    KBig(x), KStr(s) | KStr(s), KBig(x) ->
      case rt_val.string_to_bigint(s) {
        Some(y) -> #(bool_int(x == y), st)
        None -> #(0, st)
      }
    // Step 13: BigInt × Number — ℝ(x) = ℝ(y); NaN/±Infinity → false.
    KBig(x), KNum(n) | KNum(n), KBig(x) -> #(
      bool_int(bigint_equals_number(x, n)),
      st,
    )
    // Steps 5-6: Number × String — ToNumber(String) then Number strict-equal.
    // string_to_number is total (unparseable → NaN, and NaN === nothing).
    KNum(_), KStr(s) -> #(
      bool_int(rt_val.strict_equal(a, mk_number(rt_val.string_to_number(s)))),
      st,
    )
    KStr(s), KNum(_) -> #(
      bool_int(rt_val.strict_equal(mk_number(rt_val.string_to_number(s)), b)),
      st,
    )
    // Step 14: everything else (null/undef vs primitive, Sym vs Str, …).
    _, _ -> #(0, st)
  }
}

/// ES2024 §7.2.14 IsLooselyEqual, negated (`!=`).
pub fn t_neq(st: Agent, a: JsVal, b: JsVal) -> #(Int, Agent) {
  let #(r, st) = t_eq(st, a, b)
  #(1 - r, st)
}

/// §7.2.14 step 13: ℝ(BigInt) = ℝ(Number)? False for NaN/±Infinity and any
/// Number with a fractional part. Port of arc `operators.gleam:356-358` +
/// `compare_bigint_num:528-549` restricted to the Eq case.
fn bigint_equals_number(a: Int, n: JsNum) -> Bool {
  case n {
    JNan | JPosInf | JNegInf -> False
    JInt(i) -> a == i
    JFloat(f) ->
      // integral_int(f) = Some(i) iff f is an integral Number (§7.2.6) with
      // exact value i — floats ≥ 2^52 have no fractional bits so the Int
      // round-trip is exact; smaller integral floats fit exactly too.
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

// ── §13.5.4-6 unary +/- and §13.10.1 `in` (ops-unary-misc) ──────────────────
// Port of arc `operators.gleam:194-219` / `interpreter.gleam:2895-2921`.
// `typeof` is NOT re-exported here: SPEC.md:1676 dispatches `type_of` to
// `rt_val.t_type_of` directly (D4 — reads cell for callable check).

/// ES2024 §13.5.5 unary `-`. `? ToNumeric` (NOT ToNumber — a BigInt operand
/// is legal), then §6.1.6.2.1 BigInt::unaryMinus / §6.1.6.1.1 Number negate.
/// Port of arc `operators.gleam:200-204`.
pub fn t_neg(st: Agent, a: JsVal) -> #(JsVal, Agent) {
  let #(n, st) = rt_val.t_to_numeric(st, a)
  case classify(n) {
    KBig(x) -> #(mk_bigint(0 - x), st)
    KNum(x) -> #(mk_number(num_negate(x)), st)
    _ -> panic as "ToNumeric returned non-numeric"
  }
}

/// ES2024 §13.5.6 unary `+`. Exactly `? ToNumber(v)` — the ONE numeric
/// operator that rejects BigInt (the §7.1.4 ToNumber table throws TypeError
/// on it, produced inside `rt_val.t_to_number`). Port of arc
/// `operators.gleam:207-210`.
pub fn t_plus(st: Agent, a: JsVal) -> #(JsVal, Agent) {
  let #(n, st) = rt_val.t_to_number(st, a)
  #(mk_number(n), st)
}

/// ES2024 §13.10.1 RelationalExpression `key in obj`. Step 4: any non-Object
/// RHS is a TypeError (unlike `t_has_prop`, which auto-boxes primitive
/// receivers for `Reflect.has`-style callers). Step 5: `? ToPropertyKey`
/// runs AFTER the object check, so a Symbol/throwing key never coerces on
/// the error path. Port of arc `interpreter.gleam:2895-2921`.
pub fn t_in(st: Agent, key: JsVal, obj: JsVal) -> #(Int, Agent) {
  case classify(obj) {
    KHandle(_) -> {
      let #(pk, st) = rt_val.t_to_property_key(st, key)
      let #(found, st) = rt_obj.t_has_prop(st, obj, pk)
      #(bool_int(found), st)
    }
    _ -> {
      // No `inspect` in 2core (frozen modules); name the RHS by its typeof
      // tag — non-Handle `t_type_of` is a pure classify table.
      let #(tag, st) = rt_val.t_type_of(st, obj)
      rt_val.t_throw_type_error(
        st,
        "Cannot use 'in' operator to search for property in " <> tag,
      )
    }
  }
}

// ── JsNum×JsNum arithmetic kernels (ops-num-kernel) ─────────────────────────
// Port of arc `numeric.gleam:19-278` re-expressed over the five-way JsNum.
// `JInt,JInt` arms stay on exact bignum arithmetic; mixed `JInt/JFloat`
// widens the Int; every NaN/±Inf/±0 special case is an explicit arm. Only
// pow/fmod need FFI (BEAM `math:pow`/`math:fmod` badarith catch).

/// Sign of a `JsNum`, honouring the IEEE sign bit of a float zero (so
/// `1 / -0` resolves to `-Infinity`). `JNan` is never passed here.
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

/// Widen a known-finite `JsNum` (post NaN/Inf-arm dispatch) to a Float.
fn finite_to_float(n: JsNum) -> Float {
  case n {
    JInt(i) -> int.to_float(i)
    JFloat(f) -> f
    JNan | JPosInf | JNegInf -> panic as "finite_to_float on non-finite JsNum"
  }
}

/// §6.1.6.1.1 Number::unaryMinus. `JInt(0)` negates to `JInt(0)` (Int has no
/// -0; JS's -0 arises only from `float.negate(0.0)`). arc
/// `numeric.gleam:123-130`.
fn num_negate(n: JsNum) -> JsNum {
  case n {
    JNan -> JNan
    JPosInf -> JNegInf
    JNegInf -> JPosInf
    JInt(x) -> JInt(0 - x)
    JFloat(x) -> JFloat(float.negate(x))
  }
}

/// §6.1.6.1.7 Number::add. Port of arc `numeric.gleam:134-152`.
fn num_add(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    JNan, _ | _, JNan -> JNan
    JPosInf, JNegInf | JNegInf, JPosInf -> JNan
    JPosInf, _ | _, JPosInf -> JPosInf
    JNegInf, _ | _, JNegInf -> JNegInf
    JInt(x), JInt(y) -> JInt(x + y)
    JInt(x), JFloat(y) -> JFloat(int.to_float(x) +. y)
    JFloat(x), JInt(y) -> JFloat(x +. int.to_float(y))
    JFloat(x), JFloat(y) -> JFloat(x +. y)
  }
}

/// §6.1.6.1.8 Number::subtract. Port of arc `numeric.gleam:154-172`.
fn num_sub(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    JNan, _ | _, JNan -> JNan
    JPosInf, JPosInf | JNegInf, JNegInf -> JNan
    JPosInf, _ -> JPosInf
    JNegInf, _ -> JNegInf
    _, JPosInf -> JNegInf
    _, JNegInf -> JPosInf
    JInt(x), JInt(y) -> JInt(x - y)
    JInt(x), JFloat(y) -> JFloat(int.to_float(x) -. y)
    JFloat(x), JInt(y) -> JFloat(x -. int.to_float(y))
    JFloat(x), JFloat(y) -> JFloat(x -. y)
  }
}

/// ±∞ × `b`: ∞×0 is NaN; otherwise the operands' signs multiply.
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

/// §6.1.6.1.4 Number::multiply. Port of arc `numeric.gleam:174-198`.
fn num_mul(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    JNan, _ | _, JNan -> JNan
    JPosInf, _ -> inf_times(1, b)
    JNegInf, _ -> inf_times(-1, b)
    _, JPosInf -> inf_times(1, a)
    _, JNegInf -> inf_times(-1, a)
    JInt(x), JInt(y) -> JInt(x * y)
    JInt(x), JFloat(y) -> JFloat(int.to_float(x) *. y)
    JFloat(x), JInt(y) -> JFloat(x *. int.to_float(y))
    JFloat(x), JFloat(y) -> JFloat(x *. y)
  }
}

/// §6.1.6.1.5 Number::divide. Port of arc `numeric.gleam:200-238`.
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
        // JS `/` is always real division (7/2 → 3.5).
        False -> JFloat(finite_to_float(a) /. finite_to_float(b))
      }
  }
}

@external(erlang, "arc_rt_ops_ffi", "fmod_total")
fn fmod_total(a: Float, b: Float) -> JsNum

/// §6.1.6.1.6 Number::remainder — dividend-signed. Port of arc
/// `numeric.gleam:240-258`.
fn num_mod(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    JNan, _ | _, JNan -> JNan
    JPosInf, _ | JNegInf, _ -> JNan
    _, JPosInf | _, JNegInf -> a
    JInt(x), JInt(y) if y != 0 -> JInt(x % y)
    _, _ ->
      case is_zero(b) {
        True -> JNan
        False -> fmod_total(finite_to_float(a), finite_to_float(b))
      }
  }
}

@external(erlang, "arc_rt_ops_ffi", "pow_total")
fn pow_total(base: Float, exp: Float) -> JsNum

/// True iff `n` is an odd integral Number (§6.1.6.1.3's -∞/-0 branches).
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

/// `|base|` (finite) vs 1 → the ±∞-exponent result: >1→+∞, ==1→NaN, <1→+0.
fn abs_cmp_one(n: JsNum) -> JsNum {
  let af = float.absolute_value(finite_to_float(n))
  case af >. 1.0, af <. 1.0 {
    True, _ -> JPosInf
    _, True -> JFloat(0.0)
    False, False -> JNan
  }
}

/// Nonzero-exponent sign (the ±0 arm ran first).
fn exp_is_positive(exp: JsNum) -> Bool {
  case exp {
    JInt(e) -> e > 0
    JFloat(e) -> e >. 0.0
    JPosInf -> True
    JNegInf | JNan -> False
  }
}

/// §6.1.6.1.3 Number::exponentiate — the full special-case ladder. Port of
/// arc `numeric.gleam:260-278` + M5.md `npow`.
fn num_exp(base: JsNum, exp: JsNum) -> JsNum {
  case exp {
    JNan -> JNan
    // Any base (even NaN) to the ±0 is 1.
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

/// Both operands finite, `exp ≠ 0`. Handles the ±0 base and negative-base-
/// non-integral-exponent arms, then delegates to `math:pow`.
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
        // base < 0, exp not integral → NaN. pow_total handles the integral
        // case (including overflow → ±∞ by odd/even exponent parity).
        True ->
          case rt_val.integral_int(ef) {
            None -> JNan
            Some(_) -> pow_total(bf, ef)
          }
        False -> pow_total(bf, ef)
      }
  }
}

/// BigInt exponentiation by squaring. Precondition: `exp >= 0` (caller
/// throws RangeError on negative). Port of arc `operators.gleam:273-281`.
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

// ── §13.15.4 arithmetic operators (ops-arith) ───────────────────────────────
// ApplyStringOrNumericBinaryOperator. Shared spine per M5.md:47-54:
// ToPrimitive both LEFT-first (R1), then ToNumeric both, then dispatch on
// (KBig,KBig) / mixed → TypeError / (KNum,KNum). `t_add` alone has the
// pre-numeric string branch and uses `HintDefault`; the rest reuse the
// `to_numeric_operands` helper (HintNumber) shared with the bitwise ops.

/// ES2024 §13.8.1 `+`: string-concat if either primitive is a String, else
/// numeric addition. Port of arc `operators.gleam:24-70`.
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

/// ES2024 §13.8.2 `-`. Port of arc `operators.gleam:72-96`.
pub fn t_sub(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  let #(na, nb, st) = to_numeric_operands(st, a, b)
  case classify(na), classify(nb) {
    KBig(x), KBig(y) -> #(mk_bigint(x - y), st)
    KBig(_), _ | _, KBig(_) -> rt_val.t_throw_type_error(st, bigint_mix_error)
    KNum(x), KNum(y) -> #(mk_number(num_sub(x, y)), st)
    _, _ -> panic as "ToNumeric returned non-numeric"
  }
}

/// ES2024 §13.7 `*`. Port of arc `operators.gleam:98-122`.
pub fn t_mul(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  let #(na, nb, st) = to_numeric_operands(st, a, b)
  case classify(na), classify(nb) {
    KBig(x), KBig(y) -> #(mk_bigint(x * y), st)
    KBig(_), _ | _, KBig(_) -> rt_val.t_throw_type_error(st, bigint_mix_error)
    KNum(x), KNum(y) -> #(mk_number(num_mul(x, y)), st)
    _, _ -> panic as "ToNumeric returned non-numeric"
  }
}

/// ES2024 §13.7 `/`. BigInt ÷ 0 → RangeError. Port of arc
/// `operators.gleam:124-154`.
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

/// ES2024 §13.7 `%`. BigInt % 0 → RangeError. Port of arc
/// `operators.gleam:156-186`.
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

/// ES2024 §13.6 `**`. BigInt with negative exponent → RangeError. Port of
/// arc `operators.gleam:188-222`.
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
