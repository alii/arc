import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/key.{Named}
import arc/vm/limits
import arc/vm/ops/object
import arc/vm/ops/operators
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type JsValue, BigInt, Finite, FunctionObject, Infinity, JsBigInt, JsBool,
  JsNull, JsNumber, JsObject, JsString, JsSymbol, JsUndefined, JsUninitialized,
  NaN, NativeFunction, NegInfinity, ObjectSlot,
}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ============================================================================
// ToPrimitive / ToString with VM re-entry (ES2024 §7.1.1, §7.1.12)
// ============================================================================

pub type ToPrimitiveHint {
  StringHint
  NumberHint
  DefaultHint
}

/// ES2024 §7.1.1 ToPrimitive(input, preferredType)
/// For primitives, returns as-is. For objects, calls Symbol.toPrimitive
/// or falls back to OrdinaryToPrimitive.
pub fn to_primitive(
  state: State(host),
  val: JsValue,
  hint: ToPrimitiveHint,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case val {
    // Primitives pass through
    JsUndefined
    | JsNull
    | JsBool(_)
    | JsNumber(_)
    | JsString(_)
    | JsSymbol(_)
    | JsBigInt(_)
    | JsUninitialized -> Ok(#(val, state))
    // Objects: try Symbol.toPrimitive, then OrdinaryToPrimitive
    JsObject(ref) -> {
      // §7.1.1 step 1.a: check @@toPrimitive
      use #(exotic_fn, state) <- result.try(object.get_symbol_value(
        state,
        ref,
        value.symbol_to_primitive,
        val,
      ))
      case exotic_fn {
        // @@toPrimitive not found (GetMethod treats undefined and null the
        // same) → fall through to OrdinaryToPrimitive
        JsUndefined | JsNull -> ordinary_to_primitive(state, val, ref, hint)
        _ ->
          case helpers.is_callable(state.heap, exotic_fn) {
            True -> {
              let hint_str = case hint {
                StringHint -> "string"
                NumberHint -> "number"
                DefaultHint -> "default"
              }
              use #(result, new_state) <- result.try(
                state.call(state, exotic_fn, val, [JsString(hint_str)]),
              )
              case result {
                JsObject(_) ->
                  thrown_type_error(
                    new_state,
                    "Cannot convert object to primitive value",
                  )
                _ -> Ok(#(result, new_state))
              }
            }
            False -> thrown_type_error(state, "@@toPrimitive is not callable")
          }
      }
    }
  }
}

/// ES2024 §7.1.1.1 OrdinaryToPrimitive(O, hint)
/// Tries toString/valueOf (or valueOf/toString for number hint).
pub fn ordinary_to_primitive(
  state: State(host),
  val: JsValue,
  ref: value.Ref,
  hint: ToPrimitiveHint,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  let method_names = case hint {
    StringHint -> ["toString", "valueOf"]
    NumberHint | DefaultHint -> ["valueOf", "toString"]
  }
  try_to_primitive_methods(state, val, ref, method_names)
}

/// Try each method name in order; return the first primitive result.
fn try_to_primitive_methods(
  state: State(host),
  val: JsValue,
  ref: value.Ref,
  method_names: List(String),
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case method_names {
    [] -> thrown_type_error(state, "Cannot convert object to primitive value")
    [name, ..rest] -> {
      use #(method, state) <- result.try(object.get_value(
        state,
        ref,
        Named(name),
        val,
      ))
      case helpers.is_callable(state.heap, method) {
        True -> {
          use #(result, new_state) <- result.try(
            state.call(state, method, val, []),
          )
          case result {
            JsObject(_) -> try_to_primitive_methods(new_state, val, ref, rest)
            _ -> Ok(#(result, new_state))
          }
        }
        False -> try_to_primitive_methods(state, val, ref, rest)
      }
    }
  }
}

/// ES2024 §7.1.12 ToString with VM re-entry for ToPrimitive.
/// For primitives, converts directly. For objects, calls ToPrimitive(string) first.
pub fn js_to_string(
  state: State(host),
  val: JsValue,
) -> Result(#(String, State(host)), #(JsValue, State(host))) {
  case val {
    JsObject(_) -> {
      use #(prim, new_state) <- result.try(to_primitive(state, val, StringHint))
      js_to_string(new_state, prim)
    }
    JsSymbol(_) ->
      thrown_type_error(state, "Cannot convert a Symbol value to a string")
    JsString(s) -> Ok(#(s, state))
    JsNumber(Finite(n)) -> Ok(#(value.js_format_number(n), state))
    JsNumber(NaN) -> Ok(#("NaN", state))
    JsNumber(Infinity) -> Ok(#("Infinity", state))
    JsNumber(NegInfinity) -> Ok(#("-Infinity", state))
    JsBool(True) -> Ok(#("true", state))
    JsBool(False) -> Ok(#("false", state))
    JsNull -> Ok(#("null", state))
    JsUndefined -> Ok(#("undefined", state))
    JsUninitialized -> Ok(#("undefined", state))
    JsBigInt(BigInt(n)) -> Ok(#(int.to_string(n), state))
  }
}

/// CPS wrapper for js_to_string. Use with `use` syntax:
///   use str, state <- coerce.try_to_string(state, val)
pub fn try_to_string(
  state: State(host),
  val: JsValue,
  cont: fn(String, State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
  case js_to_string(state, val) {
    Ok(#(str, state)) -> cont(str, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// ES2024 §7.1.4 ToNumber with VM re-entry for ToPrimitive.
/// For primitives, converts directly. For objects, calls ToPrimitive(number) first.
pub fn js_to_number(
  state: State(host),
  val: JsValue,
) -> Result(#(value.JsNum, State(host)), #(JsValue, State(host))) {
  case val {
    JsObject(_) -> {
      use #(prim, state) <- result.try(to_primitive(state, val, NumberHint))
      js_to_number(state, prim)
    }
    other ->
      case value.to_number(other) {
        Ok(n) -> Ok(#(n, state))
        Error(msg) -> thrown_type_error(state, msg)
      }
  }
}

/// CPS wrapper for js_to_number. Use with `use` syntax:
///   use n, state <- coerce.try_to_number(state, val)
pub fn try_to_number(
  state: State(host),
  val: JsValue,
  cont: fn(value.JsNum, State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
  case js_to_number(state, val) {
    Ok(#(n, state)) -> cont(n, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

// ============================================================================
// ToIntegerOrInfinity (ES2024 §7.1.5)
// ============================================================================

/// ES2024 §7.1.5 ToIntegerOrInfinity of an already-ToNumber'd value, with
/// ±∞ saturated to ±(2^53 - 1) (`limits.max_safe_integer`, the spec cap on
/// array-like lengths). This is the canonical §7.1.5 / saturation helper;
/// prefer it over hand-rolling the JsNum match. (typed_array's
/// `to_int_or_inf` is the one remaining pre-existing local copy — it keeps
/// the ±∞ distinction its callers still need.)
///   step 2: NaN, +0, -0 → 0
///   steps 3-4: +∞ → 2^53 - 1, -∞ → -(2^53 - 1)
///   step 5: finite → truncate toward zero
/// Because every string/array length is < 2^53 - 1, the downstream
/// `int.clamp`/`int.min`/`int.max` a caller applies behaves exactly like the
/// spec's explicit "+∞ → len / -∞ → 0" branches — and range guards (e.g.
/// toString's `radix < 2 || radix > 36`) actually SEE an out-of-range value
/// for ±∞ instead of a caller-invented default.
///
/// The one thing saturation erases is the ±∞ itself. The rare callers for
/// which +∞/-∞ is directly observable (Atomics stores return it; typed-array
/// element conversion maps non-finite to +0) must match on the `value.JsNum`
/// from `try_to_number` before saturating.
pub fn jsnum_to_integer_or_infinity(num: value.JsNum) -> Int {
  case num {
    NaN -> 0
    Finite(f) -> value.float_to_int(f)
    Infinity -> limits.max_safe_integer
    NegInfinity -> -limits.max_safe_integer
  }
}

/// CPS ToIntegerOrInfinity (ES2024 §7.1.5): full ToNumber — including
/// ToPrimitive (valueOf/toString/@@toPrimitive) on objects, which can run
/// user code or throw, and TypeError on Symbol/BigInt — then
/// `jsnum_to_integer_or_infinity`. Use with `use` syntax:
///   use i, state <- coerce.try_to_integer_or_infinity(state, val)
pub fn try_to_integer_or_infinity(
  state: State(host),
  val: JsValue,
  cont: fn(Int, State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
  use num, state <- try_to_number(state, val)
  cont(jsnum_to_integer_or_infinity(num), state)
}

// ============================================================================
// ToInt32 / ToUint32 (ES2024 §7.1.6 / §7.1.7)
// ============================================================================

/// ES2024 §7.1.6 ToInt32 of an already-ToNumber'd value. Delegates to the
/// single modular-reduction implementation in `operators` (which the bitwise
/// operators use directly on the hot path).
pub fn jsnum_to_int32(num: value.JsNum) -> Int {
  operators.num_to_int32(num)
}

/// ES2024 §7.1.7 ToUint32 of an already-ToNumber'd value (see
/// `jsnum_to_int32`).
pub fn jsnum_to_uint32(num: value.JsNum) -> Int {
  operators.num_to_uint32(num)
}

/// CPS ToInt32 (ES2024 §7.1.6): full ToNumber (ToPrimitive on objects,
/// TypeError on Symbol/BigInt), then modular reduction to [-2^31, 2^31).
///   use i, state <- coerce.try_to_int32(state, val)
pub fn try_to_int32(
  state: State(host),
  val: JsValue,
  cont: fn(Int, State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
  use num, state <- try_to_number(state, val)
  cont(operators.num_to_int32(num), state)
}

/// CPS ToUint32 (ES2024 §7.1.7): full ToNumber, then modular reduction to
/// [0, 2^32).
///   use u, state <- coerce.try_to_uint32(state, val)
pub fn try_to_uint32(
  state: State(host),
  val: JsValue,
  cont: fn(Int, State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
  use num, state <- try_to_number(state, val)
  cont(operators.num_to_uint32(num), state)
}

// ============================================================================
// ToLength (ES2024 §7.1.20)
// ============================================================================

/// ES2024 §7.1.20 ToLength of an already-ToNumber'd value: ToIntegerOrInfinity
/// clamped to [0, 2^53 - 1]. Never throws.
pub fn jsnum_to_length(num: value.JsNum) -> Int {
  int.clamp(jsnum_to_integer_or_infinity(num), 0, limits.max_safe_integer)
}

/// CPS ToLength (ES2024 §7.1.20): full ToNumber — including ToPrimitive
/// (valueOf may run user code or throw) — then `jsnum_to_length`.
///   use len, state <- coerce.try_to_length(state, val)
pub fn try_to_length(
  state: State(host),
  val: JsValue,
  cont: fn(Int, State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
  use num, state <- try_to_number(state, val)
  cont(jsnum_to_length(num), state)
}

/// Relative start/end index resolution shared by the Array / ArrayBuffer
/// slice-family methods (§23.1.3.x, §25.1.6.x common steps; TypedArray still
/// uses its own private `relative_index` in typed_array.gleam):
///
///   Let relativeIndex be ? ToIntegerOrInfinity(val).
///   If relativeIndex = -∞, k = 0.
///   Else if relativeIndex < 0, k = max(len + relativeIndex, 0).
///   Else, k = min(relativeIndex, len).
///
/// `default` is used when `val` is undefined (the spec gives different
/// defaults per position: 0 for start, `len` for end). ToNumber(undefined)
/// has no observable steps, so skipping the coercion there is
/// spec-equivalent.
pub fn try_relative_index(
  state: State(host),
  val: JsValue,
  len: Int,
  default: Int,
  cont: fn(Int, State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
  case val {
    JsUndefined -> cont(default, state)
    _ -> {
      use raw, state <- try_to_integer_or_infinity(state, val)
      let k = case raw < 0 {
        True -> int.max(len + raw, 0)
        False -> int.min(raw, len)
      }
      cont(k, state)
    }
  }
}

/// Fast-path unwrap of primitive wrapper objects (Number/String/Boolean) to
/// their [[PrimitiveValue]]. Returns `val` unchanged for any other input.
/// Unlike full ToPrimitive this does NOT honor user-overridden valueOf or
/// @@toPrimitive — use js_to_number/js_to_string for spec-exact coercion.
pub fn unwrap_primitive_wrapper(h: Heap(host), val: JsValue) -> JsValue {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: value.NumberObject(value: n), ..)) -> JsNumber(n)
        Some(ObjectSlot(kind: value.StringObject(value: s), ..)) -> JsString(s)
        Some(ObjectSlot(kind: value.BooleanObject(value: b), ..)) -> JsBool(b)
        _ -> val
      }
    other -> other
  }
}

/// ES2024 §13.10.2 InstanceofOperator ( V, target )
pub fn js_instanceof(
  state: State(host),
  left: JsValue,
  constructor: JsValue,
) -> Result(#(Bool, State(host)), #(JsValue, State(host))) {
  case constructor {
    // Step 1: target must be an Object.
    JsObject(ctor_ref) ->
      case heap.read(state.heap, ctor_ref) {
        // Step 4: IsCallable(target) — we check for function slot kinds
        // (including callable proxies, §10.5.15).
        Some(ObjectSlot(kind: FunctionObject(..), ..))
        | Some(ObjectSlot(kind: NativeFunction(..), ..))
        | Some(ObjectSlot(kind: value.ProxyObject(callable: True, ..), ..)) ->
          // Step 5: OrdinaryHasInstance(target, V).
          ordinary_has_instance(state, ctor_ref, left)
        // Step 4: Not callable → TypeError.
        _ ->
          thrown_type_error(
            state,
            "Right-hand side of instanceof is not callable",
          )
      }
    // Step 1: Not an Object → TypeError.
    _ ->
      thrown_type_error(state, "Right-hand side of instanceof is not callable")
  }
}

/// ES2024 §7.3.22 OrdinaryHasInstance ( C, O ), steps 2-7. The caller has
/// already verified that `ctor_ref` is callable (step 1).
pub fn ordinary_has_instance(
  state: State(host),
  ctor_ref: value.Ref,
  left: JsValue,
) -> Result(#(Bool, State(host)), #(JsValue, State(host))) {
  case heap.read(state.heap, ctor_ref) {
    // Step 2: C has a [[BoundTargetFunction]] internal slot →
    // return InstanceofOperator(O, BC).
    Some(ObjectSlot(
      kind: NativeFunction(value.Call(value.BoundFunction(target:, ..)), ..),
      ..,
    )) -> js_instanceof(state, left, JsObject(target))
    _ ->
      // Step 3: If O is not an Object, return false (before the Get —
      // a throwing "prototype" getter must NOT fire for primitives).
      case left {
        JsObject(obj_ref) -> {
          // Step 4: Let P be ? Get(C, "prototype").
          use #(proto_val, state) <- result.try(object.get_value(
            state,
            ctor_ref,
            Named("prototype"),
            JsObject(ctor_ref),
          ))
          case proto_val {
            JsObject(proto_ref) ->
              // Step 7: prototype chain walk — stateful so a proxy on
              // the chain fires its getPrototypeOf trap (§10.5.1).
              instanceof_walk(state, obj_ref, proto_ref)
            _ ->
              // Step 5: If P is not an Object, throw TypeError.
              thrown_type_error(
                state,
                "Function has non-object prototype in instanceof check",
              )
          }
        }
        _ -> Ok(#(False, state))
      }
  }
}

/// ES2024 §7.3.22 OrdinaryHasInstance ( C, O ) — step 7 (prototype chain
/// walk). Stateful: each level goes through [[GetPrototypeOf]], which traps
/// (and may throw) for proxies on the chain.
fn instanceof_walk(
  state: State(host),
  obj_ref: value.Ref,
  target_proto: value.Ref,
) -> Result(#(Bool, State(host)), #(JsValue, State(host))) {
  // Step 6a: Let O be ? O.[[GetPrototypeOf]]().
  use #(proto_val, state) <- result.try(object.get_prototype_of_stateful(
    state,
    obj_ref,
  ))
  case proto_val {
    JsObject(proto_ref) ->
      // Step 6c: SameValue(P, O) — compare by ref identity.
      case proto_ref.id == target_proto.id {
        True -> Ok(#(True, state))
        // Step 6: Repeat — walk up the chain.
        False -> instanceof_walk(state, proto_ref, target_proto)
      }
    // Step 6b: O is null (no prototype) → return false.
    _ -> Ok(#(False, state))
  }
}

/// Helper to throw a TypeError in functions that return Result(a, #(JsValue, State)).
/// Used by toPrimitive, toString, instanceof, etc.
pub fn thrown_type_error(
  state: State(host),
  msg: String,
) -> Result(a, #(JsValue, State(host))) {
  Error(state.type_error_value(state, msg))
}

/// §7.1.13 ToBigInt: ToPrimitive(number hint), then BigInt/Boolean/String per
/// the conversion table; Number/Symbol/null/undefined → TypeError, and a
/// string StringToBigInt rejects → SyntaxError. This is THE ToBigInt — the
/// CPS wrapper below and the `ctx.to_bigint_fn` hook (typed-array element
/// stores) both route here.
pub fn to_bigint(
  state: State(host),
  val: JsValue,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  use #(prim, state) <- result.try(to_primitive(state, val, NumberHint))
  case prim {
    JsBigInt(BigInt(n)) -> Ok(#(n, state))
    JsBool(True) -> Ok(#(1, state))
    JsBool(False) -> Ok(#(0, state))
    JsString(s) ->
      case string_to_bigint(s) {
        Some(n) -> Ok(#(n, state))
        // §7.1.13: StringToBigInt returning undefined throws a SyntaxError
        // (not TypeError — that's for Number/Symbol/null/undefined).
        None ->
          Error(state.syntax_error_value(
            state,
            "Cannot convert " <> s <> " to a BigInt",
          ))
      }
    JsNumber(_) ->
      thrown_type_error(state, "Cannot convert a Number to a BigInt")
    JsSymbol(_) ->
      thrown_type_error(state, "Cannot convert a Symbol to a BigInt")
    JsNull -> thrown_type_error(state, "Cannot convert null to a BigInt")
    _ -> thrown_type_error(state, "Cannot convert undefined to a BigInt")
  }
}

/// CPS wrapper for to_bigint. Use with `use` syntax:
///   use n, state <- coerce.to_bigint_cps(state, val)
pub fn to_bigint_cps(
  state: State(host),
  val: JsValue,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case to_bigint(state, val) {
    Ok(#(n, state)) -> cont(n, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// §7.1.22 ToIndex ( value ):
///
///   1. Let integer be ? ToIntegerOrInfinity(value).
///   2. If integer is not in the inclusive interval from 0 to 2^53 - 1,
///      throw a RangeError exception (with the caller-supplied `err_msg`).
///
/// undefined → 0 (ToNumber(undefined) is NaN and has no observable steps).
pub fn to_index(
  state: State(host),
  val: JsValue,
  err_msg: String,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  case val {
    JsUndefined -> Ok(#(0, state))
    _ -> {
      use #(num, state) <- result.try(js_to_number(state, val))
      case num {
        Finite(f) -> {
          let i = value.float_to_int(f)
          case i < 0 || i > limits.max_safe_integer {
            True -> Error(state.range_error_value(state, err_msg))
            False -> Ok(#(i, state))
          }
        }
        NaN -> Ok(#(0, state))
        Infinity | NegInfinity -> Error(state.range_error_value(state, err_msg))
      }
    }
  }
}

/// CPS wrapper for `to_index` (§7.1.22). Use with `use` syntax:
///   use i, state <- coerce.to_index_cps(state, val, "Invalid ... length")
pub fn to_index_cps(
  state: State(host),
  val: JsValue,
  err_msg: String,
  cont: fn(Int, State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
  case to_index(state, val, err_msg) {
    Ok(#(i, state)) -> cont(i, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// §7.1.14 StringToBigInt — decimal (with sign) or 0x/0o/0b prefixed;
/// empty/whitespace-only → 0; anything else fails.
pub fn string_to_bigint(s: String) -> Option(Int) {
  let s = string.trim(s)
  case s {
    "" -> Some(0)
    "0x" <> rest | "0X" <> rest -> parse_radix_digits(rest, 16)
    "0o" <> rest | "0O" <> rest -> parse_radix_digits(rest, 8)
    "0b" <> rest | "0B" <> rest -> parse_radix_digits(rest, 2)
    _ -> int.parse(s) |> option.from_result
  }
}

/// Parse the digits after a 0x/0o/0b prefix. The grammar (§7.1.14
/// NonDecimalIntegerLiteral) has no SignedInteger, so a sign here is a
/// syntax error even though int.base_parse would accept it.
fn parse_radix_digits(digits: String, base: Int) -> Option(Int) {
  case digits {
    "-" <> _ | "+" <> _ -> None
    _ -> int.base_parse(digits, base) |> option.from_result
  }
}
