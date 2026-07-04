/// ES2024 §21.2 BigInt Objects
///
/// The `BigInt` global function (§21.2.1.1) — callable, not constructible —
/// plus %BigInt.prototype% (§21.2.3) with toString/toLocaleString/valueOf and
/// @@toStringTag "BigInt". BigInt wrapper objects (`Object(1n)`) carry a
/// `value.BigIntObject` ExoticKind holding the [[BigIntData]] slot; there is
/// no `new BigInt` path.
///
/// The BigInt <-> TypedArray element conversions (BigInt64Array etc.) live in
/// builtins/typed_array.gleam next to their consumers.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/ops/coerce
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type JsValue, type Ref, BigIntGlobal, BigIntPrototypeToLocaleString,
  BigIntPrototypeToString, BigIntPrototypeValueOf, Dispatch, Finite, JsNumber,
  JsObject, JsString, JsUndefined, VmNative,
}
import gleam/int
import gleam/option.{None, Some}
import gleam/result

// ============================================================================
// Init — the BigInt global function and %BigInt.prototype%
// ============================================================================

/// Set up %BigInt.prototype% (§21.2.3) and the BigInt global function
/// (§21.2.1.1). `init_type_on` installs the constructor's `prototype`
/// property { W:F, E:F, C:F } (§21.2.2.3) and %BigInt.prototype%.constructor;
/// `constructible: False` because `new BigInt` throws a TypeError.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  // %BigInt.prototype% methods (§21.2.3.3-4). Primitive BigInt property
  // access delegates here.
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("toString", VmNative(BigIntPrototypeToString), 0),
      #("toLocaleString", VmNative(BigIntPrototypeToLocaleString), 0),
      #("valueOf", VmNative(BigIntPrototypeValueOf), 0),
    ])
  // §21.2.3.5 %BigInt.prototype%[@@toStringTag] = "BigInt".
  let #(h, proto) =
    common.init_namespace(h, object_proto, "BigInt", proto_methods)
  common.init_type_on(
    h,
    proto,
    function_proto,
    [],
    fn(_) { Dispatch(VmNative(BigIntGlobal)) },
    "BigInt",
    1,
    [],
    False,
  )
}

// ============================================================================
// BigInt ( value ) — §21.2.1.1
// ============================================================================

pub fn bigint_global(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let arg = helpers.first_arg_or_undefined(args)
  wrap({
    // Step 2: prim = ToPrimitive(value, number).
    use #(prim, state) <- result.try(coerce.to_primitive(
      state,
      arg,
      coerce.NumberHint,
    ))
    case prim {
      // Step 3: Number → NumberToBigInt (RangeError unless integral).
      JsNumber(n) ->
        case n {
          // §4.4.31 IsIntegralNumber via value.integral_int, which is ±0-safe:
          // the naive `int.to_float(i) == f` compiles to BEAM `=:=`, and
          // `0.0 =:= -0.0` is False, so `BigInt(-0)` used to RangeError
          // instead of yielding 0n.
          Finite(f) ->
            case value.integral_int(f) {
              Some(i) -> Ok(#(value.JsBigInt(value.BigInt(i)), state))
              None ->
                Error(state.range_error_value(
                  state,
                  "The number "
                    <> value.js_format_number(f)
                    <> " cannot be converted to a BigInt because it is not an integer",
                ))
            }
          _ ->
            Error(state.range_error_value(
              state,
              "The number cannot be converted to a BigInt because it is not an integer",
            ))
        }
      // Step 4: otherwise ToBigInt(prim).
      _ -> {
        use #(n, state) <- result.map(coerce.to_bigint(state, prim))
        #(value.JsBigInt(value.BigInt(n)), state)
      }
    }
  })
}

// ============================================================================
// %BigInt.prototype% methods — §21.2.3
// ============================================================================

/// §21.2.3 "thisBigIntValue" — a BigInt primitive, or a BigInt wrapper
/// object (kind `BigIntObject`, allocated by ToObject) carrying the
/// [[BigIntData]] slot.
fn this_bigint_value(
  state: State(host),
  this: JsValue,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  let incompatible = fn() {
    Error(state.type_error_value(
      state,
      "BigInt.prototype method called on incompatible receiver",
    ))
  }
  case this {
    value.JsBigInt(value.BigInt(n)) -> Ok(#(n, state))
    JsObject(ref) ->
      case heap.read_bigint_object(state.heap, ref) {
        Some(value.BigInt(n)) -> Ok(#(n, state))
        None -> incompatible()
      }
    _ -> incompatible()
  }
}

/// §21.2.3.3 BigInt.prototype.toString ( [ radix ] )
pub fn bigint_proto_to_string(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  wrap({
    // Step 1: x = ? thisBigIntValue(this value).
    use #(n, state) <- result.try(this_bigint_value(state, this))
    // Step 2: radixMV = ToIntegerOrInfinity(radix); undefined → 10.
    // (±∞ saturate to ±MAX_SAFE_INTEGER, which step 3 then rejects.)
    let radix_arg = helpers.first_arg_or_undefined(args)
    use #(radix, state) <- result.try(case radix_arg {
      JsUndefined -> Ok(#(10, state))
      _ -> {
        use #(num, state) <- result.map(coerce.js_to_number(state, radix_arg))
        #(coerce.jsnum_to_integer_or_infinity(num), state)
      }
    })
    // Step 3: radixMV outside 2..36 → RangeError. Steps 4-5:
    // BigInt::toString(x, radix) — lowercase digits, no suffix.
    case value.radix(radix) {
      Ok(r) -> Ok(#(JsString(value.format_bigint_radix(n, r)), state))
      Error(Nil) ->
        Error(state.range_error_value(
          state,
          "toString() radix must be between 2 and 36",
        ))
    }
  })
}

/// §21.2.3.2 BigInt.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
///
/// NOT an alias of toString: its first argument is `locales` (ECMA-402
/// §18.3.1), never a radix. This is the no-Intl fallback — both arguments are
/// ignored and the value is rendered in decimal. When ECMA-402 is present its
/// `init` overwrites this slot with the Intl.NumberFormat-backed override,
/// which does honour `locales`/`options`.
pub fn bigint_proto_to_locale_string(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  wrap({
    use #(n, state) <- result.map(this_bigint_value(state, this))
    #(JsString(int.to_string(n)), state)
  })
}

/// §21.2.3.4 BigInt.prototype.valueOf ( )
pub fn bigint_proto_value_of(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  wrap({
    use #(n, state) <- result.try(this_bigint_value(state, this))
    Ok(#(value.JsBigInt(value.BigInt(n)), state))
  })
}

// ============================================================================
// Local helpers
// ============================================================================

/// Adapt internal Result style to the builtin dispatch tuple shape.
fn wrap(
  r: Result(#(JsValue, State(host)), #(JsValue, State(host))),
) -> #(State(host), Result(JsValue, JsValue)) {
  case r {
    Ok(#(v, state)) -> #(state, Ok(v))
    Error(#(e, state)) -> #(state, Error(e))
  }
}
