import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type State}
import arc/vm/value.{
  type JsValue, type PropertyKey, Finite, Index, JsNumber, JsObject, JsString,
  Named,
}
import gleam/float
import gleam/int
import gleam/result

// ============================================================================
// Computed property access helpers
// ============================================================================

/// ToPropertyKey (§7.1.19) for non-symbol keys — spec is ToPrimitive(hint
/// string) then ToString. We canonicalize to a PropertyKey ONCE so downstream
/// [[Get]]/[[Set]] don't round-trip through strings: array-index numbers →
/// Index(n) (skip stringify), strings → canonical_key, else ToString → canonical_key.
pub fn to_property_key(
  state: State,
  key: JsValue,
) -> Result(#(PropertyKey, State), #(JsValue, State)) {
  case key {
    JsNumber(Finite(n)) -> {
      // +. 0.0 normalizes -0.0 → +0.0 (BEAM =:= distinguishes them)
      let n = n +. 0.0
      let i = float.truncate(n)
      // Same [0, 2^32-1) array-index cap as value.canonical_key — numbers
      // beyond it (e.g. 4294967296) must stringify to Named so the numeric
      // and string forms of the same key land in the same dict slot.
      case int.to_float(i) == n && i >= 0 && i <= 4_294_967_294 {
        // Valid array index — skip stringification entirely.
        True -> Ok(#(Index(i), state))
        // Non-index number — stringify (e.g. 1.5 → "1.5", -1 → "-1").
        False -> Ok(#(Named(value.js_format_number(n)), state))
      }
    }
    JsNumber(value.NaN) -> Ok(#(Named("NaN"), state))
    JsNumber(value.Infinity) -> Ok(#(Named("Infinity"), state))
    JsNumber(value.NegInfinity) -> Ok(#(Named("-Infinity"), state))
    JsString(s) -> Ok(#(value.canonical_key(s), state))
    _ -> {
      use #(s, state) <- result.map(coerce.js_to_string(state, key))
      #(value.canonical_key(s), state)
    }
  }
}

/// [[Get]] with a JsValue key — ToPropertyKey (§7.1.19) then delegate to
/// the single [[Get]] implementation. The elements/properties storage split
/// is handled by get_own_property; this layer doesn't know about it.
pub fn get_elem_value(
  state: State,
  ref: value.Ref,
  key: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case key {
    value.JsSymbol(sym_id) ->
      object.get_symbol_value(state, ref, sym_id, JsObject(ref))
    _ -> {
      use #(pk, state) <- result.try(to_property_key(state, key))
      object.get_value(state, ref, pk, JsObject(ref))
    }
  }
}

/// [[Set]] with a JsValue key — ToPropertyKey (§7.1.19) then delegate to
/// the single [[Set]] implementation. set_value handles setter invocation,
/// proto-walk, and element storage for Index keys on arrays.
/// Returns the [[Set]] success flag so strict-mode callers can throw
/// TypeError on failure (§13.15.2 PutValue step 6.b.iv).
pub fn put_elem_value(
  state: State,
  ref: value.Ref,
  key: JsValue,
  val: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  let receiver = JsObject(ref)
  case key {
    value.JsSymbol(sym_id) ->
      object.set_symbol_value(state, ref, sym_id, val, receiver)
    _ -> {
      use #(pk, state) <- result.try(to_property_key(state, key))
      object.set_value(state, ref, pk, val, receiver)
    }
  }
}
