import arc/vm/key.{Index, Named, max_array_index}
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object.{type PropKey, PkString, PkSymbol}
import arc/vm/state.{type State}
import arc/vm/value.{
  type JsValue, Finite, JsNumber, JsObject, JsString, JsSymbol,
}
import gleam/float
import gleam/int
import gleam/list
import gleam/result

// ============================================================================
// ToPropertyKey — ES2024 §7.1.19
// ============================================================================

/// ToPropertyKey ( argument ) — ES2024 §7.1.19. THE canonical implementation.
///
///   1. Let key be ? ToPrimitive(argument, string).
///   2. If key is a Symbol, return key.
///   3. Return ! ToString(key).
///
/// Objects go through ToPrimitive FIRST and the *result* is re-dispatched, so
/// a `@@toPrimitive` that returns a Symbol yields `PkSymbol` (not a bogus
/// "Cannot convert a Symbol value to a string" TypeError). Strings/numbers
/// take the fast paths below — ToPrimitive is the identity on them.
///
/// The string result is canonicalized to a PropertyKey ONCE so downstream
/// [[Get]]/[[Set]] don't round-trip through strings: array-index numbers →
/// Index(n) (skip stringify), strings → canonical_key, else ToString →
/// canonical_key.
pub fn to_prop_key(
  state: State(host),
  key: JsValue,
) -> Result(#(PropKey, State(host)), #(JsValue, State(host))) {
  case key {
    // Step 1: ToPrimitive is only observable for objects. Step 2's Symbol
    // check must run on the *primitive*, so re-dispatch on the result.
    JsObject(_) -> {
      use #(prim, state) <- result.try(coerce.to_primitive(
        state,
        key,
        coerce.StringHint,
      ))
      primitive_to_prop_key(state, prim)
    }
    _ -> primitive_to_prop_key(state, key)
  }
}

/// Steps 2-3 of §7.1.19 for an already-primitive key.
fn primitive_to_prop_key(
  state: State(host),
  key: JsValue,
) -> Result(#(PropKey, State(host)), #(JsValue, State(host))) {
  case key {
    // Step 2: If key is a Symbol, return key.
    JsSymbol(sym) -> Ok(#(PkSymbol(sym), state))
    JsNumber(Finite(n)) -> {
      // +. 0.0 normalizes -0.0 → +0.0 (BEAM =:= distinguishes them)
      let n = n +. 0.0
      let i = float.truncate(n)
      // Same [0, 2^32-1) array-index cap as key.canonical_key — numbers
      // beyond it (e.g. 4294967296) must stringify to Named so the numeric
      // and string forms of the same key land in the same dict slot.
      case int.to_float(i) == n && i >= 0 && i <= max_array_index {
        // Valid array index — skip stringification entirely.
        True -> Ok(#(PkString(Index(i)), state))
        // Non-index number — stringify (e.g. 1.5 → "1.5", -1 → "-1").
        False -> Ok(#(PkString(Named(value.js_format_number(n))), state))
      }
    }
    JsNumber(value.NaN) -> Ok(#(PkString(Named("NaN")), state))
    JsNumber(value.Infinity) -> Ok(#(PkString(Named("Infinity")), state))
    JsNumber(value.NegInfinity) -> Ok(#(PkString(Named("-Infinity")), state))
    JsString(s) -> Ok(#(PkString(key.canonical_key(s)), state))
    // Step 3: ToString(key) — key is a non-Symbol primitive, cannot re-enter.
    _ -> {
      use #(s, state) <- result.map(coerce.js_to_string(state, key))
      #(PkString(key.canonical_key(s)), state)
    }
  }
}

// ============================================================================
// Computed property access helpers
// ============================================================================

/// [[Get]] with a JsValue key — ToPropertyKey (§7.1.19) then delegate to
/// the single [[Get]] implementation. The elements/properties storage split
/// is handled by get_own_property; this layer doesn't know about it.
pub fn get_elem_value(
  state: State(host),
  ref: value.Ref,
  key: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  use #(pk, state) <- result.try(to_prop_key(state, key))
  object.get_prop_value(state, ref, pk, JsObject(ref))
}

/// [[Set]] with a JsValue key — ToPropertyKey (§7.1.19) then delegate to
/// the single [[Set]] implementation. set_value handles setter invocation,
/// proto-walk, and element storage for Index keys on arrays.
/// Returns the [[Set]] success flag so strict-mode callers can throw
/// TypeError on failure (§13.15.2 PutValue step 6.b.iv).
pub fn put_elem_value(
  state: State(host),
  ref: value.Ref,
  key: JsValue,
  val: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  use #(pk, state) <- result.try(to_prop_key(state, key))
  object.set_prop_value(state, ref, pk, val, JsObject(ref))
}

// ============================================================================
// CreateListFromArrayLike — ES2024 §7.3.19
// ============================================================================

/// CreateListFromArrayLike ( obj ) — ES2024 §7.3.19.
///
///   1. If obj is not an Object, throw a TypeError exception.
///   3. Let len be ? LengthOfArrayLike(obj).
///   5-7. For each index < len, append ? Get(obj, ! ToString(index)).
///
/// Stateful: the "length" and element reads go through [[Get]] (accessor
/// properties, proxy traps, typed-array indices) and can throw (test262:
/// Function/prototype/apply/get-length-abrupt, get-index-abrupt,
/// argarray-not-object, resizable-buffer). This is the ONE implementation —
/// `Function.prototype.apply`, `Reflect.apply`, and `Reflect.construct` all
/// route through it, so proxy / plain-array-like argument lists are never
/// silently dropped.
pub fn create_list_from_array_like(
  state: State(host),
  arg: JsValue,
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  case arg {
    JsObject(ref) -> {
      // Step 3: Let len be ? LengthOfArrayLike(obj) = ToLength(? Get(obj, "length")).
      use #(len_val, state) <- result.try(object.get_value(
        state,
        ref,
        Named("length"),
        arg,
      ))
      use #(len_num, state) <- result.try(coerce.js_to_number(state, len_val))
      // §7.1.20 ToLength: clamp to [0, 2^53-1].
      let len = coerce.jsnum_to_length(len_num)
      case len > limits.max_iteration {
        True ->
          Error(state.range_error_value(
            state,
            "Too many arguments in function call",
          ))
        False -> gather_array_like(state, ref, arg, 0, len, [])
      }
    }
    // Step 1: If obj is not an Object, throw a TypeError exception.
    _ ->
      Error(state.type_error_value(
        state,
        "CreateListFromArrayLike called on non-object",
      ))
  }
}

/// Steps 5-7 of §7.3.19: element reads via [[Get]] (index keys).
fn gather_array_like(
  state: State(host),
  ref: value.Ref,
  receiver: JsValue,
  idx: Int,
  len: Int,
  acc: List(JsValue),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  case idx >= len {
    True -> Ok(#(list.reverse(acc), state))
    False -> {
      // Step 7.b-c: Let next be ? Get(obj, ToString(F(index))).
      use #(v, state) <- result.try(object.get_value(
        state,
        ref,
        Index(idx),
        receiver,
      ))
      gather_array_like(state, ref, receiver, idx + 1, len, [v, ..acc])
    }
  }
}
