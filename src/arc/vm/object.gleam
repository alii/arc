import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/opcode
import arc/vm/value.{
  type JsElements, type JsValue, type Property, type Ref, type SymbolId,
  AccessorProperty, ArrayObject, DataProperty, Finite, FunctionObject,
  GeneratorObject, JsNumber, JsObject, JsString, NativeFunction, ObjectSlot,
  OrdinaryObject, PromiseObject,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string

/// Top-level [[Get]] for any JsValue. This combines two spec operations:
///
/// 1. **GetV(V, P)** — ES2024 §7.3.3
///    1. Let O be ? ToObject(V).
///    2. Return ? O.[[Get]](P, V).
///
/// 2. For primitives, we skip the ToObject wrapper allocation and instead
///    delegate directly to the prototype's [[Get]] with `receiver = val`,
///    which preserves the correct `this` binding for getters.
///
/// We never allocate a wrapper object for primitives. Instead:
///   - String primitives synthesize "length" and index properties inline
///     (matching §10.4.3.5 StringGetOwnProperty without a StringObject).
///   - Number/Boolean/Symbol primitives jump straight to prototype [[Get]].
///   - null/undefined return undefined instead of throwing TypeError (callers
///     are expected to guard against this before calling get_value_of).
pub fn get_value_of(
  state: State,
  val: JsValue,
  key: String,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case val {
    // §7.3.3 step 1: V is already an Object, call O.[[Get]](P, V) directly.
    JsObject(ref) -> get_value(state, ref, key, val)
    JsString(s) -> {
      // String primitive: synthesize own properties per §10.4.3.5
      // StringGetOwnProperty, then fall through to String.prototype.
      let len = string.length(s)
      case key {
        // §10.4.3.5 step 7: "length" → {value: len, W:F, E:F, C:F}
        "length" -> Ok(#(JsNumber(Finite(int.to_float(len))), state))
        _ ->
          case int.parse(key) {
            // §10.4.3.5 steps 3-6,8-14: numeric index → single-char string
            Ok(idx) if idx >= 0 && idx < len ->
              Ok(#(JsString(string.slice(s, idx, 1)), state))
            // Not an own property — delegate to String.prototype via [[Get]]
            _ -> get_value(state, state.builtins.string.prototype, key, val)
          }
      }
    }
    // Primitive→prototype delegation (ToObject would wrap, we skip the wrapper)
    JsNumber(_) -> get_value(state, state.builtins.number.prototype, key, val)
    value.JsBool(_) ->
      get_value(state, state.builtins.boolean.prototype, key, val)
    value.JsSymbol(_) ->
      // TODO(Deviation): Symbol.prototype is not yet a dedicated object — it's Object.prototype.
      // Once Symbol.prototype is properly set up with toString/valueOf/description,
      // change this to use the dedicated Symbol.prototype ref.
      get_value(state, state.builtins.object.prototype, key, val)
    // null/undefined → JsUndefined; callers guard and throw TypeError as needed.
    _ -> Ok(#(value.JsUndefined, state))
  }
}

/// **OrdinaryGet(O, P, Receiver)** — ES2024 §10.1.8.1
///
/// Called by [[Get]](P, Receiver) (§10.1.8) which simply delegates here for
/// ordinary objects. The algorithm:
///
///   1. Let desc be ? O.[[GetOwnProperty]](P).
///   2. If desc is undefined:
///      a. Let parent be ? O.[[GetPrototypeOf]]().
///      b. If parent is null, return undefined.
///      c. Return ? parent.[[Get]](P, Receiver).
///   3. If IsDataDescriptor(desc) is true, return desc.[[Value]].
///   4. Assert: IsAccessorDescriptor(desc) is true.
///   5. Let getter be desc.[[Get]].
///   6. If getter is undefined, return undefined.
///   7. Return ? Call(getter, Receiver).
///
/// Steps are reordered — we check own property first and branch on its
/// descriptor type (steps 3-7), with the prototype walk (step 2) in the
/// None/not-found branch. Semantically equivalent.
pub fn get_value(
  state: State,
  ref: Ref,
  key: String,
  receiver: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  // Step 1: Let desc be ? O.[[GetOwnProperty]](P).
  case get_own_property(state.heap, ref, key) {
    // Step 3: IsDataDescriptor(desc) → return desc.[[Value]].
    Some(DataProperty(value: val, ..)) -> Ok(#(val, state))
    // Steps 5-7: IsAccessorDescriptor → Call(getter, Receiver) or undefined.
    Some(AccessorProperty(get: Some(getter), ..)) ->
      frame.call(state, getter, receiver, [])
    Some(AccessorProperty(get: None, ..)) -> Ok(#(value.JsUndefined, state))
    // Step 2: desc is undefined — walk prototype chain.
    None ->
      case heap.read(state.heap, ref) {
        // Step 2c: parent.[[Get]](P, Receiver)
        Some(ObjectSlot(prototype: Some(proto_ref), ..)) ->
          get_value(state, proto_ref, key, receiver)
        // Step 2b: parent is null → return undefined.
        _ -> Ok(#(value.JsUndefined, state))
      }
  }
}

/// Get the character at index `idx`.
///
/// Used by get_own_property's StringObject branch to implement
/// **StringGetOwnProperty** §10.4.3.5 steps 10-14:
///   10. Let str be S.[[StringData]].
///   11-12. Let len be the length of str.
///   13. If index >= len, return undefined.
///   14. Let resultStr be the substring of str from index to index+1.
///
/// TODO(Deviation): Gleam's `string.slice` operates on graphemes (extended grapheme
/// clusters), not UTF-16 code units. This means multi-code-unit characters
/// (e.g. emoji) are treated as single indices, diverging from spec which
/// counts 16-bit code units. Needs a UTF-16 string representation.
fn string_at(s: String, idx: Int) -> Option(String) {
  let ch = string.slice(s, idx, 1)
  case ch {
    "" -> None
    _ -> Some(ch)
  }
}

/// **[[GetOwnProperty]](P)** — dispatches to the appropriate spec algorithm
/// based on object kind.
///
/// For **ordinary objects**: **OrdinaryGetOwnProperty(O, P)** — ES2024 §10.1.5.1
///   1. If O does not have an own property with key P, return undefined.
///   2. Let D be a newly created Property Descriptor with no fields.
///   3. Let X be O's own property whose key is P.
///   4. If X is a data property:
///      a. Set D.[[Value]] to X's value.
///      b. Set D.[[Writable]] to X's writable attribute.
///   5. Else (accessor property):
///      a. Set D.[[Get]] to X's get attribute.
///      b. Set D.[[Set]] to X's set attribute.
///   6. Set D.[[Enumerable]] and D.[[Configurable]].
///   7. Return D.
///
/// For **Array exotic** (§10.4.2): "length" is a virtual data property
///   {value: <int>, writable: true, enumerable: false, configurable: false}.
///   Indices come from elements storage. Other keys from properties dict.
///
/// For **String exotic** (§10.4.3.1):
///   [[GetOwnProperty]](P):
///     1. Let desc be OrdinaryGetOwnProperty(S, P).
///     2. If desc is not undefined, return desc.
///     3. Return StringGetOwnProperty(S, P).
///
///   **StringGetOwnProperty(S, P)** — §10.4.3.5:
///     1-2. If P is not a String or not a canonical numeric index, return undefined.
///     3-4. Let index be CanonicalNumericIndexString(P). If index is undefined, return undefined.
///     5. If index is not an integer, return undefined.
///     6. If index is -0, return undefined.
///     7. Let str be S.[[StringData]].
///     8-9. Let len be the length of str. If index < 0 or index >= len, return undefined.
///     10-14. Return {value: str[index..index+1], W:false, E:true, C:false}.
///     Also: "length" → {value: len, W:false, E:false, C:false}.
///
/// For **Arguments exotic**: indices from elements storage, everything else
///   (including "length" and "callee") from the properties dict.
///
/// We check "length" as a special string key for Array and String objects
/// rather than routing through a separate internal slot. The properties dict
/// lookup for ordinary objects (step 1) is inlined.
pub fn get_own_property(heap: Heap, ref: Ref, key: String) -> Option(Property) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind:, properties:, elements:, ..)) ->
      case kind {
        // --- Array exotic [[GetOwnProperty]] (§10.4.2) ---
        ArrayObject(length:) ->
          case key {
            // Virtual "length" property (§10.4.2.4 ArraySetLength)
            "length" ->
              Some(DataProperty(
                value: JsNumber(Finite(int.to_float(length))),
                writable: True,
                enumerable: False,
                configurable: False,
              ))
            _ ->
              case int.parse(key) {
                // Array index property from elements storage
                Ok(idx) if idx >= 0 ->
                  case js_elements.has(elements, idx) {
                    True ->
                      Some(value.data_property(js_elements.get(elements, idx)))
                    False -> dict_get_option(properties, key)
                  }
                // Non-index key → OrdinaryGetOwnProperty (§10.1.5.1)
                _ -> dict_get_option(properties, key)
              }
          }
        // --- Arguments exotic [[GetOwnProperty]] (§10.4.4) ---
        value.ArgumentsObject(_) ->
          case int.parse(key) {
            Ok(idx) if idx >= 0 ->
              case js_elements.get_option(elements, idx) {
                Some(val) -> Some(value.data_property(val))
                None -> dict_get_option(properties, key)
              }
            _ -> dict_get_option(properties, key)
          }
        // --- String exotic [[GetOwnProperty]] (§10.4.3.1) ---
        value.StringObject(value: s) ->
          case key {
            // §10.4.3.5 step 7: "length" → {value: len, W:F, E:F, C:F}
            "length" ->
              Some(DataProperty(
                value: JsNumber(Finite(int.to_float(string.length(s)))),
                writable: False,
                enumerable: False,
                configurable: False,
              ))
            _ ->
              case int.parse(key) {
                // §10.4.3.5 steps 3-6: CanonicalNumericIndexString → integer index
                Ok(idx) if idx >= 0 ->
                  case string_at(s, idx) {
                    // §10.4.3.5 steps 10-14: return {value: char, W:F, E:T, C:F}
                    Some(ch) ->
                      Some(DataProperty(
                        value: JsString(ch),
                        writable: False,
                        enumerable: True,
                        configurable: False,
                      ))
                    // §10.4.3.5 step 9: index >= len → undefined, fall to ordinary
                    None -> dict_get_option(properties, key)
                  }
                // §10.4.3.1 step 1-2: not a numeric index → OrdinaryGetOwnProperty
                _ -> dict_get_option(properties, key)
              }
          }
        // --- Ordinary [[GetOwnProperty]] (§10.1.5.1) ---
        _ -> dict_get_option(properties, key)
      }
    _ -> None
  }
}

/// Helper: dict.get but returns Option instead of Result.
/// Implements §10.1.5.1 OrdinaryGetOwnProperty step 1: look up the key in
/// the object's own property storage. Returns None (spec "undefined") if
/// the key is not present, or Some(descriptor) if found.
fn dict_get_option(
  d: dict.Dict(String, Property),
  key: String,
) -> Option(Property) {
  case dict.get(d, key) {
    Ok(prop) -> Some(prop)
    Error(_) -> None
  }
}

/// §10.1.9.1 OrdinarySet ( O, P, V, Receiver )
/// Combined with §10.1.9.2 OrdinarySetWithOwnDescriptor.
///
/// Walks the proto chain. Handles accessors (calls setter), non-writable proto
/// blocking, and creates own data property on receiver when not found.
pub fn set_value(
  state: State,
  ref: Ref,
  key: String,
  val: JsValue,
  receiver: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  // §10.1.9.1 step 1: Let ownDesc be ? O.[[GetOwnProperty]](P).
  // §10.1.9.1 step 2: Return OrdinarySetWithOwnDescriptor(O, P, V, Receiver, ownDesc).
  case get_own_property(state.heap, ref, key) {
    // §10.1.9.2 step 1: If ownDesc is undefined, then
    None ->
      // §10.1.9.2 step 1.a: Let parent be ? O.[[GetPrototypeOf]]().
      case heap.read(state.heap, ref) {
        // §10.1.9.2 step 1.b: If parent is not null, return ? parent.[[Set]](P, V, Receiver).
        Some(ObjectSlot(prototype: Some(proto_ref), ..)) ->
          set_value(state, proto_ref, key, val, receiver)
        // §10.1.9.2 step 1.c: Else, set ownDesc to {[[Value]]: undefined, [[Writable]]: true,
        //   [[Enumerable]]: true, [[Configurable]]: true}.
        // (Falls through to set_on_receiver which creates the property.)
        _ -> set_on_receiver(state, receiver, key, val)
      }
    // §10.1.9.2 step 2: If IsDataDescriptor(ownDesc) is true, then
    //   step 2.a: If ownDesc.[[Writable]] is false, return false.
    Some(DataProperty(writable: False, ..)) -> Ok(#(state, False))
    // §10.1.9.2 steps 2.b-2.e: ownDesc is writable data — delegate to receiver.
    // We delegate to set_on_receiver which handles both create and update
    // via set_property (spec distinguishes steps 2.c vs 2.e but result is same).
    Some(DataProperty(writable: True, ..)) ->
      set_on_receiver(state, receiver, key, val)
    // §10.1.9.2 step 3: Assert: ownDesc is an accessor descriptor.
    //   step 3.a: Let setter be ownDesc.[[Set]].
    //   step 3.b: If setter is undefined, return false.
    Some(AccessorProperty(set: None, ..)) -> Ok(#(state, False))
    // §10.1.9.2 step 3.c: Perform ? Call(setter, Receiver, « V »).
    // §10.1.9.2 step 3.d: Return true.
    Some(AccessorProperty(set: Some(setter), ..)) -> {
      use #(_, state) <- result.map(frame.call(state, setter, receiver, [val]))
      #(state, True)
    }
  }
}

/// §10.1.9.2 OrdinarySetWithOwnDescriptor steps 2.b-2.e (receiver half).
///
/// Create or update an own data property on the receiver. Shared by
/// set_value's "not found in proto chain" and "writable proto data" branches.
///
/// The spec distinguishes "receiver has existing own property" (step 2.c —
/// only updates [[Value]]) vs "receiver has no own property" (step 2.e —
/// CreateDataProperty). We delegate both cases to set_property which handles
/// the distinction internally with identical semantics.
///
/// §10.1.9.2 step 2.b: If receiver is not an Object, return false.
fn set_on_receiver(
  state: State,
  receiver: JsValue,
  key: String,
  val: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  case receiver {
    // §10.1.9.2 step 2.c-e: Receiver is an object — define/update own property.
    JsObject(recv_ref) -> {
      let #(h, ok) = set_property(state.heap, recv_ref, key, val)
      Ok(#(State(..state, heap: h), ok))
    }
    // §10.1.9.2 step 2.b: Receiver is not an Object, return false.
    _ -> Ok(#(state, False))
  }
}

/// §10.4.2.1 Array exotic [[DefineOwnProperty]] / OrdinaryDefineOwnProperty (§10.1.6.1).
///
/// Own-property-level write. Does NOT walk the proto chain — use set_value for
/// the full [[Set]] algorithm. Respects writable flag and extensible flag.
/// Returns `#(heap, success)`.
///
/// `success = False` when:
///   - Existing property is non-writable (OrdinaryDefineOwnProperty step 3/4)
///   - New property on non-extensible object (step 2)
///   - StringObject guarded key: in-range index or "length" (§10.4.3.2 step 2-3)
///   - ref is invalid / not an ObjectSlot
///
/// Callers decide what to do with `False`: sloppy mode ignores it, strict mode
/// throws TypeError, and Array.prototype mutators always throw (they use
/// `Set(O, P, V, true)` per spec — the `true` flag means throw-on-failure).
///
/// For ArrayObject (§10.4.2.1):
///   - step 1: If P is "length", perform ArraySetLength(A, Desc) (§10.4.2.4)
///   - step 2: Else if P is an array index, validate extensibility and update
///     elements storage, growing length if index >= current length
///   - step 3: Else, OrdinaryDefineOwnProperty(A, P, Desc)
///
/// TODO(Deviation): spec passes full Property Descriptors; we only handle the
/// value-update case (equivalent to {[[Value]]: val} partial descriptor).
/// Full descriptor merging (attribute changes, data<->accessor conversion)
/// is needed for complete Object.defineProperty support.
pub fn set_property(
  h: Heap,
  ref: Ref,
  key: String,
  val: JsValue,
) -> #(Heap, Bool) {
  case heap.read(h, ref) {
    Some(ObjectSlot(kind:, elements:, extensible:, ..) as slot) ->
      case kind {
        // --- §10.4.2.1 Array exotic [[DefineOwnProperty]] ---
        ArrayObject(length:) ->
          case key {
            // §10.4.2.1 step 1: If P is "length", return ArraySetLength(A, Desc).
            "length" -> array_set_length(h, ref, val, slot, length)
            _ ->
              case int.parse(key) {
                // §10.4.2.1 step 2: If P is an array index (ToUint32 is valid index):
                Ok(idx) if idx >= 0 ->
                  // §10.4.2.1 step 2.b: If index >= oldLen, growing length —
                  // check extensible first (non-extensible can't add new indices).
                  case idx >= length && !extensible {
                    True -> #(h, False)
                    False -> {
                      // §10.4.2.1 step 2.c-d: Set element, update length to max(oldLen, index+1).
                      let new_elements = js_elements.set(elements, idx, val)
                      let new_length = int.max(length, idx + 1)
                      #(
                        heap.write(
                          h,
                          ref,
                          ObjectSlot(
                            ..slot,
                            kind: ArrayObject(new_length),
                            elements: new_elements,
                          ),
                        ),
                        True,
                      )
                    }
                  }
                // §10.4.2.1 step 3: Not "length" and not array index —
                // OrdinaryDefineOwnProperty(A, P, Desc).
                _ -> set_string_property(h, ref, key, val, slot)
              }
          }
        // --- §10.4.4 Arguments exotic — similar element-based storage ---
        value.ArgumentsObject(_) ->
          case int.parse(key) {
            Ok(idx) if idx >= 0 ->
              case !extensible && !js_elements.has(elements, idx) {
                True -> #(h, False)
                False -> #(
                  heap.write(
                    h,
                    ref,
                    ObjectSlot(
                      ..slot,
                      elements: js_elements.set(elements, idx, val),
                    ),
                  ),
                  True,
                )
              }
            _ -> set_string_property(h, ref, key, val, slot)
          }
        // --- §10.4.3.2 String exotic [[DefineOwnProperty]] ---
        value.StringObject(value: s) -> {
          let len = string.length(s)
          // §10.4.3.2 step 2: If P is a CanonicalNumericIndexString for an
          // integer in [0, length), the property is non-configurable/non-writable,
          // so [[DefineOwnProperty]] returns false for any change.
          // §10.4.3.2 step 3: "length" is also immutable.
          let is_guarded = case key {
            "length" -> True
            _ ->
              case int.parse(key) {
                Ok(idx) -> idx >= 0 && idx < len
                Error(_) -> False
              }
          }
          case is_guarded {
            // §10.4.3.2: Reject — property is immutable on String exotic.
            True -> #(h, False)
            // §10.4.3.2 step 4: Else, OrdinaryDefineOwnProperty(S, P, Desc).
            False -> set_string_property(h, ref, key, val, slot)
          }
        }
        // --- §10.1.6.1 OrdinaryDefineOwnProperty for all other objects ---
        _ -> set_string_property(h, ref, key, val, slot)
      }
    _ -> #(h, False)
  }
}

/// §10.4.2.4 ArraySetLength ( A, Desc ) — simplified.
///
/// Steps 1-3: Let newLen be ToUint32(Desc.[[Value]]).
/// Step 4: If newLen != ToNumber(Desc.[[Value]]), throw RangeError.
/// TODO(Deviation): we use coerce_length which rejects non-integer/negative/NaN but
/// returns False instead of throwing RangeError. Should throw RangeError
/// per spec step 4 when newLen != ToNumber(Desc.[[Value]]).
///
/// Steps 5-7: Let oldLen be A.[[ArrayLength]].
/// Steps 8-11: If newLen >= oldLen, set length and return true.
/// Steps 12-14: If oldLen length property is non-writable, return false.
/// TODO(Deviation): we don't track writable on the virtual length property yet.
/// Object.defineProperty(arr, 'length', {writable: false}) should freeze length.
///
/// Steps 15-18: Delete elements from oldLen-1 down to newLen.
/// TODO(Deviation): spec stops at first non-configurable element (step 17.b) and
/// returns false with length set to that index+1. Our elements have no
/// per-index descriptors, so all are implicitly configurable. Needs
/// per-element property descriptors to handle non-configurable indices.
fn array_set_length(
  h: Heap,
  ref: Ref,
  val: JsValue,
  slot: value.HeapSlot,
  old_length: Int,
) -> #(Heap, Bool) {
  // §10.4.2.4 steps 1-4: Coerce value to valid uint32 length.
  case coerce_length(val) {
    // Step 4: Would be RangeError; we return False.
    None -> #(h, False)
    Some(new_length) -> {
      let assert ObjectSlot(elements:, ..) = slot
      // §10.4.2.4 steps 8-18: If shrinking, truncate elements >= newLen.
      let new_elements = case new_length < old_length {
        True -> truncate_elements(elements, new_length, old_length)
        False -> elements
      }
      // §10.4.2.4 step 19: Set A.[[ArrayLength]] to newLen; return true.
      #(
        heap.write(
          h,
          ref,
          ObjectSlot(
            ..slot,
            kind: ArrayObject(new_length),
            elements: new_elements,
          ),
        ),
        True,
      )
    }
  }
}

/// §10.4.2.4 ArraySetLength steps 1-4: ToUint32 + RangeError validation.
///
/// Spec: Let newLen be ToUint32(Desc.[[Value]]). If newLen != ToNumber(Desc.[[Value]]),
/// throw RangeError.
///
/// Simplified: we accept finite numbers that are non-negative integers.
/// Fractional, negative, NaN, Infinity, and non-numeric values return None
/// (caller treats as failure). This covers both internal Set(O,"length",n,true)
/// calls from Array.prototype mutators and user-level `arr.length = 3.5`.
fn coerce_length(val: JsValue) -> Option(Int) {
  case val {
    JsNumber(Finite(f)) -> {
      let n = value.float_to_int(f)
      case n >= 0 && int.to_float(n) == f {
        True -> Some(n)
        False -> None
      }
    }
    _ -> None
  }
}

/// §10.4.2.4 step 17: "Repeat, while newLen < oldLen"
///   step 17.a: Set oldLen to oldLen - 1.
///   step 17.b: Let deleteSucceeded be ! A.[[Delete]](! ToString(F(oldLen))).
///   step 17.c: If deleteSucceeded is false, [stop and fail at that index].
///
/// Delete elements in [new_len, old_len), working backward per spec. Stops
/// immediately since all our elements are implicitly configurable — if we
/// ever add per-element descriptors, this needs to stop at the first
/// non-configurable and signal partial success (step 17.c).
fn truncate_elements(elements: JsElements, new_len: Int, idx: Int) -> JsElements {
  case idx <= new_len {
    True -> elements
    False ->
      truncate_elements(js_elements.delete(elements, idx - 1), new_len, idx - 1)
  }
}

/// §10.1.6.1 OrdinaryDefineOwnProperty / §10.1.6.3 ValidateAndApplyPropertyDescriptor
/// (value-update subset).
///
/// Set a string-keyed own property in the properties dict. Returns #(Heap, success).
///
/// Step 2 (ValidateAndApply): If current is undefined and extensible is false, return false.
/// Step 3: If current is undefined and extensible is true, create the property.
/// Step 4-7: If current exists, check writable. If writable is true, update [[Value]].
///           If writable is false, return false. Accessors also return false
///           (would need [[Set]] path, not [[DefineOwnProperty]]).
///
/// TODO(Deviation): spec's ValidateAndApplyPropertyDescriptor does full descriptor
/// merging (attribute changes, data<->accessor conversion). We only handle
/// the [[Value]] update case. Full descriptor support needed for
/// Object.defineProperty.
fn set_string_property(
  h: Heap,
  ref: Ref,
  key: String,
  val: JsValue,
  slot: value.HeapSlot,
) -> #(Heap, Bool) {
  case slot {
    ObjectSlot(properties:, extensible:, ..) ->
      // §10.1.6.1 step 1: Let current be ? O.[[GetOwnProperty]](P).
      case dict.get(properties, key) {
        // §10.1.6.3 step 4-7: current exists and is writable data — update [[Value]].
        Ok(DataProperty(writable: True, enumerable:, configurable:, ..)) -> {
          let new_props =
            dict.insert(
              properties,
              key,
              DataProperty(
                value: val,
                writable: True,
                enumerable:,
                configurable:,
              ),
            )
          #(heap.write(h, ref, ObjectSlot(..slot, properties: new_props)), True)
        }
        // §10.1.6.3 step 6: current.[[Writable]] is false → reject.
        Ok(DataProperty(writable: False, ..)) -> #(h, False)
        // Accessor property: [[DefineOwnProperty]] with just a value on an
        // accessor would convert it to data, but we don't support that yet.
        Ok(value.AccessorProperty(..)) -> #(h, False)
        // §10.1.6.3 step 2-3: Property doesn't exist on this object.
        Error(_) ->
          case extensible {
            // §10.1.6.3 step 2: If extensible is false, return false.
            False -> #(h, False)
            // §10.1.6.3 step 3: extensible is true — create new data property
            // with {[[Value]]: V, [[Writable]]: true, [[Enumerable]]: true,
            // [[Configurable]]: true}.
            True -> {
              let new_props =
                dict.insert(properties, key, value.data_property(val))
              #(
                heap.write(h, ref, ObjectSlot(..slot, properties: new_props)),
                True,
              )
            }
          }
      }
    _ -> #(h, False)
  }
}

/// §7.3.5 CreateDataProperty ( O, P, V )
///
/// Step 1: Let newDesc be the PropertyDescriptor {[[Value]]: V, [[Writable]]: true,
///         [[Enumerable]]: true, [[Configurable]]: true}.
/// Step 2: Return ? O.[[DefineOwnProperty]](P, newDesc).
///
/// Used for object literal fields and internal setup. Always writes regardless
/// of existing flags (spec says this "is used to create new own properties";
/// callers ensure the property doesn't already exist or don't care).
///
/// Ignores the return value (spec returns a Boolean from
/// [[DefineOwnProperty]]). Does not throw on failure — callers use this
/// only in contexts where success is guaranteed (fresh objects, literals).
fn define_own_property(heap: Heap, ref: Ref, key: String, val: JsValue) -> Heap {
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(properties:, ..) -> {
      let new_props = dict.insert(properties, key, value.data_property(val))
      ObjectSlot(..slot, properties: new_props)
    }
    _ -> slot
  }
}

/// §7.3.6 CreateMethodProperty ( O, P, V ) — ES2022 numbering; renamed to
/// CreateNonEnumerableDataPropertyOrThrow (§7.3.7) in ES2024.
///
/// Step 1: Let newDesc be the PropertyDescriptor {[[Value]]: V, [[Writable]]: true,
///         [[Enumerable]]: false, [[Configurable]]: true}.
/// Step 2: Perform ! O.[[DefineOwnProperty]](P, newDesc).
///
/// Used for class methods and built-in methods. The "!" (bang) means this
/// must not fail — callers guarantee O is extensible and P doesn't already
/// exist as a non-configurable property.
pub fn define_method_property(
  heap: Heap,
  ref: Ref,
  key: String,
  val: JsValue,
) -> Heap {
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(properties:, ..) -> {
      let new_props = dict.insert(properties, key, value.builtin_property(val))
      ObjectSlot(..slot, properties: new_props)
    }
    _ -> slot
  }
}

/// §14.3.9 Runtime Semantics: PropertyDefinitionEvaluation for
/// MethodDefinition : get PropertyName ( ) { FunctionBody }
/// and MethodDefinition : set PropertyName ( PropertySetParameterList ) { FunctionBody }
///
/// Calls §7.3.8 DefinePropertyOrThrow ( O, P, desc ) with an accessor descriptor:
///   - getter: {[[Get]]: closure, [[Enumerable]]: true, [[Configurable]]: true}
///   - setter: {[[Set]]: closure, [[Enumerable]]: true, [[Configurable]]: true}
///
/// If the property already exists as an accessor, merges the new get/set
/// (the spec achieves this via [[DefineOwnProperty]] descriptor merging in
/// §10.1.6.3 ValidateAndApplyPropertyDescriptor step 4.b: "For each field of
/// Desc that is present, set the corresponding attribute of the property to
/// the value of the field").
///
/// Used by object literal `{ get x() {}, set x(v) {} }` syntax.
pub fn define_accessor(
  heap: Heap,
  ref: Ref,
  key: String,
  func: JsValue,
  kind: opcode.AccessorKind,
) -> Heap {
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(properties:, ..) -> {
      // §10.1.6.3 step 4.b: Merge with existing accessor if present
      let existing = dict.get(properties, key)
      let new_prop = case kind {
        opcode.Getter ->
          case existing {
            Ok(AccessorProperty(set: s, ..)) ->
              AccessorProperty(
                get: Some(func),
                set: s,
                enumerable: True,
                configurable: True,
              )
            _ ->
              AccessorProperty(
                get: Some(func),
                set: None,
                enumerable: True,
                configurable: True,
              )
          }
        opcode.Setter ->
          case existing {
            Ok(AccessorProperty(get: g, ..)) ->
              AccessorProperty(
                get: g,
                set: Some(func),
                enumerable: True,
                configurable: True,
              )
            _ ->
              AccessorProperty(
                get: None,
                set: Some(func),
                enumerable: True,
                configurable: True,
              )
          }
      }
      let new_props = dict.insert(properties, key, new_prop)
      ObjectSlot(..slot, properties: new_props)
    }
    _ -> slot
  }
}

/// §10.1.7 [[HasProperty]] ( P ) / §10.1.7.1 OrdinaryHasProperty ( O, P )
///
/// Step 1: Let hasOwn be ? O.[[GetOwnProperty]](P).
/// Step 2: If hasOwn is not undefined, return true.
/// Step 3: Let parent be ? O.[[GetPrototypeOf]]().
/// Step 4: If parent is not null, return ? parent.[[HasProperty]](P).
/// Step 5: Return false.
///
/// Used by the `in` operator. Checks own properties (including elements
/// for array-like objects via get_own_property), then walks prototype chain.
///
/// Pure (no Result) — our GetOwnProperty and GetPrototypeOf cannot throw
/// (no Proxy traps), so steps 1/3 never produce abrupt completions.
pub fn has_property(heap: Heap, ref: Ref, key: String) -> Bool {
  // Step 1-2: Let hasOwn be O.[[GetOwnProperty]](P). If not undefined, return true.
  case get_own_property(heap, ref, key) {
    Some(_) -> True
    None ->
      // Step 3-4: Let parent be O.[[GetPrototypeOf]](). If not null, recurse.
      case heap.read(heap, ref) {
        Some(ObjectSlot(prototype: Some(proto_ref), ..)) ->
          has_property(heap, proto_ref, key)
        // Step 5: Return false (null prototype or non-object slot).
        _ -> False
      }
  }
}

/// §10.1.10 [[Delete]] ( P ) / §10.1.10.1 OrdinaryDelete ( O, P )
///
/// Step 1: Let desc be ? O.[[GetOwnProperty]](P).
/// Step 2: If desc is undefined, return true.
/// Step 3: If desc.[[Configurable]] is true, then
///   Step 3.a: Remove the own property with name P from O.
///   Step 3.b: Return true.
/// Step 4: Return false.
///
/// Returns #(updated_heap, success). Non-existent properties return true (step 2).
///
/// TODO(Deviation): for arrays/arguments, our elements are always configurable, so
/// element deletion always succeeds. Needs per-element property descriptors
/// to reject deletion of non-configurable index properties per spec.
pub fn delete_property(h: Heap, ref: Ref, key: String) -> #(Heap, Bool) {
  case heap.read(h, ref) {
    Some(ObjectSlot(kind:, elements:, ..) as slot) ->
      case kind {
        // Array/Arguments exotic: check if key is an array index
        ArrayObject(_) | value.ArgumentsObject(_) ->
          case int.parse(key) {
            Ok(idx) ->
              // Step 1-2: Check if element exists; if not, return true.
              case js_elements.has(elements, idx) {
                // Step 3: Element exists (implicitly configurable) — remove and return true.
                True -> #(
                  heap.write(
                    h,
                    ref,
                    ObjectSlot(
                      ..slot,
                      elements: js_elements.delete(elements, idx),
                    ),
                  ),
                  True,
                )
                // Step 2: desc is undefined → return true.
                False -> #(h, True)
              }
            Error(_) -> delete_string_property(h, ref, key, slot)
          }
        _ -> delete_string_property(h, ref, key, slot)
      }
    // Step 2: No slot found — treat as non-existent, return true.
    _ -> #(h, True)
  }
}

/// §10.1.10.1 OrdinaryDelete ( O, P ) — string-keyed property case.
///
/// Step 1: Let desc be ? O.[[GetOwnProperty]](P).
/// Step 2: If desc is undefined, return true.
/// Step 3: If desc.[[Configurable]] is true, remove and return true.
/// Step 4: Return false.
fn delete_string_property(
  h: Heap,
  ref: Ref,
  key: String,
  slot: value.HeapSlot,
) -> #(Heap, Bool) {
  case slot {
    ObjectSlot(properties:, ..) ->
      case dict.get(properties, key) {
        // Step 3: desc.[[Configurable]] is true → remove own property, return true.
        Ok(DataProperty(configurable: True, ..))
        | Ok(value.AccessorProperty(configurable: True, ..)) -> #(
          heap.write(
            h,
            ref,
            ObjectSlot(..slot, properties: dict.delete(properties, key)),
          ),
          True,
        )
        // Step 4: desc.[[Configurable]] is false → return false.
        Ok(DataProperty(configurable: False, ..))
        | Ok(value.AccessorProperty(configurable: False, ..)) -> #(h, False)
        // Step 2: desc is undefined → return true.
        Error(_) -> #(h, True)
      }
    _ -> #(h, True)
  }
}

/// §7.3.23 EnumerableOwnProperties ( O, kind ) — "key" variant only.
///
/// Step 1: Let ownKeys be ? O.[[OwnPropertyKeys]]().
/// Step 2: Let results be a new empty List.
/// Step 3: For each element key of ownKeys, do
///   Step 3.a: If key is a String, then
///     Step 3.a.i: Let desc be ? O.[[GetOwnProperty]](key).
///     Step 3.a.ii: If desc is not undefined and desc.[[Enumerable]] is true, then
///       Step 3.a.ii.1: (kind = "key") Append key to results.
/// Step 4: Return results.
///
/// Extended to walk the prototype chain for for-in enumeration (§14.7.5.9
/// ForIn/OfHeadEvaluation uses [[Enumerate]] which walks prototypes).
/// Uses a seen set to skip shadowed keys per §14.7.5.10 step 5.b:
/// "If key is already in the set visitedKeys, skip it."
///
/// The spec's EnumerableOwnProperties only handles own properties; we extend
/// it with prototype walking here because for-in needs it, matching the
/// [[Enumerate]] internal method behavior. Symbol keys are excluded per
/// step 3.a (only String keys).
pub fn enumerate_keys(heap: Heap, ref: Ref) -> List(String) {
  enumerate_keys_loop(heap, Some(ref), set.new(), [])
}

/// Helper for enumerate_keys — walks the prototype chain collecting
/// enumerable string keys, skipping shadowed keys via the seen set.
fn enumerate_keys_loop(
  heap: Heap,
  current: Option(Ref),
  seen: set.Set(String),
  acc: List(String),
) -> List(String) {
  case current {
    // Step 4: No more objects in chain → return collected results.
    None -> list.reverse(acc)
    Some(ref) ->
      case heap.read(heap, ref) {
        Some(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) -> {
          // Step 1 (partial): Collect element keys first (array index portion
          // of [[OwnPropertyKeys]], which returns indices in ascending order).
          let #(elem_acc, elem_seen) = case kind {
            ArrayObject(length:) | value.ArgumentsObject(length:) ->
              collect_element_keys(elements, 0, length, seen, acc)
            _ -> #(acc, seen)
          }
          // Step 3: For each string key, check desc.[[Enumerable]].
          // Non-enumerable keys are added to seen (for shadowing) but not to results.
          let #(final_acc, final_seen) =
            dict.fold(properties, #(elem_acc, elem_seen), fn(state, key, prop) {
              let #(a, s) = state
              case set.contains(s, key) {
                True -> #(a, s)
                False ->
                  case prop {
                    // Step 3.a.ii: desc.[[Enumerable]] is true → append key.
                    DataProperty(enumerable: True, ..) -> #(
                      [key, ..a],
                      set.insert(s, key),
                    )
                    // Non-enumerable or accessor: mark seen but don't include.
                    _ -> #(a, set.insert(s, key))
                  }
              }
            })
          // Walk prototype chain (for-in extension).
          enumerate_keys_loop(heap, prototype, final_seen, final_acc)
        }
        _ -> list.reverse(acc)
      }
  }
}

/// Helper: collect element indices [0, length) as string keys in ascending
/// order. Skips holes and already-seen keys. This corresponds to the array
/// index portion of §10.1.11.1 OrdinaryOwnPropertyKeys step 1: "For each
/// own property key P of O that is an array index, in ascending numeric
/// index order, add P to keys."
fn collect_element_keys(
  elements: JsElements,
  idx: Int,
  length: Int,
  seen: set.Set(String),
  acc: List(String),
) -> #(List(String), set.Set(String)) {
  case idx >= length {
    True -> #(acc, seen)
    False -> {
      let key = int.to_string(idx)
      case js_elements.has(elements, idx) && !set.contains(seen, key) {
        True ->
          collect_element_keys(
            elements,
            idx + 1,
            length,
            set.insert(seen, key),
            [key, ..acc],
          )
        False -> collect_element_keys(elements, idx + 1, length, seen, acc)
      }
    }
  }
}

// ============================================================================
// Symbol-keyed property access

/// §7.3.25 CopyDataProperties ( target, source, excludedItems )
///
/// Step 1: If source is undefined or null, return target.
/// Step 2: Let from be ! ToObject(source).
/// Step 3: Let keys be ? from.[[OwnPropertyKeys]]().
/// Step 4: For each element nextKey of keys, do
///   Step 4.a: Let excluded be false.
///   Step 4.b: For each element e of excludedItems, if SameValue(e, nextKey), set excluded to true.
///   Step 4.c: If excluded is false, then
///     Step 4.c.i: Let desc be ? from.[[GetOwnProperty]](nextKey).
///     Step 4.c.ii: If desc is not undefined and desc.[[Enumerable]] is true, then
///       Step 4.c.ii.1: Let propValue be ? Get(from, nextKey).
///       Step 4.c.ii.2: Perform ! CreateDataPropertyOrThrow(target, nextKey, propValue).
/// Step 5: Return target.
///
/// Used by object spread `{...source}` and Object.assign.
///
/// TODO(Deviation): we do not support excludedItems (always empty for object spread).
/// Needed for destructuring rest patterns. Also, symbol-keyed accessor
/// getters are not invoked — the descriptor is copied directly.
pub fn copy_data_properties(
  state: State,
  target_ref: Ref,
  source: JsValue,
) -> Result(State, #(JsValue, State)) {
  case source {
    // Step 2: source is already an object (from is source).
    JsObject(src_ref) ->
      case heap.read(state.heap, src_ref) {
        Some(ObjectSlot(kind:, properties:, elements:, symbol_properties:, ..)) -> {
          // Step 3-4 (array index keys): Copy element indices in ascending order.
          // These are always enumerable data properties.
          let heap = case kind {
            ArrayObject(length:) | value.ArgumentsObject(length:) ->
              copy_element_range(state.heap, target_ref, elements, 0, length)
            _ -> state.heap
          }
          let state = State(..state, heap:)
          // Step 4 (string keys): Filter to enumerable, then Get + CreateDataProperty.
          let keys =
            dict.to_list(properties)
            |> list.filter_map(fn(pair) {
              let #(k, prop) = pair
              case prop {
                // Step 4.c.ii: desc.[[Enumerable]] is true.
                DataProperty(enumerable: True, ..) -> Ok(k)
                AccessorProperty(enumerable: True, ..) -> Ok(k)
                _ -> Error(Nil)
              }
            })
          // Step 4.c.ii.1-2: Get(from, key) then CreateDataPropertyOrThrow(target, key, val).
          use state <- copy_keys_to_target(state, src_ref, target_ref, keys)
          // Step 4 (symbol keys): Copy enumerable symbol-keyed data properties.
          let sym_heap =
            dict.fold(symbol_properties, state.heap, fn(h, k, prop) {
              case prop {
                DataProperty(value: v, enumerable: True, ..) ->
                  define_symbol_property(
                    h,
                    target_ref,
                    k,
                    value.data_property(v),
                  )
                _ -> h
              }
            })
          // Step 5: Return target (implicitly via updated state).
          Ok(State(..state, heap: sym_heap))
        }
        _ -> Ok(state)
      }
    // Step 1: source is undefined or null → return target (no-op).
    _ -> Ok(state)
  }
}

/// §7.3.25 CopyDataProperties steps 4.c.ii.1-2 — for each key:
///   Step 4.c.ii.1: Let propValue be ? Get(from, nextKey).
///   Step 4.c.ii.2: Perform ! CreateDataPropertyOrThrow(target, nextKey, propValue).
///
/// Calls getters via get_value (which invokes accessor [[Get]]), then writes
/// to target via define_own_property (CreateDataProperty).
fn copy_keys_to_target(
  state: State,
  src_ref: Ref,
  target_ref: Ref,
  keys: List(String),
  cont: fn(State) -> Result(State, #(JsValue, State)),
) -> Result(State, #(JsValue, State)) {
  case keys {
    [] -> cont(state)
    [k, ..rest] -> {
      // Step 4.c.ii.1: Let propValue be ? Get(from, nextKey).
      use #(val, state) <- result.try(get_value(
        state,
        src_ref,
        k,
        JsObject(src_ref),
      ))
      // Step 4.c.ii.2: Perform ! CreateDataPropertyOrThrow(target, nextKey, propValue).
      let heap = define_own_property(state.heap, target_ref, k, val)
      copy_keys_to_target(
        State(..state, heap:),
        src_ref,
        target_ref,
        rest,
        cont,
      )
    }
  }
}

/// §7.3.25 CopyDataProperties — array index key portion.
///
/// Copies present elements from source [0, end) to target as string-keyed
/// data properties ("0", "1", ...). Holes are skipped (they have no
/// property descriptor, so step 4.c.i "desc is undefined" applies).
///
/// This is an optimization: instead of going through [[OwnPropertyKeys]]
/// and then Get for each index, we iterate the elements storage directly.
/// The result is the same because array elements are always enumerable
/// data properties (step 4.c.ii check always passes for present elements).
fn copy_element_range(
  heap: Heap,
  target_ref: Ref,
  elements: JsElements,
  idx: Int,
  end: Int,
) -> Heap {
  case idx >= end {
    True -> heap
    False ->
      case js_elements.has(elements, idx) {
        True -> {
          // Step 4.c.ii.2: CreateDataPropertyOrThrow(target, ToString(idx), value).
          let h =
            define_own_property(
              heap,
              target_ref,
              int.to_string(idx),
              js_elements.get(elements, idx),
            )
          copy_element_range(h, target_ref, elements, idx + 1, end)
        }
        // Hole — no descriptor, skip per step 4.c.i.
        False -> copy_element_range(heap, target_ref, elements, idx + 1, end)
      }
  }
}

// ============================================================================

/// §10.1.8.1 OrdinaryGet ( O, P, Receiver ) — symbol-keyed variant.
///
/// Same algorithm as string-keyed OrdinaryGet (see get_value), but operates
/// on the symbol_properties dict instead of string properties.
///
/// Step 1: Let desc be ? O.[[GetOwnProperty]](P).
/// Step 2: If desc is undefined, then
///   Step 2.a: Let parent be ? O.[[GetPrototypeOf]]().
///   Step 2.b: If parent is null, return undefined.
///   Step 2.c: Return ? parent.[[Get]](P, Receiver).
/// Step 3: If IsDataDescriptor(desc) is true, return desc.[[Value]].
/// Step 4: Assert: IsAccessorDescriptor(desc) is true.
/// Step 5: Let getter be desc.[[Get]].
/// Step 6: If getter is undefined, return undefined.
/// Step 7: Return ? Call(getter, Receiver).
pub fn get_symbol_value(
  state: State,
  ref: Ref,
  key: SymbolId,
  receiver: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(symbol_properties:, prototype:, ..)) ->
      // Step 1: Let desc be O.[[GetOwnProperty]](P).
      case dict.get(symbol_properties, key) {
        // Step 3: IsDataDescriptor → return desc.[[Value]].
        Ok(DataProperty(value: val, ..)) -> Ok(#(val, state))
        // Step 5-7: Accessor with getter → Call(getter, Receiver).
        Ok(AccessorProperty(get: Some(getter), ..)) ->
          frame.call(state, getter, receiver, [])
        // Step 6: getter is undefined → return undefined.
        Ok(AccessorProperty(get: None, ..)) -> Ok(#(value.JsUndefined, state))
        // Step 2: desc is undefined → walk prototype chain.
        Error(_) ->
          case prototype {
            // Step 2.c: Return ? parent.[[Get]](P, Receiver).
            Some(proto_ref) -> get_symbol_value(state, proto_ref, key, receiver)
            // Step 2.b: parent is null → return undefined.
            None -> Ok(#(value.JsUndefined, state))
          }
      }
    _ -> Ok(#(value.JsUndefined, state))
  }
}

/// §10.1.9.1 OrdinarySet ( O, P, V, Receiver ) / §10.1.9.2 OrdinarySetWithOwnDescriptor
/// — symbol-keyed variant.
///
/// Same algorithm as string-keyed OrdinarySet (see set_value), but operates
/// on the symbol_properties dict.
///
/// Step 1: Let ownDesc be ? O.[[GetOwnProperty]](P).
/// Step 2: Return ? OrdinarySetWithOwnDescriptor(O, P, V, Receiver, ownDesc).
///
/// OrdinarySetWithOwnDescriptor:
/// Step 1: If ownDesc is undefined, then
///   Step 1.a: Let parent be ? O.[[GetPrototypeOf]]().
///   Step 1.b: If parent is not null, return ? parent.[[Set]](P, V, Receiver).
///   Step 1.c: Else, set ownDesc to {[[Value]]: undefined, [[Writable]]: true, ...}.
/// Step 2: If IsDataDescriptor(ownDesc) is true, then
///   Step 2.a: If ownDesc.[[Writable]] is false, return false.
///   Step 2.b: If Receiver is not an Object, return false.
///   Step 2.c: Let existingDescriptor be ? Receiver.[[GetOwnProperty]](P).
///   Step 2.d-e: Create or update own property on Receiver.
/// Step 3: Assert: IsAccessorDescriptor(ownDesc) is true.
/// Step 4: Let setter be ownDesc.[[Set]].
/// Step 5: If setter is undefined, return false.
/// Step 6: Perform ? Call(setter, Receiver, << V >>).
/// Step 7: Return true.
pub fn set_symbol_value(
  state: State,
  ref: Ref,
  key: SymbolId,
  val: JsValue,
  receiver: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(symbol_properties:, prototype:, ..)) ->
      // Step 1: Let ownDesc be O.[[GetOwnProperty]](P).
      case dict.get(symbol_properties, key) {
        // Step 1 (OrdinarySetWithOwnDescriptor): ownDesc is undefined.
        Error(_) ->
          case prototype {
            // Step 1.b: parent is not null → parent.[[Set]](P, V, Receiver).
            Some(proto_ref) ->
              set_symbol_value(state, proto_ref, key, val, receiver)
            // Step 1.c + 2.d: End of chain — create own data property on receiver.
            None -> {
              case receiver {
                JsObject(recv_ref) -> {
                  let h =
                    define_symbol_property(
                      state.heap,
                      recv_ref,
                      key,
                      value.data_property(val),
                    )
                  Ok(#(State(..state, heap: h), True))
                }
                // Step 2.b: Receiver is not an Object → return false.
                _ -> Ok(#(state, False))
              }
            }
          }
        // Step 2.a: ownDesc.[[Writable]] is false → return false.
        Ok(DataProperty(writable: False, ..)) -> Ok(#(state, False))
        // Step 2.d-e: Writable data property → create/update own on receiver.
        Ok(DataProperty(writable: True, ..)) -> {
          case receiver {
            JsObject(recv_ref) -> {
              let h =
                define_symbol_property(
                  state.heap,
                  recv_ref,
                  key,
                  value.data_property(val),
                )
              Ok(#(State(..state, heap: h), True))
            }
            // Step 2.b: Receiver is not an Object → return false.
            _ -> Ok(#(state, False))
          }
        }
        // Step 6-7: Accessor with setter → Call(setter, Receiver, << V >>), return true.
        Ok(AccessorProperty(set: Some(setter), ..)) -> {
          use #(_, state) <- result.map(
            frame.call(state, setter, receiver, [val]),
          )
          #(state, True)
        }
        // Step 5: setter is undefined → return false.
        Ok(AccessorProperty(set: None, ..)) -> Ok(#(state, False))
      }
    _ -> Ok(#(state, False))
  }
}

/// §10.1.6.1 OrdinaryDefineOwnProperty ( O, P, Desc ) — symbol-keyed variant.
///
/// Simplified: always inserts the property descriptor into the symbol_properties
/// dict without validation. Used internally by CopyDataProperties and
/// set_symbol_value where the caller has already validated the operation.
///
/// TODO(Deviation): no ValidateAndApplyPropertyDescriptor checks (extensibility,
/// existing property compatibility). Full descriptor validation needed for
/// Object.defineProperty with symbol keys.
fn define_symbol_property(
  heap: Heap,
  ref: Ref,
  key: SymbolId,
  prop: Property,
) -> Heap {
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(symbol_properties:, ..) -> {
      let new_sym_props = dict.insert(symbol_properties, key, prop)
      ObjectSlot(..slot, symbol_properties: new_sym_props)
    }
    _ -> slot
  }
}

// ============================================================================
// Inspect — debugging/REPL representation (read-only, no VM re-entry)
// ============================================================================

/// Produce a human-readable representation of a JS value (for REPL / console.log).
/// Read-only — does NOT call toString/valueOf or any JS code.
pub fn inspect(val: value.JsValue, heap: Heap) -> String {
  inspect_inner(val, heap, 0, set.new())
}

fn inspect_inner(
  val: value.JsValue,
  heap: Heap,
  depth: Int,
  visited: set.Set(Int),
) -> String {
  case val {
    value.JsUndefined -> "undefined"
    value.JsNull -> "null"
    value.JsBool(True) -> "true"
    value.JsBool(False) -> "false"
    value.JsNumber(value.Finite(n)) -> value.js_format_number(n)
    value.JsNumber(value.NaN) -> "NaN"
    value.JsNumber(value.Infinity) -> "Infinity"
    value.JsNumber(value.NegInfinity) -> "-Infinity"
    value.JsString(s) -> "'" <> s <> "'"
    value.JsSymbol(id) ->
      case value.well_known_symbol_description(id) {
        option.Some(desc) -> desc
        option.None -> "Symbol()"
      }
    value.JsBigInt(value.BigInt(n)) -> int.to_string(n) <> "n"
    value.JsUninitialized -> "<uninitialized>"
    value.JsObject(value.Ref(id:) as ref) ->
      case set.contains(visited, id) {
        True -> "[Circular]"
        False ->
          case depth > 2 {
            True -> "[Object]"
            False -> inspect_object(heap, ref, depth, set.insert(visited, id))
          }
      }
  }
}

fn inspect_object(
  heap: Heap,
  ref: value.Ref,
  depth: Int,
  visited: set.Set(Int),
) -> String {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind:, properties:, elements:, ..)) ->
      case kind {
        ArrayObject(length:) ->
          inspect_array(heap, elements, length, depth, visited)
        FunctionObject(..) -> {
          let name = case dict.get(properties, "name") {
            Ok(DataProperty(value: JsString(n), ..)) -> n
            _ -> "anonymous"
          }
          "[Function: " <> name <> "]"
        }
        NativeFunction(_) -> {
          let name = case dict.get(properties, "name") {
            Ok(DataProperty(value: JsString(n), ..)) -> n
            _ -> "native"
          }
          "[Function: " <> name <> "]"
        }
        PromiseObject(_) -> "Promise {}"
        GeneratorObject(_) -> "Object [Generator] {}"
        value.ArgumentsObject(length:) ->
          "[Arguments] "
          <> inspect_array(heap, elements, length, depth, visited)
        value.StringObject(value: s) -> "[String: '" <> s <> "']"
        value.NumberObject(value: n) ->
          "[Number: " <> inspect_inner(JsNumber(n), heap, depth, visited) <> "]"
        value.BooleanObject(value: True) -> "[Boolean: true]"
        value.BooleanObject(value: False) -> "[Boolean: false]"
        value.SymbolObject(value: sym) ->
          "[Symbol: "
          <> inspect_inner(value.JsSymbol(sym), heap, depth, visited)
          <> "]"
        value.PidObject(_) -> "Pid {}"
        value.MapObject(data:, ..) ->
          "Map(" <> int.to_string(dict.size(data)) <> ")"
        value.SetObject(data:, ..) ->
          "Set(" <> int.to_string(dict.size(data)) <> ")"
        value.WeakMapObject(_) -> "WeakMap {}"
        value.WeakSetObject(_) -> "WeakSet {}"
        value.RegExpObject(pattern:, flags:) -> {
          let source = case pattern {
            "" -> "(?:)"
            p -> p
          }
          "/" <> source <> "/" <> flags
        }
        OrdinaryObject -> inspect_plain_object(heap, properties, depth, visited)
      }
    _ -> "[Object]"
  }
}

fn inspect_array(
  heap: Heap,
  elements: JsElements,
  length: Int,
  depth: Int,
  visited: set.Set(Int),
) -> String {
  let items = inspect_array_loop(heap, elements, 0, length, depth, visited, [])
  "[ " <> string.join(items, ", ") <> " ]"
}

fn inspect_array_loop(
  heap: Heap,
  elements: JsElements,
  idx: Int,
  length: Int,
  depth: Int,
  visited: set.Set(Int),
  acc: List(String),
) -> List(String) {
  case idx >= length {
    True -> list.reverse(acc)
    False -> {
      let item = case js_elements.get_option(elements, idx) {
        Some(val) -> inspect_inner(val, heap, depth + 1, visited)
        None -> "<empty>"
      }
      inspect_array_loop(heap, elements, idx + 1, length, depth, visited, [
        item,
        ..acc
      ])
    }
  }
}

fn inspect_plain_object(
  heap: Heap,
  properties: dict.Dict(String, value.Property),
  depth: Int,
  visited: set.Set(Int),
) -> String {
  let entries =
    dict.to_list(properties)
    |> list.filter_map(fn(pair) {
      let #(key, prop) = pair
      case prop {
        DataProperty(enumerable: True, value: val, ..) ->
          Ok(key <> ": " <> inspect_inner(val, heap, depth + 1, visited))
        _ -> Error(Nil)
      }
    })
  case entries {
    [] -> "{}"
    _ -> "{ " <> string.join(entries, ", ") <> " }"
  }
}
