import arc/vm/builtins/common
import arc/vm/builtins/process_objects
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/opcode
import arc/vm/state.{type Heap, type HeapSlot, type State, State}
import arc/vm/value.{
  type JsElements, type JsValue, type Property, type PropertyKey, type Ref,
  type SymbolId, AccessorProperty, ArrayObject, DataProperty, Finite,
  FunctionObject, GeneratorObject, Index, JsNumber, JsObject, JsString, Named,
  NativeFunction, ObjectSlot, OrdinaryObject, PromiseObject,
}
import gleam/bit_array
import gleam/bool
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
///   - String primitives synthesize index properties inline (matching
///     §10.4.3.5 StringGetOwnProperty) and "length" inline (an ordinary own
///     data property created by §10.4.3.4 StringCreate) without a StringObject.
///   - Number/Boolean/Symbol primitives jump straight to prototype [[Get]].
///   - null/undefined return undefined instead of throwing TypeError (callers
///     are expected to guard against this before calling get_value_of).
pub fn get_value_of(
  state: State,
  val: JsValue,
  key: PropertyKey,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case val {
    // §7.3.3 step 1: V is already an Object, call O.[[Get]](P, V) directly.
    JsObject(ref) -> get_value(state, ref, key, val)
    JsString(s) ->
      // String primitive: synthesize own properties per §10.4.3.5
      // StringGetOwnProperty, then fall through to String.prototype.
      case key {
        // §10.4.3.4 StringCreate: "length" is an ordinary own data property
        // {value: len, W:F, E:F, C:F} (NOT produced by StringGetOwnProperty).
        Named("length") -> Ok(#(value.from_int(string_length(s)), state))
        // §10.4.3.5 steps 2-10: numeric index → single-char string
        Index(idx) ->
          case string_char_at(s, idx) {
            Some(ch) -> Ok(#(JsString(ch), state))
            // Out of bounds — delegate to String.prototype via [[Get]]
            None -> get_value(state, state.builtins.string.prototype, key, val)
          }
        // Not an own property — delegate to String.prototype via [[Get]]
        Named(_) -> get_value(state, state.builtins.string.prototype, key, val)
      }
    // Primitive→prototype delegation (ToObject would wrap, we skip the wrapper)
    JsNumber(_) -> get_value(state, state.builtins.number.prototype, key, val)
    value.JsBool(_) ->
      get_value(state, state.builtins.boolean.prototype, key, val)
    value.JsSymbol(_) -> get_value(state, state.builtins.symbol_proto, key, val)
    // BigInt primitive → %BigInt.prototype% (toString/valueOf/…).
    value.JsBigInt(_) -> get_value(state, state.builtins.bigint_proto, key, val)
    // null/undefined → JsUndefined; callers guard and throw TypeError as needed.
    _ -> Ok(#(value.JsUndefined, state))
  }
}

/// Symbol-keyed variant of get_value_of — delegates to the primitive's
/// prototype for symbol lookups (e.g. `"str"[Symbol.iterator]`).
pub fn get_symbol_value_of(
  state: State,
  val: JsValue,
  sym: SymbolId,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case val {
    JsObject(ref) -> get_symbol_value(state, ref, sym, val)
    JsString(_) ->
      get_symbol_value(state, state.builtins.string.prototype, sym, val)
    JsNumber(_) ->
      get_symbol_value(state, state.builtins.number.prototype, sym, val)
    value.JsBool(_) ->
      get_symbol_value(state, state.builtins.boolean.prototype, sym, val)
    value.JsSymbol(_) ->
      get_symbol_value(state, state.builtins.symbol_proto, sym, val)
    value.JsBigInt(_) ->
      get_symbol_value(state, state.builtins.bigint_proto, sym, val)
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
  key: PropertyKey,
  receiver: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case heap.read(state.heap, ref) {
    // §10.4.6.8 Module Namespace [[Get]]: read the live binding cell, throwing
    // ReferenceError if it is still uninitialized (TDZ). Non-export keys fall
    // through to undefined (null prototype, no inheritance).
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
      namespace_get(state, exports, key)
    // §10.5.8 Proxy [[Get]] — route through the trap machinery.
    Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) ->
      proxy_get(state, target, handler, PkString(key), receiver)
    Some(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) ->
      case kind, key {
        // Fast paths for synthesized descriptors: own_property_of_slot would
        // allocate a fresh Some(DataProperty(..)) box per read just for us to
        // destructure it below. Return the value directly instead.
        // --- Array exotic: virtual "length" (§10.4.2) ---
        ArrayObject(length:), Named("length") ->
          Ok(#(value.from_int(length), state))
        // --- Array/Arguments exotic: dense element storage ---
        // properties dict is authoritative (accessor/attribute overrides set
        // via defineProperty); elements is the fast-path data-value cache.
        ArrayObject(_), Index(idx) | value.ArgumentsObject(_), Index(idx) ->
          case dict.get(properties, key) {
            // Override at this index wins — full descriptor semantics.
            Ok(prop) -> property_get_value(state, prop, receiver)
            Error(Nil) ->
              case elements.get_option(elements, idx) {
                Some(val) -> Ok(#(val, state))
                // Hole — walk prototype chain (step 2).
                None ->
                  get_value_from_prototype(state, prototype, key, receiver)
              }
          }
        // --- TypedArray exotic [[Get]] (§10.4.5.4) ---
        // Canonical numeric index: IntegerIndexedElementGet — element value
        // or undefined, never the prototype chain.
        value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:),
          Index(idx)
        -> {
          let length =
            typed_array_view_length(
              state.heap,
              buffer,
              elem_kind,
              byte_offset,
              length,
            )
          case
            typed_array_element(
              state.heap,
              buffer,
              elem_kind,
              byte_offset,
              length,
              idx,
            )
          {
            Some(v) -> Ok(#(v, state))
            None -> Ok(#(value.JsUndefined, state))
          }
        }
        value.TypedArrayObject(..), Named(s) ->
          case is_canonical_numeric_string(s) {
            True -> Ok(#(value.JsUndefined, state))
            False ->
              case dict.get(properties, key) {
                Ok(prop) -> property_get_value(state, prop, receiver)
                Error(Nil) ->
                  get_value_from_prototype(state, prototype, key, receiver)
              }
          }
        _, _ ->
          // Step 1: Let desc be ? O.[[GetOwnProperty]](P).
          case
            own_property_of_slot(state.heap, kind, properties, elements, key)
          {
            // Steps 3-7: branch on the descriptor type.
            Some(prop) -> property_get_value(state, prop, receiver)
            // Step 2: desc is undefined — walk prototype chain.
            None -> get_value_from_prototype(state, prototype, key, receiver)
          }
      }
    _ -> Ok(#(value.JsUndefined, state))
  }
}

/// §10.4.6.8 Module Namespace [[Get]] for a string key. Resolves the export's
/// live BoxSlot and returns its value, throwing ReferenceError when the binding
/// is still uninitialized (TDZ). Unknown keys return undefined.
fn namespace_get(
  state: State,
  exports: dict.Dict(String, Ref),
  key: PropertyKey,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  let name = case key {
    Named(n) -> n
    Index(i) -> int.to_string(i)
  }
  case dict.get(exports, name) {
    Error(Nil) -> Ok(#(value.JsUndefined, state))
    Ok(box) ->
      case heap.read_box(state.heap, box) {
        Some(value.JsUninitialized) ->
          Error(state.reference_error_value(
            state,
            "Cannot access '" <> name <> "' before initialization",
          ))
        Some(val) -> Ok(#(val, state))
        None -> Ok(#(value.JsUndefined, state))
      }
  }
}

/// For a Module Namespace, throw ReferenceError if any of `names` is an
/// uninitialized (TDZ) binding. §10.4.6.5 [[GetOwnProperty]] eagerly performs
/// [[Get]], so even key-only operations (Object.keys, hasOwnProperty, for-in)
/// surface the TDZ error. A no-op for non-namespace objects.
pub fn namespace_tdz_guard(
  state: State,
  ref: Ref,
  names: List(String),
) -> Result(State, #(JsValue, State)) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
      list.try_fold(names, state, fn(state, name) {
        case dict.get(exports, name) {
          Ok(box) ->
            case heap.read_box(state.heap, box) {
              Some(value.JsUninitialized) ->
                Error(state.reference_error_value(
                  state,
                  "Cannot access '" <> name <> "' before initialization",
                ))
              _ -> Ok(state)
            }
          Error(Nil) -> Ok(state)
        }
      })
    _ -> Ok(state)
  }
}

/// §10.1.8.1 OrdinaryGet steps 3-7 given a found own descriptor:
/// data → desc.[[Value]]; accessor → Call(getter, Receiver) or undefined.
pub fn property_get_value(
  state: State,
  prop: Property,
  receiver: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case prop {
    // Step 3: IsDataDescriptor(desc) → return desc.[[Value]].
    DataProperty(value: val, ..) -> Ok(#(val, state))
    // Steps 5-7: IsAccessorDescriptor → Call(getter, Receiver) or undefined.
    AccessorProperty(get: Some(getter), ..) ->
      state.call(state, getter, receiver, [])
    AccessorProperty(get: None, ..) -> Ok(#(value.JsUndefined, state))
  }
}

/// §10.1.8.1 OrdinaryGet step 2: desc is undefined — walk prototype chain.
fn get_value_from_prototype(
  state: State,
  prototype: Option(Ref),
  key: PropertyKey,
  receiver: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case prototype {
    // Step 2c: parent.[[Get]](P, Receiver)
    Some(proto_ref) -> get_value(state, proto_ref, key, receiver)
    // Step 2b: parent is null → return undefined.
    None -> Ok(#(value.JsUndefined, state))
  }
}

/// Result of a fast own-index probe (see `get_own_index`). On the dense
/// Array/Arguments elements path the value is returned directly instead of
/// being boxed into a synthesized Some(DataProperty(..)) descriptor.
pub type OwnIndex {
  /// Own data value found in dense element storage (no descriptor allocated).
  OwnIndexValue(JsValue)
  /// Own property found in the properties dict (override or non-exotic) —
  /// the dict's existing record, full descriptor semantics apply.
  OwnIndexProperty(Property)
  /// No own property at this index. Carries the slot's prototype (already
  /// read from the heap) so callers can continue the prototype-chain walk
  /// without re-reading the same slot. None when the receiver has no
  /// prototype or is not an object slot.
  OwnIndexAbsent(prototype: Option(Ref))
}

/// Fast variant of `get_own_property` for integer index keys. Identical
/// semantics, but on the Array/Arguments dense-elements hit it skips the
/// Some(DataProperty(..)) descriptor synthesis and returns the value directly.
pub fn get_own_index(heap: Heap, ref: Ref, idx: Int) -> OwnIndex {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) ->
      case kind {
        ArrayObject(_) | value.ArgumentsObject(_) ->
          case dict.get(properties, Index(idx)) {
            // accessor/attribute override at this index wins
            Ok(prop) -> OwnIndexProperty(prop)
            Error(Nil) ->
              case elements.get_option(elements, idx) {
                Some(val) -> OwnIndexValue(val)
                None -> OwnIndexAbsent(prototype)
              }
          }
        _ ->
          case
            own_property_of_slot(heap, kind, properties, elements, Index(idx))
          {
            Some(prop) -> OwnIndexProperty(prop)
            None -> OwnIndexAbsent(prototype)
          }
      }
    _ -> OwnIndexAbsent(None)
  }
}

/// Get the character at codepoint index `idx`, or None if out of bounds.
///
/// Implements **StringGetOwnProperty** §10.4.3.5 steps 8-10.
///
/// FFI walks UTF-8 codepoints directly — ~20x faster than gleam/string.slice
/// which does grapheme cluster segmentation via unicode_util:gc.
///
/// TODO(Deviation): JS indexes by UTF-16 code unit, so astral-plane chars
/// should count as 2 indices. Codepoint indexing matches code-unit indexing
/// for all BMP chars, so this is strictly more correct than grapheme
/// indexing was. Full fix needs UTF-16 string storage.
@external(erlang, "arc_vm_ffi", "string_char_at")
pub fn string_char_at(s: String, idx: Int) -> Option(String)

/// Codepoint count — ~20x faster than gleam/string.length (no grapheme
/// clustering). Same UTF-16 deviation as string_char_at.
@external(erlang, "arc_vm_ffi", "string_codepoint_length")
pub fn string_length(s: String) -> Int

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
///   **StringGetOwnProperty(S, P)** — §10.4.3.5 (10 steps; index keys only):
///     1. If P is not a String, return undefined.
///     2. Let index be CanonicalNumericIndexString(P). If undefined, return undefined.
///     3-4. If index is not an integral Number, or is -0, return undefined.
///     5-6. Let stringData be S.[[StringData]].
///     7. Let len be the length of stringData.
///     8. If index < 0 or index >= len, return undefined.
///     9-10. Return {value: substring at index, W:false, E:true, C:false}.
///   ("length" is NOT handled here — it is an ordinary own property created
///    by §10.4.3.4 StringCreate and returned at §10.4.3.1 step 1.)
///
/// For **Arguments exotic**: indices from elements storage, everything else
///   (including "length" and "callee") from the properties dict.
///
/// We check "length" as a special string key for Array and String objects
/// rather than routing through a separate internal slot. The properties dict
/// lookup for ordinary objects (step 1) is inlined.
pub fn get_own_property(
  heap: Heap,
  ref: Ref,
  key: PropertyKey,
) -> Option(Property) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
      namespace_own_property(heap, exports, key)
    Some(ObjectSlot(kind:, properties:, elements:, ..)) ->
      own_property_of_slot(heap, kind, properties, elements, key)
    _ -> None
  }
}

/// §10.4.6.5 Module Namespace [[GetOwnProperty]] for a string key: a data
/// descriptor { value: <live binding>, writable: true, enumerable: true,
/// configurable: false }. The value is read from the BoxSlot (so it may be
/// JsUninitialized — callers that read the value, e.g. getOwnPropertyDescriptor,
/// must surface the TDZ ReferenceError). Unknown keys → None.
fn namespace_own_property(
  heap: Heap,
  exports: dict.Dict(String, Ref),
  key: PropertyKey,
) -> Option(Property) {
  case dict.get(exports, value.key_to_string(key)) {
    Error(Nil) -> None
    Ok(box) -> {
      let val = heap.read_box(heap, box) |> option.unwrap(value.JsUndefined)
      Some(value.data(val) |> value.writable() |> value.enumerable())
    }
  }
}

/// **[[GetOwnProperty]](P)** dispatch given an already-read ObjectSlot's
/// fields. Same algorithm as get_own_property; callers that already hold the
/// slot use this directly instead of re-reading the heap.
///
/// INVARIANT (Array/Arguments index keys): properties dict is checked FIRST
/// (authoritative for accessor/attribute overrides), then dense elements
/// storage. This dict-then-elements probe is duplicated in three fast paths
/// that bypass this function to avoid descriptor boxing:
///   - `get_value` (Array/Arguments Index fast path)
///   - `get_own_index`
///   - `set_value` (Array/Arguments Index write fast path)
/// Any change to the storage invariant here MUST be mirrored in all of them,
/// or get/set/getOwnPropertyDescriptor will silently diverge.
fn own_property_of_slot(
  heap: Heap,
  kind: state.ExoticKind,
  properties: dict.Dict(PropertyKey, Property),
  elements: JsElements,
  key: PropertyKey,
) -> Option(Property) {
  case kind {
    // --- TypedArray (Integer-Indexed) exotic [[GetOwnProperty]] (§10.4.5.1) ---
    // Canonical numeric index keys map to buffer elements:
    //   in-bounds → { value, W:T, E:T, C:T }; out-of-bounds/detached →
    //   undefined WITHOUT consulting the ordinary table. Non-integral
    //   canonical numeric strings ("1.5", "-0", "NaN", …) are never valid
    //   indices, so they also yield undefined.
    value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:) ->
      case key {
        Index(idx) -> {
          let length =
            typed_array_view_length(
              heap,
              buffer,
              elem_kind,
              byte_offset,
              length,
            )
          typed_array_element(heap, buffer, elem_kind, byte_offset, length, idx)
          |> option.map(value.data_property)
        }
        Named(s) ->
          case is_canonical_numeric_string(s) {
            True -> None
            False -> dict_get_option(properties, key)
          }
      }
    // --- Array exotic [[GetOwnProperty]] (§10.4.2) ---
    // Per spec this IS OrdinaryGetOwnProperty — arrays only override
    // [[DefineOwnProperty]]. Our elements/properties split is an internal
    // optimization: properties dict is authoritative (holds accessors set
    // via Object.defineProperty(arr, "0", {get:...})), elements is the
    // fast-path data-value cache. Check properties first.
    ArrayObject(length:) ->
      case key {
        // Virtual "length" property (§10.4.2.4 ArraySetLength). A dict
        // override holds the attributes after defineProperty made it
        // non-writable; the value always tracks ArrayObject(length).
        Named("length") ->
          case dict_get_option(properties, key) {
            Some(DataProperty(writable:, enumerable:, configurable:, ..)) ->
              Some(DataProperty(
                value: value.from_int(length),
                writable:,
                enumerable:,
                configurable:,
              ))
            _ ->
              Some(DataProperty(
                value: value.from_int(length),
                writable: True,
                enumerable: False,
                configurable: False,
              ))
          }
        Index(idx) ->
          case dict_get_option(properties, key) {
            // accessor override at this index wins
            Some(prop) -> Some(prop)
            // Single traversal: get_option returns None for holes, so this is
            // equivalent to has+get but does one tree lookup instead of two.
            None ->
              option.map(
                elements.get_option(elements, idx),
                value.data_property,
              )
          }
        Named(_) -> dict_get_option(properties, key)
      }
    // --- Arguments exotic [[GetOwnProperty]] (§10.4.4.1) ---
    // Spec layers a [[ParameterMap]] over OrdinaryGetOwnProperty; our
    // elements/properties split is an internal storage optimization.
    value.ArgumentsObject(_) ->
      case key {
        Index(idx) ->
          case dict_get_option(properties, key) {
            Some(prop) -> Some(prop)
            None ->
              option.map(
                elements.get_option(elements, idx),
                value.data_property,
              )
          }
        Named(_) -> dict_get_option(properties, key)
      }
    // --- String exotic [[GetOwnProperty]] (§10.4.3.1) ---
    value.StringObject(value: s) ->
      case key {
        // §10.4.3.4 StringCreate: "length" is an ordinary own data
        // property {value: len, W:F, E:F, C:F}; returned at §10.4.3.1 step 1.
        Named("length") ->
          Some(DataProperty(
            value: value.from_int(string_length(s)),
            writable: False,
            enumerable: False,
            configurable: False,
          ))
        // §10.4.3.5 steps 2-4: CanonicalNumericIndexString → integer index
        Index(idx) ->
          case string_char_at(s, idx) {
            // §10.4.3.5 step 10: return {value: char, W:F, E:T, C:F}
            Some(ch) ->
              Some(DataProperty(
                value: JsString(ch),
                writable: False,
                enumerable: True,
                configurable: False,
              ))
            // §10.4.3.5 step 8: index >= len → undefined, fall to ordinary
            None -> dict_get_option(properties, key)
          }
        // §10.4.3.1 step 1: OrdinaryGetOwnProperty is called first for ALL
        // keys; for non-index keys StringGetOwnProperty (step 3) yields
        // undefined, so the ordinary result is the only answer.
        Named(_) -> dict_get_option(properties, key)
      }
    // --- Ordinary [[GetOwnProperty]] (§10.1.5.1) ---
    _ -> dict_get_option(properties, key)
  }
}

/// Helper: dict.get but returns Option instead of Result.
/// Implements §10.1.5.1 OrdinaryGetOwnProperty: step 1 returns undefined when
/// the key is not an own property; steps 2-8 build and return the descriptor
/// when it is. We collapse both into a single Option lookup.
fn dict_get_option(
  d: dict.Dict(PropertyKey, Property),
  key: PropertyKey,
) -> Option(Property) {
  dict.get(d, key) |> option.from_result
}

/// §10.1.9.1 OrdinarySet ( O, P, V, Receiver )
/// Combined with §10.1.9.2 OrdinarySetWithOwnDescriptor.
///
/// Walks the proto chain. Handles accessors (calls setter), non-writable proto
/// blocking, and creates own data property on receiver when not found.
pub fn set_value(
  state: State,
  ref: Ref,
  key: PropertyKey,
  val: JsValue,
  receiver: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  case heap.read(state.heap, ref) {
    // §10.4.6.9 Module Namespace [[Set]]: always returns false (read-only).
    Some(ObjectSlot(kind: value.ModuleNamespace(..), ..)) -> Ok(#(state, False))
    // §10.5.9 Proxy [[Set]] — route through the trap machinery.
    Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) ->
      proxy_set(state, target, handler, PkString(key), val, receiver)
    Some(
      ObjectSlot(kind:, properties:, elements:, prototype:, extensible:, ..) as slot,
    ) ->
      case kind, key {
        // Write-side fast path mirroring get_value's Array/Arguments Index
        // fast path: own_property_of_slot would allocate a fresh
        // Some(DataProperty(..)) box per write just for the descriptor
        // dispatch below to destructure `writable: True` and discard it.
        // Probe the dict (authoritative for accessor/attribute overrides)
        // then dense element presence directly instead.
        ArrayObject(_), Index(idx) | value.ArgumentsObject(_), Index(idx) ->
          case dict.get(properties, key) {
            // Override at this index wins — full descriptor semantics.
            Ok(prop) ->
              set_value_with_own_descriptor(
                state,
                ref,
                slot,
                prototype,
                Some(prop),
                key,
                val,
                receiver,
              )
            Error(Nil) ->
              case elements.has(elements, idx) {
                // Present dense element: an own writable/enumerable/
                // configurable data property by construction — go straight
                // to the receiver write, no descriptor boxing.
                True ->
                  set_on_receiver_with_slot(
                    state,
                    receiver,
                    ref,
                    slot,
                    key,
                    val,
                  )
                // Hole — proto chain may hold a setter; spec walk.
                False ->
                  set_value_with_own_descriptor(
                    state,
                    ref,
                    slot,
                    prototype,
                    None,
                    key,
                    val,
                    receiver,
                  )
              }
          }
        // --- TypedArray exotic [[Set]] (§10.4.5.5) ---
        // Canonical numeric index, SameValue(O, Receiver) →
        // IntegerIndexedElementSet (§10.4.5.16): convert the value
        // (observable, may call user code), then store if the index is
        // valid; out-of-bounds/detached writes are silent no-ops.
        // Receiver differs from O (Reflect.set / prototype-chain set):
        // step 1.b.ii — invalid index → true with NO value conversion;
        // valid index → OrdinarySet creates the property on the Receiver,
        // leaving the buffer untouched.
        value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:),
          Index(idx)
        ->
          case receiver == JsObject(ref) {
            True ->
              typed_array_store(
                state,
                buffer,
                elem_kind,
                byte_offset,
                length,
                Some(idx),
                val,
              )
            False -> {
              let length =
                typed_array_view_length(
                  state.heap,
                  buffer,
                  elem_kind,
                  byte_offset,
                  length,
                )
              case
                typed_array_element(
                  state.heap,
                  buffer,
                  elem_kind,
                  byte_offset,
                  length,
                  idx,
                )
              {
                None -> Ok(#(state, True))
                Some(_) -> set_on_receiver(state, receiver, key, val)
              }
            }
          }
        value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:),
          Named(s)
        ->
          case is_canonical_numeric_string(s) {
            // Canonical numeric but never a valid index ("1.5", "-0", "NaN"):
            // with Receiver == O run the conversion for its side effects,
            // then succeed silently; with a foreign Receiver return true
            // without any conversion (§10.4.5.5 step 1.b.ii).
            True ->
              case receiver == JsObject(ref) {
                True ->
                  typed_array_store(
                    state,
                    buffer,
                    elem_kind,
                    byte_offset,
                    length,
                    None,
                    val,
                  )
                False -> Ok(#(state, True))
              }
            False -> {
              let own =
                own_property_of_slot(
                  state.heap,
                  kind,
                  properties,
                  elements,
                  key,
                )
              set_value_with_own_descriptor(
                state,
                ref,
                slot,
                prototype,
                own,
                key,
                val,
                receiver,
              )
            }
          }
        _, _ -> {
          // §10.1.9.1 step 1: Let ownDesc be ? O.[[GetOwnProperty]](P).
          let own =
            own_property_of_slot(state.heap, kind, properties, elements, key)
          // Fused-write eligibility: True only when the key's own property
          // (if any) is guaranteed to live in the properties dict AND a plain
          // value write routes to set_string_property (the dict update). That
          // lets the fused branches below reuse the own_property_of_slot
          // lookup instead of re-doing dict.get inside set_string_property —
          // one dict.get + one dict.insert per write instead of two + one.
          //
          // Exclusions (must keep taking the descriptor-dispatch slow path):
          //   - Index keys: Array/Arguments store them in elements, and Array
          //     index writes also bump length (§10.4.2.1 step 2); String
          //     index writes are guarded (§10.4.3.2 step 2). (Array/Arguments
          //     Index never reaches here — handled by the fast path above —
          //     but String/ordinary Index does.)
          //   - Array "length": virtual property, writes go through
          //     ArraySetLength (§10.4.2.4).
          //   - String "length": synthesized non-writable descriptor
          //     (§10.4.3.4), not present in the dict.
          let fusable = case key, kind {
            Index(_), _ -> False
            Named("length"), ArrayObject(_) -> False
            Named("length"), value.StringObject(_) -> False
            Named(_), _ -> True
          }
          case fusable, receiver, own, prototype {
            // Fused update (§10.1.9.2 steps 2.b-2.h with Receiver == O):
            // the own writable data property we just found in the dict is
            // exactly the existingDescriptor the receiver half would re-fetch
            // (§10.1.6.3 step 6.c), so thread its enumerable/configurable
            // flags straight into dict.insert.
            True,
              JsObject(recv_ref),
              Some(DataProperty(
                writable: True,
                enumerable:,
                configurable:,
                value: _,
              )),
              _
              if recv_ref == ref
            -> {
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
              let h =
                heap.write(
                  state.heap,
                  ref,
                  ObjectSlot(..slot, properties: new_props),
                )
              Ok(#(State(..state, heap: h), True))
            }
            // Fused create (§10.1.9.2 step 1.c + §10.1.6.3 steps 2.a-2.e):
            // own_property_of_slot already proved the key is absent from the
            // dict, there is no prototype to walk, and extensible came with
            // the slot — skip set_string_property's redundant dict.get.
            True, JsObject(recv_ref), None, None if recv_ref == ref ->
              case extensible {
                False -> Ok(#(state, False))
                True -> {
                  let new_props =
                    dict.insert(properties, key, value.data_property(val))
                  let h =
                    heap.write(
                      state.heap,
                      ref,
                      ObjectSlot(..slot, properties: new_props),
                    )
                  Ok(#(State(..state, heap: h), True))
                }
              }
            // §10.1.9.1 step 2: Return OrdinarySetWithOwnDescriptor(O, P, V, Receiver, ownDesc).
            _, _, _, _ ->
              set_value_with_own_descriptor(
                state,
                ref,
                slot,
                prototype,
                own,
                key,
                val,
                receiver,
              )
          }
        }
      }
    _ -> set_on_receiver(state, receiver, key, val)
  }
}

/// §10.1.9.2 OrdinarySetWithOwnDescriptor ( O, P, V, Receiver, ownDesc )
/// given an already-computed ownDesc for `ref`'s already-read slot.
fn set_value_with_own_descriptor(
  state: State,
  ref: Ref,
  slot: HeapSlot,
  prototype: Option(Ref),
  own: Option(Property),
  key: PropertyKey,
  val: JsValue,
  receiver: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  case own {
    // §10.1.9.2 step 1: If ownDesc is undefined, then
    //   step 1.a: Let parent be ? O.[[GetPrototypeOf]]().
    None ->
      case prototype {
        // §10.1.9.2 step 1.b: If parent is not null, return ? parent.[[Set]](P, V, Receiver).
        Some(proto_ref) -> set_value(state, proto_ref, key, val, receiver)
        // §10.1.9.2 step 1.c: Else, set ownDesc to {[[Value]]: undefined, [[Writable]]: true,
        //   [[Enumerable]]: true, [[Configurable]]: true}.
        // (Falls through to set_on_receiver which creates the property.)
        None -> set_on_receiver_with_slot(state, receiver, ref, slot, key, val)
      }
    // §10.1.9.2 step 2: If IsDataDescriptor(ownDesc) is true, then
    //   step 2.a: If ownDesc.[[Writable]] is false, return false.
    Some(DataProperty(writable: False, ..)) -> Ok(#(state, False))
    // §10.1.9.2 steps 2.b-2.h: ownDesc is writable data — delegate to receiver.
    // We delegate to set_on_receiver which handles both create and update
    // via set_property (spec distinguishes step 2.d.ii CreateDataProperty vs
    // step 2.h OrdinaryDefineOwnProperty but result is same).
    Some(DataProperty(writable: True, ..)) ->
      set_on_receiver_with_slot(state, receiver, ref, slot, key, val)
    // §10.1.9.2 step 3: Assert: ownDesc is an accessor descriptor.
    //   step 4: Let setter be ownDesc.[[Set]].
    //   step 5: If setter is undefined, return false.
    Some(AccessorProperty(set: None, ..)) -> Ok(#(state, False))
    // §10.1.9.2 step 6: Perform ? Call(setter, Receiver, « V »).
    // §10.1.9.2 step 7: Return true.
    Some(AccessorProperty(set: Some(setter), ..)) -> {
      use #(_, state) <- result.map(state.call(state, setter, receiver, [val]))
      #(state, True)
    }
  }
}

/// §10.1.9.2 OrdinarySetWithOwnDescriptor steps 2.b-2.h (receiver half).
///
/// Create or update an own data property on the receiver. Shared by
/// set_value's "not found in proto chain" and "writable proto data" branches.
///
/// The spec distinguishes "receiver has no own property" (step 2.d.ii —
/// CreateDataProperty) vs "receiver has existing own property" (step 2.h —
/// OrdinaryDefineOwnProperty with {[[Value]]: V}). We delegate both cases to
/// set_property which handles the distinction internally with identical semantics.
///
/// §10.1.9.2 step 2.b: If receiver is not an Object, return false.
fn set_on_receiver(
  state: State,
  receiver: JsValue,
  key: PropertyKey,
  val: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  case receiver {
    JsObject(recv_ref) ->
      case heap.read(state.heap, recv_ref) {
        // §10.1.9.2 step 2.e: Receiver.[[GetOwnProperty]](P). For a module
        // namespace this performs [[Get]] on the binding, which throws a
        // ReferenceError when the export is still in TDZ. The set never
        // succeeds (namespaces aren't extensible), so return False afterwards.
        Some(ObjectSlot(kind: value.ModuleNamespace(..), ..)) -> {
          let names = case key {
            Named(n) -> [n]
            _ -> []
          }
          use state <- result.try(namespace_tdz_guard(state, recv_ref, names))
          Ok(#(state, False))
        }
        // §10.1.9.2 steps 2.c-2.e with a PROXY receiver (Reflect.set with a
        // proxy receiver, or [[Set]] forwarded through a trapless proxy):
        // the GetOwnProperty/DefineOwnProperty pair must go through traps.
        Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) ->
          set_on_proxy_receiver(state, target, handler, PkString(key), val)
        // Receiver is itself an Integer-Indexed object: the receiver half of
        // OrdinarySet routes numeric index keys through the receiver's
        // [[DefineOwnProperty]] (§10.4.5.3) → IntegerIndexedElementSet for a
        // valid index, false (no conversion) for an invalid one. Non-numeric
        // keys fall through to the ordinary dict write below.
        Some(ObjectSlot(
          kind: value.TypedArrayObject(
            buffer:,
            elem_kind:,
            byte_offset:,
            length:,
          ),
          ..,
        )) ->
          case key {
            Index(idx) ->
              case
                typed_array_element(
                  state.heap,
                  buffer,
                  elem_kind,
                  byte_offset,
                  typed_array_view_length(
                    state.heap,
                    buffer,
                    elem_kind,
                    byte_offset,
                    length,
                  ),
                  idx,
                )
              {
                Some(_) ->
                  typed_array_store(
                    state,
                    buffer,
                    elem_kind,
                    byte_offset,
                    length,
                    Some(idx),
                    val,
                  )
                None -> Ok(#(state, False))
              }
            Named(s) ->
              case is_canonical_numeric_string(s) {
                // Canonical numeric, never a valid index → CreateDataProperty
                // → [[DefineOwnProperty]] → false, with no value conversion.
                True -> Ok(#(state, False))
                False -> {
                  let #(h, ok) = set_property(state.heap, recv_ref, key, val)
                  Ok(#(State(..state, heap: h), ok))
                }
              }
          }
        // §10.1.9.2 steps 2.c-2.h: ordinary object — define/update own property.
        _ -> {
          let #(h, ok) = set_property(state.heap, recv_ref, key, val)
          Ok(#(State(..state, heap: h), ok))
        }
      }
    // §10.1.9.2 step 2.b: Receiver is not an Object, return false.
    _ -> Ok(#(state, False))
  }
}

/// CPS guard: when `recv_ref` is a proxy, route the receiver write through
/// the proxy traps instead of the ordinary continuation.
fn proxy_receiver_guard(
  state: State,
  recv_ref: Ref,
  pk: ProxyKey,
  val: JsValue,
  cont: fn() -> Result(#(State, Bool), #(JsValue, State)),
) -> Result(#(State, Bool), #(JsValue, State)) {
  case as_proxy(state.heap, recv_ref) {
    Some(#(target, handler)) ->
      set_on_proxy_receiver(state, target, handler, pk, val)
    None -> cont()
  }
}

/// §10.1.9.2 OrdinarySetWithOwnDescriptor steps 2.c-2.e for a proxy receiver:
/// existingDescriptor = ? Receiver.[[GetOwnProperty]](P); if a data property,
/// Receiver.[[DefineOwnProperty]](P, { [[Value]]: V }); if absent,
/// CreateDataProperty(Receiver, P, V). Both go through the proxy's traps.
fn set_on_proxy_receiver(
  state: State,
  target: Option(Ref),
  handler: Option(Ref),
  pk: ProxyKey,
  val: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  use #(existing, state) <- result.try(proxy_receiver_get_own(
    state,
    target,
    handler,
    pk,
  ))
  case existing {
    // Step 2.d.i-ii: accessor or non-writable existing → false.
    Some(AccessorProperty(..)) -> Ok(#(state, False))
    Some(DataProperty(writable: False, ..)) -> Ok(#(state, False))
    Some(DataProperty(..)) ->
      proxy_receiver_define(state, target, handler, pk, val, False)
    None -> proxy_receiver_define(state, target, handler, pk, val, True)
  }
}

/// Receiver-side Proxy [[GetOwnProperty]] — simplified (no invariant checks;
/// they are enforced on the read path in builtins/object). Used only by the
/// OrdinarySet receiver steps above.
fn proxy_receiver_get_own(
  state: State,
  target: Option(Ref),
  handler: Option(Ref),
  pk: ProxyKey,
) -> Result(#(Option(Property), State), #(JsValue, State)) {
  use #(t, h, trap, state) <- result.try(proxy_trap(
    state,
    target,
    handler,
    "getOwnPropertyDescriptor",
  ))
  case trap {
    None ->
      case as_proxy(state.heap, t) {
        Some(#(t2, h2)) -> proxy_receiver_get_own(state, t2, h2, pk)
        None -> Ok(#(target_own_property(state.heap, t, pk), state))
      }
    Some(trap_fn) -> {
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [JsObject(t), pk_value(pk)]),
      )
      case res {
        value.JsUndefined | value.JsNull -> Ok(#(None, state))
        JsObject(desc_ref) -> {
          // Minimal ToPropertyDescriptor: read the fields off the returned
          // object (own data reads only — descriptor objects are plain).
          let read = fn(name) {
            case get_own_property(state.heap, desc_ref, Named(name)) {
              Some(DataProperty(value: v, ..)) -> Some(v)
              _ -> None
            }
          }
          let get_f = read("get")
          let set_f = read("set")
          case get_f, set_f {
            None, None ->
              Ok(#(
                Some(DataProperty(
                  value: read("value") |> option.unwrap(value.JsUndefined),
                  writable: read("writable")
                    |> option.map(value.is_truthy)
                    |> option.unwrap(False),
                  enumerable: read("enumerable")
                    |> option.map(value.is_truthy)
                    |> option.unwrap(False),
                  configurable: read("configurable")
                    |> option.map(value.is_truthy)
                    |> option.unwrap(False),
                )),
                state,
              ))
            _, _ ->
              Ok(#(
                Some(AccessorProperty(
                  get: get_f,
                  set: set_f,
                  enumerable: read("enumerable")
                    |> option.map(value.is_truthy)
                    |> option.unwrap(False),
                  configurable: read("configurable")
                    |> option.map(value.is_truthy)
                    |> option.unwrap(False),
                )),
                state,
              ))
          }
        }
        _ -> Ok(#(None, state))
      }
    }
  }
}

/// Receiver-side Proxy [[DefineOwnProperty]] — calls the defineProperty trap
/// with `{ value: V }` (existing data property) or the full CreateDataProperty
/// descriptor (absent property). No invariant checks (receiver-write path).
fn proxy_receiver_define(
  state: State,
  target: Option(Ref),
  handler: Option(Ref),
  pk: ProxyKey,
  val: JsValue,
  full: Bool,
) -> Result(#(State, Bool), #(JsValue, State)) {
  use #(t, h, trap, state) <- result.try(proxy_trap(
    state,
    target,
    handler,
    "defineProperty",
  ))
  case trap {
    // No trap → target.[[DefineOwnProperty]] — i.e. the ordinary receiver
    // write on the (possibly nested-proxy) target.
    None ->
      case pk {
        PkString(key) -> set_on_receiver(state, JsObject(t), key, val)
        PkSymbol(sym) ->
          define_symbol_data_on_receiver(state, JsObject(t), sym, val)
      }
    Some(trap_fn) -> {
      let desc_props = case full {
        True -> [
          #(Named("value"), value.data_property(val)),
          #(Named("writable"), value.data_property(value.JsBool(True))),
          #(Named("enumerable"), value.data_property(value.JsBool(True))),
          #(Named("configurable"), value.data_property(value.JsBool(True))),
        ]
        False -> [#(Named("value"), value.data_property(val))]
      }
      let #(heap2, desc_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.from_list(desc_props),
            elements: elements.new(),
            prototype: Some(state.builtins.object.prototype),
            symbol_properties: [],
            extensible: True,
          ),
        )
      let state = State(..state, heap: heap2)
      use #(res, state) <- result.map(
        state.call(state, trap_fn, JsObject(h), [
          JsObject(t),
          pk_value(pk),
          JsObject(desc_ref),
        ]),
      )
      #(state, value.is_truthy(res))
    }
  }
}

/// Fast path for set_on_receiver when the caller already read `ref`'s slot.
///
/// In the overwhelmingly common case (`obj.x = v`, `arr[i] = v`) the receiver
/// IS the object whose slot set_value just read, and nothing has mutated the
/// heap since — so re-reading it in set_property is pure waste. When the
/// receiver is a different object (Reflect.set with explicit receiver, or a
/// proto-chain frame where ref walked past the original receiver) we fall back
/// to the slow path that reads the receiver's own slot.
fn set_on_receiver_with_slot(
  state: State,
  receiver: JsValue,
  ref: Ref,
  slot: HeapSlot,
  key: PropertyKey,
  val: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  case receiver {
    JsObject(recv_ref) if recv_ref == ref -> {
      let #(h, ok) = set_property_on_slot(state.heap, ref, slot, key, val)
      Ok(#(State(..state, heap: h), ok))
    }
    _ -> set_on_receiver(state, receiver, key, val)
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
///   - StringObject in-range index key: §10.4.3.2 step 2b returns
///     IsCompatiblePropertyDescriptor, which is false for a value-change Desc.
///     "length" goes through step 3 OrdinaryDefineOwnProperty and is rejected
///     because StringCreate (§10.4.3.4) made it non-writable/non-configurable.
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
  key: PropertyKey,
  val: JsValue,
) -> #(Heap, Bool) {
  case heap.read(h, ref) {
    Some(slot) -> set_property_on_slot(h, ref, slot, key, val)
    None -> #(h, False)
  }
}

/// set_property given an already-read slot — skips the heap.read when the
/// caller (set_on_receiver_with_slot) already holds the slot for `ref`.
/// The heap must not have been mutated since the slot was read.
fn set_property_on_slot(
  h: Heap,
  ref: Ref,
  slot: HeapSlot,
  key: PropertyKey,
  val: JsValue,
) -> #(Heap, Bool) {
  case slot {
    ObjectSlot(kind:, properties:, elements:, extensible:, ..) ->
      case kind {
        // --- §10.4.2.1 Array exotic [[DefineOwnProperty]] ---
        ArrayObject(length:) -> {
          let length_writable = case dict.get(properties, Named("length")) {
            Ok(DataProperty(writable: w, ..)) -> w
            _ -> True
          }
          case key {
            // §10.4.2.1 step 1: If P is "length", return ArraySetLength(A, Desc).
            // §10.4.2.4 step 12: a non-writable length rejects value writes.
            Named("length") ->
              case length_writable {
                False -> #(h, False)
                True -> array_set_length(h, ref, val, slot, length)
              }
            // §10.4.2.1 step 2: If P is an array index (ToUint32 is valid index):
            Index(idx) ->
              case dict.get(properties, key) {
                // Dict override (defineProperty-created attributes): honor
                // its [[Writable]] and update the override in place so the
                // attribute flags survive the write.
                Ok(DataProperty(
                  writable: True,
                  enumerable:,
                  configurable:,
                  value: _,
                )) -> #(
                  heap.write(
                    h,
                    ref,
                    ObjectSlot(
                      ..slot,
                      properties: dict.insert(
                        properties,
                        key,
                        DataProperty(
                          value: val,
                          writable: True,
                          enumerable:,
                          configurable:,
                        ),
                      ),
                    ),
                  ),
                  True,
                )
                Ok(DataProperty(writable: False, ..)) -> #(h, False)
                // Accessor overrides are routed to the setter by [[Set]]
                // before [[DefineOwnProperty]] is reached; reject here.
                Ok(value.AccessorProperty(..)) -> #(h, False)
                Error(Nil) ->
                  // §10.4.2.1 step 2.h: If index >= oldLen and the length is
                  // not writable (or A is not extensible), return false.
                  case idx >= length && { !extensible || !length_writable } {
                    True -> #(h, False)
                    False -> {
                      // §10.4.2.1 steps 2.i-2.k: define element, then set
                      // length to index+1 when index >= oldLen.
                      let new_elements = elements.set(elements, idx, val)
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
              }
            // §10.4.2.1 step 3: Not "length" and not array index —
            // OrdinaryDefineOwnProperty(A, P, Desc).
            Named(_) -> set_string_property(h, ref, key, val, slot)
          }
        }
        // --- §10.4.4.2 Arguments exotic [[DefineOwnProperty]] ---
        // Spec calls OrdinaryDefineOwnProperty then syncs [[ParameterMap]];
        // our element-based storage is an internal optimization.
        value.ArgumentsObject(_) ->
          case key {
            Index(idx) ->
              case dict.get(properties, key) {
                // Dict override (defineProperty-created attributes): honor
                // its [[Writable]] and update the override in place.
                Ok(DataProperty(
                  writable: True,
                  enumerable:,
                  configurable:,
                  value: _,
                )) -> #(
                  heap.write(
                    h,
                    ref,
                    ObjectSlot(
                      ..slot,
                      properties: dict.insert(
                        properties,
                        key,
                        DataProperty(
                          value: val,
                          writable: True,
                          enumerable:,
                          configurable:,
                        ),
                      ),
                    ),
                  ),
                  True,
                )
                Ok(DataProperty(writable: False, ..)) -> #(h, False)
                Ok(value.AccessorProperty(..)) -> #(h, False)
                Error(Nil) ->
                  case !extensible && !elements.has(elements, idx) {
                    True -> #(h, False)
                    False -> #(
                      heap.write(
                        h,
                        ref,
                        ObjectSlot(
                          ..slot,
                          elements: elements.set(elements, idx, val),
                        ),
                      ),
                      True,
                    )
                  }
              }
            Named(_) -> set_string_property(h, ref, key, val, slot)
          }
        // --- §10.4.3.2 String exotic [[DefineOwnProperty]] ---
        value.StringObject(value: s) -> {
          let len = string_length(s)
          // §10.4.3.2 step 2: If P is a CanonicalNumericIndexString for an
          // integer in [0, length), the property is non-configurable/non-writable,
          // so [[DefineOwnProperty]] returns false for any change.
          // "length" falls through to §10.4.3.2 step 3 (OrdinaryDefineOwnProperty),
          // which rejects because §10.4.3.4 StringCreate made it {W:F, C:F}.
          let is_guarded = case key {
            Named("length") -> True
            Index(idx) -> idx < len
            Named(_) -> False
          }
          case is_guarded {
            // §10.4.3.2 step 2b: returns IsCompatiblePropertyDescriptor(ext,
            // Desc, stringDesc) — false for our value-only Desc against a
            // {W:F, C:F} current descriptor. (Compatible no-op redefines would
            // return true per spec; we conservatively reject.)
            True -> #(h, False)
            // §10.4.3.2 step 3: Else, OrdinaryDefineOwnProperty(S, P, Desc).
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
/// Step 3: Let newLen be ToUint32(Desc.[[Value]]).
/// Step 5: If SameValueZero(newLen, ToNumber(Desc.[[Value]])) is false, throw RangeError.
/// TODO(Deviation): we use coerce_length which rejects non-integer/negative/NaN but
/// returns False instead of throwing RangeError. Should throw RangeError
/// per spec step 5 when newLen != ToNumber(Desc.[[Value]]).
///
/// Steps 9-10: Let oldLen be the current "length" property's [[Value]].
/// Step 11: If newLen >= oldLen, set length and return true.
/// Step 12: If the old "length" property is non-writable, return false.
/// TODO(Deviation): we don't track writable on the virtual length property yet.
/// Object.defineProperty(arr, 'length', {writable: false}) should freeze length.
///
/// Step 18: Delete own elements with index >= newLen, in descending order.
/// TODO(Deviation): spec stops at first non-configurable element (step 18.b) and
/// returns false with length set to that index+1. Our elements have no
/// per-index descriptors, so all are implicitly configurable. Needs
/// per-element property descriptors to handle non-configurable indices.
fn array_set_length(
  h: Heap,
  ref: Ref,
  val: JsValue,
  slot: HeapSlot,
  old_length: Int,
) -> #(Heap, Bool) {
  // §10.4.2.4 steps 3-5: Coerce value to valid uint32 length.
  case coerce_length(h, val) {
    // Step 5: Would be RangeError; we return False.
    None -> #(h, False)
    Some(new_length) -> {
      let assert ObjectSlot(properties:, elements:, ..) = slot
      // §10.4.2.4 steps 8-18: If shrinking, delete own indices >= newLen in
      // descending order. Plain elements are implicitly configurable; dict
      // overrides (defineProperty-created) may be non-configurable — the
      // largest such index stops the truncation (step 17.b: length becomes
      // index + 1 and the operation reports failure).
      case new_length < old_length {
        False -> #(
          heap.write(h, ref, ObjectSlot(..slot, kind: ArrayObject(new_length))),
          True,
        )
        True -> {
          let blocked =
            dict.fold(properties, option.None, fn(acc, k, prop) {
              let non_configurable = !value.prop_configurable(prop)
              case k {
                Index(i) if i >= new_length ->
                  case non_configurable {
                    True ->
                      case acc {
                        Some(m) -> Some(int.max(m, i))
                        option.None -> Some(i)
                      }
                    False -> acc
                  }
                _ -> acc
              }
            })
          let final_len = case blocked {
            Some(b) -> b + 1
            option.None -> new_length
          }
          let new_elements = truncate_elements(elements, final_len, old_length)
          let new_properties =
            dict.filter(properties, fn(k, _prop) {
              case k {
                Index(i) -> i < final_len
                Named(_) -> True
              }
            })
          #(
            heap.write(
              h,
              ref,
              ObjectSlot(
                ..slot,
                kind: ArrayObject(final_len),
                properties: new_properties,
                elements: new_elements,
              ),
            ),
            blocked == option.None,
          )
        }
      }
    }
  }
}

/// §10.4.2.4 ArraySetLength steps 3-5: ToUint32 + RangeError validation.
///
/// Spec: step 3 newLen = ToUint32(Desc.[[Value]]); step 4 numberLen =
/// ToNumber(Desc.[[Value]]); step 5 if SameValueZero(newLen, numberLen) is
/// false, throw RangeError.
///
/// Simplified: we accept finite numbers that are non-negative integers.
/// Fractional, negative, NaN, Infinity, and non-numeric values return None
/// (caller treats as failure). This covers both internal Set(O,"length",n,true)
/// calls from Array.prototype mutators and user-level `arr.length = 3.5`.
fn coerce_length(h: Heap, val: JsValue) -> Option(Int) {
  case length_to_number(h, val) {
    Some(Finite(f)) -> {
      let n = value.float_to_int(f)
      case n >= 0 && int.to_float(n) == f {
        True -> Some(n)
        False -> None
      }
    }
    _ -> None
  }
}

/// ToNumber for array-length values (§10.4.2.4 step 4), without running user
/// code: primitives coerce directly; Number/String/Boolean wrapper objects
/// unwrap their primitive data (what ToPrimitive returns when valueOf is
/// intact). Other objects return None.
///
/// TODO(Deviation): spec ToPrimitive may run user valueOf/toString here.
fn length_to_number(h: Heap, val: JsValue) -> Option(value.JsNum) {
  case val {
    JsNumber(n) -> Some(n)
    JsString(s) -> Some(value.string_to_number(s))
    value.JsBool(True) -> Some(Finite(1.0))
    value.JsBool(False) | value.JsNull -> Some(Finite(0.0))
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: value.NumberObject(value: n), ..)) -> Some(n)
        Some(ObjectSlot(kind: value.StringObject(value: s), ..)) ->
          Some(value.string_to_number(s))
        Some(ObjectSlot(kind: value.BooleanObject(value: True), ..)) ->
          Some(Finite(1.0))
        Some(ObjectSlot(kind: value.BooleanObject(value: False), ..)) ->
          Some(Finite(0.0))
        _ -> None
      }
    _ -> None
  }
}

/// §10.4.2.4 step 18: Delete elements at indices >= new_len.
///
/// Instead of iterating the full [new_len, old_len) range (which could be
/// billions for sparse arrays), we filter the underlying storage directly.
fn truncate_elements(
  elements: JsElements,
  new_len: Int,
  _idx: Int,
) -> JsElements {
  elements.truncate(elements, new_len)
}

/// §10.1.6.1 OrdinaryDefineOwnProperty / §10.1.6.3 ValidateAndApplyPropertyDescriptor
/// (value-update subset).
///
/// Set a string-keyed own property in the properties dict. Returns #(Heap, success).
///
/// Step 2.a (ValidateAndApply): If current is undefined and extensible is false, return false.
/// Steps 2.b-2.e: If current is undefined and extensible is true, create the property.
/// Steps 5-6: If current exists, check writable. If writable is true, update [[Value]]
///            (step 6.c). If writable is false, return false (step 5.e). Accessors
///            also return false (would need [[Set]] path, not [[DefineOwnProperty]]).
///
/// TODO(Deviation): spec's ValidateAndApplyPropertyDescriptor does full descriptor
/// merging (attribute changes, data<->accessor conversion). We only handle
/// the [[Value]] update case. Full descriptor support needed for
/// Object.defineProperty.
fn set_string_property(
  h: Heap,
  ref: Ref,
  key: PropertyKey,
  val: JsValue,
  slot: HeapSlot,
) -> #(Heap, Bool) {
  case slot {
    ObjectSlot(properties:, extensible:, ..) ->
      // §10.1.6.1 step 1: Let current be ? O.[[GetOwnProperty]](P).
      case dict.get(properties, key) {
        // §10.1.6.3 step 6.c: current exists and is writable data — update [[Value]].
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
        // §10.1.6.3 step 5.e: current.[[Writable]] is false → reject.
        Ok(DataProperty(writable: False, ..)) -> #(h, False)
        // Accessor property: [[DefineOwnProperty]] with just a value on an
        // accessor would convert it to data, but we don't support that yet.
        Ok(value.AccessorProperty(..)) -> #(h, False)
        // §10.1.6.3 step 2: current is undefined — property doesn't exist.
        Error(Nil) ->
          case extensible {
            // §10.1.6.3 step 2.a: If extensible is false, return false.
            False -> #(h, False)
            // §10.1.6.3 steps 2.b-2.e: extensible is true — create new data
            // property with {[[Value]]: V, [[Writable]]: true,
            // [[Enumerable]]: true, [[Configurable]]: true}.
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
/// §7.3.5 CreateDataProperty ( O, P, V ) on an ordinary target — installs an
/// enumerable/writable/configurable data property without invoking setters.
/// Public entry for trap-aware copy paths (object spread / rest with a proxy
/// source) that must CreateDataProperty on the freshly allocated target.
pub fn create_data_property(
  heap: Heap,
  ref: Ref,
  key: PropertyKey,
  val: JsValue,
) -> Heap {
  define_own_property(heap, ref, key, val)
}

fn define_own_property(
  heap: Heap,
  ref: Ref,
  key: PropertyKey,
  val: JsValue,
) -> Heap {
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(properties:, ..) -> {
      let new_props = dict.insert(properties, key, value.data_property(val))
      ObjectSlot(..slot, properties: new_props)
    }
    _ -> slot
  }
}

/// §7.3.6 CreateMethodProperty ( O, P, V ) — ES2022 numbering; removed in
/// ES2024 with call sites migrated to the pre-existing
/// CreateNonEnumerableDataPropertyOrThrow (same {W:T, E:F, C:T} semantics).
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
  key: PropertyKey,
  val: JsValue,
) -> Heap {
  // §7.3.32 PrivateSet: writing to a private METHOD throws TypeError. Arc
  // stores private methods as marker-keyed properties (value.private_key), so
  // mark them non-writable — set_found_value then reports failure and
  // PutPrivateField turns that into the spec'd TypeError.
  let prop = case value.is_private_name(key) {
    True -> value.data(val) |> value.configurable()
    False -> value.builtin_property(val)
  }
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(properties:, ..) -> {
      let new_props = dict.insert(properties, key, prop)
      ObjectSlot(..slot, properties: new_props)
    }
    _ -> slot
  }
}

/// [[Extensible]] of an object slot (non-stateful read — no proxy trap).
/// Used by the DefinePrivate* ops: proposal nonextensible-applies-to-private
/// makes PrivateFieldAdd / PrivateMethodOrAccessorAdd throw on non-extensible
/// receivers.
pub fn slot_extensible(heap: Heap, ref: Ref) -> Bool {
  case heap.read(heap, ref) {
    Some(ObjectSlot(extensible:, ..)) -> extensible
    _ -> True
  }
}

/// Raw own data-property insert for a class private element (field: writable,
/// method: non-writable so §7.3.31 PrivateSet's method check trips in
/// set_found_value). Bypasses [[DefineOwnProperty]] — private elements are
/// invisible to integrity levels, and the caller has already performed the
/// extensibility + double-initialization checks.
pub fn define_private_data(
  heap: Heap,
  ref: Ref,
  key: PropertyKey,
  val: JsValue,
  writable writable: Bool,
) -> Heap {
  let prop = case writable {
    True -> value.data(val) |> value.writable() |> value.configurable()
    False -> value.data(val) |> value.configurable()
  }
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(properties:, ..) ->
      ObjectSlot(..slot, properties: dict.insert(properties, key, prop))
    _ -> slot
  }
}

/// §10.1.6.3 step 6.c.i: merge a getter/setter into an existing accessor
/// descriptor, or build a fresh one if absent / a data property.
fn merge_accessor(
  existing: Result(Property, Nil),
  func: JsValue,
  kind: opcode.AccessorKind,
  enumerable: Bool,
) -> Property {
  let #(get, set) = case existing {
    Ok(AccessorProperty(get:, set:, ..)) -> #(get, set)
    Ok(DataProperty(..)) | Error(Nil) -> #(None, None)
  }
  case kind {
    opcode.Getter ->
      AccessorProperty(get: Some(func), set:, enumerable:, configurable: True)
    opcode.Setter ->
      AccessorProperty(get:, set: Some(func), enumerable:, configurable: True)
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
/// §10.1.6.3 ValidateAndApplyPropertyDescriptor step 6.c.i: "For each field
/// name fieldName of desc, set the attribute named fieldName of the property
/// ... to the value of the field").
///
/// Used by object literal `{ get x() {}, set x(v) {} }` syntax.
pub fn define_accessor(
  heap: Heap,
  ref: Ref,
  key: PropertyKey,
  func: JsValue,
  kind: opcode.AccessorKind,
  enumerable enumerable: Bool,
) -> Heap {
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(properties:, ..) -> {
      let new_prop =
        merge_accessor(dict.get(properties, key), func, kind, enumerable)
      ObjectSlot(..slot, properties: dict.insert(properties, key, new_prop))
    }
    _ -> slot
  }
}

/// Symbol-keyed variant of define_accessor — used by DefineAccessorComputed
/// when the computed key is a Symbol (e.g. `{ get [Symbol.iterator]() {} }`).
pub fn define_symbol_accessor(
  heap: Heap,
  ref: Ref,
  sym: SymbolId,
  func: JsValue,
  kind: opcode.AccessorKind,
  enumerable enumerable: Bool,
) -> Heap {
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(symbol_properties:, ..) -> {
      let new_prop =
        merge_accessor(
          list.key_find(symbol_properties, sym),
          func,
          kind,
          enumerable,
        )
      ObjectSlot(
        ..slot,
        symbol_properties: list.key_set(symbol_properties, sym, new_prop),
      )
    }
    _ -> slot
  }
}

/// §10.1.5.1 OrdinaryGetOwnProperty ( O, P ) — symbol-keyed variant.
/// Returns the own property descriptor for a symbol key, or None.
pub fn get_own_symbol_property(
  heap: Heap,
  ref: Ref,
  sym: SymbolId,
) -> Option(Property) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(symbol_properties:, ..)) ->
      list.key_find(symbol_properties, sym) |> option.from_result
    _ -> None
  }
}

/// §10.1.7.1 OrdinaryHasProperty ( O, P ) — symbol-keyed variant.
/// Walks the prototype chain looking for a symbol key.
pub fn has_symbol_property(heap: Heap, ref: Ref, sym: SymbolId) -> Bool {
  case heap.read(heap, ref) {
    Some(ObjectSlot(symbol_properties:, prototype:, ..)) ->
      case list.key_find(symbol_properties, sym) {
        Ok(_) -> True
        Error(Nil) ->
          case prototype {
            Some(proto_ref) -> has_symbol_property(heap, proto_ref, sym)
            None -> False
          }
      }
    _ -> False
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
pub fn has_property(heap: Heap, ref: Ref, key: PropertyKey) -> Bool {
  // Private-name keys ("#x") are invisible to ordinary [[HasProperty]] —
  // spec privates live in [[PrivateElements]], not the property table. The
  // brand check for `#x in obj` uses find_property (PrivateIn opcode), not
  // this function.
  use <- bool.guard(value.is_private_name(key), False)
  case heap.read(heap, ref) {
    // §10.4.6.6 Module Namespace [[HasProperty]]: true iff the key is an
    // exported name. Null prototype → no inheritance.
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
      dict.has_key(exports, value.key_to_string(key))
    Some(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) ->
      // Step 1-2: Let hasOwn be O.[[GetOwnProperty]](P). If not undefined, return true.
      case own_property_of_slot(heap, kind, properties, elements, key) {
        Some(_) -> True
        // §10.4.5.2 TypedArray [[HasProperty]]: a canonical numeric index key
        // answers IsValidIntegerIndex directly — own_property_of_slot already
        // said the index is invalid, so the answer is false WITHOUT consulting
        // the prototype chain (TypedArray.prototype["1.5"] is unreachable).
        None ->
          case typed_array_numeric_key(kind, key) {
            True -> False
            False ->
              // Step 3-4: Let parent be O.[[GetPrototypeOf]](). If not null, recurse.
              case prototype {
                Some(proto_ref) -> has_property(heap, proto_ref, key)
                // Step 5: Return false (null prototype).
                None -> False
              }
          }
      }
    _ -> False
  }
}

/// True when an array-like's named-properties dict holds any Index-keyed
/// entry — i.e. Object.defineProperty has converted a dense element into a
/// dict override (accessor, or data property with non-default attributes).
/// Raw-elements fast paths (array spread, Set-from-array) must check this
/// and fall back to per-index [[Get]] when it returns True, otherwise the
/// override (e.g. a getter) is silently skipped and holes read as undefined.
pub fn has_index_overrides(
  properties: dict.Dict(PropertyKey, Property),
) -> Bool {
  dict.fold(properties, False, fn(acc, key, _prop) {
    case key {
      Index(_) -> True
      Named(_) -> acc
    }
  })
}

/// True when `key` is a canonical numeric index string on a TypedArray —
/// such keys are fully resolved by the integer-indexed exotic behaviour
/// (§10.4.5) and must never fall through to the prototype chain.
fn typed_array_numeric_key(kind: state.ExoticKind, key: PropertyKey) -> Bool {
  case kind, key {
    value.TypedArrayObject(..), Index(_) -> True
    value.TypedArrayObject(..), Named(s) -> is_canonical_numeric_string(s)
    _, _ -> False
  }
}

/// Single prototype-chain walk returning the first descriptor found for
/// `key`, or None when the property is absent on the entire chain.
///
/// Same walk order as has_property (own_property_of_slot at each level), but
/// keeps the descriptor instead of discarding it. Callers that must
/// distinguish "absent" from "found" before acting — private field access
/// (§7.3.31 PrivateGet / §7.3.32 PrivateSet throw on absence) — use this once
/// instead of has_property + get_value/set_value, which would repeat the
/// identical chain walk.
pub fn find_property(
  heap: Heap,
  ref: Ref,
  key: PropertyKey,
) -> Option(Property) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) ->
      case own_property_of_slot(heap, kind, properties, elements, key) {
        Some(prop) -> Some(prop)
        None -> option.then(prototype, find_property(heap, _, key))
      }
    _ -> None
  }
}

/// §10.1.9.2 OrdinarySetWithOwnDescriptor steps 2-7 given an already-found
/// ownDesc (e.g. from find_property — avoids set_value's second chain walk).
///
/// Step 2.a: non-writable data → False. Steps 2.b-2.h: writable data →
/// create/update own data property on the receiver. Steps 4-5: accessor
/// without setter → False. Steps 6-7: Call(setter, Receiver, « V ») → True.
pub fn set_found_value(
  state: State,
  receiver: JsValue,
  prop: Property,
  key: PropertyKey,
  val: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  case prop {
    DataProperty(writable: False, ..) -> Ok(#(state, False))
    DataProperty(writable: True, ..) ->
      set_on_receiver(state, receiver, key, val)
    AccessorProperty(set: None, ..) -> Ok(#(state, False))
    AccessorProperty(set: Some(setter), ..) -> {
      use #(_, state) <- result.map(state.call(state, setter, receiver, [val]))
      #(state, True)
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
pub fn delete_symbol_property(
  h: Heap,
  ref: Ref,
  sym: value.SymbolId,
) -> #(Heap, Bool) {
  case heap.read(h, ref) {
    Some(ObjectSlot(symbol_properties:, ..) as slot) ->
      case list.key_pop(symbol_properties, sym) {
        Ok(#(value.DataProperty(configurable: False, ..), _))
        | Ok(#(value.AccessorProperty(configurable: False, ..), _)) -> #(
          h,
          False,
        )
        Ok(#(_, rest)) -> #(
          heap.write(h, ref, ObjectSlot(..slot, symbol_properties: rest)),
          True,
        )
        Error(Nil) -> #(h, True)
      }
    _ -> #(h, True)
  }
}

/// Read a dict property out of an already-matched ObjectSlot (delete path).
fn dict_get_option_for_delete(
  slot: HeapSlot,
  key: PropertyKey,
) -> Option(Property) {
  case slot {
    ObjectSlot(properties:, ..) -> dict_get_option(properties, key)
    _ -> None
  }
}

/// Remove a key from an already-matched ObjectSlot's properties dict.
fn delete_prop_key(
  slot: HeapSlot,
  key: PropertyKey,
) -> dict.Dict(PropertyKey, Property) {
  case slot {
    ObjectSlot(properties:, ..) -> dict.delete(properties, key)
    _ -> dict.new()
  }
}

pub fn delete_property(h: Heap, ref: Ref, key: PropertyKey) -> #(Heap, Bool) {
  case heap.read(h, ref) {
    // §10.4.6.10 Module Namespace [[Delete]]: deleting an exported name fails
    // (non-configurable); a non-export "succeeds" vacuously.
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
      case dict.has_key(exports, value.key_to_string(key)) {
        True -> #(h, False)
        False -> #(h, True)
      }
    Some(ObjectSlot(kind:, elements:, ..) as slot) ->
      case kind {
        // §10.4.5.5 TypedArray [[Delete]]: canonical numeric index keys are
        // deletable iff they are NOT valid indices (nothing to delete);
        // a live element is non-configurable from delete's point of view.
        value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:) ->
          case key {
            Index(idx) ->
              case
                typed_array_element(
                  h,
                  buffer,
                  elem_kind,
                  byte_offset,
                  typed_array_view_length(
                    h,
                    buffer,
                    elem_kind,
                    byte_offset,
                    length,
                  ),
                  idx,
                )
              {
                Some(_) -> #(h, False)
                None -> #(h, True)
              }
            Named(s) ->
              case is_canonical_numeric_string(s) {
                True -> #(h, True)
                False -> delete_string_property(h, ref, key, slot)
              }
          }
        // Array/Arguments exotic: check if key is an array index
        ArrayObject(_) | value.ArgumentsObject(_) ->
          case key {
            Index(idx) ->
              // Dict override (defineProperty-created attributes) wins:
              // §10.1.10.1 step 3 honors its [[Configurable]].
              case dict_get_option_for_delete(slot, key) {
                Some(prop) ->
                  case value.prop_configurable(prop) {
                    True -> #(
                      heap.write(
                        h,
                        ref,
                        ObjectSlot(
                          ..slot,
                          properties: delete_prop_key(slot, key),
                          elements: elements.delete(elements, idx),
                        ),
                      ),
                      True,
                    )
                    False -> #(h, False)
                  }
                None ->
                  // Step 1-2: Check if element exists; if not, return true.
                  case elements.has(elements, idx) {
                    // Step 3: Element exists (implicitly configurable) — remove and return true.
                    True -> #(
                      heap.write(
                        h,
                        ref,
                        ObjectSlot(
                          ..slot,
                          elements: elements.delete(elements, idx),
                        ),
                      ),
                      True,
                    )
                    // Step 2: desc is undefined → return true.
                    False -> #(h, True)
                  }
              }
            // §10.4.2: array "length" is a virtual non-configurable own
            // property — never deletable. (Arguments "length" is an ordinary
            // dict property and falls through.)
            Named("length") ->
              case kind {
                ArrayObject(_) -> #(h, False)
                _ -> delete_string_property(h, ref, key, slot)
              }
            Named(_) -> delete_string_property(h, ref, key, slot)
          }
        // String exotic: "length" and in-range indices are synthesized
        // non-configurable properties (§10.4.3) — never deletable.
        value.StringObject(value: s) ->
          case key {
            Named("length") -> #(h, False)
            Index(i) ->
              case string_char_at(s, i) {
                Some(_) -> #(h, False)
                None -> delete_string_property(h, ref, key, slot)
              }
            Named(_) -> delete_string_property(h, ref, key, slot)
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
  key: PropertyKey,
  slot: HeapSlot,
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
/// Extended to walk the prototype chain for for-in enumeration: §14.7.5.6
/// ForIn/OfHeadEvaluation calls §14.7.5.9 EnumerateObjectProperties, which
/// walks prototypes (the [[Enumerate]] internal method was removed in ES2016).
/// Uses a seen set to skip shadowed keys per §14.7.5.10.2.1
/// %ForInIteratorPrototype%.next() step 5.b.iii: a key already in
/// [[VisitedKeys]] is not re-yielded.
///
/// The spec's EnumerableOwnProperties only handles own properties; we extend
/// it with prototype walking here because for-in needs it, matching
/// EnumerateObjectProperties behavior. Symbol keys are excluded per
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
        // Module namespace: enumerable keys are the sorted export names; the
        // null prototype ends the walk. (TDZ throws are handled by the caller.)
        Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
          list.sort(dict.keys(exports), string.compare)
          |> list.fold(acc, fn(a, name) {
            case set.contains(seen, name) {
              True -> a
              False -> [name, ..a]
            }
          })
          |> list.reverse
        Some(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) -> {
          // Step 1 (partial): Collect element keys first (array index portion
          // of [[OwnPropertyKeys]], which returns indices in ascending order).
          let #(elem_acc, elem_seen) = case kind {
            ArrayObject(length:) | value.ArgumentsObject(length:) ->
              collect_element_keys(elements, length, seen, acc)
            // TypedArray: indices 0..length-1 are own enumerable properties
            // (none when the backing buffer is detached; clamped to the live
            // buffer when a resizable buffer has shrunk below the view).
            value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:) -> {
              let n =
                typed_array_live_length(
                  heap,
                  buffer,
                  elem_kind,
                  byte_offset,
                  typed_array_view_length(
                    heap,
                    buffer,
                    elem_kind,
                    byte_offset,
                    length,
                  ),
                )
              use st, idx <- int.range(from: 0, to: n, with: #(acc, seen))
              let #(a, s) = st
              let k = int.to_string(idx)
              case set.contains(s, k) {
                True -> st
                False -> #([k, ..a], set.insert(s, k))
              }
            }
            // §10.4.3.3 String exotic: one own enumerable index property per
            // code point — keeps for-in consistent with Object.keys on a
            // String wrapper.
            value.StringObject(value: str) -> {
              let n = string_length(str)
              use st, idx <- int.range(from: 0, to: n, with: #(acc, seen))
              let #(a, s) = st
              let k = int.to_string(idx)
              case set.contains(s, k) {
                True -> st
                False -> #([k, ..a], set.insert(s, k))
              }
            }
            _ -> #(acc, seen)
          }
          // Step 3: For each string key, check desc.[[Enumerable]].
          // Non-enumerable keys are added to seen (for shadowing) but not to results.
          let #(final_acc, final_seen) =
            dict.fold(properties, #(elem_acc, elem_seen), fn(state, key, prop) {
              let #(a, s) = state
              // Private names ("#x") never appear in for-in (spec keeps them
              // outside the ordinary property table).
              use <- bool.guard(value.is_private_name(key), state)
              let k = value.key_to_string(key)
              case set.contains(s, k) {
                True -> #(a, s)
                False ->
                  case prop {
                    // Step 3.a.ii: desc.[[Enumerable]] is true → append key.
                    DataProperty(enumerable: True, ..)
                    | AccessorProperty(enumerable: True, ..) -> #(
                      [k, ..a],
                      set.insert(s, k),
                    )
                    // Non-enumerable: mark seen but don't include.
                    _ -> #(a, set.insert(s, k))
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
/// index portion of §10.1.11.1 OrdinaryOwnPropertyKeys step 2: "For each
/// own property key P of O that is an array index, in ascending numeric
/// index order, append P to keys."
///
/// Iterates elements.indices() (O(k)) instead of probing 0..length (O(length))
/// so sparse arrays with huge length but few entries don't degenerate.
fn collect_element_keys(
  elements: JsElements,
  length: Int,
  seen: set.Set(String),
  acc: List(String),
) -> #(List(String), set.Set(String)) {
  use state, idx <- list.fold(elements.indices(elements), #(acc, seen))
  case idx < length {
    False -> state
    True -> {
      let #(a, s) = state
      let key = int.to_string(idx)
      case set.contains(s, key) {
        True -> state
        False -> #([key, ..a], set.insert(s, key))
      }
    }
  }
}

// ============================================================================
// Symbol-keyed property access

/// §10.1.13 GetPrototypeFromConstructor via a real [[Get]] of
/// newTarget.prototype — accessor `prototype` properties must be invoked
/// (test262 prototype-from-newtarget-abrupt.js). Non-object results fall
/// back to the intrinsic prototype.
pub fn proto_from_new_target(
  state: State,
  new_target: JsValue,
  intrinsic_proto: Ref,
  cont: fn(Ref, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case new_target {
    JsObject(nt_ref) -> {
      use proto_val, state <- state.try_op(get_value(
        state,
        nt_ref,
        Named("prototype"),
        new_target,
      ))
      case proto_val {
        JsObject(proto_ref) -> cont(proto_ref, state)
        _ -> cont(intrinsic_proto, state)
      }
    }
    _ -> cont(intrinsic_proto, state)
  }
}

/// §7.3.25 CopyDataProperties ( target, source, excludedItems )
///
/// Step 1: If source is undefined or null, return unused.
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
/// Step 5: Return unused.
///
/// Used by destructuring rest pattern `{a, b, ...rest}`. `excluded_keys`
/// holds string/index PropertyKeys already bound; `excluded_syms` holds
/// SymbolIds. Step 4.c: "If excludedItems does not contain nextKey..." —
/// filter both the element-range copy and the string/symbol property copies.
///
/// NOTE: reads the source slot's raw properties, so Proxy traps do NOT fire
/// here — proxy sources must go through
/// builtins/object.copy_data_properties_stateful, which routes through the
/// ownKeys/getOwnPropertyDescriptor/get traps and delegates back here for
/// ordinary sources.
///
/// TODO(Deviation): symbol-keyed accessor getters are not invoked — the
/// descriptor is copied directly.
pub fn copy_data_properties_excluding(
  state: State,
  target_ref: Ref,
  source: JsValue,
  excluded_keys: set.Set(PropertyKey),
  excluded_syms: set.Set(SymbolId),
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
              copy_element_range(
                state.heap,
                target_ref,
                elements,
                length,
                excluded_keys,
              )
            // TypedArray: indices 0..length-1 are own enumerable data
            // properties (§10.4.5.7) — clamped to the live buffer.
            value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:) ->
              copy_typed_element_range(
                state.heap,
                target_ref,
                buffer,
                elem_kind,
                byte_offset,
                length,
                excluded_keys,
              )
            // String exotic: one own enumerable index property per code
            // point (§10.4.3.5).
            value.StringObject(value: s) ->
              copy_string_element_range(
                state.heap,
                target_ref,
                s,
                excluded_keys,
              )
            _ -> state.heap
          }
          let state = State(..state, heap:)
          // Step 4 (string keys): Filter to enumerable + not-excluded, then
          // Get + CreateDataProperty.
          let keys =
            dict.to_list(properties)
            |> list.filter_map(fn(pair) {
              let #(k, prop) = pair
              // Private names ("#x") are not ordinary own keys — never copied
              // by spread/Object.assign.
              use <- bool.guard(value.is_private_name(k), Error(Nil))
              case prop {
                // Step 4.c.ii: desc.[[Enumerable]] is true.
                DataProperty(enumerable: True, ..)
                | AccessorProperty(enumerable: True, ..) ->
                  // Step 4.c: excludedItems does not contain nextKey.
                  case set.contains(excluded_keys, k) {
                    True -> Error(Nil)
                    False -> Ok(k)
                  }
                _ -> Error(Nil)
              }
            })
          // Step 4.c.ii.1-2: Get(from, key) then CreateDataPropertyOrThrow(target, key, val).
          use state <- copy_keys_to_target(state, src_ref, target_ref, keys)
          // Step 4 (symbol keys): Copy enumerable symbol-keyed data properties.
          let sym_heap =
            list.fold(symbol_properties, state.heap, fn(h, pair) {
              let #(k, prop) = pair
              case prop {
                DataProperty(value: v, enumerable: True, ..) ->
                  case set.contains(excluded_syms, k) {
                    True -> h
                    False ->
                      define_symbol_property(
                        h,
                        target_ref,
                        k,
                        value.data_property(v),
                      )
                  }
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
  keys: List(PropertyKey),
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
/// and then Get for each index, we iterate the elements storage directly
/// via indices() — O(k) not O(length). The result is the same because
/// array elements are always enumerable data properties (step 4.c.ii
/// check always passes for present elements).
/// §7.3.25 CopyDataProperties — TypedArray index-key portion: copy elements
/// 0..live_length-1 from the backing buffer as data properties.
fn copy_typed_element_range(
  heap: Heap,
  target_ref: Ref,
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
  excluded_keys: set.Set(PropertyKey),
) -> Heap {
  let length =
    typed_array_view_length(heap, buffer, elem_kind, byte_offset, length)
  let n = typed_array_live_length(heap, buffer, elem_kind, byte_offset, length)
  use h, idx <- int.range(from: 0, to: n, with: heap)
  case set.contains(excluded_keys, Index(idx)) {
    True -> h
    False ->
      case typed_array_element(h, buffer, elem_kind, byte_offset, length, idx) {
        Some(v) -> define_own_property(h, target_ref, Index(idx), v)
        None -> h
      }
  }
}

/// §7.3.25 CopyDataProperties — String exotic index-key portion: one
/// single-char data property per code point (§10.4.3.5).
fn copy_string_element_range(
  heap: Heap,
  target_ref: Ref,
  s: String,
  excluded_keys: set.Set(PropertyKey),
) -> Heap {
  use h, idx <- int.range(from: 0, to: string_length(s), with: heap)
  case set.contains(excluded_keys, Index(idx)) {
    True -> h
    False ->
      case string_char_at(s, idx) {
        Some(ch) -> define_own_property(h, target_ref, Index(idx), JsString(ch))
        None -> h
      }
  }
}

fn copy_element_range(
  heap: Heap,
  target_ref: Ref,
  elements: JsElements,
  end: Int,
  excluded_keys: set.Set(PropertyKey),
) -> Heap {
  use h, idx <- list.fold(elements.indices(elements), heap)
  // Step 4.c: skip excluded indices (rest-pattern `{0: a, ...r} = arr`).
  case idx < end && !set.contains(excluded_keys, Index(idx)) {
    False -> h
    True ->
      // Step 4.c.ii.2: CreateDataPropertyOrThrow(target, ToString(idx), value).
      define_own_property(
        h,
        target_ref,
        Index(idx),
        elements.get(elements, idx),
      )
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
    // §10.5.8 Proxy [[Get]] — symbol-keyed.
    Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) ->
      proxy_get(state, target, handler, PkSymbol(key), receiver)
    Some(ObjectSlot(symbol_properties:, prototype:, ..)) ->
      // Step 1: Let desc be O.[[GetOwnProperty]](P).
      case list.key_find(symbol_properties, key) {
        // Step 3: IsDataDescriptor → return desc.[[Value]].
        Ok(DataProperty(value: val, ..)) -> Ok(#(val, state))
        // Step 5-7: Accessor with getter → Call(getter, Receiver).
        Ok(AccessorProperty(get: Some(getter), ..)) ->
          state.call(state, getter, receiver, [])
        // Step 6: getter is undefined → return undefined.
        Ok(AccessorProperty(get: None, ..)) -> Ok(#(value.JsUndefined, state))
        // Step 2: desc is undefined → walk prototype chain.
        Error(Nil) ->
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
    // §10.4.6.9 Module Namespace [[Set]]: always returns false (read-only),
    // including for symbol keys — never falls through to the receiver write.
    Some(ObjectSlot(kind: value.ModuleNamespace(..), ..)) -> Ok(#(state, False))
    // §10.5.9 Proxy [[Set]] — symbol-keyed.
    Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) ->
      proxy_set(state, target, handler, PkSymbol(key), val, receiver)
    Some(ObjectSlot(symbol_properties:, prototype:, ..)) ->
      // Step 1: Let ownDesc be O.[[GetOwnProperty]](P).
      case list.key_find(symbol_properties, key) {
        // Step 1 (OrdinarySetWithOwnDescriptor): ownDesc is undefined.
        Error(Nil) ->
          case prototype {
            // Step 1.b: parent is not null → parent.[[Set]](P, V, Receiver).
            Some(proto_ref) ->
              set_symbol_value(state, proto_ref, key, val, receiver)
            // Step 1.c + 2.d: End of chain — create own data property on receiver.
            None -> define_symbol_data_on_receiver(state, receiver, key, val)
          }
        // Step 2.a: ownDesc.[[Writable]] is false → return false.
        Ok(DataProperty(writable: False, ..)) -> Ok(#(state, False))
        // Step 2.d-e: Writable data property → create/update own on receiver.
        Ok(DataProperty(writable: True, ..)) ->
          define_symbol_data_on_receiver(state, receiver, key, val)
        // Step 6-7: Accessor with setter → Call(setter, Receiver, << V >>), return true.
        Ok(AccessorProperty(set: Some(setter), ..)) -> {
          use #(_, state) <- result.map(
            state.call(state, setter, receiver, [val]),
          )
          #(state, True)
        }
        // Step 5: setter is undefined → return false.
        Ok(AccessorProperty(set: None, ..)) -> Ok(#(state, False))
      }
    _ -> Ok(#(state, False))
  }
}

/// OrdinarySetWithOwnDescriptor steps 2.b-2.e: create an own data property
/// on the receiver, or return false when the receiver is not an object.
fn define_symbol_data_on_receiver(
  state: State,
  receiver: JsValue,
  key: SymbolId,
  val: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  case receiver {
    JsObject(recv_ref) -> {
      // Proxy receiver: the GetOwnProperty/DefineOwnProperty pair must go
      // through the proxy's traps.
      use <- proxy_receiver_guard(state, recv_ref, PkSymbol(key), val)
      // §10.1.9.2 step 2.c: merging {[[Value]]: V} into the receiver's
      // existing descriptor only changes [[Value]] — attributes are
      // preserved. A non-writable or accessor existing property rejects.
      // New properties get CreateDataProperty defaults (all true).
      let existing = get_own_symbol_property(state.heap, recv_ref, key)
      case existing {
        Some(DataProperty(writable: False, ..)) -> Ok(#(state, False))
        Some(AccessorProperty(..)) -> Ok(#(state, False))
        Some(DataProperty(writable: True, enumerable:, configurable:, value: _)) -> {
          let h =
            define_symbol_property(
              state.heap,
              recv_ref,
              key,
              DataProperty(
                value: val,
                writable: True,
                enumerable:,
                configurable:,
              ),
            )
          Ok(#(State(..state, heap: h), True))
        }
        None -> {
          let h =
            define_symbol_property(
              state.heap,
              recv_ref,
              key,
              value.data_property(val),
            )
          Ok(#(State(..state, heap: h), True))
        }
      }
    }
    // Step 2.b: Receiver is not an Object → return false.
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
pub fn define_symbol_property(
  heap: Heap,
  ref: Ref,
  key: SymbolId,
  prop: Property,
) -> Heap {
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(symbol_properties:, ..) -> {
      let new_sym_props = list.key_set(symbol_properties, key, prop)
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
    value.JsString(s) -> "'" <> escape_string(s) <> "'"
    value.JsSymbol(id) ->
      value.well_known_symbol_description(id)
      |> option.map(fn(desc) { "Symbol(" <> desc <> ")" })
      |> option.unwrap("Symbol()")
    value.JsBigInt(value.BigInt(n)) -> int.to_string(n) <> "n"
    value.JsUninitialized -> "<uninitialized>"
    value.JsObject(value.Ref(id:) as ref) ->
      case set.contains(visited, id) {
        True -> "[Circular]"
        False -> inspect_object(heap, ref, depth, set.insert(visited, id))
      }
  }
}

fn escape_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("'", "\\'")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
}

fn inspect_object(
  heap: Heap,
  ref: value.Ref,
  depth: Int,
  visited: set.Set(Int),
) -> String {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind:, properties:, elements:, symbol_properties:, ..)) ->
      case kind {
        ArrayObject(length:) ->
          inspect_array(heap, elements, length, depth, visited)
        FunctionObject(..) | NativeFunction(..) -> {
          let name = case dict.get(properties, Named("name")) {
            Ok(DataProperty(value: JsString(n), ..)) -> n
            _ -> ""
          }
          case name {
            "" -> "[Function (anonymous)]"
            n -> "[Function: " <> n <> "]"
          }
        }
        PromiseObject(_) -> "Promise {}"
        value.ProxyObject(callable: True, ..) -> "[Function (Proxy)]"
        value.ProxyObject(..) -> "Proxy {}"
        GeneratorObject(_) -> "Object [Generator] {}"
        value.AsyncGeneratorObject(_) -> "Object [AsyncGenerator] {}"
        value.ArgumentsObject(length:) ->
          "[Arguments] "
          <> inspect_array(heap, elements, length, depth, visited)
        value.StringObject(value: s) -> "[String: '" <> escape_string(s) <> "']"
        value.NumberObject(value: n) ->
          "[Number: " <> inspect_inner(JsNumber(n), heap, depth, visited) <> "]"
        value.BooleanObject(value: True) -> "[Boolean: true]"
        value.BooleanObject(value: False) -> "[Boolean: false]"
        value.SymbolObject(value: sym) ->
          "[Symbol: "
          <> inspect_inner(value.JsSymbol(sym), heap, depth, visited)
          <> "]"
        value.PidObject(pid:) -> "Pid" <> process_objects.ffi_pid_to_string(pid)
        value.SubjectObject(pid:, ..) ->
          "Subject" <> process_objects.ffi_pid_to_string(pid)
        value.SelectorObject(..) -> "Selector {}"
        value.TimerObject(..) -> "Timer {}"
        value.MapObject(entries:, ..) ->
          "Map(" <> int.to_string(dict.size(entries)) <> ")"
        value.SetObject(data:, ..) ->
          "Set(" <> int.to_string(dict.size(data)) <> ")"
        value.WeakMapObject(_) -> "WeakMap {}"
        value.WeakSetObject(_) -> "WeakSet {}"
        value.FinalizationRegistryObject(..) -> "FinalizationRegistry {}"
        value.ArrayIteratorObject(..) -> "Object [Array Iterator] {}"
        value.StringIteratorObject(..) -> "Object [String Iterator] {}"
        value.SetIteratorObject(..) -> "Object [Set Iterator] {}"
        value.MapIteratorObject(..) -> "Object [Map Iterator] {}"
        value.AsyncFromSyncIteratorObject(..) ->
          "Object [Async-from-Sync Iterator] {}"
        value.DateObject(time_value:) ->
          case time_value {
            value.Finite(f) -> "Date(" <> value.js_format_number(f) <> ")"
            _ -> "Invalid Date"
          }
        value.RegExpObject(pattern:, flags:) -> {
          let source = case pattern {
            "" -> "(?:)"
            p -> p
          }
          "/" <> source <> "/" <> flags
        }
        value.DataViewObject(..) -> "DataView {}"
        value.ArrayBufferObject(data:, shared: False, ..) ->
          "ArrayBuffer { byteLength: "
          <> int.to_string(bit_array.byte_size(data))
          <> " }"
        value.ArrayBufferObject(data:, shared: True, ..) ->
          "SharedArrayBuffer { byteLength: "
          <> int.to_string(bit_array.byte_size(data))
          <> " }"
        value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:) ->
          value.typed_array_name(elem_kind)
          <> "("
          <> int.to_string(typed_array_view_length(
            heap,
            buffer,
            elem_kind,
            byte_offset,
            length,
          ))
          <> ")"
        value.IteratorHelperObject(..) -> "[Iterator Helper]"
        value.IteratorZipObject(..) -> "[Iterator Helper]"
        value.IteratorConcatObject(..) -> "[Iterator Helper]"
        value.WrapForValidIteratorObject(..) -> "[Iterator]"
        value.TemporalDateSlot(..) -> "Temporal.PlainDate {}"
        value.TemporalTimeSlot(..) -> "Temporal.PlainTime {}"
        value.TemporalDateTimeSlot(..) -> "Temporal.PlainDateTime {}"
        value.TemporalYearMonthSlot(..) -> "Temporal.PlainYearMonth {}"
        value.TemporalMonthDaySlot(..) -> "Temporal.PlainMonthDay {}"
        value.TemporalDurationSlot(..) -> "Temporal.Duration {}"
        value.TemporalInstantSlot(..) -> "Temporal.Instant {}"
        value.TemporalZonedDateTimeSlot(..) -> "Temporal.ZonedDateTime {}"
        value.ModuleNamespace(exports:) ->
          "[Module: { "
          <> string.join(list.sort(dict.keys(exports), string.compare), ", ")
          <> " }]"
        value.IteratorRecordObject(..) -> "[Iterator]"
        value.IntlObject(service:, ..) ->
          case service {
            value.IntlLocale -> "[Intl.Locale]"
            value.IntlCollator -> "[Intl.Collator]"
            value.IntlNumberFormat -> "[Intl.NumberFormat]"
            value.IntlDateTimeFormat -> "[Intl.DateTimeFormat]"
            value.IntlPluralRules -> "[Intl.PluralRules]"
            value.IntlListFormat -> "[Intl.ListFormat]"
            value.IntlRelativeTimeFormat -> "[Intl.RelativeTimeFormat]"
            value.IntlSegmenter -> "[Intl.Segmenter]"
            value.IntlDisplayNames -> "[Intl.DisplayNames]"
            value.IntlDurationFormat -> "[Intl.DurationFormat]"
            value.IntlSegments -> "[Intl Segments]"
            value.IntlSegmentIterator -> "[Intl Segment Iterator]"
          }
        value.DisposableStackObject(..) -> "DisposableStack {}"
        value.ShadowRealmObject(..) -> "ShadowRealm {}"
        OrdinaryObject | value.ErrorObject(_) -> {
          // Error instances render as "Name: message" (or the full stack, once
          // we capture one); everything else as a plain object, prefixed with
          // its Symbol.toStringTag when one is set.
          case error_display(heap, ref) {
            Some(s) -> s
            None -> {
              let body = inspect_plain_object(heap, properties, depth, visited)
              case
                list.key_find(symbol_properties, value.symbol_to_string_tag)
              {
                Ok(DataProperty(value: JsString(t), ..)) ->
                  "Object [" <> t <> "] " <> body
                _ -> body
              }
            }
          }
        }
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
  case depth > 2 {
    True -> "[Array]"
    False -> {
      let items =
        inspect_array_loop(heap, elements, 0, length, depth, visited, [])
      "[ " <> string.join(items, ", ") <> " ]"
    }
  }
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
      let item =
        elements.get_option(elements, idx)
        |> option.map(inspect_inner(_, heap, depth + 1, visited))
        |> option.unwrap("<empty>")
      inspect_array_loop(heap, elements, idx + 1, length, depth, visited, [
        item,
        ..acc
      ])
    }
  }
}

fn inspect_plain_object(
  heap: Heap,
  properties: dict.Dict(PropertyKey, value.Property),
  depth: Int,
  visited: set.Set(Int),
) -> String {
  case depth > 2 {
    True -> "[Object]"
    False -> {
      let entries =
        dict.to_list(properties)
        |> list.filter_map(fn(pair) {
          let #(key, prop) = pair
          case prop {
            DataProperty(enumerable: True, value: val, ..) ->
              Ok(
                value.key_to_string(key)
                <> ": "
                <> inspect_inner(val, heap, depth + 1, visited),
              )
            _ -> Error(Nil)
          }
        })
      case entries {
        [] -> "{}"
        _ -> "{ " <> string.join(entries, ", ") <> " }"
      }
    }
  }
}

/// Format a value for an uncaught-exception / unhandled-rejection report.
/// Error instances become "Name: message" (or their `stack`, once we capture
/// one); thrown strings are shown raw (browser-style "Uncaught boom"); anything
/// else falls back to `inspect`. Read-only — never invokes JS.
pub fn format_error(val: value.JsValue, heap: Heap) -> String {
  case val {
    JsString(s) -> s
    JsObject(ref) ->
      error_display(heap, ref) |> option.unwrap(inspect(val, heap))
    _ -> inspect(val, heap)
  }
}

/// If `ref` is an Error instance, render it for display, else None.
///
/// Errors with a captured trace render as that trace (it already embeds the
/// "Name: message" header, V8-style); otherwise we synthesize the header from
/// `name` and `message` per `Error.prototype.toString` (§20.5.3.4). The trace
/// lives in the [[ErrorData]] slot (ErrorObject kind); an own `stack` data
/// property (Error.captureStackTrace targets) is honored as a fallback.
fn error_display(heap: Heap, ref: value.Ref) -> Option(String) {
  use <- bool.guard(!is_error(heap, ref), None)
  let slot_stack = case heap.read(heap, ref) {
    Some(ObjectSlot(kind: value.ErrorObject(stack:), ..)) if stack != "" ->
      Some(stack)
    _ -> None
  }
  case option.or(slot_stack, error_property(heap, ref, "stack")) {
    Some(stack) -> Some(stack)
    None -> {
      let name = error_property(heap, ref, "name") |> option.unwrap("Error")
      let message = error_property(heap, ref, "message") |> option.unwrap("")
      Some(case name, message {
        "", _ -> message
        _, "" -> name
        _, _ -> name <> ": " <> message
      })
    }
  }
}

/// Read-only test for whether `ref` is an Error instance: the [[ErrorData]]
/// internal slot (ErrorObject kind), or — for error-shaped objects built
/// without the slot — some object in its *prototype* chain owning a `message`
/// property (the marker carried by `Error.prototype`). Checking the prototype
/// chain (not the instance's own properties) correctly excludes plain objects
/// like `{ message: "x" }`.
fn is_error(heap: Heap, ref: value.Ref) -> Bool {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind: value.ErrorObject(_), ..)) -> True
    Some(ObjectSlot(prototype: Some(proto_ref), ..)) ->
      prototype_owns_message(heap, proto_ref, 100)
    _ -> False
  }
}

fn prototype_owns_message(heap: Heap, ref: value.Ref, fuel: Int) -> Bool {
  use <- bool.guard(fuel <= 0, False)
  case heap.read(heap, ref) {
    Some(ObjectSlot(properties:, prototype:, ..)) ->
      case dict.has_key(properties, Named("message")) {
        True -> True
        False ->
          case prototype {
            Some(parent) -> prototype_owns_message(heap, parent, fuel - 1)
            None -> False
          }
      }
    _ -> False
  }
}

/// Read a string-valued data property by walking the prototype chain (own
/// shadows inherited, like [[Get]]). Returns None when absent or non-string.
fn error_property(heap: Heap, ref: value.Ref, key: String) -> Option(String) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(properties:, prototype:, ..)) ->
      case dict.get(properties, Named(key)) {
        Ok(DataProperty(value: JsString(s), ..)) -> Some(s)
        Ok(_) -> None
        Error(Nil) ->
          case prototype {
            Some(parent) -> error_property(heap, parent, key)
            None -> None
          }
      }
    _ -> None
  }
}

/// **IsConstructor(argument)** — ES2024 §7.2.4. Returns True iff `value` is an
/// object with a [[Construct]] internal method. Single source of truth for
/// constructibility, shared by the `new` / construct path (exec/call) and
/// `Reflect.construct` (builtins/reflect).
pub fn is_constructor(heap: Heap, value: JsValue) -> Bool {
  case value {
    JsObject(ref) ->
      case heap.read(heap, ref) {
        // ECMAScript function: a constructor unless it's an arrow, generator,
        // ECMAScript function: read the [[Construct]] capability stored on its
        // template, set at compile time from the function's syntactic kind
        // (normal functions + class constructors carry it; arrows, generators,
        // async functions, and methods/getters/setters do not).
        Some(ObjectSlot(kind: FunctionObject(func_template:, ..), ..)) ->
          func_template.is_constructor
        // Built-in function: read the [[Construct]] capability stored on the
        // slot — set True for constructor intrinsics at allocation, and copied
        // from its target by `bind` (§10.4.1.3 step 6). A native's dispatch tag
        // alone doesn't encode this, so unlike user functions it is stored.
        Some(ObjectSlot(kind: NativeFunction(constructible:, ..), ..)) ->
          constructible
        // Proxy: has [[Construct]] iff its target did at creation time
        // (§10.5.15 ProxyCreate step 7) — still true after revocation.
        Some(ObjectSlot(kind: value.ProxyObject(constructable:, ..), ..)) ->
          constructable
        _ -> False
      }
    _ -> False
  }
}

// ============================================================================
// Proxy exotic object internal methods — ES2024 §10.5
// ============================================================================

/// A property key as seen by proxy traps: string-ish key or symbol.
/// Unifies the codebase's PropertyKey / SymbolId split so every proxy
/// internal method exists once instead of twice.
pub type ProxyKey {
  PkString(PropertyKey)
  PkSymbol(SymbolId)
}

/// The trap-argument form of a proxy key (a String or Symbol value).
fn pk_value(pk: ProxyKey) -> JsValue {
  case pk {
    PkString(key) -> JsString(value.key_to_string(key))
    PkSymbol(sym) -> value.JsSymbol(sym)
  }
}

/// Human-readable key for invariant-violation error messages.
fn pk_label(pk: ProxyKey) -> String {
  case pk {
    PkString(key) -> "'" <> value.key_to_string(key) <> "'"
    PkSymbol(_) -> "[symbol]"
  }
}

/// Allocate a TypeError in the ops-level #(thrown, state) error shape.
fn proxy_error(state: State, msg: String) -> #(JsValue, State) {
  let #(state, res) = state.type_error(state, msg)
  case res {
    Error(err) -> #(err, state)
    // Unreachable — state.type_error always returns Error.
    Ok(val) -> #(val, state)
  }
}

/// §7.2.3 IsCallable, including proxies (callable iff target was callable at
/// creation — §10.5.15 step 7.a).
pub fn value_is_callable(h: Heap, val: JsValue) -> Bool {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: FunctionObject(..), ..)) -> True
        Some(ObjectSlot(kind: NativeFunction(..), ..)) -> True
        Some(ObjectSlot(kind: value.ProxyObject(callable:, ..), ..)) -> callable
        _ -> False
      }
    _ -> False
  }
}

/// §7.2.2 IsArray ( argument ). Pierces proxies to their target (step 3);
/// Error(Nil) signals a revoked proxy was encountered — the caller must
/// throw a TypeError.
pub fn is_array(h: Heap, val: JsValue) -> Result(Bool, Nil) {
  case val {
    // Steps 2-4 for objects.
    JsObject(ref) -> is_array_ref(h, ref)
    // Step 1: If argument is not an Object, return false.
    _ -> Ok(False)
  }
}

/// IsArray (§7.2.2) on an object ref — step 2 (Array exotic object) and
/// step 3 (Proxy: validate non-revoked, then recurse on [[ProxyTarget]]).
pub fn is_array_ref(h: Heap, ref: Ref) -> Result(Bool, Nil) {
  case heap.read(h, ref) {
    // Step 2: If argument is an Array exotic object, return true.
    Some(ObjectSlot(kind: ArrayObject(_), ..)) -> Ok(True)
    // Step 3: Proxy exotic object.
    Some(ObjectSlot(kind: value.ProxyObject(target:, ..), ..)) ->
      case target {
        // Step 3.b-c: Return ? IsArray(proxy.[[ProxyTarget]]).
        Some(t) -> is_array_ref(h, t)
        // Step 3.a: If proxy.[[ProxyHandler]] is null, throw TypeError.
        None -> Error(Nil)
      }
    // Step 4: Return false.
    _ -> Ok(False)
  }
}

/// Read a ref's ProxyObject parts, or None when it isn't a proxy.
pub fn as_proxy(h: Heap, ref: Ref) -> Option(#(Option(Ref), Option(Ref))) {
  case heap.read(h, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) ->
      Some(#(target, handler))
    _ -> None
  }
}

/// §10.5.14 ValidateNonRevokedProxy + §7.3.10 GetMethod(handler, name).
/// Returns the live target/handler refs and the trap function (None when the
/// handler doesn't define it). TypeError on revoked proxy or non-callable trap.
pub fn proxy_trap(
  state: State,
  target: Option(Ref),
  handler: Option(Ref),
  name: String,
) -> Result(#(Ref, Ref, Option(JsValue), State), #(JsValue, State)) {
  case target, handler {
    Some(t), Some(h) -> {
      // GetMethod step 1: Let func be ? GetV(V, P).
      use #(trap, state) <- result.try(get_value(
        state,
        h,
        Named(name),
        JsObject(h),
      ))
      case trap {
        // GetMethod step 2: undefined or null → undefined.
        value.JsUndefined | value.JsNull -> Ok(#(t, h, None, state))
        _ ->
          // GetMethod step 3: If IsCallable(func) is false, throw TypeError.
          case value_is_callable(state.heap, trap) {
            True -> Ok(#(t, h, Some(trap), state))
            False ->
              Error(proxy_error(
                state,
                "'" <> name <> "' trap of proxy handler is not a function",
              ))
          }
      }
    }
    _, _ ->
      Error(proxy_error(
        state,
        "Cannot perform '" <> name <> "' on a proxy that has been revoked",
      ))
  }
}

/// target.[[GetOwnProperty]](P) for either key kind. Non-trapping read used
/// by the invariant checks (a nested-proxy target reports no own properties
/// here, which only makes the invariant checks more permissive).
fn target_own_property(h: Heap, t: Ref, pk: ProxyKey) -> Option(Property) {
  case pk {
    PkString(key) -> get_own_property(h, t, key)
    PkSymbol(sym) -> get_own_symbol_property(h, t, sym)
  }
}

/// §10.5.8 Proxy [[Get]] ( P, Receiver ).
pub fn proxy_get(
  state: State,
  target: Option(Ref),
  handler: Option(Ref),
  pk: ProxyKey,
  receiver: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  // Steps 1-5: revocation check + GetMethod(handler, "get").
  use #(t, h, trap, state) <- result.try(proxy_trap(
    state,
    target,
    handler,
    "get",
  ))
  case trap {
    // Step 6: trap undefined → target.[[Get]](P, Receiver).
    None ->
      case pk {
        PkString(key) -> get_value(state, t, key, receiver)
        PkSymbol(sym) -> get_symbol_value(state, t, sym, receiver)
      }
    Some(trap_fn) -> {
      // Step 7: Call(trap, handler, « target, P, Receiver »).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [
          JsObject(t),
          pk_value(pk),
          receiver,
        ]),
      )
      // Steps 8-10: invariants against the target's own descriptor.
      case target_own_property(state.heap, t, pk) {
        Some(DataProperty(value: tv, writable: False, configurable: False, ..)) ->
          case value.same_value(res, tv) {
            True -> Ok(#(res, state))
            False ->
              Error(proxy_error(
                state,
                "'get' on proxy: property "
                  <> pk_label(pk)
                  <> " is a read-only and non-configurable data property on the proxy target but the proxy did not return its actual value",
              ))
          }
        Some(AccessorProperty(get: None, configurable: False, ..)) ->
          case res {
            value.JsUndefined -> Ok(#(res, state))
            _ ->
              Error(proxy_error(
                state,
                "'get' on proxy: property "
                  <> pk_label(pk)
                  <> " is a non-configurable accessor property on the proxy target without a getter, but the trap did not return undefined",
              ))
          }
        _ -> Ok(#(res, state))
      }
    }
  }
}

/// §10.5.9 Proxy [[Set]] ( P, V, Receiver ).
pub fn proxy_set(
  state: State,
  target: Option(Ref),
  handler: Option(Ref),
  pk: ProxyKey,
  val: JsValue,
  receiver: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  use #(t, h, trap, state) <- result.try(proxy_trap(
    state,
    target,
    handler,
    "set",
  ))
  case trap {
    // Step 6: trap undefined → target.[[Set]](P, V, Receiver).
    None ->
      case pk {
        PkString(key) -> set_value(state, t, key, val, receiver)
        PkSymbol(sym) -> set_symbol_value(state, t, sym, val, receiver)
      }
    Some(trap_fn) -> {
      // Step 7: ToBoolean(? Call(trap, handler, « target, P, V, Receiver »)).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [
          JsObject(t),
          pk_value(pk),
          val,
          receiver,
        ]),
      )
      case value.is_truthy(res) {
        // Step 8: trap returned false → [[Set]] fails.
        False -> Ok(#(state, False))
        True ->
          // Steps 9-11: invariants against the target's own descriptor.
          case target_own_property(state.heap, t, pk) {
            Some(DataProperty(
              value: tv,
              writable: False,
              configurable: False,
              ..,
            )) ->
              case value.same_value(val, tv) {
                True -> Ok(#(state, True))
                False ->
                  Error(proxy_error(
                    state,
                    "'set' on proxy: trap returned truish for property "
                      <> pk_label(pk)
                      <> " which exists in the proxy target as a non-configurable and non-writable data property with a different value",
                  ))
              }
            Some(AccessorProperty(set: None, configurable: False, ..)) ->
              Error(proxy_error(
                state,
                "'set' on proxy: trap returned truish for property "
                  <> pk_label(pk)
                  <> " which exists in the proxy target as a non-configurable accessor property without a setter",
              ))
            _ -> Ok(#(state, True))
          }
      }
    }
  }
}

/// §10.1.7.1 OrdinaryHasProperty / §10.5.7 Proxy [[HasProperty]] — the
/// trap-aware [[HasProperty]] used by the `in` operator and Reflect.has.
/// Recurses through prototype chains so a proxy anywhere on the chain traps.
pub fn has_property_stateful(
  state: State,
  ref: Ref,
  pk: ProxyKey,
) -> Result(#(Bool, State), #(JsValue, State)) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) ->
      proxy_has(state, target, handler, pk)
    Some(ObjectSlot(
      kind: value.ModuleNamespace(exports:),
      symbol_properties:,
      ..,
    )) ->
      case pk {
        PkString(key) ->
          Ok(#(dict.has_key(exports, value.key_to_string(key)), state))
        PkSymbol(sym) ->
          Ok(#(result.is_ok(list.key_find(symbol_properties, sym)), state))
      }
    Some(ObjectSlot(
      kind:,
      properties:,
      elements:,
      symbol_properties:,
      prototype:,
      ..,
    )) -> {
      // Private-element keys are invisible to ordinary [[HasProperty]] (the
      // brand check uses find_property via the PrivateIn opcode, not this
      // function). Checked here on the ordinary path only — for proxies the
      // key is an ordinary string and must reach the "has" trap.
      let is_private = case pk {
        PkString(key) -> value.is_private_name(key)
        PkSymbol(_) -> False
      }
      use <- bool.guard(is_private, Ok(#(False, state)))
      let own = case pk {
        PkString(key) ->
          option.is_some(own_property_of_slot(
            state.heap,
            kind,
            properties,
            elements,
            key,
          ))
        PkSymbol(sym) -> result.is_ok(list.key_find(symbol_properties, sym))
      }
      case own {
        True -> Ok(#(True, state))
        False -> {
          // §10.4.5.2 TypedArray [[HasProperty]]: invalid canonical numeric
          // index → false, never the prototype chain (mirrors has_property).
          let ta_numeric = case pk {
            PkString(key) -> typed_array_numeric_key(kind, key)
            PkSymbol(_) -> False
          }
          case ta_numeric {
            True -> Ok(#(False, state))
            False ->
              case prototype {
                Some(proto_ref) -> has_property_stateful(state, proto_ref, pk)
                None -> Ok(#(False, state))
              }
          }
        }
      }
    }
    _ -> Ok(#(False, state))
  }
}

/// §10.5.7 Proxy [[HasProperty]] ( P ).
pub fn proxy_has(
  state: State,
  target: Option(Ref),
  handler: Option(Ref),
  pk: ProxyKey,
) -> Result(#(Bool, State), #(JsValue, State)) {
  use #(t, h, trap, state) <- result.try(proxy_trap(
    state,
    target,
    handler,
    "has",
  ))
  case trap {
    // Step 6: trap undefined → target.[[HasProperty]](P).
    None -> has_property_stateful(state, t, pk)
    Some(trap_fn) -> {
      // Step 7: ToBoolean(? Call(trap, handler, « target, P »)).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [JsObject(t), pk_value(pk)]),
      )
      case value.is_truthy(res) {
        True -> Ok(#(True, state))
        False ->
          // Step 8: invariants when the trap reports the key as absent.
          case target_own_property(state.heap, t, pk) {
            None -> Ok(#(False, state))
            Some(prop) ->
              case value.prop_configurable(prop) {
                False ->
                  Error(proxy_error(
                    state,
                    "'has' on proxy: trap returned falsish for property "
                      <> pk_label(pk)
                      <> " which exists in the proxy target as non-configurable",
                  ))
                True -> {
                  // Step 9.b.ii: ? IsExtensible(target) — traps when the
                  // target is itself a proxy.
                  use #(ext, state) <- result.try(is_extensible_stateful(
                    state,
                    t,
                  ))
                  case ext {
                    False ->
                      Error(proxy_error(
                        state,
                        "'has' on proxy: trap returned falsish for property "
                          <> pk_label(pk)
                          <> " but the proxy target is not extensible",
                      ))
                    True -> Ok(#(False, state))
                  }
                }
              }
          }
      }
    }
  }
}

/// Trap-aware [[Delete]] used by the `delete` operator and
/// Reflect.deleteProperty. Falls through to the pure delete for non-proxies.
pub fn delete_property_stateful(
  state: State,
  ref: Ref,
  pk: ProxyKey,
) -> Result(#(State, Bool), #(JsValue, State)) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) ->
      proxy_delete(state, target, handler, pk)
    _ ->
      case pk {
        PkString(key) -> {
          let #(h, ok) = delete_property(state.heap, ref, key)
          Ok(#(State(..state, heap: h), ok))
        }
        PkSymbol(sym) -> {
          let #(h, ok) = delete_symbol_property(state.heap, ref, sym)
          Ok(#(State(..state, heap: h), ok))
        }
      }
  }
}

/// §10.5.10 Proxy [[Delete]] ( P ).
pub fn proxy_delete(
  state: State,
  target: Option(Ref),
  handler: Option(Ref),
  pk: ProxyKey,
) -> Result(#(State, Bool), #(JsValue, State)) {
  use #(t, h, trap, state) <- result.try(proxy_trap(
    state,
    target,
    handler,
    "deleteProperty",
  ))
  case trap {
    // Step 6: trap undefined → target.[[Delete]](P).
    None -> delete_property_stateful(state, t, pk)
    Some(trap_fn) -> {
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [JsObject(t), pk_value(pk)]),
      )
      case value.is_truthy(res) {
        False -> Ok(#(state, False))
        True ->
          // Steps 9-13: invariants.
          case target_own_property(state.heap, t, pk) {
            None -> Ok(#(state, True))
            Some(prop) ->
              case value.prop_configurable(prop) {
                False ->
                  Error(proxy_error(
                    state,
                    "'deleteProperty' on proxy: trap returned truish for property "
                      <> pk_label(pk)
                      <> " which is non-configurable in the proxy target",
                  ))
                True -> {
                  // Step 12: ? IsExtensible(target).
                  use #(ext, state) <- result.try(is_extensible_stateful(
                    state,
                    t,
                  ))
                  case ext {
                    False ->
                      Error(proxy_error(
                        state,
                        "'deleteProperty' on proxy: trap returned truish but the proxy target is not extensible",
                      ))
                    True -> Ok(#(state, True))
                  }
                }
              }
          }
      }
    }
  }
}

/// Trap-aware [[GetPrototypeOf]] — returns JsObject(proto) or JsNull.
pub fn get_prototype_of_stateful(
  state: State,
  ref: Ref,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) ->
      proxy_get_prototype_of(state, target, handler)
    Some(ObjectSlot(prototype: Some(p), ..)) -> Ok(#(JsObject(p), state))
    _ -> Ok(#(value.JsNull, state))
  }
}

/// §10.5.1 Proxy [[GetPrototypeOf]] ( ).
fn proxy_get_prototype_of(
  state: State,
  target: Option(Ref),
  handler: Option(Ref),
) -> Result(#(JsValue, State), #(JsValue, State)) {
  use #(t, h, trap, state) <- result.try(proxy_trap(
    state,
    target,
    handler,
    "getPrototypeOf",
  ))
  case trap {
    None -> get_prototype_of_stateful(state, t)
    Some(trap_fn) -> {
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [JsObject(t)]),
      )
      case res {
        JsObject(_) | value.JsNull -> {
          // Step 7: ? IsExtensible(target) — traps for proxy targets.
          use #(ext, state) <- result.try(is_extensible_stateful(state, t))
          case ext {
            // Step 8: extensible target → no invariant, return trap result.
            True -> Ok(#(res, state))
            False -> {
              // Steps 9-10: non-extensible target → must match actual proto.
              use #(target_proto, state) <- result.try(
                get_prototype_of_stateful(state, t),
              )
              case value.same_value(res, target_proto) {
                True -> Ok(#(res, state))
                False ->
                  Error(proxy_error(
                    state,
                    "'getPrototypeOf' on proxy: proxy target is non-extensible but the trap did not return its actual prototype",
                  ))
              }
            }
          }
        }
        _ ->
          Error(proxy_error(
            state,
            "'getPrototypeOf' on proxy: trap returned neither object nor null",
          ))
      }
    }
  }
}

/// §10.5.2 Proxy [[SetPrototypeOf]] ( V ) — trap path only; the ordinary
/// path (cycle detection etc.) lives in builtins/object and calls back in
/// for proxy refs. `proto_val` is JsObject(p) or JsNull.
pub fn proxy_set_prototype_of(
  state: State,
  target: Option(Ref),
  handler: Option(Ref),
  proto_val: JsValue,
) -> Result(#(State, Option(Ref), Bool), #(JsValue, State)) {
  use #(t, h, trap, state) <- result.try(proxy_trap(
    state,
    target,
    handler,
    "setPrototypeOf",
  ))
  case trap {
    // No trap — caller performs target.[[SetPrototypeOf]](V); signal via the
    // returned target ref.
    None -> Ok(#(state, Some(t), True))
    Some(trap_fn) -> {
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [JsObject(t), proto_val]),
      )
      case value.is_truthy(res) {
        False -> Ok(#(state, None, False))
        True -> {
          // Step 9: ? IsExtensible(target) — traps for proxy targets.
          use #(ext, state) <- result.try(is_extensible_stateful(state, t))
          case ext {
            True -> Ok(#(state, None, True))
            False -> {
              // Steps 10-11: non-extensible target → V must be actual proto.
              use #(target_proto, state) <- result.try(
                get_prototype_of_stateful(state, t),
              )
              case value.same_value(proto_val, target_proto) {
                True -> Ok(#(state, None, True))
                False ->
                  Error(proxy_error(
                    state,
                    "'setPrototypeOf' on proxy: trap returned truish for setting a new prototype on the non-extensible proxy target",
                  ))
              }
            }
          }
        }
      }
    }
  }
}

/// Trap-aware §7.2.5 IsExtensible / §10.5.3 Proxy [[IsExtensible]].
pub fn is_extensible_stateful(
  state: State,
  ref: Ref,
) -> Result(#(Bool, State), #(JsValue, State)) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) -> {
      use #(t, h, trap, state) <- result.try(proxy_trap(
        state,
        target,
        handler,
        "isExtensible",
      ))
      case trap {
        None -> is_extensible_stateful(state, t)
        Some(trap_fn) -> {
          use #(res, state) <- result.try(
            state.call(state, trap_fn, JsObject(h), [JsObject(t)]),
          )
          let b = value.is_truthy(res)
          // Steps 8-9: result must equal IsExtensible(target).
          use #(target_ext, state) <- result.try(is_extensible_stateful(
            state,
            t,
          ))
          case b == target_ext {
            True -> Ok(#(b, state))
            False ->
              Error(proxy_error(
                state,
                "'isExtensible' on proxy: trap result does not reflect extensibility of proxy target (which is '"
                  <> bool.to_string(target_ext)
                  <> "')",
              ))
          }
        }
      }
    }
    Some(ObjectSlot(extensible:, ..)) -> Ok(#(extensible, state))
    _ -> Ok(#(False, state))
  }
}

/// Trap-aware [[PreventExtensions]] / §10.5.4 Proxy [[PreventExtensions]].
pub fn prevent_extensions_stateful(
  state: State,
  ref: Ref,
) -> Result(#(State, Bool), #(JsValue, State)) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(target:, handler:, ..), ..)) -> {
      use #(t, h, trap, state) <- result.try(proxy_trap(
        state,
        target,
        handler,
        "preventExtensions",
      ))
      case trap {
        None -> prevent_extensions_stateful(state, t)
        Some(trap_fn) -> {
          use #(res, state) <- result.try(
            state.call(state, trap_fn, JsObject(h), [JsObject(t)]),
          )
          case value.is_truthy(res) {
            False -> Ok(#(state, False))
            True -> {
              // Step 8: trap returned true → target must be non-extensible.
              use #(target_ext, state) <- result.try(is_extensible_stateful(
                state,
                t,
              ))
              case target_ext {
                True ->
                  Error(proxy_error(
                    state,
                    "'preventExtensions' on proxy: trap returned truish but the proxy target is extensible",
                  ))
                False -> Ok(#(state, True))
              }
            }
          }
        }
      }
    }
    Some(ObjectSlot(..) as slot) -> {
      let h = heap.write(state.heap, ref, ObjectSlot(..slot, extensible: False))
      Ok(#(State(..state, heap: h), True))
    }
    _ -> Ok(#(state, True))
  }
}

// ============================================================================
// TypedArray (Integer-Indexed exotic) element access — ES2024 §10.4.5
// ============================================================================

@external(erlang, "arc_typed_array_ffi", "ta_get_int")
fn ta_get_int(
  data: BitArray,
  byte_offset: Int,
  size_bits: Int,
  signed: Bool,
) -> Int

@external(erlang, "arc_typed_array_ffi", "ta_set_int")
fn ta_set_int(
  data: BitArray,
  byte_offset: Int,
  size_bits: Int,
  val: Int,
) -> BitArray

@external(erlang, "arc_typed_array_ffi", "ta_get_float")
fn ta_get_float(
  data: BitArray,
  byte_offset: Int,
  size_bits: Int,
) -> #(Int, Float)

@external(erlang, "arc_typed_array_ffi", "ta_set_float")
fn ta_set_float(
  data: BitArray,
  byte_offset: Int,
  size_bits: Int,
  tag: Int,
  val: Float,
) -> BitArray

@external(erlang, "arc_typed_array_ffi", "ta_clamp_uint8")
fn ta_clamp_uint8(tag: Int, val: Float) -> Int

/// Read the backing store of a non-detached ArrayBuffer slot.
/// None when the ref isn't an ArrayBuffer or the buffer is detached.
pub fn typed_array_buffer_data(h: Heap, buffer: Ref) -> Option(BitArray) {
  case heap.read(h, buffer) {
    Some(ObjectSlot(
      kind: value.ArrayBufferObject(data:, detached: False, ..),
      ..,
    )) -> Some(data)
    _ -> None
  }
}

/// §10.4.5.13 TypedArrayLength — current [[ArrayLength]] of a typed-array
/// view. `length: None` is [[ArrayLength]] = AUTO (a length-tracking view
/// over a resizable buffer): its element count follows the buffer's live
/// byte length. Detached buffers and tracking views whose byte offset lies
/// past the end of a shrunk buffer resolve to 0 — callers detect those as
/// out of bounds via the usual `byte_offset + len * size > byte_size` check
/// (with len = 0 that reduces to `byte_offset > byte_size`, which is exactly
/// the §10.4.5.14 IsTypedArrayOutOfBounds condition for AUTO views).
pub fn typed_array_view_length(
  h: Heap,
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> Int {
  case length {
    Some(n) -> n
    None ->
      case typed_array_buffer_data(h, buffer) {
        None -> 0
        Some(data) -> {
          let size = value.typed_array_element_size(elem_kind)
          int.max(0, { bit_array.byte_size(data) - byte_offset } / size)
        }
      }
  }
}

/// §10.4.5.15 IntegerIndexedElementGet — element at `idx`, or None when the
/// index is invalid (negative, >= length, or the buffer is detached).
pub fn typed_array_element(
  h: Heap,
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Int,
  idx: Int,
) -> Option(JsValue) {
  use <- bool.guard(idx < 0 || idx >= length, None)
  case typed_array_buffer_data(h, buffer) {
    None -> None
    Some(data) -> {
      let size = value.typed_array_element_size(elem_kind)
      let off = byte_offset + idx * size
      // §10.4.5.14 IsValidIntegerIndex: validate against the CURRENT backing
      // store, not the view's construction-time length — a resizable
      // ArrayBuffer may have shrunk below the view. An out-of-bounds view
      // behaves like a detached one: EVERY index is invalid, even ones whose
      // bytes still exist, so check the whole view, not just this element.
      let view_end = byte_offset + length * size
      case view_end <= bit_array.byte_size(data) {
        True -> Some(decode_typed_element(data, off, elem_kind))
        False -> None
      }
    }
  }
}

/// §23.1.5.1 CreateArrayIterator buffer-witness check for typed-array
/// sources: each `.next()` re-validates the view against the CURRENT buffer
/// (MakeTypedArrayWithBufferWitnessRecord + IsTypedArrayOutOfBounds) and
/// throws TypeError on a detached buffer or an out-of-bounds view (a
/// resizable buffer that shrank below the view). Ok(length) otherwise.
pub fn typed_array_iter_length(
  h: Heap,
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> Result(Int, String) {
  let length =
    typed_array_view_length(h, buffer, elem_kind, byte_offset, length)
  case typed_array_buffer_data(h, buffer) {
    None -> Error("Cannot perform operation on a detached ArrayBuffer")
    Some(data) -> {
      let size = value.typed_array_element_size(elem_kind)
      case byte_offset + length * size > bit_array.byte_size(data) {
        True -> Error("TypedArray is out of bounds")
        False -> Ok(length)
      }
    }
  }
}

/// Number of valid element indices of the view against the CURRENT buffer —
/// `length` when the view is fully in bounds, 0 when the buffer is detached
/// or the view is out of bounds (a resizable buffer that shrank below the
/// view): per §10.4.5.14 an out-of-bounds view has NO valid indices, even
/// for elements whose bytes still exist.
pub fn typed_array_live_length(
  h: Heap,
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Int,
) -> Int {
  case typed_array_buffer_data(h, buffer) {
    None -> 0
    Some(data) -> {
      let size = value.typed_array_element_size(elem_kind)
      case byte_offset + length * size <= bit_array.byte_size(data) {
        True -> length
        False -> 0
      }
    }
  }
}

/// Decode one element from the backing store (§25.1.2.10 GetValueFromBuffer).
fn decode_typed_element(
  data: BitArray,
  off: Int,
  elem_kind: value.TypedArrayKind,
) -> JsValue {
  case elem_kind {
    value.Int8Kind -> value.from_int(ta_get_int(data, off, 8, True))
    value.Uint8Kind | value.Uint8ClampedKind ->
      value.from_int(ta_get_int(data, off, 8, False))
    value.Int16Kind -> value.from_int(ta_get_int(data, off, 16, True))
    value.Uint16Kind -> value.from_int(ta_get_int(data, off, 16, False))
    value.Int32Kind -> value.from_int(ta_get_int(data, off, 32, True))
    value.Uint32Kind -> value.from_int(ta_get_int(data, off, 32, False))
    value.Float32Kind -> JsNumber(tagged_to_jsnum(ta_get_float(data, off, 32)))
    value.Float64Kind -> JsNumber(tagged_to_jsnum(ta_get_float(data, off, 64)))
    value.BigInt64Kind ->
      value.JsBigInt(value.BigInt(ta_get_int(data, off, 64, True)))
    value.BigUint64Kind ->
      value.JsBigInt(value.BigInt(ta_get_int(data, off, 64, False)))
  }
}

/// FFI float tag → JsNum. Tags: 0 finite, 1 NaN, 2 +Inf, 3 -Inf.
fn tagged_to_jsnum(tagged: #(Int, Float)) -> value.JsNum {
  case tagged {
    #(1, _) -> value.NaN
    #(2, _) -> value.Infinity
    #(3, _) -> value.NegInfinity
    #(_, f) -> Finite(f)
  }
}

/// JsNum → FFI float tag pair.
fn jsnum_to_tagged(n: value.JsNum) -> #(Int, Float) {
  case n {
    Finite(f) -> #(0, f)
    value.NaN -> #(1, 0.0)
    value.Infinity -> #(2, 0.0)
    value.NegInfinity -> #(3, 0.0)
  }
}

/// ToIntegerOrInfinity-style truncation for integer element stores:
/// NaN/±Infinity → 0 (the mod-2^n wrap in the FFI handles the rest).
fn jsnum_to_store_int(n: value.JsNum) -> Int {
  case n {
    Finite(f) -> value.float_to_int(f)
    _ -> 0
  }
}

/// §7.1.21 CanonicalNumericIndexString: "-0", or a string that round-trips
/// through ToNumber → ToString. Such keys on a TypedArray NEVER reach the
/// ordinary property table (§10.4.5).
pub fn is_canonical_numeric_string(s: String) -> Bool {
  case s {
    "-0" -> True
    _ ->
      case value.string_to_number(s) {
        value.NaN -> s == "NaN"
        value.Infinity -> s == "Infinity"
        value.NegInfinity -> s == "-Infinity"
        Finite(f) -> value.js_format_number(f) == s
      }
  }
}

/// §10.4.5.16 IntegerIndexedElementSet: convert the value first (observable —
/// valueOf / toString / @@toPrimitive may run user code), then store it if
/// `idx` is Some(valid index) and the buffer is live. Always returns True
/// (out-of-bounds typed-array writes are silent no-ops).
pub fn typed_array_store(
  state: State,
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
  idx: Option(Int),
  val: JsValue,
) -> Result(#(State, Bool), #(JsValue, State)) {
  case value.typed_array_is_bigint(elem_kind) {
    True -> {
      use #(n, state) <- result.try(ta_to_bigint(state, val))
      Ok(
        do_typed_store(
          state,
          buffer,
          elem_kind,
          byte_offset,
          length,
          idx,
          fn(data, off) { ta_set_int(data, off, 64, n) },
        ),
      )
    }
    False -> {
      use #(num, state) <- result.try(ta_to_number(state, val))
      Ok(
        do_typed_store(
          state,
          buffer,
          elem_kind,
          byte_offset,
          length,
          idx,
          fn(data, off) { encode_typed_number(data, off, elem_kind, num) },
        ),
      )
    }
  }
}

/// Shared store tail: bounds/detach check, then rebuild the buffer binary.
fn do_typed_store(
  state: State,
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
  idx: Option(Int),
  write: fn(BitArray, Int) -> BitArray,
) -> #(State, Bool) {
  case idx {
    Some(i) if i >= 0 ->
      case heap.read(state.heap, buffer) {
        Some(
          ObjectSlot(
            kind: value.ArrayBufferObject(
              data:,
              detached: False,
              max_byte_length:,
              shared:,
            ),
            ..,
          ) as slot,
        ) -> {
          let size = value.typed_array_element_size(elem_kind)
          let byte_size = bit_array.byte_size(data)
          // §10.4.5.14 IsValidIntegerIndex against the LIVE buffer, resolved
          // HERE (not at [[Set]] entry): the ToNumber/ToBigInt conversion
          // above may have run user code that resized the buffer. A
          // length-tracking view (None) follows the live byte length; a
          // fixed view that no longer fits is wholly out of bounds and the
          // write is a silent no-op, like a detached buffer.
          let len = case length {
            Some(n) -> n
            None -> int.max(0, { byte_size - byte_offset } / size)
          }
          let off = byte_offset + i * size
          case i < len && byte_offset + len * size <= byte_size {
            False -> #(state, True)
            True -> {
              let new_data = write(data, off)
              let h =
                heap.write(
                  state.heap,
                  buffer,
                  ObjectSlot(
                    ..slot,
                    kind: value.ArrayBufferObject(
                      data: new_data,
                      detached: False,
                      max_byte_length:,
                      shared:,
                    ),
                  ),
                )
              #(State(..state, heap: h), True)
            }
          }
        }
        // Detached (or not a buffer): silent no-op per §10.4.5.16 step 2.
        _ -> #(state, True)
      }
    // Out of bounds / non-integral canonical index: silent no-op.
    _ -> #(state, True)
  }
}

/// §25.1.2.12 SetValueInBuffer for Number content types.
fn encode_typed_number(
  data: BitArray,
  off: Int,
  elem_kind: value.TypedArrayKind,
  num: value.JsNum,
) -> BitArray {
  case elem_kind {
    value.Uint8ClampedKind -> {
      let #(tag, f) = jsnum_to_tagged(num)
      ta_set_int(data, off, 8, ta_clamp_uint8(tag, f))
    }
    value.Float32Kind -> {
      let #(tag, f) = jsnum_to_tagged(num)
      ta_set_float(data, off, 32, tag, f)
    }
    value.Float64Kind -> {
      let #(tag, f) = jsnum_to_tagged(num)
      ta_set_float(data, off, 64, tag, f)
    }
    value.Int8Kind | value.Uint8Kind ->
      ta_set_int(data, off, 8, jsnum_to_store_int(num))
    value.Int16Kind | value.Uint16Kind ->
      ta_set_int(data, off, 16, jsnum_to_store_int(num))
    value.Int32Kind | value.Uint32Kind ->
      ta_set_int(data, off, 32, jsnum_to_store_int(num))
    // BigInt kinds never reach here (typed_array_store routes them through
    // ta_to_bigint), but keep the encode total just in case.
    value.BigInt64Kind | value.BigUint64Kind ->
      ta_set_int(data, off, 64, jsnum_to_store_int(num))
  }
}

/// Minimal IsCallable for the local ToPrimitive walk.
fn ta_is_callable(h: Heap, val: JsValue) -> Bool {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: FunctionObject(..), ..)) -> True
        Some(ObjectSlot(kind: NativeFunction(..), ..)) -> True
        _ -> False
      }
    _ -> False
  }
}

/// ToNumber for typed-array element stores. Objects go through a number-hint
/// ToPrimitive (@@toPrimitive, then valueOf, then toString).
fn ta_to_number(
  state: State,
  val: JsValue,
) -> Result(#(value.JsNum, State), #(JsValue, State)) {
  case val {
    JsObject(_) -> {
      use #(prim, state) <- result.try(ta_to_primitive_number(state, val))
      ta_number_of_primitive(state, prim)
    }
    _ -> ta_number_of_primitive(state, val)
  }
}

fn ta_number_of_primitive(
  state: State,
  prim: JsValue,
) -> Result(#(value.JsNum, State), #(JsValue, State)) {
  case value.to_number(prim) {
    Ok(n) -> Ok(#(n, state))
    Error(msg) -> Error(state.type_error_value(state, msg))
  }
}

/// §7.1.1 ToPrimitive(input, number) for objects, local to typed-array
/// stores: @@toPrimitive("number") → OrdinaryToPrimitive(valueOf, toString).
fn ta_to_primitive_number(
  state: State,
  val: JsValue,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  case val {
    JsObject(ref) -> {
      use #(exotic, state) <- result.try(get_symbol_value(
        state,
        ref,
        value.symbol_to_primitive,
        val,
      ))
      case ta_is_callable(state.heap, exotic) {
        True -> {
          use #(res, state) <- result.try(
            state.call(state, exotic, val, [JsString("number")]),
          )
          case res {
            JsObject(_) ->
              Error(state.type_error_value(
                state,
                "Cannot convert object to primitive value",
              ))
            _ -> Ok(#(res, state))
          }
        }
        False -> ta_ordinary_to_primitive(state, val, ref, "valueOf")
      }
    }
    _ -> Ok(#(val, state))
  }
}

/// §7.1.1.1 OrdinaryToPrimitive with hint number: valueOf, then toString.
fn ta_ordinary_to_primitive(
  state: State,
  val: JsValue,
  ref: Ref,
  method: String,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  use #(f, state) <- result.try(get_value(state, ref, Named(method), val))
  let fallthrough = fn(state) {
    case method {
      "valueOf" -> ta_ordinary_to_primitive(state, val, ref, "toString")
      _ ->
        Error(state.type_error_value(
          state,
          "Cannot convert object to primitive value",
        ))
    }
  }
  case ta_is_callable(state.heap, f) {
    True -> {
      use #(res, state) <- result.try(state.call(state, f, val, []))
      case res {
        JsObject(_) -> fallthrough(state)
        _ -> Ok(#(res, state))
      }
    }
    False -> fallthrough(state)
  }
}

/// §7.1.13 ToBigInt for typed-array element stores. Numbers throw TypeError
/// (BigInt and Number content types never mix).
fn ta_to_bigint(
  state: State,
  val: JsValue,
) -> Result(#(Int, State), #(JsValue, State)) {
  case val {
    value.JsBigInt(value.BigInt(n)) -> Ok(#(n, state))
    value.JsBool(True) -> Ok(#(1, state))
    value.JsBool(False) -> Ok(#(0, state))
    JsString(s) ->
      case parse_bigint_string(s) {
        Some(n) -> Ok(#(n, state))
        // §7.1.13 ToBigInt: StringToBigInt returning undefined throws a
        // SyntaxError (not TypeError — that's for Number/Symbol/etc).
        None -> {
          let #(heap, err) =
            common.make_syntax_error(
              state.heap,
              state.builtins,
              "Cannot convert " <> s <> " to a BigInt",
            )
          Error(#(err, State(..state, heap:)))
        }
      }
    JsObject(_) -> {
      use #(prim, state) <- result.try(ta_to_primitive_number(state, val))
      case prim {
        JsObject(_) ->
          Error(state.type_error_value(
            state,
            "Cannot convert object to primitive value",
          ))
        _ -> ta_to_bigint(state, prim)
      }
    }
    _ ->
      Error(state.type_error_value(
        state,
        "Cannot convert " <> string.inspect(val) <> " to a BigInt",
      ))
  }
}

/// §7.1.14 StringToBigInt (decimal only — hex/octal/binary prefixes are a
/// known deviation here). Empty/whitespace-only → 0.
fn parse_bigint_string(s: String) -> Option(Int) {
  let t = string.trim(s)
  case t {
    "" -> Some(0)
    _ -> int.parse(t) |> option.from_result
  }
}

/// Encode an ALREADY-CONVERTED element value (JsNumber or JsBigInt) into the
/// backing store at byte offset `off`. No coercion, no user code — used by
/// TypedArray bulk operations (fill/slice/constructor copies).
pub fn typed_array_encode_value(
  data: BitArray,
  off: Int,
  elem_kind: value.TypedArrayKind,
  val: JsValue,
) -> BitArray {
  // Guard against writes past the CURRENT backing store (a resizable
  // ArrayBuffer may have shrunk below the view) — out-of-bounds typed-array
  // writes are silent no-ops, never crashes.
  use <- bool.guard(
    off + value.typed_array_element_size(elem_kind) > bit_array.byte_size(data),
    data,
  )
  case val {
    JsNumber(n) -> encode_typed_number(data, off, elem_kind, n)
    value.JsBigInt(value.BigInt(n)) -> ta_set_int(data, off, 64, n)
    // Callers always pass converted numerics; anything else encodes as NaN/0.
    _ -> encode_typed_number(data, off, elem_kind, value.NaN)
  }
}
