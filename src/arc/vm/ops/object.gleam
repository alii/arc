import arc/vm/heap
import arc/vm/inspect
import arc/vm/internal/elements
import arc/vm/internal/typed_array_ffi.{ta_get_float, ta_get_int}
import arc/vm/js_string
import arc/vm/key.{type PropertyKey, Index, Named, Private}
import arc/vm/opcode
import arc/vm/ops/typed_array_elements
import arc/vm/state.{type Heap, type HeapSlot, type State, State}
import arc/vm/value.{
  type JsElements, type JsValue, type ObjectKey, type Property, type Ref,
  type SymbolId, AccessorProperty, ArrayObject, DataProperty, Finite,
  FunctionObject, JsNumber, JsObject, JsString, NativeFunction, ObjectSlot,
  OrdinaryObject, StringPropKey, SymbolPropKey, string_object_key,
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
  state: State(host),
  val: JsValue,
  key: PropertyKey,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case val {
    // §7.3.3 step 1: V is already an Object, call O.[[Get]](P, V) directly.
    JsObject(ref) -> get_value(state, ref, key, val)
    JsString(s) ->
      // String primitive: synthesize own properties per §10.4.3.5
      // StringGetOwnProperty, then fall through to String.prototype.
      case key {
        // §10.4.3.4 StringCreate: "length" is an ordinary own data property
        // {value: len, W:F, E:F, C:F} (NOT produced by StringGetOwnProperty).
        Named("length") -> Ok(#(value.from_int(js_string.length(s)), state))
        // §10.4.3.5 steps 2-10: numeric index → single-char string
        Index(idx) ->
          case js_string.char_at(s, idx) {
            Some(ch) -> Ok(#(JsString(ch), state))
            // Out of bounds — delegate to String.prototype via [[Get]]
            None -> get_value(state, state.builtins.string.prototype, key, val)
          }
        // Not an own property — delegate to String.prototype via [[Get]]
        // (a private key on a string primitive throws before reaching here).
        Named(_) | Private(_) ->
          get_value(state, state.builtins.string.prototype, key, val)
      }
    // Primitive→prototype delegation (ToObject would wrap, we skip the wrapper)
    JsNumber(_) -> get_value(state, state.builtins.number.prototype, key, val)
    value.JsBool(_) ->
      get_value(state, state.builtins.boolean.prototype, key, val)
    value.JsSymbol(_) -> get_value(state, state.builtins.symbol_proto, key, val)
    // BigInt primitive → %BigInt.prototype% (toString/valueOf/…).
    value.JsBigInt(_) ->
      get_value(state, state.builtins.bigint.prototype, key, val)
    // null/undefined → JsUndefined; callers guard and throw TypeError as needed.
    _ -> Ok(#(value.JsUndefined, state))
  }
}

/// Symbol-keyed variant of get_value_of — delegates to the primitive's
/// prototype for symbol lookups (e.g. `"str"[Symbol.iterator]`).
pub fn get_symbol_value_of(
  state: State(host),
  val: JsValue,
  sym: SymbolId,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
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
      get_symbol_value(state, state.builtins.bigint.prototype, sym, val)
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
  state: State(host),
  ref: Ref,
  key: PropertyKey,
  receiver: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case heap.read(state.heap, ref) {
    // §10.4.6.8 Module Namespace [[Get]]: read the live binding cell, throwing
    // ReferenceError if it is still uninitialized (TDZ). Non-export keys fall
    // through to undefined (null prototype, no inheritance).
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
      namespace_get(state, exports, key)
    // §10.5.8 Proxy [[Get]] — route through the trap machinery.
    Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) ->
      proxy_get(state, slots, string_object_key(key), receiver)
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
          case
            typed_array_element_live(
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
  state: State(host),
  exports: dict.Dict(String, Ref),
  key: PropertyKey,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  let name = key.key_to_text(key)
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
  state: State(host),
  ref: Ref,
  names: List(String),
) -> Result(State(host), #(JsValue, State(host))) {
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
  state: State(host),
  prop: Property,
  receiver: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
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
  state: State(host),
  prototype: Option(Ref),
  key: PropertyKey,
  receiver: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
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
  /// The ref is a Proxy — the own-index probe cannot see through it; the
  /// caller must run the has/get traps (§10.5.7/§10.5.9). Detected on the
  /// same heap read as the probe, so per-element callers don't pay a
  /// separate as_proxy heap lookup.
  OwnIndexProxy
}

/// Fast variant of `get_own_property` for integer index keys. Identical
/// semantics, but on the Array/Arguments dense-elements hit it skips the
/// Some(DataProperty(..)) descriptor synthesis and returns the value directly.
/// Proxies report OwnIndexProxy instead of probing (their [[GetOwnProperty]]
/// is a trap, not a slot read).
pub fn get_own_index(heap: Heap(host), ref: Ref, idx: Int) -> OwnIndex {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(..), ..)) -> OwnIndexProxy
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
  heap: Heap(host),
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
  heap: Heap(host),
  exports: dict.Dict(String, Ref),
  key: PropertyKey,
) -> Option(Property) {
  case dict.get(exports, key.key_to_text(key)) {
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
  heap: Heap(host),
  kind: state.ExoticKind(host),
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
    //   indices, so they also yield undefined. Immutable ArrayBuffer
    //   proposal (sec-typedarray-getownproperty): over an immutable buffer
    //   the element descriptor is { value, W:F, E:T, C:F }.
    value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:) ->
      case key {
        Index(idx) -> {
          typed_array_element_live(
            heap,
            buffer,
            elem_kind,
            byte_offset,
            length,
            idx,
          )
          |> option.map(fn(v) {
            case typed_array_elements.buffer_is_immutable(heap, buffer) {
              True ->
                DataProperty(
                  value: v,
                  writable: False,
                  enumerable: True,
                  configurable: False,
                  seq: 0,
                )
              False -> value.data_property(v)
            }
          })
        }
        Named(s) ->
          case is_canonical_numeric_string(s) {
            True -> None
            False -> dict_get_option(properties, key)
          }
        // Private elements are ordinary dict entries.
        Private(_) -> dict_get_option(properties, key)
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
            // seq: 0 on virtual descriptors: array "length" never enumerates
            // through the seq-ordered named-key path (it is emitted as a
            // special case), so no fresh creation seq is needed.
            Some(DataProperty(writable:, enumerable:, configurable:, ..)) ->
              Some(DataProperty(
                value: value.from_int(length),
                writable:,
                enumerable:,
                configurable:,
                seq: 0,
              ))
            _ ->
              Some(DataProperty(
                value: value.from_int(length),
                writable: True,
                enumerable: False,
                configurable: False,
                seq: 0,
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
        Named(_) | Private(_) -> dict_get_option(properties, key)
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
        Named(_) | Private(_) -> dict_get_option(properties, key)
      }
    // --- String exotic [[GetOwnProperty]] (§10.4.3.1) ---
    value.StringObject(value: s) ->
      case key {
        // §10.4.3.4 StringCreate: "length" is an ordinary own data
        // property {value: len, W:F, E:F, C:F}; returned at §10.4.3.1 step 1.
        Named("length") ->
          // seq: 0 — synthesized virtual descriptor, never stored or
          // enumerated through the seq-ordered named-key path.
          Some(DataProperty(
            value: value.from_int(js_string.length(s)),
            writable: False,
            enumerable: False,
            configurable: False,
            seq: 0,
          ))
        // §10.4.3.5 steps 2-4: CanonicalNumericIndexString → integer index
        Index(idx) ->
          case js_string.char_at(s, idx) {
            // §10.4.3.5 step 10: return {value: char, W:F, E:T, C:F}
            Some(ch) ->
              // seq: 0 — synthesized Index-key descriptor; index keys
              // enumerate in numeric order, never by seq.
              Some(DataProperty(
                value: JsString(ch),
                writable: False,
                enumerable: True,
                configurable: False,
                seq: 0,
              ))
            // §10.4.3.5 step 8: index >= len → undefined, fall to ordinary
            None -> dict_get_option(properties, key)
          }
        // §10.4.3.1 step 1: OrdinaryGetOwnProperty is called first for ALL
        // keys; for non-index keys StringGetOwnProperty (step 3) yields
        // undefined, so the ordinary result is the only answer.
        Named(_) | Private(_) -> dict_get_option(properties, key)
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
  state: State(host),
  ref: Ref,
  key: PropertyKey,
  val: JsValue,
  receiver: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  case heap.read(state.heap, ref) {
    // §10.4.6.9 Module Namespace [[Set]]: always returns false (read-only).
    Some(ObjectSlot(kind: value.ModuleNamespace(..), ..)) -> Ok(#(state, False))
    // §10.5.9 Proxy [[Set]] — route through the trap machinery.
    Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) ->
      proxy_set(state, slots, string_object_key(key), val, receiver)
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
              typed_array_elements.typed_array_store(
                state,
                buffer,
                elem_kind,
                byte_offset,
                length,
                Some(idx),
                val,
              )
            False ->
              case
                typed_array_element_live(
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
                  typed_array_elements.typed_array_store(
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
            Named(_), _ | Private(_), _ -> True
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
                seq:,
                value: _,
              )),
              _
              if recv_ref == ref
            -> {
              let new_props =
                dict.insert(
                  properties,
                  key,
                  // Updating an existing key keeps its creation seq
                  // (§10.1.11 — the key keeps its enumeration position).
                  DataProperty(
                    value: val,
                    writable: True,
                    enumerable:,
                    configurable:,
                    seq:,
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
  state: State(host),
  ref: Ref,
  slot: HeapSlot(host),
  prototype: Option(Ref),
  own: Option(Property),
  key: PropertyKey,
  val: JsValue,
  receiver: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
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
  state: State(host),
  receiver: JsValue,
  key: PropertyKey,
  val: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
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
        Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) ->
          set_on_proxy_receiver(state, recv_ref, slots, string_object_key(key), val)
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
                typed_array_element_live(
                  state.heap,
                  buffer,
                  elem_kind,
                  byte_offset,
                  length,
                  idx,
                )
              {
                Some(_) ->
                  typed_array_elements.typed_array_store(
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
                  let #(h, outcome) =
                    set_property(state.heap, recv_ref, key, val)
                  define_outcome_to_bool(State(..state, heap: h), outcome)
                }
              }
            // Private elements bypass the integer-indexed exotic behaviour.
            Private(_) -> {
              let #(h, outcome) = set_property(state.heap, recv_ref, key, val)
              define_outcome_to_bool(State(..state, heap: h), outcome)
            }
          }
        // §10.1.9.2 steps 2.c-2.h: ordinary object — define/update own property.
        _ -> {
          let #(h, outcome) = set_property(state.heap, recv_ref, key, val)
          define_outcome_to_bool(State(..state, heap: h), outcome)
        }
      }
    // §10.1.9.2 step 2.b: Receiver is not an Object, return false.
    _ -> Ok(#(state, False))
  }
}

/// CPS guard: when `recv_ref` is a proxy, route the receiver write through
/// the proxy traps instead of the ordinary continuation.
fn proxy_receiver_guard(
  state: State(host),
  recv_ref: Ref,
  pk: ObjectKey,
  val: JsValue,
  cont: fn() -> Result(#(State(host), Bool), #(JsValue, State(host))),
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  case as_proxy(state.heap, recv_ref) {
    Some(slots) -> set_on_proxy_receiver(state, recv_ref, slots, pk, val)
    None -> cont()
  }
}

/// §10.1.9.2 OrdinarySetWithOwnDescriptor steps 2.c-2.e for a proxy receiver:
/// existingDescriptor = ? Receiver.[[GetOwnProperty]](P); if a data property,
/// Receiver.[[DefineOwnProperty]](P, { [[Value]]: V }); if absent,
/// CreateDataProperty(Receiver, P, V). Both go through the proxy's traps.
fn set_on_proxy_receiver(
  state: State(host),
  recv_ref: Ref,
  slots: Option(value.ProxySlots),
  pk: ObjectKey,
  val: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  // Step 2.c: existingDescriptor = ? Receiver.[[GetOwnProperty]](P) — the
  // canonical §10.5.5, invariants and all, not a private re-implementation.
  use #(existing, state) <- result.try(state.get_own_property(
    state,
    recv_ref,
    pk,
  ))
  case existing {
    // Step 2.d.i-ii: accessor or non-writable existing → false.
    Some(AccessorProperty(..)) -> Ok(#(state, False))
    Some(DataProperty(writable: False, ..)) -> Ok(#(state, False))
    Some(DataProperty(..)) -> proxy_receiver_define(state, slots, pk, val, False)
    None -> proxy_receiver_define(state, slots, pk, val, True)
  }
}

/// Receiver-side Proxy [[DefineOwnProperty]] — calls the defineProperty trap
/// with `{ value: V }` (existing data property) or the full CreateDataProperty
/// descriptor (absent property). No invariant checks (receiver-write path).
fn proxy_receiver_define(
  state: State(host),
  slots: Option(value.ProxySlots),
  pk: ObjectKey,
  val: JsValue,
  full: Bool,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(proxy_trap(
    state,
    slots,
    "defineProperty",
  ))
  case trap {
    // No trap → target.[[DefineOwnProperty]] — i.e. the ordinary receiver
    // write on the (possibly nested-proxy) target.
    None ->
      case pk {
        StringPropKey(pkey:, ..) -> set_on_receiver(state, JsObject(t), pkey, val)
        SymbolPropKey(sym) ->
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
          prop_key_value(pk),
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
  state: State(host),
  receiver: JsValue,
  ref: Ref,
  slot: HeapSlot(host),
  key: PropertyKey,
  val: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  case receiver {
    JsObject(recv_ref) if recv_ref == ref -> {
      let #(h, outcome) = set_property_on_slot(state.heap, ref, slot, key, val)
      define_outcome_to_bool(State(..state, heap: h), outcome)
    }
    _ -> set_on_receiver(state, receiver, key, val)
  }
}

/// §10.4.2.1 Array exotic [[DefineOwnProperty]] / OrdinaryDefineOwnProperty (§10.1.6.1).
///
/// Own-property-level write. Does NOT walk the proto chain — use set_value for
/// the full [[Set]] algorithm. Respects writable flag and extensible flag.
/// Returns `#(heap, DefineOutcome)` — see `DefineOutcome` for the three answers.
///
/// `Rejected` when:
///   - Existing property is non-writable (OrdinaryDefineOwnProperty step 3/4)
///   - New property on non-extensible object (step 2)
///   - StringObject in-range index key: §10.4.3.2 step 2b returns
///     IsCompatiblePropertyDescriptor, which is false for a value-change Desc.
///     "length" goes through step 3 OrdinaryDefineOwnProperty and is rejected
///     because StringCreate (§10.4.3.4) made it non-writable/non-configurable.
///   - ref is invalid / not an ObjectSlot
///
/// Callers decide what to do with `Rejected`: sloppy mode ignores it, strict
/// mode throws TypeError, and Array.prototype mutators always throw (they use
/// `Set(O, P, V, true)` per spec — the `true` flag means throw-on-failure).
/// `ThrewRangeError`, in contrast, is an abrupt completion every caller must
/// re-raise, sloppy mode included.
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
  h: Heap(host),
  ref: Ref,
  key: PropertyKey,
  val: JsValue,
) -> #(Heap(host), DefineOutcome) {
  case heap.read(h, ref) {
    Some(slot) -> set_property_on_slot(h, ref, slot, key, val)
    None -> #(h, Rejected)
  }
}

/// The three answers a [[DefineOwnProperty]] can give.
///
/// The spec's internal method returns a Boolean *or* an abrupt completion, and
/// exactly one define — ArraySetLength (§10.4.2.4 step 5) — takes the abrupt
/// exit without any user code running: `arr.length = -1` (also `= 3.5`, `= NaN`)
/// throws a **RangeError**. A `Bool` result cannot say that, so a rejection and
/// a throw used to collapse into the same `False`, and sloppy-mode assignment
/// swallowed the RangeError.
///
/// `ThrewRangeError` carries only the message: `set_property` is heap-only, and
/// allocating the error object needs a `State`. Callers hold one, and every one
/// of them must re-raise it — a RangeError from ArraySetLength is a genuine
/// abrupt completion, NOT a "return false" that sloppy mode may ignore.
pub type DefineOutcome {
  /// [[DefineOwnProperty]] returned true — the property was written.
  Defined
  /// [[DefineOwnProperty]] returned false. Sloppy assignment ignores it,
  /// strict assignment throws a TypeError, `Reflect.defineProperty` reports
  /// `false`, and Array.prototype mutators (`Set(O, P, V, true)`) throw.
  Rejected
  /// ArraySetLength §10.4.2.4 step 5: `newLen != ToNumber(Desc.[[Value]])`.
  /// Always re-thrown, in every mode.
  ThrewRangeError(message: String)
}

/// Collapse a `DefineOutcome` for a caller that only distinguishes success from
/// failure, throwing the RangeError through the ops-level `Result` channel.
/// The `state` heap must already be the one `set_property` returned.
fn define_outcome_to_bool(
  state: State(host),
  outcome: DefineOutcome,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  case outcome {
    Defined -> Ok(#(state, True))
    Rejected -> Ok(#(state, False))
    ThrewRangeError(msg) -> Error(state.range_error_value(state, msg))
  }
}

/// set_property given an already-read slot — skips the heap.read when the
/// caller (set_on_receiver_with_slot) already holds the slot for `ref`.
/// The heap must not have been mutated since the slot was read.
fn set_property_on_slot(
  h: Heap(host),
  ref: Ref,
  slot: HeapSlot(host),
  key: PropertyKey,
  val: JsValue,
) -> #(Heap(host), DefineOutcome) {
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
                False -> #(h, Rejected)
                True -> array_set_length(h, ref, val, slot, length)
              }
            // §10.4.2.1 step 2: If P is an array index (ToUint32 is valid index):
            Index(idx) ->
              case write_index_override(h, ref, slot, key, val) {
                Some(result) -> result
                None ->
                  // §10.4.2.1 step 2.h: If index >= oldLen and the length is
                  // not writable (or A is not extensible), return false.
                  case idx >= length && { !extensible || !length_writable } {
                    True -> #(h, Rejected)
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
                        Defined,
                      )
                    }
                  }
              }
            // §10.4.2.1 step 3: Not "length" and not array index —
            // OrdinaryDefineOwnProperty(A, P, Desc).
            Named(_) | Private(_) -> set_string_property(h, ref, key, val, slot)
          }
        }
        // --- §10.4.4.2 Arguments exotic [[DefineOwnProperty]] ---
        // Spec calls OrdinaryDefineOwnProperty then syncs [[ParameterMap]];
        // our element-based storage is an internal optimization.
        value.ArgumentsObject(_) ->
          case key {
            Index(idx) ->
              case write_index_override(h, ref, slot, key, val) {
                Some(result) -> result
                None ->
                  case !extensible && !elements.has(elements, idx) {
                    True -> #(h, Rejected)
                    False -> #(
                      heap.write(
                        h,
                        ref,
                        ObjectSlot(
                          ..slot,
                          elements: elements.set(elements, idx, val),
                        ),
                      ),
                      Defined,
                    )
                  }
              }
            Named(_) | Private(_) -> set_string_property(h, ref, key, val, slot)
          }
        // --- §10.4.3.2 String exotic [[DefineOwnProperty]] ---
        value.StringObject(value: s) -> {
          let len = js_string.length(s)
          // §10.4.3.2 step 2: If P is a CanonicalNumericIndexString for an
          // integer in [0, length), the property is non-configurable/non-writable,
          // so [[DefineOwnProperty]] returns false for any change.
          // "length" falls through to §10.4.3.2 step 3 (OrdinaryDefineOwnProperty),
          // which rejects because §10.4.3.4 StringCreate made it {W:F, C:F}.
          let is_guarded = case key {
            Named("length") -> True
            Index(idx) -> idx < len
            Named(_) | Private(_) -> False
          }
          case is_guarded {
            // §10.4.3.2 step 2b: returns IsCompatiblePropertyDescriptor(ext,
            // Desc, stringDesc) — false for our value-only Desc against a
            // {W:F, C:F} current descriptor. (Compatible no-op redefines would
            // return true per spec; we conservatively reject.)
            True -> #(h, Rejected)
            // §10.4.3.2 step 3: Else, OrdinaryDefineOwnProperty(S, P, Desc).
            False -> set_string_property(h, ref, key, val, slot)
          }
        }
        // --- §10.1.6.1 OrdinaryDefineOwnProperty for all other objects ---
        //
        // TypedArrays land here too, and only their §10.4.5.3 step 2 arm is
        // sound at this layer: a NON-canonical name is an ordinary property.
        // A canonical numeric index (step 1) must run TypedArraySetElement,
        // whose ToNumber/ToBigInt is observable — it needs a `State` and can
        // throw, neither of which this heap-only entry point can express. The
        // real MOP entry points do it: `set_value`'s TypedArray [[Set]] arm,
        // `set_on_receiver`'s TypedArray receiver arm, and builtins/object's
        // §10.4.5.3 [[DefineOwnProperty]] all route numeric indices to
        // `typed_array_elements.typed_array_store` before reaching here.
        _ -> set_string_property(h, ref, key, val, slot)
      }
    _ -> #(h, Rejected)
  }
}

/// Shared index arm of the array (§10.4.2.1) and arguments (§10.4.4.2) exotic
/// [[DefineOwnProperty]] paths: a defineProperty-created override may live in
/// the properties dict at an index that the dense element storage would
/// otherwise own. Honor its [[Writable]] and update the override in place so
/// the attribute flags survive the write.
///
/// `None` means no override exists at `key` — the caller takes its own dense
/// element path.
fn write_index_override(
  h: Heap(host),
  ref: Ref,
  slot: HeapSlot(host),
  key: PropertyKey,
  val: JsValue,
) -> Option(#(Heap(host), DefineOutcome)) {
  case slot {
    ObjectSlot(properties:, ..) ->
      case dict.get(properties, key) {
        Ok(DataProperty(
          writable: True,
          enumerable:,
          configurable:,
          seq:,
          value: _,
        )) -> {
          let new_props =
            dict.insert(
              properties,
              key,
              DataProperty(
                value: val,
                writable: True,
                enumerable:,
                configurable:,
                seq:,
              ),
            )
          Some(#(
            heap.write(h, ref, ObjectSlot(..slot, properties: new_props)),
            Defined,
          ))
        }
        Ok(DataProperty(writable: False, ..)) -> Some(#(h, Rejected))
        // Accessor overrides are routed to the setter by [[Set]] before
        // [[DefineOwnProperty]] is reached; reject here.
        Ok(AccessorProperty(..)) -> Some(#(h, Rejected))
        Error(Nil) -> None
      }
    _ -> None
  }
}

/// §10.4.2.4 ArraySetLength ( A, Desc ) — simplified.
///
/// Step 3: Let newLen be ToUint32(Desc.[[Value]]).
/// Step 5: If SameValueZero(newLen, ToNumber(Desc.[[Value]])) is false, throw
/// RangeError — `ThrewRangeError`, which every caller re-raises regardless of
/// strict mode. Only `InvalidLength` (a value we *did* coerce, and it wasn't a
/// uint32) reaches step 5; `UncoercibleLength` is our ToPrimitive gap and stays
/// a silent `Rejected` rather than inventing a spurious RangeError.
///
/// Steps 9-10: Let oldLen be the current "length" property's [[Value]].
/// Step 11: If newLen >= oldLen, set length and return true.
/// Step 12: If the old "length" property is non-writable, return false — the
/// caller checks that (`length_writable`) before calling in.
///
/// Step 18: Delete own elements with index >= newLen, in descending order.
/// TODO(Deviation): spec stops at first non-configurable element (step 18.b) and
/// returns false with length set to that index+1. Our elements have no
/// per-index descriptors, so all are implicitly configurable. Needs
/// per-element property descriptors to handle non-configurable indices.
fn array_set_length(
  h: Heap(host),
  ref: Ref,
  val: JsValue,
  slot: HeapSlot(host),
  old_length: Int,
) -> #(Heap(host), DefineOutcome) {
  // §10.4.2.4 steps 3-5: Coerce value to valid uint32 length.
  case coerce_length(h, val) {
    // Step 5: newLen != numberLen — throw RangeError, in every mode.
    InvalidLength -> #(h, ThrewRangeError("Invalid array length"))
    // Not a step-5 failure: we simply cannot run the coercion here (see
    // `length_to_number`). Preserve the pre-existing silent no-op.
    UncoercibleLength -> #(h, Rejected)
    CoercedLength(new_length) -> {
      let assert ObjectSlot(properties:, elements:, ..) = slot
      // §10.4.2.4 steps 8-18: If shrinking, delete own indices >= newLen in
      // descending order. Plain elements are implicitly configurable; dict
      // overrides (defineProperty-created) may be non-configurable — the
      // largest such index stops the truncation (step 17.b: length becomes
      // index + 1 and the operation reports failure).
      case new_length < old_length {
        False -> #(
          heap.write(h, ref, ObjectSlot(..slot, kind: ArrayObject(new_length))),
          Defined,
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
          // §10.4.2.4 step 18: delete own indices >= final_len. Filtering the
          // storage directly beats iterating [final_len, old_length), which
          // could be billions of indices for a sparse array.
          let new_elements = elements.truncate(elements, final_len)
          let new_properties =
            dict.filter(properties, fn(k, _prop) {
              case k {
                Index(i) -> i < final_len
                Named(_) | Private(_) -> True
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
            case blocked {
              // Step 17.b: a non-configurable index stopped the truncation —
              // length is set to that index + 1 and the define reports false.
              Some(_) -> Rejected
              option.None -> Defined
            },
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
/// `value.array_length` is that validation: whatever it rejects — fractional,
/// negative, or `>= 2^32` — is `InvalidLength` (step-5 RangeError), as are NaN
/// and ±Infinity. Values whose ToNumber we cannot compute here are
/// `UncoercibleLength` — a deviation, NOT a step-5 failure. This covers both
/// internal Set(O,"length",n,true) calls from Array.prototype mutators and
/// user-level `arr.length = 3.5`.
fn coerce_length(h: Heap(host), val: JsValue) -> LengthCoercion {
  case length_to_number(h, val) {
    None -> UncoercibleLength
    Some(Finite(f)) ->
      value.array_length(f)
      |> option.map(CoercedLength)
      |> option.unwrap(InvalidLength)
    // NaN / ±Infinity: ToUint32 differs from ToNumber, so step 5 throws.
    Some(value.NaN) | Some(value.Infinity) | Some(value.NegInfinity) ->
      InvalidLength
  }
}

/// The three answers §10.4.2.4 steps 3-5 can give us. `InvalidLength` and
/// `UncoercibleLength` used to be one `None`, which made a spec RangeError and
/// an Arc deviation indistinguishable at the call site.
type LengthCoercion {
  /// ToUint32(Desc.[[Value]]) == ToNumber(Desc.[[Value]]) — a real length.
  CoercedLength(length: Int)
  /// Step 5: coerced fine, but the number is not a uint32 (`-1`, `3.5`,
  /// `2 ** 32`, `NaN`, `Infinity`, `undefined`) → RangeError.
  InvalidLength
  /// TODO(Deviation): ToNumber here would need user code (a plain object's
  /// valueOf/toString) or should be a TypeError (Symbol, BigInt). Neither is
  /// possible from this heap-only layer, so the define is silently rejected —
  /// wrong, but strictly better than throwing a RangeError the spec never
  /// throws. Fix by hoisting ToPrimitive to a State-carrying caller.
  UncoercibleLength
}

/// ToNumber for array-length values (§10.4.2.4 step 4), without running user
/// code: primitives coerce directly; Number/String/Boolean wrapper objects
/// unwrap their primitive data (what ToPrimitive returns when valueOf is
/// intact). `None` means "not coercible here" — see `UncoercibleLength`; it is
/// deliberately NOT the same as coercing to NaN.
fn length_to_number(h: Heap(host), val: JsValue) -> Option(value.JsNum) {
  case val {
    JsNumber(n) -> Some(n)
    JsString(s) -> Some(value.string_to_number(s))
    value.JsBool(True) -> Some(Finite(1.0))
    value.JsBool(False) | value.JsNull -> Some(Finite(0.0))
    value.JsUndefined -> Some(value.NaN)
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: value.NumberObject(value: n), ..)) -> Some(n)
        Some(ObjectSlot(kind: value.StringObject(value: s), ..)) ->
          Some(value.string_to_number(s))
        Some(ObjectSlot(kind: value.BooleanObject(value: True), ..)) ->
          Some(Finite(1.0))
        Some(ObjectSlot(kind: value.BooleanObject(value: False), ..)) ->
          Some(Finite(0.0))
        // Plain object: needs ToPrimitive → user valueOf/toString.
        _ -> None
      }
    // Symbol / BigInt (spec: TypeError), TDZ sentinel (never a JS value).
    value.JsSymbol(_) | value.JsBigInt(_) | value.JsUninitialized -> None
  }
}

/// §10.1.6.1 OrdinaryDefineOwnProperty / §10.1.6.3 ValidateAndApplyPropertyDescriptor
/// (value-update subset).
///
/// Set a string-keyed own property in the properties dict. Never throws — an
/// ordinary define only ever answers `Defined` or `Rejected`.
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
  h: Heap(host),
  ref: Ref,
  key: PropertyKey,
  val: JsValue,
  slot: HeapSlot(host),
) -> #(Heap(host), DefineOutcome) {
  case slot {
    ObjectSlot(properties:, extensible:, ..) ->
      // §10.1.6.1 step 1: Let current be ? O.[[GetOwnProperty]](P).
      case dict.get(properties, key) {
        // §10.1.6.3 step 6.c: current exists and is writable data — update [[Value]].
        // Keeps the existing creation seq: an updated key keeps its
        // enumeration position (§10.1.11).
        Ok(DataProperty(writable: True, enumerable:, configurable:, seq:, ..)) -> {
          let new_props =
            dict.insert(
              properties,
              key,
              DataProperty(
                value: val,
                writable: True,
                enumerable:,
                configurable:,
                seq:,
              ),
            )
          #(
            heap.write(h, ref, ObjectSlot(..slot, properties: new_props)),
            Defined,
          )
        }
        // §10.1.6.3 step 5.e: current.[[Writable]] is false → reject.
        Ok(DataProperty(writable: False, ..)) -> #(h, Rejected)
        // Accessor property: [[DefineOwnProperty]] with just a value on an
        // accessor would convert it to data, but we don't support that yet.
        Ok(value.AccessorProperty(..)) -> #(h, Rejected)
        // §10.1.6.3 step 2: current is undefined — property doesn't exist.
        Error(Nil) ->
          case extensible {
            // §10.1.6.3 step 2.a: If extensible is false, return false.
            False -> #(h, Rejected)
            // §10.1.6.3 steps 2.b-2.e: extensible is true — create new data
            // property with {[[Value]]: V, [[Writable]]: true,
            // [[Enumerable]]: true, [[Configurable]]: true}.
            True -> {
              let new_props =
                dict.insert(properties, key, value.data_property(val))
              #(
                heap.write(h, ref, ObjectSlot(..slot, properties: new_props)),
                Defined,
              )
            }
          }
      }
    _ -> #(h, Rejected)
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
  heap: Heap(host),
  ref: Ref,
  key: PropertyKey,
  val: JsValue,
) -> Heap(host) {
  define_own_property(heap, ref, key, val)
}

fn define_own_property(
  heap: Heap(host),
  ref: Ref,
  key: PropertyKey,
  val: JsValue,
) -> Heap(host) {
  use slot <- heap.update(heap, ref)
  let assert ObjectSlot(properties:, ..) = slot
    as "define_own_property target is not an ObjectSlot"
  // Single-traversal FFI upsert: a brand-new key gets a fresh creation
  // seq, a re-defined key keeps its old one (§10.1.11 — e.g. duplicate
  // keys in an object literal keep the first key's position).
  ObjectSlot(
    ..slot,
    properties: ffi_define_own_data_property(properties, key, val),
  )
}

/// §7.3.5 CreateDataProperty upsert in one map traversal — see
/// define_own_data_property in arc_vm_ffi.erl. New keys get a fresh
/// creation seq; existing keys keep theirs (§10.1.11).
@external(erlang, "arc_vm_ffi", "define_own_data_property")
fn ffi_define_own_data_property(
  properties: dict.Dict(PropertyKey, Property),
  key: PropertyKey,
  val: JsValue,
) -> dict.Dict(PropertyKey, Property)

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
  heap: Heap(host),
  ref: Ref,
  key: PropertyKey,
  val: JsValue,
) -> Heap(host) {
  // §7.3.32 PrivateSet: writing to a private METHOD throws TypeError. Arc
  // stores private methods as Private-keyed properties (key.private_key), so
  // mark them non-writable — set_found_value then reports failure and
  // PutPrivateField turns that into the spec'd TypeError.
  let prop = case value.is_private_name(key) {
    True -> value.data(val) |> value.configurable()
    False -> value.builtin_property(val)
  }
  use slot <- heap.update(heap, ref)
  let assert ObjectSlot(properties:, ..) = slot
    as "define_method_property target is not an ObjectSlot"
  // Re-defining an existing key keeps its creation seq (§10.1.11).
  let prop = case dict.get(properties, key) {
    Ok(old) -> value.with_seq_of(prop, old)
    Error(Nil) -> prop
  }
  ObjectSlot(..slot, properties: dict.insert(properties, key, prop))
}

/// [[Extensible]] of an object slot (non-stateful read — no proxy trap).
/// Used by the DefinePrivate* ops: proposal nonextensible-applies-to-private
/// makes PrivateFieldAdd / PrivateMethodOrAccessorAdd throw on non-extensible
/// receivers.
pub fn slot_extensible(heap: Heap(host), ref: Ref) -> Bool {
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
  heap: Heap(host),
  ref: Ref,
  key: PropertyKey,
  val: JsValue,
  writable writable: Bool,
) -> Heap(host) {
  let prop = case writable {
    True -> value.data(val) |> value.writable() |> value.configurable()
    False -> value.data(val) |> value.configurable()
  }
  use slot <- heap.update(heap, ref)
  let assert ObjectSlot(properties:, ..) = slot
    as "define_private_data target is not an ObjectSlot"
  ObjectSlot(..slot, properties: dict.insert(properties, key, prop))
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
  // Merging into an existing key keeps its creation seq (§10.1.11) — e.g.
  // `{ get x() {}, a: 1, set x(v) {} }` keeps "x" before "a".
  let seq = case existing {
    Ok(old) -> value.prop_seq(old)
    Error(Nil) -> value.next_prop_seq()
  }
  case kind {
    opcode.Getter ->
      AccessorProperty(
        get: Some(func),
        set:,
        enumerable:,
        configurable: True,
        seq:,
      )
    opcode.Setter ->
      AccessorProperty(
        get:,
        set: Some(func),
        enumerable:,
        configurable: True,
        seq:,
      )
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
  heap: Heap(host),
  ref: Ref,
  key: PropertyKey,
  func: JsValue,
  kind: opcode.AccessorKind,
  enumerable enumerable: Bool,
) -> Heap(host) {
  use slot <- heap.update(heap, ref)
  let assert ObjectSlot(properties:, ..) = slot
    as "define_accessor target is not an ObjectSlot"
  let new_prop =
    merge_accessor(dict.get(properties, key), func, kind, enumerable)
  ObjectSlot(..slot, properties: dict.insert(properties, key, new_prop))
}

/// Symbol-keyed variant of define_accessor — used by DefineAccessorComputed
/// when the computed key is a Symbol (e.g. `{ get [Symbol.iterator]() {} }`).
pub fn define_symbol_accessor(
  heap: Heap(host),
  ref: Ref,
  sym: SymbolId,
  func: JsValue,
  kind: opcode.AccessorKind,
  enumerable enumerable: Bool,
) -> Heap(host) {
  use slot <- heap.update(heap, ref)
  let assert ObjectSlot(symbol_properties:, ..) = slot
    as "define_symbol_accessor target is not an ObjectSlot"
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

/// §10.1.5.1 OrdinaryGetOwnProperty ( O, P ) — symbol-keyed variant.
/// Returns the own property descriptor for a symbol key, or None.
pub fn get_own_symbol_property(
  heap: Heap(host),
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
pub fn has_symbol_property(heap: Heap(host), ref: Ref, sym: SymbolId) -> Bool {
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

/// The answer of a **pure** internal-method read: one that runs no user code,
/// cannot throw, and therefore only has a defined answer while every object it
/// touches is ordinary.
///
/// `NeedsTraps(ref)` says a Proxy was reached at `ref`, whose internal methods
/// ARE traps — JS functions that observe the read, can mutate the heap, and can
/// throw. There is no honest `Bool`/`Option` for that case, and this type
/// refuses to invent one: a proxy's internal property table is always empty, so
/// answering from it silently reports "absent" for every key. Callers must say
/// what they do — delegate to the `*_stateful` twin, or spell out (via
/// `or_when_proxy`) why a conservative answer is safe there.
pub type PureLookup(a) {
  Answered(a)
  NeedsTraps(Ref)
}

/// Collapse a `PureLookup` by naming, at the call site, the answer a Proxy
/// gets. Legitimate ONLY when the fallback is conservative — "not the
/// intrinsic, so take the slow path", "a proxy carries no private elements" —
/// never as a shortcut for "proxies probably don't reach here".
pub fn or_when_proxy(lookup: PureLookup(a), when_proxy: a) -> a {
  case lookup {
    Answered(a) -> a
    NeedsTraps(_) -> when_proxy
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
/// Checks own properties (including elements for array-like objects via
/// get_own_property), then walks the prototype chain.
///
/// The `in` operator and Reflect.has use `has_property_stateful` instead: a
/// Proxy anywhere on the chain makes step 1 or step 4 the `has` trap, which
/// this pure walk cannot run — so it stops there and reports `NeedsTraps`
/// rather than reading the proxy's (always empty) internal property table and
/// answering "absent" for everything.
pub fn has_property(
  heap: Heap(host),
  ref: Ref,
  key: PropertyKey,
) -> PureLookup(Bool) {
  // Private-name keys ("#x") are invisible to ordinary [[HasProperty]] —
  // spec privates live in [[PrivateElements]], not the property table. The
  // brand check for `#x in obj` uses find_property (PrivateIn opcode), not
  // this function.
  use <- bool.guard(value.is_private_name(key), Answered(False))
  case heap.read(heap, ref) {
    // §10.5.7 Proxy [[HasProperty]] IS the `has` trap — not purely answerable.
    Some(ObjectSlot(kind: value.ProxyObject(..), ..)) -> NeedsTraps(ref)
    // §10.4.6.6 Module Namespace [[HasProperty]]: true iff the key is an
    // exported name. Null prototype → no inheritance.
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
      Answered(dict.has_key(exports, key.key_to_text(key)))
    Some(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) ->
      // Step 1-2: Let hasOwn be O.[[GetOwnProperty]](P). If not undefined, return true.
      case own_property_of_slot(heap, kind, properties, elements, key) {
        Some(_) -> Answered(True)
        // §10.4.5.2 TypedArray [[HasProperty]]: a canonical numeric index key
        // answers IsValidIntegerIndex directly — own_property_of_slot already
        // said the index is invalid, so the answer is false WITHOUT consulting
        // the prototype chain (TypedArray.prototype["1.5"] is unreachable).
        None ->
          case typed_array_numeric_key(kind, key) {
            True -> Answered(False)
            False ->
              // Step 3-4: Let parent be O.[[GetPrototypeOf]](). If not null, recurse.
              case prototype {
                Some(proto_ref) -> has_property(heap, proto_ref, key)
                // Step 5: Return false (null prototype).
                None -> Answered(False)
              }
          }
      }
    _ -> Answered(False)
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
      Named(_) | Private(_) -> acc
    }
  })
}

/// True when `key` is a canonical numeric index string on a TypedArray —
/// such keys are fully resolved by the integer-indexed exotic behaviour
/// (§10.4.5) and must never fall through to the prototype chain.
fn typed_array_numeric_key(
  kind: state.ExoticKind(host),
  key: PropertyKey,
) -> Bool {
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
///
/// A Proxy on the chain stops the walk with `NeedsTraps`: its descriptors live
/// behind the `getOwnPropertyDescriptor` trap, so no descriptor can be produced
/// without running user code.
pub fn find_property(
  heap: Heap(host),
  ref: Ref,
  key: PropertyKey,
) -> PureLookup(Option(Property)) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(..), ..)) -> NeedsTraps(ref)
    Some(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) ->
      case own_property_of_slot(heap, kind, properties, elements, key) {
        Some(prop) -> Answered(Some(prop))
        None ->
          case prototype {
            Some(proto_ref) -> find_property(heap, proto_ref, key)
            None -> Answered(None)
          }
      }
    _ -> Answered(None)
  }
}

/// Symbol-keyed twin of `find_property`: walk the prototype chain for `sym`
/// WITHOUT running any getter. A Proxy on the chain stops the walk with
/// `NeedsTraps` — its `get`/`getOwnPropertyDescriptor` traps are user code, so
/// no descriptor exists to return purely. Used as a protector read (e.g.
/// `intrinsic_iterator_guard`), never as an observable [[Get]].
pub fn find_symbol_property(
  heap: Heap(host),
  ref: Ref,
  sym: SymbolId,
) -> PureLookup(Option(Property)) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(..), ..)) -> NeedsTraps(ref)
    Some(ObjectSlot(symbol_properties:, prototype:, ..)) ->
      case list.key_find(symbol_properties, sym) {
        Ok(prop) -> Answered(Some(prop))
        Error(Nil) ->
          case prototype {
            Some(proto_ref) -> find_symbol_property(heap, proto_ref, sym)
            None -> Answered(None)
          }
      }
    _ -> Answered(None)
  }
}

/// §10.1.9.2 OrdinarySetWithOwnDescriptor steps 2-7 given an already-found
/// ownDesc (e.g. from find_property — avoids set_value's second chain walk).
///
/// Step 2.a: non-writable data → False. Steps 2.b-2.h: writable data →
/// create/update own data property on the receiver. Steps 4-5: accessor
/// without setter → False. Steps 6-7: Call(setter, Receiver, « V ») → True.
pub fn set_found_value(
  state: State(host),
  receiver: JsValue,
  prop: Property,
  key: PropertyKey,
  val: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
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
  h: Heap(host),
  ref: Ref,
  sym: value.SymbolId,
) -> #(Heap(host), Bool) {
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

pub fn delete_property(
  h: Heap(host),
  ref: Ref,
  key: PropertyKey,
) -> #(Heap(host), Bool) {
  case heap.read(h, ref) {
    // §10.4.6.10 Module Namespace [[Delete]]: deleting an exported name fails
    // (non-configurable); a non-export "succeeds" vacuously.
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
      case dict.has_key(exports, key.key_to_text(key)) {
        True -> #(h, False)
        False -> #(h, True)
      }
    Some(ObjectSlot(kind:, elements:, properties:, ..) as slot) -> {
      // §10.1.10.1 OrdinaryDelete for the string/private-key case: shared by
      // every exotic arm below that falls back to ordinary behavior.
      let ordinary_delete = fn() {
        case ordinary_delete_outcome(properties, key) {
          DeleteRemoved(rest) -> #(
            heap.write(h, ref, ObjectSlot(..slot, properties: rest)),
            True,
          )
          DeleteVacuous -> #(h, True)
          DeleteRefused -> #(h, False)
        }
      }
      case kind {
        // §10.4.5.5 TypedArray [[Delete]]: canonical numeric index keys are
        // deletable iff they are NOT valid indices (nothing to delete);
        // a live element is non-configurable from delete's point of view.
        value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:) ->
          case key {
            Index(idx) ->
              case
                typed_array_element_live(
                  h,
                  buffer,
                  elem_kind,
                  byte_offset,
                  length,
                  idx,
                )
              {
                Some(_) -> #(h, False)
                None -> #(h, True)
              }
            Named(s) ->
              case is_canonical_numeric_string(s) {
                True -> #(h, True)
                False -> ordinary_delete()
              }
            Private(_) -> ordinary_delete()
          }
        // Array/Arguments exotic: check if key is an array index
        ArrayObject(_) | value.ArgumentsObject(_) ->
          case key {
            Index(idx) ->
              // Dict override (defineProperty-created attributes) wins:
              // §10.1.10.1 step 3 honors its [[Configurable]].
              case dict_get_option(properties, key) {
                Some(prop) ->
                  case value.prop_configurable(prop) {
                    True -> #(
                      heap.write(
                        h,
                        ref,
                        ObjectSlot(
                          ..slot,
                          properties: dict.delete(properties, key),
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
                _ -> ordinary_delete()
              }
            Named(_) | Private(_) -> ordinary_delete()
          }
        // String exotic: "length" and in-range indices are synthesized
        // non-configurable properties (§10.4.3) — never deletable.
        value.StringObject(value: s) ->
          case key {
            Named("length") -> #(h, False)
            Index(i) ->
              case js_string.char_at(s, i) {
                Some(_) -> #(h, False)
                None -> ordinary_delete()
              }
            Named(_) | Private(_) -> ordinary_delete()
          }
        _ -> ordinary_delete()
      }
    }
    // Step 2: No slot found — treat as non-existent, return true.
    _ -> #(h, True)
  }
}

/// Outcome of §10.1.10.1 OrdinaryDelete ( O, P ) run against a properties
/// dict — the string/private-key case.
///
/// Step 1: Let desc be ? O.[[GetOwnProperty]](P).
/// Step 2: If desc is undefined, return true.
/// Step 3: If desc.[[Configurable]] is true, remove and return true.
/// Step 4: Return false.
type DeleteOutcome {
  /// Step 3: configurable → the properties dict with the key removed.
  DeleteRemoved(dict.Dict(PropertyKey, Property))
  /// Step 2: no such own property → nothing to write, "true".
  DeleteVacuous
  /// Step 4: non-configurable → "false".
  DeleteRefused
}

fn ordinary_delete_outcome(
  properties: dict.Dict(PropertyKey, Property),
  key: PropertyKey,
) -> DeleteOutcome {
  case dict.get(properties, key) {
    Ok(DataProperty(configurable: True, ..))
    | Ok(value.AccessorProperty(configurable: True, ..)) ->
      DeleteRemoved(dict.delete(properties, key))
    Ok(DataProperty(configurable: False, ..))
    | Ok(value.AccessorProperty(configurable: False, ..)) -> DeleteRefused
    Error(Nil) -> DeleteVacuous
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
/// Own string-keyed properties of an object in §10.1.11 OrdinaryOwnPropertyKeys
/// order, each paired with its [[Enumerable]] flag:
///   1. array-index keys (elements fast store merged with dict overrides) in
///      ascending numeric order;
///   2. the virtual "length" of Array/String exotics (non-enumerable, created
///      at object birth — before any user-added named key);
///   3. other string keys in ascending property-creation order (Property.seq).
/// Module namespaces emit their exports sorted by code-unit order (§10.4.6.11).
/// Private names are excluded. Symbol keys are not included — they live in
/// symbol_properties, which is already a creation-ordered list.
///
/// This is THE single funnel for own string-key enumeration order: for-in
/// (enumerate_keys), Object.keys/values/entries/assign, getOwnPropertyNames,
/// Reflect.ownKeys, JSON.stringify and spread/rest all route through it
/// (directly or via collect_own_keys in builtins/object).
pub fn own_string_keys_flagged(
  heap: Heap(host),
  ref: Ref,
) -> List(#(String, Bool)) {
  case heap.read(heap, ref) {
    // §10.4.6.11 Module Namespace [[OwnPropertyKeys]]: the exported names,
    // sorted by code-unit order. All are enumerable.
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
      list.sort(dict.keys(exports), string.compare)
      |> list.map(fn(name) { #(name, True) })
    Some(ObjectSlot(kind:, properties:, elements:, ..)) -> {
      let is_array = case kind {
        ArrayObject(_) -> True
        _ -> False
      }
      // Element-store indices (Array/Arguments fast storage) — always
      // enumerable data properties. String exotic synthesizes one index per
      // code point (§10.4.3.3 step 3); TypedArray its live indices
      // (§10.4.5.7 step 2.a).
      let elem_indices = case kind {
        ArrayObject(length:) | value.ArgumentsObject(length:) ->
          elements.indices(elements)
          |> list.filter(fn(idx) { idx < length })
        value.StringObject(value: s) ->
          int.range(
            from: js_string.length(s) - 1,
            to: -1,
            with: [],
            run: fn(acc, i) { [i, ..acc] },
          )
        value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:) -> {
          let n =
            typed_array_live_count(heap, buffer, elem_kind, byte_offset, length)
          int.range(from: n - 1, to: -1, with: [], run: fn(acc, i) {
            [i, ..acc]
          })
        }
        _ -> []
      }
      // Split dict entries; the Named("length") dict entry on arrays only
      // tracks frozen-length attributes — the visible key is emitted as the
      // virtual length_key below.
      let #(dict_indices, named) =
        dict.fold(properties, #([], []), fn(acc, key, prop) {
          let #(idx, named) = acc
          case key {
            Index(i) -> #([#(i, value.prop_enumerable(prop)), ..idx], named)
            Named("length") if is_array -> acc
            // Private names ("#x") never appear in [[OwnPropertyKeys]].
            Private(_) -> acc
            Named(n) -> #(idx, [
              #(value.prop_seq(prop), n, value.prop_enumerable(prop)),
              ..named
            ])
          }
        })
      // Step 1: array-index keys in ascending numeric order. An index lives
      // in exactly one store (elements or dict override), so no dedup needed.
      let index_keys =
        list.fold(elem_indices, dict_indices, fn(acc, i) { [#(i, True), ..acc] })
        |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
        |> list.map(fn(pair) { #(int.to_string(pair.0), pair.1) })
      // Array exotic: "length" is a non-enumerable own property (§10.4.2);
      // String exotic likewise (§10.4.3.4 StringCreate). Both exist from
      // object birth, so they precede all user-created named keys.
      let length_key = case kind {
        ArrayObject(_) | value.StringObject(_) -> [#("length", False)]
        _ -> []
      }
      // Step 2: other string keys in property-creation order.
      let named_keys =
        list.sort(named, fn(a, b) { int.compare(a.0, b.0) })
        |> list.map(fn(entry) { #(entry.1, entry.2) })
      list.flatten([index_keys, length_key, named_keys])
    }
    _ -> []
  }
}

pub fn enumerate_keys(heap: Heap(host), ref: Ref) -> List(String) {
  enumerate_keys_loop(heap, Some(ref), set.new(), [])
}

/// Helper for enumerate_keys — walks the prototype chain collecting
/// enumerable string keys, skipping shadowed keys via the seen set.
///
/// Per object, keys come from own_string_keys_flagged in §10.1.11
/// [[OwnPropertyKeys]] order (indices ascending, then named keys in creation
/// order). Non-enumerable own keys are added to seen (they shadow enumerable
/// prototype keys — §14.7.5.9 EnumerateObjectProperties) but not emitted.
fn enumerate_keys_loop(
  heap: Heap(host),
  current: Option(Ref),
  seen: set.Set(String),
  acc: List(String),
) -> List(String) {
  case current {
    // No more objects in chain → return collected results.
    None -> list.reverse(acc)
    Some(ref) -> {
      let #(acc, seen) =
        list.fold(
          own_string_keys_flagged(heap, ref),
          #(acc, seen),
          fn(state, pair) {
            let #(a, s) = state
            let #(k, enumerable) = pair
            case set.contains(s, k) {
              True -> state
              False ->
                case enumerable {
                  True -> #([k, ..a], set.insert(s, k))
                  // Non-enumerable: mark seen but don't include.
                  False -> #(a, set.insert(s, k))
                }
            }
          },
        )
      // Walk prototype chain (for-in extension). Module namespaces have a
      // null prototype, ending the walk.
      let prototype = case heap.read(heap, ref) {
        Some(ObjectSlot(prototype:, ..)) -> prototype
        _ -> None
      }
      enumerate_keys_loop(heap, prototype, seen, acc)
    }
  }
}

// ============================================================================
// Symbol-keyed property access

/// §7.3.22 SpeciesConstructor ( O, defaultConstructor )
///
///   1. Let C be ? Get(O, "constructor").
///   2. If C is undefined, return defaultConstructor.
///   3. If C is not an Object, throw a TypeError exception.
///   4. Let S be ? Get(C, @@species).
///   5. If S is either undefined or null, return defaultConstructor.
///   6. If IsConstructor(S) is true, return S.
///   7. Throw a TypeError exception.
///
/// The single implementation for the whole engine (ArrayBuffer.prototype.slice,
/// RegExp.prototype[@@split] / [@@matchAll], Promise.prototype.then, ...): one
/// pair of throw messages, one set of getter/proxy-trap-aware [[Get]]s.
///
/// Returns a bare `Ref`, not a `JsValue`: steps 6-7 have already proved the
/// result is a constructor object, so no caller can forget to re-check.
///
/// NOTE: ArraySpeciesCreate (§23.1.3.4) and TypedArraySpeciesCreate (§23.2.4.1)
/// are *different* abstract ops (extra IsArray / cross-realm / default-detection
/// steps) — they are not thin wrappers around this one.
pub fn species_constructor(
  state: State(host),
  o: JsValue,
  default_ctor: Ref,
) -> Result(#(Ref, State(host)), #(JsValue, State(host))) {
  // Step 1: C = ? Get(O, "constructor").
  use #(c, state) <- result.try(get_value_of(state, o, Named("constructor")))
  case c {
    // Step 2: absent constructor → the intrinsic default.
    value.JsUndefined -> Ok(#(default_ctor, state))
    JsObject(_) -> {
      // Step 4: S = ? Get(C, @@species).
      use #(s, state) <- result.try(get_symbol_value_of(
        state,
        c,
        value.symbol_species,
      ))
      case s {
        // Step 5: undefined/null species → the intrinsic default.
        value.JsUndefined | value.JsNull -> Ok(#(default_ctor, state))
        // Steps 6-7: anything else must be a constructor.
        _ ->
          case s, is_constructor(state.heap, s) {
            JsObject(s_ref), True -> Ok(#(s_ref, state))
            _, _ ->
              Error(state.type_error_value(
                state,
                "species constructor is not a constructor",
              ))
          }
      }
    }
    // Step 3: a present but non-object "constructor" is a TypeError.
    _ ->
      Error(state.type_error_value(
        state,
        "constructor property is not an object",
      ))
  }
}

/// §10.1.13.2 GetPrototypeFromConstructor ( constructor, intrinsicDefaultProto )
///
///   1. Let proto be ? Get(constructor, "prototype").
///   2. If proto is not an Object, set proto to intrinsicDefaultProto.
///   3. Return proto.
///
/// The single implementation for the whole engine: `new Foo()`, `super()`,
/// `Reflect.construct`, the primitive-wrapper natives, and every native
/// constructor's NewTarget preamble route through here. Step 1 is a real
/// [[Get]], so a proxy newTarget's `get` trap and an accessor `prototype`
/// property both fire and may throw (test262
/// prototype-from-newtarget-abrupt.js) — hence the throw-propagating `Result`
/// rather than a total function. `own_data_prototype` short-circuits the
/// overwhelmingly common ordinary case with an observationally identical read.
pub fn get_prototype_from_constructor(
  state: State(host),
  new_target_ref: Ref,
  intrinsic_default: Ref,
) -> Result(#(Ref, State(host)), #(JsValue, State(host))) {
  case own_data_prototype(state.heap, new_target_ref) {
    Some(proto_ref) -> Ok(#(proto_ref, state))
    None -> {
      use #(proto_val, state) <- result.map(get_value(
        state,
        new_target_ref,
        Named("prototype"),
        JsObject(new_target_ref),
      ))
      case proto_val {
        JsObject(proto_ref) -> #(proto_ref, state)
        _ -> #(intrinsic_default, state)
      }
    }
  }
}

/// The own, non-proxy, data-property `prototype` of `ref`, when it holds an
/// object. Two callers, one meaning — "newTarget itself declares a plain
/// `prototype` object":
///
///   - `get_prototype_from_constructor` uses it as a fast path: for such an
///     object [[Get]] returns exactly this value with no trap, no getter call
///     and no prototype walk, so short-circuiting is unobservable.
///   - `exec/call.do_construct` uses it to decide whether to re-prototype a
///     native constructor's result for a subclassing newTarget: an accessor or
///     absent `prototype` means the native's own choice stands.
pub fn own_data_prototype(h: Heap(host), ref: Ref) -> Option(Ref) {
  case heap.read(h, ref) {
    // A proxy's `prototype` is whatever its `get` trap says — never a slot read.
    Some(ObjectSlot(kind: value.ProxyObject(..), ..)) -> None
    Some(ObjectSlot(properties:, ..)) ->
      case dict.get(properties, Named("prototype")) {
        Ok(DataProperty(value: JsObject(proto_ref), ..)) -> Some(proto_ref)
        _ -> None
      }
    _ -> None
  }
}

/// §10.1.13.2 GetPrototypeFromConstructor for a NewTarget that may be
/// `undefined` (a plain [[Call]] of a native constructor), in the CPS shape
/// builtins are written in.
pub fn proto_from_new_target(
  state: State(host),
  new_target: JsValue,
  intrinsic_proto: Ref,
  cont: fn(Ref, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case new_target {
    JsObject(nt_ref) -> {
      use proto_ref, state <- state.try_op(get_prototype_from_constructor(
        state,
        nt_ref,
        intrinsic_proto,
      ))
      cont(proto_ref, state)
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
  state: State(host),
  target_ref: Ref,
  source: JsValue,
  excluded_keys: set.Set(PropertyKey),
  excluded_syms: set.Set(SymbolId),
) -> Result(State(host), #(JsValue, State(host))) {
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
          // §7.3.25 CopyDataProperties step 4 iterates from.[[OwnPropertyKeys]]
          // — §10.1.11 order, so the target's fresh creation seqs follow it.
          let keys =
            value.ordered_property_pairs(properties)
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
  state: State(host),
  src_ref: Ref,
  target_ref: Ref,
  keys: List(PropertyKey),
  cont: fn(State(host)) -> Result(State(host), #(JsValue, State(host))),
) -> Result(State(host), #(JsValue, State(host))) {
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
  heap: Heap(host),
  target_ref: Ref,
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
  excluded_keys: set.Set(PropertyKey),
) -> Heap(host) {
  // `n` is either the resolved view length (view in bounds) or 0 (detached /
  // out of bounds, loop never runs), so it doubles as the element bound below.
  let n = typed_array_live_count(heap, buffer, elem_kind, byte_offset, length)
  use h, idx <- int.range(from: 0, to: n, with: heap)
  case set.contains(excluded_keys, Index(idx)) {
    True -> h
    False ->
      case typed_array_element(h, buffer, elem_kind, byte_offset, n, idx) {
        Some(v) -> define_own_property(h, target_ref, Index(idx), v)
        None -> h
      }
  }
}

/// §7.3.25 CopyDataProperties — String exotic index-key portion: one
/// single-char data property per code point (§10.4.3.5).
fn copy_string_element_range(
  heap: Heap(host),
  target_ref: Ref,
  s: String,
  excluded_keys: set.Set(PropertyKey),
) -> Heap(host) {
  use h, idx <- int.range(from: 0, to: js_string.length(s), with: heap)
  case set.contains(excluded_keys, Index(idx)) {
    True -> h
    False ->
      case js_string.char_at(s, idx) {
        Some(ch) -> define_own_property(h, target_ref, Index(idx), JsString(ch))
        None -> h
      }
  }
}

fn copy_element_range(
  heap: Heap(host),
  target_ref: Ref,
  elements: JsElements,
  end: Int,
  excluded_keys: set.Set(PropertyKey),
) -> Heap(host) {
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
  state: State(host),
  ref: Ref,
  key: SymbolId,
  receiver: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case heap.read(state.heap, ref) {
    // §10.5.8 Proxy [[Get]] — symbol-keyed.
    Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) ->
      proxy_get(state, slots, SymbolPropKey(key), receiver)
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
  state: State(host),
  ref: Ref,
  key: SymbolId,
  val: JsValue,
  receiver: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  case heap.read(state.heap, ref) {
    // §10.4.6.9 Module Namespace [[Set]]: always returns false (read-only),
    // including for symbol keys — never falls through to the receiver write.
    Some(ObjectSlot(kind: value.ModuleNamespace(..), ..)) -> Ok(#(state, False))
    // §10.5.9 Proxy [[Set]] — symbol-keyed.
    Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) ->
      proxy_set(state, slots, SymbolPropKey(key), val, receiver)
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
  state: State(host),
  receiver: JsValue,
  key: SymbolId,
  val: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  case receiver {
    JsObject(recv_ref) -> {
      // Proxy receiver: the GetOwnProperty/DefineOwnProperty pair must go
      // through the proxy's traps.
      use <- proxy_receiver_guard(state, recv_ref, SymbolPropKey(key), val)
      // §10.1.9.2 step 2.c: merging {[[Value]]: V} into the receiver's
      // existing descriptor only changes [[Value]] — attributes are
      // preserved. A non-writable or accessor existing property rejects.
      // New properties get CreateDataProperty defaults (all true).
      let existing = get_own_symbol_property(state.heap, recv_ref, key)
      case existing {
        Some(DataProperty(writable: False, ..)) -> Ok(#(state, False))
        Some(AccessorProperty(..)) -> Ok(#(state, False))
        Some(DataProperty(
          writable: True,
          enumerable:,
          configurable:,
          seq:,
          value: _,
        )) -> {
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
                seq:,
              ),
            )
          Ok(#(State(..state, heap: h), True))
        }
        None ->
          // §10.1.6.3 step 2.a: current is undefined and receiver is not
          // extensible → reject. Mirrors the string-keyed twin at
          // set_string_property.
          case slot_extensible(state.heap, recv_ref) {
            False -> Ok(#(state, False))
            True -> {
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
  heap: Heap(host),
  ref: Ref,
  key: SymbolId,
  prop: Property,
) -> Heap(host) {
  use slot <- heap.update(heap, ref)
  let assert ObjectSlot(symbol_properties:, ..) = slot
    as "define_symbol_property target is not an ObjectSlot"
  ObjectSlot(
    ..slot,
    symbol_properties: list.key_set(symbol_properties, key, prop),
  )
}

// Inspect — debugging/REPL representation lives in arc/vm/inspect. These
// shims keep the historical `object.inspect` / `object.format_error` API
// working for existing callers.

/// Produce a human-readable representation of a JS value (for REPL / console.log).
/// Read-only — does NOT call toString/valueOf or any JS code.
pub fn inspect(val: value.JsValue, heap: Heap(host)) -> String {
  inspect.inspect(val, heap)
}

/// Format a value for an uncaught-exception / unhandled-rejection report.
pub fn format_error(val: value.JsValue, heap: Heap(host)) -> String {
  inspect.format_error(val, heap)
}

/// **IsConstructor(argument)** — ES2024 §7.2.4. Returns True iff `value` is an
/// object with a [[Construct]] internal method. Single source of truth for
/// constructibility, shared by the `new` / construct path (exec/call) and
/// `Reflect.construct` (builtins/reflect).
pub fn is_constructor(heap: Heap(host), value: JsValue) -> Bool {
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

/// The JsValue form of an ObjectKey (a String or Symbol value) — used both as
/// the proxy-trap key argument and to re-materialize an already-converted
/// key without a second user-observable ToPropertyKey.
pub fn prop_key_value(pk: ObjectKey) -> JsValue {
  case pk {
    StringPropKey(display:, ..) -> JsString(display)
    SymbolPropKey(sym) -> value.JsSymbol(sym)
  }
}

/// [[Get]] keyed by a resolved ObjectKey — dispatches to the string- or
/// symbol-keyed [[Get]] so ToPropertyKey callers don't hand-split the key.
pub fn get_prop_value(
  state: State(host),
  ref: Ref,
  pk: ObjectKey,
  receiver: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case pk {
    StringPropKey(pkey:, ..) -> get_value(state, ref, pkey, receiver)
    SymbolPropKey(sym) -> get_symbol_value(state, ref, sym, receiver)
  }
}

/// [[Get]] on any receiver keyed by a resolved ObjectKey — the primitive-
/// receiver counterpart of `get_prop_value` (which needs a Ref). Used where a
/// [[OwnPropertyKeys]] result or ToPropertyKey output is fed straight back
/// into a [[Get]] on a value that may be a primitive.
pub fn get_keyed_value_of(
  state: State(host),
  receiver: JsValue,
  pk: ObjectKey,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case pk {
    StringPropKey(pkey:, ..) -> get_value_of(state, receiver, pkey)
    SymbolPropKey(sym) -> get_symbol_value_of(state, receiver, sym)
  }
}

/// [[Set]] keyed by a resolved ObjectKey — dispatches to the string- or
/// symbol-keyed [[Set]]. Returns the [[Set]] success flag.
pub fn set_prop_value(
  state: State(host),
  ref: Ref,
  pk: ObjectKey,
  val: JsValue,
  receiver: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  case pk {
    StringPropKey(pkey:, ..) -> set_value(state, ref, pkey, val, receiver)
    SymbolPropKey(sym) -> set_symbol_value(state, ref, sym, val, receiver)
  }
}

/// Human-readable key for invariant-violation error messages.
fn pk_label(pk: ObjectKey) -> String {
  case pk {
    StringPropKey(display:, ..) -> "'" <> display <> "'"
    SymbolPropKey(_) -> "[symbol]"
  }
}

/// §7.2.3 IsCallable. Delegates to `heap.ref_is_callable` — the single source
/// of truth for the callable-ObjectKind set.
pub fn value_is_callable(h: Heap(host), val: JsValue) -> Bool {
  case val {
    JsObject(ref) -> heap.ref_is_callable(h, ref)
    _ -> False
  }
}

/// **`? IsArray ( argument )`** — ES2024 §7.2.2, with step 3.a's abrupt
/// completion already applied. This is the ONLY entry point: `IsArray` on a
/// revoked proxy throws, and every spec algorithm that says `? IsArray(x)`
/// must propagate that throw. Owning the message here means the seven call
/// sites can no longer disagree about it (or forget it).
pub fn try_is_array(
  state: State(host),
  val: JsValue,
) -> Result(#(Bool, State(host)), #(JsValue, State(host))) {
  case is_array(state.heap, val) {
    Ok(b) -> Ok(#(b, state))
    // Step 3.a: If proxy.[[ProxyHandler]] is null, throw a TypeError.
    Error(Nil) ->
      Error(state.type_error_value(
        state,
        "Cannot perform 'IsArray' on a proxy that has been revoked",
      ))
  }
}

/// §7.2.2 IsArray ( argument ). Pierces proxies to their target (step 3);
/// Error(Nil) signals a revoked proxy was encountered. Private on purpose —
/// callers go through `try_is_array`, which owns the TypeError.
fn is_array(h: Heap(host), val: JsValue) -> Result(Bool, Nil) {
  case val {
    // Steps 2-4 for objects.
    JsObject(ref) -> is_array_ref(h, ref)
    // Step 1: If argument is not an Object, return false.
    _ -> Ok(False)
  }
}

/// IsArray (§7.2.2) on an object ref — step 2 (Array exotic object) and
/// step 3 (Proxy: validate non-revoked, then recurse on [[ProxyTarget]]).
fn is_array_ref(h: Heap(host), ref: Ref) -> Result(Bool, Nil) {
  case heap.read(h, ref) {
    // Step 2: If argument is an Array exotic object, return true.
    Some(ObjectSlot(kind: ArrayObject(_), ..)) -> Ok(True)
    // Step 3: Proxy exotic object.
    Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) ->
      case slots {
        // Step 3.b-c: Return ? IsArray(proxy.[[ProxyTarget]]).
        Some(value.ProxySlots(target:, ..)) -> is_array_ref(h, target)
        // Step 3.a: If proxy.[[ProxyHandler]] is null, throw TypeError.
        None -> Error(Nil)
      }
    // Step 4: Return false.
    _ -> Ok(False)
  }
}

/// Read a ref's ProxyObject slots, or None when it isn't a proxy. The inner
/// Option is the revocation state (None = revoked).
pub fn as_proxy(h: Heap(host), ref: Ref) -> Option(Option(value.ProxySlots)) {
  case heap.read(h, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) -> Some(slots)
    _ -> None
  }
}

/// §10.5.14 ValidateNonRevokedProxy + §7.3.10 GetMethod(handler, name).
/// Returns the live target/handler refs and the trap function (None when the
/// handler doesn't define it). TypeError on revoked proxy or non-callable trap.
pub fn proxy_trap(
  state: State(host),
  slots: Option(value.ProxySlots),
  name: String,
) -> Result(#(Ref, Ref, Option(JsValue), State(host)), #(JsValue, State(host))) {
  case slots {
    Some(value.ProxySlots(target: t, handler: h)) -> {
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
              Error(state.type_error_value(
                state,
                "'" <> name <> "' trap of proxy handler is not a function",
              ))
          }
      }
    }
    None ->
      Error(state.type_error_value(
        state,
        "Cannot perform '" <> name <> "' on a proxy that has been revoked",
      ))
  }
}

/// `? target.[[GetOwnProperty]](P)` — the read every proxy invariant check
/// below is specified against, and the ONLY way to reach a target's own
/// descriptor from here. It is TRAP-AWARE: when the target is itself a proxy
/// this fires the target's `getOwnPropertyDescriptor` trap (§10.5.5) instead
/// of reading past it, so the invariants hold across nested proxies and the
/// trap's observable side effects happen. Routed through the ctx hook because
/// §10.5.5 needs descriptor parsing, which lives above this module.
fn target_own_property(
  state: State(host),
  t: Ref,
  pk: ObjectKey,
) -> Result(#(Option(Property), State(host)), #(JsValue, State(host))) {
  state.get_own_property(state, t, pk)
}

/// §10.5.8 Proxy [[Get]] ( P, Receiver ).
pub fn proxy_get(
  state: State(host),
  slots: Option(value.ProxySlots),
  pk: ObjectKey,
  receiver: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  // Steps 1-5: revocation check + GetMethod(handler, "get").
  use #(t, h, trap, state) <- result.try(proxy_trap(state, slots, "get"))
  case trap {
    // Step 6: trap undefined → target.[[Get]](P, Receiver).
    None -> get_prop_value(state, t, pk, receiver)
    Some(trap_fn) -> {
      // Step 7: Call(trap, handler, « target, P, Receiver »).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [
          JsObject(t),
          prop_key_value(pk),
          receiver,
        ]),
      )
      // Steps 8-10: invariants against `? target.[[GetOwnProperty]](P)`.
      use #(target_desc, state) <- result.try(target_own_property(state, t, pk))
      case target_desc {
        Some(DataProperty(value: tv, writable: False, configurable: False, ..)) ->
          case value.same_value(res, tv) {
            True -> Ok(#(res, state))
            False ->
              Error(state.type_error_value(
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
              Error(state.type_error_value(
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
  state: State(host),
  slots: Option(value.ProxySlots),
  pk: ObjectKey,
  val: JsValue,
  receiver: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(proxy_trap(state, slots, "set"))
  case trap {
    // Step 6: trap undefined → target.[[Set]](P, V, Receiver).
    None -> set_prop_value(state, t, pk, val, receiver)
    Some(trap_fn) -> {
      // Step 7: ToBoolean(? Call(trap, handler, « target, P, V, Receiver »)).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [
          JsObject(t),
          prop_key_value(pk),
          val,
          receiver,
        ]),
      )
      case value.is_truthy(res) {
        // Step 8: trap returned false → [[Set]] fails.
        False -> Ok(#(state, False))
        True -> {
          // Steps 9-11: invariants against `? target.[[GetOwnProperty]](P)`.
          use #(target_desc, state) <- result.try(target_own_property(
            state,
            t,
            pk,
          ))
          case target_desc {
            Some(DataProperty(
              value: tv,
              writable: False,
              configurable: False,
              ..,
            )) ->
              case value.same_value(val, tv) {
                True -> Ok(#(state, True))
                False ->
                  Error(state.type_error_value(
                    state,
                    "'set' on proxy: trap returned truish for property "
                      <> pk_label(pk)
                      <> " which exists in the proxy target as a non-configurable and non-writable data property with a different value",
                  ))
              }
            Some(AccessorProperty(set: None, configurable: False, ..)) ->
              Error(state.type_error_value(
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
}

/// §10.1.7.1 OrdinaryHasProperty / §10.5.7 Proxy [[HasProperty]] — the
/// trap-aware [[HasProperty]] used by the `in` operator and Reflect.has.
/// Recurses through prototype chains so a proxy anywhere on the chain traps.
pub fn has_property_stateful(
  state: State(host),
  ref: Ref,
  pk: ObjectKey,
) -> Result(#(Bool, State(host)), #(JsValue, State(host))) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) ->
      proxy_has(state, slots, pk)
    Some(ObjectSlot(
      kind: value.ModuleNamespace(exports:),
      symbol_properties:,
      ..,
    )) ->
      case pk {
        StringPropKey(display:, ..) ->
          Ok(#(dict.has_key(exports, display), state))
        SymbolPropKey(sym) ->
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
        StringPropKey(pkey:, ..) -> value.is_private_name(pkey)
        SymbolPropKey(_) -> False
      }
      use <- bool.guard(is_private, Ok(#(False, state)))
      let own = case pk {
        StringPropKey(pkey:, ..) ->
          option.is_some(own_property_of_slot(
            state.heap,
            kind,
            properties,
            elements,
            pkey,
          ))
        SymbolPropKey(sym) -> result.is_ok(list.key_find(symbol_properties, sym))
      }
      case own {
        True -> Ok(#(True, state))
        False -> {
          // §10.4.5.2 TypedArray [[HasProperty]]: invalid canonical numeric
          // index → false, never the prototype chain (mirrors has_property).
          let ta_numeric = case pk {
            StringPropKey(pkey:, ..) -> typed_array_numeric_key(kind, pkey)
            SymbolPropKey(_) -> False
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
  state: State(host),
  slots: Option(value.ProxySlots),
  pk: ObjectKey,
) -> Result(#(Bool, State(host)), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(proxy_trap(state, slots, "has"))
  case trap {
    // Step 6: trap undefined → target.[[HasProperty]](P).
    None -> has_property_stateful(state, t, pk)
    Some(trap_fn) -> {
      // Step 7: ToBoolean(? Call(trap, handler, « target, P »)).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [
          JsObject(t),
          prop_key_value(pk),
        ]),
      )
      case value.is_truthy(res) {
        True -> Ok(#(True, state))
        False -> {
          // Steps 9-13: invariants when the trap reports the key as absent,
          // starting from `? target.[[GetOwnProperty]](P)`.
          use #(target_desc, state) <- result.try(target_own_property(
            state,
            t,
            pk,
          ))
          case target_desc {
            None -> Ok(#(False, state))
            Some(prop) ->
              case value.prop_configurable(prop) {
                False ->
                  Error(state.type_error_value(
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
                      Error(state.type_error_value(
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
}

/// Trap-aware [[Delete]] used by the `delete` operator and
/// Reflect.deleteProperty. Falls through to the pure delete for non-proxies.
pub fn delete_property_stateful(
  state: State(host),
  ref: Ref,
  pk: ObjectKey,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) ->
      proxy_delete(state, slots, pk)
    _ ->
      case pk {
        StringPropKey(pkey:, ..) -> {
          let #(h, ok) = delete_property(state.heap, ref, pkey)
          Ok(#(State(..state, heap: h), ok))
        }
        SymbolPropKey(sym) -> {
          let #(h, ok) = delete_symbol_property(state.heap, ref, sym)
          Ok(#(State(..state, heap: h), ok))
        }
      }
  }
}

/// §10.5.10 Proxy [[Delete]] ( P ).
pub fn proxy_delete(
  state: State(host),
  slots: Option(value.ProxySlots),
  pk: ObjectKey,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(proxy_trap(
    state,
    slots,
    "deleteProperty",
  ))
  case trap {
    // Step 6: trap undefined → target.[[Delete]](P).
    None -> delete_property_stateful(state, t, pk)
    Some(trap_fn) -> {
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [
          JsObject(t),
          prop_key_value(pk),
        ]),
      )
      case value.is_truthy(res) {
        False -> Ok(#(state, False))
        True -> {
          // Steps 8-13: invariants, starting from
          // `? target.[[GetOwnProperty]](P)`.
          use #(target_desc, state) <- result.try(target_own_property(
            state,
            t,
            pk,
          ))
          case target_desc {
            None -> Ok(#(state, True))
            Some(prop) ->
              case value.prop_configurable(prop) {
                False ->
                  Error(state.type_error_value(
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
                      Error(state.type_error_value(
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
}

/// Trap-aware [[GetPrototypeOf]] — returns JsObject(proto) or JsNull.
pub fn get_prototype_of_stateful(
  state: State(host),
  ref: Ref,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) ->
      proxy_get_prototype_of(state, slots)
    Some(ObjectSlot(prototype: Some(p), ..)) -> Ok(#(JsObject(p), state))
    _ -> Ok(#(value.JsNull, state))
  }
}

/// §10.5.1 Proxy [[GetPrototypeOf]] ( ).
fn proxy_get_prototype_of(
  state: State(host),
  slots: Option(value.ProxySlots),
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(proxy_trap(
    state,
    slots,
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
                  Error(state.type_error_value(
                    state,
                    "'getPrototypeOf' on proxy: proxy target is non-extensible but the trap did not return its actual prototype",
                  ))
              }
            }
          }
        }
        _ ->
          Error(state.type_error_value(
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
  state: State(host),
  slots: Option(value.ProxySlots),
  proto_val: JsValue,
) -> Result(#(State(host), Option(Ref), Bool), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(proxy_trap(
    state,
    slots,
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
                  Error(state.type_error_value(
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
  state: State(host),
  ref: Ref,
) -> Result(#(Bool, State(host)), #(JsValue, State(host))) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) -> {
      use #(t, h, trap, state) <- result.try(proxy_trap(
        state,
        slots,
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
              Error(state.type_error_value(
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
  state: State(host),
  ref: Ref,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) -> {
      use #(t, h, trap, state) <- result.try(proxy_trap(
        state,
        slots,
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
                  Error(state.type_error_value(
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
// TypedArray (Integer-Indexed exotic) element READS — ES2024 §10.4.5
//
// Only the read half (§10.4.5.15 IntegerIndexedElementGet, §10.4.5.13
// TypedArrayLength, §7.1.21 CanonicalNumericIndexString) lives here — the
// MOP arms above need it and it never runs user code. The write half
// (§10.4.5.16 IntegerIndexedElementSet and the element encoders), which
// must run the observable ToNumber/ToBigInt on the stored value, lives in
// ops/typed_array_elements.
// ============================================================================

/// Read a snapshot of the backing store of a non-detached ArrayBuffer slot.
/// None when the ref isn't an ArrayBuffer or the buffer is detached.
/// For shared (atomics-backed) buffers this copies the live bytes out of the
/// shared cells; for plain buffers it is the backing binary itself.
pub fn typed_array_buffer_data(h: Heap(host), buffer: Ref) -> Option(BitArray) {
  typed_array_elements.buffer_bytes(h, buffer)
}

/// §10.4.5.13 TypedArrayLength — current [[ArrayLength]] of a typed-array
/// view. The primitive itself lives in ops/typed_array_elements (a leaf w.r.t.
/// this module) so the write half can share it: this is the read half's name
/// for it, kept because most of the engine reaches TypedArrayLength through
/// the object MOP.
pub fn typed_array_view_length(
  h: Heap(host),
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> Int {
  typed_array_elements.view_length(h, buffer, elem_kind, byte_offset, length)
}

/// §10.4.5.15 IntegerIndexedElementGet — element at `idx`, or None when the
/// index is invalid (negative, >= length, or the buffer is detached).
pub fn typed_array_element(
  h: Heap(host),
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Int,
  idx: Int,
) -> Option(JsValue) {
  // Cheap pre-filter, redundant with `valid_integer_index` below but ahead of
  // it: reading the backing store of a SharedArrayBuffer copies the whole
  // buffer out of its atomics cells, so an out-of-range read (`ta[len]`, the
  // terminating step of a scan loop) must not pay for it.
  use <- bool.guard(idx < 0 || idx >= length, None)
  case typed_array_buffer_data(h, buffer) {
    None -> None
    Some(data) ->
      element_of_view(
        data,
        typed_array_elements.fixed_view(
          bit_array.byte_size(data),
          elem_kind,
          byte_offset,
          length,
        ),
        elem_kind,
        idx,
      )
  }
}

/// §10.4.5.15 IntegerIndexedElementGet against the CURRENT view length: the
/// declared `length` (None for a length-tracking view) is resolved through
/// TypedArrayLength first. This is what every MOP element read wants — the
/// declared length alone is stale the moment a resizable buffer changes size.
/// The buffer is read ONCE and both TypedArrayLength and IsValidIntegerIndex
/// answer against those bytes.
pub fn typed_array_element_live(
  h: Heap(host),
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
  idx: Int,
) -> Option(JsValue) {
  use <- bool.guard(idx < 0, None)
  case typed_array_buffer_data(h, buffer) {
    None -> None
    Some(data) ->
      element_of_view(
        data,
        typed_array_elements.resolve_view(
          bit_array.byte_size(data),
          elem_kind,
          byte_offset,
          length,
        ),
        elem_kind,
        idx,
      )
  }
}

/// The tail both element reads share: §10.4.5.14 IsValidIntegerIndex against
/// the CURRENT backing store — the SAME predicate the write half applies
/// (typed_array_elements owns it), so a read and a write can never disagree
/// about which indices exist. `view` bundles the byte size the length was
/// resolved against with the length itself, so the two cannot come from
/// different buffers, and the decoded byte range comes off the same view.
fn element_of_view(
  data: BitArray,
  view: typed_array_elements.ResolvedView,
  elem_kind: value.TypedArrayKind,
  idx: Int,
) -> Option(JsValue) {
  case typed_array_elements.valid_integer_index(view, idx) {
    True ->
      Some(decode_typed_element(
        data,
        typed_array_elements.view_element_offset(view, idx),
        elem_kind,
      ))
    False -> None
  }
}

/// Why a typed-array view failed its buffer witness. The cases are NOT
/// interchangeable — a detached buffer has no bytes at all, an out-of-bounds
/// view is a resizable buffer that shrank below the view, and a non-view is a
/// receiver that never had a buffer — so callers get a category, never a
/// pre-worded string they could accidentally reword. This is the ONE witness
/// error type in the engine: the %TypedArray% builtins raise these too.
pub type ViewWitnessError {
  /// The view's `ArrayBuffer` was detached (transferred, or `.transfer()`d).
  BufferDetached
  /// The buffer is live but no longer covers the view's byte range.
  OutOfBoundsView
  /// The receiver is not a TypedArray at all (RequireInternalSlot failed).
  NotAView
}

/// The ONE place a `ViewWitnessError` becomes prose.
pub fn view_witness_error_message(err: ViewWitnessError) -> String {
  case err {
    BufferDetached -> "Cannot perform operation on a detached ArrayBuffer"
    OutOfBoundsView -> "TypedArray is out of bounds"
    NotAView -> "Method invoked on an object that is not a TypedArray"
  }
}

/// Throw the `TypeError` a failed buffer witness demands. Both categories are
/// TypeErrors (§23.1.5.1), and this owns that decision along with the wording,
/// so no caller can pick a different error class for one of them.
pub fn throw_view_witness_error(
  state: State(host),
  err: ViewWitnessError,
) -> Result(a, state.StepExit(host)) {
  state.throw_type_error(state, view_witness_error_message(err))
}

/// §23.1.5.1 CreateArrayIterator buffer-witness check for typed-array
/// sources: each `.next()` re-validates the view against the CURRENT buffer
/// (MakeTypedArrayWithBufferWitnessRecord + IsTypedArrayOutOfBounds) and
/// fails on a detached buffer or an out-of-bounds view (a resizable buffer
/// that shrank below the view). Ok(length) otherwise.
pub fn typed_array_iter_length(
  h: Heap(host),
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> Result(Int, ViewWitnessError) {
  case typed_array_buffer_data(h, buffer) {
    None -> Error(BufferDetached)
    Some(data) -> {
      let view =
        typed_array_elements.resolve_view(
          bit_array.byte_size(data),
          elem_kind,
          byte_offset,
          length,
        )
      case typed_array_elements.view_in_bounds(view) {
        False -> Error(OutOfBoundsView)
        True -> Ok(typed_array_elements.view_len(view))
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
  h: Heap(host),
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Int,
) -> Int {
  case typed_array_buffer_data(h, buffer) {
    None -> 0
    Some(data) -> {
      let view =
        typed_array_elements.fixed_view(
          bit_array.byte_size(data),
          elem_kind,
          byte_offset,
          length,
        )
      case typed_array_elements.view_in_bounds(view) {
        True -> length
        False -> 0
      }
    }
  }
}

/// `typed_array_live_length` for a view whose declared `length` may still be
/// AUTO (None for a length-tracking view): TypedArrayLength and the bounds
/// check both answer against ONE read of the buffer. The number of indices the
/// view actually has right now — 0 when detached or out of bounds.
pub fn typed_array_live_count(
  h: Heap(host),
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> Int {
  typed_array_iter_length(h, buffer, elem_kind, byte_offset, length)
  |> result.unwrap(0)
}

/// Decode one element from the backing store (§25.1.2.10 GetValueFromBuffer).
fn decode_typed_element(
  data: BitArray,
  off: Int,
  elem_kind: value.TypedArrayKind,
) -> JsValue {
  // The kind -> codec mapping is typed_array_ffi.elem_of_kind's job; all this
  // adds is the wrapper the content type calls for (Number vs BigInt).
  case elem_kind {
    value.BigKind(k) ->
      value.JsBigInt(
        value.BigInt(ta_get_int(data, off, typed_array_ffi.bigint_elem(k))),
      )
    value.NumKind(_) ->
      case typed_array_ffi.elem_of_kind(elem_kind) {
        typed_array_ffi.Int(e) -> value.from_int(ta_get_int(data, off, e))
        typed_array_ffi.Float(e) -> JsNumber(ta_get_float(data, off, e))
      }
  }
}

/// §7.1.21 CanonicalNumericIndexString: "-0", or a string that round-trips
/// through ToNumber → ToString. Such keys on a TypedArray NEVER reach the
/// ordinary property table (§10.4.5).
pub fn is_canonical_numeric_string(s: String) -> Bool {
  // Fast reject on the first byte: a canonical numeric string is the ToString
  // of a Number, which always starts with a digit, '-' (negatives/-0/
  // -Infinity), 'I' (Infinity) or 'N' (NaN). Ordinary property names
  // ("length", "buffer", "constructor", method names, …) bail out here
  // without the expensive ToNumber → ToString round-trip. '+' and '.' are
  // kept conservatively even though ToString never emits them first.
  case s {
    "0" <> _
    | "1" <> _
    | "2" <> _
    | "3" <> _
    | "4" <> _
    | "5" <> _
    | "6" <> _
    | "7" <> _
    | "8" <> _
    | "9" <> _
    | "-" <> _
    | "+" <> _
    | "." <> _
    | "I" <> _
    | "N" <> _ -> is_canonical_numeric_string_slow(s)
    _ -> False
  }
}

fn is_canonical_numeric_string_slow(s: String) -> Bool {
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
