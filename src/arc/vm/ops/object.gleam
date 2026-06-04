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
    value.JsSymbol(_) ->
      // TODO(Deviation): Symbol.prototype is not yet a dedicated object — it's Object.prototype.
      // Once Symbol.prototype is properly set up with toString/valueOf/description,
      // change this to use the dedicated Symbol.prototype ref.
      get_value(state, state.builtins.object.prototype, key, val)
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
      get_symbol_value(state, state.builtins.object.prototype, sym, val)
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
        _, _ ->
          // Step 1: Let desc be ? O.[[GetOwnProperty]](P).
          case own_property_of_slot(kind, properties, elements, key) {
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
          case own_property_of_slot(kind, properties, elements, Index(idx)) {
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
      own_property_of_slot(kind, properties, elements, key)
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
  kind: state.ExoticKind,
  properties: dict.Dict(PropertyKey, Property),
  elements: JsElements,
  key: PropertyKey,
) -> Option(Property) {
  case kind {
    // --- Array exotic [[GetOwnProperty]] (§10.4.2) ---
    // Per spec this IS OrdinaryGetOwnProperty — arrays only override
    // [[DefineOwnProperty]]. Our elements/properties split is an internal
    // optimization: properties dict is authoritative (holds accessors set
    // via Object.defineProperty(arr, "0", {get:...})), elements is the
    // fast-path data-value cache. Check properties first.
    ArrayObject(length:) ->
      case key {
        // Virtual "length" property (§10.4.2.4 ArraySetLength)
        Named("length") ->
          Some(DataProperty(
            value: value.from_int(length),
            writable: True,
            enumerable: False,
            configurable: False,
          ))
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
        _, _ -> {
          // §10.1.9.1 step 1: Let ownDesc be ? O.[[GetOwnProperty]](P).
          let own = own_property_of_slot(kind, properties, elements, key)
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
    ObjectSlot(kind:, elements:, extensible:, ..) ->
      case kind {
        // --- §10.4.2.1 Array exotic [[DefineOwnProperty]] ---
        ArrayObject(length:) ->
          case key {
            // §10.4.2.1 step 1: If P is "length", return ArraySetLength(A, Desc).
            Named("length") -> array_set_length(h, ref, val, slot, length)
            // §10.4.2.1 step 2: If P is an array index (ToUint32 is valid index):
            Index(idx) ->
              // §10.4.2.1 step 2.h: If index >= oldLen and lengthDesc.[[Writable]]
              // is false, return false. (We approximate with extensible since
              // we don't yet track length's [[Writable]] separately.)
              case idx >= length && !extensible {
                True -> #(h, False)
                False -> {
                  // §10.4.2.1 steps 2.i-2.k: define element, then set length to
                  // index+1 when index >= oldLen.
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
            // §10.4.2.1 step 3: Not "length" and not array index —
            // OrdinaryDefineOwnProperty(A, P, Desc).
            Named(_) -> set_string_property(h, ref, key, val, slot)
          }
        // --- §10.4.4.2 Arguments exotic [[DefineOwnProperty]] ---
        // Spec calls OrdinaryDefineOwnProperty then syncs [[ParameterMap]];
        // our element-based storage is an internal optimization.
        value.ArgumentsObject(_) ->
          case key {
            Index(idx) ->
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
  case coerce_length(val) {
    // Step 5: Would be RangeError; we return False.
    None -> #(h, False)
    Some(new_length) -> {
      let assert ObjectSlot(elements:, ..) = slot
      // §10.4.2.4 steps 8-18: If shrinking, truncate elements >= newLen.
      let new_elements = case new_length < old_length {
        True -> truncate_elements(elements, new_length, old_length)
        False -> elements
      }
      // §10.4.2.4 step 16 sets "length" to newLen via OrdinaryDefineOwnProperty
      // (no [[ArrayLength]] internal slot in ES2015+); step 20 returns true.
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
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(properties:, ..) -> {
      let new_props = dict.insert(properties, key, value.builtin_property(val))
      ObjectSlot(..slot, properties: new_props)
    }
    _ -> slot
  }
}

/// §10.1.6.3 step 6.c.i: merge a getter/setter into an existing accessor
/// descriptor, or build a fresh one if absent / a data property.
fn merge_accessor(
  existing: Result(Property, Nil),
  func: JsValue,
  kind: opcode.AccessorKind,
) -> Property {
  let #(get, set) = case existing {
    Ok(AccessorProperty(get:, set:, ..)) -> #(get, set)
    Ok(DataProperty(..)) | Error(Nil) -> #(None, None)
  }
  case kind {
    opcode.Getter ->
      AccessorProperty(
        get: Some(func),
        set:,
        enumerable: True,
        configurable: True,
      )
    opcode.Setter ->
      AccessorProperty(
        get:,
        set: Some(func),
        enumerable: True,
        configurable: True,
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
  heap: Heap,
  ref: Ref,
  key: PropertyKey,
  func: JsValue,
  kind: opcode.AccessorKind,
) -> Heap {
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(properties:, ..) -> {
      let new_prop = merge_accessor(dict.get(properties, key), func, kind)
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
) -> Heap {
  use slot <- heap.update(heap, ref)
  case slot {
    ObjectSlot(symbol_properties:, ..) -> {
      let new_prop =
        merge_accessor(list.key_find(symbol_properties, sym), func, kind)
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
  case heap.read(heap, ref) {
    // §10.4.6.6 Module Namespace [[HasProperty]]: true iff the key is an
    // exported name. Null prototype → no inheritance.
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
      dict.has_key(exports, value.key_to_string(key))
    Some(ObjectSlot(kind:, properties:, elements:, prototype:, ..)) ->
      // Step 1-2: Let hasOwn be O.[[GetOwnProperty]](P). If not undefined, return true.
      case own_property_of_slot(kind, properties, elements, key) {
        Some(_) -> True
        // Step 3-4: Let parent be O.[[GetPrototypeOf]](). If not null, recurse.
        None ->
          case prototype {
            Some(proto_ref) -> has_property(heap, proto_ref, key)
            // Step 5: Return false (null prototype).
            None -> False
          }
      }
    _ -> False
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
      case own_property_of_slot(kind, properties, elements, key) {
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
        // Array/Arguments exotic: check if key is an array index
        ArrayObject(_) | value.ArgumentsObject(_) ->
          case key {
            Index(idx) ->
              // Step 1-2: Check if element exists; if not, return true.
              case elements.has(elements, idx) {
                // Step 3: Element exists (implicitly configurable) — remove and return true.
                True -> #(
                  heap.write(
                    h,
                    ref,
                    ObjectSlot(..slot, elements: elements.delete(elements, idx)),
                  ),
                  True,
                )
                // Step 2: desc is undefined → return true.
                False -> #(h, True)
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
            _ -> #(acc, seen)
          }
          // Step 3: For each string key, check desc.[[Enumerable]].
          // Non-enumerable keys are added to seen (for shadowing) but not to results.
          let #(final_acc, final_seen) =
            dict.fold(properties, #(elem_acc, elem_seen), fn(state, key, prop) {
              let #(a, s) = state
              let k = value.key_to_string(key)
              case set.contains(s, k) {
                True -> #(a, s)
                False ->
                  case prop {
                    // Step 3.a.ii: desc.[[Enumerable]] is true → append key.
                    DataProperty(enumerable: True, ..) -> #(
                      [k, ..a],
                      set.insert(s, k),
                    )
                    // Non-enumerable or accessor: mark seen but don't include.
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
/// Used by object spread `{...source}` and Object.assign.
///
/// TODO(Deviation): symbol-keyed accessor getters are not invoked — the
/// descriptor is copied directly.
pub fn copy_data_properties(
  state: State,
  target_ref: Ref,
  source: JsValue,
) -> Result(State, #(JsValue, State)) {
  copy_data_properties_excluding(
    state,
    target_ref,
    source,
    set.new(),
    set.new(),
  )
}

/// §7.3.25 CopyDataProperties with non-empty excludedItems — used by
/// destructuring rest pattern `{a, b, ...rest}`. `excluded_keys` holds
/// string/index PropertyKeys already bound; `excluded_syms` holds SymbolIds.
/// Step 4.c: "If excludedItems does not contain nextKey..." — filter both
/// the element-range copy and the string/symbol property copies.
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
            _ -> state.heap
          }
          let state = State(..state, heap:)
          // Step 4 (string keys): Filter to enumerable + not-excluded, then
          // Get + CreateDataProperty.
          let keys =
            dict.to_list(properties)
            |> list.filter_map(fn(pair) {
              let #(k, prop) = pair
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
    value.JsString(s) -> "'" <> s <> "'"
    value.JsSymbol(id) ->
      value.well_known_symbol_description(id) |> option.unwrap("Symbol()")
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
          let name = case dict.get(properties, Named("name")) {
            Ok(DataProperty(value: JsString(n), ..)) -> n
            _ -> "anonymous"
          }
          "[Function: " <> name <> "]"
        }
        NativeFunction(..) -> {
          let name = case dict.get(properties, Named("name")) {
            Ok(DataProperty(value: JsString(n), ..)) -> n
            _ -> "native"
          }
          "[Function: " <> name <> "]"
        }
        PromiseObject(_) -> "Promise {}"
        GeneratorObject(_) -> "Object [Generator] {}"
        value.AsyncGeneratorObject(_) -> "Object [AsyncGenerator] {}"
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
        value.SubjectObject(..) -> "Subject {}"
        value.SelectorObject(..) -> "Selector {}"
        value.TimerObject(..) -> "Timer {}"
        value.MapObject(entries:, ..) ->
          "Map(" <> int.to_string(dict.size(entries)) <> ")"
        value.SetObject(data:, ..) ->
          "Set(" <> int.to_string(dict.size(data)) <> ")"
        value.WeakMapObject(_) -> "WeakMap {}"
        value.WeakSetObject(_) -> "WeakSet {}"
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
        value.IteratorHelperObject(..) -> "[Iterator Helper]"
        value.WrapForValidIteratorObject(..) -> "[Iterator]"
        value.ModuleNamespace(exports:) ->
          "[Module: { "
          <> string.join(list.sort(dict.keys(exports), string.compare), ", ")
          <> " }]"
        value.IteratorRecordObject(..) -> "[Iterator]"
        OrdinaryObject ->
          // Error instances render as "Name: message" (or the full stack, once
          // we capture one); everything else as a plain object.
          case error_display(heap, ref) {
            Some(s) -> s
            None -> inspect_plain_object(heap, properties, depth, visited)
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
/// Once errors carry a `stack` property, that becomes the rendering (it already
/// embeds the "Name: message" header, V8-style); until then we synthesize the
/// header from `name` and `message` per `Error.prototype.toString` (§20.5.3.4).
fn error_display(heap: Heap, ref: value.Ref) -> Option(String) {
  use <- bool.guard(!is_error(heap, ref), None)
  case error_property(heap, ref, "stack") {
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

/// Read-only test for whether `ref` is an Error instance: true iff some object
/// in its *prototype* chain owns a `message` property — the marker carried by
/// `Error.prototype`. Checking the prototype chain (not the instance's own
/// properties) correctly excludes plain objects like `{ message: "x" }`.
fn is_error(heap: Heap, ref: value.Ref) -> Bool {
  case heap.read(heap, ref) {
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
        _ -> False
      }
    _ -> False
  }
}
