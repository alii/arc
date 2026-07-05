//// §10.x Meta-Object Protocol core: [[DefineOwnProperty]], [[GetOwnProperty]],
//// [[OwnPropertyKeys]], [[SetPrototypeOf]] — the trap-aware internal methods
//// plus the ToPropertyDescriptor / FromPropertyDescriptor plumbing they share.
////
//// Split from `builtins/object` so that module carries ONLY the JS-visible
//// `Object.*` dispatch, and Reflect / the interpreter / other builtins can
//// depend on the MOP core without pulling in the whole Object-builtin surface.

import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/js_string
import arc/vm/key.{Index, Named, Private}
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/ops/property
import arc/vm/ops/typed_array_elements
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsElements, type JsValue, type ObjectKey, type Ref, AccessorProperty,
  ArrayObject, DataProperty, JsBool, JsNull, JsObject, JsString, JsSymbol,
  JsUndefined, ObjectSlot, StringPropKey, SymbolPropKey,
}
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

/// Thrown when a Module Namespace operation touches a still-uninitialized
/// (TDZ) export binding (§10.4.6 [[Get]] / [[GetOwnProperty]]).
const tdz_message = "Cannot access module export before initialization"

/// [[GetOwnProperty]] dispatching on a resolved ObjectKey.
fn get_own_property_by_key(
  heap: Heap(host),
  ref: Ref,
  key: ObjectKey,
) -> Option(value.Property) {
  case key {
    SymbolPropKey(sym) -> object.get_own_symbol_property(heap, ref, sym)
    // Private elements (see key.private_key) are stored in the ordinary
    // property table but are invisible to reflection (spec keeps them in
    // [[PrivateElements]]).
    StringPropKey(pkey:, ..) ->
      case key.is_private_key(pkey) {
        True -> None
        False -> object.get_own_property(heap, ref, pkey)
      }
  }
}

/// §10.4.3.5 StringGetOwnProperty ( S, P ) plus the "length" own data
/// property created by §10.4.3.4 StringCreate — the synthesized own-property
/// table of a String primitive receiver, keyed by an ALREADY-RESOLVED
/// property key.
///
/// This is the single source of truth for which keys are own properties of a
/// String primitive: getOwnPropertyDescriptor(s), hasOwnProperty,
/// Object.hasOwn, propertyIsEnumerable and (via `js_string.length`, the
/// same length source) the [[OwnPropertyKeys]] listing in own_keys_impl all
/// derive from it, so they agree by construction. String *wrapper objects*
/// take the mirror-image path in ops/object.own_property_of_slot.
///
/// Matching on the resolved `ObjectKey` (not the display string) is
/// load-bearing: ToPropertyKey already canonicalized array indices, so
/// "01", "+1" or " 1" arrive as `Named(..)` and correctly report no own
/// property — re-parsing the display string with int.parse would fabricate
/// index properties for them.
pub fn string_exotic_own_property(
  s: String,
  key: ObjectKey,
) -> Option(value.Property) {
  // seq: 0 on both arms — synthesized descriptors, only rendered by
  // make_descriptor_object, never stored in a property table.
  case key {
    // §10.4.3.4 StringCreate step 10: "length" is { W:F, E:F, C:F }.
    StringPropKey(pkey: Named("length"), ..) ->
      Some(DataProperty(
        value: value.from_int(js_string.length(s)),
        writable: False,
        enumerable: False,
        configurable: False,
        seq: 0,
      ))
    // §10.4.3.5 steps 5-10: an in-range integer index yields
    // { value: <element>, W:F, E:T, C:F }; out of range → undefined.
    StringPropKey(pkey: Index(i), ..) -> {
      use ch <- option.map(js_string.char_at(s, i))
      DataProperty(
        value: JsString(ch),
        writable: False,
        enumerable: True,
        configurable: False,
        seq: 0,
      )
    }
    // Any other string name, and every symbol, is not an own property of a
    // String primitive. (Private keys never come out of ToPropertyKey.)
    StringPropKey(pkey: Named(_), ..)
    | StringPropKey(pkey: Private(_), ..)
    | SymbolPropKey(_) -> None
  }
}

/// FromPropertyDescriptor ( Desc )
/// ES2024 §6.2.6.4
///
/// Converts an internal Property Descriptor to a plain object.
/// 1. If Desc is undefined, return undefined.  (handled by caller)
/// 2. Let obj be OrdinaryObjectCreate(%Object.prototype%).
/// 3. Assert: obj is extensible with no own properties.
/// 4. If Desc has a [[Value]] field, create "value" with Desc.[[Value]].
/// 5. If Desc has a [[Writable]] field, create "writable" with Desc.[[Writable]].
/// 6. If Desc has a [[Get]] field, create "get" with Desc.[[Get]].
/// 7. If Desc has a [[Set]] field, create "set" with Desc.[[Set]].
/// 8. If Desc has an [[Enumerable]] field, create "enumerable" with Desc.[[Enumerable]].
/// 9. If Desc has a [[Configurable]] field, create "configurable" with Desc.[[Configurable]].
/// 10. Return obj.
///
/// All created properties are {[[Writable]]: true, [[Enumerable]]: true,
/// [[Configurable]]: true} per spec. We use value.data_property which
/// defaults to writable=true, enumerable=true, configurable=true.
pub fn make_descriptor_object(
  heap: Heap(host),
  prop: value.Property,
  object_proto: Ref,
) -> #(Heap(host), Ref) {
  case prop {
    // Step 3: IsDataDescriptor — create "value" and "writable" properties.
    DataProperty(value: val, writable:, enumerable:, configurable:, ..) ->
      common.alloc_pojo(heap, object_proto, [
        // Step 3a: "value"
        #("value", value.data_property(val)),
        // Step 3b: "writable"
        #("writable", value.data_property(JsBool(writable))),
        // Step 5: "enumerable"
        #("enumerable", value.data_property(JsBool(enumerable))),
        // Step 6: "configurable"
        #("configurable", value.data_property(JsBool(configurable))),
      ])
    // Step 4: IsAccessorDescriptor — create "get" and "set" properties.
    AccessorProperty(get:, set:, enumerable:, configurable:, ..) -> {
      // Spec: Desc.[[Get]] / Desc.[[Set]] are stored as-is. If absent
      // internally (None), we emit undefined per convention.
      let get_val = option.unwrap(get, JsUndefined)
      let set_val = option.unwrap(set, JsUndefined)
      common.alloc_pojo(heap, object_proto, [
        // Step 4a: "get"
        #("get", value.data_property(get_val)),
        // Step 4b: "set"
        #("set", value.data_property(set_val)),
        // Step 5: "enumerable"
        #("enumerable", value.data_property(JsBool(enumerable))),
        // Step 6: "configurable"
        #("configurable", value.data_property(JsBool(configurable))),
      ])
    }
  }
}

/// ToPropertyDescriptor ( Obj ) + DefinePropertyOrThrow combined.
/// ES2024 §6.2.6.5 (ToPropertyDescriptor) + §7.3.8 (DefinePropertyOrThrow)
///
/// ToPropertyDescriptor steps:
///   1. If Type(Obj) is not Object, throw TypeError. (in parse_descriptor,
///      i.e. AFTER this function's ToPropertyKey step — spec order.)
///   2. Let desc be a new empty Property Descriptor.
///   3. If Obj has "enumerable", set desc.[[Enumerable]] = ToBoolean(Get(Obj, "enumerable")).
///   4. If Obj has "configurable", set desc.[[Configurable]] = ToBoolean(Get(Obj, "configurable")).
///   5. If Obj has "value", set desc.[[Value]] = Get(Obj, "value").
///   6. If Obj has "writable", set desc.[[Writable]] = ToBoolean(Get(Obj, "writable")).
///   7. If Obj has "get", let getter = Get(Obj, "get"). If not callable and not undefined, throw TypeError. Set desc.[[Get]].
///   8. If Obj has "set", let setter = Get(Obj, "set"). If not callable and not undefined, throw TypeError. Set desc.[[Set]].
///   9. If desc has [[Get]] or [[Set]], and desc has [[Value]] or [[Writable]], throw TypeError.
///  10. Return desc.
///
/// TODO(Deviation): Does not fully check non-configurable constraints
/// (e.g. redefining a non-configurable property's attributes should throw
/// in some cases per ValidateAndApplyPropertyDescriptor).
pub fn apply_descriptor(
  state: State(host),
  target_ref: Ref,
  key_val: JsValue,
  desc_val: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  // ToPropertyKey (which may run user code) strictly precedes
  // ToPropertyDescriptor's "not an object" rejection.
  use #(dkey, state) <- result.try(property.to_prop_key(state, key_val))
  use #(parsed, state) <- result.try(parse_descriptor(state, desc_val))
  apply_parsed_or_throw(state, target_ref, dkey, parsed)
}

/// DefinePropertyOrThrow (§7.3.8) on an already-parsed descriptor: a false
/// [[DefineOwnProperty]] result and a genuine abrupt completion both surface
/// as a throw here.
pub fn apply_parsed_or_throw(
  state: State(host),
  target_ref: Ref,
  dkey: ObjectKey,
  parsed: ParsedDesc,
) -> Result(State(host), #(JsValue, State(host))) {
  define_parsed(state, target_ref, dkey, parsed)
  |> result.map_error(as_define_throw)
}

/// [[DefineOwnProperty]] dispatch for an already-parsed descriptor.
/// Arrays get the exotic algorithm (§10.4.2.1) which special-cases "length"
/// (ArraySetLength §10.4.2.4) and array indices; proxies trap (§10.5.6);
/// everything else is OrdinaryDefineOwnProperty (§10.1.6.1).
pub fn define_parsed(
  state: State(host),
  target_ref: Ref,
  dkey: ObjectKey,
  parsed: ParsedDesc,
) -> Result(State(host), #(DefineFailure, State(host))) {
  case heap.read(state.heap, target_ref) {
    Some(ObjectSlot(kind: ArrayObject(length:), ..)) ->
      case dkey {
        StringPropKey(pkey: Named("length"), ..) ->
          array_define_length(state, target_ref, parsed, length)
        // An array index is an integer in [0, 2^32-1) — "4294967295" is a
        // plain property name on arrays, not an index (§6.1.7).
        StringPropKey(pkey: Index(idx), ..) if idx <= key.max_array_index ->
          array_define_index(state, target_ref, idx, parsed, length)
          |> result.map_error(as_rejected)
        _ ->
          ordinary_define(state, target_ref, dkey, parsed)
          |> result.map_error(as_rejected)
      }
    // §10.4.5.3 TypedArray (Integer-Indexed) [[DefineOwnProperty]]: canonical
    // numeric index keys never reach the ordinary property table — they
    // validate against the fixed {W:T, E:T, C:T} element descriptor and
    // store through IntegerIndexedElementSet. Everything else is ordinary.
    Some(ObjectSlot(
      kind: value.TypedArrayObject(buffer:, elem_kind:, byte_offset:, length:),
      ..,
    )) ->
      case dkey {
        StringPropKey(pkey: Index(idx), ..) ->
          typed_array_define_index(
            state,
            buffer,
            elem_kind,
            byte_offset,
            length,
            idx,
            parsed,
          )
        StringPropKey(pkey: Named(s), ..) ->
          case object.is_canonical_numeric_string(s) {
            // Step 1.b.i: a canonical numeric string that survived
            // canonical_key is never a valid integer index ("1.5", "-0",
            // "NaN", "-1", "1e+21", …) → false, with NO value conversion.
            True ->
              throw_type_error(state, "Invalid typed array index")
              |> result.map(fn(_) { state })
              |> result.map_error(as_rejected)
            False ->
              ordinary_define(state, target_ref, dkey, parsed)
              |> result.map_error(as_rejected)
          }
        StringPropKey(pkey: Private(_), ..) | SymbolPropKey(_) ->
          ordinary_define(state, target_ref, dkey, parsed)
          |> result.map_error(as_rejected)
      }
    // String exotic [[DefineOwnProperty]] (§10.4.3.2): the synthesized
    // "length" and in-range index properties are non-writable and
    // non-configurable — validate against their fixed descriptors instead of
    // writing to the dict.
    Some(ObjectSlot(kind: value.StringObject(value: s), extensible:, ..)) -> {
      let synthesized = case dkey {
        StringPropKey(pkey: Named("length"), ..) -> True
        StringPropKey(pkey: Index(i), ..) -> i >= 0 && i < js_string.length(s)
        _ -> False
      }
      case synthesized {
        True -> {
          let cur = get_own_property_by_key(state.heap, target_ref, dkey)
          case is_compatible_descriptor(extensible, parsed, cur) {
            // Compatible with a frozen descriptor = a no-op redefinition.
            True -> Ok(state)
            False ->
              throw_type_error(
                state,
                "Cannot redefine property: " <> key_quoted(dkey),
              )
              |> result.map(fn(_) { state })
              |> result.map_error(as_rejected)
          }
        }
        False ->
          ordinary_define(state, target_ref, dkey, parsed)
          |> result.map_error(as_rejected)
      }
    }
    // §10.4.6.6 Module Namespace [[DefineOwnProperty]] ( P, Desc ): string
    // keys never define anything — return true only when Desc is compatible
    // with the existing export binding's fixed descriptor (writable,
    // enumerable, non-configurable data property), false otherwise. The
    // throwing wrapper converts false to TypeError; define_property_bool
    // (Reflect.defineProperty) converts it back to false.
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
      case dkey {
        // Step 1: If P is a Symbol, return OrdinaryDefineOwnProperty(O, P, Desc).
        SymbolPropKey(_) ->
          ordinary_define(state, target_ref, dkey, parsed)
          |> result.map_error(as_rejected)
        StringPropKey(pkey: _, display: name) ->
          namespace_define(state, exports, name, parsed)
      }
    // §10.5.6 Proxy [[DefineOwnProperty]] — the throwing wrapper
    // (DefinePropertyOrThrow) converts a false trap result to TypeError.
    // Errors out of the proxy machinery (trap call throws, invariant
    // TypeErrors §10.5.6 steps 12-16) are genuine abrupt completions.
    Some(ObjectSlot(kind: value.ProxyObject(slots:, ..), ..)) -> {
      use #(state, ok) <- result.try(
        proxy_define_own_property(state, slots, dkey, parsed)
        |> result.map_error(as_thrown),
      )
      case ok {
        True -> Ok(state)
        False ->
          throw_type_error(
            state,
            "'defineProperty' on proxy: trap returned falsish for property "
              <> key_quoted(dkey),
          )
          |> result.map(fn(_) { state })
          |> result.map_error(as_rejected)
      }
    }
    _ ->
      ordinary_define(state, target_ref, dkey, parsed)
      |> result.map_error(as_rejected)
  }
}

/// §10.4.6.6 Module Namespace [[DefineOwnProperty]] steps 2-9 for a string
/// key. Every export is a { writable: true, enumerable: true,
/// configurable: false } data property whose value is the live binding;
/// a request is honoured (true) iff it changes nothing.
fn namespace_define(
  state: State(host),
  exports: dict.Dict(String, Ref),
  name: String,
  parsed: ParsedDesc,
) -> Result(State(host), #(DefineFailure, State(host))) {
  // Steps 2-3: current = O.[[GetOwnProperty]](P); undefined → false.
  use box <- result.try(case dict.get(exports, name) {
    Ok(box) -> Ok(box)
    Error(Nil) ->
      throw_type_error(
        state,
        "Cannot define property " <> name <> ", object is not extensible",
      )
      |> result.map_error(as_rejected)
  })
  // Step 4: Desc.[[Configurable]] present and true → false.
  // Step 5: Desc.[[Enumerable]] present and false → false.
  // Step 6: IsAccessorDescriptor(Desc) → false.
  // Step 7: Desc.[[Writable]] present and false → false.
  let incompatible =
    parsed.configurable == Some(True)
    || parsed.enumerable == Some(False)
    || desc_is_accessor(parsed)
    || parsed.writable == Some(False)
  use Nil <- result.try(case incompatible {
    True ->
      throw_type_error(state, "Cannot redefine property: " <> name)
      |> result.map_error(as_rejected)
    False -> Ok(Nil)
  })
  case parsed.value {
    // Step 9: no [[Value]] requested → true (nothing to change).
    None -> Ok(state)
    // Step 8: return SameValue(Desc.[[Value]], current.[[Value]]).
    Some(requested) -> {
      // [[GetOwnProperty]] reads the live binding; an uninitialized (TDZ)
      // binding throws ReferenceError (§10.4.6.5 performs [[Get]]).
      case heap.read_box(state.heap, box) {
        // A genuine abrupt completion, NOT a boolean-false define result:
        // Reflect.defineProperty(ns, ...) must throw this, not return false.
        Some(value.JsUninitialized) ->
          Error(
            as_thrown(state.reference_error_value(
              state,
              "Cannot access '" <> name <> "' before initialization",
            )),
          )
        current -> {
          let current_value = option.unwrap(current, JsUndefined)
          case value.same_value(requested, current_value) {
            True -> Ok(state)
            False ->
              throw_type_error(state, "Cannot redefine property: " <> name)
              |> result.map_error(as_rejected)
          }
        }
      }
    }
  }
}

/// Parsed ToPropertyDescriptor result (§6.2.6.5). `None` = field absent,
/// which matters for descriptor merging (absent fields are inherited).
pub type ParsedDesc {
  ParsedDesc(
    get: Option(JsValue),
    set: Option(JsValue),
    value: Option(JsValue),
    writable: Option(Bool),
    enumerable: Option(Bool),
    configurable: Option(Bool),
  )
}

/// IsAccessorDescriptor (§6.2.6.1).
fn desc_is_accessor(desc: ParsedDesc) -> Bool {
  option.is_some(desc.get) || option.is_some(desc.set)
}

/// IsDataDescriptor (§6.2.6.2).
fn desc_is_data(desc: ParsedDesc) -> Bool {
  option.is_some(desc.value) || option.is_some(desc.writable)
}

/// §10.4.2.1 Array exotic [[DefineOwnProperty]] — P is an array index.
fn array_define_index(
  state: State(host),
  target_ref: Ref,
  idx: Int,
  desc: ParsedDesc,
  old_len: Int,
) -> Result(State(host), #(JsValue, State(host))) {
  // Step 2.c: adding an index >= length requires length to be writable.
  let len_writable = array_length_writable(state.heap, target_ref)
  use Nil <- result.try(case idx >= old_len && !len_writable {
    True ->
      throw_type_error(
        state,
        "Cannot add property "
          <> int.to_string(idx)
          <> ", object is not extensible",
      )
    False -> Ok(Nil)
  })
  // Step 2.d: OrdinaryDefineOwnProperty(A, P, Desc) — throws on failure.
  use state <- result.try(ordinary_define(
    state,
    target_ref,
    StringPropKey(Index(idx), int.to_string(idx)),
    desc,
  ))
  // Steps 2.f-2.g: grow length to index + 1.
  case idx >= old_len {
    False -> Ok(state)
    True -> Ok(write_array_length(state, target_ref, idx + 1, False))
  }
}

/// §10.4.5.3 TypedArray [[DefineOwnProperty]] — P is a canonical integer
/// index. Steps 1.b.i-vii:
///   i.   invalid index (out of bounds / detached / shrunk) → false
///   ii.  [[Configurable]] present and false → false
///   iii. [[Enumerable]] present and false → false
///   iv.  accessor descriptor → false
///   v.   [[Writable]] present and false → false
///   vi.  [[Value]] present → ? IntegerIndexedElementSet (value conversion
///        may run user code and throw; a buffer detached DURING conversion
///        makes the store a silent no-op, still true). Immutable-buffer
///        views never reach the element store: the define succeeds only if
///        [[Value]] is SameValue to the current element and Desc asks for
///        no [[Writable]]/[[Configurable]] upgrade; otherwise it is rejected.
///   vii. true
/// The checks run BEFORE any value conversion — an invalid index must not
/// trigger observable ToNumber/ToBigInt side effects.
fn typed_array_define_index(
  state: State(host),
  buffer: Ref,
  elem_kind: value.TypedArrayKind,
  byte_offset: Int,
  length: option.Option(Int),
  idx: Int,
  desc: ParsedDesc,
) -> Result(State(host), #(DefineFailure, State(host))) {
  let current =
    object.typed_array_element(
      state.heap,
      buffer,
      elem_kind,
      byte_offset,
      object.typed_array_view_length(
        state.heap,
        buffer,
        elem_kind,
        byte_offset,
        length,
      ),
      idx,
    )
  let valid = option.is_some(current)
  let label = int.to_string(idx)
  let reject = fn(msg) {
    throw_type_error(state, msg)
    |> result.map(fn(_) { state })
    |> result.map_error(as_rejected)
  }
  use <- bool.lazy_guard(!valid, fn() {
    reject("Invalid typed array index: " <> label)
  })
  use <- bool.lazy_guard(desc.configurable == Some(False), fn() {
    reject("Cannot redefine property: " <> label)
  })
  use <- bool.lazy_guard(desc.enumerable == Some(False), fn() {
    reject("Cannot redefine property: " <> label)
  })
  use <- bool.lazy_guard(desc_is_accessor(desc), fn() {
    reject("Cannot redefine property: " <> label)
  })
  use <- bool.lazy_guard(desc.writable == Some(False), fn() {
    reject("Cannot redefine property: " <> label)
  })
  case desc.value {
    None -> Ok(state)
    Some(v) -> {
      use #(state, stored) <- result.try(
        typed_array_elements.typed_array_store(
          state,
          buffer,
          elem_kind,
          byte_offset,
          length,
          Some(idx),
          v,
        )
        |> result.map_error(as_thrown),
      )
      // Immutable ArrayBuffer proposal, [[DefineOwnProperty]]
      // (sec-typedarray-defineownproperty): an immutable-buffer-backed
      // element behaves as a {[[Writable]]: false, [[Enumerable]]: true,
      // [[Configurable]]: false} data property, so
      // ValidateAndApplyPropertyDescriptor returns true iff Desc.[[Value]]
      // is SameValue to the current element AND Desc asks for no attribute
      // upgrade (a true [[Writable]]/[[Configurable]] on a non-writable,
      // non-configurable current is always false); anything else is false.
      // The store refused (False) BEFORE any ToNumber/ToBigInt conversion,
      // so no user code ran and `current` (read at entry) is still live.
      case stored {
        True -> Ok(state)
        False -> {
          let unchanged =
            option.map(current, value.same_value(v, _))
            |> option.unwrap(False)
          let widened =
            desc.writable == Some(True) || desc.configurable == Some(True)
          case unchanged && !widened {
            True -> Ok(state)
            False -> reject("Cannot redefine property: " <> label)
          }
        }
      }
    }
  }
}

/// §10.4.2.4 ArraySetLength ( A, Desc ).
fn array_define_length(
  state: State(host),
  target_ref: Ref,
  desc: ParsedDesc,
  old_len: Int,
) -> Result(State(host), #(DefineFailure, State(host))) {
  let cur_writable = array_length_writable(state.heap, target_ref)
  case desc.value {
    // Step 1: [[Value]] absent → OrdinaryDefineOwnProperty(A, "length", Desc).
    None -> {
      use Nil <- result.try(
        validate_length_attrs(state, desc, cur_writable)
        |> result.map_error(as_rejected),
      )
      Ok(write_array_length(
        state,
        target_ref,
        old_len,
        desc.writable == Some(False),
      ))
    }
    Some(len_val) -> {
      // Steps 3-5: newLen = ToUint32(value); RangeError unless
      // SameValueZero(newLen, ToNumber(value)). Both the ToNumber call (user
      // valueOf/toString may throw) and the RangeError are genuine abrupt
      // completions — they propagate even through Reflect.defineProperty.
      use #(num, state) <- result.try(
        coerce.js_to_number(state, len_val) |> result.map_error(as_thrown),
      )
      use new_len <- result.try(
        parse_array_length(state, num) |> result.map_error(as_thrown),
      )
      // Steps 11/16: OrdinaryDefineOwnProperty validation against the current
      // length descriptor {[[Writable]]: cur, [[Enumerable]]: F, [[Configurable]]: F}.
      use Nil <- result.try(
        validate_length_attrs(state, desc, cur_writable)
        |> result.map_error(as_rejected),
      )
      // Steps 11-12: a non-writable length rejects any value change.
      use Nil <- result.try(
        case !cur_writable && new_len != old_len {
          True -> throw_type_error(state, "Cannot redefine property: length")
          False -> Ok(Nil)
        }
        |> result.map_error(as_rejected),
      )
      let freeze = desc.writable == Some(False)
      case new_len >= old_len {
        // Step 11: growing (or unchanged) — no deletions needed.
        True -> Ok(write_array_length(state, target_ref, new_len, freeze))
        // Steps 13-19: shrinking — delete indices >= newLen in descending
        // order, stopping at the first non-configurable one (step 17.b
        // returns false → a rejection, not a throw).
        False ->
          shrink_array(state, target_ref, new_len, freeze)
          |> result.map_error(as_rejected)
      }
    }
  }
}

/// ArraySetLength steps 3-5: validate a ToNumber result as a uint32 length.
/// NaN / ±Infinity: ToUint32 is 0, which ≠ the value, so step 5 throws.
fn parse_array_length(
  state: State(host),
  num: value.JsNum,
) -> Result(Int, #(JsValue, State(host))) {
  case num {
    value.Finite(f) ->
      case value.array_length(f) {
        Some(n) -> Ok(n)
        None -> throw_range_error(state, "Invalid array length")
      }
    _ -> throw_range_error(state, "Invalid array length")
  }
}

/// OrdinaryDefineOwnProperty attribute validation for the array "length"
/// property, whose current descriptor is always a data property with
/// [[Enumerable]]: false, [[Configurable]]: false (§10.4.2, §10.1.6.3).
fn validate_length_attrs(
  state: State(host),
  desc: ParsedDesc,
  cur_writable: Bool,
) -> Result(Nil, #(JsValue, State(host))) {
  // §10.1.6.3 step 4.a: cannot make a non-configurable property configurable.
  use Nil <- result.try(case desc.configurable {
    Some(True) -> throw_type_error(state, "Cannot redefine property: length")
    _ -> Ok(Nil)
  })
  // §10.1.6.3 step 4.b: cannot change [[Enumerable]] (currently false).
  use Nil <- result.try(case desc.enumerable {
    Some(True) -> throw_type_error(state, "Cannot redefine property: length")
    _ -> Ok(Nil)
  })
  // §10.1.6.3 step 6: cannot convert non-configurable data → accessor.
  use Nil <- result.try(case desc_is_accessor(desc) {
    True -> throw_type_error(state, "Cannot redefine property: length")
    False -> Ok(Nil)
  })
  // §10.1.6.3 step 7.a.i: cannot change [[Writable]] false → true.
  case desc.writable, cur_writable {
    Some(True), False ->
      throw_type_error(state, "Cannot redefine property: length")
    _, _ -> Ok(Nil)
  }
}

/// Is the array's "length" writable? True unless a defineProperty call froze
/// it (stored as a Named("length") dict override consulted by all paths).
fn array_length_writable(h: Heap(host), ref: Ref) -> Bool {
  case heap.read(h, ref) {
    Some(ObjectSlot(properties:, ..)) ->
      case dict.get(properties, Named("length")) {
        Ok(DataProperty(writable:, ..)) -> writable
        _ -> True
      }
    _ -> True
  }
}

/// Write the array's tracked length, optionally freezing [[Writable]] to
/// false (ArraySetLength steps 14/19). The frozen flag lives as a
/// Named("length") dict override; its value is kept in sync with the kind.
fn write_array_length(
  state: State(host),
  target_ref: Ref,
  new_len: Int,
  freeze: Bool,
) -> State(host) {
  case heap.read(state.heap, target_ref) {
    Some(ObjectSlot(properties:, ..) as slot) -> {
      // seq: 0 / seq passthrough — array "length" never enumerates through
      // the seq-ordered named-key path (emitted as a special case).
      let properties = case freeze, dict.get(properties, Named("length")) {
        True, _ ->
          dict.insert(
            properties,
            Named("length"),
            DataProperty(
              value: value.from_int(new_len),
              writable: False,
              enumerable: False,
              configurable: False,
              seq: 0,
            ),
          )
        False, Ok(DataProperty(writable:, enumerable:, configurable:, seq:, ..))
        ->
          dict.insert(
            properties,
            Named("length"),
            DataProperty(
              value: value.from_int(new_len),
              writable:,
              enumerable:,
              configurable:,
              seq:,
            ),
          )
        False, _ -> properties
      }
      let h =
        heap.write(
          state.heap,
          target_ref,
          ObjectSlot(..slot, kind: ArrayObject(new_len), properties:),
        )
      State(..state, heap: h)
    }
    _ -> state
  }
}

/// ArraySetLength steps 13-19: shrink the array, deleting indices >= new_len
/// in descending order. Plain elements are implicitly configurable; dict
/// overrides may be non-configurable — the largest such index stops the
/// truncation (length becomes index + 1) and the operation throws TypeError
/// (step 17.b, surfaced via DefinePropertyOrThrow).
fn shrink_array(
  state: State(host),
  target_ref: Ref,
  new_len: Int,
  freeze: Bool,
) -> Result(State(host), #(JsValue, State(host))) {
  case heap.read(state.heap, target_ref) {
    Some(ObjectSlot(properties:, elements:, ..) as slot) -> {
      // Largest non-configurable index >= new_len blocks deletion at it.
      let blocked =
        dict.fold(properties, None, fn(acc, k, prop) {
          let non_configurable = !value.prop_configurable(prop)
          case k {
            Index(i) if i >= new_len ->
              case non_configurable {
                True ->
                  case acc {
                    Some(m) -> Some(int.max(m, i))
                    None -> Some(i)
                  }
                False -> acc
              }
            _ -> acc
          }
        })
      let final_len = case blocked {
        Some(b) -> b + 1
        None -> new_len
      }
      let elements = elements.truncate(elements, final_len)
      let properties =
        dict.filter(properties, fn(k, _prop) {
          case k {
            Index(i) -> i < final_len
            Named(_) | Private(_) -> True
          }
        })
      let h =
        heap.write(
          state.heap,
          target_ref,
          ObjectSlot(..slot, properties:, elements:),
        )
      let state =
        write_array_length(
          State(..state, heap: h),
          target_ref,
          final_len,
          freeze,
        )
      case blocked {
        // Step 17.b.iv: a delete failed — length stays at idx+1, then throw.
        Some(_) -> throw_type_error(state, "Cannot redefine property: length")
        None -> Ok(state)
      }
    }
    _ -> Ok(state)
  }
}

/// OrdinaryDefineOwnProperty (§10.1.6.1) + ValidateAndApplyPropertyDescriptor
/// (§10.1.6.3). For Array/Arguments index keys the "current" property may
/// live in the fast elements store (an implicit {w:T, e:T, c:T} data
/// property); results with exactly those attributes go back to elements,
/// anything else becomes a dict override (dict is checked first everywhere).
fn ordinary_define(
  state: State(host),
  target_ref: Ref,
  dkey: ObjectKey,
  desc: ParsedDesc,
) -> Result(State(host), #(JsValue, State(host))) {
  let is_accessor = desc_is_accessor(desc)
  let has_data = desc_is_data(desc)
  let desc_get = desc.get
  let desc_set = desc.set
  let desc_value = desc.value
  let desc_writable = desc.writable
  let desc_enumerable = desc.enumerable
  let desc_configurable = desc.configurable
  case heap.read(state.heap, target_ref) {
    Some(ObjectSlot(
      kind:,
      properties:,
      symbol_properties:,
      elements:,
      prototype:,
      extensible:,
    )) -> {
      let indexed_kind = case kind {
        ArrayObject(_) | value.ArgumentsObject(_) -> True
        _ -> False
      }
      let existing = case dkey {
        StringPropKey(pkey: Index(idx), ..) if indexed_kind ->
          case dict.get(properties, Index(idx)) {
            Ok(p) -> Ok(p)
            Error(Nil) ->
              case elements.get_option(elements, idx) {
                Some(v) ->
                  // seq: 0 — synthesized from the elements store; Index keys
                  // enumerate numerically, never by seq.
                  Ok(DataProperty(
                    value: v,
                    writable: True,
                    enumerable: True,
                    configurable: True,
                    seq: 0,
                  ))
                None -> Error(Nil)
              }
          }
        StringPropKey(pkey:, ..) -> dict.get(properties, pkey)
        SymbolPropKey(sym:) -> list.key_find(symbol_properties, sym)
      }
      // V8's error text: the bare key, no quotes.
      let key = key_text(dkey)
      // §10.1.6.3 steps 2 + 5-11: is the change permitted at all? Exactly the
      // question `is_compatible_descriptor` (IsCompatiblePropertyDescriptor,
      // §10.1.6.2) answers — the same predicate the proxy and String-exotic
      // paths validate against, so all three agree on SameValue (notably that
      // -0 and +0 differ, which structural equality misses).
      use Nil <- result.try(case option.from_result(existing) {
        // Step 2: a new property needs an extensible object.
        None ->
          case extensible {
            True -> Ok(Nil)
            False ->
              throw_type_error(
                state,
                "Cannot define property " <> key <> ", object is not extensible",
              )
          }
        // Steps 5-11: a non-configurable property constrains the redefinition.
        Some(cur) ->
          case is_compatible_descriptor(extensible, desc, Some(cur)) {
            True -> Ok(Nil)
            False ->
              throw_type_error(state, "Cannot redefine property: " <> key)
          }
      })

      let enumerable =
        option.lazy_unwrap(desc_enumerable, fn() {
          existing |> result.map(value.prop_enumerable) |> result.unwrap(False)
        })
      let configurable =
        option.lazy_unwrap(desc_configurable, fn() {
          existing
          |> result.map(value.prop_configurable)
          |> result.unwrap(False)
        })
      // §10.1.11: redefining an existing key keeps its creation seq (and so
      // its enumeration position); a brand-new key gets a fresh one.
      let seq = case existing {
        Ok(old) -> value.prop_seq(old)
        Error(Nil) -> value.next_prop_seq()
      }
      let new_prop = case is_accessor, has_data {
        // Generic descriptor (§10.1.6.3): neither data nor accessor fields
        // present — keep the existing property's kind and fields, updating
        // only [[Enumerable]]/[[Configurable]]. A brand-new property created
        // from a generic descriptor is a data property with defaults.
        False, False ->
          case existing {
            Ok(DataProperty(value: v, writable: w, ..)) ->
              DataProperty(
                value: v,
                writable: w,
                enumerable:,
                configurable:,
                seq:,
              )
            Ok(AccessorProperty(get: g, set: s, ..)) ->
              AccessorProperty(get: g, set: s, enumerable:, configurable:, seq:)
            Error(Nil) ->
              DataProperty(
                value: JsUndefined,
                writable: False,
                enumerable:,
                configurable:,
                seq:,
              )
          }
        True, _ -> {
          // Accessor descriptor: merge get/set with existing accessor (if any).
          // Per §10.1.6.3 ValidateAndApplyPropertyDescriptor:
          // Fields not present in the new descriptor are inherited from the existing property.
          let getter = case desc_get {
            Some(JsUndefined) -> None
            None ->
              case existing {
                Ok(AccessorProperty(get: g, ..)) -> g
                _ -> None
              }
            Some(g) -> Some(g)
          }
          let setter = case desc_set {
            Some(JsUndefined) -> None
            None ->
              case existing {
                Ok(AccessorProperty(set: s, ..)) -> s
                _ -> None
              }
            Some(s) -> Some(s)
          }
          AccessorProperty(
            get: getter,
            set: setter,
            enumerable:,
            configurable:,
            seq:,
          )
        }
        False, True -> {
          // Data descriptor: merge value/writable with existing data property.
          // Per §10.1.6.3: absent fields inherit from existing property,
          // defaulting to undefined/false for new properties.
          let final_value = case desc_value {
            Some(v) -> v
            _ ->
              case existing {
                Ok(DataProperty(value: v, ..)) -> v
                _ -> JsUndefined
              }
          }
          let final_writable = case desc_writable {
            Some(w) -> w
            _ ->
              case existing {
                Ok(DataProperty(writable: w, ..)) -> w
                _ -> False
              }
          }
          DataProperty(
            value: final_value,
            writable: final_writable,
            enumerable:,
            configurable:,
            seq:,
          )
        }
      }

      // Write the new/updated property to the right store. Array/Arguments
      // index keys with default data attributes go to the fast elements
      // store; non-default attributes / accessors become dict overrides
      // (the element copy is removed so exactly one store owns the index).
      let #(properties, symbol_properties, elements) = case dkey {
        StringPropKey(pkey: Index(idx), ..) if indexed_kind ->
          case new_prop {
            DataProperty(
              value: v,
              writable: True,
              enumerable: True,
              configurable: True,
              ..,
            ) -> #(
              dict.delete(properties, Index(idx)),
              symbol_properties,
              elements.set(elements, idx, v),
            )
            _ -> #(
              dict.insert(properties, Index(idx), new_prop),
              symbol_properties,
              elements.delete(elements, idx),
            )
          }
        StringPropKey(pkey:, ..) -> #(
          dict.insert(properties, pkey, new_prop),
          symbol_properties,
          elements,
        )
        SymbolPropKey(sym:) -> #(
          properties,
          list.key_set(symbol_properties, sym, new_prop),
          elements,
        )
      }
      let h =
        heap.write(
          state.heap,
          target_ref,
          ObjectSlot(
            kind:,
            properties:,
            symbol_properties:,
            elements:,
            prototype:,
            extensible:,
          ),
        )
      Ok(State(..state, heap: h))
    }
    _ -> Ok(state)
  }
}

/// Failure channel for the [[DefineOwnProperty]] machinery (define_parsed and
/// friends), distinguishing the two ways a define can fail:
///
/// - `DefineRejected`: the define validated to boolean false (§10.1.6.3 /
///   §10.4.2.4 "return false"). DefinePropertyOrThrow throws the carried
///   TypeError; Reflect.defineProperty and the proxy no-trap path turn it
///   into `false`.
/// - `DefineThrew`: a genuine abrupt completion — ArraySetLength's RangeError
///   (§10.4.2.4 step 5), user code throwing during ToNumber of the length
///   value, or proxy trap/invariant errors. Always re-thrown, never `false`.
pub type DefineFailure {
  DefineRejected(JsValue)
  DefineThrew(JsValue)
}

/// The thrown value for contexts (DefinePropertyOrThrow) where both failure
/// kinds surface as a throw.
pub fn define_failure_value(failure: DefineFailure) -> JsValue {
  case failure {
    DefineRejected(err) -> err
    DefineThrew(err) -> err
  }
}

/// DefinePropertyOrThrow (§7.3.8) failure channel: a false
/// [[DefineOwnProperty]] result and a genuine abrupt completion both surface
/// as a throw. `result.map_error` adapter for `define_parsed`.
pub fn as_define_throw(
  err: #(DefineFailure, State(host)),
) -> #(JsValue, State(host)) {
  let #(failure, state) = err
  #(define_failure_value(failure), state)
}

/// Tag a raw thrown error as a validation rejection (boolean-false result).
fn as_rejected(err: #(JsValue, State(host))) -> #(DefineFailure, State(host)) {
  let #(thrown, state) = err
  #(DefineRejected(thrown), state)
}

/// Tag a raw thrown error as a genuine abrupt completion.
fn as_thrown(err: #(JsValue, State(host))) -> #(DefineFailure, State(host)) {
  let #(thrown, state) = err
  #(DefineThrew(thrown), state)
}

/// The one way this module raises a thrown TypeError: defineProperty
/// rejections, proxy invariant violations, CreateListFromArrayLike, typed-array
/// index rejection, Object.assign's failed Set, …
fn throw_type_error(
  state: State(host),
  msg: String,
) -> Result(a, #(JsValue, State(host))) {
  Error(state.type_error_value(state, msg))
}

/// The one way this module raises a thrown RangeError (ArraySetLength step 5,
/// §10.4.2.4).
fn throw_range_error(
  state: State(host),
  msg: String,
) -> Result(a, #(JsValue, State(host))) {
  Error(state.range_error_value(state, msg))
}

/// Helper for ToPropertyDescriptor: `if ? HasProperty(Obj, name) then
/// ? Get(Obj, name)` — §6.2.6.5 steps 3-8, in that order.
///
/// Returns #(Some(value), state) if the property exists, #(None, state) if absent.
/// The distinction matters because absent fields are not set in the descriptor,
/// while present-but-undefined fields ARE set (e.g. {value: undefined} is different
/// from {} — the former sets [[Value]] to undefined, the latter leaves it absent).
///
/// Both halves are trap-aware and can throw. The two are INTERLEAVED per field,
/// not batched: `Object.defineProperty(o, "k", new Proxy({}, {has(){…},
/// get(){…}}))` fires `has("enumerable")` and then, only if it claimed the
/// property, `get("enumerable")`, before `has("configurable")` is fired at all.
/// `parse_descriptor` owns the field order that makes that sequence normative.
fn read_desc_field(
  state: State(host),
  desc: JsValue,
  key: String,
) -> Result(#(option.Option(JsValue), State(host)), #(JsValue, State(host))) {
  case desc {
    JsObject(ref) -> {
      // Step 3/5/7/…: HasProperty(Obj, name).
      use #(present, state) <- result.try(object.has_property_stateful(
        state,
        ref,
        named_object_key(key),
      ))
      case present {
        False -> Ok(#(option.None, state))
        // Step 4/6/8/…: Get(Obj, name) — a getter or `get` trap runs here.
        True -> {
          use #(val, state) <- result.map(object.get_value_of(
            state,
            desc,
            Named(key),
          ))
          #(Some(val), state)
        }
      }
    }
    // ToPropertyDescriptor step 1 already rejected non-objects.
    _ -> Ok(#(option.None, state))
  }
}

/// Helper for ToPropertyDescriptor: reads a field and applies ToBoolean (§7.1.2).
/// Used for the "enumerable", "configurable", and "writable" fields.
///
/// Per §6.2.6.5 steps 4/6/10: the raw value is coerced via ToBoolean.
/// ToBoolean(x) returns false for: undefined, null, false, +0, -0, NaN, "".
/// Everything else (including objects, non-empty strings, non-zero numbers) is true.
fn read_desc_bool(
  state: State(host),
  desc: JsValue,
  key: String,
) -> Result(#(option.Option(Bool), State(host)), #(JsValue, State(host))) {
  use #(field, state) <- result.map(read_desc_field(state, desc, key))
  // Delegate to value.is_truthy for spec-correct ToBoolean — its numeric
  // check uses `!= 0.0` (Erlang `/=`) which correctly treats -0 as falsy,
  // unlike a `Finite(0.0)` pattern match (Erlang `=:=` distinguishes ±0).
  #(option.map(field, value.is_truthy), state)
}

/// Collect own property keys from an object — implements [[OwnPropertyKeys]]
/// (§10.1.11 for ordinary objects) with optional enumerable filtering.
///
/// [[OwnPropertyKeys]] ordering (§10.1.11):
///   1. For each own property key P of O that is an array index, in ascending
///      numeric index order, add P to keys.
///   2. For each own property key P of O that is a String but not an array index,
///      in ascending chronological order of property creation, add P to keys.
///   3. For each own property key P of O that is a Symbol, in ascending
///      chronological order of property creation, add P to keys.
///
/// When enumerable_only=True, this further implements the key-filtering from
/// EnumerableOwnProperties ( O, kind=key ) — §7.3.23:
///   3. For each element key of ownKeys, do
///      a. If key is a String, then
///         i. Let desc be ? O.[[GetOwnProperty]](key).
///         ii. If desc is not undefined and desc.[[Enumerable]] is true, then
///             — Append key to properties.
///
/// Symbol keys are not included here — they are stored separately in
/// symbol_properties (a creation-ordered list) and per spec step 3 ALL
/// symbol keys come after ALL string keys, so callers simply append them.
///
/// Ordering (including step-2 creation order for non-index string keys via
/// Property.seq) comes from object.own_string_keys_flagged — the single
/// engine-wide funnel for own string-key enumeration order.
pub fn collect_own_keys(
  heap: Heap(host),
  ref: Ref,
  enumerable_only: Bool,
) -> List(String) {
  let flagged = object.own_string_keys_flagged(heap, ref)
  case enumerable_only {
    True ->
      list.filter_map(flagged, fn(pair) {
        case pair {
          #(k, True) -> Ok(k)
          #(_, False) -> Error(Nil)
        }
      })
    False -> list.map(flagged, fn(pair) { pair.0 })
  }
}

/// Collect enumerable own symbol keys from an object.
/// Implements the symbol portion of OrdinaryOwnPropertyKeys (§10.1.11.1 step 4)
/// with optional enumerable filtering.
pub fn collect_own_symbol_keys(
  heap: Heap(host),
  ref: Ref,
  enumerable_only: Bool,
) -> List(value.SymbolId) {
  case heap.read(heap, ref) {
    Some(ObjectSlot(symbol_properties:, ..)) ->
      symbol_properties
      |> list.filter_map(fn(pair) {
        let #(sym, prop) = pair
        case enumerable_only {
          True ->
            case prop {
              DataProperty(enumerable: True, ..)
              | AccessorProperty(enumerable: True, ..) -> Ok(sym)
              _ -> Error(Nil)
            }
          False -> Ok(sym)
        }
      })
    _ -> []
  }
}

/// Why an object's [[SetPrototypeOf]] returned **false**. Callers that must
/// throw (Object.setPrototypeOf) turn each variant into its own TypeError;
/// callers that report a flag (Reflect.setPrototypeOf) collapse them all.
pub type SetProtoFail {
  NotExtensible
  Cyclic
  Immutable
  /// A proxy's `setPrototypeOf` trap returned falsish (§10.5.2 step 8).
  TrapRefused
}

/// Trap-aware **[[SetPrototypeOf]]** ( V ) — §10.5.2 for proxies, §10.1.2
/// OrdinarySetPrototypeOf otherwise. THE single dispatch: `Object
/// .setPrototypeOf`, `Reflect.setPrototypeOf` and `__proto__`'s setter all
/// route through it, so a proxy can never be handed to the ordinary
/// algorithm (which would rewrite the *proxy's* internal prototype slot and
/// never fire the trap).
pub fn set_prototype_of_stateful(
  state: State(host),
  ref: Ref,
  new_proto: Option(Ref),
) -> Result(#(State(host), Result(Nil, SetProtoFail)), #(JsValue, State(host))) {
  case object.as_proxy(state.heap, ref) {
    Some(slots) -> {
      use #(state, ok) <- result.map(proxy_set_proto(state, slots, new_proto))
      case ok {
        True -> #(state, Ok(Nil))
        False -> #(state, Error(TrapRefused))
      }
    }
    None ->
      case ordinary_set_prototype_of(state, ref, new_proto) {
        Ok(state) -> Ok(#(state, Ok(Nil)))
        Error(fail) -> Ok(#(state, Error(fail)))
      }
  }
}

/// The TypeError message Object.setPrototypeOf raises for each refusal.
pub fn set_proto_fail_message(fail: SetProtoFail) -> String {
  case fail {
    NotExtensible -> "Cannot set prototype of a non-extensible object"
    Cyclic -> "Cyclic __proto__ value"
    Immutable -> "Immutable prototype object cannot have its prototype set"
    TrapRefused -> "'setPrototypeOf' on proxy: trap returned falsish"
  }
}

/// OrdinarySetPrototypeOf — ES2024 §10.1.2.1
/// (with §10.4.7 SetImmutablePrototype check for Object.prototype)
///
/// Ordinary objects only — proxies must go through `set_prototype_of_stateful`.
/// `TrapRefused` is unreachable here.
pub fn ordinary_set_prototype_of(
  state: State(host),
  ref: Ref,
  new_proto: Option(Ref),
) -> Result(State(host), SetProtoFail) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(prototype: current, extensible:, ..))
      if new_proto != current
    -> {
      // §10.4.7.2 SetImmutablePrototype — Object.prototype is an Immutable
      // Prototype Exotic Object (§20.1.3): [[SetPrototypeOf]](V) returns
      // false unless SameValue(V, current). The same-value case already
      // fell through above, so any change here is rejected.
      use <- bool.guard(
        ref == state.builtins.object.prototype,
        Error(Immutable),
      )
      case extensible {
        False -> Error(NotExtensible)
        True ->
          case would_create_cycle(state.heap, ref, new_proto) {
            True -> Error(Cyclic)
            False -> {
              let heap = {
                use slot <- heap.update(state.heap, ref)
                case slot {
                  ObjectSlot(..) -> ObjectSlot(..slot, prototype: new_proto)
                  _ -> slot
                }
              }
              Ok(State(..state, heap:))
            }
          }
      }
    }
    // Same proto already, or ref isn't an ObjectSlot (unreachable) — no-op success.
    _ -> Ok(state)
  }
}

/// Cycle detection for OrdinarySetPrototypeOf — §10.1.2.1 step 7.
///
/// Implements the prototype chain walk:
///   7. Repeat, while done is false,
///      a. If p is null, set done to true.
///      b. Else if SameValue(p, O) is true, return false.
///      c. Else if p.[[GetPrototypeOf]] is not ordinary, set done to true.
///      d. Else set p to p.[[Prototype]].
///
/// Returns True if adding the link would create a cycle (step 7b triggers),
/// False if the chain terminates without hitting target (step 7a).
/// Step 7c (exotic objects) is not applicable — all our objects are ordinary.
fn would_create_cycle(
  heap: Heap(host),
  target_ref: Ref,
  new_proto: Option(Ref),
) -> Bool {
  case new_proto {
    // §10.1.2.1 step 7a: p is null → done, no cycle.
    None -> False
    // §10.1.2.1 step 7b: SameValue(p, O) → cycle detected.
    Some(p) if p == target_ref -> True
    Some(p) ->
      case heap.read(heap, p) {
        // §10.1.2.1 step 7c.i: p's [[GetPrototypeOf]] is not the ordinary
        // method (proxies trap it, §10.5.1) → done, no cycle detected.
        Some(ObjectSlot(kind: value.ProxyObject(..), ..)) -> False
        // §10.1.2.1 step 7c.ii: set p to p.[[Prototype]] and continue.
        Some(ObjectSlot(prototype: next, ..)) ->
          would_create_cycle(heap, target_ref, next)
        _ -> False
      }
  }
}

// ============================================================================
// Proxy trap machinery for descriptor/key operations — §10.5.5/.6/.11
// (kept alongside the ParsedDesc/ObjectKey descriptor plumbing so ops/object
// stays free of the descriptor engine)
// ============================================================================

/// ObjectKey → JS-visible key value (String or Symbol). Used for proxy trap
/// arguments and wherever a key crosses back into JS (arrays, result objects).
pub fn object_key_value(key: ObjectKey) -> JsValue {
  case key {
    StringPropKey(display:, ..) -> JsString(display)
    SymbolPropKey(sym) -> JsSymbol(sym)
  }
}

/// ObjectKey → bare error-message text (V8 prints the key unquoted in
/// "Cannot define property x, ..."). THE renderer for that shape.
fn key_text(key: ObjectKey) -> String {
  case key {
    StringPropKey(display:, ..) -> display
    SymbolPropKey(_) -> "[symbol]"
  }
}

/// ObjectKey → quoted error-message text, for messages that name the key
/// mid-sentence ("Cannot redefine property: 'x'").
fn key_quoted(key: ObjectKey) -> String {
  case key {
    StringPropKey(display:, ..) -> "'" <> display <> "'"
    SymbolPropKey(_) -> "[symbol]"
  }
}

/// JsValue → ObjectKey, for the JS-visible entry points that still take a raw
/// value. `None` for anything that is not a String or a Symbol: §6.1.7 says
/// such a value is not a property key, so no object can have an own property
/// under it. (Never fabricate a key here — an empty-string stand-in would
/// alias a real "" property.)
fn object_key_of_value(val: JsValue) -> Option(ObjectKey) {
  case val {
    JsSymbol(sym) -> Some(SymbolPropKey(sym))
    JsString(s) -> Some(StringPropKey(key.canonical_key(s), s))
    _ -> None
  }
}

/// The ObjectKey for a plain string key (no index parsing needed when the
/// caller already knows the shape).
pub fn named_object_key(name: String) -> ObjectKey {
  StringPropKey(Named(name), name)
}

/// The ObjectKey for an array-index key.
pub fn index_object_key(idx: Int) -> ObjectKey {
  StringPropKey(Index(idx), int.to_string(idx))
}

/// Keep the string keys of an [[OwnPropertyKeys]] result, paired with their
/// name — for the string-only reflections (Object.keys, for-in).
fn string_key_and_name(k: ObjectKey) -> Result(#(ObjectKey, String), Nil) {
  case k {
    StringPropKey(display:, ..) -> Ok(#(k, display))
    SymbolPropKey(_) -> Error(Nil)
  }
}

/// §10.5.11 steps 12-14: split a proxy target's own keys into
/// (non-configurable, configurable), each key's descriptor read with
/// `? target.[[GetOwnProperty]](key)` — a trap when the target is a proxy
/// itself. Both lists come back in `keys` order.
fn partition_configurable(
  state: State(host),
  t: Ref,
  keys: List(ObjectKey),
  nonconf: List(ObjectKey),
  conf: List(ObjectKey),
) -> Result(
  #(#(List(ObjectKey), List(ObjectKey)), State(host)),
  #(JsValue, State(host)),
) {
  case keys {
    [] -> Ok(#(#(list.reverse(nonconf), list.reverse(conf)), state))
    [k, ..rest] -> {
      use #(prop, state) <- result.try(own_property_keyed(state, t, k))
      let is_nonconf =
        option.map(prop, fn(p) { !value.prop_configurable(p) })
        |> option.unwrap(False)
      case is_nonconf {
        True -> partition_configurable(state, t, rest, [k, ..nonconf], conf)
        False -> partition_configurable(state, t, rest, nonconf, [k, ..conf])
      }
    }
  }
}

/// §6.2.6.6 CompletePropertyDescriptor — fill absent fields with defaults,
/// yielding a concrete Property.
fn complete_descriptor(parsed: ParsedDesc) -> value.Property {
  case desc_is_accessor(parsed) {
    True ->
      AccessorProperty(
        get: case parsed.get {
          Some(JsUndefined) | None -> None
          Some(g) -> Some(g)
        },
        set: case parsed.set {
          Some(JsUndefined) | None -> None
          Some(s) -> Some(s)
        },
        enumerable: option.unwrap(parsed.enumerable, False),
        configurable: option.unwrap(parsed.configurable, False),
        seq: value.next_prop_seq(),
      )
    False ->
      DataProperty(
        value: option.unwrap(parsed.value, JsUndefined),
        writable: option.unwrap(parsed.writable, False),
        enumerable: option.unwrap(parsed.enumerable, False),
        configurable: option.unwrap(parsed.configurable, False),
        seq: value.next_prop_seq(),
      )
  }
}

/// §10.1.6.3 ValidateAndApplyPropertyDescriptor in validation-only mode
/// (IsCompatiblePropertyDescriptor §10.1.6.2): would defining `desc` over
/// `current` succeed on an object with the given extensibility?
fn is_compatible_descriptor(
  extensible: Bool,
  desc: ParsedDesc,
  current: Option(value.Property),
) -> Bool {
  case current {
    // Step 2: no current property — allowed iff extensible.
    None -> extensible
    Some(cur) ->
      case value.prop_configurable(cur) {
        // Configurable current property accepts any redefinition.
        True -> True
        False -> {
          // Step 4: non-configurable — reject configurable:true or an
          // enumerable flip.
          let bad_configurable = desc.configurable == Some(True)
          let bad_enumerable = case desc.enumerable {
            Some(e) -> e != value.prop_enumerable(cur)
            None -> False
          }
          use <- bool.guard(bad_configurable || bad_enumerable, False)
          let is_acc = desc_is_accessor(desc)
          let is_dat = desc_is_data(desc)
          // Step 5: generic descriptor — no further validation.
          use <- bool.guard(!is_acc && !is_dat, True)
          case cur {
            // Step 6: kind change on non-configurable is rejected.
            DataProperty(writable: cur_w, value: cur_v, ..) ->
              case is_acc {
                True -> False
                False ->
                  case cur_w {
                    True -> True
                    // Step 7: non-writable data — no writable:true, no
                    // value change (SameValue).
                    False ->
                      desc.writable != Some(True)
                      && case desc.value {
                        Some(v) -> value.same_value(v, cur_v)
                        None -> True
                      }
                  }
              }
            // Step 8: non-configurable accessor — get/set must be unchanged.
            AccessorProperty(get: cur_g, set: cur_s, ..) ->
              case is_dat {
                True -> False
                False ->
                  case desc.get {
                    Some(g) ->
                      value.same_value(g, option.unwrap(cur_g, JsUndefined))
                    None -> True
                  }
                  && case desc.set {
                    Some(s) ->
                      value.same_value(s, option.unwrap(cur_s, JsUndefined))
                    None -> True
                  }
              }
          }
        }
      }
  }
}

/// FromPropertyDescriptor (§6.2.6.4) on a PARTIAL descriptor — only fields
/// present in `parsed` become properties. This is the descriptor object the
/// defineProperty trap receives (§10.5.6 step 9).
fn from_parsed_descriptor(
  h: Heap(host),
  parsed: ParsedDesc,
  object_proto: Ref,
) -> #(Heap(host), Ref) {
  let entries =
    list.flatten([
      case parsed.value {
        Some(v) -> [#("value", value.data_property(v))]
        None -> []
      },
      case parsed.writable {
        Some(w) -> [#("writable", value.data_property(JsBool(w)))]
        None -> []
      },
      case parsed.get {
        Some(g) -> [#("get", value.data_property(g))]
        None -> []
      },
      case parsed.set {
        Some(s) -> [#("set", value.data_property(s))]
        None -> []
      },
      case parsed.enumerable {
        Some(e) -> [#("enumerable", value.data_property(JsBool(e)))]
        None -> []
      },
      case parsed.configurable {
        Some(c) -> [#("configurable", value.data_property(JsBool(c)))]
        None -> []
      },
    ])
  common.alloc_pojo(h, object_proto, entries)
}

/// §6.2.6.5 steps 12.b / 14.b: a `get`/`set` field that is present, is not
/// undefined and is not callable is a TypeError. `role` is "Getter"/"Setter".
///
/// Split out because each check must fire the instant its own field is read —
/// batching both until after all six reads would let a later field's getter or
/// `has` trap run after the spec has already thrown.
fn require_callable_accessor(
  state: State(host),
  field: Option(JsValue),
  role: String,
) -> Result(Nil, #(JsValue, State(host))) {
  case field {
    Some(f) ->
      case f == JsUndefined || helpers.is_callable(state.heap, f) {
        True -> Ok(Nil)
        False -> throw_type_error(state, role <> " must be a function")
      }
    None -> Ok(Nil)
  }
}

/// ToPropertyDescriptor (§6.2.6.5) on an arbitrary value — step 1 rejects a
/// non-Object, then the six fields are read (invoking getters), get/set
/// callability and the accessor/data conflict validated.
///
/// This is the ONLY place the "descriptor is not an object" TypeError is
/// raised, so no caller can accidentally raise it before its own
/// ? ToPropertyKey step (whose user code — a `toString` that throws — must be
/// observed first).
///
/// The read ORDER is normative and observable — a Proxy descriptor sees the
/// `has`/`get` traps in exactly this sequence: enumerable, configurable, value,
/// writable, get, set. Step 12.b's getter check also runs BEFORE step 13 reads
/// "set", so `Object.defineProperty(o, "k", { get: 1, get set() { throw } })`
/// is a plain TypeError and never runs the `set` accessor.
pub fn parse_descriptor(
  state: State(host),
  desc_obj: JsValue,
) -> Result(#(ParsedDesc, State(host)), #(JsValue, State(host))) {
  // Step 1: If Obj is not an Object, throw a TypeError exception.
  use Nil <- result.try(case desc_obj {
    JsObject(_) -> Ok(Nil)
    _ -> throw_type_error(state, "Property description must be an object")
  })
  // Steps 3-4.
  use #(desc_enumerable, state) <- result.try(read_desc_bool(
    state,
    desc_obj,
    "enumerable",
  ))
  // Steps 5-6.
  use #(desc_configurable, state) <- result.try(read_desc_bool(
    state,
    desc_obj,
    "configurable",
  ))
  // Steps 7-8.
  use #(desc_value, state) <- result.try(read_desc_field(
    state,
    desc_obj,
    "value",
  ))
  // Steps 9-10.
  use #(desc_writable, state) <- result.try(read_desc_bool(
    state,
    desc_obj,
    "writable",
  ))
  // Steps 11-12: read "get", then reject a non-callable one immediately.
  use #(desc_get, state) <- result.try(read_desc_field(state, desc_obj, "get"))
  use Nil <- result.try(require_callable_accessor(state, desc_get, "Getter"))
  // Steps 13-14: only now is "set" observed at all.
  use #(desc_set, state) <- result.try(read_desc_field(state, desc_obj, "set"))
  use Nil <- result.try(require_callable_accessor(state, desc_set, "Setter"))
  let parsed =
    ParsedDesc(
      get: desc_get,
      set: desc_set,
      value: desc_value,
      writable: desc_writable,
      enumerable: desc_enumerable,
      configurable: desc_configurable,
    )
  // Step 15: accessor and data attributes are mutually exclusive.
  use Nil <- result.map(case desc_is_accessor(parsed) && desc_is_data(parsed) {
    True ->
      throw_type_error(
        state,
        "Invalid property descriptor. Cannot both specify accessors and a value or writable attribute",
      )
    False -> Ok(Nil)
  })
  #(parsed, state)
}

/// Trap-aware [[GetOwnProperty]] — §10.1.5.1 for ordinary objects, §10.5.5
/// for proxies. The single entry point for getOwnPropertyDescriptor-style
/// reflection on possibly-proxy refs. A key that is neither String nor Symbol
/// (§6.1.7) can be no object's own property, so it reports undefined.
pub fn get_own_property_stateful(
  state: State(host),
  ref: Ref,
  key_val: JsValue,
) -> Result(#(Option(value.Property), State(host)), #(JsValue, State(host))) {
  case object_key_of_value(key_val) {
    Some(key) -> own_property_keyed(state, ref, key)
    None -> Ok(#(None, state))
  }
}

/// ObjectKey-keyed variant of get_own_property_stateful for in-module callers
/// that already resolved the key.
///
/// The engine's TDZ sentinel is filtered out HERE, once, so a returned
/// `Option(value.Property)` structurally cannot carry it: §10.4.6.5 says a
/// module namespace's [[GetOwnProperty]] performs [[Get]] on the binding, and
/// an uninitialized binding throws a ReferenceError before any descriptor
/// exists. Callers therefore never see `DataProperty(value: JsUninitialized)`
/// and cannot leak it (or the `undefined` it renders as) into JS.
pub fn own_property_keyed(
  state: State(host),
  ref: Ref,
  key: ObjectKey,
) -> Result(#(Option(value.Property), State(host)), #(JsValue, State(host))) {
  use #(prop, state) <- result.try(case object.as_proxy(state.heap, ref) {
    Some(slots) -> proxy_get_own_property(state, slots, key)
    None -> Ok(#(get_own_property_by_key(state.heap, ref, key), state))
  })
  case prop {
    Some(DataProperty(value: value.JsUninitialized, ..)) ->
      Error(state.reference_error_value(state, tdz_message))
    _ -> Ok(#(prop, state))
  }
}

/// §10.5.5 Proxy [[GetOwnProperty]] ( P ).
fn proxy_get_own_property(
  state: State(host),
  slots: Option(value.ProxySlots),
  key: ObjectKey,
) -> Result(#(Option(value.Property), State(host)), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(object.proxy_trap(
    state,
    slots,
    "getOwnPropertyDescriptor",
  ))
  case trap {
    // Step 6: no trap → target.[[GetOwnProperty]](P).
    None -> own_property_keyed(state, t, key)
    Some(trap_fn) -> {
      // Step 7: Call(trap, handler, « target, P »).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [
          JsObject(t),
          object_key_value(key),
        ]),
      )
      case res {
        // Steps 9-11: trap says "absent". Step 9 first reads
        // `? target.[[GetOwnProperty]](P)` — the target's OWN trap when the
        // target is itself a proxy, never a raw heap read.
        JsUndefined -> {
          use #(target_desc, state) <- result.try(own_property_keyed(
            state,
            t,
            key,
          ))
          case target_desc {
            None -> Ok(#(None, state))
            Some(prop) ->
              case value.prop_configurable(prop) {
                False ->
                  throw_type_error(
                    state,
                    "'getOwnPropertyDescriptor' on proxy: trap returned undefined for property "
                      <> key_quoted(key)
                      <> " which is non-configurable in the proxy target",
                  )
                True -> {
                  // Step 11.c: extensibleTarget = ? IsExtensible(target).
                  use #(ext, state) <- result.try(object.is_extensible_stateful(
                    state,
                    t,
                  ))
                  case ext {
                    False ->
                      throw_type_error(
                        state,
                        "'getOwnPropertyDescriptor' on proxy: trap returned undefined for property "
                          <> key_quoted(key)
                          <> " which exists in the non-extensible proxy target",
                      )
                    True -> Ok(#(None, state))
                  }
                }
              }
          }
        }
        // Steps 9-17: trap returned a descriptor object — validate.
        JsObject(_) -> {
          use #(target_desc, state) <- result.try(own_property_keyed(
            state,
            t,
            key,
          ))
          // Step 12: extensibleTarget = ? IsExtensible(target).
          use #(ext, state) <- result.try(object.is_extensible_stateful(
            state,
            t,
          ))
          use #(parsed, state) <- result.try(parse_descriptor(state, res))
          let completed = complete_descriptor(parsed)
          // Step 15: IsCompatiblePropertyDescriptor against the COMPLETED
          // descriptor (CompletePropertyDescriptor ran at step 14).
          let completed_parsed = case completed {
            DataProperty(
              value: v,
              writable: w,
              enumerable: e,
              configurable: c,
              ..,
            ) ->
              ParsedDesc(
                get: None,
                set: None,
                value: Some(v),
                writable: Some(w),
                enumerable: Some(e),
                configurable: Some(c),
              )
            AccessorProperty(get: g, set: s, enumerable: e, configurable: c, ..) ->
              ParsedDesc(
                get: Some(option.unwrap(g, JsUndefined)),
                set: Some(option.unwrap(s, JsUndefined)),
                value: None,
                writable: None,
                enumerable: Some(e),
                configurable: Some(c),
              )
          }
          use Nil <- result.try(
            case is_compatible_descriptor(ext, completed_parsed, target_desc) {
              False ->
                throw_type_error(
                  state,
                  "'getOwnPropertyDescriptor' on proxy: trap returned descriptor for property "
                    <> key_quoted(key)
                    <> " that is incompatible with the existing property in the proxy target",
                )
              True -> Ok(Nil)
            },
          )
          // Step 16: resultDesc.[[Configurable]] false requires a matching
          // non-configurable target property.
          use Nil <- result.try(case value.prop_configurable(completed) {
            True -> Ok(Nil)
            False ->
              case target_desc {
                None ->
                  throw_type_error(
                    state,
                    "'getOwnPropertyDescriptor' on proxy: trap reported non-configurability for property "
                      <> key_quoted(key)
                      <> " which is non-existent in the proxy target",
                  )
                Some(td) ->
                  case value.prop_configurable(td) {
                    True ->
                      throw_type_error(
                        state,
                        "'getOwnPropertyDescriptor' on proxy: trap reported non-configurability for property "
                          <> key_quoted(key)
                          <> " which is configurable in the proxy target",
                      )
                    False ->
                      // Step 16.b: writable:false requires target's
                      // writable:false too.
                      case parsed.writable, td {
                        Some(False), DataProperty(writable: True, ..) ->
                          throw_type_error(
                            state,
                            "'getOwnPropertyDescriptor' on proxy: trap reported non-writability for property "
                              <> key_quoted(key)
                              <> " which is writable in the proxy target",
                          )
                        _, _ -> Ok(Nil)
                      }
                  }
              }
          })
          Ok(#(Some(completed), state))
        }
        _ -> {
          use Nil <- result.map(throw_type_error(
            state,
            "'getOwnPropertyDescriptor' on proxy: trap returned neither object nor undefined for property "
              <> key_quoted(key),
          ))
          #(None, state)
        }
      }
    }
  }
}

/// §10.5.6 Proxy [[DefineOwnProperty]] ( P, Desc ). Returns the raw boolean —
/// callers decide whether false throws (DefinePropertyOrThrow) or not
/// (Reflect.defineProperty).
fn proxy_define_own_property(
  state: State(host),
  slots: Option(value.ProxySlots),
  key: ObjectKey,
  parsed: ParsedDesc,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(object.proxy_trap(
    state,
    slots,
    "defineProperty",
  ))
  case trap {
    // Step 6: no trap → target.[[DefineOwnProperty]](P, Desc). The raw
    // internal method returns a boolean — only a validation rejection maps to
    // `false` here (DefinePropertyOrThrow is applied by the CALLER of the
    // outermost proxy, not per level); genuine abrupt completions (e.g.
    // ArraySetLength's RangeError) propagate.
    None ->
      case object.as_proxy(state.heap, t) {
        Some(inner) -> proxy_define_own_property(state, inner, key, parsed)
        None ->
          case define_parsed(state, t, key, parsed) {
            Ok(state) -> Ok(#(state, True))
            Error(#(DefineRejected(_), state)) -> Ok(#(state, False))
            Error(#(DefineThrew(thrown), state)) -> Error(#(thrown, state))
          }
      }
    Some(trap_fn) -> {
      // Step 9: descObj = FromPropertyDescriptor(Desc) — a fresh object
      // carrying only the present fields.
      let #(h2, desc_obj_ref) =
        from_parsed_descriptor(
          state.heap,
          parsed,
          state.builtins.object.prototype,
        )
      let state = State(..state, heap: h2)
      // Step 10: ToBoolean(Call(trap, handler, « target, P, descObj »)).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [
          JsObject(t),
          object_key_value(key),
          JsObject(desc_obj_ref),
        ]),
      )
      case value.is_truthy(res) {
        False -> Ok(#(state, False))
        True -> {
          // Steps 12-16: invariants. Both reads are the target's own internal
          // methods (`? target.[[GetOwnProperty]](P)`, `? IsExtensible(target)`)
          // — traps fire when the target is itself a proxy.
          use #(target_desc, state) <- result.try(own_property_keyed(
            state,
            t,
            key,
          ))
          use #(ext, state) <- result.try(object.is_extensible_stateful(
            state,
            t,
          ))
          let setting_config_false = parsed.configurable == Some(False)
          case target_desc {
            None -> {
              use Nil <- result.try(case ext {
                False ->
                  throw_type_error(
                    state,
                    "'defineProperty' on proxy: trap returned truish for adding property "
                      <> key_quoted(key)
                      <> " to the non-extensible proxy target",
                  )
                True -> Ok(Nil)
              })
              use Nil <- result.map(case setting_config_false {
                True ->
                  throw_type_error(
                    state,
                    "'defineProperty' on proxy: trap returned truish for defining non-configurable property "
                      <> key_quoted(key)
                      <> " which is either non-existent or configurable in the proxy target",
                  )
                False -> Ok(Nil)
              })
              #(state, True)
            }
            Some(cur) -> {
              use Nil <- result.try(
                case is_compatible_descriptor(ext, parsed, Some(cur)) {
                  False ->
                    throw_type_error(
                      state,
                      "'defineProperty' on proxy: trap returned truish for adding property "
                        <> key_quoted(key)
                        <> " that is incompatible with the existing property in the proxy target",
                    )
                  True -> Ok(Nil)
                },
              )
              use Nil <- result.try(
                case setting_config_false && value.prop_configurable(cur) {
                  True ->
                    throw_type_error(
                      state,
                      "'defineProperty' on proxy: trap returned truish for defining non-configurable property "
                        <> key_quoted(key)
                        <> " which is either non-existent or configurable in the proxy target",
                    )
                  False -> Ok(Nil)
                },
              )
              // Step 16.c: writable:false over a non-configurable writable
              // data property is rejected.
              use Nil <- result.map(case cur {
                DataProperty(configurable: False, writable: True, ..) ->
                  case parsed.writable {
                    Some(False) ->
                      throw_type_error(
                        state,
                        "'defineProperty' on proxy: trap returned truish for defining non-writable property "
                          <> key_quoted(key)
                          <> " which is writable in the proxy target",
                      )
                    _ -> Ok(Nil)
                  }
                _ -> Ok(Nil)
              })
              #(state, True)
            }
          }
        }
      }
    }
  }
}

/// `define_property_bool` for callers that already hold a descriptor OBJECT
/// (so ToPropertyDescriptor's step 1 can never fire and the argument order is
/// unobservable).
pub fn define_property_bool(
  state: State(host),
  ref: Ref,
  key_val: JsValue,
  desc_ref: Ref,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  define_property_bool_value(state, ref, key_val, JsObject(desc_ref))
}

/// Reflect.defineProperty semantics (§28.1.3): ? ToPropertyKey (step 2) then
/// ? ToPropertyDescriptor (step 3) — in that order, so a `propertyKey` whose
/// `toString` throws is observed BEFORE a non-object `attributes` is rejected
/// — then ? target.[[DefineOwnProperty]] — a validation rejection becomes
/// `false`, while genuine abrupt completions (ArraySetLength's RangeError,
/// user code throwing, proxy trap/invariant errors) are re-thrown.
pub fn define_property_bool_value(
  state: State(host),
  ref: Ref,
  key_val: JsValue,
  desc_val: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  use #(dkey, state) <- result.try(property.to_prop_key(state, key_val))
  use #(parsed, state) <- result.try(parse_descriptor(state, desc_val))
  case object.as_proxy(state.heap, ref) {
    Some(slots) -> proxy_define_own_property(state, slots, dkey, parsed)
    None ->
      case define_parsed(state, ref, dkey, parsed) {
        Ok(state) -> Ok(#(state, True))
        // [[DefineOwnProperty]] validated to false → false (not rethrown).
        Error(#(DefineRejected(_), state)) -> Ok(#(state, False))
        Error(#(DefineThrew(thrown), state)) -> Error(#(thrown, state))
      }
  }
}

/// §7.3.7 CreateDataPropertyOrThrow ( O, P, V ) in the file's CPS shape.
///
/// CreateDataProperty (§7.3.5) is [[DefineOwnProperty]](P, {[[Value]]: V,
/// [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}); the
/// OrThrow wrapper converts a `false` result (non-extensible target,
/// incompatible non-configurable existing key, falsy proxy `defineProperty`
/// trap) into the realm's TypeError. Builtins that create result properties
/// (Object.fromEntries, Object.groupBy, `stack` setter, ...) MUST funnel
/// through here rather than hand-rolling the descriptor — a hand-rolled copy
/// can silently get the attributes wrong (e.g. a non-enumerable property) or
/// drop the `false` result.
pub fn create_data_property_or_throw(
  state: State(host),
  ref: Ref,
  key_val: JsValue,
  val: JsValue,
  cont: fn(State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case create_data_property(state, ref, key_val, val) {
    Ok(state) -> cont(state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// §7.3.7 CreateDataPropertyOrThrow as a plain fallible op — the shape the
/// `iter_protocol` entry sinks want. `create_data_property_or_throw` is the
/// CPS wrapper over it, `create_data_property_bool` the un-thrown §7.3.5 it
/// wraps.
pub fn create_data_property(
  state: State(host),
  ref: Ref,
  key_val: JsValue,
  val: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  use #(dkey, state) <- result.try(property.to_prop_key(state, key_val))
  use #(state, defined) <- result.try(create_data_property_dkey(
    state,
    ref,
    dkey,
    val,
  ))
  case defined {
    True -> Ok(state)
    // §7.3.7 step 3: success is false → throw a TypeError exception.
    False ->
      throw_type_error(state, "Cannot create property " <> key_quoted(dkey))
  }
}

/// §7.3.5 CreateDataProperty ( O, P, V ) — the un-thrown form: a rejected
/// [[DefineOwnProperty]] comes back as `False`, only genuine abrupt
/// completions (proxy trap throws, user getters) are propagated. Callers whose
/// spec text says a bare `Perform ? CreateDataProperty(...)` (JSON.parse's
/// reviver, §25.5.1.1) want this one — the OrThrow variant would invent a
/// TypeError the spec never throws.
pub fn create_data_property_bool(
  state: State(host),
  ref: Ref,
  key_val: JsValue,
  val: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  use #(dkey, state) <- result.try(property.to_prop_key(state, key_val))
  create_data_property_dkey(state, ref, dkey, val)
}

/// The shared tail of §7.3.5, past ToPropertyKey — so the OrThrow wrapper can
/// name the key in its message without converting `key_val` twice (a
/// `Symbol.toPrimitive` key would be observably re-run).
fn create_data_property_dkey(
  state: State(host),
  ref: Ref,
  dkey: ObjectKey,
  val: JsValue,
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  // §7.3.5 builds the Property Descriptor RECORD directly — it never runs
  // ToPropertyDescriptor over a descriptor object. Materializing a real JS
  // POJO here and re-parsing it would (a) make the internal descriptor
  // observably inherit `get`/`set` from a user-extended %Object.prototype%
  // (breaking Object.fromEntries / Object.groupBy under a plain
  // `Object.prototype.get = ...`), and (b) cost a heap allocation plus a
  // 6-field re-parse per created property inside builtin loops.
  let parsed =
    ParsedDesc(
      get: None,
      set: None,
      value: Some(val),
      writable: Some(True),
      enumerable: Some(True),
      configurable: Some(True),
    )
  // Trap-aware [[DefineOwnProperty]] on the already-parsed record —
  // the same tail define_property_bool dispatches to after parsing.
  case object.as_proxy(state.heap, ref) {
    Some(slots) -> proxy_define_own_property(state, slots, dkey, parsed)
    None ->
      case define_parsed(state, ref, dkey, parsed) {
        Ok(state) -> Ok(#(state, True))
        // [[DefineOwnProperty]] validated to false → false (not rethrown).
        Error(#(DefineRejected(_), state)) -> Ok(#(state, False))
        Error(#(DefineThrew(thrown), state)) -> Error(#(thrown, state))
      }
  }
}

/// Trap-aware [[OwnPropertyKeys]] — §10.1.11 for ordinary objects, §10.5.11
/// for proxies. Returns the full key list: integer-index strings ascending,
/// then named strings, then symbols (or whatever order the proxy trap chose).
///
/// Every element is a String or a Symbol *by construction* — the ordinary path
/// builds them from the property tables, the proxy path validates the trap
/// result (§7.3.19). Hence `ObjectKey`, not `JsValue`: no caller has to invent
/// a "not a key" branch, and none can silently drop one.
pub fn own_property_keys(
  state: State(host),
  ref: Ref,
) -> Result(#(List(ObjectKey), State(host)), #(JsValue, State(host))) {
  case object.as_proxy(state.heap, ref) {
    Some(slots) -> proxy_own_keys(state, slots)
    None -> {
      let strings =
        collect_own_keys(state.heap, ref, False)
        |> list.map(fn(s) { StringPropKey(key.canonical_key(s), s) })
      let syms =
        collect_own_symbol_keys(state.heap, ref, False)
        |> list.map(SymbolPropKey)
      Ok(#(list.append(strings, syms), state))
    }
  }
}

/// [[OwnPropertyKeys]] as JS values — CreateArrayFromList's input, i.e. the
/// JS-visible boundary (Reflect.ownKeys, the keyed Promise combinators).
/// Prefer `own_property_keys` for anything that then looks the key back up.
pub fn own_keys_stateful(
  state: State(host),
  ref: Ref,
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  use #(keys, state) <- result.map(own_property_keys(state, ref))
  #(list.map(keys, object_key_value), state)
}

/// §10.5.11 Proxy [[OwnPropertyKeys]] ( ).
fn proxy_own_keys(
  state: State(host),
  slots: Option(value.ProxySlots),
) -> Result(#(List(ObjectKey), State(host)), #(JsValue, State(host))) {
  use #(t, h, trap, state) <- result.try(object.proxy_trap(
    state,
    slots,
    "ownKeys",
  ))
  case trap {
    // Step 6: no trap → target.[[OwnPropertyKeys]]().
    None -> own_property_keys(state, t)
    Some(trap_fn) -> {
      // Step 7: Call(trap, handler, « target »).
      use #(res, state) <- result.try(
        state.call(state, trap_fn, JsObject(h), [JsObject(t)]),
      )
      // Step 8: CreateListFromArrayLike(trapResultArray, property-key).
      use #(keys, state) <- result.try(keys_from_array_like(state, res))
      // Step 9: duplicate entries are rejected.
      use Nil <- result.try(case has_duplicate_keys(keys, []) {
        True ->
          throw_type_error(
            state,
            "'ownKeys' on proxy: trap returned duplicate entries",
          )
        False -> Ok(Nil)
      })
      // Step 10: extensibleTarget = ? IsExtensible(target).
      use #(ext, state) <- result.try(object.is_extensible_stateful(state, t))
      // Steps 11-14: split target keys by configurability.
      use #(target_keys, state) <- result.try(own_property_keys(state, t))
      use #(#(nonconf, conf), state) <- result.try(
        partition_configurable(state, t, target_keys, [], []),
      )
      // Step 17: every non-configurable target key must be reported.
      use Nil <- result.try(
        list.try_each(nonconf, fn(k) {
          case list.contains(keys, k) {
            True -> Ok(Nil)
            False ->
              throw_type_error(
                state,
                "'ownKeys' on proxy: trap result did not include "
                  <> key_quoted(k)
                  <> ", a non-configurable key of the proxy target",
              )
          }
        }),
      )
      case ext {
        // Step 18: extensible target — no further checks.
        True -> Ok(#(keys, state))
        False -> {
          // Step 19: every (configurable) target key must be reported…
          use Nil <- result.try(
            list.try_each(conf, fn(k) {
              case list.contains(keys, k) {
                True -> Ok(Nil)
                False ->
                  throw_type_error(
                    state,
                    "'ownKeys' on proxy: trap result did not include "
                      <> key_quoted(k)
                      <> ", a key of the non-extensible proxy target",
                  )
              }
            }),
          )
          // Steps 20-21: …and no extra keys may be invented.
          let extras =
            list.filter(keys, fn(k) { !list.contains(target_keys, k) })
          use Nil <- result.map(case extras {
            [] -> Ok(Nil)
            [_, ..] ->
              throw_type_error(
                state,
                "'ownKeys' on proxy: trap returned extra keys but proxy target is non-extensible",
              )
          })
          #(keys, state)
        }
      }
    }
  }
}

/// True when `keys` contains the same String/Symbol value twice.
fn has_duplicate_keys(keys: List(ObjectKey), seen: List(ObjectKey)) -> Bool {
  case keys {
    [] -> False
    [k, ..rest] ->
      case list.contains(seen, k) {
        True -> True
        False -> has_duplicate_keys(rest, [k, ..seen])
      }
  }
}

/// CreateListFromArrayLike(obj, property-key) — §7.3.19. Each element must be
/// a String or Symbol, else TypeError — so the result is a list of ObjectKeys.
fn keys_from_array_like(
  state: State(host),
  val: JsValue,
) -> Result(#(List(ObjectKey), State(host)), #(JsValue, State(host))) {
  case val {
    JsObject(arr_ref) ->
      case heap.read_array_like(state.heap, arr_ref) {
        // Fast path: genuine array-backed object — read elements directly.
        Some(#(length, els)) -> {
          let items = gather_array_like(els, 0, length, [])
          use keys <- result.map(
            list.try_map(items, validate_trap_key(state, _)),
          )
          #(keys, state)
        }
        // Generic §7.3.19 path: ToLength(? Get(obj, "length")), then
        // ? Get(obj, ToString(i)) for each index — honors array-like plain
        // objects ({0:'a', length:1}), getter-backed results, and proxies
        // wrapping arrays.
        None -> {
          // Step 3: Let len be ? LengthOfArrayLike(obj).
          use #(len_val, state) <- result.try(object.get_value(
            state,
            arr_ref,
            Named("length"),
            val,
          ))
          use #(len, state) <- result.try(trap_result_length(state, len_val))
          gather_keys_via_get(state, val, arr_ref, 0, len, [])
        }
      }
    _ -> {
      use Nil <- result.map(throw_type_error(
        state,
        "CreateListFromArrayLike called on non-object",
      ))
      #([], state)
    }
  }
}

/// §7.3.19 step 7.c (property-key element types): each element of the ownKeys
/// trap result must be a String or a Symbol — the one place a JsValue becomes
/// an ObjectKey, so downstream code never re-checks.
fn validate_trap_key(
  state: State(host),
  item: JsValue,
) -> Result(ObjectKey, #(JsValue, State(host))) {
  case object_key_of_value(item) {
    Some(k) -> Ok(k)
    None ->
      throw_type_error(
        state,
        "'ownKeys' on proxy: trap returned a non-String, non-Symbol key",
      )
  }
}

/// §7.1.20 ToLength for the ownKeys trap result, additionally bounded by the
/// iteration budget — the generic path performs one observable Get per index.
fn trap_result_length(
  state: State(host),
  val: JsValue,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  use #(n, state) <- result.try(coerce.js_to_number(state, val))
  let len = case n {
    value.Finite(f) ->
      int.clamp(value.float_to_int(f), 0, limits.max_safe_integer)
    value.Infinity -> limits.max_safe_integer
    value.NaN | value.NegInfinity -> 0
  }
  case len > limits.max_iteration {
    True ->
      Error(state.range_error_value(
        state,
        "'ownKeys' on proxy: trap result length exceeds iteration budget",
      ))
    False -> Ok(#(len, state))
  }
}

/// §7.3.19 steps 6-8 (generic path): read indices 0..len-1 with observable
/// Get, validating each element is a String or Symbol.
fn gather_keys_via_get(
  state: State(host),
  obj: JsValue,
  arr_ref: Ref,
  idx: Int,
  length: Int,
  acc: List(ObjectKey),
) -> Result(#(List(ObjectKey), State(host)), #(JsValue, State(host))) {
  case idx >= length {
    True -> Ok(#(list.reverse(acc), state))
    False -> {
      // Step 7.b: Let next be ? Get(obj, ToString(index)).
      use #(item, state) <- result.try(object.get_value(
        state,
        arr_ref,
        Index(idx),
        obj,
      ))
      // Step 7.c: validate the element type.
      use k <- result.try(validate_trap_key(state, item))
      gather_keys_via_get(state, obj, arr_ref, idx + 1, length, [k, ..acc])
    }
  }
}

fn gather_array_like(
  els: JsElements,
  idx: Int,
  length: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> list.reverse(acc)
    False ->
      gather_array_like(els, idx + 1, length, [elements.get(els, idx), ..acc])
  }
}

/// EnumerableOwnProperties(O, key) for possibly-proxy objects: own keys via
/// [[OwnPropertyKeys]] (trap), then per-key [[GetOwnProperty]] (trap) to
/// filter for enumerable STRING keys.
pub fn enumerable_string_keys_stateful(
  state: State(host),
  ref: Ref,
) -> Result(#(List(String), State(host)), #(JsValue, State(host))) {
  use #(keys, state) <- result.try(own_property_keys(state, ref))
  let string_keys = list.filter_map(keys, string_key_and_name)
  list.try_fold(string_keys, #([], state), fn(acc, entry) {
    let #(found, state) = acc
    let #(k, s) = entry
    use #(prop, state) <- result.map(own_property_keyed(state, ref, k))
    case prop {
      Some(p) ->
        case value.prop_enumerable(p) {
          True -> #([s, ..found], state)
          False -> #(found, state)
        }
      None -> #(found, state)
    }
  })
  |> result.map(fn(acc) {
    let #(found, state) = acc
    #(list.reverse(found), state)
  })
}

/// §10.5.2 Proxy [[SetPrototypeOf]] — trap, falling through to
/// target.[[SetPrototypeOf]] (which may itself be a proxy) when no trap is
/// defined. Callers use `set_prototype_of_stateful`, which owns the
/// proxy-vs-ordinary dispatch this recurses back into.
fn proxy_set_proto(
  state: State(host),
  slots: Option(value.ProxySlots),
  new_proto: Option(Ref),
) -> Result(#(State(host), Bool), #(JsValue, State(host))) {
  let proto_val = case new_proto {
    Some(p) -> JsObject(p)
    None -> JsNull
  }
  use #(state, fallthrough, ok) <- result.try(object.proxy_set_prototype_of(
    state,
    slots,
    proto_val,
  ))
  case fallthrough {
    None -> Ok(#(state, ok))
    // Step 6: no trap → target.[[SetPrototypeOf]](V).
    Some(t) -> {
      use #(state, status) <- result.map(set_prototype_of_stateful(
        state,
        t,
        new_proto,
      ))
      #(state, result.is_ok(status))
    }
  }
}

/// EnumerateObjectProperties (§14.7.5.10) for proxy-containing chains: own
/// enumerable string keys via the traps at each prototype level, with
/// shadowing (a key seen at a shallower level hides deeper ones, enumerable
/// or not).
pub fn enumerate_keys_stateful(
  state: State(host),
  ref: Ref,
) -> Result(#(List(String), State(host)), #(JsValue, State(host))) {
  use #(acc_rev, _seen, state) <- result.map(
    enumerate_chain(state, ref, [], []),
  )
  #(list.reverse(acc_rev), state)
}

fn enumerate_chain(
  state: State(host),
  ref: Ref,
  seen: List(String),
  acc_rev: List(String),
) -> Result(#(List(String), List(String), State(host)), #(JsValue, State(host))) {
  use #(keys, state) <- result.try(own_property_keys(state, ref))
  let string_keys = list.filter_map(keys, string_key_and_name)
  use #(seen, acc_rev, state) <- result.try(
    list.try_fold(string_keys, #(seen, acc_rev, state), fn(st, entry) {
      let #(seen, acc_rev, state) = st
      let #(k, s) = entry
      case list.contains(seen, s) {
        True -> Ok(#(seen, acc_rev, state))
        False -> {
          use #(prop, state) <- result.map(own_property_keyed(state, ref, k))
          case prop {
            Some(p) ->
              case value.prop_enumerable(p) {
                True -> #([s, ..seen], [s, ..acc_rev], state)
                False -> #([s, ..seen], acc_rev, state)
              }
            None -> #([s, ..seen], acc_rev, state)
          }
        }
      }
    }),
  )
  use #(proto_val, state) <- result.try(object.get_prototype_of_stateful(
    state,
    ref,
  ))
  case proto_val {
    JsObject(p) -> enumerate_chain(state, p, seen, acc_rev)
    _ -> Ok(#(acc_rev, seen, state))
  }
}
