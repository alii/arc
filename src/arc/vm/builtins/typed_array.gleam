/// ES2024 §23.2 TypedArray Objects
///
/// %TypedArray% is an abstract intrinsic constructor: it has [[Construct]]
/// (so it can appear in `extends` and Reflect.construct type checks) but
/// throws TypeError when invoked. The 11 concrete constructors
/// (Int8Array .. BigUint64Array) inherit from it (both the functions and
/// their prototypes), and produce Integer-Indexed exotic objects backed by
/// an ArrayBufferObject heap slot.
///
/// Element reads live in arc/vm/ops/object (typed_array_element and the
/// view-length helpers, next to the rest of the MOP); element stores and
/// bulk encoding live in arc/vm/ops/typed_array_elements (typed_array_store
/// / typed_array_encode_value); this module is the constructor and
/// prototype surface.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/internal/typed_array_ffi.{ta_fill_region, ta_splice, ta_zeroed}
import arc/vm/key.{Index, Named}
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/ops/typed_array_elements
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, type TypedArrayKind, type TypedArrayNativeFn, Dispatch,
  Finite, JsBool, JsNumber, JsObject, JsString, JsUndefined, ObjectSlot,
  TypedArrayConstructor, TypedArrayGetBuffer, TypedArrayGetByteLength,
  TypedArrayGetByteOffset, TypedArrayGetLength, TypedArrayGetSpecies,
  TypedArrayGetToStringTag, TypedArrayIntrinsicConstructor, TypedArrayNative,
  TypedArrayPrototypeAt, TypedArrayPrototypeCopyWithin,
  TypedArrayPrototypeEntries, TypedArrayPrototypeEvery, TypedArrayPrototypeFill,
  TypedArrayPrototypeFilter, TypedArrayPrototypeFind,
  TypedArrayPrototypeFindIndex, TypedArrayPrototypeFindLast,
  TypedArrayPrototypeFindLastIndex, TypedArrayPrototypeForEach,
  TypedArrayPrototypeIncludes, TypedArrayPrototypeIndexOf,
  TypedArrayPrototypeJoin, TypedArrayPrototypeKeys,
  TypedArrayPrototypeLastIndexOf, TypedArrayPrototypeMap,
  TypedArrayPrototypeReduce, TypedArrayPrototypeReduceRight,
  TypedArrayPrototypeReverse, TypedArrayPrototypeSet, TypedArrayPrototypeSlice,
  TypedArrayPrototypeSome, TypedArrayPrototypeSort, TypedArrayPrototypeSubarray,
  TypedArrayPrototypeToLocaleString, TypedArrayPrototypeToReversed,
  TypedArrayPrototypeToSorted, TypedArrayPrototypeToString,
  TypedArrayPrototypeValues, TypedArrayPrototypeWith,
}
import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Hard cap on a typed array's backing byte length (matches the engine's
/// practical allocation limit; over this → RangeError like real engines).
const max_byte_length = 2_147_483_647

/// 2^53 - 1 — MAX_SAFE_INTEGER, the ToIndex/ToLength upper bound.
const max_safe_integer = 9_007_199_254_740_991

// ============================================================================
// Init — %TypedArray%, %TypedArray%.prototype, and the 11 concrete ctors
// ============================================================================

pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType, List(#(TypedArrayKind, BuiltinType))) {
  // Accessor getters on %TypedArray%.prototype (§23.2.3.1-3, .18).
  let #(h, getters) =
    common.alloc_getters(h, function_proto, [
      #("buffer", TypedArrayNative(TypedArrayGetBuffer)),
      #("byteLength", TypedArrayNative(TypedArrayGetByteLength)),
      #("byteOffset", TypedArrayNative(TypedArrayGetByteOffset)),
      #("length", TypedArrayNative(TypedArrayGetLength)),
    ])
  // values() doubles as [@@iterator] — must be the SAME function object.
  let #(h, values_ref) =
    common.alloc_native_fn(
      h,
      function_proto,
      TypedArrayNative(TypedArrayPrototypeValues),
      "values",
      0,
    )
  let values_prop = value.builtin_property(JsObject(values_ref))
  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("at", TypedArrayNative(TypedArrayPrototypeAt), 1),
      #("fill", TypedArrayNative(TypedArrayPrototypeFill), 1),
      #("set", TypedArrayNative(TypedArrayPrototypeSet), 1),
      #("subarray", TypedArrayNative(TypedArrayPrototypeSubarray), 2),
      #("slice", TypedArrayNative(TypedArrayPrototypeSlice), 2),
      #("join", TypedArrayNative(TypedArrayPrototypeJoin), 1),
      #("indexOf", TypedArrayNative(TypedArrayPrototypeIndexOf), 1),
      #("includes", TypedArrayNative(TypedArrayPrototypeIncludes), 1),
      #("keys", TypedArrayNative(TypedArrayPrototypeKeys), 0),
      #("entries", TypedArrayNative(TypedArrayPrototypeEntries), 0),
      #("toString", TypedArrayNative(TypedArrayPrototypeToString), 0),
      #("copyWithin", TypedArrayNative(TypedArrayPrototypeCopyWithin), 2),
      #("every", TypedArrayNative(TypedArrayPrototypeEvery), 1),
      #("some", TypedArrayNative(TypedArrayPrototypeSome), 1),
      #("forEach", TypedArrayNative(TypedArrayPrototypeForEach), 1),
      #("map", TypedArrayNative(TypedArrayPrototypeMap), 1),
      #("filter", TypedArrayNative(TypedArrayPrototypeFilter), 1),
      #("find", TypedArrayNative(TypedArrayPrototypeFind), 1),
      #("findIndex", TypedArrayNative(TypedArrayPrototypeFindIndex), 1),
      #("findLast", TypedArrayNative(TypedArrayPrototypeFindLast), 1),
      #("findLastIndex", TypedArrayNative(TypedArrayPrototypeFindLastIndex), 1),
      #("lastIndexOf", TypedArrayNative(TypedArrayPrototypeLastIndexOf), 1),
      #("reduce", TypedArrayNative(TypedArrayPrototypeReduce), 1),
      #("reduceRight", TypedArrayNative(TypedArrayPrototypeReduceRight), 1),
      #("reverse", TypedArrayNative(TypedArrayPrototypeReverse), 0),
      #("toReversed", TypedArrayNative(TypedArrayPrototypeToReversed), 0),
      #("sort", TypedArrayNative(TypedArrayPrototypeSort), 1),
      #("toSorted", TypedArrayNative(TypedArrayPrototypeToSorted), 1),
      #(
        "toLocaleString",
        TypedArrayNative(TypedArrayPrototypeToLocaleString),
        0,
      ),
      #("with", TypedArrayNative(TypedArrayPrototypeWith), 2),
    ])
  let proto_props = list.flatten([getters, [#("values", values_prop)], methods])
  let #(h, ta) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_props,
      fn(_proto) { Dispatch(TypedArrayNative(TypedArrayIntrinsicConstructor)) },
      "TypedArray",
      0,
      [],
    )
  // %TypedArray%.prototype[@@iterator] === %TypedArray%.prototype.values
  let h =
    common.add_symbol_property(
      h,
      ta.prototype,
      value.symbol_iterator,
      values_prop,
    )
  // get %TypedArray%.prototype[@@toStringTag] (§23.2.3.38) — an accessor that
  // returns the [[TypedArrayName]] (undefined for non-TypedArray receivers).
  let #(h, tag_get) =
    common.alloc_native_fn(
      h,
      function_proto,
      TypedArrayNative(TypedArrayGetToStringTag),
      "get [Symbol.toStringTag]",
      0,
    )
  let h =
    common.add_symbol_property(
      h,
      ta.prototype,
      value.symbol_to_string_tag,
      value.accessor(
        get: Some(JsObject(tag_get)),
        set: None,
        enumerable: False,
        configurable: True,
      ),
    )
  // get %TypedArray%[@@species] (§23.2.2.4) — returns `this`.
  let #(h, species_get) =
    common.alloc_native_fn(
      h,
      function_proto,
      TypedArrayNative(TypedArrayGetSpecies),
      "get [Symbol.species]",
      0,
    )
  let h =
    common.add_symbol_property(
      h,
      ta.constructor,
      value.symbol_species,
      value.accessor(
        get: Some(JsObject(species_get)),
        set: None,
        enumerable: False,
        configurable: True,
      ),
    )
  // The 11 concrete constructors. Each ctor's [[Prototype]] is %TypedArray%
  // itself, and each prototype's [[Prototype]] is %TypedArray%.prototype
  // (§23.2.5/§23.2.6/§23.2.7). BYTES_PER_ELEMENT is {W:F, E:F, C:F} on both.
  let #(h, ctors_rev) =
    list.fold(value.all_typed_array_kinds, #(h, []), fn(acc, kind) {
      let #(h, lst) = acc
      let size = value.typed_array_element_size(kind)
      let size_prop = #("BYTES_PER_ELEMENT", value.data(value.from_int(size)))
      let #(h, bt) =
        common.init_type(
          h,
          ta.prototype,
          ta.constructor,
          [size_prop],
          fn(proto) {
            Dispatch(TypedArrayNative(TypedArrayConstructor(kind:, proto:)))
          },
          value.typed_array_name(kind),
          3,
          [size_prop],
        )
      // §23.2.6.2: the ctor's "prototype" property is {W:F, E:F, C:F}.
      let h = freeze_prototype_prop(h, bt.constructor, bt.prototype)
      // proposal-arraybuffer-base64: own methods of Uint8Array.prototype and
      // statics of the Uint8Array constructor (NOT on %TypedArray%).
      let h = case kind {
        value.Uint8Kind -> {
          let #(h, u8_methods) =
            common.alloc_methods(h, function_proto, [
              #(
                "toBase64",
                TypedArrayNative(value.Uint8ArrayPrototypeToBase64),
                0,
              ),
              #("toHex", TypedArrayNative(value.Uint8ArrayPrototypeToHex), 0),
              #(
                "setFromBase64",
                TypedArrayNative(value.Uint8ArrayPrototypeSetFromBase64),
                1,
              ),
              #(
                "setFromHex",
                TypedArrayNative(value.Uint8ArrayPrototypeSetFromHex),
                1,
              ),
            ])
          let h = add_named_props(h, bt.prototype, u8_methods)
          let #(h, u8_statics) =
            common.alloc_methods(h, function_proto, [
              #("fromBase64", TypedArrayNative(value.Uint8ArrayFromBase64), 1),
              #("fromHex", TypedArrayNative(value.Uint8ArrayFromHex), 1),
            ])
          add_named_props(h, bt.constructor, u8_statics)
        }
        _ -> h
      }
      #(h, [#(kind, bt), ..lst])
    })
  let h = freeze_prototype_prop(h, ta.constructor, ta.prototype)
  // %TypedArray%.from / %TypedArray%.of — statics inherited by all 11 ctors.
  let #(h, statics) =
    common.alloc_methods(h, function_proto, [
      #("from", TypedArrayNative(value.TypedArrayFrom), 1),
      #("of", TypedArrayNative(value.TypedArrayOf), 0),
    ])
  let h = add_named_props(h, ta.constructor, statics)
  #(h, ta, list.reverse(ctors_rev))
}

/// Insert named properties into an existing object slot.
fn add_named_props(
  h: Heap(host),
  ref: Ref,
  props: List(#(String, value.Property)),
) -> Heap(host) {
  heap.update(h, ref, fn(slot) {
    case slot {
      ObjectSlot(properties:, ..) as s ->
        ObjectSlot(
          ..s,
          properties: list.fold(props, properties, fn(acc, entry) {
            let #(name, prop) = entry
            dict.insert(acc, Named(name), prop)
          }),
        )
      other -> other
    }
  })
}

/// Rewrite a constructor's "prototype" property to the spec attributes for
/// TypedArray constructors: non-writable, non-enumerable, non-configurable.
fn freeze_prototype_prop(h: Heap(host), ctor: Ref, proto: Ref) -> Heap(host) {
  heap.update(h, ctor, fn(slot) {
    case slot {
      ObjectSlot(properties:, ..) as s ->
        ObjectSlot(
          ..s,
          properties: dict.insert(
            properties,
            Named("prototype"),
            value.data(JsObject(proto)),
          ),
        )
      other -> other
    }
  })
}

// ============================================================================
// Dispatch
// ============================================================================

pub fn dispatch(
  native: TypedArrayNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    // §23.2.1.1 %TypedArray%: always throws.
    TypedArrayIntrinsicConstructor ->
      state.type_error(
        state,
        "Abstract class TypedArray not directly constructable",
      )
    TypedArrayConstructor(kind:, proto:) ->
      ta_construct(kind, proto, args, state)
    TypedArrayGetBuffer -> get_buffer(this, state)
    TypedArrayGetByteLength -> get_byte_length(this, state)
    TypedArrayGetByteOffset -> get_byte_offset(this, state)
    TypedArrayGetLength -> get_length(this, state)
    TypedArrayGetToStringTag -> get_to_string_tag(this, state)
    TypedArrayGetSpecies -> #(state, Ok(this))
    TypedArrayPrototypeAt -> proto_at(this, args, state)
    TypedArrayPrototypeFill -> proto_fill(this, args, state)
    TypedArrayPrototypeSet -> proto_set(this, args, state)
    TypedArrayPrototypeSubarray -> proto_subarray(this, args, state)
    TypedArrayPrototypeSlice -> proto_slice(this, args, state)
    TypedArrayPrototypeJoin -> proto_join(this, args, state)
    TypedArrayPrototypeIndexOf -> proto_index_of(this, args, state)
    TypedArrayPrototypeIncludes -> proto_includes(this, args, state)
    TypedArrayPrototypeKeys -> proto_iter(this, state, IterKeys)
    TypedArrayPrototypeValues -> proto_iter(this, state, IterValues)
    TypedArrayPrototypeEntries -> proto_iter(this, state, IterEntries)
    TypedArrayPrototypeToString -> proto_join(this, [JsUndefined], state)
    TypedArrayPrototypeCopyWithin -> proto_copy_within(this, args, state)
    TypedArrayPrototypeEvery -> proto_every_some(this, args, state, True)
    TypedArrayPrototypeSome -> proto_every_some(this, args, state, False)
    TypedArrayPrototypeForEach -> proto_for_each(this, args, state)
    TypedArrayPrototypeMap -> proto_map(this, args, state)
    TypedArrayPrototypeFilter -> proto_filter(this, args, state)
    TypedArrayPrototypeFind -> proto_find(this, args, state, 1, FindValue)
    TypedArrayPrototypeFindIndex -> proto_find(this, args, state, 1, FindIdx)
    TypedArrayPrototypeFindLast -> proto_find(this, args, state, -1, FindValue)
    TypedArrayPrototypeFindLastIndex ->
      proto_find(this, args, state, -1, FindIdx)
    TypedArrayPrototypeLastIndexOf -> proto_last_index_of(this, args, state)
    TypedArrayPrototypeReduce -> proto_reduce(this, args, state, 1)
    TypedArrayPrototypeReduceRight -> proto_reduce(this, args, state, -1)
    TypedArrayPrototypeReverse -> proto_reverse(this, state)
    TypedArrayPrototypeToReversed -> proto_to_reversed(this, state)
    TypedArrayPrototypeSort -> proto_sort(this, args, state)
    TypedArrayPrototypeToSorted -> proto_to_sorted(this, args, state)
    TypedArrayPrototypeToLocaleString -> proto_to_locale_string(this, state)
    TypedArrayPrototypeWith -> proto_with(this, args, state)
    value.TypedArrayFrom -> ta_from(this, args, state)
    value.TypedArrayOf -> ta_of(this, args, state)
    value.Uint8ArrayPrototypeToBase64 -> u8_to_base64(this, args, state)
    value.Uint8ArrayPrototypeToHex -> u8_to_hex(this, state)
    value.Uint8ArrayPrototypeSetFromBase64 ->
      u8_set_from_base64(this, args, state)
    value.Uint8ArrayPrototypeSetFromHex -> u8_set_from_hex(this, args, state)
    value.Uint8ArrayFromBase64 -> u8_from_base64(args, state)
    value.Uint8ArrayFromHex -> u8_from_hex(args, state)
  }
}

// ============================================================================
// %TypedArray%.from / %TypedArray%.of — §23.2.2.1 / §23.2.2.2
// ============================================================================

/// TypedArrayCreate (§23.2.4.2): Construct(ctor, [len]), then require the
/// result to be a TypedArray of at least `len` elements.
/// Every caller of this «len» form (of/from/slice/map/filter) WRITES into
/// the result, so per the immutable-arraybuffer proposal's
/// TypedArrayCreateFromConstructor(.., ~write~) an instance backed by an
/// immutable buffer is a TypeError. (subarray — the only read-mode species
/// creator — uses ta_create_with_args directly and skips this check.)
fn ta_create(
  state: State(host),
  ctor: JsValue,
  len: Int,
) -> Result(#(JsValue, Ref, State(host)), #(JsValue, State(host))) {
  use #(obj, obj_ref, state) <- result.try(ta_create_with_args(
    state,
    ctor,
    [value.from_int(len)],
    Some(len),
  ))
  case ta_buffer_immutable(state.heap, obj_ref) {
    True ->
      Error(state.type_error_value(
        state,
        "Constructor returned a TypedArray backed by an immutable ArrayBuffer",
      ))
    False -> Ok(#(obj, obj_ref, state))
  }
}

/// TypedArrayCreateFromConstructor (§23.2.4.2) with an arbitrary argument
/// list. `min_len` enables the single-Number-argument length check.
fn ta_create_with_args(
  state: State(host),
  ctor: JsValue,
  ctor_args: List(JsValue),
  min_len: Option(Int),
) -> Result(#(JsValue, Ref, State(host)), #(JsValue, State(host))) {
  use #(obj, state) <- result.try(state.construct(state, ctor, ctor_args))
  case obj {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: value.TypedArrayObject(
            buffer:,
            elem_kind:,
            byte_offset:,
            length:,
          ),
          ..,
        )) ->
          // Step 2: ValidateTypedArray(newTypedArray) — a constructor that
          // returned a view over a detached buffer (or one that no longer
          // fits its resizable buffer) throws TypeError.
          case
            view_witness_error(
              state.heap,
              TaView(buffer:, kind: elem_kind, byte_offset:, length:),
            )
          {
            Some(err) ->
              Error(state.type_error_value(state, witness_message(err)))
            None -> {
              let l =
                object.typed_array_view_length(
                  state.heap,
                  buffer,
                  elem_kind,
                  byte_offset,
                  length,
                )
              case min_len {
                Some(len) if l < len ->
                  Error(state.type_error_value(
                    state,
                    "Derived TypedArray constructor created an array which was too small",
                  ))
                _ -> Ok(#(obj, ref, state))
              }
            }
          }
        _ ->
          Error(state.type_error_value(state, witness_message(NotTypedArray)))
      }
    _ -> Error(state.type_error_value(state, witness_message(NotTypedArray)))
  }
}

/// §23.2.2.1 %TypedArray%.from ( source [ , mapfn [ , thisArg ] ] )
fn ta_from(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use <- bool.lazy_guard(!object.is_constructor(state.heap, this), fn() {
    state.type_error(state, "%TypedArray%.from called on non-constructor")
  })
  let source = helpers.first_arg_or_undefined(args)
  let mapfn = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
  let this_arg = helpers.list_at(args, 2) |> option.unwrap(JsUndefined)
  // Step 3: mapping check.
  use mapping, state <- try_state(case mapfn {
    JsUndefined -> Ok(#(None, state))
    _ ->
      case helpers.is_callable(state.heap, mapfn) {
        True -> Ok(#(Some(mapfn), state))
        False -> Error(state.type_error_value(state, "mapfn is not a function"))
      }
  })
  use <- bool.lazy_guard(source == JsUndefined || source == value.JsNull, fn() {
    state.type_error(state, "Cannot convert undefined or null to object")
  })
  // Step 5: usingIterator = GetMethod(source, @@iterator).
  use iter_fn, state <- try_state(object.get_symbol_value_of(
    state,
    source,
    value.symbol_iterator,
  ))
  case helpers.is_callable(state.heap, iter_fn) {
    True ->
      wrap({
        use #(values, state) <- result.try(iterate_to_list(
          state,
          source,
          iter_fn,
        ))
        use #(target, target_ref, state) <- result.try(ta_create(
          state,
          this,
          list.length(values),
        ))
        let bulk = case mapping {
          None -> try_bulk_store(state, target, 0, values)
          Some(_) -> None
        }
        case bulk {
          Some(state) -> Ok(#(target, state))
          None ->
            from_store_loop(
              state,
              target,
              target_ref,
              values,
              0,
              mapping,
              this_arg,
            )
        }
      })
    False ->
      wrap({
        // Step 7: array-like path (works on primitives like strings too).
        use #(len_val, state) <- result.try(object.get_value_of(
          state,
          source,
          Named("length"),
        ))
        use #(len, state) <- result.try(to_length(state, len_val))
        use #(target, target_ref, state) <- result.try(ta_create(
          state,
          this,
          len,
        ))
        let bulk = case mapping, source {
          None, JsObject(src_ref) ->
            typed_array_elements.plain_indexed_values(state.heap, src_ref, len)
            |> option.then(try_bulk_store(state, target, 0, _))
          _, _ -> None
        }
        case bulk {
          Some(state) -> Ok(#(target, state))
          None ->
            from_array_like_loop(
              state,
              target,
              target_ref,
              source,
              0,
              len,
              mapping,
              this_arg,
            )
        }
      })
  }
}

fn from_store_loop(
  state: State(host),
  target: JsValue,
  target_ref: Ref,
  values: List(JsValue),
  k: Int,
  mapping: option.Option(JsValue),
  this_arg: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case values {
    [] -> Ok(#(target, state))
    [v, ..rest] -> {
      use state <- result.try(map_and_store(
        state,
        target,
        target_ref,
        v,
        k,
        mapping,
        this_arg,
      ))
      from_store_loop(state, target, target_ref, rest, k + 1, mapping, this_arg)
    }
  }
}

fn from_array_like_loop(
  state: State(host),
  target: JsValue,
  target_ref: Ref,
  source: JsValue,
  k: Int,
  len: Int,
  mapping: option.Option(JsValue),
  this_arg: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case k >= len {
    True -> Ok(#(target, state))
    False -> {
      use #(v, state) <- result.try(object.get_value_of(state, source, Index(k)))
      use state <- result.try(map_and_store(
        state,
        target,
        target_ref,
        v,
        k,
        mapping,
        this_arg,
      ))
      from_array_like_loop(
        state,
        target,
        target_ref,
        source,
        k + 1,
        len,
        mapping,
        this_arg,
      )
    }
  }
}

/// Shared from() element step: apply the optional mapfn, then store at k.
fn map_and_store(
  state: State(host),
  target: JsValue,
  target_ref: Ref,
  v: JsValue,
  k: Int,
  mapping: option.Option(JsValue),
  this_arg: JsValue,
) -> Result(State(host), #(JsValue, State(host))) {
  use #(mapped, state) <- result.try(case mapping {
    Some(f) -> state.call(state, f, this_arg, [v, value.from_int(k)])
    None -> Ok(#(v, state))
  })
  use #(state, _) <- result.map(object.set_value(
    state,
    target_ref,
    Index(k),
    mapped,
    target,
  ))
  state
}

/// §23.2.2.2 %TypedArray%.of ( ...items )
fn ta_of(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use <- bool.lazy_guard(!object.is_constructor(state.heap, this), fn() {
    state.type_error(state, "%TypedArray%.of called on non-constructor")
  })
  wrap({
    use #(target, target_ref, state) <- result.try(ta_create(
      state,
      this,
      list.length(args),
    ))
    case try_bulk_store(state, target, 0, args) {
      Some(state) -> Ok(#(target, state))
      None ->
        from_store_loop(state, target, target_ref, args, 0, None, JsUndefined)
    }
  })
}

/// Adapt internal Result style to the builtin dispatch tuple shape.
fn wrap(
  r: Result(#(JsValue, State(host)), #(JsValue, State(host))),
) -> #(State(host), Result(JsValue, JsValue)) {
  case r {
    Ok(#(v, state)) -> #(state, Ok(v))
    Error(#(e, state)) -> #(state, Error(e))
  }
}

// ============================================================================
// Constructor — §23.2.5.1 TypedArray ( ...args )
// ============================================================================

fn ta_construct(
  kind: TypedArrayKind,
  proto: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: If NewTarget is undefined, throw a TypeError.
  use <- bool.lazy_guard(state.new_target == JsUndefined, fn() {
    state.type_error(
      state,
      "Constructor " <> value.typed_array_name(kind) <> " requires 'new'",
    )
  })
  case args {
    // Step 4: no args → AllocateTypedArray(0).
    [] -> wrap(alloc_ta_with_length(state, kind, proto, 0))
    [first, ..rest] ->
      case first {
        JsObject(ref) ->
          case heap.read(state.heap, ref) {
            Some(ObjectSlot(kind: value.ArrayBufferObject(..), ..)) ->
              wrap(from_buffer(state, kind, proto, ref, rest))
            Some(ObjectSlot(
              kind: value.TypedArrayObject(
                buffer: src_buf,
                elem_kind: src_kind,
                byte_offset: src_off,
                length: src_len,
              ),
              ..,
            )) ->
              wrap(from_typed_array(
                state,
                kind,
                proto,
                src_buf,
                src_kind,
                src_off,
                object.typed_array_view_length(
                  state.heap,
                  src_buf,
                  src_kind,
                  src_off,
                  src_len,
                ),
              ))
            _ -> wrap(from_object(state, kind, proto, first, ref))
          }
        // Step 6.b: not an object → AllocateTypedArray(ToIndex(arg)).
        _ ->
          wrap({
            use #(len, state) <- result.try(to_index(state, first))
            alloc_ta_with_length(state, kind, proto, len)
          })
      }
  }
}

/// AllocateTypedArray + AllocateTypedArrayBuffer (§23.2.5.1.1/.6): fresh
/// zeroed buffer of `len` elements viewed from offset 0.
fn alloc_ta_with_length(
  state: State(host),
  kind: TypedArrayKind,
  proto: Ref,
  len: Int,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  let size = value.typed_array_element_size(kind)
  let byte_len = len * size
  use <- bool.lazy_guard(byte_len > max_byte_length, fn() {
    Error(state.range_error_value(state, "Invalid typed array length"))
  })
  Ok(alloc_fresh_ta(state, kind, proto, ta_zeroed(byte_len), len))
}

/// Allocate a fresh non-resizable ArrayBuffer holding `data` plus a fixed
/// `len`-element view over it from offset 0.
fn alloc_fresh_ta(
  state: State(host),
  kind: TypedArrayKind,
  proto: Ref,
  data: BitArray,
  len: Int,
) -> #(JsValue, State(host)) {
  let #(h, buf) =
    common.alloc_wrapper(
      state.heap,
      value.ArrayBufferObject(
        data: value.BufBytes(data),
        detached: False,
        max_byte_length: None,
        immutable: False,
      ),
      state.builtins.array_buffer.prototype,
    )
  let #(h, ta_ref) =
    common.alloc_wrapper(
      h,
      value.TypedArrayObject(
        buffer: buf,
        elem_kind: kind,
        byte_offset: 0,
        length: Some(len),
      ),
      proto,
    )
  #(JsObject(ta_ref), State(..state, heap: h))
}

/// §23.2.5.1.3 InitializeTypedArrayFromArrayBuffer.
fn from_buffer(
  state: State(host),
  kind: TypedArrayKind,
  proto: Ref,
  buf_ref: Ref,
  rest: List(JsValue),
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  let size = value.typed_array_element_size(kind)
  let offset_arg = helpers.first_arg_or_undefined(rest)
  let len_arg = helpers.list_at(rest, 1) |> option.unwrap(JsUndefined)
  // Step 2: offset = ToIndex(byteOffset).
  use #(offset, state) <- result.try(to_index(state, offset_arg))
  // Step 3: offset modulo elementSize must be 0.
  use <- bool.lazy_guard(offset % size != 0, fn() {
    Error(state.range_error_value(
      state,
      "start offset of "
        <> value.typed_array_name(kind)
        <> " should be a multiple of "
        <> int.to_string(size),
    ))
  })
  // Step 5: newLength = ToIndex(length) when present.
  use #(new_len, state) <- result.try(case len_arg {
    JsUndefined -> Ok(#(None, state))
    _ -> {
      use #(l, state) <- result.map(to_index(state, len_arg))
      #(Some(l), state)
    }
  })
  // Step 6: detached check AFTER the (observable) conversions.
  case object.typed_array_buffer_data(state.heap, buf_ref) {
    None ->
      Error(state.type_error_value(
        state,
        "Cannot perform Construct on a detached ArrayBuffer",
      ))
    Some(data) -> {
      let buf_len = bit_array.byte_size(data)
      let range_err = fn(msg) { Error(state.range_error_value(state, msg)) }
      let resizable = case heap.read(state.heap, buf_ref) {
        Some(ObjectSlot(
          kind: value.ArrayBufferObject(max_byte_length: Some(_), ..),
          ..,
        )) -> True
        _ -> False
      }
      case new_len {
        // Step 7: length absent over a RESIZABLE buffer — length-tracking
        // view ([[ArrayLength]] = AUTO); only the offset is validated, the
        // buffer's byte length need not divide evenly.
        None if resizable ->
          case offset > buf_len {
            True -> range_err("Invalid typed array offset")
            False -> alloc_ta_view(state, kind, proto, buf_ref, offset, None)
          }
        // Step 8.a: length auto-derived — buffer must divide evenly.
        None ->
          case buf_len % size != 0 {
            True ->
              range_err(
                "byte length of "
                <> value.typed_array_name(kind)
                <> " should be a multiple of "
                <> int.to_string(size),
              )
            False ->
              case buf_len - offset < 0 {
                True -> range_err("Invalid typed array length")
                False ->
                  alloc_ta_view(
                    state,
                    kind,
                    proto,
                    buf_ref,
                    offset,
                    Some({ buf_len - offset } / size),
                  )
              }
          }
        // Step 9: explicit length — view must fit inside the buffer.
        Some(l) ->
          case offset + l * size > buf_len {
            True -> range_err("Invalid typed array length")
            False -> alloc_ta_view(state, kind, proto, buf_ref, offset, Some(l))
          }
      }
    }
  }
}

/// Allocate a TypedArray view over an EXISTING buffer. `len: None` is a
/// length-tracking view ([[ArrayLength]] = AUTO) over a resizable buffer.
fn alloc_ta_view(
  state: State(host),
  kind: TypedArrayKind,
  proto: Ref,
  buf_ref: Ref,
  byte_offset: Int,
  len: Option(Int),
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  let #(h, ta_ref) =
    common.alloc_wrapper(
      state.heap,
      value.TypedArrayObject(
        buffer: buf_ref,
        elem_kind: kind,
        byte_offset:,
        length: len,
      ),
      proto,
    )
  Ok(#(JsObject(ta_ref), State(..state, heap: h)))
}

/// §23.2.5.1.2 InitializeTypedArrayFromTypedArray.
fn from_typed_array(
  state: State(host),
  kind: TypedArrayKind,
  proto: Ref,
  src_buf: Ref,
  src_kind: TypedArrayKind,
  src_off: Int,
  src_len: Int,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  // Step 6.c: BigInt and Number content types never mix.
  use <- bool.lazy_guard(
    value.typed_array_is_bigint(kind) != value.typed_array_is_bigint(src_kind),
    fn() {
      Error(state.type_error_value(
        state,
        "Cannot initialize "
          <> value.typed_array_name(kind)
          <> " from "
          <> value.typed_array_name(src_kind),
      ))
    },
  )
  case object.typed_array_buffer_data(state.heap, src_buf) {
    None ->
      Error(state.type_error_value(
        state,
        "Cannot perform Construct on a detached ArrayBuffer",
      ))
    Some(src_data) -> {
      let size = value.typed_array_element_size(kind)
      let byte_len = src_len * size
      use <- bool.lazy_guard(byte_len > max_byte_length, fn() {
        Error(state.range_error_value(state, "Invalid typed array length"))
      })
      // §23.2.5.1.2 step 5 (MakeTypedArrayWithBufferWitnessRecord +
      // IsTypedArrayOutOfBounds): a source view whose resizable buffer has
      // shrunk below it behaves like detached → TypeError.
      let src_size = value.typed_array_element_size(src_kind)
      use <- bool.lazy_guard(
        src_off + src_len * src_size > bit_array.byte_size(src_data),
        fn() {
          Error(state.type_error_value(
            state,
            "Cannot perform Construct on an out-of-bounds TypedArray",
          ))
        },
      )
      let new_data = case kind == src_kind {
        // Same element type: raw byte copy (in range per the guard above).
        True ->
          case bit_array.slice(src_data, src_off, byte_len) {
            Ok(bytes) -> bytes
            Error(Nil) -> ta_zeroed(byte_len)
          }
        // Different element type: element-wise convert (no user code runs).
        False ->
          convert_elements(
            state.heap,
            src_buf,
            src_kind,
            src_off,
            src_len,
            kind,
            size,
          )
      }
      Ok(alloc_fresh_ta(state, kind, proto, new_data, src_len))
    }
  }
}

/// Element-wise conversion into a fresh buffer — single pass: each element
/// is encoded into its own small binary and the segments are concatenated
/// ONCE at the end (O(n), instead of re-splicing the full-size accumulator
/// per element, which was O(n²)).
fn convert_elements(
  h: Heap(host),
  src_buf: Ref,
  src_kind: TypedArrayKind,
  src_off: Int,
  src_len: Int,
  dst_kind: TypedArrayKind,
  dst_size: Int,
) -> BitArray {
  convert_elements_loop(
    h,
    src_buf,
    src_kind,
    src_off,
    src_len,
    dst_kind,
    dst_size,
    0,
    [],
  )
}

fn convert_elements_loop(
  h: Heap(host),
  src_buf: Ref,
  src_kind: TypedArrayKind,
  src_off: Int,
  src_len: Int,
  dst_kind: TypedArrayKind,
  dst_size: Int,
  i: Int,
  acc: List(BitArray),
) -> BitArray {
  case i >= src_len {
    True -> bit_array.concat(list.reverse(acc))
    False -> {
      // Elements read back out of a typed array are always JsNumber /
      // JsBigInt; a missing element (out-of-bounds read) encodes as zero.
      let seg = case
        object.typed_array_element(h, src_buf, src_kind, src_off, src_len, i)
        |> option.then(typed_array_elements.decoded_element)
      {
        Some(el) ->
          typed_array_elements.typed_array_encode_value(
            ta_zeroed(dst_size),
            0,
            dst_kind,
            el,
          )
        None -> ta_zeroed(dst_size)
      }
      convert_elements_loop(
        h,
        src_buf,
        src_kind,
        src_off,
        src_len,
        dst_kind,
        dst_size,
        i + 1,
        [seg, ..acc],
      )
    }
  }
}

/// §23.2.5.1.4/.5 InitializeTypedArrayFromList / FromArrayLike:
/// use the @@iterator when callable, else the array-like protocol.
fn from_object(
  state: State(host),
  kind: TypedArrayKind,
  proto: Ref,
  obj_val: JsValue,
  obj_ref: Ref,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  use #(iter_fn, state) <- result.try(object.get_symbol_value_of(
    state,
    obj_val,
    value.symbol_iterator,
  ))
  case helpers.is_callable(state.heap, iter_fn) {
    True -> {
      use #(values, state) <- result.try(iterate_to_list(
        state,
        obj_val,
        iter_fn,
      ))
      use #(ta_val, state) <- result.try(alloc_ta_with_length(
        state,
        kind,
        proto,
        list.length(values),
      ))
      case try_bulk_store(state, ta_val, 0, values) {
        Some(state) -> Ok(#(ta_val, state))
        None -> store_list(state, ta_val, values, 0)
      }
    }
    False -> {
      // Array-like: len = ToLength(Get(obj, "length")).
      use #(len_val, state) <- result.try(object.get_value(
        state,
        obj_ref,
        Named("length"),
        obj_val,
      ))
      use #(len, state) <- result.try(to_length(state, len_val))
      use #(ta_val, state) <- result.try(alloc_ta_with_length(
        state,
        kind,
        proto,
        len,
      ))
      let bulk =
        typed_array_elements.plain_indexed_values(state.heap, obj_ref, len)
        |> option.then(try_bulk_store(state, ta_val, 0, _))
      case bulk {
        Some(state) -> Ok(#(ta_val, state))
        None -> store_array_like(state, ta_val, obj_ref, obj_val, 0, len)
      }
    }
  }
}

/// Run the source's iterator to completion, collecting yielded values.
fn iterate_to_list(
  state: State(host),
  obj: JsValue,
  iter_fn: JsValue,
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  use #(iter, state) <- result.try(state.call(state, iter_fn, obj, []))
  case iter {
    JsObject(iter_ref) -> {
      use #(next_fn, state) <- result.try(object.get_value(
        state,
        iter_ref,
        Named("next"),
        iter,
      ))
      iterate_loop(state, iter, next_fn, [])
    }
    _ ->
      Error(state.type_error_value(
        state,
        "Result of the Symbol.iterator method is not an object",
      ))
  }
}

fn iterate_loop(
  state: State(host),
  iter: JsValue,
  next_fn: JsValue,
  acc: List(JsValue),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  use #(res, state) <- result.try(state.call(state, next_fn, iter, []))
  case res {
    JsObject(res_ref) -> {
      use #(done, state) <- result.try(object.get_value(
        state,
        res_ref,
        Named("done"),
        res,
      ))
      case value.is_truthy(done) {
        True -> Ok(#(list.reverse(acc), state))
        False -> {
          use #(v, state) <- result.try(object.get_value(
            state,
            res_ref,
            Named("value"),
            res,
          ))
          iterate_loop(state, iter, next_fn, [v, ..acc])
        }
      }
    }
    _ ->
      Error(state.type_error_value(state, "Iterator result is not an object"))
  }
}

/// Store collected values into a fresh typed array via the exotic [[Set]]
/// path (per-element coercion, may run user code).
fn store_list(
  state: State(host),
  ta_val: JsValue,
  values: List(JsValue),
  idx: Int,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case values {
    [] -> Ok(#(ta_val, state))
    [v, ..rest] ->
      case ta_val {
        JsObject(ta_ref) -> {
          use #(state, _ok) <- result.try(object.set_value(
            state,
            ta_ref,
            Index(idx),
            v,
            ta_val,
          ))
          store_list(state, ta_val, rest, idx + 1)
        }
        _ -> Ok(#(ta_val, state))
      }
  }
}

fn store_array_like(
  state: State(host),
  ta_val: JsValue,
  obj_ref: Ref,
  obj_val: JsValue,
  k: Int,
  len: Int,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case k >= len {
    True -> Ok(#(ta_val, state))
    False -> {
      use #(v, state) <- result.try(object.get_value(
        state,
        obj_ref,
        Index(k),
        obj_val,
      ))
      case ta_val {
        JsObject(ta_ref) -> {
          use #(state, _ok) <- result.try(object.set_value(
            state,
            ta_ref,
            Index(k),
            v,
            ta_val,
          ))
          store_array_like(state, ta_val, obj_ref, obj_val, k + 1, len)
        }
        _ -> Ok(#(ta_val, state))
      }
    }
  }
}

// ============================================================================
// Conversions
// ============================================================================

/// §7.1.22 ToIndex with this module's RangeError message. The algorithm
/// lives in `coerce.to_index`.
fn to_index(
  state: State(host),
  val: JsValue,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  coerce.to_index(state, val, "Invalid typed array length")
}

/// §7.1.20 ToLength: ToIntegerOrInfinity clamped to [0, 2^53-1], no throw.
/// The clamp itself is `coerce.jsnum_to_length`.
fn to_length(
  state: State(host),
  val: JsValue,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  use #(n, state) <- result.map(coerce.js_to_number(state, val))
  #(coerce.jsnum_to_length(n), state)
}

/// ToIntegerOrInfinity, with infinities preserved as sentinels.
type IntOrInf {
  IInt(Int)
  IPosInf
  INegInf
}

fn to_int_or_inf(
  state: State(host),
  val: JsValue,
) -> Result(#(IntOrInf, State(host)), #(JsValue, State(host))) {
  use #(n, state) <- result.map(coerce.js_to_number(state, val))
  let i = case n {
    value.NaN -> IInt(0)
    Finite(f) -> IInt(value.float_to_int(f))
    value.Infinity -> IPosInf
    value.NegInfinity -> INegInf
  }
  #(i, state)
}

/// Relative start/end index resolution (§23.2.3.x shared steps):
/// negative counts from the end, clamped to [0, length].
fn relative_index(i: IntOrInf, length: Int) -> Int {
  case i {
    IPosInf -> length
    INegInf -> 0
    IInt(n) ->
      case n < 0 {
        True -> int.max(length + n, 0)
        False -> int.min(n, length)
      }
  }
}

// ============================================================================
// Receiver validation
// ============================================================================

/// A TypedArray's internal slots AS DECLARED on the object:
/// [[ViewedArrayBuffer]], the element kind ([[TypedArrayName]]),
/// [[ByteOffset]], and the declared [[ArrayLength]] — `None` for
/// length-tracking (AUTO) views, which follow the live buffer size.
pub type TaView {
  TaView(
    buffer: Ref,
    kind: TypedArrayKind,
    byte_offset: Int,
    length: Option(Int),
  )
}

/// The view a require_ta / validate_ta continuation receives: same slots,
/// but with [[ArrayLength]] RESOLVED to the current element count (AUTO
/// views included), so downstream bounds checks see a plain Int.
pub type TaWitness {
  TaWitness(buffer: Ref, kind: TypedArrayKind, byte_offset: Int, length: Int)
}

/// `v`'s TypedArray internal slots when `v` is a TypedArray object, None
/// otherwise.
fn ta_slot(h: Heap(host), v: JsValue) -> Option(TaView) {
  case v {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(
          kind: value.TypedArrayObject(
            buffer:,
            elem_kind:,
            byte_offset:,
            length:,
          ),
          ..,
        )) -> Some(TaView(buffer:, kind: elem_kind, byte_offset:, length:))
        _ -> None
      }
    _ -> None
  }
}

/// Bulk element store: when every value converts to the element type without
/// running user code (no objects, no throwing conversions), encode the whole
/// run in one pass and rebuild the buffer binary ONCE — instead of one
/// whole-buffer rebuild + heap write per element, which is O(n²) in the
/// buffer size. Returns None when any value needs the observable per-element
/// path. Mirrors do_typed_store's live-buffer rules: detached buffer or an
/// out-of-bounds view → the stores are silent no-ops; a partial fit writes
/// only the in-bounds prefix.
fn try_bulk_store(
  state: State(host),
  ta_val: JsValue,
  start: Int,
  values: List(JsValue),
) -> Option(State(host)) {
  use view <- option.then(ta_slot(state.heap, ta_val))
  let TaView(buffer:, kind:, byte_offset:, length:) = view
  use region <- option.then(typed_array_elements.typed_array_encode_primitives(
    kind,
    values,
  ))
  case object.typed_array_buffer_data(state.heap, buffer) {
    // Detached → every per-element store is a silent no-op.
    None -> Some(state)
    Some(data) -> {
      let size = value.typed_array_element_size(kind)
      let byte_size = bit_array.byte_size(data)
      let len = case length {
        Some(n) -> n
        None -> int.max(0, { byte_size - byte_offset } / size)
      }
      // Fixed view that no longer fits the (shrunk) buffer → all no-ops.
      use <- bool.guard(byte_offset + len * size > byte_size, Some(state))
      let count = int.clamp(len - start, 0, bit_array.byte_size(region) / size)
      use <- bool.guard(count <= 0, Some(state))
      let region = case count * size == bit_array.byte_size(region) {
        True -> region
        False -> bit_array.slice(region, 0, count * size) |> result.unwrap(<<>>)
      }
      let h =
        write_buffer_data(
          state.heap,
          buffer,
          ta_splice(data, byte_offset + start * size, region),
          byte_offset + start * size,
          count * size,
        )
      Some(State(..state, heap: h))
    }
  }
}

/// RequireInternalSlot(this, [[TypedArrayName]]). Calls `cont` with the
/// resolved view (a TaWitness) and the state.
fn require_ta(
  this: JsValue,
  state: State(host),
  cont: fn(TaWitness, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case ta_slot(state.heap, this) {
    Some(TaView(buffer:, kind:, byte_offset:, length:)) ->
      // Resolve [[ArrayLength]] = AUTO (length-tracking views) to the
      // CURRENT element count — downstream code sees a plain Int and
      // its existing bounds checks behave identically for fixed views.
      cont(
        TaWitness(
          buffer:,
          kind:,
          byte_offset:,
          length: object.typed_array_view_length(
            state.heap,
            buffer,
            kind,
            byte_offset,
            length,
          ),
        ),
        state,
      )
    None ->
      state.type_error(
        state,
        "Method %TypedArray%.prototype called on incompatible receiver",
      )
  }
}

/// §23.2.4.4 ValidateTypedArray ( O, seq-cst ): RequireInternalSlot plus the
/// buffer-witness checks — detached buffer or an out-of-bounds view (a
/// resizable ArrayBuffer that shrank below the view) throws TypeError.
fn validate_ta(
  this: JsValue,
  state: State(host),
  cont: fn(TaWitness, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- require_ta(this, state)
  let TaWitness(buffer:, kind:, byte_offset: off, length: len) = view
  case object.typed_array_buffer_data(state.heap, buffer) {
    None -> witness_type_error(state, Detached)
    Some(data) -> {
      let size = value.typed_array_element_size(kind)
      case off + len * size > bit_array.byte_size(data) {
        True -> witness_type_error(state, OutOfBounds)
        False -> cont(view, state)
      }
    }
  }
}

/// Immutable ArrayBuffer proposal — ValidateTypedArray step 4: accessMode
/// ~write~ on a view over an immutable buffer is a TypeError, raised BEFORE
/// any argument coercion (observable; test262 checks it).
fn require_mutable(
  state: State(host),
  buffer: Ref,
  cont: fn(State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case heap.read(state.heap, buffer) {
    Some(ObjectSlot(kind: value.ArrayBufferObject(immutable: True, ..), ..)) ->
      state.type_error(
        state,
        "Cannot modify a TypedArray backed by an immutable ArrayBuffer",
      )
    _ -> cont(state)
  }
}

/// True when a TypedArray heap ref views an immutable ArrayBuffer.
fn ta_buffer_immutable(h: Heap(host), ta_ref: Ref) -> Bool {
  case heap.read(h, ta_ref) {
    Some(ObjectSlot(kind: value.TypedArrayObject(buffer:, ..), ..)) ->
      case heap.read(h, buffer) {
        Some(ObjectSlot(kind: value.ArrayBufferObject(immutable:, ..), ..)) ->
          immutable
        _ -> False
      }
    _ -> False
  }
}

// ============================================================================
// Accessors — §23.2.3.1-3, .18, .38
// ============================================================================

fn get_buffer(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- require_ta(this, state)
  #(state, Ok(JsObject(view.buffer)))
}

/// True when the view is fully backed by the LIVE buffer — detached buffers
/// and views past the end of a shrunk resizable buffer are "out of bounds",
/// and the byteLength/byteOffset/length accessors all answer 0 for them.
fn view_in_bounds(
  h: Heap(host),
  buffer: Ref,
  kind: TypedArrayKind,
  off: Int,
  len: Int,
) -> Bool {
  case object.typed_array_buffer_data(h, buffer) {
    None -> False
    Some(data) ->
      off + len * value.typed_array_element_size(kind)
      <= bit_array.byte_size(data)
  }
}

fn get_byte_length(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- require_ta(this, state)
  let TaWitness(buffer:, kind:, byte_offset: off, length: len) = view
  let n = case view_in_bounds(state.heap, buffer, kind, off, len) {
    True -> len * value.typed_array_element_size(kind)
    False -> 0
  }
  #(state, Ok(value.from_int(n)))
}

fn get_byte_offset(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- require_ta(this, state)
  let TaWitness(buffer:, kind:, byte_offset: off, length: len) = view
  let n = case view_in_bounds(state.heap, buffer, kind, off, len) {
    True -> off
    False -> 0
  }
  #(state, Ok(value.from_int(n)))
}

fn get_length(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- require_ta(this, state)
  let TaWitness(buffer:, kind:, byte_offset: off, length: len) = view
  let n = case view_in_bounds(state.heap, buffer, kind, off, len) {
    True -> len
    False -> 0
  }
  #(state, Ok(value.from_int(n)))
}

/// §23.2.3.38: NOT a TypeError on foreign receivers — returns undefined.
fn get_to_string_tag(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.TypedArrayObject(elem_kind:, ..), ..)) -> #(
          state,
          Ok(JsString(value.typed_array_name(elem_kind))),
        )
        _ -> #(state, Ok(JsUndefined))
      }
    _ -> #(state, Ok(JsUndefined))
  }
}

// ============================================================================
// Prototype methods (initial subset)
// ============================================================================

/// §23.2.3.1 %TypedArray%.prototype.at ( index )
fn proto_at(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let TaWitness(buffer:, kind:, byte_offset: off, length: len) = view
  let arg = helpers.first_arg_or_undefined(args)
  use rel, state <- try_state(to_int_or_inf(state, arg))
  let k = case rel {
    IInt(n) ->
      case n < 0 {
        True -> len + n
        False -> n
      }
    IPosInf -> len
    INegInf -> -1
  }
  let v =
    object.typed_array_element(state.heap, buffer, kind, off, len, k)
    |> option.unwrap(JsUndefined)
  #(state, Ok(v))
}

/// §23.2.3.8 %TypedArray%.prototype.fill ( value [ , start [ , end ] ] )
fn proto_fill(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let TaWitness(buffer:, kind:, byte_offset: off, length: len) = view
  use state <- require_mutable(state, buffer)
  let value_arg = helpers.first_arg_or_undefined(args)
  let start_arg = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
  let end_arg = helpers.list_at(args, 2) |> option.unwrap(JsUndefined)
  // Step 3-4: convert the fill value per content type.
  use converted, state <- try_state(convert_for_kind(state, kind, value_arg))
  use s, state <- try_state(to_int_or_inf(state, start_arg))
  use e, state <- try_state(case end_arg {
    JsUndefined -> Ok(#(IPosInf, state))
    _ -> to_int_or_inf(state, end_arg)
  })
  let start = relative_index(s, len)
  let end = relative_index(e, len)
  // Steps 11-12: re-validate AFTER the observable value/start/end coercions
  // — a detached buffer or an out-of-bounds view throws TypeError. (A
  // shrunk length-tracking view stays in bounds; the clamp below handles
  // it.)
  use <- lazy_witness_guard(state, this)
  case object.typed_array_buffer_data(state.heap, buffer) {
    None ->
      state.type_error(state, "Cannot perform fill on a detached ArrayBuffer")
    Some(data) -> {
      let size = value.typed_array_element_size(kind)
      // Clamp to the LIVE buffer (a resizable buffer may have shrunk below
      // the view) — out-of-bounds element writes are silent no-ops.
      let avail = int.max(0, { bit_array.byte_size(data) - off } / size)
      let start = int.min(start, avail)
      let end = int.min(end, avail)
      // Single-pass fill: encode the element ONCE, then build the region
      // with binary:copy + one splice (O(n), not O(n²)).
      let elem =
        typed_array_elements.typed_array_encode_value(
          ta_zeroed(size),
          0,
          kind,
          converted,
        )
      let new_data = ta_fill_region(data, off + start * size, end - start, elem)
      let h =
        write_buffer_data(
          state.heap,
          buffer,
          new_data,
          off + start * size,
          int.max(end - start, 0) * size,
        )
      #(State(..state, heap: h), Ok(this))
    }
  }
}

/// Convert a JS value to the typed array's element domain ONCE — §7.1.13
/// ToBigInt for the BigInt kinds, §7.1.4 ToNumber otherwise, both through
/// the canonical coerce hooks. Result is the TypedElement ready for
/// typed_array_encode_value.
fn convert_for_kind(
  state: State(host),
  kind: TypedArrayKind,
  val: JsValue,
) -> Result(
  #(typed_array_elements.TypedElement, State(host)),
  #(JsValue, State(host)),
) {
  case value.typed_array_is_bigint(kind) {
    False -> {
      use #(n, state) <- result.map(coerce.js_to_number(state, val))
      #(typed_array_elements.NumberElement(n), state)
    }
    True -> {
      use #(n, state) <- result.map(coerce.to_bigint(state, val))
      #(typed_array_elements.BigIntElement(n), state)
    }
  }
}

/// §23.2.3.26 %TypedArray%.prototype.set ( source [ , offset ] )
fn proto_set(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- require_ta(this, state)
  // Immutable ArrayBuffer proposal: set() has accessMode ~write~ — checked
  // before the offset/source coercions run any user code.
  use state <- require_mutable(state, view.buffer)
  let src = helpers.first_arg_or_undefined(args)
  let off_arg = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
  use off_i, state <- try_state(to_int_or_inf(state, off_arg))
  let offset = case off_i {
    IInt(n) -> n
    IPosInf -> max_safe_integer
    INegInf -> -1
  }
  use <- bool.lazy_guard(offset < 0, fn() {
    state.range_error(state, "offset is out of bounds")
  })
  // SetTypedArrayFrom* step 2: target detached or out of bounds → TypeError
  // (checked AFTER the observable offset coercion, which can detach/resize
  // it). targetLength is the LIVE length, re-read for the same reason.
  use <- lazy_witness_guard(state, this)
  let len = ta_live_length(state.heap, this)
  case src {
    JsObject(src_ref) ->
      case ta_slot(state.heap, src) {
        Some(src_view) -> {
          let TaView(
            buffer: src_buf,
            kind: src_kind,
            byte_offset: src_off,
            length: src_len,
          ) = src_view
          // §23.2.3.26.1 step 4: SOURCE detached or out of bounds →
          // TypeError; srcLength is its live length.
          case view_witness_error(state.heap, src_view) {
            Some(err) -> witness_type_error(state, err)
            None ->
              set_from_typed_array(
                this,
                state,
                offset,
                len,
                src_buf,
                src_kind,
                src_off,
                object.typed_array_live_length(
                  state.heap,
                  src_buf,
                  src_kind,
                  src_off,
                  object.typed_array_view_length(
                    state.heap,
                    src_buf,
                    src_kind,
                    src_off,
                    src_len,
                  ),
                ),
              )
          }
        }
        None -> set_from_array_like(this, state, offset, len, src_ref, src)
      }
    // §23.2.3.26.2 step 5: ToObject(source) — primitives get wrapped (a
    // string source copies its characters; numbers/booleans/symbols have
    // no "length" so nothing is copied); undefined/null throw TypeError.
    JsUndefined | value.JsNull ->
      state.type_error(
        state,
        "Cannot convert " <> string.inspect(src) <> " to object",
      )
    _ ->
      case common.to_object(state.heap, state.builtins, src) {
        Some(#(h, src_ref)) ->
          set_from_array_like(
            this,
            State(..state, heap: h),
            offset,
            len,
            src_ref,
            JsObject(src_ref),
          )
        None ->
          state.type_error(
            state,
            "Cannot convert " <> string.inspect(src) <> " to object",
          )
      }
  }
}

/// bool.lazy_guard-shaped wrapper over ta_witness_error: throws TypeError
/// when the view's buffer is detached or the view is out of bounds,
/// otherwise continues.
fn lazy_witness_guard(
  state: State(host),
  this: JsValue,
  cont: fn() -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case ta_witness_error(state.heap, this) {
    Some(err) -> witness_type_error(state, err)
    None -> cont()
  }
}

fn set_from_typed_array(
  this: JsValue,
  state: State(host),
  offset: Int,
  len: Int,
  src_buf: Ref,
  src_kind: TypedArrayKind,
  src_off: Int,
  src_len: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- require_ta(this, state)
  let TaWitness(buffer: dst_buf, kind:, byte_offset: dst_off, ..) = view
  // §23.2.3.26.1 step 9: source buffer detached/out of bounds → TypeError.
  let src_live = case object.typed_array_buffer_data(state.heap, src_buf) {
    Some(_) -> True
    None -> False
  }
  use <- bool.lazy_guard(!src_live, fn() {
    state.type_error(state, "Cannot perform set from a detached ArrayBuffer")
  })
  use <- bool.lazy_guard(
    value.typed_array_is_bigint(kind) != value.typed_array_is_bigint(src_kind),
    fn() { state.type_error(state, "Cannot mix BigInt and other types") },
  )
  use <- bool.lazy_guard(src_len + offset > len, fn() {
    state.range_error(state, "offset is out of bounds")
  })
  let size = value.typed_array_element_size(kind)
  // Build the encoded source region in ONE pass (raw byte copy when kinds
  // match, element-wise convert+concat otherwise — no user code runs)...
  let region = case kind == src_kind {
    True -> copy_region(state.heap, src_buf, src_off, src_len * size)
    False ->
      convert_elements(
        state.heap,
        src_buf,
        src_kind,
        src_off,
        src_len,
        kind,
        size,
      )
  }
  // ...then splice it into the destination buffer with ONE rebuild,
  // clamped to the live buffer (out-of-bounds writes are silent no-ops,
  // matching per-element store semantics; detached → no-op).
  case object.typed_array_buffer_data(state.heap, dst_buf) {
    None -> #(state, Ok(JsUndefined))
    Some(data) -> {
      let start = dst_off + offset * size
      let avail =
        {
          int.clamp(bit_array.byte_size(data) - start, 0, src_len * size) / size
        }
        * size
      let region = case avail == src_len * size {
        True -> region
        False -> bit_array.slice(region, 0, avail) |> result.unwrap(<<>>)
      }
      case avail > 0 {
        True -> {
          let h =
            write_buffer_data(
              state.heap,
              dst_buf,
              ta_splice(data, start, region),
              start,
              avail,
            )
          #(State(..state, heap: h), Ok(JsUndefined))
        }
        False -> #(state, Ok(JsUndefined))
      }
    }
  }
}

fn set_from_array_like(
  this: JsValue,
  state: State(host),
  offset: Int,
  len: Int,
  src_ref: Ref,
  src: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  use len_val, state <- try_state(object.get_value(
    state,
    src_ref,
    Named("length"),
    src,
  ))
  use src_len, state <- try_state(to_length(state, len_val))
  use <- bool.lazy_guard(src_len + offset > len, fn() {
    state.range_error(state, "offset is out of bounds")
  })
  let bulk =
    typed_array_elements.plain_indexed_values(state.heap, src_ref, src_len)
    |> option.then(try_bulk_store(state, this, offset, _))
  case bulk {
    Some(state) -> #(state, Ok(JsUndefined))
    None -> set_array_like_loop(this, state, offset, src_ref, src, 0, src_len)
  }
}

fn set_array_like_loop(
  this: JsValue,
  state: State(host),
  offset: Int,
  src_ref: Ref,
  src: JsValue,
  k: Int,
  src_len: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  case k >= src_len {
    True -> #(state, Ok(JsUndefined))
    False -> {
      use v, state <- try_state(object.get_value(state, src_ref, Index(k), src))
      case this {
        JsObject(ta_ref) -> {
          use #(state, _) <- try_state_pair(object.set_value(
            state,
            ta_ref,
            Index(offset + k),
            v,
            this,
          ))
          set_array_like_loop(this, state, offset, src_ref, src, k + 1, src_len)
        }
        _ -> #(state, Ok(JsUndefined))
      }
    }
  }
}

/// §23.2.3.30 %TypedArray%.prototype.subarray ( begin, end ) — a view over
/// the SAME buffer; works on detached arrays too.
fn proto_subarray(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // §23.2.3.30 step 2: RequireInternalSlot only — NOT ValidateTypedArray.
  // A detached buffer / out-of-bounds view does not throw here; it just
  // gives srcLength = 0 (the constructor call below may still throw).
  case ta_slot(state.heap, this) {
    Some(view) -> do_subarray(this, args, state, view)
    None ->
      state.type_error(
        state,
        "Method %TypedArray%.prototype called on incompatible receiver",
      )
  }
}

fn do_subarray(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  view: TaView,
) -> #(State(host), Result(JsValue, JsValue)) {
  let TaView(buffer:, kind:, byte_offset: off, length: declared) = view
  let b_arg = helpers.first_arg_or_undefined(args)
  let e_arg = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
  // Steps 5-7: srcLength = 0 for an out-of-bounds view, else the CURRENT
  // length — snapshotted BEFORE the (observable) start/end coercions.
  let src_length =
    object.typed_array_live_length(
      state.heap,
      buffer,
      kind,
      off,
      object.typed_array_view_length(state.heap, buffer, kind, off, declared),
    )
  use b, state <- try_state(case b_arg {
    JsUndefined -> Ok(#(IInt(0), state))
    _ -> to_int_or_inf(state, b_arg)
  })
  let begin = relative_index(b, src_length)
  let size = value.typed_array_element_size(kind)
  let new_off = off + begin * size
  // Step 15: a length-tracking source with `end` undefined produces a
  // length-tracking result — « buffer, beginByteOffset », NO length arg.
  use ctor_args, state <- try_state(case declared, e_arg {
    None, JsUndefined ->
      Ok(#([JsObject(buffer), value.from_int(new_off)], state))
    _, _ -> {
      use #(e, state) <- result.map(case e_arg {
        JsUndefined -> Ok(#(IInt(src_length), state))
        _ -> to_int_or_inf(state, e_arg)
      })
      let end = relative_index(e, src_length)
      let new_len = int.max(end - begin, 0)
      #(
        [JsObject(buffer), value.from_int(new_off), value.from_int(new_len)],
        state,
      )
    }
  })
  // Step 17: TypedArraySpeciesCreate(O, argumentsList) — same buffer, never
  // a copy. The default path runs the real constructor steps (from_buffer)
  // so detached buffers throw TypeError and out-of-range views RangeError.
  use maybe_ctor, state <- try_state(resolve_species_ctor(state, this, kind))
  case maybe_ctor {
    None -> {
      let proto = default_proto_for(state, kind)
      case from_buffer(state, kind, proto, buffer, list.drop(ctor_args, 1)) {
        Ok(#(v, state)) -> #(state, Ok(v))
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
    }
    Some(ctor) -> {
      use obj, obj_ref, state <- try_state3(ta_create_with_args(
        state,
        ctor,
        ctor_args,
        None,
      ))
      use pair, state <- try_state(check_content_type(state, obj, obj_ref, kind))
      let #(obj, _) = pair
      #(state, Ok(obj))
    }
  }
}

/// §23.2.3.27 %TypedArray%.prototype.slice ( start, end ) — copies into a
/// FRESH buffer (default species).
fn proto_slice(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let TaWitness(buffer:, kind:, byte_offset: off, length: len) = view
  let s_arg = helpers.first_arg_or_undefined(args)
  let e_arg = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
  use s, state <- try_state(case s_arg {
    JsUndefined -> Ok(#(IInt(0), state))
    _ -> to_int_or_inf(state, s_arg)
  })
  use e, state <- try_state(case e_arg {
    JsUndefined -> Ok(#(IPosInf, state))
    _ -> to_int_or_inf(state, e_arg)
  })
  let start = relative_index(s, len)
  let end = relative_index(e, len)
  let count = int.max(end - start, 0)
  // Step 9: A = TypedArraySpeciesCreate(O, « count »).
  use pair, state <- try_state(ta_species_create(state, this, kind, count))
  let #(target, target_ref) = pair
  use <- bool.guard(count == 0, #(state, Ok(target)))
  // Step 10.a-b: re-validate the SOURCE — the species constructor (or
  // start/end valueOf) may have detached its buffer or shrunk it below a
  // fixed view; both throw TypeError. (A shrunk length-tracking view stays
  // in bounds — the copy below just reads fewer live bytes.)
  case ta_witness_error(state.heap, this) {
    Some(err) -> witness_type_error(state, err)
    None ->
      case ta_slot(state.heap, target) {
        Some(TaView(
          buffer: target_buf,
          kind: target_kind,
          byte_offset: target_off,
          length: _declared,
        )) ->
          case target_kind == kind {
            // Same element kind → single byte-region copy spliced into the
            // target's buffer at its view offset.
            True -> {
              let size = value.typed_array_element_size(kind)
              // Step 14.c: endIndex = min(final, live TypedArrayLength) —
              // only WHOLE live source elements are copied (a shrink
              // "between elements" must not copy a partial element); the
              // target keeps its own contents beyond them.
              let copy_elems =
                int.clamp(ta_live_length(state.heap, this) - start, 0, count)
              let src_byte = off + start * size
              case object.typed_array_buffer_data(state.heap, target_buf) {
                None -> #(state, Ok(target))
                Some(tdata) -> {
                  let avail =
                    int.clamp(
                      bit_array.byte_size(tdata) - target_off,
                      0,
                      copy_elems * size,
                    )
                  // Step 14.f copies byte-by-byte in ascending order: when
                  // a species constructor returned a view over the SAME
                  // buffer and the target range trails the source range,
                  // earlier writes feed later reads.
                  let region = case target_buf == buffer {
                    True -> seq_copy_region(tdata, src_byte, target_off, avail)
                    False -> copy_region(state.heap, buffer, src_byte, avail)
                  }
                  let h =
                    write_buffer_data(
                      state.heap,
                      target_buf,
                      ta_splice(tdata, target_off, region),
                      target_off,
                      avail,
                    )
                  #(State(..state, heap: h), Ok(target))
                }
              }
            }
            // Different (number) kind from a species constructor →
            // element-wise convert via the ordinary Set path.
            False -> {
              let elements =
                join_collect(state.heap, this, len, start, [])
                |> list.reverse
                |> list.take(count)
              use Nil, state <- try_state(write_values(
                state,
                target,
                target_ref,
                elements,
                0,
              ))
              #(state, Ok(target))
            }
          }
        None -> #(state, Ok(target))
      }
  }
}

/// The result of copying `n` bytes from `src` to `dst` within ONE binary,
/// byte-by-byte in ascending order (§23.2.3.27 step 14.f): when the target
/// range starts inside the source range, each written byte is re-read by a
/// later step, so the leading `dst - src` bytes repeat across the result.
fn seq_copy_region(data: BitArray, src: Int, dst: Int, n: Int) -> BitArray {
  case dst > src && dst < src + n {
    False -> bit_array.slice(data, src, n) |> result.unwrap(<<>>)
    True -> {
      let pattern = bit_array.slice(data, src, dst - src) |> result.unwrap(<<>>)
      repeat_to(pattern, n, <<>>)
    }
  }
}

fn repeat_to(pattern: BitArray, n: Int, acc: BitArray) -> BitArray {
  use <- bool.guard(bit_array.byte_size(pattern) == 0, acc)
  case bit_array.byte_size(acc) >= n {
    True -> bit_array.slice(acc, 0, n) |> result.unwrap(<<>>)
    False -> repeat_to(pattern, n, bit_array.concat([acc, pattern]))
  }
}

/// Copy `byte_len` bytes starting at `byte_off` of a view's backing buffer
/// into a fresh binary — ONE slice instead of per-element re-encoding.
/// Bytes beyond the live buffer (detached, or a resizable buffer that has
/// shrunk below the view) read as zero.
fn copy_region(
  h: Heap(host),
  buffer: Ref,
  byte_off: Int,
  byte_len: Int,
) -> BitArray {
  case object.typed_array_buffer_data(h, buffer) {
    None -> ta_zeroed(byte_len)
    Some(data) -> {
      let avail = int.clamp(bit_array.byte_size(data) - byte_off, 0, byte_len)
      case bit_array.slice(data, byte_off, avail) {
        Ok(bytes) ->
          case avail == byte_len {
            True -> bytes
            False -> bit_array.concat([bytes, ta_zeroed(byte_len - avail)])
          }
        Error(Nil) -> ta_zeroed(byte_len)
      }
    }
  }
}

/// §23.2.3.16 %TypedArray%.prototype.join ( separator )
fn proto_join(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let len = view.length
  let sep_arg = helpers.first_arg_or_undefined(args)
  use sep, state <- try_state(case sep_arg {
    JsUndefined -> Ok(#(",", state))
    _ -> coerce.js_to_string(state, sep_arg)
  })
  let parts =
    join_parts(state.heap, this, len, 0, [])
    |> list.reverse
  #(state, Ok(JsString(string.join(parts, sep))))
}

fn join_parts(
  h: Heap(host),
  this: JsValue,
  len: Int,
  i: Int,
  acc: List(String),
) -> List(String) {
  case i >= len {
    True -> acc
    False -> {
      // Live read: ToString(separator) above may have shrunk the buffer;
      // indices past the CURRENT length read as undefined → "".
      let s = case ta_read(h, this, i) {
        Some(JsNumber(n)) -> value.format_number_radix(n, 10)
        Some(value.JsBigInt(value.BigInt(b))) -> int.to_string(b)
        _ -> ""
      }
      join_parts(h, this, len, i + 1, [s, ..acc])
    }
  }
}

/// §23.2.3.16/.13 indexOf / includes. See search_loop for how the two
/// differ on indices past the current (shrunk/detached) length.
fn proto_index_of(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  proto_search(this, args, state, value.strict_equal, False, value.from_int)
}

fn proto_includes(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use found <- proto_search(this, args, state, value.same_value_zero, True)
  JsBool(found >= 0)
}

fn proto_search(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  eq: fn(JsValue, JsValue) -> Bool,
  missing_undefined: Bool,
  done: fn(Int) -> JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let len = view.length
  use <- bool.guard(len == 0, #(state, Ok(done(-1))))
  let search = helpers.first_arg_or_undefined(args)
  use n, state <- try_state(to_int_or_inf(
    state,
    helpers.list_at(args, 1) |> option.unwrap(JsUndefined),
  ))
  let k = case n {
    INegInf -> 0
    // A +Infinity start is past every index, so search_loop returns -1.
    IPosInf -> len
    IInt(i) ->
      case i >= 0 {
        True -> i
        False -> int.max(len + i, 0)
      }
  }
  let found =
    search_loop(state.heap, this, len, k, search, eq, missing_undefined)
  #(state, Ok(done(found)))
}

/// Scan [i, len) for `search` with live element reads. `missing_undefined`:
/// True (includes) treats an invalid index as the undefined value Get
/// returns; False (indexOf) skips it, per the HasProperty step.
fn search_loop(
  h: Heap(host),
  this: JsValue,
  len: Int,
  i: Int,
  search: JsValue,
  eq: fn(JsValue, JsValue) -> Bool,
  missing_undefined: Bool,
) -> Int {
  case i >= len {
    True -> -1
    False -> {
      let matched = case ta_read(h, this, i) {
        Some(el) -> eq(el, search)
        None -> missing_undefined && eq(JsUndefined, search)
      }
      case matched {
        True -> i
        False -> search_loop(h, this, len, i + 1, search, eq, missing_undefined)
      }
    }
  }
}

/// keys()/values()/entries() — §23.2.3.{19,35,7}: ValidateTypedArray, then
/// CreateArrayIterator over the typed array itself. LAZY: each .next()
/// re-validates the buffer witness and re-reads length/elements through the
/// live backing store, so mutation (and resizable-buffer resizes) during
/// iteration are observed per spec.
type IterKind {
  IterKeys
  IterValues
  IterEntries
}

fn proto_iter(
  this: JsValue,
  state: State(host),
  iter_kind: IterKind,
) -> #(State(host), Result(JsValue, JsValue)) {
  use _view, state <- validate_ta(this, state)
  case this {
    JsObject(ta_ref) -> {
      let kind = case iter_kind {
        IterKeys -> value.ArrayIterKeys
        IterValues -> value.ArrayIterValues
        IterEntries -> value.ArrayIterEntries
      }
      let #(h, iter_ref) =
        common.alloc_wrapper(
          state.heap,
          value.ArrayIteratorObject(source: ta_ref, index: 0, iter_kind: kind),
          state.builtins.array_iterator_proto,
        )
      #(State(..state, heap: h), Ok(JsObject(iter_ref)))
    }
    _ ->
      state.type_error(
        state,
        "Method %TypedArray%.prototype called on incompatible receiver",
      )
  }
}

/// Collect elements [i, len) with live reads, REVERSED, stopping at the
/// first invalid index — mirrors the spec's "endIndex = min(final,
/// TypedArrayLength)" re-derivation in slice after user code (species
/// constructor / valueOf) may have shrunk the buffer. For sort/toSorted no
/// user code runs between validation and collection, so the stop never
/// triggers there.
fn join_collect(
  h: Heap(host),
  this: JsValue,
  len: Int,
  i: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case i >= len {
    True -> acc
    False ->
      case ta_read(h, this, i) {
        Some(v) -> join_collect(h, this, len, i + 1, [v, ..acc])
        None -> acc
      }
  }
}

// ============================================================================
// Callback-iterating prototype methods — §23.2.3
// ============================================================================

/// Step "If IsCallable(callbackfn) is false, throw TypeError" shared by all
/// callback-taking methods; passes (callback, thisArg) on.
fn require_cb(
  args: List(JsValue),
  state: State(host),
  cont: fn(JsValue, JsValue, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let cb = helpers.first_arg_or_undefined(args)
  let this_arg = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
  case helpers.is_callable(state.heap, cb) {
    True -> cont(cb, this_arg, state)
    False -> state.type_error(state, string.inspect(cb) <> " is not a function")
  }
}

/// §10.4.5.16 IntegerIndexedElementGet against the LIVE view: re-reads the
/// TypedArray slot so user code that resized the backing buffer mid-method
/// (callback / valueOf side effects) is observed. A length-tracking view
/// follows the CURRENT buffer length; a fixed view that no longer fits is
/// wholly out of bounds. None = invalid index (like a detached buffer).
fn ta_read(h: Heap(host), this: JsValue, k: Int) -> Option(JsValue) {
  use view <- option.then(ta_slot(h, this))
  let TaView(buffer:, kind:, byte_offset:, length:) = view
  let live_len =
    object.typed_array_view_length(h, buffer, kind, byte_offset, length)
  object.typed_array_element(h, buffer, kind, byte_offset, live_len, k)
}

/// Read element `k` as the spec's Get(O, Pk) does: out-of-bounds (shrunk
/// resizable buffer / detached mid-iteration) reads as undefined.
fn ta_get(h: Heap(host), this: JsValue, k: Int) -> JsValue {
  ta_read(h, this, k) |> option.unwrap(JsUndefined)
}

/// The ways §23.2.4.4 ValidateTypedArray's witness checks can fail. Each is
/// a TypeError; witness_message owns the prose.
type WitnessError {
  Detached
  OutOfBounds
  NotTypedArray
}

/// THE owner of the witness-failure TypeError prose.
fn witness_message(err: WitnessError) -> String {
  case err {
    Detached -> "Cannot perform operation on a detached ArrayBuffer"
    OutOfBounds -> "TypedArray is out of bounds"
    NotTypedArray -> "Method invoked on an object that is not a TypedArray"
  }
}

/// Throw the TypeError for a witness failure.
fn witness_type_error(
  state: State(host),
  err: WitnessError,
) -> #(State(host), Result(JsValue, JsValue)) {
  state.type_error(state, witness_message(err))
}

/// §23.2.4.4 ValidateTypedArray buffer-witness checks against the LIVE
/// buffer: Some(error) when the buffer is detached or the view is out of
/// bounds (a fixed view past the end of a shrunk resizable buffer, or a
/// length-tracking view whose byte offset is past the end).
fn view_witness_error(h: Heap(host), view: TaView) -> Option(WitnessError) {
  let TaView(buffer:, kind:, byte_offset: off, length: declared) = view
  case object.typed_array_buffer_data(h, buffer) {
    None -> Some(Detached)
    Some(data) -> {
      let byte_size = bit_array.byte_size(data)
      let size = value.typed_array_element_size(kind)
      let oob = case declared {
        Some(n) -> off + n * size > byte_size
        None -> off > byte_size
      }
      case oob {
        True -> Some(OutOfBounds)
        False -> None
      }
    }
  }
}

/// view_witness_error keyed by the TypedArray value itself (re-reads the
/// slot so resizes since validation are observed).
fn ta_witness_error(h: Heap(host), this: JsValue) -> Option(WitnessError) {
  case ta_slot(h, this) {
    Some(view) -> view_witness_error(h, view)
    None -> Some(NotTypedArray)
  }
}

/// CURRENT number of valid indices of the view — 0 for a detached buffer or
/// an out-of-bounds fixed view; the live tracked length for a
/// length-tracking view. The §10.4.5.14 IsValidIntegerIndex bound.
fn ta_live_length(h: Heap(host), this: JsValue) -> Int {
  case ta_slot(h, this) {
    Some(TaView(buffer:, kind:, byte_offset:, length:)) ->
      object.typed_array_live_length(
        h,
        buffer,
        kind,
        byte_offset,
        object.typed_array_view_length(h, buffer, kind, byte_offset, length),
      )
    None -> 0
  }
}

/// Generic callback loop: visits indices from `k` by `step` while inside
/// [0, len), calling cb(element, k, this). `decide` inspects the callback
/// result and may stop the loop with a final value.
fn iterate_calls(
  state: State(host),
  len: Int,
  k: Int,
  step: Int,
  this: JsValue,
  cb: JsValue,
  this_arg: JsValue,
  decide: fn(JsValue, JsValue, Int) -> Option(JsValue),
) -> Result(#(Option(JsValue), State(host)), #(JsValue, State(host))) {
  use <- bool.guard(k < 0 || k >= len, Ok(#(None, state)))
  let el = ta_get(state.heap, this, k)
  use #(res, state) <- result.try(
    state.call(state, cb, this_arg, [el, value.from_int(k), this]),
  )
  case decide(res, el, k) {
    Some(v) -> Ok(#(Some(v), state))
    None ->
      iterate_calls(state, len, k + step, step, this, cb, this_arg, decide)
  }
}

/// §23.2.3.7 every / §23.2.3.28 some.
fn proto_every_some(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  is_every: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let len = view.length
  use cb, this_arg, state <- require_cb(args, state)
  let decide = fn(res, _el, _k) {
    case value.is_truthy(res) == is_every {
      True -> None
      False -> Some(JsBool(!is_every))
    }
  }
  use early, state <- try_state(iterate_calls(
    state,
    len,
    0,
    1,
    this,
    cb,
    this_arg,
    decide,
  ))
  #(state, Ok(early |> option.unwrap(JsBool(is_every))))
}

/// §23.2.3.15 forEach.
fn proto_for_each(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let len = view.length
  use cb, this_arg, state <- require_cb(args, state)
  use _early, state <- try_state(
    iterate_calls(state, len, 0, 1, this, cb, this_arg, fn(_res, _el, _k) {
      None
    }),
  )
  #(state, Ok(JsUndefined))
}

/// find/findIndex (§23.2.3.13/.14) ascending, findLast/findLastIndex
/// (§23.2.3.11/.12) descending. Result is the element or its index.
type FindMode {
  FindValue
  FindIdx
}

fn proto_find(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  step: Int,
  mode: FindMode,
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let len = view.length
  use cb, this_arg, state <- require_cb(args, state)
  let start = case step > 0 {
    True -> 0
    False -> len - 1
  }
  let decide = fn(res, el, k) {
    case value.is_truthy(res) {
      True ->
        Some(case mode {
          FindValue -> el
          FindIdx -> value.from_int(k)
        })
      False -> None
    }
  }
  use early, state <- try_state(iterate_calls(
    state,
    len,
    start,
    step,
    this,
    cb,
    this_arg,
    decide,
  ))
  let default = case mode {
    FindValue -> JsUndefined
    FindIdx -> value.from_int(-1)
  }
  #(state, Ok(early |> option.unwrap(default)))
}

/// §23.2.3.22 map ( callbackfn [ , thisArg ] ) — result via
/// TypedArraySpeciesCreate(O, « len »).
fn proto_map(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let TaWitness(kind:, length: len, ..) = view
  use cb, this_arg, state <- require_cb(args, state)
  use #(target, target_ref), state <- try_state(ta_species_create(
    state,
    this,
    kind,
    len,
  ))
  use Nil, state <- try_state(map_loop(
    state,
    len,
    0,
    this,
    cb,
    this_arg,
    target,
    target_ref,
  ))
  #(state, Ok(target))
}

fn map_loop(
  state: State(host),
  len: Int,
  k: Int,
  this: JsValue,
  cb: JsValue,
  this_arg: JsValue,
  target: JsValue,
  target_ref: Ref,
) -> Result(#(Nil, State(host)), #(JsValue, State(host))) {
  use <- bool.guard(k >= len, Ok(#(Nil, state)))
  let el = ta_get(state.heap, this, k)
  use #(mapped, state) <- result.try(
    state.call(state, cb, this_arg, [el, value.from_int(k), this]),
  )
  use #(state, _) <- result.try(object.set_value(
    state,
    target_ref,
    Index(k),
    mapped,
    target,
  ))
  map_loop(state, len, k + 1, this, cb, this_arg, target, target_ref)
}

/// §23.2.3.10 filter ( callbackfn [ , thisArg ] ) — kept values collected
/// first, then TypedArraySpeciesCreate(O, « captured ») and written out.
fn proto_filter(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let TaWitness(kind:, length: len, ..) = view
  use cb, this_arg, state <- require_cb(args, state)
  use kept_rev, state <- try_state(
    filter_collect(state, len, 0, this, cb, this_arg, []),
  )
  let kept = list.reverse(kept_rev)
  use #(target, target_ref), state <- try_state(ta_species_create(
    state,
    this,
    kind,
    list.length(kept),
  ))
  use Nil, state <- try_state(write_values(state, target, target_ref, kept, 0))
  #(state, Ok(target))
}

fn filter_collect(
  state: State(host),
  len: Int,
  k: Int,
  this: JsValue,
  cb: JsValue,
  this_arg: JsValue,
  acc: List(JsValue),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  use <- bool.guard(k >= len, Ok(#(acc, state)))
  let el = ta_get(state.heap, this, k)
  use #(res, state) <- result.try(
    state.call(state, cb, this_arg, [el, value.from_int(k), this]),
  )
  let acc = case value.is_truthy(res) {
    True -> [el, ..acc]
    False -> acc
  }
  filter_collect(state, len, k + 1, this, cb, this_arg, acc)
}

/// Write a list of (already numeric) values into a TypedArray via the
/// ordinary Set path, starting at index `k`.
fn write_values(
  state: State(host),
  target: JsValue,
  target_ref: Ref,
  values: List(JsValue),
  k: Int,
) -> Result(#(Nil, State(host)), #(JsValue, State(host))) {
  case values {
    [] -> Ok(#(Nil, state))
    [v, ..rest] -> {
      use #(state, _) <- result.try(object.set_value(
        state,
        target_ref,
        Index(k),
        v,
        target,
      ))
      write_values(state, target, target_ref, rest, k + 1)
    }
  }
}

/// §23.2.3.23/.24 reduce / reduceRight. `step` is +1 (reduce) or -1.
fn proto_reduce(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  step: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let len = view.length
  let cb = helpers.first_arg_or_undefined(args)
  use <- bool.lazy_guard(!helpers.is_callable(state.heap, cb), fn() {
    state.type_error(state, string.inspect(cb) <> " is not a function")
  })
  let start = case step > 0 {
    True -> 0
    False -> len - 1
  }
  case helpers.list_at(args, 1) {
    Some(init) -> reduce_loop(state, len, start, step, this, cb, init)
    None ->
      case len == 0 {
        True ->
          state.type_error(state, "Reduce of empty array with no initial value")
        False -> {
          let acc = ta_get(state.heap, this, start)
          reduce_loop(state, len, start + step, step, this, cb, acc)
        }
      }
  }
}

fn reduce_loop(
  state: State(host),
  len: Int,
  k: Int,
  step: Int,
  this: JsValue,
  cb: JsValue,
  acc: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  use <- bool.guard(k < 0 || k >= len, #(state, Ok(acc)))
  let el = ta_get(state.heap, this, k)
  use res, state <- state.try_call(state, cb, JsUndefined, [
    acc,
    el,
    value.from_int(k),
    this,
  ])
  reduce_loop(state, len, k + step, step, this, cb, res)
}

// ============================================================================
// copyWithin / reverse / toReversed / with — byte-level methods
// ============================================================================

/// §23.2.3.5 copyWithin ( target, start [ , end ] ).
fn proto_copy_within(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let TaWitness(buffer:, kind:, byte_offset: off, length: len) = view
  use state <- require_mutable(state, buffer)
  let target_arg = helpers.first_arg_or_undefined(args)
  let start_arg = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
  let end_arg = helpers.list_at(args, 2) |> option.unwrap(JsUndefined)
  use t, state <- try_state(to_int_or_inf(state, target_arg))
  use s, state <- try_state(to_int_or_inf(state, start_arg))
  use e, state <- try_state(case end_arg {
    JsUndefined -> Ok(#(IPosInf, state))
    _ -> to_int_or_inf(state, end_arg)
  })
  let to = relative_index(t, len)
  let from = relative_index(s, len)
  let final = relative_index(e, len)
  let count = int.min(final - from, len - to)
  use <- bool.guard(count <= 0, #(state, Ok(this)))
  // Steps 12.c-d: re-validate against the live buffer — the coercions above
  // can run user code that detaches the buffer or shrinks it below a FIXED
  // view; both throw TypeError.
  use <- lazy_witness_guard(state, this)
  case object.typed_array_buffer_data(state.heap, buffer) {
    None ->
      state.type_error(
        state,
        "Cannot perform copyWithin on a detached ArrayBuffer",
      )
    Some(data) -> {
      let size = value.typed_array_element_size(kind)
      // Steps 12.e-f: len = min(len, TypedArrayLength) — a length-tracking
      // view follows a SHRUNK buffer (clamp indices/count down), but a grow
      // never raises the snapshot bounds.
      let live_len = int.min(len, ta_live_length(state.heap, this))
      let to = int.min(to, live_len)
      let from = int.min(from, live_len)
      let final = int.min(final, live_len)
      let count = int.min(final - from, live_len - to)
      use <- bool.guard(count <= 0, #(state, Ok(this)))
      case bit_array.slice(data, off + from * size, count * size) {
        Ok(region) -> {
          let new_data = ta_splice(data, off + to * size, region)
          #(
            State(
              ..state,
              heap: write_buffer_data(
                state.heap,
                buffer,
                new_data,
                off + to * size,
                count * size,
              ),
            ),
            Ok(this),
          )
        }
        Error(Nil) -> #(state, Ok(this))
      }
    }
  }
}

/// Concatenation of a view's elements in reverse element order.
fn reversed_bytes(data: BitArray, off: Int, len: Int, size: Int) -> BitArray {
  reversed_bytes_loop(data, off, len, size, 0, [])
}

fn reversed_bytes_loop(
  data: BitArray,
  off: Int,
  len: Int,
  size: Int,
  i: Int,
  acc: List(BitArray),
) -> BitArray {
  case i >= len {
    True -> bit_array.concat(acc)
    False -> {
      let elem = case bit_array.slice(data, off + i * size, size) {
        Ok(b) -> b
        Error(Nil) -> ta_zeroed(size)
      }
      reversed_bytes_loop(data, off, len, size, i + 1, [elem, ..acc])
    }
  }
}

/// §23.2.3.25 reverse ( ) — in place, returns this.
fn proto_reverse(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let TaWitness(buffer:, kind:, byte_offset: off, length: len) = view
  use state <- require_mutable(state, buffer)
  case object.typed_array_buffer_data(state.heap, buffer) {
    None -> #(state, Ok(this))
    Some(data) -> {
      let size = value.typed_array_element_size(kind)
      let region = reversed_bytes(data, off, len, size)
      let new_data = ta_splice(data, off, region)
      #(
        State(
          ..state,
          heap: write_buffer_data(state.heap, buffer, new_data, off, len * size),
        ),
        Ok(this),
      )
    }
  }
}

/// The buffer ref backing a freshly created TypedArray value.
fn ta_view_buffer(h: Heap(host), ta_val: JsValue) -> Option(Ref) {
  case ta_val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: value.TypedArrayObject(buffer:, ..), ..)) ->
          Some(buffer)
        _ -> None
      }
    _ -> None
  }
}

/// TypedArrayCreateSameType (§23.2.4.3) — fresh array of the receiver's own
/// kind with the intrinsic default prototype (species is NOT consulted).
fn ta_same_type_create(
  state: State(host),
  kind: TypedArrayKind,
  len: Int,
) -> Result(#(JsValue, Ref, State(host)), #(JsValue, State(host))) {
  use #(ta_val, state) <- result.try(alloc_ta_with_length(
    state,
    kind,
    default_proto_for(state, kind),
    len,
  ))
  case ta_view_buffer(state.heap, ta_val) {
    Some(buf) -> Ok(#(ta_val, buf, state))
    None -> Error(state.type_error_value(state, "TypedArray allocation failed"))
  }
}

/// §23.2.3.32 toReversed ( ).
fn proto_to_reversed(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let TaWitness(buffer:, kind:, byte_offset: off, length: len) = view
  let size = value.typed_array_element_size(kind)
  use ta_val, new_buf, state <- try_state3(ta_same_type_create(state, kind, len))
  let src = copy_region(state.heap, buffer, off, len * size)
  let new_data = reversed_bytes(src, 0, len, size)
  // Fresh buffer — this caller owns every byte.
  #(
    State(
      ..state,
      heap: write_buffer_data(
        state.heap,
        new_buf,
        new_data,
        0,
        bit_array.byte_size(new_data),
      ),
    ),
    Ok(ta_val),
  )
}

/// §23.2.3.36 with ( index, value ).
fn proto_with(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let TaWitness(buffer:, kind:, byte_offset: off, length: len) = view
  let index_arg = helpers.first_arg_or_undefined(args)
  let value_arg = helpers.list_at(args, 1) |> option.unwrap(JsUndefined)
  use rel, state <- try_state(to_int_or_inf(state, index_arg))
  let actual = case rel {
    IInt(i) ->
      case i >= 0 {
        True -> i
        False -> len + i
      }
    IPosInf -> max_safe_integer
    INegInf -> -1
  }
  // Step 8: numeric conversion happens BEFORE the index range check — and
  // its valueOf may RESIZE the buffer.
  use converted, state <- try_state(convert_for_kind(state, kind, value_arg))
  let size = value.typed_array_element_size(kind)
  // Step 9: IsValidIntegerIndex(O, actualIndex) against the LIVE view — a
  // grow during the conversion can make an initially out-of-bounds index
  // valid (and vice versa).
  let valid = actual >= 0 && actual < ta_live_length(state.heap, this)
  use <- bool.lazy_guard(!valid, fn() {
    state.range_error(state, "Invalid typed array index")
  })
  // Step 10: A = TypedArrayCreateSameType(O, « len ») with the SNAPSHOT
  // length; the replacement value only lands when actualIndex is inside it.
  use ta_val, new_buf, state <- try_state3(ta_same_type_create(state, kind, len))
  let data = copy_region(state.heap, buffer, off, len * size)
  let new_data = case actual < len {
    True ->
      typed_array_elements.typed_array_encode_value(
        data,
        actual * size,
        kind,
        converted,
      )
    False -> data
  }
  // Fresh buffer — this caller owns every byte.
  #(
    State(
      ..state,
      heap: write_buffer_data(
        state.heap,
        new_buf,
        new_data,
        0,
        bit_array.byte_size(new_data),
      ),
    ),
    Ok(ta_val),
  )
}

// ============================================================================
// lastIndexOf — §23.2.3.20
// ============================================================================

fn proto_last_index_of(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let len = view.length
  use <- bool.guard(len == 0, #(state, Ok(value.from_int(-1))))
  let search = helpers.first_arg_or_undefined(args)
  // Step 4: fromIndex PRESENT (even as undefined) → ToIntegerOrInfinity;
  // absent → len - 1.
  use n, state <- try_state(case helpers.list_at(args, 1) {
    None -> Ok(#(IInt(len - 1), state))
    Some(v) -> to_int_or_inf(state, v)
  })
  case n {
    INegInf -> #(state, Ok(value.from_int(-1)))
    _ -> {
      let k = case n {
        IPosInf -> len - 1
        IInt(i) ->
          case i >= 0 {
            True -> int.min(i, len - 1)
            False -> len + i
          }
        INegInf -> -1
      }
      let found = search_down(state.heap, this, k, search)
      #(state, Ok(value.from_int(found)))
    }
  }
}

/// Scan [0, k] downward with live reads; invalid indices (fromIndex valueOf
/// shrank the buffer) are skipped per the HasProperty step.
fn search_down(h: Heap(host), this: JsValue, k: Int, search: JsValue) -> Int {
  case k < 0 {
    True -> -1
    False ->
      case ta_read(h, this, k) {
        Some(el) ->
          case value.strict_equal(el, search) {
            True -> k
            False -> search_down(h, this, k - 1, search)
          }
        None -> search_down(h, this, k - 1, search)
      }
  }
}

// ============================================================================
// sort / toSorted — §23.2.3.29 / §23.2.3.33
// ============================================================================

@external(erlang, "arc_vm_ffi", "float_same_term")
fn float_same_term(a: Float, b: Float) -> Bool

/// §23.2.4.7 CompareTypedArrayElements with comparefn undefined:
/// NaN sorts last (NaN vs NaN → 0), -0 before +0, otherwise numeric.
fn default_ta_compare(x: JsValue, y: JsValue) -> Int {
  case x, y {
    JsNumber(a), JsNumber(b) -> compare_numbers(a, b)
    value.JsBigInt(value.BigInt(a)), value.JsBigInt(value.BigInt(b)) ->
      case a < b, a > b {
        True, _ -> -1
        _, True -> 1
        False, False -> 0
      }
    _, _ -> 0
  }
}

fn compare_numbers(a: value.JsNum, b: value.JsNum) -> Int {
  case a, b {
    value.NaN, value.NaN -> 0
    value.NaN, _ -> 1
    _, value.NaN -> -1
    value.NegInfinity, value.NegInfinity -> 0
    value.Infinity, value.Infinity -> 0
    value.NegInfinity, _ -> -1
    _, value.NegInfinity -> 1
    value.Infinity, _ -> 1
    _, value.Infinity -> -1
    Finite(fa), Finite(fb) ->
      case fa <. fb, fa >. fb {
        True, _ -> -1
        _, True -> 1
        False, False -> {
          let a_neg_zero = float_same_term(fa, -0.0)
          let b_neg_zero = float_same_term(fb, -0.0)
          case a_neg_zero, b_neg_zero {
            True, False -> -1
            False, True -> 1
            _, _ -> 0
          }
        }
      }
  }
}

/// A monadic comparator: -1/0/1, threading State (a user comparefn can run
/// arbitrary JS).
fn comparator_for(
  cmp: JsValue,
) -> fn(State(host), JsValue, JsValue) ->
  Result(#(Int, State(host)), #(JsValue, State(host))) {
  case cmp {
    JsUndefined -> fn(state, x, y) { Ok(#(default_ta_compare(x, y), state)) }
    _ -> fn(state, x, y) {
      use #(res, state) <- result.try(
        state.call(state, cmp, JsUndefined, [x, y]),
      )
      use #(n, state) <- result.map(coerce.js_to_number(state, res))
      let c = case n {
        value.NaN -> 0
        Finite(f) ->
          case f <. 0.0, f >. 0.0 {
            True, _ -> -1
            _, True -> 1
            False, False -> 0
          }
        value.Infinity -> 1
        value.NegInfinity -> -1
      }
      #(c, state)
    }
  }
}

/// Stable merge sort over the snapshot list, threading State through the
/// (possibly effectful) comparator.
fn sort_values(
  state: State(host),
  items: List(JsValue),
  cmp: fn(State(host), JsValue, JsValue) ->
    Result(#(Int, State(host)), #(JsValue, State(host))),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  case items {
    [] | [_] -> Ok(#(items, state))
    _ -> {
      let #(left, right) = list.split(items, list.length(items) / 2)
      use #(ls, state) <- result.try(sort_values(state, left, cmp))
      use #(rs, state) <- result.try(sort_values(state, right, cmp))
      merge_values(state, ls, rs, cmp, [])
    }
  }
}

fn merge_values(
  state: State(host),
  left: List(JsValue),
  right: List(JsValue),
  cmp: fn(State(host), JsValue, JsValue) ->
    Result(#(Int, State(host)), #(JsValue, State(host))),
  acc: List(JsValue),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  case left, right {
    [], _ -> Ok(#(list.append(list.reverse(acc), right), state))
    _, [] -> Ok(#(list.append(list.reverse(acc), left), state))
    [x, ..xs], [y, ..ys] -> {
      use #(c, state) <- result.try(cmp(state, x, y))
      case c <= 0 {
        True -> merge_values(state, xs, right, cmp, [x, ..acc])
        False -> merge_values(state, left, ys, cmp, [y, ..acc])
      }
    }
  }
}

/// Encode a list of numeric values into one contiguous byte region — each
/// value is encoded into its own elem-size binary and the segments are
/// concatenated ONCE at the end (O(n), instead of rebuilding the full
/// backing buffer per element, which was O(n²)).
fn encode_region(
  kind: TypedArrayKind,
  size: Int,
  values: List(JsValue),
) -> BitArray {
  // The values are a snapshot READ from the typed array (sorted_snapshot),
  // so each one is a JsNumber / JsBigInt; anything else encodes as zero.
  list.map(values, fn(v) {
    case typed_array_elements.decoded_element(v) {
      Some(el) ->
        typed_array_elements.typed_array_encode_value(
          ta_zeroed(size),
          0,
          kind,
          el,
        )
      None -> ta_zeroed(size)
    }
  })
  |> bit_array.concat
}

/// Shared sort/toSorted prologue: comparefn validated FIRST, then
/// ValidateTypedArray; snapshots the elements and sorts them, passing the
/// validated view plus the sorted list to `cont`.
fn sorted_snapshot(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  cont: fn(TaWitness, List(JsValue), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let cmp = helpers.first_arg_or_undefined(args)
  use <- bool.lazy_guard(
    cmp != JsUndefined && !helpers.is_callable(state.heap, cmp),
    fn() {
      state.type_error(
        state,
        "The comparison function must be either a function or undefined",
      )
    },
  )
  use view, state <- validate_ta(this, state)
  let items = join_collect(state.heap, this, view.length, 0, []) |> list.reverse
  use sorted, state <- try_state(sort_values(state, items, comparator_for(cmp)))
  cont(view, sorted, state)
}

/// §23.2.3.29 sort ( comparefn ) — sorts a snapshot, writes back in place.
fn proto_sort(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Immutable ArrayBuffer proposal: sort() has accessMode ~write~ — the
  // comparator must never run against an immutable-backed receiver. (The
  // spec checks comparator callability first, but both failures are
  // TypeErrors, so the order is unobservable.)
  use view_w, state <- require_ta(this, state)
  use state <- require_mutable(state, view_w.buffer)
  use view, sorted, state <- sorted_snapshot(this, args, state)
  let TaWitness(buffer:, kind:, byte_offset: off, length: len) = view
  case object.typed_array_buffer_data(state.heap, buffer) {
    None -> #(state, Ok(this))
    Some(data) -> {
      let size = value.typed_array_element_size(kind)
      // Build the sorted region in ONE pass, then splice it into the live
      // buffer with ONE rebuild — clamped to the view's CURRENTLY VALID
      // indices (the buffer may have shrunk during a user comparefn).
      // Per-element Set semantics: a fixed view that no longer fits is
      // wholly out of bounds — NO index is valid, so nothing is written —
      // while a shrunk length-tracking view accepts its first live elements.
      let region = encode_region(kind, size, sorted)
      let avail = int.min(len, ta_live_length(state.heap, this)) * size
      let region = case avail == len * size {
        True -> region
        False -> bit_array.slice(region, 0, avail) |> result.unwrap(<<>>)
      }
      case avail > 0 {
        True -> {
          let h =
            write_buffer_data(
              state.heap,
              buffer,
              ta_splice(data, off, region),
              off,
              avail,
            )
          #(State(..state, heap: h), Ok(this))
        }
        False -> #(state, Ok(this))
      }
    }
  }
}

/// §23.2.3.33 toSorted ( comparefn ).
fn proto_to_sorted(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, sorted, state <- sorted_snapshot(this, args, state)
  let TaWitness(kind:, length: len, ..) = view
  use ta_val, new_buf, state <- try_state3(ta_same_type_create(state, kind, len))
  let size = value.typed_array_element_size(kind)
  // The fresh buffer is exactly len * size bytes, so the concatenated
  // region IS the new buffer contents — no splice needed.
  let new_data = case object.typed_array_buffer_data(state.heap, new_buf) {
    Some(_fresh) -> encode_region(kind, size, sorted)
    None -> ta_zeroed(len * size)
  }
  // Fresh buffer — this caller owns every byte.
  #(
    State(
      ..state,
      heap: write_buffer_data(
        state.heap,
        new_buf,
        new_data,
        0,
        bit_array.byte_size(new_data),
      ),
    ),
    Ok(ta_val),
  )
}

// ============================================================================
// toLocaleString — §23.2.3.30
// ============================================================================

fn proto_to_locale_string(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use view, state <- validate_ta(this, state)
  let len = view.length
  locale_loop(state, this, len, 0, [])
}

fn locale_loop(
  state: State(host),
  this: JsValue,
  len: Int,
  k: Int,
  acc: List(String),
) -> #(State(host), Result(JsValue, JsValue)) {
  use <- bool.guard(k >= len, #(
    state,
    Ok(JsString(string.join(list.reverse(acc), ","))),
  ))
  let el = ta_get(state.heap, this, k)
  case el {
    JsUndefined | value.JsNull ->
      locale_loop(state, this, len, k + 1, ["", ..acc])
    _ -> {
      use m, state <- try_state(object.get_value_of(
        state,
        el,
        Named("toLocaleString"),
      ))
      use res, state <- state.try_call(state, m, el, [])
      use s, state <- try_state(coerce.js_to_string(state, res))
      locale_loop(state, this, len, k + 1, [s, ..acc])
    }
  }
}

// ============================================================================
// TypedArraySpeciesCreate — §23.2.4.1
// ============================================================================

/// SpeciesConstructor(exemplar, %Kind%Array%) — §7.3.22. None means "use
/// the intrinsic default" (also chosen when @@species IS the intrinsic
/// constructor, which is observably identical and far cheaper).
fn resolve_species_ctor(
  state: State(host),
  exemplar: JsValue,
  kind: TypedArrayKind,
) -> Result(#(Option(JsValue), State(host)), #(JsValue, State(host))) {
  let default_ctor =
    state.builtins.typed_arrays
    |> list.key_find(kind)
    |> result.map(fn(bt: BuiltinType) { bt.constructor })
  // C = Get(exemplar, "constructor").
  use #(ctor, state) <- result.try(object.get_value_of(
    state,
    exemplar,
    Named("constructor"),
  ))
  case ctor {
    JsUndefined -> Ok(#(None, state))
    JsObject(_) -> {
      // S = Get(C, @@species); null/undefined → default.
      use #(species, state) <- result.try(object.get_symbol_value_of(
        state,
        ctor,
        value.symbol_species,
      ))
      case species {
        value.JsNull | JsUndefined -> Ok(#(None, state))
        JsObject(species_ref) ->
          case Ok(species_ref) == default_ctor {
            True -> Ok(#(None, state))
            False ->
              case object.is_constructor(state.heap, species) {
                True -> Ok(#(Some(species), state))
                False ->
                  Error(state.type_error_value(
                    state,
                    "Species constructor is not a constructor",
                  ))
              }
          }
        _ ->
          Error(state.type_error_value(
            state,
            "Species constructor is not a constructor",
          ))
      }
    }
    _ ->
      Error(state.type_error_value(
        state,
        "Constructor property is not an object",
      ))
  }
}

/// TypedArraySpeciesCreate post-construct check: the result's content type
/// (number vs bigint) must match the exemplar's.
fn check_content_type(
  state: State(host),
  obj: JsValue,
  obj_ref: Ref,
  kind: TypedArrayKind,
) -> Result(#(#(JsValue, Ref), State(host)), #(JsValue, State(host))) {
  case heap.read(state.heap, obj_ref) {
    Some(ObjectSlot(
      kind: value.TypedArrayObject(elem_kind: result_kind, ..),
      ..,
    )) ->
      case
        value.typed_array_is_bigint(result_kind)
        == value.typed_array_is_bigint(kind)
      {
        True -> Ok(#(#(obj, obj_ref), state))
        False ->
          Error(state.type_error_value(
            state,
            "Content types of source and created typed arrays differ",
          ))
      }
    _ -> Error(state.type_error_value(state, witness_message(NotTypedArray)))
  }
}

/// SpeciesConstructor(exemplar, default %Kind%Array%) then
/// TypedArrayCreateFromConstructor(« len »); content type must match.
fn ta_species_create(
  state: State(host),
  exemplar: JsValue,
  kind: TypedArrayKind,
  len: Int,
) -> Result(#(#(JsValue, Ref), State(host)), #(JsValue, State(host))) {
  use #(maybe_ctor, state) <- result.try(resolve_species_ctor(
    state,
    exemplar,
    kind,
  ))
  case maybe_ctor {
    None -> {
      use #(ta_val, _buf_ref, state) <- result.try(ta_same_type_create(
        state,
        kind,
        len,
      ))
      case ta_val {
        JsObject(r) -> Ok(#(#(ta_val, r), state))
        _ ->
          Error(state.type_error_value(state, "TypedArray allocation failed"))
      }
    }
    Some(species) -> {
      use #(obj, obj_ref, state) <- result.try(ta_create(state, species, len))
      check_content_type(state, obj, obj_ref, kind)
    }
  }
}

/// Like try_state but for triple results #(a, b, State).
fn try_state3(
  r: Result(#(a, b, State(host)), #(JsValue, State(host))),
  cont: fn(a, b, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case r {
    Ok(#(a, b, state)) -> cont(a, b, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

// ============================================================================
// Uint8Array base64/hex — proposal-arraybuffer-base64 (ES2026)
// toBase64 / toHex / setFromBase64 / setFromHex / fromBase64 / fromHex
// ============================================================================

const b64_alphabets = ["base64", "base64url"]

const b64_chunk_handlings = ["loose", "strict", "stop-before-partial"]

/// ValidateUint8Array — RequireInternalSlot([[TypedArrayName]]) plus the
/// Uint8Array brand check. Does NOT check buffer liveness (that happens
/// later, after option coercion, per GetUint8ArrayBytes ordering).
fn validate_u8(
  this: JsValue,
  state: State(host),
) -> Result(State(host), #(JsValue, State(host))) {
  case ta_slot(state.heap, this) {
    Some(TaView(kind: value.Uint8Kind, ..)) -> Ok(state)
    _ ->
      Error(state.type_error_value(
        state,
        "Method must be called on a Uint8Array",
      ))
  }
}

/// Immutable ArrayBuffer proposal: the write direction of ValidateUint8Array
/// — setFromBase64/setFromHex reject an immutable-backed target BEFORE any
/// option getter runs (observable; toBase64/toHex stay read-only).
fn u8_require_mutable(
  state: State(host),
  this: JsValue,
) -> Result(Nil, #(JsValue, State(host))) {
  let immutable = case ta_slot(state.heap, this) {
    Some(TaView(buffer:, ..)) ->
      case heap.read(state.heap, buffer) {
        Some(ObjectSlot(kind: value.ArrayBufferObject(immutable:, ..), ..)) ->
          immutable
        _ -> False
      }
    None -> False
  }
  case immutable {
    True ->
      Error(state.type_error_value(
        state,
        "Cannot modify a Uint8Array backed by an immutable ArrayBuffer",
      ))
    False -> Ok(Nil)
  }
}

/// MakeTypedArrayWithBufferWitnessRecord + IsTypedArrayOutOfBounds: resolve
/// the LIVE view right now (option getters may have detached/shrunk the
/// buffer). Returns (buffer ref, buffer data, byte offset, element length).
fn u8_live_view(
  state: State(host),
  this: JsValue,
) -> Result(#(Ref, BitArray, Int, Int), #(JsValue, State(host))) {
  case ta_slot(state.heap, this) {
    Some(TaView(buffer:, kind:, byte_offset: off, length: decl_len)) -> {
      let len =
        object.typed_array_view_length(state.heap, buffer, kind, off, decl_len)
      case object.typed_array_buffer_data(state.heap, buffer) {
        None -> Error(state.type_error_value(state, witness_message(Detached)))
        Some(data) ->
          case off + len > bit_array.byte_size(data) {
            True ->
              Error(state.type_error_value(state, witness_message(OutOfBounds)))
            False -> Ok(#(buffer, data, off, len))
          }
      }
    }
    None ->
      Error(state.type_error_value(
        state,
        "Method must be called on a Uint8Array",
      ))
  }
}

/// GetOptionsObject: undefined → absent, object → Some(ref), else TypeError.
fn get_opts_object(
  state: State(host),
  v: JsValue,
) -> Result(#(Option(Ref), State(host)), #(JsValue, State(host))) {
  case v {
    JsUndefined -> Ok(#(None, state))
    JsObject(ref) -> Ok(#(Some(ref), state))
    _ ->
      Error(state.type_error_value(
        state,
        "options must be an object or undefined",
      ))
  }
}

/// Get(opts, key) — observable property read on the options object.
fn get_option_value(
  state: State(host),
  opts: Option(Ref),
  key: String,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case opts {
    None -> Ok(#(JsUndefined, state))
    Some(ref) -> object.get_value(state, ref, Named(key), JsObject(ref))
  }
}

/// String-enum option per the proposal: undefined → default; a non-String
/// value or a String outside `allowed` → TypeError (NO ToString coercion).
fn get_string_enum_option(
  state: State(host),
  opts: Option(Ref),
  key: String,
  allowed: List(String),
  default: String,
) -> Result(#(String, State(host)), #(JsValue, State(host))) {
  use #(got, state) <- result.try(get_option_value(state, opts, key))
  case got {
    JsUndefined -> Ok(#(default, state))
    JsString(s) ->
      case list.contains(allowed, s) {
        True -> Ok(#(s, state))
        False ->
          Error(state.type_error_value(
            state,
            "\"" <> s <> "\" is not a valid value for option " <> key,
          ))
      }
    other ->
      Error(state.type_error_value(
        state,
        "option "
          <> key
          <> " must be a string, got "
          <> common.typeof_value(other, state.heap),
      ))
  }
}

/// Shared option reads of setFromBase64/fromBase64, in spec order:
/// GetOptionsObject, then "alphabet", then "lastChunkHandling".
fn read_b64_options(
  state: State(host),
  opt_arg: JsValue,
) -> Result(#(String, String, State(host)), #(JsValue, State(host))) {
  use #(opts, state) <- result.try(get_opts_object(state, opt_arg))
  use #(alphabet, state) <- result.try(get_string_enum_option(
    state,
    opts,
    "alphabet",
    b64_alphabets,
    "base64",
  ))
  use #(handling, state) <- result.map(get_string_enum_option(
    state,
    opts,
    "lastChunkHandling",
    b64_chunk_handlings,
    "loose",
  ))
  #(alphabet, handling, state)
}

/// Step 3 of setFromBase64/setFromHex/fromBase64/fromHex: the input must
/// already be a String — no coercion.
fn require_string(
  state: State(host),
  v: JsValue,
) -> Result(#(String, State(host)), #(JsValue, State(host))) {
  case v {
    JsString(s) -> Ok(#(s, state))
    other ->
      Error(state.type_error_value(
        state,
        "expected input to be a string, got "
          <> common.typeof_value(other, state.heap),
      ))
  }
}

/// Allocate the `{ read, written }` result object of setFromBase64/setFromHex.
fn read_written_result(
  state: State(host),
  read: Int,
  written: Int,
) -> #(JsValue, State(host)) {
  let #(heap, ref) =
    common.alloc_pojo(state.heap, state.builtins.object.prototype, [
      #("read", value.data_property(value.from_int(read))),
      #("written", value.data_property(value.from_int(written))),
    ])
  #(JsObject(ref), State(..state, heap:))
}

/// SetUint8ArrayBytes — splice the decoded bytes into the buffer at the
/// view's byte offset. Decoders never run user code, so `data` is current.
fn u8_write_bytes(
  state: State(host),
  buffer: Ref,
  data: BitArray,
  off: Int,
  bytes: BitArray,
) -> State(host) {
  case bit_array.byte_size(bytes) {
    0 -> state
    _ ->
      State(
        ..state,
        heap: write_buffer_data(
          state.heap,
          buffer,
          ta_splice(data, off, bytes),
          off,
          bit_array.byte_size(bytes),
        ),
      )
  }
}

/// Uint8Array.prototype.toBase64 ( [ options ] )
fn u8_to_base64(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  wrap({
    use state <- result.try(validate_u8(this, state))
    use #(opts, state) <- result.try(get_opts_object(
      state,
      helpers.first_arg_or_undefined(args),
    ))
    use #(alphabet, state) <- result.try(get_string_enum_option(
      state,
      opts,
      "alphabet",
      b64_alphabets,
      "base64",
    ))
    use #(omit_val, state) <- result.try(get_option_value(
      state,
      opts,
      "omitPadding",
    ))
    let padding = !value.is_truthy(omit_val)
    use #(_buffer, data, off, len) <- result.map(u8_live_view(state, this))
    let view = bit_array.slice(data, off, len) |> result.unwrap(<<>>)
    let out = case alphabet {
      "base64url" -> bit_array.base64_url_encode(view, padding)
      _ -> bit_array.base64_encode(view, padding)
    }
    #(JsString(out), state)
  })
}

/// Uint8Array.prototype.toHex ( )
fn u8_to_hex(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  wrap({
    use state <- result.try(validate_u8(this, state))
    use #(_buffer, data, off, len) <- result.map(u8_live_view(state, this))
    let view = bit_array.slice(data, off, len) |> result.unwrap(<<>>)
    #(JsString(string.lowercase(bit_array.base16_encode(view))), state)
  })
}

/// Uint8Array.prototype.setFromBase64 ( string [ , options ] )
fn u8_set_from_base64(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  wrap({
    use state <- result.try(validate_u8(this, state))
    use Nil <- result.try(u8_require_mutable(state, this))
    use #(s, state) <- result.try(require_string(
      state,
      helpers.first_arg_or_undefined(args),
    ))
    use #(alphabet, handling, state) <- result.try(read_b64_options(
      state,
      helpers.list_at(args, 1) |> option.unwrap(JsUndefined),
    ))
    use #(buffer, data, off, len) <- result.try(u8_live_view(state, this))
    let res = from_base64(s, alphabet == "base64url", handling, len)
    decode_into_view(state, buffer, data, off, res, "base64")
  })
}

/// Uint8Array.prototype.setFromHex ( string )
fn u8_set_from_hex(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  wrap({
    use state <- result.try(validate_u8(this, state))
    use Nil <- result.try(u8_require_mutable(state, this))
    use #(s, state) <- result.try(require_string(
      state,
      helpers.first_arg_or_undefined(args),
    ))
    use #(buffer, data, off, len) <- result.try(u8_live_view(state, this))
    let res = from_hex(s, len)
    decode_into_view(state, buffer, data, off, res, "hex")
  })
}

fn decode_error(state: State(host), codec: String) -> #(JsValue, State(host)) {
  state.syntax_error_value(state, "unable to decode " <> codec <> " string")
}

/// Shared setFromBase64/setFromHex tail. Per spec, bytes decoded before an
/// error ARE written, THEN the SyntaxError is thrown ("writes up to error").
fn decode_into_view(
  state: State(host),
  buffer: Ref,
  data: BitArray,
  off: Int,
  res: DecodeResult,
  codec: String,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case res {
    DecodeFailed(partial:) ->
      Error(decode_error(
        u8_write_bytes(state, buffer, data, off, partial),
        codec,
      ))
    Decoded(read:, bytes:) -> {
      let state = u8_write_bytes(state, buffer, data, off, bytes)
      Ok(read_written_result(state, read, bit_array.byte_size(bytes)))
    }
  }
}

/// Shared fromBase64/fromHex tail: allocate a fresh Uint8Array on success.
/// (No view to write into here, so a failed decode's partial bytes are
/// dropped — only the SyntaxError surfaces.)
fn decode_to_new_u8(
  state: State(host),
  res: DecodeResult,
  codec: String,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case res {
    DecodeFailed(partial: _) -> Error(decode_error(state, codec))
    Decoded(read: _, bytes:) -> u8_alloc_from_bytes(state, bytes)
  }
}

/// Uint8Array.fromBase64 ( string [ , options ] )
fn u8_from_base64(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  wrap({
    use #(s, state) <- result.try(require_string(
      state,
      helpers.first_arg_or_undefined(args),
    ))
    use #(alphabet, handling, state) <- result.try(read_b64_options(
      state,
      helpers.list_at(args, 1) |> option.unwrap(JsUndefined),
    ))
    let res =
      from_base64(s, alphabet == "base64url", handling, max_safe_integer)
    decode_to_new_u8(state, res, "base64")
  })
}

/// Uint8Array.fromHex ( string )
fn u8_from_hex(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  wrap({
    use #(s, state) <- result.try(require_string(
      state,
      helpers.first_arg_or_undefined(args),
    ))
    let res = from_hex(s, max_safe_integer)
    decode_to_new_u8(state, res, "hex")
  })
}

/// Allocate a fresh Uint8Array holding exactly `bytes`.
fn u8_alloc_from_bytes(
  state: State(host),
  bytes: BitArray,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  let len = bit_array.byte_size(bytes)
  use #(ta, state) <- result.map(alloc_ta_with_length(
    state,
    value.Uint8Kind,
    default_proto_for(state, value.Uint8Kind),
    len,
  ))
  // Fresh buffer — this caller owns every byte.
  case ta_slot(state.heap, ta) {
    Some(TaView(buffer:, ..)) -> #(
      ta,
      State(..state, heap: write_buffer_data(state.heap, buffer, bytes, 0, len)),
    )
    None -> #(ta, state)
  }
}

// ----------------------------------------------------------------------------
// FromBase64 / FromHex — the decoders (no user code runs inside).
// `read` is in string code units; positions it reports always fall inside an
// all-ASCII prefix, so iterating the UTF-8 bytes keeps the counts exact (any
// non-ASCII byte is an immediate decode error).
// ----------------------------------------------------------------------------

/// Result of FromBase64/FromHex. `Decoded` is a clean decode: `read` input
/// code units consumed and the `bytes` they produced. `DecodeFailed` is the
/// spec record that carried a SyntaxError; `partial` holds the bytes decoded
/// BEFORE the error (setFromBase64/setFromHex still write those, then throw).
type DecodeResult {
  Decoded(read: Int, bytes: BitArray)
  DecodeFailed(partial: BitArray)
}

/// Join decoded chunks (accumulated newest-first) into the final byte string.
/// A single concat at the end keeps decoding O(n); appending to a growing
/// BitArray per chunk copies the whole accumulator each time (O(n^2)).
fn decode_bytes(acc: List(BitArray)) -> BitArray {
  bit_array.concat(list.reverse(acc))
}

/// FromBase64 ( string, alphabet, lastChunkHandling, maxLength )
fn from_base64(
  s: String,
  url: Bool,
  handling: String,
  max_len: Int,
) -> DecodeResult {
  use <- bool.guard(max_len == 0, Decoded(0, <<>>))
  b64_loop(bit_array.from_string(s), 0, 0, [], 0, 0, 0, url, handling, max_len)
}

/// SkipAsciiWhitespace — TAB LF FF CR SPACE.
fn b64_skip_ws(bin: BitArray, index: Int) -> #(BitArray, Int) {
  case bin {
    <<c, rest:bits>> if c == 9 || c == 10 || c == 12 || c == 13 || c == 32 ->
      b64_skip_ws(rest, index + 1)
    _ -> #(bin, index)
  }
}

fn b64_loop(
  bin: BitArray,
  index: Int,
  read: Int,
  acc: List(BitArray),
  written: Int,
  chunk: Int,
  chunk_len: Int,
  url: Bool,
  handling: String,
  max_len: Int,
) -> DecodeResult {
  let #(bin, index) = b64_skip_ws(bin, index)
  case bin {
    <<>> ->
      case chunk_len > 0 {
        True ->
          case handling {
            "stop-before-partial" -> Decoded(read, decode_bytes(acc))
            "loose" ->
              case chunk_len == 1 {
                True -> DecodeFailed(decode_bytes(acc))
                False ->
                  case b64_decode_partial(chunk, chunk_len, False) {
                    Some(tail) -> Decoded(index, decode_bytes([tail, ..acc]))
                    None -> DecodeFailed(decode_bytes(acc))
                  }
              }
            _ -> DecodeFailed(decode_bytes(acc))
          }
        False -> Decoded(index, decode_bytes(acc))
      }
    // '='
    <<61, rest:bits>> ->
      b64_padding(rest, index + 1, read, acc, chunk, chunk_len, handling)
    <<c, rest:bits>> ->
      case b64_value(c, url) {
        None -> DecodeFailed(decode_bytes(acc))
        Some(v) -> {
          let remaining = max_len - written
          let stop =
            { remaining == 1 && chunk_len == 2 }
            || { remaining == 2 && chunk_len == 3 }
          case stop {
            True -> Decoded(read, decode_bytes(acc))
            False -> {
              let chunk = chunk * 64 + v
              case chunk_len + 1 == 4 {
                True -> {
                  let acc = [<<chunk:size(24)>>, ..acc]
                  let written = written + 3
                  case written == max_len {
                    True -> Decoded(index + 1, decode_bytes(acc))
                    False ->
                      b64_loop(
                        rest,
                        index + 1,
                        index + 1,
                        acc,
                        written,
                        0,
                        0,
                        url,
                        handling,
                        max_len,
                      )
                  }
                }
                False ->
                  b64_loop(
                    rest,
                    index + 1,
                    read,
                    acc,
                    written,
                    chunk,
                    chunk_len + 1,
                    url,
                    handling,
                    max_len,
                  )
              }
            }
          }
        }
      }
    // Unreachable: the input is a UTF-8 binary, always whole bytes.
    _ -> DecodeFailed(decode_bytes(acc))
  }
}

/// The '=' branch of FromBase64: validate padding, decode the partial chunk.
/// On entry `index` is the position just after the first '='.
fn b64_padding(
  bin: BitArray,
  index: Int,
  read: Int,
  acc: List(BitArray),
  chunk: Int,
  chunk_len: Int,
  handling: String,
) -> DecodeResult {
  use <- bool.guard(chunk_len < 2, DecodeFailed(decode_bytes(acc)))
  let #(bin, index) = b64_skip_ws(bin, index)
  case chunk_len == 2 {
    True ->
      case bin {
        <<>> ->
          case handling == "stop-before-partial" {
            True -> Decoded(read, decode_bytes(acc))
            False -> DecodeFailed(decode_bytes(acc))
          }
        // second '='
        <<61, rest:bits>> -> {
          let #(rest, index) = b64_skip_ws(rest, index + 1)
          b64_finish_padding(rest, index, acc, chunk, chunk_len, handling)
        }
        _ -> DecodeFailed(decode_bytes(acc))
      }
    False -> b64_finish_padding(bin, index, acc, chunk, chunk_len, handling)
  }
}

/// After padding: anything left in the string is an error; otherwise decode
/// the 2- or 3-char chunk ("strict" rejects non-zero extra bits).
fn b64_finish_padding(
  bin: BitArray,
  index: Int,
  acc: List(BitArray),
  chunk: Int,
  chunk_len: Int,
  handling: String,
) -> DecodeResult {
  case bin {
    <<>> ->
      case b64_decode_partial(chunk, chunk_len, handling == "strict") {
        Some(tail) -> Decoded(index, decode_bytes([tail, ..acc]))
        None -> DecodeFailed(decode_bytes(acc))
      }
    _ -> DecodeFailed(decode_bytes(acc))
  }
}

/// DecodeBase64Chunk for 2- or 3-char chunks. None = non-zero extra bits
/// rejected under `throw_on_extra_bits`.
fn b64_decode_partial(
  chunk: Int,
  chunk_len: Int,
  throw_on_extra_bits: Bool,
) -> Option(BitArray) {
  case chunk_len {
    2 -> {
      // 12 bits: 1 byte + 4 extra bits.
      let extra = int.bitwise_and(chunk, 0xF)
      case throw_on_extra_bits && extra != 0 {
        True -> None
        False -> Some(<<int.bitwise_shift_right(chunk, 4)>>)
      }
    }
    _ -> {
      // 18 bits: 2 bytes + 2 extra bits.
      let extra = int.bitwise_and(chunk, 0x3)
      case throw_on_extra_bits && extra != 0 {
        True -> None
        False -> Some(<<int.bitwise_shift_right(chunk, 2):size(16)>>)
      }
    }
  }
}

/// Map a base64 character (as its code unit) to its 6-bit value. In the
/// base64url alphabet '-'/'_' replace '+'/'/'.
fn b64_value(c: Int, url: Bool) -> Option(Int) {
  case c {
    _ if c >= 65 && c <= 90 -> Some(c - 65)
    _ if c >= 97 && c <= 122 -> Some(c - 71)
    _ if c >= 48 && c <= 57 -> Some(c + 4)
    43 if !url -> Some(62)
    47 if !url -> Some(63)
    45 if url -> Some(62)
    95 if url -> Some(63)
    _ -> None
  }
}

/// FromHex ( string, maxLength )
fn from_hex(s: String, max_len: Int) -> DecodeResult {
  // The odd-length check is on the string's UTF-16 length, not its UTF-8
  // byte count (they can differ when the bad char is non-ASCII).
  case object.string_length(s) % 2 != 0 {
    True -> DecodeFailed(<<>>)
    False -> hex_loop(bit_array.from_string(s), 0, [], 0, max_len)
  }
}

fn hex_loop(
  bin: BitArray,
  read: Int,
  acc: List(BitArray),
  written: Int,
  max_len: Int,
) -> DecodeResult {
  case bin {
    <<>> -> Decoded(read, decode_bytes(acc))
    _ if written >= max_len -> Decoded(read, decode_bytes(acc))
    <<h1, h2, rest:bits>> ->
      case hex_value(h1), hex_value(h2) {
        Some(a), Some(b) -> {
          let byte = a * 16 + b
          hex_loop(rest, read + 2, [<<byte>>, ..acc], written + 1, max_len)
        }
        _, _ -> DecodeFailed(decode_bytes(acc))
      }
    _ -> DecodeFailed(decode_bytes(acc))
  }
}

fn hex_value(c: Int) -> Option(Int) {
  case c {
    _ if c >= 48 && c <= 57 -> Some(c - 48)
    _ if c >= 97 && c <= 102 -> Some(c - 87)
    _ if c >= 65 && c <= 70 -> Some(c - 55)
    _ -> None
  }
}

// ============================================================================
// Small shared helpers
// ============================================================================

/// The intrinsic default prototype for a concrete TypedArray kind.
fn default_proto_for(state: State(host), kind: TypedArrayKind) -> Ref {
  state.builtins.typed_arrays
  |> list.key_find(kind)
  |> result.map(fn(bt: BuiltinType) { bt.prototype })
  |> result.unwrap(state.builtins.typed_array.prototype)
}

/// Replace a buffer slot's backing bytes with a full-buffer image,
/// preserving its other fields. `byte_offset`/`count` delimit the byte range
/// the caller actually modified: shared (atomics-backed) storage writes ONLY
/// those bytes into the shared cells — other regions may be concurrently
/// mutated by other agent processes, and writing the whole snapshot back
/// would clobber their updates. (Non-shared storage just swaps in the full
/// image.) Callers that own the whole buffer — a freshly allocated result —
/// pass the full range.
/// Immutable buffers (immutable-arraybuffer proposal) are never modified —
/// the write is dropped, mirroring the detached-buffer no-op treatment.
fn write_buffer_data(
  h: Heap(host),
  buffer: Ref,
  new_data: BitArray,
  byte_offset: Int,
  count: Int,
) -> Heap(host) {
  heap.update(h, buffer, fn(slot) {
    case slot {
      ObjectSlot(kind: value.ArrayBufferObject(immutable: True, ..), ..) as s ->
        s
      ObjectSlot(
        kind: value.ArrayBufferObject(
          detached:,
          max_byte_length:,
          immutable:,
          data: old_data,
        ),
        ..,
      ) as s ->
        ObjectSlot(
          ..s,
          kind: value.ArrayBufferObject(
            data: value.buffer_store_region(
              old_data,
              new_data,
              byte_offset,
              count,
            ),
            detached:,
            max_byte_length:,
            immutable:,
          ),
        )
      other -> other
    }
  })
}

/// `use`-style adapter: thread Result(#(a, State), #(thrown, State)) into a
/// builtin-shaped continuation.
fn try_state(
  r: Result(#(a, State(host)), #(JsValue, State(host))),
  cont: fn(a, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case r {
    Ok(#(v, state)) -> cont(v, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Like try_state but for ops returning #(State, Bool) pairs (set_value).
fn try_state_pair(
  r: Result(#(State(host), Bool), #(JsValue, State(host))),
  cont: fn(#(State(host), Bool)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case r {
    Ok(pair) -> cont(pair)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}
