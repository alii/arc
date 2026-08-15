//// ES2024 §23.2 TypedArray Objects
////
//// %TypedArray% is an abstract intrinsic constructor: it has [[Construct]]
//// (so it can appear in `extends` and Reflect.construct type checks) but
//// throws TypeError when invoked. The 11 concrete constructors
//// (Int8Array .. BigUint64Array) inherit from it (both the functions and
//// their prototypes), and produce Integer-Indexed exotic objects backed by
//// an ArrayBufferObj cell.
////
//// Element reads, element stores and bulk encoding live in arc/rt/buffer
//// (next to the MOP arms in arc/rt/obj that need them); this module is the
//// constructor and prototype surface.

import arc/rt/buffer
import arc/rt/builtins/array_buffer
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/iter_protocol
import arc/rt/builtins/realm_ops
import arc/rt/builtins/uint8_codec
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/typed_array_ffi.{fill_clamped, splice_clamped, ta_zeroed}
import arc/rt/types.{
  type Agent, type ArrayIterKind, type BuiltinPair, type Handle, type JsNum,
  type JsVal, type TypedArrayKind, type TypedArrayNative, type TypedArrays,
  ArrayIterEntries, ArrayIterKeys, ArrayIterValues, ArrayIterator, BigKind,
  Index, JFloat, JInt, JNan, JNegInf, JPosInf, KBig, KHandle, KNull, KNum,
  KUndef, Named, NumKind, ReturnThis, SObject, StringKey, SymbolKey,
  TypedArrayConstructor, TypedArrayFrom, TypedArrayGetBuffer,
  TypedArrayGetByteLength, TypedArrayGetByteOffset, TypedArrayGetLength,
  TypedArrayGetToStringTag, TypedArrayIntrinsicConstructor, TypedArrayN,
  TypedArrayObj, TypedArrayOf, TypedArrayPrototypeAt,
  TypedArrayPrototypeCopyWithin, TypedArrayPrototypeEntries,
  TypedArrayPrototypeEvery, TypedArrayPrototypeFill, TypedArrayPrototypeFilter,
  TypedArrayPrototypeFind, TypedArrayPrototypeFindIndex,
  TypedArrayPrototypeFindLast, TypedArrayPrototypeFindLastIndex,
  TypedArrayPrototypeForEach, TypedArrayPrototypeIncludes,
  TypedArrayPrototypeIndexOf, TypedArrayPrototypeJoin, TypedArrayPrototypeKeys,
  TypedArrayPrototypeLastIndexOf, TypedArrayPrototypeMap,
  TypedArrayPrototypeReduce, TypedArrayPrototypeReduceRight,
  TypedArrayPrototypeReverse, TypedArrayPrototypeSet, TypedArrayPrototypeSlice,
  TypedArrayPrototypeSome, TypedArrayPrototypeSort, TypedArrayPrototypeSubarray,
  TypedArrayPrototypeToLocaleString, TypedArrayPrototypeToReversed,
  TypedArrayPrototypeToSorted, TypedArrayPrototypeValues,
  TypedArrayPrototypeWith, TypedArrays, Uint8ArrayFromBase64, Uint8ArrayFromHex,
  Uint8ArrayPrototypeSetFromBase64, Uint8ArrayPrototypeSetFromHex,
  Uint8ArrayPrototypeToBase64, Uint8ArrayPrototypeToHex, Uint8Kind,
  all_typed_array_kinds, classify, mk_bool, mk_number, mk_object, mk_string,
  mk_undefined, symbol_iterator, symbol_species, symbol_to_string_tag,
  typed_array_name,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Hard cap on a typed array's backing byte length (matches the engine's
/// practical allocation limit; over this → RangeError like real engines).
const max_byte_length = 2_147_483_647

/// 2^53 - 1 — MAX_SAFE_INTEGER, the ToIndex/ToLength upper bound.
const max_safe_integer = 9_007_199_254_740_991

/// §23.2 [[ContentType]] agreement: a BigInt view and a Number view never
/// mix (construct-from, set(), species-create all reject the pairing).
fn same_content_type(a: TypedArrayKind, b: TypedArrayKind) -> Bool {
  case a, b {
    NumKind(_), NumKind(_) -> True
    BigKind(_), BigKind(_) -> True
    NumKind(_), BigKind(_) | BigKind(_), NumKind(_) -> False
  }
}

/// [[TypedArrayName]] — kept for `init_realm`'s global installation.
pub fn kind_name(kind: TypedArrayKind) -> String {
  typed_array_name(kind)
}

// ============================================================================
// Init — %TypedArray%, %TypedArray%.prototype, and the 11 concrete ctors
// ============================================================================

pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
  array: BuiltinPair,
) -> #(#(BuiltinPair, TypedArrays), Agent) {
  // Accessor getters on %TypedArray%.prototype (§23.2.3.1-3, .18).
  let #(getters, st) =
    common.alloc_getters(st, function_proto, [
      #("buffer", TypedArrayN(TypedArrayGetBuffer)),
      #("byteLength", TypedArrayN(TypedArrayGetByteLength)),
      #("byteOffset", TypedArrayN(TypedArrayGetByteOffset)),
      #("length", TypedArrayN(TypedArrayGetLength)),
    ])
  // values() doubles as [@@iterator] — must be the SAME function object.
  let #(values_h, st) =
    common.alloc_rooted_native_fn(
      st,
      function_proto,
      TypedArrayN(TypedArrayPrototypeValues),
      "values",
      0,
    )
  let #(values_prop, st) = common.builtin_property(st, mk_object(values_h))
  // §23.2.3.32 %TypedArray%.prototype.toString IS %Array.prototype.toString%
  // — the same function object.
  let #(array_to_string, st) =
    rt_obj.t_get_prop(
      st,
      mk_object(array.prototype),
      StringKey(Named("toString")),
    )
  let #(to_string_prop, st) = common.builtin_property(st, array_to_string)
  let #(methods, st) =
    common.alloc_methods(st, function_proto, [
      #("at", TypedArrayN(TypedArrayPrototypeAt), 1),
      #("fill", TypedArrayN(TypedArrayPrototypeFill), 1),
      #("set", TypedArrayN(TypedArrayPrototypeSet), 1),
      #("subarray", TypedArrayN(TypedArrayPrototypeSubarray), 2),
      #("slice", TypedArrayN(TypedArrayPrototypeSlice), 2),
      #("join", TypedArrayN(TypedArrayPrototypeJoin), 1),
      #("indexOf", TypedArrayN(TypedArrayPrototypeIndexOf), 1),
      #("includes", TypedArrayN(TypedArrayPrototypeIncludes), 1),
      #("keys", TypedArrayN(TypedArrayPrototypeKeys), 0),
      #("entries", TypedArrayN(TypedArrayPrototypeEntries), 0),
      #("copyWithin", TypedArrayN(TypedArrayPrototypeCopyWithin), 2),
      #("every", TypedArrayN(TypedArrayPrototypeEvery), 1),
      #("some", TypedArrayN(TypedArrayPrototypeSome), 1),
      #("forEach", TypedArrayN(TypedArrayPrototypeForEach), 1),
      #("map", TypedArrayN(TypedArrayPrototypeMap), 1),
      #("filter", TypedArrayN(TypedArrayPrototypeFilter), 1),
      #("find", TypedArrayN(TypedArrayPrototypeFind), 1),
      #("findIndex", TypedArrayN(TypedArrayPrototypeFindIndex), 1),
      #("findLast", TypedArrayN(TypedArrayPrototypeFindLast), 1),
      #("findLastIndex", TypedArrayN(TypedArrayPrototypeFindLastIndex), 1),
      #("lastIndexOf", TypedArrayN(TypedArrayPrototypeLastIndexOf), 1),
      #("reduce", TypedArrayN(TypedArrayPrototypeReduce), 1),
      #("reduceRight", TypedArrayN(TypedArrayPrototypeReduceRight), 1),
      #("reverse", TypedArrayN(TypedArrayPrototypeReverse), 0),
      #("toReversed", TypedArrayN(TypedArrayPrototypeToReversed), 0),
      #("sort", TypedArrayN(TypedArrayPrototypeSort), 1),
      #("toSorted", TypedArrayN(TypedArrayPrototypeToSorted), 1),
      #("toLocaleString", TypedArrayN(TypedArrayPrototypeToLocaleString), 0),
      #("with", TypedArrayN(TypedArrayPrototypeWith), 2),
    ])
  let proto_props =
    list.flatten([
      getters,
      [#("values", values_prop), #("toString", to_string_prop)],
      methods,
    ])
  // %TypedArray%.from / %TypedArray%.of — statics inherited by all 11 ctors.
  let #(statics, st) =
    common.alloc_methods(st, function_proto, [
      #("from", TypedArrayN(TypedArrayFrom), 1),
      #("of", TypedArrayN(TypedArrayOf), 0),
    ])
  let #(ta, st) =
    common.init_type(
      st,
      object_proto,
      function_proto,
      proto_props,
      fn(_proto) { TypedArrayN(TypedArrayIntrinsicConstructor) },
      "TypedArray",
      0,
      statics,
    )
  // %TypedArray%.prototype[@@iterator] === %TypedArray%.prototype.values
  let #(iter_prop, st) = common.restamp(st, values_prop)
  let st =
    common.add_symbol_property(st, ta.prototype, symbol_iterator, iter_prop)
  // get %TypedArray%.prototype[@@toStringTag] (§23.2.3.38) — an accessor that
  // returns the [[TypedArrayName]] (undefined for non-TypedArray receivers).
  let #(tag_get, st) =
    common.alloc_rooted_native_fn(
      st,
      function_proto,
      TypedArrayN(TypedArrayGetToStringTag),
      "get [Symbol.toStringTag]",
      0,
    )
  let #(tag_prop, st) =
    common.accessor_prop(
      st,
      get: Some(mk_object(tag_get)),
      set: None,
      enumerable: False,
      configurable: True,
    )
  let st =
    common.add_symbol_property(st, ta.prototype, symbol_to_string_tag, tag_prop)
  // get %TypedArray%[@@species] (§23.2.2.4) — returns `this`.
  let st =
    common.add_species_accessor(st, function_proto, ta.constructor, ReturnThis)
  // The 11 concrete constructors, allocated in `all_typed_array_kinds` order.
  // Each ctor's [[Prototype]] is %TypedArray% itself, and each prototype's
  // [[Prototype]] is %TypedArray%.prototype (§23.2.5/§23.2.6/§23.2.7).
  let #(by_kind, st) =
    list.fold(all_typed_array_kinds, #(dict.new(), st), fn(acc, kind) {
      let #(d, st) = acc
      let #(pair, st) = init_ctor(st, ta, function_proto, kind)
      #(dict.insert(d, kind, pair), st)
    })
  #(#(ta, TypedArrays(by_kind:)), st)
}

/// One concrete TypedArray constructor + prototype. BYTES_PER_ELEMENT is
/// {W:F, E:F, C:F} on both.
fn init_ctor(
  st: Agent,
  ta: BuiltinPair,
  function_proto: Handle,
  kind: TypedArrayKind,
) -> #(BuiltinPair, Agent) {
  let size = typed_array_ffi.elem_size(kind)
  let #(size_prop, st) = common.data_prop(st, mk_number(JInt(size)))
  let #(size_prop2, st) = common.restamp(st, size_prop)
  // §23.2.6.2: the ctor's "prototype" property is {W:F, E:F, C:F} —
  // installed that way by common.init_type.
  let #(bt, st) =
    common.init_type(
      st,
      ta.prototype,
      ta.constructor,
      [#("BYTES_PER_ELEMENT", size_prop)],
      fn(proto) { TypedArrayN(TypedArrayConstructor(kind:, proto:)) },
      typed_array_name(kind),
      3,
      [#("BYTES_PER_ELEMENT", size_prop2)],
    )
  // proposal-arraybuffer-base64: own methods of Uint8Array.prototype and
  // statics of the Uint8Array constructor (NOT on %TypedArray%).
  let st = case kind {
    NumKind(Uint8Kind) -> {
      let #(u8_methods, st) =
        common.alloc_methods(st, function_proto, [
          #("toBase64", TypedArrayN(Uint8ArrayPrototypeToBase64), 0),
          #("toHex", TypedArrayN(Uint8ArrayPrototypeToHex), 0),
          #("setFromBase64", TypedArrayN(Uint8ArrayPrototypeSetFromBase64), 1),
          #("setFromHex", TypedArrayN(Uint8ArrayPrototypeSetFromHex), 1),
        ])
      let st = add_named_props(st, bt.prototype, u8_methods)
      let #(u8_statics, st) =
        common.alloc_methods(st, function_proto, [
          #("fromBase64", TypedArrayN(Uint8ArrayFromBase64), 1),
          #("fromHex", TypedArrayN(Uint8ArrayFromHex), 1),
        ])
      add_named_props(st, bt.constructor, u8_statics)
    }
    _ -> st
  }
  #(bt, st)
}

/// Insert named properties into an existing object cell.
fn add_named_props(
  st: Agent,
  ref: Handle,
  props: List(#(String, types.Property)),
) -> Agent {
  use st, #(name, prop) <- list.fold(props, st)
  common.add_named_property(st, ref, name, prop)
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module [[Call]] dispatch. All TypedArray constructors throw without
/// `new` (§23.2.1.1 step 1 / §23.2.5.1 step 1).
pub fn dispatch(
  st: Agent,
  native: TypedArrayNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    // §23.2.1.1 %TypedArray%: always throws.
    TypedArrayIntrinsicConstructor ->
      rt_val.t_throw_type_error(
        st,
        "Abstract class TypedArray not directly constructable",
      )
    TypedArrayConstructor(kind:, ..) ->
      rt_val.t_throw_type_error(
        st,
        "Constructor " <> typed_array_name(kind) <> " requires 'new'",
      )
    TypedArrayGetBuffer -> get_buffer(st, this)
    TypedArrayGetByteLength -> get_byte_length(st, this)
    TypedArrayGetByteOffset -> get_byte_offset(st, this)
    TypedArrayGetLength -> get_length(st, this)
    TypedArrayGetToStringTag -> get_to_string_tag(st, this)
    TypedArrayPrototypeAt -> proto_at(st, this, args)
    TypedArrayPrototypeFill -> proto_fill(st, this, args)
    TypedArrayPrototypeSet -> proto_set(st, this, args)
    TypedArrayPrototypeSubarray -> proto_subarray(st, this, args)
    TypedArrayPrototypeSlice -> proto_slice(st, this, args)
    TypedArrayPrototypeJoin -> proto_join(st, this, args)
    TypedArrayPrototypeIndexOf -> proto_index_of(st, this, args)
    TypedArrayPrototypeIncludes -> proto_includes(st, this, args)
    TypedArrayPrototypeKeys -> proto_iter(st, this, ArrayIterKeys)
    TypedArrayPrototypeValues -> proto_iter(st, this, ArrayIterValues)
    TypedArrayPrototypeEntries -> proto_iter(st, this, ArrayIterEntries)
    TypedArrayPrototypeCopyWithin -> proto_copy_within(st, this, args)
    TypedArrayPrototypeEvery -> proto_every_some(st, this, args, True)
    TypedArrayPrototypeSome -> proto_every_some(st, this, args, False)
    TypedArrayPrototypeForEach -> proto_for_each(st, this, args)
    TypedArrayPrototypeMap -> proto_map(st, this, args)
    TypedArrayPrototypeFilter -> proto_filter(st, this, args)
    TypedArrayPrototypeFind -> proto_find(st, this, args, Ascending, FindValue)
    TypedArrayPrototypeFindIndex ->
      proto_find(st, this, args, Ascending, FindIdx)
    TypedArrayPrototypeFindLast ->
      proto_find(st, this, args, Descending, FindValue)
    TypedArrayPrototypeFindLastIndex ->
      proto_find(st, this, args, Descending, FindIdx)
    TypedArrayPrototypeLastIndexOf -> proto_last_index_of(st, this, args)
    TypedArrayPrototypeReduce -> proto_reduce(st, this, args, Ascending)
    TypedArrayPrototypeReduceRight -> proto_reduce(st, this, args, Descending)
    TypedArrayPrototypeReverse -> proto_reverse(st, this)
    TypedArrayPrototypeToReversed -> proto_to_reversed(st, this)
    TypedArrayPrototypeSort -> proto_sort(st, this, args)
    TypedArrayPrototypeToSorted -> proto_to_sorted(st, this, args)
    TypedArrayPrototypeToLocaleString -> proto_to_locale_string(st, this)
    TypedArrayPrototypeWith -> proto_with(st, this, args)
    TypedArrayFrom -> ta_from(st, this, args)
    TypedArrayOf -> ta_of(st, this, args)
    Uint8ArrayPrototypeToBase64 -> uint8_codec.u8_to_base64(st, this, args)
    Uint8ArrayPrototypeToHex -> uint8_codec.u8_to_hex(st, this)
    Uint8ArrayPrototypeSetFromBase64 ->
      uint8_codec.u8_set_from_base64(st, this, args)
    Uint8ArrayPrototypeSetFromHex -> uint8_codec.u8_set_from_hex(st, this, args)
    Uint8ArrayFromBase64 -> uint8_codec.u8_from_base64(st, args)
    Uint8ArrayFromHex -> uint8_codec.u8_from_hex(st, args)
  }
}

/// Per-module [[Construct]] dispatch — §23.2.5.1.
pub fn dispatch_construct(
  st: Agent,
  native: TypedArrayNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    // §23.2.1.1 %TypedArray%: always throws.
    TypedArrayIntrinsicConstructor ->
      rt_val.t_throw_type_error(
        st,
        "Abstract class TypedArray not directly constructable",
      )
    TypedArrayConstructor(kind:, proto:) ->
      ta_construct(st, kind, new_target, proto, args)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
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
fn ta_create(st: Agent, ctor: JsVal, len: Int) -> #(JsVal, Handle, Agent) {
  let #(obj, obj_h, st) =
    ta_create_with_args(st, ctor, [mk_number(JInt(len))], Some(len))
  let immutable = case ta_slot_of(st, obj_h) {
    Some(view) -> buffer.buffer_is_immutable(st, view.buffer)
    None -> False
  }
  case immutable {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Constructor returned a TypedArray backed by an immutable ArrayBuffer",
      )
    False -> #(obj, obj_h, st)
  }
}

/// TypedArrayCreateFromConstructor (§23.2.4.2) with an arbitrary argument
/// list. `min_len` enables the single-Number-argument length check.
fn ta_create_with_args(
  st: Agent,
  ctor: JsVal,
  ctor_args: List(JsVal),
  min_len: Option(Int),
) -> #(JsVal, Handle, Agent) {
  let #(obj_h, st) = rt_call.t_construct(st, ctor, ctor_args, ctor)
  let obj = mk_object(obj_h)
  case ta_slot_of(st, obj_h) {
    Some(view) ->
      // Step 2: ValidateTypedArray(newTypedArray) — a constructor that
      // returned a view over a detached buffer (or one that no longer
      // fits its resizable buffer) throws TypeError.
      case view_witness_bytes(st, view) {
        Error(err) -> witness_type_error(st, err)
        Ok(_bytes) -> {
          let l =
            buffer.typed_array_view_length(
              st,
              view.buffer,
              view.kind,
              view.byte_offset,
              view.length,
            )
          case min_len {
            Some(len) if l < len ->
              rt_val.t_throw_type_error(
                st,
                "Derived TypedArray constructor created an array which was too small",
              )
            _ -> #(obj, view.ref, st)
          }
        }
      }
    None -> witness_type_error(st, buffer.NotAView)
  }
}

/// §23.2.2.1 %TypedArray%.from ( source [ , mapfn [ , thisArg ] ] )
fn ta_from(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use <- bool.lazy_guard(!rt_call.is_constructor(st, this), fn() {
    rt_val.t_throw_type_error(st, "%TypedArray%.from called on non-constructor")
  })
  let source = helpers.first_arg_or_undefined(args)
  let mapfn = helpers.arg_at(args, 1)
  let this_arg = helpers.arg_at(args, 2)
  // Step 3: mapping check.
  let mapping = case classify(mapfn) {
    KUndef -> None
    _ ->
      case rt_call.is_callable(st, mapfn) {
        True -> Some(mapfn)
        False -> rt_val.t_throw_type_error(st, "mapfn is not a function")
      }
  }
  use <- bool.lazy_guard(rt_val.is_nullish(source), fn() {
    rt_val.t_throw_type_error(st, "Cannot convert undefined or null to object")
  })
  // Step 5: usingIterator = GetMethod(source, @@iterator).
  let #(iter_fn, st) = rt_obj.t_get_prop(st, source, SymbolKey(symbol_iterator))
  case rt_call.is_callable(st, iter_fn) {
    True -> {
      let #(rec, st) =
        iter_protocol.get_iterator_from_method(st, source, iter_fn)
      let #(values, st) = iter_protocol.iterator_to_list(st, rec)
      let #(target, target_h, st) = ta_create(st, this, list.length(values))
      let bulk = case mapping {
        None -> try_bulk_store(st, target_h, 0, values)
        Some(_) -> None
      }
      case bulk {
        Some(st) -> #(target, st)
        None ->
          from_store_loop(st, target, target_h, values, 0, mapping, this_arg)
      }
    }
    False -> {
      // Step 7: array-like path (works on primitives like strings too).
      let #(len_val, st) =
        rt_obj.t_get_prop(st, source, StringKey(Named("length")))
      let #(len, st) = rt_val.t_to_length(st, len_val)
      let #(target, target_h, st) = ta_create(st, this, len)
      let bulk = case mapping, classify(source) {
        None, KHandle(src_h) ->
          buffer.plain_indexed_values(st, src_h, len)
          |> option.then(try_bulk_store(st, target_h, 0, _))
        _, _ -> None
      }
      case bulk {
        Some(st) -> #(target, st)
        None ->
          from_array_like_loop(
            st,
            target,
            target_h,
            source,
            0,
            len,
            mapping,
            this_arg,
          )
      }
    }
  }
}

fn from_store_loop(
  st: Agent,
  target: JsVal,
  target_h: Handle,
  values: List(JsVal),
  k: Int,
  mapping: Option(JsVal),
  this_arg: JsVal,
) -> #(JsVal, Agent) {
  case values {
    [] -> #(target, st)
    [v, ..rest] -> {
      let st = map_and_store(st, target, target_h, v, k, mapping, this_arg)
      from_store_loop(st, target, target_h, rest, k + 1, mapping, this_arg)
    }
  }
}

fn from_array_like_loop(
  st: Agent,
  target: JsVal,
  target_h: Handle,
  source: JsVal,
  k: Int,
  len: Int,
  mapping: Option(JsVal),
  this_arg: JsVal,
) -> #(JsVal, Agent) {
  case k >= len {
    True -> #(target, st)
    False -> {
      let #(v, st) = rt_obj.t_get_prop(st, source, StringKey(Index(k)))
      let st = map_and_store(st, target, target_h, v, k, mapping, this_arg)
      from_array_like_loop(
        st,
        target,
        target_h,
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
  st: Agent,
  target: JsVal,
  target_h: Handle,
  v: JsVal,
  k: Int,
  mapping: Option(JsVal),
  this_arg: JsVal,
) -> Agent {
  let #(mapped, st) = case mapping {
    Some(f) -> call(st, f, this_arg, [v, mk_number(JInt(k))])
    None -> #(v, st)
  }
  set_index(st, target_h, target, k, mapped)
}

/// §23.2.2.2 %TypedArray%.of ( ...items )
fn ta_of(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use <- bool.lazy_guard(!rt_call.is_constructor(st, this), fn() {
    rt_val.t_throw_type_error(st, "%TypedArray%.of called on non-constructor")
  })
  let #(target, target_h, st) = ta_create(st, this, list.length(args))
  case try_bulk_store(st, target_h, 0, args) {
    Some(st) -> #(target, st)
    None -> from_store_loop(st, target, target_h, args, 0, None, mk_undefined())
  }
}

// ============================================================================
// Constructor — §23.2.5.1 TypedArray ( ...args )
// ============================================================================

/// AllocateTypedArray's GetPrototypeFromConstructor(NewTarget) is a real
/// [[Get]] that may run user code, so it sits exactly where §23.2.5.1 puts
/// AllocateTypedArray in each branch: first for the no-argument and Object
/// branches, but AFTER `? ToIndex(firstArgument)` for a primitive length.
fn ta_construct(
  st: Agent,
  kind: TypedArrayKind,
  new_target: JsVal,
  default_proto: Handle,
  args: List(JsVal),
) -> #(Handle, Agent) {
  case args {
    // Step 4: no args → AllocateTypedArray(0).
    [] -> {
      let #(proto, st) = proto_from_new_target(st, new_target, default_proto)
      fresh_handle(alloc_ta_with_length(st, kind, proto, 0))
    }
    [first, ..rest] ->
      case classify(first) {
        // Step 6.a: firstArgument is an Object → AllocateTypedArray first.
        KHandle(ref) -> {
          let #(proto, st) =
            proto_from_new_target(st, new_target, default_proto)
          case rt_store.t_cell_get(st, ref) {
            SObject(kind: types.ArrayBufferObj(..), ..) ->
              from_buffer(st, kind, proto, ref, rest)
            SObject(
              kind: TypedArrayObj(
                buffer: src_buf,
                elem_kind: src_kind,
                byte_offset: src_off,
                length: src_len,
              ),
              ..,
            ) ->
              from_typed_array(
                st,
                kind,
                proto,
                src_buf,
                src_kind,
                src_off,
                buffer.typed_array_view_length(
                  st,
                  src_buf,
                  src_kind,
                  src_off,
                  src_len,
                ),
              )
            _ -> from_object(st, kind, proto, first, ref)
          }
        }
        // Step 6.b-c: not an object → AllocateTypedArray(? ToIndex(arg)).
        _ -> {
          let #(len, st) = to_index(st, first)
          let #(proto, st) =
            proto_from_new_target(st, new_target, default_proto)
          fresh_handle(alloc_ta_with_length(st, kind, proto, len))
        }
      }
  }
}

/// A TypedArray this module just allocated: the JS value handed back, plus
/// the two handles the allocator created — the view object and its backing
/// buffer. Callers that need either take it from here rather than
/// re-deriving it out of the store (which forces an "impossible" miss branch).
type FreshTa {
  FreshTa(value: JsVal, ta_ref: Handle, buffer: Handle)
}

/// Adapt an allocation to the "just the handle" shape the constructor paths
/// hand back.
fn fresh_handle(r: #(FreshTa, Agent)) -> #(Handle, Agent) {
  let #(fresh, st) = r
  #(fresh.ta_ref, st)
}

/// AllocateTypedArray + AllocateTypedArrayBuffer (§23.2.5.1.1/.6): fresh
/// zeroed buffer of `len` elements viewed from offset 0.
fn alloc_ta_with_length(
  st: Agent,
  kind: TypedArrayKind,
  proto: Handle,
  len: Int,
) -> #(FreshTa, Agent) {
  let size = typed_array_ffi.elem_size(kind)
  let byte_len = len * size
  use <- bool.lazy_guard(byte_len > max_byte_length, fn() {
    rt_val.t_throw_range_error(st, "Invalid typed array length")
  })
  alloc_fresh_ta(st, kind, proto, byte_len, len)
}

/// Allocate a fresh non-resizable ArrayBuffer of `byte_len` zero bytes plus
/// a fixed `len`-element view over it from offset 0.
fn alloc_fresh_ta(
  st: Agent,
  kind: TypedArrayKind,
  proto: Handle,
  byte_len: Int,
  len: Int,
) -> #(FreshTa, Agent) {
  let #(buf, st) =
    array_buffer.alloc_buffer(st, st.realm.array_buffer.prototype, byte_len)
  let #(ta_ref, st) =
    realm_ops.alloc_wrapper(
      st,
      TypedArrayObj(
        buffer: buf,
        elem_kind: kind,
        byte_offset: 0,
        length: Some(len),
      ),
      proto,
    )
  #(FreshTa(value: mk_object(ta_ref), ta_ref:, buffer: buf), st)
}

/// §23.2.5.1.3 InitializeTypedArrayFromArrayBuffer.
fn from_buffer(
  st: Agent,
  kind: TypedArrayKind,
  proto: Handle,
  buf_ref: Handle,
  rest: List(JsVal),
) -> #(Handle, Agent) {
  let size = typed_array_ffi.elem_size(kind)
  let #(offset_arg, len_arg) = helpers.two_args_or_undefined(rest)
  // Step 2: offset = ToIndex(byteOffset).
  let #(offset, st) = to_index(st, offset_arg)
  // Step 3: offset modulo elementSize must be 0.
  use <- bool.lazy_guard(offset % size != 0, fn() {
    rt_val.t_throw_range_error(
      st,
      "start offset of "
        <> typed_array_name(kind)
        <> " should be a multiple of "
        <> int.to_string(size),
    )
  })
  // Step 5: newLength = ToIndex(length) when present.
  let #(new_len, st) = case classify(len_arg) {
    KUndef -> #(None, st)
    _ -> {
      let #(l, st) = to_index(st, len_arg)
      #(Some(l), st)
    }
  }
  // Step 6: detached check AFTER the (observable) conversions.
  case buffer.buffer_storage(st, buf_ref) |> option.then(types.buffer_bits) {
    None ->
      rt_val.t_throw_type_error(
        st,
        "Cannot perform Construct on a detached ArrayBuffer",
      )
    Some(data) -> {
      let buf_len = bit_array.byte_size(data)
      let range_err = fn(msg) { rt_val.t_throw_range_error(st, msg) }
      let resizable =
        buffer.buffer_storage(st, buf_ref)
        |> option.then(types.buffer_max_byte_length)
        |> option.is_some
      case new_len {
        // Step 7: length absent over a RESIZABLE buffer — length-tracking
        // view ([[ArrayLength]] = AUTO); only the offset is validated, the
        // buffer's byte length need not divide evenly.
        None if resizable ->
          case offset > buf_len {
            True -> range_err("Invalid typed array offset")
            False -> alloc_ta_view(st, kind, proto, buf_ref, offset, None)
          }
        // Step 8.a: length auto-derived — buffer must divide evenly.
        None ->
          case buf_len % size != 0 {
            True ->
              range_err(
                "byte length of "
                <> typed_array_name(kind)
                <> " should be a multiple of "
                <> int.to_string(size),
              )
            False ->
              case buf_len - offset < 0 {
                True -> range_err("Invalid typed array length")
                False ->
                  alloc_ta_view(
                    st,
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
            False -> alloc_ta_view(st, kind, proto, buf_ref, offset, Some(l))
          }
      }
    }
  }
}

/// Allocate a TypedArray view over an EXISTING buffer. `len: None` is a
/// length-tracking view ([[ArrayLength]] = AUTO) over a resizable buffer.
fn alloc_ta_view(
  st: Agent,
  kind: TypedArrayKind,
  proto: Handle,
  buf_ref: Handle,
  byte_offset: Int,
  len: Option(Int),
) -> #(Handle, Agent) {
  realm_ops.alloc_wrapper(
    st,
    TypedArrayObj(buffer: buf_ref, elem_kind: kind, byte_offset:, length: len),
    proto,
  )
}

/// §23.2.5.1.2 InitializeTypedArrayFromTypedArray.
fn from_typed_array(
  st: Agent,
  kind: TypedArrayKind,
  proto: Handle,
  src_buf: Handle,
  src_kind: TypedArrayKind,
  src_off: Int,
  src_len: Int,
) -> #(Handle, Agent) {
  // Step 6.c: BigInt and Number content types never mix.
  use <- bool.lazy_guard(!same_content_type(kind, src_kind), fn() {
    rt_val.t_throw_type_error(
      st,
      "Cannot initialize "
        <> typed_array_name(kind)
        <> " from "
        <> typed_array_name(src_kind),
    )
  })
  case buffer.buffer_bytes(st, src_buf) {
    None ->
      rt_val.t_throw_type_error(
        st,
        "Cannot perform Construct on a detached ArrayBuffer",
      )
    Some(src_data) -> {
      let size = typed_array_ffi.elem_size(kind)
      let byte_len = src_len * size
      use <- bool.lazy_guard(byte_len > max_byte_length, fn() {
        rt_val.t_throw_range_error(st, "Invalid typed array length")
      })
      // §23.2.5.1.2 step 5 (MakeTypedArrayWithBufferWitnessRecord +
      // IsTypedArrayOutOfBounds): a source view whose resizable buffer has
      // shrunk below it behaves like detached → TypeError.
      let src_size = typed_array_ffi.elem_size(src_kind)
      use <- bool.lazy_guard(
        src_off + src_len * src_size > bit_array.byte_size(src_data),
        fn() {
          rt_val.t_throw_type_error(
            st,
            "Cannot perform Construct on an out-of-bounds TypedArray",
          )
        },
      )
      let new_data = case kind == src_kind {
        // Same element type: raw byte copy. The out-of-bounds guard above
        // PROVED src_off + byte_len <= byte_size(src_data), so a failure here
        // is a broken proof, not a shrunk buffer — crash, don't zero-fill.
        True -> {
          let assert Ok(bytes) = bit_array.slice(src_data, src_off, byte_len)
          bytes
        }
        // Different element type: element-wise convert (no user code runs).
        False ->
          convert_elements(st, src_buf, src_kind, src_off, src_len, kind, size)
      }
      let #(fresh, st) = alloc_fresh_ta(st, kind, proto, byte_len, src_len)
      let st = buffer.store_region(st, fresh.buffer, new_data, 0, byte_len)
      #(fresh.ta_ref, st)
    }
  }
}

/// Element-wise conversion into a fresh buffer — single pass: each element
/// is encoded into its own small binary and the segments are concatenated
/// ONCE at the end (O(n)).
fn convert_elements(
  st: Agent,
  src_buf: Handle,
  src_kind: TypedArrayKind,
  src_off: Int,
  src_len: Int,
  dst_kind: TypedArrayKind,
  dst_size: Int,
) -> BitArray {
  convert_elements_loop(
    st,
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
  st: Agent,
  src_buf: Handle,
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
      // Elements read back out of a typed array are always Number / BigInt;
      // a missing element (out-of-bounds read) encodes as zero.
      let seg = case
        buffer.typed_array_element(st, src_buf, src_kind, src_off, src_len, i)
        |> option.then(buffer.decoded_element(dst_kind, _))
      {
        Some(el) -> buffer.typed_array_encode_value(ta_zeroed(dst_size), 0, el)
        None -> ta_zeroed(dst_size)
      }
      convert_elements_loop(
        st,
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
  st: Agent,
  kind: TypedArrayKind,
  proto: Handle,
  obj_val: JsVal,
  obj_ref: Handle,
) -> #(Handle, Agent) {
  let #(iter_fn, st) =
    rt_obj.t_get_prop(st, obj_val, SymbolKey(symbol_iterator))
  case rt_call.is_callable(st, iter_fn) {
    True -> {
      let #(rec, st) =
        iter_protocol.get_iterator_from_method(st, obj_val, iter_fn)
      let #(values, st) = iter_protocol.iterator_to_list(st, rec)
      let #(fresh, st) =
        alloc_ta_with_length(st, kind, proto, list.length(values))
      case try_bulk_store(st, fresh.ta_ref, 0, values) {
        Some(st) -> #(fresh.ta_ref, st)
        None -> #(fresh.ta_ref, store_list(st, fresh, values, 0))
      }
    }
    False -> {
      // Array-like: len = ToLength(Get(obj, "length")).
      let #(len_val, st) =
        rt_obj.t_get_prop(st, obj_val, StringKey(Named("length")))
      let #(len, st) = rt_val.t_to_length(st, len_val)
      let #(fresh, st) = alloc_ta_with_length(st, kind, proto, len)
      let bulk =
        buffer.plain_indexed_values(st, obj_ref, len)
        |> option.then(try_bulk_store(st, fresh.ta_ref, 0, _))
      case bulk {
        Some(st) -> #(fresh.ta_ref, st)
        None -> #(fresh.ta_ref, store_array_like(st, fresh, obj_val, 0, len))
      }
    }
  }
}

/// Store collected values into a fresh typed array via the exotic [[Set]]
/// path (per-element coercion, may run user code).
fn store_list(
  st: Agent,
  fresh: FreshTa,
  values: List(JsVal),
  idx: Int,
) -> Agent {
  case values {
    [] -> st
    [v, ..rest] -> {
      let st = set_index(st, fresh.ta_ref, fresh.value, idx, v)
      store_list(st, fresh, rest, idx + 1)
    }
  }
}

fn store_array_like(
  st: Agent,
  fresh: FreshTa,
  obj_val: JsVal,
  k: Int,
  len: Int,
) -> Agent {
  case k >= len {
    True -> st
    False -> {
      let #(v, st) = rt_obj.t_get_prop(st, obj_val, StringKey(Index(k)))
      let st = set_index(st, fresh.ta_ref, fresh.value, k, v)
      store_array_like(st, fresh, obj_val, k + 1, len)
    }
  }
}

// ============================================================================
// Conversions
// ============================================================================

/// §7.1.22 ToIndex with this module's RangeError message.
fn to_index(st: Agent, val: JsVal) -> #(Int, Agent) {
  rt_val.t_to_index(st, val, "Invalid typed array length")
}

/// ToIntegerOrInfinity, with infinities preserved as sentinels.
type IntOrInf {
  IInt(Int)
  IPosInf
  INegInf
}

fn to_int_or_inf(st: Agent, val: JsVal) -> #(IntOrInf, Agent) {
  let #(n, st) = rt_val.t_to_number(st, val)
  let i = case n {
    JNan -> IInt(0)
    JInt(i) -> IInt(i)
    JFloat(f) -> IInt(rt_val.float_to_int(f))
    JPosInf -> IPosInf
    JNegInf -> INegInf
  }
  #(i, st)
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

/// A TypedArray's internal slots AS DECLARED on the object, plus the `ref`
/// they were read from — carrying it means downstream code never has to
/// re-match the receiver value to recover the object it just resolved:
/// [[ViewedArrayBuffer]], the element kind ([[TypedArrayName]]),
/// [[ByteOffset]], and the declared [[ArrayLength]] — `None` for
/// length-tracking (AUTO) views, which follow the live buffer size.
type TaView {
  TaView(
    ref: Handle,
    buffer: Handle,
    kind: TypedArrayKind,
    byte_offset: Int,
    length: Option(Int),
  )
}

/// The view a require_ta / validate_ta caller receives: same slots, but
/// with [[ArrayLength]] RESOLVED to the current element count (AUTO views
/// included), so downstream bounds checks see a plain Int.
///
/// The record runs at TWO speeds, and callers must not confuse them:
/// `length` (like the other slot copies) is a SNAPSHOT taken at validation
/// time, whereas `ref` is a LIVE handle — re-reading the object through it
/// (`ta_read`/`ta_get`) observes user code that detached or resized the
/// buffer mid-method. That is exactly what the spec's per-element Get
/// requires, so the loops below iterate up to the snapshot `length` but read
/// each element through `ref`.
type TaWitness {
  TaWitness(
    ref: Handle,
    buffer: Handle,
    kind: TypedArrayKind,
    byte_offset: Int,
    length: Int,
  )
}

/// `v`'s TypedArray internal slots when `v` is a TypedArray object, None
/// otherwise.
fn ta_slot(st: Agent, v: JsVal) -> Option(TaView) {
  case classify(v) {
    KHandle(ref) -> ta_slot_of(st, ref)
    _ -> None
  }
}

/// `ref`'s TypedArray internal slots when it points at a TypedArray object.
fn ta_slot_of(st: Agent, ref: Handle) -> Option(TaView) {
  case rt_store.t_cell_get(st, ref) {
    SObject(kind: TypedArrayObj(buffer:, elem_kind:, byte_offset:, length:), ..) ->
      Some(TaView(ref:, buffer:, kind: elem_kind, byte_offset:, length:))
    _ -> None
  }
}

/// Bulk element store: when every value converts to the element type without
/// running user code (no objects, no throwing conversions), encode the whole
/// run in one pass and rebuild the buffer binary ONCE — instead of one
/// whole-buffer rebuild + store write per element, which is O(n²) in the
/// buffer size. Returns None when any value needs the observable per-element
/// path. Mirrors do_typed_store's live-buffer rules: detached buffer or an
/// out-of-bounds view → the stores are silent no-ops; a partial fit writes
/// only the in-bounds prefix.
fn try_bulk_store(
  st: Agent,
  ta_ref: Handle,
  start: Int,
  values: List(JsVal),
) -> Option(Agent) {
  use view <- option.then(ta_slot_of(st, ta_ref))
  let TaView(buffer: buf, kind:, byte_offset:, length:, ..) = view
  use region <- option.then(buffer.typed_array_encode_primitives(kind, values))
  case buffer.buffer_bytes(st, buf) {
    // Detached → every per-element store is a silent no-op.
    None -> Some(st)
    Some(data) -> {
      let size = typed_array_ffi.elem_size(kind)
      let byte_size = bit_array.byte_size(data)
      let len = case length {
        Some(n) -> n
        None -> int.max(0, { byte_size - byte_offset } / size)
      }
      // Fixed view that no longer fits the (shrunk) buffer → all no-ops.
      use <- bool.guard(byte_offset + len * size > byte_size, Some(st))
      let count = int.clamp(len - start, 0, bit_array.byte_size(region) / size)
      use <- bool.guard(count <= 0, Some(st))
      // Whole ELEMENTS only; splice_clamped's byte clamp is just the backstop.
      // `count` was clamped by byte_size(region) / size, so the slice fits.
      let region = case count * size == bit_array.byte_size(region) {
        True -> region
        False -> {
          let assert Ok(region) = bit_array.slice(region, 0, count * size)
          region
        }
      }
      let off = byte_offset + start * size
      let #(new_data, written) = splice_clamped(data, off, region)
      Some(buffer.store_region(st, buf, new_data, off, written))
    }
  }
}

/// RequireInternalSlot(this, [[TypedArrayName]]): the resolved view (a
/// TaWitness).
fn require_ta(st: Agent, this: JsVal) -> TaWitness {
  case ta_slot(st, this) {
    Some(TaView(ref:, buffer: buf, kind:, byte_offset:, length:)) ->
      // Resolve [[ArrayLength]] = AUTO (length-tracking views) to the
      // CURRENT element count — downstream code sees a plain Int and
      // its existing bounds checks behave identically for fixed views.
      TaWitness(
        ref:,
        buffer: buf,
        kind:,
        byte_offset:,
        length: buffer.typed_array_view_length(
          st,
          buf,
          kind,
          byte_offset,
          length,
        ),
      )
    None ->
      rt_val.t_throw_type_error(
        st,
        "Method %TypedArray%.prototype called on incompatible receiver",
      )
  }
}

/// §23.2.4.4 ValidateTypedArray ( O, seq-cst ): RequireInternalSlot plus the
/// buffer-witness checks — detached buffer or an out-of-bounds view (a
/// resizable ArrayBuffer that shrank below the view) throws TypeError.
fn validate_ta(st: Agent, this: JsVal) -> TaWitness {
  let view = require_ta(st, this)
  let TaWitness(buffer: buf, kind:, byte_offset: off, length: len, ..) = view
  case buffer.buffer_bytes(st, buf) {
    None -> witness_type_error(st, buffer.BufferDetached)
    Some(data) -> {
      let size = typed_array_ffi.elem_size(kind)
      case off + len * size > bit_array.byte_size(data) {
        True -> witness_type_error(st, buffer.OutOfBoundsView)
        False -> view
      }
    }
  }
}

/// Immutable ArrayBuffer proposal — ValidateTypedArray step 4: accessMode
/// ~write~ on a view over an immutable buffer is a TypeError, raised BEFORE
/// any argument coercion (observable; test262 checks it). The predicate itself
/// is `buffer.buffer_is_immutable` — the same one the [[Set]] element path
/// consults, so a builtin and an element store can never disagree about
/// whether a buffer accepts writes.
fn require_mutable(st: Agent, buf: Handle) -> Nil {
  case buffer.buffer_is_immutable(st, buf) {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Cannot modify a TypedArray backed by an immutable ArrayBuffer",
      )
    False -> Nil
  }
}

// ============================================================================
// Accessors — §23.2.3.1-3, .18, .38
// ============================================================================

fn get_buffer(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = require_ta(st, this)
  #(mk_object(view.buffer), st)
}

/// True when the view is fully backed by the LIVE buffer — detached buffers
/// and views past the end of a shrunk resizable buffer are "out of bounds",
/// and the byteLength/byteOffset/length accessors all answer 0 for them.
/// Delegates to the ONE bounds check the engine has (§10.4.5.14, owned by
/// arc/rt/buffer).
fn witness_in_bounds(st: Agent, witness: TaWitness) -> Bool {
  let TaWitness(buffer: buf, kind:, byte_offset:, length:, ..) = witness
  let view =
    buffer.ViewSlot(
      buffer: buf,
      elem_kind: kind,
      byte_offset:,
      length: Some(length),
    )
  buffer.live_view(st, view)
  |> option.map(buffer.view_in_bounds)
  |> option.unwrap(False)
}

fn get_byte_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = require_ta(st, this)
  let n = case witness_in_bounds(st, view) {
    True -> view.length * typed_array_ffi.elem_size(view.kind)
    False -> 0
  }
  #(mk_number(JInt(n)), st)
}

fn get_byte_offset(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = require_ta(st, this)
  let n = case witness_in_bounds(st, view) {
    True -> view.byte_offset
    False -> 0
  }
  #(mk_number(JInt(n)), st)
}

fn get_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = require_ta(st, this)
  let n = case witness_in_bounds(st, view) {
    True -> view.length
    False -> 0
  }
  #(mk_number(JInt(n)), st)
}

/// §23.2.3.38: NOT a TypeError on foreign receivers — returns undefined.
fn get_to_string_tag(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case ta_slot(st, this) {
    Some(view) -> #(mk_string(typed_array_name(view.kind)), st)
    None -> #(mk_undefined(), st)
  }
}

// ============================================================================
// Prototype methods (initial subset)
// ============================================================================

/// §23.2.3.1 %TypedArray%.prototype.at ( index )
fn proto_at(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let TaWitness(buffer: buf, kind:, byte_offset: off, length: len, ..) = view
  let #(rel, st) = to_int_or_inf(st, helpers.first_arg_or_undefined(args))
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
    buffer.typed_array_element(st, buf, kind, off, len, k)
    |> option.unwrap(mk_undefined())
  #(v, st)
}

/// §23.2.3.8 %TypedArray%.prototype.fill ( value [ , start [ , end ] ] )
fn proto_fill(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let TaWitness(buffer: buf, kind:, byte_offset: off, length: len, ..) = view
  require_mutable(st, buf)
  let value_arg = helpers.first_arg_or_undefined(args)
  let start_arg = helpers.arg_at(args, 1)
  let end_arg = helpers.arg_at(args, 2)
  // Step 3-4: convert the fill value per content type.
  let #(converted, st) = convert_for_kind(st, kind, value_arg)
  let #(s, st) = to_int_or_inf(st, start_arg)
  let #(e, st) = case classify(end_arg) {
    KUndef -> #(IPosInf, st)
    _ -> to_int_or_inf(st, end_arg)
  }
  let start = relative_index(s, len)
  let end = relative_index(e, len)
  // Steps 11-12: re-validate AFTER the observable value/start/end coercions
  // — a detached buffer or an out-of-bounds view throws TypeError. (A
  // shrunk length-tracking view stays in bounds; the clamp below handles
  // it.)
  let data = witness_bytes(st, this)
  let size = typed_array_ffi.elem_size(kind)
  // Clamp to the LIVE buffer (a resizable buffer may have shrunk below
  // the view) — out-of-bounds element writes are silent no-ops.
  let avail = int.max(0, { bit_array.byte_size(data) - off } / size)
  let start = int.min(start, avail)
  let end = int.min(end, avail)
  // Single-pass fill: encode the element ONCE, then build the region
  // with binary:copy + one splice (O(n), not O(n²)).
  let elem = buffer.typed_array_encode_value(ta_zeroed(size), 0, converted)
  let region_off = off + start * size
  let #(new_data, written) = fill_clamped(data, region_off, end - start, elem)
  #(this, buffer.store_region(st, buf, new_data, region_off, written))
}

/// Convert a JS value to the typed array's element domain ONCE — §7.1.13
/// ToBigInt for the BigInt kinds, §7.1.4 ToNumber otherwise. Result is the
/// TypedElement ready for typed_array_encode_value.
fn convert_for_kind(
  st: Agent,
  kind: TypedArrayKind,
  val: JsVal,
) -> #(buffer.TypedElement, Agent) {
  case kind {
    NumKind(k) -> {
      let #(n, st) = rt_val.t_to_number(st, val)
      #(buffer.NumberElement(k, n), st)
    }
    BigKind(k) -> {
      let #(n, st) = rt_val.t_to_bigint(st, val)
      #(buffer.BigIntElement(k, n), st)
    }
  }
}

/// §23.2.3.26 %TypedArray%.prototype.set ( source [ , offset ] )
fn proto_set(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = require_ta(st, this)
  // Immutable ArrayBuffer proposal: set() has accessMode ~write~ — checked
  // before the offset/source coercions run any user code.
  require_mutable(st, view.buffer)
  let src = helpers.first_arg_or_undefined(args)
  let #(off_i, st) = to_int_or_inf(st, helpers.arg_at(args, 1))
  let offset = case off_i {
    IInt(n) -> n
    IPosInf -> max_safe_integer
    INegInf -> -1
  }
  use <- bool.lazy_guard(offset < 0, fn() {
    rt_val.t_throw_range_error(st, "offset is out of bounds")
  })
  // SetTypedArrayFrom* step 2: target detached or out of bounds → TypeError
  // (checked AFTER the observable offset coercion, which can detach/resize
  // it). targetLength is the LIVE length, re-read for the same reason.
  let dst_data = witness_bytes(st, this)
  let len = ta_live_length(st, this)
  case classify(src) {
    KHandle(src_ref) ->
      case ta_slot_of(st, src_ref) {
        Some(src_view) -> {
          let TaView(
            buffer: src_buf,
            kind: src_kind,
            byte_offset: src_off,
            length: src_len,
            ..,
          ) = src_view
          // §23.2.3.26.1 step 4: SOURCE detached or out of bounds →
          // TypeError; srcLength is its live length.
          case view_witness_bytes(st, src_view) {
            Error(err) -> witness_type_error(st, err)
            Ok(_src_bytes) ->
              set_from_typed_array(
                st,
                view,
                dst_data,
                offset,
                len,
                src_buf,
                src_kind,
                src_off,
                buffer.typed_array_live_count(
                  st,
                  src_buf,
                  src_kind,
                  src_off,
                  src_len,
                ),
              )
          }
        }
        None -> set_from_array_like(st, view, offset, len, src)
      }
    // §23.2.3.26.2 step 5: ToObject(source) — primitives get wrapped (a
    // string source copies its characters; numbers/booleans/symbols have
    // no "length" so nothing is copied); undefined/null throw TypeError.
    KUndef | KNull ->
      rt_val.t_throw_type_error(
        st,
        "Cannot convert " <> rt_val.nullish_label(src) <> " to object",
      )
    _ -> {
      let #(src_h, st) = rt_val.t_to_object(st, src)
      set_from_array_like(st, view, offset, len, mk_object(src_h))
    }
  }
}

/// ValidateTypedArray keyed by the TypedArray value: throws TypeError when the
/// view's buffer is detached or the view is out of bounds, otherwise hands
/// back the live backing bytes it just proved exist. Callers therefore never
/// re-read the buffer, and the "detached" case is structurally
/// unrepresentable downstream.
fn witness_bytes(st: Agent, this: JsVal) -> BitArray {
  case ta_witness_bytes(st, this) {
    Error(err) -> witness_type_error(st, err)
    Ok(data) -> data
  }
}

fn set_from_typed_array(
  st: Agent,
  view: TaWitness,
  data: BitArray,
  offset: Int,
  len: Int,
  src_buf: Handle,
  src_kind: TypedArrayKind,
  src_off: Int,
  src_len: Int,
) -> #(JsVal, Agent) {
  let TaWitness(buffer: dst_buf, kind:, byte_offset: dst_off, ..) = view
  // §23.2.3.26.1 step 9: source buffer detached/out of bounds → TypeError.
  let src_live = option.is_some(buffer.buffer_bytes(st, src_buf))
  use <- bool.lazy_guard(!src_live, fn() {
    rt_val.t_throw_type_error(
      st,
      "Cannot perform set from a detached ArrayBuffer",
    )
  })
  use <- bool.lazy_guard(!same_content_type(kind, src_kind), fn() {
    rt_val.t_throw_type_error(st, "Cannot mix BigInt and other types")
  })
  use <- bool.lazy_guard(src_len + offset > len, fn() {
    rt_val.t_throw_range_error(st, "offset is out of bounds")
  })
  let size = typed_array_ffi.elem_size(kind)
  // Build the encoded source region in ONE pass (raw byte copy when kinds
  // match, element-wise convert+concat otherwise — no user code runs)...
  let region = case kind == src_kind {
    True -> copy_region(st, src_buf, src_off, src_len * size)
    False ->
      convert_elements(st, src_buf, src_kind, src_off, src_len, kind, size)
  }
  // ...then splice it into the destination buffer with ONE rebuild,
  // clamped to the live buffer (out-of-bounds writes are silent no-ops,
  // matching per-element store semantics). `data` is the caller's witness:
  // nothing between the guard and here can run user code, so it is current.
  let start = dst_off + offset * size
  let avail =
    { int.clamp(bit_array.byte_size(data) - start, 0, src_len * size) / size }
    * size
  // `region` is exactly src_len * size bytes and avail <= that.
  let region = case avail == src_len * size {
    True -> region
    False -> {
      let assert Ok(region) = bit_array.slice(region, 0, avail)
      region
    }
  }
  let #(new_data, written) = splice_clamped(data, start, region)
  case written > 0 {
    True -> #(
      mk_undefined(),
      buffer.store_region(st, dst_buf, new_data, start, written),
    )
    False -> #(mk_undefined(), st)
  }
}

fn set_from_array_like(
  st: Agent,
  view: TaWitness,
  offset: Int,
  len: Int,
  src: JsVal,
) -> #(JsVal, Agent) {
  let #(len_val, st) = rt_obj.t_get_prop(st, src, StringKey(Named("length")))
  let #(src_len, st) = rt_val.t_to_length(st, len_val)
  use <- bool.lazy_guard(src_len + offset > len, fn() {
    rt_val.t_throw_range_error(st, "offset is out of bounds")
  })
  let bulk = case classify(src) {
    KHandle(src_ref) ->
      buffer.plain_indexed_values(st, src_ref, src_len)
      |> option.then(try_bulk_store(st, view.ref, offset, _))
    _ -> None
  }
  case bulk {
    Some(st) -> #(mk_undefined(), st)
    None -> #(
      mk_undefined(),
      set_array_like_loop(st, view, offset, src, 0, src_len),
    )
  }
}

fn set_array_like_loop(
  st: Agent,
  view: TaWitness,
  offset: Int,
  src: JsVal,
  k: Int,
  src_len: Int,
) -> Agent {
  case k >= src_len {
    True -> st
    False -> {
      let #(v, st) = rt_obj.t_get_prop(st, src, StringKey(Index(k)))
      let st = set_index(st, view.ref, mk_object(view.ref), offset + k, v)
      set_array_like_loop(st, view, offset, src, k + 1, src_len)
    }
  }
}

/// §23.2.3.30 %TypedArray%.prototype.subarray ( begin, end ) — a view over
/// the SAME buffer; works on detached arrays too.
fn proto_subarray(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  // §23.2.3.30 step 2: RequireInternalSlot only — NOT ValidateTypedArray.
  // A detached buffer / out-of-bounds view does not throw here; it just
  // gives srcLength = 0 (the constructor call below may still throw).
  case ta_slot(st, this) {
    Some(view) -> do_subarray(st, this, args, view)
    None ->
      rt_val.t_throw_type_error(
        st,
        "Method %TypedArray%.prototype called on incompatible receiver",
      )
  }
}

fn do_subarray(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  view: TaView,
) -> #(JsVal, Agent) {
  let TaView(buffer: buf, kind:, byte_offset: off, length: declared, ..) = view
  let b_arg = helpers.first_arg_or_undefined(args)
  let e_arg = helpers.arg_at(args, 1)
  // Steps 5-7: srcLength = 0 for an out-of-bounds view, else the CURRENT
  // length — snapshotted BEFORE the (observable) start/end coercions.
  let src_length = buffer.typed_array_live_count(st, buf, kind, off, declared)
  let #(b, st) = case classify(b_arg) {
    KUndef -> #(IInt(0), st)
    _ -> to_int_or_inf(st, b_arg)
  }
  let begin = relative_index(b, src_length)
  let size = typed_array_ffi.elem_size(kind)
  let new_off = off + begin * size
  // Step 15: a length-tracking source with `end` undefined produces a
  // length-tracking result — « buffer, beginByteOffset », NO length arg.
  let #(ctor_args, st) = case declared, classify(e_arg) {
    None, KUndef -> #([mk_object(buf), mk_number(JInt(new_off))], st)
    _, _ -> {
      let #(e, st) = case classify(e_arg) {
        KUndef -> #(IInt(src_length), st)
        _ -> to_int_or_inf(st, e_arg)
      }
      let end = relative_index(e, src_length)
      let new_len = int.max(end - begin, 0)
      #(
        [mk_object(buf), mk_number(JInt(new_off)), mk_number(JInt(new_len))],
        st,
      )
    }
  }
  // Step 17: TypedArraySpeciesCreate(O, argumentsList) — same buffer, never
  // a copy. The default path runs the real constructor steps (from_buffer)
  // so detached buffers throw TypeError and out-of-range views RangeError.
  let #(maybe_ctor, st) = resolve_species_ctor(st, this, kind)
  case maybe_ctor {
    None -> {
      let proto = default_proto_for(st, kind)
      let #(h, st) = from_buffer(st, kind, proto, buf, list.drop(ctor_args, 1))
      #(mk_object(h), st)
    }
    Some(ctor) -> {
      let #(obj, obj_ref, st) = ta_create_with_args(st, ctor, ctor_args, None)
      let #(obj, _) = check_content_type(st, obj, obj_ref, kind)
      #(obj, st)
    }
  }
}

/// §23.2.3.27 %TypedArray%.prototype.slice ( start, end ) — copies into a
/// FRESH buffer (default species).
fn proto_slice(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let TaWitness(buffer: buf, kind:, byte_offset: off, length: len, ..) = view
  let s_arg = helpers.first_arg_or_undefined(args)
  let e_arg = helpers.arg_at(args, 1)
  let #(s, st) = case classify(s_arg) {
    KUndef -> #(IInt(0), st)
    _ -> to_int_or_inf(st, s_arg)
  }
  let #(e, st) = case classify(e_arg) {
    KUndef -> #(IPosInf, st)
    _ -> to_int_or_inf(st, e_arg)
  }
  let start = relative_index(s, len)
  let end = relative_index(e, len)
  let count = int.max(end - start, 0)
  // Step 9: A = TypedArraySpeciesCreate(O, « count »).
  let #(#(target, target_ref), st) = ta_species_create(st, this, kind, count)
  use <- bool.guard(count == 0, #(target, st))
  // Step 10.a-b: re-validate the SOURCE — the species constructor (or
  // start/end valueOf) may have detached its buffer or shrunk it below a
  // fixed view; both throw TypeError. (A shrunk length-tracking view stays
  // in bounds — the copy below just reads fewer live bytes.)
  let _source_bytes = witness_bytes(st, this)
  case ta_slot(st, target) {
    Some(TaView(
      buffer: target_buf,
      kind: target_kind,
      byte_offset: target_off,
      ..,
    )) ->
      case target_kind == kind {
        // Same element kind → single byte-region copy spliced into the
        // target's buffer at its view offset.
        True -> {
          let size = typed_array_ffi.elem_size(kind)
          // Step 14.c: endIndex = min(final, live TypedArrayLength) —
          // only WHOLE live source elements are copied (a shrink
          // "between elements" must not copy a partial element); the
          // target keeps its own contents beyond them.
          let copy_elems = int.clamp(ta_live_length(st, this) - start, 0, count)
          let src_byte = off + start * size
          case buffer.buffer_bytes(st, target_buf) {
            None -> #(target, st)
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
              let region = case target_buf == buf {
                True -> seq_copy_region(tdata, src_byte, target_off, avail)
                False -> copy_region(st, buf, src_byte, avail)
              }
              let #(new_data, written) =
                splice_clamped(tdata, target_off, region)
              #(
                target,
                buffer.store_region(
                  st,
                  target_buf,
                  new_data,
                  target_off,
                  written,
                ),
              )
            }
          }
        }
        // Different (number) kind from a species constructor →
        // element-wise convert via the ordinary Set path.
        False -> {
          let elements =
            join_collect(st, view, start, [])
            |> list.reverse
            |> list.take(count)
          #(target, write_values(st, target, target_ref, elements, 0))
        }
      }
    None -> #(target, st)
  }
}

/// The result of copying `n` bytes from `src` to `dst` within ONE binary,
/// byte-by-byte in ascending order (§23.2.3.27 step 14.f): when the target
/// range starts inside the source range, each written byte is re-read by a
/// later step, so the leading `dst - src` bytes repeat across the result.
fn seq_copy_region(data: BitArray, src: Int, dst: Int, n: Int) -> BitArray {
  case dst > src && dst < src + n {
    // `n` is clamped to the TARGET's remaining bytes, not the source's, so
    // the source range may run past the live buffer: clamp.
    False -> slice_clamped(data, src, n)
    True -> repeat_to(slice_clamped(data, src, dst - src), n, <<>>)
  }
}

fn repeat_to(pattern: BitArray, n: Int, acc: BitArray) -> BitArray {
  use <- bool.guard(bit_array.byte_size(pattern) == 0, acc)
  case bit_array.byte_size(acc) >= n {
    True -> {
      let assert Ok(bytes) = bit_array.slice(acc, 0, n)
      bytes
    }
    False -> repeat_to(pattern, n, bit_array.concat([acc, pattern]))
  }
}

/// The single answer to "bytes past the live buffer": the `len` bytes at
/// `off`, clamped to whatever of `[off, off + len)` actually lies inside
/// `data`. Total by construction — the slice below cannot fail. Where a
/// preceding witness check PROVES the range in bounds, the call sites use
/// `let assert Ok(..) = bit_array.slice(..)` instead, so a broken proof
/// crashes rather than quietly returning the wrong bytes.
fn slice_clamped(data: BitArray, off: Int, len: Int) -> BitArray {
  let size = bit_array.byte_size(data)
  let start = int.clamp(off, 0, size)
  let assert Ok(bytes) =
    bit_array.slice(data, start, int.clamp(len, 0, size - start))
  bytes
}

/// Copy `byte_len` bytes starting at `byte_off` of a view's backing buffer
/// into a fresh binary — ONE slice instead of per-element re-encoding.
/// Bytes beyond the live buffer (detached, or a resizable buffer that has
/// shrunk below the view) read as zero.
fn copy_region(
  st: Agent,
  buf: Handle,
  byte_off: Int,
  byte_len: Int,
) -> BitArray {
  case buffer.buffer_bytes(st, buf) {
    None -> ta_zeroed(byte_len)
    Some(data) -> {
      let bytes = slice_clamped(data, byte_off, byte_len)
      let got = bit_array.byte_size(bytes)
      case got == byte_len {
        True -> bytes
        False -> bit_array.concat([bytes, ta_zeroed(byte_len - got)])
      }
    }
  }
}

/// §23.2.3.16 %TypedArray%.prototype.join ( separator )
fn proto_join(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let sep_arg = helpers.first_arg_or_undefined(args)
  let #(sep, st) = case classify(sep_arg) {
    KUndef -> #(",", st)
    _ -> rt_val.t_to_string(st, sep_arg)
  }
  let parts = join_parts(st, view, 0, []) |> list.reverse
  #(mk_string(string.join(parts, sep)), st)
}

fn join_parts(
  st: Agent,
  view: TaWitness,
  i: Int,
  acc: List(String),
) -> List(String) {
  case i >= view.length {
    True -> acc
    False -> {
      // Live read: ToString(separator) above may have shrunk the buffer;
      // indices past the CURRENT length read as undefined → "".
      let s = case option.map(ta_read(st, view.ref, i), classify) {
        Some(KNum(n)) -> rt_val.jsnum_to_string(n)
        Some(KBig(b)) -> int.to_string(b)
        _ -> ""
      }
      join_parts(st, view, i + 1, [s, ..acc])
    }
  }
}

/// §23.2.3.16/.13 indexOf / includes. See search_loop for how the two
/// differ on indices past the current (shrunk/detached) length.
fn proto_index_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  proto_search(st, this, args, rt_val.strict_equal, False, fn(i) {
    mk_number(JInt(i))
  })
}

fn proto_includes(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use found <- proto_search(st, this, args, rt_val.same_value_zero, True)
  mk_bool(found >= 0)
}

fn proto_search(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  eq: fn(JsVal, JsVal) -> Bool,
  missing_undefined: Bool,
  done: fn(Int) -> JsVal,
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let len = view.length
  use <- bool.guard(len == 0, #(done(-1), st))
  let search = helpers.first_arg_or_undefined(args)
  let #(n, st) = to_int_or_inf(st, helpers.arg_at(args, 1))
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
  let found = search_loop(st, view, k, search, eq, missing_undefined)
  #(done(found), st)
}

/// Scan [i, len) for `search` with live element reads. `missing_undefined`:
/// True (includes) treats an invalid index as the undefined value Get
/// returns; False (indexOf) skips it, per the HasProperty step.
fn search_loop(
  st: Agent,
  view: TaWitness,
  i: Int,
  search: JsVal,
  eq: fn(JsVal, JsVal) -> Bool,
  missing_undefined: Bool,
) -> Int {
  case i >= view.length {
    True -> -1
    False -> {
      let matched = case ta_read(st, view.ref, i) {
        Some(el) -> eq(el, search)
        None -> missing_undefined && eq(mk_undefined(), search)
      }
      case matched {
        True -> i
        False -> search_loop(st, view, i + 1, search, eq, missing_undefined)
      }
    }
  }
}

/// keys()/values()/entries() — §23.2.3.{19,35,7}: ValidateTypedArray, then
/// CreateArrayIterator over the typed array itself. LAZY: each .next()
/// re-validates the buffer witness and re-reads length/elements through the
/// live backing store, so mutation (and resizable-buffer resizes) during
/// iteration are observed per spec.
fn proto_iter(
  st: Agent,
  this: JsVal,
  iter_kind: ArrayIterKind,
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(iter_ref, st) =
    realm_ops.alloc_wrapper(
      st,
      ArrayIterator(target: view.ref, index: 0, kind: iter_kind),
      st.realm.array_iter_proto,
    )
  #(mk_object(iter_ref), st)
}

/// Collect elements [i, len) with live reads, REVERSED, stopping at the
/// first invalid index — mirrors the spec's "endIndex = min(final,
/// TypedArrayLength)" re-derivation in slice after user code (species
/// constructor / valueOf) may have shrunk the buffer. For sort/toSorted no
/// user code runs between validation and collection, so the stop never
/// triggers there.
fn join_collect(
  st: Agent,
  view: TaWitness,
  i: Int,
  acc: List(JsVal),
) -> List(JsVal) {
  case i >= view.length {
    True -> acc
    False ->
      case ta_read(st, view.ref, i) {
        Some(v) -> join_collect(st, view, i + 1, [v, ..acc])
        None -> acc
      }
  }
}

// ============================================================================
// Callback-iterating prototype methods — §23.2.3
// ============================================================================

/// Step "If IsCallable(callbackfn) is false, throw TypeError" shared by all
/// callback-taking methods; hands back (callback, thisArg).
fn require_cb(st: Agent, args: List(JsVal)) -> #(JsVal, JsVal) {
  let cb = helpers.first_arg_or_undefined(args)
  let this_arg = helpers.arg_at(args, 1)
  case rt_call.is_callable(st, cb) {
    True -> #(cb, this_arg)
    False ->
      rt_val.t_throw_type_error(st, describe(st, cb) <> " is not a function")
  }
}

/// `typeof`-name a value for TypeError messages.
fn describe(st: Agent, v: JsVal) -> String {
  let #(ty, _) = rt_val.t_type_of(st, v)
  ty
}

/// The `JsOps.call` upcall (`t_call_checked`): re-raises the callee's throw.
fn call(
  st: Agent,
  f: JsVal,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let js = st.store
  js.ops.call(st, f, this, args)
}

/// Set(target, k, v) through the exotic [[Set]] path (per-element coercion,
/// may run user code). The Bool result is dropped: an integer-indexed [[Set]]
/// on the target itself never reports false except for an immutable buffer,
/// which every caller has already rejected.
fn set_index(
  st: Agent,
  target_h: Handle,
  target: JsVal,
  k: Int,
  v: JsVal,
) -> Agent {
  let #(_, st) =
    rt_obj.t_set_prop_with_receiver(
      st,
      target_h,
      StringKey(Index(k)),
      v,
      target,
    )
  st
}

/// §10.4.5.15 IntegerIndexedElementGet against the LIVE view: re-reads the
/// TypedArray cell so user code that resized the backing buffer mid-method
/// (callback / valueOf side effects) is observed. A length-tracking view
/// follows the CURRENT buffer length; a fixed view that no longer fits is
/// wholly out of bounds. None = invalid index (like a detached buffer).
fn ta_read(st: Agent, ta_ref: Handle, k: Int) -> Option(JsVal) {
  use view <- option.then(ta_slot_of(st, ta_ref))
  let TaView(buffer: buf, kind:, byte_offset:, length:, ..) = view
  buffer.typed_array_element_live(st, buf, kind, byte_offset, length, k)
}

/// Read element `k` as the spec's Get(O, Pk) does: out-of-bounds (shrunk
/// resizable buffer / detached mid-iteration) reads as undefined.
fn ta_get(st: Agent, ta_ref: Handle, k: Int) -> JsVal {
  ta_read(st, ta_ref, k) |> option.unwrap(mk_undefined())
}

/// Throw the TypeError a §23.2.4.4 ValidateTypedArray witness failure demands.
/// The categories and their prose live in `buffer.ViewWitnessError` — the ONE
/// witness error type — and this only adapts it to a builtin's return shape.
fn witness_type_error(st: Agent, err: buffer.ViewWitnessError) -> a {
  rt_val.t_throw_type_error(st, buffer.view_witness_error_message(err))
}

/// §23.2.4.4 ValidateTypedArray buffer-witness checks against the LIVE
/// buffer: Error(..) when the buffer is detached or the view is out of
/// bounds (a fixed view past the end of a shrunk resizable buffer, or a
/// length-tracking view whose byte offset is past the end).
///
/// On success it hands back the LIVE bytes it just read to prove all that:
/// the bytes ARE the proof, so they travel with it.
fn view_witness_bytes(
  st: Agent,
  view: TaView,
) -> Result(BitArray, buffer.ViewWitnessError) {
  let TaView(buffer: buf, kind:, byte_offset: off, length: declared, ..) = view
  case buffer.buffer_bytes(st, buf) {
    None -> Error(buffer.BufferDetached)
    Some(data) -> {
      let byte_size = bit_array.byte_size(data)
      let size = typed_array_ffi.elem_size(kind)
      let oob = case declared {
        Some(n) -> off + n * size > byte_size
        None -> off > byte_size
      }
      case oob {
        True -> Error(buffer.OutOfBoundsView)
        False -> Ok(data)
      }
    }
  }
}

/// view_witness_bytes keyed by the TypedArray value itself (re-reads the
/// cell so resizes since validation are observed).
fn ta_witness_bytes(
  st: Agent,
  this: JsVal,
) -> Result(BitArray, buffer.ViewWitnessError) {
  case ta_slot(st, this) {
    Some(view) -> view_witness_bytes(st, view)
    None -> Error(buffer.NotAView)
  }
}

/// CURRENT number of valid indices of the view — 0 for a detached buffer or
/// an out-of-bounds fixed view; the live tracked length for a
/// length-tracking view. The §10.4.5.14 IsValidIntegerIndex bound.
fn ta_live_length(st: Agent, this: JsVal) -> Int {
  case ta_slot(st, this) {
    Some(TaView(buffer: buf, kind:, byte_offset:, length:, ..)) ->
      buffer.typed_array_live_count(st, buf, kind, byte_offset, length)
    None -> 0
  }
}

/// Which way an index-visiting loop walks — nothing tied "step is negative"
/// to "start at len - 1" when this was a bare Int step.
type Direction {
  Ascending
  Descending
}

fn direction_step(dir: Direction) -> Int {
  case dir {
    Ascending -> 1
    Descending -> -1
  }
}

fn direction_start(dir: Direction, len: Int) -> Int {
  case dir {
    Ascending -> 0
    Descending -> len - 1
  }
}

/// Generic callback loop: visits indices from `k` in `dir` while inside
/// [0, len), calling cb(element, k, this). `decide` inspects the callback
/// result and may stop the loop with a final value.
fn iterate_calls(
  st: Agent,
  view: TaWitness,
  k: Int,
  dir: Direction,
  cb: JsVal,
  this_arg: JsVal,
  decide: fn(JsVal, JsVal, Int) -> Option(JsVal),
) -> #(Option(JsVal), Agent) {
  use <- bool.guard(k < 0 || k >= view.length, #(None, st))
  let el = ta_get(st, view.ref, k)
  let #(res, st) =
    call(st, cb, this_arg, [el, mk_number(JInt(k)), mk_object(view.ref)])
  case decide(res, el, k) {
    Some(v) -> #(Some(v), st)
    None ->
      iterate_calls(
        st,
        view,
        k + direction_step(dir),
        dir,
        cb,
        this_arg,
        decide,
      )
  }
}

/// §23.2.3.7 every / §23.2.3.28 some.
fn proto_every_some(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  is_every: Bool,
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(cb, this_arg) = require_cb(st, args)
  let decide = fn(res, _el, _k) {
    case rt_val.to_boolean(res) == is_every {
      True -> None
      False -> Some(mk_bool(!is_every))
    }
  }
  let #(early, st) = iterate_calls(st, view, 0, Ascending, cb, this_arg, decide)
  #(early |> option.unwrap(mk_bool(is_every)), st)
}

/// §23.2.3.15 forEach.
fn proto_for_each(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(cb, this_arg) = require_cb(st, args)
  let #(_early, st) =
    iterate_calls(st, view, 0, Ascending, cb, this_arg, fn(_res, _el, _k) {
      None
    })
  #(mk_undefined(), st)
}

/// find/findIndex (§23.2.3.13/.14) ascending, findLast/findLastIndex
/// (§23.2.3.11/.12) descending. Result is the element or its index.
type FindMode {
  FindValue
  FindIdx
}

fn proto_find(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  dir: Direction,
  mode: FindMode,
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(cb, this_arg) = require_cb(st, args)
  let start = direction_start(dir, view.length)
  let decide = fn(res, el, k) {
    case rt_val.to_boolean(res) {
      True ->
        Some(case mode {
          FindValue -> el
          FindIdx -> mk_number(JInt(k))
        })
      False -> None
    }
  }
  let #(early, st) = iterate_calls(st, view, start, dir, cb, this_arg, decide)
  let default = case mode {
    FindValue -> mk_undefined()
    FindIdx -> mk_number(JInt(-1))
  }
  #(early |> option.unwrap(default), st)
}

/// §23.2.3.22 map ( callbackfn [ , thisArg ] ) — result via
/// TypedArraySpeciesCreate(O, « len »).
fn proto_map(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(cb, this_arg) = require_cb(st, args)
  let #(#(target, target_ref), st) =
    ta_species_create(st, this, view.kind, view.length)
  #(target, map_loop(st, view, 0, cb, this_arg, target, target_ref))
}

fn map_loop(
  st: Agent,
  view: TaWitness,
  k: Int,
  cb: JsVal,
  this_arg: JsVal,
  target: JsVal,
  target_ref: Handle,
) -> Agent {
  use <- bool.guard(k >= view.length, st)
  let el = ta_get(st, view.ref, k)
  let #(mapped, st) =
    call(st, cb, this_arg, [el, mk_number(JInt(k)), mk_object(view.ref)])
  let st = set_index(st, target_ref, target, k, mapped)
  map_loop(st, view, k + 1, cb, this_arg, target, target_ref)
}

/// §23.2.3.10 filter ( callbackfn [ , thisArg ] ) — kept values collected
/// first, then TypedArraySpeciesCreate(O, « captured ») and written out.
fn proto_filter(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(cb, this_arg) = require_cb(st, args)
  let #(kept_rev, st) = filter_collect(st, view, 0, cb, this_arg, [])
  let kept = list.reverse(kept_rev)
  let #(#(target, target_ref), st) =
    ta_species_create(st, this, view.kind, list.length(kept))
  #(target, write_values(st, target, target_ref, kept, 0))
}

fn filter_collect(
  st: Agent,
  view: TaWitness,
  k: Int,
  cb: JsVal,
  this_arg: JsVal,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  use <- bool.guard(k >= view.length, #(acc, st))
  let el = ta_get(st, view.ref, k)
  let #(res, st) =
    call(st, cb, this_arg, [el, mk_number(JInt(k)), mk_object(view.ref)])
  let acc = case rt_val.to_boolean(res) {
    True -> [el, ..acc]
    False -> acc
  }
  filter_collect(st, view, k + 1, cb, this_arg, acc)
}

/// Write a list of (already numeric) values into a TypedArray via the
/// ordinary Set path, starting at index `k`.
fn write_values(
  st: Agent,
  target: JsVal,
  target_ref: Handle,
  values: List(JsVal),
  k: Int,
) -> Agent {
  case values {
    [] -> st
    [v, ..rest] -> {
      let st = set_index(st, target_ref, target, k, v)
      write_values(st, target, target_ref, rest, k + 1)
    }
  }
}

/// §23.2.3.23/.24 reduce (Ascending) / reduceRight (Descending).
fn proto_reduce(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  dir: Direction,
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let len = view.length
  let cb = helpers.first_arg_or_undefined(args)
  use <- bool.lazy_guard(!rt_call.is_callable(st, cb), fn() {
    rt_val.t_throw_type_error(st, describe(st, cb) <> " is not a function")
  })
  let start = direction_start(dir, len)
  case helpers.list_at(args, 1) {
    Some(init) -> reduce_loop(st, view, start, dir, cb, init)
    None ->
      case len == 0 {
        True ->
          rt_val.t_throw_type_error(
            st,
            "Reduce of empty array with no initial value",
          )
        False -> {
          let acc = ta_get(st, view.ref, start)
          reduce_loop(st, view, start + direction_step(dir), dir, cb, acc)
        }
      }
  }
}

fn reduce_loop(
  st: Agent,
  view: TaWitness,
  k: Int,
  dir: Direction,
  cb: JsVal,
  acc: JsVal,
) -> #(JsVal, Agent) {
  use <- bool.guard(k < 0 || k >= view.length, #(acc, st))
  let el = ta_get(st, view.ref, k)
  let #(res, st) =
    call(st, cb, mk_undefined(), [
      acc,
      el,
      mk_number(JInt(k)),
      mk_object(view.ref),
    ])
  reduce_loop(st, view, k + direction_step(dir), dir, cb, res)
}

// ============================================================================
// copyWithin / reverse / toReversed / with — byte-level methods
// ============================================================================

/// §23.2.3.5 copyWithin ( target, start [ , end ] ).
fn proto_copy_within(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let TaWitness(buffer: buf, kind:, byte_offset: off, length: len, ..) = view
  require_mutable(st, buf)
  let target_arg = helpers.first_arg_or_undefined(args)
  let start_arg = helpers.arg_at(args, 1)
  let end_arg = helpers.arg_at(args, 2)
  let #(t, st) = to_int_or_inf(st, target_arg)
  let #(s, st) = to_int_or_inf(st, start_arg)
  let #(e, st) = case classify(end_arg) {
    KUndef -> #(IPosInf, st)
    _ -> to_int_or_inf(st, end_arg)
  }
  let to = relative_index(t, len)
  let from = relative_index(s, len)
  let final = relative_index(e, len)
  let count = int.min(final - from, len - to)
  use <- bool.guard(count <= 0, #(this, st))
  // Steps 12.c-d: re-validate against the live buffer — the coercions above
  // can run user code that detaches the buffer or shrinks it below a FIXED
  // view; both throw TypeError.
  let data = witness_bytes(st, this)
  let size = typed_array_ffi.elem_size(kind)
  // Steps 12.e-f: len = min(len, TypedArrayLength) — a length-tracking
  // view follows a SHRUNK buffer (clamp indices/count down), but a grow
  // never raises the snapshot bounds.
  let live_len = int.min(len, ta_live_length(st, this))
  let to = int.min(to, live_len)
  let from = int.min(from, live_len)
  let final = int.min(final, live_len)
  let count = int.min(final - from, live_len - to)
  use <- bool.guard(count <= 0, #(this, st))
  // In range: the witness proved off + len * size fits `data`, and
  // from + count <= live_len <= len.
  let assert Ok(region) = bit_array.slice(data, off + from * size, count * size)
  let target = off + to * size
  let #(new_data, written) = splice_clamped(data, target, region)
  #(this, buffer.store_region(st, buf, new_data, target, written))
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
      // Callers pass a range already proved to lie inside `data`.
      let assert Ok(elem) = bit_array.slice(data, off + i * size, size)
      reversed_bytes_loop(data, off, len, size, i + 1, [elem, ..acc])
    }
  }
}

/// §23.2.3.25 reverse ( ) — in place, returns this.
fn proto_reverse(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let TaWitness(buffer: buf, kind:, byte_offset: off, length: len, ..) = view
  require_mutable(st, buf)
  case buffer.buffer_bytes(st, buf) {
    None -> #(this, st)
    Some(data) -> {
      let size = typed_array_ffi.elem_size(kind)
      let region = reversed_bytes(data, off, len, size)
      let #(new_data, written) = splice_clamped(data, off, region)
      #(this, buffer.store_region(st, buf, new_data, off, written))
    }
  }
}

/// TypedArrayCreateSameType (§23.2.4.3) — fresh array of the receiver's own
/// kind with the intrinsic default prototype (species is NOT consulted).
fn ta_same_type_create(
  st: Agent,
  kind: TypedArrayKind,
  len: Int,
) -> #(FreshTa, Agent) {
  alloc_ta_with_length(st, kind, default_proto_for(st, kind), len)
}

/// Commit `new_data` into a freshly allocated buffer this caller owns every
/// byte of, and return the new typed array as the result.
fn write_fresh_buffer(
  st: Agent,
  new_buf: Handle,
  new_data: BitArray,
  ta_val: JsVal,
) -> #(JsVal, Agent) {
  #(
    ta_val,
    buffer.store_region(st, new_buf, new_data, 0, bit_array.byte_size(new_data)),
  )
}

/// §23.2.3.32 toReversed ( ).
fn proto_to_reversed(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let TaWitness(buffer: buf, kind:, byte_offset: off, length: len, ..) = view
  let size = typed_array_ffi.elem_size(kind)
  let #(fresh, st) = ta_same_type_create(st, kind, len)
  let FreshTa(value: ta_val, buffer: new_buf, ..) = fresh
  let src = copy_region(st, buf, off, len * size)
  let new_data = reversed_bytes(src, 0, len, size)
  write_fresh_buffer(st, new_buf, new_data, ta_val)
}

/// §23.2.3.36 with ( index, value ).
fn proto_with(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let TaWitness(buffer: buf, kind:, byte_offset: off, length: len, ..) = view
  let index_arg = helpers.first_arg_or_undefined(args)
  let value_arg = helpers.arg_at(args, 1)
  let #(rel, st) = to_int_or_inf(st, index_arg)
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
  let #(converted, st) = convert_for_kind(st, kind, value_arg)
  let size = typed_array_ffi.elem_size(kind)
  // Step 9: IsValidIntegerIndex(O, actualIndex) against the LIVE view — a
  // grow during the conversion can make an initially out-of-bounds index
  // valid (and vice versa).
  let valid = actual >= 0 && actual < ta_live_length(st, this)
  use <- bool.lazy_guard(!valid, fn() {
    rt_val.t_throw_range_error(st, "Invalid typed array index")
  })
  // Step 10: A = TypedArrayCreateSameType(O, « len ») with the SNAPSHOT
  // length; the replacement value only lands when actualIndex is inside it.
  let #(fresh, st) = ta_same_type_create(st, kind, len)
  let FreshTa(value: ta_val, buffer: new_buf, ..) = fresh
  let data = copy_region(st, buf, off, len * size)
  let new_data = case actual < len {
    True -> buffer.typed_array_encode_value(data, actual * size, converted)
    False -> data
  }
  write_fresh_buffer(st, new_buf, new_data, ta_val)
}

// ============================================================================
// lastIndexOf — §23.2.3.20
// ============================================================================

fn proto_last_index_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let len = view.length
  use <- bool.guard(len == 0, #(mk_number(JInt(-1)), st))
  let search = helpers.first_arg_or_undefined(args)
  // Step 4: fromIndex PRESENT (even as undefined) → ToIntegerOrInfinity;
  // absent → len - 1.
  let #(n, st) = case helpers.list_at(args, 1) {
    None -> #(IInt(len - 1), st)
    Some(v) -> to_int_or_inf(st, v)
  }
  case n {
    INegInf -> #(mk_number(JInt(-1)), st)
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
      #(mk_number(JInt(search_down(st, view.ref, k, search))), st)
    }
  }
}

/// Scan [0, k] downward with live reads; invalid indices (fromIndex valueOf
/// shrank the buffer) are skipped per the HasProperty step.
fn search_down(st: Agent, ta_ref: Handle, k: Int, search: JsVal) -> Int {
  case k < 0 {
    True -> -1
    False ->
      case ta_read(st, ta_ref, k) {
        Some(el) ->
          case rt_val.strict_equal(el, search) {
            True -> k
            False -> search_down(st, ta_ref, k - 1, search)
          }
        None -> search_down(st, ta_ref, k - 1, search)
      }
  }
}

// ============================================================================
// sort / toSorted — §23.2.3.29 / §23.2.3.33
// ============================================================================

/// §23.2.4.7 CompareTypedArrayElements with comparefn undefined:
/// NaN sorts last (NaN vs NaN → 0), -0 before +0, otherwise numeric.
fn default_ta_compare(x: JsVal, y: JsVal) -> Int {
  case classify(x), classify(y) {
    KNum(a), KNum(b) -> compare_numbers(a, b)
    KBig(a), KBig(b) ->
      case a < b, a > b {
        True, _ -> -1
        _, True -> 1
        False, False -> 0
      }
    _, _ -> 0
  }
}

fn compare_numbers(a: JsNum, b: JsNum) -> Int {
  case a, b {
    JNan, JNan -> 0
    JNan, _ -> 1
    _, JNan -> -1
    JNegInf, JNegInf -> 0
    JPosInf, JPosInf -> 0
    JNegInf, _ -> -1
    _, JNegInf -> 1
    JPosInf, _ -> 1
    _, JPosInf -> -1
    _, _ -> {
      let fa = finite_to_float(a)
      let fb = finite_to_float(b)
      case fa <. fb, fa >. fb {
        True, _ -> -1
        _, True -> 1
        False, False -> {
          let a_neg_zero = rt_val.is_neg_zero(fa)
          let b_neg_zero = rt_val.is_neg_zero(fb)
          case a_neg_zero, b_neg_zero {
            True, False -> -1
            False, True -> 1
            _, _ -> 0
          }
        }
      }
    }
  }
}

/// A finite element value as a Float for ordering. Elements are at most 64
/// bits wide as integers only for BigInt kinds (handled apart), so every
/// Number element here converts exactly enough for comparison.
fn finite_to_float(n: JsNum) -> Float {
  case n {
    JInt(i) -> int.to_float(i)
    JFloat(f) -> f
    JNan | JPosInf | JNegInf -> 0.0
  }
}

/// A comparator: -1/0/1, threading Agent (a user comparefn can run arbitrary
/// JS).
fn compare_with(st: Agent, cmp: JsVal, x: JsVal, y: JsVal) -> #(Int, Agent) {
  case classify(cmp) {
    KUndef -> #(default_ta_compare(x, y), st)
    _ -> {
      let #(res, st) = call(st, cmp, mk_undefined(), [x, y])
      let #(n, st) = rt_val.t_to_number(st, res)
      let c = case n {
        JNan -> 0
        JInt(i) ->
          case i < 0, i > 0 {
            True, _ -> -1
            _, True -> 1
            False, False -> 0
          }
        JFloat(f) ->
          case f <. 0.0, f >. 0.0 {
            True, _ -> -1
            _, True -> 1
            False, False -> 0
          }
        JPosInf -> 1
        JNegInf -> -1
      }
      #(c, st)
    }
  }
}

/// Stable merge sort over the snapshot list, threading Agent through the
/// (possibly effectful) comparator.
fn sort_values(
  st: Agent,
  items: List(JsVal),
  cmp: JsVal,
) -> #(List(JsVal), Agent) {
  case items {
    [] | [_] -> #(items, st)
    _ -> {
      let #(left, right) = list.split(items, list.length(items) / 2)
      let #(ls, st) = sort_values(st, left, cmp)
      let #(rs, st) = sort_values(st, right, cmp)
      merge_values(st, ls, rs, cmp, [])
    }
  }
}

fn merge_values(
  st: Agent,
  left: List(JsVal),
  right: List(JsVal),
  cmp: JsVal,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case left, right {
    [], _ -> #(list.append(list.reverse(acc), right), st)
    _, [] -> #(list.append(list.reverse(acc), left), st)
    [x, ..xs], [y, ..ys] -> {
      let #(c, st) = compare_with(st, cmp, x, y)
      case c <= 0 {
        True -> merge_values(st, xs, right, cmp, [x, ..acc])
        False -> merge_values(st, left, ys, cmp, [y, ..acc])
      }
    }
  }
}

/// Encode a list of numeric values into one contiguous byte region — each
/// value is encoded into its own elem-size binary and the segments are
/// concatenated ONCE at the end (O(n)).
fn encode_region(
  kind: TypedArrayKind,
  size: Int,
  values: List(JsVal),
) -> BitArray {
  // The values are a snapshot READ from the typed array (sorted_snapshot),
  // so each one is a Number / BigInt; anything else encodes as zero.
  list.map(values, fn(v) {
    case buffer.decoded_element(kind, v) {
      Some(el) -> buffer.typed_array_encode_value(ta_zeroed(size), 0, el)
      None -> ta_zeroed(size)
    }
  })
  |> bit_array.concat
}

/// Shared sort/toSorted prologue: comparefn validated FIRST, then
/// ValidateTypedArray; snapshots the elements and sorts them, handing back
/// the validated view plus the sorted list.
fn sorted_snapshot(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(TaWitness, List(JsVal), Agent) {
  let cmp = helpers.first_arg_or_undefined(args)
  use <- bool.lazy_guard(
    classify(cmp) != KUndef && !rt_call.is_callable(st, cmp),
    fn() {
      rt_val.t_throw_type_error(
        st,
        "The comparison function must be either a function or undefined",
      )
    },
  )
  let view = validate_ta(st, this)
  let items = join_collect(st, view, 0, []) |> list.reverse
  let #(sorted, st) = sort_values(st, items, cmp)
  #(view, sorted, st)
}

/// §23.2.3.29 sort ( comparefn ) — sorts a snapshot, writes back in place.
fn proto_sort(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  // Immutable ArrayBuffer proposal: sort() has accessMode ~write~ — the
  // comparator must never run against an immutable-backed receiver. (The
  // spec checks comparator callability first, but both failures are
  // TypeErrors, so the order is unobservable.)
  let view_w = require_ta(st, this)
  require_mutable(st, view_w.buffer)
  let #(view, sorted, st) = sorted_snapshot(st, this, args)
  let TaWitness(buffer: buf, kind:, byte_offset: off, length: len, ..) = view
  case buffer.buffer_bytes(st, buf) {
    None -> #(this, st)
    Some(data) -> {
      let size = typed_array_ffi.elem_size(kind)
      // Build the sorted region in ONE pass, then splice it into the live
      // buffer with ONE rebuild — clamped to the view's CURRENTLY VALID
      // indices (the buffer may have shrunk during a user comparefn).
      // Per-element Set semantics: a fixed view that no longer fits is
      // wholly out of bounds — NO index is valid, so nothing is written —
      // while a shrunk length-tracking view accepts its first live elements.
      let region = encode_region(kind, size, sorted)
      let avail = int.min(len, ta_live_length(st, this)) * size
      // `region` is exactly len * size bytes and avail <= that.
      let region = case avail == len * size {
        True -> region
        False -> {
          let assert Ok(region) = bit_array.slice(region, 0, avail)
          region
        }
      }
      let #(new_data, written) = splice_clamped(data, off, region)
      case written > 0 {
        True -> #(this, buffer.store_region(st, buf, new_data, off, written))
        False -> #(this, st)
      }
    }
  }
}

/// §23.2.3.33 toSorted ( comparefn ).
fn proto_to_sorted(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(view, sorted, st) = sorted_snapshot(st, this, args)
  let TaWitness(kind:, length: len, ..) = view
  let #(fresh, st) = ta_same_type_create(st, kind, len)
  let FreshTa(value: ta_val, buffer: new_buf, ..) = fresh
  let size = typed_array_ffi.elem_size(kind)
  // The fresh buffer is exactly len * size bytes, so the concatenated
  // region IS the new buffer contents — no splice needed.
  let new_data = case buffer.buffer_bytes(st, new_buf) {
    Some(_fresh) -> encode_region(kind, size, sorted)
    None -> ta_zeroed(len * size)
  }
  write_fresh_buffer(st, new_buf, new_data, ta_val)
}

// ============================================================================
// toLocaleString — §23.2.3.31
// ============================================================================

fn proto_to_locale_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  locale_loop(st, view, 0, [])
}

fn locale_loop(
  st: Agent,
  view: TaWitness,
  k: Int,
  acc: List(String),
) -> #(JsVal, Agent) {
  use <- bool.guard(k >= view.length, #(
    mk_string(string.join(list.reverse(acc), ",")),
    st,
  ))
  let el = ta_get(st, view.ref, k)
  case rt_val.is_nullish(el) {
    True -> locale_loop(st, view, k + 1, ["", ..acc])
    False -> {
      let #(m, st) =
        rt_obj.t_get_prop(st, el, StringKey(Named("toLocaleString")))
      let #(res, st) = call(st, m, el, [])
      let #(s, st) = rt_val.t_to_string(st, res)
      locale_loop(st, view, k + 1, [s, ..acc])
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
  st: Agent,
  exemplar: JsVal,
  kind: TypedArrayKind,
) -> #(Option(JsVal), Agent) {
  let default_ctor = typed_array_pair(st, kind).constructor
  // C = Get(exemplar, "constructor").
  let #(ctor, st) =
    rt_obj.t_get_prop(st, exemplar, StringKey(Named("constructor")))
  case classify(ctor) {
    KUndef -> #(None, st)
    KHandle(_) -> {
      // S = Get(C, @@species); null/undefined → default.
      let #(species, st) =
        rt_obj.t_get_prop(st, ctor, SymbolKey(symbol_species))
      case classify(species) {
        KNull | KUndef -> #(None, st)
        KHandle(species_ref) ->
          case species_ref == default_ctor {
            True -> #(None, st)
            False ->
              case rt_call.is_constructor(st, species) {
                True -> #(Some(species), st)
                False ->
                  rt_val.t_throw_type_error(
                    st,
                    "Species constructor is not a constructor",
                  )
              }
          }
        _ ->
          rt_val.t_throw_type_error(
            st,
            "Species constructor is not a constructor",
          )
      }
    }
    _ -> rt_val.t_throw_type_error(st, "Constructor property is not an object")
  }
}

/// TypedArraySpeciesCreate post-construct check: the result's content type
/// (number vs bigint) must match the exemplar's.
fn check_content_type(
  st: Agent,
  obj: JsVal,
  obj_ref: Handle,
  kind: TypedArrayKind,
) -> #(JsVal, Handle) {
  case ta_slot_of(st, obj_ref) {
    Some(TaView(kind: result_kind, ..)) ->
      case same_content_type(result_kind, kind) {
        True -> #(obj, obj_ref)
        False ->
          rt_val.t_throw_type_error(
            st,
            "Content types of source and created typed arrays differ",
          )
      }
    None -> witness_type_error(st, buffer.NotAView)
  }
}

/// SpeciesConstructor(exemplar, default %Kind%Array%) then
/// TypedArrayCreateFromConstructor(« len »); content type must match.
fn ta_species_create(
  st: Agent,
  exemplar: JsVal,
  kind: TypedArrayKind,
  len: Int,
) -> #(#(JsVal, Handle), Agent) {
  let #(maybe_ctor, st) = resolve_species_ctor(st, exemplar, kind)
  case maybe_ctor {
    None -> {
      let #(fresh, st) = ta_same_type_create(st, kind, len)
      #(#(fresh.value, fresh.ta_ref), st)
    }
    Some(species) -> {
      let #(obj, obj_ref, st) = ta_create(st, species, len)
      #(check_content_type(st, obj, obj_ref, kind), st)
    }
  }
}

// ============================================================================
// Small shared helpers
// ============================================================================

/// The realm's constructor/prototype pair for a concrete kind — the table is
/// total over `all_typed_array_kinds`, so there is no "kind not installed"
/// case once `init` has run.
fn typed_array_pair(st: Agent, kind: TypedArrayKind) -> BuiltinPair {
  let assert Ok(bt) = dict.get(st.realm.typed_arrays.by_kind, kind)
    as "typed_array: kind missing from realm.typed_arrays"
  bt
}

/// The intrinsic default prototype for a concrete TypedArray kind.
fn default_proto_for(st: Agent, kind: TypedArrayKind) -> Handle {
  typed_array_pair(st, kind).prototype
}

/// §10.1.13.2 GetPrototypeFromConstructor with the intrinsic fallback.
fn proto_from_new_target(
  st: Agent,
  new_target: JsVal,
  fallback: Handle,
) -> #(Handle, Agent) {
  let #(proto, st) =
    rt_obj.t_get_prop(st, new_target, StringKey(Named("prototype")))
  case classify(proto) {
    KHandle(h) -> #(h, st)
    _ -> #(fallback, st)
  }
}
