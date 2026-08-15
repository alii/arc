//// ES2024 §23.2 TypedArray Objects — %TypedArray% + 11 concrete constructors
////
//// %TypedArray% is the abstract superclass; every concrete kind's prototype
//// inherits from `%TypedArray%.prototype` and every concrete constructor's
//// [[Prototype]] is `%TypedArray%` itself (§23.2.5/§23.2.6/§23.2.7). Internal
//// storage: `TypedArrayObj(buffer, offset, len, kind)` exotic kind. Port of
//// arc `builtins/typed_array.gleam:79-288` init/dispatch re-expressed under
//// D7/R1.

import arc/rt/builtins/array_buffer
import arc/rt/builtins/common
import arc/rt/builtins/data_view
import arc/rt/builtins/helpers
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type TypedArrayKind,
  type TypedArrayNative, type TypedArrays, ArrayBufferObj, ArrayIterEntries,
  ArrayIterKeys, ArrayIterValues, ArrayIterator, BigInt64, BigUint64, Index,
  JInt, KHandle, KUndef, Named, NoElements, ReturnThis, SObject, StringKey,
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
  TypedArrayPrototypeWith, TypedArrays, all_typed_array_kinds, classify,
  mk_number, mk_object, mk_string, mk_undefined, typed_array_elem_size,
  typed_array_name,
} as rt_types
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

/// Back-compat alias for `rt_types.typed_array_name` (init_realm calls it
/// as `b_typed_array.kind_name`).
pub fn kind_name(kind: TypedArrayKind) -> String {
  typed_array_name(kind)
}

/// Back-compat alias for `rt_types.all_typed_array_kinds`.
pub const all_kinds = all_typed_array_kinds

// ═══════════════════════════════════════════════════════════════════════════
// Init — %TypedArray% + 11 concrete constructors
// ═══════════════════════════════════════════════════════════════════════════

/// Allocate %TypedArray% (abstract) and one BuiltinPair per concrete kind.
/// Return shape matches `realm.typed_arrays: TypedArrays(by_kind: Dict)`.
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(#(BuiltinPair, TypedArrays), Agent) {
  // §23.2.3.1-3/.22 accessor getters on %TypedArray%.prototype.
  let #(getters, st) =
    common.alloc_getters(st, fn_proto, [
      #("buffer", TypedArrayN(TypedArrayGetBuffer)),
      #("byteLength", TypedArrayN(TypedArrayGetByteLength)),
      #("byteOffset", TypedArrayN(TypedArrayGetByteOffset)),
      #("length", TypedArrayN(TypedArrayGetLength)),
    ])
  // values() doubles as [@@iterator] — SAME function object.
  let #(values_h, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      TypedArrayN(TypedArrayPrototypeValues),
      "values",
      0,
    )
  let #(values_prop, st) = common.builtin_property(st, mk_object(values_h))
  let #(methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("at", TypedArrayN(TypedArrayPrototypeAt), 1),
      #("copyWithin", TypedArrayN(TypedArrayPrototypeCopyWithin), 2),
      #("entries", TypedArrayN(TypedArrayPrototypeEntries), 0),
      #("every", TypedArrayN(TypedArrayPrototypeEvery), 1),
      #("fill", TypedArrayN(TypedArrayPrototypeFill), 1),
      #("filter", TypedArrayN(TypedArrayPrototypeFilter), 1),
      #("find", TypedArrayN(TypedArrayPrototypeFind), 1),
      #("findIndex", TypedArrayN(TypedArrayPrototypeFindIndex), 1),
      #("findLast", TypedArrayN(TypedArrayPrototypeFindLast), 1),
      #("findLastIndex", TypedArrayN(TypedArrayPrototypeFindLastIndex), 1),
      #("forEach", TypedArrayN(TypedArrayPrototypeForEach), 1),
      #("includes", TypedArrayN(TypedArrayPrototypeIncludes), 1),
      #("indexOf", TypedArrayN(TypedArrayPrototypeIndexOf), 1),
      #("join", TypedArrayN(TypedArrayPrototypeJoin), 1),
      #("keys", TypedArrayN(TypedArrayPrototypeKeys), 0),
      #("lastIndexOf", TypedArrayN(TypedArrayPrototypeLastIndexOf), 1),
      #("map", TypedArrayN(TypedArrayPrototypeMap), 1),
      #("reduce", TypedArrayN(TypedArrayPrototypeReduce), 1),
      #("reduceRight", TypedArrayN(TypedArrayPrototypeReduceRight), 1),
      #("reverse", TypedArrayN(TypedArrayPrototypeReverse), 0),
      #("set", TypedArrayN(TypedArrayPrototypeSet), 1),
      #("slice", TypedArrayN(TypedArrayPrototypeSlice), 2),
      #("some", TypedArrayN(TypedArrayPrototypeSome), 1),
      #("sort", TypedArrayN(TypedArrayPrototypeSort), 1),
      #("subarray", TypedArrayN(TypedArrayPrototypeSubarray), 2),
      #("toLocaleString", TypedArrayN(TypedArrayPrototypeToLocaleString), 0),
      #("toReversed", TypedArrayN(TypedArrayPrototypeToReversed), 0),
      #("toSorted", TypedArrayN(TypedArrayPrototypeToSorted), 1),
      #("with", TypedArrayN(TypedArrayPrototypeWith), 2),
    ])
  let proto_props = list.flatten([getters, [#("values", values_prop)], methods])
  // §23.2.2 statics — from/of, inherited by all 11.
  let #(statics, st) =
    common.alloc_methods(st, fn_proto, [
      #("from", TypedArrayN(TypedArrayFrom), 1),
      #("of", TypedArrayN(TypedArrayOf), 0),
    ])
  let #(ta, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      proto_props,
      fn(_) { TypedArrayN(TypedArrayIntrinsicConstructor) },
      "TypedArray",
      0,
      statics,
    )
  // %TypedArray%.prototype[@@iterator] === .values (SAME fn object; restamp).
  let #(iter_prop, st) = common.restamp(st, values_prop)
  let st =
    common.add_symbol_property(
      st,
      ta.prototype,
      rt_types.symbol_iterator,
      iter_prop,
    )
  // §23.2.3.38 get %TypedArray%.prototype[@@toStringTag] — accessor returning
  // [[TypedArrayName]].
  let #(tag_get, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
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
    common.add_symbol_property(
      st,
      ta.prototype,
      rt_types.symbol_to_string_tag,
      tag_prop,
    )
  // §23.2.2.4 get %TypedArray%[@@species].
  let st = common.add_species_accessor(st, fn_proto, ta.constructor, ReturnThis)
  // 11 concrete kinds: proto → %TypedArray.prototype%; ctor → %TypedArray%.
  let #(by_kind, st) =
    list.fold(all_typed_array_kinds, #(dict.new(), st), fn(acc, kind) {
      let #(d, st) = acc
      let #(pair, st) = init_concrete(st, ta, kind)
      #(dict.insert(d, kind, pair), st)
    })
  #(#(ta, TypedArrays(by_kind:)), st)
}

/// One concrete TypedArray constructor + prototype. `BYTES_PER_ELEMENT` is
/// {W:F, E:F, C:F} on BOTH ctor and prototype (§23.2.6.2 / §23.2.7.2).
fn init_concrete(
  st: Agent,
  ta: BuiltinPair,
  kind: TypedArrayKind,
) -> #(BuiltinPair, Agent) {
  let size = typed_array_elem_size(kind)
  let #(size_prop, st) = common.data_prop(st, mk_number(JInt(size)))
  let #(size_prop2, st) = common.restamp(st, size_prop)
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
  #(bt, st)
}

// ═══════════════════════════════════════════════════════════════════════════
// Dispatch
// ═══════════════════════════════════════════════════════════════════════════

/// Per-module [[Call]] dispatch. All TypedArray constructors throw without
/// `new` (§23.2.1.1 step 1 / §23.2.5.1 step 1).
pub fn dispatch(
  st: Agent,
  native: TypedArrayNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
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
    TypedArrayFrom -> ta_from(st, this, args)
    TypedArrayOf -> ta_of(st, this, args)
    TypedArrayPrototypeAt -> proto_at(st, this, args)
    TypedArrayPrototypeCopyWithin -> proto_copy_within(st, this, args)
    TypedArrayPrototypeEntries -> proto_iter(st, this, ArrayIterEntries)
    TypedArrayPrototypeEvery -> proto_every_some(st, this, args, True)
    TypedArrayPrototypeFill -> proto_fill(st, this, args)
    TypedArrayPrototypeFilter -> proto_filter(st, this, args)
    TypedArrayPrototypeFind -> proto_find(st, this, args, Ascending, FindValue)
    TypedArrayPrototypeFindIndex ->
      proto_find(st, this, args, Ascending, FindIdx)
    TypedArrayPrototypeFindLast ->
      proto_find(st, this, args, Descending, FindValue)
    TypedArrayPrototypeFindLastIndex ->
      proto_find(st, this, args, Descending, FindIdx)
    TypedArrayPrototypeForEach -> proto_for_each(st, this, args)
    TypedArrayPrototypeIncludes ->
      proto_search(st, this, args, rt_val.same_value_zero, True, fn(i) {
        rt_types.mk_bool(i >= 0)
      })
    TypedArrayPrototypeIndexOf ->
      proto_search(st, this, args, rt_val.strict_equal, False, fn(i) {
        mk_number(JInt(i))
      })
    TypedArrayPrototypeJoin -> proto_join(st, this, args)
    TypedArrayPrototypeKeys -> proto_iter(st, this, ArrayIterKeys)
    TypedArrayPrototypeLastIndexOf -> proto_last_index_of(st, this, args)
    TypedArrayPrototypeMap -> proto_map(st, this, args)
    TypedArrayPrototypeReduce -> proto_reduce(st, this, args, Ascending)
    TypedArrayPrototypeReduceRight -> proto_reduce(st, this, args, Descending)
    TypedArrayPrototypeReverse -> proto_reverse(st, this)
    TypedArrayPrototypeSet -> proto_set(st, this, args)
    TypedArrayPrototypeSlice -> proto_slice(st, this, args)
    TypedArrayPrototypeSome -> proto_every_some(st, this, args, False)
    TypedArrayPrototypeSort -> proto_sort(st, this, args, False)
    TypedArrayPrototypeSubarray -> proto_subarray(st, this, args)
    TypedArrayPrototypeToLocaleString -> proto_to_locale_string(st, this)
    TypedArrayPrototypeToReversed -> proto_to_reversed(st, this)
    TypedArrayPrototypeToSorted -> proto_sort(st, this, args, True)
    TypedArrayPrototypeValues -> proto_iter(st, this, ArrayIterValues)
    TypedArrayPrototypeWith -> proto_with(st, this, args)
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
    TypedArrayIntrinsicConstructor ->
      rt_val.t_throw_type_error(
        st,
        "Abstract class TypedArray not directly constructable",
      )
    TypedArrayConstructor(kind:, proto:) ->
      constructor(st, kind, proto, args, new_target)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// ── §23.2.5.1 TypedArray ( ...args ) ────────────────────────────────────────

fn constructor(
  st: Agent,
  kind: TypedArrayKind,
  fallback_proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let #(proto, st) = proto_from_new_target(st, new_target, fallback_proto)
  let elem_size = typed_array_elem_size(kind)
  case args {
    // §23.2.5.1.1: no args → length-0 buffer.
    [] -> alloc_with_length(st, kind, proto, 0)
    [first, ..] ->
      case classify(first) {
        // §23.2.5.1.3: ArrayBuffer overload.
        KHandle(h) ->
          case rt_store.t_cell_get(st, h) {
            SObject(kind: ArrayBufferObj(bytes:, detached:), ..) -> {
              case detached {
                True ->
                  rt_val.t_throw_type_error(
                    st,
                    "Cannot construct "
                      <> typed_array_name(kind)
                      <> " with a detached ArrayBuffer",
                  )
                False -> Nil
              }
              let #(_, off_v, len_v) = helpers.three_args_or_undefined(args)
              let #(offset, st) =
                rt_val.t_to_index(st, off_v, "Invalid typed array byteOffset")
              let buf_len = bit_array.byte_size(bytes)
              case offset % elem_size != 0 {
                True ->
                  rt_val.t_throw_range_error(
                    st,
                    "start offset of "
                      <> typed_array_name(kind)
                      <> " should be a multiple of "
                      <> int_str(elem_size),
                  )
                False -> Nil
              }
              let #(len, st) = case classify(len_v) {
                rt_types.KUndef -> {
                  case { buf_len - offset } % elem_size != 0 {
                    True ->
                      rt_val.t_throw_range_error(
                        st,
                        "byte length of "
                          <> typed_array_name(kind)
                          <> " should be a multiple of "
                          <> int_str(elem_size),
                      )
                    False -> Nil
                  }
                  #({ buf_len - offset } / elem_size, st)
                }
                _ -> rt_val.t_to_index(st, len_v, "Invalid typed array length")
              }
              case offset + len * elem_size > buf_len {
                True ->
                  rt_val.t_throw_range_error(
                    st,
                    "Invalid typed array length: " <> int_str(len),
                  )
                False -> Nil
              }
              alloc_view(st, kind, proto, h, offset, len)
            }
            // §23.2.5.1.2: TypedArray overload — copy elements.
            SObject(kind: TypedArrayObj(len: src_len, ..), ..) ->
              alloc_with_length(st, kind, proto, src_len)
            // §23.2.5.1.4: object → iterable/array-like.
            _ -> alloc_with_length(st, kind, proto, 0)
          }
        // §23.2.5.1.5: numeric length.
        _ -> {
          let #(len, st) =
            rt_val.t_to_index(st, first, "Invalid typed array length")
          alloc_with_length(st, kind, proto, len)
        }
      }
  }
}

fn alloc_with_length(
  st: Agent,
  kind: TypedArrayKind,
  proto: Handle,
  len: Int,
) -> #(Handle, Agent) {
  let realm = st.realm
  let byte_len = len * typed_array_elem_size(kind)
  let #(buf_h, st) =
    array_buffer.alloc_buffer(st, realm.array_buffer.prototype, byte_len)
  alloc_view(st, kind, proto, buf_h, 0, len)
}

fn alloc_view(
  st: Agent,
  kind: TypedArrayKind,
  proto: Handle,
  buffer: Handle,
  offset: Int,
  len: Int,
) -> #(Handle, Agent) {
  rt_store.t_cell_new(
    st,
    SObject(
      kind: TypedArrayObj(buffer:, offset:, len:, kind:),
      proto: Some(proto),
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

// ── prototype accessors ─────────────────────────────────────────────────────

fn get_buffer(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(buffer, _, _, _) = require_ta(st, this)
  #(mk_object(buffer), st)
}

fn get_byte_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(_, _, len, kind) = require_ta(st, this)
  #(mk_number(JInt(len * typed_array_elem_size(kind))), st)
}

fn get_byte_offset(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(_, offset, _, _) = require_ta(st, this)
  #(mk_number(JInt(offset)), st)
}

fn get_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(_, _, len, _) = require_ta(st, this)
  #(mk_number(JInt(len)), st)
}

/// §23.2.3.38: undefined for a non-TypedArray receiver — never throws.
fn get_to_string_tag(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: TypedArrayObj(kind:, ..), ..) -> #(
          mk_string(typed_array_name(kind)),
          st,
        )
        _ -> #(mk_undefined(), st)
      }
    _ -> #(mk_undefined(), st)
  }
}

// ── brand check / helpers ───────────────────────────────────────────────────

fn require_ta(st: Agent, v: JsVal) -> #(Handle, Int, Int, TypedArrayKind) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: TypedArrayObj(buffer:, offset:, len:, kind:), ..) -> #(
          buffer,
          offset,
          len,
          kind,
        )
        _ ->
          rt_val.t_throw_type_error(
            st,
            "Method called on incompatible receiver: not a TypedArray",
          )
      }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "Method called on incompatible receiver: not a TypedArray",
      )
  }
}

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

@external(erlang, "erlang", "integer_to_binary")
fn int_str(n: Int) -> String

// ═══════════════════════════════════════════════════════════════════════════
// Element access — data_view's read/write reused per §25.1.3.16/.18
// ═══════════════════════════════════════════════════════════════════════════

/// Validated view: brand-checked, buffer not detached. arc `TaWitness`
/// simplified (2core buffers are fixed-length; no length-tracking).
type TaView {
  TaView(ta: Handle, buffer: Handle, kind: TypedArrayKind, off: Int, len: Int)
}

/// §23.2.4.4 ValidateTypedArray: brand-check + IsDetachedBuffer.
fn validate_ta(st: Agent, v: JsVal) -> TaView {
  let #(buffer, off, len, kind) = require_ta(st, v)
  let assert KHandle(ta) = classify(v)
  case buffer_bytes(st, buffer) {
    Some(_) -> TaView(ta:, buffer:, kind:, off:, len:)
    None ->
      rt_val.t_throw_type_error(
        st,
        "Cannot perform %TypedArray%.prototype method on a detached ArrayBuffer",
      )
  }
}

fn buffer_bytes(st: Agent, buffer: Handle) -> Option(BitArray) {
  case rt_store.t_cell_get(st, buffer) {
    SObject(kind: ArrayBufferObj(bytes:, detached: False), ..) -> Some(bytes)
    _ -> None
  }
}

fn write_buffer(st: Agent, buffer: Handle, bytes: BitArray) -> Agent {
  rt_store.t_cell_update(st, buffer, fn(slot) {
    case slot {
      SObject(kind: ArrayBufferObj(detached:, ..), ..) ->
        SObject(..slot, kind: ArrayBufferObj(bytes:, detached:))
      _ -> slot
    }
  })
}

/// §10.4.5.16 IntegerIndexedElementGet — Some(v) in-range, None otherwise.
/// Typed arrays are little-endian (host order per §25.1.3.16 note; BEAM has
/// no host endianness — LE matches every target §10 supports).
fn ta_read(st: Agent, view: TaView, k: Int) -> Option(JsVal) {
  case k < 0 || k >= view.len {
    True -> None
    False ->
      case buffer_bytes(st, view.buffer) {
        None -> None
        Some(bytes) ->
          Some(data_view.read_elem(
            bytes,
            view.off + k * typed_array_elem_size(view.kind),
            view.kind,
            True,
          ))
      }
  }
}

fn ta_get(st: Agent, view: TaView, k: Int) -> JsVal {
  ta_read(st, view, k) |> option.unwrap(mk_undefined())
}

/// §10.4.5.18 IntegerIndexedElementSet — silently drops out-of-range writes.
fn ta_write(st: Agent, view: TaView, k: Int, v: JsVal) -> Agent {
  let #(raw, st) = data_view.coerce_elem_value(st, v, view.kind)
  ta_write_raw(st, view, k, raw)
}

fn ta_write_raw(
  st: Agent,
  view: TaView,
  k: Int,
  raw: data_view.ElemRaw,
) -> Agent {
  case k < 0 || k >= view.len {
    True -> st
    False ->
      case buffer_bytes(st, view.buffer) {
        None -> st
        Some(bytes) -> {
          let size = typed_array_elem_size(view.kind)
          write_buffer(
            st,
            view.buffer,
            data_view.write_elem(
              bytes,
              view.off + k * size,
              view.kind,
              raw,
              True,
            ),
          )
        }
      }
  }
}

/// Re-read a TaView by handle (after user code may have detached).
fn reread_view(st: Agent, ta: Handle) -> Option(TaView) {
  case rt_store.t_cell_get(st, ta) {
    SObject(kind: TypedArrayObj(buffer:, offset:, len:, kind:), ..) ->
      case buffer_bytes(st, buffer) {
        Some(_) -> Some(TaView(ta:, buffer:, kind:, off: offset, len:))
        None -> None
      }
    _ -> None
  }
}

// ── index arithmetic ────────────────────────────────────────────────────────

type IntOrInf {
  IInt(Int)
  IPosInf
  INegInf
}

fn to_int_or_inf(st: Agent, v: JsVal) -> #(IntOrInf, Agent) {
  let #(n, st) = rt_val.t_to_number(st, v)
  case n {
    JInt(i) -> #(IInt(i), st)
    rt_types.JFloat(f) -> #(IInt(rt_val.float_to_int(f)), st)
    rt_types.JNan -> #(IInt(0), st)
    rt_types.JPosInf -> #(IPosInf, st)
    rt_types.JNegInf -> #(INegInf, st)
  }
}

/// §23.2.3 relative-index clamp: negative → len+i clamped to 0; +∞ → len.
fn relative_index(i: IntOrInf, len: Int) -> Int {
  case i {
    IInt(n) ->
      case n < 0 {
        True -> int.max(len + n, 0)
        False -> int.min(n, len)
      }
    IPosInf -> len
    INegInf -> 0
  }
}

// ── callback helpers (arc iterate_calls / require_cb) ───────────────────────

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

fn require_cb(st: Agent, args: List(JsVal)) -> #(JsVal, JsVal, Agent) {
  let cb = helpers.first_arg_or_undefined(args)
  let #(is_call, st) = rt_val.t_is_callable(st, cb)
  case is_call {
    True -> #(cb, helpers.arg_at(args, 1), st)
    False -> rt_val.t_throw_type_error(st, "callback is not a function")
  }
}

fn call_cb(
  st: Agent,
  cb: JsVal,
  this_arg: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let js = st.store
  js.ops.call(st, cb, this_arg, args)
}

/// Generic callback loop over [k, len) or [k, 0]; `decide` may stop early.
fn iterate_calls(
  st: Agent,
  view: TaView,
  k: Int,
  dir: Direction,
  cb: JsVal,
  this_arg: JsVal,
  decide: fn(JsVal, JsVal, Int) -> Option(JsVal),
) -> #(Option(JsVal), Agent) {
  case k < 0 || k >= view.len {
    True -> #(None, st)
    False -> {
      let el = ta_get(st, view, k)
      let #(res, st) =
        call_cb(st, cb, this_arg, [el, mk_number(JInt(k)), mk_object(view.ta)])
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
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// %TypedArray%.prototype methods — arc per-method port
// ═══════════════════════════════════════════════════════════════════════════

/// §23.2.3.1 at ( index ).
fn proto_at(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(rel, st) = to_int_or_inf(st, helpers.first_arg_or_undefined(args))
  let k = case rel {
    IInt(n) ->
      case n < 0 {
        True -> view.len + n
        False -> n
      }
    IPosInf -> view.len
    INegInf -> -1
  }
  #(ta_read(st, view, k) |> option.unwrap(mk_undefined()), st)
}

/// §23.2.3.8 fill ( value [ , start [ , end ] ] ).
fn proto_fill(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(raw, st) =
    data_view.coerce_elem_value(
      st,
      helpers.first_arg_or_undefined(args),
      view.kind,
    )
  let #(s, st) = to_int_or_inf(st, helpers.arg_at(args, 1))
  let #(e, st) = case classify(helpers.arg_at(args, 2)) {
    KUndef -> #(IPosInf, st)
    _ -> to_int_or_inf(st, helpers.arg_at(args, 2))
  }
  let start = relative_index(s, view.len)
  let end = relative_index(e, view.len)
  // Re-validate: coercions above may detach.
  case reread_view(st, view.ta) {
    None ->
      rt_val.t_throw_type_error(
        st,
        "Cannot perform %TypedArray%.prototype.fill on a detached ArrayBuffer",
      )
    Some(view) -> {
      let st = fill_loop(st, view, start, end, raw)
      #(this, st)
    }
  }
}

fn fill_loop(
  st: Agent,
  view: TaView,
  k: Int,
  end: Int,
  raw: data_view.ElemRaw,
) -> Agent {
  case k >= end {
    True -> st
    False -> fill_loop(ta_write_raw(st, view, k, raw), view, k + 1, end, raw)
  }
}

/// §23.2.3.16 join ( separator ).
fn proto_join(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(sep, st) = case classify(helpers.first_arg_or_undefined(args)) {
    KUndef -> #(",", st)
    _ -> rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  }
  let parts =
    list.reverse(join_collect(st, view, 0, []))
    |> list.map(elem_to_join_str)
  #(mk_string(join_sep(parts, sep, True, "")), st)
}

fn join_collect(
  st: Agent,
  view: TaView,
  i: Int,
  acc: List(JsVal),
) -> List(JsVal) {
  case i >= view.len {
    True -> acc
    False -> join_collect(st, view, i + 1, [ta_get(st, view, i), ..acc])
  }
}

fn elem_to_join_str(v: JsVal) -> String {
  case classify(v) {
    rt_types.KNum(n) -> rt_val.jsnum_to_string(n)
    rt_types.KBig(b) -> int_str(b)
    _ -> ""
  }
}

/// §23.2.3.{19,35,7} keys/values/entries — CreateArrayIterator over `this`.
fn proto_iter(
  st: Agent,
  this: JsVal,
  kind: rt_types.ArrayIterKind,
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let iter_proto = st.realm.array_iter_proto
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: ArrayIterator(target: view.ta, index: 0, kind:),
        proto: Some(iter_proto),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(mk_object(h), st)
}

/// §23.2.3.7 every / §23.2.3.28 some.
fn proto_every_some(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  is_every: Bool,
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(cb, this_arg, st) = require_cb(st, args)
  let #(early, st) =
    iterate_calls(st, view, 0, Ascending, cb, this_arg, fn(res, _el, _k) {
      case rt_val.to_boolean(res) == is_every {
        True -> None
        False -> Some(rt_types.mk_bool(!is_every))
      }
    })
  #(early |> option.unwrap(rt_types.mk_bool(is_every)), st)
}

/// §23.2.3.15 forEach.
fn proto_for_each(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(cb, this_arg, st) = require_cb(st, args)
  let #(_, st) =
    iterate_calls(st, view, 0, Ascending, cb, this_arg, fn(_r, _e, _k) { None })
  #(mk_undefined(), st)
}

type FindMode {
  FindValue
  FindIdx
}

/// §23.2.3.11-14 find/findIndex/findLast/findLastIndex.
fn proto_find(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  dir: Direction,
  mode: FindMode,
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(cb, this_arg, st) = require_cb(st, args)
  let start = direction_start(dir, view.len)
  let #(early, st) =
    iterate_calls(st, view, start, dir, cb, this_arg, fn(res, el, k) {
      case rt_val.to_boolean(res) {
        True ->
          Some(case mode {
            FindValue -> el
            FindIdx -> mk_number(JInt(k))
          })
        False -> None
      }
    })
  let default = case mode {
    FindValue -> mk_undefined()
    FindIdx -> mk_number(JInt(-1))
  }
  #(early |> option.unwrap(default), st)
}

/// §23.2.3.16/.13 indexOf / includes shared body.
fn proto_search(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  eq: fn(JsVal, JsVal) -> Bool,
  missing_undef: Bool,
  done: fn(Int) -> JsVal,
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  case view.len == 0 {
    True -> #(done(-1), st)
    False -> {
      let search = helpers.first_arg_or_undefined(args)
      let #(n, st) = to_int_or_inf(st, helpers.arg_at(args, 1))
      let k = case n {
        INegInf -> 0
        IPosInf -> view.len
        IInt(i) ->
          case i >= 0 {
            True -> i
            False -> int.max(view.len + i, 0)
          }
      }
      #(done(search_loop(st, view, k, search, eq, missing_undef)), st)
    }
  }
}

fn search_loop(
  st: Agent,
  view: TaView,
  i: Int,
  search: JsVal,
  eq: fn(JsVal, JsVal) -> Bool,
  missing_undef: Bool,
) -> Int {
  case i >= view.len {
    True -> -1
    False -> {
      let matched = case ta_read(st, view, i) {
        Some(el) -> eq(el, search)
        None -> missing_undef && eq(mk_undefined(), search)
      }
      case matched {
        True -> i
        False -> search_loop(st, view, i + 1, search, eq, missing_undef)
      }
    }
  }
}

/// §23.2.3.20 lastIndexOf.
fn proto_last_index_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  case view.len == 0 {
    True -> #(mk_number(JInt(-1)), st)
    False -> {
      let search = helpers.first_arg_or_undefined(args)
      let #(n, st) = case helpers.list_at(args, 1) {
        None -> #(IInt(view.len - 1), st)
        Some(v) -> to_int_or_inf(st, v)
      }
      case n {
        INegInf -> #(mk_number(JInt(-1)), st)
        _ -> {
          let k = case n {
            IPosInf -> view.len - 1
            IInt(i) ->
              case i >= 0 {
                True -> int.min(i, view.len - 1)
                False -> view.len + i
              }
            INegInf -> -1
          }
          #(mk_number(JInt(search_down(st, view, k, search))), st)
        }
      }
    }
  }
}

fn search_down(st: Agent, view: TaView, k: Int, search: JsVal) -> Int {
  case k < 0 {
    True -> -1
    False ->
      case ta_read(st, view, k) {
        Some(el) ->
          case rt_val.strict_equal(el, search) {
            True -> k
            False -> search_down(st, view, k - 1, search)
          }
        None -> search_down(st, view, k - 1, search)
      }
  }
}

/// §23.2.3.22 map — result via TypedArraySpeciesCreate(O, «len»).
fn proto_map(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(cb, this_arg, st) = require_cb(st, args)
  let #(target, st) = ta_species_create(st, this, view.kind, view.len)
  let st = map_loop(st, view, 0, cb, this_arg, target)
  #(mk_object(target.ta), st)
}

fn map_loop(
  st: Agent,
  view: TaView,
  k: Int,
  cb: JsVal,
  this_arg: JsVal,
  target: TaView,
) -> Agent {
  case k >= view.len {
    True -> st
    False -> {
      let el = ta_get(st, view, k)
      let #(mapped, st) =
        call_cb(st, cb, this_arg, [el, mk_number(JInt(k)), mk_object(view.ta)])
      let st = ta_write(st, target, k, mapped)
      map_loop(st, view, k + 1, cb, this_arg, target)
    }
  }
}

/// §23.2.3.10 filter.
fn proto_filter(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(cb, this_arg, st) = require_cb(st, args)
  let #(kept, st) = filter_collect(st, view, 0, cb, this_arg, [])
  let kept = list.reverse(kept)
  let #(target, st) = ta_species_create(st, this, view.kind, list.length(kept))
  let st = write_values(st, target, kept, 0)
  #(mk_object(target.ta), st)
}

fn filter_collect(
  st: Agent,
  view: TaView,
  k: Int,
  cb: JsVal,
  this_arg: JsVal,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case k >= view.len {
    True -> #(acc, st)
    False -> {
      let el = ta_get(st, view, k)
      let #(res, st) =
        call_cb(st, cb, this_arg, [el, mk_number(JInt(k)), mk_object(view.ta)])
      let acc = case rt_val.to_boolean(res) {
        True -> [el, ..acc]
        False -> acc
      }
      filter_collect(st, view, k + 1, cb, this_arg, acc)
    }
  }
}

fn write_values(
  st: Agent,
  target: TaView,
  values: List(JsVal),
  k: Int,
) -> Agent {
  case values {
    [] -> st
    [v, ..rest] -> write_values(ta_write(st, target, k, v), target, rest, k + 1)
  }
}

/// §23.2.3.23/.24 reduce / reduceRight.
fn proto_reduce(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  dir: Direction,
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let cb = helpers.first_arg_or_undefined(args)
  let #(is_call, st) = rt_val.t_is_callable(st, cb)
  case is_call {
    False -> rt_val.t_throw_type_error(st, "callback is not a function")
    True -> Nil
  }
  let start = direction_start(dir, view.len)
  case helpers.list_at(args, 1) {
    Some(init) -> reduce_loop(st, view, start, dir, cb, init)
    None ->
      case view.len == 0 {
        True ->
          rt_val.t_throw_type_error(
            st,
            "Reduce of empty array with no initial value",
          )
        False -> {
          let acc = ta_get(st, view, start)
          reduce_loop(st, view, start + direction_step(dir), dir, cb, acc)
        }
      }
  }
}

fn reduce_loop(
  st: Agent,
  view: TaView,
  k: Int,
  dir: Direction,
  cb: JsVal,
  acc: JsVal,
) -> #(JsVal, Agent) {
  case k < 0 || k >= view.len {
    True -> #(acc, st)
    False -> {
      let el = ta_get(st, view, k)
      let #(res, st) =
        call_cb(st, cb, mk_undefined(), [
          acc,
          el,
          mk_number(JInt(k)),
          mk_object(view.ta),
        ])
      reduce_loop(st, view, k + direction_step(dir), dir, cb, res)
    }
  }
}

/// §23.2.3.5 copyWithin ( target, start [ , end ] ).
fn proto_copy_within(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(t, st) = to_int_or_inf(st, helpers.first_arg_or_undefined(args))
  let #(s, st) = to_int_or_inf(st, helpers.arg_at(args, 1))
  let #(e, st) = case classify(helpers.arg_at(args, 2)) {
    KUndef -> #(IPosInf, st)
    _ -> to_int_or_inf(st, helpers.arg_at(args, 2))
  }
  let to = relative_index(t, view.len)
  let from = relative_index(s, view.len)
  let final = relative_index(e, view.len)
  let count = int.min(final - from, view.len - to)
  case count <= 0 {
    True -> #(this, st)
    False ->
      case buffer_bytes(st, view.buffer) {
        None ->
          rt_val.t_throw_type_error(
            st,
            "Cannot perform copyWithin on a detached ArrayBuffer",
          )
        Some(data) -> {
          let size = typed_array_elem_size(view.kind)
          let assert Ok(region) =
            bit_array.slice(data, view.off + from * size, count * size)
          let st =
            write_buffer(
              st,
              view.buffer,
              splice(data, view.off + to * size, region),
            )
          #(this, st)
        }
      }
  }
}

/// Splice `region` into `data` at byte `at`.
fn splice(data: BitArray, at: Int, region: BitArray) -> BitArray {
  let n = bit_array.byte_size(region)
  let assert <<head:bytes-size(at), _:bytes-size(n), tail:bits>> = data
  <<head:bits, region:bits, tail:bits>>
}

/// §23.2.3.25 reverse ( ) — in place.
fn proto_reverse(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  case buffer_bytes(st, view.buffer) {
    None -> #(this, st)
    Some(data) -> {
      let size = typed_array_elem_size(view.kind)
      let region = reversed_bytes(data, view.off, view.len, size)
      #(this, write_buffer(st, view.buffer, splice(data, view.off, region)))
    }
  }
}

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
      let assert Ok(elem) = bit_array.slice(data, off + i * size, size)
      reversed_bytes_loop(data, off, len, size, i + 1, [elem, ..acc])
    }
  }
}

/// §23.2.3.32 toReversed ( ).
fn proto_to_reversed(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(fresh, st) = ta_same_type_create(st, view.kind, view.len)
  case buffer_bytes(st, view.buffer) {
    None -> #(mk_object(fresh.ta), st)
    Some(data) -> {
      let size = typed_array_elem_size(view.kind)
      let region = reversed_bytes(data, view.off, view.len, size)
      #(mk_object(fresh.ta), write_buffer(st, fresh.buffer, region))
    }
  }
}

/// §23.2.3.36 with ( index, value ).
fn proto_with(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(rel, st) = to_int_or_inf(st, helpers.first_arg_or_undefined(args))
  let actual = case rel {
    IInt(i) ->
      case i >= 0 {
        True -> i
        False -> view.len + i
      }
    IPosInf -> view.len
    INegInf -> -1
  }
  let #(raw, st) =
    data_view.coerce_elem_value(st, helpers.arg_at(args, 1), view.kind)
  case actual < 0 || actual >= view.len {
    True -> rt_val.t_throw_range_error(st, "Invalid typed array index")
    False -> Nil
  }
  let #(fresh, st) = ta_same_type_create(st, view.kind, view.len)
  let size = typed_array_elem_size(view.kind)
  case buffer_bytes(st, view.buffer) {
    None -> #(mk_object(fresh.ta), st)
    Some(data) -> {
      let assert Ok(region) = bit_array.slice(data, view.off, view.len * size)
      let st = write_buffer(st, fresh.buffer, region)
      let st = ta_write_raw(st, fresh, actual, raw)
      #(mk_object(fresh.ta), st)
    }
  }
}

/// §23.2.3.30 subarray — view over the SAME buffer.
fn proto_subarray(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  // RequireInternalSlot only — detached does NOT throw here.
  let #(buffer, off, len, kind) = require_ta(st, this)
  let #(b, st) = to_int_or_inf(st, helpers.first_arg_or_undefined(args))
  let #(e, st) = case classify(helpers.arg_at(args, 1)) {
    KUndef -> #(IInt(len), st)
    _ -> to_int_or_inf(st, helpers.arg_at(args, 1))
  }
  let begin = relative_index(b, len)
  let end = relative_index(e, len)
  let new_len = int.max(end - begin, 0)
  let size = typed_array_elem_size(kind)
  let new_off = off + begin * size
  let #(proto, st) = species_proto(st, this, kind)
  let #(h, st) = alloc_view(st, kind, proto, buffer, new_off, new_len)
  #(mk_object(h), st)
}

/// §23.2.3.27 slice — copies into a FRESH buffer.
fn proto_slice(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  let #(s, st) = to_int_or_inf(st, helpers.first_arg_or_undefined(args))
  let #(e, st) = case classify(helpers.arg_at(args, 1)) {
    KUndef -> #(IPosInf, st)
    _ -> to_int_or_inf(st, helpers.arg_at(args, 1))
  }
  let start = relative_index(s, view.len)
  let end = relative_index(e, view.len)
  let count = int.max(end - start, 0)
  let #(target, st) = ta_species_create(st, this, view.kind, count)
  case count == 0 {
    True -> #(mk_object(target.ta), st)
    False ->
      case buffer_bytes(st, view.buffer) {
        None ->
          rt_val.t_throw_type_error(
            st,
            "Cannot perform slice on a detached ArrayBuffer",
          )
        Some(data) ->
          case target.kind == view.kind {
            True -> {
              let size = typed_array_elem_size(view.kind)
              let assert Ok(region) =
                bit_array.slice(data, view.off + start * size, count * size)
              case buffer_bytes(st, target.buffer) {
                None -> #(mk_object(target.ta), st)
                Some(tdata) -> #(
                  mk_object(target.ta),
                  write_buffer(
                    st,
                    target.buffer,
                    splice(tdata, target.off, region),
                  ),
                )
              }
            }
            False -> {
              let elems =
                list.reverse(join_collect(st, view, start, []))
                |> list.take(count)
              #(mk_object(target.ta), write_values(st, target, elems, 0))
            }
          }
      }
  }
}

/// §23.2.3.26 set ( source [ , offset ] ).
fn proto_set(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(_, _, _, _) = require_ta(st, this)
  let src = helpers.first_arg_or_undefined(args)
  let #(off_i, st) = to_int_or_inf(st, helpers.arg_at(args, 1))
  let offset = case off_i {
    IInt(n) -> n
    IPosInf -> 9_007_199_254_740_991
    INegInf -> -1
  }
  case offset < 0 {
    True -> rt_val.t_throw_range_error(st, "offset is out of bounds")
    False -> Nil
  }
  let view = validate_ta(st, this)
  case classify(src) {
    KHandle(src_h) ->
      case rt_store.t_cell_get(st, src_h) {
        SObject(
          kind: TypedArrayObj(buffer: sb, offset: so, len: sl, kind: sk),
          ..,
        ) -> set_from_typed_array(st, view, offset, sb, sk, so, sl)
        _ -> set_from_array_like(st, view, offset, src)
      }
    KUndef | rt_types.KNull ->
      rt_val.t_throw_type_error(
        st,
        "Cannot convert undefined or null to object",
      )
    _ -> {
      let #(src_h, st) = rt_val.t_to_object(st, src)
      set_from_array_like(st, view, offset, mk_object(src_h))
    }
  }
}

fn set_from_typed_array(
  st: Agent,
  view: TaView,
  offset: Int,
  src_buf: Handle,
  src_kind: TypedArrayKind,
  src_off: Int,
  src_len: Int,
) -> #(JsVal, Agent) {
  case buffer_bytes(st, src_buf) {
    None ->
      rt_val.t_throw_type_error(
        st,
        "Cannot perform set from a detached ArrayBuffer",
      )
    Some(src_data) -> {
      case same_content_type(view.kind, src_kind) {
        False ->
          rt_val.t_throw_type_error(st, "Cannot mix BigInt and other types")
        True -> Nil
      }
      case src_len + offset > view.len {
        True -> rt_val.t_throw_range_error(st, "offset is out of bounds")
        False -> Nil
      }
      case view.kind == src_kind {
        True ->
          case buffer_bytes(st, view.buffer) {
            None -> #(mk_undefined(), st)
            Some(data) -> {
              let size = typed_array_elem_size(view.kind)
              let assert Ok(region) =
                bit_array.slice(src_data, src_off, src_len * size)
              #(
                mk_undefined(),
                write_buffer(
                  st,
                  view.buffer,
                  splice(data, view.off + offset * size, region),
                ),
              )
            }
          }
        False -> {
          let src_view =
            TaView(
              ta: view.ta,
              buffer: src_buf,
              kind: src_kind,
              off: src_off,
              len: src_len,
            )
          let st = set_convert_loop(st, view, offset, src_view, 0, src_len)
          #(mk_undefined(), st)
        }
      }
    }
  }
}

fn set_convert_loop(
  st: Agent,
  dst: TaView,
  offset: Int,
  src: TaView,
  k: Int,
  src_len: Int,
) -> Agent {
  case k >= src_len {
    True -> st
    False -> {
      let v = ta_get(st, src, k)
      set_convert_loop(
        ta_write(st, dst, offset + k, v),
        dst,
        offset,
        src,
        k + 1,
        src_len,
      )
    }
  }
}

fn set_from_array_like(
  st: Agent,
  view: TaView,
  offset: Int,
  src: JsVal,
) -> #(JsVal, Agent) {
  let #(len_v, st) = rt_obj.t_get_prop(st, src, StringKey(Named("length")))
  let #(src_len, st) = rt_val.t_to_length(st, len_v)
  case src_len + offset > view.len {
    True -> rt_val.t_throw_range_error(st, "offset is out of bounds")
    False -> Nil
  }
  let st = set_array_like_loop(st, view, offset, src, 0, src_len)
  #(mk_undefined(), st)
}

fn set_array_like_loop(
  st: Agent,
  view: TaView,
  offset: Int,
  src: JsVal,
  k: Int,
  src_len: Int,
) -> Agent {
  case k >= src_len {
    True -> st
    False -> {
      let #(v, st) = rt_obj.t_get_prop(st, src, StringKey(Index(k)))
      set_array_like_loop(
        ta_write(st, view, offset + k, v),
        view,
        offset,
        src,
        k + 1,
        src_len,
      )
    }
  }
}

/// §23.2.3.29 sort ( comparefn ) / §23.2.3.33 toSorted.
fn proto_sort(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  fresh: Bool,
) -> #(JsVal, Agent) {
  let cmp = helpers.first_arg_or_undefined(args)
  let #(is_call, st) = rt_val.t_is_callable(st, cmp)
  case classify(cmp) != KUndef && !is_call {
    True ->
      rt_val.t_throw_type_error(
        st,
        "The comparison function must be either a function or undefined",
      )
    False -> Nil
  }
  let view = validate_ta(st, this)
  let items = list.reverse(join_collect(st, view, 0, []))
  let #(sorted, st) = sort_values(st, items, cmp)
  let #(target, st) = case fresh {
    True -> ta_same_type_create(st, view.kind, view.len)
    False -> #(view, st)
  }
  let st = write_values(st, target, sorted, 0)
  case fresh {
    True -> #(mk_object(target.ta), st)
    False -> #(this, st)
  }
}

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
      let #(c, st) = compare_elems(st, cmp, x, y)
      case c <= 0 {
        True -> merge_values(st, xs, right, cmp, [x, ..acc])
        False -> merge_values(st, left, ys, cmp, [y, ..acc])
      }
    }
  }
}

/// §23.2.4.7 CompareTypedArrayElements.
fn compare_elems(st: Agent, cmp: JsVal, x: JsVal, y: JsVal) -> #(Int, Agent) {
  case classify(cmp) {
    KUndef -> #(default_ta_compare(x, y), st)
    _ -> {
      let #(res, st) = call_cb(st, cmp, mk_undefined(), [x, y])
      let #(n, st) = rt_val.t_to_number(st, res)
      let c = case n {
        rt_types.JNan -> 0
        JInt(i) ->
          case i < 0, i > 0 {
            True, _ -> -1
            _, True -> 1
            _, _ -> 0
          }
        rt_types.JFloat(f) ->
          case f <. 0.0, f >. 0.0 {
            True, _ -> -1
            _, True -> 1
            _, _ -> 0
          }
        rt_types.JPosInf -> 1
        rt_types.JNegInf -> -1
      }
      #(c, st)
    }
  }
}

fn default_ta_compare(x: JsVal, y: JsVal) -> Int {
  case classify(x), classify(y) {
    rt_types.KNum(a), rt_types.KNum(b) -> compare_numbers(a, b)
    rt_types.KBig(a), rt_types.KBig(b) ->
      case a < b, a > b {
        True, _ -> -1
        _, True -> 1
        _, _ -> 0
      }
    _, _ -> 0
  }
}

fn compare_numbers(a: rt_types.JsNum, b: rt_types.JsNum) -> Int {
  case a, b {
    rt_types.JNan, rt_types.JNan -> 0
    rt_types.JNan, _ -> 1
    _, rt_types.JNan -> -1
    _, _ -> {
      let fa = jsnum_as_cmp_float(a)
      let fb = jsnum_as_cmp_float(b)
      case fa <. fb, fa >. fb {
        True, _ -> -1
        _, True -> 1
        _, _ -> 0
      }
    }
  }
}

fn jsnum_as_cmp_float(n: rt_types.JsNum) -> Float {
  case n {
    JInt(i) -> int.to_float(i)
    rt_types.JFloat(f) -> f
    rt_types.JPosInf -> 1.0e308
    rt_types.JNegInf -> -1.0e308
    rt_types.JNan -> 0.0
  }
}

/// §23.2.3.31 toLocaleString.
fn proto_to_locale_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let view = validate_ta(st, this)
  locale_loop(st, view, 0, [])
}

fn locale_loop(
  st: Agent,
  view: TaView,
  k: Int,
  acc: List(String),
) -> #(JsVal, Agent) {
  case k >= view.len {
    True -> #(mk_string(join_sep(list.reverse(acc), ",", True, "")), st)
    False -> {
      let el = ta_get(st, view, k)
      let #(m, st) =
        rt_obj.t_get_prop(st, el, StringKey(Named("toLocaleString")))
      let #(r, st) = call_cb(st, m, el, [])
      let #(s, st) = rt_val.t_to_string(st, r)
      locale_loop(st, view, k + 1, [s, ..acc])
    }
  }
}

fn join_sep(
  parts: List(String),
  sep: String,
  first: Bool,
  acc: String,
) -> String {
  case parts {
    [] -> acc
    [p, ..rest] ->
      case first {
        True -> join_sep(rest, sep, False, p)
        False -> join_sep(rest, sep, False, acc <> sep <> p)
      }
  }
}

// ── §23.2.2.1/.2 %TypedArray%.from / .of ────────────────────────────────────

fn ta_from(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let this_kind = require_ta_constructor(st, this, "from")
  let src = helpers.first_arg_or_undefined(args)
  let map_fn = helpers.arg_at(args, 1)
  let this_arg = helpers.arg_at(args, 2)
  let #(has_map, st) = case classify(map_fn) {
    KUndef -> #(False, st)
    _ -> {
      let #(ic, st) = rt_val.t_is_callable(st, map_fn)
      case ic {
        True -> #(True, st)
        False -> rt_val.t_throw_type_error(st, "mapfn is not a function")
      }
    }
  }
  let #(src_h, st) = rt_val.t_to_object(st, src)
  let #(len_v, st) =
    rt_obj.t_get_prop(st, mk_object(src_h), StringKey(Named("length")))
  let #(src_len, st) = rt_val.t_to_length(st, len_v)
  let #(target_h, st) =
    rt_call.t_construct(st, this, [mk_number(JInt(src_len))], this)
  let target = validate_ta(st, mk_object(target_h))
  let _ = this_kind
  let st =
    from_loop(
      st,
      target,
      mk_object(src_h),
      0,
      src_len,
      has_map,
      map_fn,
      this_arg,
    )
  #(mk_object(target_h), st)
}

fn from_loop(
  st: Agent,
  target: TaView,
  src: JsVal,
  k: Int,
  src_len: Int,
  has_map: Bool,
  map_fn: JsVal,
  this_arg: JsVal,
) -> Agent {
  case k >= src_len {
    True -> st
    False -> {
      let #(v, st) = rt_obj.t_get_prop(st, src, StringKey(Index(k)))
      let #(v, st) = case has_map {
        True -> call_cb(st, map_fn, this_arg, [v, mk_number(JInt(k))])
        False -> #(v, st)
      }
      from_loop(
        ta_write(st, target, k, v),
        target,
        src,
        k + 1,
        src_len,
        has_map,
        map_fn,
        this_arg,
      )
    }
  }
}

fn ta_of(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let _ = require_ta_constructor(st, this, "of")
  let len = list.length(args)
  let #(target_h, st) =
    rt_call.t_construct(st, this, [mk_number(JInt(len))], this)
  let target = validate_ta(st, mk_object(target_h))
  let st = write_values(st, target, args, 0)
  #(mk_object(target_h), st)
}

fn require_ta_constructor(
  st: Agent,
  this: JsVal,
  op: String,
) -> TypedArrayKind {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(
          kind: rt_types.KNative(
            tag: TypedArrayN(TypedArrayConstructor(kind:, ..)),
            ..,
          ),
          ..,
        ) -> kind
        _ ->
          rt_val.t_throw_type_error(
            st,
            "%TypedArray%."
              <> op
              <> " requires a TypedArray constructor as this",
          )
      }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "%TypedArray%." <> op <> " requires a TypedArray constructor as this",
      )
  }
}

// ── species / same-type creation ────────────────────────────────────────────

fn same_content_type(a: TypedArrayKind, b: TypedArrayKind) -> Bool {
  is_bigint_kind(a) == is_bigint_kind(b)
}

fn is_bigint_kind(k: TypedArrayKind) -> Bool {
  case k {
    BigInt64 | BigUint64 -> True
    _ -> False
  }
}

/// §23.2.4.3 TypedArrayCreateSameType — fresh array of receiver's own kind.
fn ta_same_type_create(
  st: Agent,
  kind: TypedArrayKind,
  len: Int,
) -> #(TaView, Agent) {
  let proto = default_proto_for(st, kind)
  let #(h, st) = alloc_with_length(st, kind, proto, len)
  let assert Some(v) = reread_view(st, h)
  #(v, st)
}

/// §23.2.4.1 TypedArraySpeciesCreate(O, «len»).
fn ta_species_create(
  st: Agent,
  exemplar: JsVal,
  kind: TypedArrayKind,
  len: Int,
) -> #(TaView, Agent) {
  let #(ctor_v, st) =
    rt_obj.t_get_prop(st, exemplar, StringKey(Named("constructor")))
  case classify(ctor_v) {
    KUndef -> ta_same_type_create(st, kind, len)
    KHandle(_) -> {
      let #(species, st) =
        rt_obj.t_get_prop(
          st,
          ctor_v,
          rt_types.SymbolKey(rt_types.symbol_species),
        )
      case classify(species) {
        KUndef | rt_types.KNull -> ta_same_type_create(st, kind, len)
        KHandle(_) -> {
          let #(h, st) =
            rt_call.t_construct(st, species, [mk_number(JInt(len))], species)
          case reread_view(st, h) {
            Some(tv) ->
              case same_content_type(tv.kind, kind) {
                True -> #(tv, st)
                False ->
                  rt_val.t_throw_type_error(
                    st,
                    "Content types of source and created typed arrays differ",
                  )
              }
            None ->
              rt_val.t_throw_type_error(
                st,
                "Species constructor did not return a TypedArray",
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

fn species_proto(
  st: Agent,
  exemplar: JsVal,
  kind: TypedArrayKind,
) -> #(Handle, Agent) {
  let default = default_proto_for(st, kind)
  let #(ctor_v, st) =
    rt_obj.t_get_prop(st, exemplar, StringKey(Named("constructor")))
  case classify(ctor_v) {
    KHandle(_) -> {
      let #(species, st) =
        rt_obj.t_get_prop(
          st,
          ctor_v,
          rt_types.SymbolKey(rt_types.symbol_species),
        )
      case classify(species) {
        KHandle(_) -> proto_from_new_target(st, species, default)
        _ -> #(default, st)
      }
    }
    _ -> #(default, st)
  }
}

fn default_proto_for(st: Agent, kind: TypedArrayKind) -> Handle {
  let assert Ok(bt) = dict.get(st.realm.typed_arrays.by_kind, kind)
  bt.prototype
}
