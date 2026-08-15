//// ES2024 §25.1 ArrayBuffer Objects + §25.2 SharedArrayBuffer Objects
////
//// Both share one exotic kind — `ArrayBufferObj` — whose whole state is a
//// `types.BufferStorage` sum type:
////   Bytes     — a plain ArrayBuffer (an immutable BEAM binary),
////   Shared    — a SharedArrayBuffer (bytes in this agent's store until
////               the buffer is shared with another agent, then held by
////               its owner process — `types.SharedBlock`),
////   Immutable — an immutable ArrayBuffer (immutable-arraybuffer proposal),
////   Detached  — [[ArrayBufferData]] = null.
//// Detached-ness, shared-ness and immutability are variants, not flags, so a
//// detached buffer literally has no bytes to read and combinations the spec
//// forbids (immutable+shared/resizable/detached, shared+detached) cannot be
//// written down. [[ArrayBufferByteLength]] is derived via
//// `types.buffer_byte_size`; resizable/growable buffers carry
//// `max_byte_length: Some(n)`.
////
//// Spec algorithms follow tc39.es/ecma262 §25.1.3 (abstract operations) and
//// were cross-checked against engine262's ArrayBuffer intrinsics.

import arc/rt/buffer
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/sab
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type ArrayBufferNative, type BufferStorage, type BuiltinPair,
  type Handle, type JsVal, ArrayBufferConstructor, ArrayBufferDetach262,
  ArrayBufferGetByteLength, ArrayBufferGetDetached, ArrayBufferGetImmutable,
  ArrayBufferGetMaxByteLength, ArrayBufferGetResizable, ArrayBufferIsView,
  ArrayBufferN, ArrayBufferObj, ArrayBufferResize, ArrayBufferSlice,
  ArrayBufferSliceToImmutable, ArrayBufferTransfer,
  ArrayBufferTransferToFixedLength, ArrayBufferTransferToImmutable, Bytes,
  DataViewObj, Detached, Immutable, JInt, KHandle, KUndef, LocalBlock, Named,
  OwnerBlock, ReturnThis, SObject, Shared, SharedArrayBufferConstructor, SharedArrayBufferGetByteLength,
  SharedArrayBufferGetGrowable, SharedArrayBufferGetMaxByteLength,
  SharedArrayBufferGrow, SharedArrayBufferSlice, StringKey, TypedArrayObj,
  classify, mk_bool, mk_number, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

/// Practical cap on a single allocation (§6.2.9.2 CreateByteDataBlock:
/// "If it is impossible to create such a Data Block, throw a RangeError").
/// 2^31 - 1 bytes — matches V8's ~2 GiB limit on 64-bit.
const max_buffer_byte_length = 2_147_483_647

/// RangeError message for every §7.1.22 ToIndex in this module (constructor
/// length, maxByteLength option, resize, grow, transfer).
const invalid_length_msg = "Invalid array buffer length"

// ============================================================================
// Init — constructors + prototypes for ArrayBuffer and SharedArrayBuffer
// ============================================================================

/// Set up ArrayBuffer + SharedArrayBuffer constructors and prototypes.
/// Returns `#(#(ArrayBuffer, SharedArrayBuffer), st)`.
pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(#(BuiltinPair, BuiltinPair), Agent) {
  // --- ArrayBuffer ---
  let #(ab_methods, st) =
    common.alloc_methods(st, function_proto, [
      #("resize", ArrayBufferN(ArrayBufferResize), 1),
      #("slice", ArrayBufferN(ArrayBufferSlice), 2),
      #("sliceToImmutable", ArrayBufferN(ArrayBufferSliceToImmutable), 2),
      #("transfer", ArrayBufferN(ArrayBufferTransfer), 0),
      #(
        "transferToFixedLength",
        ArrayBufferN(ArrayBufferTransferToFixedLength),
        0,
      ),
      #("transferToImmutable", ArrayBufferN(ArrayBufferTransferToImmutable), 0),
    ])
  let #(ab_getters, st) =
    common.alloc_getters(st, function_proto, [
      #("byteLength", ArrayBufferN(ArrayBufferGetByteLength)),
      #("detached", ArrayBufferN(ArrayBufferGetDetached)),
      #("immutable", ArrayBufferN(ArrayBufferGetImmutable)),
      #("maxByteLength", ArrayBufferN(ArrayBufferGetMaxByteLength)),
      #("resizable", ArrayBufferN(ArrayBufferGetResizable)),
    ])
  let #(ab_statics, st) =
    common.alloc_methods(st, function_proto, [
      #("isView", ArrayBufferN(ArrayBufferIsView), 1),
    ])
  let #(ab_type, st) =
    common.init_type(
      st,
      object_proto,
      function_proto,
      list.append(ab_getters, ab_methods),
      fn(proto) { ArrayBufferN(ArrayBufferConstructor(proto:)) },
      "ArrayBuffer",
      1,
      ab_statics,
    )
  let st = common.add_to_string_tag(st, ab_type.prototype, "ArrayBuffer")
  let st =
    common.add_species_accessor(
      st,
      function_proto,
      ab_type.constructor,
      ReturnThis,
    )

  // --- SharedArrayBuffer ---
  let #(sab_methods, st) =
    common.alloc_methods(st, function_proto, [
      #("grow", ArrayBufferN(SharedArrayBufferGrow), 1),
      #("slice", ArrayBufferN(SharedArrayBufferSlice), 2),
    ])
  let #(sab_getters, st) =
    common.alloc_getters(st, function_proto, [
      #("byteLength", ArrayBufferN(SharedArrayBufferGetByteLength)),
      #("growable", ArrayBufferN(SharedArrayBufferGetGrowable)),
      #("maxByteLength", ArrayBufferN(SharedArrayBufferGetMaxByteLength)),
    ])
  let #(sab_type, st) =
    common.init_type(
      st,
      object_proto,
      function_proto,
      list.append(sab_getters, sab_methods),
      fn(proto) { ArrayBufferN(SharedArrayBufferConstructor(proto:)) },
      "SharedArrayBuffer",
      1,
      [],
    )
  let st = common.add_to_string_tag(st, sab_type.prototype, "SharedArrayBuffer")
  let st =
    common.add_species_accessor(
      st,
      function_proto,
      sab_type.constructor,
      ReturnThis,
    )

  // §25.1.5.2/§25.2.4.1: the constructors' "prototype" property is
  // { writable: false, enumerable: false, configurable: false } — installed
  // that way by common.init_type.
  #(#(ab_type, sab_type), st)
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module [[Call]] dispatch. Both constructors throw without `new`
/// (§25.1.4.1 / §25.2.3.1 step 1).
pub fn dispatch(
  st: Agent,
  native: ArrayBufferNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    ArrayBufferConstructor(..) ->
      rt_val.t_throw_type_error(st, "Constructor ArrayBuffer requires 'new'")
    SharedArrayBufferConstructor(..) ->
      rt_val.t_throw_type_error(
        st,
        "Constructor SharedArrayBuffer requires 'new'",
      )
    ArrayBufferIsView -> is_view(st, args)
    ArrayBufferGetByteLength -> ab_get_byte_length(st, this)
    ArrayBufferGetDetached -> ab_get_detached(st, this)
    ArrayBufferGetImmutable -> ab_get_immutable(st, this)
    ArrayBufferGetMaxByteLength -> ab_get_max_byte_length(st, this)
    ArrayBufferGetResizable -> ab_get_resizable(st, this)
    ArrayBufferResize -> ab_resize(st, this, args)
    ArrayBufferSlice -> buffer_slice(st, this, args, shared: False)
    ArrayBufferSliceToImmutable -> slice_to_immutable(st, this, args)
    ArrayBufferTransfer -> ab_transfer(st, this, args, PreserveResizability)
    ArrayBufferTransferToFixedLength ->
      ab_transfer(st, this, args, ToFixedLength)
    ArrayBufferTransferToImmutable -> ab_transfer(st, this, args, ToImmutable)
    SharedArrayBufferGetByteLength -> sab_get_byte_length(st, this)
    SharedArrayBufferGetGrowable -> sab_get_growable(st, this)
    SharedArrayBufferGetMaxByteLength -> sab_get_max_byte_length(st, this)
    SharedArrayBufferGrow -> sab_grow(st, this, args)
    SharedArrayBufferSlice -> buffer_slice(st, this, args, shared: True)
    ArrayBufferDetach262 -> detach_262(st, args)
  }
}

/// test262 `$262.detachArrayBuffer(buffer)`: §25.1.3.5 DetachArrayBuffer on
/// an unshared buffer. Immutable buffers can never be detached (Immutable
/// ArrayBuffer proposal), so that is a TypeError like a shared one.
fn detach_262(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let method = "detachArrayBuffer"
  let buf = require_buffer(st, helpers.first_arg_or_undefined(args), method)
  let buf = require_unshared(st, buf, method)
  let buf = require_not_immutable(st, buf, method)
  #(mk_undefined(), detach(st, buf))
}

/// Per-module [[Construct]] dispatch.
pub fn dispatch_construct(
  st: Agent,
  native: ArrayBufferNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    ArrayBufferConstructor(proto:) ->
      constructor(st, proto, args, new_target, shared: False)
    SharedArrayBufferConstructor(proto:) ->
      constructor(st, proto, args, new_target, shared: True)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// ============================================================================
// §25.1.4.1 ArrayBuffer ( length [ , options ] )
// §25.2.3.1 SharedArrayBuffer ( length [ , options ] )
// ============================================================================

/// Steps:
///   1. If NewTarget is undefined, throw a TypeError exception (the [[Call]]
///      dispatch arm above).
///   2. Let byteLength be ? ToIndex(length).
///   3. Let requestedMaxByteLength be ? GetArrayBufferMaxByteLengthOption(options).
///   4. Return ? AllocateArrayBuffer(NewTarget, byteLength, requestedMaxByteLength).
fn constructor(
  st: Agent,
  proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
  shared shared: Bool,
) -> #(Handle, Agent) {
  // Step 2: ToIndex(length)
  let #(byte_length, st) =
    rt_val.t_to_index(
      st,
      helpers.first_arg_or_undefined(args),
      invalid_length_msg,
    )
  // Step 3: GetArrayBufferMaxByteLengthOption(options)
  let #(max, st) = max_byte_length_option(st, helpers.arg_at(args, 1))
  // Step 4: AllocateArrayBuffer / AllocateSharedArrayBuffer
  allocate(st, new_target, proto, byte_length, max, shared)
}

/// §25.1.3.1 AllocateArrayBuffer ( constructor, byteLength [ , maxByteLength ] )
/// (also §25.2.2.1 AllocateSharedArrayBuffer — same shape for our model)
///
///   3a. If byteLength > maxByteLength, throw a RangeError exception
///       (BEFORE OrdinaryCreateFromConstructor — test262
///       options-maxbytelength-compared-before-object-creation.js).
///   4.  OrdinaryCreateFromConstructor — reads NewTarget.prototype, which may
///       run a getter (test262 data-allocation-after-object-creation.js).
///   5.  CreateByteDataBlock(byteLength) — RangeError if impossible.
fn allocate(
  st: Agent,
  new_target: JsVal,
  intrinsic_proto: Handle,
  byte_length: Int,
  max: Option(Int),
  shared: Bool,
) -> #(Handle, Agent) {
  // Step 3a
  case max {
    Some(m) if byte_length > m ->
      rt_val.t_throw_range_error(
        st,
        ctor_name(shared) <> " length exceeds maxByteLength option",
      )
    _ -> {
      // Step 4: GetPrototypeFromConstructor(NewTarget, intrinsic) — must use
      // a real [[Get]] so accessor `prototype` properties are invoked.
      let #(proto, st) = proto_from_new_target(st, new_target, intrinsic_proto)
      // Step 5 (+ step 6a for resizable): CreateByteDataBlock limits
      let max_ok = case max {
        Some(m) -> m <= max_buffer_byte_length
        None -> True
      }
      case byte_length <= max_buffer_byte_length && max_ok {
        False ->
          rt_val.t_throw_range_error(st, "Array buffer allocation failed")
        True -> {
          // §6.2.9.2 CreateByteDataBlock / CreateSharedByteDataBlock: both
          // start as an immutable BEAM binary in this agent's store; a
          // shared block moves to an owner process only once it is shared.
          let storage = case shared {
            False -> Bytes(bytes: zero_block(byte_length), max_byte_length: max)
            True ->
              Shared(
                block: LocalBlock(bytes: zero_block(byte_length)),
                max_byte_length: max,
              )
          }
          realm_ops.alloc_wrapper(st, ArrayBufferObj(storage:), proto)
        }
      }
    }
  }
}

/// §25.1.3.1 AllocateArrayBuffer(%ArrayBuffer%, byteLength) with the
/// intrinsic prototype — a fresh zero-filled fixed-length buffer. The
/// TypedArray allocators call this.
pub fn alloc_buffer(
  st: Agent,
  proto: Handle,
  byte_len: Int,
) -> #(Handle, Agent) {
  realm_ops.alloc_wrapper(
    st,
    ArrayBufferObj(storage: Bytes(
      bytes: zero_block(byte_len),
      max_byte_length: None,
    )),
    proto,
  )
}

/// §25.1.3.7 GetArrayBufferMaxByteLengthOption ( options )
///
///   1. If options is not an Object, return empty.
///   2. Let maxByteLength be ? Get(options, "maxByteLength").
///   3. If maxByteLength is undefined, return empty.
///   4. Return ? ToIndex(maxByteLength).
fn max_byte_length_option(st: Agent, options: JsVal) -> #(Option(Int), Agent) {
  case classify(options) {
    KHandle(_) -> {
      let #(max_val, st) =
        rt_obj.t_get_prop(st, options, StringKey(Named("maxByteLength")))
      case classify(max_val) {
        KUndef -> #(None, st)
        _ -> {
          let #(max, st) = rt_val.t_to_index(st, max_val, invalid_length_msg)
          #(Some(max), st)
        }
      }
    }
    _ -> #(None, st)
  }
}

// ============================================================================
// §25.1.5.1 ArrayBuffer.isView ( arg )
// ============================================================================

/// Returns true iff arg has a [[ViewedArrayBuffer]] internal slot
/// (TypedArray or DataView instances).
fn is_view(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let result = case classify(helpers.first_arg_or_undefined(args)) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: DataViewObj(..), ..)
        | SObject(kind: TypedArrayObj(..), ..) -> True
        _ -> False
      }
    _ -> False
  }
  #(mk_bool(result), st)
}

// ============================================================================
// ArrayBuffer.prototype getters — §25.1.6.2–25.1.6.5
// ============================================================================

/// §25.1.6.2 get ArrayBuffer.prototype.byteLength
///   shared → TypeError; detached → +0; else [[ArrayBufferByteLength]].
fn ab_get_byte_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "byteLength")
  let buf = require_unshared(st, buf, "byteLength")
  #(mk_number(JInt(live_byte_size(buf))), st)
}

/// §25.1.6.3 get ArrayBuffer.prototype.detached
fn ab_get_detached(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "detached")
  let buf = require_unshared(st, buf, "detached")
  #(mk_bool(types.buffer_is_detached(buf.storage)), st)
}

/// Immutable ArrayBuffer proposal: get ArrayBuffer.prototype.immutable
///   1. RequireInternalSlot(O, [[ArrayBufferData]]).
///   2. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.
///   3. Return IsImmutableBuffer(O).
fn ab_get_immutable(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "immutable")
  let buf = require_unshared(st, buf, "immutable")
  #(mk_bool(types.buffer_is_immutable(buf.storage)), st)
}

/// §25.1.6.4 get ArrayBuffer.prototype.maxByteLength
///   shared → TypeError; detached → +0; fixed-length → byteLength;
///   else [[ArrayBufferMaxByteLength]].
fn ab_get_max_byte_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "maxByteLength")
  let buf = require_unshared(st, buf, "maxByteLength")
  let result = case buf.storage {
    Detached(..) -> 0
    live ->
      case types.buffer_max_byte_length(live) {
        Some(max) -> max
        None -> types.buffer_byte_size(live)
      }
  }
  #(mk_number(JInt(result)), st)
}

/// §25.1.6.5 get ArrayBuffer.prototype.resizable
fn ab_get_resizable(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "resizable")
  let buf = require_unshared(st, buf, "resizable")
  #(mk_bool(max_byte_length(buf) != None), st)
}

// ============================================================================
// §25.1.6.6 ArrayBuffer.prototype.resize ( newLength )
// ============================================================================

///   1. Perform ? RequireInternalSlot(O, [[ArrayBufferMaxByteLength]]).
///   2. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.
///   3. Let newByteLength be ? ToIndex(newLength).
///   4. If IsDetachedBuffer(O) is true, throw a TypeError exception.
///   5. If newByteLength > O.[[ArrayBufferMaxByteLength]], throw a RangeError.
///   6. Realloc: shrink truncates, grow zero-fills.
fn ab_resize(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "resize")
  // Step 1: fixed-length buffers lack [[ArrayBufferMaxByteLength]] (an
  // immutable buffer never has one, so it lands here, not on a write path)
  case max_byte_length(buf) {
    None ->
      rt_val.t_throw_type_error(
        st,
        "ArrayBuffer.prototype.resize called on a non-resizable ArrayBuffer",
      )
    Some(_) -> {
      // Step 2
      let buf = require_unshared(st, buf, "resize")
      // Step 3: ToIndex may run user code (valueOf) — re-read O after.
      let #(new_len, st) =
        rt_val.t_to_index(
          st,
          helpers.first_arg_or_undefined(args),
          invalid_length_msg,
        )
      let buf = require_buffer(st, mk_object(buf.ref), "resize")
      // Step 4 (the gate hands us the live bytes of a RESIZABLE byte buffer:
      // step 1 already proved the storage is `Bytes(_, Some(_))`, so the only
      // thing user code can have changed underneath us is detaching it)
      let #(bytes, max) = require_resizable_bytes(st, buf, "resize")
      // Step 5
      case new_len > max {
        True ->
          rt_val.t_throw_range_error(
            st,
            "ArrayBuffer.prototype.resize: new length exceeds maxByteLength",
          )
        False -> {
          // Step 6. The storage stays `Bytes` with the SAME max: a resize
          // cannot turn a resizable buffer into a fixed-length one.
          let storage =
            Bytes(
              bytes: resize_data(bytes, new_len),
              max_byte_length: Some(max),
            )
          #(mk_undefined(), buffer.set_storage(st, buf.ref, storage))
        }
      }
    }
  }
}

// ============================================================================
// §25.1.6.7 ArrayBuffer.prototype.slice / §25.2.5.6 SharedArrayBuffer slice
// ============================================================================

/// Shared body for both slice methods — they differ only in which buffer
/// family they accept (and therefore which species default they use).
fn buffer_slice(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  shared shared: Bool,
) -> #(JsVal, Agent) {
  // Steps 2-3: RequireInternalSlot + shared/unshared gate
  let buf = require_buffer(st, this, "slice")
  let buf = require_family(st, buf, "slice", shared)
  // Step 4 (AB only): detached → TypeError
  let storage = require_live(st, buf, "slice")
  // Step 5
  let len = types.buffer_byte_size(storage)
  // Steps 6-7: relativeStart
  let #(first, st) =
    relative_index(st, helpers.first_arg_or_undefined(args), len, 0)
  // Steps 8-9: relativeEnd (undefined → len)
  let #(final, st) = relative_index(st, helpers.arg_at(args, 1), len, len)
  // Step 10
  let new_len = int.max(final - first, 0)
  // Step 11: SpeciesConstructor(O, intrinsic default)
  let default_ctor = case shared {
    True -> st.realm.shared_array_buffer.constructor
    False -> st.realm.array_buffer.constructor
  }
  let #(ctor, st) = species_constructor(st, this, default_ctor)
  // Step 12: Construct(ctor, « 𝔽(newLen) »)
  let #(new_h, st) =
    rt_call.t_construct(st, ctor, [mk_number(JInt(new_len))], ctor)
  let new_val = mk_object(new_h)
  // Steps 13-15: validate the constructed buffer
  let new_buf = require_buffer(st, new_val, "slice")
  let new_buf = require_family(st, new_buf, "slice", shared)
  let new_storage = require_live(st, new_buf, "slice")
  // Immutable ArrayBuffer proposal: a species constructor returning an
  // immutable buffer is a TypeError — slice must write into the result.
  let new_buf = require_not_immutable(st, new_buf, "slice")
  // Step 16: SameValue(new, O) → TypeError
  case new_buf.ref == buf.ref {
    True ->
      rt_val.t_throw_type_error(
        st,
        "species constructor returned the same " <> ctor_name(shared),
      )
    False ->
      // Step 17
      case types.buffer_byte_size(new_storage) < new_len {
        True ->
          rt_val.t_throw_type_error(
            st,
            "species constructor returned a buffer smaller than requested",
          )
        False -> {
          // Steps 18-19: species ctor may have detached O — re-read.
          let buf = require_buffer(st, mk_object(buf.ref), "slice")
          let storage = require_live(st, buf, "slice")
          let current_len = types.buffer_byte_size(storage)
          // Copy min(newLen, currentLen - first) bytes from offset `first`.
          case first < current_len {
            False -> #(new_val, st)
            True -> {
              let bits = require_live_bits(st, buf, "slice")
              let new_bits = require_live_bits(st, new_buf, "slice")
              let count = int.min(new_len, current_len - first)
              let copied = copy_into(bits, first, count, new_bits)
              // §6.2.9.3 CopyDataBlockBytes writes exactly [0, count) of the
              // destination.
              #(new_val, buffer.store_region(st, new_buf.ref, copied, 0, count))
            }
          }
        }
      }
  }
}

// ============================================================================
// Immutable ArrayBuffer proposal: ArrayBuffer.prototype.sliceToImmutable
// ============================================================================

/// ArrayBuffer.prototype.sliceToImmutable ( start, end )
///
///   1. Let O be the this value.
///   2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
///   3. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.
///   4. If IsDetachedBuffer(O) is true, throw a TypeError exception.
///   5. Let len be O.[[ArrayBufferByteLength]].
///   6. Let bounds be ? ResolveBounds(len, start, end)  (may run user code).
///   9. Let newLen be max(final - first, 0).
///  11. If IsDetachedBuffer(O) is true, throw a TypeError exception
///      (the coercions may have detached O).
///  14. If currentLen < final, throw a RangeError exception (O shrank).
///  15. Return AllocateImmutableArrayBuffer(%ArrayBuffer%, newLen, copy).
///
/// No species lookup — the result is always a plain immutable %ArrayBuffer%.
fn slice_to_immutable(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  // Steps 2-4
  let buf = require_buffer(st, this, "sliceToImmutable")
  let bytes = require_unshared_bytes(st, buf, "sliceToImmutable")
  // Step 5
  let len = bit_array.byte_size(bytes)
  // Steps 6-8: ResolveBounds — ToIntegerOrInfinity may run user code.
  let #(first, st) =
    relative_index(st, helpers.first_arg_or_undefined(args), len, 0)
  let #(final, st) = relative_index(st, helpers.arg_at(args, 1), len, len)
  // Step 9
  let new_len = int.max(final - first, 0)
  // Steps 11-14: re-read O — the coercions may have detached or resized it.
  let buf = require_buffer(st, mk_object(buf.ref), "sliceToImmutable")
  let bytes = require_unshared_bytes(st, buf, "sliceToImmutable")
  let current_len = bit_array.byte_size(bytes)
  case current_len < final {
    True ->
      rt_val.t_throw_range_error(
        st,
        "ArrayBuffer.prototype.sliceToImmutable: source was resized below the requested range",
      )
    False -> {
      // Step 15: AllocateImmutableArrayBuffer — copy [first, first+newLen).
      // In bounds by construction: step 14 just proved final <= currentLen,
      // and newLen > 0 implies first < final. (newLen == 0 must not reach
      // bit_array.slice: a mid-coercion shrink can leave `first` past
      // currentLen, and binary:part rejects an out-of-range start even for
      // an empty take.)
      let data = case new_len {
        0 -> <<>>
        _ -> {
          let assert Ok(part) = bit_array.slice(bytes, first, new_len)
          part
        }
      }
      let #(new_h, st) =
        realm_ops.alloc_wrapper(
          st,
          ArrayBufferObj(storage: Immutable(bytes: data)),
          st.realm.array_buffer.prototype,
        )
      #(mk_object(new_h), st)
    }
  }
}

// ============================================================================
// §25.1.6.8/.9 transfer / transferToFixedLength — ArrayBufferCopyAndDetach
// ============================================================================

/// The three ArrayBufferCopyAndDetach flavours §25.1.6.8/.9 (+ the immutable-
/// arraybuffer proposal's transferToImmutable) can be in. Exactly one axis,
/// exactly three states: `preserveResizability` and `immutable` are NOT
/// independent — a resizable immutable buffer does not exist.
type TransferMode {
  /// ArrayBuffer.prototype.transfer — a resizable source stays resizable.
  PreserveResizability
  /// ArrayBuffer.prototype.transferToFixedLength.
  ToFixedLength
  /// ArrayBuffer.prototype.transferToImmutable — fixed length by definition.
  ToImmutable
}

/// §25.1.3.4 ArrayBufferCopyAndDetach ( arrayBuffer, newLength, preserveResizability )
///
///   1. RequireInternalSlot(arrayBuffer, [[ArrayBufferData]]).
///   2. If IsSharedArrayBuffer(arrayBuffer), throw TypeError.
///   3. newByteLength: undefined → current byteLength, else ? ToIndex(newLength).
///   4. If IsDetachedBuffer(arrayBuffer), throw TypeError.
///   5. preserve-resizability + resizable → keep maxByteLength, else fixed.
///   7. newBuffer = AllocateArrayBuffer(%ArrayBuffer%, newByteLength, newMax).
///   8. Copy min(newByteLength, old byteLength) bytes; rest is zero-filled.
///   9. DetachArrayBuffer(arrayBuffer).
fn ab_transfer(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  mode: TransferMode,
) -> #(JsVal, Agent) {
  // Steps 1-2
  let buf = require_buffer(st, this, "transfer")
  let buf = require_unshared(st, buf, "transfer")
  // Step 3
  let len_arg = helpers.first_arg_or_undefined(args)
  let #(new_len, st) = case classify(len_arg) {
    KUndef -> #(live_byte_size(buf), st)
    _ -> rt_val.t_to_index(st, len_arg, invalid_length_msg)
  }
  // ToIndex may run user code — re-read O.
  let buf = require_buffer(st, mk_object(buf.ref), "transfer")
  // Step 4 (the gate hands us the source bytes)
  let old_bits = require_unshared_bytes(st, buf, "transfer")
  // Step 6 (immutable-arraybuffer proposal): IsImmutableBuffer(O) →
  // TypeError. Immutable buffers cannot be detached, so no transfer flavour
  // accepts one.
  let buf = require_not_immutable(st, buf, "transfer")
  // Step 5 (+ the proposal's step 5 for transferToImmutable). An immutable
  // result carries no maxByteLength: `Immutable` has no such field.
  let new_max = case mode {
    PreserveResizability -> max_byte_length(buf)
    ToFixedLength | ToImmutable -> None
  }
  // Step 7 (AllocateArrayBuffer with the intrinsic constructor): 3a + limits
  let max_ok = case new_max {
    Some(m) -> new_len <= m && m <= max_buffer_byte_length
    None -> True
  }
  case new_len <= max_buffer_byte_length && max_ok {
    False -> rt_val.t_throw_range_error(st, "Array buffer allocation failed")
    True -> {
      // Step 8: copy then zero-extend. In bounds by construction:
      // copyLen = min(newByteLength, old byteLength) <= byte_size(old_bits).
      let old_len = bit_array.byte_size(old_bits)
      let copy_len = int.min(new_len, old_len)
      let assert Ok(copied) = bit_array.slice(old_bits, 0, copy_len)
      let data = bit_array.append(copied, zero_block(new_len - copy_len))
      let storage = case mode {
        ToImmutable -> Immutable(bytes: data)
        PreserveResizability | ToFixedLength ->
          Bytes(bytes: data, max_byte_length: new_max)
      }
      let #(new_h, st) =
        realm_ops.alloc_wrapper(
          st,
          ArrayBufferObj(storage:),
          st.realm.array_buffer.prototype,
        )
      // Step 9: DetachArrayBuffer(O) — data → null, byteLength → 0.
      // [[ArrayBufferMaxByteLength]] survives (resizable getter stays true).
      let st = detach(st, buf)
      #(mk_object(new_h), st)
    }
  }
}

// ============================================================================
// SharedArrayBuffer.prototype getters + grow — §25.2.5
// ============================================================================

/// §25.2.5.2 get SharedArrayBuffer.prototype.byteLength
fn sab_get_byte_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "byteLength")
  let _block = require_shared(st, buf, "byteLength")
  #(mk_number(JInt(live_byte_size(buf))), st)
}

/// §25.2.5.4 get SharedArrayBuffer.prototype.growable
fn sab_get_growable(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "growable")
  let _bytes = require_shared(st, buf, "growable")
  #(mk_bool(max_byte_length(buf) != None), st)
}

/// §25.2.5.5 get SharedArrayBuffer.prototype.maxByteLength
fn sab_get_max_byte_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "maxByteLength")
  let _block = require_shared(st, buf, "maxByteLength")
  let max = option.unwrap(max_byte_length(buf), live_byte_size(buf))
  #(mk_number(JInt(max)), st)
}

/// §25.2.5.3 SharedArrayBuffer.prototype.grow ( newLength )
///
///   1. RequireInternalSlot(O, [[ArrayBufferMaxByteLength]]).
///   2. If IsSharedArrayBuffer(O) is false, throw TypeError.
///   3. newByteLength = ? ToIndex(newLength).
///   4. GrowSharedArrayBuffer: newByteLength < currentByteLength or
///      newByteLength > maxByteLength → RangeError. Growth zero-fills.
fn sab_grow(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "grow")
  case max_byte_length(buf) {
    None ->
      rt_val.t_throw_type_error(
        st,
        "SharedArrayBuffer.prototype.grow called on a non-growable SharedArrayBuffer",
      )
    Some(max) -> {
      let _block = require_shared(st, buf, "grow")
      let #(new_len, st) =
        rt_val.t_to_index(
          st,
          helpers.first_arg_or_undefined(args),
          invalid_length_msg,
        )
      // ToIndex may run user code (a nested grow) — re-read the length.
      let buf = require_buffer(st, mk_object(buf.ref), "grow")
      let block = require_shared(st, buf, "grow")
      let invalid = fn() {
        rt_val.t_throw_range_error(
          st,
          "SharedArrayBuffer.prototype.grow: invalid length",
        )
      }
      // The length is monotonic: shrinking is a RangeError, so is exceeding
      // the max the storage was declared with.
      use <- bool.lazy_guard(new_len > max, invalid)
      case block {
        LocalBlock(bytes: bits) -> {
          let current = bit_array.byte_size(bits)
          use <- bool.lazy_guard(new_len < current, invalid)
          let storage =
            Shared(
              block: LocalBlock(
                bytes: bit_array.append(bits, zero_block(new_len - current)),
              ),
              max_byte_length: Some(max),
            )
          #(mk_undefined(), buffer.set_storage(st, buf.ref, storage))
        }
        // Another agent may grow the block between any read of its length
        // and our write, so the compare-and-grow is the owner's one step
        // (§25.2.2.3 GrowSharedArrayBuffer's compare-exchange loop).
        OwnerBlock(owner:, ..) ->
          case sab.grow(owner, new_len) {
            Ok(Nil) -> #(mk_undefined(), st)
            Error(Nil) -> invalid()
          }
      }
    }
  }
}

// ============================================================================
// Helpers
// ============================================================================

/// Internal view of an ArrayBufferObj cell: its handle plus its whole
/// storage state. Shared-ness, detached-ness and immutability are variants of
/// `storage`, not fields, so no combination of them can be out of step.
type Buf {
  Buf(ref: Handle, storage: BufferStorage)
}

fn ctor_name(shared: Bool) -> String {
  case shared {
    True -> "SharedArrayBuffer"
    False -> "ArrayBuffer"
  }
}

/// [[ArrayBufferByteLength]] of a Buf — 0 for a detached buffer, which is
/// what §25.1.6.2/§25.1.3.4 both want.
fn live_byte_size(buf: Buf) -> Int {
  types.buffer_byte_size(buf.storage)
}

/// [[ArrayBufferMaxByteLength]] of a Buf, absent for fixed-length buffers.
fn max_byte_length(buf: Buf) -> Option(Int) {
  types.buffer_max_byte_length(buf.storage)
}

/// §25.1.3.5 DetachArrayBuffer — [[ArrayBufferData]] becomes null. There is
/// no leftover byte array to read: the storage is simply gone.
/// [[ArrayBufferMaxByteLength]] survives (the resizable getter stays true).
fn detach(st: Agent, buf: Buf) -> Agent {
  buffer.set_storage(
    st,
    buf.ref,
    Detached(max_byte_length: max_byte_length(buf)),
  )
}

/// RequireInternalSlot(O, [[ArrayBufferData]]) — `this` must be an
/// ArrayBuffer or SharedArrayBuffer object.
fn require_buffer(st: Agent, this: JsVal, method: String) -> Buf {
  case classify(this) {
    KHandle(ref) ->
      case buffer.buffer_storage(st, ref) {
        Some(storage) -> Buf(ref:, storage:)
        None -> incompatible(st, method)
      }
    _ -> incompatible(st, method)
  }
}

/// IsSharedArrayBuffer(O) must be false, else TypeError. The buffer may still
/// be detached — the getters (byteLength/detached/maxByteLength/resizable)
/// are the ones that need to see that state.
fn require_unshared(st: Agent, buf: Buf, method: String) -> Buf {
  case buf.storage {
    Shared(..) -> incompatible(st, method)
    Bytes(..) | Immutable(..) | Detached(..) -> buf
  }
}

/// IsSharedArrayBuffer(O) must be true, else TypeError. Hands back the shared
/// block — the proof travels with the gate, so no caller has to write a "what
/// if it were byte storage" branch. (`Shared` is never detached.)
fn require_shared(st: Agent, buf: Buf, method: String) -> types.SharedBlock {
  case buf.storage {
    Shared(block:, ..) -> block
    Bytes(..) | Immutable(..) | Detached(..) -> incompatible(st, method)
  }
}

/// Gate on the expected buffer family: shared=True requires a
/// SharedArrayBuffer, shared=False requires a plain ArrayBuffer.
fn require_family(st: Agent, buf: Buf, method: String, shared: Bool) -> Buf {
  case shared {
    True -> {
      let _bytes = require_shared(st, buf, method)
      buf
    }
    False -> require_unshared(st, buf, method)
  }
}

/// IsDetachedBuffer(O) must be false, else TypeError. Hands back the live
/// storage. (Shared buffers are never detached, so this always succeeds for
/// them.)
fn require_live(st: Agent, buf: Buf, method: String) -> BufferStorage {
  case buf.storage {
    Detached(..) -> detached_error(st, method)
    live -> live
  }
}

/// `require_live`, but hands back the live bytes — a detached buffer has
/// none.
fn require_live_bits(st: Agent, buf: Buf, method: String) -> BitArray {
  case types.buffer_bits(buf.storage) {
    Some(bits) -> bits
    None -> detached_error(st, method)
  }
}

/// The unshared+live gate: IsSharedArrayBuffer(O) is false AND
/// IsDetachedBuffer(O) is false. Hands back the buffer's bytes directly.
/// Immutable buffers pass: they are a legal SOURCE (sliceToImmutable), and
/// every write path gates on `require_not_immutable` besides.
fn require_unshared_bytes(st: Agent, buf: Buf, method: String) -> BitArray {
  case buf.storage {
    Bytes(bytes:, ..) | Immutable(bytes:) -> bytes
    Shared(..) -> incompatible(st, method)
    Detached(..) -> detached_error(st, method)
  }
}

/// The gate §25.1.6.6 resize needs after its (user-code-running) ToIndex: a
/// live, non-shared, RESIZABLE byte buffer. Hands over both the bytes and the
/// declared max, so the write-back rebuilds `Bytes` with the max it just
/// proved — an immutable or fixed-length buffer cannot slip through and be
/// rewritten as a resizable one.
fn require_resizable_bytes(
  st: Agent,
  buf: Buf,
  method: String,
) -> #(BitArray, Int) {
  case buf.storage {
    Bytes(bytes:, max_byte_length: Some(max)) -> #(bytes, max)
    Bytes(max_byte_length: None, ..) | Immutable(..) ->
      rt_val.t_throw_type_error(
        st,
        "ArrayBuffer.prototype."
          <> method
          <> " called on a non-resizable ArrayBuffer",
      )
    Shared(..) -> incompatible(st, method)
    Detached(..) -> detached_error(st, method)
  }
}

fn detached_error(st: Agent, method: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "ArrayBuffer.prototype." <> method <> " called on a detached ArrayBuffer",
  )
}

/// Immutable ArrayBuffer proposal: IsImmutableBuffer(O) must be false,
/// else TypeError (ArrayBufferCopyAndDetach step 6, DetachArrayBuffer).
fn require_not_immutable(st: Agent, buf: Buf, method: String) -> Buf {
  case buf.storage {
    Immutable(..) ->
      rt_val.t_throw_type_error(
        st,
        "ArrayBuffer.prototype."
          <> method
          <> " called on an immutable ArrayBuffer",
      )
    Bytes(..) | Shared(..) | Detached(..) -> buf
  }
}

fn incompatible(st: Agent, method: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "Method " <> method <> " called on incompatible receiver",
  )
}

/// A zero-filled binary of `n` bytes (§6.2.9.2 CreateByteDataBlock).
fn zero_block(n: Int) -> BitArray {
  let bits = n * 8
  <<0:size(bits)>>
}

/// Resize a data block: shrink truncates, grow zero-fills (§25.1.6.6 step 6).
fn resize_data(data: BitArray, new_len: Int) -> BitArray {
  let old_len = bit_array.byte_size(data)
  case new_len <= old_len {
    True -> {
      // In bounds by construction: newLen <= byte_size(data).
      let assert Ok(truncated) = bit_array.slice(data, 0, new_len)
      truncated
    }
    False -> bit_array.append(data, zero_block(new_len - old_len))
  }
}

/// Overwrite the first `count` bytes of `target` with
/// `source[offset .. offset+count)` (§6.2.9.3 CopyDataBlockBytes at
/// destination offset 0, as used by slice).
///
/// The caller (§25.1.6.7 slice, steps 19-21) has already proven both ranges,
/// so a slice failure is an arithmetic bug, never a data path.
fn copy_into(
  source: BitArray,
  offset: Int,
  count: Int,
  target: BitArray,
) -> BitArray {
  let target_len = bit_array.byte_size(target)
  let assert Ok(part) = bit_array.slice(source, offset, count)
  let assert Ok(rest) = bit_array.slice(target, count, target_len - count)
  bit_array.append(part, rest)
}

/// §7.1.5-based relative index (the shared ResolveBounds step): undefined →
/// `default`; negative counts from the end; clamped to [0, len].
fn relative_index(
  st: Agent,
  val: JsVal,
  len: Int,
  default: Int,
) -> #(Int, Agent) {
  case classify(val) {
    KUndef -> #(default, st)
    _ -> {
      let #(raw, st) = rt_val.t_to_integer_or_infinity(st, val)
      let k = case raw < 0 {
        True -> int.max(len + raw, 0)
        False -> int.min(raw, len)
      }
      #(k, st)
    }
  }
}

/// §7.3.22 SpeciesConstructor(O, defaultConstructor).
fn species_constructor(
  st: Agent,
  o: JsVal,
  default_ctor: Handle,
) -> #(JsVal, Agent) {
  // Step 1: C = ? Get(O, "constructor").
  let #(c, st) = rt_obj.t_get_prop(st, o, StringKey(Named("constructor")))
  case classify(c) {
    // Step 2: absent constructor → the intrinsic default.
    KUndef -> #(mk_object(default_ctor), st)
    KHandle(_) -> {
      // Step 4: S = ? Get(C, @@species).
      let #(s, st) =
        rt_obj.t_get_prop(st, c, types.SymbolKey(types.symbol_species))
      case classify(s) {
        // Step 5: undefined/null species → the intrinsic default.
        KUndef | types.KNull -> #(mk_object(default_ctor), st)
        // Steps 6-7: anything else must be a constructor.
        _ ->
          case rt_call.is_constructor(st, s) {
            True -> #(s, st)
            False ->
              rt_val.t_throw_type_error(
                st,
                "species constructor is not a constructor",
              )
          }
      }
    }
    // Step 3: a present but non-object "constructor" is a TypeError.
    _ -> rt_val.t_throw_type_error(st, "constructor property is not an object")
  }
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
