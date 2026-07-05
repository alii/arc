//// ES2024 §25.1 ArrayBuffer Objects + §25.2 SharedArrayBuffer Objects
////
//// Both share one exotic kind — value.ArrayBufferObject — whose whole state
//// is a value.BufferStorage sum type:
////   value.Bytes     — a plain ArrayBuffer (an immutable BEAM binary),
////   value.Shared    — a SharedArrayBuffer (an Erlang `atomics` array,
////                     genuinely shared across BEAM processes, see
////                     arc_sab_ffi.erl),
////   value.Immutable — an immutable ArrayBuffer (immutable-arraybuffer
////                     proposal),
////   value.Detached  — [[ArrayBufferData]] = null.
//// Detached-ness, shared-ness and immutability are variants, not flags, so a
//// detached buffer literally has no bytes to read and combinations the spec
//// forbids (immutable+shared/resizable/detached, shared+detached) cannot be
//// written down. [[ArrayBufferByteLength]] is derived via
//// value.buffer_byte_size; resizable/growable buffers carry
//// `max_byte_length: Some(n)`.
////
//// Spec algorithms follow tc39.es/ecma262 §25.1.3 (abstract operations) and
//// were cross-checked against engine262's ArrayBuffer intrinsics.

import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/key.{Named}
import arc/vm/ops/buffer as ops_buffer
import arc/vm/ops/coerce
import arc/vm/ops/object as ops_object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type ArrayBufferNativeFn, type JsValue, type Ref, ArrayBufferConstructor,
  ArrayBufferGetByteLength, ArrayBufferGetDetached, ArrayBufferGetImmutable,
  ArrayBufferGetMaxByteLength, ArrayBufferGetResizable, ArrayBufferGetSpecies,
  ArrayBufferIsView, ArrayBufferNative, ArrayBufferObject, ArrayBufferResize,
  ArrayBufferSlice, ArrayBufferSliceToImmutable, ArrayBufferTransfer,
  ArrayBufferTransferToFixedLength, ArrayBufferTransferToImmutable,
  DetachArrayBuffer262, Dispatch, JsBool, JsObject, JsUndefined, ObjectSlot,
  SharedArrayBufferConstructor, SharedArrayBufferGetByteLength,
  SharedArrayBufferGetGrowable, SharedArrayBufferGetMaxByteLength,
  SharedArrayBufferGetSpecies, SharedArrayBufferGrow, SharedArrayBufferSlice,
}
import gleam/bit_array
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
/// Returns #(heap, ArrayBuffer BuiltinType, SharedArrayBuffer BuiltinType).
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType, BuiltinType) {
  // --- ArrayBuffer ---
  let #(h, ab_methods) =
    common.alloc_methods(h, function_proto, [
      #("resize", ArrayBufferNative(ArrayBufferResize), 1),
      #("slice", ArrayBufferNative(ArrayBufferSlice), 2),
      #("sliceToImmutable", ArrayBufferNative(ArrayBufferSliceToImmutable), 2),
      #("transfer", ArrayBufferNative(ArrayBufferTransfer), 0),
      #(
        "transferToFixedLength",
        ArrayBufferNative(ArrayBufferTransferToFixedLength),
        0,
      ),
      #(
        "transferToImmutable",
        ArrayBufferNative(ArrayBufferTransferToImmutable),
        0,
      ),
    ])
  let #(h, ab_getters) =
    common.alloc_getters(h, function_proto, [
      #("byteLength", ArrayBufferNative(ArrayBufferGetByteLength)),
      #("detached", ArrayBufferNative(ArrayBufferGetDetached)),
      #("immutable", ArrayBufferNative(ArrayBufferGetImmutable)),
      #("maxByteLength", ArrayBufferNative(ArrayBufferGetMaxByteLength)),
      #("resizable", ArrayBufferNative(ArrayBufferGetResizable)),
    ])
  let #(h, ab_statics) =
    common.alloc_methods(h, function_proto, [
      #("isView", ArrayBufferNative(ArrayBufferIsView), 1),
    ])
  let #(h, ab_type) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      list.append(ab_getters, ab_methods),
      fn(proto) { Dispatch(ArrayBufferNative(ArrayBufferConstructor(proto))) },
      "ArrayBuffer",
      1,
      ab_statics,
    )
  let h = common.add_to_string_tag(h, ab_type.prototype, "ArrayBuffer")
  let #(h, ab_species) =
    alloc_species_getter(h, function_proto, ArrayBufferGetSpecies)
  let h =
    common.add_symbol_property(
      h,
      ab_type.constructor,
      value.symbol_species,
      ab_species,
    )

  // --- SharedArrayBuffer ---
  let #(h, sab_methods) =
    common.alloc_methods(h, function_proto, [
      #("grow", ArrayBufferNative(SharedArrayBufferGrow), 1),
      #("slice", ArrayBufferNative(SharedArrayBufferSlice), 2),
    ])
  let #(h, sab_getters) =
    common.alloc_getters(h, function_proto, [
      #("byteLength", ArrayBufferNative(SharedArrayBufferGetByteLength)),
      #("growable", ArrayBufferNative(SharedArrayBufferGetGrowable)),
      #("maxByteLength", ArrayBufferNative(SharedArrayBufferGetMaxByteLength)),
    ])
  let #(h, sab_type) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      list.append(sab_getters, sab_methods),
      fn(proto) {
        Dispatch(ArrayBufferNative(SharedArrayBufferConstructor(proto)))
      },
      "SharedArrayBuffer",
      1,
      [],
    )
  let h = common.add_to_string_tag(h, sab_type.prototype, "SharedArrayBuffer")
  let #(h, sab_species) =
    alloc_species_getter(h, function_proto, SharedArrayBufferGetSpecies)
  let h =
    common.add_symbol_property(
      h,
      sab_type.constructor,
      value.symbol_species,
      sab_species,
    )

  // §25.1.5.2/§25.2.4.1: the constructors' "prototype" property is
  // { writable: false, enumerable: false, configurable: false } — installed
  // that way by common.init_type (see common.ctor_properties).
  #(h, ab_type, sab_type)
}

/// Allocate a `get [Symbol.species]` accessor (getter only, configurable).
fn alloc_species_getter(
  h: Heap(host),
  function_proto: Ref,
  native: ArrayBufferNativeFn,
) -> #(Heap(host), value.Property) {
  let #(h, get_ref) =
    common.alloc_native_fn(
      h,
      function_proto,
      ArrayBufferNative(native),
      "get [Symbol.species]",
      0,
    )
  #(
    h,
    value.accessor(
      get: Some(JsObject(get_ref)),
      set: None,
      enumerable: False,
      configurable: True,
    ),
  )
}

// ============================================================================
// Dispatch
// ============================================================================

/// Per-module dispatch for ArrayBuffer/SharedArrayBuffer native functions.
pub fn dispatch(
  native: ArrayBufferNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    ArrayBufferConstructor(proto:) ->
      constructor(proto, args, state, shared: False)
    SharedArrayBufferConstructor(proto:) ->
      constructor(proto, args, state, shared: True)
    ArrayBufferIsView -> is_view(args, state)
    ArrayBufferGetSpecies | SharedArrayBufferGetSpecies -> #(state, Ok(this))
    ArrayBufferGetByteLength -> ab_get_byte_length(this, state)
    ArrayBufferGetDetached -> ab_get_detached(this, state)
    ArrayBufferGetImmutable -> ab_get_immutable(this, state)
    ArrayBufferGetMaxByteLength -> ab_get_max_byte_length(this, state)
    ArrayBufferGetResizable -> ab_get_resizable(this, state)
    ArrayBufferResize -> ab_resize(this, args, state)
    ArrayBufferSlice -> buffer_slice(this, args, state, shared: False)
    ArrayBufferSliceToImmutable -> slice_to_immutable(this, args, state)
    ArrayBufferTransfer -> ab_transfer(this, args, state, PreserveResizability)
    ArrayBufferTransferToFixedLength ->
      ab_transfer(this, args, state, ToFixedLength)
    ArrayBufferTransferToImmutable ->
      ab_transfer(this, args, state, ToImmutable)
    SharedArrayBufferGetByteLength -> sab_get_byte_length(this, state)
    SharedArrayBufferGetGrowable -> sab_get_growable(this, state)
    SharedArrayBufferGetMaxByteLength -> sab_get_max_byte_length(this, state)
    SharedArrayBufferGrow -> sab_grow(this, args, state)
    SharedArrayBufferSlice -> buffer_slice(this, args, state, shared: True)
    DetachArrayBuffer262 -> detach_262(args, state)
  }
}

// ============================================================================
// §25.1.4.1 ArrayBuffer ( length [ , options ] )
// §25.2.3.1 SharedArrayBuffer ( length [ , options ] )
// ============================================================================

/// Steps:
///   1. If NewTarget is undefined, throw a TypeError exception.
///   2. Let byteLength be ? ToIndex(length).
///   3. Let requestedMaxByteLength be ? GetArrayBufferMaxByteLengthOption(options).
///   4. Return ? AllocateArrayBuffer(NewTarget, byteLength, requestedMaxByteLength).
fn constructor(
  proto: Ref,
  args: List(JsValue),
  state: State(host),
  shared shared: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  let name = ctor_name(shared)
  // Step 1: do_construct sets state.new_target before native dispatch;
  // a plain call leaves it JsUndefined.
  case state.new_target {
    JsUndefined ->
      state.type_error(state, "Constructor " <> name <> " requires 'new'")
    new_target -> {
      // Step 2: ToIndex(length)
      use byte_length, state <- coerce.to_index_cps(
        state,
        helpers.first_arg_or_undefined(args),
        invalid_length_msg,
      )
      // Step 3: GetArrayBufferMaxByteLengthOption(options)
      let options = helpers.arg_at(args, 1)
      use max, state <- try_max_byte_length_option(state, options)
      // Step 4: AllocateArrayBuffer / AllocateSharedArrayBuffer
      allocate(state, new_target, proto, byte_length, max, shared)
    }
  }
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
  state: State(host),
  new_target: JsValue,
  intrinsic_proto: Ref,
  byte_length: Int,
  max: Option(Int),
  shared: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 3a
  case max {
    Some(m) if byte_length > m ->
      state.range_error(
        state,
        ctor_name(shared) <> " length exceeds maxByteLength option",
      )
    _ -> {
      // Step 4: GetPrototypeFromConstructor(NewTarget, intrinsic) — must use
      // a real [[Get]] so accessor `prototype` properties are invoked.
      use proto, state <- ops_object.proto_from_new_target(
        state,
        new_target,
        intrinsic_proto,
      )
      // Step 5 (+ step 6a for resizable): CreateByteDataBlock limits
      let max_ok = case max {
        Some(m) -> m <= max_buffer_byte_length
        None -> True
      }
      case byte_length <= max_buffer_byte_length && max_ok {
        False -> state.range_error(state, "Array buffer allocation failed")
        True -> {
          // §6.2.9.2 CreateByteDataBlock / CreateSharedByteDataBlock:
          // non-shared buffers are an immutable BEAM binary; shared buffers
          // live in an Erlang `atomics` array (zero-initialized). Growable
          // SABs pre-allocate max_byte_length cells so the ref never needs
          // reallocating (grow only publishes a new length into the shared
          // length cell).
          let storage = case shared {
            False ->
              value.Bytes(bytes: zero_block(byte_length), max_byte_length: max)
            True -> {
              let capacity = option.unwrap(max, byte_length)
              value.Shared(
                ref: value.sab_new(capacity, byte_length),
                max_byte_length: max,
              )
            }
          }
          let #(heap, ref) =
            common.alloc_wrapper(state.heap, ArrayBufferObject(storage:), proto)
          #(State(..state, heap:), Ok(JsObject(ref)))
        }
      }
    }
  }
}

/// §25.1.3.7 GetArrayBufferMaxByteLengthOption ( options )
///
///   1. If options is not an Object, return empty.
///   2. Let maxByteLength be ? Get(options, "maxByteLength").
///   3. If maxByteLength is undefined, return empty.
///   4. Return ? ToIndex(maxByteLength).
fn try_max_byte_length_option(
  state: State(host),
  options: JsValue,
  cont: fn(Option(Int), State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case options {
    JsObject(opt_ref) -> {
      use max_val, state <- state.try_op(ops_object.get_value(
        state,
        opt_ref,
        Named("maxByteLength"),
        options,
      ))
      case max_val {
        JsUndefined -> cont(None, state)
        _ -> {
          use max, state <- coerce.to_index_cps(
            state,
            max_val,
            invalid_length_msg,
          )
          cont(Some(max), state)
        }
      }
    }
    _ -> cont(None, state)
  }
}

// ============================================================================
// §25.1.5.1 ArrayBuffer.isView ( arg )
// ============================================================================

/// Returns true iff arg has a [[ViewedArrayBuffer]] internal slot
/// (TypedArray or DataView instances).
fn is_view(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let result = case helpers.first_arg_or_undefined(args) {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.DataViewObject(..), ..)) -> True
        Some(ObjectSlot(kind: value.TypedArrayObject(..), ..)) -> True
        _ -> False
      }
    _ -> False
  }
  #(state, Ok(JsBool(result)))
}

// ============================================================================
// ArrayBuffer.prototype getters — §25.1.6.2–25.1.6.5
// ============================================================================

/// §25.1.6.2 get ArrayBuffer.prototype.byteLength
///   shared → TypeError; detached → +0; else [[ArrayBufferByteLength]].
fn ab_get_byte_length(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use buf, state <- require_buffer(this, state, "byteLength")
  use buf, state <- require_unshared(buf, state, "byteLength")
  #(state, Ok(value.from_int(live_byte_size(buf))))
}

/// §25.1.6.3 get ArrayBuffer.prototype.detached
fn ab_get_detached(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use buf, state <- require_buffer(this, state, "detached")
  use buf, state <- require_unshared(buf, state, "detached")
  #(state, Ok(JsBool(value.buffer_is_detached(buf.storage))))
}

/// Immutable ArrayBuffer proposal: get ArrayBuffer.prototype.immutable
///   1. RequireInternalSlot(O, [[ArrayBufferData]]).
///   2. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.
///   3. Return IsImmutableBuffer(O).
fn ab_get_immutable(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use buf, state <- require_buffer(this, state, "immutable")
  use buf, state <- require_unshared(buf, state, "immutable")
  #(state, Ok(JsBool(value.buffer_is_immutable(buf.storage))))
}

/// §25.1.6.4 get ArrayBuffer.prototype.maxByteLength
///   shared → TypeError; detached → +0; fixed-length → byteLength;
///   else [[ArrayBufferMaxByteLength]].
fn ab_get_max_byte_length(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use buf, state <- require_buffer(this, state, "maxByteLength")
  use buf, state <- require_unshared(buf, state, "maxByteLength")
  let result = case buf.storage {
    value.Detached(..) -> 0
    live ->
      case value.buffer_max_byte_length(live) {
        Some(max) -> max
        None -> value.buffer_byte_size(live)
      }
  }
  #(state, Ok(value.from_int(result)))
}

/// §25.1.6.5 get ArrayBuffer.prototype.resizable
fn ab_get_resizable(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use buf, state <- require_buffer(this, state, "resizable")
  use buf, state <- require_unshared(buf, state, "resizable")
  #(state, Ok(JsBool(max_byte_length(buf) != None)))
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
fn ab_resize(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use buf, state <- require_buffer(this, state, "resize")
  // Step 1: fixed-length buffers lack [[ArrayBufferMaxByteLength]] (an
  // immutable buffer never has one, so it lands here, not on a write path)
  case max_byte_length(buf) {
    None ->
      state.type_error(
        state,
        "ArrayBuffer.prototype.resize called on a non-resizable ArrayBuffer",
      )
    Some(_) -> {
      // Step 2
      use buf, state <- require_unshared(buf, state, "resize")
      // Step 3: ToIndex may run user code (valueOf) — re-read O after.
      use new_len, state <- coerce.to_index_cps(
        state,
        helpers.first_arg_or_undefined(args),
        invalid_length_msg,
      )
      use buf, state <- require_buffer(JsObject(buf.ref), state, "resize")
      // Step 4 (the gate hands us the live bytes of a RESIZABLE byte buffer:
      // step 1 already proved the storage is `Bytes(_, Some(_))`, so the only
      // thing user code can have changed underneath us is detaching it)
      use bytes, max, buf, state <- require_resizable_bytes(
        buf,
        state,
        "resize",
      )
      // Step 5
      case new_len > max {
        True ->
          state.range_error(
            state,
            "ArrayBuffer.prototype.resize: new length exceeds maxByteLength",
          )
        False -> {
          // Step 6. The storage stays `Bytes` with the SAME max: a resize
          // cannot turn a resizable buffer into a fixed-length one.
          let storage =
            value.Bytes(
              bytes: resize_data(bytes, new_len),
              max_byte_length: Some(max),
            )
          let heap =
            heap.update_kind(state.heap, buf.ref, ArrayBufferObject(storage:))
          #(State(..state, heap:), Ok(JsUndefined))
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
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  shared shared: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 2-3: RequireInternalSlot + shared/unshared gate
  use buf, state <- require_buffer(this, state, "slice")
  use buf, state <- require_family(buf, state, "slice", shared)
  // Step 4 (AB only): detached → TypeError
  use storage, buf, state <- require_live(buf, state, "slice")
  // Step 5
  let len = value.buffer_byte_size(storage)
  // Steps 6-7: relativeStart
  use first, state <- coerce.try_relative_index(
    state,
    helpers.first_arg_or_undefined(args),
    len,
    0,
  )
  // Steps 8-9: relativeEnd (undefined → len)
  use final, state <- coerce.try_relative_index(
    state,
    helpers.arg_at(args, 1),
    len,
    len,
  )
  // Step 10
  let new_len = int.max(final - first, 0)
  // Step 11: SpeciesConstructor(O, intrinsic default)
  let default_ctor = case shared {
    True -> state.builtins.shared_array_buffer.constructor
    False -> state.builtins.array_buffer.constructor
  }
  use ctor_ref, state <- state.try_op(ops_object.species_constructor(
    state,
    this,
    default_ctor,
  ))
  // Step 12: Construct(ctor, « 𝔽(newLen) »)
  use new_val, state <- state.try_op(
    state.construct(state, JsObject(ctor_ref), [
      value.from_int(new_len),
    ]),
  )
  // Steps 13-15: validate the constructed buffer
  use new_buf, state <- require_buffer(new_val, state, "slice")
  use new_buf, state <- require_family(new_buf, state, "slice", shared)
  use new_storage, new_buf, state <- require_live(new_buf, state, "slice")
  // Immutable ArrayBuffer proposal: a species constructor returning an
  // immutable buffer is a TypeError — slice must write into the result.
  use new_buf, state <- require_not_immutable(new_buf, state, "slice")
  // Step 16: SameValue(new, O) → TypeError
  case new_buf.ref == buf.ref {
    True ->
      state.type_error(
        state,
        "species constructor returned the same " <> ctor_name(shared),
      )
    False ->
      // Step 17
      case value.buffer_byte_size(new_storage) < new_len {
        True ->
          state.type_error(
            state,
            "species constructor returned a buffer smaller than requested",
          )
        False -> {
          // Steps 18-19: species ctor may have detached O — re-read.
          use buf, state <- require_buffer(JsObject(buf.ref), state, "slice")
          use storage, buf, state <- require_live(buf, state, "slice")
          let current_len = value.buffer_byte_size(storage)
          // Copy min(newLen, currentLen - first) bytes from offset `first`.
          // Snapshotting the bytes is O(n) for shared storage, so only do it
          // once we know there is something to copy.
          case first < current_len {
            False -> #(state, Ok(new_val))
            True -> {
              use bits, _buf, state <- require_live_bits(buf, state, "slice")
              use new_bits, new_buf, state <- require_live_bits(
                new_buf,
                state,
                "slice",
              )
              let count = int.min(new_len, current_len - first)
              let copied = copy_into(bits, first, count, new_bits)
              // §6.2.9.3 CopyDataBlockBytes writes exactly [0, count) of the
              // destination — anything past `count` in a SHARED destination
              // belongs to whatever agent last wrote it, so the write range
              // must be the copied region, not the whole snapshot image.
              let heap =
                ops_buffer.store_region(
                  state.heap,
                  new_buf.ref,
                  copied,
                  0,
                  count,
                )
              #(State(..state, heap:), Ok(new_val))
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
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 2-4
  use buf, state <- require_buffer(this, state, "sliceToImmutable")
  use bytes, buf, state <- require_unshared_bytes(
    buf,
    state,
    "sliceToImmutable",
  )
  // Step 5
  let len = bit_array.byte_size(bytes)
  // Steps 6-8: ResolveBounds — ToIntegerOrInfinity may run user code.
  use first, state <- coerce.try_relative_index(
    state,
    helpers.first_arg_or_undefined(args),
    len,
    0,
  )
  use final, state <- coerce.try_relative_index(
    state,
    helpers.arg_at(args, 1),
    len,
    len,
  )
  // Step 9
  let new_len = int.max(final - first, 0)
  // Steps 11-14: re-read O — the coercions may have detached or resized it.
  use buf, state <- require_buffer(JsObject(buf.ref), state, "sliceToImmutable")
  use bytes, _buf, state <- require_unshared_bytes(
    buf,
    state,
    "sliceToImmutable",
  )
  let current_len = bit_array.byte_size(bytes)
  case current_len < final {
    True ->
      state.range_error(
        state,
        "ArrayBuffer.prototype.sliceToImmutable: source was resized below the requested range",
      )
    False -> {
      // Step 15: AllocateImmutableArrayBuffer — copy [first, first+newLen).
      // In bounds by construction: step 14 just proved final <= currentLen,
      // and newLen > 0 implies first < final, so [first, first+newLen) =
      // [first, final) sits inside the buffer. (newLen == 0 must not reach
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
      let #(heap, new_ref) =
        common.alloc_wrapper(
          state.heap,
          ArrayBufferObject(storage: value.Immutable(bytes: data)),
          state.builtins.array_buffer.prototype,
        )
      #(State(..state, heap:), Ok(JsObject(new_ref)))
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
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  mode: TransferMode,
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2
  use buf, state <- require_buffer(this, state, "transfer")
  use buf, state <- require_unshared(buf, state, "transfer")
  // Step 3
  let len_arg = helpers.first_arg_or_undefined(args)
  use new_len, state <- try_transfer_length(state, len_arg, buf)
  // ToIndex may run user code — re-read O.
  use buf, state <- require_buffer(JsObject(buf.ref), state, "transfer")
  // Step 4 (the gate hands us the source bytes)
  use old_bits, buf, state <- require_unshared_bytes(buf, state, "transfer")
  // Step 6 (immutable-arraybuffer proposal): IsImmutableBuffer(O) →
  // TypeError. Immutable buffers cannot be detached, so no transfer flavour
  // (transfer / transferToFixedLength / transferToImmutable) accepts one.
  use buf, state <- require_not_immutable(buf, state, "transfer")
  // Step 5 (+ the proposal's step 5 for transferToImmutable). An immutable
  // result carries no maxByteLength: `value.Immutable` has no such field.
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
    False -> state.range_error(state, "Array buffer allocation failed")
    True -> {
      // Step 8: copy then zero-extend. In bounds by construction:
      // copyLen = min(newByteLength, old byteLength) <= byte_size(old_bits),
      // so CopyDataBlockBytes' source range [0, copyLen) always exists.
      let old_len = bit_array.byte_size(old_bits)
      let copy_len = int.min(new_len, old_len)
      let assert Ok(copied) = bit_array.slice(old_bits, 0, copy_len)
      let data = bit_array.append(copied, zero_block(new_len - copy_len))
      let storage = case mode {
        ToImmutable -> value.Immutable(bytes: data)
        PreserveResizability | ToFixedLength ->
          value.Bytes(bytes: data, max_byte_length: new_max)
      }
      let #(heap, new_ref) =
        common.alloc_wrapper(
          state.heap,
          ArrayBufferObject(storage:),
          state.builtins.array_buffer.prototype,
        )
      // Step 9: DetachArrayBuffer(O) — data → null, byteLength → 0.
      // [[ArrayBufferMaxByteLength]] survives (resizable getter stays true).
      let heap = heap.update_kind(heap, buf.ref, kind_detached(buf))
      #(State(..state, heap:), Ok(JsObject(new_ref)))
    }
  }
}

/// Step 3 of ArrayBufferCopyAndDetach: undefined → current length, else ToIndex.
fn try_transfer_length(
  state: State(host),
  len_arg: JsValue,
  buf: Buf,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case len_arg {
    JsUndefined -> cont(live_byte_size(buf), state)
    _ -> coerce.to_index_cps(state, len_arg, invalid_length_msg, cont)
  }
}

// ============================================================================
// SharedArrayBuffer.prototype getters + grow — §25.2.5
// ============================================================================

/// §25.2.5.2 get SharedArrayBuffer.prototype.byteLength
fn sab_get_byte_length(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use buf, state <- require_buffer(this, state, "byteLength")
  use _ref, byte_length, _buf, state <- require_shared(buf, state, "byteLength")
  #(state, Ok(value.from_int(byte_length)))
}

/// §25.2.5.4 get SharedArrayBuffer.prototype.growable
fn sab_get_growable(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use buf, state <- require_buffer(this, state, "growable")
  use _ref, _len, buf, state <- require_shared(buf, state, "growable")
  #(state, Ok(JsBool(max_byte_length(buf) != None)))
}

/// §25.2.5.5 get SharedArrayBuffer.prototype.maxByteLength
fn sab_get_max_byte_length(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use buf, state <- require_buffer(this, state, "maxByteLength")
  use _ref, byte_length, buf, state <- require_shared(
    buf,
    state,
    "maxByteLength",
  )
  #(state, Ok(value.from_int(option.unwrap(max_byte_length(buf), byte_length))))
}

/// §25.2.5.3 SharedArrayBuffer.prototype.grow ( newLength )
///
///   1. RequireInternalSlot(O, [[ArrayBufferMaxByteLength]]).
///   2. If IsSharedArrayBuffer(O) is false, throw TypeError.
///   3. newByteLength = ? ToIndex(newLength).
///   4. GrowSharedArrayBuffer: newByteLength < currentByteLength or
///      newByteLength > maxByteLength → RangeError. Growth zero-fills.
///
/// The buffer's byte length lives in the SHARED atomics block, not in this
/// agent's heap slot, so nothing here is written back into the heap: the FFI
/// publishes the new length and every agent holding the buffer sees it.
fn sab_grow(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use buf, state <- require_buffer(this, state, "grow")
  case max_byte_length(buf) {
    None ->
      state.type_error(
        state,
        "SharedArrayBuffer.prototype.grow called on a non-growable SharedArrayBuffer",
      )
    Some(_) -> {
      // The gate proves the storage is shared and hands us its atomics ref —
      // there is no "grow a byte-array SharedArrayBuffer" branch to write.
      use ref, _len, _buf, state <- require_shared(buf, state, "grow")
      use new_len, state <- coerce.to_index_cps(
        state,
        helpers.first_arg_or_undefined(args),
        invalid_length_msg,
      )
      // Shared storage pre-allocated max_byte_length data cells (already
      // zero-filled), so growth only publishes the new length — and the FFI,
      // which owns those cells, is the one that decides whether new_len fits:
      // `TooLarge` past the allocated max, `TooSmall` when a concurrent agent
      // has already grown past new_len (the length is monotonic).
      case value.sab_grow(ref, new_len) {
        value.Grown -> #(state, Ok(JsUndefined))
        value.TooSmall | value.TooLarge ->
          state.range_error(
            state,
            "SharedArrayBuffer.prototype.grow: invalid length",
          )
      }
    }
  }
}

// ============================================================================
// $262.detachArrayBuffer ( buffer ) — test262 host hook
// ============================================================================

/// DetachArrayBuffer (§25.1.3.5) on the given buffer. Shared buffers and
/// non-buffers are a TypeError.
fn detach_262(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let arg = helpers.first_arg_or_undefined(args)
  use buf, state <- require_buffer(arg, state, "detachArrayBuffer")
  use buf, state <- require_unshared(buf, state, "detachArrayBuffer")
  // Immutable ArrayBuffer proposal: DetachArrayBuffer throws on an
  // immutable buffer — they can never be detached.
  use buf, state <- require_not_immutable(buf, state, "detachArrayBuffer")
  let heap = heap.update_kind(state.heap, buf.ref, kind_detached(buf))
  #(State(..state, heap:), Ok(JsUndefined))
}

// ============================================================================
// Helpers
// ============================================================================

/// Internal view of an ArrayBufferObject heap slot: its ref plus its whole
/// storage state. Shared-ness, detached-ness and immutability are variants of
/// `storage`, not fields, so no combination of them can be out of step.
type Buf {
  Buf(ref: Ref, storage: value.BufferStorage)
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
  value.buffer_byte_size(buf.storage)
}

/// [[ArrayBufferMaxByteLength]] of a Buf, absent for fixed-length buffers.
fn max_byte_length(buf: Buf) -> Option(Int) {
  value.buffer_max_byte_length(buf.storage)
}

/// §25.1.3.5 DetachArrayBuffer — [[ArrayBufferData]] becomes null. There is
/// no leftover byte array to read: the storage is simply gone.
/// [[ArrayBufferMaxByteLength]] survives (the resizable getter stays true).
fn kind_detached(buf: Buf) -> state.ExoticKind(host) {
  ArrayBufferObject(
    storage: value.Detached(max_byte_length: max_byte_length(buf)),
  )
}

/// RequireInternalSlot(O, [[ArrayBufferData]]) — `this` must be an
/// ArrayBuffer or SharedArrayBuffer object.
fn require_buffer(
  this: JsValue,
  state: State(host),
  method: String,
  cont: fn(Buf, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: ArrayBufferObject(storage:), ..)) ->
          cont(Buf(ref:, storage:), state)
        _ -> incompatible(state, method)
      }
    _ -> incompatible(state, method)
  }
}

/// IsSharedArrayBuffer(O) must be false, else TypeError. The buffer may still
/// be detached — the getters (byteLength/detached/maxByteLength/resizable)
/// are the ones that need to see that state.
fn require_unshared(
  buf: Buf,
  state: State(host),
  method: String,
  cont: fn(Buf, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case buf.storage {
    value.Shared(..) -> incompatible(state, method)
    value.Bytes(..) | value.Immutable(..) | value.Detached(..) ->
      cont(buf, state)
  }
}

/// IsSharedArrayBuffer(O) must be true, else TypeError. Hands the
/// continuation the atomics ref that IS the shared storage plus its current
/// (shared) byte length — the proof travels with the gate, so no caller has
/// to re-derive it or write a "what if it were byte storage" branch. (Only
/// `value.Shared` carries an atomics ref, and it is never detached.)
fn require_shared(
  buf: Buf,
  state: State(host),
  method: String,
  cont: fn(value.AtomicsRef, Int, Buf, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case buf.storage {
    value.Shared(ref:, ..) -> cont(ref, value.sab_byte_length(ref), buf, state)
    value.Bytes(..) | value.Immutable(..) | value.Detached(..) ->
      incompatible(state, method)
  }
}

/// Gate on the expected buffer family: shared=True requires a
/// SharedArrayBuffer, shared=False requires a plain ArrayBuffer.
fn require_family(
  buf: Buf,
  state: State(host),
  method: String,
  shared: Bool,
  cont: fn(Buf, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case shared {
    True ->
      require_shared(buf, state, method, fn(_ref, _len, buf, state) {
        cont(buf, state)
      })
    False -> require_unshared(buf, state, method, cont)
  }
}

/// IsDetachedBuffer(O) must be false, else TypeError. Hands the continuation
/// the live storage — cheap, so byte-length reads stay O(1) even for `Shared`
/// storage. Use `require_live_bits` when the bytes themselves are needed.
/// (Shared buffers are never detached, so this always succeeds for them.)
fn require_live(
  buf: Buf,
  state: State(host),
  method: String,
  cont: fn(value.BufferStorage, Buf, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case buf.storage {
    value.Detached(..) -> detached_error(state, method)
    live -> cont(live, buf, state)
  }
}

/// `require_live`, but hands the continuation a snapshot of the live bytes —
/// a detached buffer has none. For `Shared` storage the snapshot COPIES the
/// atomics cells (O(byte length)), so only reach for this gate where the
/// bytes are actually consumed; use `require_live` for length checks.
fn require_live_bits(
  buf: Buf,
  state: State(host),
  method: String,
  cont: fn(BitArray, Buf, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case value.buffer_bits(buf.storage) {
    Some(bits) -> cont(bits, buf, state)
    None -> detached_error(state, method)
  }
}

/// The unshared+live gate: IsSharedArrayBuffer(O) is false AND
/// IsDetachedBuffer(O) is false. Hands the continuation the buffer's bytes
/// directly — a non-shared buffer's storage IS a BitArray, so nothing
/// downstream needs a shared-storage branch. Immutable buffers pass: they are
/// a legal SOURCE (sliceToImmutable), and every write path gates on
/// `require_not_immutable` besides.
fn require_unshared_bytes(
  buf: Buf,
  state: State(host),
  method: String,
  cont: fn(BitArray, Buf, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case buf.storage {
    value.Bytes(bytes:, ..) | value.Immutable(bytes:) -> cont(bytes, buf, state)
    value.Shared(..) -> incompatible(state, method)
    value.Detached(..) -> detached_error(state, method)
  }
}

/// The gate §25.1.6.6 resize needs after its (user-code-running) ToIndex: a
/// live, non-shared, RESIZABLE byte buffer. Hands over both the bytes and the
/// declared max, so the write-back rebuilds `value.Bytes` with the max it just
/// proved — an immutable or fixed-length buffer cannot slip through and be
/// rewritten as a resizable one.
fn require_resizable_bytes(
  buf: Buf,
  state: State(host),
  method: String,
  cont: fn(BitArray, Int, Buf, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case buf.storage {
    value.Bytes(bytes:, max_byte_length: Some(max)) ->
      cont(bytes, max, buf, state)
    value.Bytes(max_byte_length: None, ..) | value.Immutable(..) ->
      state.type_error(
        state,
        "ArrayBuffer.prototype."
          <> method
          <> " called on a non-resizable ArrayBuffer",
      )
    value.Shared(..) -> incompatible(state, method)
    value.Detached(..) -> detached_error(state, method)
  }
}

fn detached_error(
  state: State(host),
  method: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  state.type_error(
    state,
    "ArrayBuffer.prototype." <> method <> " called on a detached ArrayBuffer",
  )
}

/// Immutable ArrayBuffer proposal: IsImmutableBuffer(O) must be false,
/// else TypeError (ArrayBufferCopyAndDetach step 6, DetachArrayBuffer).
fn require_not_immutable(
  buf: Buf,
  state: State(host),
  method: String,
  cont: fn(Buf, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case buf.storage {
    value.Immutable(..) ->
      state.type_error(
        state,
        "ArrayBuffer.prototype."
          <> method
          <> " called on an immutable ArrayBuffer",
      )
    value.Bytes(..) | value.Shared(..) | value.Detached(..) -> cont(buf, state)
  }
}

fn incompatible(
  state: State(host),
  method: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  state.type_error(
    state,
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
/// The caller (§25.1.6.13 slice, steps 19-21) has already proven both ranges:
/// `offset < currentLen` and `count = min(newLen, currentLen - offset)` bound
/// the source, and step 17 (`byteLength(new) >= newLen >= count`) bounds the
/// target — so a slice failure is an arithmetic bug, never a data path.
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
