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
  OwnerBlock, ReturnThis, SObject, Shared, SharedArrayBufferConstructor,
  SharedArrayBufferGetByteLength, SharedArrayBufferGetGrowable,
  SharedArrayBufferGetMaxByteLength, SharedArrayBufferGrow,
  SharedArrayBufferSlice, StringKey, TypedArrayObj, classify, mk_bool, mk_number,
  mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

// 2^31 - 1, matches v8
const max_buffer_byte_length = 2_147_483_647

const invalid_length_msg = "Invalid array buffer length"

pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(#(BuiltinPair, BuiltinPair), Agent) {
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

  #(#(ab_type, sab_type), st)
}

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

fn detach_262(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let method = "detachArrayBuffer"
  let buf = require_buffer(st, helpers.first_arg_or_undefined(args), method)
  let buf = require_unshared(st, buf, method)
  let buf = require_not_immutable(st, buf, method)
  #(mk_undefined(), detach(st, buf))
}

pub fn dispatch_construct(
  st: Agent,
  native: ArrayBufferNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    ArrayBufferConstructor(..) ->
      constructor(st, args, new_target, shared: False)
    SharedArrayBufferConstructor(..) ->
      constructor(st, args, new_target, shared: True)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

fn constructor(
  st: Agent,
  args: List(JsVal),
  new_target: JsVal,
  shared shared: Bool,
) -> #(Handle, Agent) {
  let #(byte_length, st) =
    rt_val.t_to_index(
      st,
      helpers.first_arg_or_undefined(args),
      invalid_length_msg,
    )
  let #(max, st) = max_byte_length_option(st, helpers.arg_at(args, 1))
  allocate(st, new_target, byte_length, max, shared)
}

// §25.1.3.1 allocatearraybuffer
fn allocate(
  st: Agent,
  new_target: JsVal,
  byte_length: Int,
  max: Option(Int),
  shared: Bool,
) -> #(Handle, Agent) {
  case max {
    Some(m) if byte_length > m ->
      rt_val.t_throw_range_error(
        st,
        ctor_name(shared) <> " length exceeds maxByteLength option",
      )
    _ -> {
      let #(proto, st) =
        rt_call.get_prototype_from_constructor(st, new_target, fn(r) {
          case shared {
            True -> r.shared_array_buffer.prototype
            False -> r.array_buffer.prototype
          }
        })
      let max_ok = case max {
        Some(m) -> m <= max_buffer_byte_length
        None -> True
      }
      case byte_length <= max_buffer_byte_length && max_ok {
        False ->
          rt_val.t_throw_range_error(st, "Array buffer allocation failed")
        True -> {
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

// §25.1.3.7
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

fn ab_get_byte_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "byteLength")
  let buf = require_unshared(st, buf, "byteLength")
  #(mk_number(JInt(live_byte_size(buf))), st)
}

fn ab_get_detached(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "detached")
  let buf = require_unshared(st, buf, "detached")
  #(mk_bool(types.buffer_is_detached(buf.storage)), st)
}

fn ab_get_immutable(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "immutable")
  let buf = require_unshared(st, buf, "immutable")
  #(mk_bool(types.buffer_is_immutable(buf.storage)), st)
}

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

fn ab_get_resizable(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "resizable")
  let buf = require_unshared(st, buf, "resizable")
  #(mk_bool(max_byte_length(buf) != None), st)
}

// §25.1.6.6
fn ab_resize(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "resize")
  case max_byte_length(buf) {
    None ->
      rt_val.t_throw_type_error(
        st,
        "ArrayBuffer.prototype.resize called on a non-resizable ArrayBuffer",
      )
    Some(_) -> {
      let buf = require_unshared(st, buf, "resize")
      let #(new_len, st) =
        rt_val.t_to_index(
          st,
          helpers.first_arg_or_undefined(args),
          invalid_length_msg,
        )
      let buf = require_buffer(st, mk_object(buf.ref), "resize")
      let #(bytes, max) = require_resizable_bytes(st, buf, "resize")
      case new_len > max {
        True ->
          rt_val.t_throw_range_error(
            st,
            "ArrayBuffer.prototype.resize: new length exceeds maxByteLength",
          )
        False -> {
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

// §25.1.6.7 slice, shared and unshared
fn buffer_slice(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  shared shared: Bool,
) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "slice")
  let buf = require_family(st, buf, "slice", shared)
  let storage = require_live(st, buf, "slice")
  let len = types.buffer_byte_size(storage)
  let #(first, st) =
    relative_index(st, helpers.first_arg_or_undefined(args), len, 0)
  let #(final, st) = relative_index(st, helpers.arg_at(args, 1), len, len)
  let new_len = int.max(final - first, 0)
  let default_ctor = case shared {
    True -> st.realm.shared_array_buffer.constructor
    False -> st.realm.array_buffer.constructor
  }
  let #(ctor, st) = species_constructor(st, this, default_ctor)
  let #(new_h, st) =
    rt_call.t_construct(st, ctor, [mk_number(JInt(new_len))], ctor)
  let new_val = mk_object(new_h)
  let new_buf = require_buffer(st, new_val, "slice")
  let new_buf = require_family(st, new_buf, "slice", shared)
  let new_storage = require_live(st, new_buf, "slice")
  let new_buf = require_not_immutable(st, new_buf, "slice")
  case new_buf.ref == buf.ref {
    True ->
      rt_val.t_throw_type_error(
        st,
        "species constructor returned the same " <> ctor_name(shared),
      )
    False ->
      case types.buffer_byte_size(new_storage) < new_len {
        True ->
          rt_val.t_throw_type_error(
            st,
            "species constructor returned a buffer smaller than requested",
          )
        False -> {
          let buf = require_buffer(st, mk_object(buf.ref), "slice")
          let storage = require_live(st, buf, "slice")
          let current_len = types.buffer_byte_size(storage)
          case first < current_len {
            False -> #(new_val, st)
            True -> {
              let bits = require_live_bits(st, buf, "slice")
              let new_bits = require_live_bits(st, new_buf, "slice")
              let count = int.min(new_len, current_len - first)
              let copied = copy_into(bits, first, count, new_bits)
              #(new_val, buffer.store_region(st, new_buf.ref, copied, 0, count))
            }
          }
        }
      }
  }
}

fn slice_to_immutable(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "sliceToImmutable")
  let bytes = require_unshared_bytes(st, buf, "sliceToImmutable")
  let len = bit_array.byte_size(bytes)
  let #(first, st) =
    relative_index(st, helpers.first_arg_or_undefined(args), len, 0)
  let #(final, st) = relative_index(st, helpers.arg_at(args, 1), len, len)
  let new_len = int.max(final - first, 0)
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

type TransferMode {
  PreserveResizability
  ToFixedLength
  ToImmutable
}

// §25.1.3.4 arraybuffercopyanddetach
fn ab_transfer(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  mode: TransferMode,
) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "transfer")
  let buf = require_unshared(st, buf, "transfer")
  let len_arg = helpers.first_arg_or_undefined(args)
  let #(new_len, st) = case classify(len_arg) {
    KUndef -> #(live_byte_size(buf), st)
    _ -> rt_val.t_to_index(st, len_arg, invalid_length_msg)
  }
  let buf = require_buffer(st, mk_object(buf.ref), "transfer")
  let old_bits = require_unshared_bytes(st, buf, "transfer")
  let buf = require_not_immutable(st, buf, "transfer")
  let new_max = case mode {
    PreserveResizability -> max_byte_length(buf)
    ToFixedLength | ToImmutable -> None
  }
  let max_ok = case new_max {
    Some(m) -> new_len <= m && m <= max_buffer_byte_length
    None -> True
  }
  case new_len <= max_buffer_byte_length && max_ok {
    False -> rt_val.t_throw_range_error(st, "Array buffer allocation failed")
    True -> {
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
      let st = detach(st, buf)
      #(mk_object(new_h), st)
    }
  }
}

fn sab_get_byte_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "byteLength")
  let _block = require_shared(st, buf, "byteLength")
  #(mk_number(JInt(live_byte_size(buf))), st)
}

fn sab_get_growable(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "growable")
  let _bytes = require_shared(st, buf, "growable")
  #(mk_bool(max_byte_length(buf) != None), st)
}

fn sab_get_max_byte_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let buf = require_buffer(st, this, "maxByteLength")
  let _block = require_shared(st, buf, "maxByteLength")
  let max = option.unwrap(max_byte_length(buf), live_byte_size(buf))
  #(mk_number(JInt(max)), st)
}

// §25.2.5.3
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
      let buf = require_buffer(st, mk_object(buf.ref), "grow")
      let block = require_shared(st, buf, "grow")
      let invalid = fn() {
        rt_val.t_throw_range_error(
          st,
          "SharedArrayBuffer.prototype.grow: invalid length",
        )
      }
      use <- bool.lazy_guard(new_len > max, invalid)
      case block {
        LocalBlock(bytes: bits) -> {
          let current = bit_array.byte_size(bits)
          use <- bool.lazy_guard(new_len < current, invalid)
          let storage =
            Shared(
              block: LocalBlock(bytes: bit_array.append(
                bits,
                zero_block(new_len - current),
              )),
              max_byte_length: Some(max),
            )
          #(mk_undefined(), buffer.set_storage(st, buf.ref, storage))
        }
        OwnerBlock(owner:, ..) ->
          case sab.grow(owner, new_len) {
            Ok(Nil) -> #(mk_undefined(), st)
            Error(Nil) -> invalid()
          }
      }
    }
  }
}

type Buf {
  Buf(ref: Handle, storage: BufferStorage)
}

fn ctor_name(shared: Bool) -> String {
  case shared {
    True -> "SharedArrayBuffer"
    False -> "ArrayBuffer"
  }
}

fn live_byte_size(buf: Buf) -> Int {
  types.buffer_byte_size(buf.storage)
}

fn max_byte_length(buf: Buf) -> Option(Int) {
  types.buffer_max_byte_length(buf.storage)
}

fn detach(st: Agent, buf: Buf) -> Agent {
  buffer.set_storage(
    st,
    buf.ref,
    Detached(max_byte_length: max_byte_length(buf)),
  )
}

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

fn require_unshared(st: Agent, buf: Buf, method: String) -> Buf {
  case buf.storage {
    Shared(..) -> incompatible(st, method)
    Bytes(..) | Immutable(..) | Detached(..) -> buf
  }
}

fn require_shared(st: Agent, buf: Buf, method: String) -> types.SharedBlock {
  case buf.storage {
    Shared(block:, ..) -> block
    Bytes(..) | Immutable(..) | Detached(..) -> incompatible(st, method)
  }
}

fn require_family(st: Agent, buf: Buf, method: String, shared: Bool) -> Buf {
  case shared {
    True -> {
      let _bytes = require_shared(st, buf, method)
      buf
    }
    False -> require_unshared(st, buf, method)
  }
}

fn require_live(st: Agent, buf: Buf, method: String) -> BufferStorage {
  case buf.storage {
    Detached(..) -> detached_error(st, method)
    live -> live
  }
}

fn require_live_bits(st: Agent, buf: Buf, method: String) -> BitArray {
  case types.buffer_bits(buf.storage) {
    Some(bits) -> bits
    None -> detached_error(st, method)
  }
}

fn require_unshared_bytes(st: Agent, buf: Buf, method: String) -> BitArray {
  case buf.storage {
    Bytes(bytes:, ..) | Immutable(bytes:) -> bytes
    Shared(..) -> incompatible(st, method)
    Detached(..) -> detached_error(st, method)
  }
}

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

fn zero_block(n: Int) -> BitArray {
  let bits = n * 8
  <<0:size(bits)>>
}

fn resize_data(data: BitArray, new_len: Int) -> BitArray {
  let old_len = bit_array.byte_size(data)
  case new_len <= old_len {
    True -> {
      let assert Ok(truncated) = bit_array.slice(data, 0, new_len)
      truncated
    }
    False -> bit_array.append(data, zero_block(new_len - old_len))
  }
}

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

// §7.3.22 speciesconstructor
fn species_constructor(
  st: Agent,
  o: JsVal,
  default_ctor: Handle,
) -> #(JsVal, Agent) {
  let #(c, st) = rt_obj.t_get_prop(st, o, StringKey(Named("constructor")))
  case classify(c) {
    KUndef -> #(mk_object(default_ctor), st)
    KHandle(_) -> {
      let #(s, st) =
        rt_obj.t_get_prop(st, c, types.SymbolKey(types.symbol_species))
      case classify(s) {
        KUndef | types.KNull -> #(mk_object(default_ctor), st)
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
    _ -> rt_val.t_throw_type_error(st, "constructor property is not an object")
  }
}
