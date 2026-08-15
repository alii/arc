//// ES2024 §25.1 ArrayBuffer Objects
////
//// Internal storage: `ArrayBufferObj(bytes: BitArray, detached: Bool)` exotic
//// kind. SharedArrayBuffer is OUT of scope (Realm has no `shared_array_buffer`
//// field). Port of arc `builtins/array_buffer.gleam:62-145` init/dispatch
//// re-expressed under D7 (`Error(e)` → `t_throw`) and R1 (`#(V, St')`).

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type ArrayBufferNative, type BuiltinPair, type Handle, type JsVal,
  ArrayBufferConstructor, ArrayBufferGetByteLength, ArrayBufferGetDetached,
  ArrayBufferGetMaxByteLength, ArrayBufferGetResizable, ArrayBufferIsView,
  ArrayBufferN, ArrayBufferObj, ArrayBufferResize, ArrayBufferSlice,
  ArrayBufferTransfer, ArrayBufferTransferToFixedLength, DataViewObj, JInt,
  KHandle, Named, NoElements, ReturnThis, SObject, StringKey, TypedArrayObj,
  classify, mk_bool, mk_number, mk_object,
} as rt_types
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{Some}

// ═══════════════════════════════════════════════════════════════════════════
// Init — ArrayBuffer constructor + ArrayBuffer.prototype
// ═══════════════════════════════════════════════════════════════════════════

/// Set up ArrayBuffer constructor + ArrayBuffer.prototype (§25.1.5/6).
/// ArrayBuffer.length is 1.
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("slice", ArrayBufferN(ArrayBufferSlice), 2),
      #("resize", ArrayBufferN(ArrayBufferResize), 1),
      #("transfer", ArrayBufferN(ArrayBufferTransfer), 0),
      #(
        "transferToFixedLength",
        ArrayBufferN(ArrayBufferTransferToFixedLength),
        0,
      ),
    ])
  let #(getters, st) =
    common.alloc_getters(st, fn_proto, [
      #("byteLength", ArrayBufferN(ArrayBufferGetByteLength)),
      #("detached", ArrayBufferN(ArrayBufferGetDetached)),
      #("maxByteLength", ArrayBufferN(ArrayBufferGetMaxByteLength)),
      #("resizable", ArrayBufferN(ArrayBufferGetResizable)),
    ])
  let #(statics, st) =
    common.alloc_methods(st, fn_proto, [
      #("isView", ArrayBufferN(ArrayBufferIsView), 1),
    ])
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      list.append(getters, methods),
      fn(proto) { ArrayBufferN(ArrayBufferConstructor(proto:)) },
      "ArrayBuffer",
      1,
      statics,
    )
  let st = common.add_to_string_tag(st, bt.prototype, "ArrayBuffer")
  let st = common.add_species_accessor(st, fn_proto, bt.constructor, ReturnThis)
  #(bt, st)
}

// ═══════════════════════════════════════════════════════════════════════════
// Dispatch
// ═══════════════════════════════════════════════════════════════════════════

/// Per-module [[Call]] dispatch. `ArrayBuffer()` without `new` throws (§25.1.4.1).
pub fn dispatch(
  st: Agent,
  native: ArrayBufferNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    ArrayBufferConstructor(..) ->
      rt_val.t_throw_type_error(st, "Constructor ArrayBuffer requires 'new'")
    ArrayBufferIsView -> is_view(st, args)
    ArrayBufferGetByteLength -> get_byte_length(st, this)
    ArrayBufferGetDetached -> get_detached(st, this)
    ArrayBufferGetMaxByteLength -> get_byte_length(st, this)
    ArrayBufferGetResizable -> #(mk_bool(False), require_ab_st(st, this))
    ArrayBufferSlice -> slice(st, this, args)
    ArrayBufferResize ->
      rt_val.t_throw_type_error(
        st,
        "ArrayBuffer.prototype.resize called on non-resizable ArrayBuffer",
      )
    ArrayBufferTransfer -> transfer(st, this, args)
    ArrayBufferTransferToFixedLength -> transfer(st, this, args)
  }
}

/// Per-module [[Construct]] dispatch.
pub fn dispatch_construct(
  st: Agent,
  native: ArrayBufferNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    ArrayBufferConstructor(proto:) -> constructor(st, proto, args, new_target)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// ── §25.1.4.1 ArrayBuffer ( length [ , options ] ) ──────────────────────────

fn constructor(
  st: Agent,
  fallback_proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let len_v = helpers.first_arg_or_undefined(args)
  // Step 2: byteLength = ? ToIndex(length).
  let #(byte_len, st) =
    rt_val.t_to_index(st, len_v, "Invalid ArrayBuffer length")
  // Step 3: AllocateArrayBuffer(newTarget, byteLength).
  let #(proto, st) = proto_from_new_target(st, new_target, fallback_proto)
  alloc_buffer(st, proto, byte_len)
}

/// §25.1.3.1 AllocateArrayBuffer — a fresh zero-filled `bytes`.
pub fn alloc_buffer(
  st: Agent,
  proto: Handle,
  byte_len: Int,
) -> #(Handle, Agent) {
  rt_store.t_cell_new(
    st,
    SObject(
      kind: ArrayBufferObj(bytes: <<0:size({ byte_len * 8 })>>, detached: False),
      proto: Some(proto),
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

// ── statics / prototype methods ─────────────────────────────────────────────

/// §25.1.5.1 ArrayBuffer.isView ( arg ) — has [[ViewedArrayBuffer]].
fn is_view(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let v = helpers.first_arg_or_undefined(args)
  let is = case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: TypedArrayObj(..), ..)
        | SObject(kind: DataViewObj(..), ..) -> True
        _ -> False
      }
    _ -> False
  }
  #(mk_bool(is), st)
}

fn get_byte_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(bytes, detached) = require_ab(st, this, "byteLength")
  case detached {
    True -> #(mk_number(JInt(0)), st)
    False -> #(mk_number(JInt(bit_array.byte_size(bytes))), st)
  }
}

fn get_detached(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(_, detached) = require_ab(st, this, "detached")
  #(mk_bool(detached), st)
}

/// §25.1.6.9 ArrayBuffer.prototype.slice ( start, end ).
fn slice(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(bytes, detached) = require_ab(st, this, "slice")
  case detached {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Cannot perform ArrayBuffer.prototype.slice on a detached ArrayBuffer",
      )
    False -> Nil
  }
  let len = bit_array.byte_size(bytes)
  let #(start_v, end_v) = helpers.two_args_or_undefined(args)
  let #(first, st) = clamp_index(st, start_v, len)
  let #(final, st) = case classify(end_v) {
    rt_types.KUndef -> #(len, st)
    _ -> clamp_index(st, end_v, len)
  }
  let new_len = int.max(final - first, 0)
  let assert Ok(sliced) = bit_array.slice(bytes, first, new_len)
  let realm = st.realm
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: ArrayBufferObj(bytes: sliced, detached: False),
        proto: Some(realm.array_buffer.prototype),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(mk_object(h), st)
}

/// §25.1.6.11/12 transfer / transferToFixedLength — copy bytes, detach source.
fn transfer(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let this_h = require_ab_handle(st, this, "transfer")
  let #(bytes, detached) = require_ab(st, this, "transfer")
  case detached {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Cannot perform ArrayBuffer.prototype.transfer on a detached ArrayBuffer",
      )
    False -> Nil
  }
  let old_len = bit_array.byte_size(bytes)
  let #(new_len, st) = case classify(helpers.first_arg_or_undefined(args)) {
    rt_types.KUndef -> #(old_len, st)
    _ ->
      rt_val.t_to_index(
        st,
        helpers.first_arg_or_undefined(args),
        "Invalid ArrayBuffer length",
      )
  }
  let new_bytes = case new_len >= old_len {
    True -> <<bytes:bits, 0:size({ { new_len - old_len } * 8 })>>
    False -> {
      let assert Ok(b) = bit_array.slice(bytes, 0, new_len)
      b
    }
  }
  // Detach source.
  let st =
    rt_store.t_cell_update(st, this_h, fn(slot) {
      case slot {
        SObject(kind: ArrayBufferObj(..), ..) ->
          SObject(..slot, kind: ArrayBufferObj(bytes: <<>>, detached: True))
        _ -> slot
      }
    })
  let realm = st.realm
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: ArrayBufferObj(bytes: new_bytes, detached: False),
        proto: Some(realm.array_buffer.prototype),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(mk_object(h), st)
}

// ── brand checks / helpers ──────────────────────────────────────────────────

fn require_ab(st: Agent, v: JsVal, op: String) -> #(BitArray, Bool) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: ArrayBufferObj(bytes:, detached:), ..) -> #(
          bytes,
          detached,
        )
        _ -> throw_receiver(st, op)
      }
    _ -> throw_receiver(st, op)
  }
}

fn require_ab_handle(st: Agent, v: JsVal, op: String) -> Handle {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: ArrayBufferObj(..), ..) -> h
        _ -> throw_receiver(st, op)
      }
    _ -> throw_receiver(st, op)
  }
}

fn require_ab_st(st: Agent, v: JsVal) -> Agent {
  let _ = require_ab(st, v, "resizable")
  st
}

fn throw_receiver(st: Agent, op: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "Method ArrayBuffer.prototype." <> op <> " called on incompatible receiver",
  )
}

/// Normalize a slice-style relative index to [0, len] (§25.1.6.9 steps 4-8).
fn clamp_index(st: Agent, v: JsVal, len: Int) -> #(Int, Agent) {
  let #(rel, st) = rt_val.t_to_integer_or_infinity(st, v)
  let abs = case rel < 0 {
    True -> int.max(len + rel, 0)
    False -> int.min(rel, len)
  }
  #(abs, st)
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
