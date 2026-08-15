//// ES2024 §25.3 DataView Objects
////
//// A DataView is a view onto an ArrayBuffer that reads/writes at arbitrary byte
//// offsets in an endian-specified layout, independent of any TypedArray element
//// alignment. Internal storage: `DataViewObj(buffer, offset, len)` exotic kind.
//// Port of arc `builtins/data_view.gleam:38-107` init/dispatch re-expressed
//// under D7 (`Error(e)` → `t_throw`) and R1 (`#(V, St')`).

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type DataViewNative, type Handle, type JsVal,
  type TypedArrayKind, ArrayBufferObj, BigInt64, BigUint64, DataViewConstructor,
  DataViewGet, DataViewGetBuffer, DataViewGetByteLength, DataViewGetByteOffset,
  DataViewN, DataViewObj, DataViewSet, Float32, Float64, Int16, Int32, Int8,
  JInt, KHandle, Named, NoElements, SObject, StringKey, Uint16, Uint32, Uint8,
  Uint8Clamped, classify, mk_bigint, mk_number, mk_object, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{Some}

// ═══════════════════════════════════════════════════════════════════════════
// Init — DataView constructor + DataView.prototype
// ═══════════════════════════════════════════════════════════════════════════

/// Set up DataView constructor + DataView.prototype (§25.3.3/4). DataView.length
/// is 1 (§25.3.3). Prototype is an ordinary object (not a DataView instance).
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(getters, st) =
    common.alloc_getters(st, fn_proto, [
      #("buffer", DataViewN(DataViewGetBuffer)),
      #("byteLength", DataViewN(DataViewGetByteLength)),
      #("byteOffset", DataViewN(DataViewGetByteOffset)),
    ])
  let #(methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("getInt8", DataViewN(DataViewGet(Int8)), 1),
      #("getUint8", DataViewN(DataViewGet(Uint8)), 1),
      #("getInt16", DataViewN(DataViewGet(Int16)), 1),
      #("getUint16", DataViewN(DataViewGet(Uint16)), 1),
      #("getInt32", DataViewN(DataViewGet(Int32)), 1),
      #("getUint32", DataViewN(DataViewGet(Uint32)), 1),
      #("getFloat32", DataViewN(DataViewGet(Float32)), 1),
      #("getFloat64", DataViewN(DataViewGet(Float64)), 1),
      #("getBigInt64", DataViewN(DataViewGet(BigInt64)), 1),
      #("getBigUint64", DataViewN(DataViewGet(BigUint64)), 1),
      #("setInt8", DataViewN(DataViewSet(Int8)), 2),
      #("setUint8", DataViewN(DataViewSet(Uint8)), 2),
      #("setInt16", DataViewN(DataViewSet(Int16)), 2),
      #("setUint16", DataViewN(DataViewSet(Uint16)), 2),
      #("setInt32", DataViewN(DataViewSet(Int32)), 2),
      #("setUint32", DataViewN(DataViewSet(Uint32)), 2),
      #("setFloat32", DataViewN(DataViewSet(Float32)), 2),
      #("setFloat64", DataViewN(DataViewSet(Float64)), 2),
      #("setBigInt64", DataViewN(DataViewSet(BigInt64)), 2),
      #("setBigUint64", DataViewN(DataViewSet(BigUint64)), 2),
    ])
  let proto_props = list.append(getters, methods)
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      proto_props,
      fn(proto) { DataViewN(DataViewConstructor(proto:)) },
      "DataView",
      1,
      [],
    )
  let st = common.add_to_string_tag(st, bt.prototype, "DataView")
  #(bt, st)
}

// ═══════════════════════════════════════════════════════════════════════════
// Dispatch
// ═══════════════════════════════════════════════════════════════════════════

/// Per-module [[Call]] dispatch. `DataView()` without `new` throws (§25.3.2.1).
pub fn dispatch(
  st: Agent,
  native: DataViewNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    DataViewConstructor(..) ->
      rt_val.t_throw_type_error(st, "Constructor DataView requires 'new'")
    DataViewGetBuffer -> get_buffer(st, this)
    DataViewGetByteLength -> get_byte_length(st, this)
    DataViewGetByteOffset -> get_byte_offset(st, this)
    DataViewGet(elem) -> get_view_value(st, this, args, elem)
    DataViewSet(elem) -> set_view_value(st, this, args, elem)
  }
}

/// Per-module [[Construct]] dispatch.
pub fn dispatch_construct(
  st: Agent,
  native: DataViewNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    DataViewConstructor(proto:) -> constructor(st, proto, args, new_target)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// ── §25.3.2.1 DataView ( buffer [ , byteOffset [ , byteLength ] ] ) ─────────

fn constructor(
  st: Agent,
  fallback_proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let #(buf_v, off_v, len_v) = helpers.three_args_or_undefined(args)
  // Step 2: RequireInternalSlot(buffer, [[ArrayBufferData]]).
  let #(buf_h, buf_len) = require_array_buffer(st, buf_v)
  // Step 3: offset = ? ToIndex(byteOffset).
  let #(offset, st) =
    rt_val.t_to_index(st, off_v, "DataView byteOffset out of range")
  // Step 5-6: offset > bufferByteLength → RangeError.
  case offset > buf_len {
    True ->
      rt_val.t_throw_range_error(
        st,
        "Start offset "
          <> int_str(offset)
          <> " is outside the bounds of the buffer",
      )
    False -> Nil
  }
  // Steps 7-9: viewByteLength.
  let #(view_len, st) = case classify(len_v) {
    rt_types.KUndef -> #(buf_len - offset, st)
    _ -> {
      let #(len, st) =
        rt_val.t_to_index(st, len_v, "DataView byteLength out of range")
      case offset + len > buf_len {
        True ->
          rt_val.t_throw_range_error(
            st,
            "Invalid DataView length " <> int_str(len),
          )
        False -> #(len, st)
      }
    }
  }
  // Steps 10-14: OrdinaryCreateFromConstructor + set internal slots.
  let #(proto, st) = proto_from_new_target(st, new_target, fallback_proto)
  rt_store.t_cell_new(
    st,
    SObject(
      kind: DataViewObj(buffer: buf_h, offset:, len: view_len),
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
  let #(buffer, _, _) = require_data_view(st, this, "buffer")
  #(mk_object(buffer), st)
}

fn get_byte_length(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(buffer, _, len) = require_data_view(st, this, "byteLength")
  ensure_not_detached(st, buffer, "byteLength")
  #(mk_number(JInt(len)), st)
}

fn get_byte_offset(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(buffer, offset, _) = require_data_view(st, this, "byteOffset")
  ensure_not_detached(st, buffer, "byteOffset")
  #(mk_number(JInt(offset)), st)
}

// ── §25.3.1.1 GetViewValue / §25.3.1.2 SetViewValue ─────────────────────────

fn get_view_value(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  elem: TypedArrayKind,
) -> #(JsVal, Agent) {
  let #(buffer, view_off, view_len) =
    require_data_view(st, this, "get" <> elem_name(elem))
  let #(idx_v, le_v) = helpers.two_args_or_undefined(args)
  let #(idx, st) =
    rt_val.t_to_index(st, idx_v, "Offset is outside the bounds of the DataView")
  let little = rt_val.to_boolean(le_v)
  let bytes = ensure_not_detached(st, buffer, "get" <> elem_name(elem))
  let elem_size = rt_types.typed_array_elem_size(elem)
  case idx + elem_size > view_len {
    True ->
      rt_val.t_throw_range_error(
        st,
        "Offset is outside the bounds of the DataView",
      )
    False -> Nil
  }
  #(read_elem(bytes, view_off + idx, elem, little), st)
}

fn set_view_value(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  elem: TypedArrayKind,
) -> #(JsVal, Agent) {
  let #(buffer, view_off, view_len) =
    require_data_view(st, this, "set" <> elem_name(elem))
  let #(idx_v, val_v, le_v) = helpers.three_args_or_undefined(args)
  let #(idx, st) =
    rt_val.t_to_index(st, idx_v, "Offset is outside the bounds of the DataView")
  // BigInt64/BigUint64 use ToBigInt; others use ToNumber (§25.3.1.2 step 4).
  let #(raw, st) = coerce_elem_value(st, val_v, elem)
  let little = rt_val.to_boolean(le_v)
  let bytes = ensure_not_detached(st, buffer, "set" <> elem_name(elem))
  let elem_size = rt_types.typed_array_elem_size(elem)
  case idx + elem_size > view_len {
    True ->
      rt_val.t_throw_range_error(
        st,
        "Offset is outside the bounds of the DataView",
      )
    False -> Nil
  }
  let new_bytes = write_elem(bytes, view_off + idx, elem, raw, little)
  let st =
    rt_store.t_cell_update(st, buffer, fn(slot) {
      case slot {
        SObject(kind: ArrayBufferObj(detached:, ..), ..) ->
          SObject(..slot, kind: ArrayBufferObj(bytes: new_bytes, detached:))
        _ -> slot
      }
    })
  #(mk_undefined(), st)
}

// ── brand checks / helpers ──────────────────────────────────────────────────

fn require_data_view(st: Agent, v: JsVal, op: String) -> #(Handle, Int, Int) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: DataViewObj(buffer:, offset:, len:), ..) -> #(
          buffer,
          offset,
          len,
        )
        _ -> throw_receiver(st, op)
      }
    _ -> throw_receiver(st, op)
  }
}

fn require_array_buffer(st: Agent, v: JsVal) -> #(Handle, Int) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: ArrayBufferObj(bytes:, detached:), ..) ->
          case detached {
            True ->
              rt_val.t_throw_type_error(
                st,
                "Cannot construct DataView with a detached ArrayBuffer",
              )
            False -> #(h, bit_array.byte_size(bytes))
          }
        _ ->
          rt_val.t_throw_type_error(
            st,
            "First argument to DataView constructor must be an ArrayBuffer",
          )
      }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "First argument to DataView constructor must be an ArrayBuffer",
      )
  }
}

fn ensure_not_detached(st: Agent, buffer: Handle, op: String) -> BitArray {
  case rt_store.t_cell_get(st, buffer) {
    SObject(kind: ArrayBufferObj(bytes:, detached: False), ..) -> bytes
    _ ->
      rt_val.t_throw_type_error(
        st,
        "Cannot perform DataView.prototype."
          <> op
          <> " on a detached ArrayBuffer",
      )
  }
}

fn throw_receiver(st: Agent, op: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "Method DataView.prototype." <> op <> " called on incompatible receiver",
  )
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

// ── raw byte read/write per §25.1.3.16 RawBytesToNumeric / §25.1.3.18 ───────
// Exported for typed_array.gleam — the same encode/decode backs both views.

pub fn read_elem(
  bytes: BitArray,
  at: Int,
  kind: TypedArrayKind,
  little: Bool,
) -> JsVal {
  case kind, little {
    Int8, _ -> {
      let assert <<_:bytes-size(at), n:signed-8, _:bits>> = bytes
      mk_number(JInt(n))
    }
    Uint8, _ | Uint8Clamped, _ -> {
      let assert <<_:bytes-size(at), n:unsigned-8, _:bits>> = bytes
      mk_number(JInt(n))
    }
    Int16, True -> {
      let assert <<_:bytes-size(at), n:signed-little-16, _:bits>> = bytes
      mk_number(JInt(n))
    }
    Int16, False -> {
      let assert <<_:bytes-size(at), n:signed-big-16, _:bits>> = bytes
      mk_number(JInt(n))
    }
    Uint16, True -> {
      let assert <<_:bytes-size(at), n:unsigned-little-16, _:bits>> = bytes
      mk_number(JInt(n))
    }
    Uint16, False -> {
      let assert <<_:bytes-size(at), n:unsigned-big-16, _:bits>> = bytes
      mk_number(JInt(n))
    }
    Int32, True -> {
      let assert <<_:bytes-size(at), n:signed-little-32, _:bits>> = bytes
      mk_number(JInt(n))
    }
    Int32, False -> {
      let assert <<_:bytes-size(at), n:signed-big-32, _:bits>> = bytes
      mk_number(JInt(n))
    }
    Uint32, True -> {
      let assert <<_:bytes-size(at), n:unsigned-little-32, _:bits>> = bytes
      mk_number(JInt(n))
    }
    Uint32, False -> {
      let assert <<_:bytes-size(at), n:unsigned-big-32, _:bits>> = bytes
      mk_number(JInt(n))
    }
    Float32, True -> {
      let assert <<_:bytes-size(at), n:float-little-32, _:bits>> = bytes
      mk_number(rt_types.JFloat(n))
    }
    Float32, False -> {
      let assert <<_:bytes-size(at), n:float-big-32, _:bits>> = bytes
      mk_number(rt_types.JFloat(n))
    }
    Float64, True -> {
      let assert <<_:bytes-size(at), n:float-little-64, _:bits>> = bytes
      mk_number(rt_types.JFloat(n))
    }
    Float64, False -> {
      let assert <<_:bytes-size(at), n:float-big-64, _:bits>> = bytes
      mk_number(rt_types.JFloat(n))
    }
    BigInt64, True -> {
      let assert <<_:bytes-size(at), n:signed-little-64, _:bits>> = bytes
      mk_bigint(n)
    }
    BigInt64, False -> {
      let assert <<_:bytes-size(at), n:signed-big-64, _:bits>> = bytes
      mk_bigint(n)
    }
    BigUint64, True -> {
      let assert <<_:bytes-size(at), n:unsigned-little-64, _:bits>> = bytes
      mk_bigint(n)
    }
    BigUint64, False -> {
      let assert <<_:bytes-size(at), n:unsigned-big-64, _:bits>> = bytes
      mk_bigint(n)
    }
  }
}

pub fn write_elem(
  bytes: BitArray,
  at: Int,
  kind: TypedArrayKind,
  raw: ElemRaw,
  little: Bool,
) -> BitArray {
  let size = rt_types.typed_array_elem_size(kind)
  let assert <<head:bytes-size(at), _:bytes-size(size), tail:bits>> = bytes
  // Construction: `signed`/`unsigned` are pattern-only; construction takes the
  // Int modulo 2^width regardless (BEAM bit-syntax semantics), so signedness is
  // irrelevant on the write side.
  let mid = case kind, raw, little {
    Int8, RawInt(n), _ | Uint8, RawInt(n), _ | Uint8Clamped, RawInt(n), _ -> <<
      n:8,
    >>
    Int16, RawInt(n), True | Uint16, RawInt(n), True -> <<n:little-16>>
    Int16, RawInt(n), False | Uint16, RawInt(n), False -> <<n:big-16>>
    Int32, RawInt(n), True | Uint32, RawInt(n), True -> <<n:little-32>>
    Int32, RawInt(n), False | Uint32, RawInt(n), False -> <<n:big-32>>
    Float32, RawFloat(n), True -> <<n:float-little-32>>
    Float32, RawFloat(n), False -> <<n:float-big-32>>
    Float64, RawFloat(n), True -> <<n:float-little-64>>
    Float64, RawFloat(n), False -> <<n:float-big-64>>
    BigInt64, RawInt(n), True | BigUint64, RawInt(n), True -> <<n:little-64>>
    BigInt64, RawInt(n), False | BigUint64, RawInt(n), False -> <<n:big-64>>
    _, _, _ -> <<0:size({ size * 8 })>>
  }
  <<head:bits, mid:bits, tail:bits>>
}

pub type ElemRaw {
  RawInt(Int)
  RawFloat(Float)
}

/// §25.3.1.2 step 4: ToBigInt for BigInt64/BigUint64, ToNumber otherwise; then
/// integer-truncate for the int kinds (via ToInt32/ToUint32-style modulo wrap
/// applied by the bit-pattern write itself — BEAM `<<N:size-S>>` truncates).
pub fn coerce_elem_value(
  st: Agent,
  v: JsVal,
  kind: TypedArrayKind,
) -> #(ElemRaw, Agent) {
  case kind {
    BigInt64 | BigUint64 -> {
      let #(n, st) = rt_val.t_to_bigint(st, v)
      #(RawInt(n), st)
    }
    Float32 | Float64 -> {
      let #(n, st) = rt_val.t_to_number(st, v)
      #(RawFloat(jsnum_as_float(n)), st)
    }
    Int8 | Uint8 | Uint8Clamped | Int16 | Uint16 | Int32 | Uint32 -> {
      let #(n, st) = rt_val.t_to_number(st, v)
      #(RawInt(rt_val.jsnum_to_integer_or_infinity(n)), st)
    }
  }
}

/// A `JsNum` as a bare BEAM Float. NaN/±Inf are unrepresentable in Erlang's
/// float term — arc routes those through `arc_typed_array_ffi`; here they
/// collapse to 0.0 pending §10 FFI copy (`arc_rt_typed_array_ffi.erl`).
fn jsnum_as_float(n: rt_types.JsNum) -> Float {
  case n {
    JInt(i) -> int_to_float(i)
    rt_types.JFloat(f) -> f
    rt_types.JNan | rt_types.JPosInf | rt_types.JNegInf -> 0.0
  }
}

@external(erlang, "erlang", "float")
fn int_to_float(n: Int) -> Float

fn elem_name(kind: TypedArrayKind) -> String {
  case kind {
    Int8 -> "Int8"
    Uint8 -> "Uint8"
    Uint8Clamped -> "Uint8"
    Int16 -> "Int16"
    Uint16 -> "Uint16"
    Int32 -> "Int32"
    Uint32 -> "Uint32"
    Float32 -> "Float32"
    Float64 -> "Float64"
    BigInt64 -> "BigInt64"
    BigUint64 -> "BigUint64"
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_str(n: Int) -> String
