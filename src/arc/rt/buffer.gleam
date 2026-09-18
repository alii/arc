import arc/bytecode/key.{type PropertyKey, Index}
import arc/rt/elements
import arc/rt/store as rt_store
import arc/rt/typed_array_bytes.{
  U8, clamp_uint8, get_float, get_int, set_float, set_int, zeroed,
}
import arc/rt/types.{
  type Agent, type BigIntKind, type BufferStorage, type Handle, type JsElements,
  type JsNum, type JsVal, type NumberKind, type Property, type SabOwner,
  type TypedArrayKind, AccessorProperty, ArgumentsObj, ArrayBufferObj, ArrayObj,
  BigKind, Bytes, DataProperty, Detached, Immutable, JFloat, JInt, JNan, JNegInf,
  JPosInf, KBig, KHandle, KNum, LocalBlock, NumKind, Ordinary, OwnerBlock,
  SObject, SShapedObject, Shared, classify, mk_bigint, mk_int, mk_number,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

@external(erlang, "arc_rt_sab_ffi", "byte_length")
fn byte_length(owner: SabOwner) -> Int

@external(erlang, "arc_rt_sab_ffi", "read")
fn read(owner: SabOwner) -> BitArray

@external(erlang, "arc_rt_sab_ffi", "write")
fn write(owner: SabOwner, byte_offset: Int, chunk: BitArray) -> Nil

pub fn buffer_is_shared(storage: BufferStorage) -> Bool {
  case storage {
    Shared(..) -> True
    Bytes(..) | Immutable(..) | Detached(..) -> False
  }
}

pub fn buffer_is_detached(storage: BufferStorage) -> Bool {
  case storage {
    Detached(..) -> True
    Bytes(..) | Immutable(..) | Shared(..) -> False
  }
}

pub fn buffer_is_immutable(storage: BufferStorage) -> Bool {
  case storage {
    Immutable(..) -> True
    Bytes(..) | Shared(..) | Detached(..) -> False
  }
}

pub fn buffer_max_byte_length(storage: BufferStorage) -> Option(Int) {
  case storage {
    Detached(max_byte_length:)
    | Bytes(max_byte_length:, ..)
    | Shared(max_byte_length:, ..) -> max_byte_length
    Immutable(..) -> None
  }
}

pub fn buffer_byte_size(storage: BufferStorage) -> Int {
  case storage {
    Detached(..) -> 0
    Bytes(bytes:, ..)
    | Immutable(bytes:)
    | Shared(block: LocalBlock(bytes:), ..) -> bit_array.byte_size(bytes)
    Shared(block: OwnerBlock(byte_length:, ..), max_byte_length: None) ->
      byte_length
    Shared(block: OwnerBlock(owner:, ..), max_byte_length: Some(_)) ->
      byte_length(owner)
  }
}

pub fn buffer_bits(storage: BufferStorage) -> Option(BitArray) {
  case storage {
    Detached(..) -> None
    Bytes(bytes:, ..)
    | Immutable(bytes:)
    | Shared(block: LocalBlock(bytes:), ..) -> Some(bytes)
    Shared(block: OwnerBlock(owner:, ..), ..) -> Some(read(owner))
  }
}

pub fn buffer_store_region(
  storage: BufferStorage,
  new_bits: BitArray,
  byte_offset: Int,
  count: Int,
) -> BufferStorage {
  case storage {
    Bytes(bytes: _, max_byte_length:) ->
      Bytes(bytes: new_bits, max_byte_length:)
    Shared(block:, max_byte_length:) -> {
      let assert True =
        byte_offset >= 0
        && count >= 0
        && byte_offset + count <= bit_array.byte_size(new_bits)
        as "buffer_store_region: write range outside the new buffer image"
      case block {
        LocalBlock(_) ->
          Shared(block: LocalBlock(bytes: new_bits), max_byte_length:)
        OwnerBlock(owner:, ..) -> {
          let assert Ok(chunk) = bit_array.slice(new_bits, byte_offset, count)
            as "buffer_store_region: region checked above"
          let Nil = write(owner, byte_offset, chunk)
          storage
        }
      }
    }
    Immutable(..) | Detached(..) -> storage
  }
}

pub fn storage(st: Agent, buffer: Handle) -> Option(BufferStorage) {
  case rt_store.t_cell_get(st, buffer) {
    SObject(kind: ArrayBufferObj(storage:), ..) -> Some(storage)
    _ -> None
  }
}

pub fn bytes(st: Agent, buffer: Handle) -> Option(BitArray) {
  storage(st, buffer) |> option.then(buffer_bits)
}

pub fn is_immutable(st: Agent, buffer: Handle) -> Bool {
  storage(st, buffer)
  |> option.map(buffer_is_immutable)
  |> option.unwrap(False)
}

fn live_byte_size(st: Agent, buffer: Handle) -> Int {
  storage(st, buffer)
  |> option.map(buffer_byte_size)
  |> option.unwrap(0)
}

pub fn set_storage(st: Agent, buffer: Handle, storage: BufferStorage) -> Agent {
  use cell <- rt_store.t_cell_update(st, buffer)
  let assert SObject(kind: ArrayBufferObj(..), ..) = cell
    as "buffer.set_storage: handle does not hold an ArrayBuffer"
  SObject(..cell, kind: ArrayBufferObj(storage:))
}

pub fn store_region(
  st: Agent,
  buffer: Handle,
  new_bits: BitArray,
  byte_offset: Int,
  count: Int,
) -> Agent {
  use cell <- rt_store.t_cell_update(st, buffer)
  let assert SObject(kind: ArrayBufferObj(storage:), ..) = cell
    as "buffer.store_region: handle does not hold an ArrayBuffer"
  SObject(
    ..cell,
    kind: ArrayBufferObj(storage: buffer_store_region(
      storage,
      new_bits,
      byte_offset,
      count,
    )),
  )
}

pub type View {
  View(
    buffer: Handle,
    elem_kind: TypedArrayKind,
    byte_offset: Int,
    length: Option(Int),
  )
}

pub opaque type ViewBounds {
  ViewBounds(byte_size: Int, elem_size: Int, byte_offset: Int, len: Int)
}

pub fn view_bounds(
  byte_size: Int,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> ViewBounds {
  let elem_size = typed_array_bytes.elem_size(elem_kind)
  ViewBounds(
    byte_size:,
    elem_size:,
    byte_offset:,
    len: bounds_len(byte_size, elem_size, byte_offset, length),
  )
}

pub fn fixed_view(
  byte_size: Int,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  len: Int,
) -> ViewBounds {
  ViewBounds(
    byte_size:,
    elem_size: typed_array_bytes.elem_size(elem_kind),
    byte_offset:,
    len:,
  )
}

fn bounds_len(
  byte_size: Int,
  elem_size: Int,
  byte_offset: Int,
  length: Option(Int),
) -> Int {
  case length {
    Some(n) -> n
    None -> int.max(0, { byte_size - byte_offset } / elem_size)
  }
}

pub fn view_len(view: ViewBounds) -> Int {
  view.len
}

pub fn view_element_offset(view: ViewBounds, idx: Int) -> Int {
  view.byte_offset + idx * view.elem_size
}

pub fn view_in_bounds(view: ViewBounds) -> Bool {
  view.byte_offset + view.len * view.elem_size <= view.byte_size
}

pub fn view_length(st: Agent, view: View) -> Int {
  bounds_len(
    live_byte_size(st, view.buffer),
    typed_array_bytes.elem_size(view.elem_kind),
    view.byte_offset,
    view.length,
  )
}

pub fn live_view(st: Agent, view: View) -> Option(ViewBounds) {
  let View(buffer:, elem_kind:, byte_offset:, length:) = view
  use data <- option.map(bytes(st, buffer))
  view_bounds(bit_array.byte_size(data), elem_kind, byte_offset, length)
}

pub fn valid_integer_index(view: ViewBounds, idx: Int) -> Bool {
  idx >= 0 && idx < view.len && view_in_bounds(view)
}

pub fn typed_array_view_length(
  st: Agent,
  buffer: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> Int {
  view_length(st, View(buffer:, elem_kind:, byte_offset:, length:))
}

pub fn typed_array_element(
  st: Agent,
  buffer: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Int,
  idx: Int,
) -> Option(JsVal) {
  use <- bool.guard(idx < 0 || idx >= length, None)
  case bytes(st, buffer) {
    None -> None
    Some(data) ->
      element_of_view(
        data,
        fixed_view(bit_array.byte_size(data), elem_kind, byte_offset, length),
        elem_kind,
        idx,
      )
  }
}

pub fn typed_array_element_live(
  st: Agent,
  buffer: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
  idx: Int,
) -> Option(JsVal) {
  use <- bool.guard(idx < 0, None)
  case bytes(st, buffer) {
    None -> None
    Some(data) ->
      element_of_view(
        data,
        view_bounds(bit_array.byte_size(data), elem_kind, byte_offset, length),
        elem_kind,
        idx,
      )
  }
}

fn element_of_view(
  data: BitArray,
  view: ViewBounds,
  elem_kind: TypedArrayKind,
  idx: Int,
) -> Option(JsVal) {
  case valid_integer_index(view, idx) {
    True ->
      Some(decode_typed_element(data, view_element_offset(view, idx), elem_kind))
    False -> None
  }
}

pub type ViewWitnessError {
  BufferDetached
  OutOfBoundsView
  NotAView
}

pub fn view_witness_error_message(err: ViewWitnessError) -> String {
  case err {
    BufferDetached -> "Cannot perform operation on a detached ArrayBuffer"
    OutOfBoundsView -> "TypedArray is out of bounds"
    NotAView -> "Method invoked on an object that is not a TypedArray"
  }
}

pub fn typed_array_iter_length(
  st: Agent,
  buffer: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> Result(Int, ViewWitnessError) {
  case bytes(st, buffer) {
    None -> Error(BufferDetached)
    Some(data) -> {
      let view =
        view_bounds(bit_array.byte_size(data), elem_kind, byte_offset, length)
      case view_in_bounds(view) {
        False -> Error(OutOfBoundsView)
        True -> Ok(view_len(view))
      }
    }
  }
}

pub fn typed_array_live_length(
  st: Agent,
  buffer: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Int,
) -> Int {
  case bytes(st, buffer) {
    None -> 0
    Some(data) -> {
      let view =
        fixed_view(bit_array.byte_size(data), elem_kind, byte_offset, length)
      case view_in_bounds(view) {
        True -> length
        False -> 0
      }
    }
  }
}

pub fn typed_array_live_count(
  st: Agent,
  buffer: Handle,
  elem_kind: TypedArrayKind,
  byte_offset: Int,
  length: Option(Int),
) -> Int {
  typed_array_iter_length(st, buffer, elem_kind, byte_offset, length)
  |> result.unwrap(0)
}

fn decode_typed_element(
  data: BitArray,
  off: Int,
  elem_kind: TypedArrayKind,
) -> JsVal {
  case elem_kind {
    BigKind(k) ->
      mk_bigint(get_int(data, off, typed_array_bytes.bigint_elem(k)))
    NumKind(_) ->
      case typed_array_bytes.elem_of_kind(elem_kind) {
        typed_array_bytes.Int(e) -> mk_int(get_int(data, off, e))
        typed_array_bytes.Float(e) -> mk_number(get_float(data, off, e))
      }
  }
}

// §7.1.21 canonical numeric index string
pub fn is_canonical_numeric_string(s: String) -> Bool {
  case s {
    "0" <> _
    | "1" <> _
    | "2" <> _
    | "3" <> _
    | "4" <> _
    | "5" <> _
    | "6" <> _
    | "7" <> _
    | "8" <> _
    | "9" <> _
    | "-" <> _
    | "+" <> _
    | "." <> _
    | "I" <> _
    | "N" <> _ ->
      s == "-0" || rt_val.jsnum_to_string(rt_val.string_to_number(s)) == s
    _ -> False
  }
}

fn jsnum_to_store_int(n: JsNum) -> Int {
  case n {
    JInt(i) -> i
    JFloat(f) -> rt_val.float_to_int(f)
    JNan | JPosInf | JNegInf -> 0
  }
}

pub fn typed_array_store(
  st: Agent,
  view: View,
  idx: Option(Int),
  val: JsVal,
) -> #(Bool, Agent) {
  // immutable check must run before coercion so no user code runs
  use <- bool.guard(is_immutable(st, view.buffer), #(False, st))
  case view.elem_kind {
    BigKind(big_kind) -> {
      let #(n, st) = rt_val.t_to_bigint(st, val)
      write_typed_element(st, view, idx, fn(data, off) {
        set_int(data, off, typed_array_bytes.bigint_elem(big_kind), n)
      })
    }
    NumKind(num_kind) -> {
      let #(num, st) = rt_val.t_to_number(st, val)
      write_typed_element(st, view, idx, fn(data, off) {
        encode_typed_number(data, off, num_kind, num)
      })
    }
  }
}

fn write_typed_element(
  st: Agent,
  view: View,
  idx: Option(Int),
  write: fn(BitArray, Int) -> BitArray,
) -> #(Bool, Agent) {
  case idx {
    Some(i) ->
      case rt_store.t_cell_get(st, view.buffer) {
        SObject(kind: ArrayBufferObj(storage:), ..) as cell -> {
          let size = typed_array_bytes.elem_size(view.elem_kind)
          // bounds taken here: coercion may have resized the buffer
          let bounds =
            view_bounds(
              buffer_byte_size(storage),
              view.elem_kind,
              view.byte_offset,
              view.length,
            )
          let off = view_element_offset(bounds, i)
          use <- bool.guard(!valid_integer_index(bounds, i), #(True, st))
          use <- bool.guard(buffer_is_immutable(storage), #(False, st))
          case buffer_bits(storage) {
            None -> #(True, st)
            Some(data) -> {
              let new_bits = write(data, off)
              let new_storage =
                buffer_store_region(storage, new_bits, off, size)
              let st =
                rt_store.t_cell_set(
                  st,
                  view.buffer,
                  SObject(..cell, kind: ArrayBufferObj(storage: new_storage)),
                )
              #(True, st)
            }
          }
        }
        _ -> #(True, st)
      }
    None -> #(True, st)
  }
}

fn encode_typed_number(
  data: BitArray,
  off: Int,
  elem_kind: NumberKind,
  num: JsNum,
) -> BitArray {
  case typed_array_bytes.store_elem_of_kind(NumKind(elem_kind)) {
    typed_array_bytes.StoreClampedU8 -> set_int(data, off, U8, clamp_uint8(num))
    typed_array_bytes.StoreInt(e) ->
      set_int(data, off, e, jsnum_to_store_int(num))
    typed_array_bytes.StoreFloat(e) -> set_float(data, off, e, num)
  }
}

pub type TypedElement {
  NumberElement(kind: NumberKind, num: JsNum)
  BigIntElement(kind: BigIntKind, int: Int)
}

fn element_size(el: TypedElement) -> Int {
  case el {
    NumberElement(kind:, ..) -> typed_array_bytes.elem_size(NumKind(kind))
    BigIntElement(..) -> 8
  }
}

pub fn decoded_element(
  kind: TypedArrayKind,
  val: JsVal,
) -> Option(TypedElement) {
  case kind, classify(val) {
    NumKind(k), KNum(n) -> Some(NumberElement(k, n))
    BigKind(k), KBig(n) -> Some(BigIntElement(k, n))
    _, _ -> None
  }
}

pub fn typed_array_encode_value(
  data: BitArray,
  off: Int,
  el: TypedElement,
) -> BitArray {
  use <- bool.guard(off + element_size(el) > bit_array.byte_size(data), data)
  case el {
    NumberElement(kind:, num:) -> encode_typed_number(data, off, kind, num)
    BigIntElement(kind:, int:) ->
      set_int(data, off, typed_array_bytes.bigint_elem(kind), int)
  }
}

// none when any value needs the observable per-element path
pub fn typed_array_encode_primitives(
  elem_kind: TypedArrayKind,
  values: List(JsVal),
) -> Option(BitArray) {
  let size = typed_array_bytes.elem_size(elem_kind)
  typed_array_encode_primitives_loop(elem_kind, size, values, [])
}

fn typed_array_encode_primitives_loop(
  elem_kind: TypedArrayKind,
  size: Int,
  values: List(JsVal),
  acc: List(BitArray),
) -> Option(BitArray) {
  case values {
    [] -> Some(bit_array.concat(list.reverse(acc)))
    [v, ..rest] -> {
      let seg = case elem_kind, classify(v) {
        BigKind(k), KBig(n) ->
          Some(set_int(zeroed(size), 0, typed_array_bytes.bigint_elem(k), n))
        BigKind(_), _ -> None
        NumKind(_), KHandle(_) -> None
        NumKind(k), _ ->
          case rt_val.prim_to_number(v) {
            Ok(num) -> Some(encode_typed_number(zeroed(size), 0, k, num))
            Error(rt_val.BigIntToNumber)
            | Error(rt_val.SymbolToNumber)
            | Error(rt_val.NeedsToPrimitive) -> None
          }
      }
      case seg {
        Some(s) ->
          typed_array_encode_primitives_loop(elem_kind, size, rest, [s, ..acc])
        None -> None
      }
    }
  }
}

// own data values 0..len-1 without user code, else none
pub fn plain_indexed_values(
  st: Agent,
  h: Handle,
  len: Int,
) -> Option(List(JsVal)) {
  case rt_store.t_cell_get(st, h) {
    SObject(kind:, props:, elements:, ..) ->
      case kind {
        ArrayObj(_) | ArgumentsObj(..) | Ordinary ->
          plain_indexed_values_loop(props, elements, len - 1, [])
        _ -> None
      }
    SShapedObject(..) ->
      case len {
        0 -> Some([])
        _ -> None
      }
    _ -> None
  }
}

fn plain_indexed_values_loop(
  props: dict.Dict(PropertyKey, Property),
  elements: JsElements,
  k: Int,
  acc: List(JsVal),
) -> Option(List(JsVal)) {
  case k < 0 {
    True -> Some(acc)
    False -> {
      let v = case dict.get(props, Index(k)) {
        Ok(DataProperty(value: v, ..)) -> Some(v)
        Ok(AccessorProperty(..)) -> None
        Error(Nil) -> elements.get_option(elements, k)
      }
      case v {
        None -> None
        Some(v) ->
          case classify(v) {
            KHandle(_) -> None
            _ -> plain_indexed_values_loop(props, elements, k - 1, [v, ..acc])
          }
      }
    }
  }
}
