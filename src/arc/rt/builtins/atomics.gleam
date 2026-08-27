import arc/rt/async as rt_async
import arc/rt/buffer
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/sab
import arc/rt/store as rt_store
import arc/rt/typed_array_ffi.{
  type IntElem, I16, I32, I64, I8, U16, U32, U64, U8, int_elem_bits,
  int_elem_signed, int_elem_size, ta_get_int, ta_set_int, ta_zeroed,
}
import arc/rt/types.{
  type Agent, type AtomicsNative, type BufferStorage, type Handle, type JsVal,
  type SabOwner, type TypedArrayKind, AtomicsAdd, AtomicsAnd,
  AtomicsCompareExchange, AtomicsExchange, AtomicsIsLockFree, AtomicsLoad,
  AtomicsN, AtomicsNotify, AtomicsOr, AtomicsPause, AtomicsStore, AtomicsSub,
  AtomicsWait, AtomicsWaitAsync, AtomicsXor, BigInt64Kind, BigKind,
  BigUint64Kind, Detached, Int16Kind, Int32Kind, Int8Kind, JFloat, JInt, JNan,
  JNegInf, JPosInf, KHandle, KNum, KUndef, NumKind, OwnerBlock, SObject, Shared,
  TypedArrayObj, Uint16Kind, Uint32Kind, Uint8Kind, classify, mk_bigint, mk_bool,
  mk_number, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}

pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(Handle, Agent) {
  let #(methods, st) =
    common.alloc_methods(st, function_proto, [
      #("add", AtomicsN(AtomicsAdd), 3),
      #("and", AtomicsN(AtomicsAnd), 3),
      #("compareExchange", AtomicsN(AtomicsCompareExchange), 4),
      #("exchange", AtomicsN(AtomicsExchange), 3),
      #("isLockFree", AtomicsN(AtomicsIsLockFree), 1),
      #("load", AtomicsN(AtomicsLoad), 2),
      #("notify", AtomicsN(AtomicsNotify), 3),
      #("or", AtomicsN(AtomicsOr), 3),
      #("pause", AtomicsN(AtomicsPause), 0),
      #("store", AtomicsN(AtomicsStore), 3),
      #("sub", AtomicsN(AtomicsSub), 3),
      #("wait", AtomicsN(AtomicsWait), 4),
      #("waitAsync", AtomicsN(AtomicsWaitAsync), 4),
      #("xor", AtomicsN(AtomicsXor), 3),
    ])
  common.init_namespace(st, object_proto, "Atomics", methods)
}

pub fn dispatch(
  st: Agent,
  native: AtomicsNative,
  _this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    AtomicsAdd -> rmw(st, args, fn(old, v) { old + v })
    AtomicsAnd -> rmw(st, args, int.bitwise_and)
    AtomicsOr -> rmw(st, args, int.bitwise_or)
    AtomicsXor -> rmw(st, args, int.bitwise_exclusive_or)
    AtomicsSub -> rmw(st, args, fn(old, v) { old - v })
    AtomicsExchange -> rmw(st, args, fn(_old, v) { v })
    AtomicsCompareExchange -> compare_exchange(st, args)
    AtomicsLoad -> atomic_load(st, args)
    AtomicsStore -> atomic_store(st, args)
    AtomicsIsLockFree -> is_lock_free(st, args)
    AtomicsNotify -> notify(st, args)
    AtomicsWait -> do_wait(st, args, sync: True)
    AtomicsWaitAsync -> do_wait(st, args, sync: False)
    AtomicsPause -> pause(st, args)
  }
}

type AtomicAccess {
  RmwAccess
  LoadAccess
  WaitAccess
  NotifyAccess
}

type TaInfo {
  TaInfo(
    buffer: Handle,
    elem_kind: TypedArrayKind,
    byte_offset: Int,
    elem: IntElem,
  )
}

fn atomics_elem(kind: TypedArrayKind, waitable: Bool) -> Option(IntElem) {
  case waitable, kind {
    True, NumKind(Int32Kind) -> Some(I32)
    True, BigKind(BigInt64Kind) -> Some(I64)
    True, _ -> None
    False, NumKind(Int8Kind) -> Some(I8)
    False, NumKind(Uint8Kind) -> Some(U8)
    False, NumKind(Int16Kind) -> Some(I16)
    False, NumKind(Uint16Kind) -> Some(U16)
    False, NumKind(Int32Kind) -> Some(I32)
    False, NumKind(Uint32Kind) -> Some(U32)
    False, BigKind(BigInt64Kind) -> Some(I64)
    False, BigKind(BigUint64Kind) -> Some(U64)
    False, _ -> None
  }
}

fn elem_size(info: TaInfo) -> Int {
  int_elem_size(info.elem)
}

// §25.4.3.1 + §25.4.3.3 validate typed array and index
fn with_ta_and_index(
  st: Agent,
  args: List(JsVal),
  mode mode: AtomicAccess,
) -> #(TaInfo, Int, Agent) {
  let waitable = case mode {
    WaitAccess | NotifyAccess -> True
    RmwAccess | LoadAccess -> False
  }
  let require_shared = case mode {
    WaitAccess -> True
    RmwAccess | LoadAccess | NotifyAccess -> False
  }
  let write = case mode {
    RmwAccess -> True
    LoadAccess | WaitAccess | NotifyAccess -> False
  }
  let ta_val = helpers.first_arg_or_undefined(args)
  use view <- helpers.some_or(read_typed_array(st, ta_val), fn() {
    rt_val.t_throw_type_error(
      st,
      "Atomics operation needs an integer TypedArray",
    )
  })
  use elem <- helpers.some_or(atomics_elem(view.elem_kind, waitable), fn() {
    rt_val.t_throw_type_error(
      st,
      "Invalid TypedArray element type for Atomics operation",
    )
  })
  use storage <- helpers.some_or(buffer.buffer_storage(st, view.buffer), fn() {
    rt_val.t_throw_type_error(st, "TypedArray is not attached")
  })
  use Nil <- helpers.guard(
    !require_shared || types.buffer_is_shared(storage),
    fn() {
      rt_val.t_throw_type_error(
        st,
        "Atomics.wait requires a SharedArrayBuffer TypedArray",
      )
    },
  )
  use buf <- helpers.some_or(live_buffer(storage), fn() {
    rt_val.t_throw_type_error(st, "ArrayBuffer is detached")
  })
  use Nil <- helpers.guard(!write || !buf.immutable, fn() {
    rt_val.t_throw_type_error(
      st,
      "Atomics operation cannot write to an immutable ArrayBuffer",
    )
  })
  let size = int_elem_size(elem)
  let avail = { buf.byte_size - view.byte_offset } / size
  let live = int.clamp(avail, 0, view.length)
  let info =
    TaInfo(
      buffer: view.buffer,
      elem_kind: view.elem_kind,
      byte_offset: view.byte_offset,
      elem:,
    )
  let #(idx, st) =
    rt_val.t_to_index(
      st,
      helpers.arg_at(args, 1),
      "Invalid atomic access index",
    )
  use Nil <- helpers.guard(idx < live, fn() {
    rt_val.t_throw_range_error(st, "Atomics access index out of range")
  })
  #(info, idx, st)
}

type TaView {
  TaView(
    buffer: Handle,
    elem_kind: TypedArrayKind,
    byte_offset: Int,
    length: Int,
  )
}

fn read_typed_array(st: Agent, val: JsVal) -> Option(TaView) {
  case classify(val) {
    KHandle(ref) ->
      case rt_store.t_cell_get(st, ref) {
        SObject(
          kind: TypedArrayObj(buffer:, elem_kind:, byte_offset:, length:),
          ..,
        ) ->
          Some(TaView(
            buffer:,
            elem_kind:,
            byte_offset:,
            length: buffer.typed_array_view_length(
              st,
              buffer,
              elem_kind,
              byte_offset,
              length,
            ),
          ))
        _ -> None
      }
    _ -> None
  }
}

type BufferInfo {
  BufferInfo(data: LiveData, byte_size: Int, immutable: Bool)
}

type LiveData {
  StoreData(storage: BufferStorage, bits: BitArray)
  OwnerData(owner: SabOwner)
}

fn live_buffer(storage: BufferStorage) -> Option(BufferInfo) {
  case storage {
    Detached(..) -> None
    Shared(block: OwnerBlock(owner:, ..), ..) ->
      Some(BufferInfo(
        data: OwnerData(owner:),
        byte_size: types.buffer_byte_size(storage),
        immutable: False,
      ))
    _ -> {
      use bits <- option.map(types.buffer_bits(storage))
      BufferInfo(
        data: StoreData(storage:, bits:),
        byte_size: types.buffer_byte_size(storage),
        immutable: types.buffer_is_immutable(storage),
      )
    }
  }
}

// §25.4.3.4 coercion may have detached or shrunk buffer
fn revalidate(st: Agent, info: TaInfo, idx: Int) -> BufferInfo {
  use storage <- helpers.some_or(buffer.buffer_storage(st, info.buffer), fn() {
    rt_val.t_throw_type_error(st, "TypedArray is not attached")
  })
  use buf <- helpers.some_or(live_buffer(storage), fn() {
    rt_val.t_throw_type_error(st, "ArrayBuffer is detached")
  })
  let size = elem_size(info)
  let byte_off = info.byte_offset + idx * size
  use Nil <- helpers.guard(byte_off + size <= buf.byte_size, fn() {
    rt_val.t_throw_range_error(st, "Atomics access index out of range")
  })
  buf
}

// non-finite maps to +0, match before saturating
fn to_operand(st: Agent, info: TaInfo, val: JsVal) -> #(Int, Agent) {
  case info.elem_kind {
    BigKind(_) -> rt_val.t_to_bigint(st, val)
    NumKind(_) -> {
      let #(num, st) = rt_val.t_to_number(st, val)
      case num {
        JPosInf | JNegInf -> #(0, st)
        _ -> #(rt_val.jsnum_to_integer_or_infinity(num), st)
      }
    }
  }
}

fn wrap_to_kind(v: Int, elem: IntElem) -> Int {
  let bits = int_elem_bits(elem)
  let modulus = int.bitwise_shift_left(1, bits)
  let m = int.bitwise_and(v, modulus - 1)
  case int_elem_signed(elem) && m >= modulus / 2 {
    True -> m - modulus
    False -> m
  }
}

fn element_offset(info: TaInfo, idx: Int) -> Int {
  info.byte_offset + idx * elem_size(info)
}

fn element_bytes(info: TaInfo, v: Int) -> BitArray {
  ta_set_int(ta_zeroed(elem_size(info)), 0, info.elem, v)
}

fn read_element(buf: BufferInfo, info: TaInfo, idx: Int) -> Int {
  let off = element_offset(info, idx)
  case buf.data {
    StoreData(bits:, ..) -> ta_get_int(bits, off, info.elem)
    OwnerData(owner:) ->
      ta_get_int(sab.read_part(owner, off, elem_size(info)), 0, info.elem)
  }
}

fn write_element(
  st: Agent,
  info: TaInfo,
  buf: BufferInfo,
  idx: Int,
  v: Int,
) -> Agent {
  let size = elem_size(info)
  let off = element_offset(info, idx)
  case buf.data {
    StoreData(storage:, bits:) ->
      buffer.set_storage(
        st,
        info.buffer,
        types.buffer_store_region(
          storage,
          ta_set_int(bits, off, info.elem, v),
          off,
          size,
        ),
      )
    OwnerData(owner:) -> {
      let Nil = sab.write(owner, off, element_bytes(info, v))
      st
    }
  }
}

fn modify_element(
  st: Agent,
  info: TaInfo,
  buf: BufferInfo,
  idx: Int,
  op: fn(Int) -> Option(Int),
) -> #(Int, Agent) {
  case buf.data {
    StoreData(..) -> {
      let old = read_element(buf, info, idx)
      case op(old) {
        Some(new) -> #(old, write_element(st, info, buf, idx, new))
        None -> #(old, st)
      }
    }
    OwnerData(owner:) -> {
      let old = {
        use old_bits <- sab.update(
          owner,
          element_offset(info, idx),
          elem_size(info),
        )
        let old = ta_get_int(old_bits, 0, info.elem)
        case op(old) {
          Some(new) -> #(old, ta_set_int(old_bits, 0, info.elem, new))
          None -> #(old, old_bits)
        }
      }
      #(old, st)
    }
  }
}

fn element_to_js(info: TaInfo, raw: Int) -> JsVal {
  case info.elem_kind {
    BigKind(_) -> mk_bigint(raw)
    NumKind(_) -> mk_number(JInt(raw))
  }
}

fn rmw(
  st: Agent,
  args: List(JsVal),
  op: fn(Int, Int) -> Int,
) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: RmwAccess)
  let #(operand, st) = to_operand(st, info, helpers.arg_at(args, 2))
  let buf = revalidate(st, info, idx)
  let #(old, st) =
    modify_element(st, info, buf, idx, fn(old) { Some(op(old, operand)) })
  #(element_to_js(info, old), st)
}

fn compare_exchange(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: RmwAccess)
  let #(expected, st) = to_operand(st, info, helpers.arg_at(args, 2))
  let #(replacement, st) = to_operand(st, info, helpers.arg_at(args, 3))
  let buf = revalidate(st, info, idx)
  let wrapped_expected = wrap_to_kind(expected, info.elem)
  let #(old, st) =
    modify_element(st, info, buf, idx, fn(old) {
      case old == wrapped_expected {
        True -> Some(replacement)
        False -> None
      }
    })
  #(element_to_js(info, old), st)
}

fn atomic_load(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: LoadAccess)
  let buf = revalidate(st, info, idx)
  #(element_to_js(info, read_element(buf, info, idx)), st)
}

fn atomic_store(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: RmwAccess)
  case info.elem_kind {
    BigKind(_) -> {
      let #(v, st) = rt_val.t_to_bigint(st, helpers.arg_at(args, 2))
      let buf = revalidate(st, info, idx)
      let st = write_element(st, info, buf, idx, v)
      #(mk_bigint(v), st)
    }
    NumKind(_) -> {
      let #(num, st) = rt_val.t_to_number(st, helpers.arg_at(args, 2))
      let buf = revalidate(st, info, idx)
      let #(stored, ret) = case num {
        JPosInf -> #(0, mk_number(JPosInf))
        JNegInf -> #(0, mk_number(JNegInf))
        _ -> {
          let n = rt_val.jsnum_to_integer_or_infinity(num)
          #(n, mk_number(JInt(n)))
        }
      }
      let st = write_element(st, info, buf, idx, stored)
      #(ret, st)
    }
  }
}

fn is_lock_free(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(n, st) =
    rt_val.t_to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  let ok = case n {
    1 | 2 | 4 | 8 -> True
    _ -> False
  }
  #(mk_bool(ok), st)
}

fn pause(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KUndef -> #(mk_undefined(), st)
    KNum(JInt(_)) -> #(mk_undefined(), st)
    KNum(JFloat(f)) ->
      case rt_val.integral_int(f) {
        Some(_) -> #(mk_undefined(), st)
        None ->
          rt_val.t_throw_type_error(st, "Atomics.pause: not an integral number")
      }
    _ -> rt_val.t_throw_type_error(st, "Atomics.pause: not an integral number")
  }
}

// §25.4.3.14 dowait
fn do_wait(st: Agent, args: List(JsVal), sync sync: Bool) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: WaitAccess)
  let #(v, st) = wait_value(st, info, helpers.arg_at(args, 2))
  let #(timeout_ms, st) = wait_timeout(st, helpers.arg_at(args, 3))
  use Nil <- helpers.guard(!sync || st.hooks.can_block, fn() {
    rt_val.t_throw_type_error(st, "Atomics.wait cannot be called in this agent")
  })
  let _buf = revalidate(st, info, idx)
  let byte_off = element_offset(info, idx)
  let expected = element_bytes(info, v)
  let #(owner, st) = sab.share(st, info.buffer)
  let assert Some(owner) = owner
    as "Atomics.wait: WaitAccess validated a SharedArrayBuffer"
  case sync, timeout_ms {
    True, _ -> {
      let outcome =
        sab.wait_sync(owner, byte_off, expected, option.unwrap(timeout_ms, -1))
      #(rt_async.wait_result_js(outcome), st)
    }
    False, Some(0) -> {
      let live = sab.read_part(owner, byte_off, elem_size(info))
      let outcome = case live == expected {
        True -> rt_async.TimedOut
        False -> rt_async.NotEqual
      }
      wait_result_object(st, False, rt_async.wait_result_js(outcome))
    }
    False, _ -> {
      let deadline =
        option.map(timeout_ms, fn(ms) { st.hooks.monotonic_now() + ms })
      case sab.register_async(st, owner, byte_off, expected, deadline) {
        #(Some(promise), st) -> wait_result_object(st, True, mk_object(promise))
        #(None, st) ->
          wait_result_object(
            st,
            False,
            rt_async.wait_result_js(rt_async.NotEqual),
          )
      }
    }
  }
}

fn wait_value(st: Agent, info: TaInfo, val: JsVal) -> #(Int, Agent) {
  case info.elem_kind {
    BigKind(_) -> {
      let #(n, st) = rt_val.t_to_bigint(st, val)
      #(wrap_to_kind(n, I64), st)
    }
    NumKind(_) -> rt_val.t_to_int32(st, val)
  }
}

fn wait_timeout(st: Agent, val: JsVal) -> #(Option(Int), Agent) {
  let #(num, st) = rt_val.t_to_number(st, val)
  let t = case num {
    JNan | JPosInf -> None
    JNegInf -> Some(0)
    JInt(i) -> Some(int.max(i, 0))
    JFloat(f) ->
      case f <=. 0.0 {
        True -> Some(0)
        False -> Some(float.round(float.ceiling(f)))
      }
  }
  #(t, st)
}

fn wait_result_object(
  st: Agent,
  is_async: Bool,
  value: JsVal,
) -> #(JsVal, Agent) {
  let #(h, st) =
    common.alloc_pojo(st, st.realm.object.prototype, [
      #("async", mk_bool(is_async)),
      #("value", value),
    ])
  #(mk_object(h), st)
}

// §25.4.11
fn notify(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(info, idx, st) = with_ta_and_index(st, args, mode: NotifyAccess)
  let #(count, st) = notify_count(st, helpers.arg_at(args, 2))
  case buffer.buffer_storage(st, info.buffer) {
    Some(Shared(block: OwnerBlock(owner:, ..), ..)) -> {
      let n = sab.notify(owner, element_offset(info, idx), count)
      #(mk_number(JInt(n)), st)
    }
    Some(_) | None -> #(mk_number(JInt(0)), st)
  }
}

fn notify_count(st: Agent, val: JsVal) -> #(Int, Agent) {
  case classify(val) {
    KUndef -> #(rt_val.max_safe_integer, st)
    _ -> {
      let #(n, st) = rt_val.t_to_integer_or_infinity(st, val)
      #(int.max(n, 0), st)
    }
  }
}
