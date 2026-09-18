// spec abstract operations shared by rt and builtins

import arc/bytecode/key.{Named}
import arc/rt/elements
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, ArrayObj, KHandle, KUndef, ProxyObj,
  SObject, StringKey, classify,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list

// §7.3.2 get for an index, own dense elements first
pub fn get_index(st: Agent, this: JsVal, idx: Int) -> #(JsVal, Agent) {
  case elements.own_element(st, this, idx) {
    elements.Hit(v) -> #(v, st)
    elements.Miss -> rt_obj.get_prop(st, this, StringKey(key.index(idx)))
  }
}

// §7.2.2 isarray
pub fn is_array(st: Agent, v: JsVal) -> Bool {
  case classify(v) {
    KHandle(h) -> is_array_handle(st, h)
    _ -> False
  }
}

// §7.2.2, throws on revoked proxy
pub fn is_array_handle(st: Agent, h: Handle) -> Bool {
  case rt_store.cell_get(st, h) {
    SObject(kind: ArrayObj(_), ..) -> True
    SObject(kind: ProxyObj(revoked: True, ..), ..) ->
      rt_val.throw_type_error(
        st,
        "Cannot perform 'IsArray' on a proxy that has been revoked",
      )
    SObject(kind: ProxyObj(target:, ..), ..) -> is_array_handle(st, target)
    _ -> False
  }
}

// §7.3.18 lengthofarraylike
pub fn length_of_array_like(st: Agent, obj: JsVal) -> #(Int, Agent) {
  let #(len_v, st) = rt_obj.get_prop(st, obj, StringKey(Named("length")))
  rt_val.to_length(st, len_v)
}

// relative index clamp shared by slice, at, fill, copyWithin, subarray
pub fn relative_index(
  st: Agent,
  val: JsVal,
  len: Int,
  default: Int,
) -> #(Int, Agent) {
  case classify(val) {
    KUndef -> #(default, st)
    _ -> {
      let #(raw, st) = rt_val.to_integer_or_infinity(st, val)
      let k = case raw < 0 {
        True -> int.max(len + raw, 0)
        False -> int.min(raw, len)
      }
      #(k, st)
    }
  }
}

type ArgList {
  DenseArgs(List(JsVal))
  Miss
}

@external(erlang, "arc_rt_array_ffi", "arg_list")
fn arg_list(st: Agent, arr: JsVal) -> ArgList

// §7.3.20 createlistfromarraylike
pub fn create_list_from_array_like(
  st: Agent,
  arr: JsVal,
) -> #(List(JsVal), Agent) {
  case arg_list(st, arr), classify(arr) {
    DenseArgs(args), _ -> #(args, st)
    Miss, KHandle(h) -> {
      let #(len, st) = case rt_store.cell_get(st, h) {
        SObject(kind: ArrayObj(length:), ..) -> #(length, st)
        _ -> length_of_array_like(st, arr)
      }
      collect_array_like(st, arr, 0, len, [])
    }
    Miss, _ ->
      rt_val.throw_type_error(
        st,
        "CreateListFromArrayLike called on non-object",
      )
  }
}

fn collect_array_like(
  st: Agent,
  arr: JsVal,
  i: Int,
  len: Int,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case i >= len {
    True -> #(list.reverse(acc), st)
    False -> {
      let #(v, st) = get_index(st, arr, i)
      collect_array_like(st, arr, i + 1, len, [v, ..acc])
    }
  }
}
