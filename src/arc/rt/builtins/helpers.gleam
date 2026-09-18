//// small shared helpers for builtin natives

import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type ObjKind, type SymbolId, KHandle,
  KSym, SObject, StringKey, classify, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{type Option, None, Some}

pub type OwnElement {
  Hit(JsVal)
  Miss
}

@external(erlang, "arc_rt_array_ffi", "own_element")
pub fn own_element(st: Agent, this: JsVal, idx: Int) -> OwnElement

pub fn get_index(st: Agent, this: JsVal, idx: Int) -> #(JsVal, Agent) {
  case own_element(st, this, idx) {
    Hit(v) -> #(v, st)
    Miss -> rt_obj.t_get_prop(st, this, StringKey(rt_types.index_key(idx)))
  }
}

pub fn get_named(st: Agent, recv: JsVal, key: String) -> #(JsVal, Agent) {
  rt_val.get_named(st, recv, key)
}

pub fn get_symbol(st: Agent, recv: JsVal, sym: SymbolId) -> #(JsVal, Agent) {
  rt_val.get_symbol(st, recv, sym)
}

// strict set, throws on failure
@external(erlang, "arc_rt_obj_ffi", "t_set_named")
pub fn t_set_named(
  st: Agent,
  obj: JsVal,
  key: String,
  v: JsVal,
  strict: Bool,
) -> Agent

pub fn list_at(lst: List(a), idx: Int) -> Option(a) {
  case idx, lst {
    0, [x, ..] -> Some(x)
    _, [_, ..rest] if idx > 0 -> list_at(rest, idx - 1)
    _, _ -> None
  }
}

pub fn arg_at(args: List(JsVal), idx: Int) -> JsVal {
  case args, idx {
    [v, ..], 0 -> v
    [_, ..rest], _ -> arg_at(rest, idx - 1)
    [], _ -> mk_undefined()
  }
}

pub fn first_arg_or_undefined(args: List(JsVal)) -> JsVal {
  case args {
    [v, ..] -> v
    [] -> mk_undefined()
  }
}

pub fn two_args_or_undefined(args: List(JsVal)) -> #(JsVal, JsVal) {
  case args {
    [a, b, ..] -> #(a, b)
    [a] -> #(a, mk_undefined())
    [] -> #(mk_undefined(), mk_undefined())
  }
}

pub fn three_args_or_undefined(args: List(JsVal)) -> #(JsVal, JsVal, JsVal) {
  case args {
    [a, b, c, ..] -> #(a, b, c)
    [a, b] -> #(a, b, mk_undefined())
    [a] -> #(a, mk_undefined(), mk_undefined())
    [] -> #(mk_undefined(), mk_undefined(), mk_undefined())
  }
}

// threaded list.map, results in input order
pub fn map_threaded(
  st: Agent,
  items: List(a),
  f: fn(Agent, a) -> #(b, Agent),
) -> #(List(b), Agent) {
  let #(rev, st) = {
    use #(acc, st), item <- list.fold(items, #([], st))
    let #(out, st) = f(st, item)
    #([out, ..acc], st)
  }
  #(list.reverse(rev), st)
}

pub fn guard(cond: Bool, or_else: fn() -> r, cont: fn(Nil) -> r) -> r {
  case cond {
    True -> cont(Nil)
    False -> or_else()
  }
}

pub fn some_or(opt: Option(a), or_else: fn() -> r, cont: fn(a) -> r) -> r {
  case opt {
    Some(v) -> cont(v)
    None -> or_else()
  }
}

pub fn brand_of(
  st: Agent,
  this: JsVal,
  extract: fn(ObjKind) -> Option(a),
) -> Option(#(a, Handle)) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind:, ..) ->
          case extract(kind) {
            Some(v) -> Some(#(v, h))
            None -> None
          }
        _ -> None
      }
    _ -> None
  }
}

pub fn require_brand(
  st: Agent,
  this: JsVal,
  msg: fn() -> String,
  extract: fn(ObjKind) -> Option(a),
  cont: fn(a, Handle) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case brand_of(st, this, extract) {
    Some(#(v, h)) -> cont(v, h)
    None -> rt_val.t_throw_type_error(st, msg())
  }
}

pub fn require_object(st: Agent, this: JsVal, name: String) -> Handle {
  case classify(this) {
    KHandle(h) -> h
    _ -> rt_val.t_throw_type_error(st, name <> " called on non-object")
  }
}

pub fn require_callable(
  st: Agent,
  val: JsVal,
  msg: fn() -> String,
  cont: fn(JsVal) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let #(callable, _) = rt_val.t_is_callable(st, val)
  case callable {
    True -> cont(val)
    False -> rt_val.t_throw_type_error(st, msg())
  }
}

pub fn can_be_held_weakly(v: JsVal) -> Bool {
  case classify(v) {
    KHandle(_) -> True
    KSym(id) -> !rt_types.is_registered_symbol(id)
    _ -> False
  }
}
