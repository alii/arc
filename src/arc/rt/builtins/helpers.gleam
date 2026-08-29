import arc/bytecode/key.{type Key}
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsNum, type JsVal, type ObjKind, type SymbolId,
  BigIntObj, BooleanObj, KBig, KBool, KHandle, KNum, KStr, KSym, NumberObj,
  SObject, StringKey, StringObj, SymbolObj, classify, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/option.{type Option, None, Some}

pub type OwnElement {
  Hit(JsVal)
  Slow
}

@external(erlang, "arc_rt_array_ffi", "own_element")
pub fn own_element(st: Agent, this: JsVal, idx: Int) -> OwnElement

pub fn get_index(st: Agent, this: JsVal, idx: Int) -> #(JsVal, Agent) {
  case own_element(st, this, idx) {
    Hit(v) -> #(v, st)
    Slow -> {
      let #(k, st) = rt_store.t_key_of_int(st, idx)
      rt_obj.t_get_prop(st, this, StringKey(k))
    }
  }
}

@external(erlang, "arc_rt_obj_ffi", "t_get_prop_slow")
fn ffi_get_named(
  st: Agent,
  recv: JsVal,
  key: Key,
  site: Option(Nil),
) -> #(JsVal, Agent)

pub fn get_named(st: Agent, recv: JsVal, key: Key) -> #(JsVal, Agent) {
  ffi_get_named(st, recv, key, None)
}

@external(erlang, "arc_rt_helpers_ffi", "get_symbol_data")
fn get_symbol_data(st: Agent, recv: JsVal, sym: SymbolId) -> JsVal

@external(erlang, "arc_rt_helpers_ffi", "is_miss")
fn is_miss(v: JsVal) -> Bool

pub fn get_symbol(st: Agent, recv: JsVal, sym: SymbolId) -> #(JsVal, Agent) {
  let v = get_symbol_data(st, recv, sym)
  case is_miss(v) {
    True -> rt_obj.t_get_prop(st, recv, rt_types.SymbolKey(sym))
    False -> #(v, st)
  }
}

// strict set, throws on failure
@external(erlang, "arc_rt_obj_ffi", "t_set_prop_named")
pub fn set_named(
  st: Agent,
  obj: JsVal,
  key: Key,
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

pub fn this_string_value(st: Agent, this: JsVal) -> String {
  case classify(this) {
    KStr(s) -> s
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: StringObj(value:), ..) -> value
        _ -> this_value_type_error(st, "String")
      }
    _ -> this_value_type_error(st, "String")
  }
}

pub fn this_number_value(st: Agent, this: JsVal) -> JsNum {
  case classify(this) {
    KNum(n) -> n
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: NumberObj(value:), ..) -> value
        _ -> this_value_type_error(st, "Number")
      }
    _ -> this_value_type_error(st, "Number")
  }
}

pub fn this_boolean_value(st: Agent, this: JsVal) -> Bool {
  case classify(this) {
    KBool(b) -> b
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: BooleanObj(value:), ..) -> value
        _ -> this_value_type_error(st, "Boolean")
      }
    _ -> this_value_type_error(st, "Boolean")
  }
}

pub fn this_symbol_value(st: Agent, this: JsVal) -> SymbolId {
  case classify(this) {
    KSym(id) -> id
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: SymbolObj(value:), ..) -> value
        _ -> this_value_type_error(st, "Symbol")
      }
    _ -> this_value_type_error(st, "Symbol")
  }
}

pub fn this_bigint_value(st: Agent, this: JsVal) -> Int {
  case classify(this) {
    KBig(n) -> n
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: BigIntObj(value:), ..) -> value
        _ -> this_value_type_error(st, "BigInt")
      }
    _ -> this_value_type_error(st, "BigInt")
  }
}

fn this_value_type_error(st: Agent, name: String) -> a {
  rt_val.t_throw_type_error(
    st,
    name <> ".prototype method called on incompatible receiver",
  )
}
