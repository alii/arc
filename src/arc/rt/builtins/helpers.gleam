//// `rt_builtins/helpers` — argument extraction + brand-check utilities
//// shared by every per-module builtin (SPEC §7.M6 common-and-scaffold(2)).
////
//// Argument helpers + brand checks over the threaded `Agent` model. Errors
//// go through `rt_val.t_throw_type_error` (D7 — raise, never `Result`).

import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsNum, type JsVal, type ObjKind, type SymbolId,
  BigIntObj, BooleanObj, KBig, KBool, KHandle, KNum, KStr, KSym, NumberObj,
  SObject, StringKey, StringObj, SymbolObj, classify, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/option.{type Option, None, Some}

/// A present own element of a plain Array / Arguments object read straight
/// off the heap term, or `Slow` for anything `rt_obj.t_get_own_index` has to
/// look at properly (index override, hole, exotic receiver, primitive).
pub type OwnElement {
  Hit(JsVal)
  Slow
}

@external(erlang, "arc_rt_array_ffi", "own_element")
pub fn own_element(st: Agent, this: JsVal, idx: Int) -> OwnElement

/// §7.3.2 Get on any array-like receiver (including primitive strings) by
/// integer index. A present own element of a plain Array is read directly;
/// everything else goes through `t_get_prop`, which auto-boxes primitives.
pub fn get_index(st: Agent, this: JsVal, idx: Int) -> #(JsVal, Agent) {
  case own_element(st, this, idx) {
    Hit(v) -> #(v, st)
    Slow -> rt_obj.t_get_prop(st, this, StringKey(rt_types.index_key(idx)))
  }
}

/// Get element at index from a list (0-based). O(n).
pub fn list_at(lst: List(a), idx: Int) -> Option(a) {
  case idx, lst {
    0, [x, ..] -> Some(x)
    _, [_, ..rest] if idx > 0 -> list_at(rest, idx - 1)
    _, _ -> None
  }
}

/// The i-th argument, or `undefined` when the caller passed fewer — JS's rule
/// that a missing argument is undefined.
pub fn arg_at(args: List(JsVal), idx: Int) -> JsVal {
  list_at(args, idx) |> option.unwrap(mk_undefined())
}

/// `arg_at(args, 0)` without the walk — the first argument, or `undefined`
/// when there is none.
pub fn first_arg_or_undefined(args: List(JsVal)) -> JsVal {
  case args {
    [v, ..] -> v
    [] -> mk_undefined()
  }
}

/// The first two arguments, each `undefined` when the caller passed fewer.
/// One list match, no walk — the shape most two-parameter builtins want.
pub fn two_args_or_undefined(args: List(JsVal)) -> #(JsVal, JsVal) {
  case args {
    [a, b, ..] -> #(a, b)
    [a] -> #(a, mk_undefined())
    [] -> #(mk_undefined(), mk_undefined())
  }
}

/// The first three arguments, each `undefined` when the caller passed fewer.
pub fn three_args_or_undefined(args: List(JsVal)) -> #(JsVal, JsVal, JsVal) {
  case args {
    [a, b, c, ..] -> #(a, b, c)
    [a, b] -> #(a, b, mk_undefined())
    [a] -> #(a, mk_undefined(), mk_undefined())
    [] -> #(mk_undefined(), mk_undefined(), mk_undefined())
  }
}

// ── control-flow gates (arc helpers.gleam:145-160) ──────────────────────────

/// Bool gate for `use` chains: continue when `cond` holds, else `or_else()`.
/// `use Nil <- helpers.guard(ok, fn() { … })`.
pub fn guard(cond: Bool, or_else: fn() -> r, cont: fn(Nil) -> r) -> r {
  case cond {
    True -> cont(Nil)
    False -> or_else()
  }
}

/// `guard`'s Option twin: continue with the value when present, else
/// `or_else()`. `use v <- helpers.some_or(opt, fn() { type_error(…) })`.
pub fn some_or(opt: Option(a), or_else: fn() -> r, cont: fn(a) -> r) -> r {
  case opt {
    Some(v) -> cont(v)
    None -> or_else()
  }
}

// ── brand checks (arc helpers.gleam:49-90; ported to raise-on-fail D7) ──────

/// The one heap read behind every branded builtin's RequireInternalSlot check:
/// `this` must be an object whose slot kind `extract` recognises. Yields the
/// extracted internal slot alongside the receiver's `Handle`. `require_brand`
/// is this plus the TypeError.
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

/// RequireInternalSlot(this, [[Slot]]) — `this` must be an object carrying the
/// brand `extract` recognises, else RAISE TypeError with `msg()`. `msg` is a
/// thunk so its (concatenated) message costs nothing on the common path.
/// CPS: `use store, ref <- helpers.require_brand(st, this, msg, extract)`.
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

/// `this` must be an object; return its handle or RAISE TypeError naming
/// `name`. The generic step-1 of most `Object.prototype.*` methods.
pub fn require_object(st: Agent, this: JsVal, name: String) -> Handle {
  case classify(this) {
    KHandle(h) -> h
    _ -> rt_val.t_throw_type_error(st, name <> " called on non-object")
  }
}

/// §7.2.3 IsCallable gate — TypeError with `msg()` when `val` isn't callable,
/// otherwise continue with it.
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

/// ES2024 §9.13 CanBeHeldWeakly ( v ) — objects and non-registered Symbols.
/// Pure: registered-ness is on the `SymbolId` itself. arc :102-108.
pub fn can_be_held_weakly(v: JsVal) -> Bool {
  case classify(v) {
    KHandle(_) -> True
    KSym(id) -> !rt_types.is_registered_symbol(id)
    _ -> False
  }
}

// ── ThisFooValue (§21.1.3.7.1 etc.) — primitive-or-wrapper unbox ────────────
// Each returns the underlying primitive value from `this` when `this` is
// either the primitive itself OR a wrapper object with the matching internal
// slot; RAISE TypeError otherwise.

/// §22.1.3 ThisStringValue.
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

/// §21.1.3 ThisNumberValue.
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

/// §20.3.3 ThisBooleanValue.
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

/// §20.4.3 ThisSymbolValue.
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

/// §21.2.3 ThisBigIntValue.
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
