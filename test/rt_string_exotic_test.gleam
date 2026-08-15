//// String exotic objects (§10.4.3) on the arc/rt runtime: the synthesized
//// index and "length" own properties, their fixed attributes, and the
//// [[OwnPropertyKeys]] order.

import arc/rt/builtins as rt_builtins
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type JsVal, type ParsedDesc, DataProperty, Index, JInt, KNum, KStr,
  KUndef, Named, ParsedDesc, StringKey, SymbolKey, canonical_key, classify,
  mk_number, mk_object, mk_string,
}
import gleam/list
import gleam/option.{None, Some}
import rt_helpers

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks())
}

fn key(name: String) {
  StringKey(canonical_key(name))
}

fn int(i: Int) -> JsVal {
  mk_number(JInt(i))
}

/// `new String(s)`.
fn wrapper(st: Agent, s: String) -> #(JsVal, Agent) {
  let #(ctor, st) = rt_obj.t_global_get(st, <<"String">>)
  let #(h, st) = rt_call.t_construct(st, ctor, [mk_string(s)], ctor)
  #(mk_object(h), st)
}

fn handle(v: JsVal) {
  let assert types.KHandle(h) = classify(v)
  h
}

fn value_desc(v: JsVal) -> ParsedDesc {
  ParsedDesc(
    value: Some(v),
    get: None,
    set: None,
    writable: None,
    enumerable: None,
    configurable: None,
  )
}

pub fn index_and_length_descriptors_test() {
  let st = agent()
  let #(s, st) = wrapper(st, "abc")
  let sh = handle(s)
  let #(d, st) = rt_obj.t_get_own_property(st, sh, key("1"))
  let assert Some(DataProperty(
    value:,
    writable: False,
    enumerable: True,
    configurable: False,
    ..,
  )) = d
  assert classify(value) == KStr("b")
  let #(d, st) = rt_obj.t_get_own_property(st, sh, key("3"))
  assert d == None
  let #(d, st) = rt_obj.t_get_own_property(st, sh, key("length"))
  let assert Some(DataProperty(
    value:,
    writable: False,
    enumerable: False,
    configurable: False,
    ..,
  )) = d
  assert classify(value) == KNum(JInt(3))
  // [[Get]] / [[HasProperty]] agree.
  let #(v, st) = rt_obj.t_get_prop(st, s, key("2"))
  assert classify(v) == KStr("c")
  let #(v, st) = rt_obj.t_get_prop(st, s, key("3"))
  assert classify(v) == KUndef
  let #(has, st) = rt_obj.t_has_prop(st, s, key("0"))
  assert has
  let #(has, _) = rt_obj.t_has_prop(st, s, key("3"))
  assert !has
}

pub fn synthesized_properties_are_read_only_test() {
  let st = agent()
  let #(s, st) = wrapper(st, "abc")
  let sh = handle(s)
  let #(ok, st) = rt_obj.t_set_prop(st, s, key("0"), mk_string("z"))
  assert !ok
  let #(ok, st) = rt_obj.t_set_prop(st, s, key("length"), int(9))
  assert !ok
  let #(v, st) = rt_obj.t_get_prop(st, s, key("0"))
  assert classify(v) == KStr("a")
  // Writing through a foreign object with the wrapper as Receiver also
  // reaches the synthesized non-writable descriptor.
  let #(other, st) = rt_obj.t_new_object_literal(st)
  let #(ok, st) =
    rt_obj.t_set_prop_with_receiver(st, handle(other), key("1"), int(1), s)
  assert !ok
  // Out-of-range indices are ordinary.
  let #(ok, st) = rt_obj.t_set_prop(st, s, key("5"), mk_string("x"))
  assert ok
  let #(ok, st) = rt_obj.t_delete_prop(st, sh, key("0"))
  assert !ok
  let #(ok, st) = rt_obj.t_delete_prop(st, sh, key("length"))
  assert !ok
  let #(ok, st) = rt_obj.t_delete_prop(st, sh, key("5"))
  assert ok
  let #(has, _) = rt_obj.t_has_prop(st, s, key("5"))
  assert !has
}

pub fn define_own_property_validates_against_fixed_descriptors_test() {
  let st = agent()
  let #(s, st) = wrapper(st, "abc")
  let sh = handle(s)
  // Same value: a compatible no-op that must not materialize a dict entry.
  let #(ok, st) =
    rt_obj.t_define_own_prop(st, sh, key("0"), value_desc(mk_string("a")))
  assert ok
  let #(ok, st) =
    rt_obj.t_define_own_prop(st, sh, key("0"), value_desc(mk_string("z")))
  assert !ok
  let #(ok, st) =
    rt_obj.t_define_own_prop(st, sh, key("length"), value_desc(int(3)))
  assert ok
  let #(ok, st) =
    rt_obj.t_define_own_prop(st, sh, key("length"), value_desc(int(4)))
  assert !ok
  let widen =
    ParsedDesc(..value_desc(mk_string("a")), value: None, writable: Some(True))
  let #(ok, st) = rt_obj.t_define_own_prop(st, sh, key("0"), widen)
  assert !ok
  let accessor =
    ParsedDesc(..widen, writable: None, get: Some(types.mk_undefined()))
  let #(ok, st) = rt_obj.t_define_own_prop(st, sh, key("1"), accessor)
  assert !ok
  let #(keys, _) = rt_obj.t_own_keys(st, sh)
  assert keys == [key("0"), key("1"), key("2"), key("length")]
}

pub fn own_property_keys_order_test() {
  let st = agent()
  let #(s, st) = wrapper(st, "abc")
  let sh = handle(s)
  let #(_, st) = rt_obj.t_set_prop(st, s, key("foo"), int(1))
  let #(_, st) = rt_obj.t_set_prop(st, s, key("7"), int(2))
  let #(_, st) = rt_obj.t_set_prop(st, s, key("bar"), int(3))
  let sym = types.symbol_iterator
  let #(_, st) = rt_obj.t_set_prop(st, s, SymbolKey(sym), int(4))
  let #(keys, st) = rt_obj.t_own_keys(st, sh)
  // Indices ascending (synthesized and dict-held merged), then the
  // birth-time "length", then named keys in creation order, then symbols.
  assert keys
    == [
      StringKey(Index(0)),
      StringKey(Index(1)),
      StringKey(Index(2)),
      StringKey(Index(7)),
      StringKey(Named("length")),
      StringKey(Named("foo")),
      StringKey(Named("bar")),
      SymbolKey(sym),
    ]
  // for-in / Object.keys see the enumerable string keys only.
  let #(names, st) = rt_obj.t_for_in_keys(st, s)
  assert list.map(names, classify)
    == list.map(["0", "1", "2", "7", "foo", "bar"], KStr)
  let #(object, st) = rt_obj.t_global_get(st, <<"Object">>)
  let #(ks, st) = rt_call.t_call_method(st, object, key("keys"), [s])
  let #(joined, st) =
    rt_call.t_call_method(st, ks, key("join"), [mk_string(",")])
  assert classify(joined) == KStr("0,1,2,7,foo,bar")
  let #(ns, st) =
    rt_call.t_call_method(st, object, key("getOwnPropertyNames"), [s])
  let #(joined, _) =
    rt_call.t_call_method(st, ns, key("join"), [mk_string(",")])
  assert classify(joined) == KStr("0,1,2,7,length,foo,bar")
}

pub fn frozen_string_wrapper_test() {
  let st = agent()
  let #(s, st) = wrapper(st, "ab")
  let #(object, st) = rt_obj.t_global_get(st, <<"Object">>)
  let #(frozen, st) = rt_call.t_call_method(st, object, key("isFrozen"), [s])
  assert classify(frozen) == types.KBool(False)
  let #(_, st) = rt_call.t_call_method(st, object, key("freeze"), [s])
  let #(frozen, st) = rt_call.t_call_method(st, object, key("isFrozen"), [s])
  assert classify(frozen) == types.KBool(True)
  // Indices stay readable after freezing.
  let #(v, _) = rt_obj.t_get_prop(st, s, key("1"))
  assert classify(v) == KStr("b")
}
