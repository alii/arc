import arc/bytecode/key
import arc/rt/builtins as rt_builtins
import arc/rt/call as rt_call
import arc/rt/name_keys as nk
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type JsVal, type ParsedDesc, DataProperty, JInt, KNum, KStr,
  KUndef, ParsedDesc, StringKey, SymbolKey, classify, mk_number, mk_object,
  mk_string,
}
import gleam/list
import gleam/option.{None, Some}
import rt_helpers

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks())
}

fn int(i: Int) -> JsVal {
  mk_number(JInt(i))
}

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
  let #(k_, st) = rt_helpers.key(st, "1")
  let #(d, st) = rt_obj.t_get_own_property(st, sh, k_)
  let assert Some(DataProperty(
    value:,
    writable: False,
    enumerable: True,
    configurable: False,
    ..,
  )) = d
  assert classify(value) == KStr("b")
  let #(k_, st) = rt_helpers.key(st, "3")
  let #(d, st) = rt_obj.t_get_own_property(st, sh, k_)
  assert d == None
  let #(k_, st) = rt_helpers.key(st, "length")
  let #(d, st) = rt_obj.t_get_own_property(st, sh, k_)
  let assert Some(DataProperty(
    value:,
    writable: False,
    enumerable: False,
    configurable: False,
    ..,
  )) = d
  assert classify(value) == KNum(JInt(3))
  let #(k_, st) = rt_helpers.key(st, "2")
  let #(v, st) = rt_obj.t_get_prop(st, s, k_)
  assert classify(v) == KStr("c")
  let #(k_, st) = rt_helpers.key(st, "3")
  let #(v, st) = rt_obj.t_get_prop(st, s, k_)
  assert classify(v) == KUndef
  let #(k_, st) = rt_helpers.key(st, "0")
  let #(has, st) = rt_obj.t_has_prop(st, s, k_)
  assert has
  let #(k_, st) = rt_helpers.key(st, "3")
  let #(has, _) = rt_obj.t_has_prop(st, s, k_)
  assert !has
}

pub fn synthesized_properties_are_read_only_test() {
  let st = agent()
  let #(s, st) = wrapper(st, "abc")
  let sh = handle(s)
  let #(k_, st) = rt_helpers.key(st, "0")
  let #(ok, st) = rt_obj.t_set_prop(st, s, k_, mk_string("z"))
  assert !ok
  let #(k_, st) = rt_helpers.key(st, "length")
  let #(ok, st) = rt_obj.t_set_prop(st, s, k_, int(9))
  assert !ok
  let #(k_, st) = rt_helpers.key(st, "0")
  let #(v, st) = rt_obj.t_get_prop(st, s, k_)
  assert classify(v) == KStr("a")
  let #(other, st) = rt_obj.t_new_object_literal(st)
  let #(k_, st) = rt_helpers.key(st, "1")
  let #(ok, st) =
    rt_obj.t_set_prop_with_receiver(st, handle(other), k_, int(1), s)
  assert !ok
  let #(k_, st) = rt_helpers.key(st, "5")
  let #(ok, st) = rt_obj.t_set_prop(st, s, k_, mk_string("x"))
  assert ok
  let #(k_, st) = rt_helpers.key(st, "0")
  let #(ok, st) = rt_obj.t_delete_prop(st, sh, k_)
  assert !ok
  let #(k_, st) = rt_helpers.key(st, "length")
  let #(ok, st) = rt_obj.t_delete_prop(st, sh, k_)
  assert !ok
  let #(k_, st) = rt_helpers.key(st, "5")
  let #(ok, st) = rt_obj.t_delete_prop(st, sh, k_)
  assert ok
  let #(k_, st) = rt_helpers.key(st, "5")
  let #(has, _) = rt_obj.t_has_prop(st, s, k_)
  assert !has
}

pub fn define_own_property_validates_against_fixed_descriptors_test() {
  let st = agent()
  let #(s, st) = wrapper(st, "abc")
  let sh = handle(s)
  let #(k_, st) = rt_helpers.key(st, "0")
  let #(ok, st) =
    rt_obj.t_define_own_prop(st, sh, k_, value_desc(mk_string("a")))
  assert ok
  let #(k_, st) = rt_helpers.key(st, "0")
  let #(ok, st) =
    rt_obj.t_define_own_prop(st, sh, k_, value_desc(mk_string("z")))
  assert !ok
  let #(k_, st) = rt_helpers.key(st, "length")
  let #(ok, st) = rt_obj.t_define_own_prop(st, sh, k_, value_desc(int(3)))
  assert ok
  let #(k_, st) = rt_helpers.key(st, "length")
  let #(ok, st) = rt_obj.t_define_own_prop(st, sh, k_, value_desc(int(4)))
  assert !ok
  let widen =
    ParsedDesc(..value_desc(mk_string("a")), value: None, writable: Some(True))
  let #(k_, st) = rt_helpers.key(st, "0")
  let #(ok, st) = rt_obj.t_define_own_prop(st, sh, k_, widen)
  assert !ok
  let accessor =
    ParsedDesc(..widen, writable: None, get: Some(types.mk_undefined()))
  let #(k_, st) = rt_helpers.key(st, "1")
  let #(ok, st) = rt_obj.t_define_own_prop(st, sh, k_, accessor)
  assert !ok
  let #(keys, _) = rt_obj.t_own_keys(st, sh)
  assert keys
    == [
      StringKey(key.index(0)),
      StringKey(key.index(1)),
      StringKey(key.index(2)),
      StringKey(nk.length),
    ]
}

pub fn own_property_keys_order_test() {
  let st = agent()
  let #(s, st) = wrapper(st, "abc")
  let sh = handle(s)
  let #(k_, st) = rt_helpers.key(st, "foo")
  let #(_, st) = rt_obj.t_set_prop(st, s, k_, int(1))
  let #(k_, st) = rt_helpers.key(st, "7")
  let #(_, st) = rt_obj.t_set_prop(st, s, k_, int(2))
  let #(k_, st) = rt_helpers.key(st, "bar")
  let #(_, st) = rt_obj.t_set_prop(st, s, k_, int(3))
  let sym = types.symbol_iterator
  let #(_, st) = rt_obj.t_set_prop(st, s, SymbolKey(sym), int(4))
  let #(keys, st) = rt_obj.t_own_keys(st, sh)
  assert keys
    == [
      StringKey(key.index(0)),
      StringKey(key.index(1)),
      StringKey(key.index(2)),
      StringKey(key.index(7)),
      StringKey(nk.length),
      rt_helpers.key(st, "foo").0,
      rt_helpers.key(st, "bar").0,
      SymbolKey(sym),
    ]
  let #(names, st) = rt_obj.t_for_in_keys(st, s)
  assert list.map(names, classify)
    == list.map(["0", "1", "2", "7", "foo", "bar"], KStr)
  let #(object, st) = rt_obj.t_global_get(st, <<"Object">>)
  let #(k_, st) = rt_helpers.key(st, "keys")
  let #(ks, st) = rt_call.t_call_method(st, object, k_, [s])
  let #(k_, st) = rt_helpers.key(st, "join")
  let #(joined, st) = rt_call.t_call_method(st, ks, k_, [mk_string(",")])
  assert classify(joined) == KStr("0,1,2,7,foo,bar")
  let #(k_, st) = rt_helpers.key(st, "getOwnPropertyNames")
  let #(ns, st) = rt_call.t_call_method(st, object, k_, [s])
  let #(k_, st) = rt_helpers.key(st, "join")
  let #(joined, _) = rt_call.t_call_method(st, ns, k_, [mk_string(",")])
  assert classify(joined) == KStr("0,1,2,7,length,foo,bar")
}

pub fn frozen_string_wrapper_test() {
  let st = agent()
  let #(s, st) = wrapper(st, "ab")
  let #(object, st) = rt_obj.t_global_get(st, <<"Object">>)
  let #(k_, st) = rt_helpers.key(st, "isFrozen")
  let #(frozen, st) = rt_call.t_call_method(st, object, k_, [s])
  assert classify(frozen) == types.KBool(False)
  let #(k_, st) = rt_helpers.key(st, "freeze")
  let #(_, st) = rt_call.t_call_method(st, object, k_, [s])
  let #(k_, st) = rt_helpers.key(st, "isFrozen")
  let #(frozen, st) = rt_call.t_call_method(st, object, k_, [s])
  assert classify(frozen) == types.KBool(True)
  let #(k_, st) = rt_helpers.key(st, "1")
  let #(v, _) = rt_obj.t_get_prop(st, s, k_)
  assert classify(v) == KStr("b")
}
