//// JSON.rawJSON boxes carry an [[IsRawJSON]] brand: a look-alike frozen
//// null-prototype `{rawJSON}` object is not one.

import arc/rt/builtins as rt_builtins
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type JsVal, DataProperty, KBool, KStr, StringKey, canonical_key,
  classify, mk_object, mk_string,
}
import gleam/option.{None, Some}
import rt_helpers

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks())
}

fn key(name: String) {
  StringKey(canonical_key(name))
}

fn json(st: Agent, method: String, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(ns, st) = rt_obj.t_global_get(st, <<"JSON">>)
  rt_call.t_call_method(st, ns, key(method), args)
}

pub fn raw_json_is_a_brand_not_a_shape_test() {
  let st = agent()
  let #(raw, st) = json(st, "rawJSON", [mk_string("12")])
  let #(is_raw, st) = json(st, "isRawJSON", [raw])
  assert classify(is_raw) == KBool(True)
  // The box: null prototype, frozen, one non-writable "rawJSON" property.
  let assert types.KHandle(raw_h) = classify(raw)
  let #(proto, st) = rt_obj.t_get_prototype_of(st, raw_h)
  assert proto == None
  let #(d, st) = rt_obj.t_get_own_property(st, raw_h, key("rawJSON"))
  let assert Some(DataProperty(
    value:,
    writable: False,
    enumerable: True,
    configurable: False,
    ..,
  )) = d
  assert classify(value) == KStr("12")
  let #(object, st) = rt_obj.t_global_get(st, <<"Object">>)
  let #(frozen, st) = rt_call.t_call_method(st, object, key("isFrozen"), [raw])
  assert classify(frozen) == KBool(True)
  // A structurally identical impostor is not raw JSON and stringifies as an
  // ordinary object.
  let #(fake_h, st) = rt_obj.t_new_object(st, None)
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      fake_h,
      key("rawJSON"),
      mk_string("12"),
      False,
      True,
      False,
    )
  let #(_, st) = rt_obj.t_prevent_extensions(st, fake_h)
  let fake = mk_object(fake_h)
  let #(is_raw, st) = json(st, "isRawJSON", [fake])
  assert classify(is_raw) == KBool(False)
  let #(holder, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) = rt_obj.t_set_prop(st, holder, key("real"), raw)
  let #(_, st) = rt_obj.t_set_prop(st, holder, key("fake"), fake)
  let #(out, _) = json(st, "stringify", [holder])
  assert classify(out) == KStr("{\"real\":12,\"fake\":{\"rawJSON\":\"12\"}}")
}
