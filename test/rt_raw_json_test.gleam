import arc/rt/builtins as rt_builtins
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type JsVal, DataProperty, KBool, KStr, classify, mk_object,
  mk_string,
}
import gleam/option.{None, Some}
import rt_helpers

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks())
}

fn json(st: Agent, method: String, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(ns, st) = rt_obj.t_global_get(st, <<"JSON">>)
  let #(k_, st) = rt_helpers.key(st, method)
  rt_call.t_call_method(st, ns, k_, args)
}

pub fn raw_json_is_a_brand_not_a_shape_test() {
  let st = agent()
  let #(raw, st) = json(st, "rawJSON", [mk_string("12")])
  let #(is_raw, st) = json(st, "isRawJSON", [raw])
  assert classify(is_raw) == KBool(True)
  let assert types.KHandle(raw_h) = classify(raw)
  let #(proto, st) = rt_obj.t_get_prototype_of(st, raw_h)
  assert proto == None
  let #(k_, st) = rt_helpers.key(st, "rawJSON")
  let #(d, st) = rt_obj.t_get_own_property(st, raw_h, k_)
  let assert Some(DataProperty(
    value:,
    writable: False,
    enumerable: True,
    configurable: False,
    ..,
  )) = d
  assert classify(value) == KStr("12")
  let #(object, st) = rt_obj.t_global_get(st, <<"Object">>)
  let #(k_, st) = rt_helpers.key(st, "isFrozen")
  let #(frozen, st) = rt_call.t_call_method(st, object, k_, [raw])
  assert classify(frozen) == KBool(True)
  let #(fake_h, st) = rt_obj.t_new_object(st, None)
  let #(k_, st) = rt_helpers.key(st, "rawJSON")
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      fake_h,
      k_,
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
  let #(k_, st) = rt_helpers.key(st, "real")
  let #(_, st) = rt_obj.t_set_prop(st, holder, k_, raw)
  let #(k_, st) = rt_helpers.key(st, "fake")
  let #(_, st) = rt_obj.t_set_prop(st, holder, k_, fake)
  let #(out, _) = json(st, "stringify", [holder])
  assert classify(out) == KStr("{\"real\":12,\"fake\":{\"rawJSON\":\"12\"}}")
}
