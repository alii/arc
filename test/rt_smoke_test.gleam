import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type JsVal, JInt, KHandle, KNum, KStr, Named, StringKey, classify,
  mk_number, mk_object, mk_string,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{Some}
import rt_helpers

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn t_apply_protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(rt_call.Completion, Agent)

fn agent() -> Agent {
  rt_helpers.agent()
}

pub fn array_join_via_t_call_test() {
  let st = agent()
  let #(arr, st) =
    rt_obj.t_new_array(st, [
      mk_number(JInt(1)),
      mk_number(JInt(2)),
      mk_number(JInt(3)),
    ])
  let #(join, st) = rt_obj.t_get_prop(st, arr, StringKey(Named("join")))
  let assert KHandle(_) = classify(join)
  let #(result, _st) = rt_call.t_call(st, join, arr, [])
  let assert NormalCompletion(v) = result
  assert classify(v) == KStr("1,2,3")
}

pub fn array_length_test() {
  let st = agent()
  let #(arr, st) =
    rt_obj.t_new_array(st, [
      mk_number(JInt(1)),
      mk_number(JInt(2)),
      mk_number(JInt(3)),
    ])
  let #(len, _st) = rt_obj.t_get_prop(st, arr, StringKey(Named("length")))
  assert classify(len) == KNum(JInt(3))
}

pub fn object_set_get_round_trip_test() {
  let st = agent()
  let #(h, st) = rt_obj.t_new_object(st, Some(st.realm.object.prototype))
  let obj = mk_object(h)
  let key = StringKey(Named("greeting"))
  let #(ok, st) = rt_obj.t_set_prop(st, obj, key, mk_string("hi"))
  assert ok
  let #(v, _st) = rt_obj.t_get_prop(st, obj, key)
  assert classify(v) == KStr("hi")
}

pub fn type_error_is_caught_as_throw_completion_test() {
  let st = agent()
  let #(completion, st) =
    t_apply_protected(st, fn(st) { rt_val.t_throw_type_error(st, "boom") })
  let assert ThrowCompletion(err) = completion
  let assert KHandle(_) = classify(err)
  let #(msg, _st) = rt_obj.t_get_prop(st, err, StringKey(Named("message")))
  assert classify(msg) == KStr("boom")
}

@external(erlang, "arc_rt_layout_root_ffi", "slots")
fn slots(vals: List(JsVal)) -> types.ShapeSlots

pub fn shaped_set_transitions_test() {
  let st = agent()
  let proto = st.realm.object.prototype
  let #(a, st) =
    rt_store.t_cell_new(
      st,
      types.SShapedObject(
        shape_id: 0,
        proto: Some(proto),
        slots: slots([]),
        offsets: dict.new(),
      ),
    )
  let #(b, st) =
    rt_store.t_cell_new(
      st,
      types.SShapedObject(
        shape_id: 0,
        proto: Some(proto),
        slots: slots([]),
        offsets: dict.new(),
      ),
    )
  let x = StringKey(Named("x"))
  let y = StringKey(Named("y"))
  let #(ok, st) = rt_obj.t_set_prop(st, mk_object(a), x, mk_number(JInt(1)))
  assert ok
  let #(ok, st) = rt_obj.t_set_prop(st, mk_object(a), y, mk_number(JInt(2)))
  assert ok
  let #(ok, st) = rt_obj.t_set_prop(st, mk_object(b), x, mk_number(JInt(3)))
  assert ok
  let #(ok, st) = rt_obj.t_set_prop(st, mk_object(a), x, mk_number(JInt(4)))
  assert ok
  let assert types.SShapedObject(shape_id: sa, ..) = rt_store.t_cell_get(st, a)
  let assert types.SShapedObject(shape_id: sb, ..) = rt_store.t_cell_get(st, b)
  assert sa == 2
  assert sb == 1
  let #(ax, st) = rt_obj.t_get_prop(st, mk_object(a), x)
  let #(ay, st) = rt_obj.t_get_prop(st, mk_object(a), y)
  let #(bx, st) = rt_obj.t_get_prop(st, mk_object(b), x)
  assert classify(ax) == KNum(JInt(4))
  assert classify(ay) == KNum(JInt(2))
  assert classify(bx) == KNum(JInt(3))
  let #(keys, _st) = rt_obj.t_own_keys(st, a)
  assert keys == [x, y]
}
