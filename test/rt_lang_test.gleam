import arc/rt/builtins as rt_builtins
import arc/rt/lang
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, JInt, KHandle, KNum, KStr, KUndef, Named, StringKey, classify,
  mk_number, mk_string, mk_undefined,
}
import gleam/list
import gleam/option.{Some}
import rt_helpers

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks())
}

fn ints(xs: List(Int)) {
  list.map(xs, fn(i) { mk_number(JInt(i)) })
}

pub fn iterator_record_steps_then_stays_done_test() {
  let st = agent()
  let #(arr, st) = rt_obj.t_new_array(st, ints([7, 8]))
  let #(rec, st) = lang.t_get_iterator(st, arr, lang.Sync)
  let #(#(d1, v1), st) = lang.t_iter_next(st, rec)
  let #(#(d2, v2), st) = lang.t_iter_next(st, rec)
  let #(#(d3, v3), st) = lang.t_iter_next(st, rec)
  let #(#(d4, _), st) = lang.t_iter_next(st, rec)
  assert !d1 && !d2 && d3 && d4
  assert classify(v1) == KNum(JInt(7))
  assert classify(v2) == KNum(JInt(8))
  assert classify(v3) == KUndef
  let st = lang.t_iter_close(st, rec, False)
  let _st = lang.t_iter_close(st, rec, True)
}

pub fn iter_rest_and_spread_test() {
  let st = agent()
  let #(arr, st) = rt_obj.t_new_array(st, ints([1, 2, 3]))
  let #(rec, st) = lang.t_get_iterator(st, arr, lang.Sync)
  let #(_, st) = lang.t_iter_next(st, rec)
  let #(rest, st) = lang.t_iter_rest(st, rec)
  let #(len, st) = rt_obj.t_get_prop(st, rest, StringKey(Named("length")))
  assert classify(len) == KNum(JInt(2))
  let #(spread, _st) = lang.t_spread_into_list(st, [mk_string("a")], arr)
  assert list.map(spread, classify)
    == [KStr("a"), KNum(JInt(1)), KNum(JInt(2)), KNum(JInt(3))]
}

pub fn object_rest_excludes_keys_test() {
  let st = agent()
  let #(src_h, st) = rt_obj.t_new_object(st, Some(st.realm.object.prototype))
  let src = types.mk_object(src_h)
  let #(_, st) =
    rt_obj.t_set_prop(st, src, StringKey(Named("a")), mk_string("1"))
  let #(_, st) =
    rt_obj.t_set_prop(st, src, StringKey(Named("b")), mk_string("2"))
  let #(rest, st) = lang.t_object_rest(st, src, [StringKey(Named("a"))])
  let #(a, st) = rt_obj.t_get_prop(st, rest, StringKey(Named("a")))
  let #(b, _st) = rt_obj.t_get_prop(st, rest, StringKey(Named("b")))
  assert classify(a) == KUndef
  assert classify(b) == KStr("2")
}

pub fn template_object_is_cached_per_site_test() {
  let st = agent()
  let cooked = [mk_string("a"), mk_undefined()]
  let #(t1, st) = lang.t_get_template_object(st, "m#11", cooked, ["a", "\\u"])
  let #(t2, st) = lang.t_get_template_object(st, "m#11", cooked, ["a", "\\u"])
  let #(t3, st) = lang.t_get_template_object(st, "m#12", cooked, ["a", "\\u"])
  assert t1 == t2
  assert t1 != t3
  let #(raw, st) = rt_obj.t_get_prop(st, t1, StringKey(Named("raw")))
  let assert KHandle(_) = classify(raw)
  let #(r1, st) = rt_obj.t_get_prop(st, raw, StringKey(types.Index(1)))
  assert classify(r1) == KStr("\\u")
  let #(ok, _st) =
    rt_obj.t_set_prop(st, t1, StringKey(types.Index(0)), mk_string("z"))
  assert !ok
}
