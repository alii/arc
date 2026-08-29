import arc/rt/gc as rt_gc
import arc/rt/obj as rt_obj
import arc/rt/types.{KHandle, classify, mk_undefined}
import rt_helpers

pub fn closure_capture_keeps_object_alive_test() {
  let st = rt_helpers.agent()
  let #(captured, st) = rt_obj.t_new_object_literal(st)
  let assert KHandle(captured_h) = classify(captured)
  let #(f, st) = rt_helpers.func(st, fn(st, _) { #(captured, st) })
  let st = rt_obj.t_global_set(st, "keep", f)
  let st = rt_gc.t_collect(st, [])
  assert rt_gc.t_is_live(st, captured_h)
  let st = rt_obj.t_global_set(st, "keep", mk_undefined())
  let st = rt_gc.t_collect(st, [])
  assert !rt_gc.t_is_live(st, captured_h)
}
