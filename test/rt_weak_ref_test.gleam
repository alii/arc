//// §26.1 WeakRef on arc/rt: deref while the target lives, and the GC
//// contract — [[WeakRefTarget]] weak, emptied once the target is swept.

import arc/rt/call as rt_call
import arc/rt/gc as rt_gc
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Handle, type JsVal, KHandle, KUndef, classify, mk_object,
}
import rt_helpers.{agent, call_method, global}

fn handle(v: JsVal) -> Handle {
  let assert KHandle(h) = classify(v)
  h
}

fn new_weak_ref(st: Agent, target: JsVal) -> #(JsVal, Agent) {
  let #(ctor, st) = global(st, "WeakRef")
  let #(h, st) = rt_call.t_construct(st, ctor, [target], ctor)
  #(mk_object(h), st)
}

pub fn deref_returns_target_test() {
  let st = agent()
  let #(target, st) = rt_obj.t_new_object_literal(st)
  let #(ref, st) = new_weak_ref(st, target)
  let #(got, _st) = call_method(st, ref, "deref", [])
  assert got == target
}

pub fn target_is_weak_test() {
  let st = agent()
  let #(target, st) = rt_obj.t_new_object_literal(st)
  let #(ref, st) = new_weak_ref(st, target)
  // Target reachable elsewhere: deref still answers it after a collection.
  let st = rt_gc.t_collect(st, [handle(ref), handle(target)])
  assert rt_gc.t_is_live(st, handle(target))
  let #(got, st) = call_method(st, ref, "deref", [])
  assert got == target
  // Only the WeakRef holds it: the target is swept and the slot emptied,
  // so a fresh object recycling the cell id is never handed out.
  let st = rt_gc.t_collect(st, [handle(ref)])
  assert !rt_gc.t_is_live(st, handle(target))
  let #(_fresh, st) = rt_obj.t_new_object_literal(st)
  let #(got, _st) = call_method(st, ref, "deref", [])
  assert classify(got) == KUndef
}
