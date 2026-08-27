import arc/rt/call as rt_call
import arc/rt/gc as rt_gc
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, FinalizationRegistryObj, KBool, KHandle,
  SObject, classify, mk_object,
}
import gleam/list
import rt_helpers.{agent, call_method, func, global}

fn handle(v: JsVal) -> Handle {
  let assert KHandle(h) = classify(v)
  h
}

fn new_registry(st: Agent) -> #(JsVal, Agent) {
  let #(ctor, st) = global(st, "FinalizationRegistry")
  let #(cb, st) = func(st, fn(st, _args) { #(types.mk_undefined(), st) })
  let #(h, st) = rt_call.t_construct(st, ctor, [cb], ctor)
  #(mk_object(h), st)
}

fn cell_count(st: Agent, registry: JsVal) -> Int {
  let assert SObject(kind: FinalizationRegistryObj(cells:, ..), ..) =
    rt_store.t_cell_get(st, handle(registry))
  list.length(cells)
}

pub fn register_unregister_test() {
  let st = agent()
  let #(reg, st) = new_registry(st)
  let #(target, st) = rt_obj.t_new_object_literal(st)
  let #(token, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) = call_method(st, reg, "register", [target, token, token])
  let #(_, st) = call_method(st, reg, "register", [target, token])
  assert cell_count(st, reg) == 2
  let #(removed, st) = call_method(st, reg, "unregister", [token])
  assert classify(removed) == KBool(True)
  assert cell_count(st, reg) == 1
  let #(removed, st) = call_method(st, reg, "unregister", [token])
  assert classify(removed) == KBool(False)
  assert cell_count(st, reg) == 1
}

pub fn held_value_strong_target_weak_test() {
  let st = agent()
  let #(reg, st) = new_registry(st)
  let #(target, st) = rt_obj.t_new_object_literal(st)
  let #(held, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) = call_method(st, reg, "register", [target, held])
  let st = rt_gc.t_collect(st, [handle(reg), handle(target)])
  assert rt_gc.t_is_live(st, handle(target))
  assert rt_gc.t_is_live(st, handle(held))
  assert cell_count(st, reg) == 1
  let st = rt_gc.t_collect(st, [handle(reg)])
  assert !rt_gc.t_is_live(st, handle(target))
  assert cell_count(st, reg) == 0
  let st = rt_gc.t_collect(st, [handle(reg)])
  assert !rt_gc.t_is_live(st, handle(held))
}

pub fn dead_token_is_emptied_test() {
  let st = agent()
  let #(reg, st) = new_registry(st)
  let #(target, st) = rt_obj.t_new_object_literal(st)
  let #(token, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) = call_method(st, reg, "register", [target, token, token])
  let st = rt_gc.t_collect(st, [handle(reg), handle(target)])
  assert rt_gc.t_is_live(st, handle(token))
  assert cell_count(st, reg) == 1
  let #(reg2, st) = new_registry(st)
  let #(token2, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) =
    call_method(st, reg2, "register", [target, types.mk_undefined(), token2])
  let st = rt_gc.t_collect(st, [handle(reg2), handle(target)])
  assert !rt_gc.t_is_live(st, handle(token2))
  assert cell_count(st, reg2) == 1
  let #(fresh, st) = rt_obj.t_new_object_literal(st)
  let #(removed, st) = call_method(st, reg2, "unregister", [fresh])
  assert classify(removed) == KBool(False)
  assert cell_count(st, reg2) == 1
}
