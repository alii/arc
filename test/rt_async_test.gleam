import arc/bytecode/key.{Named}
import arc/host_hooks.{HostHooks}
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/call as rt_call
import arc/rt/class as rt_class
import arc/rt/obj as rt_obj
import arc/rt/ops as rt_ops
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type JsVal, FnFlags, HostJob, JInt, KBool, KHandle, KNum, KStr,
  StringKey, classify, mk_number, mk_object, mk_string, mk_undefined,
}
import gleam/option.{None, Some}
import rt_helpers

fn recording_agent() -> Agent {
  let assert [] = reports()
  rt_builtins.new_agent(
    HostHooks(..rt_helpers.quiet_hooks(), report_uncaught: rt_helpers.record),
  )
}

fn reports() -> List(String) {
  rt_helpers.recorded()
}

fn promise_static(st: Agent, method: String, arg: JsVal) -> #(JsVal, Agent) {
  let #(promise, st) = rt_helpers.global(st, "Promise")
  rt_helpers.call_method(st, promise, method, [arg])
}

fn new_error(st: Agent, msg: String) -> #(JsVal, Agent) {
  let #(error, st) = rt_helpers.global(st, "Error")
  let #(h, st) = rt_call.t_construct(st, error, [mk_string(msg)], error)
  #(mk_object(h), st)
}

fn recorder(st: Agent, label: String) -> #(JsVal, Agent) {
  rt_helpers.func(st, fn(st, _args) {
    rt_helpers.record(label)
    #(mk_undefined(), st)
  })
}

pub fn unhandled_rejection_reported_once_at_drain_end_test() {
  let st = recording_agent()
  let #(e, st) = new_error(st, "boom")
  let #(_, st) = promise_static(st, "reject", e)
  let assert [] = reports()
  let st = rt_async.drain(st)
  assert reports() == ["Uncaught (in promise) Error: boom"]
  let _ = rt_async.drain(st)
  let assert [] = reports()
}

pub fn rejection_handled_later_in_same_drain_not_reported_test() {
  let st = recording_agent()
  let #(p, st) = promise_static(st, "reject", mk_string("late"))
  let #(noop, st) = rt_helpers.func(st, fn(st, _) { #(mk_undefined(), st) })
  let st =
    rt_async.t_enqueue_job(
      st,
      HostJob(fn(st) { rt_helpers.call_method(st, p, "catch", [noop]).1 }),
    )
  let _ = rt_async.drain(st)
  let assert [] = reports()
}

pub fn unhandled_string_rejection_text_test() {
  let st = recording_agent()
  let #(_, st) = promise_static(st, "reject", mk_string("plain"))
  let _ = rt_async.drain(st)
  assert reports() == ["Uncaught (in promise) plain"]
}

pub fn host_job_runs_fifo_with_reaction_jobs_test() {
  let st = recording_agent()
  let #(p, st) = promise_static(st, "resolve", mk_string("v"))
  let #(first, st) = recorder(st, "then1")
  let #(second, st) = recorder(st, "then2")
  let #(_, st) = rt_helpers.call_method(st, p, "then", [first])
  let st =
    rt_async.t_enqueue_job(
      st,
      HostJob(fn(st) {
        rt_helpers.record("host")
        st
      }),
    )
  let #(_, st) = rt_helpers.call_method(st, p, "then", [second])
  let assert [] = reports()
  let _ = rt_async.drain(st)
  assert reports() == ["then1", "host", "then2"]
}

pub fn throwing_host_job_is_reported_test() {
  let st = recording_agent()
  let #(e, st) = new_error(st, "hj")
  let st = rt_async.t_enqueue_job(st, HostJob(rt_store.t_throw(_, e)))
  let _ = rt_async.drain(st)
  assert reports() == ["Uncaught (in promise job) Error: hj"]
}

fn is_extensible(st: Agent, v: JsVal) -> Bool {
  let #(object, st) = rt_helpers.global(st, "Object")
  let #(r, _) = rt_helpers.call_method(st, object, "isExtensible", [v])
  classify(r) == KBool(True)
}

pub fn promise_takes_own_properties_test() {
  let st = rt_helpers.agent()
  let #(p, st) = promise_static(st, "resolve", mk_number(JInt(1)))
  let #(_, st) =
    rt_obj.t_set_prop(st, p, StringKey(Named("tag")), mk_string("t"))
  let #(tag, st) = rt_helpers.get(st, p, "tag")
  assert classify(tag) == KStr("t")
  assert is_extensible(st, p)
  let assert KHandle(ph) = classify(p)
  let #(keys, _) = rt_obj.t_own_keys(st, ph)
  assert keys == [StringKey(Named("tag"))]
}

pub fn promise_subclass_test() {
  let st = rt_helpers.agent()
  let #(promise, st) = rt_helpers.global(st, "Promise")
  let ctor_code =
    // constructor(...args) { super(...args) }
    rt_helpers.as_code(fn(st, frame, args) {
      let assert KHandle(active) = classify(rt_helpers.frame_at(2, frame))
      let new_target = rt_helpers.frame_at(4, frame)
      let #(h, st) = rt_class.t_super_call(st, active, args, new_target)
      #(mk_object(h), st)
    })
  let flags =
    FnFlags(
      is_constructor: True,
      is_class_constructor: True,
      is_derived_constructor: True,
      is_arrow: False,
      is_method: False,
      is_generator: False,
      is_async: False,
      is_strict: True,
    )
  let #(p_ctor_h, st) =
    rt_call.t_fn_new(st, ctor_code, flags, "P", 1, None, None)
  let #(p_proto_h, st) = rt_class.t_class_setup(st, p_ctor_h, promise)
  let p_ctor = mk_object(p_ctor_h)
  let #(executor, st) =
    rt_helpers.func(st, fn(st, args) {
      let assert [resolve, ..] = args
      rt_call.t_call_checked(st, resolve, mk_undefined(), [mk_number(JInt(1))])
    })
  let #(inst_h, st) = rt_call.t_construct(st, p_ctor, [executor], p_ctor)
  let inst = mk_object(inst_h)
  assert rt_obj.t_get_proto(st, inst_h).0 == Some(p_proto_h)
  let #(is_p, st) = rt_ops.t_instance_of(st, inst, p_ctor)
  assert is_p == 1
  let #(noop, st) = rt_helpers.func(st, fn(st, _) { #(mk_undefined(), st) })
  let #(child, st) = rt_helpers.call_method(st, inst, "then", [noop])
  let assert KHandle(child_h) = classify(child)
  assert rt_obj.t_get_proto(st, child_h).0 == Some(p_proto_h)
  let #(seen, st) = recorder(st, "settled")
  let #(_, st) = rt_helpers.call_method(st, inst, "then", [seen])
  let _ = rt_async.drain(st)
  assert reports() == ["settled"]
}

pub fn generator_object_is_extensible_with_own_props_test() {
  let st = rt_helpers.agent()
  let loc = rt_helpers.as_loc(#(mk_string("a"), mk_string("b"), mk_string("d")))
  let #(gen_h, st) =
    rt_async.t_gen_start(
      st,
      rt_helpers.counter_sm(),
      rt_helpers.as_frame(#(
        mk_undefined(),
        mk_undefined(),
        mk_undefined(),
        mk_undefined(),
      )),
      [],
      loc,
    )
  let gen = mk_object(gen_h)
  assert rt_obj.t_get_proto(st, gen_h).0 == Some(st.realm.generator.prototype)
  assert is_extensible(st, gen)
  let #(_, st) =
    rt_obj.t_set_prop(st, gen, StringKey(Named("x")), mk_number(JInt(5)))
  let #(x, st) = rt_helpers.get(st, gen, "x")
  assert classify(x) == KNum(JInt(5))
  let next_value = fn(st) {
    let #(r, st) = rt_helpers.call_method(st, gen, "next", [])
    let #(v, st) = rt_helpers.get(st, r, "value")
    let #(d, st) = rt_helpers.get(st, r, "done")
    #(classify(v), classify(d), st)
  }
  let #(v, d, st) = next_value(st)
  assert v == KStr("a") && d == KBool(False)
  let #(v, d, st) = next_value(st)
  assert v == KStr("b") && d == KBool(False)
  let #(v, d, st) = next_value(st)
  assert v == KStr("d") && d == KBool(True)
  let #(x, _) = rt_helpers.get(st, gen, "x")
  assert classify(x) == KNum(JInt(5))
}
