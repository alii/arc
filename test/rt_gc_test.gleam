import arc/rt/gc as rt_gc
import arc/rt/lang as rt_lang
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Job, type JobQueue, type JsVal, KHandle, ResolveThenableJob,
  classify, job_queue_aged, job_queue_fresh, job_queue_new, job_queue_pop,
  job_queue_push, job_queue_to_list, mk_int, mk_undefined,
}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import rt_helpers

pub fn closure_capture_keeps_object_alive_test() {
  let st = rt_helpers.agent()
  let #(captured, st) = rt_obj.new_object_literal(st)
  let assert KHandle(captured_h) = classify(captured)
  let #(f, st) = rt_helpers.func(st, fn(st, _) { #(captured, st) })
  let st = rt_lang.global_set(st, <<"keep">>, f)
  let st = rt_gc.collect(st, [])
  assert rt_gc.is_live(st, captured_h)
  let st = rt_lang.global_set(st, <<"keep">>, mk_undefined())
  let st = rt_gc.collect(st, [])
  assert !rt_gc.is_live(st, captured_h)
}

fn objects(st: Agent, count: Int) -> #(List(JsVal), Agent) {
  use #(made, st), _ <- int.range(from: 0, to: count, with: #([], st))
  let #(obj, st) = rt_obj.new_object_literal(st)
  #([obj, ..made], st)
}

pub fn pins_made_after_keeping_put_off_the_next_collection_test() {
  let #(kept, st) = objects(rt_helpers.agent(), 20_000)
  let #(kept, st) = rt_obj.new_array(st, kept)
  let st = rt_lang.global_set(st, <<"kept">>, kept) |> rt_gc.keep_all
  let #(pinned, st) = objects(st, 10_000)
  let #(_, st) = rt_gc.hold_roots(st, pinned)
  let st = rt_gc.set_threshold(st, 64) |> rt_gc.collect_some([])
  assert st.gc_at - st.next_id > 1000
}

fn job(n: Int) -> Job {
  let v = mk_int(n)
  ResolveThenableJob(v, v, v, v, v)
}

fn push_all(queue: JobQueue, ns: List(Int)) -> JobQueue {
  list.fold(ns, queue, fn(queue, n) { job_queue_push(queue, job(n)) })
}

pub fn jobs_come_out_in_the_order_they_went_in_test() {
  let queue = push_all(job_queue_new(), [1, 2])
  let assert Some(#(first, queue)) = job_queue_pop(queue)
  let queue = push_all(job_queue_aged(queue), [3])
  assert first == job(1)
  assert job_queue_to_list(queue) == [job(2), job(3)]
  let assert Some(#(_, queue)) = job_queue_pop(queue)
  let assert Some(#(last, queue)) = job_queue_pop(queue)
  assert last == job(3)
  assert job_queue_pop(queue) == None
}

pub fn only_jobs_queued_since_a_collection_are_fresh_test() {
  let queue = push_all(job_queue_new(), [1, 2]) |> job_queue_aged
  assert job_queue_fresh(queue) == []
  let queue = push_all(queue, [3, 4])
  assert job_queue_fresh(queue) == [job(4), job(3)]
}

pub fn fresh_jobs_stay_fresh_as_the_queue_drains_test() {
  let queue = push_all(job_queue_new(), [1]) |> job_queue_aged |> push_all([2])
  let assert Some(#(_, queue)) = job_queue_pop(queue)
  let fresh = job_queue_fresh(push_all(queue, [3]))
  assert list.contains(fresh, job(2))
  assert list.contains(fresh, job(3))
}

@external(erlang, "arc_rt_gc_ffi", "diff_refs")
fn diff_refs(before: Dynamic, after: Dynamic, acc: List(Int)) -> List(Int)

@external(erlang, "rt_helpers_ffi", "improper_list")
fn improper_list(head: JsVal) -> Dynamic

// as a host object may hold
pub fn a_change_to_an_improper_list_is_traced_test() {
  let #(held, _st) = rt_obj.new_object_literal(rt_helpers.agent())
  let assert KHandle(handle) = classify(held)
  assert diff_refs(improper_list(mk_int(1)), improper_list(held), [])
    == [handle.id]
}
