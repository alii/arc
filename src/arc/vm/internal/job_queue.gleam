/// FIFO queue for promise microtask jobs. Backed by Erlang's `queue`
/// (two-list Okasaki) — O(1) amortized enqueue/dequeue.
import gleam/list
import gleam/option.{type Option}

pub type JobQueue(a)

/// Empty queue.
@external(erlang, "arc_job_queue_ffi", "job_queue_new")
pub fn new() -> JobQueue(a)

/// Enqueue at the back. O(1).
@external(erlang, "arc_job_queue_ffi", "job_queue_push")
pub fn push(queue: JobQueue(a), item: a) -> JobQueue(a)

/// Dequeue from the front. O(1) amortized.
/// Returns None when empty, Some(#(head, rest)) otherwise.
@external(erlang, "arc_job_queue_ffi", "job_queue_pop")
pub fn pop(queue: JobQueue(a)) -> Option(#(a, JobQueue(a)))

/// True when the queue has no items. O(1).
@external(erlang, "arc_job_queue_ffi", "job_queue_is_empty")
pub fn is_empty(queue: JobQueue(a)) -> Bool

/// All queued items in front-to-back order. O(n).
@external(erlang, "arc_job_queue_ffi", "job_queue_to_list")
pub fn to_list(queue: JobQueue(a)) -> List(a)

/// Enqueue a list of items at the back, preserving order. O(k).
pub fn append(queue: JobQueue(a), items: List(a)) -> JobQueue(a) {
  list.fold(items, queue, push)
}
