%% Backing store for the microtask job queue
%% (src/arc/vm/internal/job_queue.gleam) — Erlang's `queue` module (two-list
%% Okasaki FIFO). O(1) amortized in/out vs a List+append O(n) per enqueue.
-module(arc_job_queue_ffi).
-export([job_queue_new/0, job_queue_push/2, job_queue_pop/1]).

job_queue_new() -> queue:new().
job_queue_push(Q, Item) -> queue:in(Item, Q).
job_queue_pop(Q) ->
    case queue:out(Q) of
        {{value, Item}, Q2} -> {some, {Item, Q2}};
        {empty, _} -> none
    end.
