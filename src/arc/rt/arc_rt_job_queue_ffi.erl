%% jobs come off Front and go onto Back, newest first. Fresh is how many of Back
%% came since the last collection, and FreshFront whether any of Front did
-module(arc_rt_job_queue_ffi).
-export([job_queue_new/0, job_queue_push/2, job_queue_pop/1, job_queue_to_list/1,
         job_queue_fresh/1, job_queue_aged/1]).

-define(QUEUE(Front, Back, Fresh, FreshFront), {Front, Back, Fresh, FreshFront}).

job_queue_new() -> ?QUEUE([], [], 0, false).

job_queue_push(?QUEUE(Front, Back, Fresh, FreshFront), Job) ->
    ?QUEUE(Front, [Job | Back], Fresh + 1, FreshFront).

job_queue_pop(?QUEUE([Job | Front], Back, Fresh, FreshFront)) ->
    {some, {Job, ?QUEUE(Front, Back, Fresh, FreshFront)}};
job_queue_pop(?QUEUE([], [], _, _)) -> none;
job_queue_pop(?QUEUE([], [Job], _, _)) -> {some, {Job, job_queue_new()}};
job_queue_pop(?QUEUE([], Back, Fresh, _)) ->
    job_queue_pop(?QUEUE(lists:reverse(Back), [], 0, Fresh > 0)).

job_queue_to_list(?QUEUE(Front, Back, _, _)) -> Front ++ lists:reverse(Back).

job_queue_fresh(?QUEUE(_, Back, Fresh, false)) -> lists:sublist(Back, Fresh);
job_queue_fresh(?QUEUE(Front, Back, _, true)) -> Front ++ Back.

job_queue_aged(?QUEUE(Front, Back, _, _)) -> ?QUEUE(Front, Back, 0, false).
