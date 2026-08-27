%% worker catches and ships its crash so caller re-raises same class
-module(arc_compile_task_ffi).
-export([run_compile_task/2]).

-define(COMPILE_TASK_THRESHOLD, 262144).         %% bytes
-define(COMPILE_HEAP_WORDS_PER_BYTE, 16).
-define(COMPILE_HEAP_MAX_WORDS, 134217728).      %% ~1GB

run_compile_task(SourceBytes, Task) when SourceBytes < ?COMPILE_TASK_THRESHOLD ->
    Task();
run_compile_task(SourceBytes, Task) ->
    Heap = min(SourceBytes * ?COMPILE_HEAP_WORDS_PER_BYTE,
               ?COMPILE_HEAP_MAX_WORDS),
    Self = self(),
    Ref = make_ref(),
    {Pid, MRef} = spawn_opt(
        fun() ->
            watch_caller(Self, self()),
            Reply = try {ok, Task()}
                    catch Class:Reason:Stack -> {raise, Class, Reason, Stack}
                    end,
            Self ! {Ref, Reply}
        end,
        [monitor, {min_heap_size, Heap}]),
    receive
        {Ref, {ok, Result}} ->
            erlang:demonitor(MRef, [flush]),
            Result;
        {Ref, {raise, Class, Reason, Stack}} ->
            erlang:demonitor(MRef, [flush]),
            erlang:raise(Class, Reason, Stack);
        {'DOWN', MRef, process, Pid, Reason} ->
            erlang:exit(Reason)
    end.

%% kills worker if caller dies mid-compile
watch_caller(Caller, Worker) ->
    spawn(fun() ->
        CallerRef = erlang:monitor(process, Caller),
        WorkerRef = erlang:monitor(process, Worker),
        receive
            {'DOWN', CallerRef, process, Caller, _} -> erlang:exit(Worker, kill);
            {'DOWN', WorkerRef, process, Worker, _} -> ok
        end
    end),
    ok.
