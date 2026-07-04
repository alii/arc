%% Heap-sized scratch process for parse/compile work — the Erlang half of
%% src/arc/vm/compile_task.gleam. Nothing else lives here: this module owns
%% exactly one capability, the compile fork/join.
%%
%% Run a 0-arity parse/compile task. For big sources the task runs in a
%% short-lived process whose initial heap is sized to the source, for two
%% reasons: parsing allocates large transient structures (token list, AST,
%% IR) that the generational copying GC would otherwise re-copy many times
%% as the live set grows (a 4MB-source eval spent ~3x longer in GC than in
%% actual work), and process death frees everything at once with no sweep.
%% Small sources (the overwhelmingly common case) stay in-process — a spawn
%% plus result copy would cost more than it saves.
%%
%% CRASH FIDELITY. The forked and inline paths must be indistinguishable to
%% the caller: a `panic` inside the task has to raise the SAME exception
%% class either side of the size threshold, or an embedder's try/catch
%% suddenly stops matching once a source crosses 256KB. The worker therefore
%% catches its own crash and ships {Class, Reason, Stacktrace} back as data,
%% and the parent re-raises it with erlang:raise/3. (Letting the crash reach
%% the parent as a monitor DOWN and calling erlang:exit(Reason) does NOT
%% preserve the class: a Gleam `panic` is `error:{gleam_error, ...}` in the
%% child and would resurface as `exit:{gleam_error, ...}` in the parent.)
%% The DOWN arm therefore only fires for a death the worker itself could not
%% observe — killed by an exit signal, or heap/OOM.
%%
%% LIFETIME. The worker is spawned with `link` as well as `monitor`: the
%% caller sits blocked in the receive below for the whole compile, so if the
%% caller dies (killed test worker, embedder shutdown) the worker must die
%% with it rather than run a multi-second parse for a result nobody will
%% ever read. The link's own exit signal to the caller is irrelevant — the
%% caller is already gone — and the worker's crash never propagates over the
%% link because it never crashes: it catches everything.
-module(arc_compile_task_ffi).
-export([run_compile_task/2]).

-define(COMPILE_TASK_THRESHOLD, 262144).         %% bytes
-define(COMPILE_HEAP_WORDS_PER_BYTE, 16).
-define(COMPILE_HEAP_MAX_WORDS, 134217728).      %% 128M words (~1GB)

run_compile_task(SourceBytes, Task) when SourceBytes < ?COMPILE_TASK_THRESHOLD ->
    Task();
run_compile_task(SourceBytes, Task) ->
    Heap = min(SourceBytes * ?COMPILE_HEAP_WORDS_PER_BYTE,
               ?COMPILE_HEAP_MAX_WORDS),
    Self = self(),
    Ref = make_ref(),
    {Pid, MRef} = spawn_opt(
        fun() ->
            Reply = try {ok, Task()}
                    catch Class:Reason:Stack -> {raise, Class, Reason, Stack}
                    end,
            Self ! {Ref, Reply}
        end,
        [link, monitor, {min_heap_size, Heap}]),
    receive
        {Ref, {ok, Result}} ->
            erlang:demonitor(MRef, [flush]),
            Result;
        {Ref, {raise, Class, Reason, Stack}} ->
            erlang:demonitor(MRef, [flush]),
            %% Same class, reason and stack the inline path would have
            %% raised — the threshold is invisible to the caller's catch.
            erlang:raise(Class, Reason, Stack);
        {'DOWN', MRef, process, Pid, Reason} ->
            %% The worker died without getting to reply: an exit signal from
            %% outside, or the runtime tearing it down (max_heap_size, OOM).
            %% Nothing to re-raise faithfully — surface it as an exit.
            erlang:exit(Reason)
    end.
