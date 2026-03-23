-module(arc_test_ffi).
-export([main/0]).

%% Custom test harness — no EUnit, pure BEAM parallelism.
%% All tests (unit tests + test262 files) run in one flat pool.
main() ->
    %% Discover test modules
    GleamFiles = filelib:wildcard("**/*.gleam", "test"),
    ErlFiles = filelib:wildcard("**/*.erl", "test"),
    GleamModules = [gleam_to_erl_module(F) || F <- GleamFiles],
    ErlModules = [erl_to_module(F) || F <- ErlFiles],
    AllModules = lists:usort(GleamModules ++ ErlModules),

    %% Exclude non-test modules and test262_exec (handled separately)
    Excluded = [arc_test_ffi, test262_exec_ffi, test_runner_ffi, test262_exec],
    TestModules = [M || M <- AllModules,
                        not lists:member(M, Excluded),
                        has_test_functions(M)],

    %% Collect unit test functions: {Name, Fun}
    UnitTests = lists:flatmap(fun(M) ->
        [{format_test_name(M, F), fun() -> M:F(), ok end}
         || {F, 0} <- M:module_info(exports),
            is_test_function(F)]
    end, TestModules),

    %% If TEST262_EXEC=1, add test262 files as individual tests.
    %% TEST262_FILTER=path/prefix filters to only matching files.
    {Test262Tests, HasTest262} = case os:getenv("TEST262_EXEC") of
        false -> {[], false};
        "" -> {[], false};
        _ ->
            test262_exec:init(),
            AllFiles = test262_exec:list_files(),
            Filter = case os:getenv("TEST262_FILTER") of
                false -> <<>>;
                Val -> list_to_binary(Val)
            end,
            Files = case Filter of
                <<>> -> AllFiles;
                _ -> [F || F <- AllFiles, binary:match(F, Filter) =/= nomatch]
            end,
            Tests = [{<<"test262/", F/binary>>, fun() ->
                case test262_exec:run_file(F) of
                    {ok, nil} -> ok;
                    {error, Reason} -> error(Reason)
                end
            end} || F <- Files],
            {Tests, true}
    end,

    AllTests = UnitTests ++ Test262Tests,
    Total = length(AllTests),

    ModuleCount = length(TestModules) + case HasTest262 of true -> 1; false -> 0 end,
    io:format("Running ~b tests across ~b modules~n", [Total, ModuleCount]),

    %% Spawn tests with bounded concurrency — avoids 53k processes fighting
    %% for 16 scheduler threads. Each running test gets meaningful CPU time
    %% instead of being preempted thousands of times.
    Parent = self(),
    Ref = make_ref(),
    T0 = erlang:monotonic_time(millisecond),
    MaxWorkers = erlang:system_info(schedulers_online) * 8,
    spawn_link(fun() -> feeder(AllTests, Parent, Ref, MaxWorkers) end),

    %% Collect results with live progress + stall detection
    Pending = maps:from_list([{Name, true} || {Name, _} <- AllTests]),
    {Passed, Failed} = collect(Total, Ref, 0, [], Total, Pending),
    clear_line(),
    T1 = erlang:monotonic_time(millisecond),
    Elapsed = T1 - T0,

    %% If test262 was enabled, call finish to print summary + write snapshot
    Test262Errors = [{binary_to_list(N), to_list(R)}
                     || {N, _Class, R, _Stack} <- Failed,
                        is_binary(N),
                        binary:match(N, <<"test262/">>) =/= nomatch],
    case HasTest262 of
        true ->
            case test262_exec:finish(Test262Errors) of
                {ok, nil} -> ok;
                {error, _Reason} -> ok  %% failures already counted
            end;
        false -> ok
    end,

    %% Split test262 failures from unit-test failures. Unit-test failures
    %% always fail the build. test262 failures are snapshot mismatches
    %% (REGRESSION or NEW PASS) and fail the build UNLESS in
    %% UPDATE_SNAPSHOT mode — the snapshot was already written, exit 0
    %% lets the dev commit it without a second run.
    %%
    %% Harness-level timeouts/heap-kills bypass run_file's snapshot check.
    %% Only count those if the test WAS in the snapshot (= regression).
    %% run_file errors (REGRESSION / NEW PASS) always count — the snapshot
    %% check already happened there.
    {T262Raw, NonT262Failed} = lists:partition(fun({N, _, _, _}) ->
        is_binary(N) andalso binary:match(N, <<"test262/">>) =/= nomatch
    end, Failed),
    T262Failed = case HasTest262 of
        false -> T262Raw;
        true -> [F || {<<"test262/", Path/binary>>, _, R, _} = F <- T262Raw,
                      case R of
                          test_timeout -> test262_exec_ffi:snapshot_contains(Path);
                          heap_limit_exceeded -> test262_exec_ffi:snapshot_contains(Path);
                          _ -> true
                      end]
    end,
    lists:foreach(fun({Name, Class, Reason, Stack}) ->
        io:format("~n  FAIL ~ts~n", [Name]),
        print_failure(Class, Reason, Stack)
    end, NonT262Failed),
    UpdateMode = case os:getenv("UPDATE_SNAPSHOT") of
        false -> false;
        "" -> false;
        _ -> true
    end,
    case {T262Failed, UpdateMode} of
        {[], _} -> ok;
        {_, true} ->
            io:format("~n  ~b test262 timeout(s)/error(s) during snapshot "
                      "update — these are not in the snapshot.~n",
                      [length(T262Failed)]);
        {_, false} ->
            io:format("~n  ~b test262 snapshot mismatch(es) — "
                      "investigate, then UPDATE_SNAPSHOT=1 to accept:~n",
                      [length(T262Failed)]),
            lists:foreach(fun({Name, _, Reason, _}) ->
                io:format("    ~ts: ~ts~n", [Name, to_list(Reason)])
            end, lists:sublist(T262Failed, 20)),
            case length(T262Failed) > 20 of
                true -> io:format("    ... and ~b more~n", [length(T262Failed) - 20]);
                false -> ok
            end
    end,

    FailCount = length(NonT262Failed) + case UpdateMode of
        true -> 0;
        false -> length(T262Failed)
    end,
    io:format("~n~b passed, ~b failed (~.1fs)~n", [Passed, FailCount, Elapsed / 1000.0]),

    case FailCount of
        0 -> erlang:halt(0);
        _ -> erlang:halt(1)
    end.

%% --- Helpers ---

%% Bounded-concurrency feeder: spawns up to MaxWorkers tests at a time,
%% spawning a new one each time a worker finishes. Uses spawn_link +
%% trap_exit so crashed workers are detected instead of silently lost.
feeder(Tests, Parent, Ref, MaxWorkers) ->
    process_flag(trap_exit, true),
    FeedRef = make_ref(),
    Self = self(),
    {Initial, Rest} = take(Tests, MaxWorkers),
    PidMap = maps:from_list(
        [{spawn_worker(T, Parent, Ref, Self, FeedRef), element(1, T)}
         || T <- Initial]),
    feeder_loop(Rest, Parent, Ref, Self, FeedRef, length(Initial), PidMap).

feeder_loop(_Remaining, _Parent, _Ref, _Self, _FeedRef, 0, _PidMap) -> ok;
feeder_loop(Remaining, Parent, Ref, Self, FeedRef, Active, PidMap) ->
    receive
        {FeedRef, done} ->
            case Remaining of
                [{_Name, _Fun} = T | Rest] ->
                    Pid = spawn_worker(T, Parent, Ref, Self, FeedRef),
                    feeder_loop(Rest, Parent, Ref, Self, FeedRef, Active,
                                maps:put(Pid, element(1, T), PidMap));
                [] ->
                    feeder_loop([], Parent, Ref, Self, FeedRef, Active - 1, PidMap)
            end;
        {'EXIT', _Pid, normal} ->
            %% Worker exited normally — results already sent via messages
            feeder_loop(Remaining, Parent, Ref, Self, FeedRef, Active, PidMap);
        {'EXIT', Pid, Reason} ->
            %% Worker crashed before sending results — report failure and free slot
            case maps:find(Pid, PidMap) of
                {ok, Name} ->
                    Parent ! {Ref, Name, {error, {exit, Reason, []}}},
                    NewPidMap = maps:remove(Pid, PidMap),
                    case Remaining of
                        [{_N, _F} = T | Rest] ->
                            NewPid = spawn_worker(T, Parent, Ref, Self, FeedRef),
                            feeder_loop(Rest, Parent, Ref, Self, FeedRef, Active,
                                        maps:put(NewPid, element(1, T), NewPidMap));
                        [] ->
                            feeder_loop([], Parent, Ref, Self, FeedRef, Active - 1, NewPidMap)
                    end;
                error ->
                    feeder_loop(Remaining, Parent, Ref, Self, FeedRef, Active, PidMap)
            end
    end.

spawn_worker({Name, Fun}, Parent, Ref, Feeder, FeedRef) ->
    spawn_link(fun() ->
        %% Run the test in a sub-process with a 10s timeout.
        %% If it hangs or uses >512MB, we kill it and report a failure.
        Self = self(),
        TestRef = make_ref(),
        process_flag(trap_exit, true),
        Pid = spawn_link(fun() ->
            %% Limit heap to ~80MB to prevent pathological tests
            %% (e.g. Array(2**32-1).join()) from consuming all RAM on CI.
            process_flag(max_heap_size, #{size => 10000000, kill => true, error_logger => false}),
            Res = try Fun(), ok
            catch Class:Reason:Stack -> {error, {Class, Reason, Stack}}
            end,
            Self ! {TestRef, Res}
        end),
        Timeout = timeout_for(Name),
        Result = receive
            {TestRef, R} -> R;
            {'EXIT', Pid, killed} -> {error, {error, heap_limit_exceeded, []}}
        after Timeout ->
            exit(Pid, kill),
            {error, {error, test_timeout, []}}
        end,
        Parent ! {Ref, Name, Result},
        Feeder ! {FeedRef, done}
    end).

take(List, N) -> take(List, N, []).
take(List, 0, Acc) -> {lists:reverse(Acc), List};
take([], _N, Acc) -> {lists:reverse(Acc), []};
take([H|T], N, Acc) -> take(T, N - 1, [H | Acc]).

collect(0, _Ref, Passed, Failed, _Total, _Pending) -> {Passed, Failed};
collect(N, Ref, Passed, Failed, Total, Pending) ->
    receive
        {Ref, Name, ok} ->
            Done = Total - N + 1,
            NewPending = maps:remove(Name, Pending),
            maybe_progress(Done, Total, Passed + 1, length(Failed)),
            collect(N - 1, Ref, Passed + 1, Failed, Total, NewPending);
        {Ref, Name, {error, {Class, Reason, Stack}}} ->
            Done = Total - N + 1,
            NewPending = maps:remove(Name, Pending),
            %% Print test262 failures immediately so they survive job
            %% cancellation (finish() only runs at the end).
            case binary:match(Name, <<"test262/">>) of
                nomatch -> ok;
                _ -> io:format("~n  FAIL ~ts: ~ts~n", [Name, to_list(Reason)])
            end,
            maybe_progress(Done, Total, Passed, length(Failed) + 1),
            collect(N - 1, Ref, Passed, [{Name, Class, Reason, Stack} | Failed], Total, NewPending)
    after 10000 ->
        %% No test completed in 10s — show what's still running
        Still = maps:keys(Pending),
        Remaining = length(Still),
        clear_line(),
        case Remaining > 10 of
            true ->
                io:format("  [~b/~b] waiting for ~b tests...~n",
                          [Total - N, Total, Remaining]);
            false ->
                io:format("  [~b/~b] waiting for ~b tests:~n",
                          [Total - N, Total, Remaining]),
                lists:foreach(fun(Name) ->
                    io:format("    ~ts~n", [Name])
                end, lists:sort(Still))
        end,
        collect(N, Ref, Passed, Failed, Total, Pending)
    end.

maybe_progress(Done, Total, _Pass, _Fail) when Done =:= Total ->
    ok;
maybe_progress(Done, Total, Pass, Fail) ->
    io:format("\r  [~b/~b] ~b passed, ~b failed", [Done, Total, Pass, Fail]).

clear_line() ->
    io:format("\r\e[K", []).

format_test_name(Module, Function) ->
    iolist_to_binary([atom_to_list(Module), ":", atom_to_list(Function)]).

print_failure(error, test_timeout, _Stack) ->
    io:format("    timed out (>10s)~n");
print_failure(error, {gleam_error, assert, Message, _Module, _Function, _Line, _Extra}, _Stack) ->
    io:format("    ~ts~n", [Message]);
print_failure(error, {gleam_error, let_assert, Message, _Module, _Function, _Line, _Extra}, _Stack) ->
    io:format("    ~ts~n", [Message]);
print_failure(error, {assertion_failed, Props}, _Stack) ->
    Reason = proplists:get_value(reason, Props, <<"unknown">>),
    io:format("    ~ts~n", [Reason]);
print_failure(_Class, Reason, Stack) ->
    io:format("    ~p~n", [Reason]),
    case Stack of
        [Top | _] -> io:format("    at ~p~n", [Top]);
        _ -> ok
    end.

gleam_to_erl_module(Path) ->
    NoExt = filename:rootname(Path),
    Replaced = string:replace(NoExt, "/", "@", all),
    binary_to_atom(iolist_to_binary(Replaced), utf8).

erl_to_module(Path) ->
    Basename = filename:basename(Path, ".erl"),
    list_to_atom(Basename).

has_test_functions(Module) ->
    case code:ensure_loaded(Module) of
        {module, _} ->
            Exports = Module:module_info(exports),
            lists:any(fun({Name, Arity}) ->
                (Arity =:= 0) andalso is_test_function(Name)
            end, Exports);
        _ -> false
    end.

is_test_function(Name) ->
    lists:suffix("_test", atom_to_list(Name)).

to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_atom(V) -> atom_to_list(V);
to_list(V) when is_list(V) -> V;
to_list(V) -> lists:flatten(io_lib:format("~p", [V])).

%% Per-test timeout. Unit tests get 10s. test262 gets 30s except for a handful
%% of codepoint-iteration tests that take 80-100s locally / 400+s on CI
%% (they iterate 0..0x10FFFF building/matching giant strings).
timeout_for(Name) ->
    case binary:match(Name, <<"test262/">>) of
        nomatch -> 10000;
        _ ->
            case is_slow_test(Name) of
                true -> 600000;
                false -> 30000
            end
    end.

is_slow_test(Name) ->
    Slow = [
        <<"RegExp/CharacterClassEscapes/character-class-">>,
        <<"decodeURI/S15.1.3.1_A2.5_T1">>,
        <<"decodeURIComponent/S15.1.3.2_A2.5_T1">>,
        <<"encodeURI/S15.1.3.3_A2.3_T1">>,
        <<"encodeURIComponent/S15.1.3.4_A2.3_T1">>
    ],
    lists:any(fun(P) -> binary:match(Name, P) =/= nomatch end, Slow).
