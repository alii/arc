-module(arc_aot_test_ffi).
-export([main/0]).

%% Custom test harness — no EUnit, pure BEAM parallelism.
%% All tests (unit tests + test262 files) run in one flat pool.
main() ->
    GleamFiles = filelib:wildcard("**/*.gleam", "test"),
    ErlFiles = filelib:wildcard("**/*.erl", "test"),
    GleamModules = [gleam_to_erl_module(F) || F <- GleamFiles],
    ErlModules = [erl_to_module(F) || F <- ErlFiles],
    AllModules = lists:usort(GleamModules ++ ErlModules),

    Excluded = [arc_aot_test_ffi, arc_aot_test262_ffi, test262_aot_exec],
    TestModules = [M || M <- AllModules,
                        not lists:member(M, Excluded),
                        has_test_functions(M)],

    %% Load everything up front so on-demand code loading is not charged
    %% against the first test's max_heap_size.
    Ebin = filename:dirname(code:which(?MODULE)),
    ok = code:ensure_modules_loaded(
           [list_to_atom(filename:rootname(B))
            || B <- filelib:wildcard("*.beam", Ebin)]),

    UnitTests = lists:flatmap(fun(M) ->
        [{format_test_name(M, F), fun() -> M:F(), ok end}
         || {F, 0} <- M:module_info(exports),
            is_test_function(F)]
    end, TestModules),

    %% If TEST262_EXEC=1, add test262 files as individual pool entries
    %% (setup applies TEST262_FILTER / TEST262_SHARD and precompiles the
    %% harness). Ctx rides in each entry; the snapshot stays here.
    {Test262Tests, T262} = case os:getenv("TEST262_EXEC") of
        false -> {[], none};
        "" -> {[], none};
        _ ->
            {setup, Ctx, Entries} = test262_aot_exec:setup(),
            {[{<<"test262/", F/binary>>, {t262, Ctx, F, Expected}}
              || {F, Expected} <- Entries],
             {ctx, Ctx}}
    end,

    AllTests = UnitTests ++ Test262Tests,
    Total = length(AllTests),

    ModuleCount = length(TestModules) + case T262 of none -> 0; _ -> 1 end,
    io:format("Running ~b tests across ~b modules~n", [Total, ModuleCount]),

    Parent = self(),
    Ref = make_ref(),
    T0 = erlang:monotonic_time(millisecond),
    MaxWorkers = erlang:system_info(schedulers_online),
    spawn_link(fun() -> feeder(AllTests, Parent, Ref, MaxWorkers) end),

    Pending = maps:from_list([{Name, true} || {Name, _} <- AllTests]),
    {Passed, Failed, Results} =
        collect(Total, Ref, 0, [], 0, [], Total, Pending, T262),
    clear_line(),
    T1 = erlang:monotonic_time(millisecond),
    Elapsed = T1 - T0,

    Mismatches = case T262 of
        none -> 0;
        {ctx, Ctx1} -> test262_aot_exec:finish(Ctx1, Results)
    end,

    lists:foreach(fun({Name, Class, Reason, Stack}) ->
        io:format("~n  FAIL ~ts~n", [Name]),
        print_failure(Class, Reason, Stack)
    end, Failed),

    UpdateMode = case os:getenv("UPDATE_SNAPSHOT") of
        false -> false;
        "" -> false;
        _ -> true
    end,
    case {Mismatches, UpdateMode} of
        {0, _} -> ok;
        {_, true} -> ok;
        {_, false} ->
            io:format("~n  ~b test262 snapshot mismatch(es) — "
                      "investigate, then UPDATE_SNAPSHOT=1 to accept~n",
                      [Mismatches])
    end,
    FailCount = length(Failed) + case UpdateMode of
        true -> 0;
        false -> Mismatches
    end,
    case T262 of
        none -> ok;
        _ -> io:format("~ntest262: ~b files in ~.1fs (~.1f files/s)~n",
                       [length(Results), Elapsed / 1000.0,
                        length(Results) / max(Elapsed / 1000.0, 0.001)])
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
            feeder_loop(Remaining, Parent, Ref, Self, FeedRef, Active, PidMap);
        {'EXIT', Pid, Reason} ->
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

%% A test262 entry runs test262_aot_exec:run_file/3 under a module-name
%% prefix minted here, so the modules its variants load (<Base>_0/_1) can be
%% unloaded afterwards even when the worker was killed mid-run. Its result
%% is {t262, File, Expected, Outcome}; a timeout or heap kill is a FAIL
%% outcome, not a pool error.
spawn_worker({Name, {t262, Ctx, File, Expected}}, Parent, Ref, Feeder, FeedRef) ->
    spawn_link(fun() ->
        Base = <<"arc_aot_t262_",
                 (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
        Fun = fun() -> test262_aot_exec:run_file(Ctx, File, Base) end,
        Outcome = case run_capped(Name, Fun) of
            {ok, O} -> O;
            {error, {error, test_timeout, _}} -> {fail, <<"timeout">>};
            {error, {error, heap_limit_exceeded, _}} -> {fail, <<"heap limit exceeded">>};
            {error, {C, R, S}} -> {fail, render_crash(C, R, S)}
        end,
        [arc_aot_exec_ffi:unload(binary_to_atom(<<Base/binary, Sfx/binary>>))
         || Sfx <- [<<"_0">>, <<"_1">>]],
        Parent ! {Ref, Name, {t262, File, Expected, Outcome}},
        Feeder ! {FeedRef, done}
    end);
spawn_worker({Name, Fun}, Parent, Ref, Feeder, FeedRef) ->
    spawn_link(fun() ->
        Result = case run_capped(Name, Fun) of
            {ok, _} -> ok;
            {error, _} = E -> E
        end,
        Parent ! {Ref, Name, Result},
        Feeder ! {FeedRef, done}
    end).

%% Run Fun in a linked sub-process under the entry's heap cap and timeout.
%% {ok, Value} | {error, {Class, Reason, Stack}}.
run_capped(Name, Fun) ->
    Self = self(),
    TestRef = make_ref(),
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() ->
        process_flag(max_heap_size,
                     #{size => max_heap_for(Name), kill => true,
                       error_logger => false}),
        Res = try {ok, Fun()}
        catch Class:Reason:Stack -> {error, {Class, Reason, Stack}}
        end,
        Self ! {TestRef, Res}
    end),
    receive
        {TestRef, R} -> R;
        {'EXIT', Pid, killed} -> {error, {error, heap_limit_exceeded, []}}
    after timeout_for(Name) ->
        exit(Pid, kill),
        {error, {error, test_timeout, []}}
    end.

render_crash(Class, Reason, Stack) ->
    Top = case Stack of [H | _] -> H; _ -> no_stack end,
    unicode:characters_to_binary(
        io_lib:format("crash ~0p:~0p at ~0p", [Class, Reason, Top])).

take(List, N) -> take(List, N, []).
take(List, 0, Acc) -> {lists:reverse(Acc), List};
take([], _N, Acc) -> {lists:reverse(Acc), []};
take([H|T], N, Acc) -> take(T, N - 1, [H | Acc]).

%% Results accumulates the test262 {test_result, File, Expected, Outcome}
%% records for finish/2. A test262 entry counts as failed (Mis) for the
%% progress line only when it mismatches the snapshot; those are printed at
%% once so they survive a cancelled job.
collect(0, _Ref, Passed, Failed, _Mis, Results, _Total, _Pending, _T262) ->
    {Passed, Failed, Results};
collect(N, Ref, Passed, Failed, Mis, Results, Total, Pending, T262) ->
    receive
        {Ref, Name, ok} ->
            Done = Total - N + 1,
            NewPending = maps:remove(Name, Pending),
            maybe_progress(Done, Total, Passed + 1, length(Failed) + Mis),
            collect(N - 1, Ref, Passed + 1, Failed, Mis, Results, Total,
                    NewPending, T262);
        {Ref, Name, {t262, File, Expected, Outcome}} ->
            Done = Total - N + 1,
            NewPending = maps:remove(Name, Pending),
            Result = {test_result, File, Expected, Outcome},
            {ctx, Ctx} = T262,
            {P1, M1} = case test262_aot_exec:is_mismatch(Ctx, Result) of
                false -> {Passed + 1, Mis};
                true ->
                    What = case Outcome of
                        pass -> <<"NEW PASS">>;
                        {fail, Reason} -> <<"REGRESSION: ", Reason/binary>>;
                        {skip, Cat} -> <<"SKIP: ", Cat/binary>>
                    end,
                    io:format("~n  FAIL ~ts: ~ts~n", [Name, What]),
                    {Passed, Mis + 1}
            end,
            maybe_progress(Done, Total, P1, length(Failed) + M1),
            collect(N - 1, Ref, P1, Failed, M1, [Result | Results], Total,
                    NewPending, T262);
        {Ref, Name, {error, {Class, Reason, Stack}}} ->
            Done = Total - N + 1,
            NewPending = maps:remove(Name, Pending),
            maybe_progress(Done, Total, Passed, length(Failed) + 1 + Mis),
            collect(N - 1, Ref, Passed, [{Name, Class, Reason, Stack} | Failed],
                    Mis, Results, Total, NewPending, T262)
    after 10000 ->
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
        collect(N, Ref, Passed, Failed, Mis, Results, Total, Pending, T262)
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
    io:format("    timed out~n");
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

%% Per-entry timeout. Unit tests get 10s; a test262 file compiles up to two
%% variants through the BEAM compiler in-process, so it gets 60s.
timeout_for(Name) ->
    case binary:match(Name, <<"test262/">>) of
        nomatch -> 10000;
        _ -> 60000
    end.

%% Per-test heap cap (words). emit_2core_test:* diff-tests and test262
%% entries run a full JS→IR→Core→BEAM compile in-process; the beam compiler
%% alone can brush the default 80MB cap on the longer fixtures.
max_heap_for(Name) ->
    case binary:match(Name, [<<"emit_2core_test:">>, <<"test262/">>]) of
        nomatch -> 10000000;
        _ -> 30000000
    end.
