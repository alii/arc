-module(arc_aot_test_ffi).
-export([main/0]).

%% {WorkerPid, Name, StartMs} for every test currently running
-define(IN_FLIGHT, arc_aot_in_flight).

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

    %% preload so code loading is not charged to the first test heap
    Ebin = filename:dirname(code:which(?MODULE)),
    ok = code:ensure_modules_loaded(
           [list_to_atom(filename:rootname(B))
            || B <- filelib:wildcard("*.beam", Ebin)]),

    UnitTests = lists:flatmap(fun(M) ->
        [{format_test_name(M, F), fun() -> M:F(), ok end}
         || {F, 0} <- M:module_info(exports),
            is_test_function(F)]
    end, TestModules),

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
    ?IN_FLIGHT = ets:new(?IN_FLIGHT, [named_table, public, set]),
    spawn_link(fun() -> memory_guard(memory_budget_bytes()) end),
    spawn_link(fun() -> feeder(AllTests, Parent, Ref, MaxWorkers) end),

    {Passed, Failed, Results} =
        collect(Total, Ref, 0, [], 0, [], Total, T262),
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

%% MaxWorkers tests run at once, each in a numbered slot. The vm never frees a
%% module name (65536 of them, ever), so a test's compiled modules are named
%% after the slot and reused, not after the test.
feeder(Tests, Parent, Ref, MaxWorkers) ->
    process_flag(trap_exit, true),
    {Initial, Rest} = take(Tests, MaxWorkers),
    Slots = lists:seq(0, length(Initial) - 1),
    PidMap = maps:from_list(
        [{spawn_worker(T, Slot, Parent, Ref), {element(1, T), Slot}}
         || {T, Slot} <- lists:zip(Initial, Slots)]),
    feeder_loop(Rest, Parent, Ref, PidMap).

%% a worker exit frees its slot for the next test; a worker that died before
%% it could report is reported here
feeder_loop(_Remaining, _Parent, _Ref, PidMap) when map_size(PidMap) =:= 0 ->
    ok;
feeder_loop(Remaining, Parent, Ref, PidMap) ->
    receive
        {'EXIT', Pid, Reason} ->
            case maps:take(Pid, PidMap) of
                {{Name, Slot}, Others} ->
                    ets:delete(?IN_FLIGHT, Pid),
                    case Reason of
                        normal -> ok;
                        _ -> Parent ! {Ref, Name, {error, {exit, Reason, []}}}
                    end,
                    case Remaining of
                        [T | Rest] ->
                            NewPid = spawn_worker(T, Slot, Parent, Ref),
                            feeder_loop(Rest, Parent, Ref,
                                        Others#{NewPid => {element(1, T), Slot}});
                        [] ->
                            feeder_loop([], Parent, Ref, Others)
                    end;
                error ->
                    feeder_loop(Remaining, Parent, Ref, PidMap)
            end
    end.

spawn_worker({Name, {t262, Ctx, File, Expected}}, Slot, Parent, Ref) ->
    spawn_link(fun() ->
        Base = <<"arc_aot_t262_s", (integer_to_binary(Slot))/binary>>,
        Mods = [binary_to_atom(<<Base/binary, Sfx/binary>>)
                || Sfx <- [<<"_0">>, <<"_1">>]],
        %% a test killed mid-run leaves the slot's modules loaded
        [arc_aot_exec_ffi:unload(M) || M <- Mods],
        Fun = fun() -> test262_aot_exec:run_file(Ctx, File, Base) end,
        Outcome = case run_capped(Name, Fun) of
            {ok, O} -> O;
            {error, {error, test_timeout, _}} -> {fail, <<"timeout">>};
            {error, {error, heap_limit_exceeded, _}} -> {fail, <<"heap limit exceeded">>};
            {error, {C, R, S}} -> {fail, render_crash(C, R, S)}
        end,
        [arc_aot_exec_ffi:unload(M) || M <- Mods],
        Parent ! {Ref, Name, {t262, File, Expected, Outcome}}
    end);
spawn_worker({Name, Fun}, _Slot, Parent, Ref) ->
    spawn_link(fun() ->
        Result = case run_capped(Name, Fun) of
            {ok, _} -> ok;
            {error, _} = E -> E
        end,
        Parent ! {Ref, Name, Result}
    end).

run_capped(Name, Fun) ->
    Self = self(),
    TestRef = make_ref(),
    process_flag(trap_exit, true),
    ets:insert(?IN_FLIGHT, {Self, Name, erlang:monotonic_time(millisecond)}),
    Pid = spawn_link(fun() ->
        process_flag(max_heap_size,
                     #{size => max_heap_for(Name), kill => true,
                       error_logger => false}),
        Res = try {ok, Fun()}
        catch Class:Reason:Stack -> {error, {Class, Reason, Stack}}
        end,
        Self ! {TestRef, Res}
    end),
    R = receive
        {TestRef, Res} -> Res;
        {'EXIT', Pid, killed} -> {error, {error, heap_limit_exceeded, []}}
    after timeout_for(Name) ->
        exit(Pid, kill),
        {error, {error, test_timeout, []}}
    end,
    ets:delete(?IN_FLIGHT, Self),
    R.

%% longest-running first
in_flight() ->
    Now = erlang:monotonic_time(millisecond),
    lists:reverse(lists:sort(
        [{Now - T0, Pid, Name} || {Pid, Name, T0} <- ets:tab2list(?IN_FLIGHT)])).

%% max_heap_size only sees a test's heap: a runaway that keeps its garbage in
%% off-heap binaries (strings) grows the vm until the machine kills it. When
%% the whole vm outgrows the budget, kill the longest-running test instead.
memory_guard(Budget) ->
    timer:sleep(250),
    case erlang:memory(total) of
        Total when Total > Budget ->
            case in_flight() of
                [{Ms, Pid, Name} | _] ->
                    io:format("~n  vm at ~b MB, over the ~b MB budget "
                              "(TEST_MEMORY_BUDGET_MB): killing ~ts after ~.1fs~n",
                              [Total div 1048576, Budget div 1048576,
                               Name, Ms / 1000.0]),
                    ets:delete(?IN_FLIGHT, Pid),
                    exit(Pid, kill),
                    timer:sleep(2000);
                [] -> ok
            end;
        _ -> ok
    end,
    memory_guard(Budget).

memory_budget_bytes() ->
    Mb = case os:getenv("TEST_MEMORY_BUDGET_MB") of
        false -> 4096;
        "" -> 4096;
        S -> list_to_integer(S)
    end,
    Mb * 1048576.

render_crash(Class, Reason, Stack) ->
    Top = case Stack of [H | _] -> H; _ -> no_stack end,
    unicode:characters_to_binary(
        io_lib:format("crash ~0p:~0p at ~0p", [Class, Reason, Top])).

take(List, N) -> take(List, N, []).
take(List, 0, Acc) -> {lists:reverse(Acc), List};
take([], _N, Acc) -> {lists:reverse(Acc), []};
take([H|T], N, Acc) -> take(T, N - 1, [H | Acc]).

collect(0, _Ref, Passed, Failed, _Mis, Results, _Total, _T262) ->
    {Passed, Failed, Results};
collect(N, Ref, Passed, Failed, Mis, Results, Total, T262) ->
    receive
        {Ref, _Name, ok} ->
            Done = Total - N + 1,
            maybe_progress(Done, Total, Passed + 1, length(Failed) + Mis),
            collect(N - 1, Ref, Passed + 1, Failed, Mis, Results, Total, T262);
        {Ref, Name, {t262, File, Expected, Outcome}} ->
            Done = Total - N + 1,
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
            collect(N - 1, Ref, P1, Failed, M1, [Result | Results], Total, T262);
        {Ref, Name, {error, {Class, Reason, Stack}}} ->
            Done = Total - N + 1,
            maybe_progress(Done, Total, Passed, length(Failed) + 1 + Mis),
            collect(N - 1, Ref, Passed, [{Name, Class, Reason, Stack} | Failed],
                    Mis, Results, Total, T262)
    after 10000 ->
        clear_line(),
        io:format("  [~b/~b] nothing finished in 10s, ~b left; running now:~n",
                  [Total - N, Total, N]),
        lists:foreach(fun({Ms, _Pid, Name}) ->
            io:format("    ~ts (~.1fs)~n", [Name, Ms / 1000.0])
        end, in_flight()),
        collect(N, Ref, Passed, Failed, Mis, Results, Total, T262)
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
print_failure(exit, killed, _Stack) ->
    io:format("    killed by the memory guard~n");
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

timeout_for(Name) ->
    case binary:match(Name, <<"test262/">>) of
        nomatch -> 10000;
        _ -> 60000
    end.

max_heap_for(Name) ->
    case binary:match(Name, [<<"emit_2core_test:">>, <<"test262/">>]) of
        nomatch -> 10000000;
        _ -> 30000000
    end.
