-module(test_runner_ffi).

-export([get_env/1, run_with_timeout/2, list_files/1, run_parallel/2]).

%% Read an environment variable. Returns {ok, Value} or {error, nil}.
get_env(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.

%% Run a zero-arg function with a timeout in milliseconds.
%% Returns {ok, Result} or {error, <<"timeout">>}.
run_with_timeout(Fun, TimeoutMs) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        try
            Result = Fun(),
            Parent ! {Ref, {ok, Result}}
        catch
            _:Reason ->
                Parent ! {Ref, {error, list_to_binary(io_lib:format("~p", [Reason]))}}
        end
    end),
    receive
        {Ref, Result} -> Result
    after TimeoutMs ->
        exit(Pid, kill),
        {error, <<"timeout">>}
    end.

%% List all .js files in a directory, returning their filenames (not full paths).
list_files(Dir) ->
    DirStr = binary_to_list(Dir),
    case file:list_dir(DirStr) of
        {ok, Files} ->
            JsFiles = [list_to_binary(F)
                       || F <- lists:sort(Files),
                          lists:suffix(".js", F)],
            {ok, JsFiles};
        {error, Reason} ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Parallel runner — one process per item, BEAM handles scheduling.
%% TestFn: fun(Item) -> {ok, nil} | {error, Reason}
%% Returns: list of {Item, Reason} for failures.
%% Prints live progress for large sets (>100 items).
run_parallel(Items, TestFn) ->
    Parent = self(),
    Ref = make_ref(),
    Pids = [spawn_link(fun() ->
        Result = try TestFn(Item) of
            {ok, nil} -> ok;
            {error, Reason} -> {error, Reason}
        catch
            _:Reason ->
                {error, list_to_binary(io_lib:format("crash: ~p", [Reason]))}
        end,
        Parent ! {Ref, Item, Result}
    end) || Item <- Items],
    Total = length(Pids),
    ShowProgress = Total > 5000,
    Errors = collect_results(Total, Ref, 0, 0, [], Total, ShowProgress),
    case ShowProgress of
        true -> io:format("\r\e[K", []);
        false -> ok
    end,
    Errors.

collect_results(0, _Ref, _Pass, _Fail, Errors, _Total, _Show) -> Errors;
collect_results(N, Ref, Pass, Fail, Errors, Total, Show) ->
    receive
        {Ref, _Item, ok} ->
            NewPass = Pass + 1,
            progress(Total - N + 1, Total, NewPass, Fail, Show),
            collect_results(N - 1, Ref, NewPass, Fail, Errors, Total, Show);
        {Ref, Item, {error, Reason}} ->
            NewFail = Fail + 1,
            progress(Total - N + 1, Total, Pass, NewFail, Show),
            collect_results(N - 1, Ref, Pass, NewFail, [{Item, Reason} | Errors], Total, Show)
    end.

progress(Done, Total, Pass, Fail, true) ->
    case (Done rem 500 =:= 0) orelse (Done =:= Total) of
        true ->
            io:format("\r  [~b/~b] ~b pass, ~b fail", [Done, Total, Pass, Fail]);
        false ->
            ok
    end;
progress(_Done, _Total, _Pass, _Fail, false) ->
    ok.
