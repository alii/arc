-module(test_runner_ffi).

-export([list_files/1, generate_eunit_tests/2,
         generate_recursive_eunit_tests/2, get_env/1, empty_tests/0]).

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

%% Generate EUnit test descriptors — one per .js file in a directory.
%% Pre-reads all files upfront so test funs only run the callback (no disk I/O).
%%
%% TestFn: fun(Filename, Source) -> {ok, nil} | {error, Reason}
%%   where Filename and Source are binaries, Reason is a binary.
%%
%% Returns {ok, {inparallel, [{TestName, Fun}]}} for use as EUnit test generators.
generate_eunit_tests(Dir, TestFn) ->
    DirStr = binary_to_list(Dir),
    case file:list_dir(DirStr) of
        {ok, AllFiles} ->
            JsFiles = [F || F <- lists:sort(AllFiles), lists:suffix(".js", F)],
            %% Pre-read all files
            FilesWithSource = lists:filtermap(
                fun(Filename) ->
                    FilePath = filename:join(DirStr, Filename),
                    case file:read_file(FilePath) of
                        {ok, Source} -> {true, {Filename, Source}};
                        {error, _} -> false
                    end
                end,
                JsFiles
            ),
            Tests = lists:map(
                fun({Filename, Source}) ->
                    FilenameBin = list_to_binary(Filename),
                    TestName = binary_to_list(FilenameBin),
                    {TestName, fun() ->
                        case TestFn(FilenameBin, Source) of
                            {ok, nil} -> ok;
                            {error, Reason} ->
                                erlang:error({assertion_failed,
                                    [{file, FilenameBin},
                                     {reason, Reason}]})
                        end
                    end}
                end,
                FilesWithSource
            ),
            {ok, {inparallel, Tests}};
        {error, Reason} ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Recursively list and pre-read all .js files under Dir.
%% Returns relative paths as test names. Filters out _FIXTURE files.
generate_recursive_eunit_tests(Dir, TestFn) ->
    DirStr = binary_to_list(Dir),
    AllJs = filelib:wildcard("**/*.js", DirStr),
    %% Filter out _FIXTURE helper files, sort for deterministic order
    JsFiles = lists:sort([F || F <- AllJs,
                           string:find(F, "_FIXTURE") =:= nomatch]),
    %% Pre-read all files
    FilesWithSource = lists:filtermap(
        fun(RelPath) ->
            FullPath = filename:join(DirStr, RelPath),
            case file:read_file(FullPath) of
                {ok, Source} -> {true, {list_to_binary(RelPath), Source}};
                {error, _} -> false
            end
        end,
        JsFiles
    ),
    Tests = lists:map(
        fun({RelPathBin, Source}) ->
            TestName = binary_to_list(RelPathBin),
            {TestName, fun() ->
                case TestFn(RelPathBin, Source) of
                    {ok, nil} -> ok;
                    {error, Reason} ->
                        erlang:error({assertion_failed,
                            [{file, RelPathBin},
                             {reason, Reason}]})
                end
            end}
        end,
        FilesWithSource
    ),
    {ok, {inparallel, Tests}}.

%% Read an environment variable. Returns {ok, Value} or {error, nil}.
get_env(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.

%% Return an empty EUnit test descriptor (no tests to run).
empty_tests() -> {inparallel, []}.
