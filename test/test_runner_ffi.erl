-module(test_runner_ffi).

-export([list_files/1, generate_eunit_tests/2,
         generate_recursive_eunit_tests/2, get_env/1, empty_tests/0,
         run_with_timeout/2, timeout_test/2, make_test_suite/2,
         make_test_suite/3, lazy_file_tests/3, with_cleanup/3]).

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

%% Create a single EUnit test with a custom timeout (in seconds).
timeout_test(TimeoutSec, Fun) ->
    {timeout, TimeoutSec, Fun}.

%% Build a list of EUnit test descriptors from {Name, TestFn} pairs.
%% Each test gets an individual timeout. Tests run in parallel.
%% TestFn: fun() -> {ok, nil} | {error, Reason}
%% Build parallel test suite with optional sequential cleanup test at the end.
%% Tests: list of {Name, TestFn} where TestFn() -> {ok, nil} | {error, Reason}
%% CleanupFn: {Name, Fun} to run sequentially after the parallel block, or 'none'.
make_test_suite(Tests, TimeoutSec) ->
    make_test_suite(Tests, TimeoutSec, none).

make_test_suite(Tests, TimeoutSec, CleanupTest) ->
    EunitTests = lists:map(
        fun({Name, TestFn}) ->
            NameStr = binary_to_list(Name),
            {NameStr, {timeout, TimeoutSec, fun() ->
                case TestFn() of
                    {ok, nil} -> ok;
                    {error, Reason} ->
                        erlang:error({assertion_failed,
                            [{test, Name},
                             {reason, Reason}]})
                end
            end}}
        end,
        Tests
    ),
    ParallelBlock = {inparallel, EunitTests},
    case CleanupTest of
        none -> ParallelBlock;
        {CName, CFun} ->
            %% Sequential list: parallel block first, then cleanup
            [ParallelBlock, {binary_to_list(CName), {timeout, 60, fun() ->
                case CFun() of
                    {ok, nil} -> ok;
                    {error, Reason} ->
                        erlang:error({assertion_failed, [{test, CName}, {reason, Reason}]})
                end
            end}}]
    end.

%% Lazily generate EUnit tests by walking a directory tree.
%% Each subdirectory becomes a {generator, Fun} — only listed when EUnit
%% reaches it. Files within a directory run in parallel.
%% TestFn: fun(RelPathBin) -> {ok, nil} | {error, Reason}
lazy_file_tests(RootDir, TestFn, TimeoutSec) ->
    Prefix = binary_to_list(RootDir) ++ "/",
    generate_dir_tests(binary_to_list(RootDir), Prefix, TestFn, TimeoutSec).

generate_dir_tests(Dir, Prefix, TestFn, TimeoutSec) ->
    case file:list_dir(Dir) of
        {ok, Entries} ->
            Sorted = lists:sort(Entries),
            %% Separate into files and subdirectories
            {Files, Dirs} = lists:partition(fun(E) ->
                not filelib:is_dir(filename:join(Dir, E))
            end, Sorted),
            %% Filter to .js files, excluding _FIXTURE
            JsFiles = [F || F <- Files,
                        lists:suffix(".js", F),
                        string:find(F, "_FIXTURE") =:= nomatch],
            %% Build test descriptors for JS files
            FileTests = lists:map(fun(Filename) ->
                FullPath = filename:join(Dir, Filename),
                RelPath = lists:nthtail(length(Prefix), FullPath),
                RelPathBin = list_to_binary(RelPath),
                {RelPath, {timeout, TimeoutSec, fun() ->
                    case TestFn(RelPathBin) of
                        {ok, nil} -> ok;
                        {error, Reason} ->
                            erlang:error({assertion_failed,
                                [{test, RelPathBin},
                                 {reason, Reason}]})
                    end
                end}}
            end, JsFiles),
            %% Build lazy generators for subdirectories
            DirGens = lists:map(fun(SubDir) ->
                SubDirPath = filename:join(Dir, SubDir),
                {generator, fun() ->
                    generate_dir_tests(SubDirPath, Prefix, TestFn, TimeoutSec)
                end}
            end, Dirs),
            %% Files in parallel, then subdirectory generators
            case FileTests of
                [] -> DirGens;
                _  -> [{inparallel, FileTests} | DirGens]
            end;
        {error, _} ->
            []
    end.

%% Append a cleanup test that runs sequentially after a test block.
%% Works with both lists and {inparallel, ...} tuples.
with_cleanup(Tests, {Name, Fun}, TimeoutSec) ->
    CleanupTest = {binary_to_list(Name), {timeout, TimeoutSec, fun() ->
        case Fun() of
            {ok, nil} -> ok;
            {error, Reason} ->
                erlang:error({assertion_failed, [{test, Name}, {reason, Reason}]})
        end
    end}},
    [Tests, CleanupTest].

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
