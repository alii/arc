-module(test262_exec_ffi).

-export([init_stats/0, record_pass/0, record_fail/0, record_skip/0,
         get_stats/0, record_pass_path/1, get_pass_paths/0,
         list_test_files/1]).

%% ETS-backed atomic counters for pass/fail/skip across parallel tests.

init_stats() ->
    catch ets:delete(test262_stats),
    catch ets:delete(test262_passes),
    ets:new(test262_stats, [named_table, public, set]),
    ets:insert(test262_stats, [{pass, 0}, {fail, 0}, {skip, 0}]),
    ets:new(test262_passes, [named_table, public, bag]),
    nil.

record_pass() -> ets:update_counter(test262_stats, pass, 1), nil.
record_fail() -> ets:update_counter(test262_stats, fail, 1), nil.
record_skip() -> ets:update_counter(test262_stats, skip, 1), nil.

get_stats() ->
    [{_, Pass}] = ets:lookup(test262_stats, pass),
    [{_, Fail}] = ets:lookup(test262_stats, fail),
    [{_, Skip}] = ets:lookup(test262_stats, skip),
    {Pass, Fail, Skip}.

record_pass_path(Path) ->
    ets:insert(test262_passes, {Path}),
    nil.

get_pass_paths() ->
    lists:sort([P || {P} <- ets:tab2list(test262_passes)]).

%% Fast recursive listing of .js test files under Dir.
%% Returns sorted list of relative paths as binaries. No file reading.
list_test_files(Dir) ->
    DirStr = binary_to_list(Dir),
    AllJs = filelib:wildcard("**/*.js", DirStr),
    Filtered = lists:sort([list_to_binary(F) || F <- AllJs,
                           string:find(F, "_FIXTURE") =:= nomatch]),
    Filtered.
