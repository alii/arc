-module(test262_exec_ffi).

-export([init_stats/0, record_pass/0, record_fail/0, record_skip/0,
         get_stats/0, record_pass_path/1, get_pass_paths/0,
         init_config/3, get_update_mode/0, get_has_snapshot/0, get_fail_log/0,
         init_snapshot_set/1, snapshot_contains/1,
         cache_get/1, cache_put/2,
         spawn_agent/1, broadcast/2, await_broadcast_or_wake/1,
         send_report/2, take_report/0]).

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

init_config(UpdateMode, HasSnapshot, FailLog) ->
    ets:insert(test262_stats, {config, UpdateMode, HasSnapshot, FailLog}),
    nil.

get_update_mode() ->
    [{config, UpdateMode, _, _}] = ets:lookup(test262_stats, config),
    UpdateMode.

get_has_snapshot() ->
    [{config, _, HasSnapshot, _}] = ets:lookup(test262_stats, config),
    HasSnapshot.

get_fail_log() ->
    [{config, _, _, FailLog}] = ets:lookup(test262_stats, config),
    FailLog.

init_snapshot_set(Paths) ->
    catch ets:delete(test262_snapshot_set),
    ets:new(test262_snapshot_set, [named_table, public, set]),
    lists:foreach(fun(P) -> ets:insert(test262_snapshot_set, {P}) end, Paths),
    nil.

snapshot_contains(Path) ->
    ets:member(test262_snapshot_set, Path).

%% persistent_term for zero-copy reads from worker processes
cache_get(Key) ->
    case persistent_term:get({test262_cache, Key}, '$test262_cache_miss') of
        '$test262_cache_miss' -> none;
        Value -> {some, Value}
    end.

cache_put(Key, Value) ->
    persistent_term:put({test262_cache, Key}, Value),
    nil.

spawn_agent(Body) ->
    Parent = self(),
    spawn(fun() ->
        erlang:monitor(process, Parent),
        try
            Body(Parent)
        catch
            Class:Reason:Stack ->
                io:format(
                    standard_error,
                    "[$262.agent] agent process crashed: ~p:~p~n~p~n",
                    [Class, Reason, Stack]
                )
        end
    end).

%% children ack on receipt, before callbacks, so this can't deadlock
broadcast(Pids, Payload) ->
    Parent = self(),
    Pending =
        [begin
             MRef = erlang:monitor(process, Pid),
             Pid ! {arc_agent_broadcast, Parent, MRef, Payload},
             MRef
         end || Pid <- Pids],
    await_acks(Pending).

await_acks([]) ->
    nil;
await_acks([MRef | Rest]) ->
    receive
        {arc_agent_ack, MRef} ->
            erlang:demonitor(MRef, [flush]);
        {'DOWN', MRef, process, _Pid, _Reason} ->
            ok
    end,
    await_acks(Rest).

await_broadcast_or_wake(Parent) ->
    receive
        {arc_agent_broadcast, From, Ref, Payload} ->
            From ! {arc_agent_ack, Ref},
            {agent_wake_broadcast, Payload};
        {arc_sab_wake, WRef, async} ->
            {agent_wake_sab, WRef};
        {'DOWN', _MRef, process, Parent, _Reason} ->
            agent_wake_parent_down
    end.

send_report(Parent, Report) ->
    Parent ! {arc_agent_report, Report},
    nil.

take_report() ->
    receive
        {arc_agent_report, Report} -> {ok, Report}
    after 0 ->
        {error, nil}
    end.
