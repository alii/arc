-module(test262_exec_ffi).

-export([init_stats/0, record_pass/0, record_fail/0, record_skip/0,
         get_stats/0, record_pass_path/1, get_pass_paths/0,
         list_test_files/1,
         init_config/3, get_update_mode/0, get_has_snapshot/0, get_fail_log/0,
         init_snapshot_set/1, snapshot_contains/1,
         cache_get/1, cache_put/2,
         spawn_agent/1, agent_children/0, agent_parent/0,
         send_broadcast/2, await_acks/1, await_broadcast_or_notify/0,
         send_report/2, take_report/0]).

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
list_test_files(Dir) ->
    DirStr = binary_to_list(Dir),
    AllJs = filelib:wildcard("**/*.js", DirStr),
    Filtered = lists:sort([list_to_binary(F) || F <- AllJs,
                           string:find(F, "_FIXTURE") =:= nomatch]),
    Filtered.

%% --- Config stored in ETS so run_file can access it ---

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

%% --- Snapshot set in ETS for O(1) lookup ---

init_snapshot_set(Paths) ->
    catch ets:delete(test262_snapshot_set),
    ets:new(test262_snapshot_set, [named_table, public, set]),
    lists:foreach(fun(P) -> ets:insert(test262_snapshot_set, {P}) end, Paths),
    nil.

snapshot_contains(Path) ->
    ets:member(test262_snapshot_set, Path).

%% --- Cross-test cache (booted realm, compiled harness templates) ---
%%
%% persistent_term so per-test worker processes get zero-copy reads of the
%% shared immutable Gleam data. Values are deterministic for a given key, so
%% a racing duplicate put stores an equal term (no global GC sweep).

cache_get(Key) ->
    case persistent_term:get({test262_cache, Key}, '$test262_cache_miss') of
        '$test262_cache_miss' -> none;
        Value -> {some, Value}
    end.

cache_put(Key, Value) ->
    persistent_term:put({test262_cache, Key}, Value),
    nil.

%% --- $262.agent — real BEAM child processes (harness host layer) ---
%%
%% Each $262.agent.start(script) spawns a genuine child process that boots a
%% FRESH realm (own heap, own builtins, own globals) and runs the agent
%% script. Cross-agent communication:
%%
%%   parent -> child   {arc_agent_broadcast, Payload}
%%   child  -> parent  {arc_agent_ack, ChildPid}      (on broadcast receipt)
%%   child  -> parent  {arc_agent_report, Utf8Binary} ($262.agent.report)
%%
%% SharedArrayBuffer payloads carry the BufShared atomics ref (see
%% arc_sab_ffi.erl) — atomics refs pass between processes BY REFERENCE, so
%% the child's reconstructed SAB aliases the very same mutable cells as the
%% parent's. That is what makes Atomics.* writes in an agent visible to the
%% main agent and vice versa.
%%
%% This lives in the test harness FFI, not under src/arc/vm/: agent
%% spawn/broadcast/ack/report is test262 HOST machinery (INTERPRETING.md),
%% and its mailbox receives belong to the embedder layer — the same
%% boundary as arc_beam_ffi.erl (see the contract in arc/host.gleam).
%%
%% Bookkeeping lives in the process dictionary, which is naturally scoped to
%% the per-test worker process the test262 runner spawns:
%%   arc_agent_children — pids of children this (parent) process started
%%   arc_agent_parent   — set in a child: the pid that spawned it
%%
%% Liveness/cleanup:
%%   * the parent monitors each child (spawn_monitor), so await_acks/1 can
%%     treat a 'DOWN' as the ack a dead child will never send (a dead agent
%%     has trivially "received" the broadcast in the only sense available);
%%   * each child monitors its parent, so a child idling in
%%     await_broadcast_or_notify/0 exits normally when the per-test worker
%%     process goes away.

%% Spawn a child agent process running Body (a 0-arity Gleam closure that
%% boots the fresh realm, executes the agent script, and then loops in the
%% broadcast loop). Registers the child in this process's children list.
spawn_agent(Body) ->
    Parent = self(),
    {Pid, _MRef} = erlang:spawn_monitor(fun() ->
        erlang:put(arc_agent_parent, Parent),
        erlang:monitor(process, Parent),
        try
            Body()
        catch
            Class:Reason:Stack ->
                io:format(
                    standard_error,
                    "[$262.agent] agent process crashed: ~p:~p~n~p~n",
                    [Class, Reason, Stack]
                )
        end
    end),
    erlang:put(arc_agent_children, agent_children() ++ [Pid]),
    nil.

%% Children started by this process, in start order.
agent_children() ->
    case erlang:get(arc_agent_children) of
        undefined -> [];
        Pids -> Pids
    end.

%% In a child: the spawning parent's pid. {error, nil} in the main agent.
agent_parent() ->
    case erlang:get(arc_agent_parent) of
        undefined -> {error, nil};
        Pid -> {ok, Pid}
    end.

%% Parent side: hand a broadcast payload to one child.
send_broadcast(Pid, Payload) ->
    Pid ! {arc_agent_broadcast, Payload},
    nil.

%% Parent side: block until every pid in Pids has acked the broadcast.
%% test262 INTERPRETING.md: "broadcast blocks until all agents have received".
%% Children ack on RECEIPT (before invoking their receiveBroadcast callbacks),
%% so a callback that blocks in Atomics.wait cannot deadlock broadcast.
%% A 'DOWN' for a pending pid counts as its ack (it can never receive), and
%% the dead child is pruned so later broadcasts skip it.
await_acks([]) ->
    nil;
await_acks(Pids) ->
    receive
        {arc_agent_ack, Pid} ->
            await_acks(lists:delete(Pid, Pids));
        {'DOWN', _MRef, process, Pid, _Reason} ->
            erlang:put(arc_agent_children, lists:delete(Pid, agent_children())),
            await_acks(lists:delete(Pid, Pids))
    end.

%% Child side: block until the next broadcast arrives (ack the parent and
%% return {agent_wake_broadcast, Payload}), OR a cross-process Atomics.notify
%% lands ({agent_wake_notify, SabKey, ByteIndex} — the caller settles its
%% matching waitAsync State waiter via event_loop.inject_notify).
%% Without the arc_notify clause an agent idling here with a pending
%% infinite-timeout Atomics.waitAsync waiter would never consume the notify
%% message and the waiter would never settle. Returns agent_wake_parent_down
%% when the parent died — the caller ends its loop and the child process
%% exits normally.
await_broadcast_or_notify() ->
    receive
        {arc_agent_broadcast, Payload} ->
            case erlang:get(arc_agent_parent) of
                undefined -> ok;
                Parent -> Parent ! {arc_agent_ack, self()}
            end,
            {agent_wake_broadcast, Payload};
        {arc_notify, _Ref, SabKey, ByteIndex} ->
            {agent_wake_notify, SabKey, ByteIndex};
        {'DOWN', _MRef, process, _Pid, _Reason} ->
            agent_wake_parent_down
    end.

%% Child side: post a $262.agent.report(string) to the parent's mailbox.
send_report(Parent, Report) ->
    Parent ! {arc_agent_report, Report},
    nil.

%% Parent side: non-blocking dequeue of the oldest child report.
%% Erlang mailboxes are FIFO per sender; reports from one agent arrive in
%% the order it posted them, which is the ordering the tests rely on.
take_report() ->
    receive
        {arc_agent_report, Report} -> {ok, Report}
    after 0 ->
        {error, nil}
    end.
