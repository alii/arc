-module(test262_exec_ffi).

-export([init_stats/0, record_pass/0, record_fail/0, record_skip/0,
         get_stats/0, record_pass_path/1, get_pass_paths/0,
         init_config/3, get_update_mode/0, get_has_snapshot/0, get_fail_log/0,
         init_snapshot_set/1, snapshot_contains/1,
         cache_get/1, cache_put/2,
         spawn_agent/1, broadcast/2, await_broadcast_or_wake/1,
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
%% FRESH agent (own store, own intrinsics, own globals) and runs the agent
%% script. Cross-agent communication:
%%
%%   parent -> child   {arc_agent_broadcast, ParentPid, Ref, Payload}
%%   child  -> parent  {arc_agent_ack, Ref}             (on broadcast receipt)
%%   child  -> parent  {arc_agent_report, Utf8Binary}   ($262.agent.report)
%%
%% A SharedArrayBuffer payload carries the buffer's storage after its block
%% has been handed to an owner process (arc/rt/sab, arc_rt_sab_ffi): that
%% storage is an ordinary pid, so the child's reconstructed SAB aliases the
%% very same block as the parent's, and every Atomics read, write, wait and
%% notify in one agent is a message to the one owner every other agent
%% talks to.
%%
%% This lives in the test harness FFI, not under src/: agent
%% spawn/broadcast/ack/report is test262 HOST machinery (INTERPRETING.md),
%% and its mailbox receives belong to the embedder layer (see the contract
%% in arc/host.gleam).
%%
%% Nothing is remembered here between calls. The parent keeps the pids of
%% the children it started in its own agent state (the Gleam side stores
%% them on its $262.agent object) and hands them to broadcast/2; a child is
%% handed its parent's pid as the argument of its body and threads it to
%% await_broadcast_or_wake/1 and send_report/2.
%%
%% Liveness/cleanup:
%%   * broadcast/2 monitors each child for the span of one broadcast, so a
%%     'DOWN' stands in for the ack a dead child will never send (a dead
%%     agent has trivially "received" the broadcast in the only sense
%%     available);
%%   * each child monitors its parent, so a child idling in
%%     await_broadcast_or_wake/1 exits normally when the per-test worker
%%     process goes away.

%% Spawn a child agent process running Body (a 1-arity Gleam closure given
%% the parent's pid; it boots the fresh agent, executes the agent script,
%% and then loops in the broadcast loop). Returns the child's pid.
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

%% Parent side: hand Payload to every child in Pids, then block until each
%% has acked it. test262 INTERPRETING.md: "broadcast blocks until all agents
%% have received". Children ack on RECEIPT (before invoking their
%% receiveBroadcast callbacks), so a callback that blocks in Atomics.wait
%% cannot deadlock broadcast. Each child is monitored for this one
%% broadcast; the ack names that monitor ref, and a 'DOWN' on it counts as
%% the ack the dead child can never send.
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

%% Child side: block until the next broadcast arrives (ack the sender and
%% return {agent_wake_broadcast, Payload}), OR the owner of a block this
%% agent has a deadline-free Atomics.waitAsync waiter on wakes it
%% ({agent_wake_sab, WRef} -- the async wake of arc_rt_sab_ffi; the caller
%% hands it to arc/rt/async:t_wake_waiter and drains). Without the wake
%% clause an agent idling here with a pending waiter would never consume
%% its wake and the waiter's promise would never settle. Returns
%% agent_wake_parent_down when Parent died -- the caller ends its loop and
%% the child process exits normally.
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
