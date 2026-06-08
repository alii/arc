-module(arc_beam_ffi).
-export([receivers_get/0, receivers_put/1]).
-export([receive_settle_only/0, receive_settle_or_subject/1,
         receive_settle_only_timeout/1, receive_settle_or_subject_timeout/2]).
-export([send_subject_message/3, receive_subject_message/1,
         receive_subject_message_timeout/2]).
-export([select_message/1, select_message_timeout/2]).
-export([send_after/3, cancel_timer/1, sleep/1]).
-export([await_notify/2, deliver_wakes/1, wait_for_notify/1]).

%% This module is the BEAM-mailbox embedder layer for `src/arc/beam.gleam`:
%% every event-driven `receive` / `!` that drives the beam.run macrotask loop
%% lives here, OUTSIDE the VM core (src/arc/vm/**), which must stay free of
%% event-driven mailbox coupling.

%% Process-dictionary store for pending `receiveAsync` tickets — kept out of
%% core `State` because it is per-BEAM-process bookkeeping for a
%% per-BEAM-process loop.
%%
%% Stored as {TagToRefs, RefToTag}: a map from subject tag to the FIFO list
%% of pending settle refs, and the reverse map from settle ref to tag. Both
%% are maintained incrementally so registration and per-event lookup/removal
%% are O(log n) instead of rebuilding from a list on every mailbox event.

receivers_get() ->
    case get(arc_beam_receivers) of
        undefined -> {#{}, #{}};
        Pair -> Pair
    end.

receivers_put(Pair) ->
    put(arc_beam_receivers, Pair),
    nil.

%% Event loop selective receive. Accepts SettlePromise, ReceiverTimeout,
%% cross-process Atomics wakes ({arc_notify, ...} from a notifier's wake
%% delivery, surfaced as the AtomicsNotify mailbox event so beam.run can
%% inject them into core), and optionally subject messages whose ref is in
%% RefMap.
receive_settle_only() ->
    receive
        {settle_promise, _, _} = E -> E;
        {receiver_timeout, _} = E -> E;
        {arc_notify, _Ref, Key, ByteIndex} -> {atomics_notify, Key, ByteIndex}
    end.
receive_settle_or_subject(RefMap) ->
    receive
        {settle_promise, _, _} = E -> E;
        {receiver_timeout, _} = E -> E;
        {arc_notify, _Ref, Key, ByteIndex} -> {atomics_notify, Key, ByteIndex};
        {Ref, Pm} when is_map_key(Ref, RefMap) -> {subject_message, Ref, Pm}
    end.

%% Timeout variants for embedder loops with pending host-timer / atomics
%% deadlines: {error, nil} on timeout means a deadline is due — re-drain.
receive_settle_only_timeout(Timeout) ->
    receive
        {settle_promise, _, _} = E -> {ok, E};
        {receiver_timeout, _} = E -> {ok, E};
        {arc_notify, _Ref, Key, ByteIndex} ->
            {ok, {atomics_notify, Key, ByteIndex}}
    after Timeout -> {error, nil}
    end.
receive_settle_or_subject_timeout(RefMap, Timeout) ->
    receive
        {settle_promise, _, _} = E -> {ok, E};
        {receiver_timeout, _} = E -> {ok, E};
        {arc_notify, _Ref, Key, ByteIndex} ->
            {ok, {atomics_notify, Key, ByteIndex}};
        {Ref, Pm} when is_map_key(Ref, RefMap) -> {ok, {subject_message, Ref, Pm}}
    after Timeout -> {error, nil}
    end.

%% Subject-based selective receive. Messages are {Ref, PortableMessage} tuples
%% where Ref is the subject's unique erlang:make_ref() tag.
%% The BEAM optimizes receive on a bound ref by skipping older messages.
send_subject_message(Pid, Ref, Msg) ->
    Pid ! {Ref, Msg}, nil.
receive_subject_message(Ref) ->
    receive {Ref, Pm} -> Pm end.
receive_subject_message_timeout(Ref, Timeout) ->
    receive {Ref, Pm} -> {ok, Pm}
    after Timeout -> {error, nil}
    end.

%% Multi-subject select: RefMap is #{Ref1 => true, Ref2 => true, ...}.
%% Uses is_map_key/2 guard BIF for dynamic selective receive.
select_message(RefMap) ->
    receive {Ref, _} = Msg when is_map_key(Ref, RefMap) -> Msg end.
select_message_timeout(RefMap, Timeout) ->
    receive {Ref, _} = Msg when is_map_key(Ref, RefMap) -> {ok, Msg}
    after Timeout -> {error, nil}
    end.

send_after(Ms, Pid, Msg) ->
    erlang:send_after(Ms, Pid, Msg).

%% Returns true if the timer was still active (cancelled successfully),
%% false if it had already fired.
cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
        false -> false;
        _TimeLeft -> true
    end.

sleep(Ms) -> timer:sleep(Ms), nil.

%% -- Atomics host capabilities (contract: arc/host.gleam, clauses 1-4) ------
%%
%% The waiterlist registry itself (the named public ETS table) is DATA-ONLY
%% core, owned by src/arc/vm/builtins/arc_waiter_ffi.erl. Every event-driven
%% receive/send of its {arc_notify, Ref, Key, ByteIndex} wake protocol lives
%% HERE, in the embedder layer (mirrored in test/test262_exec_ffi.erl for the
%% harness). Waiter handles are the {EtsKey, MsgRef} pairs produced by
%% arc_waiter_ffi:insert_waiter.

%% The waiterlist table (created and owned by arc_waiter_ffi:ensure_table;
%% by the time any function below runs, insert_waiter has created it).
-define(WAITER_TAB, arc_atomics_waiterlist).
%% Safety valve for the "a notifier claimed our entry but its message never
%% arrived" case (the notifying process died between take and send).
-define(FLUSH_SAFETY_MS, 1000).
%% erlang `receive ... after` rejects timeouts >= 2^32; clamp (49 days).
-define(MAX_RECV_MS, 4294967294).

%% Contract clause 1: block the calling agent until its waiterlist entry is
%% notified, or TimeoutMs elapses (negative -> infinity). Returns the JS
%% result strings <<"ok">> | <<"timed-out">>. The notify-vs-timeout race is
%% resolved lock-free via ets:take of our OWN entry on timeout: we removed
%% it ourselves -> nobody claimed us -> "timed-out"; it is gone -> a
%% notifier claimed (and counted) us and its wake message is in flight ->
%% bounded flush-receive, then "ok". Relocated verbatim from the old
%% arc_waiter_ffi:await_notify — semantics must not drift.
%%
%% TimeoutMs = 0 doubles as contract clause 4's cancel flush (the old
%% arc_waiter_ffi:cancel_waiter flush arm): core's sync_block "not-equal"
%% path calls the host_sync_wait capability with a zero timeout exactly
%% when its data-only cancel found the entry already claimed by a notifier,
%% so the claimed branch below consumes the in-flight wake (safety-bounded)
%% and it cannot pollute a later receive.
await_notify({Key, Ref}, TimeoutMs) ->
    Timeout =
        if
            TimeoutMs < 0 -> infinity;
            TimeoutMs > ?MAX_RECV_MS -> ?MAX_RECV_MS;
            true -> TimeoutMs
        end,
    receive
        {arc_notify, Ref, _, _} -> <<"ok">>
    after Timeout ->
        case ets:take(?WAITER_TAB, Key) of
            [_] ->
                %% We removed our own entry: no notifier claimed us.
                <<"timed-out">>;
            [] ->
                %% Timeout raced a notifier that already took our entry —
                %% its message is (about to be) in our mailbox. Per spec
                %% the notifier counted us as woken, so report "ok".
                receive
                    {arc_notify, Ref, _, _} -> <<"ok">>
                after ?FLUSH_SAFETY_MS ->
                    <<"timed-out">>
                end
        end
    end.

%% Contract clause 2: deliver one wake message per remote waiter claimed by
%% Atomics.notify's waiterlist take. Claiming (the atomic ets:take in
%% arc_waiter_ffi:take_waiters) already counted the waiter as woken; only
%% the claimer may message it, so each entry is delivered at most once.
%% Claimed terms are {Pid, Ref, Key, ByteIndex} (opaque state.ClaimedWaiter
%% on the Gleam side).
deliver_wakes(Claimed) ->
    lists:foreach(
        fun({Pid, Ref, Key, ByteIndex}) ->
            Pid ! {arc_notify, Ref, Key, ByteIndex}
        end,
        Claimed),
    nil.

%% Atomics host-capability contract, clause 4 (see arc/host.gleam): the
%% bounded dry-queue receive for cross-process Atomics.notify wakes.
%% Blocks at most Ms (negative -> poll, clamped to the BEAM receive cap)
%% for an {arc_notify, Ref, Key, ByteIndex} message sent by a notifier's
%% wake delivery; returns {some, {Key, ByteIndex}} | none (gleam Option)
%% for injection into core via builtins_atomics.settle_notified_waiter.
%% Embedder FFI on purpose — core (src/arc/vm/**) never receives.
wait_for_notify(Ms) ->
    Timeout =
        if
            Ms < 0 -> 0;
            Ms > ?MAX_RECV_MS -> ?MAX_RECV_MS;
            true -> Ms
        end,
    receive
        {arc_notify, _Ref, SabKey, ByteIndex} -> {some, {SabKey, ByteIndex}}
    after Timeout ->
        none
    end.
