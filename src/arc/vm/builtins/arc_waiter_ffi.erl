%% Cross-process waiterlist for Atomics.wait / Atomics.notify / waitAsync
%% (ES2024 §25.4.3.14 DoWait, §25.4.11 Atomics.notify).
%%
%% DATA-ONLY MODULE — this is the shared waiterlist REGISTRY — pure ETS operations, zero
%% event-driven mailbox interaction. It never executes a `receive` for
%% wake messages and never sends one. (Its ONLY receive is the boot-time
%% sync-join handshake in start_registry/0, plus the table owner's own idle
%% loop; neither is part of the wake protocol.) The blocking wait (the old
%% await_notify), the bounded dry-queue receive (the old wait_for_notify)
%% and the actual `Pid ! {arc_notify, Ref, Key, ByteIndex}` wake sends all
%% live in EMBEDDER FFI (test/test262_exec_ffi.erl for the test262
%% harness, the in-tree reference embedder), driven through the sync_wait /
%% deliver_wake capabilities of the realm's HostHooks record.
%%
%% Registry: a public named ETS ordered_set, keyed {SabKey, ByteIndex, Seq}
%% where Seq = erlang:unique_integer([monotonic]) — ordered_set iteration
%% over the (SabKey, ByteIndex) prefix is therefore FIFO, matching the
%% spec's waiterlist ordering. The value is {Pid, Ref, IsAsync}. The table
%% is owned by a dedicated long-lived process so it survives the death of
%% whichever (possibly short-lived) process first touched it. It is created
%% ONCE per node, explicitly, by start_registry/0 at realm boot
%% (interpreter.new_state) — never lazily from a waiterlist operation, so
%% every function below is a plain ETS access that cannot fail with
%% "the table doesn't exist yet".
%%
%% Wake protocol — who may message a waiter:
%%   * Only the process that successfully ets:take/2-s an entry may have
%%     the {arc_notify, Ref, SabKey, ByteIndex} message sent for it. take
%%     is atomic, so an entry is claimed at most once. take_waiters RETURNS
%%     the claimed remote waiters; the claimer's embedder performs the
%%     sends (deliver_wake). A notifier that has NO deliver_wake capability
%%     must therefore never call take_waiters — it would claim (and count)
%%     remote waiters it cannot wake, blocking them forever. Such a
%%     notifier calls take_self_async_tokens/3 instead, which claims only
%%     the calling process's own async tokens (settled in-core, no message).
%%   * A blocked sync waiter that times out tries to ets:take its OWN key
%%     (in the embedder's await_notify). If the take succeeds nobody
%%     claimed it -> "timed-out". If the take finds nothing, a notifier
%%     claimed the entry and its message is in flight -> the embedder does
%%     a safety-bounded receive for it and reports "ok". This resolves the
%%     notify-vs-timeout race without a lock.
%%   * Entries whose Pid is no longer alive (a killed test worker left
%%     them behind) are dropped without being counted as woken.
%%
%% Async (waitAsync) waiters are registered as interchangeable TOKENS: the
%% promise itself lives on the registering agent's State.atomics_waiters
%% (FIFO); the ETS entry only makes the waiter visible to notifiers in
%% other processes and lets notify count it. Same-process notify settles
%% those directly (take_waiters reports them as SelfAsync); cross-process
%% notify wakes arrive in the owning EMBEDDER's mailbox and are injected
%% into core via event_loop.inject_notify, which settles the first matching
%% State waiter. A wake message names no waiter, so what has to stay
%% balanced is the COUNT of a process's live tokens against its count of
%% pending State waiters — hence one primitive, take_self_async_tokens/3,
%% for both "notify settles my own tokens" (budget = count) and "an expiring
%% waiter withdraws one of mine" (budget = 1). Withdrawing "one of ours"
%% rather than "this specific one" is what keeps that balance: a waiter that
%% withdrew ITS OWN entry after a notifier had claimed a DIFFERENT one would
%% settle itself "ok" and still leave the notifier's in-flight wake to settle
%% a second waiter — two wakeups for one claim.
%%
%% Async timeout vs. cross-process claim: take_self_async_tokens/3 finding
%% none of ours means a notifier atomically claimed one first and counted us
%% as woken, so the expiring waiter settles "ok" rather than "timed-out" and
%% the notifier's count stays truthful. The remaining, accepted approximation
%% is only in the ORDER of the two: the notifier's in-flight wake message
%% then finds no pending State waiter and is dropped. The spec closes even
%% that with the agent critical section; a lock-free mailbox protocol cannot.
%%
%% cancel_waiter/1 (SYNC entries, whose handle its own waiter holds for the
%% duration of the blocking wait) reports which side won as
%% `withdrew | already_claimed`.
-module(arc_waiter_ffi).

-export([local_buffer_key/1, shared_buffer_key/1, start_registry/0,
         insert_waiter/3, cancel_waiter/1, take_waiters/3,
         take_self_async_tokens/3]).

-define(TAB, arc_atomics_waiterlist).

%% Process-local buffer identity fallback: the heap ref id scoped by the
%% owning process. Used for buffers without process-independent shared
%% storage; pid-scoping keeps concurrently running tests with colliding
%% small heap ids apart. The rest of this module is key-agnostic.
local_buffer_key(Id) ->
    {self(), Id}.

%% Cross-process buffer identity: the Erlang `atomics` ref backing a
%% SharedArrayBuffer (see arc_sab_ffi.erl). Refs are globally unique and
%% pass between processes by reference, so every agent observing the same
%% SAB derives the same waiterlist key.
shared_buffer_key(AtomicsRef) ->
    {shared, AtomicsRef}.

%% Register the calling process as a waiter on (SabKey, ByteIndex).
%% IsAsync is false for a blocking Atomics.wait, true for a waitAsync
%% token. Returns the opaque handle ({EtsKey, MsgRef}) that uniquely names
%% THIS entry — used by cancel_waiter/1 and by the embedder's blocking
%% wait. Requires start_registry/0 to have run in this node.
insert_waiter(SabKey, ByteIndex, IsAsync) ->
    Seq = erlang:unique_integer([monotonic]),
    Ref = make_ref(),
    Key = {SabKey, ByteIndex, Seq},
    true = ets:insert(?TAB, {Key, self(), Ref, IsAsync}),
    {Key, Ref}.

%% Withdraw the exact entry named by Handle — a SYNC waiter's, whose caller
%% holds the handle for the whole wait: its post-insert re-read saw a
%% different value ("not-equal"), or an error unwound out of that re-read
%% (detached / shrunk buffer). DATA-ONLY: just the ets:take, no flush receive
%% — but the caller MUST know whether a notifier already claimed the entry,
%% because the claimer's {arc_notify, Ref, ...} wake message is then in flight
%% to our mailbox and would otherwise sit there forever, spuriously settling
%% the NEXT waiter this agent registers at the same (key, byte index)
%% (embedder loops match wakes by key+index, not ref).
%%
%% Returns `already_claimed` when a notifier got there first — core then
%% delegates the bounded flush receive to the embedder's sync_wait capability,
%% whose await_notify selectively receives on the exact Ref — and `withdrew`
%% when we removed the entry ourselves, so no wake can be in flight.
cancel_waiter({Key, _Ref}) ->
    case ets:take(?TAB, Key) of
        [_] -> withdrew;
        [] -> already_claimed
    end.

%% Atomics.notify with a wake-DELIVERY capability in hand: atomically claim
%% up to Count waiters on (SabKey, ByteIndex) in FIFO order. Returns
%% {Claimed, SelfAsyncTaken}: Claimed is the list of claimed REMOTE waiters
%% as opaque {Pid, Ref, SabKey, ByteIndex} terms — the caller routes them to
%% the embedder's deliver_wake capability, which owns the message sends —
%% and SelfAsyncTaken counts our own waitAsync tokens, which the caller
%% settles directly on State.
%%
%% Claiming a remote waiter is a PROMISE to wake it, so this function is
%% only reachable from a notifier holding the deliver_wake capability; a
%% notifier without one calls take_self_async_tokens/3.
take_waiters(SabKey, ByteIndex, Count) ->
    {Claimed, SelfAsync} =
        take_loop(first_key(SabKey, ByteIndex), SabKey, ByteIndex, Count,
                  self(), [], 0),
    {lists:reverse(Claimed), SelfAsync}.

take_loop(none, _S, _B, _N, _Self, Claimed, SelfAsync) ->
    {Claimed, SelfAsync};
take_loop(_Key, _S, _B, 0, _Self, Claimed, SelfAsync) ->
    {Claimed, SelfAsync};
take_loop(Key, S, B, N, Self, Claimed, SelfAsync) ->
    Next = next_key(Key, S, B),
    case ets:take(?TAB, Key) of
        [] ->
            %% Another notifier took this entry between our scan and take.
            take_loop(Next, S, B, N, Self, Claimed, SelfAsync);
        [{_, Self, _Ref, true}] ->
            %% Our own waitAsync token: caller settles it from State.
            take_loop(Next, S, B, N - 1, Self, Claimed, SelfAsync + 1);
        [{_, Self, _Ref, false}] ->
            %% Our own SYNC entry — impossible while we are running notify
            %% (a sync waiter blocks the whole process). Stale by
            %% construction; drop without counting.
            take_loop(Next, S, B, N, Self, Claimed, SelfAsync);
        [{_, Pid, Ref, _IsAsync}] ->
            case is_process_alive(Pid) of
                true ->
                    %% Claimed: counts as woken. Delivery is the embedder's
                    %% (deliver_wake) — no send here.
                    take_loop(Next, S, B, N - 1, Self,
                              [{Pid, Ref, S, B} | Claimed], SelfAsync);
                false ->
                    %% Leftover from a killed process: not a waiter.
                    take_loop(Next, S, B, N, Self, Claimed, SelfAsync)
            end
    end.

%% Claim up to Count of the calling process's OWN waitAsync tokens on
%% (SabKey, ByteIndex), FIFO, and return how many were taken. Two callers,
%% one operation:
%%
%%   * Atomics.notify WITHOUT a wake-delivery capability. Other processes'
%%     entries are left registered and untouched: we could not deliver their
%%     wake message, and a claimed-but-undeliverable waiter is a waiter that
%%     blocks forever. Settling our own tokens needs no message at all — the
%%     caller fulfils their promises straight from State.
%%   * An expiring waitAsync waiter withdrawing itself (Count = 1). Tokens
%%     are fungible, so "one of mine" is the correct budget; a return of 0
%%     means a notifier claimed one first (and counted us as woken).
%%
%% Removal is an ets:take/2 — atomic. Candidate keys are identified with a
%% lookup first, so a blind take can never unregister a waiter belonging to
%% another process. A key's value never changes once inserted (the Seq
%% component makes every entry's key unique), so a key that read as ours can
%% only ever be taken as ours or found already gone (a notifier in another
%% process claimed it, and counted it, first).
take_self_async_tokens(SabKey, ByteIndex, Count) ->
    self_async_loop(first_key(SabKey, ByteIndex), SabKey, ByteIndex, Count,
                    self(), 0).

self_async_loop(none, _S, _B, _N, _Self, Taken) ->
    Taken;
self_async_loop(_Key, _S, _B, 0, _Self, Taken) ->
    Taken;
self_async_loop(Key, S, B, N, Self, Taken) ->
    Next = next_key(Key, S, B),
    case ets:lookup(?TAB, Key) of
        [{Key, Self, _Ref, true}] ->
            case ets:take(?TAB, Key) of
                [_] ->
                    self_async_loop(Next, S, B, N - 1, Self, Taken + 1);
                [] ->
                    %% A notifier claimed it between our lookup and our take.
                    self_async_loop(Next, S, B, N, Self, Taken)
            end;
        _ ->
            self_async_loop(Next, S, B, N, Self, Taken)
    end.

%% ---------------------------------------------------------------------
%% FIFO scan over the (SabKey, ByteIndex) prefix of the ordered_set.
%% Sequence numbers are integers; -1.0e308 sorts below all of them, so
%% ets:next from {S, B, -1.0e308} lands on the prefix's first live key.
%% ---------------------------------------------------------------------

first_key(S, B) ->
    check_key(ets:next(?TAB, {S, B, -1.0e308}), S, B).

next_key(Key, S, B) ->
    check_key(ets:next(?TAB, Key), S, B).

check_key({S, B, _} = Key, S, B) -> Key;
check_key(_, _, _) -> none.

%% ---------------------------------------------------------------------
%% Registry lifecycle. Called ONCE per realm boot (interpreter.new_state,
%% at the same seam that reads arc_agent_ffi:can_block/0) — never lazily
%% from a waiterlist operation, so no hot-path function has to carry a
%% "table might not exist" failure mode.
%%
%% The table is owned by a dedicated process so it is not torn down when
%% its creator (e.g. a per-test worker) exits. Creation races between
%% concurrently booting agent processes resolve inside ets:new/2 itself:
%% creating a NAMED table whose name is taken raises badarg, and by the
%% time it does the table exists — so both the winner and every loser can
%% report "ready" and every caller returns with the table live. The join is
%% synchronous (a monitored spawn + one ack), so no caller ever races ahead
%% of the table's existence and no poll/timeout heuristic is needed.
%% ---------------------------------------------------------------------

start_registry() ->
    case ets:whereis(?TAB) of
        undefined -> spawn_owner();
        _ -> nil
    end.

spawn_owner() ->
    Caller = self(),
    Tag = make_ref(),
    %% Monitored, NOT linked: the owner must outlive this caller, but a
    %% caller must not hang if the owner dies before acking.
    {Pid, Mon} = spawn_monitor(fun() ->
        try ets:new(?TAB, [ordered_set, public, named_table,
                           {write_concurrency, true},
                           {read_concurrency, true}]) of
            ?TAB ->
                Caller ! {Tag, ready},
                owner_loop()
        catch
            error:badarg ->
                %% badarg means "the name is taken" — USUALLY because another
                %% owner won the creation race, in which case the table exists
                %% NOW and we may ack. But badarg is also what a bad option or
                %% an exhausted ETS table limit raises, and acking THOSE would
                %% hand every caller a table that does not exist. So look
                %% before acking: only the racing-owner reading of badarg
                %% leaves ?TAB alive.
                case ets:whereis(?TAB) of
                    undefined -> Caller ! {Tag, {failed, badarg}};
                    _Tid -> Caller ! {Tag, ready}
                end
        end
    end),
    receive
        {Tag, ready} ->
            true = erlang:demonitor(Mon, [flush]),
            nil;
        {Tag, {failed, Reason}} ->
            true = erlang:demonitor(Mon, [flush]),
            erlang:error({arc_atomics_waiterlist_unavailable, Reason});
        {'DOWN', Mon, process, Pid, Reason} ->
            erlang:error({arc_atomics_waiterlist_unavailable, Reason})
    end.

%% The owner exists to hold the table, nothing more: it is unregistered,
%% nobody holds its pid, and it never terminates. It has no protocol, so it
%% just drains anything that somehow reaches its mailbox rather than letting
%% it accumulate and slow every later selective receive down.
owner_loop() ->
    receive
        _ -> owner_loop()
    end.
