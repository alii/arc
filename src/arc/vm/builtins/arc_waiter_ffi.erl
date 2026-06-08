%% Cross-process waiterlist for Atomics.wait / Atomics.notify / waitAsync
%% (ES2024 §25.4.3.14 DoWait, §25.4.11 Atomics.notify).
%%
%% DATA-ONLY MODULE (host-capability contract clause 4, see arc/host.gleam):
%% this is the shared waiterlist REGISTRY — pure ETS operations, zero
%% event-driven mailbox interaction. It never executes a `receive` for
%% wake messages and never sends one. The blocking wait (the old
%% await_notify), the bounded dry-queue receive (the old wait_for_notify)
%% and the actual `Pid ! {arc_notify, Ref, Key, ByteIndex}` wake sends all
%% live in EMBEDDER FFI (arc_beam_ffi.erl for the beam embedder, the
%% test262 harness FFI for the test runner), driven through the
%% State-installed host_sync_wait / host_deliver_wake capabilities.
%%
%% Registry: a public named ETS ordered_set, keyed {SabKey, ByteIndex, Seq}
%% where Seq = erlang:unique_integer([monotonic]) — ordered_set iteration
%% over the (SabKey, ByteIndex) prefix is therefore FIFO, matching the
%% spec's waiterlist ordering. The value is {Pid, Ref, IsAsync}. The table
%% is owned by a dedicated long-lived process so it survives the death of
%% whichever (possibly short-lived) process first touched it.
%%
%% Wake protocol — who may message a waiter:
%%   * Only the process that successfully ets:take/2-s an entry may have
%%     the {arc_notify, Ref, SabKey, ByteIndex} message sent for it. take
%%     is atomic, so an entry is claimed at most once. take_waiters RETURNS
%%     the claimed remote waiters; the claimer's embedder performs the
%%     sends (host_deliver_wake).
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
%% the State waiter directly (take_waiters reports those as SelfAsync);
%% cross-process notify wakes arrive in the owning EMBEDDER's mailbox and
%% are injected into core via event_loop.inject_notify, which settles the
%% first matching State waiter. Because tokens at the same
%% (SabKey, ByteIndex, Pid) are interchangeable, settling FIFO from State
%% is equivalent to tracking entry identity.
%%
%% Known approximation (documented, accepted): if an async waiter's timeout
%% expires in the same instant a notifier in another process claims its
%% token, the waiter settles "timed-out" while the in-flight notify message
%% finds no pending State waiter and is dropped — the notifier may
%% overcount by one in that window. The spec closes this with the agent
%% critical section; a lock-free mailbox protocol cannot, so we accept the
%% race.
-module(arc_waiter_ffi).

-export([local_buffer_key/1, shared_buffer_key/1, insert_waiter/3,
         cancel_waiter/1, take_waiters/3, remove_async_token/2]).

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
%% token. Returns the opaque handle ({EtsKey, MsgRef}) used by cancel and
%% by the embedder's blocking wait.
insert_waiter(SabKey, ByteIndex, IsAsync) ->
    ok = ensure_table(),
    Seq = erlang:unique_integer([monotonic]),
    Ref = make_ref(),
    Key = {SabKey, ByteIndex, Seq},
    true = ets:insert(?TAB, {Key, self(), Ref, IsAsync}),
    {Key, Ref}.

%% Drop our own entry (the post-insert re-read saw a different value ->
%% "not-equal"). DATA-ONLY: just the ets:take, no flush receive — but the
%% caller MUST know whether a notifier already claimed the entry, because
%% the claimer's {arc_notify, Ref, ...} wake message is then in flight to
%% our mailbox and would otherwise sit there forever, spuriously settling
%% the NEXT waiter this agent registers at the same (key, byte index)
%% (embedder loops match wakes by key+index, not ref). Returns `true` when
%% the entry was already claimed — core then delegates the bounded flush
%% receive to the embedder's host_sync_wait capability (whose await_notify
%% selectively receives on the exact Ref) — and `false` when we removed
%% the entry ourselves and no wake can be in flight.
cancel_waiter({Key, _Ref}) ->
    case ets:take(?TAB, Key) of
        [_] -> false;
        [] -> true
    end.

%% Atomics.notify: atomically claim up to Count waiters on
%% (SabKey, ByteIndex) in FIFO order. Returns {Claimed, SelfAsyncTaken}:
%% Claimed is the list of claimed REMOTE waiters as opaque
%% {Pid, Ref, SabKey, ByteIndex} terms — the caller routes them to the
%% embedder's host_deliver_wake capability, which owns the message sends —
%% and SelfAsyncTaken counts our own waitAsync tokens, which the caller
%% settles directly on State.
take_waiters(SabKey, ByteIndex, Count) ->
    ok = ensure_table(),
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
                    %% (host_deliver_wake) — no send here.
                    take_loop(Next, S, B, N - 1, Self,
                              [{Pid, Ref, S, B} | Claimed], SelfAsync);
                false ->
                    %% Leftover from a killed process: not a waiter.
                    take_loop(Next, S, B, N, Self, Claimed, SelfAsync)
            end
    end.

%% Remove one of our own async tokens for (SabKey, ByteIndex) — the
%% matching State waiter expired and settled "timed-out". If a notifier
%% claimed the token first its message is dropped by the embedder loop (no
%% pending State waiter matches); see the race note in the module doc.
remove_async_token(SabKey, ByteIndex) ->
    ok = ensure_table(),
    remove_async_loop(first_key(SabKey, ByteIndex), SabKey, ByteIndex,
                      self()).

remove_async_loop(none, _S, _B, _Self) ->
    nil;
remove_async_loop(Key, S, B, Self) ->
    Next = next_key(Key, S, B),
    case ets:lookup(?TAB, Key) of
        [{Key, Self, _Ref, true}] ->
            true = ets:delete(?TAB, Key),
            nil;
        _ ->
            remove_async_loop(Next, S, B, Self)
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
%% Lazy table creation, owned by a dedicated process so the table is not
%% torn down when its creator (e.g. a per-test worker) exits. Creation
%% races resolve via register/2: exactly one spawned owner wins; losers
%% ack immediately and the caller polls until the winner's table exists.
%%
%% BOUNDARY JUSTIFICATION: the single `receive` below is a synchronous
%% spawn-ack JOIN on table creation (a one-shot creation barrier, same
%% category as arc_vm_ffi:run_compile_task's spawn-compute-join), not an
%% event-driven mailbox interaction — no wake/notify/IO message ever
%% arrives through it, and it completes before insert_waiter returns.
%% ---------------------------------------------------------------------

ensure_table() ->
    case ets:whereis(?TAB) of
        undefined -> start_owner();
        _ -> ok
    end.

start_owner() ->
    Caller = self(),
    Tag = make_ref(),
    %% Plain spawn (unlinked): the owner must outlive the caller.
    _Pid = spawn(fun() ->
        try register(arc_atomics_waiterlist_owner, self()) of
            true ->
                ?TAB = ets:new(?TAB, [ordered_set, public, named_table,
                                      {write_concurrency, true},
                                      {read_concurrency, true}]),
                Caller ! {Tag, ready},
                owner_loop()
        catch
            error:badarg ->
                %% Another owner exists (or is mid-creation).
                Caller ! {Tag, ready}
        end
    end),
    receive
        {Tag, ready} -> ok
    end,
    await_table(200).

await_table(0) ->
    erlang:error(arc_atomics_waiterlist_unavailable);
await_table(N) ->
    case ets:whereis(?TAB) of
        undefined ->
            timer:sleep(1),
            await_table(N - 1);
        _ ->
            ok
    end.

owner_loop() ->
    receive
        _ -> owner_loop()
    end.
