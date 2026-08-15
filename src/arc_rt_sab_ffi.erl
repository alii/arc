-module(arc_rt_sab_ffi).

%% Owner process of a SharedArrayBuffer's Shared Data Block (ES2024 §6.2.9,
%% §25.4 Atomics). Once a SAB may be seen by more than one agent its bytes
%% and its WaiterList move out of the creating agent's store into ONE
%% process; every agent that holds the buffer holds this pid, and every
%% read / write / read-modify-write / wait / notify on the block is a
%% synchronous message to it. The owner serialises them, which is the
%% spec's critical section: nothing here needs atomics refs, ETS or the
%% process dictionary.
%%
%% Protocol (client -> owner: {sab, From, CallRef, Request}; owner ->
%% client: {CallRef, Reply}):
%%
%%   length                          -> ByteLength
%%   read                            -> Bytes
%%   {part, Off, Size}               -> binary Size bytes at Off
%%   {write, Off, Chunk}             -> nil        splice Chunk in at Off
%%   {update, Off, Size, Fun}        -> Reply      {Reply, New} = Fun(Old)
%%   {grow, NewLen}                  -> {ok,nil} | {error,nil} (shrink)
%%   {wait, WRef, Off, Expected, Tag}-> not_equal | waiting
%%   {cancel, WRef}                  -> cancelled | already_woken
%%   {notify, Off, Count}            -> N woken
%%
%% A registered waiter is {Pid, WRef, Off, Tag}. Waking it sends
%% Pid ! {arc_sab_wake, WRef, Tag}. Timeouts are the WAITER's business: it
%% blocks (sync) or tracks a deadline (async) itself and withdraws with
%% `cancel`; `already_woken` tells it the wake message is already in its
%% mailbox (same sender, so it precedes the cancel reply), which resolves
%% the notify-vs-timeout race without any shared table.
%%
%% The owner monitors the process that created it and exits when that
%% process goes away.

-export([spawn_owner/1, byte_length/1, read/1, read_part/3, write/3,
         update/4, grow/2, make_waiter_ref/0, wait_sync/4, wait_async/4,
         cancel/2, notify/3, take_wake/1, await_wake/1]).

%% erlang `receive ... after` rejects timeouts above 16#FFFFFFFF.
-define(MAX_RECV_MS, 16#FFFFFFFF).

%% -- owner ------------------------------------------------------------------

spawn_owner(Bytes) ->
    Creator = self(),
    spawn(fun() ->
        MRef = erlang:monitor(process, Creator),
        loop(Bytes, [], MRef)
    end).

loop(Bytes, Waiters, Creator) ->
    receive
        {sab, From, Ref, Req} ->
            {Reply, Bytes1, Waiters1} = handle(Req, From, Bytes, Waiters),
            From ! {Ref, Reply},
            loop(Bytes1, Waiters1, Creator);
        {'DOWN', Creator, process, _Pid, _Reason} ->
            ok
    end.

handle(length, _From, Bytes, Waiters) ->
    {byte_size(Bytes), Bytes, Waiters};
handle(read, _From, Bytes, Waiters) ->
    {Bytes, Bytes, Waiters};
handle({part, Off, Size}, _From, Bytes, Waiters) ->
    {binary:part(Bytes, Off, Size), Bytes, Waiters};
handle({write, Off, Chunk}, _From, Bytes, Waiters) ->
    {nil, splice(Bytes, Off, Chunk), Waiters};
handle({update, Off, Size, Fun}, _From, Bytes, Waiters) ->
    Old = binary:part(Bytes, Off, Size),
    {Reply, New} = Fun(Old),
    Size = byte_size(New),
    {Reply, splice(Bytes, Off, New), Waiters};
handle({grow, NewLen}, _From, Bytes, Waiters) ->
    Cur = byte_size(Bytes),
    case NewLen < Cur of
        true -> {{error, nil}, Bytes, Waiters};
        false ->
            Pad = (NewLen - Cur) * 8,
            {{ok, nil}, <<Bytes/binary, 0:Pad>>, Waiters}
    end;
handle({wait, WRef, Off, Expected, Tag}, From, Bytes, Waiters) ->
    case binary:part(Bytes, Off, byte_size(Expected)) of
        Expected ->
            {waiting, Bytes, Waiters ++ [{From, WRef, Off, Tag}]};
        _ ->
            {not_equal, Bytes, Waiters}
    end;
handle({cancel, WRef}, _From, Bytes, Waiters) ->
    case lists:keytake(WRef, 2, Waiters) of
        {value, _W, Rest} -> {cancelled, Bytes, Rest};
        false -> {already_woken, Bytes, Waiters}
    end;
handle({notify, Off, Count}, _From, Bytes, Waiters) ->
    {Woken, Rest} = take_waiters(Waiters, Off, Count, [], []),
    lists:foreach(
        fun({Pid, WRef, _Off, Tag}) -> Pid ! {arc_sab_wake, WRef, Tag} end,
        Woken),
    {length(Woken), Bytes, Rest}.

%% Up to Count waiters at Off, FIFO (§25.4.3.9 RemoveWaiters).
take_waiters([], _Off, _Count, Woken, Kept) ->
    {lists:reverse(Woken), lists:reverse(Kept)};
take_waiters(Rest, _Off, 0, Woken, Kept) ->
    {lists:reverse(Woken), lists:reverse(Kept, Rest)};
take_waiters([{_, _, Off, _} = W | Rest], Off, Count, Woken, Kept) ->
    take_waiters(Rest, Off, Count - 1, [W | Woken], Kept);
take_waiters([W | Rest], Off, Count, Woken, Kept) ->
    take_waiters(Rest, Off, Count, Woken, [W | Kept]).

%% Replace byte_size(Chunk) bytes at Off. Every caller validated the range
%% against the live length and the block never shrinks, so a range past the
%% end is a caller bug: badarg rather than a silent partial write.
splice(Bytes, Off, Chunk) ->
    Size = byte_size(Chunk),
    <<Pre:Off/binary, _:Size/binary, Post/binary>> = Bytes,
    <<Pre/binary, Chunk/binary, Post/binary>>.

%% -- client -----------------------------------------------------------------

call(Owner, Req) ->
    MRef = erlang:monitor(process, Owner),
    Owner ! {sab, self(), MRef, Req},
    receive
        {MRef, Reply} ->
            erlang:demonitor(MRef, [flush]),
            Reply;
        {'DOWN', MRef, process, _Pid, Reason} ->
            erlang:error({shared_array_buffer_owner_down, Reason})
    end.

byte_length(Owner) -> call(Owner, length).

read(Owner) -> call(Owner, read).

read_part(Owner, Off, Size) -> call(Owner, {part, Off, Size}).

write(Owner, Off, Chunk) -> call(Owner, {write, Off, Chunk}).

update(Owner, Off, Size, Fun) -> call(Owner, {update, Off, Size, Fun}).

grow(Owner, NewLen) -> call(Owner, {grow, NewLen}).

make_waiter_ref() -> erlang:make_ref().

%% §25.4.3.14 DoWait, sync mode, steps 16-31: compare-and-add-waiter in the
%% owner's critical section, then SuspendThisAgent = block THIS process in a
%% selective receive for the wake. TimeoutMs < 0 is +infinity. Returns
%% woken | timed_out | not_equal.
wait_sync(Owner, Off, Expected, TimeoutMs) ->
    WRef = erlang:make_ref(),
    case call(Owner, {wait, WRef, Off, Expected, sync}) of
        not_equal -> not_equal;
        waiting -> block(Owner, WRef, TimeoutMs)
    end.

block(Owner, WRef, TimeoutMs) ->
    Timeout =
        if
            TimeoutMs < 0 -> infinity;
            TimeoutMs > ?MAX_RECV_MS -> ?MAX_RECV_MS;
            true -> TimeoutMs
        end,
    MRef = erlang:monitor(process, Owner),
    Result =
        receive
            {arc_sab_wake, WRef, _Tag} -> woken;
            {'DOWN', MRef, process, _Pid, Reason} ->
                erlang:error({shared_array_buffer_owner_down, Reason})
        after Timeout ->
            case call(Owner, {cancel, WRef}) of
                cancelled -> timed_out;
                already_woken -> await_wake(WRef), woken
            end
        end,
    erlang:demonitor(MRef, [flush]),
    Result.

%% §25.4.3.14 DoWait, async mode: compare-and-add-waiter only. The wake
%% arrives later as {arc_sab_wake, WRef, async} in this process's mailbox,
%% where the waiting agent's drain (arc/rt/async) takes it.
wait_async(Owner, WRef, Off, Expected) ->
    case call(Owner, {wait, WRef, Off, Expected, async}) of
        waiting -> waiting;
        not_equal -> not_equal_now
    end.

cancel(Owner, WRef) -> call(Owner, {cancel, WRef}).

notify(Owner, Off, Count) -> call(Owner, {notify, Off, Count}).

%% Dequeue the oldest async wake addressed to this process, blocking at
%% most TimeoutMs (negative = only what is already queued). A sync waiter's
%% wake never sits here unclaimed: block/3 consumes its own, by ref.
%% Gleam: Option(WaiterRef).
take_wake(TimeoutMs) ->
    Timeout =
        if
            TimeoutMs < 0 -> 0;
            TimeoutMs > ?MAX_RECV_MS -> ?MAX_RECV_MS;
            true -> TimeoutMs
        end,
    receive
        {arc_sab_wake, WRef, async} -> {some, WRef}
    after Timeout ->
        none
    end.

%% Consume the wake for WRef that a notifier has already sent (the owner
%% answered `already_woken`), so it cannot be mistaken for a later one.
await_wake(WRef) ->
    receive
        {arc_sab_wake, WRef, _Tag} -> nil
    end.
