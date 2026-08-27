%% owner process for a shared array buffer block
-module(arc_rt_sab_ffi).

-export([spawn_owner/1, byte_length/1, read/1, read_part/3, write/3,
         update/4, grow/2, make_waiter_ref/0, wait_sync/4, wait_async/4,
         cancel/2, notify/3, take_wake/2, await_wake/1]).

%% receive after rejects larger timeouts
-define(MAX_RECV_MS, 16#FFFFFFFF).

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

take_waiters([], _Off, _Count, Woken, Kept) ->
    {lists:reverse(Woken), lists:reverse(Kept)};
take_waiters(Rest, _Off, 0, Woken, Kept) ->
    {lists:reverse(Woken), lists:reverse(Kept, Rest)};
take_waiters([{_, _, Off, _} = W | Rest], Off, Count, Woken, Kept) ->
    take_waiters(Rest, Off, Count - 1, [W | Woken], Kept);
take_waiters([W | Rest], Off, Count, Woken, Kept) ->
    take_waiters(Rest, Off, Count, Woken, [W | Kept]).

splice(Bytes, Off, Chunk) ->
    Size = byte_size(Chunk),
    <<Pre:Off/binary, _:Size/binary, Post/binary>> = Bytes,
    <<Pre/binary, Chunk/binary, Post/binary>>.

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

wait_async(Owner, WRef, Off, Expected) ->
    case call(Owner, {wait, WRef, Off, Expected, async}) of
        waiting -> waiting;
        not_equal -> not_equal_now
    end.

cancel(Owner, WRef) -> call(Owner, {cancel, WRef}).

notify(Owner, Off, Count) -> call(Owner, {notify, Off, Count}).

take_wake(Refs, TimeoutMs) ->
    Own = maps:from_keys(Refs, []),
    Timeout =
        if
            TimeoutMs < 0 -> 0;
            TimeoutMs > ?MAX_RECV_MS -> ?MAX_RECV_MS;
            true -> TimeoutMs
        end,
    receive
        {arc_sab_wake, WRef, async} when is_map_key(WRef, Own) -> {some, WRef}
    after Timeout ->
        none
    end.

await_wake(WRef) ->
    receive
        {arc_sab_wake, WRef, _Tag} -> nil
    end.
