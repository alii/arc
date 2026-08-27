-module(emit_2core_harness_ffi).
-export([capture_stdout/1, to_dynamic/1,
         seed_random/1, next_random/0,
         buf_reset/0, buf_push/1, buf_read/0, err_push/1, err_read/0,
         env_is_truthy/1]).

env_is_truthy(Name) ->
    case os:getenv(binary_to_list(Name)) of
        "1" -> true;
        "true" -> true;
        _ -> false
    end.

to_dynamic(X) -> X.

capture_stdout(Thunk) ->
    OldGL = erlang:group_leader(),
    Collector = spawn(fun() -> collector_loop(<<>>) end),
    true = erlang:group_leader(Collector, self()),
    Result = try Thunk() after erlang:group_leader(OldGL, self()) end,
    Ref = make_ref(),
    Collector ! {get_output, self(), Ref},
    receive {Ref, Output} -> {Output, Result} end.

collector_loop(Acc) ->
    receive
        {io_request, From, ReplyAs, Req} ->
            {Reply, NewAcc} = handle_io(Req, Acc),
            From ! {io_reply, ReplyAs, Reply},
            collector_loop(NewAcc);
        {get_output, From, Ref} ->
            From ! {Ref, Acc}
    end.

handle_io({put_chars, unicode, Chars}, Acc) ->
    {ok, <<Acc/binary, (unicode:characters_to_binary(Chars))/binary>>};
handle_io({put_chars, unicode, M, F, A}, Acc) ->
    {ok, <<Acc/binary, (unicode:characters_to_binary(apply(M, F, A)))/binary>>};
handle_io({put_chars, latin1, Chars}, Acc) ->
    {ok, <<Acc/binary, (iolist_to_binary(Chars))/binary>>};
handle_io({put_chars, latin1, M, F, A}, Acc) ->
    {ok, <<Acc/binary, (iolist_to_binary(apply(M, F, A)))/binary>>};
handle_io({requests, Reqs}, Acc) ->
    lists:foldl(fun(R, {_, A}) -> handle_io(R, A) end, {ok, Acc}, Reqs);
handle_io(_Other, Acc) ->
    {ok, Acc}.

%% xorshift64*, float in [0,1)
seed_random(Seed) ->
    erlang:put(emit_2core_rand, Seed band 16#FFFFFFFFFFFFFFFF),
    nil.

next_random() ->
    S0 = case erlang:get(emit_2core_rand) of
        undefined -> 16#193A6754A8A7D469;
        V -> V
    end,
    S1 = (S0 bxor (S0 bsr 12)) band 16#FFFFFFFFFFFFFFFF,
    S2 = (S1 bxor (S1 bsl 25)) band 16#FFFFFFFFFFFFFFFF,
    S3 = (S2 bxor (S2 bsr 27)) band 16#FFFFFFFFFFFFFFFF,
    erlang:put(emit_2core_rand, S3),
    Mixed = (S3 * 16#2545F4914F6CDD1D) band 16#FFFFFFFFFFFFFFFF,
    %% top 53 bits over 2^53
    (Mixed bsr 11) / 9007199254740992.0.

buf_reset() ->
    erlang:put(emit_2core_buf, []),
    erlang:put(emit_2core_err, []),
    nil.

buf_push(Line) -> push(emit_2core_buf, Line).

err_push(Line) -> push(emit_2core_err, Line).

buf_read() -> read(emit_2core_buf).

err_read() -> read(emit_2core_err).

push(Key, Line) ->
    Cur = case erlang:get(Key) of
        undefined -> [];
        V -> V
    end,
    erlang:put(Key, [<<Line/binary, "\n">> | Cur]),
    nil.

read(Key) ->
    case erlang:get(Key) of
        undefined -> <<>>;
        V -> iolist_to_binary(lists:reverse(V))
    end.
