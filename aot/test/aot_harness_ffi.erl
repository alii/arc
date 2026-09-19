-module(aot_harness_ffi).
-export([to_dynamic/1,
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

%% xorshift64*, float in [0,1)
seed_random(Seed) ->
    erlang:put(aot_rand, Seed band 16#FFFFFFFFFFFFFFFF),
    nil.

next_random() ->
    S0 = case erlang:get(aot_rand) of
        undefined -> 16#193A6754A8A7D469;
        V -> V
    end,
    S1 = (S0 bxor (S0 bsr 12)) band 16#FFFFFFFFFFFFFFFF,
    S2 = (S1 bxor (S1 bsl 25)) band 16#FFFFFFFFFFFFFFFF,
    S3 = (S2 bxor (S2 bsr 27)) band 16#FFFFFFFFFFFFFFFF,
    erlang:put(aot_rand, S3),
    Mixed = (S3 * 16#2545F4914F6CDD1D) band 16#FFFFFFFFFFFFFFFF,
    %% top 53 bits over 2^53
    (Mixed bsr 11) / 9007199254740992.0.

buf_reset() ->
    erlang:put(aot_buf, []),
    erlang:put(aot_err, []),
    nil.

buf_push(Line) -> push(aot_buf, Line).

err_push(Line) -> push(aot_err, Line).

buf_read() -> read(aot_buf).

err_read() -> read(aot_err).

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
