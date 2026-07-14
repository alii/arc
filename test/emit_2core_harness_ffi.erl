-module(emit_2core_harness_ffi).
-export([capture_stdout/1, to_dynamic/1,
         seed_random/1, next_random/0,
         buf_reset/0, buf_push/1, buf_read/0]).

%% Identity cast: any Gleam value -> gleam/dynamic.Dynamic. On the Erlang
%% target Dynamic is `term()`, so this is just identity.
to_dynamic(X) -> X.

%% Capture everything the thunk writes to stdout (io:put_chars / io:format,
%% which gleam io.println routes through) by installing a collector process
%% as this process's group_leader for the duration of the call.
%% Returns {CapturedBytes :: binary(), ThunkResult}.
%%
%% Used by emit_2core_harness.run_interpreted so arc's console.log output
%% (arc/vm/builtins/console.gleam -> io.println) can be diffed against the
%% compiled path's captured console bytes.
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
    %% getopts / setopts / get_geometry etc — acknowledge, capture nothing.
    {ok, Acc}.

%% ---------------------------------------------------------------------------
%% Deterministic-hooks state (SPEC §20). Process-dictionary-backed: each
%% test runs in its own BEAM process (arc_test_ffi:spawn_worker), so this
%% state is per-test-isolated with no explicit cleanup.
%% ---------------------------------------------------------------------------

%% Seeded PRNG: xorshift64* → Float in [0,1). Backs `HostHooks.random`.
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
    %% top 53 bits / 2^53 → uniform [0,1) with a full double mantissa
    (Mixed bsr 11) / 9007199254740992.0.

%% Print buffer: reversed list of utf8-binary lines. Backs
%% `HostHooks.print` / arc `report_uncaught`.
buf_reset() ->
    erlang:put(emit_2core_buf, []),
    nil.

buf_push(Line) ->
    Cur = case erlang:get(emit_2core_buf) of
        undefined -> [];
        V -> V
    end,
    %% One trailing newline per call so the compiled path's captured bytes
    %% match arc's io.println output byte-for-byte.
    erlang:put(emit_2core_buf, [<<Line/binary, "\n">> | Cur]),
    nil.

buf_read() ->
    case erlang:get(emit_2core_buf) of
        undefined -> <<>>;
        V -> iolist_to_binary(lists:reverse(V))
    end.
