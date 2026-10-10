-module(spark_encoding_ffi).
-export([decode_utf8/3]).

%% whatwg utf-8 decode: each maximal invalid prefix becomes one U+FFFD, or
%% the whole decode fails when Fatal. When Stream, a sequence cut off at the
%% end is left undecoded and its byte count returned.
decode_utf8(Bin, Fatal, Stream) ->
    decode(Bin, [], Fatal, Stream).

decode(Bin, Acc, Fatal, Stream) ->
    case unicode:characters_to_binary(Bin, utf8, utf8) of
        Text when is_binary(Text) ->
            done([Text | Acc], 0);
        {Stopped, Good, Rest} ->
            Skip = bad_prefix(Rest),
            %% erlang also calls a lead byte with a wrong second byte
            %% incomplete; only a whole valid prefix can wait for more
            Waits = Stopped =:= incomplete andalso Stream
                andalso Skip =:= byte_size(Rest),
            if
                Waits ->
                    done([Good | Acc], byte_size(Rest));
                Fatal ->
                    {error, nil};
                true ->
                    <<_:Skip/binary, More/binary>> = Rest,
                    decode(More, [<<16#FFFD/utf8>>, Good | Acc], Fatal, Stream)
            end
    end.

done(Acc, Leftover) ->
    {ok, {iolist_to_binary(lists:reverse(Acc)), Leftover}}.

%% bytes of the longest prefix that could still have started a valid sequence
bad_prefix(<<Lead, Rest/binary>>) when Lead >= 16#E0, Lead =< 16#EF ->
    {Low, High} = second_range(Lead),
    case Rest of
        <<B, _/binary>> when B >= Low, B =< High -> 2;
        _ -> 1
    end;
bad_prefix(<<Lead, Rest/binary>>) when Lead >= 16#F0, Lead =< 16#F4 ->
    {Low, High} = second_range(Lead),
    case Rest of
        <<B, C, _/binary>> when B >= Low, B =< High, C >= 16#80, C =< 16#BF -> 3;
        <<B, _/binary>> when B >= Low, B =< High -> 2;
        _ -> 1
    end;
bad_prefix(_) ->
    1.

second_range(16#E0) -> {16#A0, 16#BF};
second_range(16#ED) -> {16#80, 16#9F};
second_range(16#F0) -> {16#90, 16#BF};
second_range(16#F4) -> {16#80, 16#8F};
second_range(_) -> {16#80, 16#BF}.
