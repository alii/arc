-module(arc_float_ffi).
-export([parse_float/1]).

parse_float(S) ->
    Norm = normalize(S),
    case try_binary_to_float(Norm) of
        {ok, F} -> {ok, F};
        %% badarg on valid float syntax can only be overflow
        error ->
            case is_float_syntax(Norm) of
                true -> {error, out_of_range};
                false -> {error, invalid}
            end
    end.

try_binary_to_float(S) ->
    try
        {ok, erlang:binary_to_float(S)}
    catch
        error:badarg -> error
    end.

%% pad ".5" "1." "1e10" into the shape binary_to_float accepts
normalize(S) ->
    {Mantissa, Exponent} = split_exponent(S),
    {Sign, Digits} = take_sign(Mantissa),
    <<Sign/binary, (pad_mantissa(Digits))/binary, Exponent/binary>>.

split_exponent(S) ->
    case binary:match(S, [<<"e">>, <<"E">>]) of
        {Pos, _Len} ->
            <<Mantissa:Pos/binary, Exponent/binary>> = S,
            {Mantissa, Exponent};
        nomatch ->
            {S, <<>>}
    end.

take_sign(<<C, Rest/binary>>) when C =:= $+; C =:= $- -> {<<C>>, Rest};
take_sign(S) -> {<<>>, S}.

pad_mantissa(<<>>) ->
    <<>>;
pad_mantissa(<<".", _/binary>> = M) ->
    pad_mantissa(<<"0", M/binary>>);
pad_mantissa(M) ->
    case binary:match(M, <<".">>) of
        nomatch -> <<M/binary, ".0">>;
        _ ->
            case binary:last(M) of
                $. -> <<M/binary, "0">>;
                _ -> M
            end
    end.

is_float_syntax(S0) ->
    S1 = skip_sign(S0),
    case take_digits(S1) of
        {true, <<".", S2/binary>>} ->
            case take_digits(S2) of
                {true, <<>>} -> true;
                {true, <<E, S3/binary>>} when E =:= $e; E =:= $E ->
                    case take_digits(skip_sign(S3)) of
                        {true, <<>>} -> true;
                        _ -> false
                    end;
                _ -> false
            end;
        _ -> false
    end.

skip_sign(<<C, Rest/binary>>) when C =:= $+; C =:= $- -> Rest;
skip_sign(S) -> S.

take_digits(S) -> take_digits(S, false).
take_digits(<<D, Rest/binary>>, _) when D >= $0, D =< $9 ->
    take_digits(Rest, true);
take_digits(S, Seen) ->
    {Seen, S}.
