-module(arc_float_ffi).
-export([parse_float/1]).

%% Decimal float literal → double. Sole caller: arc/parser/number.gleam.
%% Escape decoding for string/template literals lives in arc_escape_ffi.

%% Convert a decimal float/exponent literal to a double.
%% Returns {ok, Float};
%%         {error, out_of_range} when the text is valid float syntax but its
%%             magnitude overflows an IEEE double (binary_to_float raises
%%             badarg for overflow; underflow rounds to 0.0 and succeeds);
%%         {error, invalid} for text binary_to_float cannot parse at all.
%% The tags mirror number.gleam's `Result(Float, FloatParseError)`.
%%
%% A JS decimal literal is not quite what erlang:binary_to_float/1 accepts, so
%% the text is normalized ONCE up front — here, and nowhere else: the caller
%% hands over the literal verbatim. Both binary_to_float and the out-of-range
%% classifier see the same normalized text — the classifier's "does this look
%% like a float?" question is only meaningful about the string binary_to_float
%% actually rejected. (When they disagreed, ".5" and "1e400" classified as
%% `invalid` rather than parsing / overflowing.)
parse_float(S) ->
    Norm = normalize(S),
    case try_binary_to_float(Norm) of
        {ok, F} -> {ok, F};
        %% binary_to_float raised badarg. If the text is nonetheless
        %% well-formed float syntax, the only remaining cause is a magnitude
        %% outside the double range — a valid JS literal (e.g. "1e400") the
        %% caller must not zero out.
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

%% Pad a JS decimal literal into the shape binary_to_float accepts:
%% [+-]?Digits "." Digits ([eE][+-]?Digits)?. JS lets the mantissa omit the
%% integer part (".5"), the fraction ("1.", "1.e3") or the dot itself
%% ("1e10"); Erlang requires a dot with a digit on each side. Anything else is
%% left alone for is_float_syntax/1 to reject.
normalize(S) ->
    {Mantissa, Exponent} = split_exponent(S),
    {Sign, Digits} = take_sign(Mantissa),
    <<Sign/binary, (pad_mantissa(Digits))/binary, Exponent/binary>>.

%% Split off the exponent at the first e/E, keeping the marker with it
%% ("1.e3" -> {<<"1.">>, <<"e3">>}); the exponent is <<>> when absent.
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

%% [+-]?Digits "." Digits ([eE][+-]?Digits)? — the shape binary_to_float
%% accepts, so a badarg on a matching input can only be a range error.
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

%% Consume leading decimal digits: {SawAtLeastOneDigit, Rest}.
take_digits(S) -> take_digits(S, false).
take_digits(<<D, Rest/binary>>, _) when D >= $0, D =< $9 ->
    take_digits(Rest, true);
take_digits(S, Seen) ->
    {Seen, S}.
