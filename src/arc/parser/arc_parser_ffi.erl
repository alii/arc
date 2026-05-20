-module(arc_parser_ffi).
-export([parse_float/1, decode_string_escapes/1]).

parse_float(S) ->
    try
        {ok, erlang:binary_to_float(S)}
    catch
        error:badarg ->
            %% Erlang's binary_to_float requires a decimal point. For inputs
            %% like "1e10" or "1E20" we insert ".0" before the exponent so the
            %% string becomes "1.0e10" / "1.0E20" which Erlang accepts.
            case insert_dot_before_exp(S) of
                {ok, S2} ->
                    try
                        {ok, erlang:binary_to_float(S2)}
                    catch
                        error:badarg -> {error, nil}
                    end;
                error -> {error, nil}
            end
    end.

%% Insert ".0" before the first 'e' or 'E' in a binary, if no '.' precedes it.
%% Returns {ok, NewBinary} or `error` if no exponent or already has decimal.
insert_dot_before_exp(S) ->
    case binary:match(S, [<<"e">>, <<"E">>]) of
        {Pos, _Len} ->
            <<Mantissa:Pos/binary, Rest/binary>> = S,
            case binary:match(Mantissa, <<".">>) of
                nomatch -> {ok, <<Mantissa/binary, ".0", Rest/binary>>};
                _ -> error
            end;
        nomatch -> error
    end.

%% Decode a JavaScript string literal's escape sequences per ES2024 §12.9.4.
%% Input: the raw content between quotes (e.g., "a\\nb").
%% Output: the cooked string with escapes processed (e.g., "a\nb").
%% Lexer has already validated escape syntax; this is purely a transform.
decode_string_escapes(S) when is_binary(S) ->
    %% Fast path: no backslashes means no escapes, return as-is.
    case binary:match(S, <<"\\">>) of
        nomatch -> S;
        _ -> iolist_to_binary(decode_escapes_loop(S, []))
    end.

decode_escapes_loop(<<>>, Acc) ->
    lists:reverse(Acc);
decode_escapes_loop(<<"\\", Rest/binary>>, Acc) ->
    case Rest of
        <<>> -> lists:reverse(Acc);
        <<"b", T/binary>> -> decode_escapes_loop(T, [<<8>> | Acc]);
        <<"t", T/binary>> -> decode_escapes_loop(T, [<<9>> | Acc]);
        <<"n", T/binary>> -> decode_escapes_loop(T, [<<10>> | Acc]);
        <<"v", T/binary>> -> decode_escapes_loop(T, [<<11>> | Acc]);
        <<"f", T/binary>> -> decode_escapes_loop(T, [<<12>> | Acc]);
        <<"r", T/binary>> -> decode_escapes_loop(T, [<<13>> | Acc]);
        <<"\"", T/binary>> -> decode_escapes_loop(T, [<<34>> | Acc]);
        <<"'", T/binary>> -> decode_escapes_loop(T, [<<39>> | Acc]);
        <<"\\", T/binary>> -> decode_escapes_loop(T, [<<"\\">> | Acc]);
        %% Line continuations: \<CR><LF>, \<CR>, \<LF> → empty
        <<"\r\n", T/binary>> -> decode_escapes_loop(T, Acc);
        <<"\r", T/binary>> -> decode_escapes_loop(T, Acc);
        <<"\n", T/binary>> -> decode_escapes_loop(T, Acc);
        %% Unicode line terminators U+2028 (LS) and U+2029 (PS)
        <<16#E2, 16#80, 16#A8, T/binary>> -> decode_escapes_loop(T, Acc);
        <<16#E2, 16#80, 16#A9, T/binary>> -> decode_escapes_loop(T, Acc);
        %% \xHH — 2 hex digits
        <<"x", H1, H2, T/binary>> ->
            CP = list_to_integer([H1, H2], 16),
            decode_escapes_loop(T, [encode_codepoint(CP) | Acc]);
        %% \u{...} — variable hex digits
        <<"u{", T/binary>> ->
            {Hex, Rest1} = read_until_brace(T, []),
            CP = list_to_integer(Hex, 16),
            decode_escapes_loop(Rest1, [encode_codepoint(CP) | Acc]);
        %% \uHHHH — 4 hex digits
        <<"u", H1, H2, H3, H4, T/binary>> ->
            CP = list_to_integer([H1, H2, H3, H4], 16),
            decode_escapes_loop(T, [encode_codepoint(CP) | Acc]);
        %% Legacy octal escapes \0..\7 (sloppy mode; lexer validated this)
        %% \0 not followed by digit → null character
        <<"0", T/binary>> ->
            case T of
                <<D, _/binary>> when D >= $0, D =< $9 ->
                    %% Multi-digit octal escape
                    decode_octal(<<"0", T/binary>>, Acc);
                _ ->
                    decode_escapes_loop(T, [<<0>> | Acc])
            end;
        <<D, _/binary>> when D >= $1, D =< $7 ->
            decode_octal(Rest, Acc);
        %% Any other char after backslash: just the char
        <<C/utf8, T/binary>> ->
            decode_escapes_loop(T, [<<C/utf8>> | Acc])
    end;
decode_escapes_loop(<<C/utf8, Rest/binary>>, Acc) ->
    decode_escapes_loop(Rest, [<<C/utf8>> | Acc]);
decode_escapes_loop(<<B, Rest/binary>>, Acc) ->
    %% Fallback for any non-UTF8 byte
    decode_escapes_loop(Rest, [<<B>> | Acc]).

%% Read up to 6 octal digits for a legacy octal escape.
decode_octal(<<D1, Rest/binary>>, Acc) when D1 >= $0, D1 =< $7 ->
    case Rest of
        <<D2, T/binary>> when D2 >= $0, D2 =< $7 ->
            case T of
                <<D3, T2/binary>> when D3 >= $0, D3 =< $7, D1 =< $3 ->
                    %% 3 octal digits, valid only if first is 0-3
                    CP = list_to_integer([D1, D2, D3], 8),
                    decode_escapes_loop(T2, [<<CP>> | Acc]);
                _ ->
                    CP = list_to_integer([D1, D2], 8),
                    decode_escapes_loop(T, [<<CP>> | Acc])
            end;
        _ ->
            CP = list_to_integer([D1], 8),
            decode_escapes_loop(Rest, [<<CP>> | Acc])
    end.

read_until_brace(<<"}", Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
read_until_brace(<<C, Rest/binary>>, Acc) ->
    read_until_brace(Rest, [C | Acc]);
read_until_brace(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>}.

encode_codepoint(CP) when CP =< 16#7F ->
    <<CP>>;
encode_codepoint(CP) when CP =< 16#10FFFF ->
    <<CP/utf8>>;
encode_codepoint(_) ->
    <<16#EF, 16#BF, 16#BD>>.   %% U+FFFD replacement
