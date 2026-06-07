-module(arc_parser_ffi).
-export([parse_float/1, decode_string_escapes/1, cook_template_string/1]).

-define(IS_HEX1(C),
    ((C >= $0 andalso C =< $9) orelse
     (C >= $a andalso C =< $f) orelse
     (C >= $A andalso C =< $F))).
-define(IS_HEX4(A, B, C, D),
    (?IS_HEX1(A) andalso ?IS_HEX1(B) andalso ?IS_HEX1(C) andalso ?IS_HEX1(D))).

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
            CU = list_to_integer(Hex, 16),
            handle_unicode_escape(CU, Rest1, Acc);
        %% \uHHHH — 4 hex digits
        <<"u", H1, H2, H3, H4, T/binary>> ->
            CU = list_to_integer([H1, H2, H3, H4], 16),
            handle_unicode_escape(CU, T, Acc);
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

%% Handle the code unit from a \u escape. JS string literals are UTF-16, so a
%% high surrogate (D800..DBFF) immediately followed by a low-surrogate escape
%% (DC00..DFFF) forms a single astral codepoint. Lone surrogates can't be
%% encoded as UTF-8, so encode_codepoint maps them to U+FFFD (matching the rest
%% of the runtime — JSON.parse, String.fromCharCode).
handle_unicode_escape(CU, Rest, Acc) when CU >= 16#D800, CU =< 16#DBFF ->
    case read_low_surrogate_escape(Rest) of
        {ok, Low, Rest1} ->
            CP = 16#10000 + (CU - 16#D800) * 16#400 + (Low - 16#DC00),
            decode_escapes_loop(Rest1, [encode_codepoint(CP) | Acc]);
        error ->
            decode_escapes_loop(Rest, [encode_codepoint(CU) | Acc])
    end;
handle_unicode_escape(CU, Rest, Acc) ->
    decode_escapes_loop(Rest, [encode_codepoint(CU) | Acc]).

%% Peek for a \uHHHH or \u{...} escape that is a low surrogate (DC00..DFFF).
%% Returns {ok, Low, Rest} consuming it, or `error` without consuming so the
%% high surrogate is emitted on its own. The lexer has already validated escape
%% syntax, so the hex digits are well-formed.
read_low_surrogate_escape(<<"\\u{", T/binary>>) ->
    {Hex, Rest} = read_until_brace(T, []),
    classify_low_surrogate(list_to_integer(Hex, 16), Rest);
read_low_surrogate_escape(<<"\\u", H1, H2, H3, H4, T/binary>>) ->
    classify_low_surrogate(list_to_integer([H1, H2, H3, H4], 16), T);
read_low_surrogate_escape(_) ->
    error.

classify_low_surrogate(Low, Rest) when Low >= 16#DC00, Low =< 16#DFFF ->
    {ok, Low, Rest};
classify_low_surrogate(_, _) ->
    error.

%% Compute a template quasi's Template Value (TV) per ES2024 §12.9.6.
%% Input: the RAW quasi text (line endings already normalized to LF).
%% Output: {ok, Cooked} or {error, nil} when the quasi contains an escape
%% sequence that is invalid in templates (\1-\7, \8, \9, \0<digit>, bad \x,
%% bad \u). Tagged templates map the error to an undefined cooked entry;
%% untagged templates turn it into a SyntaxError.
cook_template_string(S) when is_binary(S) ->
    try
        {ok, iolist_to_binary(cook_loop(S, []))}
    catch
        throw:invalid_template_escape -> {error, nil}
    end.

cook_loop(<<>>, Acc) ->
    lists:reverse(Acc);
cook_loop(<<"\\", Rest/binary>>, Acc) ->
    case Rest of
        <<>> -> throw(invalid_template_escape);
        <<"b", T/binary>> -> cook_loop(T, [<<8>> | Acc]);
        <<"t", T/binary>> -> cook_loop(T, [<<9>> | Acc]);
        <<"n", T/binary>> -> cook_loop(T, [<<10>> | Acc]);
        <<"v", T/binary>> -> cook_loop(T, [<<11>> | Acc]);
        <<"f", T/binary>> -> cook_loop(T, [<<12>> | Acc]);
        <<"r", T/binary>> -> cook_loop(T, [<<13>> | Acc]);
        <<"\"", T/binary>> -> cook_loop(T, [<<34>> | Acc]);
        <<"'", T/binary>> -> cook_loop(T, [<<39>> | Acc]);
        <<"`", T/binary>> -> cook_loop(T, [<<"`">> | Acc]);
        <<"$", T/binary>> -> cook_loop(T, [<<"$">> | Acc]);
        <<"\\", T/binary>> -> cook_loop(T, [<<"\\">> | Acc]);
        %% Line continuation (raw text is LF-normalized, but accept CR forms)
        <<"\r\n", T/binary>> -> cook_loop(T, Acc);
        <<"\r", T/binary>> -> cook_loop(T, Acc);
        <<"\n", T/binary>> -> cook_loop(T, Acc);
        %% U+2028 (LS) and U+2029 (PS) line continuations
        <<16#E2, 16#80, 16#A8, T/binary>> -> cook_loop(T, Acc);
        <<16#E2, 16#80, 16#A9, T/binary>> -> cook_loop(T, Acc);
        %% \xHH — exactly 2 hex digits required
        <<"x", H1, H2, T/binary>> ->
            case is_hex(H1) andalso is_hex(H2) of
                true ->
                    CP = list_to_integer([H1, H2], 16),
                    cook_loop(T, [encode_codepoint(CP) | Acc]);
                false -> throw(invalid_template_escape)
            end;
        <<"x", _/binary>> -> throw(invalid_template_escape);
        %% \u{...} — 1+ hex digits, value =< 0x10FFFF, closing brace required
        <<"u{", T/binary>> ->
            case cook_read_braced_hex(T, []) of
                {ok, CU, Rest1} -> cook_unicode_escape(CU, Rest1, Acc);
                error -> throw(invalid_template_escape)
            end;
        %% \uHHHH — exactly 4 hex digits required
        <<"u", H1, H2, H3, H4, T/binary>> when ?IS_HEX4(H1, H2, H3, H4) ->
            CU = list_to_integer([H1, H2, H3, H4], 16),
            cook_unicode_escape(CU, T, Acc);
        <<"u", _/binary>> -> throw(invalid_template_escape);
        %% \0 not followed by a decimal digit → NUL; otherwise invalid
        <<"0", T/binary>> ->
            case T of
                <<D, _/binary>> when D >= $0, D =< $9 ->
                    throw(invalid_template_escape);
                _ -> cook_loop(T, [<<0>> | Acc])
            end;
        %% Octal and non-octal decimal escapes are never legal in templates
        <<D, _/binary>> when D >= $1, D =< $9 ->
            throw(invalid_template_escape);
        %% NonEscapeCharacter: \g → "g" etc.
        <<C/utf8, T/binary>> ->
            cook_loop(T, [<<C/utf8>> | Acc]);
        <<B, T/binary>> ->
            cook_loop(T, [<<B>> | Acc])
    end;
cook_loop(<<C/utf8, Rest/binary>>, Acc) ->
    cook_loop(Rest, [<<C/utf8>> | Acc]);
cook_loop(<<B, Rest/binary>>, Acc) ->
    cook_loop(Rest, [<<B>> | Acc]).

%% Read hex digits up to a closing brace for \u{...}. Requires at least one
%% hex digit, only hex digits before the brace, and value =< 0x10FFFF.
cook_read_braced_hex(<<"}", _/binary>>, []) ->
    error;
cook_read_braced_hex(<<"}", Rest/binary>>, Acc) ->
    CU = list_to_integer(lists:reverse(Acc), 16),
    case CU =< 16#10FFFF of
        true -> {ok, CU, Rest};
        false -> error
    end;
cook_read_braced_hex(<<C, Rest/binary>>, Acc) ->
    case is_hex(C) of
        true -> cook_read_braced_hex(Rest, [C | Acc]);
        false -> error
    end;
cook_read_braced_hex(<<>>, _Acc) ->
    error.

%% Surrogate pairing for template \u escapes, mirroring handle_unicode_escape
%% but feeding cook_loop (and validating the low-surrogate escape's syntax).
cook_unicode_escape(CU, Rest, Acc) when CU >= 16#D800, CU =< 16#DBFF ->
    case cook_read_low_surrogate(Rest) of
        {ok, Low, Rest1} ->
            CP = 16#10000 + (CU - 16#D800) * 16#400 + (Low - 16#DC00),
            cook_loop(Rest1, [encode_codepoint(CP) | Acc]);
        error ->
            cook_loop(Rest, [encode_codepoint(CU) | Acc])
    end;
cook_unicode_escape(CU, Rest, Acc) ->
    cook_loop(Rest, [encode_codepoint(CU) | Acc]).

cook_read_low_surrogate(<<"\\u{", T/binary>>) ->
    case cook_read_braced_hex(T, []) of
        {ok, CU, Rest} -> classify_low_surrogate(CU, Rest);
        error -> error
    end;
cook_read_low_surrogate(<<"\\u", H1, H2, H3, H4, T/binary>>)
    when ?IS_HEX4(H1, H2, H3, H4) ->
    classify_low_surrogate(list_to_integer([H1, H2, H3, H4], 16), T);
cook_read_low_surrogate(_) ->
    error.

is_hex(C) when C >= $0, C =< $9 -> true;
is_hex(C) when C >= $a, C =< $f -> true;
is_hex(C) when C >= $A, C =< $F -> true;
is_hex(_) -> false.

encode_codepoint(CP) when CP =< 16#7F ->
    <<CP>>;
%% Lone surrogates have no UTF-8 encoding (<<CP/utf8>> raises badarg) → U+FFFD.
encode_codepoint(CP) when CP >= 16#D800, CP =< 16#DFFF ->
    <<16#EF, 16#BF, 16#BD>>;
encode_codepoint(CP) when CP =< 16#10FFFF ->
    <<CP/utf8>>;
encode_codepoint(_) ->
    <<16#EF, 16#BF, 16#BD>>.   %% U+FFFD replacement
