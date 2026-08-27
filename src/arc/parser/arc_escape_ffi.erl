-module(arc_escape_ffi).
-export([decode_string_escapes/1, cook_template_string/1]).

-define(IS_HEX1(C),
    ((C >= $0 andalso C =< $9) orelse
     (C >= $a andalso C =< $f) orelse
     (C >= $A andalso C =< $F))).
-define(IS_HEX4(A, B, C, D),
    (?IS_HEX1(A) andalso ?IS_HEX1(B) andalso ?IS_HEX1(C) andalso ?IS_HEX1(D))).

%% lexer already validated escapes, so an error here is a bug
decode_string_escapes(S) when is_binary(S) ->
    case decode_escapes(S, string) of
        {ok, Cooked} -> Cooked;
        {error, invalid_escape} -> erlang:error({invalid_string_escape, S})
    end.

%% §12.9.6 template value; {error, nil} on a notescapesequence
cook_template_string(S) when is_binary(S) ->
    case decode_escapes(S, template) of
        {ok, Cooked} -> {ok, Cooked};
        {error, invalid_escape} -> {error, nil}
    end.

decode_escapes(S, Mode) ->
    case binary:match(S, <<"\\">>) of
        nomatch -> {ok, S};
        _ ->
            try
                {ok, iolist_to_binary(escape_loop(S, Mode, []))}
            catch
                throw:invalid_escape -> {error, invalid_escape}
            end
    end.

escape_loop(<<>>, _Mode, Acc) ->
    lists:reverse(Acc);
escape_loop(<<"\\", Rest/binary>>, Mode, Acc) ->
    case Rest of
        <<>> -> throw(invalid_escape);
        <<"b", T/binary>> -> escape_loop(T, Mode, [<<8>> | Acc]);
        <<"t", T/binary>> -> escape_loop(T, Mode, [<<9>> | Acc]);
        <<"n", T/binary>> -> escape_loop(T, Mode, [<<10>> | Acc]);
        <<"v", T/binary>> -> escape_loop(T, Mode, [<<11>> | Acc]);
        <<"f", T/binary>> -> escape_loop(T, Mode, [<<12>> | Acc]);
        <<"r", T/binary>> -> escape_loop(T, Mode, [<<13>> | Acc]);
        <<"\"", T/binary>> -> escape_loop(T, Mode, [<<34>> | Acc]);
        <<"'", T/binary>> -> escape_loop(T, Mode, [<<39>> | Acc]);
        <<"`", T/binary>> -> escape_loop(T, Mode, [<<"`">> | Acc]);
        <<"$", T/binary>> -> escape_loop(T, Mode, [<<"$">> | Acc]);
        <<"\\", T/binary>> -> escape_loop(T, Mode, [<<"\\">> | Acc]);
        <<"\r\n", T/binary>> -> escape_loop(T, Mode, Acc);
        <<"\r", T/binary>> -> escape_loop(T, Mode, Acc);
        <<"\n", T/binary>> -> escape_loop(T, Mode, Acc);
        %% u+2028 / u+2029 line continuations
        <<16#E2, 16#80, 16#A8, T/binary>> -> escape_loop(T, Mode, Acc);
        <<16#E2, 16#80, 16#A9, T/binary>> -> escape_loop(T, Mode, Acc);
        <<"x", H1, H2, T/binary>> when ?IS_HEX1(H1), ?IS_HEX1(H2) ->
            CP = list_to_integer([H1, H2], 16),
            escape_loop(T, Mode, [encode_codepoint(CP) | Acc]);
        <<"x", _/binary>> -> throw(invalid_escape);
        <<"u{", T/binary>> ->
            case read_braced_hex(T) of
                {ok, CU, Rest1} -> unicode_escape(CU, Rest1, Mode, Acc);
                error -> throw(invalid_escape)
            end;
        <<"u", H1, H2, H3, H4, T/binary>> when ?IS_HEX4(H1, H2, H3, H4) ->
            CU = list_to_integer([H1, H2, H3, H4], 16),
            unicode_escape(CU, T, Mode, Acc);
        <<"u", _/binary>> -> throw(invalid_escape);
        <<"0", T/binary>> ->
            case T of
                <<D, _/binary>> when D >= $0, D =< $9 ->
                    decode_octal(<<"0", T/binary>>, Mode, Acc);
                _ ->
                    escape_loop(T, Mode, [<<0>> | Acc])
            end;
        <<D, _/binary>> when D >= $1, D =< $7 ->
            decode_octal(Rest, Mode, Acc);
        <<D, T/binary>> when D =:= $8; D =:= $9 ->
            ok = require_legacy_octal(Mode),
            escape_loop(T, Mode, [<<D>> | Acc]);
        <<C/utf8, T/binary>> ->
            escape_loop(T, Mode, [<<C/utf8>> | Acc]);
        <<B, T/binary>> ->
            escape_loop(T, Mode, [<<B>> | Acc])
    end;
escape_loop(<<C/utf8, Rest/binary>>, Mode, Acc) ->
    escape_loop(Rest, Mode, [<<C/utf8>> | Acc]);
escape_loop(<<B, Rest/binary>>, Mode, Acc) ->
    escape_loop(Rest, Mode, [<<B>> | Acc]).

%% annex b: legacy octal and \8 \9 only in string literals
require_legacy_octal(string) -> ok;
require_legacy_octal(template) -> throw(invalid_escape).

decode_octal(<<D1, Rest/binary>>, Mode, Acc) when D1 >= $0, D1 =< $7 ->
    ok = require_legacy_octal(Mode),
    case Rest of
        <<D2, T/binary>> when D2 >= $0, D2 =< $7 ->
            case T of
                <<D3, T2/binary>> when D3 >= $0, D3 =< $7, D1 =< $3 ->
                    CP = list_to_integer([D1, D2, D3], 8),
                    escape_loop(T2, Mode, [encode_codepoint(CP) | Acc]);
                _ ->
                    CP = list_to_integer([D1, D2], 8),
                    escape_loop(T, Mode, [encode_codepoint(CP) | Acc])
            end;
        _ ->
            CP = list_to_integer([D1], 8),
            escape_loop(Rest, Mode, [encode_codepoint(CP) | Acc])
    end.

read_braced_hex(Bin) -> read_braced_hex(Bin, []).

read_braced_hex(<<"}", _/binary>>, []) ->
    error;
read_braced_hex(<<"}", Rest/binary>>, Acc) ->
    CU = list_to_integer(lists:reverse(Acc), 16),
    case CU =< 16#10FFFF of
        true -> {ok, CU, Rest};
        false -> error
    end;
read_braced_hex(<<C, Rest/binary>>, Acc) when ?IS_HEX1(C) ->
    read_braced_hex(Rest, [C | Acc]);
read_braced_hex(_, _Acc) ->
    error.

unicode_escape(CU, Rest, Mode, Acc) when CU >= 16#D800, CU =< 16#DBFF ->
    case read_low_surrogate_escape(Rest) of
        {ok, Low, Rest1} ->
            CP = 16#10000 + (CU - 16#D800) * 16#400 + (Low - 16#DC00),
            escape_loop(Rest1, Mode, [encode_codepoint(CP) | Acc]);
        error ->
            escape_loop(Rest, Mode, [encode_codepoint(CU) | Acc])
    end;
unicode_escape(CU, Rest, Mode, Acc) ->
    escape_loop(Rest, Mode, [encode_codepoint(CU) | Acc]).

read_low_surrogate_escape(<<"\\u{", T/binary>>) ->
    case read_braced_hex(T) of
        {ok, CU, Rest} -> classify_low_surrogate(CU, Rest);
        error -> error
    end;
read_low_surrogate_escape(<<"\\u", H1, H2, H3, H4, T/binary>>)
    when ?IS_HEX4(H1, H2, H3, H4) ->
    classify_low_surrogate(list_to_integer([H1, H2, H3, H4], 16), T);
read_low_surrogate_escape(_) ->
    error.

classify_low_surrogate(Low, Rest) when Low >= 16#DC00, Low =< 16#DFFF ->
    {ok, Low, Rest};
classify_low_surrogate(_, _) ->
    error.

encode_codepoint(CP) when CP =< 16#7F ->
    <<CP>>;
%% lone surrogate has no utf-8 encoding, becomes u+fffd
encode_codepoint(CP) when CP >= 16#D800, CP =< 16#DFFF ->
    <<16#EF, 16#BF, 16#BD>>;
encode_codepoint(CP) when CP =< 16#10FFFF ->
    <<CP/utf8>>;
encode_codepoint(_) ->
    <<16#EF, 16#BF, 16#BD>>.
