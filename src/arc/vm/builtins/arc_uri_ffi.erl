-module(arc_uri_ffi).
-export([encode/2, decode/2]).

%% JavaScript encodeURI / encodeURIComponent implementation.
%% When PreserveUriChars is true, behaves like encodeURI (preserves ;/?:@&=+$,#).
%% When false, behaves like encodeURIComponent (encodes everything except unreserved).

encode(Str, PreserveUriChars) when is_binary(Str) ->
    encode_binary(Str, PreserveUriChars, <<>>).

encode_binary(<<>>, _Preserve, Acc) ->
    Acc;
encode_binary(<<C, Rest/binary>>, Preserve, Acc) when C < 16#80 ->
    case is_unreserved(C) orelse (Preserve andalso is_reserved(C)) of
        true -> encode_binary(Rest, Preserve, <<Acc/binary, C>>);
        false ->
            encode_binary(Rest, Preserve, <<Acc/binary, (percent_encode_byte(C))/binary>>)
    end;
encode_binary(<<_/utf8, Rest/binary>> = Bin, Preserve, Acc) ->
    %% Multi-byte UTF-8: slice the codepoint's bytes straight from the source
    %% and percent-encode each. A non-UTF-8 byte matches no clause and
    %% crashes with function_clause — see arc_string_ffi's INVALID UTF-8
    %% POLICY: a JS string is always well-formed UTF-8, so a bad byte here is
    %% a boundary bug and must not be silently encoded.
    CpBytes = binary:part(Bin, 0, byte_size(Bin) - byte_size(Rest)),
    Encoded = percent_encode_bytes(CpBytes, <<>>),
    encode_binary(Rest, Preserve, <<Acc/binary, Encoded/binary>>).

percent_encode_bytes(<<>>, Acc) ->
    Acc;
percent_encode_bytes(<<B, Rest/binary>>, Acc) ->
    Hex = percent_encode_byte(B),
    percent_encode_bytes(Rest, <<Acc/binary, Hex/binary>>).

percent_encode_byte(B) ->
    Hi = hex_digit(B bsr 4),
    Lo = hex_digit(B band 16#0F),
    <<$%, Hi, Lo>>.

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $A + N - 10.

%% JavaScript decodeURI / decodeURIComponent — ES2024 19.2.6.2 Decode.
%%
%% Returns {ok, Binary} where Binary is guaranteed to be well-formed UTF-8,
%% or {error, {malformed, ByteOffset}} for anything the spec rejects with a
%% URIError: a truncated `%`, non-hex escape digits, a first octet that is
%% not a legal UTF-8 leading byte, missing/out-of-range continuation
%% escapes, or an escape run whose octets are not the UTF-8 encoding of a
%% Unicode scalar value (overlong forms, surrogates, > U+10FFFF).
%%
%% Every percent-escape run is re-validated with unicode:characters_to_binary
%% before it is appended, so a byte can never reach the accumulator unless
%% it is part of well-formed UTF-8 — the result is always safe to hand to
%% Gleam as a String.
%%
%% When PreserveReserved is true (decodeURI), an escape that decodes to a
%% single-byte member of uriReserved-plus-'#' (`;/?:@&=+$,#`) is kept as its
%% original three-character `%XY` text (19.2.6.2 step 4.c.iv), so e.g.
%% decodeURI("%26") is "%26", not "&".
decode(Str, PreserveReserved) when is_binary(Str) ->
    decode_binary(Str, PreserveReserved, 0, <<>>).

decode_binary(<<>>, _Preserve, _K, Acc) ->
    {ok, Acc};
decode_binary(<<$%, Rest/binary>> = Input, Preserve, K, Acc) ->
    case hex_pair(Rest) of
        error ->
            {error, {malformed, K}};
        {ok, B, Rest1} when B < 16#80 ->
            case Preserve andalso is_reserved(B) of
                true ->
                    %% Keep the original `%XY` text (original hex case).
                    <<Escape:3/binary, _/binary>> = Input,
                    decode_binary(Rest1, Preserve, K + 3, <<Acc/binary, Escape/binary>>);
                false ->
                    decode_binary(Rest1, Preserve, K + 3, <<Acc/binary, B>>)
            end;
        {ok, B, Rest1} ->
            case continuation_count(B) of
                error ->
                    {error, {malformed, K}};
                N ->
                    case take_continuations(Rest1, N, <<B>>) of
                        error ->
                            {error, {malformed, K}};
                        {ok, Octets, Rest2} ->
                            %% Reject overlong forms, surrogates and
                            %% out-of-range codepoints: only well-formed
                            %% UTF-8 may enter the accumulator.
                            case unicode:characters_to_binary(Octets) of
                                Valid when is_binary(Valid) ->
                                    decode_binary(
                                        Rest2,
                                        Preserve,
                                        K + 3 * (N + 1),
                                        <<Acc/binary, Valid/binary>>
                                    );
                                _ ->
                                    {error, {malformed, K}}
                            end
                    end
            end
    end;
decode_binary(<<C, Rest/binary>>, Preserve, K, Acc) ->
    decode_binary(Rest, Preserve, K + 1, <<Acc/binary, C>>).

%% Parse the two hex digits of a `%XY` escape. `error` on truncation or a
%% non-hex digit — never a partial recovery.
hex_pair(<<H1, H2, Rest/binary>>) ->
    case {hex_val(H1), hex_val(H2)} of
        {error, _} -> error;
        {_, error} -> error;
        {V1, V2} -> {ok, V1 * 16 + V2, Rest}
    end;
hex_pair(_) ->
    error.

%% How many continuation octets a UTF-8 leading byte demands.
%% Continuation bytes (10xxxxxx) and 11111xxx are never legal leaders.
continuation_count(B) when B >= 16#C0, B =< 16#DF -> 1;
continuation_count(B) when B >= 16#E0, B =< 16#EF -> 2;
continuation_count(B) when B >= 16#F0, B =< 16#F7 -> 3;
continuation_count(_) -> error.

%% Read N further `%XY` escapes, each of which must decode to a UTF-8
%% continuation octet (0x80-0xBF). A raw (unescaped) byte, a short string,
%% or an out-of-range octet is an error — the spec requires every octet of
%% a multi-byte sequence to come from its own escape.
take_continuations(Rest, 0, Acc) ->
    {ok, Acc, Rest};
take_continuations(<<$%, Rest/binary>>, N, Acc) ->
    case hex_pair(Rest) of
        {ok, B, Rest1} when B >= 16#80, B =< 16#BF ->
            take_continuations(Rest1, N - 1, <<Acc/binary, B>>);
        _ ->
            error
    end;
take_continuations(_, _, _) ->
    error.

%% uriUnescaped (§19.2.6.1): the characters both encodeURI and
%% encodeURIComponent leave untouched.
is_unreserved(C) when C >= $A, C =< $Z -> true;
is_unreserved(C) when C >= $a, C =< $z -> true;
is_unreserved(C) when C >= $0, C =< $9 -> true;
is_unreserved($-) -> true;
is_unreserved($_) -> true;
is_unreserved($.) -> true;
is_unreserved($!) -> true;
is_unreserved($~) -> true;
is_unreserved($*) -> true;
is_unreserved($') -> true;
is_unreserved($() -> true;
is_unreserved($)) -> true;
is_unreserved(_) -> false.

%% uriReserved plus '#': the set encodeURI leaves untouched and decodeURI
%% leaves as its literal `%XY` escape. One definition — Encode and Decode
%% cannot drift.
is_reserved($;) -> true;
is_reserved($/) -> true;
is_reserved($?) -> true;
is_reserved($:) -> true;
is_reserved($@) -> true;
is_reserved($&) -> true;
is_reserved($=) -> true;
is_reserved($+) -> true;
is_reserved($$) -> true;
is_reserved($,) -> true;
is_reserved($#) -> true;
is_reserved(_) -> false.

hex_val(C) when C >= $0, C =< $9 -> C - $0;
hex_val(C) when C >= $A, C =< $F -> C - $A + 10;
hex_val(C) when C >= $a, C =< $f -> C - $a + 10;
hex_val(_) -> error.
