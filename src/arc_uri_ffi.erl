-module(arc_uri_ffi).
-export([encode/2, decode/1]).

%% JavaScript encodeURI / encodeURIComponent implementation.
%% When PreserveUriChars is true, behaves like encodeURI (preserves ;/?:@&=+$,#).
%% When false, behaves like encodeURIComponent (encodes everything except unreserved).

encode(Str, PreserveUriChars) when is_binary(Str) ->
    encode_binary(Str, PreserveUriChars, <<>>).

encode_binary(<<>>, _Preserve, Acc) ->
    Acc;
encode_binary(<<C, Rest/binary>>, Preserve, Acc) when
    (C >= $A andalso C =< $Z);
    (C >= $a andalso C =< $z);
    (C >= $0 andalso C =< $9);
    C =:= $-; C =:= $_; C =:= $.; C =:= $!; C =:= $~;
    C =:= $*; C =:= $'; C =:= $(; C =:= $) ->
    %% Unreserved chars — never encoded
    encode_binary(Rest, Preserve, <<Acc/binary, C>>);
encode_binary(<<C, Rest/binary>>, true, Acc) when
    C =:= $;; C =:= $/; C =:= $?; C =:= $:; C =:= $@;
    C =:= $&; C =:= $=; C =:= $+; C =:= $$; C =:= $,;
    C =:= $# ->
    %% URI reserved chars — only preserved by encodeURI
    encode_binary(Rest, true, <<Acc/binary, C>>);
encode_binary(Str, Preserve, Acc) ->
    %% Get next UTF-8 codepoint and percent-encode all its bytes
    case Str of
        <<C/utf8, Rest/binary>> ->
            Bytes = unicode:characters_to_binary([C], utf8),
            Encoded = percent_encode_bytes(Bytes, <<>>),
            encode_binary(Rest, Preserve, <<Acc/binary, Encoded/binary>>);
        <<B, Rest/binary>> ->
            %% Invalid UTF-8 byte — encode it raw
            Hex = percent_encode_byte(B),
            encode_binary(Rest, Preserve, <<Acc/binary, Hex/binary>>)
    end.

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

%% JavaScript decodeURI / decodeURIComponent implementation.
decode(Str) when is_binary(Str) ->
    decode_binary(Str, <<>>).

decode_binary(<<>>, Acc) ->
    Acc;
decode_binary(<<$%, H1, H2, Rest/binary>>, Acc) ->
    case {hex_val(H1), hex_val(H2)} of
        {error, _} -> decode_binary(<<H1, H2, Rest/binary>>, <<Acc/binary, $%>>);
        {_, error} -> decode_binary(<<H2, Rest/binary>>, <<Acc/binary, $%, H1>>);
        {V1, V2} ->
            Byte = V1 * 16 + V2,
            decode_binary(Rest, <<Acc/binary, Byte>>)
    end;
decode_binary(<<C, Rest/binary>>, Acc) ->
    decode_binary(Rest, <<Acc/binary, C>>).

hex_val(C) when C >= $0, C =< $9 -> C - $0;
hex_val(C) when C >= $A, C =< $F -> C - $A + 10;
hex_val(C) when C >= $a, C =< $f -> C - $a + 10;
hex_val(_) -> error.
