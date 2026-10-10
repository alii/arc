-module(spark_buffer_ffi).
-export([encode/3, decode/2, compare/2, index_of/5, swap/2, atob/1,
         is_ascii/1, is_latin1/1, utf16_units/1]).

%% a string as bytes in one of node's encodings, at most Max of them and
%% never a split character (utf8) or a split unit (utf16le)
encode(Text, Encoding, Max) ->
    fit(Encoding, bytes(Encoding, Text), Max).

fit(_, Bin, Max) when byte_size(Bin) =< Max -> Bin;
fit(utf8, Bin, Max) -> binary:part(Bin, 0, utf8_boundary(Bin, Max));
fit(utf16le, Bin, Max) -> binary:part(Bin, 0, Max band (bnot 1));
fit(_, Bin, Max) -> binary:part(Bin, 0, Max).

%% the last character start at or before At
utf8_boundary(_, 0) -> 0;
utf8_boundary(Bin, At) ->
    case binary:at(Bin, At) band 16#C0 of
        16#80 -> utf8_boundary(Bin, At - 1);
        _ -> At
    end.

bytes(utf8, Text) -> Text;
bytes(utf16le, Text) -> unicode:characters_to_binary(Text, utf8, {utf16, little});
bytes(hex, Text) -> unhex(Text, <<>>);
bytes(base64, Text) -> unbase64(Text, 0, 0, <<>>);
bytes(base64url, Text) -> unbase64(Text, 0, 0, <<>>);
%% latin1 and ascii keep the low byte of each utf-16 unit
bytes(_, Text) ->
    case unicode:characters_to_binary(Text, utf8, latin1) of
        Bin when is_binary(Bin) -> Bin;
        _ -> << <<Unit>> || <<Unit:8, _:8>> <= bytes(utf16le, Text) >>
    end.

%% whole pairs up to the first one that is not hex
unhex(<<A, B, Rest/binary>>, Acc) ->
    case {nibble(A), nibble(B)} of
        {X, Y} when X >= 0, Y >= 0 -> unhex(Rest, <<Acc/binary, (X * 16 + Y)>>);
        _ -> Acc
    end;
unhex(_, Acc) -> Acc.

nibble(C) when C >= $0, C =< $9 -> C - $0;
nibble(C) when C >= $a, C =< $f -> C - $a + 10;
nibble(C) when C >= $A, C =< $F -> C - $A + 10;
nibble(_) -> -1.

%% node's forgiving decode: either alphabet, anything else skipped, stops at
%% the first '='. Bits holds Count pending sextets
unbase64(<<$=, _/binary>>, Bits, Count, Acc) -> flush(Bits, Count, Acc);
unbase64(<<C, Rest/binary>>, Bits, Count, Acc) ->
    case sextet(C) of
        -1 -> unbase64(Rest, Bits, Count, Acc);
        V when Count =:= 3 ->
            unbase64(Rest, 0, 0, <<Acc/binary, ((Bits bsl 6) bor V):24>>);
        V -> unbase64(Rest, (Bits bsl 6) bor V, Count + 1, Acc)
    end;
unbase64(<<>>, Bits, Count, Acc) -> flush(Bits, Count, Acc).

flush(Bits, 2, Acc) -> <<Acc/binary, (Bits bsr 4):8>>;
flush(Bits, 3, Acc) -> <<Acc/binary, (Bits bsr 2):16>>;
flush(_, _, Acc) -> Acc.

sextet(C) when C >= $A, C =< $Z -> C - $A;
sextet(C) when C >= $a, C =< $z -> C - $a + 26;
sextet(C) when C >= $0, C =< $9 -> C - $0 + 52;
sextet($+) -> 62;
sextet($-) -> 62;
sextet($/) -> 63;
sextet($_) -> 63;
sextet(_) -> -1.

%% bytes as a string in one of node's encodings
decode(Bin, utf8) ->
    {ok, {Text, 0}} = spark_encoding_ffi:decode_utf8(Bin, false, false),
    Text;
decode(Bin, latin1) -> unicode:characters_to_binary(Bin, latin1, utf8);
decode(Bin, ascii) ->
    case is_ascii(Bin) of
        true -> Bin;
        false -> << <<(B band 16#7F)>> || <<B>> <= Bin >>
    end;
decode(Bin, utf16le) -> utf16(Bin, []);
decode(Bin, hex) -> binary:encode_hex(Bin, lowercase);
decode(Bin, base64) -> base64:encode(Bin);
decode(Bin, base64url) -> base64:encode(Bin, #{mode => urlsafe, padding => false}).

%% a lone surrogate cannot be held in a string here, so it reads as U+FFFD;
%% a last odd byte is dropped
utf16(<<High:16/little, Low:16/little, Rest/binary>>, Acc)
  when High >= 16#D800, High =< 16#DBFF, Low >= 16#DC00, Low =< 16#DFFF ->
    Point = 16#10000 + ((High - 16#D800) bsl 10) + (Low - 16#DC00),
    utf16(Rest, [<<Point/utf8>> | Acc]);
utf16(<<Unit:16/little, Rest/binary>>, Acc) when Unit >= 16#D800, Unit =< 16#DFFF ->
    utf16(Rest, [<<16#FFFD/utf8>> | Acc]);
utf16(<<Unit:16/little, Rest/binary>>, Acc) ->
    utf16(Rest, [<<Unit/utf8>> | Acc]);
utf16(_, Acc) -> iolist_to_binary(lists:reverse(Acc)).

compare(A, B) when A < B -> -1;
compare(A, B) when A > B -> 1;
compare(_, _) -> 0.

%% node's indexOf and lastIndexOf over bytes. Step is 2 for utf-16 text,
%% where a match has to sit on a unit edge
index_of(Hay, Needle, Offset, Forward, Step) ->
    Length = byte_size(Hay),
    Size = byte_size(Needle),
    Start = start_offset(Length, Offset, Size, Forward),
    if
        Size =:= 0 -> Start;
        Length =:= 0; Start < 0 -> -1;
        Forward andalso Size + Start > Length -> -1;
        Size > Length -> -1;
        Step =:= 2 andalso (Length < 2 orelse Size < 2) -> -1;
        Forward -> forward(Hay, Needle, Start - (Start rem Step), Length, Step);
        true -> backward(Hay, Needle, Start, Size, Step)
    end.

start_offset(Length, Offset, Size, Forward) when Offset < 0 ->
    if
        Offset + Length >= 0 -> Length + Offset;
        Forward; Size =:= 0 -> 0;
        true -> -1
    end;
start_offset(Length, Offset, Size, Forward) ->
    if
        Offset + Size =< Length -> Offset;
        Size =:= 0 -> Length;
        Forward -> -1;
        true -> Length - 1
    end.

forward(Hay, Needle, From, Length, Step) when From < Length ->
    case binary:match(Hay, Needle, [{scope, {From, Length - From}}]) of
        nomatch -> -1;
        {At, _} when At rem Step =:= 0 -> At;
        {At, _} -> forward(Hay, Needle, At + 1, Length, Step)
    end;
forward(_, _, _, _, _) -> -1.

%% the last match that starts at or before From
backward(Hay, Needle, From, Size, Step) ->
    Last = min(From, byte_size(Hay) - Size),
    down(Hay, Needle, Last - (Last rem Step), Size, Step).

down(_, _, At, _, _) when At < 0 -> -1;
down(Hay, Needle, At, Size, Step) ->
    case binary:part(Hay, At, Size) of
        Needle -> At;
        _ -> down(Hay, Needle, At - Step, Size, Step)
    end.

%% each Bits-wide unit with its bytes reversed
swap(Bin, Bits) -> << <<Unit:Bits/little>> || <<Unit:Bits/big>> <= Bin >>.

%% whatwg forgiving-base64 decode to a string of code points 0..255
atob(Text) ->
    Clean = << <<C>> || <<C>> <= Text, C =/= $\t, C =/= $\n, C =/= $\f, C =/= $\r, C =/= $\s >>,
    Data = case byte_size(Clean) rem 4 of
        0 -> unpad(Clean);
        _ -> Clean
    end,
    case strict(Data, 0, 0, <<>>) of
        invalid -> {error, invalid_character};
        _ when byte_size(Data) rem 4 =:= 1 -> {error, bad_length};
        Bin -> {ok, unicode:characters_to_binary(Bin, latin1, utf8)}
    end.

unpad(Bin) ->
    Size = byte_size(Bin),
    case Bin of
        <<Head:(Size - 2)/binary, "==">> when Size >= 2 -> Head;
        <<Head:(Size - 1)/binary, "=">> when Size >= 1 -> Head;
        _ -> Bin
    end.

strict(<<C, Rest/binary>>, Bits, Count, Acc) ->
    case standard_sextet(C) of
        -1 -> invalid;
        V when Count =:= 3 -> strict(Rest, 0, 0, <<Acc/binary, ((Bits bsl 6) bor V):24>>);
        V -> strict(Rest, (Bits bsl 6) bor V, Count + 1, Acc)
    end;
strict(<<>>, Bits, Count, Acc) -> flush(Bits, Count, Acc).

standard_sextet($-) -> -1;
standard_sextet($_) -> -1;
standard_sextet(C) -> sextet(C).

is_ascii(<<A, B, C, D, E, F, G, H, Rest/binary>>) ->
    (A bor B bor C bor D bor E bor F bor G bor H) < 128 andalso is_ascii(Rest);
is_ascii(<<A, Rest/binary>>) -> A < 128 andalso is_ascii(Rest);
is_ascii(<<>>) -> true.

is_latin1(Text) -> is_binary(unicode:characters_to_binary(Text, utf8, latin1)).

%% how many utf-16 units the bytes hold once read as utf-8
utf16_units(Bin) ->
    byte_size(unicode:characters_to_binary(decode(Bin, utf8), utf8, {utf16, little})) div 2.
