%% ascii strings are bare binaries, others {js_str, Utf8, CpLen, Crumbs}
%% crumbs: byte offsets of codepoints 0, stride, 2 stride .., none when long
%% TODO(Deviation): indexes by codepoint, js wants utf-16 code units
-module(arc_rt_js_string_ffi).
-export([from_text/1, from_texts/1, text/1, length/1, is_str/1,
         codepoint_at/2, char_at/2, substring/3, concat/2, concat_loose/2,
         index_of/3]).
-compile({no_auto_import, [length/1]}).

-include("arc_rt_layout.hrl").

-define(CRUMB_STRIDE, 32).
-define(MAX_CRUMBED_LEN, 8192).
-define(ASCII_HI_MASK, 16#80808080808080).

from_text(Bin) when is_binary(Bin) ->
    case ascii(Bin) of
        true -> Bin;
        false -> tag(Bin)
    end.

from_texts(L) -> [from_text(B) || B <- L].

tag(Bin) ->
    {Len, Crumbs} = build_crumbs(Bin, 0, 0, []),
    {?STR_TAG, Bin, Len, Crumbs}.

ascii(<<W1:56, W2:56, W3:56, W4:56, R/binary>>)
    when (W1 bor W2 bor W3 bor W4) band ?ASCII_HI_MASK =:= 0 ->
    ascii(R);
ascii(<<W:56, R/binary>>) when W band ?ASCII_HI_MASK =:= 0 -> ascii(R);
ascii(<<W:24, R/binary>>) when W band 16#808080 =:= 0 -> ascii(R);
ascii(<<C, R/binary>>) when C < 16#80 -> ascii(R);
ascii(<<>>) -> true;
ascii(_) -> false.

%% records a crumb every stride codepoints, j * stride =< len
build_crumbs(Bin, Off, N, Acc) ->
    Acc1 = [Off | Acc],
    case advance(Bin, ?CRUMB_STRIDE, Off) of
        {?CRUMB_STRIDE, Off1, Rest} ->
            build_crumbs(Rest, Off1, N + ?CRUMB_STRIDE, Acc1);
        {Got, _, _} -> {N + Got, list_to_tuple(lists:reverse(Acc1))}
    end.

%% consume up to N codepoints: {consumed, byte offset after, rest}
advance(Bin, N, Off) -> advance(Bin, N, Off, 0).
advance(<<W1:56, W2:56, W3:56, W4:56, R/binary>>, N, Off, Got)
    when N - Got >= 28, (W1 bor W2 bor W3 bor W4) band ?ASCII_HI_MASK =:= 0 ->
    advance(R, N, Off + 28, Got + 28);
advance(<<W:56, R/binary>>, N, Off, Got)
    when N - Got >= 7, W band ?ASCII_HI_MASK =:= 0 ->
    advance(R, N, Off + 7, Got + 7);
advance(Bin, N, Off, Got) when Got >= N -> {Got, Off, Bin};
advance(<<C, R/binary>>, N, Off, Got) when C < 16#80 ->
    advance(R, N, Off + 1, Got + 1);
advance(<<C, _, R/binary>>, N, Off, Got) when C >= 16#C0, C < 16#E0 ->
    advance(R, N, Off + 2, Got + 1);
advance(<<C, _, _, R/binary>>, N, Off, Got) when C >= 16#E0, C < 16#F0 ->
    advance(R, N, Off + 3, Got + 1);
advance(<<C, _, _, _, R/binary>>, N, Off, Got) when C >= 16#F0 ->
    advance(R, N, Off + 4, Got + 1);
advance(<<>>, _, Off, Got) -> {Got, Off, <<>>};
advance(Bin, _, _, _) -> erlang:error({invalid_utf8, Bin}).

text(B) when is_binary(B) -> B;
text({?STR_TAG, B, _, _}) -> B.

length(B) when is_binary(B) -> byte_size(B);
length({?STR_TAG, _, L, _}) -> L.

is_str(B) when is_binary(B) -> true;
is_str({?STR_TAG, _, _, _}) -> true;
is_str(_) -> false.

%% byte offset of codepoint I, 0 =< I =< len
byte_offset(B, I) when is_binary(B) -> I;
byte_offset({?STR_TAG, B, _, none}, I) ->
    {_, Off, _} = advance(B, I, 0),
    Off;
byte_offset({?STR_TAG, B, _, Cr}, I) ->
    J = I div ?CRUMB_STRIDE,
    Base = element(J + 1, Cr),
    <<_:Base/binary, Rest/binary>> = B,
    {_, Off, _} = advance(Rest, I - J * ?CRUMB_STRIDE, Base),
    Off.

%% codepoint index of byte offset Off, Off on a boundary
codepoint_index(B, Off) when is_binary(B) -> Off;
codepoint_index({?STR_TAG, B, L, none}, Off) ->
    L - count(binary:part(B, Off, byte_size(B) - Off));
codepoint_index({?STR_TAG, B, _, Cr}, Off) ->
    J = seek(Cr, Off, 1, tuple_size(Cr)),
    Base = element(J, Cr),
    (J - 1) * ?CRUMB_STRIDE + count(binary:part(B, Base, Off - Base)).

%% last crumb at or before Off
seek(_, _, Lo, Hi) when Lo >= Hi -> Lo;
seek(Cr, Off, Lo, Hi) ->
    Mid = (Lo + Hi + 1) div 2,
    case element(Mid, Cr) =< Off of
        true -> seek(Cr, Off, Mid, Hi);
        false -> seek(Cr, Off, Lo, Mid - 1)
    end.

count(Bin) ->
    {N, _, _} = advance(Bin, byte_size(Bin), 0),
    N.

codepoint_at(S, I) when I >= 0 ->
    case I < length(S) of
        true ->
            Off = byte_offset(S, I),
            <<_:Off/binary, C/utf8, _/binary>> = text(S),
            {?SOME, C};
        false -> ?NONE
    end;
codepoint_at(_, _) -> ?NONE.

char_at(S, I) ->
    case codepoint_at(S, I) of
        {?SOME, C} when C < 16#80 -> {?SOME, <<C>>};
        {?SOME, C} -> {?SOME, {?STR_TAG, <<C/utf8>>, 1, {0}}};
        ?NONE -> ?NONE
    end.

%% caller clamps: 0 =< Start, 0 =< N, Start + N =< len
substring(_, _, N) when N =< 0 -> <<>>;
substring(B, Start, N) when is_binary(B) -> binary:part(B, Start, N);
substring(S, Start, N) ->
    B = text(S),
    O1 = byte_offset(S, Start),
    O2 = byte_offset(S, Start + N),
    Part = binary:part(B, O1, O2 - O1),
    case O2 - O1 =:= N of
        true -> Part;
        false -> tag(Part)
    end.

concat(A, B) when is_binary(A), is_binary(B) -> <<A/binary, B/binary>>;
concat(A, B) ->
    BA = text(A),
    LA = length(A),
    Len = LA + length(B),
    New = <<BA/binary, (text(B))/binary>>,
    Crumbs = case Len > ?MAX_CRUMBED_LEN of
        true -> none;
        false -> extend(A, LA, New)
    end,
    {?STR_TAG, New, Len, Crumbs}.

%% keep A's crumbs, rescan from its last one through the appended tail
extend(A, LA, New) ->
    J = LA div ?CRUMB_STRIDE,
    {Kept, Base} = case A of
        _ when is_binary(A) ->
            {[I * ?CRUMB_STRIDE || I <- lists:seq(0, J - 1)], J * ?CRUMB_STRIDE};
        {?STR_TAG, _, _, Cr} ->
            {lists:sublist(tuple_to_list(Cr), J), element(J + 1, Cr)}
    end,
    <<_:Base/binary, Rest/binary>> = New,
    {_, More} = build_crumbs(Rest, Base, J * ?CRUMB_STRIDE, []),
    list_to_tuple(Kept ++ tuple_to_list(More)).

%% codepoint index of Needle at or after cp From, both js strings
index_of(Hay, Needle, From) ->
    HB = text(Hay),
    NB = text(Needle),
    Start = byte_offset(Hay, From),
    case NB of
        <<>> -> {?SOME, From};
        _ ->
            case binary:match(HB, NB, [{scope, {Start, byte_size(HB) - Start}}]) of
                nomatch -> ?NONE;
                {Pos, _} -> {?SOME, codepoint_index(Hay, Pos)}
            end
    end.

%% either side may be a plain utf8 binary that was never checked
concat_loose(A, B) -> concat(loose(A), loose(B)).

loose(B) when is_binary(B) -> from_text(B);
loose(S) -> S.
