%% ascii strings are bare binaries, others {js_str, Utf8, CpLen, Crumbs}
%% crumbs: byte offsets of codepoints 0, K, 2K.., or none past MAXCRUMB
%% TODO(Deviation): indexes by codepoint, js wants utf-16 code units
-module(arc_rt_str_ffi).
-export([mk/1, mk_list/1, bin/1, len/1, is_str/1, cp_at/2, char_at/2,
         sub/3, concat/2, concat_loose/2, index_of/3]).

-include("arc_rt_layout.hrl").

-define(K, 32).
-define(MAXCRUMB, 8192).
-define(HI7, 16#80808080808080).

mk(Bin) when is_binary(Bin) ->
    case ascii(Bin) of
        true -> Bin;
        false -> tag(Bin)
    end.

mk_list(L) -> [mk(B) || B <- L].

tag(Bin) ->
    {Len, Crumbs} = walk(Bin, 0, 0, []),
    {?STR_TAG, Bin, Len, Crumbs}.

ascii(<<W1:56, W2:56, W3:56, W4:56, R/binary>>)
    when (W1 bor W2 bor W3 bor W4) band ?HI7 =:= 0 ->
    ascii(R);
ascii(<<W:56, R/binary>>) when W band ?HI7 =:= 0 -> ascii(R);
ascii(<<W:24, R/binary>>) when W band 16#808080 =:= 0 -> ascii(R);
ascii(<<C, R/binary>>) when C < 16#80 -> ascii(R);
ascii(<<>>) -> true;
ascii(_) -> false.

%% records a crumb every K codepoints, j*K =< Len
walk(Bin, Off, N, Acc) ->
    Acc1 = [Off | Acc],
    case take(Bin, ?K, Off) of
        {?K, Off1, Rest} -> walk(Rest, Off1, N + ?K, Acc1);
        {Got, _, _} -> {N + Got, list_to_tuple(lists:reverse(Acc1))}
    end.

%% consume up to N codepoints: {consumed, byte offset after, rest}
take(Bin, N, Off) -> take(Bin, N, Off, 0).
take(<<W1:56, W2:56, W3:56, W4:56, R/binary>>, N, Off, Got)
    when N - Got >= 28, (W1 bor W2 bor W3 bor W4) band ?HI7 =:= 0 ->
    take(R, N, Off + 28, Got + 28);
take(<<W:56, R/binary>>, N, Off, Got) when N - Got >= 7, W band ?HI7 =:= 0 ->
    take(R, N, Off + 7, Got + 7);
take(Bin, N, Off, Got) when Got >= N -> {Got, Off, Bin};
take(<<C, R/binary>>, N, Off, Got) when C < 16#80 ->
    take(R, N, Off + 1, Got + 1);
take(<<C, _, R/binary>>, N, Off, Got) when C >= 16#C0, C < 16#E0 ->
    take(R, N, Off + 2, Got + 1);
take(<<C, _, _, R/binary>>, N, Off, Got) when C >= 16#E0, C < 16#F0 ->
    take(R, N, Off + 3, Got + 1);
take(<<C, _, _, _, R/binary>>, N, Off, Got) when C >= 16#F0 ->
    take(R, N, Off + 4, Got + 1);
take(<<>>, _, Off, Got) -> {Got, Off, <<>>};
take(Bin, _, _, _) -> erlang:error({invalid_utf8, Bin}).

bin(B) when is_binary(B) -> B;
bin({?STR_TAG, B, _, _}) -> B.

len(B) when is_binary(B) -> byte_size(B);
len({?STR_TAG, _, L, _}) -> L.

is_str(B) when is_binary(B) -> true;
is_str({?STR_TAG, _, _, _}) -> true;
is_str(_) -> false.

%% byte offset of codepoint I, 0 =< I =< len
off(B, I) when is_binary(B) -> I;
off({?STR_TAG, B, _, none}, I) ->
    {_, Off, _} = take(B, I, 0),
    Off;
off({?STR_TAG, B, _, Cr}, I) ->
    J = I div ?K,
    Base = element(J + 1, Cr),
    <<_:Base/binary, Rest/binary>> = B,
    {_, Off, _} = take(Rest, I - J * ?K, Base),
    Off.

%% codepoint index of byte offset Off, Off on a boundary
cp_index(B, Off) when is_binary(B) -> Off;
cp_index({?STR_TAG, B, L, none}, Off) ->
    L - count(binary:part(B, Off, byte_size(B) - Off));
cp_index({?STR_TAG, B, _, Cr}, Off) ->
    J = seek(Cr, Off, 1, tuple_size(Cr)),
    Base = element(J, Cr),
    (J - 1) * ?K + count(binary:part(B, Base, Off - Base)).

%% last crumb at or before Off
seek(_, _, Lo, Hi) when Lo >= Hi -> Lo;
seek(Cr, Off, Lo, Hi) ->
    Mid = (Lo + Hi + 1) div 2,
    case element(Mid, Cr) =< Off of
        true -> seek(Cr, Off, Mid, Hi);
        false -> seek(Cr, Off, Lo, Mid - 1)
    end.

count(Bin) ->
    {N, _, _} = take(Bin, byte_size(Bin), 0),
    N.

cp_at(S, I) when I >= 0 ->
    case I < len(S) of
        true ->
            Off = off(S, I),
            <<_:Off/binary, C/utf8, _/binary>> = bin(S),
            {some, C};
        false -> none
    end;
cp_at(_, _) -> none.

char_at(S, I) ->
    case cp_at(S, I) of
        {some, C} when C < 16#80 -> {some, <<C>>};
        {some, C} -> {some, {?STR_TAG, <<C/utf8>>, 1, {0}}};
        none -> none
    end.

%% caller clamps: 0 =< Start, 0 =< N, Start + N =< len
sub(_, _, N) when N =< 0 -> <<>>;
sub(B, Start, N) when is_binary(B) -> binary:part(B, Start, N);
sub(S, Start, N) ->
    B = bin(S),
    O1 = off(S, Start),
    O2 = off(S, Start + N),
    Part = binary:part(B, O1, O2 - O1),
    case O2 - O1 =:= N of
        true -> Part;
        false -> tag(Part)
    end.

concat(A, B) when is_binary(A), is_binary(B) -> <<A/binary, B/binary>>;
concat(A, B) ->
    BA = bin(A),
    LA = len(A),
    Len = LA + len(B),
    New = <<BA/binary, (bin(B))/binary>>,
    Crumbs = case Len > ?MAXCRUMB of
        true -> none;
        false -> extend(A, LA, New)
    end,
    {?STR_TAG, New, Len, Crumbs}.

%% keep A's crumbs, rescan from its last one through the appended tail
extend(A, LA, New) ->
    J = LA div ?K,
    {Kept, Base} = case A of
        _ when is_binary(A) ->
            {[I * ?K || I <- lists:seq(0, J - 1)], J * ?K};
        {?STR_TAG, _, _, Cr} ->
            {lists:sublist(tuple_to_list(Cr), J), element(J + 1, Cr)}
    end,
    <<_:Base/binary, Rest/binary>> = New,
    {_, More} = walk(Rest, Base, J * ?K, []),
    list_to_tuple(Kept ++ tuple_to_list(More)).

%% codepoint index of Needle at or after cp From, both js strings
index_of(Hay, Needle, From) ->
    HB = bin(Hay),
    NB = bin(Needle),
    Start = off(Hay, From),
    case NB of
        <<>> -> {some, From};
        _ ->
            case binary:match(HB, NB, [{scope, {Start, byte_size(HB) - Start}}]) of
                nomatch -> none;
                {Pos, _} -> {some, cp_index(Hay, Pos)}
            end
    end.

%% either side may be a plain utf8 binary that was never checked
concat_loose(A, B) -> concat(loose(A), loose(B)).

loose(B) when is_binary(B) -> mk(B);
loose(S) -> S.
