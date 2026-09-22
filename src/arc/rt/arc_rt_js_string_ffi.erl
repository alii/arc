%% ascii strings are bare binaries, others {js_str, Wtf8, UnitLen, Crumbs}
%% Wtf8 encodes lone surrogates as 3-byte sequences
%% crumbs: {ByteOffset, UnitIndex} per CRUMB_STRIDE units, none when long
-module(arc_rt_js_string_ffi).
-export([from_text/1, from_texts/1, from_units/1, text/1, length/1, is_str/1,
         codepoint_at/2, code_unit_at/2, char_at/2, substring/3, concat/2,
         concat_loose/2, index_of/3, compare/2]).
-export([is_ascii/1, unit_length/1, raw_unit_at/2, raw_codepoint_at/2,
         raw_char_at/2, raw_char_at_offset/2, raw_slice/3, raw_drop/2,
         raw_explode/1, raw_index_of/3, raw_last_index_of/3,
         raw_last_index_of_all/2, raw_compare/2, raw_byte_offset/2,
         raw_unit_index/2, raw_is_well_formed/1, raw_to_well_formed/1]).
-compile({no_auto_import, [length/1]}).

-include("arc_rt_layout.hrl").

-define(CRUMB_STRIDE, 32).
-define(MAX_CRUMBED_LEN, 8192).
-define(ASCII_HI_MASK, 16#80808080808080).
-define(HI_START, 16#D800).
-define(LO_START, 16#DC00).

%% decode the head char -> {CodePoint, Units, ByteLen}
decode(<<C, _R/binary>>) when C < 16#80 -> {C, 1, 1};
decode(<<C, C2, _/binary>>) when C >= 16#C0, C < 16#E0 ->
    {((C band 16#1F) bsl 6) bor (C2 band 16#3F), 1, 2};
decode(<<C, C2, C3, _/binary>>) when C >= 16#E0, C < 16#F0 ->
    {((C band 16#0F) bsl 12) bor ((C2 band 16#3F) bsl 6)
        bor (C3 band 16#3F),
     1, 3};
decode(<<C, C2, C3, C4, _/binary>>) when C >= 16#F0 ->
    {((C band 16#07) bsl 18) bor ((C2 band 16#3F) bsl 12)
        bor ((C3 band 16#3F) bsl 6) bor (C4 band 16#3F),
     2, 4}.

%% encode one UTF-16 code unit (surrogates take the WTF-8 3-byte form)
encode_unit(U) when U < 16#80 -> <<U>>;
encode_unit(U) when U < 16#800 ->
    <<(16#C0 bor (U bsr 6)), (16#80 bor (U band 16#3F))>>;
encode_unit(U) ->
    <<(16#E0 bor (U bsr 12)), (16#80 bor ((U bsr 6) band 16#3F)),
      (16#80 bor (U band 16#3F))>>.

encode_cp(Cp) when Cp =< 16#FFFF -> encode_unit(Cp);
encode_cp(Cp) ->
    <<(16#F0 bor (Cp bsr 18)), (16#80 bor ((Cp bsr 12) band 16#3F)),
      (16#80 bor ((Cp bsr 6) band 16#3F)), (16#80 bor (Cp band 16#3F))>>.

units_of(Cp, 2) ->
    N = Cp - 16#10000,
    [?HI_START + (N bsr 10), ?LO_START + (N band 16#3FF)];
units_of(Cp, 1) ->
    [Cp].

from_text(Bin) when is_binary(Bin) ->
    case is_ascii(Bin) of
        true -> Bin;
        false -> tag(Bin)
    end.

from_texts(L) -> [from_text(B) || B <- L].

%% pack well-formed pairs back into one astral sequence so fromCharCode
%% matches the encoding of source literals
from_units(Us) ->
    from_text(iolist_to_binary(pack_units(Us))).

pack_units([Hi, Lo | Rest])
    when Hi >= 16#D800, Hi =< 16#DBFF, Lo >= 16#DC00, Lo =< 16#DFFF ->
    [encode_cp(16#10000 + ((Hi - 16#D800) bsl 10) + (Lo - 16#DC00))
     | pack_units(Rest)];
pack_units([U | Rest]) -> [encode_unit(U) | pack_units(Rest)];
pack_units([]) -> [].

tag(Bin) ->
    {Len, Crumbs} = build_crumbs(Bin),
    {?STR_TAG, Bin, Len, Crumbs}.

tag_len(Raw) ->
    case is_ascii(Raw) of
        true -> Raw;
        false ->
            {Len, Crumbs} = build_crumbs(Raw),
            {?STR_TAG, Raw, Len, Crumbs}
    end.

text(B) when is_binary(B) -> B;
text({?STR_TAG, B, _, _}) -> B.

length(B) when is_binary(B) -> byte_size(B);
length({?STR_TAG, _, L, _}) -> L;
length(Other) -> erlang:error({bad_length, Other}).

is_str(B) when is_binary(B) -> true;
is_str({?STR_TAG, _, _, _}) -> true;
is_str(_) -> false.

is_ascii(<<W1:56, W2:56, W3:56, W4:56, R/binary>>)
    when (W1 bor W2 bor W3 bor W4) band ?ASCII_HI_MASK =:= 0 ->
    is_ascii(R);
is_ascii(<<W:56, R/binary>>) when W band ?ASCII_HI_MASK =:= 0 -> is_ascii(R);
is_ascii(<<W:24, R/binary>>) when W band 16#808080 =:= 0 -> is_ascii(R);
is_ascii(<<C, R/binary>>) when C < 16#80 -> is_ascii(R);
is_ascii(<<>>) -> true;
is_ascii(_) -> false.

build_crumbs(Bin) -> build(Bin, 0, 0, 0, []).

build(<<>>, Off, U, K, Acc) ->
    {U, list_to_tuple(lists:reverse(record_tail(U, K, Off, Acc)))};
build(Bin, Off, U, K, Acc) ->
    {_Cp, CU, BLen} = decode(Bin),
    case U + CU =< K * ?CRUMB_STRIDE of
        true ->
            <<_:BLen/binary, Rest/binary>> = Bin,
            build(Rest, Off + BLen, U + CU, K, Acc);
        false ->
            build(Bin, Off, U, K + 1, [{Off, U} | Acc])
    end.

record_tail(U, K, Off, Acc) ->
    case K * ?CRUMB_STRIDE =< U of
        true -> record_tail(U, K + 1, Off, [{Off, U} | Acc]);
        false -> Acc
    end.

%% keep A's crumbs, rescan only the appended tail
extend_crumbs(A, New) ->
    case A of
        {?STR_TAG, _, _LA, Cr} when Cr =/= none ->
            LastK = tuple_size(Cr),
            {BaseOff, BaseU} = element(LastK, Cr),
            <<_:BaseOff/binary, Tail/binary>> = New,
            {_, More} = build(Tail, BaseOff, BaseU, LastK, []),
            Kept = [element(I, Cr) || I <- lists:seq(1, LastK)],
            list_to_tuple(Kept ++ tuple_to_list(More));
        _ ->
            element(2, build_crumbs(New))
    end.

%% JsVal: bare binaries are ascii, tagged uses crumbs
locate(S, I, js) when I >= 0 ->
    case S of
        B when is_binary(B) ->
            case I < byte_size(B) of
                true -> {I, I, binary:at(B, I), 1, 1};
                false -> not_found
            end;
        {?STR_TAG, B, Len, none} ->
            case I < Len of
                true -> scan(B, 0, 0, I);
                false -> not_found
            end;
        {?STR_TAG, B, Len, Cr} ->
            case I < Len of
                true ->
                    K = I div ?CRUMB_STRIDE,
                    {Base, U0} = element(K + 1, Cr),
                    <<_:Base/binary, Rest/binary>> = B,
                    scan(Rest, Base, U0, I);
                false -> not_found
            end
    end;
%% raw binary from arc_rt_utf8_ffi, may be non-ascii
locate(Bin, I, raw) when is_binary(Bin), I >= 0 ->
    scan(Bin, 0, 0, I);
locate(_, _, _) ->
    not_found.

scan(<<>>, _Off, _U, _I) -> not_found;
scan(Bin, Off, U, I) ->
    {Cp, CU, BLen} = decode(Bin),
    case I < U + CU of
        true -> {Off, U, Cp, CU, BLen};
        false ->
            <<_:BLen/binary, Rest/binary>> = Bin,
            scan(Rest, Off + BLen, U + CU, I)
    end.

unit_at(S, I, Loc) ->
    case locate(S, I, Loc) of
        {_Off, _U, Cp, 1, _} -> Cp;
        {_Off, U, Cp, 2, _} when I =:= U ->
            ?HI_START + ((Cp - 16#10000) bsr 10);
        {_Off, _U, Cp, 2, _} ->
            ?LO_START + ((Cp - 16#10000) band 16#3FF);
        not_found -> none
    end.

unit_length(Bin) -> ul(Bin, 0).

ul(<<W1:56, W2:56, W3:56, W4:56, R/binary>>, N)
    when (W1 bor W2 bor W3 bor W4) band ?ASCII_HI_MASK =:= 0 ->
    ul(R, N + 28);
ul(<<W:56, R/binary>>, N) when W band ?ASCII_HI_MASK =:= 0 -> ul(R, N + 7);
ul(<<C, R/binary>>, N) when C < 16#80 -> ul(R, N + 1);
ul(<<>>, N) -> N;
ul(Bin, N) -> ul_mb(Bin, N).

ul_mb(<<C, _, _, _, R/binary>>, N) when C >= 16#F0 -> ul_mb(R, N + 2);
ul_mb(<<C, _, _, R/binary>>, N) when C >= 16#E0, C < 16#F0 -> ul_mb(R, N + 1);
ul_mb(<<C, _, R/binary>>, N) when C >= 16#C0, C < 16#E0 -> ul_mb(R, N + 1);
ul_mb(<<C, _/binary>> = Bin, N) when C < 16#80 -> ul(Bin, N);
ul_mb(<<>>, N) -> N.

code_unit_at(S, I) ->
    case unit_at(S, I, js) of
        none -> ?NONE;
        U -> {?SOME, U}
    end.

codepoint_at(S, I) ->
    raw_codepoint_at(text(S), I).

raw_unit_at(Bin, I) -> unit_at(Bin, I, raw).

raw_codepoint_at(Bin, I) when I >= 0 ->
    case unit_at(Bin, I, raw) of
        none -> ?NONE;
        U when U >= ?HI_START, U =< 16#DBFF ->
            case unit_at(Bin, I + 1, raw) of
                L when L >= ?LO_START, L =< 16#DFFF ->
                    {?SOME,
                     16#10000 + ((U - ?HI_START) bsl 10) + (L - ?LO_START)};
                _ -> {?SOME, U}
            end;
        U -> {?SOME, U}
    end;
raw_codepoint_at(_, _) -> ?NONE.

char_at(S, I) ->
    case unit_at(S, I, js) of
        none -> ?NONE;
        U -> {?SOME, from_text(encode_unit(U))}
    end.

raw_char_at(Bin, I) ->
    case unit_at(Bin, I, raw) of
        none -> none;
        U -> {some, encode_unit(U)}
    end.

%% whole code point at a byte offset, for the string iterator
raw_char_at_offset(Bin, Off) when Off >= 0, Off < byte_size(Bin) ->
    <<_:Off/binary, Rest/binary>> = Bin,
    {Cp, _CU, BLen} = decode(Rest),
    {some, {encode_cp(Cp), Off + BLen}};
raw_char_at_offset(_, _) -> none.

substring(_S, _Start, N) when N =< 0 -> <<>>;
substring(B, Start, N) when is_binary(B) -> binary:part(B, Start, N);
substring(S, Start, N) ->
    tag_len(do_substring(S, text(S), Start, N, js)).

raw_slice(_Bin, _Start, N) when N =< 0 -> <<>>;
raw_slice(Bin, Start, N) when Start >= 0 ->
    do_substring(Bin, Bin, Start, N, raw);
raw_slice(_, _, _) -> <<>>.

do_substring(S, Bin, Start, N, Loc) ->
    End = Start + N,
    {Off1, U1, _Cp1, _Cu1, Bl1} = locate(S, Start, Loc),
    MiddleStart = case Start > U1 of
        true -> Off1 + Bl1;
        false -> Off1
    end,
    {OffL, UL, _CpL, CuL, BlL} = locate(S, End - 1, Loc),
    EndInside = End - 1 < UL + CuL - 1,
    MiddleEnd = case EndInside of
        true -> OffL;
        false -> OffL + BlL
    end,
    Prefix = case Start > U1 of
        true -> encode_unit(unit_at(S, Start, Loc));
        false -> <<>>
    end,
    Suffix = case EndInside of
        true -> encode_unit(unit_at(S, End - 1, Loc));
        false -> <<>>
    end,
    Middle = case MiddleEnd > MiddleStart of
        true -> binary:part(Bin, MiddleStart, MiddleEnd - MiddleStart);
        false -> <<>>
    end,
    <<Prefix/binary, Middle/binary, Suffix/binary>>.

raw_drop(Bin, N) when N > 0 ->
    case locate(Bin, N, raw) of
        not_found -> <<>>;
        {Off, _, _, _, _} -> binary:part(Bin, Off, byte_size(Bin) - Off)
    end;
raw_drop(Bin, _) -> Bin.

raw_explode(Bin) -> expl(Bin, []).

expl(<<>>, Acc) -> lists:reverse(Acc);
expl(Bin, Acc) ->
    {Cp, CU, BLen} = decode(Bin),
    <<_:BLen/binary, Rest/binary>> = Bin,
    expl(Rest, lists:reverse([encode_unit(U) || U <- units_of(Cp, CU)]) ++ Acc).

concat(A, B) when is_binary(A), is_binary(B) -> <<A/binary, B/binary>>;
concat(A, B) ->
    New = <<(text(A))/binary, (text(B))/binary>>,
    Len = length(A) + length(B),
    Crumbs = case Len > ?MAX_CRUMBED_LEN of
        true -> none;
        false -> extend_crumbs(A, New)
    end,
    {?STR_TAG, New, Len, Crumbs}.

%% either side may be a plain WTF-8 binary that was never checked
concat_loose(A, B) -> concat(loose(A), loose(B)).

loose(B) when is_binary(B) -> from_text(B);
loose(S) -> S.

index_of(Hay, Needle, From) ->
    raw_index_of(text(Hay), text(Needle), From).

raw_index_of(Hay, Needle, From) ->
    HL = unit_length(Hay),
    F0 = max(From, 0),
    case Needle of
        <<>> -> {?SOME, min(F0, HL)};
        _ ->
            case F0 > HL of
                true -> ?NONE;
                false ->
                    case is_ascii(Hay) andalso is_ascii(Needle) of
                        true ->
                            case binary:match(Hay, Needle,
                                              [{scope, {F0, byte_size(Hay) - F0}}]) of
                                nomatch -> ?NONE;
                                {Pos, _} -> {?SOME, Pos}
                            end;
                        false ->
                            index_units(units_list(Hay), units_list(Needle), F0)
                    end
            end
    end.

raw_last_index_of_all(Hay, <<>>) -> {?SOME, unit_length(Hay)};
raw_last_index_of_all(Hay, Needle) ->
    last_from(units_list(Hay), units_list(Needle)).

raw_last_index_of(Hay, <<>>, From) ->
    {?SOME, min(max(From, 0), unit_length(Hay))};
raw_last_index_of(Hay, Needle, From) ->
    H = units_list(Hay),
    N = units_list(Needle),
    Start = min(max(From, 0), erlang:length(H) - erlang:length(N)),
    last_from(H, N, Start).

index_units(_H, _N, From) when From < 0 -> ?NONE;
index_units(H, N, From) ->
    case erlang:length(H) - From < erlang:length(N) of
        true -> ?NONE;
        false ->
            case lists:prefix(N, lists:nthtail(From, H)) of
                true -> {?SOME, From};
                false -> index_units(H, N, From + 1)
            end
    end.

last_from(H, N) -> last_from(H, N, erlang:length(H) - erlang:length(N)).

last_from(_H, _N, Start) when Start < 0 -> ?NONE;
last_from(H, N, Start) ->
    case lists:prefix(N, lists:nthtail(Start, H)) of
        true -> {?SOME, Start};
        false -> last_from(H, N, Start - 1)
    end.

units_list(Bin) -> lists:reverse(units_list(Bin, [])).

units_list(<<>>, Acc) -> Acc;
units_list(Bin, Acc) ->
    {Cp, CU, BLen} = decode(Bin),
    <<_:BLen/binary, Rest/binary>> = Bin,
    units_list(Rest, lists:reverse(units_of(Cp, CU)) ++ Acc).

compare(A, B) -> raw_compare(text(A), text(B)).
raw_compare(A, B) -> cmp(A, B).

cmp(<<>>, <<>>) -> eq;
cmp(<<>>, _) -> lt;
cmp(_, <<>>) -> gt;
cmp(A, B) ->
    {CA, UA, LA} = decode(A),
    {CB, UB, LB} = decode(B),
    cmp_units(units_of(CA, UA), units_of(CB, UB), A, B, LA, LB).

cmp_units([], [], A, B, LA, LB) ->
    <<_:LA/binary, RA/binary>> = A,
    <<_:LB/binary, RB/binary>> = B,
    cmp(RA, RB);
cmp_units([], [_ | _], _, _, _, _) -> lt;
cmp_units([_ | _], [], _, _, _, _) -> gt;
cmp_units([X | _], [Y | _], _, _, _, _) when X < Y -> lt;
cmp_units([X | _], [Y | _], _, _, _, _) when X > Y -> gt;
cmp_units([_ | Xs], [_ | Ys], A, B, LA, LB) ->
    cmp_units(Xs, Ys, A, B, LA, LB).

%% byte offset for a unit index, mid-pair lands inside its char
raw_byte_offset(Bin, I) ->
    case locate(Bin, I, raw) of
        not_found ->
            Len = unit_length(Bin),
            case I =< Len of
                true -> byte_size(Bin);
                false -> byte_size(Bin) + (I - Len)
            end;
        {Off, U, _Cp, CU, BLen} ->
            case I =:= U of
                true -> Off;
                false ->
                    case CU =:= 2 of
                        true -> Off + BLen - 1;
                        false -> Off
                    end
            end
    end.

%% unit index of a byte offset that is a char boundary
raw_unit_index(Bin, Off) -> count_units(Bin, Off, 0).

count_units(_Bin, Off, N) when Off =< 0 -> N;
count_units(Bin, Off, N) ->
    {_Cp, CU, BLen} = decode(Bin),
    case BLen =< Off of
        true ->
            <<_:BLen/binary, Rest/binary>> = Bin,
            count_units(Rest, Off - BLen, N + CU);
        false -> N
    end.

%% true when no unpaired surrogate; adjacent hi+lo units form a pair
raw_is_well_formed(Bin) -> is_wf(Bin).

is_wf(<<>>) -> true;
is_wf(Bin) ->
    {Cp, CU, BLen} = decode(Bin),
    <<_:BLen/binary, Rest/binary>> = Bin,
    case CU of
        2 -> is_wf(Rest);
        1 when Cp >= 16#D800, Cp =< 16#DBFF ->
            case next_low(Rest) of
                {some, _LoBin, Rest2} -> is_wf(Rest2);
                none -> false
            end;
        1 when Cp >= 16#DC00, Cp =< 16#DFFF -> false;
        _ -> is_wf(Rest)
    end.

%% replace every unpaired surrogate with U+FFFD
raw_to_well_formed(Bin) ->
    iolist_to_binary(to_wf(Bin, [])).

to_wf(<<>>, Acc) -> lists:reverse(Acc);
to_wf(Bin, Acc) ->
    {Cp, CU, BLen} = decode(Bin),
    <<Head:BLen/binary, Rest/binary>> = Bin,
    case CU of
        2 -> to_wf(Rest, [Head | Acc]);
        1 when Cp >= 16#D800, Cp =< 16#DBFF ->
            case next_low(Rest) of
                {some, LoHead, Rest2} -> to_wf(Rest2, [LoHead, Head | Acc]);
                none -> to_wf(Rest, [<<16#EF, 16#BF, 16#BD>> | Acc])
            end;
        1 when Cp >= 16#DC00, Cp =< 16#DFFF ->
            to_wf(Rest, [<<16#EF, 16#BF, 16#BD>> | Acc]);
        _ -> to_wf(Rest, [Head | Acc])
    end.

next_low(<<>>) -> none;
next_low(Bin) ->
    {Cp, CU, BLen} = decode(Bin),
    case CU =:= 1 andalso Cp >= 16#DC00 andalso Cp =< 16#DFFF of
        true ->
            <<Head:BLen/binary, Rest/binary>> = Bin,
            {some, Head, Rest};
        false -> none
    end.
