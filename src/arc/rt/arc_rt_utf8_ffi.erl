%% code-unit ops on a WTF-8 binary; index work is delegated to
%% arc_rt_js_string_ffi so both files share one UTF-16 model
-module(arc_rt_utf8_ffi).
-compile({no_auto_import, [length/1]}).
-export([char_at/2, length/1,
         char_at_offset/2, replacement_codepoint/0]).
-export([index_of/3, last_index_of/3, contains/2,
         has_byte/2, last_index_of_all/2]).
-export([slice/3, drop_start/2, explode/1]).
-export([split/3, repeat/2, replace_literal/4]).
-export([ascii_upper/1, ascii_lower/1, case_map/2, has_surrogate/1]).
-export([to_graphemes/1, first_unit/1]).
-export([trim_js_ws/1, trim_leading_js_ws/1, trim_trailing_js_ws/1]).

char_at(Bin, Idx) -> arc_rt_js_string_ffi:raw_char_at(Bin, Idx).

length(Bin) -> arc_rt_js_string_ffi:unit_length(Bin).

char_at_offset(Bin, Off) -> arc_rt_js_string_ffi:raw_char_at_offset(Bin, Off).

replacement_codepoint() -> 16#FFFD.

index_of(Hay, Needle, From) ->
    arc_rt_js_string_ffi:raw_index_of(Hay, Needle, From).

last_index_of_all(Hay, Needle) ->
    arc_rt_js_string_ffi:raw_last_index_of_all(Hay, Needle).

last_index_of(Hay, Needle, From) ->
    arc_rt_js_string_ffi:raw_last_index_of(Hay, Needle, From).

slice(Bin, Start, Len) -> arc_rt_js_string_ffi:raw_slice(Bin, Start, Len).

drop_start(Bin, N) -> arc_rt_js_string_ffi:raw_drop(Bin, N).

explode(Bin) -> arc_rt_js_string_ffi:raw_explode(Bin).

has_byte(<<C, _/binary>>, C) -> true;
has_byte(<<_, R/binary>>, C) -> has_byte(R, C);
has_byte(<<>>, _) -> false.

contains(_Hay, <<>>) -> true;
contains(Hay, Needle) -> binary:match(Hay, Needle) =/= nomatch.

split(Hay, Sep, Lim) ->
    Parts = binary:split(Hay, Sep, [global]),
    case erlang:length(Parts) > Lim of
        true -> lists:sublist(Parts, Lim);
        false -> Parts
    end.

%% search is non-empty
replace_literal(Hay, Search, Repl, true) ->
    binary:replace(Hay, Search, Repl, [global]);
replace_literal(Hay, Search, Repl, false) ->
    binary:replace(Hay, Search, Repl, []).

repeat(Bin, N) when N > 1024, byte_size(Bin) < 1024 ->
    Block = binary:copy(Bin, 1024),
    Whole = binary:copy(Block, N div 1024),
    Tail = binary:copy(Bin, N rem 1024),
    <<Whole/binary, Tail/binary>>;
repeat(Bin, N) when N > 0 -> binary:copy(Bin, N);
repeat(_, _) -> <<>>.

ascii_upper(Bin) ->
    ascii_map(Bin, 16#1F1F1F1F1F1F1F, 16#05050505050505, <<>>).
ascii_lower(Bin) ->
    ascii_map(Bin, 16#3F3F3F3F3F3F3F, 16#25252525252525, <<>>).

%% case mapping that leaves lone surrogates alone
case_map(Bin, Upper) -> iolist_to_binary(cm(Bin, Upper, [])).

cm(<<>>, Upper, Acc) -> [flush_run(Acc, Upper)];
cm(<<16#ED, B, C, R/binary>>, Upper, Acc) when B >= 16#A0, B =< 16#BF ->
    [flush_run(Acc, Upper), <<16#ED, B, C>> | cm(R, Upper, [])];
cm(<<H, R/binary>>, Upper, Acc) -> cm(R, Upper, [H | Acc]).

flush_run([], _Upper) -> [];
flush_run(Acc, true) -> string:uppercase(list_to_binary(lists:reverse(Acc)));
flush_run(Acc, false) -> string:lowercase(list_to_binary(lists:reverse(Acc))).

has_surrogate(Bin) -> hs(Bin).

hs(<<16#ED, B, _C, _/binary>>) when B >= 16#A0, B =< 16#BF -> true;
hs(<<_, R/binary>>) -> hs(R);
hs(<<>>) -> false.

%% lone surrogates each become their own grapheme
to_graphemes(Bin) -> lists:reverse(tg(Bin, <<>>, [])).

tg(<<>>, Run, Acc) ->
    lists:reverse(grapheme_bins(Run)) ++ Acc;
tg(<<16#ED, B, C, R/binary>>, Run, Acc) when B >= 16#A0, B =< 16#BF ->
    tg(R, <<>>, [<<16#ED, B, C>> | lists:reverse(grapheme_bins(Run)) ++ Acc]);
tg(<<H, R/binary>>, Run, Acc) -> tg(R, <<Run/binary, H>>, Acc).

grapheme_bins(Run) ->
    [grapheme_bin(G) || G <- string:to_graphemes(Run)].

%% a lone codepoint comes as an int, a cluster as a codepoint list
grapheme_bin(G) when is_integer(G) -> <<G/utf8>>;
grapheme_bin(G) -> unicode:characters_to_binary(G).

first_unit(Bin) ->
    case arc_rt_js_string_ffi:raw_unit_at(Bin, 0) of
        none -> none;
        U -> {some, U}
    end.

ascii_map(<<W:56, Rest/binary>>, Lo, Hi, Acc) when W band 16#80808080808080 =:= 0 ->
    M = ((W + Lo) band (bnot (W + Hi))) band 16#80808080808080,
    ascii_map(Rest, Lo, Hi, <<Acc/binary, (W bxor (M bsr 2)):56>>);
ascii_map(<<C, Rest/binary>>, Lo, Hi, Acc) when C < 16#80 ->
    M = ((C + (Lo band 16#FF)) band (bnot (C + (Hi band 16#FF)))) band 16#80,
    ascii_map(Rest, Lo, Hi, <<Acc/binary, (C bxor (M bsr 2))>>);
ascii_map(<<>>, _Lo, _Hi, Acc) -> {some, Acc};
ascii_map(_Bin, _Lo, _Hi, _Acc) -> none.

trim_js_ws(Bin) -> trim_trailing_js_ws(trim_leading_js_ws(Bin)).

trim_leading_js_ws(<<C, R/binary>>)
    when C =:= 16#09; C =:= 16#0A; C =:= 16#0B; C =:= 16#0C; C =:= 16#0D;
         C =:= 16#20 ->
    trim_leading_js_ws(R);
trim_leading_js_ws(<<16#C2, 16#A0, R/binary>>) -> trim_leading_js_ws(R);
trim_leading_js_ws(<<16#E1, 16#9A, 16#80, R/binary>>) -> trim_leading_js_ws(R);
trim_leading_js_ws(<<16#E2, 16#80, C, R/binary>>)
    when C >= 16#80, C =< 16#8A; C =:= 16#A8; C =:= 16#A9; C =:= 16#AF ->
    trim_leading_js_ws(R);
trim_leading_js_ws(<<16#E2, 16#81, 16#9F, R/binary>>) -> trim_leading_js_ws(R);
trim_leading_js_ws(<<16#E3, 16#80, 16#80, R/binary>>) -> trim_leading_js_ws(R);
trim_leading_js_ws(<<16#EF, 16#BB, 16#BF, R/binary>>) -> trim_leading_js_ws(R);
trim_leading_js_ws(Bin) -> Bin.

trim_trailing_js_ws(Bin) ->
    Size = byte_size(Bin),
    case trail(Bin, Size) of
        Size -> Bin;
        Keep -> binary:part(Bin, 0, Keep)
    end.

trail(_Bin, 0) -> 0;
trail(Bin, N) ->
    case binary:at(Bin, N - 1) of
        C when C =:= 16#20; C >= 16#09, C =< 16#0D -> trail(Bin, N - 1);
        C when C >= 16#80, N >= 2 ->
            case ws_tail(Bin, N, C) of
                0 -> N;
                L -> trail(Bin, N - L)
            end;
        _ -> N
    end.

%% byte length of a multi-byte js whitespace char ending at n, or 0
ws_tail(Bin, N, 16#A0) ->
    case binary:at(Bin, N - 2) of 16#C2 -> 2; _ -> 0 end;
ws_tail(Bin, N, C) when N >= 3 ->
    case {binary:at(Bin, N - 3), binary:at(Bin, N - 2), C} of
        {16#E1, 16#9A, 16#80} -> 3;
        {16#E2, 16#80, X} when X >= 16#80, X =< 16#8A; X =:= 16#A8; X =:= 16#A9;
                               X =:= 16#AF -> 3;
        {16#E2, 16#81, 16#9F} -> 3;
        {16#E3, 16#80, 16#80} -> 3;
        {16#EF, 16#BB, 16#BF} -> 3;
        _ -> 0
    end;
ws_tail(_, _, _) -> 0.
