%% indexes by codepoint; invalid utf-8 crashes on purpose, no fallback clauses
%% TODO(Deviation): js indexes by utf-16 code unit
-module(arc_string_ffi).
-export([string_char_at/2, string_codepoint_at/2, string_codepoint_length/1,
         string_char_at_offset/2, replacement_codepoint/0]).
-export([string_index_of/3, string_last_index_of/3, string_contains/2,
         has_byte/2, string_last_index_of_all/2]).
-export([string_cp_slice/3, string_cp_drop/2, string_cp_explode/1]).
-export([slice_known/4, drop_known/3, index_of_known/4]).
-export([string_split/3, string_repeat/2, string_replace_literal/4]).
-export([string_ascii_upper/1, string_ascii_lower/1]).
-export([trim_js_ws/1, trim_leading_js_ws/1, trim_trailing_js_ws/1]).

%% match window bytes per backward step
-define(LAST_INDEX_CHUNK, 65536).

string_char_at(Bin, Idx) ->
    case string_codepoint_at(Bin, Idx) of
        {some, C} -> {some, <<C/utf8>>};
        none -> none
    end.

string_codepoint_at(Bin, Idx) when Idx >= 0 ->
    Off = cp_off(Bin, Idx, 0),
    case Bin of
        <<_:Off/binary, C/utf8, _/binary>> -> {some, C};
        _ -> none
    end;
string_codepoint_at(_, _) -> none.

string_char_at_offset(Bin, Off) when Off >= 0, Off < byte_size(Bin) ->
    <<_:Off/binary, C/utf8, _/binary>> = Bin,
    Ch = <<C/utf8>>,
    {some, {Ch, Off + byte_size(Ch)}};
string_char_at_offset(_, _) -> none.

replacement_codepoint() -> 16#FFFD.

string_codepoint_length(Bin) -> cp_length(Bin, 0).
%% 56 bits = 7 ascii bytes, still a small int
cp_length(<<W1:56, W2:56, W3:56, W4:56, W5:56, W6:56, W7:56, W8:56,
            Rest/binary>>, N)
    when (W1 bor W2 bor W3 bor W4 bor W5 bor W6 bor W7 bor W8)
         band 16#80808080808080 =:= 0 ->
    cp_length(Rest, N + 56);
cp_length(<<W1:56, W2:56, Rest/binary>>, N)
    when (W1 bor W2) band 16#80808080808080 =:= 0 ->
    cp_length(Rest, N + 14);
cp_length(<<C, Rest/binary>>, N) when C < 16#80 -> cp_length(Rest, N + 1);
cp_length(<<>>, N) -> N;
cp_length(Bin, N) -> cp_length_mb(Bin, N).

%% runs of non-ascii, by lead byte
cp_length_mb(<<C, _, _, Rest/binary>>, N) when C >= 16#E0, C < 16#F0 ->
    cp_length_mb(Rest, N + 1);
cp_length_mb(<<C, _, Rest/binary>>, N) when C >= 16#C0, C < 16#E0 ->
    cp_length_mb(Rest, N + 1);
cp_length_mb(<<C, _, _, _, Rest/binary>>, N) when C >= 16#F0 ->
    cp_length_mb(Rest, N + 1);
cp_length_mb(<<C, _/binary>> = Bin, N) when C < 16#80 -> cp_length(Bin, N);
cp_length_mb(<<>>, N) -> N;
cp_length_mb(Bin, _) -> erlang:error({invalid_utf8, Bin}).

has_byte(<<C, _/binary>>, C) -> true;
has_byte(<<_, R/binary>>, C) -> has_byte(R, C);
has_byte(<<>>, _) -> false.

string_contains(_Hay, <<>>) -> true;
string_contains(Hay, Needle) -> binary:match(Hay, Needle) =/= nomatch.

string_index_of(Hay, <<>>, From) ->
    {some, clamp_cp(Hay, From)};
string_index_of(Hay, Needle, From) ->
    Start = cp_byte_offset(Hay, max(From, 0)),
    case binary:match(Hay, Needle, [{scope, {Start, byte_size(Hay) - Start}}]) of
        nomatch -> none;
        {BytePos, _} -> {some, cp_length(binary:part(Hay, 0, BytePos), 0)}
    end.

string_last_index_of_all(Hay, <<>>) ->
    {some, string_codepoint_length(Hay)};
string_last_index_of_all(Hay, Needle) ->
    last_index_of(Hay, Needle, byte_size(Hay)).

string_last_index_of(Hay, <<>>, From) ->
    {some, clamp_cp(Hay, From)};
string_last_index_of(Hay, Needle, From) ->
    Limit = cp_byte_offset(Hay, max(From, 0)),
    last_index_of(Hay, Needle, min(Limit + byte_size(Needle), byte_size(Hay))).

last_index_of(_Hay, Needle, End) when End < byte_size(Needle) -> none;
last_index_of(Hay, Needle, End) ->
    HighestStart = End - byte_size(Needle),
    Chunk = max(?LAST_INDEX_CHUNK, 2 * byte_size(Needle)),
    case scan_back(Hay, Needle, max(0, HighestStart - Chunk + 1), HighestStart, Chunk) of
        none -> none;
        {some, BytePos} -> {some, cp_length(binary:part(Hay, 0, BytePos), 0)}
    end.

scan_back(Hay, Needle, Lo, Hi, Chunk) ->
    M = byte_size(Needle),
    case binary:matches(Hay, Needle, [{scope, {Lo, Hi + M - Lo}}]) of
        [] when Lo =:= 0 -> none;
        [] -> scan_back(Hay, Needle, max(0, Lo - Chunk), Lo - 1, Chunk);
        Matches ->
            {L, _} = lists:last(Matches),
            {some, latest_overlap(Hay, Needle, L, min(L + M - 1, Hi))}
    end.

latest_overlap(_Hay, _Needle, L, Pos) when Pos =< L -> L;
latest_overlap(Hay, Needle, L, Pos) ->
    case binary:part(Hay, Pos, byte_size(Needle)) of
        Needle -> Pos;
        _Other -> latest_overlap(Hay, Needle, L, Pos - 1)
    end.

clamp_cp(Hay, From) -> min(max(From, 0), string_codepoint_length(Hay)).

%% cp length already computed; ascii when it equals byte size
slice_known(Bin, CpLen, Start, Len) when CpLen =:= byte_size(Bin) ->
    case Start >= 0 andalso Len > 0 andalso Start < CpLen of
        true -> binary:part(Bin, Start, min(Len, CpLen - Start));
        false -> <<>>
    end;
slice_known(Bin, _CpLen, Start, Len) ->
    string_cp_slice(Bin, Start, Len).

drop_known(Bin, CpLen, N) when CpLen =:= byte_size(Bin) ->
    case N > 0 of
        true when N >= CpLen -> <<>>;
        true -> binary:part(Bin, N, CpLen - N);
        false -> Bin
    end;
drop_known(Bin, _CpLen, N) ->
    string_cp_drop(Bin, N).

index_of_known(Hay, CpLen, Needle, From) when CpLen =:= byte_size(Hay) ->
    case Needle of
        <<>> -> {some, min(max(From, 0), CpLen)};
        _ ->
            Start = max(From, 0),
            case Start > CpLen of
                true -> none;
                false ->
                    case binary:match(Hay, Needle, [{scope, {Start, CpLen - Start}}]) of
                        nomatch -> none;
                        {BytePos, _} -> {some, BytePos}
                    end
            end
    end;
index_of_known(Hay, _CpLen, Needle, From) ->
    string_index_of(Hay, Needle, From).

string_cp_slice(Bin, Start, Len) when Start >= 0, Len > 0 ->
    Off = cp_off(Bin, Start, 0),
    <<_:Off/binary, Rest/binary>> = Bin,
    binary:part(Bin, Off, cp_off(Rest, Len, 0));
string_cp_slice(_, _, _) -> <<>>.

string_cp_drop(Bin, N) when N > 0 ->
    Off = cp_off(Bin, N, 0),
    binary:part(Bin, Off, byte_size(Bin) - Off);
string_cp_drop(Bin, _) -> Bin.

string_cp_explode(Bin) -> cp_explode(Bin, []).
cp_explode(<<>>, Acc) -> lists:reverse(Acc);
cp_explode(<<C/utf8, Rest/binary>>, Acc) -> cp_explode(Rest, [<<C/utf8>> | Acc]).

string_split(Hay, Sep, Lim) ->
    Parts = binary:split(Hay, Sep, [global]),
    case length(Parts) > Lim of
        true -> lists:sublist(Parts, Lim);
        false -> Parts
    end.

%% search is non-empty
string_replace_literal(Hay, Search, Repl, true) ->
    binary:replace(Hay, Search, Repl, [global]);
string_replace_literal(Hay, Search, Repl, false) ->
    binary:replace(Hay, Search, Repl, []).

string_repeat(Bin, N) when N > 0 -> binary:copy(Bin, N);
string_repeat(_, _) -> <<>>.

cp_byte_offset(Bin, N) -> cp_off(Bin, N, 0).

cp_off(<<W1:56, W2:56, W3:56, W4:56, R/binary>>, N, Off)
    when N >= 28, (W1 bor W2 bor W3 bor W4) band 16#80808080808080 =:= 0 ->
    cp_off(R, N - 28, Off + 28);
cp_off(<<W:56, R/binary>>, N, Off)
    when N >= 7, W band 16#80808080808080 =:= 0 ->
    cp_off(R, N - 7, Off + 7);
cp_off(<<C, R/binary>>, N, Off) when N >= 1, C < 16#80 ->
    cp_off(R, N - 1, Off + 1);
cp_off(<<C, _, R/binary>>, N, Off) when N >= 1, C >= 16#C0, C < 16#E0 ->
    cp_off(R, N - 1, Off + 2);
cp_off(<<C, _, _, R/binary>>, N, Off) when N >= 1, C >= 16#E0, C < 16#F0 ->
    cp_off(R, N - 1, Off + 3);
cp_off(<<C, _, _, _, R/binary>>, N, Off) when N >= 1, C >= 16#F0 ->
    cp_off(R, N - 1, Off + 4);
cp_off(<<>>, _N, Off) -> Off;
cp_off(_Bin, 0, Off) -> Off.

string_ascii_upper(Bin) ->
    ascii_map(Bin, 16#1F1F1F1F1F1F1F, 16#05050505050505, <<>>).
string_ascii_lower(Bin) ->
    ascii_map(Bin, 16#3F3F3F3F3F3F3F, 16#25252525252525, <<>>).

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
