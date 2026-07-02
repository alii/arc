-module(arc_string_ffi).
-export([string_char_at/2, string_codepoint_at/2, string_codepoint_length/1,
         replacement_codepoint/0]).
-export([string_index_of/3, string_last_index_of/3]).
-export([string_cp_slice/3, string_cp_drop/2, string_cp_explode/1]).

%% Fast string indexing by codepoint (not grapheme cluster). Gleam's
%% string.slice/string.length do grapheme segmentation via unicode_util:gc
%% which is ~20x slower and spec-incorrect for JS (which uses UTF-16 code
%% units). Codepoints are closer to correct and far cheaper.
%%
%% Consumers: src/arc/vm/builtins/string.gleam and src/arc/vm/ops/object.gleam
%% (StringGetOwnProperty / "length").
%%
%% TODO(Deviation): still not fully spec-correct — JS indexes by UTF-16
%% code unit, so astral-plane chars (U+10000+) should count as 2 indices.
%% A full fix needs UTF-16 string storage. Codepoint indexing matches
%% grapheme indexing for all BMP chars so this is strictly more correct
%% than the previous string.slice approach.
%%
%% Both entry points keep a one-entry per-process cache so the canonical
%% JS idiom `for (i = 0; i < s.length; i++) use(s[i])` is O(n) instead of
%% O(n^2):
%%   - string_codepoint_length caches {Bin, Len}: the first call scans,
%%     repeat calls on the same string are O(1).
%%   - string_char_at keeps a cursor {Bin, CharIdx, ByteOffset} and resumes
%%     the UTF-8 walk from the last position for forward access, so a
%%     sequential scan advances one codepoint per call instead of re-walking
%%     from byte 0 (O(i) per access).
%% Cache hits use `=:=`, which is O(1) when the argument is the identical
%% heap term (the common case: the same JsString value threaded through the
%% interpreter loop) and fails fast on different strings (size compared
%% first). Binaries are immutable, so a hit can never be stale; misses just
%% replace the entry. Caches are per-process, so concurrent VM processes
%% (generators, actors) are isolated and merely start cold.
-define(STR_LEN_CACHE, '$arc_str_len_cache').
-define(STR_POS_CACHE, '$arc_str_pos_cache').

string_char_at(Bin, Idx) ->
    case string_codepoint_at(Bin, Idx) of
        {some, C} -> {some, <<C/utf8>>};
        none -> none
    end.

%% Same cursor-cached walk as string_char_at, but returns the codepoint as an
%% integer — for String.prototype.codePointAt, where building even a one-char
%% binary per call would be wasted allocation.
string_codepoint_at(Bin, Idx) when Idx >= 0 ->
    {Base, Skip} =
        case get(?STR_POS_CACHE) of
            {B, CIdx, COff} when B =:= Bin, CIdx =< Idx -> {COff, Idx - CIdx};
            _ -> {0, Idx}
        end,
    <<_:Base/binary, Rest/binary>> = Bin,
    case char_at_skip(Rest, Skip, Base) of
        {Char, Off} ->
            put(?STR_POS_CACHE, {Bin, Idx, Off}),
            {some, Char};
        none -> none
    end;
string_codepoint_at(_, _) -> none.

%% Walk N codepoints forward, returning the integer codepoint there plus its
%% byte offset (for the cursor cache). Off accumulates from the caller's base.
char_at_skip(<<C/utf8, _/binary>>, 0, Off) -> {C, Off};
char_at_skip(<<C/utf8, Rest/binary>>, N, Off) ->
    char_at_skip(Rest, N - 1, Off + cp_byte_size(C));
char_at_skip(_, _, _) -> none.

%% UTF-8 encoded byte length of a codepoint.
cp_byte_size(C) when C < 16#80 -> 1;
cp_byte_size(C) when C < 16#800 -> 2;
cp_byte_size(C) when C < 16#10000 -> 3;
cp_byte_size(_) -> 4.

%% U+FFFD REPLACEMENT CHARACTER. UtfCodepoint is an integer on the Erlang
%% target, so this is a constant-pool literal — no Result/assert overhead.
replacement_codepoint() -> 16#FFFD.

string_codepoint_length(Bin) ->
    case get(?STR_LEN_CACHE) of
        {B, Len} when B =:= Bin -> Len;
        _ ->
            Len = cp_length(Bin, 0),
            put(?STR_LEN_CACHE, {Bin, Len}),
            Len
    end.
%% W:56 clause: 7 ASCII bytes per step, small-int safe (see cp_drop).
cp_length(<<W:56, Rest/binary>>, N)
    when W band 16#80808080808080 =:= 0 ->
    cp_length(Rest, N + 7);
cp_length(<<>>, N) -> N;
cp_length(<<_/utf8, Rest/binary>>, N) -> cp_length(Rest, N + 1);
cp_length(<<_, Rest/binary>>, N) -> cp_length(Rest, N + 1).

%% O(n) StringIndexOf: skip From codepoints to a byte offset, run
%% binary:match (Boyer-Moore BIF) over the remaining scope, convert the
%% match's byte position back to a codepoint index. Caller handles the
%% empty-needle case (binary:match badargs on <<>>).
string_index_of(Hay, Needle, From) ->
    Start = cp_byte_offset(Hay, max(From, 0)),
    case binary:match(Hay, Needle, [{scope, {Start, byte_size(Hay) - Start}}]) of
        nomatch -> -1;
        {BytePos, _} -> cp_length(binary:part(Hay, 0, BytePos), 0)
    end.

%% O(n) reverse StringIndexOf: restrict to the first (From + |Needle|_cp)
%% codepoints, ask string:find/3 for the trailing (last) occurrence —
%% handles overlapping needles — then count codepoints before the match.
string_last_index_of(Hay, Needle, From) ->
    Limit = cp_byte_offset(Hay, max(From, 0) + cp_length(Needle, 0)),
    Prefix = binary:part(Hay, 0, Limit),
    case string:find(Prefix, Needle, trailing) of
        nomatch -> -1;
        Suffix -> cp_length(binary:part(Hay, 0, byte_size(Prefix) - byte_size(Suffix)), 0)
    end.

%% Codepoint-based substring: Len codepoints starting at codepoint Start.
%% Plain UTF-8 byte walk + binary:part — returns a sub-binary referencing
%% the original, so no per-character allocation (vs gleam/string.slice's
%% grapheme clustering which allocates a list cell per character).
string_cp_slice(Bin, Start, Len) when Start >= 0, Len > 0 ->
    Off = cp_off(Bin, Start, 0),
    <<_:Off/binary, Rest/binary>> = Bin,
    binary:part(Bin, Off, cp_off(Rest, Len, 0));
string_cp_slice(_, _, _) -> <<>>.

%% Drop the first N codepoints; sub-binary, alloc-free walk.
string_cp_drop(Bin, N) when N > 0 ->
    Off = cp_off(Bin, N, 0),
    binary:part(Bin, Off, byte_size(Bin) - Off);
string_cp_drop(Bin, _) -> Bin.

%% Split into single-codepoint binaries (String.prototype.split("")).
string_cp_explode(Bin) -> cp_explode(Bin, []).
cp_explode(<<>>, Acc) -> lists:reverse(Acc);
cp_explode(<<C/utf8, Rest/binary>>, Acc) -> cp_explode(Rest, [<<C/utf8>> | Acc]);
cp_explode(<<B, Rest/binary>>, Acc) -> cp_explode(Rest, [<<B>> | Acc]).

%% Byte offset after skipping N codepoints (clamps at end). Alloc-free.
cp_byte_offset(Bin, N) -> cp_off(Bin, N, 0).

%% Codepoint-skip walker. Returns the byte offset (an integer, never the
%% binary) and every clause begins with a binary match — both are required
%% for BEAM's match-context reuse (a leading non-binary clause forces a
%% sub-binary allocation on every step; verify with erlc +bin_opt_info).
%% The W:56 clause batches 7 ASCII bytes per step (high bit of every byte
%% clear means 7 one-byte codepoints; 56 bits stays an immediate small
%% int — 64 would allocate a bignum per step). Non-ASCII steps skip by
%% UTF-8 lead byte class without decoding the codepoint. Invalid lead
%% bytes advance one byte, matching cp_length's per-byte fallback.
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
cp_off(<<_, R/binary>>, N, Off) when N >= 1 -> cp_off(R, N - 1, Off + 1);
cp_off(_, _, Off) -> Off.
