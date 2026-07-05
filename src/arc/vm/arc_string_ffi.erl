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
%% INVALID UTF-8 POLICY (the one policy, for every walker below): a JS string
%% is always well-formed UTF-8 — every construction path maps lone surrogates
%% to U+FFFD (arc_escape_ffi:encode_codepoint, char_codes_to_string,
%% from_code_point_loop) — so a bad byte reaching here means the boundary that
%% produced the string is broken. Every walker therefore has NO per-byte
%% fallback clause and crashes with function_clause on one, at the string it
%% was handed, instead of quietly counting it as a codepoint (cp_length,
%% cp_off, cp_explode used to) or reporting end-of-string (char_at_skip did) —
%% three answers to the same question, none of them a bug report.
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
%% Running out of string is `none` (an out-of-range index); an invalid byte is
%% not a clause here — see the invalid-UTF-8 policy above.
char_at_skip(<<C/utf8, _/binary>>, 0, Off) -> {C, Off};
char_at_skip(<<C/utf8, Rest/binary>>, N, Off) ->
    char_at_skip(Rest, N - 1, Off + cp_byte_size(C));
char_at_skip(<<>>, _, _) -> none.

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
cp_length(<<_/utf8, Rest/binary>>, N) -> cp_length(Rest, N + 1).

%% StringIndexOf (§7.1.18) and its reverse. Both return `none | {some, Idx}`
%% (an Option(Int) on the Gleam side) — no -1 sentinel to forget to test.
%%
%% Both search at the BYTE level with binary:match, so forward and reverse
%% agree by construction: they see the same occurrences of the same needle.
%% (`string:find/3` would not — it is grapheme-cluster aware, so it misses a
%% needle that ends inside a cluster, e.g. "e" in "e\x{301}", which the
%% forward byte search finds.)
%%
%% The empty needle is the spec's step-2 special case (`return fromIndex` when
%% fromIndex =< len) and lives here rather than in a caller: binary:match
%% badargs on <<>>, and an uncatchable BEAM crash is not something a total
%% Gleam signature may hide behind a hand-written wrapper.

%% Skip From codepoints to a byte offset, run binary:match (Boyer-Moore BIF)
%% over the remaining scope, convert the match's byte position back to a
%% codepoint index.
string_index_of(Hay, <<>>, From) ->
    {some, clamp_cp(Hay, From)};
string_index_of(Hay, Needle, From) ->
    Start = cp_byte_offset(Hay, max(From, 0)),
    case binary:match(Hay, Needle, [{scope, {Start, byte_size(Hay) - Start}}]) of
        nomatch -> none;
        {BytePos, _} -> {some, cp_length(binary:part(Hay, 0, BytePos), 0)}
    end.

%% Reverse StringIndexOf: the last occurrence starting at or before codepoint
%% index From. Walks matches forward (advancing one byte past each match start,
%% so overlapping needles are all seen) and keeps the last one whose byte
%% offset is =< the byte offset of codepoint index From.
string_last_index_of(Hay, <<>>, From) ->
    {some, clamp_cp(Hay, From)};
string_last_index_of(Hay, Needle, From) ->
    Limit = cp_byte_offset(Hay, max(From, 0)),
    case last_match(Hay, Needle, 0, Limit, none) of
        none -> none;
        {some, BytePos} -> {some, cp_length(binary:part(Hay, 0, BytePos), 0)}
    end.

last_match(Hay, Needle, Pos, Limit, Best) when Pos =< byte_size(Hay) ->
    case binary:match(Hay, Needle, [{scope, {Pos, byte_size(Hay) - Pos}}]) of
        nomatch -> Best;
        {BytePos, _} when BytePos > Limit -> Best;
        {BytePos, _} -> last_match(Hay, Needle, BytePos + 1, Limit, {some, BytePos})
    end;
last_match(_Hay, _Needle, _Pos, _Limit, Best) -> Best.

%% The empty needle matches at From, clamped into [0, len] (spec step 2 read
%% together with the callers' step-7/step-8 clamp).
clamp_cp(Hay, From) -> min(max(From, 0), string_codepoint_length(Hay)).

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
cp_explode(<<C/utf8, Rest/binary>>, Acc) -> cp_explode(Rest, [<<C/utf8>> | Acc]).

%% Byte offset after skipping N codepoints (clamps at end). Alloc-free.
cp_byte_offset(Bin, N) -> cp_off(Bin, N, 0).

%% Codepoint-skip walker. Returns the byte offset (an integer, never the
%% binary) and every clause begins with a binary match — both are required
%% for BEAM's match-context reuse (a leading non-binary clause forces a
%% sub-binary allocation on every step; verify with erlc +bin_opt_info).
%% The W:56 clause batches 7 ASCII bytes per step (high bit of every byte
%% clear means 7 one-byte codepoints; 56 bits stays an immediate small
%% int — 64 would allocate a bignum per step). Non-ASCII steps skip by
%% UTF-8 lead byte class without decoding the codepoint. An invalid lead byte
%% (or a truncated multibyte sequence) matches no clause and crashes — see the
%% invalid-UTF-8 policy at the top of the module. The two terminal clauses are
%% "ran off the end" (clamp) and "skipped them all", nothing else.
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
