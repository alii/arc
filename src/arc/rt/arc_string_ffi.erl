-module(arc_string_ffi).
-export([string_char_at/2, string_codepoint_at/2, string_codepoint_length/1,
         replacement_codepoint/0]).
-export([string_index_of/3, string_last_index_of/3]).
-export([string_cp_slice/3, string_cp_drop/2, string_cp_explode/1]).
-export([string_ascii_upper/1, string_ascii_lower/1]).

%% Bytes of match-start window string_last_index_of scans per backward step.
%% Big enough that a whole small string is one window; small enough that a
%% dense haystack never builds a huge match list.
-define(LAST_INDEX_CHUNK, 65536).

%% Fast string indexing by codepoint (not grapheme cluster). Gleam's
%% string.slice/string.length do grapheme segmentation via unicode_util:gc
%% which is ~20x slower and spec-incorrect for JS (which uses UTF-16 code
%% units). Codepoints are closer to correct and far cheaper.
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
%% Both walks start from byte 0 on every call: string_char_at is O(i) and
%% string_codepoint_length is O(n), so `for (i = 0; i < s.length; i++)
%% use(s[i])` is O(n^2) in the string length.

string_char_at(Bin, Idx) ->
    case string_codepoint_at(Bin, Idx) of
        {some, C} -> {some, <<C/utf8>>};
        none -> none
    end.

%% Same walk as string_char_at, but returns the codepoint as an integer —
%% for String.prototype.codePointAt, where building even a one-char binary
%% per call would be wasted allocation.
string_codepoint_at(Bin, Idx) when Idx >= 0 ->
    case char_at_skip(Bin, Idx) of
        none -> none;
        Char -> {some, Char}
    end;
string_codepoint_at(_, _) -> none.

%% Walk N codepoints forward, returning the integer codepoint there. Running
%% out of string is `none` (an out-of-range index); an invalid byte is not a
%% clause here — see the invalid-UTF-8 policy above.
char_at_skip(<<C/utf8, _/binary>>, 0) -> C;
char_at_skip(<<_/utf8, Rest/binary>>, N) -> char_at_skip(Rest, N - 1);
char_at_skip(<<>>, _) -> none.

%% U+FFFD REPLACEMENT CHARACTER. UtfCodepoint is an integer on the Erlang
%% target, so this is a constant-pool literal — no Result/assert overhead.
replacement_codepoint() -> 16#FFFD.

string_codepoint_length(Bin) -> cp_length(Bin, 0).
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
%% index From.
%%
%% A winning match starts at =< Limit, so it lies wholly inside the first
%% `Limit + byte_size(Needle)` bytes: search only that prefix. That bound is
%% both the spec's fromIndex filter (no separate `BytePos > Limit` guard is
%% needed) and an early exit — `hugeString.lastIndexOf(x, 0)` must not scan
%% the whole haystack.
string_last_index_of(Hay, <<>>, From) ->
    {some, clamp_cp(Hay, From)};
string_last_index_of(Hay, Needle, From) ->
    Limit = cp_byte_offset(Hay, max(From, 0)),
    last_index_of(Hay, Needle, min(Limit + byte_size(Needle), byte_size(Hay))).

%% End is the exclusive byte bound of the searchable prefix; below one needle
%% length there is no room for a match starting at or before Limit.
last_index_of(_Hay, Needle, End) when End < byte_size(Needle) -> none;
last_index_of(Hay, Needle, End) ->
    HighestStart = End - byte_size(Needle),
    Chunk = max(?LAST_INDEX_CHUNK, 2 * byte_size(Needle)),
    case scan_back(Hay, Needle, max(0, HighestStart - Chunk + 1), HighestStart, Chunk) of
        none -> none;
        {some, BytePos} -> {some, cp_length(binary:part(Hay, 0, BytePos), 0)}
    end.

%% Highest match start in the window of starts [Lo, Hi], else the next window
%% down, else none.
%%
%% Chunking backwards keeps this O(1) windows for the common case where the
%% last occurrence is near the end, and — unlike one binary:matches over the
%% whole prefix — never materialises a match list proportional to the haystack
%% ("x".repeat(5.0e6) has 2.5M matches of "xx"; that list is ~100 MB).
scan_back(Hay, Needle, Lo, Hi, Chunk) ->
    M = byte_size(Needle),
    %% Scope spans the window's starts plus one needle, so a match starting at
    %% Hi still fits inside it.
    case binary:matches(Hay, Needle, [{scope, {Lo, Hi + M - Lo}}]) of
        [] when Lo =:= 0 -> none;
        [] -> scan_back(Hay, Needle, max(0, Lo - Chunk), Lo - 1, Chunk);
        Matches ->
            %% binary:matches yields the leftmost NON-overlapping matches, so
            %% its last hit L can be beaten by an overlapping one ("xx" in
            %% "xxxxxxx": matches stops at 4, the answer is 5). Nothing starts
            %% at or after L + M (matches resumed there and found none), so the
            %% true last start is in [L, L + M - 1] — at most M positions.
            {L, _} = lists:last(Matches),
            {some, latest_overlap(Hay, Needle, L, min(L + M - 1, Hi))}
    end.

latest_overlap(_Hay, _Needle, L, Pos) when Pos =< L -> L;
latest_overlap(Hay, Needle, L, Pos) ->
    case binary:part(Hay, Pos, byte_size(Needle)) of
        Needle -> Pos;
        _Other -> latest_overlap(Hay, Needle, L, Pos - 1)
    end.

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

%% Case-map an all-ASCII string 7 bytes per step; `none` at the first
%% non-ASCII byte so the caller can run the full Unicode mapping instead.
%% ASCII has no context-sensitive casing, so this equals string:uppercase /
%% string:lowercase wherever it answers. Per byte b (upper shown, lower is
%% the same with 16#3F/16#25): b in [$a,$z] <=> bit 7 of b+16#1F is set and
%% bit 7 of b+16#05 is clear; that bit shifted down to 16#20 is the case bit.
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
