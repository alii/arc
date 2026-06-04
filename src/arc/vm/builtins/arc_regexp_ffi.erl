-module(arc_regexp_ffi).
-export([regexp_exec/4, regexp_test/3, canonical_flags/1, byte_slice/3,
         byte_drop_start/2, next_char_boundary/2]).

%% O(1) sub-binary slice by byte offsets. re:run returns byte indices, so
%% slicing must be byte-based (grapheme-based slicing would be both wrong
%% and O(offset+len)). Bounds are clamped so binary:part never raises.
byte_slice(Bin, Start, Len) ->
    Size = byte_size(Bin),
    S = min(max(Start, 0), Size),
    L = min(max(Len, 0), Size - S),
    binary:part(Bin, S, L).

%% O(1) suffix from byte offset Start (clamped).
byte_drop_start(Bin, Start) ->
    Size = byte_size(Bin),
    S = min(max(Start, 0), Size),
    binary:part(Bin, S, Size - S).

%% Smallest UTF-8 character boundary strictly greater than Pos. Skips
%% continuation bytes (2#10xxxxxx). re:run raises badarg for an offset in
%% the middle of a multibyte character, so byte-offset loops that step
%% forward (AdvanceStringIndex, ES2026 22.2.7.3) must use this instead of
%% Pos + 1. May return a value past byte_size(Bin) (e.g. Pos at the last
%% character), which callers use as their loop-termination signal.
next_char_boundary(Bin, Pos) ->
    next_boundary(Bin, Pos + 1, byte_size(Bin)).

next_boundary(_Bin, P, Size) when P >= Size -> P;
next_boundary(Bin, P, Size) ->
    case binary:at(Bin, P) band 16#C0 of
        16#80 -> next_boundary(Bin, P + 1, Size);
        _ -> P
    end.

%% Spec order "dgimsuvy" is ascending ASCII, so byte-sort = canonical order.
canonical_flags(Flags) ->
    list_to_binary(lists:sort(binary_to_list(Flags))).

%% Convert JS flags to re:compile options
flags_to_opts(Flags) ->
    flags_to_opts(Flags, [unicode]).  %% always enable unicode for UTF-8 strings

flags_to_opts(<<>>, Acc) -> Acc;
flags_to_opts(<<"i", Rest/binary>>, Acc) -> flags_to_opts(Rest, [caseless | Acc]);
flags_to_opts(<<"m", Rest/binary>>, Acc) -> flags_to_opts(Rest, [multiline | Acc]);
flags_to_opts(<<"s", Rest/binary>>, Acc) -> flags_to_opts(Rest, [dotall | Acc]);
flags_to_opts(<<_, Rest/binary>>, Acc) -> flags_to_opts(Rest, Acc).
%% g, y, u, d, v are handled at the Gleam level, not PCRE options

%% Get a compiled pattern, caching it in the process dictionary. re:run/3
%% with a binary pattern recompiles the PCRE pattern on every call, and the
%% global match/replace/split loops at the Gleam level call exec once per
%% match — so a /g operation with k matches compiled the same regex k times.
%% Caching the compiled MP makes that one compile + k cheap match calls.
%% Keyed by {Pattern, CompileOpts} so flag chars that do not affect
%% compilation (g/y/d/etc.) share an entry.
get_compiled(Pattern, Flags) ->
    Opts = flags_to_opts(Flags),
    Key = {arc_re_mp, Pattern, Opts},
    case erlang:get(Key) of
        undefined ->
            case re:compile(Pattern, Opts) of
                {ok, MP} ->
                    cache_put(Key, MP),
                    MP;
                {error, _Reason} ->
                    %% Same failure mode as re:run/3 on an invalid pattern.
                    erlang:error(badarg)
            end;
        MP ->
            MP
    end.

%% Max compiled patterns cached per process before the cache is flushed.
-define(CACHE_MAX, 512).

%% Bound cache size: flush all entries once the cap is hit. Real programs
%% have a small, fixed set of patterns; the cap only triggers for
%% pathological dynamically-generated patterns, where recompiling matches
%% the old behavior anyway.
cache_put(Key, MP) ->
    N = case erlang:get(arc_re_mp_count) of
            undefined -> 0;
            C -> C
        end,
    case N >= ?CACHE_MAX of
        true ->
            [erlang:erase(K) || {{arc_re_mp, _, _} = K, _} <- erlang:get()],
            erlang:put(arc_re_mp_count, 1);
        false ->
            erlang:put(arc_re_mp_count, N + 1)
    end,
    erlang:put(Key, MP).

%% regexp_test(Pattern, Flags, String) -> true | false
regexp_test(Pattern, Flags, String) ->
    case safe_run(String, Pattern, Flags, [{capture, none}]) of
        match -> true;
        {match, _} -> true;
        _ -> false
    end.

%% Run with a per-process cached compiled pattern. re:compile/re:run raise
%% badarg on a pattern PCRE can't handle (or an out-of-range offset); catch it
%% so such a regex degrades to "no match" instead of crashing the VM.
safe_run(String, Pattern, Flags, RunOpts) ->
    try
        MP = get_compiled(translate_pattern(Pattern), Flags),
        re:run(String, MP, RunOpts)
    catch _:_ -> nomatch
    end.

%% Translate the JS-regex escapes PCRE doesn't accept into their PCRE form.
%% Currently: \uHHHH and \u{H..} -> \x{H..}. Backslash escapes are consumed in
%% pairs so an escaped backslash (\\) before a `u` is not misread as a unicode
%% escape. The original (untranslated) source is what RegExp.prototype.source
%% returns; this only affects what is handed to re.
translate_pattern(Pattern) ->
    unicode:characters_to_binary(
        translate_pat(unicode:characters_to_list(Pattern), false)).

%% Word-character class matching JS \w: [0-9A-Za-z_]. Used both as a class and,
%% under PCRE caseless, to get JS's case-fold closure for free — caseless
%% folding maps U+017F (long s) to s and U+212A (Kelvin) to k, so `(?i:[a-z])`
%% matches them. PCRE's own \w doesn't do this, and `ucp` over-broadens \s/\b.
-define(WORD, "[0-9A-Za-z_]").
-define(NWORD, "[^0-9A-Za-z_]").

%% Second argument tracks whether we are inside a [...] character class, where
%% \w/\b are not the same productions (\b is backspace) and must be left alone.
translate_pat([], _InClass) -> [];
translate_pat([$\\, $u, ${ | Rest], InClass) ->
    case take_hex(Rest, []) of
        {Hex, [$} | Rest2]} when Hex =/= [] ->
            [$\\, $x, ${] ++ Hex ++ [$}] ++ translate_pat(Rest2, InClass);
        _ ->
            [$\\, $u, ${ | translate_pat(Rest, InClass)]
    end;
translate_pat([$\\, $u, A, B, C, D | Rest], InClass) ->
    case is_hex(A) andalso is_hex(B) andalso is_hex(C) andalso is_hex(D) of
        true -> [$\\, $x, ${, A, B, C, D, $} | translate_pat(Rest, InClass)];
        false -> [$\\, $u | translate_pat([A, B, C, D | Rest], InClass)]
    end;
%% \w, \W, \b, \B outside a character class -> explicit JS forms so caseless
%% folding includes long-s / Kelvin (matching the JS word-char case closure).
translate_pat([$\\, $w | Rest], false) -> ?WORD ++ translate_pat(Rest, false);
translate_pat([$\\, $W | Rest], false) -> ?NWORD ++ translate_pat(Rest, false);
translate_pat([$\\, $b | Rest], false) ->
    %% Word boundary: word|nonword transition (start/end count as nonword).
    "(?:(?<=" ?WORD ")(?!" ?WORD ")|(?<!" ?WORD ")(?=" ?WORD "))"
        ++ translate_pat(Rest, false);
translate_pat([$\\, $B | Rest], false) ->
    %% Non-boundary: both sides word, or both sides nonword/edge.
    "(?:(?<=" ?WORD ")(?=" ?WORD ")|(?<!" ?WORD ")(?!" ?WORD "))"
        ++ translate_pat(Rest, false);
%% Preserve any other escape pair verbatim (don't reinterpret its 2nd char).
translate_pat([$\\, C | Rest], InClass) -> [$\\, C | translate_pat(Rest, InClass)];
%% Track character-class nesting on unescaped brackets.
translate_pat([$[ | Rest], false) -> [$[ | translate_pat(Rest, true)];
translate_pat([$] | Rest], true) -> [$] | translate_pat(Rest, false)];
translate_pat([C | Rest], InClass) -> [C | translate_pat(Rest, InClass)].

take_hex([C | Rest], Acc) ->
    case is_hex(C) of
        true -> take_hex(Rest, [C | Acc]);
        false -> {lists:reverse(Acc), [C | Rest]}
    end;
take_hex([], Acc) -> {lists:reverse(Acc), []}.

is_hex(C) ->
    (C >= $0 andalso C =< $9)
        orelse (C >= $a andalso C =< $f)
        orelse (C >= $A andalso C =< $F).

%% regexp_exec(Pattern, Flags, String, Offset) -> {ok, Matches} | {error, nil}
%% Matches = [{Start, Length}, ...] for full match + captures.
%%
%% Offset and the returned Start/Length are BYTE indices into the UTF-8
%% binary — the Gleam caller slices with byte_slice/byte_drop_start and steps
%% empty matches with next_char_boundary, so no grapheme conversion happens
%% on either side. An offset past the end of the string is no-match (re:run
%% raises badarg for it; JS semantics for lastIndex > length are a failed
%% match).
regexp_exec(_Pattern, _Flags, String, Offset) when Offset > byte_size(String) ->
    {error, nil};
regexp_exec(Pattern, Flags, String, Offset) ->
    Opts = [{offset, Offset}, {capture, all, index}],
    case safe_run(String, Pattern, Flags, Opts) of
        {match, Captured} -> {ok, Captured};
        _ -> {error, nil}
    end.
