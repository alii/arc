-module(arc_regexp_ffi).
-export([regexp_exec/4, regexp_test/3, canonical_flags/1]).

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

%% regexp_test(Pattern, Flags, String) -> true | false
regexp_test(Pattern, Flags, String) ->
    Opts = flags_to_opts(Flags),
    case safe_run(String, translate_pattern(Pattern), Opts) of
        {match, _} -> true;
        _ -> false
    end.

%% re:run raises badarg on a pattern PCRE can't compile. Catch it so a regex
%% the engine can't handle degrades to "no match" instead of crashing the VM.
safe_run(String, Pattern, Opts) ->
    try re:run(String, Pattern, Opts)
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
%% Offset and the returned Start/Length are GRAPHEME indices, matching Gleam's
%% string:slice / string:length (which the caller uses). re:run, however, works
%% on byte offsets of the UTF-8 binary, so we convert at the boundary. Without
%% this, a non-ASCII match advances lastIndex by graphemes but feeds it back as
%% a byte offset, landing mid-character — re:run then crashes with badarg.
%% For ASCII input grapheme == byte, so this is a no-op there.
regexp_exec(Pattern, Flags, String, Offset) ->
    ByteOffset = grapheme_to_byte(String, Offset),
    Opts = [{offset, ByteOffset}, {capture, all, index} | flags_to_opts(Flags)],
    case safe_run(String, translate_pattern(Pattern), Opts) of
        {match, Captured} ->
            {ok, [byte_span_to_grapheme(String, Span) || Span <- Captured]};
        _ ->
            {error, nil}
    end.

%% Byte offset of the start of the Nth grapheme (clamped to byte_size).
grapheme_to_byte(_String, N) when N =< 0 -> 0;
grapheme_to_byte(String, N) ->
    case string:slice(String, 0, N) of
        Prefix when is_binary(Prefix) -> byte_size(Prefix);
        Prefix -> byte_size(unicode:characters_to_binary(Prefix))
    end.

%% Convert a {ByteStart, ByteLen} span from re into a {GraphemeStart, GraphemeLen}
%% span. Unmatched optional captures come back as {-1, 0}; pass them through.
byte_span_to_grapheme(_String, {ByteStart, _}) when ByteStart < 0 -> {-1, 0};
byte_span_to_grapheme(String, {ByteStart, ByteLen}) ->
    GStart = byte_to_grapheme(String, ByteStart),
    GEnd = byte_to_grapheme(String, ByteStart + ByteLen),
    {GStart, GEnd - GStart}.

%% Number of graphemes in the first BytePos bytes of String.
byte_to_grapheme(String, BytePos) ->
    <<Prefix:BytePos/binary, _/binary>> = String,
    string:length(Prefix).
