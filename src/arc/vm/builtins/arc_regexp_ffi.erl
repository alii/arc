%% JS RegExp -> PCRE (`re`) bridge: JS flags to re:compile options, the source
%% pattern scan (group count / named groups), the escape + v-mode class
%% translation, the compiled-pattern cache and exec.
%%
%% Two neighbours own the parts that are not about `re`:
%%   arc_regex_charset — the codepoint set algebra (union/intersection/
%%     complement over ranges), simple case folding, and rendering a set back
%%     to a PCRE class. Pure, no `re` dependency.
%%   arc_bytes_ffi     — byte-offset UTF-8 slicing, shared with the lexer.
-module(arc_regexp_ffi).
-export([regexp_exec_info/5,
         byte_slice/3, byte_drop_start/2, next_char_boundary/2]).

%% The codepoint-set algebra module, abbreviated: it appears often enough in
%% the v-mode class evaluator that spelling it out would hide the code.
-define(CS, arc_regex_charset).

%% Byte-offset helpers for the Gleam caller. re:run returns byte indices, so
%% slicing must be byte-based (grapheme-based slicing would be both wrong and
%% O(offset+len)). One implementation, one out-of-range policy: arc_bytes_ffi.
byte_slice(Bin, Start, Len) -> arc_bytes_ffi:unsafe_slice(Bin, Start, Len).
byte_drop_start(Bin, Start) -> arc_bytes_ffi:drop_start(Bin, Start).
next_char_boundary(Bin, Pos) -> arc_bytes_ffi:next_char_boundary(Bin, Pos).

%% Convert JS flags to re:compile options
flags_to_opts(Flags) ->
    flags_to_opts(Flags, [unicode]).  %% always enable unicode for UTF-8 strings

flags_to_opts(<<>>, Acc) -> Acc;
flags_to_opts(<<"i", Rest/binary>>, Acc) -> flags_to_opts(Rest, [caseless | Acc]);
flags_to_opts(<<"m", Rest/binary>>, Acc) -> flags_to_opts(Rest, [multiline | Acc]);
flags_to_opts(<<"s", Rest/binary>>, Acc) -> flags_to_opts(Rest, [dotall | Acc]);
flags_to_opts(<<_, Rest/binary>>, Acc) -> flags_to_opts(Rest, Acc).
%% g, y, u, d, v are handled at the Gleam level, not PCRE options

%% get_compiled(Pattern, Flags) -> {ok, {MP, GroupCount, Names}}
%%                               | {error, {compile_failed, Reason}}
%%
%% Get a compiled pattern, caching it in the process dictionary. re:run/3
%% with a binary pattern recompiles the PCRE pattern on every call, and the
%% global match/replace/split loops at the Gleam level call exec once per
%% match — so a /g operation with k matches compiled the same regex k times.
%% Caching the compiled MP makes that one compile + k cheap match calls.
%% Translation happens behind the same cache: property escapes can expand
%% into multi-kilobyte classes, so re-translating per exec would dominate
%% tight test()/exec loops over short subjects. The scan of the source
%% pattern (group count + named groups) is cached alongside the MP, so
%% regexp_exec_info gets it for free on a hit. Keyed by the ORIGINAL pattern
%% plus {CompileOpts, UnicodeMode} — both compilation and translation inputs
%% — so flag chars that affect neither (g/y/d/etc.) share an entry.
get_compiled(Pattern, Flags) ->
    Opts = flags_to_opts(Flags),
    Mode = unicode_mode(Flags),
    Key = {arc_re_mp, Pattern, Opts, Mode},
    case erlang:get(Key) of
        undefined ->
            Caseless = lists:member(caseless, Opts),
            {Stripped, GroupCount, Names} = scan_pattern(Pattern),
            Translated = unicode:characters_to_binary(
                           translate_pat(Stripped, false, Mode, Caseless)),
            case re:compile(Translated, Opts) of
                {ok, MP} ->
                    Entry = {MP, GroupCount, Names},
                    cache_put(Key, Entry),
                    {ok, Entry};
                {error, Reason} ->
                    {error, {compile_failed, Reason}}
            end;
        Entry ->
            {ok, Entry}
    end.

%% Max compiled patterns cached per process before the cache is flushed.
-define(CACHE_MAX, 512).

%% Bound cache size: flush all entries once the cap is hit. Real programs
%% have a small, fixed set of patterns; the cap only triggers for
%% pathological dynamically-generated patterns, where recompiling matches
%% the old behavior anyway.
cache_put(Key, Entry) ->
    N = case erlang:get(arc_re_mp_count) of
            undefined -> 0;
            C -> C
        end,
    case N >= ?CACHE_MAX of
        true ->
            [erlang:erase(K) || {{arc_re_mp, _, _, _} = K, _} <- erlang:get()],
            erlang:put(arc_re_mp_count, 1);
        false ->
            erlang:put(arc_re_mp_count, N + 1)
    end,
    erlang:put(Key, Entry).

%% re:run/3 raises badarg for a byte offset that is out of range or lands in
%% the middle of a UTF-8 character; JS semantics for both is a failed match.
%% NOTHING else is caught here: a crash inside the translator (scan_pattern,
%% translate_pat, vclass, ...) is a bug in this module and must surface as a
%% crash rather than masquerade as "the regex didn't match".
run_mp(String, MP, RunOpts) ->
    try re:run(String, MP, RunOpts)
    catch error:badarg -> nomatch
    end.

%% ---- One scan of the source pattern -------------------------------------
%%
%% scan_pattern(Pattern) -> {StrippedChars, GroupCount, Names}
%%
%% A single walk of the JS pattern grammar produces everything the rest of
%% the module needs to know about the source pattern:
%%
%%   StrippedChars — the pattern with named-group syntax removed. JS group
%%     names (any IdentifierName, including $ and astral characters, and
%%     ES2025 duplicate names across alternatives) are a superset of what
%%     PCRE accepts, so "(?<name>" becomes a plain "(" and "\k<name>" becomes
%%     a numeric "\g{N}" backreference. With no named groups in the pattern
%%     \k<...> is left alone (Annex B identity-escape semantics).
%%   GroupCount — number of capturing groups.
%%   Names — [{GroupName, CaptureIndex}] in source order, handed to the Gleam
%%     caller to build the `groups` object. A name may repeat (ES2025).
%%
%% Group counting and name stripping used to be two independent hand-written
%% scanners over the same grammar; nothing forced them to agree. They are one
%% walk now, so they cannot drift.
scan_pattern(Pattern) ->
    {ChunksRev, GroupCount, NamesRev} =
        scan(unicode:characters_to_list(Pattern), false, 0, [], []),
    Names = lists:reverse(NamesRev),
    Stripped = resolve_backrefs(lists:reverse(ChunksRev), index_by_name(Names)),
    {Stripped, GroupCount, Names}.

%% Chunks accumulate in reverse: a codepoint, or a {backref, Name, Raw} that
%% resolve_backrefs/2 fills in once every group index is known (a
%% backreference may textually precede the group it names).
scan([], _InClass, N, Chunks, Names) ->
    {Chunks, N, Names};
scan([$\\, $k, $< | Rest], false, N, Chunks, Names) ->
    {Name, Rest2, Terminated} = take_group_name(Rest),
    Raw = "\\k<" ++ Name ++ case Terminated of true -> ">"; false -> "" end,
    Chunk = {backref, unicode:characters_to_binary(Name), Raw},
    scan(Rest2, false, N, [Chunk | Chunks], Names);
scan([$\\, C | Rest], InClass, N, Chunks, Names) ->
    scan(Rest, InClass, N, [C, $\\ | Chunks], Names);
scan([$[ | Rest], false, N, Chunks, Names) ->
    scan(Rest, true, N, [$[ | Chunks], Names);
scan([$] | Rest], true, N, Chunks, Names) ->
    scan(Rest, false, N, [$] | Chunks], Names);
%% `(?` introduces non-capturing constructs — except `(?<name>` (named
%% capture), distinguished from the lookbehinds `(?<=` / `(?<!` by the third
%% character.
scan([$(, $?, $<, C | Rest], false, N, Chunks, Names) when C =/= $=, C =/= $! ->
    {Name, Rest2, _Terminated} = take_group_name([C | Rest]),
    scan(Rest2, false, N + 1, [$( | Chunks],
         [{unicode:characters_to_binary(Name), N + 1} | Names]);
scan([$(, $? | Rest], false, N, Chunks, Names) ->
    scan(Rest, false, N, [$?, $( | Chunks], Names);
scan([$( | Rest], false, N, Chunks, Names) ->
    scan(Rest, false, N + 1, [$( | Chunks], Names);
scan([C | Rest], InClass, N, Chunks, Names) ->
    scan(Rest, InClass, N, [C | Chunks], Names).

%% [{Name, Idx}] (source order, duplicates allowed) -> [{Name, [Idx]}].
index_by_name(Names) ->
    lists:foldl(
      fun({Name, Idx}, Acc) ->
              case lists:keyfind(Name, 1, Acc) of
                  {Name, Idxs} ->
                      lists:keyreplace(Name, 1, Acc, {Name, Idxs ++ [Idx]});
                  false ->
                      Acc ++ [{Name, [Idx]}]
              end
      end, [], Names).

resolve_backrefs(Chunks, ByName) ->
    lists:flatmap(fun(Chunk) -> resolve_chunk(Chunk, ByName) end, Chunks).

resolve_chunk({backref, Name, Raw}, ByName) ->
    case lists:keyfind(Name, 1, ByName) of
        {_, [Idx]} ->
            "\\g{" ++ integer_to_list(Idx) ++ "}";
        {_, Idxs} ->
            %% ES2025 duplicate group names: the same name is bound by several
            %% groups in different alternatives, so at most one of them can
            %% have participated in the match. An unset PCRE backreference
            %% fails to match, so an alternation over every index picks
            %% whichever group actually did participate.
            Refs = ["\\g{" ++ integer_to_list(I) ++ "}" || I <- Idxs],
            "(?:" ++ lists:append(lists:join("|", Refs)) ++ ")";
        false ->
            %% No such group (or no named groups at all): leave the source
            %% text alone.
            Raw
    end;
resolve_chunk(C, _ByName) when is_integer(C) ->
    [C].

%% Property escapes are only translated in unicode mode (u or v flag) —
%% without it, JS treats \p as an identity escape, not a property. The v
%% flag additionally enables the properties of strings (emoji sequences).
unicode_mode(<<>>) -> none;
unicode_mode(<<"v", _/binary>>) -> v;
unicode_mode(<<"u", Rest/binary>>) ->
    case unicode_mode(Rest) of
        v -> v;
        _ -> u
    end;
unicode_mode(<<_, Rest/binary>>) -> unicode_mode(Rest).

%% Word-character class matching JS \w: [0-9A-Za-z_]. Spelled out (rather than
%% left as PCRE's \w) only under the u/v flag: there JS's Canonicalize is simple
%% case folding, and PCRE's caseless folding of a spelled-out class picks up
%% exactly the two extra word characters the spec asks for — U+017F (long s)
%% folds to s, U+212A (Kelvin) folds to k, so /\w/iu matches them.
%% Without u/v, JS's Canonicalize is the toUpperCase rule, which refuses to map
%% a >=128 character onto a <128 one, so WordCharacters has NO extra members and
%% /\w/i must reject both. A spelled-out class cannot opt out of `caseless`, so
%% non-unicode mode leaves \w/\W to PCRE — they are ASCII-only and are never
%% case-folded, which is precisely the non-unicode semantics. See word_atom/1.
-define(WORD_BODY, "0-9A-Za-z_").
-define(WORD, "[" ?WORD_BODY "]").
-define(NWORD, "[^" ?WORD_BODY "]").

%% JS \s per §22.2.2.9: WhiteSpace + LineTerminator productions — ASCII
%% whitespace plus NBSP, Ogham space, the U+2000 block, LS/PS, NNBSP, MMSP,
%% ideographic space, and BOM. PCRE's \s is ASCII-only, and `ucp` \s both
%% over- and under-shoots (includes U+0085 NEL, excludes U+FEFF BOM).
-define(JSS_CHARS,
        "\\t\\n\\x0B\\f\\r \\x{A0}\\x{1680}\\x{2000}-\\x{200A}"
        "\\x{2028}\\x{2029}\\x{202F}\\x{205F}\\x{3000}\\x{FEFF}").

%% ---- Escape translation --------------------------------------------------
%%
%% Rewrite the JS-regex escapes PCRE doesn't accept, or reads differently, into
%% their PCRE form: \uHHHH and \u{H..} -> \x{H..}; \s/\S/\w/\W/\b/\B -> the
%% explicit JS sets; and (with the u or v flag) \p{...}/\P{...} property escapes
%% -> the PCRE2 property syntax (long General_Category names -> short codes,
%% Script=X -> sc:X, Script_Extensions=X -> scx:X, Assigned -> negated Cn).
%% Backslash escapes are consumed in pairs so an escaped backslash (\\) before
%% a `u` is not misread as a unicode escape. The original (untranslated) source
%% is what RegExp.prototype.source returns; this only affects what re sees.
%%
%% translate_pat(Chars, InClass, Mode, CI)
%%   InClass — inside a [...] character class, where \w/\b are not the same
%%             productions (\b is backspace) and a negated set cannot be
%%             written as a nested class.
%%   Mode    — none | u | v: whether property escapes / v-flag class-set
%%             expressions are in play.
%%   CI      — the i flag. It changes what a class must contain: PCRE folds
%%             every class item at match time, so an emitted set has to be
%%             closed under simple case folding to mean what JS means.
translate_pat([], _InClass, _Mode, _CI) -> [];
translate_pat([$\\, $u, ${ | Rest], InClass, Mode, CI) ->
    case take_hex(Rest, []) of
        {Hex, [$} | Rest2]} when Hex =/= [] ->
            case is_surrogate_hex(Hex) of
                true ->
                    %% Lone surrogate: PCRE refuses \x{D800}-\x{DFFF} in UTF
                    %% mode, which would degrade the WHOLE pattern to
                    %% no-match. Neutralize to a never-matching sentinel.
                    surrogate_sentinel(InClass)
                        ++ translate_pat(Rest2, InClass, Mode, CI);
                false ->
                    [$\\, $x, ${] ++ Hex ++ [$}]
                        ++ translate_pat(Rest2, InClass, Mode, CI)
            end;
        _ ->
            [$\\, $u, ${ | translate_pat(Rest, InClass, Mode, CI)]
    end;
translate_pat([$\\, $u, A, B, C, D | Rest], InClass, Mode, CI) ->
    case is_hex(A) andalso is_hex(B) andalso is_hex(C) andalso is_hex(D) of
        true ->
            V = list_to_integer([A, B, C, D], 16),
            if
                V >= 16#D800, V =< 16#DBFF, not InClass ->
                    %% Lead surrogate outside a class: JS (non-u) regexes pair
                    %% the pattern's code units, so a lead+trail escape pair
                    %% denotes one astral character. Combine an immediately
                    %% following trail-surrogate escape; an unpaired lead
                    %% surrogate can never match (arc strings are well-formed
                    %% Unicode) — emit the sentinel so PCRE still compiles
                    %% the rest of the pattern.
                    case Rest of
                        [$\\, $u, E, F, G, H | Rest2] ->
                            case is_hex(E) andalso is_hex(F) andalso is_hex(G)
                                andalso is_hex(H) of
                                true ->
                                    W = list_to_integer([E, F, G, H], 16),
                                    if
                                        W >= 16#DC00, W =< 16#DFFF ->
                                            CP = 16#10000
                                                + (V - 16#D800) * 16#400
                                                + (W - 16#DC00),
                                            "\\x{" ++ integer_to_list(CP, 16)
                                                ++ "}"
                                                ++ translate_pat(Rest2, InClass, Mode, CI);
                                        true ->
                                            surrogate_sentinel(InClass)
                                                ++ translate_pat(Rest, InClass, Mode, CI)
                                    end;
                                false ->
                                    surrogate_sentinel(InClass)
                                        ++ translate_pat(Rest, InClass, Mode, CI)
                            end;
                        _ ->
                            surrogate_sentinel(InClass)
                                ++ translate_pat(Rest, InClass, Mode, CI)
                    end;
                V >= 16#D800, V =< 16#DFFF ->
                    %% Unpaired trail surrogate, or any surrogate inside a
                    %% character class: never matches a well-formed string —
                    %% the sentinel keeps the pattern compiling.
                    surrogate_sentinel(InClass)
                        ++ translate_pat(Rest, InClass, Mode, CI);
                true ->
                    [$\\, $x, ${, A, B, C, D, $}
                     | translate_pat(Rest, InClass, Mode, CI)]
            end;
        false -> [$\\, $u | translate_pat([A, B, C, D | Rest], InClass, Mode, CI)]
    end;
%% \p{...} / \P{...} in unicode mode -> PCRE2 property syntax. An
%% untranslatable payload is left verbatim; PCRE then fails to compile and
%% the regex degrades to no-match (the parser already rejected invalid names
%% in literals, so this only affects RegExp-constructor patterns).
translate_pat([$\\, P, ${ | Rest], InClass, Mode, CI)
  when (P =:= $p orelse P =:= $P), Mode =/= none ->
    case take_prop(Rest, []) of
        {Payload, Rest2} ->
            case prop_translation(Payload, P =:= $P, InClass, Mode) of
                {ok, Io} ->
                    unicode:characters_to_list(iolist_to_binary(Io))
                        ++ translate_pat(Rest2, InClass, Mode, CI);
                error ->
                    [$\\, P, ${ | translate_pat(Rest, InClass, Mode, CI)]
            end;
        none ->
            [$\\, P, ${ | translate_pat(Rest, InClass, Mode, CI)]
    end;
%% \s, \S -> the explicit JS sets: PCRE's own \s is ASCII-only and `ucp` \s both
%% over- and under-shoots. \w, \W -> the explicit sets under u/v only; see
%% word_atom/1. Outside a class the negated forms are their own negated bracket
%% class; inside a class they must become class ITEMS (classes cannot nest), so
%% the negated forms splice their complement — see class_complement/2.
translate_pat([$\\, $s | Rest], false, Mode, CI) ->
    "[" ?JSS_CHARS "]" ++ translate_pat(Rest, false, Mode, CI);
translate_pat([$\\, $S | Rest], false, Mode, CI) ->
    "[^" ?JSS_CHARS "]" ++ translate_pat(Rest, false, Mode, CI);
translate_pat([$\\, $w | Rest], false, Mode, CI) ->
    word_atom(Mode) ++ translate_pat(Rest, false, Mode, CI);
translate_pat([$\\, $W | Rest], false, Mode, CI) ->
    nword_atom(Mode) ++ translate_pat(Rest, false, Mode, CI);
translate_pat([$\\, $s | Rest], true, Mode, CI) ->
    splice_in_class(?JSS_CHARS, Rest, Mode, CI);
translate_pat([$\\, $S | Rest], true, Mode, CI) ->
    splice_in_class(?CS:class_complement(?CS:vspace(), CI), Rest, Mode, CI);
translate_pat([$\\, $w | Rest], true, Mode, CI) ->
    splice_in_class(word_items(Mode), Rest, Mode, CI);
translate_pat([$\\, $W | Rest], true, Mode, CI) ->
    splice_in_class(nword_items(Mode, CI), Rest, Mode, CI);
%% \d/\D need no translation, but they are class escapes too, so route them
%% through splice_in_class for the dash rule below (`[\d-x]` is three atoms in
%% JS; PCRE would reject it as a bad range).
translate_pat([$\\, D | Rest], true, Mode, CI) when D =:= $d; D =:= $D ->
    splice_in_class([$\\, D], Rest, Mode, CI);
%% A `-` immediately BEFORE a class escape is a literal, never a range operator
%% (JS never lets a class escape be a range endpoint). Escape it and let the
%% escape splice normally, or PCRE reads `[!-\w]` as the range `!`-`0` and
%% silently widens the class. Mirror image of the trailing dash that
%% splice_in_class/4 escapes.
translate_pat([$-, $\\, E | Rest], true, Mode, CI)
  when E =:= $d; E =:= $D; E =:= $s; E =:= $S; E =:= $w; E =:= $W ->
    [$\\, $- | translate_pat([$\\, E | Rest], true, Mode, CI)];
%% \b, \B outside a character class (inside one, \b is backspace).
translate_pat([$\\, $b | Rest], false, Mode, CI) ->
    %% Word boundary: word|nonword transition (start/end count as nonword).
    W = word_atom(Mode),
    "(?:(?<=" ++ W ++ ")(?!" ++ W ++ ")|(?<!" ++ W ++ ")(?=" ++ W ++ "))"
        ++ translate_pat(Rest, false, Mode, CI);
translate_pat([$\\, $B | Rest], false, Mode, CI) ->
    %% Non-boundary: both sides word, or both sides nonword/edge.
    W = word_atom(Mode),
    "(?:(?<=" ++ W ++ ")(?=" ++ W ++ ")|(?<!" ++ W ++ ")(?!" ++ W ++ "))"
        ++ translate_pat(Rest, false, Mode, CI);
%% Preserve any other escape pair verbatim (don't reinterpret its 2nd char).
translate_pat([$\\, C | Rest], InClass, Mode, CI) ->
    [$\\, C | translate_pat(Rest, InClass, Mode, CI)];
%% v-flag classes: PCRE has no ClassSetExpression (nested classes, &&
%% intersection, -- subtraction, \q{...} string literals, properties of
%% strings inside classes). Desugar the whole [...] here: parse the set
%% expression, evaluate the set algebra over {codepoint ranges, strings},
%% and emit a plain PCRE class plus an alternation for the strings. A class
%% the desugarer can't handle falls through to the generic translation
%% (degrading to PCRE's interpretation / no-match), as before.
translate_pat([$[ | Rest], false, v, CI) ->
    case vclass(Rest, CI) of
        {ok, Ranges0, Strings, Rest2} ->
            %% With the i flag the algebra ran over scf-folded sets; PCRE's own
            %% caseless closure is unreliable inside large compiled classes
            %% (it folds single chars and small ranges fully, but not long
            %% XCLASS item lists), so close the final set over the scf
            %% equivalence classes ourselves instead of relying on it.
            Ranges = case CI of
                         true -> ?CS:vclose(Ranges0);
                         false -> Ranges0
                     end,
            ?CS:emit_vclass(Ranges, Strings) ++ translate_pat(Rest2, false, v, CI);
        error ->
            [$[ | translate_pat(Rest, true, v, CI)]
    end;
%% Track character-class nesting on unescaped brackets.
translate_pat([$[ | Rest], false, Mode, CI) ->
    [$[ | translate_pat(Rest, true, Mode, CI)];
translate_pat([$] | Rest], true, Mode, CI) ->
    [$] | translate_pat(Rest, false, Mode, CI)];
translate_pat([C | Rest], InClass, Mode, CI) ->
    [C | translate_pat(Rest, InClass, Mode, CI)].

%% Splice a class escape's items into the enclosing [...] and carry on.
%%
%% A `-` right after the splice must NOT be read as a range endpoint. JS never
%% lets a class escape start a range: `[\w-.]` is three atoms under Annex B and
%% a SyntaxError under u/v (which the parser rejects before we get here). PCRE
%% would read the last spliced item and the `-` as a range — either rejecting
%% the pattern (`[\s-.]`: \x{FEFF}-. is out of order) or, worse, silently
%% widening it (`[\w-z]`: _-z quietly admits a backtick). Escape the dash.
splice_in_class(Items, [$-, C | Rest], Mode, CI) when C =/= $] ->
    Items ++ [$\\, $-] ++ translate_pat([C | Rest], true, Mode, CI);
splice_in_class(Items, Rest, Mode, CI) ->
    Items ++ translate_pat(Rest, true, Mode, CI).

%% JS \w / \W as PCRE text, per unicode mode. Under u/v the spelled-out class is
%% what earns the spec's two extra word characters (U+017F, U+212A) from PCRE's
%% caseless folding; without u/v those must NOT fold in, and PCRE's own \w/\W —
%% ASCII-only, never case-folded even under `caseless` — are exactly right.
%% The *_items forms are the same thing as class ITEMS, spliced into an
%% enclosing [...] (which cannot nest, hence the complement for \W).
word_atom(none) -> "\\w";
word_atom(_UOrV) -> ?WORD.

nword_atom(none) -> "\\W";
nword_atom(_UOrV) -> ?NWORD.

word_items(none) -> "\\w";
word_items(_UOrV) -> ?WORD_BODY.

nword_items(none, _CI) -> "\\W";
nword_items(_UOrV, CI) -> ?CS:class_complement(?CS:vword(), CI).

%% Collect a property-escape payload up to the closing }. Only property
%% name/value characters and a single = are expected; anything else means
%% this is not a well-formed property escape and is left untouched.
take_prop([$} | Rest], Acc) -> {lists:reverse(Acc), Rest};
take_prop([C | Rest], Acc)
  when (C >= $a andalso C =< $z); (C >= $A andalso C =< $Z);
       (C >= $0 andalso C =< $9); C =:= $_; C =:= $= ->
    take_prop(Rest, [C | Acc]);
take_prop(_, _Acc) -> none.

prop_translation(Payload, Negated, InClass, Mode) ->
    case binary:split(list_to_binary(Payload), <<"=">>) of
        [Name, Value] ->
            arc_regex_props_ffi:translate_pair(Name, Value, Negated, InClass);
        [Name] ->
            arc_regex_props_ffi:translate_lone(Name, Negated, InClass, Mode =:= v)
    end.

%% ---- v-flag ClassSetExpression desugaring (§22.2.1 ClassSetExpression) ----
%%
%% A v-mode class evaluates to a pair {Ranges, Strings}: a set of codepoints
%% (sorted disjoint ranges) and a set of multi-codepoint strings (lists of
%% codepoints; single-codepoint members are folded into Ranges, the empty
%% string is the [] member). Union, && intersection and -- subtraction apply
%% pointwise to both components. The parser (regex.gleam) has already
%% validated literals, so this evaluator is lenient: anything it can't
%% express returns `error` and the class falls back to the generic path.
%%
%% What lives here is only the PARSER: it walks the class body and hands the
%% operand sets it recognises to ?CS (arc_regex_charset), which owns the set
%% algebra, the case folding (CI, the i flag, changes what every primitive
%% operand set means — see that module) and the emit back to PCRE text.

%% vclass(L, CI): parse the body of a class after `[`, through the closing `]`.
vclass([$^ | Rest], CI) ->
    case vexpr(Rest, CI) of
        {ok, Ranges, [], Rest2} ->
            {ok, ?CS:vcomp(Ranges, CI), [], Rest2};
        %% A negated class may not contain strings (MayContainStrings).
        {ok, _Ranges, [_ | _], _Rest2} -> error;
        error -> error
    end;
vclass(Rest, CI) ->
    vexpr(Rest, CI).

%% First operand decides the expression form: union, && chain, or -- chain.
vexpr([$] | Rest], _CI) ->
    %% `[]` — the empty set, matches nothing.
    {ok, [], [], Rest};
vexpr(L, CI) ->
    case vrange_or_item(L, CI) of
        {ok, R, S, [$&, $& | T]} -> vchain(T, inter, R, S, CI);
        {ok, R, S, [$-, $- | T]} -> vchain(T, subtract, R, S, CI);
        {ok, R, S, Rest} -> vunion(Rest, R, S, CI);
        error -> error
    end.

%% ClassUnion: operands and ranges until the closing ].
vunion([$] | Rest], R, S, _CI) -> {ok, R, S, Rest};
vunion([], _R, _S, _CI) -> error;
vunion(L, R, S, CI) ->
    case vrange_or_item(L, CI) of
        %% Operators may not be mixed into a union (e.g. `[ab--c]`).
        {ok, _R2, _S2, [$&, $& | _]} -> error;
        {ok, _R2, _S2, [$-, $- | _]} -> error;
        {ok, R2, S2, Rest} -> vunion(Rest, R2 ++ R, S2 ++ S, CI);
        error -> error
    end.

%% ClassIntersection / ClassSubtraction: operand (op operand)* ].
vchain(L, Op, R, S, CI) ->
    case vrange_or_item(L, CI) of
        {ok, R2, S2, Rest} ->
            {R3, S3} = vapply(Op, R, S, R2, S2),
            case Rest of
                [$] | Rest2] -> {ok, R3, S3, Rest2};
                [$&, $& | T] when Op =:= inter -> vchain(T, Op, R3, S3, CI);
                [$-, $- | T] when Op =:= subtract -> vchain(T, Op, R3, S3, CI);
                _ -> error
            end;
        error -> error
    end.

vapply(inter, R, S, R2, S2) ->
    {?CS:vinter(?CS:vnorm(R), ?CS:vnorm(R2)),
     ordsets:intersection(ordsets:from_list(S), ordsets:from_list(S2))};
vapply(subtract, R, S, R2, S2) ->
    {?CS:vinter(?CS:vnorm(R), ?CS:vcomplement(?CS:vnorm(R2))),
     ordsets:subtract(ordsets:from_list(S), ordsets:from_list(S2))}.

%% One ClassSetOperand, possibly extended to a ClassSetRange (`a-z`).
%% A trailing `--` is left unconsumed for the caller's operator dispatch.
vrange_or_item(L, CI) ->
    case vitem(L, CI) of
        {char, _Lo, [$-, $- | _]} = Item -> vsingle(Item, CI);
        {char, _Lo, [$-, $] | _]} ->
            %% Dangling `-` is not a ClassSetCharacter in v mode.
            error;
        {char, Lo, [$- | R2]} ->
            case vitem(R2, CI) of
                %% Range validity uses the RAW endpoints; the resulting set
                %% is then folded (MaybeSimpleCaseFolding of CharacterRange).
                {char, Hi, R3} when Lo =< Hi -> {ok, ?CS:vfold([{Lo, Hi}], CI), [], R3};
                {char, _Hi, _R3} -> error;
                {set, _R, _S, _Rest} -> error;
                error -> error
            end;
        {char, _CP, _Rest} = Item -> vsingle(Item, CI);
        {set, R, S, Rest} -> {ok, R, S, Rest};
        error -> error
    end.

vsingle({char, CP, Rest}, CI) -> {ok, ?CS:vfold([{CP, CP}], CI), [], Rest}.

%% One operand: nested class, escape, or literal ClassSetCharacter.
vitem([$[ | Rest], CI) ->
    case vclass(Rest, CI) of
        {ok, R, S, Rest2} -> {set, R, S, Rest2};
        error -> error
    end;
vitem([$\\ | Rest], CI) ->
    vescape(Rest, CI);
vitem([C | _], _CI)
  when C =:= $]; C =:= $(; C =:= $); C =:= ${; C =:= $}; C =:= $/;
       C =:= $-; C =:= $| ->
    error;
vitem([C | Rest], _CI) ->
    {char, C, Rest};
vitem([], _CI) ->
    error.

%% Class escapes (\d, \w, ...), character escapes, \q{...} and \p{...}.
%% Set-producing escapes fold their set here; {char, ...} results are raw
%% (range endpoints must stay raw) and are folded by vsingle/vrange_or_item.
vescape([$d | R], CI) -> {set, ?CS:vfold(?CS:vdigit(), CI), [], R};
vescape([$D | R], CI) -> {set, ?CS:vcomp(?CS:vfold(?CS:vdigit(), CI), CI), [], R};
vescape([$w | R], CI) -> {set, ?CS:vfold(?CS:vword(), CI), [], R};
vescape([$W | R], CI) -> {set, ?CS:vcomp(?CS:vfold(?CS:vword(), CI), CI), [], R};
vescape([$s | R], CI) -> {set, ?CS:vfold(?CS:vspace(), CI), [], R};
vescape([$S | R], CI) -> {set, ?CS:vcomp(?CS:vfold(?CS:vspace(), CI), CI), [], R};
vescape([$b | R], _CI) -> {char, 16#08, R};
vescape([$t | R], _CI) -> {char, $\t, R};
vescape([$n | R], _CI) -> {char, $\n, R};
vescape([$v | R], _CI) -> {char, 16#0B, R};
vescape([$f | R], _CI) -> {char, 16#0C, R};
vescape([$r | R], _CI) -> {char, $\r, R};
vescape([$0, D | _], _CI) when D >= $0, D =< $9 -> error;
vescape([$0 | R], _CI) -> {char, 0, R};
vescape([$c, C | R], _CI)
  when (C >= $a andalso C =< $z); (C >= $A andalso C =< $Z) ->
    {char, C band 31, R};
vescape([$x, A, B | R], _CI) ->
    case is_hex(A) andalso is_hex(B) of
        true -> {char, list_to_integer([A, B], 16), R};
        false -> error
    end;
vescape([$u, ${ | R], _CI) ->
    case take_hex(R, []) of
        {Hex, [$} | R2]} when Hex =/= [] ->
            CP = list_to_integer(Hex, 16),
            case CP =< 16#10FFFF of
                true -> {char, CP, R2};
                false -> error
            end;
        _ -> error
    end;
vescape([$u, A, B, C, D | R], _CI) ->
    case is_hex(A) andalso is_hex(B) andalso is_hex(C) andalso is_hex(D) of
        true ->
            CP = list_to_integer([A, B, C, D], 16),
            case CP >= 16#D800 andalso CP =< 16#DBFF of
                true -> vlead_surrogate(CP, R);
                false -> {char, CP, R}
            end;
        false -> error
    end;
vescape([$q, ${ | R], CI) ->
    vstrings(R, [], [], [], CI);
vescape([P, ${ | R], CI) when P =:= $p; P =:= $P ->
    vprop(P =:= $P, R, CI);
%% Identity escapes: any non-alphanumeric (covers ClassSetSyntaxCharacter
%% and the ClassSetReservedPunctuators).
vescape([C | R], _CI)
  when not ((C >= $0 andalso C =< $9)
            orelse (C >= $a andalso C =< $z)
            orelse (C >= $A andalso C =< $Z)) ->
    {char, C, R};
vescape(_, _CI) ->
    error.

%% A \uHHHH lead surrogate followed by a \uHHHH trail surrogate is one
%% combined codepoint (the pattern is parsed as UTF-16 per the spec).
vlead_surrogate(Lead, [$\\, $u, A, B, C, D | R] = Whole) ->
    case is_hex(A) andalso is_hex(B) andalso is_hex(C) andalso is_hex(D) of
        true ->
            Trail = list_to_integer([A, B, C, D], 16),
            case Trail >= 16#DC00 andalso Trail =< 16#DFFF of
                true ->
                    CP = 16#10000 + (Lead - 16#D800) * 16#400
                        + (Trail - 16#DC00),
                    {char, CP, R};
                false -> {char, Lead, Whole}
            end;
        false -> {char, Lead, Whole}
    end;
vlead_surrogate(Lead, R) ->
    {char, Lead, R}.

%% \q{a|bc|}: alternatives separated by |. Single-codepoint alternatives are
%% characters; everything else (length 0 or 2+) joins the string set.
vstrings(L, CurRev, Rs, Ss, CI) ->
    case L of
        [$} | Rest] ->
            {R2, S2} = vstring_close(lists:reverse(CurRev), Rs, Ss, CI),
            {set, R2, S2, Rest};
        [$| | Rest] ->
            {R2, S2} = vstring_close(lists:reverse(CurRev), Rs, Ss, CI),
            vstrings(Rest, [], R2, S2, CI);
        _ ->
            case vstring_char(L, CI) of
                {char, CP, Rest} -> vstrings(Rest, [CP | CurRev], Rs, Ss, CI);
                error -> error
            end
    end.

vstring_close([CP], Rs, Ss, CI) -> {?CS:vfold([{CP, CP}], CI) ++ Rs, Ss};
vstring_close(Str, Rs, Ss, CI) -> {Rs, [?CS:vfold_str(Str, CI) | Ss]}.

%% One ClassString character — a ClassSetCharacter; no class escapes here.
vstring_char([$\\ | R], CI) ->
    case vescape(R, CI) of
        {char, CP, Rest} -> {char, CP, Rest};
        {set, _R, _S, _Rest} -> error;
        error -> error
    end;
vstring_char([C | R], _CI)
  when C =/= $(, C =/= $), C =/= $[, C =/= $], C =/= ${, C =/= $},
       C =/= $/, C =/= $-, C =/= $\\, C =/= $| ->
    {char, C, R};
vstring_char(_, _CI) ->
    error.

%% \p{...} / \P{...} inside a v-mode class: exact ranges, or (non-negated)
%% a property of strings. \P complements the FOLDED set (so /[\P{Lu}]/iv
%% behaves like /[^\p{Lu}]/iv, per UnicodeSets semantics).
vprop(Negated, L, CI) ->
    case take_prop(L, []) of
        {Payload, Rest} ->
            PayloadBin = list_to_binary(Payload),
            case arc_regex_props_ffi:char_set(PayloadBin) of
                {ok, Ranges} when Negated ->
                    {set, ?CS:vcomp(?CS:vfold(Ranges, CI), CI), [], Rest};
                {ok, Ranges} ->
                    {set, ?CS:vfold(Ranges, CI), [], Rest};
                %% Only a property OF STRINGS has a string list, and only \p
                %% (never \P) may name one — no blind retry on other causes.
                {error, property_of_strings} when not Negated ->
                    vstring_prop(PayloadBin, Rest, CI);
                {error, property_of_strings} -> error;
                {error, unknown_property} -> error;
                {error, no_exact_data} -> error
            end;
        none -> error
    end.

vstring_prop(PayloadBin, Rest, CI) ->
    case arc_regex_props_ffi:string_list(PayloadBin) of
        {ok, Strs} ->
            {R, S} = ?CS:vsplit_singles(Strs, CI),
            {set, R, S, Rest};
        %% RGI_Emoji is stored as a compressed regex the desugarer cannot
        %% decode; the class then falls back to the non-desugared translation.
        {error, no_exact_data} -> error
    end.

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

%% Is a hex-digit string a surrogate codepoint (U+D800..U+DFFF)?
is_surrogate_hex(Hex) ->
    V = list_to_integer(Hex, 16),
    V >= 16#D800 andalso V =< 16#DFFF.

%% A pattern fragment that compiles under PCRE UTF mode but never matches a
%% well-formed string, standing in for an unmatchable lone-surrogate escape.
%% Outside a class: a never-true assertion. Inside a class (where assertions
%% are not allowed): the noncharacter U+FDD0, which arc strings never contain.
surrogate_sentinel(false) -> "(?!)";
surrogate_sentinel(true) -> "\\x{FDD0}".

%% regexp_exec_info(Pattern, Flags, String, Offset, Sticky)
%%   -> {ok, {Captures, GroupCount, Names}} | {error, nil}
%%
%% Offset and the returned Start/Length are BYTE indices into the UTF-8
%% binary — the Gleam caller slices with byte_slice/byte_drop_start and steps
%% empty matches with next_char_boundary, so no grapheme conversion happens
%% on either side. An offset past the end of the string is no-match (re:run
%% raises badarg for it; JS semantics for lastIndex > length are a failed
%% match). A negative Offset is clamped to 0 (ToLength, §7.1.20).
%% Additionally:
%%   - Sticky=true anchors the match at Offset (JS `y` flag semantics),
%%   - Captures is padded with {-1, 0} up to GroupCount + 1 entries (PCRE
%%     omits trailing unset groups; JS exposes them as undefined),
%%   - Names is [{GroupName, CaptureIndex}] for (?<name>...) groups, in
%%     source order, so the caller can build the `groups` object.
regexp_exec_info(Pattern, Flags, String, Offset, Sticky) when Offset < 0 ->
    regexp_exec_info(Pattern, Flags, String, 0, Sticky);
regexp_exec_info(_Pattern, _Flags, String, Offset, _Sticky)
  when Offset > byte_size(String) ->
    {error, nil};
regexp_exec_info(Pattern, Flags, String, Offset, Sticky) ->
    Opts0 = [{offset, Offset}, {capture, all, index}],
    Opts = case Sticky of
               true -> [anchored | Opts0];
               false -> Opts0
           end,
    case get_compiled(Pattern, Flags) of
        %% A pattern PCRE cannot express even after translation degrades to
        %% "no match" rather than crashing the VM.
        {error, {compile_failed, _Reason}} ->
            {error, nil};
        {ok, {MP, GroupCount, Names}} ->
            case run_mp(String, MP, Opts) of
                {match, Captured} ->
                    Padded = pad_captures(Captured, GroupCount + 1),
                    {ok, {Padded, GroupCount, Names}};
                _ -> {error, nil}
            end
    end.

%% Pad the capture list with {-1, 0} (PCRE's "unset group" marker) to N
%% entries — PCRE drops trailing unset groups, JS does not.
pad_captures(Caps, N) when length(Caps) >= N -> Caps;
pad_captures(Caps, N) -> Caps ++ lists:duplicate(N - length(Caps), {-1, 0}).

%% Consume a group name up to its closing `>` (after "(?<" or "\k<").
%% Terminated=false means the pattern ran out before the `>` — the caller
%% needs to know so it can restore the source text byte-for-byte.
take_group_name(L) -> take_group_name(L, []).

take_group_name([$> | Rest], Acc) -> {lists:reverse(Acc), Rest, true};
take_group_name([C | Rest], Acc) -> take_group_name(Rest, [C | Acc]);
take_group_name([], Acc) -> {lists:reverse(Acc), [], false}.
