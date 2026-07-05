%% JS RegExp -> PCRE (`re`) bridge: JS flags to re:compile options, the source
%% pattern scan (group count / named groups), the escape + v-mode class
%% translation, the compiled-pattern cache and exec.
%%
%% Two neighbours own the parts that are not about `re`:
%%   arc_regex_charset — the codepoint set algebra (union/intersection/
%%     complement over ranges), simple case folding, and rendering a set back
%%     to a PCRE class. Pure, no `re` dependency.
%%   arc_bytes_ffi     — byte-offset UTF-8 slicing, shared with the lexer.
%%     re:run returns byte indices, so the Gleam caller slices byte-wise; it
%%     calls arc_bytes_ffi directly (unsafe_slice/3, drop_start/2,
%%     next_char_boundary/2) rather than through forwarders here, so there is
%%     one name and one out-of-range policy for those three functions.
-module(arc_regexp_ffi).
-export([regexp_exec_info/5]).

%% The codepoint-set algebra module, abbreviated: it appears often enough in
%% the v-mode class evaluator that spelling it out would hide the code.
-define(CS, arc_regex_charset).

%% translate_pat/5's InClass parameter is three-valued, not a boolean:
%%   false — outside a character class.
%%   true  — inside `[...]`, and the previous item CANNOT be a range low
%%           endpoint: the class just opened (`[` or `[^`), or the previous item
%%           was a class escape, a property escape, or a range that completed.
%%   atom  — inside `[...]`, and the previous item CAN be a range low endpoint
%%           (a literal character or a single-character escape).
%% The extra bit exists to decide whether a `-` is a range operator or a
%% literal, which JS and PCRE agree on but which this module must know for
%% itself: only a real range operator may clamp a lone-surrogate high endpoint
%% (see translate_range_hi/4). `[-\uD800]` and `[a-b-\uD800]` are literal
%% dashes; `[a-\uD800]` is a range.
-define(IN_CLASS(X), (X =:= true orelse X =:= atom)).

%% Convert JS flags to re:compile options.
%%
%% `i` is the only flag PCRE can be trusted with directly. `m` and `s` are NOT
%% passed through: PCRE's line-terminator set is LF-only (JS's LineTerminator is
%% {LF, CR, U+2028, U+2029}) and PCRE's `$` also matches before a trailing
%% newline, so `multiline`/`dotall` give the wrong answer for /a$/.test("a\n")
%% and /^b/m.test("a\rb"). Instead `.`, `^` and `$` are desugared in
%% translate_pat/5 against ?JS_LT — the same "spell the JS set out rather than
%% trust PCRE's" policy that \s, \w and \b already follow in this module.
flags_to_opts(Flags) ->
    flags_to_opts(Flags, [unicode]).  %% always enable unicode for UTF-8 strings

flags_to_opts(<<>>, Acc) -> Acc;
flags_to_opts(<<"i", Rest/binary>>, Acc) -> flags_to_opts(Rest, [caseless | Acc]);
flags_to_opts(<<_, Rest/binary>>, Acc) -> flags_to_opts(Rest, Acc).
%% g, y, u, d, v are handled at the Gleam level, not PCRE options;
%% m and s are handled by translate_pat/5 — see newline_mode/1.

%% newline_mode(Flags) -> {Multiline, DotAll} — the two flags translate_pat/5
%% needs in order to desugar `^`, `$` and `.`. It is the BOTTOM of that
%% function's modifier stack; a `(?ims-ims:...)` group pushes onto it.
newline_mode(Flags) -> newline_mode(Flags, false, false).

newline_mode(<<>>, M, S) -> {M, S};
newline_mode(<<"m", Rest/binary>>, _M, S) -> newline_mode(Rest, true, S);
newline_mode(<<"s", Rest/binary>>, M, _S) -> newline_mode(Rest, M, true);
newline_mode(<<_, Rest/binary>>, M, S) -> newline_mode(Rest, M, S).

%% get_compiled(Pattern, Flags) -> {ok, {MP, GroupCount, Names}}
%%                               | {error, {pattern_compile_failed, Reason}}
%%
%% The failure shape is one of the `regexp:ExecFailure` constructors the Gleam
%% caller matches on — a pattern PCRE cannot compile is NOT the same thing as
%% a pattern that failed to match, and neither side may confuse them. A compile
%% failure is not necessarily an arc bug: PCRE rejects constructs that are legal
%% ECMAScript, e.g. an unbounded-length lookbehind like `(?<=^\w+)`. It is
%% cached like a success, so a script exec'ing such a regexp in a loop pays the
%% translation + compile once, not once per call.
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
%% plus {CompileOpts, UnicodeMode, NewlineMode} — every compilation and
%% translation input — so flag chars that affect none of them (g/y/d/etc.)
%% share an entry.
get_compiled(Pattern, Flags) ->
    Opts = flags_to_opts(Flags),
    Mode = unicode_mode(Flags),
    NL = newline_mode(Flags),
    Key = {arc_re_mp, Pattern, Opts, Mode, NL},
    case erlang:get(Key) of
        undefined ->
            Caseless = lists:member(caseless, Opts),
            {Stripped, GroupCount, Names} = scan_pattern(Pattern),
            Translated = unicode:characters_to_binary(
                           leading_star_prefix(Stripped, NL)
                           ++ translate_pat(Stripped, false, Mode, Caseless, [NL])),
            Result = case re:compile(Translated, Opts) of
                         {ok, MP} ->
                             {ok, {MP, GroupCount, Names}};
                         {error, Reason} ->
                             {error,
                              {pattern_compile_failed, compile_reason(Reason)}}
                     end,
            cache_put(Key, Result),
            Result;
        Result ->
            Result
    end.

%% re:compile's {ErrString, Position} (or anything else it may hand back)
%% rendered as a binary the Gleam side can carry in
%% PatternCompileFailed(reason: String).
compile_reason({Msg, Pos}) when is_list(Msg), is_integer(Pos) ->
    unicode:characters_to_binary(io_lib:format("~ts at ~b", [Msg, Pos]));
compile_reason(Other) ->
    unicode:characters_to_binary(io_lib:format("~tp", [Other])).

%% Max compiled patterns cached per process before the cache is flushed.
-define(CACHE_MAX, 512).

%% Bound cache size: flush all entries once the cap is hit. Real programs
%% have a small, fixed set of patterns; the cap only triggers for
%% pathological dynamically-generated patterns, where recompiling matches
%% the old behavior anyway.
%% Value cached is get_compiled's whole result — {ok, Entry} or the
%% pattern_compile_failed error — so failures cost one compile too.
cache_put(Key, Result) ->
    N = case erlang:get(arc_re_mp_count) of
            undefined -> 0;
            C -> C
        end,
    case N >= ?CACHE_MAX of
        true ->
            [erlang:erase(K) || {{arc_re_mp, _, _, _, _} = K, _} <- erlang:get()],
            erlang:put(arc_re_mp_count, 1);
        false ->
            erlang:put(arc_re_mp_count, N + 1)
    end,
    erlang:put(Key, Result).

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

%% JS LineTerminator per §12.3: LF, CR, LS (U+2028), PS (U+2029). PCRE's own
%% newline set is LF-only, so `.`, `^` and `$` are desugared against this class
%% instead of being handed to PCRE (see flags_to_opts/1). Class-item text: it
%% is always spliced between `[` and `]`.
-define(JS_LT, "\\n\\r\\x{2028}\\x{2029}").

%% Restore the start-position optimization that desugaring `.` costs us.
%%
%% PCRE recognises a pattern beginning with an unanchored `.*`/`.+` and, instead
%% of retrying at every offset, either anchors it (dotall — `.` reaches the end
%% of the subject from anywhere) or restricts starts to line beginnings (its
%% "startline" flag). Both are exact: if the pattern matches starting at p, the
%% leading star can just eat more, so it also matches starting at the beginning
%% of p's line — hence the leftmost match always starts there. But translate_pat/5
%% rewrites `.` into a class (PCRE's `.` has the wrong newline set for JS) and
%% PCRE only recognises the star of a real `.`, so a failing /.*a$/ went from one
%% scan to a quadratic bump-along. Assert the same thing here, in the pattern.
%%
%% `\G` — "the offset re:run was given" — keeps that very first attempt available:
%% the theorem above only reaches back to the start of the line, which may lie
%% before the offset (a /g loop resuming mid-line must still be able to match).
%% That is exactly the exemption PCRE's own startline scan makes.
leading_star_prefix([$., Star | _], {_Multiline, DotAll})
  when Star =:= $*; Star =:= $+ ->
    case DotAll of
        true -> "\\G";
        false -> "(?:\\G|(?<=[" ?JS_LT "]))"
    end;
leading_star_prefix(_Stripped, _NewlineMode) ->
    "".

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
%% translate_pat(Chars, InClass, Mode, CI, MS)
%%   InClass — false | true | atom, see ?IN_CLASS. Inside a [...] character
%%             class \w/\b are not the same productions (\b is backspace) and a
%%             negated set cannot be written as a nested class; `atom` further
%%             says the previous class item can be a range low endpoint.
%%   Mode    — none | u | v: whether property escapes / v-flag class-set
%%             expressions are in play.
%%   CI      — the i flag. It changes what a class must contain: PCRE folds
%%             every class item at match time, so an emitted set has to be
%%             closed under simple case folding to mean what JS means.
%%   MS      — a stack of {Multiline, DotAll}, head = the state in force. The
%%             m and s flags are not PCRE options here; `.`, `^` and `$` are
%%             rewritten against ?JS_LT, so their scope — which an ES2025
%%             `(?ims-ims:...)` modifier group narrows — is tracked here.
translate_pat([], _InClass, _Mode, _CI, _MS) -> [];
translate_pat([$\\, $u, ${ | Rest], InClass, Mode, CI, MS) ->
    case take_hex(Rest, []) of
        {Hex, [$} | Rest2]} when Hex =/= [] ->
            case is_surrogate_hex(Hex) of
                true ->
                    %% Lone surrogate: PCRE refuses \x{D800}-\x{DFFF} in UTF
                    %% mode, which would degrade the WHOLE pattern to
                    %% no-match. Emit an unmatchable stand-in instead.
                    emit_surrogate(?IN_CLASS(InClass), Rest2, Mode, CI, MS);
                false ->
                    [$\\, $x, ${] ++ Hex ++ [$}]
                        ++ translate_pat(Rest2, after_atom(InClass), Mode, CI, MS)
            end;
        _ ->
            [$\\, $u, ${ | translate_pat(Rest, InClass, Mode, CI, MS)]
    end;
translate_pat([$\\, $u, A, B, C, D | Rest], InClass, Mode, CI, MS) ->
    case is_hex(A) andalso is_hex(B) andalso is_hex(C) andalso is_hex(D) of
        true ->
            V = list_to_integer([A, B, C, D], 16),
            if
                V >= 16#D800, V =< 16#DBFF,
                (InClass =:= false orelse Mode =/= none) ->
                    %% Lead surrogate: a lead+trail escape pair denotes one
                    %% astral character. Outside a class that is how JS
                    %% (non-u) pairs the pattern's code units; INSIDE a class
                    %% it is only true under u/v, where the
                    %% RegExpUnicodeEscapeSequence[+U] LeadSurrogate/
                    %% TrailSurrogate production applies to class atoms too
                    %% (`[😀]` matches U+1F600). Without u/v the two
                    %% escapes really are two lone code units in the class, so
                    %% each falls to the stand-in below.
                    %% An unpaired lead surrogate can never match (arc strings
                    %% are well-formed Unicode) — emit the stand-in so PCRE
                    %% still compiles the rest of the pattern.
                    case pair_trail(Rest) of
                        {ok, W, Rest2} ->
                            CP = 16#10000 + (V - 16#D800) * 16#400
                                + (W - 16#DC00),
                            "\\x{" ++ integer_to_list(CP, 16) ++ "}"
                                ++ translate_pat(Rest2, after_atom(InClass),
                                                 Mode, CI, MS);
                        none ->
                            emit_surrogate(?IN_CLASS(InClass), Rest, Mode, CI, MS)
                    end;
                V >= 16#D800, V =< 16#DFFF ->
                    %% Unpaired trail surrogate, or a lone surrogate inside a
                    %% non-unicode class: never matches a well-formed string.
                    emit_surrogate(?IN_CLASS(InClass), Rest, Mode, CI, MS);
                true ->
                    [$\\, $x, ${, A, B, C, D, $}
                     | translate_pat(Rest, after_atom(InClass), Mode, CI, MS)]
            end;
        false -> [$\\, $u | translate_pat([A, B, C, D | Rest], InClass, Mode, CI, MS)]
    end;
%% \p{...} / \P{...} in unicode mode -> PCRE2 property syntax. An
%% untranslatable payload is left verbatim; PCRE then fails to compile and
%% the regex degrades to no-match (the parser already rejected invalid names
%% in literals, so this only affects RegExp-constructor patterns).
translate_pat([$\\, P, ${ | Rest], InClass, Mode, CI, MS)
  when (P =:= $p orelse P =:= $P), Mode =/= none ->
    case take_prop(Rest, []) of
        {Payload, Rest2} ->
            case prop_translation(Payload, P =:= $P, ?IN_CLASS(InClass), Mode) of
                {ok, Io} ->
                    %% A property escape is a class, never a range low endpoint.
                    unicode:characters_to_list(iolist_to_binary(Io))
                        ++ translate_pat(Rest2, after_class_item(InClass),
                                         Mode, CI, MS);
                error ->
                    [$\\, P, ${ | translate_pat(Rest, InClass, Mode, CI, MS)]
            end;
        none ->
            [$\\, P, ${ | translate_pat(Rest, InClass, Mode, CI, MS)]
    end;
%% \s, \S -> the explicit JS sets: PCRE's own \s is ASCII-only and `ucp` \s both
%% over- and under-shoots. \w, \W -> the explicit sets under u/v only; see
%% word_atom/1. Outside a class the negated forms are their own negated bracket
%% class; inside a class they must become class ITEMS (classes cannot nest), so
%% the negated forms splice their complement — see emit_complement/2.
translate_pat([$\\, $s | Rest], false, Mode, CI, MS) ->
    "[" ?JSS_CHARS "]" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$\\, $S | Rest], false, Mode, CI, MS) ->
    "[^" ?JSS_CHARS "]" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$\\, $w | Rest], false, Mode, CI, MS) ->
    word_atom(Mode) ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$\\, $W | Rest], false, Mode, CI, MS) ->
    nword_atom(Mode) ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$\\, $s | Rest], IC, Mode, CI, MS) when ?IN_CLASS(IC) ->
    splice_in_class(?JSS_CHARS, Rest, Mode, CI, MS);
translate_pat([$\\, $S | Rest], IC, Mode, CI, MS) when ?IN_CLASS(IC) ->
    splice_in_class(?CS:emit_complement(?CS:vspace(), CI), Rest, Mode, CI, MS);
translate_pat([$\\, $w | Rest], IC, Mode, CI, MS) when ?IN_CLASS(IC) ->
    splice_in_class(word_items(Mode), Rest, Mode, CI, MS);
translate_pat([$\\, $W | Rest], IC, Mode, CI, MS) when ?IN_CLASS(IC) ->
    splice_in_class(nword_items(Mode, CI), Rest, Mode, CI, MS);
%% \d/\D need no translation, but they are class escapes too, so route them
%% through splice_in_class for the dash rule below (`[\d-x]` is three atoms in
%% JS; PCRE would reject it as a bad range).
translate_pat([$\\, D | Rest], IC, Mode, CI, MS)
  when ?IN_CLASS(IC), D =:= $d orelse D =:= $D ->
    splice_in_class([$\\, D], Rest, Mode, CI, MS);
%% A `-` immediately BEFORE a class escape is a literal, never a range operator
%% (JS never lets a class escape be a range endpoint). Escape it and let the
%% escape splice normally, or PCRE reads `[!-\w]` as the range `!`-`0` and
%% silently widens the class. Mirror image of the trailing dash that
%% splice_in_class/4 escapes.
translate_pat([$-, $\\, E | Rest], IC, Mode, CI, MS)
  when ?IN_CLASS(IC),
       E =:= $d orelse E =:= $D orelse E =:= $s orelse E =:= $S
       orelse E =:= $w orelse E =:= $W ->
    [$\\, $- | translate_pat([$\\, E | Rest], true, Mode, CI, MS)];
%% A `-` in a class is a range operator only when the previous item can be a
%% range LOW endpoint (state `atom`) and something other than the closing `]`
%% follows. `[-\uD800]`, `[^-\uD800]` and `[a-b-\uD800]` are literal dashes;
%% reading them as operators would substitute a real character (U+D7FF) for an
%% unmatchable one and silently widen the class — the same hazard the clause
%% above exists to prevent, from the other side.
translate_pat([$-, C | _] = L, atom, Mode, CI, MS) when C =/= $] ->
    translate_range_hi(tl(L), Mode, CI, MS);
%% A literal `-`. Escape it so PCRE cannot read it as an operator either (the
%% item after it may be a class, e.g. `[a-b-\uD800]` -> `[a-b\-\p{Cs}]`), and
%% remember that a literal dash IS itself a range low endpoint (`[--a]`).
translate_pat([$- | Rest], IC, Mode, CI, MS) when ?IN_CLASS(IC) ->
    [$\\, $- | translate_pat(Rest, atom, Mode, CI, MS)];
%% \b, \B outside a character class (inside one, \b is backspace).
translate_pat([$\\, $b | Rest], false, Mode, CI, MS) ->
    %% Word boundary: word|nonword transition (start/end count as nonword).
    W = word_atom(Mode),
    "(?:(?<=" ++ W ++ ")(?!" ++ W ++ ")|(?<!" ++ W ++ ")(?=" ++ W ++ "))"
        ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$\\, $B | Rest], false, Mode, CI, MS) ->
    %% Non-boundary: both sides word, or both sides nonword/edge.
    W = word_atom(Mode),
    "(?:(?<=" ++ W ++ ")(?=" ++ W ++ ")|(?<!" ++ W ++ ")(?!" ++ W ++ "))"
        ++ translate_pat(Rest, false, Mode, CI, MS);
%% Preserve any other escape pair verbatim (don't reinterpret its 2nd char).
%% Every escape that reaches here denotes a single character (the class escapes
%% were taken above), so in a class it can start a range.
translate_pat([$\\, C | Rest], InClass, Mode, CI, MS) ->
    [$\\, C | translate_pat(Rest, after_atom(InClass), Mode, CI, MS)];
%% v-flag classes: PCRE has no ClassSetExpression (nested classes, &&
%% intersection, -- subtraction, \q{...} string literals, properties of
%% strings inside classes). Desugar the whole [...] here: parse the set
%% expression, evaluate the set algebra over {codepoint ranges, strings},
%% and emit a plain PCRE class plus an alternation for the strings. A class
%% the desugarer can't handle falls through to the generic translation
%% (degrading to PCRE's interpretation / no-match), as before.
translate_pat([$[ | Rest], false, v, CI, MS) ->
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
            ?CS:emit_vclass(Ranges, Strings) ++ translate_pat(Rest2, false, v, CI, MS);
        error ->
            open_class(Rest, v, CI, MS)
    end;
%% Track character-class nesting on unescaped brackets.
translate_pat([$[ | Rest], false, Mode, CI, MS) ->
    open_class(Rest, Mode, CI, MS);
translate_pat([$] | Rest], IC, Mode, CI, MS) when ?IN_CLASS(IC) ->
    [$] | translate_pat(Rest, false, Mode, CI, MS)];
%% Modifier groups `(?ims-ims:...)` (ES2025 RegExp modifiers) change m/s for
%% their body only. `.`, `^` and `$` are desugared here rather than left to
%% PCRE, so their m/s scope has to be tracked here too — hence MS is a stack
%% whose head is the modifier state in force. Every other `(` re-pushes it, and
%% `)` pops. (The `i` letter is left to PCRE, which sees the group verbatim.)
translate_pat([$( | Rest], false, Mode, CI, [Cur | _] = MS) ->
    case take_modifiers(Rest, Cur) of
        {ok, Src, Cur2, Rest2} ->
            [$( | Src] ++ translate_pat(Rest2, false, Mode, CI, [Cur2 | MS]);
        none ->
            [$( | translate_pat(Rest, false, Mode, CI, [Cur | MS])]
    end;
translate_pat([$) | Rest], false, Mode, CI, MS) ->
    [$) | translate_pat(Rest, false, Mode, CI, pop_ms(MS))];
%% `.` (§22.2.2.7): every character except a JS LineTerminator, or every
%% character at all under the s flag. PCRE's `.` excludes LF only, and its
%% `dotall` would still leave the no-s case wrong, so both cases are spelled out.
translate_pat([$. | Rest], false, Mode, CI, [{_M, false} | _] = MS) ->
    "[^" ?JS_LT "]" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$. | Rest], false, Mode, CI, [{_M, true} | _] = MS) ->
    "(?s:.)" ++ translate_pat(Rest, false, Mode, CI, MS);
%% `^` (§22.2.2.6): start of input, plus (m flag) any position after a JS
%% LineTerminator. PCRE's `^` only knows LF, so use \A + an explicit lookbehind.
translate_pat([$^ | Rest], false, Mode, CI, [{false, _S} | _] = MS) ->
    "\\A" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$^ | Rest], false, Mode, CI, [{true, _S} | _] = MS) ->
    "(?:\\A|(?<=[" ?JS_LT "]))" ++ translate_pat(Rest, false, Mode, CI, MS);
%% `$` (§22.2.2.6): end of input, plus (m flag) any position before a JS
%% LineTerminator. PCRE's `$` also matches before a trailing newline even
%% without `multiline`, which JS never does — hence \z, not `$`.
translate_pat([$$ | Rest], false, Mode, CI, [{false, _S} | _] = MS) ->
    "\\z" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$$ | Rest], false, Mode, CI, [{true, _S} | _] = MS) ->
    "(?=[" ?JS_LT "]|\\z)" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([C | Rest], InClass, Mode, CI, MS) ->
    [C | translate_pat(Rest, after_atom(InClass), Mode, CI, MS)].

%% Enter a `[...]` class. A leading `^` is the negation, not a class item, so it
%% must not become a range low endpoint — `[^-\uD800]`'s dash is a literal, the
%% same as `[-\uD800]`'s.
open_class(Rest, Mode, CI, MS) ->
    {Open, Body} = case Rest of
                       [$^ | R] -> {"[^", R};
                       _ -> {"[", Rest}
                   end,
    Open ++ translate_pat(Body, true, Mode, CI, MS).

%% State after emitting a class item that CAN be a range low endpoint (a literal
%% character, or an escape denoting one).
after_atom(false) -> false;
after_atom(_InClass) -> atom.

%% State after emitting a class item that CANNOT be a range low endpoint (a
%% class escape, a property escape, a completed range).
after_class_item(false) -> false;
after_class_item(_InClass) -> true.

%% The HIGH endpoint of a class range, consumed as exactly one item so the state
%% resets: whatever follows a completed range starts fresh, and a `-` there is a
%% literal rather than a second range operator (`[a-b-\uD800]`).
%%
%% Only `\u` needs care. A lone surrogate cannot be a PCRE range endpoint — its
%% stand-in \p{Cs} is a class, not a codepoint — and no well-formed subject holds
%% one, so JS's Lo..surrogate range is exactly Lo..U+D7FF: clamp. Only the
%% \uHHHH form pairs with a following trail escape into one astral character
%% (the \u{...} form never does), and only under u/v — `[a-😀]/u` is
%% the range a..U+1F600, but `[a-\u{D83D}\uDE00]/u` is a clamped range plus a
%% lone trail surrogate.
translate_range_hi([$\\, $u | R0] = L, Mode, CI, MS) ->
    Braced = case R0 of [${ | _] -> true; _ -> false end,
    case parse_uescape(R0) of
        {ok, V, R1} when V < 16#D800; V > 16#DFFF ->
            "-\\x{" ++ integer_to_list(V, 16) ++ "}"
                ++ translate_pat(R1, true, Mode, CI, MS);
        {ok, V, R1} ->
            case (not Braced) andalso Mode =/= none andalso V =< 16#DBFF
                andalso pair_trail(R1) of
                {ok, W, R2} ->
                    CP = 16#10000 + (V - 16#D800) * 16#400 + (W - 16#DC00),
                    "-\\x{" ++ integer_to_list(CP, 16) ++ "}"
                        ++ translate_pat(R2, true, Mode, CI, MS);
                _ ->
                    "-\\x{D7FF}" ++ translate_pat(R1, true, Mode, CI, MS)
            end;
        none ->
            range_hi_verbatim(L, Mode, CI, MS)
    end;
translate_range_hi(L, Mode, CI, MS) ->
    range_hi_verbatim(L, Mode, CI, MS).

%% Any other high endpoint is emitted as the generic translation would emit it;
%% all this adds is knowing where the item ends.
range_hi_verbatim([$\\, $x, A, B | R] = L, Mode, CI, MS) ->
    case is_hex(A) andalso is_hex(B) of
        true -> [$-, $\\, $x, A, B | translate_pat(R, true, Mode, CI, MS)];
        false -> range_hi_escape(L, Mode, CI, MS)
    end;
range_hi_verbatim([$\\, $c, C | R] = L, Mode, CI, MS) ->
    case (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) of
        true -> [$-, $\\, $c, C | translate_pat(R, true, Mode, CI, MS)];
        false -> range_hi_escape(L, Mode, CI, MS)
    end;
range_hi_verbatim(L, Mode, CI, MS) ->
    range_hi_escape(L, Mode, CI, MS).

range_hi_escape([$\\, C | R], Mode, CI, MS) ->
    [$-, $\\, C | translate_pat(R, true, Mode, CI, MS)];
range_hi_escape([C | R], Mode, CI, MS) ->
    [$-, C | translate_pat(R, true, Mode, CI, MS)].

%% Read a modifier-group prefix — "?[ims][-[ims]]:" — from just after the `(`,
%% and fold it into the enclosing {Multiline, DotAll}. Either RegularExpressionFlags
%% may be empty, so `(?m-:...)` and the plain non-capturing `(?:...)` are the same
%% production and land here too, the latter simply re-pushing Cur. Anything else
%% (a lookaround, a plain group) is `none`. The prefix's source text is returned so
%% it can be re-emitted verbatim — PCRE still needs to see the `i` letter, which we
%% do not desugar.
take_modifiers([$? | Rest], Cur) ->
    {Add, R1} = take_ims(Rest, []),
    case R1 of
        [$: | R2] ->
            {ok, [$? | Add] ++ ":", apply_ims(Cur, Add, []), R2};
        [$- | R1b] ->
            case take_ims(R1b, []) of
                {Rem, [$: | R2]} ->
                    {ok, [$? | Add] ++ [$- | Rem] ++ ":",
                     apply_ims(Cur, Add, Rem), R2};
                _ -> none
            end;
        _ -> none
    end;
take_modifiers(_Rest, _Cur) -> none.

take_ims([C | Rest], Acc) when C =:= $i; C =:= $m; C =:= $s ->
    take_ims(Rest, [C | Acc]);
take_ims(Rest, Acc) -> {lists:reverse(Acc), Rest}.

apply_ims({M, S}, Add, Rem) ->
    {ims_bit($m, Add, Rem, M), ims_bit($s, Add, Rem, S)}.

ims_bit(C, Add, Rem, Cur) ->
    case {lists:member(C, Add), lists:member(C, Rem)} of
        {true, _} -> true;
        {_, true} -> false;
        _ -> Cur
    end.

%% Leave the pattern's own {Multiline, DotAll} at the bottom of the stack: an
%% unbalanced `)` is a pattern PCRE will reject anyway, and popping past the
%% bottom here would crash the translation instead.
pop_ms([_Inner, Outer | Rest]) -> [Outer | Rest];
pop_ms([Bottom]) -> [Bottom].

%% Splice a class escape's items into the enclosing [...] and carry on.
%%
%% A `-` right after the splice must NOT be read as a range endpoint. JS never
%% lets a class escape start a range: `[\w-.]` is three atoms under Annex B and
%% a SyntaxError under u/v (which the parser rejects before we get here). PCRE
%% would read the last spliced item and the `-` as a range — either rejecting
%% the pattern (`[\s-.]`: \x{FEFF}-. is out of order) or, worse, silently
%% widening it (`[\w-z]`: _-z quietly admits a backtick). Escape the dash.
splice_in_class(Items, [$-, C | Rest], Mode, CI, MS) when C =/= $] ->
    Items ++ [$\\, $-] ++ translate_pat([C | Rest], true, Mode, CI, MS);
splice_in_class(Items, Rest, Mode, CI, MS) ->
    Items ++ translate_pat(Rest, true, Mode, CI, MS).

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
nword_items(_UOrV, CI) -> ?CS:emit_complement(?CS:vword(), CI).

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
            {ok, ?CS:character_complement(Ranges, CI), [], Rest2};
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
    {?CS:vsubtract(R, R2),
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
vescape([$D | R], CI) -> {set, ?CS:character_complement(?CS:vdigit(), CI), [], R};
vescape([$w | R], CI) -> {set, ?CS:vfold(?CS:vword(), CI), [], R};
vescape([$W | R], CI) -> {set, ?CS:character_complement(?CS:vword(), CI), [], R};
vescape([$s | R], CI) -> {set, ?CS:vfold(?CS:vspace(), CI), [], R};
vescape([$S | R], CI) -> {set, ?CS:character_complement(?CS:vspace(), CI), [], R};
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
                    {set, ?CS:character_complement(Ranges, CI), [], Rest};
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

%% Emit the stand-in for a lone-surrogate escape PCRE cannot express, then keep
%% translating. Rest is the source AFTER the escape.
%%
%% Outside a class the stand-in is a never-true assertion. Inside a class it has
%% to be a class ITEM that no valid UTF-8 subject can ever match: \p{Cs} — the
%% surrogate general category, empty by construction over well-formed strings.
%% (It cannot be a codepoint: every scalar value, noncharacters included, is a
%% legal member of an arc string, so no single codepoint is unmatchable.)
emit_surrogate(false, Rest, Mode, CI, MS) ->
    "(?!)" ++ translate_pat(Rest, false, Mode, CI, MS);
emit_surrogate(true, Rest, Mode, CI, MS) ->
    {Item, Rest2} = class_surrogate_item(Rest),
    Item ++ translate_pat(Rest2, true, Mode, CI, MS).

%% \p{Cs} is a class, not a codepoint, so PCRE rejects it as a range endpoint.
%% A range STARTING at a lone surrogate is therefore consumed here, its high
%% endpoint clamped out of the surrogate block:
%%   [\uD800-\uDFFF]  -> matches nothing        -> \p{Cs}
%%   [\uD800-\uFFFF]  -> matches U+E000..U+FFFF -> \x{E000}-\x{FFFF}
%% A high endpoint we cannot read (only reachable from a pattern whose range is
%% already out of order, i.e. a JS SyntaxError) keeps its dash as a literal so
%% the pattern still compiles.
class_surrogate_item([$-, C | _] = Rest) when C =/= $] ->
    case class_range_hi(tl(Rest)) of
        {ok, Hi, Rest2} when Hi > 16#DFFF ->
            {"\\x{E000}-\\x{" ++ integer_to_list(Hi, 16) ++ "}", Rest2};
        {ok, _Hi, Rest2} ->
            {"\\p{Cs}", Rest2};
        none ->
            {"\\p{Cs}\\-", tl(Rest)}
    end;
class_surrogate_item(Rest) ->
    {"\\p{Cs}", Rest}.

%% The high endpoint of a class range starting at a lone surrogate: only the
%% forms that can legally sit above U+D7FF are worth reading.
class_range_hi([$\\, $u | R0]) -> parse_uescape(R0);
class_range_hi([C | R]) when C > 16#DFFF -> {ok, C, R};
class_range_hi(_) -> none.

%% Read the body of a `\u` escape (the source AFTER the `\u`) as a codepoint:
%% either \u{H..} or \uHHHH. Returns none if it is neither.
parse_uescape([${ | R]) ->
    case take_hex(R, []) of
        {Hex, [$} | R2]} when Hex =/= [] -> {ok, list_to_integer(Hex, 16), R2};
        _ -> none
    end;
parse_uescape([A, B, C, D | R]) ->
    case is_hex(A) andalso is_hex(B) andalso is_hex(C) andalso is_hex(D) of
        true -> {ok, list_to_integer([A, B, C, D], 16), R};
        false -> none
    end;
parse_uescape(_) -> none.

%% An immediately following `\uHHHH` trail-surrogate escape, to be paired with a
%% lead surrogate already read.
pair_trail([$\\, $u, E, F, G, H | Rest]) ->
    case is_hex(E) andalso is_hex(F) andalso is_hex(G) andalso is_hex(H) of
        true ->
            case list_to_integer([E, F, G, H], 16) of
                W when W >= 16#DC00, W =< 16#DFFF -> {ok, W, Rest};
                _ -> none
            end;
        false -> none
    end;
pair_trail(_) -> none.

%% regexp_exec_info(Pattern, Flags, String, Offset, Sticky)
%%   -> {ok, {WholeMatch, Groups, GroupCount, Names}}
%%    | {error, no_match}
%%    | {error, offset_out_of_range}
%%    | {error, {pattern_compile_failed, ReasonBinary}}
%%
%% The three failures are DISTINCT and the caller (regexp.gleam's ExecFailure)
%% must treat them so: only no_match/offset_out_of_range are "the regex did not
%% match"; a pattern PCRE cannot compile is an engine-level failure.
%%
%% Offset and the returned Start/Length are BYTE indices into the UTF-8
%% binary — the Gleam caller slices with byte_slice/byte_drop_start and steps
%% empty matches with next_char_boundary, so no grapheme conversion happens
%% on either side. re:run/3 raises badarg for the two offsets JS treats as a
%% failed match — past the end of the string, or mid-UTF-8-character — so both
%% are rejected by the guards below BEFORE re:run sees them, and no badarg is
%% caught anywhere in this module: any other badarg is a bug here and must
%% crash rather than masquerade as "the regex didn't match".
%% A negative Offset is clamped to 0 (ToLength, §7.1.20).
%% Additionally:
%%   - Sticky=true anchors the match at Offset (JS `y` flag semantics),
%%   - WholeMatch is the {Start, Length} of capture 0 — split out so the Gleam
%%     caller's type never admits an empty capture list (a PCRE match always
%%     has one),
%%   - Groups is captures 1..N padded with {-1, 0} up to GroupCount entries
%%     (PCRE omits trailing unset groups; JS exposes them as undefined),
%%   - Names is [{GroupName, CaptureIndex}] for (?<name>...) groups, in
%%     source order, so the caller can build the `groups` object.
regexp_exec_info(Pattern, Flags, String, Offset, Sticky) when Offset < 0 ->
    regexp_exec_info(Pattern, Flags, String, 0, Sticky);
regexp_exec_info(_Pattern, _Flags, String, Offset, _Sticky)
  when Offset > byte_size(String) ->
    {error, offset_out_of_range};
%% An offset landing on a UTF-8 continuation byte is mid-character; re:run
%% would raise badarg. Same test as arc_bytes_ffi:next_char_boundary's.
regexp_exec_info(Pattern, Flags, String, Offset, Sticky)
  when Offset < byte_size(String) ->
    case (binary:at(String, Offset) band 16#C0) =:= 16#80 of
        true -> {error, no_match};
        false -> exec_compiled(Pattern, Flags, String, Offset, Sticky)
    end;
regexp_exec_info(Pattern, Flags, String, Offset, Sticky) ->
    exec_compiled(Pattern, Flags, String, Offset, Sticky).

exec_compiled(Pattern, Flags, String, Offset, Sticky) ->
    Opts0 = [{offset, Offset}, {capture, all, index}],
    Opts = case Sticky of
               true -> [anchored | Opts0];
               false -> Opts0
           end,
    case get_compiled(Pattern, Flags) of
        {error, {pattern_compile_failed, _Reason}} = Err ->
            Err;
        {ok, {MP, GroupCount, Names}} ->
            case re:run(String, MP, Opts) of
                {match, [Whole | Groups]} ->
                    Padded = pad_captures(Groups, GroupCount),
                    {ok, {Whole, Padded, GroupCount, Names}};
                nomatch -> {error, no_match}
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
