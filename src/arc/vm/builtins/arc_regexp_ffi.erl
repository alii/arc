-module(arc_regexp_ffi).
-export([regexp_exec/4, regexp_exec_info/5, regexp_test/3, canonical_flags/1,
         byte_slice/3, byte_drop_start/2, next_char_boundary/2]).

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
%% Translation happens behind the same cache: property escapes can expand
%% into multi-kilobyte classes, so re-translating per exec would dominate
%% tight test()/exec loops over short subjects. Keyed by the ORIGINAL
%% pattern plus {CompileOpts, UnicodeMode} — both compilation and
%% translation inputs — so flag chars that affect neither (g/y/d/etc.)
%% share an entry.
get_compiled(Pattern, Flags) ->
    Opts = flags_to_opts(Flags),
    Mode = unicode_mode(Flags),
    Key = {arc_re_mp, Pattern, Opts, Mode},
    case erlang:get(Key) of
        undefined ->
            Caseless = lists:member(caseless, Opts),
            case re:compile(translate_pattern(Pattern, Mode, Caseless), Opts) of
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
            [erlang:erase(K) || {{arc_re_mp, _, _, _} = K, _} <- erlang:get()],
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
        MP = get_compiled(Pattern, Flags),
        re:run(String, MP, RunOpts)
    catch _:_ -> nomatch
    end.

%% Translate the JS-regex escapes PCRE doesn't accept into their PCRE form.
%% Currently: \uHHHH and \u{H..} -> \x{H..}, and (with the u or v flag)
%% \p{...}/\P{...} property escapes -> the PCRE2 property syntax (long
%% General_Category names -> short codes, Script=X -> sc:X,
%% Script_Extensions=X -> scx:X, Assigned -> negated Cn). Backslash escapes
%% are consumed in pairs so an escaped backslash (\\) before a `u` is not
%% misread as a unicode escape. The original (untranslated) source is what
%% RegExp.prototype.source returns; this only affects what is handed to re.
%% The caseless flag matters in v mode: the desugared class-set algebra must
%% run over case-folded operands (mode `vi`); see MaybeSimpleCaseFolding.
translate_pattern(Pattern, Mode, Caseless) ->
    TransMode = case Mode of
                    v when Caseless -> vi;
                    _ -> Mode
                end,
    {_, Names} = group_info(Pattern),
    Stripped = strip_named(unicode:characters_to_list(Pattern), Names, false),
    unicode:characters_to_binary(translate_pat(Stripped, false, TransMode)).

%% Strip named-group syntax before handing the pattern to PCRE: JS group
%% names (any identifier, including $ and astral chars, and ES2025 duplicate
%% names across alternatives) are a superset of what PCRE accepts, so
%% "(?<name>" becomes a plain "(" and "\k<name>" becomes the numeric "\g{N}"
%% backreference. Names are resolved on the Gleam side via group_info/1.
%% With no named groups in the pattern, \k<...> is left alone (Annex B
%% identity-escape semantics).
strip_named([], _Names, _InClass) -> [];
strip_named([$\\, $k, $< | Rest], Names, false) when Names =/= [] ->
    case take_group_name(Rest, []) of
        {Name, Rest2} ->
            case lists:keyfind(unicode:characters_to_binary(Name), 1, Names) of
                {_, Idx} ->
                    "\\g{" ++ integer_to_list(Idx) ++ "}"
                        ++ strip_named(Rest2, Names, false);
                false ->
                    [$\\, $k, $< | strip_named(Rest, Names, false)]
            end
    end;
strip_named([$\\, C | Rest], Names, InClass) ->
    [$\\, C | strip_named(Rest, Names, InClass)];
strip_named([$[ | Rest], Names, false) ->
    [$[ | strip_named(Rest, Names, true)];
strip_named([$] | Rest], Names, true) ->
    [$] | strip_named(Rest, Names, false)];
strip_named([$(, $?, $<, C | Rest], Names, false) when C =/= $=, C =/= $! ->
    {_Name, Rest2} = take_group_name([C | Rest], []),
    [$( | strip_named(Rest2, Names, false)];
strip_named([C | Rest], Names, InClass) ->
    [C | strip_named(Rest, Names, InClass)].

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

%% Word-character class matching JS \w: [0-9A-Za-z_]. Used both as a class and,
%% under PCRE caseless, to get JS's case-fold closure for free — caseless
%% folding maps U+017F (long s) to s and U+212A (Kelvin) to k, so `(?i:[a-z])`
%% matches them. PCRE's own \w doesn't do this, and `ucp` over-broadens \s/\b.
-define(WORD, "[0-9A-Za-z_]").
-define(NWORD, "[^0-9A-Za-z_]").

%% JS \s per §22.2.2.9: WhiteSpace + LineTerminator productions — ASCII
%% whitespace plus NBSP, Ogham space, the U+2000 block, LS/PS, NNBSP, MMSP,
%% ideographic space, and BOM. PCRE's \s is ASCII-only, and `ucp` \s both
%% over- and under-shoots (includes U+0085 NEL, excludes U+FEFF BOM).
-define(JSS_CHARS,
        "\\t\\n\\x0B\\f\\r \\x{A0}\\x{1680}\\x{2000}-\\x{200A}"
        "\\x{2028}\\x{2029}\\x{202F}\\x{205F}\\x{3000}\\x{FEFF}").

%% Second argument tracks whether we are inside a [...] character class, where
%% \w/\b are not the same productions (\b is backspace) and must be left alone.
%% Third argument is whether the u or v flag is set (property escape mode).
translate_pat([], _InClass, _Prop) -> [];
translate_pat([$\\, $u, ${ | Rest], InClass, Prop) ->
    case take_hex(Rest, []) of
        {Hex, [$} | Rest2]} when Hex =/= [] ->
            case is_surrogate_hex(Hex) of
                true ->
                    %% Lone surrogate: PCRE refuses \x{D800}-\x{DFFF} in UTF
                    %% mode, which would degrade the WHOLE pattern to
                    %% no-match. Neutralize to a never-matching sentinel.
                    surrogate_sentinel(InClass)
                        ++ translate_pat(Rest2, InClass, Prop);
                false ->
                    [$\\, $x, ${] ++ Hex ++ [$}] ++ translate_pat(Rest2, InClass, Prop)
            end;
        _ ->
            [$\\, $u, ${ | translate_pat(Rest, InClass, Prop)]
    end;
translate_pat([$\\, $u, A, B, C, D | Rest], InClass, Prop) ->
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
                                                ++ translate_pat(Rest2, InClass, Prop);
                                        true ->
                                            surrogate_sentinel(InClass)
                                                ++ translate_pat(Rest, InClass, Prop)
                                    end;
                                false ->
                                    surrogate_sentinel(InClass)
                                        ++ translate_pat(Rest, InClass, Prop)
                            end;
                        _ ->
                            surrogate_sentinel(InClass)
                                ++ translate_pat(Rest, InClass, Prop)
                    end;
                V >= 16#D800, V =< 16#DFFF ->
                    %% Unpaired trail surrogate, or any surrogate inside a
                    %% character class: never matches a well-formed string —
                    %% the sentinel keeps the pattern compiling.
                    surrogate_sentinel(InClass)
                        ++ translate_pat(Rest, InClass, Prop);
                true ->
                    [$\\, $x, ${, A, B, C, D, $} | translate_pat(Rest, InClass, Prop)]
            end;
        false -> [$\\, $u | translate_pat([A, B, C, D | Rest], InClass, Prop)]
    end;
%% \p{...} / \P{...} in unicode mode -> PCRE2 property syntax. An
%% untranslatable payload is left verbatim; PCRE then fails to compile and
%% the regex degrades to no-match (the parser already rejected invalid names
%% in literals, so this only affects RegExp-constructor patterns).
translate_pat([$\\, P, ${ | Rest], InClass, Mode)
  when (P =:= $p orelse P =:= $P), Mode =/= none ->
    case take_prop(Rest, []) of
        {Payload, Rest2} ->
            case prop_translation(Payload, P =:= $P, InClass, Mode) of
                {ok, Io} ->
                    unicode:characters_to_list(iolist_to_binary(Io))
                        ++ translate_pat(Rest2, InClass, Mode);
                error ->
                    [$\\, P, ${ | translate_pat(Rest, InClass, Mode)]
            end;
        none ->
            [$\\, P, ${ | translate_pat(Rest, InClass, Mode)]
    end;
%% \s, \S -> explicit JS whitespace class (see ?JSS_CHARS). Inside a
%% character class the set's contents are spliced in directly; a negated \S
%% cannot be spliced into a class, so it keeps PCRE semantics there.
translate_pat([$\\, $s | Rest], false, Prop) ->
    "[" ?JSS_CHARS "]" ++ translate_pat(Rest, false, Prop);
translate_pat([$\\, $S | Rest], false, Prop) ->
    "[^" ?JSS_CHARS "]" ++ translate_pat(Rest, false, Prop);
translate_pat([$\\, $s | Rest], true, Prop) ->
    ?JSS_CHARS ++ translate_pat(Rest, true, Prop);
%% \w, \W, \b, \B outside a character class -> explicit JS forms so caseless
%% folding includes long-s / Kelvin (matching the JS word-char case closure).
translate_pat([$\\, $w | Rest], false, Prop) -> ?WORD ++ translate_pat(Rest, false, Prop);
translate_pat([$\\, $W | Rest], false, Prop) -> ?NWORD ++ translate_pat(Rest, false, Prop);
translate_pat([$\\, $b | Rest], false, Prop) ->
    %% Word boundary: word|nonword transition (start/end count as nonword).
    "(?:(?<=" ?WORD ")(?!" ?WORD ")|(?<!" ?WORD ")(?=" ?WORD "))"
        ++ translate_pat(Rest, false, Prop);
translate_pat([$\\, $B | Rest], false, Prop) ->
    %% Non-boundary: both sides word, or both sides nonword/edge.
    "(?:(?<=" ?WORD ")(?=" ?WORD ")|(?<!" ?WORD ")(?!" ?WORD "))"
        ++ translate_pat(Rest, false, Prop);
%% Preserve any other escape pair verbatim (don't reinterpret its 2nd char).
translate_pat([$\\, C | Rest], InClass, Prop) -> [$\\, C | translate_pat(Rest, InClass, Prop)];
%% v-flag classes: PCRE has no ClassSetExpression (nested classes, &&
%% intersection, -- subtraction, \q{...} string literals, properties of
%% strings inside classes). Desugar the whole [...] here: parse the set
%% expression, evaluate the set algebra over {codepoint ranges, strings},
%% and emit a plain PCRE class plus an alternation for the strings. A class
%% the desugarer can't handle falls through to the generic translation
%% (degrading to PCRE's interpretation / no-match), as before.
translate_pat([$[ | Rest], false, Mode) when Mode =:= v; Mode =:= vi ->
    case vclass(Rest, Mode =:= vi) of
        {ok, Ranges0, Strings, Rest2} ->
            %% In vi mode the algebra ran over scf-folded sets; PCRE's own
            %% caseless closure is unreliable inside large compiled classes
            %% (it folds single chars and small ranges fully, but not long
            %% XCLASS item lists), so close the final set over the scf
            %% equivalence classes ourselves instead of relying on it.
            Ranges = case Mode of
                         vi -> vclose(Ranges0);
                         v -> Ranges0
                     end,
            emit_vclass(Ranges, Strings) ++ translate_pat(Rest2, false, Mode);
        error ->
            [$[ | translate_pat(Rest, true, Mode)]
    end;
%% Track character-class nesting on unescaped brackets.
translate_pat([$[ | Rest], false, Prop) -> [$[ | translate_pat(Rest, true, Prop)];
translate_pat([$] | Rest], true, Prop) -> [$] | translate_pat(Rest, false, Prop)];
translate_pat([C | Rest], InClass, Prop) -> [C | translate_pat(Rest, InClass, Prop)].

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
            arc_regex_props_ffi:translate_lone(Name, Negated, InClass,
                                               Mode =:= v orelse Mode =:= vi)
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
%% CI (caseless, the i flag) changes the algebra per the spec: every
%% primitive operand set is mapped through simple case folding first
%% (MaybeSimpleCaseFolding, §22.2.2.4), and complement is taken over the
%% scf fixed points (AllCharacters in UnicodeSets+IgnoreCase mode,
%% §22.2.2.6) — NOT over all of 0..10FFFF. Complement-after-folding is what
%% keeps e.g. /[^k]/iv from matching "K": "K" folds to "k", which the
%% complement excludes. PCRE's caseless option then folds the subject at
%% match time, completing Canonicalize.

%% vclass(L, CI): parse the body of a class after `[`, through the closing `]`.
vclass([$^ | Rest], CI) ->
    case vexpr(Rest, CI) of
        {ok, Ranges, [], Rest2} ->
            {ok, vcomp(Ranges, CI), [], Rest2};
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
    {vinter(vnorm(R), vnorm(R2)),
     ordsets:intersection(ordsets:from_list(S), ordsets:from_list(S2))};
vapply(subtract, R, S, R2, S2) ->
    {vinter(vnorm(R), vcomplement(vnorm(R2))),
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
                {char, Hi, R3} when Lo =< Hi -> {ok, vfold([{Lo, Hi}], CI), [], R3};
                {char, _Hi, _R3} -> error;
                {set, _R, _S, _Rest} -> error;
                error -> error
            end;
        {char, _CP, _Rest} = Item -> vsingle(Item, CI);
        {set, R, S, Rest} -> {ok, R, S, Rest};
        error -> error
    end.

vsingle({char, CP, Rest}, CI) -> {ok, vfold([{CP, CP}], CI), [], Rest}.

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
vescape([$d | R], CI) -> {set, vfold(vdigit(), CI), [], R};
vescape([$D | R], CI) -> {set, vcomp(vfold(vdigit(), CI), CI), [], R};
vescape([$w | R], CI) -> {set, vfold(vword(), CI), [], R};
vescape([$W | R], CI) -> {set, vcomp(vfold(vword(), CI), CI), [], R};
vescape([$s | R], CI) -> {set, vfold(vspace(), CI), [], R};
vescape([$S | R], CI) -> {set, vcomp(vfold(vspace(), CI), CI), [], R};
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

vstring_close([CP], Rs, Ss, CI) -> {vfold([{CP, CP}], CI) ++ Rs, Ss};
vstring_close(Str, Rs, Ss, CI) -> {Rs, [vfold_str(Str, CI) | Ss]}.

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
                    {set, vcomp(vfold(Ranges, CI), CI), [], Rest};
                {ok, Ranges} ->
                    {set, vfold(Ranges, CI), [], Rest};
                error when Negated ->
                    error;
                error ->
                    case arc_regex_props_ffi:string_list(PayloadBin) of
                        {ok, Strs} ->
                            {R, S} = vsplit_singles(Strs, CI),
                            {set, R, S, Rest};
                        error -> error
                    end
            end;
        none -> error
    end.

vsplit_singles(Strs, CI) ->
    lists:foldl(
      fun([CP], {R, S}) -> {vfold([{CP, CP}], CI) ++ R, S};
         (Str, {R, S}) -> {R, [vfold_str(Str, CI) | S]}
      end,
      {[], []},
      Strs).

vdigit() -> [{16#30, 16#39}].
vword() -> [{16#30, 16#39}, {16#41, 16#5A}, {16#5F, 16#5F}, {16#61, 16#7A}].
%% JS \s per §22.2.2.9 (same set as ?JSS_CHARS above).
vspace() ->
    [{16#09, 16#0D}, {16#20, 16#20}, {16#A0, 16#A0}, {16#1680, 16#1680},
     {16#2000, 16#200A}, {16#2028, 16#2029}, {16#202F, 16#202F},
     {16#205F, 16#205F}, {16#3000, 16#3000}, {16#FEFF, 16#FEFF}].

%% Sort and merge into disjoint ascending ranges.
vnorm(Ranges) -> vmerge(lists:sort(Ranges)).

vmerge([{Lo, Hi}, {Lo2, Hi2} | Rest]) when Lo2 =< Hi + 1 ->
    vmerge([{Lo, max(Hi, Hi2)} | Rest]);
vmerge([R | Rest]) -> [R | vmerge(Rest)];
vmerge([]) -> [].

%% Complement over 0..10FFFF (input normalized).
vcomplement(Ranges) -> vcomplement(Ranges, 0).

vcomplement([], Next) when Next =< 16#10FFFF -> [{Next, 16#10FFFF}];
vcomplement([], _Next) -> [];
vcomplement([{Lo, Hi} | Rest], Next) when Lo > Next ->
    [{Next, Lo - 1} | vcomplement(Rest, Hi + 1)];
vcomplement([{_Lo, Hi} | Rest], Next) ->
    vcomplement(Rest, max(Next, Hi + 1)).

%% Intersection of two normalized range lists.
vinter([], _B) -> [];
vinter(_A, []) -> [];
vinter([{ALo, AHi} | AR] = A, [{BLo, BHi} | BR] = B) ->
    Lo = max(ALo, BLo),
    Hi = min(AHi, BHi),
    Head = case Lo =< Hi of
               true -> [{Lo, Hi}];
               false -> []
           end,
    Head ++ case AHi =< BHi of
                true -> vinter(AR, B);
                false -> vinter(A, BR)
            end.

%% MaybeSimpleCaseFolding (§22.2.2.4): with the i flag in v mode, map every
%% codepoint of an operand set through scf BEFORE any set algebra. Only
%% codepoints in the scf domain can change, so split the set against the
%% domain and remap just that (small) part.
vfold(Ranges, false) -> Ranges;
vfold(Ranges, true) ->
    N = vnorm(Ranges),
    Dom = scf_domain(),
    Fixed = vinter(N, vcomplement(Dom)),
    Folded = [{F, F} || {Lo, Hi} <- vinter(N, Dom),
                        C <- lists:seq(Lo, Hi),
                        F <- [scf(C)]],
    vnorm(Fixed ++ Folded).

vfold_str(Str, false) -> Str;
vfold_str(Str, true) -> [scf(C) || C <- Str].

%% Close a folded set over the scf equivalence classes: add every codepoint
%% whose scf image is a member (the match-time half of Canonicalize, done at
%% translation time so the emitted class needs no help from PCRE's caseless
%% logic — its own additions then become no-ops, since case partners always
%% share an scf image).
vclose(Ranges) ->
    N = vnorm(Ranges),
    Dom = scf_domain(),
    Extra = [{C, C} || {Lo, Hi} <- Dom,
                       C <- lists:seq(Lo, Hi),
                       vmember(scf(C), N)],
    vnorm(N ++ Extra).

%% Membership test on normalized (sorted, disjoint) ranges.
vmember(_CP, []) -> false;
vmember(CP, [{Lo, Hi} | _]) when CP >= Lo, CP =< Hi -> true;
vmember(CP, [{_Lo, Hi} | Rest]) when CP > Hi -> vmember(CP, Rest);
vmember(_CP, _Ranges) -> false.

%% CharacterComplement (§22.2.2.5) over AllCharacters (§22.2.2.6): with the
%% i flag in v mode the universe is the scf FIXED POINTS, not 0..10FFFF —
%% complement happens after case folding, so a folded-away codepoint (e.g.
%% "K", which folds to "k") is never re-admitted by [^k].
vcomp(Ranges, false) -> vcomplement(vnorm(Ranges));
vcomp(Ranges, true) -> vcomplement(vnorm(Ranges ++ scf_domain())).

%% scf(CP): Simple Case Folding (CaseFolding.txt C+S mappings), the spec's
%% scf abstract operation. string:casefold/1 implements FULL folding (C+F):
%% a single-codepoint result is the common (C) mapping, which scf shares.
%% A multi-codepoint result means CP only has a full (F) mapping — under
%% scf those map to themselves — EXCEPT the 31 codepoints with an explicit
%% simple (S) mapping, hardcoded below (stable per Unicode's policy on
%% existing case foldings).
scf(16#1E9E) -> 16#DF;            %% LATIN CAPITAL LETTER SHARP S
scf(16#1FBC) -> 16#1FB3;          %% GREEK CAPITAL ALPHA WITH PROSGEGRAMMENI
scf(16#1FCC) -> 16#1FC3;          %% GREEK CAPITAL ETA WITH PROSGEGRAMMENI
scf(16#1FD3) -> 16#0390;          %% GREEK SMALL IOTA, DIALYTIKA AND OXIA
scf(16#1FE3) -> 16#03B0;          %% GREEK SMALL UPSILON, DIALYTIKA AND OXIA
scf(16#1FFC) -> 16#1FF3;          %% GREEK CAPITAL OMEGA WITH PROSGEGRAMMENI
scf(16#FB05) -> 16#FB06;          %% LATIN SMALL LIGATURE LONG S T
scf(CP) when CP >= 16#1F88, CP =< 16#1F8F;     %% GREEK CAPITAL ALPHA/ETA/
             CP >= 16#1F98, CP =< 16#1F9F;     %% OMEGA + PROSGEGRAMMENI
             CP >= 16#1FA8, CP =< 16#1FAF ->   %% rows fold to -8
    CP - 8;
scf(CP) ->
    case string:casefold([CP]) of
        [F] -> F;
        _ -> CP
    end.

%% The scf domain — codepoints with scf(c) =/= c — as normalized ranges,
%% cached per process. Derived from Changes_When_Casefolded (a superset:
%% it also contains the F-only codepoints, which scf fixes; filter those
%% out by applying scf).
scf_domain() ->
    case erlang:get(arc_scf_domain) of
        undefined ->
            Cwcf = case arc_regex_props_ffi:char_set(
                          <<"Changes_When_Casefolded">>) of
                       {ok, Ranges} -> Ranges;
                       error -> []
                   end,
            Dom = vnorm([{C, C} || {Lo, Hi} <- Cwcf,
                                   C <- lists:seq(Lo, Hi),
                                   scf(C) =/= C]),
            erlang:put(arc_scf_domain, Dom),
            Dom;
        Dom ->
            Dom
    end.

%% PCRE2 rejects surrogate codepoints in UTF patterns, and valid-UTF-8
%% subjects cannot contain them — drop them from emitted sets.
vstrip_surrogates([]) -> [];
vstrip_surrogates([{Lo, Hi} | Rest]) when Hi < 16#D800; Lo > 16#DFFF ->
    [{Lo, Hi} | vstrip_surrogates(Rest)];
vstrip_surrogates([{Lo, Hi} | Rest]) ->
    Left = case Lo < 16#D800 of
               true -> [{Lo, 16#D7FF}];
               false -> []
           end,
    Right = case Hi > 16#DFFF of
                true -> [{16#E000, Hi}];
                false -> []
            end,
    Left ++ Right ++ vstrip_surrogates(Rest).

%% Render the evaluated set back to PCRE: longer strings first (the spec
%% matches CharSetOfStrings longest-first), then the codepoint class, then
%% the empty string (matches last).
emit_vclass(Ranges0, Strings0) ->
    Ranges = vstrip_surrogates(vnorm(Ranges0)),
    Strings = lists:usort(Strings0),
    NonEmpty = [S || S <- Strings, S =/= []],
    HasEmpty = lists:member([], Strings),
    Sorted = lists:sort(
               fun(A, B) -> {-length(A), A} =< {-length(B), B} end,
               NonEmpty),
    StrParts = [vrender_string(S) || S <- Sorted],
    ClassPart = case Ranges of
                    [] -> [];
                    _ -> [[$[, vrender_ranges(Ranges), $]]]
                end,
    EmptyPart = case HasEmpty of
                    true -> [[]];
                    false -> []
                end,
    Txt = case {StrParts, ClassPart, EmptyPart} of
              %% Nothing at all: a class that can never match.
              {[], [], []} -> ["[^\\x{0}-\\x{10FFFF}]"];
              {[], [Class], []} -> [Class];
              {Ps, Cs, Es} -> ["(?:", lists:join($|, Ps ++ Cs ++ Es), ")"]
          end,
    unicode:characters_to_list(iolist_to_binary(Txt)).

vrender_string(CPs) ->
    [["\\x{", integer_to_list(CP, 16), "}"] || CP <- CPs].

vrender_ranges([]) -> [];
vrender_ranges([{Lo, Lo} | Rest]) ->
    ["\\x{", integer_to_list(Lo, 16), "}" | vrender_ranges(Rest)];
vrender_ranges([{Lo, Hi} | Rest]) ->
    ["\\x{", integer_to_list(Lo, 16), "}-\\x{", integer_to_list(Hi, 16), "}"
     | vrender_ranges(Rest)].

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
%% ToLength clamps a negative lastIndex to 0 (§7.1.20); re:run raises badarg.
regexp_exec(Pattern, Flags, String, Offset) when Offset < 0 ->
    regexp_exec(Pattern, Flags, String, 0);
regexp_exec(Pattern, Flags, String, Offset) ->
    Opts = [{offset, Offset}, {capture, all, index}],
    case safe_run(String, Pattern, Flags, Opts) of
        {match, Captured} -> {ok, Captured};
        _ -> {error, nil}
    end.

%% regexp_exec_info(Pattern, Flags, String, Offset, Sticky)
%%   -> {ok, {Captures, GroupCount, Names}} | {error, nil}
%%
%% Like regexp_exec/4 but:
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
    case safe_run(String, Pattern, Flags, Opts) of
        {match, Captured} ->
            {GroupCount, Names} = group_info(Pattern),
            Padded = pad_captures(Captured, GroupCount + 1),
            {ok, {Padded, GroupCount, Names}};
        _ -> {error, nil}
    end.

%% Pad the capture list with {-1, 0} (PCRE's "unset group" marker) to N
%% entries — PCRE drops trailing unset groups, JS does not.
pad_captures(Caps, N) when length(Caps) >= N -> Caps;
pad_captures(Caps, N) -> Caps ++ lists:duplicate(N - length(Caps), {-1, 0}).

%% Count capturing groups and collect named-group capture indices by scanning
%% the ORIGINAL (untranslated) JS pattern. Tracks character classes (parens
%% are literal inside [...]) and escape pairs. `(?` introduces non-capturing
%% constructs — except `(?<name>` (named capture), distinguished from the
%% lookbehinds `(?<=` / `(?<!` by the third character.
group_info(Pattern) ->
    group_info(unicode:characters_to_list(Pattern), false, 0, []).

group_info([], _InClass, N, Names) ->
    {N, lists:reverse(Names)};
group_info([$\\, _ | Rest], InClass, N, Names) ->
    group_info(Rest, InClass, N, Names);
group_info([$[ | Rest], false, N, Names) ->
    group_info(Rest, true, N, Names);
group_info([$] | Rest], true, N, Names) ->
    group_info(Rest, false, N, Names);
group_info([$(, $?, $<, C | Rest], false, N, Names)
  when C =/= $=, C =/= $! ->
    {Name, Rest2} = take_group_name([C | Rest], []),
    group_info(Rest2, false, N + 1,
               [{unicode:characters_to_binary(Name), N + 1} | Names]);
group_info([$(, $? | Rest], false, N, Names) ->
    group_info(Rest, false, N, Names);
group_info([$( | Rest], false, N, Names) ->
    group_info(Rest, false, N + 1, Names);
group_info([_ | Rest], InClass, N, Names) ->
    group_info(Rest, InClass, N, Names).

take_group_name([$> | Rest], Acc) -> {lists:reverse(Acc), Rest};
take_group_name([C | Rest], Acc) -> take_group_name(Rest, [C | Acc]);
take_group_name([], Acc) -> {lists:reverse(Acc), []}.
