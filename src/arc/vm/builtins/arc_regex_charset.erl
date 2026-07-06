%% Codepoint-set algebra for the JS regex translator: sorted disjoint ranges,
%% the operations §22.2.2 defines over them (union / intersection /
%% subtraction / CharacterComplement), simple case folding (scf), and
%% rendering a set back into a PCRE character class.
%%
%% Split out of arc_regexp_ffi so this half is what it looks like: PURE, with
%% no dependency on `re` and no knowledge of the JS pattern grammar. Its
%% caller (arc_regexp_ffi's v-mode class parser) walks the source and hands
%% operand sets down here; the ONLY thing that comes back is a set, or the
%% PCRE text of one. Callers never see the range representation's invariants
%% (sorted, disjoint, merged) — every exported function re-establishes them.
%%
%% A range list is [{Lo, Hi}] of codepoints, ascending and non-adjacent once
%% normalised. Strings (the v flag's \q{...} and properties-of-strings) are
%% lists of codepoints and are only touched by vfold_str/2 and vsplit_singles/2.
%%
%% CI (caseless, the i flag) changes the algebra per the spec: every primitive
%% operand set is mapped through simple case folding first
%% (MaybeSimpleCaseFolding, §22.2.2.4), and complement is taken over the scf
%% fixed points (AllCharacters in UnicodeSets+IgnoreCase mode, §22.2.2.6) —
%% NOT over all of 0..10FFFF. Complement-after-folding is what keeps e.g.
%% /[^k]/iv from matching "K": "K" folds to "k", which the complement excludes.
%% PCRE's caseless option then folds the subject at match time, completing
%% Canonicalize.
-module(arc_regex_charset).

-export([vdigit/0, vword/0, vspace/0]).
-export([vinter/2, vsubtract/2]).
-export([vfold/2, vfold_str/2, vclose/1, character_complement/2, vsplit_singles/2]).
-export([emit_complement/2, emit_vclass/2, vstrip_surrogates/1, vrender_ranges/1]).

%% ---- The JS class-escape base sets --------------------------------------

vdigit() -> [{16#30, 16#39}].
vword() -> [{16#30, 16#39}, {16#41, 16#5A}, {16#5F, 16#5F}, {16#61, 16#7A}].
%% JS \s per §22.2.2.9: WhiteSpace + LineTerminator productions.
vspace() ->
    [{16#09, 16#0D}, {16#20, 16#20}, {16#A0, 16#A0}, {16#1680, 16#1680},
     {16#2000, 16#200A}, {16#2028, 16#2029}, {16#202F, 16#202F},
     {16#205F, 16#205F}, {16#3000, 16#3000}, {16#FEFF, 16#FEFF}].

%% ---- Range algebra -------------------------------------------------------

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

%% Intersection (normalizes both sides).
vinter(A, B) -> vinter_sorted(vnorm(A), vnorm(B)).

vinter_sorted([], _B) -> [];
vinter_sorted(_A, []) -> [];
vinter_sorted([{ALo, AHi} | AR] = A, [{BLo, BHi} | BR] = B) ->
    Lo = max(ALo, BLo),
    Hi = min(AHi, BHi),
    Head = case Lo =< Hi of
               true -> [{Lo, Hi}];
               false -> []
           end,
    Head ++ case AHi =< BHi of
                true -> vinter_sorted(AR, B);
                false -> vinter_sorted(A, BR)
            end.

%% Subtraction A -- B (normalizes both sides).
vsubtract(A, B) -> vinter_sorted(vnorm(A), vcomplement(vnorm(B))).

%% Membership test on normalized (sorted, disjoint) ranges.
vmember(_CP, []) -> false;
vmember(CP, [{Lo, Hi} | _]) when CP >= Lo, CP =< Hi -> true;
vmember(CP, [{_Lo, Hi} | Rest]) when CP > Hi -> vmember(CP, Rest);
vmember(_CP, _Ranges) -> false.

%% CharacterComplement (§22.2.2.5) over AllCharacters (§22.2.2.6): with the
%% i flag in v mode the universe is the scf FIXED POINTS, not 0..10FFFF —
%% complement happens after case folding, so a folded-away codepoint (e.g.
%% "K", which folds to "k") is never re-admitted by [^k]. The operand set is
%% folded HERE (idempotent on already-folded input), so an unfolded caller
%% cannot silently miscompute the complement.
character_complement(Ranges, false) -> vcomplement(vnorm(Ranges));
character_complement(Ranges, true) ->
    vcomplement(vnorm(vfold(Ranges, true) ++ scf_domain())).

%% ---- Case folding --------------------------------------------------------

%% MaybeSimpleCaseFolding (§22.2.2.4): with the i flag in v mode, map every
%% codepoint of an operand set through scf BEFORE any set algebra. Only
%% codepoints in the scf domain can change, so split the set against the
%% domain and remap just that (small) part.
vfold(Ranges, false) -> Ranges;
vfold(Ranges, true) ->
    N = vnorm(Ranges),
    Dom = scf_domain(),
    Fixed = vinter_sorted(N, vcomplement(Dom)),
    Folded = [{F, F} || {Lo, Hi} <- vinter_sorted(N, Dom),
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

%% Split a list of codepoint strings into {SingleCodepointRanges, Strings},
%% folding both halves. Single-codepoint members of a string set are ordinary
%% characters (§22.2.2 CharSetOfStrings).
vsplit_singles(Strs, CI) ->
    lists:foldl(
      fun([CP], {R, S}) -> {vfold([{CP, CP}], CI) ++ R, S};
         (Str, {R, S}) -> {R, [vfold_str(Str, CI) | S]}
      end,
      {[], []},
      Strs).

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

%% The scf domain — codepoints with scf(c) =/= c — as normalized ranges.
%% Derived from Changes_When_Casefolded (a superset: it also contains the
%% F-only codepoints, which scf fixes; filter those out by applying scf).
%%
%% Cached in persistent_term, not the process dictionary: the table is derived
%% from immutable generated data, so it is the same in every process, and the
%% sibling caches of the same shape (arc_unicode_tables) already live there.
%% A per-process cache re-derived the whole thing — a full
%% lists:seq over every Changes_When_Casefolded range — once per agent process.
%%
%% The table carrying Changes_When_Casefolded is a hard invariant of this
%% module: without it every case-insensitive class silently evaluates against
%% an EMPTY fold domain and matches the wrong characters. Crash on a missing
%% table rather than degrade forever.
scf_domain() ->
    Key = {?MODULE, scf_domain},
    case persistent_term:get(Key, undefined) of
        undefined ->
            {ok, Cwcf} =
                arc_regex_props_ffi:char_set(<<"Changes_When_Casefolded">>),
            Dom = vnorm([{C, C} || {Lo, Hi} <- Cwcf,
                                   C <- lists:seq(Lo, Hi),
                                   scf(C) =/= C]),
            persistent_term:put(Key, Dom),
            Dom;
        Dom ->
            Dom
    end.

%% ---- Emitting a set back to PCRE ----------------------------------------

%% PCRE2 rejects surrogate codepoints in UTF patterns, and valid-UTF-8
%% subjects cannot contain them — drop them from emitted sets.
vstrip_surrogates(Ranges) -> vsubtract(Ranges, [{16#D800, 16#DFFF}]).

%% Class ITEMS for a negated JS class escape (\S, \W) spliced into a [...]:
%% PCRE has no nested classes, so the complement set has to be written out.
%%
%% Under `caseless` PCRE folds every class item at match time, so a spliced
%% item drags its case partners into the class — closing the POSITIVE set over
%% the scf equivalence classes first is what stops a complement item folding
%% back onto a member of it. That is what makes /[\W]/i reject "ſ" and "K"
%% (they fold to word characters), exactly like the out-of-class /\W/i, which
%% PCRE renders as [^0-9A-Za-z_] and folds for us. Surrogates are dropped:
%% PCRE2 rejects them in UTF patterns and valid subjects cannot contain them.
emit_complement(Set, CI) ->
    Closed = case CI of
                 true -> vclose(Set);
                 false -> vnorm(Set)
             end,
    Ranges = vstrip_surrogates(vcomplement(Closed)),
    unicode:characters_to_list(iolist_to_binary(vrender_ranges(Ranges))).

%% Render the evaluated set back to PCRE: longer strings first (the spec
%% matches CharSetOfStrings longest-first), then the codepoint class, then
%% the empty string (matches last).
%%
%% This is the ONE place the PCRE "no lone surrogates in a UTF pattern"
%% constraint is applied, and it applies to both halves of the set: a range is
%% clipped (vstrip_surrogates) and an ALTERNATIVE containing an unpaired
%% surrogate is dropped, on the same argument — a valid UTF-8 subject can never
%% contain one, so neither can ever match. Rendering such an alternative
%% verbatim (`\x{D800}`) makes PCRE reject a perfectly legal JS pattern like
%% `[\q{\uD800}]`.
emit_vclass(Ranges0, Strings0) ->
    Ranges = vstrip_surrogates(vnorm(Ranges0)),
    Strings = [S || S <- lists:usort(Strings0), not has_surrogate(S)],
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

has_surrogate(CPs) ->
    lists:any(fun(CP) -> CP >= 16#D800 andalso CP =< 16#DFFF end, CPs).

vrender_string(CPs) ->
    [["\\x{", integer_to_list(CP, 16), "}"] || CP <- CPs].

vrender_ranges([]) -> [];
vrender_ranges([{Lo, Lo} | Rest]) ->
    ["\\x{", integer_to_list(Lo, 16), "}" | vrender_ranges(Rest)];
vrender_ranges([{Lo, Hi} | Rest]) ->
    ["\\x{", integer_to_list(Lo, 16), "}-\\x{", integer_to_list(Hi, 16), "}"
     | vrender_ranges(Rest)].
