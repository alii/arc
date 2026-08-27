%% codepoint sets as sorted disjoint [{Lo, Hi}] ranges, §22.2.2

-module(arc_regex_charset).

-export([vdigit/0, vword/0, vspace/0]).
-export([vinter/2, vsubtract/2]).
-export([vfold/2, vfold_str/2, vclose/1, character_complement/2, vsplit_singles/2]).
-export([scf/1]).
-export([emit_complement/2, emit_vclass/2, vstrip_surrogates/1, vrender_ranges/1]).

vdigit() -> [{16#30, 16#39}].
vword() -> [{16#30, 16#39}, {16#41, 16#5A}, {16#5F, 16#5F}, {16#61, 16#7A}].
%% §22.2.2.9 whitespace + lineterminator
vspace() ->
    [{16#09, 16#0D}, {16#20, 16#20}, {16#A0, 16#A0}, {16#1680, 16#1680},
     {16#2000, 16#200A}, {16#2028, 16#2029}, {16#202F, 16#202F},
     {16#205F, 16#205F}, {16#3000, 16#3000}, {16#FEFF, 16#FEFF}].

vnorm(Ranges) -> vmerge(lists:sort(Ranges)).

vmerge([{Lo, Hi}, {Lo2, Hi2} | Rest]) when Lo2 =< Hi + 1 ->
    vmerge([{Lo, max(Hi, Hi2)} | Rest]);
vmerge([R | Rest]) -> [R | vmerge(Rest)];
vmerge([]) -> [].

vcomplement(Ranges) -> vcomplement(Ranges, 0).

vcomplement([], Next) when Next =< 16#10FFFF -> [{Next, 16#10FFFF}];
vcomplement([], _Next) -> [];
vcomplement([{Lo, Hi} | Rest], Next) when Lo > Next ->
    [{Next, Lo - 1} | vcomplement(Rest, Hi + 1)];
vcomplement([{_Lo, Hi} | Rest], Next) ->
    vcomplement(Rest, max(Next, Hi + 1)).

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

vsubtract(A, B) -> vinter_sorted(vnorm(A), vcomplement(vnorm(B))).

vmember(_CP, []) -> false;
vmember(CP, [{Lo, Hi} | _]) when CP >= Lo, CP =< Hi -> true;
vmember(CP, [{_Lo, Hi} | Rest]) when CP > Hi -> vmember(CP, Rest);
vmember(_CP, _Ranges) -> false.

%% §22.2.2.5; with i flag the universe is scf fixed points
character_complement(Ranges, false) -> vcomplement(vnorm(Ranges));
character_complement(Ranges, true) ->
    vcomplement(vnorm(vfold(Ranges, true) ++ scf_domain())).

%% §22.2.2.4 maybesimplecasefolding
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

%% add every codepoint whose scf image is a member
vclose(Ranges) ->
    N = vnorm(Ranges),
    Dom = scf_domain(),
    Extra = [{C, C} || {Lo, Hi} <- Dom,
                       C <- lists:seq(Lo, Hi),
                       vmember(scf(C), N)],
    vnorm(N ++ Extra).

vsplit_singles(Strs, CI) ->
    lists:foldl(
      fun([CP], {R, S}) -> {vfold([{CP, CP}], CI) ++ R, S};
         (Str, {R, S}) -> {R, [vfold_str(Str, CI) | S]}
      end,
      {[], []},
      Strs).

%% simple case folding; casefold/1 is full folding, s-only mappings hardcoded
scf(16#1E9E) -> 16#DF;
scf(16#1FBC) -> 16#1FB3;
scf(16#1FCC) -> 16#1FC3;
scf(16#1FD3) -> 16#0390;
scf(16#1FE3) -> 16#03B0;
scf(16#1FFC) -> 16#1FF3;
scf(16#FB05) -> 16#FB06;
scf(CP) when CP >= 16#1F88, CP =< 16#1F8F;
             CP >= 16#1F98, CP =< 16#1F9F;
             CP >= 16#1FA8, CP =< 16#1FAF ->
    CP - 8;
scf(CP) ->
    case string:casefold([CP]) of
        [F] -> F;
        _ -> CP
    end.

scf_domain() -> arc_regex_uni17_ffi:scf_domain().

%% pcre2 rejects surrogates in utf patterns
vstrip_surrogates(Ranges) -> vsubtract(Ranges, [{16#D800, 16#DFFF}]).

%% close over scf first so caseless pcre cannot fold back onto a member
emit_complement(Set, CI) ->
    Closed = case CI of
                 true -> vclose(Set);
                 false -> vnorm(Set)
             end,
    Ranges = vstrip_surrogates(vcomplement(Closed)),
    unicode:characters_to_list(iolist_to_binary(vrender_ranges(Ranges))).

%% longest strings first, then the class, then empty
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
