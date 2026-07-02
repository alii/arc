%% Unicode ID_Start / ID_Continue membership for the lexer, backed by the
%% SAME generated Unicode 17 range tables the regex engine uses
%% (arc_regex_uni17_ffi:ranges/1), so the lexer and \p{ID_Start} in RegExp
%% can never disagree about which identifiers are legal.
%%
%% Both predicates are total over the integers: surrogates (reachable from a
%% `\uD800` identifier escape) and out-of-range codepoints answer `false`
%% instead of crashing.
-module(arc_unicode_ffi).
-export([is_id_start/1, is_id_continue/1]).

%% ID_Start (Unicode derived property: L, Nl + Other_ID_Start).
is_id_start(CP) when CP >= 16#D800, CP =< 16#DFFF -> false;
is_id_start(CP) when CP < 0; CP > 16#10FFFF -> false;
is_id_start(CP) -> in_ranges(CP, id_ranges(id_start)).

%% ID_Continue (ID_Start + Mn, Mc, Nd, Pc + Other_ID_Continue).
is_id_continue(CP) when CP >= 16#D800, CP =< 16#DFFF -> false;
is_id_continue(CP) when CP < 0; CP > 16#10FFFF -> false;
is_id_continue(CP) -> in_ranges(CP, id_ranges(id_continue)).

%% -- range tables ---------------------------------------------------------

%% Tuple of {Lo, Hi} pairs (inclusive, sorted, disjoint), decoded once from
%% the generated hex-packed table and cached in persistent_term so lookups
%% are a pure O(log n) binary search.
id_ranges(Which) ->
    Key = {?MODULE, Which},
    case persistent_term:get(Key, undefined) of
        undefined ->
            Ranges = list_to_tuple(decode_ranges(table(Which))),
            persistent_term:put(Key, Ranges),
            Ranges;
        Ranges ->
            Ranges
    end.

table(id_start) -> arc_regex_uni17_ffi:ranges(<<"bin:ID_Start">>);
table(id_continue) -> arc_regex_uni17_ffi:ranges(<<"bin:ID_Continue">>).

%% Hex-packed ranges: 12 hex chars each (6 for the low bound, 6 for the
%% high). Same encoding arc_regex_props_ffi decodes for the regex engine.
decode_ranges(<<>>) -> [];
decode_ranges(<<Lo:6/binary, Hi:6/binary, Rest/binary>>) ->
    [{binary_to_integer(Lo, 16), binary_to_integer(Hi, 16)}
     | decode_ranges(Rest)].

%% Binary search: is CP inside any {Lo, Hi} of the sorted, disjoint tuple?
in_ranges(CP, Ranges) -> in_ranges(CP, Ranges, 1, tuple_size(Ranges)).

in_ranges(_CP, _Ranges, Lo, Hi) when Lo > Hi ->
    false;
in_ranges(CP, Ranges, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    {RangeLo, RangeHi} = element(Mid, Ranges),
    if
        CP < RangeLo -> in_ranges(CP, Ranges, Lo, Mid - 1);
        CP > RangeHi -> in_ranges(CP, Ranges, Mid + 1, Hi);
        true -> true
    end.
