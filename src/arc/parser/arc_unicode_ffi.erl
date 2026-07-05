%% Unicode ID_Start / ID_Continue membership for the lexer, backed by the
%% SAME generated Unicode 17 range tables the regex engine uses
%% (arc_unicode_tables:range_tuple/1), so the lexer and \p{ID_Start} in RegExp
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
is_id_start(CP) -> in_ranges(CP, table(id_start)).

%% ID_Continue (ID_Start + Mn, Mc, Nd, Pc + Other_ID_Continue).
is_id_continue(CP) when CP >= 16#D800, CP =< 16#DFFF -> false;
is_id_continue(CP) when CP < 0; CP > 16#10FFFF -> false;
is_id_continue(CP) -> in_ranges(CP, table(id_continue)).

%% -- range tables ---------------------------------------------------------

%% Tuple of {Lo, Hi} pairs (inclusive, sorted, disjoint) so lookups are a pure
%% O(log n) binary search. arc_unicode_tables decodes it (once per node).
%%
%% These predicates run once per source character, so the fetch is memoized
%% here too, under a FLAT atom-only persistent_term key. Reaching through
%% arc_unicode_tables:range_tuple/1 on every call would re-hash the nested
%% `{arc_unicode_tables, {range_tuple, <<"bin:ID_Start">>}}` key each time —
%% about twice the cost of looking up `{?MODULE, id_start}`.
table(Which) ->
    case persistent_term:get({?MODULE, Which}, undefined) of
        undefined -> load_table(Which);
        Ranges -> Ranges
    end.

%% The generated table carrying these two properties is a hard build
%% invariant: without it every identifier in every source file would be
%% rejected. Crash on a missing key rather than answer `false` forever.
load_table(Which) ->
    Key = table_key(Which),
    case arc_unicode_tables:range_tuple(Key) of
        none ->
            erlang:error({missing_unicode_table, Key});
        Ranges ->
            persistent_term:put({?MODULE, Which}, Ranges),
            Ranges
    end.

table_key(id_start) -> <<"bin:ID_Start">>;
table_key(id_continue) -> <<"bin:ID_Continue">>.

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
