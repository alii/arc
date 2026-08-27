-module(arc_unicode_ffi).
-export([is_id_start/1, is_id_continue/1]).

is_id_start(CP) when CP >= 16#D800, CP =< 16#DFFF -> false;
is_id_start(CP) when CP < 0; CP > 16#10FFFF -> false;
is_id_start(CP) -> in_ranges(CP, table(<<"bin:ID_Start">>)).

is_id_continue(CP) when CP >= 16#D800, CP =< 16#DFFF -> false;
is_id_continue(CP) when CP < 0; CP > 16#10FFFF -> false;
is_id_continue(CP) -> in_ranges(CP, table(<<"bin:ID_Continue">>)).

table(Key) ->
    case arc_regex_uni17_ffi:range_tuple(Key) of
        none -> erlang:error({missing_unicode_table, Key});
        Ranges -> Ranges
    end.

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
