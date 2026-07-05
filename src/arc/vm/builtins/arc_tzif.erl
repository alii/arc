%% TZif files (RFC 8536): the binary format the IANA zoneinfo tree is compiled
%% into, and the searches over the transition table it yields.
%%
%% A parsed zone is opaque-ish but flat, so it can live in a persistent_term:
%% see tz(). Everything past its last recorded transition is governed by the
%% POSIX footer, which arc_posix_tz owns.
-module(arc_tzif).

-export([parse/1, offset_at/2, first_transition_after/2,
         last_transition_before/2, footer/1, last_transition/1]).

-export_type([tz/0, footer/0]).

%% What governs the offset past a zone's last recorded transition. `none` only
%% for TZif v1 files, which carry no footer at all.
-type footer() :: none | arc_posix_tz:footer().

%% The offset before the first transition, the ascending transition tuple
%% ({Sec, OffSec} pairs, each one changing the offset — a *tuple* so a query can
%% binary-search it), the footer, and the last transition's time (`none` when
%% there are no transitions), precomputed rather than walked on every query.
-type tz() :: {tz, integer(), tuple(), footer(), integer() | none}.

-spec footer(tz()) -> footer().
footer({tz, _First, _Trans, Footer, _LastT}) -> Footer.

-spec last_transition(tz()) -> integer() | none.
last_transition({tz, _First, _Trans, _Footer, LastT}) -> LastT.

%% ----------------------------------------------------------------------
%% File format
%% ----------------------------------------------------------------------

%% Throws on anything that is not a well-formed TZif file; the caller catches.
-spec parse(binary()) -> tz().
parse(<<"TZif", Ver:8, _:15/binary, IsUt:32, IsStd:32, Leap:32,
        Timecnt:32, Typecnt:32, Charcnt:32, Rest/binary>>) ->
    case Ver of
        0 ->
            %% Version 1: 32-bit data, no footer.
            {First, Trans, _After} =
                parse_block(Rest, Timecnt, Typecnt, Charcnt, 4),
            make_zone(First, dedupe(First, Trans), none);
        _ ->
            %% Version 2/3: skip the v1 block, parse the 64-bit block + footer.
            V1Size = Timecnt * 5 + Typecnt * 6 + Charcnt + Leap * 8
                     + IsStd + IsUt,
            <<_:V1Size/binary, "TZif", _V2:8, _:15/binary,
              IsUt2:32, IsStd2:32, Leap2:32, Timecnt2:32, Typecnt2:32,
              Charcnt2:32, Rest2/binary>> = Rest,
            {First, Trans, After} =
                parse_block(Rest2, Timecnt2, Typecnt2, Charcnt2, 8),
            SkipTail = Leap2 * 12 + IsStd2 + IsUt2,
            <<_:SkipTail/binary, FooterBin/binary>> = After,
            make_zone(First, dedupe(First, Trans), parse_footer(FooterBin))
    end.

make_zone(First, Trans, Footer) ->
    LastT = case Trans of
        [] -> none;
        _ -> element(1, lists:last(Trans))
    end,
    {tz, First, list_to_tuple(Trans), Footer, LastT}.

parse_block(Bin, Timecnt, Typecnt, Charcnt, TSize) ->
    TransBytes = Timecnt * TSize,
    TypeBytes = Typecnt * 6,
    TBits = TSize * 8,
    <<TransBin:TransBytes/binary, IdxBin:Timecnt/binary,
      TypesBin:TypeBytes/binary, _Abbr:Charcnt/binary, After/binary>> = Bin,
    Times = [T || <<T:TBits/signed-big>> <= TransBin],
    Idxs = binary_to_list(IdxBin),
    Types = [{Off, IsDst} || <<Off:32/signed-big, IsDst:8, _:8>> <= TypesBin],
    First = first_offset(Types),
    Trans = lists:zipwith(
              fun(T, Idx) ->
                  {Off, _} = lists:nth(Idx + 1, Types),
                  {T, Off}
              end, Times, Idxs),
    {First, Trans, After}.

first_offset([]) -> 0;
first_offset(Types) ->
    case [Off || {Off, IsDst} <- Types, IsDst =:= 0] of
        [Off | _] -> Off;
        [] -> element(1, hd(Types))
    end.

%% Drop transitions that do not change the UTC offset.
dedupe(First, Trans) ->
    {_, Out} = lists:foldl(
                 fun({T, Off}, {Prev, Acc}) ->
                     case Off =:= Prev of
                         true -> {Prev, Acc};
                         false -> {Off, [{T, Off} | Acc]}
                     end
                 end, {First, []}, Trans),
    lists:reverse(Out).

%% The footer is a POSIX TZ string on its own line at the end of the file.
parse_footer(<<"\n", Rest/binary>>) ->
    case binary:split(Rest, <<"\n">>) of
        [<<>>, _] -> none;
        [TzStr, _] -> arc_posix_tz:parse(binary_to_list(TzStr));
        _ -> none
    end;
parse_footer(_) -> none.

%% ----------------------------------------------------------------------
%% Transition search — every one of these is a binary search over the tuple
%% ----------------------------------------------------------------------

%% The offset a parsed zone reports at Sec: past the last recorded transition
%% the POSIX footer rule takes over, before it a binary search over the
%% transition array answers in O(log n).
-spec offset_at(tz(), integer()) -> integer().
offset_at({tz, First, Trans, Footer, LastT}, Sec) ->
    UseFooter = Footer =/= none andalso
        (LastT =:= none orelse Sec >= LastT),
    case UseFooter of
        true -> arc_posix_tz:offset_at(Footer, Sec);
        false -> offset_from_transitions(First, Trans, Sec)
    end.

%% Offset in effect at Sec: the offset of the last transition at or before it,
%% First when there is none. Zones carry a couple hundred transitions and this
%% runs on every Date getter.
offset_from_transitions(First, Trans, Sec) ->
    search_transitions(First, Trans, Sec, 1, tuple_size(Trans)).

search_transitions(Acc, _Trans, _Sec, Lo, Hi) when Lo > Hi -> Acc;
search_transitions(Acc, Trans, Sec, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    case element(Mid, Trans) of
        {T, Off} when T =< Sec -> search_transitions(Off, Trans, Sec, Mid + 1, Hi);
        _ -> search_transitions(Acc, Trans, Sec, Lo, Mid - 1)
    end.

%% Smallest recorded transition time strictly after Sec, `none` if there is
%% none.
-spec first_transition_after(tz(), integer()) -> integer() | none.
first_transition_after({tz, _First, Trans, _Footer, _LastT}, Sec) ->
    search_after(none, Trans, Sec, 1, tuple_size(Trans)).

search_after(Best, _Trans, _Sec, Lo, Hi) when Lo > Hi -> Best;
search_after(Best, Trans, Sec, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    case element(Mid, Trans) of
        {T, _} when T > Sec -> search_after(T, Trans, Sec, Lo, Mid - 1);
        _ -> search_after(Best, Trans, Sec, Mid + 1, Hi)
    end.

%% Largest recorded transition time strictly before Sec, `none` if there is
%% none.
-spec last_transition_before(tz(), integer()) -> integer() | none.
last_transition_before({tz, _First, Trans, _Footer, _LastT}, Sec) ->
    search_before(none, Trans, Sec, 1, tuple_size(Trans)).

search_before(Best, _Trans, _Sec, Lo, Hi) when Lo > Hi -> Best;
search_before(Best, Trans, Sec, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    case element(Mid, Trans) of
        {T, _} when T < Sec -> search_before(T, Trans, Sec, Mid + 1, Hi);
        _ -> search_before(Best, Trans, Sec, Lo, Mid - 1)
    end.
