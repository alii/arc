%% tzif files (rfc 8536) and transition search
-module(arc_tzif).

-export([parse/1, offset_at/2, first_transition_after/2,
         last_transition_before/2]).

-export_type([tz/0, footer/0]).

-type footer() :: none | arc_posix_tz:footer().

%% {tz, first offset, transitions tuple, footer, last transition sec}
-type tz() :: {tz, integer(), tuple(), footer(), integer() | none}.

%% throws on malformed input, caller catches
-spec parse(binary()) -> tz().
parse(<<"TZif", Ver:8, _:15/binary, IsUt:32, IsStd:32, Leap:32,
        Timecnt:32, Typecnt:32, Charcnt:32, Rest/binary>>) ->
    case Ver of
        0 ->
            %% v1: 32-bit data, no footer
            {First, Trans, _After} =
                parse_block(Rest, Timecnt, Typecnt, Charcnt, 4),
            make_zone(First, dedupe(First, Trans), none);
        _ ->
            %% v2/3: skip v1 block, parse 64-bit block and footer
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

%% drop transitions that keep the same offset
dedupe(First, Trans) ->
    {_, Out} = lists:foldl(
                 fun({T, Off}, {Prev, Acc}) ->
                     case Off =:= Prev of
                         true -> {Prev, Acc};
                         false -> {Off, [{T, Off} | Acc]}
                     end
                 end, {First, []}, Trans),
    lists:reverse(Out).

parse_footer(<<"\n", Rest/binary>>) ->
    case binary:split(Rest, <<"\n">>) of
        [<<>>, _] -> none;
        [TzStr, _] -> arc_posix_tz:parse(binary_to_list(TzStr));
        _ -> none
    end;
parse_footer(_) -> none.

-spec offset_at(tz(), integer()) -> integer().
offset_at({tz, First, Trans, Footer, LastT}, Sec) ->
    UseFooter = Footer =/= none andalso
        (LastT =:= none orelse Sec >= LastT),
    case UseFooter of
        true -> arc_posix_tz:offset_at(Footer, Sec);
        false -> offset_from_transitions(First, Trans, Sec)
    end.

offset_from_transitions(First, Trans, Sec) ->
    search_transitions(First, Trans, Sec, 1, tuple_size(Trans)).

search_transitions(Acc, _Trans, _Sec, Lo, Hi) when Lo > Hi -> Acc;
search_transitions(Acc, Trans, Sec, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    case element(Mid, Trans) of
        {T, Off} when T =< Sec -> search_transitions(Off, Trans, Sec, Mid + 1, Hi);
        _ -> search_transitions(Acc, Trans, Sec, Lo, Mid - 1)
    end.

-spec first_transition_after(tz(), integer()) -> integer() | none.
first_transition_after({tz, _First, Trans, Footer, LastT}, Sec) ->
    case search_after(none, Trans, Sec, 1, tuple_size(Trans)) of
        none -> footer_next(Footer, LastT, Sec);
        T -> T
    end.

search_after(Best, _Trans, _Sec, Lo, Hi) when Lo > Hi -> Best;
search_after(Best, Trans, Sec, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    case element(Mid, Trans) of
        {T, _} when T > Sec -> search_after(T, Trans, Sec, Lo, Mid - 1);
        _ -> search_after(Best, Trans, Sec, Mid + 1, Hi)
    end.

footer_next(none, _LastT, _Sec) -> none;
footer_next(Footer, LastT, Sec) ->
    FromY = case LastT of
        none -> arc_posix_tz:year_of(Sec);
        L -> max(arc_posix_tz:year_of(Sec), arc_posix_tz:year_of(L))
    end,
    Cands = [T || {T, _} <- arc_posix_tz:transitions(Footer, FromY - 1, FromY + 2),
                  T > Sec,
                  LastT =:= none orelse T > LastT],
    case Cands of
        [] -> none;
        _ -> lists:min(Cands)
    end.

-spec last_transition_before(tz(), integer()) -> integer() | none.
last_transition_before({tz, _First, Trans, Footer, LastT}, Sec) ->
    case footer_previous(Footer, LastT, Sec) of
        none -> search_before(none, Trans, Sec, 1, tuple_size(Trans));
        T -> T
    end.

search_before(Best, _Trans, _Sec, Lo, Hi) when Lo > Hi -> Best;
search_before(Best, Trans, Sec, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    case element(Mid, Trans) of
        {T, _} when T < Sec -> search_before(T, Trans, Sec, Mid + 1, Hi);
        _ -> search_before(Best, Trans, Sec, Lo, Mid - 1)
    end.

footer_previous(none, _LastT, _Sec) -> none;
footer_previous(Footer, LastT, Sec) ->
    case LastT =:= none orelse Sec > LastT of
        false -> none;
        true ->
            Y = arc_posix_tz:year_of(Sec),
            Cands = [T || {T, _} <- arc_posix_tz:transitions(Footer, Y - 2, Y + 1),
                          T < Sec,
                          LastT =:= none orelse T > LastT],
            case Cands of
                [] -> none;
                _ -> lists:max(Cands)
            end
    end.
