%% tzif files (rfc 8536) and transition search
-module(arc_tzif).

-export([parse/1, offset_at/2, next_transition/2, previous_transition/2]).

-export_type([tz/0]).

-type footer() :: none | arc_posix_tz:posix_tz().

-record(tz, {initial_offset :: integer(),
             transitions :: tuple(),
             footer :: footer(),
             last_transition :: integer() | none}).

-type tz() :: #tz{}.

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
    #tz{initial_offset = First, transitions = list_to_tuple(Trans),
        footer = Footer, last_transition = LastT}.

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
offset_at(#tz{footer = Footer, last_transition = LastT} = Tz, Sec) ->
    UseFooter = Footer =/= none andalso
        (LastT =:= none orelse Sec >= LastT),
    case UseFooter of
        true -> arc_posix_tz:offset_at(Footer, Sec);
        false ->
            Trans = Tz#tz.transitions,
            offset_at_or_before(Tz#tz.initial_offset, Trans, Sec,
                                1, tuple_size(Trans))
    end.

offset_at_or_before(Best, _Trans, _Sec, Lo, Hi) when Lo > Hi -> Best;
offset_at_or_before(Best, Trans, Sec, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    case element(Mid, Trans) of
        {T, Off} when T =< Sec -> offset_at_or_before(Off, Trans, Sec, Mid + 1, Hi);
        _ -> offset_at_or_before(Best, Trans, Sec, Lo, Mid - 1)
    end.

-spec next_transition(tz(), integer()) -> {some, integer()} | none.
next_transition(#tz{transitions = Trans, footer = Footer,
                    last_transition = LastT}, Sec) ->
    case first_after(none, Trans, Sec, 1, tuple_size(Trans)) of
        none -> footer_next(Footer, LastT, Sec);
        Found -> Found
    end.

first_after(Best, _Trans, _Sec, Lo, Hi) when Lo > Hi -> Best;
first_after(Best, Trans, Sec, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    case element(Mid, Trans) of
        {T, _} when T > Sec -> first_after({some, T}, Trans, Sec, Lo, Mid - 1);
        _ -> first_after(Best, Trans, Sec, Mid + 1, Hi)
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
        _ -> {some, lists:min(Cands)}
    end.

-spec previous_transition(tz(), integer()) -> {some, integer()} | none.
previous_transition(#tz{transitions = Trans, footer = Footer,
                        last_transition = LastT}, Sec) ->
    case footer_previous(Footer, LastT, Sec) of
        none -> last_before(none, Trans, Sec, 1, tuple_size(Trans));
        Found -> Found
    end.

last_before(Best, _Trans, _Sec, Lo, Hi) when Lo > Hi -> Best;
last_before(Best, Trans, Sec, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    case element(Mid, Trans) of
        {T, _} when T < Sec -> last_before({some, T}, Trans, Sec, Mid + 1, Hi);
        _ -> last_before(Best, Trans, Sec, Lo, Mid - 1)
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
                _ -> {some, lists:max(Cands)}
            end
    end.
