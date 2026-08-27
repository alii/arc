%% offsets here are local minus utc, posix sign negated at parse
-module(arc_posix_tz).

-export([parse/1, offset_at/2, transitions/3, year_of/1]).

-export_type([footer/0, rule/0]).

-define(EPOCH_GS, 62167219200).
-define(EPOCH_DAYS, 719528).

-type rule() :: {m, 1..12, 1..5, 0..6, integer()}
              | {j, 1..365, integer()}
              | {d0, 0..365, integer()}.

-type footer() :: {fixed, integer()} | {dst, integer(), integer(), rule(), rule()}.

bind(none, _F) -> none;
bind({ok, Rest}, F) -> F(Rest).

bind3(none, _F) -> none;
bind3({ok, V, Rest}, F) -> F(V, Rest).

-spec parse(string()) -> footer() | none.
parse(S) ->
    bind(parse_name(S),
         fun(R1) ->
             bind3(parse_posix_offset(R1),
                   fun(StdPosix, R2) -> parse_posix_dst(-StdPosix, R2) end)
         end).

parse_posix_dst(StdOff, S) ->
    case parse_name(S) of
        none -> {fixed, StdOff};
        {ok, R3} ->
            {DstOff, R4} =
                case parse_posix_offset(R3) of
                    {ok, DP, RR} -> {-DP, RR};
                    none -> {StdOff + 3600, R3}
                end,
            parse_posix_rules(StdOff, DstOff, R4)
    end.

parse_posix_rules(StdOff, DstOff, "," ++ R5) ->
    Footer =
        bind3(parse_rule(R5),
              fun(Rule1, "," ++ R6) ->
                      bind3(parse_rule(R6),
                            fun(Rule2, _) ->
                                    {dst, StdOff, DstOff, Rule1, Rule2}
                            end);
                 (_, _) -> none
              end),
    case Footer of
        none -> {fixed, StdOff};
        _ -> Footer
    end;
parse_posix_rules(StdOff, DstOff, _) ->
    %% no rule given: posix default us rule
    {dst, StdOff, DstOff, {m, 3, 2, 0, 7200}, {m, 11, 1, 0, 7200}}.

parse_name("<" ++ Rest) ->
    case lists:splitwith(fun(C) -> C =/= $> end, Rest) of
        {Q, ">" ++ R} when Q =/= [] -> {ok, R};
        _ -> none
    end;
parse_name(S) ->
    {Name, R} = lists:splitwith(fun is_alpha/1, S),
    case Name of
        [] -> none;
        _ -> {ok, R}
    end.

is_alpha(C) -> (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z).

parse_posix_offset(S) ->
    {Sign, R0} = case S of
        "-" ++ R -> {-1, R};
        "+" ++ R -> {1, R};
        _ -> {1, S}
    end,
    case parse_int(R0) of
        {ok, H, R1} ->
            {M, R2} = parse_opt_colon_int(R1),
            {Sc, R3} = parse_opt_colon_int(R2),
            {ok, Sign * (H * 3600 + M * 60 + Sc), R3};
        none -> none
    end.

parse_opt_colon_int(":" ++ R) ->
    case parse_int(R) of
        {ok, N, R2} -> {N, R2};
        none -> {0, ":" ++ R}
    end;
parse_opt_colon_int(R) -> {0, R}.

parse_int(S) ->
    {Digits, R} = lists:splitwith(fun(C) -> C >= $0 andalso C =< $9 end, S),
    case Digits of
        [] -> none;
        _ -> {ok, list_to_integer(Digits), R}
    end.

%% ranges checked at parse; eval has no handler
in_range(N, Lo, Hi) -> is_integer(N) andalso N >= Lo andalso N =< Hi.

parse_rule("M" ++ R0) ->
    bind3(parse_int(R0),
          fun(M, "." ++ R1) ->
                  bind3(parse_int(R1),
                        fun(W, "." ++ R2) ->
                                bind3(parse_int(R2),
                                      fun(D, R3) ->
                                              {T, R4} = parse_rule_time(R3),
                                              rule_m(M, W, D, T, R4)
                                      end);
                           (_, _) -> none
                        end);
             (_, _) -> none
          end);
parse_rule("J" ++ R0) ->
    bind3(parse_int(R0),
          fun(N, R1) ->
                  {T, R2} = parse_rule_time(R1),
                  case in_range(N, 1, 365) of
                      true -> {ok, {j, N, T}, R2};
                      false -> none
                  end
          end);
parse_rule(S) ->
    bind3(parse_int(S),
          fun(N, R1) ->
                  {T, R2} = parse_rule_time(R1),
                  case in_range(N, 0, 365) of
                      true -> {ok, {d0, N, T}, R2};
                      false -> none
                  end
          end).

%% week 5 = last, weekday 0 = sunday
rule_m(M, W, D, T, Rest) ->
    case in_range(M, 1, 12) andalso in_range(W, 1, 5) andalso in_range(D, 0, 6) of
        true -> {ok, {m, M, W, D, T}, Rest};
        false -> none
    end.

parse_rule_time("/" ++ R0) ->
    {Sign, R1} = case R0 of
        "-" ++ R -> {-1, R};
        "+" ++ R -> {1, R};
        _ -> {1, R0}
    end,
    case parse_int(R1) of
        {ok, H, R2} ->
            {M, R3} = parse_opt_colon_int(R2),
            {S, R4} = parse_opt_colon_int(R3),
            {Sign * (H * 3600 + M * 60 + S), R4};
        none -> {7200, R0}
    end;
parse_rule_time(R) -> {7200, R}.

-spec offset_at(footer(), integer()) -> integer().
offset_at({fixed, Off}, _Sec) -> Off;
offset_at({dst, Std, Dst, _R1, _R2} = F, Sec) ->
    Y = year_of(Sec + Std),
    Trans = transitions(F, Y - 1, Y + 1),
    Initial = case Trans of
        [{_, First} | _] when First =:= Dst -> Std;
        [{_, _} | _] -> Dst;
        [] -> Std
    end,
    lists:foldl(
      fun({T, Off}, Acc) ->
          case T =< Sec of
              true -> Off;
              false -> Acc
          end
      end, Initial, Trans).

-spec transitions(footer(), integer(), integer()) -> [{integer(), integer()}].
transitions({fixed, _Off}, _FromY, _ToY) -> [];
transitions({dst, Std, Dst, R1, R2}, FromY, ToY) ->
    L = lists:flatmap(
          fun(Y) ->
              [{rule_to_utc(Y, R1, Std), Dst},
               {rule_to_utc(Y, R2, Dst), Std}]
          end, lists:seq(FromY, ToY)),
    lists:keysort(1, L).

rule_to_utc(Y, {m, M, W, D, T}, OffBefore) ->
    Day = month_week_day(Y, M, W, D),
    days_from_epoch(Y, M, Day) * 86400 + T - OffBefore;
rule_to_utc(Y, {j, N, T}, OffBefore) ->
    %% Jn never counts feb 29
    Extra = case N > 59 andalso is_leap(Y) of
        true -> 1;
        false -> 0
    end,
    (days_from_epoch(Y, 1, 1) + N - 1 + Extra) * 86400 + T - OffBefore;
rule_to_utc(Y, {d0, N, T}, OffBefore) ->
    (days_from_epoch(Y, 1, 1) + N) * 86400 + T - OffBefore.

month_week_day(Y, M, W, D) ->
    FirstDow = calendar:day_of_the_week(Y, M, 1) rem 7,
    FirstHit = 1 + ((D - FirstDow + 7) rem 7),
    Cand = FirstHit + (W - 1) * 7,
    Last = calendar:last_day_of_the_month(Y, M),
    case Cand > Last of
        true -> Cand - 7;
        false -> Cand
    end.

is_leap(Y) -> calendar:is_leap_year(Y).

days_from_epoch(Y, M, D) ->
    calendar:date_to_gregorian_days(Y, M, D) - ?EPOCH_DAYS.

-spec year_of(integer()) -> integer().
year_of(Sec) ->
    {{Y, _, _}, _} = calendar:gregorian_seconds_to_datetime(Sec + ?EPOCH_GS),
    Y.
