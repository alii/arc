%% offsets here are local minus utc, posix sign negated at parse
-module(arc_posix_tz).

-export([parse/1, offset_at/2, transitions/3, year_of/1]).

-export_type([posix_tz/0]).

-define(UNIX_EPOCH_GREGORIAN_SECONDS, 62167219200).
-define(UNIX_EPOCH_GREGORIAN_DAYS, 719528).

%% posix Mm.w.d, Jn and n date rules
-type rule() :: {m, 1..12, 1..5, 0..6, integer()}
              | {j, 1..365, integer()}
              | {d0, 0..365, integer()}.

-type posix_tz() :: {fixed, integer()}
                  | {dst, integer(), integer(), rule(), rule()}.

-spec parse(string()) -> posix_tz() | none.
parse(S) ->
    maybe
        {ok, R1} ?= parse_name(S),
        {ok, StdPosix, R2} ?= parse_signed_hms(R1),
        parse_dst(-StdPosix, R2)
    else
        none -> none
    end.

parse_dst(StdOff, S) ->
    case parse_name(S) of
        none -> {fixed, StdOff};
        {ok, R3} ->
            {DstOff, R4} =
                case parse_signed_hms(R3) of
                    {ok, DstPosix, RR} -> {-DstPosix, RR};
                    none -> {StdOff + 3600, R3}
                end,
            parse_dst_rules(StdOff, DstOff, R4)
    end.

parse_dst_rules(StdOff, DstOff, "," ++ R5) ->
    maybe
        {ok, DstStart, "," ++ R6} ?= parse_rule(R5),
        {ok, DstEnd, _} ?= parse_rule(R6),
        {dst, StdOff, DstOff, DstStart, DstEnd}
    else
        _ -> {fixed, StdOff}
    end;
parse_dst_rules(StdOff, DstOff, _) ->
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

%% [+-]h[:m[:s]] in seconds
parse_signed_hms(S) ->
    {Sign, R0} = case S of
        "-" ++ R -> {-1, R};
        "+" ++ R -> {1, R};
        _ -> {1, S}
    end,
    case parse_int(R0) of
        {ok, H, R1} ->
            {M, R2} = parse_opt_colon_int(R1),
            {Sec, R3} = parse_opt_colon_int(R2),
            {ok, Sign * (H * 3600 + M * 60 + Sec), R3};
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
in_range(N, Lo, Hi) -> N >= Lo andalso N =< Hi.

parse_rule("M" ++ R0) ->
    maybe
        {ok, M, "." ++ R1} ?= parse_int(R0),
        {ok, W, "." ++ R2} ?= parse_int(R1),
        {ok, D, R3} ?= parse_int(R2),
        {T, R4} = parse_rule_time(R3),
        nth_weekday_rule(M, W, D, T, R4)
    else
        _ -> none
    end;
parse_rule("J" ++ R0) ->
    maybe
        {ok, N, R1} ?= parse_int(R0),
        {T, R2} = parse_rule_time(R1),
        true ?= in_range(N, 1, 365),
        {ok, {j, N, T}, R2}
    else
        _ -> none
    end;
parse_rule(S) ->
    maybe
        {ok, N, R1} ?= parse_int(S),
        {T, R2} = parse_rule_time(R1),
        true ?= in_range(N, 0, 365),
        {ok, {d0, N, T}, R2}
    else
        _ -> none
    end.

%% week 5 = last, weekday 0 = sunday
nth_weekday_rule(M, W, D, T, Rest) ->
    case in_range(M, 1, 12) andalso in_range(W, 1, 5) andalso in_range(D, 0, 6) of
        true -> {ok, {m, M, W, D, T}, Rest};
        false -> none
    end.

parse_rule_time("/" ++ R0) ->
    case parse_signed_hms(R0) of
        {ok, T, R} -> {T, R};
        none -> {7200, R0}
    end;
parse_rule_time(R) -> {7200, R}.

-spec offset_at(posix_tz(), integer()) -> integer().
offset_at({fixed, Off}, _Sec) -> Off;
offset_at({dst, Std, Dst, _DstStart, _DstEnd} = Tz, Sec) ->
    Y = year_of(Sec + Std),
    Trans = transitions(Tz, Y - 1, Y + 1),
    %% offset in force before the first listed transition
    Initial = case Trans of
        [{_, FirstTarget} | _] when FirstTarget =:= Dst -> Std;
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

-spec transitions(posix_tz(), integer(), integer()) -> [{integer(), integer()}].
transitions({fixed, _Off}, _FromY, _ToY) -> [];
transitions({dst, Std, Dst, DstStart, DstEnd}, FromY, ToY) ->
    L = lists:flatmap(
          fun(Y) ->
              [{rule_to_utc(Y, DstStart, Std), Dst},
               {rule_to_utc(Y, DstEnd, Dst), Std}]
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
    calendar:date_to_gregorian_days(Y, M, D) - ?UNIX_EPOCH_GREGORIAN_DAYS.

-spec year_of(integer()) -> integer().
year_of(Sec) ->
    {{Y, _, _}, _} = calendar:gregorian_seconds_to_datetime(
                       Sec + ?UNIX_EPOCH_GREGORIAN_SECONDS),
    Y.
