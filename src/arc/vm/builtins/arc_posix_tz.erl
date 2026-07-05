%% POSIX TZ strings ("PST8PDT,M3.2.0,M11.1.0", "<-03>3") — the grammar, and the
%% offsets the parsed rules produce at a given instant.
%%
%% Two callers, one job: the footer of a TZif file (RFC 8536 §3.3, which governs
%% every instant past the file's last recorded transition) and a bare TZ
%% environment variable are the same language, so both go through here.
%%
%% Offsets out of this module are LOCAL MINUS UTC, the opposite of the POSIX
%% string's own sign convention; the negation happens at parse time.
-module(arc_posix_tz).

-export([parse/1, offset_at/2, transitions/3, year_of/1]).

-export_type([footer/0, rule/0]).

-define(EPOCH_GS, 62167219200).  %% gregorian seconds at 1970-01-01T00:00Z
-define(EPOCH_DAYS, 719528).     %% gregorian days at 1970-01-01

%% A rule date: month/week/weekday, Julian day (Feb 29 never counted), or
%% zero-based day of year — each with a local seconds-past-midnight.
-type rule() :: {m, 1..12, 1..5, 0..6, integer()}
              | {j, 1..365, integer()}
              | {d0, 0..365, integer()}.

%% What a parsed TZ string says: a zone that never leaves standard time, or one
%% that switches between two offsets on two rules.
-type footer() :: {fixed, integer()} | {dst, integer(), integer(), rule(), rule()}.

%% ----------------------------------------------------------------------
%% Grammar: "StdName StdOff [DstName [DstOff] [,Rule,Rule]]"
%% ----------------------------------------------------------------------

%% Every sub-parser below answers `{ok, Rest} | none` or `{ok, Value, Rest} |
%% none`; `bind`/`bind3` chain them so a failure short-circuits to `none`
%% instead of another level of nesting.
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

%% Everything after the standard-time name and offset. No DST name at all
%% means the zone never leaves standard time.
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

%% The ",Rule,Rule" tail. A malformed or out-of-range rule degrades the whole
%% footer to a fixed-offset zone rather than being carried into rule
%% evaluation, which happens at query time outside any try/catch.
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
    %% DST named with no rule: use the POSIX default US rule (M3.2.0, M11.1.0).
    {dst, StdOff, DstOff, {m, 3, 2, 0, 7200}, {m, 11, 1, 0, 7200}}.

%% Zone abbreviation: <...> quoted form or a run of letters.
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

%% POSIX offset: [+|-]hh[:mm[:ss]]; returns seconds with the POSIX sign
%% convention (positive = west of Greenwich).
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

%% Rule date: Mm.w.d | Jn | n, optionally followed by /time. Out-of-range
%% components are rejected here, at parse time — footer rules are otherwise
%% only *evaluated* at query time, where a bad month or weekday would crash
%% calendar/1 with no handler.
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
                  %% Jn: 1..365, February 29 never counted.
                  case in_range(N, 1, 365) of
                      true -> {ok, {j, N, T}, R2};
                      false -> none
                  end
          end);
parse_rule(S) ->
    bind3(parse_int(S),
          fun(N, R1) ->
                  {T, R2} = parse_rule_time(R1),
                  %% n: zero-based day of year, 0..365.
                  case in_range(N, 0, 365) of
                      true -> {ok, {d0, N, T}, R2};
                      false -> none
                  end
          end).

%% Mm.w.d: month 1..12, week 1..5 (5 = last), weekday 0..6 (0 = Sunday).
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

%% ----------------------------------------------------------------------
%% Rule evaluation
%% ----------------------------------------------------------------------

%% There is deliberately no clause for a footer that does not exist: `none` is
%% filtered out by every caller, so a new footer shape crashes here rather than
%% quietly claiming "UTC, forever".
-spec offset_at(footer(), integer()) -> integer().
offset_at({fixed, Off}, _Sec) -> Off;
offset_at({dst, Std, Dst, _R1, _R2} = F, Sec) ->
    Y = year_of(Sec + Std),
    Trans = transitions(F, Y - 1, Y + 1),
    %% Offset in effect at Sec: state before the first listed transition is
    %% the opposite of what that transition switches to.
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

%% Sorted [{TransitionSec, NewOffsetSec}] generated from the rule for the
%% given (inclusive) year range. Only a {dst, ...} footer has transitions, and
%% only such a footer is ever passed here.
-spec transitions({dst, integer(), integer(), rule(), rule()},
                  integer(), integer()) -> [{integer(), integer()}].
transitions({dst, Std, Dst, R1, R2}, FromY, ToY) ->
    L = lists:flatmap(
          fun(Y) ->
              [{rule_to_utc(Y, R1, Std), Dst},
               {rule_to_utc(Y, R2, Dst), Std}]
          end, lists:seq(FromY, ToY)),
    lists:keysort(1, L).

%% UTC epoch second at which the rule fires in year Y. The rule time is in
%% local time using the offset in effect before the transition.
rule_to_utc(Y, {m, M, W, D, T}, OffBefore) ->
    Day = month_week_day(Y, M, W, D),
    days_from_epoch(Y, M, Day) * 86400 + T - OffBefore;
rule_to_utc(Y, {j, N, T}, OffBefore) ->
    %% Jn: day N (1-365), February 29 never counted.
    Extra = case N > 59 andalso is_leap(Y) of
        true -> 1;
        false -> 0
    end,
    (days_from_epoch(Y, 1, 1) + N - 1 + Extra) * 86400 + T - OffBefore;
rule_to_utc(Y, {d0, N, T}, OffBefore) ->
    %% n: zero-based day of year, February 29 counted.
    (days_from_epoch(Y, 1, 1) + N) * 86400 + T - OffBefore.

%% Day of month for "week W, weekday D (0=Sunday)" of month M in year Y.
%% W=5 means "last occurrence".
month_week_day(Y, M, W, D) ->
    %% calendar:day_of_the_week: 1=Monday..7=Sunday -> POSIX 0=Sunday.
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

%% The Gregorian year an epoch second falls in — the unit rules are stated in.
-spec year_of(integer()) -> integer().
year_of(Sec) ->
    {{Y, _, _}, _} = calendar:gregorian_seconds_to_datetime(Sec + ?EPOCH_GS),
    Y.
