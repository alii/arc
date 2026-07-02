%% IANA time zone database access for Temporal.
%%
%% Reads binary TZif files (RFC 8536) from the system zoneinfo directory and
%% answers three questions:
%%   * is this a valid zone identifier (case-insensitive), and what is its
%%     properly-cased spelling?
%%   * what is the UTC offset (in seconds) at a given epoch second?
%%   * when is the next/previous UTC-offset transition?
%%
%% Parsed zone data is cached in persistent_term (immutable, read-mostly).
%% If no zoneinfo database exists on the host, lookups simply fail and the
%% runtime degrades to UTC + fixed-offset zones only.
-module(arc_tz_ffi).

-export([lookup/1, offset_at/2, next_transition/2, previous_transition/2,
         canonical_id/1]).

-define(EPOCH_GS, 62167219200).  %% gregorian seconds at 1970-01-01T00:00Z
-define(EPOCH_DAYS, 719528).     %% gregorian days at 1970-01-01

%% ----------------------------------------------------------------------
%% Public API
%% ----------------------------------------------------------------------

%% Case-insensitive zone id lookup. {ok, ProperlyCasedId} | {error, nil}.
lookup(Id) when is_binary(Id) ->
    case maps:find(string:lowercase(Id), names()) of
        {ok, Proper} -> {ok, Proper};
        error -> {error, nil}
    end.

%% UTC offset in seconds at the given epoch second.
%% {ok, OffsetSec} | {error, nil} when the zone is unknown or its TZif data
%% cannot be read/parsed.
offset_at(Id, Sec) ->
    case zone(Id) of
        error -> {error, nil};
        {First, Trans, Footer} ->
            LastT = last_transition_time(Trans),
            UseFooter = Footer =/= none andalso
                (LastT =:= none orelse Sec >= LastT),
            case UseFooter of
                true -> {ok, footer_offset(Footer, Sec)};
                false -> {ok, offset_from_transitions(First, Trans, Sec)}
            end
    end.

%% Smallest transition time T (epoch seconds) with T > Sec where the UTC
%% offset changes. {ok, T} | {error, nil}.
next_transition(Id, Sec) ->
    case zone(Id) of
        error -> {error, nil};
        {_First, Trans, Footer} ->
            LastT = last_transition_time(Trans),
            case [T || {T, _} <- Trans, T > Sec] of
                [T | _] -> {ok, T};
                [] ->
                    case Footer of
                        {dst, _, _, _, _} ->
                            FromY = case LastT of
                                none -> year_of(Sec);
                                L -> max(year_of(Sec), year_of(L))
                            end,
                            Cands = [T || {T, _} <- footer_transitions(Footer, FromY - 1, FromY + 2),
                                          T > Sec,
                                          LastT =:= none orelse T > LastT],
                            case Cands of
                                [] -> {error, nil};
                                _ -> {ok, lists:min(Cands)}
                            end;
                        _ -> {error, nil}
                    end
            end
    end.

%% Largest transition time T with T < Sec where the UTC offset changes.
previous_transition(Id, Sec) ->
    case zone(Id) of
        error -> {error, nil};
        {_First, Trans, Footer} ->
            LastT = last_transition_time(Trans),
            FooterCands =
                case Footer of
                    {dst, _, _, _, _} when LastT =:= none orelse Sec > LastT ->
                        Y = year_of(Sec),
                        [T || {T, _} <- footer_transitions(Footer, Y - 2, Y + 1),
                              T < Sec,
                              LastT =:= none orelse T > LastT];
                    _ -> []
                end,
            case FooterCands of
                [] ->
                    case [T || {T, _} <- Trans, T < Sec] of
                        [] -> {error, nil};
                        Ts -> {ok, lists:max(Ts)}
                    end;
                _ -> {ok, lists:max(FooterCands)}
            end
    end.

%% Resolve a (properly-cased) zone id through tzdata.zi Link entries to its
%% canonical zone name. Identity for unknown ids or when no link data exists.
canonical_id(Id) when is_binary(Id) ->
    follow_links(Id, links(), 8).

follow_links(Id, _Links, 0) -> Id;
follow_links(Id, Links, N) ->
    case maps:find(string:lowercase(Id), Links) of
        {ok, Target} -> follow_links(Target, Links, N - 1);
        error -> Id
    end.

%% ----------------------------------------------------------------------
%% Zone name table
%% ----------------------------------------------------------------------

root() ->
    case persistent_term:get({?MODULE, root}, undefined) of
        undefined ->
            R = find_root(["/usr/share/zoneinfo", "/usr/share/lib/zoneinfo",
                           "/etc/zoneinfo"]),
            persistent_term:put({?MODULE, root}, R),
            R;
        R -> R
    end.

find_root([]) -> none;
find_root([D | Rest]) ->
    case filelib:is_dir(D) of
        true -> D;
        false -> find_root(Rest)
    end.

names() ->
    case persistent_term:get({?MODULE, names}, undefined) of
        undefined ->
            N = build_names(),
            persistent_term:put({?MODULE, names}, N),
            N;
        N -> N
    end.

build_names() ->
    case root() of
        none -> #{};
        Root ->
            %% Prefer deriving names from tzdata.zi (one small file read,
            %% ~2ms) over scanning the whole zoneinfo tree and opening every
            %% file to sniff the TZif magic (~12s cold). The slow scan made
            %% the first lookup miss stall long enough that parallel test
            %% harness workers were killed before the cache was populated.
            %% Zone data itself is still validated lazily in load_zone/1.
            case names_from_zi(Root) of
                {ok, Names} when map_size(Names) > 0 -> Names;
                _ -> names_from_scan(Root)
            end
    end.

%% Zone and link names from tzdata.zi ("Z <name> ..." / "L <target> <name>").
names_from_zi(Root) ->
    Path = filename:join(Root, "tzdata.zi"),
    case file:read_file(Path) of
        {ok, Bin} ->
            Lines = binary:split(Bin, <<"\n">>, [global]),
            {ok, lists:foldl(fun add_zi_name/2, #{}, Lines)};
        {error, Reason} -> {error, Reason}
    end.

add_zi_name(<<"Z ", Rest/binary>>, Acc) ->
    case binary:split(Rest, <<" ">>) of
        [Name | _] -> add_name(Name, Acc);
        _ -> Acc
    end;
add_zi_name(<<"L ", Rest/binary>>, Acc) ->
    case binary:split(Rest, <<" ">>) of
        [_Target, LinkName] -> add_name(LinkName, Acc);
        _ -> Acc
    end;
add_zi_name(_, Acc) -> Acc.

add_name(Name, Acc) ->
    case valid_zone_name(binary_to_list(Name)) of
        true -> Acc#{string:lowercase(Name) => Name};
        false -> Acc
    end.

%% Fallback for zoneinfo databases without tzdata.zi: scan the tree and
%% TZif-sniff each regular file.
names_from_scan(Root) ->
    Files = filelib:wildcard("**", Root),
    lists:foldl(
      fun(F, Acc) ->
          Path = filename:join(Root, F),
          case valid_zone_name(F) andalso filelib:is_regular(Path)
               andalso is_tzif(Path) of
              true ->
                  Bin = unicode:characters_to_binary(F),
                  Acc#{string:lowercase(Bin) => Bin};
              false -> Acc
          end
      end, #{}, Files).

valid_zone_name("posixrules") -> false;
valid_zone_name("Factory") -> false;
valid_zone_name("posix/" ++ _) -> false;
valid_zone_name("right/" ++ _) -> false;
valid_zone_name("SystemV/" ++ _) -> false;
valid_zone_name(_) -> true.

is_tzif(Path) ->
    case file:open(Path, [read, raw, binary]) of
        {ok, Fd} ->
            R = case file:read(Fd, 4) of
                {ok, <<"TZif">>} -> true;
                _ -> false
            end,
            file:close(Fd),
            R;
        {error, _} -> false
    end.

%% ----------------------------------------------------------------------
%% Link table (tzdata.zi)
%% ----------------------------------------------------------------------

links() ->
    case persistent_term:get({?MODULE, links}, undefined) of
        undefined ->
            L = build_links(),
            persistent_term:put({?MODULE, links}, L),
            L;
        L -> L
    end.

build_links() ->
    case root() of
        none -> #{};
        Root ->
            Path = filename:join(Root, "tzdata.zi"),
            case file:read_file(Path) of
                {ok, Bin} ->
                    Lines = binary:split(Bin, <<"\n">>, [global]),
                    lists:foldl(fun add_link_line/2, #{}, Lines);
                {error, _} -> #{}
            end
    end.

add_link_line(<<"L ", Rest/binary>>, Acc) ->
    case binary:split(Rest, <<" ">>) of
        [Target, LinkName] ->
            Acc#{string:lowercase(LinkName) => Target};
        _ -> Acc
    end;
add_link_line(_, Acc) -> Acc.

%% ----------------------------------------------------------------------
%% TZif file parsing
%% ----------------------------------------------------------------------

%% zone(Id) -> {FirstOffsetSec, [{TransitionSec, OffsetSec}], Footer} | error
%% Transitions are ascending and deduplicated so each entry changes the
%% offset. Footer is none | {fixed, OffSec} | {dst, StdOff, DstOff, R1, R2}.
zone(Id) ->
    Key = {?MODULE, zone, Id},
    case persistent_term:get(Key, undefined) of
        undefined ->
            Z = load_zone(Id),
            persistent_term:put(Key, Z),
            Z;
        Z -> Z
    end.

load_zone(Id) ->
    case root() of
        none -> error;
        Root ->
            Path = filename:join(Root, binary_to_list(Id)),
            case file:read_file(Path) of
                {ok, Bin} ->
                    try parse_tzif(Bin)
                    catch _:_ -> error
                    end;
                {error, _} -> error
            end
    end.

parse_tzif(<<"TZif", Ver:8, _:15/binary, IsUt:32, IsStd:32, Leap:32,
             Timecnt:32, Typecnt:32, Charcnt:32, Rest/binary>>) ->
    case Ver of
        0 ->
            %% Version 1: 32-bit data, no footer.
            {First, Trans, _After} =
                parse_block(Rest, Timecnt, Typecnt, Charcnt, 4),
            {First, dedupe(First, Trans), none};
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
            {First, dedupe(First, Trans), parse_footer(FooterBin)}
    end.

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

last_transition_time([]) -> none;
last_transition_time(Trans) -> element(1, lists:last(Trans)).

offset_from_transitions(First, Trans, Sec) ->
    lists:foldl(
      fun({T, Off}, Acc) ->
          case T =< Sec of
              true -> Off;
              false -> Acc
          end
      end, First, Trans).

%% ----------------------------------------------------------------------
%% POSIX TZ footer ("PST8PDT,M3.2.0,M11.1.0")
%% ----------------------------------------------------------------------

parse_footer(<<"\n", Rest/binary>>) ->
    case binary:split(Rest, <<"\n">>) of
        [<<>>, _] -> none;
        [TzStr, _] -> parse_posix(binary_to_list(TzStr));
        _ -> none
    end;
parse_footer(_) -> none.

parse_posix(S) ->
    case parse_name(S) of
        {ok, R1} ->
            case parse_posix_offset(R1) of
                {ok, StdPosix, R2} ->
                    StdOff = -StdPosix,
                    case parse_name(R2) of
                        {ok, R3} ->
                            {DstOff, R4} =
                                case parse_posix_offset(R3) of
                                    {ok, DP, RR} -> {-DP, RR};
                                    none -> {StdOff + 3600, R3}
                                end,
                            case R4 of
                                "," ++ R5 ->
                                    case parse_rule(R5) of
                                        {ok, Rule1, "," ++ R6} ->
                                            case parse_rule(R6) of
                                                {ok, Rule2, _} ->
                                                    {dst, StdOff, DstOff,
                                                     Rule1, Rule2};
                                                _ -> {fixed, StdOff}
                                            end;
                                        _ -> {fixed, StdOff}
                                    end;
                                _ ->
                                    %% DST named with no rule: use the POSIX
                                    %% default US rule (M3.2.0, M11.1.0).
                                    {dst, StdOff, DstOff,
                                     {m, 3, 2, 0, 7200}, {m, 11, 1, 0, 7200}}
                            end;
                        none -> {fixed, StdOff}
                    end;
                none -> none
            end;
        none -> none
    end.

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

%% Rule date: Mm.w.d | Jn | n, optionally followed by /time.
parse_rule("M" ++ R0) ->
    case parse_int(R0) of
        {ok, M, "." ++ R1} ->
            case parse_int(R1) of
                {ok, W, "." ++ R2} ->
                    case parse_int(R2) of
                        {ok, D, R3} ->
                            {T, R4} = parse_rule_time(R3),
                            {ok, {m, M, W, D, T}, R4};
                        none -> none
                    end;
                _ -> none
            end;
        _ -> none
    end;
parse_rule("J" ++ R0) ->
    case parse_int(R0) of
        {ok, N, R1} ->
            {T, R2} = parse_rule_time(R1),
            {ok, {j, N, T}, R2};
        none -> none
    end;
parse_rule(S) ->
    case parse_int(S) of
        {ok, N, R1} ->
            {T, R2} = parse_rule_time(R1),
            {ok, {d0, N, T}, R2};
        none -> none
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
%% Footer rule evaluation
%% ----------------------------------------------------------------------

footer_offset({fixed, Off}, _Sec) -> Off;
footer_offset({dst, Std, Dst, _R1, _R2} = F, Sec) ->
    Y = year_of(Sec + Std),
    Trans = footer_transitions(F, Y - 1, Y + 1),
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
      end, Initial, Trans);
footer_offset(none, _Sec) -> 0.

%% Sorted [{TransitionSec, NewOffsetSec}] generated from the rule for the
%% given (inclusive) year range.
footer_transitions({dst, Std, Dst, R1, R2}, FromY, ToY) ->
    L = lists:flatmap(
          fun(Y) ->
              [{rule_to_utc(Y, R1, Std), Dst},
               {rule_to_utc(Y, R2, Dst), Std}]
          end, lists:seq(FromY, ToY)),
    lists:keysort(1, L);
footer_transitions(_, _, _) -> [].

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

year_of(Sec) ->
    {{Y, _, _}, _} = calendar:gregorian_seconds_to_datetime(Sec + ?EPOCH_GS),
    Y.
