%% IANA time zone database access — the runtime's ONLY time zone engine.
%% Both Temporal (explicit zones) and Date (the host zone) go through here.
%%
%% Reads binary TZif files (RFC 8536) from the system zoneinfo directory and
%% answers four questions:
%%   * is this a valid zone identifier (case-insensitive), and what is its
%%     properly-cased spelling?
%%   * what is the UTC offset (in seconds) at a given epoch second?
%%   * when is the next/previous UTC-offset transition?
%%   * which zone is the host in, and what is its offset at a given UTC
%%     instant / for a given local wall-clock time?
%%
%% Every offset this module reports is LOCAL MINUS UTC (so America/Los_Angeles
%% in winter is -28800 seconds / -480 minutes). JS `getTimezoneOffset` uses the
%% opposite sign; that negation lives at that one call site, not here.
%%
%% Parsed zone data is cached in persistent_term (immutable, read-mostly).
%% If no zoneinfo database exists on the host, lookups simply fail and the
%% runtime degrades to UTC + fixed-offset zones only.
-module(arc_tz_ffi).

-export([lookup/1, offset_at/2, next_transition/2, previous_transition/2,
         canonical_id/1, host_zone/0, offset_at_utc_ms/1,
         offset_at_local_ms/1]).

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
        {error, _} -> {error, nil};
        {ok, {First, Trans, Footer}} ->
            LastT = last_transition_time(Trans),
            UseFooter = Footer =/= none andalso
                (LastT =:= none orelse Sec >= LastT),
            case UseFooter of
                true -> {ok, footer_offset(Footer, Sec)};
                false -> {ok, offset_from_transitions(First, Trans, Sec)}
            end
    end.

%% Smallest transition time T (epoch seconds) with T > Sec where the UTC
%% offset changes.
%%   {found, T}     -- the next transition
%%   no_transition  -- the zone has none after Sec (a `null` result for JS)
%%   unloadable     -- the zone's TZif data could not be read/parsed
%% The last two used to be indistinguishable; they are different bugs.
next_transition(Id, Sec) ->
    case zone(Id) of
        {error, _} -> unloadable;
        {ok, {_First, Trans, Footer}} ->
            LastT = last_transition_time(Trans),
            case [T || {T, _} <- Trans, T > Sec] of
                [T | _] -> {found, T};
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
                                [] -> no_transition;
                                _ -> {found, lists:min(Cands)}
                            end;
                        _ -> no_transition
                    end
            end
    end.

%% Largest transition time T with T < Sec where the UTC offset changes.
%% Same three-way result as next_transition/2.
previous_transition(Id, Sec) ->
    case zone(Id) of
        {error, _} -> unloadable;
        {ok, {_First, Trans, Footer}} ->
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
                        [] -> no_transition;
                        Ts -> {found, lists:max(Ts)}
                    end;
                _ -> {found, lists:max(FooterCands)}
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
%% Host time zone — Date's LocalTZA (ES2024 §21.4.1.25)
%% ----------------------------------------------------------------------

%% The host's IANA zone id, or `none` when it cannot be resolved — in which
%% case the runtime's local time simply is UTC. Cached in persistent_term
%% alongside the zone tables; the host zone cannot change under a live VM.
host_zone() ->
    case persistent_term:get({?MODULE, host_zone}, undefined) of
        undefined ->
            Z = detect_host_zone(),
            persistent_term:put({?MODULE, host_zone}, Z),
            Z;
        Z -> Z
    end.

%% TZ overrides the host default (as it does for libc, and as node does);
%% otherwise /etc/localtime, a symlink into the zoneinfo tree on every platform
%% we support.
detect_host_zone() ->
    case os:getenv("TZ") of
        false -> zone_from_localtime_link();
        Raw -> zone_from_tz_env(Raw)
    end.

zone_from_localtime_link() ->
    case file:read_link_all("/etc/localtime") of
        {ok, Target} -> zone_from_path(Target);
        {error, _} -> none
    end.

%% ".../zoneinfo/Europe/London" -> <<"Europe/London">>.
zone_from_path(Path) ->
    case string:split(Path, "zoneinfo/", trailing) of
        [_, Id] -> known_zone(Id);
        _ -> none
    end.

%% TZ set but empty means UTC (POSIX), and so does a TZ we cannot resolve to a
%% zone we can load — a bare POSIX rule string like "<-03>3" names no zoneinfo
%% file. Either way TZ still wins: we do not silently fall back to the host
%% default the user asked us to override.
zone_from_tz_env(Raw) ->
    %% glibc allows a leading ':' before either a zone name or a path.
    Tz = string:trim(Raw, leading, ":"),
    case known_zone(Tz) of
        none -> zone_from_path(Tz);
        Id -> Id
    end.

%% A host zone must name a zone we can actually load; a POSIX TZ string
%% ("PST8PDT7,M3.2.0") that is not also a zoneinfo file is not one.
known_zone("") -> none;
known_zone(Name) ->
    case lookup(unicode:characters_to_binary(Name)) of
        {ok, Id} -> Id;
        {error, nil} -> none
    end.

%% Local-minus-UTC offset in MINUTES at the UTC instant EpochMs.
%% 0 when the host zone is unresolvable or its data cannot be loaded (the
%% failure is logged by load_zone/1 for anything but a missing database).
offset_at_utc_ms(EpochMs) when is_integer(EpochMs) ->
    case host_zone() of
        none -> 0;
        Zone -> to_minutes(host_offset(Zone, floor_div(EpochMs, 1000)))
    end.

%% Local-minus-UTC offset in MINUTES for the local wall-clock time LocalMs
%% (§21.4.1.25 LocalTZA with isUTC = false): a local time that a transition
%% skips or repeats "must be interpreted using the time zone offset before the
%% transition".
offset_at_local_ms(LocalMs) when is_integer(LocalMs) ->
    case host_zone() of
        none -> 0;
        Zone ->
            LocalSec = floor_div(LocalMs, 1000),
            %% The instant a wall clock names is within a day of itself (all
            %% offsets are < 24h), so the offsets a day either side are the
            %% only two candidates.
            Before = host_offset(Zone, LocalSec - 86400),
            After = host_offset(Zone, LocalSec + 86400),
            to_minutes(local_offset(Zone, LocalSec, Before, After))
    end.

%% An offset is a possible reading of the wall clock when the instant it
%% produces really has that offset. Two possible (ambiguous) or none possible
%% (skipped) both resolve to `Before`, the offset before the transition.
local_offset(_Zone, _LocalSec, none, _After) -> none;
local_offset(_Zone, _LocalSec, _Before, none) -> none;
local_offset(Zone, LocalSec, Before, After) ->
    case is_possible(Zone, LocalSec, Before) of
        true -> Before;
        false ->
            case is_possible(Zone, LocalSec, After) of
                true -> After;
                false -> Before
            end
    end.

is_possible(Zone, LocalSec, Off) ->
    host_offset(Zone, LocalSec - Off) =:= Off.

host_offset(Zone, Sec) ->
    case offset_at(Zone, Sec) of
        {ok, Off} -> Off;
        {error, nil} -> none
    end.

to_minutes(none) -> 0;
to_minutes(OffSec) -> floor_div(OffSec, 60).

%% Erlang's `div` truncates toward zero; every division here is of a signed
%% instant/offset by a positive unit and wants floor.
floor_div(A, B) ->
    case A rem B =/= 0 andalso (A < 0) =/= (B < 0) of
        true -> A div B - 1;
        false -> A div B
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

%% zone(Id) -> {ok, {FirstOffsetSec, [{TransitionSec, OffsetSec}], Footer}}
%%           | {error, Reason}
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

%% {ok, Zone} | {error, no_zoneinfo} | {error, {read, Reason}}
%%           | {error, {parse, Class, Reason}}
%% "the host has no zoneinfo database" and "our TZif parser blew up on a file
%% that does exist" are different problems; only the first is expected. Any
%% unexpected reason is logged rather than silently swallowed.
load_zone(Id) ->
    case root() of
        none -> {error, no_zoneinfo};
        Root ->
            Path = filename:join(Root, binary_to_list(Id)),
            case file:read_file(Path) of
                {ok, Bin} ->
                    try {ok, parse_tzif(Bin)}
                    catch Class:Reason:Stack ->
                        logger:warning(
                          "arc_tz_ffi: cannot parse TZif ~ts: ~p:~p~n~p",
                          [Path, Class, Reason, Stack]),
                        {error, {parse, Class, Reason}}
                    end;
                {error, enoent} -> {error, {read, enoent}};
                {error, Reason} ->
                    logger:warning("arc_tz_ffi: cannot read ~ts: ~p",
                                   [Path, Reason]),
                    {error, {read, Reason}}
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

%% Every sub-parser below answers `{ok, Rest} | none` or `{ok, Value, Rest} |
%% none`; `bind`/`bind3` chain them so a failure short-circuits to `none`
%% instead of another level of nesting.
bind(none, _F) -> none;
bind({ok, Rest}, F) -> F(Rest).

bind3(none, _F) -> none;
bind3({ok, V, Rest}, F) -> F(V, Rest).

%% "StdName StdOff [DstName [DstOff] [,Rule,Rule]]".
parse_posix(S) ->
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
%% components are rejected here, at parse time and inside load_zone/1's
%% try/catch — footer rules are otherwise only *evaluated* at query time,
%% where a bad month or weekday would crash calendar/1 with no handler.
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
