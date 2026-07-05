%% IANA time zone database access — the runtime's ONLY time zone engine.
%% Both Temporal (explicit zones) and Date (the host zone) go through here.
%%
%% This module is the public API, the caches behind it, and the host-zone
%% detection that Date's LocalTZA needs. The two file formats it reads live
%% next door: `arc_tzif` (TZif binaries, RFC 8536) and `arc_posix_tz` (POSIX TZ
%% strings, both as a TZif footer and as a bare $TZ). It answers four questions:
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
%% If no zoneinfo database exists on the host, lookups fail with `no_zoneinfo`
%% and the runtime degrades to UTC + fixed-offset zones only.
-module(arc_tz_ffi).

-export([lookup/1, offset_at/2, next_transition/2, previous_transition/2,
         canonical_id/1, host_zone/0, offset_at_utc_ms/1,
         offset_at_local_ms/1]).

%% Why a zone would not load. `no_zoneinfo` (the host has no tzdata at all) is
%% expected on slim containers; the other two mean the database that *is* there
%% is broken, and callers are meant to be able to tell them apart. The shapes
%% below are the ones temporal_tz.gleam's `TzError` decodes.
-type tz_error() :: no_zoneinfo | {unreadable, binary()} | {unparseable, binary()}.

%% Where the host's local time comes from: a validated IANA id, a bare POSIX TZ
%% rule, or nothing at all (in which case local time is UTC).
-type host_zone() :: binary() | {posix, arc_posix_tz:footer()} | none.

%% Result of a next/previous transition query.
-type transition() :: {found, integer()} | no_transition | {load_failed, tz_error()}.

%% ----------------------------------------------------------------------
%% Public API
%% ----------------------------------------------------------------------

%% Case-insensitive zone id lookup. {ok, ProperlyCasedId} | {error, nil}.
-spec lookup(binary()) -> {ok, binary()} | {error, nil}.
lookup(Id) when is_binary(Id) ->
    case maps:find(string:lowercase(Id), names()) of
        {ok, Proper} -> {ok, Proper};
        error -> {error, nil}
    end.

%% UTC offset in seconds at the given epoch second.
%% {error, Reason} when the zone's TZif data cannot be read/parsed; the reason
%% says which, so a missing database and a corrupt one stay distinguishable.
-spec offset_at(binary(), integer()) -> {ok, integer()} | {error, tz_error()}.
offset_at(Id, Sec) ->
    case zone(Id) of
        {error, Reason} -> {error, Reason};
        {ok, Zone} -> {ok, arc_tzif:offset_at(Zone, Sec)}
    end.

%% Smallest transition time T (epoch seconds) with T > Sec where the UTC
%% offset changes.
%%   {found, T}            -- the next transition
%%   no_transition         -- the zone has none after Sec (a `null` for JS)
%%   {load_failed, Reason} -- the zone's TZif data could not be read/parsed
%% The last two used to be indistinguishable; they are different bugs.
-spec next_transition(binary(), integer()) -> transition().
next_transition(Id, Sec) ->
    case zone(Id) of
        {error, Reason} -> {load_failed, Reason};
        {ok, Zone} ->
            case arc_tzif:first_transition_after(Zone, Sec) of
                none -> footer_next(arc_tzif:footer(Zone),
                                    arc_tzif:last_transition(Zone), Sec);
                T -> {found, T}
            end
    end.

%% Past the last recorded transition the footer rule generates them; look a
%% couple of years either side of Sec for the earliest one it fires after Sec.
footer_next({dst, _, _, _, _} = Footer, LastT, Sec) ->
    FromY = case LastT of
        none -> arc_posix_tz:year_of(Sec);
        L -> max(arc_posix_tz:year_of(Sec), arc_posix_tz:year_of(L))
    end,
    Cands = [T || {T, _} <- arc_posix_tz:transitions(Footer, FromY - 1, FromY + 2),
                  T > Sec,
                  LastT =:= none orelse T > LastT],
    case Cands of
        [] -> no_transition;
        _ -> {found, lists:min(Cands)}
    end;
footer_next(_NoRule, _LastT, _Sec) -> no_transition.

%% Largest transition time T with T < Sec where the UTC offset changes.
%% Same three-way result as next_transition/2.
-spec previous_transition(binary(), integer()) -> transition().
previous_transition(Id, Sec) ->
    case zone(Id) of
        {error, Reason} -> {load_failed, Reason};
        {ok, Zone} ->
            case footer_previous(arc_tzif:footer(Zone),
                                 arc_tzif:last_transition(Zone), Sec) of
                [] ->
                    case arc_tzif:last_transition_before(Zone, Sec) of
                        none -> no_transition;
                        T -> {found, T}
                    end;
                Cands -> {found, lists:max(Cands)}
            end
    end.

%% Footer-generated transitions before Sec that postdate the recorded ones.
footer_previous({dst, _, _, _, _} = Footer, LastT, Sec)
  when LastT =:= none orelse Sec > LastT ->
    Y = arc_posix_tz:year_of(Sec),
    [T || {T, _} <- arc_posix_tz:transitions(Footer, Y - 2, Y + 1),
          T < Sec,
          LastT =:= none orelse T > LastT];
footer_previous(_NoRule, _LastT, _Sec) -> [].

%% Resolve a (properly-cased) zone id through tzdata.zi Link entries to its
%% canonical zone name. Identity for unknown ids or when no link data exists.
-spec canonical_id(binary()) -> binary().
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

%% The host's time zone: an IANA zone id, a `{posix, Footer}` rule from a bare
%% POSIX TZ string, or `none` when nothing resolves — in which case the
%% runtime's local time simply is UTC. Cached in persistent_term alongside the
%% zone tables; the host zone cannot change under a live VM.
-spec host_zone() -> host_zone().
host_zone() ->
    cached(host_zone, fun detect_host_zone/0).

%% TZ overrides the host default (as it does for libc, and as node does);
%% otherwise the /etc/localtime chain.
detect_host_zone() ->
    case os:getenv("TZ") of
        false -> detect_localtime_zone();
        Raw -> zone_from_tz_env(Raw)
    end.

%% /etc/localtime is a symlink into the zoneinfo tree on most hosts, but a
%% plain copy of the TZif file on plenty of others (`cp` installs, RHEL,
%% `docker run -v /etc/localtime:/etc/localtime`). Falling straight to UTC
%% there would silently report the wrong local time, so try, in order: the
%% symlink target, /etc/timezone (Debian), and finally the bytes of
%% /etc/localtime matched against the zoneinfo tree.
detect_localtime_zone() ->
    first_resolved([fun zone_from_localtime_link/0,
                    fun zone_from_timezone_file/0,
                    fun zone_from_localtime_contents/0]).

first_resolved([]) ->
    warn_unresolved_host_zone(),
    none;
first_resolved([Resolve | Rest]) ->
    case Resolve() of
        none -> first_resolved(Rest);
        Zone -> Zone
    end.

%% A host with no zoneinfo database at all is legitimately UTC-only; a host
%% that has one and still cannot say which zone it is in is a misconfiguration
%% worth surfacing rather than quietly answering UTC.
warn_unresolved_host_zone() ->
    case root() of
        none -> ok;
        Root ->
            logger:warning(
              "arc_tz_ffi: cannot determine host time zone (zoneinfo at ~ts, "
              "no TZ, no /etc/localtime symlink, no /etc/timezone, no content "
              "match); local time will be UTC", [Root])
    end.

zone_from_localtime_link() ->
    case file:read_link_all("/etc/localtime") of
        {ok, Target} -> zone_from_path(Target);
        {error, _NotASymlink} -> none
    end.

%% Debian/Ubuntu record the bare zone id here ("Europe/London\n").
zone_from_timezone_file() ->
    case file:read_file("/etc/timezone") of
        {ok, Bin} -> known_zone(string:trim(binary_to_list(Bin)));
        {error, _NoSuchFile} -> none
    end.

%% Last resort: /etc/localtime is a regular TZif file with no name attached to
%% it. Its bytes identify the zone — find the zoneinfo file that matches.
zone_from_localtime_contents() ->
    case file:read_file("/etc/localtime") of
        {ok, Bin} -> zone_with_contents(Bin);
        {error, enoent} -> none;
        {error, Reason} ->
            logger:warning("arc_tz_ffi: cannot read /etc/localtime: ~p",
                           [Reason]),
            none
    end.

zone_with_contents(Bin) ->
    case root() of
        none -> none;
        Root -> match_zone_contents(Root, Bin, maps:values(names()))
    end.

match_zone_contents(_Root, _Bin, []) -> none;
match_zone_contents(Root, Bin, [Id | Rest]) ->
    Path = filename:join(Root, binary_to_list(Id)),
    case file:read_file(Path) of
        {ok, Bin} -> Id;
        _Miss -> match_zone_contents(Root, Bin, Rest)
    end.

%% ".../zoneinfo/Europe/London" -> <<"Europe/London">>.
zone_from_path(Path) ->
    case string:split(Path, "zoneinfo/", trailing) of
        [_, Id] -> known_zone(Id);
        _ -> none
    end.

%% TZ set but empty means UTC (POSIX), silently. Anything else names either a
%% zone we can load, a path to one, or a bare POSIX rule ("<-03>3",
%% "PST8PDT,M3.2.0") that libc would honour and so do we — and a TZ we cannot
%% resolve at all is a typo the user wants to hear about, not a silent slide to
%% UTC. Either way TZ wins: we never fall back to the host default the user
%% asked us to override.
zone_from_tz_env(Raw) ->
    %% glibc allows a leading ':' before either a zone name or a path.
    Tz = string:trim(Raw, leading, ":"),
    case known_zone(Tz) of
        none -> zone_from_path_or_posix(Tz);
        Id -> Id
    end.

%% POSIX: TZ="" (or a bare ":") is UTC and says nothing about it.
zone_from_path_or_posix("") -> none;
zone_from_path_or_posix(Tz) ->
    case zone_from_path(Tz) of
        none -> posix_zone_or_warn(Tz);
        Id -> Id
    end.

posix_zone_or_warn(Tz) ->
    case posix_zone(Tz) of
        none ->
            logger:warning(
              "arc_tz_ffi: TZ=~ts is not a known zone, a path into a zoneinfo "
              "tree, or a POSIX TZ rule; local time will be UTC", [Tz]),
            none;
        Zone -> Zone
    end.

%% A POSIX TZ rule string is a complete zone definition on its own: keep it as
%% a synthetic zone whose offsets come from arc_posix_tz:offset_at/2, exactly as
%% they would past the last transition of a real zone.
posix_zone(Tz) ->
    case arc_posix_tz:parse(Tz) of
        none -> none;
        Footer -> {posix, Footer}
    end.

%% A named host zone must be one we can actually load.
known_zone("") -> none;
known_zone(Name) ->
    case lookup(unicode:characters_to_binary(Name)) of
        {ok, Id} -> Id;
        {error, nil} -> none
    end.

%% Local-minus-UTC offset in MINUTES at the UTC instant EpochMs.
%% 0 when the host zone is unresolvable or its data cannot be loaded (the
%% failure is logged by load_zone/1 for anything but a missing database).
-spec offset_at_utc_ms(integer()) -> integer().
offset_at_utc_ms(EpochMs) when is_integer(EpochMs) ->
    case host_zone() of
        none -> 0;
        Zone -> to_minutes(host_offset(Zone, floor_div(EpochMs, 1000)))
    end.

%% Local-minus-UTC offset in MINUTES for the local wall-clock time LocalMs
%% (§21.4.1.25 LocalTZA with isUTC = false): a local time that a transition
%% skips or repeats "must be interpreted using the time zone offset before the
%% transition".
-spec offset_at_local_ms(integer()) -> integer().
offset_at_local_ms(LocalMs) when is_integer(LocalMs) ->
    case host_zone() of
        none -> 0;
        Zone -> to_minutes(memo(local, Zone, floor_div(LocalMs, 1000),
                                fun resolve_local_offset/2))
    end.

resolve_local_offset(Zone, LocalSec) ->
    %% The instant a wall clock names is within a day of itself (all offsets
    %% are < 24h), so the offsets a day either side are the only two
    %% candidates.
    Before = host_offset(Zone, LocalSec - 86400),
    After = host_offset(Zone, LocalSec + 86400),
    local_offset(Zone, LocalSec, Before, After).

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
    memo(utc, Zone, Sec, fun resolve_host_offset/2).

resolve_host_offset({posix, Footer}, Sec) -> arc_posix_tz:offset_at(Footer, Sec);
resolve_host_offset(Zone, Sec) ->
    case offset_at(Zone, Sec) of
        {ok, Off} -> Off;
        %% load_zone/1 has already logged anything but a missing database.
        {error, _Reason} -> none
    end.

%% Per-instant offset memo, in the process dictionary. Date getters ask for the
%% same instant over and over — `d.getFullYear(); d.getMonth(); d.getDate()` is
%% three lookups of one epoch second, and every wall-clock resolution probes
%% four instants — so an offset search per call is exactly what V8's DateCache
%% and its SunSpider-era dst-offset-caching tests exist to avoid. Bounded, and
%% dropped wholesale when full: the working set of a date-heavy program is
%% small and clustered, so a cleared cache refills immediately.
-define(MEMO_LIMIT, 512).

memo(Kind, Zone, Sec, Resolve) ->
    Key = {Kind, Zone, Sec},
    case memo_cache() of
        #{Key := Off} -> Off;
        _ ->
            Off = Resolve(Zone, Sec),
            %% Resolve may have memoized on its own (a wall-clock lookup probes
            %% instants), so re-read rather than write back a stale snapshot.
            Cache = memo_cache(),
            Kept = case map_size(Cache) >= ?MEMO_LIMIT of
                true -> #{};
                false -> Cache
            end,
            put({?MODULE, memo}, Kept#{Key => Off}),
            Off
    end.

memo_cache() ->
    case get({?MODULE, memo}) of
        undefined -> #{};
        Cache -> Cache
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
%% Zone name and link tables (one tzdata.zi read)
%% ----------------------------------------------------------------------

%% Read-mostly tables live in persistent_term, computed on first use.
cached(Key, Build) ->
    case persistent_term:get({?MODULE, Key}, undefined) of
        undefined ->
            V = Build(),
            persistent_term:put({?MODULE, Key}, V),
            V;
        V -> V
    end.

root() ->
    cached(root, fun() ->
        find_root(["/usr/share/zoneinfo", "/usr/share/lib/zoneinfo",
                   "/etc/zoneinfo"])
    end).

find_root([]) -> none;
find_root([D | Rest]) ->
    case filelib:is_dir(D) of
        true -> D;
        false -> find_root(Rest)
    end.

names() ->
    cached(names, fun build_names/0).

%% "Asia/Calcutta" -> "Asia/Kolkata", from tzdata.zi's Link lines.
links() ->
    maps:get(links, zi_tables()).

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
            case maps:get(names, zi_tables()) of
                Names when map_size(Names) > 0 -> Names;
                _Empty -> names_from_scan(Root)
            end
    end.

%% Zone names and links both come from tzdata.zi ("Z <name> ..." /
%% "L <target> <name>"), so the file is read and walked exactly once and both
%% tables fall out of the same fold. #{names => #{lower => Proper},
%% links => #{lower => Target}}.
zi_tables() ->
    cached(zi_tables, fun build_zi_tables/0).

build_zi_tables() ->
    Empty = #{names => #{}, links => #{}},
    case root() of
        none -> Empty;
        Root ->
            Path = filename:join(Root, "tzdata.zi"),
            case file:read_file(Path) of
                {ok, Bin} ->
                    Lines = binary:split(Bin, <<"\n">>, [global]),
                    lists:foldl(fun add_zi_line/2, Empty, Lines);
                {error, enoent} ->
                    %% Zoneinfo trees without tzdata.zi are normal (macOS);
                    %% names_from_scan/1 covers them and there are no links.
                    Empty;
                {error, Reason} ->
                    logger:warning("arc_tz_ffi: cannot read ~ts: ~p",
                                   [Path, Reason]),
                    Empty
            end
    end.

add_zi_line(<<"Z ", Rest/binary>>, Acc) ->
    case binary:split(Rest, <<" ">>) of
        [Name | _] -> add_zi_name(Name, Acc);
        _NoFields -> Acc
    end;
add_zi_line(<<"L ", Rest/binary>>, Acc) ->
    case binary:split(Rest, <<" ">>) of
        [Target, LinkName] -> add_zi_link(Target, LinkName, Acc);
        _NoLinkName -> Acc
    end;
add_zi_line(_Other, Acc) -> Acc.

add_zi_name(Name, #{names := Names} = Acc) ->
    case valid_zone_name(binary_to_list(Name)) of
        true -> Acc#{names := Names#{string:lowercase(Name) => Name}};
        false -> Acc
    end.

%% A link is a name too: "Asia/Calcutta" is a zone you can ask for. A link name
%% the name table rejects gets no link entry either — the two agree by
%% construction, where the two old parses could not.
add_zi_link(Target, LinkName, Acc0) ->
    Key = string:lowercase(LinkName),
    #{names := Names, links := Links} = Acc = add_zi_name(LinkName, Acc0),
    case maps:is_key(Key, Names) of
        true -> Acc#{links := Links#{Key => Target}};
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
                _NotTzif -> false
            end,
            file:close(Fd),
            R;
        {error, _CannotOpen} -> false
    end.

%% ----------------------------------------------------------------------
%% Zone data cache
%% ----------------------------------------------------------------------

%% The parsed zone, cached forever. See arc_tzif:tz() for the shape.
-spec zone(binary()) -> {ok, arc_tzif:tz()} | {error, tz_error()}.
zone(Id) ->
    Key = {?MODULE, zone, Id},
    case persistent_term:get(Key, undefined) of
        undefined ->
            Z = load_zone(Id),
            persistent_term:put(Key, Z),
            Z;
        Z -> Z
    end.

%% "the host has no zoneinfo database" and "our TZif parser blew up on a file
%% that does exist" are different problems; only the first is expected. Both
%% travel out to the caller as a tz_error(); an unexpected one is logged too.
-spec load_zone(binary()) -> {ok, arc_tzif:tz()} | {error, tz_error()}.
load_zone(Id) ->
    case root() of
        none -> {error, no_zoneinfo};
        Root ->
            Path = filename:join(Root, binary_to_list(Id)),
            case file:read_file(Path) of
                {ok, Bin} ->
                    try {ok, arc_tzif:parse(Bin)}
                    catch Class:Reason:Stack ->
                        logger:warning(
                          "arc_tz_ffi: cannot parse TZif ~ts: ~p:~p~n~p",
                          [Path, Class, Reason, Stack]),
                        {error, {unparseable,
                                 detail("~ts: ~p:~p", [Path, Class, Reason])}}
                    end;
                {error, enoent} ->
                    {error, {unreadable, detail("~ts: enoent", [Path])}};
                {error, Reason} ->
                    logger:warning("arc_tz_ffi: cannot read ~ts: ~p",
                                   [Path, Reason]),
                    {error, {unreadable, detail("~ts: ~p", [Path, Reason])}}
            end
    end.

%% The human-readable half of a tz_error(): a binary, so Gleam can carry it in
%% a `String` field and put it in the JS error message.
-spec detail(io:format(), [term()]) -> binary().
detail(Format, Args) ->
    unicode:characters_to_binary(io_lib:format(Format, Args)).
