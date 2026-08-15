%% IANA time zone database access — the runtime's ONLY time zone engine.
%% Both Temporal (explicit zones) and Date (the host zone) go through here.
%%
%% This module is the public API and the host-zone detection that Date's
%% LocalTZA needs. The two file formats it reads live next door: `arc_tzif`
%% (TZif binaries, RFC 8536) and `arc_posix_tz` (POSIX TZ strings, both as a
%% TZif footer and as a bare $TZ). It answers four questions:
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
%% Nothing here is cached: every function is a plain function of its arguments
%% and the zoneinfo files it reads, read through prim_file so concurrent
%% agents never queue on the file server. A resolved zone is a value
%% (`local_zone()`) the caller keeps and hands back; both runtimes hold the
%% host's on their HostHooks. If no zoneinfo database exists on the host, lookups fail with
%% `no_zoneinfo` and the runtime degrades to UTC + fixed-offset zones only.
-module(arc_tz_ffi).

-export([lookup/1, offset_at/2, next_transition/2, previous_transition/2,
         canonical_id/1, host_zone/0, zone_named/1, utc_zone/0, zone_id/1,
         zone_offset_at_utc_ms/2, zone_offset_at_local_ms/2]).

-export_type([local_zone/0]).

%% Why a zone would not load. `no_zoneinfo` (the host has no tzdata at all) is
%% expected on slim containers; the other two mean the database that *is* there
%% is broken, and callers are meant to be able to tell them apart. The shapes
%% below are the ones temporal_tz.gleam's `TzError` decodes.
-type tz_error() :: no_zoneinfo | {unreadable, binary()} | {unparseable, binary()}.

%% Where local time comes from: a loaded IANA zone, a bare POSIX TZ rule, or
%% nothing at all (in which case local time is UTC).
-type local_zone() :: {tzif, binary(), arc_tzif:tz()}
                    | {posix, arc_posix_tz:footer()}
                    | none.

%% Result of a next/previous transition query.
-type transition() :: {found, integer()} | no_transition | {load_failed, tz_error()}.

%% ----------------------------------------------------------------------
%% Public API
%% ----------------------------------------------------------------------

%% Case-insensitive zone id lookup. {ok, ProperlyCasedId} | {error, nil}.
-spec lookup(binary()) -> {ok, binary()} | {error, nil}.
lookup(Id) when is_binary(Id) ->
    case root() of
        none -> {error, nil};
        Root ->
            case maps:get(names, zi_tables(Root)) of
                Names when map_size(Names) > 0 ->
                    case maps:find(string:lowercase(Id), Names) of
                        {ok, Proper} -> {ok, Proper};
                        error -> {error, nil}
                    end;
                _NoZi -> resolve_in_tree(Root, Id)
            end
    end.

%% UTC offset in seconds at the given epoch second.
%% {error, Reason} when the zone's TZif data cannot be read/parsed; the reason
%% says which, so a missing database and a corrupt one stay distinguishable.
-spec offset_at(binary(), integer()) -> {ok, integer()} | {error, tz_error()}.
offset_at(Id, Sec) ->
    case load_zone(Id) of
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
    case load_zone(Id) of
        {error, Reason} -> {load_failed, Reason};
        {ok, Zone} ->
            case arc_tzif:first_transition_after(Zone, Sec) of
                none -> no_transition;
                T -> {found, T}
            end
    end.

%% Largest transition time T with T < Sec where the UTC offset changes.
%% Same three-way result as next_transition/2.
-spec previous_transition(binary(), integer()) -> transition().
previous_transition(Id, Sec) ->
    case load_zone(Id) of
        {error, Reason} -> {load_failed, Reason};
        {ok, Zone} ->
            case arc_tzif:last_transition_before(Zone, Sec) of
                none -> no_transition;
                T -> {found, T}
            end
    end.

%% Resolve a (properly-cased) zone id through tzdata.zi Link entries to its
%% canonical zone name. Identity for unknown ids or when no link data exists.
-spec canonical_id(binary()) -> binary().
canonical_id(Id) when is_binary(Id) ->
    case root() of
        none -> Id;
        Root -> follow_links(Id, maps:get(links, zi_tables(Root)), 8)
    end.

follow_links(Id, _Links, 0) -> Id;
follow_links(Id, Links, N) ->
    case maps:find(string:lowercase(Id), Links) of
        {ok, Target} -> follow_links(Target, Links, N - 1);
        error -> Id
    end.

%% ----------------------------------------------------------------------
%% Host time zone — Date's LocalTZA (ES2024 §21.4.1.25)
%% ----------------------------------------------------------------------

%% The host's time zone, resolved AND loaded: an IANA zone with its parsed
%% TZif data, a `{posix, Footer}` rule from a bare POSIX TZ string, or `none`
%% when nothing resolves (or the resolved zone's data cannot be loaded), in
%% which case local time simply is UTC. Reads the environment and the
%% zoneinfo tree every time it is called; the caller keeps the result.
%%
%% TZ overrides the host default (as it does for libc, and as node does);
%% otherwise the /etc/localtime chain.
-spec host_zone() -> local_zone().
host_zone() ->
    case os:getenv("TZ") of
        false -> detect_localtime_zone();
        Raw -> zone_from_tz_env(Raw)
    end.

%% A named IANA zone (case-insensitive), loaded. {error, nil} for an unknown
%% id or one whose data cannot be loaded (load_zone/1 logs the latter).
-spec zone_named(binary()) -> {ok, local_zone()} | {error, nil}.
zone_named(Name) when is_binary(Name) ->
    case lookup(Name) of
        {error, nil} -> {error, nil};
        {ok, Id} ->
            case loaded(Id) of
                none -> {error, nil};
                Zone -> {ok, Zone}
            end
    end.

%% The zone whose local time is UTC.
-spec utc_zone() -> local_zone().
utc_zone() -> none.

%% The IANA id of a resolved zone, when it has one.
-spec zone_id(local_zone()) -> {ok, binary()} | {error, nil}.
zone_id({tzif, Id, _Tz}) -> {ok, Id};
zone_id(_PosixOrNone) -> {error, nil}.

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
    case prim_file:read_link_all("/etc/localtime") of
        {ok, Target} -> zone_from_path(Target);
        {error, _NotASymlink} -> none
    end.

%% Debian/Ubuntu record the bare zone id here ("Europe/London\n").
zone_from_timezone_file() ->
    case prim_file:read_file("/etc/timezone") of
        {ok, Bin} -> known_zone(string:trim(binary_to_list(Bin)));
        {error, _NoSuchFile} -> none
    end.

%% Last resort: /etc/localtime is a regular TZif file with no name attached to
%% it. Its bytes identify the zone — find the zoneinfo file that matches.
zone_from_localtime_contents() ->
    case prim_file:read_file("/etc/localtime") of
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
        Root ->
            Names = maps:values(maps:get(names, zi_tables(Root))),
            match_zone_contents(Root, Bin, Names)
    end.

match_zone_contents(_Root, _Bin, []) -> none;
match_zone_contents(Root, Bin, [Id | Rest]) ->
    Path = filename:join(Root, binary_to_list(Id)),
    case prim_file:read_file(Path) of
        {ok, Bin} -> loaded(Id);
        _Miss -> match_zone_contents(Root, Bin, Rest)
    end.

%% ".../zoneinfo/Europe/London" -> the loaded Europe/London zone.
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
        Zone -> Zone
    end.

%% POSIX: TZ="" (or a bare ":") is UTC and says nothing about it.
zone_from_path_or_posix("") -> none;
zone_from_path_or_posix(Tz) ->
    case zone_from_path(Tz) of
        none -> posix_zone_or_warn(Tz);
        Zone -> Zone
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

%% A named host zone must be one we can actually load. The name is taken as
%% spelled (TZ and /etc/localtime carry the tree's own casing, as libc
%% requires), so this is one file probe rather than a walk of the name table.
known_zone("") -> none;
known_zone(Name) ->
    case root() of
        none -> none;
        Root ->
            case safe_zone_name(Name) andalso
                 is_tzif(filename:join(Root, Name)) of
                true -> loaded(unicode:characters_to_binary(Name));
                false -> none
            end
    end.

%% A relative path that stays inside the zoneinfo tree and is not one of the
%% tree's non-zone entries.
safe_zone_name(Name) ->
    valid_zone_name(Name) andalso
        filename:pathtype(Name) =:= relative andalso
        not lists:member("..", filename:split(Name)).

%% The loaded zone for a properly-cased id, `none` when its data cannot be
%% read (load_zone/1 logs anything but a missing database).
loaded(Id) ->
    case load_zone(Id) of
        {ok, Tz} -> {tzif, Id, Tz};
        {error, _Logged} -> none
    end.

%% Local-minus-UTC offset in MINUTES at the UTC instant EpochMs, in Zone.
-spec zone_offset_at_utc_ms(local_zone(), integer()) -> integer().
zone_offset_at_utc_ms(none, _EpochMs) -> 0;
zone_offset_at_utc_ms(Zone, EpochMs) when is_integer(EpochMs) ->
    to_minutes(zone_offset(Zone, floor_div(EpochMs, 1000))).

%% Local-minus-UTC offset in MINUTES for the local wall-clock time LocalMs in
%% Zone (§21.4.1.25 LocalTZA with isUTC = false): a local time that a
%% transition skips or repeats "must be interpreted using the time zone offset
%% before the transition".
-spec zone_offset_at_local_ms(local_zone(), integer()) -> integer().
zone_offset_at_local_ms(none, _LocalMs) -> 0;
zone_offset_at_local_ms(Zone, LocalMs) when is_integer(LocalMs) ->
    LocalSec = floor_div(LocalMs, 1000),
    %% The instant a wall clock names is within a day of itself (all offsets
    %% are < 24h), so the offsets a day either side are the only two
    %% candidates.
    Before = zone_offset(Zone, LocalSec - 86400),
    After = zone_offset(Zone, LocalSec + 86400),
    to_minutes(local_offset(Zone, LocalSec, Before, After)).

%% An offset is a possible reading of the wall clock when the instant it
%% produces really has that offset. Two possible (ambiguous) or none possible
%% (skipped) both resolve to `Before`, the offset before the transition.
local_offset(Zone, LocalSec, Before, After) ->
    case zone_offset(Zone, LocalSec - Before) =:= Before of
        true -> Before;
        false ->
            case zone_offset(Zone, LocalSec - After) =:= After of
                true -> After;
                false -> Before
            end
    end.

zone_offset({posix, Footer}, Sec) -> arc_posix_tz:offset_at(Footer, Sec);
zone_offset({tzif, _Id, Tz}, Sec) -> arc_tzif:offset_at(Tz, Sec).

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

root() ->
    find_root(["/usr/share/zoneinfo", "/usr/share/lib/zoneinfo",
               "/etc/zoneinfo"]).

find_root([]) -> none;
find_root([D | Rest]) ->
    case prim_file:read_file_info(D) of
        {ok, Info} when element(3, Info) =:= directory -> D;
        _NotADir -> find_root(Rest)
    end.

%% Zone names and links both come from tzdata.zi ("Z <name> ..." /
%% "L <target> <name>"), so the file is read and walked exactly once and both
%% tables fall out of the same fold. #{names => #{lower => Proper},
%% links => #{lower => Target}}.
zi_tables(Root) ->
    Empty = #{names => #{}, links => #{}},
    Path = filename:join(Root, "tzdata.zi"),
    case prim_file:read_file(Path) of
        {ok, Bin} ->
            Lines = binary:split(Bin, <<"\n">>, [global]),
            lists:foldl(fun add_zi_line/2, Empty, Lines);
        {error, enoent} ->
            %% Zoneinfo trees without tzdata.zi are normal (macOS);
            %% resolve_in_tree/2 covers them and there are no links.
            Empty;
        {error, Reason} ->
            logger:warning("arc_tz_ffi: cannot read ~ts: ~p", [Path, Reason]),
            Empty
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

%% Zoneinfo trees without tzdata.zi (macOS): resolve the id one path component
%% at a time against the directory listing, case-insensitively, so the answer
%% carries the tree's own casing. A couple of list_dir calls per lookup instead
%% of a TZif-sniffing scan of the whole tree.
resolve_in_tree(Root, Id) ->
    Name = unicode:characters_to_list(Id),
    case safe_zone_name(Name) of
        false -> {error, nil};
        true ->
            case resolve_components(Root, filename:split(Name), []) of
                {ok, Parts} ->
                    Rel = filename:join(Parts),
                    case valid_zone_name(Rel) andalso
                         is_tzif(filename:join(Root, Rel)) of
                        true -> {ok, unicode:characters_to_binary(Rel)};
                        false -> {error, nil}
                    end;
                error -> {error, nil}
            end
    end.

resolve_components(_Dir, [], Acc) -> {ok, lists:reverse(Acc)};
resolve_components(Dir, [Comp | Rest], Acc) ->
    case prim_file:list_dir(Dir) of
        {error, _NotADir} -> error;
        {ok, Entries} ->
            Lower = string:lowercase(Comp),
            case [E || E <- Entries, string:lowercase(E) =:= Lower] of
                [Proper | _] ->
                    resolve_components(filename:join(Dir, Proper), Rest,
                                       [Proper | Acc]);
                [] -> error
            end
    end.

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
%% Zone data
%% ----------------------------------------------------------------------

%% "the host has no zoneinfo database" and "our TZif parser blew up on a file
%% that does exist" are different problems; only the first is expected. Both
%% travel out to the caller as a tz_error(); an unexpected one is logged too.
-spec load_zone(binary()) -> {ok, arc_tzif:tz()} | {error, tz_error()}.
load_zone(Id) ->
    case root() of
        none -> {error, no_zoneinfo};
        Root ->
            Path = filename:join(Root, binary_to_list(Id)),
            case prim_file:read_file(Path) of
                {ok, Bin} ->
                    try {ok, arc_tzif:parse(Bin)}
                    catch Class:Reason:Stack ->
                        logger:warning(
                          "arc_tz_ffi: cannot parse TZif ~ts: ~p:~p~n~p",
                          [Path, Class, Reason, Stack]),
                        %% Reason is often {badmatch, <<the whole file>>}: it
                        %% ends up in a JS RangeError message, so bound it with
                        %% ~P. The log above keeps the full term.
                        {error, {unparseable,
                                 detail("~ts: ~p:~P",
                                        [Path, Class, Reason, 8])}}
                    end;
                {error, enoent} ->
                    {error, {unreadable, detail("~ts: enoent", [Path])}};
                {error, Reason} ->
                    logger:warning("arc_tz_ffi: cannot read ~ts: ~p",
                                   [Path, Reason]),
                    {error, {unreadable,
                             detail("~ts: ~P", [Path, Reason, 8])}}
            end
    end.

%% The human-readable half of a tz_error(): a binary, so Gleam can carry it in
%% a `String` field and put it in the JS error message.
-spec detail(io:format(), [term()]) -> binary().
detail(Format, Args) ->
    unicode:characters_to_binary(io_lib:format(Format, Args)).
