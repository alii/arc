%% every offset here is local minus utc, in seconds
-module(arc_tz_ffi).

-export([lookup/1, offset_at/2, next_transition/2, previous_transition/2,
         canonical_id/1, host_zone/0, zone_named/1, utc_zone/0, zone_id/1,
         zone_offset_at_utc_ms/2, zone_offset_at_local_ms/2]).

-export_type([local_zone/0]).

-type tz_error() :: no_zoneinfo | {unreadable, binary()} | {unparseable, binary()}.

-type local_zone() :: {tzif, binary(), arc_tzif:tz()}
                    | {posix, arc_posix_tz:footer()}
                    | none.

-type transition() :: {found, integer()} | no_transition | {load_failed, tz_error()}.

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

-spec offset_at(binary(), integer()) -> {ok, integer()} | {error, tz_error()}.
offset_at(Id, Sec) ->
    case load_zone(Id) of
        {error, Reason} -> {error, Reason};
        {ok, Zone} -> {ok, arc_tzif:offset_at(Zone, Sec)}
    end.

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

-spec host_zone() -> local_zone().
host_zone() ->
    try
        case os:getenv("TZ") of
            false -> detect_localtime_zone();
            Raw -> zone_from_tz_env(Raw)
        end
    %% no prim_file on atomvm: utc
    catch error:undef -> none
    end.

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

-spec utc_zone() -> local_zone().
utc_zone() -> none.

-spec zone_id(local_zone()) -> {ok, binary()} | {error, nil}.
zone_id({tzif, Id, _Tz}) -> {ok, Id};
zone_id(_PosixOrNone) -> {error, nil}.

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

zone_from_timezone_file() ->
    case prim_file:read_file("/etc/timezone") of
        {ok, Bin} -> known_zone(string:trim(binary_to_list(Bin)));
        {error, _NoSuchFile} -> none
    end.

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

zone_from_path(Path) ->
    case string:split(Path, "zoneinfo/", trailing) of
        [_, Id] -> known_zone(Id);
        _ -> none
    end.

zone_from_tz_env(Raw) ->
    Tz = string:trim(Raw, leading, ":"),
    case known_zone(Tz) of
        none -> zone_from_path_or_posix(Tz);
        Zone -> Zone
    end.

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

posix_zone(Tz) ->
    case arc_posix_tz:parse(Tz) of
        none -> none;
        Footer -> {posix, Footer}
    end.

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

safe_zone_name(Name) ->
    valid_zone_name(Name) andalso
        filename:pathtype(Name) =:= relative andalso
        not lists:member("..", filename:split(Name)).

loaded(Id) ->
    case load_zone(Id) of
        {ok, Tz} -> {tzif, Id, Tz};
        {error, _Logged} -> none
    end.

-spec zone_offset_at_utc_ms(local_zone(), integer()) -> integer().
zone_offset_at_utc_ms(none, _EpochMs) -> 0;
zone_offset_at_utc_ms(Zone, EpochMs) when is_integer(EpochMs) ->
    to_minutes(zone_offset(Zone, floor_div(EpochMs, 1000))).

%% §21.4.1.25 localtza, isUTC false
-spec zone_offset_at_local_ms(local_zone(), integer()) -> integer().
zone_offset_at_local_ms(none, _LocalMs) -> 0;
zone_offset_at_local_ms(Zone, LocalMs) when is_integer(LocalMs) ->
    LocalSec = floor_div(LocalMs, 1000),
    Before = zone_offset(Zone, LocalSec - 86400),
    After = zone_offset(Zone, LocalSec + 86400),
    to_minutes(local_offset(Zone, LocalSec, Before, After)).

%% ambiguous or skipped wall times use the offset before
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

%% div truncates; we need floor
floor_div(A, B) ->
    case A rem B =/= 0 andalso (A < 0) =/= (B < 0) of
        true -> A div B - 1;
        false -> A div B
    end.

root() ->
    find_root(["/usr/share/zoneinfo", "/usr/share/lib/zoneinfo",
               "/etc/zoneinfo"]).

find_root([]) -> none;
find_root([D | Rest]) ->
    case prim_file:read_file_info(D) of
        {ok, Info} when element(3, Info) =:= directory -> D;
        _NotADir -> find_root(Rest)
    end.

zi_tables(Root) ->
    Empty = #{names => #{}, links => #{}},
    Path = filename:join(Root, "tzdata.zi"),
    case prim_file:read_file(Path) of
        {ok, Bin} ->
            Lines = binary:split(Bin, <<"\n">>, [global]),
            lists:foldl(fun add_zi_line/2, Empty, Lines);
        {error, enoent} ->
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

add_zi_link(Target, LinkName, Acc0) ->
    Key = string:lowercase(LinkName),
    #{names := Names, links := Links} = Acc = add_zi_name(LinkName, Acc0),
    case maps:is_key(Key, Names) of
        true -> Acc#{links := Links#{Key => Target}};
        false -> Acc
    end.

%% trees without tzdata.zi (macos)
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

-spec detail(io:format(), [term()]) -> binary().
detail(Format, Args) ->
    unicode:characters_to_binary(io_lib:format(Format, Args)).
