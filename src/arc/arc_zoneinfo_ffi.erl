%% the os tz database; arc's only disk read, opted into through arc/zoneinfo
-module(arc_zoneinfo_ffi).

-export([load/1, available_ids/0, system_time_zone/0, time_zone_named/1]).

-include_lib("kernel/include/file.hrl").

-type tz_error() :: no_zoneinfo | {unreadable, binary()} | {unparseable, binary()}.

%% rules for a canonical id
-spec load(binary()) -> {ok, arc_tzif:tz()} | {error, tz_error()}.
load(Id) when is_binary(Id) ->
    case root() of
        none -> {error, no_zoneinfo};
        Root ->
            Path = filename:join(Root, binary_to_list(Id)),
            case prim_file:read_file(Path) of
                {ok, Bin} ->
                    try {ok, arc_tzif:parse(Bin)}
                    catch Class:Reason ->
                        {error, {unparseable,
                                 detail("~ts: ~p:~P", [Path, Class, Reason, 8])}}
                    end;
                {error, Reason} ->
                    {error, {unreadable, detail("~ts: ~P", [Path, Reason, 8])}}
            end
    end.

%% bundled zone names that have a file here
-spec available_ids() -> [binary()].
available_ids() ->
    case root() of
        none -> [];
        Root -> [Z || Z <- arc_tz_links_ffi:primary_zones(), is_file(Root, Z)]
    end.

%% tz env var, else /etc/localtime, else /etc/timezone, else utc
-spec system_time_zone() -> arc_tz_ffi:local_zone().
system_time_zone() ->
    case detected_zone() of
        none -> arc_tz_ffi:utc_time_zone();
        Zone -> Zone
    end.

detected_zone() ->
    try
        case os:getenv("TZ") of
            false ->
                first_resolved([fun zone_from_localtime_link/0,
                                fun zone_from_timezone_file/0,
                                fun zone_from_localtime_contents/0]);
            Raw -> zone_from_tz_env(Raw)
        end
    %% no prim_file or os on atomvm
    catch error:undef -> none
    end.

-spec time_zone_named(binary()) -> {ok, arc_tz_ffi:local_zone()} | {error, nil}.
time_zone_named(Name) when is_binary(Name) ->
    case arc_tz_ffi:known_identifier(Name) of
        none -> {error, nil};
        {some, Id} ->
            case zone_for_id(Id) of
                none -> {error, nil};
                Zone -> {ok, Zone}
            end
    end.

zone_for_id(Id) ->
    case load(arc_tz_ffi:canonical_id(Id)) of
        {ok, Tz} -> arc_tz_ffi:tzif_zone(Id, Tz);
        {error, _NoData} -> none
    end.

first_resolved([]) -> none;
first_resolved([Resolve | Rest]) ->
    case Resolve() of
        none -> first_resolved(Rest);
        Zone -> Zone
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
        {error, _Unreadable} -> none
    end.

zone_with_contents(Bin) ->
    case root() of
        none -> none;
        Root -> match_zone_contents(Root, Bin, arc_tz_links_ffi:primary_zones())
    end.

match_zone_contents(_Root, _Bin, []) -> none;
match_zone_contents(Root, Bin, [Id | Rest]) ->
    Path = filename:join(Root, binary_to_list(Id)),
    case prim_file:read_file(Path) of
        {ok, Contents} when Contents =:= Bin -> zone_for_id(Id);
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
        none ->
            case arc_tz_ffi:posix_zone(unicode:characters_to_binary(Tz)) of
                {some, Zone} -> Zone;
                none -> none
            end;
        Zone -> Zone
    end.

known_zone("") -> none;
known_zone(Name) ->
    case arc_tz_ffi:known_identifier(unicode:characters_to_binary(Name)) of
        {some, Id} -> zone_for_id(Id);
        none -> host_only_zone(Name)
    end.

%% a zone the os ships but the bundled table doesn't know
host_only_zone(Name) ->
    case root() of
        none -> none;
        Root ->
            case safe_zone_name(Name) andalso
                 is_tzif(filename:join(Root, Name)) of
                true -> zone_for_id(unicode:characters_to_binary(Name));
                false -> none
            end
    end.

safe_zone_name(Name) ->
    valid_zone_name(Name) andalso
        filename:pathtype(Name) =:= relative andalso
        not lists:member("..", filename:split(Name)).

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

is_file(Root, Id) ->
    case prim_file:read_file_info(filename:join(Root, binary_to_list(Id))) of
        {ok, #file_info{type = regular}} -> true;
        _NotAFile -> false
    end.

root() ->
    find_root(["/usr/share/zoneinfo", "/usr/share/lib/zoneinfo",
               "/etc/zoneinfo"]).

find_root([]) -> none;
find_root([D | Rest]) ->
    case prim_file:read_file_info(D) of
        {ok, #file_info{type = directory}} -> D;
        _NotADir -> find_root(Rest)
    end.

-spec detail(io:format(), [term()]) -> binary().
detail(Format, Args) ->
    unicode:characters_to_binary(io_lib:format(Format, Args)).
