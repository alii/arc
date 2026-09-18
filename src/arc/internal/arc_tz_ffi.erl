%% every offset here is local minus utc, in seconds; no io, see arc_zoneinfo_ffi
-module(arc_tz_ffi).

-export([lookup/1, canonical_id/1, rules_offset_at/2, rules_next_transition/2,
         rules_previous_transition/2, utc_time_zone/0, tzif_zone/2,
         posix_zone/1, time_zone_id/1, zone_offset_at_utc_ms/2,
         zone_offset_at_local_ms/2]).

-export_type([local_zone/0]).

-type local_zone() :: {tzif, binary(), arc_tzif:tz()}
                    | {posix, arc_posix_tz:posix_tz()}
                    | none.

-spec lookup(binary()) -> {ok, binary()} | {error, nil}.
lookup(Id) when is_binary(Id) ->
    case maps:find(ascii_lowercase(Id), arc_tz_links_ffi:names_by_lowercase()) of
        {ok, Proper} -> {ok, Proper};
        error -> {error, nil}
    end.

ascii_lowercase(Bin) -> << <<(ascii_lower(C))>> || <<C>> <= Bin >>.

ascii_lower(C) when C >= $A, C =< $Z -> C + 32;
ascii_lower(C) -> C.

-spec canonical_id(binary()) -> binary().
canonical_id(Id) when is_binary(Id) ->
    Proper = case lookup(Id) of
        {ok, P} -> P;
        {error, nil} -> Id
    end,
    maps:get(Proper, arc_tz_links_ffi:links(), Proper).

-spec rules_offset_at(arc_tzif:tz(), integer()) -> integer().
rules_offset_at(Rules, Sec) -> arc_tzif:offset_at(Rules, Sec).

-spec rules_next_transition(arc_tzif:tz(), integer()) -> {some, integer()} | none.
rules_next_transition(Rules, Sec) ->
    arc_tzif:next_transition(Rules, Sec).

-spec rules_previous_transition(arc_tzif:tz(), integer()) -> {some, integer()} | none.
rules_previous_transition(Rules, Sec) ->
    arc_tzif:previous_transition(Rules, Sec).

-spec utc_time_zone() -> local_zone().
utc_time_zone() -> none.

-spec tzif_zone(binary(), arc_tzif:tz()) -> local_zone().
tzif_zone(Id, Rules) -> {tzif, Id, Rules}.

%% a posix tz rule string such as "EST5EDT,M3.2.0,M11.1.0"
-spec posix_zone(binary()) -> {ok, local_zone()} | {error, nil}.
posix_zone(Tz) when is_binary(Tz) ->
    case arc_posix_tz:parse(binary_to_list(Tz)) of
        none -> {error, nil};
        PosixTz -> {ok, {posix, PosixTz}}
    end.

-spec time_zone_id(local_zone()) -> {ok, binary()} | {error, nil}.
time_zone_id({tzif, Id, _Tz}) -> {ok, Id};
time_zone_id(_PosixOrNone) -> {error, nil}.

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

zone_offset({posix, PosixTz}, Sec) -> arc_posix_tz:offset_at(PosixTz, Sec);
zone_offset({tzif, _Id, Tz}, Sec) -> arc_tzif:offset_at(Tz, Sec).

to_minutes(OffSec) -> floor_div(OffSec, 60).

%% same as int_math.floor_div
floor_div(A, B) ->
    case A rem B =/= 0 andalso (A < 0) =/= (B < 0) of
        true -> A div B - 1;
        false -> A div B
    end.
