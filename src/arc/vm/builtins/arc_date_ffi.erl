-module(arc_date_ffi).
-export([now_ms/0, tz_offset_minutes/1]).

%% UTC milliseconds since 1970-01-01T00:00:00Z.
now_ms() -> erlang:system_time(millisecond).

%% Minutes UTC is ahead of local at the given epoch-ms (JS sign convention:
%% UTC - local, so US Pacific Standard is +480, CET is -60).
%% We convert via gregorian seconds so this works for any epoch including
%% negative (pre-1970) values.
tz_offset_minutes(EpochMs) ->
    Secs = EpochMs div 1000,
    %% gregorian seconds at 1970-01-01T00:00:00Z
    Greg = Secs + 62167219200,
    case Greg < 0 of
        true -> 0;
        false ->
            UTC = calendar:gregorian_seconds_to_datetime(Greg),
            case (catch calendar:universal_time_to_local_time(UTC)) of
                {'EXIT', _} -> 0;
                Local ->
                    LG = calendar:datetime_to_gregorian_seconds(Local),
                    (Greg - LG) div 60
            end
    end.
