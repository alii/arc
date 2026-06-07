-module(arc_date_ffi).
-export([now_ms/0, tz_offset_minutes/1]).

%% UTC milliseconds since 1970-01-01T00:00:00Z.
now_ms() -> erlang:system_time(millisecond).

%% Minutes UTC is ahead of local at the given epoch-ms (JS sign convention:
%% UTC - local, so US Pacific Standard is +480, CET is -60).
%% We convert via gregorian seconds so this works for any epoch including
%% negative (pre-1970) values.
%%
%% Results are memoized per-process (keyed by epoch second) because the
%% OS-level conversion costs ~2.5us a call and Date-heavy code asks for the
%% same instants over and over — engines are expected to cache local tz
%% offsets (V8 DateCache / SpiderMonkey DateTimeInfo; see the SunSpider-era
%% staging/sm/Date/dst-offset-caching-* tests). An exact-key cache is always
%% correct: same instant, same offset. Bounded so pathological workloads
%% cannot grow it without limit; a full reset on overflow keeps hits O(1).
-define(TZ_CACHE_KEY, arc_tz_offset_cache).
-define(TZ_CACHE_MAX, 16384).

tz_offset_minutes(EpochMs) ->
    Secs = EpochMs div 1000,
    Cache = case erlang:get(?TZ_CACHE_KEY) of
        undefined -> #{};
        C -> C
    end,
    case Cache of
        #{Secs := Cached} ->
            Cached;
        _ ->
            Off = compute_tz_offset_minutes(Secs),
            Base = case map_size(Cache) >= ?TZ_CACHE_MAX of
                true -> #{};
                false -> Cache
            end,
            erlang:put(?TZ_CACHE_KEY, Base#{Secs => Off}),
            Off
    end.

compute_tz_offset_minutes(Secs) ->
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
