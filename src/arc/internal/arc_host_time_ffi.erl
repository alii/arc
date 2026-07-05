%% The host wall clock, behind arc/internal/host_time.
%%
%% The local time zone offsets are NOT here: they come from arc_tz_ffi
%% (offset_at_utc_ms/1, offset_at_local_ms/1), so the runtime has one time zone
%% engine, reading the same TZif data Temporal reads, with one sign convention.
-module(arc_host_time_ffi).
-export([now_ms/0]).

%% UTC milliseconds since 1970-01-01T00:00:00Z.
now_ms() -> erlang:system_time(millisecond).
