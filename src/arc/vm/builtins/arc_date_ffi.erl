-module(arc_date_ffi).
-export([now_ms/0]).

%% UTC milliseconds since 1970-01-01T00:00:00Z.
now_ms() -> erlang:system_time(millisecond).

%% Local time zone offsets used to live here, computed with
%% calendar:universal_time_to_local_time/1 and cached in the process
%% dictionary. They now come from arc_tz_ffi (offset_at_utc_ms/1,
%% offset_at_local_ms/1) — the runtime has one time zone engine, reading the
%% same TZif data Temporal reads, with one sign convention.
