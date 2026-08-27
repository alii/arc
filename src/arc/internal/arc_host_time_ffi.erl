-module(arc_host_time_ffi).
-export([now_ms/0]).

now_ms() -> erlang:system_time(millisecond).
