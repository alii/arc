-module(arc_clock_ffi).
-export([monotonic_now/0, sleep/1]).

%% offset so readings are non-negative
monotonic_now() ->
    erlang:convert_time_unit(
        erlang:monotonic_time() - erlang:system_info(start_time),
        native, millisecond).

sleep(Ms) when Ms =< 0 -> nil;
sleep(Ms) -> timer:sleep(Ms), nil.
