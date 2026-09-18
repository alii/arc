-module(arc_clock_ffi).
-export([now_ms/0, monotonic_now/0, sleep_ms/1]).

now_ms() -> erlang:system_time(millisecond).

%% offset so readings are non-negative
monotonic_now() ->
    erlang:convert_time_unit(
        erlang:monotonic_time() - erlang:system_info(start_time),
        native, millisecond).

sleep_ms(Ms) when Ms =< 0 -> nil;
sleep_ms(Ms) -> timer:sleep(Ms), nil.
