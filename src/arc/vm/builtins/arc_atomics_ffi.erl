%% Blocking sleep for Atomics.wait. Arc is single-agent, so a sync wait can
%% never be notified — it just suspends the (only) JS thread for the timeout.
-module(arc_atomics_ffi).
-export([sleep/1, sleep_forever/0, monotonic_now/0,
         set_agent_callback_mode/1, in_agent_callback_mode/0]).

%% Process-local flag: true while a cooperative $262.agent broadcast
%% callback is running. Sync Atomics.wait with an infinite timeout inside
%% such a callback can never be notified (the main script is suspended
%% until the callback returns), so the wait is bounded instead of
%% deadlocking the host process.
set_agent_callback_mode(Bool) ->
    put(arc_agent_callback_mode, Bool),
    nil.

in_agent_callback_mode() ->
    get(arc_agent_callback_mode) =:= true.

sleep(Ms) when Ms =< 0 -> nil;
sleep(Ms) -> timer:sleep(Ms), nil.

sleep_forever() -> timer:sleep(infinity), nil.

%% Monotonic clock in milliseconds, offset so readings are non-negative
%% (erlang:monotonic_time/1 starts at a large negative number).
monotonic_now() ->
    erlang:convert_time_unit(
        erlang:monotonic_time() - erlang:system_info(start_time),
        native, millisecond).
