%% Blocking sleep for Atomics.wait. Arc is single-agent, so a sync wait can
%% never be notified — it just suspends the (only) JS thread for the timeout.
-module(arc_atomics_ffi).
-export([sleep/1, sleep_forever/0, monotonic_now/0,
         set_agent_callback_mode/1, in_agent_callback_mode/0,
         set_can_block/1, can_block/0]).

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

%% Process-local Agent Record [[CanBlock]] (ES2024 §9.7). Defaults to true
%% (an unset key reads as true): arc's main agent and spawned agent children
%% may block in Atomics.wait. The test262 runner clears it before booting a
%% realm for a test carrying the CanBlockIsFalse flag; interpreter new_state
%% reads it once at state init to seed State.can_block.
set_can_block(Bool) ->
    put(arc_can_block, Bool),
    nil.

can_block() ->
    get(arc_can_block) =/= false.

sleep(Ms) when Ms =< 0 -> nil;
sleep(Ms) -> timer:sleep(Ms), nil.

sleep_forever() -> timer:sleep(infinity), nil.

%% Monotonic clock in milliseconds, offset so readings are non-negative
%% (erlang:monotonic_time/1 starts at a large negative number).
monotonic_now() ->
    erlang:convert_time_unit(
        erlang:monotonic_time() - erlang:system_info(start_time),
        native, millisecond).
