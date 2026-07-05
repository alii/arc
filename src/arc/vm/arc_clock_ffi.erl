%% The DEFAULT clock/sleep host capability (host_hooks.default_host_hooks): the
%% BEAM monotonic clock and timer:sleep/1. Every host has a clock, so unlike
%% the Atomics blocking-wait capability these are not optional — an embedder
%% that virtualises time overrides `HostHooks.monotonic_now` / `sleep_ms`
%% with its own functions and never reaches this module.
%%
%% `sleep/1` is a BOUNDED idle: the event loop uses it to wait out the next
%% timer / waitAsync deadline, so the argument is always a finite number of
%% milliseconds. There is deliberately no unbounded sleep here — an untimed
%% synchronous Atomics.wait blocks through the embedder's
%% `HostHooks.atomics.await_notify` capability, which suspends on a mailbox
%% that a notifier can actually reach, not on a local sleep nothing can wake.
-module(arc_clock_ffi).
-export([monotonic_now/0, sleep/1]).

%% Monotonic clock in milliseconds, offset so readings are non-negative
%% (erlang:monotonic_time/1 starts at a large negative number).
monotonic_now() ->
    erlang:convert_time_unit(
        erlang:monotonic_time() - erlang:system_info(start_time),
        native, millisecond).

sleep(Ms) when Ms =< 0 -> nil;
sleep(Ms) -> timer:sleep(Ms), nil.
