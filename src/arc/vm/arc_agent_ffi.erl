%% The Agent Record's [[CanBlock]] flag (ES2024 §9.7), stored in the process
%% dictionary because it is per-AGENT policy and an agent is one BEAM
%% process. Nothing else about the agent record lives here.
%%
%% Defaults to true (an unset key reads as true): arc's main agent and its
%% spawned agent children may block in a synchronous Atomics.wait.
%% `interpreter.new_state` reads it once at realm boot to seed
%% `State.can_block`; the test262 runner clears it before booting a realm for
%% a test carrying the CanBlockIsFalse flag.
%%
%% [[CanBlock]] is only half of AgentCanSuspend(): the realm must ALSO have
%% an installed `HostHooks.atomics` capability pair, since the blocking
%% itself happens in the embedder (see arc/host.gleam). A false flag here, or
%% a missing capability there, both make sync Atomics.wait a TypeError.
-module(arc_agent_ffi).
-export([set_can_block/1, can_block/0]).

set_can_block(Bool) ->
    put(arc_can_block, Bool),
    nil.

can_block() ->
    get(arc_can_block) =/= false.
