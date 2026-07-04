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

%% Guarded so the dictionary can only ever hold a boolean: without the guard a
%% mis-declared `@external` (or an FFI caller passing anything at all) could
%% store a non-boolean, and the old `=/= false` reading of it silently meant
%% "may block".
set_can_block(Bool) when is_boolean(Bool) ->
    put(arc_can_block, Bool),
    nil.

%% Total: `undefined` (unset key) is the documented default of true, and the
%% only other reachable values are the booleans set_can_block/1 admits. Any
%% other term is a bug in this module and crashes here rather than being
%% coerced into "true".
can_block() ->
    case get(arc_can_block) of
        undefined -> true;
        Bool when is_boolean(Bool) -> Bool
    end.
