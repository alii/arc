%% arc_realm_ffi — process-local registry for the embedder's $262 extension
%% hook (see realm.gleam's Extend262). DATA-ONLY: two process-dictionary
%% operations, no receives, no sends — the same pattern as the CanBlock
%% flag in arc_agent_ffi.erl. The hook is a Gleam closure; pdict scoping
%% means each per-test worker (and each spawned agent child) registers its
%% own, and processes that never register one build a plain $262.
-module(arc_realm_ffi).

-export([set_extend_262/1, get_extend_262/0]).

set_extend_262(Hook) ->
    erlang:put(arc_extend_262, Hook),
    nil.

%% An unregistered hook is a MISSING VALUE, not a failed operation, so this
%% answers Gleam's `Option` (`{some, Hook} | none`) rather than a `Result`.
get_extend_262() ->
    case erlang:get(arc_extend_262) of
        undefined -> none;
        Hook -> {some, Hook}
    end.
