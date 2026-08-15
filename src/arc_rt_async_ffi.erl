%%% Protected apply of a compiled coroutine state machine for arc/rt/async.
%%% Gleam cannot apply an opaque `SmFn` as a fun, and a JS throw escaping the
%%% state machine's own per-arm try arrives as `{wasm_exn, 0, [St, E]}` and
%%% must become a `StepThrow` (same posture as `t_call_protected`). The wire
%%% step `{return,V} | {throw,V} | {yield,V,Ns,Loc} | {await,V,Ns,Loc}` is
%%% decoded here into arc/rt/types.Step, folding `(Sm, Ns, Loc)` into a
%%% `ResumeCompiled` so the driver never re-pins anything.
-module(arc_rt_async_ffi).
-export([apply_sm/5, loc_empty/0]).

-include("arc_rt_layout.hrl").

%% apply_sm(St, Sm, Rs, Sent, Loc) -> {Step, St'}
apply_sm(St, Sm, Rs, Sent, Loc) ->
    try Sm(St, Rs, Sent, Loc) of
        {StepWire, St2} -> {step(Sm, StepWire), St2}
    catch
        error:{wasm_exn, 0, [St2, E]} -> {{?STEP_THROW, E}, St2}
    end.

%% Total over the 4-variant protocol; a malformed step function-clause-
%% crashes rather than fabricating a value (engine bug).
step(_Sm, {return, V}) -> {?STEP_RETURN, V};
step(_Sm, {throw, V}) -> {?STEP_THROW, V};
step(Sm, {yield, V, Ns, Loc}) -> {?STEP_YIELD, V, {?RESUME_COMPILED_TAG, Sm, Ns, Loc}};
step(Sm, {await, V, Ns, Loc}) -> {?STEP_AWAIT, V, {?RESUME_COMPILED_TAG, Sm, Ns, Loc}}.

%% Initial locals tuple for a body with zero hoisted locals.
loc_empty() -> {}.
