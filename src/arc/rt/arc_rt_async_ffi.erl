-module(arc_rt_async_ffi).
-export([apply_state_machine/5]).

-include("arc_rt_layout.hrl").

apply_state_machine(St, Machine, ResumePoint, Sent, Locals) ->
    try Machine(St, ResumePoint, Sent, Locals) of
        {StepWire, St2} -> {step(Machine, StepWire), St2}
    catch
        error:?JS_THROW(St2, E) -> {{?STEP_THROW, E}, St2}
    end.

step(_Machine, {return, V}) -> {?STEP_RETURN, V};
step(_Machine, {throw, V}) -> {?STEP_THROW, V};
step(Machine, {yield, V, Ns, Locals}) -> {?STEP_YIELD, V, {?RESUMECOMPILED_TAG, Machine, Ns, Locals}};
step(Machine, {await, V, Ns, Locals}) -> {?STEP_AWAIT, V, {?RESUMECOMPILED_TAG, Machine, Ns, Locals}}.
