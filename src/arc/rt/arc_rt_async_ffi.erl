-module(arc_rt_async_ffi).
-export([apply_sm/5]).

-include("arc_rt_layout.hrl").

apply_sm(St, Sm, Rs, Sent, Loc) ->
    try Sm(St, Rs, Sent, Loc) of
        {StepWire, St2} -> {step(Sm, StepWire), St2}
    catch
        error:{wasm_exn, 0, [St2, E]} -> {{?STEP_THROW, E}, St2}
    end.

step(_Sm, {return, V}) -> {?STEP_RETURN, V};
step(_Sm, {throw, V}) -> {?STEP_THROW, V};
step(Sm, {yield, V, Ns, Loc}) -> {?STEP_YIELD, V, {?RESUME_COMPILED_TAG, Sm, Ns, Loc}};
step(Sm, {await, V, Ns, Loc}) -> {?STEP_AWAIT, V, {?RESUME_COMPILED_TAG, Sm, Ns, Loc}}.
