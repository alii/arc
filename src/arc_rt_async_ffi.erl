%%% arc_rt_async_ffi — sm invocation + resume-pin shim for `rt_async`
%%% (SPEC §7.M8 / §7.M18; R2).
%%%
%%% Hand-written Erlang, so it carries the `arc_rt_` namespace prefix
%%% (overview §5) and can NEVER collide with an OTP module — exactly like
%%% `arc_rt_store_ffi` / `arc_rt_gc_ffi`. Pure term construction
%%% + protected apply: no NIF, no process state, cannot crash the node.
%%%
%%% Why a shim: (1) `apply_sm` must apply an opaque `CompiledFn` (BEAM fun/4)
%%% and catch `{wasm_exn, 0, [St, E]}` into a `StepThrow` — same protected-
%%% apply shape as `t_call_protected` (SPEC §7.M-CALL). Gleam cannot apply an
%%% opaque type as a fun. (2) `mk_resume`/`repin_resume` store `{Sm, Rs, Loc}`
%%% in a `CompiledFn`-typed slot (u-resume-ffi-pin) so `SGenerator`/`SAsyncGen`
%%% carry rs+loc without a schema change to rt_types. (3) `step_classify`
%%% decodes the M18 wire step (`{return,V}` etc — plain-atom tags, SPEC:1509)
%%% into the Gleam `Step` sum's constructor atoms (`step_return` etc).
-module(arc_rt_async_ffi).
-export([apply_sm/5, apply_resume/3, mk_resume/3, repin_resume/3, loc_empty/0,
         step_classify/1]).

%% step_classify(StepWire) -> Step
%% Decode the M18 wire step into the Gleam `Step` sum. The wire tags are the
%% bare atoms `return`/`throw`/`yield`/`await` (SPEC:1509 — emitted via
%% `ConstAtom("return")` etc); the Gleam side is `step_return`/`step_throw`/
%% `step_yield`/`step_await`. Total over the 4-variant protocol; a malformed
%% step function-clause-crashes rather than fabricating a value (engine bug).
step_classify({return, V})            -> {step_return, V};
step_classify({throw,  V})            -> {step_throw,  V};
step_classify({yield,  V, Ns, Loc})   -> {step_yield,  V, Ns, Loc};
step_classify({await,  V, Ns, Loc})   -> {step_await,  V, Ns, Loc}.

%% apply_sm(St, Sm, Rs, Sent, Loc) -> {Step, St'}
%% Protected apply of a compiled sm closure (fun/4 — St + the three IR params
%% `_rs`/`_sent`/`_loc`; captures already curried by `MakeClosure`, SPEC
%% §18.1). A `{wasm_exn, 0, [St', E]}` escaping the sm's own per-arm Try (R2
%% payload order — St FIRST) becomes `StepThrow(E)`; every other error class
%% propagates as an engine bug (matches `t_call_protected` posture).
apply_sm(St, Sm, Rs, Sent, Loc) ->
    try Sm(St, Rs, Sent, Loc) of
        {StepWire, St2} -> {step_classify(StepWire), St2}
    catch
        error:{wasm_exn, 0, [St2, E]} -> {{step_throw, E}, St2}
    end.

%% mk_resume(Sm, Rs, Loc) -> {Sm, Rs, Loc}
%% Opaque `CompiledFn` term stored on `SGenerator.resume`/`SAsyncGen.resume`.
%% GC-transparent: `arc_rt_gc_ffi:refs_in_term/2` walks tuples + fun
%% envs, so `Sm`'s captured Handle bindings AND every `{js_cell,N}` inside
%% `Loc` stay traced with no rt_gc change.
mk_resume(Sm, Rs, Loc) -> {Sm, Rs, Loc}.

%% repin_resume({Sm, _, _}, Rs, Loc) -> {Sm, Rs, Loc}
%% Re-pin at a new resume-state + locals after a yield/await. Partial by
%% design — a non-`mk_resume` term function-clause-crashes (engine bug: only
%% `mk_resume`/`repin_resume` write the field).
repin_resume({Sm, _, _}, Rs, Loc) -> {Sm, Rs, Loc}.

%% apply_resume(St, {Sm, Rs, Loc}, Sent) -> {Step, St'}
%% Unpack a stored `resume` and delegate to `apply_sm`.
apply_resume(St, {Sm, Rs, Loc}, Sent) -> apply_sm(St, Sm, Rs, Sent, Loc).

%% loc_empty() -> {}
%% Initial locals tuple for a body with zero hoisted locals.
loc_empty() -> {}.
