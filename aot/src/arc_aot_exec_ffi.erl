%%% Top-level protected apply of a compiled module's `js_main/3`, plus the
%%% turn-end epilogue (drain microtasks, GC safepoint) the runner owns.
-module(arc_aot_exec_ffi).
-export([apply_js_main/2]).

%% apply_js_main(Mod, St) -> {JsExecOutcome, St'}
%%   {js_returned, V} normal completion of js_main and the epilogue
%%   {js_threw, E}    uncaught JS throw from js_main or from a microtask
%%   {js_crashed, R}  any other error; St' is the input St
%% The own-data overlay is cleared before entry so a re-applied seed sees a
%% fresh realm, and flushed into St' on every JS exit path.
apply_js_main(Mod, St) ->
    arc_rt_obj_ffi:jsv_clear(),
    Frame = {undefined, undefined, undefined, undefined},
    try
        {Outcome, St2} =
            try Mod:js_main(St, Frame, []) of
                {V, St1} -> {{js_returned, V}, St1}
            catch
                error:{wasm_exn, 0, [St1, E1]} -> {{js_threw, E1}, St1}
            end,
        {Outcome, epilogue(St2)}
    of
        {O, St3} -> {O, arc_rt_obj_ffi:jsv_flush(St3)}
    catch
        error:{wasm_exn, 0, [St4, E2]} ->
            {{js_threw, E2}, arc_rt_obj_ffi:jsv_flush(St4)};
        Class:Reason:Stk ->
            arc_rt_obj_ffi:jsv_clear(),
            {{js_crashed, render_reason(Class, Reason, Stk)}, St}
    end.

epilogue(St0) ->
    St1 = 'arc@rt@async':t_drain_microtasks(St0),
    'arc@rt@gc':t_maybe_collect(St1).

render_reason(Class, Reason, Stk) ->
    Top = case Stk of [H | _] -> H; [] -> no_stack end,
    unicode:characters_to_binary(
        io_lib:format("~0p:~0p at ~0p", [Class, Reason, Top])).
