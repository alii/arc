-module(arc_aot_wasm_ffi).
-export([start/0]).

%% packbeam entry for the website's AtomVM bundle. Two registered endpoints:
%%
%%   main — the interpreter (arc_wasm_ffi's loop, in its own process)
%%   aot  — the AOT compiler's inspection surface: JS source in, the three
%%          emitted artefacts out (2core IR, Core Erlang, Erlang source),
%%          joined by U+001E (record separator)
%%
%% Same invariant as arc_wasm_ffi: every message carrying a JS promise leaves
%% the loop with that promise settled exactly once.
start() ->
    %% spawn/3, not spawn(fun M:F/0): an external-fun literal aborts AtomVM
    %% 0.7.0-alpha at load.
    spawn(arc_wasm_ffi, start, []),
    register(aot, self()),
    loop().

loop() ->
    receive
        {emscripten, {call, Promise, Src0}} ->
            handle(Promise, Src0),
            loop();
        Other ->
            io:format("arc_aot_wasm_ffi: unexpected message ~p~n", [Other]),
            loop()
    end.

handle(Promise, Src0) ->
    try
        Src = unicode:characters_to_binary(Src0),
        case in_worker(fun() -> arc_aot@playground:emit(Src, <<"playground">>) end) of
            {ok, {emitted, Ir, Core, Erl}} ->
                emscripten:promise_resolve(Promise,
                                           <<Ir/binary, 30, Core/binary, 30, Erl/binary>>);
            {error, Msg} when is_binary(Msg) ->
                emscripten:promise_reject(Promise, Msg);
            {crash, C, R, St} ->
                reject_quietly(Promise, fun() -> format_crash(C, R, St) end)
        end
    catch
        C0:R0:St0 ->
            reject_quietly(Promise, fun() -> format_crash(C0, R0, St0) end)
    end.

%% Run Work in a fresh process and wait for its result.
%%
%% Why: AtomVM's copying GC makes every allocation cost O(live heap). A
%% request handled inside this long-lived loop leaves the loop's heap large,
%% and every later request pays for it (measured: the same compile went from
%% 17s to 29s across consecutive runs in-loop, and a stable 4s in a fresh
%% worker). The worker's result is a tuple of binaries — refc, off-heap — so
%% handing it back is cheap.
in_worker(Work) ->
    Self = self(),
    Ref = make_ref(),
    %% fibonacci growth: see arc_wasm_ffi:in_worker/1 for the measurements.
    _Pid = spawn_opt(fun() ->
                         Self ! {Ref, try Work()
                                      catch C:R:St -> {crash, C, R, St}
                                      end}
                     end, [{atomvm_heap_growth, fibonacci}]),
    receive
        {Ref, Result} -> Result
    end.

reject_quietly(Promise, ReasonFun) ->
    Reason = try ReasonFun()
             catch _:_ -> <<"aot: internal error">>
             end,
    try emscripten:promise_reject(Promise, Reason)
    catch
        C:R ->
            io:format("arc_aot_wasm_ffi: promise_reject failed ~p:~p "
                      "(original: ~p)~n", [C, R, Reason])
    end.

format_crash(Class, Reason, Stack) ->
    Top = case Stack of
        [{M, F, A, _} | _] when is_integer(A) ->
            io_lib:format(" at ~p:~p/~p", [M, F, A]);
        [{M, F, A, _} | _] when is_list(A) ->
            io_lib:format(" at ~p:~p/~p", [M, F, length(A)]);
        _ -> ""
    end,
    case unicode:characters_to_binary(io_lib:format("BEAM ~p: ~p~s", [Class, Reason, Top])) of
        Bin when is_binary(Bin) -> Bin;
        _NotUnicode -> <<"aot: internal error">>
    end.
