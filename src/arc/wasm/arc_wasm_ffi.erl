-module(arc_wasm_ffi).
-export([start/0]).

%% packbeam entry. Owns the receive loop so Erlang-level crashes (undef from
%% AtomVM stdlib gaps, etc.) can be caught and surfaced instead of killing the
%% listener and leaving JS with a dead "noproc" endpoint.
start() ->
    register(main, self()),
    loop().

loop() ->
    receive
        {emscripten, {call, Promise, Src0}} ->
            Src = if is_binary(Src0) -> Src0;
                     true -> unicode:characters_to_binary(Src0)
                  end,
            try arc@wasm@playground:eval(Src) of
                {ok, Out} -> emscripten:promise_resolve(Promise, Out);
                {error, Msg} -> emscripten:promise_reject(Promise, Msg)
            catch
                C:R:St ->
                    emscripten:promise_reject(Promise, format_crash(C, R, St))
            end,
            loop();
        _ ->
            loop()
    end.

format_crash(Class, Reason, Stack) ->
    Top = case Stack of
        [{M, F, A, _} | _] when is_integer(A) ->
            io_lib:format(" at ~p:~p/~p", [M, F, A]);
        [{M, F, A, _} | _] when is_list(A) ->
            io_lib:format(" at ~p:~p/~p", [M, F, length(A)]);
        _ -> ""
    end,
    unicode:characters_to_binary(
        io_lib:format("BEAM ~p: ~p~s", [Class, Reason, Top])).
