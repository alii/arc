-module(arc_wasm_ffi).
-export([start/0]).

%% packbeam entry. Owns the receive loop so Erlang-level crashes (undef from
%% AtomVM stdlib gaps, etc.) are caught and surfaced instead of killing the
%% listener and leaving JS with a dead "noproc" endpoint.
%%
%% THE INVARIANT: every message that carries a JS promise leaves this loop
%% with that promise SETTLED, exactly once. A promise nobody settles is a JS
%% caller hung forever with no error to show, which is strictly worse than any
%% error we could report — so the whole handler body, including the input
%% normalisation and both emscripten:promise_* calls, sits inside the try's
%% protected part.
start() ->
    register(main, self()),
    loop().

loop() ->
    receive
        {emscripten, {call, Promise, Src0}} ->
            handle_call(Promise, Src0),
            loop();
        {emscripten, Req} = Msg when is_tuple(Req), tuple_size(Req) >= 2,
                                     element(1, Req) =:= call ->
            %% A `call` request whose payload no longer matches the clause
            %% above — emscripten's request format drifted away from ours.
            %% Reject the promise we can still see rather than dropping the
            %% message: a JS caller must never be left waiting.
            reject_malformed(element(2, Req), Msg),
            loop();
        Other ->
            %% Nothing to settle, but never silently swallowed: an unexpected
            %% message here means someone is talking to us in a protocol we
            %% do not implement.
            io:format("arc_wasm_ffi: unexpected message ~p~n", [Other]),
            loop()
    end.

%% Both promise_resolve/2 and promise_reject/2 are inside the protected part
%% (no `of` section), so an AtomVM crash in either — or in the
%% characters_to_binary normalisation above them — still ends in a rejection
%% rather than an unsettled promise. The nested try guards the last resort:
%% if the reject itself is what crashed, there is nothing left to do but log.
handle_call(Promise, Src0) ->
    try
        Src = normalise_source(Src0),
        case arc@wasm@playground:eval(Src) of
            {ok, Out} -> emscripten:promise_resolve(Promise, Out);
            {error, Msg} -> emscripten:promise_reject(Promise, Msg)
        end
    catch
        C:R:St ->
            reject_quietly(Promise, format_crash(C, R, St))
    end.

normalise_source(Src) when is_binary(Src) -> Src;
normalise_source(Src) -> unicode:characters_to_binary(Src).

reject_malformed(Promise, Msg) ->
    reject_quietly(
        Promise,
        unicode:characters_to_binary(
            io_lib:format("arc: malformed request ~p", [Msg]))).

reject_quietly(Promise, Reason) ->
    try emscripten:promise_reject(Promise, Reason)
    catch
        C:R ->
            io:format("arc_wasm_ffi: promise_reject failed ~p:~p "
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
    unicode:characters_to_binary(
        io_lib:format("BEAM ~p: ~p~s", [Class, Reason, Top])).
