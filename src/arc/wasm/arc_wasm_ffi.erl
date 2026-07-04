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
%% (no `of` section), so an AtomVM crash in either — or in the source
%% normalisation above them — still ends in a rejection rather than an
%% unsettled promise. reject_quietly/2 takes a THUNK, not a reason: building
%% the reason string (io_lib:format over an arbitrary crash term) is itself
%% code that can crash, and doing it eagerly inside the catch handler would
%% leave the promise unsettled — the one outcome this module forbids.
handle_call(Promise, Src0) ->
    try
        case normalise_source(Src0) of
            {ok, Src} ->
                case arc@wasm@playground:eval(Src) of
                    {ok, Out} -> emscripten:promise_resolve(Promise, Out);
                    {error, Msg} -> emscripten:promise_reject(Promise, Msg)
                end;
            {error, Reason} ->
                emscripten:promise_reject(Promise, Reason)
        end
    catch
        C:R:St ->
            reject_quietly(Promise, fun() -> format_crash(C, R, St) end)
    end.

%% The request payload as it arrives from JS. `unicode:characters_to_binary/1`
%% is not total — ill-formed input answers `{error, _, _}` / `{incomplete, _, _}`
%% and a non-iodata term (atom, integer) raises badarg — so its result is
%% CHECKED here rather than assumed to be a binary and passed on to the
%% evaluator as one.
normalise_source(Src) when is_binary(Src) ->
    {ok, Src};
normalise_source(Src) ->
    try unicode:characters_to_binary(Src) of
        Bin when is_binary(Bin) -> {ok, Bin};
        _NotUnicode -> {error, <<"arc: request payload is not valid unicode">>}
    catch
        _:_ -> {error, <<"arc: request payload is not valid unicode">>}
    end.

reject_malformed(Promise, Msg) ->
    reject_quietly(Promise, fun() -> format_malformed(Msg) end).

format_malformed(Msg) ->
    to_binary(io_lib:format("arc: malformed request ~p", [Msg])).

%% Settle the promise with `ReasonFun()`, and settle it even if ReasonFun
%% itself blows up: an unsettled promise is a JS caller hung forever, which is
%% strictly worse than a vague error message. The outer try guards the last
%% resort — if the reject is what crashed, there is nothing left to do but log.
reject_quietly(Promise, ReasonFun) ->
    Reason = try ReasonFun()
             catch _:_ -> <<"arc: internal error">>
             end,
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
    to_binary(io_lib:format("BEAM ~p: ~p~s", [Class, Reason, Top])).

%% io_lib:format output is well-formed chardata, but characters_to_binary/1
%% still HAS an error-tuple return type — pin it to a binary rather than
%% handing emscripten a 3-tuple where it expects a rejection reason.
to_binary(Chars) ->
    case unicode:characters_to_binary(Chars) of
        Bin when is_binary(Bin) -> Bin;
        _NotUnicode -> <<"arc: internal error">>
    end.
