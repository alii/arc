-module(arc_wasm_ffi).
-export([start/0]).

%% invariant: every promise is settled exactly once
start() ->
    register(main, self()),
    Eng = arc@wasm@playground:new_engine(),
    announce_ready(),
    loop(Eng).

%% calling in before this hangs the tab; best effort
announce_ready() ->
    try
        emscripten:run_script(<<"globalThis.dispatchEvent(new Event('atomvm:ready'))">>,
                              [main_thread, async])
    catch
        C:R -> io:format("arc_wasm_ffi: ready announcement failed ~p:~p~n", [C, R])
    end.

loop(Eng) ->
    receive
        {emscripten, {call, Promise, Src0}} ->
            handle_call(Promise, Src0, Eng),
            loop(Eng);
        {emscripten, Req} = Msg when is_tuple(Req), tuple_size(Req) >= 2,
                                     element(1, Req) =:= call ->
            reject_malformed(element(2, Req), Msg),
            loop(Eng);
        Other ->
            io:format("arc_wasm_ffi: unexpected message ~p~n", [Other]),
            loop(Eng)
    end.

handle_call(Promise, Src0, Eng) ->
    try
        case normalise_source(Src0) of
            {ok, Src} ->
                case in_worker(fun() -> arc@wasm@playground:eval(Eng, Src) end) of
                    {ok, Out} -> emscripten:promise_resolve(Promise, Out);
                    {error, Msg} -> emscripten:promise_reject(Promise, Msg);
                    {crash, WC, WR, WSt} ->
                        reject_quietly(Promise, fun() -> format_crash(WC, WR, WSt) end)
                end;
            {error, Reason} ->
                emscripten:promise_reject(Promise, Reason)
        end
    catch
        C:R:St ->
            reject_quietly(Promise, fun() -> format_crash(C, R, St) end)
    end.

%% fresh process per run so garbage never piles up here
%% fibonacci growth measured 2-3x faster; never add min_heap_size
in_worker(Eval) ->
    Self = self(),
    Ref = make_ref(),
    _Pid = spawn_opt(fun() ->
                         Self ! {Ref, try Eval()
                                      catch C:R:St -> {crash, C, R, St}
                                      end}
                     end, [{atomvm_heap_growth, fibonacci}]),
    receive
        {Ref, Result} -> Result
    end.

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

%% thunk so a crashing reason still settles the promise
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

to_binary(Chars) ->
    case unicode:characters_to_binary(Chars) of
        Bin when is_binary(Bin) -> Bin;
        _NotUnicode -> <<"arc: internal error">>
    end.
