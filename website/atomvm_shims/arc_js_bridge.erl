%% Erlang -> page JavaScript round trip for the browser playground.
%%
%% AtomVM's emscripten port can run a script on the page's main thread
%% (`emscripten:run_script/2`) but not get a value back; and the page can push
%% text into a REGISTERED Erlang process (`Module.cast(name, text)`, delivered
%% as `{emscripten, {cast, Binary}}`). This module is the registered process
%% that stitches the two into a call: `call(Script)` evaluates `Script` (JS
%% source of an expression that yields a string) on the main thread and
%% returns `{ok, Text}` — or `{error, Text}` if it threw. Replies are matched
%% to callers by a request id the page echoes back.
%%
%% The page half is `globalThis.__arcJsBridge` in website/src/playground/
%% use-atomvm.ts. Everything a browser host can do for Arc that Erlang cannot
%% (RegExp via the JS engine, for one) is built on this.
-module(arc_js_bridge).
-export([start/0, call/1, js_string/1]).

-define(SEP, 31).  %% U+001F unit separator between id / status / payload
-define(TIMEOUT_MS, 30000).

start() ->
    register(arc_js_bridge, self()),
    loop(#{}, 1).

%% call(Script) -> {ok, binary()} | {error, binary()}
call(Script) when is_binary(Script) ->
    Ref = make_ref(),
    arc_js_bridge ! {call, self(), Ref, Script},
    receive
        {arc_js_bridge, Ref, Reply} -> Reply
    after ?TIMEOUT_MS ->
        {error, <<"arc_js_bridge: no reply from the page within 30s">>}
    end.

loop(Pending, NextId) ->
    receive
        {call, From, Ref, Script} ->
            Id = integer_to_binary(NextId),
            Js = <<"globalThis.__arcJsBridge.run(", Id/binary,
                   ", function () { return (", Script/binary, "); });">>,
            try
                emscripten:run_script(Js, [main_thread, async]),
                loop(Pending#{Id => {From, Ref}}, NextId + 1)
            catch
                C:R ->
                    Msg = unicode:characters_to_binary(
                            io_lib:format("arc_js_bridge: run_script failed ~p:~p", [C, R])),
                    From ! {arc_js_bridge, Ref, {error, Msg}},
                    loop(Pending, NextId + 1)
            end;
        {emscripten, {cast, Bin}} when is_binary(Bin) ->
            case binary:split(Bin, <<?SEP>>, [global]) of
                [Id, Status, Payload] ->
                    %% (maps:take/2 does not exist in AtomVM's maps)
                    case maps:find(Id, Pending) of
                        {ok, {From, Ref}} ->
                            Reply = case Status of
                                <<"ok">> -> {ok, Payload};
                                _ -> {error, Payload}
                            end,
                            From ! {arc_js_bridge, Ref, Reply},
                            loop(maps:remove(Id, Pending), NextId);
                        error ->
                            loop(Pending, NextId)
                    end;
                _Malformed ->
                    loop(Pending, NextId)
            end;
        _Other ->
            loop(Pending, NextId)
    end.

%% A JS string literal (double-quoted) for a UTF-8 binary: escapes `\`, `"`,
%% control characters and U+2028/U+2029 (line terminators, illegal in a JS
%% string literal); everything else passes through as UTF-8, which the JS
%% engine decodes.
js_string(Bin) when is_binary(Bin) ->
    <<$", (js_escape(Bin, <<>>))/binary, $">>.

js_escape(<<>>, Acc) -> Acc;
js_escape(<<$\\, R/binary>>, Acc) -> js_escape(R, <<Acc/binary, "\\\\">>);
js_escape(<<$", R/binary>>, Acc) -> js_escape(R, <<Acc/binary, "\\\"">>);
js_escape(<<$\n, R/binary>>, Acc) -> js_escape(R, <<Acc/binary, "\\n">>);
js_escape(<<$\r, R/binary>>, Acc) -> js_escape(R, <<Acc/binary, "\\r">>);
js_escape(<<16#E2, 16#80, 16#A8, R/binary>>, Acc) -> js_escape(R, <<Acc/binary, "\\u2028">>);
js_escape(<<16#E2, 16#80, 16#A9, R/binary>>, Acc) -> js_escape(R, <<Acc/binary, "\\u2029">>);
js_escape(<<C, R/binary>>, Acc) when C < 16#20; C =:= 16#7F ->
    Hex = integer_to_list(C, 16),
    Pad = lists:duplicate(4 - length(Hex), $0),
    js_escape(R, <<Acc/binary, "\\u", (list_to_binary(Pad ++ Hex))/binary>>);
js_escape(<<C, R/binary>>, Acc) -> js_escape(R, <<Acc/binary, C>>).
