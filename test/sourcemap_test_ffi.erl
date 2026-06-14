-module(sourcemap_test_ffi).
-export([is_valid_json/1]).

%% Validate that a binary is well-formed JSON by decoding it with OTP's
%% built-in `json` module. Returns `true` if it parses, `false` otherwise.
%% Used by the source-map round-trip test to assert `codec:to_json` emits
%% syntactically valid JSON.
is_valid_json(Bin) when is_binary(Bin) ->
    try
        _ = json:decode(Bin),
        true
    catch
        _:_ -> false
    end;
is_valid_json(_) ->
    false.
