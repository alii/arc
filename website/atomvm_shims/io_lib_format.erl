%% AtomVM shim — gleam_stdlib calls io_lib_format:fwrite_g/1 for float
%% formatting. AtomVM has io_lib but not this internal module.
%% float_to_list gives a longer mantissa than OTP's shortest-round-trip
%% algorithm but is correct.
-module(io_lib_format).
-export([fwrite_g/1]).

fwrite_g(F) when is_float(F) ->
    float_to_list(F).
