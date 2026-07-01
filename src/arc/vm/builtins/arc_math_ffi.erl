-module(arc_math_ffi).
-export([fround/1, is_neg_zero/1]).

%% Math.fround: round a 64-bit float to the nearest 32-bit (IEEE 754
%% single-precision) float, then widen back to 64-bit. This matches the
%% ES2024 §20.2.2.17 spec behavior.
%%
%% We encode the float as a 32-bit big-endian binary and decode it back.
%% This is the standard Erlang trick for single-precision rounding.
fround(X) when is_float(X) ->
    <<F32:32/float>> = <<X:32/float>>,
    F32;
fround(X) when is_integer(X) ->
    fround(float(X)).

%% Detect IEEE 754 negative zero. BEAM floats preserve the sign bit but
%% Erlang's == and =:= don't reliably distinguish ±0 in all contexts.
%% We check the sign bit of the IEEE 754 binary representation directly.
is_neg_zero(X) when is_float(X) ->
    <<Sign:1, _:63>> = <<X:64/float>>,
    X == 0.0 andalso Sign =:= 1;
is_neg_zero(_) ->
    false.
