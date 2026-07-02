%% Total wrappers around the BEAM's partial float math.
%%
%% Erlang has no IEEE-754 infinities: `math:exp/1`, `math:pow/2`, `math:cosh/1`
%% and `math:sinh/1` raise `badarith` the moment the true result overflows a
%% 64-bit float, and decoding a `:32/float` segment whose bits encode ±Inf
%% raises `badmatch`. Left unhandled those exceptions take down the whole VM
%% process, so `Math.exp(710)`, `1e300 ** 2`, `Math.cosh(711)`,
%% `Math.fround(1e300)`, ... would crash the runtime instead of returning
%% ±Infinity as ES §21.3.2 requires.
%%
%% Every overflow-capable entry point here therefore returns the Gleam
%% `arc/vm/value.JsNum` runtime shape directly —
%%
%%     {finite, Float} | infinity | neg_infinity | na_n
%%
%% — so the Gleam side is FORCED (by the type) to handle the non-finite
%% outcomes; there is no `-> Float` signature left through which an overflow
%% can escape as an uncaught exception.
-module(arc_math_ffi).
-export([exp/1, pow/2, cosh/1, sinh/1, fround/1, is_neg_zero/1]).

%% math:exp/1 overflows only toward +Infinity (e^x for large positive x).
exp(X) ->
    try {finite, math:exp(X)}
    catch error:badarith -> infinity
    end.

%% math:cosh/1 is even and >= 1, so overflow is always +Infinity.
cosh(X) ->
    try {finite, math:cosh(X)}
    catch error:badarith -> infinity
    end.

%% math:sinh/1 is odd, so the overflow takes the sign of the argument.
sinh(X) ->
    try {finite, math:sinh(X)}
    catch
        error:badarith when X < 0 -> neg_infinity;
        error:badarith -> infinity
    end.

%% math:pow/2 raises badarith in exactly two situations:
%%   * the true result's magnitude overflows a 64-bit float, or
%%   * Base < 0 with a non-integer Exp (no real result).
%% Distinguish them so overflow gets the sign the real result would have
%% had: negative iff the base is negative and the (integer) exponent is odd.
pow(Base, Exp) ->
    try {finite, math:pow(Base, Exp)}
    catch error:badarith -> pow_non_finite(Base, Exp)
    end.

pow_non_finite(Base, Exp) when Base < 0 ->
    T = trunc(Exp),
    if
        T /= Exp -> na_n;
        T rem 2 =:= 0 -> infinity;
        true -> neg_infinity
    end;
pow_non_finite(_Base, _Exp) ->
    infinity.

%% Math.fround: round a 64-bit float to the nearest 32-bit (IEEE 754
%% single-precision) float, then widen back to 64-bit (ES2024 §21.3.2.17).
%%
%% The ENCODE `<<X:32/float>>` always succeeds: a magnitude past the float32
%% range simply produces the IEEE ±infinity bit pattern (exponent field all
%% ones, mantissa zero). It is the DECODE that Erlang refuses — a
%% `<<F:32/float>>` match fails on those bits because BEAM floats cannot hold
%% infinity. So match the infinity encodings explicitly and only fall through
%% to the float decode for bits that are guaranteed to represent a finite
%% value. (Exponent-255/nonzero-mantissa — NaN — is unreachable from a finite
%% input; the final clause keeps the function total anyway.)
fround(X) when is_float(X) ->
    case <<X:32/float>> of
        <<0:1, 255:8, 0:23>> -> infinity;
        <<1:1, 255:8, 0:23>> -> neg_infinity;
        <<F32:32/float>> -> {finite, F32};
        _ -> na_n
    end;
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
