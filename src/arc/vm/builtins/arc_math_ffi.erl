%% Total wrappers around the BEAM's partial float math.
%%
%% Erlang has no IEEE-754 infinities: `math:exp/1`, `math:pow/2`, `math:cosh/1`
%% and `math:sinh/1` raise `badarith` the moment the true result overflows a
%% 64-bit float, and decoding a `:32/float` segment whose bits encode ±Inf
%% raises `badmatch`. Plain float arithmetic (`X * X`, `A + B`) badariths on
%% overflow too — see `hypot/1`. Left unhandled those exceptions take down the
%% whole VM process, so `Math.exp(710)`, `1e300 ** 2`, `Math.cosh(711)`,
%% `Math.fround(1e300)`, `Math.hypot(1e200, 1e200)`, ... would crash the
%% runtime instead of returning a finite result or ±Infinity as ES §21.3.2
%% requires.
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
-export([exp/1, pow/2, cosh/1, sinh/1, hypot/1, fround/1, is_neg_zero/1]).

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
    catch error:badarith -> signed_infinity(X)
    end.

%% math:pow/2 raises badarith in exactly three situations:
%%   * a ±0.0 base with a negative Exp (the true result is ±Infinity),
%%   * the true result's magnitude overflows a 64-bit float, or
%%   * the base is negative with a non-integer Exp (no real result).
%% Distinguish them so overflow gets the sign the real result would have
%% had (negative iff the base is negative and the integer exponent is odd),
%% and so a -0.0 base is not mistaken for the negative-non-integer NaN case
%% via its sign bit — §6.1.6.1.3: (-0) ** -0.5 is +Infinity, not NaN.
pow(Base, Exp) ->
    try {finite, math:pow(Base, Exp)}
    catch error:badarith -> pow_non_finite(Base, Exp)
    end.

%% Zero base only reaches here with a negative Exp (a positive/zero Exp
%% returns a finite ±0 or 1 and never badariths). §6.1.6.1.3 steps 12–15:
%% -Infinity iff base is -0 AND Exp is an odd integer, otherwise +Infinity.
pow_non_finite(Base, Exp) when Base == 0.0 ->
    T = trunc(Exp),
    case neg_sign(Base) andalso T == Exp andalso T rem 2 =/= 0 of
        true -> neg_infinity;
        false -> infinity
    end;
pow_non_finite(Base, Exp) ->
    case neg_sign(Base) of
        true ->
            T = trunc(Exp),
            if
                T /= Exp -> na_n;
                T rem 2 =:= 0 -> infinity;
                true -> neg_infinity
            end;
        false ->
            infinity
    end.

%% Math.hypot's sum of squares over the FINITE arguments (the caller has
%% already short-circuited ±Infinity and NaN per §21.3.2.18). Naively folding
%% `S + V*V` in Gleam overflows a 64-bit float — and thus badariths, killing
%% the process — for arguments as ordinary as `Math.hypot(1e200, 1e200)`,
%% whose true result is finite. So scale by the largest magnitude first: every
%% (V/Max)^2 is then in [0, 1] and the sum cannot overflow. Only the final
%% rescale can, and only when the true result really is out of range
%% (`Math.hypot(1e308, 1e308)`), where ES requires +Infinity.
hypot(Values) ->
    Max = lists:foldl(fun(V, Acc) -> max(abs(V), Acc) end, 0.0, Values),
    case Max == 0.0 of
        true ->
            {finite, 0.0};
        false ->
            SumSq = lists:foldl(
                fun(V, Acc) -> R = V / Max, Acc + R * R end,
                0.0,
                Values
            ),
            try {finite, Max * math:sqrt(SumSq)}
            catch error:badarith -> infinity
            end
    end.

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

%% THE sign test for a float, straight off the IEEE 754 sign bit — the ONLY
%% thing in this module allowed to ask "is this negative". Erlang's `<`, `>=`
%% and friends are arithmetic comparisons: `-0.0 < 0` is FALSE, so a guard
%% like `when Base < 0` silently classifies -0.0 as positive and hands the
%% overflow the wrong sign.
neg_sign(X) when is_float(X) ->
    <<Sign:1, _:63>> = <<X:64/float>>,
    Sign =:= 1.

%% ±Infinity, taking the sign of X (for odd functions that overflowed).
signed_infinity(X) ->
    case neg_sign(X) of
        true -> neg_infinity;
        false -> infinity
    end.

%% Detect IEEE 754 negative zero: a zero whose sign bit is set. BEAM floats
%% preserve the sign bit but Erlang's == and =:= don't reliably distinguish
%% ±0 in all contexts.
is_neg_zero(X) when is_float(X) ->
    X == 0.0 andalso neg_sign(X);
is_neg_zero(_) ->
    false.
