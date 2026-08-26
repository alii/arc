%% Total wrappers around the BEAM's partial float math (Math.* builtins).
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
%% `arc/rt/types.JsNum` runtime shape directly —
%%
%%     {j_float, Float} | j_pos_inf | j_neg_inf | j_nan
%%
%% — so the Gleam side is FORCED (by the type) to handle the non-finite
%% outcomes; there is no `-> Float` signature left through which an overflow
%% can escape as an uncaught exception.
%%
%% Faithful port of arc_math_ffi.erl; JsNum tag names adapted to 2core's
%% wire encoding (JInt/JFloat/JNan/JPosInf/JNegInf → snake_case atoms).
-module(arc_rt_math_ffi).
-export([exp/1, pow/2, cosh/1, sinh/1, hypot/1, fround/1,
         t_math_sqrt/1, t_math_floor/1, t_math_abs/1,
         t_math_pow/2, t_math_min/2, t_math_max/2]).

-define(MAX_SAFE_INT, 9007199254740991).

%% math:exp/1 overflows only toward +Infinity (e^x for large positive x).
exp(X) ->
    try {j_float, math:exp(X)}
    catch error:badarith -> j_pos_inf
    end.

%% math:cosh/1 is even and >= 1, so overflow is always +Infinity.
cosh(X) ->
    try {j_float, math:cosh(X)}
    catch error:badarith -> j_pos_inf
    end.

%% math:sinh/1 is odd, so the overflow takes the sign of the argument.
sinh(X) ->
    try {j_float, math:sinh(X)}
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
    try {j_float, math:pow(Base, Exp)}
    catch error:badarith -> pow_non_finite(Base, Exp)
    end.

%% Zero base only reaches here with a negative Exp (a positive/zero Exp
%% returns a finite ±0 or 1 and never badariths). §6.1.6.1.3 steps 12–15:
%% -Infinity iff base is -0 AND Exp is an odd integer, otherwise +Infinity.
pow_non_finite(Base, Exp) when Base == 0.0 ->
    T = trunc(Exp),
    case neg_sign(Base) andalso T == Exp andalso T rem 2 =/= 0 of
        true -> j_neg_inf;
        false -> j_pos_inf
    end;
pow_non_finite(Base, Exp) ->
    case neg_sign(Base) of
        true ->
            T = trunc(Exp),
            if
                T /= Exp -> j_nan;
                T rem 2 =:= 0 -> j_pos_inf;
                true -> j_neg_inf
            end;
        false ->
            j_pos_inf
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
            {j_float, 0.0};
        false ->
            SumSq = lists:foldl(
                fun(V, Acc) -> R = V / Max, Acc + R * R end,
                0.0,
                Values
            ),
            try {j_float, Max * math:sqrt(SumSq)}
            catch error:badarith -> j_pos_inf
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
        <<0:1, 255:8, 0:23>> -> j_pos_inf;
        <<1:1, 255:8, 0:23>> -> j_neg_inf;
        <<F32:32/float>> -> {j_float, F32};
        _ -> j_nan
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
        true -> j_neg_inf;
        false -> j_pos_inf
    end.

%% ── JPure Math.* direct-dispatch fast paths (raytrace hot path) ────────────
%% JsVal wire form in, JsVal wire form out: bare int / bare float / js_nan /
%% js_inf / js_neg_inf. Any non-number receiver → `miss` (emitter coerces via
%% ToNumber and retries). ES §21.3.2.
%%
%% §21.3.2 does ToNumber on EVERY arg first (throws on Symbol/BigInt, runs
%% valueOf), so a short-circuit clause must NEVER wildcard-match a non-number
%% arg — that would return a value where the spec throws. Gate every `_` on
%% ?IS_NUMLIKE so non-number-shaped operands fall through to `miss`.
-define(IS_NUMLIKE(X),
        (is_number(X) orelse X =:= js_nan
         orelse X =:= js_inf orelse X =:= js_neg_inf)).

%% §21.3.2.32: sqrt(NaN)=NaN, sqrt(x<0)=NaN, sqrt(-0)=-0, sqrt(+∞)=+∞.
t_math_sqrt(X) when is_integer(X) -> t_math_sqrt(float(X));
t_math_sqrt(X) when is_float(X), X < 0.0 -> js_nan;
t_math_sqrt(X) when is_float(X) ->
    %% -0.0: math:sqrt(-0.0) already returns -0.0 on IEEE platforms.
    try math:sqrt(X) catch error:badarith -> js_nan end;
t_math_sqrt(js_nan) -> js_nan;
t_math_sqrt(js_inf) -> js_inf;
t_math_sqrt(js_neg_inf) -> js_nan;
t_math_sqrt(_) -> miss.

%% §21.3.2.16: floor(NaN)=NaN, floor(±∞)=±∞, floor(±0)=±0.
t_math_floor(X) when is_integer(X) -> X;
%% Integral results come back as bare ints (the JInt wire shape) so they hit
%% the integer index / smi fast paths; ±0 keeps its sign, and anything past
%% 2^53 is already integral so X passes through unchanged.
t_math_floor(X) when is_float(X) ->
    case floor(X) of
        0 -> case neg_sign(X) of true -> X; false -> 0 end;
        R when R > ?MAX_SAFE_INT; R < -?MAX_SAFE_INT -> X;
        R -> R
    end;
t_math_floor(js_nan) -> js_nan;
t_math_floor(js_inf) -> js_inf;
t_math_floor(js_neg_inf) -> js_neg_inf;
t_math_floor(_) -> miss.

%% §21.3.2.1: abs(NaN)=NaN, abs(-∞)=+∞, abs(-0)=+0.
t_math_abs(X) when is_integer(X) -> abs(X);
t_math_abs(X) when is_float(X) -> abs(X);
t_math_abs(js_nan) -> js_nan;
t_math_abs(js_inf) -> js_inf;
t_math_abs(js_neg_inf) -> js_inf;
t_math_abs(_) -> miss.

%% §21.3.2.26 → §6.1.6.1.3 Number::exponentiate. Reuse the total pow/2
%% badarith wrapper for the finite×finite case; spell out the non-finite
%% mixes ES defines that never reach math:pow/2.
t_math_pow(B, E) when is_number(B), is_number(E) ->
    Bf = as_float(B), Ef = as_float(E),
    case pow(Bf, Ef) of
        {j_float, F} -> F;
        j_pos_inf -> js_inf;
        j_neg_inf -> js_neg_inf;
        j_nan -> js_nan
    end;
t_math_pow(B, E) when E == 0, ?IS_NUMLIKE(B) -> 1;
t_math_pow(js_nan, E) when ?IS_NUMLIKE(E) -> js_nan;
t_math_pow(B, js_nan) when ?IS_NUMLIKE(B) -> js_nan;
t_math_pow(js_inf, E) when is_number(E) ->
    if E > 0 -> js_inf; E < 0 -> 0; true -> 1 end;
t_math_pow(js_neg_inf, E) when is_number(E) ->
    T = trunc(as_float(E)),
    Odd = T == E andalso T rem 2 =/= 0,
    if E > 0, Odd -> js_neg_inf; E > 0 -> js_inf;
       E < 0, Odd -> -0.0; E < 0 -> 0; true -> 1 end;
t_math_pow(B, js_inf) when is_number(B) ->
    A = abs(as_float(B)),
    if A > 1.0 -> js_inf; A < 1.0 -> 0; true -> js_nan end;
t_math_pow(B, js_neg_inf) when is_number(B) ->
    A = abs(as_float(B)),
    if A > 1.0 -> 0; A < 1.0 -> js_inf; true -> js_nan end;
t_math_pow(js_inf, js_inf) -> js_inf;
t_math_pow(js_inf, js_neg_inf) -> 0;
t_math_pow(js_neg_inf, js_inf) -> js_inf;
t_math_pow(js_neg_inf, js_neg_inf) -> 0;
t_math_pow(_, _) -> miss.

%% §21.3.2.25 min / §21.3.2.24 max: any NaN → NaN; -0 < +0.
t_math_min(js_nan, B) when ?IS_NUMLIKE(B) -> js_nan;
t_math_min(A, js_nan) when ?IS_NUMLIKE(A) -> js_nan;
t_math_min(js_neg_inf, B) when ?IS_NUMLIKE(B) -> js_neg_inf;
t_math_min(A, js_neg_inf) when ?IS_NUMLIKE(A) -> js_neg_inf;
t_math_min(js_inf, B) -> num_or_miss(B);
t_math_min(A, js_inf) -> num_or_miss(A);
t_math_min(A, B) when is_number(A), is_number(B) ->
    if A < B -> A; A > B -> B;
       true -> case is_neg_zero_v(A) of true -> A; false -> B end
    end;
t_math_min(_, _) -> miss.

t_math_max(js_nan, B) when ?IS_NUMLIKE(B) -> js_nan;
t_math_max(A, js_nan) when ?IS_NUMLIKE(A) -> js_nan;
t_math_max(js_inf, B) when ?IS_NUMLIKE(B) -> js_inf;
t_math_max(A, js_inf) when ?IS_NUMLIKE(A) -> js_inf;
t_math_max(js_neg_inf, B) -> num_or_miss(B);
t_math_max(A, js_neg_inf) -> num_or_miss(A);
t_math_max(A, B) when is_number(A), is_number(B) ->
    if A > B -> A; A < B -> B;
       true -> case is_neg_zero_v(A) of true -> B; false -> A end
    end;
t_math_max(_, _) -> miss.

as_float(X) when is_float(X) -> X;
as_float(X) when is_integer(X) -> float(X).

num_or_miss(X) when is_number(X) -> X;
num_or_miss(js_nan) -> js_nan;
num_or_miss(js_inf) -> js_inf;
num_or_miss(js_neg_inf) -> js_neg_inf;
num_or_miss(_) -> miss.

is_neg_zero_v(X) when is_float(X) -> X == 0.0 andalso neg_sign(X);
is_neg_zero_v(_) -> false.
