-module(arc_number_ffi).
-export([
    format_to_fixed/2,
    format_to_exponential/2,
    format_to_exponential_auto/1,
    format_to_precision/2,
    js_number_to_string/1
]).

%% JS number formatting: ES2024 §6.1.6.1.20 Number::toString and the
%% Number.prototype.{toFixed,toExponential,toPrecision} algorithms
%% (§21.1.3.3, §21.1.3.2, §21.1.3.5).
%%
%% Two digit sources, both derived from the double itself rather than from a
%% pre-rounded intermediate string:
%%   * shortest_digits/1   — the shortest digit string that round-trips the
%%     double (Erlang's [short] / Ryu output), used wherever the spec asks
%%     for "k is as small as possible".
%%   * significant_exact/2 — the first P significant digits of the (near)
%%     exact decimal expansion, rounded once, half away from zero.
%% Fixed-point rounding goes through decimals_exact/2 for the same
%% single-rounding reason (see its doc comment).

%% Number.prototype.toFixed(fractionDigits) — ES2024 §21.1.3.3.
%% The Gleam caller guarantees |X| < 1e21 (step 10: larger magnitudes use
%% Number::toString instead) and 0 =< Digits =< 100.
format_to_fixed(X, Digits) ->
    with_abs(X, fun(A) -> list_to_binary(decimals_exact(A, Digits)) end).

%% Number.prototype.toExponential(fractionDigits) — ES2024 §21.1.3.2 with an
%% explicit fractionDigits (0..100): FractionDigits+1 significant digits.
format_to_exponential(X, FractionDigits) ->
    with_abs(X, fun(A) -> exponential_pos(A, FractionDigits) end).

%% Number.prototype.toExponential() with fractionDigits undefined — §21.1.3.2
%% step 6.c: as many significant digits as needed to round-trip the value
%% and no more ("k is as small as possible").
format_to_exponential_auto(X) ->
    with_abs(X, fun exponential_auto_pos/1).

%% Number.prototype.toPrecision(precision) — ES2024 §21.1.3.5, 1 =< P =< 100.
format_to_precision(X, Precision) ->
    with_abs(X, fun(A) -> precision_pos(A, Precision) end).

%% JS Number::toString(x, 10) per ES2024 §6.1.6.1.20 for a finite x.
%% -0 stringifies as "0" (the spec works on ℝ(x)), unlike the three
%% Number.prototype formatters above, which keep the sign — so this one
%% does not go through with_abs/2.
js_number_to_string(N) when is_float(N) ->
    case N == 0.0 of
        true -> <<"0">>;
        false when N < 0.0 -> <<"-", (js_positive_to_string(-N))/binary>>;
        false -> js_positive_to_string(N)
    end;
js_number_to_string(N) when is_integer(N) ->
    integer_to_binary(N).

%% ---------------------------------------------------------------------------
%% Shared sign prelude
%% ---------------------------------------------------------------------------

%% The three Number.prototype formatters all emit the absolute value and
%% prefix "-" when the input's IEEE 754 sign bit is set — including -0.0,
%% which compares equal to 0.0 and so needs the sign bit, not `<`.
%% Fmt receives a float with a clear sign bit and returns a binary.
with_abs(X, Fmt) ->
    <<Sign:1, _:63>> = <<X/float>>,
    case Sign of
        1 -> <<"-", (Fmt(-X))/binary>>;
        0 -> Fmt(X)
    end.

%% ---------------------------------------------------------------------------
%% Number::toString (radix 10)
%% ---------------------------------------------------------------------------

%% §6.1.6.1.20 steps 5-10 for a positive finite X, using its shortest
%% round-trip digits d1…dk and the leading digit's decimal exponent E
%% (the spec's e is E + 1).
js_positive_to_string(X) ->
    {Digits, E} = shortest_digits(X),
    K = length(Digits),
    if
        %% Step 6: k =< e =< 21 — integer notation, zero-padded to e digits.
        E >= K - 1, E =< 20 ->
            list_to_binary(Digits ++ lists:duplicate(E + 1 - K, $0));
        %% Step 7: 0 < e =< 21 — decimal point inside the digit string.
        E >= 0, E =< 20 ->
            {I, F} = lists:split(E + 1, Digits),
            list_to_binary(I ++ "." ++ F);
        %% Step 8: -6 < e =< 0 — leading "0." and -e zeros.
        E >= -6, E < 0 ->
            list_to_binary("0." ++ lists:duplicate(-E - 1, $0) ++ Digits);
        %% Steps 9-10: exponential notation.
        true ->
            format_exponential(Digits, E)
    end.

%% ---------------------------------------------------------------------------
%% toExponential
%% ---------------------------------------------------------------------------

%% F+1 significant digits of positive X in exponential notation.
exponential_pos(X, F) when X == 0.0 ->
    format_exponential(lists:duplicate(F + 1, $0), 0);
exponential_pos(X, F) ->
    {Digits, E} = significant_exact(X, F + 1),
    format_exponential(Digits, E).

%% Shortest round-trip digits of positive X in exponential notation.
exponential_auto_pos(X) when X == 0.0 ->
    <<"0e+0">>;
exponential_auto_pos(X) ->
    {Digits, E} = shortest_digits(X),
    format_exponential(Digits, E).

%% ---------------------------------------------------------------------------
%% toPrecision
%% ---------------------------------------------------------------------------

precision_pos(X, P) when X == 0.0 ->
    %% §21.1.3.5 step 9: m is p zeros and e is 0.
    format_precision(lists:duplicate(P, $0), 0, P);
precision_pos(X, P) ->
    {Digits, E} = significant_exact(X, P),
    format_precision(Digits, E, P).

%% §21.1.3.5 steps 10-12: fixed vs exponential is decided from the exponent
%% E of the ROUNDED p-significant-digit string, not of the raw value.
format_precision(Digits, E, P) when E < -6; E >= P ->
    %% Step 10: exponential notation.
    format_exponential(Digits, E);
format_precision(Digits, E, P) when E =:= P - 1 ->
    %% Step 11: exactly p integer digits.
    list_to_binary(Digits);
format_precision(Digits, E, _P) when E >= 0 ->
    %% Step 12.a: decimal point after e+1 digits.
    {I, F} = lists:split(E + 1, Digits),
    list_to_binary(I ++ "." ++ F);
format_precision(Digits, E, _P) ->
    %% Step 12.b: -6 =< e < 0 — leading "0." and -(e+1) zeros.
    list_to_binary("0." ++ lists:duplicate(-(E + 1), $0) ++ Digits).

%% ---------------------------------------------------------------------------
%% Digit extraction
%% ---------------------------------------------------------------------------

%% "d.ddd…e±n" from a significant-digit list and the leading digit's decimal
%% exponent: JS style, so no trailing "." for a single digit, no exponent
%% zero-padding, and an explicit "+" for non-negative exponents.
format_exponential([D | Rest], E) ->
    Frac = case Rest of
        [] -> "";
        _ -> [$. | Rest]
    end,
    Sign = case E < 0 of
        true -> $-;
        false -> $+
    end,
    list_to_binary([D, Frac, $e, Sign, integer_to_list(abs(E))]).

%% Decompose positive X into the shortest digit string that round-trips it
%% (leading and trailing zeros removed) and the decimal exponent E of its
%% leading digit: X = d1.d2…dk × 10^E.
shortest_digits(X) ->
    {Mantissa, E0} = split_exponent(float_to_list(X, [short])),
    [IntPart, FracPart] = string:split(Mantissa, "."),
    Combined = IntPart ++ FracPart,
    Lead = length(lists:takewhile(fun(C) -> C =:= $0 end, Combined)),
    Digits = string:trim(lists:nthtail(Lead, Combined), trailing, "0"),
    {Digits, length(IntPart) - 1 - Lead + E0}.

%% The first P significant decimal digits of positive X, rounded once, half
%% away from zero (the spec's "pick the larger n"), and the decimal exponent
%% E of the result's leading digit. Rounds the exact expansion (Erlang's
%% {scientific, N} formats via the libc's correctly-rounded "%.*e") with 30
%% guard digits, matching decimals_exact/2.
significant_exact(X, P) ->
    Sci = float_to_list(X, [{scientific, min(249, P + 30)}]),
    {Mantissa, E0} = split_exponent(Sci),
    [IntPart, FracPart] = string:split(Mantissa, "."),
    {Keep, Rest} = lists:split(P, IntPart ++ FracPart),
    RoundUp = case Rest of [C | _] when C >= $5 -> true; _ -> false end,
    Rounded = case RoundUp of
        true -> integer_to_list(list_to_integer(Keep) + 1);
        false -> Keep
    end,
    case length(Rounded) > P of
        %% Rounding carried into a new leading digit (e.g. 9.99 -> 10):
        %% keep P digits and bump the exponent.
        true -> {lists:sublist(Rounded, P), E0 + 1};
        false -> {Rounded, E0}
    end.

%% Split Erlang float text into its mantissa and integer exponent
%% ("1.5e-7" -> {"1.5", -7}); no exponent part means 0.
split_exponent(S) ->
    case string:split(S, "e") of
        [Mantissa, Exp] -> {Mantissa, list_to_integer(Exp)};
        [Mantissa] -> {Mantissa, 0}
    end.

%% float_to_list with {decimals, D} rounds from the float's shortest decimal
%% representation, double-rounding values like 1.3548387096774195 at 15
%% decimals ("…420" instead of the correct "…419"). Format with 30 guard
%% digits of the exact expansion and round once, half away from zero
%% (matching the spec's "pick the larger n").
decimals_exact(X, D) ->
    Wide = float_to_list(X, [{decimals, min(253, D + 30)}]),
    [IntPart, Frac] = string:split(Wide, "."),
    Keep = lists:sublist(Frac, D),
    Rest = lists:nthtail(D, Frac),
    RoundUp = case Rest of [C | _] when C >= $5 -> true; _ -> false end,
    Num0 = list_to_integer(IntPart ++ Keep),
    Num = case RoundUp of true -> Num0 + 1; false -> Num0 end,
    S = integer_to_list(Num),
    case D of
        0 -> S;
        _ ->
            Padded = lists:duplicate(max(0, D + 1 - length(S)), $0) ++ S,
            {I2, F2} = lists:split(length(Padded) - D, Padded),
            I2 ++ "." ++ F2
    end.
