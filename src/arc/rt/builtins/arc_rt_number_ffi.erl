-module(arc_rt_number_ffi).
-export([
    format_to_fixed/2,
    format_to_exponential/2,
    format_to_exponential_auto/1,
    format_to_precision/2,
    format_float_radix/2,
    format_int_radix/2
]).

format_int_radix(I, Base) ->
    list_to_binary(lower(integer_to_list(I, Base))).

lower(S) -> [case C >= $A andalso C =< $Z of true -> C + 32; false -> C end || C <- S].

%% §21.1.3.3, caller ensures |x| < 1e21 and 0..100 digits
format_to_fixed(X, Digits) ->
    with_abs(X, fun(A) -> list_to_binary(decimals_exact(A, Digits)) end).

format_to_exponential(X, FractionDigits) ->
    with_abs(X, fun(A) -> exponential_pos(A, FractionDigits) end).

format_to_exponential_auto(X) ->
    with_abs(X, fun exponential_auto_pos/1).

format_to_precision(X, Precision) ->
    with_abs(X, fun(A) -> precision_pos(A, Precision) end).

%% -0 formats unsigned; fmt always sees positive or +0.0
with_abs(X, Fmt) ->
    case X < 0.0 of
        true -> <<"-", (Fmt(-X))/binary>>;
        false when X == 0.0 -> Fmt(0.0);
        false -> Fmt(X)
    end.

exponential_pos(X, F) when X == 0.0 ->
    format_exponential(lists:duplicate(F + 1, $0), 0);
exponential_pos(X, F) ->
    {Digits, E} = significant_exact(X, F + 1),
    format_exponential(Digits, E).

exponential_auto_pos(X) when X == 0.0 ->
    <<"0e+0">>;
exponential_auto_pos(X) ->
    {Digits, E} = arc_rt_float_ffi:shortest_digits(X),
    format_exponential(Digits, E).

precision_pos(X, P) when X == 0.0 ->
    format_precision(lists:duplicate(P, $0), 0, P);
precision_pos(X, P) ->
    {Digits, E} = significant_exact(X, P),
    format_precision(Digits, E, P).

%% §21.1.3.5 steps 10-12, e is of the rounded digits
format_precision(Digits, E, P) when E < -6; E >= P ->
    format_exponential(Digits, E);
format_precision(Digits, E, P) when E =:= P - 1 ->
    list_to_binary(Digits);
format_precision(Digits, E, _P) when E >= 0 ->
    {I, F} = lists:split(E + 1, Digits),
    list_to_binary(I ++ "." ++ F);
format_precision(Digits, E, _P) ->
    list_to_binary("0." ++ lists:duplicate(-(E + 1), $0) ++ Digits).

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

%% first p significant digits, rounded once half away from zero
significant_exact(X, P) ->
    Sci = arc_rt_float_ffi:scientific(X, min(249, P + 30)),
    {Mantissa, E0} = arc_rt_float_ffi:split_exponent(Sci),
    {IntPart, FracPart} = arc_rt_float_ffi:split_dot(Mantissa),
    {Keep, Rest} = lists:split(P, IntPart ++ FracPart),
    RoundUp = case Rest of [C | _] when C >= $5 -> true; _ -> false end,
    Rounded = case RoundUp of
        true -> integer_to_list(list_to_integer(Keep) + 1);
        false -> Keep
    end,
    case length(Rounded) > P of
        true -> {lists:sublist(Rounded, P), E0 + 1};
        false -> {Rounded, E0}
    end.

%% exact: n = round-half-up(m * 2^e * 10^d) in integers
decimals_exact(X, D) ->
    <<_:1, BE:11, F:52>> = <<X/float>>,
    {M, E} = case BE of
        0 -> {F, -1074};
        _ -> {F + (1 bsl 52), BE - 1075}
    end,
    P = pow10(D, 1),
    N = case E >= 0 of
        true -> (M bsl E) * P;
        false -> (M * P + (1 bsl (-E - 1))) bsr -E
    end,
    S = integer_to_list(N),
    case D of
        0 -> S;
        _ ->
            Padded = lists:duplicate(max(0, D + 1 - length(S)), $0) ++ S,
            {I2, F2} = lists:split(length(Padded) - D, Padded),
            I2 ++ "." ++ F2
    end.

pow10(0, Acc) -> Acc;
pow10(D, Acc) -> pow10(D - 1, Acc * 10).

format_float_radix(F, Base) ->
    with_abs(F, fun(A) -> radix_pos(A, Base) end).

radix_pos(A, Base) ->
    Int = trunc(A),
    Frac = A - float(Int),
    {FracDigits, Carry} =
        case Frac > 0.0 of
            false -> {[], false};
            true ->
                %% half a ulp: past this digits stop distinguishing a
                Delta = max(0.5 * (next_double(A) - A), next_double(0.0)),
                case Frac >= Delta of
                    false -> {[], false};
                    true -> fraction_loop(Frac, Delta, Base, [])
                end
        end,
    IntPart = case Carry of true -> Int + 1; false -> Int end,
    IntStr = lower(integer_to_list(IntPart, Base)),
    FracStr = case FracDigits of
        [] -> "";
        Ds -> [$. | [radix_digit(D) || D <- Ds]]
    end,
    list_to_binary(IntStr ++ FracStr).

fraction_loop(Frac, Delta, Base, Acc) ->
    BaseF = float(Base),
    Scaled = Frac * BaseF,
    Delta1 = Delta * BaseF,
    D = trunc(Scaled),
    Frac1 = Scaled - float(D),
    Acc1 = [D | Acc],
    RoundUp = (Frac1 > 0.5) orelse (Frac1 == 0.5 andalso (D band 1) =:= 1),
    case RoundUp andalso Frac1 + Delta1 > 1.0 of
        true -> propagate_carry(Acc1, Base);
        false ->
            case Frac1 >= Delta1 of
                true -> fraction_loop(Frac1, Delta1, Base, Acc1);
                false -> {lists:reverse(Acc1), false}
            end
    end.

propagate_carry([], _Base) -> {[], true};
propagate_carry([D | Rest], Base) when D + 1 =:= Base ->
    propagate_carry(Rest, Base);
propagate_carry([D | Rest], _Base) ->
    {lists:reverse([D + 1 | Rest]), false}.

next_double(X) ->
    <<Bits:64>> = <<X/float>>,
    <<Y/float>> = <<(Bits + 1):64>>,
    Y.

radix_digit(D) when D < 10 -> $0 + D;
radix_digit(D) -> $a + D - 10.
