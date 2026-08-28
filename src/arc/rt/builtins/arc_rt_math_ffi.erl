%% math:* badariths on overflow, so these return jsnum shapes
-module(arc_rt_math_ffi).
-export([exp/1, pow/2, cosh/1, sinh/1, hypot/1, fround/1,
         t_math_sqrt/1, t_math_floor/1, t_math_abs/1,
         t_math_pow/2, t_math_min/2, t_math_max/2, fast/2, is_miss/1]).

is_miss(V) -> V =:= miss.

%% plain number args straight to the kernels, else miss
fast(math_floor, [X | _]) -> t_math_floor(X);
fast(math_abs, [X | _]) -> t_math_abs(X);
fast(math_sqrt, [X | _]) -> t_math_sqrt(X);
fast(math_pow, [B, E | _]) -> t_math_pow(B, E);
fast(math_max, [A, B]) -> t_math_max(A, B);
fast(math_min, [A, B]) -> t_math_min(A, B);
fast(math_ceil, [X | _]) when is_integer(X) -> X;
fast(math_round, [X | _]) when is_integer(X) -> X;
fast(math_trunc, [X | _]) when is_integer(X) -> X;
fast(_, _) -> miss.

-define(MAX_SAFE_INT, 9007199254740991).

exp(X) ->
    try {j_float, math:exp(X)}
    catch error:badarith -> j_pos_inf
    end.

cosh(X) ->
    try {j_float, math:cosh(X)}
    catch error:badarith -> j_pos_inf
    end.

sinh(X) ->
    try {j_float, math:sinh(X)}
    catch error:badarith -> signed_infinity(X)
    end.

pow(Base, Exp) ->
    try {j_float, math:pow(Base, Exp)}
    catch error:badarith -> pow_non_finite(Base, Exp)
    end.

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

%% scale by max so the squares cannot overflow
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

%% decoding inf bits as :32/float would badmatch
fround(X) when is_float(X) ->
    case <<X:32/float>> of
        <<0:1, 255:8, 0:23>> -> j_pos_inf;
        <<1:1, 255:8, 0:23>> -> j_neg_inf;
        <<F32:32/float>> -> {j_float, F32};
        _ -> j_nan
    end;
fround(X) when is_integer(X) ->
    fround(float(X)).

%% sign bit, because -0.0 < 0 is false
neg_sign(X) when is_float(X) ->
    <<Sign:1, _:63>> = <<X:64/float>>,
    Sign =:= 1.

signed_infinity(X) ->
    case neg_sign(X) of
        true -> j_neg_inf;
        false -> j_pos_inf
    end.

%% never wildcard-match a non-number arg, spec throws there
-define(IS_NUMLIKE(X),
        (is_number(X) orelse X =:= js_nan
         orelse X =:= js_inf orelse X =:= js_neg_inf)).

t_math_sqrt(X) when is_number(X), X >= 0 -> math:sqrt(X);
t_math_sqrt(X) when is_number(X) -> js_nan;
t_math_sqrt(js_nan) -> js_nan;
t_math_sqrt(js_inf) -> js_inf;
t_math_sqrt(js_neg_inf) -> js_nan;
t_math_sqrt(_) -> miss.

t_math_floor(X) when is_integer(X) -> X;
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

t_math_abs(X) when is_integer(X) -> abs(X);
t_math_abs(X) when is_float(X) -> abs(X);
t_math_abs(js_nan) -> js_nan;
t_math_abs(js_inf) -> js_inf;
t_math_abs(js_neg_inf) -> js_inf;
t_math_abs(_) -> miss.

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
