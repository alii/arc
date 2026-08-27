%% kernels answer miss when the slow path is needed
%% float ops raise badarith past 1.8e308, caught as infinity
-module(arc_rt_ops_ffi).
-export([add/2, sub/2, mul/2, 'div'/2, mod/2, neg/1, plus/1, step/2,
         lt/2, le/2, gt/2, ge/2, eq/2, neq/2, binop/3, pure_binop/3,
         t_add/3, t_sub/3, t_mul/3, t_div/3, t_mod/3, t_neg/2,
         pow_total/2, fmod_total/2, fadd/2, fsub/2, fmul/2, fdiv/2,
         t_eq_fast/2, nul_eq/1, strict_eq/2, strict_neq/2, strict_eq_i32/2,
         t_bitand_fast/2, t_bitor_fast/2, t_bitxor_fast/2,
         t_shl_fast/2, t_shr_fast/2, t_ushr_fast/2, t_bitnot_fast/1]).

-define(MAX_SAFE_INT, 9007199254740991).
-define(IS_INF(X), (X =:= js_inf orelse X =:= js_neg_inf)).
-define(IS_NUM(X), (is_float(X) orelse is_integer(X) orelse X =:= js_nan
                    orelse ?IS_INF(X))).
-compile({inline, [norm/1, inf_val/1, nul/1, add/2, sub/2, mul/2,
                   'div'/2, mod/2, lt/2, le/2, gt/2, ge/2,
                   eq/2, neq/2, strict_eq/2, strict_neq/2,
                   t_bitand_fast/2, t_bitor_fast/2, t_bitxor_fast/2,
                   t_shl_fast/2, t_shr_fast/2, t_ushr_fast/2]}).
norm(R) when R > ?MAX_SAFE_INT; R < -?MAX_SAFE_INT -> arc_rt_val_ffi:mk_int(R);
norm(R) -> R.

inf_val(false) -> js_inf;
inf_val(true) -> js_neg_inf.

add(A, B) when is_integer(A), is_integer(B) -> norm(A + B);
add(A, B) when is_number(A), is_number(B) ->
    try A + B
    catch error:badarith -> inf_val(A < 0)
    end;
add(A, B) when is_binary(A), is_binary(B) -> <<A/binary, B/binary>>;
add(A, B) when is_binary(A) ->
    case str_of(B) of
        miss -> miss;
        S -> <<A/binary, S/binary>>
    end;
add(A, B) when is_binary(B) ->
    case str_of(A) of
        miss -> miss;
        S -> <<S/binary, B/binary>>
    end;
add({js_bigint, A}, {js_bigint, B}) -> {js_bigint, A + B};
add(A, B) -> nonfinite_add(A, B).

nonfinite_add(js_nan, B) when ?IS_NUM(B) -> js_nan;
nonfinite_add(A, js_nan) when ?IS_NUM(A) -> js_nan;
nonfinite_add(js_inf, js_neg_inf) -> js_nan;
nonfinite_add(js_neg_inf, js_inf) -> js_nan;
nonfinite_add(js_inf, B) when is_number(B); B =:= js_inf -> js_inf;
nonfinite_add(js_neg_inf, B) when is_number(B); B =:= js_neg_inf -> js_neg_inf;
nonfinite_add(A, js_inf) when is_number(A) -> js_inf;
nonfinite_add(A, js_neg_inf) when is_number(A) -> js_neg_inf;
nonfinite_add(_, _) -> miss.

str_of(N) when is_integer(N) -> integer_to_binary(N);
str_of(F) when is_float(F) -> arc_rt_val_ffi:js_number_to_string(F);
str_of(undefined) -> <<"undefined">>;
str_of(null) -> <<"null">>;
str_of(true) -> <<"true">>;
str_of(false) -> <<"false">>;
str_of(js_nan) -> <<"NaN">>;
str_of(js_inf) -> <<"Infinity">>;
str_of(js_neg_inf) -> <<"-Infinity">>;
str_of({js_bigint, N}) -> integer_to_binary(N);
str_of(_) -> miss.

sub(A, B) when is_integer(A), is_integer(B) -> norm(A - B);
sub(A, B) when is_number(A), is_number(B) ->
    try A - B
    catch error:badarith -> inf_val(A < 0)
    end;
sub({js_bigint, A}, {js_bigint, B}) -> {js_bigint, A - B};
sub(A, js_inf) -> nonfinite_add(A, js_neg_inf);
sub(A, js_neg_inf) -> nonfinite_add(A, js_inf);
sub(A, B) when is_number(B); B =:= js_nan -> nonfinite_add(A, B);
sub(_, _) -> miss.

mul(A, B) when is_integer(A), is_integer(B) ->
    case A * B of
        0 when A < 0; B < 0 -> -0.0;
        R -> norm(R)
    end;
mul(A, B) when is_number(A), is_number(B) ->
    try A * B
    catch error:badarith -> inf_val((A < 0) =/= (B < 0))
    end;
mul({js_bigint, A}, {js_bigint, B}) -> {js_bigint, A * B};
mul(js_nan, B) when ?IS_NUM(B) -> js_nan;
mul(A, js_nan) when ?IS_NUM(A) -> js_nan;
mul(A, B) when ?IS_INF(A) -> inf_times(A, B);
mul(A, B) when ?IS_INF(B) -> inf_times(B, A);
mul(_, _) -> miss.

inf_times(Inf, B) when is_number(B) ->
    case B == 0 of
        true -> js_nan;
        false -> inf_val((Inf =:= js_neg_inf) =/= num_is_negative(B))
    end;
inf_times(Inf, Inf) -> js_inf;
inf_times(_, B) when ?IS_INF(B) -> js_neg_inf;
inf_times(_, _) -> miss.

num_is_negative(F) when is_float(F) ->
    F < 0.0 orelse arc_rt_val_ffi:is_neg_zero(F);
num_is_negative(N) -> N < 0.

'div'(A, B) when is_integer(A), is_integer(B) ->
    if
        B =:= 0 -> zero_divisor(A, false);
        A =:= 0 -> case B < 0 of true -> -0.0; false -> 0 end;
        A rem B =:= 0 -> A div B;
        true -> A / B
    end;
'div'(A, B) when is_number(A), is_number(B) ->
    case B == 0 of
        true -> zero_divisor(A, num_is_negative(B));
        false ->
            try A / B
            catch error:badarith ->
                inf_val(num_is_negative(A) =/= num_is_negative(B))
            end
    end;
'div'(A, B) -> nonfinite_div(A, B).

zero_divisor(A, DivisorNeg) ->
    case A == 0 of
        true -> js_nan;
        false -> inf_val(num_is_negative(A) =/= DivisorNeg)
    end.

nonfinite_div(js_nan, B) when ?IS_NUM(B) -> js_nan;
nonfinite_div(A, js_nan) when ?IS_NUM(A) -> js_nan;
nonfinite_div(A, B) when ?IS_INF(A), ?IS_INF(B) -> js_nan;
nonfinite_div(A, B) when ?IS_INF(A), is_number(B) ->
    inf_val((A =:= js_neg_inf) =/= num_is_negative(B));
nonfinite_div(A, B) when is_number(A), ?IS_INF(B) ->
    case (B =:= js_neg_inf) =/= num_is_negative(A) of
        true -> -0.0;
        false -> 0.0
    end;
nonfinite_div(_, _) -> miss.

mod(A, B) when is_integer(A), is_integer(B) ->
    case B of
        0 -> js_nan;
        _ ->
            case A rem B of
                0 when A < 0 -> -0.0;
                R -> R
            end
    end;
mod(A, B) when is_number(A), is_number(B) ->
    case B == 0 of
        true -> js_nan;
        false ->
            try math:fmod(float(A), float(B))
            catch error:badarith -> js_nan
            end
    end;
mod(_, _) -> miss.

neg(0) -> -0.0;
neg(N) when is_integer(N) -> -N;
neg(F) when is_float(F) ->
    case F == 0.0 andalso arc_rt_val_ffi:is_neg_zero(F) of
        true -> 0;
        false -> -F
    end;
neg(js_nan) -> js_nan;
neg(js_inf) -> js_neg_inf;
neg(js_neg_inf) -> js_inf;
neg({js_bigint, N}) -> {js_bigint, -N};
neg(_) -> miss.

plus(N) when is_number(N) -> N;
plus(A) when A =:= js_nan; ?IS_INF(A) -> A;
plus(_) -> miss.

step(A, D) when is_integer(A) -> norm(A + D);
step(A, D) when is_float(A) -> A + D;
step(A, _) when A =:= js_nan; ?IS_INF(A) -> A;
step(_, _) -> miss.

lt(A, B) when is_number(A), is_number(B) -> A < B;
lt(A, B) when is_binary(A), is_binary(B) -> A < B;
lt({js_bigint, A}, {js_bigint, B}) -> A < B;
lt(A, B) -> cmp_nonfinite(A, B, lt).

le(A, B) when is_number(A), is_number(B) -> A =< B;
le(A, B) when is_binary(A), is_binary(B) -> A =< B;
le({js_bigint, A}, {js_bigint, B}) -> A =< B;
le(A, B) -> cmp_nonfinite(A, B, le).

gt(A, B) when is_number(A), is_number(B) -> A > B;
gt(A, B) when is_binary(A), is_binary(B) -> A > B;
gt({js_bigint, A}, {js_bigint, B}) -> A > B;
gt(A, B) -> cmp_nonfinite(A, B, gt).

ge(A, B) when is_number(A), is_number(B) -> A >= B;
ge(A, B) when is_binary(A), is_binary(B) -> A >= B;
ge({js_bigint, A}, {js_bigint, B}) -> A >= B;
ge(A, B) -> cmp_nonfinite(A, B, ge).

cmp_nonfinite(A, B, Op) ->
    case {num_rank(A), num_rank(B)} of
        {miss, _} -> miss;
        {_, miss} -> miss;
        {nan, _} -> false;
        {_, nan} -> false;
        {RA, RB} ->
            case Op of
                lt -> RA < RB;
                le -> RA =< RB;
                gt -> RA > RB;
                ge -> RA >= RB
            end
    end.

num_rank(js_neg_inf) -> -1;
num_rank(N) when is_number(N) -> 0;
num_rank(js_inf) -> 1;
num_rank(js_nan) -> nan;
num_rank(_) -> miss.

strict_eq(js_tdz, _) -> miss;
strict_eq(_, js_tdz) -> miss;
strict_eq(js_nan, _) -> false;
strict_eq(_, js_nan) -> false;
strict_eq(A, B) when is_number(A), is_number(B) -> A == B;
strict_eq(A, B) -> A =:= B.

strict_neq(js_tdz, _) -> miss;
strict_neq(_, js_tdz) -> miss;
strict_neq(js_nan, _) -> true;
strict_neq(_, js_nan) -> true;
strict_neq(A, B) when is_number(A), is_number(B) -> A /= B;
strict_neq(A, B) -> A =/= B.

eq(js_tdz, _) -> miss;
eq(_, js_tdz) -> miss;
eq(undefined, B) -> nul(B);
eq(null, B) -> nul(B);
eq(A, undefined) -> nul(A);
eq(A, null) -> nul(A);
eq({js_cell, A}, {js_cell, B}) -> A =:= B;
eq({js_cell, _}, _) -> miss;
eq(_, {js_cell, _}) -> miss;
eq(A, B) when is_number(A), is_number(B) -> A == B;
eq(A, B) when is_binary(A), is_binary(B) -> A =:= B;
eq(A, B) when is_boolean(A), is_boolean(B) -> A =:= B;
eq({js_bigint, A}, {js_bigint, B}) -> A =:= B;
eq({js_sym, A}, {js_sym, B}) -> A =:= B;
eq(js_nan, _) -> false;
eq(_, js_nan) -> false;
eq(A, B) when ?IS_INF(A) ->
    case B of
        A -> true;
        _ when is_number(B); ?IS_INF(B) -> false;
        _ -> miss
    end;
eq(A, B) when ?IS_INF(B) ->
    case is_number(A) of true -> false; false -> miss end;
eq(_, _) -> miss.

neq(A, B) ->
    case eq(A, B) of
        miss -> miss;
        R -> not R
    end.

nul(undefined) -> true;
nul(null) -> true;
nul(_) -> false.

-define(PURE_BINOP(Op, A, B),
    case Op of
        {arith, arith_sub} -> sub(A, B);
        {arith, arith_mul} -> mul(A, B);
        {arith, arith_div} -> 'div'(A, B);
        {arith, arith_mod} -> mod(A, B);
        {bitwise, and_op} -> t_bitand_fast(A, B);
        {bitwise, or_op} -> t_bitor_fast(A, B);
        {bitwise, xor_op} -> t_bitxor_fast(A, B);
        {bitwise, shl_op} -> t_shl_fast(A, B);
        {bitwise, shr_op} -> t_shr_fast(A, B);
        {bitwise, u_shr_op} -> t_ushr_fast(A, B);
        {compare, lt_cmp} -> lt(A, B);
        {compare, lt_eq_cmp} -> le(A, B);
        {compare, gt_cmp} -> gt(A, B);
        {compare, gt_eq_cmp} -> ge(A, B);
        {equality, strict_eq_op} -> strict_eq(A, B);
        {equality, strict_not_eq_op} -> strict_neq(A, B);
        {equality, eq_op} -> eq(A, B);
        {equality, not_eq_op} -> neq(A, B);
        _ -> miss
    end).

binop(add_op, A, B) -> add(A, B);
binop({pure_op, Op}, A, B) -> ?PURE_BINOP(Op, A, B);
binop(_, _, _) -> miss.

pure_binop(Op, A, B) -> ?PURE_BINOP(Op, A, B).

t_add(St, A, B) when is_number(A), is_number(B) -> {add(A, B), St};
t_add(St, A, B) -> 'arc@rt@ops':t_add(St, A, B).

t_sub(St, A, B) when is_number(A), is_number(B) -> {sub(A, B), St};
t_sub(St, A, B) -> 'arc@rt@ops':t_sub(St, A, B).

t_mul(St, A, B) when is_number(A), is_number(B) -> {mul(A, B), St};
t_mul(St, A, B) -> 'arc@rt@ops':t_mul(St, A, B).

t_div(St, A, B) when is_number(A), is_number(B) -> {'div'(A, B), St};
t_div(St, A, B) -> 'arc@rt@ops':t_div(St, A, B).

t_mod(St, A, B) when is_number(A), is_number(B) -> {mod(A, B), St};
t_mod(St, A, B) -> 'arc@rt@ops':t_mod(St, A, B).

t_neg(St, A) when is_number(A) -> {neg(A), St};
t_neg(St, A) -> 'arc@rt@ops':t_neg(St, A).

fadd(X, Y) ->
    try {j_float, X + Y}
    catch error:badarith -> inf_num(sum_is_negative(X, Y))
    end.

fsub(X, Y) ->
    try {j_float, X - Y}
    catch error:badarith -> inf_num(sum_is_negative(X, -Y))
    end.

fmul(X, Y) ->
    try {j_float, X * Y}
    catch error:badarith -> inf_num(is_negative(X) =/= is_negative(Y))
    end.

fdiv(X, Y) ->
    try {j_float, X / Y}
    catch error:badarith -> inf_num(is_negative(X) =/= is_negative(Y))
    end.

sum_is_negative(A, _B) -> is_negative(A).

is_negative(X) -> X < 0.

inf_num(false) -> j_pos_inf;
inf_num(true) -> j_neg_inf.

pow_total(Base, Exp) ->
    try {j_float, math:pow(Base, Exp)}
    catch error:badarith ->
        case Base < 0.0 of
            false -> j_pos_inf;
            true ->
                T = trunc(Exp),
                if
                    T /= Exp -> j_nan;
                    T rem 2 =:= 0 -> j_pos_inf;
                    true -> j_neg_inf
                end
        end
    end.

fmod_total(A, B) ->
    try {j_float, math:fmod(A, B)}
    catch error:badarith -> j_nan
    end.

t_eq_fast(undefined, B) -> nul_eq(B);
t_eq_fast(null, B) -> nul_eq(B);
t_eq_fast(A, undefined) -> nul_eq(A);
t_eq_fast(A, null) -> nul_eq(A);
t_eq_fast(A, B) when is_number(A), is_number(B) ->
    case A == B of true -> 1; false -> 0 end;
t_eq_fast(A, B) when is_binary(A), is_binary(B) ->
    case A =:= B of true -> 1; false -> 0 end;
t_eq_fast({js_cell, A}, {js_cell, B}) ->
    case A =:= B of true -> 1; false -> 0 end;
t_eq_fast(A, B) when is_boolean(A), is_boolean(B) ->
    case A =:= B of true -> 1; false -> 0 end;
t_eq_fast(_, _) -> miss.

nul_eq(undefined) -> 1;
nul_eq(null) -> 1;
nul_eq(_) -> 0.

strict_eq_i32(A, B) ->
    case arc_rt_val_ffi:strict_eq(A, B) of true -> 1; false -> 0 end.

-compile({inline, [w32/1]}).
w32(I) ->
    case I band 16#FFFFFFFF of
        U when U > 16#7FFFFFFF -> U - 16#100000000;
        U -> U
    end.

int_of(I) when is_integer(I) -> I;
int_of(F) when is_float(F) -> trunc(F);
int_of(_) -> 0.
i32(X) -> w32(int_of(X)).
u32(X) -> int_of(X) band 16#FFFFFFFF.

t_bitand_fast(A, B) when is_integer(A), is_integer(B) ->
    w32(A) band w32(B);
t_bitand_fast(A, B) when ?IS_NUM(A), ?IS_NUM(B) -> i32(A) band i32(B);
t_bitand_fast(_, _) -> miss.
t_bitor_fast(A, B) when is_integer(A), is_integer(B) ->
    w32(A) bor w32(B);
t_bitor_fast(A, B) when ?IS_NUM(A), ?IS_NUM(B) -> i32(A) bor i32(B);
t_bitor_fast(_, _) -> miss.
t_bitxor_fast(A, B) when is_integer(A), is_integer(B) ->
    w32(A) bxor w32(B);
t_bitxor_fast(A, B) when ?IS_NUM(A), ?IS_NUM(B) -> i32(A) bxor i32(B);
t_bitxor_fast(_, _) -> miss.
t_shr_fast(A, B) when is_integer(A), is_integer(B) ->
    w32(A) bsr (B band 31);
t_shr_fast(A, B) when ?IS_NUM(A), ?IS_NUM(B) -> i32(A) bsr (u32(B) band 31);
t_shr_fast(_, _) -> miss.
t_shl_fast(A, B) when is_integer(A), is_integer(B) ->
    w32(w32(A) bsl (B band 31));
t_shl_fast(A, B) when ?IS_NUM(A), ?IS_NUM(B) ->
    w32(i32(A) bsl (u32(B) band 31));
t_shl_fast(_, _) -> miss.
t_ushr_fast(A, B) when is_integer(A), is_integer(B) ->
    (A band 16#FFFFFFFF) bsr (B band 31);
t_ushr_fast(A, B) when ?IS_NUM(A), ?IS_NUM(B) -> u32(A) bsr (u32(B) band 31);
t_ushr_fast(_, _) -> miss.
t_bitnot_fast(A) when is_integer(A) -> bnot w32(A);
t_bitnot_fast(A) when ?IS_NUM(A) -> bnot i32(A);
t_bitnot_fast(_) -> miss.
