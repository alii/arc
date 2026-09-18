%% kernels answer miss when the general path is needed
%% float ops raise badarith past 1.8e308, caught as infinity
-module(arc_rt_ops_ffi).
-export([add/2, sub/2, mul/2, 'div'/2, mod/2, neg/1, plus/1, step/2,
         lt/2, le/2, gt/2, ge/2, eq/2, neq/2, classified_binop/3, pure_binop/3,
         t_add/3, t_sub/3, t_mul/3, t_div/3, t_mod/3, t_neg/2,
         pow_total/2, fmod_total/2, fadd/2, fsub/2, fmul/2, fdiv/2,
         eq_i32/2, strict_eq/2, strict_neq/2, strict_eq_i32/2,
         bitand/2, bitor/2, bitxor/2,
         shl/2, shr/2, ushr/2, bitnot/1]).

-include("arc_rt_layout.hrl").

-compile({inline, [norm/1, inf_jsval/1, add/2, sub/2, mul/2,
                   'div'/2, mod/2, lt/2, le/2, gt/2, ge/2,
                   eq/2, neq/2, strict_eq/2, strict_neq/2,
                   bitand/2, bitor/2, bitxor/2,
                   shl/2, shr/2, ushr/2]}).
norm(R) when R > ?MAX_SAFE_INT; R < -?MAX_SAFE_INT -> arc_rt_val_ffi:mk_int(R);
norm(R) -> R.

inf_jsval(false) -> js_inf;
inf_jsval(true) -> js_neg_inf.

add(A, B) when is_integer(A), is_integer(B) -> norm(A + B);
add(A, B) when is_number(A), is_number(B) ->
    try A + B
    catch error:badarith -> inf_jsval(A < 0)
    end;
add(A, B) when is_binary(A), is_binary(B) -> <<A/binary, B/binary>>;
add(A, B) when ?IS_STR(A) ->
    case str_of(B) of
        miss -> miss;
        S -> arc_rt_js_string_ffi:concat(A, S)
    end;
add(A, B) when ?IS_STR(B) ->
    case str_of(A) of
        miss -> miss;
        S -> arc_rt_js_string_ffi:concat(S, B)
    end;
add({js_bigint, A}, {js_bigint, B}) -> {js_bigint, A + B};
add(A, B) -> nonfinite_add(A, B).

nonfinite_add(js_nan, B) when ?IS_JS_NUMBER(B) -> js_nan;
nonfinite_add(A, js_nan) when ?IS_JS_NUMBER(A) -> js_nan;
nonfinite_add(js_inf, js_neg_inf) -> js_nan;
nonfinite_add(js_neg_inf, js_inf) -> js_nan;
nonfinite_add(js_inf, B) when is_number(B); B =:= js_inf -> js_inf;
nonfinite_add(js_neg_inf, B) when is_number(B); B =:= js_neg_inf -> js_neg_inf;
nonfinite_add(A, js_inf) when is_number(A) -> js_inf;
nonfinite_add(A, js_neg_inf) when is_number(A) -> js_neg_inf;
nonfinite_add(_, _) -> miss.

str_of(S) when ?IS_STR(S) -> S;
str_of(N) when is_integer(N) -> integer_to_binary(N);
str_of(F) when is_float(F) -> arc_rt_val_ffi:js_format_float(F);
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
    catch error:badarith -> inf_jsval(A < 0)
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
    catch error:badarith -> inf_jsval((A < 0) =/= (B < 0))
    end;
mul({js_bigint, A}, {js_bigint, B}) -> {js_bigint, A * B};
mul(js_nan, B) when ?IS_JS_NUMBER(B) -> js_nan;
mul(A, js_nan) when ?IS_JS_NUMBER(A) -> js_nan;
mul(A, B) when ?IS_INF(A) -> inf_times(A, B);
mul(A, B) when ?IS_INF(B) -> inf_times(B, A);
mul(_, _) -> miss.

inf_times(Inf, B) when is_number(B) ->
    case B == 0 of
        true -> js_nan;
        false -> inf_jsval((Inf =:= js_neg_inf) =/= num_is_negative(B))
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
                inf_jsval(num_is_negative(A) =/= num_is_negative(B))
            end
    end;
'div'(A, B) -> nonfinite_div(A, B).

zero_divisor(A, DivisorNeg) ->
    case A == 0 of
        true -> js_nan;
        false -> inf_jsval(num_is_negative(A) =/= DivisorNeg)
    end.

nonfinite_div(js_nan, B) when ?IS_JS_NUMBER(B) -> js_nan;
nonfinite_div(A, js_nan) when ?IS_JS_NUMBER(A) -> js_nan;
nonfinite_div(A, B) when ?IS_INF(A), ?IS_INF(B) -> js_nan;
nonfinite_div(A, B) when ?IS_INF(A), is_number(B) ->
    inf_jsval((A =:= js_neg_inf) =/= num_is_negative(B));
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
lt(A, B) when ?IS_STR(A), ?IS_STR(B) -> arc_rt_js_string_ffi:text(A) < arc_rt_js_string_ffi:text(B);
lt({js_bigint, A}, {js_bigint, B}) -> A < B;
lt(A, B) -> cmp_nonfinite(A, B, lt).

le(A, B) when is_number(A), is_number(B) -> A =< B;
le(A, B) when is_binary(A), is_binary(B) -> A =< B;
le(A, B) when ?IS_STR(A), ?IS_STR(B) -> arc_rt_js_string_ffi:text(A) =< arc_rt_js_string_ffi:text(B);
le({js_bigint, A}, {js_bigint, B}) -> A =< B;
le(A, B) -> cmp_nonfinite(A, B, le).

gt(A, B) when is_number(A), is_number(B) -> A > B;
gt(A, B) when is_binary(A), is_binary(B) -> A > B;
gt(A, B) when ?IS_STR(A), ?IS_STR(B) -> arc_rt_js_string_ffi:text(A) > arc_rt_js_string_ffi:text(B);
gt({js_bigint, A}, {js_bigint, B}) -> A > B;
gt(A, B) -> cmp_nonfinite(A, B, gt).

ge(A, B) when is_number(A), is_number(B) -> A >= B;
ge(A, B) when is_binary(A), is_binary(B) -> A >= B;
ge(A, B) when ?IS_STR(A), ?IS_STR(B) -> arc_rt_js_string_ffi:text(A) >= arc_rt_js_string_ffi:text(B);
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
eq(undefined, B) -> ?IS_NULLISH(B);
eq(null, B) -> ?IS_NULLISH(B);
eq(A, undefined) -> ?IS_NULLISH(A);
eq(A, null) -> ?IS_NULLISH(A);
eq({?HANDLE_TAG, A}, {?HANDLE_TAG, B}) -> A =:= B;
eq({?HANDLE_TAG, _}, _) -> miss;
eq(_, {?HANDLE_TAG, _}) -> miss;
eq(A, B) when is_number(A), is_number(B) -> A == B;
eq(A, B) when ?IS_STR(A), ?IS_STR(B) -> A =:= B;
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

-define(PURE_BINOP(Op, A, B),
    case Op of
        {arith, sub} -> sub(A, B);
        {arith, mul} -> mul(A, B);
        {arith, 'div'} -> 'div'(A, B);
        {arith, mod} -> mod(A, B);
        {bitwise, bit_and} -> bitand(A, B);
        {bitwise, bit_or} -> bitor(A, B);
        {bitwise, bit_xor} -> bitxor(A, B);
        {bitwise, shift_left} -> shl(A, B);
        {bitwise, shift_right} -> shr(A, B);
        {bitwise, shift_right_unsigned} -> ushr(A, B);
        {compare, less} -> lt(A, B);
        {compare, less_eq} -> le(A, B);
        {compare, greater} -> gt(A, B);
        {compare, greater_eq} -> ge(A, B);
        {equality, strict_eq} -> strict_eq(A, B);
        {equality, strict_not_eq} -> strict_neq(A, B);
        {equality, loose_eq} -> eq(A, B);
        {equality, loose_not_eq} -> neq(A, B);
        _ -> miss
    end).

classified_binop(add_op, A, B) -> add(A, B);
classified_binop({pure_op, Op}, A, B) -> ?PURE_BINOP(Op, A, B);
classified_binop(_, _, _) -> miss.

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

%% a float sum only overflows when both terms share the sign of x
fadd(X, Y) ->
    try {j_float, X + Y}
    catch error:badarith -> inf_jsnum(is_negative(X))
    end.

fsub(X, Y) ->
    try {j_float, X - Y}
    catch error:badarith -> inf_jsnum(is_negative(X))
    end.

fmul(X, Y) ->
    try {j_float, X * Y}
    catch error:badarith -> inf_jsnum(is_negative(X) =/= is_negative(Y))
    end.

fdiv(X, Y) ->
    try {j_float, X / Y}
    catch error:badarith -> inf_jsnum(is_negative(X) =/= is_negative(Y))
    end.

is_negative(X) -> X < 0.

inf_jsnum(false) -> j_pos_inf;
inf_jsnum(true) -> j_neg_inf.

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

eq_i32(undefined, B) -> is_nullish_i32(B);
eq_i32(null, B) -> is_nullish_i32(B);
eq_i32(A, undefined) -> is_nullish_i32(A);
eq_i32(A, null) -> is_nullish_i32(A);
eq_i32(A, B) when is_number(A), is_number(B) ->
    case A == B of true -> 1; false -> 0 end;
eq_i32(A, B) when ?IS_STR(A), ?IS_STR(B) ->
    case A =:= B of true -> 1; false -> 0 end;
eq_i32({?HANDLE_TAG, A}, {?HANDLE_TAG, B}) ->
    case A =:= B of true -> 1; false -> 0 end;
eq_i32(A, B) when is_boolean(A), is_boolean(B) ->
    case A =:= B of true -> 1; false -> 0 end;
eq_i32(_, _) -> miss.

is_nullish_i32(V) ->
    case ?IS_NULLISH(V) of true -> 1; false -> 0 end.

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

bitand(A, B) when is_integer(A), is_integer(B) ->
    w32(A) band w32(B);
bitand(A, B) when ?IS_JS_NUMBER(A), ?IS_JS_NUMBER(B) -> i32(A) band i32(B);
bitand(_, _) -> miss.
bitor(A, B) when is_integer(A), is_integer(B) ->
    w32(A) bor w32(B);
bitor(A, B) when ?IS_JS_NUMBER(A), ?IS_JS_NUMBER(B) -> i32(A) bor i32(B);
bitor(_, _) -> miss.
bitxor(A, B) when is_integer(A), is_integer(B) ->
    w32(A) bxor w32(B);
bitxor(A, B) when ?IS_JS_NUMBER(A), ?IS_JS_NUMBER(B) -> i32(A) bxor i32(B);
bitxor(_, _) -> miss.
shr(A, B) when is_integer(A), is_integer(B) ->
    w32(A) bsr (B band 31);
shr(A, B) when ?IS_JS_NUMBER(A), ?IS_JS_NUMBER(B) -> i32(A) bsr (u32(B) band 31);
shr(_, _) -> miss.
shl(A, B) when is_integer(A), is_integer(B) ->
    w32(w32(A) bsl (B band 31));
shl(A, B) when ?IS_JS_NUMBER(A), ?IS_JS_NUMBER(B) ->
    w32(i32(A) bsl (u32(B) band 31));
shl(_, _) -> miss.
ushr(A, B) when is_integer(A), is_integer(B) ->
    (A band 16#FFFFFFFF) bsr (B band 31);
ushr(A, B) when ?IS_JS_NUMBER(A), ?IS_JS_NUMBER(B) -> u32(A) bsr (u32(B) band 31);
ushr(_, _) -> miss.
bitnot(A) when is_integer(A) -> bnot w32(A);
bitnot(A) when ?IS_JS_NUMBER(A) -> bnot i32(A);
bitnot(_) -> miss.
