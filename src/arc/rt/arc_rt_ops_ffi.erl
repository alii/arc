%%% JsVal operator kernels shared by rt_ops, the interpreter's fused opcodes
%%% and the AOT number fast path. They match the raw JsVal wire term
%%% (arc_rt_val_ffi §2.3: bare ints/floats/binaries, `undefined | null | true
%%% | false | js_nan | js_inf | js_neg_inf | js_tdz` atoms, `{js_cell,N} |
%%% {js_bigint,N} | {js_sym,S}` tuples) and answer the result directly, or the
%%% atom `miss` when the operands need anything observable (ToPrimitive on an
%%% object, a Symbol's TypeError). They are TOTAL: no clause raises for any
%%% wire term.
%%%
%%% The BEAM has no IEEE infinities: float `+ - * /` and math:pow raise
%%% `badarith` the moment the true result passes 1.8e308, so every
%%% overflow-capable operation here catches that and answers the ±Infinity
%%% ES §6.1.6.1 requires. An integer result wider than 2^53 - 1 becomes the
%%% nearest double (arc_rt_val_ffi:mk_int/1).
-module(arc_rt_ops_ffi).
-export([add/2, sub/2, mul/2, 'div'/2, mod/2, neg/1, plus/1, step/2,
         lt/2, le/2, gt/2, ge/2, eq/2, neq/2, binop/3, pure_binop/3,
         t_add/3, t_sub/3, t_mul/3, t_div/3, t_mod/3, t_neg/2,
         pow_total/2, fmod_total/2, fadd/2, fsub/2, fmul/2, fdiv/2,
         t_eq_fast/2, nul_eq/1, strict_eq/2, strict_neq/2, strict_eq_i32/2,
         t_bitand_fast/2, t_bitor_fast/2, t_bitxor_fast/2,
         t_shl_fast/2, t_shr_fast/2, t_ushr_fast/2, t_bitnot_fast/1]).

%% ── 1. Number kernels (JsVal in, JsVal | miss out) ───────────────────────

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

%% add(A, B) -> JsVal | miss
%% §13.15.3 ApplyStringOrNumericBinaryOperator `+` for primitive operands:
%% number + number (integer arithmetic never overflows, so only the float
%% clause pays for a catch frame), string ++ string, and string with a
%% primitive whose ToString is pure. Objects (ToPrimitive), symbols
%% (TypeError) and BigInt mixes miss.
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

%% §6.1.6.1.7 Number::add rows with a NaN or ±Infinity operand; anything
%% that is not two Numbers misses.
nonfinite_add(js_nan, B) when ?IS_NUM(B) -> js_nan;
nonfinite_add(A, js_nan) when ?IS_NUM(A) -> js_nan;
nonfinite_add(js_inf, js_neg_inf) -> js_nan;
nonfinite_add(js_neg_inf, js_inf) -> js_nan;
nonfinite_add(js_inf, B) when is_number(B); B =:= js_inf -> js_inf;
nonfinite_add(js_neg_inf, B) when is_number(B); B =:= js_neg_inf -> js_neg_inf;
nonfinite_add(A, js_inf) when is_number(A) -> js_inf;
nonfinite_add(A, js_neg_inf) when is_number(A) -> js_neg_inf;
nonfinite_add(_, _) -> miss.

%% §7.1.17 ToString for the primitives where it observes nothing.
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

%% sub(A, B) -> JsVal | miss
%% §6.1.6.1.8 Number::subtract on two Numbers; everything else misses.
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

%% mul(A, B) -> JsVal | miss
%% §6.1.6.1.4 Number::multiply on two Numbers. An integer product of zero
%% takes the operands' sign (0 * -1 is -0); Infinity * 0 is NaN.
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

%% ±Infinity times a Number: zero gives NaN, otherwise the sign product.
inf_times(Inf, B) when is_number(B) ->
    case B == 0 of
        true -> js_nan;
        false -> inf_val((Inf =:= js_neg_inf) =/= num_is_negative(B))
    end;
inf_times(Inf, Inf) -> js_inf;
inf_times(_, B) when ?IS_INF(B) -> js_neg_inf;
inf_times(_, _) -> miss.

%% Sign of a finite Number term, reading the IEEE sign bit for -0.0.
num_is_negative(F) when is_float(F) ->
    F < 0.0 orelse arc_rt_val_ffi:is_neg_zero(F);
num_is_negative(N) -> N < 0.

%% div(A, B) -> JsVal | miss
%% §6.1.6.1.5 Number::divide on two Numbers. Exact integer quotients stay
%% integers; a zero dividend or divisor takes the IEEE sign rules (0 / -3 is
%% -0, 1 / 0 is Infinity, 0 / 0 is NaN); a float quotient past 1.8e308 is
%% ±Infinity.
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

%% x / ±0: NaN for a zero dividend, else Infinity signed by both operands.
zero_divisor(A, DivisorNeg) ->
    case A == 0 of
        true -> js_nan;
        false -> inf_val(num_is_negative(A) =/= DivisorNeg)
    end.

%% The rows with a NaN or ±Infinity operand: Infinity / Infinity is NaN,
%% Infinity / n keeps the sign product, n / Infinity is a signed zero.
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

%% mod(A, B) -> JsVal | miss
%% §6.1.6.1.6 Number::remainder on two finite Numbers: the sign follows the
%% dividend (Erlang `rem`, C fmod), so a zero result from a negative
%% dividend is -0; n % ±0 is NaN. Non-finite operands miss.
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

%% neg(A) -> JsVal | miss
%% §6.1.6.1.1 Number::unaryMinus. Integer 0 negates to -0.0; -0.0 to 0.
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

%% plus(A) -> JsVal | miss
%% §13.5.4 unary `+` (ToNumber): identity on Numbers, else miss.
plus(N) when is_number(N) -> N;
plus(A) when A =:= js_nan; ?IS_INF(A) -> A;
plus(_) -> miss.

%% step(A, Delta) -> JsVal | miss
%% `ToNumber(A) + Delta` for a Number A and a small integer Delta (the
%% `i++` / `i--` kernel): add(plus(A), Delta) without building the Delta
%% term. A finite float plus a small integer cannot overflow.
step(A, D) when is_integer(A) -> norm(A + D);
step(A, D) when is_float(A) -> A + D;
step(A, _) when A =:= js_nan; ?IS_INF(A) -> A;
step(_, _) -> miss.

%% lt/le/gt/ge(A, B) -> boolean() | miss
%% §7.2.13 IsLessThan for Number×Number (mixed int/float compare
%% numerically on the BEAM), String×String (byte order, matching rt/ops
%% D10) and BigInt×BigInt; NaN compares false; everything else misses.
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

%% Relational compare when at least one operand is NaN/±Infinity and both
%% are Numbers: rank -Infinity < finite < Infinity and compare ranks (two
%% finites never reach here). NaN is false under every operator.
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

%% strict_eq(A, B) -> boolean() | miss
%% §7.2.15 IsStrictlyEqual (as arc_rt_val_ffi:strict_eq/2) as an operator
%% kernel: a TDZ sentinel operand (the interpreter's fused compare-and-branch
%% ops read locals directly) misses so the slow path throws the
%% ReferenceError.
strict_eq(js_tdz, _) -> miss;
strict_eq(_, js_tdz) -> miss;
strict_eq(js_nan, _) -> false;
strict_eq(_, js_nan) -> false;
strict_eq(A, B) when is_number(A), is_number(B) -> A == B;
strict_eq(A, B) -> A =:= B.

%% strict_neq(A, B) -> boolean() | miss
strict_neq(js_tdz, _) -> miss;
strict_neq(_, js_tdz) -> miss;
strict_neq(js_nan, _) -> true;
strict_neq(_, js_nan) -> true;
strict_neq(A, B) when is_number(A), is_number(B) -> A /= B;
strict_neq(A, B) -> A =/= B.

%% eq(A, B) -> boolean() | miss
%% §7.2.14 IsLooselyEqual for the pairs that never run user code: null /
%% undefined against anything (steps 2-3, 14), same-type primitives, object
%% identity. Object×primitive (ToPrimitive), cross-type coercions and a TDZ
%% sentinel miss.
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

%% neq(A, B) -> boolean() | miss
neq(A, B) ->
    case eq(A, B) of
        miss -> miss;
        R -> not R
    end.

nul(undefined) -> true;
nul(null) -> true;
nul(_) -> false.

%% The PureBinOp dispatch, expanded into both binop/3 and pure_binop/3 so
%% the inlined kernels sit directly under one call.
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

%% binop(Kind, A, B) -> JsVal | miss
%% The kernel above for an `arc/bytecode/opcode.Classified` term (the BinOp
%% operand the resolver stores): `add_op | in_op | instance_of_op |
%% {pure_op, PureBinOp}`. `**`, `in` and `instanceof` need the heap or a
%% float pow and always miss here.
binop(add_op, A, B) -> add(A, B);
binop({pure_op, Op}, A, B) -> ?PURE_BINOP(Op, A, B);
binop(_, _, _) -> miss.

%% pure_binop(Op, A, B) -> JsVal | miss
%% binop/3 for an `arc/bytecode/binop.PureBinOp` term:
%% `{arith|bitwise|compare|equality, Op}`.
pure_binop(Op, A, B) -> ?PURE_BINOP(Op, A, B).

%% ── 2. One call per operator site ────────────────────────────────────────
%% The Number kernel above when the operands are BEAM numbers, else the full
%% operator in arc@rt@ops (non-finite atoms, strings, BigInt, ToNumeric /
%% ToPrimitive on anything else).

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

%% ── 3. Float kernels for rt/ops (two finite doubles in, JsNum out) ───────
%% JsNum results use the wire encoding of arc_rt_val_ffi:mk_number/1:
%% {j_float,F} | j_nan | j_pos_inf | j_neg_inf.

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

%% Division by zero never arrives here (num_div decides it first); what
%% overflows is a large dividend over a tiny divisor.
fdiv(X, Y) ->
    try {j_float, X / Y}
    catch error:badarith -> inf_num(is_negative(X) =/= is_negative(Y))
    end.

%% A sum only overflows when both operands are huge and share a sign, so
%% either operand's sign is the result's.
sum_is_negative(A, _B) -> is_negative(A).

is_negative(X) -> X < 0.

inf_num(false) -> j_pos_inf;
inf_num(true) -> j_neg_inf.

%% math:pow/2 raises badarith on overflow or on negative-base with a
%% non-integer exponent (no real result). Callers (num_exp) pre-filter ±0
%% base and the neg-base+non-integer case, so overflow gets the sign the
%% real result would have had — negative iff base < 0 and the integer
%% exponent is odd (§6.1.6.1.3). Port of arc_math_ffi:pow/2.
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

%% math:fmod with badarith (bignum-beyond-double, or platform quirk) → NaN.
fmod_total(A, B) ->
    try {j_float, math:fmod(A, B)}
    catch error:badarith -> j_nan
    end.

%% ── 4. i32-valued kernels for the AOT emitter's branches ─────────────────

%% t_eq_fast(A, B) -> 0 | 1 | miss
%% JPure §7.2.14 IsLooselyEqual fast path for the operand pairs richards
%% actually hits (~64k/run): x==null, int×int, object identity. Any pair
%% that reaches ToPrimitive (object×prim) or a cross-type coercion
%% (bool×num, num×str, bigint, non-finite) → `miss`; the emitter falls
%% back to full JMut t_eq/3. NOTE: null/undef vs anything (incl object)
%% is 0 by step 14 — never coerces — so those arms return 0, not miss.
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

%% nul_eq(V) -> 0 | 1
%% JPure `V == null` (§7.2.14 steps 2-3 + 14). Also called directly by
%% the emitter when one `==` operand is a literal null/undefined — total,
%% never `miss` (null/undef vs anything, incl objects, never coerces).
nul_eq(undefined) -> 1;
nul_eq(null) -> 1;
nul_eq(_) -> 0.

%% strict_eq_i32(A, B) -> 0 | 1
%% §7.2.15 IsStrictlyEqual (arc_rt_val_ffi:strict_eq/2) as an i32.
strict_eq_i32(A, B) ->
    case arc_rt_val_ffi:strict_eq(A, B) of true -> 1; false -> 0 end.

%% JPure §13.12 bitwise fast paths (richards: ~19k/run int32_binop, each
%% dragging 2×ToPrimitive + 2×ToNumeric behind it). Gate on both bare
%% integers (JInt wire form); do ToInt32 wrap + BIF op inline. Any
%% bigint/string/object → `miss`; the emitter falls back to full
%% JMut t_bit*/t_sh*. band/bor/bxor on i32-range operands stay i32-range
%% under Erlang's infinite two's-complement, so only the OPERANDS wrap.
%% w32 is force-inlined — the self-call showed at 34ns/call × 2 = a third
%% of the probe's wall time.
-compile({inline, [w32/1]}).
w32(I) ->
    case I band 16#FFFFFFFF of
        U when U > 16#7FFFFFFF -> U - 16#100000000;
        U -> U
    end.

%% The Number operands that are not bare integers: a float truncates toward
%% zero before the modulo-2^32 wrap (§7.1.6 ToInt32 / §7.1.7 ToUint32; the
%% `x | 0` and `x >>> 0` idioms exist to do exactly this) and NaN / ±Infinity
%% are 0. Anything else (BigInt, string, object, …) still misses.
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
%% §13.9.2: >> is arithmetic (sign-extend), shift count = ToUint32(b) & 31.
t_shr_fast(A, B) when is_integer(A), is_integer(B) ->
    w32(A) bsr (B band 31);
t_shr_fast(A, B) when ?IS_NUM(A), ?IS_NUM(B) -> i32(A) bsr (u32(B) band 31);
t_shr_fast(_, _) -> miss.
%% §13.9.1: << wraps the result (1<<31 = -2147483648).
t_shl_fast(A, B) when is_integer(A), is_integer(B) ->
    w32(w32(A) bsl (B band 31));
t_shl_fast(A, B) when ?IS_NUM(A), ?IS_NUM(B) ->
    w32(i32(A) bsl (u32(B) band 31));
t_shl_fast(_, _) -> miss.
%% §13.9.3: >>> is ToUint32 (unsigned) — band strips the sign so bsr on the
%% non-negative operand is logical; result stays in [0, 2^32).
t_ushr_fast(A, B) when is_integer(A), is_integer(B) ->
    (A band 16#FFFFFFFF) bsr (B band 31);
t_ushr_fast(A, B) when ?IS_NUM(A), ?IS_NUM(B) -> u32(A) bsr (u32(B) band 31);
t_ushr_fast(_, _) -> miss.
%% §13.5.8: ~a = -(ToInt32(a)+1). Erlang `bnot` on an i32-range int stays
%% i32-range (bnot X = -X-1).
t_bitnot_fast(A) when is_integer(A) -> bnot w32(A);
t_bitnot_fast(A) when ?IS_NUM(A) -> bnot i32(A);
t_bitnot_fast(_) -> miss.
