%%% Total (badarith-safe) float kernels for rt_ops and the AOT number fast
%%% path. The BEAM has no IEEE infinities: float `+ - * /` and math:pow raise
%%% `badarith` the moment the true result passes 1.8e308, so every
%%% overflow-capable operation here catches that and answers the ±Infinity
%%% ES §6.1.6.1 requires. JsNum results use the wire encoding of
%%% arc_rt_val_ffi:mk_number/1: {j_float,F} | j_nan | j_pos_inf | j_neg_inf.
-module(arc_rt_ops_ffi).
-export([pow_total/2, fmod_total/2, fadd/2, fsub/2, fmul/2, fdiv/2,
         t_eq_fast/2, nul_eq/1,
         add/2, sub/2, mul/2, t_add/3, t_sub/3, t_mul/3,
         num_div/2, num_mod/2, num_neg/1, t_div/3, t_mod/3, t_neg/2,
         t_bitand_fast/2, t_bitor_fast/2, t_bitxor_fast/2,
         t_shl_fast/2, t_shr_fast/2, t_ushr_fast/2, t_bitnot_fast/1,
         num_add/2, num_sub/2, num_mul/2, int_mod/2, add_prim/2,
         strict_eq/2]).

%% JPure `+ - *` on two BEAM number terms (the emitter's is_number-guarded
%% arm), returning a JsVal: a bare number, or the `js_inf`/`js_neg_inf` atom
%% when a float result overflows. Two Number invariants an exact BEAM integer
%% cannot carry by itself are restored here: a result wider than 2^53 - 1
%% becomes the nearest double (arc_rt_val_ffi:mk_int/1), and an integer
%% product of zero takes the sign of its operands (§6.1.6.1.4: 0 * -1 is -0).
%% Integer arithmetic never overflows, so only the float clauses pay for a
%% catch frame.
-define(MAX_SAFE_INT, 9007199254740991).
-compile({inline, [norm/1, add/2, sub/2, mul/2]}).
norm(R) when R > ?MAX_SAFE_INT; R < -?MAX_SAFE_INT -> arc_rt_val_ffi:mk_int(R);
norm(R) -> R.

add(A, B) when is_integer(A), is_integer(B) -> norm(A + B);
add(A, B) ->
    try A + B
    catch error:badarith -> inf_val(sum_is_negative(A, B))
    end.

sub(A, B) when is_integer(A), is_integer(B) -> norm(A - B);
sub(A, B) ->
    try A - B
    catch error:badarith -> inf_val(sum_is_negative(A, -B))
    end.

mul(A, B) when is_integer(A), is_integer(B) ->
    case A * B of
        0 when A < 0; B < 0 -> -0.0;
        R -> norm(R)
    end;
mul(A, B) ->
    try A * B
    catch error:badarith -> inf_val(is_negative(A) =/= is_negative(B))
    end.

%% JS `+ - *` as one call per site: the two-number kernel above when both
%% operands are BEAM numbers, else the full §13.15.4 operator in arc@rt@ops.
t_add(St, A, B) when is_number(A), is_number(B) -> {add(A, B), St};
t_add(St, A, B) -> 'arc@rt@ops':t_add(St, A, B).

t_sub(St, A, B) when is_number(A), is_number(B) -> {sub(A, B), St};
t_sub(St, A, B) -> 'arc@rt@ops':t_sub(St, A, B).

t_mul(St, A, B) when is_number(A), is_number(B) -> {mul(A, B), St};
t_mul(St, A, B) -> 'arc@rt@ops':t_mul(St, A, B).

%% num_div(A, B) -> JsVal
%% §6.1.6.1.5 Number::divide, total over two Number JsVals (finite BEAM
%% numbers or js_nan / js_inf / js_neg_inf).
%% Exact integer quotients stay integers; a zero dividend or divisor takes
%% the IEEE sign rules (0 / -3 is -0, 1 / 0 is Infinity, 0 / 0 is NaN); a
%% float quotient past 1.8e308 is ±Infinity.
num_div(A, B) when is_integer(A), is_integer(B) ->
    if
        B =:= 0 -> zero_divisor(A, false);
        A =:= 0 -> case B < 0 of true -> -0.0; false -> 0 end;
        A rem B =:= 0 -> A div B;
        true -> A / B
    end;
num_div(A, B) when is_number(A), is_number(B) ->
    case B == 0 of
        true -> zero_divisor(A, num_is_negative(B));
        false ->
            try A / B
            catch error:badarith ->
                inf_val(num_is_negative(A) =/= num_is_negative(B))
            end
    end;
num_div(js_nan, _) -> js_nan;
num_div(_, js_nan) -> js_nan;
num_div(A, B) when is_atom(A), is_atom(B) -> js_nan;
num_div(A, B) when is_atom(A) -> inf_val((A =:= js_neg_inf) =/= num_is_negative(B));
num_div(A, B) ->
    case (B =:= js_neg_inf) =/= num_is_negative(A) of
        true -> -0.0;
        false -> 0.0
    end.

%% x / ±0: NaN for a zero dividend, else Infinity signed by both operands.
zero_divisor(A, DivisorNeg) ->
    case A == 0 of
        true -> js_nan;
        false -> inf_val(num_is_negative(A) =/= DivisorNeg)
    end.

%% Sign of a finite Number term, reading the IEEE sign bit for -0.0.
num_is_negative(F) when is_float(F) ->
    F < 0.0 orelse arc_rt_val_ffi:is_neg_zero(F);
num_is_negative(N) -> N < 0.

%% num_mod(A, B) -> JsVal
%% §6.1.6.1.6 Number::remainder on two finite Numbers: the sign follows the
%% dividend (Erlang `rem`, C fmod), so a zero result from a negative
%% dividend is -0; n % ±0 is NaN.
num_mod(A, B) when is_integer(A), is_integer(B) ->
    case B of
        0 -> js_nan;
        _ ->
            case A rem B of
                0 when A < 0 -> -0.0;
                R -> R
            end
    end;
num_mod(_, B) when B == 0 -> js_nan;
num_mod(A, B) ->
    try math:fmod(float(A), float(B))
    catch error:badarith -> js_nan
    end.

%% num_neg(A) -> JsVal
%% §6.1.6.1.1 Number::unaryMinus on a Number term (finite or not). Integer
%% 0 negates to -0.0; -0.0 to 0.
num_neg(0) -> -0.0;
num_neg(N) when is_integer(N) -> -N;
num_neg(F) when is_float(F) ->
    case F == 0.0 andalso arc_rt_val_ffi:is_neg_zero(F) of
        true -> 0;
        false -> -F
    end;
num_neg(js_nan) -> js_nan;
num_neg(js_inf) -> js_neg_inf;
num_neg(js_neg_inf) -> js_inf.

%% JS `/`, `%` and unary `-` as one call per site: the Number kernel above
%% when the operands are BEAM numbers, else the full operator in arc@rt@ops
%% (non-finite atoms, BigInt, ToNumeric on anything else).
t_div(St, A, B) when is_number(A), is_number(B) -> {num_div(A, B), St};
t_div(St, A, B) -> 'arc@rt@ops':t_div(St, A, B).

t_mod(St, A, B) when is_number(A), is_number(B) -> {num_mod(A, B), St};
t_mod(St, A, B) -> 'arc@rt@ops':t_mod(St, A, B).

t_neg(St, A) when is_number(A) -> {num_neg(A), St};
t_neg(St, A) -> 'arc@rt@ops':t_neg(St, A).

%% The same four operations over two finite doubles, as JsNum.
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

inf_val(false) -> js_inf;
inf_val(true) -> js_neg_inf.

inf_num(false) -> j_pos_inf;
inf_num(true) -> j_neg_inf.

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

%% JPure §13.12 bitwise fast paths (richards: ~19k/run int32_binop, each
%% dragging 2×ToPrimitive + 2×ToNumeric behind it). Gate on both bare
%% integers (JInt wire form); do ToInt32 wrap + BIF op inline. Any
%% float/nan/±inf/bigint/object → `miss`; the emitter falls back to full
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
-define(IS_NUM(X), (is_float(X) orelse is_integer(X) orelse X =:= js_nan
                    orelse X =:= js_inf orelse X =:= js_neg_inf)).
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

%% ── AOT operator kernels (Pure, wire terms, no St) ───────────────────────

%% num_add / num_sub / num_mul(A, B) -> JsVal
%% `add`/`sub`/`mul` made total over two Number JsVals: an operand that is
%% already js_nan / js_inf / js_neg_inf (an earlier overflow the emitter
%% still counts as a known number) takes the §6.1.6.1.7-9 non-finite rows
%% instead of reaching BEAM arithmetic.
num_add(A, B) when is_number(A), is_number(B) -> add(A, B);
num_add(A, B) -> nonfinite_add(A, B).

num_sub(A, B) when is_number(A), is_number(B) -> sub(A, B);
num_sub(A, B) -> nonfinite_add(A, num_neg(B)).

num_mul(A, B) when is_number(A), is_number(B) -> mul(A, B);
num_mul(js_nan, _) -> js_nan;
num_mul(_, js_nan) -> js_nan;
num_mul(A, B) when is_atom(A) -> inf_times(A, B);
num_mul(A, B) -> inf_times(B, A).

%% ±Infinity times a Number: zero gives NaN, otherwise the sign product.
inf_times(Inf, B) when is_number(B) ->
    case B == 0 of
        true -> js_nan;
        false -> inf_val((Inf =:= js_neg_inf) =/= num_is_negative(B))
    end;
inf_times(Inf, Other) -> inf_val((Inf =:= js_neg_inf) =/= (Other =:= js_neg_inf)).

%% int_mod(A, B) -> JsVal | miss
%% §6.1.6.1.6 Number::remainder for two integers: sign follows the
%% dividend, so a zero result from a negative dividend is -0; n % 0 is NaN.
%% Anything else misses to the full operator.
int_mod(A, B) when is_integer(A), is_integer(B) ->
    case B of
        0 -> js_nan;
        _ ->
            case A rem B of
                0 when A < 0 -> -0.0;
                R -> R
            end
    end;
int_mod(_, _) -> miss.

%% add_prim(A, B) -> JsVal | miss
%% §13.15.3 `+` for primitive operands: number + number, string ++ string,
%% and string with a primitive whose ToString observes nothing. Objects
%% (ToPrimitive), symbols (TypeError) and BigInt mixes miss.
add_prim(A, B) when is_number(A), is_number(B) -> add(A, B);
add_prim(A, B) when is_binary(A), is_binary(B) -> <<A/binary, B/binary>>;
add_prim(A, B) when is_binary(A) ->
    case str_of(B) of
        miss -> miss;
        S -> <<A/binary, S/binary>>
    end;
add_prim(A, B) when is_binary(B) ->
    case str_of(A) of
        miss -> miss;
        S -> <<S/binary, B/binary>>
    end;
add_prim({js_bigint, A}, {js_bigint, B}) -> {js_bigint, A + B};
add_prim(A, B) -> nonfinite_add(A, B).

%% §6.1.6.1.7 Number::add rows with a NaN or ±Infinity operand; anything
%% that is not two Numbers misses.
nonfinite_add(js_nan, B) when is_number(B); B =:= js_nan;
                              B =:= js_inf; B =:= js_neg_inf -> js_nan;
nonfinite_add(A, js_nan) when is_number(A);
                              A =:= js_inf; A =:= js_neg_inf -> js_nan;
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

%% strict_eq(A, B) -> 0 | 1
%% §7.2.15 IsStrictlyEqual on wire terms: Numbers compare numerically (so
%% 1 === 1.0 and 0 === -0), NaN equals nothing, every other value is equal
%% only to the identical term (strings are binaries, objects are handles).
strict_eq(A, B) when is_number(A), is_number(B) ->
    case A == B of true -> 1; false -> 0 end;
strict_eq(js_nan, _) -> 0;
strict_eq(_, js_nan) -> 0;
strict_eq(A, B) ->
    case A =:= B of true -> 1; false -> 0 end.
