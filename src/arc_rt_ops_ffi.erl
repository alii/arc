%%% Total (badarith-safe) float kernels for rt_ops and the AOT number fast
%%% path. The BEAM has no IEEE infinities: float `+ - * /` and math:pow raise
%%% `badarith` the moment the true result passes 1.8e308, so every
%%% overflow-capable operation here catches that and answers the ±Infinity
%%% ES §6.1.6.1 requires. JsNum results use the wire encoding of
%%% arc_rt_val_ffi:mk_number/1: {j_float,F} | j_nan | j_pos_inf | j_neg_inf.
-module(arc_rt_ops_ffi).
-export([pow_total/2, fmod_total/2, fadd/2, fsub/2, fmul/2, fdiv/2,
         t_eq_fast/2, nul_eq/1,
         add/2, sub/2, mul/2,
         t_bitand_fast/2, t_bitor_fast/2, t_bitxor_fast/2,
         t_shl_fast/2, t_shr_fast/2, t_ushr_fast/2, t_bitnot_fast/1]).

%% JPure `+ - *` on two BEAM number terms (the emitter's is_number-guarded
%% arm), returning a JsVal: a bare number, or the `js_inf`/`js_neg_inf` atom
%% when a float result overflows. Two Number invariants an exact BEAM integer
%% cannot carry by itself are restored here: a result wider than 2^53 - 1
%% becomes the nearest double (arc_rt_val_ffi:mk_int/1), and an integer
%% product of zero takes the sign of its operands (§6.1.6.1.4: 0 * -1 is -0).
%% Integer arithmetic never overflows, so only the float clauses pay for a
%% catch frame.
-define(MAX_SAFE_INT, 9007199254740991).
-compile({inline, [norm/1]}).
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

t_bitand_fast(A, B) when is_integer(A), is_integer(B) ->
    w32(A) band w32(B);
t_bitand_fast(_, _) -> miss.
t_bitor_fast(A, B) when is_integer(A), is_integer(B) ->
    w32(A) bor w32(B);
t_bitor_fast(_, _) -> miss.
t_bitxor_fast(A, B) when is_integer(A), is_integer(B) ->
    w32(A) bxor w32(B);
t_bitxor_fast(_, _) -> miss.
%% §13.9.2: >> is arithmetic (sign-extend), shift count = ToUint32(b) & 31.
t_shr_fast(A, B) when is_integer(A), is_integer(B) ->
    w32(A) bsr (B band 31);
t_shr_fast(_, _) -> miss.
%% §13.9.1: << wraps the result (1<<31 = -2147483648).
t_shl_fast(A, B) when is_integer(A), is_integer(B) ->
    w32(w32(A) bsl (B band 31));
t_shl_fast(_, _) -> miss.
%% §13.9.3: >>> is ToUint32 (unsigned) — band strips the sign so bsr on the
%% non-negative operand is logical; result stays in [0, 2^32).
t_ushr_fast(A, B) when is_integer(A), is_integer(B) ->
    (A band 16#FFFFFFFF) bsr (B band 31);
t_ushr_fast(_, _) -> miss.
%% §13.5.8: ~a = -(ToInt32(a)+1). Erlang `bnot` on an i32-range int stays
%% i32-range (bnot X = -X-1).
t_bitnot_fast(A) when is_integer(A) -> bnot w32(A);
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
