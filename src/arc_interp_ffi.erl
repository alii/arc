%%% arc_interp_ffi — the bytecode interpreter's Erlang shims.
%%%
%%% Three families, all pure term work over threaded values (no process
%%% state, no NIF):
%%%
%%%  1. guardN / guard_unitN — the raise adapter between the raise-based
%%%     runtime (`arc_rt_store_ffi:t_throw/2` raises
%%%     `error:{wasm_exn, 0, [Agent, Thrown]}`) and the Result-based
%%%     interpreter. `guardN(F, Agent, A2..AN)` applies the arity-N runtime
%%%     function and answers `{ok, V, Agent2}` or `{threw, Agent2, E}`, which
%%%     is the wire form of `arc/interp/ffi.Guarded`. Callers pass literal
%%%     remote fun refs (`fun 'arc@rt@obj':t_get_prop/3`), so no closure is
%%%     allocated per call.
%%%
%%%  2. Fused hot-path kernels (add/2, lt/2, get_field/3, ...) that match the
%%%     raw JsVal wire term (arc_rt_val_ffi §2.3: bare ints/floats/binaries,
%%%     `undefined | null | true | false | js_nan | js_inf | js_neg_inf |
%%%     js_tdz` atoms, `{js_cell,N} | {js_bigint,N} | {js_sym,S}` tuples) and
%%%     answer the result directly, or the atom `miss` when the operands need
%%%     anything observable (ToPrimitive on an object, a getter, a proxy
%%%     trap, a throw). They are TOTAL: no clause raises for any wire term.
%%%     The Gleam side types each kernel with its hit type and checks
%%%     `is_miss/1` before touching the result.
%%%
%%%  3. setup_locals_tuple/6, setup_locals_seeded/10 — the one-pass locals
%%%     tuple build for a call prologue.
-module(arc_interp_ffi).

-export([guard1/2, guard2/3, guard3/4, guard4/5, guard5/6, guard6/7,
         guard7/8,
         guard_unit1/2, guard_unit2/3, guard_unit3/4, guard_unit4/5,
         guard_unit5/6, guard_unit6/7]).
-export([is_miss/1, is_tdz/1,
         add/2, sub/2, mul/2, 'div'/2, mod/2, neg/1, plus/1,
         lt/2, le/2, gt/2, ge/2, strict_eq/2, eq/2,
         truthy/1, nullish/1, typeof/1, typeof/2,
         get_field/3, get_elem/3, put_field/4, put_elem/4]).
-export([setup_locals_tuple/6, setup_locals_seeded/10]).

-include("arc_rt_layout.hrl").

%% ── 1. raise adapter ─────────────────────────────────────────────────────

%% guardN(F, St, A2..AN) -> {ok, V, St2} | {threw, St2, E}
%% F is a value-first runtime function `F(St, ..) -> {V, St2}`. Only the JS
%% exception term is caught; engine panics and other errors propagate. The
%% `of` arm runs outside the protected region.
guard1(F, St) ->
    try F(St) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard2(F, St, A) ->
    try F(St, A) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard3(F, St, A, B) ->
    try F(St, A, B) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard4(F, St, A, B, C) ->
    try F(St, A, B, C) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard5(F, St, A, B, C, D) ->
    try F(St, A, B, C, D) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard6(F, St, A, B, C, D, X) ->
    try F(St, A, B, C, D, X) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard7(F, St, A, B, C, D, X, Y) ->
    try F(St, A, B, C, D, X, Y) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

%% guard_unitN(F, St, A2..AN) -> {ok, nil, St2} | {threw, St2, E}
%% Same, for runtime functions that return the bare Agent.
guard_unit1(F, St) ->
    try F(St) of St2 -> {ok, nil, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard_unit2(F, St, A) ->
    try F(St, A) of St2 -> {ok, nil, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard_unit3(F, St, A, B) ->
    try F(St, A, B) of St2 -> {ok, nil, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard_unit4(F, St, A, B, C) ->
    try F(St, A, B, C) of St2 -> {ok, nil, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard_unit5(F, St, A, B, C, D) ->
    try F(St, A, B, C, D) of St2 -> {ok, nil, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard_unit6(F, St, A, B, C, D, X) ->
    try F(St, A, B, C, D, X) of St2 -> {ok, nil, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

%% ── 2. fused kernels ─────────────────────────────────────────────────────

%% is_miss(X) -> boolean()
%% The one probe that knows a kernel result may be the `miss` atom instead
%% of its declared type. `miss` is not a JsVal wire term, so it can never
%% collide with a real value.
is_miss(miss) -> true;
is_miss(_) -> false.

%% is_tdz(V) -> boolean()
%% V is the TDZ sentinel `js_tdz` (an uninitialised let/const/class slot).
is_tdz(js_tdz) -> true;
is_tdz(_) -> false.

%% Number results keep the two invariants arc_rt_ops_ffi:add/2 keeps: an
%% integer wider than 2^53 - 1 becomes the nearest double, and float
%% overflow (badarith, the BEAM has no infinities) becomes ±Infinity.
-define(MAX_SAFE_INT, 9007199254740991).
-compile({inline, [norm/1, inf_val/1, nul/1]}).
norm(R) when R > ?MAX_SAFE_INT; R < -?MAX_SAFE_INT -> arc_rt_val_ffi:mk_int(R);
norm(R) -> R.

inf_val(false) -> js_inf;
inf_val(true) -> js_neg_inf.

%% add(A, B) -> JsVal | miss
%% §13.15.3 ApplyStringOrNumericBinaryOperator `+` for primitive operands:
%% number + number, string ++ string, and string with a primitive whose
%% ToString is pure. Objects (ToPrimitive), symbols (TypeError) and BigInt
%% mixes miss.
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
mul(js_nan, B) when is_number(B); B =:= js_nan;
                    B =:= js_inf; B =:= js_neg_inf -> js_nan;
mul(A, js_nan) when is_number(A); A =:= js_inf; A =:= js_neg_inf -> js_nan;
mul(A, B) when A =:= js_inf; A =:= js_neg_inf -> inf_times(A, B);
mul(A, B) when B =:= js_inf; B =:= js_neg_inf -> inf_times(B, A);
mul(_, _) -> miss.

%% ±Infinity times a Number: zero gives NaN, otherwise the sign product.
inf_times(Inf, B) when is_number(B) ->
    case B == 0 of
        true -> js_nan;
        false -> inf_val((Inf =:= js_neg_inf) =/= num_is_negative(B))
    end;
inf_times(Inf, Inf) -> js_inf;
inf_times(_, B) when B =:= js_inf; B =:= js_neg_inf -> js_neg_inf;
inf_times(_, _) -> miss.

%% Sign of a finite Number term, reading the IEEE sign bit for -0.0.
num_is_negative(F) when is_float(F) ->
    F < 0.0 orelse arc_rt_val_ffi:is_neg_zero(F);
num_is_negative(N) -> N < 0.

%% div(A, B) -> JsVal | miss
%% §6.1.6.1.5 Number::divide on two finite Numbers. Exact integer
%% quotients stay integers; a zero dividend or divisor takes the IEEE sign
%% rules (0 / -3 is -0, 1 / 0 is Infinity, 0 / 0 is NaN). Non-finite
%% operands miss (the sign table lives in rt/ops num_div).
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
'div'(_, _) -> miss.

%% x / ±0: NaN for a zero dividend, else Infinity signed by both operands.
zero_divisor(A, DivisorNeg) ->
    case A == 0 of
        true -> js_nan;
        false -> inf_val(num_is_negative(A) =/= DivisorNeg)
    end.

%% mod(A, B) -> JsVal | miss
%% §6.1.6.1.6 Number::remainder for two integers: sign follows the
%% dividend (Erlang `rem`), so a zero result from a negative dividend is -0;
%% n % 0 is NaN. Floats miss (fmod and its ±0/Infinity table are rt/ops).
mod(A, B) when is_integer(A), is_integer(B) ->
    case B of
        0 -> js_nan;
        _ ->
            case A rem B of
                0 when A < 0 -> -0.0;
                R -> R
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
plus(A) when A =:= js_nan; A =:= js_inf; A =:= js_neg_inf -> A;
plus(_) -> miss.

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

%% strict_eq(A, B) -> boolean()
%% §7.2.15 IsStrictlyEqual, total on wire terms. NaN is unequal to itself;
%% Numbers compare numerically (1 === 1.0, +0 === -0); every other row is
%% exact term identity (same atom, same binary, same {js_cell,N} /
%% {js_bigint,N} / {js_sym,S}).
strict_eq(js_nan, _) -> false;
strict_eq(_, js_nan) -> false;
strict_eq(A, B) when is_number(A), is_number(B) -> A == B;
strict_eq(A, B) -> A =:= B.

%% eq(A, B) -> boolean() | miss
%% §7.2.14 IsLooselyEqual for the pairs that never run user code: null /
%% undefined against anything (steps 2-3, 14), same-type primitives, object
%% identity. Object×primitive (ToPrimitive) and cross-type coercions miss.
eq(undefined, B) -> nul(B);
eq(null, B) -> nul(B);
eq(A, undefined) -> nul(A);
eq(A, null) -> nul(A);
eq({?HANDLE_TAG, A}, {?HANDLE_TAG, B}) -> A =:= B;
eq({?HANDLE_TAG, _}, _) -> miss;
eq(_, {?HANDLE_TAG, _}) -> miss;
eq(A, B) when is_number(A), is_number(B) -> A == B;
eq(A, B) when is_binary(A), is_binary(B) -> A =:= B;
eq(A, B) when is_boolean(A), is_boolean(B) -> A =:= B;
eq({js_bigint, A}, {js_bigint, B}) -> A =:= B;
eq({js_sym, A}, {js_sym, B}) -> A =:= B;
eq(js_nan, _) -> false;
eq(_, js_nan) -> false;
eq(A, B) when A =:= js_inf; A =:= js_neg_inf ->
    case B of
        A -> true;
        _ when is_number(B); B =:= js_inf; B =:= js_neg_inf -> false;
        _ -> miss
    end;
eq(A, B) when B =:= js_inf; B =:= js_neg_inf ->
    case is_number(A) of true -> false; false -> miss end;
eq(_, _) -> miss.

nul(undefined) -> true;
nul(null) -> true;
nul(_) -> false.

%% truthy(V) -> boolean()
%% §7.1.2 ToBoolean, total; row-for-row with arc_rt_val_ffi:to_boolean_i32.
truthy(undefined) -> false;
truthy(null) -> false;
truthy(false) -> false;
truthy(true) -> true;
truthy(0) -> false;
truthy(N) when is_integer(N) -> true;
truthy(F) when is_float(F) -> F /= 0.0;
truthy(js_nan) -> false;
truthy(js_inf) -> true;
truthy(js_neg_inf) -> true;
truthy(<<>>) -> false;
truthy(B) when is_binary(B) -> true;
truthy({js_bigint, 0}) -> false;
truthy({js_bigint, _}) -> true;
truthy({js_sym, _}) -> true;
truthy({?HANDLE_TAG, _}) -> true;
truthy(js_tdz) -> false.

%% nullish(V) -> boolean()
%% `V` is null or undefined (the `??` / `?.` / JumpIfNullish test). Total.
nullish(undefined) -> true;
nullish(null) -> true;
nullish(_) -> false.

%% typeof(V) -> binary() | miss
%% §13.5.3 for primitives. Objects need the store to tell "function" from
%% "object": use typeof/2, or take the miss.
typeof(undefined) -> <<"undefined">>;
typeof(null) -> <<"object">>;
typeof(B) when is_boolean(B) -> <<"boolean">>;
typeof(N) when is_number(N) -> <<"number">>;
typeof(A) when A =:= js_nan; A =:= js_inf; A =:= js_neg_inf -> <<"number">>;
typeof(B) when is_binary(B) -> <<"string">>;
typeof({js_bigint, _}) -> <<"bigint">>;
typeof({js_sym, _}) -> <<"symbol">>;
typeof(js_tdz) -> <<"undefined">>;
typeof(_) -> miss.

%% typeof(Store, V) -> binary() | miss
%% typeof/1 plus the object rows: a cell whose kind has [[Call]] is
%% "function", any other object cell "object". A Proxy answers from its
%% target (§10.5.14), so it misses rather than chase the chain here.
typeof(Store, {?HANDLE_TAG, Id}) ->
    case element(?STORE_DATA, Store) of
        #{Id := Slot} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case kind_tag(element(?SOBJECT_KIND, Slot)) of
                ?KFN_TAG -> <<"function">>;
                ?KBYTECODE_TAG -> <<"function">>;
                ?KNATIVE_TAG -> <<"function">>;
                k_bound -> <<"function">>;
                ?PROXYOBJ_TAG -> miss;
                _ -> <<"object">>
            end;
        #{Id := Slot} when element(1, Slot) =:= ?SSHAPED_TAG -> <<"object">>;
        _ -> miss
    end;
typeof(_Store, V) -> typeof(V).

%% The constructor atom of an ObjKind term (nullary variants are bare
%% atoms, payload variants are tagged tuples).
kind_tag(Kind) when is_atom(Kind) -> Kind;
kind_tag(Kind) -> element(1, Kind).

%% get_field(Store, V, KeyBin) -> JsVal | miss
%% §10.1.8.1 OrdinaryGet for a Named string key on an object cell, walking
%% the prototype chain while every hop is an ordinary read: an own slot on
%% an SShapedObject, or an own DataProperty in an SObject's props map for a
%% kind whose named keys are not virtual. Accessors, Proxy / module
%% namespace / TypedArray cells, Array and String "length", a dangling
%% handle, a primitive receiver, or more than 64 hops all miss. Absent on
%% the whole chain is `undefined`, exactly as OrdinaryGet answers.
%% KeyBin is a canonical Named key (the compiler emits Index keys for
%% array-index strings).
get_field(Store, {?HANDLE_TAG, Id}, KeyBin) ->
    field_walk(element(?STORE_DATA, Store), element(?STORE_SHAPES, Store),
               Id, KeyBin, 64);
get_field(_, _, _) -> miss.

field_walk(_, _, _, _, 0) -> miss;
field_walk(Data, Shapes, Id, KeyBin, Fuel) ->
    case Data of
        #{Id := {?SSHAPED_TAG, Sid, Proto, Slots}} ->
            case Shapes of
                #{Sid := Desc} ->
                    case element(?SHAPE_OFFSETS, Desc) of
                        #{KeyBin := Off} -> element(Off + 1, Slots);
                        _ -> field_next(Data, Shapes, Proto, KeyBin, Fuel)
                    end;
                _ -> miss
            end;
        #{Id := Slot} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case named_is_ordinary(element(?SOBJECT_KIND, Slot), KeyBin) of
                false -> miss;
                true ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_NAMED, KeyBin} := Prop} ->
                            case element(1, Prop) of
                                ?DATAPROP_TAG -> element(?DATAPROP_VALUE, Prop);
                                _ -> miss
                            end;
                        _ ->
                            field_next(Data, Shapes,
                                       element(?SOBJECT_PROTO, Slot),
                                       KeyBin, Fuel)
                    end
            end;
        _ -> miss
    end.

field_next(_, _, ?NONE, _, _) -> undefined;
field_next(Data, Shapes, {?SOME, {?HANDLE_TAG, P}}, KeyBin, Fuel) ->
    field_walk(Data, Shapes, P, KeyBin, Fuel - 1);
field_next(_, _, _, _, _) -> miss.

%% Whether a Named key on this ObjKind is a plain props-map entry for both
%% [[Get]] and [[Set]] (rt/obj own_property_of, get_from, set arms): Proxy,
%% module namespace and TypedArray cells are exotic for string keys, and
%% Array / String objects synthesize "length".
named_is_ordinary(?ORDINARY, _) -> true;
named_is_ordinary(Kind, _) when is_atom(Kind) -> true;
named_is_ordinary(Kind, KeyBin) ->
    case element(1, Kind) of
        ?PROXYOBJ_TAG -> false;
        module_namespace -> false;
        typed_array_obj -> false;
        ?ARRAYOBJ_TAG -> KeyBin =/= <<"length">>;
        string_obj -> KeyBin =/= <<"length">>;
        _ -> true
    end.

%% get_elem(Store, V, Key) -> JsVal | miss
%% `V[Key]` for the two shapes a loop body produces: a non-negative integer
%% index into an Array cell (own element present, no {index,_} props
%% override; holes miss so the full path does the proto walk), or a string
%% key, which canonicalizes and reads as get_field / an index. Anything
%% else (float or negative index, symbol, object key, non-array cell) misses.
get_elem(Store, {?HANDLE_TAG, Id}, Idx) when is_integer(Idx), Idx >= 0 ->
    case element(?STORE_DATA, Store) of
        #{Id := Slot} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                {?ARRAYOBJ_TAG, Length} when Idx < Length ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_INDEX, Idx} := _} -> miss;
                        _ -> elem_read(element(?SOBJECT_ELEMENTS, Slot), Idx)
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
get_elem(Store, {?HANDLE_TAG, _} = Obj, Key) when is_binary(Key) ->
    case arc_rt_val_ffi:t_to_property_key_fast(Key) of
        {?OKEY_STRING, {?KEY_NAMED, KeyBin}} -> get_field(Store, Obj, KeyBin);
        {?OKEY_STRING, {?KEY_INDEX, Idx}} -> get_elem(Store, Obj, Idx);
        _ -> miss
    end;
get_elem(_, _, _) -> miss.

elem_read({?ELEMS_DENSE, A}, Idx) ->
    case Idx < array:size(A) of
        true ->
            case array:get(Idx, A) of
                js_hole -> miss;
                V -> V
            end;
        false -> miss
    end;
elem_read({?ELEMS_SPARSE, M}, Idx) ->
    case M of
        #{Idx := V} -> V;
        _ -> miss
    end;
elem_read(_, _) -> miss.

%% put_field(Store, V, KeyBin, Val) -> Store2 | miss
%% §10.1.9.2 OrdinarySetWithOwnDescriptor step 2 for an EXISTING own
%% writable data property: overwrite the SShapedObject slot, or replace the
%% value inside the DataProperty (attributes and creation seq kept,
%% §10.1.11) for a kind whose named keys are ordinary. Property creation
%% (needs the proto-chain setter walk and a fresh seq), non-writable,
%% accessors and exotic receivers miss. Returns the rebuilt store.
put_field(Store, {?HANDLE_TAG, Id}, KeyBin, V) ->
    Data = element(?STORE_DATA, Store),
    case Data of
        #{Id := {?SSHAPED_TAG, Sid, P, Slots}} ->
            case element(?STORE_SHAPES, Store) of
                #{Sid := Desc} ->
                    case element(?SHAPE_OFFSETS, Desc) of
                        #{KeyBin := Off} ->
                            NewSlot = {?SSHAPED_TAG, Sid, P,
                                       setelement(Off + 1, Slots, V)},
                            setelement(?STORE_DATA, Store, Data#{Id := NewSlot});
                        _ -> miss
                    end;
                _ -> miss
            end;
        #{Id := Slot} when element(1, Slot) =:= ?SOBJECT_TAG ->
            case named_is_ordinary(element(?SOBJECT_KIND, Slot), KeyBin) of
                false -> miss;
                true ->
                    Props = element(?SOBJECT_PROPS, Slot),
                    K = {?KEY_NAMED, KeyBin},
                    case Props of
                        #{K := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG,
                               element(?DATAPROP_WRITABLE, Prop) =:= true ->
                            NewProps =
                                Props#{K := setelement(?DATAPROP_VALUE, Prop, V)},
                            NewSlot = setelement(?SOBJECT_PROPS, Slot, NewProps),
                            setelement(?STORE_DATA, Store, Data#{Id := NewSlot});
                        _ -> miss
                    end
            end;
        _ -> miss
    end;
put_field(_, _, _, _) -> miss.

%% put_elem(Store, V, Idx, Val) -> Store2 | miss
%% `V[Idx] = Val` on an extensible Array cell for an array index Idx
%% (0 =< Idx =< 2^32-2, rt_types.max_array_index) in [0, Length].
%% Overwriting a present element is a write to an own writable data
%% property. Filling a hole or appending at Idx == Length creates a
%% property, so it first needs the prototype chain to hold nothing at Idx
%% (a setter or read-only index up the chain takes the store, §10.1.9.2
%% step 2) and, for the append, a writable "length" (§10.4.2.1 step 2.h).
%% An {index,Idx} props override, a non-extensible or non-array receiver,
%% a key past the array-index range (2^32-1 is a Named key and never moves
%% "length"), or a dense fill past the allocated size misses.
-define(MAX_ARRAY_INDEX, 4294967294).
put_elem(Store, {?HANDLE_TAG, Id}, Idx, V)
  when is_integer(Idx), Idx >= 0, Idx =< ?MAX_ARRAY_INDEX ->
    Data = element(?STORE_DATA, Store),
    case Data of
        #{Id := Slot}
          when element(1, Slot) =:= ?SOBJECT_TAG,
               element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
            Props = element(?SOBJECT_PROPS, Slot),
            case element(?SOBJECT_KIND, Slot) of
                _ when is_map_key({?KEY_INDEX, Idx}, Props) -> miss;
                {?ARRAYOBJ_TAG, Length} when Idx < Length ->
                    Elems = element(?SOBJECT_ELEMENTS, Slot),
                    case elem_has(Elems, Idx)
                         orelse index_free(Data, element(?STORE_SHAPES, Store),
                                           element(?SOBJECT_PROTO, Slot), Idx, 64) of
                        false -> miss;
                        true ->
                            case elem_write(Elems, Idx, V) of
                                miss -> miss;
                                NewE ->
                                    NewSlot = setelement(?SOBJECT_ELEMENTS, Slot, NewE),
                                    setelement(?STORE_DATA, Store,
                                               Data#{Id := NewSlot})
                            end
                    end;
                {?ARRAYOBJ_TAG, Idx} ->
                    case length_writable(Props)
                         andalso index_free(Data, element(?STORE_SHAPES, Store),
                                            element(?SOBJECT_PROTO, Slot), Idx, 64) of
                        false -> miss;
                        true ->
                            case elem_write_grow(element(?SOBJECT_ELEMENTS, Slot), Idx, V) of
                                miss -> miss;
                                NewE ->
                                    NewSlot = setelement(?SOBJECT_ELEMENTS,
                                        setelement(?SOBJECT_KIND, Slot,
                                                   {?ARRAYOBJ_TAG, Idx + 1}),
                                        NewE),
                                    setelement(?STORE_DATA, Store,
                                               Data#{Id := NewSlot})
                            end
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
put_elem(_, _, _, _) -> miss.

%% The Array "length" attribute override, when defineProperty made one;
%% absent means the default writable length.
length_writable(#{{?KEY_NAMED, <<"length">>} := Prop})
  when element(1, Prop) =:= ?DATAPROP_TAG ->
    element(?DATAPROP_WRITABLE, Prop) =:= true;
length_writable(_) -> true.

%% index_free(Data, Shapes, Proto, Idx, Fuel) -> boolean()
%% No object on the prototype chain starting at Proto has an own property
%% at Idx, along hops whose index lookup is a pure props/elements probe.
%% A Proxy, String, TypedArray or namespace hop, a dangling handle, or more
%% than Fuel hops answer false.
index_free(_, _, ?NONE, _, _) -> true;
index_free(_, _, _, _, 0) -> false;
index_free(Data, Shapes, {?SOME, {?HANDLE_TAG, P}}, Idx, Fuel) ->
    case Data of
        #{P := {?SSHAPED_TAG, Sid, Proto, _Slots}} ->
            case Shapes of
                #{Sid := Desc} ->
                    (not is_map_key(integer_to_binary(Idx),
                                    element(?SHAPE_OFFSETS, Desc)))
                        andalso index_free(Data, Shapes, Proto, Idx, Fuel - 1);
                _ -> false
            end;
        #{P := Slot} when element(1, Slot) =:= ?SOBJECT_TAG ->
            index_is_plain(element(?SOBJECT_KIND, Slot))
                andalso (not is_map_key({?KEY_INDEX, Idx},
                                        element(?SOBJECT_PROPS, Slot)))
                andalso (not elem_has(element(?SOBJECT_ELEMENTS, Slot), Idx))
                andalso index_free(Data, Shapes, element(?SOBJECT_PROTO, Slot),
                                   Idx, Fuel - 1);
        _ -> false
    end;
index_free(_, _, _, _, _) -> false.

%% Whether an Index key on this ObjKind is answered by the props map plus
%% the elements store alone (rt/obj own_property_of): Proxy and namespace
%% cells trap, String objects expose their code units, TypedArrays their
%% buffer.
index_is_plain(Kind) when is_atom(Kind) -> true;
index_is_plain(Kind) ->
    case element(1, Kind) of
        ?PROXYOBJ_TAG -> false;
        module_namespace -> false;
        typed_array_obj -> false;
        string_obj -> false;
        _ -> true
    end.

%% A present (non-hole) element at Idx.
elem_has({?ELEMS_DENSE, A}, Idx) ->
    Idx < array:size(A) andalso array:get(Idx, A) =/= js_hole;
elem_has({?ELEMS_SPARSE, M}, Idx) -> is_map_key(Idx, M);
elem_has(_, _) -> false.

elem_write({?ELEMS_DENSE, A}, Idx, V) ->
    case Idx < array:size(A) of
        true -> {?ELEMS_DENSE, array:set(Idx, V, A)};
        false -> miss
    end;
elem_write({?ELEMS_SPARSE, M}, Idx, V) -> {?ELEMS_SPARSE, M#{Idx => V}};
elem_write(_, _, _) -> miss.

%% Append at Idx == Length. A dense array:set/3 extends past size(A) itself;
%% the gap and size bounds are rt/elements' dense-promotion policy, past
%% which the write belongs to the sparse representation (miss). An empty
%% store starts dense the way rt/elements `set` does.
-define(MAX_GAP, 1024).
-define(MAX_DENSE_INDEX, 10000000).
elem_write_grow({?ELEMS_DENSE, A}, Idx, V) ->
    case Idx - array:size(A) =< ?MAX_GAP andalso Idx < ?MAX_DENSE_INDEX of
        true -> {?ELEMS_DENSE, array:set(Idx, V, A)};
        false -> miss
    end;
elem_write_grow({?ELEMS_SPARSE, M}, Idx, V) -> {?ELEMS_SPARSE, M#{Idx => V}};
elem_write_grow(?ELEMS_NONE, Idx, V) when Idx =< ?MAX_GAP ->
    {?ELEMS_DENSE, array:set(Idx, V, array:new({default, js_hole}))};
elem_write_grow(_, _, _) -> miss.

%% ── 3. locals tuple build ────────────────────────────────────────────────

%% Build the locals tuple for a JS function call in one forward pass:
%%   [Env..., Seeds..., Args(padded/truncated to Arity)..., Undef × rest]
%% One body-recursive build + list_to_tuple instead of append / reverse
%% chains. Env is the closure's captured environment, a list or a tuple of
%% values. local_count is compiler-bounded, so non-tail recursion is fine.
setup_locals_tuple(Env, Seeds, Args, Arity, LocalCount, Undef) when is_tuple(Env) ->
    setup_locals_tuple(tuple_to_list(Env), Seeds, Args, Arity, LocalCount, Undef);
setup_locals_tuple(Env, Seeds, Args, Arity, LocalCount, Undef) ->
    list_to_tuple(locals_env(Env, Seeds, Args, Arity, LocalCount, Undef)).

%% Non-arrow locals build. Lexical is an arc/bytecode/lexical.LexicalSlots term
%% (that module owns the ordering, see lexical.all_lexical_refs):
%%   {owned_lexical_slots, Base} — all four owned, contiguous, in canonical
%%       order [this, active_func, home_object, new_target] starting at
%%       Base (== length(Env)); the hot clause writes the seeds inline right
%%       after the env values.
%%   no_lexical_slots — none at all.
%% `captured_lexical_slots` belongs to arrows, which go through
%% setup_locals_tuple/6; it is left unmatched on purpose: seeding call-time
%% values into captured slots (which hold parent box refs at
%% non-contiguous indices) would be silently wrong.
setup_locals_seeded(Env, Lexical, This, FnObj, Home, NT, Args, Arity,
                    LocalCount, Undef) when is_tuple(Env) ->
    setup_locals_seeded(tuple_to_list(Env), Lexical, This, FnObj, Home, NT,
                        Args, Arity, LocalCount, Undef);
setup_locals_seeded(Env, {owned_lexical_slots, _Base},
                    This, FnObj, Home, NT, Args, Arity, LocalCount, Undef)
        when LocalCount >= 4 ->
    list_to_tuple(locals_env4(Env, This, FnObj, Home, NT, Args, Arity,
                              LocalCount, Undef));
setup_locals_seeded(Env, Lexical,
                    This, FnObj, Home, NT, Args, Arity, LocalCount, Undef) ->
    {LT, LA, LH, LN} = case Lexical of
        {owned_lexical_slots, B} ->
            {{some, B}, {some, B + 1}, {some, B + 2}, {some, B + 3}};
        no_lexical_slots -> {none, none, none, none}
    end,
    S0 = seed(LN, NT, []),
    S1 = seed(LH, Home, S0),
    S2 = seed(LA, FnObj, S1),
    Seeds = seed(LT, This, S2),
    list_to_tuple(locals_env(Env, Seeds, Args, Arity, LocalCount, Undef)).

seed(none, _Value, Acc) -> Acc;
seed({some, _Idx}, Value, Acc) -> [Value | Acc].

locals_env4([E | Env], This, FnObj, Home, NT, Args, Arity, N, Undef) when N > 4 ->
    [E | locals_env4(Env, This, FnObj, Home, NT, Args, Arity, N - 1, Undef)];
locals_env4([], This, FnObj, Home, NT, Args, Arity, N, Undef) when N >= 4 ->
    [This, FnObj, Home, NT | locals_args(Args, Arity, N - 4, Undef)];
locals_env4(Env, This, FnObj, Home, NT, Args, Arity, N, Undef) ->
    %% local_count exhausted mid-env (compiler bounds local_count, so
    %% unreachable in practice): fall back to the generic truncation.
    locals_env(Env, [This, FnObj, Home, NT], Args, Arity, N, Undef).

locals_env(_, _, _, _, 0, _) -> [];
locals_env([E | Env], Seeds, Args, Arity, N, Undef) ->
    [E | locals_env(Env, Seeds, Args, Arity, N - 1, Undef)];
locals_env([], Seeds, Args, Arity, N, Undef) ->
    locals_seeds(Seeds, Args, Arity, N, Undef).

locals_seeds(_, _, _, 0, _) -> [];
locals_seeds([S | Seeds], Args, Arity, N, Undef) ->
    [S | locals_seeds(Seeds, Args, Arity, N - 1, Undef)];
locals_seeds([], Args, Arity, N, Undef) ->
    locals_args(Args, Arity, N, Undef).

locals_args(_, _, 0, _) -> [];
locals_args(_, 0, N, Undef) -> locals_pad(N, Undef);
locals_args([A | Args], Arity, N, Undef) ->
    [A | locals_args(Args, Arity - 1, N - 1, Undef)];
locals_args([], Arity, N, Undef) ->
    [Undef | locals_args([], Arity - 1, N - 1, Undef)].

locals_pad(0, _) -> [];
locals_pad(N, Undef) -> [Undef | locals_pad(N - 1, Undef)].
