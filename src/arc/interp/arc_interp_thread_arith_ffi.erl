%% threaded code for arithmetic, comparisons and the branches on them
-module(arc_interp_thread_arith_ffi).
-export([binop/6, fuses_put/1, binop_put/6, compare/7, step/6, step_compare/8,
         of_type/4, branch/2, branch_local/4, unary/3]).

-include("arc_interp_thread.hrl").

-define(INTS, (is_integer(X) andalso is_integer(Y))).
-define(NUMBERS, (is_number(X) andalso is_number(Y))).
%% an integer in these bounds is a small, so arithmetic on it needs no more tests
-define(WITHIN(N, Bits),
        (is_integer(N) andalso (N) >= -(1 bsl (Bits)) andalso (N) < 1 bsl (Bits))).
%% two of these add up to a safe integer
-define(HALVES, (is_integer(X) andalso X > -(1 bsl 52) andalso X < 1 bsl 52
                 andalso ?WITHIN(Y, 52))).
-define(INT32S, (?WITHIN(X, 31) andalso ?WITHIN(Y, 31))).
%% a zero product may be -0
-define(FACTORS, (?WITHIN(X, 26) andalso ?WITHIN(Y, 26) andalso X * Y =/= 0)).
%% §7.1.6 toint32 of an integer
-define(INT32(N), ((((N) + 16#80000000) band 16#FFFFFFFF) - 16#80000000)).

%% X op Y onto the stack. The operands are the top two of the stack, or the top
%% and a constant or local, or a local and a constant or local
-define(BINOP(Op, Guard, Expr, General),
binop(Op, stack, _, _, N, Pc) when ?IS_OP(N) ->
    ?FN ->
        [X | St1] = St,
        Y = T,
        if Guard -> ?GO(N, Expr, St1);
           true -> General(X, St1, ?ENV, N, Pc, Y, Op)
        end
    end;
binop(Op, const, Y, _, N, Pc) when ?IS_OP(N) ->
    ?FN ->
        X = T,
        if Guard -> ?GO(N, Expr, St);
           true -> General(X, St, ?ENV, N, Pc, Y, Op)
        end
    end;
binop(Op, local, J, _, N, Pc) when ?IS_OP(N) ->
    ?FN ->
        X = T,
        Y = ?RD(J),
        if Guard -> ?GO(N, Expr, St);
           true -> General(X, St, ?ENV, N, Pc, Y, Op)
        end
    end;
binop(Op, local_local, {I, J}, S, N, Pc) when ?IS_OP(N) ->
    ?PUSHING(S, begin
        X = ?RD(I),
        Y = ?RD(J),
        if Guard -> ?GO(N, Expr, St);
           true -> General(X, St, ?ENV, N, Pc, Y, Op)
        end
    end);
binop(Op, local_const, {I, Y}, S, N, Pc) when ?IS_OP(N) ->
    ?PUSHING(S, begin
        X = ?RD(I),
        if Guard -> ?GO(N, Expr, St);
           true -> General(X, St, ?ENV, N, Pc, Y, Op)
        end
    end)).

%% a constant known to be a small integer
-define(BINOP_INT(Op, Known, Guard, Expr, General),
binop(Op, const, Y, _, N, Pc) when ?IS_OP(N), Known ->
    ?FN ->
        X = T,
        if Guard -> ?GO(N, Expr, St);
           true -> General(X, St, ?ENV, N, Pc, Y, Op)
        end
    end;
binop(Op, local_const, {I, Y}, S, N, Pc) when ?IS_OP(N), Known ->
    ?BY_REGISTER(I, Here, ?PUSHING(S, begin
        X = ?RD(Here),
        if Guard -> ?GO(N, Expr, St);
           true -> General(X, St, ?ENV, N, Pc, Y, Op)
        end
    end))).

?BINOP_INT(add, ?WITHIN(Y, 31), ?WITHIN(X, 52), X + Y, add);
?BINOP_INT(sub, ?WITHIN(Y, 31), ?WITHIN(X, 52), X - Y, sub);
?BINOP_INT(mul, ?WITHIN(Y, 20) andalso Y > 0, ?WITHIN(X, 32), X * Y, mul);
?BINOP_INT(mod, ?WITHIN(Y, 31) andalso Y > 0, ?WITHIN(X, 52) andalso X >= 0, X rem Y, general);
?BINOP_INT(bit_and, ?WITHIN(Y, 31) andalso Y >= 0, ?WITHIN(X, 52), X band Y, general);
?BINOP_INT(shift_right, ?WITHIN(Y, 5) andalso Y >= 0, ?WITHIN(X, 31), X bsr Y, bits);
?BINOP_INT(shift_left, ?WITHIN(Y, 4) andalso Y >= 0, ?WITHIN(X, 31), ?INT32(X bsl Y), general);
?BINOP(add, ?HALVES, X + Y, add);
?BINOP(sub, ?HALVES, X - Y, sub);
?BINOP(mul, ?FACTORS, X * Y, mul);
?BINOP('div', ?INTS andalso X > 0 andalso Y > 0 andalso X rem Y =:= 0, X div Y, divide);
?BINOP(mod, ?INTS andalso X >= 0 andalso Y > 0, X rem Y, general);
?BINOP(bit_and, ?INT32S, X band Y, bits);
?BINOP(bit_or, ?INT32S, X bor Y, bits);
?BINOP(bit_xor, ?INT32S, X bxor Y, bits);
%% drops the bits that would leave, so this is never a bignum
?BINOP(shift_left, ?HALVES,
       ?INT32((X band (16#FFFFFFFF bsr (Y band 31))) bsl (Y band 31)), general);
?BINOP(shift_right, ?INT32S, X bsr (Y band 31), bits);
?BINOP(shift_right_unsigned, ?HALVES, (X band 16#FFFFFFFF) bsr (Y band 31), general);
?BINOP(less, ?NUMBERS, X < Y, general);
?BINOP(less_eq, ?NUMBERS, X =< Y, general);
?BINOP(greater, ?NUMBERS, X > Y, general);
?BINOP(greater_eq, ?NUMBERS, X >= Y, general);
?BINOP(strict_eq, ?INTS, X =:= Y, equal);
?BINOP(strict_not_eq, ?INTS, X =/= Y, unequal);
?BINOP(loose_eq, ?INTS, X =:= Y, equal);
?BINOP(loose_not_eq, ?INTS, X =/= Y, unequal);
binop(Op, stack, _, _, N, Pc) when ?IS_OP(N) ->
    ?FN ->
        [X | St1] = St,
        general(X, St1, ?ENV, N, Pc, T, Op)
    end;
binop(Op, const, Y, _, N, Pc) when ?IS_OP(N) -> ?FN -> general(T, St, ?ENV, N, Pc, Y, Op) end;
binop(Op, local, J, _, N, Pc) when ?IS_OP(N) -> ?FN -> general(T, St, ?ENV, N, Pc, ?RD(J), Op) end;
binop(Op, local_local, {I, J}, S, N, Pc) when ?IS_OP(N) ->
    ?PUSHING(S, general(?RD(I), St, ?ENV, N, Pc, ?RD(J), Op));
binop(Op, local_const, {I, Y}, S, N, Pc) when ?IS_OP(N) ->
    ?PUSHING(S, general(?RD(I), St, ?ENV, N, Pc, Y, Op)).

%% float ops raise badarith past 1.8e308, which the general path turns into infinity
-define(FLOATS(Name, Expr),
Name(X, St, ?ENV, N, Pc, Y, Op) when is_float(X) ->
    if is_float(Y) ->
           try Expr of
               V -> ?GO(N, V, St)
           catch
               error:badarith -> general(X, St, ?ENV, N, Pc, Y, Op)
           end;
       is_integer(Y) -> Name(X, St, ?ENV, N, Pc, float(Y), Op);
       true -> general(X, St, ?ENV, N, Pc, Y, Op)
    end;
Name(X, St, ?ENV, N, Pc, Y, Op) when is_integer(X), is_float(Y) ->
    Name(float(X), St, ?ENV, N, Pc, Y, Op)).

?FLOATS(add, X + Y);
add(X, St, ?ENV, N, _, Y, _) when is_binary(X), is_binary(Y) -> ?GO(N, ?JOINED(X, Y), St);
add(X, St, ?ENV, N, _, Y, _) when is_binary(X), is_integer(Y) ->
    Digits = integer_to_binary(Y),
    ?GO(N, ?JOINED(X, Digits), St);
add(X, St, ?ENV, N, Pc, Y, Op) -> general(X, St, ?ENV, N, Pc, Y, Op).

?FLOATS(sub, X - Y);
sub(X, St, ?ENV, N, Pc, Y, Op) -> general(X, St, ?ENV, N, Pc, Y, Op).

?FLOATS(mul, X * Y);
mul(X, St, ?ENV, N, Pc, Y, Op) -> general(X, St, ?ENV, N, Pc, Y, Op).

?FLOATS(divide, X / Y);
divide(X, St, ?ENV, N, Pc, Y, Op) -> general(X, St, ?ENV, N, Pc, Y, Op).

%% the pairs that == and === agree on
-define(ALIKE(Same, Different, Otherwise),
        case X of
            {?HANDLE_TAG, Id} ->
                case Y of
                    {?HANDLE_TAG, Id} -> Same;
                    {?HANDLE_TAG, _} -> Different;
                    undefined -> Different;
                    null -> Different;
                    _ -> Otherwise
                end;
            _ when is_binary(X), is_binary(Y) ->
                if X =:= Y -> Same;
                   true -> Different
                end;
            _ when ?IS_NULLISH(Y), (is_binary(X) orelse is_number(X)) -> Different;
            _ -> Otherwise
        end).

equal(X, St, ?ENV, N, Pc, Y, Op) ->
    ?ALIKE(?GO(N, true, St), ?GO(N, false, St), general(X, St, ?ENV, N, Pc, Y, Op)).

unequal(X, St, ?ENV, N, Pc, Y, Op) ->
    ?ALIKE(?GO(N, false, St), ?GO(N, true, St), general(X, St, ?ENV, N, Pc, Y, Op)).

bits(X, St, ?ENV, N, _, Y, Op) when ?HALVES ->
    case Op of
        bit_and -> ?GO(N, ?INT32(X band Y), St);
        bit_or -> ?GO(N, ?INT32(X bor Y), St);
        bit_xor -> ?GO(N, ?INT32(X bxor Y), St);
        shift_right -> ?GO(N, ?INT32(X) bsr (Y band 31), St)
    end;
bits(X, St, ?ENV, N, Pc, Y, Op) -> general(X, St, ?ENV, N, Pc, Y, Op).

general(X, St, ?ENV, N, _, Y, mul) when ?INTS, ?SAFE(X * Y), X * Y =/= 0 -> ?GO(N, X * Y, St);
general(X, St, ?ENV, N, Pc, Y, Op) ->
    V = case Op of
        instance_of ->
            arc_interp_ffi:instance_of(A, X, Y, {well_known_symbol, sym_has_instance});
        in -> arc_interp_prop_ffi:has_property(A, X, Y);
        _ -> arc_rt_ops_ffi:binop(Op, X, Y)
    end,
    case V of
        miss when X =:= js_tdz; Y =:= js_tdz -> ?CORE:tdz(X, St, ?ENV, Pc);
        miss -> ?CORE:via({bin_op, Op}, [Y, X | St], ?ENV, N, Pc, c);
        _ -> ?GO(N, V, St)
    end.

%% the ops worth a closure that also stores the result
fuses_put(Op) ->
    Op =:= add orelse Op =:= sub orelse Op =:= mul orelse Op =:= bit_and
        orelse Op =:= bit_or orelse Op =:= bit_xor.

%% X op Y into local D, leaving nothing in T
-define(BINOP_PUT(Op, Guard, Expr, General),
binop_put(Op, stack, _, D, N, Pc) when ?IS_OP(N) ->
    ?FN ->
        [X | St1] = St,
        Y = T,
        if Guard -> V = Expr, ?WR(D, V, N, T, St1);
           true -> General(X, St1, ?ENV, N, Pc, Y, Op, D)
        end
    end;
binop_put(Op, const, Y, D, N, Pc) when ?IS_OP(N) ->
    ?FN ->
        X = T,
        if Guard -> V = Expr, ?WR(D, V, N, T, St);
           true -> General(X, St, ?ENV, N, Pc, Y, Op, D)
        end
    end;
binop_put(Op, local, J, D, N, Pc) when ?IS_OP(N) ->
    ?FN ->
        X = T,
        Y = ?RD(J),
        if Guard -> V = Expr, ?WR(D, V, N, T, St);
           true -> General(X, St, ?ENV, N, Pc, Y, Op, D)
        end
    end;
binop_put(Op, local_local, {I, J}, D, N, Pc) when ?IS_OP(N) ->
    ?FN ->
        X = ?RD(I),
        Y = ?RD(J),
        if Guard -> V = Expr, ?WR(D, V, N, T, St);
           true -> General(X, St, ?ENV, N, Pc, Y, Op, D)
        end
    end).

binop_put(add, stack, _, D, N, Pc) when ?IS_OP(N), D < 0 ->
    ?BY_REGISTER(D, Here, ?OP(begin
        [X | St1] = St,
        Y = T,
        if ?HALVES -> V = X + Y, ?WR(Here, V, N, T, St1);
           true -> add_put(X, St1, ?ENV, N, Pc, Y, add, Here)
        end
    end));
?BINOP_PUT(add, ?HALVES, X + Y, add_put);
?BINOP_PUT(sub, ?HALVES, X - Y, sub_put);
?BINOP_PUT(mul, ?FACTORS, X * Y, mul_put);
?BINOP_PUT(bit_and, ?INT32S, X band Y, bits_put);
?BINOP_PUT(bit_or, ?INT32S, X bor Y, bits_put);
?BINOP_PUT(bit_xor, ?INT32S, X bxor Y, bits_put).

-define(FLOATS_PUT(Name, Expr),
Name(X, St, ?ENV, N, Pc, Y, Op, D) when is_float(X) ->
    if is_float(Y) ->
           try Expr of
               V -> ?WR(D, V, N, X, St)
           catch
               error:badarith -> general_put(X, St, ?ENV, N, Pc, Y, Op, D)
           end;
       is_integer(Y) -> Name(X, St, ?ENV, N, Pc, float(Y), Op, D);
       true -> general_put(X, St, ?ENV, N, Pc, Y, Op, D)
    end;
Name(X, St, ?ENV, N, Pc, Y, Op, D) when is_integer(X), is_float(Y) ->
    Name(float(X), St, ?ENV, N, Pc, Y, Op, D)).

?FLOATS_PUT(add_put, X + Y);
add_put(X, St, ?ENV, N, _, Y, _, D) when is_binary(X), is_binary(Y) ->
    V = ?JOINED(X, Y),
    ?WR(D, V, N, X, St);
add_put(X, St, ?ENV, N, Pc, Y, Op, D) -> general_put(X, St, ?ENV, N, Pc, Y, Op, D).

?FLOATS_PUT(sub_put, X - Y);
sub_put(X, St, ?ENV, N, Pc, Y, Op, D) -> general_put(X, St, ?ENV, N, Pc, Y, Op, D).

?FLOATS_PUT(mul_put, X * Y);
mul_put(X, St, ?ENV, N, Pc, Y, Op, D) -> general_put(X, St, ?ENV, N, Pc, Y, Op, D).

bits_put(X, St, ?ENV, N, _, Y, Op, D) when ?HALVES ->
    V = case Op of
        bit_and -> ?INT32(X band Y);
        bit_or -> ?INT32(X bor Y);
        bit_xor -> ?INT32(X bxor Y)
    end,
    ?WR(D, V, N, X, St);
bits_put(X, St, ?ENV, N, Pc, Y, Op, D) -> general_put(X, St, ?ENV, N, Pc, Y, Op, D).

general_put(X, St, ?ENV, N, _, Y, mul, D) when ?INTS, ?SAFE(X * Y), X * Y =/= 0 ->
    V = X * Y,
    ?WR(D, V, N, X, St);
general_put(X, St, ?ENV, N, Pc, Y, Op, D) ->
    case arc_rt_ops_ffi:binop(Op, X, Y) of
        miss when X =:= js_tdz; Y =:= js_tdz -> ?CORE:tdz(X, St, ?ENV, Pc);
        miss -> ?CORE:via({bin_op, Op}, [Y, X | St], ?ENV, ?CORE:store_top(D, N), Pc, c);
        V -> ?WR(D, V, N, X, St)
    end.

%% goes to Yes when X op Y holds. The forms that read the stack leave nothing in
%% T, and the ones that only read locals leave T be
-define(COMPARE(Op, Test),
compare(Op, stack, _, _, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?FN ->
        [X | St1] = St,
        Y = T,
        ?DECIDE(Op, Test, St1)
    end;
compare(Op, const, _, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), ?WITHIN(Y, 52) ->
    ?FN ->
        X = T,
        ?DECIDE(Op, Test, St)
    end;
compare(Op, const, _, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?FN ->
        X = T,
        ?DECIDE(Op, Test, St)
    end;
compare(Op, local_local, I, J, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?BY_REGISTER(I, Here, ?OP(begin
        X = ?RD(Here),
        Y = ?RD(J),
        ?DECIDE(Op, Test, St)
    end));
compare(Op, local_const, I, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), ?WITHIN(Y, 52) ->
    ?BY_REGISTER(I, Here, ?OP(begin
        X = ?RD(Here),
        ?DECIDE(Op, Test, St)
    end));
compare(Op, local_const, I, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?FN ->
        X = ?RD(I),
        ?DECIDE(Op, Test, St)
    end).

-define(DECIDE(Op, Test, Stack),
        if ?NUMBERS ->
               if Test -> ?GO(Yes, T, Stack);
                  true -> ?GO(No, T, Stack)
               end;
           true -> compare_general(T, Stack, ?ENV, Yes, No, P, X, Y, Op)
        end).

%% Y is a constant that only equals itself
-define(IDENTICAL(Stack),
        if X =:= Y -> ?GO(Yes, T, Stack);
           X =:= js_tdz -> ?CORE:tdz(T, St, ?ENV, P);
           true -> ?GO(No, T, Stack)
        end).
-define(NULLISH(Stack),
        case X of
            undefined -> ?GO(Yes, T, Stack);
            null -> ?GO(Yes, T, Stack);
            js_tdz -> ?CORE:tdz(T, St, ?ENV, P);
            _ -> ?GO(No, T, Stack)
        end).
-define(SAME_INT(Op, Stack),
        if X =:= Y -> ?GO(Yes, T, Stack);
           is_integer(X) -> ?GO(No, T, Stack);
           true -> compare_general(T, Stack, ?ENV, Yes, No, P, X, Y, Op)
        end).
-define(SAME_TEXT_AS(Stack),
        if X =:= Y -> ?GO(Yes, T, Stack);
           is_binary(X) -> ?GO(No, T, Stack);
           true -> compare_general(T, Stack, ?ENV, Yes, No, P, X, Y, loose_eq)
        end).
-define(SAME(Op, Stack),
        case X of
            _ when ?INTS ->
                if X =:= Y -> ?GO(Yes, T, Stack);
                   true -> ?GO(No, T, Stack)
                end;
            {?HANDLE_TAG, Id} ->
                case Y of
                    {?HANDLE_TAG, Id} -> ?GO(Yes, T, Stack);
                    {?HANDLE_TAG, _} -> ?GO(No, T, Stack);
                    _ -> compare_general(T, Stack, ?ENV, Yes, No, P, X, Y, Op)
                end;
            _ -> compare_general(T, Stack, ?ENV, Yes, No, P, X, Y, Op)
        end).
-define(IS_WORD(Y), (?IS_NULLISH(Y) orelse is_boolean(Y))).

compare(strict_not_eq, Form, I, J, Yes, No, P) -> compare(strict_eq, Form, I, J, No, Yes, P);
compare(loose_not_eq, Form, I, J, Yes, No, P) -> compare(loose_eq, Form, I, J, No, Yes, P);
?COMPARE(less, X < Y);
?COMPARE(less_eq, X =< Y);
?COMPARE(greater, X > Y);
?COMPARE(greater_eq, X >= Y);
compare(strict_eq, const, _, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), ?IS_WORD(Y) ->
    ?FN -> X = T, ?IDENTICAL(St) end;
compare(strict_eq, local_const, I, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), ?IS_WORD(Y) ->
    ?BY_REGISTER(I, Here, ?OP(begin X = ?RD(Here), ?IDENTICAL(St) end));
compare(strict_eq, const, _, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), ?IS_STR(Y) ->
    ?FN -> X = T, ?IDENTICAL(St) end;
compare(strict_eq, local_const, I, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), ?IS_STR(Y) ->
    ?FN -> X = ?RD(I), ?IDENTICAL(St) end;
compare(loose_eq, const, _, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), is_binary(Y) ->
    ?FN -> X = T, ?SAME_TEXT_AS(St) end;
compare(loose_eq, local_const, I, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), is_binary(Y) ->
    ?FN -> X = ?RD(I), ?SAME_TEXT_AS(St) end;
compare(loose_eq, const, _, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), ?IS_NULLISH(Y) ->
    ?FN -> X = T, ?NULLISH(St) end;
compare(loose_eq, local_const, I, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), ?IS_NULLISH(Y) ->
    ?BY_REGISTER(I, Here, ?OP(begin X = ?RD(Here), ?NULLISH(St) end));
compare(Op, const, _, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), ?WITHIN(Y, 52) ->
    ?FN -> X = T, ?SAME_INT(Op, St) end;
compare(Op, local_const, I, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), ?WITHIN(Y, 52) ->
    ?BY_REGISTER(I, Here, ?OP(begin X = ?RD(Here), ?SAME_INT(Op, St) end));
compare(Op, const, _, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?FN -> compare_general(T, St, ?ENV, Yes, No, P, T, Y, Op) end;
compare(Op, local_const, I, Y, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?FN -> compare_general(T, St, ?ENV, Yes, No, P, ?RD(I), Y, Op) end;
compare(Op, stack, _, _, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?FN ->
        [X | St1] = St,
        Y = T,
        ?SAME(Op, St1)
    end;
compare(Op, local_local, I, J, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?FN ->
        X = ?RD(I),
        Y = ?RD(J),
        ?SAME(Op, St)
    end.

compare_general(T, St, ?ENV, Yes, No, P, X, Y, Op) when Op =:= strict_eq; Op =:= loose_eq ->
    ?ALIKE(?GO(Yes, T, St), ?GO(No, T, St), decide(T, St, ?ENV, Yes, No, P, X, Y, Op));
compare_general(T, St, ?ENV, Yes, No, P, X, Y, Op) -> decide(T, St, ?ENV, Yes, No, P, X, Y, Op).

decide(T, St, ?ENV, Yes, No, P, X, Y, Op) ->
    case arc_rt_ops_ffi:binop(Op, X, Y) of
        true -> ?GO(Yes, T, St);
        false -> ?GO(No, T, St);
        miss when X =:= js_tdz; Y =:= js_tdz -> ?CORE:tdz(T, St, ?ENV, P);
        miss when P < 0 ->
            Decide = branch(?CORE:fill(Yes), ?CORE:fill(No)),
            ?CORE:via({bin_op, Op}, [Y, X, T | St], ?ENV, Decide, -P, c);
        miss -> ?CORE:via({bin_op, Op}, [Y, X | St], ?ENV, branch(Yes, No), P, c)
    end.

%% typeof T == Name, going to Yes or No with nothing in T. Pc is the typeof's
-define(OF_TYPE(Name, Test),
of_type(Name, Yes, No, _) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?FN ->
        if Test -> ?GO(Yes, T, St);
           true -> ?GO(No, T, St)
        end
    end).

?OF_TYPE(<<"string">>, ?IS_STR(T));
?OF_TYPE(<<"number">>, ?IS_JS_NUMBER(T));
?OF_TYPE(<<"undefined">>, T =:= undefined orelse T =:= js_tdz);
?OF_TYPE(<<"boolean">>, is_boolean(T));
?OF_TYPE(<<"symbol">>, element(1, T) =:= js_sym);
of_type(<<"function">> = Name, Yes, No, Pc) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?FN ->
        case T of
            {?HANDLE_TAG, H} ->
                case arena_get(H, ?CELLS(A)) of
                    {?SOBJECT_TAG, #bytecode_fn{}, _, _, _, _, _} -> ?GO(Yes, T, St);
                    {?SSHAPEDOBJECT_TAG, _, _, _, _} -> ?GO(No, T, St);
                    _ -> named_type(T, St, ?ENV, Yes, No, Pc, Name)
                end;
            _ -> ?GO(No, T, St)
        end
    end;
of_type(<<"object">> = Name, Yes, No, Pc) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?FN ->
        case T of
            {?HANDLE_TAG, H} ->
                case arena_get(H, ?CELLS(A)) of
                    {?SSHAPEDOBJECT_TAG, _, _, _, _} -> ?GO(Yes, T, St);
                    {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, _, _, _, _, _} -> ?GO(Yes, T, St);
                    _ -> named_type(T, St, ?ENV, Yes, No, Pc, Name)
                end;
            null -> ?GO(Yes, T, St);
            _ -> ?GO(No, T, St)
        end
    end;
of_type(Name, Yes, No, Pc) -> ?FN -> named_type(T, St, ?ENV, Yes, No, Pc, Name) end.

named_type(T, St, ?ENV, Yes, No, Pc, Name) ->
    case arc_interp_ffi:type_of(A, T) of
        Name -> ?GO(Yes, T, St);
        miss ->
            Compare = compare(strict_eq, const, none, Name, Yes, No, Pc + 1),
            ?CORE:via(type_of, [T | St], ?ENV, Compare, Pc, c);
        _ -> ?GO(No, T, St)
    end.

%% adds By to local I. post also pushes what it held
step(plain, I, By, _, N, P) when ?IS_OP(N), ?WITHIN(By, 1) ->
    ?BY_REGISTER(I, Here, ?OP(begin
        X = ?RD(Here),
        if ?WITHIN(X, 52) -> V = X + By, ?WR(Here, V, N, T, St);
           true -> step_general(T, St, ?ENV, N, P, Here, By, X)
        end
    end));
step(post, I, By, S, N, Pc) when ?IS_OP(N), ?WITHIN(By, 1) ->
    ?BY_REGISTER(I, Here, ?PUSHING(S, begin
        X = ?RD(Here),
        if ?WITHIN(X, 52) -> V = X + By, ?WR(Here, V, N, X, St);
           true -> post_step_general(X, St, ?ENV, N, Pc, Here, By)
        end
    end)).

step_general(T, St, ?ENV, N, P, I, By, X) ->
    case arc_rt_ops_ffi:step(X, By) of
        miss ->
            Op = case By of 1 -> inc_local; -1 -> dec_local end,
            ?CORE:via_local(Op, I, T, St, ?ENV, N, P);
        V -> ?WR(I, V, N, T, St)
    end.

post_step_general(X, St, ?ENV, N, Pc, I, By) ->
    case arc_rt_ops_ffi:step(X, By) of
        miss -> ?CORE:via_flushed(St, ?ENV, N, Pc, c);
        V -> ?WR(I, V, N, X, St)
    end.

%% step then compare, the way a counting loop ends
-define(STEP_COMPARE(Op, Test),
step_compare(Op, const, I, By, Y, Yes, No, P)
  when ?IS_OP(Yes), ?IS_OP(No), ?WITHIN(By, 1), ?WITHIN(Y, 52) ->
    ?BY_REGISTER(I, Here, ?OP(begin
        Old = ?RD(Here),
        ?STEP_DECIDE(Op, const, Y, Test)
    end));
step_compare(Op, local, I, By, J, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No), ?WITHIN(By, 1) ->
    ?BY_REGISTER(I, Here, ?OP(begin
        Old = ?RD(Here),
        Y = ?RD(J),
        ?STEP_DECIDE(Op, local, J, Test)
    end))).

-define(STEP_DECIDE(Op, Form, Right, Test),
        if ?WITHIN(Old, 52), is_integer(Y) ->
               X = Old + By,
               if Test -> ?WR(Here, X, Yes, T, St);
                  true -> ?WR(Here, X, No, T, St)
               end;
           true ->
               step_compare_general(T, St, ?ENV, Yes, No, P, Here, By, Old, Op, Form, Right)
        end).

?STEP_COMPARE(less, X < Y);
?STEP_COMPARE(less_eq, X =< Y);
?STEP_COMPARE(greater, X > Y);
?STEP_COMPARE(greater_eq, X >= Y);
step_compare(Op, const, I, By, Y, Yes, No, P) ->
    step(plain, I, By, none, compare(Op, local_const, I, Y, Yes, No, P), P);
step_compare(Op, local, I, By, J, Yes, No, P) ->
    step(plain, I, By, none, compare(Op, local_local, I, J, Yes, No, P), P).

step_compare_general(T, St, ?ENV, Yes, No, P, I, By, Old, Op, Form, Right) ->
    Compare = case Form of
        const -> compare(Op, local_const, I, Right, Yes, No, P);
        local -> compare(Op, local_local, I, Right, Yes, No, P)
    end,
    step_general(T, St, ?ENV, Compare, P, I, By, Old).

%% §7.1.2 toboolean, the usual values first
-define(TRUTHY(X, Other),
        case X of
            true -> ?GO(Yes, T, St);
            false -> ?GO(No, T, St);
            {?HANDLE_TAG, _} -> ?GO(Yes, T, St);
            undefined -> ?GO(No, T, St);
            null -> ?GO(No, T, St);
            0 -> ?GO(No, T, St);
            _ when is_integer(X) -> ?GO(Yes, T, St);
            _ -> Other
        end).

%% T stays, for a target that wants it
branch(Yes, No) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?FN -> ?TRUTHY(T, branch_general(T, St, ?ENV, Yes, No, T)) end.

branch_local(I, Yes, No, P) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?BY_REGISTER(I, Here, ?OP(begin
        X = ?RD(Here),
        ?TRUTHY(X, case X of
                       js_tdz -> ?CORE:tdz(T, St, ?ENV, P);
                       _ -> branch_general(T, St, ?ENV, Yes, No, X)
                   end)
    end)).

branch_general(T, St, ?ENV, Yes, No, X) ->
    case arc_rt_val_ffi:to_boolean(X) of
        true -> ?GO(Yes, T, St);
        false -> ?GO(No, T, St)
    end.

unary(logical_not, N, _) when ?IS_OP(N) ->
    ?FN ->
        case T of
            true -> ?GO(N, false, St);
            false -> ?GO(N, true, St);
            _ -> ?GO(N, arc_rt_val_ffi:logical_not(T), St)
        end
    end;
unary(void, N, _) when ?IS_OP(N) -> ?FN -> ?GO(N, undefined, St) end;
unary(neg, N, Pc) when ?IS_OP(N) ->
    ?FN ->
        if is_integer(T), T =/= 0 -> ?GO(N, -T, St);
           true -> unary_general(T, St, ?ENV, N, Pc, neg)
        end
    end;
unary(pos, N, Pc) when ?IS_OP(N) ->
    ?FN ->
        if is_number(T) -> ?GO(N, T, St);
           is_binary(T) ->
               ?GO(N, arc_rt_val_ffi:mk_number(arc_rt_val_ffi:string_to_number(T)), St);
           true -> unary_general(T, St, ?ENV, N, Pc, pos)
        end
    end;
unary(bit_not, N, Pc) when ?IS_OP(N) ->
    ?FN ->
        if is_integer(T) -> ?GO(N, bnot ?INT32(T), St);
           true -> unary_general(T, St, ?ENV, N, Pc, bit_not)
        end
    end.

unary_general(T, St, ?ENV, N, Pc, Kind) ->
    V = case Kind of
        neg -> arc_rt_ops_ffi:neg(T);
        pos -> arc_rt_ops_ffi:plus(T);
        bit_not -> arc_rt_ops_ffi:bitnot(T)
    end,
    case V of
        miss -> ?CORE:via({unary_op, Kind}, [T | St], ?ENV, N, Pc, c);
        _ -> ?GO(N, V, St)
    end.
