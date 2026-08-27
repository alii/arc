-module(arc_rt_val_ffi).

-export([
    classify/1,
    mk_undefined/0, mk_hole/0, mk_null/0, mk_bool/1, mk_number/1, mk_int/1,
    mk_string/1, mk_bigint/1, mk_symbol/1, mk_object/1, mk_tdz/0,
    to_boolean_i32/1, to_boolean/1,
    strict_eq/2, same_value_zero/2,
    t_to_property_key_fast/1,
    js_number_to_string/1,
    parse_float/1,
    is_neg_zero/1, float_same_term/2
]).

%% no catch-all: a bad wire term should crash
classify(undefined) -> k_undef;
classify(null) -> k_null;
classify(true) -> {k_bool, true};
classify(false) -> {k_bool, false};
classify(N) when is_integer(N) -> {k_num, {j_int, N}};
classify(N) when is_float(N) -> {k_num, {j_float, N}};
classify(js_nan) -> {k_num, j_nan};
classify(js_inf) -> {k_num, j_pos_inf};
classify(js_neg_inf) -> {k_num, j_neg_inf};
classify(B) when is_binary(B) -> {k_str, B};
classify({js_bigint, N}) -> {k_big, N};
classify({js_sym, S}) -> {k_sym, S};
classify({js_cell, N}) -> {k_handle, {js_cell, N}};
classify(js_tdz) -> k_tdz.

to_boolean_i32(undefined) -> 0;
to_boolean_i32(null) -> 0;
to_boolean_i32(false) -> 0;
to_boolean_i32(true) -> 1;
to_boolean_i32(0) -> 0;
to_boolean_i32(N) when is_integer(N) -> 1;
to_boolean_i32(F) when is_float(F) ->
    case F == 0.0 of true -> 0; false -> 1 end;
to_boolean_i32(js_nan) -> 0;
to_boolean_i32(js_inf) -> 1;
to_boolean_i32(js_neg_inf) -> 1;
to_boolean_i32(<<>>) -> 0;
to_boolean_i32(B) when is_binary(B) -> 1;
to_boolean_i32({js_bigint, 0}) -> 0;
to_boolean_i32({js_bigint, _}) -> 1;
to_boolean_i32({js_sym, _}) -> 1;
to_boolean_i32({js_cell, _}) -> 1;
to_boolean_i32(js_tdz) -> 0.

%% not dead: carder rewrites to_boolean_i32 to this
to_boolean(undefined) -> false;
to_boolean(null) -> false;
to_boolean(false) -> false;
to_boolean(true) -> true;
to_boolean(0) -> false;
to_boolean(N) when is_integer(N) -> true;
to_boolean(F) when is_float(F) -> F /= 0.0;
to_boolean(js_nan) -> false;
to_boolean(js_inf) -> true;
to_boolean(js_neg_inf) -> true;
to_boolean(<<>>) -> false;
to_boolean(B) when is_binary(B) -> true;
to_boolean({js_bigint, 0}) -> false;
to_boolean({js_bigint, _}) -> true;
to_boolean({js_sym, _}) -> true;
to_boolean({js_cell, _}) -> true;
to_boolean(js_tdz) -> false.

strict_eq(js_nan, _) -> false;
strict_eq(_, js_nan) -> false;
strict_eq(A, B) when is_number(A), is_number(B) -> A == B;
strict_eq(A, B) -> A =:= B.

same_value_zero(js_nan, js_nan) -> true;
same_value_zero(A, B) -> strict_eq(A, B).

-define(MAX_ARRAY_INDEX, 4294967294).
t_to_property_key_fast(N)
  when is_integer(N), N >= 0, N =< ?MAX_ARRAY_INDEX ->
    {string_key, {index, N}};
t_to_property_key_fast(B) when is_binary(B) ->
    {string_key, canonical_key_bin(B)};
t_to_property_key_fast({js_sym, S}) ->
    {symbol_key, S};
t_to_property_key_fast(_) -> miss.

canonical_key_bin(<<C, _/binary>> = B) when C >= $0, C =< $9 ->
    try binary_to_integer(B) of
        N when N >= 0, N =< ?MAX_ARRAY_INDEX ->
            case integer_to_binary(N) =:= B of
                true -> {index, N};
                false -> {named, B}
            end;
        _ -> {named, B}
    catch _:_ -> {named, B}
    end;
canonical_key_bin(B) -> {named, B}.

%% not a jsval, classify has no clause for it
mk_hole() -> js_hole.

mk_undefined() -> undefined.

mk_null() -> null.

mk_bool(B) -> B.

mk_number({j_int, N}) -> mk_int(N);
mk_number({j_float, F}) -> F;
mk_number(j_nan) -> js_nan;
mk_number(j_pos_inf) -> js_inf;
mk_number(j_neg_inf) -> js_neg_inf.

-define(MAX_SAFE_INT, 9007199254740991).
mk_int(N) when N > ?MAX_SAFE_INT; N < -?MAX_SAFE_INT ->
    mk_number('arc@rt@val':num_from_int(N));
mk_int(N) -> N.

mk_string(S) -> S.

mk_bigint(N) -> {js_bigint, N}.

mk_symbol(S) -> {js_sym, S}.

mk_object(H) -> H.

mk_tdz() -> js_tdz.

%% §6.1.6.1.20 number tostring
js_number_to_string(N) when is_float(N) ->
    case N == 0.0 of
        true -> <<"0">>;
        false when N < 0.0 -> <<"-", (js_positive_to_string(-N))/binary>>;
        false -> js_positive_to_string(N)
    end.

js_positive_to_string(X) ->
    {Digits, E} = shortest_digits(X),
    K = length(Digits),
    if
        E >= K - 1, E =< 20 ->
            list_to_binary(Digits ++ lists:duplicate(E + 1 - K, $0));
        E >= 0, E =< 20 ->
            {I, F} = lists:split(E + 1, Digits),
            list_to_binary(I ++ "." ++ F);
        E >= -6, E < 0 ->
            list_to_binary("0." ++ lists:duplicate(-E - 1, $0) ++ Digits);
        true ->
            format_exponential(Digits, E)
    end.

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

shortest_digits(X) ->
    {Mantissa, E0} = split_exponent(arc_rt_float_ffi:shortest(X)),
    [IntPart, FracPart] = string:split(Mantissa, "."),
    Combined = IntPart ++ FracPart,
    Lead = length(lists:takewhile(fun(C) -> C =:= $0 end, Combined)),
    Digits = string:trim(lists:nthtail(Lead, Combined), trailing, "0"),
    {Digits, length(IntPart) - 1 - Lead + E0}.

split_exponent(S) ->
    case string:split(S, "e") of
        [Mantissa, Exp] -> {Mantissa, list_to_integer(Exp)};
        [Mantissa] -> {Mantissa, 0}
    end.

is_neg_zero(X) when is_float(X) ->
    case <<X/float>> of
        <<1:1, 0:63>> -> true;
        _ -> false
    end.

float_same_term(A, B) -> A =:= B.

%% §7.1.4.1.1 string to number
parse_float(S) ->
    Norm = normalize(S),
    case try_binary_to_float(Norm) of
        {ok, F} -> {ok, F};
        error ->
            case is_float_syntax(Norm) of
                true -> {error, out_of_range};
                false -> {error, invalid}
            end
    end.

try_binary_to_float(S) ->
    try
        {ok, erlang:binary_to_float(S)}
    catch
        error:badarg -> error
    end.

normalize(S) ->
    {Mantissa, Exponent} = split_exponent_bin(S),
    {Sign, Digits} = take_sign(Mantissa),
    <<Sign/binary, (pad_mantissa(Digits))/binary, Exponent/binary>>.

split_exponent_bin(S) ->
    case binary:match(S, [<<"e">>, <<"E">>]) of
        {Pos, _Len} ->
            <<Mantissa:Pos/binary, Exponent/binary>> = S,
            {Mantissa, Exponent};
        nomatch ->
            {S, <<>>}
    end.

take_sign(<<C, Rest/binary>>) when C =:= $+; C =:= $- -> {<<C>>, Rest};
take_sign(S) -> {<<>>, S}.

pad_mantissa(<<>>) ->
    <<>>;
pad_mantissa(<<".", _/binary>> = M) ->
    pad_mantissa(<<"0", M/binary>>);
pad_mantissa(M) ->
    case binary:match(M, <<".">>) of
        nomatch -> <<M/binary, ".0">>;
        _ ->
            case binary:last(M) of
                $. -> <<M/binary, "0">>;
                _ -> M
            end
    end.

is_float_syntax(S0) ->
    S1 = skip_sign(S0),
    case take_digits(S1) of
        {true, <<".", S2/binary>>} ->
            case take_digits(S2) of
                {true, <<>>} -> true;
                {true, <<E, S3/binary>>} when E =:= $e; E =:= $E ->
                    case take_digits(skip_sign(S3)) of
                        {true, <<>>} -> true;
                        _ -> false
                    end;
                _ -> false
            end;
        _ -> false
    end.

skip_sign(<<C, Rest/binary>>) when C =:= $+; C =:= $- -> Rest;
skip_sign(S) -> S.

take_digits(S) -> take_digits(S, false).
take_digits(<<D, Rest/binary>>, _) when D >= $0, D =< $9 ->
    take_digits(Rest, true);
take_digits(S, Seen) ->
    {Seen, S}.
