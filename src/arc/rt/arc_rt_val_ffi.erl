-module(arc_rt_val_ffi).

-export([
    classify/1,
    mk_undefined/0, mk_hole/0, mk_null/0, mk_bool/1, mk_number/1, mk_int/1,
    mk_string/1, mk_bigint/1, mk_symbol/1, mk_object/1, mk_tdz/0,
    to_boolean_i32/1, to_boolean/1,
    strict_eq/2, same_value_zero/2,
    t_to_property_key_fast/1,
    js_number_to_string/1,
    t_to_string/2, t_to_number/2, t_to_integer_or_infinity/2, t_to_length/2,
    string_to_number/1,
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

%% hot heads of the val.gleam coercions, everything else goes back there
t_to_string(St, V) when is_binary(V) -> {V, St};
t_to_string(St, V) when is_integer(V) -> {integer_to_binary(V), St};
t_to_string(St, V) when is_float(V) -> {js_number_to_string(V), St};
t_to_string(St, V) -> 'arc@rt@val':t_to_string_slow(St, V).

t_to_number(St, V) when is_integer(V) -> {{j_int, V}, St};
t_to_number(St, V) when is_float(V) -> {{j_float, V}, St};
t_to_number(St, V) -> 'arc@rt@val':t_to_number_slow(St, V).

t_to_integer_or_infinity(St, V) when is_integer(V) -> {V, St};
t_to_integer_or_infinity(St, V) when is_float(V) -> {trunc(V), St};
t_to_integer_or_infinity(St, undefined) -> {0, St};
t_to_integer_or_infinity(St, V) -> 'arc@rt@val':t_to_integer_or_infinity_slow(St, V).

t_to_length(St, V) when is_integer(V), V >= 0 -> {V, St};
t_to_length(St, V) when is_integer(V) -> {0, St};
t_to_length(St, V) -> 'arc@rt@val':t_to_length_slow(St, V).

%% §6.1.6.1.20 number tostring
js_number_to_string(N) when is_float(N) ->
    case N == 0.0 of
        true -> <<"0">>;
        false when N < 0.0 -> <<"-", (js_positive_to_string(-N))/binary>>;
        false -> js_positive_to_string(N)
    end.

js_positive_to_string(X) ->
    try short_fixed(float_to_binary(X, [short])) of
        general -> js_positive_digits(X);
        Bin -> Bin
    catch
        error:badarg -> js_positive_digits(X)
    end.

%% erlang's fixed-notation window sits inside js's, so only ".0" differs
short_fixed(<<$0, _/binary>>) -> general;
short_fixed(Bin) -> short_fixed(Bin, Bin, 0, 0).

short_fixed(<<$e, _/binary>>, _, _, _) -> general;
short_fixed(<<$., R/binary>>, Bin, I, _) -> short_fixed(R, Bin, I + 1, I);
short_fixed(<<_, R/binary>>, Bin, I, Dot) -> short_fixed(R, Bin, I + 1, Dot);
short_fixed(<<>>, _, _, Dot) when Dot > 21 -> general;
short_fixed(<<>>, Bin, Size, Dot) when Size - Dot =:= 2 ->
    case binary:last(Bin) of
        $0 -> binary:part(Bin, 0, Dot);
        _ -> Bin
    end;
short_fixed(<<>>, Bin, _, _) -> Bin.

js_positive_digits(X) ->
    {Digits, E} = arc_rt_float_ffi:shortest_digits(X),
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

is_neg_zero(X) when is_float(X) ->
    case <<X/float>> of
        <<1:1, 0:63>> -> true;
        _ -> false
    end.

float_same_term(A, B) -> A =:= B.

%% §7.1.4.1.1 stringtonumber, one pass over the trimmed bytes
string_to_number(S) ->
    stn(arc_string_ffi:trim_js_ws(S)).

stn(<<>>) -> {j_float, 0.0};
stn(<<$0, X, D/binary>>) when X =:= $x; X =:= $X -> stn_radix(D, 16);
stn(<<$0, O, D/binary>>) when O =:= $o; O =:= $O -> stn_radix(D, 8);
stn(<<$0, B, D/binary>>) when B =:= $b; B =:= $B -> stn_radix(D, 2);
stn(<<$-, R/binary>>) -> stn_negate(stn_unsigned(R));
stn(<<$+, R/binary>>) -> stn_unsigned(R);
stn(B) -> stn_unsigned(B).

stn_radix(<<C, _/binary>>, _) when C =:= $-; C =:= $+ -> j_nan;
stn_radix(<<>>, _) -> j_nan;
stn_radix(D, Base) ->
    try binary_to_integer(D, Base) of
        N -> 'arc@rt@val':num_from_int(N)
    catch
        error:badarg -> j_nan
    end.

stn_negate({j_float, F}) -> {j_float, -F};
stn_negate({j_int, 0}) -> {j_float, -0.0};
stn_negate({j_int, I}) -> {j_int, -I};
stn_negate(j_pos_inf) -> j_neg_inf;
stn_negate(Other) -> Other.

stn_unsigned(<<"Infinity">>) -> j_pos_inf;
stn_unsigned(B) ->
    I = stn_digits(B, 0),
    case B of
        <<_:I/binary>> when I > 0 ->
            'arc@rt@val':int_number(binary_to_integer(B));
        <<Int:I/binary, $., R/binary>> ->
            F = stn_digits(R, 0),
            case I + F > 0 of
                true ->
                    <<Frac:F/binary, E/binary>> = R,
                    stn_decimal(Int, Frac, E);
                false -> j_nan
            end;
        <<Int:I/binary, E/binary>> when I > 0 -> stn_decimal(Int, <<>>, E);
        _ -> j_nan
    end.

stn_digits(<<D, R/binary>>, N) when D >= $0, D =< $9 -> stn_digits(R, N + 1);
stn_digits(_, N) -> N.

%% e is empty or an exponent part, else nan
stn_decimal(Int, Frac, E) ->
    Exp = case E of
        <<>> -> <<"0">>;
        <<C, S, D/binary>>
          when (C =:= $e orelse C =:= $E), (S =:= $+ orelse S =:= $-) ->
            stn_exp_digits(D, <<S>>);
        <<C, D/binary>> when C =:= $e; C =:= $E -> stn_exp_digits(D, <<>>);
        _ -> nan
    end,
    case Exp of
        nan -> j_nan;
        _ ->
            Norm = <<(stn_or_zero(Int))/binary, $., (stn_or_zero(Frac))/binary,
                     $e, Exp/binary>>,
            try binary_to_float(Norm) of
                Fl -> {j_float, Fl}
            catch
                %% syntax is already checked, so this is overflow
                error:badarg -> j_pos_inf
            end
    end.

stn_exp_digits(D, Sign) ->
    case stn_digits(D, 0) =:= byte_size(D) andalso D =/= <<>> of
        true -> <<Sign/binary, D/binary>>;
        false -> nan
    end.

stn_or_zero(<<>>) -> <<"0">>;
stn_or_zero(B) -> B.


