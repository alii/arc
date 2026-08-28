%% fallbacks are for atomvm: no [short], precision capped at 57
-module(arc_rt_float_ffi).
-export([shortest/1, scientific/2]).
-export([shortest_digits/1, split_exponent/1, split_dot/1]).

shortest(X) when is_float(X) ->
    try float_to_list(X, [short])
    catch error:badarg -> shortest_search(X, 0)
    end.

shortest_search(X, P) when P =< 16 ->
    S = with_fraction(float_to_list(X, [{scientific, P}])),
    case round_trips(S, X) of
        true -> S;
        false -> shortest_search(X, P + 1)
    end;
shortest_search(X, _P) ->
    with_fraction(float_to_list(X, [{scientific, 16}])).

round_trips(S, X) ->
    try list_to_float(S) =:= X
    catch error:badarg -> false
    end.

with_fraction(S) ->
    case lists:member($., S) of
        true -> S;
        false ->
            {Mantissa, Exp} = split_exponent(S),
            Mantissa ++ ".0e" ++ integer_to_list(Exp)
    end.

%% digits sans leading/trailing zeros, exponent of the first
shortest_digits(X) ->
    {Mantissa, E0} = split_exponent(shortest(X)),
    {IntPart, FracPart} = split_dot(Mantissa),
    {Lead, Rest} = drop_zeros(IntPart ++ FracPart, 0),
    Digits = lists:reverse(element(2, drop_zeros(lists:reverse(Rest), 0))),
    {Digits, length(IntPart) - 1 - Lead + E0}.

drop_zeros([$0 | T], N) -> drop_zeros(T, N + 1);
drop_zeros(L, N) -> {N, L}.

split_exponent(S) -> split_exponent(S, []).
split_exponent([$e | Exp], Acc) -> {lists:reverse(Acc), list_to_integer(Exp)};
split_exponent([C | T], Acc) -> split_exponent(T, [C | Acc]);
split_exponent([], Acc) -> {lists:reverse(Acc), 0}.

split_dot(S) -> split_dot(S, []).
split_dot([$. | F], Acc) -> {lists:reverse(Acc), F};
split_dot([C | T], Acc) -> split_dot(T, [C | Acc]);
split_dot([], Acc) -> {lists:reverse(Acc), []}.

scientific(X, N) when is_float(X), is_integer(N) ->
    try float_to_list(X, [{scientific, N}])
    catch error:badarg when N > 57 -> float_to_list(X, [{scientific, 57}])
    end.
