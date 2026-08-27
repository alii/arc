%% fallbacks are for atomvm: no [short], precision capped at 57
-module(arc_rt_float_ffi).
-export([shortest/1, scientific/2, decimals/2]).

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
            [Mantissa, Exp] = string:split(S, "e"),
            Mantissa ++ ".0e" ++ Exp
    end.

scientific(X, N) when is_float(X), is_integer(N) ->
    try float_to_list(X, [{scientific, N}])
    catch error:badarg when N > 57 -> float_to_list(X, [{scientific, 57}])
    end.

decimals(X, D) when is_float(X), is_integer(D) ->
    try float_to_list(X, [{decimals, D}])
    catch error:badarg when D > 57 -> float_to_list(X, [{decimals, 57}])
    end.
