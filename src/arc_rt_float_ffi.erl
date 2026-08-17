%% Float → decimal text primitives for Number formatting, in one place because
%% arc_rt_number_ffi and arc_rt_val_ffi both need them and because the plain
%% BIF calls do not survive AtomVM (the browser playground): its
%% erlang:float_to_list/2 knows neither `short` (OTP 25+ shortest round-trip)
%% nor precisions above 57. Each function is the OTP call first and, only if
%% that raises badarg, a fallback that computes the same thing another way —
%% so on OTP nothing changes, and on AtomVM numbers still print.
-module(arc_rt_float_ffi).
-export([shortest/1, scientific/2, decimals/2]).

%% Shortest digit string that round-trips X, as float_to_list(X, [short])
%% renders it: mantissa with a "." and an optional "e<exp>". Callers only ever
%% split it on "." and "e", so the fallback's "d.ddde+XX" form is fine.
shortest(X) when is_float(X) ->
    try float_to_list(X, [short])
    catch error:badarg -> shortest_search(X, 0)
    end.

%% Try P = 0, 1, … significant decimals until the text parses back to X. 17
%% significant digits always round-trip a double, so this ends by P = 16.
%% {scientific, P} rounds correctly, so the first hit is the closest P-digit
%% decimal — the same choice Number::toString specifies.
shortest_search(X, P) when P =< 16 ->
    S = with_fraction(float_to_list(X, [{scientific, P}])),
    case round_trips(S, X) of
        true -> S;
        false -> shortest_search(X, P + 1)
    end;
shortest_search(X, _P) ->
    with_fraction(float_to_list(X, [{scientific, 16}])).

%% Rounding to few digits can leave the maximum double as "2.0e+308", which
%% is not a float at all — that is simply "does not round-trip".
round_trips(S, X) ->
    try list_to_float(S) =:= X
    catch error:badarg -> false
    end.

%% list_to_float insists on a fraction ("1e+00" is not a float literal).
with_fraction(S) ->
    case lists:member($., S) of
        true -> S;
        false ->
            [Mantissa, Exp] = string:split(S, "e"),
            Mantissa ++ ".0e" ++ Exp
    end.

%% float_to_list(X, [{scientific, N}]); AtomVM caps N at 57.
scientific(X, N) when is_float(X), is_integer(N) ->
    try float_to_list(X, [{scientific, N}])
    catch error:badarg when N > 57 -> float_to_list(X, [{scientific, 57}])
    end.

%% float_to_list(X, [{decimals, D}]); AtomVM caps D at 57.
decimals(X, D) when is_float(X), is_integer(D) ->
    try float_to_list(X, [{decimals, D}])
    catch error:badarg when D > 57 -> float_to_list(X, [{decimals, 57}])
    end.
