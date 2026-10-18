%% what closures capture: a tuple of values, the first Nested of them in a
%% tuple of their own when the function's code wants them so
-module(arc_rt_env_ffi).
-export([nest/2, renest/2, capture/4, capture_with/4]).

nest(Values, 0) -> list_to_tuple(Values);
nest(Values, Nested) ->
    case drop(Nested, Values) of
        [] -> {list_to_tuple(Values)};
        Rest -> list_to_tuple([list_to_tuple(first(Nested, Values)) | Rest])
    end.

drop(0, Values) -> Values;
drop(N, [_ | Values]) -> drop(N - 1, Values).

first(0, _) -> [];
first(N, [V | Values]) -> [V | first(N - 1, Values)].

%% the same for values already in a flat tuple
renest(Env, 0) -> Env;
renest(Env, Nested) -> nest(tuple_to_list(Env), Nested).

%% the parent's values at Slots. Slots count as if the parent's frame were
%% flat; From is how many of its own captures it keeps nested in slot 0
capture([], _, _, _) -> {};
capture([I], 0, L, 0) -> {element(I + 1, L)};
capture([I, J], 0, L, 0) -> {element(I + 1, L), element(J + 1, L)};
capture(Slots, Nested, L, 0) -> capture(Slots, Nested, none, L, 0, 1);
capture(Slots, Nested, L, From) -> capture(Slots, Nested, element(1, L), L, From, 2).

%% Off is where the slot after the parent's captures sits in its frame
-define(AT(I), (case I < From of
                    true -> element(I + 1, E);
                    false -> element(I - From + Off, L)
                end)).

capture([I, J], 2, E, L, From, Off) -> {{?AT(I), ?AT(J)}};
capture([I, J, T], 2, E, L, From, Off) -> {{?AT(I), ?AT(J)}, ?AT(T)};
capture([I, J, K], 3, E, L, From, Off) -> {{?AT(I), ?AT(J), ?AT(K)}};
capture([I, J, K, T], 3, E, L, From, Off) -> {{?AT(I), ?AT(J), ?AT(K)}, ?AT(T)};
capture([I, J, K, M], 4, E, L, From, Off) -> {{?AT(I), ?AT(J), ?AT(K), ?AT(M)}};
capture(Slots, Nested, E, L, From, Off) ->
    nest(values(Slots, E, L, From, Off), Nested).

%% flat, with Last after them
capture_with(Slots, L, 0, Last) ->
    list_to_tuple(values(Slots, none, L, 0, 1) ++ [Last]);
capture_with(Slots, L, From, Last) ->
    list_to_tuple(values(Slots, element(1, L), L, From, 2) ++ [Last]).

values(Slots, E, L, From, Off) -> [?AT(I) || I <- Slots].
