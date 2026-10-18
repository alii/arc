%% frame locals and registers; sloppy_this may answer miss
-module(arc_interp_locals_ffi).
-export([frame_locals/9, sloppy_this/2, flush_registers/5, captured/2]).

-include("../rt/arc_rt_layout.hrl").

%% locals tuple: [env..., this, fnobj, home, nt, args to arity..., undefined...]
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 4) ->
    {This, FnObj, Home, NT};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 5) ->
    {This, FnObj, Home, NT, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 6) ->
    {This, FnObj, Home, NT, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 7) ->
    {This, FnObj, Home, NT, undefined, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 5) ->
    {This, FnObj, Home, NT, A};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 6) ->
    {This, FnObj, Home, NT, A, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 7) ->
    {This, FnObj, Home, NT, A, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 8) ->
    {This, FnObj, Home, NT, A, undefined, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 6) ->
    {This, FnObj, Home, NT, A, B};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 7) ->
    {This, FnObj, Home, NT, A, B, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 8) ->
    {This, FnObj, Home, NT, A, B, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 9) ->
    {This, FnObj, Home, NT, A, B, undefined, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B, C | _], 3, 7) ->
    {This, FnObj, Home, NT, A, B, C};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B, C | _], 3, 8) ->
    {This, FnObj, Home, NT, A, B, C, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B, C | _], 3, 9) ->
    {This, FnObj, Home, NT, A, B, C, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B, C | _], 3, 10) ->
    {This, FnObj, Home, NT, A, B, C, undefined, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B, C, D | _], 4, 8) ->
    {This, FnObj, Home, NT, A, B, C, D};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B, C, D | _], 4, 9) ->
    {This, FnObj, Home, NT, A, B, C, D, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B, C, D | _], 4, 10) ->
    {This, FnObj, Home, NT, A, B, C, D, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B, C, D | _], 4, 11) ->
    {This, FnObj, Home, NT, A, B, C, D, undefined, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, Args, Arity, LC) ->
    list_to_tuple([This, FnObj, Home, NT | slots(Args, Arity, LC - 4)]);
frame_locals({E1}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 5) ->
    {E1, This, FnObj, Home, NT};
frame_locals({E1}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 6) ->
    {E1, This, FnObj, Home, NT, undefined};
frame_locals({E1}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 7) ->
    {E1, This, FnObj, Home, NT, undefined, undefined};
frame_locals({E1}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 8) ->
    {E1, This, FnObj, Home, NT, undefined, undefined, undefined};
frame_locals({E1}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 6) ->
    {E1, This, FnObj, Home, NT, A};
frame_locals({E1}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 7) ->
    {E1, This, FnObj, Home, NT, A, undefined};
frame_locals({E1}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 8) ->
    {E1, This, FnObj, Home, NT, A, undefined, undefined};
frame_locals({E1}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 9) ->
    {E1, This, FnObj, Home, NT, A, undefined, undefined, undefined};
frame_locals({E1}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 7) ->
    {E1, This, FnObj, Home, NT, A, B};
frame_locals({E1}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 8) ->
    {E1, This, FnObj, Home, NT, A, B, undefined};
frame_locals({E1}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 9) ->
    {E1, This, FnObj, Home, NT, A, B, undefined, undefined};
frame_locals({E1}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 10) ->
    {E1, This, FnObj, Home, NT, A, B, undefined, undefined, undefined};
frame_locals({E1, E2}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 6) ->
    {E1, E2, This, FnObj, Home, NT};
frame_locals({E1, E2}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 7) ->
    {E1, E2, This, FnObj, Home, NT, undefined};
frame_locals({E1, E2}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 8) ->
    {E1, E2, This, FnObj, Home, NT, undefined, undefined};
frame_locals({E1, E2}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 9) ->
    {E1, E2, This, FnObj, Home, NT, undefined, undefined, undefined};
frame_locals({E1, E2}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 7) ->
    {E1, E2, This, FnObj, Home, NT, A};
frame_locals({E1, E2}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 8) ->
    {E1, E2, This, FnObj, Home, NT, A, undefined};
frame_locals({E1, E2}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 9) ->
    {E1, E2, This, FnObj, Home, NT, A, undefined, undefined};
frame_locals({E1, E2}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 10) ->
    {E1, E2, This, FnObj, Home, NT, A, undefined, undefined, undefined};
frame_locals({E1, E2}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 8) ->
    {E1, E2, This, FnObj, Home, NT, A, B};
frame_locals({E1, E2}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 9) ->
    {E1, E2, This, FnObj, Home, NT, A, B, undefined};
frame_locals({E1, E2}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 10) ->
    {E1, E2, This, FnObj, Home, NT, A, B, undefined, undefined};
frame_locals({E1, E2}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 11) ->
    {E1, E2, This, FnObj, Home, NT, A, B, undefined, undefined, undefined};
frame_locals({E1, E2, E3}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 7) ->
    {E1, E2, E3, This, FnObj, Home, NT};
frame_locals({E1, E2, E3}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 8) ->
    {E1, E2, E3, This, FnObj, Home, NT, undefined};
frame_locals({E1, E2, E3}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 9) ->
    {E1, E2, E3, This, FnObj, Home, NT, undefined, undefined};
frame_locals({E1, E2, E3}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 10) ->
    {E1, E2, E3, This, FnObj, Home, NT, undefined, undefined, undefined};
frame_locals({E1, E2, E3}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 8) ->
    {E1, E2, E3, This, FnObj, Home, NT, A};
frame_locals({E1, E2, E3}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 9) ->
    {E1, E2, E3, This, FnObj, Home, NT, A, undefined};
frame_locals({E1, E2, E3}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 10) ->
    {E1, E2, E3, This, FnObj, Home, NT, A, undefined, undefined};
frame_locals({E1, E2, E3}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 11) ->
    {E1, E2, E3, This, FnObj, Home, NT, A, undefined, undefined, undefined};
frame_locals({E1, E2, E3}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 9) ->
    {E1, E2, E3, This, FnObj, Home, NT, A, B};
frame_locals({E1, E2, E3}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 10) ->
    {E1, E2, E3, This, FnObj, Home, NT, A, B, undefined};
frame_locals({E1, E2, E3}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 11) ->
    {E1, E2, E3, This, FnObj, Home, NT, A, B, undefined, undefined};
frame_locals({E1, E2, E3}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 12) ->
    {E1, E2, E3, This, FnObj, Home, NT, A, B, undefined, undefined, undefined};
frame_locals(Env, {owned_lexical_slots, _}, This, FnObj, Home, NT, Args, Arity, LC) ->
    N = tuple_size(Env),
    list_to_tuple(onto(Env, N, [This, FnObj, Home, NT | slots(Args, Arity, LC - 4 - N)]));
%% arrows own no this slots
frame_locals({}, _, _, _, _, _, _, 0, 0) ->
    {};
frame_locals({}, _, _, _, _, _, _, 0, 1) ->
    {undefined};
frame_locals({}, _, _, _, _, _, _, 0, 2) ->
    {undefined, undefined};
frame_locals({}, _, _, _, _, _, _, 0, 3) ->
    {undefined, undefined, undefined};
frame_locals({}, _, _, _, _, _, [A | _], 1, 1) ->
    {A};
frame_locals({}, _, _, _, _, _, [A | _], 1, 2) ->
    {A, undefined};
frame_locals({}, _, _, _, _, _, [A | _], 1, 3) ->
    {A, undefined, undefined};
frame_locals({}, _, _, _, _, _, [A | _], 1, 4) ->
    {A, undefined, undefined, undefined};
frame_locals({}, _, _, _, _, _, [A, B | _], 2, 2) ->
    {A, B};
frame_locals({}, _, _, _, _, _, [A, B | _], 2, 3) ->
    {A, B, undefined};
frame_locals({}, _, _, _, _, _, [A, B | _], 2, 4) ->
    {A, B, undefined, undefined};
frame_locals({}, _, _, _, _, _, [A, B | _], 2, 5) ->
    {A, B, undefined, undefined, undefined};
frame_locals({}, _, _, _, _, _, [A, B, C | _], 3, 3) ->
    {A, B, C};
frame_locals({}, _, _, _, _, _, [A, B, C | _], 3, 4) ->
    {A, B, C, undefined};
frame_locals({}, _, _, _, _, _, [A, B, C | _], 3, 5) ->
    {A, B, C, undefined, undefined};
frame_locals({}, _, _, _, _, _, [A, B, C | _], 3, 6) ->
    {A, B, C, undefined, undefined, undefined};
frame_locals({}, _, _, _, _, _, [A, B, C, D | _], 4, 4) ->
    {A, B, C, D};
frame_locals({}, _, _, _, _, _, [A, B, C, D | _], 4, 5) ->
    {A, B, C, D, undefined};
frame_locals({}, _, _, _, _, _, [A, B, C, D | _], 4, 6) ->
    {A, B, C, D, undefined, undefined};
frame_locals({}, _, _, _, _, _, [A, B, C, D | _], 4, 7) ->
    {A, B, C, D, undefined, undefined, undefined};
frame_locals({}, _Lexical, _This, _FnObj, _Home, _NT, Args, Arity, LC) ->
    list_to_tuple(slots(Args, Arity, LC));
frame_locals({E1}, _, _, _, _, _, _, 0, 2) ->
    {E1, undefined};
frame_locals({E1}, _, _, _, _, _, _, 0, 3) ->
    {E1, undefined, undefined};
frame_locals({E1}, _, _, _, _, _, _, 0, 4) ->
    {E1, undefined, undefined, undefined};
frame_locals({E1}, _, _, _, _, _, [A | _], 1, 2) ->
    {E1, A};
frame_locals({E1}, _, _, _, _, _, [A | _], 1, 3) ->
    {E1, A, undefined};
frame_locals({E1}, _, _, _, _, _, [A | _], 1, 4) ->
    {E1, A, undefined, undefined};
frame_locals({E1}, _, _, _, _, _, [A | _], 1, 5) ->
    {E1, A, undefined, undefined, undefined};
frame_locals({E1}, _, _, _, _, _, [A, B | _], 2, 3) ->
    {E1, A, B};
frame_locals({E1}, _, _, _, _, _, [A, B | _], 2, 4) ->
    {E1, A, B, undefined};
frame_locals({E1}, _, _, _, _, _, [A, B | _], 2, 5) ->
    {E1, A, B, undefined, undefined};
frame_locals({E1}, _, _, _, _, _, [A, B | _], 2, 6) ->
    {E1, A, B, undefined, undefined, undefined};
frame_locals({E1, E2}, _, _, _, _, _, _, 0, 3) ->
    {E1, E2, undefined};
frame_locals({E1, E2}, _, _, _, _, _, _, 0, 4) ->
    {E1, E2, undefined, undefined};
frame_locals({E1, E2}, _, _, _, _, _, _, 0, 5) ->
    {E1, E2, undefined, undefined, undefined};
frame_locals({E1, E2}, _, _, _, _, _, [A | _], 1, 3) ->
    {E1, E2, A};
frame_locals({E1, E2}, _, _, _, _, _, [A | _], 1, 4) ->
    {E1, E2, A, undefined};
frame_locals({E1, E2}, _, _, _, _, _, [A | _], 1, 5) ->
    {E1, E2, A, undefined, undefined};
frame_locals({E1, E2}, _, _, _, _, _, [A | _], 1, 6) ->
    {E1, E2, A, undefined, undefined, undefined};
frame_locals({E1, E2}, _, _, _, _, _, [A, B | _], 2, 4) ->
    {E1, E2, A, B};
frame_locals({E1, E2}, _, _, _, _, _, [A, B | _], 2, 5) ->
    {E1, E2, A, B, undefined};
frame_locals({E1, E2}, _, _, _, _, _, [A, B | _], 2, 6) ->
    {E1, E2, A, B, undefined, undefined};
frame_locals({E1, E2}, _, _, _, _, _, [A, B | _], 2, 7) ->
    {E1, E2, A, B, undefined, undefined, undefined};
frame_locals({E1, E2, E3}, _, _, _, _, _, _, 0, 4) ->
    {E1, E2, E3, undefined};
frame_locals({E1, E2, E3}, _, _, _, _, _, _, 0, 5) ->
    {E1, E2, E3, undefined, undefined};
frame_locals({E1, E2, E3}, _, _, _, _, _, _, 0, 6) ->
    {E1, E2, E3, undefined, undefined, undefined};
frame_locals({E1, E2, E3}, _, _, _, _, _, [A | _], 1, 4) ->
    {E1, E2, E3, A};
frame_locals({E1, E2, E3}, _, _, _, _, _, [A | _], 1, 5) ->
    {E1, E2, E3, A, undefined};
frame_locals({E1, E2, E3}, _, _, _, _, _, [A | _], 1, 6) ->
    {E1, E2, E3, A, undefined, undefined};
frame_locals({E1, E2, E3}, _, _, _, _, _, [A | _], 1, 7) ->
    {E1, E2, E3, A, undefined, undefined, undefined};
frame_locals({E1, E2, E3}, _, _, _, _, _, [A, B | _], 2, 5) ->
    {E1, E2, E3, A, B};
frame_locals({E1, E2, E3}, _, _, _, _, _, [A, B | _], 2, 6) ->
    {E1, E2, E3, A, B, undefined};
frame_locals({E1, E2, E3}, _, _, _, _, _, [A, B | _], 2, 7) ->
    {E1, E2, E3, A, B, undefined, undefined};
frame_locals({E1, E2, E3}, _, _, _, _, _, [A, B | _], 2, 8) ->
    {E1, E2, E3, A, B, undefined, undefined, undefined};
frame_locals(Env, _Lexical, _This, _FnObj, _Home, _NT, [], 0, LC)
        when tuple_size(Env) =:= LC ->
    Env;
frame_locals(Env, _Lexical, _This, _FnObj, _Home, _NT, Args, Arity, LC) ->
    N = tuple_size(Env),
    list_to_tuple(onto(Env, N, slots(Args, Arity, LC - N))).

%% the first N elements of Tuple, in order, in front of Tail
onto(_, 0, Tail) -> Tail;
onto(T, 1, Tail) -> [element(1, T) | Tail];
onto(T, 2, Tail) -> [element(1, T), element(2, T) | Tail];
onto(T, 3, Tail) -> [element(1, T), element(2, T), element(3, T) | Tail];
onto(T, N, Tail) ->
    onto(T, N - 4, [element(N - 3, T), element(N - 2, T), element(N - 1, T),
                    element(N, T) | Tail]).

%% duplicate param names own fewer slots than arity
slots(Args, Arity, Room) when Room >= Arity -> fit(Args, Arity, Room - Arity);
slots(Args, _Arity, Room) -> fit(Args, Room, 0).

fit([], 0, Extra) -> pad(Extra);
fit([A], 1, Extra) -> [A | pad(Extra)];
fit([A, B], 2, Extra) -> [A, B | pad(Extra)];
fit([A, B, C], 3, Extra) -> [A, B, C | pad(Extra)];
fit([A | Args], Arity, Extra) when Arity > 0 -> [A | fit(Args, Arity - 1, Extra)];
fit(_, Arity, Extra) -> pad(Arity + Extra).

pad(0) -> [];
pad(1) -> [undefined];
pad(2) -> [undefined, undefined];
pad(3) -> [undefined, undefined, undefined];
pad(N) when N > 3 -> [undefined, undefined, undefined, undefined | pad(N - 4)].

%% §10.2.1.2 sloppy this, miss means primitive needs a wrapper
sloppy_this({?HANDLE_TAG, _} = This, _Global) -> This;
sloppy_this(undefined, Global) -> Global;
sloppy_this(null, Global) -> Global;
sloppy_this(js_tdz, _Global) -> js_tdz;
sloppy_this(_, _) -> miss.

flush_registers(L, A, B, R0, R1) ->
    put_reg(put_reg(L, A, R0), B, R1).

%% a register that was only read still holds what its slot does; floats are
%% always written, since not every vm tells 0.0 from -0.0 with =:=
put_reg(L, S, _) when S < 0 -> L;
put_reg(L, S, V) when is_float(V) -> setelement(S + 1, L, V);
put_reg(L, S, V) ->
    case element(S + 1, L) =:= V of
        true -> L;
        false -> setelement(S + 1, L, V)
    end.

%% what a closure captured: the leading run of its frame's locals
captured(_, 0) -> {};
captured(Locals, Count) ->
    list_to_tuple(onto(Locals, Count, [])).

