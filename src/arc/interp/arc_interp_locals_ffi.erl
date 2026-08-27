%%% arc_interp_locals_ffi — the call prologue's value kernels: the locals
%%% tuple build (`frame_locals/9`) and sloppy `this` binding (`bind_this/2`).
%%% Pure term construction: no process state, no NIF.
-module(arc_interp_locals_ffi).
-export([frame_locals/9, bind_this/2]).

-include("../rt/arc_rt_layout.hrl").

%% frame_locals(Env, Lexical, This, FnObj, Home, NT, Args, Arity, LocalCount)
%%     -> tuple()
%% The callee's locals tuple, laid out
%%   [Env..., Seeds..., Args (fitted to Arity)..., undefined × rest]
%% in one pass. Env is the closure's captured environment tuple. Lexical is
%% the template's arc/bytecode/lexical.LexicalSlots term (that module owns
%% the seed order, see lexical.all_lexical_refs):
%%   {owned_lexical_slots, Base} — the body owns all four slots, contiguous
%%       right after the env (Base == tuple_size(Env)), seeded
%%       [This, FnObj, Home, NT];
%%   {captured_lexical_slots, ...} / no_lexical_slots — nothing is seeded
%%       (an arrow reads its parent's boxes through the env; a body that
%%       names none has no slots).
%% Actual arguments beyond Arity (or beyond the slots the body owns) are
%% dropped: they stay in the frame's call args.
%% The leading clauses spell out the commonest small frames (no env, every
%% parameter supplied, one or two spare slots) as literal tuples.
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 5) ->
    {This, FnObj, Home, NT, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 6) ->
    {This, FnObj, Home, NT, A, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 7) ->
    {This, FnObj, Home, NT, A, B, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B, C | _], 3, 8) ->
    {This, FnObj, Home, NT, A, B, C, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, _, 0, 6) ->
    {This, FnObj, Home, NT, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A | _], 1, 7) ->
    {This, FnObj, Home, NT, A, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, [A, B | _], 2, 8) ->
    {This, FnObj, Home, NT, A, B, undefined, undefined};
frame_locals({}, {owned_lexical_slots, _}, This, FnObj, Home, NT, Args, Arity, LC) ->
    list_to_tuple([This, FnObj, Home, NT | slots(Args, Arity, LC - 4)]);
frame_locals(Env, {owned_lexical_slots, _}, This, FnObj, Home, NT, Args, Arity, LC) ->
    N = tuple_size(Env),
    list_to_tuple(tuple_to_list(Env)
                  ++ [This, FnObj, Home, NT | slots(Args, Arity, LC - 4 - N)]);
frame_locals({}, _Lexical, _This, _FnObj, _Home, _NT, Args, Arity, LC) ->
    list_to_tuple(slots(Args, Arity, LC));
frame_locals(Env, _Lexical, _This, _FnObj, _Home, _NT, [], 0, LC)
        when tuple_size(Env) =:= LC ->
    Env;
frame_locals(Env, _Lexical, _This, _FnObj, _Home, _NT, Args, Arity, LC) ->
    N = tuple_size(Env),
    list_to_tuple(tuple_to_list(Env) ++ slots(Args, Arity, LC - N)).

%% The Room slots after the env and seeds: the parameters then the spare
%% locals. A body binding duplicate parameter names (sloppy `function(a, a)`)
%% owns fewer slots than its Arity; the parameter run is cut to fit.
slots(Args, Arity, Room) when Room >= Arity -> fit(Args, Arity, Room - Arity);
slots(Args, _Arity, Room) -> fit(Args, Room, 0).

%% Args fitted to exactly Arity slots, then Extra undefineds.
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

%% bind_this(ThisArg, Global) -> JsVal | miss
%% §10.2.1.2 OrdinaryCallBindThis for a SLOPPY non-arrow callee: an object
%% (or the derived-constructor TDZ sentinel) passes through, null/undefined
%% become the realm's global object, and a primitive misses (it needs a
%% wrapper object allocated: rt/call.resolve_this).
bind_this({?HANDLE_TAG, _} = This, _Global) -> This;
bind_this(undefined, Global) -> Global;
bind_this(null, Global) -> Global;
bind_this(js_tdz, _Global) -> js_tdz;
bind_this(_, _) -> miss.
