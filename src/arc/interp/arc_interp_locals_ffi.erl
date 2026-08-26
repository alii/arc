%%% arc_interp_locals_ffi — the one-pass locals tuple build for a call
%%% prologue (`setup_locals_tuple/6`, `setup_locals_seeded/10`). Pure term
%%% construction: no process state, no NIF.
-module(arc_interp_locals_ffi).
-export([setup_locals_tuple/6, setup_locals_seeded/10]).

%% Build the locals tuple for a JS function call in one forward pass:
%%   [Env..., Seeds..., Args(padded/truncated to Arity)..., Undef × rest]
%% One body-recursive build + list_to_tuple instead of append / reverse
%% chains. Env is the closure's captured environment, a list or a tuple of
%% values. local_count is compiler-bounded, so non-tail recursion is fine.
setup_locals_tuple({}, [], Args, Arity, Arity, _Undef) when length(Args) =:= Arity ->
    list_to_tuple(Args);
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
%% No env, every arg supplied, no extra locals: the tuple is the seeds
%% followed by the args as given.
setup_locals_seeded({}, {owned_lexical_slots, _Base},
                    This, FnObj, Home, NT, Args, Arity, LocalCount, _Undef)
        when LocalCount =:= Arity + 4, length(Args) =:= Arity ->
    list_to_tuple([This, FnObj, Home, NT | Args]);
%% No env: the seeds lead, then the args padded/truncated to the local count.
setup_locals_seeded({}, {owned_lexical_slots, _Base},
                    This, FnObj, Home, NT, Args, Arity, LocalCount, Undef)
        when LocalCount >= 4 ->
    list_to_tuple([This, FnObj, Home, NT
                   | locals_args(Args, Arity, LocalCount - 4, Undef)]);
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
%% Every parameter supplied, no extras: the args as given, then the pad.
locals_args(Args, Arity, N, Undef) when N >= Arity, length(Args) =:= Arity ->
    Args ++ locals_pad(N - Arity, Undef);
locals_args([A | Args], Arity, N, Undef) ->
    [A | locals_args(Args, Arity - 1, N - 1, Undef)];
locals_args([], Arity, N, Undef) ->
    [Undef | locals_args([], Arity - 1, N - 1, Undef)].

locals_pad(0, _) -> [];
locals_pad(N, Undef) -> [Undef | locals_pad(N - 1, Undef)].
