-module(arc_rt_bytecode_ffi).
-export([shared/1]).

-include("arc_rt_layout.hrl").

%% the template with each op, key and string that is in it more than once made
%% the one term. The compiler makes them apart, and a copy between processes
%% would set them apart again
shared(Template) ->
    case erlang:system_info(machine) of
        "BEAM" -> element(1, template(Template, #{}));
        %% atomvm looks through a map key by key, which costs more than it saves
        _ -> Template
    end.

template(T, Seen) when tuple_size(T) =:= ?FUNCTEMPLATE_SIZE ->
    {Ops, Seen1} = ops(tuple_to_list(element(?FUNCTEMPLATE_BYTECODE, T)), Seen),
    {Children, Seen2} = lists:mapfoldl(fun template/2, Seen1,
                                       tuple_to_list(element(?FUNCTEMPLATE_FUNCTIONS, T))),
    {Name, Seen3} = term(element(?FUNCTEMPLATE_NAME, T), Seen2),
    {Lexical, Seen4} = term(element(?FUNCTEMPLATE_LEXICAL, T), Seen3),
    T1 = setelement(?FUNCTEMPLATE_BYTECODE, T, list_to_tuple(Ops)),
    T2 = setelement(?FUNCTEMPLATE_FUNCTIONS, T1, list_to_tuple(Children)),
    T3 = setelement(?FUNCTEMPLATE_NAME, T2, Name),
    {setelement(?FUNCTEMPLATE_LEXICAL, T3, Lexical), Seen4}.

ops([Op | Ops], Seen) ->
    {Op1, Seen1} = op(Op, Seen),
    {Ops1, Seen2} = ops(Ops, Seen1),
    {[Op1 | Ops1], Seen2};
ops([], Seen) -> {[], Seen}.

%% a name whose id tells it apart is quicker to find by that than by its text
op({Tag, {?KEY_NAMED, Id, _} = K}, Seen) when ?ID_IS_EXACT(Id) ->
    As = [Tag | Id],
    case Seen of
        #{As := Known} -> {Known, Seen};
        _ ->
            {K1, Seen1} = term(K, Seen),
            Op = {Tag, K1},
            {Op, Seen1#{As => Op}}
    end;
%% long strings are not on the heap
op({_, Text} = Op, Seen) when byte_size(Text) > 64 -> {Op, Seen};
op({keep_unit_text, _, _} = Op, Seen) -> {Op, Seen};
%% not every vm tells 0.0 from -0.0 as a key, so an op with a float stays as it is
op(Op, Seen) ->
    try term(Op, Seen)
    catch throw:float -> {Op, Seen}
    end.

terms([X | Xs], Seen) ->
    {X1, Seen1} = term(X, Seen),
    {Xs1, Seen2} = terms(Xs, Seen1),
    {[X1 | Xs1], Seen2};
terms([], Seen) -> {[], Seen}.

term({?KEY_NAMED, Id, _} = K, Seen) when ?ID_IS_EXACT(Id) ->
    case Seen of
        #{Id := Known} -> {Known, Seen};
        _ -> {K, Seen#{Id => K}}
    end;
term({Tag, N} = X, Seen) when is_atom(Tag), is_integer(N) ->
    case Seen of
        #{X := Known} -> {Known, Seen};
        _ -> {X, Seen#{X => X}}
    end;
term(X, Seen) when is_tuple(X) ->
    case Seen of
        #{X := Known} -> {Known, Seen};
        _ ->
            {Parts, Seen1} = terms(tuple_to_list(X), Seen),
            X1 = list_to_tuple(Parts),
            {X1, Seen1#{X1 => X1}}
    end;
term(X, Seen) when is_binary(X) ->
    case Seen of
        #{X := Known} -> {Known, Seen};
        _ -> {X, Seen#{X => X}}
    end;
term(X, Seen) when is_list(X) -> terms(X, Seen);
term(X, _) when is_float(X) -> throw(float);
term(X, Seen) -> {X, Seen}.
