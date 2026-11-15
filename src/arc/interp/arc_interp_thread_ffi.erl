%% builds a function's threaded code and runs it; see arc_interp_thread.hrl
-module(arc_interp_thread_ffi).
-export([enter/1, build/2]).
-export([put_local/13, store_top/2, fill/1, via/13, via_op/12, via_flushed/12,
         via_local/14, stepped/10, tdz/11, threw/11,
         resume_generator/6, redrafted/2, collected/10]).

-include("arc_interp_thread.hrl").

%% what building needs of the function. pcs is the table's
-record(cx, {ops, lines, pcs, regs, keys, template, returns, shapes, fields, draft, hints,
             here}).

-define(DRAFT_RUNS, 8).
-define(OPS_PER_CELL, 4).
-define(META(A), ((A#agent.store)#store.meta)).

%% running

enter(#state{agent = A, pc = Pc, stack = St, locals = L, frame = F} = State) ->
    case F#frame.func of
        #func_template{code = ?CODE(_, _, _, C)} = Draft when element(?TABLE_DRAFT, C) ->
            case redrafted(A, Draft) of
                none -> enter(Pc, St, F, L, A);
                Template -> enter(Pc, St, F#frame{func = Template}, L, A)
            end;
        #func_template{code = ?CODE(_, _, _, _)} -> enter(Pc, St, F, L, A);
        Template ->
            {Threaded, A1} = build(A, Template),
            enter(State#state{agent = A1, frame = F#frame{func = Threaded}})
    end.

enter(Pc, St, F, L, A) ->
    #func_template{code = ?CODE(_, _, Start, C), regs = Regs} = F#frame.func,
    {R0, R1, R2, R3} = load(Regs, L),
    case Pc of
        1 when element(?TABLE_FIRST, C) =:= none -> ?GO(Start, undefined, St);
        1 -> ?GO((element(?TABLE_FIRST, C)), undefined, St);
        _ -> resume(Pc, St, ?ENV)
    end.

resume(Pc, St, ?ENV) ->
    case element(?TABLE_PCS, C) of
        #{Pc := Ix} when Ix > 0 -> (element(Ix, C))(undefined, St, ?ENV);
        #{Pc := Ix} ->
            [T | St1] = St,
            (element(-Ix, C))(T, St1, ?ENV);
        %% nothing is known to come back in here, so nothing was kept for it
        Pcs ->
            ?GO((from(Pc, e, cx(A, F#frame.func, Pcs, #{}))), undefined, St)
    end.

load(no_regs, _) -> {undefined, undefined, undefined, undefined};
load({regs, A, B, C, D}, L) -> {reg(A, L), reg(B, L), reg(C, L), reg(D, L)}.

reg(0, _) -> undefined;
reg(I, L) -> element(I, L).

flushed(F, L, ?RS) ->
    arc_interp_locals_ffi:flush_registers(L, (F#frame.func)#func_template.regs, ?RS).

state(Pc, St, F, L, A) ->
    #state{agent = 'arc@interp@frames':sync(F, A, Pc), pc = Pc, stack = St, locals = L,
           frame = F}.

%% has the general path run Op, which mustn't look at a local that may be in a
%% register, then goes on to N. Out says where N looks for the top
via(Op, St, ?ENV, N, Pc, Out) ->
    stepped('arc@interp@interpreter':step(state(Pc, St, F, L, A), Op), F, C, ?RS, N, Pc, Out).

via_op(St, ?ENV, N, Pc, Out) ->
    via(element(Pc, (F#frame.func)#func_template.bytecode), St, ?ENV, N, Pc, Out).

%% the same for the op at Pc when it does look
via_flushed(St, ?ENV, N, Pc, Out) ->
    Op = element(Pc, (F#frame.func)#func_template.bytecode),
    flushing(Op, St, ?ENV, N, Pc, Out).

%% and for {Op, I} on a local, leaving T be
via_local(Op, I, T, St, ?ENV, N, P) ->
    Slot = case I < 0 of
        true -> element(I - ?IN_R0 + 2, (F#frame.func)#func_template.regs);
        false -> I
    end,
    case P < 0 of
        true -> flushing({Op, Slot}, [T | St], ?ENV, N, -P, c);
        false -> flushing({Op, Slot}, St, ?ENV, N, P, e)
    end.

flushing(Op, St, ?ENV, N, Pc, Out) ->
    State = state(Pc, St, F, flushed(F, L, ?RS), A),
    case 'arc@interp@interpreter':step(State, Op) of
        {ok, #state{locals = L1, frame = F1}} = Stepped ->
            Mine = case F1 of
                #frame{depth = Depth} when Depth =:= F#frame.depth -> L1;
                #frame{return_to = Saved} -> element(#saved_frame.locals, Saved)
            end,
            {Q0, Q1, Q2, Q3} = load((F#frame.func)#func_template.regs, Mine),
            stepped(Stepped, F, C, Q0, Q1, Q2, Q3, N, Pc, Out);
        {error, Exit} -> 'arc@interp@interpreter':after_exit(Exit)
    end.

%% for an op that may go somewhere other than on
via_jumping(Op, St, ?ENV, Pc) ->
    case 'arc@interp@interpreter':step(state(Pc, St, F, flushed(F, L, ?RS), A), Op) of
        {ok, State} -> enter(State);
        {error, Exit} -> 'arc@interp@interpreter':after_exit(Exit)
    end.

%% goes on from what the general path made of the op at Pc of frame F
stepped({ok, #state{agent = A, pc = Pc1, stack = St, locals = L, frame = F1} = State},
        F, C, ?RS, N, Pc, Out) ->
    Depth = F#frame.depth,
    case F1 of
        #frame{depth = Depth} when Pc1 =:= Pc + 1 ->
            case Out of
                e -> N(undefined, St, F1, L, A, C, ?RS);
                c ->
                    [T | St1] = St,
                    N(T, St1, F1, L, A, C, ?RS)
            end;
        #frame{depth = Inner, return_to = Saved} when Inner =:= Depth + 1 ->
            enter(State#state{frame = F1#frame{return_to = resumable(Saved, C, ?RS, N, Out)}});
        %% a tail call, or a stub that became the real function
        _ -> enter(State)
    end;
stepped({error, Exit}, F, _, ?RS, _, _, _) ->
    Flush = fun(#state{locals = L} = State) -> State#state{locals = flushed(F, L, ?RS)} end,
    'arc@interp@interpreter':after_exit('arc@interp@state':map_exit(Exit, Flush)).

resumable(#saved_frame{caller = F, pc = Pc, stack = St, locals = L,
                       constructor_this = CtorThis}, C, ?RS, N, c) ->
    #saved_regs{caller = F, pc = Pc, stack = St, locals = L, constructor_this = CtorThis,
                next = N, code = C, r0 = R0, r1 = R1, r2 = R2, r3 = R3};
resumable(#saved_frame{} = Saved, C, ?RS, N, e) -> resumable(Saved, C, ?RS, spill(N), c);
resumable(#saved_cont{} = Saved, _, ?RS, N, Out) ->
    Resume = #resumption{next = N, takes_top = Out =:= c, r0 = R0, r1 = R1, r2 = R2, r3 = R3},
    Saved#saved_cont{resume = {?SOME, Resume}}.

tdz(T, St, ?ENV, P) when P < 0 -> tdz(T, [T | St], ?ENV, -P);
tdz(_, St, ?ENV, Pc) ->
    Op = {throw_reference_error, <<"Cannot access variable before initialization (TDZ)">>},
    via(Op, St, ?ENV, none, Pc, e).

threw(Thrown, St, ?ENV, Pc) ->
    State = #state{agent = A, pc = Pc, stack = St, locals = flushed(F, L, ?RS), frame = F},
    'arc@interp@interpreter':after_exit({threw, Thrown, State}).

-define(SIZED(Of, At), {Of} -> N(T, St, F, case D of At end, A, C, ?RS)).

put_local(T, St, ?ENV, N, D, V) ->
    case L of
        ?SIZED(?OF1, ?AT1(L, V));
        ?SIZED(?OF2, ?AT2(L, V));
        ?SIZED(?OF3, ?AT3(L, V));
        ?SIZED(?OF4, ?AT4(L, V));
        ?SIZED(?OF5, ?AT5(L, V));
        ?SIZED(?OF6, ?AT6(L, V));
        ?SIZED(?OF7, ?AT7(L, V));
        ?SIZED(?OF8, ?AT8(L, V));
        ?SIZED(?OF9, ?AT9(L, V));
        ?SIZED(?OF10, ?AT10(L, V));
        ?SIZED(?OF11, ?AT11(L, V));
        ?SIZED(?OF12, ?AT12(L, V));
        ?SIZED(?OF13, ?AT13(L, V));
        ?SIZED(?OF14, ?AT14(L, V));
        ?SIZED(?OF15, ?AT15(L, V));
        ?SIZED(?OF16, ?AT16(L, V));
        _ -> N(T, St, F, setelement(D, L, V), A, C, ?RS)
    end.

%% building

%% code that works on objects is built twice
build(A, #func_template{is_generator = false, is_async = false, bytecode = Ops} = Template) ->
    case lists:any(fun on_shapes/1, tuple_to_list(Ops)) of
        true -> build(A, Template, true, #{});
        false -> build(A, Template, false, #{})
    end;
build(A, Template) -> build(A, Template, false, #{}).

build(#agent{store = #store{meta = Meta}} = A, Template, Draft, Hints) ->
    #func_template{bytecode = Ops, functions = Functions} = Template,
    {Named, NextId} = name_children(tuple_to_list(Functions), Meta#store_meta.next_template_id),
    FirstFields = first_fields(Template),
    Joins = case FirstFields of
        {First, _, Fields} -> [First + length(Fields)];
        none -> []
    end,
    {Heads, Keys} = survey(Ops, 1, tuple_size(Ops), 1, #{}, Joins, Meta#store_meta.thread_keys),
    Pcs = maps:from_list(places(Heads, ?TABLE_DRAFT + 1, Ops)),
    Template1 = Template#func_template{functions = list_to_tuple(Named)},
    X0 = fields_shape(FirstFields, cx(A, Template1, Pcs, Keys)),
    X = case Draft of
        true -> X0#cx{draft = true};
        false -> X0#cx{hints = Hints}
    end,
    Rest = [at(Pc, wants(element(Pc, Ops)), X) || Pc <- Heads],
    {Start, AtOne} = case loader(Template) of
        none -> {from(1, e, X), none};
        Loader -> {Loader, from(1, e, X)}
    end,
    Entry = case Draft of
        true -> ?CALLS:draft_entry(Template1);
        false -> ?CALLS:entry(Template)
    end,
    Code = ?CODE(takes_this(Template), Entry, Start, list_to_tuple([Pcs, AtOne, Draft | Rest])),
    %% code takes room too, and it is a full gc that lets go of it
    Weight = (tuple_size(Ops) + 32) div ?OPS_PER_CELL,
    Meta1 = Meta#store_meta{next_template_id = NextId, thread_keys = Keys,
                            major_live = Meta#store_meta.major_live - Weight},
    {Template1#func_template{code = Code},
     with_meta(A#agent{gc_at = A#agent.gc_at - Weight}, Meta1)}.

%% what to run in place of a draft, once that has been built
redrafted(A, #func_template{id = Id, name = Name}) ->
    Known = case ?META(A) of
        #store_meta{threaded = #{Id := Lately}} -> Lately;
        #store_meta{threaded_before = #{Id := Before}} -> Before;
        _ -> none
    end,
    case Known of
        #func_template{code = ?CODE(_, _, _, C)} when element(?TABLE_DRAFT, C) -> none;
        #func_template{name = Name} -> Known;
        #func_template{} -> Known#func_template{name = Name};
        none -> none
    end.

%% counts a draft's way out or trip round a loop, and after a few of them builds
%% it again for what its locals then hold
draft_ran(#frame{func = #func_template{id = Id} = Draft} = F, L, A, ?RS) ->
    #store_meta{draft_runs = Runs} = Meta = ?META(A),
    case {redrafted(A, Draft), maps:get(Id, Runs, 1)} of
        {none, Run} when Run < ?DRAFT_RUNS ->
            {none, with_meta(A, Meta#store_meta{draft_runs = Runs#{Id => Run + 1}})};
        {none, _} ->
            {Template, A1} = build(A, Draft, false, hints(A, Draft, flushed(F, L, ?RS))),
            #store_meta{threaded = Threaded} = Meta1 = ?META(A1),
            Threaded1 = case Id of
                0 -> Threaded;
                _ -> Threaded#{Id => Template}
            end,
            {Template, with_meta(A1, Meta1#store_meta{threaded = Threaded1,
                                                      draft_runs = maps:remove(Id, Runs)})};
        {Template, _} -> {Template, A}
    end.

with_meta(#agent{store = Store} = A, Meta) -> A#agent{store = Store#store{meta = Meta}}.

round_again(Pc, S) ->
    ?PUSHING(S,
             case draft_ran(F, L, A, ?RS) of
                 {none, A1} -> resume(Pc, St, F, L, A1, C, ?RS);
                 {#func_template{code = ?CODE(_, _, _, C1)} = Template, A1} ->
                     resume(Pc, St, F#frame{func = Template}, L, A1, C1, ?RS)
             end).

leaving(N) when ?IS_OP(N) -> ?FN -> ?GO(N, T, St, element(2, draft_ran(F, L, A, ?RS))) end.

on_shapes({new_object_shaped, _, _}) -> true;
on_shapes(Op) -> field_use(Op) =/= none.

%% {where the object comes from, key} for an op on a named field
field_use(Op) ->
    case Op of
        {get_local_field, I, K} -> {I, K};
        {get_local_field_keep, I, K} -> {I, K};
        {get_local_field_call, I, K} -> {I, K};
        {bin_op_local_field, _, I, K} -> {I, K};
        {put_local_local_field, I, _, K} -> {I, K};
        {put_local_const_field, I, _, K} -> {I, K};
        {get_field, K} -> {top, K};
        {get_field_keep, K} -> {top, K};
        {get_field_call, K} -> {top, K};
        {get_field_call1, K, _} -> {top, K};
        {put_field, K} -> {below, K};
        {put_field_pop, K} -> {below, K};
        _ -> none
    end.

%% by pc, the shape each such op would have met, as far as L tells. One it does
%% not tell takes what another found for the same key
hints(A, #func_template{bytecode = Ops}, L) ->
    Uses = [{Pc, From, K} || Pc <- lists:seq(1, tuple_size(Ops)),
                             {From, {?KEY_NAMED, _, _} = K} <- [field_use(element(Pc, Ops))]],
    Met = [{Pc, K, met(A, receiver(From, Pc, Ops, A, L), K)} || {Pc, From, K} <- Uses],
    ByKey = maps:from_list([{K, Hint} || {_, K, {_, At} = Hint} <- Met, At >= 0]),
    maps:from_list([{Pc, Hint} || {Pc, K, Own} <- Met,
                                  {_, _} = Hint <- [case Own of
                                                        none -> maps:get(K, ByKey, none);
                                                        _ -> Own
                                                    end]]).

receiver(below, _, _, _, _) -> miss;
receiver(top, Pc, Ops, A, L) -> pushed(Pc - 1, Ops, A, L, 4);
receiver(I, _, _, _, L) -> element(I, L).

%% what the op at Pc would push now, or miss
pushed(Pc, _, _, _, Back) when Pc =:= 0; Back =:= 0 -> miss;
pushed(Pc, Ops, A, L, Back) ->
    case element(Pc, Ops) of
        {get_local, I} -> element(I, L);
        {get_local_field, I, K} -> arc_interp_prop_ffi:get_field(A, element(I, L), K);
        {get_field, K} ->
            arc_interp_prop_ffi:get_field(A, pushed(Pc - 1, Ops, A, L, Back - 1), K);
        {get_elem_locals, I, J} -> arc_interp_prop_ffi:get_elem(A, element(I, L), element(J, L));
        {get_env, I} -> element(I, element(1, L));
        {get_boxed, I} -> arc_interp_ffi:box_get(A, element(I, L));
        {get_env_boxed, I} -> arc_interp_ffi:box_get(A, element(I, element(1, L)));
        {get_global, K} ->
            arc_interp_prop_ffi:get_global(A, element(?REALM_LEXICAL_GLOBALS, A#agent.realm), K);
        _ -> miss
    end.

met(A, V, K) ->
    case arc_interp_ffi:cell_of(A, V) of
        {?SSHAPEDOBJECT_TAG, Shape, _, _, Offs} ->
            case Offs of
                #{?KEY_ID(K) := {SK, At}} when ?SAME_TEXT(SK, K) -> {Shape, At};
                #{?KEY_ID(K) := _} -> none;
                _ -> {Shape, -1}
            end;
        _ -> none
    end.

hint(Pc, #cx{hints = Hints}) -> maps:get(Pc, Hints, {none, 0}).

put_hint(Pc, X) ->
    case hint(Pc, X) of
        {_, -1} -> {none, 0};
        Hint -> Hint
    end.

cx(A, #func_template{bytecode = Ops, lines = Lines, regs = Regs} = Template, Pcs, Keys) ->
    #cx{ops = Ops, lines = Lines, pcs = Pcs, regs = Regs, keys = Keys, template = Template,
        returns = returns(Template), shapes = (A#agent.store)#store.shapes, fields = none,
        draft = false, hints = #{}, here = 0}.

%% the writes a constructor likely opens with, as {Pc, object's slot, [{Key,
%% slot or {constant}}]}
first_fields(#func_template{is_constructor = true, is_derived_constructor = false,
                            bytecode = Ops}) ->
    first_fields(Ops, 1);
first_fields(_) -> none.

first_fields(Ops, Pc) when Pc =< tuple_size(Ops) ->
    case element(Pc, Ops) of
        {Tag, I, _, _} when Tag =:= put_local_local_field; Tag =:= put_local_const_field ->
            case fields(Ops, Pc, I, #{?KEY_ID(?PROTO_KEY) => nil}) of
                [_, _ | _] = Fields -> {Pc, I, Fields};
                _ -> none
            end;
        _ -> first_fields(Ops, Pc + 1)
    end;
first_fields(_, _) -> none.

fields(Ops, Pc, I, Seen) when map_size(Seen) =< 16 ->
    case element(Pc, Ops) of
        {put_local_local_field, I, J, {?KEY_NAMED, Id, _} = K} when not is_map_key(Id, Seen) ->
            [{K, J} | fields(Ops, Pc + 1, I, Seen#{Id => nil})];
        {put_local_const_field, I, V, {?KEY_NAMED, Id, _} = K} when not is_map_key(Id, Seen) ->
            [{K, {V}} | fields(Ops, Pc + 1, I, Seen#{Id => nil})];
        _ -> []
    end;
fields(_, _, _, _) -> [].

%% the shape those writes have taken a new object to, unless a jump lands
%% among them
fields_shape({Pc, _, Fields} = Run, #cx{pcs = Pcs, shapes = Shapes} = X) ->
    Past = Pc + length(Fields),
    case is_map_key(Past, Pcs)
         andalso not lists:any(fun(P) -> is_map_key(P, Pcs) end, lists:seq(Pc + 1, Past - 1))
         andalso shape_after(Fields, Shapes, {0, #{}}) of
        {Shape, Offs} -> X#cx{fields = {Run, Shape, Offs}};
        _ -> X
    end;
fields_shape(none, X) -> X.

shape_after([{K, _} | Fields], Shapes, {From, _}) ->
    Step = ?SHAPE_STEP(K),
    shape_after(Fields, Shapes, ?SHAPED_NEXT(Shapes, From, Step));
shape_after(_, _, Reached) -> Reached.

%% a function that makes nothing itself leaves collecting to one that does
returns(#func_template{is_derived_constructor = true}) -> derived;
returns(#func_template{bytecode = Ops}) ->
    case lists:any(fun makes/1, tuple_to_list(Ops)) of
        true -> collecting;
        false -> plain
    end.

makes(Op) when is_atom(Op) ->
    case Op of
        return -> false;
        pop -> false;
        dup -> false;
        get_elem -> false;
        put_elem -> false;
        put_elem_pop -> false;
        type_of -> false;
        throw -> false;
        _ -> true
    end;
makes(Op) ->
    case element(1, Op) of
        call -> true;
        call_method -> true;
        call_new -> true;
        call_constructor -> true;
        call_eval -> true;
        get_field_call -> true;
        get_field_call1 -> true;
        get_local_field_call -> true;
        apply_arguments -> true;
        new_object_with -> true;
        new_object_shaped -> true;
        array_from -> true;
        array_from_with_holes -> true;
        object_rest_copy -> true;
        make_closure -> true;
        make_lazy_closure -> true;
        box_local -> true;
        box_locals -> true;
        create_arguments -> true;
        create_rest_array -> true;
        _ -> false
    end.

name_children([#func_template{id = 0} = Child | Rest], Id) ->
    {Named, NextId} = name_children(Rest, Id + 1),
    {[Child#func_template{id = Id} | Named], NextId};
name_children([Child | Rest], Id) ->
    {Named, NextId} = name_children(Rest, Id),
    {[Child | Named], NextId};
name_children([], Id) -> {[], Id}.

takes_this(#func_template{is_generator = true}) -> special;
takes_this(#func_template{is_async = true}) -> special;
takes_this(#func_template{is_class_constructor = true}) -> special;
takes_this(#func_template{is_arrow = true}) -> as_given;
takes_this(#func_template{is_strict = true}) -> as_given;
takes_this(#func_template{lexical = {trimmed_lexical_slots, _, false}}) -> as_given;
takes_this(_) -> coerced.

places([Pc | Heads], Ix, Ops) ->
    case wants(element(Pc, Ops)) of
        e -> [{Pc, Ix} | places(Heads, Ix + 1, Ops)];
        c -> [{Pc, -Ix} | places(Heads, Ix + 1, Ops)]
    end;
places([], _, _) -> [].

%% finds the heads, the pcs that go in the table: those reached from more than
%% one place, from further on, or from outside. Every other closure has just
%% one holder, so copying a function's code never multiplies it. Falls is 1
%% when the op before Pc goes on to it; Into counts the jumps to each pc, and
%% one from outside or from further on counts for two. Also gathers the names
%% whose text has to be compared, so that every use of one shares it
survey(Ops, Pc, Last, Falls, Into, Joins, Keys) when Pc =< Last ->
    Joins1 = case Into of
        #{Pc := Count} when Count + Falls > 1 -> [Pc | Joins];
        _ -> Joins
    end,
    Op = element(Pc, Ops),
    case edges(Op, Pc) of
        %% see after_next/2
        on when Op =:= iterator_next; Op =:= for_in_next ->
            Joins2 = case element(Pc + 1, Ops) of
                {Tag, _} when Tag =:= jump_if_true; Tag =:= jump_if_false -> Joins1;
                _ -> [Pc + 1 | Joins1]
            end,
            survey(Ops, Pc + 1, Last, 1, Into, Joins2, Keys);
        on -> survey(Ops, Pc + 1, Last, 1, Into, Joins1, shared(Op, Keys));
        {Falls1, Jumps, Outside} ->
            {Into1, Joins2} = jumps(Jumps, 1, Pc, Into, Joins1),
            {Into2, Joins3} = jumps(Outside, 2, Pc, Into1, Joins2),
            survey(Ops, Pc + 1, Last, Falls1, Into2, Joins3, Keys)
    end;
survey(_, _, _, _, _, Joins, Keys) -> {lists:usort(Joins), Keys}.

jumps([To | Rest], Weight, Pc, Into, Joins) when To =< Pc ->
    jumps(Rest, Weight, Pc, Into, [To | Joins]);
jumps([To | Rest], Weight, Pc, Into, Joins) ->
    jumps(Rest, Weight, Pc, Into#{To => maps:get(To, Into, 0) + Weight}, Joins);
jumps([], _, _, Into, Joins) -> {Into, Joins}.

edges(Op, Pc) when is_atom(Op) ->
    case Op of
        return -> {0, [], []};
        throw -> {0, [], []};
        ret -> {0, [], []};
        initial_yield -> {0, [], [Pc + 1]};
        yield -> {0, [], [Pc + 1]};
        await -> {0, [], [Pc + 1]};
        yield_star -> {0, [], [Pc, Pc + 1]};
        async_yield_star_next -> {0, [], [Pc, Pc + 1]};
        _ -> on
    end;
edges(Op, Pc) ->
    case Op of
        {jump, To} -> {0, [To], []};
        {jump_if_false, To} -> {1, [To], []};
        {jump_if_true, To} -> {1, [To], []};
        {jump_if_nullish, To} -> {1, [To], []};
        {jump_if_not_nullish, To} -> {1, [To], []};
        {jump_if_false_or_pop, To} -> {1, [To], []};
        {jump_if_true_or_pop, To} -> {1, [To], []};
        {jump_if_not_nullish_or_pop, To} -> {1, [To], []};
        {jump_undefined_if_nullish, To} -> {1, [To], []};
        {iterate_view, _, To} -> {1, [To], []};
        {unpack_array, _, To} -> {1, [To], []};
        {inc_local_jump, _, To} -> {0, [To], []};
        {jump_if_local, _, Yes, No} -> {0, [Yes, No], []};
        {inc_local_cmp_const_jump, _, _, _, _, Yes, No} -> {0, [Yes, No], []};
        {inc_local_cmp_local_jump, _, _, _, _, Yes, No} -> {0, [Yes, No], []};
        {cmp_local_local_jump, _, _, _, Yes, No} -> {0, [Yes, No], []};
        {cmp_local_const_jump, _, _, _, Yes, No} -> {0, [Yes, No], []};
        {cmp_jump, _, Yes, No} -> {0, [Yes, No], []};
        {cmp_const_jump, _, _, Yes, No} -> {0, [Yes, No], []};
        {gosub, To} -> {0, [To], [Pc + 1]};
        {push_try, Catch, {finally, Finally}} -> {1, [], [Catch, Finally]};
        {push_try, Catch, _} -> {1, [], [Catch]};
        {with_get_var, _, To} -> {0, [], [To, Pc + 1]};
        {with_get_var_this, _, To} -> {0, [], [To, Pc + 1]};
        {with_put_var, _, To} -> {0, [], [To, Pc + 1]};
        {with_delete_var, _, To} -> {0, [], [To, Pc + 1]};
        {with_make_ref, _, To} -> {0, [], [To, Pc + 1]};
        {with_get_ref_value, _, To} -> {0, [], [To, Pc + 1]};
        {with_put_ref_value, _, To} -> {0, [], [To, Pc + 1]};
        {async_yield_star_resume, To} -> {0, [], [To, Pc, Pc + 1]};
        {throw_const_assign, _} -> {0, [], []};
        {throw_reference_error, _} -> {0, [], []};
        _ -> on
    end.

shared({_, {?KEY_NAMED, _, _} = K}, Keys) -> share(K, Keys);
shared({_, {?KEY_NAMED, _, _} = K, _}, Keys) -> share(K, Keys);
shared({_, _, {?KEY_NAMED, _, _} = K}, Keys) -> share(K, Keys);
shared({_, _, _, {?KEY_NAMED, _, _} = K}, Keys) -> share(K, Keys);
shared({new_object_with, Ks, _}, Keys) -> lists:foldl(fun share/2, Keys, Ks);
shared({new_object_shaped, Ks, _}, Keys) -> lists:foldl(fun share/2, Keys, Ks);
shared(_, Keys) -> Keys.

share({?KEY_NAMED, Id, _} = K, Keys) when not ?ID_IS_EXACT(Id), not is_map_key(Id, Keys) ->
    Keys#{Id => K};
share(_, Keys) -> Keys.

key({?KEY_NAMED, Id, Text} = K, #cx{keys = Keys}) when not ?ID_IS_EXACT(Id) ->
    case Keys of
        #{Id := {_, _, Text} = Shared} -> Shared;
        _ -> K
    end;
key(K, _) -> K.

%% where a closure for Op is best handed the top of the stack
wants(Op) when is_atom(Op) ->
    case Op of
        new_object -> e;
        safepoint -> e;
        pop_try -> e;
        return -> e;
        yield -> e;
        await -> e;
        initial_yield -> e;
        compile_self -> e;
        _ -> c
    end;
wants(Op) ->
    case element(1, Op) of
        put_local -> c;
        put_local_check_init -> c;
        put_global -> c;
        get_field -> c;
        get_field_keep -> c;
        put_field -> c;
        put_field_pop -> c;
        define_field -> c;
        get_field_call -> c;
        get_field_call1 -> c;
        call -> c;
        call_method -> c;
        call_new -> c;
        call_constructor -> c;
        apply_arguments -> c;
        jump_if_false -> c;
        jump_if_true -> c;
        jump_if_nullish -> c;
        jump_if_not_nullish -> c;
        jump_if_false_or_pop -> c;
        jump_if_true_or_pop -> c;
        jump_if_not_nullish_or_pop -> c;
        jump_undefined_if_nullish -> c;
        iterate_view -> c;
        unpack_array -> c;
        put_boxed -> c;
        put_env_boxed -> c;
        bin_op -> c;
        unary_op -> c;
        cmp_jump -> c;
        cmp_const_jump -> c;
        bin_op_const -> c;
        bin_op_local -> c;
        bin_op_local_field -> c;
        bin_op_put -> c;
        bin_op_const_put -> c;
        bin_op_local_put -> c;
        _ -> e
    end.

-define(LOADED(I), (case I of 0 -> undefined; _ -> element(I, L) end)).

-define(FIRST(Q0, Q1, Q2, Q3), (element(?TABLE_FIRST, C))(T, St, F, L, A, C, Q0, Q1, Q2, Q3)).

%% a call starts the registers out undefined, as it does the locals past the
%% parameters
loader(#func_template{regs = no_regs}) -> none;
loader(#func_template{regs = {regs, P0, P1, P2, P3}} = Template) ->
    Given = given(Template),
    case {unless_unset(P0, Given), unless_unset(P1, Given), unless_unset(P2, Given),
          unless_unset(P3, Given)} of
        {0, 0, 0, 0} -> none;
        {I, 0, 0, 0} -> ?FN -> ?FIRST(element(I, L), R1, R2, R3) end;
        {0, J, 0, 0} -> ?FN -> ?FIRST(R0, element(J, L), R2, R3) end;
        {0, 0, K, 0} -> ?FN -> ?FIRST(R0, R1, element(K, L), R3) end;
        {0, 0, 0, M} -> ?FN -> ?FIRST(R0, R1, R2, element(M, L)) end;
        {I, J, K, M} -> ?FN -> ?FIRST(?LOADED(I), ?LOADED(J), ?LOADED(K), ?LOADED(M)) end
    end.

given(#func_template{lexical = {owned_lexical_slots, Base}, arity = Arity}) ->
    Base + 4 + Arity;
given(#func_template{lexical = {trimmed_lexical_slots, Base, _}, arity = Arity}) ->
    Base + 1 + Arity;
given(#func_template{env_descriptors = Captures, nested_captures = 0, arity = Arity}) ->
    length(Captures) + Arity;
given(#func_template{env_descriptors = Captures, nested_captures = Nested, arity = Arity}) ->
    length(Captures) - Nested + 1 + Arity.

unless_unset(P, Given) when P > Given -> 0;
unless_unset(P, _) -> P.

%% the closure that goes on at Pc with the stack found as S says
from(Pc, S, X) ->
    case closure_from(Pc, S, X) of
        N when ?IS_OP(N) -> N
    end.

closure_from(Pc, S, #cx{draft = true, here = Here}) when Pc =< Here -> round_again(Pc, S);
closure_from(Pc, S, #cx{pcs = Pcs} = X) ->
    case Pcs of
        #{Pc := Ix} when Ix > 0, S =:= e -> goto(Ix);
        #{Pc := Ix} when Ix > 0 -> goto_spilling(Ix);
        #{Pc := Ix} when S =:= c -> goto(-Ix);
        #{Pc := Ix} -> goto_filling(-Ix);
        _ -> at(Pc, S, X)
    end.

at(Pc, S, #cx{draft = true, here = Here, ops = Ops, template = Template} = X) when Here =/= Pc ->
    N = at(Pc, S, X#cx{here = Pc}),
    %% code with no id runs the once
    case Template#func_template.id =/= 0
         andalso (element(Pc, Ops) =:= return orelse in_tail_position(Pc, X)) of
        true -> leaving(N);
        false -> N
    end;
at(Pc, e, #cx{ops = Ops, fields = {{Pc, I, Fields}, Shape, Offs}} = X) ->
    Sources = [case From of
                   {_} -> From;
                   _ -> place(From, X)
               end || {_, From} <- Fields],
    ?PROPS:first_fields(place(I, X), [?KEY_ID(K) || {K, _} <- Fields], Sources, Shape, Offs,
                        op(element(Pc, Ops), Pc, e, X), from(Pc + length(Fields), e, X));
at(Pc, c, #cx{fields = {{Pc, _, _}, _, _}} = X) -> spill(at(Pc, e, X));
at(Pc, S, #cx{ops = Ops} = X) -> op(element(Pc, Ops), Pc, S, X).

%% a literal place in the table is quicker to read, and needs no closure made
-define(GOTO(Ix),
goto(Ix) -> ?FN -> ?GO((element(Ix, C)), T, St) end).
-define(GOTO_SPILLING(Ix),
goto_spilling(Ix) -> ?FN -> ?GO((element(Ix, C)), T, [T | St]) end).
-define(GOTO_FILLING(Ix),
goto_filling(Ix) -> fun(_, [T | St], ?ENV) -> ?GO((element(Ix, C)), T, St) end).

?GOTO(4); ?GOTO(5); ?GOTO(6); ?GOTO(7); ?GOTO(8);
?GOTO(9); ?GOTO(10); ?GOTO(11); ?GOTO(12); ?GOTO(13); ?GOTO(14);
?GOTO(15); ?GOTO(16); ?GOTO(17); ?GOTO(18); ?GOTO(19); ?GOTO(20);
?GOTO(21); ?GOTO(22); ?GOTO(23); ?GOTO(24); ?GOTO(25); ?GOTO(26);
?GOTO(27); ?GOTO(28); ?GOTO(29); ?GOTO(30); ?GOTO(31); ?GOTO(32);
?GOTO(33); ?GOTO(34); ?GOTO(35); ?GOTO(36); ?GOTO(37); ?GOTO(38);
?GOTO(Ix).

?GOTO_SPILLING(4); ?GOTO_SPILLING(5); ?GOTO_SPILLING(6);
?GOTO_SPILLING(7); ?GOTO_SPILLING(8); ?GOTO_SPILLING(9); ?GOTO_SPILLING(10);
?GOTO_SPILLING(11); ?GOTO_SPILLING(12); ?GOTO_SPILLING(13); ?GOTO_SPILLING(14);
?GOTO_SPILLING(15); ?GOTO_SPILLING(16); ?GOTO_SPILLING(17); ?GOTO_SPILLING(18);
?GOTO_SPILLING(Ix).

?GOTO_FILLING(4); ?GOTO_FILLING(5); ?GOTO_FILLING(6);
?GOTO_FILLING(7); ?GOTO_FILLING(8); ?GOTO_FILLING(9); ?GOTO_FILLING(10);
?GOTO_FILLING(Ix).

spill(N) when ?IS_OP(N) -> ?FN -> ?GO(N, T, [T | St]) end.

fill(N) when ?IS_OP(N) -> fun(_, [T | St], ?ENV) -> ?GO(N, T, St) end.

%% a local's place: a register if it has one
place(I, #cx{regs = Regs}) ->
    case Regs of
        {regs, I, _, _, _} -> ?IN_R0;
        {regs, _, I, _, _} -> ?IN_R1;
        {regs, _, _, I, _} -> ?IN_R2;
        {regs, _, _, _, I} -> ?IN_R3;
        _ -> I
    end.

store_top(?IN_R0, N) when ?IS_OP(N) -> ?FN -> N(T, St, F, L, A, C, T, R1, R2, R3) end;
store_top(?IN_R1, N) when ?IS_OP(N) -> ?FN -> N(T, St, F, L, A, C, R0, T, R2, R3) end;
store_top(?IN_R2, N) when ?IS_OP(N) -> ?FN -> N(T, St, F, L, A, C, R0, R1, T, R3) end;
store_top(?IN_R3, N) when ?IS_OP(N) -> ?FN -> N(T, St, F, L, A, C, R0, R1, R2, T) end;
store_top(D, N) -> ?FN -> put_local(T, St, ?ENV, N, D, T) end.

%% the line and mode of a call at Pc
site(Pc, #cx{lines = Lines, regs = Regs} = X) ->
    Keep = case Regs of
        no_regs -> ?KEEP_PLAIN;
        _ -> ?KEEP_REGS
    end,
    case in_tail_position(Pc, X) of
        true -> {element(Pc, Lines), Keep + ?KEEP_NOTHING};
        false -> {element(Pc, Lines), Keep}
    end.

in_tail_position(Pc, #cx{ops = Ops, template = Template}) ->
    Template#func_template.is_strict andalso Pc < tuple_size(Ops)
        andalso element(Pc + 1, Ops) =:= return
        andalso not Template#func_template.is_derived_constructor.

binop_put(Op, Form, Operand, D, Pc, X) ->
    N = from(Pc + 1, e, X),
    case ?ARITH:fuses_put(Op) of
        true -> ?ARITH:binop_put(Op, Form, Operand, place(D, X), N, Pc);
        false -> ?ARITH:binop(Op, Form, Operand, e, store_top(place(D, X), N), Pc)
    end.

%% the very term typeof gives equals itself without a look at the bytes
compared(V) when is_binary(V) -> arc_interp_ffi:type_name(V);
compared(V) -> V.

-define(ON(S), from(Pc + 1, S, X)).
-define(IS_NAMED(K), element(1, K) =:= ?KEY_NAMED).

-define(SHAPED(Shape, Slots, Offs, Stack),
        ?PROPS:new_cell(T, Stack, ?ENV, N,
                        {?SSHAPEDOBJECT_TAG, Shape,
                         ?REALM_SHARES(?REALMSHARED_OBJECT_PROTO, A#agent.realm), Slots, Offs})).
-define(ARRAY(Elements, Stack),
        ?PROPS:new_cell(T, Stack, ?ENV, N,
                        {?SOBJECT_TAG, Kind,
                         ?REALM_SHARES(?REALMSHARED_ARRAY_PROTO, A#agent.realm), #{}, [],
                         {?ELEMS_DENSE, Elements}, true})).
%% the code another closure over this template has already been given
-define(THREADED(Id, Name, Child),
        (case ?META(A)#store_meta.threaded of
             #{Id := #func_template{name = Name} = Threaded} -> Threaded;
             %% first run under a name given at run time
             #{Id := Threaded} -> Threaded#func_template{name = Name};
             _ -> Child
         end)).
%% as rt/closure makes a function whose prototype can wait
-define(CLOSURE(Birth),
        begin
            Env = case Captures of
                [] -> {};
                _ -> arc_rt_env_ffi:capture(Captures, Nested, L, From)
            end,
            #frame{unit_id = Unit, realm = Realm} = F,
            Shared = element(?REALM_SHARED, A#agent.realm),
            Kind = #bytecode_fn{template = ?THREADED(Id, Name, Child), env = Env, home_object = ?NONE,
                                flags = Flags, fields_init = ?NONE, realm = Realm,
                                unit_id = Unit, birth = Birth},
            ?PROPS:new_cell(T, St, ?ENV, N,
                            {?SOBJECT_TAG, Kind, element(?REALMSHARED_FUNCTION_PROTO, Shared),
                             #{}, [], ?ELEMS_NONE, true})
        end).

%% ops that take the stack as they find it
op({push_const, V}, Pc, S, X) ->
    N = ?ON(c),
    ?PUSHING(S, ?GO(N, V, St));
op({get_local, Slot}, Pc, S, X) ->
    I = place(Slot, X),
    N = ?ON(c),
    ?PLACED(I, S, V, case V of
                         js_tdz -> tdz(V, St, ?ENV, Pc);
                         _ -> ?GO(N, V, St)
                     end);
op(pop, Pc, c, X) -> ?ON(e);
op(pop, Pc, e, X) ->
    N = ?ON(e),
    fun(T, [_ | St], ?ENV) -> ?GO(N, T, St) end;
op({jump, To}, _, S, X) -> from(To, S, X);
op(return, Pc, S, X) ->
    Return = ?CALLS:return(X#cx.returns, Pc),
    case S of
        c -> Return;
        e ->
            ?FN ->
                case St of
                    [V | St1] -> ?GO(Return, V, St1);
                    [] -> ?GO(Return, undefined, St)
                end
            end
    end;
op({get_env, I}, Pc, S, X) ->
    N = ?ON(c),
    ?PUSHING(S, ?GO(N, element(I, element(1, L)), St));
op({get_boxed, I}, Pc, S, X) -> ?PROPS:get_boxed(I, S, ?ON(c), Pc);
op({get_env_boxed, I}, Pc, S, X) -> ?PROPS:get_env_boxed(I, S, ?ON(c), Pc);
op({box_local, I}, Pc, S, X) -> ?PROPS:box_local(I, ?ON(S));
op({get_global, K}, Pc, S, X) when ?IS_NAMED(K) ->
    ?PROPS:get_global(key(K, X), S, ?ON(c), Pc);
op({typeof_global, K}, Pc, S, X) when ?IS_NAMED(K) ->
    ?PROPS:typeof_global(key(K, X), S, ?ON(c), Pc);
op({get_local_field, I, K}, Pc, S, X) when ?IS_NAMED(K) ->
    ?PROPS:get_local_field(place(I, X), key(K, X), hint(Pc, X), S, ?ON(c), Pc);
op({get_local_field_keep, I, K}, Pc, S, X) when ?IS_NAMED(K) ->
    ?PROPS:get_local_field_keep(place(I, X), key(K, X), hint(Pc, X), S, ?ON(c), Pc);
op({get_local_field_call, I, K}, Pc, S, X) when ?IS_NAMED(K) ->
    ?PROPS:local_field_call(place(I, X), key(K, X), hint(Pc, X), S, ?ON(c), Pc + 1, site(Pc, X));
op({bin_op_local_field, Op, I, K}, Pc, c, X) when ?IS_NAMED(K) ->
    N = ?ARITH:binop(Op, stack, none, c, ?ON(c), Pc),
    ?PROPS:get_local_field(place(I, X), key(K, X), hint(Pc, X), c, N, Pc);
op({get_elem_locals, I, J}, Pc, S, X) ->
    ?PROPS:get_elem_locals(place(I, X), place(J, X), S, ?ON(c), Pc);
op({get_elem_post_inc, I, J}, Pc, S, X) ->
    ?PROPS:get_elem_post_inc(place(I, X), place(J, X), S, ?ON(c), Pc);
op({bin_op_local_local, Op, I, J}, Pc, S, X) ->
    ?ARITH:binop(Op, local_local, {place(I, X), place(J, X)}, S, ?ON(c), Pc);
op({bin_op_local_const, Op, I, V}, Pc, S, X) ->
    ?ARITH:binop(Op, local_const, {place(I, X), compared(V)}, S, ?ON(c), Pc);
op({post_inc_local, I}, Pc, S, X) -> ?ARITH:step(post, place(I, X), 1, S, ?ON(c), Pc);
op({post_dec_local, I}, Pc, S, X) -> ?ARITH:step(post, place(I, X), -1, S, ?ON(c), Pc);
op({inc_local, I}, Pc, S, X) ->
    ?ARITH:step(plain, place(I, X), 1, S, ?ON(S), ?SIGNED(Pc, S));
op({dec_local, I}, Pc, S, X) ->
    ?ARITH:step(plain, place(I, X), -1, S, ?ON(S), ?SIGNED(Pc, S));
op({inc_local_jump, I, To}, Pc, S, X) ->
    ?ARITH:step(plain, place(I, X), 1, S, from(To, S, X), ?SIGNED(Pc, S));
op({jump_if_local, I, Yes, No}, Pc, S, X) ->
    ?ARITH:branch_local(place(I, X), from(Yes, S, X), from(No, S, X), ?SIGNED(Pc, S));
op({inc_local_cmp_const_jump, I, By, V, Op, Yes, No}, Pc, S, X) ->
    ?ARITH:step_compare(Op, const, place(I, X), By, V, from(Yes, S, X), from(No, S, X),
                        ?SIGNED(Pc, S));
op({inc_local_cmp_local_jump, I, By, J, Op, Yes, No}, Pc, S, X) when I =/= J ->
    ?ARITH:step_compare(Op, local, place(I, X), By, place(J, X), from(Yes, S, X),
                        from(No, S, X), ?SIGNED(Pc, S));
op({inc_local_cmp_local_jump, I, By, I, Op, Yes, No}, Pc, S, X) ->
    Compare = op({cmp_local_local_jump, I, I, Op, Yes, No}, Pc, S, X),
    ?ARITH:step(plain, place(I, X), By, S, Compare, ?SIGNED(Pc, S));
op({cmp_local_local_jump, I, J, Op, Yes, No}, Pc, S, X) ->
    ?ARITH:compare(Op, local_local, place(I, X), place(J, X), from(Yes, S, X),
                   from(No, S, X), ?SIGNED(Pc, S));
op({cmp_local_const_jump, I, V, Op, Yes, No}, Pc, S, X) ->
    ?ARITH:compare(Op, local_const, place(I, X), compared(V), from(Yes, S, X), from(No, S, X),
                   ?SIGNED(Pc, S));
op(new_object, Pc, S, X) ->
    N = ?ON(c),
    ?PUSHING(S, ?SHAPED(?PLAIN_EMPTY_SHAPE, {}, #{}, St));
op({new_object_with, [], 0}, Pc, S, X) -> op(new_object, Pc, S, X);
op({new_object_with, Keys0, Count}, Pc, S, X) ->
    Keys = [key(K, X) || K <- Keys0],
    N = ?ON(c),
    ?PUSHING(S, begin
        {Obj, St1, A1} = arc_interp_prop_ffi:new_object(A, Keys, Count, St),
        ?GO(N, Obj, St1, A1)
    end);
op({new_object_shaped, Keys0, Count}, Pc, S, X) ->
    Keys = [key(K, X) || K <- Keys0],
    Step = 'arc@rt@obj':literal_step(Keys),
    N = ?ON(c),
    case literal_shape(X#cx.shapes, Step) of
        {Shape, Offs} -> literal(Shape, Offs, Count, S, N);
        %% a shape is there for good, so a literal that never runs gets none
        miss ->
            ?FN ->
                case literal_shape((A#agent.store)#store.shapes, Step) of
                    {Shape, Offs} -> ?GO((literal(Shape, Offs, Count, S, N)), T, St);
                    miss ->
                        {Shape, Offs, A1} = 'arc@rt@obj':literal_shape(A, Keys),
                        ?GO((literal(Shape, Offs, Count, S, N)), T, St, A1)
                end
            end
    end;
op({array_from, Count}, Pc, S, X) ->
    N = ?ON(c),
    Kind = {?ARRAYOBJ_TAG, Count},
    case {Count, S} of
        {0, _} -> ?PUSHING(S, ?ARRAY({}, St));
        {1, c} -> ?FN -> ?ARRAY({T}, St) end;
        {2, c} -> fun(T, [U | St], ?ENV) -> ?ARRAY({U, T}, St) end;
        {3, c} -> fun(T, [V, U | St], ?ENV) -> ?ARRAY({U, V, T}, St) end;
        _ when Count > 64 ->
            ?PUSHING(S, begin
                {Items, St1} = popped(Count, St, []),
                {Array, A1} = 'arc@rt@obj':new_array(A, Items),
                ?GO(N, Array, St1, A1)
            end);
        _ ->
            ?PUSHING(S, begin
                {Items, St1} = popped(Count, St, []),
                ?ARRAY(arc_tree_array_ffi:from_list(Items), St1)
            end)
    end;
op({create_rest_array, From}, Pc, S, X) ->
    N = ?ON(c),
    ?PUSHING(S, begin
        {Array, A1} = 'arc@rt@obj':new_array(A, dropped(From, F#frame.call_args)),
        ?GO(N, Array, St, A1)
    end);
op({create_arguments, Simple}, Pc, S, X) ->
    N = ?ON(c),
    ?PUSHING(S, begin
        State = #state{agent = A, pc = Pc, stack = St, locals = L, frame = F},
        {Arguments, A1} = 'arc@interp@call':arguments_object(State, Simple),
        ?GO(N, Arguments, St, A1)
    end);
op({make_closure, Ix}, Pc, S, #cx{template = Parent} = X) ->
    #func_template{id = Id, name = Name, env_descriptors = Slots, nested_captures = Nested} = Child =
        element(Ix + 1, Parent#func_template.functions),
    Captures = 'arc@rt@bytecode':capture_slots(Parent, Slots),
    From = Parent#func_template.nested_captures,
    Flags = 'arc@rt@closure':flags_of(Child),
    N = ?ON(c),
    case Child of
        #func_template{is_generator = false, is_async = false, is_constructor = false} ->
            ?PUSHING(S, ?CLOSURE({birth_pending, ?NONE}));
        #func_template{is_generator = false, is_async = false, is_class_constructor = false} ->
            ?PUSHING(S, ?CLOSURE(element(?REALMSHARED_CONSTRUCTOR_BIRTH, Shared)));
        _ ->
            ?PUSHING(S, begin
                Env = arc_rt_env_ffi:capture(Captures, Nested, L, From),
                {Fn, A1} = 'arc@rt@closure':new_bytecode_function(
                               A, ?THREADED(Id, Name, Child), Env, F#frame.unit_id),
                ?GO(N, Fn, St, A1)
            end)
    end;
op({make_lazy_closure, Ix, Record}, Pc, S, X) ->
    N = ?ON(c),
    ?PUSHING(S, begin
        {Fn, A1} = 'arc@interp@lazy':make_closure(A, F#frame.func, F#frame.unit_id, L, Ix,
                                                  Record),
        ?GO(N, Fn, St, A1)
    end);
op({gosub, To}, Pc, S, X) ->
    N = from(To, c, X),
    Back = Pc + 1,
    ?PUSHING(S, ?GO(N, Back, St));
op(pop_try, Pc, S, X) ->
    N = ?ON(S),
    ?FN ->
        [_ | Tries] = F#frame.try_stack,
        N(T, St, F#frame{try_stack = Tries}, L, A, C, ?RS)
    end;
op(yield, Pc, S, _) -> ?PUSHING(S, park(yield, St, ?ENV, Pc));
op(await, Pc, S, _) -> ?PUSHING(S, park(await, St, ?ENV, Pc));
op(initial_yield, Pc, S, _) -> ?PUSHING(S, park(yield, [undefined | St], ?ENV, Pc));
op({push_try, Catch, Kind}, Pc, S, X) ->
    N = ?ON(e),
    ?PUSHING(S, begin
        Tries = [{try_frame, Catch, length(St), Kind} | F#frame.try_stack],
        N(T, St, F#frame{try_stack = Tries}, L, A, C, ?RS)
    end);
op(safepoint, Pc, S, X) ->
    N = ?ON(e),
    ?PUSHING(S, case A of
                    #agent{next_id = Id, gc_at = Due}
                      when Id >= Due, F#frame.outer_depth =:= 0 ->
                        ?GO(N, T, St, collected(St, ?ENV, Pc));
                    _ -> ?GO(N, T, St)
                end);
%% ops built with nothing in T
op({bin_op_local_local_put, Op, I, J, D}, Pc, e, X) ->
    binop_put(Op, local_local, {place(I, X), place(J, X)}, D, Pc, X);
op({put_local_local_field, I, J, K}, Pc, e, X) when ?IS_NAMED(K) ->
    ?PROPS:put_local_field(local, place(I, X), place(J, X), key(K, X), put_hint(Pc, X),
                           ?ON(e), Pc);
op({put_local_const_field, I, V, K}, Pc, e, X) when ?IS_NAMED(K) ->
    ?PROPS:put_local_field(const, place(I, X), V, key(K, X), put_hint(Pc, X), ?ON(e), Pc);
op({Tag, _, _, _} = Op, Pc, c, X)
  when Tag =:= put_local_local_field; Tag =:= put_local_const_field ->
    spill(op(Op, Pc, e, X));
op({bin_op_local_local_put, _, _, _, _} = Op, Pc, c, X) -> spill(op(Op, Pc, e, X));
op(Op, Pc, S, X) ->
    case {wants(Op), S} of
        {c, c} -> taking(Op, Pc, X);
        {c, e} -> fill(taking(Op, Pc, X));
        {e, _} -> general(Op, Pc, S, X)
    end.

%% ops built with the top of the stack in T
taking({put_local, I}, Pc, X) -> store_top(place(I, X), ?ON(e));
taking(dup, Pc, X) -> spill(?ON(c));
taking(swap, Pc, X) ->
    N = ?ON(c),
    fun(T, [U | St], ?ENV) -> ?GO(N, U, [T | St]) end;
taking(rot3, Pc, X) ->
    N = ?ON(c),
    fun(T, [U, V | St], ?ENV) -> ?GO(N, V, [T, U | St]) end;
taking(unrot4, Pc, X) ->
    N = ?ON(c),
    fun(T, [U, V, W | St], ?ENV) -> ?GO(N, U, [V, W, T | St]) end;
taking({put_boxed, I}, Pc, X) -> ?PROPS:put_boxed(I, ?ON(e), Pc);
taking({put_env_boxed, I}, Pc, X) -> ?PROPS:put_env_boxed(I, ?ON(e), Pc);
taking({put_global, K}, Pc, X) when ?IS_NAMED(K) ->
    ?PROPS:put_global(key(K, X), (X#cx.template)#func_template.is_strict, ?ON(e), Pc);
taking({get_field, K}, Pc, X) when ?IS_NAMED(K) ->
    ?PROPS:get_field(key(K, X), hint(Pc, X), ?ON(c), Pc);
taking({get_field_keep, K}, Pc, X) when ?IS_NAMED(K) ->
    ?PROPS:get_field_keep(key(K, X), hint(Pc, X), ?ON(c), Pc);
taking({put_field, K}, Pc, X) when ?IS_NAMED(K) ->
    ?PROPS:put_field(key(K, X), put_hint(Pc, X), ?ON(c), Pc);
taking({put_field_pop, K}, Pc, X) when ?IS_NAMED(K) ->
    ?PROPS:put_field_pop(key(K, X), put_hint(Pc, X), ?ON(e), Pc);
taking({get_field_call, K}, Pc, X) when ?IS_NAMED(K) ->
    ?PROPS:field_call(key(K, X), hint(Pc, X), ?ON(c), Pc + 1, site(Pc, X));
taking({get_field_call1, K, J}, Pc, X) when ?IS_NAMED(K) ->
    ?PROPS:field_call1(key(K, X), hint(Pc, X), place(J, X), ?ON(c), Pc + 1, site(Pc, X));
taking(get_elem, Pc, X) -> ?PROPS:get_elem(?ON(c), Pc);
taking(get_elem_keep, Pc, X) -> ?PROPS:get_elem_keep(?ON(c), Pc);
taking(put_elem, Pc, X) -> ?PROPS:put_elem(?ON(c), Pc);
taking(put_elem_pop, Pc, X) -> ?PROPS:put_elem_pop(?ON(e), Pc);
taking({call, Arity}, Pc, X) -> ?CALLS:call(Arity, ?ON(c), Pc + 1, site(Pc, X));
taking({call_method, Arity}, Pc, X) -> ?CALLS:call_method(Arity, ?ON(c), Pc + 1, site(Pc, X));
taking({call_new, Arity}, Pc, X) -> ?CALLS:call_new(Arity, ?ON(c), Pc + 1, site(Pc, X));
taking({call_constructor, Arity}, Pc, X) ->
    ?CALLS:call_constructor(Arity, ?ON(c), Pc + 1, site(Pc, X));
taking({apply_arguments, Slot, _}, Pc, X) ->
    ?CALLS:apply_arguments(Slot, ?ON(c), Pc + 1, site(Pc, X));
taking({bin_op, Op}, Pc, X) -> ?ARITH:binop(Op, stack, none, c, ?ON(c), Pc);
taking({bin_op_const, Op, V}, Pc, X) ->
    ?ARITH:binop(Op, const, compared(V), c, ?ON(c), Pc);
taking({bin_op_local, Op, J}, Pc, X) -> ?ARITH:binop(Op, local, place(J, X), c, ?ON(c), Pc);
taking({bin_op_put, Op, D}, Pc, X) -> binop_put(Op, stack, none, D, Pc, X);
taking({bin_op_const_put, Op, V, D}, Pc, X) -> binop_put(Op, const, V, D, Pc, X);
taking({bin_op_local_put, Op, J, D}, Pc, X) -> binop_put(Op, local, place(J, X), D, Pc, X);
taking({bin_op_local_field, _, _, K} = Op, Pc, X) when ?IS_NAMED(K) -> op(Op, Pc, c, X);
taking({unary_op, Kind}, Pc, X) -> ?ARITH:unary(Kind, ?ON(c), Pc);
taking({cmp_jump, Op, Yes, No}, Pc, X) ->
    ?ARITH:compare(Op, stack, none, none, from(Yes, e, X), from(No, e, X), Pc);
taking({cmp_const_jump, V, Op, Yes, No}, Pc, X) ->
    ?ARITH:compare(Op, const, none, compared(V), from(Yes, e, X), from(No, e, X), Pc);
taking({jump_if_false, To}, Pc, X) -> ?ARITH:branch(?ON(e), from(To, e, X));
taking({jump_if_true, To}, Pc, X) -> ?ARITH:branch(from(To, e, X), ?ON(e));
taking({jump_if_false_or_pop, To}, Pc, X) -> ?ARITH:branch(?ON(e), from(To, c, X));
taking({jump_if_true_or_pop, To}, Pc, X) -> ?ARITH:branch(from(To, c, X), ?ON(e));
taking({jump_if_nullish, To}, Pc, X) -> on_nullish(from(To, e, X), ?ON(e));
taking({jump_if_not_nullish, To}, Pc, X) -> on_nullish(?ON(e), from(To, e, X));
taking({jump_if_not_nullish_or_pop, To}, Pc, X) -> on_nullish(?ON(e), from(To, c, X));
taking({jump_undefined_if_nullish, To}, Pc, X) ->
    Yes = from(To, c, X),
    No = ?ON(c),
    ?FN ->
        if ?IS_NULLISH(T) -> ?GO(Yes, undefined, St);
           true -> ?GO(No, T, St)
        end
    end;
taking({iterate_view, View, To}, Pc, X) ->
    Hit = from(To, c, X),
    N = ?ON(c),
    ?FN ->
        case arc_rt_lang_ffi:view_iter_start(A, T, View) of
            miss -> ?GO(N, T, St);
            Record -> ?GO(Hit, Record, St)
        end
    end;
taking({unpack_array, Count, To}, Pc, X) ->
    Missed = from(To, c, X),
    N = ?ON(e),
    ?FN ->
        case arc_rt_lang_ffi:unpack_array(A, T, Count, St) of
            miss -> ?GO(Missed, T, St);
            St1 -> ?GO(N, T, St1)
        end
    end;
taking(ret, Pc, _) ->
    ?FN ->
        if is_integer(T), T > 0 -> resume(T, St, ?ENV);
           true -> via(ret, [T | St], ?ENV, none, Pc, e)
        end
    end;
taking(to_object, Pc, X) ->
    N = ?ON(c),
    ?FN ->
        case T of
            {?HANDLE_TAG, _} -> ?GO(N, T, St);
            _ -> via(to_object, [T | St], ?ENV, N, Pc, c)
        end
    end;
taking(to_string_val, Pc, X) ->
    N = ?ON(c),
    ?FN ->
        if ?IS_STR(T) -> ?GO(N, T, St);
           is_integer(T) -> ?GO(N, integer_to_binary(T), St);
           true -> via(to_string_val, [T | St], ?ENV, N, Pc, c)
        end
    end;
taking(type_of, Pc, #cx{ops = Ops, pcs = Pcs} = X) ->
    case element(Pc + 1, Ops) of
        {cmp_const_jump, Name, Op, Yes, No}
          when is_binary(Name), not is_map_key(Pc + 1, Pcs),
               (Op =:= strict_eq orelse Op =:= loose_eq) ->
            ?ARITH:of_type(Name, from(Yes, e, X), from(No, e, X), Pc);
        {cmp_const_jump, Name, Op, Yes, No}
          when is_binary(Name), not is_map_key(Pc + 1, Pcs),
               (Op =:= strict_not_eq orelse Op =:= loose_not_eq) ->
            ?ARITH:of_type(Name, from(No, e, X), from(Yes, e, X), Pc);
        _ ->
            N = ?ON(c),
            ?FN ->
                case arc_interp_ffi:type_of(A, T) of
                    miss -> via(type_of, [T | St], ?ENV, N, Pc, c);
                    Type -> ?GO(N, Type, St)
                end
            end
    end;
taking(get_prototype_of, Pc, X) ->
    N = ?ON(c),
    ?FN ->
        case arc_interp_ffi:cell_of(A, T) of
            {?SOBJECT_TAG, _, {?SOME, Proto}, _, _, _, _} -> ?GO(N, Proto, St);
            {?SSHAPEDOBJECT_TAG, _, {?SOME, Proto}, _, _} -> ?GO(N, Proto, St);
            _ -> via(get_prototype_of, [T | St], ?ENV, N, Pc, c)
        end
    end;
%% only a getter would need this
taking(get_super_value, Pc, X) ->
    N = ?ON(c),
    ?FN ->
        case St of
            [{?HANDLE_TAG, _} = Base, _ | St1] when is_binary(T) ->
                case arc_interp_prop_ffi:get_elem(A, Base, T) of
                    miss -> via(get_super_value, [T | St], ?ENV, N, Pc, c);
                    V -> ?GO(N, V, St1)
                end;
            _ -> via(get_super_value, [T | St], ?ENV, N, Pc, c)
        end
    end;
taking({put_local_check_init, I} = Op, Pc, X) ->
    N = ?ON(e),
    ?FN ->
        case element(I, L) of
            js_tdz -> put_local(T, St, ?ENV, N, I, T);
            _ -> via(Op, [T | St], ?ENV, N, Pc, e)
        end
    end;
taking({define_field, K0}, Pc, X) when ?IS_NAMED(K0) ->
    K = key(K0, X),
    N = ?ON(c),
    ?FN ->
        [Obj | St1] = St,
        case arc_interp_prop_ffi:define_field(A, Obj, K, T) of
            miss -> via({define_field, K}, [T | St], ?ENV, N, Pc, c);
            A1 -> ?GO(N, Obj, St1, A1)
        end
    end;
taking(get_private_field_dyn, Pc, X) ->
    N = ?ON(c),
    ?FN ->
        [Obj | St1] = St,
        case arc_interp_prop_ffi:private_get(A, Obj, T) of
            miss -> via(get_private_field_dyn, [T | St], ?ENV, N, Pc, c);
            V -> ?GO(N, V, St1)
        end
    end;
taking(get_private_field_dyn_keep, Pc, X) ->
    N = ?ON(c),
    ?FN ->
        case arc_interp_prop_ffi:private_get(A, hd(St), T) of
            miss -> via(get_private_field_dyn_keep, [T | St], ?ENV, N, Pc, c);
            V -> ?GO(N, V, St)
        end
    end;
taking(put_private_field_dyn, Pc, X) ->
    N = ?ON(c),
    ?FN ->
        [V, Obj | St1] = St,
        case arc_interp_prop_ffi:private_put(A, Obj, T, V) of
            miss -> via(put_private_field_dyn, [T | St], ?ENV, N, Pc, c);
            A1 -> ?GO(N, V, St1, A1)
        end
    end;
taking(define_private_field, Pc, X) ->
    N = ?ON(c),
    ?FN ->
        [Name, Obj | St1] = St,
        case arc_interp_prop_ffi:private_define(A, Obj, Name, T) of
            miss -> via(define_private_field, [T | St], ?ENV, N, Pc, c);
            A1 -> ?GO(N, Obj, St1, A1)
        end
    end;
taking(get_iterator, Pc, X) ->
    N = ?ON(c),
    ?FN ->
        case arc_rt_lang_ffi:array_iter_start(A, T) of
            miss -> via(get_iterator, [T | St], ?ENV, N, Pc, c);
            Record -> ?GO(N, Record, St)
        end
    end;
taking(iterator_close, Pc, X) ->
    N = ?ON(e),
    ?FN ->
        case T =:= undefined orelse closes_quietly(A, T) of
            true -> ?GO(N, T, St);
            false -> via(iterator_close, [T | St], ?ENV, N, Pc, e)
        end
    end;
taking(for_in_next, Pc, X) ->
    {More, Done} = after_next(Pc, X),
    ?FN ->
        case T of
            {for_in, [Key | Keys]} -> ?GO(More, Key, [{for_in, Keys} | St]);
            _ -> ?GO(Done, undefined, [T | St])
        end
    end;
taking(iterator_next, Pc, X) ->
    {More, Done} = after_next(Pc, X),
    ?FN -> iterator_next(T, St, ?ENV, More, Done, Pc) end;
taking(Op, Pc, X) -> general(Op, Pc, c, X).

on_nullish(Yes, No) when ?IS_OP(Yes), ?IS_OP(No) ->
    ?FN ->
        if ?IS_NULLISH(T) -> ?GO(Yes, T, St);
           true -> ?GO(No, T, St)
        end
    end.

%% an op that steps an iteration pushes a value, then whether it was done,
%% which the next op nearly always branches on. These take just the value
after_next(Pc, #cx{ops = Ops, pcs = Pcs} = X) ->
    case element(Pc + 1, Ops) of
        {jump_if_true, To} when not is_map_key(Pc + 1, Pcs) ->
            {from(Pc + 2, c, X), from(To, c, X)};
        {jump_if_false, To} when not is_map_key(Pc + 1, Pcs) ->
            {from(To, c, X), from(Pc + 2, c, X)};
        %% a head, so that holding it twice holds no code twice
        _ ->
            N = ?ON(c),
            {?FN -> ?GO(N, false, [T | St]) end, ?FN -> ?GO(N, true, [T | St]) end}
    end.

iterator_next(undefined, St, ?ENV, _, Done, _) -> ?GO(Done, undefined, [undefined | St]);
iterator_next({?ARC_ITER, _, _, _, _} = Record, St, ?ENV, More, Done, Pc) ->
    case arc_rt_lang_ffi:array_iter_next(A, Record) of
        {iter_step, false, V, Record1} -> ?GO(More, V, [Record1 | St]);
        {iter_step, true, V, Record1} -> ?GO(Done, V, [Record1 | St]);
        {iter_pair, Key, V, Record1} ->
            {Pair, A1} = 'arc@rt@obj':new_array(A, [Key, V]),
            ?GO(More, Pair, [Record1 | St], A1);
        iter_miss -> via(iterator_next, [Record | St], ?ENV, on_done(More, Done), Pc, c)
    end;
iterator_next(Record, St, ?ENV, More, Done, Pc) ->
    case arc_rt_lang_ffi:iter_step(A, Record) of
        {advanced, false, V, A1} -> ?GO(More, V, [Record | St], A1);
        {advanced, true, V, A1} -> ?GO(Done, V, [undefined | St], A1);
        {pair_advanced, Key, V, A1} ->
            {Pair, A2} = 'arc@rt@obj':new_array(A1, [Key, V]),
            ?GO(More, Pair, [Record | St], A2);
        {resume_generator, Generator} = Plan ->
            case resume_generator(A, F, Pc, Generator, undefined, 0) of
                {false, V, A1} -> ?GO(More, V, [Record | St], A1);
                {true, V, A1} -> ?GO(Done, V, [undefined | St], A1);
                {threw, Thrown, A1} -> threw(Thrown, [undefined | St], F, L, A1, C, ?RS, Pc);
                miss -> next_by(Plan, Record, St, ?ENV, More, Done, Pc);
                Failed -> Failed
            end;
        Plan -> next_by(Plan, Record, St, ?ENV, More, Done, Pc)
    end.

next_by(Plan, Record, St, ?ENV, More, Done, Pc) ->
    State = state(Pc, [Record | St], F, L, A),
    case 'arc@interp@interpreter':iterator_next_general(State, Record, St, Plan) of
        {ok, #state{agent = A1, stack = [false, V | St1]}} -> ?GO(More, V, St1, A1);
        {ok, #state{agent = A1, stack = [true, V | St1]}} -> ?GO(Done, V, St1, A1);
        Exit -> stepped(Exit, F, C, ?RS, none, Pc, c)
    end.

%% §27.5.3.3 for the op at Pc of frame F, as {Done, value, agent} or {threw,
%% value, agent}, or the error the run ends with; misses unless the body can
%% just go on from where it parked. Between is how many builtin calls deep the
%% resumption would have been made from
resume_generator(#agent{cells = Cells, call_depth = Synced, frames = Frames} = A, F, Pc,
                 {?HANDLE_TAG, H} = Generator, Sent, Between) ->
    #frame{func = #func_template{lines = Lines}, depth = Depth, realm = Realm} = F,
    case arc_rt_arena_ffi:get(H, Cells) of
        {s_generator, Status,
         {resume_frame,
          #suspended_frame{template = #func_template{code = ?CODE(_, _, _, C)} = Template,
                           eval_env = ?NONE, parked = Parked, realm = Realm} = Body} = Resume}
          when (Status =:= gen_suspended_yield orelse Status =:= gen_suspended_start),
               (Parked =:= parked_op orelse Parked =:= parked_start),
               Depth + Between < ?MAX_CALL_DEPTH ->
            From = Depth + Between,
            #suspended_frame{pc = At, locals = L, stack = St, try_stack = Tries,
                             call_args = Args, unit_id = Unit} = Body,
            A1 = A#agent{cells = arc_rt_arena_ffi:set(H, {s_generator, gen_executing, Resume},
                                                      Cells),
                         call_depth = From,
                         frames = [{unlisted, F, element(Pc, Lines), Depth - Synced} | Frames]},
            F1 = #frame{func = Template, unit_id = Unit, return_to = outermost,
                        outer_depth = From + 1, depth = From + 1, try_stack = Tries,
                        call_args = Args, eval_env = ?NONE, realm = Realm},
            {R0, R1, R2, R3} = load(Template#func_template.regs, L),
            St1 = case Parked of
                parked_op -> [Sent | St];
                parked_start -> St
            end,
            try resume(At, St1, F1, L, A1, C, ?RS) of
                {ok, {{suspended, yield, V},
                      #state{agent = A2, pc = At1, stack = St2, locals = L1,
                             frame = #frame{try_stack = Tries1, eval_env = ?NONE}}}} ->
                    Body1 = Body#suspended_frame{pc = At1, locals = L1, stack = St2,
                                                 try_stack = Tries1, parked = parked_op},
                    Cell = {s_generator, gen_suspended_yield, {resume_frame, Body1}},
                    {false, V, A2#agent{cells = arc_rt_arena_ffi:set(H, Cell, ?CELLS(A2)),
                                        call_depth = Synced, frames = Frames}};
                Outcome ->
                    generator_stopped(A, F, Pc, Generator, Resume, {value, Outcome, A1})
            catch
                error:?JS_THROW(A2, Thrown) ->
                    generator_stopped(A, F, Pc, Generator, Resume, {thrown, Thrown, A2})
            end;
        _ -> miss
    end.

generator_stopped(#agent{call_depth = Depth, frames = Frames} = A, F, Pc, Generator, Resume,
                  Outcome) ->
    State = #state{agent = A, pc = Pc, stack = [], locals = {}, frame = F},
    case 'arc@interp@interpreter':generator_stopped(State, Generator, Resume, Depth, Frames,
                                                    Outcome) of
        {ok, {{Done, V}, #state{agent = A1}}} -> {Done, V, A1};
        {error, {threw, Thrown, #state{agent = A1}}} -> {threw, Thrown, A1};
        {error, {vm_failed, Error, _}} -> {error, Error}
    end.

on_done(More, Done) ->
    fun(false, [V | St], ?ENV) -> ?GO(More, V, St);
       (true, [V | St], ?ENV) -> ?GO(Done, V, St)
    end.

closes_quietly(A, {?ARC_ITER, _, _, _, _} = Record) ->
    Proto = arc_rt_lang_ffi:array_iter_proto(A, Record),
    arc_interp_prop_ffi:get_field(A, Proto, ?RETURN_KEY) =:= undefined;
closes_quietly(_, _) -> false.

popped(0, St, Items) -> {Items, St};
popped(Count, [V | St], Items) -> popped(Count - 1, St, [V | Items]).

dropped(0, List) -> List;
dropped(_, []) -> [];
dropped(Count, [_ | List]) -> dropped(Count - 1, List).

literal_shape(Shapes, Step) -> ?SHAPED_NEXT(Shapes, 0, Step).

literal(Shape, Offs, Count, S, N) when ?IS_OP(N) ->
    case {Count, S} of
        {1, c} -> ?FN -> ?SHAPED(Shape, {T}, Offs, St) end;
        {2, c} -> fun(T, [U | St], ?ENV) -> ?SHAPED(Shape, {U, T}, Offs, St) end;
        {3, c} -> fun(T, [V, U | St], ?ENV) -> ?SHAPED(Shape, {U, V, T}, Offs, St) end;
        {4, c} -> fun(T, [W, V, U | St], ?ENV) -> ?SHAPED(Shape, {U, V, W, T}, Offs, St) end;
        _ ->
            ?PUSHING(S, begin
                {Values, St1} = popped(Count, St, []),
                ?SHAPED(Shape, list_to_tuple(Values), Offs, St1)
            end)
    end.

park(Kind, St, ?ENV, Pc) ->
    {V, St1} = case St of
        [Top | Rest] -> {Top, Rest};
        [] -> {undefined, []}
    end,
    {ok, {{suspended, Kind, V},
          #state{agent = A, pc = Pc + 1, stack = St1, locals = flushed(F, L, ?RS), frame = F}}}.

%% the agent after a collection at Pc, with St all that the frame still wants
collected(St, ?ENV, Pc) ->
    State = state(Pc, St, F, flushed(F, L, ?RS), A),
    ('arc@interp@safepoint':maybe_collect_at_return(State))#state.agent.

%% whatever is left goes by the general path
general(Op, Pc, S, #cx{ops = Ops} = X) ->
    case edges(Op, Pc) of
        on ->
            Out = wants(element(Pc + 1, Ops)),
            N = ?ON(Out),
            case reads_locals(Op) of
                true -> ?PUSHING(S, flushing(Op, St, ?ENV, N, Pc, Out));
                false -> ?PUSHING(S, via(Op, St, ?ENV, N, Pc, Out))
            end;
        _ -> ?PUSHING(S, via_jumping(Op, St, ?ENV, Pc))
    end.

reads_locals(Op) -> 'arc@bytecode@opcode':slot_uses(Op) =/= [].
