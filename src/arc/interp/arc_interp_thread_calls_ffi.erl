%% threaded code for calls and returns
-module(arc_interp_thread_calls_ffi).
-export([call/4, call_method/4, call_new/4, call_constructor/4, apply_arguments/4,
         return/2, entry/1, draft_entry/1]).
-export([call/16, call_at/14, prepare/4]).

-include("arc_interp_thread.hrl").

-define(SITE, N, Pc1, Line, Mode).

call(0, N, Pc1, {Line, Mode}) ->
    ?FN -> call(T, St, ?ENV, ?SITE, [], undefined) end;
call(1, N, Pc1, {Line, Mode}) ->
    ?FN ->
        [Fn | St1] = St,
        call(Fn, St1, ?ENV, ?SITE, [T], undefined)
    end;
call(2, N, Pc1, {Line, Mode}) ->
    ?FN ->
        [X, Fn | St1] = St,
        call(Fn, St1, ?ENV, ?SITE, [X, T], undefined)
    end;
call(3, N, Pc1, {Line, Mode}) ->
    ?FN ->
        [Y, X, Fn | St1] = St,
        call(Fn, St1, ?ENV, ?SITE, [X, Y, T], undefined)
    end;
call(Arity, N, Pc1, {Line, Mode}) ->
    ?FN -> call_popping(T, St, ?ENV, ?SITE, [T], Arity - 1) end.

call_popping(T, [Fn | St], ?ENV, ?SITE, Args, 0) ->
    call(Fn, St, ?ENV, ?SITE, Args, undefined);
call_popping(T, [X | St], ?ENV, ?SITE, Args, Left) ->
    call_popping(T, St, ?ENV, ?SITE, [X | Args], Left - 1).

call_method(0, N, Pc1, {Line, Mode}) ->
    ?FN ->
        [This | St1] = St,
        call(T, St1, ?ENV, ?SITE, [], This)
    end;
call_method(1, N, Pc1, {Line, Mode}) ->
    ?FN ->
        [Fn, This | St1] = St,
        call(Fn, St1, ?ENV, ?SITE, [T], This)
    end;
call_method(2, N, Pc1, {Line, Mode}) ->
    ?FN ->
        [X, Fn, This | St1] = St,
        call(Fn, St1, ?ENV, ?SITE, [X, T], This)
    end;
call_method(3, N, Pc1, {Line, Mode}) ->
    ?FN ->
        [Y, X, Fn, This | St1] = St,
        call(Fn, St1, ?ENV, ?SITE, [X, Y, T], This)
    end;
call_method(Arity, N, Pc1, {Line, Mode}) ->
    ?FN -> method_popping(T, St, ?ENV, ?SITE, [T], Arity - 1) end.

method_popping(T, [Fn, This | St], ?ENV, ?SITE, Args, 0) ->
    call(Fn, St, ?ENV, ?SITE, Args, This);
method_popping(T, [X | St], ?ENV, ?SITE, Args, Left) ->
    method_popping(T, St, ?ENV, ?SITE, [X | Args], Left - 1).

%% f.apply(this, arguments) while nothing has needed the arguments object
apply_arguments(Slot, N, Pc1, {Line, Mode}) ->
    ?FN ->
        [Apply, Fn | St1] = St,
        case element(Slot, L) of
            {?HANDLE_TAG, _} -> ?CORE:via_op([T | St], ?ENV, N, Pc1 - 1, c);
            _ ->
                case arc_interp_ffi:cell_of(A, Apply) of
                    {?SOBJECT_TAG, {?NATIVEFN_TAG, {function_n, function_apply}, _, _, _},
                     _, _, _, _, _} ->
                        call(Fn, St1, ?ENV, ?SITE, F#frame.call_args, T);
                    _ -> ?CORE:via_op([T | St], ?ENV, N, Pc1 - 1, c)
                end
        end
    end.

%% a call some other op makes, such as to a getter
call_at(Fn, St, ?ENV, N, Pc, Args, This) ->
    #func_template{lines = Lines, regs = Regs} = F#frame.func,
    Mode = case Regs of
        no_regs -> ?KEEP_PLAIN;
        _ -> ?KEEP_REGS
    end,
    call(Fn, St, ?ENV, N, Pc + 1, element(Pc, Lines), Mode, Args, This).

-define(CALLEE_FRAME(ReturnTo, Depth),
        #frame{func = Template, unit_id = Unit, return_to = ReturnTo, outer_depth = Outer,
               depth = Depth, try_stack = [], call_args = Args, eval_env = ?NONE,
               realm = Realm}).
%% §10.2.1.2 what the callee gets as this, or miss
-define(BOUND_THIS(Takes, This),
        (case Takes of
             as_given -> This;
             coerced ->
                 case This of
                     {?HANDLE_TAG, _} -> This;
                     undefined -> element(?REALM_GLOBAL_OBJECT, A#agent.realm);
                     null -> element(?REALM_GLOBAL_OBJECT, A#agent.realm);
                     _ -> miss
                 end;
             special -> miss
         end)).
-define(SAVED(CtorThis),
        (case Mode band ?KEEP_REGS of
             ?KEEP_PLAIN ->
                 #saved_thread{caller = F, pc = Pc1, stack = St, locals = L,
                               constructor_this = CtorThis, next = N, code = C};
             _ ->
                 #saved_regs{caller = F, pc = Pc1, stack = St, locals = L,
                             constructor_this = CtorThis, next = N, code = C,
                             r0 = R0, r1 = R1, r2 = R2, r3 = R3}
         end)).

%% §10.2.1 call, in the caller's own run when the callee is bytecode of its realm
call(Fn, St, ?ENV, ?SITE, Args, This) ->
    case Fn of
        {?HANDLE_TAG, H} ->
            case arena_get(H, ?CELLS(A)) of
                {?SOBJECT_TAG, #bytecode_fn{} = Kind, _, _, _, _, _} ->
                    call_bytecode(Fn, St, ?ENV, ?SITE, Args, This, Kind);
                {?SOBJECT_TAG, {?NATIVEFN_TAG, Token, _, _, _}, _, _, _, _, _} ->
                    call_native(Fn, St, ?ENV, ?SITE, Args, This, Token);
                {?SOBJECT_TAG, {?BOUNDFN_TAG, Target, BoundThis, BoundArgs, _, _, _},
                 _, _, _, _, _} ->
                    call(Target, St, ?ENV, ?SITE, BoundArgs ++ Args, BoundThis);
                _ -> call_general(Fn, St, ?ENV, ?SITE, Args, This)
            end;
        _ -> call_general(Fn, St, ?ENV, ?SITE, Args, This)
    end.

call_bytecode(Fn, St, ?ENV, ?SITE, Args, This,
              #bytecode_fn{template = Template, env = Env, home_object = Home,
                           realm = Realm, unit_id = Unit}) ->
    case F of
        #frame{outer_depth = Outer, depth = Depth, realm = Realm}
          when is_integer(Depth), 0 =< Depth, Depth < ?MAX_CALL_DEPTH ->
            case Template#func_template.code of
                ?CODE(Takes, Entry, Start, Table) ->
                    case ?BOUND_THIS(Takes, This) of
                        miss -> call_general(Fn, St, ?ENV, ?SITE, Args, This);
                        This1 when Mode =:= ?KEEP_PLAIN ->
                            Saved = #saved_thread{caller = F, pc = Pc1, stack = St, locals = L,
                                                  constructor_this = ?NONE, next = N, code = C},
                            Entry(This1, Args, ?CALLEE_FRAME(Saved, Depth + 1), Env, A, Table,
                                  Fn, Home, undefined, Start);
                        This1 when Mode =:= ?KEEP_REGS ->
                            Saved = #saved_regs{caller = F, pc = Pc1, stack = St, locals = L,
                                                constructor_this = ?NONE, next = N, code = C,
                                                r0 = R0, r1 = R1, r2 = R2, r3 = R3},
                            Entry(This1, Args, ?CALLEE_FRAME(Saved, Depth + 1), Env, A, Table,
                                  Fn, Home, undefined, Start);
                        This1 ->
                            case F of
                                #frame{try_stack = [], return_to = Saved}
                                  when element(#saved_frame.constructor_this, Saved) =:= ?NONE,
                                       element(1, Saved) =/= saved_cont ->
                                    Entry(This1, Args, ?CALLEE_FRAME(Saved, Depth), Env,
                                          leave(A, Depth), Table, Fn, Home, undefined, Start);
                                _ ->
                                    call(Fn, St, ?ENV, N, Pc1, Line, Mode - ?KEEP_NOTHING,
                                         Args, This)
                            end
                    end;
                {} ->
                    {_, A1} = 'arc@interp@thread':first_call(A, Fn, Template),
                    call(Fn, St, F, L, A1, C, ?RS, ?SITE, Args, This)
            end;
        _ -> call_general(Fn, St, ?ENV, ?SITE, Args, This)
    end.

%% §20.2.3.3 function.prototype.call
call_native(_, St, ?ENV, ?SITE, Args, This, {function_n, function_call}) ->
    case Args of
        [This1 | Args1] -> call(This, St, ?ENV, ?SITE, Args1, This1);
        [] -> call(This, St, ?ENV, ?SITE, [], undefined)
    end;
%% §20.2.3.1 function.prototype.apply
call_native(Fn, St, ?ENV, ?SITE, Args, This, {function_n, function_apply}) ->
    case Args of
        [This1, List | _] ->
            case arc_interp_ffi:list_from_array_like(A, List) of
                miss -> call_general(Fn, St, ?ENV, ?SITE, Args, This);
                Args1 -> call(This, St, ?ENV, ?SITE, Args1, This1)
            end;
        _ -> call_general(Fn, St, ?ENV, ?SITE, Args, This)
    end;
call_native(Fn, St, ?ENV, ?SITE, Args, This, {reflect_n, reflect_apply}) ->
    call_general(Fn, St, ?ENV, ?SITE, Args, This);
%% §27.5.1.2 generator.prototype.next
call_native(Fn, St, ?ENV, ?SITE, Args, This, ?TOKEN_GENERATOR_NEXT = Token) ->
    Sent = case Args of
        [First | _] -> First;
        [] -> undefined
    end,
    case arc_interp_ffi:cell_of(A, This) of
        _ when A#agent.next_id >= A#agent.gc_at, F#frame.outer_depth =:= 0 ->
            call_general(Fn, St, ?ENV, ?SITE, Args, This);
        {?SOBJECT_TAG, {?GENERATOROBJ_TAG, Generator}, _, _, _, _, _} ->
            case ?CORE:resume_generator(A, F, Pc1 - 1, Generator, Sent, 1) of
                {threw, Thrown, A1} -> ?CORE:threw(Thrown, St, F, L, A1, C, ?RS, Pc1 - 1);
                {Done, V, A1} ->
                    ?PROPS:new_cell(V, St, F, L, A1, C, ?RS, N, ?ITER_RESULT(A1, V, Done));
                miss -> dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token);
                Failed -> Failed
            end;
        _ -> dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token)
    end;
%% the builtins that mostly answer without running or observing anything
call_native(Fn, St, ?ENV, ?SITE, Args, This, {string_n, string_prototype_char_code_at} = Token) ->
    case Args of
        [I] when is_binary(This), is_integer(I), I >= 0, I < byte_size(This) ->
            <<_:I/binary, Code, _/binary>> = This,
            ?GO(N, Code, St);
        _ -> call_plain(Fn, St, ?ENV, ?SITE, Args, This, Token)
    end;
call_native(Fn, St, ?ENV, ?SITE, Args, This, {string_n, Which} = Token) when is_binary(This) ->
    case arc_rt_string_ffi:ascii_method(Which, This, Args) of
        miss -> dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token);
        V -> ?GO(N, V, St)
    end;
call_native(Fn, St, ?ENV, ?SITE, Args, This, {math_n, Which} = Token) ->
    case arc_rt_math_ffi:math_kernel(Which, Args) of
        miss -> dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token);
        V -> ?GO(N, V, St)
    end;
call_native(Fn, St, ?ENV, ?SITE, Args, This, {array_n, array_prototype_push} = Token) ->
    case arc_rt_array_ffi:push(A, This, Args) of
        {pushed, Length, A1} -> ?GO(N, Length, St, A1);
        push_miss -> dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token)
    end;
call_native(Fn, St, ?ENV, ?SITE, Args, This, {array_n, array_prototype_pop} = Token) ->
    case arc_rt_array_ffi:pop(A, This) of
        {popped, V, A1} -> ?GO(N, V, St, A1);
        pop_miss -> dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token)
    end;
call_native(Fn, St, ?ENV, ?SITE, [Key | More] = Args, {?HANDLE_TAG, H} = This,
            {Group, Which} = Token)
  when Group =:= map_n; Group =:= set_n ->
    case arena_get(H, ?CELLS(A)) of
        {?SOBJECT_TAG, {?MAPOBJ_TAG, Table}, _, _, _, _, _} = Cell when Group =:= map_n ->
            K = arc_rt_val_ffi:map_key(Key),
            case Which of
                map_get ->
                    case element(?ORDERED_ENTRIES, Table) of
                        #{K := {_, V}} -> ?GO(N, V, St);
                        _ -> ?GO(N, undefined, St)
                    end;
                map_has -> ?GO(N, is_map_key(K, element(?ORDERED_ENTRIES, Table)), St);
                map_set when More =/= [] ->
                    Table1 = arc_ordered_entries_ffi:insert(Table, K, hd(More)),
                    ?GO(N, This, St, with_table(A, H, Cell, Table1));
                _ -> dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token)
            end;
        {?SOBJECT_TAG, {?SETOBJ_TAG, Table}, _, _, _, _, _} = Cell when Group =:= set_n ->
            K = arc_rt_val_ffi:map_key(Key),
            case Which of
                set_has -> ?GO(N, is_map_key(K, element(?ORDERED_ENTRIES, Table)), St);
                set_add when is_map_key(K, element(?ORDERED_ENTRIES, Table)) -> ?GO(N, This, St);
                set_add when not is_float(Key) ->
                    Table1 = arc_ordered_entries_ffi:insert(Table, K, Key),
                    ?GO(N, This, St, with_table(A, H, Cell, Table1));
                _ -> dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token)
            end;
        _ -> dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token)
    end;
call_native(_, St, ?ENV, N, _, _, _, [], This, {number_n, number_prototype_to_string})
  when is_number(This) ->
    ?GO(N, arc_rt_ops_ffi:str_of(This), St);
call_native(_, St, ?ENV, N, _, _, _, [V | _], _, {string_n, string_constructor})
  when is_number(V); is_binary(V) ->
    ?GO(N, arc_rt_ops_ffi:str_of(V), St);
call_native(_, St, ?ENV, N, _, _, _, [V | _], _, {number_n, number_constructor})
  when is_number(V) ->
    ?GO(N, V, St);
call_native(_, St, ?ENV, N, _, _, _, [V | _], _, {number_n, number_constructor})
  when is_binary(V) ->
    ?GO(N, arc_rt_val_ffi:mk_number(arc_rt_val_ffi:string_to_number(V)), St);
call_native(_, St, ?ENV, N, _, _, _, [{?HANDLE_TAG, _} = Object | _], _,
            {object_n, object_constructor}) ->
    ?GO(N, Object, St);
call_native(Fn, St, ?ENV, ?SITE, [Key] = Args, This,
            {object_n, object_prototype_has_own_property} = Token) ->
    case arc_interp_prop_ffi:has_own(A, This, Key) of
        miss -> dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token);
        Has -> ?GO(N, Has, St)
    end;
call_native(Fn, St, ?ENV, ?SITE, Args, This, {Group, Which} = Token)
  when Group =:= map_n; Group =:= set_n; Group =:= weak_n;
       Which =:= array_prototype_includes; Which =:= array_prototype_index_of;
       Which =:= array_is_array; Which =:= typed_array_get_length ->
    call_plain(Fn, St, ?ENV, ?SITE, Args, This, Token);
call_native(Fn, St, ?ENV, ?SITE, Args, This, Token) ->
    dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token).

with_kind(#agent{cells = Cells} = A, H, {Tag, _, Proto, Props, Symbols, Els, Extensible}, Kind) ->
    Cell = {Tag, Kind, Proto, Props, Symbols, Els, Extensible},
    A#agent{cells = arc_rt_arena_ffi:set(H, Cell, Cells)}.

with_table(A, H, {_, {Tag, Was}, _, _, _, _, _} = Cell, Table) ->
    ?TABLE_GROWN(with_kind(A, H, Cell, {Tag, Table}), Was, Table).

call_plain(Fn, St, ?ENV, ?SITE, Args, This, Token) ->
    case 'arc@rt@builtins':plain_answer(A, Token, This, Args) of
        {?SOME, V} -> ?GO(N, V, St);
        ?NONE -> dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token)
    end.

dispatch(Fn, St, ?ENV, ?SITE, Args, This, Token) ->
    case F#frame.depth of
        Depth when is_integer(Depth), 0 =< Depth, Depth < ?MAX_CALL_DEPTH ->
            #agent{call_depth = Synced, frames = Frames} = A,
            Frames1 = case Frames of
                [#frame_info{line = Line} | _] when Synced =:= Depth -> Frames;
                _ -> [{unlisted, F, Line, Depth - Synced} | Frames]
            end,
            A1 = A#agent{call_depth = Depth + 1, frames = Frames1},
            try 'arc@rt@builtins':dispatch_native(A1, Token, This, Args) of
                {V, #agent{next_id = Id, gc_at = Due} = A2}
                  when Id >= Due, F#frame.outer_depth =:= 0 ->
                    A3 = A2#agent{call_depth = Synced, frames = Frames},
                    ?GO(N, V, St, ?CORE:collected([V | St], F, L, A3, C, ?RS, Pc1 - 1));
                {V, #agent{} = A2} ->
                    ?GO(N, V, St, A2#agent{call_depth = Synced, frames = Frames})
            catch
                error:?JS_THROW(A2, Thrown) ->
                    ?CORE:threw(Thrown, St, F, L, A2#agent{call_depth = Synced, frames = Frames},
                                C, ?RS, Pc1 - 1)
            end;
        _ -> call_general(Fn, St, ?ENV, ?SITE, Args, This)
    end.

%% a loop that only calls has no safepoint, and neither has every callee
call_general(Fn, St, ?ENV, ?SITE, Args, This)
  when A#agent.next_id >= A#agent.gc_at, F#frame.outer_depth =:= 0 ->
    A1 = ?CORE:collected([Fn, This | Args] ++ St, ?ENV, Pc1 - 1),
    step_into(Fn, St, F, L, A1, C, ?RS, N, Pc1 - 1, Mode, Args, This);
call_general(Fn, St, ?ENV, ?SITE, Args, This) ->
    step_into(Fn, St, ?ENV, N, Pc1 - 1, Mode, Args, This).

step_into(Fn, St, ?ENV, N, Pc, Mode, Args, This) ->
    State = #state{agent = 'arc@interp@frames':sync(F, A, Pc), pc = Pc, stack = St,
                   locals = L, frame = F},
    Called = 'arc@interp@call':call_from(State, Fn, This, Args, St, Mode >= ?KEEP_NOTHING),
    ?CORE:stepped(Called, F, C, ?RS, N, Pc, c).

call_new(0, N, Pc1, {_, Mode}) ->
    ?FN -> construct(T, St, ?ENV, N, Pc1, Mode, [], T) end;
call_new(1, N, Pc1, {_, Mode}) ->
    ?FN ->
        [Fn | St1] = St,
        construct(Fn, St1, ?ENV, N, Pc1, Mode, [T], Fn)
    end;
call_new(2, N, Pc1, {_, Mode}) ->
    ?FN ->
        [X, Fn | St1] = St,
        construct(Fn, St1, ?ENV, N, Pc1, Mode, [X, T], Fn)
    end;
call_new(3, N, Pc1, {_, Mode}) ->
    ?FN ->
        [Y, X, Fn | St1] = St,
        construct(Fn, St1, ?ENV, N, Pc1, Mode, [X, Y, T], Fn)
    end;
call_new(Arity, N, Pc1, {_, Mode}) ->
    ?FN -> new_popping(T, St, ?ENV, N, Pc1, Mode, [T], Arity - 1) end.

new_popping(_, [Fn | St], ?ENV, N, Pc1, Mode, Args, 0) ->
    construct(Fn, St, ?ENV, N, Pc1, Mode, Args, Fn);
new_popping(T, [X | St], ?ENV, N, Pc1, Mode, Args, Left) ->
    new_popping(T, St, ?ENV, N, Pc1, Mode, [X | Args], Left - 1).

call_constructor(Arity, N, Pc1, {_, Mode}) ->
    ?FN -> constructor_popping(T, [T | St], ?ENV, N, Pc1, Mode, [], Arity) end.

constructor_popping(_, [NewTarget, Fn | St], ?ENV, N, Pc1, Mode, Args, 0) ->
    construct(Fn, St, ?ENV, N, Pc1, Mode, Args, NewTarget);
constructor_popping(T, [X | St], ?ENV, N, Pc1, Mode, Args, Left) ->
    constructor_popping(T, St, ?ENV, N, Pc1, Mode, [X | Args], Left - 1).

%% §10.2.2 construct
construct({?HANDLE_TAG, H} = Fn, St, ?ENV, N, Pc1, Mode, Args, NewTarget) ->
    #frame{outer_depth = Outer, depth = Depth, realm = Realm} = F,
    #agent{cells = Cells, next_id = Id} = A,
    case arena_get(H, Cells) of
        {?SOBJECT_TAG,
         #bytecode_fn{template = #func_template{code = Code} = Template, env = Env,
                      home_object = Home, flags = Flags, realm = Realm, unit_id = Unit},
         _, Props, _, _, _}
          when element(?FNFLAGS_IS_CONSTRUCTOR, Flags), is_integer(Depth), 0 =< Depth,
               Depth < ?MAX_CALL_DEPTH ->
            case Code of
                ?CODE(_, Entry, Start, Table)
                  when Template#func_template.is_derived_constructor ->
                    F1 = ?CALLEE_FRAME(?SAVED(?NONE), Depth + 1),
                    Entry(js_tdz, Args, F1, Env, A, Table, Fn, Home, NewTarget, Start);
                ?CODE(_, Entry, Start, Table) ->
                    Made = case NewTarget of
                        Fn -> made_with(Props, Home);
                        _ -> made_for(A, NewTarget)
                    end,
                    case Made of
                        %% §10.1.13
                        {?SOME, _} ->
                            Cell = {?SSHAPEDOBJECT_TAG, 0, Made, {}, #{}},
                            A1 = A#agent{cells = arc_rt_arena_ffi:set(Id, Cell, Cells),
                                         next_id = Id + 1},
                            This = {?HANDLE_TAG, Id},
                            F1 = ?CALLEE_FRAME(?SAVED({?SOME, This}), Depth + 1),
                            Entry(This, Args, F1, Env, A1, Table, Fn, Home, NewTarget, Start);
                        {?HANDLE_TAG, _}
                          when not Template#func_template.is_class_constructor ->
                            construct(Fn, St, F, L, at_home(A, H, Made), C, ?RS, N, Pc1, Mode,
                                      Args, NewTarget);
                        _ -> construct_general(Fn, St, ?ENV, N, Pc1, Args, NewTarget)
                    end;
                {} ->
                    {_, A1} = 'arc@interp@thread':first_call(A, Fn, Template),
                    construct(Fn, St, F, L, A1, C, ?RS, N, Pc1, Mode, Args, NewTarget)
            end;
        _ -> construct_general(Fn, St, ?ENV, N, Pc1, Args, NewTarget)
    end;
construct(Fn, St, ?ENV, N, Pc1, _, Args, NewTarget) ->
    construct_general(Fn, St, ?ENV, N, Pc1, Args, NewTarget).

%% a class constructor's home object is its prototype, so what it makes can all
%% share the one term. any other constructor has no use for a home object, and
%% is given its prototype as one for the same end
-compile({inline, [made_with/2]}).
made_with(Props, Home) ->
    case Props of
        #{?KEY_ID(?PROTOTYPE_KEY) :=
              {_, {?DATAPROPERTY_TAG, {?HANDLE_TAG, P} = Proto, _, _, _, _}}} ->
            case Home of
                {?SOME, {?HANDLE_TAG, P}} -> Home;
                _ -> Proto
            end;
        _ -> miss
    end.

made_for(A, {?HANDLE_TAG, H} = NewTarget) ->
    Made = case arc_rt_arena_ffi:get(H, ?CELLS(A)) of
        {?SOBJECT_TAG, #bytecode_fn{home_object = Home}, _, Props, _, _, _} ->
            made_with(Props, Home);
        _ -> miss
    end,
    case Made of
        {?SOME, _} -> Made;
        _ ->
            case arc_interp_prop_ffi:get_field(A, NewTarget, ?PROTOTYPE_KEY) of
                {?HANDLE_TAG, _} = Proto -> {?SOME, Proto};
                _ -> miss
            end
    end.

at_home(#agent{cells = Cells} = A, H, Proto) ->
    Cell = arc_rt_arena_ffi:get(H, Cells),
    with_kind(A, H, Cell, (element(?SOBJECT_KIND, Cell))#bytecode_fn{home_object = {?SOME, Proto}}).

construct_general(Fn, St, ?ENV, N, Pc1, Args, NewTarget) ->
    Pc = Pc1 - 1,
    State = #state{agent = 'arc@interp@frames':sync(F, A, Pc), pc = Pc, stack = St,
                   locals = L, frame = F},
    ?CORE:stepped('arc@interp@call':construct(State, Fn, Args, St, NewTarget),
                  F, C, ?RS, N, Pc, c).

%% calls from a builtin, each a run of its own that never collects
prepare(Fn, #bytecode_fn{template = Template, env = Env, home_object = Home, realm = Realm,
                         unit_id = Unit}, This, General) ->
    ?CODE(Takes, Entry, Start, Table) = Template#func_template.code,
    fun(#agent{call_depth = Depth, frames = Frames} = A, Args)
          when is_integer(Depth), 0 =< Depth, Depth < ?MAX_CALL_DEPTH ->
            case ?BOUND_THIS(Takes, This) of
                miss -> General(A, Args);
                This1 ->
                    Outer = Depth + 1,
                    try Entry(This1, Args, ?CALLEE_FRAME(direct, Outer), Env, A, Table, Fn,
                              Home, undefined, Start) of
                        [V | #agent{call_depth = Depth, frames = Frames} = A1] -> {V, A1};
                        [V | A1] -> {V, A1#agent{call_depth = Depth, frames = Frames}};
                        Outcome ->
                            {{error, Thrown}, A1} =
                                'arc@interp@resume':call_result(A, Outcome),
                            erlang:error(?JS_THROW(A1#agent{call_depth = Depth,
                                                            frames = Frames}, Thrown))
                    catch
                        error:?JS_THROW(A1, Thrown) ->
                            erlang:error(?JS_THROW(A1#agent{call_depth = Depth,
                                                            frames = Frames}, Thrown))
                    end
            end;
       (A, Args) -> General(A, Args)
    end.

%% frames above agent.call_depth were never pushed
leave(#agent{call_depth = Depth, frames = Frames} = A, Depth) ->
    A#agent{call_depth = Depth - 1,
            frames = case Frames of
                         [_ | Rest] -> Rest;
                         [] -> []
                     end};
leave(A, _) -> A.

-define(RESULT(CtorThis),
        (if CtorThis =:= ?NONE -> V;
            element(1, V) =:= ?HANDLE_TAG -> V;
            true -> element(2, CtorThis)
         end)).

-define(RETURN(Eventful),
        begin
            V = T,
            #frame{return_to = Saved, outer_depth = Outer, depth = Depth} = F,
            #agent{call_depth = Synced, next_id = Id, gc_at = Due} = A,
            case Saved of
                direct -> [V | A];
                _ when Eventful -> return(V, St, ?ENV, Pc);
                #saved_thread{caller = F1, stack = St1, locals = L1,
                              constructor_this = CtorThis, next = N, code = C1} ->
                    N(?RESULT(CtorThis), St1, F1, L1, A, C1, ?RS);
                #saved_regs{caller = F1, stack = St1, locals = L1,
                            constructor_this = CtorThis, next = N, code = C1,
                            r0 = Q0, r1 = Q1, r2 = Q2, r3 = Q3} ->
                    N(?RESULT(CtorThis), St1, F1, L1, A, C1, Q0, Q1, Q2, Q3);
                _ -> return(V, St, ?ENV, Pc)
            end
        end).

%% built with the value in T
return(plain, Pc) -> ?OP(?RETURN(Synced =:= Depth));
%% only the outermost run collects
return(collecting, Pc) ->
    ?OP(?RETURN(Synced =:= Depth orelse Id >= Due andalso Outer =:= 0));
return(derived, Pc) -> ?FN -> return_derived(T, St, ?ENV, Pc) end.

return(V, St, ?ENV, Pc) ->
    case F of
        #frame{return_to = direct} -> [V | A];
        %% a finished frame's locals are never read, so no flush
        #frame{return_to = outermost} ->
            {ok, {{completed, {?COMPLETION_NORMAL, V}},
                  #state{agent = A, pc = Pc, stack = St, locals = L, frame = F}}};
        #frame{return_to = Saved, outer_depth = Outer, depth = Depth} ->
            CtorThis = element(#saved_frame.constructor_this, Saved),
            return_to(?RESULT(CtorThis), Saved, leave(A, Depth), Outer)
    end.

%% §10.2.2 steps 10-12
return_derived(V, St, ?ENV, Pc) ->
    case F of
        #frame{return_to = Saved, outer_depth = Outer, depth = Depth}
          when element(#saved_frame.constructor_this, Saved) =:= ?NONE,
               element(1, Saved) =/= saved_cont ->
            State = #state{agent = 'arc@interp@frames':sync(F, A, Pc), pc = Pc, stack = St,
                           locals = L, frame = F},
            case 'arc@interp@call':constructor_result(State, V, ?NONE) of
                {ok, Result} -> return_to(Result, Saved, leave(State#state.agent, Depth), Outer);
                {error, {Thrown, #state{agent = A1}}} ->
                    ?CORE:threw(Thrown, St, F, L, A1, C, ?RS, Pc)
            end;
        _ -> return(V, St, ?ENV, Pc)
    end.

%% cont finishes the op cut off midway. There's no safepoint because its
%% captures live only here
return_to(V, #saved_cont{caller = F, pc = Pc1, stack = St, locals = L, cont = Cont,
                         resume = Resume}, A, _) ->
    Pc = Pc1 - 1,
    State = #state{agent = 'arc@interp@frames':sync(F, A, Pc), pc = Pc, stack = St,
                   locals = L, frame = F},
    case Resume of
        {?SOME, #resumption{next = N, takes_top = TakesTop, r0 = R0, r1 = R1, r2 = R2,
                            r3 = R3}} ->
            ?CODE(_, _, _, C) = (F#frame.func)#func_template.code,
            Out = case TakesTop of true -> c; false -> e end,
            ?CORE:stepped(Cont(V, State), F, C, ?RS, N, Pc, Out);
        ?NONE ->
            case Cont(V, State) of
                {ok, State1} -> ?CORE:enter(State1);
                {error, Exit} -> 'arc@interp@interpreter':after_exit(Exit)
            end
    end;
return_to(V, Saved, A, Outer) ->
    A1 = case A of
        #agent{next_id = Id, gc_at = Due} when Outer =:= 0, Id >= Due ->
            Synced = 'arc@interp@frames':sync(element(#saved_frame.caller, Saved), A,
                                              element(#saved_frame.pc, Saved)),
            {?SOME, State} = 'arc@interp@call':restore_frame(Synced, Saved, [V]),
            ('arc@interp@safepoint':maybe_collect_at_return(State))#state.agent;
        _ -> A
    end,
    case Saved of
        #saved_thread{caller = F, stack = St, locals = L, next = N, code = C} ->
            N(V, St, F, L, A1, C, undefined, undefined, undefined, undefined);
        #saved_regs{caller = F, stack = St, locals = L, next = N, code = C,
                    r0 = R0, r1 = R1, r2 = R2, r3 = R3} ->
            N(V, St, F, L, A1, C, ?RS);
        #saved_frame{caller = F, pc = Pc, stack = St, locals = L} ->
            ?CORE:enter(#state{agent = A1, pc = Pc, stack = [V | St], locals = L, frame = F})
    end.

%% a function still holding a draft that has been built again takes that
draft_entry(Draft) ->
    Entry = entry(Draft),
    fun(This, Args, #frame{func = Held} = F, Env, A, C, {_, H} = Fn, Home, NewTarget, Start) ->
        case ?CORE:redrafted(A, Held) of
            none -> Entry(This, Args, F, Env, A, C, Fn, Home, NewTarget, Start);
            #func_template{code = ?CODE(_, Entry1, Start1, C1)} = Template ->
                Cell = arc_rt_arena_ffi:get(H, ?CELLS(A)),
                A1 = case element(?SOBJECT_KIND, Cell) of
                    #bytecode_fn{template = #func_template{code = ?CODE(_, _, _, Has)}} = Kind
                      when element(?TABLE_DRAFT, Has) ->
                        with_kind(A, H, Cell, Kind#bytecode_fn{template = Template});
                    %% a builtin that prepared its call goes on coming in here
                    _ -> A
                end,
                Entry1(This, Args, F#frame{func = Template}, Env, A1, C1, Fn, Home, NewTarget,
                       Start1)
        end
    end.

%% how a call comes into a function: lays out the locals, as
%% arc_interp_locals_ffi:frame_locals/9 would, and starts
-define(U, undefined).
-define(START(Locals), Start(This, [], F, Locals, A, C, ?U, ?U, ?U, ?U)).
-define(ENTRY(EnvPattern, ArgsPattern, Locals),
        fun(This, ArgsPattern, F, EnvPattern, A, C, Fn, Home, NewTarget, Start) ->
                ?START(Locals);
           (This, Args, F, Env, A, C, Fn, Home, NewTarget, Start) ->
                entry_general(This, Args, F, Env, A, C, Fn, Home, NewTarget, Start)
        end).
%% N captures and Arity parameters, laid out as First, then up to five more locals
-define(ENTRIES(Keeps, N, Arity, EnvPattern, ArgsPattern, First),
entry(Keeps, N, Arity, 0) -> ?ENTRY(EnvPattern, ArgsPattern, {First});
entry(Keeps, N, Arity, 1) -> ?ENTRY(EnvPattern, ArgsPattern, {First, ?U});
entry(Keeps, N, Arity, 2) -> ?ENTRY(EnvPattern, ArgsPattern, {First, ?U, ?U});
entry(Keeps, N, Arity, 3) -> ?ENTRY(EnvPattern, ArgsPattern, {First, ?U, ?U, ?U});
entry(Keeps, N, Arity, 4) -> ?ENTRY(EnvPattern, ArgsPattern, {First, ?U, ?U, ?U, ?U});
entry(Keeps, N, Arity, 5) -> ?ENTRY(EnvPattern, ArgsPattern, {First, ?U, ?U, ?U, ?U, ?U})).

-define(E1, E1).
-define(E2, E1, E2).
-define(A1, X).
-define(A2, X, Y).
-define(A3, X, Y, Z).
-define(A4, X, Y, Z, W).
-define(THIS_0_1, This, ?A1).
-define(THIS_0_2, This, ?A2).
-define(THIS_0_3, This, ?A3).
-define(THIS_0_4, This, ?A4).
-define(THIS_1_0, ?E1, This).
-define(THIS_1_1, ?E1, This, ?A1).
-define(THIS_1_2, ?E1, This, ?A2).
-define(THIS_1_3, ?E1, This, ?A3).
-define(THIS_1_4, ?E1, This, ?A4).
-define(THIS_2_0, ?E2, This).
-define(THIS_2_1, ?E2, This, ?A1).
-define(THIS_2_2, ?E2, This, ?A2).
-define(THIS_2_3, ?E2, This, ?A3).
-define(THIS_2_4, ?E2, This, ?A4).
-define(NOTHING_1_1, ?E1, ?A1).
-define(NOTHING_1_2, ?E1, ?A2).
-define(NOTHING_1_3, ?E1, ?A3).
-define(NOTHING_1_4, ?E1, ?A4).
-define(NOTHING_2_1, ?E2, ?A1).
-define(NOTHING_2_2, ?E2, ?A2).
-define(NOTHING_2_3, ?E2, ?A3).
-define(NOTHING_2_4, ?E2, ?A4).

entry(#func_template{arity = Arity, local_count = Count, env_descriptors = Captures,
                     nested_captures = Nested, lexical = Lexical}) ->
    case Lexical of
        {owned_lexical_slots, _} -> fun entry_general/10;
        {trimmed_lexical_slots, N, true} -> entry(this, N, Arity, Count - N - 1 - Arity);
        _ ->
            N = case Nested of
                0 -> length(Captures);
                _ -> length(Captures) - Nested + 1
            end,
            entry(nothing, N, Arity, Count - N - Arity)
    end.

entry(nothing, 0, 0, Count) when Count < 8 ->
    Locals = erlang:make_tuple(Count, ?U),
    fun(This, _, F, _, A, C, _, _, _, Start) -> ?START(Locals) end;
?ENTRIES(this, 0, 0, _, [], This);
?ENTRIES(this, 0, 1, _, [?A1 | _], ?THIS_0_1);
?ENTRIES(this, 0, 2, _, [?A2 | _], ?THIS_0_2);
?ENTRIES(this, 0, 3, _, [?A3 | _], ?THIS_0_3);
?ENTRIES(this, 0, 4, _, [?A4 | _], ?THIS_0_4);
?ENTRIES(this, 1, 0, {?E1}, _, ?THIS_1_0);
?ENTRIES(this, 1, 1, {?E1}, [?A1 | _], ?THIS_1_1);
?ENTRIES(this, 1, 2, {?E1}, [?A2 | _], ?THIS_1_2);
?ENTRIES(this, 1, 3, {?E1}, [?A3 | _], ?THIS_1_3);
?ENTRIES(this, 1, 4, {?E1}, [?A4 | _], ?THIS_1_4);
?ENTRIES(this, 2, 0, {?E2}, _, ?THIS_2_0);
?ENTRIES(this, 2, 1, {?E2}, [?A1 | _], ?THIS_2_1);
?ENTRIES(this, 2, 2, {?E2}, [?A2 | _], ?THIS_2_2);
?ENTRIES(this, 2, 3, {?E2}, [?A3 | _], ?THIS_2_3);
?ENTRIES(this, 2, 4, {?E2}, [?A4 | _], ?THIS_2_4);
?ENTRIES(nothing, 0, 1, _, [?A1 | _], ?A1);
?ENTRIES(nothing, 0, 2, _, [?A2 | _], ?A2);
?ENTRIES(nothing, 0, 3, _, [?A3 | _], ?A3);
?ENTRIES(nothing, 0, 4, _, [?A4 | _], ?A4);
?ENTRIES(nothing, 1, 0, {?E1}, _, ?E1);
?ENTRIES(nothing, 1, 1, {?E1}, [?A1 | _], ?NOTHING_1_1);
?ENTRIES(nothing, 1, 2, {?E1}, [?A2 | _], ?NOTHING_1_2);
?ENTRIES(nothing, 1, 3, {?E1}, [?A3 | _], ?NOTHING_1_3);
?ENTRIES(nothing, 1, 4, {?E1}, [?A4 | _], ?NOTHING_1_4);
?ENTRIES(nothing, 2, 0, {?E2}, _, ?E2);
?ENTRIES(nothing, 2, 1, {?E2}, [?A1 | _], ?NOTHING_2_1);
?ENTRIES(nothing, 2, 2, {?E2}, [?A2 | _], ?NOTHING_2_2);
?ENTRIES(nothing, 2, 3, {?E2}, [?A3 | _], ?NOTHING_2_3);
?ENTRIES(nothing, 2, 4, {?E2}, [?A4 | _], ?NOTHING_2_4);
entry(_, _, _, _) -> fun entry_general/10.

entry_general(This, Args, F, Env, A, C, Fn, Home, NewTarget, Start) ->
    #func_template{arity = Arity, local_count = Count, lexical = Lexical} = F#frame.func,
    HomeObject = case Home of
        {?SOME, Object} -> Object;
        ?NONE -> undefined
    end,
    ?START(arc_interp_locals_ffi:frame_locals(Env, Lexical, This, Fn, HomeObject, NewTarget,
                                              Args, Arity, Count)).
