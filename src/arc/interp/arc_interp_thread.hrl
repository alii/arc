%% what every module of threaded code shares.
%%
%% An op is a closure over its operands and the closure that follows it. They
%% all take the same arguments, so one passes on for free whatever it leaves be:
%%   T   the top of the operand stack, when the op is built to find it there
%%   St  the rest of the operand stack
%%   F   the frame, L its locals, A the agent
%%   C   the function's table of the closures that more than one place goes to
%%   R0..R3  the locals held in registers, which L has stale
%% Whether T holds the top (c) or nothing (e) is fixed for each closure when it
%% is built.

-ifndef(ARC_INTERP_THREAD_HRL).
-define(ARC_INTERP_THREAD_HRL, true).

-include("../rt/arc_rt_layout.hrl").
-include("../rt/arc_rt_arena.hrl").

-compile([nowarn_unused_vars, nowarn_unused_record]).

%% gleam records, checked by arc_rt_layout_test
-record(agent, {cells, next_id, gc_at, prop_seq, call_depth, frames, realm, store, hooks,
                globals}).
-record(store, {gc_threshold, shapes, next_shape, ics, plain_write_protos, global_epoch, ops,
                microtasks, pinned_roots, meta}).
-record(store_meta, {live_count, next_private_id, next_symbol_id, next_unit_id,
                     unhandled_rejections, old_gen, young_start,
                     major_live, minors_since_major, plain_index_protos,
                     async_context_mapping, lazy_code, threaded, threaded_before,
                     thread_keys, draft_runs, next_template_id, unit_texts, kept,
                     template_objects, host_fns, realms, import_hook, waiters, tz_zones,
                     checked_subject}).
-record(state, {agent, pc, stack, locals, frame}).
-record(frame, {func, unit_id, return_to, outer_depth, depth, try_stack, call_args, eval_env,
                realm}).
-record(saved_frame, {caller, pc, stack, locals, constructor_this}).
-record(saved_thread, {caller, pc, stack, locals, constructor_this, next, code}).
-record(saved_regs, {caller, pc, stack, locals, constructor_this, next, code,
                     r0, r1, r2, r3}).
-record(saved_cont, {caller, pc, stack, locals, constructor_this, cont, resume}).
-record(resumption, {next, takes_top, r0, r1, r2, r3}).
-record(func_template, {name, arity, length, local_count, bytecode, lines, functions,
                        env_descriptors, is_strict, is_arrow, is_derived_constructor,
                        is_generator, is_async, is_constructor, is_class_constructor,
                        local_names, lexical, code_kind, regs, nested_captures, code,
                        id}).
-record(bytecode_fn, {template, env, home_object, flags, fields_init, realm, unit_id,
                      birth}).
-record(suspended_frame, {template, pc, locals, stack, try_stack, eval_env, parked, call_args,
                          realm, unit_id}).
-record(frame_info, {name, script, line}).

%% limits.max_call_depth
-define(MAX_CALL_DEPTH, 10000).

%% a template's code once threaded. This says what a plain call does about
%% this: as_given, coerced, or special for a function no plain call just enters.
%% Entry lays out a call's locals and goes to Start
-define(CODE(This, Entry, Start, Table), {This, Entry, Start, Table}).
%% the table starts with where each pc that can be resumed at is in it, negated
%% when that closure takes the top in T; then, when Start loads registers
%% first, the closure for pc 1; then whether this is a first draft, to be built
%% again once a run has shown what its code works on
-define(TABLE_PCS, 1).
-define(TABLE_FIRST, 2).
-define(TABLE_DRAFT, 3).

%% a call site's mode says what to keep of the caller
-define(KEEP_PLAIN, 0).
-define(KEEP_REGS, 1).
%% added to either in §15.10 tail position, where the frame may allow it
-define(KEEP_NOTHING, 2).

-define(CORE, arc_interp_thread_ffi).
-define(ARITH, arc_interp_thread_arith_ffi).
-define(PROPS, arc_interp_thread_props_ffi).
-define(CALLS, arc_interp_thread_calls_ffi).

-define(RS, R0, R1, R2, R3).
-define(ENV, F, L, A, C, ?RS).
-define(FN, fun(T, St, ?ENV)).
%% ?FN can't be handed to another macro
-define(OP(Body), ?FN -> Body end).
-define(GO(N, Top, Stack), N(Top, Stack, ?ENV)).
-define(GO(N, Top, Stack, Agent), N(Top, Stack, F, L, Agent, C, ?RS)).

%% an op that leaves a value in T, for either way of finding the stack
-define(PUSHING(S, Body),
        case S of
            e -> ?FN -> Body end;
            c -> fun(T, St0, ?ENV) -> St = [T | St0], Body end
        end).

%% an op that leaves T be carries its pc negated when T holds the top
-define(SIGNED(Pc, S), (case S of e -> Pc; c -> -(Pc) end)).

%% a local operand is a position in L, or one of these; a match tries the
%% lowest first, and R0 is the hottest
-define(IN_R0, -4).
-define(IN_R1, -3).
-define(IN_R2, -2).
-define(IN_R3, -1).
-define(RD(I),
        (case I of
             ?IN_R0 -> R0;
             ?IN_R1 -> R1;
             ?IN_R2 -> R2;
             ?IN_R3 -> R3;
             _ -> element(I, L)
         end)).
-define(WR(D, V, N, Top, Stack),
        case D of
            ?IN_R0 -> N(Top, Stack, F, L, A, C, V, R1, R2, R3);
            ?IN_R1 -> N(Top, Stack, F, L, A, C, R0, V, R2, R3);
            ?IN_R2 -> N(Top, Stack, F, L, A, C, R0, R1, V, R3);
            ?IN_R3 -> N(Top, Stack, F, L, A, C, R0, R1, R2, V);
            _ -> ?CORE:put_local(Top, Stack, ?ENV, N, D, V)
        end).

%% Closure once for each register local I can be in, with Here bound to it, so
%% that ?RD(Here) and ?WR(Here, ...) are settled when compiled
-define(BY_REGISTER(I, Here, Closure),
        case I of
            ?IN_R0 -> Here = ?IN_R0, Closure;
            ?IN_R1 -> Here = ?IN_R1, Closure;
            ?IN_R2 -> Here = ?IN_R2, Closure;
            ?IN_R3 -> Here = ?IN_R3, Closure;
            Here when is_integer(Here), Here > 0 -> Closure
        end).

%% one closure for each place a local operand can be, found with no test
-define(PLACED(I, S, X, Body),
        case I of
            ?IN_R0 -> ?PUSHING(S, begin X = R0, Body end);
            ?IN_R1 -> ?PUSHING(S, begin X = R1, Body end);
            ?IN_R2 -> ?PUSHING(S, begin X = R2, Body end);
            ?IN_R3 -> ?PUSHING(S, begin X = R3, Body end);
            1 -> ?PUSHING(S, begin X = element(1, L), Body end);
            2 -> ?PUSHING(S, begin X = element(2, L), Body end);
            3 -> ?PUSHING(S, begin X = element(3, L), Body end);
            4 -> ?PUSHING(S, begin X = element(4, L), Body end);
            5 -> ?PUSHING(S, begin X = element(5, L), Body end);
            6 -> ?PUSHING(S, begin X = element(6, L), Body end);
            7 -> ?PUSHING(S, begin X = element(7, L), Body end);
            8 -> ?PUSHING(S, begin X = element(8, L), Body end);
            _ -> ?PUSHING(S, begin X = element(I, L), Body end)
        end).

%% a builder that checks this hands its closures an N they can call unchecked
-define(IS_OP(N), is_function(N, 10)).

%% a literal position in a tuple of known size is a copy, not a bif call
-define(AT1(Tuple, V), 1 -> setelement(1, Tuple, V)).
-define(AT2(Tuple, V), ?AT1(Tuple, V); 2 -> setelement(2, Tuple, V)).
-define(AT3(Tuple, V), ?AT2(Tuple, V); 3 -> setelement(3, Tuple, V)).
-define(AT4(Tuple, V), ?AT3(Tuple, V); 4 -> setelement(4, Tuple, V)).
-define(AT5(Tuple, V), ?AT4(Tuple, V); 5 -> setelement(5, Tuple, V)).
-define(AT6(Tuple, V), ?AT5(Tuple, V); 6 -> setelement(6, Tuple, V)).
-define(AT7(Tuple, V), ?AT6(Tuple, V); 7 -> setelement(7, Tuple, V)).
-define(AT8(Tuple, V), ?AT7(Tuple, V); 8 -> setelement(8, Tuple, V)).
-define(AT9(Tuple, V), ?AT8(Tuple, V); 9 -> setelement(9, Tuple, V)).
-define(AT10(Tuple, V), ?AT9(Tuple, V); 10 -> setelement(10, Tuple, V)).
-define(AT11(Tuple, V), ?AT10(Tuple, V); 11 -> setelement(11, Tuple, V)).
-define(AT12(Tuple, V), ?AT11(Tuple, V); 12 -> setelement(12, Tuple, V)).
-define(AT13(Tuple, V), ?AT12(Tuple, V); 13 -> setelement(13, Tuple, V)).
-define(AT14(Tuple, V), ?AT13(Tuple, V); 14 -> setelement(14, Tuple, V)).
-define(AT15(Tuple, V), ?AT14(Tuple, V); 15 -> setelement(15, Tuple, V)).
-define(AT16(Tuple, V), ?AT15(Tuple, V); 16 -> setelement(16, Tuple, V)).
-define(OF1, _).
-define(OF2, ?OF1, _).
-define(OF3, ?OF2, _).
-define(OF4, ?OF3, _).
-define(OF5, ?OF4, _).
-define(OF6, ?OF5, _).
-define(OF7, ?OF6, _).
-define(OF8, ?OF7, _).
-define(OF9, ?OF8, _).
-define(OF10, ?OF9, _).
-define(OF11, ?OF10, _).
-define(OF12, ?OF11, _).
-define(OF13, ?OF12, _).
-define(OF14, ?OF13, _).
-define(OF15, ?OF14, _).
-define(OF16, ?OF15, _).

-define(CELLS(A), A#agent.cells).
-define(SAFE(N), ((N) =< ?MAX_SAFE_INT andalso (N) >= -?MAX_SAFE_INT)).

-endif.
