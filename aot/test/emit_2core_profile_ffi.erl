-module(emit_2core_profile_ffi).
-export([trace_on/1, trace_off/0, reset/0, top_n/1, alloc_bytes/0,
         all_mods/0, module_total/1, count_of/3, bench_op/4,
         count_pdict_gets/2, probe_jsf/2, eprof_run/2]).

mods() ->
    [
     'arc@rt@store',
     'arc@rt@obj',
     'arc@rt@lang',
     'arc@rt@call',
     'arc@rt@ops',
     'arc@rt@val',
     'arc@rt@gc',
     'arc@rt@class',
     'arc@rt@async',
     'arc@rt@types',
     'arc@vm@internal@ordered_entries',
     arc_rt_call_ffi,
     arc_rt_call_fast_ffi,
     arc_rt_store_ffi,
     arc_rt_ops_ffi,
     arc_rt_val_ffi,
     arc_rt_obj_ffi,
     'gleam@dict', 'gleam@list', 'gleam@option',
     gleam_stdlib
    ].

all_mods() -> mods().

trace_on(BenchMod) ->
    _ = [code:ensure_loaded(M) || M <- [BenchMod | mods()]],
    erlang:trace_pattern({'_','_','_'}, false, [call_time, local]),
    erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    %% bench module global only, local tracing is 100x overhead
    _ = [erlang:trace_pattern({M,'_','_'}, true, [call_time, local])
         || M <- mods()],
    _ = erlang:trace_pattern({BenchMod,'_','_'}, true, [call_time]),
    erlang:trace(self(), true, [call]),
    nil.

trace_off() ->
    erlang:trace(self(), false, [call]),
    nil.

reset() ->
    erlang:trace_pattern({'_','_','_'}, false, [call_time, local]),
    erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    nil.

top_n(N) ->
    All = lists:flatmap(
            fun(M) -> [{M, F, A} || {F, A} <- funs(M)] end,
            mods() ++ [M || M <- erlang:loaded(),
                            case atom_to_list(M) of
                                "arc_prof_" ++ _ -> true;
                                _ -> false
                            end]),
    Rows = lists:filtermap(
             fun({M, F, A}) ->
                 case erlang:trace_info({M, F, A}, call_time) of
                     {call_time, [{_Pid, Count, Sec, Usec}]} when Count > 0 ->
                         {true, {atom_to_binary(M, utf8),
                                 atom_to_binary(F, utf8),
                                 A, Count, Sec * 1000000 + Usec}};
                     {call_time, L} when is_list(L), L =/= [] ->
                         {C, U} = lists:foldl(
                                    fun({_P, Ct, S, Us}, {Ca, Ua}) ->
                                        {Ca + Ct, Ua + S * 1000000 + Us}
                                    end, {0, 0}, L),
                         case C of
                             0 -> false;
                             _ -> {true, {atom_to_binary(M, utf8),
                                          atom_to_binary(F, utf8),
                                          A, C, U}}
                         end;
                     _ -> false
                 end
             end, All),
    Sorted = lists:sort(fun({_,_,_,_,U1}, {_,_,_,_,U2}) -> U1 >= U2 end, Rows),
    lists:sublist(Sorted, N).

module_total(M) ->
    lists:foldl(
      fun({F, A}, Acc) ->
          case erlang:trace_info({M, F, A}, call_time) of
              {call_time, L} when is_list(L) ->
                  Acc + lists:sum([S * 1000000 + Us || {_P,_C,S,Us} <- L]);
              _ -> Acc
          end
      end, 0, funs(M)).

%% core erlang modules lack module_info
funs(M) ->
    try M:module_info(functions) catch _:_ -> [] end.

count_of(M, F, A) ->
    case erlang:trace_info({M, F, A}, call_time) of
        {call_time, L} when is_list(L) ->
            lists:sum([C || {_P, C, _S, _U} <- L]);
        _ -> 0
    end.

bench_op(Which, St, Arg, N) ->
    T0 = erlang:monotonic_time(microsecond),
    bench_op_loop(Which, St, Arg, N),
    erlang:monotonic_time(microsecond) - T0.

bench_op_loop(_, _, _, 0) -> ok;
bench_op_loop(kfn_code, St, F, N) ->
    _ = 'arc@rt@call':t_kfn_code(St, F, undefined),
    bench_op_loop(kfn_code, St, F, N-1);
bench_op_loop(kfn_code_ffi, St, F, N) ->
    _ = arc_rt_call_ffi:t_kfn_code(St, F, undefined),
    bench_op_loop(kfn_code_ffi, St, F, N-1);
bench_op_loop(cell_get, St, H, N) ->
    _ = 'arc@rt@store':t_cell_get(St, H),
    bench_op_loop(cell_get, St, H, N-1);
bench_op_loop(cell_get_ffi, St, H, N) ->
    _ = arc_rt_store_ffi:t_cell_get(St, H),
    bench_op_loop(cell_get_ffi, St, H, N-1);
bench_op_loop(get_prop, St, {O, K}, N) ->
    {_, _} = 'arc@rt@obj':t_get_prop(St, O, K),
    bench_op_loop(get_prop, St, {O, K}, N-1);
bench_op_loop(get_prop_fast, St, {O, Kb}, N) ->
    _ = arc_rt_obj_fast_ffi:t_get_prop(St, O, Kb, 0),
    bench_op_loop(get_prop_fast, St, {O, Kb}, N-1);
bench_op_loop(set_prop_own_data, St, {O, Kb}, N) ->
    _ = arc_rt_obj_ffi:t_set_prop_own_data(St, O, Kb, 42),
    bench_op_loop(set_prop_own_data, St, {O, Kb}, N-1);
bench_op_loop(set_prop, St, {O, K}, N) ->
    {_, St2} = 'arc@rt@obj':t_set_prop(St, O, K, 42),
    bench_op_loop(set_prop, St2, {O, K}, N-1);
bench_op_loop(nop, St, A, N) ->
    bench_op_loop(nop, St, A, N-1).

alloc_bytes() ->
    {garbage_collection_info, I} = process_info(self(), garbage_collection_info),
    proplists:get_value(heap_size, I, 0) * erlang:system_info(wordsize).

mod_funs(Mod) ->
    try erlang:get_module_info(Mod, functions)
    catch _:_ -> []
    end.

eprof_run(Mod, St) ->
    _ = code:ensure_loaded(Mod),
    Jsf = [{list_to_atom("jsf_" ++ integer_to_list(N) ++ S), A}
           || N <- lists:seq(0, 80),
              {S, A} <- [{"", 3}, {"_s", 1}, {"_s", 2}, {"_s", 3}, {"_s", 4},
                         {"_s", 5}, {"_s", 6}, {"_s", 7}, {"_s", 8},
                         {"_t", 2}, {"_t", 3}, {"_t", 4}, {"_t", 5},
                         {"_t", 6}, {"_t", 7}, {"_t", 8}]],
    Bifs = [{erlang, get, 1}, {erlang, put, 2}, {erlang, element, 2},
            {erlang, setelement, 3}, {erlang, is_tuple, 1},
            {erlang, is_atom, 1}, {erlang, is_map, 1}, {erlang, '=:=', 2},
            {erlang, is_integer, 1}, {erlang, tuple_size, 1},
            {erlang, is_map_key, 2}, {erlang, map_get, 2}],
    AllFuns = mod_funs(Mod),
    Jn = [{F, A} || {F, A} <- AllFuns,
                    case atom_to_list(F) of
                        "jsf_" ++ _ -> false;
                        "js_main" ++ _ -> false;
                        "module_info" ++ _ -> false;
                        _ -> true
                    end],
    erlang:trace_pattern({'_','_','_'}, false, [call_count, local]),
    erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    erlang:trace_pattern({'_','_','_'}, false, [call_time, local]),
    _ = [erlang:trace_pattern({Mod,F,A}, true, [call_count, local])
         || {F,A} <- Jn],
    _ = [erlang:trace_pattern({Mod,F,A}, true, [call_time, local])
         || {F,A} <- Jsf],
    _ = [erlang:trace_pattern(MFA, true, [call_count]) || MFA <- Bifs],
    erlang:trace(self(), true, [call]),
    _ = arc_aot_exec_ffi:apply_js_main(Mod, St),
    erlang:trace(self(), false, [call]),
    io:format("  BIF counts:~n", []),
    lists:foreach(
      fun({M,F,A}) ->
          case erlang:trace_info({M,F,A}, call_count) of
              {call_count, C} when is_integer(C), C > 0 ->
                  io:format("    ~-30s ~10B~n",
                            [io_lib:format("~p:~p/~p", [M,F,A]), C]);
              _ -> ok
          end
      end, Bifs),
    JnRows = lists:sort(
               fun({_,_,C1},{_,_,C2}) -> C1 >= C2 end,
               [{F,A,C} || {F,A} <- Jn,
                           {call_count, C} <- [erlang:trace_info({Mod,F,A}, call_count)],
                           is_integer(C), C > 0]),
    JnTotal = lists:sum([C || {_,_,C} <- JnRows]),
    io:format("    letrec-apply (~B distinct fns) TOTAL: ~B~n",
              [length(JnRows), JnTotal]),
    lists:foreach(
      fun({F,A,C}) ->
          io:format("      ~-30s ~10B~n",
                    [io_lib:format("~p/~p", [F,A]), C])
      end, lists:sublist(JnRows, 25)),
    io:format("  jsf_N by us (call_time; own excl. children):~n", []),
    Rows = lists:filtermap(
             fun({F,A}) ->
                 case erlang:trace_info({Mod,F,A}, call_time) of
                     {call_time, [{_,C,S,U}|_]} when C > 0 ->
                         {true, {S*1000000+U, C, F, A}};
                     _ -> false
                 end
             end, Jsf),
    Total = lists:sum([U || {U,_,_,_} <- Rows]),
    io:format("    TOTAL jsf us: ~B~n", [Total]),
    lists:foreach(
      fun({U,C,F,A}) ->
          io:format("    ~-22s ~10B  ~8B us  ~6B ns/call~n",
                    [io_lib:format("~p/~p", [F,A]), C, U,
                     U*1000 div max(1,C)])
      end, lists:reverse(lists:sort(Rows))),
    erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    nil.

probe_jsf(Mod, St) ->
    _ = code:ensure_loaded(Mod),
    Exports = try Mod:module_info(exports) catch _:_ -> [] end,
    Jsf = [{F,A} || {F,A} <- Exports,
                    case atom_to_list(F) of "jsf_" ++ _ -> true; _ -> false end],
    erlang:trace_pattern({'_','_','_'}, false, [call_time, local]),
    erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    _ = [erlang:trace_pattern({Mod,F,A}, true, [call_time]) || {F,A} <- Jsf],
    erlang:trace_pattern({erlang, get, 1}, true, [call_count]),
    erlang:trace_pattern({erlang, put, 2}, true, [call_count]),
    erlang:trace_pattern({erlang, element, 2}, true, [call_count]),
    erlang:trace_pattern({erlang, setelement, 3}, true, [call_count]),
    erlang:trace(self(), true, [call]),
    arc_aot_exec_ffi:apply_js_main(Mod, St),
    erlang:trace(self(), false, [call]),
    {call_count, Gc} = erlang:trace_info({erlang, get, 1}, call_count),
    {call_count, Pc} = erlang:trace_info({erlang, put, 2}, call_count),
    {call_count, Ec} = erlang:trace_info({erlang, element, 2}, call_count),
    {call_count, Sc} = erlang:trace_info({erlang, setelement, 3}, call_count),
    Rows = lists:filtermap(
             fun({F,A}) ->
                 case erlang:trace_info({Mod,F,A}, call_time) of
                     {call_time, [{_,C,S,U}|_]} when C > 0 ->
                         {true, {atom_to_binary(F,utf8), A, C, S*1000000+U}};
                     _ -> false
                 end
             end, Jsf),
    Sorted = lists:sort(fun({_,_,_,U1},{_,_,_,U2}) -> U1 >= U2 end, Rows),
    erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    {Gc, Pc, Ec, Sc, Sorted}.

count_pdict_gets(Mod, St) ->
    erlang:trace_pattern({erlang, get, 1}, true, [call_count]),
    erlang:trace_pattern({erlang, put, 2}, true, [call_count]),
    erlang:trace(self(), true, [call]),
    T0 = erlang:monotonic_time(microsecond),
    arc_aot_exec_ffi:apply_js_main(Mod, St),
    Us = erlang:monotonic_time(microsecond) - T0,
    erlang:trace(self(), false, [call]),
    {call_count, Gc} = erlang:trace_info({erlang, get, 1}, call_count),
    {call_count, Pc} = erlang:trace_info({erlang, put, 2}, call_count),
    erlang:trace_pattern({erlang, get, 1}, false, [call_count]),
    erlang:trace_pattern({erlang, put, 2}, false, [call_count]),
    {Gc, Pc, Us}.
