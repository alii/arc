%%% call_time profiler for the emit_2core benches. Installs
%%% erlang:trace_pattern call_time counters on every module the compiled
%%% path can hit (rt_js_* + the FFI shims + the compiled bench module),
%%% runs the workload, then reads back {M,F,A} -> {Count, µs} for the
%%% top-N by time.
-module(emit_2core_profile_ffi).
-export([trace_on/1, trace_off/0, reset/0, top_n/1, alloc_bytes/0,
         all_mods/0, module_total/1, count_of/3, bench_op/4,
         count_pdict_gets/2, probe_jsf/2, eprof_run/2]).

%% Modules whose functions we want per-{M,F,A} timing for.
mods() ->
    [
     %% state-threaded runtime layer (Gleam)
     'twocore@runtime@rt_js_store',
     'twocore@runtime@rt_js_obj',
     'twocore@runtime@rt_js_call',
     'twocore@runtime@rt_js_ops',
     'twocore@runtime@rt_js_val',
     'twocore@runtime@rt_js_gc',
     'twocore@runtime@rt_js_class',
     'twocore@runtime@rt_js_async',
     'twocore@runtime@rt_js_types',
     'twocore@runtime@rt_js_ordered_entries',
     'twocore@runtime@rt_state',
     %% hand FFI shims
     twocore_rt_js_call_ffi,
     twocore_rt_js_store_ffi,
     twocore_rt_js_ops_ffi,
     twocore_rt_js_val_ffi,
     twocore_rt_js_obj_ffi,
     twocore_rt_js_ffi,
     %% gleam stdlib the runtime touches
     'gleam@dict', 'gleam@list', 'gleam@option',
     gleam_stdlib
    ].

all_mods() -> mods().

%% Install call_time counters on every function in mods() plus the
%% compiled bench module (passed in). Traces only the calling process.
trace_on(BenchMod) ->
    _ = [code:ensure_loaded(M) || M <- [BenchMod | mods()]],
    %% reset any prior counters
    erlang:trace_pattern({'_','_','_'}, false, [call_time, local]),
    erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    %% Runtime modules: trace local so intra-module helpers count.
    %% Bench module: only global (its Core-Erlang letrecs are join points,
    %% tracing them all is 100×+ overhead and they have no module_info anyway).
    _ = [erlang:trace_pattern({M,'_','_'}, true, [call_time, local])
         || M <- mods()],
    _ = erlang:trace_pattern({BenchMod,'_','_'}, true, [call_time]),
    erlang:trace(self(), true, [call]),
    nil.

trace_off() ->
    erlang:trace(self(), false, [call]),
    %% keep counters (top_n reads them AFTER trace_off) but do drop patterns
    %% before the next bench's untraced timing — done separately in reset/0.
    nil.

reset() ->
    erlang:trace_pattern({'_','_','_'}, false, [call_time, local]),
    erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    nil.

%% Return the top-N {M,F,A} by total µs across mods() + any traced module.
%% Each row: {ModBin, FnBin, Arity, Count, Micros}.
top_n(N) ->
    %% every {M,F,A} for every traced module
    All = lists:flatmap(
            fun(M) -> [{M, F, A} || {F, A} <- funs(M)] end,
            %% pull traced-module list from the pattern registry: any module
            %% with a call_time pattern installed. Simplest: reuse mods() +
            %% compiled bench modules by convention (arc_prof_*).
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
                         %% multiple pids/schedulers — sum
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

%% Per-module total µs (sum over all its functions with nonzero count).
module_total(M) ->
    lists:foldl(
      fun({F, A}, Acc) ->
          case erlang:trace_info({M, F, A}, call_time) of
              {call_time, L} when is_list(L) ->
                  Acc + lists:sum([S * 1000000 + Us || {_P,_C,S,Us} <- L]);
              _ -> Acc
          end
      end, 0, funs(M)).

%% Compiled-from-Core-Erlang bench modules don't export module_info/1.
funs(M) ->
    try M:module_info(functions) catch _:_ -> [] end.

%% Call count for a single {M,F,A}.
count_of(M, F, A) ->
    case erlang:trace_info({M, F, A}, call_time) of
        {call_time, L} when is_list(L) ->
            lists:sum([C || {_P, C, _S, _U} <- L]);
        _ -> 0
    end.

%% Untraced tight-loop micro-bench of kfn_code / cell_get / t_get_prop_any /
%% t_set_prop_any at N iterations against a seeded state. Returns µs.
bench_op(Which, St, Arg, N) ->
    T0 = erlang:monotonic_time(microsecond),
    bench_op_loop(Which, St, Arg, N),
    erlang:monotonic_time(microsecond) - T0.

bench_op_loop(_, _, _, 0) -> ok;
bench_op_loop(kfn_code, St, F, N) ->
    _ = 'twocore@runtime@rt_js_call':t_kfn_code(St, F, undefined),
    bench_op_loop(kfn_code, St, F, N-1);
bench_op_loop(kfn_code_ffi, St, F, N) ->
    _ = twocore_rt_js_call_ffi:t_kfn_code(St, F, undefined),
    bench_op_loop(kfn_code_ffi, St, F, N-1);
bench_op_loop(cell_get, St, H, N) ->
    _ = 'twocore@runtime@rt_js_store':t_cell_get(St, H),
    bench_op_loop(cell_get, St, H, N-1);
bench_op_loop(cell_get_ffi, St, H, N) ->
    _ = twocore_rt_js_store_ffi:t_cell_get(St, H),
    bench_op_loop(cell_get_ffi, St, H, N-1);
bench_op_loop(get_prop, St, {O, K}, N) ->
    {_, _} = 'twocore@runtime@rt_js_obj':t_get_prop_any(St, O, K),
    bench_op_loop(get_prop, St, {O, K}, N-1);
bench_op_loop(get_prop_own_data, St, {O, Kb}, N) ->
    _ = twocore_rt_js_obj_ffi:t_get_prop_own_data(St, O, Kb),
    bench_op_loop(get_prop_own_data, St, {O, Kb}, N-1);
bench_op_loop(get_prop_own_data_2k, St, {O, Kb1, Kb2}, N) ->
    %% Second-key path: Kb1 warms the mono cache, Kb2 hits the `{_,_}` arm
    %% (persistent-store peek, no install). Measures the multi-field read.
    _ = twocore_rt_js_obj_ffi:t_get_prop_own_data(St, O, Kb1),
    _ = twocore_rt_js_obj_ffi:t_get_prop_own_data(St, O, Kb2),
    bench_op_loop(get_prop_own_data_2k, St, {O, Kb1, Kb2}, N-1);
bench_op_loop(set_prop_own_data, St, {O, Kb}, N) ->
    _ = twocore_rt_js_obj_ffi:t_set_prop_own_data(St, O, Kb, 42),
    bench_op_loop(set_prop_own_data, St, {O, Kb}, N-1);
bench_op_loop(set_prop, St, {O, K}, N) ->
    {_, St2} = 'twocore@runtime@rt_js_obj':t_set_prop_any(St, O, K, 42),
    bench_op_loop(set_prop, St2, {O, K}, N-1);
bench_op_loop(nop, St, A, N) ->
    bench_op_loop(nop, St, A, N-1).

%% Bytes allocated by this process (for a coarse heap-churn number).
alloc_bytes() ->
    {garbage_collection_info, I} = process_info(self(), garbage_collection_info),
    proplists:get_value(heap_size, I, 0) * erlang:system_info(wordsize).

%% Enumerate every function actually loaded for `Mod` via erlang:get_module_info
%% (works on Core-Erlang-compiled modules where Mod:module_info/1 does not).
mod_funs(Mod) ->
    try erlang:get_module_info(Mod, functions)
    catch _:_ -> []
    end.

%% Per-jsf_N call_count + per-BIF call_count for one richards run.
%% Core-Erlang-loaded modules lack module_info; enumerate jsf_0..80 in
%% every ABI variant. Prints directly.
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
    %% Also trace every non-jsf local function in Mod (letrec join points).
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
    _ = twocore_rt_js_exec_ffi:apply_js_main(Mod, St),
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

%% Trace exported jsf_* of the compiled module + erlang:get/put; report
%% per-jsf {count,µs} + total pdict get/put/element/setelement — the
%% inline-path work the FFI-module trace can't see.
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
    twocore_rt_js_exec_ffi:apply_js_main(Mod, St),
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

%% count_pdict_gets(BenchMod, St) -> {GetCount, PutCount, Us}
count_pdict_gets(Mod, St) ->
    erlang:trace_pattern({erlang, get, 1}, true, [call_count]),
    erlang:trace_pattern({erlang, put, 2}, true, [call_count]),
    erlang:trace(self(), true, [call]),
    T0 = erlang:monotonic_time(microsecond),
    twocore_rt_js_exec_ffi:apply_js_main(Mod, St),
    Us = erlang:monotonic_time(microsecond) - T0,
    erlang:trace(self(), false, [call]),
    {call_count, Gc} = erlang:trace_info({erlang, get, 1}, call_count),
    {call_count, Pc} = erlang:trace_info({erlang, put, 2}, call_count),
    erlang:trace_pattern({erlang, get, 1}, false, [call_count]),
    erlang:trace_pattern({erlang, put, 2}, false, [call_count]),
    {Gc, Pc, Us}.
