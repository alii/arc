%% Enumerate all functions in the compiled module via erlang:get_module_info
%% (works when module_info/1 is not exported), install LOCAL call_time on
%% every one, run once, dump.
-module(local_probe).
-export([run/2, raytrace_as_sobject/2, count_new_object_shaped/2]).

run(Mod, St) ->
    Fns = erlang:get_module_info(Mod, functions),
    io:format("  ~p total functions in ~p~n", [length(Fns), Mod]),
    erlang:trace_pattern({'_','_','_'}, false, [call_time, local]),
    erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    _ = [erlang:trace_pattern({Mod,F,A}, true, [call_time, local])
         || {F,A} <- Fns],
    erlang:trace(self(), true, [call]),
    _ = twocore_rt_js_exec_ffi:apply_js_main(Mod, St),
    erlang:trace(self(), false, [call]),
    Rows = lists:filtermap(
             fun({F,A}) ->
                 case erlang:trace_info({Mod,F,A}, call_time) of
                     {call_time, [{_,C,S,U}|_]} when C > 0 ->
                         {true, {S*1000000+U, C, F, A}};
                     _ -> false
                 end
             end, Fns),
    Sorted = lists:reverse(lists:sort(Rows)),
    Total = lists:sum([C || {_,C,_,_} <- Rows]),
    TotalUs = lists:sum([U || {U,_,_,_} <- Rows]),
    io:format("  Σ local calls: ~B   Σ local µs: ~B~n", [Total, TotalUs]),
    %% categorize by function-name prefix
    Cats = lists:foldl(
             fun({U,C,F,_A}, Acc) ->
                 K = case atom_to_list(F) of
                         "jsf_" ++ _ -> jsf;
                         "-jsf_" ++ _ -> jsf_letrec;
                         "-js_main" ++ _ -> js_main_anon;
                         "js_main" ++ _ -> js_main;
                         "j" ++ R when R >= "0", R =< "999" -> jN;
                         _ -> other
                     end,
                 {C0,U0} = maps:get(K, Acc, {0,0}),
                 Acc#{K => {C0+C, U0+U}}
             end, #{}, Rows),
    io:format("  ── categories ──~n", []),
    lists:foreach(fun({K,{C,U}}) ->
        io:format("    ~-16w  ~10B calls  ~8B µs~n", [K,C,U])
    end, maps:to_list(Cats)),
    io:format("  ── top-40 local functions by µs ──~n", []),
    lists:foreach(
      fun({U,C,F,A}) ->
          io:format("    ~-38s ~10B  ~8B us  ~6B ns/call~n",
                    [io_lib:format("~p/~p", [F,A]), C, U,
                     U*1000 div max(1,C)])
      end, lists:sublist(Sorted, 40)),
    nil.

%% Trace as_sobject/2 with a match_spec that captures element(2, Slot)=shape_id
%% via call_count on a per-Sid basis. Simpler: intercept via dbg-style trace
%% and count calls; also dump Store.shapes after run.
raytrace_as_sobject(Mod, St) ->
    erlang:trace_pattern({'_','_','_'}, false, [call_time, local]),
    erlang:trace_pattern({'twocore@runtime@rt_js_obj', as_sobject, 2},
                         true, [call_time, local]),
    erlang:trace_pattern({'twocore@runtime@rt_js_obj', read_object, 2},
                         true, [call_time, local]),
    erlang:trace_pattern({'twocore@runtime@rt_js_obj', devolve, 2},
                         true, [call_time, local]),
    erlang:trace_pattern({'twocore@runtime@rt_js_obj', get_from, 4},
                         true, [call_time, local]),
    erlang:trace(self(), true, [call]),
    {_, St2} = twocore_rt_js_exec_ffi:apply_js_main(Mod, St),
    erlang:trace(self(), false, [call]),
    lists:foreach(
      fun({M,F,A}) ->
          case erlang:trace_info({M,F,A}, call_time) of
              {call_time, [{_,C,_,_}|_]} ->
                  io:format("  ~p:~p/~p  ~10B calls~n", [M,F,A,C]);
              _ -> ok
          end
      end,
      [{'twocore@runtime@rt_js_obj', as_sobject, 2},
       {'twocore@runtime@rt_js_obj', read_object, 2},
       {'twocore@runtime@rt_js_obj', devolve, 2},
       {'twocore@runtime@rt_js_obj', get_from, 4}]),
    %% Dump shapes table: Sid -> arity
    {some, Store} = element(9, St2),
    Shapes = element(17, Store),
    io:format("  ── shapes table (Sid → arity, key sample) ──~n", []),
    lists:foreach(
      fun({Sid, {shape_desc, Arity, Offsets, _}}) ->
          Keys = [K || {K,_} <- lists:sublist(maps:to_list(Offsets), 3)],
          io:format("    sid=~3B arity=~3B keys=~p~n", [Sid, Arity, Keys])
      end, lists:keysort(1, maps:to_list(Shapes))),
    nil.

count_new_object_shaped(Mod, St) ->
    erlang:trace_pattern({'_','_','_'}, false, [call_count]),
    erlang:trace_pattern({twocore_rt_js_obj_ffi, t_new_object_shaped, 4},
                         true, [call_count]),
    erlang:trace_pattern({twocore_rt_js_call_ffi, shape_learn, 2},
                         true, [call_count]),
    erlang:trace_pattern({twocore_rt_js_obj_ffi, shape_transition, 3},
                         true, [call_count, local]),
    erlang:trace(self(), true, [call]),
    _ = twocore_rt_js_exec_ffi:apply_js_main(Mod, St),
    erlang:trace(self(), false, [call]),
    lists:foreach(
      fun({M,F,A}) ->
          case erlang:trace_info({M,F,A}, call_count) of
              {call_count, C} -> io:format("  ~p:~p/~p = ~B~n", [M,F,A,C]);
              _ -> ok
          end
      end,
      [{twocore_rt_js_obj_ffi, t_new_object_shaped, 4},
       {twocore_rt_js_call_ffi, shape_learn, 2},
       {twocore_rt_js_obj_ffi, shape_transition, 3}]),
    nil.

