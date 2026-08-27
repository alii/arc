#!/usr/bin/env escript
%% -*- erlang -*-
%%! +sbwt none
%% Deterministic interpreter measurement.
%% usage: escript bench/measure.escript <file.js> [runs] [ebin_root]
%%   Compiles once, runs the script `runs` times (fresh engine each),
%%   prints: REDS <min reductions>  MS <min ms>  MED <median ms>  GCS <n> 
%% Reductions are near-deterministic; use them to hill-climb, ms to confirm.
main([File | Rest]) ->
    {Runs, Root} = case Rest of
        [] -> {5, "build/dev/erlang"};
        [R] -> {list_to_integer(R), "build/dev/erlang"};
        [R, P | _] -> {list_to_integer(R), P}
    end,
    [code:add_pathz(D) || D <- filelib:wildcard(Root ++ "/*/ebin")],
    {ok, Src} = file:read_file(File),
    {ok, {Body, Sb}} = arc@parser:parse_script(Src),
    {ok, T} = arc@compiler:compile(Body, Sb),
    Results = [run_once(T) || _ <- lists:seq(1, Runs)],
    Reds = lists:min([R || {R, _, _} <- Results]),
    RedsMax = lists:max([R || {R, _, _} <- Results]),
    Mss = lists:sort([M || {_, M, _} <- Results]),
    Gcs = lists:min([G || {_, _, G} <- Results]),
    Med = lists:nth((length(Mss) + 1) div 2, Mss),
    io:format("REDS ~p REDSMAX ~p MS ~.1f MED ~.1f GCS ~p~n",
              [Reds, RedsMax, hd(Mss), Med, Gcs]).

run_once(T) ->
    Self = self(),
    Pid = spawn_opt(fun() ->
        E = arc@engine:new(),
        A = arc@engine:heap(E),
        erlang:garbage_collect(),
        {reductions, R0} = process_info(self(), reductions),
        {garbage_collection, GI0} = process_info(self(), garbage_collection),
        G0 = proplists:get_value(minor_gcs, GI0),
        T0 = erlang:monotonic_time(microsecond),
        {C, _A2} = arc@interp@entry:run_script(A, T),
        T1 = erlang:monotonic_time(microsecond),
        {reductions, R1} = process_info(self(), reductions),
        {garbage_collection, GI1} = process_info(self(), garbage_collection),
        G1 = proplists:get_value(minor_gcs, GI1),
        case element(1, C) of
            normal_completion -> ok;
            _ -> io:format(standard_error, "completion: ~p~n", [C])
        end,
        Self ! {done, R1 - R0, (T1 - T0) / 1000, G1 - G0}
    end, [{min_heap_size, 4000000}]),
    receive {done, R, Ms, G} -> {R, Ms, G}
    after 600000 -> exit(Pid, kill), error(timeout)
    end.
