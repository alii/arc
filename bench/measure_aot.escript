#!/usr/bin/env escript
%% -*- erlang -*-
%%! +sbwt none
%% Deterministic AOT measurement. Run from repo root after `cd aot && gleam build`.
%% usage: escript bench/measure_aot.escript <file.js> [runs]
%%   Compiles to a beam once, runs js_main `runs` times (fresh agent each),
%%   prints: REDS <min reductions>  MS <min ms>  MED <median ms>  GCS <n>
main([File | Rest]) ->
    Runs = case Rest of [] -> 5; [R | _] -> list_to_integer(R) end,
    [code:add_pathz(D) || D <- filelib:wildcard("aot/build/dev/erlang/*/ebin")],
    {ok, Src} = file:read_file(File),
    Name = <<"arc_aot_bench_mod">>,
    {ok, Beam} = arc_aot@compile:to_beam(Src, Name),
    {ok, Mod} = arc_aot@run:load(Beam, Name),
    Hooks = arc@host_hooks:default_host_hooks(),
    Results = [run_once(Mod, Hooks) || _ <- lists:seq(1, Runs)],
    Reds = lists:min([R || {R, _, _} <- Results]),
    RedsMax = lists:max([R || {R, _, _} <- Results]),
    Mss = lists:sort([M || {_, M, _} <- Results]),
    Gcs = lists:min([G || {_, _, G} <- Results]),
    Med = lists:nth((length(Mss) + 1) div 2, Mss),
    io:format("REDS ~p REDSMAX ~p MS ~.1f MED ~.1f GCS ~p~n",
              [Reds, RedsMax, hd(Mss), Med, Gcs]).

run_once(Mod, Hooks) ->
    Self = self(),
    Pid = spawn_opt(fun() ->
        A = arc_aot@run:seed(Hooks),
        erlang:garbage_collect(),
        {reductions, R0} = process_info(self(), reductions),
        {garbage_collection, GI0} = process_info(self(), garbage_collection),
        G0 = proplists:get_value(minor_gcs, GI0),
        T0 = erlang:monotonic_time(microsecond),
        {C, _A2} = arc_aot@run:apply_main(Mod, A),
        T1 = erlang:monotonic_time(microsecond),
        {reductions, R1} = process_info(self(), reductions),
        {garbage_collection, GI1} = process_info(self(), garbage_collection),
        G1 = proplists:get_value(minor_gcs, GI1),
        case element(1, C) of
            js_returned -> ok;
            _ -> io:format(standard_error, "outcome: ~p~n", [C])
        end,
        Self ! {done, R1 - R0, (T1 - T0) / 1000, G1 - G0}
    end, [{min_heap_size, 4000000}]),
    receive {done, R, Ms, G} -> {R, Ms, G}
    after 600000 -> exit(Pid, kill), error(timeout)
    end.
