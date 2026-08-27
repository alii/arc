#!/usr/bin/env escript
%% -*- erlang -*-
%% usage: escript bench/profile.escript <file.js> [ebin_root] | tail -60
%% eprof of one interpreter run (compile excluded), sorted by time ascending.
main([File | Rest]) ->
    Root = case Rest of [] -> "build/dev/erlang"; [P | _] -> P end,
    [code:add_pathz(D) || D <- filelib:wildcard(Root ++ "/*/ebin")],
    {ok, Src} = file:read_file(File),
    {ok, {Body, Sb}} = arc@parser:parse_script(Src),
    {ok, T} = arc@compiler:compile(Body, Sb),
    E = arc@engine:new(),
    Ag = arc@engine:heap(E),
    _ = arc@interp@entry:run_script(Ag, T),
    eprof:start(),
    eprof:profile(fun() -> arc@interp@entry:run_script(Ag, T) end),
    eprof:analyze(total, [{sort, time}]),
    eprof:stop().
