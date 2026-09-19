#!/usr/bin/env escript
%% -*- erlang -*-
%%! +sbwt none
%% usage: escript bench/memory.escript <file.js> [ebin_root]
%%   run 1, default heap flags under gc tracing:
%%     ALLOC <words allocated>  PEAK <max heap+old_heap block words>
%%     LIVE <words retained by final agent>  CELLS <arena cells>  MINOR/MAJOR gcs
%%   run 2, fullsweep_after 8 so majors are frequent:
%%     HIGH <max words after any major gc>  close to the live size arc controls
main([File | Rest]) ->
    Root = case Rest of [] -> "build/dev/erlang"; [P | _] -> P end,
    [code:add_pathz(D) || D <- filelib:wildcard(Root ++ "/*/ebin")],
    {ok, Src} = file:read_file(File),
    {ok, {Body, Scopes}} = arc@parser:parse_script(Src),
    {ok, T} = arc@compiler:compile_script(Body, Scopes),
    _ = arc@engine:new(),
    {Alloc, Peak, Live, Cells, Minor, Major, Ms} = run(T, []),
    High = run(T, [{fullsweep_after, 8}]),
    io:format("ALLOC ~p PEAK ~p HIGH ~p LIVE ~p CELLS ~p MINOR ~p MAJOR ~p MS ~.1f~n",
              [Alloc, Peak, High, Live, Cells, Minor, Major, Ms]).

run(T, Opts) ->
    Self = self(),
    Pid = spawn_opt(fun() ->
        receive go -> ok end,
        E = arc@engine:new(),
        A = arc@engine:agent(E),
        T0 = erlang:monotonic_time(microsecond),
        {C, A2} = arc@interp@entry:run_script(A, T),
        T1 = erlang:monotonic_time(microsecond),
        case element(1, C) of
            normal_completion -> ok;
            _ -> io:format(standard_error, "completion: ~p~n", [C])
        end,
        Store = element(2, A2),
        Live = erts_debug:size(A2),
        Cells = arc_rt_arena_ffi:count(element(2, Store)),
        Self ! {done, (T1 - T0) / 1000, Live, Cells}
    end, Opts),
    erlang:trace(Pid, true, [garbage_collection, {tracer, self()}]),
    Pid ! go,
    case Opts of
        [] -> loop(Pid, 0, 0, 0, 0, 0);
        _ -> high(Pid, 0)
    end.

high(Pid, High) ->
    receive
        {trace, Pid, gc_major_end, Info} ->
            high(Pid, max(High, proplists:get_value(heap_size, Info, 0)
                               + proplists:get_value(old_heap_size, Info, 0)));
        {trace, Pid, _, _} -> high(Pid, High);
        {done, _, _, _} -> High
    after 600000 -> exit(Pid, kill), error(timeout)
    end.

%% alloc sums young-heap growth between gcs, in words
loop(Pid, Peak, Minor, Major, Alloc, Last) ->
    receive
        {trace, Pid, Tag, Info} ->
            Sz = proplists:get_value(heap_block_size, Info, 0)
                + proplists:get_value(old_heap_block_size, Info, 0)
                + proplists:get_value(mbuf_size, Info, 0),
            H = proplists:get_value(heap_size, Info, 0)
                + proplists:get_value(mbuf_size, Info, 0),
            Peak1 = max(Peak, Sz),
            case Tag of
                gc_minor_start ->
                    loop(Pid, Peak1, Minor + 1, Major, Alloc + max(H - Last, 0), Last);
                gc_major_start ->
                    loop(Pid, Peak1, Minor, Major + 1, Alloc + max(H - Last, 0), Last);
                _ -> loop(Pid, Peak1, Minor, Major, Alloc, H)
            end;
        {done, Ms, Live, Cells} ->
            {Alloc, Peak, Live, Cells, Minor, Major, Ms}
    after 600000 -> exit(Pid, kill), error(timeout)
    end.
