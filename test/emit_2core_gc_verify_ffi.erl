-module(emit_2core_gc_verify_ffi).
-export([try_refs/1, try_collect/1, try_run/2]).

try_refs(Slot) ->
    try {ok, twocore@runtime@rt_js_gc:refs_in_cell(Slot)}
    catch C:R -> {error, unicode:characters_to_binary(io_lib:format("~p:~p", [C, R]))}
    end.

try_collect(St) ->
    try {ok, twocore@runtime@rt_js_gc:t_collect(St, [])}
    catch C:R:T ->
        {error, unicode:characters_to_binary(
            io_lib:format("~p:~p at ~p", [C, R, hd(T)]))}
    end.

try_run(Mod, St) ->
    try {ok, twocore_rt_js_exec_ffi:apply_js_main(Mod, St)}
    catch C:R:T ->
        {error, unicode:characters_to_binary(
            io_lib:format("~p:~p at ~p", [C, R, hd(T)]))}
    end.
