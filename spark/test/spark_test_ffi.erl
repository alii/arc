-module(spark_test_ffi).
-export([main/0, note/1, notes/0, keep/1, kept/0]).

main() ->
    Modules = [list_to_atom(filename:rootname(F))
               || F <- filelib:wildcard("*.gleam", "test")],
    Tests = [{M, F} || M <- Modules,
                       {F, 0} <- M:module_info(exports),
                       lists:suffix("_test", atom_to_list(F))],
    Failed = lists:filter(fun({M, F}) ->
        try M:F() of
            _ -> false
        catch Class:Reason:Stack ->
            io:format("FAIL ~s:~s~n  ~p:~p~n  ~p~n",
                      [M, F, Class, Reason, Stack]),
            true
        end
    end, Tests),
    io:format("~p passed, ~p failed~n",
              [length(Tests) - length(Failed), length(Failed)]),
    case Failed of
        [] -> nil;
        _ -> erlang:halt(1)
    end.

note(Term) ->
    self() ! {spark_test_note, Term},
    nil.

notes() ->
    receive {spark_test_note, Term} -> [Term | notes()]
    after 0 -> []
    end.

keep(Term) ->
    self() ! {spark_test_keep, Term},
    nil.

kept() ->
    receive {spark_test_keep, Term} -> [Term | kept()]
    after 0 -> []
    end.
