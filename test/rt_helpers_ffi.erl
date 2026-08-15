-module(rt_helpers_ffi).
-export([record/1, recorded/0]).

%% Mailbox-backed recorder for hook callbacks that return Nil.
record(Term) ->
    self() ! {rt_test_record, Term},
    nil.

recorded() ->
    receive
        {rt_test_record, T} -> [T | recorded()]
    after 0 -> []
    end.
