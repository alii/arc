-module(rt_helpers_ffi).
-export([record/1, recorded/0, counter_sm/0, 'catch'/1]).

record(Term) ->
    self() ! {rt_test_record, Term},
    nil.

recorded() ->
    receive
        {rt_test_record, T} -> [T | recorded()]
    after 0 -> []
    end.

counter_sm() ->
    fun(St, 0, _Sent, {A, _, _} = Loc) -> {{yield, A, 1, Loc}, St};
       (St, 1, _Sent, {_, B, _} = Loc) -> {{yield, B, 2, Loc}, St};
       (St, 2, _Sent, {_, _, Done}) -> {{return, Done}, St}
    end.

'catch'(F) ->
    try {ok, F()} catch Class:Reason -> {error, {Class, Reason}} end.
