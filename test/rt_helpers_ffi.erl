-module(rt_helpers_ffi).
-export([record/1, recorded/0, counter_state_machine/0, reductions/1,
         improper_list/1]).

record(Term) ->
    self() ! {rt_test_record, Term},
    nil.

recorded() ->
    receive
        {rt_test_record, T} -> [T | recorded()]
    after 0 -> []
    end.

counter_state_machine() ->
    fun(St, 0, _Sent, {A, _, _} = Loc) -> {{yield, A, 1, Loc}, St};
       (St, 1, _Sent, {_, B, _} = Loc) -> {{yield, B, 2, Loc}, St};
       (St, 2, _Sent, {_, _, Done}) -> {{return, Done}, St}
    end.

%% what Run costs, as the vm counts it whatever the machine
reductions(Run) ->
    {reductions, Before} = process_info(self(), reductions),
    Run(),
    {reductions, After} = process_info(self(), reductions),
    After - Before.

improper_list(Head) -> [Head | <<"tail">>].
