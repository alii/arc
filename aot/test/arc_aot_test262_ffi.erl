-module(arc_aot_test262_ffi).
-export([pmap/2]).

%% Map F over Items with one process per item, preserving order. A crash in
%% F crashes the caller with the same reason.
pmap(Items, F) ->
    Parent = self(),
    Ref = make_ref(),
    Pids = [spawn_link(fun() -> Parent ! {Ref, self(), F(I)} end) || I <- Items],
    [receive {Ref, Pid, R} -> R end || Pid <- Pids].
