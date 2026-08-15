-module(arc_aot_test262_ffi).
-export([pmap/2, atom_count/0, atom_limit/0]).

atom_count() -> erlang:system_info(atom_count).

atom_limit() -> erlang:system_info(atom_limit).

%% Map F over Items with one process per item, preserving order. A crash in
%% F crashes the caller with the same reason.
pmap(Items, F) ->
    Parent = self(),
    Ref = make_ref(),
    Pids = [spawn_link(fun() -> Parent ! {Ref, self(), F(I)} end) || I <- Items],
    [receive {Ref, Pid, R} -> R end || Pid <- Pids].
