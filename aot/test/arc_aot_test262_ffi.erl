-module(arc_aot_test262_ffi).
-export([pmap/2, atom_count/0, atom_limit/0]).

atom_count() -> erlang:system_info(atom_count).

atom_limit() -> erlang:system_info(atom_limit).

pmap(Items, F) ->
    Parent = self(),
    Ref = make_ref(),
    Pids = [spawn_link(fun() -> Parent ! {Ref, self(), F(I)} end) || I <- Items],
    [receive {Ref, Pid, R} -> R end || Pid <- Pids].
