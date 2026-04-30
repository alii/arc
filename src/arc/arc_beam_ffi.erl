-module(arc_beam_ffi).
-export([receivers_get/0, receivers_put/1]).

%% Process-dictionary store for pending `receiveAsync` tickets — kept out of
%% core `State` because it is per-BEAM-process bookkeeping for a
%% per-BEAM-process loop.

receivers_get() ->
    case get(arc_beam_receivers) of
        undefined -> [];
        L -> L
    end.

receivers_put(L) ->
    put(arc_beam_receivers, L),
    nil.
