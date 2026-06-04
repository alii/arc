-module(arc_beam_ffi).
-export([receivers_get/0, receivers_put/1]).

%% Process-dictionary store for pending `receiveAsync` tickets — kept out of
%% core `State` because it is per-BEAM-process bookkeeping for a
%% per-BEAM-process loop.
%%
%% Stored as {TagToRefs, RefToTag}: a map from subject tag to the FIFO list
%% of pending settle refs, and the reverse map from settle ref to tag. Both
%% are maintained incrementally so registration and per-event lookup/removal
%% are O(log n) instead of rebuilding from a list on every mailbox event.

receivers_get() ->
    case get(arc_beam_receivers) of
        undefined -> {#{}, #{}};
        Pair -> Pair
    end.

receivers_put(Pair) ->
    put(arc_beam_receivers, Pair),
    nil.
