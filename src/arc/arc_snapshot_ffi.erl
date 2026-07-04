%% Both sides of the engine snapshot CONTAINER (see arc/engine.gleam
%% `serialize`/`deserialize`). Kept in Erlang for one reason: `binary_to_term`
%% must never run on bytes we have not first proved to be one of our own
%% snapshots.
%%
%% Container layout — plain bytes, no term encoding:
%%
%%     <<"arc-engine", Version:32, TermBin/binary>>
%%
%% The tag and version live OUTSIDE the term, so `decode/2` can reject a
%% foreign, corrupt or stale binary by pattern-matching bytes, before
%% `binary_to_term` is ever handed anything. (The old format put the tag and
%% version INSIDE the term, which meant every byte sequence reaching the
%% public `engine.deserialize/1` was fully deserialized first and only then
%% checked — the check came too late to protect the decoder that performed it.)
-module(arc_snapshot_ffi).
-export([encode/2, decode/2]).

%% Bumping this is a format break; the Gleam-side `snapshot_version` const is
%% what identifies the payload SHAPE and is the Version passed in below.
-define(TAG, "arc-engine").

%% encode(Version, {Heap, Builtins, Global}) -> binary()
%%
%% Monomorphic on purpose: this is the only place in the tree that reaches for
%% `term_to_binary`, and it can only ever be handed a snapshot payload.
encode(Version, Snapshot) when is_integer(Version), is_tuple(Snapshot) ->
    Term = erlang:term_to_binary(Snapshot),
    <<?TAG, Version:32, Term/binary>>.

%% decode(Version, Bin) -> {ok, {Heap, Builtins, Global}}
%%                       | {error, malformed_binary}      % not a snapshot at all
%%                       | {error, incompatible_snapshot} % ours, but not this build's
%%
%% The error atoms are the constructors of `engine.DeserializeError`.
%%
%% Plain binary_to_term (not [safe]): a snapshot legitimately contains funs,
%% which [safe] would reject. Safety comes from the header match above it —
%% only bytes carrying our tag AND our exact version reach the decoder.
decode(Version, <<?TAG, V:32, TermBin/binary>>) when V =:= Version ->
    try erlang:binary_to_term(TermBin) of
        {_Heap, _Builtins, _Global} = Snapshot ->
            {ok, Snapshot};
        _Other ->
            %% Our tag and our version, but not our payload shape: a truncated
            %% or hand-forged container.
            {error, incompatible_snapshot}
    catch
        error:badarg -> {error, incompatible_snapshot}
    end;
%% Our tag, a different version: a snapshot from an older or newer build.
decode(_Version, <<?TAG, _/binary>>) ->
    {error, incompatible_snapshot};
%% Anything else — random bytes, a bare Erlang term, a pre-container snapshot,
%% or (a Gleam BitArray need not be byte-aligned) not even a binary.
decode(_Version, _Bin) ->
    {error, malformed_binary}.
