%% The snapshot CONTAINER for arc/rt/snapshot.gleam (and arc/engine.gleam).
%%
%%     <<"arc-engine", Version:32, TermBin/binary>>
%%     TermBin = term_to_binary({arc_snapshot, Version, Store, Realms})
%%
%% The tag and version sit outside the term so `decode/2` rejects stale,
%% foreign or corrupt bytes by matching bytes, before `binary_to_term` runs;
%% the same pair is repeated inside the term as a shape check on what was
%% decoded. `Store` and `Realms` are opaque here: the Gleam caller builds and
%% types them, this module only owns the envelope.
%%
%% TRUST: the header is an accident guard, not an authenticity check. Anyone
%% can prepend it to a hostile term, and `binary_to_term` runs without
%% [safe] (a legitimate image may mint atoms the loading node has not seen
%% yet). Only hand `decode/2` bytes this library produced.
-module(arc_snapshot_ffi).
-export([encode/3, decode/2]).

-define(TAG, "arc-engine").
-define(TERM_TAG, arc_snapshot).

%% encode(Version, Store, Realms) -> binary()
encode(Version, Store, Realms) when is_integer(Version) ->
    Term = erlang:term_to_binary({?TERM_TAG, Version, Store, Realms}),
    <<?TAG, Version:32, Term/binary>>.

%% decode(Version, Bin) -> {ok, {Store, Realms}}
%%                      | {error, malformed_binary}      % not a snapshot at all
%%                      | {error, incompatible_snapshot} % ours, not this build's
%%
%% The error atoms are the constructors of `snapshot.DeserializeError`.
decode(Version, <<?TAG, V:32, TermBin/binary>>) when V =:= Version ->
    try erlang:binary_to_term(TermBin) of
        {?TERM_TAG, Version, Store, Realms} ->
            {ok, {Store, Realms}};
        _Other ->
            %% Our header, not our payload shape: keeps a wrong-shaped term
            %% from badmatching once it reaches Gleam.
            {error, incompatible_snapshot}
    catch
        error:badarg -> {error, incompatible_snapshot}
    end;
%% Our tag, another version: written by an older or newer build.
decode(_Version, <<?TAG, _/binary>>) ->
    {error, incompatible_snapshot};
%% Random bytes, a bare Erlang term, a pre-container snapshot, or (a Gleam
%% BitArray need not be byte-aligned) not even a binary.
decode(_Version, _Bin) ->
    {error, malformed_binary}.
