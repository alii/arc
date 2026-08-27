%% no [safe]: only decode bytes this library produced
-module(arc_snapshot_ffi).
-export([encode/3, decode/2]).

-define(TAG, "arc-engine").
-define(TERM_TAG, arc_snapshot).

encode(Version, Store, Realms) when is_integer(Version) ->
    Term = erlang:term_to_binary({?TERM_TAG, Version, Store, Realms}),
    <<?TAG, Version:32, Term/binary>>.

%% error atoms mirror snapshot.DeserializeError constructors
decode(Version, <<?TAG, V:32, TermBin/binary>>) when V =:= Version ->
    try erlang:binary_to_term(TermBin) of
        {?TERM_TAG, Version, Store, Realms} ->
            {ok, {Store, Realms}};
        _Other ->
            {error, incompatible_snapshot}
    catch
        error:badarg -> {error, incompatible_snapshot}
    end;
decode(_Version, <<?TAG, _/binary>>) ->
    {error, incompatible_snapshot};
decode(_Version, _Bin) ->
    {error, malformed_binary}.
