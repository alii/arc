%% no [safe]: only decode bytes this library produced
-module(arc_snapshot_ffi).
-export([encode/4, decode/3]).

-define(TAG, "arc-engine").
-define(TERM_TAG, arc_snapshot).

%% names is the fingerprint of the fixed name list the keys in the image assume
encode(Version, Names, Store, Realms) when is_integer(Version), is_integer(Names) ->
    Term = erlang:term_to_binary({?TERM_TAG, Version, Store, Realms}),
    <<?TAG, Version:32, Names:64, Term/binary>>.

%% error atoms mirror snapshot.DeserializeError constructors
decode(Version, Names, <<?TAG, V:32, N:64, TermBin/binary>>)
  when V =:= Version, N =:= Names ->
    try erlang:binary_to_term(TermBin) of
        {?TERM_TAG, Version, Store, Realms} ->
            {ok, {Store, Realms}};
        _Other ->
            {error, incompatible_snapshot}
    catch
        error:badarg -> {error, incompatible_snapshot}
    end;
decode(_Version, _Names, <<?TAG, _/binary>>) ->
    {error, incompatible_snapshot};
decode(_Version, _Names, _Bin) ->
    {error, malformed_binary}.
