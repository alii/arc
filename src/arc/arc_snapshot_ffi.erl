%% Decoding side of the engine snapshot format (see arc/engine.gleam
%% `serialize`/`deserialize`). Kept in Erlang because `binary_to_term` can
%% only be made total here: garbage bytes raise `badarg`, and a term that
%% decodes but is not the versioned envelope tuple must be rejected before
%% it reaches Gleam, where the return type promises the envelope shape.
-module(arc_snapshot_ffi).
-export([decode/1]).

%% decode(Bin) -> {ok, {Tag, Version, Heap, Builtins, Global}}
%%              | {error, malformed_binary}      % not external term format
%%              | {error, incompatible_snapshot} % decoded, but not an envelope
%%
%% The error atoms are the constructors of `engine.DeserializeError`.
%% Plain binary_to_term (not [safe]): the snapshot is trusted in-VM data and
%% legitimately contains funs, which [safe] would reject.
decode(Bin) when is_binary(Bin) ->
    try erlang:binary_to_term(Bin) of
        {Tag, Version, Heap, Builtins, Global}
          when is_binary(Tag), is_integer(Version) ->
            {ok, {Tag, Version, Heap, Builtins, Global}};
        _Other ->
            {error, incompatible_snapshot}
    catch
        error:badarg -> {error, malformed_binary}
    end;
%% A Gleam BitArray need not be byte-aligned; anything that isn't a binary
%% cannot be a snapshot.
decode(_Bits) ->
    {error, malformed_binary}.
