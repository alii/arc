%% Backing store for tuple-backed fixed arrays
%% (src/arc/vm/internal/tuple_array.gleam) — bytecode, constant pools, call
%% frame locals, function tables. Reads dominate; writes are rare and copy.
-module(arc_tuple_array_ffi).
-export([array_get/2, array_repeat/2,
         array_get_unchecked/2, array_set_unchecked/3]).

array_get(Index, Tuple) ->
    case Index >= 0 andalso Index < tuple_size(Tuple) of
        true -> {some, element(Index + 1, Tuple)};
        false -> none
    end.

%% Unchecked variants for hot-path reads where the compiler guarantees
%% the index is in bounds (bytecode fetch, constant pool, locals). No
%% bounds check, no Option box — badarg on violation.
array_get_unchecked(Index, Tuple) ->
    element(Index + 1, Tuple).
array_set_unchecked(Index, Value, Tuple) ->
    setelement(Index + 1, Tuple, Value).

%% Refuse to allocate an absurd tuple. This is a SANITY CAP on one BEAM
%% allocation (10M elements ~ 80MB on 64-bit), not a JS policy: array_repeat's
%% callers are the tuple-backed fixed arrays (a call frame's locals, a
%% constant pool), whose sizes the compiler bounds. The dense-vs-sparse policy
%% for JS array elements is a different number in a different place —
%% limits.max_dense_index, applied by elements.set — and the two were once the
%% same hand-copied literal, which read as if a locals frame and a JS array
%% shared a limit. They do not (they now do not even share a module).
-define(MAX_TUPLE_ALLOC, 10000000).

array_repeat(Value, Count) when Count =< ?MAX_TUPLE_ALLOC ->
    erlang:make_tuple(Count, Value);
array_repeat(_Value, _Count) ->
    erlang:error(array_too_large).
