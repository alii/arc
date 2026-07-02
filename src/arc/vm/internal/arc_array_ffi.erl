%% Backing-store FFI for the VM's internal collection modules:
%%   - tuple-backed fixed arrays  (src/arc/vm/internal/tuple_array.gleam)
%%   - array-module tree arrays   (src/arc/vm/internal/tree_array.gleam)
%%   - the microtask job queue    (src/arc/vm/internal/job_queue.gleam)
-module(arc_array_ffi).
-export([array_get/2, array_set/3, array_repeat/2]).
-export([array_unsafe_get/2, array_set_unchecked/3]).
-export([tree_array_new/1, tree_array_from_list/2, tree_array_to_list/1,
         tree_array_get/2, tree_array_get_option/2, tree_array_set/3,
         tree_array_size/1, tree_array_resize/2,
         tree_array_reset/2, tree_array_sparse_fold/3]).
-export([job_queue_new/0, job_queue_push/2, job_queue_pop/1]).

%% Array (tuple-backed) operations
array_get(Index, Tuple) ->
    case Index >= 0 andalso Index < tuple_size(Tuple) of
        true -> {some, element(Index + 1, Tuple)};
        false -> none
    end.
array_set(Index, Value, Tuple) ->
    case Index >= 0 andalso Index < tuple_size(Tuple) of
        true -> {ok, setelement(Index + 1, Tuple, Value)};
        false -> {error, nil}
    end.

%% Unchecked variants for hot-path reads where the compiler guarantees
%% the index is in bounds (bytecode fetch, constant pool, locals). No
%% bounds check, no Option box — badarg on violation.
array_unsafe_get(Index, Tuple) ->
    element(Index + 1, Tuple).
array_set_unchecked(Index, Value, Tuple) ->
    setelement(Index + 1, Tuple, Value).
%% Cap tuple-backed arrays at 10M elements (~80MB on 64-bit).
%% JS specs allow arrays up to 2^32-1 but we use a sparse dict for those.
%% Mirrors limits.max_dense_index / limits.max_iteration in
%% src/arc/vm/limits.gleam — keep the three in sync. Used by array_repeat
%% and as the upper bound of tree_array_set's guard.
-define(MAX_DENSE_ELEMENTS, 10000000).

array_repeat(Value, Count) when Count =< ?MAX_DENSE_ELEMENTS ->
    erlang:make_tuple(Count, Value);
array_repeat(_Value, _Count) ->
    erlang:error(array_too_large).

%% Erlang's array module — O(log n) functional array for JS elements.
%% Default is the caller-provided JsUndefined so unset slots and to_list
%% both return valid JsValues (no atom sentinel leaks into Gleam).
tree_array_new(Default) ->
    array:new({default, Default}).
tree_array_from_list(List, Default) ->
    array:from_list(List, Default).
tree_array_to_list(A) ->
    array:to_list(A).
tree_array_get(Index, A) when Index >= 0 ->
    array:get(Index, A);
tree_array_get(_Index, A) ->
    array:default(A).
%% DenseElements uses JsUninitialized as default so holes (reset slots) are
%% distinguishable from explicit `arr[i] = undefined`. A slot that equals
%% default means "hole" → none. Out-of-bounds/negative → none.
tree_array_get_option(Index, A) when Index >= 0 ->
    case Index < array:size(A) of
        true ->
            V = array:get(Index, A),
            case V =:= array:default(A) of
                true -> none;
                false -> {some, V}
            end;
        false -> none
    end;
tree_array_get_option(_Index, _A) ->
    none.
%% elements.set (the only caller) promotes to the sparse representation at
%% limits.max_dense_index BEFORE calling us, so every Index that arrives is
%% 0 =< Index < max_dense_index; the guard re-enforces that cap at the FFI
%% boundary as defense in depth. There is deliberately no fallback clause —
%% an out-of-contract index must crash (function_clause), never be silently
%% discarded as it once was.
tree_array_set(Index, Value, A)
    when Index >= 0, Index < ?MAX_DENSE_ELEMENTS ->
    array:set(Index, Value, A).
tree_array_size(A) ->
    array:size(A).
%% NewSize < 0 is a caller bug (elements.truncate only passes JS array
%% lengths, which are >= 0). No fallback clause: crash, don't no-op.
tree_array_resize(A, NewSize) when NewSize >= 0 ->
    array:resize(NewSize, A).
%% Reset slot to default (creates a hole). O(log n). Out-of-bounds is a no-op.
tree_array_reset(Index, A) when Index >= 0 ->
    case Index < array:size(A) of
        true -> array:reset(Index, A);
        false -> A
    end;
tree_array_reset(_Index, A) ->
    A.
%% Fold over non-default entries only. Skips holes. O(k) where k = set count.
tree_array_sparse_fold(F, Acc, A) ->
    array:sparse_foldl(F, Acc, A).

%% Job queue — Erlang's queue module (two-list Okasaki FIFO). O(1) amortized
%% in/out vs the previous List+append O(n) per enqueue.
job_queue_new() -> queue:new().
job_queue_push(Q, Item) -> queue:in(Item, Q).
job_queue_pop(Q) ->
    case queue:out(Q) of
        {{value, Item}, Q2} -> {some, {Item, Q2}};
        {empty, _} -> none
    end.
