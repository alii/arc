%% Backing-store FFI for the VM's internal collection modules:
%%   - tuple-backed fixed arrays  (src/arc/vm/internal/tuple_array.gleam)
%%   - array-module tree arrays   (src/arc/vm/internal/tree_array.gleam)
%%   - the microtask job queue    (src/arc/vm/internal/job_queue.gleam)
-module(arc_array_ffi).
-export([array_get/2, array_repeat/2]).
-export([array_unsafe_get/2, array_set_unchecked/3]).
-export([tree_array_new/1, tree_array_from_list/2,
         tree_array_get_option/2, tree_array_set/3,
         tree_array_size/1, tree_array_resize/2,
         tree_array_reset/2, tree_array_sparse_fold/3]).
-export([job_queue_new/0, job_queue_push/2, job_queue_pop/1]).

%% Array (tuple-backed) operations
array_get(Index, Tuple) ->
    case Index >= 0 andalso Index < tuple_size(Tuple) of
        true -> {some, element(Index + 1, Tuple)};
        false -> none
    end.

%% Unchecked variants for hot-path reads where the compiler guarantees
%% the index is in bounds (bytecode fetch, constant pool, locals). No
%% bounds check, no Option box — badarg on violation.
array_unsafe_get(Index, Tuple) ->
    element(Index + 1, Tuple).
array_set_unchecked(Index, Value, Tuple) ->
    setelement(Index + 1, Tuple, Value).
%% Refuse to allocate an absurd tuple. This is a SANITY CAP on one BEAM
%% allocation (10M elements ~ 80MB on 64-bit), not a JS policy: array_repeat's
%% callers are the tuple-backed fixed arrays (a call frame's locals, a
%% constant pool), whose sizes the compiler bounds. The dense-vs-sparse policy
%% for JS array elements is a different number in a different place —
%% limits.max_dense_index, applied by elements.set before it ever reaches this
%% module — and the two were once the same hand-copied literal, which read as
%% if a locals frame and a JS array shared a limit. They do not.
-define(MAX_TUPLE_ALLOC, 10000000).

array_repeat(Value, Count) when Count =< ?MAX_TUPLE_ALLOC ->
    erlang:make_tuple(Count, Value);
array_repeat(_Value, _Count) ->
    erlang:error(array_too_large).

%% Erlang's array module — O(log n) functional array for JS elements.
%% Default is the caller-provided sentinel, so unset slots read back as a
%% valid JsValue (no atom sentinel leaks into Gleam).
tree_array_new(Default) ->
    array:new({default, Default}).
tree_array_from_list(List, Default) ->
    array:from_list(List, Default).
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
%% limits.max_dense_index BEFORE calling us, so the dense/sparse policy is
%% already decided by the time we get here and does not need restating (with a
%% second, hand-copied copy of the constant) at this boundary. Erlang's `array`
%% accepts any non-negative index, so the guard only rejects what `array` would
%% reject anyway. There is deliberately no fallback clause — a negative index
%% must crash (function_clause), never be silently discarded as it once was.
tree_array_set(Index, Value, A) when Index >= 0 ->
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
