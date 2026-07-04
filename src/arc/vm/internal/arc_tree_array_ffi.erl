%% Backing store for the O(log n) functional tree arrays
%% (src/arc/vm/internal/tree_array.gleam) — Erlang's `array` module, used for
%% JS array elements (DenseElements).
%%
%% Default is the caller-provided sentinel, so unset slots read back as a
%% valid JsValue (no atom sentinel leaks into Gleam).
%%
%% NO mutator here has a silent out-of-contract fallback: a negative index is
%% a caller bug and must crash (function_clause), never be quietly discarded.
-module(arc_tree_array_ffi).
-export([tree_array_new/1, tree_array_from_list/2,
         tree_array_get_option/2, tree_array_set/3,
         tree_array_size/1, tree_array_resize/2,
         tree_array_reset/2, tree_array_sparse_fold/3]).

tree_array_new(Default) ->
    array:new({default, Default}).
tree_array_from_list(List, Default) ->
    array:from_list(List, Default).

%% DenseElements uses JsUninitialized as default so holes (reset slots) are
%% distinguishable from explicit `arr[i] = undefined`. A slot that equals
%% default means "hole" → none. Out-of-bounds/negative → none. This is a READ,
%% so a negative index answers "nothing there" rather than crashing.
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
%% reject anyway.
tree_array_set(Index, Value, A) when Index >= 0 ->
    array:set(Index, Value, A).

tree_array_size(A) ->
    array:size(A).

%% NewSize < 0 is a caller bug (elements.truncate only passes JS array
%% lengths, which are >= 0). No fallback clause: crash, don't no-op.
tree_array_resize(A, NewSize) when NewSize >= 0 ->
    array:resize(NewSize, A).

%% Reset slot to default (creates a hole). O(log n). Resetting past the end is
%% a legitimate no-op (deleting an index the array never grew to); a NEGATIVE
%% index is not, and has no clause — it crashes like set/resize.
tree_array_reset(Index, A) when Index >= 0 ->
    case Index < array:size(A) of
        true -> array:reset(Index, A);
        false -> A
    end.

%% Fold over non-default entries only. Skips holes. O(k) where k = set count.
tree_array_sparse_fold(F, Acc, A) ->
    array:sparse_foldl(F, Acc, A).
