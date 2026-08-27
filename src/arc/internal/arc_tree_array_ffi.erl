%% negative index on a mutator must crash, no fallback clause
-module(arc_tree_array_ffi).
-export([tree_array_new/1, tree_array_from_list/2,
         tree_array_get_option/2, tree_array_set/3,
         tree_array_size/1, tree_array_resize/2,
         tree_array_reset/2, tree_array_sparse_fold/3]).

tree_array_new(Default) ->
    array:new({default, Default}).
tree_array_from_list(List, Default) ->
    array:from_list(List, Default).

%% slot equal to default is a hole
tree_array_get_option(Index, A) when Index >= 0 ->
    V = array:get(Index, A),
    case V =:= array:default(A) of
        true -> none;
        false -> {some, V}
    end;
tree_array_get_option(_Index, _A) ->
    none.

tree_array_set(Index, Value, A) when Index >= 0 ->
    array:set(Index, Value, A).

tree_array_size(A) ->
    array:size(A).

tree_array_resize(A, NewSize) when NewSize >= 0 ->
    array:resize(NewSize, A).

tree_array_reset(Index, A) when Index >= 0 ->
    case Index < array:size(A) of
        true -> array:reset(Index, A);
        false -> A
    end.

tree_array_sparse_fold(F, Acc, A) ->
    array:sparse_foldl(F, Acc, A).
