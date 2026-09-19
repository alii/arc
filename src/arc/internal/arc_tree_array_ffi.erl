%% dense js element store, unset slots hold js_hole
%% bare tuple up to ?FLAT_MAX slots, else 16-way trie with hot leaf
%% trie invariant: every leaf below size except the hot one is in the trie
-module(arc_tree_array_ffi).
-compile({no_auto_import, [size/1]}).
-export([new/0, from_list/1, get_or_hole/2, get/2, set/3, size/1, resize/2,
         reset/2, sparse_fold/3, to_list/1, dense_list/2, append_list/2,
         range_list/3]).

-include("../rt/arc_rt_layout.hrl").

-define(HOLE, ?ELEMS_HOLE).
-define(FLAT_MAX, 64).
-define(LEVEL_BITS, 4).
-define(FANOUT, 16).
-define(MASK, 15).
-define(EMPTY,
        {?HOLE, ?HOLE, ?HOLE, ?HOLE, ?HOLE, ?HOLE, ?HOLE, ?HOLE,
         ?HOLE, ?HOLE, ?HOLE, ?HOLE, ?HOLE, ?HOLE, ?HOLE, ?HOLE}).

new() -> {}.

from_list(L) ->
    case length(L) of
        N when N =< ?FLAT_MAX -> list_to_tuple(L);
        N -> build(L, N)
    end.

size({?VEC_TAG, Size, _, _, _, _}) -> Size;
size(T) -> tuple_size(T).

get_or_hole(I, {?VEC_TAG, _, _, _, HotIx, Hot}) when I bsr ?LEVEL_BITS =:= HotIx ->
    element((I band ?MASK) + 1, Hot);
get_or_hole(I, {?VEC_TAG, Size, 4, N, _, _}) when I < Size, I >= 0 ->
    element((I band ?MASK) + 1, element((I bsr 4) + 1, N));
get_or_hole(I, {?VEC_TAG, Size, 8, N, _, _}) when I < Size, I >= 0 ->
    element((I band ?MASK) + 1,
    element(((I bsr 4) band ?MASK) + 1,
    element((I bsr 8) + 1, N)));
get_or_hole(I, {?VEC_TAG, Size, S, N, _, _}) when I < Size, I >= 0 -> vget(I, S, N);
get_or_hole(_, {?VEC_TAG, _, _, _, _, _}) -> ?HOLE;
get_or_hole(I, T) when I < tuple_size(T), I >= 0 -> element(I + 1, T);
get_or_hole(_, _) -> ?HOLE.

get(I, V) ->
    case get_or_hole(I, V) of
        ?HOLE -> ?NONE;
        X -> {?SOME, X}
    end.

vget(I, 0, N) -> element((I band ?MASK) + 1, N);
vget(I, 4, N) ->
    element((I band ?MASK) + 1, element(((I bsr 4) band ?MASK) + 1, N));
vget(I, 8, N) ->
    element((I band ?MASK) + 1,
    element(((I bsr 4) band ?MASK) + 1,
    element(((I bsr 8) band ?MASK) + 1, N)));
vget(I, 12, N) ->
    element((I band ?MASK) + 1,
    element(((I bsr 4) band ?MASK) + 1,
    element(((I bsr 8) band ?MASK) + 1,
    element(((I bsr 12) band ?MASK) + 1, N))));
vget(I, S, N) ->
    vget(I, S - ?LEVEL_BITS, element(((I bsr S) band ?MASK) + 1, N)).

set(I, V, {?VEC_TAG, Size, S, N, HotIx, Hot}) when I bsr ?LEVEL_BITS =:= HotIx ->
    Size1 = if I < Size -> Size; true -> I + 1 end,
    {?VEC_TAG, Size1, S, N, HotIx, setelement((I band ?MASK) + 1, Hot, V)};
set(I, V, {?VEC_TAG, Size, S, N, HotIx, Hot}) when I < Size, I >= 0 ->
    {S1, N1} = put_leaf(HotIx bsl ?LEVEL_BITS, Hot, S, N),
    {?VEC_TAG, Size, S1, N1, I bsr ?LEVEL_BITS,
     setelement((I band ?MASK) + 1, leaf(I, S1, N1), V)};
set(I, V, {?VEC_TAG, Size, S, N, HotIx, Hot}) when I >= Size ->
    {S1, N1} = put_leaf(HotIx bsl ?LEVEL_BITS, Hot, S, N),
    {S2, N2} = fill(((Size - 1) bsr ?LEVEL_BITS) + 1, I bsr ?LEVEL_BITS, S1, N1),
    {?VEC_TAG, I + 1, S2, N2, I bsr ?LEVEL_BITS,
     setelement((I band ?MASK) + 1, leaf(I, S2, N2), V)};
set(I, V, T) when I < tuple_size(T), I >= 0 -> setelement(I + 1, T, V);
set(I, V, T) when I =:= tuple_size(T), I < ?FLAT_MAX ->
    erlang:append_element(T, V);
set(I, V, T) when I < ?FLAT_MAX ->
    erlang:make_tuple(I + 1, ?HOLE, [{I + 1, V} | indexed(tuple_to_list(T), 1)]);
set(I, V, T) when I >= 0 ->
    set(I, V, promote(T)).

indexed([X | Xs], K) -> [{K, X} | indexed(Xs, K + 1)];
indexed([], _) -> [].

promote(T) -> build(tuple_to_list(T), tuple_size(T)).

%% missing leaves between old size and the new hot one
fill(L, To, S, N) when L < To ->
    {S1, N1} = put_leaf(L bsl ?LEVEL_BITS, ?EMPTY, S, N),
    fill(L + 1, To, S1, N1);
fill(_, _, S, N) -> {S, N}.

put_leaf(I, L, 0, _) when I < ?FANOUT -> {0, L};
put_leaf(I, L, 4, N) when I bsr 4 < ?FANOUT ->
    {4, setelement((I bsr 4) + 1, N, L)};
put_leaf(I, L, 8, N) when I bsr 8 < ?FANOUT ->
    I2 = (I bsr 8) + 1,
    case element(I2, N) of
        N1 when is_tuple(N1) ->
            {8, setelement(I2, N, setelement(((I bsr 4) band ?MASK) + 1, N1, L))};
        _ -> {8, put_leaf_walk(I, L, 8, N)}
    end;
put_leaf(I, L, S, N) when I bsr S < ?FANOUT -> {S, put_leaf_walk(I, L, S, N)};
put_leaf(I, L, S, N) ->
    put_leaf(I, L, S + ?LEVEL_BITS, setelement(1, ?EMPTY, N)).

put_leaf_walk(_, L, 0, _) -> L;
put_leaf_walk(I, L, S, N) when is_tuple(N) ->
    Ix = ((I bsr S) band ?MASK) + 1,
    setelement(Ix, N, put_leaf_walk(I, L, S - ?LEVEL_BITS, element(Ix, N)));
put_leaf_walk(I, L, S, _) ->
    setelement(((I bsr S) band ?MASK) + 1, ?EMPTY,
               put_leaf_walk(I, L, S - ?LEVEL_BITS, ?HOLE)).

leaf(I, S, N) when I bsr S < ?FANOUT -> leaf_walk(I, S, N);
leaf(_, _, _) -> ?EMPTY.

leaf_walk(_, 0, N) when is_tuple(N) -> N;
leaf_walk(I, S, N) when is_tuple(N) ->
    leaf_walk(I, S - ?LEVEL_BITS, element(((I bsr S) band ?MASK) + 1, N));
leaf_walk(_, _, _) -> ?EMPTY.

reset(I, V) ->
    case I < size(V) of
        true -> set(I, ?HOLE, V);
        false -> V
    end.

resize({?VEC_TAG, Size, _, _, _, _} = V, NewSize) when NewSize >= Size -> V;
resize({?VEC_TAG, _, _, _, _, _}, 0) -> {};
resize({?VEC_TAG, Size, S, N, HotIx, Hot}, NewSize) when NewSize > 0 ->
    Last = NewSize - 1,
    Ix = Last bsr ?LEVEL_BITS,
    case (Size - 1) bsr ?LEVEL_BITS of
        Ix when Ix =:= HotIx ->
            {?VEC_TAG, NewSize, S, N, HotIx, clear_from(Hot, (Last band ?MASK) + 1)};
        OldHi ->
            {S1, N1} = put_leaf(HotIx bsl ?LEVEL_BITS, Hot, S, N),
            N2 = case OldHi of
                Ix -> N1;
                _ -> prune(Last, S1, N1)
            end,
            {?VEC_TAG, NewSize, S1, N2, Ix,
             clear_from(leaf(Last, S1, N2), (Last band ?MASK) + 1)}
    end;
resize(T, NewSize) when NewSize >= tuple_size(T) -> T;
resize(T, NewSize) when NewSize >= 0 ->
    list_to_tuple(lists:sublist(tuple_to_list(T), NewSize)).

%% holes from slot K + 1 on
clear_from(T, K) when K >= ?FANOUT -> T;
clear_from(T, K) -> clear_from(setelement(K + 1, T, ?HOLE), K + 1).

%% drop every subtree right of the path to I
prune(_, 0, N) -> N;
prune(I, S, N) ->
    Ix = ((I bsr S) band ?MASK) + 1,
    clear_from(setelement(Ix, N, prune(I, S - ?LEVEL_BITS, element(Ix, N))), Ix).

settle({?VEC_TAG, Size, S, N, HotIx, Hot}) ->
    {S1, N1} = put_leaf(HotIx bsl ?LEVEL_BITS, Hot, S, N),
    {?VEC_TAG, Size, S1, N1, HotIx, Hot}.

sparse_fold(F, Acc, {?VEC_TAG, _, _, _, _, _} = V) ->
    {?VEC_TAG, Size, S, N, _, _} = settle(V),
    fold_subtree(F, Acc, N, S, 0, Size);
sparse_fold(F, Acc, T) -> fold_flat(F, Acc, T, 1, tuple_size(T)).

fold_flat(F, Acc, T, I, N) when I =< N ->
    case element(I, T) of
        ?HOLE -> fold_flat(F, Acc, T, I + 1, N);
        V -> fold_flat(F, F(I - 1, V, Acc), T, I + 1, N)
    end;
fold_flat(_, Acc, _, _, _) -> Acc.

fold_subtree(_, Acc, _, _, Base, Size) when Base >= Size -> Acc;
fold_subtree(_, Acc, N, _, _, _) when not is_tuple(N) -> Acc;
fold_subtree(F, Acc, N, 0, Base, _) -> fold_leaf(F, Acc, N, Base, 1);
fold_subtree(F, Acc, N, S, Base, Size) -> fold_node(F, Acc, N, S, Base, 1, Size).

fold_leaf(F, Acc, N, Base, Ix) when Ix =< ?FANOUT ->
    case element(Ix, N) of
        ?HOLE -> fold_leaf(F, Acc, N, Base, Ix + 1);
        V -> fold_leaf(F, F(Base + Ix - 1, V, Acc), N, Base, Ix + 1)
    end;
fold_leaf(_, Acc, _, _, _) -> Acc.

fold_node(F, Acc, N, S, Base, Ix, Size) when Ix =< ?FANOUT ->
    Acc1 = fold_subtree(F, Acc, element(Ix, N), S - ?LEVEL_BITS,
                  Base + ((Ix - 1) bsl S), Size),
    fold_node(F, Acc1, N, S, Base, Ix + 1, Size);
fold_node(_, Acc, _, _, _, _, _) -> Acc.

%% the first Len values when all are set, else none
dense_list(A, Len) ->
    case size(A) of
        Len ->
            L = to_list(A),
            case lists:member(?HOLE, L) of
                true -> ?NONE;
                false -> {?SOME, L}
            end;
        _ when Len =:= 0 -> {?SOME, []};
        _ -> ?NONE
    end.

%% Count values from index From when all are set, else none
range_list(_, _, 0) -> {?SOME, []};
range_list(A, From, Count) when From >= 0 ->
    case From + Count =< size(A) of
        false -> ?NONE;
        true -> range_acc(A, From, From + Count - 1, [])
    end;
range_list(_, _, _) -> ?NONE.

range_acc(_, From, I, Acc) when I < From -> {?SOME, Acc};
range_acc(A, From, I, Acc) ->
    case get_or_hole(I, A) of
        ?HOLE -> ?NONE;
        V -> range_acc(A, From, I - 1, [V | Acc])
    end.

%% values written at size, size + 1, ..
append_list({?VEC_TAG, Size, _, _, _, _} = V, L) ->
    {V1, _} = lists:foldl(fun(X, {A, I}) -> {set(I, X, A), I + 1} end,
                          {V, Size}, L),
    V1;
append_list(T, L) ->
    from_list(tuple_to_list(T) ++ L).

to_list({?VEC_TAG, Size, _, _, _, _} = V) ->
    {?VEC_TAG, _, S, N, _, _} = settle(V),
    lists:sublist(leaves(N, S, []), Size);
to_list(T) -> tuple_to_list(T).

leaves(N, _, Acc) when not is_tuple(N) -> Acc;
leaves(N, 0, Acc) -> tuple_to_list(N) ++ Acc;
leaves(N, S, Acc) -> leaves_node(N, S, ?FANOUT, Acc).

leaves_node(_, _, 0, Acc) -> Acc;
leaves_node(N, S, Ix, Acc) ->
    leaves_node(N, S, Ix - 1, leaves(element(Ix, N), S - ?LEVEL_BITS, Acc)).

%% list longer than ?FLAT_MAX into a settled trie
build([], _) -> {?VEC_TAG, 0, 0, ?EMPTY, 0, ?EMPTY};
build(L, N) ->
    {S, Root} = levels(chunks(L, []), 0),
    Last = N - 1,
    {?VEC_TAG, N, S, Root, Last bsr ?LEVEL_BITS, leaf(Last, S, Root)}.

chunks([], Acc) -> lists:reverse(Acc);
chunks(L, Acc) ->
    {Chunk, Rest} = take(L, ?FANOUT, []),
    chunks(Rest, [Chunk | Acc]).

%% exactly ?FANOUT items, absent ones are holes
take(Rest, 0, Acc) -> {list_to_tuple(lists:reverse(Acc)), Rest};
take([], K, Acc) -> take([], K - 1, [?HOLE | Acc]);
take([X | Rest], K, Acc) -> take(Rest, K - 1, [X | Acc]).

levels([Root], S) -> {S, Root};
levels(Nodes, S) -> levels(chunks(Nodes, []), S + ?LEVEL_BITS).
