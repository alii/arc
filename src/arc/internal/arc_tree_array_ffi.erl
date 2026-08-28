%% dense js element store, unset slots hold js_hole
%% bare tuple up to ?FLAT_MAX slots, else 16-way trie with hot leaf
%% trie invariant: every leaf below size except the hot one is in the trie
-module(arc_tree_array_ffi).
-compile({no_auto_import, [size/1]}).
-export([new/0, from_list/1, get/2, get_option/2, set/3, size/1, resize/2,
         reset/2, sparse_fold/3, to_list/1]).

-include("../rt/arc_rt_layout.hrl").

-define(H, ?ELEMS_HOLE).
-define(FLAT_MAX, 64).
-define(B, 4).
-define(W, 16).
-define(M, 15).
-define(EMPTY,
        {?H, ?H, ?H, ?H, ?H, ?H, ?H, ?H, ?H, ?H, ?H, ?H, ?H, ?H, ?H, ?H}).

new() -> {}.

from_list(L) ->
    case length(L) of
        N when N =< ?FLAT_MAX -> list_to_tuple(L);
        N -> build(L, N)
    end.

size({?VEC_TAG, Size, _, _, _, _}) -> Size;
size(T) -> tuple_size(T).

get(I, {?VEC_TAG, _, _, _, HotIx, Hot}) when I bsr ?B =:= HotIx ->
    element((I band ?M) + 1, Hot);
get(I, {?VEC_TAG, Size, 4, N, _, _}) when I < Size, I >= 0 ->
    element((I band ?M) + 1, element((I bsr 4) + 1, N));
get(I, {?VEC_TAG, Size, 8, N, _, _}) when I < Size, I >= 0 ->
    element((I band ?M) + 1,
    element(((I bsr 4) band ?M) + 1,
    element((I bsr 8) + 1, N)));
get(I, {?VEC_TAG, Size, S, N, _, _}) when I < Size, I >= 0 -> vget(I, S, N);
get(_, {?VEC_TAG, _, _, _, _, _}) -> ?H;
get(I, T) when I < tuple_size(T), I >= 0 -> element(I + 1, T);
get(_, _) -> ?H.

get_option(I, V) ->
    case get(I, V) of
        ?H -> none;
        X -> {some, X}
    end.

vget(I, 0, N) -> element((I band ?M) + 1, N);
vget(I, 4, N) ->
    element((I band ?M) + 1, element(((I bsr 4) band ?M) + 1, N));
vget(I, 8, N) ->
    element((I band ?M) + 1,
    element(((I bsr 4) band ?M) + 1,
    element(((I bsr 8) band ?M) + 1, N)));
vget(I, 12, N) ->
    element((I band ?M) + 1,
    element(((I bsr 4) band ?M) + 1,
    element(((I bsr 8) band ?M) + 1,
    element(((I bsr 12) band ?M) + 1, N))));
vget(I, S, N) -> vget(I, S - ?B, element(((I bsr S) band ?M) + 1, N)).

set(I, V, {?VEC_TAG, Size, S, N, HotIx, Hot}) when I bsr ?B =:= HotIx ->
    Size1 = if I < Size -> Size; true -> I + 1 end,
    {?VEC_TAG, Size1, S, N, HotIx, setelement((I band ?M) + 1, Hot, V)};
set(I, V, {?VEC_TAG, Size, S, N, HotIx, Hot}) when I < Size, I >= 0 ->
    {S1, N1} = put_leaf(HotIx bsl ?B, Hot, S, N),
    {?VEC_TAG, Size, S1, N1, I bsr ?B,
     setelement((I band ?M) + 1, leaf(I, S1, N1), V)};
set(I, V, {?VEC_TAG, Size, S, N, HotIx, Hot}) when I >= Size ->
    {S1, N1} = put_leaf(HotIx bsl ?B, Hot, S, N),
    {S2, N2} = fill(((Size - 1) bsr ?B) + 1, I bsr ?B, S1, N1),
    {?VEC_TAG, I + 1, S2, N2, I bsr ?B,
     setelement((I band ?M) + 1, leaf(I, S2, N2), V)};
set(I, V, T) when I < tuple_size(T), I >= 0 -> setelement(I + 1, T, V);
set(I, V, T) when I =:= tuple_size(T), I < ?FLAT_MAX ->
    erlang:append_element(T, V);
set(I, V, T) when I < ?FLAT_MAX ->
    erlang:make_tuple(I + 1, ?H, [{I + 1, V} | indexed(tuple_to_list(T), 1)]);
set(I, V, T) when I >= 0 ->
    set(I, V, promote(T)).

indexed([X | Xs], K) -> [{K, X} | indexed(Xs, K + 1)];
indexed([], _) -> [].

promote(T) -> build(tuple_to_list(T), tuple_size(T)).

%% missing leaves between old size and the new hot one
fill(L, To, S, N) when L < To ->
    {S1, N1} = put_leaf(L bsl ?B, ?EMPTY, S, N),
    fill(L + 1, To, S1, N1);
fill(_, _, S, N) -> {S, N}.

put_leaf(I, L, 0, _) when I < ?W -> {0, L};
put_leaf(I, L, 4, N) when I bsr 4 < ?W ->
    {4, setelement((I bsr 4) + 1, N, L)};
put_leaf(I, L, 8, N) when I bsr 8 < ?W ->
    I2 = (I bsr 8) + 1,
    case element(I2, N) of
        N1 when is_tuple(N1) ->
            {8, setelement(I2, N, setelement(((I bsr 4) band ?M) + 1, N1, L))};
        _ -> {8, put_leaf_1(I, L, 8, N)}
    end;
put_leaf(I, L, S, N) when I bsr S < ?W -> {S, put_leaf_1(I, L, S, N)};
put_leaf(I, L, S, N) ->
    put_leaf(I, L, S + ?B, setelement(1, ?EMPTY, N)).

put_leaf_1(_, L, 0, _) -> L;
put_leaf_1(I, L, S, N) when is_tuple(N) ->
    Ix = ((I bsr S) band ?M) + 1,
    setelement(Ix, N, put_leaf_1(I, L, S - ?B, element(Ix, N)));
put_leaf_1(I, L, S, _) ->
    setelement(((I bsr S) band ?M) + 1, ?EMPTY, put_leaf_1(I, L, S - ?B, ?H)).

leaf(I, S, N) when I bsr S < ?W -> leaf_1(I, S, N);
leaf(_, _, _) -> ?EMPTY.

leaf_1(_, 0, N) when is_tuple(N) -> N;
leaf_1(I, S, N) when is_tuple(N) ->
    leaf_1(I, S - ?B, element(((I bsr S) band ?M) + 1, N));
leaf_1(_, _, _) -> ?EMPTY.

reset(I, V) ->
    case I < size(V) of
        true -> set(I, ?H, V);
        false -> V
    end.

resize({?VEC_TAG, Size, _, _, _, _} = V, NewSize) when NewSize >= Size -> V;
resize({?VEC_TAG, _, _, _, _, _}, 0) -> {};
resize({?VEC_TAG, Size, S, N, HotIx, Hot}, NewSize) when NewSize > 0 ->
    Last = NewSize - 1,
    Ix = Last bsr ?B,
    case (Size - 1) bsr ?B of
        Ix when Ix =:= HotIx ->
            {?VEC_TAG, NewSize, S, N, HotIx, clear_from(Hot, (Last band ?M) + 1)};
        OldHi ->
            {S1, N1} = put_leaf(HotIx bsl ?B, Hot, S, N),
            N2 = case OldHi of
                Ix -> N1;
                _ -> prune(Last, S1, N1)
            end,
            {?VEC_TAG, NewSize, S1, N2, Ix,
             clear_from(leaf(Last, S1, N2), (Last band ?M) + 1)}
    end;
resize(T, NewSize) when NewSize >= tuple_size(T) -> T;
resize(T, NewSize) when NewSize >= 0 ->
    list_to_tuple(lists:sublist(tuple_to_list(T), NewSize)).

%% holes from slot K + 1 on
clear_from(T, K) when K >= ?W -> T;
clear_from(T, K) -> clear_from(setelement(K + 1, T, ?H), K + 1).

%% drop every subtree right of the path to I
prune(_, 0, N) -> N;
prune(I, S, N) ->
    Ix = ((I bsr S) band ?M) + 1,
    clear_from(setelement(Ix, N, prune(I, S - ?B, element(Ix, N))), Ix).

settle({?VEC_TAG, Size, S, N, HotIx, Hot}) ->
    {S1, N1} = put_leaf(HotIx bsl ?B, Hot, S, N),
    {?VEC_TAG, Size, S1, N1, HotIx, Hot}.

sparse_fold(F, Acc, {?VEC_TAG, _, _, _, _, _} = V) ->
    {?VEC_TAG, Size, S, N, _, _} = settle(V),
    fold_1(F, Acc, N, S, 0, Size);
sparse_fold(F, Acc, T) -> fold_flat(F, Acc, T, 1, tuple_size(T)).

fold_flat(F, Acc, T, I, N) when I =< N ->
    case element(I, T) of
        ?H -> fold_flat(F, Acc, T, I + 1, N);
        V -> fold_flat(F, F(I - 1, V, Acc), T, I + 1, N)
    end;
fold_flat(_, Acc, _, _, _) -> Acc.

fold_1(_, Acc, _, _, Base, Size) when Base >= Size -> Acc;
fold_1(_, Acc, N, _, _, _) when not is_tuple(N) -> Acc;
fold_1(F, Acc, N, 0, Base, _) -> fold_leaf(F, Acc, N, Base, 1);
fold_1(F, Acc, N, S, Base, Size) -> fold_node(F, Acc, N, S, Base, 1, Size).

fold_leaf(F, Acc, N, Base, Ix) when Ix =< ?W ->
    case element(Ix, N) of
        ?H -> fold_leaf(F, Acc, N, Base, Ix + 1);
        V -> fold_leaf(F, F(Base + Ix - 1, V, Acc), N, Base, Ix + 1)
    end;
fold_leaf(_, Acc, _, _, _) -> Acc.

fold_node(F, Acc, N, S, Base, Ix, Size) when Ix =< ?W ->
    Acc1 = fold_1(F, Acc, element(Ix, N), S - ?B, Base + ((Ix - 1) bsl S), Size),
    fold_node(F, Acc1, N, S, Base, Ix + 1, Size);
fold_node(_, Acc, _, _, _, _, _) -> Acc.

%% every slot below size, holes included
to_list({?VEC_TAG, Size, _, _, _, _} = V) ->
    {?VEC_TAG, _, S, N, _, _} = settle(V),
    lists:sublist(leaves(N, S, []), Size);
to_list(T) -> tuple_to_list(T).

leaves(N, _, Acc) when not is_tuple(N) -> Acc;
leaves(N, 0, Acc) -> tuple_to_list(N) ++ Acc;
leaves(N, S, Acc) -> leaves_node(N, S, ?W, Acc).

leaves_node(_, _, 0, Acc) -> Acc;
leaves_node(N, S, Ix, Acc) ->
    leaves_node(N, S, Ix - 1, leaves(element(Ix, N), S - ?B, Acc)).

%% list longer than ?FLAT_MAX into a settled trie
build([], _) -> {?VEC_TAG, 0, 0, ?EMPTY, 0, ?EMPTY};
build(L, N) ->
    {S, Root} = levels(chunks(L, []), 0),
    Last = N - 1,
    {?VEC_TAG, N, S, Root, Last bsr ?B, leaf(Last, S, Root)}.

chunks([], Acc) -> lists:reverse(Acc);
chunks(L, Acc) ->
    {Chunk, Rest} = take(L, ?W, []),
    chunks(Rest, [Chunk | Acc]).

%% exactly ?W items, absent ones are holes
take(Rest, 0, Acc) -> {list_to_tuple(lists:reverse(Acc)), Rest};
take([], K, Acc) -> take([], K - 1, [?H | Acc]);
take([X | Rest], K, Acc) -> take(Rest, K - 1, [X | Acc]).

levels([Root], S) -> {S, Root};
levels(Nodes, S) -> levels(chunks(Nodes, []), S + ?B).
