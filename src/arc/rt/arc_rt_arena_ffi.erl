%% persistent 16-way trie by cell id, shift is 4 * (levels - 1)
%% hot is the last written leaf held outside the trie, trie copy stale
%% get/2 crashes on ids never minted, probe/2 is total
-module(arc_rt_arena_ffi).
-export([new/0, get/2, get_option/2, probe/2, set/3, reset/2, fold/3,
         from_descending/1, count/1]).

-include("arc_rt_layout.hrl").

-define(B, 4).
-define(W, 16).
-define(M, 15).
-define(F, ?STORE_FREE_SLOT).
-define(EMPTY,
        {?F, ?F, ?F, ?F, ?F, ?F, ?F, ?F, ?F, ?F, ?F, ?F, ?F, ?F, ?F, ?F}).

new() -> {arena, 0, ?EMPTY, 0, ?EMPTY}.

get(I, {arena, _, _, HotIx, Hot}) when I bsr 4 =:= HotIx ->
    element((I band ?M) + 1, Hot);
get(I, {arena, 4, N, _, _}) ->
    element((I band ?M) + 1, element((I bsr 4) + 1, N));
get(I, {arena, 8, N, _, _}) ->
    element((I band ?M) + 1,
    element(((I bsr 4) band ?M) + 1,
    element((I bsr 8) + 1, N)));
get(I, {arena, 12, N, _, _}) ->
    element((I band ?M) + 1,
    element(((I bsr 4) band ?M) + 1,
    element(((I bsr 8) band ?M) + 1,
    element((I bsr 12) + 1, N))));
get(I, {arena, 16, N, _, _}) ->
    element((I band ?M) + 1,
    element(((I bsr 4) band ?M) + 1,
    element(((I bsr 8) band ?M) + 1,
    element(((I bsr 12) band ?M) + 1,
    element((I bsr 16) + 1, N)))));
get(I, {arena, 20, N, _, _}) ->
    element((I band ?M) + 1,
    element(((I bsr 4) band ?M) + 1,
    element(((I bsr 8) band ?M) + 1,
    element(((I bsr 12) band ?M) + 1,
    element(((I bsr 16) band ?M) + 1,
    element((I bsr 20) + 1, N))))));
get(I, {arena, S, N, _, _}) ->
    walk(I, S, N).

walk(I, 0, N) -> element((I band ?M) + 1, N);
walk(I, S, N) -> walk(I, S - ?B, element(((I bsr S) band ?M) + 1, N)).

probe(I, {arena, _, _, HotIx, Hot}) when I bsr 4 =:= HotIx ->
    element((I band ?M) + 1, Hot);
probe(I, {arena, S, N, _, _}) when I >= 0, I bsr S < ?W -> probe_1(I, S, N);
probe(_, _) -> ?F.

probe_1(_, _, ?F) -> ?F;
probe_1(I, 0, N) -> element((I band ?M) + 1, N);
probe_1(I, S, N) -> probe_1(I, S - ?B, element(((I bsr S) band ?M) + 1, N)).

get_option(I, A) ->
    case probe(I, A) of
        ?F -> none;
        V -> {some, V}
    end.

set(I, V, {arena, S, N, HotIx, Hot}) when I bsr 4 =:= HotIx ->
    {arena, S, N, HotIx, set16(I band ?M, Hot, V)};
set(I, V, {arena, S, N, HotIx, Hot}) when HotIx bsr S =:= 0 ->
    N1 = put_leaf_1(HotIx bsl ?B, Hot, S, N),
    {arena, S, N1, I bsr 4, set16(I band ?M, leaf(I, S, N1), V)};
set(I, V, {arena, S, N, HotIx, Hot}) when I >= 0 ->
    {S1, N1} = put_leaf(HotIx bsl ?B, Hot, S, N),
    {arena, S1, N1, I bsr 4, set16(I band ?M, leaf(I, S1, N1), V)}.

%% literal positions compile to an in-place copy, not a bif call
-compile({inline, [set16/3]}).
set16(I, L, V) when tuple_size(L) =:= ?W ->
    case I of
        0 -> setelement(1, L, V);
        1 -> setelement(2, L, V);
        2 -> setelement(3, L, V);
        3 -> setelement(4, L, V);
        4 -> setelement(5, L, V);
        5 -> setelement(6, L, V);
        6 -> setelement(7, L, V);
        7 -> setelement(8, L, V);
        8 -> setelement(9, L, V);
        9 -> setelement(10, L, V);
        10 -> setelement(11, L, V);
        11 -> setelement(12, L, V);
        12 -> setelement(13, L, V);
        13 -> setelement(14, L, V);
        14 -> setelement(15, L, V);
        _ -> setelement(16, L, V)
    end.

put_leaf(I, L, S, N) when I bsr S < ?W -> {S, put_leaf_1(I, L, S, N)};
put_leaf(I, L, S, N) -> put_leaf(I, L, S + ?B, setelement(1, ?EMPTY, N)).

put_leaf_1(_, L, 0, _) -> L;
put_leaf_1(I, L, S, ?F) ->
    set16((I bsr S) band ?M, ?EMPTY, put_leaf_1(I, L, S - ?B, ?F));
put_leaf_1(I, L, S, N) ->
    Ix = (I bsr S) band ?M,
    set16(Ix, N, put_leaf_1(I, L, S - ?B, element(Ix + 1, N))).

leaf(I, 4, N) when I bsr 4 < ?W ->
    full(element((I bsr 4) + 1, N));
leaf(I, 8, N) when I bsr 8 < ?W ->
    case element((I bsr 8) + 1, N) of
        N1 when is_tuple(N1) -> full(element(((I bsr 4) band ?M) + 1, N1));
        _ -> ?EMPTY
    end;
leaf(I, 12, N) when I bsr 12 < ?W ->
    case element((I bsr 12) + 1, N) of
        N2 when is_tuple(N2) ->
            case element(((I bsr 8) band ?M) + 1, N2) of
                N1 when is_tuple(N1) ->
                    full(element(((I bsr 4) band ?M) + 1, N1));
                _ -> ?EMPTY
            end;
        _ -> ?EMPTY
    end;
leaf(I, S, N) when I bsr S < ?W -> leaf_1(I, S, N);
leaf(_, _, _) -> ?EMPTY.

-compile({inline, [full/1]}).
full(?F) -> ?EMPTY;
full(L) -> L.

leaf_1(_, _, ?F) -> ?EMPTY;
leaf_1(_, 0, N) -> N;
leaf_1(I, S, N) -> leaf_1(I, S - ?B, element(((I bsr S) band ?M) + 1, N)).

reset(I, A) ->
    case probe(I, A) of
        ?F -> A;
        _ -> set(I, ?F, A)
    end.

settle({arena, S, N, HotIx, Hot}) ->
    {S1, N1} = put_leaf(HotIx bsl ?B, Hot, S, N),
    {arena, S1, N1, HotIx, Hot}.

fold(Fun, Acc, A) ->
    {arena, S, N, _, _} = settle(A),
    fold_1(Fun, Acc, N, S, 0).

fold_1(_, Acc, ?F, _, _) -> Acc;
fold_1(Fun, Acc, N, 0, Base) -> fold_leaf(Fun, Acc, N, Base, 1);
fold_1(Fun, Acc, N, S, Base) -> fold_node(Fun, Acc, N, S, Base, 1).

fold_leaf(Fun, Acc, N, Base, Ix) when Ix =< ?W ->
    case element(Ix, N) of
        ?F -> fold_leaf(Fun, Acc, N, Base, Ix + 1);
        V -> fold_leaf(Fun, Fun(Base + Ix - 1, V, Acc), N, Base, Ix + 1)
    end;
fold_leaf(_, Acc, _, _, _) -> Acc.

fold_node(Fun, Acc, N, S, Base, Ix) when Ix =< ?W ->
    Acc1 = fold_1(Fun, Acc, element(Ix, N), S - ?B, Base + ((Ix - 1) bsl S)),
    fold_node(Fun, Acc1, N, S, Base, Ix + 1);
fold_node(_, Acc, _, _, _, _) -> Acc.

count(A) -> fold(fun(_, _, K) -> K + 1 end, 0, A).

from_descending([]) -> new();
from_descending([{Top, _} | _] = Cells) ->
    {arena, S, N} = build(level(lists:reverse(Cells), []), 0),
    {arena, S, N, Top bsr ?B, leaf(Top, S, N)}.

build([{0, N}], S) -> {arena, S, N};
build(Nodes, S) -> build(level(Nodes, []), S + ?B).

level([], Acc) -> lists:reverse(Acc);
level([{Ix, _} | _] = L, Acc) ->
    P = Ix bsr ?B,
    {Elems, Rest} = take(L, P bsl ?B, 0, []),
    level(Rest, [{P, list_to_tuple(Elems)} | Acc]).

take(L, _, ?W, Acc) -> {lists:reverse(Acc), L};
take([{Ix, X} | Rest], Base, K, Acc) when Ix =:= Base + K ->
    take(Rest, Base, K + 1, [X | Acc]);
take(L, Base, K, Acc) -> take(L, Base, K + 1, [?F | Acc]).
