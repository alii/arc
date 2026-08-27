%%% arc_rt_arena_ffi: the cell arena behind `rt/arena.gleam`.
%%%
%%% {arena, Shift, Root, HotIx, Hot}. Root is a persistent 16-way trie
%%% indexed by cell id: a node at shift 0 is a leaf tuple of 16 slots,
%%% above that a tuple of 16 child nodes, and Shift is 4 * (levels - 1).
%%% The atom ?STORE_FREE_SLOT stands both for a free slot in a leaf and
%%% for a wholly free subtree. Ids are dense from 0, so the trie is as
%%% shallow as the highest id allows and grows by adding a root level.
%%%
%%% Hot is the last-written leaf (leaf index HotIx, ids HotIx*16 ..
%%% HotIx*16+15) held outside the trie, whose own copy of that leaf is
%%% stale: a write to it copies 16 slots instead of the whole path, which
%%% is what a run of allocations and the field writes that follow them do.
%%% A write to another leaf puts Hot back and takes that leaf out.
%%%
%%% get/2 is the hot read: the Hot check, then one clause per depth with no
%%% bounds or default bookkeeping. It is only defined for an id whose leaf
%%% exists (every minted, unswept id); anything else is a caller bug and
%%% crashes. probe/2 is the total read.
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

%% probe(I, Arena) -> Slot | ?STORE_FREE_SLOT, for any integer I.
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

%% set(I, V, Arena), I >= 0.
set(I, V, {arena, S, N, HotIx, Hot}) when I bsr 4 =:= HotIx ->
    {arena, S, N, HotIx, setelement((I band ?M) + 1, Hot, V)};
set(I, V, {arena, S, N, HotIx, Hot}) when I >= 0 ->
    {S1, N1} = put_leaf(HotIx bsl ?B, Hot, S, N),
    {arena, S1, N1, I bsr 4, setelement((I band ?M) + 1, leaf(I, S1, N1), V)}.

%% The trie with leaf L written at the leaf holding id I, grown to reach it.
%% One clause per depth for a path that already exists; put_leaf_slow
%% makes fresh nodes and levels.
put_leaf(I, L, 4, N) when I bsr 4 < ?W ->
    {4, setelement((I bsr 4) + 1, N, L)};
put_leaf(I, L, 8, N) when I bsr 8 < ?W ->
    I2 = (I bsr 8) + 1,
    case element(I2, N) of
        N1 when is_tuple(N1) ->
            {8, setelement(I2, N, setelement(((I bsr 4) band ?M) + 1, N1, L))};
        _ -> put_leaf_slow(I, L, 8, N)
    end;
put_leaf(I, L, 12, N) when I bsr 12 < ?W ->
    I3 = (I bsr 12) + 1,
    case element(I3, N) of
        N2 when is_tuple(N2) ->
            I2 = ((I bsr 8) band ?M) + 1,
            case element(I2, N2) of
                N1 when is_tuple(N1) ->
                    {12,
                     setelement(I3, N,
                     setelement(I2, N2,
                     setelement(((I bsr 4) band ?M) + 1, N1, L)))};
                _ -> put_leaf_slow(I, L, 12, N)
            end;
        _ -> put_leaf_slow(I, L, 12, N)
    end;
put_leaf(I, L, 16, N) when I bsr 16 < ?W ->
    I4 = (I bsr 16) + 1,
    case element(I4, N) of
        N3 when is_tuple(N3) ->
            I3 = ((I bsr 12) band ?M) + 1,
            case element(I3, N3) of
                N2 when is_tuple(N2) ->
                    I2 = ((I bsr 8) band ?M) + 1,
                    case element(I2, N2) of
                        N1 when is_tuple(N1) ->
                            {16,
                             setelement(I4, N,
                             setelement(I3, N3,
                             setelement(I2, N2,
                             setelement(((I bsr 4) band ?M) + 1, N1, L))))};
                        _ -> put_leaf_slow(I, L, 16, N)
                    end;
                _ -> put_leaf_slow(I, L, 16, N)
            end;
        _ -> put_leaf_slow(I, L, 16, N)
    end;
put_leaf(I, L, S, N) ->
    put_leaf_slow(I, L, S, N).

put_leaf_slow(I, L, S, N) when I bsr S < ?W -> {S, put_leaf_1(I, L, S, N)};
put_leaf_slow(I, L, S, N) ->
    put_leaf_slow(I, L, S + ?B, setelement(1, ?EMPTY, N)).

put_leaf_1(_, L, 0, _) -> L;
put_leaf_1(I, L, S, ?F) ->
    setelement(((I bsr S) band ?M) + 1, ?EMPTY, put_leaf_1(I, L, S - ?B, ?F));
put_leaf_1(I, L, S, N) ->
    Ix = ((I bsr S) band ?M) + 1,
    setelement(Ix, N, put_leaf_1(I, L, S - ?B, element(Ix, N))).

%% The trie's leaf holding id I, ?EMPTY when it has none.
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

%% reset(I, Arena): free slot I; an id the arena never grew to is a no-op.
reset(I, A) ->
    case probe(I, A) of
        ?F -> A;
        _ -> set(I, ?F, A)
    end.

%% The arena with Hot written back, so the trie alone tells the truth.
settle({arena, S, N, HotIx, Hot}) ->
    {S1, N1} = put_leaf(HotIx bsl ?B, Hot, S, N),
    {arena, S1, N1, HotIx, Hot}.

%% fold(Fun, Acc, Arena): Fun(Id, Slot, Acc) over the taken slots in
%% ascending id order.
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

%% from_descending([{Id, Slot}]) -> Arena holding exactly those slots, given
%% highest id first. Each node is built once, bottom up; a subtree with no
%% slot in it stays free. The highest leaf starts out hot.
from_descending([]) -> new();
from_descending([{Top, _} | _] = Cells) ->
    {arena, S, N} = build(level(lists:reverse(Cells), []), 0),
    {arena, S, N, Top bsr ?B, leaf(Top, S, N)}.

build([{0, N}], S) -> {arena, S, N};
build(Nodes, S) -> build(level(Nodes, []), S + ?B).

%% level([{Ix, X}] ascending) -> [{Ix bsr 4, ParentTuple}] ascending.
level([], Acc) -> lists:reverse(Acc);
level([{Ix, _} | _] = L, Acc) ->
    P = Ix bsr ?B,
    {Elems, Rest} = take(L, P bsl ?B, 0, []),
    level(Rest, [{P, list_to_tuple(Elems)} | Acc]).

take(L, _, ?W, Acc) -> {lists:reverse(Acc), L};
take([{Ix, X} | Rest], Base, K, Acc) when Ix =:= Base + K ->
    take(Rest, Base, K + 1, [X | Acc]);
take(L, Base, K, Acc) -> take(L, Base, K + 1, [?F | Acc]).
