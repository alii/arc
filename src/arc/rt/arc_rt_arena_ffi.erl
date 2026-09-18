%% persistent 16-way trie by cell id, {arena, Shift, Root, HotIx, Hot}
%% shift is 4 * (levels - 1); hot is the last written leaf, its trie copy stale
%% get/2 crashes on ids never minted, probe/2 is total
-module(arc_rt_arena_ffi).
-export([new/0, get/2, get_option/2, probe/2, set/3, free/2, fold/3,
         from_descending/1, count/1, truncate/2, diff_below/3]).

-include("arc_rt_layout.hrl").

-define(LEVEL_BITS, 4).
-define(FANOUT, 16).
-define(MASK, 15).
-define(FREE, ?STORE_FREE_CELL).
-define(EMPTY,
        {?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE,
         ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE}).

new() -> {arena, 0, ?EMPTY, 0, ?EMPTY}.

get(I, {arena, _, _, HotIx, Hot}) when I bsr ?LEVEL_BITS =:= HotIx ->
    element((I band ?MASK) + 1, Hot);
get(I, {arena, 4, Root, _, _}) ->
    element((I band ?MASK) + 1, element((I bsr 4) + 1, Root));
get(I, {arena, 8, Root, _, _}) ->
    element((I band ?MASK) + 1,
    element(((I bsr 4) band ?MASK) + 1,
    element((I bsr 8) + 1, Root)));
get(I, {arena, 12, Root, _, _}) ->
    element((I band ?MASK) + 1,
    element(((I bsr 4) band ?MASK) + 1,
    element(((I bsr 8) band ?MASK) + 1,
    element((I bsr 12) + 1, Root))));
get(I, {arena, 16, Root, _, _}) ->
    element((I band ?MASK) + 1,
    element(((I bsr 4) band ?MASK) + 1,
    element(((I bsr 8) band ?MASK) + 1,
    element(((I bsr 12) band ?MASK) + 1,
    element((I bsr 16) + 1, Root)))));
get(I, {arena, 20, Root, _, _}) ->
    element((I band ?MASK) + 1,
    element(((I bsr 4) band ?MASK) + 1,
    element(((I bsr 8) band ?MASK) + 1,
    element(((I bsr 12) band ?MASK) + 1,
    element(((I bsr 16) band ?MASK) + 1,
    element((I bsr 20) + 1, Root))))));
get(I, {arena, Shift, Root, _, _}) ->
    walk(I, Shift, Root).

walk(I, 0, Node) -> element((I band ?MASK) + 1, Node);
walk(I, Shift, Node) ->
    walk(I, Shift - ?LEVEL_BITS, element(((I bsr Shift) band ?MASK) + 1, Node)).

probe(I, {arena, _, _, HotIx, Hot}) when I bsr ?LEVEL_BITS =:= HotIx ->
    element((I band ?MASK) + 1, Hot);
probe(I, {arena, Shift, Root, _, _}) when I >= 0, I bsr Shift < ?FANOUT ->
    probe_1(I, Shift, Root);
probe(_, _) -> ?FREE.

probe_1(_, _, ?FREE) -> ?FREE;
probe_1(I, 0, Node) -> element((I band ?MASK) + 1, Node);
probe_1(I, Shift, Node) ->
    probe_1(I, Shift - ?LEVEL_BITS, element(((I bsr Shift) band ?MASK) + 1, Node)).

get_option(I, A) ->
    case probe(I, A) of
        ?FREE -> none;
        V -> {some, V}
    end.

set(I, V, {arena, Shift, Root, HotIx, Hot}) when I bsr ?LEVEL_BITS =:= HotIx ->
    {arena, Shift, Root, HotIx, set16(I band ?MASK, Hot, V)};
set(I, V, {arena, Shift, Root, HotIx, Hot}) when HotIx bsr Shift =:= 0 ->
    Root1 = put_leaf_1(HotIx bsl ?LEVEL_BITS, Hot, Shift, Root),
    {arena, Shift, Root1, I bsr ?LEVEL_BITS,
     set16(I band ?MASK, leaf(I, Shift, Root1), V)};
set(I, V, {arena, Shift, Root, HotIx, Hot}) when I >= 0 ->
    {Shift1, Root1} = put_leaf(HotIx bsl ?LEVEL_BITS, Hot, Shift, Root),
    {arena, Shift1, Root1, I bsr ?LEVEL_BITS,
     set16(I band ?MASK, leaf(I, Shift1, Root1), V)}.

%% literal positions compile to an in-place copy, not a bif call
-compile({inline, [set16/3]}).
set16(I, Leaf, V) when tuple_size(Leaf) =:= ?FANOUT ->
    case I of
        0 -> setelement(1, Leaf, V);
        1 -> setelement(2, Leaf, V);
        2 -> setelement(3, Leaf, V);
        3 -> setelement(4, Leaf, V);
        4 -> setelement(5, Leaf, V);
        5 -> setelement(6, Leaf, V);
        6 -> setelement(7, Leaf, V);
        7 -> setelement(8, Leaf, V);
        8 -> setelement(9, Leaf, V);
        9 -> setelement(10, Leaf, V);
        10 -> setelement(11, Leaf, V);
        11 -> setelement(12, Leaf, V);
        12 -> setelement(13, Leaf, V);
        13 -> setelement(14, Leaf, V);
        14 -> setelement(15, Leaf, V);
        _ -> setelement(16, Leaf, V)
    end.

put_leaf(I, Leaf, Shift, Root) when I bsr Shift < ?FANOUT ->
    {Shift, put_leaf_1(I, Leaf, Shift, Root)};
put_leaf(I, Leaf, Shift, Root) ->
    put_leaf(I, Leaf, Shift + ?LEVEL_BITS, setelement(1, ?EMPTY, Root)).

put_leaf_1(_, Leaf, 0, _) -> Leaf;
put_leaf_1(I, Leaf, Shift, ?FREE) ->
    set16((I bsr Shift) band ?MASK, ?EMPTY,
          put_leaf_1(I, Leaf, Shift - ?LEVEL_BITS, ?FREE));
put_leaf_1(I, Leaf, Shift, Node) ->
    Ix = (I bsr Shift) band ?MASK,
    set16(Ix, Node, put_leaf_1(I, Leaf, Shift - ?LEVEL_BITS, element(Ix + 1, Node))).

leaf(I, 4, Root) when I bsr 4 < ?FANOUT ->
    or_empty(element((I bsr 4) + 1, Root));
leaf(I, 8, Root) when I bsr 8 < ?FANOUT ->
    case element((I bsr 8) + 1, Root) of
        Node1 when is_tuple(Node1) ->
            or_empty(element(((I bsr 4) band ?MASK) + 1, Node1));
        _ -> ?EMPTY
    end;
leaf(I, 12, Root) when I bsr 12 < ?FANOUT ->
    case element((I bsr 12) + 1, Root) of
        Node2 when is_tuple(Node2) ->
            case element(((I bsr 8) band ?MASK) + 1, Node2) of
                Node1 when is_tuple(Node1) ->
                    or_empty(element(((I bsr 4) band ?MASK) + 1, Node1));
                _ -> ?EMPTY
            end;
        _ -> ?EMPTY
    end;
leaf(I, Shift, Root) when I bsr Shift < ?FANOUT -> leaf_1(I, Shift, Root);
leaf(_, _, _) -> ?EMPTY.

-compile({inline, [or_empty/1]}).
or_empty(?FREE) -> ?EMPTY;
or_empty(Leaf) -> Leaf.

leaf_1(_, _, ?FREE) -> ?EMPTY;
leaf_1(_, 0, Node) -> Node;
leaf_1(I, Shift, Node) ->
    leaf_1(I, Shift - ?LEVEL_BITS, element(((I bsr Shift) band ?MASK) + 1, Node)).

free(I, A) ->
    case probe(I, A) of
        ?FREE -> A;
        _ -> set(I, ?FREE, A)
    end.

%% frees every id >= W, keeps depth
truncate(W, A) ->
    {arena, Shift, Root, _, _} = settle(A),
    Root1 = case W bsr Shift < ?FANOUT of
        true -> cut(W, Shift, Root);
        false -> Root
    end,
    Top = max(W - 1, 0),
    {arena, Shift, Root1, Top bsr ?LEVEL_BITS, leaf(Top, Shift, Root1)}.

cut(_, _, ?FREE) -> ?FREE;
cut(W, 0, Node) -> cut_from((W band ?MASK) + 1, Node);
cut(W, Shift, Node) ->
    Ix = ((W bsr Shift) band ?MASK) + 1,
    setelement(Ix, cut_from(Ix + 1, Node),
               cut(W, Shift - ?LEVEL_BITS, element(Ix, Node))).

cut_from(Ix, Node) when Ix =< ?FANOUT -> cut_from(Ix + 1, setelement(Ix, Node, ?FREE));
cut_from(_, Node) -> Node.

%% ids below W whose cell differs between Old and New; New descends from Old
%% by set, so =:= settles shared subtrees by identity without walking them
diff_below(W, Old, New) ->
    {arena, OldShift, OldRoot, _, _} = settle(Old),
    {arena, NewShift, NewRoot, _, _} = settle(New),
    diff_node(OldRoot, descend(NewRoot, NewShift, OldShift), OldShift, 0, W, []).

descend(Node, Shift, Shift) -> Node;
descend(?FREE, _, _) -> ?FREE;
descend(Node, Shift, OldShift) ->
    descend(element(1, Node), Shift - ?LEVEL_BITS, OldShift).

diff_node(_, _, _, Base, W, Acc) when Base >= W -> Acc;
diff_node(A, B, Shift, Base, W, Acc) ->
    case A =:= B of
        true -> Acc;
        false when Shift =:= 0 ->
            diff_leaf(or_empty(A), or_empty(B), Base, 1, W, Acc);
        false -> diff_kids(or_empty(A), or_empty(B), Shift, Base, W, 1, Acc)
    end.

diff_kids(A, B, Shift, Base, W, Ix, Acc) when Ix =< ?FANOUT ->
    Acc1 = diff_node(element(Ix, A), element(Ix, B), Shift - ?LEVEL_BITS,
                     Base + ((Ix - 1) bsl Shift), W, Acc),
    diff_kids(A, B, Shift, Base, W, Ix + 1, Acc1);
diff_kids(_, _, _, _, _, _, Acc) -> Acc.

diff_leaf(A, B, Base, Ix, W, Acc) when Ix =< ?FANOUT, Base + Ix - 1 < W ->
    case element(Ix, A) =:= element(Ix, B) of
        true -> diff_leaf(A, B, Base, Ix + 1, W, Acc);
        false -> diff_leaf(A, B, Base, Ix + 1, W, [Base + Ix - 1 | Acc])
    end;
diff_leaf(_, _, _, _, _, Acc) -> Acc.

settle({arena, Shift, Root, HotIx, Hot}) ->
    {Shift1, Root1} = put_leaf(HotIx bsl ?LEVEL_BITS, Hot, Shift, Root),
    {arena, Shift1, Root1, HotIx, Hot}.

fold(Fun, Acc, A) ->
    {arena, Shift, Root, _, _} = settle(A),
    fold_1(Fun, Acc, Root, Shift, 0).

fold_1(_, Acc, ?FREE, _, _) -> Acc;
fold_1(Fun, Acc, Node, 0, Base) -> fold_leaf(Fun, Acc, Node, Base, 1);
fold_1(Fun, Acc, Node, Shift, Base) -> fold_node(Fun, Acc, Node, Shift, Base, 1).

fold_leaf(Fun, Acc, Leaf, Base, Ix) when Ix =< ?FANOUT ->
    case element(Ix, Leaf) of
        ?FREE -> fold_leaf(Fun, Acc, Leaf, Base, Ix + 1);
        V -> fold_leaf(Fun, Fun(Base + Ix - 1, V, Acc), Leaf, Base, Ix + 1)
    end;
fold_leaf(_, Acc, _, _, _) -> Acc.

fold_node(Fun, Acc, Node, Shift, Base, Ix) when Ix =< ?FANOUT ->
    Acc1 = fold_1(Fun, Acc, element(Ix, Node), Shift - ?LEVEL_BITS,
                  Base + ((Ix - 1) bsl Shift)),
    fold_node(Fun, Acc1, Node, Shift, Base, Ix + 1);
fold_node(_, Acc, _, _, _, _) -> Acc.

count(A) -> fold(fun(_, _, K) -> K + 1 end, 0, A).

from_descending([]) -> new();
from_descending([{Top, _} | _] = Cells) ->
    {arena, Shift, Root} = build(level(lists:reverse(Cells), []), 0),
    {arena, Shift, Root, Top bsr ?LEVEL_BITS, leaf(Top, Shift, Root)}.

build([{0, Root}], Shift) -> {arena, Shift, Root};
build(Nodes, Shift) -> build(level(Nodes, []), Shift + ?LEVEL_BITS).

level([], Acc) -> lists:reverse(Acc);
level([{Ix, _} | _] = L, Acc) ->
    P = Ix bsr ?LEVEL_BITS,
    {Elems, Rest} = take(L, P bsl ?LEVEL_BITS, 0, []),
    level(Rest, [{P, list_to_tuple(Elems)} | Acc]).

take(L, _, ?FANOUT, Acc) -> {lists:reverse(Acc), L};
take([{Ix, X} | Rest], Base, K, Acc) when Ix =:= Base + K ->
    take(Rest, Base, K + 1, [X | Acc]);
take(L, Base, K, Acc) -> take(L, Base, K + 1, [?FREE | Acc]).
