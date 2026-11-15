%% the arena is a persistent trie keyed by cell id: leaves of 16 cells under nodes of 32 kids
%% shift is how many id bits sit below the root's own five, and is never under 9
%% the first leaves, which hold the builtins and what a program makes first and reads most,
%% sit in low instead, one step from the top
%% hot is the last written leaf and warm the one before it; their other copies are stale
%% last is the last written cell, which belongs in hot and is stale there
%% get/2 crashes on ids never minted, probe/2 is total
-module(arc_rt_arena_ffi).
-export([new/0, get/2, get_option/2, probe/2, set/3, free/2, fold/3,
         count/1, keep_marked/4, diff_below/3]).

-include("arc_rt_layout.hrl").
-include("arc_rt_arena.hrl").

-define(LEAF_SIZE, 16).
-define(NODE_SIZE, 32).
-define(MIN_SHIFT, 9).
%% the index of a warm slot that holds no leaf
-define(NO_LEAF, -1).
-define(FREE, ?STORE_FREE_CELL).
-define(EMPTY_LEAF,
        {?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE,
         ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE}).
-define(EMPTY_NODE,
        {?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE,
         ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE,
         ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE,
         ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE, ?FREE}).

new() -> ?ARENA(0, ?FREE, 0, ?EMPTY_LEAF, ?NO_LEAF, ?FREE, empty_low(), ?MIN_SHIFT, ?EMPTY_NODE).

empty_low() -> erlang:make_tuple(?LOW_LEAVES, ?EMPTY_LEAF).

get(I, A) -> arena_get(I, A).

probe(I, ?ARENA(LastId, Last, HotIx, Hot, WarmIx, Warm, Low, Shift, Root))
  when ?IS_CELL_ID(I) ->
    case I bsr ?LEAF_BITS of
        HotIx when I =:= LastId -> Last;
        HotIx -> ?IN_LEAF(I, Hot);
        WarmIx -> ?IN_LEAF(I, Warm);
        Ix when Ix < ?LOW_LEAVES -> ?IN_LEAF(I, element(Ix + 1, Low));
        _ -> ?IN_LEAF(I, leaf(I, Shift, Root))
    end;
probe(_, _) -> ?FREE.

get_option(I, A) ->
    case probe(I, A) of
        ?FREE -> ?NONE;
        V -> {?SOME, V}
    end.

set(I, V, ?ARENA(I, _, HotIx, Hot, WarmIx, Warm, Low, Shift, Root)) ->
    ?ARENA(I, V, HotIx, Hot, WarmIx, Warm, Low, Shift, Root);
set(I, V, ?ARENA(LastId, Last, HotIx, Hot, WarmIx, Warm, Low, Shift, Root))
  when ?IS_CELL_ID(I), ?IS_CELL_ID(LastId) ->
    Hot1 = set16(LastId band ?LEAF_MASK, Hot, Last),
    case I bsr ?LEAF_BITS of
        HotIx -> ?ARENA(I, V, HotIx, Hot1, WarmIx, Warm, Low, Shift, Root);
        WarmIx -> ?ARENA(I, V, WarmIx, Warm, HotIx, Hot1, Low, Shift, Root);
        %% warm goes back where it came from to make room
        Ix when WarmIx < ?LOW_LEAVES ->
            ?ARENA(I, V, Ix, leaf(I, Low, Shift, Root), HotIx, Hot1, put_low(WarmIx, Warm, Low),
                   Shift, Root);
        Ix ->
            {Shift1, Root1} = put_leaf(WarmIx, Warm, Shift, Root),
            ?ARENA(I, V, Ix, leaf(I, Low, Shift1, Root1), HotIx, Hot1, Low, Shift1, Root1)
    end.

put_low(?NO_LEAF, _, Low) -> Low;
put_low(Ix, Leaf, Low) -> setelement(Ix + 1, Low, Leaf).

%% literal positions compile to an in-place copy, not a bif call
-compile({inline, [set16/3]}).
set16(I, Leaf, V) when tuple_size(Leaf) =:= ?LEAF_SIZE ->
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

set32(I, Node, V) when tuple_size(Node) =:= ?NODE_SIZE ->
    case I of
        0 -> setelement(1, Node, V);
        1 -> setelement(2, Node, V);
        2 -> setelement(3, Node, V);
        3 -> setelement(4, Node, V);
        4 -> setelement(5, Node, V);
        5 -> setelement(6, Node, V);
        6 -> setelement(7, Node, V);
        7 -> setelement(8, Node, V);
        8 -> setelement(9, Node, V);
        9 -> setelement(10, Node, V);
        10 -> setelement(11, Node, V);
        11 -> setelement(12, Node, V);
        12 -> setelement(13, Node, V);
        13 -> setelement(14, Node, V);
        14 -> setelement(15, Node, V);
        15 -> setelement(16, Node, V);
        16 -> setelement(17, Node, V);
        17 -> setelement(18, Node, V);
        18 -> setelement(19, Node, V);
        19 -> setelement(20, Node, V);
        20 -> setelement(21, Node, V);
        21 -> setelement(22, Node, V);
        22 -> setelement(23, Node, V);
        23 -> setelement(24, Node, V);
        24 -> setelement(25, Node, V);
        25 -> setelement(26, Node, V);
        26 -> setelement(27, Node, V);
        27 -> setelement(28, Node, V);
        28 -> setelement(29, Node, V);
        29 -> setelement(30, Node, V);
        30 -> setelement(31, Node, V);
        _ -> setelement(32, Node, V)
    end.

%% writes the leaf numbered Ix back, growing the trie to reach it
put_leaf(Ix, Leaf, Shift, Root) when Ix bsr (Shift - ?LEAF_BITS) < ?NODE_SIZE ->
    {Shift, put_leaf_walk(Ix bsl ?LEAF_BITS, Leaf, Shift, Root)};
put_leaf(Ix, Leaf, Shift, Root) ->
    put_leaf(Ix, Leaf, Shift + ?NODE_BITS, setelement(1, ?EMPTY_NODE, Root)).

put_leaf_walk(I, Leaf, Shift, ?FREE) -> put_leaf_walk(I, Leaf, Shift, ?EMPTY_NODE);
put_leaf_walk(I, Leaf, ?LEAF_BITS, Node) ->
    set32((I bsr ?LEAF_BITS) band ?NODE_MASK, Node, Leaf);
put_leaf_walk(I, Leaf, Shift, Node) ->
    Ix = (I bsr Shift) band ?NODE_MASK,
    set32(Ix, Node, put_leaf_walk(I, Leaf, Shift - ?NODE_BITS, element(Ix + 1, Node))).

leaf(I, Low, _, _) when I bsr ?LEAF_BITS < ?LOW_LEAVES -> element((I bsr ?LEAF_BITS) + 1, Low);
leaf(I, _, Shift, Root) -> leaf(I, Shift, Root).

%% the trie's copy of the leaf holding I, empty when it has none
leaf(I, Shift, Root) when I bsr Shift < ?NODE_SIZE -> leaf_walk(I, Shift, Root);
leaf(_, _, _) -> ?EMPTY_LEAF.

leaf_walk(I, 9, Node) when is_tuple(Node) ->
    case ?KID(I, 9, Node) of
        ?FREE -> ?EMPTY_LEAF;
        Node4 ->
            case ?KID(I, 4, Node4) of
                ?FREE -> ?EMPTY_LEAF;
                Leaf -> Leaf
            end
    end;
leaf_walk(I, Shift, Node) when is_tuple(Node) ->
    leaf_walk(I, Shift - ?NODE_BITS, ?KID(I, Shift, Node));
leaf_walk(_, _, ?FREE) -> ?EMPTY_LEAF.

free(I, A) ->
    case probe(I, A) of
        ?FREE -> A;
        _ -> set(I, ?FREE, A)
    end.

%% frees the ids from W up that Marks lacks, see arc_rt_gc_ffi:mark/4, and is
%% ready for writes at Next
keep_marked(W, Marks, Next, A) ->
    {Low, Shift, Root} = settle(A),
    Low1 = case W bsr ?LEAF_BITS < ?LOW_LEAVES of
        true ->
            list_to_tuple([or_empty(Leaf, ?EMPTY_LEAF)
                           || Leaf <- sweep_kids(tuple_to_list(Low), 0, 0, W, Marks)]);
        false -> Low
    end,
    {Shift1, Root1} = shallow(Shift, or_empty(sweep(Root, Shift, 0, W, Marks), ?EMPTY_NODE)),
    settled(Next, Low1, Shift1, Root1).

sweep(?FREE, _, _, _, _) -> ?FREE;
sweep(Leaf, 0, Base, W, _) when Base + ?LEAF_SIZE =< W -> Leaf;
sweep(Leaf, 0, Base, W, Marks) ->
    Ix = Base bsr ?MARK_BITS,
    Marked = case Marks of
        #{Ix := Bits} -> (Bits bsr (Base band ?MARK_MASK)) band 16#FFFF;
        _ -> 0
    end,
    case Marked bor ((1 bsl max(W - Base, 0)) - 1) of
        0 -> ?FREE;
        16#FFFF -> Leaf;
        Kept -> keep_cells(Leaf, Kept)
    end;
sweep(Node, Shift, Base, W, _) when Base + (?NODE_SIZE bsl Shift) =< W -> Node;
sweep(Node, Shift, Base, W, Marks) ->
    case list_to_tuple(sweep_kids(tuple_to_list(Node), below(Shift), Base, W, Marks)) of
        ?EMPTY_NODE -> ?FREE;
        Node1 -> Node1
    end.

sweep_kids([Kid | Kids], Shift, Base, W, Marks) ->
    Span = case Shift of
        0 -> ?LEAF_SIZE;
        _ -> ?NODE_SIZE bsl Shift
    end,
    [sweep(Kid, Shift, Base, W, Marks) | sweep_kids(Kids, Shift, Base + Span, W, Marks)];
sweep_kids([], _, _, _, _) -> [].

-define(KEPT(I, Cell), (if Kept band (1 bsl I) =:= 0 -> ?FREE; true -> Cell end)).
keep_cells({A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P}, Kept) when is_integer(Kept) ->
    {?KEPT(0, A), ?KEPT(1, B), ?KEPT(2, C), ?KEPT(3, D), ?KEPT(4, E), ?KEPT(5, F),
     ?KEPT(6, G), ?KEPT(7, H), ?KEPT(8, I), ?KEPT(9, J), ?KEPT(10, K), ?KEPT(11, L),
     ?KEPT(12, M), ?KEPT(13, N), ?KEPT(14, O), ?KEPT(15, P)}.

%% drops the levels only the freed ids needed
shallow(Shift, Root) when Shift > ?MIN_SHIFT ->
    case setelement(1, Root, ?FREE) of
        ?EMPTY_NODE -> shallow(Shift - ?NODE_BITS, or_empty(element(1, Root), ?EMPTY_NODE));
        _ -> {Shift, Root}
    end;
shallow(Shift, Root) -> {Shift, Root}.

%% an arena with nothing stale in it, ready for writes near Top
settled(Top, Low, Shift, Root) ->
    Leaf = leaf(Top, Low, Shift, Root),
    ?ARENA(Top, ?IN_LEAF(Top, Leaf), Top bsr ?LEAF_BITS, Leaf, ?NO_LEAF, ?FREE, Low, Shift, Root).

%% shift 0 is a leaf, and a leaf's parent is at shift 4
below(?LEAF_BITS) -> 0;
below(Shift) -> Shift - ?NODE_BITS.

%% ids below W whose cell differs; =:= skips shared subtrees by identity
diff_below(W, Old, New) ->
    {OldLow, OldShift, OldRoot} = settle(Old),
    {NewLow, NewShift, NewRoot} = settle(New),
    Shift = min(OldShift, NewShift),
    diff_node(descend(OldRoot, OldShift, Shift), descend(NewRoot, NewShift, Shift), Shift, 0, W,
              diff_low(OldLow, NewLow, 1, W, [])).

diff_low(A, B, Ix, W, Acc) when Ix =< ?LOW_LEAVES ->
    Acc1 = diff_node(element(Ix, A), element(Ix, B), 0, (Ix - 1) bsl ?LEAF_BITS, W, Acc),
    diff_low(A, B, Ix + 1, W, Acc1);
diff_low(_, _, _, _, Acc) -> Acc.

descend(Node, Shift, Shift) -> Node;
descend(?FREE, _, _) -> ?FREE;
descend(Node, Shift, To) -> descend(element(1, Node), Shift - ?NODE_BITS, To).

diff_node(_, _, _, Base, W, Acc) when Base >= W -> Acc;
diff_node(A, B, Shift, Base, W, Acc) ->
    case A =:= B of
        true -> Acc;
        false when Shift =:= 0 ->
            diff_leaf(or_empty(A, ?EMPTY_LEAF), or_empty(B, ?EMPTY_LEAF), Base, 1, W, Acc);
        false ->
            diff_kids(or_empty(A, ?EMPTY_NODE), or_empty(B, ?EMPTY_NODE), Shift, Base, W,
                      1, Acc)
    end.

or_empty(?FREE, Empty) -> Empty;
or_empty(T, _) -> T.

diff_kids(A, B, Shift, Base, W, Ix, Acc) when Ix =< ?NODE_SIZE ->
    Acc1 = diff_node(element(Ix, A), element(Ix, B), below(Shift),
                     Base + ((Ix - 1) bsl Shift), W, Acc),
    diff_kids(A, B, Shift, Base, W, Ix + 1, Acc1);
diff_kids(_, _, _, _, _, _, Acc) -> Acc.

diff_leaf(A, B, Base, Ix, W, Acc) when Ix =< ?LEAF_SIZE, Base + Ix - 1 < W ->
    case element(Ix, A) =:= element(Ix, B) of
        true -> diff_leaf(A, B, Base, Ix + 1, W, Acc);
        false -> diff_leaf(A, B, Base, Ix + 1, W, [Base + Ix - 1 | Acc])
    end;
diff_leaf(_, _, _, _, _, Acc) -> Acc.

%% low and the trie with everything cached written back
settle(?ARENA(LastId, Last, HotIx, Hot, WarmIx, Warm, Low, Shift, Root)) ->
    put_back(HotIx, set16(LastId band ?LEAF_MASK, Hot, Last),
             put_back(WarmIx, Warm, {Low, Shift, Root})).

put_back(Ix, Leaf, {Low, Shift, Root}) when Ix < ?LOW_LEAVES ->
    {put_low(Ix, Leaf, Low), Shift, Root};
put_back(Ix, Leaf, {Low, Shift, Root}) ->
    {Shift1, Root1} = put_leaf(Ix, Leaf, Shift, Root),
    {Low, Shift1, Root1}.

fold(Fun, Acc, A) ->
    {Low, Shift, Root} = settle(A),
    fold_subtree(Fun, fold_low(Fun, Acc, Low, 1), Root, Shift, 0).

fold_low(Fun, Acc, Low, Ix) when Ix =< ?LOW_LEAVES ->
    Acc1 = fold_leaf(Fun, Acc, element(Ix, Low), (Ix - 1) bsl ?LEAF_BITS, 1),
    fold_low(Fun, Acc1, Low, Ix + 1);
fold_low(_, Acc, _, _) -> Acc.

fold_subtree(_, Acc, ?FREE, _, _) -> Acc;
fold_subtree(Fun, Acc, Leaf, 0, Base) -> fold_leaf(Fun, Acc, Leaf, Base, 1);
fold_subtree(Fun, Acc, Node, Shift, Base) -> fold_node(Fun, Acc, Node, Shift, Base, 1).

fold_leaf(Fun, Acc, Leaf, Base, Ix) when Ix =< ?LEAF_SIZE ->
    case element(Ix, Leaf) of
        ?FREE -> fold_leaf(Fun, Acc, Leaf, Base, Ix + 1);
        V -> fold_leaf(Fun, Fun(Base + Ix - 1, V, Acc), Leaf, Base, Ix + 1)
    end;
fold_leaf(_, Acc, _, _, _) -> Acc.

fold_node(Fun, Acc, Node, Shift, Base, Ix) when Ix =< ?NODE_SIZE ->
    Acc1 = fold_subtree(Fun, Acc, element(Ix, Node), below(Shift),
                        Base + ((Ix - 1) bsl Shift)),
    fold_node(Fun, Acc1, Node, Shift, Base, Ix + 1);
fold_node(_, Acc, _, _, _, _) -> Acc.

count(A) -> fold(fun(_, _, K) -> K + 1 end, 0, A).
