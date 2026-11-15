%% the arena's layout and its read, for kernels hot enough to want it inlined.
%% arc_rt_arena_ffi describes the layout and owns the writes, but for the two
%% that arc_interp_thread_props_ffi makes into the hot leaf

-ifndef(ARC_RT_ARENA_HRL).
-define(ARC_RT_ARENA_HRL, true).

-define(ARENA(LastId, Last, HotIx, Hot, WarmIx, Warm, Low, Shift, Root),
        {LastId, Last, HotIx, Hot, WarmIx, Warm, Low, Shift, Root}).
%% how many leaves low holds
-define(LOW_LEAVES, 128).
-define(LEAF_BITS, 4).
-define(LEAF_MASK, 15).
-define(NODE_BITS, 5).
-define(NODE_MASK, 31).
%% the collector's marks come a word to two leaves
-define(MARK_BITS, 5).
-define(MARK_MASK, 31).
%% far past any real id; the bound tells the compiler an id is never a bignum
-define(IS_CELL_ID(I), is_integer(I), 0 =< I, I =< 16#3FFFFFFFFFFFF).
-define(IN_LEAF(I, Leaf), element(((I) band ?LEAF_MASK) + 1, Leaf)).
-define(KID(I, Shift, Node), element((((I) bsr (Shift)) band ?NODE_MASK) + 1, Node)).

-compile({inline, [arena_get/2]}).
-compile({nowarn_unused_function, [arena_get/2]}).
%% early cells sit down the left edge however deep the trie has grown, so
%% those skip the indexed steps
arena_get(I, ?ARENA(LastId, Last, HotIx, Hot, WarmIx, Warm, Low, Shift, Root))
  when ?IS_CELL_ID(I) ->
    case I bsr ?LEAF_BITS of
        HotIx when I =:= LastId -> Last;
        HotIx -> ?IN_LEAF(I, Hot);
        WarmIx -> ?IN_LEAF(I, Warm);
        Ix when Ix < ?LOW_LEAVES -> ?IN_LEAF(I, element(Ix + 1, Low));
        _ ->
            Node9 = case Shift of
                9 -> Root;
                14 when I < 16#4000 -> element(1, Root);
                14 -> ?KID(I, 14, Root);
                19 when I < 16#4000 -> element(1, element(1, Root));
                19 -> ?KID(I, 14, ?KID(I, 19, Root));
                24 when I < 16#4000 -> element(1, element(1, element(1, Root)));
                24 -> ?KID(I, 14, ?KID(I, 19, ?KID(I, 24, Root)));
                29 -> ?KID(I, 14, ?KID(I, 19, ?KID(I, 24, ?KID(I, 29, Root))));
                34 ->
                    ?KID(I, 14, ?KID(I, 19, ?KID(I, 24, ?KID(I, 29, ?KID(I, 34, Root)))));
                39 ->
                    ?KID(I, 14, ?KID(I, 19, ?KID(I, 24, ?KID(I, 29, ?KID(I, 34,
                         ?KID(I, 39, Root))))))
            end,
            ?IN_LEAF(I, ?KID(I, 4, ?KID(I, 9, Node9)))
    end.

-endif.
