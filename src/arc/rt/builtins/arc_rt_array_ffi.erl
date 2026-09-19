%% array kernels; exports may answer miss
-module(arc_rt_array_ffi).
-export([index_range_plain/5, scan_forward/5, scan_backward/4, push/3, pop/2]).

-include("../arc_rt_layout.hrl").
-compile({inline, [elem_at/2]}).

elem_at(Els, Idx) -> ?ELEM_AT(Els, Idx).

index_range_plain(St, Props, Proto, Start, Count) ->
    (not props_have_index(Props, Start, Count))
        andalso chain_index_range_plain(element(?STORE_CELLS, element(?AGENT_STORE, St)),
                           Proto, Start, Count).

chain_index_range_plain(_, ?NONE, _, _) -> true;
chain_index_range_plain(Cells, {?SOME, {?HANDLE_TAG, Id}}, Start, Count) ->
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length}, Proto, _, _, _, _}
          when Start >= Length ->
            chain_index_range_plain(Cells, Proto, Start, Count);
        {?SOBJECT_TAG, Kind, Proto, Props, _, Els, _} ->
            case Kind of
                _ when element(1, Kind) =:= ?PROXYOBJ_TAG -> false;
                {?STRINGOBJ_TAG, S} when S =/= <<>> -> false;
                _ ->
                    (Els =:= ?ELEMS_NONE
                        orelse not elements_have_index(Els, Start, Count))
                    andalso (not props_have_index(Props, Start, Count))
                    andalso chain_index_range_plain(Cells, Proto, Start, Count)
            end;
        Shaped when element(1, Shaped) =:= ?SSHAPEDOBJECT_TAG ->
            chain_index_range_plain(Cells, element(?SSHAPEDOBJECT_PROTO, Shaped), Start, Count);
        _ -> true
    end.

props_have_index(Props, Start, 1) -> is_map_key({?KEY_INDEX, Start}, Props);
props_have_index(_, _, Count) when Count =< 0 -> false;
props_have_index(Props, _, _) when map_size(Props) =:= 0 -> false;
props_have_index(Props, _, Count) when Count > 4 -> any_index_key(maps:next(maps:iterator(Props)));
props_have_index(Props, Start, Count) -> probe_keys(Props, Start, Start + Count).

any_index_key(none) -> false;
any_index_key({{?KEY_INDEX, _}, _, _}) -> true;
any_index_key({_, _, I}) -> any_index_key(maps:next(I)).

probe_keys(_, Idx, End) when Idx >= End -> false;
probe_keys(Props, Idx, End) ->
    is_map_key({?KEY_INDEX, Idx}, Props) orelse probe_keys(Props, Idx + 1, End).

elements_have_index({?ELEMS_DENSE, A}, Start, Count) ->
    arc_tree_array_ffi:size(A) > 0
        andalso (Count > 64 orelse probe_dense(A, Start, Start + Count));
elements_have_index({?ELEMS_SPARSE, M}, Start, Count) ->
    map_size(M) > 0
        andalso (Count > 64 orelse probe_sparse(M, Start, Start + Count));
elements_have_index(_, _, _) -> false.

probe_dense(_, Idx, End) when Idx >= End -> false;
probe_dense(A, Idx, End) ->
    arc_tree_array_ffi:get_or_hole(Idx, A) =/= ?ELEMS_HOLE orelse probe_dense(A, Idx + 1, End).

probe_sparse(_, Idx, End) when Idx >= End -> false;
probe_sparse(M, Idx, End) ->
    is_map_key(Idx, M) orelse probe_sparse(M, Idx + 1, End).

scan_forward(_, _, Idx, End, _) when Idx >= End -> absent;
scan_forward(Els, Search, Idx, End, Eq) ->
    case elem_at(Els, Idx) of
        ?ELEMS_HOLE -> {hole_at, Idx};
        V ->
            case eq(Eq, V, Search) of
                true -> {match, Idx};
                false -> scan_forward(Els, Search, Idx + 1, End, Eq)
            end
    end.

scan_backward(_, _, Idx, _) when Idx < 0 -> absent;
scan_backward(Els, Search, Idx, Eq) ->
    case elem_at(Els, Idx) of
        ?ELEMS_HOLE -> {hole_at, Idx};
        V ->
            case eq(Eq, V, Search) of
                true -> {match, Idx};
                false -> scan_backward(Els, Search, Idx - 1, Eq)
            end
    end.

eq(strict, A, B) -> arc_rt_val_ffi:strict_eq(A, B);
eq(same_value_zero, A, B) -> arc_rt_val_ffi:same_value_zero(A, B).

%% plain extensible array with no own props, free proto chain
push(St, {?HANDLE_TAG, Id}, Args) ->
    Store = element(?AGENT_STORE, St),
    Cells = element(?STORE_CELLS, Store),
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, Proto, Props, Sym, Els, true}
          when Props =:= #{} ->
            N = length(Args),
            NewLen = Len + N,
            case NewLen =< ?MAX_DENSE_INDEX
                 andalso chain_index_range_plain(Cells, Proto, Len, N) of
                false -> push_miss;
                true ->
                    case append(Els, Len, Args) of
                        miss -> push_miss;
                        NewEls ->
                            Cell = {?SOBJECT_TAG, {?ARRAYOBJ_TAG, NewLen}, Proto, Props,
                                    Sym, NewEls, true},
                            {pushed, NewLen,
                             setelement(?AGENT_STORE, St,
                                        setelement(?STORE_CELLS, Store,
                                                   arc_rt_arena_ffi:set(Id, Cell, Cells)))}
                    end
            end;
        _ -> push_miss
    end;
push(_, _, _) -> push_miss.

append(?ELEMS_NONE, 0, Args) -> {?ELEMS_DENSE, arc_tree_array_ffi:from_list(Args)};
append(?ELEMS_NONE, Len, Args) when Len =< ?MAX_GAP ->
    {?ELEMS_DENSE, set_each(Args, Len, {})};
append({?ELEMS_DENSE, A}, Len, Args) ->
    case arc_tree_array_ffi:size(A) of
        Size when Size =< Len, Len - Size =< ?MAX_GAP ->
            {?ELEMS_DENSE, set_each(Args, Len, A)};
        _ -> miss
    end;
append(_, _, _) -> miss.

set_each([V | Vs], I, A) -> set_each(Vs, I + 1, arc_tree_array_ffi:set(I, V, A));
set_each([], _, A) -> A.

pop(St, {?HANDLE_TAG, Id}) ->
    Store = element(?AGENT_STORE, St),
    Cells = element(?STORE_CELLS, Store),
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, Proto, Props, Sym, {?ELEMS_DENSE, A}, true}
          when Props =:= #{}, Len > 0 ->
            Last = Len - 1,
            case arc_tree_array_ffi:size(A) =:= Len
                 andalso arc_tree_array_ffi:get_or_hole(Last, A) of
                false -> pop_miss;
                ?ELEMS_HOLE -> pop_miss;
                V ->
                    Cell = {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Last}, Proto, Props, Sym,
                            {?ELEMS_DENSE, arc_tree_array_ffi:resize(A, Last)}, true},
                    {popped, V,
                     setelement(?AGENT_STORE, St,
                                setelement(?STORE_CELLS, Store,
                                           arc_rt_arena_ffi:set(Id, Cell, Cells)))}
            end;
        _ -> pop_miss
    end;
pop(_, _) -> pop_miss.
