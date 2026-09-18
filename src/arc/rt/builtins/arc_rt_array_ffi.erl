%% array kernels; exports may answer miss
-module(arc_rt_array_ffi).
-export([own_element/3, arg_list/2, index_range_plain/5, scan_forward/5, scan_backward/4,
         push/3, pop/2]).

-include("../arc_rt_layout.hrl").
-compile({inline, [own_read/2, elem_at/2]}).

own_element(St, {?HANDLE_TAG, Id}, Idx) when is_integer(Idx), Idx >= 0 ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, St))) of
        {?SOBJECT_TAG, Kind, _, Props, _, Els, _}
          when element(1, Kind) =:= ?ARRAYOBJ_TAG;
               element(1, Kind) =:= ?ARGUMENTSOBJ_TAG ->
            if
                Props =:= #{} -> own_read(Els, Idx);
                is_map_key({?KEY_INDEX, Idx}, Props) -> miss;
                true -> own_read(Els, Idx)
            end;
        _ -> miss
    end;
own_element(_, _, _) -> miss.

own_read({?ELEMS_DENSE, T}, Idx)
  when element(1, T) =/= ?VEC_TAG, Idx < tuple_size(T) ->
    case element(Idx + 1, T) of
        ?ELEMS_HOLE -> miss;
        V -> {hit, V}
    end;
own_read(Els, Idx) ->
    case elem_at(Els, Idx) of
        ?ELEMS_HOLE -> miss;
        V -> {hit, V}
    end.

elem_at(Els, Idx) -> ?ELEM_AT(Els, Idx).

index_range_plain(St, Props, Proto, Start, Count) ->
    (not props_have_index(Props, Start, Count))
        andalso chain_index_range_plain(element(?STORE_DATA, element(?AGENT_STORE, St)),
                           Proto, Start, Count).

chain_index_range_plain(_, ?NONE, _, _) -> true;
chain_index_range_plain(Data, {?SOME, {?HANDLE_TAG, Id}}, Start, Count) ->
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length}, Proto, _, _, _, _}
          when Start >= Length ->
            chain_index_range_plain(Data, Proto, Start, Count);
        {?SOBJECT_TAG, Kind, Proto, Props, _, Els, _} ->
            case Kind of
                _ when element(1, Kind) =:= ?PROXYOBJ_TAG -> false;
                {?STRINGOBJ_TAG, S} when S =/= <<>> -> false;
                _ ->
                    (Els =:= ?ELEMS_NONE
                        orelse not elements_have_index(Els, Start, Count))
                    andalso (not props_have_index(Props, Start, Count))
                    andalso chain_index_range_plain(Data, Proto, Start, Count)
            end;
        Shaped when element(1, Shaped) =:= ?SSHAPED_TAG ->
            chain_index_range_plain(Data, element(?SSHAPED_PROTO, Shaped), Start, Count);
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
    arc_tree_array_ffi:get(Idx, A) =/= ?ELEMS_HOLE orelse probe_dense(A, Idx + 1, End).

probe_sparse(_, Idx, End) when Idx >= End -> false;
probe_sparse(M, Idx, End) ->
    is_map_key(Idx, M) orelse probe_sparse(M, Idx + 1, End).

-define(CALLEE_KEY, {?KEY_NAMED, <<"callee">>}).

%% §7.3.19 createlistfromarraylike for plain arrays and arguments, else miss
arg_list(St, {?HANDLE_TAG, Id}) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, Props, _, Els, _}
          when map_size(Props) =:= 0 ->
            dense_prefix(Els, Len);
        {?SOBJECT_TAG, {?ARGUMENTSOBJ_TAG, _, _}, _,
         #{?LENGTH_KEY := LenProp, ?CALLEE_KEY := _} = Props, _, Els, _}
          when map_size(Props) =:= 2,
               element(1, LenProp) =:= ?DATAPROP_TAG,
               is_integer(element(?DATAPROP_VALUE, LenProp)) ->
            dense_prefix(Els, element(?DATAPROP_VALUE, LenProp));
        _ -> miss
    end;
arg_list(_, _) -> miss.

dense_prefix(_, 0) -> {dense_args, []};
dense_prefix({?ELEMS_DENSE, A}, Len) when Len > 0 ->
    case arc_tree_array_ffi:size(A) of
        Len -> hole_free(arc_tree_array_ffi:to_list(A));
        Size when Size > Len ->
            hole_free(lists:sublist(arc_tree_array_ffi:to_list(A), Len));
        _ -> miss
    end;
dense_prefix(_, _) -> miss.

hole_free(L) ->
    case lists:member(?ELEMS_HOLE, L) of
        true -> miss;
        false -> {dense_args, L}
    end.

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
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, Proto, Props, Sym, Els, true}
          when Props =:= #{} ->
            N = length(Args),
            NewLen = Len + N,
            case NewLen =< ?MAX_DENSE_INDEX
                 andalso chain_index_range_plain(Data, Proto, Len, N) of
                false -> push_miss;
                true ->
                    case append(Els, Len, Args) of
                        miss -> push_miss;
                        NewEls ->
                            Cell = {?SOBJECT_TAG, {?ARRAYOBJ_TAG, NewLen}, Proto, Props,
                                    Sym, NewEls, true},
                            {pushed, NewLen,
                             setelement(?AGENT_STORE, St,
                                        setelement(?STORE_DATA, Store,
                                                   arc_rt_arena_ffi:set(Id, Cell, Data)))}
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
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, Proto, Props, Sym, {?ELEMS_DENSE, A}, true}
          when Props =:= #{}, Len > 0 ->
            Last = Len - 1,
            case arc_tree_array_ffi:size(A) =:= Len
                 andalso arc_tree_array_ffi:get(Last, A) of
                false -> pop_miss;
                ?ELEMS_HOLE -> pop_miss;
                V ->
                    Cell = {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Last}, Proto, Props, Sym,
                            {?ELEMS_DENSE, arc_tree_array_ffi:resize(A, Last)}, true},
                    {popped, V,
                     setelement(?AGENT_STORE, St,
                                setelement(?STORE_DATA, Store,
                                           arc_rt_arena_ffi:set(Id, Cell, Data)))}
            end;
        _ -> pop_miss
    end;
pop(_, _) -> pop_miss.
