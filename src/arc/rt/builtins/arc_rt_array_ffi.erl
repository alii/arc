%%% arc_rt_array_ffi — alloc-free wire-term kernels the Array builtins scan
%%% elements with (arc/rt/builtins/array.gleam, helpers.gleam). Pure term
%%% work over the threaded `St`: no NIF, no process state.
-module(arc_rt_array_ffi).
-export([own_element/3, arg_list/2, index_free/5, scan_forward/5, scan_backward/4]).

-include("../arc_rt_layout.hrl").

%% own_element(St, Obj, Idx) -> {hit, V} | slow
%% The `OwnIndexValue` arm of rt_obj.t_get_own_index: the receiver is an
%% Array or Arguments SObject with no own Index(Idx) property override and a
%% present element at Idx. Everything else (holes, accessors, proxies,
%% typed arrays, primitives) answers `slow` for the Gleam path.
own_element(St, {?HANDLE_TAG, Id}, Idx) when is_integer(Idx), Idx >= 0 ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, St))) of
        {?SOBJECT_TAG, Kind, _, Props, _, Els, _}
          when element(1, Kind) =:= ?ARRAYOBJ_TAG;
               element(1, Kind) =:= ?ARGUMENTSOBJ_TAG ->
            case Props of
                #{{?KEY_INDEX, Idx} := _} -> slow;
                _ ->
                    %% The elements array is never fixed, so a read
                    %% past its size answers the hole default.
                    case Els of
                        {?ELEMS_DENSE, A} ->
                            case array:get(Idx, A) of
                                ?ELEMS_HOLE -> slow;
                                V -> {hit, V}
                            end;
                        {?ELEMS_SPARSE, #{Idx := ?ELEMS_HOLE}} -> slow;
                        {?ELEMS_SPARSE, #{Idx := V}} -> {hit, V};
                        _ -> slow
                    end
            end;
        _ -> slow
    end;
own_element(_, _, _) -> slow.

elem_read({?ELEMS_DENSE, A}, Idx) -> array:get(Idx, A);
elem_read({?ELEMS_SPARSE, M}, Idx) ->
    case M of
        #{Idx := V} -> V;
        _ -> ?ELEMS_HOLE
    end;
elem_read(_, _) -> ?ELEMS_HOLE.

%% index_free(St, Props, Proto, Start, Count) -> boolean()
%% Nothing intercepts a plain element read or write at the indices
%% [Start, Start+Count) of an object with own `Props` and prototype `Proto`:
%% no own Index key there, and up the chain only ordinary cells with no such
%% key and no element there. A proxy or a non-empty String wrapper on the
%% chain, whose indices are not in its props, answers false. A handful of
%% indices are probed one by one; a wider range asks the cheaper superset
%% question (any Index key at all, any element at all past 64).
index_free(St, Props, Proto, Start, Count) ->
    (not props_have_index(Props, Start, Count))
        andalso chain_free(element(?STORE_DATA, element(?AGENT_STORE, St)),
                           Proto, Start, Count).

chain_free(_, ?NONE, _, _) -> true;
chain_free(Data, {?SOME, {?HANDLE_TAG, Id}}, Start, Count) ->
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SOBJECT_TAG, Kind, Proto, Props, _, Els, _} ->
            case Kind of
                _ when element(1, Kind) =:= ?PROXYOBJ_TAG -> false;
                {?STRINGOBJ_TAG, S} when S =/= <<>> -> false;
                _ ->
                    (Els =:= ?ELEMS_NONE
                        orelse not elements_have_index(Els, Start, Count))
                    andalso (not props_have_index(Props, Start, Count))
                    andalso chain_free(Data, Proto, Start, Count)
            end;
        Shaped when element(1, Shaped) =:= ?SSHAPED_TAG ->
            chain_free(Data, element(?SSHAPED_PROTO, Shaped), Start, Count);
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
    array:sparse_size(A) > 0
        andalso (Count > 64 orelse probe_dense(A, Start, Start + Count));
elements_have_index({?ELEMS_SPARSE, M}, Start, Count) ->
    map_size(M) > 0
        andalso (Count > 64 orelse probe_sparse(M, Start, Start + Count));
elements_have_index(_, _, _) -> false.

probe_dense(_, Idx, End) when Idx >= End -> false;
probe_dense(A, Idx, End) ->
    array:get(Idx, A) =/= ?ELEMS_HOLE orelse probe_dense(A, Idx + 1, End).

probe_sparse(_, Idx, End) when Idx >= End -> false;
probe_sparse(M, Idx, End) ->
    is_map_key(Idx, M) orelse probe_sparse(M, Idx + 1, End).
%% arg_list(St, Obj) -> {args_hit, [V]} | args_slow
%% §7.3.19 CreateListFromArrayLike when every step is a plain read: an
%% Array with no property overrides, or an Arguments object whose only
%% named props are its own "length" (a data property still equal to the
%% element count) and "callee", holding a hole-free dense prefix of that
%% length. Anything else answers `args_slow` for the [[Get]]-per-index path.
-define(LENGTH_KEY, {?KEY_NAMED, <<"length">>}).
-define(CALLEE_KEY, {?KEY_NAMED, <<"callee">>}).
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
        _ -> args_slow
    end;
arg_list(_, _) -> args_slow.

dense_prefix(_, 0) -> {args_hit, []};
dense_prefix({?ELEMS_DENSE, A}, Len) when Len > 0 ->
    case array:size(A) of
        Len -> hole_free(array:to_list(A));
        Size when Size > Len -> hole_free(lists:sublist(array:to_list(A), Len));
        _ -> args_slow
    end;
dense_prefix(_, _) -> args_slow.

hole_free(L) ->
    case lists:member(?ELEMS_HOLE, L) of
        true -> args_slow;
        false -> {args_hit, L}
    end.

%% scan_forward(Els, Search, Idx, End, Eq) -> {match, I} | {hole_at, I} | absent
%% Compare present elements in [Idx, End) against Search with `strict` /
%% `same_value_zero`, stopping at the first match or the first hole (the
%% caller decides what a hole means and resumes past it).
scan_forward(_, _, Idx, End, _) when Idx >= End -> absent;
scan_forward(Els, Search, Idx, End, Eq) ->
    case elem_read(Els, Idx) of
        ?ELEMS_HOLE -> {hole_at, Idx};
        V ->
            case eq(Eq, V, Search) of
                true -> {match, Idx};
                false -> scan_forward(Els, Search, Idx + 1, End, Eq)
            end
    end.

%% scan_backward(Els, Search, Idx, Eq) -> {match, I} | {hole_at, I} | absent
%% The lastIndexOf mirror: [Idx, 0] descending.
scan_backward(_, _, Idx, _) when Idx < 0 -> absent;
scan_backward(Els, Search, Idx, Eq) ->
    case elem_read(Els, Idx) of
        ?ELEMS_HOLE -> {hole_at, Idx};
        V ->
            case eq(Eq, V, Search) of
                true -> {match, Idx};
                false -> scan_backward(Els, Search, Idx - 1, Eq)
            end
    end.

eq(strict, A, B) -> arc_rt_val_ffi:strict_eq(A, B);
eq(same_value_zero, A, B) -> arc_rt_val_ffi:same_value_zero(A, B).
