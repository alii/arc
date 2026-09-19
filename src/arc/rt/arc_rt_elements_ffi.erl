%% element kernels; exports may answer miss
-module(arc_rt_elements_ffi).
-export([own_element/3, arg_list/2]).

-include("arc_rt_layout.hrl").
-compile({inline, [own_read/2, elem_at/2]}).

own_element(St, {?HANDLE_TAG, Id}, Idx) when is_integer(Idx), Idx >= 0 ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_CELLS, element(?AGENT_STORE, St))) of
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

-define(CALLEE_KEY, {?KEY_NAMED, <<"callee">>}).

%% §7.3.19 createlistfromarraylike for plain arrays and arguments, else miss
arg_list(St, {?HANDLE_TAG, Id}) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_CELLS, Store)) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, Props, _, Els, _}
          when map_size(Props) =:= 0 ->
            dense_prefix(Els, Len);
        {?SOBJECT_TAG, {?ARGUMENTSOBJ_TAG, _, _}, _,
         #{?LENGTH_KEY := LenProp, ?CALLEE_KEY := _} = Props, _, Els, _}
          when map_size(Props) =:= 2,
               element(1, LenProp) =:= ?DATAPROPERTY_TAG,
               is_integer(element(?DATAPROPERTY_VALUE, LenProp)) ->
            dense_prefix(Els, element(?DATAPROPERTY_VALUE, LenProp));
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
