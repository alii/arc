%% element kernels; exports may answer miss
-module(arc_rt_elements_ffi).
-export([own_element/3, arg_list/2]).

-include("arc_rt_layout.hrl").
-include("arc_rt_arena.hrl").
-compile({inline, [own_read/2, elem_at/2]}).

-define(HIT(Expr),
        case Expr of
            ?ELEMS_HOLE -> miss;
            V -> {hit, V}
        end).

own_element(St, {?HANDLE_TAG, Id}, Idx)
  when is_integer(Idx), Idx >= 0, Idx =< ?MAX_ARRAY_INDEX, tuple_size(St) =:= ?AGENT_SIZE ->
    case arena_get(Id, element(?AGENT_CELLS, St)) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, _, Props, _,
         {?ELEMS_DENSE, {?VEC_TAG, Size, Shift, Root, HotIx, _}} = Els, _}
          when Props =:= #{}, Idx < Size, Idx bsr 4 =/= HotIx ->
            case Shift of
                4 -> ?HIT(?IN_NODE(Idx, 0, ?IN_NODE(Idx, 4, Root)));
                8 -> ?HIT(?IN_NODE(Idx, 0, ?IN_NODE(Idx, 4, ?IN_NODE(Idx, 8, Root))));
                12 ->
                    ?HIT(?IN_NODE(Idx, 0, ?IN_NODE(Idx, 4, ?IN_NODE(Idx, 8,
                                                                    ?IN_NODE(Idx, 12, Root)))));
                _ -> own_read(Els, Idx)
            end;
        {?SOBJECT_TAG, Kind, _, Props, _, Els, _}
          when element(1, Kind) =:= ?ARRAYOBJ_TAG;
               element(1, Kind) =:= ?ARGUMENTSOBJ_TAG ->
            if
                Props =:= #{} -> own_read(Els, Idx);
                is_map_key(?INDEX_SLOT(Idx), Props) -> miss;
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


%% §7.3.19 createlistfromarraylike for plain arrays and arguments, else miss
arg_list(St, {?HANDLE_TAG, Id}) ->
    case arc_rt_arena_ffi:get(Id, element(?AGENT_CELLS, St)) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, Props, _, Els, _}
          when map_size(Props) =:= 0 ->
            dense_prefix(Els, Len);
        {?SOBJECT_TAG, {?ARGUMENTSOBJ_TAG, _, _}, _,
         #{?KEY_ID(?LENGTH_KEY) := {_, LenProp}, ?KEY_ID(?CALLEE_KEY) := {_, _}} = Props, _, Els, _}
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
