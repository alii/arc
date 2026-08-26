%%% arc_rt_array_ffi — alloc-free wire-term kernels the Array builtins scan
%%% elements with (arc/rt/builtins/array.gleam, helpers.gleam). Pure term
%%% work over the threaded `St`: no NIF, no process state.
-module(arc_rt_array_ffi).
-export([own_element/3, scan_forward/5, scan_backward/4]).

-include("../arc_rt_layout.hrl").

%% own_element(St, Obj, Idx) -> {hit, V} | slow
%% The `OwnIndexValue` arm of rt_obj.t_get_own_index: the receiver is an
%% Array or Arguments SObject with no own Index(Idx) property override and a
%% present element at Idx. Everything else (holes, accessors, proxies,
%% typed arrays, primitives) answers `slow` for the Gleam path.
own_element(St, {?HANDLE_TAG, Id}, Idx) when is_integer(Idx), Idx >= 0 ->
    Store = element(?AGENT_STORE, St),
    case array:get(Id, element(?STORE_DATA, Store)) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, _, Props, _, Els, _} ->
            unless_overridden(Props, Els, Idx);
        {?SOBJECT_TAG, {?ARGUMENTSOBJ_TAG, _, _}, _, Props, _, Els, _} ->
            unless_overridden(Props, Els, Idx);
        _ -> slow
    end;
own_element(_, _, _) -> slow.

unless_overridden(#{} = Props, Els, Idx) when map_size(Props) =:= 0 ->
    hit(elem_read(Els, Idx));
unless_overridden(Props, Els, Idx) ->
    case Props of
        #{{?KEY_INDEX, Idx} := _} -> slow;
        _ -> hit(elem_read(Els, Idx))
    end.

hit(?ELEMS_HOLE) -> slow;
hit(V) -> {hit, V}.

elem_read({?ELEMS_DENSE, A}, Idx) ->
    case Idx < array:size(A) of
        true -> array:get(Idx, A);
        false -> ?ELEMS_HOLE
    end;
elem_read({?ELEMS_SPARSE, M}, Idx) ->
    case M of
        #{Idx := V} -> V;
        _ -> ?ELEMS_HOLE
    end;
elem_read(_, _) -> ?ELEMS_HOLE.

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
