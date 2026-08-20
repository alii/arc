%%% arc_rt_builtins_ffi — the M4→M6 native-dispatch seam, plus the
%%% alloc-free wire-term kernels the Array builtins scan elements with.
%%%
%%% `rt_call.gleam:83-98` forward-declares `dispatch_native/4` and
%%% `dispatch_native_construct/4` as `@external(erlang,
%%% "arc_rt_builtins_ffi", ...)` so `rt_call` can compile before
%%% `rt_builtins` exists (SPEC §7.M6 — @external targets are unresolved by
%%% `gleam check`). This shim just forwards to the real Gleam bodies in
%%% `arc@rt@builtins` — the ONE place native dispatch lives.
%%%
%%% Hand-written Erlang, so it carries the `arc_rt_` namespace prefix
%%% (overview §5) and cannot collide with an OTP module.
-module(arc_rt_builtins_ffi).
-export([dispatch_native/4, dispatch_native_construct/4]).
-export([own_element/3, scan_forward/5, scan_backward/4]).

-include("arc_rt_layout.hrl").

%% dispatch_native(St, Tag, This, Args) -> {JsVal, St'}.
dispatch_native(St, Tag, This, Args) ->
    arc@rt@builtins:dispatch_native(St, Tag, This, Args).

%% dispatch_native_construct(St, Tag, Args, NewTarget) -> {Handle, St'}.
dispatch_native_construct(St, Tag, Args, NewTarget) ->
    arc@rt@builtins:dispatch_native_construct(St, Tag, Args, NewTarget).

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
