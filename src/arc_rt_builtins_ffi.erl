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
-export([strict_eq/2, same_value_zero/2, own_index/3, scan_forward/5,
         scan_backward/4]).

-include("arc_rt_layout.hrl").

%% arc/rt/types.ObjKind: ArgumentsObj(length, mapped). Reads resolve exactly
%% like an Array's (own Index override, else the elements store).
-define(ARGUMENTSOBJ_TAG, arguments_obj).

%% dispatch_native(St, Tag, This, Args) -> {JsVal, St'}.
dispatch_native(St, Tag, This, Args) ->
    arc@rt@builtins:dispatch_native(St, Tag, This, Args).

%% dispatch_native_construct(St, Tag, Args, NewTarget) -> {Handle, St'}.
dispatch_native_construct(St, Tag, Args, NewTarget) ->
    arc@rt@builtins:dispatch_native_construct(St, Tag, Args, NewTarget).

%% strict_eq(A, B) -> boolean()
%% §7.2.15 IsStrictlyEqual, total on wire terms. NaN is unequal to itself;
%% Numbers compare numerically (1 === 1.0, +0 === -0); every other row is
%% exact term identity (same atom, same binary, same {js_cell,N} /
%% {js_bigint,N} / {js_sym,S}).
strict_eq(js_nan, _) -> false;
strict_eq(_, js_nan) -> false;
strict_eq(A, B) when is_number(A), is_number(B) -> A == B;
strict_eq(A, B) -> A =:= B.

%% same_value_zero(A, B) -> boolean()
%% §7.2.12 SameValueZero: IsStrictlyEqual except NaN equals NaN.
same_value_zero(js_nan, js_nan) -> true;
same_value_zero(A, B) -> strict_eq(A, B).

%% own_index(St, Obj, Idx) -> {hit, V} | slow
%% The `OwnIndexValue` arm of rt_obj.t_get_own_index: the receiver is an
%% Array or Arguments SObject with no own Index(Idx) property override and a
%% present element at Idx. Everything else (holes, accessors, proxies,
%% typed arrays, primitives) answers `slow` for the Gleam path.
own_index(St, {?HANDLE_TAG, Id}, Idx) when is_integer(Idx), Idx >= 0 ->
    Store = element(?AGENT_STORE, St),
    case array:get(Id, element(?STORE_DATA, Store)) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, _, Props, _, Els, _} ->
            own_element(Props, Els, Idx);
        {?SOBJECT_TAG, {?ARGUMENTSOBJ_TAG, _, _}, _, Props, _, Els, _} ->
            own_element(Props, Els, Idx);
        _ -> slow
    end;
own_index(_, _, _) -> slow.

own_element(#{} = Props, Els, Idx) when map_size(Props) =:= 0 ->
    hit(elem_read(Els, Idx));
own_element(Props, Els, Idx) ->
    case Props of
        #{{?KEY_INDEX, Idx} := _} -> slow;
        _ -> hit(elem_read(Els, Idx))
    end.

hit(js_hole) -> slow;
hit(V) -> {hit, V}.

elem_read({?ELEMS_DENSE, A}, Idx) ->
    case Idx < array:size(A) of
        true -> array:get(Idx, A);
        false -> js_hole
    end;
elem_read({?ELEMS_SPARSE, M}, Idx) ->
    case M of
        #{Idx := V} -> V;
        _ -> js_hole
    end;
elem_read(_, _) -> js_hole.

%% scan_forward(Els, Search, Idx, End, Eq) -> {match, I} | {hole_at, I} | absent
%% Compare present elements in [Idx, End) against Search with `strict` /
%% `same_value_zero`, stopping at the first match or the first hole (the
%% caller decides what a hole means and resumes past it).
scan_forward(_, _, Idx, End, _) when Idx >= End -> absent;
scan_forward(Els, Search, Idx, End, Eq) ->
    case elem_read(Els, Idx) of
        js_hole -> {hole_at, Idx};
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
        js_hole -> {hole_at, Idx};
        V ->
            case eq(Eq, V, Search) of
                true -> {match, Idx};
                false -> scan_backward(Els, Search, Idx - 1, Eq)
            end
    end.

eq(strict, A, B) -> strict_eq(A, B);
eq(same_value_zero, A, B) -> same_value_zero(A, B).
