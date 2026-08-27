%% fast-path kernels: total, answer `miss` when anything observable is needed
-module(arc_interp_ffi).
-export([is_miss/1, is_tdz/1, is_undefined/1,
         truthy/1, lnot/1, nullish/1, typeof/1, typeof/2,
         box_get/2, cell_of/2, ctor_prototype/2, list_of/2, instance_of/4,
         capture_env/2, iter_step/2]).

-include("../rt/arc_rt_layout.hrl").

is_miss(miss) -> true;
is_miss(_) -> false.

is_tdz(js_tdz) -> true;
is_tdz(_) -> false.

is_undefined(undefined) -> true;
is_undefined(_) -> false.

%% §7.1.2 toboolean, keep in step with arc_rt_val_ffi:to_boolean_i32
truthy(undefined) -> false;
truthy(null) -> false;
truthy(false) -> false;
truthy(true) -> true;
truthy(0) -> false;
truthy(N) when is_integer(N) -> true;
truthy(F) when is_float(F) -> F /= 0.0;
truthy(js_nan) -> false;
truthy(js_inf) -> true;
truthy(js_neg_inf) -> true;
truthy(<<>>) -> false;
truthy(B) when is_binary(B) -> true;
truthy({js_bigint, 0}) -> false;
truthy({js_bigint, _}) -> true;
truthy({js_sym, _}) -> true;
truthy({?HANDLE_TAG, _}) -> true;
truthy(js_tdz) -> false.

lnot(V) -> not truthy(V).

nullish(undefined) -> true;
nullish(null) -> true;
nullish(_) -> false.

%% §13.5.3 primitives only, objects miss
typeof(undefined) -> <<"undefined">>;
typeof(null) -> <<"object">>;
typeof(B) when is_boolean(B) -> <<"boolean">>;
typeof(N) when is_number(N) -> <<"number">>;
typeof(A) when A =:= js_nan; A =:= js_inf; A =:= js_neg_inf -> <<"number">>;
typeof(B) when is_binary(B) -> <<"string">>;
typeof({js_bigint, _}) -> <<"bigint">>;
typeof({js_sym, _}) -> <<"symbol">>;
typeof(js_tdz) -> <<"undefined">>;
typeof(_) -> miss.

%% proxy misses, §10.5.14
typeof(Store, {?HANDLE_TAG, Id}) ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case kind_tag(element(?SOBJECT_KIND, Slot)) of
                ?KFN_TAG -> <<"function">>;
                ?KBYTECODE_TAG -> <<"function">>;
                ?KNATIVE_TAG -> <<"function">>;
                k_bound -> <<"function">>;
                ?PROXYOBJ_TAG -> miss;
                _ -> <<"object">>
            end;
        Slot when element(1, Slot) =:= ?SSHAPED_TAG -> <<"object">>;
        _ -> miss
    end;
typeof(_Store, V) -> typeof(V).

kind_tag(Kind) when is_atom(Kind) -> Kind;
kind_tag(Kind) -> element(1, Kind).

cell_of(Agent, {?HANDLE_TAG, Id}) ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, Agent))) of
        ?STORE_FREE_SLOT -> miss;
        Slot -> Slot
    end;
cell_of(_, _) -> miss.

%% §10.1.13 step 2 when own data "prototype" is an object
ctor_prototype(Agent, {?HANDLE_TAG, Id}) ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, Agent))) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            Kind = kind_tag(element(?SOBJECT_KIND, Slot)),
            case
                Kind =:= ?KBYTECODE_TAG orelse Kind =:= ?KFN_TAG
                orelse Kind =:= ?KNATIVE_TAG
            of
                true ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{{?KEY_NAMED, <<"prototype">>} := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG ->
                            case element(?DATAPROP_VALUE, Prop) of
                                {?HANDLE_TAG, _} = P -> P;
                                _ -> miss
                            end;
                        _ -> miss
                    end;
                false -> miss
            end;
        _ -> miss
    end;
ctor_prototype(_, _) -> miss.

%% §7.3.20 for plain arrays and unmapped arguments, holes miss
list_of(Agent, {?HANDLE_TAG, Id}) ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, Agent))) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, Props, _, {?ELEMS_DENSE, A}, _}
          when map_size(Props) =:= 0 ->
            dense_list(A, Len);
        {?SOBJECT_TAG, {?ARGUMENTSOBJ_TAG, _, Mapped}, _, Props, _,
         {?ELEMS_DENSE, A}, _}
          when map_size(Props) =:= 2, (Mapped =:= ?NONE orelse Mapped =:= {?SOME, []}) ->
            case Props of
                #{{?KEY_NAMED, <<"length">>} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG,
                       is_integer(element(?DATAPROP_VALUE, Prop)),
                       is_map_key({?KEY_NAMED, <<"callee">>}, Props) ->
                    dense_list(A, element(?DATAPROP_VALUE, Prop));
                _ -> miss
            end;
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, 0}, _, Props, _, _, _}
          when map_size(Props) =:= 0 ->
            [];
        _ -> miss
    end;
list_of(_, _) -> miss.

dense_list(A, Len) ->
    case array:size(A) >= Len of
        false -> miss;
        true -> dense_prefix(A, Len - 1, [])
    end.

dense_prefix(_, I, Acc) when I < 0 -> Acc;
dense_prefix(A, I, Acc) ->
    case array:get(I, A) of
        ?ELEMS_HOLE -> miss;
        V -> dense_prefix(A, I - 1, [V | Acc])
    end.

%% tdz box misses
box_get(Agent, {?HANDLE_TAG, Id}) ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, Agent))) of
        {?SBOX_TAG, js_tdz} -> miss;
        {?SBOX_TAG, V} -> V;
        _ -> miss
    end;
box_get(_, _) -> miss.

capture_env([], _) -> {};
capture_env([{capture_local, I}], Locals) -> {element(I + 1, Locals)};
capture_env([{capture_local, I}, {capture_local, J}], Locals) ->
    {element(I + 1, Locals), element(J + 1, Locals)};
capture_env(Descriptors, Locals) ->
    list_to_tuple([element(I + 1, Locals) || {capture_local, I} <- Descriptors]).

%% §13.10.2 + §7.3.22 inlined when @@hasInstance is provably the intrinsic
%% max 64 hops, proxies miss
instance_of(_, js_tdz, _, _) -> miss;
instance_of(Agent, V, {?HANDLE_TAG, CId}, Sym) ->
    Data = element(?STORE_DATA, element(?AGENT_STORE, Agent)),
    {?HANDLE_TAG, FP} =
        element(?PAIR_PROTO,
                element(?REALM_FUNCTION, element(?AGENT_REALM, Agent))),
    case arc_rt_arena_ffi:get(CId, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            Kind = kind_tag(element(?SOBJECT_KIND, Slot)),
            case
                (Kind =:= ?KBYTECODE_TAG orelse Kind =:= ?KFN_TAG
                 orelse Kind =:= ?KNATIVE_TAG)
                andalso ordinary_has_instance(Data, Slot, FP, Sym, 64)
            of
                false -> miss;
                true ->
                    case V of
                        {?HANDLE_TAG, VId} ->
                            case element(?SOBJECT_PROPS, Slot) of
                                #{{?KEY_NAMED, <<"prototype">>} := Prop}
                                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                                    case element(?DATAPROP_VALUE, Prop) of
                                        {?HANDLE_TAG, PId} ->
                                            chain_reaches(Data, VId, PId, 64);
                                        _ -> miss
                                    end;
                                _ -> miss
                            end;
                        _ -> false
                    end
            end;
        _ -> miss
    end;
instance_of(_, _, _, _) -> miss.

ordinary_has_instance(_, _, _, _, 0) -> false;
ordinary_has_instance(Data, Slot, FP, Sym, Fuel) ->
    (not lists:keymember(Sym, 1, element(?SOBJECT_SYMBOL_PROPS, Slot)))
        andalso case element(?SOBJECT_PROTO, Slot) of
                    ?NONE -> true;
                    {?SOME, {?HANDLE_TAG, FP}} -> true;
                    {?SOME, {?HANDLE_TAG, P}} -> plain_above(Data, P, FP, Sym, Fuel - 1);
                    _ -> false
                end.

plain_above(Data, P, FP, Sym, Fuel) ->
    case arc_rt_arena_ffi:get(P, Data) of
        %% shapes hold string keys only
        {?SSHAPED_TAG, _, ?NONE, _} -> true;
        {?SSHAPED_TAG, _, {?SOME, {?HANDLE_TAG, FP}}, _} -> true;
        {?SSHAPED_TAG, _, {?SOME, {?HANDLE_TAG, Q}}, _} ->
            plain_above(Data, Q, FP, Sym, Fuel - 1);
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case kind_tag(element(?SOBJECT_KIND, Slot)) of
                ?PROXYOBJ_TAG -> false;
                _ -> ordinary_has_instance(Data, Slot, FP, Sym, Fuel)
            end;
        _ -> false
    end.

%% §7.3.22 step 7
chain_reaches(_, _, _, 0) -> miss;
chain_reaches(Data, VId, PId, Fuel) ->
    case arc_rt_arena_ffi:get(VId, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG;
                  element(1, Slot) =:= ?SSHAPED_TAG ->
            case element(1, Slot) =:= ?SOBJECT_TAG
                 andalso kind_tag(element(?SOBJECT_KIND, Slot)) =:= ?PROXYOBJ_TAG of
                true -> miss;
                false ->
                    %% proto is element 3 of both cell shapes
                    case element(?SOBJECT_PROTO, Slot) of
                        ?NONE -> false;
                        {?SOME, {?HANDLE_TAG, PId}} -> true;
                        {?SOME, {?HANDLE_TAG, Next}} ->
                            chain_reaches(Data, Next, PId, Fuel - 1);
                        _ -> miss
                    end
            end;
        _ -> miss
    end.

%% §23.1.5.2.1 array iterator or generator resume, else `protocol`
%% index -1 marks exhausted
-define(ITERATOR_KEY, {?KEY_NAMED, <<"iterator">>}).
-define(NEXT_KEY, {?KEY_NAMED, <<"next">>}).
iter_step(Store, {?HANDLE_TAG, RecId}) ->
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(RecId, Data) of
        {?SOBJECT_TAG, ?ORDINARY, _, #{?ITERATOR_KEY := IP, ?NEXT_KEY := NP},
         _, _, _}
          when element(1, IP) =:= ?DATAPROP_TAG,
               element(1, NP) =:= ?DATAPROP_TAG ->
            case {element(?DATAPROP_VALUE, NP), element(?DATAPROP_VALUE, IP)} of
                {{?HANDLE_TAG, NextId}, {?HANDLE_TAG, IterId}} ->
                    iter_step_with(Store, Data, native_token(arc_rt_arena_ffi:get(NextId, Data)),
                                   IterId, arc_rt_arena_ffi:get(IterId, Data));
                _ -> protocol
            end;
        _ -> protocol
    end;
iter_step(_, _) -> protocol.

native_token(Slot)
  when element(1, Slot) =:= ?SOBJECT_TAG,
       element(1, element(?SOBJECT_KIND, Slot)) =:= ?KNATIVE_TAG ->
    element(?KNATIVE_TOKEN, element(?SOBJECT_KIND, Slot));
native_token(_) -> none.

iter_step_with(Store, Data, ?TOKEN_ARRAY_ITER_NEXT, IterId, IterSlot)
  when element(1, IterSlot) =:= ?SOBJECT_TAG ->
    case element(?SOBJECT_KIND, IterSlot) of
        {?ARRAYITER_TAG, _, Index, ?ARRAYITER_VALUES} when Index < 0 ->
            {array_step, true, undefined, Store};
        {?ARRAYITER_TAG, {?HANDLE_TAG, T} = Target, Index, ?ARRAYITER_VALUES} ->
            case arc_rt_arena_ffi:get(T, Data) of
                {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, _, _, _, _} when Index >= Len ->
                    array_iter_advance(Store, Data, IterId, IterSlot, Target, -1,
                                       true, undefined);
                {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, _, Props, _, Els, _} ->
                    case map_size(Props) =/= 0
                         andalso is_map_key({?KEY_INDEX, Index}, Props) of
                        true -> protocol;
                        false ->
                            case iter_elem(Els, Index) of
                                ?ELEMS_HOLE -> protocol;
                                V ->
                                    array_iter_advance(Store, Data, IterId, IterSlot,
                                                       Target, Index + 1, false, V)
                            end
                    end;
                _ -> protocol
            end;
        _ -> protocol
    end;
iter_step_with(_, _, ?TOKEN_GENERATOR_NEXT, _, IterSlot)
  when element(1, IterSlot) =:= ?SOBJECT_TAG ->
    case element(?SOBJECT_KIND, IterSlot) of
        {?GENERATOROBJ_TAG, DataH} -> {gen_step, DataH};
        _ -> protocol
    end;
iter_step_with(_, _, _, _, _) -> protocol.

array_iter_advance(Store, Data, IterId, IterSlot, Target, Index, Done, V) ->
    NewSlot = setelement(?SOBJECT_KIND, IterSlot,
                         {?ARRAYITER_TAG, Target, Index, ?ARRAYITER_VALUES}),
    {array_step, Done, V,
     setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(IterId, NewSlot, Data))}.

iter_elem({?ELEMS_DENSE, A}, Idx) -> array:get(Idx, A);
iter_elem({?ELEMS_SPARSE, M}, Idx) ->
    case M of
        #{Idx := V} -> V;
        _ -> ?ELEMS_HOLE
    end;
iter_elem(_, _) -> ?ELEMS_HOLE.
