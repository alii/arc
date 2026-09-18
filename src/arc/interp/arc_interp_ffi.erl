%% interpreter kernels; exports may answer miss when anything observable is needed
-module(arc_interp_ffi).
-export([for_in_list/1, for_in_next/1,
         type_of/2,
         box_get/2, cell_of/2, ctor_prototype/2, list_of/2, instance_of/4,
         capture_env/2, iter_step/2]).

-include("../rt/arc_rt_layout.hrl").

%% §13.5.3 primitives only, objects miss
type_of(undefined) -> <<"undefined">>;
type_of(null) -> <<"object">>;
type_of(B) when is_boolean(B) -> <<"boolean">>;
type_of(N) when is_number(N) -> <<"number">>;
type_of(A) when A =:= js_nan; A =:= js_inf; A =:= js_neg_inf -> <<"number">>;
type_of(B) when is_binary(B) -> <<"string">>;
type_of({?STR_TAG, _, _, _}) -> <<"string">>;
type_of({js_bigint, _}) -> <<"bigint">>;
type_of({js_sym, _}) -> <<"symbol">>;
type_of(js_tdz) -> <<"undefined">>;
type_of(_) -> miss.

%% proxy misses, §10.5.14
type_of(Store, {?HANDLE_TAG, Id}) ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case kind_tag(element(?SOBJECT_KIND, Cell)) of
                ?COMPILEDFN_TAG -> <<"function">>;
                ?BYTECODEFN_TAG -> <<"function">>;
                ?NATIVEFN_TAG -> <<"function">>;
                ?BOUNDFN_TAG -> <<"function">>;
                ?PROXYOBJ_TAG -> miss;
                _ -> <<"object">>
            end;
        Cell when element(1, Cell) =:= ?SSHAPED_TAG -> <<"object">>;
        _ -> miss
    end;
type_of(_Store, V) -> type_of(V).

kind_tag(Kind) when is_atom(Kind) -> Kind;
kind_tag(Kind) -> element(1, Kind).

cell_of(Agent, {?HANDLE_TAG, Id}) ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, Agent))) of
        ?STORE_FREE_CELL -> miss;
        Cell -> Cell
    end;
cell_of(_, _) -> miss.

%% §10.1.13 step 2 when own data "prototype" is an object
ctor_prototype(Agent, {?HANDLE_TAG, Id}) ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, Agent))) of
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            Kind = kind_tag(element(?SOBJECT_KIND, Cell)),
            case
                Kind =:= ?BYTECODEFN_TAG orelse Kind =:= ?COMPILEDFN_TAG
                orelse Kind =:= ?NATIVEFN_TAG
            of
                true ->
                    case element(?SOBJECT_PROPS, Cell) of
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
    case arc_tree_array_ffi:size(A) of
        Len -> hole_free(arc_tree_array_ffi:to_list(A));
        Size when Size > Len ->
            hole_free(lists:sublist(arc_tree_array_ffi:to_list(A), Len));
        _ -> miss
    end.

hole_free(L) ->
    case lists:member(?ELEMS_HOLE, L) of
        true -> miss;
        false -> L
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
%% proxies miss
instance_of(_, js_tdz, _, _) -> miss;
instance_of(Agent, V, {?HANDLE_TAG, CId}, Sym) ->
    Data = element(?STORE_DATA, element(?AGENT_STORE, Agent)),
    {?HANDLE_TAG, FP} =
        element(?PAIR_PROTO,
                element(?REALM_FUNCTION, element(?AGENT_REALM, Agent))),
    case arc_rt_arena_ffi:get(CId, Data) of
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            Kind = kind_tag(element(?SOBJECT_KIND, Cell)),
            case
                (Kind =:= ?BYTECODEFN_TAG orelse Kind =:= ?COMPILEDFN_TAG
                 orelse Kind =:= ?NATIVEFN_TAG)
                andalso ordinary_has_instance(Data, Cell, FP, Sym, ?MAX_PROTO_HOPS)
            of
                false -> miss;
                true ->
                    case V of
                        {?HANDLE_TAG, VId} ->
                            case element(?SOBJECT_PROPS, Cell) of
                                #{{?KEY_NAMED, <<"prototype">>} := Prop}
                                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                                    case element(?DATAPROP_VALUE, Prop) of
                                        {?HANDLE_TAG, PId} ->
                                            chain_reaches(Data, VId, PId,
                                                          ?MAX_PROTO_HOPS);
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
ordinary_has_instance(Data, Cell, FP, Sym, Fuel) ->
    (not lists:keymember(Sym, 1, element(?SOBJECT_SYMBOL_PROPS, Cell)))
        andalso case element(?SOBJECT_PROTO, Cell) of
                    ?NONE -> true;
                    {?SOME, {?HANDLE_TAG, FP}} -> true;
                    {?SOME, {?HANDLE_TAG, P}} -> plain_above(Data, P, FP, Sym, Fuel - 1);
                    _ -> false
                end.

plain_above(Data, P, FP, Sym, Fuel) ->
    case arc_rt_arena_ffi:get(P, Data) of
        %% shapes hold string keys only
        {?SSHAPED_TAG, _, ?NONE, _, _} -> true;
        {?SSHAPED_TAG, _, {?SOME, {?HANDLE_TAG, FP}}, _, _} -> true;
        {?SSHAPED_TAG, _, {?SOME, {?HANDLE_TAG, Q}}, _, _} ->
            plain_above(Data, Q, FP, Sym, Fuel - 1);
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case kind_tag(element(?SOBJECT_KIND, Cell)) of
                ?PROXYOBJ_TAG -> false;
                _ -> ordinary_has_instance(Data, Cell, FP, Sym, Fuel)
            end;
        _ -> false
    end.

%% §7.3.22 step 7
chain_reaches(_, _, _, 0) -> miss;
chain_reaches(Data, VId, PId, Fuel) ->
    case arc_rt_arena_ffi:get(VId, Data) of
        Cell when element(1, Cell) =:= ?SOBJECT_TAG;
                  element(1, Cell) =:= ?SSHAPED_TAG ->
            case element(1, Cell) =:= ?SOBJECT_TAG
                 andalso kind_tag(element(?SOBJECT_KIND, Cell)) =:= ?PROXYOBJ_TAG of
                true -> miss;
                false ->
                    case element(?CELL_PROTO, Cell) of
                        ?NONE -> false;
                        {?SOME, {?HANDLE_TAG, PId}} -> true;
                        {?SOME, {?HANDLE_TAG, Next}} ->
                            chain_reaches(Data, Next, PId, Fuel - 1);
                        _ -> miss
                    end
            end;
        _ -> miss
    end.

%% §23.1.5.2.1 array iterator or generator resume, else iter_miss
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
                _ -> iter_miss
            end;
        _ -> iter_miss
    end;
iter_step(_, _) -> iter_miss.


native_token(Cell) -> ?NATIVE_TOKEN(Cell).

iter_step_with(Store, Data, ?TOKEN_ARRAY_ITER_NEXT, IterId, IterCell)
  when element(1, IterCell) =:= ?SOBJECT_TAG ->
    case element(?SOBJECT_KIND, IterCell) of
        {?ARRAYITER_TAG, _, Index, ?ARRAYITER_VALUES} when Index < 0 ->
            {array_advanced, true, undefined, Store};
        {?ARRAYITER_TAG, {?HANDLE_TAG, T} = Target, Index, ?ARRAYITER_VALUES} ->
            case arc_rt_arena_ffi:get(T, Data) of
                {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, _, _, _, _} when Index >= Len ->
                    array_iter_advance(Store, Data, IterId, IterCell, Target, -1,
                                       true, undefined);
                {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, _, Props, _, Els, _} ->
                    case map_size(Props) =/= 0
                         andalso is_map_key({?KEY_INDEX, Index}, Props) of
                        true -> iter_miss;
                        false ->
                            case elem_at(Els, Index) of
                                ?ELEMS_HOLE -> iter_miss;
                                V ->
                                    array_iter_advance(Store, Data, IterId, IterCell,
                                                       Target, Index + 1, false, V)
                            end
                    end;
                _ -> iter_miss
            end;
        _ -> iter_miss
    end;
iter_step_with(_, _, ?TOKEN_GENERATOR_NEXT, _, IterCell)
  when element(1, IterCell) =:= ?SOBJECT_TAG ->
    case element(?SOBJECT_KIND, IterCell) of
        {?GENERATOROBJ_TAG, DataH} -> {resume_generator, DataH};
        _ -> iter_miss
    end;
iter_step_with(_, _, _, _, _) -> iter_miss.

array_iter_advance(Store, Data, IterId, IterCell, Target, Index, Done, V) ->
    NewCell = setelement(?SOBJECT_KIND, IterCell,
                         {?ARRAYITER_TAG, Target, Index, ?ARRAYITER_VALUES}),
    {array_advanced, Done, V,
     setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(IterId, NewCell, Data))}.

elem_at(Els, Idx) -> ?ELEM_AT(Els, Idx).

for_in_list(Keys) -> {for_in, Keys}.

for_in_next({for_in, [K | Rest]}) -> {for_in_key, K, {for_in, Rest}};
for_in_next({for_in, []}) -> for_in_end.
