%%% arc_rt_obj_fast_ffi — object fast paths only AOT-emitted code calls
%%% (`aot/src/arc_aot/host_ops.gleam`). Like arc_rt_call_fast_ffi every entry
%%% handles the common shape in place and hands anything else to the same
%%% runtime path the emitter would otherwise have called, so the result is
%%% what the unfused sequence yields. Record indices come from
%%% arc_rt_layout.hrl (asserted by arc_rt_layout_test). Nothing here is bound
%%% from Gleam except by tests.
-module(arc_rt_obj_fast_ffi).
-export([t_set_props_named/5, t_new_object_props/3]).

-include("arc_rt_layout.hrl").

%% t_set_props_named(St, Obj, Keys, Vals, Strict) -> St'
%% JMutUnit. A run of `obj.k1 = v1; obj.k2 = v2; ...` statements whose values
%% are already evaluated, as ONE host op: the same as folding
%% `arc_rt_obj_ffi:t_set_prop_named` over the pairs in order. While the
%% receiver is an SShapedObject and each write is a slot overwrite or a step
%% along an existing transition edge the proto chain cannot intercept
%% (`named_free`), the slots are rebuilt locally and stored once; the first
%% pair that needs anything else stores what was built and the rest of the
%% run goes pair by pair through `t_set_prop_named`.
t_set_props_named(St, Obj = {?HANDLE_TAG, Id}, Keys, Vals, Strict) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, Sid, P, Slots} ->
            shaped_run(St, Store, Data, Id, Obj, Sid, P, Slots, Keys, Vals,
                       Strict, element(?STORE_SHAPES, Store), false);
        _ -> each_named(St, Obj, Keys, Vals, Strict)
    end;
t_set_props_named(St, Obj, Keys, Vals, Strict) ->
    each_named(St, Obj, Keys, Vals, Strict).

shaped_run(St, Store, Data, Id, Obj, Sid, P, Slots, [K | Ks], [V | Vs],
           Strict, Shapes, Dirty) ->
    case Shapes of
        #{Sid := Desc} ->
            case element(?SHAPE_OFFSETS, Desc) of
                #{K := Off} ->
                    shaped_run(St, Store, Data, Id, Obj, Sid, P,
                               setelement(Off + 1, Slots, V), Ks, Vs, Strict,
                               Shapes, true);
                _ ->
                    case element(?SHAPE_TRANSITIONS, Desc) of
                        #{K := To} ->
                            case arc_rt_obj_ffi:named_free(
                                   Data, Shapes, P, {?KEY_NAMED, K}, 64) of
                                true ->
                                    shaped_run(St, Store, Data, Id, Obj, To,
                                               P,
                                               erlang:append_element(Slots,
                                                                     V),
                                               Ks, Vs, Strict, Shapes, true);
                                false ->
                                    St1 = commit(St, Store, Data, Id, Sid, P,
                                                 Slots, Dirty),
                                    each_named(St1, Obj, [K | Ks], [V | Vs],
                                               Strict)
                            end;
                        _ ->
                            St1 = commit(St, Store, Data, Id, Sid, P, Slots,
                                         Dirty),
                            each_named(St1, Obj, [K | Ks], [V | Vs], Strict)
                    end
            end;
        _ ->
            St1 = commit(St, Store, Data, Id, Sid, P, Slots, Dirty),
            each_named(St1, Obj, [K | Ks], [V | Vs], Strict)
    end;
shaped_run(St, Store, Data, Id, _, Sid, P, Slots, _, _, _, _, Dirty) ->
    commit(St, Store, Data, Id, Sid, P, Slots, Dirty).

commit(St, _, _, _, _, _, _, false) -> St;
commit(St, Store, Data, Id, Sid, P, Slots, true) ->
    setelement(?AGENT_STORE, St,
               setelement(?STORE_DATA, Store,
                          arc_rt_arena_ffi:set(Id, {?SSHAPED_TAG, Sid, P, Slots}, Data))).

each_named(St, Obj, [K | Ks], [V | Vs], Strict) ->
    each_named(arc_rt_obj_ffi:t_set_prop_named(St, Obj, K, V, Strict), Obj,
               Ks, Vs, Strict);
each_named(St, _, _, _, _) -> St.

%% t_new_object_props(St, Keys, Vals) -> {Obj, St'}
%% JMut. The leading `key: value` members of an object literal, all plain
%% data members under distinct static Named keys with their values already
%% evaluated in source order, as ONE host op: what `t_new_object_literal`
%% followed by one `t_create_data_prop` per member builds (an ordinary
%% extensible SObject on %Object.prototype% whose props carry consecutive
%% prop_seq stamps), allocated with a single store write as `t_cell_new`
%% does it.
t_new_object_props(St, Keys, Vals) ->
    Store = element(?AGENT_STORE, St),
    new_object_props(St, Store, Keys, Vals).

new_object_props(St, Store, Keys, Vals)
  when tuple_size(Store) =:= ?STORE_ARITY ->
    Seq = element(?STORE_PROP_SEQ, Store),
    {Props, Seq1} = props_of(Keys, Vals, Seq, []),
    Proto = element(?PAIR_PROTO,
                    element(?REALM_OBJECT, element(?AGENT_REALM, St))),
    Slot = {?SOBJECT_TAG, ?ORDINARY, {?SOME, Proto}, Props, [], ?ELEMS_NONE,
            true},
    Id = element(?STORE_NEXT, Store),
    Store1 = setelement(?STORE_DATA, Store,
                        arc_rt_arena_ffi:set(Id, Slot, element(?STORE_DATA, Store))),
    Store2 = setelement(?STORE_NEXT, Store1, Id + 1),
    Store3 = setelement(?STORE_ALLOC, Store2,
                        element(?STORE_ALLOC, Store) + 1),
    Store4 = setelement(?STORE_PROP_SEQ, Store3, Seq1),
    {{?HANDLE_TAG, Id}, setelement(?AGENT_STORE, St, Store4)}.

props_of([K | Ks], [V | Vs], Seq, Acc) ->
    props_of(Ks, Vs, Seq + 1,
             [{{?KEY_NAMED, K}, {?DATAPROP_TAG, V, true, true, true, Seq}}
              | Acc]);
props_of([], [], Seq, Acc) -> {maps:from_list(Acc), Seq}.
