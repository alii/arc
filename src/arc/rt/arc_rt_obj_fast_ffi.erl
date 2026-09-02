%% called only by aot emitted code
-module(arc_rt_obj_fast_ffi).
-export([t_set_props_named/5, t_set_props_init/6, t_new_object_props/3,
         t_set_prop/5, t_set_prop/6, t_get_prop/4, t_global_get/3,
         t_global_get_miss/3]).

-include("arc_rt_layout.hrl").

-compile({inline, [slot_offset/3]}).

-define(IC_READ, ic_read).

-define(IC_GLOBAL, ic_global).
-define(IC_GLOBAL_REFILLS, 16).

%% site cache of a global object data property, valid while the epoch holds;
%% keyed like the read and call ics so a site can never answer for another name
t_global_get(St, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    case element(?STORE_ICS, Store) of
        #{Site := {?IC_GLOBAL, KeyBin, Epoch, V, _}}
          when Epoch =:= element(?STORE_GLOBAL_EPOCH, Store) ->
            V;
        #{Site := ic_off} -> arc_rt_obj_ffi:t_global_get_fast(St, KeyBin);
        _ -> miss
    end.

t_global_get_miss(St, KeyBin, Site) when tuple_size(St) =:= ?AGENT_ARITY ->
    {V, St1} = arc_rt_obj_ffi:t_global_get(St, KeyBin),
    case element(?AGENT_STORE, St1) of
        Store when tuple_size(Store) =:= ?STORE_ARITY ->
            Ics = element(?STORE_ICS, Store),
            N = case Ics of
                #{Site := {?IC_GLOBAL, KeyBin, _, _, N0}} -> N0 + 1;
                #{Site := _} -> ?IC_GLOBAL_REFILLS + 1;
                _ -> 0
            end,
            {?HANDLE_TAG, GId} = element(?REALM_GLOBAL, element(?AGENT_REALM, St1)),
            Slot = arc_rt_arena_ffi:get(GId, element(?STORE_DATA, Store)),
            Entry = case N < ?IC_GLOBAL_REFILLS andalso global_plain(Slot, KeyBin, V) of
                true -> {?IC_GLOBAL, KeyBin, element(?STORE_GLOBAL_EPOCH, Store), V, N};
                false -> ic_off
            end,
            case N > ?IC_GLOBAL_REFILLS of
                true -> {V, St1};
                false ->
                    {V, setelement(?AGENT_STORE, St1,
                                   setelement(?STORE_ICS, Store, Ics#{Site => Entry}))}
            end
    end.

global_plain(Slot, KeyBin, V)
  when tuple_size(Slot) =:= ?SOBJECT_ARITY, element(1, Slot) =:= ?SOBJECT_TAG ->
    case element(?SOBJECT_PROPS, Slot) of
        #{{?KEY_NAMED, KeyBin} := Prop} ->
            element(1, Prop) =:= ?DATAPROP_TAG
                andalso element(?DATAPROP_VALUE, Prop) =:= V;
        _ -> false
    end;
global_plain(_, _, _) -> false.

%% bare value or miss, miss takes arc_rt_obj_ffi:t_get_prop_slow which fills
t_get_prop(St, {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    Slot = arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)),
    case element(1, Slot) of
        ?SSHAPED_TAG ->
            case element(?STORE_ICS, Store) of
                #{Site := {?IC_READ, KeyBin, Offs}} ->
                    case Offs of
                        #{element(?SSHAPED_SID, Slot) := Off} ->
                            element(Off + 1, element(?SSHAPED_SLOTS, Slot));
                        _ -> miss
                    end;
                _ -> miss
            end;
        ?SOBJECT_TAG ->
            case element(?SOBJECT_PROPS, Slot) of
                #{{?KEY_NAMED, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                    Kind = element(?SOBJECT_KIND, Slot),
                    case is_atom(Kind)
                         orelse arc_rt_obj_ffi:named_plain(Kind, KeyBin) of
                        true -> element(?DATAPROP_VALUE, Prop);
                        false -> miss
                    end;
                _ ->
                    case element(?SOBJECT_KIND, Slot) of
                        {?ARRAYOBJ_TAG, Len} when KeyBin =:= <<"length">> -> Len;
                        _ -> miss
                    end
            end;
        _ -> miss
    end;
t_get_prop(_, Bin, <<"length">>, _) when is_binary(Bin) ->
    arc_string_ffi:string_codepoint_length(Bin);
t_get_prop(_, _, _, _) -> miss.

%% own slot overwrite only, guards keep setelement inline
t_set_prop(St, Obj = {?HANDLE_TAG, Id}, KeyBin, V, Strict)
  when tuple_size(St) =:= ?AGENT_ARITY ->
    case element(?AGENT_STORE, St) of
        Store when tuple_size(Store) =:= ?STORE_ARITY ->
            Data = element(?STORE_DATA, Store),
            case arc_rt_arena_ffi:get(Id, Data) of
                Slot when element(1, Slot) =:= ?SSHAPED_TAG,
                          tuple_size(Slot) =:= ?SSHAPED_ARITY ->
                    case slot_offset(Store, Slot, KeyBin) of
                        miss ->
                            arc_rt_obj_ffi:t_set_prop_named(St, Obj, KeyBin, V,
                                                            Strict);
                        Off ->
                            Slots = setelement(Off + 1,
                                               element(?SSHAPED_SLOTS, Slot), V),
                            NewSlot = setelement(?SSHAPED_SLOTS, Slot, Slots),
                            setelement(?AGENT_STORE, St,
                                       setelement(?STORE_DATA, Store,
                                                  arc_rt_arena_ffi:set(Id, NewSlot, Data)))
                    end;
                Slot when element(1, Slot) =:= ?SOBJECT_TAG,
                          tuple_size(Slot) =:= ?SOBJECT_ARITY,
                          element(?SOBJECT_KIND, Slot) =:= ?ORDINARY ->
                    Props = element(?SOBJECT_PROPS, Slot),
                    K = {?KEY_NAMED, KeyBin},
                    case Props of
                        #{K := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG,
                               tuple_size(Prop) =:= ?DATAPROP_ARITY,
                               element(?DATAPROP_WRITABLE, Prop) =:= true ->
                            NewSlot = setelement(?SOBJECT_PROPS, Slot,
                                                 Props#{K := setelement(?DATAPROP_VALUE, Prop, V)}),
                            setelement(?AGENT_STORE, St,
                                       setelement(?STORE_DATA, Store,
                                                  arc_rt_arena_ffi:set(Id, NewSlot, Data)));
                        _ ->
                            arc_rt_obj_ffi:t_set_prop_named(St, Obj, KeyBin, V,
                                                            Strict)
                    end;
                _ -> arc_rt_obj_ffi:t_set_prop_named(St, Obj, KeyBin, V, Strict)
            end
    end;
t_set_prop(St, Obj, KeyBin, V, Strict) ->
    arc_rt_obj_ffi:t_set_prop_named(St, Obj, KeyBin, V, Strict).

%% own overwrite first, then the site's cached transition for a new key
t_set_prop(St, Obj = {?HANDLE_TAG, Id}, KeyBin, V, Strict, Site)
  when tuple_size(St) =:= ?AGENT_ARITY ->
    case element(?AGENT_STORE, St) of
        Store when tuple_size(Store) =:= ?STORE_ARITY ->
            Data = element(?STORE_DATA, Store),
            case arc_rt_arena_ffi:get(Id, Data) of
                Slot when element(1, Slot) =:= ?SSHAPED_TAG,
                          tuple_size(Slot) =:= ?SSHAPED_ARITY ->
                    case slot_offset(Store, Slot, KeyBin) of
                        miss ->
                            shaped_init(St, Store, Data, Id, Slot, Obj,
                                        [KeyBin], [V], Strict, Site);
                        Off ->
                            Slots = setelement(Off + 1,
                                               element(?SSHAPED_SLOTS, Slot), V),
                            NewSlot = setelement(?SSHAPED_SLOTS, Slot, Slots),
                            setelement(?AGENT_STORE, St,
                                       setelement(?STORE_DATA, Store,
                                                  arc_rt_arena_ffi:set(Id, NewSlot, Data)))
                    end;
                _ -> t_set_prop(St, Obj, KeyBin, V, Strict)
            end
    end;
t_set_prop(St, Obj, KeyBin, V, Strict, _) ->
    arc_rt_obj_ffi:t_set_prop_named(St, Obj, KeyBin, V, Strict).

slot_offset(_, Slot, KeyBin) ->
    case element(?SSHAPED_OFFSETS, Slot) of
        #{KeyBin := Off} -> Off;
        _ -> miss
    end.

-define(IC_INIT, ic_init).
-define(IC_INIT_HOPS, 8).

%% caches a pure append run from one shape, proto chain checked by identity
t_set_props_init(St, Obj = {?HANDLE_TAG, Id}, Keys, Vals, Strict, Site)
  when tuple_size(St) =:= ?AGENT_ARITY ->
    case element(?AGENT_STORE, St) of
        Store when tuple_size(Store) =:= ?STORE_ARITY ->
            Data = element(?STORE_DATA, Store),
            case arc_rt_arena_ffi:get(Id, Data) of
                Slot when element(1, Slot) =:= ?SSHAPED_TAG,
                          tuple_size(Slot) =:= ?SSHAPED_ARITY ->
                    shaped_init(St, Store, Data, Id, Slot, Obj, Keys, Vals,
                                Strict, Site);
                _ -> t_set_props_named(St, Obj, Keys, Vals, Strict)
            end
    end;
t_set_props_init(St, Obj, Keys, Vals, Strict, _) ->
    t_set_props_named(St, Obj, Keys, Vals, Strict).

shaped_init(St, Store, Data, Id, Slot, Obj, Keys, Vals, Strict, Site)
  when tuple_size(St) =:= ?AGENT_ARITY, tuple_size(Store) =:= ?STORE_ARITY ->
    Sid = element(?SSHAPED_SID, Slot),
    Proto = element(?SSHAPED_PROTO, Slot),
    case element(?STORE_ICS, Store) of
        #{Site := {?IC_INIT, Sid, _, Blank, Chain}}
          when element(?SSHAPED_PROTO, Blank) =:= Proto ->
            case chain_same(Data, Proto, Chain) of
                true ->
                    Slots = init_slots(element(?SSHAPED_SLOTS, Slot), Vals),
                    NewSlot = setelement(?SSHAPED_SLOTS, Blank, Slots),
                    setelement(?AGENT_STORE, St,
                               setelement(?STORE_DATA, Store,
                                          arc_rt_arena_ffi:set(Id, NewSlot, Data)));
                false ->
                    init_fill(t_set_props_named(St, Obj, Keys, Vals, Strict),
                              Id, Sid, Proto, Keys, Site)
            end;
        #{Site := _} -> t_set_props_named(St, Obj, Keys, Vals, Strict);
        _ ->
            init_fill(t_set_props_named(St, Obj, Keys, Vals, Strict), Id, Sid,
                      Proto, Keys, Site)
    end.

init_slots({}, Vals) -> list_to_tuple(Vals);
init_slots(Slots, Vals) -> list_to_tuple(tuple_to_list(Slots) ++ Vals).

chain_same(_, ?NONE, []) -> true;
chain_same(Data, {?SOME, {?HANDLE_TAG, PId}}, [{PId, PSlot} | Rest]) ->
    case arc_rt_arena_ffi:get(PId, Data) of
        PSlot -> chain_same(Data, element(?SOBJECT_PROTO, PSlot), Rest);
        _ -> false
    end;
chain_same(_, _, _) -> false.

%% fill only when the run appended every key in order onto sid
init_fill(St, Id, Sid, Proto, Keys, Site)
  when tuple_size(St) =:= ?AGENT_ARITY ->
    case element(?AGENT_STORE, St) of
        Store when tuple_size(Store) =:= ?STORE_ARITY ->
            Data = element(?STORE_DATA, Store),
            Slot = arc_rt_arena_ffi:get(Id, Data),
            Shapes = element(?STORE_SHAPES, Store),
            Ics = element(?STORE_ICS, Store),
            Entry = case element(1, Slot) =:= ?SSHAPED_TAG
                         andalso element(?SSHAPED_PROTO, Slot) =:= Proto
                         andalso appended(Shapes, Sid, Keys) of
                To when is_integer(To), To =:= element(?SSHAPED_SID, Slot) ->
                    case chain_of(Data, Proto, ?IC_INIT_HOPS, []) of
                        none -> ic_off;
                        Chain ->
                            Blank = setelement(?SSHAPED_SLOTS, Slot, {}),
                            {?IC_INIT, Sid, To, Blank, Chain}
                    end;
                _ -> ic_off
            end,
            setelement(?AGENT_STORE, St,
                       setelement(?STORE_ICS, Store, Ics#{Site => Entry}))
    end.

appended(_, Sid, []) -> Sid;
appended(Shapes, Sid, [K | Ks]) ->
    case Shapes of
        #{Sid := Desc} ->
            Arity = element(?SHAPE_ARITY_F, Desc),
            case element(?SHAPE_TRANSITIONS, Desc) of
                #{K := To} ->
                    case Shapes of
                        #{To := ToDesc}
                          when element(?SHAPE_ARITY_F, ToDesc) =:= Arity + 1 ->
                            appended(Shapes, To, Ks);
                        _ -> false
                    end;
                _ -> false
            end;
        _ -> false
    end.

chain_of(_, ?NONE, _, Acc) -> lists:reverse(Acc);
chain_of(_, _, 0, _) -> none;
chain_of(Data, {?SOME, {?HANDLE_TAG, PId}}, Fuel, Acc) ->
    case arc_rt_arena_ffi:get(PId, Data) of
        PSlot when element(1, PSlot) =:= ?SOBJECT_TAG;
                   element(1, PSlot) =:= ?SSHAPED_TAG ->
            chain_of(Data, element(?SOBJECT_PROTO, PSlot), Fuel - 1,
                     [{PId, PSlot} | Acc]);
        _ -> none
    end;
chain_of(_, _, _, _) -> none.

t_set_props_named(St, Obj = {?HANDLE_TAG, Id}, Keys, Vals, Strict) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, Sid, P, Slots, _} ->
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
                                                 Slots, Shapes, Dirty),
                                    each_named(St1, Obj, [K | Ks], [V | Vs],
                                               Strict)
                            end;
                        _ ->
                            St1 = commit(St, Store, Data, Id, Sid, P, Slots,
                                         Shapes, Dirty),
                            each_named(St1, Obj, [K | Ks], [V | Vs], Strict)
                    end
            end;
        _ ->
            St1 = commit(St, Store, Data, Id, Sid, P, Slots, Shapes, Dirty),
            each_named(St1, Obj, [K | Ks], [V | Vs], Strict)
    end;
shaped_run(St, Store, Data, Id, _, Sid, P, Slots, _, _, _, Shapes, Dirty) ->
    commit(St, Store, Data, Id, Sid, P, Slots, Shapes, Dirty).

commit(St, _, _, _, _, _, _, _, false) -> St;
commit(St, Store, Data, Id, Sid, P, Slots, Shapes, true)
  when tuple_size(St) =:= ?AGENT_ARITY, tuple_size(Store) =:= ?STORE_ARITY ->
    #{Sid := Desc} = Shapes,
    Slot = {?SSHAPED_TAG, Sid, P, Slots, element(?SHAPE_OFFSETS, Desc)},
    setelement(?AGENT_STORE, St,
               setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(Id, Slot, Data))).

each_named(St, Obj, [K | Ks], [V | Vs], Strict) ->
    each_named(arc_rt_obj_ffi:t_set_prop_named(St, Obj, K, V, Strict), Obj,
               Ks, Vs, Strict);
each_named(St, _, _, _, _) -> St.

t_new_object_props(St, Keys, Vals) ->
    Store = element(?AGENT_STORE, St),
    new_object_props(St, Store, Keys, Vals).

new_object_props(St, Store, Keys, Vals)
  when tuple_size(St) =:= ?AGENT_ARITY, tuple_size(Store) =:= ?STORE_ARITY ->
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
